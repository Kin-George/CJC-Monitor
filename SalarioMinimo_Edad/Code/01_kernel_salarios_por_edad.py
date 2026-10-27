"""Distribucion del salario mensual de los asalariados por grupo de edad (menores de 25 frente a 25 o mas) y el salario minimo sobre la distribucion.

Motivacion: evaluar una propuesta de salario minimo diferenciado por edad (como la propuesta de un minimo por regiones). El grafico muestra donde cae el minimo en la
distribucion de cada grupo y que tanta gente se concentra en el.

Poblacion: ocupados de 15 anos o mas que son asalariados (posicion ocupacional P6430 = 1 empleado particular, 2 empleado del gobierno, 3 empleado domestico, 7 jornalero),
con salario mensual del empleo principal positivo (P6500). Se muestran todos los asalariados y los de tiempo completo (horas habituales P6800 >= 40).
Ponderadores: FEX_C18 (mensual). Kernel gaussiano ponderado sobre el logaritmo del salario (ancho de banda 0,09 = 9%).
Salario minimo mensual (sin auxilio de transporte): 2022 $1.000.000; 2023 $1.160.000; 2024 $1.300.000; 2025 $1.423.500; 2026 $1.750.905 (salariominimocolombia.net).

Entradas: C:/Users/jorge/Documents/Databases/GEIH/GEIH_<anio>_total.dta (2022-2026)
Salidas : SalarioMinimo_Edad/Outputs/tables/estadisticas_salario_por_edad.csv
          SalarioMinimo_Edad/Outputs/Figures/fig_kernel_salario_edad_2025.png y fig_kernel_salario_edad_multiplos_2022_2026.png
Uso     : python SalarioMinimo_Edad/Code/01_kernel_salarios_por_edad.py   (desde la raiz del repositorio)
"""
from pathlib import Path

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import pyreadstat
from matplotlib import font_manager
from matplotlib.ticker import FuncFormatter

ROOT = Path(__file__).resolve().parents[2]
OUT = ROOT / "SalarioMinimo_Edad" / "Outputs"
GEIH = Path("C:/Users/jorge/Documents/Databases/GEIH")
SMMLV = {2022: 1_000_000, 2023: 1_160_000, 2024: 1_300_000, 2025: 1_423_500, 2026: 1_750_905}
H = 0.09                                    # ancho de banda en logaritmos

# ------------------------------------------------------------------ datos
D = {}
for a in SMMLV:
    _, meta = pyreadstat.read_dta(GEIH / f"GEIH_{a}_total.dta", metadataonly=True)
    names = {c.upper(): c for c in meta.column_names}
    edad = names.get("P6040") or names["EDAD"]
    mes = names["MES"]
    renombre = {mes: "MES", names["FEX_C18"]: "FEX", names["OCI"]: "OCI", edad: "EDAD", names["P6430"]: "P6430", names["P6500"]: "P6500", names["P6800"]: "P6800", names["INGLABO"]: "INGLABO"}
    df, _ = pyreadstat.read_dta(GEIH / f"GEIH_{a}_total.dta", usecols=list(renombre))
    df = df.rename(columns=renombre)[list(renombre.values())]       # pyreadstat devuelve las columnas en el orden del archivo
    for c in df.columns:
        df[c] = pd.to_numeric(df[c], errors="coerce")
    if a == 2026:
        df = df[df.MES <= 8]
    df = df[(df.OCI == 1) & (df.FEX > 0) & (df.EDAD >= 15) & df.P6430.isin([1, 2, 3, 7]) & (df.P6500 > 0)].copy()
    df["joven"] = df.EDAD < 25
    df["meses"] = 8 if a == 2026 else 12
    D[a] = df


def cuantil(x, w, q):
    i = np.argsort(x)
    x, w = np.asarray(x)[i], np.asarray(w)[i]
    c = np.cumsum(w) / w.sum()
    return float(np.interp(q, c, x))


def kernel(x, w, grid):
    """Densidad gaussiana ponderada de ln(x) en la rejilla (binning fino y suavizado)."""
    lx = np.log(x)
    edges = np.append(grid - (grid[1] - grid[0]) / 2, grid[-1] + (grid[1] - grid[0]) / 2)
    hist, _ = np.histogram(lx, bins=edges, weights=w)
    dens = hist / hist.sum() / (grid[1] - grid[0])
    k = np.exp(-0.5 * ((np.arange(-400, 401) * (grid[1] - grid[0])) / H) ** 2)
    k /= k.sum()
    return np.convolve(dens, k, mode="same")


# ------------------------------------------------------------------ estadisticas
filas = []
for a, df in D.items():
    sm = SMMLV[a]
    for nombre, filtro in (("todos los asalariados", np.ones(len(df), bool)), ("tiempo completo (>= 40 h)", (df.P6800 >= 40).values)):
        for g, m in (("menores de 25", df.joven.values), ("25 o mas", ~df.joven.values)):
            x = df.P6500.values[filtro & m]
            w = df.FEX.values[filtro & m]
            filas.append({"anio": a, "poblacion": nombre, "grupo": g, "muestra": len(x), "asalariados_millones": w.sum() / df.meses.iloc[0] / 1e6,
                          "mediana": cuantil(x, w, 0.5), "p25": cuantil(x, w, 0.25), "p75": cuantil(x, w, 0.75), "media": np.average(x, weights=w),
                          "mediana_sobre_minimo": cuantil(x, w, 0.5) / sm,
                          "pct_bajo_el_minimo": 100 * w[x < 0.97 * sm].sum() / w.sum(), "pct_en_el_minimo_(±3%)": 100 * w[(x >= 0.97 * sm) & (x <= 1.03 * sm)].sum() / w.sum(),
                          "pct_hasta_1,25_minimos": 100 * w[x <= 1.25 * sm].sum() / w.sum(), "pct_mas_de_2_minimos": 100 * w[x > 2 * sm].sum() / w.sum()})
est = pd.DataFrame(filas)
est.round(2).to_csv(OUT / "tables" / "estadisticas_salario_por_edad.csv", index=False, encoding="utf-8-sig")
pd.set_option("display.width", 250)
print(est[est.anio.isin([2025])].round(1).to_string(index=False))

# ------------------------------------------------------------------ figuras
for fn in ("segoeui.ttf", "segoeuib.ttf"):
    p = Path(r"C:\Windows\Fonts") / fn
    if p.exists():
        font_manager.fontManager.addfont(str(p))
plt.rcParams["font.family"] = ["Segoe UI", "DejaVu Sans"]
NAVY, MUTED, GRID, ORANGE, RED = "#17294D", "#8591A3", "#E9EDF2", "#F26B38", "#D64545"
c = lambda x, d=1: f"{x:.{d}f}".replace(".", ",")
pesos = lambda v: "$" + f"{v:,.0f}".replace(",", ".")
grid = np.linspace(np.log(150_000), np.log(9_000_000), 1400)


def lienzo(figsize, titulo, subtitulo, nota, ncols=1):
    fig, axs = plt.subplots(1, ncols, figsize=figsize, dpi=200, facecolor="white", squeeze=False)
    for ax in axs[0]:
        ax.grid(axis="y", color=GRID, lw=1, zorder=0)
        ax.set_axisbelow(True)
        for sp in ("top", "right", "left"):
            ax.spines[sp].set_visible(False)
        ax.spines["bottom"].set_color(GRID)
        ax.tick_params(length=0, labelsize=10.5, labelcolor=MUTED)
        ax.set_yticks([])
    fig.text(0.045, 0.965, titulo, fontsize=20, fontweight="bold", color=NAVY, va="top")
    fig.text(0.045, 0.895, subtitulo, fontsize=11.2, color=MUTED, va="top")
    fig.text(0.045, 0.014, nota, fontsize=8.5, color=MUTED, va="bottom", linespacing=1.5)
    fig.text(0.97, 0.014, "Centro Javeriano de Competitividad", fontsize=8.5, color=NAVY, va="bottom", ha="right", fontweight="bold")
    return fig, axs[0]


def ejes_pesos(ax, sm=None, mult=False):
    if mult:
        ticks = [0.25, 0.5, 1, 1.5, 2, 3, 5]
        ax.set_xticks(np.log(ticks))
        ax.set_xticklabels([str(t).replace(".", ",") + "×" for t in ticks])
        ax.set_xlim(np.log(0.15), np.log(7))
    else:
        ticks = [250_000, 500_000, 1_000_000, 2_000_000, 3_000_000, 5_000_000, 8_000_000]
        ax.set_xticks(np.log(ticks))
        ax.set_xticklabels([f"${t / 1e6:.2f}M".replace(".", ",").replace(",00M", "M").replace(",50M", ",5M") for t in ticks])
        ax.set_xlim(np.log(200_000), np.log(8_500_000))


def panel(ax, df, a, filtro, titulo):
    sm = SMMLV[a]
    cols = {"menores de 25": ORANGE, "25 o mas": NAVY}
    ymax = 0
    for g, m in (("25 o mas", ~df.joven.values), ("menores de 25", df.joven.values)):
        x, w = df.P6500.values[filtro & m], df.FEX.values[filtro & m]
        y = kernel(x, w, grid)
        ymax = max(ymax, y.max())
        ax.fill_between(grid, y, color=cols[g], alpha=0.16, lw=0, zorder=2)
        ax.plot(grid, y, color=cols[g], lw=2.4, zorder=4)
        med = cuantil(x, w, 0.5)
        ax.scatter([np.log(med)], [np.interp(np.log(med), grid, y)], s=34, color="white", edgecolor=cols[g], linewidth=1.6, zorder=6)
    ax.axvline(np.log(sm), color=RED, lw=1.6, ls=(0, (4, 3)), zorder=5)
    ax.set_ylim(0, ymax * 1.28)
    ax.text(np.log(sm) * 1.0 + 0.03, ymax * 1.20, f"Salario mínimo {a}\n{pesos(sm)}", color=RED, fontsize=10, fontweight="bold", va="top", ha="left")
    ax.set_title(titulo, loc="left", fontsize=12, fontweight="bold", color=NAVY, pad=8)
    return ymax


# --- figura 1: 2025, dos paneles
df = D[2025]
fig, (a1, a2) = lienzo((13.5, 6.3), "¿Dónde cae el salario mínimo en la distribución de salarios?",
                       "Salario mensual del empleo principal de los asalariados, 2025, menores de 25 años frente a 25 años o más (escala logarítmica)",
                       "Fuente: DANE, GEIH 2025 (asalariados de 15 años o más; ponderador mensual FEX_C18) y cálculos propios. Kernel gaussiano ponderado, ancho de banda 9%.\nLos círculos marcan la mediana de cada grupo. Tiempo completo: horas habituales de 40 o más por semana.", ncols=2)
panel(a1, df, 2025, np.ones(len(df), bool), "A. Todos los asalariados")
panel(a2, df, 2025, (df.P6800 >= 40).values, "B. Asalariados de tiempo completo")
for ax in (a1, a2):
    ejes_pesos(ax)
# leyenda y estadísticas
e = est[(est.anio == 2025)]
for ax, pob in ((a1, "todos los asalariados"), (a2, "tiempo completo (>= 40 h)")):
    j = e[(e.poblacion == pob) & (e.grupo == "menores de 25")].iloc[0]
    v = e[(e.poblacion == pob) & (e.grupo == "25 o mas")].iloc[0]
    for yy, g, col in ((0.66, j, ORANGE), (0.46, v, NAVY)):
        nom = "Menores de 25" if col == ORANGE else "25 años o más"
        ax.text(0.985, yy, f"{nom} ({c(g.asalariados_millones, 1)} M asalariados)\nMediana {pesos(g.mediana)}\n{c(g['pct_en_el_minimo_(±3%)'], 0)}% gana alrededor del mínimo; {c(g.pct_bajo_el_minimo, 0)}% menos", transform=ax.transAxes,
                ha="right", va="top", fontsize=8.8, color=col, linespacing=1.55, fontweight="bold",
                bbox=dict(boxstyle="round,pad=0.45,rounding_size=0.5", fc="white", ec=col, lw=1.1))
a1.plot([], [], color=ORANGE, lw=2.4, label="Menores de 25 años")
a1.plot([], [], color=NAVY, lw=2.4, label="25 años o más")
a1.legend(loc="upper left", frameon=False, fontsize=10, labelcolor=NAVY)
fig.subplots_adjust(top=0.80, bottom=0.17, left=0.03, right=0.985, wspace=0.08)
fig.savefig(OUT / "Figures" / "fig_kernel_salario_edad_2025.png", dpi=200, facecolor="white")

# --- figura 2: estabilidad 2022-2026 en multiplos del salario minimo de cada anio
fig, (b1, b2) = lienzo((13.5, 6.0), "¿Es estable el patrón? Salario en múltiplos del salario mínimo, 2022–2026",
                       "Salario mensual de los asalariados de tiempo completo, dividido por el salario mínimo de cada año (1× = el mínimo)",
                       "Fuente: DANE, GEIH 2022–2026 (2026: enero–agosto, con el mínimo de $1.750.905) y cálculos propios.\nAsalariados de tiempo completo (40 horas o más). Kernel gaussiano ponderado, ancho de banda 9%.", ncols=2)
estilos = {2022: ("#B9C4D6", 1.6), 2023: ("#8FA3C2", 1.6), 2024: ("#5C77A6", 1.8), 2025: (None, 2.6), 2026: (None, 2.0)}
for ax, g, col, tit in ((b1, "menores de 25", ORANGE, "A. Menores de 25 años"), (b2, "25 o mas", NAVY, "B. 25 años o más")):
    ymax = 0
    for a, d in D.items():
        m = (d.joven.values if g == "menores de 25" else ~d.joven.values) & (d.P6800 >= 40).values
        y = kernel(d.P6500.values[m] / SMMLV[a], d.FEX.values[m], grid_m := np.linspace(np.log(0.15), np.log(7), 1000))
        ymax = max(ymax, y.max())
        base, lw = estilos[a]
        cc = col if base is None else base
        ax.plot(grid_m, y, color=cc, lw=lw, ls=(0, (3, 2)) if a == 2026 else "-", alpha=1 if a >= 2025 else 0.9, zorder=4 if a >= 2025 else 3, label=str(a) + (" (ene–ago)" if a == 2026 else ""))
    ax.axvline(0, color=RED, lw=1.6, ls=(0, (4, 3)), zorder=5)
    ax.text(0.03, ymax * 1.17, "1× = salario\nmínimo", color=RED, fontsize=10, fontweight="bold", va="top")
    ax.set_ylim(0, ymax * 1.25)
    ax.set_title(tit, loc="left", fontsize=12, fontweight="bold", color=NAVY, pad=8)
    ejes_pesos(ax, mult=True)
    ax.legend(loc="upper right", frameon=False, fontsize=9.5, labelcolor=NAVY, ncol=1)
fig.subplots_adjust(top=0.80, bottom=0.15, left=0.03, right=0.985, wspace=0.08)
fig.savefig(OUT / "Figures" / "fig_kernel_salario_edad_multiplos_2022_2026.png", dpi=200, facecolor="white")
print("ok")
