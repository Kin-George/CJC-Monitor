"""Dos series de tiempo con estética de gráfico de encuestas: (1) PTF histórica del DANE y PTF simulada de 2026; (2) aumento histórico del salario
mínimo y el simulado para 2027, con las referencias (piso de inflación esperada, gremios, analistas, centrales).

Entradas: PTF/Outputs/tables/ptf_serie_empalmada_2005_2025.csv y sim2026_resumen.csv (percentiles de la simulación, script 16)
Salidas : PTF/Outputs/Figures/fig_serie_ptf_2026.png y fig_serie_salario_minimo_2027.png
Uso     : python PTF/Code/17_Figuras_series_simulacion_2026.py   (desde la raíz; correr antes el script 16)
"""
from pathlib import Path

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
from matplotlib import font_manager

ROOT = Path(__file__).resolve().parents[2]
T = ROOT / "PTF" / "Outputs" / "tables"
OUT = ROOT / "PTF" / "Outputs" / "Figures"
for fn in ("segoeui.ttf", "segoeuib.ttf", "seguisb.ttf"):
    p = Path(r"C:\Windows\Fonts") / fn
    if p.exists():
        font_manager.fontManager.addfont(str(p))
plt.rcParams["font.family"] = ["Segoe UI", "DejaVu Sans"]
NAVY, INK, MUTED, GRID = "#17294D", "#2B3A55", "#8591A3", "#E9EDF2"
ORANGE, GREEN, BLUE, GREY, RED = "#F26B38", "#1FA971", "#2F6FDE", "#8A94A0", "#D64545"
c = lambda x, d=1: f"{x:.{d}f}".replace(".", ",")


def lienzo(titulo, subtitulo, nota, ylabel):
    fig, ax = plt.subplots(figsize=(12.5, 6.6), dpi=200, facecolor="white")
    ax.set_facecolor("white")
    ax.grid(axis="y", color=GRID, lw=1, zorder=0)
    ax.set_axisbelow(True)
    for sp in ("top", "right", "left"):
        ax.spines[sp].set_visible(False)
    ax.spines["bottom"].set_color(GRID)
    ax.tick_params(length=0, labelsize=11, labelcolor=MUTED)
    ax.set_ylabel(ylabel, fontsize=10.5, color=MUTED, labelpad=8)
    fig.text(0.045, 0.965, titulo, fontsize=21, fontweight="bold", color=NAVY, va="top")
    fig.text(0.045, 0.895, subtitulo, fontsize=11.5, color=MUTED, va="top")
    fig.text(0.045, 0.014, nota, fontsize=8.6, color=MUTED, va="bottom", linespacing=1.5)
    fig.text(0.97, 0.014, "Centro Javeriano de Competitividad", fontsize=8.6, color=NAVY, va="bottom", ha="right", fontweight="bold")
    fig.subplots_adjust(top=0.80, bottom=0.15, left=0.075, right=0.73)
    return fig, ax


def etiqueta(ax, xy, txt, color, dx, dy, grande=False, ha="left"):
    ax.annotate(txt, xy=xy, xytext=(dx, dy), textcoords="offset points", ha=ha, va="center", fontsize=11 if grande else 9.6,
                color="white", fontweight="bold", zorder=9,
                bbox=dict(boxstyle="round,pad=0.3,rounding_size=0.6", fc=color, ec="none"),
                arrowprops=dict(arrowstyle="-", color=color, lw=0.9, alpha=0.8, shrinkA=0, shrinkB=3))


def abanico(ax, x0, y0, x1, med, p05, p25, p75, p95, color):
    """Abanico de incertidumbre: franja clara 5-95, franja más oscura 25-75, línea punteada de la mediana y, en el último año, un rango fino con topes."""
    t = np.linspace(0, 1, 40)
    x = x0 + (x1 - x0) * t
    centro = y0 + (med - y0) * t
    ax.fill_between(x, centro + t * (p05 - med), centro + t * (p95 - med), color=color, alpha=0.10, lw=0, zorder=2)
    ax.fill_between(x, centro + t * (p25 - med), centro + t * (p75 - med), color=color, alpha=0.18, lw=0, zorder=3)
    ax.plot(x, centro, color=color, lw=2.0, ls=(0, (1.0, 1.6)), zorder=5, solid_capstyle="round")
    cap = (x1 - x0) * 0.07
    ax.plot([x1, x1], [p05, p95], color=color, lw=1.4, alpha=0.6, zorder=4, solid_capstyle="butt")
    for yy in (p05, p95):
        ax.plot([x1 - cap, x1 + cap], [yy, yy], color=color, lw=1.4, alpha=0.6, zorder=4, solid_capstyle="butt")
    ax.plot([x1, x1], [p25, p75], color=color, lw=5, alpha=0.5, zorder=4, solid_capstyle="butt")


def punto_mediana(ax, x, y, color):
    ax.scatter([x], [y], s=50, color=color, edgecolor="white", linewidth=1.5, zorder=8)


def punto_ref(ax, x, y, color):
    ax.scatter([x], [y], marker="D", s=24, color=color, edgecolor="white", linewidth=0.8, zorder=8)


def valores(ax, xs, ys, d=1):
    """Etiqueta de valor en cada punto: arriba si el punto está por encima del promedio de sus vecinos, abajo si no."""
    ys = list(ys)
    for i, (x, yv) in enumerate(zip(xs, ys)):
        vec = [ys[j] for j in (i - 1, i + 1) if 0 <= j < len(ys)]
        arriba = yv >= np.mean(vec)
        sube = i + 1 < len(ys) and ys[i + 1] > yv
        dx, ha = (-9, "right") if (arriba and sube) else (0, "center")
        ax.annotate(c(yv, d), xy=(x, yv), xytext=(dx, 11 if arriba else -17), textcoords="offset points", ha=ha, fontsize=9.3, color=NAVY, fontweight="bold", zorder=9)


NOMBRE = "Centro Javeriano\nde Competitividad"

res = pd.read_csv(T / "sim2026_resumen.csv", index_col=0)
ps = res.loc["PTF"]
ss = res.loc[[i for i in res.index if i.startswith("Aumento salario mínimo 2027 - Base")][0]]
pi = res.loc["Inflación de cierre 2026", "mediana"]

# ------------------------------------------------------------------ 1. PTF
ser = pd.read_csv(T / "ptf_serie_empalmada_2005_2025.csv")
ser["anio"] = ser["anio"].astype(int)
h = ser[ser.anio >= 2006].reset_index(drop=True)
fig, ax = lienzo("¿Cuánto crecerá la productividad en 2026?",
                 "Productividad total de los factores (PTF), variación anual %. DANE 2006–2025 y simulación de 2026 (año corrido enero–septiembre)",
                 "Fuente: DANE (PTF por valor agregado; 2025 preliminar) y simulación propia, 200.000 trayectorias (PTF/Code/16). La cifra de 2026 no es del DANE.",
                 "PTF, variación anual (%)")
ax.axhline(0, color=INK, lw=1, alpha=0.5, zorder=2)
ax.plot(h.anio, h.PTF, color=NAVY, lw=2.6, zorder=6, solid_capstyle="round")
ax.scatter(h.anio, h.PTF, s=22, color="white", edgecolor=NAVY, linewidth=1.5, zorder=7)
v25 = h.PTF.iloc[-1]
abanico(ax, 2025, v25, 2026, ps["mediana"], ps["p05"], ps["p25"], ps["p75"], ps["p95"], ORANGE)
punto_mediana(ax, 2026, ps["mediana"], ORANGE)
valores(ax, h.anio, h.PTF, 1)
ax.annotate("pandemia", xy=(2020, h[h.anio == 2020].PTF.iloc[0]), xytext=(0, -34), textcoords="offset points", ha="center", fontsize=9, color=MUTED)
punto_ref(ax, 2026, 0.91, GREEN)
punto_ref(ax, 2026, 1.2, BLUE)
etiqueta(ax, (2026, 1.2), "1,2  Corficolombiana", BLUE, 42, 56)
etiqueta(ax, (2026, ps["mediana"]), f"{c(ps['mediana'], 2)}  {NOMBRE}", ORANGE, 42, -34, grande=True)
etiqueta(ax, (2026, 0.91), "0,91  DANE, mismo periodo 2025", GREEN, 42, 22)
ax.set_xlim(2005.4, 2026.5)
ax.set_xticks(list(range(2006, 2027, 2)))
ax.set_ylim(-3.4, 4.8)
fig.savefig(OUT / "fig_serie_ptf_2026.png", dpi=200, facecolor="white")


# ------------------------------------------------------------------ 1b. Productividad laboral por hora
cu = pd.read_excel(ROOT / "PTF/Datos/Raw/anex-PTF-ProductividadLaboral-2025.xlsx", sheet_name="Cuadro 1", header=None).iloc[14:35, 1:3]
cu.columns = ["anio", "lp"]
cu["anio"] = cu["anio"].astype(str).str.extract(r"(\d{4})")[0].astype(int)
cu["lp"] = cu["lp"].astype(float)
cu = cu[cu.anio >= 2006].reset_index(drop=True)
lh = res.loc["Productividad laboral por hora"]
fig, ax = lienzo("¿Cuánto crecerá la productividad por hora en 2026?",
                 "Productividad laboral por hora trabajada, variación anual %. DANE 2006–2025 y simulación de 2026 (año corrido enero–septiembre)",
                 "Fuente: DANE (productividad laboral por hora; 2025 preliminar), GEIH 2026 enero–agosto (horas efectivas) y simulación propia (PTF/Code/16 y 18). No es una cifra del DANE.",
                 "Productividad por hora, variación anual (%)")
ax.axhline(0, color=INK, lw=1, alpha=0.5, zorder=2)
ax.plot(cu.anio, cu.lp, color=NAVY, lw=2.6, zorder=6, solid_capstyle="round")
ax.scatter(cu.anio, cu.lp, s=22, color="white", edgecolor=NAVY, linewidth=1.5, zorder=7)
valores(ax, cu.anio, cu.lp, 1)
abanico(ax, 2025, cu.lp.iloc[-1], 2026, lh["mediana"], lh["p05"], lh["p25"], lh["p75"], lh["p95"], ORANGE)
punto_mediana(ax, 2026, lh["mediana"], ORANGE)
punto_ref(ax, 2026, 0.57, GREEN)
etiqueta(ax, (2026, lh["mediana"]), f"{c(lh['mediana'])}  {NOMBRE}", ORANGE, 42, 30, grande=True)
etiqueta(ax, (2026, 0.57), "0,57  DANE, mismo periodo 2025", GREEN, 42, -34)
ax.set_xlim(2005.4, 2026.5)
ax.set_xticks(list(range(2006, 2027, 2)))
ax.set_ylim(-3.4, 7.8)
fig.savefig(OUT / "fig_serie_productividad_hora_2026.png", dpi=200, facecolor="white")

# ------------------------------------------------------------------ 2. Salario mínimo
# aumento del salario fijado para cada año, calculado de los montos de salariominimocolombia.net (verificado)
aum = {2014: 4.50, 2015: 4.60, 2016: 7.00, 2017: 7.00, 2018: 5.90, 2019: 6.00, 2020: 6.00, 2021: 3.50, 2022: 10.07, 2023: 16.00, 2024: 12.07, 2025: 9.50, 2026: 23.00}
a = pd.Series(aum)
fig, ax = lienzo("¿Cuánto subirá el salario mínimo en 2027?",
                 "Aumento nominal del salario mínimo (%) en cada año y simulación para 2027, con el piso de la inflación esperada y las propuestas conocidas",
                 "Fuente: salariominimocolombia.net (montos) y simulación propia (PTF/Code/16). Piso: inflación esperada de cierre de 2026.\nPesos de las reglas: 45% técnica, 40% histórica, 15% activista.",
                 "Aumento del salario mínimo (%)")
ax.plot(a.index, a.values, color=NAVY, lw=2.6, zorder=6, solid_capstyle="round")
ax.scatter(a.index, a.values, s=24, color="white", edgecolor=NAVY, linewidth=1.5, zorder=7)
valores(ax, a.index, a.values, 1)
abanico(ax, 2026, 23.0, 2027, ss["mediana"], ss["p05"], ss["p25"], ss["p75"], ss["p95"], ORANGE)
punto_mediana(ax, 2027, ss["mediana"], ORANGE)
ax.axhline(pi, xmin=0.60, color=GREY, lw=1.4, ls=(0, (4, 3)), zorder=3)
punto_ref(ax, 2027, 8.5, GREEN)
punto_ref(ax, 2027, 8.0, BLUE)
etiqueta(ax, (2027, ss["mediana"]), f"{c(ss['mediana'])}  {NOMBRE}", ORANGE, 44, 36, grande=True)
etiqueta(ax, (2027, 8.5), "8,5  Davivienda", GREEN, 44, 8)
etiqueta(ax, (2027, 8.0), "8,0  Corficolombiana", BLUE, 44, -17)
etiqueta(ax, (2027, pi), f"{c(pi)}  piso: inflación esperada", GREY, 44, -44)
ax.text(2025.0, 26.2, "La CUT propone 38,5%  ▲ fuera de escala", fontsize=10.5, color="white", fontweight="bold", ha="left", va="center",
        bbox=dict(boxstyle="round,pad=0.4,rounding_size=0.7", fc=RED, ec="none"))
ax.set_xlim(2013.5, 2027.5)
ax.set_xticks(list(range(2014, 2028)))
ax.set_ylim(0, 28)
fig.savefig(OUT / "fig_serie_salario_minimo_2027.png", dpi=200, facecolor="white")


# ------------------------------------------------------------------ 2b. Salario mínimo en pesos
monto = {2014: 616000, 2015: 644350, 2016: 689455, 2017: 737717, 2018: 781242, 2019: 828116, 2020: 877803, 2021: 908526, 2022: 1000000,
         2023: 1160000, 2024: 1300000, 2025: 1423500, 2026: 1750905}
m = pd.Series(monto)
base_ = m[2026]
pesos = lambda v: "$" + f"{v:,.0f}".replace(",", ".")
proy = {k_: base_ * (1 + ss[k_] / 100) for k_ in ("p05", "p25", "mediana", "p75", "p95")}
fig, ax = lienzo("¿Cuánto será el salario mínimo en 2027, en pesos?",
                 "Salario mínimo mensual nominal (sin auxilio de transporte), 2014–2026, y simulación para 2027 aplicando el aumento simulado al monto de 2026",
                 "Fuente: salariominimocolombia.net (montos 2014–2026) y simulación propia (PTF/Code/16). Propuestas: Corficolombiana y Davivienda (prensa), CUT (38,5%).",
                 "Salario mínimo mensual (pesos)")
ax.plot(m.index, m.values, color=NAVY, lw=2.6, zorder=6, solid_capstyle="round")
ax.scatter(m.index, m.values, s=24, color="white", edgecolor=NAVY, linewidth=1.5, zorder=7)
for yr, v in m.items():
    ax.annotate(pesos(v), xy=(yr, v), xytext=(-2 if yr > 2014 else 0, 12 if yr != 2014 else -19), textcoords="offset points", ha="right" if yr > 2014 else "center", fontsize=8.4, color=NAVY, fontweight="bold", zorder=9)
abanico(ax, 2026, base_, 2027, proy["mediana"], proy["p05"], proy["p25"], proy["p75"], proy["p95"], ORANGE)
punto_mediana(ax, 2027, proy["mediana"], ORANGE)
for yy, col in ((base_ * 1.085, GREEN), (base_ * 1.08, BLUE)):
    punto_ref(ax, 2027, yy, col)
ax.axhline(base_ * (1 + pi / 100), xmin=0.60, color=GREY, lw=1.4, ls=(0, (4, 3)), zorder=3)
etiqueta(ax, (2027, proy["mediana"]), f"{pesos(proy['mediana'])}  {NOMBRE}", ORANGE, 44, 60, grande=True)
etiqueta(ax, (2027, base_ * 1.085), f"{pesos(base_ * 1.085)}  Davivienda", GREEN, 44, 24)
etiqueta(ax, (2027, base_ * 1.08), f"{pesos(base_ * 1.08)}  Corficolombiana", BLUE, 44, -6)
etiqueta(ax, (2027, base_ * (1 + pi / 100)), f"{pesos(base_ * (1 + pi / 100))}  piso: inflación esperada", GREY, 44, -36)
ax.text(2014.0, 2.08e6, "La CUT propone $2.424.003 (38,5%)  ▲ fuera de escala", fontsize=10.5, color="white", fontweight="bold", ha="left", va="center",
        bbox=dict(boxstyle="round,pad=0.4,rounding_size=0.7", fc=RED, ec="none"))
ax.set_xlim(2013.5, 2027.5)
ax.set_xticks(list(range(2014, 2028)))
ax.set_ylim(400000, 2150000)
ax.set_yticks([500000, 1000000, 1500000, 2000000])
ax.set_yticklabels(["$500.000", "$1.000.000", "$1.500.000", "$2.000.000"])
fig.subplots_adjust(left=0.115)
fig.savefig(OUT / "fig_serie_salario_minimo_pesos_2027.png", dpi=200, facecolor="white")
print({k_: round(v) for k_, v in proy.items()})

# ------------------------------------------------------------------ 3. PTF por rama
sec = pd.read_csv(T / "sim2026_sectores_PTF_2026.csv").sort_values("PTF_2026_media").reset_index(drop=True)
fig, (ax, ax2) = plt.subplots(1, 2, figsize=(13.5, 6.4), dpi=200, facecolor="white", gridspec_kw={"width_ratios": [1.5, 1], "wspace": 0.12})
for aa in (ax, ax2):
    aa.grid(axis="x", color=GRID, lw=1, zorder=0)
    aa.set_axisbelow(True)
    for sp in ("top", "right", "left"):
        aa.spines[sp].set_visible(False)
    aa.spines["bottom"].set_color(GRID)
    aa.tick_params(length=0, labelsize=10.5, labelcolor=MUTED)
y = np.arange(len(sec))
ax.axvline(0, color=INK, lw=1, alpha=0.5, zorder=2)
for i, r in sec.iterrows():
    col = ORANGE if r.PTF_2026_media >= 0 else BLUE
    ax.plot([r.p05, r.p95], [i, i], color=col, lw=5, alpha=0.28, solid_capstyle="round", zorder=3)
    ax.scatter([r.PTF_2026_media], [i], s=60, color=col, edgecolor="white", linewidth=1.5, zorder=6)
    ax.scatter([r["PTF_apriori_(promedio_2011-2024)"]], [i], marker="D", s=40, facecolor="white", edgecolor=GREY, linewidth=1.6, zorder=5)
    ax.annotate(c(r.PTF_2026_media, 1), xy=(r.PTF_2026_media, i), xytext=(0, 13), textcoords="offset points", ha="center", fontsize=10, color=NAVY, fontweight="bold")
ax.set_yticks(y)
ax.set_yticklabels(sec.rama, fontsize=11, color=INK)
ax.set_xlabel("PTF de la rama, enero–septiembre de 2026 (%)", fontsize=10.5, color=MUTED)
ax.set_title("PTF 2026 por rama (círculo: estimación; barra: 5–95%; rombo: promedio 2011–2024)", loc="left", fontsize=11, fontweight="bold", color=NAVY, pad=10)
cols = [ORANGE if v >= 0 else BLUE for v in sec.aporte_a_PTF_agregada_pp]
ax2.barh(y, sec.aporte_a_PTF_agregada_pp, color=cols, height=0.55, zorder=3)
for i, v in enumerate(sec.aporte_a_PTF_agregada_pp):
    ax2.annotate(c(v, 2), xy=(v, i), xytext=(6 if v >= 0 else -6, 0), textcoords="offset points", ha="left" if v >= 0 else "right", va="center", fontsize=10, color=NAVY, fontweight="bold")
ax2.axvline(0, color=INK, lw=1, alpha=0.5)
ax2.set_yticks([])
ax2.set_xlim(-0.55, 0.75)
ax2.set_xlabel("Aporte a la PTF agregada (pp) = peso de Domar × PTF", fontsize=10.5, color=MUTED)
ax2.set_title(f"Suma de aportes: {c(sec.aporte_a_PTF_agregada_pp.sum(), 2)} pp", loc="left", fontsize=11, fontweight="bold", color=NAVY, pad=10)
fig.text(0.045, 0.975, "¿De qué ramas vendrá la productividad de 2026?", fontsize=21, fontweight="bold", color=NAVY, va="top")
fig.text(0.045, 0.905, "PTF por rama (enfoque de producción del DANE) combinada con la identidad agregada de 2026; la suma ponderada reproduce la PTF agregada", fontsize=11.5, color=MUTED, va="top")
fig.text(0.045, 0.014, "Fuente: DANE (PTF por actividad 2005–2024; cuentas nacionales) y cálculos propios (PTF/Code/16). Los valores de 2026 por rama no son cifras del DANE.", fontsize=8.6, color=MUTED, va="bottom")
fig.text(0.97, 0.014, "Centro Javeriano de Competitividad", fontsize=8.6, color=NAVY, va="bottom", ha="right", fontweight="bold")
fig.subplots_adjust(top=0.80, bottom=0.12, left=0.22, right=0.97)
fig.savefig(OUT / "fig_sectores_ptf_2026.png", dpi=200, facecolor="white")
print("ok")
