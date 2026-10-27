"""Dinamica del empleo asalariado e independiente por sector (version moderna).

Replica para Colombia el panel (b) de la figura de Laos:
  Eje x: cambio en ocupados independientes (solo trabajadores por cuenta propia), miles.
  Eje y: cambio en ocupados asalariados (empleados particulares y del gobierno, domesticos y jornaleros; desde 2021 los jornaleros vienen con codigo 7), miles.
  Tamano: ocupados totales del sector en el anio inicial.
  Color: azul = el sector crea mas empleo asalariado que independiente (por encima de la diagonal);
         naranja = lo contrario.
Una figura por periodo: 2010-2025, 2010-2019 y 2019-2025.

Insumo: Datos/Processed/GEIH/Paper-GEIH_base_modelo_personas_2008_2025.dta (GEIH; sin 2020 comparable).
"""

import importlib.util
from pathlib import Path

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

ROOT = Path(__file__).resolve().parents[3]
DTA = ROOT / "Datos" / "Processed" / "GEIH" / "Paper-GEIH_base_modelo_personas_2008_2025.dta"
CACHE = ROOT / "Outputs" / "Tables"
CACHE.mkdir(parents=True, exist_ok=True)
OUT_DIRS = [ROOT / "Outputs" / "Figures", ROOT / "Paper" / "figures"]

# reutiliza utilidades de la figura anterior (fuentes, colores, colocacion de etiquetas)
_spec = importlib.util.spec_from_file_location("bubbles", Path(__file__).with_name("02.10_Burbujas_CambioEstructural.py"))
bub = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(bub)
INK, INK2, MUTED, GRID, AXIS, GAIN, LOSS = bub.INK, bub.INK2, bub.MUTED, bub.GRID, bub.AXIS, bub.GAIN, bub.LOSS
es = bub.es

SHORT = {
    "Agricultura, ganadería, silvicultura y pesca": "Agropecuario",
    "Minas y canteras": "Minas",
    "Industrias manufactureras": "Manufactura",
    "Electricidad, gas, agua y saneamiento": "Electricidad, gas\ny agua",
    "Construcción": "Construcción",
    "Comercio y reparación": "Comercio y\nreparación",
    "Alojamiento y servicios de comida": "Alojamiento y\ncomida",
    "Transporte y almacenamiento": "Transporte y\nalmacenamiento",
    "Información y comunicaciones": "Información y\ncomunicaciones",
    "Actividades financieras y de seguros": "Financieras\ny seguros",
    "Inmobiliarias, profesionales y administrativas": "Inmobiliarias, profesionales\ny administrativas",
    "Administración pública y defensa": "Administración\npública",
    "Educación": "Educación",
    "Salud y asistencia social": "Salud",
    "Artes, recreación y otros servicios": "Artes, recreación\ny otros",
    "Hogares como empleadores": "Hogares\ncomo empleadores",
}


def load_table():
    cache = CACHE / "ocupados_sector_tipo_empleo_2010_2025.csv"
    if cache.exists():
        return pd.read_csv(cache)
    df = pd.read_stata(DTA, columns=["anio", "sector_hom_cod", "sector", "posicion_ocupacional", "fex"],
                       convert_categoricals=False)
    df = df[(df.fex > 0) & df.anio.between(2010, 2025) & (df.anio != 2020) & (df.sector_hom_cod != 17)].copy()
    # independiente = solo cuenta propia (4); patrones, familiares sin remuneracion y otros quedan en "otro"
    df["tipo"] = np.where(df.posicion_ocupacional.isin([1, 2, 3, 7, 8]), "asalariado",
                          np.where(df.posicion_ocupacional == 4, "independiente", "otro"))
    g = df.groupby(["anio", "sector", "tipo"]).fex.sum().unstack("tipo").fillna(0).reset_index()
    g.to_csv(cache, index=False, encoding="utf-8-sig")
    return g


def period(g, y0, y1):
    a = g[g.anio == y0].set_index("sector")
    b = g[g.anio == y1].set_index("sector")
    d = pd.DataFrame({
        "asal": (b.asalariado - a.asalariado) / 1e3,
        "indep": (b.independiente - a.independiente) / 1e3,
        "emp0": (a.asalariado + a.independiente + a.otro) / 1e3,
    })
    return d


def build(g, y0, y1, filename):
    d = period(g, y0, y1)
    tot_a, tot_i = d.asal.sum(), d.indep.sum()
    fig = plt.figure(figsize=(12, 8.6), dpi=200, facecolor="white")
    ax = fig.add_axes([0.085, 0.145, 0.885, 0.64])
    pad_x, pad_y = (d.indep.max() - d.indep.min()) * 0.16, (d.asal.max() - d.asal.min()) * 0.16
    xlo, xhi = min(d.indep.min(), 0) - pad_x, d.indep.max() + pad_x * 1.2
    ylo, yhi = min(d.asal.min(), 0) - pad_y, d.asal.max() + pad_y
    lim = max(abs(xlo), abs(ylo), xhi, yhi)
    xlo, xhi, ylo, yhi = xlo, xhi, ylo, yhi
    ax.set_xlim(xlo, xhi)
    ax.set_ylim(ylo, yhi)

    ax.axhline(0, color=AXIS, lw=1.2, zorder=1)
    ax.axvline(0, color=AXIS, lw=1.2, zorder=1)
    diag = np.array([min(xlo, ylo), max(xhi, yhi)])
    ax.plot(diag, diag, color="#B8C2C9", lw=1.0, ls=(0, (4, 4)), zorder=1)
    ax.grid(axis="both", color=GRID, lw=0.8, zorder=0)
    ax.set_axisbelow(True)
    ax.tick_params(length=0, pad=8, labelsize=11, labelcolor=MUTED)
    for sp in ("top", "right", "left", "bottom"):
        ax.spines[sp].set_visible(False)
    fmt = lambda v, _: es(v, 0)
    ax.xaxis.set_major_formatter(plt.FuncFormatter(fmt))
    ax.yaxis.set_major_formatter(plt.FuncFormatter(fmt))

    qk = dict(fontsize=10.5, color=MUTED, fontweight="bold", va="center")
    ax.text(0.015, 0.965, "Más asalariados, menos independientes", transform=ax.transAxes, ha="left", **qk)
    ax.text(0.985, 0.965, "Crecen ambos tipos de empleo", transform=ax.transAxes, ha="right", **qk)
    ax.text(0.015, 0.035, "Caen ambos tipos de empleo", transform=ax.transAxes, ha="left", **qk)
    ax.text(0.985, 0.035, "Menos asalariados, más independientes", transform=ax.transAxes, ha="right", **qk)

    k_pt = 52 / np.sqrt(d.emp0.max())
    items = []
    for name, r in d.sort_values("emp0", ascending=False).iterrows():
        rad_pt = max(5.5, k_pt * np.sqrt(r.emp0))
        color = GAIN if r.asal >= r.indep else LOSS
        ax.scatter([r.indep], [r.asal], s=(2 * rad_pt) ** 2 * 0.5, color=color, alpha=0.80, edgecolor="white",
                   linewidth=1.8, zorder=3)
        items.append({"x": r.indep, "y": r.asal, "r_px": rad_pt * fig.dpi / 72 * 0.72,
                      "text": SHORT.get(name, name), "fs": 10.3})
    bub.place_labels(fig, ax, items, obstacles=[(0.0, 0.10, 0.27, 0.42), (0.0, 0.0, 0.30, 0.075), (0.68, 0.0, 1.0, 0.075), (0.0, 0.925, 0.36, 1.0), (0.72, 0.925, 1.0, 1.0)])

    # escala de tamanos (concentricos, zona libre abajo a la izquierda)
    ax_h = ax.get_window_extent().height
    cx_f, base_f = 0.075, 0.12
    for v in (2000, 1000, 300):
        r_pt = k_pt * np.sqrt(v) * 0.7071
        r_px = r_pt * fig.dpi / 72
        yc = base_f + r_px / ax_h
        ax.scatter([cx_f], [yc], s=(2 * k_pt * np.sqrt(v)) ** 2 * 0.5, transform=ax.transAxes, facecolor="none",
                   edgecolor="#A9B4BC", linewidth=1.1, zorder=3)
        ytop = yc + r_px / ax_h
        ax.plot([cx_f, cx_f + 0.08], [ytop, ytop], transform=ax.transAxes, color="#A9B4BC", lw=0.9, zorder=3)
        ax.text(cx_f + 0.085, ytop, es(v, 0), transform=ax.transAxes, fontsize=10, color=INK2, va="center", ha="left")
        if v == 2000:
            ax.text(cx_f - 0.03, ytop + 0.035, "Ocupados del sector en " + str(y0) + " (miles)", transform=ax.transAxes,
                    fontsize=10, color=MUTED, va="bottom", ha="left")

    fig.text(0.04, 0.945, f"Empleo asalariado e independiente por sector, {y0}–{y1}", fontsize=22,
             fontweight="bold", color=INK, va="top")
    fig.text(0.04, 0.895, "Cambio en miles de ocupados de cada sector. Azul: el sector crea más empleo asalariado que independiente.",
             fontsize=13, color=INK2, va="top")
    fig.text(0.04, 0.855,
             f"En total, el empleo asalariado cambió {es(tot_a, 0)} mil ocupados y el independiente {es(tot_i, 0)} mil.",
             fontsize=11.5, color=INK2, va="top")
    ax.set_xlabel("Cambio en trabajadores independientes, es decir, por cuenta propia (miles)",
                  fontsize=12, color=INK2, labelpad=10)
    ax.set_ylabel("Cambio en trabajadores asalariados (miles)", fontsize=12, color=INK2, labelpad=10)
    fig.text(0.04, 0.03, "Fuente: cálculos propios con GEIH (ocupados expandidos). No hay GEIH comparable para 2020. "
             "La línea punteada marca igual cambio en ambos tipos de empleo.", fontsize=9.5, color=MUTED, va="bottom")
    for dirp in OUT_DIRS:
        fig.savefig(dirp / filename, dpi=200, facecolor="white")
    plt.close(fig)
    return d


if __name__ == "__main__":
    g = load_table()
    for y0, y1 in ((2010, 2025), (2010, 2019), (2019, 2025)):
        d = build(g, y0, y1, f"fig_burbujas_asalariados_independientes_{y0}_{y1}.png")
        print(y0, y1, "asalariados", round(d.asal.sum()), "independientes", round(d.indep.sum()))
