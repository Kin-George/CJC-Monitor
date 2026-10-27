"""Barras apiladas de empleo asalariado e independiente por grupo de sectores (version moderna).

Replica para Colombia el panel (b) de la figura de Laos: para cada grupo de sectores, dos barras
(anio inicial y final) apiladas en independientes (solo cuenta propia) y asalariados, en millones.
Una figura por periodo: 2010-2025, 2010-2019 y 2019-2025.

Insumo: Outputs/Tables/ocupados_sector_tipo_empleo_2010_2025.csv (lo genera 02.11 a partir de la GEIH).
"""

import importlib.util
from pathlib import Path

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

ROOT = Path(__file__).resolve().parents[3]
TABLE = ROOT / "Outputs" / "Tables" / "ocupados_sector_tipo_empleo_2010_2025.csv"
OUT_DIRS = [ROOT / "Outputs" / "Figures", ROOT / "Paper" / "figures"]

_spec = importlib.util.spec_from_file_location("bubbles", Path(__file__).with_name("02.10_Burbujas_CambioEstructural.py"))
bub = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(bub)
INK, INK2, MUTED, GRID, AXIS, GAIN, LOSS = bub.INK, bub.INK2, bub.MUTED, bub.GRID, bub.AXIS, bub.GAIN, bub.LOSS
es = bub.es

WAGE, SELF = GAIN, LOSS  # azul = asalariados, naranja = independientes (igual que en las burbujas)

GROUPS = [
    ("Agropecuario", ["Agricultura, ganadería, silvicultura y pesca"]),
    ("Minas,\nelectricidad\ny agua", ["Minas y canteras", "Electricidad, gas, agua y saneamiento"]),
    ("Manufactura", ["Industrias manufactureras"]),
    ("Construcción", ["Construcción"]),
    ("Comercio y\nreparación", ["Comercio y reparación"]),
    ("Alojamiento\ny comida", ["Alojamiento y servicios de comida"]),
    ("Transporte y\nalmacena-\nmiento", ["Transporte y almacenamiento"]),
    ("Información,\nfinanzas y servicios\nprofesionales", ["Información y comunicaciones", "Actividades financieras y de seguros",
                                                          "Inmobiliarias, profesionales y administrativas"]),
    ("Administración\npública, educación\ny salud", ["Administración pública y defensa", "Educación", "Salud y asistencia social"]),
    ("Artes y\nhogares", ["Artes, recreación y otros servicios", "Hogares como empleadores"]),
]


def build(g, y0, y1, filename):
    rows = []
    for name, members in GROUPS:
        sub = g[g.sector.isin(members)]
        for y in (y0, y1):
            s = sub[sub.anio == y]
            rows.append({"grupo": name, "anio": y, "asal": s.asalariado.sum() / 1e6, "indep": s.independiente.sum() / 1e6})
    d = pd.DataFrame(rows)

    fig = plt.figure(figsize=(13.5, 8.0), dpi=200, facecolor="white")
    ax = fig.add_axes([0.07, 0.255, 0.915, 0.50])
    n = len(GROUPS)
    bw, gap = 0.36, 0.14
    ymax = float(np.ceil((d.asal + d.indep).max() * 4) / 4) + 0.25
    ax.set_xlim(-0.6, n - 0.4)
    ax.set_ylim(0, ymax)
    ax.set_yticks(np.arange(0, ymax + 0.01, 0.5))
    ax.set_yticklabels([es(v, 1) for v in np.arange(0, ymax + 0.01, 0.5)], fontsize=11, color=MUTED)
    ax.grid(axis="y", color=GRID, lw=0.9, zorder=0)
    ax.set_axisbelow(True)
    ax.tick_params(length=0, pad=8)
    for sp in ("top", "right", "left"):
        ax.spines[sp].set_visible(False)
    ax.spines["bottom"].set_color(AXIS)
    ax.set_xticks([])

    for i, (name, _) in enumerate(GROUPS):
        for j, y in enumerate((y0, y1)):
            r = d[(d.grupo == name) & (d.anio == y)].iloc[0]
            x = i + (-0.5 if j == 0 else 0.5) * (bw + gap / 2) * 1.0
            x = i + (j - 0.5) * (bw + 0.04)
            ax.bar(x, r.indep, width=bw, color=SELF, edgecolor="white", linewidth=1.2, zorder=3, label="Independientes (cuenta propia)" if (i == 0 and j == 0) else None)
            ax.bar(x, r.asal, bottom=r.indep, width=bw, color=WAGE, edgecolor="white", linewidth=1.2, zorder=3,
                   label="Asalariados" if (i == 0 and j == 0) else None)
            tot = r.indep + r.asal
            ax.text(x, tot + ymax * 0.012, es(tot, 2), ha="center", va="bottom", fontsize=9.5, color=INK2)
            ax.text(x, -ymax * 0.035, str(y), ha="center", va="top", fontsize=10.5, color=MUTED)
        ax.text(i, -ymax * 0.115, name, ha="center", va="top", fontsize=10.2, color=INK, linespacing=1.15)
        if i < n - 1:
            ax.axvline(i + 0.5, ymin=-0.32, ymax=0.0, color=AXIS, lw=1.0, clip_on=False)

    handles, labels = ax.get_legend_handles_labels()
    fig.legend(handles[::-1], labels[::-1], loc="upper left", bbox_to_anchor=(0.075, 0.835), ncol=2, frameon=False,
               fontsize=12, labelcolor=INK2, handlelength=1.0, handleheight=1.0, columnspacing=2.0)

    # titulos y lectura
    a0, a1 = d[d.anio == y0], d[d.anio == y1]
    d_as, d_in = a1.asal.sum() - a0.asal.sum(), a1.indep.sum() - a0.indep.sum()
    fig.text(0.04, 0.955, f"Empleo asalariado e independiente por sector, {y0} y {y1}", fontsize=22, fontweight="bold",
             color=INK, va="top")
    fig.text(0.04, 0.905, "Ocupados en millones, apilados en independientes (cuenta propia) y asalariados",
             fontsize=13, color=INK2, va="top")
    fig.text(0.04, 0.868, f"Entre {y0} y {y1} el empleo asalariado de estos sectores cambió {es(d_as, 1)} millones y el independiente {es(d_in, 1)} millones.",
             fontsize=11.5, color=INK2, va="top")
    ax.set_ylabel("Millones de ocupados", fontsize=12, color=INK2, labelpad=10)
    fig.text(0.04, 0.03, "Fuente: cálculos propios con GEIH (ocupados expandidos). No hay GEIH comparable para 2020. "
             "No incluye patrones, familiares sin remuneración ni otras posiciones ocupacionales.", fontsize=9.5, color=MUTED, va="bottom")
    for dirp in OUT_DIRS:
        fig.savefig(dirp / filename, dpi=200, facecolor="white")
    plt.close(fig)
    return d


if __name__ == "__main__":
    g = pd.read_csv(TABLE)
    for y0, y1 in ((2010, 2025), (2010, 2019), (2019, 2025)):
        d = build(g, y0, y1, f"fig_barras_asalariados_independientes_{y0}_{y1}.png")
        print(y0, y1, "ok")
