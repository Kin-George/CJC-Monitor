"""Figura: descomposición dinámica de Olley-Pakes de la productividad laboral de la industria colombiana (EAM, empresas).

A. Componentes de la DOPD por ventana.   B. Contribución de la entrada y de la reasignación según el método (DOPD frente a GR y FHK).
Entradas: Outputs/tables/dopd_laboral_ventanas.csv y dopd_laboral_comparacion.csv   Salida: Outputs/Figures/fig_dopd_laboral.png
"""
from pathlib import Path

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
from matplotlib import font_manager

ROOT = Path(__file__).resolve().parents[1]
for f in ("segoeui.ttf", "segoeuib.ttf"):
    p = Path(r"C:\Windows\Fonts") / f
    if p.exists():
        font_manager.fontManager.addfont(str(p))
plt.rcParams["font.family"] = ["Segoe UI", "DejaVu Sans"]
INK, INK2, MUTED, GRID, AXIS = "#14202B", "#46535F", "#7A8791", "#E7EBEE", "#C9D1D6"
COL = {"dentro_media_sobrevivientes": "#9AA7B0", "reasignacion_covarianza_sobrevivientes": "#2A78D6", "entrada": "#EB6834", "salida": "#1BAF7A"}
LAB = {"dentro_media_sobrevivientes": "Mejora dentro de las empresas (media sin ponderar)", "reasignacion_covarianza_sobrevivientes": "Reasignación entre sobrevivientes (covarianza)",
       "entrada": "Entrada", "salida": "Salida"}
T = ROOT / "Outputs" / "tables"
res = pd.read_csv(T / "dopd_laboral_ventanas.csv")
cmp_ = pd.read_csv(T / "dopd_laboral_comparacion.csv")
orden = ["2010-2015", "2015-2019", "2019-2024"]

fig, (ax, ax2) = plt.subplots(1, 2, figsize=(13, 5.6), dpi=200, facecolor="white", gridspec_kw={"width_ratios": [1.15, 1], "wspace": 0.25})
for a in (ax, ax2):
    a.grid(axis="y", color=GRID, zorder=0)
    a.set_axisbelow(True)
    for sp in ("top", "right"):
        a.spines[sp].set_visible(False)
    for sp in ("left", "bottom"):
        a.spines[sp].set_color(AXIS)
    a.tick_params(length=0, labelsize=10.5, labelcolor=MUTED)

# A
for i, v in enumerate(orden):
    d = res[res.ventana == v].set_index("componente")
    pos = neg = 0.0
    for c in COL:
        x = d.loc[c, "valor_pp"]
        if x >= 0:
            ax.bar(i, x, 0.55, bottom=pos, color=COL[c], edgecolor="white", linewidth=0.8, zorder=3, label=LAB[c] if i == 0 else None)
            pos += x
        else:
            ax.bar(i, x, 0.55, bottom=neg, color=COL[c], edgecolor="white", linewidth=0.8, zorder=3, label=LAB[c] if i == 0 else None)
            neg += x
    tot = d["cambio_total"].iloc[0]
    ax.scatter([i], [tot], marker="_", s=420, color=INK, linewidths=2.4, zorder=5, label="Cambio total" if i == 0 else None)
    ax.text(i + 0.31, tot, f"{tot:.1f}", va="center", fontsize=10.5, color=INK, fontweight="bold")
ax.axhline(0, color=AXIS, lw=1.2, zorder=2)
ax.set_xticks(range(len(orden)))
ax.set_xticklabels([o.replace("-", " → ") for o in orden], fontsize=11, color=INK2)
ax.set_ylabel("Cambio de la productividad laboral (puntos log × 100)", fontsize=10.5, color=INK2)
ax.set_title("A. Descomposición del cambio agregado", loc="left", fontsize=12, fontweight="bold", color=INK, pad=10)
ax.legend(loc="upper left", frameon=False, fontsize=9, labelcolor=INK2)
ax.set_ylim(-9, 31)

# B
w = 0.25
for j, (met, col) in enumerate((("DOPD", "#2A78D6"), ("GR", "#9AA7B0"), ("FHK", "#C9D1D6"))):
    ax2.bar(np.arange(3) + (j - 1) * w, cmp_.set_index("ventana").loc[orden, f"{met}_entrada"], w * 0.92, color=col, edgecolor="white", zorder=3, label=met)
ax2.axhline(0, color=AXIS, lw=1.2, zorder=2)
ax2.set_xticks(range(3))
ax2.set_xticklabels([o.replace("-", " → ") for o in orden], fontsize=11, color=INK2)
ax2.set_ylabel("Contribución de la entrada (puntos log × 100)", fontsize=10.5, color=INK2)
ax2.set_title("B. La entrada según el método", loc="left", fontsize=12, fontweight="bold", color=INK, pad=10)
ax2.legend(loc="lower right", frameon=False, fontsize=10, labelcolor=INK2)
ax2.set_ylim(-1.5, 0.05)
ax2.text(0.37, 0.32, "GR y FHK dan a la entrada un valor\nmayor (menos negativo) que la DOPD:\nel artículo muestra que lo sobreestiman.", transform=ax2.transAxes, va="top", fontsize=9.5, color=INK2, linespacing=1.4)

fig.text(0.04, 0.975, "Productividad laboral de la industria colombiana: de dónde viene el cambio", fontsize=16, fontweight="bold", color=INK, va="top")
fig.text(0.04, 0.925, "Descomposición dinámica de Olley–Pakes (Melitz y Polanec, 2015). EAM, empresas, valor agregado real por trabajador y participaciones de empleo.",
         fontsize=10.5, color=INK2, va="top")
fig.text(0.04, 0.012, "Fuente: DANE (EAM; IPP industria) y cálculos propios. La EAM cubre establecimientos con 10 o más personas o producción sobre un umbral: la entrada y la salida incluyen empresas que cruzan ese umbral.",
         fontsize=8.5, color=MUTED, va="bottom")
fig.subplots_adjust(top=0.82, bottom=0.12, left=0.07, right=0.98)
out = ROOT / "Outputs" / "Figures"
out.mkdir(parents=True, exist_ok=True)
fig.savefig(out / "fig_dopd_laboral.png", dpi=200, facecolor="white")
print("ok", out / "fig_dopd_laboral.png")
