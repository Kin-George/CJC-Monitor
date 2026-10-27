"""Figura de validacion: PTF y aporte del trabajo, replica propia (v0 y v1) frente al Cuadro 1 del DANE."""
from pathlib import Path

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
from matplotlib import font_manager

ROOT = Path(__file__).resolve().parents[1]
PTF = ROOT / "Datos" / "Processed"
OUT = ROOT / "Outputs" / "Figures"

for f in ("segoeui.ttf", "segoeuib.ttf"):
    p = Path(r"C:\Windows\Fonts") / f
    if p.exists():
        font_manager.fontManager.addfont(str(p))
plt.rcParams["font.family"] = ["Segoe UI", "DejaVu Sans"]
INK, INK2, MUTED, GRID = "#14202B", "#46535F", "#7A8791", "#E7EBEE"
DANE, V0, V1 = "#14202B", "#C9D1D6", "#2A78D6"

v1 = pd.read_csv(PTF / "validacion_total_economia_v1.csv")
v2 = pd.read_csv(PTF / "validacion_total_economia_v2.csv")
per = list(v1["periodo"])
x = np.arange(len(per))

fig, axes = plt.subplots(2, 1, figsize=(11, 8.6), dpi=200, facecolor="white", gridspec_kw={"hspace": 0.5, "top": 0.83})


def panel(ax, title, dane, v1v, v0v=None, ylim=None):
    ax.set_title(title, loc="left", fontsize=13, fontweight="bold", color=INK, pad=10)
    w = 0.27 if v0v is not None else 0.34
    off = (-w, 0, w) if v0v is not None else (-w / 2, w / 2)
    ax.bar(x + off[0], dane, w, color=DANE, label="DANE (Cuadro 1)", zorder=3)
    k = 1
    if v0v is not None:
        ax.bar(x + off[1], v0v, w, color=V0, label="Replica con horas habituales (v1)", zorder=3)
        k = 2
    ax.bar(x + off[k], v1v, w, color=V1, label="Replica con horas efectivas (v2)", zorder=3)
    ax.axhline(0, color="#C9D1D6", lw=1.2, zorder=2)
    ax.grid(axis="y", color=GRID, zorder=0)
    ax.set_axisbelow(True)
    for sp in ("top", "right", "left"):
        ax.spines[sp].set_visible(False)
    ax.spines["bottom"].set_color("#C9D1D6")
    ax.tick_params(length=0, labelsize=10.5, labelcolor=MUTED)
    ax.set_xticks(x)
    ax.set_xticklabels([p.replace("2020-21", "2020–21") for p in per])
    ax.set_ylabel("Puntos porcentuales", fontsize=10.5, color=INK2)
    if ylim:
        ax.set_ylim(*ylim)


panel(axes[0], "Productividad total de los factores (aporte al crecimiento del valor agregado)", v1["PTF_DANE"], v2["PTF_propia"], v1["PTF_propia"], (-8, 3.6))
axes[0].legend(loc="upper left", frameon=False, fontsize=10.5, labelcolor=INK2, ncol=3)
panel(axes[1], "Aporte del trabajo (servicios laborales)", v1["L_DANE"], v2["L_propia"], v1["L_propia"])
axes[1].legend(loc="upper left", frameon=False, fontsize=10.5, labelcolor=INK2, ncol=3)
axes[1].set_ylim(-1.5, 7.5)
fig.text(0.04, 0.975, "Replicación de la PTF del DANE para el total de la economía, 2015–2024", fontsize=16, fontweight="bold", color=INK, va="top")
mae = lambda a, b: np.nanmean(np.abs(np.array(a) - np.array(b)))
post = v2["periodo"].isin(["2022", "2023", "2024"])
fig.text(0.04, 0.93, f"Con horas efectivas (P6850) el error de la PTF en 2022–2024 es de {mae(v2.loc[post, 'PTF_propia'], v2.loc[post, 'PTF_DANE']):.1f} pp, frente a "
         f"{mae(v1.loc[post, 'PTF_propia'], v1.loc[post, 'PTF_DANE']):.1f} pp con horas habituales. Antes de 2020 la brecha sigue.", fontsize=10.5, color=INK2, va="top")
fig.text(0.04, 0.02, "Fuente: cálculos propios con COU y stock de capital del DANE, GEIH (2020 sin GEIH comparable: 2020–21 es el cambio 2019–2021 frente a 2020 más 2021 del DANE; ese tramo mezcla marcos muestrales y no es confiable).",
         fontsize=8.5, color=MUTED, va="bottom")
OUT.mkdir(parents=True, exist_ok=True)
fig.savefig(OUT / "fig_ptf_replica_vs_dane.png", dpi=200, facecolor="white", bbox_inches="tight")
print("ok")
