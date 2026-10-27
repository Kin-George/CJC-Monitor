"""Figura que explica como se compara la replica con el DANE.

A. El crecimiento del valor agregado se descompone en trabajo + capital + PTF (residual); cada año se pone la barra del
   DANE al lado de la barra de la replica.
B. Chequeo por actividad: crecimiento de horas por rama implicito en el DANE (anexo, Cuadros 4-23) contra la replica.
C. Chequeo de la PTF total: cada punto es un año; sobre la diagonal, la replica coincide con el DANE.
"""
from pathlib import Path

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
from matplotlib import font_manager

ROOT = Path(__file__).resolve().parents[1]
PTF = ROOT / "Datos" / "Processed"
TAB = ROOT / "Outputs" / "tables"
for f in ("segoeui.ttf", "segoeuib.ttf"):
    p = Path(r"C:\Windows\Fonts") / f
    if p.exists():
        font_manager.fontManager.addfont(str(p))
plt.rcParams["font.family"] = ["Segoe UI", "DejaVu Sans"]
INK, INK2, MUTED, GRID, AXIS = "#14202B", "#46535F", "#7A8791", "#E7EBEE", "#C9D1D6"
C_L, C_K, C_T = "#2A78D6", "#EB6834", "#1BAF7A"

v2 = pd.read_csv(PTF / "validacion_total_economia_v2.csv")
v2 = v2[v2.periodo != "2020-21"].copy()
v2["anio"] = v2.periodo.astype(int)
e25 = pd.read_csv(TAB / "ptf_2025_replica.csv").iloc[0]
rows = []
for _, r in v2.iterrows():
    rows.append({"anio": r.anio, "dane": (r.L_DANE, r.K_DANE, r.PTF_DANE), "rep": (r.L_propia, r.K_propia, r.PTF_propia), "est": False})
rows.append({"anio": 2025, "dane": (e25.L_DANE, e25.K_DANE, e25.PTF_DANE), "rep": (e25.L_replica, e25.K_estimado, e25.PTF_estimado), "est": True})
d = rows

fig = plt.figure(figsize=(13, 10.4), dpi=200, facecolor="white")
gs = fig.add_gridspec(2, 2, height_ratios=[1.25, 1], hspace=0.42, wspace=0.22, left=0.07, right=0.98, top=0.86, bottom=0.07)
axA = fig.add_subplot(gs[0, :])
axB = fig.add_subplot(gs[1, 0])
axC = fig.add_subplot(gs[1, 1])


def style(ax):
    ax.grid(color=GRID, zorder=0)
    ax.set_axisbelow(True)
    for sp in ("top", "right"):
        ax.spines[sp].set_visible(False)
    for sp in ("left", "bottom"):
        ax.spines[sp].set_color(AXIS)
    ax.tick_params(length=0, labelsize=10.5, labelcolor=MUTED)


# ---- A
style(axA)
axA.grid(axis="x", visible=False)
w = 0.36
xt, xl = [], []
for i, r in enumerate(d):
    for k, (key, off) in enumerate((("dane", -w / 2 - 0.02), ("rep", w / 2 + 0.02))):
        vals = r[key]
        pos, neg = 0.0, 0.0
        for v, c in zip(vals, (C_L, C_K, C_T)):
            hatch = "////" if (key == "rep" and r["est"]) else None
            if v >= 0:
                axA.bar(i + off, v, w, bottom=pos, color=c, edgecolor="white", linewidth=0.8, hatch=hatch, zorder=3)
                pos += v
            else:
                axA.bar(i + off, v, w, bottom=neg, color=c, edgecolor="white", linewidth=0.8, hatch=hatch, zorder=3)
                neg += v
        axA.scatter([i + off], [sum(vals)], marker="_", s=260, color=INK, linewidths=2, zorder=5)
        axA.text(i + off, -2.55 if False else axA.get_ylim()[0], "", ha="center")
    xt.append(i)
    xl.append(str(r["anio"]))
axA.set_xticks(xt)
axA.set_xticklabels(xl)
axA.axhline(0, color=AXIS, lw=1.2, zorder=2)
axA.set_ylim(axA.get_ylim()[0], 7.5)
ymin, ymax = axA.get_ylim()
for i, r in enumerate(d):
    axA.text(i - w / 2 - 0.02, ymin + 0.08, "DANE", ha="center", va="bottom", fontsize=7.5, color=MUTED)
    axA.text(i + w / 2 + 0.02, ymin + 0.08, "réplica", ha="center", va="bottom", fontsize=7.5, color=MUTED)
axA.axvspan(len(d) - 1.5, len(d) - 0.5, color="#F4F6F8", zorder=0)
axA.axvline(4.5, color=AXIS, lw=1, ls=(0, (4, 4)))
axA.text(4.45, 6.6, "2020–21: sin GEIH comparable", fontsize=9.5, color=MUTED, va="top", ha="right")
axA.set_ylabel("Aporte al crecimiento del valor agregado (pp)", fontsize=10.5, color=INK2)
axA.set_title("A. El crecimiento del valor agregado se descompone en trabajo + capital + PTF; en cada año, DANE (izquierda) y réplica (derecha)",
              loc="left", fontsize=12, fontweight="bold", color=INK, pad=12)
handles = [plt.Rectangle((0, 0), 1, 1, color=c) for c in (C_L, C_K, C_T)] + [plt.Line2D([0], [0], marker="_", color=INK, lw=0, ms=12, mew=2),
                                                                         plt.Rectangle((0, 0), 1, 1, facecolor="white", edgecolor=INK2, hatch="////")]
axA.legend(handles, ["Trabajo (servicios laborales)", "Capital", "PTF (residuo)", "Crecimiento del valor agregado", "2025: réplica estimada"], loc="upper left", ncol=5, frameon=False,
           fontsize=9.5, labelcolor=INK2, bbox_to_anchor=(0, 1.0), columnspacing=1.2, handlelength=1.2)

# ---- B
style(axB)
diag = pd.read_csv(TAB / "ptf_diagnostico_por_actividad.csv")
pre = diag[diag.anio.between(2015, 2019)]
post = diag[diag.anio.between(2022, 2024)]
lim = 20
axB.plot([-lim, lim], [-lim, lim], color=INK2, lw=1.2, ls=(0, (4, 4)), zorder=2)
axB.scatter(pre.dlnH_DANE * 100, pre.dlnH * 100, s=34, color="#9AA7B0", alpha=0.85, edgecolor="white", linewidth=0.6, zorder=3, label="2015–2019")
axB.scatter(post.dlnH_DANE * 100, post.dlnH * 100, s=46, color=C_L, edgecolor="white", linewidth=0.8, zorder=4, label="2022–2024")
mae = lambda s: (s.dlnH - s.dlnH_DANE).abs().mean() * 100
axB.set_xlim(-lim, lim)
axB.set_ylim(-lim, lim)
axB.set_xlabel("Crecimiento de horas implícito en el DANE (%)", fontsize=10.5, color=INK2)
axB.set_ylabel("Crecimiento de horas, réplica (%)", fontsize=10.5, color=INK2)
axB.set_title("B. Horas por actividad (9 ramas × año)", loc="left", fontsize=12, fontweight="bold", color=INK, pad=10)
axB.legend(loc="upper left", frameon=False, fontsize=10, labelcolor=INK2)
axB.text(0.98, 0.04, f"error medio 2022–24: {mae(post):.1f} pp\nerror medio 2015–19: {mae(pre):.1f} pp", transform=axB.transAxes, ha="right", va="bottom",
         fontsize=9.5, color=INK2, linespacing=1.4)

# ---- C
style(axC)
axC.plot([-2, 5], [-2, 5], color=INK2, lw=1.2, ls=(0, (4, 4)), zorder=2)
a = [r for r in d if r["anio"] <= 2019]
b = [r for r in d if 2022 <= r["anio"] <= 2024]
c25 = [r for r in d if r["anio"] == 2025]
axC.scatter([r["dane"][2] for r in a], [r["rep"][2] for r in a], s=60, color="#9AA7B0", edgecolor="white", linewidth=0.8, zorder=3, label="2015–2019")
axC.scatter([r["dane"][2] for r in b], [r["rep"][2] for r in b], s=80, color=C_L, edgecolor="white", linewidth=1, zorder=4, label="2022–2024")
axC.errorbar([c25[0]["dane"][2]], [c25[0]["rep"][2]], yerr=0.24, fmt="o", ms=9, mfc="white", mec=C_L, mew=2, ecolor=C_L, capsize=4, zorder=5, label="2025 (estimada)")
for r in b + c25:
    axC.annotate(str(r["anio"]), (r["dane"][2], r["rep"][2]), xytext=(7, -10), textcoords="offset points", fontsize=9.5, color=INK2)
axC.set_xlim(-1.5, 1.7)
axC.set_ylim(-1.5, 1.7)
axC.set_xlabel("PTF del DANE (pp)", fontsize=10.5, color=INK2)
axC.set_ylabel("PTF de la réplica (pp)", fontsize=10.5, color=INK2)
axC.set_title("C. PTF total, un punto por año", loc="left", fontsize=12, fontweight="bold", color=INK, pad=10)
axC.legend(loc="upper left", frameon=False, fontsize=10, labelcolor=INK2)
e_post = np.mean([abs(r["rep"][2] - r["dane"][2]) for r in b])
e_pre = np.mean([abs(r["rep"][2] - r["dane"][2]) for r in a])
axC.text(0.98, 0.04, f"error medio 2022–24: {e_post:.2f} pp\nerror medio 2015–19: {e_pre:.2f} pp", transform=axC.transAxes, ha="right", va="bottom",
         fontsize=9.5, color=INK2, linespacing=1.4)

fig.text(0.04, 0.975, "Cómo comparamos nuestra réplica de la PTF con la del DANE", fontsize=17, fontweight="bold", color=INK, va="top")
fig.text(0.04, 0.935, "Mismas fórmulas, mismas fuentes públicas; solo cambian las horas (efectivas, P6850). La diagonal marca coincidencia exacta.", fontsize=11, color=INK2, va="top")
fig.text(0.04, 0.012, "Fuente: DANE (anex-PTF-Productividad-2025, Cuadros 1 y 4-23); cálculos propios con COU, stock de capital, PIB trimestral y GEIH. Horas implícitas del DANE = aporte de las horas ÷ peso del trabajo en la producción.",
         fontsize=8.5, color=MUTED, va="bottom")
fig.savefig(ROOT / "Outputs" / "Figures" / "fig_ptf_comparacion_metodo.png", dpi=200, facecolor="white")
print("ok")
