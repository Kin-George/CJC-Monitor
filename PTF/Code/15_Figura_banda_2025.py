"""Figura: PTF de 2025 estimada con nuestro metodo (con su banda) frente al dato preliminar del DANE (2025pr).

Banda interna: error medio de la regla de persistencia del capital (0,24 pp, 12_PTF_2025_completar.py).
Banda externa: se le suma el error medio de usar pesos del año anterior en vez de los del año (0,04 pp, prueba retrospectiva).
Entrada: Outputs/tables/ptf_2025_replica.csv
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
C_L, C_K, C_T, C_DANE = "#2A78D6", "#EB6834", "#1BAF7A", "#14202B"
B_CAP, B_PES = 0.24, 0.04

e = pd.read_csv(ROOT / "Outputs" / "tables" / "ptf_2025_replica.csv").iloc[0]
est, dane = e.PTF_estimado, e.PTF_DANE
lo_i, hi_i = est - B_CAP, est + B_CAP
lo_o, hi_o = est - B_CAP - B_PES, est + B_CAP + B_PES

fig, (ax, ax2) = plt.subplots(1, 2, figsize=(12.5, 5.4), dpi=200, facecolor="white", gridspec_kw={"width_ratios": [1.15, 1], "wspace": 0.28})
for a in (ax, ax2):
    a.grid(color=GRID, zorder=0)
    a.set_axisbelow(True)
    for sp in ("top", "right"):
        a.spines[sp].set_visible(False)
    for sp in ("left", "bottom"):
        a.spines[sp].set_color(AXIS)
    a.tick_params(length=0, labelsize=10.5, labelcolor=MUTED)

# ---- A. PTF 2025: estimación con banda vs DANE
ax.grid(axis="x", visible=False)
ax.barh([0], [hi_o - lo_o], left=lo_o, height=0.5, color="#DCE8F8", zorder=2, label="Banda con pesos (±%.2f pp)" % (B_CAP + B_PES))
ax.barh([0], [hi_i - lo_i], left=lo_i, height=0.5, color="#A9C8F0", zorder=3, label="Banda del capital (±%.2f pp)" % B_CAP)
ax.scatter([est], [0], s=170, color=C_L, edgecolor="white", linewidth=1.6, zorder=5, label="Estimación propia: %.2f pp" % est)
ax.scatter([dane], [0], s=170, marker="D", color=C_DANE, edgecolor="white", linewidth=1.6, zorder=6, label="DANE preliminar (2025pr): %.2f pp" % dane)
ax.axvline(0, color=AXIS, lw=1.2, zorder=1)
ax.set_xlim(0.2, 1.3)
ax.set_ylim(-0.7, 0.7)
ax.set_yticks([])
ax.set_xlabel("Aporte de la PTF al crecimiento del valor agregado, 2025 (pp)", fontsize=10.5, color=INK2)
ax.set_title("A. PTF 2025: estimación propia y dato del DANE", loc="left", fontsize=12, fontweight="bold", color=INK, pad=10)
ax.annotate(f"{lo_o:.2f}", (lo_o, -0.30), ha="center", fontsize=9.5, color=MUTED)
ax.annotate(f"{hi_o:.2f}", (hi_o, -0.30), ha="center", fontsize=9.5, color=MUTED)
ax.text(0.02, 0.97, f"El dato del DANE cae {abs(dane - est):.2f} pp de la estimación,\ndentro de la banda interna.", transform=ax.transAxes, va="top", fontsize=10.5, color=INK2, linespacing=1.4)
ax.legend(loc="lower center", frameon=False, fontsize=10, labelcolor=INK2, bbox_to_anchor=(0.5, -0.02))

# ---- B. descomposición: DANE vs estimación
comp = [("Valor\nagregado", e.VA_DANE, e.VA_estimado, INK2), ("Trabajo", e.L_DANE, e.L_replica, C_L), ("Capital", e.K_DANE, e.K_estimado, C_K), ("PTF\n(residuo)", e.PTF_DANE, e.PTF_estimado, C_T)]
x = np.arange(len(comp))
w = 0.36
for i, (name, d, r, col) in enumerate(comp):
    ax2.bar(i - w / 2 - 0.02, d, w, color=col, alpha=0.45, zorder=3, edgecolor="white")
    ax2.bar(i + w / 2 + 0.02, r, w, color=col, zorder=3, edgecolor="white", hatch="////" if name == "Capital" else None)
    ax2.text(i - w / 2 - 0.02, d + 0.05, f"{d:.2f}", ha="center", fontsize=9.5, color=INK2)
    ax2.text(i + w / 2 + 0.02, r + 0.05, f"{r:.2f}", ha="center", fontsize=9.5, color=INK)
ax2.set_xticks(x)
ax2.set_xticklabels([c[0] for c in comp], fontsize=10.5, color=INK2)
ax2.set_ylim(0, 3.1)
ax2.set_ylabel("Aporte al crecimiento (pp)", fontsize=10.5, color=INK2)
ax2.set_title("B. De dónde viene la diferencia", loc="left", fontsize=12, fontweight="bold", color=INK, pad=10)
h = [plt.Rectangle((0, 0), 1, 1, color="#7A8791", alpha=0.45), plt.Rectangle((0, 0), 1, 1, color="#46535F"), plt.Rectangle((0, 0), 1, 1, facecolor="white", edgecolor=INK2, hatch="////")]
ax2.legend(h, ["DANE (2025pr)", "Estimación propia", "Capital estimado por persistencia"], loc="upper right", frameon=False, fontsize=9.5, labelcolor=INK2)

fig.text(0.04, 0.975, "PTF de Colombia 2025: nuestra estimación frente al dato preliminar del DANE", fontsize=16, fontweight="bold", color=INK, va="top")
fig.text(0.04, 0.925, "Valor agregado del PIB trimestral, trabajo con horas efectivas de la GEIH 2025, capital proyectado con la persistencia de 2024 y pesos de 2024.",
         fontsize=10.5, color=INK2, va="top")
fig.text(0.04, 0.012, "Fuente: DANE (anex-PTF-Productividad-2025, Cuadro 1; PIB trimestral); cálculos propios con GEIH, COU y stock de capital de 2024. "
         "Bandas: error medio de la regla del capital (2015–2024) y de usar pesos del año anterior.", fontsize=8.5, color=MUTED, va="bottom")
fig.subplots_adjust(top=0.82, bottom=0.14, left=0.06, right=0.98)
out = ROOT / "Outputs" / "Figures" / "fig_ptf_banda_2025.png"
fig.savefig(out, dpi=200, facecolor="white")
print("ok", out)
