"""Burbujas de cambio estructural por sector (version moderna).

Una figura por periodo: 2010-2025, 2010-2019 y 2019-2025.
  Eje x: cambio en la participacion del empleo del sector (puntos porcentuales).
  Eje y: producto real por trabajador en el anio inicial (millones de pesos de 2015, escala log).
  Tamano: participacion del sector en el empleo en el anio inicial.
  Color: azul = gana participacion, naranja = pierde participacion.

Insumo: Paper/tables/pib_geih_productividad_sector_series.csv (PIB real por sector de las cuentas
nacionales y ocupados de la GEIH, 2010-2025; no hay GEIH comparable para 2020).
"""

from pathlib import Path

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
import matplotlib.text
import numpy as np
import pandas as pd
from matplotlib import font_manager

ROOT = Path(__file__).resolve().parents[3]
TABLE = ROOT / "Paper" / "tables" / "pib_geih_productividad_sector_series.csv"
OUT_DIRS = [ROOT / "Outputs" / "Figures", ROOT / "Paper" / "figures"]
for d in OUT_DIRS:
    d.mkdir(parents=True, exist_ok=True)

for f in ("segoeui.ttf", "segoeuib.ttf", "segoeuil.ttf"):
    p = Path(r"C:\Windows\Fonts") / f
    if p.exists():
        font_manager.fontManager.addfont(str(p))
plt.rcParams["font.family"] = ["Segoe UI", "DejaVu Sans"]

INK, INK2, MUTED = "#14202B", "#46535F", "#7A8791"
GRID, AXIS = "#E7EBEE", "#C9D1D6"
GAIN, LOSS = "#2A78D6", "#EB6834"
YMIN, YMAX = 4.5, 480

SHORT = {
    "Comercio, transporte y alojamiento": "Comercio, transporte\ny alojamiento",
    "Adm. pública, educación y salud": "Administración pública,\neducación y salud",
    "Profesionales y administrativas": "Profesionales y\nadministrativas",
    "Información y comunicaciones": "Información y\ncomunicaciones",
    "Arte, recreación y otros servicios": "Arte, recreación y\notros servicios",
    "Hogares como empleadores": "Hogares como\nempleadores",
}


def period_data(y0, y1):
    s = pd.read_csv(TABLE)
    a = s[s.anio == y0].set_index("sector_name_short")
    b = s[s.anio == y1].set_index("sector_name_short")
    d = pd.DataFrame({
        "s0": 100 * a.ocupados / a.ocupados.sum(),
        "s1": 100 * b.ocupados / b.ocupados.sum(),
        "y0": a.pib_por_trabajador_millones_2015,
    })
    d["chg"] = d.s1 - d.s0
    return d


def reallocation_effect(d):
    """Cuanto cambia la productividad agregada solo por mover empleo entre sectores, con la productividad
    de cada sector fija en el anio inicial (en % de la productividad agregada inicial)."""
    base = (d.s0 / 100 * d.y0).sum()
    return 100 * (d.chg / 100 * d.y0).sum() / base


def es(v, dec=1, sign=False):
    t = f"{v:+.{dec}f}" if sign else f"{v:.{dec}f}"
    return t.replace(".", ",").replace("-", "\u2212")


def place_labels(fig, ax, items, obstacles=()):
    """Coloca cada etiqueta en la primera posicion libre (derecha, izquierda, arriba, abajo y diagonales)."""
    fig.canvas.draw()
    renderer = fig.canvas.get_renderer()
    placed = []
    for (x0f, y0f, x1f, y1f) in obstacles:  # zonas reservadas (p. ej. la leyenda), en fracciones del eje
        (px0, py0), (px1, py1) = ax.transAxes.transform((x0f, y0f)), ax.transAxes.transform((x1f, y1f))
        placed.append((px0, py0, px1, py1))
    circles = [(*ax.transData.transform((it["x"], it["y"])), it["r_px"]) for it in items]
    axbb = ax.get_window_extent()

    def penalty(bb, own):
        for (x0, y0, x1, y1) in placed:
            if not (bb.x1 < x0 or bb.x0 > x1 or bb.y1 < y0 or bb.y0 > y1):
                return 2.0
        pen = 0.0
        for i, (cx, cy, r) in enumerate(circles):
            nx, ny = min(max(cx, bb.x0), bb.x1), min(max(cy, bb.y0), bb.y1)
            if i != own and (nx - cx) ** 2 + (ny - cy) ** 2 < (r * 1.06) ** 2:
                pen += 1.0
        if bb.x0 < axbb.x0 or bb.x1 > axbb.x1 or bb.y0 < axbb.y0 or bb.y1 > axbb.y1:
            pen += 3.0
        return pen

    for i in sorted(range(len(items)), key=lambda k: -items[k]["r_px"]):
        it = items[i]
        r = it["r_px"]
        cands = []
        for gap in (7, 48, 95, 150, 215):  # cerca, y con linea guia si hay que alejar la etiqueta
            cands += [
                (r + gap, 0, "left", "center"), (-r - gap, 0, "right", "center"),
                (0, r + gap, "center", "bottom"), (0, -r - gap, "center", "top"),
                (r * .72 + gap, r * .72 + gap, "left", "bottom"), (-r * .72 - gap, r * .72 + gap, "right", "bottom"),
                (r * .72 + gap, -r * .72 - gap, "left", "top"), (-r * .72 - gap, -r * .72 - gap, "right", "top"),
            ]
        best = None
        for idx, (dx, dy, ha, va) in enumerate(cands):
            far = idx >= 8
            arrow = dict(arrowstyle="-", color="#A9B4BC", lw=0.9, shrinkA=2, shrinkB=r * 72 / fig.dpi) if far else None
            t = ax.annotate(it["text"], (it["x"], it["y"]), xytext=(dx / fig.dpi * 72, dy / fig.dpi * 72),
                            textcoords="offset points", ha=ha, va=va, fontsize=it["fs"], color=INK,
                            linespacing=1.1, zorder=6, arrowprops=arrow)
            t.update_positions(renderer)
            bb = matplotlib.text.Text.get_window_extent(t, renderer)  # solo el texto, sin la linea guia
            sc = penalty(bb, i) + (0.05 if far else 0)
            if best is None or sc < best[0]:
                if best is not None:
                    best[1].remove()
                best = (sc, t, bb)
            else:
                t.remove()
            if sc < 0.06:
                break
        placed.append((best[2].x0 - 3, best[2].y0 - 2, best[2].x1 + 3, best[2].y1 + 2))


def build(y0, y1, filename, xlim):
    XMIN, XMAX = xlim
    d = period_data(y0, y1)
    eff = reallocation_effect(d)
    fig = plt.figure(figsize=(12, 8.6), dpi=200, facecolor="white")
    ax = fig.add_axes([0.085, 0.145, 0.885, 0.64])
    ax.set_xlim(XMIN, XMAX)
    ax.set_yscale("log")
    ax.set_ylim(YMIN, YMAX)

    for t in (5, 10, 20, 50, 100, 200, 400):
        ax.axhline(t, color=GRID, lw=0.9, zorder=0)
    ax.axvline(0, color=AXIS, lw=1.2, zorder=1)
    ax.set_yticks([5, 10, 20, 50, 100, 200, 400])
    ax.set_yticklabels(["5", "10", "20", "50", "100", "200", "400"], fontsize=11, color=MUTED)
    ax.minorticks_off()
    ticks = list(range(int(np.ceil(XMIN)), int(np.floor(XMAX)) + 1))
    ax.set_xticks(ticks)
    ax.set_xticklabels([es(v, 0) if v else "0" for v in ticks], fontsize=11, color=MUTED)
    ax.tick_params(length=0, pad=8)
    for sp in ("top", "right", "left"):
        ax.spines[sp].set_visible(False)
    ax.spines["bottom"].set_color(AXIS)

    ax.text(XMIN + 0.05, YMAX / 1.18, "\u2190  Pierde participación en el empleo", fontsize=11.5, color=LOSS,
            fontweight="bold", va="top")
    ax.text(XMAX - 0.05, YMAX / 1.18, "Gana participación en el empleo  \u2192", fontsize=11.5, color=GAIN,
            fontweight="bold", va="top", ha="right")

    k_pt = 56 / np.sqrt(d.s0.max())  # radio (puntos) de la burbuja mayor
    items = []
    for name, r in d.sort_values("s0", ascending=False).iterrows():
        rad_pt = max(5.5, k_pt * np.sqrt(r.s0))
        color = GAIN if r.chg >= 0 else LOSS
        ax.scatter([r.chg], [r.y0], s=(2 * rad_pt) ** 2 * 0.5, color=color, alpha=0.80, edgecolor="white",
                   linewidth=1.8, zorder=3)
        items.append({"x": r.chg, "y": r.y0, "r_px": rad_pt * fig.dpi / 72 * 0.72,
                      "text": SHORT.get(name, name) + f"  {es(r.s0, 1)}%", "fs": 10.5 if r.s0 > 3 else 10})
    place_labels(fig, ax, items)

    # escala de tamanos: circulos concentricos en el cuadrante superior izquierdo (vacio)
    ax_h = ax.get_window_extent().height
    ax_w = ax.get_window_extent().width
    cx_f, base_f = 0.085, 0.60
    ax.text(cx_f - 0.035, base_f + 0.215, "Participación en el empleo, " + str(y0), transform=ax.transAxes,
            fontsize=10, color=MUTED, va="bottom", ha="left")
    for v in (30, 15, 5):
        r_pt = k_pt * np.sqrt(v) * 0.7071
        r_px = r_pt * fig.dpi / 72
        yc = base_f + r_px / ax_h
        ax.scatter([cx_f], [yc], s=(2 * k_pt * np.sqrt(v)) ** 2 * 0.5, transform=ax.transAxes, facecolor="none",
                   edgecolor="#A9B4BC", linewidth=1.1, zorder=3)
        ytop = yc + r_px / ax_h
        ax.plot([cx_f, cx_f + 0.09], [ytop, ytop], transform=ax.transAxes, color="#A9B4BC", lw=0.9, zorder=3)
        ax.text(cx_f + 0.095, ytop, f"{v}%", transform=ax.transAxes, fontsize=10, color=INK2, va="center", ha="left")

    fig.text(0.04, 0.945, f"Cambio estructural del empleo en Colombia, {y0}\u2013{y1}", fontsize=22,
             fontweight="bold", color=INK, va="top")
    fig.text(0.04, 0.895, "Participación de cada sector en el empleo y su producto por trabajador al inicio del periodo",
             fontsize=13, color=INK2, va="top")
    verb = "sumó" if eff >= 0 else "restó"
    fig.text(0.04, 0.855,
             f"Mover empleo entre sectores {verb} {es(abs(eff), 1)}% a la productividad agregada del país "
             f"(con la productividad de cada sector fija en {y0}).", fontsize=11.5, color=INK2, va="top")
    ax.set_xlabel("Cambio en la participación del sector en el empleo (puntos porcentuales)", fontsize=12,
                  color=INK2, labelpad=10)
    ax.set_ylabel(f"Producto por trabajador en {y0}\n(millones de pesos de 2015, escala logarítmica)", fontsize=12,
                  color=INK2, labelpad=10)
    fig.text(0.04, 0.03, "Fuente: cálculos propios con PIB real por sector (precios de 2015, DANE) y ocupados de la GEIH. "
             "No hay GEIH comparable para 2020. El rango del eje horizontal cambia entre figuras.", fontsize=9.5, color=MUTED, va="bottom")
    for dirp in OUT_DIRS:
        fig.savefig(dirp / filename, dpi=200, facecolor="white")
    plt.close(fig)
    return d, eff


if __name__ == "__main__":
    for y0, y1, xlim in ((2010, 2025, (-4.6, 3.2)), (2010, 2019, (-3.4, 3.0)), (2019, 2025, (-2.2, 2.4))):
        d, eff = build(y0, y1, f"fig_burbujas_cambio_estructural_{y0}_{y1}.png", xlim)
        print(y0, y1, "efecto reasignacion %", round(eff, 2))
