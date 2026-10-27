"""
Serie de PTF empalmada: cifras oficiales del DANE hasta 2021 + replica propia desde 2022.

Por que: con la GEIH cruda y horas efectivas (P6850) la replica coincide con el Cuadro 1 del DANE
en 2022-2024 (error de la PTF menor a 0,1 pp) y por actividad en 8 de las 9 ramas; antes de 2021 el DANE usa
insumos laborales retroproyectados que no se pueden reconstruir con los microdatos publicos
(ver NOTAS_que_hace_el_DANE.md y 10_Diagnostico_por_actividad.py).

Salida:
  Outputs/tables/ptf_serie_empalmada_2005_2025.csv
  Outputs/Figures/fig_ptf_serie_empalmada.png
2025: el COU y el stock de capital propios llegan a 2024, asi que solo se replica el aporte del trabajo
(con los pesos de 2024) y se compara con el 2025pr del DANE; la PTF de 2025 queda como dato oficial.
"""
import importlib.util
from pathlib import Path

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
from matplotlib import font_manager

ROOT = Path(__file__).resolve().parents[1]
spec = importlib.util.spec_from_file_location("v2", Path(__file__).with_name("07_PTF_v2_horasEfectivas.py"))
v2 = importlib.util.module_from_spec(spec)
spec.loader.exec_module(v2)
v1 = v2.v1

for f in ("segoeui.ttf", "segoeuib.ttf"):
    p = Path(r"C:\Windows\Fonts") / f
    if p.exists():
        font_manager.fontManager.addfont(str(p))
plt.rcParams["font.family"] = ["Segoe UI", "DejaVu Sans"]
INK, INK2, MUTED, GRID = "#14202B", "#46535F", "#7A8791", "#E7EBEE"
DANE_C, REP_C = "#14202B", "#2A78D6"


def replica() -> pd.DataFrame:
    c = v2.cargar_celdas()
    c = c[c.anio != 2020]
    r, cel = v2.paneles(c, "efectivas", False)
    p = v2.panel_base(r)
    H = cel.groupby(["anio", "rama_ptf"]).horas_pond.sum().unstack()
    filas = []
    for a1 in (2022, 2023, 2024, 2025):
        a0 = a1 - 1
        if a1 <= 2024:
            w0, w1 = v1.pesos(p, a0), v1.pesos(p, a1)
            wL, wK, s = (w0.wL + w1.wL) / 2, (w0.wK + w1.wK) / 2, (w0.s + w1.s) / 2
        else:  # 2025: sin COU ni stock de capital propios; se usan los pesos de 2024
            w = v1.pesos(p, 2024)
            wL, wK, s = w.wL, w.wK, w.s
        dL = v1.indice_L(cel, a0, a1)
        dH = np.log(H.loc[a1]) - np.log(H.loc[a0])
        fila = {"anio": a1, "L_replica": (s * wL * dL).sum() * 100, "horas_replica": (s * wL * dH).sum() * 100}
        fila["composicion_replica"] = fila["L_replica"] - fila["horas_replica"]
        if a1 <= 2024:
            dV = np.log(w1.va_con) - np.log(w0.va_cor)
            dK = np.log(w1.stock) - np.log(w0.stock)
            fila["VA_replica"] = (s * dV).sum() * 100
            fila["K_replica"] = (s * wK * dK).sum() * 100
            fila["PTF_replica"] = fila["VA_replica"] - fila["L_replica"] - fila["K_replica"]
        filas.append(fila)
    return pd.DataFrame(filas)


def main() -> None:
    of = v1.oficial().reset_index()
    of.columns = ["anio", "VA", "L", "K", "contrib", "PTF"]
    rep = replica()
    t = of.merge(rep, on="anio", how="left")
    e25 = pd.read_csv(ROOT / "Outputs" / "tables" / "ptf_2025_replica.csv").iloc[0]  # 12_PTF_2025_completar.py
    m25 = t.anio == 2025
    t.loc[m25, "VA_replica"] = e25.VA_estimado
    t.loc[m25, "K_replica"] = e25.K_estimado
    t.loc[m25, "PTF_replica"] = e25.PTF_estimado
    t["PTF_replica_estimada"] = m25
    t["fuente_serie"] = np.where(t.anio <= 2021, "DANE", np.where(m25, "DANE; replica 2025 estimada (capital por persistencia)", "DANE; replica propia validada"))
    for v in ("L", "PTF"):
        t[f"dif_{v}"] = t[f"{v}_replica"] - t[v]
    out = ROOT / "Outputs" / "tables" / "ptf_serie_empalmada_2005_2025.csv"
    t.round(3).to_csv(out, index=False)
    pd.set_option("display.width", 250)
    print(t[t.anio >= 2019].round(2).to_string(index=False))

    # figura
    d = t[t.anio >= 2006].reset_index(drop=True)
    fig, ax = plt.subplots(figsize=(11.5, 5.6), dpi=200, facecolor="white")
    x = np.arange(len(d))
    cols = [DANE_C if a <= 2021 else "#9AA7B0" for a in d.anio]
    ax.bar(x, d.PTF, color=cols, width=0.62, zorder=3, label="PTF, DANE (Cuadro 1)")
    m = d.PTF_replica.notna()
    ok = m & ~d.PTF_replica_estimada
    ax.scatter(x[ok], d.PTF_replica[ok], s=110, color=REP_C, zorder=5, edgecolor="white", linewidth=1.5, label="PTF, réplica propia (2022–2024)")
    es = m & d.PTF_replica_estimada
    ax.errorbar(x[es], d.PTF_replica[es], yerr=0.24, fmt="o", ms=9, mfc="white", mec=REP_C, mew=2, ecolor=REP_C, elinewidth=1.6, capsize=4, zorder=5,
                label="PTF 2025, réplica estimada (±0,24 pp)")
    ax.axhline(0, color="#C9D1D6", lw=1.2, zorder=2)
    ax.axvspan(15.5, len(d) - 0.5, color="#F4F6F8", zorder=0)
    ax.text(15.6, ax.get_ylim()[1] * 0.95, "Desde 2022 la réplica\ncoincide con el DANE\n(error < 0,1 pp;\n2025 estimada)", fontsize=10, color=INK2, va="top", linespacing=1.3)
    ax.set_xticks(x)
    ax.set_xticklabels([str(int(a)) for a in d.anio], fontsize=10, color=MUTED)
    ax.tick_params(length=0, labelsize=10.5, labelcolor=MUTED)
    ax.grid(axis="y", color=GRID, zorder=0)
    ax.set_axisbelow(True)
    for sp in ("top", "right", "left"):
        ax.spines[sp].set_visible(False)
    ax.spines["bottom"].set_color("#C9D1D6")
    ax.set_ylabel("Aporte al crecimiento del valor agregado (pp)", fontsize=10.5, color=INK2)
    ax.legend(loc="lower right", frameon=False, fontsize=10.5, labelcolor=INK2)
    fig.text(0.04, 0.97, "PTF de Colombia: serie del DANE y replica propia", fontsize=16, fontweight="bold", color=INK, va="top")
    fig.text(0.04, 0.915, "Enfoque de valor agregado, total economía. 2020: sin GEIH comparable; la replica se usa solo desde 2022.", fontsize=10.5, color=INK2, va="top")
    fig.text(0.04, 0.015, "Fuente: DANE, Cuentas Nacionales (Cuadro 1 de anex-PTF-Productividad-2025); cálculos propios con COU, stock de capital y GEIH (horas efectivas, P6850). 2025: valor agregado del PIB trimestral y capital por persistencia de 2024.",
             fontsize=8.5, color=MUTED, va="bottom")
    fig.subplots_adjust(top=0.84, bottom=0.12, left=0.07, right=0.98)
    fig.savefig(ROOT / "Outputs" / "Figures" / "fig_ptf_serie_empalmada.png", dpi=200, facecolor="white")
    print("ok")


if __name__ == "__main__":
    main()
