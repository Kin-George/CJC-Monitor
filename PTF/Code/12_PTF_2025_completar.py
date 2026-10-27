"""
Completa la replica de la PTF para 2025 (valor agregado + capital + trabajo).

Faltantes de 2025 y como se resuelven (todo con datos publicos):
  - Valor agregado por rama a precios constantes: PIB trimestral (anex-ProduccionConstantes-Itrim2026.xlsx, Cuadro 2,
    25 agrupaciones CIIU Rev. 4). El anual es la suma de los cuatro trimestres. Se valida contra el valor agregado del
    COU en 2022-2024.
  - Pesos (participacion en el VA, participacion del trabajo): los de 2024, con la participacion del VA actualizada por el
    crecimiento real de cada rama (no hay VA ni remuneracion corrientes de 2025 por rama).
  - Stock de capital productivo de 2025: no existe (el anexo del DANE llega a 2024p). Se supone que cada rama crece en 2025
    lo mismo que en 2024 ("persistencia"). Con esa regla, el error del aporte del capital en 2015-2024 fue de 0,24 pp en promedio
    (mejor que promedios moviles o tendencia); ese error se usa como banda de la PTF estimada.
  - Trabajo: igual que 2022-2024, con horas efectivas (P6850) de la GEIH 2025.

Salida: Outputs/tables/ptf_2025_replica.csv
"""
import importlib.util
from pathlib import Path

import numpy as np
import pandas as pd

ROOT = Path(__file__).resolve().parents[1]
REPO = Path(__file__).resolve().parents[2]  # raiz del repositorio (entradas compartidas del DANE)
PIB = REPO / "Datos" / "Raw" / "PIB_DANE" / "anex-ProduccionConstantes-Itrim2026.xlsx"
spec = importlib.util.spec_from_file_location("v2", Path(__file__).with_name("07_PTF_v2_horasEfectivas.py"))
v2 = importlib.util.module_from_spec(spec)
spec.loader.exec_module(v2)
v1 = v2.v1

# filas del Cuadro 2 (25 agrupaciones) -> 9 ramas PTF (misma agregacion que el COU de 00a_ExtraerCOU.py)
FILAS = {
    "A-B_AgriculturaPesca": [15],
    "C_Mineria": [17],
    "D_Manufactura": [18],
    "E_ElectricidadGasAgua": [25],
    "F_Construccion": [28],
    "G-H_ComercioHotelesRest": [33, 35],
    "I_TransporteComunicaciones": [34, 37],
    "J-K_FinancieroInmobiliario": [39, 41, 43],
    "L-Q_ServiciosSocialesComunales": [45, 46, 47, 49, 50],
}
ERROR_K = 0.24  # pp, error medio de la regla de persistencia del capital en 2015-2024


def va_pib_anual() -> pd.DataFrame:
    d = pd.read_excel(PIB, "Cuadro 2", header=None)
    hdr = d.iloc[11]
    anios = {}
    for j, v in enumerate(hdr):
        if pd.notna(v) and j >= 4:
            anios[int(str(v)[:4])] = j
    out = {}
    for rama, filas in FILAS.items():
        out[rama] = {a: sum(float(d.iat[f, j + q]) for f in filas for q in range(4)) for a, j in anios.items() if a <= 2025}
    return pd.DataFrame(out)


def main() -> None:
    va = va_pib_anual()
    g = np.log(va).diff()

    # validacion del VA anual del PIB trimestral frente al COU (dlnV de v1 usa va_con / va_cor_{t-1})
    c = v2.cargar_celdas()
    c = c[c.anio != 2020]
    r, cel = v2.paneles(c, "efectivas", False)
    p = v2.panel_base(r)
    print("Crecimiento del VA 2022-2024: PIB trimestral vs COU (pp, ponderado por participacion en el VA)")
    for a in (2022, 2023, 2024):
        w0, w1 = v1.pesos(p, a - 1), v1.pesos(p, a)
        s = (w0.s + w1.s) / 2
        cou = (s * (np.log(w1.va_con) - np.log(w0.va_cor))).sum() * 100
        pib = (s * g.loc[a]).sum() * 100
        print(f"  {a}: COU {cou:.2f}  PIB trimestral {pib:.2f}")

    # 2025
    w24 = v1.pesos(p, 2024)
    s24 = w24.s
    s25 = s24 * np.exp(g.loc[2025]) / (s24 * np.exp(g.loc[2025])).sum()
    s_bar = (s24 + s25) / 2
    wL, wK = w24.wL, w24.wK
    VA25 = (s_bar * g.loc[2025]).sum() * 100
    H = cel.groupby(["anio", "rama_ptf"]).horas_pond.sum().unstack()
    dL = v1.indice_L(cel, 2024, 2025)
    dH = np.log(H.loc[2025]) - np.log(H.loc[2024])
    L25 = (s_bar * wL * dL).sum() * 100
    horas25 = (s_bar * wL * dH).sum() * 100
    stock = pd.read_csv(ROOT / "Datos" / "Processed" / "StockCapitalProductivo_rama.csv")
    st = stock[stock.rama_ptf != "TOT_Economia"].pivot(index="anio", columns="rama_ptf", values="stock_capital_productivo").sort_index()
    dK = np.log(st.loc[2024]) - np.log(st.loc[2023])  # persistencia
    K25 = (s_bar * wK * dK).sum() * 100
    fila = {"anio": 2025, "VA_estimado": VA25, "L_replica": L25, "horas_replica": horas25, "composicion_replica": L25 - horas25,
            "K_estimado": K25, "PTF_estimado": VA25 - L25 - K25, "banda_PTF_pp": ERROR_K}
    of = v1.oficial().loc[2025]
    fila.update({"VA_DANE": of.VA, "L_DANE": of.L, "K_DANE": of.K, "PTF_DANE": of.PTF})
    out = pd.DataFrame([fila])
    out.round(3).to_csv(ROOT / "Outputs" / "tables" / "ptf_2025_replica.csv", index=False)
    pd.set_option("display.width", 250)
    print(out.round(2).T.to_string(header=False))


if __name__ == "__main__":
    main()
