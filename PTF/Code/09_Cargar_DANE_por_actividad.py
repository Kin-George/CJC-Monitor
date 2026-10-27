"""Lee los Cuadros 4-23 del anexo oficial (enfoque de produccion, por actividad, 2005-2024) en una tabla larga.

Columnas: anio, actividad, prod (tasa %), comp, horas, L_total, K_tic, K_notic, K_total, energia, materiales, servicios, CI_total, contrib_factores, ptf.
Los aportes estan en puntos porcentuales del crecimiento de la PRODUCCION.
"""
from pathlib import Path

import pandas as pd

ROOT = Path(__file__).resolve().parents[1]
XLSX = ROOT / "Datos" / "Raw" / "anex-PTF-Productividad-2025.xlsx"
OUT = ROOT / "Outputs" / "tables" / "dane_ptf_por_actividad_2005_2024.csv"
COLS = ["prod", "comp", "horas", "L_total", "K_tic", "K_notic", "K_total", "energia", "materiales", "servicios", "CI_total", "contrib_factores", "ptf"]
ACT = {"Agricultura": "A-B_AgriculturaPesca", "Minería": "C_Mineria", "Industrias": "D_Manufactura", "Electricidad": "E_ElectricidadGasAgua",
       "Construcción": "F_Construccion", "Comercio": "G-H_ComercioHotelesRest", "Transporte": "I_TransporteComunicaciones",
       "Intermediación": "J-K_FinancieroInmobiliario", "Actividades de servicios": "L-Q_ServiciosSocialesComunales", "Total": "TOT_Economia"}


def main() -> None:
    x = pd.ExcelFile(XLSX)
    filas = []
    for n in range(4, 24):
        anio = 2005 + (n - 4)
        d = pd.read_excel(x, f"Cuadro {n}", header=None)
        d = d[d[1].notna() & d[2].apply(lambda v: isinstance(v, (int, float)) and pd.notna(v))]
        for _, r in d.iterrows():
            nombre = str(r[1])
            rama = next((v for k, v in ACT.items() if nombre.startswith(k)), None)
            if rama is None:
                continue
            filas.append({"anio": anio, "rama_ptf": rama, **{c: float(r[2 + i]) for i, c in enumerate(COLS)}})
    t = pd.DataFrame(filas)
    t.to_csv(OUT, index=False)
    print(t.groupby("anio").size().to_dict())
    print(t[t.anio == 2015].round(2).to_string(index=False))


if __name__ == "__main__":
    main()
