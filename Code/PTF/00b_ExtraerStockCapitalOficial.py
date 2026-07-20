"""
Extrae el stock de capital productivo por rama de actividad (1990-2024p) que
el DANE ya publica en su anexo oficial de PTF. Se usa como K de la version v0
de la replicacion (en vez de reconstruirlo desde FBKF por tipo de activo,
que se deja para una version posterior).
"""
import re

import pandas as pd

RAW = "Datos/Raw/PTF/anex-PTF-StockCapital-acervos-2024.xlsx"
OUT = "Datos/Processed/PTF/StockCapitalProductivo_rama.csv"

BRANCH_LABELS = {
    "Agricultura, ganadería, caza, silvicultura y pesca": "A-B_AgriculturaPesca",
    "Minería y extracción": "C_Mineria",
    "Industrias manufactureras": "D_Manufactura",
    "Electricidad, gas y agua": "E_ElectricidadGasAgua",
    "Construcción": "F_Construccion",
    "Comercio, hoteles y restaurantes": "G-H_ComercioHotelesRest",
    "Transporte, almacenamiento y comunicaciones": "I_TransporteComunicaciones",
    "Intermediación financiera, actividades inmobiliarias, empresariales y de alquiler": "J-K_FinancieroInmobiliario",
    "Actividades de servicios sociales, comunales y personales": "L-Q_ServiciosSocialesComunales",
    "Total nacional": "TOT_Economia",
}


def run():
    df = pd.read_excel(RAW, sheet_name="Cuadro 3", header=None)
    header_row = df.iloc[10]
    years = {}
    for j, v in header_row.items():
        if j < 1 or pd.isna(v):
            continue
        if isinstance(v, (int, float)):
            years[j] = int(v)
        else:
            m = re.match(r"(\d{4})", str(v).strip())
            if m:
                years[j] = int(m.group(1))

    out_rows = []
    for _, row in df.iterrows():
        label = str(row[0]).strip()
        if label not in BRANCH_LABELS:
            continue
        branch = BRANCH_LABELS[label]
        for j, year in years.items():
            out_rows.append({"anio": year, "rama_ptf": branch, "stock_capital_productivo": row[j]})

    out = pd.DataFrame(out_rows).sort_values(["rama_ptf", "anio"])
    out.to_csv(OUT, index=False)
    print("Guardado:", OUT, out.shape)
    print(out["rama_ptf"].unique())


if __name__ == "__main__":
    run()
