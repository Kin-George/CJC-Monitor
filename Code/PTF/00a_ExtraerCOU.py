"""
Extrae Valor Agregado, Remuneracion de asalariados y Excedente de explotacion
bruto (+ ingreso mixto) por rama de actividad economica, a partir de los
Cuadros de Utilizacion (COU) del DANE, precios corrientes y constantes, base 2015.

Se corre en Python (no R) porque los anexos son archivos de Excel de gran
tamano con una estructura de encabezados en varias filas que openpyxl/pandas
maneja mas comodo aqui; el resultado es un csv compacto rama x anio que
despues se lee desde R para el calculo de la PTF.

Uso: python 00a_ExtraerCOU.py
"""
import re
import pandas as pd

RAW = "Datos/Raw/PTF"
OUT = "Datos/Processed/PTF"

# Columnas (0-indexed) de la tabla "Cuadro utilizacion" que corresponden a
# cada una de las 61 divisiones/agrupaciones CIIU Rev.4, agrupadas en las 9
# ramas que usa el documento metodologico de PTF del DANE (Tabla 4):
#   A-B, C, D, E, F, G-H, I, J-K, L-Q
BRANCH_COLS = {
    "A-B_AgriculturaPesca": list(range(5, 10)),      # agricultura, ganaderia, silvicultura, pesca
    "C_Mineria": list(range(10, 15)),                 # carbon, petroleo y gas, metaliferos, otras minas, apoyo
    "D_Manufactura": list(range(15, 39)),              # alimentos ... otras industrias manufactureras
    "E_ElectricidadGasAgua": list(range(39, 44)),      # energia electrica, gas, agua, aguas residuales, reciclaje
    "F_Construccion": list(range(44, 47)),             # edificaciones, obras ingenieria civil, especializadas
    "G-H_ComercioHotelesRest": [47, 48, 54],           # comercio, mantenimiento vehiculos, alojamiento y comidas
    "I_TransporteComunicaciones": [49, 50, 51, 52, 53, 55],  # transporte terrestre/acuatico/aereo/almacen/correo, info y comunicaciones
    "J-K_FinancieroInmobiliario": [56, 57, 58, 59],    # financieras, inmobiliarias, profesionales, administrativas y apoyo
    "L-Q_ServiciosSocialesComunales": [60, 61, 62, 63, 64, 65],  # admon publica, educacion (mercado/no mercado), salud, artes+otros, hogares empleadores
}
TOTAL_COL = 66

CONCEPT_ROWS = {
    "produccion": "Total producción",
    "valor_agregado": "Valor agregado",
    "remuneracion_asalariados": "Remuneración de los asalariados",
    "ingreso_mixto": "Ingreso mixto",
    "excedente_explotacion_bruto": "Excedente de explotación bruto",
}


def find_year_sheets(xlsx_path):
    """A partir de la hoja Indice, mapea anio -> nombre de hoja 'Cuadro utilizacion' (a 61 divisiones)."""
    idx = pd.read_excel(xlsx_path, sheet_name="Índice", header=None)
    year_sheet = {}
    current_year = None
    for _, row in idx.iterrows():
        text = " ".join(str(v) for v in row.values if pd.notna(v))
        m_year = re.search(r"(20\d\d)\s*(provisional|preliminar)?\s*a\s+dos\s+d[ií]gitos", text, re.IGNORECASE)
        if m_year:
            current_year = int(m_year.group(1))
            continue
        if current_year is not None and "Cuadro utiliz" in text:
            m_cuadro = re.search(r"Cuadro\s+(\d+)", text)
            if m_cuadro:
                year_sheet[current_year] = f"Cuadro {m_cuadro.group(1)}"
                current_year = None  # ya emparejado oferta+utilizacion, esperar el siguiente bloque de anio
    return year_sheet


def extract_year(xlsx_path, sheet, year):
    df = pd.read_excel(xlsx_path, sheet_name=sheet, header=None)
    concept_col = df[1].astype(str)

    out_rows = []
    for concept_key, concept_label in CONCEPT_ROWS.items():
        matches = concept_col[concept_col.str.strip() == concept_label]
        if matches.empty:
            continue
        row_idx = matches.index[0]
        row = df.iloc[row_idx]
        for branch, cols in BRANCH_COLS.items():
            valid_cols = [c for c in cols if c < len(row)]
            val = pd.to_numeric(row[valid_cols], errors="coerce").sum(skipna=True)
            out_rows.append({"anio": year, "rama_ptf": branch, "concepto": concept_key, "valor": val})
        total_val = pd.to_numeric(row[TOTAL_COL], errors="coerce") if TOTAL_COL < len(row) else float("nan")
        out_rows.append({"anio": year, "rama_ptf": "TOT_Economia", "concepto": concept_key, "valor": total_val})
    return out_rows


def run(xlsx_name, out_name):
    xlsx_path = f"{RAW}/{xlsx_name}"
    year_sheet = find_year_sheets(xlsx_path)
    print(xlsx_name, "->", year_sheet)
    all_rows = []
    for year, sheet in sorted(year_sheet.items()):
        all_rows.extend(extract_year(xlsx_path, sheet, year))
    out = pd.DataFrame(all_rows)
    out_path = f"{OUT}/{out_name}"
    out.to_csv(out_path, index=False)
    print("Guardado:", out_path, out.shape)
    return out


if __name__ == "__main__":
    run("COU-PreciosCorrientes.xlsx", "COU_corrientes_rama.csv")
    run("COU-PreciosConstantes.xlsx", "COU_constantes_rama.csv")
