"""
Agrega la GEIH (microdatos por persona, 2008-2025, ~1.9GB) a nivel de
celdas anio x rama PTF x categoria (sexo x grupo de edad x nivel educativo),
para construir el indice de Servicios Laborales (L) de la metodologia KLEMS
del DANE, y el ratio horas-ocupados / horas-asalariados que se usa para
ajustar la remuneracion (LAB_j).

Se corre en Python, leyendo el .dta por chunks (pandas.read_stata con
chunksize), porque el archivo es de 1.9GB y la maquina tiene poca memoria
libre; leerlo completo con haven en R es riesgoso. El resultado son dos csv
compactos que despues se leen desde R.

Uso: python 00c_AgregarGEIH.py
"""
import pandas as pd

GEIH_PATH = "Datos/Processed/Paper-GEIH_base_modelo_personas_2008_2025.dta"
OUT_CELDAS = "Datos/Processed/PTF/GEIH_celdas_categoria.csv"
OUT_RATIO = "Datos/Processed/PTF/GEIH_horas_totales_rama.csv"

COLUMNS = [
    "anio", "edad", "sector", "posicion_ocupacional_label",
    "educacion", "sexo", "fex", "horas", "ingreso_laboral_hora",
]

SECTOR_A_RAMA_PTF = {
    "Agricultura, ganadería, silvicultura y pesca": "A-B_AgriculturaPesca",
    "Minas y canteras": "C_Mineria",
    "Industrias manufactureras": "D_Manufactura",
    "Electricidad, gas, agua y saneamiento": "E_ElectricidadGasAgua",
    "Construcción": "F_Construccion",
    "Comercio y reparación": "G-H_ComercioHotelesRest",
    "Alojamiento y servicios de comida": "G-H_ComercioHotelesRest",
    "Transporte y almacenamiento": "I_TransporteComunicaciones",
    "Información y comunicaciones": "I_TransporteComunicaciones",
    "Actividades financieras y de seguros": "J-K_FinancieroInmobiliario",
    "Inmobiliarias, profesionales y administrativas": "J-K_FinancieroInmobiliario",
    "Administración pública y defensa": "L-Q_ServiciosSocialesComunales",
    "Educación": "L-Q_ServiciosSocialesComunales",
    "Salud y asistencia social": "L-Q_ServiciosSocialesComunales",
    "Artes, recreación y otros servicios": "L-Q_ServiciosSocialesComunales",
    "Hogares como empleadores": "L-Q_ServiciosSocialesComunales",
    "Organizaciones extraterritoriales": "L-Q_ServiciosSocialesComunales",
}

EDUC_A_GRUPO = {
    "Ninguno": "1_Bajo",
    "Preescolar": "1_Bajo",
    "Básica primaria": "1_Bajo",
    "Básica secundaria": "2_Medio",
    "Media": "2_Medio",
    "Superior o universitaria": "3_Alto",
}

ASALARIADO_LABELS = {
    "Obrero o empleado de empresa particular",
    "Obrero o empleado del gobierno",
    "Empleado doméstico",
    "Jornalero o peón",
}


def grupo_edad(edad):
    if pd.isna(edad):
        return None
    if 15 <= edad <= 29:
        return "15-29"
    if 30 <= edad <= 49:
        return "30-49"
    if edad >= 50:
        return "50+"
    return None


def process_chunk(chunk, celdas_acc, ratio_acc):
    chunk = chunk.copy()
    chunk["rama_ptf"] = chunk["sector"].map(SECTOR_A_RAMA_PTF)
    chunk["grupo_educ"] = chunk["educacion"].map(EDUC_A_GRUPO)
    chunk["grupo_edad"] = chunk["edad"].apply(grupo_edad)
    chunk["asalariado"] = chunk["posicion_ocupacional_label"].isin(ASALARIADO_LABELS)

    chunk["horas_pond"] = chunk["horas"] * chunk["fex"]
    chunk["ingreso_pond"] = chunk["ingreso_laboral_hora"] * chunk["horas_pond"]

    # 1) celdas para el indice L: anio x rama x sexo x grupo_edad x grupo_educ
    valid = chunk.dropna(subset=["rama_ptf", "grupo_educ", "grupo_edad", "sexo"])
    g1 = (
        valid.groupby(["anio", "rama_ptf", "sexo", "grupo_edad", "grupo_educ"])
        .agg(horas_pond=("horas_pond", "sum"), ingreso_pond=("ingreso_pond", "sum"), n=("horas_pond", "size"))
        .reset_index()
    )
    celdas_acc.append(g1)

    # 2) horas totales ocupados vs asalariados, por anio x rama (para LAB_j)
    valid2 = chunk.dropna(subset=["rama_ptf"])
    g2 = (
        valid2.groupby(["anio", "rama_ptf", "asalariado"])
        .agg(horas_pond=("horas_pond", "sum"))
        .reset_index()
    )
    ratio_acc.append(g2)


def run(chunksize=500_000):
    celdas_acc, ratio_acc = [], []
    reader = pd.read_stata(GEIH_PATH, columns=COLUMNS, chunksize=chunksize)
    n_chunks = 0
    for chunk in reader:
        process_chunk(chunk, celdas_acc, ratio_acc)
        n_chunks += 1
        print(f"chunk {n_chunks} procesado ({len(chunk)} filas)")

    celdas = pd.concat(celdas_acc, ignore_index=True)
    celdas = (
        celdas.groupby(["anio", "rama_ptf", "sexo", "grupo_edad", "grupo_educ"])
        .agg(horas_pond=("horas_pond", "sum"), ingreso_pond=("ingreso_pond", "sum"), n=("n", "sum"))
        .reset_index()
    )
    celdas.to_csv(OUT_CELDAS, index=False)
    print("Guardado:", OUT_CELDAS, celdas.shape)

    ratio = pd.concat(ratio_acc, ignore_index=True)
    ratio = (
        ratio.groupby(["anio", "rama_ptf", "asalariado"])
        .agg(horas_pond=("horas_pond", "sum"))
        .reset_index()
    )
    ratio.to_csv(OUT_RATIO, index=False)
    print("Guardado:", OUT_RATIO, ratio.shape)


if __name__ == "__main__":
    run()
