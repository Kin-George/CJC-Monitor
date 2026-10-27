"""
Extensión del pipeline de la herramienta 04: nivel educativo DETALLADO
(hasta 12 categorías, disponible solo 2021-2025 por el rediseño del
cuestionario GEIH — variable P3042 / NIVEL_MAS_ALTO). Replica exactamente
la metodología de ingreso ya validada en CreacionPanelLimpio2021-2025_EI.do
y baseConsolidada2021-2025.do (mismo deflactor IPC, misma fórmula de
ingreso por hora), leyendo directamente de las bases anuales crudas de la
GEIH, para no depender de BaseconCINE11.dta (que solo trae el nivel
agregado a 7 categorías).

Salida: agrega la llave "detallado" a outputs/educacion/educacion_data.json.
"""
import json
from pathlib import Path

import numpy as np
import pandas as pd

RAW_DIR = Path(r"C:\Users\jorge\Documents\Databases\GEIH")
JSON_PATH = Path(__file__).resolve().parents[2] / "outputs" / "educacion" / "educacion_data.json"

YEARS = [2021, 2022, 2023, 2024, 2025]
MONTHS_PER_WEEK = 52.0 / 12.0

# IPC diciembre de cada año (misma tabla que CreacionPanelLimpio2021-2025_EI.do)
IPC = {2021: 111.41, 2022: 126.03, 2023: 137.72, 2024: 144.88, 2025: 152.27}
IPC_BASE = IPC[2025]

# Códigos crudos P3042 / NIVEL_MAS_ALTO -> etiqueta agrupada (1 y 2 se
# fusionan en "Preescolar o ninguno", igual que en el Cuadro 6 del informe)
CODE_LABEL = {
    1: "Preescolar o ninguno", 2: "Preescolar o ninguno",
    3: "Básica primaria", 4: "Básica secundaria",
    5: "Media académica", 6: "Media técnica", 7: "Normalista",
    8: "Técnica profesional", 9: "Tecnológica",
    10: "Pregrado universitario", 11: "Especialización",
    12: "Maestría", 13: "Doctorado",
}
LABEL_ORDER = [
    "Preescolar o ninguno", "Básica primaria", "Básica secundaria",
    "Media académica", "Media técnica", "Normalista",
    "Técnica profesional", "Tecnológica", "Pregrado universitario",
    "Especialización", "Maestría", "Doctorado",
]

EDUC_VAR = {2021: "P3042", 2022: "NIVEL_MAS_ALTO", 2023: "P3042", 2024: "P3042", 2025: "P3042"}

# Sector económico homologado CIIU Rev. 4 (RAMA2D_R4, 2 dígitos), misma
# homologación de CreacionPanelLimpio2021-2025_EI.do (bloque "Rev. 4") y
# mismas etiquetas que Code/Educacion/04_analisis_educacion_2025.py.
SECTOR_LABELS = {
    1: "Agricultura, ganadería, silvicultura y pesca",
    2: "Minas y canteras",
    3: "Industrias manufactureras",
    4: "Electricidad, gas, agua y saneamiento",
    5: "Construcción",
    6: "Comercio y reparación",
    7: "Alojamiento y servicios de comida",
    8: "Transporte y almacenamiento",
    9: "Información y comunicaciones",
    10: "Actividades financieras y de seguros",
    11: "Inmobiliarias, profesionales y administrativas",
    12: "Administración pública y defensa",
    13: "Educación",
    14: "Salud y asistencia social",
    15: "Artes, recreación y otros servicios",
    16: "Hogares como empleadores",
    17: "Organizaciones extraterritoriales",
}
# Tamaño de muestra (sin ponderar) mínimo para reportar un punto nivel x
# sector: evita puntos inestables en combinaciones raras (p.ej. doctorado en
# un sector con un puñado de observaciones).
MIN_N_SECTOR = 30


def sector_hom(rama2d):
    r = int(rama2d)
    if 1 <= r <= 3:
        return 1
    if 5 <= r <= 9:
        return 2
    if 10 <= r <= 33:
        return 3
    if 35 <= r <= 39:
        return 4
    if 41 <= r <= 43:
        return 5
    if 45 <= r <= 47:
        return 6
    if r in (55, 56):
        return 7
    if 49 <= r <= 53:
        return 8
    if 58 <= r <= 63:
        return 9
    if 64 <= r <= 66:
        return 10
    if r == 68 or 69 <= r <= 75 or 77 <= r <= 82:
        return 11
    if r == 84:
        return 12
    if r == 85:
        return 13
    if 86 <= r <= 88:
        return 14
    if 90 <= r <= 96:
        return 15
    if 97 <= r <= 98:
        return 16
    if r == 99:
        return 17
    return np.nan


def weighted_mean(values, weights):
    return float(np.average(values, weights=weights))


def weighted_quantile(values, weights, q):
    sorter = np.argsort(values)
    v = values[sorter]
    w = weights[sorter]
    cw = np.cumsum(w) - 0.5 * w
    cw /= w.sum()
    return float(np.interp(q, cw, v))


def load_year(year: int) -> pd.DataFrame:
    educ_var = EDUC_VAR[year]
    cols = [educ_var, "INGLABO", "P6800", "FEX_C18", "OCI", "RAMA2D_R4"]
    path = RAW_DIR / f"GEIH_{year}_total.dta"
    df = pd.read_stata(path, columns=cols, convert_categoricals=False)
    df = df.rename(columns={educ_var: "educ_cod"})
    df["RAMA2D_R4"] = pd.to_numeric(df["RAMA2D_R4"], errors="coerce")
    df = df.dropna(subset=["educ_cod", "INGLABO", "P6800", "FEX_C18", "OCI"])
    df = df[
        (df["OCI"] == 1)
        & (df["INGLABO"] > 0)
        & (df["P6800"] > 0)
        & (df["P6800"] <= 168)
        & (df["educ_cod"].isin(CODE_LABEL.keys()))
    ].copy()

    factor_precios = IPC_BASE / IPC[year]
    ingreso_laboral_anual = df["INGLABO"] * 12
    ingreso_laboral_hora = ingreso_laboral_anual / (df["P6800"] * 52)
    df["ingreso_hora_real"] = ingreso_laboral_hora * factor_precios
    df["rem_mensual"] = df["ingreso_hora_real"] * df["P6800"] * MONTHS_PER_WEEK
    df["label"] = df["educ_cod"].map(CODE_LABEL)
    df["anio"] = year
    df["fex"] = df["FEX_C18"]
    df["sector_hom"] = df["RAMA2D_R4"].apply(lambda r: sector_hom(r) if pd.notna(r) else np.nan)
    return df[["anio", "label", "ingreso_hora_real", "rem_mensual", "P6800", "fex", "sector_hom"]]


def main():
    print("Cargando bases anuales GEIH 2021-2025 (P3042 / NIVEL_MAS_ALTO)...")
    frames = [load_year(y) for y in YEARS]
    df = pd.concat(frames, ignore_index=True)
    print(f"Filas válidas: {len(df):,}")

    # ---- serie por año x nivel detallado ----
    # GEIH_{year}_total.dta apila las 12 muestras mensuales del año, así que
    # FEX_C18 sumado sin ajustar da "persona-meses", no personas. Se divide
    # por 12 para obtener el promedio anual de ocupados (mismo criterio que
    # usa DANE para el nivel agregado en BaseconCINE11.dta).
    MONTHS_PER_YEAR = 12.0
    serie_rows = []
    for anio, g in df.groupby("anio"):
        ocupados_anio = g["fex"].sum() / MONTHS_PER_YEAR
        for label in LABEL_ORDER:
            gg = g[g["label"] == label]
            if gg.empty:
                continue
            w = gg["fex"].to_numpy()
            serie_rows.append({
                "anio": int(anio), "label": label,
                "ocupados": float(w.sum() / MONTHS_PER_YEAR),
                "participacion": float(w.sum() / MONTHS_PER_YEAR / ocupados_anio),
                "remMensual": weighted_mean(gg["rem_mensual"].to_numpy(), w),
                "remHora": weighted_mean(gg["ingreso_hora_real"].to_numpy(), w),
            })
    serie = pd.DataFrame(serie_rows)

    # ---- detalle 2025 para la calculadora (percentiles dentro de esta muestra) ----
    g25 = df[df["anio"] == 2025]
    v_mensual = g25["rem_mensual"].to_numpy()
    v_hora = g25["ingreso_hora_real"].to_numpy()
    w25 = g25["fex"].to_numpy()

    # ---- densidad de la remuneración mensual dentro de cada nivel ----
    # Bins compartidos (en log10 de pesos) para que la posición de cada barra
    # sea comparable entre tarjetas: recorren del p1 al p99 del pool 2025.
    N_BINS = 16
    log_mensual = np.log10(v_mensual)
    lo, hi = weighted_quantile(log_mensual, w25, 0.01), weighted_quantile(log_mensual, w25, 0.99)
    bin_edges_log = np.linspace(lo, hi, N_BINS + 1)
    bin_edges_pesos = (10 ** bin_edges_log).tolist()

    def weighted_density(vals_log, weights, edges):
        clipped = np.clip(vals_log, edges[0], edges[-1] - 1e-9)
        idx = np.searchsorted(edges, clipped, side="right") - 1
        idx = np.clip(idx, 0, len(edges) - 2)
        counts = np.zeros(len(edges) - 1)
        np.add.at(counts, idx, weights)
        total = counts.sum()
        return (counts / total).tolist() if total > 0 else counts.tolist()

    def percentile_of(vals, weights, value):
        sorter = np.argsort(vals)
        v_sorted = vals[sorter]
        w_sorted = weights[sorter]
        cw = np.cumsum(w_sorted)
        cw /= cw[-1]
        idx = min(np.searchsorted(v_sorted, value), len(cw) - 1)
        return float(cw[idx]) * 100

    baseline = serie[(serie.anio == 2025) & (serie.label == "Preescolar o ninguno")].iloc[0]

    niveles = []
    for label in LABEL_ORDER:
        row25 = serie[(serie.anio == 2025) & (serie.label == label)]
        if row25.empty:
            continue
        row25 = row25.iloc[0]
        row21 = serie[(serie.anio == 2021) & (serie.label == label)]
        crec_mensual = None
        crec_hora = None
        if not row21.empty:
            r21 = row21.iloc[0]
            crec_mensual = (row25.remMensual / r21.remMensual) ** (1 / 4) - 1
            crec_hora = (row25.remHora / r21.remHora) ** (1 / 4) - 1
        gg = g25[g25["label"] == label]
        w = gg["fex"].to_numpy()
        v_mensual_lvl = gg["rem_mensual"].to_numpy()

        por_sector = []
        gg_sector = gg.dropna(subset=["sector_hom"])
        for sector_cod, gs in gg_sector.groupby("sector_hom"):
            if len(gs) < MIN_N_SECTOR:
                continue
            ws = gs["fex"].to_numpy()
            vs = gs["rem_mensual"].to_numpy()
            por_sector.append({
                "sector": SECTOR_LABELS[int(sector_cod)],
                "remMensual": weighted_mean(vs, ws),
                "ocupados": float(ws.sum() / MONTHS_PER_YEAR),
                "p10": weighted_quantile(vs, ws, 0.10),
                "p25": weighted_quantile(vs, ws, 0.25),
                "mediana": weighted_quantile(vs, ws, 0.50),
                "p75": weighted_quantile(vs, ws, 0.75),
                "p90": weighted_quantile(vs, ws, 0.90),
            })
        por_sector.sort(key=lambda x: x["remMensual"])

        niveles.append({
            "label": label,
            "ocupados2025": row25.ocupados,
            "participacion2025": row25.participacion,
            "remMensual2025": row25.remMensual,
            "remMensualMediana2025": weighted_quantile(v_mensual_lvl, w, 0.5),
            "remMensualP25_2025": weighted_quantile(v_mensual_lvl, w, 0.25),
            "remMensualP75_2025": weighted_quantile(v_mensual_lvl, w, 0.75),
            "remHora2025": row25.remHora,
            "remHoraMediana2025": weighted_quantile(gg["ingreso_hora_real"].to_numpy(), w, 0.5),
            "remMensual2021": None if row21.empty else float(row21.iloc[0].remMensual),
            "remHora2021": None if row21.empty else float(row21.iloc[0].remHora),
            "crecAnualMensual": crec_mensual,
            "crecAnualHora": crec_hora,
            "razonMensual": row25.remMensual / baseline.remMensual,
            "razonHora": row25.remHora / baseline.remHora,
            "percentilMensual": percentile_of(v_mensual, w25, row25.remMensual),
            "percentilHora": percentile_of(v_hora, w25, row25.remHora),
            "densidadMensual": weighted_density(np.log10(v_mensual_lvl), w, bin_edges_log),
            "porSector": por_sector,
        })

    payload = {
        "niveles": niveles,
        "densidadBinsMensual": bin_edges_pesos,
        "serie": [
            {"anio": int(r.anio), "label": r.label, "remMensual": r.remMensual,
             "remHora": r.remHora, "ocupados": r.ocupados, "participacion": r.participacion}
            for r in serie.itertuples(index=False)
        ],
    }

    with open(JSON_PATH, "r", encoding="utf-8") as f:
        existing = json.load(f)
    existing["detallado"] = payload
    with open(JSON_PATH, "w", encoding="utf-8") as f:
        json.dump(existing, f, ensure_ascii=False, separators=(",", ":"))

    print(f"JSON actualizado en {JSON_PATH} ({JSON_PATH.stat().st_size/1024:.1f} KB)")
    for n in niveles:
        print(n["label"], "->", round(n["remMensual2025"]), "crec:", n["crecAnualMensual"])


if __name__ == "__main__":
    main()
