"""
Pipeline de datos para la herramienta interactiva 04 (Remuneración laboral por
logro educativo). Lee la base GEIH homologada con nivel educativo CINE-11
(Datos/Processed/GEIH/BaseconCINE11.dta), reproduce las cifras del Informe 0X
("Remuneración Laboral por Logro Educativo en Colombia, 2010-2025") y agrega
cortes adicionales (series de tiempo, sexo, formalidad, sector, departamento,
percentiles) pensados para la herramienta interactiva, no solo para el PDF.

Salida: CSV en outputs/tables/ y un payload compacto en outputs/educacion_data.json
listo para incrustar en la página interactiva.
"""
from pathlib import Path
import json

import numpy as np
import pandas as pd

ROOT = Path(__file__).resolve().parents[2]
DATA_PATH = ROOT / "Datos" / "Processed" / "GEIH" / "BaseconCINE11.dta"
TABLE_DIR = ROOT / "outputs" / "educacion" / "tables"
JSON_PATH = ROOT / "outputs" / "educacion" / "educacion_data.json"

YEARS = list(range(2010, 2020)) + list(range(2021, 2026))
START_YEAR, END_YEAR = 2010, 2025
MONTHS_PER_WEEK = 52.0 / 12.0

CINE11_ORDER = [1, 2, 3, 4, 5, 6, 7]
CINE11_LABELS = {
    1: "Ninguno",
    2: "Básica primaria",
    3: "Básica secundaria",
    4: "Educación media",
    5: "Técnico o tecnológico",
    6: "Pregrado universitario",
    7: "Posgrado",
}

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

DEPARTMENTS_33 = {
    5: "Antioquia", 8: "Atlántico", 11: "Bogotá D.C.", 13: "Bolívar",
    15: "Boyacá", 17: "Caldas", 18: "Caquetá", 19: "Cauca", 20: "Cesar",
    23: "Córdoba", 25: "Cundinamarca", 27: "Chocó", 41: "Huila",
    44: "La Guajira", 47: "Magdalena", 50: "Meta", 52: "Nariño",
    54: "Norte de Santander", 63: "Quindío", 66: "Risaralda",
    68: "Santander", 70: "Sucre", 73: "Tolima", 76: "Valle del Cauca",
    81: "Arauca", 85: "Casanare", 86: "Putumayo", 88: "San Andrés y Providencia",
    91: "Amazonas", 94: "Guainía", 95: "Guaviare", 97: "Vaupés", 99: "Vichada",
}


def weighted_mean(values: np.ndarray, weights: np.ndarray) -> float:
    return float(np.average(values, weights=weights))


def weighted_quantile(values: np.ndarray, weights: np.ndarray, q: float) -> float:
    sorter = np.argsort(values)
    v = values[sorter]
    w = weights[sorter]
    cw = np.cumsum(w) - 0.5 * w
    cw /= w.sum()
    return float(np.interp(q, cw, v))


def load_data() -> pd.DataFrame:
    cols = [
        "anio", "fex", "horas", "ingreso_hora_real", "cine11_hom_cod",
        "sexo_hom_cod", "formalidad_cod", "sector_hom_cod", "depto", "edad",
    ]
    df = pd.read_stata(DATA_PATH, columns=cols, convert_categoricals=False)
    df = df[df["anio"].isin(YEARS)]
    df = df.dropna(subset=["fex", "horas", "ingreso_hora_real", "cine11_hom_cod"])
    df = df[
        (df["fex"] > 0)
        & (df["horas"] > 0)
        & (df["horas"] <= 112)
        & (df["ingreso_hora_real"] > 0)
        & (df["cine11_hom_cod"].isin(CINE11_ORDER))
    ]
    df = df.copy()
    df["rem_mensual"] = df["ingreso_hora_real"] * df["horas"] * MONTHS_PER_WEEK
    df["cine11"] = df["cine11_hom_cod"].map(CINE11_LABELS)
    return df


def national_series(df: pd.DataFrame) -> pd.DataFrame:
    rows = []
    for anio, g in df.groupby("anio"):
        ocupados_anio = g["fex"].sum()
        for cod in CINE11_ORDER:
            gg = g[g["cine11_hom_cod"] == cod]
            if gg.empty:
                continue
            w = gg["fex"].to_numpy()
            rows.append({
                "anio": int(anio),
                "cine11_cod": cod,
                "cine11": CINE11_LABELS[cod],
                "ocupados": float(w.sum()),
                "participacion": float(w.sum() / ocupados_anio),
                "rem_mensual_prom": weighted_mean(gg["rem_mensual"].to_numpy(), w),
                "rem_mensual_mediana": weighted_quantile(gg["rem_mensual"].to_numpy(), w, 0.5),
                "rem_hora_prom": weighted_mean(gg["ingreso_hora_real"].to_numpy(), w),
                "rem_hora_mediana": weighted_quantile(gg["ingreso_hora_real"].to_numpy(), w, 0.5),
            })
    return pd.DataFrame(rows).sort_values(["cine11_cod", "anio"])


def national_headline(df: pd.DataFrame) -> dict:
    out = {}
    for year in (START_YEAR, END_YEAR):
        g = df[df["anio"] == year]
        w = g["fex"].to_numpy()
        out[year] = {
            "ocupados": float(w.sum()),
            "rem_mensual_prom": weighted_mean(g["rem_mensual"].to_numpy(), w),
            "rem_hora_prom": weighted_mean(g["ingreso_hora_real"].to_numpy(), w),
        }
    out["crec_anual_mensual"] = (out[END_YEAR]["rem_mensual_prom"] / out[START_YEAR]["rem_mensual_prom"]) ** (1 / (END_YEAR - START_YEAR)) - 1
    out["crec_anual_hora"] = (out[END_YEAR]["rem_hora_prom"] / out[START_YEAR]["rem_hora_prom"]) ** (1 / (END_YEAR - START_YEAR)) - 1
    return out


def decomposition(series: pd.DataFrame, metric: str) -> dict:
    """Replica la ecuación (2) del informe: productividad vs. composición."""
    piv = series.pivot(index="cine11_cod", columns="anio", values=[metric, "participacion"])
    total = 0.0
    prod = 0.0
    comp = 0.0
    for cod in CINE11_ORDER:
        r0 = piv[(metric, START_YEAR)].get(cod, np.nan)
        r1 = piv[(metric, END_YEAR)].get(cod, np.nan)
        s0 = piv[("participacion", START_YEAR)].get(cod, np.nan)
        s1 = piv[("participacion", END_YEAR)].get(cod, np.nan)
        if pd.isna(r0) or pd.isna(r1) or pd.isna(s0) or pd.isna(s1):
            continue
        s_bar = 0.5 * (s0 + s1)
        r_bar = 0.5 * (r0 + r1)
        prod += s_bar * (r1 - r0)
        comp += r_bar * (s1 - s0)
    total = prod + comp
    return {"productividad": prod, "composicion": comp, "total": total}


def crosstab(df: pd.DataFrame, year: int, dim_col: str, dim_labels: dict) -> pd.DataFrame:
    g = df[df["anio"] == year]
    rows = []
    for dim_val, gd in g.groupby(dim_col):
        if dim_val not in dim_labels:
            continue
        for cod in CINE11_ORDER:
            gg = gd[gd["cine11_hom_cod"] == cod]
            if gg.empty:
                continue
            w = gg["fex"].to_numpy()
            rows.append({
                "dim_val": dim_labels[dim_val],
                "cine11_cod": cod,
                "cine11": CINE11_LABELS[cod],
                "ocupados": float(w.sum()),
                "rem_mensual_prom": weighted_mean(gg["rem_mensual"].to_numpy(), w),
                "rem_hora_prom": weighted_mean(gg["ingreso_hora_real"].to_numpy(), w),
            })
    return pd.DataFrame(rows)


def ranking(df: pd.DataFrame, year: int, dim_col: str, dim_labels: dict) -> pd.DataFrame:
    g = df[df["anio"] == year]
    rows = []
    for dim_val, gd in g.groupby(dim_col):
        if dim_val not in dim_labels:
            continue
        w = gd["fex"].to_numpy()
        superior = gd["cine11_hom_cod"].isin([5, 6, 7]).to_numpy()
        rows.append({
            "dim_val": dim_labels[dim_val],
            "ocupados": float(w.sum()),
            "rem_mensual_prom": weighted_mean(gd["rem_mensual"].to_numpy(), w),
            "rem_hora_prom": weighted_mean(gd["ingreso_hora_real"].to_numpy(), w),
            "participacion_superior": float(w[superior].sum() / w.sum()),
        })
    return pd.DataFrame(rows).sort_values("rem_mensual_prom", ascending=False)


def recent_graduates(df: pd.DataFrame) -> dict:
    out = {}
    for year in (START_YEAR, END_YEAR):
        g = df[(df["anio"] == year) & (df["cine11_hom_cod"] == 6) & (df["edad"].between(25, 29))]
        w = g["fex"].to_numpy()
        out[year] = {
            "ocupados": float(w.sum()),
            "rem_mensual_prom": weighted_mean(g["rem_mensual"].to_numpy(), w),
            "rem_mensual_mediana": weighted_quantile(g["rem_mensual"].to_numpy(), w, 0.5),
        }
    return out


def percentile_lookup(df: pd.DataFrame, year: int, metric_col: str, values: dict) -> dict:
    """Para cada logro educativo, ubica su promedio dentro de la distribución
    nacional ponderada del año (percentil aproximado)."""
    g = df[df["anio"] == year]
    v = g[metric_col].to_numpy()
    w = g["fex"].to_numpy()
    sorter = np.argsort(v)
    v_sorted = v[sorter]
    w_sorted = w[sorter]
    cw = np.cumsum(w_sorted)
    cw /= cw[-1]
    out = {}
    for cod, val in values.items():
        idx = np.searchsorted(v_sorted, val)
        idx = min(idx, len(cw) - 1)
        out[cod] = float(cw[idx]) * 100
    return out


def main():
    TABLE_DIR.mkdir(parents=True, exist_ok=True)
    JSON_PATH.parent.mkdir(parents=True, exist_ok=True)

    print("Cargando BaseconCINE11.dta ...")
    df = load_data()
    print(f"Filas válidas: {len(df):,}")

    series = national_series(df)
    series.to_csv(TABLE_DIR / "01_serie_nacional_educacion.csv", index=False)

    headline = national_headline(df)
    decomp_mensual = decomposition(series, "rem_mensual_prom")
    decomp_hora = decomposition(series, "rem_hora_prom")

    sexo_ct = crosstab(df, END_YEAR, "sexo_hom_cod", {1: "Hombre", 2: "Mujer"})
    sexo_ct.to_csv(TABLE_DIR / "02_cruce_sexo_educacion.csv", index=False)

    formalidad_ct = crosstab(df, END_YEAR, "formalidad_cod", {1: "Formal", 2: "Informal"})
    formalidad_ct.to_csv(TABLE_DIR / "03_cruce_formalidad_educacion.csv", index=False)

    sector_rank = ranking(df, END_YEAR, "sector_hom_cod", SECTOR_LABELS)
    sector_rank.to_csv(TABLE_DIR / "04_ranking_sector.csv", index=False)

    depto_rank = ranking(df, END_YEAR, "depto", DEPARTMENTS_33)
    depto_rank.to_csv(TABLE_DIR / "05_ranking_departamento.csv", index=False)

    grads = recent_graduates(df)

    latest = series[series["anio"] == END_YEAR].set_index("cine11_cod")
    mensual_by_cod = latest["rem_mensual_prom"].to_dict()
    hora_by_cod = latest["rem_hora_prom"].to_dict()
    pct_mensual = percentile_lookup(df, END_YEAR, "rem_mensual", mensual_by_cod)
    pct_hora = percentile_lookup(df, END_YEAR, "ingreso_hora_real", hora_by_cod)

    early = series[series["anio"] == START_YEAR].set_index("cine11_cod")

    baseline_mensual_2025 = mensual_by_cod[1]
    baseline_hora_2025 = hora_by_cod[1]

    niveles = []
    for cod in CINE11_ORDER:
        r25 = latest.loc[cod]
        r10 = early.loc[cod] if cod in early.index else None
        crec_mensual = (r25["rem_mensual_prom"] / r10["rem_mensual_prom"]) ** (1 / (END_YEAR - START_YEAR)) - 1 if r10 is not None else None
        crec_hora = (r25["rem_hora_prom"] / r10["rem_hora_prom"]) ** (1 / (END_YEAR - START_YEAR)) - 1 if r10 is not None else None
        niveles.append({
            "cod": cod,
            "label": CINE11_LABELS[cod],
            "ocupados2025": r25["ocupados"],
            "participacion2025": r25["participacion"],
            "remMensual2025": r25["rem_mensual_prom"],
            "remMensualMediana2025": r25["rem_mensual_mediana"],
            "remHora2025": r25["rem_hora_prom"],
            "remHoraMediana2025": r25["rem_hora_mediana"],
            "remMensual2010": None if r10 is None else r10["rem_mensual_prom"],
            "remHora2010": None if r10 is None else r10["rem_hora_prom"],
            "crecAnualMensual": crec_mensual,
            "crecAnualHora": crec_hora,
            "razonMensual": r25["rem_mensual_prom"] / baseline_mensual_2025,
            "razonHora": r25["rem_hora_prom"] / baseline_hora_2025,
            "percentilMensual": pct_mensual[cod],
            "percentilHora": pct_hora[cod],
        })

    payload = {
        "niveles": niveles,
        "serieNacional": [
            {
                "anio": int(row.anio),
                "cod": int(row.cine11_cod),
                "remMensual": row.rem_mensual_prom,
                "remHora": row.rem_hora_prom,
                "ocupados": row.ocupados,
                "participacion": row.participacion,
            }
            for row in series.itertuples(index=False)
        ],
        "sexo": [
            {"grupo": row.dim_val, "cod": int(row.cine11_cod), "label": row.cine11,
             "remMensual": row.rem_mensual_prom, "remHora": row.rem_hora_prom, "ocupados": row.ocupados}
            for row in sexo_ct.itertuples(index=False)
        ],
        "formalidad": [
            {"grupo": row.dim_val, "cod": int(row.cine11_cod), "label": row.cine11,
             "remMensual": row.rem_mensual_prom, "remHora": row.rem_hora_prom, "ocupados": row.ocupados}
            for row in formalidad_ct.itertuples(index=False)
        ],
        "sectores": [
            {"label": row.dim_val, "ocupados": row.ocupados, "remMensual": row.rem_mensual_prom,
             "remHora": row.rem_hora_prom, "participacionSuperior": row.participacion_superior}
            for row in sector_rank.itertuples(index=False)
        ],
        "departamentos": [
            {"label": row.dim_val, "ocupados": row.ocupados, "remMensual": row.rem_mensual_prom,
             "remHora": row.rem_hora_prom, "participacionSuperior": row.participacion_superior}
            for row in depto_rank.itertuples(index=False)
        ],
        "nacional": {
            "ocupados2010": headline[START_YEAR]["ocupados"],
            "ocupados2025": headline[END_YEAR]["ocupados"],
            "remMensual2010": headline[START_YEAR]["rem_mensual_prom"],
            "remMensual2025": headline[END_YEAR]["rem_mensual_prom"],
            "remHora2010": headline[START_YEAR]["rem_hora_prom"],
            "remHora2025": headline[END_YEAR]["rem_hora_prom"],
            "crecAnualMensual": headline["crec_anual_mensual"],
            "crecAnualHora": headline["crec_anual_hora"],
            "decompMensual": decomp_mensual,
            "decompHora": decomp_hora,
            "recienEgresados2010": grads[START_YEAR],
            "recienEgresados2025": grads[END_YEAR],
        },
    }

    with open(JSON_PATH, "w", encoding="utf-8") as f:
        json.dump(payload, f, ensure_ascii=False, indent=None, separators=(",", ":"))

    print(f"JSON escrito en {JSON_PATH} ({JSON_PATH.stat().st_size/1024:.1f} KB)")
    print("Nacional 2025:", headline[END_YEAR])
    print("Descomposición mensual:", decomp_mensual)


if __name__ == "__main__":
    main()
