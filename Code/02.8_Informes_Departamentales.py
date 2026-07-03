from __future__ import annotations

import csv
import math
import os
import re
import unicodedata
from pathlib import Path

import numpy as np
import pandas as pd
from PIL import Image, ImageDraw, ImageFont


PROJECT_ROOT = Path(__file__).resolve().parents[1]
PIB_DEP_XLSX = Path(
    os.environ.get(
        "PIB_DEP_XLSX",
        r"C:\Users\olive\Downloads\anex-PIBDep-departamento-2024pr.xlsx",
    )
)
GEIH_DTA_CANDIDATES = [
    PROJECT_ROOT / "Datos" / "Processed" / "Paper-GEIH_base_modelo_personas_2008_2025.dta",
    PROJECT_ROOT / "Datos" / "Processed" / "GEIH_base_modelo_personas_2008_2025.dta",
    PROJECT_ROOT / "Outputs" / "tables" / "Paper-GEIH_base_modelo_personas_2008_2025.dta",
    PROJECT_ROOT / "Outputs" / "tables" / "GEIH_base_modelo_personas_2008_2025.dta",
]
GEIH_DTA = next((path for path in GEIH_DTA_CANDIDATES if path.exists()), GEIH_DTA_CANDIDATES[0])
GEOMETRY_CSV = PROJECT_ROOT / "DocumentacionAuxiliar" / "Geometria" / "gadm41_COL_1_polygons.csv"

TABLE_DIR = PROJECT_ROOT / "Paper" / "tables"
SECTION_DIR = PROJECT_ROOT / "Paper" / "sections"
FIGURE_DIR = PROJECT_ROOT / "Paper" / "figures"
OUTPUT_TABLE_DIR = PROJECT_ROOT / "Outputs" / "tables"
OUTPUT_FIGURE_DIR = PROJECT_ROOT / "Outputs" / "Figures"

for directory in [TABLE_DIR, SECTION_DIR, FIGURE_DIR, OUTPUT_TABLE_DIR, OUTPUT_FIGURE_DIR]:
    directory.mkdir(parents=True, exist_ok=True)

EXCLUDED_YEARS = {2020}
MONTHS_PER_WEEK = 52.0 / 12.0

PANEL_24_START = 2010
PANEL_33_START = 2014
PRODUCTIVITY_END = 2024
REMUNERATION_END = 2025

DEPARTMENTS_24 = {
    5: "Antioquia",
    8: "Atlántico",
    11: "Bogotá D.C.",
    13: "Bolívar",
    15: "Boyacá",
    17: "Caldas",
    18: "Caquetá",
    19: "Cauca",
    20: "Cesar",
    23: "Córdoba",
    25: "Cundinamarca",
    27: "Chocó",
    41: "Huila",
    44: "La Guajira",
    47: "Magdalena",
    50: "Meta",
    52: "Nariño",
    54: "Norte de Santander",
    63: "Quindío",
    66: "Risaralda",
    68: "Santander",
    70: "Sucre",
    73: "Tolima",
    76: "Valle del Cauca",
}

DEPARTMENTS_33 = {
    **DEPARTMENTS_24,
    81: "Arauca",
    85: "Casanare",
    86: "Putumayo",
    88: "San Andrés y Providencia",
    91: "Amazonas",
    94: "Guainía",
    95: "Guaviare",
    97: "Vaupés",
    99: "Vichada",
}


def normalize_name(value: object) -> str:
    text = str(value).split(":")[0].strip().upper()
    text = "".join(ch for ch in unicodedata.normalize("NFKD", text) if not unicodedata.combining(ch))
    text = re.sub(r"[^A-Z0-9 ]+", " ", text)
    text = re.sub(r"\s+", " ", text).strip()
    if text.startswith("SAN ANDRES"):
        return "SAN ANDRES Y PROVIDENCIA"
    return text


DEPARTMENT_CODE_BY_NAME = {normalize_name(name): code for code, name in DEPARTMENTS_33.items()}


def parse_year(value: object) -> int | None:
    if pd.isna(value):
        return None
    text = str(value).strip().lower().replace("pr", "").replace("p", "")
    try:
        return int(float(text))
    except ValueError:
        return None


def cagr(start_value: float, end_value: float, start_year: int, end_year: int) -> float:
    if start_value <= 0 or end_value <= 0 or end_year <= start_year:
        return np.nan
    return (end_value / start_value) ** (1 / (end_year - start_year)) - 1


def fmt_num_es(value: float, digits: int = 1) -> str:
    if pd.isna(value):
        return "--"
    text = f"{value:,.{digits}f}"
    return text.replace(",", "X").replace(".", ",").replace("X", ".")


def fmt_pct_es(value: float, digits: int = 1) -> str:
    if pd.isna(value):
        return "--"
    return fmt_num_es(value * 100, digits) + r"\%"


def escape_latex(text: object) -> str:
    replacements = {
        "\\": r"\textbackslash{}",
        "&": r"\&",
        "%": r"\%",
        "$": r"\$",
        "#": r"\#",
        "_": r"\_",
        "{": r"\{",
        "}": r"\}",
        "~": r"\textasciitilde{}",
        "^": r"\textasciicircum{}",
    }
    result = str(text)
    for old, new in replacements.items():
        result = result.replace(old, new)
    return result


def latex_id(text: object) -> str:
    result = normalize_name(text).lower().replace(" ", "_")
    return re.sub(r"[^a-z0-9_]+", "", result)


def font(size: int, bold: bool = False) -> ImageFont.FreeTypeFont | ImageFont.ImageFont:
    candidates = [
        r"C:\Windows\Fonts\georgiab.ttf" if bold else r"C:\Windows\Fonts\georgia.ttf",
        r"C:\Windows\Fonts\timesbd.ttf" if bold else r"C:\Windows\Fonts\times.ttf",
        r"C:\Windows\Fonts\arialbd.ttf" if bold else r"C:\Windows\Fonts\arial.ttf",
    ]
    for candidate in candidates:
        try:
            return ImageFont.truetype(candidate, size)
        except OSError:
            continue
    return ImageFont.load_default()


def load_pib_departamental(departments: dict[int, str], start_year: int, end_year: int) -> pd.DataFrame:
    raw = pd.read_excel(PIB_DEP_XLSX, sheet_name="Cuadro 2", header=None)
    header_rows = raw.index[
        raw.iloc[:, 0].astype(str).str.contains("Cuentas Nacionales", case=False, na=False)
    ].tolist()

    rows = []
    for header_idx in header_rows:
        department_title = raw.iat[header_idx - 5, 0]
        code = DEPARTMENT_CODE_BY_NAME.get(normalize_name(department_title))
        if code not in departments:
            continue

        year_cols = {col: parse_year(raw.iat[header_idx, col]) for col in range(3, min(23, raw.shape[1]))}

        pib_row = None
        for idx in range(header_idx + 1, min(header_idx + 25, len(raw))):
            if str(raw.iat[idx, 2]).strip().upper() == "PIB DEPARTAMENTAL":
                pib_row = idx
                break
        if pib_row is None:
            raise ValueError(f"No se encontró el renglón de PIB departamental para {department_title}")

        for col, year in year_cols.items():
            value = pd.to_numeric(raw.iat[pib_row, col], errors="coerce")
            if year is not None and start_year <= year <= end_year and pd.notna(value):
                rows.append(
                    {
                        "anio": year,
                        "depto": code,
                        "departamento": departments[code],
                        "pib_miles_millones_2015": float(value),
                    }
                )

    pib = pd.DataFrame(rows)
    expected = len(departments) * (end_year - start_year + 1)
    if len(pib) != expected:
        raise ValueError(f"Se esperaban {expected} filas de PIB departamental y se obtuvieron {len(pib)}")
    return pib


def load_geih_base() -> pd.DataFrame:
    columns = ["anio", "depto", "fex", "horas", "ingreso_hora_real"]
    data_path = None
    for candidate in GEIH_DTA_CANDIDATES:
        if not candidate.exists():
            continue
        try:
            reader = pd.read_stata(candidate, columns=columns, convert_categoricals=False, iterator=True)
            close = getattr(reader, "close", None)
            if close is not None:
                close()
            data_path = candidate
            break
        except ValueError:
            continue
    if data_path is None:
        raise FileNotFoundError(
            "No se encontró una base GEIH de personas con las columnas requeridas: "
            + ", ".join(columns)
        )
    geih = pd.read_stata(data_path, columns=columns, convert_categoricals=False)
    for col in columns:
        geih[col] = pd.to_numeric(geih[col], errors="coerce")
    geih = geih[(geih["fex"] > 0) & geih["anio"].notna() & geih["depto"].notna()].copy()
    geih["anio"] = geih["anio"].astype(int)
    geih["depto"] = geih["depto"].astype(int)
    return geih


def slice_panel(
    geih: pd.DataFrame,
    departments: dict[int, str],
    start_year: int,
    end_year: int,
) -> pd.DataFrame:
    return geih[
        geih["anio"].between(start_year, end_year)
        & ~geih["anio"].isin(EXCLUDED_YEARS)
        & geih["depto"].isin(departments)
    ].copy()


def build_labor_panel(
    geih: pd.DataFrame,
    departments: dict[int, str],
    start_year: int,
    end_year: int,
) -> pd.DataFrame:
    panel = slice_panel(geih, departments, start_year, end_year)
    panel["horas_validas"] = panel["horas"].where(panel["horas"].between(1, 112))
    panel["horas_sem_expand"] = panel["fex"] * panel["horas_validas"]
    panel["fex_horas_validas"] = panel["fex"].where(panel["horas_validas"].notna(), 0)
    labor = (
        panel.groupby(["anio", "depto"], as_index=False)
        .agg(
            ocupados=("fex", "sum"),
            ocupados_horas_validas=("fex_horas_validas", "sum"),
            horas_sem_expandidas_validas=("horas_sem_expand", "sum"),
        )
        .assign(
            horas_semanales_promedio=lambda x: x["horas_sem_expandidas_validas"] / x["ocupados_horas_validas"],
            share_ocupados_sin_horas_validas=lambda x: 1 - x["ocupados_horas_validas"] / x["ocupados"],
            horas_sem_expandidas=lambda x: x["ocupados"] * x["horas_semanales_promedio"],
            horas_anuales=lambda x: x["horas_sem_expandidas"] * 52,
            horas_mensuales=lambda x: x["horas_anuales"] / 12,
        )
    )
    labor["departamento"] = labor["depto"].map(departments)
    required_years = set(range(start_year, end_year + 1)) - EXCLUDED_YEARS
    counts = labor.groupby("depto")["anio"].nunique()
    missing = counts[counts != len(required_years)]
    if not missing.empty:
        raise ValueError(f"Departamentos sin todos los años GEIH requeridos: {missing.to_dict()}")
    return labor.sort_values(["departamento", "anio"])


def build_remuneration_panel(
    geih: pd.DataFrame,
    departments: dict[int, str],
    start_year: int,
    end_year: int,
) -> tuple[pd.DataFrame, pd.DataFrame, dict[str, float]]:
    labor = build_labor_panel(geih, departments, start_year, end_year)
    panel = slice_panel(geih, departments, start_year, end_year)
    valid = panel[
        panel["horas"].between(1, 112)
        & panel["ingreso_hora_real"].notna()
        & (panel["ingreso_hora_real"] > 0)
    ].copy()
    valid["horas_mensuales_remuneracion_valida"] = valid["fex"] * valid["horas"] * MONTHS_PER_WEEK
    valid["rem_total_mensual"] = valid["ingreso_hora_real"] * valid["horas_mensuales_remuneracion_valida"]
    rem = valid.groupby(["anio", "depto"], as_index=False).agg(
        ocupados_remuneracion_valida=("fex", "sum"),
        horas_mensuales_remuneracion_valida=("horas_mensuales_remuneracion_valida", "sum"),
        rem_total_mensual=("rem_total_mensual", "sum"),
    )
    series = labor.merge(rem, on=["anio", "depto"], how="inner")
    series["rem_por_trabajador"] = series["rem_total_mensual"] / series["ocupados_remuneracion_valida"]
    series["rem_por_hora"] = series["rem_total_mensual"] / series["horas_mensuales_remuneracion_valida"]
    series["share_ocupados_remuneracion_valida"] = (
        series["ocupados_remuneracion_valida"] / series["ocupados"]
    )

    rows = []
    for depto, part in series.groupby("depto"):
        start = part[part["anio"] == start_year].iloc[0]
        end = part[part["anio"] == end_year].iloc[0]
        rows.append(
            {
                "depto": depto,
                "departamento": end["departamento"],
                "ocupados_inicio": start["ocupados"],
                "ocupados_fin": end["ocupados"],
                "horas_sem_inicio": start["horas_semanales_promedio"],
                "horas_sem_fin": end["horas_semanales_promedio"],
                "rem_trabajador_inicio": start["rem_por_trabajador"],
                "rem_trabajador_fin": end["rem_por_trabajador"],
                "rem_hora_inicio": start["rem_por_hora"],
                "rem_hora_fin": end["rem_por_hora"],
                "crec_ocupados": cagr(start["ocupados"], end["ocupados"], start_year, end_year),
                "crec_rem_trabajador": cagr(start["rem_por_trabajador"], end["rem_por_trabajador"], start_year, end_year),
                "crec_rem_hora": cagr(start["rem_por_hora"], end["rem_por_hora"], start_year, end_year),
                "share_ocupados_remuneracion_valida_fin": end["share_ocupados_remuneracion_valida"],
            }
        )
    summary = pd.DataFrame(rows)
    summary["ranking_rem_trabajador"] = summary["rem_trabajador_fin"].rank(ascending=False, method="min").astype(int)
    summary["ranking_rem_hora"] = summary["rem_hora_fin"].rank(ascending=False, method="min").astype(int)
    summary["ranking_crec_rem_trabajador"] = summary["crec_rem_trabajador"].rank(ascending=False, method="min").astype(int)
    summary["ranking_crec_rem_hora"] = summary["crec_rem_hora"].rank(ascending=False, method="min").astype(int)
    summary["rem_trabajador_rel_lider"] = summary["rem_trabajador_fin"] / summary["rem_trabajador_fin"].max()
    summary["rem_hora_rel_lider"] = summary["rem_hora_fin"] / summary["rem_hora_fin"].max()
    summary["rem_trabajador_brecha_lider"] = 1 - summary["rem_trabajador_rel_lider"]
    summary["rem_hora_brecha_lider"] = 1 - summary["rem_hora_rel_lider"]

    start_agg = series[series["anio"] == start_year]
    end_agg = series[series["anio"] == end_year]
    rem_trab_start = start_agg["rem_total_mensual"].sum() / start_agg["ocupados_remuneracion_valida"].sum()
    rem_trab_end = end_agg["rem_total_mensual"].sum() / end_agg["ocupados_remuneracion_valida"].sum()
    rem_hora_start = (
        start_agg["rem_total_mensual"].sum() / start_agg["horas_mensuales_remuneracion_valida"].sum()
    )
    rem_hora_end = end_agg["rem_total_mensual"].sum() / end_agg["horas_mensuales_remuneracion_valida"].sum()
    benchmarks = {
        "start_year": start_year,
        "end_year": end_year,
        "n_departamentos": len(departments),
        "ocupados_fin": end_agg["ocupados"].sum(),
        "rem_trabajador_inicio": rem_trab_start,
        "rem_trabajador_fin": rem_trab_end,
        "rem_hora_inicio": rem_hora_start,
        "rem_hora_fin": rem_hora_end,
        "crec_rem_trabajador": cagr(rem_trab_start, rem_trab_end, start_year, end_year),
        "crec_rem_hora": cagr(rem_hora_start, rem_hora_end, start_year, end_year),
    }
    return series.sort_values(["departamento", "anio"]), summary, benchmarks


def build_productivity_panel(
    geih: pd.DataFrame,
    departments: dict[int, str],
    start_year: int,
    end_year: int,
) -> tuple[pd.DataFrame, pd.DataFrame, dict[str, float]]:
    labor = build_labor_panel(geih, departments, start_year, end_year)
    pib = load_pib_departamental(departments, start_year, end_year)
    series = pib.merge(labor, on=["anio", "depto", "departamento"], how="inner")
    series["pib_pesos_2015"] = series["pib_miles_millones_2015"] * 1e9
    series["pib_trabajador"] = series["pib_pesos_2015"] / series["ocupados"] / 1e6
    series["pib_hora"] = series["pib_pesos_2015"] / series["horas_anuales"] / 1000

    rows = []
    for depto, part in series.groupby("depto"):
        start = part[part["anio"] == start_year].iloc[0]
        end = part[part["anio"] == end_year].iloc[0]
        rows.append(
            {
                "depto": depto,
                "departamento": end["departamento"],
                "pib_inicio": start["pib_miles_millones_2015"],
                "pib_fin": end["pib_miles_millones_2015"],
                "ocupados_inicio": start["ocupados"],
                "ocupados_fin": end["ocupados"],
                "horas_sem_inicio": start["horas_semanales_promedio"],
                "horas_sem_fin": end["horas_semanales_promedio"],
                "pib_trabajador_inicio": start["pib_trabajador"],
                "pib_trabajador_fin": end["pib_trabajador"],
                "pib_hora_inicio": start["pib_hora"],
                "pib_hora_fin": end["pib_hora"],
                "crec_pib": cagr(start["pib_miles_millones_2015"], end["pib_miles_millones_2015"], start_year, end_year),
                "crec_ocupados": cagr(start["ocupados"], end["ocupados"], start_year, end_year),
                "crec_pib_trabajador": cagr(start["pib_trabajador"], end["pib_trabajador"], start_year, end_year),
                "crec_pib_hora": cagr(start["pib_hora"], end["pib_hora"], start_year, end_year),
            }
        )
    summary = pd.DataFrame(rows)
    summary["ranking_pib_trabajador"] = summary["pib_trabajador_fin"].rank(ascending=False, method="min").astype(int)
    summary["ranking_pib_hora"] = summary["pib_hora_fin"].rank(ascending=False, method="min").astype(int)
    summary["ranking_crec_pib_trabajador"] = summary["crec_pib_trabajador"].rank(ascending=False, method="min").astype(int)
    summary["ranking_crec_pib_hora"] = summary["crec_pib_hora"].rank(ascending=False, method="min").astype(int)
    summary["pib_hora_rel_lider"] = summary["pib_hora_fin"] / summary["pib_hora_fin"].max()

    start_agg = series[series["anio"] == start_year]
    end_agg = series[series["anio"] == end_year]
    pib_start = start_agg["pib_miles_millones_2015"].sum() * 1e9
    pib_end = end_agg["pib_miles_millones_2015"].sum() * 1e9
    pib_trab_start = pib_start / start_agg["ocupados"].sum() / 1e6
    pib_trab_end = pib_end / end_agg["ocupados"].sum() / 1e6
    pib_hora_start = pib_start / start_agg["horas_anuales"].sum() / 1000
    pib_hora_end = pib_end / end_agg["horas_anuales"].sum() / 1000
    benchmarks = {
        "start_year": start_year,
        "end_year": end_year,
        "n_departamentos": len(departments),
        "ocupados_fin": end_agg["ocupados"].sum(),
        "pib_trabajador_inicio": pib_trab_start,
        "pib_trabajador_fin": pib_trab_end,
        "pib_hora_inicio": pib_hora_start,
        "pib_hora_fin": pib_hora_end,
        "crec_pib_trabajador": cagr(pib_trab_start, pib_trab_end, start_year, end_year),
        "crec_pib_hora": cagr(pib_hora_start, pib_hora_end, start_year, end_year),
        "crec_ocupados": cagr(start_agg["ocupados"].sum(), end_agg["ocupados"].sum(), start_year, end_year),
    }
    return series.sort_values(["departamento", "anio"]), summary, benchmarks


def build_relation_table(
    productivity_summary: pd.DataFrame,
    remuneration_series: pd.DataFrame,
    start_year: int,
    end_year: int,
) -> tuple[pd.DataFrame, dict[str, float]]:
    rem_start = remuneration_series[remuneration_series["anio"] == start_year][
        [
            "depto",
            "rem_por_trabajador",
            "rem_por_hora",
            "rem_total_mensual",
            "ocupados_remuneracion_valida",
            "horas_mensuales_remuneracion_valida",
        ]
    ].rename(
        columns={
            "rem_por_trabajador": "rem_trabajador_inicio",
            "rem_por_hora": "rem_hora_inicio",
            "rem_total_mensual": "rem_total_mensual_inicio",
            "ocupados_remuneracion_valida": "ocupados_rem_inicio",
            "horas_mensuales_remuneracion_valida": "horas_mensuales_inicio",
        }
    )
    rem_end = remuneration_series[remuneration_series["anio"] == end_year][
        [
            "depto",
            "rem_por_trabajador",
            "rem_por_hora",
            "rem_total_mensual",
            "ocupados_remuneracion_valida",
            "horas_mensuales_remuneracion_valida",
        ]
    ].rename(
        columns={
            "rem_por_trabajador": "rem_trabajador_fin",
            "rem_por_hora": "rem_hora_fin",
            "rem_total_mensual": "rem_total_mensual_fin",
            "ocupados_remuneracion_valida": "ocupados_rem_fin",
            "horas_mensuales_remuneracion_valida": "horas_mensuales_fin",
        }
    )
    table = productivity_summary.merge(rem_start, on="depto", how="inner").merge(rem_end, on="depto", how="inner")
    table["crec_rem_trabajador"] = table.apply(
        lambda row: cagr(row["rem_trabajador_inicio"], row["rem_trabajador_fin"], start_year, end_year),
        axis=1,
    )
    table["crec_rem_hora"] = table.apply(
        lambda row: cagr(row["rem_hora_inicio"], row["rem_hora_fin"], start_year, end_year),
        axis=1,
    )

    for x_col, y_col, suffix in [
        ("pib_trabajador_fin", "rem_trabajador_fin", "trabajador"),
        ("pib_hora_fin", "rem_hora_fin", "hora"),
    ]:
        x = table[x_col].astype(float).to_numpy()
        y = table[y_col].astype(float).to_numpy()
        slope, intercept = np.polyfit(x, y, 1)
        pred = intercept + slope * table[x_col]
        table[f"rem_predicha_{suffix}"] = pred
        table[f"residuo_{suffix}"] = table[y_col] - pred
        table[f"residuo_pct_{suffix}"] = table[f"residuo_{suffix}"] / pred

    benchmarks = {
        "corr_nivel_trabajador": table[["pib_trabajador_fin", "rem_trabajador_fin"]].corr().iloc[0, 1],
        "corr_nivel_hora": table[["pib_hora_fin", "rem_hora_fin"]].corr().iloc[0, 1],
        "corr_crec_trabajador": table[["crec_pib_trabajador", "crec_rem_trabajador"]].corr().iloc[0, 1],
        "corr_crec_hora": table[["crec_pib_hora", "crec_rem_hora"]].corr().iloc[0, 1],
    }
    return table, benchmarks


def write_csv(df: pd.DataFrame, name: str) -> None:
    for directory in [TABLE_DIR, OUTPUT_TABLE_DIR]:
        df.to_csv(directory / name, index=False, encoding="utf-8-sig")


def write_benchmarks(benchmarks: dict[str, float], name: str) -> None:
    write_csv(pd.DataFrame([benchmarks]), name)


def write_remuneration_level_table(summary: pd.DataFrame, end_year: int, suffix: str) -> None:
    ranked = summary.sort_values("ranking_rem_trabajador")
    lines = [
        r"\begin{table}[H]",
        r"\centering",
        rf"\caption{{Niveles de remuneración laboral por departamento, {end_year}}}",
        rf"\label{{tab:dept_remuneracion_niveles_{suffix}}}",
        r"\scriptsize",
        r"\begin{tabular}{lrrrrrr}",
        r"\toprule",
        r"Departamento & Rem./trab. & Brecha & Rem./hora & Brecha & Ocupados & Horas/trab. \\",
        r"\midrule",
    ]
    for _, row in ranked.iterrows():
        lines.append(
            f"{escape_latex(row['departamento'])} & "
            f"{fmt_num_es(row['rem_trabajador_fin'] / 1e6, 2)} & "
            f"{fmt_pct_es(row['rem_trabajador_brecha_lider'], 1)} & "
            f"{fmt_num_es(row['rem_hora_fin'] / 1000, 1)} & "
            f"{fmt_pct_es(row['rem_hora_brecha_lider'], 1)} & "
            f"{fmt_num_es(row['ocupados_fin'] / 1e6, 2)} & "
            f"{fmt_num_es(row['horas_sem_fin'], 1)} \\\\"
        )
    lines.extend(
        [
            r"\bottomrule",
            r"\end{tabular}",
            r"\caption*{\footnotesize Nota: remuneración por trabajador en millones de pesos al mes entre ocupados con ingreso horario positivo y horas válidas; remuneración por hora en miles de pesos para ese mismo universo. Ocupados en millones. Horas/trab. corresponde a horas semanales promedio por ocupado. La brecha se calcula como la distancia porcentual frente al departamento líder en cada indicador. Un valor de 0\% corresponde al líder; un valor de 30\% indica que el departamento está 30\% por debajo del líder. Fuente: cálculos propios con GEIH.}",
            r"\end{table}",
        ]
    )
    (SECTION_DIR / f"dept_remuneracion_niveles_{suffix}.tex").write_text("\n".join(lines), encoding="utf-8")


def write_remuneration_growth_table(summary: pd.DataFrame, start_year: int, end_year: int, suffix: str) -> None:
    ranked = summary.sort_values("ranking_crec_rem_trabajador")
    lines = [
        r"\begin{table}[H]",
        r"\centering",
        rf"\caption{{Crecimiento de la remuneración laboral por departamento, {start_year}--{end_year}}}",
        rf"\label{{tab:dept_remuneracion_crecimientos_{suffix}}}",
        r"\scriptsize",
        r"\begin{tabular}{lrrrr}",
        r"\toprule",
        r"Departamento & Crec. rem./trab. & Puesto & Crec. rem./hora & Puesto \\",
        r"\midrule",
    ]
    for _, row in ranked.iterrows():
        lines.append(
            f"{escape_latex(row['departamento'])} & "
            f"{fmt_pct_es(row['crec_rem_trabajador'], 1)} & "
            f"{int(row['ranking_crec_rem_trabajador'])} & "
            f"{fmt_pct_es(row['crec_rem_hora'], 1)} & "
            f"{int(row['ranking_crec_rem_hora'])} \\\\"
        )
    lines.extend(
        [
            r"\bottomrule",
            r"\end{tabular}",
            r"\caption*{\footnotesize Nota: tasas de crecimiento anualizadas. Los indicadores de remuneración se calculan entre ocupados con ingreso horario positivo y horas válidas. Fuente: cálculos propios con GEIH.}",
            r"\end{table}",
        ]
    )
    (SECTION_DIR / f"dept_remuneracion_crecimientos_{suffix}.tex").write_text("\n".join(lines), encoding="utf-8")


def convergence_stats(
    summary: pd.DataFrame,
    *,
    start_col: str,
    end_col: str,
    start_year: int,
    end_year: int,
) -> dict[str, float]:
    data = summary[["departamento", start_col, end_col]].dropna().copy()
    data = data[(data[start_col] > 0) & (data[end_col] > 0)]
    years = end_year - start_year
    x = np.log(data[start_col].astype(float).to_numpy())
    y = (np.log(data[end_col].astype(float).to_numpy()) - x) / years
    x_design = np.column_stack([np.ones(len(x)), x])
    alpha, beta = np.linalg.lstsq(x_design, y, rcond=None)[0]
    y_hat = x_design @ np.array([alpha, beta])
    resid = y - y_hat
    n, k = len(y), 2
    sigma2 = float((resid @ resid) / max(n - k, 1))
    vcov = sigma2 * np.linalg.inv(x_design.T @ x_design)
    beta_se = float(np.sqrt(vcov[1, 1]))
    t_stat = float(beta / beta_se) if beta_se > 0 else np.nan
    ss_total = float(((y - y.mean()) @ (y - y.mean())))
    r2 = 1 - float(resid @ resid) / ss_total if ss_total > 0 else np.nan
    lambda_speed = np.nan
    half_life = np.nan
    if beta < 0 and 1 + beta * years > 0:
        lambda_speed = -math.log(1 + beta * years) / years
        half_life = math.log(2) / lambda_speed
    return {
        "n": n,
        "alpha": float(alpha),
        "beta": float(beta),
        "beta_se": beta_se,
        "t_stat": t_stat,
        "r2": float(r2),
        "lambda": float(lambda_speed) if not pd.isna(lambda_speed) else np.nan,
        "vida_media_anios": float(half_life) if not pd.isna(half_life) else np.nan,
    }


def build_remuneration_convergence(summary: pd.DataFrame, start_year: int, end_year: int) -> tuple[pd.DataFrame, pd.DataFrame]:
    specs = [
        ("Remuneración por trabajador", "rem_trabajador_inicio", "rem_trabajador_fin"),
        ("Remuneración por hora", "rem_hora_inicio", "rem_hora_fin"),
    ]
    rows = []
    panel_rows = []
    years = end_year - start_year
    for label, start_col, end_col in specs:
        stats = convergence_stats(summary, start_col=start_col, end_col=end_col, start_year=start_year, end_year=end_year)
        direction = "Convergencia" if stats["beta"] < 0 else "Divergencia"
        rows.append(
            {
                "indicador": label,
                **stats,
                "lectura": direction,
            }
        )
        for _, row in summary.iterrows():
            if row[start_col] > 0 and row[end_col] > 0:
                panel_rows.append(
                    {
                        "indicador": label,
                        "depto": row["depto"],
                        "departamento": row["departamento"],
                        "nivel_inicial_log": math.log(float(row[start_col])),
                        "crecimiento_anualizado": (math.log(float(row[end_col])) - math.log(float(row[start_col]))) / years,
                        "nivel_inicial": float(row[start_col]),
                        "nivel_final": float(row[end_col]),
                        "ocupados_fin": float(row["ocupados_fin"]),
                    }
                )
    return pd.DataFrame(rows), pd.DataFrame(panel_rows)


def write_remuneration_convergence_table(convergence: pd.DataFrame, start_year: int, end_year: int) -> None:
    lines = [
        r"\begin{table}[H]",
        r"\centering",
        rf"\caption{{Convergencia beta de la remuneración laboral departamental, {start_year}--{end_year}}}",
        r"\label{tab:dept_remuneracion_convergencia_24}",
        r"\small",
        r"\begin{tabular}{lrrrrl}",
        r"\toprule",
        r"Indicador & $\beta$ & Error est. & Estad. $t$ & $R^2$ & Lectura \\",
        r"\midrule",
    ]
    for _, row in convergence.iterrows():
        lines.append(
            f"{escape_latex(row['indicador'])} & "
            f"{fmt_num_es(row['beta'], 4)} & "
            f"{fmt_num_es(row['beta_se'], 4)} & "
            f"{fmt_num_es(row['t_stat'], 2)} & "
            f"{fmt_num_es(row['r2'], 3)} & "
            f"{escape_latex(row['lectura'])} \\\\"
        )
    lines.extend(
        [
            r"\bottomrule",
            r"\end{tabular}",
            r"\caption*{\footnotesize Nota: la variable dependiente es el crecimiento anualizado del indicador y la variable explicativa es el logaritmo del nivel inicial del mismo indicador. Un coeficiente $\beta<0$ indica convergencia beta; un coeficiente $\beta>0$ indica que los departamentos con mayor remuneración inicial crecieron más rápido. Fuente: cálculos propios con GEIH.}",
            r"\end{table}",
        ]
    )
    (SECTION_DIR / "dept_remuneracion_convergencia_24.tex").write_text("\n".join(lines), encoding="utf-8")


def draw_remuneration_convergence(convergence_points: pd.DataFrame, convergence: pd.DataFrame) -> None:
    data = convergence_points.copy()
    width, height = 2300, 1250
    img = Image.new("RGB", (width, height), "white")
    draw = ImageDraw.Draw(img)
    draw.text((80, 42), "Convergencia de la remuneración laboral departamental, 2010--2025", fill="#222222", font=font(48, True))
    draw.text((80, 98), "Panel de 24 departamentos; eje horizontal en logaritmo del nivel inicial", fill="#555555", font=font(30))

    panels = [
        ((80, 185, 1115, 1045), "Remuneración por trabajador"),
        ((1235, 185, 2270, 1045), "Remuneración por hora"),
    ]
    for box, indicator in panels:
        part = data[data["indicador"] == indicator].copy()
        stats = convergence[convergence["indicador"] == indicator].iloc[0]
        left, top, right, bottom = box
        plot_left, plot_top = left + 115, top + 95
        plot_right, plot_bottom = right - 55, bottom - 105
        x = part["nivel_inicial_log"].astype(float).to_numpy()
        y = part["crecimiento_anualizado"].astype(float).to_numpy() * 100
        x_min, x_max = x.min(), x.max()
        y_min, y_max = y.min(), y.max()
        x_pad = (x_max - x_min) * 0.08
        y_pad = (y_max - y_min) * 0.16
        x_min, x_max = x_min - x_pad, x_max + x_pad
        y_min, y_max = y_min - y_pad, y_max + y_pad

        def xp(value: float) -> float:
            return plot_left + (value - x_min) / (x_max - x_min) * (plot_right - plot_left)

        def yp(value: float) -> float:
            return plot_bottom - (value - y_min) / (y_max - y_min) * (plot_bottom - plot_top)

        draw.text((left + 15, top + 18), indicator, fill="#222222", font=font(34, True))
        draw.rectangle((plot_left, plot_top, plot_right, plot_bottom), outline="#333333", width=2)
        for i in range(6):
            xg = plot_left + i / 5 * (plot_right - plot_left)
            yg = plot_top + i / 5 * (plot_bottom - plot_top)
            draw.line((xg, plot_top, xg, plot_bottom), fill="#eeeeee", width=1)
            draw.line((plot_left, yg, plot_right, yg), fill="#eeeeee", width=1)
        slope = float(stats["beta"]) * 100
        intercept = float(stats["alpha"]) * 100
        draw.line((xp(x_min), yp(intercept + slope * x_min), xp(x_max), yp(intercept + slope * x_max)), fill="#b44b3f", width=5)
        draw.text(
            (plot_left + 18, plot_top + 18),
            rf"$\beta$ = {fmt_num_es(float(stats['beta']), 4)}",
            fill="#b44b3f",
            font=font(27, True),
        )
        max_occ, min_occ = part["ocupados_fin"].max(), part["ocupados_fin"].min()
        label_offsets = {
            "Bogotá D.C.": (-135, -34),
            "Antioquia": (-120, 12),
            "Cundinamarca": (18, -34),
            "La Guajira": (18, -10),
            "Caldas": (18, -32),
            "Chocó": (18, -10),
            "Sucre": (16, 8),
            "Risaralda": (18, -30),
            "Quindío": (18, 8),
        }
        for _, row in part.iterrows():
            radius = 7 + 27 * math.sqrt((row["ocupados_fin"] - min_occ) / max(max_occ - min_occ, 1))
            px, py = xp(float(row["nivel_inicial_log"])), yp(float(row["crecimiento_anualizado"]) * 100)
            draw.ellipse((px - radius, py - radius, px + radius, py + radius), fill="#4e79a7", outline="white", width=2)
            if row["departamento"] in label_offsets:
                dx, dy = label_offsets[row["departamento"]]
                draw.text((px + dx, py + dy), row["departamento"], fill="#222222", font=font(18))
        draw.text((plot_left + 180, plot_bottom + 52), "Log nivel inicial, 2010", fill="#333333", font=font(22))
        draw.text((plot_left, plot_top - 34), "Crecimiento anualizado (%)", fill="#333333", font=font(22))

    draw.text((80, 1148), "Nota: una pendiente negativa indicaría convergencia. En ambos paneles la pendiente estimada es positiva.", fill="#555555", font=font(24))
    for directory in [FIGURE_DIR, OUTPUT_FIGURE_DIR]:
        img.save(directory / "fig_dept_remuneracion_convergencia_24.png")


def write_productivity_summary_table(summary: pd.DataFrame, start_year: int, end_year: int, suffix: str) -> None:
    ranked = summary.sort_values("crec_pib_hora", ascending=False)
    lines = [
        r"\begin{table}[H]",
        r"\centering",
        rf"\caption{{Productividad laboral departamental, {start_year}--{end_year}pr}}",
        rf"\label{{tab:dept_productividad_resumen_{suffix}}}",
        r"\scriptsize",
        r"\begin{tabular}{lrrrr}",
        r"\toprule",
        rf"Departamento & PIB/trab. {end_year}pr & Crec. & PIB/hora {end_year}pr & Crec. \\",
        r"\midrule",
    ]
    for _, row in ranked.iterrows():
        lines.append(
            f"{escape_latex(row['departamento'])} & "
            f"{fmt_num_es(row['pib_trabajador_fin'], 1)} & "
            f"{fmt_pct_es(row['crec_pib_trabajador'], 1)} & "
            f"{fmt_num_es(row['pib_hora_fin'], 1)} & "
            f"{fmt_pct_es(row['crec_pib_hora'], 1)} \\\\"
        )
    lines.extend(
        [
            r"\bottomrule",
            r"\end{tabular}",
            r"\caption*{\footnotesize Nota: PIB por trabajador en millones de pesos constantes de 2015; PIB por hora en miles de pesos constantes de 2015. Fuente: cálculos propios con DANE y GEIH.}",
            r"\end{table}",
        ]
    )
    (SECTION_DIR / f"dept_productividad_resumen_{suffix}.tex").write_text("\n".join(lines), encoding="utf-8")


def write_relation_table(table: pd.DataFrame, end_year: int, suffix: str) -> None:
    ranked = table.sort_values("residuo_pct_hora", ascending=False)
    lines = [
        r"\begin{table}[H]",
        r"\centering",
        rf"\caption{{Productividad y remuneración por departamento, {end_year}pr}}",
        rf"\label{{tab:dept_productividad_remuneracion_{suffix}}}",
        r"\scriptsize",
        r"\begin{tabular}{lrrrrr}",
        r"\toprule",
        rf"Departamento & PIB/hora {end_year}pr & Rem./hora {end_year} & Residuo & PIB/trab. {end_year}pr & Rem./trab. {end_year} \\",
        r"\midrule",
    ]
    for _, row in ranked.iterrows():
        lines.append(
            f"{escape_latex(row['departamento'])} & "
            f"{fmt_num_es(row['pib_hora_fin'], 1)} & "
            f"{fmt_num_es(row['rem_hora_fin'] / 1000, 1)} & "
            f"{fmt_pct_es(row['residuo_pct_hora'], 1)} & "
            f"{fmt_num_es(row['pib_trabajador_fin'], 1)} & "
            f"{fmt_num_es(row['rem_trabajador_fin'] / 1e6, 2)} \\\\"
        )
    lines.extend(
        [
            r"\bottomrule",
            r"\end{tabular}",
            r"\caption*{\footnotesize Nota: PIB por hora en miles de pesos constantes de 2015; remuneración por hora en miles de pesos constantes de 2025; PIB por trabajador en millones de pesos constantes de 2015; remuneración por trabajador en millones de pesos constantes de 2025 al mes. Los indicadores de remuneración se calculan entre ocupados con ingreso horario positivo y horas válidas. El residuo mide la distancia porcentual frente a la tendencia lineal entre PIB por hora y remuneración por hora. Fuente: cálculos propios con DANE y GEIH.}",
            r"\end{table}",
        ]
    )
    (SECTION_DIR / f"dept_productividad_remuneracion_{suffix}.tex").write_text("\n".join(lines), encoding="utf-8")


def hex_to_rgb(value: str) -> tuple[int, int, int]:
    value = value.lstrip("#")
    return tuple(int(value[i : i + 2], 16) for i in (0, 2, 4))


def lerp_color(a: tuple[int, int, int], b: tuple[int, int, int], t: float) -> tuple[int, int, int]:
    t = max(0.0, min(1.0, t))
    return tuple(int(a[i] + (b[i] - a[i]) * t) for i in range(3))


def sequential_color(value: float, lo: float, hi: float, palette: list[str]) -> tuple[int, int, int]:
    if pd.isna(value):
        return (238, 238, 234)
    if hi <= lo:
        return hex_to_rgb(palette[-1])
    pos = (value - lo) / (hi - lo)
    scaled = pos * (len(palette) - 1)
    idx = min(len(palette) - 2, max(0, int(math.floor(scaled))))
    return lerp_color(hex_to_rgb(palette[idx]), hex_to_rgb(palette[idx + 1]), scaled - idx)


def diverging_color(value: float, midpoint: float, limit: float, low: str, mid: str, high: str) -> tuple[int, int, int]:
    if pd.isna(value):
        return (238, 238, 234)
    if limit <= 0:
        return hex_to_rgb(mid)
    if value < midpoint:
        return lerp_color(hex_to_rgb(low), hex_to_rgb(mid), (value - (midpoint - limit)) / limit)
    return lerp_color(hex_to_rgb(mid), hex_to_rgb(high), (value - midpoint) / limit)


def read_polygons() -> dict[str, list[list[tuple[float, float]]]]:
    groups: dict[tuple[str, str], list[tuple[float, float]]] = {}
    with GEOMETRY_CSV.open("r", encoding="utf-8-sig", newline="") as fh:
        reader = csv.DictReader(fh)
        for row in reader:
            name = row["departamento_geo"]
            groups.setdefault((name, row["group"]), []).append((float(row["lon"]), float(row["lat"])))
    polygons: dict[str, list[list[tuple[float, float]]]] = {}
    for (name, _group), points in groups.items():
        polygons.setdefault(name, []).append(points)
    return polygons


def map_projection(width: int, height: int):
    lon_min, lon_max = -79.5, -66.5
    lat_min, lat_max = -4.6, 13.5
    pad_x, pad_y = 70, 70

    def project(lon: float, lat: float) -> tuple[int, int]:
        x = pad_x + (lon - lon_min) / (lon_max - lon_min) * (width - 2 * pad_x)
        y = height - pad_y - (lat - lat_min) / (lat_max - lat_min) * (height - 2 * pad_y)
        return int(x), int(y)

    return project


def department_centers(polygons: dict[str, list[list[tuple[float, float]]]]) -> dict[str, tuple[float, float]]:
    centers = {}
    for name, rings in polygons.items():
        points = [point for ring in rings for point in ring]
        lon_values = [point[0] for point in points]
        lat_values = [point[1] for point in points]
        centers[name] = ((min(lon_values) + max(lon_values)) / 2, (min(lat_values) + max(lat_values)) / 2)
    adjustments = {
        "Bogotá D.C.": (0.35, -0.12),
        "Cundinamarca": (-0.25, 0.12),
        "Atlántico": (0.10, 0.08),
        "Quindío": (-0.12, -0.08),
        "Risaralda": (0.12, 0.10),
        "San Andrés y Providencia": (1.8, -2.0),
    }
    for name, (dx, dy) in adjustments.items():
        if name in centers:
            lon, lat = centers[name]
            centers[name] = (lon + dx, lat + dy)
    return centers


def draw_single_map(
    data: pd.DataFrame,
    value_col: str,
    title: str,
    subtitle: str,
    *,
    sequential: bool = True,
    midpoint: float | None = None,
    palette: list[str] | None = None,
    category_colors: dict[str, str] | None = None,
    bubble: bool = False,
    size_col: str = "ocupados_fin",
) -> Image.Image:
    width, height = 1000, 1050
    img = Image.new("RGB", (width, height), "white")
    draw = ImageDraw.Draw(img)
    polygons = read_polygons()
    centers = department_centers(polygons)
    project = map_projection(width, height)
    data_by_name = data.set_index("departamento").to_dict("index")

    draw.text((35, 22), title, fill="#222222", font=font(34, True))
    draw.text((35, 68), subtitle, fill="#555555", font=font(23))

    if category_colors:
        values = pd.Series(dtype=float)
    else:
        values = data[value_col].dropna().astype(float)
    if category_colors:
        lo, hi = 0.0, 1.0
    elif sequential:
        lo, hi = float(values.min()), float(values.max())
        palette = palette or ["#eff6fb", "#bdd7e7", "#6baed6", "#2171b5", "#08306b"]
    else:
        midpoint = float(midpoint or 0)
        limit = float(max(abs(values - midpoint).max(), 1e-9))

    for name, rings in polygons.items():
        row = data_by_name.get(name)
        for ring in rings:
            xy = [project(lon, lat) for lon, lat in ring]
            if row is None:
                fill = (238, 238, 234)
            elif category_colors:
                fill = hex_to_rgb(category_colors.get(str(row[value_col]), "#eeeeea"))
            elif sequential:
                fill = sequential_color(float(row[value_col]), lo, hi, palette or [])
            else:
                fill = diverging_color(float(row[value_col]), midpoint or 0, limit, "#b2182b", "#f7f7f7", "#2166ac")
            draw.polygon(xy, fill=fill, outline=(255, 255, 255))

    if bubble:
        max_size = max(float(row[size_col]) for row in data_by_name.values())
        min_size = min(float(row[size_col]) for row in data_by_name.values())
        for name, row in data_by_name.items():
            if name not in centers:
                continue
            x, y = project(*centers[name])
            denom = max(max_size - min_size, 1.0)
            radius = 9 + 42 * math.sqrt((float(row[size_col]) - min_size) / denom)
            if category_colors:
                fill = hex_to_rgb(category_colors.get(str(row[value_col]), "#9aa7b0"))
            elif sequential:
                fill = sequential_color(float(row[value_col]), lo, hi, palette or [])
            else:
                fill = diverging_color(float(row[value_col]), midpoint or 0, limit, "#b2182b", "#f7f7f7", "#2166ac")
            draw.ellipse((x - radius, y - radius, x + radius, y + radius), fill=fill, outline="white", width=3)

    if category_colors:
        legend_y = height - 92
        items = list(category_colors.items())
        for idx, (label, color) in enumerate(items):
            col = idx % 2
            row = idx // 2
            x = 35 + col * 430
            y = legend_y + row * 28
            draw.rectangle((x, y, x + 22, y + 22), fill=hex_to_rgb(color), outline="#ffffff")
            draw.text((x + 30, y - 2), label, fill="#333333", font=font(16))
        draw.rectangle((35, legend_y + 58, 57, legend_y + 80), fill=(238, 238, 234), outline="#ffffff")
        draw.text((65, legend_y + 56), "Sin información en el panel", fill="#555555", font=font(15))
    else:
        legend_y = height - 62
        bar_x, bar_y, bar_w, bar_h = 35, legend_y + 8, 300, 18
        steps = 80
        for i in range(steps):
            value = lo + (hi - lo) * i / max(steps - 1, 1) if sequential else (midpoint or 0) - limit + (2 * limit) * i / max(steps - 1, 1)
            if sequential:
                color = sequential_color(value, lo, hi, palette or [])
            else:
                color = diverging_color(value, midpoint or 0, limit, "#b2182b", "#f7f7f7", "#2166ac")
            x0 = bar_x + int(i * bar_w / steps)
            x1 = bar_x + int((i + 1) * bar_w / steps)
            draw.rectangle((x0, bar_y, x1, bar_y + bar_h), fill=color)
        if sequential:
            draw.text((bar_x, bar_y + 24), fmt_num_es(lo, 1), fill="#555555", font=font(17))
            draw.text((bar_x + bar_w - 55, bar_y + 24), fmt_num_es(hi, 1), fill="#555555", font=font(17))
        else:
            draw.text((bar_x, bar_y + 24), fmt_num_es((midpoint or 0) - limit, 1), fill="#555555", font=font(17))
            draw.text((bar_x + 127, bar_y + 24), fmt_num_es(midpoint or 0, 1), fill="#555555", font=font(17))
            draw.text((bar_x + bar_w - 55, bar_y + 24), fmt_num_es((midpoint or 0) + limit, 1), fill="#555555", font=font(17))

    return img


def compose_four_panel(panels: list[Image.Image], out_name: str) -> None:
    width, height = 2000, 2100
    canvas = Image.new("RGB", (width, height), "white")
    positions = [(0, 0), (1000, 0), (0, 1050), (1000, 1050)]
    for panel, pos in zip(panels, positions):
        canvas.paste(panel, pos)
    for directory in [FIGURE_DIR, OUTPUT_FIGURE_DIR]:
        canvas.save(directory / out_name)


def compose_two_panel(panels: list[Image.Image], out_name: str) -> None:
    width, height = 2000, 1050
    canvas = Image.new("RGB", (width, height), "white")
    canvas.paste(panels[0], (0, 0))
    canvas.paste(panels[1], (1000, 0))
    for directory in [FIGURE_DIR, OUTPUT_FIGURE_DIR]:
        canvas.save(directory / out_name)


QUADRANT_COLORS = {
    "Nivel alto, crecimiento alto": "#f28e2b",
    "Nivel alto, crecimiento bajo": "#9aa7b0",
    "Nivel bajo, crecimiento alto": "#59a14f",
    "Nivel bajo, crecimiento bajo": "#4e79a7",
}


def classify_quadrants(
    data: pd.DataFrame,
    level_col: str,
    growth_col: str,
    level_ref: float,
    growth_ref: float,
    out_col: str,
) -> pd.DataFrame:
    data[out_col] = np.select(
        [
            (data[level_col] >= level_ref) & (data[growth_col] >= growth_ref),
            (data[level_col] >= level_ref) & (data[growth_col] < growth_ref),
            (data[level_col] < level_ref) & (data[growth_col] >= growth_ref),
        ],
        [
            "Nivel alto, crecimiento alto",
            "Nivel alto, crecimiento bajo",
            "Nivel bajo, crecimiento alto",
        ],
        default="Nivel bajo, crecimiento bajo",
    )
    return data


def draw_remuneration_maps(summary: pd.DataFrame, benchmarks: dict[str, float], suffix: str) -> None:
    data = summary.copy()
    data["rem_trabajador_millones"] = data["rem_trabajador_fin"] / 1e6
    data["rem_hora_miles"] = data["rem_hora_fin"] / 1000
    data["crec_rem_trabajador_pct"] = data["crec_rem_trabajador"] * 100
    data["crec_rem_hora_pct"] = data["crec_rem_hora"] * 100
    data = classify_quadrants(
        data,
        "rem_hora_fin",
        "crec_rem_hora",
        benchmarks["rem_hora_fin"],
        benchmarks["crec_rem_hora"],
        "cuadrante_rem_hora",
    )
    compose_four_panel(
        [
            draw_single_map(data, "rem_trabajador_millones", "Remuneración por trabajador", "Mapa de calor"),
            draw_single_map(data, "rem_trabajador_millones", "Remuneración por trabajador", "Burbuja: ocupados", bubble=True),
            draw_single_map(data, "rem_hora_miles", "Remuneración por hora", "Mapa de calor"),
            draw_single_map(data, "rem_hora_miles", "Remuneración por hora", "Burbuja: ocupados", bubble=True),
        ],
        f"fig_dept_remuneracion_mapa_niveles_{suffix}.png",
    )
    compose_four_panel(
        [
            draw_single_map(data, "crec_rem_trabajador_pct", "Crec. rem. por trabajador", "Escala centrada en el agregado", sequential=False, midpoint=benchmarks["crec_rem_trabajador"] * 100),
            draw_single_map(data, "crec_rem_trabajador_pct", "Crec. rem. por trabajador", "Burbuja: ocupados", sequential=False, midpoint=benchmarks["crec_rem_trabajador"] * 100, bubble=True),
            draw_single_map(data, "crec_rem_hora_pct", "Crec. rem. por hora", "Escala centrada en el agregado", sequential=False, midpoint=benchmarks["crec_rem_hora"] * 100),
            draw_single_map(data, "crec_rem_hora_pct", "Crec. rem. por hora", "Burbuja: ocupados", sequential=False, midpoint=benchmarks["crec_rem_hora"] * 100, bubble=True),
        ],
        f"fig_dept_remuneracion_mapa_crecimientos_{suffix}.png",
    )
    compose_two_panel(
        [
            draw_single_map(data, "cuadrante_rem_hora", "Cuadrantes de remuneración por hora", "Mapa por departamento", category_colors=QUADRANT_COLORS),
            draw_single_map(data, "cuadrante_rem_hora", "Cuadrantes y tamaño del empleo", "Cada burbuja representa un departamento", category_colors=QUADRANT_COLORS, bubble=True),
        ],
        f"fig_dept_remuneracion_mapa_cuadrantes_{suffix}.png",
    )
    draw_quadrant_bubble_chart(
        data,
        level_col="rem_hora_miles",
        growth_col="crec_rem_hora",
        level_ref=benchmarks["rem_hora_fin"] / 1000,
        growth_ref=benchmarks["crec_rem_hora"],
        title="Nivel y crecimiento de la remuneración por hora",
        subtitle="Cada burbuja representa un departamento; el tamaño es proporcional al número de ocupados",
        x_label="Crecimiento anualizado de la remuneración por hora",
        y_label="Remuneración por hora, miles de pesos de 2025",
        out_name=f"fig_dept_remuneracion_cuadrantes_{suffix}.png",
        source="GEIH",
    )


def draw_scatter_panel(
    draw: ImageDraw.ImageDraw,
    box: tuple[int, int, int, int],
    data: pd.DataFrame,
    x_col: str,
    y_col: str,
    x_label: str,
    y_label: str,
    title: str,
    corr: float,
) -> None:
    left, top, right, bottom = box
    plot_left, plot_top = left + 130, top + 120
    plot_right, plot_bottom = right - 55, bottom - 110
    xs = data[x_col].astype(float)
    ys = data[y_col].astype(float)
    x_min, x_max = xs.min(), xs.max()
    y_min, y_max = ys.min(), ys.max()
    x_pad = (x_max - x_min) * 0.08
    y_pad = (y_max - y_min) * 0.12
    x_min, x_max = x_min - x_pad, x_max + x_pad
    y_min, y_max = y_min - y_pad, y_max + y_pad

    def xp(value: float) -> float:
        return plot_left + (value - x_min) / (x_max - x_min) * (plot_right - plot_left)

    def yp(value: float) -> float:
        return plot_bottom - (value - y_min) / (y_max - y_min) * (plot_bottom - plot_top)

    draw.text((left + 20, top + 20), title, fill="#222222", font=font(36, True))
    draw.text((plot_left, plot_top - 38), y_label, fill="#555555", font=font(22))
    draw.rectangle((plot_left, plot_top, plot_right, plot_bottom), outline="#333333", width=2)
    for i in range(6):
        x = plot_left + i / 5 * (plot_right - plot_left)
        y = plot_top + i / 5 * (plot_bottom - plot_top)
        draw.line((x, plot_top, x, plot_bottom), fill="#eeeeee", width=1)
        draw.line((plot_left, y, plot_right, y), fill="#eeeeee", width=1)

    slope, intercept = np.polyfit(xs.to_numpy(), ys.to_numpy(), 1)
    draw.line((xp(x_min), yp(slope * x_min + intercept), xp(x_max), yp(slope * x_max + intercept)), fill="#b44b3f", width=5)
    draw.text((plot_left + 22, plot_top + 18), f"r = {fmt_num_es(corr, 2)}", fill="#b44b3f", font=font(28, True))

    max_occ = data["ocupados_fin"].max()
    min_occ = data["ocupados_fin"].min()
    labels = {
        "Bogotá D.C.": (-135, -42),
        "Antioquia": (-135, -24),
        "Caldas": (-95, -42),
        "Meta": (18, -35),
        "Santander": (18, 8),
        "Valle del Cauca": (18, 18),
        "La Guajira": (18, 4),
        "Cundinamarca": (16, -40),
        "Risaralda": (-120, 14),
        "Norte de Santander": (18, -38),
        "Bolívar": (16, 12),
        "Boyacá": (18, -34),
        "Quindío": (18, 10),
        "Caquetá": (18, 12),
    }
    for _, row in data.iterrows():
        x, y = xp(float(row[x_col])), yp(float(row[y_col]))
        radius = 9 + 32 * math.sqrt((row["ocupados_fin"] - min_occ) / max(max_occ - min_occ, 1))
        draw.ellipse((x - radius, y - radius, x + radius, y + radius), fill="#4e79a7", outline="white", width=3)
        dx, dy = labels.get(row["departamento"], (14, -10))
        draw.text((x + dx, y + dy), row["departamento"], fill="#222222", font=font(20))

    draw.text((plot_left + 220, plot_bottom + 52), x_label, fill="#333333", font=font(25))


def draw_quadrant_bubble_chart(
    summary: pd.DataFrame,
    *,
    level_col: str,
    growth_col: str,
    level_ref: float,
    growth_ref: float,
    title: str,
    subtitle: str,
    x_label: str,
    y_label: str,
    out_name: str,
    source: str = "DANE y GEIH",
) -> None:
    data = summary.copy()
    data["x_value"] = data[growth_col] * 100
    data["y_value"] = data[level_col]
    data = classify_quadrants(
        data,
        "y_value",
        "x_value",
        level_ref,
        growth_ref * 100,
        "cuadrante",
    )

    width, height = 2100, 1680
    img = Image.new("RGB", (width, height), "white")
    draw = ImageDraw.Draw(img)
    draw.text((80, 45), title, fill="#222222", font=font(54, True))
    draw.text((80, 115), subtitle, fill="#555555", font=font(31))

    plot_left, plot_top, plot_right, plot_bottom = 245, 245, 1985, 1195
    x_min = min(data["x_value"].min(), growth_ref * 100)
    x_max = max(data["x_value"].max(), growth_ref * 100)
    y_min = min(data["y_value"].min(), level_ref)
    y_max = max(data["y_value"].max(), level_ref)
    x_pad = max((x_max - x_min) * 0.12, 0.35)
    y_pad = max((y_max - y_min) * 0.12, 0.35)
    x_min, x_max = x_min - x_pad, x_max + x_pad
    y_min, y_max = y_min - y_pad, y_max + y_pad

    def xp(value: float) -> float:
        return plot_left + (value - x_min) / (x_max - x_min) * (plot_right - plot_left)

    def yp(value: float) -> float:
        return plot_bottom - (value - y_min) / (y_max - y_min) * (plot_bottom - plot_top)

    draw.rectangle((plot_left, plot_top, plot_right, plot_bottom), outline="#333333", width=2)
    for i in range(6):
        x = plot_left + i / 5 * (plot_right - plot_left)
        y = plot_top + i / 5 * (plot_bottom - plot_top)
        draw.line((x, plot_top, x, plot_bottom), fill="#eeeeee", width=1)
        draw.line((plot_left, y, plot_right, y), fill="#eeeeee", width=1)
        x_tick = x_min + i / 5 * (x_max - x_min)
        y_tick = y_min + (5 - i) / 5 * (y_max - y_min)
        draw.text((x - 31, plot_bottom + 20), f"{fmt_num_es(x_tick, 1)}%", fill="#555555", font=font(24))
        draw.text((plot_left - 100, y - 15), fmt_num_es(y_tick, 1), fill="#555555", font=font(24))

    x_ref = growth_ref * 100
    y_ref = level_ref
    draw.line((xp(x_ref), plot_top, xp(x_ref), plot_bottom), fill="#1f5aa6", width=4)
    draw.line((plot_left, yp(y_ref), plot_right, yp(y_ref)), fill="#1f5aa6", width=4)
    x_ref_label = max(plot_left + 20, xp(x_ref) - 260)
    draw.text((x_ref_label, plot_top + 92), f"Crec. agregado: {fmt_num_es(x_ref, 1)}%", fill="#1f5aa6", font=font(23, True))
    draw.text((plot_left + 20, yp(y_ref) - 34), f"Nivel agregado: {fmt_num_es(y_ref, 1)}", fill="#1f5aa6", font=font(23, True))

    corner_font = font(28, True)
    draw.text((plot_left + 20, plot_top + 18), "Nivel alto,\ncrecimiento bajo", fill="#1f5aa6", font=corner_font)
    draw.text((plot_right - 390, plot_top + 18), "Nivel alto,\ncrecimiento alto", fill="#1f5aa6", font=corner_font)
    draw.text((plot_left + 20, plot_bottom - 82), "Nivel bajo,\ncrecimiento bajo", fill="#1f5aa6", font=corner_font)
    draw.text((plot_right - 390, plot_bottom - 92), "Nivel bajo,\ncrecimiento alto", fill="#1f5aa6", font=corner_font)

    max_occ = data["ocupados_fin"].max()
    min_occ = data["ocupados_fin"].min()
    label_offsets = {
        "Bogotá D.C.": (-142, -52),
        "Antioquia": (-155, -36),
        "Cundinamarca": (22, -44),
        "Valle del Cauca": (-70, -54),
        "Santander": (22, 16),
        "Meta": (24, -44),
        "Caldas": (52, -76),
        "Risaralda": (-138, 14),
        "Quindío": (24, 24),
        "Chocó": (24, -2),
        "Caquetá": (-126, -44),
        "Nariño": (-112, -32),
        "La Guajira": (24, 8),
        "Bolívar": (24, -26),
        "Boyacá": (28, -24),
        "Norte de Santander": (-150, -48),
        "Cauca": (24, -10),
        "Tolima": (28, 18),
        "Atlántico": (24, 12),
        "Huila": (-86, -10),
        "Cesar": (24, 20),
        "Magdalena": (-140, -10),
        "Córdoba": (24, 26),
        "Sucre": (24, 14),
    }
    display_labels = {
        "Bogotá D.C.": "Bogotá D.C.",
        "Norte de Santander": "N. Santander",
        "Valle del Cauca": "Valle",
    }
    label_font = font(23)

    def draw_label(x_pos: float, y_pos: float, text: str) -> None:
        for ox, oy in [(-2, 0), (2, 0), (0, -2), (0, 2), (-1, -1), (-1, 1), (1, -1), (1, 1)]:
            draw.text((x_pos + ox, y_pos + oy), text, fill="white", font=label_font)
        draw.text((x_pos, y_pos), text, fill="#222222", font=label_font)

    for _, row in data.sort_values("ocupados_fin", ascending=False).iterrows():
        x = xp(float(row["x_value"]))
        y = yp(float(row["y_value"]))
        radius = 10 + 42 * math.sqrt((row["ocupados_fin"] - min_occ) / max(max_occ - min_occ, 1))
        fill = hex_to_rgb(QUADRANT_COLORS[str(row["cuadrante"])])
        draw.ellipse((x - radius, y - radius, x + radius, y + radius), fill=fill, outline="white", width=3)
        dx, dy = label_offsets.get(row["departamento"], (16, -12))
        draw_label(x + dx, y + dy, display_labels.get(row["departamento"], row["departamento"]))

    x_label_font = font(31)
    x_label_width = draw.textlength(x_label, font=x_label_font)
    draw.text((plot_left + (plot_right - plot_left - x_label_width) / 2, plot_bottom + 90), x_label, fill="#333333", font=x_label_font)
    draw.text((plot_left, plot_top - 48), y_label, fill="#333333", font=font(31))

    legend_x, legend_y = 140, 1415
    for idx, (label, color) in enumerate(QUADRANT_COLORS.items()):
        x = legend_x + (idx % 2) * 630
        y = legend_y + (idx // 2) * 46
        draw.rectangle((x, y, x + 28, y + 28), fill=hex_to_rgb(color), outline="#ffffff")
        draw.text((x + 40, y - 2), label, fill="#333333", font=font(25))
    draw.text(
        (90, 1610),
        f"Fuente: cálculos propios con {source}. Líneas azules: agregado de los departamentos comparables; burbuja: número de ocupados.",
        fill="#555555",
        font=font(24),
    )

    for directory in [FIGURE_DIR, OUTPUT_FIGURE_DIR]:
        img.save(directory / out_name)


def draw_relation_scatter(table: pd.DataFrame, benchmarks: dict[str, float], suffix: str) -> None:
    data = table.copy()
    data["pib_trabajador_plot"] = data["pib_trabajador_fin"]
    data["rem_trabajador_plot"] = data["rem_trabajador_fin"] / 1e6
    data["pib_hora_plot"] = data["pib_hora_fin"]
    data["rem_hora_plot"] = data["rem_hora_fin"] / 1000
    img = Image.new("RGB", (2450, 1400), "white")
    draw = ImageDraw.Draw(img)
    draw.text((80, 42), "Productividad y remuneración laboral por departamento", fill="#222222", font=font(44, True))
    draw.text(
        (80, 100),
        "Cada burbuja representa un departamento; el tamaño es proporcional al número de ocupados",
        fill="#555555",
        font=font(28),
    )
    draw_scatter_panel(
        draw,
        (60, 150, 1215, 1300),
        data,
        "pib_trabajador_plot",
        "rem_trabajador_plot",
        "PIB por trabajador, millones de pesos de 2015",
        "Remuneración mensual por trabajador, millones de pesos de 2025",
        "Por trabajador",
        benchmarks["corr_nivel_trabajador"],
    )
    draw_scatter_panel(
        draw,
        (1235, 150, 2390, 1300),
        data,
        "pib_hora_plot",
        "rem_hora_plot",
        "PIB por hora, miles de pesos de 2015",
        "Remuneración por hora, miles de pesos de 2025",
        "Por hora",
        benchmarks["corr_nivel_hora"],
    )
    draw.text((80, 1338), "Fuente: cálculos propios con DANE y GEIH. La línea roja muestra la tendencia lineal simple entre departamentos.", fill="#555555", font=font(23))
    for directory in [FIGURE_DIR, OUTPUT_FIGURE_DIR]:
        img.save(directory / f"fig_dept_productividad_remuneracion_scatter_{suffix}.png")


def draw_productivity_maps(summary: pd.DataFrame, benchmarks: dict[str, float], suffix: str) -> None:
    data = summary.copy()
    data["crec_pib_trabajador_pct"] = data["crec_pib_trabajador"] * 100
    data["crec_pib_hora_pct"] = data["crec_pib_hora"] * 100
    compose_four_panel(
        [
            draw_single_map(data, "pib_trabajador_fin", "PIB por trabajador", "Mapa de calor"),
            draw_single_map(data, "pib_trabajador_fin", "PIB por trabajador", "Burbuja: ocupados", bubble=True),
            draw_single_map(data, "pib_hora_fin", "PIB por hora trabajada", "Mapa de calor"),
            draw_single_map(data, "pib_hora_fin", "PIB por hora trabajada", "Burbuja: ocupados", bubble=True),
        ],
        f"fig_dept_productividad_mapa_niveles_{suffix}.png",
    )
    compose_four_panel(
        [
            draw_single_map(data, "crec_pib_trabajador_pct", "Crec. PIB por trabajador", "Escala centrada en el agregado", sequential=False, midpoint=benchmarks["crec_pib_trabajador"] * 100),
            draw_single_map(data, "crec_pib_trabajador_pct", "Crec. PIB por trabajador", "Burbuja: ocupados", sequential=False, midpoint=benchmarks["crec_pib_trabajador"] * 100, bubble=True),
            draw_single_map(data, "crec_pib_hora_pct", "Crec. PIB por hora", "Escala centrada en el agregado", sequential=False, midpoint=benchmarks["crec_pib_hora"] * 100),
            draw_single_map(data, "crec_pib_hora_pct", "Crec. PIB por hora", "Burbuja: ocupados", sequential=False, midpoint=benchmarks["crec_pib_hora"] * 100, bubble=True),
        ],
        f"fig_dept_productividad_mapa_crecimientos_{suffix}.png",
    )
    draw_quadrant_bubble_chart(
        data,
        level_col="pib_hora_fin",
        growth_col="crec_pib_hora",
        level_ref=benchmarks["pib_hora_fin"],
        growth_ref=benchmarks["crec_pib_hora"],
        title="Nivel y crecimiento del PIB por hora trabajada",
        subtitle="Cada burbuja representa un departamento; el tamaño es proporcional al número de ocupados",
        x_label="Crecimiento anualizado del PIB por hora trabajada",
        y_label="PIB por hora, miles de pesos de 2015",
        out_name=f"fig_dept_productividad_cuadrantes_{suffix}.png",
    )


def write_remuneration_body(
    summary24: pd.DataFrame,
    bench24: dict[str, float],
    summary33: pd.DataFrame,
    bench33: dict[str, float],
    convergence24: pd.DataFrame,
) -> None:
    leader = summary24.sort_values("ranking_rem_trabajador").iloc[0]
    low = summary24.sort_values("ranking_rem_trabajador").tail(1).iloc[0]
    fastest = summary24.sort_values("ranking_crec_rem_trabajador").iloc[0]
    slowest = summary24.sort_values("ranking_crec_rem_trabajador").tail(1).iloc[0]
    conv_worker = convergence24[convergence24["indicador"] == "Remuneración por trabajador"].iloc[0]
    conv_hour = convergence24[convergence24["indicador"] == "Remuneración por hora"].iloc[0]
    lines = [
        r"\section{Introducción}",
        "",
        r"\textbf{La remuneración laboral también tiene una geografía.} Las diferencias de ingreso entre trabajadores no dependen únicamente de su educación, ocupación o actividad económica. También dependen del territorio donde trabajan, de la estructura productiva local, de la informalidad, de la conectividad con mercados y de la densidad empresarial de cada departamento.",
        "",
        rf"\textbf{{Este informe estudia la remuneración laboral departamental entre {PANEL_24_START} y {REMUNERATION_END}.}} El ejercicio se concentra primero en los 24 departamentos que pueden seguirse de manera comparable en la GEIH desde 2010. Luego presenta una lectura complementaria para los 33 departamentos, disponible desde 2014. La pregunta central es sencilla: en qué departamentos se remunera más el trabajo y en cuáles ha crecido más la remuneración real.",
        "",
        r"\textbf{La lectura territorial de la remuneración es importante para la agenda de productividad.} Un aumento de la productividad solo mejora de manera amplia el bienestar si se traduce, al menos parcialmente, en mejores ingresos laborales. Antes de estudiar esa relación de manera directa, conviene entender primero la geografía propia de la remuneración.",
        "",
        r"\section{Metodología}",
        "",
        r"\textbf{La medición utiliza los microdatos armonizados de la GEIH.} Para cada departamento y año se calcula la remuneración laboral mensual total a partir del ingreso laboral por hora, las horas semanales trabajadas y el factor de expansión de la encuesta. La remuneración se expresa en pesos constantes de 2025.",
        "",
        r"\textbf{Se construyen dos indicadores de remuneración laboral.} La remuneración por trabajador divide la remuneración laboral mensual total observada entre ocupados con ingreso horario positivo y horas válidas entre el número de ocupados de ese mismo universo. La remuneración por hora divide esa misma remuneración total entre las horas mensuales trabajadas por esos ocupados. Esta definición evita que los trabajadores sin ingreso u horas válidas sean tratados implícitamente como trabajadores con remuneración cero.",
        "",
        r"\textbf{El informe trabaja con dos paneles departamentales.} El panel principal cubre los 24 departamentos comparables de la GEIH entre 2010 y 2025. Aunque la base permite observar esos departamentos desde 2009, el análisis principal empieza en 2010 para mantener consistencia con los demás informes de la serie. San Andrés aparece de manera separada desde 2010 y los departamentos de la Amazonía y la Orinoquía aparecen desde 2014. Por eso, el informe también presenta una lectura complementaria para los 33 departamentos desde 2014. En ambos casos se excluye 2020.",
        "",
        r"\section{Niveles de remuneración por departamento}",
        "",
        rf"\textbf{{El ranking de niveles confirma la centralidad de {escape_latex(leader['departamento'])}, pero también muestra brechas amplias entre departamentos.}} El Cuadro \ref{{tab:dept_remuneracion_niveles_24}} ordena los 24 departamentos comparables por remuneración mensual por trabajador en {REMUNERATION_END}. {escape_latex(leader['departamento'])} ocupa el primer lugar, con {fmt_num_es(leader['rem_trabajador_fin'] / 1e6, 2)} millones de pesos de 2025 al mes por trabajador. En el extremo inferior aparece {escape_latex(low['departamento'])}, con {fmt_num_es(low['rem_trabajador_fin'] / 1e6, 2)} millones.",
        "",
        r"\input{Paper/sections/dept_remuneracion_niveles_24}",
        "",
        r"\begin{figure}[H]",
        r"  \centering",
        r"  \includegraphics[width=\textwidth]{Paper/figures/fig_dept_remuneracion_mapa_niveles_24.png}",
        rf"  \caption{{Mapa de niveles de remuneración laboral departamental, {REMUNERATION_END}}}",
        r"  \label{fig:dept_remuneracion_mapa_niveles_24}",
        r"  \caption*{\footnotesize Nota: los paneles superiores muestran la remuneración por trabajador; los inferiores, la remuneración por hora. En los paneles de la derecha, el tamaño de la burbuja es proporcional al número de ocupados. Fuente: cálculos propios con GEIH.}",
        r"\end{figure}",
        "",
        r"\textbf{Los mapas muestran que la geografía de la remuneración tiene un patrón territorial claro.} Los mayores niveles se concentran en Bogotá D.C. y en algunos departamentos del centro del país. En contraste, varios departamentos de la periferia norte, pacífica y sur registran remuneraciones menores. La lectura por hora confirma que estas diferencias no se explican únicamente por el número de horas trabajadas.",
        "",
        r"\section{Crecimiento y cuadrantes de remuneración}",
        "",
        rf"\textbf{{El crecimiento de la remuneración también fue desigual.}} Entre {PANEL_24_START} y {REMUNERATION_END}, {escape_latex(fastest['departamento'])} tuvo el mayor crecimiento anual de la remuneración por trabajador ({fmt_pct_es(fastest['crec_rem_trabajador'], 1)}). En el otro extremo, {escape_latex(slowest['departamento'])} registró el desempeño más débil ({fmt_pct_es(slowest['crec_rem_trabajador'], 1)}).",
        "",
        r"\input{Paper/sections/dept_remuneracion_crecimientos_24}",
        "",
        r"\begin{figure}[H]",
        r"  \centering",
        r"  \includegraphics[width=\textwidth]{Paper/figures/fig_dept_remuneracion_mapa_crecimientos_24.png}",
        rf"  \caption{{Mapa de crecimiento de la remuneración laboral departamental, {PANEL_24_START}--{REMUNERATION_END}}}",
        r"  \label{fig:dept_remuneracion_mapa_crecimientos_24}",
        r"  \caption*{\footnotesize Nota: las escalas de color se centran en el crecimiento agregado del panel de 24 departamentos. Fuente: cálculos propios con GEIH.}",
        r"\end{figure}",
        "",
        r"\textbf{La clasificación por cuadrantes separa nivel y dinamismo.} Un departamento puede tener una remuneración alta pero crecer por debajo del agregado; también puede partir de niveles bajos y crecer rápido. La Figura \ref{fig:dept_remuneracion_cuadrantes_24} cruza el nivel de remuneración por hora con su crecimiento anualizado. Los cuatro cuadrantes se nombran de forma descriptiva: nivel alto y crecimiento alto; nivel alto y crecimiento bajo; nivel bajo y crecimiento alto; y nivel bajo y crecimiento bajo.",
        "",
        r"\begin{figure}[H]",
        r"  \centering",
        r"  \includegraphics[width=\textwidth]{Paper/figures/fig_dept_remuneracion_cuadrantes_24.png}",
        rf"  \caption{{Nivel y crecimiento de la remuneración por hora, {PANEL_24_START}--{REMUNERATION_END}}}",
        r"  \label{fig:dept_remuneracion_cuadrantes_24}",
        r"  \caption*{\footnotesize Nota: la línea vertical muestra el crecimiento agregado de la remuneración por hora; la línea horizontal muestra el nivel agregado de remuneración por hora. El tamaño de la burbuja es proporcional al número de ocupados. Fuente: cálculos propios con GEIH.}",
        r"\end{figure}",
        "",
        r"\textbf{El mapa permite ver la misma clasificación en el territorio.} La Figura \ref{fig:dept_remuneracion_mapa_cuadrantes_24} muestra dónde se ubican los departamentos de cada cuadrante y complementa la lectura de la dispersión.",
        "",
        r"\begin{figure}[H]",
        r"  \centering",
        r"  \includegraphics[width=\textwidth]{Paper/figures/fig_dept_remuneracion_mapa_cuadrantes_24.png}",
        rf"  \caption{{Mapa de cuadrantes de remuneración por hora, {PANEL_24_START}--{REMUNERATION_END}}}",
        r"  \label{fig:dept_remuneracion_mapa_cuadrantes_24}",
        rf"  \caption*{{\footnotesize Nota: los cuadrantes se construyen con el nivel de remuneración por hora en 2025 y su crecimiento anualizado desde {PANEL_24_START}. Fuente: cálculos propios con GEIH.}}",
        r"\end{figure}",
        "",
        r"\section{Convergencia de la remuneración departamental}",
        "",
        rf"\textbf{{La pregunta de convergencia es si los departamentos inicialmente rezagados crecieron más rápido que los departamentos con mayor remuneración inicial.}} Para responderla se estima una regresión de convergencia beta, en la que el crecimiento anualizado de la remuneración entre {PANEL_24_START} y {REMUNERATION_END} se relaciona con el logaritmo del nivel inicial de remuneración en {PANEL_24_START}. Un coeficiente negativo indicaría convergencia; un coeficiente positivo indicaría que los departamentos con mayor remuneración inicial crecieron más rápido.",
        "",
        r"\input{Paper/sections/dept_remuneracion_convergencia_24}",
        "",
        rf"\textbf{{Los resultados no muestran convergencia en el panel largo de 24 departamentos.}} El coeficiente beta es positivo tanto para la remuneración por trabajador ({fmt_num_es(conv_worker['beta'], 4)}) como para la remuneración por hora ({fmt_num_es(conv_hour['beta'], 4)}). Esto significa que, en promedio, los departamentos que partían de mayores niveles de remuneración en {PANEL_24_START} no crecieron menos que los rezagados. Por el contrario, la pendiente estimada apunta a persistencia o divergencia débil de las brechas territoriales.",
        "",
        r"\begin{figure}[H]",
        r"  \centering",
        r"  \includegraphics[width=\textwidth]{Paper/figures/fig_dept_remuneracion_convergencia_24.png}",
        rf"  \caption{{Nivel inicial y crecimiento posterior de la remuneración departamental, {PANEL_24_START}--{REMUNERATION_END}}}",
        r"  \label{fig:dept_remuneracion_convergencia_24}",
        r"  \caption*{\footnotesize Nota: cada punto representa un departamento del panel comparable de 24 departamentos. La pendiente negativa indicaría convergencia beta. Fuente: cálculos propios con GEIH.}",
        r"\end{figure}",
        "",
        r"\textbf{La implicación es importante: las brechas territoriales de remuneración no se cerraron automáticamente durante el periodo.} Aunque algunos departamentos de menor remuneración inicial crecieron por encima del promedio, el patrón conjunto no es suficientemente fuerte para hablar de convergencia. Esto refuerza la idea de que las diferencias territoriales requieren una agenda explícita y no pueden dejarse solo a la dinámica agregada del mercado laboral.",
        "",
        r"\section{Lectura complementaria con 33 departamentos}",
        "",
        rf"\textbf{{La GEIH permite observar los 33 departamentos desde 2014.}} Esta apertura no reemplaza el panel largo de 24 departamentos, pero permite incluir a San Andrés y Providencia y a los departamentos de la Amazonía y la Orinoquía. En este panel ampliado, la comparación cubre {PANEL_33_START}--{REMUNERATION_END} y también excluye 2020.",
        "",
        r"\input{Paper/sections/dept_remuneracion_niveles_33}",
        "",
        r"\begin{figure}[H]",
        r"  \centering",
        r"  \includegraphics[width=\textwidth]{Paper/figures/fig_dept_remuneracion_mapa_cuadrantes_33.png}",
        rf"  \caption{{Mapa de cuadrantes de remuneración por hora, 33 departamentos, {PANEL_33_START}--{REMUNERATION_END}}}",
        r"  \label{fig:dept_remuneracion_mapa_cuadrantes_33}",
        r"  \caption*{\footnotesize Nota: la figura usa el panel ampliado de 33 departamentos disponible desde 2014. Fuente: cálculos propios con GEIH.}",
        r"\end{figure}",
        "",
        r"\section{Conclusiones}",
        "",
        r"\textbf{La remuneración laboral en Colombia presenta brechas territoriales profundas.} Incluso al limitar el análisis al panel comparable de 24 departamentos, la diferencia entre los departamentos de mayor y menor remuneración es amplia. Esta brecha no desaparece cuando se mira la remuneración por hora trabajada.",
        "",
        r"\textbf{El crecimiento de la remuneración no ha sido homogéneo.} Algunos departamentos lograron aumentos importantes de remuneración real, mientras otros se rezagaron. Esto implica que las brechas territoriales no dependen solo de los niveles iniciales, sino también de trayectorias recientes muy distintas.",
        "",
        r"\textbf{La geografía de la remuneración debe ser parte de la agenda económica nacional.} Mejorar los ingresos laborales no depende únicamente de reglas salariales nacionales. También requiere mejorar las condiciones que permiten a los territorios conectar trabajadores y empresas con actividades de mayor valor agregado.",
    ]
    (SECTION_DIR / "CuerpoInformeRemuneracionDepartamentos.tex").write_text("\n".join(lines), encoding="utf-8")


def write_productivity_relation_body(
    prod24: pd.DataFrame,
    prod_bench24: dict[str, float],
    relation24: pd.DataFrame,
    relation_bench24: dict[str, float],
    prod33: pd.DataFrame,
    relation33: pd.DataFrame,
) -> None:
    top_growth = prod24.sort_values("crec_pib_hora", ascending=False).iloc[0]
    low_level = prod24.sort_values("pib_hora_fin").iloc[0]
    high_resid = relation24.sort_values("residuo_pct_hora", ascending=False).iloc[0]
    low_resid = relation24.sort_values("residuo_pct_hora").iloc[0]
    lines = [
        r"\section{Introducción}",
        "",
        r"\textbf{La productividad laboral departamental importa, pero no debe leerse aislada de la remuneración.} Un departamento puede producir mucho por trabajador y, aun así, no traducir esa productividad en remuneraciones proporcionalmente altas. También puede ocurrir lo contrario: algunos territorios pueden registrar remuneraciones relativamente altas para su nivel de productividad.",
        "",
        rf"\textbf{{Este informe estudia la productividad laboral departamental y su relación con la remuneración.}} La productividad se mide con el PIB departamental por trabajador y por hora trabajada entre {PANEL_24_START} y {PRODUCTIVITY_END}pr. La relación con la remuneración se analiza usando la remuneración por trabajador y por hora de la GEIH. Como el PIB departamental más reciente llega a {PRODUCTIVITY_END}pr, este informe cierra en ese año.",
        "",
        r"\textbf{La lectura principal se hace con los 24 departamentos comparables desde 2010.} Al final se presenta una lectura complementaria con los 33 departamentos disponibles desde 2014. Esta separación evita imputar ocupados u horas en departamentos que no estaban cubiertos por la GEIH al inicio del periodo largo.",
        "",
        r"\section{Metodología}",
        "",
        r"\textbf{La medición combina cuentas nacionales departamentales y microdatos de la GEIH.} El numerador de productividad corresponde al PIB departamental real del DANE, expresado en pesos constantes de 2015. Los denominadores se construyen con el número anual promedio de ocupados y el total anual de horas trabajadas de la GEIH.",
        "",
        r"\textbf{Se comparan indicadores por trabajador y por hora.} El PIB por trabajador usa como denominador el número anual promedio de ocupados, mientras que la remuneración por trabajador se calcula entre ocupados con ingreso horario positivo y horas válidas. El PIB por hora usa el total de horas trabajadas del departamento, mientras que la remuneración por hora usa las horas observadas de ese mismo universo con remuneración válida. Esta doble lectura es importante porque los departamentos pueden diferir tanto en productividad como en intensidad horaria.",
        "",
        r"\section{Productividad laboral departamental}",
        "",
        rf"\textbf{{La productividad laboral creció a ritmos muy distintos entre departamentos.}} El Cuadro \ref{{tab:dept_productividad_resumen_24}} ordena los departamentos por crecimiento del PIB por hora trabajada. {escape_latex(top_growth['departamento'])} registra el mayor crecimiento del panel ({fmt_pct_es(top_growth['crec_pib_hora'], 1)} anual), mientras que {escape_latex(low_level['departamento'])} se ubica entre los menores niveles de productividad por hora en {PRODUCTIVITY_END}pr.",
        "",
        r"\input{Paper/sections/dept_productividad_resumen_24}",
        "",
        r"\begin{figure}[H]",
        r"  \centering",
        r"  \includegraphics[width=\textwidth]{Paper/figures/fig_dept_productividad_mapa_niveles_24.png}",
        rf"  \caption{{Mapa de niveles de productividad laboral departamental, {PRODUCTIVITY_END}pr}}",
        r"  \label{fig:dept_productividad_mapa_niveles_24}",
        r"  \caption*{\footnotesize Nota: los paneles superiores muestran PIB por trabajador; los inferiores, PIB por hora. En los paneles de la derecha, el tamaño de la burbuja es proporcional al número de ocupados. Fuente: cálculos propios con DANE y GEIH.}",
        r"\end{figure}",
        "",
        r"\begin{figure}[H]",
        r"  \centering",
        r"  \includegraphics[width=\textwidth]{Paper/figures/fig_dept_productividad_mapa_crecimientos_24.png}",
        rf"  \caption{{Mapa de crecimiento de la productividad laboral departamental, {PANEL_24_START}--{PRODUCTIVITY_END}pr}}",
        r"  \label{fig:dept_productividad_mapa_crecimientos_24}",
        r"  \caption*{\footnotesize Nota: las escalas de color se centran en el crecimiento agregado del panel de 24 departamentos. Fuente: cálculos propios con DANE y GEIH.}",
        r"\end{figure}",
        "",
        r"\textbf{También es útil separar el nivel de productividad de su crecimiento.} La Figura \ref{fig:dept_productividad_cuadrantes_24} cruza el nivel de PIB por hora trabajada en 2024pr con su crecimiento anualizado. La línea vertical marca el crecimiento agregado y la línea horizontal marca el nivel agregado. Así se distinguen cuatro situaciones sin convertir la clasificación en un juicio de desempeño: nivel alto y crecimiento alto; nivel alto y crecimiento bajo; nivel bajo y crecimiento alto; y nivel bajo y crecimiento bajo.",
        "",
        r"\begin{figure}[H]",
        r"  \centering",
        r"  \includegraphics[width=\textwidth]{Paper/figures/fig_dept_productividad_cuadrantes_24.png}",
        rf"  \caption{{Nivel y crecimiento del PIB por hora trabajada, {PANEL_24_START}--{PRODUCTIVITY_END}pr}}",
        r"  \label{fig:dept_productividad_cuadrantes_24}",
        r"  \caption*{\footnotesize Nota: la línea vertical muestra el crecimiento agregado del PIB por hora; la línea horizontal muestra el nivel agregado de PIB por hora. El tamaño de la burbuja es proporcional al número de ocupados. Fuente: cálculos propios con DANE y GEIH.}",
        r"\end{figure}",
        "",
        r"\section{Productividad y remuneración}",
        "",
        rf"\textbf{{Los departamentos más productivos tienden a tener mayores remuneraciones, pero la relación no es perfecta.}} La correlación entre PIB por trabajador y remuneración por trabajador es {fmt_num_es(relation_bench24['corr_nivel_trabajador'], 2)}; entre PIB por hora y remuneración por hora es {fmt_num_es(relation_bench24['corr_nivel_hora'], 2)}. La Figura \ref{{fig:dept_productividad_remuneracion_scatter_24}} muestra ambas relaciones.",
        "",
        r"\begin{figure}[H]",
        r"  \centering",
        r"  \includegraphics[width=\textwidth]{Paper/figures/fig_dept_productividad_remuneracion_scatter_24.png}",
        rf"  \caption{{Productividad y remuneración laboral por departamento, {PRODUCTIVITY_END}pr}}",
        r"  \label{fig:dept_productividad_remuneracion_scatter_24}",
        r"\end{figure}",
        "",
        rf"\textbf{{Los departamentos más alejados de la tendencia son especialmente informativos.}} En la comparación por hora, {escape_latex(high_resid['departamento'])} aparece por encima de la relación promedio: su remuneración por hora es mayor a la que sugeriría su nivel de PIB por hora. En cambio, {escape_latex(low_resid['departamento'])} aparece por debajo de la tendencia. Estos casos no implican que unos departamentos paguen demasiado y otros muy poco; muestran que la conexión entre productividad y remuneración pasa por la estructura productiva y laboral de cada territorio.",
        "",
        r"\input{Paper/sections/dept_productividad_remuneracion_24}",
        "",
        r"\section{Lectura complementaria con 33 departamentos}",
        "",
        rf"\textbf{{La apertura de 33 departamentos puede hacerse desde 2014.}} Esta lectura permite incluir a San Andrés y Providencia y a los departamentos de la Amazonía y la Orinoquía, pero cubre un periodo más corto: {PANEL_33_START}--{PRODUCTIVITY_END}pr. Por eso debe leerse como complemento del panel largo de 24 departamentos.",
        "",
        r"\input{Paper/sections/dept_productividad_resumen_33}",
        "",
        r"\section{Conclusiones}",
        "",
        r"\textbf{La productividad laboral colombiana tiene una dimensión territorial fuerte.} Las diferencias entre departamentos son amplias tanto en niveles como en crecimientos. El promedio nacional oculta trayectorias territoriales muy distintas.",
        "",
        r"\textbf{Productividad y remuneración están asociadas, pero no se mueven de manera idéntica.} La relación es clara en niveles, pero hay departamentos que se apartan de la tendencia promedio. Esos casos deben estudiarse con más detalle, porque pueden reflejar diferencias en composición sectorial, informalidad, capital humano, estructura empresarial y condiciones locales del mercado laboral.",
        "",
        r"\textbf{La agenda de productividad necesita una lectura territorial explícita.} La política pública debe preguntarse no solo dónde se produce más, sino también dónde esa productividad se transforma en mejores remuneraciones para los trabajadores.",
    ]
    (SECTION_DIR / "CuerpoInformeProductividadRemuneracionDepartamentos.tex").write_text("\n".join(lines), encoding="utf-8")


def write_main_tex_files() -> None:
    informe_02 = r"""\documentclass[12pt, a4paper]{article}

\input{Paper/config_informes_departamentales}

\renewcommand{\reportnumber}{Informe 02}
\renewcommand{\reporttitle}{La Geografía de la Remuneración Laboral en Colombia}
\renewcommand{\reportsubtitle}{Diferencias departamentales, 2010--2025}

\title{\reporttitle\\\reportsubtitle}
\author{\reportauthorone \and \reportauthortwo}
\date{\today}

\begin{document}

\input{Paper/sections/PortadaInformesDepartamentales}

\begin{mdframed}[
  linewidth=1pt,
  linecolor=gray!60,
  backgroundcolor=gray!6,
  innertopmargin=12pt,
  innerbottommargin=12pt,
  innerleftmargin=14pt,
  innerrightmargin=14pt
]
\section*{Resumen ejecutivo}
\addcontentsline{toc}{section}{Resumen ejecutivo}

\textbf{Este informe analiza la remuneración laboral por trabajador y por hora en los departamentos de Colombia.} El ejercicio usa la GEIH para construir indicadores de remuneración real en pesos constantes de 2025. El panel principal cubre 24 departamentos entre 2010 y 2025; una lectura complementaria cubre los 33 departamentos desde 2014.

\textbf{La remuneración laboral presenta brechas territoriales profundas.} Bogotá D.C. ocupa una posición dominante en niveles, mientras varios departamentos de la periferia registran remuneraciones mucho menores. La brecha aparece tanto por trabajador como por hora trabajada.

\textbf{El crecimiento de la remuneración también fue desigual.} Algunos departamentos lograron aumentos importantes de remuneración real, mientras otros se rezagaron. Por eso, la discusión no debe limitarse al promedio nacional.

\textbf{Los mapas muestran una dimensión territorial marcada.} La remuneración alta se concentra en pocos departamentos, mientras varios rezagos se ubican en la periferia norte, pacífica y sur del país. Esta geografía sugiere que los ingresos laborales dependen también de condiciones territoriales como conectividad, densidad empresarial, composición productiva e infraestructura.
\end{mdframed}

\newpage
\tableofcontents
\newpage

\input{Paper/sections/CuerpoInformeRemuneracionDepartamentos}

\end{document}
"""
    informe_03 = r"""\documentclass[12pt, a4paper]{article}

\input{Paper/config_informes_departamentales}

\renewcommand{\reportnumber}{Informe 03}
\renewcommand{\reporttitle}{Productividad y Remuneración Laboral en los Departamentos de Colombia}
\renewcommand{\reportsubtitle}{Una lectura conjunta, 2010--2024pr}

\title{\reporttitle\\\reportsubtitle}
\author{\reportauthorone \and \reportauthortwo}
\date{\today}

\begin{document}

\input{Paper/sections/PortadaInformesDepartamentales}

\begin{mdframed}[
  linewidth=1pt,
  linecolor=gray!60,
  backgroundcolor=gray!6,
  innertopmargin=12pt,
  innerbottommargin=12pt,
  innerleftmargin=14pt,
  innerrightmargin=14pt
]
\section*{Resumen ejecutivo}
\addcontentsline{toc}{section}{Resumen ejecutivo}

\textbf{Este informe analiza la productividad laboral departamental y su relación con la remuneración.} La productividad se mide con PIB por trabajador y PIB por hora, usando cuentas departamentales del DANE y microdatos de la GEIH. Como el PIB departamental más reciente llega a 2024pr, el análisis de productividad se cierra en ese año.

\textbf{La productividad laboral presenta una geografía desigual.} Algunos departamentos tienen niveles altos de PIB por hora, mientras otros mantienen rezagos persistentes. Además, los departamentos donde más creció la productividad no son necesariamente los más productivos en niveles.

\textbf{Productividad y remuneración están asociadas, pero no son lo mismo.} Los departamentos más productivos tienden a remunerar mejor el trabajo, pero la relación no es perfecta. Algunos departamentos registran remuneraciones superiores a las que sugiere su nivel de productividad, mientras otros se ubican por debajo de la tendencia.

\textbf{La agenda territorial debe mirar simultáneamente productividad y remuneración.} El reto no es solo producir más por trabajador o por hora, sino entender bajo qué condiciones esa productividad se convierte en mejores ingresos laborales.
\end{mdframed}

\newpage
\tableofcontents
\newpage

\input{Paper/sections/CuerpoInformeProductividadRemuneracionDepartamentos}

\end{document}
"""
    # The first two paths keep compatibility with earlier Overleaf project names.
    (PROJECT_ROOT / "Paper" / "informe_02_remuneracion_departamentos.tex").write_text(informe_02, encoding="utf-8")
    (PROJECT_ROOT / "Paper" / "informe_02_productividad_departamentos.tex").write_text(informe_02, encoding="utf-8")
    (PROJECT_ROOT / "Paper" / "informe_03_productividad_remuneracion_departamentos.tex").write_text(informe_03, encoding="utf-8")
    (PROJECT_ROOT / "Paper" / "informe_04_productividad_remuneracion_departamentos.tex").write_text(informe_03, encoding="utf-8")


def main() -> None:
    geih = load_geih_base()

    rem_series24, rem_summary24, rem_bench24 = build_remuneration_panel(
        geih, DEPARTMENTS_24, PANEL_24_START, REMUNERATION_END
    )
    rem_series33, rem_summary33, rem_bench33 = build_remuneration_panel(
        geih, DEPARTMENTS_33, PANEL_33_START, REMUNERATION_END
    )
    prod_series24, prod_summary24, prod_bench24 = build_productivity_panel(
        geih, DEPARTMENTS_24, PANEL_24_START, PRODUCTIVITY_END
    )
    prod_series33, prod_summary33, prod_bench33 = build_productivity_panel(
        geih, DEPARTMENTS_33, PANEL_33_START, PRODUCTIVITY_END
    )
    rem_series24_to_2024, _, _ = build_remuneration_panel(
        geih, DEPARTMENTS_24, PANEL_24_START, PRODUCTIVITY_END
    )
    rem_series33_to_2024, _, _ = build_remuneration_panel(
        geih, DEPARTMENTS_33, PANEL_33_START, PRODUCTIVITY_END
    )
    relation24, relation_bench24 = build_relation_table(
        prod_summary24, rem_series24_to_2024, PANEL_24_START, PRODUCTIVITY_END
    )
    relation33, relation_bench33 = build_relation_table(
        prod_summary33, rem_series33_to_2024, PANEL_33_START, PRODUCTIVITY_END
    )
    convergence24, convergence_points24 = build_remuneration_convergence(
        rem_summary24, PANEL_24_START, REMUNERATION_END
    )

    write_csv(rem_series24, "dept_remuneracion_series_24_2010_2025.csv")
    write_csv(rem_summary24, "dept_remuneracion_summary_24_2010_2025.csv")
    write_csv(rem_series33, "dept_remuneracion_series_33_2014_2025.csv")
    write_csv(rem_summary33, "dept_remuneracion_summary_33_2014_2025.csv")
    write_csv(prod_series24, "dept_productividad_series_24_2010_2024.csv")
    write_csv(prod_summary24, "dept_productividad_summary_24_2010_2024.csv")
    write_csv(prod_series33, "dept_productividad_series_33_2014_2024.csv")
    write_csv(prod_summary33, "dept_productividad_summary_33_2014_2024.csv")
    write_csv(relation24, "dept_productividad_remuneracion_24_2010_2024.csv")
    write_csv(relation33, "dept_productividad_remuneracion_33_2014_2024.csv")
    write_csv(convergence24, "dept_remuneracion_convergencia_24_2010_2025.csv")
    write_csv(convergence_points24, "dept_remuneracion_convergencia_puntos_24_2010_2025.csv")
    write_benchmarks(rem_bench24, "dept_remuneracion_benchmarks_24_2010_2025.csv")
    write_benchmarks(rem_bench33, "dept_remuneracion_benchmarks_33_2014_2025.csv")
    write_benchmarks(prod_bench24, "dept_productividad_benchmarks_24_2010_2024.csv")
    write_benchmarks(prod_bench33, "dept_productividad_benchmarks_33_2014_2024.csv")
    write_benchmarks(relation_bench24, "dept_productividad_remuneracion_benchmarks_24_2010_2024.csv")
    write_benchmarks(relation_bench33, "dept_productividad_remuneracion_benchmarks_33_2014_2024.csv")

    write_remuneration_level_table(rem_summary24, REMUNERATION_END, "24")
    write_remuneration_growth_table(rem_summary24, PANEL_24_START, REMUNERATION_END, "24")
    write_remuneration_level_table(rem_summary33, REMUNERATION_END, "33")
    write_remuneration_growth_table(rem_summary33, PANEL_33_START, REMUNERATION_END, "33")
    write_remuneration_convergence_table(convergence24, PANEL_24_START, REMUNERATION_END)
    write_productivity_summary_table(prod_summary24, PANEL_24_START, PRODUCTIVITY_END, "24")
    write_productivity_summary_table(prod_summary33, PANEL_33_START, PRODUCTIVITY_END, "33")
    write_relation_table(relation24, PRODUCTIVITY_END, "24")
    write_relation_table(relation33, PRODUCTIVITY_END, "33")

    draw_remuneration_maps(rem_summary24, rem_bench24, "24")
    draw_remuneration_maps(rem_summary33, rem_bench33, "33")
    draw_remuneration_convergence(convergence_points24, convergence24)
    draw_productivity_maps(prod_summary24, prod_bench24, "24")
    draw_productivity_maps(prod_summary33, prod_bench33, "33")
    draw_relation_scatter(relation24, relation_bench24, "24")
    draw_relation_scatter(relation33, relation_bench33, "33")

    write_remuneration_body(rem_summary24, rem_bench24, rem_summary33, rem_bench33, convergence24)
    write_productivity_relation_body(
        prod_summary24, prod_bench24, relation24, relation_bench24, prod_summary33, relation33
    )
    write_main_tex_files()

    print("Cobertura confirmada:")
    print("24 departamentos: remuneración 2010-2025; productividad 2010-2024pr.")
    print("33 departamentos: remuneración 2014-2025; productividad 2014-2024pr.")
    print("Panel 24:", ", ".join(DEPARTMENTS_24.values()))
    print("Panel 33 incluye además:", ", ".join(DEPARTMENTS_33[k] for k in sorted(set(DEPARTMENTS_33) - set(DEPARTMENTS_24))))


if __name__ == "__main__":
    main()
