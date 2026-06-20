from __future__ import annotations

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
GEIH_DTA = PROJECT_ROOT / "Datos" / "Processed" / "Paper-GEIH_base_modelo_personas_2008_2025.dta"

TABLE_DIR = PROJECT_ROOT / "Paper" / "tables"
SECTION_DIR = PROJECT_ROOT / "Paper" / "sections"
FIGURE_DIR = PROJECT_ROOT / "Paper" / "figures"
OUTPUT_TABLE_DIR = PROJECT_ROOT / "Outputs" / "tables"
OUTPUT_FIGURE_DIR = PROJECT_ROOT / "Outputs" / "Figures"

START_YEAR = 2010
END_YEAR = 2024
EXCLUDED_YEARS = {2020}
MONTHS_PER_WEEK = 52.0 / 12.0

for directory in [TABLE_DIR, SECTION_DIR, FIGURE_DIR, OUTPUT_TABLE_DIR, OUTPUT_FIGURE_DIR]:
    directory.mkdir(parents=True, exist_ok=True)


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


def normalize_name(value: object) -> str:
    text = str(value).split(":")[0].strip().upper()
    text = "".join(ch for ch in unicodedata.normalize("NFKD", text) if not unicodedata.combining(ch))
    text = re.sub(r"[^A-Z0-9 ]+", "", text)
    return re.sub(r"\s+", " ", text).strip()


DEPARTMENT_CODE_BY_NAME = {normalize_name(name): code for code, name in DEPARTMENTS_24.items()}


def parse_year(value: object) -> int | None:
    if pd.isna(value):
        return None
    text = str(value).strip().lower().replace("pr", "").replace("p", "")
    try:
        return int(float(text))
    except ValueError:
        return None


def cagr(start_value: float, end_value: float, start_year: int = START_YEAR, end_year: int = END_YEAR) -> float:
    if start_value <= 0 or end_value <= 0 or end_year <= start_year:
        return np.nan
    return (end_value / start_value) ** (1 / (end_year - start_year)) - 1


def fmt_num_es(value: float, digits: int = 1) -> str:
    if pd.isna(value):
        return "--"
    text = f"{value:,.{digits}f}"
    return text.replace(",", "X").replace(".", ",").replace("X", ".")


def fmt_pct_es(value: float, digits: int = 2) -> str:
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


def load_pib_departamental() -> pd.DataFrame:
    raw = pd.read_excel(PIB_DEP_XLSX, sheet_name="Cuadro 2", header=None)
    header_rows = raw.index[
        raw.iloc[:, 0].astype(str).str.contains("Cuentas Nacionales", case=False, na=False)
    ].tolist()

    rows = []
    for header_idx in header_rows:
        department_title = raw.iat[header_idx - 5, 0]
        code = DEPARTMENT_CODE_BY_NAME.get(normalize_name(department_title))
        if code is None:
            continue

        year_cols = {
            col: parse_year(raw.iat[header_idx, col])
            for col in range(3, min(23, raw.shape[1]))
        }

        pib_row = None
        for idx in range(header_idx + 1, min(header_idx + 25, len(raw))):
            if str(raw.iat[idx, 2]).strip().upper() == "PIB DEPARTAMENTAL":
                pib_row = idx
                break
        if pib_row is None:
            raise ValueError(f"No se encontró el renglón de PIB departamental para {department_title}")

        for col, year in year_cols.items():
            value = pd.to_numeric(raw.iat[pib_row, col], errors="coerce")
            if year is not None and START_YEAR <= year <= END_YEAR and pd.notna(value):
                rows.append(
                    {
                        "anio": year,
                        "depto": code,
                        "departamento": DEPARTMENTS_24[code],
                        "pib_miles_millones_2015": float(value),
                    }
                )

    pib = pd.DataFrame(rows)
    expected = len(DEPARTMENTS_24) * (END_YEAR - START_YEAR + 1)
    if len(pib) != expected:
        raise ValueError(f"Se esperaban {expected} filas de PIB departamental y se obtuvieron {len(pib)}")
    return pib


def load_geih_departamental() -> pd.DataFrame:
    geih = pd.read_stata(
        GEIH_DTA,
        columns=["anio", "depto", "fex", "horas"],
        convert_categoricals=False,
    )
    geih["anio"] = pd.to_numeric(geih["anio"], errors="coerce").astype("Int64")
    geih["depto"] = pd.to_numeric(geih["depto"], errors="coerce").astype("Int64")
    geih["fex"] = pd.to_numeric(geih["fex"], errors="coerce")
    geih["horas"] = pd.to_numeric(geih["horas"], errors="coerce")
    geih = geih[
        geih["anio"].between(START_YEAR, END_YEAR)
        & ~geih["anio"].isin(EXCLUDED_YEARS)
        & geih["depto"].isin(DEPARTMENTS_24)
        & (geih["fex"] > 0)
    ].copy()
    geih["horas_validas"] = geih["horas"].where(geih["horas"].between(1, 112))
    geih["horas_sem_expand"] = geih["fex"] * geih["horas_validas"]
    geih["fex_horas_validas"] = geih["fex"].where(geih["horas_validas"].notna(), 0)
    dep = (
        geih.groupby(["anio", "depto"], as_index=False)
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
        )
    )
    dep["departamento"] = dep["depto"].map(DEPARTMENTS_24)

    required_years = set(range(START_YEAR, END_YEAR + 1)) - EXCLUDED_YEARS
    counts = dep.groupby("depto")["anio"].nunique()
    missing = counts[counts != len(required_years)]
    if not missing.empty:
        raise ValueError(f"Departamentos sin todos los años GEIH requeridos: {missing.to_dict()}")
    return dep


def build_productivity_departamental() -> tuple[pd.DataFrame, pd.DataFrame, dict[str, float]]:
    pib = load_pib_departamental()
    labor = load_geih_departamental()
    data = pib.merge(labor, on=["anio", "depto", "departamento"], how="inner")
    data = data[~data["anio"].isin(EXCLUDED_YEARS)].copy()
    data["pib_pesos_2015"] = data["pib_miles_millones_2015"] * 1e9
    data["pib_por_trabajador_millones_2015"] = data["pib_pesos_2015"] / data["ocupados"] / 1e6
    data["pib_por_hora_pesos_2015"] = data["pib_pesos_2015"] / data["horas_anuales"]
    data["horas_semanales_por_trabajador"] = data["horas_anuales"] / data["ocupados"] / 52
    data = data.sort_values(["departamento", "anio"])

    rows = []
    for depto, part in data.groupby("depto"):
        start = part[part["anio"] == START_YEAR]
        end = part[part["anio"] == END_YEAR]
        if start.empty or end.empty:
            continue
        start = start.iloc[0]
        end = end.iloc[0]
        rows.append(
            {
                "depto": depto,
                "departamento": end["departamento"],
                "pib_2010": start["pib_miles_millones_2015"],
                "pib_2024": end["pib_miles_millones_2015"],
                "ocupados_2010": start["ocupados"],
                "ocupados_2024": end["ocupados"],
                "horas_2010": start["horas_anuales"],
                "horas_2024": end["horas_anuales"],
                "horas_sem_2010": start["horas_semanales_por_trabajador"],
                "horas_sem_2024": end["horas_semanales_por_trabajador"],
                "pib_trabajador_2010": start["pib_por_trabajador_millones_2015"],
                "pib_trabajador_2024": end["pib_por_trabajador_millones_2015"],
                "pib_hora_2010": start["pib_por_hora_pesos_2015"],
                "pib_hora_2024": end["pib_por_hora_pesos_2015"],
                "crec_pib": cagr(start["pib_miles_millones_2015"], end["pib_miles_millones_2015"]),
                "crec_ocupados": cagr(start["ocupados"], end["ocupados"]),
                "crec_horas": cagr(start["horas_anuales"], end["horas_anuales"]),
                "crec_horas_por_trabajador": cagr(
                    start["horas_semanales_por_trabajador"],
                    end["horas_semanales_por_trabajador"],
                ),
                "crec_pib_trabajador": cagr(
                    start["pib_por_trabajador_millones_2015"],
                    end["pib_por_trabajador_millones_2015"],
                ),
                "crec_pib_hora": cagr(
                    start["pib_por_hora_pesos_2015"],
                    end["pib_por_hora_pesos_2015"],
                ),
            }
        )
    summary = pd.DataFrame(rows).sort_values("crec_pib_hora", ascending=False)

    aggregate = (
        data.groupby("anio", as_index=False)
        .agg(
            pib_miles_millones_2015=("pib_miles_millones_2015", "sum"),
            ocupados=("ocupados", "sum"),
            horas_anuales=("horas_anuales", "sum"),
        )
    )
    aggregate["pib_pesos_2015"] = aggregate["pib_miles_millones_2015"] * 1e9
    aggregate["pib_por_trabajador_millones_2015"] = aggregate["pib_pesos_2015"] / aggregate["ocupados"] / 1e6
    aggregate["pib_por_hora_pesos_2015"] = aggregate["pib_pesos_2015"] / aggregate["horas_anuales"]
    aggregate["horas_semanales_por_trabajador"] = aggregate["horas_anuales"] / aggregate["ocupados"] / 52
    agg_start = aggregate[aggregate["anio"] == START_YEAR].iloc[0]
    agg_end = aggregate[aggregate["anio"] == END_YEAR].iloc[0]
    benchmarks = {
        "pib_trabajador_2024": agg_end["pib_por_trabajador_millones_2015"],
        "pib_hora_2024": agg_end["pib_por_hora_pesos_2015"],
        "crec_pib_trabajador": cagr(
            agg_start["pib_por_trabajador_millones_2015"],
            agg_end["pib_por_trabajador_millones_2015"],
        ),
        "crec_pib_hora": cagr(
            agg_start["pib_por_hora_pesos_2015"],
            agg_end["pib_por_hora_pesos_2015"],
        ),
        "crec_ocupados": cagr(agg_start["ocupados"], agg_end["ocupados"]),
        "crec_pib": cagr(agg_start["pib_miles_millones_2015"], agg_end["pib_miles_millones_2015"]),
        "horas_sem_2024": agg_end["horas_semanales_por_trabajador"],
    }
    return data, summary, benchmarks


def write_summary_table(summary: pd.DataFrame) -> None:
    table = summary.sort_values("crec_pib_hora", ascending=False)
    lines = [
        r"\begin{table}[H]",
        r"\centering",
        rf"\caption{{Productividad laboral por departamento, {START_YEAR}--{END_YEAR}pr}}",
        r"\label{tab:pib_geih_productividad_departamento}",
        r"\scriptsize",
        r"\begin{tabular}{lrrrr}",
        r"\toprule",
        rf"Departamento & PIB/trab. {END_YEAR}pr & PIB/hora {END_YEAR}pr & Crec. PIB/trab. & Crec. PIB/hora \\",
        r"\midrule",
    ]
    for _, row in table.iterrows():
        lines.append(
            f"{escape_latex(row['departamento'])} & "
            f"{fmt_num_es(row['pib_trabajador_2024'], 1)} & "
            f"{fmt_num_es(row['pib_hora_2024'] / 1000, 1)} & "
            f"{fmt_pct_es(row['crec_pib_trabajador'], 2)} & "
            f"{fmt_pct_es(row['crec_pib_hora'], 2)} \\\\"
        )
    lines.extend(
        [
            r"\bottomrule",
            r"\end{tabular}",
            r"\caption*{\footnotesize Nota: PIB por trabajador en millones de pesos constantes de 2015; PIB por hora en miles de pesos constantes de 2015. Bogotá se trata como departamento. El universo corresponde a los 24 departamentos con información comparable en la GEIH para todo el periodo. Se excluye 2020 por no contar con GEIH anual comparable en la base del proyecto. Fuente: cálculos propios con DANE, Cuentas Nacionales Departamentales, y GEIH.}",
            r"\end{table}",
        ]
    )
    (SECTION_DIR / "pib_geih_productividad_departamento_table.tex").write_text(
        "\n".join(lines), encoding="utf-8"
    )


def write_level_ranking_table(summary: pd.DataFrame) -> None:
    table = summary.copy()
    table["ranking_pib_trabajador"] = table["pib_trabajador_2024"].rank(ascending=False, method="min").astype(int)
    table["ranking_pib_hora"] = table["pib_hora_2024"].rank(ascending=False, method="min").astype(int)
    table = table.sort_values("ranking_pib_hora")

    lines = [
        r"\begin{table}[H]",
        r"\centering",
        rf"\caption{{Ranking departamental de niveles de productividad laboral, {END_YEAR}pr}}",
        r"\label{tab:pib_geih_productividad_departamento_ranking_niveles}",
        r"\scriptsize",
        r"\begin{tabular}{lrrrr}",
        r"\toprule",
        rf"Departamento & PIB/trab. {END_YEAR}pr & Puesto & PIB/hora {END_YEAR}pr & Puesto \\",
        r"\midrule",
    ]
    for _, row in table.iterrows():
        lines.append(
            f"{escape_latex(row['departamento'])} & "
            f"{fmt_num_es(row['pib_trabajador_2024'], 1)} & "
            f"{int(row['ranking_pib_trabajador'])} & "
            f"{fmt_num_es(row['pib_hora_2024'] / 1000, 1)} & "
            f"{int(row['ranking_pib_hora'])} \\\\"
        )
    lines.extend(
        [
            r"\bottomrule",
            r"\end{tabular}",
            r"\caption*{\footnotesize Nota: PIB por trabajador en millones de pesos constantes de 2015; PIB por hora en miles de pesos constantes de 2015. La tabla está ordenada por el nivel de PIB por hora trabajada en 2024pr. Fuente: cálculos propios con DANE y GEIH.}",
            r"\end{table}",
        ]
    )

    table[
        [
            "depto",
            "departamento",
            "pib_trabajador_2024",
            "ranking_pib_trabajador",
            "pib_hora_2024",
            "ranking_pib_hora",
        ]
    ].to_csv(
        TABLE_DIR / "pib_geih_productividad_departamento_ranking_niveles.csv",
        index=False,
        encoding="utf-8-sig",
    )
    table[
        [
            "depto",
            "departamento",
            "pib_trabajador_2024",
            "ranking_pib_trabajador",
            "pib_hora_2024",
            "ranking_pib_hora",
        ]
    ].to_csv(
        OUTPUT_TABLE_DIR / "pib_geih_productividad_departamento_ranking_niveles.csv",
        index=False,
        encoding="utf-8-sig",
    )
    (SECTION_DIR / "pib_geih_productividad_departamento_ranking_niveles.tex").write_text(
        "\n".join(lines), encoding="utf-8"
    )


def build_productivity_remuneration_table(summary: pd.DataFrame) -> tuple[pd.DataFrame, dict[str, float]]:
    totals: dict[int, list[float]] = {}
    reader = pd.read_stata(
        GEIH_DTA,
        columns=["anio", "depto", "fex", "horas", "ingreso_hora_real"],
        convert_categoricals=False,
        chunksize=200_000,
    )

    for chunk in reader:
        chunk = chunk[
            (chunk["anio"] == END_YEAR)
            & chunk["depto"].isin(DEPARTMENTS_24)
        ].copy()
        if chunk.empty:
            continue
        for col in ["fex", "horas", "ingreso_hora_real"]:
            chunk[col] = pd.to_numeric(chunk[col], errors="coerce")
        valid = chunk[
            (chunk["fex"] > 0)
            & chunk["horas"].between(1, 112)
            & (chunk["ingreso_hora_real"] > 0)
        ].copy()
        if valid.empty:
            continue
        valid["rem_total_mensual"] = (
            valid["fex"] * valid["ingreso_hora_real"] * valid["horas"] * MONTHS_PER_WEEK
        )
        grouped = valid.groupby("depto", as_index=False).agg(
            ocupados_remuneracion_valida=("fex", "sum"),
            rem_total_mensual=("rem_total_mensual", "sum"),
        )
        for row in grouped.itertuples(index=False):
            key = int(row.depto)
            if key not in totals:
                totals[key] = [0.0, 0.0]
            totals[key][0] += float(row.ocupados_remuneracion_valida)
            totals[key][1] += float(row.rem_total_mensual)

    rem = pd.DataFrame(
        [
            {
                "depto": depto,
                "ocupados_remuneracion_valida": values[0],
                "rem_total_mensual": values[1],
            }
            for depto, values in totals.items()
        ]
    )
    table = summary[
        ["depto", "departamento", "ocupados_2024", "pib_trabajador_2024"]
    ].merge(rem, on="depto", how="inner")
    if len(table) != len(DEPARTMENTS_24):
        raise ValueError(
            f"Se esperaban {len(DEPARTMENTS_24)} departamentos con remuneracion y se obtuvieron {len(table)}"
        )

    table["rem_por_trabajador_2024"] = table["rem_total_mensual"] / table["ocupados_2024"]
    table["share_ocupados_remuneracion_valida"] = (
        table["ocupados_remuneracion_valida"] / table["ocupados_2024"]
    )
    table["ranking_pib_trabajador"] = table["pib_trabajador_2024"].rank(
        ascending=False, method="min"
    ).astype(int)
    table["ranking_rem_trabajador"] = table["rem_por_trabajador_2024"].rank(
        ascending=False, method="min"
    ).astype(int)
    table["brecha_ranking_rem_menos_pib"] = (
        table["ranking_rem_trabajador"] - table["ranking_pib_trabajador"]
    )
    slope, intercept = np.polyfit(
        table["pib_trabajador_2024"].astype(float).to_numpy(),
        (table["rem_por_trabajador_2024"] / 1e6).astype(float).to_numpy(),
        1,
    )
    table["rem_por_trabajador_predicha_tendencia"] = (
        intercept + slope * table["pib_trabajador_2024"]
    ) * 1e6
    table["residuo_rem_por_trabajador_tendencia"] = (
        table["rem_por_trabajador_2024"] - table["rem_por_trabajador_predicha_tendencia"]
    )
    table["residuo_pct_tendencia"] = (
        table["residuo_rem_por_trabajador_tendencia"]
        / table["rem_por_trabajador_predicha_tendencia"]
    )
    table = table.sort_values("ranking_pib_trabajador")

    benchmarks = {
        "rem_por_trabajador_2024": table["rem_total_mensual"].sum() / table["ocupados_2024"].sum(),
        "pib_trabajador_2024": summary["pib_2024"].sum() * 1e9 / summary["ocupados_2024"].sum() / 1e6,
        "corr_pib_remuneracion": table[["pib_trabajador_2024", "rem_por_trabajador_2024"]]
        .corr()
        .iloc[0, 1],
        "corr_rank_pib_remuneracion": table[["ranking_pib_trabajador", "ranking_rem_trabajador"]]
        .corr()
        .iloc[0, 1],
    }
    return table, benchmarks


def write_productivity_remuneration_table(table: pd.DataFrame) -> None:
    lines = [
        r"\begin{table}[H]",
        r"\centering",
        rf"\caption{{Ranking departamental de PIB por trabajador y remuneraci\'on por trabajador, {END_YEAR}pr}}",
        r"\label{tab:pib_geih_productividad_departamento_remuneracion}",
        r"\scriptsize",
        r"\begin{tabular}{lrrrrr}",
        r"\toprule",
        rf"Departamento & PIB/trab. {END_YEAR}pr & Puesto & Rem./trab. {END_YEAR} & Puesto & Dif. puestos \\",
        r"\midrule",
    ]
    for _, row in table.iterrows():
        lines.append(
            f"{escape_latex(row['departamento'])} & "
            f"{fmt_num_es(row['pib_trabajador_2024'], 1)} & "
            f"{int(row['ranking_pib_trabajador'])} & "
            f"{fmt_num_es(row['rem_por_trabajador_2024'] / 1e6, 2)} & "
            f"{int(row['ranking_rem_trabajador'])} & "
            f"{int(row['brecha_ranking_rem_menos_pib']):+d} \\\\"
        )
    lines.extend(
        [
            r"\bottomrule",
            r"\end{tabular}",
            r"\caption*{\footnotesize Nota: PIB por trabajador en millones de pesos constantes de 2015; remuneraci\'on por trabajador en millones de pesos constantes de 2025 al mes. La remuneraci\'on por trabajador se calcula como la remuneraci\'on laboral mensual total observada entre ocupados con ingreso horario positivo y horas v\'alidas, dividida por el n\'umero total de ocupados del departamento. La diferencia de puestos corresponde al puesto en remuneraci\'on menos el puesto en PIB por trabajador; un valor positivo indica que el departamento ocupa una posici\'on m\'as baja en remuneraci\'on que en productividad. Fuente: c\'alculos propios con DANE y GEIH.}",
            r"\end{table}",
        ]
    )
    cols = [
        "depto",
        "departamento",
        "pib_trabajador_2024",
        "ranking_pib_trabajador",
        "rem_por_trabajador_2024",
        "ranking_rem_trabajador",
        "brecha_ranking_rem_menos_pib",
        "share_ocupados_remuneracion_valida",
        "rem_por_trabajador_predicha_tendencia",
        "residuo_rem_por_trabajador_tendencia",
        "residuo_pct_tendencia",
    ]
    for directory in [TABLE_DIR, OUTPUT_TABLE_DIR]:
        table[cols].to_csv(
            directory / "pib_geih_productividad_departamento_remuneracion.csv",
            index=False,
            encoding="utf-8-sig",
        )
    (SECTION_DIR / "pib_geih_productividad_departamento_remuneracion.tex").write_text(
        "\n".join(lines), encoding="utf-8"
    )


def metric_rows(start: pd.Series, end: pd.Series) -> list[tuple[str, str, str, str]]:
    return [
        (
            "PIB real (Billones de pesos de 2015)",
            fmt_num_es(start["pib_miles_millones_2015"] / 1000, 1),
            fmt_num_es(end["pib_miles_millones_2015"] / 1000, 1),
            fmt_pct_es(cagr(start["pib_miles_millones_2015"], end["pib_miles_millones_2015"]), 2),
        ),
        (
            "Ocupados (Millones)",
            fmt_num_es(start["ocupados"] / 1e6, 2),
            fmt_num_es(end["ocupados"] / 1e6, 2),
            fmt_pct_es(cagr(start["ocupados"], end["ocupados"]), 2),
        ),
        (
            "PIB por trabajador (Millones de pesos de 2015)",
            fmt_num_es(start["pib_por_trabajador_millones_2015"], 1),
            fmt_num_es(end["pib_por_trabajador_millones_2015"], 1),
            fmt_pct_es(cagr(start["pib_por_trabajador_millones_2015"], end["pib_por_trabajador_millones_2015"]), 2),
        ),
        (
            "Horas semanales por trabajador",
            fmt_num_es(start["horas_semanales_por_trabajador"], 1),
            fmt_num_es(end["horas_semanales_por_trabajador"], 1),
            fmt_pct_es(
                cagr(start["horas_semanales_por_trabajador"], end["horas_semanales_por_trabajador"]),
                2,
            ),
        ),
        (
            "PIB por hora trabajada (Miles de pesos de 2015)",
            fmt_num_es(start["pib_por_hora_pesos_2015"] / 1000, 1),
            fmt_num_es(end["pib_por_hora_pesos_2015"] / 1000, 1),
            fmt_pct_es(cagr(start["pib_por_hora_pesos_2015"], end["pib_por_hora_pesos_2015"]), 2),
        ),
    ]


def write_detail_section(data: pd.DataFrame, summary: pd.DataFrame) -> None:
    lines = [
        r"\textbf{A continuación se presenta el detalle departamental del crecimiento de la productividad laboral.} Para cada departamento se reportan el PIB real, el número de ocupados, el PIB por trabajador, las horas semanales por trabajador y el PIB por hora trabajada al inicio y al final del periodo.",
        "",
    ]
    ordered = summary.sort_values("crec_pib_hora", ascending=False)
    for _, row in ordered.iterrows():
        part = data[data["depto"] == row["depto"]].sort_values("anio")
        start = part[part["anio"] == START_YEAR].iloc[0]
        end = part[part["anio"] == END_YEAR].iloc[0]
        name = row["departamento"]
        lines.extend(
            [
                rf"\subsection{{{escape_latex(name)}}}",
                rf"\textbf{{Entre {START_YEAR} y {END_YEAR}pr, el PIB por hora trabajada de {escape_latex(name)} creció {fmt_pct_es(row['crec_pib_hora'], 2)} anual.}} El PIB por trabajador creció {fmt_pct_es(row['crec_pib_trabajador'], 2)} anual, mientras que las horas semanales por trabajador cambiaron {fmt_pct_es(row['crec_horas_por_trabajador'], 2)} anual. Esta diferencia muestra si la productividad medida por trabajador se mueve en línea con la productividad por hora o si está afectada por cambios en la intensidad horaria.",
                r"\begin{table}[H]",
                r"\centering",
                rf"\caption{{{escape_latex(name)}: PIB, ocupados, horas y productividad laboral, {START_YEAR}--{END_YEAR}pr}}",
                rf"\label{{tab:departamento_{latex_id(name)}_productividad}}",
                r"\scriptsize",
                r"\begin{tabular}{lrrr}",
                r"\toprule",
                rf"Indicador & {START_YEAR} & {END_YEAR}pr & Crec. anual \\",
                r"\midrule",
            ]
        )
        for label, value_start, value_end, growth in metric_rows(start, end):
            lines.append(f"{escape_latex(label)} & {value_start} & {value_end} & {growth} \\\\")
        lines.extend(
            [
                r"\bottomrule",
                r"\end{tabular}",
                r"\end{table}",
                "",
            ]
        )
    lines.append(
        r"{\footnotesize Nota general: PIB real en billones de pesos constantes de 2015; ocupados en millones; PIB por trabajador en millones de pesos constantes de 2015; PIB por hora en miles de pesos constantes de 2015. Fuente: cálculos propios con DANE y GEIH.}"
    )
    (SECTION_DIR / "pib_geih_productividad_departamento_detalle.tex").write_text(
        "\n".join(lines), encoding="utf-8"
    )


def write_correlation_table(summary: pd.DataFrame) -> None:
    pairs = [
        ("Crec. ocupados", "Crec. PIB por trabajador", "crec_ocupados", "crec_pib_trabajador"),
        ("Crec. ocupados", "Crec. PIB por hora", "crec_ocupados", "crec_pib_hora"),
        ("Crec. horas totales", "Crec. PIB por trabajador", "crec_horas", "crec_pib_trabajador"),
        ("Crec. horas totales", "Crec. PIB por hora", "crec_horas", "crec_pib_hora"),
        ("PIB por trabajador inicial", "Crec. PIB por trabajador", "pib_trabajador_2010", "crec_pib_trabajador"),
        ("PIB por hora inicial", "Crec. PIB por hora", "pib_hora_2010", "crec_pib_hora"),
    ]
    lines = [
        r"\begin{table}[H]",
        r"\centering",
        r"\caption{Correlaciones departamentales seleccionadas sobre crecimiento y productividad}",
        r"\label{tab:pib_geih_productividad_departamento_correlaciones}",
        r"\begin{tabular}{llr}",
        r"\toprule",
        r"Variable 1 & Variable 2 & Correlación \\",
        r"\midrule",
    ]
    rows = []
    for label_x, label_y, x, y in pairs:
        corr = summary[[x, y]].dropna().corr().iloc[0, 1]
        rows.append({"variable_1": label_x, "variable_2": label_y, "correlacion": corr})
        lines.append(f"{escape_latex(label_x)} & {escape_latex(label_y)} & {fmt_num_es(corr, 2)} \\\\")
    lines.extend(
        [
            r"\bottomrule",
            r"\end{tabular}",
            r"\caption*{\footnotesize Nota: correlaciones de Pearson calculadas entre los 24 departamentos comparables, tratando a Bogotá como departamento. Fuente: cálculos propios con DANE y GEIH.}",
            r"\end{table}",
        ]
    )
    pd.DataFrame(rows).to_csv(
        TABLE_DIR / "pib_geih_productividad_departamento_correlaciones.csv",
        index=False,
        encoding="utf-8-sig",
    )
    pd.DataFrame(rows).to_csv(
        OUTPUT_TABLE_DIR / "pib_geih_productividad_departamento_correlaciones.csv",
        index=False,
        encoding="utf-8-sig",
    )
    (SECTION_DIR / "pib_geih_productividad_departamento_correlaciones.tex").write_text(
        "\n".join(lines), encoding="utf-8"
    )


def fonts() -> tuple[ImageFont.ImageFont, ImageFont.ImageFont, ImageFont.ImageFont, ImageFont.ImageFont, ImageFont.ImageFont]:
    font = ImageFont.load_default()
    arial = Path(r"C:\Windows\Fonts\arial.ttf")
    arial_bold = Path(r"C:\Windows\Fonts\arialbd.ttf")
    if arial.exists():
        return (
            ImageFont.truetype(str(arial_bold if arial_bold.exists() else arial), 54),
            ImageFont.truetype(str(arial_bold if arial_bold.exists() else arial), 36),
            ImageFont.truetype(str(arial), 29),
            ImageFont.truetype(str(arial), 25),
            ImageFont.truetype(str(arial), 21),
        )
    return font, font, font, font, font


def draw_department_growth_chart(summary: pd.DataFrame) -> None:
    data = summary.sort_values("crec_pib_hora", ascending=True).reset_index(drop=True)
    title_font, label_font, small_font, note_font, _ = fonts()
    img = Image.new("RGB", (1900, 1650), "white")
    draw = ImageDraw.Draw(img)
    draw.text(
        (80, 45),
        f"Crecimiento anualizado de la productividad laboral, {START_YEAR}--{END_YEAR}pr",
        fill="#222222",
        font=title_font,
    )
    draw.text(
        (80, 105),
        "24 departamentos comparables; Bogotá se trata como departamento",
        fill="#555555",
        font=label_font,
    )
    left, top, right, bottom = 600, 200, 1780, 1460
    min_value = min(data["crec_pib_trabajador"].min(), data["crec_pib_hora"].min(), 0)
    max_value = max(data["crec_pib_trabajador"].max(), data["crec_pib_hora"].max(), 0)
    min_value = math.floor(min_value * 100) / 100 - 0.005
    max_value = math.ceil(max_value * 100) / 100 + 0.005

    def x_pos(value: float) -> float:
        return left + (value - min_value) / (max_value - min_value) * (right - left)

    row_h = (bottom - top) / len(data)
    for tick in np.arange(math.ceil(min_value * 100), math.floor(max_value * 100) + 1, 1):
        value = tick / 100
        x = x_pos(value)
        draw.line((x, top - 20, x, bottom), fill="#eeeeee", width=1)
        draw.text((x - 26, bottom + 18), f"{tick}%", fill="#555555", font=small_font)
    x0 = x_pos(0)
    draw.line((x0, top - 20, x0, bottom), fill="#888888", width=2)

    for i, row in data.iterrows():
        y = top + i * row_h + row_h / 2
        draw.text((80, y - 15), row["departamento"], fill="#333333", font=small_font)
        for value, color, offset in [
            (row["crec_pib_trabajador"], "#1f77b4", -7),
            (row["crec_pib_hora"], "#d95f02", 7),
        ]:
            x = x_pos(value)
            draw.rectangle((min(x0, x), y + offset - 5, max(x0, x), y + offset + 5), fill=color)
            draw.ellipse((x - 7, y + offset - 7, x + 7, y + offset + 7), fill=color)

    draw.rectangle((1180, 155, 1210, 177), fill="#1f77b4")
    draw.text((1220, 147), "PIB por trabajador", fill="#333333", font=small_font)
    draw.rectangle((1460, 155, 1490, 177), fill="#d95f02")
    draw.text((1500, 147), "PIB por hora", fill="#333333", font=small_font)
    draw.text((80, 1580), "Fuente: cálculos propios con DANE y GEIH. Se excluye 2020.", fill="#555555", font=note_font)
    for directory in [FIGURE_DIR, OUTPUT_FIGURE_DIR]:
        img.save(directory / "fig_pib_geih_productividad_departamento.png")


def classify_quadrant(row: pd.Series, x_ref: float, y_ref: float) -> str:
    right = row["crec_pib_hora"] >= x_ref
    high = row["pib_hora_2024"] >= y_ref
    if high and right:
        return "Líderes en auge"
    if high and not right:
        return "Líderes en declive"
    if not high and right:
        return "Aceleradores"
    return "Rezagados"


def draw_department_quadrant_chart(summary: pd.DataFrame, benchmarks: dict[str, float]) -> None:
    data = summary.copy()
    x_ref = benchmarks["crec_pib_hora"]
    y_ref = benchmarks["pib_hora_2024"]
    data["cuadrante"] = data.apply(classify_quadrant, axis=1, args=(x_ref, y_ref))

    title_font, label_font, small_font, note_font, tiny_font = fonts()
    img = Image.new("RGB", (2400, 1700), "white")
    draw = ImageDraw.Draw(img)
    draw.text(
        (90, 45),
        f"Nivel y crecimiento de la productividad por hora, {START_YEAR}--{END_YEAR}pr",
        fill="#222222",
        font=title_font,
    )
    draw.text(
        (90, 108),
        "Cada burbuja representa un departamento; el eje vertical mide PIB por hora en 2024pr y el eje horizontal su crecimiento anualizado",
        fill="#555555",
        font=label_font,
    )

    left, top, right, bottom = 320, 250, 2180, 1390
    x_values = data["crec_pib_hora"] * 100
    y_values = data["pib_hora_2024"] / 1000
    x_ref_pct = x_ref * 100
    y_ref_th = y_ref / 1000
    x_min = math.floor(min(x_values.min(), x_ref_pct) - 1.2)
    x_max = math.ceil(max(x_values.max(), x_ref_pct) + 1.2)
    y_min = max(0, math.floor(min(y_values.min(), y_ref_th) - 2))
    y_max = math.ceil(max(y_values.max(), y_ref_th) + 5)

    def x_pos(value: float) -> float:
        return left + (value - x_min) / (x_max - x_min) * (right - left)

    def y_pos(value: float) -> float:
        return bottom - (value - y_min) / (y_max - y_min) * (bottom - top)

    draw.rectangle((left, top, right, bottom), outline="#333333", width=2)
    for tick in range(math.ceil(x_min), math.floor(x_max) + 1, 1):
        x = x_pos(tick)
        draw.line((x, top, x, bottom), fill="#eeeeee", width=1)
        draw.text((x - 22, bottom + 18), f"{tick}%", fill="#555555", font=small_font)
    y_tick_step = 5 if y_max <= 45 else 10
    for tick in range(math.ceil(y_min / y_tick_step) * y_tick_step, math.floor(y_max) + 1, y_tick_step):
        y = y_pos(tick)
        draw.line((left, y, right, y), fill="#eeeeee", width=1)
        draw.text((left - 85, y - 15), f"{tick}", fill="#555555", font=small_font)

    x_line = x_pos(x_ref_pct)
    y_line = y_pos(y_ref_th)
    blue = "#1f66c2"
    draw.line((x_line, top, x_line, bottom), fill=blue, width=4)
    draw.line((left, y_line, right, y_line), fill=blue, width=4)
    draw.text((x_line + 10, top + 12), f"Crec. agregado: {fmt_num_es(x_ref_pct, 1)}%", fill=blue, font=small_font)
    draw.text((right - 430, y_line - 42), f"Nivel agregado: {fmt_num_es(y_ref_th, 1)}", fill=blue, font=small_font)

    quadrant_labels = [
        ("Líderes en declive", left + 25, top + 25),
        ("Líderes en auge", right - 390, top + 25),
        ("Rezagados", left + 25, y_line + 25),
        ("Aceleradores", right - 330, y_line + 25),
    ]
    for label, x, y in quadrant_labels:
        draw.text((x, y), label, fill=blue, font=label_font)

    colors = {
        "Líderes en auge": "#f28e2b",
        "Líderes en declive": "#9aa7b0",
        "Aceleradores": "#59a14f",
        "Rezagados": "#4e79a7",
    }
    pib = data["pib_2024"]
    min_pib, max_pib = pib.min(), pib.max()

    label_offsets = {
        "Bogotá D.C.": (-160, -36),
        "Antioquia": (12, 34),
        "Valle del Cauca": (16, 12),
        "Santander": (16, -28),
        "Cundinamarca": (-210, 22),
        "Atlántico": (-170, -20),
        "Bolívar": (15, -35),
        "Meta": (15, 15),
        "Cesar": (15, -30),
        "Córdoba": (-150, 12),
        "Nariño": (16, 12),
        "Norte de Santander": (-250, -26),
        "Quindío": (16, -28),
        "Risaralda": (16, 10),
        "Caldas": (16, -32),
        "Sucre": (16, 10),
        "Chocó": (16, 10),
        "La Guajira": (16, -18),
        "Magdalena": (16, 10),
        "Caquetá": (16, -28),
        "Cauca": (-120, 16),
        "Huila": (16, -28),
        "Boyacá": (16, -28),
        "Tolima": (16, 10),
    }

    for _, row in data.sort_values("pib_2024", ascending=False).iterrows():
        x = x_pos(row["crec_pib_hora"] * 100)
        y = y_pos(row["pib_hora_2024"] / 1000)
        radius = 13 + 62 * math.sqrt((row["pib_2024"] - min_pib) / (max_pib - min_pib))
        color = colors[row["cuadrante"]]
        draw.ellipse((x - radius, y - radius, x + radius, y + radius), fill=color, outline="white", width=3)
        dx, dy = label_offsets.get(row["departamento"], (16, 10))
        draw.text((x + dx, y + dy), row["departamento"], fill="#222222", font=tiny_font)

    draw.text(
        ((left + right) / 2 - 260, bottom + 75),
        "Crecimiento anualizado del PIB por hora trabajada",
        fill="#333333",
        font=label_font,
    )
    draw.text(
        (left, top - 55),
        "PIB por hora trabajada, 2024pr (miles de pesos de 2015)",
        fill="#333333",
        font=label_font,
    )
    draw.text(
        (90, 1570),
        "Fuente: cálculos propios con DANE y GEIH. Líneas azules: agregado de los 24 departamentos comparables. Se excluye 2020.",
        fill="#555555",
        font=note_font,
    )
    for directory in [FIGURE_DIR, OUTPUT_FIGURE_DIR]:
        img.save(directory / "fig_pib_geih_productividad_departamento_cuadrantes.png")


def draw_department_correlation_scatter(summary: pd.DataFrame) -> None:
    data = summary.dropna(subset=["crec_ocupados", "crec_horas", "crec_pib_trabajador", "crec_pib_hora"]).copy()
    title_font, label_font, small_font, note_font, _ = fonts()
    img = Image.new("RGB", (2500, 1750), "white")
    draw = ImageDraw.Draw(img)
    draw.text(
        (90, 45),
        f"Crecimiento del trabajo y la productividad por departamento, {START_YEAR}--{END_YEAR}pr",
        fill="#222222",
        font=title_font,
    )
    draw.text(
        (90, 105),
        f"Tasas anualizadas para {len(data)} departamentos comparables; cada punto representa un departamento",
        fill="#555555",
        font=label_font,
    )

    panels = [
        (90, 220, 1200, 780, "crec_ocupados", "crec_pib_trabajador", "Ocupados", "PIB por trabajador"),
        (1330, 220, 2440, 780, "crec_ocupados", "crec_pib_hora", "Ocupados", "PIB por hora"),
        (90, 930, 1200, 1490, "crec_horas", "crec_pib_trabajador", "Horas totales", "PIB por trabajador"),
        (1330, 930, 2440, 1490, "crec_horas", "crec_pib_hora", "Horas totales", "PIB por hora"),
    ]
    y_min = math.floor(min(data["crec_pib_trabajador"].min(), data["crec_pib_hora"].min()) * 100) / 100 - 0.01
    y_max = math.ceil(max(data["crec_pib_trabajador"].max(), data["crec_pib_hora"].max()) * 100) / 100 + 0.01

    for left, top, right, bottom, x_col, y_col, x_lab, y_lab in panels:
        x_min = math.floor(data[x_col].min() * 100) / 100 - 0.01
        x_max = math.ceil(data[x_col].max() * 100) / 100 + 0.01
        plot_left, plot_top = left + 155, top + 80
        plot_right, plot_bottom = right - 60, bottom - 95
        draw.text((left, top), f"{x_lab} vs. {y_lab}", fill="#222222", font=label_font)
        draw.rectangle((plot_left, plot_top, plot_right, plot_bottom), outline="#333333", width=2)

        def x_pos(value: float) -> float:
            return plot_left + (value - x_min) / (x_max - x_min) * (plot_right - plot_left)

        def y_pos(value: float) -> float:
            return plot_bottom - (value - y_min) / (y_max - y_min) * (plot_bottom - plot_top)

        for tick in np.arange(math.ceil(x_min * 100), math.floor(x_max * 100) + 1, 2):
            value = tick / 100
            x = x_pos(value)
            draw.line((x, plot_top, x, plot_bottom), fill="#eeeeee", width=1)
            draw.text((x - 34, plot_bottom + 16), f"{tick}%", fill="#555555", font=small_font)
        for tick in np.arange(math.ceil(y_min * 100), math.floor(y_max * 100) + 1, 2):
            value = tick / 100
            y = y_pos(value)
            draw.line((plot_left, y, plot_right, y), fill="#eeeeee", width=1)
            draw.text((plot_left - 95, y - 15), f"{tick}%", fill="#555555", font=small_font)
        if x_min < 0 < x_max:
            draw.line((x_pos(0), plot_top, x_pos(0), plot_bottom), fill="#999999", width=2)
        if y_min < 0 < y_max:
            draw.line((plot_left, y_pos(0), plot_right, y_pos(0)), fill="#999999", width=2)

        xs = data[x_col].astype(float).to_numpy()
        ys = data[y_col].astype(float).to_numpy()
        slope, intercept = np.polyfit(xs, ys, 1)
        draw.line(
            (x_pos(x_min), y_pos(slope * x_min + intercept), x_pos(x_max), y_pos(slope * x_max + intercept)),
            fill="#b44b3f",
            width=4,
        )
        for _, row in data.iterrows():
            x = x_pos(row[x_col])
            y = y_pos(row[y_col])
            draw.ellipse((x - 10, y - 10, x + 10, y + 10), fill="#1f77b4", outline="white", width=2)
        corr = data[[x_col, y_col]].corr().iloc[0, 1]
        draw.text((plot_left + 16, plot_top + 12), f"r = {fmt_num_es(corr, 2)}", fill="#b44b3f", font=label_font)

    draw.text(
        (90, 1630),
        "Nota: r es la correlación de Pearson; la línea roja muestra la tendencia lineal simple entre departamentos.",
        fill="#555555",
        font=note_font,
    )
    for directory in [FIGURE_DIR, OUTPUT_FIGURE_DIR]:
        img.save(directory / "fig_pib_geih_productividad_departamento_correlaciones.png")


def draw_department_convergence_scatter(summary: pd.DataFrame) -> None:
    data = summary.dropna(
        subset=["pib_trabajador_2010", "pib_hora_2010", "crec_pib_trabajador", "crec_pib_hora"]
    ).copy()
    title_font, label_font, small_font, note_font, tiny_font = fonts()
    img = Image.new("RGB", (2500, 1500), "white")
    draw = ImageDraw.Draw(img)
    draw.text(
        (90, 45),
        f"Nivel inicial y crecimiento de la productividad laboral, {START_YEAR}--{END_YEAR}pr",
        fill="#222222",
        font=title_font,
    )
    draw.text(
        (90, 105),
        "Cada punto representa un departamento; la línea roja muestra la tendencia lineal simple",
        fill="#555555",
        font=label_font,
    )

    panels = [
        (
            90,
            230,
            1200,
            1220,
            "pib_trabajador_2010",
            "crec_pib_trabajador",
            "PIB por trabajador en 2010",
            "Crec. PIB por trabajador",
            "millones de pesos de 2015",
            1.0,
        ),
        (
            1330,
            230,
            2440,
            1220,
            "pib_hora_2010",
            "crec_pib_hora",
            "PIB por hora en 2010",
            "Crec. PIB por hora",
            "miles de pesos de 2015",
            1000.0,
        ),
    ]

    label_offsets = {
        "Bogotá D.C.": (-120, -36),
        "Meta": (-110, 12),
        "Quindío": (16, -30),
        "Caquetá": (16, 12),
        "Risaralda": (16, -28),
        "Chocó": (16, -20),
        "La Guajira": (-130, 12),
        "Cundinamarca": (16, -34),
        "Antioquia": (16, 12),
    }

    for left, top, right, bottom, x_col, y_col, x_title, y_title, x_unit, divisor in panels:
        plot_left, plot_top = left + 150, top + 90
        plot_right, plot_bottom = right - 60, bottom - 145
        x_values = data[x_col] / divisor
        y_values = data[y_col] * 100
        x_min = math.floor(x_values.min() / 5) * 5 if divisor == 1000.0 else math.floor(x_values.min() / 10) * 10
        x_max = math.ceil(x_values.max() / 5) * 5 if divisor == 1000.0 else math.ceil(x_values.max() / 10) * 10
        if x_max == x_min:
            x_max += 1
        y_min = math.floor(min(y_values.min(), 0) - 1)
        y_max = math.ceil(y_values.max() + 1)

        def x_pos(value: float) -> float:
            return plot_left + (value - x_min) / (x_max - x_min) * (plot_right - plot_left)

        def y_pos(value: float) -> float:
            return plot_bottom - (value - y_min) / (y_max - y_min) * (plot_bottom - plot_top)

        draw.text((left, top), y_title, fill="#222222", font=label_font)
        draw.rectangle((plot_left, plot_top, plot_right, plot_bottom), outline="#333333", width=2)

        x_step = 10 if divisor == 1.0 else 5
        for tick in range(int(x_min), int(x_max) + 1, x_step):
            x = x_pos(tick)
            draw.line((x, plot_top, x, plot_bottom), fill="#eeeeee", width=1)
            draw.text((x - 30, plot_bottom + 18), f"{tick}", fill="#555555", font=small_font)
        for tick in range(int(y_min), int(y_max) + 1, 1):
            y = y_pos(tick)
            draw.line((plot_left, y, plot_right, y), fill="#eeeeee", width=1)
            draw.text((plot_left - 90, y - 15), f"{tick}%", fill="#555555", font=small_font)
        if y_min < 0 < y_max:
            draw.line((plot_left, y_pos(0), plot_right, y_pos(0)), fill="#999999", width=2)

        xs = x_values.astype(float).to_numpy()
        ys = y_values.astype(float).to_numpy()
        slope, intercept = np.polyfit(xs, ys, 1)
        draw.line(
            (x_pos(x_min), y_pos(slope * x_min + intercept), x_pos(x_max), y_pos(slope * x_max + intercept)),
            fill="#b44b3f",
            width=4,
        )
        corr = np.corrcoef(xs, ys)[0, 1]
        draw.text((plot_left + 18, plot_top + 16), f"r = {fmt_num_es(corr, 2)}", fill="#b44b3f", font=label_font)

        for _, row in data.iterrows():
            x = x_pos(row[x_col] / divisor)
            y = y_pos(row[y_col] * 100)
            draw.ellipse((x - 10, y - 10, x + 10, y + 10), fill="#1f77b4", outline="white", width=2)
            if row["departamento"] in label_offsets:
                dx, dy = label_offsets[row["departamento"]]
                draw.text((x + dx, y + dy), row["departamento"], fill="#222222", font=tiny_font)

        draw.text(
            (plot_left + 190, plot_bottom + 80),
            f"{x_title} ({x_unit})",
            fill="#333333",
            font=small_font,
        )

    draw.text(
        (90, 1370),
        "Fuente: cálculos propios con DANE y GEIH. Se excluye 2020. Una correlación negativa sugiere convergencia descriptiva, no una relación causal.",
        fill="#555555",
        font=note_font,
    )
    for directory in [FIGURE_DIR, OUTPUT_FIGURE_DIR]:
        img.save(directory / "fig_pib_geih_productividad_departamento_convergencia.png")


def draw_productivity_remuneration_scatter(
    table: pd.DataFrame, benchmarks: dict[str, float]
) -> None:
    data = table.copy()
    data["pib_trabajador_millones"] = data["pib_trabajador_2024"]
    data["rem_trabajador_millones"] = data["rem_por_trabajador_2024"] / 1e6

    title_font, label_font, small_font, note_font, tiny_font = fonts()
    img = Image.new("RGB", (2400, 1550), "white")
    draw = ImageDraw.Draw(img)
    draw.text(
        (90, 45),
        f"PIB por trabajador y remuneraci\u00f3n por trabajador, {END_YEAR}pr",
        fill="#222222",
        font=title_font,
    )
    draw.text(
        (90, 105),
        "Cada punto representa un departamento; las l\u00edneas azules muestran el agregado de los 24 departamentos",
        fill="#555555",
        font=label_font,
    )

    plot_left, plot_top, plot_right, plot_bottom = 250, 230, 2260, 1290
    x_min = math.floor(data["pib_trabajador_millones"].min() / 10) * 10
    x_max = math.ceil(data["pib_trabajador_millones"].max() / 10) * 10
    y_min = math.floor(data["rem_trabajador_millones"].min() * 10) / 10
    y_max = math.ceil(data["rem_trabajador_millones"].max() * 10) / 10

    def x_pos(value: float) -> float:
        return plot_left + (value - x_min) / (x_max - x_min) * (plot_right - plot_left)

    def y_pos(value: float) -> float:
        return plot_bottom - (value - y_min) / (y_max - y_min) * (plot_bottom - plot_top)

    draw.rectangle((plot_left, plot_top, plot_right, plot_bottom), outline="#333333", width=2)

    for tick in range(int(x_min), int(x_max) + 1, 10):
        x = x_pos(tick)
        draw.line((x, plot_top, x, plot_bottom), fill="#eeeeee", width=1)
        draw.text((x - 28, plot_bottom + 18), f"{tick}", fill="#555555", font=small_font)
    y_tick = y_min
    while y_tick <= y_max + 1e-9:
        y = y_pos(y_tick)
        draw.line((plot_left, y, plot_right, y), fill="#eeeeee", width=1)
        draw.text((plot_left - 95, y - 15), fmt_num_es(y_tick, 1), fill="#555555", font=small_font)
        y_tick += 0.2

    x_agg = benchmarks["pib_trabajador_2024"]
    y_agg = benchmarks["rem_por_trabajador_2024"] / 1e6
    draw.line((x_pos(x_agg), plot_top, x_pos(x_agg), plot_bottom), fill="#1f5aa6", width=4)
    draw.line((plot_left, y_pos(y_agg), plot_right, y_pos(y_agg)), fill="#1f5aa6", width=4)

    xs = data["pib_trabajador_millones"].astype(float).to_numpy()
    ys = data["rem_trabajador_millones"].astype(float).to_numpy()
    slope, intercept = np.polyfit(xs, ys, 1)
    draw.line(
        (x_pos(x_min), y_pos(slope * x_min + intercept), x_pos(x_max), y_pos(slope * x_max + intercept)),
        fill="#b44b3f",
        width=5,
    )

    label_offsets = {
        "Bogotá D.C.": (-165, -44),
        "Meta": (18, -38),
        "Santander": (18, 16),
        "Valle del Cauca": (16, 18),
        "Antioquia": (-160, -38),
        "Caldas": (-115, -44),
        "Risaralda": (-120, 18),
        "Norte de Santander": (18, -40),
        "Bolívar": (18, 12),
        "Boyacá": (18, -40),
        "La Guajira": (18, 10),
        "Nariño": (-105, 16),
        "Cundinamarca": (20, -44),
        "Quindío": (18, 12),
        "Atlántico": (18, -6),
    }
    for _, row in data.iterrows():
        x = x_pos(row["pib_trabajador_millones"])
        y = y_pos(row["rem_trabajador_millones"])
        draw.ellipse((x - 13, y - 13, x + 13, y + 13), fill="#1f77b4", outline="white", width=2)
        dx, dy = label_offsets.get(row["departamento"], (16, -12))
        draw.text((x + dx, y + dy), row["departamento"], fill="#222222", font=tiny_font)

    corr = benchmarks["corr_pib_remuneracion"]
    draw.text(
        (plot_left + 24, plot_top + 22),
        f"r = {fmt_num_es(corr, 2)}",
        fill="#b44b3f",
        font=label_font,
    )
    draw.text(
        (plot_left + 570, plot_bottom + 80),
        "PIB por trabajador, millones de pesos de 2015",
        fill="#333333",
        font=label_font,
    )
    y_label = "Remuneraci\u00f3n mensual por trabajador, millones de pesos de 2025"
    label_img = Image.new("RGBA", (980, 70), (255, 255, 255, 0))
    label_draw = ImageDraw.Draw(label_img)
    label_draw.text((0, 0), y_label, fill="#333333", font=label_font)
    label_img = label_img.rotate(90, expand=True)
    img.paste(label_img, (45, 380), label_img)
    draw.text(
        (90, 1430),
        "Fuente: c\u00e1lculos propios con DANE y GEIH. La l\u00ednea roja muestra la tendencia lineal simple entre departamentos.",
        fill="#555555",
        font=note_font,
    )
    for directory in [FIGURE_DIR, OUTPUT_FIGURE_DIR]:
        img.save(directory / "fig_pib_geih_productividad_departamento_remuneracion.png")


def write_benchmarks(benchmarks: dict[str, float]) -> None:
    pd.DataFrame([benchmarks]).to_csv(
        TABLE_DIR / "pib_geih_productividad_departamento_benchmarks.csv",
        index=False,
        encoding="utf-8-sig",
    )
    pd.DataFrame([benchmarks]).to_csv(
        OUTPUT_TABLE_DIR / "pib_geih_productividad_departamento_benchmarks.csv",
        index=False,
        encoding="utf-8-sig",
    )


def main() -> None:
    data, summary, benchmarks = build_productivity_departamental()
    data.to_csv(TABLE_DIR / "pib_geih_productividad_departamento_series.csv", index=False, encoding="utf-8-sig")
    data.to_csv(OUTPUT_TABLE_DIR / "pib_geih_productividad_departamento_series.csv", index=False, encoding="utf-8-sig")
    summary.to_csv(TABLE_DIR / "pib_geih_productividad_departamento_summary.csv", index=False, encoding="utf-8-sig")
    summary.to_csv(OUTPUT_TABLE_DIR / "pib_geih_productividad_departamento_summary.csv", index=False, encoding="utf-8-sig")
    write_benchmarks(benchmarks)

    write_summary_table(summary)
    write_level_ranking_table(summary)
    rem_table, rem_benchmarks = build_productivity_remuneration_table(summary)
    write_productivity_remuneration_table(rem_table)
    write_detail_section(data, summary)
    write_correlation_table(summary)
    draw_department_growth_chart(summary)
    draw_department_quadrant_chart(summary, benchmarks)
    draw_department_convergence_scatter(summary)
    draw_department_correlation_scatter(summary)
    draw_productivity_remuneration_scatter(rem_table, rem_benchmarks)

    print(f"Departamentos con información completa: {len(summary)}")
    print(f"Periodo: {START_YEAR}-{END_YEAR}pr; se excluye 2020")
    print(
        summary[["departamento", "crec_pib_trabajador", "crec_pib_hora", "crec_ocupados"]]
        .sort_values("crec_pib_hora", ascending=False)
        .to_string(index=False)
    )
    print("Agregado 24 departamentos:", benchmarks)
    print("Correlacion PIB por trabajador y remuneracion por trabajador:", rem_benchmarks["corr_pib_remuneracion"])


if __name__ == "__main__":
    main()
