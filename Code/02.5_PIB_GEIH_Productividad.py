from __future__ import annotations

import math
import os
from pathlib import Path

import numpy as np
import pandas as pd
from PIL import Image, ImageDraw, ImageFont


PROJECT_ROOT = Path(__file__).resolve().parents[1]
PIB_XLSX = Path(
    os.environ.get(
        "PIB_TRIMESTRAL_XLSX",
        r"C:\Users\olive\Downloads\anex-ProduccionConstantes-Itrim2026.xlsx",
    )
)
GEIH_DTA = PROJECT_ROOT / "Datos" / "Processed" / "Paper-GEIH_base_modelo_personas_2008_2025.dta"

TABLE_DIR = PROJECT_ROOT / "Paper" / "tables"
SECTION_DIR = PROJECT_ROOT / "Paper" / "sections"
FIGURE_DIR = PROJECT_ROOT / "Paper" / "figures"
OUTPUT_TABLE_DIR = PROJECT_ROOT / "Outputs" / "tables"
OUTPUT_FIGURE_DIR = PROJECT_ROOT / "Outputs" / "Figures"

for directory in [TABLE_DIR, SECTION_DIR, FIGURE_DIR, OUTPUT_TABLE_DIR, OUTPUT_FIGURE_DIR]:
    directory.mkdir(parents=True, exist_ok=True)


SECTOR_ORDER = [
    "A",
    "B",
    "C",
    "D+E",
    "F",
    "G+H+I",
    "J",
    "K",
    "L",
    "M+N",
    "O+P+Q",
    "R+S+T",
]

SECTOR_SHORT = {
    "A": "Agropecuario",
    "B": "Minas",
    "C": "Manufactura",
    "D+E": "Servicios públicos",
    "F": "Construcción",
    "G+H+I": "Comercio, transporte y alojamiento",
    "J": "Información y comunicaciones",
    "K": "Financieras",
    "L": "Inmobiliarias",
    "M+N": "Profesionales y administrativas",
    "O+P+Q": "Adm. pública, educación y salud",
    "R+S+T": "Artes, otros servicios y hogares",
}

SECTOR_DESCRIPTION = {
    "A": "agricultura, ganadería, caza, silvicultura, pesca y acuicultura",
    "B": "extracción de carbón, petróleo, gas y otras actividades de minas y canteras",
    "C": "industrias manufactureras, incluyendo alimentos, textiles, químicos, metales, maquinaria, vehículos, muebles y otras manufacturas",
    "D+E": "suministro de electricidad, gas y agua, junto con saneamiento, manejo de residuos y actividades relacionadas",
    "F": "construcción de edificaciones, obras civiles y actividades especializadas de construcción",
    "G+H+I": "comercio al por mayor y al por menor, reparación de vehículos, transporte, almacenamiento, alojamiento y servicios de comida",
    "J": "telecomunicaciones, actividades editoriales y audiovisuales, software, informática y servicios de información",
    "K": "intermediación financiera, seguros y actividades auxiliares del sistema financiero",
    "L": "actividades inmobiliarias, incluyendo alquiler, administración y operación de bienes inmuebles",
    "M+N": "actividades profesionales, científicas y técnicas, investigación y desarrollo, y servicios administrativos y de apoyo",
    "O+P+Q": "administración pública y defensa, educación, salud humana y servicios sociales",
    "R+S+T": "actividades artísticas, entretenimiento, recreación, otros servicios personales, asociaciones y hogares como empleadores",
}

SUBRAMA_TO_SECTOR = {}
for code in range(1, 4):
    SUBRAMA_TO_SECTOR[code] = "A"
for code in range(4, 6):
    SUBRAMA_TO_SECTOR[code] = "B"
for code in range(6, 17):
    SUBRAMA_TO_SECTOR[code] = "C"
for code in range(17, 19):
    SUBRAMA_TO_SECTOR[code] = "D+E"
SUBRAMA_TO_SECTOR[19] = "F"
for code in range(20, 28):
    SUBRAMA_TO_SECTOR[code] = "G+H+I"
for code in range(28, 33):
    SUBRAMA_TO_SECTOR[code] = "J"
for code in range(33, 35):
    SUBRAMA_TO_SECTOR[code] = "K"
SUBRAMA_TO_SECTOR[35] = "L"
for code in range(36, 39):
    SUBRAMA_TO_SECTOR[code] = "M+N"
for code in range(39, 43):
    SUBRAMA_TO_SECTOR[code] = "O+P+Q"
for code in range(43, 46):
    SUBRAMA_TO_SECTOR[code] = "R+S+T"


def parse_year(value) -> int | None:
    if pd.isna(value):
        return None
    text = str(value)
    digits = "".join(ch for ch in text if ch.isdigit())
    if len(digits) >= 4:
        return int(digits[:4])
    return None


def cagr(start_value: float, end_value: float, start_year: int, end_year: int) -> float:
    if start_value <= 0 or end_value <= 0:
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
    return fmt_num_es(100 * value, digits) + r"\%"


def escape_latex(text: str) -> str:
    replacements = {
        "&": r"\&",
        "%": r"\%",
        "$": r"\$",
        "#": r"\#",
        "_": r"\_",
    }
    for old, new in replacements.items():
        text = text.replace(old, new)
    return text


def indicator_with_unit(row: pd.Series) -> str:
    unit = str(row.get("unidad", "")).strip()
    indicator = str(row["indicador"]).strip()
    if unit:
        return f"{indicator} ({unit})"
    return indicator


def load_pib_quarterly() -> tuple[pd.DataFrame, pd.DataFrame]:
    raw = pd.read_excel(PIB_XLSX, sheet_name="Cuadro 1", header=None)
    year_row = raw.iloc[11]
    quarter_row = raw.iloc[12]

    columns = []
    current_year = None
    for col in range(3, raw.shape[1]):
        year = parse_year(year_row.iloc[col])
        if year is not None:
            current_year = year
        quarter = quarter_row.iloc[col]
        if current_year is not None and quarter in ["I", "II", "III", "IV"]:
            columns.append((col, current_year, str(quarter)))

    sector_rows = raw.iloc[14:26, [1, 2] + [col for col, _, _ in columns]].copy()
    sector_rows.columns = ["sector_code", "sector_name"] + [
        f"{year}_{quarter}" for _, year, quarter in columns
    ]
    sector_long = sector_rows.melt(
        id_vars=["sector_code", "sector_name"],
        var_name="period",
        value_name="pib_miles_millones_2015",
    )
    sector_long["sector_code"] = (
        sector_long["sector_code"].astype(str).str.replace(" ", "", regex=False)
    )
    sector_long[["anio", "trimestre"]] = sector_long["period"].str.split("_", expand=True)
    sector_long["anio"] = sector_long["anio"].astype(int)
    sector_long = sector_long.dropna(subset=["pib_miles_millones_2015"])

    total_row = raw.iloc[[28], [2] + [col for col, _, _ in columns]].copy()
    total_row.columns = ["concepto"] + [f"{year}_{quarter}" for _, year, quarter in columns]
    total_long = total_row.melt(
        id_vars=["concepto"],
        var_name="period",
        value_name="pib_miles_millones_2015",
    )
    total_long[["anio", "trimestre"]] = total_long["period"].str.split("_", expand=True)
    total_long["anio"] = total_long["anio"].astype(int)
    total_long = total_long.dropna(subset=["pib_miles_millones_2015"])

    def annualize(data: pd.DataFrame, group_cols: list[str]) -> pd.DataFrame:
        counts = data.groupby(group_cols + ["anio"])["trimestre"].nunique().reset_index(name="n_trim")
        complete = counts[counts["n_trim"] == 4][group_cols + ["anio"]]
        return (
            data.merge(complete, on=group_cols + ["anio"], how="inner")
            .groupby(group_cols + ["anio"], as_index=False)["pib_miles_millones_2015"]
            .sum()
        )

    annual_sector = annualize(sector_long, ["sector_code", "sector_name"])
    annual_total = annualize(total_long, ["concepto"])
    return annual_total, annual_sector


def load_geih() -> tuple[pd.DataFrame, pd.DataFrame]:
    geih = pd.read_stata(
        GEIH_DTA,
        columns=["anio", "fex", "horas", "subrama_det_cod"],
        convert_categoricals=False,
    )
    geih["anio"] = geih["anio"].astype(int)
    geih["fex"] = pd.to_numeric(geih["fex"], errors="coerce")
    geih["horas"] = pd.to_numeric(geih["horas"], errors="coerce")
    geih["subrama_det_cod"] = pd.to_numeric(geih["subrama_det_cod"], errors="coerce")

    geih = geih[
        (geih["anio"].between(2010, 2025))
        & (geih["anio"] != 2020)
        & (geih["fex"] > 0)
    ].copy()
    geih["horas_validas"] = geih["horas"].where(geih["horas"].between(1, 112))
    geih["horas_sem_expand"] = geih["fex"] * geih["horas_validas"]

    total = (
        geih.groupby("anio", as_index=False)
        .agg(
            ocupados=("fex", "sum"),
            horas_sem_expandidas=("horas_sem_expand", "sum"),
        )
        .assign(horas_anuales=lambda x: x["horas_sem_expandidas"] * 52)
    )

    geih["sector_code"] = geih["subrama_det_cod"].map(SUBRAMA_TO_SECTOR)
    sector = (
        geih.dropna(subset=["sector_code"])
        .groupby(["anio", "sector_code"], as_index=False)
        .agg(
            ocupados=("fex", "sum"),
            horas_sem_expandidas=("horas_sem_expand", "sum"),
        )
        .assign(horas_anuales=lambda x: x["horas_sem_expandidas"] * 52)
    )
    sector["sector_name_short"] = sector["sector_code"].map(SECTOR_SHORT)
    return total, sector


def build_productivity() -> tuple[pd.DataFrame, pd.DataFrame, pd.DataFrame, pd.DataFrame]:
    pib_total, pib_sector = load_pib_quarterly()
    geih_total, geih_sector = load_geih()

    total = pib_total.merge(geih_total, on="anio", how="inner")
    total = total[total["anio"] != 2020].copy()
    total["pib_pesos_2015"] = total["pib_miles_millones_2015"] * 1e9
    total["pib_por_trabajador_millones_2015"] = total["pib_pesos_2015"] / total["ocupados"] / 1e6
    total["pib_por_hora_pesos_2015"] = total["pib_pesos_2015"] / total["horas_anuales"]
    total = total.sort_values("anio")

    sector = pib_sector.merge(geih_sector, on=["anio", "sector_code"], how="inner")
    sector = sector[sector["anio"] != 2020].copy()
    sector["sector_name_short"] = sector["sector_code"].map(SECTOR_SHORT)
    sector["pib_pesos_2015"] = sector["pib_miles_millones_2015"] * 1e9
    sector["pib_por_trabajador_millones_2015"] = sector["pib_pesos_2015"] / sector["ocupados"] / 1e6
    sector["pib_por_hora_pesos_2015"] = sector["pib_pesos_2015"] / sector["horas_anuales"]
    sector["sector_order"] = sector["sector_code"].map({code: i for i, code in enumerate(SECTOR_ORDER)})
    sector = sector.sort_values(["sector_order", "anio"])

    start_year, end_year = 2010, 2025
    total_summary = pd.DataFrame(
        [
            {
                "indicador": "PIB por trabajador",
                "unidad": "Millones de pesos de 2015 por ocupado",
                "valor_2010": total.loc[total["anio"] == start_year, "pib_por_trabajador_millones_2015"].iloc[0],
                "valor_2025": total.loc[total["anio"] == end_year, "pib_por_trabajador_millones_2015"].iloc[0],
            },
            {
                "indicador": "PIB por hora trabajada",
                "unidad": "Pesos de 2015 por hora",
                "valor_2010": total.loc[total["anio"] == start_year, "pib_por_hora_pesos_2015"].iloc[0],
                "valor_2025": total.loc[total["anio"] == end_year, "pib_por_hora_pesos_2015"].iloc[0],
            },
        ]
    )
    total_summary["crecimiento_anualizado"] = total_summary.apply(
        lambda r: cagr(r["valor_2010"], r["valor_2025"], start_year, end_year),
        axis=1,
    )

    sector_summary_rows = []
    for code in SECTOR_ORDER:
        part = sector[sector["sector_code"] == code]
        if start_year not in set(part["anio"]) or end_year not in set(part["anio"]):
            continue
        start = part[part["anio"] == start_year].iloc[0]
        end = part[part["anio"] == end_year].iloc[0]
        sector_summary_rows.append(
            {
                "sector_code": code,
                "sector": SECTOR_SHORT[code],
                "pib_trabajador_2010": start["pib_por_trabajador_millones_2015"],
                "pib_trabajador_2025": end["pib_por_trabajador_millones_2015"],
                "crec_pib_trabajador": cagr(
                    start["pib_por_trabajador_millones_2015"],
                    end["pib_por_trabajador_millones_2015"],
                    start_year,
                    end_year,
                ),
                "pib_hora_2010": start["pib_por_hora_pesos_2015"],
                "pib_hora_2025": end["pib_por_hora_pesos_2015"],
                "crec_pib_hora": cagr(
                    start["pib_por_hora_pesos_2015"],
                    end["pib_por_hora_pesos_2015"],
                    start_year,
                    end_year,
                ),
            }
        )
    sector_summary = pd.DataFrame(sector_summary_rows)
    return total, total_summary, sector, sector_summary


def write_latex_tables(
    total: pd.DataFrame,
    total_summary: pd.DataFrame,
    sector_summary: pd.DataFrame,
) -> None:
    total_lines = [
        r"\begin{table}[H]",
        r"\centering",
        r"\caption{Productividad laboral agregada a partir del PIB y la GEIH, 2010--2025}",
        r"\label{tab:pib_geih_productividad_total}",
        r"\small",
        r"\begin{tabular}{lrrr}",
        r"\toprule",
        r"Indicador & 2010 & 2025 & Crec. anualizado \\",
        r"\midrule",
    ]
    for _, row in total_summary.iterrows():
        digits = 1 if row["indicador"] == "PIB por trabajador" else 0
        total_lines.append(
            f"{escape_latex(indicator_with_unit(row))} & "
            f"{fmt_num_es(row['valor_2010'], digits)} & "
            f"{fmt_num_es(row['valor_2025'], digits)} & "
            f"{fmt_pct_es(row['crecimiento_anualizado'])} \\\\"
        )
    total_lines.extend(
        [
            r"\bottomrule",
            r"\end{tabular}",
            r"\caption*{\footnotesize Nota: el PIB se expresa en pesos constantes de 2015 y se obtiene como la suma de los cuatro trimestres de cada año. El PIB por trabajador se calcula dividiendo el PIB anual por los ocupados expandidos de la GEIH. El PIB por hora divide el PIB anual por las horas anuales trabajadas, estimadas como horas semanales ponderadas por 52. Fuente: cálculos propios con DANE, PIB trimestral por el enfoque de producción, y GEIH.}",
            r"\end{table}",
        ]
    )
    (SECTION_DIR / "pib_geih_productividad_total_table.tex").write_text(
        "\n".join(total_lines), encoding="utf-8"
    )

    hours = total[total["anio"].between(2010, 2025)].copy()
    hours = hours[hours["anio"] != 2020].copy()
    hours["horas_semanales_por_trabajador"] = (
        hours["horas_anuales"] / hours["ocupados"] / 52
    )
    hours_lines = [
        r"\begin{table}[H]",
        r"\centering",
        r"\caption{Horas semanales promedio por trabajador, 2010--2025}",
        r"\label{tab:horas_semanales_promedio}",
        r"\small",
        r"\begin{tabular}{lr}",
        r"\toprule",
        r"Año & Horas semanales \\",
        r"\midrule",
    ]
    for _, row in hours.sort_values("anio").iterrows():
        hours_lines.append(
            f"{int(row['anio'])} & "
            f"{fmt_num_es(row['horas_semanales_por_trabajador'], 1)} \\\\"
        )
    hours_lines.extend(
        [
            r"\bottomrule",
            r"\end{tabular}",
            r"\caption*{\footnotesize Nota: horas semanales promedio calculadas como horas anuales totales divididas por ocupados y por 52 semanas. Se excluye 2020 por no contar con GEIH anual comparable en la base del proyecto. Fuente: cálculos propios con GEIH.}",
            r"\end{table}",
        ]
    )
    (SECTION_DIR / "horas_semanales_promedio_table.tex").write_text(
        "\n".join(hours_lines), encoding="utf-8"
    )

    start_year, end_year = 2010, 2025
    start = hours.loc[hours["anio"] == start_year].iloc[0]
    end = hours.loc[hours["anio"] == end_year].iloc[0]
    labor_summary = pd.DataFrame(
        [
            {
                "indicador": "PIB real",
                "unidad": "Billones de pesos de 2015",
                "valor_2010": start["pib_pesos_2015"] / 1e12,
                "valor_2025": end["pib_pesos_2015"] / 1e12,
                "crecimiento_anualizado": cagr(
                    start["pib_pesos_2015"],
                    end["pib_pesos_2015"],
                    start_year,
                    end_year,
                ),
            },
            {
                "indicador": "Ocupados",
                "unidad": "Millones de personas",
                "valor_2010": start["ocupados"] / 1e6,
                "valor_2025": end["ocupados"] / 1e6,
                "crecimiento_anualizado": cagr(
                    start["ocupados"], end["ocupados"], start_year, end_year
                ),
            },
            {
                "indicador": "Horas semanales por trabajador",
                "unidad": "Horas por semana",
                "valor_2010": start["horas_semanales_por_trabajador"],
                "valor_2025": end["horas_semanales_por_trabajador"],
                "crecimiento_anualizado": cagr(
                    start["horas_semanales_por_trabajador"],
                    end["horas_semanales_por_trabajador"],
                    start_year,
                    end_year,
                ),
            },
            {
                "indicador": "PIB por trabajador",
                "unidad": "Millones de pesos de 2015 por ocupado",
                "valor_2010": start["pib_por_trabajador_millones_2015"],
                "valor_2025": end["pib_por_trabajador_millones_2015"],
                "crecimiento_anualizado": cagr(
                    start["pib_por_trabajador_millones_2015"],
                    end["pib_por_trabajador_millones_2015"],
                    start_year,
                    end_year,
                ),
            },
            {
                "indicador": "PIB por hora trabajada",
                "unidad": "Miles de pesos de 2015 por hora",
                "valor_2010": start["pib_por_hora_pesos_2015"] / 1000,
                "valor_2025": end["pib_por_hora_pesos_2015"] / 1000,
                "crecimiento_anualizado": cagr(
                    start["pib_por_hora_pesos_2015"],
                    end["pib_por_hora_pesos_2015"],
                    start_year,
                    end_year,
                ),
            },
        ]
    )
    labor_summary.to_csv(
        TABLE_DIR / "ocupados_horas_resumen.csv", index=False, encoding="utf-8-sig"
    )
    labor_summary.to_csv(
        OUTPUT_TABLE_DIR / "ocupados_horas_resumen.csv",
        index=False,
        encoding="utf-8-sig",
    )

    labor_lines = [
        r"\begin{table}[H]",
        r"\centering",
        r"\caption{PIB, ocupados, horas y productividad laboral, 2010--2025}",
        r"\label{tab:ocupados_horas_resumen}",
        r"\small",
        r"\begin{tabular}{lrrr}",
        r"\toprule",
        r"Indicador & 2010 & 2025 & Crec. anualizado \\",
        r"\midrule",
    ]
    for _, row in labor_summary.iterrows():
        labor_lines.append(
            f"{escape_latex(indicator_with_unit(row))} & "
            f"{fmt_num_es(row['valor_2010'], 1)} & "
            f"{fmt_num_es(row['valor_2025'], 1)} & "
            f"{fmt_pct_es(row['crecimiento_anualizado'])} \\\\"
        )
    labor_lines.extend(
        [
            r"\bottomrule",
            r"\end{tabular}",
            r"\caption*{\footnotesize Nota: PIB real en pesos constantes de 2015. Ocupados expandidos con el factor \texttt{fex}. Las horas semanales promedio se calculan como horas anuales totales divididas por ocupados y por 52 semanas. Se excluye 2020 por no contar con GEIH anual comparable en la base del proyecto. Fuente: cálculos propios con DANE, PIB trimestral por el enfoque de producción, y GEIH.}",
            r"\end{table}",
        ]
    )
    (SECTION_DIR / "ocupados_horas_resumen_table.tex").write_text(
        "\n".join(labor_lines), encoding="utf-8"
    )

    sector_sorted = sector_summary.sort_values("crec_pib_trabajador", ascending=False)
    sector_lines = [
        r"\begin{table}[H]",
        r"\centering",
        r"\caption{Productividad laboral por sector CIIU, 2010--2025}",
        r"\label{tab:pib_geih_productividad_sector}",
        r"\scriptsize",
        r"\begin{tabular}{lrrrrrr}",
        r"\toprule",
        r"& \multicolumn{3}{c}{PIB por trabajador (millones de pesos de 2015)} & \multicolumn{3}{c}{PIB por hora (miles de pesos de 2015)} \\",
        r"\cmidrule(lr){2-4}\cmidrule(lr){5-7}",
        r"Sector & 2010 & 2025 & Crec. & 2010 & 2025 & Crec. \\",
        r"\midrule",
    ]
    for _, row in sector_sorted.iterrows():
        sector_lines.append(
            f"{escape_latex(row['sector'])} & "
            f"{fmt_num_es(row['pib_trabajador_2010'], 1)} & "
            f"{fmt_num_es(row['pib_trabajador_2025'], 1)} & "
            f"{fmt_pct_es(row['crec_pib_trabajador'])} & "
            f"{fmt_num_es(row['pib_hora_2010'] / 1000, 1)} & "
            f"{fmt_num_es(row['pib_hora_2025'] / 1000, 1)} & "
            f"{fmt_pct_es(row['crec_pib_hora'])} \\\\"
        )
    sector_lines.extend(
        [
            r"\bottomrule",
            r"\end{tabular}",
            r"\caption*{\footnotesize Nota: PIB por trabajador en millones de pesos constantes de 2015 por ocupado; PIB por hora en miles de pesos constantes de 2015 por hora trabajada. La comparación entre ambas columnas debe hacerse por tasas de crecimiento, no por niveles, porque los denominadores y las escalas son distintos. Sectores según 12 agrupaciones CIIU Rev. 4 A.C. del DANE; ocupados y horas se agregan desde GEIH usando la homologación de subramas del proyecto. Se excluyen organizaciones extraterritoriales del cruce sectorial por no hacer parte de las 12 agrupaciones. Fuente: cálculos propios con DANE y GEIH.}",
            r"\end{table}",
        ]
    )
    (SECTION_DIR / "pib_geih_productividad_sector_table.tex").write_text(
        "\n".join(sector_lines), encoding="utf-8"
    )


def fmt_corr_es(value: float) -> str:
    return f"{value:.3f}".replace(".", ",")


def write_sector_correlation_table(sector: pd.DataFrame) -> None:
    endpoints = (
        sector[sector["anio"].isin([2010, 2025])]
        .pivot(index=["sector_code", "sector_name_short"], columns="anio")
        .sort_index()
    )
    years = 2025 - 2010
    growth = pd.DataFrame(index=endpoints.index)
    growth["crec_ocupados"] = (
        endpoints[("ocupados", 2025)] / endpoints[("ocupados", 2010)]
    ) ** (1 / years) - 1
    growth["crec_horas"] = (
        endpoints[("horas_anuales", 2025)] / endpoints[("horas_anuales", 2010)]
    ) ** (1 / years) - 1
    growth["crec_productividad_hora"] = (
        endpoints[("pib_por_hora_pesos_2015", 2025)]
        / endpoints[("pib_por_hora_pesos_2015", 2010)]
    ) ** (1 / years) - 1
    growth["crec_productividad_trabajador"] = (
        endpoints[("pib_por_trabajador_millones_2015", 2025)]
        / endpoints[("pib_por_trabajador_millones_2015", 2010)]
    ) ** (1 / years) - 1
    growth = growth.reset_index()

    corr_vars = [
        "crec_ocupados",
        "crec_horas",
        "crec_productividad_hora",
        "crec_productividad_trabajador",
    ]
    labels = {
        "crec_ocupados": "Ocupados",
        "crec_horas": "Horas totales",
        "crec_productividad_hora": "Prod. por hora",
        "crec_productividad_trabajador": "Prod. por trabajador",
    }
    corr = growth[corr_vars].corr()
    corr.index = [labels[col] for col in corr.index]
    corr.columns = [labels[col] for col in corr.columns]

    growth.to_csv(
        TABLE_DIR / "pib_geih_productividad_sector_correlaciones_base.csv",
        index=False,
        encoding="utf-8-sig",
    )
    growth.to_csv(
        OUTPUT_TABLE_DIR / "pib_geih_productividad_sector_correlaciones_base.csv",
        index=False,
        encoding="utf-8-sig",
    )
    corr.to_csv(
        TABLE_DIR / "pib_geih_productividad_sector_correlaciones.csv",
        encoding="utf-8-sig",
    )
    corr.to_csv(
        OUTPUT_TABLE_DIR / "pib_geih_productividad_sector_correlaciones.csv",
        encoding="utf-8-sig",
    )

    lines = [
        r"\begin{table}[H]",
        r"\centering",
        r"\caption{Correlaciones entre crecimientos sectoriales, 2010--2025}",
        r"\label{tab:pib_geih_productividad_sector_correlaciones}",
        r"\scriptsize",
        r"\begin{tabular}{lrrrr}",
        r"\toprule",
        r"Variable & Ocupados & Horas totales & Prod. por hora & Prod. por trabajador \\",
        r"\midrule",
    ]
    for row_label, row in corr.iterrows():
        lines.append(
            f"{escape_latex(row_label)} & "
            f"{fmt_corr_es(row['Ocupados'])} & "
            f"{fmt_corr_es(row['Horas totales'])} & "
            f"{fmt_corr_es(row['Prod. por hora'])} & "
            f"{fmt_corr_es(row['Prod. por trabajador'])} \\\\"
        )
    lines.extend(
        [
            r"\bottomrule",
            r"\end{tabular}",
            r"\caption*{\footnotesize Nota: correlaciones de Pearson calculadas entre las tasas de crecimiento anualizado 2010--2025 de las 12 agrupaciones sectoriales CIIU. Las horas corresponden al total anual de horas trabajadas por sector, estimado a partir de GEIH como horas semanales ponderadas por el factor de expansi\'on y multiplicadas por 52. Fuente: c\'alculos propios con DANE y GEIH.}",
            r"\end{table}",
        ]
    )
    (SECTION_DIR / "pib_geih_productividad_sector_correlaciones.tex").write_text(
        "\n".join(lines), encoding="utf-8"
    )


def classify_growth(value: float) -> str:
    if pd.isna(value):
        return "sin información suficiente"
    if value >= 0.03:
        return "alto"
    if value >= 0.01:
        return "moderado"
    if value >= 0:
        return "bajo"
    return "negativo"


def build_metric_rows(start: pd.Series, end: pd.Series) -> pd.DataFrame:
    start_year = int(start["anio"])
    end_year = int(end["anio"])
    start_hours = start["horas_anuales"] / start["ocupados"] / 52
    end_hours = end["horas_anuales"] / end["ocupados"] / 52
    rows = [
        {
            "indicador": "PIB real",
            "unidad": "Billones de pesos de 2015",
            "valor_2010": start["pib_pesos_2015"] / 1e12,
            "valor_2025": end["pib_pesos_2015"] / 1e12,
            "crecimiento_anualizado": cagr(
                start["pib_pesos_2015"], end["pib_pesos_2015"], start_year, end_year
            ),
        },
        {
            "indicador": "Ocupados",
            "unidad": "Millones de personas",
            "valor_2010": start["ocupados"] / 1e6,
            "valor_2025": end["ocupados"] / 1e6,
            "crecimiento_anualizado": cagr(
                start["ocupados"], end["ocupados"], start_year, end_year
            ),
        },
        {
            "indicador": "Horas semanales por trabajador",
            "unidad": "Horas por semana",
            "valor_2010": start_hours,
            "valor_2025": end_hours,
            "crecimiento_anualizado": cagr(start_hours, end_hours, start_year, end_year),
        },
        {
            "indicador": "PIB por trabajador",
            "unidad": "Millones de pesos de 2015 por ocupado",
            "valor_2010": start["pib_por_trabajador_millones_2015"],
            "valor_2025": end["pib_por_trabajador_millones_2015"],
            "crecimiento_anualizado": cagr(
                start["pib_por_trabajador_millones_2015"],
                end["pib_por_trabajador_millones_2015"],
                start_year,
                end_year,
            ),
        },
        {
            "indicador": "PIB por hora trabajada",
            "unidad": "Miles de pesos de 2015 por hora",
            "valor_2010": start["pib_por_hora_pesos_2015"] / 1000,
            "valor_2025": end["pib_por_hora_pesos_2015"] / 1000,
            "crecimiento_anualizado": cagr(
                start["pib_por_hora_pesos_2015"],
                end["pib_por_hora_pesos_2015"],
                start_year,
                end_year,
            ),
        },
    ]
    return pd.DataFrame(rows)


def metric_table_lines(metrics: pd.DataFrame, label: str, caption: str) -> list[str]:
    lines = [
        r"\begin{table}[H]",
        r"\centering",
        f"\\caption{{{escape_latex(caption)}}}",
        f"\\label{{{label}}}",
        r"\scriptsize",
        r"\begin{tabular}{lrrr}",
        r"\toprule",
        r"Indicador & 2010 & 2025 & Crec. anualizado \\",
        r"\midrule",
    ]
    for _, row in metrics.iterrows():
        lines.append(
            f"{escape_latex(indicator_with_unit(row))} & "
            f"{fmt_num_es(row['valor_2010'], 1)} & "
            f"{fmt_num_es(row['valor_2025'], 1)} & "
            f"{fmt_pct_es(row['crecimiento_anualizado'])} \\\\"
        )
    lines.extend([r"\bottomrule", r"\end{tabular}", r"\end{table}"])
    return lines


def write_sector_detail_sections(sector: pd.DataFrame) -> None:
    detail_rows = []
    lines = [
        "A continuación se presenta el mismo ejercicio para cada una de las doce agrupaciones CIIU. En cada caso, el cuadro resume el PIB real sectorial, el número de ocupados, las horas semanales promedio por trabajador, el PIB por trabajador y el PIB por hora trabajada. La lectura conjunta de estas variables permite distinguir si los cambios de productividad responden principalmente al dinamismo del valor agregado, a variaciones en el empleo, a cambios en las horas trabajadas o a una combinación de estos factores.",
        "",
    ]

    for code in SECTOR_ORDER:
        part = sector[sector["sector_code"] == code].sort_values("anio")
        if 2010 not in set(part["anio"]) or 2025 not in set(part["anio"]):
            continue
        start = part[part["anio"] == 2010].iloc[0]
        end = part[part["anio"] == 2025].iloc[0]
        metrics = build_metric_rows(start, end)
        metrics["sector_code"] = code
        metrics["sector"] = SECTOR_SHORT[code]
        detail_rows.append(metrics)

        pib_growth = metrics.loc[
            metrics["indicador"].eq("PIB real"), "crecimiento_anualizado"
        ].iloc[0]
        emp_growth = metrics.loc[
            metrics["indicador"].eq("Ocupados"), "crecimiento_anualizado"
        ].iloc[0]
        hours_growth = metrics.loc[
            metrics["indicador"].eq("Horas semanales por trabajador"),
            "crecimiento_anualizado",
        ].iloc[0]
        worker_growth = metrics.loc[
            metrics["indicador"].eq("PIB por trabajador"),
            "crecimiento_anualizado",
        ].iloc[0]
        hour_growth = metrics.loc[
            metrics["indicador"].eq("PIB por hora trabajada"),
            "crecimiento_anualizado",
        ].iloc[0]

        relation = (
            "por encima"
            if hour_growth > worker_growth
            else "por debajo"
            if hour_growth < worker_growth
            else "en línea"
        )
        hours_text = (
            "una reducción de las horas semanales promedio"
            if hours_growth < 0
            else "un aumento de las horas semanales promedio"
            if hours_growth > 0
            else "estabilidad en las horas semanales promedio"
        )

        lines.extend(
            [
                f"\\subsubsection{{{escape_latex(SECTOR_SHORT[code])}}}",
                "",
                f"Esta agrupación incluye {SECTOR_DESCRIPTION[code]}.",
                "",
                *metric_table_lines(
                    metrics,
                    f"tab:sector_{code.lower().replace('+', '_')}_productividad",
                    f"{SECTOR_SHORT[code]}: PIB, ocupados, horas y productividad laboral, 2010--2025",
                ),
                "",
                (
                    f"\\textbf{{En {SECTOR_SHORT[code].lower()}, el crecimiento del PIB real fue "
                    f"{classify_growth(pib_growth)} y el del PIB por trabajador fue "
                    f"{classify_growth(worker_growth)}.}} "
                    f"Entre 2010 y 2025, el PIB real sectorial varió a una tasa anualizada de "
                    f"{fmt_pct_es(pib_growth)}, mientras que el número de ocupados lo hizo a "
                    f"{fmt_pct_es(emp_growth)}. Como resultado, el PIB por trabajador varió "
                    f"{fmt_pct_es(worker_growth)} anual. El PIB por hora se ubicó {relation} de "
                    f"esa dinámica, con una tasa de {fmt_pct_es(hour_growth)}, en un contexto de "
                    f"{hours_text} ({fmt_pct_es(hours_growth)} anual)."
                ),
                "",
            ]
        )

    if detail_rows:
        detail = pd.concat(detail_rows, ignore_index=True)
        detail.to_csv(
            TABLE_DIR / "pib_geih_productividad_sector_detalle.csv",
            index=False,
            encoding="utf-8-sig",
        )
        detail.to_csv(
            OUTPUT_TABLE_DIR / "pib_geih_productividad_sector_detalle.csv",
            index=False,
            encoding="utf-8-sig",
        )

    lines.append(
        r"{\footnotesize Nota general: PIB real en pesos constantes de 2015. Ocupados expandidos con el factor \texttt{fex}. Las horas semanales promedio se calculan como horas anuales totales divididas por ocupados y por 52 semanas. Se excluye 2020 por no contar con GEIH anual comparable en la base del proyecto. Fuente: cálculos propios con DANE, PIB trimestral por el enfoque de producción, y GEIH.}"
    )
    (SECTION_DIR / "pib_geih_productividad_sector_detalle.tex").write_text(
        "\n".join(lines), encoding="utf-8"
    )


def draw_index_chart(total: pd.DataFrame) -> None:
    data = total[total["anio"].between(2010, 2025)].copy()
    data = data[data["anio"] != 2020]
    data["idx_worker"] = (
        data["pib_por_trabajador_millones_2015"]
        / data.loc[data["anio"] == 2010, "pib_por_trabajador_millones_2015"].iloc[0]
        * 100
    )
    data["idx_hour"] = (
        data["pib_por_hora_pesos_2015"]
        / data.loc[data["anio"] == 2010, "pib_por_hora_pesos_2015"].iloc[0]
        * 100
    )

    img = Image.new("RGB", (1400, 820), "white")
    draw = ImageDraw.Draw(img)
    font = ImageFont.load_default()
    title_font = ImageFont.truetype("arial.ttf", 30) if Path(r"C:\Windows\Fonts\arial.ttf").exists() else font
    label_font = ImageFont.truetype("arial.ttf", 20) if Path(r"C:\Windows\Fonts\arial.ttf").exists() else font

    left, top, right, bottom = 120, 110, 1150, 690
    draw.text((left, 35), "Productividad laboral agregada: índice 2010 = 100", fill="#222222", font=title_font)
    draw.text((left, 73), "PIB por trabajador y PIB por hora trabajada, pesos constantes de 2015", fill="#555555", font=label_font)

    years = data["anio"].tolist()
    ymin = math.floor(min(data["idx_worker"].min(), data["idx_hour"].min()) / 5) * 5
    ymax = math.ceil(max(data["idx_worker"].max(), data["idx_hour"].max()) / 5) * 5

    def x_pos(year):
        return left + (year - 2010) / (2025 - 2010) * (right - left)

    def y_pos(value):
        return bottom - (value - ymin) / (ymax - ymin) * (bottom - top)

    for tick in range(ymin, ymax + 1, 5):
        y = y_pos(tick)
        draw.line((left, y, right, y), fill="#e7e7e7", width=1)
        draw.text((55, y - 10), str(tick), fill="#555555", font=label_font)
    draw.line((left, top, left, bottom), fill="#333333", width=2)
    draw.line((left, bottom, right, bottom), fill="#333333", width=2)

    for year in range(2010, 2026, 3):
        x = x_pos(year)
        draw.line((x, bottom, x, bottom + 8), fill="#333333", width=2)
        draw.text((x - 22, bottom + 16), str(year), fill="#555555", font=label_font)

    def draw_series(values, color):
        points = [(x_pos(y), y_pos(v)) for y, v in zip(years, values)]
        for p1, p2 in zip(points, points[1:]):
            draw.line((p1[0], p1[1], p2[0], p2[1]), fill=color, width=5)
        for x, y in points:
            draw.ellipse((x - 5, y - 5, x + 5, y + 5), fill=color)
        return points[-1]

    end_worker = draw_series(data["idx_worker"].tolist(), "#1f77b4")
    end_hour = draw_series(data["idx_hour"].tolist(), "#d95f02")
    draw.text((end_worker[0] + 10, end_worker[1] - 14), "PIB por trabajador", fill="#1f77b4", font=label_font)
    draw.text((end_hour[0] + 10, end_hour[1] - 14), "PIB por hora", fill="#d95f02", font=label_font)
    draw.text((left, 748), "Nota: 2020 no aparece porque no hay GEIH anual comparable en la base del proyecto.", fill="#555555", font=label_font)

    for directory in [FIGURE_DIR, OUTPUT_FIGURE_DIR]:
        img.save(directory / "fig_pib_geih_productividad_total.png")


def draw_sector_cagr_chart(sector_summary: pd.DataFrame) -> None:
    data = sector_summary.sort_values("crec_pib_trabajador", ascending=True).reset_index(drop=True)
    img = Image.new("RGB", (1600, 1050), "white")
    draw = ImageDraw.Draw(img)
    font = ImageFont.load_default()
    title_font = ImageFont.truetype("arial.ttf", 30) if Path(r"C:\Windows\Fonts\arial.ttf").exists() else font
    label_font = ImageFont.truetype("arial.ttf", 18) if Path(r"C:\Windows\Fonts\arial.ttf").exists() else font

    draw.text((80, 35), "Crecimiento anualizado de la productividad laboral por sector, 2010--2025", fill="#222222", font=title_font)
    draw.text((80, 73), "PIB por trabajador y PIB por hora trabajada", fill="#555555", font=label_font)

    left, top, right, bottom = 520, 150, 1480, 900
    max_abs = max(abs(data["crec_pib_trabajador"]).max(), abs(data["crec_pib_hora"]).max())
    xmin, xmax = -max_abs * 1.15, max_abs * 1.15
    zero_x = left + (0 - xmin) / (xmax - xmin) * (right - left)
    draw.line((zero_x, top - 10, zero_x, bottom + 10), fill="#555555", width=2)

    for pct in np.arange(math.floor(xmin * 100), math.ceil(xmax * 100) + 1, 1):
        value = pct / 100
        x = left + (value - xmin) / (xmax - xmin) * (right - left)
        color = "#eeeeee" if pct != 0 else "#555555"
        draw.line((x, top - 5, x, bottom + 5), fill=color, width=1)
        if pct % 2 == 0:
            draw.text((x - 18, bottom + 20), f"{pct}%", fill="#555555", font=label_font)

    row_h = (bottom - top) / len(data)
    for i, row in data.iterrows():
        y = top + i * row_h + row_h / 2
        draw.text((80, y - 12), row["sector"], fill="#333333", font=label_font)
        for value, offset, color in [
            (row["crec_pib_trabajador"], -8, "#1f77b4"),
            (row["crec_pib_hora"], 8, "#d95f02"),
        ]:
            x = left + (value - xmin) / (xmax - xmin) * (right - left)
            draw.line((zero_x, y + offset, x, y + offset), fill=color, width=8)
            draw.ellipse((x - 5, y + offset - 5, x + 5, y + offset + 5), fill=color)

    draw.rectangle((1080, 78, 1105, 98), fill="#1f77b4")
    draw.text((1115, 75), "PIB por trabajador", fill="#333333", font=label_font)
    draw.rectangle((1320, 78, 1345, 98), fill="#d95f02")
    draw.text((1355, 75), "PIB por hora", fill="#333333", font=label_font)

    for directory in [FIGURE_DIR, OUTPUT_FIGURE_DIR]:
        img.save(directory / "fig_pib_geih_productividad_sector.png")


def draw_sector_correlation_scatter(sector: pd.DataFrame) -> None:
    endpoints = (
        sector[sector["anio"].isin([2010, 2025])]
        .pivot(index=["sector_code", "sector_name_short"], columns="anio")
        .sort_index()
    )
    years = 2025 - 2010
    data = pd.DataFrame(index=endpoints.index)
    data["ocupados"] = (
        endpoints[("ocupados", 2025)] / endpoints[("ocupados", 2010)]
    ) ** (1 / years) - 1
    data["horas"] = (
        endpoints[("horas_anuales", 2025)] / endpoints[("horas_anuales", 2010)]
    ) ** (1 / years) - 1
    data["prod_trabajador"] = (
        endpoints[("pib_por_trabajador_millones_2015", 2025)]
        / endpoints[("pib_por_trabajador_millones_2015", 2010)]
    ) ** (1 / years) - 1
    data["prod_hora"] = (
        endpoints[("pib_por_hora_pesos_2015", 2025)]
        / endpoints[("pib_por_hora_pesos_2015", 2010)]
    ) ** (1 / years) - 1
    data = data.reset_index()

    img = Image.new("RGB", (1800, 1250), "white")
    draw = ImageDraw.Draw(img)
    font = ImageFont.load_default()
    title_font = ImageFont.truetype("arial.ttf", 32) if Path(r"C:\Windows\Fonts\arial.ttf").exists() else font
    label_font = ImageFont.truetype("arial.ttf", 20) if Path(r"C:\Windows\Fonts\arial.ttf").exists() else font
    small_font = ImageFont.truetype("arial.ttf", 17) if Path(r"C:\Windows\Fonts\arial.ttf").exists() else font

    draw.text((70, 35), "Crecimiento sectorial del trabajo y la productividad, 2010--2025", fill="#222222", font=title_font)
    draw.text((70, 78), "Tasas anualizadas por agrupación CIIU; cada punto representa un sector", fill="#555555", font=label_font)

    panels = [
        (90, 150, 850, 600, "ocupados", "prod_trabajador", "Ocupados", "PIB por trabajador"),
        (1010, 150, 1770, 600, "ocupados", "prod_hora", "Ocupados", "PIB por hora"),
        (90, 720, 850, 1170, "horas", "prod_trabajador", "Horas totales", "PIB por trabajador"),
        (1010, 720, 1770, 1170, "horas", "prod_hora", "Horas totales", "PIB por hora"),
    ]

    x_min = math.floor(min(data["ocupados"].min(), data["horas"].min()) * 100) / 100 - 0.01
    x_max = math.ceil(max(data["ocupados"].max(), data["horas"].max()) * 100) / 100 + 0.01
    y_min = math.floor(min(data["prod_trabajador"].min(), data["prod_hora"].min()) * 100) / 100 - 0.01
    y_max = math.ceil(max(data["prod_trabajador"].max(), data["prod_hora"].max()) * 100) / 100 + 0.01

    def pct_label(value: float) -> str:
        return f"{value * 100:.0f}%"

    for left, top, right, bottom, x_col, y_col, x_lab, y_lab in panels:
        plot_left, plot_top = left + 90, top + 45
        plot_right, plot_bottom = right - 35, bottom - 70

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
            draw.text((x - 18, plot_bottom + 12), pct_label(value), fill="#555555", font=small_font)
        for tick in np.arange(math.ceil(y_min * 100), math.floor(y_max * 100) + 1, 2):
            value = tick / 100
            y = y_pos(value)
            draw.line((plot_left, y, plot_right, y), fill="#eeeeee", width=1)
            draw.text((plot_left - 58, y - 10), pct_label(value), fill="#555555", font=small_font)

        if x_min < 0 < x_max:
            x0 = x_pos(0)
            draw.line((x0, plot_top, x0, plot_bottom), fill="#999999", width=2)
        if y_min < 0 < y_max:
            y0 = y_pos(0)
            draw.line((plot_left, y0, plot_right, y0), fill="#999999", width=2)

        xs = data[x_col].astype(float).to_numpy()
        ys = data[y_col].astype(float).to_numpy()
        slope, intercept = np.polyfit(xs, ys, 1)
        x1, x2 = x_min, x_max
        draw.line((x_pos(x1), y_pos(slope * x1 + intercept), x_pos(x2), y_pos(slope * x2 + intercept)), fill="#b44b3f", width=3)

        for _, row in data.iterrows():
            x = x_pos(row[x_col])
            y = y_pos(row[y_col])
            draw.ellipse((x - 7, y - 7, x + 7, y + 7), fill="#1f77b4", outline="white", width=2)
            draw.text((x + 9, y - 10), row["sector_code"], fill="#333333", font=small_font)

        corr = data[[x_col, y_col]].corr().iloc[0, 1]
        draw.text((plot_left + 10, plot_top + 10), f"r = {corr:.2f}", fill="#b44b3f", font=label_font)
        draw.text(((plot_left + plot_right) / 2 - 90, bottom - 38), f"Crec. {x_lab.lower()}", fill="#333333", font=small_font)

    draw.text((70, 1210), "Nota: la línea roja muestra la tendencia lineal simple entre sectores.", fill="#555555", font=small_font)

    for directory in [FIGURE_DIR, OUTPUT_FIGURE_DIR]:
        img.save(directory / "fig_pib_geih_productividad_sector_correlaciones.png")


def main() -> None:
    total, total_summary, sector, sector_summary = build_productivity()

    total.to_csv(TABLE_DIR / "pib_geih_productividad_total_series.csv", index=False, encoding="utf-8-sig")
    total.to_csv(OUTPUT_TABLE_DIR / "pib_geih_productividad_total_series.csv", index=False, encoding="utf-8-sig")
    horas_promedio = total[["anio", "ocupados", "horas_anuales"]].copy()
    horas_promedio["horas_semanales_por_trabajador"] = (
        horas_promedio["horas_anuales"] / horas_promedio["ocupados"] / 52
    )
    horas_promedio.to_csv(TABLE_DIR / "horas_semanales_promedio.csv", index=False, encoding="utf-8-sig")
    horas_promedio.to_csv(OUTPUT_TABLE_DIR / "horas_semanales_promedio.csv", index=False, encoding="utf-8-sig")
    total_summary.to_csv(TABLE_DIR / "pib_geih_productividad_total_summary.csv", index=False, encoding="utf-8-sig")
    total_summary.to_csv(OUTPUT_TABLE_DIR / "pib_geih_productividad_total_summary.csv", index=False, encoding="utf-8-sig")
    sector.to_csv(TABLE_DIR / "pib_geih_productividad_sector_series.csv", index=False, encoding="utf-8-sig")
    sector.to_csv(OUTPUT_TABLE_DIR / "pib_geih_productividad_sector_series.csv", index=False, encoding="utf-8-sig")
    sector_summary.to_csv(TABLE_DIR / "pib_geih_productividad_sector_summary.csv", index=False, encoding="utf-8-sig")
    sector_summary.to_csv(OUTPUT_TABLE_DIR / "pib_geih_productividad_sector_summary.csv", index=False, encoding="utf-8-sig")

    write_latex_tables(total, total_summary, sector_summary)
    write_sector_correlation_table(sector)
    write_sector_detail_sections(sector)
    draw_index_chart(total)
    draw_sector_cagr_chart(sector_summary)
    draw_sector_correlation_scatter(sector)

    print("Resumen total")
    print(total_summary.to_string(index=False))
    print("\nSectores ordenados por crecimiento de PIB por trabajador")
    print(sector_summary.sort_values("crec_pib_trabajador", ascending=False).to_string(index=False))


if __name__ == "__main__":
    main()
