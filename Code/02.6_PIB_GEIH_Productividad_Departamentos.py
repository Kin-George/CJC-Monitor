from __future__ import annotations

import math
import os
from pathlib import Path

import numpy as np
import pandas as pd
from PIL import Image, ImageDraw, ImageFont


PROJECT_ROOT = Path(__file__).resolve().parents[1]
PIB_DEP_XLSX = Path(
    os.environ.get(
        "PIB_DEP_XLSX",
        r"C:\Users\olive\Downloads\anex-PIBDep-Regiones-2024pr.xlsx",
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


DEPARTMENT_NAMES = {
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

REGION_NAMES = {"CARIBE", "ORIENTAL", "CENTRAL", "PACÍFICA", "AMAZONÍA - ORINOQUÍA", "COLOMBIA"}


def clean_divipola(value: object) -> int | None:
    if pd.isna(value):
        return None
    text = str(value).strip()
    if not text:
        return None
    try:
        return int(float(text))
    except ValueError:
        return None


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
    return (
        str(text)
        .lower()
        .replace(" ", "_")
        .replace(".", "")
        .replace("á", "a")
        .replace("é", "e")
        .replace("í", "i")
        .replace("ó", "o")
        .replace("ú", "u")
        .replace("ñ", "n")
    )


def load_pib_departamental() -> pd.DataFrame:
    raw = pd.read_excel(PIB_DEP_XLSX, sheet_name="Cuadro 2", header=None)
    block_start = raw.index[
        raw.iloc[:, 0].astype(str).str.contains("Producto Interno Bruto por regiones", case=False, na=False)
    ].max()
    header_idx = block_start + 4
    years = {col: parse_year(raw.iat[header_idx, col]) for col in range(2, raw.shape[1])}
    year_cols = [col for col, year in years.items() if year is not None]

    rows = []
    for idx in range(header_idx + 1, len(raw)):
        code = clean_divipola(raw.iat[idx, 0])
        name = raw.iat[idx, 1]
        if isinstance(raw.iat[idx, 0], str) and "Fuente:" in raw.iat[idx, 0]:
            break
        if code is None or code not in DEPARTMENT_NAMES:
            continue
        for col in year_cols:
            value = pd.to_numeric(raw.iat[idx, col], errors="coerce")
            if pd.notna(value):
                rows.append(
                    {
                        "anio": years[col],
                        "depto": code,
                        "departamento": DEPARTMENT_NAMES.get(code, str(name).strip()),
                        "pib_miles_millones_2015": float(value),
                    }
                )
    pib = pd.DataFrame(rows)
    return pib[pib["anio"].between(2014, 2024)].copy()


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
        geih["anio"].between(2014, 2024)
        & (geih["anio"] != 2020)
        & geih["depto"].isin(DEPARTMENT_NAMES)
        & (geih["fex"] > 0)
    ].copy()
    geih["horas_validas"] = geih["horas"].where(geih["horas"].between(1, 112))
    geih["horas_sem_expand"] = geih["fex"] * geih["horas_validas"]
    dep = (
        geih.groupby(["anio", "depto"], as_index=False)
        .agg(ocupados=("fex", "sum"), horas_sem_expandidas=("horas_sem_expand", "sum"))
        .assign(horas_anuales=lambda x: x["horas_sem_expandidas"] * 52)
    )
    dep["departamento"] = dep["depto"].map(DEPARTMENT_NAMES)
    return dep


def build_productivity_departamental() -> tuple[pd.DataFrame, pd.DataFrame]:
    pib = load_pib_departamental()
    labor = load_geih_departamental()
    data = pib.merge(labor, on=["anio", "depto", "departamento"], how="inner")
    data["pib_pesos_2015"] = data["pib_miles_millones_2015"] * 1e9
    data["pib_por_trabajador_millones_2015"] = data["pib_pesos_2015"] / data["ocupados"] / 1e6
    data["pib_por_hora_pesos_2015"] = data["pib_pesos_2015"] / data["horas_anuales"]
    data["horas_semanales_por_trabajador"] = data["horas_anuales"] / data["ocupados"] / 52
    data = data.sort_values(["departamento", "anio"])

    start_year, end_year = 2014, 2024
    rows = []
    for depto, part in data.groupby("depto"):
        start = part[part["anio"] == start_year]
        end = part[part["anio"] == end_year]
        if start.empty or end.empty:
            continue
        start = start.iloc[0]
        end = end.iloc[0]
        rows.append(
            {
                "depto": depto,
                "departamento": end["departamento"],
                "pib_2014": start["pib_miles_millones_2015"],
                "pib_2024": end["pib_miles_millones_2015"],
                "ocupados_2014": start["ocupados"],
                "ocupados_2024": end["ocupados"],
                "horas_2014": start["horas_anuales"],
                "horas_2024": end["horas_anuales"],
                "horas_sem_2014": start["horas_semanales_por_trabajador"],
                "horas_sem_2024": end["horas_semanales_por_trabajador"],
                "pib_trabajador_2014": start["pib_por_trabajador_millones_2015"],
                "pib_trabajador_2024": end["pib_por_trabajador_millones_2015"],
                "pib_hora_2014": start["pib_por_hora_pesos_2015"],
                "pib_hora_2024": end["pib_por_hora_pesos_2015"],
                "crec_pib": cagr(start["pib_miles_millones_2015"], end["pib_miles_millones_2015"], start_year, end_year),
                "crec_ocupados": cagr(start["ocupados"], end["ocupados"], start_year, end_year),
                "crec_horas": cagr(start["horas_anuales"], end["horas_anuales"], start_year, end_year),
                "crec_horas_por_trabajador": cagr(
                    start["horas_semanales_por_trabajador"],
                    end["horas_semanales_por_trabajador"],
                    start_year,
                    end_year,
                ),
                "crec_pib_trabajador": cagr(
                    start["pib_por_trabajador_millones_2015"],
                    end["pib_por_trabajador_millones_2015"],
                    start_year,
                    end_year,
                ),
                "crec_pib_hora": cagr(
                    start["pib_por_hora_pesos_2015"],
                    end["pib_por_hora_pesos_2015"],
                    start_year,
                    end_year,
                ),
            }
        )
    summary = pd.DataFrame(rows).sort_values("crec_pib_hora", ascending=False)
    return data, summary


def write_summary_table(summary: pd.DataFrame) -> None:
    table = summary.sort_values("crec_pib_hora", ascending=False)
    lines = [
        r"\begin{table}[H]",
        r"\centering",
        r"\caption{Productividad laboral por departamento, 2014--2024pr}",
        r"\label{tab:pib_geih_productividad_departamento}",
        r"\scriptsize",
        r"\begin{tabular}{lrrrr}",
        r"\toprule",
        r"Departamento & PIB/trab. 2024 & PIB/hora 2024 & Crec. PIB/trab. & Crec. PIB/hora \\",
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
            r"\caption*{\footnotesize Nota: PIB por trabajador en millones de pesos constantes de 2015; PIB por hora en miles de pesos constantes de 2015. Bogotá se trata como departamento. Se excluye 2020 por no contar con GEIH anual comparable en la base del proyecto. Fuente: cálculos propios con DANE, Cuentas Nacionales Departamentales, y GEIH.}",
            r"\end{table}",
        ]
    )
    (SECTION_DIR / "pib_geih_productividad_departamento_table.tex").write_text(
        "\n".join(lines), encoding="utf-8"
    )


def metric_rows(start: pd.Series, end: pd.Series) -> list[tuple[str, str, str, str]]:
    return [
        (
            "PIB real",
            fmt_num_es(start["pib_miles_millones_2015"] / 1000, 1),
            fmt_num_es(end["pib_miles_millones_2015"] / 1000, 1),
            fmt_pct_es(cagr(start["pib_miles_millones_2015"], end["pib_miles_millones_2015"], 2014, 2024), 2),
        ),
        (
            "Ocupados",
            fmt_num_es(start["ocupados"] / 1e6, 2),
            fmt_num_es(end["ocupados"] / 1e6, 2),
            fmt_pct_es(cagr(start["ocupados"], end["ocupados"], 2014, 2024), 2),
        ),
        (
            "Horas semanales por trabajador",
            fmt_num_es(start["horas_semanales_por_trabajador"], 1),
            fmt_num_es(end["horas_semanales_por_trabajador"], 1),
            fmt_pct_es(
                cagr(start["horas_semanales_por_trabajador"], end["horas_semanales_por_trabajador"], 2014, 2024),
                2,
            ),
        ),
        (
            "PIB por trabajador",
            fmt_num_es(start["pib_por_trabajador_millones_2015"], 1),
            fmt_num_es(end["pib_por_trabajador_millones_2015"], 1),
            fmt_pct_es(
                cagr(start["pib_por_trabajador_millones_2015"], end["pib_por_trabajador_millones_2015"], 2014, 2024),
                2,
            ),
        ),
        (
            "PIB por hora",
            fmt_num_es(start["pib_por_hora_pesos_2015"] / 1000, 1),
            fmt_num_es(end["pib_por_hora_pesos_2015"] / 1000, 1),
            fmt_pct_es(cagr(start["pib_por_hora_pesos_2015"], end["pib_por_hora_pesos_2015"], 2014, 2024), 2),
        ),
    ]


def write_detail_section(data: pd.DataFrame, summary: pd.DataFrame) -> None:
    lines = [
        r"\textbf{A continuación se presenta la descomposición departamental del crecimiento de la productividad laboral.} Para cada departamento se reportan el PIB real, el número de ocupados, las horas semanales por trabajador, el PIB por trabajador y el PIB por hora trabajada al inicio y al final del periodo disponible.",
        "",
    ]
    ordered = summary.sort_values("crec_pib_hora", ascending=False)
    for _, row in ordered.iterrows():
        part = data[data["depto"] == row["depto"]].sort_values("anio")
        start = part[part["anio"] == 2014].iloc[0]
        end = part[part["anio"] == 2024].iloc[0]
        name = row["departamento"]
        lines.extend(
            [
                rf"\subsection{{{escape_latex(name)}}}",
                rf"\textbf{{Entre 2014 y 2024, el PIB por hora trabajada de {escape_latex(name)} creció {fmt_pct_es(row['crec_pib_hora'], 2)} anual.}} El PIB por trabajador creció {fmt_pct_es(row['crec_pib_trabajador'], 2)} anual, mientras que las horas semanales por trabajador cambiaron {fmt_pct_es(row['crec_horas_por_trabajador'], 2)} anual. Esta diferencia muestra si la productividad medida por trabajador se mueve en línea con la productividad por hora o si está afectada por cambios en la intensidad horaria.",
                r"\begin{table}[H]",
                r"\centering",
                rf"\caption{{{escape_latex(name)}: PIB, ocupados, horas y productividad laboral, 2014--2024pr}}",
                rf"\label{{tab:departamento_{latex_id(name)}_productividad}}",
                r"\scriptsize",
                r"\begin{tabular}{lrrr}",
                r"\toprule",
                r"Indicador & 2014 & 2024pr & Crec. anual \\",
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
        ("PIB por hora inicial", "Crec. PIB por hora", "pib_hora_2014", "crec_pib_hora"),
    ]
    lines = [
        r"\begin{table}[H]",
        r"\centering",
        r"\caption{Correlaciones departamentales entre crecimiento del trabajo y crecimiento de la productividad}",
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
            r"\caption*{\footnotesize Nota: correlaciones de Pearson calculadas entre departamentos, tratando a Bogotá como departamento. Fuente: cálculos propios con DANE y GEIH.}",
            r"\end{table}",
        ]
    )
    pd.DataFrame(rows).to_csv(TABLE_DIR / "pib_geih_productividad_departamento_correlaciones.csv", index=False, encoding="utf-8-sig")
    pd.DataFrame(rows).to_csv(OUTPUT_TABLE_DIR / "pib_geih_productividad_departamento_correlaciones.csv", index=False, encoding="utf-8-sig")
    (SECTION_DIR / "pib_geih_productividad_departamento_correlaciones.tex").write_text(
        "\n".join(lines), encoding="utf-8"
    )


def fonts() -> tuple[ImageFont.ImageFont, ImageFont.ImageFont, ImageFont.ImageFont, ImageFont.ImageFont]:
    font = ImageFont.load_default()
    arial = Path(r"C:\Windows\Fonts\arial.ttf")
    if arial.exists():
        return (
            ImageFont.truetype(str(arial), 52),
            ImageFont.truetype(str(arial), 34),
            ImageFont.truetype(str(arial), 28),
            ImageFont.truetype(str(arial), 24),
        )
    return font, font, font, font


def draw_department_growth_chart(summary: pd.DataFrame) -> None:
    data = summary.sort_values("crec_pib_hora", ascending=True).reset_index(drop=True)
    title_font, label_font, small_font, note_font = fonts()
    img = Image.new("RGB", (1900, 1700), "white")
    draw = ImageDraw.Draw(img)
    draw.text((80, 45), "Crecimiento anualizado de la productividad laboral por departamento, 2014--2024pr", fill="#222222", font=title_font)
    draw.text((80, 105), "Bogotá se trata como departamento; PIB departamental real y trabajo GEIH", fill="#555555", font=label_font)
    left, top, right, bottom = 600, 190, 1780, 1530
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
        draw.text((x - 26, bottom + 20), f"{tick}%", fill="#555555", font=small_font)
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

    draw.rectangle((1180, 1538, 1210, 1560), fill="#1f77b4")
    draw.text((1220, 1530), "PIB por trabajador", fill="#333333", font=small_font)
    draw.rectangle((1460, 1538, 1490, 1560), fill="#d95f02")
    draw.text((1500, 1530), "PIB por hora", fill="#333333", font=small_font)
    draw.text((80, 1620), "Fuente: cálculos propios con DANE y GEIH. Se excluye 2020.", fill="#555555", font=note_font)
    for directory in [FIGURE_DIR, OUTPUT_FIGURE_DIR]:
        img.save(directory / "fig_pib_geih_productividad_departamento.png")


def draw_department_correlation_scatter(summary: pd.DataFrame) -> None:
    data = summary.dropna(subset=["crec_ocupados", "crec_horas", "crec_pib_trabajador", "crec_pib_hora"]).copy()
    title_font, label_font, small_font, note_font = fonts()
    img = Image.new("RGB", (2500, 1750), "white")
    draw = ImageDraw.Draw(img)
    draw.text((90, 45), "Crecimiento del trabajo y la productividad por departamento, 2014--2024pr", fill="#222222", font=title_font)
    draw.text((90, 105), f"Tasas anualizadas para {len(data)} departamentos; cada punto representa un departamento", fill="#555555", font=label_font)

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
        draw.line((x_pos(x_min), y_pos(slope * x_min + intercept), x_pos(x_max), y_pos(slope * x_max + intercept)), fill="#b44b3f", width=4)
        for _, row in data.iterrows():
            x = x_pos(row[x_col])
            y = y_pos(row[y_col])
            draw.ellipse((x - 10, y - 10, x + 10, y + 10), fill="#1f77b4", outline="white", width=2)
        corr = data[[x_col, y_col]].corr().iloc[0, 1]
        draw.text((plot_left + 16, plot_top + 12), f"r = {fmt_num_es(corr, 2)}", fill="#b44b3f", font=label_font)

    draw.text((90, 1630), "Nota: r es la correlación de Pearson; la línea roja muestra la tendencia lineal simple entre departamentos.", fill="#555555", font=note_font)
    for directory in [FIGURE_DIR, OUTPUT_FIGURE_DIR]:
        img.save(directory / "fig_pib_geih_productividad_departamento_correlaciones.png")


def main() -> None:
    data, summary = build_productivity_departamental()
    data.to_csv(TABLE_DIR / "pib_geih_productividad_departamento_series.csv", index=False, encoding="utf-8-sig")
    data.to_csv(OUTPUT_TABLE_DIR / "pib_geih_productividad_departamento_series.csv", index=False, encoding="utf-8-sig")
    summary.to_csv(TABLE_DIR / "pib_geih_productividad_departamento_summary.csv", index=False, encoding="utf-8-sig")
    summary.to_csv(OUTPUT_TABLE_DIR / "pib_geih_productividad_departamento_summary.csv", index=False, encoding="utf-8-sig")

    write_summary_table(summary)
    write_detail_section(data, summary)
    write_correlation_table(summary)
    draw_department_growth_chart(summary)
    draw_department_correlation_scatter(summary)

    print(f"Departamentos con información completa: {len(summary)}")
    print(summary[["departamento", "crec_pib_trabajador", "crec_pib_hora", "crec_ocupados"]].to_string(index=False))


if __name__ == "__main__":
    main()
