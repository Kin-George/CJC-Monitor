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

    geih = geih[(geih["anio"].between(2010, 2025)) & (geih["fex"] > 0)].copy()
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
    total["pib_pesos_2015"] = total["pib_miles_millones_2015"] * 1e9
    total["pib_por_trabajador_millones_2015"] = total["pib_pesos_2015"] / total["ocupados"] / 1e6
    total["pib_por_hora_pesos_2015"] = total["pib_pesos_2015"] / total["horas_anuales"]
    total = total.sort_values("anio")

    sector = pib_sector.merge(geih_sector, on=["anio", "sector_code"], how="inner")
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


def write_latex_tables(total_summary: pd.DataFrame, sector_summary: pd.DataFrame) -> None:
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
            f"{escape_latex(row['indicador'])} & "
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

    sector_sorted = sector_summary.sort_values("crec_pib_trabajador", ascending=False)
    sector_lines = [
        r"\begin{table}[H]",
        r"\centering",
        r"\caption{Productividad laboral por sector CIIU, 2010--2025}",
        r"\label{tab:pib_geih_productividad_sector}",
        r"\scriptsize",
        r"\begin{tabular}{lrrrrrr}",
        r"\toprule",
        r"& \multicolumn{3}{c}{PIB por trabajador} & \multicolumn{3}{c}{PIB por hora} \\",
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
            f"{fmt_num_es(row['pib_hora_2010'], 0)} & "
            f"{fmt_num_es(row['pib_hora_2025'], 0)} & "
            f"{fmt_pct_es(row['crec_pib_hora'])} \\\\"
        )
    sector_lines.extend(
        [
            r"\bottomrule",
            r"\end{tabular}",
            r"\caption*{\footnotesize Nota: PIB por trabajador en millones de pesos constantes de 2015 por ocupado; PIB por hora en pesos constantes de 2015 por hora trabajada. Sectores según 12 agrupaciones CIIU Rev. 4 A.C. del DANE; ocupados y horas se agregan desde GEIH usando la homologación de subramas del proyecto. Se excluyen organizaciones extraterritoriales del cruce sectorial por no hacer parte de las 12 agrupaciones. Fuente: cálculos propios con DANE y GEIH.}",
            r"\end{table}",
        ]
    )
    (SECTION_DIR / "pib_geih_productividad_sector_table.tex").write_text(
        "\n".join(sector_lines), encoding="utf-8"
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


def main() -> None:
    total, total_summary, sector, sector_summary = build_productivity()

    total.to_csv(TABLE_DIR / "pib_geih_productividad_total_series.csv", index=False, encoding="utf-8-sig")
    total.to_csv(OUTPUT_TABLE_DIR / "pib_geih_productividad_total_series.csv", index=False, encoding="utf-8-sig")
    total_summary.to_csv(TABLE_DIR / "pib_geih_productividad_total_summary.csv", index=False, encoding="utf-8-sig")
    total_summary.to_csv(OUTPUT_TABLE_DIR / "pib_geih_productividad_total_summary.csv", index=False, encoding="utf-8-sig")
    sector.to_csv(TABLE_DIR / "pib_geih_productividad_sector_series.csv", index=False, encoding="utf-8-sig")
    sector.to_csv(OUTPUT_TABLE_DIR / "pib_geih_productividad_sector_series.csv", index=False, encoding="utf-8-sig")
    sector_summary.to_csv(TABLE_DIR / "pib_geih_productividad_sector_summary.csv", index=False, encoding="utf-8-sig")
    sector_summary.to_csv(OUTPUT_TABLE_DIR / "pib_geih_productividad_sector_summary.csv", index=False, encoding="utf-8-sig")

    write_latex_tables(total_summary, sector_summary)
    draw_index_chart(total)
    draw_sector_cagr_chart(sector_summary)

    print("Resumen total")
    print(total_summary.to_string(index=False))
    print("\nSectores ordenados por crecimiento de PIB por trabajador")
    print(sector_summary.sort_values("crec_pib_trabajador", ascending=False).to_string(index=False))


if __name__ == "__main__":
    main()
