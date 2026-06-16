from pathlib import Path
import math

import numpy as np
import pandas as pd
from PIL import Image, ImageDraw, ImageFont


ROOT = Path(__file__).resolve().parents[1]
DATA_PATH = ROOT / "Datos" / "Processed" / "Paper-GEIH_base_modelo_personas_2008_2025.dta"
TABLE_DIR = ROOT / "Paper" / "tables"
SECTION_DIR = ROOT / "Paper" / "sections"
FIG_DIR = ROOT / "Paper" / "figures"

YEARS = list(range(2010, 2020)) + list(range(2021, 2026))
START_YEAR = 2010
END_YEAR = 2025
MONTHS_PER_WEEK = 52.0 / 12.0


def weighted_aggregate_from_dta(path: Path) -> pd.DataFrame:
    cols = ["anio", "fex", "horas", "ingreso_hora_real", "formal"]
    totals = {}

    reader = pd.read_stata(
        path,
        columns=cols,
        convert_categoricals=False,
        chunksize=200_000,
    )

    for chunk in reader:
        chunk = chunk[chunk["anio"].isin(YEARS)]
        chunk = chunk.dropna(subset=["anio", "fex", "horas", "ingreso_hora_real"])
        chunk = chunk[
            (chunk["fex"] > 0)
            & (chunk["horas"] > 0)
            & (chunk["horas"] <= 112)
            & (chunk["ingreso_hora_real"] > 0)
        ]

        if chunk.empty:
            continue

        chunk = chunk.copy()
        chunk["rem_mensual"] = chunk["ingreso_hora_real"] * chunk["horas"] * MONTHS_PER_WEEK
        chunk["horas_mensuales"] = chunk["horas"] * MONTHS_PER_WEEK
        chunk["grupo"] = "Total"

        formality = chunk[chunk["formal"].isin([0, 1])].copy()
        formality["grupo"] = np.where(formality["formal"] == 1, "Formal", "Informal")

        use = pd.concat(
            [
                chunk[["anio", "fex", "rem_mensual", "horas_mensuales", "grupo"]],
                formality[["anio", "fex", "rem_mensual", "horas_mensuales", "grupo"]],
            ],
            ignore_index=True,
        )

        use["ocupados"] = use["fex"]
        use["rem_total_mensual"] = use["fex"] * use["rem_mensual"]
        use["horas_mensuales"] = use["fex"] * use["horas_mensuales"]

        grouped = use.groupby(["anio", "grupo"], as_index=False)[
            ["ocupados", "rem_total_mensual", "horas_mensuales"]
        ].sum()

        for row in grouped.itertuples(index=False):
            key = (int(row.anio), row.grupo)
            if key not in totals:
                totals[key] = [0.0, 0.0, 0.0]
            totals[key][0] += float(row.ocupados)
            totals[key][1] += float(row.rem_total_mensual)
            totals[key][2] += float(row.horas_mensuales)

    rows = []
    for (year, group), values in sorted(totals.items()):
        ocupados, rem_total_mensual, horas_mensuales = values
        rows.append(
            {
                "anio": year,
                "grupo": group,
                "ocupados": ocupados,
                "rem_total_mensual": rem_total_mensual,
                "horas_mensuales": horas_mensuales,
                "rem_por_trabajador": rem_total_mensual / ocupados,
                "rem_por_hora": rem_total_mensual / horas_mensuales,
                "horas_semanales": (horas_mensuales / ocupados) / MONTHS_PER_WEEK,
            }
        )

    order = {"Total": 0, "Formal": 1, "Informal": 2}
    out = pd.DataFrame(rows)
    out["grupo_orden"] = out["grupo"].map(order)
    return out.sort_values(["grupo_orden", "anio"]).drop(columns="grupo_orden")


def annual_growth(start, end, n_years=END_YEAR - START_YEAR):
    return (end / start) ** (1 / n_years) - 1


def build_detail(series: pd.DataFrame) -> pd.DataFrame:
    endpoints = series[series["anio"].isin([START_YEAR, END_YEAR])].copy()
    wide = endpoints.pivot(index="grupo", columns="anio")
    rows = []

    indicators = [
        ("Remuneración laboral total mensual", "Billones de pesos de 2025", "rem_total_mensual", 1e12),
        ("Ocupados", "Millones", "ocupados", 1e6),
        ("Remuneración por trabajador", "Millones de pesos de 2025 al mes", "rem_por_trabajador", 1e6),
        ("Horas semanales por trabajador", "", "horas_semanales", 1),
        ("Remuneración por hora trabajada", "Miles de pesos de 2025", "rem_por_hora", 1e3),
    ]

    for group in ["Total", "Formal", "Informal"]:
        for indicator, unit, col, divisor in indicators:
            v0 = float(wide.loc[group, (col, START_YEAR)]) / divisor
            v1 = float(wide.loc[group, (col, END_YEAR)]) / divisor
            growth = annual_growth(
                float(wide.loc[group, (col, START_YEAR)]),
                float(wide.loc[group, (col, END_YEAR)]),
            )
            rows.append(
                {
                    "grupo": group,
                    "indicador": indicator,
                    "unidad": unit,
                    "valor_2010": v0,
                    "valor_2025": v1,
                    "crec_anual": growth,
                }
            )

    return pd.DataFrame(rows)


def fmt_decimal(value, digits=1):
    text = f"{value:,.{digits}f}"
    return text.replace(",", "X").replace(".", ",").replace("X", ".")


def fmt_pct(value, digits=2):
    return fmt_decimal(100 * value, digits) + r"\%"


def fmt_indicator(indicator, unit):
    if unit:
        return f"{indicator} ({unit})"
    return indicator


def write_latex_tables(detail: pd.DataFrame):
    SECTION_DIR.mkdir(parents=True, exist_ok=True)

    aggregate = detail[detail["grupo"] == "Total"].copy()
    lines = [
        r"\begin{table}[H]",
        r"\centering",
        r"\caption{Remuneración laboral, ocupados, horas y remuneración por hora, 2010--2025}",
        r"\label{tab:remuneracion_geih_agregado}",
        r"\small",
        r"\begin{tabular}{@{}p{8.4cm}rrr@{}}",
        r"\toprule",
        r"Indicador & 2010 & 2025 & Crec. anual \\",
        r"\midrule",
    ]
    for row in aggregate.itertuples(index=False):
        lines.append(
            f"{fmt_indicator(row.indicador, row.unidad)} & "
            f"{fmt_decimal(row.valor_2010, 1)} & "
            f"{fmt_decimal(row.valor_2025, 1)} & "
            f"{fmt_pct(row.crec_anual, 2)} \\\\"
        )
    lines.extend(
        [
            r"\bottomrule",
            r"\end{tabular}",
            r"\caption*{\footnotesize Nota: La remuneración por trabajador corresponde a una remuneración mensual equivalente en pesos constantes de 2025. La remuneración por hora se calcula como la remuneración laboral total dividida por las horas trabajadas. Fuente: cálculos propios con GEIH del DANE.}",
            r"\end{table}",
        ]
    )
    (SECTION_DIR / "remuneracion_geih_agregado_table.tex").write_text("\n".join(lines) + "\n", encoding="utf-8")

    formal = detail[detail["grupo"].isin(["Formal", "Informal"])].copy()
    lines = [
        r"\begin{table}[H]",
        r"\centering",
        r"\caption{Remuneración laboral por formalidad, 2010--2025}",
        r"\label{tab:remuneracion_geih_formalidad}",
        r"\footnotesize",
        r"\begin{tabular}{@{}p{1.6cm}p{7.0cm}rrr@{}}",
        r"\toprule",
        r"Grupo & Indicador & 2010 & 2025 & Crec. anual \\",
        r"\midrule",
    ]
    for row in formal.itertuples(index=False):
        lines.append(
            f"{row.grupo} & {fmt_indicator(row.indicador, row.unidad)} & "
            f"{fmt_decimal(row.valor_2010, 1)} & "
            f"{fmt_decimal(row.valor_2025, 1)} & "
            f"{fmt_pct(row.crec_anual, 2)} \\\\"
        )
    lines.extend(
        [
            r"\bottomrule",
            r"\end{tabular}",
            r"\caption*{\footnotesize Nota: La remuneración por trabajador corresponde a una remuneración mensual equivalente en pesos constantes de 2025. La remuneración por hora se calcula como la remuneración laboral total dividida por las horas trabajadas. Fuente: cálculos propios con GEIH del DANE.}",
            r"\end{table}",
        ]
    )
    (SECTION_DIR / "remuneracion_geih_formalidad_table.tex").write_text("\n".join(lines) + "\n", encoding="utf-8")


def font(size, bold=False):
    candidates = [
        Path("C:/Windows/Fonts/arialbd.ttf" if bold else "C:/Windows/Fonts/arial.ttf"),
        Path("C:/Windows/Fonts/calibrib.ttf" if bold else "C:/Windows/Fonts/calibri.ttf"),
    ]
    for candidate in candidates:
        if candidate.exists():
            return ImageFont.truetype(str(candidate), size=size)
    return ImageFont.load_default()


def draw_text(draw, xy, text, fill, size=24, bold=False, anchor=None):
    draw.text(xy, text, fill=fill, font=font(size, bold), anchor=anchor)


def draw_line_chart(series: pd.DataFrame):
    FIG_DIR.mkdir(parents=True, exist_ok=True)
    width, height = 1800, 1100
    img = Image.new("RGB", (width, height), "white")
    draw = ImageDraw.Draw(img)

    title = "Remuneración por trabajador y por hora trabajada, 2010-2025"
    draw_text(draw, (80, 45), title, "#111111", size=42, bold=True)
    draw_text(draw, (80, 95), "Índice 2010 = 100. Pesos constantes de 2025", "#444444", size=28)

    panels = [
        ("rem_por_trabajador", "Remuneración por trabajador", (95, 175, 855, 910)),
        ("rem_por_hora", "Remuneración por hora trabajada", (975, 175, 1735, 910)),
    ]
    colors = {"Total": "#222222", "Formal": "#1f5aa6", "Informal": "#b33a3a"}
    years = sorted(series["anio"].unique())
    x_min, x_max = min(years), max(years)

    for col, panel_title, box in panels:
        left, top, right, bottom = box
        base = series[series["anio"] == START_YEAR].set_index("grupo")[col]
        data = series.copy()
        data["indice"] = data.apply(lambda r: 100 * r[col] / base.loc[r["grupo"]], axis=1)
        y_min = math.floor((data["indice"].min() - 5) / 5) * 5
        y_max = math.ceil((data["indice"].max() + 5) / 5) * 5
        if y_max <= y_min:
            y_max = y_min + 10

        draw.rectangle((left, top, right, bottom), outline="#222222", width=2)
        draw_text(draw, (left, top - 48), panel_title, "#111111", size=30, bold=True)

        for t in range(int(y_min), int(y_max) + 1, 10):
            y = bottom - (t - y_min) / (y_max - y_min) * (bottom - top)
            draw.line((left, y, right, y), fill="#e6e6e6", width=1)
            draw_text(draw, (left - 12, y), str(t), "#555555", size=20, anchor="rm")

        tick_years = [2010, 2015, 2019, 2021, 2025]
        for yr in tick_years:
            x = left + (yr - x_min) / (x_max - x_min) * (right - left)
            draw.line((x, bottom, x, bottom + 8), fill="#222222", width=2)
            draw_text(draw, (x, bottom + 16), str(yr), "#555555", size=20, anchor="mt")

        for group in ["Total", "Formal", "Informal"]:
            d = data[data["grupo"] == group].sort_values("anio")
            points = []
            for row in d.itertuples(index=False):
                x = left + (row.anio - x_min) / (x_max - x_min) * (right - left)
                y = bottom - (row.indice - y_min) / (y_max - y_min) * (bottom - top)
                points.append((x, y))
            if len(points) >= 2:
                draw.line(points, fill=colors[group], width=5)
            for x, y in points:
                draw.ellipse((x - 5, y - 5, x + 5, y + 5), fill=colors[group])
            x_last, y_last = points[-1]
            label = f"{group}: {fmt_decimal(d['indice'].iloc[-1], 1)}"
            y_offset = 0
            if col == "rem_por_trabajador" and group == "Formal":
                y_offset = -22
            if col == "rem_por_trabajador" and group == "Informal":
                y_offset = 20
            if x_last > right - 20:
                draw_text(draw, (x_last - 10, y_last + y_offset), label, colors[group], size=22, bold=True, anchor="rm")
            else:
                draw_text(draw, (x_last + 10, y_last + y_offset), label, colors[group], size=22, bold=True, anchor="lm")

    legend_y = 980
    x = 95
    for group, color in colors.items():
        draw.line((x, legend_y, x + 48, legend_y), fill=color, width=6)
        draw_text(draw, (x + 62, legend_y), group, "#222222", size=24, anchor="lm")
        x += 230

    draw_text(draw, (95, 1035), "Fuente: cálculos propios con GEIH del DANE.", "#555555", size=22)
    img.save(FIG_DIR / "fig_remuneracion_geih_indices.png", quality=95)


def draw_decomposition(detail: pd.DataFrame):
    FIG_DIR.mkdir(parents=True, exist_ok=True)
    total = detail[detail["grupo"] == "Total"].set_index("indicador")
    g_worker = total.loc["Remuneración por trabajador", "crec_anual"]
    g_hour = total.loc["Remuneración por hora trabajada", "crec_anual"]
    g_hours = total.loc["Horas semanales por trabajador", "crec_anual"]

    width, height = 1500, 980
    img = Image.new("RGB", (width, height), "white")
    draw = ImageDraw.Draw(img)

    draw_text(draw, (75, 45), "Descomposición del crecimiento de la remuneración por trabajador", "#111111", size=38, bold=True)
    draw_text(draw, (75, 92), "Crecimiento anualizado 2010-2025. Contribuciones aproximadas", "#444444", size=26)

    chart = (120, 190, 1380, 680)
    left, top, right, bottom = chart
    y_min, y_max = -0.008, 0.024
    zero = bottom - (0 - y_min) / (y_max - y_min) * (bottom - top)
    draw.line((left, zero, right, zero), fill="#222222", width=2)

    ticks = [-0.005, 0.0, 0.005, 0.010, 0.015, 0.020]
    for t in ticks:
        y = bottom - (t - y_min) / (y_max - y_min) * (bottom - top)
        draw.line((left, y, right, y), fill="#e8e8e8", width=1)
        draw_text(draw, (left - 14, y), fmt_decimal(100 * t, 1) + "%", "#555555", size=20, anchor="rm")

    bars = [
        ("Rem. por hora\ntrabajada", g_hour, "#1f5aa6"),
        ("Horas por\ntrabajador", g_hours, "#b33a3a"),
        ("Rem. por\ntrabajador", g_worker, "#222222"),
    ]
    xs = [420, 750, 1080]
    bar_w = 170
    for (label, value, color), x in zip(bars, xs):
        y = bottom - (value - y_min) / (y_max - y_min) * (bottom - top)
        if value >= 0:
            draw.rectangle((x - bar_w / 2, y, x + bar_w / 2, zero), fill=color)
            label_y = y - 18
            anchor = "mb"
        else:
            draw.rectangle((x - bar_w / 2, zero, x + bar_w / 2, y), fill=color)
            label_y = y + 18
            anchor = "mt"
        draw_text(draw, (x, label_y), fmt_decimal(100 * value, 2) + "%", color, size=28, bold=True, anchor=anchor)
        for i, part in enumerate(label.split("\n")):
            draw_text(draw, (x, bottom + 38 + i * 28), part, "#222222", size=24, bold=True, anchor="mt")

    draw_text(draw, (120, 815), "Nota: la remuneración por trabajador es el producto entre la remuneración por hora trabajada y las horas trabajadas por trabajador.", "#444444", size=23)
    draw_text(draw, (120, 850), "Por redondeo, las contribuciones pueden no sumar exactamente el crecimiento reportado.", "#444444", size=23)
    draw_text(draw, (120, 890), "Fuente: cálculos propios con GEIH del DANE.", "#555555", size=22)
    img.save(FIG_DIR / "fig_remuneracion_geih_descomposicion.png", quality=95)


def main():
    TABLE_DIR.mkdir(parents=True, exist_ok=True)
    series = weighted_aggregate_from_dta(DATA_PATH)
    detail = build_detail(series)

    series.to_csv(TABLE_DIR / "remuneracion_geih_formalidad_series.csv", index=False)
    detail.to_csv(TABLE_DIR / "remuneracion_geih_formalidad_summary.csv", index=False)
    write_latex_tables(detail)
    draw_line_chart(series)
    draw_decomposition(detail)


if __name__ == "__main__":
    main()
