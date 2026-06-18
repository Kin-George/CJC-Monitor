from __future__ import annotations

import math
from pathlib import Path

import pandas as pd
from PIL import Image, ImageDraw, ImageFont


PROJECT_ROOT = Path(__file__).resolve().parents[1]
TABLE_DIR = PROJECT_ROOT / "Paper" / "tables"
FIGURE_DIR = PROJECT_ROOT / "Paper" / "figures" / "presentation_panels"

SECTOR_ORDER = ["A", "B", "C", "D+E", "F", "G+H+I", "J", "K", "L", "M+N", "O+P+Q", "R+S", "T"]
CODE_SLUG = {
    "A": "A",
    "B": "B",
    "C": "C",
    "D+E": "DE",
    "F": "F",
    "G+H+I": "GHI",
    "J": "J",
    "K": "K",
    "L": "L",
    "M+N": "MN",
    "O+P+Q": "OPQ",
    "R+S": "RS",
    "T": "T",
}
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
    "R+S": "Arte, recreación y otros servicios",
    "T": "Hogares como empleadores",
}


def font(size: int, bold: bool = False) -> ImageFont.FreeTypeFont | ImageFont.ImageFont:
    candidates = [
        Path(r"C:\Windows\Fonts\arialbd.ttf" if bold else r"C:\Windows\Fonts\arial.ttf"),
        Path(r"C:\Windows\Fonts\calibrib.ttf" if bold else r"C:\Windows\Fonts\calibri.ttf"),
    ]
    for path in candidates:
        if path.exists():
            return ImageFont.truetype(str(path), size)
    return ImageFont.load_default()


def text_width(draw: ImageDraw.ImageDraw, text: str, used_font) -> int:
    bbox = draw.textbbox((0, 0), text, font=used_font)
    return bbox[2] - bbox[0]


def draw_centered(draw: ImageDraw.ImageDraw, text: str, x: float, y: float, used_font, fill: str = "#333333") -> None:
    draw.text((x - text_width(draw, text, used_font) / 2, y), text, font=used_font, fill=fill)


def fmt_num_es(value: float, decimals: int = 1) -> str:
    return f"{value:.{decimals}f}".replace(".", ",")


def cagr(start: float, end: float, start_year: int = 2010, end_year: int = 2025) -> float:
    return (end / start) ** (1 / (end_year - start_year)) - 1


def draw_line_chart(sector: pd.DataFrame, code: str, y_min: int, y_max: int) -> None:
    img = Image.new("RGB", (1600, 900), "white")
    draw = ImageDraw.Draw(img)

    title_font = font(42, bold=True)
    subtitle_font = font(27)
    axis_font = font(24)
    label_font = font(26, bold=True)
    note_font = font(22)

    blue = "#1f77b4"
    orange = "#d95f02"
    grid = "#e6e6e6"
    axis = "#333333"

    left, top, right, bottom = 145, 165, 1505, 760

    def x_pos(year: float) -> float:
        return left + (year - 2010) / (2025 - 2010) * (right - left)

    def y_pos(value: float) -> float:
        return bottom - (value - y_min) / (y_max - y_min) * (bottom - top)

    draw.text((70, 45), SECTOR_SHORT[code], fill="#222222", font=title_font)
    draw.text((70, 97), "PIB por trabajador y PIB por hora trabajada, índice 2010 = 100", fill="#555555", font=subtitle_font)
    draw.line((1070, 83, 1145, 83), fill=blue, width=7)
    draw.text((1160, 66), "PIB por trabajador", fill="#333333", font=subtitle_font)
    draw.line((1070, 126, 1145, 126), fill=orange, width=7)
    draw.text((1160, 109), "PIB por hora", fill="#333333", font=subtitle_font)

    for tick in range(y_min, y_max + 1, 50):
        y = y_pos(tick)
        line_color = "#bbbbbb" if tick == 100 else grid
        line_width = 3 if tick == 100 else 1
        draw.line((left, y, right, y), fill=line_color, width=line_width)
        draw.text((left - 78, y - 13), str(tick), fill="#555555", font=axis_font)

    for year in [2010, 2015, 2020, 2025]:
        x = x_pos(year)
        draw.line((x, bottom, x, bottom + 9), fill=axis, width=2)
        draw_centered(draw, str(year), x, bottom + 20, axis_font, fill="#555555")

    draw.line((left, top, left, bottom), fill=axis, width=2)
    draw.line((left, bottom, right, bottom), fill=axis, width=2)
    draw.text((left, top - 42), "Índice", fill="#555555", font=axis_font)

    sector = sector.sort_values("anio")
    years = sector["anio"].tolist()

    def draw_series(values: list[float], color: str) -> None:
        points = [(x_pos(year), y_pos(value)) for year, value in zip(years, values)]
        for p1, p2 in zip(points, points[1:]):
            draw.line((p1[0], p1[1], p2[0], p2[1]), fill=color, width=6)
        for x, y in points:
            draw.ellipse((x - 6, y - 6, x + 6, y + 6), fill=color, outline="white", width=2)

    worker = sector["idx_worker"].tolist()
    hour = sector["idx_hour"].tolist()
    draw_series(worker, blue)
    draw_series(hour, orange)

    draw.text((x_pos(2025) - 86, y_pos(worker[-1]) - 38), fmt_num_es(worker[-1], 0), fill=blue, font=label_font)
    draw.text((x_pos(2025) - 86, y_pos(hour[-1]) + 10), fmt_num_es(hour[-1], 0), fill=orange, font=label_font)
    draw.text((70, 825), "Nota: 2020 no aparece porque no hay GEIH anual comparable en la base del proyecto.", fill="#555555", font=note_font)

    img.save(FIGURE_DIR / f"fig_presentacion_indices_{CODE_SLUG[code]}.png")


def draw_waterfall(rows: dict[str, dict], code: str, y_min: int, y_max: int) -> None:
    row = rows[code]
    img = Image.new("RGB", (1600, 900), "white")
    draw = ImageDraw.Draw(img)

    title_font = font(42, bold=True)
    subtitle_font = font(27)
    axis_font = font(24)
    value_font = font(29, bold=True)
    note_font = font(22)

    left, top, right, bottom = 160, 165, 1500, 735

    def y_pos(value: float) -> float:
        return bottom - (value - y_min) / (y_max - y_min) * (bottom - top)

    def fmt_pp(value: float) -> str:
        sign = "+" if value > 0 else ""
        return f"{sign}{fmt_num_es(value, 1)}"

    draw.text((70, 45), SECTOR_SHORT[code], fill="#222222", font=title_font)
    draw.text((70, 97), "Descomposición del crecimiento anual del PIB, 2010--2025; puntos porcentuales", fill="#555555", font=subtitle_font)

    tick_start = int(math.floor(y_min / 2) * 2)
    tick_end = int(math.ceil(y_max / 2) * 2)
    for tick in range(tick_start, tick_end + 1, 2):
        if tick < y_min or tick > y_max:
            continue
        y = y_pos(tick)
        color = "#bdbdbd" if tick == 0 else "#e8e8e8"
        width = 3 if tick == 0 else 1
        draw.line((left, y, right, y), fill=color, width=width)
        draw.text((left - 72, y - 13), fmt_num_es(tick, 0), fill="#555555", font=axis_font)

    draw.line((left, top, left, bottom), fill="#333333", width=2)
    draw.line((left, bottom, right, bottom), fill="#333333", width=2)
    draw.text((left, top - 42), "p.p.", fill="#555555", font=axis_font)

    centers = [left + 190, left + 515, left + 840, left + 1165]
    labels = ["Ocupados", "Horas", "PIB/hora", "PIB total"]
    bar_w = 155
    running = 0.0

    for idx, (_, value, color) in enumerate(row["components"]):
        center = centers[idx]
        x0, x1 = center - bar_w / 2, center + bar_w / 2
        y0, y1 = y_pos(running), y_pos(running + value)
        rect_top, rect_bottom = min(y0, y1), max(y0, y1)
        draw.rectangle((x0, rect_top, x1, rect_bottom), fill=color, outline="#333333", width=2)
        label_y = rect_top - 36 if value >= 0 else rect_bottom + 8
        draw_centered(draw, fmt_pp(value), center, label_y, value_font, fill="#222222")
        running += value
        if idx < 2:
            y_connector = y_pos(running)
            draw.line((x1, y_connector, centers[idx + 1] - bar_w / 2, y_connector), fill="#888888", width=2)

    center = centers[-1]
    x0, x1 = center - bar_w / 2, center + bar_w / 2
    y0, y1 = y_pos(0), y_pos(row["pib"])
    draw.rectangle((x0, min(y0, y1), x1, max(y0, y1)), fill="#555555", outline="#333333", width=2)
    label_y = min(y0, y1) - 36 if row["pib"] >= 0 else max(y0, y1) + 8
    draw_centered(draw, fmt_pp(row["pib"]), center, label_y, value_font, fill="#222222")

    for center, label in zip(centers, labels):
        draw_centered(draw, label, center, bottom + 24, axis_font, fill="#555555")

    draw.text((70, 825), "Nota: las barras muestran contribuciones anualizadas al crecimiento del PIB de cada actividad.", fill="#555555", font=note_font)

    img.save(FIGURE_DIR / f"fig_presentacion_descomposicion_{CODE_SLUG[code]}.png")


def main() -> None:
    FIGURE_DIR.mkdir(parents=True, exist_ok=True)
    series = pd.read_csv(TABLE_DIR / "pib_geih_productividad_sector_series.csv")

    indexed_parts = []
    for code in SECTOR_ORDER:
        part = series[series["sector_code"] == code].copy()
        base_worker = part.loc[part["anio"] == 2010, "pib_por_trabajador_millones_2015"].iloc[0]
        base_hour = part.loc[part["anio"] == 2010, "pib_por_hora_pesos_2015"].iloc[0]
        part["idx_worker"] = part["pib_por_trabajador_millones_2015"] / base_worker * 100
        part["idx_hour"] = part["pib_por_hora_pesos_2015"] / base_hour * 100
        indexed_parts.append(part)
    indexed = pd.concat(indexed_parts, ignore_index=True)

    y_min_indices = math.floor(min(indexed["idx_worker"].min(), indexed["idx_hour"].min()) / 50) * 50
    y_max_indices = math.ceil(max(indexed["idx_worker"].max(), indexed["idx_hour"].max()) / 50) * 50
    y_min_indices = min(y_min_indices, 50)
    y_max_indices = max(y_max_indices, 150)

    decomposition_rows: dict[str, dict] = {}
    for code in SECTOR_ORDER:
        part = series[series["sector_code"] == code].set_index("anio")
        start = part.loc[2010]
        end = part.loc[2025]
        start_hours = start["horas_anuales"] / start["ocupados"] / 52
        end_hours = end["horas_anuales"] / end["ocupados"] / 52
        components = [
            ("Ocupados", 100 * cagr(start["ocupados"], end["ocupados"]), "#2a6fbb"),
            ("Horas", 100 * cagr(start_hours, end_hours), "#b44b3f"),
            ("PIB/hora", 100 * cagr(start["pib_por_hora_pesos_2015"], end["pib_por_hora_pesos_2015"]), "#2a9d8f"),
        ]
        running = 0.0
        cumulative = [0.0]
        for _, value, _ in components:
            running += value
            cumulative.append(running)
        decomposition_rows[code] = {
            "components": components,
            "pib": 100 * cagr(start["pib_miles_millones_2015"], end["pib_miles_millones_2015"]),
            "cumulative": cumulative,
        }

    y_min_decomp = math.floor((min(min(row["cumulative"]) for row in decomposition_rows.values()) - 0.7))
    y_max_decomp = math.ceil((max(max(row["cumulative"] + [row["pib"]]) for row in decomposition_rows.values()) + 0.7))
    y_min_decomp = min(y_min_decomp, -1)

    for code in SECTOR_ORDER:
        draw_line_chart(indexed[indexed["sector_code"] == code], code, y_min_indices, y_max_indices)
        draw_waterfall(decomposition_rows, code, y_min_decomp, y_max_decomp)


if __name__ == "__main__":
    main()
