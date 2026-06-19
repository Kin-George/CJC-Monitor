from __future__ import annotations

import math
from pathlib import Path

import pandas as pd
from PIL import Image, ImageDraw, ImageFont


PROJECT_ROOT = Path(__file__).resolve().parents[1]
TABLE_DIR = PROJECT_ROOT / "Paper" / "tables"
FIGURE_DIR = PROJECT_ROOT / "Paper" / "figures"
OUTPUT_FIGURE_DIR = PROJECT_ROOT / "Outputs" / "Figures"

SECTOR_ORDER = ["A", "B", "C", "D+E", "F", "G+H+I", "J", "K", "L", "M+N", "O+P+Q", "R+S", "T"]
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

CATEGORY_COLORS = {
    "lider_dinamico": ((80, 153, 137, 150), "#2A7F73"),
    "lider_menor_crecimiento": ((78, 121, 167, 145), "#2A5E9A"),
    "aceleradora": ((239, 177, 70, 150), "#B97816"),
    "rezagada": ((183, 91, 77, 145), "#944236"),
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


def wrap_text(draw: ImageDraw.ImageDraw, text: str, used_font, max_width: int) -> list[str]:
    words = text.split()
    lines: list[str] = []
    current = ""
    for word in words:
        candidate = f"{current} {word}".strip()
        if text_width(draw, candidate, used_font) <= max_width:
            current = candidate
        else:
            if current:
                lines.append(current)
            current = word
    if current:
        lines.append(current)
    return lines


def draw_label(
    draw: ImageDraw.ImageDraw,
    text: str,
    x: float,
    y: float,
    used_font,
    max_width: int = 270,
    fill: str = "#222222",
) -> None:
    for line_no, line in enumerate(wrap_text(draw, text, used_font, max_width)):
        draw.text((x, y + line_no * 36), line, font=used_font, fill=fill)


def fmt_num_es(value: float, decimals: int = 1) -> str:
    return f"{value:.{decimals}f}".replace(".", ",")


def classify(row: pd.Series, agg_level: float, agg_growth: float) -> str:
    high_level = row["level_hour"] >= agg_level
    high_growth = row["growth_hour"] >= agg_growth
    if high_level and high_growth:
        return "lider_dinamico"
    if high_level and not high_growth:
        return "lider_menor_crecimiento"
    if not high_level and high_growth:
        return "aceleradora"
    return "rezagada"


def main() -> None:
    FIGURE_DIR.mkdir(parents=True, exist_ok=True)
    OUTPUT_FIGURE_DIR.mkdir(parents=True, exist_ok=True)

    summary = pd.read_csv(TABLE_DIR / "pib_geih_productividad_sector_summary.csv")
    series = pd.read_csv(TABLE_DIR / "pib_geih_productividad_sector_series.csv")
    total = pd.read_csv(TABLE_DIR / "pib_geih_productividad_total_summary.csv")

    pib_2025 = series[series["anio"] == 2025][["sector_code", "pib_miles_millones_2015"]]
    data = summary.merge(pib_2025, on="sector_code", how="left")
    data["sector"] = data["sector_code"].map(SECTOR_SHORT)
    data["level_hour"] = data["pib_hora_2025"] / 1000
    data["growth_hour"] = data["crec_pib_hora"] * 100
    data["pib_billones"] = data["pib_miles_millones_2015"] / 1000
    data["order"] = data["sector_code"].map({code: idx for idx, code in enumerate(SECTOR_ORDER)})
    data = data.sort_values("order")

    agg_level = float(total.loc[total["indicador"] == "PIB por hora trabajada", "valor_2025"].iloc[0] / 1000)
    agg_growth = float(total.loc[total["indicador"] == "PIB por hora trabajada", "crecimiento_anualizado"].iloc[0] * 100)
    data["category"] = data.apply(lambda row: classify(row, agg_level, agg_growth), axis=1)

    img_w, img_h = 2600, 1800
    img = Image.new("RGBA", (img_w, img_h), "white")
    draw = ImageDraw.Draw(img)

    title_font = font(58, bold=True)
    subtitle_font = font(37)
    axis_font = font(36)
    tick_font = font(32)
    label_font = font(31)
    quadrant_font = font(36, bold=True)
    note_font = font(30)

    blue = "#1F5EA8"
    gray = "#4A4A4A"
    grid = "#E7E7E7"

    left, top, right, bottom = 220, 290, 2390, 1480
    x_min, x_max = 0, 128
    y_min, y_max = -4.2, 8.1
    max_pib = data["pib_billones"].max()
    min_radius, max_radius = 20, 72

    def radius(pib_billones: float) -> float:
        return min_radius + math.sqrt(pib_billones / max_pib) * (max_radius - min_radius)

    def x_pos(value: float) -> float:
        return left + (value - x_min) / (x_max - x_min) * (right - left)

    def y_pos(value: float) -> float:
        return bottom - (value - y_min) / (y_max - y_min) * (bottom - top)

    draw.text((75, 55), "Nivel y crecimiento de la productividad por hora en 13 grandes ramas", fill="#222222", font=title_font)
    draw.text(
        (75, 125),
        "Eje horizontal: PIB por hora trabajada en 2025. Eje vertical: crecimiento anualizado, 2010--2025. Tamaño de burbuja: PIB de la rama.",
        fill="#555555",
        font=subtitle_font,
    )

    for tick in range(0, 129, 20):
        x = x_pos(tick)
        draw.line((x, top, x, bottom), fill=grid, width=1)
        draw.line((x, bottom, x, bottom + 10), fill=gray, width=2)
        draw_centered(draw, fmt_num_es(tick, 0), x, bottom + 24, tick_font, fill="#555555")

    for tick in range(-4, 9, 2):
        y = y_pos(tick)
        draw.line((left, y, right, y), fill=grid, width=1)
        draw.line((left - 10, y, left, y), fill=gray, width=2)
        draw.text((left - 85, y - 17), f"{tick}%", fill="#555555", font=tick_font)

    x_line = x_pos(agg_level)
    y_line = y_pos(agg_growth)
    draw.line((x_line, top, x_line, bottom), fill=blue, width=3)
    draw.line((left, y_line, right, y_line), fill=blue, width=3)
    draw.line((left, top, left, bottom), fill="#333333", width=2)
    draw.line((left, bottom, right, bottom), fill="#333333", width=2)

    draw.text((x_line + 15, top + 220), f"Agregado = {fmt_num_es(agg_level, 1)}", fill=blue, font=tick_font)
    draw.text((right - 330, y_line - 45), f"Agregado = {fmt_num_es(agg_growth, 1)}%", fill=blue, font=tick_font)

    for _, row in data.sort_values("pib_billones", ascending=False).iterrows():
        x, y = x_pos(row["level_hour"]), y_pos(row["growth_hour"])
        r = radius(row["pib_billones"])
        fill, outline = CATEGORY_COLORS[row["category"]]
        draw.ellipse((x - r, y - r, x + r, y + r), fill=fill, outline=outline, width=3)

    def draw_quadrant_label(text: str, x: float, y: float) -> None:
        padding_x, padding_y = 10, 6
        bbox = draw.textbbox((x, y), text, font=quadrant_font)
        draw.rectangle(
            (bbox[0] - padding_x, bbox[1] - padding_y, bbox[2] + padding_x, bbox[3] + padding_y),
            fill=(255, 255, 255, 225),
        )
        draw.text((x, y), text, fill=blue, font=quadrant_font)

    draw_quadrant_label("Aceleradoras", left + 30, top + 35)
    draw_quadrant_label("Líderes dinámicos", x_line + 260, top + 35)
    draw_quadrant_label("Rezagadas", left + 30, y_line + 120)
    draw_quadrant_label("Líderes con menor crecimiento", x_pos(64), y_line + 55)

    offsets = {
        "A": (26, -46, 230),
        "B": (28, -24, 180),
        "C": (32, 32, 220),
        "D+E": (-55, -8, 220),
        "F": (26, 10, 190),
        "G+H+I": (-45, 26, 300),
        "J": (30, -52, 300),
        "K": (30, -32, 180),
        "L": (-360, -30, 260),
        "M+N": (30, -70, 310),
        "O+P+Q": (28, -6, 320),
        "R+S": (30, 8, 360),
        "T": (-55, -12, 230),
    }
    for _, row in data.iterrows():
        x, y = x_pos(row["level_hour"]), y_pos(row["growth_hour"])
        dx, dy, width = offsets[row["sector_code"]]
        draw_label(draw, row["sector"], x + dx, y + dy, label_font, max_width=width)

    draw.text((left, bottom + 95), "PIB por hora trabajada en 2025 (miles de pesos constantes de 2015)", fill="#222222", font=axis_font)
    draw.text((left, top - 55), "Crecimiento anual del PIB por hora trabajada, 2010--2025", fill="#222222", font=axis_font)

    legend_x, legend_y = 1780, 445
    draw.text((legend_x, legend_y), "PIB 2025", fill="#222222", font=axis_font)
    for i, value in enumerate([25, 75, 175]):
        r = radius(value)
        cx = legend_x + 60 + i * 175
        cy = legend_y + 110
        draw.ellipse((cx - r, cy - r, cx + r, cy + r), fill=(162, 179, 190, 120), outline="#466B80", width=2)
        draw_centered(draw, f"{value}", cx, cy + max_radius + 16, tick_font, fill="#555555")
    draw.text((legend_x, legend_y + 245), "Billones de pesos de 2015", fill="#555555", font=tick_font)

    draw.text(
        (75, 1715),
        "Fuente: cálculos propios con DANE y GEIH. El tamaño de la burbuja es proporcional al PIB real de cada rama en 2025.",
        fill="#555555",
        font=note_font,
    )

    output_name = "fig_pib_geih_productividad_burbujas_2025.png"
    img.convert("RGB").save(FIGURE_DIR / output_name, quality=95)
    img.convert("RGB").save(OUTPUT_FIGURE_DIR / output_name, quality=95)


if __name__ == "__main__":
    main()
