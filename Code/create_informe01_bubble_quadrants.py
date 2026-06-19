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
    max_width: int = 250,
    fill: str = "#222222",
) -> None:
    for line_no, line in enumerate(wrap_text(draw, text, used_font, max_width)):
        draw.text((x, y + line_no * 31), line, font=used_font, fill=fill)


def fmt_num_es(value: float, decimals: int = 1) -> str:
    return f"{value:.{decimals}f}".replace(".", ",")


def main() -> None:
    FIGURE_DIR.mkdir(parents=True, exist_ok=True)
    OUTPUT_FIGURE_DIR.mkdir(parents=True, exist_ok=True)

    summary = pd.read_csv(TABLE_DIR / "pib_geih_productividad_sector_summary.csv")
    series = pd.read_csv(TABLE_DIR / "pib_geih_productividad_sector_series.csv")
    total = pd.read_csv(TABLE_DIR / "pib_geih_productividad_total_summary.csv")

    pib_2025 = series[series["anio"] == 2025][["sector_code", "pib_miles_millones_2015"]]
    data = summary.merge(pib_2025, on="sector_code", how="left")
    data["sector"] = data["sector_code"].map(SECTOR_SHORT)
    data["x"] = data["crec_pib_hora"] * 100
    data["y"] = data["pib_hora_2025"] / 1000
    data["pib_billones"] = data["pib_miles_millones_2015"] / 1000
    data["order"] = data["sector_code"].map({code: idx for idx, code in enumerate(SECTOR_ORDER)})
    data = data.sort_values("order")

    agg_growth = float(total.loc[total["indicador"] == "PIB por hora trabajada", "crecimiento_anualizado"].iloc[0] * 100)
    agg_level = float(total.loc[total["indicador"] == "PIB por hora trabajada", "valor_2025"].iloc[0] / 1000)

    img_w, img_h = 2300, 1600
    img = Image.new("RGBA", (img_w, img_h), "white")
    draw = ImageDraw.Draw(img)

    title_font = font(48, bold=True)
    subtitle_font = font(30)
    axis_font = font(28)
    tick_font = font(25)
    label_font = font(23)
    quadrant_font = font(28, bold=True)
    note_font = font(25)

    blue = "#1F5EA8"
    gray = "#4A4A4A"
    grid = "#E7E7E7"
    bubble_fill = (162, 179, 190, 145)
    bubble_outline = "#466B80"

    left, top, right, bottom = 185, 250, 2090, 1350
    x_min, x_max = -4.5, 8.2
    y_min, y_max = 0, 130

    def x_pos(value: float) -> float:
        return left + (value - x_min) / (x_max - x_min) * (right - left)

    def y_pos(value: float) -> float:
        return bottom - (value - y_min) / (y_max - y_min) * (bottom - top)

    draw.text((75, 55), "Nivel y crecimiento de la productividad por hora en 13 grandes ramas", fill="#222222", font=title_font)
    draw.text(
        (75, 112),
        "Eje horizontal: crecimiento anualizado, 2010--2025. Eje vertical: PIB por hora en 2025. Tamaño de burbuja: PIB de la rama.",
        fill="#555555",
        font=subtitle_font,
    )

    for tick in range(-4, 9, 2):
        x = x_pos(tick)
        draw.line((x, top, x, bottom), fill=grid, width=1)
        draw.line((x, bottom, x, bottom + 9), fill=gray, width=2)
        draw_centered(draw, f"{tick}%", x, bottom + 21, tick_font, fill="#555555")

    for tick in range(0, 131, 20):
        y = y_pos(tick)
        draw.line((left, y, right, y), fill=grid, width=1)
        draw.line((left - 9, y, left, y), fill=gray, width=2)
        draw.text((left - 82, y - 14), fmt_num_es(tick, 0), fill="#555555", font=tick_font)

    x_line = x_pos(agg_growth)
    y_line = y_pos(agg_level)
    draw.line((x_line, top, x_line, bottom), fill=blue, width=3)
    draw.line((left, y_line, right, y_line), fill=blue, width=3)
    draw.line((left, top, left, bottom), fill="#333333", width=2)
    draw.line((left, bottom, right, bottom), fill="#333333", width=2)

    draw.text((x_line + 12, top + 56), f"Agregado = {fmt_num_es(agg_growth, 1)}%", fill=blue, font=tick_font)
    draw.text((right - 315, y_line - 40), f"Agregado = {fmt_num_es(agg_level, 1)}", fill=blue, font=tick_font)

    draw.text((left + 25, top + 35), "Líderes con menor crecimiento", fill=blue, font=quadrant_font)
    draw.text((x_line + 100, top + 35), "Líderes dinámicos", fill=blue, font=quadrant_font)
    draw.text((left + 25, y_line + 50), "Rezagadas", fill=blue, font=quadrant_font)
    draw.text((x_pos(4.8), y_line + 50), "Aceleradoras", fill=blue, font=quadrant_font)

    max_pib = data["pib_billones"].max()
    min_radius, max_radius = 15, 70

    def radius(pib_billones: float) -> float:
        return min_radius + math.sqrt(pib_billones / max_pib) * (max_radius - min_radius)

    for _, row in data.sort_values("pib_billones", ascending=False).iterrows():
        x, y = x_pos(row["x"]), y_pos(row["y"])
        r = radius(row["pib_billones"])
        draw.ellipse((x - r, y - r, x + r, y + r), fill=bubble_fill, outline=bubble_outline, width=3)

    offsets = {
        "A": (24, -26, 210),
        "B": (28, -44, 190),
        "C": (-230, 22, 200),
        "D+E": (24, 8, 190),
        "F": (26, 14, 175),
        "G+H+I": (26, -18, 260),
        "J": (26, -42, 260),
        "K": (28, -30, 175),
        "L": (26, -34, 180),
        "M+N": (28, -70, 260),
        "O+P+Q": (26, -10, 275),
        "R+S": (-380, -36, 340),
        "T": (24, -10, 210),
    }

    for _, row in data.iterrows():
        x, y = x_pos(row["x"]), y_pos(row["y"])
        dx, dy, width = offsets[row["sector_code"]]
        draw_label(draw, row["sector"], x + dx, y + dy, label_font, max_width=width)

    draw.text((left, bottom + 85), "Crecimiento anual del PIB por hora trabajada, 2010--2025", fill="#222222", font=axis_font)
    draw.text((left, top - 45), "Miles de pesos constantes de 2015 por hora trabajada", fill="#222222", font=axis_font)

    legend_x, legend_y = 1640, 335
    draw.text((legend_x, legend_y), "PIB 2025", fill="#222222", font=axis_font)
    for i, value in enumerate([25, 75, 175]):
        r = radius(value)
        cx = legend_x + 45 + i * 150
        cy = legend_y + 90
        draw.ellipse((cx - r, cy - r, cx + r, cy + r), fill=bubble_fill, outline=bubble_outline, width=2)
        draw_centered(draw, f"{value}", cx, cy + max_radius + 12, tick_font, fill="#555555")
    draw.text((legend_x, legend_y + 205), "Billones de pesos de 2015", fill="#555555", font=tick_font)

    draw.text(
        (75, 1510),
        "Fuente: cálculos propios con DANE y GEIH. El tamaño de la burbuja es proporcional al PIB real de cada rama en 2025.",
        fill="#555555",
        font=note_font,
    )

    output_name = "fig_pib_geih_productividad_burbujas_2025.png"
    img.convert("RGB").save(FIGURE_DIR / output_name, quality=95)
    img.convert("RGB").save(OUTPUT_FIGURE_DIR / output_name, quality=95)


if __name__ == "__main__":
    main()
