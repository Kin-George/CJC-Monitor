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
    max_width: int = 250,
    fill: str = "#222222",
) -> None:
    for line_no, line in enumerate(wrap_text(draw, text, used_font, max_width)):
        draw.text((x, y + line_no * 34), line, font=used_font, fill=fill)


def fmt_num_es(value: float, decimals: int = 1) -> str:
    return f"{value:.{decimals}f}".replace(".", ",")


def classify_by_hour(row: pd.Series, agg_growth_hour: float, agg_level_hour: float) -> str:
    high_growth = row["growth_hour"] >= agg_growth_hour
    high_level = row["level_hour"] >= agg_level_hour
    if high_growth and high_level:
        return "lider_dinamico"
    if high_growth and not high_level:
        return "aceleradora"
    if not high_growth and high_level:
        return "lider_menor_crecimiento"
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
    data["growth_worker"] = data["crec_pib_trabajador"] * 100
    data["level_worker"] = data["pib_trabajador_2025"]
    data["growth_hour"] = data["crec_pib_hora"] * 100
    data["level_hour"] = data["pib_hora_2025"] / 1000
    data["pib_billones"] = data["pib_miles_millones_2015"] / 1000
    data["order"] = data["sector_code"].map({code: idx for idx, code in enumerate(SECTOR_ORDER)})
    data = data.sort_values("order")

    agg_growth_worker = float(total.loc[total["indicador"] == "PIB por trabajador", "crecimiento_anualizado"].iloc[0] * 100)
    agg_level_worker = float(total.loc[total["indicador"] == "PIB por trabajador", "valor_2025"].iloc[0])
    agg_growth_hour = float(total.loc[total["indicador"] == "PIB por hora trabajada", "crecimiento_anualizado"].iloc[0] * 100)
    agg_level_hour = float(total.loc[total["indicador"] == "PIB por hora trabajada", "valor_2025"].iloc[0] / 1000)

    data["category"] = data.apply(lambda row: classify_by_hour(row, agg_growth_hour, agg_level_hour), axis=1)

    img_w, img_h = 2600, 3000
    img = Image.new("RGBA", (img_w, img_h), "white")
    draw = ImageDraw.Draw(img)

    title_font = font(58, bold=True)
    subtitle_font = font(36)
    panel_font = font(42, bold=True)
    axis_font = font(34)
    tick_font = font(30)
    label_font = font(29)
    quadrant_font = font(34, bold=True)
    note_font = font(29)

    blue = "#1F5EA8"
    gray = "#4A4A4A"
    grid = "#E7E7E7"

    left, right = 215, 2405
    x_min, x_max = -4.5, 8.2
    max_pib = data["pib_billones"].max()
    min_radius, max_radius = 18, 76

    def radius(pib_billones: float) -> float:
        return min_radius + math.sqrt(pib_billones / max_pib) * (max_radius - min_radius)

    def draw_panel(
        *,
        top: int,
        bottom: int,
        y_min: float,
        y_max: float,
        y_ticks: list[int],
        x_col: str,
        y_col: str,
        agg_growth: float,
        agg_level: float,
        panel_title: str,
        y_axis_label: str,
        x_axis_label: str | None,
        label_offsets: dict[str, tuple[int, int, int]],
        show_quadrants: bool,
        show_size_legend: bool,
    ) -> None:
        def x_pos(value: float) -> float:
            return left + (value - x_min) / (x_max - x_min) * (right - left)

        def y_pos(value: float) -> float:
            return bottom - (value - y_min) / (y_max - y_min) * (bottom - top)

        draw.text((left, top - 108), panel_title, fill="#222222", font=panel_font)
        draw.text((left, top - 55), y_axis_label, fill="#555555", font=axis_font)

        for tick in range(-4, 9, 2):
            x = x_pos(tick)
            draw.line((x, top, x, bottom), fill=grid, width=1)
            draw.line((x, bottom, x, bottom + 10), fill=gray, width=2)
            draw_centered(draw, f"{tick}%", x, bottom + 24, tick_font, fill="#555555")

        for tick in y_ticks:
            y = y_pos(tick)
            draw.line((left, y, right, y), fill=grid, width=1)
            draw.line((left - 10, y, left, y), fill=gray, width=2)
            draw.text((left - 95, y - 17), fmt_num_es(tick, 0), fill="#555555", font=tick_font)

        x_line = x_pos(agg_growth)
        y_line = y_pos(agg_level)
        draw.line((x_line, top, x_line, bottom), fill=blue, width=3)
        draw.line((left, y_line, right, y_line), fill=blue, width=3)
        draw.line((left, top, left, bottom), fill="#333333", width=2)
        draw.line((left, bottom, right, bottom), fill="#333333", width=2)

        draw.text((x_line + 12, top + 18), f"Agregado = {fmt_num_es(agg_growth, 1)}%", fill=blue, font=tick_font)
        draw.text((right - 335, y_line - 43), f"Agregado = {fmt_num_es(agg_level, 1)}", fill=blue, font=tick_font)

        if show_quadrants:
            draw.text((left + 30, top + 35), "Líderes con menor crecimiento", fill=blue, font=quadrant_font)
            draw.text((x_line + 115, top + 35), "Líderes dinámicos", fill=blue, font=quadrant_font)
            draw.text((left + 30, y_line + 50), "Rezagadas", fill=blue, font=quadrant_font)
            draw.text((x_pos(4.7), y_line + 50), "Aceleradoras", fill=blue, font=quadrant_font)

        for _, row in data.sort_values("pib_billones", ascending=False).iterrows():
            x, y = x_pos(row[x_col]), y_pos(row[y_col])
            r = radius(row["pib_billones"])
            fill, outline = CATEGORY_COLORS[row["category"]]
            draw.ellipse((x - r, y - r, x + r, y + r), fill=fill, outline=outline, width=3)

        for _, row in data.iterrows():
            x, y = x_pos(row[x_col]), y_pos(row[y_col])
            dx, dy, width = label_offsets[row["sector_code"]]
            draw_label(draw, row["sector"], x + dx, y + dy, label_font, max_width=width)

        if x_axis_label:
            draw.text((left, bottom + 95), x_axis_label, fill="#222222", font=axis_font)

        if show_size_legend:
            legend_x, legend_y = 1740, top + 135
            draw.text((legend_x, legend_y), "PIB 2025", fill="#222222", font=axis_font)
            for i, value in enumerate([25, 75, 175]):
                r = radius(value)
                cx = legend_x + 55 + i * 165
                cy = legend_y + 105
                draw.ellipse(
                    (cx - r, cy - r, cx + r, cy + r),
                    fill=(162, 179, 190, 120),
                    outline="#466B80",
                    width=2,
                )
                draw_centered(draw, f"{value}", cx, cy + max_radius + 14, tick_font, fill="#555555")
            draw.text((legend_x, legend_y + 230), "Billones de pesos de 2015", fill="#555555", font=tick_font)

    worker_offsets = {
        "A": (24, -14, 230),
        "B": (26, -40, 190),
        "C": (30, 6, 220),
        "D+E": (26, 0, 220),
        "F": (24, 8, 190),
        "G+H+I": (-395, -24, 340),
        "J": (24, -34, 300),
        "K": (26, -35, 190),
        "L": (-245, -58, 220),
        "M+N": (-355, -52, 310),
        "O+P+Q": (24, -4, 310),
        "R+S": (24, -28, 330),
        "T": (24, -10, 230),
    }

    hour_offsets = {
        "A": (24, -26, 210),
        "B": (28, -44, 190),
        "C": (-245, 22, 210),
        "D+E": (24, 8, 210),
        "F": (26, 14, 185),
        "G+H+I": (26, -18, 270),
        "J": (26, -42, 270),
        "K": (28, -30, 185),
        "L": (26, -34, 190),
        "M+N": (28, -70, 275),
        "O+P+Q": (26, -10, 290),
        "R+S": (-415, -36, 365),
        "T": (24, -10, 220),
    }

    draw.text((75, 55), "Nivel y crecimiento de la productividad laboral en 13 grandes ramas", fill="#222222", font=title_font)
    draw.text(
        (75, 124),
        "Eje horizontal: crecimiento anualizado, 2010--2025. Eje vertical: nivel en 2025. Tamaño de burbuja: PIB de la rama.",
        fill="#555555",
        font=subtitle_font,
    )
    draw.text(
        (75, 169),
        "Los colores y las cuatro categorías se definen con base en el panel de productividad por hora.",
        fill="#555555",
        font=subtitle_font,
    )

    draw_panel(
        top=330,
        bottom=1330,
        y_min=0,
        y_max=305,
        y_ticks=[0, 50, 100, 150, 200, 250, 300],
        x_col="growth_worker",
        y_col="level_worker",
        agg_growth=agg_growth_worker,
        agg_level=agg_level_worker,
        panel_title="PIB por trabajador",
        y_axis_label="Millones de pesos constantes de 2015 por ocupado",
        x_axis_label=None,
        label_offsets=worker_offsets,
        show_quadrants=False,
        show_size_legend=True,
    )

    draw_panel(
        top=1630,
        bottom=2630,
        y_min=0,
        y_max=130,
        y_ticks=[0, 20, 40, 60, 80, 100, 120],
        x_col="growth_hour",
        y_col="level_hour",
        agg_growth=agg_growth_hour,
        agg_level=agg_level_hour,
        panel_title="PIB por hora trabajada",
        y_axis_label="Miles de pesos constantes de 2015 por hora trabajada",
        x_axis_label="Crecimiento anual de la productividad, 2010--2025",
        label_offsets=hour_offsets,
        show_quadrants=True,
        show_size_legend=False,
    )

    draw.text(
        (75, 2890),
        "Fuente: cálculos propios con DANE y GEIH. El tamaño de la burbuja es proporcional al PIB real de cada rama en 2025.",
        fill="#555555",
        font=note_font,
    )

    output_name = "fig_pib_geih_productividad_burbujas_2025.png"
    img.convert("RGB").save(FIGURE_DIR / output_name, quality=95)
    img.convert("RGB").save(OUTPUT_FIGURE_DIR / output_name, quality=95)


if __name__ == "__main__":
    main()
