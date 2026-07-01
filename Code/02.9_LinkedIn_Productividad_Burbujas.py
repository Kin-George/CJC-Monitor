from pathlib import Path
from textwrap import wrap

import numpy as np
import pandas as pd
from PIL import Image, ImageDraw, ImageFont


ROOT = Path(__file__).resolve().parents[1]
TABLE_DIR = ROOT / "Paper" / "tables"
PAPER_FIG_DIR = ROOT / "Paper" / "figures"
OUTPUT_FIG_DIR = ROOT / "Outputs" / "Figures"

for directory in (PAPER_FIG_DIR, OUTPUT_FIG_DIR):
    directory.mkdir(parents=True, exist_ok=True)


def font(size: int, bold: bool = False) -> ImageFont.FreeTypeFont:
    candidates = [
        Path(r"C:\Windows\Fonts\arialbd.ttf" if bold else r"C:\Windows\Fonts\arial.ttf"),
        Path(r"C:\Windows\Fonts\segoeuib.ttf" if bold else r"C:\Windows\Fonts\segoeui.ttf"),
    ]
    for path in candidates:
        if path.exists():
            return ImageFont.truetype(str(path), size)
    return ImageFont.load_default()


def fmt_es(value: float, digits: int = 1) -> str:
    return f"{value:.{digits}f}".replace(".", ",")


def text_size(draw: ImageDraw.ImageDraw, text: str, used_font: ImageFont.ImageFont) -> tuple[int, int]:
    bbox = draw.textbbox((0, 0), text, font=used_font)
    return bbox[2] - bbox[0], bbox[3] - bbox[1]


def draw_multiline(
    draw: ImageDraw.ImageDraw,
    xy: tuple[float, float],
    text: str,
    used_font: ImageFont.ImageFont,
    fill: str,
    anchor: str = "la",
    line_gap: int = 5,
) -> None:
    x, y = xy
    lines = str(text).split("\n")
    line_h = text_size(draw, "Ag", used_font)[1] + line_gap
    for i, line in enumerate(lines):
        draw.text((x, y + i * line_h), line, font=used_font, fill=fill, anchor=anchor)


def draw_rotated_text(
    img: Image.Image,
    center_xy: tuple[float, float],
    text: str,
    used_font: ImageFont.ImageFont,
    fill: str,
    angle: int = 90,
) -> None:
    padding = 18
    dummy = Image.new("RGBA", (1, 1), (255, 255, 255, 0))
    dummy_draw = ImageDraw.Draw(dummy)
    bbox = dummy_draw.multiline_textbbox((0, 0), text, font=used_font, spacing=8)
    w, h = bbox[2] - bbox[0] + padding * 2, bbox[3] - bbox[1] + padding * 2
    layer = Image.new("RGBA", (w, h), (255, 255, 255, 0))
    layer_draw = ImageDraw.Draw(layer)
    layer_draw.multiline_text((padding, padding), text, font=used_font, fill=fill, spacing=8, align="center")
    rotated = layer.rotate(angle, expand=True, resample=Image.Resampling.BICUBIC)
    x = int(center_xy[0] - rotated.width / 2)
    y = int(center_xy[1] - rotated.height / 2)
    img.paste(rotated, (x, y), rotated)


def save_reveal_gif(img: Image.Image, out_path: Path) -> None:
    gif_w, gif_h = 1200, 675
    base = img.resize((gif_w, gif_h), Image.Resampling.LANCZOS)
    bg = Image.new("RGB", (gif_w, gif_h), "#FBFAF7")
    frames = []

    for i in range(34):
        t = i / 33
        eased = 1 - (1 - t) ** 3
        reveal_w = max(1, int(gif_w * eased))
        frame = bg.copy()
        frame.paste(base.crop((0, 0, reveal_w, gif_h)), (0, 0))

        if reveal_w < gif_w:
            fade_w = 45
            edge = Image.new("RGBA", (fade_w, gif_h), (251, 250, 247, 0))
            edge_draw = ImageDraw.Draw(edge)
            for x in range(fade_w):
                alpha = int(255 * (x / fade_w) ** 1.7)
                edge_draw.line((x, 0, x, gif_h), fill=(251, 250, 247, alpha))
            frame.paste(edge, (max(0, reveal_w - fade_w), 0), edge)

        frames.append(frame)

    frames.extend([base.copy() for _ in range(18)])
    frames[0].save(
        out_path,
        save_all=True,
        append_images=frames[1:],
        duration=55,
        loop=0,
        optimize=True,
    )


def save_bubble_motion_gif(data: pd.DataFrame, x_agg: float, y_agg: float, out_path: Path) -> None:
    W, H = 1200, 675
    frames = []

    title_font = font(27, bold=True)
    subtitle_font = font(13)
    axis_font = font(12)
    tick_font = font(10)
    label_font = font(10)
    bold_small = font(12, bold=True)
    note_font = font(9)

    ink = "#17212B"
    muted = "#64707A"
    grid = "#E8E1D8"
    axis = "#98A1A8"
    blue = "#86B5EA"
    colors = {
        "Lideres en auge": "#168A73",
        "Lideres en declive": "#2F6FAD",
        "Aceleradoras": "#F2A93B",
        "Rezagadas": "#C85A4A",
    }

    plot_left, plot_top, plot_right, plot_bottom = 102, 125, 1112, 527
    x_min, x_max = 0, 128
    y_min, y_max = -4.2, 8.1

    label_positions = {
        "Agropecuario": (1.4, 4.0, "la"),
        "Minas": (49.0, -3.1, "la"),
        "Manufactura": (20.9, 0.55, "la"),
        "Servicios\npúblicos": (37.0, -3.65, "ra"),
        "Construcción": (12.1, -3.1, "la"),
        "Comercio,\ntransporte y alojamiento": (2.2, 0.65, "la"),
        "Información y\ncomunicaciones": (35.2, 5.0, "la"),
        "Financieras": (55.5, 1.55, "la"),
        "Inmobiliarias": (108.2, 0.72, "ra"),
        "Profesionales y\nadministrativas": (22.1, -1.3, "la"),
        "Adm. pública,\neducación y salud": (27.1, 1.2, "la"),
        "Arte, recreación\ny otros servicios": (17.2, 8.15, "la"),
        "Hogares como\nempleadores": (5.5, 3.0, "la"),
    }

    def x_pos(x: float) -> float:
        return plot_left + (x - x_min) / (x_max - x_min) * (plot_right - plot_left)

    def y_pos(y: float) -> float:
        return plot_bottom - (y - y_min) / (y_max - y_min) * (plot_bottom - plot_top)

    def radius(pib_billones: float) -> float:
        return 6.5 + np.sqrt(max(pib_billones, 0)) * 1.75

    def ease(t: float) -> float:
        t = min(max(t, 0), 1)
        return 1 - (1 - t) ** 3

    def draw_static() -> Image.Image:
        img = Image.new("RGB", (W, H), "#FBFAF7")
        draw = ImageDraw.Draw(img)
        draw.text((58, 32), "Productividad por hora: nivel y crecimiento en 13 ramas", fill=ink, font=title_font)
        draw.text(
            (58, 65),
            "PIB por hora en 2025 vs. crecimiento anualizado 2010-2025. El tamaño de burbuja muestra el PIB de la rama.",
            fill=muted,
            font=subtitle_font,
        )
        draw.rounded_rectangle((plot_left - 14, plot_top - 13, plot_right + 12, plot_bottom + 12), radius=14, fill="#FFFDF8")

        for tick in range(0, 121, 20):
            x = x_pos(tick)
            draw.line((x, plot_top, x, plot_bottom), fill=grid, width=1)
            draw.text((x, plot_bottom + 15), str(tick), fill=muted, font=tick_font, anchor="ma")
        for tick in range(-4, 9, 2):
            y = y_pos(tick)
            draw.line((plot_left, y, plot_right, y), fill=grid, width=1)
            draw.text((plot_left - 12, y), f"{tick}%", fill=muted, font=tick_font, anchor="rm")

        draw.line((plot_left, plot_bottom, plot_right, plot_bottom), fill=axis, width=2)
        draw.line((plot_left, plot_top, plot_left, plot_bottom), fill=axis, width=2)
        draw.line((x_pos(x_agg), plot_top, x_pos(x_agg), plot_bottom), fill=blue, width=3)
        draw.line((plot_left, y_pos(y_agg), plot_right, y_pos(y_agg)), fill=blue, width=3)
        draw.text((x_pos(x_agg) + 8, y_pos(5.45)), f"Agregado: {fmt_es(x_agg)}", fill="#397DC0", font=bold_small)
        draw.text((x_pos(109), y_pos(y_agg) - 18), f"Agregado: {fmt_es(y_agg)}%", fill="#397DC0", font=bold_small)

        q_color = "#2366A8"
        draw.text((plot_left + 9, plot_top + 11), "Aceleradoras", fill=q_color, font=bold_small)
        draw.text((x_pos(x_agg) + 14, plot_top + 34), "Líderes en auge", fill=q_color, font=bold_small)
        draw.text((x_pos(63), y_pos(y_agg) + 14), "Líderes en declive", fill=q_color, font=bold_small)
        draw.text((plot_left + 9, y_pos(y_agg) + 14), "Rezagadas", fill=q_color, font=bold_small)

        draw.text(((plot_left + plot_right) / 2, H - 80), "PIB por hora trabajada en 2025 (miles de pesos constantes de 2015)", fill=ink, font=axis_font, anchor="ma")
        draw.text((33, (plot_top + plot_bottom) / 2), "Crecimiento anual\n2010-2025", fill=ink, font=axis_font, anchor="mm")

        lx, ly = 882, 180
        draw.text((lx, ly - 46), "PIB 2025", fill=ink, font=bold_small)
        draw.text((lx, ly - 29), "Billones de pesos de 2015", fill=muted, font=label_font)
        for i, val in enumerate([25, 75, 175]):
            cx = lx + 13 + i * 62
            r = radius(val)
            draw.ellipse((cx - r, ly - r, cx + r, ly + r), fill="#8EA4B1", outline="white", width=2)
            draw.text((cx, ly + 31), str(val), fill=muted, font=tick_font, anchor="ma")

        legend_y = H - 53
        legend_x = 58
        for name, col in colors.items():
            draw.ellipse((legend_x, legend_y - 4, legend_x + 11, legend_y + 7), fill=col, outline="white", width=1)
            label = name.replace("Lideres", "Líderes")
            draw.text((legend_x + 16, legend_y - 5), label, fill=ink, font=label_font)
            legend_x += text_size(draw, label, label_font)[0] + 46

        draw.text((58, H - 27), "Fuente: cálculos propios con DANE, PIB trimestral por producción y GEIH.", fill="#6B747C", font=note_font)
        draw.text((W - 60, H - 27), "Productividad laboral | Colombia", fill=ink, font=font(10, bold=True), anchor="ra")
        return img

    ordered = data.sort_values(["cuadrante", "pib_2025_billones"], ascending=[True, False]).reset_index(drop=True)
    n_frames = 62
    start_x, start_y = x_pos(x_agg), y_pos(y_agg)

    for frame_idx in range(n_frames):
        img = draw_static()
        draw = ImageDraw.Draw(img)
        label_phase = frame_idx >= n_frames - 13

        for bubble_idx, row in ordered.iterrows():
            p = ease((frame_idx - bubble_idx * 2.1) / 24)
            if p <= 0:
                continue
            final_x, final_y = x_pos(row["x"]), y_pos(row["y"])
            current_x = start_x + (final_x - start_x) * p
            current_y = start_y + (final_y - start_y) * p
            r = radius(row["pib_2025_billones"]) * p
            draw.ellipse(
                (current_x - r, current_y - r, current_x + r, current_y + r),
                fill=colors[row["cuadrante"]],
                outline="white",
                width=2,
            )

        if label_phase:
            for _, row in data.iterrows():
                point_x, point_y = x_pos(row["x"]), y_pos(row["y"])
                label_data_x, label_data_y, anchor = label_positions.get(row["label"], (row["x"] + 2, row["y"], "la"))
                label_x, label_y = x_pos(label_data_x), y_pos(label_data_y)
                if abs(label_x - point_x) > 30 or abs(label_y - point_y) > 23:
                    draw.line((point_x, point_y, label_x, label_y), fill="#C9D1D6", width=1)
                draw_multiline(draw, (label_x, label_y), row["label"], label_font, "#263238", anchor=anchor, line_gap=2)

        frames.append(img)

    frames.extend([frames[-1].copy() for _ in range(24)])
    frames[0].save(
        out_path,
        save_all=True,
        append_images=frames[1:],
        duration=45,
        loop=0,
        optimize=True,
    )


def load_data() -> tuple[pd.DataFrame, float, float]:
    summary = pd.read_csv(TABLE_DIR / "pib_geih_productividad_sector_summary.csv")
    series = pd.read_csv(TABLE_DIR / "pib_geih_productividad_sector_series.csv")
    total = pd.read_csv(TABLE_DIR / "pib_geih_productividad_total_summary.csv")

    pib_2025 = (
        series.loc[series["anio"] == 2025, ["sector_code", "pib_miles_millones_2015"]]
        .rename(columns={"pib_miles_millones_2015": "pib_2025_miles_millones"})
    )

    data = summary.merge(pib_2025, on="sector_code", how="left")
    data["x"] = data["pib_hora_2025"] / 1000
    data["y"] = data["crec_pib_hora"] * 100
    data["pib_2025_billones"] = data["pib_2025_miles_millones"] / 1000
    data["label"] = data["sector"].replace(
        {
            "Comercio, transporte y alojamiento": "Comercio,\ntransporte y alojamiento",
            "Adm. pública, educación y salud": "Adm. pública,\neducación y salud",
            "Profesionales y administrativas": "Profesionales y\nadministrativas",
            "Arte, recreación y otros servicios": "Arte, recreación\ny otros servicios",
            "Información y comunicaciones": "Información y\ncomunicaciones",
            "Servicios públicos": "Servicios\npúblicos",
            "Hogares empleadores": "Hogares como\nempleadores",
        }
    )

    total_hour = total.loc[total["indicador"] == "PIB por hora trabajada"].iloc[0]
    x_agg = total_hour["valor_2025"] / 1000
    y_agg = total_hour["crecimiento_anualizado"] * 100

    data["cuadrante"] = np.select(
        [
            (data["x"] >= x_agg) & (data["y"] >= y_agg),
            (data["x"] >= x_agg) & (data["y"] < y_agg),
            (data["x"] < x_agg) & (data["y"] >= y_agg),
        ],
        ["Lideres en auge", "Lideres en declive", "Aceleradoras"],
        default="Rezagadas",
    )
    return data, x_agg, y_agg


def main() -> None:
    data, x_agg, y_agg = load_data()

    W, H = 2400, 1350
    img = Image.new("RGB", (W, H), "#FBFAF7")
    draw = ImageDraw.Draw(img)

    title_font = font(52, bold=True)
    subtitle_font = font(25)
    axis_font = font(23)
    tick_font = font(21)
    label_font = font(22)
    label_small = font(19)
    bold_small = font(23, bold=True)
    note_font = font(18)

    ink = "#17212B"
    muted = "#64707A"
    grid = "#E8E1D8"
    axis = "#98A1A8"
    blue = "#86B5EA"
    colors = {
        "Lideres en auge": "#168A73",
        "Lideres en declive": "#2F6FAD",
        "Aceleradoras": "#F2A93B",
        "Rezagadas": "#C85A4A",
    }

    plot_left, plot_top, plot_right, plot_bottom = 205, 250, 2225, 1055
    x_min, x_max = 0, 128
    y_min, y_max = -4.2, 8.1

    def x_pos(x: float) -> float:
        return plot_left + (x - x_min) / (x_max - x_min) * (plot_right - plot_left)

    def y_pos(y: float) -> float:
        return plot_bottom - (y - y_min) / (y_max - y_min) * (plot_bottom - plot_top)

    def radius(pib_billones: float) -> float:
        return 13 + np.sqrt(max(pib_billones, 0)) * 3.5

    draw.text((115, 65), "Productividad por hora: nivel y crecimiento en 13 ramas", fill=ink, font=title_font)
    draw.text(
        (115, 130),
        "PIB por hora en 2025 vs. crecimiento anualizado 2010-2025. El tamaño de burbuja muestra el PIB de la rama.",
        fill=muted,
        font=subtitle_font,
    )

    # Plot background and grid.
    draw.rounded_rectangle((plot_left - 28, plot_top - 26, plot_right + 25, plot_bottom + 25), radius=28, fill="#FFFDF8")

    for tick in range(0, 121, 20):
        x = x_pos(tick)
        draw.line((x, plot_top, x, plot_bottom), fill=grid, width=2)
        draw.text((x, plot_bottom + 30), str(tick), fill=muted, font=tick_font, anchor="ma")

    for tick in range(-4, 9, 2):
        y = y_pos(tick)
        draw.line((plot_left, y, plot_right, y), fill=grid, width=2)
        draw.text((plot_left - 25, y), f"{tick}%", fill=muted, font=tick_font, anchor="rm")

    draw.line((plot_left, plot_bottom, plot_right, plot_bottom), fill=axis, width=3)
    draw.line((plot_left, plot_top, plot_left, plot_bottom), fill=axis, width=3)

    # Aggregate lines.
    draw.line((x_pos(x_agg), plot_top, x_pos(x_agg), plot_bottom), fill=blue, width=5)
    draw.line((plot_left, y_pos(y_agg), plot_right, y_pos(y_agg)), fill=blue, width=5)
    draw.text(
        (x_pos(x_agg) + 15, y_pos(5.45)),
        f"Agregado: {fmt_es(x_agg)}",
        fill="#397DC0",
        font=bold_small,
        anchor="la",
    )
    draw.text(
        (x_pos(109), y_pos(y_agg) - 35),
        f"Agregado: {fmt_es(y_agg)}%",
        fill="#397DC0",
        font=bold_small,
        anchor="la",
    )

    # Quadrant labels.
    q_color = "#2366A8"
    draw.text((plot_left + 18, plot_top + 22), "Aceleradoras", fill=q_color, font=bold_small)
    draw.text((x_pos(x_agg) + 28, plot_top + 68), "Líderes en auge", fill=q_color, font=bold_small)
    draw.text((x_pos(63), y_pos(y_agg) + 28), "Líderes en declive", fill=q_color, font=bold_small)
    draw.text((plot_left + 18, y_pos(y_agg) + 28), "Rezagadas", fill=q_color, font=bold_small)

    # Bubbles.
    for _, row in data.sort_values("pib_2025_billones", ascending=False).iterrows():
        x, y = x_pos(row["x"]), y_pos(row["y"])
        r = radius(row["pib_2025_billones"])
        fill_col = colors[row["cuadrante"]]
        draw.ellipse((x - r, y - r, x + r, y + r), fill=fill_col, outline="white", width=4)

    label_positions = {
        "Agropecuario": (1.4, 4.0, "la"),
        "Minas": (49.0, -3.1, "la"),
        "Manufactura": (20.9, 0.55, "la"),
        "Servicios\npúblicos": (37.0, -3.65, "ra"),
        "Construcción": (12.1, -3.1, "la"),
        "Comercio,\ntransporte y alojamiento": (2.2, 0.65, "la"),
        "Información y\ncomunicaciones": (35.2, 5.0, "la"),
        "Financieras": (55.5, 1.55, "la"),
        "Inmobiliarias": (108.2, 0.72, "ra"),
        "Profesionales y\nadministrativas": (22.1, -1.3, "la"),
        "Adm. pública,\neducación y salud": (27.1, 1.2, "la"),
        "Arte, recreación\ny otros servicios": (17.2, 8.15, "la"),
        "Hogares como\nempleadores": (5.5, 3.0, "la"),
    }

    for _, row in data.iterrows():
        point_x, point_y = x_pos(row["x"]), y_pos(row["y"])
        label_data_x, label_data_y, anchor = label_positions.get(row["label"], (row["x"] + 2, row["y"], "la"))
        label_x, label_y = x_pos(label_data_x), y_pos(label_data_y)
        if abs(label_x - point_x) > 60 or abs(label_y - point_y) > 45:
            draw.line((point_x, point_y, label_x, label_y), fill="#C9D1D6", width=2)
        draw_multiline(draw, (label_x, label_y), row["label"], label_small, "#263238", anchor=anchor, line_gap=4)

    # Axis titles.
    draw.text(
        ((plot_left + plot_right) / 2, H - 160),
        "PIB por hora trabajada en 2025 (miles de pesos constantes de 2015)",
        fill=ink,
        font=axis_font,
        anchor="ma",
    )
    draw_rotated_text(
        img,
        (72, (plot_top + plot_bottom) / 2),
        "Crecimiento anual del PIB por hora trabajada\n2010-2025",
        axis_font,
        ink,
        angle=90,
    )

    # Bubble legend.
    lx, ly = 1765, 360
    draw.text((lx, ly - 92), "PIB 2025", fill=ink, font=bold_small)
    draw.text((lx, ly - 58), "Billones de pesos de 2015", fill=muted, font=label_small)
    for i, val in enumerate([25, 75, 175]):
        cx = lx + 25 + i * 125
        r = radius(val)
        draw.ellipse((cx - r, ly - r, cx + r, ly + r), fill="#8EA4B1", outline="white", width=3)
        draw.text((cx, ly + 62), str(val), fill=muted, font=tick_font, anchor="ma")

    # Color legend.
    legend_y = H - 105
    legend_x = 115
    for name, col in colors.items():
        draw.ellipse((legend_x, legend_y - 8, legend_x + 22, legend_y + 14), fill=col, outline="white", width=2)
        label = name.replace("Lideres", "Líderes")
        draw.text((legend_x + 32, legend_y - 10), label, fill=ink, font=label_small)
        legend_x += text_size(draw, label, label_small)[0] + 92

    source = (
        "Fuente: cálculos propios con DANE, PIB trimestral por producción y GEIH. "
        "Se excluye 2020 por no contar con GEIH anual comparable."
    )
    for i, line in enumerate(wrap(source, width=145)):
        draw.text((115, H - 55 + i * 24), line, fill="#6B747C", font=note_font)
    draw.text((W - 120, H - 54), "Productividad laboral | Colombia", fill=ink, font=font(20, bold=True), anchor="ra")

    out_name = "fig_linkedin_productividad_burbujas_2025.png"
    gif_name = "fig_linkedin_productividad_burbujas_2025.gif"
    for directory in (PAPER_FIG_DIR, OUTPUT_FIG_DIR):
        img.save(directory / out_name, quality=95)
        save_bubble_motion_gif(data, x_agg, y_agg, directory / gif_name)


if __name__ == "__main__":
    main()
