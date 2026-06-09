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
    "R+S+T": "Artes, entretenimiento y hogares",
}

SECTOR_INTRO_LEAD = {
    "A": "La actividad agropecuaria comprende cultivos, apoyo agropecuario y actividades mixtas; café; ganadería; silvicultura; y pesca y acuicultura.",
    "B": "La actividad de minas y canteras comprende petróleo, gas y apoyo conexo; otras minas y canteras; carbón; y minerales metalíferos.",
    "C": "La actividad manufacturera agrupa alimentos, bebidas y tabaco; textiles, confecciones y cuero; madera, papel e impresión; refinación, químicos y minerales; metalurgia, maquinaria y equipo; y muebles y otras manufactureras.",
    "D+E": "La actividad de servicios públicos combina electricidad, gas y vapor con agua, saneamiento y desechos.",
    "F": "La actividad de construcción comprende edificaciones, obras civiles y actividades especializadas de construcción.",
    "G+H+I": "La agrupación de comercio, transporte y alojamiento combina comercio y reparación; transporte y almacenamiento; y alojamiento y servicios de comida.",
    "J": "La actividad de información y comunicaciones reúne edición y contenidos audiovisuales, radio y televisión, telecomunicaciones, desarrollo de software, procesamiento de datos, portales web y otros servicios de información.",
    "K": "La actividad financiera reúne intermediación financiera y banca, seguros, fondos de pensiones y servicios auxiliares como administración de mercados, corretaje y otras actividades de apoyo financiero.",
    "L": "La actividad inmobiliaria reúne alquiler, compra, venta, administración e intermediación de bienes inmuebles, tanto propios o arrendados como de terceros.",
    "M+N": "La agrupación de actividades profesionales y administrativas combina actividades profesionales, científicas y técnicas con servicios administrativos y de apoyo.",
    "O+P+Q": "La agrupación de administración pública, educación y salud combina administración pública y defensa, educación, salud humana y servicios sociales.",
    "R+S+T": "La agrupación de artes, entretenimiento y hogares combina artes, entretenimiento y otros servicios con los hogares como empleadores.",
}

SECTOR_CIIU_CODES = {
    "A": "A; divisiones 01--03",
    "B": "B; divisiones 05--09",
    "C": "C; divisiones 10--33",
    "D+E": "D y E; divisiones 35--39",
    "F": "F; divisiones 41--43",
    "G+H+I": "G, H e I; divisiones 45--56",
    "J": "J; divisiones 58--63",
    "K": "K; divisiones 64--66",
    "L": "L; división 68",
    "M+N": "M y N; divisiones 69--82",
    "O+P+Q": "O, P y Q; divisiones 84--88",
    "R+S+T": "R, S y T; divisiones 90--98",
}

SECTOR_BODY_SUBJECT = {
    "A": "la actividad agropecuaria",
    "B": "la actividad de minas y canteras",
    "C": "la actividad manufacturera",
    "D+E": "la actividad de servicios públicos",
    "F": "la actividad de construcción",
    "G+H+I": "la agrupación de comercio, transporte y alojamiento",
    "J": "la actividad de información y comunicaciones",
    "K": "la actividad financiera",
    "L": "la actividad inmobiliaria",
    "M+N": "la agrupación de actividades profesionales y administrativas",
    "O+P+Q": "la agrupación de administración pública, educación y salud",
    "R+S+T": "la agrupación de artes, entretenimiento y hogares",
}

SECTOR_PRODUCTIVITY_SUBJECT = {
    "A": "del sector agropecuario",
    "B": "de minas y canteras",
    "C": "de la manufactura",
    "D+E": "de los servicios públicos",
    "F": "de la construcción",
    "G+H+I": "de comercio, transporte y alojamiento",
    "J": "de información y comunicaciones",
    "K": "de las actividades financieras",
    "L": "de las actividades inmobiliarias",
    "M+N": "de las actividades profesionales y administrativas",
    "O+P+Q": "de administración pública, educación y salud",
    "R+S+T": "de artes, entretenimiento y hogares",
}

SECTOR_ZOOM_CAPTION = {
    "A": "agropecuarias",
    "B": "mineras",
    "C": "manufactureras",
    "D+E": "de servicios públicos",
    "F": "de construcción",
    "G+H+I": "de comercio, transporte y alojamiento",
    "J": "de información y comunicaciones",
    "K": "financieras",
    "L": "inmobiliarias",
    "M+N": "profesionales y administrativas",
    "O+P+Q": "de administración pública, educación y salud",
    "R+S+T": "artísticas, de entretenimiento y hogares",
}

SECTOR_ZOOM_CONTEXT = {
    "A": "del sector agropecuario",
    "B": "de la actividad de minas y canteras",
    "C": "de la manufactura",
    "D+E": "de los servicios públicos",
    "F": "de la construcción",
    "G+H+I": "de la agrupación de comercio, transporte y alojamiento",
    "J": "de información y comunicaciones",
    "K": "de las actividades financieras",
    "L": "de las actividades inmobiliarias",
    "M+N": "de las actividades profesionales y administrativas",
    "O+P+Q": "de administración pública, educación y salud",
    "R+S+T": "de artes, entretenimiento y hogares",
}

AGG25_SHORT = {
    "A": "Agropecuario",
    "B": "Minas",
    "C01": "Alimentos, bebidas y tabaco",
    "C02": "Textiles, confecciones y cuero",
    "C03": "Madera, papel e impresión",
    "C04": "Refinación, químicos y minerales",
    "C05": "Metalurgia, maquinaria y equipo",
    "C06": "Muebles y otras manufactureras",
    "D": "Electricidad, gas y vapor",
    "E": "Agua, saneamiento y desechos",
    "F01": "Edificaciones",
    "F02": "Obras civiles",
    "F03": "Actividades especializadas de construcción",
    "G": "Comercio y reparación",
    "H": "Transporte y almacenamiento",
    "I": "Alojamiento y comida",
    "J": "Información y comunicaciones",
    "K": "Financieras y seguros",
    "L": "Inmobiliarias",
    "M+N": "Profesionales y administrativas",
    "O": "Administración pública",
    "P": "Educación",
    "Q": "Salud y servicios sociales",
    "R+S": "Artes, entretenimiento y otros servicios",
    "T": "Hogares como empleadores",
}

AGG61_SHORT = {
    "104-108": "Artes, entretenimiento, recreación y otros servicios",
    "075": "Transporte aéreo",
    "085-088": "Actividades financieras y de seguros",
    "102,103": "Salud humana y servicios sociales",
    "098,099": "Administración pública y defensa",
    "101": "Educación de no mercado",
    "076": "Almacenamiento y apoyo al transporte",
    "016": "Pesca y acuicultura",
    "070": "Comercio al por mayor y al por menor",
    "052": "Aparatos eléctricos, electrónicos y ópticos",
    "019": "Minerales metalíferos",
    "039": "Cuero, calzado y artículos de viaje",
    "017": "Carbón de piedra y lignito",
    "022": "Apoyo a otras actividades mineras",
    "073": "Transporte acuático",
    "067": "Edificaciones residenciales y no residenciales",
    "040": "Madera y productos de madera",
    "018,021": "Petróleo, gas natural y apoyo conexo",
    "053,057": "Maquinaria y equipo; instalación y mantenimiento",
    "020": "Otras minas y canteras",
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


AGG25_ORDER = [
    "A",
    "B",
    "C01",
    "C02",
    "C03",
    "C04",
    "C05",
    "C06",
    "D",
    "E",
    "F01",
    "F02",
    "F03",
    "G",
    "H",
    "I",
    "J",
    "K",
    "L",
    "M+N",
    "O",
    "P",
    "Q",
    "R+S",
    "T",
]

AGG25_DESCRIPTION = {
    "A": "agricultura, ganadería, caza, silvicultura y pesca",
    "B": "explotación de minas y canteras",
    "C01": "elaboración de productos alimenticios, bebidas y tabaco",
    "C02": "textiles, confecciones, cuero, calzado y artículos de viaje",
    "C03": "madera, papel, cartón, impresión y reproducción de grabaciones",
    "C04": "refinación, químicos, farmacéuticos, caucho, plástico y minerales no metálicos",
    "C05": "metalurgia, productos metálicos, equipo eléctrico, productos electrónicos, maquinaria, vehículos y equipo de transporte",
    "C06": "muebles, colchones, somieres y otras industrias manufactureras",
    "D": "suministro de electricidad, gas, vapor y aire acondicionado",
    "E": "agua, saneamiento, residuos, desechos y saneamiento ambiental",
    "F01": "construcción de edificaciones residenciales y no residenciales",
    "F02": "construcción de carreteras, vías férreas, proyectos de servicio público y obras de ingeniería civil",
    "F03": "actividades especializadas para construcción de edificaciones y obras de ingeniería civil",
    "G": "comercio al por mayor y al por menor, y reparación de vehículos automotores y motocicletas",
    "H": "transporte y almacenamiento",
    "I": "alojamiento y servicios de comida",
    "J": "información y comunicaciones",
    "K": "actividades financieras y de seguros",
    "L": "actividades inmobiliarias",
    "M+N": "actividades profesionales, científicas y técnicas, y servicios administrativos y de apoyo",
    "O": "administración pública y defensa, y planes de seguridad social de afiliación obligatoria",
    "P": "educación",
    "Q": "actividades de atención de la salud humana y de servicios sociales",
    "R+S": "actividades artísticas, entretenimiento, recreación y otros servicios",
    "T": "actividades de los hogares como empleadores y producción no diferenciada de los hogares para uso propio",
}

AGG25_CIIU_CODES = {
    "A": "A; divisiones 01--03",
    "B": "B; divisiones 05--09",
    "C01": "C; divisiones 10--12",
    "C02": "C; divisiones 13--15",
    "C03": "C; divisiones 16--18",
    "C04": "C; divisiones 19--23",
    "C05": "C; divisiones 24--30 y 33",
    "C06": "C; divisiones 31--32",
    "D": "D; división 35",
    "E": "E; divisiones 36--39",
    "F01": "F; división 41",
    "F02": "F; división 42",
    "F03": "F; división 43",
    "G": "G; divisiones 45--47",
    "H": "H; divisiones 49--53",
    "I": "I; divisiones 55--56",
    "J": "J; divisiones 58--63",
    "K": "K; divisiones 64--66",
    "L": "L; división 68",
    "M+N": "M y N; divisiones 69--82",
    "O": "O; división 84",
    "P": "P; división 85",
    "Q": "Q; divisiones 86--88",
    "R+S": "R y S; divisiones 90--96",
    "T": "T; divisiones 97--98",
}

AGG25_POOLS = [
    {"subramas": [1, 2, 3], "groups": ["A"]},
    {"subramas": [4, 5], "groups": ["B"]},
    {"subramas": [6], "groups": ["C01"]},
    {"subramas": [7, 8], "groups": ["C02"]},
    {"subramas": [9, 10], "groups": ["C03"]},
    {"subramas": [11, 12], "groups": ["C04"]},
    {"subramas": [13, 14, 15], "groups": ["C05"]},
    {"subramas": [16], "groups": ["C06"]},
    {"subramas": [17], "groups": ["D"]},
    {"subramas": [18], "groups": ["E"]},
    {"subramas": [19], "groups": ["F01", "F02", "F03"]},
    {"subramas": [20, 21, 22], "groups": ["G"]},
    {"subramas": [25, 26, 27], "groups": ["H"]},
    {"subramas": [23, 24], "groups": ["I"]},
    {"subramas": [28, 29, 30, 31, 32], "groups": ["J"]},
    {"subramas": [33, 34], "groups": ["K"]},
    {"subramas": [35], "groups": ["L"]},
    {"subramas": [36, 37, 38], "groups": ["M+N"]},
    {"subramas": [39], "groups": ["O"]},
    {"subramas": [40], "groups": ["P"]},
    {"subramas": [41, 42], "groups": ["Q"]},
    {"subramas": [43, 44], "groups": ["R+S"]},
    {"subramas": [45], "groups": ["T"]},
]

AGG25_TO_SECTOR = {}
for pool in AGG25_POOLS:
    pool_sectors = {
        SUBRAMA_TO_SECTOR[subrama]
        for subrama in pool["subramas"]
        if subrama in SUBRAMA_TO_SECTOR
    }
    if len(pool_sectors) == 1:
        for group_code in pool["groups"]:
            AGG25_TO_SECTOR[group_code] = next(iter(pool_sectors))

AGG61_ORDER = [
    "001,002,004-008,013",
    "003",
    "009-012",
    "014,015",
    "016",
    "017",
    "018,021",
    "019",
    "020",
    "022",
    "023-025",
    "026",
    "027",
    "028,032,035",
    "029",
    "030,031",
    "033",
    "034",
    "036",
    "037,038",
    "039",
    "040",
    "041",
    "042",
    "043,044",
    "045-047",
    "048",
    "049",
    "050,051",
    "052",
    "053,057",
    "054",
    "055",
    "056",
    "058-060",
    "061",
    "062",
    "063,064,066",
    "065",
    "067",
    "068",
    "069",
    "070",
    "071",
    "072,074",
    "073",
    "075",
    "076",
    "077",
    "078-080",
    "081-084",
    "085-088",
    "089",
    "090-093",
    "094-097",
    "098,099",
    "100",
    "101",
    "102,103",
    "104-108",
    "109",
]

AGG61_POOLS = [
    {"subramas": [1], "groups": ["001,002,004-008,013", "003", "009-012"]},
    {"subramas": [2], "groups": ["014,015"]},
    {"subramas": [3], "groups": ["016"]},
    {"subramas": [4], "groups": ["017", "018,021", "022"]},
    {"subramas": [5], "groups": ["019", "020"]},
    {"subramas": [6], "groups": ["023-025", "026", "027", "028,032,035", "029", "030,031", "033", "034", "036"]},
    {"subramas": [7], "groups": ["037,038"]},
    {"subramas": [8], "groups": ["039"]},
    {"subramas": [9], "groups": ["040", "041"]},
    {"subramas": [10], "groups": ["042"]},
    {"subramas": [11], "groups": ["043,044", "045-047"]},
    {"subramas": [12], "groups": ["048", "049"]},
    {"subramas": [13], "groups": ["050,051"]},
    {"subramas": [14], "groups": ["052", "053,057"]},
    {"subramas": [15], "groups": ["054"]},
    {"subramas": [16], "groups": ["053,057", "055", "056"]},
    {"subramas": [17], "groups": ["058-060", "061"]},
    {"subramas": [18], "groups": ["062", "063,064,066", "065"]},
    {"subramas": [19], "groups": ["067", "068", "069"]},
    {"subramas": [20], "groups": ["070", "071"]},
    {"subramas": [21, 22], "groups": ["070"]},
    {"subramas": [25], "groups": ["072,074"]},
    {"subramas": [26], "groups": ["073", "075"]},
    {"subramas": [27], "groups": ["076", "077"]},
    {"subramas": [23, 24], "groups": ["078-080"]},
    {"subramas": [28, 29, 30, 31, 32], "groups": ["081-084"]},
    {"subramas": [33, 34], "groups": ["085-088"]},
    {"subramas": [35], "groups": ["089"]},
    {"subramas": [36, 37], "groups": ["090-093"]},
    {"subramas": [38], "groups": ["094-097"]},
    {"subramas": [39], "groups": ["098,099"]},
    {"subramas": [40], "groups": ["100", "101"]},
    {"subramas": [41, 42], "groups": ["102,103"]},
    {"subramas": [43, 44], "groups": ["104-108"]},
    {"subramas": [45], "groups": ["109"]},
]

AGG61_TO_SECTOR = {}
for pool in AGG61_POOLS:
    pool_sectors = {
        SUBRAMA_TO_SECTOR[subrama]
        for subrama in pool["subramas"]
        if subrama in SUBRAMA_TO_SECTOR
    }
    if len(pool_sectors) == 1:
        for group_code in pool["groups"]:
            AGG61_TO_SECTOR[group_code] = next(iter(pool_sectors))

AGG61_SHORT.update(
    {
        "001,002,004-008,013": "Cultivos, apoyo agropecuario y mixtos",
        "003": "Café",
        "009-012": "Ganadería",
        "014,015": "Silvicultura",
        "016": "Pesca y acuicultura",
        "017": "Carbón",
        "018,021": "Petróleo, gas y apoyo conexo",
        "019": "Minerales metalíferos",
        "020": "Otras minas y canteras",
        "022": "Apoyo a otras actividades mineras",
        "023-025": "Carnes, aves y pescado",
        "026": "Aceites y grasas",
        "027": "Lácteos",
        "028,032,035": "Molinería, panadería y alimentos",
        "029": "Productos de café",
        "030,031": "Azúcar y panela",
        "033": "Cacao y confitería",
        "034": "Frutas, hortalizas y otros alimentos",
        "036": "Bebidas y tabaco",
        "037,038": "Textiles y confecciones",
        "039": "Cuero, calzado y artículos de viaje",
        "040": "Madera",
        "041": "Papel y cartón",
        "042": "Impresión y reproducción",
        "043,044": "Coquización y refinación",
        "045-047": "Químicos y farmacéuticos",
        "048": "Caucho y plástico",
        "049": "Minerales no metálicos",
        "050,051": "Metalurgia y productos metálicos",
        "052": "Equipo eléctrico y electrónico",
        "053,057": "Maquinaria e instalación",
        "054": "Vehículos y equipo de transporte",
        "055": "Muebles y colchones",
        "056": "Otras manufactureras",
        "058-060": "Energía eléctrica",
        "061": "Gas, vapor y aire acondicionado",
        "062": "Agua",
        "063,064,066": "Aguas residuales, desechos y saneamiento",
        "065": "Reciclaje",
        "067": "Edificaciones",
        "068": "Obras civiles",
        "069": "Actividades especializadas de construcción",
        "070": "Comercio",
        "071": "Reparación de vehículos",
        "072,074": "Transporte terrestre y tuberías",
        "073": "Transporte acuático",
        "075": "Transporte aéreo",
        "076": "Almacenamiento y apoyo al transporte",
        "077": "Correo y mensajería",
        "078-080": "Alojamiento y comida",
        "081-084": "Información y comunicaciones",
        "085-088": "Financieras y seguros",
        "089": "Inmobiliarias",
        "090-093": "Profesionales, científicas y técnicas",
        "094-097": "Servicios administrativos y de apoyo",
        "098,099": "Administración pública y defensa",
        "100": "Educación de mercado",
        "101": "Educación de no mercado",
        "102,103": "Salud y servicios sociales",
        "104-108": "Artes, entretenimiento y otros servicios",
        "109": "Hogares como empleadores",
    }
)

COMPARABLE_LABEL_ALIASES = {
    "023-025|026|027|028,032,035|029|030,031|033|034|036": "Alimentos, bebidas y tabaco",
}

COMPARABLE_LABEL_NOTES = {
    "Alimentos, bebidas y tabaco": (
        "agrega manufacturas de carnes y pescado, aceites, lácteos, molinería y "
        "panadería, café, azúcar y panela, cacao y confitería, frutas y hortalizas, "
        "otros alimentos, bebidas y tabaco"
    ),
}


AGG25_LABOR_POOLS = [{"subramas": [code], "groups": [code]} for code in AGG25_ORDER]

AGG25_BY_SUBRAMA = {}
for pool in AGG25_POOLS:
    if len(pool["groups"]) == 1:
        for subrama in pool["subramas"]:
            AGG25_BY_SUBRAMA[subrama] = pool["groups"][0]

# Groups whose labor input can be identified from GEIH four-digit activity codes
# without allocating workers or hours mechanically by PIB shares.
AGG61_DIRECT_GROUPS = {
    "001,002,004-008,013",
    "003",
    "009-012",
    "014,015",
    "016",
    "017",
    "018,021",
    "019",
    "020",
    "022",
    "040",
    "041",
    "043,044",
    "045-047",
    "048",
    "049",
    "052",
    "053,057",
    "054",
    "055",
    "056",
    "058-060",
    "061",
    "062",
    "063,064,066",
    "065",
    "067",
    "068",
    "069",
    "073",
    "075",
    "076",
    "077",
}


def labor61_fallback_code(subrama: int) -> str:
    return f"S{subrama}"


def build_labor61_pools() -> list[dict[str, list]]:
    pools = []
    seen_direct = set()
    for pool in AGG61_POOLS:
        for group in pool["groups"]:
            if group in AGG61_DIRECT_GROUPS and group not in seen_direct:
                pools.append({"subramas": [group], "groups": [group]})
                seen_direct.add(group)
        fallback_groups = [
            group for group in pool["groups"] if group not in AGG61_DIRECT_GROUPS
        ]
        if fallback_groups:
            pools.append(
                {
                    "subramas": [
                        labor61_fallback_code(int(subrama))
                        for subrama in pool["subramas"]
                    ],
                    "groups": fallback_groups,
                }
            )
    return pools


AGG61_LABOR_POOLS = build_labor61_pools()


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


def latex_id(text: str) -> str:
    return (
        str(text)
        .lower()
        .replace("+", "_")
        .replace("|", "_")
        .replace(",", "_")
        .replace(";", "_")
        .replace(" ", "_")
    )


def indicator_with_unit(row: pd.Series) -> str:
    unit = str(row.get("unidad", "")).strip()
    indicator = str(row["indicador"]).strip()
    if unit:
        return f"{indicator} ({unit})"
    return indicator


def quarter_columns(raw: pd.DataFrame, first_data_col: int) -> list[tuple[int, int, str]]:
    year_row = raw.iloc[11]
    quarter_row = raw.iloc[12]
    columns = []
    current_year = None
    for col in range(first_data_col, raw.shape[1]):
        year = parse_year(year_row.iloc[col])
        if year is not None:
            current_year = year
        quarter = quarter_row.iloc[col]
        if current_year is not None and quarter in ["I", "II", "III", "IV"]:
            columns.append((col, current_year, str(quarter)))
    return columns


def annualize_quarterly(data: pd.DataFrame, group_cols: list[str]) -> pd.DataFrame:
    counts = data.groupby(group_cols + ["anio"])["trimestre"].nunique().reset_index(name="n_trim")
    complete = counts[counts["n_trim"] == 4][group_cols + ["anio"]]
    return (
        data.merge(complete, on=group_cols + ["anio"], how="inner")
        .groupby(group_cols + ["anio"], as_index=False)["pib_miles_millones_2015"]
        .sum()
    )


def load_pib_quarterly() -> tuple[pd.DataFrame, pd.DataFrame]:
    raw = pd.read_excel(PIB_XLSX, sheet_name="Cuadro 1", header=None)
    columns = quarter_columns(raw, 3)

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

    annual_sector = annualize_quarterly(sector_long, ["sector_code", "sector_name"])
    annual_total = annualize_quarterly(total_long, ["concepto"])
    return annual_total, annual_sector


def set_labor_code(
    data: pd.DataFrame, column: str, mask: pd.Series, code: str
) -> None:
    data.loc[mask.fillna(False), column] = code


def add_labor_disaggregation_codes(geih: pd.DataFrame) -> pd.DataFrame:
    geih = geih.copy()
    sub = geih["subrama_det_cod"]
    div = geih["rama4d_div"]
    three = geih["rama3d"]
    cls = geih["rama4d"]
    rev = geih["ciiu_revision_rama4d"].fillna("").astype(str)
    rev3 = rev.str.contains("Rev. 3", regex=False)
    rev4 = rev.str.contains("Rev. 4", regex=False)

    geih["labor25_code"] = sub.map(AGG25_BY_SUBRAMA)

    construction = sub == 19
    set_labor_code(geih, "labor25_code", construction & rev4 & (div == 41), "F01")
    set_labor_code(geih, "labor25_code", construction & rev4 & (div == 42), "F02")
    set_labor_code(geih, "labor25_code", construction & rev4 & (div == 43), "F03")
    set_labor_code(
        geih,
        "labor25_code",
        construction & rev3 & (cls.isin([4521, 4522])),
        "F01",
    )
    set_labor_code(geih, "labor25_code", construction & rev3 & (cls == 4530), "F02")
    set_labor_code(
        geih,
        "labor25_code",
        construction
        & rev3
        & (cls.isin([4511, 4512, 4541, 4542, 4543, 4549, 4551, 4552, 4559, 4560])),
        "F03",
    )

    geih["labor61_code"] = sub.map(
        lambda value: labor61_fallback_code(int(value)) if pd.notna(value) else np.nan
    )

    # Agropecuario
    set_labor_code(geih, "labor61_code", sub == 1, "001,002,004-008,013")
    set_labor_code(geih, "labor61_code", (sub == 1) & rev3 & (cls == 111), "003")
    set_labor_code(geih, "labor61_code", (sub == 1) & rev4 & (cls == 123), "003")
    set_labor_code(geih, "labor61_code", (sub == 1) & rev3 & (three == 12), "009-012")
    set_labor_code(geih, "labor61_code", (sub == 1) & rev4 & (three == 14), "009-012")
    set_labor_code(geih, "labor61_code", sub == 2, "014,015")
    set_labor_code(geih, "labor61_code", sub == 3, "016")

    # Minas
    set_labor_code(geih, "labor61_code", (sub == 4) & rev3 & (div == 10), "017")
    set_labor_code(geih, "labor61_code", (sub == 4) & rev3 & (div == 11), "018,021")
    set_labor_code(geih, "labor61_code", (sub == 4) & rev4 & (div == 5), "017")
    set_labor_code(
        geih,
        "labor61_code",
        (sub == 4) & rev4 & ((div == 6) | (three == 91)),
        "018,021",
    )
    set_labor_code(geih, "labor61_code", (sub == 4) & rev4 & (three == 99), "022")
    set_labor_code(geih, "labor61_code", (sub == 5) & rev3 & (div.isin([12, 13])), "019")
    set_labor_code(geih, "labor61_code", (sub == 5) & rev4 & (div == 7), "019")
    set_labor_code(geih, "labor61_code", (sub == 5) & rev3 & (div == 14), "020")
    set_labor_code(geih, "labor61_code", (sub == 5) & rev4 & (div == 8), "020")

    # Manufactura de alimentos, bebidas y tabaco
    set_labor_code(geih, "labor61_code", (sub == 6) & rev3 & cls.isin([1511, 1512]), "023-025")
    set_labor_code(geih, "labor61_code", (sub == 6) & rev4 & cls.isin([1011, 1012]), "023-025")
    set_labor_code(geih, "labor61_code", (sub == 6) & rev3 & (cls == 1522), "026")
    set_labor_code(geih, "labor61_code", (sub == 6) & rev4 & cls.isin([1031, 1032, 1033]), "026")
    set_labor_code(geih, "labor61_code", (sub == 6) & rev3 & (cls == 1530), "027")
    set_labor_code(geih, "labor61_code", (sub == 6) & rev4 & (cls == 1040), "027")
    set_labor_code(
        geih,
        "labor61_code",
        (sub == 6) & rev3 & cls.isin([1541, 1542, 1543]),
        "028,032,035",
    )
    set_labor_code(
        geih,
        "labor61_code",
        (sub == 6) & rev4 & cls.isin([1051, 1052, 1081, 1083, 1090]),
        "028,032,035",
    )
    set_labor_code(geih, "labor61_code", (sub == 6) & rev3 & (three == 156), "029")
    set_labor_code(geih, "labor61_code", (sub == 6) & rev4 & (three == 106), "029")
    set_labor_code(geih, "labor61_code", (sub == 6) & rev3 & cls.isin([1571, 1572]), "030,031")
    set_labor_code(geih, "labor61_code", (sub == 6) & rev4 & cls.isin([1071, 1072]), "030,031")
    set_labor_code(geih, "labor61_code", (sub == 6) & rev3 & (cls == 1581), "033")
    set_labor_code(geih, "labor61_code", (sub == 6) & rev4 & (cls == 1082), "033")
    set_labor_code(geih, "labor61_code", (sub == 6) & rev3 & cls.isin([1521, 1589]), "034")
    set_labor_code(geih, "labor61_code", (sub == 6) & rev4 & cls.isin([1020, 1084, 1089]), "034")
    set_labor_code(geih, "labor61_code", (sub == 6) & rev3 & (three.isin([155, 159, 160])), "036")
    set_labor_code(geih, "labor61_code", (sub == 6) & rev4 & (div.isin([11, 12])), "036")
    # The historical GEIH food-manufacturing codes do not map cleanly enough
    # to the DANE 61-way food groups, so this block remains aggregated.
    geih.loc[sub == 6, "labor61_code"] = labor61_fallback_code(6)

    # Otras aperturas manufactureras con correspondencia directa por división.
    set_labor_code(geih, "labor61_code", (sub == 9) & rev3 & (div == 20), "040")
    set_labor_code(geih, "labor61_code", (sub == 9) & rev4 & (div == 16), "040")
    set_labor_code(geih, "labor61_code", (sub == 9) & rev3 & (div == 21), "041")
    set_labor_code(geih, "labor61_code", (sub == 9) & rev4 & (div == 17), "041")
    set_labor_code(geih, "labor61_code", (sub == 11) & rev3 & (div == 23), "043,044")
    set_labor_code(geih, "labor61_code", (sub == 11) & rev4 & (div == 19), "043,044")
    set_labor_code(geih, "labor61_code", (sub == 11) & rev3 & (div == 24), "045-047")
    set_labor_code(geih, "labor61_code", (sub == 11) & rev4 & (div.isin([20, 21])), "045-047")
    set_labor_code(geih, "labor61_code", (sub == 12) & rev3 & (div == 25), "048")
    set_labor_code(geih, "labor61_code", (sub == 12) & rev4 & (div == 22), "048")
    set_labor_code(geih, "labor61_code", (sub == 12) & rev3 & (div == 26), "049")
    set_labor_code(geih, "labor61_code", (sub == 12) & rev4 & (div == 23), "049")
    set_labor_code(geih, "labor61_code", (sub == 14) & rev3 & (div.isin([30, 31, 32, 33])), "052")
    set_labor_code(geih, "labor61_code", (sub == 14) & rev4 & (div.isin([26, 27])), "052")
    set_labor_code(geih, "labor61_code", (sub == 14) & rev3 & (div == 29), "053,057")
    set_labor_code(geih, "labor61_code", (sub == 14) & rev4 & (div == 28), "053,057")
    set_labor_code(geih, "labor61_code", (sub == 16) & rev4 & (div == 33), "053,057")
    set_labor_code(geih, "labor61_code", sub == 15, "054")
    set_labor_code(geih, "labor61_code", (sub == 16) & rev3 & (three == 361), "055")
    set_labor_code(geih, "labor61_code", (sub == 16) & rev4 & (div == 31), "055")
    set_labor_code(geih, "labor61_code", (sub == 16) & rev3 & (three == 369), "056")
    set_labor_code(geih, "labor61_code", (sub == 16) & rev4 & (div == 32), "056")
    set_labor_code(geih, "labor61_code", (sub == 16) & rev3 & (div == 37), "065")

    # Servicios públicos
    set_labor_code(geih, "labor61_code", (sub == 17) & rev3 & (three == 401), "058-060")
    set_labor_code(geih, "labor61_code", (sub == 17) & rev4 & (three == 351), "058-060")
    set_labor_code(geih, "labor61_code", (sub == 17) & rev3 & (three.isin([402, 403])), "061")
    set_labor_code(geih, "labor61_code", (sub == 17) & rev4 & (three.isin([352, 353])), "061")
    set_labor_code(geih, "labor61_code", (sub == 18) & rev3 & (div == 41), "062")
    set_labor_code(geih, "labor61_code", (sub == 18) & rev4 & (div == 36), "062")
    set_labor_code(geih, "labor61_code", (sub == 18) & rev3 & (div == 90), "063,064,066")
    set_labor_code(
        geih,
        "labor61_code",
        (sub == 18) & rev4 & ((div.isin([37, 39])) | (cls.isin([3811, 3812, 3821, 3822]))),
        "063,064,066",
    )
    set_labor_code(geih, "labor61_code", (sub == 18) & rev4 & (cls == 3830), "065")

    # Construcción
    set_labor_code(geih, "labor61_code", construction & (geih["labor25_code"] == "F01"), "067")
    set_labor_code(geih, "labor61_code", construction & (geih["labor25_code"] == "F02"), "068")
    set_labor_code(geih, "labor61_code", construction & (geih["labor25_code"] == "F03"), "069")

    # Transporte acuático/aéreo y actividades de apoyo/correo.
    set_labor_code(geih, "labor61_code", (sub == 26) & rev3 & (div == 61), "073")
    set_labor_code(geih, "labor61_code", (sub == 26) & rev4 & (div == 50), "073")
    set_labor_code(geih, "labor61_code", (sub == 26) & rev3 & (div == 62), "075")
    set_labor_code(geih, "labor61_code", (sub == 26) & rev4 & (div == 51), "075")
    set_labor_code(geih, "labor61_code", (sub == 27) & rev3 & (div == 63), "076")
    set_labor_code(geih, "labor61_code", (sub == 27) & rev4 & (div == 52), "076")
    set_labor_code(geih, "labor61_code", (sub == 27) & rev3 & (three == 641), "077")
    set_labor_code(geih, "labor61_code", (sub == 27) & rev4 & (div == 53), "077")

    return geih


def aggregate_labor_by_code(geih: pd.DataFrame, code_column: str) -> pd.DataFrame:
    labor = (
        geih.dropna(subset=[code_column])
        .groupby(["anio", code_column], as_index=False)
        .agg(
            ocupados=("fex", "sum"),
            horas_sem_expandidas=("horas_sem_expand", "sum"),
        )
        .assign(horas_anuales=lambda x: x["horas_sem_expandidas"] * 52)
        .rename(columns={code_column: "labor_code"})
    )
    labor["labor_code"] = labor["labor_code"].astype(str)
    return labor


def load_geih() -> tuple[pd.DataFrame, pd.DataFrame, pd.DataFrame, pd.DataFrame, pd.DataFrame]:
    geih = pd.read_stata(
        GEIH_DTA,
        columns=[
            "anio",
            "fex",
            "horas",
            "subrama_det_cod",
            "subrama_det",
            "rama4d",
            "rama3d",
            "rama4d_div",
            "ciiu_revision_rama4d",
        ],
        convert_categoricals=False,
    )
    geih["anio"] = geih["anio"].astype(int)
    geih["fex"] = pd.to_numeric(geih["fex"], errors="coerce")
    geih["horas"] = pd.to_numeric(geih["horas"], errors="coerce")
    geih["subrama_det_cod"] = pd.to_numeric(geih["subrama_det_cod"], errors="coerce")
    geih["rama4d"] = pd.to_numeric(geih["rama4d"], errors="coerce")
    geih["rama3d"] = pd.to_numeric(geih["rama3d"], errors="coerce")
    geih["rama4d_div"] = pd.to_numeric(geih["rama4d_div"], errors="coerce")

    geih = geih[
        (geih["anio"].between(2010, 2025))
        & (geih["anio"] != 2020)
        & (geih["fex"] > 0)
    ].copy()
    geih["horas_validas"] = geih["horas"].where(geih["horas"].between(1, 112))
    geih["horas_sem_expand"] = geih["fex"] * geih["horas_validas"]
    geih = add_labor_disaggregation_codes(geih)

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

    subrama = (
        geih.dropna(subset=["subrama_det_cod"])
        .groupby(["anio", "subrama_det_cod"], as_index=False)
        .agg(
            ocupados=("fex", "sum"),
            horas_sem_expandidas=("horas_sem_expand", "sum"),
        )
        .assign(horas_anuales=lambda x: x["horas_sem_expandidas"] * 52)
    )
    subrama["subrama_det_cod"] = subrama["subrama_det_cod"].astype(int)
    labor25 = aggregate_labor_by_code(geih, "labor25_code")
    labor61 = aggregate_labor_by_code(geih, "labor61_code")
    return total, sector, subrama, labor25, labor61


def build_productivity() -> tuple[
    pd.DataFrame,
    pd.DataFrame,
    pd.DataFrame,
    pd.DataFrame,
    pd.DataFrame,
    pd.DataFrame,
]:
    pib_total, pib_sector = load_pib_quarterly()
    geih_total, geih_sector, geih_subrama, geih_labor25, geih_labor61 = load_geih()

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
    return total, total_summary, sector, sector_summary, geih_labor25, geih_labor61


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
        r"Indicador & 2010 & 2025 & Crec. anual \\",
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
        r"Indicador & 2010 & 2025 & Crec. anual \\",
        r"\midrule",
    ]
    labor_labels = {
        "PIB real": "PIB real (Billones de pesos de 2015)",
        "Ocupados": "Ocupados (Millones)",
        "PIB por trabajador": "PIB por trabajador (Millones de pesos de 2015)",
        "Horas semanales por trabajador": "Horas semanales por trabajador ",
        "PIB por hora trabajada": "PIB por hora trabajada (Miles de pesos de 2015)",
    }
    for _, row in labor_summary.iterrows():
        indicator = str(row["indicador"])
        digits = 0 if indicator == "PIB real" else 1
        labor_lines.append(
            f"{escape_latex(labor_labels[indicator])} & "
            f"{fmt_num_es(row['valor_2010'], digits)} & "
            f"{fmt_num_es(row['valor_2025'], digits)} & "
            f"{fmt_pct_es(row['crecimiento_anualizado'])} \\\\"
        )
    labor_lines.extend(
        [
            r"\bottomrule",
            r"\end{tabular}",
            r"\caption*{\footnotesize Fuente: cálculos propios con base en Cuentas Nacionales y GEIH del DANE.}",
            r"\end{table}",
        ]
    )
    (SECTION_DIR / "ocupados_horas_resumen_table.tex").write_text(
        "\n".join(labor_lines) + "\n", encoding="utf-8"
    )

    sector_sorted = sector_summary.sort_values("crec_pib_trabajador", ascending=False)
    sector_lines = [
        r"\begin{table}[H]",
        r"\centering",
        r"\caption{Productividad laboral por actividad económica CIIU, 2010--2025}",
        r"\label{tab:pib_geih_productividad_sector}",
        r"\small",
        r"\begin{tabular}{lrrrrrr}",
        r"\toprule",
        r"& \multicolumn{3}{c}{PIB por trabajador} & \multicolumn{3}{c}{PIB por hora} \\",
        r"& \multicolumn{3}{c}{\footnotesize Millones de pesos de 2015} & \multicolumn{3}{c}{\footnotesize Miles de pesos de 2015} \\",
        r"\cmidrule(lr){2-4}\cmidrule(lr){5-7}",
        r"Actividad económica & 2010 & 2025 & Crec. & 2010 & 2025 & Crec. \\",
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
            r"\caption*{\footnotesize Nota: El numerador de ambos indicadores de productividad corresponde estrictamente al valor agregado bruto de cada actividad. Las actividades económicas están clasificadas según las 12 agrupaciones CIIU Rev. 4 A.C. del DANE. Los ocupados y las horas se agregan desde GEIH usando la actividad económica reportada en la encuesta. Fuente: cálculos propios con DANE y GEIH.}",
            r"\end{table}",
        ]
    )
    (SECTION_DIR / "pib_geih_productividad_sector_table.tex").write_text(
        "\n".join(sector_lines), encoding="utf-8"
    )



def fmt_corr_es(value: float) -> str:
    return f"{value:.3f}".replace(".", ",")


def write_sector_correlation_table(summary: pd.DataFrame) -> None:
    growth = summary[
        [
            "codigo",
            "actividad_corta",
            "crec_ocupados",
            "crec_horas",
            "crec_pib_hora",
            "crec_pib_trabajador",
        ]
    ].copy()
    growth = growth.rename(
        columns={
            "codigo": "sector_code",
            "actividad_corta": "sector_name_short",
            "crec_pib_hora": "crec_productividad_hora",
            "crec_pib_trabajador": "crec_productividad_trabajador",
        }
    )

    corr_vars = [
        "crec_ocupados",
        "crec_horas",
        "crec_productividad_hora",
        "crec_productividad_trabajador",
    ]
    labels = {
        "crec_ocupados": "Ocupados",
        "crec_horas": "Horas totales",
        "crec_productividad_hora": "PIB por hora",
        "crec_productividad_trabajador": "PIB por trabajador",
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
        r"\caption{Correlaciones entre crecimientos por actividad económica, 2010--2025}",
        r"\label{tab:pib_geih_productividad_sector_correlaciones}",
        r"\small",
        r"\begin{tabular}{lrrrr}",
        r"\toprule",
        r"Variable & Ocupados & Horas totales & PIB por hora & PIB por trabajador \\",
        r"\midrule",
    ]
    for row_label, row in corr.iterrows():
        lines.append(
            f"{escape_latex(row_label)} & "
            f"{fmt_corr_es(row['Ocupados'])} & "
            f"{fmt_corr_es(row['Horas totales'])} & "
            f"{fmt_corr_es(row['PIB por hora'])} & "
            f"{fmt_corr_es(row['PIB por trabajador'])} \\\\"
        )
    lines.extend(
        [
            r"\bottomrule",
            r"\end{tabular}",
            f"\\caption*{{\\footnotesize Nota: correlaciones de Pearson calculadas entre las tasas de crecimiento anualizado 2010--2025 de {len(growth)} observaciones detalladas comparables construidas a partir de la apertura de 61 agrupaciones CIIU. Las horas corresponden al total anual de horas trabajadas por actividad económica, estimado a partir de GEIH como horas semanales ponderadas por el factor de expansión y multiplicadas por 52. Fuente: cálculos propios con DANE y GEIH.}}",
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
            "indicador": "Horas semanales por trabajador",
            "unidad": "Horas por semana",
            "valor_2010": start_hours,
            "valor_2025": end_hours,
            "crecimiento_anualizado": cagr(start_hours, end_hours, start_year, end_year),
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
    display_labels = {
        "PIB real": "PIB real  (Billones de pesos de 2015)",
        "Ocupados": "Ocupados (Millones)",
        "PIB por trabajador": "PIB por trabajador (Millones de pesos de 2015)",
        "Horas semanales por trabajador": "Horas semanales por trabajador",
        "PIB por hora trabajada": "PIB por hora trabajada (Miles de pesos de 2015)",
    }
    lines = [
        r"\begin{table}[H]",
        r"\centering",
        f"\\caption{{{escape_latex(caption)}}}",
        f"\\label{{{label}}}",
        r"\small",
        r"\begin{tabular}{lrrr}",
        r"\toprule",
        r"Indicador & 2010 & 2025 & Crec. anual \\",
        r"\midrule",
    ]
    for _, row in metrics.iterrows():
        indicator = display_labels.get(str(row["indicador"]), indicator_with_unit(row))
        lines.append(
            f"{escape_latex(indicator)} & "
            f"{fmt_num_es(row['valor_2010'], 1)} & "
            f"{fmt_num_es(row['valor_2025'], 1)} & "
            f"{fmt_pct_es(row['crecimiento_anualizado'])} \\\\"
        )
    lines.extend([r"\bottomrule", r"\end{tabular}", r"\end{table}"])
    return lines


def metric_growth_lookup(metrics: pd.DataFrame) -> dict[str, float]:
    return {
        str(row["indicador"]): float(row["crecimiento_anualizado"])
        for _, row in metrics.iterrows()
    }


def metric_level_lookup(metrics: pd.DataFrame) -> dict[str, float]:
    return {
        str(row["indicador"]): float(row["valor_2025"])
        for _, row in metrics.iterrows()
    }


def relative_to_aggregate(value: float, aggregate_value: float) -> str:
    if abs(value - aggregate_value) <= 0.002:
        return f"muy cerca del agregado ({fmt_pct_es(aggregate_value)})"
    if value > aggregate_value:
        return f"por encima del agregado ({fmt_pct_es(aggregate_value)})"
    return f"por debajo del agregado ({fmt_pct_es(aggregate_value)})"


def comparison_fragment(
    subject: str, verb: str, value: float, aggregate_value: float
) -> str:
    return (
        f"{subject} {verb} una tasa anualizada de {fmt_pct_es(value)}, "
        f"{relative_to_aggregate(value, aggregate_value)}"
    )


def growth_rate_phrase(value: float, period: str = "anual") -> str:
    if value < 0:
        if period.endswith("o"):
            period = period[:-1] + "a"
        return f"una caída {period} del {fmt_pct_es(abs(value))}"
    return f"un crecimiento {period} del {fmt_pct_es(value)}"


def relative_to_named_aggregate(
    value: float, aggregate_value: float, aggregate_text: str
) -> str:
    if abs(value - aggregate_value) <= 0.002:
        return f"muy cerca {aggregate_text} ({fmt_pct_es(aggregate_value)})"
    if value > aggregate_value:
        prefix = "muy por encima" if abs(value - aggregate_value) >= 0.01 else "por encima"
        return f"{prefix} {aggregate_text} ({fmt_pct_es(aggregate_value)})"
    prefix = "muy por debajo" if abs(value - aggregate_value) >= 0.01 else "por debajo"
    return f"{prefix} {aggregate_text} ({fmt_pct_es(aggregate_value)})"


def growth_comparison_sentence(
    subject: str,
    value: float,
    aggregate_value: float,
    aggregate_text: str,
    period: str = "anual",
) -> str:
    verb = "registraron" if subject.startswith(("Los ", "Las ")) else "registró"
    return (
        f"{subject} {verb} {growth_rate_phrase(value, period)}, "
        f"{relative_to_named_aggregate(value, aggregate_value, aggregate_text)}"
    )


def hours_relation_to_aggregate(value: float, aggregate_value: float) -> str:
    if value < 0 and aggregate_value < 0:
        if abs(value - aggregate_value) <= 0.002:
            relation = f"muy cerca de la caída agregada ({fmt_pct_es(aggregate_value)})"
        elif value < aggregate_value:
            relation = (
                f"una caída más pronunciada que la del agregado "
                f"({fmt_pct_es(aggregate_value)})"
            )
        else:
            relation = (
                f"una caída menos pronunciada que la del agregado "
                f"({fmt_pct_es(aggregate_value)})"
            )
    else:
        relation = relative_to_aggregate(value, aggregate_value)
    return relation


def hours_comparison_fragment(value: float, aggregate_value: float) -> str:
    relation = hours_relation_to_aggregate(value, aggregate_value)
    return (
        f"Las horas semanales por trabajador registraron una tasa anualizada de "
        f"{fmt_pct_es(value)}, {relation}"
    )


def hours_growth_comparison_sentence(value: float, aggregate_value: float) -> str:
    relation = hours_relation_to_aggregate(value, aggregate_value)
    return (
        f"Las horas semanales por trabajador registraron "
        f"{growth_rate_phrase(value)}, {relation}"
    )


def relative_level_to_aggregate(
    value: float, aggregate_value: float, aggregate_text: str
) -> str:
    tolerance = max(abs(aggregate_value) * 0.05, 0.05)
    if abs(value - aggregate_value) <= tolerance:
        relation = "muy cerca del agregado"
    elif value > aggregate_value:
        relation = "por encima del agregado"
    else:
        relation = "por debajo del agregado"
    return f"{relation} ({aggregate_text})"


def sector_level_paragraph(
    metrics: pd.DataFrame, aggregate_levels: dict[str, float]
) -> str:
    levels = metric_level_lookup(metrics)
    pib_share = levels["PIB real"] / aggregate_levels["PIB real"]
    occupied_share = levels["Ocupados"] / aggregate_levels["Ocupados"]
    worker_relation = relative_level_to_aggregate(
        levels["PIB por trabajador"],
        aggregate_levels["PIB por trabajador"],
        f"{fmt_num_es(aggregate_levels['PIB por trabajador'], 1)} millones",
    )
    hours_relation = relative_level_to_aggregate(
        levels["Horas semanales por trabajador"],
        aggregate_levels["Horas semanales por trabajador"],
        fmt_num_es(aggregate_levels["Horas semanales por trabajador"], 1),
    )
    hourly_relation = relative_level_to_aggregate(
        levels["PIB por hora trabajada"],
        aggregate_levels["PIB por hora trabajada"],
        f"{fmt_num_es(aggregate_levels['PIB por hora trabajada'], 1)} mil",
    )
    return (
        r"\textbf{En niveles de 2025, el tamaño relativo y la productividad de la actividad económica también importan.} "
        f"La actividad representó {fmt_pct_es(pib_share, 1)} del PIB real agregado, "
        f"con {fmt_num_es(levels['PIB real'], 1)} billones de pesos de 2015, "
        f"y concentró {fmt_pct_es(occupied_share, 1)} de los ocupados, "
        f"con {fmt_num_es(levels['Ocupados'], 1)} millones de personas. "
        f"El PIB por trabajador fue {fmt_num_es(levels['PIB por trabajador'], 1)} "
        f"millones de pesos de 2015 por ocupado, {worker_relation}. "
        f"Las horas semanales por trabajador fueron "
        f"{fmt_num_es(levels['Horas semanales por trabajador'], 1)}, "
        f"{hours_relation}. "
        f"El PIB por hora trabajada fue "
        f"{fmt_num_es(levels['PIB por hora trabajada'], 1)} "
        f"mil pesos de 2015 por hora, {hourly_relation}."
    )


def sector_intro_level_paragraph(
    sector_code: str, metrics: pd.DataFrame, aggregate_levels: dict[str, float]
) -> str:
    levels = metric_level_lookup(metrics)
    pib_share = levels["PIB real"] / aggregate_levels["PIB real"]
    occupied_share = levels["Ocupados"] / aggregate_levels["Ocupados"]
    worker_relation = relative_level_to_aggregate(
        levels["PIB por trabajador"],
        aggregate_levels["PIB por trabajador"],
        f"{fmt_num_es(aggregate_levels['PIB por trabajador'], 1)} millones",
    )
    hours_relation = relative_level_to_aggregate(
        levels["Horas semanales por trabajador"],
        aggregate_levels["Horas semanales por trabajador"],
        fmt_num_es(aggregate_levels["Horas semanales por trabajador"], 1),
    )
    hourly_relation = relative_level_to_aggregate(
        levels["PIB por hora trabajada"],
        aggregate_levels["PIB por hora trabajada"],
        f"{fmt_num_es(aggregate_levels['PIB por hora trabajada'], 1)} mil",
    )
    lead = SECTOR_INTRO_LEAD[sector_code]
    subject = SECTOR_BODY_SUBJECT[sector_code]
    return (
        f"\\textbf{{{escape_latex(lead)}}} "
        f"En 2025 {subject} representó {fmt_pct_es(pib_share, 1)} del PIB, "
        f"con {fmt_num_es(levels['PIB real'], 1)} billones de pesos de 2015, "
        f"y concentró {fmt_pct_es(occupied_share, 1)} de los ocupados, "
        f"con {fmt_num_es(levels['Ocupados'], 1)} millones de personas. "
        f"El PIB por trabajador fue {fmt_num_es(levels['PIB por trabajador'], 1)} "
        f"millones de pesos de 2015 por ocupado, {worker_relation}. "
        f"Las horas semanales por trabajador fueron "
        f"{fmt_num_es(levels['Horas semanales por trabajador'], 1)}, "
        f"{hours_relation}. "
        f"El PIB por hora trabajada fue "
        f"{fmt_num_es(levels['PIB por hora trabajada'], 1)} "
        f"mil pesos de 2015 por hora, {hourly_relation}."
    )


def sector_comparison_paragraph(
    metrics: pd.DataFrame, aggregate_growth: dict[str, float]
) -> str:
    growth = metric_growth_lookup(metrics)
    return (
        r"\textbf{Frente al agregado nacional, la comparación variable por variable muestra diferencias relevantes.} "
        + comparison_fragment(
            "El PIB real de la actividad",
            "registró",
            growth["PIB real"],
            aggregate_growth["PIB real"],
        )
        + ". "
        + comparison_fragment(
            "La ocupación en la actividad",
            "registró",
            growth["Ocupados"],
            aggregate_growth["Ocupados"],
        )
        + ". "
        + comparison_fragment(
            "El PIB por trabajador",
            "registró",
            growth["PIB por trabajador"],
            aggregate_growth["PIB por trabajador"],
        )
        + ". "
        + hours_comparison_fragment(
            growth["Horas semanales por trabajador"],
            aggregate_growth["Horas semanales por trabajador"],
        )
        + ". "
        + comparison_fragment(
            "El PIB por hora trabajada",
            "registró",
            growth["PIB por hora trabajada"],
            aggregate_growth["PIB por hora trabajada"],
        )
        + "."
    )


def sector_detail_comparison_paragraph(
    sector_code: str, metrics: pd.DataFrame, aggregate_growth: dict[str, float]
) -> str:
    growth = metric_growth_lookup(metrics)
    worker_growth = growth["PIB por trabajador"]
    hour_growth = growth["PIB por hora trabajada"]
    worker_agg = aggregate_growth["PIB por trabajador"]
    hour_agg = aggregate_growth["PIB por hora trabajada"]
    productivity_subject = SECTOR_PRODUCTIVITY_SUBJECT[sector_code]
    if worker_growth > worker_agg + 0.002 and hour_growth > hour_agg + 0.002:
        headline = (
            f"El crecimiento de la productividad {productivity_subject} fue superior "
            "al crecimiento de la productividad agregada."
        )
    elif worker_growth < 0 and hour_growth < 0:
        headline = f"La productividad {productivity_subject} cayó entre 2010 y 2025."
    elif worker_growth < worker_agg - 0.002 and hour_growth < hour_agg - 0.002:
        headline = (
            f"El crecimiento de la productividad {productivity_subject} fue inferior "
            "al crecimiento de la productividad agregada."
        )
    else:
        headline = (
            f"La evolución de la productividad {productivity_subject} fue heterogénea "
            "frente a la productividad agregada."
        )
    subject = SECTOR_BODY_SUBJECT[sector_code]
    return (
        f"\\textbf{{{escape_latex(headline)}}} "
        + growth_comparison_sentence(
            f"Entre 2010 y 2025 el PIB de {subject}",
            growth["PIB real"],
            aggregate_growth["PIB real"],
            "del agregado",
            "anualizado",
        )
        + ". "
        + growth_comparison_sentence(
            "Los ocupados en la actividad",
            growth["Ocupados"],
            aggregate_growth["Ocupados"],
            "del agregado",
        )
        + ". "
        + growth_comparison_sentence(
            "El PIB por trabajador",
            growth["PIB por trabajador"],
            aggregate_growth["PIB por trabajador"],
            "del crecimiento del PIB por trabajador de toda la economía",
        )
        + ". "
        + hours_growth_comparison_sentence(
            growth["Horas semanales por trabajador"],
            aggregate_growth["Horas semanales por trabajador"],
        )
        + ". "
        + growth_comparison_sentence(
            "El PIB por hora trabajada",
            growth["PIB por hora trabajada"],
            aggregate_growth["PIB por hora trabajada"],
            "del crecimiento del PIB por hora trabajada de toda la economía",
        )
        + "."
    )


def join_latex_items(items: list[str]) -> str:
    if not items:
        return ""
    if len(items) == 1:
        return items[0]
    if len(items) == 2:
        return f"{items[0]} y {items[1]}"
    if any("," in item for item in items):
        return f"{'; '.join(items[:-1])}; y {items[-1]}"
    return f"{', '.join(items[:-1])} y {items[-1]}"


def tied_activity_names(data: pd.DataFrame, column: str, value: float) -> str:
    tied = data[data[column].round(10) == round(value, 10)].copy()
    names = [escape_latex(name) for name in tied["actividad_corta"].tolist()]
    if len(names) > 3:
        shown = names[:3]
        shown.append(f"{len(names) - 3} más")
        names = shown
    return join_latex_items(names)


def activity_count_subject(count: int, total: int) -> str:
    words = {
        1: "una",
        2: "dos",
        3: "tres",
        4: "cuatro",
        5: "cinco",
        6: "seis",
        7: "siete",
        8: "ocho",
        9: "nueve",
        10: "diez",
    }
    if count == 0:
        return "ninguna subactividad"
    if count == total:
        return "todas las subactividades"
    count_text = words.get(count, str(count))
    total_text = words.get(total, str(total))
    if count == 1:
        return f"una de las {total_text} subactividades"
    return f"{count_text} de las {total_text} subactividades"


def activity_count_verb(count: int, singular: str, plural: str) -> str:
    return singular if count in {0, 1} else plural


def sector_zoom_balance_sentence(
    sector_code: str, subset: pd.DataFrame, aggregate_growth: dict[str, float]
) -> str:
    total = len(subset)
    worker_agg = aggregate_growth["PIB por trabajador"]
    hour_agg = aggregate_growth["PIB por hora trabajada"]
    above_worker = int((subset["crec_pib_trabajador"] > worker_agg).sum())
    above_hour = int((subset["crec_pib_hora"] > hour_agg).sum())
    negative_worker = int((subset["crec_pib_trabajador"] < 0).sum())
    negative_hour = int((subset["crec_pib_hora"] < 0).sum())
    both_negative = int(
        ((subset["crec_pib_trabajador"] < 0) & (subset["crec_pib_hora"] < 0)).sum()
    )
    context = SECTOR_ZOOM_CONTEXT[sector_code]

    if above_worker == total and above_hour == total:
        return (
            f"El balance de la apertura {context} es favorable: todas las subactividades "
            "superan el crecimiento agregado tanto en PIB por trabajador como en PIB por hora trabajada."
        )
    if above_worker == 0 and above_hour == 0:
        if negative_worker == total and negative_hour == total:
            return (
                f"El balance de la apertura {context} es claramente negativo: todas las subactividades "
                "registran caídas de productividad y ninguna alcanza el crecimiento agregado."
            )
        if negative_worker > 0 or negative_hour > 0:
            if negative_worker == total:
                return (
                    f"El balance de la apertura {context} es negativo: ninguna subactividad supera el crecimiento agregado, "
                    f"todas registran caídas en PIB por trabajador y {activity_count_subject(negative_hour, total)} "
                    f"{activity_count_verb(negative_hour, 'también cae', 'también caen')} por hora trabajada."
                )
            if both_negative > 0:
                return (
                    f"El balance de la apertura {context} es negativo: ninguna subactividad supera el crecimiento agregado "
                    f"y {activity_count_subject(both_negative, total)} {activity_count_verb(both_negative, 'cae', 'caen')} tanto en PIB por trabajador como en PIB por hora trabajada."
                )
            negative_any = max(negative_worker, negative_hour)
            return (
                f"El balance de la apertura {context} es negativo: ninguna subactividad supera el crecimiento agregado "
                f"y {activity_count_subject(negative_any, total)} {activity_count_verb(negative_any, 'registra', 'registran')} caídas en al menos una medida de productividad."
            )
        return (
            f"El balance de la apertura {context} es débil: aun las subactividades de mejor desempeño "
            "crecen por debajo de la productividad agregada."
        )
    if above_worker == total and above_hour < total:
        return (
            f"El balance de la apertura {context} es favorable en PIB por trabajador, pero menos uniforme por hora trabajada: "
            f"{activity_count_subject(above_worker, total)} {activity_count_verb(above_worker, 'supera', 'superan')} el crecimiento agregado por trabajador y "
            f"{activity_count_subject(above_hour, total)} {activity_count_verb(above_hour, 'lo hace', 'lo hacen')} por hora."
        )
    if above_worker < total and above_hour == total:
        return (
            f"El balance de la apertura {context} es favorable por hora trabajada, pero más heterogéneo por trabajador: "
            f"{activity_count_subject(above_hour, total)} {activity_count_verb(above_hour, 'supera', 'superan')} el crecimiento agregado por hora y "
            f"{activity_count_subject(above_worker, total)} {activity_count_verb(above_worker, 'lo hace', 'lo hacen')} por trabajador."
        )
    return (
        f"El balance de la apertura {context} es heterogéneo: "
        f"{activity_count_subject(above_worker, total)} {activity_count_verb(above_worker, 'supera', 'superan')} el crecimiento agregado del PIB por trabajador y "
        f"{activity_count_subject(above_hour, total)} {activity_count_verb(above_hour, 'supera', 'superan')} el crecimiento agregado del PIB por hora trabajada, "
        "mientras otras quedan rezagadas."
    )


def sector_zoom_lines(
    summary25: pd.DataFrame | None,
    summary61: pd.DataFrame | None,
    sector_code: str,
    aggregate_growth: dict[str, float],
) -> list[str]:
    subset25 = pd.DataFrame()
    subset61 = pd.DataFrame()
    if summary25 is not None and not summary25.empty and "sector_code" in summary25.columns:
        subset25 = summary25[summary25["sector_code"] == sector_code].copy()
    if summary61 is not None and not summary61.empty and "sector_code" in summary61.columns:
        subset61 = summary61[summary61["sector_code"] == sector_code].copy()

    use61 = len(subset25) == 1 and len(subset61) > 1
    level = 61 if use61 else 25
    subset = subset61 if use61 else subset25
    if subset.empty:
        return []

    subset = subset.sort_values("crec_pib_trabajador", ascending=False)

    if len(subset) == 1:
        return []

    top_worker = subset.iloc[0]
    bottom_worker = subset.sort_values("crec_pib_trabajador", ascending=True).iloc[0]
    top_hour = subset.sort_values("crec_pib_hora", ascending=False).iloc[0]
    bottom_hour = subset.sort_values("crec_pib_hora", ascending=True).iloc[0]
    label_code = latex_id(sector_code)

    if use61:
        note = (
            r"\caption*{\footnotesize Nota: PIB por trabajador en millones de pesos constantes de 2015; PIB por hora en miles de pesos constantes de 2015. "
            r"Cuando la GEIH no separa ocupados y horas al mismo nivel de las cuentas nacionales, se agrupan las subactividades del DANE hasta el nivel laboral comparable. "
            r"Fuente: cálculos propios con DANE y GEIH.}"
        )
    else:
        note = (
            r"\caption*{\footnotesize Nota: PIB por trabajador en millones de pesos constantes de 2015; PIB por hora en miles de pesos constantes de 2015. Fuente: cálculos propios con DANE y GEIH.}"
        )

    lines = [
        r"\begin{table}[H]",
        r"\centering",
        f"\\caption{{Productividad laboral de las actividades {escape_latex(SECTOR_ZOOM_CAPTION[sector_code])}, 2010--2025}}",
        f"\\label{{tab:sector_{label_code}_zoom{level}}}",
        r"\small",
        r"\begin{tabular}{p{0.40\textwidth}rrrr}",
        r"\toprule",
        r"Actividad económica & PIB/trab. 2025 & Crec. & PIB/hora 2025 & Crec. \\",
        r"\midrule",
    ]
    for _, row in subset.iterrows():
        lines.append(
            f"{escape_latex(row['actividad_corta'])} & "
            f"{fmt_num_es(row['pib_trabajador_2025'], 1)} & "
            f"{fmt_pct_es(row['crec_pib_trabajador'])} & "
            f"{fmt_num_es(row['pib_hora_2025'], 1)} & "
            f"{fmt_pct_es(row['crec_pib_hora'])} \\\\"
        )
    lines.extend(
        [
            r"\bottomrule",
            r"\end{tabular}",
            note,
            r"\end{table}",
            "",
        ]
    )
    same_worker_growth = subset["crec_pib_trabajador"].round(10).nunique(dropna=True) == 1
    same_hour_growth = subset["crec_pib_hora"].round(10).nunique(dropna=True) == 1
    worker_aggregate_text = "del crecimiento agregado del PIB por trabajador"
    hour_aggregate_text = "del crecimiento agregado del PIB por hora trabajada"
    if same_worker_growth and same_hour_growth:
        worker_growth = float(subset["crec_pib_trabajador"].iloc[0])
        hour_growth = float(subset["crec_pib_hora"].iloc[0])
        lines.extend(
            [
                "En esta apertura, las tasas estimadas de productividad son iguales entre las subactividades porque la información laboral comparable no permite separar ocupados y horas con ese nivel de detalle. Por eso, la tabla debe leerse como una apertura descriptiva del PIB dentro de la actividad amplia, no como evidencia de diferencias efectivas de productividad entre esas subactividades. "
                f"Frente al agregado nacional, la tasa de PIB por trabajador se ubica {relative_to_named_aggregate(worker_growth, aggregate_growth['PIB por trabajador'], worker_aggregate_text)} y la tasa de PIB por hora trabajada se ubica {relative_to_named_aggregate(hour_growth, aggregate_growth['PIB por hora trabajada'], hour_aggregate_text)}.",
                "",
            ]
        )
    else:
        top_worker_names = tied_activity_names(
            subset, "crec_pib_trabajador", top_worker["crec_pib_trabajador"]
        )
        bottom_worker_names = tied_activity_names(
            subset, "crec_pib_trabajador", bottom_worker["crec_pib_trabajador"]
        )
        top_hour_names = tied_activity_names(subset, "crec_pib_hora", top_hour["crec_pib_hora"])
        bottom_hour_names = tied_activity_names(
            subset, "crec_pib_hora", bottom_hour["crec_pib_hora"]
        )
        top_worker_relation = relative_to_named_aggregate(
            top_worker["crec_pib_trabajador"],
            aggregate_growth["PIB por trabajador"],
            worker_aggregate_text,
        )
        bottom_worker_relation = relative_to_named_aggregate(
            bottom_worker["crec_pib_trabajador"],
            aggregate_growth["PIB por trabajador"],
            worker_aggregate_text,
        )
        top_hour_relation = relative_to_named_aggregate(
            top_hour["crec_pib_hora"],
            aggregate_growth["PIB por hora trabajada"],
            hour_aggregate_text,
        )
        bottom_hour_relation = relative_to_named_aggregate(
            bottom_hour["crec_pib_hora"],
            aggregate_growth["PIB por hora trabajada"],
            hour_aggregate_text,
        )
        balance = sector_zoom_balance_sentence(sector_code, subset, aggregate_growth)
        lines.extend(
            [
                f"\\textbf{{{escape_latex(balance)}}} "
                f"En esta apertura, el mayor crecimiento del PIB por trabajador se observa en {top_worker_names} "
                f"({fmt_pct_es(top_worker['crec_pib_trabajador'])}), {top_worker_relation}; el menor se registra en "
                f"{bottom_worker_names} ({fmt_pct_es(bottom_worker['crec_pib_trabajador'])}), {bottom_worker_relation}. "
                f"Por hora trabajada, el mejor desempeño corresponde a {top_hour_names} "
                f"({fmt_pct_es(top_hour['crec_pib_hora'])}), {top_hour_relation}; el más rezagado corresponde a "
                f"{bottom_hour_names} ({fmt_pct_es(bottom_hour['crec_pib_hora'])}), {bottom_hour_relation}.",
                "",
            ]
        )
    return lines


def write_sector_detail_sections(
    sector: pd.DataFrame,
    total: pd.DataFrame,
    summary25: pd.DataFrame | None = None,
    summary61: pd.DataFrame | None = None,
) -> None:
    detail_rows = []
    lines = [
        r"\textbf{A continuación se presenta la descomposición del crecimiento de cada una de las doce agrupaciones de actividad económica CIIU.} En particular, se presenta el PIB real de la actividad, el número de ocupados, el PIB por trabajador, las horas semanales promedio por trabajador y el PIB por hora trabajada para los años 2010 y 2025. La lectura conjunta de estas variables permite distinguir si los cambios de productividad responden principalmente al dinamismo del producto, a variaciones en el empleo, a cambios en las horas trabajadas o a una combinación de estos factores. Las descripciones siguen la agregación usada por el DANE; por eso, en algunos casos reúnen actividades económicas muy distintas dentro de una misma agrupación.",
        "",
    ]
    total_start = total[total["anio"] == 2010].iloc[0]
    total_end = total[total["anio"] == 2025].iloc[0]
    aggregate_metrics = build_metric_rows(total_start, total_end)
    aggregate_growth = metric_growth_lookup(aggregate_metrics)
    aggregate_levels = metric_level_lookup(aggregate_metrics)

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

        lines.extend(
            [
                f"\\subsection{{{escape_latex(SECTOR_SHORT[code])}}}",
                "",
                sector_intro_level_paragraph(code, metrics, aggregate_levels),
                "",
                *metric_table_lines(
                    metrics,
                    f"tab:sector_{latex_id(code)}_productividad",
                    f"{SECTOR_SHORT[code]}: PIB, ocupados, horas y productividad laboral, 2010--2025",
                ),
                "",
                sector_detail_comparison_paragraph(code, metrics, aggregate_growth),
                "",
                *sector_zoom_lines(summary25, summary61, code, aggregate_growth),
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
        r"%{\footnotesize Nota general: para facilitar la lectura, el informe usa PIB por actividad económica; en sentido estricto, el numerador corresponde al valor agregado bruto de cada actividad reportado por el DANE. Valores en pesos constantes de 2015. Ocupados expandidos con el factor \texttt{fex}. Las horas semanales promedio se calculan como horas anuales totales divididas por ocupados y por 52 semanas. Se excluye 2020 por no contar con GEIH anual comparable en la base del proyecto. Fuente: cálculos propios con DANE y GEIH.}"
    )
    (SECTION_DIR / "pib_geih_productividad_sector_detalle.tex").write_text(
        "\n".join(lines), encoding="utf-8"
    )


def clean_group_code(value) -> str | None:
    if pd.isna(value):
        return None
    text = str(value).strip()
    if not text:
        return None
    return text.replace(" ", "")


def shorten_label(text: str, max_chars: int = 70) -> str:
    text = " ".join(str(text).split())
    if len(text) <= max_chars:
        return text
    return text[: max_chars - 3].rstrip() + "..."


def load_va_disaggregation(sheet_name: str, code_col: int, concept_col: int) -> pd.DataFrame:
    raw = pd.read_excel(PIB_XLSX, sheet_name=sheet_name, header=None)
    columns = quarter_columns(raw, concept_col + 1)
    concept_text = raw.iloc[:, concept_col].astype(str).str.strip()
    stop_rows = concept_text[
        (raw.index >= 14) & concept_text.eq("Producto Interno Bruto")
    ].index
    stop = int(stop_rows[0]) if len(stop_rows) else raw.shape[0]
    rows = raw.iloc[14:stop, [code_col, concept_col] + [col for col, _, _ in columns]].copy()
    rows.columns = ["group_code", "group_name"] + [
        f"{year}_{quarter}" for _, year, quarter in columns
    ]
    rows["group_code"] = rows["group_code"].apply(clean_group_code)
    rows = rows.dropna(subset=["group_code", "group_name"])

    long = rows.melt(
        id_vars=["group_code", "group_name"],
        var_name="period",
        value_name="pib_miles_millones_2015",
    )
    long[["anio", "trimestre"]] = long["period"].str.split("_", expand=True)
    long["anio"] = long["anio"].astype(int)
    long = long.dropna(subset=["pib_miles_millones_2015"])
    return annualize_quarterly(long, ["group_code", "group_name"])


def summarize_va_disaggregation(
    annual: pd.DataFrame, short_labels: dict[str, str] | None = None
) -> pd.DataFrame:
    start_year, end_year = 2010, 2025
    rows = []
    for code, part in annual.groupby("group_code", sort=False):
        if start_year not in set(part["anio"]) or end_year not in set(part["anio"]):
            continue
        start = part[part["anio"] == start_year].iloc[0]
        end = part[part["anio"] == end_year].iloc[0]
        name = str(end["group_name"])
        rows.append(
            {
                "codigo": code,
                "actividad": name,
                "actividad_corta": short_labels.get(code, name) if short_labels else shorten_label(name),
                "va_2010_billones": start["pib_miles_millones_2015"] / 1000,
                "va_2025_billones": end["pib_miles_millones_2015"] / 1000,
                "crecimiento_anualizado": cagr(
                    start["pib_miles_millones_2015"],
                    end["pib_miles_millones_2015"],
                    start_year,
                    end_year,
                ),
            }
        )
    return pd.DataFrame(rows)


def labor_code_sector(code: object) -> str | None:
    if code in AGG25_TO_SECTOR:
        return AGG25_TO_SECTOR[code]
    if code in AGG61_TO_SECTOR:
        return AGG61_TO_SECTOR[code]
    if isinstance(code, str) and code.startswith("S"):
        try:
            return SUBRAMA_TO_SECTOR[int(code[1:])]
        except (KeyError, ValueError):
            return None
    try:
        return SUBRAMA_TO_SECTOR[int(code)]
    except (KeyError, TypeError, ValueError):
        return None


def comparable_components(
    pools: list[dict[str, list]],
    order: list[str],
    annual_codes: set[str],
) -> list[dict[str, object]]:
    order_index = {code: i for i, code in enumerate(order)}
    valid_pools = []
    for pool in pools:
        groups = [code for code in pool["groups"] if code in annual_codes]
        if groups:
            valid_pools.append({"groups": groups, "subramas": list(pool["subramas"])})

    parent = list(range(len(valid_pools)))

    def find(idx: int) -> int:
        while parent[idx] != idx:
            parent[idx] = parent[parent[idx]]
            idx = parent[idx]
        return idx

    def union(left: int, right: int) -> None:
        left_root = find(left)
        right_root = find(right)
        if left_root != right_root:
            parent[right_root] = left_root

    seen_group: dict[str, int] = {}
    seen_subrama: dict[object, int] = {}
    for idx, pool in enumerate(valid_pools):
        for group in pool["groups"]:
            if group in seen_group:
                union(idx, seen_group[group])
            else:
                seen_group[group] = idx
        for subrama in pool["subramas"]:
            if subrama in seen_subrama:
                union(idx, seen_subrama[subrama])
            else:
                seen_subrama[subrama] = idx

    grouped: dict[int, dict[str, set]] = {}
    for idx, pool in enumerate(valid_pools):
        root = find(idx)
        if root not in grouped:
            grouped[root] = {"groups": set(), "subramas": set()}
        grouped[root]["groups"].update(pool["groups"])
        grouped[root]["subramas"].update(pool["subramas"])

    components = []
    for component in grouped.values():
        groups = sorted(component["groups"], key=lambda code: order_index.get(code, 10_000))
        subramas = sorted(component["subramas"], key=lambda code: str(code))
        sectors = {
            labor_code_sector(subrama)
            for subrama in subramas
            if labor_code_sector(subrama) is not None
        }
        components.append(
            {
                "groups": groups,
                "subramas": subramas,
                "group_code": "|".join(groups),
                "group_order": min(order_index.get(code, 10_000) for code in groups),
                "sector_code": next(iter(sectors)) if len(sectors) == 1 else None,
            }
        )

    return sorted(components, key=lambda component: component["group_order"])


def comparable_label(groups: list[str], short_labels: dict[str, str] | None) -> str:
    group_key = "|".join(groups)
    if group_key in COMPARABLE_LABEL_ALIASES:
        return COMPARABLE_LABEL_ALIASES[group_key]
    if short_labels:
        labels = [short_labels.get(group, group) for group in groups]
    else:
        labels = groups
    return "; ".join(labels)


def build_labor_at_comparable_level(
    geih_subrama: pd.DataFrame,
    annual: pd.DataFrame,
    pools: list[dict[str, list]],
    order: list[str],
    short_labels: dict[str, str] | None = None,
) -> pd.DataFrame:
    annual_codes = set(annual["group_code"].astype(str))
    components = comparable_components(pools, order, annual_codes)
    rows = []

    for component in components:
        groups = component["groups"]
        subramas = component["subramas"]
        labor_code_col = "labor_code" if "labor_code" in geih_subrama.columns else "subrama_det_cod"
        va = (
            annual[annual["group_code"].isin(groups)]
            .groupby("anio", as_index=False)
            .agg(pib_miles_millones_2015=("pib_miles_millones_2015", "sum"))
        )
        labor = (
            geih_subrama[geih_subrama[labor_code_col].isin(subramas)]
            .groupby("anio", as_index=False)
            .agg(
                ocupados=("ocupados", "sum"),
                horas_sem_expandidas=("horas_sem_expandidas", "sum"),
                horas_anuales=("horas_anuales", "sum"),
            )
        )
        if va.empty or labor.empty:
            continue
        merged = va.merge(labor, on="anio", how="inner")
        merged["group_code"] = component["group_code"]
        merged["group_name"] = comparable_label(groups, short_labels)
        merged["group_label_short"] = comparable_label(groups, short_labels)
        merged["group_order"] = component["group_order"]
        merged["sector_code"] = component["sector_code"]
        merged["dane_groups"] = "; ".join(groups)
        rows.append(merged)

    if not rows:
        return pd.DataFrame()
    return pd.concat(rows, ignore_index=True)


def build_productivity_disaggregation(
    annual: pd.DataFrame,
    geih_subrama: pd.DataFrame,
    pools: list[dict[str, list]],
    order: list[str],
    short_labels: dict[str, str] | None = None,
) -> pd.DataFrame:
    data = build_labor_at_comparable_level(geih_subrama, annual, pools, order, short_labels)
    if data.empty:
        return data
    data = data[data["anio"] != 2020].copy()
    data["pib_pesos_2015"] = data["pib_miles_millones_2015"] * 1e9
    data["pib_por_trabajador_millones_2015"] = data["pib_pesos_2015"] / data["ocupados"] / 1e6
    data["pib_por_hora_pesos_2015"] = data["pib_pesos_2015"] / data["horas_anuales"]
    return data.sort_values(["group_order", "anio"])


def summarize_productivity_disaggregation(
    data: pd.DataFrame,
    order: list[str],
) -> pd.DataFrame:
    start_year, end_year = 2010, 2025
    rows = []
    if data.empty:
        return pd.DataFrame()
    component_order = (
        data[["group_code", "group_order"]]
        .drop_duplicates()
        .sort_values(["group_order", "group_code"])
    )
    for code in component_order["group_code"]:
        part = data[data["group_code"] == code].sort_values("anio")
        if start_year not in set(part["anio"]) or end_year not in set(part["anio"]):
            continue
        start = part[part["anio"] == start_year].iloc[0]
        end = part[part["anio"] == end_year].iloc[0]
        start_hours = start["horas_anuales"] / start["ocupados"] / 52
        end_hours = end["horas_anuales"] / end["ocupados"] / 52
        rows.append(
            {
                "codigo": code,
                "actividad": end["group_name"],
                "actividad_corta": end["group_label_short"],
                "sector_code": end.get("sector_code"),
                "dane_groups": end.get("dane_groups", code),
                "group_order": end.get("group_order"),
                "pib_2010_billones": start["pib_pesos_2015"] / 1e12,
                "pib_2025_billones": end["pib_pesos_2015"] / 1e12,
                "crec_pib": cagr(
                    start["pib_pesos_2015"],
                    end["pib_pesos_2015"],
                    start_year,
                    end_year,
                ),
                "ocupados_2010_millones": start["ocupados"] / 1e6,
                "ocupados_2025_millones": end["ocupados"] / 1e6,
                "crec_ocupados": cagr(start["ocupados"], end["ocupados"], start_year, end_year),
                "horas_2010_millones": start["horas_anuales"] / 1e6,
                "horas_2025_millones": end["horas_anuales"] / 1e6,
                "crec_horas": cagr(start["horas_anuales"], end["horas_anuales"], start_year, end_year),
                "horas_semanales_2010": start_hours,
                "horas_semanales_2025": end_hours,
                "crec_horas_semanales": cagr(start_hours, end_hours, start_year, end_year),
                "pib_trabajador_2010": start["pib_por_trabajador_millones_2015"],
                "pib_trabajador_2025": end["pib_por_trabajador_millones_2015"],
                "crec_pib_trabajador": cagr(
                    start["pib_por_trabajador_millones_2015"],
                    end["pib_por_trabajador_millones_2015"],
                    start_year,
                    end_year,
                ),
                "pib_hora_2010": start["pib_por_hora_pesos_2015"] / 1000,
                "pib_hora_2025": end["pib_por_hora_pesos_2015"] / 1000,
                "crec_pib_hora": cagr(
                    start["pib_por_hora_pesos_2015"],
                    end["pib_por_hora_pesos_2015"],
                    start_year,
                    end_year,
                ),
            }
        )
    return pd.DataFrame(rows)


def growth_items(summary: pd.DataFrame, ascending: bool, n: int = 3) -> str:
    data = summary.sort_values("crecimiento_anualizado", ascending=ascending).head(n)
    return "; ".join(
        f"{row['actividad_corta']} ({fmt_pct_es(row['crecimiento_anualizado'])})"
        for _, row in data.iterrows()
    )


def productivity_items(summary: pd.DataFrame, column: str, ascending: bool, n: int = 3) -> str:
    data = summary.sort_values(column, ascending=ascending).head(n)
    return "; ".join(
        f"{row['actividad_corta']} ({fmt_pct_es(row[column])})" for _, row in data.iterrows()
    )


def write_productivity_summary_table(
    summary: pd.DataFrame,
    filename: str,
    label: str,
    caption: str,
    note: str,
    use_longtable: bool = False,
) -> None:
    table = summary.sort_values("crec_pib_trabajador", ascending=False)
    if use_longtable:
        lines = [
            r"\begingroup",
            r"\footnotesize",
            r"\begin{longtable}{p{0.34\textwidth}rrrrrr}",
            f"\\caption{{{escape_latex(caption)}}}\\label{{{label}}}\\\\",
            r"\toprule",
            r"& \multicolumn{3}{c}{PIB por trabajador} & \multicolumn{3}{c}{PIB por hora} \\",
            r"& \multicolumn{3}{c}{\footnotesize Millones de pesos de 2015} & \multicolumn{3}{c}{\footnotesize Miles de pesos de 2015} \\",
            r"\cmidrule(lr){2-4}\cmidrule(lr){5-7}",
            r"Actividad económica & 2010 & 2025 & Crec. & 2010 & 2025 & Crec. \\",
            r"\midrule",
            r"\endfirsthead",
            r"\toprule",
            r"& \multicolumn{3}{c}{PIB por trabajador} & \multicolumn{3}{c}{PIB por hora} \\",
            r"& \multicolumn{3}{c}{\footnotesize Millones de pesos de 2015} & \multicolumn{3}{c}{\footnotesize Miles de pesos de 2015} \\",
            r"\cmidrule(lr){2-4}\cmidrule(lr){5-7}",
            r"Actividad económica & 2010 & 2025 & Crec. & 2010 & 2025 & Crec. \\",
            r"\midrule",
            r"\endhead",
        ]
        end_lines = [
            r"\bottomrule",
            r"\end{longtable}",
            r"\endgroup",
            f"{{\\footnotesize {note}}}",
        ]
    else:
        lines = [
            r"\begin{table}[H]",
            r"\centering",
            f"\\caption{{{escape_latex(caption)}}}",
            f"\\label{{{label}}}",
            r"\footnotesize",
            r"\begin{tabular}{p{0.36\textwidth}rrrrrr}",
            r"\toprule",
            r"& \multicolumn{3}{c}{PIB por trabajador} & \multicolumn{3}{c}{PIB por hora} \\",
            r"& \multicolumn{3}{c}{\footnotesize Millones de pesos de 2015} & \multicolumn{3}{c}{\footnotesize Miles de pesos de 2015} \\",
            r"\cmidrule(lr){2-4}\cmidrule(lr){5-7}",
            r"Actividad económica & 2010 & 2025 & Crec. & 2010 & 2025 & Crec. \\",
            r"\midrule",
        ]
        end_lines = [
            r"\bottomrule",
            r"\end{tabular}",
            f"\\caption*{{\\footnotesize {note}}}",
            r"\end{table}",
        ]

    for _, row in table.iterrows():
        lines.append(
            f"{escape_latex(row['actividad_corta'])} & "
            f"{fmt_num_es(row['pib_trabajador_2010'], 1)} & "
            f"{fmt_num_es(row['pib_trabajador_2025'], 1)} & "
            f"{fmt_pct_es(row['crec_pib_trabajador'])} & "
            f"{fmt_num_es(row['pib_hora_2010'], 1)} & "
            f"{fmt_num_es(row['pib_hora_2025'], 1)} & "
            f"{fmt_pct_es(row['crec_pib_hora'])} \\\\"
        )
    lines.extend(end_lines)
    (SECTION_DIR / filename).write_text("\n".join(lines) + "\n", encoding="utf-8")


def write_productivity_25_section(
    data: pd.DataFrame,
    summary: pd.DataFrame,
    total: pd.DataFrame,
) -> None:
    summary.to_csv(
        TABLE_DIR / "pib_geih_productividad_25_summary.csv",
        index=False,
        encoding="utf-8-sig",
    )
    summary.to_csv(
        OUTPUT_TABLE_DIR / "pib_geih_productividad_25_summary.csv",
        index=False,
        encoding="utf-8-sig",
    )
    data.to_csv(
        TABLE_DIR / "pib_geih_productividad_25_series.csv",
        index=False,
        encoding="utf-8-sig",
    )
    data.to_csv(
        OUTPUT_TABLE_DIR / "pib_geih_productividad_25_series.csv",
        index=False,
        encoding="utf-8-sig",
    )

    note = (
        "Nota: PIB por trabajador en millones de pesos constantes de 2015; PIB por hora en miles de pesos constantes de 2015. "
        "A nivel de actividad económica, el numerador corresponde estrictamente al valor agregado bruto de cada actividad. "
        "Ocupados y horas se agregan desde GEIH usando la actividad económica reportada por cada ocupado. Cuando el código a cuatro dígitos permite una homologación clara, "
        "se usa esa apertura; cuando la apertura del DANE es más fina que la apertura laboral comparable, "
        "las subactividades del DANE se agrupan hasta el nivel laboral comparable. Fuente: cálculos propios con DANE y GEIH."
    )
    table_lines_file = "pib_geih_productividad_25_table.tex"
    write_productivity_summary_table(
        summary,
        table_lines_file,
        "tab:pib_geih_productividad_25",
        "Productividad laboral por actividad económica comparable, apertura de 25 agrupaciones CIIU, 2010--2025",
        note,
    )
    table_text = (SECTION_DIR / table_lines_file).read_text(encoding="utf-8").rstrip()

    total_start = total[total["anio"] == 2010].iloc[0]
    total_end = total[total["anio"] == 2025].iloc[0]
    aggregate_metrics = build_metric_rows(total_start, total_end)
    aggregate_growth = metric_growth_lookup(aggregate_metrics)
    aggregate_levels = metric_level_lookup(aggregate_metrics)

    detail_rows = []
    lines = [
        "La apertura de 25 agrupaciones permite mirar dentro de algunas de las grandes actividades económicas usadas en la sección anterior y repetir el mismo ejercicio de productividad laboral con una mayor desagregación. Cuando la GEIH no permite separar ocupados y horas al mismo nivel del PIB del DANE, las subactividades se presentan agrupadas en el nivel laboral comparable.",
        "",
        table_text,
        "",
        r"\textbf{La desagregación a 25 agrupaciones muestra que la heterogeneidad de la productividad es mayor que la observada en las doce agrupaciones.} Las mayores tasas de crecimiento del PIB por trabajador se observan en "
        + productivity_items(summary, "crec_pib_trabajador", ascending=False, n=3)
        + ". En el otro extremo, las menores tasas aparecen en "
        + productivity_items(summary, "crec_pib_trabajador", ascending=True, n=3)
        + ". La lectura por hora confirma el mismo mensaje general: las mayores mejoras en PIB por hora se concentran en "
        + productivity_items(summary, "crec_pib_hora", ascending=False, n=3)
        + ", mientras que los rezagos más marcados se ubican en "
        + productivity_items(summary, "crec_pib_hora", ascending=True, n=3)
        + ".",
        "",
        "A continuación se presenta el detalle de cada agrupación. Las comparaciones se hacen frente al agregado nacional para mantener el mismo punto de referencia usado en la sección anterior.",
        "",
    ]

    detail_order = (
        data[["group_code", "group_order"]]
        .drop_duplicates()
        .sort_values(["group_order", "group_code"])
    )
    for code in detail_order["group_code"]:
        part = data[data["group_code"] == code].sort_values("anio")
        if 2010 not in set(part["anio"]) or 2025 not in set(part["anio"]):
            continue
        start = part[part["anio"] == 2010].iloc[0]
        end = part[part["anio"] == 2025].iloc[0]
        metrics = build_metric_rows(start, end)
        metrics["codigo"] = code
        metrics["actividad"] = end["group_label_short"]
        detail_rows.append(metrics)

        lines.extend(
            [
                f"\\subsection{{{escape_latex(end['group_label_short'])}}}",
                "",
                f"Esta agrupación corresponde al nivel laboral comparable para {escape_latex(end['group_label_short'])}.",
                "",
                *metric_table_lines(
                    metrics,
                    f"tab:agg25_{latex_id(code)}_productividad",
                    f"{end['group_label_short']}: PIB, ocupados, horas y productividad laboral, 2010--2025",
                ),
                "",
                sector_comparison_paragraph(metrics, aggregate_growth),
                "",
                sector_level_paragraph(metrics, aggregate_levels),
                "",
            ]
        )

    if detail_rows:
        detail = pd.concat(detail_rows, ignore_index=True)
        detail.to_csv(
            TABLE_DIR / "pib_geih_productividad_25_detalle.csv",
            index=False,
            encoding="utf-8-sig",
        )
        detail.to_csv(
            OUTPUT_TABLE_DIR / "pib_geih_productividad_25_detalle.csv",
            index=False,
            encoding="utf-8-sig",
        )

    lines.append(
        r"{\footnotesize Nota general: para facilitar la lectura, el informe usa PIB por actividad económica; en sentido estricto, el numerador corresponde al valor agregado bruto de cada actividad reportado por el DANE. Ocupados expandidos con el factor \texttt{fex}. Las horas semanales promedio se calculan como horas anuales totales divididas por ocupados y por 52 semanas. Se excluye 2020 por no contar con GEIH anual comparable en la base del proyecto. Fuente: cálculos propios con DANE y GEIH.}"
    )
    (SECTION_DIR / "pib_geih_productividad_25_agrupaciones.tex").write_text(
        "\n".join(lines) + "\n", encoding="utf-8"
    )


def write_productivity_61_section(data: pd.DataFrame, summary: pd.DataFrame) -> None:
    summary.to_csv(
        TABLE_DIR / "pib_geih_productividad_61_summary.csv",
        index=False,
        encoding="utf-8-sig",
    )
    summary.to_csv(
        OUTPUT_TABLE_DIR / "pib_geih_productividad_61_summary.csv",
        index=False,
        encoding="utf-8-sig",
    )
    data.to_csv(
        TABLE_DIR / "pib_geih_productividad_61_series.csv",
        index=False,
        encoding="utf-8-sig",
    )
    data.to_csv(
        OUTPUT_TABLE_DIR / "pib_geih_productividad_61_series.csv",
        index=False,
        encoding="utf-8-sig",
    )

    note = (
        "Nota: PIB por trabajador en millones de pesos constantes de 2015; PIB por hora en miles de pesos constantes de 2015. "
        "A nivel de actividad económica, el numerador corresponde estrictamente al valor agregado bruto de cada actividad. "
        "La tabla parte de las 61 agrupaciones del DANE, pero solo presenta observaciones para las que también existe un nivel laboral comparable en la GEIH. "
        "Cuando el código de actividad económica a cuatro dígitos permite una homologación clara, se usa esa apertura; cuando varias subactividades del DANE comparten la misma información laboral comparable, se reportan agrupadas. "
        f"En particular, el renglón Alimentos, bebidas y tabaco {COMPARABLE_LABEL_NOTES['Alimentos, bebidas y tabaco']}. Fuente: cálculos propios con DANE y GEIH."
    )
    table_lines_file = "pib_geih_productividad_61_table.tex"
    write_productivity_summary_table(
        summary,
        table_lines_file,
        "tab:pib_geih_productividad_61",
        "Productividad laboral por actividad económica comparable, apertura de 61 agrupaciones CIIU, 2010--2025",
        note,
        use_longtable=True,
    )
    table_text = (SECTION_DIR / table_lines_file).read_text(encoding="utf-8").rstrip()
    lines = [
        "La tabla presenta la apertura máxima comparable que puede construirse a partir de las 61 agrupaciones de actividad económica del DANE. Cuando la GEIH no permite separar ocupados y horas con el mismo detalle, las subactividades del DANE aparecen agrupadas en la observación laboral comparable.",
        "",
        table_text,
    ]
    (SECTION_DIR / "pib_geih_productividad_61_agrupaciones.tex").write_text(
        "\n".join(lines) + "\n", encoding="utf-8"
    )


def write_pib_ocupados_appendix(summary: pd.DataFrame) -> None:
    table = summary.sort_values(["group_order", "actividad_corta"]).copy()
    table["var_pib"] = table["pib_2025_billones"] / table["pib_2010_billones"] - 1
    table["var_ocupados"] = (
        table["ocupados_2025_millones"] / table["ocupados_2010_millones"] - 1
    )
    export = table[
        [
            "codigo",
            "actividad_corta",
            "dane_groups",
            "pib_2010_billones",
            "pib_2025_billones",
            "var_pib",
            "ocupados_2010_millones",
            "ocupados_2025_millones",
            "var_ocupados",
        ]
    ].copy()
    export.to_csv(
        TABLE_DIR / "pib_ocupados_61_comparable.csv",
        index=False,
        encoding="utf-8-sig",
    )
    export.to_csv(
        OUTPUT_TABLE_DIR / "pib_ocupados_61_comparable.csv",
        index=False,
        encoding="utf-8-sig",
    )

    lines = [
        r"\begingroup",
        r"\footnotesize",
        r"\begin{longtable}{p{0.34\textwidth}rrrrrr}",
        r"\caption{PIB y ocupados por actividad económica comparable, apertura de 61 agrupaciones CIIU, 2010--2025}\label{tab:pib_ocupados_61_comparable}\\",
        r"\toprule",
        r"& \multicolumn{3}{c}{PIB real} & \multicolumn{3}{c}{Ocupados} \\",
        r"& \multicolumn{3}{c}{\footnotesize Billones de pesos de 2015} & \multicolumn{3}{c}{\footnotesize Millones de personas} \\",
        r"\cmidrule(lr){2-4}\cmidrule(lr){5-7}",
        r"Actividad económica & 2010 & 2025 & Var. & 2010 & 2025 & Var. \\",
        r"\midrule",
        r"\endfirsthead",
        r"\toprule",
        r"& \multicolumn{3}{c}{PIB real} & \multicolumn{3}{c}{Ocupados} \\",
        r"& \multicolumn{3}{c}{\footnotesize Billones de pesos de 2015} & \multicolumn{3}{c}{\footnotesize Millones de personas} \\",
        r"\cmidrule(lr){2-4}\cmidrule(lr){5-7}",
        r"Actividad económica & 2010 & 2025 & Var. & 2010 & 2025 & Var. \\",
        r"\midrule",
        r"\endhead",
    ]
    for _, row in table.iterrows():
        lines.append(
            f"{escape_latex(row['actividad_corta'])} & "
            f"{fmt_num_es(row['pib_2010_billones'], 1)} & "
            f"{fmt_num_es(row['pib_2025_billones'], 1)} & "
            f"{fmt_pct_es(row['var_pib'], 1)} & "
            f"{fmt_num_es(row['ocupados_2010_millones'], 2)} & "
            f"{fmt_num_es(row['ocupados_2025_millones'], 2)} & "
            f"{fmt_pct_es(row['var_ocupados'], 1)} \\\\"
        )
    lines.extend(
        [
            r"\bottomrule",
            r"\end{longtable}",
            r"\endgroup",
            rf"{{\footnotesize Nota: el PIB se expresa en billones de pesos constantes de 2015 y los ocupados en millones de personas. La variación corresponde al cambio porcentual acumulado entre 2010 y 2025, no a una tasa anualizada. La tabla parte de la apertura de 61 agrupaciones del DANE, pero agrupa subactividades cuando la GEIH no permite separar ocupados al mismo nivel. En particular, el renglón Alimentos, bebidas y tabaco {COMPARABLE_LABEL_NOTES['Alimentos, bebidas y tabaco']}. Fuente: cálculos propios con DANE y GEIH.}}",
        ]
    )
    (SECTION_DIR / "pib_ocupados_61_comparable.tex").write_text(
        "\n".join(lines) + "\n", encoding="utf-8"
    )


def write_va_25_section(summary: pd.DataFrame) -> None:
    summary.to_csv(
        TABLE_DIR / "valor_agregado_25_agrupaciones.csv",
        index=False,
        encoding="utf-8-sig",
    )
    summary.to_csv(
        OUTPUT_TABLE_DIR / "valor_agregado_25_agrupaciones.csv",
        index=False,
        encoding="utf-8-sig",
    )

    table = summary.sort_values("crecimiento_anualizado", ascending=False)
    lines = [
        r"\begin{table}[H]",
        r"\centering",
        r"\caption{PIB por actividad económica, 25 agrupaciones CIIU, 2010--2025}",
        r"\label{tab:valor_agregado_25_agrupaciones}",
        r"\small",
        r"\begin{tabular}{p{0.50\textwidth}rrr}",
        r"\toprule",
        r"Actividad económica & 2010 & 2025 & Crec. \\",
        r"\midrule",
    ]
    for _, row in table.iterrows():
        lines.append(
            f"{escape_latex(row['actividad_corta'])} & "
            f"{fmt_num_es(row['va_2010_billones'], 1)} & "
            f"{fmt_num_es(row['va_2025_billones'], 1)} & "
            f"{fmt_pct_es(row['crecimiento_anualizado'])} \\\\"
        )
    lines.extend(
        [
            r"\bottomrule",
            r"\end{tabular}",
            r"\caption*{\footnotesize Nota: valores en billones de pesos constantes de 2015. El cuadro usa las 25 agrupaciones de actividad económica del DANE por el enfoque de producción. En sentido estricto, el numerador corresponde al valor agregado bruto de cada actividad. Fuente: cálculos propios con DANE, anexo de producción a precios constantes.}",
            r"\end{table}",
            "",
            r"\textbf{La desagregación a 25 agrupaciones muestra que buena parte de la heterogeneidad está dentro de las grandes actividades económicas.} Las mayores tasas de crecimiento del PIB se observan en "
            + growth_items(summary, ascending=False, n=3)
            + ". En el otro extremo, las menores tasas se registran en "
            + growth_items(summary, ascending=True, n=3)
            + ". Esta lectura ayuda a identificar dónde se originan los cambios agregados: no basta con saber que una gran actividad crece o cae, porque dentro de ella pueden coexistir subactividades con trayectorias muy distintas.",
        ]
    )
    (SECTION_DIR / "valor_agregado_25_agrupaciones.tex").write_text(
        "\n".join(lines) + "\n", encoding="utf-8"
    )


def write_va_61_section(summary: pd.DataFrame) -> None:
    summary.to_csv(
        TABLE_DIR / "valor_agregado_61_agrupaciones.csv",
        index=False,
        encoding="utf-8-sig",
    )
    summary.to_csv(
        OUTPUT_TABLE_DIR / "valor_agregado_61_agrupaciones.csv",
        index=False,
        encoding="utf-8-sig",
    )

    top = summary.sort_values("crecimiento_anualizado", ascending=False).head(10).copy()
    bottom = summary.sort_values("crecimiento_anualizado", ascending=True).head(10).copy()
    top["grupo"] = "Mayores"
    bottom["grupo"] = "Menores"
    table = pd.concat([top, bottom], ignore_index=True)

    lines = [
        r"\begin{table}[H]",
        r"\centering",
        r"\caption{Mayores y menores crecimientos del PIB, 61 agrupaciones CIIU, 2010--2025}",
        r"\label{tab:valor_agregado_61_agrupaciones}",
        r"\small",
        r"\begin{tabular}{llp{0.46\textwidth}rr}",
        r"\toprule",
        r"Grupo & Código & Actividad económica & 2025 & Crec. \\",
        r"\midrule",
    ]
    for _, row in table.iterrows():
        lines.append(
            f"{escape_latex(row['grupo'])} & "
            f"{escape_latex(row['codigo'])} & "
            f"{escape_latex(row['actividad_corta'])} & "
            f"{fmt_num_es(row['va_2025_billones'], 1)} & "
            f"{fmt_pct_es(row['crecimiento_anualizado'])} \\\\"
        )
    lines.extend(
        [
            r"\bottomrule",
            r"\end{tabular}",
            r"\caption*{\footnotesize Nota: valores de 2025 en billones de pesos constantes de 2015. La tabla muestra los diez mayores y los diez menores crecimientos anualizados entre las 61 agrupaciones de actividad económica del DANE. En sentido estricto, el numerador corresponde al valor agregado bruto de cada actividad. La tabla completa se deja como archivo de resultados del proyecto. Fuente: cálculos propios con DANE, anexo de producción a precios constantes.}",
            r"\end{table}",
            "",
            r"\textbf{La apertura a 61 agrupaciones confirma que el crecimiento del PIB es aún más desigual cuando se mira con mayor detalle.} Entre las agrupaciones de mayor crecimiento aparecen "
            + growth_items(summary, ascending=False, n=4)
            + ". En contraste, las mayores contracciones se observan en "
            + growth_items(summary, ascending=True, n=4)
            + ". Esta evidencia no reemplaza la medición de productividad laboral de las doce agrupaciones, pero sí permite ubicar con mayor precisión las actividades que explican el dinamismo o el rezago del PIB.",
        ]
    )
    (SECTION_DIR / "valor_agregado_61_agrupaciones.tex").write_text(
        "\n".join(lines) + "\n", encoding="utf-8"
    )


def write_va_disaggregation_sections(
    geih_labor25: pd.DataFrame, geih_labor61: pd.DataFrame, total: pd.DataFrame
) -> tuple[pd.DataFrame, pd.DataFrame]:
    va25 = load_va_disaggregation("Cuadro 2", code_col=2, concept_col=3)
    va61 = load_va_disaggregation("Cuadro 3", code_col=2, concept_col=3)
    prod25 = build_productivity_disaggregation(
        va25,
        geih_labor25,
        AGG25_LABOR_POOLS,
        AGG25_ORDER,
        AGG25_SHORT,
    )
    prod61 = build_productivity_disaggregation(
        va61,
        geih_labor61,
        AGG61_LABOR_POOLS,
        AGG61_ORDER,
        AGG61_SHORT,
    )
    summary25 = summarize_productivity_disaggregation(prod25, AGG25_ORDER)
    summary61 = summarize_productivity_disaggregation(prod61, AGG61_ORDER)
    write_productivity_25_section(prod25, summary25, total)
    write_productivity_61_section(prod61, summary61)
    write_va_25_section(summarize_va_disaggregation(va25, AGG25_SHORT))
    write_va_61_section(summarize_va_disaggregation(va61, AGG61_SHORT))
    return summary25, summary61


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

    draw.text((80, 35), "Crecimiento anualizado de la productividad laboral por actividad económica, 2010--2025", fill="#222222", font=title_font)
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


def draw_sector_correlation_scatter(summary: pd.DataFrame) -> None:
    data = summary[
        [
            "codigo",
            "actividad_corta",
            "crec_ocupados",
            "crec_horas",
            "crec_pib_trabajador",
            "crec_pib_hora",
        ]
    ].copy()
    data = data.rename(
        columns={
            "codigo": "sector_code",
            "actividad_corta": "sector_name_short",
            "crec_ocupados": "ocupados",
            "crec_horas": "horas",
            "crec_pib_trabajador": "prod_trabajador",
            "crec_pib_hora": "prod_hora",
        }
    )

    img = Image.new("RGB", (1800, 1250), "white")
    draw = ImageDraw.Draw(img)
    font = ImageFont.load_default()
    title_font = ImageFont.truetype("arial.ttf", 32) if Path(r"C:\Windows\Fonts\arial.ttf").exists() else font
    label_font = ImageFont.truetype("arial.ttf", 20) if Path(r"C:\Windows\Fonts\arial.ttf").exists() else font
    small_font = ImageFont.truetype("arial.ttf", 17) if Path(r"C:\Windows\Fonts\arial.ttf").exists() else font

    draw.text((70, 35), "Crecimiento del trabajo y la productividad por actividad económica, 2010--2025", fill="#222222", font=title_font)
    draw.text((70, 78), f"Tasas anualizadas para {len(data)} observaciones detalladas comparables; cada punto representa una actividad", fill="#555555", font=label_font)

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
            if len(data) <= 15:
                draw.text((x + 9, y - 10), row["sector_code"], fill="#333333", font=small_font)

        corr = data[[x_col, y_col]].corr().iloc[0, 1]
        draw.text((plot_left + 10, plot_top + 10), f"r = {corr:.2f}", fill="#b44b3f", font=label_font)
        draw.text(((plot_left + plot_right) / 2 - 90, bottom - 38), f"Crec. {x_lab.lower()}", fill="#333333", font=small_font)

    draw.text((70, 1210), "Nota: la línea roja muestra la tendencia lineal simple entre actividades económicas.", fill="#555555", font=small_font)

    for directory in [FIGURE_DIR, OUTPUT_FIGURE_DIR]:
        img.save(directory / "fig_pib_geih_productividad_sector_correlaciones.png")


def draw_initial_productivity_growth_scatter(summary: pd.DataFrame) -> None:
    data = summary[
        [
            "actividad_corta",
            "pib_trabajador_2010",
            "pib_hora_2010",
            "crec_pib_trabajador",
            "crec_pib_hora",
        ]
    ].copy()
    data = data.dropna().copy()
    data = data[(data["pib_trabajador_2010"] > 0) & (data["pib_hora_2010"] > 0)]

    img = Image.new("RGB", (2200, 900), "white")
    draw = ImageDraw.Draw(img)
    font = ImageFont.load_default()
    title_font = ImageFont.truetype("arial.ttf", 34) if Path(r"C:\Windows\Fonts\arial.ttf").exists() else font
    label_font = ImageFont.truetype("arial.ttf", 22) if Path(r"C:\Windows\Fonts\arial.ttf").exists() else font
    small_font = ImageFont.truetype("arial.ttf", 18) if Path(r"C:\Windows\Fonts\arial.ttf").exists() else font
    tiny_font = ImageFont.truetype("arial.ttf", 15) if Path(r"C:\Windows\Fonts\arial.ttf").exists() else font

    draw.text(
        (80, 40),
        "Nivel inicial de productividad y crecimiento anualizado, 2010--2025",
        fill="#222222",
        font=title_font,
    )
    draw.text(
        (80, 84),
        f"Apertura fina comparable: {len(data)} actividades económicas; eje horizontal en escala logarítmica",
        fill="#555555",
        font=label_font,
    )

    panels = [
        (
            90,
            170,
            1055,
            760,
            "pib_trabajador_2010",
            "crec_pib_trabajador",
            "PIB por trabajador",
            "Millones de pesos de 2015 por trabajador",
        ),
        (
            1180,
            170,
            2145,
            760,
            "pib_hora_2010",
            "crec_pib_hora",
            "PIB por hora trabajada",
            "Miles de pesos de 2015 por hora",
        ),
    ]
    tick_candidates = [2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000, 5000]
    y_min = math.floor(min(data["crec_pib_trabajador"].min(), data["crec_pib_hora"].min()) * 100) / 100 - 0.01
    y_max = math.ceil(max(data["crec_pib_trabajador"].max(), data["crec_pib_hora"].max()) * 100) / 100 + 0.01

    def pct_label(value: float) -> str:
        return f"{value * 100:.0f}%"

    def fmt_corr(value: float) -> str:
        return f"{value:.2f}".replace(".", ",")

    for left, top, right, bottom, x_col, y_col, title, x_label in panels:
        plot_left, plot_top = left + 100, top + 55
        plot_right, plot_bottom = right - 45, bottom - 80
        values_x = data[x_col].astype(float).to_numpy()
        values_y = data[y_col].astype(float).to_numpy()
        log_x = np.log10(values_x)
        x_min = log_x.min() - 0.08
        x_max = log_x.max() + 0.08

        draw.text((left, top), title, fill="#222222", font=label_font)
        draw.rectangle((plot_left, plot_top, plot_right, plot_bottom), outline="#333333", width=2)

        def x_pos(value: float) -> float:
            return plot_left + (math.log10(value) - x_min) / (x_max - x_min) * (plot_right - plot_left)

        def y_pos(value: float) -> float:
            return plot_bottom - (value - y_min) / (y_max - y_min) * (plot_bottom - plot_top)

        for tick in tick_candidates:
            if values_x.min() <= tick <= values_x.max():
                x = x_pos(tick)
                draw.line((x, plot_top, x, plot_bottom), fill="#eeeeee", width=1)
                draw.text((x - 18, plot_bottom + 14), f"{tick}", fill="#555555", font=small_font)
        for tick in np.arange(math.ceil(y_min * 100), math.floor(y_max * 100) + 1, 2):
            value = tick / 100
            y = y_pos(value)
            draw.line((plot_left, y, plot_right, y), fill="#eeeeee", width=1)
            draw.text((plot_left - 65, y - 11), pct_label(value), fill="#555555", font=small_font)

        if y_min < 0 < y_max:
            y0 = y_pos(0)
            draw.line((plot_left, y0, plot_right, y0), fill="#999999", width=2)

        slope, intercept = np.polyfit(log_x, values_y, 1)
        x1, x2 = 10 ** x_min, 10 ** x_max
        draw.line(
            (x_pos(x1), y_pos(slope * math.log10(x1) + intercept), x_pos(x2), y_pos(slope * math.log10(x2) + intercept)),
            fill="#b44b3f",
            width=4,
        )

        for _, row in data.iterrows():
            x = x_pos(row[x_col])
            y = y_pos(row[y_col])
            draw.ellipse((x - 8, y - 8, x + 8, y + 8), fill="#1f77b4", outline="white", width=2)

        labels = pd.concat(
            [
                data.nlargest(2, y_col),
                data.nsmallest(2, y_col),
                data.nlargest(1, x_col),
            ]
        ).drop_duplicates(subset=["actividad_corta"])
        for _, row in labels.iterrows():
            x = x_pos(row[x_col])
            y = y_pos(row[y_col])
            text = str(row["actividad_corta"])
            if len(text) > 22:
                text = text[:21] + "."
            text_x = x + 10 if x < plot_right - 210 else x - 190
            draw.text((max(plot_left + 8, text_x), y - 10), text, fill="#333333", font=tiny_font)

        corr = np.corrcoef(log_x, values_y)[0, 1]
        draw.text((plot_left + 12, plot_top + 12), f"r = {fmt_corr(corr)}", fill="#b44b3f", font=label_font)
        draw.text(((plot_left + plot_right) / 2 - 155, bottom - 45), x_label, fill="#333333", font=small_font)

    draw.text(
        (80, 830),
        "Nota: la línea roja muestra la tendencia lineal entre el crecimiento anualizado y el logaritmo del nivel de productividad en 2010.",
        fill="#555555",
        font=small_font,
    )

    for directory in [FIGURE_DIR, OUTPUT_FIGURE_DIR]:
        img.save(directory / "fig_pib_geih_productividad_nivel_inicial_crecimiento.png")


def main() -> None:
    total, total_summary, sector, sector_summary, geih_labor25, geih_labor61 = build_productivity()

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

    summary25, summary61 = write_va_disaggregation_sections(geih_labor25, geih_labor61, total)
    write_latex_tables(total, total_summary, sector_summary)
    write_sector_correlation_table(summary61)
    write_sector_detail_sections(sector, total, summary25, summary61)
    write_pib_ocupados_appendix(summary61)
    draw_index_chart(total)
    draw_sector_cagr_chart(sector_summary)
    draw_sector_correlation_scatter(summary61)
    draw_initial_productivity_growth_scatter(summary61)

    print("Resumen total")
    print(total_summary.to_string(index=False))
    print("\nActividades económicas ordenadas por crecimiento de PIB por trabajador")
    print(sector_summary.sort_values("crec_pib_trabajador", ascending=False).to_string(index=False))


if __name__ == "__main__":
    main()
