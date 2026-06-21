from pathlib import Path
import csv
import json


ROOT = Path(__file__).resolve().parents[1]
GEOJSON_PATH = ROOT / "DocumentacionAuxiliar" / "Geometria" / "gadm41_COL_1.json"
OUT_PATH = ROOT / "DocumentacionAuxiliar" / "Geometria" / "gadm41_COL_1_polygons.csv"


def normalize_department(name: str) -> str:
    replacements = {
        "Bogot\u00e1D.C.": "Bogot\u00e1 D.C.",
        "LaGuajira": "La Guajira",
        "NortedeSantander": "Norte de Santander",
        "Nari\u00f1o": "Nari\u00f1o",
        "SanAndr\u00e9syProvidencia": "San Andr\u00e9s y Providencia",
        "ValledelCauca": "Valle del Cauca",
    }
    return replacements.get(name, name)


def iter_rings(geometry: dict):
    geom_type = geometry["type"]
    coords = geometry["coordinates"]
    if geom_type == "Polygon":
        for polygon_idx, polygon in enumerate([coords]):
            for ring_idx, ring in enumerate(polygon):
                yield polygon_idx, ring_idx, ring
    elif geom_type == "MultiPolygon":
        for polygon_idx, polygon in enumerate(coords):
            for ring_idx, ring in enumerate(polygon):
                yield polygon_idx, ring_idx, ring
    else:
        raise ValueError(f"Unsupported geometry type: {geom_type}")


def main() -> None:
    data = json.loads(GEOJSON_PATH.read_text(encoding="utf-8"))
    rows = []
    for feature_idx, feature in enumerate(data["features"], start=1):
        props = feature["properties"]
        gid = props["GID_1"]
        name = normalize_department(props["NAME_1"])
        for polygon_idx, ring_idx, ring in iter_rings(feature["geometry"]):
            group = f"{gid}_{polygon_idx}_{ring_idx}"
            for point_idx, point in enumerate(ring):
                rows.append(
                    {
                        "gid": gid,
                        "departamento_geo": name,
                        "group": group,
                        "polygon": polygon_idx,
                        "ring": ring_idx,
                        "point": point_idx,
                        "lon": point[0],
                        "lat": point[1],
                    }
                )

    OUT_PATH.parent.mkdir(parents=True, exist_ok=True)
    with OUT_PATH.open("w", newline="", encoding="utf-8-sig") as fh:
        writer = csv.DictWriter(
            fh,
            fieldnames=["gid", "departamento_geo", "group", "polygon", "ring", "point", "lon", "lat"],
        )
        writer.writeheader()
        writer.writerows(rows)
    print(f"Wrote {len(rows)} vertices to {OUT_PATH}")


if __name__ == "__main__":
    main()
