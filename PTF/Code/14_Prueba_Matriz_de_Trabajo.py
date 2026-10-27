"""Prueba: ¿las horas por actividad del DANE (implicitas en el anexo de PTF) siguen la Matriz de Trabajo (MT)?

Se leen las tablas "Horas trabajadas por actividad" de los boletines de la MT (se comparan solo anios dentro de un mismo boletin,
porque entre boletines cambia la definicion: 2018 incluye trabajo secundario y 2019 de la version nueva solo trabajo principal)
y se comparan con (a) las horas por actividad implicitas en el DANE y (b) las de la replica con la GEIH empalmada.

Grupos comparables (la MT agrupa comercio + transporte + alojamiento):
  A-B | C | D | E | F | G-H + I | J-K | L-Q
Entradas: PDFs de la MT descargados en MT_DIR.
"""
import importlib.util
import re
import sys
from pathlib import Path

import numpy as np
import pandas as pd

ROOT = Path(__file__).resolve().parents[1]
MT_DIR = Path(sys.argv[1]) if len(sys.argv) > 1 else Path(".")
spec = importlib.util.spec_from_file_location("v2", Path(__file__).with_name("07_PTF_v2_horasEfectivas.py"))
v2 = importlib.util.module_from_spec(spec)
spec.loader.exec_module(v2)

MT_ACT = [("Agricultura", "A"), ("Explotación de minas", "B"), ("Industrias manufactureras", "C"), ("Suministro de electricidad", "D"), ("Construcción", "F"),
          ("Comercio al por mayor", "GHI"), ("Información y comunicaciones", "J"), ("Actividades financieras", "K"), ("Actividades inmobiliarias", "L"),
          ("Actividades profesionales", "MN"), ("Administración pública", "OPQ"), ("Actividades artísticas", "RSTU")]
GRUPO = {"A": "A-B", "B": "C", "C": "D", "D": "E", "F": "F", "GHI": "GHI+J", "J": "GHI+J", "K": "J-K", "L": "J-K", "MN": "J-K", "OPQ": "L-Q", "RSTU": "L-Q"}
RAMA_A_GRUPO = {"A-B_AgriculturaPesca": "A-B", "C_Mineria": "C", "D_Manufactura": "D", "E_ElectricidadGasAgua": "E", "F_Construccion": "F",
                "G-H_ComercioHotelesRest": "GHI+J", "I_TransporteComunicaciones": "GHI+J", "J-K_FinancieroInmobiliario": "J-K",
                "L-Q_ServiciosSocialesComunales": "L-Q"}
# (boletin, titulo de la tabla, anio)
TABLAS = [("BP_Matriz_empleo_2015-2016-2017p.pdf.txt", 2016, 2017), ("BP_Matriz_empleo_2018-2019p.pdf.txt", 2018, 2019), ("BP_Matriz_empleo_2019-2020p.pdf.txt", 2019, 2020)]


def num(s):
    s = s.strip().replace(".", "").replace(",", ".")
    return float(s) if re.fullmatch(r"\d+(\.\d+)?", s) else None


def leer_tablas(path):
    t = [x.strip() for x in open(path, encoding="utf-8").read().split("\n") if x.strip()]
    tablas = []
    for i, l in enumerate(t):
        if re.match(r"Cuadro \d+\.\s+Horas trabajadas", l):
            m = re.match(r"(\d{4})", t[i + 2]) if i + 2 < len(t) else None
            if not m:
                continue
            anio, fila, vals = int(m.group(1)), 0, {}
            j = i + 3
            while j < min(i + 80, len(t)) and len(vals) < 12:
                for nombre, cod in MT_ACT:
                    if t[j].startswith(nombre) and cod not in vals:
                        k = j + 1
                        while k < len(t) and num(t[k]) is None:
                            k += 1
                        vals[cod] = num(t[k])
                j += 1
            tablas.append((anio, vals))
    return tablas


def main():
    mt = {}
    for f, a0, a1 in TABLAS:
        tb = dict(leer_tablas(MT_DIR / f))
        mt[(f, a0)], mt[(f, a1)] = tb.get(a0), tb.get(a1)
    filas_mt = []
    for f, a0, a1 in TABLAS:
        x0, x1 = mt[(f, a0)], mt[(f, a1)]
        if not x0 or not x1 or len(x0) < 12 or len(x1) < 12:
            print("tabla incompleta", f, a0, a1, len(x0 or {}), len(x1 or {}))
            continue
        for g in sorted(set(GRUPO.values())):
            h0 = sum(x0[c] for c in x0 if GRUPO[c] == g)
            h1 = sum(x1[c] for c in x1 if GRUPO[c] == g)
            filas_mt.append({"anio": a1, "grupo": g, "dlnH_MT": np.log(h1 / h0)})
    mtdf = pd.DataFrame(filas_mt)

    d = pd.read_csv(ROOT / "Outputs" / "tables" / "ptf_diagnostico_por_actividad.csv")
    c = pd.read_csv(ROOT / "Datos" / "Processed" / "GEIH_ptf_celdas_efectivas_educdet.csv")
    RAMAS = {1: "A-B_AgriculturaPesca", 2: "C_Mineria", 3: "D_Manufactura", 4: "E_ElectricidadGasAgua", 5: "F_Construccion", 6: "G-H_ComercioHotelesRest",
             7: "I_TransporteComunicaciones", 8: "J-K_FinancieroInmobiliario", 9: "L-Q_ServiciosSocialesComunales"}
    c["rama_ptf"] = c.rama.map(RAMAS)
    c["h"] = np.where(c.pers6850 > 0, c.H6850 / c.pers6850 * c.pers, np.nan)
    c["grupo"] = c.rama_ptf.map(RAMA_A_GRUPO)
    H = c.groupby(["anio", "grupo"]).h.sum().unstack()
    ours = pd.DataFrame([{"anio": a, "grupo": g, "dlnH_propia": np.log(H.loc[a, g] / H.loc[a - 1, g])} for a in range(2016, 2025) if a - 1 in H.index and a != 2020 and a != 2021 for g in H.columns])
    d["grupo"] = d.rama_ptf.map(RAMA_A_GRUPO)
    # horas DANE por grupo: promedio ponderado por las horas de la replica del anio anterior
    d["w"] = d.apply(lambda r: H.loc[r.anio - 1, r.grupo] if r.anio - 1 in H.index else np.nan, axis=1)
    # (dos ramas en el grupo GHI+J: se ponderan por sus horas de la GEIH)
    hr = c.groupby(["anio", "rama_ptf"]).h.sum()
    d["w"] = d.apply(lambda r: hr.get((r.anio - 1, r.rama_ptf), np.nan), axis=1)
    dane = d.dropna(subset=["w"]).groupby(["anio", "grupo"]).apply(lambda s: (s.dlnH_DANE * s.w).sum() / s.w.sum(), include_groups=False).rename("dlnH_DANE").reset_index()
    m = mtdf.merge(dane, on=["anio", "grupo"]).merge(ours, on=["anio", "grupo"], how="left")
    pd.set_option("display.width", 250)
    print((m.set_index(["anio", "grupo"]) * 100).round(1).to_string())
    for lab, col in (("MT vs DANE", "dlnH_MT"), ("replica GEIH vs DANE", "dlnH_propia")):
        s = m.dropna(subset=[col])
        print(lab, "n=", len(s), "corr", round(np.corrcoef(s[col], s.dlnH_DANE)[0, 1], 2), "MAE pp", round((s[col] - s.dlnH_DANE).abs().mean() * 100, 2))
    m.to_csv(ROOT / "Outputs" / "tables" / "ptf_prueba_matriz_trabajo.csv", index=False)


if __name__ == "__main__":
    main()
