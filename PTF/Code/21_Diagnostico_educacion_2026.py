"""Prueba de confiabilidad 3: el salto de la educacion en la GEIH 2026 (enero-agosto) frente a 2023-2025.

Pregunta: la composicion laboral de +1,15 pp de 2026 sale de una proporcion de adultos con educacion superior que sube 1,8 pp en un ano. Es un cambio real,
un cambio de la muestra o de la medicion?  Se mira (i) la serie 2023-2026, (ii) por grupos de edad y condicion de actividad, (iii) dentro de cada departamento,
(iv) los codigos de P3042, (v) las ocupaciones.  La documentacion del DANE (control de cambios del formulario 2022-2026 y diccionario de datos 2026) esta en
Datos/Raw/GEIH_DANE_documentacion/: no registra cambios en las preguntas de educacion despues de 2022 (las variables nuevas de 2026 son P3560-P3563, sobre RUT
y personeria juridica, P1879S1 y P3363S1, texto "otro, cual").
P3042: 1 ninguno, 2 preescolar, 3 basica primaria, 4 basica secundaria, 5 media academica, 6 media tecnica, 7 normalista, 8 tecnica profesional,
9 tecnologica, 10 universitaria, 11 especializacion, 12 maestria, 13 doctorado.  "Superior" = 7 a 13 (igual que 06_ExtraerHorasEfectivas.do).

Entradas: C:/Users/jorge/Documents/Databases/GEIH/GEIH_<anio>_total.dta (2023-2026)
Salidas : PTF/Outputs/tables/geih2026_diag_educacion_serie.csv, _edad.csv, _departamentos.csv, _codigos.csv, _ocupaciones.csv
Uso     : python PTF/Code/21_Diagnostico_educacion_2026.py
"""
from pathlib import Path

import numpy as np
import pandas as pd
import pyreadstat

ROOT = Path(__file__).resolve().parents[1]
T = ROOT / "Outputs" / "tables"
GEIH = Path("C:/Users/jorge/Documents/Databases/GEIH")
COLS = ["MES", "FEX_C18", "OCI", "P3042", "P6040", "DPTO", "OFICIO_C8"]
D = {}
for a in (2023, 2024, 2025, 2026):
    df, _ = pyreadstat.read_dta(GEIH / f"GEIH_{a}_total.dta", usecols=COLS)
    for c in ("MES", "FEX_C18", "OCI", "P3042", "P6040", "DPTO"):
        df[c] = pd.to_numeric(df[c], errors="coerce")
    df["of2"] = pd.to_numeric(df.OFICIO_C8.astype(str).str.zfill(4).str[:2], errors="coerce")
    df = df[(df.FEX_C18 > 0) & (df.P6040 >= 25) & (df.MES <= 8) & df.P3042.notna()].copy()
    df["sup"] = ((df.P3042 >= 7) & (df.P3042 <= 13)).astype(float)
    df["ocup"] = df.OCI == 1
    D[a] = df


def pct(x, mask=None):
    m = np.ones(len(x), bool) if mask is None else mask
    return 100 * np.average(x.sup[m], weights=x.FEX_C18[m])


# (i) serie
serie = pd.DataFrame({a: {"todos_25+": pct(d), "ocupados_25+": pct(d, d.ocup.values), "no_ocupados_25+": pct(d, ~d.ocup.values)} for a, d in D.items()}).T
serie["cambio_todos_pp"] = serie["todos_25+"].diff()
serie.round(2).to_csv(T / "geih2026_diag_educacion_serie.csv")
print("== proporcion con educacion superior (25+, enero-agosto)"); print(serie.round(2).to_string())

# (ii) edad
bins = [24, 34, 44, 54, 64, 120]
labs = ["25-34", "35-44", "45-54", "55-64", "65+"]
ed = pd.DataFrame({a: D[a].groupby(pd.cut(D[a].P6040, bins, labels=labs)).apply(lambda x: pct(x)) for a in D})
ed["dif_2025_2026"] = ed[2026] - ed[2025]
ed["dif_2024_2025"] = ed[2025] - ed[2024]
ed.round(2).to_csv(T / "geih2026_diag_educacion_edad.csv")
print("\n== por edad"); print(ed.round(2).to_string())

# (iii) departamentos
dp = pd.DataFrame({a: D[a].groupby("DPTO").apply(lambda x: pd.Series({"sup": pct(x), "pob": x.FEX_C18.sum() / 8 / 1e6})) .stack() for a in (2025, 2026)}).unstack()
dpt = pd.DataFrame({"sup_2025": D[2025].groupby("DPTO").apply(pct), "sup_2026": D[2026].groupby("DPTO").apply(pct),
                    "pob_2025": D[2025].groupby("DPTO").FEX_C18.sum() / 8 / 1e6})
dpt["dif_pp"] = dpt.sup_2026 - dpt.sup_2025
dentro = float(((dpt.pob_2025 / dpt.pob_2025.sum()) * dpt.dif_pp).sum())
dpt.round(2).to_csv(T / "geih2026_diag_educacion_departamentos.csv")
print(f"\n== dentro de departamento: {dentro:.2f} pp de {serie.loc[2026, 'todos_25+'] - serie.loc[2025, 'todos_25+']:.2f}; departamentos con aumento: {(dpt.dif_pp > 0).sum()} de {len(dpt)}")

# (iv) codigos
cod = pd.DataFrame({a: 100 * D[a].groupby("P3042").FEX_C18.sum() / D[a].FEX_C18.sum() for a in (2024, 2025, 2026)}).fillna(0)
cod["dif_2025_2026"] = cod[2026] - cod[2025]
cod["dif_2024_2025"] = cod[2025] - cod[2024]
cod.round(2).to_csv(T / "geih2026_diag_educacion_codigos.csv")
print("\n== por codigo de P3042"); print(cod.round(2).to_string())

# (v) ocupaciones (ocupados de 25+): directivos, profesionales y tecnicos = CIUO-08 grandes grupos 1 a 3
oc = {}
for a, d in D.items():
    o = d[d.ocup]
    prof = o.of2.between(11, 35)
    oc[a] = {"directivos_prof_tecnicos_%": 100 * np.average(prof, weights=o.FEX_C18), "superior_%": pct(o),
             "superior_en_prof_%": pct(o, prof.values), "superior_en_no_prof_%": pct(o, ~prof.values)}
oc = pd.DataFrame(oc).T
oc.round(2).to_csv(T / "geih2026_diag_educacion_ocupaciones.csv")
print("\n== ocupaciones"); print(oc.round(2).to_string())
