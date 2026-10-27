"""Prueba de confiabilidad 2: error de muestreo y robustez de la composicion laboral y las horas de la GEIH 2026 (enero-agosto frente a enero-agosto de 2025).

1. Bootstrap por vivienda (DIRECTORIO, comun a los dos anos: las viviendas se repiten en la muestra rotativa): remuestrea viviendas con reemplazo y recalcula
   el aporte del trabajo, las horas y la composicion con la misma receta de 18_Trabajo_GEIH_2026.py.
2. Composicion mes a mes (cada mes de 2026 contra el mismo mes de 2025) para ver si el patron esta en todos los meses o en uno solo.
3. Variables nuevas de 2026 (P3560-P3563) y etiquetas de la educacion (P3042) en 2025 y 2026.

Entradas: C:/Users/jorge/Documents/Databases/GEIH/GEIH_2025_total.dta y GEIH_2026_total.dta
Salidas : PTF/Outputs/tables/geih2026_bootstrap_composicion.csv, geih2026_composicion_mensual.csv, geih2026_variables_nuevas.csv
Uso     : python PTF/Code/20_Bootstrap_composicion.py
"""
import importlib.util
from pathlib import Path

import numpy as np
import pandas as pd
import pyreadstat

ROOT = Path(__file__).resolve().parents[1]
T = ROOT / "Outputs" / "tables"
GEIH = Path("C:/Users/jorge/Documents/Databases/GEIH")
NB = 300
rng = np.random.default_rng(7)


def cargar(nombre):
    spec = importlib.util.spec_from_file_location(nombre[:2], Path(__file__).with_name(nombre))
    m = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(m)
    return m


import contextlib
import io

with contextlib.redirect_stdout(io.StringIO()):
    t18 = cargar("18_Trabajo_GEIH_2026.py")
W = t18.W24
S_, WL_ = W.s.values, W.wL.values            # orden de ramas: el de v1.pesos (alfabetico por rama_ptf)
RAMAS = list(W.index)

COLS = ["MES", "DIRECTORIO", "FEX_C18", "OCI", "P6430", "P6850", "P6040", "P3271", "P3042", "RAMA2D_R4", "INGLABO"]


def leer(anio):
    df, meta = pyreadstat.read_dta(GEIH / f"GEIH_{anio}_total.dta", usecols=COLS)
    for c in COLS:
        if c != "RAMA2D_R4":
            df[c] = pd.to_numeric(df[c], errors="coerce")
    df["RAMA2D_R4"] = pd.to_numeric(df["RAMA2D_R4"], errors="coerce")
    df = df[(df.OCI == 1) & (df.FEX_C18 > 0) & (df.MES <= 8)].copy()
    return df, meta


def rama_ptf(x):
    """Rama PTF (1-9) desde CIIU Rev. 4 a 2 digitos, igual que map_rama en 06_ExtraerHorasEfectivas.do."""
    r = np.zeros(len(x), dtype=int)
    for lo, hi, g in ((1, 3, 1), (5, 9, 2), (10, 33, 3), (35, 39, 4), (41, 43, 5), (45, 47, 6), (55, 56, 6), (49, 53, 7), (58, 63, 7), (64, 66, 8),
                      (68, 68, 8), (69, 75, 8), (77, 82, 8), (84, 99, 9)):
        r[(x >= lo) & (x <= hi)] = g
    return r


def preparar(df):
    d = pd.DataFrame({"mes": df.MES.values, "dir": df.DIRECTORIO.values, "w": df.FEX_C18.values / 8.0})
    d["rama"] = rama_ptf(df.RAMA2D_R4.values)
    sexo = np.where(df.P3271 == 1, 1, np.where(df.P3271 == 2, 2, 0))
    edad = np.where((df.P6040 >= 15) & (df.P6040 <= 29), 1, np.where((df.P6040 >= 30) & (df.P6040 <= 49), 2, np.where(df.P6040 >= 50, 3, 0)))
    e = df.P3042.values
    educ_h = np.where((e >= 1) & (e <= 4), e, np.where((e == 5) | (e == 6), 5, np.where((e >= 7) & (e <= 13), 6, 0)))
    educ_g = np.select([np.isin(educ_h, [1, 2, 3]), np.isin(educ_h, [4, 5]), educ_h == 6], [1, 2, 3], 0)
    asal = np.isin(df.P6430.values, [1, 2, 3, 7]).astype(int)
    h = df.P6850.values
    ok_h = (~np.isnan(h)) & (h >= 0) & (h <= 168)
    ing = df.INGLABO.values
    ok_i = (~np.isnan(ing)) & (ing > 0)
    d["cat"] = np.where((sexo > 0) & (edad > 0) & (educ_g > 0), (sexo - 1) * 9 + (edad - 1) * 3 + (educ_g - 1), 18)
    d["fino"] = (((d.rama.values * 3 + sexo) * 4 + edad) * 7 * 2 + educ_h * 2 + asal).astype(int)      # celda fina para imputar las horas faltantes con el promedio de la celda
    d["okh"] = ok_h.astype(float)
    d["hrs"] = np.where(ok_h, h, 0.0)
    d["ing"] = np.where(ok_i, ing, 0.0)
    d = d[d.rama > 0].reset_index(drop=True)
    return d


def indices(d, mult):
    """Aporte del trabajo, de las horas y composicion de un conjunto de personas (con multiplicidades de bootstrap)."""
    W_ = d.w.values * mult
    nf = int(d.fino.max()) + 1
    P = np.bincount(d.fino.values, W_, nf)
    P6 = np.bincount(d.fino.values, W_ * d.okh.values, nf)
    H = np.bincount(d.fino.values, W_ * d.hrs.values * d.okh.values, nf)
    ING = np.bincount(d.fino.values, W_ * d.ing.values, nf)
    horas = np.where(P6 > 0, H / np.where(P6 > 0, P6, 1) * P, 0.0)
    # mapa de la celda fina a (rama, categoria)
    f = np.arange(nf)
    rama_f = f // (3 * 4 * 7 * 2)
    cat_f = np.full(nf, 18)
    cat_map = d.groupby("fino").cat.first()
    cat_f[cat_map.index.values] = cat_map.values
    key = rama_f * 19 + cat_f
    HX = np.bincount(key, horas, 10 * 19).reshape(10, 19)
    IX = np.bincount(key, ING, 10 * 19).reshape(10, 19)
    PX = np.bincount(key, P, 10 * 19).reshape(10, 19)
    return HX, IX, PX


def aporte(a, b):
    """a, b = (HX, IX, PX) de los dos periodos; devuelve L, horas, composicion (pp), crecimiento de horas totales y de ocupados (%)."""
    L = hor = 0.0
    for i, r in enumerate(RAMAS):
        k = i + 1                                   # las ramas PTF 1..9 siguen el orden alfabetico de RAMAS (A-B, C, D, E, F, G-H, I, J-K, L-Q)
        x0, x1 = a[0][k, :18], b[0][k, :18]
        v0, v1 = a[1][k, :18], b[1][k, :18]
        v0, v1 = v0 / v0.sum(), v1 / v1.sum()
        ok = (x0 > 0) & (x1 > 0)
        dL = (((v0 + v1) / 2)[ok] * (np.log(x1[ok]) - np.log(x0[ok]))).sum()
        dH = np.log(x1.sum()) - np.log(x0.sum())
        L += S_[i] * WL_[i] * dL
        hor += S_[i] * WL_[i] * dH
    gH = 100 * np.log(b[0][1:10, :].sum() / a[0][1:10, :].sum())
    gN = 100 * np.log(b[2][1:10, :].sum() / a[2][1:10, :].sum())
    return 100 * L, 100 * hor, 100 * (L - hor), gH, gN


d25, m25 = leer(2025)
d26, m26 = leer(2026)
d25, d26 = preparar(d25), preparar(d26)
print("ocupados muestra:", len(d25), len(d26))

# ---- punto central (debe parecerse a 18_Trabajo_GEIH_2026.py: L 1,16; horas 0,01; composicion 1,15; horas totales -0,98; ocupados 2,31)
centro = aporte(indices(d25, np.ones(len(d25))), indices(d26, np.ones(len(d26))))
print("punto central L, horas, comp, gH, gN:", np.round(centro, 3))

# ---- bootstrap por vivienda (las viviendas se comparten entre anos)
ids = np.unique(np.concatenate([d25.dir.values, d26.dir.values]))
pos25 = np.searchsorted(ids, d25.dir.values)
pos26 = np.searchsorted(ids, d26.dir.values)
res = []
for b in range(NB):
    m = np.bincount(rng.integers(0, len(ids), len(ids)), minlength=len(ids)).astype(float)
    res.append(aporte(indices(d25, m[pos25]), indices(d26, m[pos26])))
res = pd.DataFrame(res, columns=["L_pp", "horas_pp", "composicion_pp", "horas_totales_%", "ocupados_%"])
res["horas_por_ocupado_%"] = res["horas_totales_%"] - res["ocupados_%"]
resumen = pd.DataFrame({"centro": dict(zip(["L_pp", "horas_pp", "composicion_pp", "horas_totales_%", "ocupados_%"], centro)),
                        "bootstrap_media": res.drop(columns="horas_por_ocupado_%").mean(), "bootstrap_desv": res.drop(columns="horas_por_ocupado_%").std(),
                        "p05": res.drop(columns="horas_por_ocupado_%").quantile(0.05), "p95": res.drop(columns="horas_por_ocupado_%").quantile(0.95)})
resumen.loc["horas_por_ocupado_%"] = [centro[3] - centro[4], res["horas_por_ocupado_%"].mean(), res["horas_por_ocupado_%"].std(),
                                      res["horas_por_ocupado_%"].quantile(0.05), res["horas_por_ocupado_%"].quantile(0.95)]
resumen.round(3).to_csv(T / "geih2026_bootstrap_composicion.csv")
print(resumen.round(3).to_string())

# ---- mes a mes
filas = []
for mes in range(1, 9):
    a = preparar_a = d25[d25.mes == mes]
    b = d26[d26.mes == mes]
    sc = 8.0   # los pesos ya dividen por 8; para un mes solo importa la proporcion
    r = aporte(indices(a, np.ones(len(a))), indices(b, np.ones(len(b))))
    filas.append({"mes": mes, "L_pp": r[0], "horas_pp": r[1], "composicion_pp": r[2], "horas_totales_%": r[3], "ocupados_%": r[4], "horas_por_ocupado_%": r[3] - r[4]})
mensual = pd.DataFrame(filas).round(2)
mensual.to_csv(T / "geih2026_composicion_mensual.csv", index=False)
print(mensual.to_string(index=False))

# ---- variables nuevas y etiquetas de educacion
cols_meta = {}
_, meta26 = pyreadstat.read_dta(GEIH / "GEIH_2026_total.dta", metadataonly=True)
_, meta25 = pyreadstat.read_dta(GEIH / "GEIH_2025_total.dta", metadataonly=True)
nuevas = ["P1879S1", "P3363S1", "P3560", "P3561", "P3562", "P3563"]
tab = pd.DataFrame({"variable": nuevas, "etiqueta_2026": [meta26.column_names_to_labels.get(v) for v in nuevas]})
tab.to_csv(T / "geih2026_variables_nuevas.csv", index=False, encoding="utf-8-sig")
print(tab.to_string(index=False))
print("P3042 etiqueta 2025:", meta25.column_names_to_labels.get("P3042"), "| 2026:", meta26.column_names_to_labels.get("P3042"))
print("P3042 valores 2025:", meta25.variable_value_labels.get("P3042"))
print("P3042 valores 2026:", meta26.variable_value_labels.get("P3042"))
