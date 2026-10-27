"""
PTF v1 (enfoque de valor agregado, K y L) con la GEIH completa.

Por que existe esta version
---------------------------
La v0 (00c + 01 + 02) agregaba las horas desde `Paper-GEIH_base_modelo_personas_2008_2025.dta`,
que solo conserva a los ocupados con ingreso horario valido. Esa proporcion cambia mucho entre
anios (86 % en 2021, 94 % en 2022), asi que las horas de la v0 crecian 17 % entre 2021 y 2022 y
el residual de la PTF salia con -7 pp. `GEIHfinal.dta` trae a todos los ocupados con horas y
factor de expansion (suma 20,4 millones en 2021 y 22,0 millones en 2022, igual que la GEIH oficial).

Que hace
--------
1. Agrega `GEIHfinal.dta` por anio x rama PTF x categoria (sexo x edad x educacion, 18 categorias):
   - horas de TODOS los ocupados (indice de servicios laborales L),
   - ingreso laboral solo de quienes lo reportan (participaciones v_l del indice de Tornqvist),
   - horas de ocupados y de asalariados (ajuste de la remuneracion, LAB_j).
2. Recalcula la PTF con las formulas del documento metodologico del DANE (valor agregado).
   Reutiliza el COU (VA corriente y constante, remuneracion) y el stock de capital productivo
   oficial ya extraidos en `Datos/Processed/`.
3. Compara con el Cuadro 1 del anexo oficial (aportes al crecimiento, puntos porcentuales).

2020 no tiene GEIH comparable: 2021 se calcula como cambio 2019 -> 2021 y se compara con la
suma de 2020 y 2021 del DANE.

Uso:  python PTF/Code/04_PTF_v1_GEIHcompleta.py [--agregar]
      (--agregar vuelve a leer el .dta de 2,2 GB; sin la bandera usa los csv ya generados)
"""
from __future__ import annotations

import sys
from pathlib import Path

import numpy as np
import pandas as pd

ROOT = Path(__file__).resolve().parents[1]
PTF_DIR = ROOT / "Datos" / "Processed"
REPO = Path(__file__).resolve().parents[2]  # raiz del repositorio: ahi vive la GEIH consolidada (pesada, no se mueve)
GEIH_PATH = REPO / "Datos" / "Processed" / "GEIH" / "GEIHfinal.dta"
OFICIAL = ROOT / "Datos" / "Raw" / "anex-PTF-Productividad-2025.xlsx"
OUT_CELDAS = PTF_DIR / "GEIHfinal_celdas_categoria.csv"
OUT_HORAS = PTF_DIR / "GEIHfinal_horas_totales_rama.csv"
OUT_TABLE_DIR = ROOT / "Outputs" / "tables"

COLUMNS = ["anio", "edad", "sector", "posicion_ocupacional_label", "educacion", "sexo", "fex", "horas", "ingreso_laboral_hora"]

SECTOR_A_RAMA_PTF = {
    "Agricultura, ganadería, silvicultura y pesca": "A-B_AgriculturaPesca",
    "Minas y canteras": "C_Mineria",
    "Industrias manufactureras": "D_Manufactura",
    "Electricidad, gas, agua y saneamiento": "E_ElectricidadGasAgua",
    "Construcción": "F_Construccion",
    "Comercio y reparación": "G-H_ComercioHotelesRest",
    "Alojamiento y servicios de comida": "G-H_ComercioHotelesRest",
    "Transporte y almacenamiento": "I_TransporteComunicaciones",
    "Información y comunicaciones": "I_TransporteComunicaciones",
    "Actividades financieras y de seguros": "J-K_FinancieroInmobiliario",
    "Inmobiliarias, profesionales y administrativas": "J-K_FinancieroInmobiliario",
    "Administración pública y defensa": "L-Q_ServiciosSocialesComunales",
    "Educación": "L-Q_ServiciosSocialesComunales",
    "Salud y asistencia social": "L-Q_ServiciosSocialesComunales",
    "Artes, recreación y otros servicios": "L-Q_ServiciosSocialesComunales",
    "Hogares como empleadores": "L-Q_ServiciosSocialesComunales",
    "Organizaciones extraterritoriales": "L-Q_ServiciosSocialesComunales",
}
EDUC_A_GRUPO = {
    "Ninguno": "1_Bajo", "Preescolar": "1_Bajo", "Básica primaria": "1_Bajo",
    "Básica secundaria": "2_Medio", "Media": "2_Medio", "Superior o universitaria": "3_Alto",
}
ASALARIADO_LABELS = {
    "Obrero o empleado de empresa particular", "Obrero o empleado del gobierno", "Empleado doméstico", "Jornalero o peón",
}


# --------------------------------------------------------------------------- 1. agregar la GEIH
def agregar_geih(chunksize: int = 500_000) -> None:
    celdas, horas = [], []
    for i, ch in enumerate(pd.read_stata(GEIH_PATH, columns=COLUMNS, chunksize=chunksize)):
        ch = ch[ch["fex"] > 0].copy()
        ch["rama_ptf"] = ch["sector"].map(SECTOR_A_RAMA_PTF)
        ch["grupo_educ"] = ch["educacion"].map(EDUC_A_GRUPO)
        ch["grupo_edad"] = pd.cut(ch["edad"], [14.5, 29.5, 49.5, 200], labels=["15-29", "30-49", "50+"]).astype(object)
        ch["asalariado"] = ch["posicion_ocupacional_label"].isin(ASALARIADO_LABELS)
        ch["horas_pond"] = ch["horas"] * ch["fex"]
        ch["ingreso_pond"] = ch["ingreso_laboral_hora"].where(ch["ingreso_laboral_hora"] > 0) * ch["horas_pond"]
        v = ch.dropna(subset=["rama_ptf", "grupo_educ", "grupo_edad", "sexo"])
        celdas.append(v.groupby(["anio", "rama_ptf", "sexo", "grupo_edad", "grupo_educ"], observed=True)
                      .agg(horas_pond=("horas_pond", "sum"), ingreso_pond=("ingreso_pond", "sum")).reset_index())
        v2 = ch.dropna(subset=["rama_ptf"])
        horas.append(v2.groupby(["anio", "rama_ptf", "asalariado"], observed=True).agg(horas_pond=("horas_pond", "sum")).reset_index())
        print("chunk", i + 1, flush=True)
    c = pd.concat(celdas).groupby(["anio", "rama_ptf", "sexo", "grupo_edad", "grupo_educ"], observed=True).sum().reset_index()
    h = pd.concat(horas).groupby(["anio", "rama_ptf", "asalariado"], observed=True).sum().reset_index()
    c.to_csv(OUT_CELDAS, index=False)
    h.to_csv(OUT_HORAS, index=False)


# --------------------------------------------------------------------------- 2. PTF
def cargar_panel() -> tuple[pd.DataFrame, pd.DataFrame]:
    cor = pd.read_csv(PTF_DIR / "COU_corrientes_rama.csv").pivot_table(index=["anio", "rama_ptf"], columns="concepto", values="valor").reset_index()
    con = pd.read_csv(PTF_DIR / "COU_constantes_rama.csv").pivot_table(index=["anio", "rama_ptf"], columns="concepto", values="valor").reset_index()
    cor = cor.rename(columns={"valor_agregado": "va_cor", "remuneracion_asalariados": "comp"})[["anio", "rama_ptf", "va_cor", "comp"]]
    con = con.rename(columns={"valor_agregado": "va_con"})[["anio", "rama_ptf", "va_con"]]
    stock = pd.read_csv(PTF_DIR / "StockCapitalProductivo_rama.csv").rename(columns={"stock_capital_productivo": "stock"})
    h = pd.read_csv(OUT_HORAS)
    h = h.pivot_table(index=["anio", "rama_ptf"], columns="asalariado", values="horas_pond", fill_value=0).reset_index()
    h.columns = ["anio", "rama_ptf", "H_no_asal", "H_EMPE"]
    h["H_EMP"] = h["H_no_asal"] + h["H_EMPE"]
    p = cor.merge(con, on=["anio", "rama_ptf"], how="outer").merge(stock, on=["anio", "rama_ptf"], how="left").merge(
        h[["anio", "rama_ptf", "H_EMP", "H_EMPE"]], on=["anio", "rama_ptf"], how="left")
    p = p[p["rama_ptf"] != "TOT_Economia"].sort_values(["rama_ptf", "anio"]).reset_index(drop=True)
    cel = pd.read_csv(OUT_CELDAS)
    cel["categoria"] = cel["sexo"] + "_" + cel["grupo_edad"] + "_" + cel["grupo_educ"]
    return p, cel[["anio", "rama_ptf", "categoria", "horas_pond", "ingreso_pond"]]


def indice_L(cel: pd.DataFrame, a0: int, a1: int) -> pd.Series:
    """dln(L) por rama entre a0 y a1 (Tornqvist sobre las 18 categorias)."""
    out = {}
    for rama, d in cel.groupby("rama_ptf"):
        x0 = d[d.anio == a0].set_index("categoria")
        x1 = d[d.anio == a1].set_index("categoria")
        cats = x0.index.intersection(x1.index)
        x0, x1 = x0.loc[cats], x1.loc[cats]
        ok = (x0.horas_pond > 0) & (x1.horas_pond > 0)
        v0 = d[d.anio == a0].set_index("categoria").ingreso_pond
        v1 = d[d.anio == a1].set_index("categoria").ingreso_pond
        v0, v1 = v0 / v0.sum(), v1 / v1.sum()
        vbar = (v0.reindex(cats) + v1.reindex(cats)) / 2
        out[rama] = float((vbar[ok] * (np.log(x1.horas_pond[ok]) - np.log(x0.horas_pond[ok]))).sum())
    return pd.Series(out)


def pesos(p: pd.DataFrame, anio: int) -> pd.DataFrame:
    d = p[p.anio == anio].set_index("rama_ptf").copy()
    lab = d.H_EMP / d.H_EMPE * d.comp
    cap_raw = d.va_cor - lab
    lab = lab.where(cap_raw >= 0, d.va_cor)
    d["wL"] = lab / d.va_cor
    d["wK"] = cap_raw.clip(lower=0) / d.va_cor
    d["s"] = d.va_cor / d.va_cor.sum()
    return d[["wL", "wK", "s", "va_cor", "va_con", "stock"]]


def calcular(p: pd.DataFrame, cel: pd.DataFrame) -> pd.DataFrame:
    """Aportes al crecimiento (puntos porcentuales) por periodo. 2021 = cambio 2019 -> 2021."""
    periodos = [(a - 1, a, str(a)) for a in range(2015, 2020)] + [(2019, 2021, "2020-21")] + [(a - 1, a, str(a)) for a in range(2022, 2025)]
    filas, por_rama = [], []
    for a0, a1, nombre in periodos:
        w0, w1 = pesos(p, a0), pesos(p, a1)
        wL, wK, s = (w0.wL + w1.wL) / 2, (w0.wK + w1.wK) / 2, (w0.s + w1.s) / 2
        if a1 - a0 == 1:
            dV = np.log(w1.va_con) - np.log(w0.va_cor)
        else:  # 2020-21: suma de las dos variaciones anuales encadenadas
            pp = {a: pesos(p, a) for a in (2019, 2020, 2021)}
            dV = (np.log(pp[2020].va_con) - np.log(pp[2019].va_cor)) + (np.log(pp[2021].va_con) - np.log(pp[2020].va_cor))
        dK = np.log(w1.stock) - np.log(w0.stock)
        dL = indice_L(cel, a0, a1)
        r = pd.DataFrame({"dV": dV, "dK": dK, "dL": dL, "wL": wL, "wK": wK, "s": s})
        r["aporte_L"], r["aporte_K"] = r.wL * r.dL, r.wK * r.dK
        r["dT"] = r.dV - r.aporte_L - r.aporte_K
        r["periodo"] = nombre
        por_rama.append(r.reset_index())
        filas.append({"periodo": nombre, "VA": (s * r.dV).sum() * 100, "L": (s * r.aporte_L).sum() * 100,
                      "K": (s * r.aporte_K).sum() * 100, "PTF": (s * r.dT).sum() * 100, "dlnL": (s * r.dL).sum() * 100})
    pd.concat(por_rama).to_csv(PTF_DIR / "PTF_v1_por_rama.csv", index=False)
    return pd.DataFrame(filas)


def oficial() -> pd.DataFrame:
    x = pd.read_excel(OFICIAL, "Cuadro 1", header=None).iloc[15:36, 1:7]
    x.columns = ["anio", "VA", "L", "K", "contrib", "PTF"]
    x["anio"] = x.anio.astype(str).str.extract(r"(\d{4})")[0].astype(int)
    x = x.astype(float).set_index("anio")
    return x


def main() -> None:
    if "--agregar" in sys.argv or not OUT_CELDAS.exists():
        agregar_geih()
    p, cel = cargar_panel()
    res = calcular(p, cel)
    of = oficial()
    filas = []
    for _, r in res.iterrows():
        anios = [2020, 2021] if r.periodo == "2020-21" else [int(r.periodo)]
        o = of.loc[anios].sum()
        filas.append({"periodo": r.periodo, "VA_propia": r.VA, "VA_DANE": o.VA, "L_propia": r.L, "L_DANE": o.L, "K_propia": r.K, "K_DANE": o.K,
                      "PTF_propia": r.PTF, "PTF_DANE": o.PTF})
    cmp = pd.DataFrame(filas)
    for v in ("VA", "L", "K", "PTF"):
        cmp[f"dif_{v}"] = cmp[f"{v}_propia"] - cmp[f"{v}_DANE"]
    cmp = cmp.round(2)
    cmp.to_csv(PTF_DIR / "validacion_total_economia_v1.csv", index=False)
    OUT_TABLE_DIR.mkdir(parents=True, exist_ok=True)
    cmp.to_csv(OUT_TABLE_DIR / "ptf_v1_vs_dane_total_economia.csv", index=False)
    pd.set_option("display.width", 250)
    print(cmp.to_string(index=False))
    sub = cmp[cmp.periodo != "2020-21"]
    print("\nError absoluto medio (pp), sin 2020-21:",
          {v: round(sub[f"dif_{v}"].abs().mean(), 2) for v in ("VA", "L", "K", "PTF")})
    print("Correlacion PTF propia vs DANE:", round(np.corrcoef(cmp.PTF_propia, cmp.PTF_DANE)[0, 1], 2))
    print("Suma PTF propia / DANE 2015-2024:", round(cmp.PTF_propia.sum(), 2), round(cmp.PTF_DANE.sum(), 2))


if __name__ == "__main__":
    main()
