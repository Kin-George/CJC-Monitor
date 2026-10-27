"""
PTF v2: servicios laborales con horas EFECTIVAS (P6850) desde la GEIH cruda.

Insumo nuevo: Datos/Processed/GEIH_ptf_celdas_efectivas.csv (lo genera 06_ExtraerHorasEfectivas.do).
Todo lo demas (COU, stock de capital, formulas) es igual a la v1 (04_PTF_v1_GEIHcompleta.py).

Compara varias definiciones del insumo trabajo contra el Cuadro 1 del DANE:
  efectivas : horas de la semana pasada (P6850), lo que usa el DANE
  habituales: horas normales (P6800), lo que usaban v0 y v1
y, para cada una, ponderadores de Tornqvist con el ingreso laboral de quienes lo reportan
("sin_escalar") o escalado a todos los ocupados de la celda ("escalado").

Uso: python PTF/Code/07_PTF_v2_horasEfectivas.py
"""
from __future__ import annotations

import importlib.util
from pathlib import Path

import numpy as np
import pandas as pd

ROOT = Path(__file__).resolve().parents[1]
PTF_DIR = ROOT / "Datos" / "Processed"
CELDAS = PTF_DIR / "GEIH_ptf_celdas_efectivas.csv"
OUT_TABLE_DIR = ROOT / "Outputs" / "tables"

_spec = importlib.util.spec_from_file_location("v1", Path(__file__).with_name("04_PTF_v1_GEIHcompleta.py"))
v1 = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(v1)

RAMAS = {1: "A-B_AgriculturaPesca", 2: "C_Mineria", 3: "D_Manufactura", 4: "E_ElectricidadGasAgua", 5: "F_Construccion",
         6: "G-H_ComercioHotelesRest", 7: "I_TransporteComunicaciones", 8: "J-K_FinancieroInmobiliario", 9: "L-Q_ServiciosSocialesComunales"}


def cargar_celdas() -> pd.DataFrame:
    c = pd.read_csv(CELDAS)
    c20 = PTF_DIR / "GEIH_ptf_celdas_efectivas_2020.csv"  # 2020 se extrae aparte (factor del marco 2018, 8 meses con ocupados)
    if c20.exists():
        c = pd.concat([c[c.anio != 2020], pd.read_csv(c20)], ignore_index=True)
    c["rama_ptf"] = c["rama"].map(RAMAS)
    return c


def horas_variante(c: pd.DataFrame, tipo: str) -> pd.DataFrame:
    """Horas semanales expandidas por celda, escaladas a todos los ocupados (los que no reportan horas se imputan con el promedio de la celda)."""
    h, p = ("H6850", "pers6850") if tipo == "efectivas" else ("H6800", "pers6800")
    c = c.copy()
    c["horas"] = np.where(c[p] > 0, c[h] / c[p] * c["pers"], np.nan)
    return c


def paneles(c: pd.DataFrame, tipo: str, escalar_ingreso: bool):
    c = horas_variante(c, tipo)
    # ratio horas ocupados / horas asalariados por rama-anio (para LAB_j)
    r = c.groupby(["anio", "rama_ptf", "asal"]).horas.sum().unstack("asal").fillna(0).reset_index()
    r.columns = ["anio", "rama_ptf", "H_no_asal", "H_EMPE"]
    r["H_EMP"] = r.H_no_asal + r.H_EMPE
    # celdas de las 18 categorias (sin dato -> fuera)
    k = c[(c.sexo > 0) & (c.edad_g > 0) & (c.educ_g > 0)].copy()
    k["categoria"] = k.sexo.astype(str) + "_" + k.edad_g.astype(str) + "_" + k.educ_g.astype(str)
    k["ing_v"] = k["ing"] * (k["pers"] / k["pers_ing"].where(k["pers_ing"] > 0)) if escalar_ingreso else k["ing"]
    cel = k.groupby(["anio", "rama_ptf", "categoria"], as_index=False).agg(horas_pond=("horas", "sum"), ingreso_pond=("ing_v", "sum"))
    return r, cel


def panel_base(r: pd.DataFrame) -> pd.DataFrame:
    p, _ = v1.cargar_panel()
    return p.drop(columns=["H_EMP", "H_EMPE"]).merge(r[["anio", "rama_ptf", "H_EMP", "H_EMPE"]], on=["anio", "rama_ptf"], how="left")


def comparar(res: pd.DataFrame, of: pd.DataFrame) -> pd.DataFrame:
    filas = []
    for _, r in res.iterrows():
        anios = [2020, 2021] if r.periodo == "2020-21" else [int(r.periodo)]
        o = of.loc[anios].sum()
        filas.append({"periodo": r.periodo, "VA_propia": r.VA, "VA_DANE": o.VA, "L_propia": r.L, "L_DANE": o.L, "K_propia": r.K, "K_DANE": o.K,
                      "PTF_propia": r.PTF, "PTF_DANE": o.PTF})
    d = pd.DataFrame(filas)
    for v in ("VA", "L", "K", "PTF"):
        d[f"dif_{v}"] = d[f"{v}_propia"] - d[f"{v}_DANE"]
    return d


def main() -> None:
    c = cargar_celdas()
    c = c[c.anio != 2020]  # el archivo crudo de 2020 solo trae un subconjunto de ocupados con variables completas
    of = v1.oficial()
    resumen, detalle = [], {}
    for tipo in ("efectivas", "habituales"):
        for esc in (False, True):
            r, cel = paneles(c, tipo, esc)
            p = panel_base(r)
            d = comparar(v1.calcular(p, cel), of)
            nombre = (tipo, "escalado" if esc else "sin_escalar")
            detalle[nombre] = d
            resumen.append({"horas": tipo, "ingreso": nombre[1], "MAE_L": d.dif_L.abs().mean(), "MAE_K": d.dif_K.abs().mean(),
                            "MAE_PTF": d.dif_PTF.abs().mean(), "corr_L": np.corrcoef(d.L_propia, d.L_DANE)[0, 1],
                            "corr_PTF": np.corrcoef(d.PTF_propia, d.PTF_DANE)[0, 1], "PTF_acum": d.PTF_propia.sum(),
                            "PTF_acum_DANE": d.PTF_DANE.sum(), "signo_PTF_ok": int((np.sign(d.PTF_propia) == np.sign(d.PTF_DANE)).sum())})
    pd.set_option("display.width", 250)
    print(pd.DataFrame(resumen).round(2).to_string(index=False))
    pd.DataFrame(resumen).round(3).to_csv(OUT_TABLE_DIR / "ptf_v2_variantes_resumen.csv", index=False)
    mejor = detalle[("efectivas", "sin_escalar")].round(2)
    print(mejor.to_string(index=False))
    mejor.to_csv(OUT_TABLE_DIR / "ptf_v2_vs_dane_total_economia.csv", index=False)
    mejor.to_csv(PTF_DIR / "validacion_total_economia_v2.csv", index=False)


if __name__ == "__main__":
    main()
