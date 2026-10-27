"""
Pruebas de sensibilidad del indice de trabajo (PTF v2): que definicion de las 18 categorias y de la rama
reproduce mejor el aporte del trabajo y la PTF del Cuadro 1 del DANE.

Insumo: Datos/Processed/GEIH_ptf_celdas_efectivas_educdet.csv (06_ExtraerHorasEfectivas.do)
        con educacion en 6 niveles y las ramas con CIIU Rev. 4 (rama) y Rev. 3 (rama3).

Variantes de educacion ("basica, media, superior"):
  actual   : bajo = ninguno-primaria | medio = secundaria + media | alto = superior
  basica   : bajo = ninguno-secundaria (educacion basica = grados 1-9) | medio = media (10-11) | alto = superior
Variantes de rama:
  R4       : CIIU Rev. 4 del archivo (como en v2)
  R3       : CIIU Rev. 3 nativa para 2010-2019 (R4 desde 2021, solo existe esa)
"""
from __future__ import annotations

import importlib.util
from pathlib import Path

import numpy as np
import pandas as pd

ROOT = Path(__file__).resolve().parents[1]
PTF_DIR = ROOT / "Datos" / "Processed"
CSV = PTF_DIR / "GEIH_ptf_celdas_efectivas_educdet.csv"

_spec = importlib.util.spec_from_file_location("v1", Path(__file__).with_name("04_PTF_v1_GEIHcompleta.py"))
v1 = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(v1)

RAMAS = {1: "A-B_AgriculturaPesca", 2: "C_Mineria", 3: "D_Manufactura", 4: "E_ElectricidadGasAgua", 5: "F_Construccion",
         6: "G-H_ComercioHotelesRest", 7: "I_TransporteComunicaciones", 8: "J-K_FinancieroInmobiliario", 9: "L-Q_ServiciosSocialesComunales"}
EDUC = {
    "actual": {1: 1, 2: 1, 3: 1, 4: 2, 5: 2, 6: 3},
    "basica": {1: 1, 2: 1, 3: 1, 4: 1, 5: 2, 6: 3},
}
PERIODOS = [2015, 2016, 2017, 2018, 2019, 2022, 2023, 2024]


def preparar(c: pd.DataFrame, educ: str, rama: str, extra_asal: bool) -> tuple[pd.DataFrame, pd.DataFrame]:
    c = c.copy()
    col = "rama3" if rama == "R3" else "rama"
    c["rama_u"] = np.where((c.anio <= 2019) & (c[col] > 0), c[col], c["rama"])
    c = c[c.rama_u > 0]
    c["rama_ptf"] = c["rama_u"].map(RAMAS)
    c["educ_g"] = c["educ_h"].map(EDUC[educ]).fillna(0).astype(int)
    c["horas"] = np.where(c.pers6850 > 0, c.H6850 / c.pers6850 * c.pers, np.nan)
    r = c.groupby(["anio", "rama_ptf", "asal"]).horas.sum().unstack("asal").fillna(0).reset_index()
    r.columns = ["anio", "rama_ptf", "H_no_asal", "H_EMPE"]
    r["H_EMP"] = r.H_no_asal + r.H_EMPE
    k = c[(c.sexo > 0) & (c.edad_g > 0) & (c.educ_g > 0)].copy()
    k["categoria"] = k.sexo.astype(str) + "_" + k.edad_g.astype(str) + "_" + k.educ_g.astype(str)
    if extra_asal:
        k["categoria"] += "_" + k.asal.astype(str)
    cel = k.groupby(["anio", "rama_ptf", "categoria"], as_index=False).agg(horas_pond=("horas", "sum"), ingreso_pond=("ing", "sum"))
    return r, cel


def correr(c: pd.DataFrame, educ: str, rama: str, extra_asal: bool = False) -> pd.DataFrame:
    r, cel = preparar(c, educ, rama, extra_asal)
    p, _ = v1.cargar_panel()
    p = p.drop(columns=["H_EMP", "H_EMPE"]).merge(r[["anio", "rama_ptf", "H_EMP", "H_EMPE"]], on=["anio", "rama_ptf"], how="left")
    of = v1.oficial()
    filas = []
    for a1 in PERIODOS:
        a0 = a1 - 1
        w0, w1 = v1.pesos(p, a0), v1.pesos(p, a1)
        wL, wK, s = (w0.wL + w1.wL) / 2, (w0.wK + w1.wK) / 2, (w0.s + w1.s) / 2
        dV = np.log(w1.va_con) - np.log(w0.va_cor)
        dK = np.log(w1.stock) - np.log(w0.stock)
        dL = v1.indice_L(cel, a0, a1)
        aL, aK = wL * dL, wK * dK
        filas.append({"anio": a1, "L": (s * aL).sum() * 100, "PTF": (s * (dV - aL - aK)).sum() * 100,
                      "L_DANE": of.loc[a1, "L"], "PTF_DANE": of.loc[a1, "PTF"]})
    return pd.DataFrame(filas).set_index("anio")


def main() -> None:
    c = pd.read_csv(CSV)
    out = []
    for educ in ("actual", "basica"):
        for rama in ("R4", "R3"):
            for asal in (False, True):
                d = correr(c, educ, rama, asal)
                pre, post = d.loc[2015:2019], d.loc[2022:2024]
                out.append({"educacion": educ, "rama": rama, "36_celdas": asal,
                            "MAE_L_15_19": (pre.L - pre.L_DANE).abs().mean(), "MAE_PTF_15_19": (pre.PTF - pre.PTF_DANE).abs().mean(),
                            "corr_L_15_19": np.corrcoef(pre.L, pre.L_DANE)[0, 1], "L_acum": pre.L.sum(), "L_acum_DANE": pre.L_DANE.sum(),
                            "MAE_L_22_24": (post.L - post.L_DANE).abs().mean(), "MAE_PTF_22_24": (post.PTF - post.PTF_DANE).abs().mean()})
    pd.set_option("display.width", 250)
    res = pd.DataFrame(out).round(2)
    print(res.to_string(index=False))
    res.to_csv(ROOT / "Outputs" / "tables" / "ptf_pruebas_definiciones.csv", index=False)


if __name__ == "__main__":
    main()
