"""Prueba de confiabilidad 1: la receta del trabajo (horas efectivas y composicion laboral de la GEIH) aplicada a enero-septiembre de 2023, 2024 y 2025,
comparada con lo que el DANE publico para esos mismos periodos (boletines de productividad para la concertacion del salario minimo).

Que se prueba: si la medicion propia del trabajo reproduce los aportes de horas y composicion del DANE, y por tanto la productividad por hora y la PTF
que salen cuando se usan el valor agregado y el capital oficiales. No prueba el nowcast del valor agregado ni del capital.

Cifras del DANE (año corrido III trimestre, preliminares; aportes al crecimiento del VA, %):
  2023: VA 0,89; PTF -1,00; trabajo 0,91 (horas 0,55; composicion 0,35); capital 0,99; productividad por hora 0,76; por persona -0,70   [Productividad-2023pr.pdf]
  2024: VA 1,79; PTF 1,73; trabajo -0,63 (horas -0,73; composicion 0,10); capital 0,70; productividad por hora 3,43; por persona 1,76    [prod2024.pdf / prensa]
  2025: VA 2,91; PTF 0,91; trabajo 1,23 (horas 1,43; composicion -0,20); capital 0,76; productividad por hora 0,57; por persona -0,32   [Productividad-2025pr.pdf]

Entradas: PTF/Datos/Processed/GEIH_ptf_celdas_efectivas_ene-sep_2022_2025.csv (06_ExtraerHorasEfectivas.do con MAXMES = 9) y los de la replica
Salida  : PTF/Outputs/tables/geih_retrospectiva_ene_sep_2023_2025.csv
Uso     : python PTF/Code/19_Retrospectiva_ene_sep.py   (desde la raiz del repositorio)
"""
import importlib.util
from pathlib import Path

import numpy as np
import pandas as pd

ROOT = Path(__file__).resolve().parents[1]
T = ROOT / "Outputs" / "tables"
PD = ROOT / "Datos" / "Processed"


def cargar(nombre):
    spec = importlib.util.spec_from_file_location(nombre[:2], Path(__file__).with_name(nombre))
    m = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(m)
    return m


p08 = cargar("08_PTF_pruebas_definiciones.py")
v1 = p08.v1

DANE = {  # año corrido enero-septiembre, preliminar
    2023: dict(VA=0.89, PTF=-1.00, L=0.91, horas=0.55, comp=0.35, K=0.99, LPh=0.76, LPp=-0.70),
    2024: dict(VA=1.79, PTF=1.73, L=-0.63, horas=-0.73, comp=0.10, K=0.70, LPh=3.43, LPp=1.76),
    2025: dict(VA=2.91, PTF=0.91, L=1.23, horas=1.43, comp=-0.20, K=0.76, LPh=0.57, LPp=-0.32),
}

full = pd.read_csv(PD / "GEIH_ptf_celdas_efectivas_educdet.csv")
r_full, _ = p08.preparar(full, "actual", "R4", False)
p, _ = v1.cargar_panel()
p = p.drop(columns=["H_EMP", "H_EMPE"]).merge(r_full[["anio", "rama_ptf", "H_EMP", "H_EMPE"]], on=["anio", "rama_ptf"], how="left")


def pesos_par(a0, a1):
    w0 = v1.pesos(p, a0)
    w1 = v1.pesos(p, a1) if a1 <= 2024 else w0          # 2025: sin COU, se usan los pesos de 2024
    return (w0.s + w1.s) / 2, (w0.wL + w1.wL) / 2


c = pd.read_csv(PD / "GEIH_ptf_celdas_efectivas_ene-sep_2022_2025.csv")
r, cel = p08.preparar(c, "actual", "R4", False)
H = cel.groupby(["anio", "rama_ptf"]).horas_pond.sum().unstack()
cc = c[c.rama > 0].copy()
cc["horas"] = np.where(cc.pers6850 > 0, cc.H6850 / cc.pers6850 * cc.pers, np.nan)
tot = cc.groupby("anio").agg(horas=("horas", "sum"), pers=("pers", "sum"))

filas = []
for a1 in (2023, 2024, 2025):
    a0 = a1 - 1
    s, wL = pesos_par(a0, a1)
    dL = v1.indice_L(cel, a0, a1)
    dH = np.log(H.loc[a1]) - np.log(H.loc[a0])
    L = (s * wL * dL).sum() * 100
    hor = (s * wL * dH).sum() * 100
    gH = 100 * np.log(tot.horas[a1] / tot.horas[a0])
    gN = 100 * np.log(tot.pers[a1] / tot.pers[a0])
    d = DANE[a1]
    filas.append({"anio": a1, "horas_total_%": gH, "ocupados_%": gN,
                  "L_propio": L, "L_DANE": d["L"], "dif_L": L - d["L"],
                  "horas_propio": hor, "horas_DANE": d["horas"], "dif_horas": hor - d["horas"],
                  "comp_propio": L - hor, "comp_DANE": d["comp"], "dif_comp": (L - hor) - d["comp"],
                  "PTF_propia(VA y K del DANE)": d["VA"] - L - d["K"], "PTF_DANE": d["PTF"], "dif_PTF": d["VA"] - L - d["K"] - d["PTF"],
                  "prod_hora_propia(VA del DANE)": d["VA"] - gH, "prod_hora_DANE": d["LPh"], "dif_prod_hora": d["VA"] - gH - d["LPh"],
                  "prod_persona_propia": d["VA"] - gN, "prod_persona_DANE": d["LPp"], "dif_prod_persona": d["VA"] - gN - d["LPp"]})
out = pd.DataFrame(filas).set_index("anio").round(2)
out.to_csv(T / "geih_retrospectiva_ene_sep_2023_2025.csv")
pd.set_option("display.width", 250)
print(out.T.to_string())
print("\nMAE: L", out.dif_L.abs().mean().round(2), "| horas", out.dif_horas.abs().mean().round(2), "| composicion", out.dif_comp.abs().mean().round(2),
      "| PTF", out.dif_PTF.abs().mean().round(2), "| prod por hora", out.dif_prod_hora.abs().mean().round(2))
