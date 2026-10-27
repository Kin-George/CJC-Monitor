"""Trabajo medido con la GEIH 2026 (enero-agosto) frente a enero-agosto de 2025: ocupados, horas efectivas y composicion laboral.

Reemplaza tres supuestos de la simulacion (script 16): los meses de ocupados interpolados, el cambio de las horas por ocupado y la composicion laboral.
Sigue la replica de la PTF (scripts 04, 07, 08 y 12): horas efectivas (P6850) por celda sexo x edad x educacion x rama, indice de Tornqvist con el
ingreso laboral como ponderador y pesos de las ramas de 2024.

Entradas: PTF/Datos/Processed/GEIH_ptf_celdas_efectivas_ene-ago_2025_2026.csv   (06_ExtraerHorasEfectivas.do con MAXMES = 8)
          PTF/Datos/Processed/GEIH_ocupados_horas_mensual_2025_2026.csv         (06c_OcupadosHorasMensuales.do)
          PTF/Datos/Processed/GEIH_ptf_celdas_efectivas_educdet.csv             (2010-2025 completo, para validar el metodo con 2025)
Salidas : PTF/Outputs/tables/geih2026_trabajo_ene_ago.csv, geih2026_ocupados_mensual.csv, geih2026_validacion_metodo_2025.csv
Uso     : python PTF/Code/18_Trabajo_GEIH_2026.py   (desde la raiz del repositorio)
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


def pesos_2024():
    """Pesos de las ramas de 2024 (participacion en el VA y participacion del trabajo) con las horas efectivas del ano completo, como en la replica."""
    full = pd.read_csv(PD / "GEIH_ptf_celdas_efectivas_educdet.csv")
    r, _ = p08.preparar(full, "actual", "R4", False)
    p, _ = v1.cargar_panel()
    p = p.drop(columns=["H_EMP", "H_EMPE"]).merge(r[["anio", "rama_ptf", "H_EMP", "H_EMPE"]], on=["anio", "rama_ptf"], how="left")
    return v1.pesos(p, 2024), full


W24, FULL = pesos_2024()


def trabajo(c, a0, a1, detalle=False):
    """Variacion (log, en %) de las horas, los ocupados y la composicion laboral entre dos 'anios' de la base de celdas; aportes de L, horas y composicion."""
    r, cel = p08.preparar(c, "actual", "R4", False)
    s, wL = W24.s, W24.wL
    H = cel.groupby(["anio", "rama_ptf"]).horas_pond.sum().unstack()
    dL = v1.indice_L(cel, a0, a1)
    dH = np.log(H.loc[a1]) - np.log(H.loc[a0])
    L = (s * wL * dL).sum() * 100
    hor = (s * wL * dH).sum() * 100
    cc = c[c.rama > 0].copy()
    cc["horas"] = np.where(cc.pers6850 > 0, cc.H6850 / cc.pers6850 * cc.pers, np.nan)
    tot = cc.groupby("anio").agg(horas=("horas", "sum"), pers=("pers", "sum"))
    out = {"dlnHoras_%": 100 * np.log(tot.horas[a1] / tot.horas[a0]), "dlnOcupados_%": 100 * np.log(tot.pers[a1] / tot.pers[a0]),
           "aporte_L_pp": L, "aporte_horas_pp": hor, "aporte_composicion_pp": L - hor}
    if detalle:
        d = pd.DataFrame({"peso_s*wL": s * wL, "dlnL_%": 100 * dL, "dlnH_%": 100 * dH, "comp_%": 100 * (dL - dH)})
        d["aporte_composicion_pp"] = d["peso_s*wL"] * d["comp_%"]
        return out, d.round(3)
    return out


# ---- validacion del metodo: 2025 frente a 2024 con el ano completo (la replica propia de 12_PTF_2025_completar.py da horas 1,292, composicion -0,141, L 1,151)
full = FULL
val = []
for a in (2023, 2024, 2025):
    v = trabajo(full, a - 1, a)
    v["anio"] = a
    val.append(v)
val = pd.DataFrame(val).set_index("anio").round(3)
val.to_csv(T / "geih2026_validacion_metodo_2025.csv")
print("== validacion (ano completo)"); print(val.to_string())

# ---- enero-agosto: 2025 y 2026 con los mismos meses
ene_ago = pd.read_csv(PD / "GEIH_ptf_celdas_efectivas_ene-ago_2025_2026.csv")
t, det = trabajo(ene_ago, 2025, 2026, detalle=True)
print(det.to_string())
det.to_csv(T / "geih2026_trabajo_por_rama.csv")
t["desv_horas_por_ocupado_pp"] = t["dlnHoras_%"] - t["dlnOcupados_%"]
out = pd.DataFrame([t]).round(3)
out.to_csv(T / "geih2026_trabajo_ene_ago.csv", index=False)
print("== enero-agosto 2026 frente a enero-agosto 2025"); print(out.T.to_string(header=False))

# ---- ocupados por mes
m = pd.read_csv(PD / "GEIH_ocupados_horas_mensual_2025_2026.csv")
m["MES"] = m.MES.astype(int)
a = m[m.anio == 2025].set_index("MES"); b = m[m.anio == 2026].set_index("MES")
mm = pd.DataFrame({"ocupados_2025_miles": a.ocupados / 1e3, "ocupados_2026_miles": b.ocupados / 1e3}).dropna()
mm["var_ocupados_%"] = 100 * (mm.ocupados_2026_miles / mm.ocupados_2025_miles - 1)
mm["var_horas_P6850_%"] = 100 * (b.h6850 / a.h6850 - 1)
mm["var_horas_por_ocupado_%"] = 100 * ((b.h6850 / b.ocupados) / (a.h6850 / a.ocupados) - 1)
mm["var_segundo_empleo_horas_%"] = 100 * (b.h7045 / a.h7045 - 1)
mm = mm.round(2)
mm.to_csv(T / "geih2026_ocupados_mensual.csv")
print("== mensual"); print(mm.to_string())
