"""Diagnostico por actividad: compara el crecimiento de horas y la composicion del trabajo implicitos en el DANE
(Cuadros 4-23, enfoque de produccion) con los de la replica v2.

Del anexo: aporte(pp) = peso_L,prod x dln(factor). El peso de la mano de obra en la produccion es
LAB / Produccion corriente (promedio de t y t-1), con LAB = H_EMP / H_EMPE x remuneracion (COU).
"""
import importlib.util
from pathlib import Path

import numpy as np
import pandas as pd

ROOT = Path(__file__).resolve().parents[1]
PTF_DIR = ROOT / "Datos" / "Processed"
spec = importlib.util.spec_from_file_location("t8", Path(__file__).with_name("08_PTF_pruebas_definiciones.py"))
t8 = importlib.util.module_from_spec(spec); spec.loader.exec_module(t8)
v1 = t8.v1

dane = pd.read_csv(ROOT / "Outputs" / "tables" / "dane_ptf_por_actividad_2005_2024.csv")
cor = pd.read_csv(PTF_DIR / "COU_corrientes_rama.csv").pivot_table(index=["anio", "rama_ptf"], columns="concepto", values="valor").reset_index()
con = pd.read_csv(PTF_DIR / "COU_constantes_rama.csv").pivot_table(index=["anio", "rama_ptf"], columns="concepto", values="valor").reset_index()
cor = cor.rename(columns={"produccion": "y_cor", "valor_agregado": "va_cor", "remuneracion_asalariados": "comp"})
con = con.rename(columns={"produccion": "y_con", "valor_agregado": "va_con"})


def nuestro(c, educ="actual", rama="R4", asal=False):
    r, cel = t8.preparar(c, educ, rama, asal)
    p, _ = v1.cargar_panel()
    p = p.drop(columns=["H_EMP", "H_EMPE"]).merge(r[["anio", "rama_ptf", "H_EMP", "H_EMPE"]], on=["anio", "rama_ptf"], how="left")
    H = cel.groupby(["anio", "rama_ptf"]).horas_pond.sum().unstack()
    filas = []
    for a1 in t8.PERIODOS:
        a0 = a1 - 1
        dL = v1.indice_L(cel, a0, a1)
        dH = np.log(H.loc[a1]) - np.log(H.loc[a0])
        w = {}
        for a in (a0, a1):
            d = p[p.anio == a].set_index("rama_ptf")
            lab = (d.H_EMP / d.H_EMPE * d.comp).clip(upper=d.va_cor)
            w[a] = lab / cor[cor.anio == a].set_index("rama_ptf").y_cor
        wbar = (w[a0] + w[a1]) / 2
        for r_ in dL.index:
            filas.append({"anio": a1, "rama_ptf": r_, "dlnH": dH[r_], "dlnLC": dL[r_] - dH[r_], "w": wbar[r_]})
    return pd.DataFrame(filas)


def main():
    c = pd.read_csv(t8.CSV)
    n = nuestro(c)
    d = dane[dane.rama_ptf != "TOT_Economia"].merge(n, on=["anio", "rama_ptf"])
    d["dlnH_DANE"] = d.horas / 100 / d.w
    d["dlnLC_DANE"] = d.comp / 100 / d.w
    d["dlnL_DANE"] = d.dlnH_DANE + d.dlnLC_DANE
    d["dlnL_prop"] = d.dlnH + d.dlnLC
    pd.set_option("display.width", 250)
    res = []
    for lab, yrs in (("2015-2019", range(2015, 2020)), ("2022-2024", range(2022, 2025))):
        s = d[d.anio.isin(list(yrs))]
        for nombre, a, b in (("horas", "dlnH", "dlnH_DANE"), ("composicion", "dlnLC", "dlnLC_DANE"), ("total L", "dlnL_prop", "dlnL_DANE")):
            res.append({"periodo": lab, "componente": nombre, "corr": np.corrcoef(s[a], s[b])[0, 1], "MAE_pp": (s[a] - s[b]).abs().mean() * 100,
                        "sesgo_pp": (s[a] - s[b]).mean() * 100, "sd_DANE_pp": s[b].std() * 100, "sd_propia_pp": s[a].std() * 100})
    print(pd.DataFrame(res).round(2).to_string(index=False))
    por = []
    for rama, s in d[d.anio.isin(list(range(2015, 2020)))].groupby("rama_ptf"):
        por.append({"rama": rama, "MAE_horas": (s.dlnH - s.dlnH_DANE).abs().mean() * 100, "MAE_comp": (s.dlnLC - s.dlnLC_DANE).abs().mean() * 100,
                    "corr_horas": np.corrcoef(s.dlnH, s.dlnH_DANE)[0, 1], "corr_comp": np.corrcoef(s.dlnLC, s.dlnLC_DANE)[0, 1]})
    print(pd.DataFrame(por).round(2).to_string(index=False))
    d.to_csv(ROOT / "Outputs" / "tables" / "ptf_diagnostico_por_actividad.csv", index=False)
    print(d[d.anio.isin([2016, 2017, 2018, 2019])][["anio", "rama_ptf", "dlnH", "dlnH_DANE", "dlnLC", "dlnLC_DANE"]].assign(**{c: lambda x, c=c: (x[c] * 100).round(2) for c in ["dlnH", "dlnH_DANE", "dlnLC", "dlnLC_DANE"]}).to_string(index=False))


if __name__ == "__main__":
    main()
