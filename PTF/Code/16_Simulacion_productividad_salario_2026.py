"""Simulación informada de la productividad de 2026 (año corrido enero-septiembre) y del aumento del salario mínimo de 2027.

No replica la metodología del DANE (faltan los microdatos y las cuentas por sector de 2026). La PTF agregada se construye con DOS fuentes y se combina
según su precisión; después alimenta la regla del salario mínimo.

  (A) Desde los sectores (abajo hacia arriba). La PTF agregada de valor agregado del DANE es la suma de las PTF sectoriales ponderadas por
      producción bruta / valor agregado total (pesos de Domar): se verifica 2015-2024 (error < 0,15 pp). Con las 9 ramas del DANE (2005-2024) se estima la
      media y la covarianza de la PTF sectorial; un modelo con el crecimiento sectorial del valor agregado solo se usa si gana por validación cruzada
      (dejando un año fuera) en el agregado.
  (B) Desde las cuentas agregadas (arriba hacia abajo). Identidad del DANE con insumos de 2026:
        PTF = g_VA - s_L * g_horas - aporte_composicion - aporte_capital
        productividad por hora = g_VA - g_horas ;  por persona = g_VA - g_ocupados
      Su error se mide en una retrospectiva 2023-2025.
  (C) Combinación: actualización gaussiana. La media y covarianza sectorial (A) son el a priori; (B) es la medición del agregado. El resultado reparte la
      PTF agregada entre ramas y deja una PTF agregada con su incertidumbre.
  (D) Salario mínimo: piso = inflación esperada de cierre de 2026; tres reglas con pesos explícitos; la regla técnica usa la PTF de (C).

Entradas: PTF/Outputs/tables/*.csv, PTF/Datos/Processed/COU_corrientes_rama.csv, PTF/Datos/Raw/anex-PTF-ProductividadLaboral-2025.xlsx,
          Datos/Raw/PIB_DANE/anex-ProduccionConstantes-IItrim2026.xlsx
Salidas : PTF/Outputs/tables/sim2026_*.csv, PTF/Outputs/Figures/fig_sim2026_productividad_salario.png
Uso     : python PTF/Code/16_Simulacion_productividad_salario_2026.py   (desde la raíz del repositorio)
"""
from pathlib import Path

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
from matplotlib import font_manager

ROOT = Path(__file__).resolve().parents[2]
T = ROOT / "PTF" / "Outputs" / "tables"
N = 200_000
rng = np.random.default_rng(2026)
# ------------------------------------------------------------------------------------- DATOS QUE SE ACTUALIZAN
# Cuando salga un dato nuevo se cambia aquí y se vuelve a correr el script (desv. 0 = dato ya publicado).
ISE_ACUM_ENE_AGO = 2.27                     # ISE acumulado enero-agosto de 2026, % (DANE, 19-oct-2026)
ISE_SEP = (1.0, 1.2)                        # ISE de septiembre (media, desv.): sale a mediados de noviembre
# Trabajo enero-agosto: medido con los microdatos de la GEIH 2026 (scripts 06, 06c y 18; GEIH_2026_total.dta). Septiembre se simula; cuando salga la GEIH
# de septiembre se agrega a la base, se corren los scripts 06, 06c y 18 con MAXMES = 9 y estas tres líneas se ponen en (valor, 0).
OCUPADOS_SEP = (-0.5, 1.2)                  # variación anual de ocupados de septiembre (media, desv.), %
HORAS_SEP = (-3.0, 2.5)                     # variación anual de las horas efectivas de septiembre (media, desv.), %: julio -3,1 y agosto -4,9
L_SEP_DESV = 1.0                            # desviación del aporte del trabajo de septiembre alrededor del de enero-agosto, pp
PROB_RUPTURA_EDUC = 0.75                    # probabilidad de que la composición laboral que publique el DANE incluya el salto de la educación de 2026 (juicio; ver Docs)
COMP_SIN_RUPTURA = (0.25, 0.30)             # composición laboral (aporte a la PTF, pp) si el salto no entra: media y desv.; el DANE publicó -0,20, 0,10 y 0,35 en enero-septiembre de 2025, 2024 y 2023
HORAS_SESGO = (0.5, 1.0)                    # las horas de la GEIH crecen más que las implícitas del DANE: +0,5 (2025) y +0,6 (2024) pp, +2,4 pp en 2023 (datos revisados)
ERROR_L = 0.30                              # error del aporte del trabajo propio frente al DANE (retrospectiva enero-septiembre 2023-2025: 0,89; 0,17; -0,09)
PROB_PROD_HORA = 0.25                       # probabilidad de que la regla técnica use la productividad por hora en vez de la PTF (juicio: en 2026 el decreto citó las dos)
IPC_ACUM_ENE_SEP = 5.74                     # inflación acumulada enero-septiembre de 2026, %
IPC_Q4 = (1.07, 0.20)                       # inflación de octubre-diciembre, % (media, desv.): consenso de analistas de 6,87% a diciembre (rango 6,74-6,93)
FBCF_Q3 = (5.0, 3.0)                        # crecimiento anual de la FBCF del III trimestre (media, desv.): PIB del III trimestre sale a mediados de noviembre
EXCL = [2020, 2021]                       # años de la pandemia: se excluyen de las estimaciones
RAMAS = ["A-B_AgriculturaPesca", "C_Mineria", "D_Manufactura", "E_ElectricidadGasAgua", "F_Construccion", "G-H_ComercioHotelesRest",
         "I_TransporteComunicaciones", "J-K_FinancieroInmobiliario", "L-Q_ServiciosSocialesComunales"]
NOM = {"A-B_AgriculturaPesca": "Agricultura y pesca", "C_Mineria": "Minería", "D_Manufactura": "Industria", "E_ElectricidadGasAgua": "Electricidad y agua",
       "F_Construccion": "Construcción", "G-H_ComercioHotelesRest": "Comercio, hoteles y restaurantes", "I_TransporteComunicaciones": "Transporte y comunicaciones",
       "J-K_FinancieroInmobiliario": "Financiero e inmobiliario", "L-Q_ServiciosSocialesComunales": "Servicios sociales y comunales"}

# ======================================================================================= datos históricos
ser = pd.read_csv(T / "ptf_serie_empalmada_2005_2025.csv")
ser["anio"] = ser["anio"].astype(int)
ser = ser.set_index("anio")
cu1 = pd.read_excel(ROOT / "PTF/Datos/Raw/anex-PTF-ProductividadLaboral-2025.xlsx", sheet_name="Cuadro 1", header=None).iloc[14:35, 1:6]
cu1.columns = ["anio", "lp_hora", "ptf", "composicion", "capital"]
cu1["anio"] = cu1["anio"].astype(str).str.extract(r"(\d{4})")[0].astype(int)
cu1 = cu1.astype(float)
sect = pd.read_csv(T / "dane_ptf_por_actividad_2005_2024.csv")
sect = sect[sect.rama_ptf != "TOT_Economia"]
PTFs = sect.pivot(index="anio", columns="rama_ptf", values="ptf")[RAMAS]

# pesos de Domar: producción bruta de la rama / valor agregado total (precios corrientes)
cc = pd.read_csv(ROOT / "PTF/Datos/Processed/COU_corrientes_rama.csv")
cc = cc.pivot_table(index=["anio", "rama_ptf"], columns="concepto", values="valor").reset_index()
va_tot = cc[cc.rama_ptf == "TOT_Economia"].set_index("anio").valor_agregado
cc = cc[cc.rama_ptf != "TOT_Economia"]
cc["w"] = cc.produccion / cc.anio.map(va_tot)
W = cc.pivot(index="anio", columns="rama_ptf", values="w")[RAMAS]
w = W.loc[2024].values

# ---- (A.1) verificación: suma de Domar vs PTF agregada del DANE
filas = []
for t in range(2015, 2025):
    wt = 0.5 * (W.loc[t] + W.loc[t - 1])
    filas.append((t, (wt * PTFs.loc[t]).sum(), ser.PTF[t]))
val = pd.DataFrame(filas, columns=["anio", "suma_sectorial_ponderada", "PTF_agregada_DANE"])
val["diferencia"] = val.suma_sectorial_ponderada - val.PTF_agregada_DANE
val.round(3).to_csv(T / "sim2026_validacion_domar.csv", index=False, encoding="utf-8-sig")
sd_domar = val.diferencia.std(ddof=1)

# ======================================================================================= PIB por rama (trimestral, 2005-I a 2026-II)
d = pd.read_excel(ROOT / "Datos/Raw/PIB_DANE/anex-ProduccionConstantes-IItrim2026.xlsx", sheet_name="Cuadro 2", header=None)
filas_pib = {"A-B_AgriculturaPesca": [14], "C_Mineria": [16], "D_Manufactura": [18], "E_ElectricidadGasAgua": [25], "F_Construccion": [28],
             "G-H_ComercioHotelesRest": [33, 35], "I_TransporteComunicaciones": [34, 36], "J-K_FinancieroInmobiliario": [38, 40, 42],
             "L-Q_ServiciosSocialesComunales": [44, 48]}
qs = [(2005 + i // 4, i % 4 + 1) for i in range(86)]
Q = pd.DataFrame({k: sum(d.iloc[r, 4:90].astype(float).values for r in rr) for k, rr in filas_pib.items()},
                 index=pd.MultiIndex.from_tuples(qs, names=["anio", "q"]))
ann = Q.groupby("anio").sum().loc[:2025]
GVA = 100 * (ann / ann.shift(1) - 1)                              # crecimiento anual del VA por rama
yoy = 100 * (Q / Q.shift(4) - 1)

# ======================================================================================= (A.2) prior sectorial y validación cruzada
yrs = [y for y in range(2011, 2025) if y not in EXCL]               # muestra de estimación: desde 2011 (después de la ruptura de 2006-2010)
yrs_cv = [y for y in range(2006, 2025) if y not in EXCL]


def fit_ols(tr, r):
    x, y = GVA.loc[tr, r], PTFs.loc[tr, r]
    b = ((x - x.mean()) * (y - y.mean())).sum() / ((x - x.mean()) ** 2).sum()
    return y.mean() - b * x.mean(), b


rows, e_mean, e_slope = [], [], []
for t in [y for y in yrs_cv if y >= 2011]:
    tr = [y for y in yrs_cv if y != t]
    pm = ps = act = 0
    for i, r in enumerate(RAMAS):
        a0, b = fit_ols(tr, r)
        pm += w[i] * PTFs.loc[tr, r].mean()
        ps += w[i] * (a0 + b * GVA.loc[t, r])
        act += w[i] * PTFs.loc[t, r]
    e_mean.append(pm - act); e_slope.append(ps - act)
rmse_mean, rmse_slope = np.sqrt(np.mean(np.square(e_mean))), np.sqrt(np.mean(np.square(e_slope)))
cv_sec = []
for r in RAMAS:
    a_, b_ = [], []
    for t in [y for y in yrs_cv if y >= 2011]:
        tr = [y for y in yrs_cv if y != t]
        a0, b = fit_ols(tr, r)
        a_.append(PTFs.loc[tr, r].mean() - PTFs.loc[t, r]); b_.append(a0 + b * GVA.loc[t, r] - PTFs.loc[t, r])
    cv_sec.append((NOM[r], np.sqrt(np.mean(np.square(a_))), np.sqrt(np.mean(np.square(b_)))))
cv = pd.DataFrame(cv_sec, columns=["rama", "RMSE_promedio_historico", "RMSE_con_crecimiento_VA_sectorial"])
cv["gana_modelo_con_VA"] = cv.RMSE_con_crecimiento_VA_sectorial < cv.RMSE_promedio_historico
cv.loc[len(cv)] = ["AGREGADO (suma ponderada)", rmse_mean, rmse_slope, rmse_slope < rmse_mean]
cv.round(3).to_csv(T / "sim2026_cv_modelos_sectoriales.csv", index=False, encoding="utf-8-sig")
USAR_PENDIENTE = rmse_slope < rmse_mean                              # criterio: error fuera de muestra del agregado
mu = PTFs.loc[yrs].mean().values
dev = PTFs.loc[yrs].values - mu
S_raw = np.cov(dev.T, ddof=1)
lam = 0.3                                                             # contracción hacia la diagonal (12 años, 9 ramas)
Sig = (1 - lam) * S_raw + lam * np.diag(np.diag(S_raw))
var_prior_agg = w @ Sig @ w

# ======================================================================================= nowcast sectorial del VA (enero-septiembre de 2026)
share25 = (ann.loc[2025] / ann.loc[2025].sum()).values
g_q1 = yoy.loc[(2026, 1)].values
g_q2 = yoy.loc[(2026, 2)].values                                  # II trimestre de 2026 observado por rama (anexo del 18-ago-2026)
# Q3: señales del ISE (jul y ago). Primarias: agricultura y minería; secundarias: industria y construcción; terciarias: el resto
prim = [RAMAS.index(k) for k in ("A-B_AgriculturaPesca", "C_Mineria")]
secu = [RAMAS.index(k) for k in ("D_Manufactura", "F_Construccion")]
terc = [i for i in range(9) if i not in prim + secu]
sh_q3 = (Q.loc[(2025, 3)] / Q.loc[(2025, 3)].sum()).values
sp, ss_, st = sh_q3[prim].sum(), sh_q3[secu].sum(), sh_q3[terc].sum()
t_ago = (-0.04 - sp * (-3.19) - ss_ * (-3.03)) / st                  # terciarias de agosto, despejadas del total del ISE (-0,04%)
g_q3 = np.zeros(9)
g_q3[prim], g_q3[secu], g_q3[terc] = (-3.72 - 3.19) / 2, (-2.29 - 3.03) / 2, (2.59 + t_ago) / 2
nowc = pd.DataFrame({"rama": [NOM[r] for r in RAMAS], "I_trim_2026_observado": g_q1, "II_trim_2026_observado": g_q2, "III_trim_2026_estimado_jul_ago": g_q3})
nowc["ene_sep_2026"] = nowc[["I_trim_2026_observado", "II_trim_2026_observado", "III_trim_2026_estimado_jul_ago"]].mean(axis=1)
nowc["peso_VA_2025_%"] = 100 * share25
va_bu = float(share25 @ nowc["ene_sep_2026"].values)
nowc.round(2).to_csv(T / "sim2026_nowcast_VA_sectorial.csv", index=False, encoding="utf-8-sig")
g_sec26 = nowc["ene_sep_2026"].values

# prior sectorial para 2026: media histórica, o recta con el crecimiento sectorial si gana en el agregado
mu26 = mu.copy()
if USAR_PENDIENTE:
    for i, r in enumerate(RAMAS):
        a0, b = fit_ols(yrs_cv, r)
        mu26[i] = a0 + b * g_sec26[i]

# ======================================================================================= (B) identidad desde las cuentas agregadas
k = ser["K"]
# capital: inventario perpetuo. El stock productivo crece casi uno a uno con la FBCF / stock del año anterior (gK = a + b * FBCF/K(t-1));
# el aporte del DANE es gK por una razón (promedio de los últimos 3 años sin pandemia).
gasto = pd.read_excel(ROOT / "Datos/Raw/PIB_DANE/anex-GastoConstantes-IItrim2026.xlsx", sheet_name="Cuadro 1", header=None)
qs_g = [(2005 + i // 4, i % 4 + 1) for i in range(gasto.shape[1] - 2)]
Ig = pd.Series(gasto.iloc[17, 2:2 + len(qs_g)].astype(float).values, index=pd.MultiIndex.from_tuples(qs_g))   # fila 17 = FBCF
I_anual = Ig.groupby(level=0).sum().loc[:2025]
g_I_sem = [100 * (Ig[(2026, qq)] / Ig[(2025, qq)] - 1) for qq in (1, 2)]                                    # FBCF I y II trimestre de 2026
st = pd.read_excel(ROOT / "PTF/Datos/Raw/anex-PTF-StockCapital-acervos-2024.xlsx", sheet_name="Cuadro 3", header=None)
yrs_st = [int(float(str(v).replace("p", ""))) for v in st.iloc[10, 1:36].tolist() if str(v) != "nan"]
Kst = pd.Series(st.iloc[21, 1:1 + len(yrs_st)].astype(float).values, index=yrs_st)                          # fila 21 = total nacional
gK = 100 * (Kst / Kst.shift(1) - 1)
dfk = pd.DataFrame({"gK": gK, "I": I_anual, "Klag": Kst.shift(1), "Kcon": k}).dropna(subset=["gK", "I", "Klag"])
dfk["IK"] = 100 * dfk.I / dfk.Klag
dfk["razon"] = dfk.Kcon / dfk.gK


def kmodelo(excluir=()):
    tr = dfk.drop(list(excluir), errors="ignore")
    return np.linalg.lstsq(np.column_stack([np.ones(len(tr)), tr.IK.values]), tr.gK.values, rcond=None)[0]


def razon_prev(t):
    prev = [y for y in (t - 4, t - 3, t - 2, t - 1) if y in dfk.index and y not in EXCL][-3:]
    return float(np.mean([dfk.razon[y] for y in prev]))


kbt = []
for t in [y for y in range(2016, 2026) if y not in EXCL]:
    ik = (100 * I_anual[t] / Kst[t - 1])
    bb_ = kmodelo([t, 2025]) if t in dfk.index else kmodelo()
    kp = razon_prev(t) * (bb_[0] + bb_[1] * ik)
    pares = [(k[a - 1], k[a]) for a in range(2008, t) if a not in EXCL and a - 1 not in EXCL]
    xx_, yy_ = np.array(pares).T
    b1, a1 = np.polyfit(xx_, yy_, 1)
    kbt.append((t, k[t], kp, a1 + b1 * k[t - 1]))
kbt = pd.DataFrame(kbt, columns=["anio", "aporte_capital_DANE", "inventario_perpetuo", "AR1"])
rmse_kinv = np.sqrt(((kbt.inventario_perpetuo - kbt.aporte_capital_DANE) ** 2).mean())
rmse_kar = np.sqrt(((kbt.AR1 - kbt.aporte_capital_DANE) ** 2).mean())
kbt.round(3).to_csv(T / "sim2026_capital_retrospectiva.csv", index=False, encoding="utf-8-sig")
bK = kmodelo()
rmse_gK = float(np.sqrt(np.mean([(bK[0] + bK[1] * dfk.IK[t] - dfk.gK[t]) ** 2 for t in dfk.index])))
K25 = Kst[2024] * (1 + (bK[0] + bK[1] * 100 * I_anual[2025] / Kst[2024]) / 100)
razon26 = razon_prev(2026)
k_media = razon26 * (bK[0] + bK[1] * 100 * I_anual[2025] * (1 + np.mean(g_I_sem + [FBCF_Q3[0]]) / 100) / K25)
sd_k = rmse_kinv                                                    # error del aporte del capital fuera de muestra (retrospectiva 2016-2025)
comp = cu1[(cu1.anio >= 2018) & (cu1.anio != 2020)]["composicion"]
comp_mu, comp_sd = 0.0, max(comp.std(), 0.20)
sL = 1.114 / 2.25
dh = np.array([6.30 - 7.83, 2.08 - 3.40, -0.44 - 1.09, 2.60 - 3.38])   # horas menos ocupados, 2022-2025 (réplica propia)
dh_mu, dh_sd = dh.mean(), max(dh.std(ddof=1), 0.45)

# retrospectiva 2023-2025: la misma receta con el VA y los ocupados observados, horas/ocupado y capital estimados solo con años anteriores
oc_h = {2022: 7.83, 2023: 3.40, 2024: 1.09, 2025: 3.38}; hr_h = {2022: 6.30, 2023: 2.08, 2024: -0.44, 2025: 2.60}
ct_h = {2022: 4.218, 2023: 1.225, 2024: -0.214, 2025: 1.29}
bt = []
for t in (2023, 2024, 2025):
    prev = [yv for yv in oc_h if yv < t]
    dh_p = np.mean([hr_h[yv] - oc_h[yv] for yv in prev]); sl_p = np.mean([ct_h[yv] / hr_h[yv] for yv in prev])
    pred = ser.VA[t] - sl_p * (oc_h[t] + dh_p) - kbt.set_index("anio").inventario_perpetuo[t]
    bt.append((t, pred, ser.PTF[t], pred - ser.PTF[t]))
bt = pd.DataFrame(bt, columns=["anio", "PTF_prevista", "PTF_DANE", "error"])
rmse_td = np.sqrt((bt.error ** 2).mean())
bt.round(3).to_csv(T / "sim2026_retrospectiva_identidad.csv", index=False, encoding="utf-8-sig")

ise_sep = rng.normal(ISE_SEP[0], ISE_SEP[1], N)
g_va = (8 * ISE_ACUM_ENE_AGO + ise_sep) / 9 + rng.normal(-0.05, 0.15, N)
# trabajo medido con la GEIH 2026 (enero-agosto frente a enero-agosto de 2025)
gtr = pd.read_csv(T / "geih2026_trabajo_ene_ago.csv").iloc[0]
mens = pd.read_csv(T / "geih2026_ocupados_mensual.csv")
g_oc = (mens["var_ocupados_%"].sum() + rng.normal(OCUPADOS_SEP[0], OCUPADOS_SEP[1], N)) / 9
sesgo_h = rng.normal(HORAS_SESGO[0], HORAS_SESGO[1], N)                    # horas propias menos horas del DANE (pp)
g_h = (8 * gtr["dlnHoras_%"] + rng.normal(HORAS_SEP[0], HORAS_SEP[1], N)) / 9 - sesgo_h + rng.normal(0, 0.30, N)
h_c = (8 * gtr["aporte_horas_pp"] + rng.normal(gtr["aporte_horas_pp"], L_SEP_DESV, N)) / 9 - 0.5 * sesgo_h      # aporte de las horas (s_L ~ 0,5)
comp_A = (8 * gtr["aporte_composicion_pp"] + rng.normal(gtr["aporte_composicion_pp"], L_SEP_DESV, N)) / 9 + rng.normal(0, 0.19, N)   # medida: error de muestreo 0,19
comp_B = rng.normal(COMP_SIN_RUPTURA[0], COMP_SIN_RUPTURA[1], N)
rup = rng.random(N) < PROB_RUPTURA_EDUC                                    # el DANE publica la composición con el salto de la educación
e_L = rng.normal(0, ERROR_L, N)
g_I = (g_I_sem[0] + g_I_sem[1] + rng.normal(FBCF_Q3[0], FBCF_Q3[1], N)) / 3
gK26 = bK[0] + bK[1] * 100 * I_anual[2025] * (1 + g_I / 100) / K25 + rng.normal(0, rmse_gK, N)
ap_k = razon26 * gK26 + rng.normal(0, sd_k, N)
lp_hora = g_va - g_h
lp_pers = g_va - g_oc
ptf_td_A = g_va - (h_c + comp_A + e_L) - ap_k                              # medición con la composición medida (incluye el salto)
ptf_td_B = g_va - (h_c + comp_B + e_L) - ap_k                              # medición con una composición típica
ptf_td = np.where(rup, ptf_td_A, ptf_td_B)
retro = T / "geih_retrospectiva_ene_sep_2023_2025.csv"
if retro.exists():                                                          # error de la receta del trabajo frente al DANE en enero-septiembre 2023-2025 (script 19)
    rmse_td = float(np.sqrt((pd.read_csv(retro)["dif_PTF"] ** 2).mean()))

# ======================================================================================= (C) combinación gaussiana
m_B = w @ mu26
Sw = Sig @ w
Lchol = None


def combinar(z):
    """Actualización gaussiana del a priori sectorial con una medición agregada z (draws): devuelve la PTF agregada y los aportes por rama."""
    m_z = z.mean()
    s_z = max(z.std(), rmse_td)                                             # el mayor entre la propagación de insumos y el error de la retrospectiva
    Kg = Sw / (w @ Sw + s_z ** 2 + sd_domar ** 2)
    mu_p = mu26 + Kg * (m_z - m_B)
    Sp = Sig - np.outer(Kg, Sw)
    Lc = np.linalg.cholesky((Sp + Sp.T) / 2 + 1e-9 * np.eye(9))
    xp = mu_p + rng.standard_normal((N, 9)) @ Lc.T
    return xp @ w + rng.normal(0, sd_domar, N), xp, m_z, s_z


ptf_A, xA, mA_, sA_ = combinar(ptf_td_A)
ptf_B, xB, mB_, sB_ = combinar(ptf_td_B)
m_A, s_A = ptf_td.mean(), max(ptf_td.std(), rmse_td)
ptf = np.where(rup, ptf_A, ptf_B)
x_post = np.where(rup[:, None], xA, xB)
rho = float(np.corrcoef(ptf_td, lp_hora)[0, 1])
u_ = (ptf_td - ptf_td.mean()) / ptf_td.std()
score = rho * u_ + np.sqrt(1 - rho ** 2) * rng.standard_normal(N)
rango = np.argsort(np.argsort(score))
orden_ = np.argsort(ptf)
ptf, x_post, rup_o = ptf[orden_][rango], x_post[orden_][rango], rup[orden_][rango]   # la PTF y los aportes por rama quedan correlacionados con la productividad por hora
sec_t = pd.DataFrame({"rama": [NOM[r] for r in RAMAS], "peso_Domar_2024": w, "VA_ene_sep_2026_%": g_sec26,
                      "PTF_apriori_(promedio_2011-2024)": mu26, "PTF_2026_media": x_post.mean(0), "p05": np.percentile(x_post, 5, axis=0),
                      "p95": np.percentile(x_post, 95, axis=0)})
sec_t["aporte_a_PTF_agregada_pp"] = sec_t.peso_Domar_2024 * sec_t["PTF_2026_media"]
sec_t.round(3).to_csv(T / "sim2026_sectores_PTF_2026.csv", index=False, encoding="utf-8-sig")
comb = pd.DataFrame([("A) Desde sectores (a priori)", m_B, np.sqrt(var_prior_agg), rmse_mean),
                     ("B) Identidad agregada con insumos de 2026 (mezcla de escenarios)", m_A, s_A, rmse_td),
                     ("B1) ... si el DANE incluye el salto de la educación (composición +1,15)", mA_, sA_, rmse_td),
                     ("B2) ... si la composición es típica (0,25 ± 0,30)", mB_, sB_, rmse_td),
                     ("C) Combinación (PTF usada)", ptf.mean(), ptf.std(), np.nan),
                     ("C1) PTF si el DANE incluye el salto", ptf_A.mean(), ptf_A.std(), np.nan),
                     ("C2) PTF si la composición es típica", ptf_B.mean(), ptf_B.std(), np.nan)],
                    columns=["fuente", "media_PTF_2026", "desv_est", "error_retrospectivo_RMSE"]).round(3)
comb.to_csv(T / "sim2026_combinacion_PTF.csv", index=False, encoding="utf-8-sig")

# ======================================================================================= (D) inflación y salario mínimo
pi_cierre = ((1 + IPC_ACUM_ENE_SEP / 100) * (1 + rng.normal(IPC_Q4[0], IPC_Q4[1], N) / 100) - 1) * 100
pi_ref = pi_cierre + rng.normal(0, 0.10, N)
prima_hist = np.array([2.56, 0.94, 0.23, 1.25, 1.81, 2.82, 2.20, 1.89])     # fijados para 2014-2021 (de memoria: verificar)
prima_act = np.array([4.45, 2.88, 2.79, 4.30, 17.70])                       # fijados para 2022-2026


def salario(pesos):
    u = rng.random(N)
    r = np.where(u < pesos[0], 0, np.where(u < pesos[0] + pesos[1], 1, 2))
    extra = rng.uniform(0, 1.0, N)
    prod = np.where(rng.random(N) < PROB_PROD_HORA, lp_hora, ptf)          # medida de productividad que entra en la regla técnica
    dd = np.where(r == 0, pi_ref + np.maximum(prod, 0) + extra,
                  np.where(r == 1, pi_ref + rng.choice(prima_hist, N) + rng.normal(0, 0.4, N),
                           pi_ref + rng.choice(prima_act, N) + rng.normal(0, 0.8, N)))
    return np.maximum(dd, pi_ref), r


BASE = "Base (nuevo gobierno, reglas técnica/historica/activista 45/40/15)"
BASE_NOM = BASE
PESOS = {BASE: (0.45, 0.40, 0.15), "Gremial (80/15/5)": (0.80, 0.15, 0.05), "Sindical-activista (10/30/60)": (0.10, 0.30, 0.60)}
sal = {nom: salario(p)[0] for nom, p in PESOS.items()}
# sensibilidad al supuesto de qué productividad usa la regla técnica (0% = solo PTF, 100% = solo productividad por hora)
_pp = PROB_PROD_HORA
sens_r = []
for pr in (0.0, 0.25, 0.5, 1.0):
    PROB_PROD_HORA = pr
    v_ = salario(PESOS[BASE_NOM])[0]
    sens_r.append((pr, np.median(v_), v_.mean(), np.percentile(v_, 5), np.percentile(v_, 95), 100 * (v_ > 8).mean(), 100 * (v_ > 10).mean()))
PROB_PROD_HORA = _pp

# sensibilidad a la probabilidad de que el DANE incluya el salto de la educación en la composición
_ptf0 = ptf.copy()
sens_e = []
for pe in (0.0, 0.25, 0.5, 0.75, 1.0):
    ptf = np.where(rng.random(N) < pe, ptf_A, ptf_B)
    v_ = salario(PESOS[BASE_NOM])[0]
    sens_e.append((pe, np.median(ptf), np.percentile(ptf, 5), np.percentile(ptf, 95), np.median(v_), v_.mean(), 100 * (v_ > 8).mean(), 100 * (v_ > 10).mean()))
ptf = _ptf0
pd.DataFrame(sens_e, columns=["prob_salto_educacion_en_DANE", "PTF_mediana", "PTF_p05", "PTF_p95", "salario_mediana", "salario_media", "prob_mayor_8_%", "prob_mayor_10_%"]).round(2).to_csv(
    T / "sim2026_sensibilidad_ruptura_educacion.csv", index=False, encoding="utf-8-sig")
pd.DataFrame(sens_r, columns=["prob_productividad_por_hora", "mediana", "media", "p05", "p95", "prob_mayor_8_%", "prob_mayor_10_%"]).round(2).to_csv(
    T / "sim2026_sensibilidad_regla_tecnica.csv", index=False, encoding="utf-8-sig")


def q(v):
    return pd.Series({"media": v.mean(), "p05": np.percentile(v, 5), "p25": np.percentile(v, 25), "mediana": np.median(v),
                      "p75": np.percentile(v, 75), "p95": np.percentile(v, 95)})


out = pd.DataFrame({"Valor agregado (año corrido ene-sep)": g_va, "Ocupados": g_oc, "Horas trabajadas": g_h,
                    "Productividad laboral por hora": lp_hora, "Productividad laboral por persona": lp_pers, "PTF": ptf,
                    "PTF (solo identidad agregada)": ptf_td, "Inflación de cierre 2026": pi_cierre}).apply(q).T.round(2)
for nom, v in sal.items():
    out.loc["Aumento salario mínimo 2027 - " + nom] = q(v).round(2)
out.index.name = "variable"
out.to_csv(T / "sim2026_resumen.csv", encoding="utf-8-sig")

ins = pd.DataFrame([
    ("Pesos de Domar", "Producción bruta / valor agregado total de cada rama en 2024 (suman %.2f)" % w.sum(), "COU corrientes (DANE)"),
    ("PTF sectorial a priori", f"Promedio 2011-2024 sin 2020-2021 por rama; covarianza con contracción {lam}; modelo con crecimiento sectorial {'ACTIVADO' if USAR_PENDIENTE else 'DESCARTADO por validación cruzada'}", "DANE PTF por actividad (producción), 2005-2024"),
    ("Valor agregado total", f"ISE enero-agosto acumulado {ISE_ACUM_ENE_AGO}%; septiembre ~ N{ISE_SEP}; VA = ISE - 0,05 (± 0,15)", "DANE ISE (19-oct-2026)"),
    ("VA sectorial", "I y II trim. observados por rama (anexo del 18-ago-2026); III trim. con las señales del ISE de julio y agosto por grupo", "DANE PIB; ISE"),
    ("Ocupados", f"GEIH 2026 enero-agosto medida mes a mes (variación anual media {mens['var_ocupados_%'].mean():.2f}%); sep ~ N{OCUPADOS_SEP}", "GEIH_2026_total.dta (script 06c, 18)"),
    ("Horas efectivas", f"GEIH 2026 enero-agosto vs 2025: {gtr['dlnHoras_%']:.2f}% (horas por ocupado {gtr['desv_horas_por_ocupado_pp']:.2f}%); sep ~ N{HORAS_SEP}; se resta un sesgo ~ N{HORAS_SESGO} (las horas de la GEIH crecen más que las implícitas del DANE)", "GEIH_2026_total.dta (scripts 06, 18, 19)"),
    ("Aporte del trabajo", f"GEIH 2026 enero-agosto: horas {gtr['aporte_horas_pp']:.2f} pp; composición medida {gtr['aporte_composicion_pp']:.2f} pp (error de muestreo 0,19); con probabilidad {PROB_RUPTURA_EDUC:.0%} el DANE publica esa composición y con {1 - PROB_RUPTURA_EDUC:.0%} una típica N{COMP_SIN_RUPTURA}; error propio frente al DANE {ERROR_L}", "Réplica de la PTF con la GEIH 2026; ruptura de la educación (script 20)"),
    ("Medición del agregado", f"desv. = max(propagación {ptf_td.std():.2f}; error retrospectivo {rmse_td:.2f}) y error de Domar {sd_domar:.2f}", "Retrospectiva 2023-2025"),
    ("Inflación de cierre", f"(1+{IPC_ACUM_ENE_SEP}% acumulada a septiembre) x (1+N{IPC_Q4}%) - 1; consenso de analistas 6,87% a diciembre (rango 6,74-6,93); desv. de la expectativa 0,10", "DANE IPC; encuesta de analistas (Infobae, 21-oct-2026)"),
    ("Regla del salario", f"Piso = inflación esperada. R1 = inflación + max(productividad,0) + U(0,1), con la productividad por hora en {PROB_PROD_HORA:.0%} de los casos y la PTF en el resto; R2 = inflación + prima 2014-2021; R3 = inflación + prima 2022-2026", "Decretos; prensa"),
], columns=["insumo", "supuesto", "fuente"])
ins.to_csv(T / "sim2026_insumos.csv", index=False, encoding="utf-8-sig")

consenso = pd.DataFrame([("Corficolombiana (técnica)", 8.0, 8.0), ("Banco de Bogotá (técnica)", 8.0, 8.0), ("Banco de Bogotá (probable, con política)", 8.5, 8.5),
                         ("Davivienda", 8.5, 8.5), ("Anif y Corficolombiana (rango)", 8.0, 9.5), ("Bancolombia (rango)", 8.5, 9.0), ("Credicorp Capital (rango)", 9.0, 10.0)],
                        columns=["fuente", "aumento_bajo_%", "aumento_alto_%"])
consenso["prob_simulada_de_quedar_por_debajo_del_bajo_%"] = [100 * (sal[BASE] < x).mean() for x in consenso["aumento_bajo_%"]]
consenso["prob_simulada_dentro_del_rango_%"] = [100 * ((sal[BASE] >= lo) & (sal[BASE] <= hi)).mean() if hi > lo else np.nan for lo, hi in zip(consenso["aumento_bajo_%"], consenso["aumento_alto_%"])]
consenso.round(1).to_csv(T / "sim2026_consenso_analistas.csv", index=False, encoding="utf-8-sig")
prob = pd.DataFrame({
    "evento": ["PTF > 0", "PTF > 1,2% (propuesta Corficolombiana)", "PTF > 0,91% (año corrido 2025pr)", "Prod. laboral por hora > 0,57% (2025pr)",
               "Prod. por persona > 0", "Aumento > 8%", "Aumento > 10%", "Aumento > 15%", "Inflación de cierre > 7%"],
    "probabilidad_%": [100 * (ptf > 0).mean(), 100 * (ptf > 1.2).mean(), 100 * (ptf > 0.91).mean(), 100 * (lp_hora > 0.57).mean(), 100 * (lp_pers > 0).mean(),
                       100 * (sal[BASE] > 8).mean(), 100 * (sal[BASE] > 10).mean(), 100 * (sal[BASE] > 15).mean(), 100 * (pi_cierre > 7).mean()]}).round(1)
prob.to_csv(T / "sim2026_probabilidades.csv", index=False, encoding="utf-8-sig")

# ======================================================================================= figura resumen
for fn in ("segoeui.ttf", "segoeuib.ttf"):
    p = Path(r"C:\Windows\Fonts") / fn
    if p.exists():
        font_manager.fontManager.addfont(str(p))
plt.rcParams["font.family"] = ["Segoe UI", "DejaVu Sans"]
INK, INK2, MUTED, GRID, AXIS = "#14202B", "#46535F", "#7A8791", "#E7EBEE", "#C9D1D6"
fig, (ax, ax2) = plt.subplots(1, 2, figsize=(13.5, 5.8), dpi=200, facecolor="white", gridspec_kw={"wspace": 0.22})
for aa_ in (ax, ax2):
    aa_.grid(axis="y", color=GRID, zorder=0)
    aa_.set_axisbelow(True)
    for sp in ("top", "right"):
        aa_.spines[sp].set_visible(False)
    for sp in ("left", "bottom"):
        aa_.spines[sp].set_color(AXIS)
    aa_.tick_params(length=0, labelsize=10.5, labelcolor=MUTED)
series = [("Productividad por hora", lp_hora, "#2A78D6", 0.57), ("Productividad por persona", lp_pers, "#9AA7B0", -0.56), ("PTF", ptf, "#EB6834", 0.91)]
for i, (nom, v, col, ref) in enumerate(series):
    p5, p25, p50, p75, p95 = np.percentile(v, [5, 25, 50, 75, 95])
    ax.plot([i, i], [p5, p95], color=col, lw=2, zorder=3)
    ax.bar(i, p75 - p25, 0.38, bottom=p25, color=col, alpha=0.85, zorder=4)
    ax.scatter([i], [p50], color="white", s=36, zorder=5)
    ax.text(i + 0.26, p50, f"{p50:.1f}".replace(".", ","), va="center", fontsize=11, color=INK, fontweight="bold")
    ax.scatter([i], [ref], marker="_", s=380, color=INK, linewidths=2, zorder=6)
ax.axhline(0, color=AXIS, lw=1.2)
ax.set_ylim(-1.3, 3.0)
ax.set_xticks(range(3))
ax.set_xticklabels([s[0] for s in series], fontsize=11, color=INK2)
ax.set_ylabel("Crecimiento enero-septiembre de 2026 (%)", fontsize=10.5, color=INK2)
ax.set_title("A. Productividad de 2026 (año corrido)", loc="left", fontsize=12, fontweight="bold", color=INK, pad=10)
ax.text(0.5, 0.03, "Caja: percentiles 25-75. Línea: 5-95. Marca negra: mismo\nperiodo de 2025 (DANE: 0,57; -0,56; 0,91).", transform=ax.transAxes, va="bottom", ha="center", fontsize=9.3, color=INK2)
bins = np.arange(5.5, 16.01, 0.25)
v = sal[BASE]
ax2.hist(v, bins=bins, color="#2A78D6", alpha=0.85, zorder=3, weights=np.ones(N) * 100 / N)
ax2.axvline(np.median(v), color=INK, lw=1.4)
ax2.set_xlim(5.5, 16)
ytop = ax2.get_ylim()[1]
ax2.text(np.median(v) + 0.15, ytop * 0.97, f"mediana {np.median(v):.1f}".replace(".", ",") + "%", fontsize=10, color=INK, fontweight="bold")
for xv in (np.percentile(pi_ref, 50), 8.0, 8.5):
    ax2.axvline(xv, color="#EB6834", lw=1.2, ls="--")
ax2.text(0.98, 0.9, "Líneas naranja:\npiso = inflación esperada (6,8)\nCorficolombiana 8,0 · Davivienda 8,5", transform=ax2.transAxes, ha="right", va="top", fontsize=9.3, color="#EB6834", linespacing=1.4)
ax2.text(0.98, 0.62, f"{100 * (v > 16).mean():.1f}% de las trayectorias\nsuperan 16%".replace(".", ","), transform=ax2.transAxes, ha="right", fontsize=9.3, color=INK2)
ax2.set_xlabel("Aumento del salario mínimo para 2027 (%)", fontsize=10.5, color=INK2)
ax2.set_ylabel("Probabilidad por intervalo de 0,25 pp (%)", fontsize=10.5, color=INK2)
ax2.set_title("B. Salario mínimo 2027 (piso: inflación esperada)", loc="left", fontsize=12, fontweight="bold", color=INK, pad=10)
fig.text(0.04, 0.975, "Simulación informada: productividad de 2026 y salario mínimo de 2027", fontsize=16, fontweight="bold", color=INK, va="top")
fig.text(0.04, 0.925, f"Mediana de la PTF {np.median(ptf):.1f}% y de la productividad por hora {np.median(lp_hora):.1f}%; inflación de cierre {np.median(pi_cierre):.1f}%.".replace(".", ","), fontsize=10.5, color=INK2, va="top")
fig.text(0.04, 0.012, "Fuente: DANE (ISE, GEIH, IPC, PTF por rama y 2025pr), prensa y cálculos propios. Simulación de 200.000 trayectorias; no reemplaza la cifra oficial del DANE (esperada a fines de noviembre).",
         fontsize=8.5, color=MUTED, va="bottom")
fig.subplots_adjust(top=0.82, bottom=0.12, left=0.07, right=0.98)
(ROOT / "PTF/Outputs/Figures").mkdir(parents=True, exist_ok=True)
fig.savefig(ROOT / "PTF/Outputs/Figures/fig_sim2026_productividad_salario.png", dpi=200, facecolor="white")

pd.set_option("display.width", 230)
print("Domar vs DANE (2015-2024): sd diferencia", round(sd_domar, 3), "| max", round(val.diferencia.abs().max(), 3))
print(cv.round(2).to_string()); print("usar pendiente:", USAR_PENDIENTE)
print("VA sectorial ene-sep (suma ponderada):", round(va_bu, 2), "| ISE-based:", round(g_va.mean(), 2)); print(nowc.round(2).to_string())
print(bt.round(2).to_string(), "RMSE identidad", round(rmse_td, 2))
print(comb.to_string()); print(sec_t.round(2).to_string())
print(out.to_string()); print(prob.to_string()); print(consenso.round(1).to_string())
print("capital: RMSE inventario", round(rmse_kinv, 3), "AR1", round(rmse_kar, 3), "| aporte 2026", round(k_media, 2), "| rho PTF-LP/h", round(float(np.corrcoef(ptf, lp_hora)[0, 1]), 2)); print(kbt.round(2).to_string())
