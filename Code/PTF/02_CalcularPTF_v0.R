# =========================================================
# PTF v0 (enfoque de valor agregado, K y L) - calculo del residual
#
# Implementa las formulas de la seccion 2 ("Marco metodologico - Modelo
# KLEMS", enfoque de Valor Agregado) del documento metodologico del DANE:
#
#   LAB_j   = (H_EMP_j / H_EMPE_j) * COMP_j                  (ajuste cuenta propia)
#   CAP_j   = V_j - LAB_j   (0 si el residual es negativo)
#   wL_j, wK_j = LAB_j/V_j, CAP_j/V_j                        (participaciones)
#   dln(L_j)  = suma_l vbar_l * dln(H_l)                     (Tornqvist, 18 categorias)
#   dln(K_j)  = dln(stock de capital productivo)             (v0: stock oficial DANE)
#   dln(T_j)  = dln(V_j) - wKbar_j*dln(K_j) - wLbar_j*dln(L_j)   <- residual PTF
#
# Nota (v0): K se toma directo del stock de capital productivo que el DANE
# ya publica por rama (no se reconstruye todavia desde FBKF por tipo de
# activo). Ver seccion 4 del documento para la version completa.
# =========================================================

source(if (file.exists("Code/_paths.R")) "Code/_paths.R" else "_paths.R")
setwd(PROJECT_ROOT)
ensure_project_dirs()

library(dplyr)
library(tidyr)
library(readr)

dir_ptf <- project_path("Datos", "Processed", "PTF")

panel_base <- readRDS(file.path(dir_ptf, "panel_base_rama_anio.rds"))
geih_celdas <- readRDS(file.path(dir_ptf, "geih_celdas_L.rds"))

RAMAS_9 <- setdiff(unique(panel_base$rama_ptf), "TOT_Economia")

# =========================
# 1. Indice de Servicios Laborales (L): Tornqvist sobre 18 categorias
#    sexo x grupo_edad x nivel educativo, por rama
# =========================

geih_shares <- geih_celdas %>%
  group_by(rama_ptf, anio) %>%
  mutate(total_ingreso_rama_anio = sum(ingreso_pond, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(v = ingreso_pond / total_ingreso_rama_anio)

L_index <- geih_shares %>%
  arrange(rama_ptf, categoria, anio) %>%
  group_by(rama_ptf, categoria) %>%
  mutate(
    anio_prev = lag(anio),
    horas_prev = lag(horas_pond),
    v_prev = lag(v)
  ) %>%
  ungroup() %>%
  filter(anio == anio_prev + 1, horas_pond > 0, horas_prev > 0) %>%
  mutate(
    v_bar = (v + v_prev) / 2,
    dln_H = log(horas_pond) - log(horas_prev)
  ) %>%
  group_by(rama_ptf, anio) %>%
  summarise(
    dln_L = sum(v_bar * dln_H),
    n_categorias_usadas = n(),
    .groups = "drop"
  )

# =========================
# 2. Pesos wL, wK y crecimientos de V y K, por rama
# =========================

panel_ramas <- panel_base %>%
  filter(rama_ptf %in% RAMAS_9) %>%
  arrange(rama_ptf, anio) %>%
  mutate(
    LAB = (H_EMP / H_EMPE) * comp_corriente,
    CAP_raw = va_corriente - LAB,
    CAP = pmax(CAP_raw, 0),
    LAB = if_else(CAP_raw < 0, va_corriente, LAB),
    wL = LAB / va_corriente,
    wK = CAP / va_corriente
  ) %>%
  group_by(rama_ptf) %>%
  mutate(
    wL_bar = (wL + lag(wL)) / 2,
    wK_bar = (wK + lag(wK)) / 2,
    # OJO: el COU "precios constantes" del DANE esta a precios del anio
    # ANTERIOR (no es una serie encadenada de base fija 2015), asi que el
    # crecimiento correcto es va_constante_t (a precios t-1) sobre
    # va_corriente_(t-1) (a precios corrientes t-1), no va_constante_t
    # sobre va_constante_(t-1) (que estarian en bases de precios distintas
    # y mezcladas). El stock de capital SI viene ya encadenado a base 2015
    # (lo dice el anexo), asi que ese si se compara consigo mismo.
    dln_V = log(va_constante) - log(lag(va_corriente)),
    dln_K = log(stock_capital_productivo) - log(lag(stock_capital_productivo))
  ) %>%
  ungroup() %>%
  left_join(L_index, by = c("rama_ptf", "anio"))

ptf_ramas <- panel_ramas %>%
  filter(!is.na(dln_V), !is.na(dln_K), !is.na(dln_L)) %>%
  mutate(
    aporte_L = wL_bar * dln_L,
    aporte_K = wK_bar * dln_K,
    dln_T = dln_V - aporte_K - aporte_L
  ) %>%
  select(anio, rama_ptf, dln_V, dln_L, dln_K, wL_bar, wK_bar, aporte_L, aporte_K, dln_T)

write_csv(ptf_ramas, file.path(dir_ptf, "PTF_v0_por_rama.csv"))

cat("=== PTF v0 por rama (tasas de crecimiento anual, log-diferencias) ===\n")
print(ptf_ramas %>% filter(anio == 2024) %>% mutate(across(where(is.numeric) & !anio, ~ round(.x * 100, 2))))

# =========================
# 3. Agregacion a Total Economia: suma ponderada Tornqvist por
#    participacion nominal del VA de cada rama (ver "Agregacion de las
#    medidas de productividad" en el documento metodologico)
# =========================

va_shares <- panel_base %>%
  filter(rama_ptf %in% RAMAS_9) %>%
  select(anio, rama_ptf, va_corriente) %>%
  group_by(anio) %>%
  mutate(s = va_corriente / sum(va_corriente)) %>%
  ungroup() %>%
  arrange(rama_ptf, anio) %>%
  group_by(rama_ptf) %>%
  mutate(s_bar = (s + lag(s)) / 2) %>%
  ungroup() %>%
  select(anio, rama_ptf, s_bar)

ptf_total <- ptf_ramas %>%
  left_join(va_shares, by = c("anio", "rama_ptf")) %>%
  filter(!is.na(s_bar)) %>%
  group_by(anio) %>%
  summarise(
    dln_V_agg = sum(s_bar * dln_V),
    dln_L_agg = sum(s_bar * dln_L),
    dln_K_agg = sum(s_bar * dln_K),
    dln_T_agg = sum(s_bar * dln_T),
    .groups = "drop"
  )

# Chequeo: dln_V_agg (Tornqvist a partir de las ramas) vs. crecimiento
# directo del VA constante Total Economia del COU (no son identicos por
# el encadenamiento, pero deberian ser muy cercanos)
va_total_directo <- panel_base %>%
  filter(rama_ptf == "TOT_Economia") %>%
  arrange(anio) %>%
  mutate(dln_V_directo = log(va_constante) - log(lag(va_corriente))) %>%
  select(anio, dln_V_directo)

ptf_total <- ptf_total %>%
  left_join(va_total_directo, by = "anio")

write_csv(ptf_total, file.path(dir_ptf, "PTF_v0_total_economia.csv"))

cat("\n=== PTF v0 Total Economia (tasas de crecimiento anual, %) ===\n")
print(ptf_total %>% mutate(across(where(is.numeric) & !anio, ~ round(.x * 100, 2))))
