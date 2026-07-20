# =========================================================
# PTF v0 (enfoque de valor agregado, K y L) - construccion del panel base
#
# Junta, por rama PTF (9 ramas + Total Economia) x anio:
#   - Valor agregado, remuneracion de asalariados, ingreso mixto y excedente
#     de explotacion bruto a precios corrientes (COU)
#   - Valor agregado a precios constantes, base 2015 (COU)
#   - Stock de capital productivo oficial del DANE (K de v0)
#   - Horas totales de ocupados y de asalariados (GEIH), para ajustar
#     la remuneracion de asalariados a ingreso laboral total (LAB_j)
#   - Celdas sexo x grupo de edad x nivel educativo (GEIH), para el
#     indice de Servicios Laborales (L)
#
# Fuente de las formulas: DocumentacionAuxiliar/doc-metodologico-PTF-
# productividad-total-factores-2021.pdf (DANE, marzo 2022)
# =========================================================

source(if (file.exists("Code/_paths.R")) "Code/_paths.R" else "_paths.R")
setwd(PROJECT_ROOT)
ensure_project_dirs()

library(dplyr)
library(tidyr)
library(readr)
library(stringr)

dir_ptf <- project_path("Datos", "Processed", "PTF")

# =========================
# 1. Valor agregado y sus componentes (COU, precios corrientes)
# =========================

cou_corrientes <- read_csv(file.path(dir_ptf, "COU_corrientes_rama.csv"), show_col_types = FALSE) %>%
  pivot_wider(names_from = concepto, values_from = valor) %>%
  rename(
    produccion_corriente = produccion,
    va_corriente = valor_agregado,
    comp_corriente = remuneracion_asalariados,
    ingreso_mixto_corriente = ingreso_mixto,
    ebe_corriente = excedente_explotacion_bruto
  )

# =========================
# 2. Valor agregado a precios constantes (volumenes encadenados, ref. 2015)
# =========================

cou_constantes <- read_csv(file.path(dir_ptf, "COU_constantes_rama.csv"), show_col_types = FALSE) %>%
  pivot_wider(names_from = concepto, values_from = valor) %>%
  rename(
    produccion_constante = produccion,
    va_constante = valor_agregado
  )

# =========================
# 3. Stock de capital productivo (oficial DANE, K de v0)
# =========================

stock_capital <- read_csv(file.path(dir_ptf, "StockCapitalProductivo_rama.csv"), show_col_types = FALSE)

# =========================
# 4. GEIH: horas totales ocupados vs. asalariados, por rama-anio
#    (se usa para ajustar COMP_j -> LAB_j, ver seccion "Participacion de
#    los insumos" del documento metodologico)
# =========================

geih_horas_totales <- read_csv(file.path(dir_ptf, "GEIH_horas_totales_rama.csv"), show_col_types = FALSE) %>%
  mutate(tipo = if_else(asalariado, "H_EMPE", "H_no_asalariado")) %>%
  select(-asalariado) %>%
  pivot_wider(names_from = tipo, values_from = horas_pond) %>%
  mutate(
    H_no_asalariado = replace_na(H_no_asalariado, 0),
    H_EMPE = replace_na(H_EMPE, 0),
    H_EMP = H_no_asalariado + H_EMPE
  ) %>%
  select(anio, rama_ptf, H_EMP, H_EMPE)

# =========================
# 5. GEIH: celdas sexo x grupo_edad x grupo_educ, por rama-anio
#    (se usa para el indice de Servicios Laborales L)
# =========================

geih_celdas <- read_csv(file.path(dir_ptf, "GEIH_celdas_categoria.csv"), show_col_types = FALSE) %>%
  mutate(categoria = paste(sexo, grupo_edad, grupo_educ, sep = "_")) %>%
  select(anio, rama_ptf, categoria, horas_pond, ingreso_pond)

# =========================
# 6. Panel base rama x anio (variables agregadas, sin las celdas de L)
# =========================

panel_base <- cou_corrientes %>%
  full_join(cou_constantes, by = c("anio", "rama_ptf")) %>%
  full_join(stock_capital, by = c("anio", "rama_ptf")) %>%
  full_join(geih_horas_totales, by = c("anio", "rama_ptf")) %>%
  arrange(rama_ptf, anio)

saveRDS(panel_base, file.path(dir_ptf, "panel_base_rama_anio.rds"))
saveRDS(geih_celdas, file.path(dir_ptf, "geih_celdas_L.rds"))

cat("panel_base:", nrow(panel_base), "filas,", n_distinct(panel_base$rama_ptf), "ramas\n")
cat("rango de anios:", paste(range(panel_base$anio, na.rm = TRUE), collapse = " - "), "\n")
print(panel_base %>% filter(anio == 2019))
