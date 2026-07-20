# =========================================================
# PTF v0 - validacion contra el anexo oficial del DANE
#   Datos/Raw/PTF/anex-PTF-Productividad-2025.xlsx
#     Cuadro 1  -> serie Total Economia 2005-2025pr (enfoque valor agregado)
#     Cuadro 23 -> desagregacion por rama, año 2024p (enfoque de produccion,
#                  pero reporta los mismos aportes L/K/PTF por rama que nos
#                  interesa comparar)
#
# La comparacion no debe dar identico: nuestro K (v0) es el stock oficial
# ya publicado (bien), pero nuestro indice L usa una base GEIH distinta
# a la que probablemente use internamente el DANE (celdas homologadas
# propias del proyecto, no las 18 celdas oficiales), y el DANE tiene 2020
# y 2021 con su propio tratamiento de la pandemia mientras nuestro panel
# GEIH no tiene observaciones para 2020. Sirve para ver si el orden de
# magnitud y el signo del residual son razonables.
# =========================================================

source(if (file.exists("Code/_paths.R")) "Code/_paths.R" else "_paths.R")
setwd(PROJECT_ROOT)

library(dplyr)
library(readr)
library(readxl)

dir_ptf <- project_path("Datos", "Processed", "PTF")
raw_xlsx <- project_path("Datos", "Raw", "PTF", "anex-PTF-Productividad-2025.xlsx")

# =========================
# 1. Serie oficial Total Economia (Cuadro 1)
# =========================

cuadro1 <- read_excel(raw_xlsx, sheet = "Cuadro 1", col_names = FALSE)
header_row <- which(apply(cuadro1, 1, function(r) any(r == "Año", na.rm = TRUE)))[1]

oficial_total <- cuadro1[(header_row + 2):nrow(cuadro1), 2:7]
colnames(oficial_total) <- c("anio", "dane_va", "dane_L", "dane_K", "dane_contrib_factores", "dane_ptf")
oficial_total <- oficial_total %>%
  mutate(across(everything(), as.numeric)) %>%
  filter(!is.na(anio))

# =========================
# 2. Nuestra serie Total Economia (v0)
# =========================

propia_total <- read_csv(file.path(dir_ptf, "PTF_v0_total_economia.csv"), show_col_types = FALSE) %>%
  transmute(
    anio,
    propia_va = dln_V_agg * 100,
    propia_L = dln_L_agg * 100,
    propia_K = dln_K_agg * 100,
    propia_ptf = dln_T_agg * 100
  )

comparacion_total <- oficial_total %>%
  inner_join(propia_total, by = "anio") %>%
  mutate(
    diff_va = round(propia_va - dane_va, 2),
    diff_ptf = round(propia_ptf - dane_ptf, 2)
  ) %>%
  mutate(across(where(is.numeric) & !anio, ~ round(.x, 2)))

cat("=== Total Economia: v0 (propia) vs. oficial DANE (Cuadro 1), tasas % ===\n")
print(comparacion_total %>% select(anio, dane_va, propia_va, diff_va, dane_ptf, propia_ptf, diff_ptf))

write_csv(comparacion_total, file.path(dir_ptf, "validacion_total_economia.csv"))

# =========================
# 3. Por rama: NO hay comparacion oficial directa disponible.
#    - Cuadro 23 del anexo es del enfoque de PRODUCCION (incluye consumos
#      intermedios), no de valor agregado, asi que su "Produccion2" no es
#      comparable con nuestro dln_V (valor agregado).
#    - El Cuadro 2 (enfoque de Valor Agregado) por rama solo existe para el
#      ultimo año publicado (2025pr), que no podemos calcular todavia
#      porque el COU y el stock de capital oficiales llegan hasta 2024p.
#    Por eso aqui solo se deja el resultado propio por rama, sin comparar.
# =========================

propia_ramas_2024 <- read_csv(file.path(dir_ptf, "PTF_v0_por_rama.csv"), show_col_types = FALSE) %>%
  filter(anio == 2024) %>%
  transmute(rama_ptf, propia_va = round(dln_V * 100, 2), propia_ptf = round(dln_T * 100, 2))

cat("\n=== Por rama, 2024p: v0 (propia), sin referencia oficial comparable ===\n")
print(propia_ramas_2024)
