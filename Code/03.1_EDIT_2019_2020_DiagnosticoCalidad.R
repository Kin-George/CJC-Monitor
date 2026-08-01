# ==============================================================================
# EDIT Industria 2019-2020 - Diagnostico de calidad para inversion ACTI
# ==============================================================================
# Evalua, a nivel nacional y por division CIIU Rev. 4:
# - missing values, ceros y valores negativos
# - consistencia entre componentes y total de inversion ACTI
# - consistencia entre fuentes y total de financiacion
# - consistencia del detalle de recursos publicos
# - concentracion y valores extremos de inversion total
#
# No crea una base nueva. Imprime tablas y muestra graficos en R/RStudio.
# ==============================================================================

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(readxl)
  library(stringr)
  library(ggplot2)
  library(scales)
})

find_project_root <- function(start = getwd()) {
  current <- normalizePath(start, mustWork = TRUE)
  repeat {
    if (dir.exists(file.path(current, "Code")) && dir.exists(file.path(current, "Datos"))) return(current)
    parent <- dirname(current)
    if (identical(parent, current)) stop("No pude encontrar la raiz del proyecto desde: ", start)
    current <- parent
  }
}

to_numeric_safe <- function(x) {
  if (is.numeric(x)) return(x)
  x <- as.character(x)
  x <- str_replace_all(x, "\\.", "")
  x <- str_replace_all(x, ",", ".")
  suppressWarnings(as.numeric(x))
}

sum_components <- function(data, vars) {
  vars <- intersect(vars, names(data))
  tmp <- data[, vars, drop = FALSE] %>% mutate(across(everything(), to_numeric_safe))
  n_reportados <- rowSums(!is.na(tmp))
  total <- rowSums(tmp, na.rm = TRUE)
  total[n_reportados == 0] <- NA_real_
  total
}

clean_varname <- function(x) {
  out <- toupper(trimws(as.character(x)))
  out <- iconv(out, from = "", to = "ASCII//TRANSLIT")
  out <- gsub("[^A-Z0-9]+", "_", out)
  out <- gsub("^_+|_+$", "", out)
  out[out == ""] <- "VAR"
  out <- ifelse(grepl("^[0-9]", out), paste0("X", out), out)
  make.unique(out, sep = "_")
}

load_edit_dictionary <- function(path, period = "2019_2020") {
  read_excel(path, sheet = "variables", col_types = "text") %>%
    transmute(
      edit_period = as.character(period),
      order_global = suppressWarnings(as.integer(order_global)),
      variable_original_dic = as.character(variable),
      label_dane = as.character(label)
    ) %>%
    filter(edit_period == period) %>%
    arrange(order_global) %>%
    mutate(variable_dic = clean_varname(variable_original_dic))
}

apply_dictionary_names <- function(data, dict_period) {
  raw_names <- names(data)

  if (nrow(dict_period) == ncol(data)) {
    names(data) <- make.unique(dict_period$variable_dic, sep = "_")
    attr(data, "dictionary_name_source") <- "diccionario_dane_por_orden"
  } else {
    names(data) <- clean_varname(raw_names)
    attr(data, "dictionary_name_source") <- "nombres_microdato_limpios"
    warning(
      "El diccionario 2019_2020 tiene ", nrow(dict_period),
      " variables, pero la base cruda tiene ", ncol(data),
      ". Se usan nombres limpios de la base cruda."
    )
  }

  attr(data, "raw_names") <- raw_names
  data
}

as_plain_character <- function(x) {
  if (inherits(x, "haven_labelled")) return(as.character(as_factor(x, levels = "values")))
  as.character(x)
}

normalize_ciiu_code <- function(x) {
  out <- str_extract(as.character(x), "[0-9]+")
  ifelse(is.na(out) | out == "", NA_character_, out)
}

detect_ciiu_var <- function(data) {
  nms <- names(data)

  case_when(
    "CIIU4" %in% nms ~ "CIIU4",
    "CIIU_4" %in% nms ~ "CIIU_4",
    "CIIU3" %in% nms ~ "CIIU3",
    "CIIU_3" %in% nms ~ "CIIU_3",
    "ACT3" %in% nms ~ "ACT3",
    "ACT" %in% nms ~ "ACT",
    TRUE ~ NA_character_
  )
}

project_root <- find_project_root()
raw_dir <- file.path(project_root, "Datos", "Raw", "EDIT")
processed_dir <- file.path(project_root, "Datos", "Processed")
doc_dir <- file.path(project_root, "DocumentacionAuxiliar")
dict_dir <- file.path(project_root, "Diccionarios", "EDIT")

raw_path <- file.path(raw_dir, "EDIT_X_2019_2020.dta")
dictionary_path <- file.path(dict_dir, "EDIT_Diccionarios_Consolidado.xlsx")
ciiu4_structure_path <- file.path(doc_dir, "Estructura-detallada-CIIU-4AC-2022.xlsx")

if (!file.exists(raw_path)) stop("No existe la base cruda EDIT 2019-2020: ", raw_path)
if (!file.exists(dictionary_path)) stop("No existe el diccionario consolidado EDIT: ", dictionary_path)
if (!file.exists(ciiu4_structure_path)) stop("No existe: ", ciiu4_structure_path)

ciiu4_div_labels <- read_excel(ciiu4_structure_path, sheet = 1, skip = 1, col_types = "text") %>%
  transmute(
    ciiu4_div = str_pad(as.character(División), width = 2, pad = "0"),
    sector = str_to_sentence(str_to_lower(as.character(Descripción)))
  ) %>%
  filter(!is.na(ciiu4_div), !is.na(sector)) %>%
  distinct(ciiu4_div, .keep_all = TRUE) %>%
  mutate(sector_label = paste0(ciiu4_div, " - ", sector))

# ------------------------------------------------------------------------------
# 1. Definicion del universo de variables
# ------------------------------------------------------------------------------
# Estas son las variables que alimentan los calculos de inversion, financiacion
# total y detalle de recursos publicos. C1 corresponde a 2019 y C2/C3/C4 a 2020
# segun la estructura de cada pregunta del formulario.
investment_vars <- c(
  "II1R1C1", "II1R2C1", "II1R3C1", "II1R4C1", "II1R5C1", "II1R6C1",
  "II1R7C1", "II1R8C1", "II1R9C1", "II1R11C1", "II1R12C1", "II1R10C1",
  "II1R1C2", "II1R2C2", "II1R3C2", "II1R4C2", "II1R5C2", "II1R6C2",
  "II1R7C2", "II1R8C2", "II1R9C2", "II1R11C2", "II1R12C2", "II1R10C2"
)

financing_vars <- c(
  "III1R1C1", "III1R2C1", "III1R3C1", "III1R4C1", "III1R4C2", "III1R5C1", "III1R5C2",
  "III1R6C1", "III1R6C2", "III1R7C1", "III1R7C2", "III1R8C1",
  "III1R1C2", "III1R2C2", "III1R3C2", "III1R4C3", "III1R4C4", "III1R5C3", "III1R5C4",
  "III1R6C3", "III1R6C4", "III1R7C3", "III1R7C4", "III1R8C2"
)

public_detail_vars <- c(
  "III2R1C1", "III2R2C1", "III2R3C1", "III2R4C1", "III2R5C1", "III2R6C1", "III2R7C1", "III2R8C1", "III2R9C1", "III2R10C1",
  "III2R1C2", "III2R2C2", "III2R3C2", "III2R4C2", "III2R5C2", "III2R6C2", "III2R7C2", "III2R8C2", "III2R9C2", "III2R10C2"
)

id_vars <- c("edit_period", "nordemp", "ciiu4_div", "ciiu4_homologado")
needed_vars <- unique(c(id_vars, investment_vars, financing_vars, public_detail_vars))

# ------------------------------------------------------------------------------
# 2. Carga exclusiva de EDIT Industria 2019-2020
# ------------------------------------------------------------------------------
# Se usa la base cruda del DANE para 2019-2020. Como sus nombres originales no
# siempre coinciden con los nombres publicados en el diccionario, primero se
# renombran las columnas por orden usando el diccionario descargado del DANE.
# Luego se crean los identificadores mínimos que antes venían desde EDIT_Panel.
dict_2019_2020 <- load_edit_dictionary(dictionary_path, period = "2019_2020")

edit_raw <- read_dta(raw_path)
edit_raw <- apply_dictionary_names(edit_raw, dict_2019_2020)

message("Fuente de nombres: ", attr(edit_raw, "dictionary_name_source"))

ciiu_var <- detect_ciiu_var(edit_raw)
if (is.na(ciiu_var)) {
  warning("No se encontro una variable CIIU reconocible en la base cruda.")
}
ciiu_select <- if (is.na(ciiu_var)) character(0) else ciiu_var

edit_raw <- edit_raw %>%
  mutate(
    edit_period = "2019_2020",
    nordemp = if ("NORDEMP" %in% names(.)) as_plain_character(NORDEMP) else NA_character_,
    empresa_id = as.character(nordemp),
    ciiu_original = if (!is.na(ciiu_var)) normalize_ciiu_code(as_plain_character(.data[[ciiu_var]])) else NA_character_,
    ciiu4_homologado = case_when(
      !is.na(ciiu_original) & nchar(ciiu_original) >= 4 ~ str_sub(str_pad(ciiu_original, 4, pad = "0"), 1, 4),
      !is.na(ciiu_original) & nchar(ciiu_original) == 3 ~ str_pad(ciiu_original, 3, pad = "0"),
      !is.na(ciiu_original) & nchar(ciiu_original) == 2 ~ str_pad(ciiu_original, 2, pad = "0"),
      TRUE ~ NA_character_
    ),
    ciiu4_div = case_when(
      !is.na(ciiu4_homologado) & nchar(ciiu4_homologado) >= 3 ~ str_sub(ciiu4_homologado, 1, 2),
      !is.na(ciiu4_homologado) & nchar(ciiu4_homologado) == 2 ~ ciiu4_homologado,
      TRUE ~ NA_character_
    )
  )

available_vars <- names(edit_raw)
missing_vars <- setdiff(needed_vars, available_vars)
if (length(missing_vars) > 0) warning("Variables no encontradas: ", paste(missing_vars, collapse = ", "))

edit <- edit_raw %>%
  select(any_of(needed_vars), empresa_id, ciiu_original, any_of(ciiu_select)) %>%
  left_join(ciiu4_div_labels, by = "ciiu4_div")

if (nrow(edit) == 0) stop("No encontre observaciones en la base cruda EDIT 2019-2020.")

investment_labels <- c(
  II1R1C1 = "I+D interna 2019", II1R2C1 = "I+D externa 2019", II1R3C1 = "Maquinaria 2019",
  II1R4C1 = "TIC, software y datos 2019", II1R5C1 = "Mercadotecnia 2019", II1R6C1 = "Propiedad intelectual 2019",
  II1R7C1 = "Consultoria 2019", II1R8C1 = "Ingenieria y diseno 2019", II1R9C1 = "Capacitacion 2019",
  II1R11C1 = "Edificaciones 2019", II1R12C1 = "Metodos organizativos 2019", II1R10C1 = "Total ACTI 2019",
  II1R1C2 = "I+D interna 2020", II1R2C2 = "I+D externa 2020", II1R3C2 = "Maquinaria 2020",
  II1R4C2 = "TIC, software y datos 2020", II1R5C2 = "Mercadotecnia 2020", II1R6C2 = "Propiedad intelectual 2020",
  II1R7C2 = "Consultoria 2020", II1R8C2 = "Ingenieria y diseno 2020", II1R9C2 = "Capacitacion 2020",
  II1R11C2 = "Edificaciones 2020", II1R12C2 = "Metodos organizativos 2020", II1R10C2 = "Total ACTI 2020"
)

# ------------------------------------------------------------------------------
# 3. Faltantes, ceros y negativos para TODAS las variables utilizadas
# ------------------------------------------------------------------------------
# Esta tabla es la primera referencia para decidir si una variable es apta para
# analisis: reporta el numero exacto de missings, ceros y negativos por variable.
all_analysis_vars <- unique(c(investment_vars, financing_vars, public_detail_vars))

long_all_variables <- edit %>%
  select(empresa_id, ciiu4_div, sector_label, any_of(all_analysis_vars)) %>%
  pivot_longer(
    -c(empresa_id, ciiu4_div, sector_label),
    names_to = "variable",
    values_to = "valor",
    values_transform = list(valor = as.character)
  ) %>%
  mutate(valor = to_numeric_safe(valor))

missing_por_variable <- long_all_variables %>%
  group_by(variable) %>%
  summarise(
    empresas = n(),
    missing = sum(is.na(valor)),
    pct_missing = mean(is.na(valor)),
    ceros = sum(valor == 0, na.rm = TRUE),
    negativos = sum(valor < 0, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(pct_missing), variable)

missing_por_variable_sector <- long_all_variables %>%
  filter(!is.na(ciiu4_div), ciiu4_div != "") %>%
  group_by(ciiu4_div, sector_label, variable) %>%
  summarise(
    empresas = n(),
    missing = sum(is.na(valor)),
    pct_missing = mean(is.na(valor)),
    ceros = sum(valor == 0, na.rm = TRUE),
    negativos = sum(valor < 0, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(variable, desc(pct_missing), ciiu4_div)

# ------------------------------------------------------------------------------
# 4. Diagnostico especifico de los componentes de inversion ACTI
# ------------------------------------------------------------------------------
# El formato largo permite comparar la calidad de cada rubro de inversion entre
# 2019 y 2020, tanto en el total nacional como por division CIIU.
long_inversion <- edit %>%
  select(empresa_id, ciiu4_div, sector_label, any_of(names(investment_labels))) %>%
  pivot_longer(
    -c(empresa_id, ciiu4_div, sector_label),
    names_to = "variable",
    values_to = "valor",
    values_transform = list(valor = as.character)
  ) %>%
  mutate(
    valor = to_numeric_safe(valor),
    indicador = unname(investment_labels[variable]),
    year = if_else(str_detect(variable, "C1$"), 2019L, 2020L),
    es_total = str_detect(variable, "II1R10")
  )

diagnostico_general <- long_inversion %>%
  group_by(year, indicador, es_total) %>%
  summarise(
    empresas = n(),
    missing = sum(is.na(valor)),
    pct_missing = mean(is.na(valor)),
    ceros = sum(valor == 0, na.rm = TRUE),
    pct_ceros = mean(valor == 0, na.rm = TRUE),
    negativos = sum(valor < 0, na.rm = TRUE),
    minimo = suppressWarnings(min(valor, na.rm = TRUE)),
    p50 = quantile(valor, 0.50, na.rm = TRUE, names = FALSE),
    p99 = quantile(valor, 0.99, na.rm = TRUE, names = FALSE),
    maximo = suppressWarnings(max(valor, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(across(c(minimo, p50, p99, maximo), ~ ifelse(is.infinite(.x), NA_real_, .x))) %>%
  arrange(year, desc(es_total), indicador)

diagnostico_sector <- long_inversion %>%
  filter(!is.na(ciiu4_div), ciiu4_div != "") %>%
  group_by(year, ciiu4_div, sector_label, indicador, es_total) %>%
  summarise(
    empresas = n(),
    missing = sum(is.na(valor)),
    pct_missing = mean(is.na(valor)),
    ceros = sum(valor == 0, na.rm = TRUE),
    pct_ceros = mean(valor == 0, na.rm = TRUE),
    negativos = sum(valor < 0, na.rm = TRUE),
    p50 = quantile(valor, 0.50, na.rm = TRUE, names = FALSE),
    p99 = quantile(valor, 0.99, na.rm = TRUE, names = FALSE),
    maximo = suppressWarnings(max(valor, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(across(c(p50, p99, maximo), ~ ifelse(is.infinite(.x), NA_real_, .x))) %>%
  arrange(year, indicador, desc(pct_missing))

edit_quality <- edit %>%
  mutate(
    inversion_componentes_2019 = sum_components(., c("II1R1C1", "II1R2C1", "II1R3C1", "II1R4C1", "II1R5C1", "II1R6C1", "II1R7C1", "II1R8C1", "II1R9C1", "II1R11C1", "II1R12C1")),
    inversion_total_2019 = to_numeric_safe(II1R10C1),
    inversion_componentes_2020 = sum_components(., c("II1R1C2", "II1R2C2", "II1R3C2", "II1R4C2", "II1R5C2", "II1R6C2", "II1R7C2", "II1R8C2", "II1R9C2", "II1R11C2", "II1R12C2")),
    inversion_total_2020 = to_numeric_safe(II1R10C2),
    financiacion_componentes_2019 = sum_components(., c("III1R1C1", "III1R2C1", "III1R3C1", "III1R4C1", "III1R4C2", "III1R5C1", "III1R5C2", "III1R6C1", "III1R6C2", "III1R7C1", "III1R7C2")),
    financiacion_total_2019 = to_numeric_safe(III1R8C1),
    financiacion_componentes_2020 = sum_components(., c("III1R1C2", "III1R2C2", "III1R3C2", "III1R4C3", "III1R4C4", "III1R5C3", "III1R5C4", "III1R6C3", "III1R6C4", "III1R7C3", "III1R7C4")),
    financiacion_total_2020 = to_numeric_safe(III1R8C2),
    publico_componentes_2019 = sum_components(., c("III2R1C1", "III2R2C1", "III2R3C1", "III2R4C1", "III2R5C1", "III2R6C1", "III2R7C1", "III2R8C1", "III2R9C1")),
    publico_total_2019 = to_numeric_safe(III2R10C1),
    publico_componentes_2020 = sum_components(., c("III2R1C2", "III2R2C2", "III2R3C2", "III2R4C2", "III2R5C2", "III2R6C2", "III2R7C2", "III2R8C2", "III2R9C2")),
    publico_total_2020 = to_numeric_safe(III2R10C2)
  )

# ------------------------------------------------------------------------------
# 5. Pruebas de reconciliacion contable
# ------------------------------------------------------------------------------
# Cada prueba verifica si una columna consolidada es igual a la suma de sus
# columnas desagregadas: total ACTI, total de financiacion y recursos publicos.
# Una empresa es consistente solo cuando ambas partes estan reportadas y la
# diferencia es cero, salvo tolerancia numerica menor a 0.000001.
reconciliacion_empresa <- bind_rows(
  edit_quality %>% transmute(empresa_id, ciiu4_div, sector_label, year = 2019L, prueba = "Inversion ACTI: componentes vs total", componentes = inversion_componentes_2019, total = inversion_total_2019),
  edit_quality %>% transmute(empresa_id, ciiu4_div, sector_label, year = 2020L, prueba = "Inversion ACTI: componentes vs total", componentes = inversion_componentes_2020, total = inversion_total_2020),
  edit_quality %>% transmute(empresa_id, ciiu4_div, sector_label, year = 2019L, prueba = "Financiacion: fuentes vs total", componentes = financiacion_componentes_2019, total = financiacion_total_2019),
  edit_quality %>% transmute(empresa_id, ciiu4_div, sector_label, year = 2020L, prueba = "Financiacion: fuentes vs total", componentes = financiacion_componentes_2020, total = financiacion_total_2020),
  edit_quality %>% transmute(empresa_id, ciiu4_div, sector_label, year = 2019L, prueba = "Recursos publicos: detalle vs total", componentes = publico_componentes_2019, total = publico_total_2019),
  edit_quality %>% transmute(empresa_id, ciiu4_div, sector_label, year = 2020L, prueba = "Recursos publicos: detalle vs total", componentes = publico_componentes_2020, total = publico_total_2020)
) %>%
  mutate(
    diferencia = total - componentes,
    diferencia_relativa = if_else(total > 0, diferencia / total, NA_real_),
    comparable = !is.na(componentes) & !is.na(total),
    consistente = comparable & abs(diferencia) < 1e-6
  )

reconciliacion_general <- reconciliacion_empresa %>%
  group_by(year, prueba) %>%
  summarise(
    empresas = n(),
    comparables = sum(comparable),
    pct_comparables = mean(comparable),
    consistentes = sum(consistente, na.rm = TRUE),
    pct_consistentes = mean(consistente, na.rm = TRUE),
    diferencia_abs_p50 = median(abs(diferencia), na.rm = TRUE),
    diferencia_abs_p99 = quantile(abs(diferencia), 0.99, na.rm = TRUE, names = FALSE),
    .groups = "drop"
  )

reconciliacion_sector <- reconciliacion_empresa %>%
  filter(!is.na(ciiu4_div), ciiu4_div != "") %>%
  group_by(year, prueba, ciiu4_div, sector_label) %>%
  summarise(
    empresas = n(),
    comparables = sum(comparable),
    pct_comparables = mean(comparable),
    pct_consistentes = mean(consistente, na.rm = TRUE),
    diferencia_abs_p50 = median(abs(diferencia), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(year, prueba, pct_consistentes)

outliers_sector <- edit_quality %>%
  transmute(
    empresa_id, ciiu4_div, sector_label,
    inversion_total_2019 = inversion_total_2019,
    inversion_total_2020 = inversion_total_2020
  ) %>%
  pivot_longer(starts_with("inversion_total_"), names_to = "year", values_to = "inversion_total") %>%
  mutate(year = if_else(year == "inversion_total_2019", 2019L, 2020L)) %>%
  filter(!is.na(inversion_total), inversion_total > 0) %>%
  group_by(year, ciiu4_div, sector_label) %>%
  mutate(percentil_en_sector = percent_rank(inversion_total)) %>%
  ungroup() %>%
  filter(percentil_en_sector >= 0.99) %>%
  arrange(year, desc(inversion_total))

# ------------------------------------------------------------------------------
# 6. Graficos legibles por sector
# ------------------------------------------------------------------------------
# Para evitar etiquetas superpuestas, cada grafico muestra diez sectores: los de
# mayor porcentaje de faltantes para el primer grafico y los de menor consistencia
# entre los sectores que si tienen empresas comparables para el segundo.
top_sectors_missing <- diagnostico_sector %>%
  filter(es_total, !is.na(sector_label)) %>%
  group_by(year) %>%
  slice_max(pct_missing, n = 10, with_ties = FALSE) %>%
  ungroup()

top_sectors_consistencia <- reconciliacion_sector %>%
  filter(
    prueba == "Inversion ACTI: componentes vs total",
    !is.na(sector_label),
    comparables > 0,
    !is.na(pct_consistentes)
  ) %>%
  group_by(year) %>%
  slice_min(pct_consistentes, n = 10, with_ties = FALSE) %>%
  ungroup()

grafico_missing <- ggplot(top_sectors_missing, aes(x = reorder(sector_label, pct_missing), y = pct_missing, fill = factor(year))) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~year, scales = "free_y") +
  scale_y_continuous(labels = percent, limits = c(0, 1)) +
  scale_x_discrete(labels = function(x) str_trunc(x, width = 70)) +
  labs(
    title = "Faltantes en inversion total ACTI por sector",
    subtitle = "Diez divisiones CIIU con mayor proporcion de faltantes por año.",
    x = NULL,
    y = "Proporcion de empresas con valor faltante"
  ) +
  theme_minimal(base_size = 10) +
  theme(plot.title = element_text(face = "bold"), axis.text.y = element_text(size = 8))

grafico_consistencia <- top_sectors_consistencia %>%
  ggplot(aes(x = reorder(sector_label, pct_consistentes), y = pct_consistentes, fill = factor(year))) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~year, scales = "free_y") +
  scale_y_continuous(labels = percent, limits = c(0, 1)) +
  scale_x_discrete(labels = function(x) str_trunc(x, width = 70)) +
  labs(
    title = "Consistencia de inversion ACTI por sector",
    subtitle = "Diez sectores con menor consistencia; solo incluye empresas comparables.",
    x = NULL,
    y = "Proporcion de empresas consistentes"
  ) +
  theme_minimal(base_size = 10) +
  theme(plot.title = element_text(face = "bold"), axis.text.y = element_text(size = 8))

cat("\n=== 1. Diagnostico general de faltantes y valores ===\n")
print(diagnostico_general)
cat("\n=== 1A. Faltantes, ceros y negativos de cada variable utilizada ===\n")
print(missing_por_variable)
cat("\n=== 1B. Faltantes por variable y sector ===\n")
print(missing_por_variable_sector)
cat("\n=== 2. Reconciliacion general de totales ===\n")
print(reconciliacion_general)
cat("\n=== 3. Diagnostico por sector ===\n")
print(diagnostico_sector)
cat("\n=== 4. Reconciliacion por sector ===\n")
print(reconciliacion_sector)
cat("\n=== 5. Valores extremos de inversion total ===\n")
print(outliers_sector)
print(grafico_missing)
print(grafico_consistencia)

message("Listo. Los objetos missing_por_variable, missing_por_variable_sector, diagnostico_general, diagnostico_sector, reconciliacion_general, reconciliacion_sector y outliers_sector quedan disponibles en memoria.")
