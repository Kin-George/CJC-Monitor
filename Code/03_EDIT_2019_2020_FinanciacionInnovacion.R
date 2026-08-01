# ==============================================================================
# EDIT Industria 2019-2020 - Financiacion de innovacion y tecnologia
# ==============================================================================
# Pregunta:
#   Quien financia la inversion en innovacion y tecnologia: recursos publicos
#   o recursos privados?
#
# Usa solamente EDIT Industria 2019-2020 y el modulo III.1 del diccionario DANE.
# Los recursos de cooperacion/donaciones se muestran por separado: no se pueden
# clasificar de forma rigurosa como publicos o privados con esta variable.
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

first_nonmissing <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_real_)
  x[1]
}

first_nonmissing_chr <- function(x) {
  x <- x[!is.na(x) & x != ""]
  if (length(x) == 0) return(NA_character_)
  x[1]
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
      variable_original_dic = as.character(variable)
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
      " variables, pero la base cruda EDIT tiene ", ncol(data),
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

normalize_id <- function(x) {
  x <- as_plain_character(x)
  x <- str_trim(x)
  x <- str_replace(x, "\\.0$", "")
  x <- str_replace_all(x, "[^0-9A-Za-z]", "")
  ifelse(x == "", NA_character_, x)
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

find_var <- function(data, candidates) {
  nms <- names(data)
  hit <- candidates[toupper(candidates) %in% toupper(nms)]
  if (length(hit) == 0) return(NA_character_)
  nms[match(toupper(hit[1]), toupper(nms))]
}

project_root <- find_project_root()
raw_edit_dir <- file.path(project_root, "Datos", "Raw", "EDIT")
doc_dir <- file.path(project_root, "DocumentacionAuxiliar")
dict_dir <- file.path(project_root, "Diccionarios", "EDIT")

edit_raw_path <- file.path(raw_edit_dir, "EDIT_X_2019_2020.dta")
dictionary_path <- file.path(dict_dir, "EDIT_Diccionarios_Consolidado.xlsx")
ciiu4_structure_path <- file.path(doc_dir, "Estructura-detallada-CIIU-4AC-2022.xlsx")

if (!file.exists(edit_raw_path)) stop("No existe la base cruda EDIT 2019-2020: ", edit_raw_path)
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

vars_needed <- c(
  "edit_period", "nordemp", "ciiu4_div", "ciiu4_homologado",
  "III1R1C1", "III1R1C2", # Recursos propios
  "III1R2C1", "III1R2C2", # Otras empresas del grupo
  "III1R3C1", "III1R3C2", # Recursos publicos
  "III1R4C1", "III1R4C2", "III1R4C3", "III1R4C4", # Banca privada
  "III1R5C1", "III1R5C2", "III1R5C3", "III1R5C4", # Otras empresas
  "III1R6C1", "III1R6C2", "III1R6C3", "III1R6C4", # Capital privado
  "III1R7C1", "III1R7C2", "III1R7C3", "III1R7C4", # Cooperacion/donaciones
  "III1R8C1", "III1R8C2", # Total fuentes de financiacion
  "II1R10C1", "II1R10C2", # Total invertido en innovacion
  "II1R1C1", "II1R1C2", "II1R2C1", "II1R2C2", "II1R3C1", "II1R3C2",
  "II1R4C1", "II1R4C2", "II1R5C1", "II1R5C2", "II1R6C1", "II1R6C2",
  "II1R7C1", "II1R7C2", "II1R8C1", "II1R8C2", "II1R9C1", "II1R9C2",
  "II1R11C1", "II1R11C2", "II1R12C1", "II1R12C2", # Destino de la inversion ACTI
  "I3R1C1", "I3R1C2", "I3R2C1", "I3R2C2", # Ingresos/ventas nacionales y exportaciones
  "III3R1C1", "III4R1C1", "III4R2C1", "III4R3C1", "III4R4C1", "III4R5C1", "III4R6C1", "III5R1C1",
  "I2R5C1", "I2R6C1", "I2R16C1", "I2R17C1", "I2R18C1", # Impactos de innovacion
  "I4R1C1", "I4R2C1", "I4R3C1", "I4R4C1", "I4R5C1", # Ventas nacionales por tipo de innovacion
  "IV1R11C1", "IV1R11C2", "IV1R11C3", "IV1R11C4", # Personal total y personal ACTI
  "III2R1C1", "III2R1C2", "III2R2C1", "III2R2C2", "III2R3C1", "III2R3C2",
  "III2R4C1", "III2R4C2", "III2R5C1", "III2R5C2", "III2R6C1", "III2R6C2",
  "III2R7C1", "III2R7C2", "III2R8C1", "III2R8C2", "III2R9C1", "III2R9C2"
)

dict_2019_2020 <- load_edit_dictionary(dictionary_path, period = "2019_2020")

edit_raw <- read_dta(edit_raw_path)
edit_raw <- apply_dictionary_names(edit_raw, dict_2019_2020)
message("Fuente de nombres EDIT: ", attr(edit_raw, "dictionary_name_source"))

ciiu_var <- detect_ciiu_var(edit_raw)
if (is.na(ciiu_var)) warning("No se encontro una variable CIIU reconocible en EDIT.")

edit_raw <- edit_raw %>%
  mutate(
    edit_period = "2019_2020",
    nordemp = if ("NORDEMP" %in% names(.)) normalize_id(NORDEMP) else NA_character_,
    empresa_id = nordemp,
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
missing_vars <- setdiff(vars_needed, available_vars)
if (length(missing_vars) > 0) warning("Variables no encontradas en EDIT: ", paste(missing_vars, collapse = ", "))

edit_2019_2020 <- edit_raw %>%
  select(any_of(vars_needed), empresa_id, ciiu_original) %>%
  mutate(
    ciiu4_div = as.character(ciiu4_div)
  ) %>%
  left_join(ciiu4_div_labels, by = "ciiu4_div")

financiacion_empresa_anio <- bind_rows(
  edit_2019_2020 %>%
    transmute(
      year = 2019L,
      empresa_id,
      ciiu4_div,
      ciiu4_homologado = as.character(ciiu4_homologado),
      sector_label,
      recursos_propios = to_numeric_safe(III1R1C1),
      recursos_grupo = to_numeric_safe(III1R2C1),
      recursos_publicos = to_numeric_safe(III1R3C1),
      banca_privada = to_numeric_safe(III1R4C1) + to_numeric_safe(III1R4C2),
      otras_empresas = to_numeric_safe(III1R5C1) + to_numeric_safe(III1R5C2),
      capital_privado = to_numeric_safe(III1R6C1) + to_numeric_safe(III1R6C2),
      cooperacion_donaciones = to_numeric_safe(III1R7C1) + to_numeric_safe(III1R7C2),
      total_financiacion_reportado = to_numeric_safe(III1R8C1),
      total_inversion_innovacion = to_numeric_safe(II1R10C1),
      ingresos_ventas_nacionales = to_numeric_safe(I3R1C1),
      exportaciones_totales = to_numeric_safe(I3R1C2),
      inversion_id_interna = to_numeric_safe(II1R1C1),
      inversion_id_externa = to_numeric_safe(II1R2C1),
      inversion_maquinaria = to_numeric_safe(II1R3C1),
      inversion_tic_datos = to_numeric_safe(II1R4C1),
      inversion_mercadotecnia = to_numeric_safe(II1R5C1),
      inversion_propiedad_intelectual = to_numeric_safe(II1R6C1),
      inversion_consultoria = to_numeric_safe(II1R7C1),
      inversion_ingenieria_diseno = to_numeric_safe(II1R8C1),
      inversion_capacitacion = to_numeric_safe(II1R9C1),
      inversion_edificaciones = to_numeric_safe(II1R11C1),
      inversion_metodos_organizativos = to_numeric_safe(II1R12C1),
      empleo_total = to_numeric_safe(IV1R11C1),
      empleo_acti = to_numeric_safe(IV1R11C3),
      intencion_solicitar_publicos = to_numeric_safe(III3R1C1),
      beneficio_tributario = to_numeric_safe(III5R1C1),
      impacto_productividad = to_numeric_safe(I2R5C1),
      impacto_costos_laborales = to_numeric_safe(I2R6C1),
      impacto_ventas = to_numeric_safe(I2R16C1),
      impacto_utilidades = to_numeric_safe(I2R17C1),
      impacto_gestion = to_numeric_safe(I2R18C1),
      ventas_innovacion_empresa = to_numeric_safe(I4R1C1) + to_numeric_safe(I4R2C1) + to_numeric_safe(I4R3C1),
      ventas_no_innovadoras = to_numeric_safe(I4R4C1),
      ventas_nacionales_total_pct = to_numeric_safe(I4R5C1),
      barrera_desconocimiento = to_numeric_safe(III4R1C1),
      barrera_requisitos = to_numeric_safe(III4R2C1),
      barrera_tramites = to_numeric_safe(III4R3C1),
      barrera_tiempo = to_numeric_safe(III4R4C1),
      barrera_condiciones = to_numeric_safe(III4R5C1),
      barrera_intermediacion = to_numeric_safe(III4R6C1),
      mincit_innpulsa_colombia_productiva = to_numeric_safe(III2R1C1),
      sena = to_numeric_safe(III2R2C1),
      minciencias = to_numeric_safe(III2R3C1),
      mintic = to_numeric_safe(III2R4C1),
      otra_entidad_publica = to_numeric_safe(III2R5C1),
      bancoldex = to_numeric_safe(III2R6C1),
      finagro = to_numeric_safe(III2R7C1),
      fondos_territoriales = to_numeric_safe(III2R8C1),
      sgr_cti = to_numeric_safe(III2R9C1)
    ),
  edit_2019_2020 %>%
    transmute(
      year = 2020L,
      empresa_id,
      ciiu4_div,
      ciiu4_homologado = as.character(ciiu4_homologado),
      sector_label,
      recursos_propios = to_numeric_safe(III1R1C2),
      recursos_grupo = to_numeric_safe(III1R2C2),
      recursos_publicos = to_numeric_safe(III1R3C2),
      banca_privada = to_numeric_safe(III1R4C3) + to_numeric_safe(III1R4C4),
      otras_empresas = to_numeric_safe(III1R5C3) + to_numeric_safe(III1R5C4),
      capital_privado = to_numeric_safe(III1R6C3) + to_numeric_safe(III1R6C4),
      cooperacion_donaciones = to_numeric_safe(III1R7C3) + to_numeric_safe(III1R7C4),
      total_financiacion_reportado = to_numeric_safe(III1R8C2),
      total_inversion_innovacion = to_numeric_safe(II1R10C2),
      ingresos_ventas_nacionales = to_numeric_safe(I3R2C1),
      exportaciones_totales = to_numeric_safe(I3R2C2),
      inversion_id_interna = to_numeric_safe(II1R1C2),
      inversion_id_externa = to_numeric_safe(II1R2C2),
      inversion_maquinaria = to_numeric_safe(II1R3C2),
      inversion_tic_datos = to_numeric_safe(II1R4C2),
      inversion_mercadotecnia = to_numeric_safe(II1R5C2),
      inversion_propiedad_intelectual = to_numeric_safe(II1R6C2),
      inversion_consultoria = to_numeric_safe(II1R7C2),
      inversion_ingenieria_diseno = to_numeric_safe(II1R8C2),
      inversion_capacitacion = to_numeric_safe(II1R9C2),
      inversion_edificaciones = to_numeric_safe(II1R11C2),
      inversion_metodos_organizativos = to_numeric_safe(II1R12C2),
      empleo_total = to_numeric_safe(IV1R11C2),
      empleo_acti = to_numeric_safe(IV1R11C4),
      intencion_solicitar_publicos = to_numeric_safe(III3R1C1),
      beneficio_tributario = to_numeric_safe(III5R1C1),
      impacto_productividad = to_numeric_safe(I2R5C1),
      impacto_costos_laborales = to_numeric_safe(I2R6C1),
      impacto_ventas = to_numeric_safe(I2R16C1),
      impacto_utilidades = to_numeric_safe(I2R17C1),
      impacto_gestion = to_numeric_safe(I2R18C1),
      ventas_innovacion_empresa = to_numeric_safe(I4R1C1) + to_numeric_safe(I4R2C1) + to_numeric_safe(I4R3C1),
      ventas_no_innovadoras = to_numeric_safe(I4R4C1),
      ventas_nacionales_total_pct = to_numeric_safe(I4R5C1),
      barrera_desconocimiento = to_numeric_safe(III4R1C1),
      barrera_requisitos = to_numeric_safe(III4R2C1),
      barrera_tramites = to_numeric_safe(III4R3C1),
      barrera_tiempo = to_numeric_safe(III4R4C1),
      barrera_condiciones = to_numeric_safe(III4R5C1),
      barrera_intermediacion = to_numeric_safe(III4R6C1),
      mincit_innpulsa_colombia_productiva = to_numeric_safe(III2R1C2),
      sena = to_numeric_safe(III2R2C2),
      minciencias = to_numeric_safe(III2R3C2),
      mintic = to_numeric_safe(III2R4C2),
      otra_entidad_publica = to_numeric_safe(III2R5C2),
      bancoldex = to_numeric_safe(III2R6C2),
      finagro = to_numeric_safe(III2R7C2),
      fondos_territoriales = to_numeric_safe(III2R8C2),
      sgr_cti = to_numeric_safe(III2R9C2)
    )
) %>%
  mutate(
    ingresos_ventas_totales = case_when(
      is.na(ingresos_ventas_nacionales) & is.na(exportaciones_totales) ~ NA_real_,
      TRUE ~ replace_na(ingresos_ventas_nacionales, 0) + replace_na(exportaciones_totales, 0)
    ),
    across(
      c(
        recursos_propios,
        recursos_grupo,
        recursos_publicos,
        banca_privada,
        otras_empresas,
        capital_privado,
        cooperacion_donaciones,
        inversion_id_interna,
        inversion_id_externa,
        inversion_maquinaria,
        inversion_tic_datos,
        inversion_mercadotecnia,
        inversion_propiedad_intelectual,
        inversion_consultoria,
        inversion_ingenieria_diseno,
        inversion_capacitacion,
        inversion_edificaciones,
        inversion_metodos_organizativos
      ),
      ~ replace_na(.x, 0)
    )
  ) %>%
  mutate(
    financiamiento_privado = recursos_propios + recursos_grupo + banca_privada + otras_empresas + capital_privado,
    financiamiento_clasificado = financiamiento_privado + recursos_publicos + cooperacion_donaciones,
    diferencia_con_total_reportado = total_financiacion_reportado - financiamiento_clasificado,
    participacion_publica = if_else(total_financiacion_reportado > 0, recursos_publicos / total_financiacion_reportado, NA_real_),
    participacion_privada = if_else(total_financiacion_reportado > 0, financiamiento_privado / total_financiacion_reportado, NA_real_),
    participacion_cooperacion = if_else(total_financiacion_reportado > 0, cooperacion_donaciones / total_financiacion_reportado, NA_real_),
    intensidad_acti = empleo_acti / empleo_total,
    tipo_financiacion = case_when(
      recursos_publicos > 0 & financiamiento_privado > 0 ~ "Mixta",
      recursos_publicos > 0 ~ "Publica",
      financiamiento_privado > 0 ~ "Privada",
      cooperacion_donaciones > 0 ~ "Cooperacion/donaciones",
      TRUE ~ "Sin financiacion reportada"
    )
  )

base_edit_2019 <- financiacion_empresa_anio %>%
  filter(year == 2019L, !is.na(empresa_id), empresa_id != "") %>%
  mutate(
    total_privado = financiamiento_privado,
    total_publico = recursos_publicos,
    total_cooperacion = cooperacion_donaciones,
    fuente_financiacion_principal = case_when(
      total_publico > total_privado & total_publico > total_cooperacion ~ "Publica",
      total_privado > total_publico & total_privado > total_cooperacion ~ "Privada",
      total_cooperacion > total_publico & total_cooperacion > total_privado ~ "Cooperacion/donaciones",
      total_publico > 0 & total_privado > 0 & total_publico == total_privado ~ "Mixta",
      total_publico > 0 & total_privado > 0 ~ "Mixta",
      total_publico > 0 ~ "Publica",
      total_privado > 0 ~ "Privada",
      total_cooperacion > 0 ~ "Cooperacion/donaciones",
      TRUE ~ "Sin financiacion reportada"
    ),
    pct_financiacion_publica = if_else(total_financiacion_reportado > 0, total_publico / total_financiacion_reportado, NA_real_),
    pct_financiacion_privada = if_else(total_financiacion_reportado > 0, total_privado / total_financiacion_reportado, NA_real_),
    pct_financiacion_cooperacion = if_else(total_financiacion_reportado > 0, total_cooperacion / total_financiacion_reportado, NA_real_),
    pct_inversion_innovacion_ventas = if_else(ingresos_ventas_totales > 0, total_inversion_innovacion / ingresos_ventas_totales, NA_real_),
    inversion_destinos_suma = rowSums(
      across(c(
        inversion_id_interna,
        inversion_id_externa,
        inversion_maquinaria,
        inversion_tic_datos,
        inversion_mercadotecnia,
        inversion_propiedad_intelectual,
        inversion_consultoria,
        inversion_ingenieria_diseno,
        inversion_capacitacion,
        inversion_edificaciones,
        inversion_metodos_organizativos
      )),
      na.rm = TRUE
    ),
    pct_inversion_id_interna = if_else(total_inversion_innovacion > 0, inversion_id_interna / total_inversion_innovacion, NA_real_),
    pct_inversion_id_externa = if_else(total_inversion_innovacion > 0, inversion_id_externa / total_inversion_innovacion, NA_real_),
    pct_inversion_maquinaria = if_else(total_inversion_innovacion > 0, inversion_maquinaria / total_inversion_innovacion, NA_real_),
    pct_inversion_tic_datos = if_else(total_inversion_innovacion > 0, inversion_tic_datos / total_inversion_innovacion, NA_real_),
    pct_inversion_mercadotecnia = if_else(total_inversion_innovacion > 0, inversion_mercadotecnia / total_inversion_innovacion, NA_real_),
    pct_inversion_propiedad_intelectual = if_else(total_inversion_innovacion > 0, inversion_propiedad_intelectual / total_inversion_innovacion, NA_real_),
    pct_inversion_consultoria = if_else(total_inversion_innovacion > 0, inversion_consultoria / total_inversion_innovacion, NA_real_),
    pct_inversion_ingenieria_diseno = if_else(total_inversion_innovacion > 0, inversion_ingenieria_diseno / total_inversion_innovacion, NA_real_),
    pct_inversion_capacitacion = if_else(total_inversion_innovacion > 0, inversion_capacitacion / total_inversion_innovacion, NA_real_),
    pct_inversion_edificaciones = if_else(total_inversion_innovacion > 0, inversion_edificaciones / total_inversion_innovacion, NA_real_),
    pct_inversion_metodos_organizativos = if_else(total_inversion_innovacion > 0, inversion_metodos_organizativos / total_inversion_innovacion, NA_real_)
  ) %>%
  select(
    empresa_id,
    ciiu4_div,
    ciiu4_homologado,
    sector_label,
    ingresos_ventas_nacionales,
    exportaciones_totales,
    ingresos_ventas_totales,
    total_inversion_innovacion,
    total_financiacion_reportado,
    total_publico,
    total_privado,
    total_cooperacion,
    fuente_financiacion_principal,
    pct_financiacion_publica,
    pct_financiacion_privada,
    pct_financiacion_cooperacion,
    pct_inversion_innovacion_ventas,
    starts_with("inversion_"),
    starts_with("pct_inversion_")
  )

resumen_base_edit_2019 <- tibble(
  empresas_edit_2019 = n_distinct(base_edit_2019$empresa_id, na.rm = TRUE),
  empresas_con_inversion = sum(base_edit_2019$total_inversion_innovacion > 0, na.rm = TRUE),
  empresas_con_ventas_reportadas = sum(base_edit_2019$ingresos_ventas_totales > 0, na.rm = TRUE),
  ingresos_ventas_totales = sum(base_edit_2019$ingresos_ventas_totales, na.rm = TRUE),
  inversion_innovacion_2019 = sum(base_edit_2019$total_inversion_innovacion, na.rm = TRUE),
  pct_inversion_sobre_ventas = inversion_innovacion_2019 / ingresos_ventas_totales
)

tabla_fuente_edit_2019 <- base_edit_2019 %>%
  summarise(
    recursos_publicos = sum(total_publico, na.rm = TRUE),
    financiamiento_privado = sum(total_privado, na.rm = TRUE),
    cooperacion_donaciones = sum(total_cooperacion, na.rm = TRUE),
    total_financiacion = sum(total_financiacion_reportado, na.rm = TRUE)
  ) %>%
  pivot_longer(
    c(recursos_publicos, financiamiento_privado, cooperacion_donaciones),
    names_to = "fuente",
    values_to = "monto"
  ) %>%
  mutate(participacion = monto / total_financiacion)

tabla_destino_edit_2019 <- base_edit_2019 %>%
  select(
    inversion_id_interna,
    inversion_id_externa,
    inversion_maquinaria,
    inversion_tic_datos,
    inversion_mercadotecnia,
    inversion_propiedad_intelectual,
    inversion_consultoria,
    inversion_ingenieria_diseno,
    inversion_capacitacion,
    inversion_edificaciones,
    inversion_metodos_organizativos
  ) %>%
  summarise(across(everything(), ~ sum(.x, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "destino", values_to = "monto") %>%
  mutate(
    destino = recode(
      destino,
      inversion_id_interna = "I+D interna",
      inversion_id_externa = "I+D externa",
      inversion_maquinaria = "Maquinaria y equipo",
      inversion_tic_datos = "TIC, software y datos",
      inversion_mercadotecnia = "Mercadotecnia",
      inversion_propiedad_intelectual = "Propiedad intelectual",
      inversion_consultoria = "Asistencia tecnica y consultoria",
      inversion_ingenieria_diseno = "Ingenieria y diseno",
      inversion_capacitacion = "Formacion y capacitacion",
      inversion_edificaciones = "Edificaciones",
      inversion_metodos_organizativos = "Metodos organizativos"
    ),
    participacion = monto / sum(monto, na.rm = TRUE)
  ) %>%
  arrange(desc(monto))

tabla_intensidad_inversion_ventas <- base_edit_2019 %>%
  filter(ingresos_ventas_totales > 0) %>%
  summarise(
    empresas = n(),
    ingresos_ventas_totales = sum(ingresos_ventas_totales, na.rm = TRUE),
    inversion_innovacion_total = sum(total_inversion_innovacion, na.rm = TRUE),
    pct_inversion_sobre_ventas_agregado = inversion_innovacion_total / ingresos_ventas_totales,
    mediana_pct_empresa = median(pct_inversion_innovacion_ventas, na.rm = TRUE),
    promedio_pct_empresa = mean(pct_inversion_innovacion_ventas, na.rm = TRUE),
    p90_pct_empresa = quantile(pct_inversion_innovacion_ventas, 0.90, na.rm = TRUE, names = FALSE)
  )

resumen_nacional <- financiacion_empresa_anio %>%
  group_by(year) %>%
  summarise(
    empresas = n(),
    empresas_con_inversion = sum(total_inversion_innovacion > 0, na.rm = TRUE),
    inversion_total = sum(total_inversion_innovacion, na.rm = TRUE),
    financiamiento_total = sum(total_financiacion_reportado, na.rm = TRUE),
    recursos_publicos = sum(recursos_publicos, na.rm = TRUE),
    financiamiento_privado = sum(financiamiento_privado, na.rm = TRUE),
    cooperacion_donaciones = sum(cooperacion_donaciones, na.rm = TRUE),
    participacion_publica = recursos_publicos / financiamiento_total,
    participacion_privada = financiamiento_privado / financiamiento_total,
    participacion_cooperacion = cooperacion_donaciones / financiamiento_total,
    .groups = "drop"
  )

resumen_sector <- financiacion_empresa_anio %>%
  filter(!is.na(ciiu4_div), ciiu4_div != "") %>%
  group_by(year, ciiu4_div, sector_label) %>%
  summarise(
    empresas = n(),
    inversion_total = sum(total_inversion_innovacion, na.rm = TRUE),
    recursos_publicos = sum(recursos_publicos, na.rm = TRUE),
    financiamiento_privado = sum(financiamiento_privado, na.rm = TRUE),
    cooperacion_donaciones = sum(cooperacion_donaciones, na.rm = TRUE),
    financiamiento_total = sum(total_financiacion_reportado, na.rm = TRUE),
    participacion_publica = recursos_publicos / financiamiento_total,
    participacion_privada = financiamiento_privado / financiamiento_total,
    .groups = "drop"
  ) %>%
  arrange(year, desc(inversion_total))

detalle_financiacion_publica <- financiacion_empresa_anio %>%
  group_by(year) %>%
  summarise(
    MinCIT_INNpulsa_Colombia_Productiva = sum(mincit_innpulsa_colombia_productiva, na.rm = TRUE),
    SENA = sum(sena, na.rm = TRUE),
    MinCiencias = sum(minciencias, na.rm = TRUE),
    MinTIC = sum(mintic, na.rm = TRUE),
    Otra_entidad_publica = sum(otra_entidad_publica, na.rm = TRUE),
    BANCOLDEX = sum(bancoldex, na.rm = TRUE),
    FINAGRO = sum(finagro, na.rm = TRUE),
    Fondos_territoriales = sum(fondos_territoriales, na.rm = TRUE),
    SGR_CTI = sum(sgr_cti, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(-year, names_to = "fuente_publica", values_to = "monto")

detalle_financiacion_privada <- financiacion_empresa_anio %>%
  group_by(year) %>%
  summarise(
    Recursos_propios = sum(recursos_propios, na.rm = TRUE),
    Empresas_del_grupo = sum(recursos_grupo, na.rm = TRUE),
    Banca_privada = sum(banca_privada, na.rm = TRUE),
    Otras_empresas = sum(otras_empresas, na.rm = TRUE),
    Fondos_de_capital_privado = sum(capital_privado, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(-year, names_to = "fuente_privada", values_to = "monto")

fuentes_nacionales <- resumen_nacional %>%
  select(year, recursos_publicos, financiamiento_privado, cooperacion_donaciones) %>%
  pivot_longer(-year, names_to = "fuente", values_to = "monto") %>%
  mutate(
    fuente = recode(
      fuente,
      recursos_publicos = "Recursos publicos",
      financiamiento_privado = "Recursos privados",
      cooperacion_donaciones = "Cooperacion y donaciones"
    )
  )

acceso_publico <- financiacion_empresa_anio %>%
  group_by(year) %>%
  summarise(
    empresas = n(),
    intento_solicitar_recursos_publicos = mean(intencion_solicitar_publicos == 1, na.rm = TRUE),
    recibio_recursos_publicos = mean(recursos_publicos > 0, na.rm = TRUE),
    obtuvo_beneficio_tributario = mean(beneficio_tributario == 1, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(-c(year, empresas), names_to = "indicador", values_to = "proporcion") %>%
  mutate(
    indicador = recode(
      indicador,
      intento_solicitar_recursos_publicos = "Intento solicitar recursos publicos",
      recibio_recursos_publicos = "Recibio recursos publicos",
      obtuvo_beneficio_tributario = "Obtuvo beneficio tributario"
    )
  )

barreras_publicas <- financiacion_empresa_anio %>%
  summarise(
    Desconocimiento_de_lineas = mean(barrera_desconocimiento == 1, na.rm = TRUE),
    Falta_de_informacion_sobre_requisitos = mean(barrera_requisitos == 1, na.rm = TRUE),
    Dificultad_para_cumplir_tramites = mean(barrera_tramites == 1, na.rm = TRUE),
    Tiempo_de_tramite_excesivo = mean(barrera_tiempo == 1, na.rm = TRUE),
    Condiciones_poco_atractivas = mean(barrera_condiciones == 1, na.rm = TRUE),
    Demora_en_intermediacion_bancaria = mean(barrera_intermediacion == 1, na.rm = TRUE)
  ) %>%
  pivot_longer(everything(), names_to = "barrera", values_to = "proporcion_alta") %>%
  mutate(barrera = str_replace_all(barrera, "_", " ")) %>%
  arrange(proporcion_alta)

destino_inversion <- financiacion_empresa_anio %>%
  select(
    year,
    inversion_id_interna,
    inversion_id_externa,
    inversion_maquinaria,
    inversion_tic_datos,
    inversion_mercadotecnia,
    inversion_propiedad_intelectual,
    inversion_consultoria,
    inversion_ingenieria_diseno,
    inversion_capacitacion,
    inversion_edificaciones,
    inversion_metodos_organizativos
  ) %>%
  pivot_longer(-year, names_to = "destino", values_to = "monto") %>%
  group_by(year, destino) %>%
  summarise(monto = sum(monto, na.rm = TRUE), .groups = "drop") %>%
  group_by(year) %>%
  mutate(participacion = monto / sum(monto, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    destino = recode(
      destino,
      inversion_id_interna = "I+D interna",
      inversion_id_externa = "I+D externa",
      inversion_maquinaria = "Maquinaria y equipo",
      inversion_tic_datos = "TIC, software y datos",
      inversion_mercadotecnia = "Mercadotecnia",
      inversion_propiedad_intelectual = "Propiedad intelectual",
      inversion_consultoria = "Asistencia tecnica y consultoria",
      inversion_ingenieria_diseno = "Ingenieria y diseno",
      inversion_capacitacion = "Formacion y capacitacion",
      inversion_edificaciones = "Edificaciones",
      inversion_metodos_organizativos = "Metodos organizativos"
    )
  )

financiacion_empresa_bienio <- financiacion_empresa_anio %>%
  group_by(empresa_id, ciiu4_div, sector_label) %>%
  summarise(
    total_inversion_innovacion = sum(total_inversion_innovacion, na.rm = TRUE),
    total_financiacion_reportado = sum(total_financiacion_reportado, na.rm = TRUE),
    recursos_publicos = sum(recursos_publicos, na.rm = TRUE),
    financiamiento_privado = sum(financiamiento_privado, na.rm = TRUE),
    cooperacion_donaciones = sum(cooperacion_donaciones, na.rm = TRUE),
    impacto_productividad = first_nonmissing(impacto_productividad),
    impacto_ventas = first_nonmissing(impacto_ventas),
    impacto_utilidades = first_nonmissing(impacto_utilidades),
    impacto_gestion = first_nonmissing(impacto_gestion),
    .groups = "drop"
  ) %>%
  filter(total_inversion_innovacion > 0 | total_financiacion_reportado > 0) %>%
  mutate(
    tipo_financiacion = case_when(
      recursos_publicos > 0 & financiamiento_privado > 0 ~ "Mixta",
      recursos_publicos > 0 ~ "Publica",
      financiamiento_privado > 0 ~ "Privada",
      cooperacion_donaciones > 0 ~ "Cooperacion/donaciones",
      TRUE ~ "Sin financiacion reportada"
    )
  )

resultados_por_fuente <- financiacion_empresa_bienio %>%
  filter(tipo_financiacion %in% c("Publica", "Privada", "Mixta")) %>%
  group_by(tipo_financiacion) %>%
  summarise(
    n_empresas = n(),
    Aumento_productividad = mean(impacto_productividad == 1, na.rm = TRUE),
    Aumento_ventas = mean(impacto_ventas == 1, na.rm = TRUE),
    Aumento_utilidades = mean(impacto_utilidades == 1, na.rm = TRUE),
    Mejora_gestion = mean(impacto_gestion == 1, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(-c(tipo_financiacion, n_empresas), names_to = "resultado", values_to = "proporcion_alta") %>%
  mutate(
    resultado = str_replace_all(resultado, "_", " "),
    tipo_financiacion_plot = paste0(tipo_financiacion, " (n=", n_empresas, ")")
  )

concentracion_financiacion <- financiacion_empresa_anio %>%
  filter(total_financiacion_reportado > 0) %>%
  group_by(year) %>%
  arrange(desc(total_financiacion_reportado), .by_group = TRUE) %>%
  mutate(rango = row_number()) %>%
  summarise(
    empresas_financiadas = n(),
    participacion_top_10 = sum(total_financiacion_reportado[rango <= 10], na.rm = TRUE) / sum(total_financiacion_reportado, na.rm = TRUE),
    participacion_top_1 = first(total_financiacion_reportado) / sum(total_financiacion_reportado, na.rm = TRUE),
    .groups = "drop"
  )

top_sectores <- resumen_sector %>%
  filter(year == 2020) %>%
  slice_max(inversion_total, n = 12, with_ties = FALSE) %>%
  pull(ciiu4_div)

grafico_nacional <- ggplot(fuentes_nacionales, aes(x = factor(year), y = monto, fill = fuente)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = percent) +
  labs(
    title = "Quien financia la inversion en innovacion y tecnologia?",
    subtitle = "EDIT Industria 2019-2020. Participacion sobre la financiacion reportada.",
    x = NULL,
    y = "Participacion",
    fill = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"), legend.position = "bottom")

grafico_sector <- resumen_sector %>%
  filter(year == 2020, ciiu4_div %in% top_sectores) %>%
  select(sector_label, recursos_publicos, financiamiento_privado, cooperacion_donaciones) %>%
  pivot_longer(-sector_label, names_to = "fuente", values_to = "monto") %>%
  mutate(
    fuente = recode(
      fuente,
      recursos_publicos = "Recursos publicos",
      financiamiento_privado = "Recursos privados",
      cooperacion_donaciones = "Cooperacion y donaciones"
    )
  ) %>%
  ggplot(aes(x = reorder(sector_label, monto, FUN = sum), y = monto, fill = fuente)) +
  geom_col(position = "fill") +
  coord_flip() +
  scale_y_continuous(labels = percent) +
  labs(
    title = "Financiacion de la innovacion por sector, 2020",
    subtitle = "Doce divisiones CIIU con mayor inversion en innovacion.",
    x = NULL,
    y = "Participacion",
    fill = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"), legend.position = "bottom")

grafico_publico <- detalle_financiacion_publica %>%
  filter(monto > 0) %>%
  ggplot(aes(x = factor(year), y = monto, fill = fuente_publica)) +
  geom_col() +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Composicion de los recursos publicos para innovacion",
    subtitle = "EDIT Industria 2019-2020. Valores en miles de pesos corrientes.",
    x = NULL,
    y = "Monto",
    fill = "Fuente publica"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"), legend.position = "bottom")

grafico_privado <- detalle_financiacion_privada %>%
  filter(monto > 0) %>%
  ggplot(aes(x = factor(year), y = monto, fill = fuente_privada)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = percent) +
  labs(
    title = "Composicion del financiamiento privado de la innovacion",
    subtitle = "EDIT Industria 2019-2020. Participacion dentro de las fuentes privadas.",
    x = NULL,
    y = "Participacion",
    fill = "Fuente privada"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"), legend.position = "bottom")

grafico_acceso_publico <- ggplot(acceso_publico, aes(x = indicador, y = proporcion, fill = factor(year))) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65) +
  scale_y_continuous(labels = percent, limits = c(0, 1)) +
  labs(
    title = "Acceso empresarial al financiamiento publico",
    subtitle = "EDIT Industria 2019-2020.",
    x = NULL,
    y = "Proporcion de empresas",
    fill = "Anio"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"), axis.text.x = element_text(angle = 15, hjust = 1), legend.position = "bottom")

grafico_barreras <- ggplot(barreras_publicas, aes(x = reorder(barrera, proporcion_alta), y = proporcion_alta)) +
  geom_col(fill = "#D97706") +
  coord_flip() +
  scale_y_continuous(labels = percent, limits = c(0, 1)) +
  labs(
    title = "Principales barreras para acceder a apoyo publico",
    subtitle = "Proporcion de empresas que reporta una barrera alta, EDIT Industria 2019-2020.",
    x = NULL,
    y = "Proporcion de empresas"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

grafico_destino <- ggplot(destino_inversion, aes(x = factor(year), y = participacion, fill = destino)) +
  geom_col() +
  scale_y_continuous(labels = percent) +
  labs(
    title = "Destino de la inversion en ACTI",
    subtitle = "Composicion de la inversion reportada por las empresas.",
    x = NULL,
    y = "Participacion de la inversion",
    fill = "Destino"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"), legend.position = "bottom")

grafico_resultados <- ggplot(resultados_por_fuente, aes(x = resultado, y = proporcion_alta, fill = tipo_financiacion_plot)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65) +
  scale_y_continuous(labels = percent, limits = c(0, 1)) +
  labs(
    title = "Resultados de innovacion segun fuente de financiacion",
    subtitle = "Proporcion de empresas que reporta impacto alto.",
    x = NULL,
    y = "Proporcion de empresas",
    fill = "Fuente de financiacion"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"), axis.text.x = element_text(angle = 15, hjust = 1), legend.position = "bottom")

grafico_concentracion <- ggplot(concentracion_financiacion, aes(x = factor(year), y = participacion_top_10)) +
  geom_col(fill = "#7C3AED", width = 0.6) +
  geom_text(aes(label = percent(participacion_top_10, accuracy = 0.1)), vjust = -0.35, fontface = "bold") +
  scale_y_continuous(labels = percent, limits = c(0, 1.05)) +
  labs(
    title = "Concentracion del financiamiento de ACTI",
    subtitle = "Participacion de las diez empresas con mayor financiamiento reportado.",
    x = NULL,
    y = "Participacion del financiamiento total"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

cat("\n=== 0. Resumen EDIT 2019 ===\n")
print(resumen_base_edit_2019)
cat("\n=== 0.1. Fuente de financiacion de la inversion en innovacion, EDIT 2019 ===\n")
print(tabla_fuente_edit_2019)
cat("\n=== 0.2. Destino de la inversion ACTI, EDIT 2019 ===\n")
print(tabla_destino_edit_2019)
cat("\n=== 0.3. Inversion en innovacion como porcentaje de ingresos/ventas, EDIT 2019 ===\n")
print(tabla_intensidad_inversion_ventas)

cat("\n=== 1. Panorama nacional EDIT 2019-2020 ===\n")
print(resumen_nacional)
cat("\n=== 2. Financiamiento por sector ===\n")
print(resumen_sector)
cat("\n=== 3. Acceso, barreras y concentracion ===\n")
print(acceso_publico)
print(barreras_publicas)
print(concentracion_financiacion)
print(grafico_nacional)
print(grafico_sector)
print(grafico_publico)
print(grafico_privado)
print(grafico_acceso_publico)
print(grafico_barreras)
print(grafico_destino)
print(grafico_resultados)
print(grafico_concentracion)

message("Listo. Los objetos de fuentes, acceso, barreras, destinos, resultados y concentracion quedan disponibles en memoria.")
