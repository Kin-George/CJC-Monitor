# ==============================================================================
# EDIT Servicios 2020-2021 - Financiacion de innovacion y tecnologia
# ==============================================================================
# Preguntas:
#   1. La fuente de financiacion para inversion en innovacion es publica o privada?
#   2. Este dinero invertido en innovacion para que es?
#   3. Que porcentaje de los ingresos/ventas se invierte en innovacion?
#
# Usa solamente la base cruda:
#   Datos/Raw/EDIT-S/Estructura_EDITS_VIII_2020_2021.dta
#
# No guarda bases ni archivos. Imprime tablas en R/RStudio.
# ==============================================================================

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(readxl)
  library(stringr)
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

clean_varname <- function(x) {
  out <- toupper(trimws(as.character(x)))
  out <- iconv(out, from = "", to = "ASCII//TRANSLIT")
  out <- gsub("[^A-Z0-9]+", "_", out)
  out <- gsub("^_+|_+$", "", out)
  out[out == ""] <- "VAR"
  out <- ifelse(grepl("^[0-9]", out), paste0("X", out), out)
  make.unique(out, sep = "_")
}

load_edits_dictionary <- function(path, period = "2020_2021") {
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
      "El diccionario 2020_2021 tiene ", nrow(dict_period),
      " variables, pero la base cruda EDITS tiene ", ncol(data),
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

detect_nordemp_var <- function(data) {
  nms <- names(data)
  hit <- nms[str_detect(str_to_upper(nms), "^NORDEMP")]
  if (length(hit) == 0) return(NA_character_)
  hit[1]
}

project_root <- find_project_root()
raw_edits_dir <- file.path(project_root, "Datos", "Raw", "EDIT-S")
doc_dir <- file.path(project_root, "DocumentacionAuxiliar")
dict_dir <- file.path(project_root, "Diccionarios", "EDIT-S")

edits_raw_path <- file.path(raw_edits_dir, "Estructura_EDITS_VIII_2020_2021.dta")
dictionary_path <- file.path(dict_dir, "EDITS_Diccionarios_Consolidado.xlsx")
ciiu4_structure_path <- file.path(doc_dir, "Estructura-detallada-CIIU-4AC-2022.xlsx")

if (!file.exists(edits_raw_path)) stop("No existe la base cruda EDITS 2020-2021: ", edits_raw_path)
if (!file.exists(dictionary_path)) stop("No existe el diccionario consolidado EDITS: ", dictionary_path)
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
  "I3R1C1", "I3R1C2", "I3R2C1", "I3R2C2", # Ingresos/ventas nacionales y exportaciones
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
  "II1R11C1", "II1R11C2", "II1R12C1", "II1R12C2"
)

dict_2020_2021 <- load_edits_dictionary(dictionary_path, period = "2020_2021")

edits_raw <- read_dta(edits_raw_path)
edits_raw <- apply_dictionary_names(edits_raw, dict_2020_2021)
message("Fuente de nombres EDITS: ", attr(edits_raw, "dictionary_name_source"))

ciiu_var <- detect_ciiu_var(edits_raw)
if (is.na(ciiu_var)) warning("No se encontro una variable CIIU reconocible en EDITS.")

nordemp_var <- detect_nordemp_var(edits_raw)
if (is.na(nordemp_var)) warning("No se encontro una variable NORDEMP reconocible en EDITS.")
message("Variable de identificacion empresarial usada: ", ifelse(is.na(nordemp_var), "ninguna", nordemp_var))

edits_raw <- edits_raw %>%
  mutate(
    edit_period = "2020_2021",
    nordemp = if (!is.na(nordemp_var)) normalize_id(.data[[nordemp_var]]) else NA_character_,
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

missing_vars <- setdiff(vars_needed, names(edits_raw))
if (length(missing_vars) > 0) warning("Variables no encontradas en EDITS: ", paste(missing_vars, collapse = ", "))

edits_2020_2021 <- edits_raw %>%
  select(any_of(vars_needed), empresa_id, ciiu_original) %>%
  mutate(ciiu4_div = as.character(ciiu4_div)) %>%
  left_join(ciiu4_div_labels, by = "ciiu4_div")

financiacion_empresa_anio <- bind_rows(
  edits_2020_2021 %>%
    transmute(
      year = 2020L,
      empresa_id,
      ciiu4_div,
      ciiu4_homologado = as.character(ciiu4_homologado),
      sector_label,
      ingresos_ventas_nacionales = to_numeric_safe(I3R1C1),
      exportaciones_totales = to_numeric_safe(I3R1C2),
      recursos_propios = to_numeric_safe(III1R1C1),
      recursos_grupo = to_numeric_safe(III1R2C1),
      recursos_publicos = to_numeric_safe(III1R3C1),
      banca_privada = to_numeric_safe(III1R4C1) + to_numeric_safe(III1R4C2),
      otras_empresas = to_numeric_safe(III1R5C1) + to_numeric_safe(III1R5C2),
      capital_privado = to_numeric_safe(III1R6C1) + to_numeric_safe(III1R6C2),
      cooperacion_donaciones = to_numeric_safe(III1R7C1) + to_numeric_safe(III1R7C2),
      total_financiacion_reportado = to_numeric_safe(III1R8C1),
      total_inversion_innovacion = to_numeric_safe(II1R10C1),
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
      inversion_metodos_organizativos = to_numeric_safe(II1R12C1)
    ),
  edits_2020_2021 %>%
    transmute(
      year = 2021L,
      empresa_id,
      ciiu4_div,
      ciiu4_homologado = as.character(ciiu4_homologado),
      sector_label,
      ingresos_ventas_nacionales = to_numeric_safe(I3R2C1),
      exportaciones_totales = to_numeric_safe(I3R2C2),
      recursos_propios = to_numeric_safe(III1R1C2),
      recursos_grupo = to_numeric_safe(III1R2C2),
      recursos_publicos = to_numeric_safe(III1R3C2),
      banca_privada = to_numeric_safe(III1R4C3) + to_numeric_safe(III1R4C4),
      otras_empresas = to_numeric_safe(III1R5C3) + to_numeric_safe(III1R5C4),
      capital_privado = to_numeric_safe(III1R6C3) + to_numeric_safe(III1R6C4),
      cooperacion_donaciones = to_numeric_safe(III1R7C3) + to_numeric_safe(III1R7C4),
      total_financiacion_reportado = to_numeric_safe(III1R8C2),
      total_inversion_innovacion = to_numeric_safe(II1R10C2),
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
      inversion_metodos_organizativos = to_numeric_safe(II1R12C2)
    )
) %>%
  mutate(
    ingresos_ventas_totales = case_when(
      is.na(ingresos_ventas_nacionales) & is.na(exportaciones_totales) ~ NA_real_,
      TRUE ~ replace_na(ingresos_ventas_nacionales, 0) + replace_na(exportaciones_totales, 0)
    ),
    across(
      c(
        recursos_propios, recursos_grupo, recursos_publicos, banca_privada,
        otras_empresas, capital_privado, cooperacion_donaciones,
        inversion_id_interna, inversion_id_externa, inversion_maquinaria,
        inversion_tic_datos, inversion_mercadotecnia, inversion_propiedad_intelectual,
        inversion_consultoria, inversion_ingenieria_diseno, inversion_capacitacion,
        inversion_edificaciones, inversion_metodos_organizativos
      ),
      ~ replace_na(.x, 0)
    ),
    financiamiento_privado = recursos_propios + recursos_grupo + banca_privada + otras_empresas + capital_privado,
    financiamiento_clasificado = financiamiento_privado + recursos_publicos + cooperacion_donaciones,
    diferencia_con_total_reportado = total_financiacion_reportado - financiamiento_clasificado,
    participacion_publica = if_else(total_financiacion_reportado > 0, recursos_publicos / total_financiacion_reportado, NA_real_),
    participacion_privada = if_else(total_financiacion_reportado > 0, financiamiento_privado / total_financiacion_reportado, NA_real_),
    participacion_cooperacion = if_else(total_financiacion_reportado > 0, cooperacion_donaciones / total_financiacion_reportado, NA_real_),
    pct_inversion_innovacion_ventas = if_else(ingresos_ventas_totales > 0, total_inversion_innovacion / ingresos_ventas_totales, NA_real_),
    fuente_financiacion_principal = case_when(
      recursos_publicos > 0 & financiamiento_privado > 0 ~ "Mixta",
      recursos_publicos > 0 ~ "Publica",
      financiamiento_privado > 0 ~ "Privada",
      cooperacion_donaciones > 0 ~ "Cooperacion/donaciones",
      TRUE ~ "Sin financiacion reportada"
    )
  )

tabla_resumen <- financiacion_empresa_anio %>%
  group_by(year) %>%
  summarise(
    empresas = n_distinct(empresa_id, na.rm = TRUE),
    empresas_con_inversion = sum(total_inversion_innovacion > 0, na.rm = TRUE),
    empresas_con_ventas_reportadas = sum(ingresos_ventas_totales > 0, na.rm = TRUE),
    ingresos_ventas_totales = sum(ingresos_ventas_totales, na.rm = TRUE),
    inversion_innovacion = sum(total_inversion_innovacion, na.rm = TRUE),
    pct_inversion_sobre_ventas = inversion_innovacion / ingresos_ventas_totales,
    .groups = "drop"
  )

tabla_fuente <- financiacion_empresa_anio %>%
  group_by(year) %>%
  summarise(
    recursos_publicos = sum(recursos_publicos, na.rm = TRUE),
    financiamiento_privado = sum(financiamiento_privado, na.rm = TRUE),
    cooperacion_donaciones = sum(cooperacion_donaciones, na.rm = TRUE),
    total_financiacion = sum(total_financiacion_reportado, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    c(recursos_publicos, financiamiento_privado, cooperacion_donaciones),
    names_to = "fuente",
    values_to = "monto"
  ) %>%
  mutate(participacion = monto / total_financiacion) %>%
  arrange(year, desc(monto))

tabla_fuente_principal_empresas <- financiacion_empresa_anio %>%
  count(year, fuente_financiacion_principal, name = "empresas") %>%
  group_by(year) %>%
  mutate(participacion_empresas = empresas / sum(empresas)) %>%
  ungroup() %>%
  arrange(year, desc(empresas))

tabla_destino <- financiacion_empresa_anio %>%
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
  ) %>%
  arrange(year, desc(monto))

tabla_intensidad_inversion_ventas <- financiacion_empresa_anio %>%
  filter(ingresos_ventas_totales > 0) %>%
  group_by(year) %>%
  summarise(
    empresas = n(),
    ingresos_ventas_totales = sum(ingresos_ventas_totales, na.rm = TRUE),
    inversion_innovacion_total = sum(total_inversion_innovacion, na.rm = TRUE),
    pct_inversion_sobre_ventas_agregado = inversion_innovacion_total / ingresos_ventas_totales,
    mediana_pct_empresa = median(pct_inversion_innovacion_ventas, na.rm = TRUE),
    promedio_pct_empresa = mean(pct_inversion_innovacion_ventas, na.rm = TRUE),
    p90_pct_empresa = quantile(pct_inversion_innovacion_ventas, 0.90, na.rm = TRUE, names = FALSE),
    .groups = "drop"
  )

tabla_fuente_sector <- financiacion_empresa_anio %>%
  filter(!is.na(ciiu4_div), ciiu4_div != "") %>%
  group_by(year, ciiu4_div, sector_label) %>%
  summarise(
    empresas = n_distinct(empresa_id, na.rm = TRUE),
    inversion_innovacion = sum(total_inversion_innovacion, na.rm = TRUE),
    recursos_publicos = sum(recursos_publicos, na.rm = TRUE),
    financiamiento_privado = sum(financiamiento_privado, na.rm = TRUE),
    cooperacion_donaciones = sum(cooperacion_donaciones, na.rm = TRUE),
    total_financiacion = sum(total_financiacion_reportado, na.rm = TRUE),
    pct_publico = recursos_publicos / total_financiacion,
    pct_privado = financiamiento_privado / total_financiacion,
    .groups = "drop"
  ) %>%
  arrange(year, desc(inversion_innovacion))

cat("\n=== EDIT Servicios 2020-2021: resumen general ===\n")
print(tabla_resumen)

cat("\n=== Fuente de financiacion: montos y participaciones ===\n")
print(tabla_fuente)

cat("\n=== Fuente principal de financiacion: numero de empresas ===\n")
print(tabla_fuente_principal_empresas)

cat("\n=== Destino de la inversion ACTI ===\n")
print(tabla_destino)

cat("\n=== Inversion en innovacion como porcentaje de ingresos/ventas ===\n")
print(tabla_intensidad_inversion_ventas)

cat("\n=== Fuente de financiacion por sector CIIU Rev. 4 ===\n")
print(tabla_fuente_sector)

message("Listo. Las tablas quedan disponibles en memoria.")
