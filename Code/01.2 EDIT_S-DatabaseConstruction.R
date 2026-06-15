# ==============================================================================
# EDIT Servicios - Construccion de panel consolidado
# ==============================================================================

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(purrr)
  library(readxl)
  library(stringr)
  library(tibble)
  library(readr)
  library(writexl)
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

clean_varname <- function(x) {
  out <- toupper(trimws(as.character(x)))
  out <- iconv(out, from = "", to = "ASCII//TRANSLIT")
  out <- gsub("[^A-Z0-9]+", "_", out)
  out <- gsub("^_+|_+$", "", out)
  out[out == ""] <- "VAR"
  out <- ifelse(grepl("^[0-9]", out), paste0("X", out), out)
  make.unique(out, sep = "_")
}

extract_years <- function(file_name) {
  yrs <- str_extract_all(file_name, "(19|20)[0-9]{2}")[[1]]
  yrs <- as.integer(yrs)
  if (length(yrs) >= 2) return(list(start = min(yrs), end = max(yrs), period = paste0(min(yrs), "_", max(yrs))))
  if (length(yrs) == 1) return(list(start = yrs[1], end = yrs[1], period = as.character(yrs[1])))
  list(start = NA_integer_, end = NA_integer_, period = NA_character_)
}

num_from_text <- function(x, width = NULL) {
  out <- str_extract(as.character(x), "[0-9]+")
  out <- ifelse(is.na(out), NA_character_, out)
  if (!is.null(width)) out <- ifelse(is.na(out), NA_character_, str_pad(out, width = width, pad = "0"))
  out
}

modal_first <- function(x) {
  x <- x[!is.na(x) & x != ""]
  if (length(x) == 0) return(NA_character_)
  names(sort(table(x), decreasing = TRUE))[1]
}

build_ciiu_bridge <- function(path) {
  raw <- read_excel(path, sheet = 1, skip = 10, col_names = TRUE, col_types = "text")
  names(raw) <- paste0("COL", seq_along(raw))

  bridge <- raw %>%
    transmute(
      rev3_div = num_from_text(COL1, 2),
      rev3_group = num_from_text(COL2, 3),
      rev3_class = num_from_text(COL3, 4),
      rev4_div = num_from_text(COL5, 2),
      rev4_group = num_from_text(COL6, 3),
      rev4_class = num_from_text(COL7, 4)
    )

  list(
    class = bridge %>%
      filter(!is.na(rev3_class), !is.na(rev4_class)) %>%
      group_by(rev3_class) %>%
      summarise(ciiu4_hom = modal_first(rev4_class), n_destinos_clase = n_distinct(rev4_class), .groups = "drop"),
    group = bridge %>%
      filter(!is.na(rev3_group), !is.na(rev4_group)) %>%
      group_by(rev3_group) %>%
      summarise(ciiu4_group_hom = modal_first(rev4_group), n_destinos_grupo = n_distinct(rev4_group), .groups = "drop"),
    div = bridge %>%
      filter(!is.na(rev3_div), !is.na(rev4_div)) %>%
      group_by(rev3_div) %>%
      summarise(ciiu4_div_hom = modal_first(rev4_div), n_destinos_div = n_distinct(rev4_div), .groups = "drop")
  )
}

project_root <- find_project_root()
raw_dir <- file.path(project_root, "Datos", "Raw", "EDIT-S")
processed_dir <- file.path(project_root, "Datos", "Processed")
doc_dir <- file.path(project_root, "DocumentacionAuxiliar")
dict_dir <- file.path(project_root, "Diccionarios", "EDIT-S")
dir.create(processed_dir, recursive = TRUE, showWarnings = FALSE)

correlativa_path <- file.path(doc_dir, "TC-CIIU-3ACvsCIIU-4AC-2020.xlsx")
dictionary_path <- file.path(dict_dir, "EDITS_Diccionarios_Consolidado.xlsx")
output_path <- file.path(processed_dir, "EDITS_Panel.dta")
audit_path <- file.path(processed_dir, "EDITS_Panel_auditoria_archivos.csv")
vars_path <- file.path(processed_dir, "EDITS_Panel_diccionario_variables.csv")
vars_xlsx_path <- file.path(processed_dir, "EDITS_Panel_diccionario_variables.xlsx")
ciiu_audit_path <- file.path(processed_dir, "EDITS_Panel_auditoria_ciiu.csv")

if (!dir.exists(raw_dir)) stop("No existe la carpeta de datos EDIT-S: ", raw_dir)
if (!file.exists(correlativa_path)) stop("No existe la tabla correlativa CIIU: ", correlativa_path)
if (!file.exists(dictionary_path)) stop("No existe el diccionario consolidado EDIT-S: ", dictionary_path)

ciiu_bridge <- build_ciiu_bridge(correlativa_path)

edit_dictionary <- read_excel(dictionary_path, sheet = "variables", col_types = "text") %>%
  transmute(
    edit_period = as.character(period),
    year_start_dic = suppressWarnings(as.integer(year_start)),
    year_end_dic = suppressWarnings(as.integer(year_end)),
    catalog_id = as.character(catalog_id),
    file_id = as.character(file_id),
    file_name_dic = as.character(file_name),
    source_dictionary_url = as.character(source_dictionary_url),
    order_global = suppressWarnings(as.integer(order_global)),
    variable_original_dic = as.character(variable),
    label_dane = as.character(label),
    variable_id_dane = as.character(variable_id),
    variable_url_dane = as.character(variable_url),
    page_url_dane = as.character(page_url)
  ) %>%
  group_by(edit_period) %>%
  mutate(variable_dic = clean_varname(variable_original_dic)) %>%
  ungroup() %>%
  arrange(edit_period, order_global)

get_dictionary_for_period <- function(period) {
  edit_dictionary %>% filter(edit_period == period) %>% arrange(order_global)
}

as_plain_character <- function(x) {
  if (inherits(x, "haven_labelled")) return(as.character(as_factor(x, levels = "values")))
  as.character(x)
}

coerce_for_panel <- function(data) {
  data %>%
    mutate(across(everything(), ~ if (inherits(.x, "haven_labelled")) zap_labels(.x) else .x))
}

apply_dictionary_names <- function(data, dict_period) {
  raw_names <- names(data)
  if (nrow(dict_period) == ncol(data)) {
    names(data) <- make.unique(dict_period$variable_dic, sep = "_")
    attr(data, "dictionary_name_source") <- "diccionario_dane_por_orden"
  } else {
    names(data) <- clean_varname(raw_names)
    attr(data, "dictionary_name_source") <- "nombres_microdato_limpios"
  }
  attr(data, "raw_names") <- raw_names
  data
}

detect_ciiu <- function(data) {
  nms <- names(data)
  candidates <- c("CIIU4", "CIIU_4", "CIIU_REV4", "CIIU3", "CIIU_3", "ACT3", "ACT", "CIIU")
  var <- candidates[candidates %in% nms][1]
  rev <- case_when(
    is.na(var) ~ NA_integer_,
    str_detect(var, "4") ~ 4L,
    TRUE ~ 3L
  )
  list(var = var, rev = rev)
}

normalize_ciiu_code <- function(x) {
  out <- str_extract(as.character(x), "[0-9]+")
  ifelse(out == "", NA_character_, out)
}

add_ciiu_homologation <- function(data, bridge) {
  data %>%
    mutate(
      ciiu_original = normalize_ciiu_code(ciiu_original),
      ciiu_digits = nchar(ciiu_original),
      ciiu3_class = if_else(ciiu_revision_original == 3L & ciiu_digits >= 4, str_sub(str_pad(ciiu_original, 4, pad = "0"), 1, 4), NA_character_),
      ciiu3_group = if_else(ciiu_revision_original == 3L & ciiu_digits >= 3, str_sub(str_pad(ciiu_original, 3, pad = "0"), 1, 3), NA_character_),
      ciiu3_div = if_else(ciiu_revision_original == 3L & ciiu_digits >= 2, str_sub(str_pad(ciiu_original, 2, pad = "0"), 1, 2), NA_character_),
      ciiu4_class_direct = if_else(ciiu_revision_original == 4L & ciiu_digits >= 4, str_sub(str_pad(ciiu_original, 4, pad = "0"), 1, 4), NA_character_),
      ciiu4_group_direct = case_when(
        ciiu_revision_original == 4L & ciiu_digits == 3 ~ str_pad(ciiu_original, 3, pad = "0"),
        ciiu_revision_original == 4L & ciiu_digits >= 4 ~ str_sub(str_pad(ciiu_original, 4, pad = "0"), 1, 3),
        TRUE ~ NA_character_
      ),
      ciiu4_div_direct = case_when(
        ciiu_revision_original == 4L & ciiu_digits == 2 ~ str_pad(ciiu_original, 2, pad = "0"),
        ciiu_revision_original == 4L & ciiu_digits >= 3 ~ str_sub(str_pad(ciiu_original, 3, pad = "0"), 1, 2),
        TRUE ~ NA_character_
      )
    ) %>%
    left_join(bridge$class, by = c("ciiu3_class" = "rev3_class")) %>%
    left_join(bridge$group, by = c("ciiu3_group" = "rev3_group")) %>%
    left_join(bridge$div, by = c("ciiu3_div" = "rev3_div")) %>%
    mutate(
      ciiu4_homologado = coalesce(ciiu4_class_direct, ciiu4_group_direct, ciiu4_div_direct, ciiu4_hom, ciiu4_group_hom, ciiu4_div_hom),
      ciiu4_nivel_homologacion = case_when(
        !is.na(ciiu4_class_direct) ~ "clase_rev4_directa",
        !is.na(ciiu4_group_direct) ~ "grupo_rev4_directo",
        !is.na(ciiu4_div_direct) ~ "division_rev4_directa",
        !is.na(ciiu4_hom) ~ "clase_rev3_a_rev4",
        !is.na(ciiu4_group_hom) ~ "grupo_rev3_a_rev4",
        !is.na(ciiu4_div_hom) ~ "division_rev3_a_rev4",
        TRUE ~ "sin_homologar"
      ),
      ciiu4_div = case_when(
        ciiu4_nivel_homologacion %in% c("clase_rev4_directa", "clase_rev3_a_rev4", "grupo_rev4_directo", "grupo_rev3_a_rev4") ~ str_sub(ciiu4_homologado, 1, 2),
        ciiu4_nivel_homologacion %in% c("division_rev4_directa", "division_rev3_a_rev4") ~ ciiu4_homologado,
        TRUE ~ NA_character_
      ),
      ciiu4_group = case_when(
        ciiu4_nivel_homologacion %in% c("clase_rev4_directa", "clase_rev3_a_rev4") ~ str_sub(ciiu4_homologado, 1, 3),
        ciiu4_nivel_homologacion %in% c("grupo_rev4_directo", "grupo_rev3_a_rev4") ~ ciiu4_homologado,
        TRUE ~ NA_character_
      ),
      ciiu4_homologacion_ambigua = case_when(
        !is.na(n_destinos_clase) ~ n_destinos_clase > 1,
        !is.na(n_destinos_grupo) ~ n_destinos_grupo > 1,
        !is.na(n_destinos_div) ~ n_destinos_div > 1,
        TRUE ~ FALSE
      )
    ) %>%
    select(-ciiu4_class_direct, -ciiu4_group_direct, -ciiu4_div_direct, -ciiu4_hom, -ciiu4_group_hom, -ciiu4_div_hom, -n_destinos_clase, -n_destinos_grupo, -n_destinos_div)
}

process_edit_file <- function(path) {
  file_name <- basename(path)
  years <- extract_years(file_name)
  dict_period <- get_dictionary_for_period(years$period)
  message("Procesando ", file_name, "...")

  df <- read_dta(path)
  original_names <- names(df)
  df <- apply_dictionary_names(df, dict_period)
  panel_names <- names(df)
  dictionary_name_source <- attr(df, "dictionary_name_source")
  df <- coerce_for_panel(df)

  ciiu_info <- detect_ciiu(df)
  ciiu_original <- if (!is.na(ciiu_info$var)) as_plain_character(df[[ciiu_info$var]]) else rep(NA_character_, nrow(df))
  tipologia_var <- intersect(c("TIPOLOGIA", "TIPOLO", "TIPOLO2005", "TIPOLO2009", "TIPOLO2011", "TIPOLO2017", "TIPOLO2019", "TIPOLO2021"), names(df))
  tipologia <- if (length(tipologia_var)) as_plain_character(df[[tipologia_var[1]]]) else rep(NA_character_, nrow(df))
  nordemp <- if ("NORDEMP" %in% names(df)) as_plain_character(df$NORDEMP) else rep(NA_character_, nrow(df))

  df <- df %>%
    mutate(
      edit_file = file_name,
      edit_period = years$period,
      year_start = as.integer(years$start),
      year_end = as.integer(years$end),
      year = as.integer(years$end),
      nordemp = nordemp,
      tipologia = tipologia,
      ciiu_var_original = ciiu_info$var,
      ciiu_revision_original = as.integer(ciiu_info$rev),
      ciiu_original = ciiu_original,
      .before = 1
    ) %>%
    add_ciiu_homologation(ciiu_bridge)

  vars_audit <- tibble(
    edit_file = file_name,
    edit_period = years$period,
    year_start = as.integer(years$start),
    year_end = as.integer(years$end),
    order_global = seq_along(panel_names),
    variable_original = original_names,
    variable_panel = panel_names,
    aparece = TRUE,
    dictionary_name_source = dictionary_name_source
  ) %>%
    left_join(
      dict_period %>%
        select(order_global, variable_original_dic, variable_dic, label_dane, variable_id_dane, variable_url_dane, page_url_dane, source_dictionary_url),
      by = "order_global"
    ) %>%
    mutate(
      variable_dic = coalesce(variable_dic, variable_panel),
      variable_match_dictionary = variable_panel == variable_dic,
      label_dane = ifelse(is.na(label_dane), "", label_dane)
    )

  file_audit <- tibble(
    edit_file = file_name,
    edit_period = years$period,
    year_start = as.integer(years$start),
    year_end = as.integer(years$end),
    year = as.integer(years$end),
    ciiu_var_original = ciiu_info$var,
    ciiu_revision_original = as.integer(ciiu_info$rev),
    observaciones = nrow(df),
    variables_archivo = length(panel_names),
    variables_diccionario = nrow(dict_period),
    dictionary_name_source = dictionary_name_source,
    variables_con_label_dane = sum(vars_audit$label_dane != "", na.rm = TRUE),
    variables_sin_label_dane = sum(vars_audit$label_dane == "" | is.na(vars_audit$label_dane)),
    variables_no_coinciden_diccionario = sum(!vars_audit$variable_match_dictionary, na.rm = TRUE)
  )

  list(data = df, vars = vars_audit, file_audit = file_audit)
}

edit_files <- list.files(raw_dir, pattern = "\\.dta$", full.names = TRUE)
edit_files <- edit_files[order(basename(edit_files))]
if (length(edit_files) == 0) stop("No encontre archivos .dta en: ", raw_dir)

processed <- map(edit_files, process_edit_file)
data_list <- map(processed, "data")

all_columns <- unique(unlist(map(data_list, names), use.names = FALSE))
is_char_by_col <- setNames(rep(FALSE, length(all_columns)), all_columns)
for (col in all_columns) {
  is_char_by_col[[col]] <- any(map_lgl(data_list, function(df) col %in% names(df) && (is.character(df[[col]]) || is.factor(df[[col]]))))
}

data_list <- map(data_list, function(df) {
  for (col in intersect(names(df), names(is_char_by_col)[is_char_by_col])) df[[col]] <- as.character(df[[col]])
  for (col in intersect(names(df), names(is_char_by_col)[!is_char_by_col])) {
    if (!is.numeric(df[[col]]) && !is.logical(df[[col]])) df[[col]] <- suppressWarnings(as.numeric(as.character(df[[col]])))
  }
  df
})

panel <- bind_rows(data_list)
vars_audit <- bind_rows(map(processed, "vars"))
file_audit <- bind_rows(map(processed, "file_audit"))

audit_homologacion <- panel %>%
  group_by(edit_file) %>%
  summarise(
    ciiu_missing = sum(is.na(ciiu_original) | ciiu_original == ""),
    ciiu4_homologado_missing = sum(is.na(ciiu4_homologado) | ciiu4_homologado == ""),
    ciiu4_homologacion_ambigua = sum(ciiu4_homologacion_ambigua, na.rm = TRUE),
    .groups = "drop"
  )

audit <- file_audit %>%
  left_join(audit_homologacion, by = "edit_file") %>%
  arrange(year_start, year_end, edit_file)

ciiu_audit <- panel %>%
  group_by(edit_period, year_start, year_end, ciiu_revision_original, ciiu_var_original, ciiu4_nivel_homologacion, ciiu4_homologacion_ambigua) %>%
  summarise(
    observaciones = n(),
    ciiu_original_distintos = n_distinct(ciiu_original, na.rm = TRUE),
    ciiu4_homologado_distintos = n_distinct(ciiu4_homologado, na.rm = TRUE),
    ciiu_missing = sum(is.na(ciiu_original) | ciiu_original == ""),
    ciiu4_homologado_missing = sum(is.na(ciiu4_homologado) | ciiu4_homologado == ""),
    .groups = "drop"
  ) %>%
  arrange(year_start, year_end, ciiu_revision_original, ciiu4_nivel_homologacion)

ordered_first <- c(
  "edit_file", "edit_period", "year_start", "year_end", "year",
  "nordemp", "tipologia",
  "ciiu_var_original", "ciiu_revision_original", "ciiu_original",
  "ciiu3_class", "ciiu3_group", "ciiu3_div",
  "ciiu4_homologado", "ciiu4_div", "ciiu4_group",
  "ciiu4_nivel_homologacion", "ciiu4_homologacion_ambigua"
)

panel <- panel %>% select(any_of(ordered_first), everything())

panel_variable_labels <- vars_audit %>%
  filter(!is.na(label_dane), label_dane != "") %>%
  group_by(variable_panel) %>%
  arrange(desc(year_end), desc(year_start), .by_group = TRUE) %>%
  summarise(label_panel = first(label_dane), .groups = "drop")

for (i in seq_len(nrow(panel_variable_labels))) {
  var_i <- panel_variable_labels$variable_panel[[i]]
  if (var_i %in% names(panel)) attr(panel[[var_i]], "label") <- substr(panel_variable_labels$label_panel[[i]], 1, 80)
}

write_csv(audit, audit_path)
write_csv(vars_audit, vars_path)
write_csv(ciiu_audit, ciiu_audit_path)
write_xlsx(
  list(
    resumen_archivos = audit,
    auditoria_ciiu = ciiu_audit,
    diccionario_panel = vars_audit,
    labels_panel = panel_variable_labels
  ),
  path = vars_xlsx_path
)
write_dta(panel, output_path, version = 14)

message("Listo.")
message("Panel EDIT-S: ", output_path)
message("Auditoria de archivos: ", audit_path)
message("Diccionario de variables: ", vars_path)
message("Auditoria CIIU: ", ciiu_audit_path)
message("Diccionario de variables XLSX: ", vars_xlsx_path)
message("Observaciones consolidadas: ", nrow(panel))
message("Variables consolidadas: ", ncol(panel))
