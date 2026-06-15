# ==============================================================================
# EDIT - Base simple y descriptivos sectoriales
# ==============================================================================
# Objetivo:
#   Crear una base empresa-anio simple, comparable con el espiritu de los
#   descriptivos GEIH/PIB del informe de productividad.
#
#   El script usa el diccionario oficial DANE ya cruzado con el panel para mapear
#   variables por label. No asume que todos los indicadores existen en todas las
#   rondas: genera una auditoria de cobertura y calcula cada indicador solo donde
#   el diccionario permite identificarlo.
#
# Entradas:
#   Datos/Processed/EDIT_Panel.dta
#   Datos/Processed/EDIT_Panel_diccionario_variables.xlsx
#
# Salidas:
#   Graficos en el visor de R/RStudio.
#   No exporta bases nuevas.
# ==============================================================================

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(readxl)
  library(readr)
  library(stringr)
  library(tibble)
  library(ggplot2)
  library(scales)
})

find_project_root <- function(start = getwd()) {
  current <- normalizePath(start, mustWork = TRUE)
  repeat {
    if (dir.exists(file.path(current, "Code")) && dir.exists(file.path(current, "Datos"))) {
      return(current)
    }
    parent <- dirname(current)
    if (identical(parent, current)) {
      stop("No pude encontrar la raiz del proyecto desde: ", start)
    }
    current <- parent
  }
}

clean_label <- function(x) {
  x <- ifelse(is.na(x), "", as.character(x))
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- tolower(x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

to_numeric_safe <- function(x) {
  if (is.numeric(x)) {
    return(x)
  }
  x <- as.character(x)
  x <- str_replace_all(x, "\\.", "")
  x <- str_replace_all(x, ",", ".")
  suppressWarnings(as.numeric(x))
}

first_existing <- function(data, vars) {
  vars <- intersect(vars, names(data))
  if (length(vars) == 0) {
    return(rep(NA_real_, nrow(data)))
  }
  out <- rep(NA_real_, nrow(data))
  for (v in vars) {
    x <- to_numeric_safe(data[[v]])
    out <- ifelse(is.na(out), x, out)
  }
  out
}

row_sum_existing <- function(data, vars) {
  vars <- intersect(vars, names(data))
  if (length(vars) == 0) {
    return(rep(NA_real_, nrow(data)))
  }

  tmp <- data[, vars, drop = FALSE] %>%
    mutate(across(everything(), to_numeric_safe))

  out <- rowSums(tmp, na.rm = TRUE)
  out[rowSums(!is.na(tmp)) == 0] <- NA_real_
  out
}

row_any_yes_existing <- function(data, vars) {
  vars <- intersect(vars, names(data))
  if (length(vars) == 0) {
    return(rep(NA, nrow(data)))
  }

  tmp <- data[, vars, drop = FALSE] %>%
    mutate(across(everything(), to_numeric_safe))

  out <- rowSums(tmp == 1, na.rm = TRUE) > 0
  out[rowSums(!is.na(tmp)) == 0] <- NA
  out
}

extract_year_from_label <- function(label_clean, year_start, year_end) {
  year_hits <- str_extract_all(label_clean, "(19|20)[0-9]{2}")
  year_hit <- vapply(year_hits, function(x) {
    x <- suppressWarnings(as.integer(x))
    if (length(x) == 0 || all(is.na(x))) {
      return(NA_integer_)
    }
    max(x, na.rm = TRUE)
  }, integer(1))

  ifelse(
    !is.na(year_hit),
    year_hit,
    suppressWarnings(as.integer(year_end))
  )
}

classify_indicator <- function(variable_panel, label_clean) {
  case_when(
    str_detect(label_clean, "total personal ocupado promedio") &
      !str_detect(label_clean, "participo|participó|acti|educacion superior") ~
      "empleo_total",

    str_detect(label_clean, "total personal ocupado promedio") &
      str_detect(label_clean, "acti") &
      str_detect(label_clean, "participo|participó") ~
      "empleo_acti",

    str_detect(label_clean, "ventas nacionales totales|ingresos o ventas nacionales totales|ventas totales") ~
      "ventas_nacionales",

    str_detect(label_clean, "actividades de i\\+d internas|actividades de i\\+d internas|i\\+d internas") &
      str_detect(label_clean, "monto invertido|total inversion|total inversión") ~
      "inversion_id_interna",

    str_detect(label_clean, "investigacion y desarrollo i \\+ d|investigación y desarrollo i \\+ d") &
      str_detect(label_clean, "total inversion|total inversión|miles de pesos") ~
      "inversion_id_interna",

    str_detect(label_clean, "adquisicion de maquinaria y equipo|adquisición de maquinaria y equipo|maquinaria y equipo") &
      str_detect(label_clean, "monto invertido|total inversion|total inversión|miles de pesos") ~
      "inversion_maquinaria_equipo",

    str_detect(label_clean, "numero total de innovaciones|número total de innovaciones|total innovaciones") ~
      "numero_innovaciones",

    str_detect(label_clean, "si=1, no=2|si =1, no=2|si=1 no=2") &
      str_detect(label_clean, "bienes o servicios nuevos|bienes o servicios mejorados|metodos de produccion|metodos de prestacion|metodos organizativos|tecnicas de comercializacion|sistemas logisticos|distribucion") ~
      "indicador_innova",

    TRUE ~ NA_character_
  )
}

detect_department <- function(label_clean) {
  departamentos <- c(
    "amazonas", "antioquia", "arauca", "atlantico", "bogota d.c.",
    "bogota", "bolivar", "boyaca", "caldas", "caqueta", "casanare",
    "cauca", "cesar", "choco", "cordoba", "cundinamarca", "guainia",
    "guaviare", "huila", "la guajira", "magdalena", "meta", "narino",
    "norte de santander", "putumayo", "quindio", "risaralda",
    "san andres y providencia", "san andres", "santander", "sucre",
    "tolima", "valle del cauca", "valle", "vaupes", "vichada"
  )

  vapply(label_clean, function(x) {
    if (is.na(x) || x == "") {
      return(NA_character_)
    }
    hit <- departamentos[str_detect(x, paste0("^", departamentos, "\\b"))]
    ifelse(length(hit) == 0, NA_character_, hit[1])
  }, character(1))
}

pretty_department <- function(x) {
  recode(
    x,
    "bogota" = "Bogota D.C.",
    "bogota d.c." = "Bogota D.C.",
    "atlantico" = "Atlantico",
    "bolivar" = "Bolivar",
    "boyaca" = "Boyaca",
    "caqueta" = "Caqueta",
    "choco" = "Choco",
    "cordoba" = "Cordoba",
    "guainia" = "Guainia",
    "la guajira" = "La Guajira",
    "narino" = "Narino",
    "norte de santander" = "Norte de Santander",
    "quindio" = "Quindio",
    "san andres" = "San Andres y Providencia",
    "san andres y providencia" = "San Andres y Providencia",
    "valle" = "Valle del Cauca",
    "valle del cauca" = "Valle del Cauca",
    "vaupes" = "Vaupes",
    .default = str_to_title(x)
  )
}

project_root <- find_project_root()
processed_dir <- file.path(project_root, "Datos", "Processed")
doc_dir <- file.path(project_root, "DocumentacionAuxiliar")
output_dir <- processed_dir

panel_path <- file.path(processed_dir, "EDIT_Panel.dta")
dictionary_path <- file.path(processed_dir, "EDIT_Panel_diccionario_variables.xlsx")
ciiu4_structure_path <- file.path(doc_dir, "Estructura-detallada-CIIU-4AC-2022.xlsx")

if (!file.exists(panel_path)) {
  stop("No existe: ", panel_path)
}
if (!file.exists(dictionary_path)) {
  stop("No existe: ", dictionary_path)
}
if (!file.exists(ciiu4_structure_path)) {
  stop("No existe: ", ciiu4_structure_path)
}

ciiu4_div_labels <- read_excel(
  ciiu4_structure_path,
  sheet = 1,
  skip = 1,
  col_types = "text"
) %>%
  transmute(
    ciiu4_div = str_pad(as.character(División), width = 2, pad = "0"),
    ciiu4_div_descripcion = as.character(Descripción)
  ) %>%
  filter(!is.na(ciiu4_div), !is.na(ciiu4_div_descripcion)) %>%
  distinct(ciiu4_div, .keep_all = TRUE) %>%
  mutate(
    ciiu4_div_label = paste0(ciiu4_div, " - ", str_to_sentence(str_to_lower(ciiu4_div_descripcion))),
    ciiu4_div_label_plot = str_wrap(ciiu4_div_label, width = 42)
  )

dict <- read_excel(dictionary_path, sheet = "diccionario_panel", col_types = "text") %>%
  mutate(
    edit_period = as.character(edit_period),
    year_start = suppressWarnings(as.integer(year_start)),
    year_end = suppressWarnings(as.integer(year_end)),
    variable_panel = as.character(variable_panel),
    label_clean = clean_label(label_dane),
    indicador = classify_indicator(variable_panel, label_clean),
    indicador_year = extract_year_from_label(label_clean, year_start, year_end)
  ) %>%
  filter(!is.na(indicador)) %>%
  distinct(
    edit_period,
    year_start,
    year_end,
    indicador_year,
    indicador,
    variable_panel,
    label_dane,
    variable_original,
    variable_original_dic,
    variable_url_dane,
    .keep_all = TRUE
  )

dept_dict <- read_excel(dictionary_path, sheet = "diccionario_panel", col_types = "text") %>%
  mutate(
    edit_period = as.character(edit_period),
    year_start = suppressWarnings(as.integer(year_start)),
    year_end = suppressWarnings(as.integer(year_end)),
    variable_panel = as.character(variable_panel),
    label_clean = clean_label(label_dane),
    indicador_year = extract_year_from_label(label_clean, year_start, year_end),
    departamento = detect_department(label_clean),
    departamento = pretty_department(departamento)
  ) %>%
  filter(
    !is.na(departamento),
    str_detect(label_clean, "personal ocupado promedio"),
    str_detect(label_clean, "acti|actividades cientificas|actividades científicas")
  ) %>%
  distinct(
    edit_period,
    indicador_year,
    departamento,
    variable_panel,
    label_dane,
    .keep_all = TRUE
  )

indicator_coverage <- dict %>%
  group_by(indicador) %>%
  summarise(
    n_periodos = n_distinct(edit_period),
    periodos = paste(sort(unique(edit_period)), collapse = ", "),
    primer_anio = min(indicador_year, na.rm = TRUE),
    ultimo_anio = max(indicador_year, na.rm = TRUE),
    n_variables = n(),
    disponible_2007_2020 = all(c(
      "2007_2008", "2009_2010", "2011_2012", "2013_2014",
      "2015_2016", "2017_2018", "2019_2020"
    ) %in% unique(edit_period)),
    disponible_2003_2020 = all(c(
      "2003_2004", "2005_2006", "2007_2008", "2009_2010",
      "2011_2012", "2013_2014", "2015_2016", "2017_2018", "2019_2020"
    ) %in% unique(edit_period)),
    .groups = "drop"
  ) %>%
  arrange(desc(disponible_2007_2020), indicador)

vars_needed <- unique(c(
  "edit_file", "edit_period", "year_start", "year_end", "year",
  "nordemp", "tipologia",
  "ciiu_revision_original", "ciiu_original",
  "ciiu4_homologado", "ciiu4_div", "ciiu4_group",
  "ciiu4_nivel_homologacion", "ciiu4_homologacion_ambigua",
  dict$variable_panel,
  dept_dict$variable_panel
))

panel_names <- names(read_dta(panel_path, n_max = 0))
vars_needed <- intersect(vars_needed, panel_names)

panel <- read_dta(panel_path, col_select = all_of(vars_needed))

base_static <- panel %>%
  transmute(
    edit_file = as.character(edit_file),
    edit_period = as.character(edit_period),
    year_start = as.integer(year_start),
    year_end = as.integer(year_end),
    empresa_id = as.character(nordemp),
    tipologia = as.character(tipologia),
    ciiu_revision_original = as.integer(ciiu_revision_original),
    ciiu_original = as.character(ciiu_original),
    ciiu4_homologado = as.character(ciiu4_homologado),
    ciiu4_div = as.character(ciiu4_div),
    ciiu4_group = as.character(ciiu4_group),
    ciiu4_nivel_homologacion = as.character(ciiu4_nivel_homologacion),
    ciiu4_homologacion_ambigua = as.logical(ciiu4_homologacion_ambigua)
  )

expected_indicators <- c(
  "empleo_total",
  "empleo_acti",
  "ventas_nacionales",
  "inversion_id_interna",
  "inversion_maquinaria_equipo",
  "numero_innovaciones",
  "indicador_innova"
)

make_base_for_year <- function(year_i) {
  base_i <- base_static %>%
    filter(year_start == year_i | year_end == year_i) %>%
    mutate(year = year_i)

  rows_i <- which(base_static$year_start == year_i | base_static$year_end == year_i)
  panel_i <- panel[rows_i, , drop = FALSE]

  for (indicator_name in expected_indicators) {
    vars_i <- dict %>%
      filter(indicador == indicator_name, indicador_year == year_i) %>%
      pull(variable_panel) %>%
      unique()

    if (indicator_name == "indicador_innova") {
      base_i[[indicator_name]] <- row_any_yes_existing(panel_i, vars_i)
    } else if (indicator_name %in% c("empleo_total", "empleo_acti", "ventas_nacionales")) {
      base_i[[indicator_name]] <- first_existing(panel_i, vars_i)
    } else {
      base_i[[indicator_name]] <- row_sum_existing(panel_i, vars_i)
    }
  }

  base_i
}

years_panel <- sort(unique(c(base_static$year_start, base_static$year_end)))
years_panel <- years_panel[!is.na(years_panel)]

base <- bind_rows(lapply(years_panel, make_base_for_year))

base <- base %>%
  left_join(ciiu4_div_labels, by = "ciiu4_div") %>%
  mutate(
    ventas_por_trabajador = ventas_nacionales / empleo_total,
    inversion_id_por_trabajador = inversion_id_interna / empleo_total,
    inversion_maquinaria_por_trabajador = inversion_maquinaria_equipo / empleo_total,
    innovaciones_por_trabajador = numero_innovaciones / empleo_total,
    intensidad_acti = empleo_acti / empleo_total,
    firma_con_innovacion = indicador_innova
  ) %>%
  select(
    edit_file, edit_period, year_start, year_end, year,
    empresa_id, tipologia,
    ciiu_revision_original, ciiu_original,
    ciiu4_homologado, ciiu4_div, ciiu4_div_label, ciiu4_div_label_plot, ciiu4_group,
    ciiu4_nivel_homologacion, ciiu4_homologacion_ambigua,
    empleo_total, empleo_acti,
    ventas_nacionales,
    inversion_id_interna,
    inversion_maquinaria_equipo,
    numero_innovaciones,
    firma_con_innovacion,
    ventas_por_trabajador,
    inversion_id_por_trabajador,
    inversion_maquinaria_por_trabajador,
    innovaciones_por_trabajador,
    intensidad_acti,
    everything()
  )

metric_availability <- base %>%
  group_by(edit_period, year) %>%
  summarise(
    empresas = n(),
    across(
      c(
        empleo_total,
        empleo_acti,
        ventas_nacionales,
        inversion_id_interna,
        inversion_maquinaria_equipo,
        numero_innovaciones,
        firma_con_innovacion
      ),
      ~ sum(!is.na(.x)),
      .names = "n_{.col}"
    ),
    .groups = "drop"
  ) %>%
  arrange(year)

sector_year <- base %>%
  filter(!is.na(ciiu4_div), ciiu4_div != "") %>%
  group_by(year, edit_period, ciiu4_div, ciiu4_div_label, ciiu4_div_label_plot) %>%
  summarise(
    empresas_edit = n(),
    empresas_con_empleo = sum(!is.na(empleo_total)),
    empleo_total = sum(empleo_total, na.rm = TRUE),
    empleo_acti = sum(empleo_acti, na.rm = TRUE),
    ventas_nacionales = sum(ventas_nacionales, na.rm = TRUE),
    inversion_id_interna = sum(inversion_id_interna, na.rm = TRUE),
    inversion_maquinaria_equipo = sum(inversion_maquinaria_equipo, na.rm = TRUE),
    numero_innovaciones = sum(numero_innovaciones, na.rm = TRUE),
    tasa_innovacion = mean(firma_con_innovacion, na.rm = TRUE),
    ventas_por_trabajador = ventas_nacionales / empleo_total,
    inversion_id_por_trabajador = inversion_id_interna / empleo_total,
    inversion_maquinaria_por_trabajador = inversion_maquinaria_equipo / empleo_total,
    innovaciones_por_trabajador = numero_innovaciones / empleo_total,
    intensidad_acti = empleo_acti / empleo_total,
    empleo_promedio_empresa = mean(empleo_total, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(year, ciiu4_div)

sector_growth <- sector_year %>%
  group_by(ciiu4_div) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(
    g_empleo_total = empleo_total / lag(empleo_total) - 1,
    g_ventas_por_trabajador = ventas_por_trabajador / lag(ventas_por_trabajador) - 1,
    g_inversion_id_por_trabajador = inversion_id_por_trabajador / lag(inversion_id_por_trabajador) - 1
  ) %>%
  ungroup()

dept_year <- bind_rows(lapply(sort(unique(dept_dict$indicador_year)), function(year_i) {
  rows_i <- which(base_static$year_start == year_i | base_static$year_end == year_i)
  if (length(rows_i) == 0) return(tibble())

  panel_i <- panel[rows_i, , drop = FALSE]
  static_i <- base_static[rows_i, , drop = FALSE]

  bind_rows(lapply(sort(unique(dept_dict$departamento)), function(dep_i) {
    vars_i <- dept_dict %>%
      filter(indicador_year == year_i, departamento == dep_i) %>%
      pull(variable_panel) %>%
      unique()

    vars_i <- intersect(vars_i, names(panel_i))
    if (length(vars_i) == 0) return(tibble())

    tibble(
      year = year_i,
      departamento = dep_i,
      personal_acti_departamento = row_sum_existing(panel_i, vars_i),
      ciiu4_div = static_i$ciiu4_div,
      ciiu4_group = static_i$ciiu4_group
    )
  }))
})) %>%
  filter(!is.na(personal_acti_departamento)) %>%
  group_by(year, departamento) %>%
  summarise(
    personal_acti_departamento = sum(personal_acti_departamento, na.rm = TRUE),
    empresas_con_personal_acti_dep = sum(personal_acti_departamento > 0, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(year, departamento)

dept_sector_year <- bind_rows(lapply(sort(unique(dept_dict$indicador_year)), function(year_i) {
  rows_i <- which(base_static$year_start == year_i | base_static$year_end == year_i)
  if (length(rows_i) == 0) return(tibble())

  panel_i <- panel[rows_i, , drop = FALSE]
  static_i <- base_static[rows_i, , drop = FALSE]

  bind_rows(lapply(sort(unique(dept_dict$departamento)), function(dep_i) {
    vars_i <- dept_dict %>%
      filter(indicador_year == year_i, departamento == dep_i) %>%
      pull(variable_panel) %>%
      unique()

    vars_i <- intersect(vars_i, names(panel_i))
    if (length(vars_i) == 0) return(tibble())

    tibble(
      year = year_i,
      departamento = dep_i,
      ciiu4_div = static_i$ciiu4_div,
      personal_acti_departamento = row_sum_existing(panel_i, vars_i)
    )
  }))
})) %>%
  filter(!is.na(personal_acti_departamento), !is.na(ciiu4_div), ciiu4_div != "") %>%
  group_by(year, departamento, ciiu4_div) %>%
  summarise(
    personal_acti_departamento = sum(personal_acti_departamento, na.rm = TRUE),
    empresas_con_personal_acti_dep = sum(personal_acti_departamento > 0, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(year, departamento, ciiu4_div)

theme_edit <- theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

top_sectors <- sector_year %>%
  group_by(ciiu4_div) %>%
  summarise(empleo_total = sum(empleo_total, na.rm = TRUE), .groups = "drop") %>%
  slice_max(empleo_total, n = 12, with_ties = FALSE) %>%
  pull(ciiu4_div)

plot_empresas_sector <- sector_year %>%
  filter(ciiu4_div %in% top_sectors) %>%
  ggplot(aes(x = year, y = empresas_edit, color = ciiu4_div_label_plot, group = ciiu4_div)) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.6) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "EDIT: empresas por division CIIU Rev. 4",
    x = NULL,
    y = "Empresas",
    color = "Sector"
  ) +
  theme_edit

plot_empleo_sector <- sector_year %>%
  filter(ciiu4_div %in% top_sectors) %>%
  ggplot(aes(x = year, y = empleo_total, color = ciiu4_div_label_plot, group = ciiu4_div)) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.6) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "EDIT: personal ocupado promedio por division CIIU Rev. 4",
    x = NULL,
    y = "Personal ocupado",
    color = "Sector"
  ) +
  theme_edit

plot_ventas_trabajador <- sector_year %>%
  filter(ciiu4_div %in% top_sectors, !is.na(ventas_por_trabajador)) %>%
  ggplot(aes(x = year, y = ventas_por_trabajador, color = ciiu4_div_label_plot, group = ciiu4_div)) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.6) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "EDIT: ventas nacionales por trabajador",
    subtitle = "Disponible donde el diccionario reporta ventas nacionales totales",
    x = NULL,
    y = "Miles de pesos corrientes por trabajador",
    color = "Sector"
  ) +
  theme_edit

plot_inversion_id <- sector_year %>%
  filter(ciiu4_div %in% top_sectors, !is.na(inversion_id_por_trabajador)) %>%
  ggplot(aes(x = year, y = inversion_id_por_trabajador, color = ciiu4_div_label_plot, group = ciiu4_div)) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.6) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "EDIT: inversion en I+D interna por trabajador",
    x = NULL,
    y = "Miles de pesos corrientes por trabajador",
    color = "Sector"
  ) +
  theme_edit

plot_intensidad_acti <- sector_year %>%
  filter(ciiu4_div %in% top_sectors, !is.na(intensidad_acti)) %>%
  ggplot(aes(x = year, y = intensidad_acti, color = ciiu4_div_label_plot, group = ciiu4_div)) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.6) +
  scale_y_continuous(labels = percent) +
  labs(
    title = "EDIT: intensidad de personal ACTI",
    x = NULL,
    y = "Personal ACTI / personal ocupado",
    color = "Sector"
  ) +
  theme_edit

plot_tasa_innovacion <- sector_year %>%
  filter(ciiu4_div %in% top_sectors, !is.na(tasa_innovacion)) %>%
  ggplot(aes(x = year, y = tasa_innovacion, color = ciiu4_div_label_plot, group = ciiu4_div)) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.6) +
  scale_y_continuous(labels = percent) +
  labs(
    title = "EDIT: tasa de empresas innovadoras",
    x = NULL,
    y = "Proporcion de empresas",
    color = "Sector"
  ) +
  theme_edit

latest_dept_year <- max(dept_year$year, na.rm = TRUE)

plot_dept_acti <- dept_year %>%
  filter(year == latest_dept_year) %>%
  slice_max(personal_acti_departamento, n = 15, with_ties = FALSE) %>%
  ggplot(aes(x = reorder(departamento, personal_acti_departamento), y = personal_acti_departamento)) +
  geom_col(fill = "#2563EB") +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  labs(
    title = paste0("EDIT: personal ACTI por departamento, ", latest_dept_year),
    x = NULL,
    y = "Personal ACTI"
  ) +
  theme_edit

plot_dept_time <- dept_year %>%
  group_by(departamento) %>%
  summarise(total = sum(personal_acti_departamento, na.rm = TRUE), .groups = "drop") %>%
  slice_max(total, n = 10, with_ties = FALSE) %>%
  inner_join(dept_year, by = "departamento") %>%
  ggplot(aes(x = year, y = personal_acti_departamento, color = departamento, group = departamento)) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.6) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "EDIT: personal ACTI por departamento",
    x = NULL,
    y = "Personal ACTI",
    color = "Departamento"
  ) +
  theme_edit

print(indicator_coverage)
print(metric_availability)
print(plot_empresas_sector)
print(plot_empleo_sector)
print(plot_ventas_trabajador)
print(plot_inversion_id)
print(plot_intensidad_acti)
print(plot_tasa_innovacion)
print(plot_dept_acti)
print(plot_dept_time)

message("Listo. Los objetos base, sector_year, dept_year y dept_sector_year quedan disponibles en memoria.")
