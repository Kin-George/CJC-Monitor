# ==============================================================================
# EDIT Servicios - Diccionarios oficiales DANE
# ==============================================================================
# Extrae nombre de variable y label desde las paginas de diccionario del DANE.
# No clasifica ni selecciona variables.
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(purrr)
  library(tibble)
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

clean_html_text <- function(x) {
  x <- gsub("<[^>]+>", " ", x)
  x <- gsub("&nbsp;", " ", x, fixed = TRUE)
  x <- gsub("&amp;", "&", x, fixed = TRUE)
  x <- gsub("&quot;", "\"", x, fixed = TRUE)
  x <- gsub("&#039;", "'", x, fixed = TRUE)
  x <- gsub("&lt;", "<", x, fixed = TRUE)
  x <- gsub("&gt;", ">", x, fixed = TRUE)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

read_url_text <- function(url_text) {
  con <- url(url_text, open = "rb")
  on.exit(close(con), add = TRUE)
  raw <- readBin(con, what = "raw", n = 100000000)
  paste(readLines(rawConnection(raw), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

make_offset_url <- function(base_url, offset) {
  if (grepl("offset=", base_url)) return(sub("offset=[0-9]*", paste0("offset=", offset), base_url))
  separator <- ifelse(grepl("\\?", base_url), "&", "?")
  paste0(base_url, separator, "offset=", offset)
}

extract_variable_rows <- function(html) {
  rows <- unlist(strsplit(html, '<div class="row var-row " >', fixed = TRUE))
  rows[-1]
}

extract_links_from_row <- function(row_html) {
  str_match_all(
    row_html,
    '<a[^>]*class="[^"]*var-id[^"]*"[^>]*href="([^"]+)"[^>]*>([\\s\\S]*?)</a>'
  )[[1]]
}

scrape_dictionary_page <- function(base_url, offset) {
  page_url <- make_offset_url(base_url, offset)
  html <- read_url_text(page_url)
  rows <- extract_variable_rows(html)
  if (length(rows) == 0) return(tibble())

  bind_rows(lapply(seq_along(rows), function(i) {
    links <- extract_links_from_row(rows[[i]])
    variable_url <- NA_character_
    variable_name <- NA_character_
    variable_label <- NA_character_
    variable_id <- NA_character_

    if (nrow(links) >= 1) {
      variable_url <- links[1, 2]
      variable_name <- clean_html_text(links[1, 3])
      variable_name_from_url <- str_match(variable_url, "name=([^&\"']+)")[, 2]
      variable_id <- str_match(variable_url, "/variable/[^/]+/([^?\"']+)\\?name=")[, 2]
      if (!is.na(variable_name_from_url) && variable_name_from_url != "") variable_name <- variable_name_from_url
    }
    if (nrow(links) >= 2) variable_label <- clean_html_text(links[2, 3])

    tibble(
      order_in_page = i,
      offset = offset,
      variable = toupper(variable_name),
      label = variable_label,
      variable_id = variable_id,
      variable_url = variable_url,
      page_url = page_url
    )
  }))
}

download_dictionary <- function(base_url, page_size = 300, max_pages = 50, sleep_seconds = 0.2) {
  pages <- list()
  for (page_index in seq_len(max_pages)) {
    offset <- (page_index - 1) * page_size
    message("  offset=", offset)
    page <- tryCatch(
      scrape_dictionary_page(base_url, offset),
      error = function(e) {
        warning("No pude leer offset ", offset, ": ", conditionMessage(e))
        tibble()
      }
    )
    if (nrow(page) == 0) break
    pages[[length(pages) + 1]] <- page
    if (nrow(page) < page_size) break
    Sys.sleep(sleep_seconds)
  }

  bind_rows(pages) %>%
    distinct(variable, .keep_all = TRUE) %>%
    mutate(order_global = row_number()) %>%
    select(order_global, order_in_page, offset, variable, label, variable_id, variable_url, page_url)
}

project_root <- find_project_root()
output_dir <- file.path(project_root, "Diccionarios", "EDIT-S")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

sources <- tribble(
  ~period,      ~year_start, ~year_end, ~catalog_id, ~file_id, ~file_name,                              ~dictionary_url,
  "2004_2005", 2004L,       2005L,     569L,        "F1",     "Estructura_EDIT_SER_2004_2005",          "https://microdatos.dane.gov.co/index.php/catalog/569/data-dictionary/F1?file_name=Estructura_EDIT_SER_2004_2005",
  "2008_2009", 2008L,       2009L,     533L,        "F2",     "Estructura_EDIT_SERVICIOS_II_2008_2009", "https://microdatos.dane.gov.co/index.php/catalog/533/data-dictionary/F2?file_name=Estructura_EDIT_SERVICIOS_II_2008_2009",
  "2010_2011", 2010L,       2011L,     534L,        "F3",     "Estructura_EDIT_SERV_III_2010_2011",     "https://microdatos.dane.gov.co/index.php/catalog/534/data-dictionary/F3?file_name=Estructura_EDIT_SERV_III_2010_2011",
  "2016_2017", 2016L,       2017L,     584L,        "F35",    "Estructura_EDITS_VI_2016_2017",          "https://microdatos.dane.gov.co/index.php/catalog/584/data-dictionary/F35?file_name=Estructura_EDITS_VI_2016_2017",
  "2018_2019", 2018L,       2019L,     699L,        "F44",    "Estructura_EDITS_VII_2018_2019",         "https://microdatos.dane.gov.co/index.php/catalog/699/data-dictionary/F44?file_name=Estructura_EDITS_VII_2018_2019",
  "2020_2021", 2020L,       2021L,     867L,        "F1",     "Estructura_EDITS_VIII_2020_2021",        "https://microdatos.dane.gov.co/index.php/catalog/867/data-dictionary/F1?file_name=Estructura_EDITS_VIII_2020_2021"
)

all_dictionaries <- list()

for (i in seq_len(nrow(sources))) {
  source_i <- sources[i, ]
  message("Descargando diccionario EDIT-S ", source_i$period, "...")

  dict_i <- download_dictionary(source_i$dictionary_url) %>%
    mutate(
      period = source_i$period,
      year_start = source_i$year_start,
      year_end = source_i$year_end,
      catalog_id = source_i$catalog_id,
      file_id = source_i$file_id,
      file_name = source_i$file_name,
      source_dictionary_url = source_i$dictionary_url,
      .before = 1
    )

  all_dictionaries[[source_i$period]] <- dict_i

  write_xlsx(
    list(fuente = source_i, variables = dict_i),
    path = file.path(output_dir, paste0("EDITS_Diccionario_", source_i$period, ".xlsx"))
  )
}

consolidated <- bind_rows(all_dictionaries)

coverage_by_variable <- consolidated %>%
  group_by(variable) %>%
  summarise(
    n_periods = n_distinct(period),
    periods = paste(sort(unique(period)), collapse = ", "),
    first_year = min(year_start, na.rm = TRUE),
    last_year = max(year_end, na.rm = TRUE),
    labels_observed = paste(unique(label[!is.na(label) & label != ""]), collapse = " | "),
    .groups = "drop"
  ) %>%
  arrange(variable)

summary_by_source <- consolidated %>%
  group_by(period, year_start, year_end, catalog_id, file_id, file_name) %>%
  summarise(
    n_variables = n(),
    n_labels_nonmissing = sum(!is.na(label) & label != ""),
    n_labels_missing = sum(is.na(label) | label == ""),
    .groups = "drop"
  ) %>%
  arrange(year_start, year_end)

write_xlsx(
  list(
    fuentes = sources,
    resumen = summary_by_source,
    variables = consolidated,
    cobertura_por_variable = coverage_by_variable
  ),
  path = file.path(output_dir, "EDITS_Diccionarios_Consolidado.xlsx")
)

message("Listo.")
message("Carpeta de salida: ", output_dir)
