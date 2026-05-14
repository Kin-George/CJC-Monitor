find_project_root <- function(path = getwd()) {
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)

  repeat {
    if (
      dir.exists(file.path(path, ".git")) &&
        dir.exists(file.path(path, "Code")) &&
        dir.exists(file.path(path, "Paper"))
    ) {
      return(path)
    }

    parent <- dirname(path)
    if (identical(parent, path)) {
      stop("No se encontro la raiz del proyecto CJC-Monitor.", call. = FALSE)
    }
    path <- parent
  }
}

PROJECT_ROOT <- find_project_root()

project_path <- function(...) {
  file.path(PROJECT_ROOT, ...)
}

ensure_project_dirs <- function() {
  dirs <- c(
    project_path("Outputs", "Figures"),
    project_path("Outputs", "tables"),
    project_path("Paper", "figures"),
    project_path("Paper", "tables")
  )
  invisible(lapply(dirs, dir.create, recursive = TRUE, showWarnings = FALSE))
}

save_project_table <- function(x, filename, ...) {
  ensure_project_dirs()
  outputs_path <- project_path("Outputs", "tables", filename)
  paper_path <- project_path("Paper", "tables", filename)
  csv_name <- sub("\\.xlsx$", ".csv", filename, ignore.case = TRUE)
  outputs_csv_path <- project_path("Outputs", "tables", csv_name)
  paper_csv_path <- project_path("Paper", "tables", csv_name)

  openxlsx::write.xlsx(x, outputs_path, ...)
  openxlsx::write.xlsx(x, paper_path, ...)
  utils::write.csv(x, outputs_csv_path, row.names = FALSE, fileEncoding = "UTF-8")
  utils::write.csv(x, paper_csv_path, row.names = FALSE, fileEncoding = "UTF-8")

  invisible(c(
    outputs = outputs_path,
    paper = paper_path,
    outputs_csv = outputs_csv_path,
    paper_csv = paper_csv_path
  ))
}

save_project_figure <- function(plot, filename, width = 9, height = 5.2, dpi = 300, ...) {
  ensure_project_dirs()
  outputs_path <- project_path("Outputs", "Figures", filename)
  paper_path <- project_path("Paper", "figures", filename)

  ggplot2::ggsave(outputs_path, plot = plot, width = width, height = height, dpi = dpi, ...)
  ggplot2::ggsave(paper_path, plot = plot, width = width, height = height, dpi = dpi, ...)

  invisible(c(outputs = outputs_path, paper = paper_path))
}

sync_existing_outputs_to_paper <- function() {
  ensure_project_dirs()

  figures <- list.files(
    project_path("Outputs", "Figures"),
    pattern = "\\.(png|pdf|jpg|jpeg)$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  tables <- list.files(
    project_path("Outputs", "tables"),
    pattern = "\\.(xlsx|csv|tex)$",
    full.names = TRUE,
    ignore.case = TRUE
  )

  if (length(figures) > 0) {
    file.copy(figures, project_path("Paper", "figures"), overwrite = TRUE)
  }
  if (length(tables) > 0) {
    file.copy(tables, project_path("Paper", "tables"), overwrite = TRUE)
  }

  invisible(list(figures = figures, tables = tables))
}
