local_r_lib <- file.path(getwd(), ".Rlib")
if (dir.exists(local_r_lib)) {
  .libPaths(c(local_r_lib, .libPaths()))
}

local_r_config <- file.path(getwd(), ".Rconfig")
dir.create(local_r_config, recursive = TRUE, showWarnings = FALSE)
Sys.setenv(
  R_USER_CONFIG_DIR = local_r_config,
  XDG_CONFIG_HOME = local_r_config
)

source(file.path("Code", "_paths.R"))

library(haven)
library(dplyr)
library(fixest)
library(broom)
library(stringr)

format_coef <- function(x, p = NA_real_) {
  stars <- dplyr::case_when(
    is.na(p) ~ "",
    p < 0.01 ~ "***",
    p < 0.05 ~ "**",
    p < 0.10 ~ "*",
    TRUE ~ ""
  )
  paste0(sprintf("%.3f", x), stars)
}

format_se <- function(x) {
  ifelse(is.na(x), "n.a.", paste0("(", sprintf("%.3f", x), ")"))
}

geih <- read_dta(geih_personas_data_path())

geih <- geih %>%
  mutate(
    anio = as.factor(anio),
    edad = as.numeric(edad),
    edad2 = edad^2,
    sector = as.factor(sector),
    tamano_empresa = as.factor(tamano_empresa),
    educacion = as.factor(educacion)
  )

geih_model <- geih %>%
  filter(
    !is.na(log_ingreso_hora_real),
    !is.na(tamano_empresa),
    !is.na(mujer),
    !is.na(edad),
    !is.na(edad2),
    !is.na(educacion),
    !is.na(formal),
    !is.na(sector),
    !is.na(anio),
    !is.na(fex),
    fex > 0
  ) %>%
  mutate(
    size_midpoint = dplyr::case_when(
      tamano_empresa == "Solo" ~ 1,
      tamano_empresa == "2-3" ~ 2.5,
      tamano_empresa == "4-5" ~ 4.5,
      tamano_empresa == "6-10" ~ 8,
      tamano_empresa == "11-19" ~ 15,
      tamano_empresa == "20-30" ~ 25,
      tamano_empresa == "31-50" ~ 40.5,
      tamano_empresa == "51-100" ~ 75.5,
      tamano_empresa == "101+" ~ 150,
      TRUE ~ NA_real_
    ),
    log_size_midpoint = log(size_midpoint)
  ) %>%
  filter(!is.na(log_size_midpoint))

education_ref <- grep("secundaria", levels(geih_model$educacion), ignore.case = TRUE, value = TRUE)[1]

if (is.na(education_ref)) {
  stop("No se encontro una categoria de educacion de referencia que contenga 'secundaria'.")
}

m_elasticity_raw <- feols(
  log_ingreso_hora_real ~ log_size_midpoint,
  weights = ~ fex,
  cluster = ~ sector,
  data = geih_model
)

m_elasticity_full <- feols(
  log_ingreso_hora_real ~
    log_size_midpoint +
    mujer +
    edad +
    edad2 +
    i(educacion, ref = education_ref) +
    formal |
    sector^anio,
  weights = ~ fex,
  cluster = ~ sector,
  data = geih_model
)

m_elasticity_formal <- feols(
  log_ingreso_hora_real ~
    log_size_midpoint +
    mujer +
    edad +
    edad2 +
    i(educacion, ref = education_ref) |
    sector^anio,
  weights = ~ fex,
  cluster = ~ sector,
  data = geih_model %>% filter(formal == 1)
)

m_elasticity_informal <- feols(
  log_ingreso_hora_real ~
    log_size_midpoint +
    mujer +
    edad +
    edad2 +
    i(educacion, ref = education_ref) |
    sector^anio,
  weights = ~ fex,
  cluster = ~ sector,
  data = geih_model %>% filter(formal == 0)
)

extract_elasticity <- function(model, source, specification, controls, order) {
  tidy(model) %>%
    filter(term == "log_size_midpoint") %>%
    transmute(
      source = source,
      specification = specification,
      controls = controls,
      elasticity = estimate,
      std_error = std.error,
      p_value = p.value,
      order = order
    )
}

elasticity_benchmark <- bind_rows(
  tibble(
    source = "Diegmann et al. (2026)",
    specification = c(
      "No worker-heterogeneity controls",
      "Observable worker controls"
    ),
    controls = c(
      "Literature mean",
      "Literature mean"
    ),
    elasticity = c(0.062, 0.036),
    std_error = NA_real_,
    p_value = NA_real_,
    order = 1:2
  ),
  extract_elasticity(
    m_elasticity_raw,
    "Colombia (GEIH)",
    "No controls",
    "None",
    3
  ),
  extract_elasticity(
    m_elasticity_full,
    "Colombia (GEIH)",
    "Full sample",
    "Sector-year FE, worker controls, and formality",
    4
  ),
  extract_elasticity(
    m_elasticity_formal,
    "Colombia (GEIH)",
    "Formal workers",
    "Sector-year FE and worker controls",
    5
  ),
  extract_elasticity(
    m_elasticity_informal,
    "Colombia (GEIH)",
    "Informal workers",
    "Sector-year FE and worker controls",
    6
  )
) %>%
  arrange(order)

dir.create("Paper/tables", recursive = TRUE, showWarnings = FALSE)
write.csv(
  elasticity_benchmark,
  "Paper/tables/regression_elasticity_benchmark.csv",
  row.names = FALSE
)

table_rows <- c()

for (i in seq_len(nrow(elasticity_benchmark))) {
  row <- elasticity_benchmark[i, ]

  if (row$order == 3) {
    table_rows <- c(table_rows, "    \\midrule")
  }

  table_rows <- c(
    table_rows,
    paste0(
      "    ",
      row$source,
      " & ",
      row$specification,
      " & ",
      format_coef(row$elasticity, row$p_value),
      " & ",
      format_se(row$std_error),
      " \\\\"
    )
  )
}

elasticity_table <- c(
  "\\begin{table}[htbp]",
  "  \\centering",
  "  \\caption{Benchmarking Colombia's approximate firm-size wage elasticity}",
  "  \\label{tab:firm-size-elasticity-benchmark}",
  "  \\small",
  "  \\begin{tabular}{p{0.30\\textwidth}p{0.40\\textwidth}cc}",
  "    \\toprule",
  "    Source & Specification & Elasticity & S.E. \\\\",
  "    \\midrule",
  table_rows,
  "    \\bottomrule",
  "  \\end{tabular}",
  "  \\vspace{0.3em}",
  "  \\begin{minipage}{0.95\\textwidth}",
  "  \\footnotesize",
  "  Notes: Diegmann et al. (2026) benchmarks correspond to the mean elasticities reported in their Table 1 for specifications without worker-heterogeneity controls and with observable worker controls. Their worker fixed-effect and AKM employer-effect benchmarks are not reported because GEIH does not follow workers across employers or identify firms. Colombian elasticities are approximate log-log slopes estimated with GEIH expansion weights. The full-sample Colombian specification includes sector-year fixed effects, worker controls, and formality; the formal- and informal-worker specifications include sector-year fixed effects and worker controls. Because GEIH reports firm size in bins, the estimates assign representative values of 1, 2.5, 4.5, 8, 15, 25, 40.5, 75.5, and 150 workers to the harmonized categories from solo work to 101+ workers. Standard errors for the Colombian estimates are clustered by sector. Significance levels: * $p<0.10$, ** $p<0.05$, *** $p<0.01$.",
  "  \\end{minipage}",
  "\\end{table}"
)

writeLines(
  elasticity_table,
  "Paper/sections/regression_elasticity_benchmark_table.tex"
)
