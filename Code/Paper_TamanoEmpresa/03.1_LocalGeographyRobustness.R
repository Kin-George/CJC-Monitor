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

geih <- read_dta(
  geih_personas_data_path()
)

geih_geo <- geih %>%
  mutate(
    anio = as.factor(anio),
    depto = as.factor(depto),
    edad = as.numeric(edad),
    edad2 = edad^2,
    sector = as.factor(sector),
    tamano_empresa = as.factor(tamano_empresa),
    educacion = as.factor(educacion),
    posicion_ocupacional_label = as.factor(posicion_ocupacional_label),
    sector_year = interaction(sector, anio, drop = TRUE),
    depto_year = interaction(depto, anio, drop = TRUE),
    sector_depto = interaction(sector, depto, drop = TRUE),
    sector_depto_year = interaction(sector, depto, anio, drop = TRUE)
  ) %>%
  filter(
    !is.na(log_ingreso_hora_real),
    !is.na(tamano_empresa),
    !is.na(mujer),
    !is.na(edad),
    !is.na(edad2),
    !is.na(educacion),
    !is.na(formal),
    !is.na(sector),
    !is.na(depto),
    !is.na(anio),
    !is.na(fex),
    fex > 0
  )

geo_formula_rhs <- paste(
  "log_ingreso_hora_real ~",
  "i(tamano_empresa, ref = 'Solo') +",
  "mujer + edad + edad2 +",
  "i(educacion, ref = 'Básica secundaria') +",
  "formal"
)

m_geo_sector_year <- feols(
  as.formula(paste(geo_formula_rhs, "| sector_year")),
  weights = ~ fex,
  cluster = ~ sector_depto,
  data = geih_geo
)

m_geo_depto_year <- feols(
  as.formula(paste(geo_formula_rhs, "| sector_year + depto_year")),
  weights = ~ fex,
  cluster = ~ sector_depto,
  data = geih_geo
)

m_geo_local_sector_year <- feols(
  as.formula(paste(geo_formula_rhs, "| sector_depto_year")),
  weights = ~ fex,
  cluster = ~ sector_depto,
  data = geih_geo
)

m_geo_local_sector_year_position <- feols(
  as.formula(paste(
    geo_formula_rhs,
    "+ i(posicion_ocupacional_label)",
    "| sector_depto_year"
  )),
  weights = ~ fex,
  cluster = ~ sector_depto,
  data = geih_geo %>% filter(!is.na(posicion_ocupacional_label))
)

model_list <- list(
  "(1)" = m_geo_sector_year,
  "(2)" = m_geo_depto_year,
  "(3)" = m_geo_local_sector_year,
  "(4)" = m_geo_local_sector_year_position
)

format_coef <- function(x, p) {
  stars <- case_when(
    is.na(p) ~ "",
    p < 0.01 ~ "***",
    p < 0.05 ~ "**",
    p < 0.10 ~ "*",
    TRUE ~ ""
  )
  paste0(sprintf("%.3f", x), stars)
}

format_se <- function(x) {
  paste0("(", sprintf("%.3f", x), ")")
}

format_obs <- function(x) {
  format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
}

size_levels <- c("2-3", "4-5", "6-10", "11-19", "20-30", "31-50", "51-100", "101+")
size_terms <- paste0("tamano_empresa::", size_levels)
size_labels <- paste0("Firm size: ", size_levels)

premium_rows <- bind_rows(
  tidy(m_geo_sector_year, conf.int = TRUE) %>%
    mutate(specification = "Sector-year FE"),
  tidy(m_geo_depto_year, conf.int = TRUE) %>%
    mutate(specification = "Sector-year and department-year FE"),
  tidy(m_geo_local_sector_year, conf.int = TRUE) %>%
    mutate(specification = "Sector-department-year FE"),
  tidy(m_geo_local_sector_year_position, conf.int = TRUE) %>%
    mutate(specification = "Sector-department-year FE and occupational-position controls")
) %>%
  filter(str_detect(term, "^tamano_empresa::")) %>%
  mutate(
    tamano_empresa = str_remove(term, "^tamano_empresa::"),
    premium = 100 * (exp(estimate) - 1),
    ci_low = 100 * (exp(conf.low) - 1),
    ci_high = 100 * (exp(conf.high) - 1)
  )

dir.create("Paper/tables", recursive = TRUE, showWarnings = FALSE)
write.csv(
  premium_rows,
  "Paper/tables/regression_local_geography_robustness.csv",
  row.names = FALSE
)

table_rows <- c()

for (i in seq_along(size_terms)) {
  coefs <- c()
  ses <- c()

  for (model_name in names(model_list)) {
    model_tidy <- tidy(model_list[[model_name]])
    model_row <- model_tidy %>% filter(term == size_terms[i])

    if (nrow(model_row) == 0) {
      coefs <- c(coefs, "")
      ses <- c(ses, "")
    } else {
      coefs <- c(coefs, format_coef(model_row$estimate, model_row$p.value))
      ses <- c(ses, format_se(model_row$std.error))
    }
  }

  table_rows <- c(
    table_rows,
    paste0("    ", size_labels[i], " & ", paste(coefs, collapse = " & "), " \\\\"),
    paste0("     & ", paste(ses, collapse = " & "), " \\\\")
  )
}

n_obs <- vapply(model_list, nobs, numeric(1))
r2_vals <- vapply(model_list, function(m) as.numeric(fitstat(m, "r2")), numeric(1))

regression_table <- c(
  "\\begin{table}[htbp]",
  "  \\centering",
  "  \\caption{Firm-size wage premium robustness checks}",
  "  \\label{tab:firm-size-local-geography-robustness}",
  "  \\small",
  "  \\begin{tabular}{lcccc}",
  "    \\toprule",
  "    & \\multicolumn{4}{c}{Dependent variable: log real hourly labor income} \\\\",
  "    \\cmidrule(lr){2-5}",
  "    & (1) & (2) & (3) & (4) \\\\",
  "    \\midrule",
  table_rows,
  "    \\midrule",
  "    Worker and formality controls & Yes & Yes & Yes & Yes \\\\",
  "    Occupational-position controls & No & No & No & Yes \\\\",
  "    Fixed effects & $s\\times t$ & $s\\times t$, $d\\times t$ & $s\\times d\\times t$ & $s\\times d\\times t$ \\\\",
  paste0("    Observations & ", paste(format_obs(n_obs), collapse = " & "), " \\\\"),
  paste0("    $R^2$ & ", paste(sprintf('%.3f', r2_vals), collapse = " & "), " \\\\"),
  "    \\bottomrule",
  "  \\end{tabular}",
  "  \\vspace{0.3em}",
  "  \\begin{minipage}{0.95\\textwidth}",
  "  \\footnotesize",
  "  Notes: The omitted category is solo workers. The sample is restricted to observations with nonmissing department codes. Worker controls include a female-worker indicator, age, age squared, education dummies, and labor formality. The local geography $d$ is the department, treating Bogot\\'a as a department-equivalent Capital District. Column (3) absorbs sector-department-year fixed effects, so identification comes from wage differences across firm-size categories within the same sector, department, and year. Column (4) adds occupational-position controls to this most stringent local-geography specification. Standard errors are clustered by sector-department cells. Significance levels: * $p<0.10$, ** $p<0.05$, *** $p<0.01$.",
  "  \\end{minipage}",
  "\\end{table}"
)

dir.create("Paper/sections", recursive = TRUE, showWarnings = FALSE)
writeLines(
  regression_table,
  "Paper/sections/regression_local_geography_robustness_table.tex"
)
