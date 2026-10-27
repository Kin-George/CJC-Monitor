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
library(ggplot2)
library(scales)
library(tidyr)

save_figure_versions <- function(base_name, plot_en, plot_es, width, height, dpi = 300) {
  dir.create("Paper/figures", recursive = TRUE, showWarnings = FALSE)

  ggsave(
    filename = file.path("Paper/figures", paste0(base_name, "_en.png")),
    plot = plot_en,
    width = width,
    height = height,
    dpi = dpi
  )

  ggsave(
    filename = file.path("Paper/figures", paste0(base_name, "_es.png")),
    plot = plot_es,
    width = width,
    height = height,
    dpi = dpi
  )

  ggsave(
    filename = file.path("Paper/figures", paste0(base_name, ".png")),
    plot = plot_en,
    width = width,
    height = height,
    dpi = dpi
  )
}

# Modelo enfoque personas
geih <- read_dta(
  geih_personas_data_path()
)

geih <- geih %>%
  mutate(
    anio = as.factor(anio),
    edad = as.numeric(edad),
    edad2 = edad^2,
    sector = as.factor(sector),
    tamano_empresa = as.factor(tamano_empresa),
    educacion = as.factor(educacion),
    formalidad = as.factor(formalidad)
  )

# Usamos una muestra comun para que las columnas de la tabla sean comparables.
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
    anio_num = as.integer(as.character(anio)),
    year_trend = anio_num - min(anio_num, na.rm = TRUE),
    formality_group = factor(
      if_else(formal == 1, "Formal", "Informal"),
      levels = c("Formal", "Informal")
    )
  )

size_levels <- c("2-3", "4-5", "6-10", "11-19", "20-30", "31-50", "51-100", "101+")
education_ref <- grep("secundaria", levels(geih_model$educacion), ignore.case = TRUE, value = TRUE)[1]

if (is.na(education_ref)) {
  stop("No se encontro una categoria de educacion de referencia que contenga 'secundaria'.")
}

m_raw <- feols(
  log_ingreso_hora_real ~
    i(tamano_empresa, ref = "Solo"),
  weights = ~ fex,
  cluster = ~ sector,
  data = geih_model
)

m_fe <- feols(
  log_ingreso_hora_real ~
    i(tamano_empresa, ref = "Solo") |
    sector^anio,
  weights = ~ fex,
  cluster = ~ sector,
  data = geih_model
)

m_demog <- feols(
  log_ingreso_hora_real ~
    i(tamano_empresa, ref = "Solo") +
    mujer +
    edad +
    edad2 +
    i(educacion, ref = education_ref) |
    sector^anio,
  weights = ~ fex,
  cluster = ~ sector,
  data = geih_model
)

m_full <- feols(
  log_ingreso_hora_real ~
    i(tamano_empresa, ref = "Solo") +
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

m_formality_size <- feols(
  log_ingreso_hora_real ~
    i(tamano_empresa, ref = "Solo") * formal +
    mujer +
    edad +
    edad2 +
    i(educacion, ref = education_ref) |
    sector^anio,
  weights = ~ fex,
  cluster = ~ sector,
  data = geih_model
)

summary(m_full)

premium_by_specification <- bind_rows(
  tidy(m_raw, conf.int = TRUE, conf.level = 0.95) %>%
    mutate(specification = "Raw", specification_order = 1),
  tidy(m_fe, conf.int = TRUE, conf.level = 0.95) %>%
    mutate(specification = "Sector-year FE", specification_order = 2),
  tidy(m_demog, conf.int = TRUE, conf.level = 0.95) %>%
    mutate(specification = "+ worker controls", specification_order = 3),
  tidy(m_full, conf.int = TRUE, conf.level = 0.95) %>%
    mutate(specification = "+ formality", specification_order = 4)
) %>%
  filter(str_detect(term, "^tamano_empresa::")) %>%
  mutate(
    tamano_empresa = str_remove(term, "^tamano_empresa::"),
    premium = 100 * (exp(estimate) - 1),
    ci_low = 100 * (exp(conf.low) - 1),
    ci_high = 100 * (exp(conf.high) - 1),
    tamano_empresa = factor(
      tamano_empresa,
      levels = c(
        "2-3",
        "4-5",
        "6-10",
        "11-19",
        "20-30",
        "31-50",
        "51-100",
        "101+"
      )
    ),
    specification = factor(
      specification,
      levels = c(
        "Raw",
        "Sector-year FE",
        "+ worker controls",
        "+ formality"
      )
    )
  ) %>%
  arrange(tamano_empresa, specification_order)

write.csv(
  premium_by_specification,
  "Paper/tables/regression_premium_by_specification.csv",
  row.names = FALSE
)

premium_attenuation_plot <- premium_by_specification %>%
  filter(tamano_empresa %in% c("2-3", "6-10", "101+"))

g_premium_attenuation <- ggplot(
  premium_attenuation_plot,
  aes(
    x = specification,
    y = premium,
    color = tamano_empresa,
    group = tamano_empresa
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_text(
    aes(label = paste0(round(premium, 1), "%")),
    vjust = -0.75,
    size = 3.1,
    show.legend = FALSE
  ) +
  scale_color_manual(
    values = c(
      "2-3" = "darkgreen",
      "6-10" = "darkblue",
      "101+" = "darkred"
    )
  ) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0.08, 0.16))
  ) +
  labs(
    title = "How controls attenuate the firm-size wage premium",
    subtitle = "Premium relative to solo workers for selected firm-size categories",
    x = "Specification",
    y = "Wage premium (%)",
    color = "Firm size"
  ) +
  theme_classic(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 20, hjust = 1),
    legend.position = "bottom"
  )

g_premium_attenuation_es <- g_premium_attenuation +
  scale_x_discrete(
    labels = c(
      "Raw" = "Bruto",
      "Sector-year FE" = "EF sector-a\u00f1o",
      "+ worker controls" = "+ controles trabajador",
      "+ formality" = "+ formalidad"
    )
  ) +
  labs(
    title = "C\u00f3mo los controles aten\u00faan el premium salarial por tama\u00f1o de empresa",
    subtitle = "Premium frente a trabajadores solos para categor\u00edas seleccionadas",
    x = "Especificaci\u00f3n",
    y = "Premium salarial (%)",
    color = "Tama\u00f1o de empresa"
  )

save_figure_versions(
  base_name = "fig68",
  plot_en = g_premium_attenuation,
  plot_es = g_premium_attenuation_es,
  width = 10,
  height = 6,
  dpi = 300
)

#========================================================
# 1. Extraer coeficientes de tamaño de empresa
#========================================================

betas_tamano <- tidy(
  m_full,
  conf.int = TRUE,
  conf.level = 0.95
) %>%
  filter(str_detect(term, "^tamano_empresa::")) %>%
  mutate(
    tamano_empresa = str_remove(term, "^tamano_empresa::"),

    # Transformacion de log puntos a porcentaje
    premium = 100 * (exp(estimate) - 1),
    ci_low = 100 * (exp(conf.low) - 1),
    ci_high = 100 * (exp(conf.high) - 1),

    significativo = ci_low > 0 | ci_high < 0,

    tamano_empresa = factor(
      tamano_empresa,
      levels = c(
        "2-3",
        "4-5",
        "6-10",
        "11-19",
        "20-30",
        "31-50",
        "51-100",
        "101+"
      )
    )
  ) %>%
  arrange(tamano_empresa)


#========================================================
# 2. Revisar tabla de coeficientes transformados
#========================================================

betas_tamano %>%
  select(
    tamano_empresa,
    estimate,
    premium,
    ci_low,
    ci_high,
    significativo,
    p.value
  )


#========================================================
# 3. Grafico del premium salarial por tamaño de empresa
#========================================================

g_premium_tamano <- ggplot(
  betas_tamano,
  aes(
    x = tamano_empresa,
    y = premium
  )
) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "gray45",
    linewidth = 0.7
  ) +
  geom_errorbar(
    aes(
      ymin = ci_low,
      ymax = ci_high,
      color = significativo
    ),
    width = 0.15,
    linewidth = 0.9
  ) +
  geom_point(
    aes(color = significativo),
    size = 3.8
  ) +
  geom_label(
    aes(
      label = paste0(round(premium, 1), "%")
    ),
    fill = "black",
    color = "white",
    fontface = "bold",
    size = 3.8,
    vjust = -0.8,
    linewidth = 0.15,
    show.legend = FALSE
  ) +
  scale_color_manual(
    values = c(
      "TRUE" = "darkblue",
      "FALSE" = "gray55"
    ),
    labels = c(
      "TRUE" = "Significant at 5%",
      "FALSE" = "Not significant"
    )
  ) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0.12, 0.18))
  ) +
  labs(
    title = "Firm-size wage premium in Colombia",
    subtitle = "Wage premium relative to solo workers. 95% confidence intervals",
    x = "Firm size",
    y = "Wage premium (%)",
    color = NULL
  ) +
  theme_classic(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

dir.create("Paper/figures", recursive = TRUE, showWarnings = FALSE)

g_premium_tamano_es <- g_premium_tamano +
  scale_color_manual(
    values = c(
      "TRUE" = "darkblue",
      "FALSE" = "gray55"
    ),
    labels = c(
      "TRUE" = "Significativo al 5%",
      "FALSE" = "No significativo"
    )
  ) +
  labs(
    title = "Premium salarial por tama\u00f1o de empresa en Colombia",
    subtitle = "Premium salarial frente a trabajadores solos. Intervalos de confianza al 95%",
    x = "Tama\u00f1o de empresa",
    y = "Premium salarial (%)",
    color = NULL
  )

save_figure_versions(
  base_name = "fig57",
  plot_en = g_premium_tamano,
  plot_es = g_premium_tamano_es,
  width = 10,
  height = 6,
  dpi = 300
)

interaction_term_for_size <- function(size, coef_names) {
  candidates <- c(
    paste0("formal:tamano_empresa::", size),
    paste0("tamano_empresa::", size, ":formal")
  )
  match <- candidates[candidates %in% coef_names]
  if (length(match) == 0) {
    return(NA_character_)
  }
  match[[1]]
}

linear_combo <- function(coefs, vcov_mat, terms, weights) {
  keep <- !is.na(terms)
  terms <- terms[keep]
  weights <- weights[keep]

  estimate <- sum(coefs[terms] * weights)
  vcov_sub <- vcov_mat[terms, terms, drop = FALSE]
  se <- sqrt(as.numeric(t(weights) %*% vcov_sub %*% weights))
  z <- estimate / se
  p_value <- 2 * pnorm(abs(z), lower.tail = FALSE)
  ci_low <- estimate - 1.96 * se
  ci_high <- estimate + 1.96 * se

  tibble(
    estimate = estimate,
    std.error = se,
    p.value = p_value,
    conf.low = ci_low,
    conf.high = ci_high
  )
}

formality_coefs <- coef(m_formality_size)
formality_vcov <- vcov(m_formality_size)
formality_coef_names <- names(formality_coefs)

formality_size_premium <- bind_rows(lapply(size_levels, function(size) {
  size_term <- paste0("tamano_empresa::", size)
  interaction_term <- interaction_term_for_size(size, formality_coef_names)

  informal <- linear_combo(
    coefs = formality_coefs,
    vcov_mat = formality_vcov,
    terms = c(size_term),
    weights = c(1)
  ) %>%
    mutate(formality = "Informal", tamano_empresa = size)

  formal <- linear_combo(
    coefs = formality_coefs,
    vcov_mat = formality_vcov,
    terms = c(size_term, interaction_term),
    weights = c(1, 1)
  ) %>%
    mutate(formality = "Formal", tamano_empresa = size)

  bind_rows(formal, informal)
})) %>%
  mutate(
    premium = 100 * (exp(estimate) - 1),
    ci_low = 100 * (exp(conf.low) - 1),
    ci_high = 100 * (exp(conf.high) - 1),
    significativo = ci_low > 0 | ci_high < 0,
    tamano_empresa = factor(tamano_empresa, levels = size_levels),
    formality = factor(formality, levels = c("Formal", "Informal"))
  ) %>%
  arrange(formality, tamano_empresa)

write.csv(
  formality_size_premium,
  "Paper/tables/regression_formality_size_premium.csv",
  row.names = FALSE
)

formality_interaction_coefficients <- bind_rows(lapply(size_levels, function(size) {
  size_term <- paste0("tamano_empresa::", size)
  interaction_term <- interaction_term_for_size(size, formality_coef_names)

  beta <- linear_combo(
    coefs = formality_coefs,
    vcov_mat = formality_vcov,
    terms = c(size_term),
    weights = c(1)
  ) %>%
    mutate(
      tamano_empresa = size,
      parameter = "beta",
      parameter_label = "$\\hat{\\beta}_g$"
    )

  theta <- linear_combo(
    coefs = formality_coefs,
    vcov_mat = formality_vcov,
    terms = c(interaction_term),
    weights = c(1)
  ) %>%
    mutate(
      tamano_empresa = size,
      parameter = "theta",
      parameter_label = "$\\hat{\\theta}_g$"
    )

  formal_sum <- linear_combo(
    coefs = formality_coefs,
    vcov_mat = formality_vcov,
    terms = c(size_term, interaction_term),
    weights = c(1, 1)
  ) %>%
    mutate(
      tamano_empresa = size,
      parameter = "beta_plus_theta",
      parameter_label = "$\\hat{\\beta}_g+\\hat{\\theta}_g$"
    )

  bind_rows(beta, theta, formal_sum)
})) %>%
  mutate(
    tamano_empresa = factor(tamano_empresa, levels = size_levels),
    parameter = factor(parameter, levels = c("beta", "theta", "beta_plus_theta")),
    premium = 100 * (exp(estimate) - 1),
    ci_low = 100 * (exp(conf.low) - 1),
    ci_high = 100 * (exp(conf.high) - 1)
  ) %>%
  arrange(tamano_empresa, parameter)

write.csv(
  formality_interaction_coefficients,
  "Paper/tables/regression_formality_interaction_coefficients.csv",
  row.names = FALSE
)

if (!("formal" %in% formality_coef_names)) {
  stop("The formal main-effect coefficient was not found in the formality-size model.")
}

formality_gap_by_size <- bind_rows(lapply(c("Solo", size_levels), function(size) {
  if (size == "Solo") {
    terms <- c("formal")
    weights <- c(1)
  } else {
    interaction_term <- interaction_term_for_size(size, formality_coef_names)
    terms <- c("formal", interaction_term)
    weights <- c(1, 1)
  }

  linear_combo(
    coefs = formality_coefs,
    vcov_mat = formality_vcov,
    terms = terms,
    weights = weights
  ) %>%
    mutate(tamano_empresa = size)
})) %>%
  mutate(
    tamano_empresa = factor(tamano_empresa, levels = c("Solo", size_levels)),
    premium = 100 * (exp(estimate) - 1),
    ci_low = 100 * (exp(conf.low) - 1),
    ci_high = 100 * (exp(conf.high) - 1),
    significativo = ci_low > 0 | ci_high < 0
  ) %>%
  arrange(tamano_empresa)

write.csv(
  formality_gap_by_size,
  "Paper/tables/regression_formality_gap_by_size.csv",
  row.names = FALSE
)

formal_reference_sizes <- c("Solo", size_levels[size_levels != "101+"])
formal_101_contrasts <- bind_rows(lapply(formal_reference_sizes, function(reference_size) {
  large_size_term <- "tamano_empresa::101+"
  large_interaction_term <- interaction_term_for_size("101+", formality_coef_names)

  if (reference_size == "Solo") {
    terms <- c(large_size_term, large_interaction_term)
    weights <- c(1, 1)
  } else {
    reference_size_term <- paste0("tamano_empresa::", reference_size)
    reference_interaction_term <- interaction_term_for_size(reference_size, formality_coef_names)
    terms <- c(
      large_size_term,
      large_interaction_term,
      reference_size_term,
      reference_interaction_term
    )
    weights <- c(1, 1, -1, -1)
  }

  linear_combo(
    coefs = formality_coefs,
    vcov_mat = formality_vcov,
    terms = terms,
    weights = weights
  ) %>%
    mutate(
      comparison = paste0("101+ vs ", reference_size),
      reference_size = reference_size
    )
})) %>%
  mutate(
    premium = 100 * (exp(estimate) - 1),
    ci_low = 100 * (exp(conf.low) - 1),
    ci_high = 100 * (exp(conf.high) - 1),
    significativo = ci_low > 0 | ci_high < 0,
    reference_size = factor(reference_size, levels = formal_reference_sizes)
  ) %>%
  arrange(reference_size)

write.csv(
  formal_101_contrasts,
  "Paper/tables/regression_formal_101_contrasts.csv",
  row.names = FALSE
)

format_p_value <- function(x) {
  ifelse(x < 0.001, "$<0.001$", sprintf("%.3f", x))
}

format_log_coef <- function(x, p) {
  stars <- case_when(
    is.na(p) ~ "",
    p < 0.01 ~ "***",
    p < 0.05 ~ "**",
    p < 0.10 ~ "*",
    TRUE ~ ""
  )
  paste0(sprintf("%.3f", x), stars)
}

format_log_se <- function(x) {
  paste0("(", sprintf("%.3f", x), ")")
}

formality_interaction_wide <- formality_interaction_coefficients %>%
  mutate(
    coef_label = format_log_coef(estimate, p.value),
    se_label = format_log_se(std.error)
  ) %>%
  select(tamano_empresa, parameter, coef_label, se_label) %>%
  tidyr::pivot_wider(
    names_from = parameter,
    values_from = c(coef_label, se_label)
  )

formality_interaction_table_rows <- unlist(lapply(seq_len(nrow(formality_interaction_wide)), function(i) {
  row <- formality_interaction_wide[i, ]
  c(
    paste0(
      "    ",
      as.character(row$tamano_empresa),
      " & ",
      row$coef_label_beta,
      " & ",
      row$coef_label_theta,
      " & ",
      row$coef_label_beta_plus_theta,
      " \\\\"
    ),
    paste0(
      "     & ",
      row$se_label_beta,
      " & ",
      row$se_label_theta,
      " & ",
      row$se_label_beta_plus_theta,
      " \\\\"
    )
  )
}))

formality_interaction_table <- c(
  "\\begin{table}[htbp]",
  "  \\centering",
  "  \\caption{Firm-size coefficients by formality status}",
  "  \\label{tab:formality-interaction-coefficients}",
  "  \\small",
  "  \\begin{tabular}{lccc}",
  "    \\toprule",
  "    Firm size & $\\hat{\\beta}_g$ & $\\hat{\\theta}_g$ & $\\hat{\\beta}_g+\\hat{\\theta}_g$ \\\\",
  "    \\midrule",
  formality_interaction_table_rows,
  "    \\bottomrule",
  "  \\end{tabular}",
  "  \\vspace{0.3em}",
  "  \\begin{minipage}{0.92\\textwidth}",
  "  \\footnotesize",
  "  Notes: Estimates correspond to Equation~(\\ref{eq:formality-interaction}). The omitted category is solo workers within each formality group. $\\hat{\\beta}_g$ is the firm-size coefficient among informal workers. $\\hat{\\theta}_g$ is the additional coefficient for formal workers in the same firm-size category. $\\hat{\\beta}_g+\\hat{\\theta}_g$ is the implied firm-size coefficient among formal workers. All coefficients are in log points. Standard errors, clustered by sector, are reported in parentheses. The specification controls for gender, age, age squared, education, and sector-year fixed effects. Significance levels: * $p<0.10$, ** $p<0.05$, *** $p<0.01$.",
  "  \\end{minipage}",
  "\\end{table}"
)

writeLines(
  formality_interaction_table,
  "Paper/sections/regression_formality_interaction_coefficients_table.tex"
)

formality_gap_table_rows <- formality_gap_by_size %>%
  mutate(
    size_label = as.character(tamano_empresa),
    coef_label = format_log_coef(estimate, p.value),
    se_label = format_log_se(std.error),
    premium_label = sprintf("%.1f\\%%", premium),
    p_label = format_p_value(p.value)
  ) %>%
  transmute(
    coef_row = paste0(
      "    ",
      size_label,
      " & ",
      coef_label,
      " & ",
      premium_label,
      " & ",
      p_label,
      " \\\\"
    ),
    se_row = paste0("     & ", se_label, " &  &  \\\\")
  ) %>%
  tidyr::unite("row", coef_row, se_row, sep = "\n") %>%
  pull(row) %>%
  paste(collapse = "\n")

formality_gap_table <- c(
  "\\begin{table}[htbp]",
  "  \\centering",
  "  \\caption{Conditional formal-informal wage gap by firm size}",
  "  \\label{tab:formal-wage-gap-by-size}",
  "  \\small",
  "  \\begin{tabular}{lccc}",
  "    \\toprule",
  "    Firm size & Log gap & Premium & $p$-value \\\\",
  "    \\midrule",
  formality_gap_table_rows,
  "    \\bottomrule",
  "  \\end{tabular}",
  "  \\vspace{0.3em}",
  "  \\begin{minipage}{0.92\\textwidth}",
  "  \\footnotesize",
  "  Notes: Each row reports the estimated wage gap between formal and informal workers within the same firm-size category from Equation~(\\ref{eq:formality-interaction}). For solo workers, the gap is $\\hat{\\phi}$; for all other categories, it is $\\hat{\\phi}+\\hat{\\theta}_g$. Premiums are computed as $100\\times[\\exp(\\widehat{gap})-1]$. Standard errors, clustered by sector, are reported in parentheses below the log gap. The specification controls for gender, age, age squared, education, and sector-year fixed effects. Significance levels: * $p<0.10$, ** $p<0.05$, *** $p<0.01$.",
  "  \\end{minipage}",
  "\\end{table}"
)

writeLines(
  formality_gap_table,
  "Paper/sections/regression_formality_gap_by_size_table.tex"
)

formal_101_table_rows <- formal_101_contrasts %>%
  mutate(
    reference_label = case_when(
      reference_size == "Solo" ~ "Solo workers",
      TRUE ~ as.character(reference_size)
    ),
    premium_label = sprintf("%.1f\\%%", premium),
    p_label = format_p_value(p.value)
  ) %>%
  transmute(
    row = paste0(
      "    ",
      reference_label,
      " & ",
      premium_label,
      " & ",
      p_label,
      " \\\\"
    )
  ) %>%
  pull(row)

formal_101_table <- c(
  "\\begin{table}[htbp]",
  "  \\centering",
  "  \\caption{Formal-worker large-firm wage premium relative to other formal firm-size categories}",
  "  \\label{tab:formal-101-contrasts}",
  "  \\small",
  "  \\begin{tabular}{lcc}",
  "    \\toprule",
  "    Reference category & Premium & $p$-value \\\\",
  "    \\midrule",
  formal_101_table_rows,
  "    \\bottomrule",
  "  \\end{tabular}",
  "  \\vspace{0.3em}",
  "  \\begin{minipage}{0.92\\textwidth}",
  "  \\footnotesize",
  "  Notes: Each row compares formal workers in firms with 101 or more workers with formal workers in the reference firm-size category. Estimates come from the firm-size-by-formality specification with gender, age, age squared, education, and sector-year fixed effects. Premiums are computed as $100\\times[\\exp(\\hat{\\beta})-1]$ from the relevant linear contrast.",
  "  \\end{minipage}",
  "\\end{table}"
)

writeLines(
  formal_101_table,
  "Paper/sections/regression_formal_101_contrasts_table.tex"
)

g_formality_size_premium <- ggplot(
  formality_size_premium,
  aes(
    x = tamano_empresa,
    y = premium,
    group = formality
  )
) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "gray45",
    linewidth = 0.7
  ) +
  geom_errorbar(
    aes(ymin = ci_low, ymax = ci_high),
    width = 0.15,
    color = "gray35",
    linewidth = 0.8
  ) +
  geom_line(
    color = "darkblue",
    linewidth = 1
  ) +
  geom_point(
    aes(shape = significativo),
    color = "darkblue",
    size = 3.5
  ) +
  geom_label(
    aes(label = paste0(round(premium, 1), "%")),
    fill = "black",
    color = "white",
    fontface = "bold",
    size = 3.2,
    vjust = -0.75,
    linewidth = 0.15,
    show.legend = FALSE
  ) +
  facet_wrap(~ formality, ncol = 2) +
  scale_shape_manual(
    values = c(
      "TRUE" = 16,
      "FALSE" = 1
    ),
    labels = c(
      "TRUE" = "Significant at 5%",
      "FALSE" = "Not significant"
    )
  ) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0.14, 0.22))
  ) +
  labs(
    title = "Adjusted firm-size wage premium by formality status",
    subtitle = "Premium relative to solo workers within each formality group",
    x = "Firm size",
    y = "Wage premium (%)",
    shape = NULL
  ) +
  theme_classic(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom"
  )

g_formality_size_premium_es <- g_formality_size_premium +
  facet_wrap(
    ~ formality,
    ncol = 2,
    labeller = as_labeller(c(
      "Formal" = "Trabajadores formales",
      "Informal" = "Trabajadores informales"
    ))
  ) +
  scale_shape_manual(
    values = c(
      "TRUE" = 16,
      "FALSE" = 1
    ),
    labels = c(
      "TRUE" = "Significativo al 5%",
      "FALSE" = "No significativo"
    )
  ) +
  labs(
    title = "Premium salarial ajustado por tama\u00f1o de empresa y formalidad",
    subtitle = "Premium frente a trabajadores solos dentro de cada grupo de formalidad",
    x = "Tama\u00f1o de empresa",
    y = "Premium salarial (%)",
    shape = NULL
  )

save_figure_versions(
  base_name = "fig74",
  plot_en = g_formality_size_premium,
  plot_es = g_formality_size_premium_es,
  width = 11,
  height = 6.2,
  dpi = 300
)

year_levels <- sort(unique(geih_model$anio_num))

formality_size_year_premium <- bind_rows(lapply(year_levels, function(year_value) {
  data_year <- geih_model %>%
    filter(anio_num == year_value)

  model_year <- feols(
    log_ingreso_hora_real ~
      i(tamano_empresa, ref = "Solo") * formal +
      mujer +
      edad +
      edad2 +
      i(educacion, ref = education_ref) |
      sector,
    weights = ~ fex,
    cluster = ~ sector,
    data = data_year
  )

  year_coefs <- coef(model_year)
  year_vcov <- vcov(model_year)
  year_coef_names <- names(year_coefs)

  bind_rows(lapply(size_levels, function(size_value) {
    size_term <- paste0("tamano_empresa::", size_value)
    interaction_term <- interaction_term_for_size(size_value, year_coef_names)

    informal <- linear_combo(
      coefs = year_coefs,
      vcov_mat = year_vcov,
      terms = c(size_term),
      weights = c(1)
    ) %>%
      mutate(
        formality = "Informal",
        anio = year_value,
        tamano_empresa = size_value
      )

    formal <- linear_combo(
      coefs = year_coefs,
      vcov_mat = year_vcov,
      terms = c(size_term, interaction_term),
      weights = c(1, 1)
    ) %>%
      mutate(
        formality = "Formal",
        anio = year_value,
        tamano_empresa = size_value
      )

    bind_rows(formal, informal)
  }))
})) %>%
  mutate(
    premium = 100 * (exp(estimate) - 1),
    ci_low = 100 * (exp(conf.low) - 1),
    ci_high = 100 * (exp(conf.high) - 1),
    significativo = ci_low > 0 | ci_high < 0,
    tamano_empresa = factor(tamano_empresa, levels = size_levels),
    formality = factor(formality, levels = c("Formal", "Informal"))
  ) %>%
  arrange(formality, tamano_empresa, anio)

write.csv(
  formality_size_year_premium,
  "Paper/tables/regression_formality_size_year_premium.csv",
  row.names = FALSE
)

large_firm_formality_year_premium <- formality_size_year_premium %>%
  filter(tamano_empresa == "101+")

large_firm_labels <- large_firm_formality_year_premium %>%
  group_by(formality) %>%
  filter(anio == max(anio, na.rm = TRUE)) %>%
  ungroup()

g_large_firm_formality_year <- ggplot(
  large_firm_formality_year_premium,
  aes(
    x = anio,
    y = premium,
    color = formality,
    fill = formality
  )
) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "gray45",
    linewidth = 0.6
  ) +
  geom_ribbon(
    aes(ymin = ci_low, ymax = ci_high),
    alpha = 0.12,
    color = NA
  ) +
  geom_line(linewidth = 1) +
  geom_point(
    aes(shape = significativo),
    size = 2.4
  ) +
  geom_label(
    data = large_firm_labels,
    aes(label = paste0(round(premium, 1), "%")),
    fill = "black",
    color = "white",
    fontface = "bold",
    size = 3.2,
    nudge_x = 0.35,
    show.legend = FALSE
  ) +
  scale_color_manual(
    values = c("Formal" = "darkblue", "Informal" = "darkred")
  ) +
  scale_fill_manual(
    values = c("Formal" = "darkblue", "Informal" = "darkred")
  ) +
  scale_shape_manual(
    values = c(
      "TRUE" = 16,
      "FALSE" = 1
    ),
    labels = c(
      "TRUE" = "Significant at 5%",
      "FALSE" = "Not significant"
    )
  ) +
  scale_x_continuous(
    breaks = seq(min(year_levels), max(year_levels), by = 4),
    expand = expansion(mult = c(0.02, 0.08))
  ) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0.12, 0.18))
  ) +
  labs(
    title = "Adjusted large-firm wage premium by formality over time",
    subtitle = "Premium in firms with 101+ workers relative to solo workers within each formality-year group",
    x = "Year",
    y = "Wage premium (%)",
    color = "Formality",
    fill = "Formality",
    shape = NULL
  ) +
  theme_classic(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

g_large_firm_formality_year_es <- g_large_firm_formality_year +
  scale_color_manual(
    values = c("Formal" = "darkblue", "Informal" = "darkred"),
    labels = c("Formal" = "Formal", "Informal" = "Informal")
  ) +
  scale_fill_manual(
    values = c("Formal" = "darkblue", "Informal" = "darkred"),
    labels = c("Formal" = "Formal", "Informal" = "Informal")
  ) +
  scale_shape_manual(
    values = c(
      "TRUE" = 16,
      "FALSE" = 1
    ),
    labels = c(
      "TRUE" = "Significativo al 5%",
      "FALSE" = "No significativo"
    )
  ) +
  labs(
    title = "Premium salarial ajustado en firmas grandes por formalidad",
    subtitle = "Premium en firmas de 101+ trabajadores frente a trabajadores solos dentro de cada formalidad-a\u00f1o",
    x = "A\u00f1o",
    y = "Premium salarial (%)",
    color = "Formalidad",
    fill = "Formalidad",
    shape = NULL
  )

save_figure_versions(
  base_name = "fig75",
  plot_en = g_large_firm_formality_year,
  plot_es = g_large_firm_formality_year_es,
  width = 10.5,
  height = 6.2,
  dpi = 300
)

endpoint_years <- c(2008, 2025)

endpoint_adjusted_premium <- formality_size_year_premium %>%
  filter(anio %in% endpoint_years) %>%
  select(
    formality,
    anio,
    tamano_empresa,
    premium,
    ci_low,
    ci_high,
    significativo
  ) %>%
  bind_rows(
    expand.grid(
      formality = levels(formality_size_year_premium$formality),
      anio = endpoint_years,
      tamano_empresa = "Solo",
      stringsAsFactors = FALSE
    ) %>%
      mutate(
        premium = 0,
        ci_low = NA_real_,
        ci_high = NA_real_,
        significativo = TRUE
      )
  ) %>%
  mutate(
    tamano_empresa = factor(
      as.character(tamano_empresa),
      levels = c("Solo", size_levels)
    ),
    anio = factor(anio, levels = endpoint_years),
    formality = factor(as.character(formality), levels = c("Formal", "Informal"))
  ) %>%
  arrange(formality, anio, tamano_empresa)

endpoint_adjusted_labels <- endpoint_adjusted_premium %>%
  filter(tamano_empresa == "101+", anio == "2025")

g_endpoint_adjusted_formality <- ggplot(
  endpoint_adjusted_premium,
  aes(
    x = tamano_empresa,
    y = premium,
    color = anio,
    group = anio
  )
) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "gray45",
    linewidth = 0.6
  ) +
  geom_errorbar(
    data = endpoint_adjusted_premium %>% filter(tamano_empresa != "Solo"),
    aes(ymin = ci_low, ymax = ci_high),
    width = 0.12,
    linewidth = 0.75,
    position = position_dodge(width = 0.35)
  ) +
  geom_line(
    aes(linetype = anio),
    linewidth = 1
  ) +
  geom_point(
    aes(shape = anio),
    size = 2.8,
    position = position_dodge(width = 0.35)
  ) +
  geom_label(
    data = endpoint_adjusted_labels,
    aes(label = paste0(round(premium, 1), "%")),
    fill = "black",
    color = "white",
    fontface = "bold",
    size = 3,
    nudge_x = 0.25,
    show.legend = FALSE
  ) +
  facet_wrap(~ formality, ncol = 2) +
  scale_color_manual(
    values = c("2008" = "#d95f02", "2025" = "#0072B2")
  ) +
  scale_linetype_manual(
    values = c("2008" = "dashed", "2025" = "solid")
  ) +
  scale_shape_manual(
    values = c("2008" = 1, "2025" = 16)
  ) +
  scale_x_discrete(
    limits = c("Solo", size_levels),
    drop = FALSE
  ) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0.14, 0.18))
  ) +
  labs(
    title = "Adjusted firm-size wage premium by formality, 2008 and 2025",
    subtitle = "Premium relative to solo workers within each formality-year group. Controls: gender, age, education, and sector",
    x = "Firm size",
    y = "Adjusted wage premium (%)",
    color = "Year",
    linetype = "Year",
    shape = "Year"
  ) +
  theme_classic(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom"
  )

g_endpoint_adjusted_formality_es <- g_endpoint_adjusted_formality +
  facet_wrap(
    ~ formality,
    ncol = 2,
    labeller = as_labeller(c(
      "Formal" = "Trabajadores formales",
      "Informal" = "Trabajadores informales"
    ))
  ) +
  scale_color_manual(
    values = c("2008" = "#d95f02", "2025" = "#0072B2")
  ) +
  scale_linetype_manual(
    values = c("2008" = "dashed", "2025" = "solid")
  ) +
  scale_shape_manual(
    values = c("2008" = 1, "2025" = 16)
  ) +
  labs(
    title = "Premium salarial ajustado por tama\u00f1o y formalidad, 2008 y 2025",
    subtitle = "Premium frente a trabajadores solos dentro de cada formalidad-a\u00f1o. Controles: g\u00e9nero, edad, educaci\u00f3n y sector",
    x = "Tama\u00f1o de empresa",
    y = "Premium salarial ajustado (%)",
    color = "A\u00f1o",
    linetype = "A\u00f1o",
    shape = "A\u00f1o"
  )

save_figure_versions(
  base_name = "fig76",
  plot_en = g_endpoint_adjusted_formality,
  plot_es = g_endpoint_adjusted_formality_es,
  width = 11,
  height = 6.2,
  dpi = 300
)

g_beta_tamano <- ggplot(
  betas_tamano,
  aes(
    x = tamano_empresa,
    y = estimate
  )
) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "gray45",
    linewidth = 0.7
  ) +
  geom_errorbar(
    aes(
      ymin = conf.low,
      ymax = conf.high,
      color = significativo
    ),
    width = 0.15,
    linewidth = 0.9
  ) +
  geom_point(
    aes(color = significativo),
    size = 3.8
  ) +
  geom_label(
    aes(
      label = sprintf("%.3f", estimate)
    ),
    fill = "black",
    color = "white",
    fontface = "bold",
    size = 3.6,
    vjust = -0.8,
    linewidth = 0.15,
    show.legend = FALSE
  ) +
  scale_color_manual(
    values = c(
      "TRUE" = "darkblue",
      "FALSE" = "gray55"
    ),
    labels = c(
      "TRUE" = "Significant at 5%",
      "FALSE" = "Not significant"
    )
  ) +
  scale_y_continuous(
    labels = number_format(accuracy = 0.01),
    expand = expansion(mult = c(0.12, 0.18))
  ) +
  labs(
    title = "Firm-size coefficients in the full specification",
    subtitle = "Column (4): gender, age, education, formality, and sector-year fixed effects",
    x = "Firm size",
    y = "Estimated coefficient in log points",
    color = NULL
  ) +
  theme_classic(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

g_beta_tamano_es <- g_beta_tamano +
  scale_color_manual(
    values = c(
      "TRUE" = "darkblue",
      "FALSE" = "gray55"
    ),
    labels = c(
      "TRUE" = "Significativo al 5%",
      "FALSE" = "No significativo"
    )
  ) +
  labs(
    title = "Coeficientes de tama\u00f1o de empresa en la especificaci\u00f3n completa",
    subtitle = "Columna (4): g\u00e9nero, edad, educaci\u00f3n, formalidad y efectos fijos sector-a\u00f1o",
    x = "Tama\u00f1o de empresa",
    y = "Coeficiente estimado en log puntos",
    color = NULL
  )

save_figure_versions(
  base_name = "fig58",
  plot_en = g_beta_tamano,
  plot_es = g_beta_tamano_es,
  width = 10,
  height = 6,
  dpi = 300
)

#========================================================
# 4. Premium salarial por tamano de empresa y ano
#========================================================

m_dynamic <- feols(
  log_ingreso_hora_real ~
    i(anio, tamano_empresa, ref2 = "Solo") +
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

dynamic_terms <- tidy(
  m_dynamic,
  conf.int = TRUE,
  conf.level = 0.95
) %>%
  filter(str_detect(term, "^anio::"))

dynamic_parts <- str_match(
  dynamic_terms$term,
  "^anio::([^:]+):tamano_empresa::(.+)$"
)

betas_tamano_anio <- dynamic_terms %>%
  mutate(
    anio = as.integer(dynamic_parts[, 2]),
    tamano_empresa = dynamic_parts[, 3],

    # Transformacion de log puntos a porcentaje
    premium = 100 * (exp(estimate) - 1),
    ci_low = 100 * (exp(conf.low) - 1),
    ci_high = 100 * (exp(conf.high) - 1),

    significativo = ci_low > 0 | ci_high < 0,

    tamano_empresa = factor(
      tamano_empresa,
      levels = c(
        "2-3",
        "4-5",
        "6-10",
        "11-19",
        "20-30",
        "31-50",
        "51-100",
        "101+"
      )
    )
  ) %>%
  filter(!is.na(anio), !is.na(tamano_empresa)) %>%
  arrange(tamano_empresa, anio)

betas_tamano_anio %>%
  select(
    anio,
    tamano_empresa,
    estimate,
    premium,
    ci_low,
    ci_high,
    significativo,
    p.value
  )

g_premium_tamano_anio <- ggplot(
  betas_tamano_anio,
  aes(
    x = anio,
    y = premium,
    group = tamano_empresa
  )
) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "gray45",
    linewidth = 0.5
  ) +
  geom_ribbon(
    aes(
      ymin = ci_low,
      ymax = ci_high
    ),
    fill = "darkblue",
    alpha = 0.12,
    color = NA
  ) +
  geom_line(
    color = "darkblue",
    linewidth = 0.9
  ) +
  geom_point(
    aes(shape = significativo),
    color = "darkblue",
    size = 2
  ) +
  facet_wrap(~ tamano_empresa, ncol = 4) +
  scale_shape_manual(
    values = c(
      "TRUE" = 16,
      "FALSE" = 1
    ),
    labels = c(
      "TRUE" = "Significant at 5%",
      "FALSE" = "Not significant"
    )
  ) +
  scale_x_continuous(
    breaks = seq(
      min(betas_tamano_anio$anio, na.rm = TRUE),
      max(betas_tamano_anio$anio, na.rm = TRUE),
      by = 4
    )
  ) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0.12, 0.12))
  ) +
  labs(
    title = "Firm-size wage premium over time in Colombia",
    subtitle = "Year-specific premiums relative to solo workers. Full specification with 95% confidence intervals",
    x = "Year",
    y = "Wage premium (%)",
    shape = NULL
  ) +
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom"
  )

g_premium_tamano_anio_es <- g_premium_tamano_anio +
  scale_shape_manual(
    values = c(
      "TRUE" = 16,
      "FALSE" = 1
    ),
    labels = c(
      "TRUE" = "Significativo al 5%",
      "FALSE" = "No significativo"
    )
  ) +
  labs(
    title = "Premium salarial por tama\u00f1o de empresa en el tiempo en Colombia",
    subtitle = "Premium por a\u00f1o frente a trabajadores solos. Especificaci\u00f3n completa con intervalos de confianza al 95%",
    x = "A\u00f1o",
    y = "Premium salarial (%)",
    shape = NULL
  )

save_figure_versions(
  base_name = "fig59",
  plot_en = g_premium_tamano_anio,
  plot_es = g_premium_tamano_anio_es,
  width = 12,
  height = 7,
  dpi = 300
)

#========================================================
# 5. Test de tendencia del premium salarial
#========================================================

m_trend <- feols(
  log_ingreso_hora_real ~
    i(tamano_empresa, ref = "Solo") +
    i(tamano_empresa, year_trend, ref = "Solo") +
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

trend_horizon <- max(geih_model$year_trend, na.rm = TRUE)

trend_terms <- tidy(
  m_trend,
  conf.int = TRUE,
  conf.level = 0.95
) %>%
  filter(str_detect(term, ":year_trend$") | str_detect(term, "^year_trend:"))

trend_size_a <- str_match(
  trend_terms$term,
  "^tamano_empresa::(.+):year_trend$"
)

trend_size_b <- str_match(
  trend_terms$term,
  "^year_trend:tamano_empresa::(.+)$"
)

trend_test <- trend_terms %>%
  mutate(
    tamano_empresa = coalesce(trend_size_a[, 2], trend_size_b[, 2]),
    p_trend = p.value,
    period_change_log_points = estimate * trend_horizon,
    period_change_percent = 100 * (exp(period_change_log_points) - 1),
    significant_trend = p_trend < 0.05,
    tamano_empresa = factor(
      tamano_empresa,
      levels = c(
        "2-3",
        "4-5",
        "6-10",
        "11-19",
        "20-30",
        "31-50",
        "51-100",
        "101+"
      )
    )
  ) %>%
  filter(!is.na(tamano_empresa)) %>%
  arrange(tamano_empresa)

trend_test %>%
  select(
    tamano_empresa,
    estimate,
    std.error,
    p_trend,
    period_change_log_points,
    period_change_percent,
    significant_trend
  )

g_trend_test <- ggplot(
  trend_test,
  aes(
    x = tamano_empresa,
    y = estimate * 100
  )
) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "gray45",
    linewidth = 0.7
  ) +
  geom_errorbar(
    aes(
      ymin = conf.low * 100,
      ymax = conf.high * 100,
      color = significant_trend
    ),
    width = 0.15,
    linewidth = 0.9
  ) +
  geom_point(
    aes(color = significant_trend),
    size = 3.8
  ) +
  geom_label(
    aes(
      label = sprintf("%.2f", estimate * 100)
    ),
    fill = "black",
    color = "white",
    fontface = "bold",
    size = 3.6,
    vjust = -0.8,
    linewidth = 0.15,
    show.legend = FALSE
  ) +
  scale_color_manual(
    values = c(
      "TRUE" = "darkblue",
      "FALSE" = "gray55"
    ),
    labels = c(
      "TRUE" = "Significant trend at 5%",
      "FALSE" = "Not significant"
    )
  ) +
  scale_y_continuous(
    labels = number_format(accuracy = 0.1),
    expand = expansion(mult = c(0.12, 0.18))
  ) +
  labs(
    title = "Test of the firm-size wage premium trend",
    subtitle = "Slope of the year trend by firm-size category. Two-sided test: trend different from zero",
    x = "Firm size",
    y = "Annual premium change (log points x 100)",
    color = NULL
  ) +
  theme_classic(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

g_trend_test_es <- g_trend_test +
  scale_color_manual(
    values = c(
      "TRUE" = "darkblue",
      "FALSE" = "gray55"
    ),
    labels = c(
      "TRUE" = "Tendencia significativa al 5%",
      "FALSE" = "No significativo"
    )
  ) +
  labs(
    title = "Test de tendencia del premium salarial por tama\u00f1o de empresa",
    subtitle = "Pendiente de la tendencia anual por tama\u00f1o de empresa. Test bilateral: tendencia distinta de cero",
    x = "Tama\u00f1o de empresa",
    y = "Cambio anual del premium (log puntos x 100)",
    color = NULL
  )

save_figure_versions(
  base_name = "fig60",
  plot_en = g_trend_test,
  plot_es = g_trend_test_es,
  width = 10,
  height = 6,
  dpi = 300
)

trend_table_rows <- c()

for (i in seq_len(nrow(trend_test))) {
  trend_table_rows <- c(
    trend_table_rows,
    paste0(
      "    ",
      trend_test$tamano_empresa[i],
      " & ",
      sprintf("%.4f", trend_test$estimate[i]),
      " & ",
      sprintf("(%.4f)", trend_test$std.error[i]),
      " & ",
      sprintf("%.3f", trend_test$p_trend[i]),
      " & ",
      sprintf("%.1f\\%%", trend_test$period_change_percent[i]),
      " \\\\"
    )
  )
}

trend_test_table <- c(
  "\\begin{table}[htbp]",
  "  \\centering",
  "  \\caption{Testing whether the firm-size wage premium changed over time}",
  "  \\label{tab:firm-size-premium-trend-test}",
  "  \\small",
  "  \\begin{tabular}{lcccc}",
  "    \\toprule",
  "    Firm size & Annual trend & S.E. & $p$-value & Implied 2008--2025 change \\\\",
  "    \\midrule",
  trend_table_rows,
  "    \\bottomrule",
  "  \\end{tabular}",
  "  \\vspace{0.3em}",
  "  \\begin{minipage}{0.95\\textwidth}",
  "  \\footnotesize",
  "  Notes: The annual trend is the coefficient on the interaction between firm-size category and a linear year trend, with solo workers as the omitted category. The specification controls for gender, age, age squared, education, formality, and sector-year fixed effects, and uses GEIH expansion weights. Standard errors are clustered by sector. The $p$-value corresponds to the two-sided test that the trend differs from zero. The final column reports $100\\times[\\exp(17\\hat{\\delta})-1]$, the implied change in the firm-size wage ratio between 2008 and 2025.",
  "  \\end{minipage}",
  "\\end{table}"
)

writeLines(trend_test_table, "Paper/sections/regression_firm_size_trend_test.tex")


#========================================================
# 6. Tabla de regresion tipo paper
#========================================================

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

model_list <- list(
  "(1)" = m_raw,
  "(2)" = m_fe,
  "(3)" = m_demog,
  "(4)" = m_full
)

size_terms <- paste0("tamano_empresa::", size_levels)
size_labels <- paste0("Firm size: ", size_levels)

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
  "  \\caption{Firm-size wage premium regressions: the role of formality}",
  "  \\label{tab:firm-size-wage-premium-regressions}",
  "  \\small",
  "  \\begin{tabular}{lcccc}",
  "    \\toprule",
  "    & \\multicolumn{4}{c}{Dependent variable: log real hourly labor income} \\\\",
  "    \\cmidrule(lr){2-5}",
  "    & (1) & (2) & (3) & (4) \\\\",
  "    \\midrule",
  table_rows,
  "    \\midrule",
  "    Gender, age, and education controls & No & No & Yes & Yes \\\\",
  "    Formality control & No & No & No & Yes \\\\",
  "    Sector-year fixed effects & No & Yes & Yes & Yes \\\\",
  paste0("    Observations & ", paste(format_obs(n_obs), collapse = " & "), " \\\\"),
  paste0("    $R^2$ & ", paste(sprintf('%.3f', r2_vals), collapse = " & "), " \\\\"),
  "    \\bottomrule",
  "  \\end{tabular}",
  "  \\vspace{0.3em}",
  "  \\begin{minipage}{0.95\\textwidth}",
  "  \\footnotesize",
  "  Notes: The omitted category is solo workers. All columns use the same estimation sample and GEIH expansion weights. Standard errors, clustered by sector, are reported in parentheses. Worker controls include a female-worker indicator, age, age squared, and education dummies. Column (4) adds labor formality. Significance levels: * $p<0.10$, ** $p<0.05$, *** $p<0.01$.",
  "  \\end{minipage}",
  "\\end{table}"
)

dir.create("Paper/sections", recursive = TRUE, showWarnings = FALSE)
writeLines(regression_table, "Paper/sections/regression_firm_size_table.tex")
