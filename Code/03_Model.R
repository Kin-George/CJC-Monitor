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

library(haven)
library(dplyr)
library(fixest)
library(broom)
library(stringr)
library(ggplot2)
library(scales)

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
  "Datos/Processed/GEIH_base_modelo_personas_2008_2025.dta"
)

geih <- geih %>%
  mutate(
    anio = as.factor(anio),
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
    !is.na(educacion),
    !is.na(formal),
    !is.na(sector),
    !is.na(anio),
    !is.na(fex),
    fex > 0
  ) %>%
  mutate(
    anio_num = as.integer(as.character(anio)),
    year_trend = anio_num - min(anio_num, na.rm = TRUE)
  )

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
    i(educacion, ref = "Básica secundaria") |
    sector^anio,
  weights = ~ fex,
  cluster = ~ sector,
  data = geih_model
)

m_full <- feols(
  log_ingreso_hora_real ~
    i(tamano_empresa, ref = "Solo") +
    mujer +
    i(educacion, ref = "Básica secundaria") +
    formal |
    sector^anio,
  weights = ~ fex,
  cluster = ~ sector,
  data = geih_model
)

summary(m_full)

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
    subtitle = "Column (4): gender, education, formality, and sector-year fixed effects",
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
    subtitle = "Columna (4): g\u00e9nero, educaci\u00f3n, formalidad y efectos fijos sector-a\u00f1o",
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
    i(educacion, ref = "Básica secundaria") +
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
    i(educacion, ref = "Básica secundaria") +
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
    p_increase = case_when(
      estimate >= 0 ~ p.value / 2,
      TRUE ~ 1 - p.value / 2
    ),
    period_change_log_points = estimate * trend_horizon,
    period_change_percent = 100 * (exp(period_change_log_points) - 1),
    significant_increase = p_increase < 0.05,
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
    p_increase,
    period_change_log_points,
    period_change_percent,
    significant_increase
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
      color = significant_increase
    ),
    width = 0.15,
    linewidth = 0.9
  ) +
  geom_point(
    aes(color = significant_increase),
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
      "TRUE" = "Significant increase at 5%",
      "FALSE" = "Not significant"
    )
  ) +
  scale_y_continuous(
    labels = number_format(accuracy = 0.1),
    expand = expansion(mult = c(0.12, 0.18))
  ) +
  labs(
    title = "Test of whether the firm-size wage premium increased",
    subtitle = "Slope of the year trend by firm-size category. One-sided test: premium increasing over time",
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
      "TRUE" = "Aumento significativo al 5%",
      "FALSE" = "No significativo"
    )
  ) +
  labs(
    title = "Test de aumento del premium salarial por tama\u00f1o de empresa",
    subtitle = "Pendiente de la tendencia anual por tama\u00f1o de empresa. Test unilateral: premium creciente en el tiempo",
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
      sprintf("%.3f", trend_test$p_increase[i]),
      " & ",
      sprintf("%.1f\\%%", trend_test$period_change_percent[i]),
      " \\\\"
    )
  )
}

trend_test_table <- c(
  "\\begin{table}[htbp]",
  "  \\centering",
  "  \\caption{Testing whether the firm-size wage premium increased over time}",
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
  "  Notes: The annual trend is the coefficient on the interaction between firm-size category and a linear year trend, with solo workers as the omitted category. The specification controls for gender, education, formality, and sector-year fixed effects, and uses GEIH expansion weights. Standard errors are clustered by sector. The $p$-value corresponds to the one-sided test that the premium increased over time. The final column reports $100\\times[\\exp(17\\hat{\\delta})-1]$, the implied change in the firm-size wage ratio between 2008 and 2025.",
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

size_levels <- c("2-3", "4-5", "6-10", "11-19", "20-30", "31-50", "51-100", "101+")
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
  "    Gender and education controls & No & No & Yes & Yes \\\\",
  "    Formality control & No & No & No & Yes \\\\",
  "    Sector-year fixed effects & No & Yes & Yes & Yes \\\\",
  paste0("    Observations & ", paste(format_obs(n_obs), collapse = " & "), " \\\\"),
  paste0("    $R^2$ & ", paste(sprintf('%.3f', r2_vals), collapse = " & "), " \\\\"),
  "    \\bottomrule",
  "  \\end{tabular}",
  "  \\vspace{0.3em}",
  "  \\begin{minipage}{0.95\\textwidth}",
  "  \\footnotesize",
  "  Notes: The omitted category is solo workers. All columns use the same estimation sample and GEIH expansion weights. Standard errors, clustered by sector, are reported in parentheses. Gender and education controls include a female-worker indicator and education dummies. Column (4) adds labor formality. Significance levels: * $p<0.10$, ** $p<0.05$, *** $p<0.01$.",
  "  \\end{minipage}",
  "\\end{table}"
)

dir.create("Paper/sections", recursive = TRUE, showWarnings = FALSE)
writeLines(regression_table, "Paper/sections/regression_firm_size_table.tex")
