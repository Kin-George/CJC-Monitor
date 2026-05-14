library(haven)
library(dplyr)
library(fixest)
library(broom)
library(stringr)
library(ggplot2)
library(scales)

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
    label.size = 0.15,
    show.legend = FALSE
  ) +
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
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0.12, 0.18))
  ) +
  labs(
    title = "Firm size wage premium en Colombia",
    subtitle = "Premium salarial frente a trabajadores solos. Intervalos de confianza al 95%",
    x = "Tamaño de empresa",
    y = "Premium salarial frente a trabajadores solos (%)",
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

ggsave(
  filename = "Paper/figures/fig57.png",
  plot = g_premium_tamano,
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
      "TRUE" = "Significativo al 5%",
      "FALSE" = "No significativo"
    )
  ) +
  scale_y_continuous(
    labels = number_format(accuracy = 0.01),
    expand = expansion(mult = c(0.12, 0.18))
  ) +
  labs(
    title = "Firm-size coefficients in the full specification",
    subtitle = "Column (4): gender, education, formality, and sector-year fixed effects",
    x = "Tamaño de empresa",
    y = "Coeficiente estimado en log puntos",
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

ggsave(
  filename = "Paper/figures/fig58.png",
  plot = g_beta_tamano,
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
      "TRUE" = "Significativo al 5%",
      "FALSE" = "No significativo"
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
    title = "Firm size wage premium over time in Colombia",
    subtitle = "Year-specific premiums relative to solo workers. Full specification with 95% confidence intervals",
    x = "Año",
    y = "Premium salarial frente a trabajadores solos (%)",
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

ggsave(
  filename = "Paper/figures/fig59.png",
  plot = g_premium_tamano_anio,
  width = 12,
  height = 7,
  dpi = 300
)


#========================================================
# 5. Tabla de regresion tipo paper
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
