library(haven)
library(dplyr)
library(fixest)
library(broom)
library(stringr)
library(ggplot2)
library(scales)

# Moedelo enfoque personas
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

m1 <- feols(
  log_ingreso_hora_real ~ 
    i(tamano_empresa, ref = "2-3") +
    mujer +
    i(educacion, ref = "Básica secundaria") +
    formal |
    sector^anio,
  weights = ~ fex,
  cluster = ~ sector,
  data = geih %>% filter(!is.na(formal))
)

summary(m1)

#========================================================
# 1. Extraer coeficientes de tamaño de empresa
#========================================================

betas_tamano <- tidy(
  m1,
  conf.int = TRUE,
  conf.level = 0.95
) %>%
  filter(str_detect(term, "^tamano_empresa::")) %>%
  mutate(
    tamano_empresa = str_remove(term, "^tamano_empresa::"),
    
    # Transformación de log puntos a porcentaje
    premium = 100 * (exp(estimate) - 1),
    ci_low = 100 * (exp(conf.low) - 1),
    ci_high = 100 * (exp(conf.high) - 1),
    
    significativo = ci_low > 0 | ci_high < 0,
    
    tamano_empresa = factor(
      tamano_empresa,
      levels = c(
        "Solo",
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
# 3. Gráfico del premium salarial por tamaño de empresa
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
    subtitle = "Premium salarial frente a empresas de 2-3 trabajadores. Intervalos de confianza al 95%",
    x = "Tamaño de empresa",
    y = "Premium salarial estimado (%)",
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

g_premium_tamano



