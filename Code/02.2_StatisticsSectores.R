options(scipen = 999)
setwd("~/Trabajo-Profesional/Javeriana")
setwd("C:/Users/wb640673/OneDrive - WBG/Externo/Javeriana-EAM")
# Analisis GEIH
library(dplyr)
library(stringr)
library(tibble)
library(scales)
library(ggplot2)
library(readr)
library(openxlsx)
library(forcats)
library(viridis)
library(grid)
library(haven)
library(openxlsx)
library(tidyr)
library(readxl)
library(ggrepel)

# Datos
consolidadosector <- read_excel("Datos/Processed/consolidadosector.xlsx")

#========================================================
# 1. Preparar base
#========================================================

cs <- consolidadosector %>%
  mutate(
    seccion = `Sección`,
    descripcion = `Descripción de sección`,
    trabajadores = as.numeric(`Trabajadores expandidos`),
    ingreso_mensual_prom = as.numeric(`Ingreso laboral mensual promedio`),
    ingreso_mensual_med = as.numeric(`Ingreso laboral mensual mediano`),
    ingreso_hora_prom = as.numeric(`Ingreso laboral por hora promedio`),
    participacion_trabajadores = trabajadores / sum(trabajadores, na.rm = TRUE),
    sector_label = paste0(seccion, " - ", str_wrap(descripcion, width = 35))
  )

#========================================================
# 2. Función para gráficos de barras mejorados
#========================================================

grafico_barras_mejorado <- function(data, variable, titulo, subtitulo = NULL,
                                    xlab = NULL, formato = c("numero", "porcentaje")) {
  
  formato <- match.arg(formato)
  
  df_plot <- data %>%
    mutate(
      valor = {{ variable }},
      rank_top = min_rank(desc(valor)),
      rank_bottom = min_rank(valor),
      grupo_color = case_when(
        rank_top <= 5 ~ "Top 5",
        rank_bottom <= 5 ~ "Bottom 5",
        TRUE ~ "Resto"
      )
    ) %>%
    arrange(valor) %>%
    mutate(
      sector_label = factor(sector_label, levels = sector_label)
    )
  
  if (formato == "porcentaje") {
    df_plot <- df_plot %>%
      mutate(etiqueta = percent(valor, accuracy = 0.1))
    
    escala_y <- scale_y_continuous(
      labels = label_percent(accuracy = 0.1),
      expand = expansion(mult = c(0, 0.18))
    )
  } else {
    df_plot <- df_plot %>%
      mutate(etiqueta = comma(valor, accuracy = 1))
    
    escala_y <- scale_y_continuous(
      labels = label_comma(),
      expand = expansion(mult = c(0, 0.18))
    )
  }
  
  ggplot(df_plot, aes(x = sector_label, y = valor, fill = grupo_color)) +
    geom_col(width = 0.72) +
    geom_text(
      aes(label = etiqueta),
      hjust = -0.08,
      size = 3.3
    ) +
    coord_flip() +
    escala_y +
    scale_fill_manual(
      values = c(
        "Top 5" = "darkgreen",
        "Bottom 5" = "darkred",
        "Resto" = "grey70"
      )
    ) +
    labs(
      title = titulo,
      subtitle = subtitulo,
      x = xlab,
      y = NULL,
      fill = NULL
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 11),
      axis.text.y = element_text(size = 9),
      axis.text.x = element_text(size = 10),
      legend.position = "none",
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank()
    )
}

g1_trabajadores <- grafico_barras_mejorado(
  data = cs,
  variable = trabajadores,
  titulo = "Número de trabajadores por sector",
  subtitulo = "Top 5 en verde, bottom 5 en rojo",
  xlab = "Sector",
  formato = "numero"
)

g1_trabajadores

g2_participacion <- grafico_barras_mejorado(
  data = cs,
  variable = participacion_trabajadores,
  titulo = "Participación de cada sector en el total de trabajadores",
  subtitulo = "Top 5 en verde, bottom 5 en rojo",
  xlab = "Sector",
  formato = "porcentaje"
)

g2_participacion

g3_ingreso_prom <- grafico_barras_mejorado(
  data = cs,
  variable = ingreso_mensual_prom,
  titulo = "Ingreso laboral mensual promedio por sector",
  subtitulo = "Top 5 en verde, bottom 5 en rojo",
  xlab = "Sector",
  formato = "numero"
)

g3_ingreso_prom

g4_ingreso_med <- grafico_barras_mejorado(
  data = cs,
  variable = ingreso_mensual_med,
  titulo = "Ingreso laboral mensual mediano por sector",
  subtitulo = "Top 5 en verde, bottom 5 en rojo",
  xlab = "Sector",
  formato = "numero"
)

g4_ingreso_med

g5_ingreso_hora <- grafico_barras_mejorado(
  data = cs,
  variable = ingreso_hora_prom,
  titulo = "Ingreso laboral por hora promedio por sector",
  subtitulo = "Top 5 en verde, bottom 5 en rojo",
  xlab = "Sector",
  formato = "numero"
)

g5_ingreso_hora

cs_explore <- consolidadosector %>%
  mutate(
    seccion = `Sección`,
    descripcion = `Descripción de sección`,
    sector_label = paste0(seccion, " - ", str_wrap(descripcion, width = 35)),
    
    trabajadores = as.numeric(`Trabajadores expandidos`),
    ingreso_mensual_prom = as.numeric(`Ingreso laboral mensual promedio`),
    ingreso_mensual_med = as.numeric(`Ingreso laboral mensual mediano`),
    ingreso_hora_prom = as.numeric(`Ingreso laboral por hora promedio`),
    ingreso_hora_med = as.numeric(`Ingreso laboral por hora mediano`),
    total_ingreso_anual = as.numeric(`Total ingreso laboral anual expandido`),
    
    part_trabajadores = trabajadores / sum(trabajadores, na.rm = TRUE),
    part_ingreso = total_ingreso_anual / sum(total_ingreso_anual, na.rm = TRUE),
    
    brecha_prom_med_mensual = ingreso_mensual_prom - ingreso_mensual_med,
    brecha_prom_med_pct = brecha_prom_med_mensual / ingreso_mensual_med,
    
    ratio_ingreso_trabajadores = part_ingreso / part_trabajadores
  )

g_participaciones <- ggplot(
  cs_explore,
  aes(x = part_trabajadores, y = part_ingreso)
) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_point(aes(size = trabajadores), alpha = 0.7) +
  geom_text_repel(aes(label = seccion), size = 4, max.overlaps = 30) +
  scale_x_continuous(labels = percent) +
  scale_y_continuous(labels = percent) +
  scale_size_continuous(labels = comma) +
  labs(
    title = "Participación en trabajadores vs participación en ingreso laboral total",
    subtitle = "Sectores sobre la diagonal concentran relativamente más ingreso que empleo",
    x = "Participación en trabajadores",
    y = "Participación en ingreso laboral total",
    size = "Trabajadores"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

g_participaciones

g_brecha <- ggplot(
  cs_explore %>% arrange(desc(brecha_prom_med_pct)),
  aes(x = reorder(sector_label, brecha_prom_med_pct), y = brecha_prom_med_pct)
) +
  geom_col(fill = "darkgreen") +
  geom_text(
    aes(label = percent(brecha_prom_med_pct, accuracy = 0.1)),
    hjust = -0.08,
    size = 3.3
  ) +
  coord_flip() +
  scale_y_continuous(labels = percent, expand = expansion(mult = c(0, 0.18))) +
  labs(
    title = "Brecha entre ingreso mensual promedio y mediano",
    subtitle = "Mayor brecha sugiere más asimetría salarial dentro del sector",
    x = "Sector",
    y = "Brecha porcentual"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank()
  )

g_brecha

g_ratio <- ggplot(
  cs_explore %>% arrange(desc(ratio_ingreso_trabajadores)),
  aes(x = reorder(sector_label, ratio_ingreso_trabajadores), y = ratio_ingreso_trabajadores)
) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_col(aes(fill = ratio_ingreso_trabajadores >= 1)) +
  geom_text(
    aes(label = round(ratio_ingreso_trabajadores, 2)),
    hjust = -0.08,
    size = 3.3
  ) +
  coord_flip() +
  scale_fill_manual(
    values = c("TRUE" = "darkgreen", "FALSE" = "darkred"),
    labels = c("FALSE" = "Menor a 1", "TRUE" = "Mayor a 1")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(
    title = "Ingreso relativo por sector",
    subtitle = "Ratio = participación en ingreso / participación en trabajadores",
    x = "Sector",
    y = "Ratio relativo",
    fill = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    panel.grid.major.y = element_blank()
  )

g_ratio

### Analisis desagregado por tamaño de empresa
geih_2025 <- read_excel("Datos/Processed/INGLABO_secciones_tamano_empresa_2025.xlsx")

#========================================================
# 1. Función segura para promedio ponderado
#========================================================

wmean_safe <- function(x, w) {
  ok <- !is.na(x) & !is.na(w) & w > 0
  if (sum(ok) == 0) return(NA_real_)
  weighted.mean(x[ok], w[ok])
}

#========================================================
# 2. Preparar base limpia
#========================================================

df_emp <- geih_2025 %>%
  mutate(
    seccion = `Sección`,
    descripcion = `Descripción de sección`,
    sector_label = paste0(seccion, " - ", str_wrap(descripcion, width = 35)),
    
    tamano_cod = as.numeric(`Código tamaño empresa`),
    tamano_empresa = `Tamaño empresa P3069`,
    
    trabajadores = as.numeric(`Trabajadores expandidos`),
    peso_trab = trabajadores,
    
    ingreso_mensual_prom = as.numeric(`IngresoPromMensualPromedio`),
    ingreso_mensual_med = as.numeric(`IngresoMedMensual`),
    ingreso_hora_prom = as.numeric(`Ingreso laboral por hora promedio`),
    ingreso_hora_med = as.numeric(`Ingreso laboral por hora mediano`),
    total_ingreso_anual = as.numeric(`Total ingreso laboral anual expandido`),
    n_obs = as.numeric(`Observaciones no expandidas`)
  ) %>%
  filter(
    !is.na(tamano_cod),
    !is.na(peso_trab),
    peso_trab > 0,
    tamano_cod >= 1,
    tamano_cod <= 10
  ) %>%
  mutate(
    tamano_empresa = factor(
      tamano_empresa,
      levels = c(
        "Trabaja solo",
        "2 a 3 personas",
        "4 a 5 personas",
        "6 a 10 personas",
        "11 a 19 personas",
        "20 a 30 personas",
        "31 a 50 personas",
        "51 a 100 personas",
        "101 a 200 personas",
        "201 o más personas"
      )
    )
  )

trab_por_tamano <- df_emp %>%
  group_by(tamano_cod, tamano_empresa) %>%
  summarise(
    ingreso_hora_prom = wmean_safe(ingreso_hora_prom, peso_trab),
    ingreso_hora_med = wmean_safe(ingreso_hora_med, peso_trab),
    ingreso_mensual_prom = wmean_safe(ingreso_mensual_prom, peso_trab),
    ingreso_mensual_med = wmean_safe(ingreso_mensual_med, peso_trab),
    trabajadores = sum(peso_trab, na.rm = TRUE),
    total_ingreso_anual = sum(total_ingreso_anual, na.rm = TRUE),
    observaciones = sum(n_obs, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    participacion = trabajadores / sum(trabajadores, na.rm = TRUE)
  )

g_part_tamano <- ggplot(
  trab_por_tamano,
  aes(x = tamano_empresa, y = participacion)
) +
  geom_col(fill = "darkgreen", width = 0.72) +
  geom_text(
    aes(label = percent(participacion, accuracy = 0.1)),
    vjust = -0.25,
    size = 3.5
  ) +
  scale_y_continuous(
    labels = percent,
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(
    title = "Participación del empleo por tamaño de empresa",
    subtitle = "Distribución del total de trabajadores expandidos",
    x = "Tamaño de empresa",
    y = "Participación"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank()
  )

g_part_tamano

g_ingreso_hora_tamano <- ggplot(
  trab_por_tamano,
  aes(x = tamano_empresa, y = ingreso_hora_prom, group = 1)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 3, color = "darkgreen") +
  geom_text(
    aes(label = comma(ingreso_hora_prom, accuracy = 1)),
    vjust = -0.7,
    size = 3.4
  ) +
  scale_y_continuous(
    labels = comma,
    expand = expansion(mult = c(0.05, 0.15))
  ) +
  labs(
    title = "Ingreso laboral por hora promedio según tamaño de empresa",
    subtitle = "Promedio ponderado por trabajadores expandidos",
    x = "Tamaño de empresa",
    y = "Ingreso laboral por hora promedio"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank()
  )

g_ingreso_hora_tamano

df_part_sector <- df_emp %>%
  group_by(seccion, descripcion, sector_label) %>%
  mutate(
    trabajadores_sector = sum(trabajadores, na.rm = TRUE),
    part_dentro_sector = trabajadores / trabajadores_sector
  ) %>%
  ungroup()

g_heat_part <- ggplot(
  df_part_sector,
  aes(x = tamano_empresa, y = sector_label, fill = part_dentro_sector)
) +
  geom_tile(color = "white") +
  geom_text(
    aes(label = percent(part_dentro_sector, accuracy = 0.1)),
    size = 2.7
  ) +
  scale_fill_gradient(
    low = "grey95",
    high = "darkgreen",
    labels = percent
  ) +
  labs(
    title = "Composición del empleo por tamaño de empresa dentro de cada sector",
    subtitle = "Cada fila suma 100%",
    x = "Tamaño de empresa",
    y = "Sector",
    fill = "Participación"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )

g_heat_part

g_facet_sector <- ggplot(
  df_emp,
  aes(x = tamano_cod, y = ingreso_hora_prom)
) +
  geom_line(group = 1, linewidth = 0.7) +
  geom_point(aes(size = trabajadores), color = "darkgreen", alpha = 0.7) +
  facet_wrap(~ sector_label, scales = "free_y") +
  scale_x_continuous(
    breaks = trab_por_tamano$tamano_cod,
    labels = trab_por_tamano$tamano_empresa
  ) +
  scale_y_continuous(labels = comma) +
  scale_size_continuous(labels = comma) +
  labs(
    title = "Ingreso laboral por hora y tamaño de empresa, por sector",
    x = "Tamaño de empresa",
    y = "Ingreso laboral por hora promedio",
    size = "Trabajadores"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
    strip.text = element_text(face = "bold", size = 7),
    legend.position = "bottom"
  )

g_facet_sector

g_heat_ingreso <- ggplot(
  df_emp,
  aes(x = tamano_empresa, y = sector_label, fill = ingreso_hora_prom)
) +
  geom_tile(color = "black") +
  geom_text(
    aes(label = comma(ingreso_hora_prom, accuracy = 1)),
    size = 2.6,
    color = "white"
  ) +
  scale_fill_gradient(
    low = "darkred",
    high = "darkgreen",
    labels = comma
  ) +
  labs(
    title = "Ingreso laboral por hora promedio por sector y tamaño de empresa",
    subtitle = "Valores promedio ponderados por trabajadores expandidos",
    x = "Tamaño de empresa",
    y = "Sector",
    fill = "Ingreso hora"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )

g_heat_ingreso

##### Dentro de cada sector, ¿el ingreso laboral por hora aumenta cuando aumenta el tamaño de la empresa?

#========================================================
# 1. Funciones auxiliares
#========================================================

wmean_safe <- function(x, w) {
  ok <- !is.na(x) & !is.na(w) & w > 0
  if (sum(ok) == 0) return(NA_real_)
  weighted.mean(x[ok], w[ok])
}

wcor_safe <- function(x, y, w) {
  ok <- !is.na(x) & !is.na(y) & !is.na(w) & w > 0
  x <- x[ok]
  y <- y[ok]
  w <- w[ok]
  
  if (length(x) < 3) return(NA_real_)
  if (length(unique(x)) < 3) return(NA_real_)
  if (length(unique(y)) < 3) return(NA_real_)
  
  mx <- weighted.mean(x, w)
  my <- weighted.mean(y, w)
  
  cov_xy <- sum(w * (x - mx) * (y - my)) / sum(w)
  var_x  <- sum(w * (x - mx)^2) / sum(w)
  var_y  <- sum(w * (y - my)^2) / sum(w)
  
  if (var_x == 0 | var_y == 0) return(NA_real_)
  
  cov_xy / sqrt(var_x * var_y)
}

df_emp <- geih_2025 %>%
  mutate(
    seccion = `Sección`,
    descripcion = `Descripción de sección`,
    sector_label = paste0(seccion, " - ", str_wrap(descripcion, width = 35)),
    
    tamano_cod = as.numeric(`Código tamaño empresa`),
    tamano_empresa = `Tamaño empresa P3069`,
    
    trabajadores = as.numeric(`Trabajadores expandidos`),
    peso_trab = trabajadores,
    
    ingreso_hora_prom = as.numeric(`Ingreso laboral por hora promedio`),
    ingreso_hora_med = as.numeric(`Ingreso laboral por hora mediano`),
    ingreso_mensual_prom = as.numeric(`IngresoPromMensualPromedio`),
    ingreso_mensual_med = as.numeric(`IngresoMedMensual`)
  ) %>%
  filter(
    !is.na(tamano_cod),
    !is.na(ingreso_hora_prom),
    !is.na(peso_trab),
    peso_trab > 0,
    tamano_cod >= 1,
    tamano_cod <= 10
  ) %>%
  mutate(
    tamano_empresa = factor(
      tamano_empresa,
      levels = c(
        "Trabaja solo",
        "2 a 3 personas",
        "4 a 5 personas",
        "6 a 10 personas",
        "11 a 19 personas",
        "20 a 30 personas",
        "31 a 50 personas",
        "51 a 100 personas",
        "101 a 200 personas",
        "201 o más personas"
      )
    )
  )

indicadores_escala <- df_emp %>%
  group_by(seccion, descripcion, sector_label) %>%
  group_modify(~ {
    
    d <- .x %>%
      filter(
        !is.na(tamano_cod),
        !is.na(ingreso_hora_prom),
        !is.na(peso_trab),
        peso_trab > 0
      )
    
    n_cat <- n_distinct(d$tamano_cod)
    
    corr_spearman <- if (n_cat >= 3) {
      cor(d$tamano_cod, d$ingreso_hora_prom, method = "spearman", use = "complete.obs")
    } else {
      NA_real_
    }
    
    corr_pearson_pond <- wcor_safe(
      x = d$tamano_cod,
      y = d$ingreso_hora_prom,
      w = d$peso_trab
    )
    
    pendiente_pond <- if (n_cat >= 3) {
      coef(lm(ingreso_hora_prom ~ tamano_cod, data = d, weights = peso_trab))[2]
    } else {
      NA_real_
    }
    
    ingreso_pequenas <- d %>%
      filter(tamano_cod %in% 1:4) %>%
      summarise(valor = wmean_safe(ingreso_hora_prom, peso_trab)) %>%
      pull(valor)
    
    ingreso_grandes <- d %>%
      filter(tamano_cod %in% 9:10) %>%
      summarise(valor = wmean_safe(ingreso_hora_prom, peso_trab)) %>%
      pull(valor)
    
    prima_grandes_pequenas <- ingreso_grandes / ingreso_pequenas - 1
    
    tibble(
      n_categorias = n_cat,
      trabajadores_total = sum(d$peso_trab, na.rm = TRUE),
      corr_spearman = corr_spearman,
      corr_pearson_ponderada = corr_pearson_pond,
      pendiente_ponderada = pendiente_pond,
      ingreso_hora_pequenas = ingreso_pequenas,
      ingreso_hora_grandes = ingreso_grandes,
      prima_grandes_pequenas = prima_grandes_pequenas
    )
    
  }) %>%
  ungroup() %>%
  mutate(
    clasificacion = case_when(
      is.na(corr_spearman) ~ "Información insuficiente",
      corr_spearman >= 0.50 & pendiente_ponderada > 0 ~ "Fuerte positiva",
      corr_spearman >= 0.25 & pendiente_ponderada > 0 ~ "Positiva moderada",
      corr_spearman <= -0.25 & pendiente_ponderada < 0 ~ "Negativa",
      TRUE ~ "Débil o mixta"
    )
  ) %>%
  arrange(desc(corr_spearman))

indicadores_escala

indicadores_escala %>%
  select(
    seccion,
    descripcion,
    n_categorias,
    trabajadores_total,
    corr_spearman,
    corr_pearson_ponderada,
    pendiente_ponderada,
    prima_grandes_pequenas,
    clasificacion
  ) %>%
  arrange(desc(corr_spearman))

corr_plot <- indicadores_escala %>%
  filter(!is.na(corr_spearman)) %>%
  mutate(
    grupo_color = case_when(
      corr_spearman >= 0.25 ~ "Relación positiva",
      corr_spearman <= -0.25 ~ "Relación negativa",
      TRUE ~ "Relación débil"
    ),
    sector_label = factor(sector_label, levels = sector_label[order(corr_spearman)])
  )

g_corr_sector <- ggplot(
  corr_plot,
  aes(x = sector_label, y = corr_spearman, fill = grupo_color)
) +
  geom_col(width = 0.72) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_text(
    aes(label = round(corr_spearman, 2)),
    hjust = ifelse(corr_plot$corr_spearman >= 0, -0.15, 1.15),
    size = 3.4
  ) +
  coord_flip() +
  scale_y_continuous(
    limits = c(-1, 1),
    breaks = seq(-1, 1, 0.25)
  ) +
  scale_fill_manual(
    values = c(
      "Relación positiva" = "darkgreen",
      "Relación negativa" = "darkred",
      "Relación débil" = "grey70"
    )
  ) +
  labs(
    title = "Correlación entre tamaño de empresa e ingreso laboral por hora",
    subtitle = "Correlación Spearman por sector",
    x = "Sector",
    y = "Correlación Spearman",
    fill = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    panel.grid.major.y = element_blank()
  )

g_corr_sector

pendiente_plot <- indicadores_escala %>%
  filter(!is.na(pendiente_ponderada)) %>%
  mutate(
    grupo_color = case_when(
      pendiente_ponderada > 0 ~ "Pendiente positiva",
      pendiente_ponderada < 0 ~ "Pendiente negativa",
      TRUE ~ "Sin cambio"
    ),
    sector_label = factor(sector_label, levels = sector_label[order(pendiente_ponderada)])
  )

g_pendiente_sector <- ggplot(
  pendiente_plot,
  aes(x = sector_label, y = pendiente_ponderada, fill = grupo_color)
) +
  geom_col(width = 0.72) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_text(
    aes(label = comma(pendiente_ponderada, accuracy = 1)),
    hjust = ifelse(pendiente_plot$pendiente_ponderada >= 0, -0.1, 1.1),
    size = 3.2
  ) +
  coord_flip() +
  scale_y_continuous(
    labels = comma,
    expand = expansion(mult = c(0.15, 0.20))
  ) +
  scale_fill_manual(
    values = c(
      "Pendiente positiva" = "darkgreen",
      "Pendiente negativa" = "darkred",
      "Sin cambio" = "grey70"
    )
  ) +
  labs(
    title = "Pendiente entre tamaño de empresa e ingreso laboral por hora",
    subtitle = "Estimación ponderada por trabajadores expandidos",
    x = "Sector",
    y = "Cambio promedio en ingreso por hora por categoría de tamaño",
    fill = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    panel.grid.major.y = element_blank()
  )

g_pendiente_sector

df_lineas_escala <- df_emp %>%
  left_join(
    indicadores_escala %>%
      select(
        seccion,
        corr_spearman,
        pendiente_ponderada,
        clasificacion
      ),
    by = "seccion"
  ) %>%
  mutate(
    facet_label = paste0(
      seccion, " - ", str_wrap(descripcion, width = 28),
      "\nρ = ", round(corr_spearman, 2),
      " | β = ", comma(pendiente_ponderada, accuracy = 1)
    )
  )

labels_tamano <- df_emp %>%
  distinct(tamano_cod, tamano_empresa) %>%
  arrange(tamano_cod)

g_lineas_escala <- ggplot(
  df_lineas_escala,
  aes(x = tamano_cod, y = ingreso_hora_prom)
) +
  geom_line(group = 1, color = "darkgreen", linewidth = 0.8) +
  geom_point(aes(size = peso_trab), color = "darkgreen", alpha = 0.75) +
  facet_wrap(~ facet_label, scales = "free_y") +
  scale_x_continuous(
    breaks = labels_tamano$tamano_cod,
    labels = labels_tamano$tamano_empresa
  ) +
  scale_y_continuous(labels = comma) +
  scale_size_continuous(labels = comma) +
  labs(
    title = "Ingreso laboral por hora y tamaño de empresa, por sector",
    subtitle = "ρ = correlación Spearman; β = pendiente ponderada por trabajadores",
    x = "Tamaño de empresa",
    y = "Ingreso laboral por hora promedio",
    size = "Trabajadores"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
    strip.text = element_text(face = "bold", size = 7),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

g_lineas_escala

#========================================================
# 1. Crear índice base Trabaja solo = 100 dentro de cada sector
#========================================================

df_indice <- df_emp %>%
  group_by(seccion, descripcion, sector_label) %>%
  mutate(
    ingreso_base = ingreso_hora_prom[tamano_cod == 1][1],
    indice_ingreso_hora = (ingreso_hora_prom / ingreso_base) * 100
  ) %>%
  ungroup() %>%
  filter(!is.na(indice_ingreso_hora))

#========================================================
# 2. Gráfico de líneas indexadas
#========================================================

g_indice_tamano_sector <- ggplot(
  df_indice,
  aes(x = tamano_cod, y = indice_ingreso_hora)
) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "grey40") +
  geom_line(group = 1, color = "darkgreen", linewidth = 0.8) +
  geom_point(aes(size = peso_trab), color = "darkgreen", alpha = 0.75) +
  facet_wrap(~ sector_label, scales = "free_y") +
  scale_x_continuous(
    breaks = sort(unique(df_indice$tamano_cod)),
    labels = levels(df_indice$tamano_empresa)
  ) +
  scale_y_continuous(labels = comma) +
  scale_size_continuous(labels = comma) +
  labs(
    title = "Ingreso laboral por hora relativo al trabajo individual",
    subtitle = "Base: ingreso por hora de quienes trabajan solos = 100 dentro de cada sector",
    x = "Tamaño de empresa",
    y = "Índice de ingreso por hora",
    size = "Trabajadores"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
    strip.text = element_text(face = "bold", size = 7),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

g_indice_tamano_sector

# Escogemos
# g2 - Participacion de cada sector en el total de trabajadores
# g5 - Ingreso laboral por hora promedio por sector
# g_part_tamaño
# g_heat_part
# g_corr_sector
# Anexo: g_lineas_escala

#### Encuesta anual de servicios
EAS_2024 <- read_dta("Datos/Raw/EAS/EAS_2024.dta")
# Calculo del valor agregado (miles de pesos)
resumen_eas <- EAS_2024 %>%
  summarise(
    Encuesta = "EAS",
    Valor_Agregado_miles_pesos = sum(VALAGRE, na.rm = TRUE),
    Total_ocupados = sum(pottot, na.rm = TRUE)
  ) %>%
  mutate(
    Valor_Agregado_miles_millones = Valor_Agregado_miles_pesos / 1e6,
    Valor_Agregado_por_ocupado_miles_pesos = Valor_Agregado_miles_pesos / Total_ocupados,
    Valor_Agregado_por_ocupado_millones_pesos = Valor_Agregado_por_ocupado_miles_pesos / 1e3
  )

resumen_eas_largo <- resumen_eas %>%
  select(
    Valor_Agregado_miles_millones,
    Total_ocupados,
    Valor_Agregado_por_ocupado_millones_pesos
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Indicador",
    values_to = "Valor"
  )

# EAM
eam_2024 <- read_dta("Datos/Raw/EAM/EAM_2024.dta")
# Calculo del valor agregado (miles de pesos)
resumen_eam <- eam_2024 %>%
  summarise(
    Encuesta = "EAM",
    Valor_Agregado_miles_pesos = sum(VALAGRI, na.rm = TRUE),
    Total_ocupados = sum(PERTOTAL, na.rm = TRUE)
  ) %>%
  mutate(
    Valor_Agregado_miles_millones = Valor_Agregado_miles_pesos / 1e6,
    Valor_Agregado_por_ocupado_miles_pesos = Valor_Agregado_miles_pesos / Total_ocupados,
    Valor_Agregado_por_ocupado_millones_pesos = Valor_Agregado_por_ocupado_miles_pesos / 1e3
  )

resumen_eam_largo <- resumen_eam %>%
  select(
    Valor_Agregado_miles_millones,
    Total_ocupados,
    Valor_Agregado_por_ocupado_millones_pesos
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Indicador",
    values_to = "Valor"
  )
# EAC
EAC_2024 <- read_dta("Datos/Raw/EAC/EAC_2024.dta")
# Calculo del valor agregado (miles de pesos)
resumen_eac <- EAC_2024 %>%
  summarise(
    Encuesta = "EAC",
    Valor_Agregado_miles_pesos = sum(AGREGA, na.rm = TRUE),
    Total_ocupados = sum(TOTPERSO, na.rm = TRUE)
  ) %>%
  mutate(
    Valor_Agregado_miles_millones = Valor_Agregado_miles_pesos / 1e6,
    Valor_Agregado_por_ocupado_miles_pesos = Valor_Agregado_miles_pesos / Total_ocupados,
    Valor_Agregado_por_ocupado_millones_pesos = Valor_Agregado_por_ocupado_miles_pesos / 1e3
  )

resumen_eac_largo <- resumen_eac %>%
  select(
    Valor_Agregado_miles_millones,
    Total_ocupados,
    Valor_Agregado_por_ocupado_millones_pesos
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Indicador",
    values_to = "Valor"
  )

comparativo_encuestas <- bind_rows(
  resumen_eam,
  resumen_eas,
  resumen_eac
) %>%
  select(
    Encuesta,
    Valor_Agregado_miles_pesos,
    Total_ocupados,
    Valor_Agregado_miles_millones,
    Valor_Agregado_por_ocupado_miles_pesos,
    Valor_Agregado_por_ocupado_millones_pesos
  )

#========================================================
# 5. Pasar a formato largo para graficar
#========================================================

comparativo_largo <- comparativo_encuestas %>%
  select(
    Encuesta,
    Valor_Agregado_miles_millones,
    Total_ocupados,
    Valor_Agregado_por_ocupado_millones_pesos
  ) %>%
  pivot_longer(
    cols = -Encuesta,
    names_to = "Indicador",
    values_to = "Valor"
  ) %>%
  mutate(
    Indicador = case_when(
      Indicador == "Valor_Agregado_miles_millones" ~ 
        "Valor agregado\n(miles de millones de pesos)",
      Indicador == "Total_ocupados" ~ 
        "Personal ocupado\n(personas)",
      Indicador == "Valor_Agregado_por_ocupado_millones_pesos" ~ 
        "Valor agregado por ocupado\n(millones de pesos)",
      TRUE ~ Indicador
    ),
    Indicador = factor(
      Indicador,
      levels = c(
        "Valor agregado\n(miles de millones de pesos)",
        "Personal ocupado\n(personas)",
        "Valor agregado por ocupado\n(millones de pesos)"
      )
    ),
    etiqueta = case_when(
      str_detect(Indicador, "Personal ocupado") ~ comma(Valor, accuracy = 1),
      str_detect(Indicador, "Valor agregado por ocupado") ~ comma(Valor, accuracy = 0.1),
      TRUE ~ comma(Valor, accuracy = 1)
    )
  )

g_comparativo_encuestas <- ggplot(
  comparativo_largo,
  aes(
    x = reorder(Encuesta, Valor),
    y = Valor,
    fill = Encuesta
  )
) +
  geom_col(width = 0.65) +
  geom_text(
    aes(label = etiqueta),
    vjust = -0.25,
    size = 3.8
  ) +
  facet_wrap(
    ~ Indicador,
    scales = "free_y",
    nrow = 1
  ) +
  scale_y_continuous(
    labels = comma,
    expand = expansion(mult = c(0, 0.18))
  ) +
  labs(
    title = "Valor agregado, personal ocupado y valor agregado por ocupado",
    subtitle = "Comparativo entre EAM, EAS y EAC, 2024",
    x = NULL,
    y = NULL,
    fill = "Encuesta"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 11),
    strip.text = element_text(face = "bold", size = 10),
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

g_comparativo_encuestas
