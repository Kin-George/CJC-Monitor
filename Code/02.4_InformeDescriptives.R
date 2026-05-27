setwd("~/Trabajo-Profesional/Javeriana")
#========================================================
# Evolución del ingreso laboral por hora real
# GEIH 2008-2025
#========================================================

library(haven)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyr)
library(scales)
library(grid)

options(scipen = 999)

#--------------------------------------------------------
# 1. Cargar base individual limpia
#--------------------------------------------------------

geih <- read_dta(
  "Datos/Processed/Paper-GEIH_base_modelo_personas_2008_2025.dta"
) %>%
  mutate(
    anio = as.integer(anio),
    fex = as.numeric(fex),
    ingreso_hora_real = as.numeric(ingreso_hora_real)
  )

#--------------------------------------------------------
# 2. Función de media ponderada
#--------------------------------------------------------

weighted_mean <- function(x, w) {
  ok <- !is.na(x) & !is.na(w) & w > 0
  
  if (!any(ok)) {
    return(NA_real_)
  }
  
  weighted.mean(x[ok], w[ok])
}

#--------------------------------------------------------
# 3. Base válida para descriptivas
#--------------------------------------------------------

geih_desc <- geih %>%
  filter(
    !is.na(ingreso_hora_real),
    ingreso_hora_real > 0,
    !is.na(fex),
    fex > 0,
    !is.na(anio)
  )

#--------------------------------------------------------
# 4. Serie anual del ingreso laboral por hora real
#--------------------------------------------------------

serie_ingreso_real <- geih_desc %>%
  filter(
    !is.na(anio),
    !is.na(ingreso_hora_real),
    ingreso_hora_real > 0,
    !is.na(fex),
    fex > 0
  ) %>%
  group_by(anio) %>%
  summarise(
    observaciones = n(),
    trabajadores_expandidos = sum(fex, na.rm = TRUE),
    ingreso_hora_real_promedio = weighted.mean(
      ingreso_hora_real,
      fex,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  arrange(anio)

serie_ingreso_real

serie_plot <- serie_ingreso_real %>%
  arrange(anio)

# Valores inicial y final
ingreso_2008 <- serie_plot %>%
  filter(anio == 2008) %>%
  pull(ingreso_hora_real_promedio)

ingreso_2025 <- serie_plot %>%
  filter(anio == 2025) %>%
  pull(ingreso_hora_real_promedio)

variacion_2008_2025 <- 100 * (ingreso_2025 / ingreso_2008 - 1)

label_variacion_total <- paste0(
  "Cambio 2008–2025: ",
  ifelse(variacion_2008_2025 >= 0, "+", ""),
  round(variacion_2008_2025, 1),
  "%"
)

# Posición de la etiqueta de variación
max_y <- max(serie_plot$ingreso_hora_real_promedio, na.rm = TRUE)
min_y <- min(serie_plot$ingreso_hora_real_promedio, na.rm = TRUE)
rango_y <- max_y - min_y

y_variacion <- max_y + 0.12 * rango_y

#--------------------------------------------------------
# 6. Gráfico de línea limpio y pedagógico
#--------------------------------------------------------

g_ingreso_linea_simple <- ggplot(
  serie_plot,
  aes(
    x = anio,
    y = ingreso_hora_real_promedio
  )
) +
  geom_line(
    color = "darkblue",
    linewidth = 1.35
  ) +
  geom_point(
    shape = 21,
    fill = "darkblue",
    color = "white",
    stroke = 0.8,
    size = 3.8
  ) +
  geom_label(
    aes(
      label = comma(ingreso_hora_real_promedio, accuracy = 1)
    ),
    fill = "darkblue",
    color = "white",
    fontface = "bold",
    size = 3.2,
    label.size = 0.15,
    label.padding = unit(0.14, "lines"),
    vjust = -0.85,
    show.legend = FALSE
  ) +
  geom_segment(
    aes(
      x = 2008,
      xend = 2025,
      y = y_variacion,
      yend = y_variacion
    ),
    inherit.aes = FALSE,
    color = "gray35",
    linewidth = 0.8,
    arrow = arrow(
      length = unit(0.18, "cm"),
      type = "closed"
    )
  ) +
  geom_label(
    aes(
      x = 2016.5,
      y = y_variacion,
      label = label_variacion_total
    ),
    inherit.aes = FALSE,
    fill = "white",
    color = "black",
    fontface = "bold",
    size = 4,
    label.size = 0.15,
    label.padding = unit(0.18, "lines")
  ) +
  scale_x_continuous(
    breaks = sort(unique(serie_plot$anio))
  ) +
  scale_y_continuous(
    labels = comma,
    expand = expansion(mult = c(0.06, 0.22))
  ) +
  labs(
    title = "Evolución del ingreso laboral por hora promedio, 2008–2025",
    subtitle = "Pesos constantes de 2025",
    x = "",
    y = "Ingreso laboral por hora promedio"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 90, hjust = 1),
    axis.text.y = element_text(size = 10),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

g_ingreso_linea_simple

# Sexo
#--------------------------------------------------------
# 1. Serie por sexo
#--------------------------------------------------------

serie_genero <- geih_desc %>%
  group_by(anio, sexo) %>%
  summarise(
    observaciones = n(),
    trabajadores_expandidos = sum(fex, na.rm = TRUE),
    ingreso_hora_real_promedio = weighted_mean(
      ingreso_hora_real,
      fex
    ),
    .groups = "drop"
  ) %>%
  mutate(
    sexo = factor(sexo, levels = c("Hombre", "Mujer"))
  ) %>%
  arrange(sexo, anio)

#--------------------------------------------------------
# 2. Etiquetas solo para 2008 y 2025
#--------------------------------------------------------

labels_genero <- serie_genero %>%
  filter(anio %in% c(2008, 2025)) %>%
  mutate(
    vjust_label = if_else(sexo == "Hombre", -0.85, 1.75)
  )

#--------------------------------------------------------
# 3. Gráfico
#--------------------------------------------------------

g_ingreso_genero_niveles <- ggplot(
  serie_genero,
  aes(
    x = anio,
    y = ingreso_hora_real_promedio,
    color = sexo,
    group = sexo
  )
) +
  geom_line(
    linewidth = 1.35,
    alpha = 0.95
  ) +
  geom_point(
    size = 3.5,
    alpha = 0.95
  ) +
  geom_label(
    data = labels_genero,
    aes(
      label = comma(ingreso_hora_real_promedio, accuracy = 1),
      fill = sexo,
      vjust = vjust_label
    ),
    color = "white",
    fontface = "bold",
    size = 3.4,
    label.size = 0.15,
    label.padding = unit(0.15, "lines"),
    show.legend = FALSE
  ) +
  scale_color_manual(
    values = c(
      "Hombre" = "darkred",
      "Mujer" = "darkblue"
    ),
    name = ""
  ) +
  scale_fill_manual(
    values = c(
      "Hombre" = "darkred",
      "Mujer" = "darkblue"
    )
  ) +
  scale_x_continuous(
    breaks = sort(unique(serie_genero$anio))
  ) +
  scale_y_continuous(
    labels = comma,
    expand = expansion(mult = c(0.08, 0.16))
  ) +
  labs(
    title = "Evolución del ingreso laboral por hora promedio por sexo, 2008–2025",
    subtitle = "Pesos constantes de 2025",
    x = "",
    y = "Ingreso laboral por hora promedio"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 90, hjust = 1),
    axis.text.y = element_text(size = 10),
    legend.position = "bottom",
    legend.text = element_text(face = "bold", size = 11),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

g_ingreso_genero_niveles


# Brecha
brecha_genero <- serie_genero %>%
  select(anio, sexo, ingreso_hora_real_promedio) %>%
  pivot_wider(
    names_from = sexo,
    values_from = ingreso_hora_real_promedio
  ) %>%
  mutate(
    brecha_pct = 100 * (Hombre - Mujer) / Hombre
  ) %>%
  arrange(anio)

labels_brecha <- brecha_genero %>%
  filter(anio %in% c(2008, 2019, 2025))

g_brecha_genero <- ggplot(
  brecha_genero,
  aes(
    x = anio,
    y = brecha_pct
  )
) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "gray50",
    linewidth = 0.7
  ) +
  geom_line(
    color = "darkblue",
    linewidth = 1.35
  ) +
  geom_point(
    color = "darkblue",
    size = 3.5
  ) +
  geom_label(
    data = labels_brecha,
    aes(
      label = paste0(round(brecha_pct, 1), "%")
    ),
    fill = "darkblue",
    color = "white",
    fontface = "bold",
    size = 3.4,
    label.size = 0.15,
    label.padding = unit(0.15, "lines"),
    vjust = -0.85
  ) +
  scale_x_continuous(
    breaks = sort(unique(brecha_genero$anio))
  ) +
  scale_y_continuous(
    labels = function(x) paste0(round(x, 1), "%"),
    expand = expansion(mult = c(0.08, 0.16))
  ) +
  labs(
    title = "Brecha porcentual del ingreso laboral por hora entre hombres y mujeres",
    subtitle = "Brecha calculada como porcentaje del ingreso promedio de los hombres",
    x = "",
    y = "Brecha porcentual (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 90, hjust = 1),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

g_brecha_genero

# Formalidad
geih <- read_dta(
  "Datos/Processed/Paper-GEIH_base_modelo_personas_2008_2025.dta"
) %>%
  mutate(
    anio = as.integer(anio),
    fex = as.numeric(fex),
    ingreso_hora_real = as.numeric(ingreso_hora_real),
    formal = as.numeric(formal),
    
    formalidad_grupo = case_when(
      formal == 1 ~ "Formal",
      formal == 0 ~ "Informal",
      TRUE ~ NA_character_
    ),
    
    formalidad_grupo = factor(
      formalidad_grupo,
      levels = c("Informal", "Formal")
    )
  )

#========================================================
# 2. Serie anual por formalidad
#========================================================

serie_ingreso_formalidad <- geih %>%
  filter(
    !is.na(anio),
    !is.na(formalidad_grupo),
    !is.na(ingreso_hora_real),
    ingreso_hora_real > 0,
    !is.na(fex),
    fex > 0
  ) %>%
  group_by(anio, formalidad_grupo) %>%
  summarise(
    observaciones = n(),
    trabajadores_expandidos = sum(fex, na.rm = TRUE),
    ingreso_hora_real_promedio = weighted.mean(
      ingreso_hora_real,
      fex,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  arrange(formalidad_grupo, anio)

serie_formalidad_plot <- serie_ingreso_formalidad %>%
  mutate(
    formalidad_grupo = factor(
      formalidad_grupo,
      levels = c("Informal", "Formal")
    )
  ) %>%
  arrange(formalidad_grupo, anio)

#--------------------------------------------------------
# 2. Etiquetas de valores: solo 2008 y 2025
#--------------------------------------------------------

labels_formalidad <- serie_formalidad_plot %>%
  filter(anio %in% c(2008, 2025)) %>%
  mutate(
    vjust_label = case_when(
      formalidad_grupo == "Formal" ~ -0.85,
      formalidad_grupo == "Informal" ~ 1.75,
      TRUE ~ -0.85
    )
  )

#--------------------------------------------------------
# 3. Cambio acumulado 2008-2025 por formalidad
#--------------------------------------------------------

cambio_formalidad <- serie_formalidad_plot %>%
  filter(anio %in% c(2008, 2025)) %>%
  select(anio, formalidad_grupo, ingreso_hora_real_promedio) %>%
  pivot_wider(
    names_from = anio,
    values_from = ingreso_hora_real_promedio,
    names_prefix = "y_"
  ) %>%
  mutate(
    cambio_pct = 100 * (y_2025 / y_2008 - 1),
    label_cambio = paste0(
      "Cambio 2008–2025: ",
      ifelse(cambio_pct >= 0, "+", ""),
      round(cambio_pct, 1),
      "%"
    )
  )

#--------------------------------------------------------
# 4. Posición de flechas y etiquetas de cambio
#    Cada flecha se ubica encima de su propia serie
#--------------------------------------------------------

max_y <- max(serie_formalidad_plot$ingreso_hora_real_promedio, na.rm = TRUE)
min_y <- min(serie_formalidad_plot$ingreso_hora_real_promedio, na.rm = TRUE)
rango_y <- max_y - min_y

pos_cambio_formalidad <- cambio_formalidad %>%
  left_join(
    serie_formalidad_plot %>%
      group_by(formalidad_grupo) %>%
      summarise(
        max_grupo = max(ingreso_hora_real_promedio, na.rm = TRUE),
        .groups = "drop"
      ),
    by = "formalidad_grupo"
  ) %>%
  mutate(
    x_inicio = 2008,
    x_fin = 2025,
    x_label = 2016.5,
    
    # Flecha justo encima de cada serie
    y_arrow = case_when(
      formalidad_grupo == "Formal" ~ max_grupo + 0.07 * rango_y,
      formalidad_grupo == "Informal" ~ max_grupo + 0.05 * rango_y,
      TRUE ~ max_grupo + 0.05 * rango_y
    )
  )

#--------------------------------------------------------
# 6. Gráfico pedagógico por formalidad
#--------------------------------------------------------

g_ingreso_formalidad_niveles <- ggplot(
  serie_formalidad_plot,
  aes(
    x = anio,
    y = ingreso_hora_real_promedio,
    color = formalidad_grupo,
    group = formalidad_grupo
  )
) +
  geom_line(
    linewidth = 1.35,
    alpha = 0.95
  ) +
  geom_point(
    size = 3.5,
    alpha = 0.95
  ) +
  geom_label(
    data = labels_formalidad,
    aes(
      label = comma(ingreso_hora_real_promedio, accuracy = 1),
      fill = formalidad_grupo,
      vjust = vjust_label
    ),
    color = "white",
    fontface = "bold",
    size = 3.4,
    label.size = 0.15,
    label.padding = unit(0.15, "lines"),
    show.legend = FALSE
  ) +
  geom_segment(
    data = pos_cambio_formalidad,
    aes(
      x = x_inicio,
      xend = x_fin,
      y = y_arrow,
      yend = y_arrow,
      color = formalidad_grupo
    ),
    inherit.aes = FALSE,
    linewidth = 0.9,
    arrow = arrow(
      length = unit(0.18, "cm"),
      type = "closed"
    ),
    show.legend = FALSE
  ) +
  geom_label(
    data = pos_cambio_formalidad,
    aes(
      x = x_label,
      y = y_arrow,
      label = label_cambio,
      fill = formalidad_grupo
    ),
    inherit.aes = FALSE,
    color = "white",
    fontface = "bold",
    size = 3.8,
    label.size = 0.15,
    label.padding = unit(0.18, "lines"),
    show.legend = FALSE
  ) +
  scale_color_manual(
    values = c(
      "Informal" = "darkred",
      "Formal" = "darkblue"
    ),
    name = ""
  ) +
  scale_fill_manual(
    values = c(
      "Informal" = "darkred",
      "Formal" = "darkblue"
    )
  ) +
  scale_x_continuous(
    breaks = sort(unique(serie_formalidad_plot$anio))
  ) +
  scale_y_continuous(
    labels = comma,
    expand = expansion(mult = c(0.08, 0.24))
  ) +
  labs(
    title = "Evolución del ingreso laboral por hora promedio por formalidad, 2008–2025",
    subtitle = "Pesos constantes de 2025. Promedio ponderado por factores de expansión",
    x = "",
    y = "Ingreso laboral por hora promedio"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 90, hjust = 1),
    axis.text.y = element_text(size = 10),
    legend.position = "bottom",
    legend.text = element_text(face = "bold", size = 11),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

g_ingreso_formalidad_niveles

# Brecha
geih <- read_dta(
  "Datos/Processed/Paper-GEIH_base_modelo_personas_2008_2025.dta"
) %>%
  mutate(
    anio = as.integer(anio),
    fex = as.numeric(fex),
    ingreso_hora_real = as.numeric(ingreso_hora_real),
    formal = as.numeric(formal),
    
    formalidad_grupo = case_when(
      formal == 1 ~ "Formal",
      formal == 0 ~ "Informal",
      TRUE ~ NA_character_
    )
  )

#========================================================
# 2. Función de media ponderada
#========================================================

weighted_mean <- function(x, w) {
  ok <- !is.na(x) & !is.na(w) & w > 0
  
  if (!any(ok)) return(NA_real_)
  
  weighted.mean(x[ok], w[ok])
}

#========================================================
# 3. Serie anual de ingreso por formalidad
#========================================================

serie_ingreso_formalidad <- geih %>%
  filter(
    !is.na(anio),
    !is.na(formalidad_grupo),
    !is.na(ingreso_hora_real),
    ingreso_hora_real > 0,
    !is.na(fex),
    fex > 0
  ) %>%
  group_by(anio, formalidad_grupo) %>%
  summarise(
    ingreso_hora_real_promedio = weighted.mean(ingreso_hora_real, fex),
    .groups = "drop"
  )

#========================================================
# 4. Construir la brecha formal vs informal
#========================================================

serie_brecha_formalidad <- serie_ingreso_formalidad %>%
  tidyr::pivot_wider(
    names_from = formalidad_grupo,
    values_from = ingreso_hora_real_promedio
  ) %>%
  mutate(
    brecha_pct = 100 * (Formal / Informal - 1),
    label_brecha = paste0(round(brecha_pct, 1), "%")
  ) %>%
  arrange(anio)

#========================================================
# 5. Gráfico de la brecha entre formales e informales
#========================================================

g_brecha_formal_informal <- ggplot(
  serie_brecha_formalidad,
  aes(x = anio, y = brecha_pct)
) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "gray50",
    linewidth = 0.7
  ) +
  geom_line(
    color = "darkblue",
    linewidth = 1.2
  ) +
  geom_point(
    color = "darkblue",
    size = 3.5
  ) +
  geom_label(
    aes(label = label_brecha),
    fill = "darkblue",
    color = "white",
    fontface = "bold",
    size = 3.4,
    label.size = 0.15,
    label.padding = unit(0.15, "lines"),
    vjust = -0.6
  ) +
  scale_x_continuous(
    breaks = 2008:2025
  ) +
  scale_y_continuous(
    labels = function(x) paste0(round(x, 0), "%"),
    expand = expansion(mult = c(0.08, 0.16))
  ) +
  labs(
    title = "Brecha del ingreso laboral por hora entre formales e informales, 2008–2025",
    subtitle = "Brecha porcentual: ((Ingreso formal / ingreso informal) - 1) × 100",
    x = "Año",
    y = "Brecha porcentual (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 90, hjust = 1)
  )

g_brecha_formal_informal


# Nivel educativo
geih <- read_dta(
  "Datos/Processed/Paper-GEIH_base_modelo_personas_2008_2025.dta"
) %>%
  mutate(
    anio = as.integer(anio),
    fex = as.numeric(fex),
    ingreso_hora_real = as.numeric(ingreso_hora_real),
    educacion = as.character(educacion)
  )

#========================================================
# 2. Función de media ponderada
#========================================================

weighted_mean <- function(x, w) {
  ok <- !is.na(x) & !is.na(w) & w > 0
  
  if (!any(ok)) return(NA_real_)
  
  weighted.mean(x[ok], w[ok])
}

#========================================================
# 3. Serie por nivel educativo
#========================================================

serie_ingreso_educacion <- geih %>%
  filter(
    !is.na(anio),
    anio %in% c(2008, 2025),
    !is.na(educacion),
    educacion != "No sabe, no informa",
    !is.na(ingreso_hora_real),
    ingreso_hora_real > 0,
    !is.na(fex),
    fex > 0
  ) %>%
  mutate(
    educacion = factor(
      educacion,
      levels = c(
        "Ninguno",
        "Preescolar",
        "Básica primaria",
        "Básica secundaria",
        "Media",
        "Superior o universitaria"
      )
    )
  ) %>%
  group_by(anio, educacion) %>%
  summarise(
    observaciones = n(),
    trabajadores_expandidos = sum(fex, na.rm = TRUE),
    ingreso_hora_real_promedio = weighted.mean(ingreso_hora_real, fex),
    .groups = "drop"
  ) %>%
  filter(!is.na(educacion)) %>%
  mutate(
    anio = factor(anio, levels = c(2008, 2025))
  )

#========================================================
# 4. Gráfico de barras horizontales
#========================================================

g_barras_educacion_2008_2025 <- ggplot(
  serie_ingreso_educacion,
  aes(
    x = educacion,
    y = ingreso_hora_real_promedio,
    fill = anio
  )
) +
  geom_col(
    position = position_dodge(width = 0.72),
    width = 0.62
  ) +
  geom_text(
    aes(
      label = comma(ingreso_hora_real_promedio, accuracy = 1)
    ),
    position = position_dodge(width = 0.72),
    hjust = -0.12,
    size = 3.4,
    fontface = "bold"
  ) +
  coord_flip() +
  scale_fill_manual(
    values = c(
      "2008" = "darkred",
      "2025" = "darkblue"
    )
  ) +
  scale_y_continuous(
    labels = comma,
    expand = expansion(mult = c(0.02, 0.16))
  ) +
  labs(
    title = "Ingreso laboral por hora real por nivel educativo: 2008 vs. 2025",
    subtitle = "Pesos constantes de 2025. Promedio ponderado por factores de expansión",
    x = "Nivel educativo",
    y = "Ingreso laboral por hora promedio, pesos de 2025",
    fill = "Año"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

g_barras_educacion_2008_2025

# Ocupacion
geih <- read_dta(
  "Datos/Processed/Paper-GEIH_base_modelo_personas_2008_2025.dta"
) %>%
  mutate(
    anio = as.integer(anio),
    fex = as.numeric(fex),
    ingreso_hora_real = as.numeric(ingreso_hora_real),
    ocupacion = as.character(ocupacion)
  )

#========================================================
# 2. Crear labels cortos de ocupación
#========================================================

geih <- geih %>%
  mutate(
    ocupacion_label = case_when(
      ocupacion == "Obrero o empleado de empresa particular" ~ "Empleado particular",
      ocupacion == "Obrero o empleado del gobierno" ~ "Empleado gobierno",
      ocupacion == "Empleado doméstico" ~ "Servicio doméstico",
      ocupacion == "Trabajador por cuenta propia" ~ "Cuenta propia",
      ocupacion == "Patrón o empleador" ~ "Patrón/empleador",
      TRUE ~ ocupacion
    )
  )

#========================================================
# 3. Seleccionar ocupaciones
#========================================================

ocupaciones_seleccionadas <- c(
  "Empleado particular",
  "Empleado gobierno",
  "Servicio doméstico",
  "Cuenta propia",
  "Patrón/empleador"
)

#========================================================
# 4. Calcular ingreso promedio por ocupación: 2008 vs 2025
#========================================================

serie_ocupacion_2008_2025 <- geih %>%
  filter(
    anio %in% c(2008, 2025),
    ocupacion_label %in% ocupaciones_seleccionadas,
    !is.na(ingreso_hora_real),
    ingreso_hora_real > 0,
    !is.na(fex),
    fex > 0
  ) %>%
  mutate(
    ocupacion_label = factor(
      ocupacion_label,
      levels = c(
        "Servicio doméstico",
        "Cuenta propia",
        "Empleado particular",
        "Empleado gobierno",
        "Patrón/empleador"
      )
    ),
    anio = factor(anio, levels = c(2008, 2025))
  ) %>%
  group_by(anio, ocupacion_label) %>%
  summarise(
    observaciones = n(),
    trabajadores_expandidos = sum(fex, na.rm = TRUE),
    ingreso_hora_real_promedio = weighted.mean(
      ingreso_hora_real,
      fex,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

#========================================================
# 5. Gráfico de barras horizontales
#========================================================

g_barras_ocupacion_2008_2025 <- ggplot(
  serie_ocupacion_2008_2025,
  aes(
    x = ocupacion_label,
    y = ingreso_hora_real_promedio,
    fill = anio
  )
) +
  geom_col(
    position = position_dodge(width = 0.72),
    width = 0.62
  ) +
  geom_text(
    aes(
      label = comma(ingreso_hora_real_promedio, accuracy = 1)
    ),
    position = position_dodge(width = 0.72),
    hjust = -0.12,
    size = 3.4,
    fontface = "bold"
  ) +
  coord_flip() +
  scale_fill_manual(
    values = c(
      "2008" = "darkred",
      "2025" = "darkblue"
    )
  ) +
  scale_y_continuous(
    labels = comma,
    expand = expansion(mult = c(0.02, 0.18))
  ) +
  labs(
    title = "Ingreso laboral por hora real por posición ocupacional: 2008 vs. 2025",
    subtitle = "Pesos constantes de 2025. Promedio ponderado por factores de expansión",
    x = "Posición ocupacional",
    y = "Ingreso laboral por hora promedio",
    fill = "Año"
  ) +
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(face = "bold"),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  )

g_barras_ocupacion_2008_2025

# Por Departamento
geih <- read_dta(
  "Datos/Processed/Paper-GEIH_base_modelo_personas_2008_2025.dta"
) %>%
  mutate(
    anio = as.integer(anio),
    fex = as.numeric(fex),
    ingreso_hora_real = as.numeric(ingreso_hora_real),
    depto = as.numeric(depto)
  )

#========================================================
# 2. Etiquetas de departamento
#========================================================

geih <- geih %>%
  mutate(
    depto_label = case_when(
      depto == 5  ~ "Antioquia",
      depto == 8  ~ "Atlántico",
      depto == 11 ~ "Bogotá D.C.",
      depto == 13 ~ "Bolívar",
      depto == 15 ~ "Boyacá",
      depto == 17 ~ "Caldas",
      depto == 18 ~ "Caquetá",
      depto == 19 ~ "Cauca",
      depto == 20 ~ "Cesar",
      depto == 23 ~ "Córdoba",
      depto == 25 ~ "Cundinamarca",
      depto == 27 ~ "Chocó",
      depto == 41 ~ "Huila",
      depto == 44 ~ "La Guajira",
      depto == 47 ~ "Magdalena",
      depto == 50 ~ "Meta",
      depto == 52 ~ "Nariño",
      depto == 54 ~ "Norte de Santander",
      depto == 63 ~ "Quindío",
      depto == 66 ~ "Risaralda",
      depto == 68 ~ "Santander",
      depto == 70 ~ "Sucre",
      depto == 73 ~ "Tolima",
      depto == 76 ~ "Valle del Cauca",
      depto == 81 ~ "Arauca",
      depto == 85 ~ "Casanare",
      depto == 86 ~ "Putumayo",
      depto == 88 ~ "San Andrés",
      depto == 91 ~ "Amazonas",
      depto == 94 ~ "Guainía",
      depto == 95 ~ "Guaviare",
      depto == 97 ~ "Vaupés",
      depto == 99 ~ "Vichada",
      TRUE ~ paste0("Depto ", depto)
    )
  )

#========================================================
# 3. Departamentos que quieres mostrar
#========================================================

deptos_seleccionados <- c(
  "Antioquia",
  "Valle del Cauca",
  "Bogotá D.C.",
  "Atlántico",
  "Santander",
  "Caldas",
  "Risaralda",
  "Bolívar",
  "Tolima",
  "Meta",
  "Norte de Santander",
  "Nariño",
  "Córdoba"
)

#========================================================
# 4. Base 2008 vs 2025 por departamento
#========================================================

serie_depto_2008_2025 <- geih %>%
  filter(
    anio %in% c(2008, 2025),
    depto_label %in% deptos_seleccionados,
    !is.na(ingreso_hora_real),
    ingreso_hora_real > 0,
    !is.na(fex),
    fex > 0
  ) %>%
  group_by(anio, depto_label) %>%
  summarise(
    observaciones = n(),
    trabajadores_expandidos = sum(fex, na.rm = TRUE),
    ingreso_hora_real_promedio = weighted.mean(
      ingreso_hora_real,
      fex,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

#========================================================
# 5. Ordenar departamentos según 2025
#========================================================

orden_2025 <- serie_depto_2008_2025 %>%
  filter(anio == 2025) %>%
  arrange(ingreso_hora_real_promedio) %>%
  pull(depto_label)

serie_depto_2008_2025 <- serie_depto_2008_2025 %>%
  mutate(
    depto_label = factor(depto_label, levels = orden_2025),
    anio = factor(anio, levels = c(2008, 2025))
  )

#========================================================
# 6. Gráfico de barras horizontales
#========================================================

g_depto_2008_2025 <- ggplot(
  serie_depto_2008_2025,
  aes(
    x = depto_label,
    y = ingreso_hora_real_promedio,
    fill = anio
  )
) +
  geom_col(
    position = position_dodge(width = 0.72),
    width = 0.62
  ) +
  geom_text(
    aes(label = comma(ingreso_hora_real_promedio, accuracy = 1)),
    position = position_dodge(width = 0.72),
    hjust = -0.10,
    size = 3.2,
    fontface = "bold"
  ) +
  coord_flip() +
  scale_fill_manual(
    values = c(
      "2008" = "darkred",
      "2025" = "darkblue"
    )
  ) +
  scale_y_continuous(
    labels = comma,
    expand = expansion(mult = c(0.02, 0.18))
  ) +
  labs(
    title = "Ingreso laboral por hora real por departamento: 2008 vs. 2025",
    subtitle = "Solo departamentos seleccionados. Pesos constantes de 2025",
    x = "Departamento",
    y = "Ingreso laboral por hora promedio",
    fill = "Año"
  ) +
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(face = "bold"),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  )

g_depto_2008_2025

# Por sector
geih <- read_dta(
  "Datos/Processed/Paper-GEIH_base_modelo_personas_2008_2025.dta"
) %>%
  mutate(
    anio = as.integer(anio),
    fex = as.numeric(fex),
    ingreso_hora_real = as.numeric(ingreso_hora_real),
    sector = as.character(sector)
  )

#========================================================
# 2. Labels cortos de sector
#========================================================

geih <- geih %>%
  mutate(
    sector_label = case_when(
      sector == "Agricultura, ganadería, silvicultura y pesca" ~ "Agricultura",
      sector == "Minas y canteras" ~ "Minas",
      sector == "Industrias manufactureras" ~ "Manufactura",
      sector == "Electricidad, gas, agua y saneamiento" ~ "Servicios públicos",
      sector == "Construcción" ~ "Construcción",
      sector == "Comercio y reparación" ~ "Comercio",
      sector == "Alojamiento y servicios de comida" ~ "Alojamiento y comida",
      sector == "Transporte y almacenamiento" ~ "Transporte",
      sector == "Información y comunicaciones" ~ "Información y comunicaciones",
      sector == "Actividades financieras y de seguros" ~ "Financieras",
      sector == "Inmobiliarias, profesionales y administrativas" ~ "Inmobiliarias/profesionales",
      sector == "Administración pública y defensa" ~ "Adm. pública",
      sector == "Educación" ~ "Educación",
      sector == "Salud y asistencia social" ~ "Salud",
      sector == "Artes, recreación y otros servicios" ~ "Artes y otros servicios",
      sector == "Hogares como empleadores" ~ "Hogares empleadores",
      sector == "Organizaciones extraterritoriales" ~ "Extraterritoriales",
      TRUE ~ sector
    )
  )

#========================================================
# 3. Calcular ingreso promedio por sector: 2008 y 2025
#========================================================

serie_sector_2008_2025 <- geih %>%
  filter(
    anio %in% c(2008, 2025),
    !is.na(sector_label),
    !is.na(ingreso_hora_real),
    ingreso_hora_real > 0,
    !is.na(fex),
    fex > 0
  ) %>%
  group_by(anio, sector_label) %>%
  summarise(
    observaciones = n(),
    trabajadores_expandidos = sum(fex, na.rm = TRUE),
    ingreso_hora_real_promedio = weighted.mean(
      ingreso_hora_real,
      fex,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

#========================================================
# 4. Quedarse solo con sectores que tienen 2008 y 2025
#========================================================

sectores_2008_2025 <- serie_sector_2008_2025 %>%
  group_by(sector_label) %>%
  summarise(
    n_anios = n_distinct(anio),
    .groups = "drop"
  ) %>%
  filter(n_anios == 2)

serie_sector_2008_2025 <- serie_sector_2008_2025 %>%
  semi_join(
    sectores_2008_2025,
    by = "sector_label"
  )

#========================================================
# 5. Clasificar sectores en ingresos altos y bajos
#    usando el ingreso de 2025
#========================================================

corte_2025 <- serie_sector_2008_2025 %>%
  filter(anio == 2025) %>%
  summarise(
    mediana_2025 = median(ingreso_hora_real_promedio, na.rm = TRUE)
  ) %>%
  pull(mediana_2025)

sector_grupos <- serie_sector_2008_2025 %>%
  filter(anio == 2025) %>%
  mutate(
    grupo_ingreso = if_else(
      ingreso_hora_real_promedio >= corte_2025,
      "Sectores de ingresos altos",
      "Sectores de ingresos bajos"
    )
  ) %>%
  select(sector_label, grupo_ingreso)

serie_sector_2008_2025 <- serie_sector_2008_2025 %>%
  left_join(
    sector_grupos,
    by = "sector_label"
  )

#========================================================
# 6. Ordenar sectores según ingreso de 2025
#========================================================

orden_sector_2025 <- serie_sector_2008_2025 %>%
  filter(anio == 2025) %>%
  arrange(ingreso_hora_real_promedio) %>%
  pull(sector_label)

serie_sector_2008_2025 <- serie_sector_2008_2025 %>%
  mutate(
    sector_label = factor(sector_label, levels = orden_sector_2025),
    anio = factor(anio, levels = c(2008, 2025)),
    grupo_ingreso = factor(
      grupo_ingreso,
      levels = c(
        "Sectores de ingresos altos",
        "Sectores de ingresos bajos"
      )
    )
  )

#========================================================
# 7. Gráfico de barras horizontales por grupo
#========================================================

g_sector_barras_2008_2025 <- ggplot(
  serie_sector_2008_2025,
  aes(
    x = ingreso_hora_real_promedio,
    y = sector_label,
    fill = anio
  )
) +
  geom_col(
    position = position_dodge(width = 0.72),
    width = 0.62
  ) +
  geom_text(
    aes(
      label = comma(ingreso_hora_real_promedio, accuracy = 1)
    ),
    position = position_dodge(width = 0.72),
    hjust = -0.10,
    size = 3.1,
    fontface = "bold"
  ) +
  facet_wrap(
    ~ grupo_ingreso,
    scales = "free_y",
    ncol = 1
  ) +
  scale_fill_manual(
    values = c(
      "2008" = "darkred",
      "2025" = "darkblue"
    )
  ) +
  scale_x_continuous(
    labels = comma,
    expand = expansion(mult = c(0.02, 0.20))
  ) +
  labs(
    title = "Ingreso laboral por hora real por sector económico: 2008 vs. 2025",
    subtitle = "Sectores agrupados según su nivel de ingreso promedio en 2025. Pesos constantes de 2025",
    x = "Ingreso laboral por hora promedio",
    y = "Sector económico",
    fill = "Año"
  ) +
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(face = "bold"),
    axis.text.y = element_text(size = 9),
    axis.text.x = element_text(size = 10),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold", size = 12),
    panel.grid.major.x = element_line(color = "gray90", linewidth = 0.3),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

g_sector_barras_2008_2025

# Tamanio
geih <- read_dta(
  "Datos/Processed/Paper-GEIH_base_modelo_personas_2008_2025.dta"
) %>%
  mutate(
    anio = as.integer(anio),
    fex = as.numeric(fex),
    ingreso_hora_real = as.numeric(ingreso_hora_real),
    tamano_empresa = as.character(tamano_empresa)
  )

#========================================================
# 2. Orden del tamaño de empresa
#========================================================

orden_tamano <- c(
  "Solo", "2-3", "4-5", "6-10", "11-19",
  "20-30", "31-50", "51-100", "101+"
)

#========================================================
# 3. Serie 2008 vs 2025
#========================================================

serie_tamano_2008_2025 <- geih %>%
  filter(
    anio %in% c(2008, 2025),
    !is.na(tamano_empresa),
    !is.na(ingreso_hora_real),
    ingreso_hora_real > 0,
    !is.na(fex),
    fex > 0
  ) %>%
  mutate(
    tamano_empresa = factor(
      tamano_empresa,
      levels = orden_tamano
    ),
    anio = factor(anio, levels = c(2008, 2025))
  ) %>%
  filter(!is.na(tamano_empresa)) %>%
  group_by(anio, tamano_empresa) %>%
  summarise(
    observaciones = n(),
    trabajadores_expandidos = sum(fex, na.rm = TRUE),
    ingreso_hora_real_promedio = weighted.mean(
      ingreso_hora_real,
      fex,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  arrange(tamano_empresa, anio)

#========================================================
# 4. Gráfico corregido
#========================================================

g_barras_tamano_2008_2025_flip <- ggplot(
  serie_tamano_2008_2025,
  aes(
    x = tamano_empresa,
    y = ingreso_hora_real_promedio,
    fill = anio
  )
) +
  geom_col(
    position = position_dodge(width = 0.72),
    width = 0.62,
    alpha = 0.95
  ) +
  geom_label(
    aes(
      label = comma(ingreso_hora_real_promedio, accuracy = 1),
      fill = anio
    ),
    position = position_dodge(width = 0.72),
    color = "white",
    fontface = "bold",
    size = 3.2,
    label.size = 0.15,
    label.padding = unit(0.15, "lines"),
    hjust = -0.08,
    show.legend = FALSE
  ) +
  scale_fill_manual(
    values = c(
      "2008" = "darkred",
      "2025" = "darkblue"
    )
  ) +
  scale_y_continuous(
    labels = comma,
    expand = expansion(mult = c(0.02, 0.18))
  ) +
  coord_flip(clip = "off") +
  labs(
    title = "Ingreso laboral por hora real según tamaño de empresa",
    subtitle = "Comparación 2008 vs. 2025. Promedio ponderado por factores de expansión",
    x = "Tamaño de empresa",
    y = "Ingreso laboral por hora promedio, pesos de 2025",
    fill = "Año"
  ) +
  theme_classic(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    plot.margin = margin(10, 40, 10, 10)
  )

g_barras_tamano_2008_2025_flip

# Serie de informalidad
geih <- read_dta(
  "Datos/Processed/Paper-GEIH_base_modelo_personas_2008_2025.dta"
) %>%
  mutate(
    anio = as.integer(anio),
    fex = as.numeric(fex),
    formal = as.numeric(formal)
  )

#========================================================
# 2. Calcular tasa de informalidad anual
#========================================================

serie_informalidad <- geih %>%
  filter(
    !is.na(anio),
    !is.na(fex),
    fex > 0,
    !is.na(formal)
  ) %>%
  mutate(
    informal = if_else(formal == 0, 1, 0)
  ) %>%
  group_by(anio) %>%
  summarise(
    observaciones = n(),
    trabajadores_total = sum(fex, na.rm = TRUE),
    trabajadores_informales = sum(fex * informal, na.rm = TRUE),
    tasa_informalidad = trabajadores_informales / trabajadores_total,
    .groups = "drop"
  ) %>%
  arrange(anio)

g_tasa_informalidad <- ggplot(
  serie_informalidad,
  aes(
    x = anio,
    y = tasa_informalidad
  )
) +
  geom_line(
    color = "black",
    linewidth = 1
  ) +
  geom_point(
    color = "darkred",
    size = 3
  ) +
  geom_text(
    aes(
      label = percent(tasa_informalidad, accuracy = 0.1)
    ),
    vjust = -0.8,
    size = 3.5
  ) +
  scale_x_continuous(
    breaks = 2008:2025
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    expand = expansion(mult = c(0.08, 0.16))
  ) +
  labs(
    title = "Tasa de informalidad laboral en Colombia, 2008–2025",
    subtitle = "Porcentaje de trabajadores que no cotizan a pensión. Cálculo ponderado por factores de expansión",
    x = "Año",
    y = "Tasa de informalidad"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  )

g_tasa_informalidad

# Masa laboral
geih <- read_dta(
  "Datos/Processed/Paper-GEIH_base_modelo_personas_2008_2025.dta"
) %>%
  mutate(
    anio = as.integer(anio),
    fex = as.numeric(fex),
    sexo = as.character(sexo),
    formal = as.numeric(formal),
    
    formalidad_grupo = case_when(
      formal == 1 ~ "Formal",
      formal == 0 ~ "Informal",
      TRUE ~ NA_character_
    ),
    
    formalidad_grupo = factor(
      formalidad_grupo,
      levels = c("Informal", "Formal")
    )
  )

serie_trabajadores_total <- geih %>%
  filter(
    !is.na(anio),
    !is.na(fex),
    fex > 0
  ) %>%
  group_by(anio) %>%
  summarise(
    observaciones = n(),
    trabajadores_expandidos = sum(fex, na.rm = TRUE),
    trabajadores_millones = trabajadores_expandidos / 1e6,
    .groups = "drop"
  ) %>%
  arrange(anio)

# Cambio acumulado 2008-2025
trab_2008 <- serie_trabajadores_total %>%
  filter(anio == 2008) %>%
  pull(trabajadores_millones)

trab_2025 <- serie_trabajadores_total %>%
  filter(anio == 2025) %>%
  pull(trabajadores_millones)

cambio_trabajadores <- 100 * (trab_2025 / trab_2008 - 1)

label_cambio_trabajadores <- paste0(
  "Cambio 2008–2025: ",
  ifelse(cambio_trabajadores >= 0, "+", ""),
  round(cambio_trabajadores, 1),
  "%"
)

max_y <- max(serie_trabajadores_total$trabajadores_millones, na.rm = TRUE)
min_y <- min(serie_trabajadores_total$trabajadores_millones, na.rm = TRUE)
rango_y <- max_y - min_y

y_arrow <- max_y + 0.12 * rango_y

labels_total <- serie_trabajadores_total %>%
  filter(anio %in% c(2008, 2025))

g_trabajadores_total <- ggplot(
  serie_trabajadores_total,
  aes(
    x = anio,
    y = trabajadores_millones
  )
) +
  geom_line(
    color = "darkblue",
    linewidth = 1.35
  ) +
  geom_point(
    color = "darkblue",
    size = 3.6
  ) +
  geom_label(
    data = labels_total,
    aes(
      label = number(trabajadores_millones, accuracy = 0.1)
    ),
    fill = "darkblue",
    color = "white",
    fontface = "bold",
    size = 3.6,
    label.size = 0.15,
    label.padding = unit(0.15, "lines"),
    vjust = -0.85
  ) +
  geom_segment(
    aes(
      x = 2008,
      xend = 2025,
      y = y_arrow,
      yend = y_arrow
    ),
    inherit.aes = FALSE,
    color = "gray35",
    linewidth = 0.85,
    arrow = arrow(
      length = unit(0.18, "cm"),
      type = "closed"
    )
  ) +
  geom_label(
    aes(
      x = 2016.5,
      y = y_arrow,
      label = label_cambio_trabajadores
    ),
    inherit.aes = FALSE,
    fill = "gray20",
    color = "white",
    fontface = "bold",
    size = 4,
    label.size = 0.15,
    label.padding = unit(0.18, "lines")
  ) +
  scale_x_continuous(
    breaks = sort(unique(serie_trabajadores_total$anio))
  ) +
  scale_y_continuous(
    labels = function(x) paste0(number(x, accuracy = 0.1), " M"),
    expand = expansion(mult = c(0.08, 0.24))
  ) +
  labs(
    title = "Evolución del número de trabajadores, 2008–2025",
    subtitle = "Trabajadores expandidos con factores de expansión. Valores en millones",
    x = "",
    y = "Trabajadores, millones"
  ) +
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 90, hjust = 1),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

g_trabajadores_total

