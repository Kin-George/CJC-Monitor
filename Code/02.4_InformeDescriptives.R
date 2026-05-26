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
  "Datos/Processed/GEIH_base_modelo_personas_2008_2025.dta"
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

#--------------------------------------------------------
# 5. Preparar datos para etiquetas y variaciones
#--------------------------------------------------------

#--------------------------------------------------------
# 5. Preparar datos para etiquetas y variaciones
#--------------------------------------------------------

max_y <- max(serie_ingreso_real$ingreso_hora_real_promedio, na.rm = TRUE)

serie_plot <- serie_ingreso_real %>%
  arrange(anio) %>%
  mutate(
    y_label_valor = ingreso_hora_real_promedio + 0.03 * max_y
  )

cambios_plot <- serie_plot %>%
  mutate(
    anio_prev = lag(anio),
    ingreso_prev = lag(ingreso_hora_real_promedio),
    y_label_prev = lag(y_label_valor)
  ) %>%
  filter(!is.na(anio_prev)) %>%
  mutate(
    variacion_pct = 100 * (ingreso_hora_real_promedio / ingreso_prev - 1),
    label_variacion = paste0(
      ifelse(variacion_pct >= 0, "+", ""),
      round(variacion_pct, 1),
      "%"
    ),
    x_mid = (anio_prev + anio) / 2,
    y_barra = pmax(y_label_prev, y_label_valor) + 0.03 * max_y,
    y_barra_tick = y_barra - 0.015 * max_y,
    y_label_variacion = y_barra + 0.02 * max_y,
    color_variacion = ifelse(variacion_pct >= 0, "darkgreen", "darkred")
  )

#--------------------------------------------------------
# 6. Gráfico
#--------------------------------------------------------

g_puntos_stems_cambios <- ggplot(
  serie_plot,
  aes(
    x = anio,
    y = ingreso_hora_real_promedio
  )
) +
  # líneas verticales desde el eje X
  geom_segment(
    aes(
      x = anio,
      xend = anio,
      y = 0,
      yend = ingreso_hora_real_promedio
    ),
    color = "darkgrey",
    linewidth = 1.25
  ) +
  # puntos
  geom_point(
    color = "darkblue",
    size = 4.5,
    alpha = 0.95
  ) +
  # etiquetas de valores
  geom_label(
    aes(
      y = y_label_valor,
      label = comma(ingreso_hora_real_promedio, accuracy = 1)
    ),
    fill = "black",
    color = "white",
    fontface = "bold",
    size = 3.5,
    label.size = 0.15,
    label.padding = unit(0.15, "lines")
  ) +
  # barra horizontal gris entre años
  geom_segment(
    data = cambios_plot,
    aes(
      x = anio_prev,
      xend = anio,
      y = y_barra,
      yend = y_barra
    ),
    inherit.aes = FALSE,
    color = "gray50",
    linewidth = 0.45
  ) +
  # pequeñas líneas verticales en los extremos de la barra
  geom_segment(
    data = cambios_plot,
    aes(
      x = anio_prev,
      xend = anio_prev,
      y = y_barra_tick,
      yend = y_barra
    ),
    inherit.aes = FALSE,
    color = "gray50",
    linewidth = 0.45
  ) +
  geom_segment(
    data = cambios_plot,
    aes(
      x = anio,
      xend = anio,
      y = y_barra_tick,
      yend = y_barra
    ),
    inherit.aes = FALSE,
    color = "gray50",
    linewidth = 0.6
  ) +
  # texto con variación porcentual
  geom_text(
    data = cambios_plot,
    aes(
      x = x_mid,
      y = y_label_variacion,
      label = label_variacion,
      color = color_variacion
    ),
    inherit.aes = FALSE,
    fontface = "bold",
    size = 3.6,
    show.legend = FALSE
  ) +
  scale_color_identity() +
  scale_x_continuous(
    breaks = sort(unique(serie_plot$anio))
  ) +
  scale_y_continuous(
    labels = comma,
    expand = expansion(mult = c(0.02, 0.20))
  ) +
  labs(
    title = "Evolución del ingreso por hora promedio, 2008–2025",
    subtitle = "Pesos constantes (2025)",
    x = "",
    y = "Ingreso laboral por hora promedio"
  ) +
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 90, hjust = 1),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

g_puntos_stems_cambios

# Sexo
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
# 5. Base de brecha
#--------------------------------------------------------

brecha_genero <- serie_genero %>%
  select(anio, sexo, ingreso_hora_real_promedio) %>%
  pivot_wider(
    names_from = sexo,
    values_from = ingreso_hora_real_promedio
  ) %>%
  mutate(
    brecha_pct = 100 * (Hombre / Mujer - 1),
    label_brecha = paste0(round(brecha_pct, 1), "%")
  )

#--------------------------------------------------------
# 6. Reescalar brecha para graficarla con segundo eje
#--------------------------------------------------------

y_min <- min(
  c(brecha_genero$Hombre, brecha_genero$Mujer),
  na.rm = TRUE
)

y_max <- max(
  c(brecha_genero$Hombre, brecha_genero$Mujer),
  na.rm = TRUE
)

b_min <- min(brecha_genero$brecha_pct, na.rm = TRUE)
b_max <- max(brecha_genero$brecha_pct, na.rm = TRUE)

map_brecha_to_y <- function(b) {
  y_min + (b - b_min) / (b_max - b_min) * (y_max - y_min)
}

map_y_to_brecha <- function(y) {
  b_min + (y - y_min) / (y_max - y_min) * (b_max - b_min)
}

brecha_genero <- brecha_genero %>%
  mutate(
    brecha_y = map_brecha_to_y(brecha_pct)
  )

#--------------------------------------------------------
# 7. Gráfico con segundo eje y leyenda corregida
#--------------------------------------------------------

g_ingreso_genero_secaxis <- ggplot() +
  
  # Línea hombres
  geom_line(
    data = serie_genero %>% filter(sexo == "Hombre"),
    aes(
      x = anio,
      y = ingreso_hora_real_promedio,
      color = sexo,
      linetype = sexo,
      group = sexo
    ),
    linewidth = 1.1
  ) +
  
  geom_point(
    data = serie_genero %>% filter(sexo == "Hombre"),
    aes(
      x = anio,
      y = ingreso_hora_real_promedio,
      color = sexo
    ),
    size = 2.8,
    show.legend = FALSE
  ) +
  
  # Línea mujeres
  geom_line(
    data = serie_genero %>% filter(sexo == "Mujer"),
    aes(
      x = anio,
      y = ingreso_hora_real_promedio,
      color = sexo,
      linetype = sexo,
      group = sexo
    ),
    linewidth = 1.1
  ) +
  
  geom_point(
    data = serie_genero %>% filter(sexo == "Mujer"),
    aes(
      x = anio,
      y = ingreso_hora_real_promedio,
      color = sexo
    ),
    size = 2.8,
    show.legend = FALSE
  ) +
  
  # Etiquetas hombres arriba
  geom_label(
    data = serie_genero %>% filter(sexo == "Hombre"),
    aes(
      x = anio,
      y = ingreso_hora_real_promedio,
      label = comma(ingreso_hora_real_promedio, accuracy = 1)
    ),
    fill = "darkblue",
    color = "white",
    fontface = "bold",
    size = 3.1,
    label.size = 0.12,
    label.padding = unit(0.13, "lines"),
    vjust = -0.85,
    show.legend = FALSE
  ) +
  
  # Etiquetas mujeres abajo
  geom_label(
    data = serie_genero %>% filter(sexo == "Mujer"),
    aes(
      x = anio,
      y = ingreso_hora_real_promedio,
      label = comma(ingreso_hora_real_promedio, accuracy = 1)
    ),
    fill = "darkred",
    color = "white",
    fontface = "bold",
    size = 3.1,
    label.size = 0.12,
    label.padding = unit(0.13, "lines"),
    vjust = 1.7,
    show.legend = FALSE
  ) +
  
  # Línea de brecha: ahora entra en la leyenda
  geom_line(
    data = brecha_genero,
    aes(
      x = anio,
      y = brecha_y,
      color = "Brecha",
      linetype = "Brecha",
      group = 1
    ),
    linewidth = 1,
    show.legend = TRUE
  ) +
  
  geom_point(
    data = brecha_genero,
    aes(
      x = anio,
      y = brecha_y
    ),
    color = "gray30",
    size = 2.3,
    show.legend = FALSE
  ) +
  
  # Etiquetas de brecha
  geom_text(
    data = brecha_genero,
    aes(
      x = anio,
      y = brecha_y,
      label = label_brecha
    ),
    color = "black",
    fontface = "bold",
    size = 3.5,
    vjust = -0.9,
    show.legend = FALSE
  ) +
  
  scale_color_manual(
    name = NULL,
    values = c(
      "Hombre" = "darkblue",
      "Mujer" = "darkred",
      "Brecha" = "gray30"
    ),
    breaks = c("Hombre", "Mujer", "Brecha")
  ) +
  
  scale_linetype_manual(
    name = NULL,
    values = c(
      "Hombre" = "solid",
      "Mujer" = "solid",
      "Brecha" = "dashed"
    ),
    breaks = c("Hombre", "Mujer", "Brecha")
  ) +
  
  scale_x_continuous(
    breaks = 2008:2025
  ) +
  
  scale_y_continuous(
    labels = comma,
    expand = expansion(mult = c(0.08, 0.14)),
    name = "Ingreso laboral por hora promedio",
    sec.axis = sec_axis(
      trans = ~ map_y_to_brecha(.),
      name = "Brecha de género (%)",
      labels = function(x) paste0(round(x, 1), "%")
    )
  ) +
  
  labs(
    title = "Evolución del ingreso por hora promedio por sexo, 2008–2025",
    subtitle = "Pesos constantes (2025)",
    x = ""
  ) +
  
  guides(
    color = guide_legend(
      override.aes = list(
        linewidth = c(1.1, 1.1, 1),
        linetype = c("solid", "solid", "dashed")
      )
    ),
    linetype = "none"
  ) +
  
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 90, hjust = 1),
    legend.position = "bottom",
    axis.title.y.right = element_text(face = "bold", color = "gray25"),
    axis.text.y.right = element_text(color = "gray25")
  )

g_ingreso_genero_secaxis


# Formalidad
geih <- read_dta(
  "Datos/Processed/GEIH_base_modelo_personas_2008_2025.dta"
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

g_ingreso_real_formalidad <- ggplot(
  serie_ingreso_formalidad,
  aes(
    x = anio,
    y = ingreso_hora_real_promedio,
    color = formalidad_grupo,
    group = formalidad_grupo
  )
) +
  geom_line(
    linewidth = 1.1
  ) +
  geom_point(
    size = 3
  ) +
  geom_text(
    aes(
      label = comma(ingreso_hora_real_promedio, accuracy = 1),
      vjust = ifelse(formalidad_grupo == "Formal", -0.8, 1.5)
    ),
    size = 3.2,
    show.legend = FALSE
  ) +
  scale_color_manual(
    values = c(
      "Informal" = "darkred",
      "Formal" = "darkblue"
    )
  ) +
  scale_x_continuous(
    breaks = 2008:2025
  ) +
  scale_y_continuous(
    labels = comma,
    expand = expansion(mult = c(0.10, 0.16))
  ) +
  labs(
    title = "Ingreso laboral por hora promedio real por formalidad en Colombia, 2008–2025",
    subtitle = "Valores expresados en pesos constantes de 2025. Promedio ponderado por factores de expansión",
    x = "Año",
    y = "Ingreso laboral por hora promedio, pesos de 2025",
    color = "Formalidad"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

g_ingreso_real_formalidad

# Nivel educativo
#========================================================
# 1. Cargar base de personas
#========================================================

geih <- read_dta(
  "Datos/Processed/GEIH_base_modelo_personas_2008_2025.dta"
) %>%
  mutate(
    anio = as.integer(anio),
    fex = as.numeric(fex),
    ingreso_hora_real = as.numeric(ingreso_hora_real),
    educacion = as.character(educacion)
  )

#========================================================
# 2. Serie anual por nivel educativo
#========================================================

serie_ingreso_educacion <- geih %>%
  filter(
    !is.na(anio),
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
    ingreso_hora_real_promedio = weighted.mean(
      ingreso_hora_real,
      fex,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  filter(!is.na(educacion)) %>%
  arrange(educacion, anio)

labels_educacion <- serie_ingreso_educacion %>%
  group_by(educacion) %>%
  filter(anio == max(anio, na.rm = TRUE)) %>%
  ungroup()

#========================================================
# 4. Gráfico
#========================================================

g_ingreso_real_educacion <- ggplot(
  serie_ingreso_educacion,
  aes(
    x = anio,
    y = ingreso_hora_real_promedio,
    color = educacion,
    group = educacion
  )
) +
  geom_line(
    linewidth = 1.1
  ) +
  geom_point(
    size = 2.6
  ) +
  geom_text(
    data = labels_educacion,
    aes(
      label = comma(ingreso_hora_real_promedio, accuracy = 1)
    ),
    hjust = -0.08,
    size = 3.2,
    fontface = "bold",
    show.legend = FALSE
  ) +
  scale_x_continuous(
    breaks = 2008:2025,
    expand = expansion(mult = c(0.02, 0.10))
  ) +
  scale_y_continuous(
    labels = comma,
    expand = expansion(mult = c(0.08, 0.15))
  ) +
  labs(
    title = "Ingreso laboral por hora promedio real por nivel educativo en Colombia, 2008–2025",
    subtitle = "Valores expresados en pesos constantes de 2025. Promedio ponderado por factores de expansión",
    x = "Año",
    y = "Ingreso laboral por hora promedio, pesos de 2025",
    color = "Nivel educativo"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

g_ingreso_real_educacion

# Ocupacion
geih <- read_dta(
  "Datos/Processed/GEIH_base_modelo_personas_2008_2025.dta"
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
      ocupacion == "Trabajador familiar sin remuneración" ~ "Familiar sin remun.",
      ocupacion == "Trabajador sin remuneración en otros hogares" ~ "Sin remun. otros hogares",
      ocupacion == "Jornalero o peón" ~ "Jornalero/peón",
      ocupacion == "Otro" ~ "Otro",
      TRUE ~ ocupacion
    ),
    ocupacion_label = factor(
      ocupacion_label,
      levels = c(
        "Empleado particular",
        "Empleado gobierno",
        "Servicio doméstico",
        "Cuenta propia",
        "Patrón/empleador",
        "Familiar sin remun.",
        "Sin remun. otros hogares",
        "Jornalero/peón",
        "Otro"
      )
    )
  )

#========================================================
# 3. Serie anual por ocupación
#========================================================

serie_ingreso_ocupacion <- geih %>%
  filter(
    !is.na(anio),
    !is.na(ocupacion_label),
    !is.na(ingreso_hora_real),
    ingreso_hora_real > 0,
    !is.na(fex),
    fex > 0
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
  ) %>%
  arrange(ocupacion_label, anio)

serie_ingreso_ocupacion

labels_ocupacion <- serie_ingreso_ocupacion %>%
  filter(anio %in% c(2008, 2025))

g_ingreso_real_ocupacion <- ggplot(
  serie_ingreso_ocupacion,
  aes(
    x = anio,
    y = ingreso_hora_real_promedio
  )
) +
  geom_line(
    color = "darkblue",
    linewidth = 1
  ) +
  geom_point(
    color = "darkblue",
    size = 2.5
  ) +
  geom_text(
    data = labels_ocupacion,
    aes(
      label = comma(ingreso_hora_real_promedio, accuracy = 1)
    ),
    vjust = -0.8,
    size = 3,
    fontface = "bold",
    show.legend = FALSE
  ) +
  facet_wrap(
    ~ ocupacion_label,
    scales = "free_y",
    ncol = 3
  ) +
  scale_x_continuous(
    breaks = 2008:2025
  ) +
  scale_y_continuous(
    labels = comma,
    expand = expansion(mult = c(0.10, 0.20))
  ) +
  labs(
    title = "Ingreso laboral por hora promedio real por posición ocupacional en Colombia, 2008–2025",
    subtitle = "Valores expresados en pesos constantes de 2025. Promedio ponderado por factores de expansión",
    x = "Año",
    y = "Ingreso laboral por hora promedio, pesos de 2025"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

g_ingreso_real_ocupacion

# Por Departamento
geih <- read_dta(
  "Datos/Processed/GEIH_base_modelo_personas_2008_2025.dta"
) %>%
  mutate(
    anio = as.integer(anio),
    fex = as.numeric(fex),
    ingreso_hora_real = as.numeric(ingreso_hora_real),
    depto = as.numeric(depto)
  )

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

serie_ingreso_depto <- geih %>%
  filter(
    !is.na(anio),
    !is.na(depto_label),
    !is.na(ingreso_hora_real),
    ingreso_hora_real > 0,
    !is.na(fex),
    fex > 0
  ) %>%
  group_by(anio, depto, depto_label) %>%
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
  arrange(depto, anio)

anios_muestra <- c(2008:2019, 2021:2025)

deptos_completos <- serie_ingreso_depto %>%
  group_by(depto, depto_label) %>%
  summarise(
    n_anios = n_distinct(anio),
    .groups = "drop"
  ) %>%
  filter(n_anios == length(anios_muestra))

deptos_completos

serie_ingreso_depto_completa <- serie_ingreso_depto %>%
  semi_join(
    deptos_completos,
    by = c("depto", "depto_label")
  ) %>%
  arrange(depto, anio)

labels_depto_2025 <- serie_ingreso_depto_completa %>%
  filter(anio == 2025)

g_ingreso_real_depto <- ggplot(
  serie_ingreso_depto_completa,
  aes(
    x = anio,
    y = ingreso_hora_real_promedio,
    color = depto_label,
    group = depto_label
  )
) +
  geom_line(
    linewidth = 0.9,
    alpha = 0.85
  ) +
  geom_point(
    size = 2,
    alpha = 0.85
  ) +
  geom_text(
    data = labels_depto_2025,
    aes(
      x = anio + 0.25,
      label = depto_label
    ),
    hjust = 0,
    size = 2.8,
    fontface = "bold",
    show.legend = FALSE
  ) +
  scale_x_continuous(
    breaks = c(2008, 2012, 2016, 2019, 2021, 2025),
    limits = c(2008, 2027)
  ) +
  scale_y_continuous(
    labels = comma,
    expand = expansion(mult = c(0.08, 0.12))
  ) +
  labs(
    title = "Ingreso laboral por hora promedio real por departamento en Colombia, 2008–2025",
    subtitle = "Solo departamentos con información en todos los años disponibles. Promedio ponderado por factores de expansión",
    x = "Año",
    y = "Ingreso laboral por hora promedio, pesos de 2025",
    color = "Departamento"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none",
    panel.grid.minor = element_blank()
  )

g_ingreso_real_depto

# Ciudades
geih <- read_dta(
  "Datos/Processed/GEIH_base_modelo_personas_2008_2025.dta"
) %>%
  mutate(
    anio = as.integer(anio),
    fex = as.numeric(fex),
    ingreso_hora_real = as.numeric(ingreso_hora_real),
    area = as.numeric(area)
  )

#========================================================
# 2. Labels de AREA
#========================================================

geih <- geih %>%
  mutate(
    area_label = case_when(
      area == 5  ~ "Medellín A.M.",
      area == 8  ~ "Barranquilla A.M.",
      area == 11 ~ "Bogotá D.C.",
      area == 13 ~ "Cartagena",
      area == 17 ~ "Manizales A.M.",
      area == 23 ~ "Montería",
      area == 50 ~ "Villavicencio",
      area == 52 ~ "Pasto",
      area == 54 ~ "Cúcuta A.M.",
      area == 66 ~ "Pereira A.M.",
      area == 68 ~ "Bucaramanga A.M.",
      area == 73 ~ "Ibagué",
      area == 76 ~ "Cali A.M.",
      TRUE ~ paste0("Área ", area)
    )
  )

#========================================================
# 3. Serie anual por AREA
#========================================================

serie_ingreso_area <- geih %>%
  filter(
    !is.na(anio),
    !is.na(area),
    !is.na(area_label),
    !is.na(ingreso_hora_real),
    ingreso_hora_real > 0,
    !is.na(fex),
    fex > 0
  ) %>%
  group_by(anio, area, area_label) %>%
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
  arrange(area, anio)

#========================================================
# 4. Quedarse solo con áreas completas
#========================================================

anios_muestra <- c(2008:2019, 2021:2025)

areas_completas <- serie_ingreso_area %>%
  group_by(area, area_label) %>%
  summarise(
    n_anios = n_distinct(anio),
    .groups = "drop"
  ) %>%
  filter(n_anios == length(anios_muestra))

serie_ingreso_area_completa <- serie_ingreso_area %>%
  semi_join(
    areas_completas,
    by = c("area", "area_label")
  ) %>%
  arrange(area, anio)

labels_area_2025 <- serie_ingreso_area_completa %>%
  filter(anio == 2025)

g_ingreso_real_area <- ggplot(
  serie_ingreso_area_completa,
  aes(
    x = anio,
    y = ingreso_hora_real_promedio,
    color = area_label,
    group = area_label
  )
) +
  geom_line(
    linewidth = 0.95,
    alpha = 0.85
  ) +
  geom_point(
    size = 2,
    alpha = 0.85
  ) +
  geom_text(
    data = labels_area_2025,
    aes(
      x = anio + 0.25,
      label = area_label
    ),
    hjust = 0,
    size = 3,
    fontface = "bold",
    show.legend = FALSE
  ) +
  scale_x_continuous(
    breaks = c(2008, 2012, 2016, 2019, 2021, 2025),
    limits = c(2008, 2027)
  ) +
  scale_y_continuous(
    labels = comma,
    expand = expansion(mult = c(0.08, 0.12))
  ) +
  labs(
    title = "Ingreso laboral por hora promedio real por ciudad principal, 2008–2025",
    subtitle = "Solo áreas con información en todos los años disponibles. Promedio ponderado por factores de expansión",
    x = "Año",
    y = "Ingreso laboral por hora promedio, pesos de 2025",
    color = "Ciudad / área"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none",
    panel.grid.minor = element_blank()
  )

g_ingreso_real_area

# Por sector
geih <- read_dta(
  "Datos/Processed/GEIH_base_modelo_personas_2008_2025.dta"
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
# 3. Serie anual por sector
#========================================================

serie_ingreso_sector <- geih %>%
  filter(
    !is.na(anio),
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
  ) %>%
  arrange(sector_label, anio)

#========================================================
# 4. Quedarse solo con sectores completos
#========================================================

anios_muestra <- c(2008:2019, 2021:2025)

sectores_completos <- serie_ingreso_sector %>%
  group_by(sector_label) %>%
  summarise(
    n_anios = n_distinct(anio),
    .groups = "drop"
  ) %>%
  filter(n_anios == length(anios_muestra))

serie_ingreso_sector_completa <- serie_ingreso_sector %>%
  semi_join(
    sectores_completos,
    by = "sector_label"
  ) %>%
  arrange(sector_label, anio)

#========================================================
# 1. Clasificar sectores en altos y bajos
#    usando el ingreso de 2025
#========================================================

corte_2025 <- serie_ingreso_sector_completa %>%
  filter(anio == 2025) %>%
  summarise(
    mediana_2025 = median(ingreso_hora_real_promedio, na.rm = TRUE)
  ) %>%
  pull(mediana_2025)

sector_grupos <- serie_ingreso_sector_completa %>%
  filter(anio == 2025) %>%
  mutate(
    grupo_ingreso = if_else(
      ingreso_hora_real_promedio >= corte_2025,
      "Ingresos altos",
      "Ingresos bajos"
    )
  ) %>%
  select(sector_label, grupo_ingreso)

serie_ingreso_sector_grupos <- serie_ingreso_sector_completa %>%
  left_join(sector_grupos, by = "sector_label")

labels_sector_2025 <- serie_ingreso_sector_grupos %>%
  filter(anio == 2025)

g_ingreso_real_sector_grupos <- ggplot(
  serie_ingreso_sector_grupos,
  aes(
    x = anio,
    y = ingreso_hora_real_promedio,
    color = sector_label,
    group = sector_label
  )
) +
  geom_line(
    linewidth = 0.95,
    alpha = 0.85
  ) +
  geom_point(
    size = 2,
    alpha = 0.85
  ) +
  geom_text(
    data = labels_sector_2025,
    aes(
      x = anio + 0.25,
      label = sector_label
    ),
    hjust = 0,
    size = 3,
    fontface = "bold",
    show.legend = FALSE
  ) +
  facet_wrap(
    ~ grupo_ingreso,
    scales = "free_y",
    ncol = 1
  ) +
  scale_x_continuous(
    breaks = c(2008, 2012, 2016, 2019, 2021, 2025),
    limits = c(2008, 2027.5)
  ) +
  scale_y_continuous(
    labels = comma,
    expand = expansion(mult = c(0.08, 0.12))
  ) +
  labs(
    title = "Ingreso laboral por hora promedio real por sector económico, 2008–2025",
    subtitle = "Sectores agrupados según su nivel de ingreso en 2025. Promedio ponderado por factores de expansión",
    x = "Año",
    y = "Ingreso laboral por hora promedio, pesos de 2025",
    color = "Sector"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none",
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", size = 12)
  )

g_ingreso_real_sector_grupos

# Tamanio
geih <- read_dta(
  "Datos/Processed/GEIH_base_modelo_personas_2008_2025.dta"
) %>%
  mutate(
    anio = as.integer(anio),
    fex = as.numeric(fex),
    ingreso_hora_real = as.numeric(ingreso_hora_real),
    tamano_empresa = as.character(tamano_empresa)
  )

#========================================================
# 2. Ordenar tamaño de empresa
#========================================================

orden_tamano <- c(
  "Solo", "2-3", "4-5", "6-10", "11-19",
  "20-30", "31-50", "51-100", "101+"
)

#========================================================
# 3. Calcular promedio ponderado por año y tamaño
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
    )
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
  mutate(
    anio = factor(anio, levels = c(2008, 2025))
  ) %>%
  arrange(anio, tamano_empresa)

g_ingreso_real_tamano_2008_2025 <- ggplot(
  serie_tamano_2008_2025,
  aes(
    x = tamano_empresa,
    y = ingreso_hora_real_promedio,
    color = anio,
    group = anio
  )
) +
  geom_line(
    linewidth = 1.15,
    alpha = 0.9
  ) +
  geom_point(
    size = 3.5,
    alpha = 0.9
  ) +
  geom_label(
    aes(
      label = comma(ingreso_hora_real_promedio, accuracy = 1)
    ),
    fill = "black",
    color = "white",
    fontface = "bold",
    size = 3.4,
    label.size = 0.15,
    vjust = -0.8,
    show.legend = FALSE
  ) +
  scale_color_manual(
    values = c(
      "2008" = "darkred",
      "2025" = "darkblue"
    )
  ) +
  scale_y_continuous(
    labels = comma,
    expand = expansion(mult = c(0.10, 0.20))
  ) +
  labs(
    title = "Ingreso laboral por hora real según tamaño de empresa",
    subtitle = "Comparación 2008 vs. 2025. Promedio ponderado por factores de expansión",
    x = "Tamaño de empresa",
    y = "Ingreso laboral por hora promedio, pesos de 2025",
    color = "Año"
  ) +
  theme_classic(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  )

g_ingreso_real_tamano_2008_2025

# Por sexo y tamanio
geih <- read_dta(
  "Datos/Processed/GEIH_base_modelo_personas_2008_2025.dta"
) %>%
  mutate(
    anio = as.integer(anio),
    fex = as.numeric(fex),
    ingreso_hora_real = as.numeric(ingreso_hora_real),
    tamano_empresa = as.character(tamano_empresa),
    sexo = as.character(sexo)
  )

#========================================================
# 2. Ordenar tamaño de empresa y género
#========================================================

orden_tamano <- c(
  "Solo", "2-3", "4-5", "6-10", "11-19",
  "20-30", "31-50", "51-100", "101+"
)

#========================================================
# 3. Calcular promedio ponderado por año, género y tamaño
#========================================================

serie_tamano_genero_2008_2025 <- geih %>%
  filter(
    anio %in% c(2008, 2025),
    !is.na(sexo),
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
    sexo = factor(
      sexo,
      levels = c("Hombre", "Mujer")
    )
  ) %>%
  filter(
    !is.na(tamano_empresa),
    !is.na(sexo)
  ) %>%
  group_by(anio, sexo, tamano_empresa) %>%
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
  mutate(
    anio = factor(anio, levels = c(2008, 2025))
  ) %>%
  arrange(sexo, anio, tamano_empresa)

g_ingreso_real_tamano_genero_2008_2025 <- ggplot(
  serie_tamano_genero_2008_2025,
  aes(
    x = tamano_empresa,
    y = ingreso_hora_real_promedio,
    color = anio,
    group = anio
  )
) +
  geom_line(
    linewidth = 1.15,
    alpha = 0.9
  ) +
  geom_point(
    size = 3.5,
    alpha = 0.9
  ) +
  geom_label(
    aes(
      label = comma(ingreso_hora_real_promedio, accuracy = 1)
    ),
    fill = "black",
    color = "white",
    fontface = "bold",
    size = 3.2,
    label.size = 0.15,
    vjust = -0.8,
    show.legend = FALSE
  ) +
  facet_wrap(
    ~ sexo,
    scales = "free_y",
    ncol = 2
  ) +
  scale_color_manual(
    values = c(
      "2008" = "darkred",
      "2025" = "darkblue"
    )
  ) +
  scale_y_continuous(
    labels = comma,
    expand = expansion(mult = c(0.10, 0.22))
  ) +
  labs(
    title = "Ingreso laboral por hora real según tamaño de empresa y género",
    subtitle = "Comparación 2008 vs. 2025. Promedio ponderado por factores de expansión",
    x = "Tamaño de empresa",
    y = "Ingreso laboral por hora promedio, pesos de 2025",
    color = "Año"
  ) +
  theme_classic(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold", size = 13),
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  )

g_ingreso_real_tamano_genero_2008_2025

# FOrmalidad
geih <- read_dta(
  "Datos/Processed/GEIH_base_modelo_personas_2008_2025.dta"
) %>%
  mutate(
    anio = as.integer(anio),
    fex = as.numeric(fex),
    ingreso_hora_real = as.numeric(ingreso_hora_real),
    tamano_empresa = as.character(tamano_empresa),
    formalidad = as.character(formalidad)
  )

#========================================================
# 2. Ordenar tamaño de empresa
#========================================================

orden_tamano <- c(
  "Solo", "2-3", "4-5", "6-10", "11-19",
  "20-30", "31-50", "51-100", "101+"
)

#========================================================
# 3. Calcular promedio ponderado por año, formalidad y tamaño
#========================================================

serie_tamano_formalidad_2008_2025 <- geih %>%
  filter(
    anio %in% c(2008, 2025),
    formalidad %in% c("Formal", "Informal"),
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
    formalidad = factor(
      formalidad,
      levels = c("Informal", "Formal")
    )
  ) %>%
  filter(
    !is.na(tamano_empresa),
    !is.na(formalidad)
  ) %>%
  group_by(anio, formalidad, tamano_empresa) %>%
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
  mutate(
    anio = factor(anio, levels = c(2008, 2025))
  ) %>%
  arrange(formalidad, anio, tamano_empresa)

g_ingreso_real_tamano_formalidad_2008_2025 <- ggplot(
  serie_tamano_formalidad_2008_2025,
  aes(
    x = tamano_empresa,
    y = ingreso_hora_real_promedio,
    color = anio,
    group = anio
  )
) +
  geom_line(
    linewidth = 1.15,
    alpha = 0.9
  ) +
  geom_point(
    size = 3.5,
    alpha = 0.9
  ) +
  geom_label(
    aes(
      label = comma(ingreso_hora_real_promedio, accuracy = 1)
    ),
    fill = "black",
    color = "white",
    fontface = "bold",
    size = 3.2,
    label.size = 0.15,
    vjust = -0.8,
    show.legend = FALSE
  ) +
  facet_wrap(
    ~ formalidad,
    scales = "free_y",
    ncol = 2
  ) +
  scale_color_manual(
    values = c(
      "2008" = "darkred",
      "2025" = "darkblue"
    )
  ) +
  scale_y_continuous(
    labels = comma,
    expand = expansion(mult = c(0.10, 0.22))
  ) +
  labs(
    title = "Ingreso laboral por hora real según tamaño de empresa y formalidad",
    subtitle = "Comparación 2008 vs. 2025. Promedio ponderado por factores de expansión",
    x = "Tamaño de empresa",
    y = "Ingreso laboral por hora promedio, pesos de 2025",
    color = "Año"
  ) +
  theme_classic(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold", size = 13),
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  )

g_ingreso_real_tamano_formalidad_2008_2025

# Serie de informalidad
geih <- read_dta(
  "Datos/Processed/GEIH_base_modelo_personas_2008_2025.dta"
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
