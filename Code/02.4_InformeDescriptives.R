setwd("~/Trabajo-Profesional/Javeriana")

#========================================================
# INFORME DESCRIPTIVO GEIH 2008-2025
# Versión organizada por gráfico:
# 1) preparar base agregada del gráfico
# 2) construir gráfico
# 3) limpiar objetos intermedios
#
# Nota:
# - La base individual se carga una sola vez.
# - No incluye ggsave.
# - La estética de los gráficos se mantiene.
#========================================================

library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(ggrepel)
library(grid)

options(scipen = 999)

#========================================================
# 0. Parámetros generales
#========================================================

path_geih <- "Datos/Processed/Paper-GEIH_base_modelo_personas_2008_2025.dta"

anios_muestra <- c(2010:2019, 2021:2025)

cols_necesarias <- c(
  "anio",
  "fex",
  "horas",
  "ingreso_hora_real",
  "sexo",
  "formal",
  "educacion",
  "ocupacion",
  "depto",
  "sector",
  "tamano_empresa",
  "rama4d",
  "rama4d_clase",
  "rama3d",
  "rama4d_div",
  "ciiu_revision_rama4d",
  "subrama_det_cod",
  "subrama_det"
)

#========================================================
# 1. Funciones auxiliares
#========================================================

weighted_mean <- function(x, w) {
  ok <- !is.na(x) & !is.na(w) & w > 0
  
  if (!any(ok)) {
    return(NA_real_)
  }
  
  weighted.mean(x[ok], w[ok])
}

read_geih_light <- function(path, cols) {
  tryCatch(
    read_dta(
      path,
      col_select = any_of(cols)
    ),
    error = function(e) {
      message("Tu versión de haven no soportó col_select. Se lee completa y luego se selecciona.")
      read_dta(path) %>%
        select(any_of(cols))
    }
  )
}

calc_crecimiento_anualizado <- function(data, group_var, value_var, anio_inicio = 2010, anio_final = 2025) {
  data %>%
    filter(anio %in% c(anio_inicio, anio_final)) %>%
    select(anio, {{ group_var }}, {{ value_var }}) %>%
    pivot_wider(
      names_from = anio,
      values_from = {{ value_var }},
      names_prefix = "y_"
    ) %>%
    mutate(
      n_anios = anio_final - anio_inicio,
      crecimiento_anualizado = 100 * ((.data[[paste0("y_", anio_final)]] / .data[[paste0("y_", anio_inicio)]])^(1 / n_anios) - 1),
      label_crecimiento = paste0(
        "Crecimiento anualizado ",
        anio_inicio,
        "–",
        anio_final,
        ": ",
        ifelse(crecimiento_anualizado >= 0, "+", ""),
        round(crecimiento_anualizado, 2),
        "% anual"
      )
    )
}

#========================================================
# 2. Cargar base una sola vez y crear etiquetas comunes
#========================================================

geih <- read_geih_light(path_geih, cols_necesarias) %>%
  transmute(
    anio = as.integer(anio),
    fex = as.numeric(fex),
    horas = as.numeric(horas),
    ingreso_hora_real = as.numeric(ingreso_hora_real),
    sexo = as.character(sexo),
    formal = as.numeric(formal),
    educacion = as.character(educacion),
    ocupacion = as.character(ocupacion),
    depto = as.numeric(depto),
    sector = as.character(sector),
    tamano_empresa = as.character(tamano_empresa),
    rama4d = as.numeric(rama4d),
    rama4d_clase = as.character(rama4d_clase),
    rama3d = as.numeric(rama3d),
    rama4d_div = as.numeric(rama4d_div),
    ciiu_revision_rama4d = as.character(ciiu_revision_rama4d),
    subrama_det_cod = as.numeric(subrama_det_cod),
    subrama_det = as.character(subrama_det)
  ) %>%
  filter(
    anio %in% anios_muestra
  ) %>%
  mutate(
    formalidad_grupo = case_when(
      formal == 1 ~ "Formal",
      formal == 0 ~ "Informal",
      TRUE ~ NA_character_
    ),
    formalidad_grupo = factor(
      formalidad_grupo,
      levels = c("Informal", "Formal")
    ),
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
    ),
    subrama_det_label = case_when(
      subrama_det == "Información y comunicaciones" ~ "Información y comunicaciones",
      TRUE ~ subrama_det
    ),
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

# Base liviana para gráficos de ingreso.
geih_ingreso <- geih %>%
  filter(
    !is.na(anio),
    !is.na(ingreso_hora_real),
    ingreso_hora_real > 0,
    !is.na(fex),
    fex > 0
  )

#========================================================
# GRÁFICO 1. Ingreso laboral por hora promedio
#========================================================

#--------------------------------------------------------
# 1.1. Preparar datos
#--------------------------------------------------------

serie_ingreso_real <- geih_ingreso %>%
  group_by(anio) %>%
  summarise(
    observaciones = n(),
    trabajadores_expandidos = sum(fex, na.rm = TRUE),
    ingreso_hora_real_promedio = weighted_mean(ingreso_hora_real, fex),
    .groups = "drop"
  ) %>%
  arrange(anio)

serie_plot <- serie_ingreso_real %>%
  arrange(anio)

anio_inicio <- min(serie_plot$anio, na.rm = TRUE)
anio_final <- max(serie_plot$anio, na.rm = TRUE)

ingreso_inicio <- serie_plot %>%
  filter(anio == anio_inicio) %>%
  pull(ingreso_hora_real_promedio)

ingreso_final <- serie_plot %>%
  filter(anio == anio_final) %>%
  pull(ingreso_hora_real_promedio)

crecimiento_anualizado <- 100 * ((ingreso_final / ingreso_inicio)^(1 / (anio_final - anio_inicio)) - 1)

label_variacion_total <- paste0(
  "Crecimiento anualizado ",
  anio_inicio,
  "–",
  anio_final,
  ": ",
  ifelse(crecimiento_anualizado >= 0, "+", ""),
  round(crecimiento_anualizado, 2),
  "% anual"
)

max_y <- max(serie_plot$ingreso_hora_real_promedio, na.rm = TRUE)
min_y <- min(serie_plot$ingreso_hora_real_promedio, na.rm = TRUE)
rango_y <- max_y - min_y

y_variacion <- max_y + 0.12 * rango_y
x_label_variacion <- anio_inicio + 0.5 * (anio_final - anio_inicio)

#--------------------------------------------------------
# 1.2. Graficar
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
      x = anio_inicio,
      xend = anio_final,
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
      x = x_label_variacion,
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
    title = paste0("Evolución del ingreso laboral por hora promedio, ", anio_inicio, "–", anio_final),
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

#========================================================
# GRÁFICO 2. Ingreso por sexo en niveles
#========================================================

#--------------------------------------------------------
# 2.1. Preparar datos
#--------------------------------------------------------

serie_genero <- geih_ingreso %>%
  filter(!is.na(sexo)) %>%
  mutate(
    sexo = factor(sexo, levels = c("Hombre", "Mujer"))
  ) %>%
  filter(!is.na(sexo)) %>%
  group_by(anio, sexo) %>%
  summarise(
    observaciones = n(),
    trabajadores_expandidos = sum(fex, na.rm = TRUE),
    ingreso_hora_real_promedio = weighted_mean(ingreso_hora_real, fex),
    .groups = "drop"
  ) %>%
  arrange(sexo, anio)

anio_inicio <- min(serie_genero$anio, na.rm = TRUE)
anio_final  <- max(serie_genero$anio, na.rm = TRUE)
x_label_crecimiento <- anio_inicio + 0.5 * (anio_final - anio_inicio)

max_y <- max(serie_genero$ingreso_hora_real_promedio, na.rm = TRUE)
min_y <- min(serie_genero$ingreso_hora_real_promedio, na.rm = TRUE)
rango_y <- max_y - min_y

offset_label <- 0.055 * rango_y

#--------------------------------------------------------
# 2.2. Etiquetas inicio y final
#--------------------------------------------------------

labels_inicio_fin <- serie_genero %>%
  filter(anio %in% c(anio_inicio, anio_final)) %>%
  mutate(
    x_label = case_when(
      anio == anio_inicio ~ anio - 0.18,
      anio == anio_final  ~ anio + 0.18
    ),
    y_label = case_when(
      anio == anio_inicio & sexo == "Hombre" ~ ingreso_hora_real_promedio + offset_label,
      anio == anio_inicio & sexo == "Mujer"  ~ ingreso_hora_real_promedio - offset_label,
      
      anio == anio_final & sexo == "Mujer"  ~ ingreso_hora_real_promedio + offset_label,
      anio == anio_final & sexo == "Hombre" ~ ingreso_hora_real_promedio - offset_label,
      
      TRUE ~ ingreso_hora_real_promedio
    )
  )

#--------------------------------------------------------
# 2.3. Crecimiento anualizado por sexo
#--------------------------------------------------------

crecimiento_genero <- serie_genero %>%
  filter(anio %in% c(anio_inicio, anio_final)) %>%
  select(anio, sexo, ingreso_hora_real_promedio) %>%
  pivot_wider(
    names_from = anio,
    values_from = ingreso_hora_real_promedio,
    names_prefix = "y_"
  ) %>%
  mutate(
    n_anios = anio_final - anio_inicio,
    ingreso_inicio = .data[[paste0("y_", anio_inicio)]],
    ingreso_final  = .data[[paste0("y_", anio_final)]],
    crecimiento_anualizado = 100 * ((ingreso_final / ingreso_inicio)^(1 / n_anios) - 1),
    label_crecimiento = paste0(
      "Crecimiento anualizado ",
      anio_inicio,
      "–",
      anio_final,
      ": ",
      ifelse(crecimiento_anualizado >= 0, "+", ""),
      round(crecimiento_anualizado, 2),
      "% anual"
    )
  )

pos_crecimiento_genero <- crecimiento_genero %>%
  arrange(desc(crecimiento_anualizado)) %>%
  mutate(
    orden_crecimiento = row_number(),
    x_inicio = anio_inicio,
    x_fin = anio_final,
    x_label = x_label_crecimiento,
    y_arrow = case_when(
      orden_crecimiento == 1 ~ max_y + 0.22 * rango_y,  # mayor crecimiento arriba
      orden_crecimiento == 2 ~ max_y + 0.11 * rango_y   # menor crecimiento abajo
    )
  )
#========================================================
# 2.4. Graficar
#========================================================

g_ingreso_sexo_ajustado <- ggplot(
  serie_genero,
  aes(
    x = anio,
    y = ingreso_hora_real_promedio,
    color = sexo,
    group = sexo
  )
) +
  geom_line(
    linewidth = 1.2
  ) +
  geom_point(
    size = 3.2
  ) +
  
  # Flechas con crecimiento anualizado
  geom_segment(
    data = pos_crecimiento_genero,
    aes(
      x = x_inicio,
      xend = x_fin,
      y = y_arrow,
      yend = y_arrow,
      color = sexo
    ),
    inherit.aes = FALSE,
    linewidth = 0.75,
    alpha = 0.95,
    arrow = arrow(
      length = unit(0.18, "cm"),
      type = "closed"
    ),
    show.legend = FALSE
  ) +
  
  geom_label(
    data = pos_crecimiento_genero,
    aes(
      x = x_label,
      y = y_arrow,
      label = label_crecimiento,
      fill = sexo
    ),
    inherit.aes = FALSE,
    color = "white",
    fontface = "bold",
    size = 3.3,
    label.size = 0.15,
    label.padding = unit(0.16, "lines"),
    show.legend = FALSE
  ) +
  
  # Etiquetas de ingreso en inicio y final
  geom_label(
    data = labels_inicio_fin,
    aes(
      x = x_label,
      y = y_label,
      label = comma(ingreso_hora_real_promedio, accuracy = 1),
      fill = sexo
    ),
    color = "white",
    fontface = "bold",
    size = 3.5,
    label.size = 0.15,
    label.padding = unit(0.14, "lines"),
    show.legend = FALSE
  ) +
  
  scale_color_manual(
    values = c(
      "Hombre" = "#8C1C13",
      "Mujer"  = "#1D4E89"
    )
  ) +
  scale_fill_manual(
    values = c(
      "Hombre" = "#8C1C13",
      "Mujer"  = "#1D4E89"
    )
  ) +
  scale_x_continuous(
    breaks = sort(unique(serie_genero$anio)),
    limits = c(anio_inicio - 0.5, anio_final + 0.8)
  ) +
  scale_y_continuous(
    labels = comma,
    expand = expansion(mult = c(0.08, 0.22))
  ) +
  coord_cartesian(clip = "off") +
  labs(
    title = paste0(
      "Evolución del ingreso laboral por hora promedio por sexo, ",
      anio_inicio,
      "–",
      anio_final
    ),
    subtitle = "Pesos constantes de 2025. Promedio ponderado por factores de expansión",
    x = "Año",
    y = "Ingreso laboral por hora promedio",
    color = NULL
  ) +
  theme_classic(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    legend.position = "bottom",
    legend.text = element_text(face = "bold"),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.35),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 95, 10, 10)
  )

g_ingreso_sexo_ajustado
#========================================================
# GRÁFICO 3. Composición porcentual de trabajadores por sexo
#========================================================

#--------------------------------------------------------
# 3.1. Preparar datos
#--------------------------------------------------------

serie_comp_sexo <- geih %>%
  filter(
    !is.na(anio),
    !is.na(sexo),
    !is.na(fex),
    fex > 0
  ) %>%
  mutate(
    sexo = factor(sexo, levels = c("Hombre", "Mujer"))
  ) %>%
  filter(!is.na(sexo)) %>%
  group_by(anio, sexo) %>%
  summarise(
    observaciones = n(),
    trabajadores_expandidos = sum(fex, na.rm = TRUE),
    trabajadores_millones = trabajadores_expandidos / 1e6,
    .groups = "drop"
  ) %>%
  group_by(anio) %>%
  mutate(
    total_trabajadores = sum(trabajadores_millones, na.rm = TRUE),
    participacion = trabajadores_millones / total_trabajadores
  ) %>%
  ungroup() %>%
  arrange(anio, sexo)

serie_comp_sexo_wide <- serie_comp_sexo %>%
  select(anio, sexo, participacion) %>%
  pivot_wider(
    names_from = sexo,
    values_from = participacion,
    values_fill = 0
  ) %>%
  mutate(
    share_mujer = Mujer,
    share_hombre = Hombre,
    ymin_mujer = 0,
    ymax_mujer = share_mujer,
    ymin_hombre = share_mujer,
    ymax_hombre = 1
  ) %>%
  arrange(anio)

anio_inicio <- min(serie_comp_sexo_wide$anio, na.rm = TRUE)
anio_final  <- max(serie_comp_sexo_wide$anio, na.rm = TRUE)

#--------------------------------------------------------
# 3.2. Etiquetas de inicio
# Solo porcentaje, sin repetir nombre del grupo
#--------------------------------------------------------

labels_inicio_sexo <- bind_rows(
  
  serie_comp_sexo_wide %>%
    filter(anio == anio_inicio) %>%
    transmute(
      anio = anio_inicio + 0.25,
      sexo = "Mujer",
      y = share_mujer / 2,
      label = percent(share_mujer, accuracy = 0.1)
    ),
  
  serie_comp_sexo_wide %>%
    filter(anio == anio_inicio) %>%
    transmute(
      anio = anio_inicio + 0.25,
      sexo = "Hombre",
      y = share_mujer + share_hombre / 2,
      label = percent(share_hombre, accuracy = 0.1)
    )
)

#--------------------------------------------------------
# 3.3. Etiquetas finales
# Nombre + porcentaje
#--------------------------------------------------------

labels_final_sexo <- bind_rows(
  
  serie_comp_sexo_wide %>%
    filter(anio == anio_final) %>%
    transmute(
      anio = anio_final + 0.35,
      sexo = "Mujer",
      y = share_mujer / 2,
      label = paste0("Mujer: ", percent(share_mujer, accuracy = 0.1))
    ),
  
  serie_comp_sexo_wide %>%
    filter(anio == anio_final) %>%
    transmute(
      anio = anio_final + 0.35,
      sexo = "Hombre",
      y = share_mujer + share_hombre / 2,
      label = paste0("Hombre: ", percent(share_hombre, accuracy = 0.1))
    )
)

#--------------------------------------------------------
# 3.4. Cambio en puntos porcentuales por categoría
#--------------------------------------------------------

cambio_comp_sexo <- bind_rows(
  
  serie_comp_sexo_wide %>%
    filter(anio %in% c(anio_inicio, anio_final)) %>%
    transmute(
      anio,
      sexo = "Mujer",
      participacion = share_mujer
    ),
  
  serie_comp_sexo_wide %>%
    filter(anio %in% c(anio_inicio, anio_final)) %>%
    transmute(
      anio,
      sexo = "Hombre",
      participacion = share_hombre
    )
) %>%
  pivot_wider(
    names_from = anio,
    values_from = participacion,
    names_prefix = "y_"
  ) %>%
  mutate(
    participacion_inicio = .data[[paste0("y_", anio_inicio)]],
    participacion_final  = .data[[paste0("y_", anio_final)]],
    cambio_pp = 100 * (participacion_final - participacion_inicio),
    label_cambio = paste0(
      ifelse(cambio_pp >= 0, "+", ""),
      round(cambio_pp, 1),
      " p.p."
    )
  )

anio_label_cambio <- serie_comp_sexo_wide$anio[
  which.min(abs(
    serie_comp_sexo_wide$anio -
      (anio_inicio + 0.58 * (anio_final - anio_inicio))
  ))
]

labels_cambio_sexo <- bind_rows(
  
  serie_comp_sexo_wide %>%
    filter(anio == anio_label_cambio) %>%
    transmute(
      anio,
      sexo = "Mujer",
      y = share_mujer / 2
    ),
  
  serie_comp_sexo_wide %>%
    filter(anio == anio_label_cambio) %>%
    transmute(
      anio,
      sexo = "Hombre",
      y = share_mujer + share_hombre / 2
    )
) %>%
  left_join(
    cambio_comp_sexo %>%
      select(sexo, label_cambio),
    by = "sexo"
  ) %>%
  mutate(
    x_label = anio_label_cambio,
    label = label_cambio
  )

#--------------------------------------------------------
# 3.5. Puntos sobre la frontera
#--------------------------------------------------------

puntos_frontera <- serie_comp_sexo_wide %>%
  filter(anio %in% c(anio_inicio, anio_final))

#--------------------------------------------------------
# 3.6. Graficar
#--------------------------------------------------------

g_composicion_sexo_area <- ggplot(
  serie_comp_sexo_wide,
  aes(x = anio)
) +
  geom_ribbon(
    aes(
      ymin = ymin_mujer,
      ymax = ymax_mujer,
      fill = "Mujer"
    ),
    alpha = 0.96,
    linewidth = 0
  ) +
  geom_ribbon(
    aes(
      ymin = ymin_hombre,
      ymax = ymax_hombre,
      fill = "Hombre"
    ),
    alpha = 0.96,
    linewidth = 0
  ) +
  
  # Línea divisoria: muestra el cambio en la composición
  geom_line(
    aes(y = share_mujer),
    color = "white",
    linewidth = 1.15,
    alpha = 0.95
  ) +
  geom_line(
    aes(y = share_mujer),
    color = "#E8E8E8",
    linewidth = 0.45,
    alpha = 0.9
  ) +
  
  # Puntos de inicio y final sobre la frontera
  geom_point(
    data = puntos_frontera,
    aes(
      x = anio,
      y = share_mujer
    ),
    shape = 21,
    fill = "white",
    color = "gray25",
    stroke = 0.8,
    size = 3.4
  ) +
  
  # Etiquetas internas al inicio: solo porcentaje
  geom_label(
    data = labels_inicio_sexo,
    aes(
      x = anio,
      y = y,
      label = label,
      fill = sexo
    ),
    color = "white",
    fontface = "bold",
    size = 3.5,
    label.size = 0.15,
    label.padding = unit(0.15, "lines"),
    show.legend = FALSE
  ) +
  
  # Cambio en p.p. por categoría
  geom_label(
    data = labels_cambio_sexo,
    aes(
      x = x_label,
      y = y,
      label = label,
      fill = sexo
    ),
    color = "white",
    fontface = "bold",
    size = 3.3,
    label.size = 0.12,
    label.padding = unit(0.13, "lines"),
    alpha = 0.98,
    show.legend = FALSE
  ) +
  
  # Etiquetas finales directas
  geom_text(
    data = labels_final_sexo,
    aes(
      x = anio,
      y = y,
      label = label,
      color = sexo
    ),
    hjust = 0,
    fontface = "bold",
    size = 4
  ) +
  
  # Guías hacia etiquetas finales
  geom_segment(
    data = labels_final_sexo,
    aes(
      x = anio_final,
      xend = anio - 0.08,
      y = y,
      yend = y,
      color = sexo
    ),
    linewidth = 0.6,
    alpha = 0.8,
    show.legend = FALSE
  ) +
  
  scale_fill_manual(
    values = c(
      "Hombre" = "#8C1C13",
      "Mujer"  = "darkblue"
    )
  ) +
  scale_color_manual(
    values = c(
      "Hombre" = "#8C1C13",
      "Mujer"  = "darkblue"
    )
  ) +
  scale_x_continuous(
    breaks = sort(unique(serie_comp_sexo_wide$anio)),
    limits = c(anio_inicio, anio_final + 1.8)
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 1),
    breaks = seq(0, 1, 0.25),
    expand = expansion(mult = c(0, 0))
  ) +
  coord_cartesian(clip = "off") +
  labs(
    title = paste0(
      "Composición porcentual del número de trabajadores por sexo, ",
      anio_inicio,
      "–",
      anio_final
    ),
    subtitle = "Participación porcentual sobre el total de ocupados. Cálculo ponderado por factores de expansión",
    x = "",
    y = "Participación en el total de trabajadores",
    fill = NULL,
    color = NULL
  ) +
  theme_classic(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    axis.text.y = element_text(size = 10),
    legend.position = "none",
    panel.grid.major.y = element_line(color = "gray88", linewidth = 0.35),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 135, 10, 10)
  )

g_composicion_sexo_area

#========================================================
# GRÁFICO 4. Horas semanales promedio por sexo
#========================================================

#--------------------------------------------------------
# 23.1. Preparar datos
#--------------------------------------------------------

serie_horas_sexo <- geih %>%
  filter(
    !is.na(anio),
    !is.na(sexo),
    !is.na(fex),
    fex > 0,
    !is.na(horas),
    horas > 0,
    horas <= 112
  ) %>%
  mutate(
    sexo = factor(sexo, levels = c("Hombre", "Mujer"))
  ) %>%
  filter(!is.na(sexo)) %>%
  group_by(anio, sexo) %>%
  summarise(
    observaciones = n(),
    trabajadores_expandidos = sum(fex, na.rm = TRUE),
    horas_promedio = weighted_mean(horas, fex),
    .groups = "drop"
  ) %>%
  arrange(sexo, anio)

anio_inicio <- min(serie_horas_sexo$anio, na.rm = TRUE)
anio_final  <- max(serie_horas_sexo$anio, na.rm = TRUE)
x_label_crecimiento <- anio_inicio + 0.5 * (anio_final - anio_inicio)

max_y <- max(serie_horas_sexo$horas_promedio, na.rm = TRUE)
min_y <- min(serie_horas_sexo$horas_promedio, na.rm = TRUE)
rango_y <- max_y - min_y

offset_label <- 0.08 * rango_y

#--------------------------------------------------------
# 23.2. Etiquetas de inicio y final
#--------------------------------------------------------

labels_horas_inicio_fin <- serie_horas_sexo %>%
  filter(anio %in% c(anio_inicio, anio_final)) %>%
  mutate(
    x_label = case_when(
      anio == anio_inicio ~ anio - 0.18,
      anio == anio_final  ~ anio + 0.18
    ),
    y_label = case_when(
      anio == anio_inicio & sexo == "Hombre" ~ horas_promedio + offset_label,
      anio == anio_inicio & sexo == "Mujer"  ~ horas_promedio - offset_label,
      anio == anio_final & sexo == "Mujer"   ~ horas_promedio + offset_label,
      anio == anio_final & sexo == "Hombre"  ~ horas_promedio - offset_label,
      TRUE ~ horas_promedio
    )
  )

#--------------------------------------------------------
# 23.3. Crecimiento anualizado por sexo
#--------------------------------------------------------

crecimiento_horas_sexo <- serie_horas_sexo %>%
  filter(anio %in% c(anio_inicio, anio_final)) %>%
  select(anio, sexo, horas_promedio) %>%
  pivot_wider(
    names_from = anio,
    values_from = horas_promedio,
    names_prefix = "y_"
  ) %>%
  mutate(
    n_anios = anio_final - anio_inicio,
    horas_inicio = .data[[paste0("y_", anio_inicio)]],
    horas_final  = .data[[paste0("y_", anio_final)]],
    crecimiento_anualizado = 100 * ((horas_final / horas_inicio)^(1 / n_anios) - 1),
    label_crecimiento = paste0(
      "Decrecimiento anualizado ",
      anio_inicio,
      "–",
      anio_final,
      ": ",
      ifelse(crecimiento_anualizado >= 0, "+", ""),
      round(crecimiento_anualizado, 2),
      "% anual"
    )
  )

pos_crecimiento_horas_sexo <- crecimiento_horas_sexo %>%
  left_join(
    serie_horas_sexo %>%
      group_by(sexo) %>%
      summarise(
        max_grupo = max(horas_promedio, na.rm = TRUE),
        valor_final = horas_promedio[anio == max(anio)],
        .groups = "drop"
      ),
    by = "sexo"
  ) %>%
  mutate(
    x_inicio = anio_inicio,
    x_fin = anio_final,
    x_label = x_label_crecimiento,
    y_arrow = case_when(
      sexo == "Mujer"  ~ max_grupo + 0.18 * rango_y,   # justo encima de la serie azul
      sexo == "Hombre" ~ max_grupo + 0.08 * rango_y    # un poco encima de la serie roja
    )
  )

#--------------------------------------------------------
# 23.4. Graficar
#--------------------------------------------------------

g_horas_sexo <- ggplot(
  serie_horas_sexo,
  aes(
    x = anio,
    y = horas_promedio,
    color = sexo,
    group = sexo
  )
) +
  geom_line(
    linewidth = 1.2
  ) +
  geom_point(
    size = 3.2
  ) +
  
  # Flechas con crecimiento anualizado
  geom_segment(
    data = pos_crecimiento_horas_sexo,
    aes(
      x = x_inicio,
      xend = x_fin,
      y = y_arrow,
      yend = y_arrow,
      color = sexo
    ),
    inherit.aes = FALSE,
    linewidth = 0.75,
    alpha = 0.95,
    arrow = arrow(
      length = unit(0.18, "cm"),
      type = "closed"
    ),
    show.legend = FALSE
  ) +
  
  geom_label(
    data = pos_crecimiento_horas_sexo,
    aes(
      x = x_label,
      y = y_arrow,
      label = label_crecimiento,
      fill = sexo
    ),
    inherit.aes = FALSE,
    color = "white",
    fontface = "bold",
    size = 3.3,
    label.size = 0.15,
    label.padding = unit(0.16, "lines"),
    show.legend = FALSE
  ) +
  
  # Etiquetas de horas en inicio y final
  geom_label(
    data = labels_horas_inicio_fin,
    aes(
      x = x_label,
      y = y_label,
      label = number(horas_promedio, accuracy = 0.1),
      fill = sexo
    ),
    color = "white",
    fontface = "bold",
    size = 3.5,
    label.size = 0.15,
    label.padding = unit(0.14, "lines"),
    show.legend = FALSE
  ) +
  
  scale_color_manual(
    values = c(
      "Hombre" = "#8C1C13",
      "Mujer"  = "darkblue"
    )
  ) +
  scale_fill_manual(
    values = c(
      "Hombre" = "#8C1C13",
      "Mujer"  = "darkblue"
    )
  ) +
  scale_x_continuous(
    breaks = sort(unique(serie_horas_sexo$anio)),
    limits = c(anio_inicio - 0.5, anio_final + 0.8)
  ) +
  scale_y_continuous(
    labels = function(x) paste0(number(x, accuracy = 0.1), " h"),
    expand = expansion(mult = c(0.08, 0.32))
  ) +
  coord_cartesian(clip = "off") +
  labs(
    title = paste0(
      "Evolución de las horas semanales promedio por sexo, ",
      anio_inicio,
      "–",
      anio_final
    ),
    subtitle = "Promedio ponderado por factores de expansión",
    x = "Año",
    y = "Horas semanales promedio",
    color = NULL
  ) +
  theme_classic(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    legend.position = "bottom",
    legend.text = element_text(face = "bold"),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.35),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 95, 10, 10)
  )

g_horas_sexo


#========================================================
# GRÁFICO 5. Ingreso por formalidad
#========================================================

#--------------------------------------------------------
# 5.1. Preparar datos
#--------------------------------------------------------

serie_formalidad_plot <- geih_ingreso %>%
  filter(!is.na(formalidad_grupo)) %>%
  mutate(
    formalidad_grupo = factor(
      formalidad_grupo,
      levels = c("Informal", "Formal")
    )
  ) %>%
  filter(!is.na(formalidad_grupo)) %>%
  group_by(anio, formalidad_grupo) %>%
  summarise(
    observaciones = n(),
    trabajadores_expandidos = sum(fex, na.rm = TRUE),
    ingreso_hora_real_promedio = weighted_mean(ingreso_hora_real, fex),
    .groups = "drop"
  ) %>%
  arrange(formalidad_grupo, anio)

anio_inicio <- min(serie_formalidad_plot$anio, na.rm = TRUE)
anio_final  <- max(serie_formalidad_plot$anio, na.rm = TRUE)
x_label_crecimiento <- anio_inicio + 0.5 * (anio_final - anio_inicio)

max_y <- max(serie_formalidad_plot$ingreso_hora_real_promedio, na.rm = TRUE)
min_y <- min(serie_formalidad_plot$ingreso_hora_real_promedio, na.rm = TRUE)
rango_y <- max_y - min_y

offset_label <- 0.055 * rango_y

#--------------------------------------------------------
# 5.2. Etiquetas inicio y final
#--------------------------------------------------------

labels_formalidad <- serie_formalidad_plot %>%
  filter(anio %in% c(anio_inicio, anio_final)) %>%
  mutate(
    x_label = case_when(
      anio == anio_inicio ~ anio - 0.18,
      anio == anio_final  ~ anio + 0.18
    ),
    y_label = case_when(
      formalidad_grupo == "Formal"   ~ ingreso_hora_real_promedio + offset_label,
      formalidad_grupo == "Informal" ~ ingreso_hora_real_promedio - offset_label,
      TRUE ~ ingreso_hora_real_promedio
    )
  )

#--------------------------------------------------------
# 5.3. Crecimiento anualizado por formalidad
#--------------------------------------------------------

crecimiento_formalidad <- serie_formalidad_plot %>%
  filter(anio %in% c(anio_inicio, anio_final)) %>%
  select(anio, formalidad_grupo, ingreso_hora_real_promedio) %>%
  pivot_wider(
    names_from = anio,
    values_from = ingreso_hora_real_promedio,
    names_prefix = "y_"
  ) %>%
  mutate(
    n_anios = anio_final - anio_inicio,
    ingreso_inicio = .data[[paste0("y_", anio_inicio)]],
    ingreso_final  = .data[[paste0("y_", anio_final)]],
    crecimiento_anualizado = 100 * ((ingreso_final / ingreso_inicio)^(1 / n_anios) - 1),
    label_crecimiento = paste0(
      "Crecimiento anualizado ",
      anio_inicio,
      "–",
      anio_final,
      ": ",
      ifelse(crecimiento_anualizado >= 0, "+", ""),
      round(crecimiento_anualizado, 2),
      "% anual"
    )
  )

pos_cambio_formalidad <- crecimiento_formalidad %>%
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
    x_inicio = anio_inicio,
    x_fin = anio_final,
    x_label = x_label_crecimiento,
    y_arrow = case_when(
      formalidad_grupo == "Formal"   ~ max_grupo + 0.07 * rango_y,
      formalidad_grupo == "Informal" ~ max_grupo + 0.10 * rango_y,
      TRUE ~ max_grupo + 0.08 * rango_y
    )
  )

#--------------------------------------------------------
# 5.4. Graficar
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
  
  # Flechas con crecimiento anualizado
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
    linewidth = 0.85,
    alpha = 0.95,
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
      label = label_crecimiento,
      fill = formalidad_grupo
    ),
    inherit.aes = FALSE,
    color = "white",
    fontface = "bold",
    size = 3.3,
    label.size = 0.15,
    label.padding = unit(0.16, "lines"),
    show.legend = FALSE
  ) +
  
  # Etiquetas de ingreso en inicio y final
  geom_label(
    data = labels_formalidad,
    aes(
      x = x_label,
      y = y_label,
      label = comma(ingreso_hora_real_promedio, accuracy = 1),
      fill = formalidad_grupo
    ),
    color = "white",
    fontface = "bold",
    size = 3.4,
    label.size = 0.15,
    label.padding = unit(0.14, "lines"),
    show.legend = FALSE
  ) +
  
  scale_color_manual(
    values = c(
      "Informal" = "darkred",
      "Formal"   = "darkblue"
    ),
    name = ""
  ) +
  scale_fill_manual(
    values = c(
      "Informal" = "darkred",
      "Formal"   = "darkblue"
    )
  ) +
  scale_x_continuous(
    breaks = sort(unique(serie_formalidad_plot$anio)),
    limits = c(anio_inicio - 0.5, anio_final + 0.8)
  ) +
  scale_y_continuous(
    labels = comma,
    expand = expansion(mult = c(0.08, 0.24))
  ) +
  coord_cartesian(clip = "off") +
  labs(
    title = paste0(
      "Evolución del ingreso laboral por hora promedio por formalidad, ",
      anio_inicio,
      "–",
      anio_final
    ),
    subtitle = "Pesos constantes de 2025. Promedio ponderado por factores de expansión",
    x = "",
    y = "Ingreso laboral por hora promedio"
  ) +
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 90, hjust = 1),
    axis.text.y = element_text(size = 10),
    legend.position = "bottom",
    legend.text = element_text(face = "bold", size = 11),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.35),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 95, 10, 10)
  )

g_ingreso_formalidad_niveles

#========================================================
# GRÁFICO 6. Razón de ingreso formal / informal
#========================================================

#--------------------------------------------------------
# 6.1. Preparar datos
#--------------------------------------------------------

serie_ingreso_formalidad <- geih_ingreso %>%
  filter(!is.na(formalidad_grupo)) %>%
  mutate(
    formalidad_grupo = factor(
      formalidad_grupo,
      levels = c("Informal", "Formal")
    )
  ) %>%
  filter(!is.na(formalidad_grupo)) %>%
  group_by(anio, formalidad_grupo) %>%
  summarise(
    observaciones = n(),
    trabajadores_expandidos = sum(fex, na.rm = TRUE),
    ingreso_hora_real_promedio = weighted_mean(ingreso_hora_real, fex),
    .groups = "drop"
  ) %>%
  arrange(formalidad_grupo, anio)

serie_razon_formalidad <- serie_ingreso_formalidad %>%
  select(anio, formalidad_grupo, ingreso_hora_real_promedio) %>%
  pivot_wider(
    names_from = formalidad_grupo,
    values_from = ingreso_hora_real_promedio
  ) %>%
  mutate(
    razon_formal_informal = Formal / Informal,
    label_razon = paste0(number(razon_formal_informal, accuracy = 0.01), "x")
  ) %>%
  arrange(anio)

anio_inicio <- min(serie_razon_formalidad$anio, na.rm = TRUE)
anio_final  <- max(serie_razon_formalidad$anio, na.rm = TRUE)
x_label_crecimiento <- anio_inicio + 0.5 * (anio_final - anio_inicio)

max_y <- max(serie_razon_formalidad$razon_formal_informal, na.rm = TRUE)
min_y <- min(serie_razon_formalidad$razon_formal_informal, na.rm = TRUE)
rango_y <- max_y - min_y

#--------------------------------------------------------
# 6.2. Etiquetas inicio y final
#--------------------------------------------------------

labels_razon_formalidad <- serie_razon_formalidad %>%
  filter(anio %in% c(anio_inicio, anio_final)) %>%
  mutate(
    x_label = case_when(
      anio == anio_inicio ~ anio - 0.18,
      anio == anio_final  ~ anio + 0.18
    ),
    y_label = case_when(
      anio == anio_inicio ~ razon_formal_informal + 0.06 * rango_y,
      anio == anio_final  ~ razon_formal_informal + 0.06 * rango_y,
      TRUE ~ razon_formal_informal
    )
  )

#--------------------------------------------------------
# 6.3. Graficar
#--------------------------------------------------------

g_razon_formal_informal <- ggplot(
  serie_razon_formalidad,
  aes(
    x = anio,
    y = razon_formal_informal
  )
) +
  geom_hline(
    yintercept = 1,
    linetype = "dashed",
    color = "gray45",
    linewidth = 0.7
  ) +
  geom_line(
    color = "darkblue",
    linewidth = 1.35,
    alpha = 0.95
  ) +
  geom_point(
    shape = 21,
    fill = "darkblue",
    color = "white",
    stroke = 0.8,
    size = 3.8
  ) +
  
  # Etiquetas de inicio y final
  geom_label(
    data = labels_razon_formalidad,
    aes(
      x = x_label,
      y = y_label,
      label = label_razon
    ),
    inherit.aes = FALSE,
    fill = "darkblue",
    color = "white",
    fontface = "bold",
    size = 3.5,
    label.size = 0.15,
    label.padding = unit(0.14, "lines"),
    show.legend = FALSE
  ) +
  scale_x_continuous(
    breaks = sort(unique(serie_razon_formalidad$anio)),
    limits = c(anio_inicio - 0.5, anio_final + 0.8)
  ) +
  scale_y_continuous(
    labels = function(x) paste0(number(x, accuracy = 0.01), "x"),
    expand = expansion(mult = c(0.08, 0.16))
  ) +
  coord_cartesian(clip = "off") +
  labs(
    title = paste0(
      "Razón del ingreso laboral por hora entre formales e informales, ",
      anio_inicio,
      "–",
      anio_final
    ),
    subtitle = "Razón: ingreso promedio formal / ingreso promedio informal. Pesos constantes de 2025",
    x = "",
    y = "Razón formal / informal"
  ) +
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 90, hjust = 1),
    axis.text.y = element_text(size = 10),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.35),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 70, 10, 10)
  )

g_razon_formal_informal

#========================================================
# GRÁFICO 7. Composición porcentual por formalidad
# Área apilada 100%
#========================================================

#--------------------------------------------------------
# 7.1. Preparar datos
#--------------------------------------------------------

serie_comp_formalidad <- geih %>%
  filter(
    !is.na(anio),
    !is.na(formalidad_grupo),
    !is.na(fex),
    fex > 0
  ) %>%
  mutate(
    formalidad_grupo = factor(
      formalidad_grupo,
      levels = c("Formal", "Informal")
    )
  ) %>%
  filter(!is.na(formalidad_grupo)) %>%
  group_by(anio, formalidad_grupo) %>%
  summarise(
    observaciones = n(),
    trabajadores_expandidos = sum(fex, na.rm = TRUE),
    trabajadores_millones = trabajadores_expandidos / 1e6,
    .groups = "drop"
  ) %>%
  group_by(anio) %>%
  mutate(
    total_trabajadores = sum(trabajadores_millones, na.rm = TRUE),
    participacion = trabajadores_millones / total_trabajadores
  ) %>%
  ungroup() %>%
  arrange(anio, formalidad_grupo)

serie_comp_formalidad_wide <- serie_comp_formalidad %>%
  select(anio, formalidad_grupo, participacion) %>%
  pivot_wider(
    names_from = formalidad_grupo,
    values_from = participacion,
    values_fill = 0
  ) %>%
  mutate(
    share_formal = Formal,
    share_informal = Informal,
    
    ymin_formal = 0,
    ymax_formal = share_formal,
    
    ymin_informal = share_formal,
    ymax_informal = 1
  ) %>%
  arrange(anio)

anio_inicio <- min(serie_comp_formalidad_wide$anio, na.rm = TRUE)
anio_final  <- max(serie_comp_formalidad_wide$anio, na.rm = TRUE)

#--------------------------------------------------------
# 7.2. Etiquetas de inicio
# Solo porcentaje, sin repetir nombre del grupo
#--------------------------------------------------------

labels_inicio_formalidad <- bind_rows(
  
  serie_comp_formalidad_wide %>%
    filter(anio == anio_inicio) %>%
    transmute(
      anio = anio_inicio + 0.25,
      formalidad_grupo = "Formal",
      y = share_formal / 2,
      label = percent(share_formal, accuracy = 0.1)
    ),
  
  serie_comp_formalidad_wide %>%
    filter(anio == anio_inicio) %>%
    transmute(
      anio = anio_inicio + 0.25,
      formalidad_grupo = "Informal",
      y = share_formal + share_informal / 2,
      label = percent(share_informal, accuracy = 0.1)
    )
)

#--------------------------------------------------------
# 7.3. Etiquetas finales
# Nombre + porcentaje
#--------------------------------------------------------

labels_final_formalidad <- bind_rows(
  
  serie_comp_formalidad_wide %>%
    filter(anio == anio_final) %>%
    transmute(
      anio = anio_final + 0.35,
      formalidad_grupo = "Formal",
      y = share_formal / 2,
      label = paste0("Formal: ", percent(share_formal, accuracy = 0.1))
    ),
  
  serie_comp_formalidad_wide %>%
    filter(anio == anio_final) %>%
    transmute(
      anio = anio_final + 0.35,
      formalidad_grupo = "Informal",
      y = share_formal + share_informal / 2,
      label = paste0("Informal: ", percent(share_informal, accuracy = 0.1))
    )
)

#--------------------------------------------------------
# 7.4. Cambio en puntos porcentuales por categoría
#--------------------------------------------------------

cambio_comp_formalidad <- bind_rows(
  
  serie_comp_formalidad_wide %>%
    filter(anio %in% c(anio_inicio, anio_final)) %>%
    transmute(
      anio,
      formalidad_grupo = "Formal",
      participacion = share_formal
    ),
  
  serie_comp_formalidad_wide %>%
    filter(anio %in% c(anio_inicio, anio_final)) %>%
    transmute(
      anio,
      formalidad_grupo = "Informal",
      participacion = share_informal
    )
) %>%
  pivot_wider(
    names_from = anio,
    values_from = participacion,
    names_prefix = "y_"
  ) %>%
  mutate(
    participacion_inicio = .data[[paste0("y_", anio_inicio)]],
    participacion_final  = .data[[paste0("y_", anio_final)]],
    cambio_pp = 100 * (participacion_final - participacion_inicio),
    label_cambio = paste0(
      ifelse(cambio_pp >= 0, "+", ""),
      round(cambio_pp, 1),
      " p.p."
    )
  )

anio_label_cambio <- serie_comp_formalidad_wide$anio[
  which.min(abs(
    serie_comp_formalidad_wide$anio -
      (anio_inicio + 0.58 * (anio_final - anio_inicio))
  ))
]

labels_cambio_formalidad <- bind_rows(
  
  serie_comp_formalidad_wide %>%
    filter(anio == anio_label_cambio) %>%
    transmute(
      anio,
      formalidad_grupo = "Formal",
      y = share_formal / 2
    ),
  
  serie_comp_formalidad_wide %>%
    filter(anio == anio_label_cambio) %>%
    transmute(
      anio,
      formalidad_grupo = "Informal",
      y = share_formal + share_informal / 2
    )
) %>%
  left_join(
    cambio_comp_formalidad %>%
      select(formalidad_grupo, label_cambio),
    by = "formalidad_grupo"
  ) %>%
  mutate(
    x_label = anio_label_cambio,
    label = label_cambio
  )

#--------------------------------------------------------
# 7.5. Puntos sobre la frontera
#--------------------------------------------------------

puntos_frontera_formalidad <- serie_comp_formalidad_wide %>%
  filter(anio %in% c(anio_inicio, anio_final))

#--------------------------------------------------------
# 7.6. Graficar
#--------------------------------------------------------

g_composicion_formalidad_area <- ggplot(
  serie_comp_formalidad_wide,
  aes(x = anio)
) +
  geom_ribbon(
    aes(
      ymin = ymin_formal,
      ymax = ymax_formal,
      fill = "Formal"
    ),
    alpha = 0.96,
    linewidth = 0
  ) +
  geom_ribbon(
    aes(
      ymin = ymin_informal,
      ymax = ymax_informal,
      fill = "Informal"
    ),
    alpha = 0.96,
    linewidth = 0
  ) +
  
  # Línea divisoria: muestra el cambio en la composición
  geom_line(
    aes(y = share_formal),
    color = "white",
    linewidth = 1.15,
    alpha = 0.95
  ) +
  geom_line(
    aes(y = share_formal),
    color = "#E8E8E8",
    linewidth = 0.45,
    alpha = 0.9
  ) +
  
  # Puntos de inicio y final sobre la frontera
  geom_point(
    data = puntos_frontera_formalidad,
    aes(
      x = anio,
      y = share_formal
    ),
    shape = 21,
    fill = "white",
    color = "gray25",
    stroke = 0.8,
    size = 3.4
  ) +
  
  # Etiquetas internas al inicio: solo porcentaje
  geom_label(
    data = labels_inicio_formalidad,
    aes(
      x = anio,
      y = y,
      label = label,
      fill = formalidad_grupo
    ),
    color = "white",
    fontface = "bold",
    size = 3.5,
    label.size = 0.15,
    label.padding = unit(0.15, "lines"),
    show.legend = FALSE
  ) +
  
  # Cambio en p.p. por categoría
  geom_label(
    data = labels_cambio_formalidad,
    aes(
      x = x_label,
      y = y,
      label = label,
      fill = formalidad_grupo
    ),
    color = "white",
    fontface = "bold",
    size = 3.3,
    label.size = 0.12,
    label.padding = unit(0.13, "lines"),
    alpha = 0.98,
    show.legend = FALSE
  ) +
  
  # Etiquetas finales directas
  geom_text(
    data = labels_final_formalidad,
    aes(
      x = anio,
      y = y,
      label = label,
      color = formalidad_grupo
    ),
    hjust = 0,
    fontface = "bold",
    size = 4
  ) +
  
  # Guías hacia etiquetas finales
  geom_segment(
    data = labels_final_formalidad,
    aes(
      x = anio_final,
      xend = anio - 0.08,
      y = y,
      yend = y,
      color = formalidad_grupo
    ),
    linewidth = 0.6,
    alpha = 0.8,
    show.legend = FALSE
  ) +
  
  scale_fill_manual(
    values = c(
      "Informal" = "darkred",
      "Formal"   = "darkblue"
    )
  ) +
  scale_color_manual(
    values = c(
      "Informal" = "darkred",
      "Formal"   = "darkblue"
    )
  ) +
  scale_x_continuous(
    breaks = sort(unique(serie_comp_formalidad_wide$anio)),
    limits = c(anio_inicio, anio_final + 1.8)
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 1),
    breaks = seq(0, 1, 0.25),
    expand = expansion(mult = c(0, 0))
  ) +
  coord_cartesian(clip = "off") +
  labs(
    title = paste0(
      "Composición porcentual del número de trabajadores por formalidad, ",
      anio_inicio,
      "–",
      anio_final
    ),
    subtitle = "Participación porcentual sobre el total de ocupados. Cálculo ponderado por factores de expansión",
    x = "",
    y = "Participación en el total de trabajadores",
    fill = NULL,
    color = NULL
  ) +
  theme_classic(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    axis.text.y = element_text(size = 10),
    legend.position = "none",
    panel.grid.major.y = element_line(color = "gray88", linewidth = 0.35),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 135, 10, 10)
  )

g_composicion_formalidad_area

#========================================================
# GRÁFICO 8. Número total de trabajadores por año
#========================================================

#--------------------------------------------------------
# 25.1. Preparar datos
#--------------------------------------------------------

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

anio_inicio <- min(serie_trabajadores_total$anio, na.rm = TRUE)
anio_final  <- max(serie_trabajadores_total$anio, na.rm = TRUE)

trabajadores_inicio <- serie_trabajadores_total %>%
  filter(anio == anio_inicio) %>%
  pull(trabajadores_millones)

trabajadores_final <- serie_trabajadores_total %>%
  filter(anio == anio_final) %>%
  pull(trabajadores_millones)

crecimiento_anualizado_trab <- 100 * (
  (trabajadores_final / trabajadores_inicio)^(1 / (anio_final - anio_inicio)) - 1
)

label_crecimiento_trab <- paste0(
  "Crecimiento anualizado ",
  anio_inicio,
  "–",
  anio_final,
  ": ",
  ifelse(crecimiento_anualizado_trab >= 0, "+", ""),
  round(crecimiento_anualizado_trab, 2),
  "% anual"
)

max_y <- max(serie_trabajadores_total$trabajadores_millones, na.rm = TRUE)
min_y <- min(serie_trabajadores_total$trabajadores_millones, na.rm = TRUE)
rango_y <- max_y - min_y

y_arrow <- max_y + 0.12 * rango_y
x_label_crecimiento <- anio_inicio + 0.5 * (anio_final - anio_inicio)

labels_trabajadores_total <- serie_trabajadores_total %>%
  mutate(
    label = paste0(number(trabajadores_millones, accuracy = 0.1), " M")
  )

g_trabajadores_total <- ggplot(
  serie_trabajadores_total,
  aes(
    x = anio,
    y = trabajadores_millones
  )
) +
  geom_line(
    color = "darkblue",
    linewidth = 1.35,
    alpha = 0.95
  ) +
  geom_point(
    shape = 21,
    fill = "darkblue",
    color = "white",
    stroke = 0.8,
    size = 3.8
  ) +
  
  geom_label(
    data = labels_trabajadores_total,
    aes(label = label),
    fill = "darkblue",
    color = "white",
    fontface = "bold",
    size = 3.2,
    label.size = 0.15,
    label.padding = unit(0.13, "lines"),
    vjust = -0.75,
    show.legend = FALSE
  ) +
  
  geom_segment(
    aes(
      x = anio_inicio,
      xend = anio_final,
      y = y_arrow,
      yend = y_arrow
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
      x = x_label_crecimiento,
      y = y_arrow,
      label = label_crecimiento_trab
    ),
    inherit.aes = FALSE,
    fill = "white",
    color = "black",
    fontface = "bold",
    size = 3.8,
    label.size = 0.15,
    label.padding = unit(0.18, "lines")
  ) +
  
  scale_x_continuous(
    breaks = sort(unique(serie_trabajadores_total$anio)),
    limits = c(anio_inicio - 0.5, anio_final + 0.5)
  ) +
  scale_y_continuous(
    labels = function(x) paste0(number(x, accuracy = 0.1), " M"),
    expand = expansion(mult = c(0.08, 0.22))
  ) +
  coord_cartesian(clip = "off") +
  labs(
    title = paste0(
      "Evolución del número total de trabajadores, ",
      anio_inicio,
      "–",
      anio_final
    ),
    subtitle = "Trabajadores ocupados expandidos con factores de expansión. Valores en millones",
    x = "",
    y = "Trabajadores, millones"
  ) +
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 90, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.35),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 70, 10, 10)
  )

g_trabajadores_total

#========================================================
# GRÁFICO 9. Tasa de informalidad
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

#--------------------------------------------------------
# 12.2. Preparar elementos auxiliares
#--------------------------------------------------------

resumen_informalidad <- serie_informalidad %>%
  summarise(
    tasa_inicial = tasa_informalidad[anio == min(anio, na.rm = TRUE)],
    tasa_final   = tasa_informalidad[anio == max(anio, na.rm = TRUE)],
    cambio_pp    = 100 * (tasa_final - tasa_inicial)
  )

puntos_destacados <- serie_informalidad %>%
  mutate(
    tipo = case_when(
      anio == min(anio, na.rm = TRUE) ~ "Inicio",
      anio == max(anio, na.rm = TRUE) ~ "Final",
      TRUE ~ "Intermedio"
    )
  )

#--------------------------------------------------------
# 12.3. Graficar
#--------------------------------------------------------

g_tasa_informalidad <- ggplot(
  serie_informalidad,
  aes(
    x = anio,
    y = tasa_informalidad
  )
) +
  geom_line(
    color = "#1F2937",
    linewidth = 1.2
  ) +
  geom_point(
    data = puntos_destacados %>% filter(tipo == "Intermedio"),
    color = "#8B0000",
    size = 2.8
  ) +
  geom_point(
    data = puntos_destacados %>% filter(tipo != "Intermedio"),
    color = "#8B0000",
    size = 3.8
  ) +
  geom_label(
    aes(
      label = percent(tasa_informalidad, accuracy = 0.1)
    ),
    size = 3.3,
    fontface = "bold",
    fill = "white",
    color = "black",
    label.size = 0.15,
    label.padding = unit(0.12, "lines"),
    vjust = -0.75
  ) +
  annotate(
    "text",
    x = max(serie_informalidad$anio, na.rm = TRUE) - 1.5,
    y = max(serie_informalidad$tasa_informalidad, na.rm = TRUE) + 0.002,
    label = paste0(
      "Cambio 20010–2025: ",
      ifelse(resumen_informalidad$cambio_pp >= 0, "+", ""),
      round(resumen_informalidad$cambio_pp, 1),
      " p.p."
    ),
    hjust = 1,
    size = 4,
    fontface = "bold",
    color = "#1F2937"
  ) +
  scale_x_continuous(
    breaks = sort(unique(serie_informalidad$anio))
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(
      floor(min(serie_informalidad$tasa_informalidad, na.rm = TRUE) * 100) / 100 - 0.01,
      ceiling(max(serie_informalidad$tasa_informalidad, na.rm = TRUE) * 100) / 100 + 0.02
    ),
    expand = expansion(mult = c(0.01, 0.03))
  ) +
  labs(
    title = "Tasa de informalidad laboral en Colombia, 2010–2025",
    subtitle = "Porcentaje de trabajadores que no cotizan a pensión. Cálculo ponderado por factores de expansión",
    x = "",
    y = "Tasa de informalidad"
  ) +
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 90, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 20, 10, 10)
  )

g_tasa_informalidad

#========================================================
# GRÁFICO 10. Ingreso por nivel educativo: 2010 vs 2025
#========================================================

#--------------------------------------------------------
# 10.1. Preparar datos
#--------------------------------------------------------

niveles_educacion_agrupados <- c(
  "Ninguno o preescolar",
  "Básica primaria",
  "Básica secundaria o media",
  "Superior o universitaria"
)

serie_educacion_2010_2025 <- geih_ingreso %>%
  filter(
    anio %in% c(2010, 2025),
    !is.na(educacion),
    educacion != "No sabe, no informa"
  ) %>%
  mutate(
    educacion_grupo = case_when(
      educacion %in% c("Ninguno", "Preescolar") ~ "Ninguno o preescolar",
      educacion == "Básica primaria" ~ "Básica primaria",
      educacion %in% c("Básica secundaria", "Media") ~ "Básica secundaria o media",
      educacion == "Superior o universitaria" ~ "Superior o universitaria",
      TRUE ~ NA_character_
    ),
    educacion_grupo = factor(
      educacion_grupo,
      levels = niveles_educacion_agrupados
    )
  ) %>%
  filter(!is.na(educacion_grupo)) %>%
  group_by(anio, educacion_grupo) %>%
  summarise(
    observaciones = n(),
    trabajadores_expandidos = sum(fex, na.rm = TRUE),
    ingreso_hora_real_promedio = weighted_mean(ingreso_hora_real, fex),
    .groups = "drop"
  ) %>%
  mutate(
    anio = factor(anio, levels = c(2010, 2025))
  )

#--------------------------------------------------------
# 10.2. Graficar
#--------------------------------------------------------

g_barras_educacion_2010_2025 <- ggplot(
  serie_educacion_2010_2025,
  aes(
    x = educacion_grupo,
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
    label.padding = unit(0.14, "lines"),
    hjust = -0.08,
    show.legend = FALSE
  ) +
  coord_flip(clip = "off") +
  scale_fill_manual(
    values = c(
      "2010" = "#8C1C13",
      "2025" = "darkblue"
    )
  ) +
  scale_y_continuous(
    labels = comma,
    expand = expansion(mult = c(0.02, 0.18))
  ) +
  labs(
    title = "Ingreso laboral por hora real por nivel educativo: 2010 vs. 2025",
    subtitle = "Pesos constantes de 2025. Promedio ponderado por factores de expansión",
    x = "Nivel educativo",
    y = "Ingreso laboral por hora promedio",
    fill = "Año"
  ) +
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(face = "bold"),
    panel.grid.major.x = element_line(color = "gray90", linewidth = 0.35),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 40, 10, 10)
  )

g_barras_educacion_2010_2025

#========================================================
# GRÁFICO 11. Crecimiento del ingreso por nivel educativo
# Lollipop - Índice año inicial = 100
# Etiqueta: crecimiento anualizado
#========================================================

#--------------------------------------------------------
# 11.1. Preparar datos
#--------------------------------------------------------

niveles_educacion_agrupados <- c(
  "Ninguno o preescolar",
  "Básica primaria",
  "Básica secundaria o media",
  "Superior o universitaria"
)

serie_educacion_anual <- geih_ingreso %>%
  filter(
    !is.na(educacion),
    educacion != "No sabe, no informa"
  ) %>%
  mutate(
    educacion_grupo = case_when(
      educacion %in% c("Ninguno", "Preescolar") ~ "Ninguno o preescolar",
      educacion == "Básica primaria" ~ "Básica primaria",
      educacion %in% c("Básica secundaria", "Media") ~ "Básica secundaria o media",
      educacion == "Superior o universitaria" ~ "Superior o universitaria",
      TRUE ~ NA_character_
    ),
    educacion_grupo = factor(
      educacion_grupo,
      levels = niveles_educacion_agrupados
    )
  ) %>%
  filter(!is.na(educacion_grupo)) %>%
  group_by(anio, educacion_grupo) %>%
  summarise(
    observaciones = n(),
    trabajadores_expandidos = sum(fex, na.rm = TRUE),
    ingreso_hora_real_promedio = weighted_mean(ingreso_hora_real, fex),
    .groups = "drop"
  ) %>%
  arrange(educacion_grupo, anio)

anio_inicio <- min(serie_educacion_anual$anio, na.rm = TRUE)
anio_final  <- max(serie_educacion_anual$anio, na.rm = TRUE)

educacion_crecimiento <- serie_educacion_anual %>%
  filter(anio %in% c(anio_inicio, anio_final)) %>%
  select(anio, educacion_grupo, ingreso_hora_real_promedio) %>%
  pivot_wider(
    names_from = anio,
    values_from = ingreso_hora_real_promedio,
    names_prefix = "y_"
  ) %>%
  filter(
    !is.na(.data[[paste0("y_", anio_inicio)]]),
    !is.na(.data[[paste0("y_", anio_final)]])
  ) %>%
  mutate(
    ingreso_inicio = .data[[paste0("y_", anio_inicio)]],
    ingreso_final  = .data[[paste0("y_", anio_final)]],
    
    indice_inicio = 100,
    indice_final = 100 * ingreso_final / ingreso_inicio,
    
    crecimiento_pct = indice_final - 100,
    
    crecimiento_anualizado = 100 * (
      (ingreso_final / ingreso_inicio)^(1 / (anio_final - anio_inicio)) - 1
    ),
    
    grupo_crecimiento = case_when(
      crecimiento_anualizado < 0    ~ "Decrecimiento",
      crecimiento_anualizado < 0.75 ~ "Crecimiento bajo",
      crecimiento_anualizado < 1.50 ~ "Crecimiento medio",
      TRUE                          ~ "Crecimiento alto"
    ),
    grupo_crecimiento = factor(
      grupo_crecimiento,
      levels = c(
        "Crecimiento alto",
        "Crecimiento medio",
        "Crecimiento bajo",
        "Decrecimiento"
      )
    ),
    
    label_final = paste0(
      round(indice_final, 1),
      " | ",
      ifelse(crecimiento_anualizado >= 0, "+", ""),
      round(crecimiento_anualizado, 2),
      "% anual"
    ),
    
    educacion_grupo = reorder(educacion_grupo, indice_final)
  )

#--------------------------------------------------------
# 11.2. Graficar
#--------------------------------------------------------

g_lollipop_educacion_crecimiento <- ggplot(
  educacion_crecimiento,
  aes(
    y = educacion_grupo,
    x = indice_final
  )
) +
  geom_vline(
    xintercept = 100,
    linetype = "dashed",
    color = "gray45",
    linewidth = 0.7
  ) +
  geom_segment(
    aes(
      x = 100,
      xend = indice_final,
      y = educacion_grupo,
      yend = educacion_grupo,
      color = grupo_crecimiento
    ),
    linewidth = 1.5,
    lineend = "round"
  ) +
  geom_point(
    aes(color = grupo_crecimiento),
    size = 4.5
  ) +
  geom_text(
    data = educacion_crecimiento %>% filter(indice_final >= 100),
    aes(label = label_final),
    hjust = -0.10,
    size = 3.5,
    fontface = "bold"
  ) +
  geom_text(
    data = educacion_crecimiento %>% filter(indice_final < 100),
    aes(label = label_final),
    hjust = 1.10,
    size = 3.5,
    fontface = "bold"
  ) +
  scale_color_manual(
    values = c(
      "Crecimiento alto"  = "#0B7285",
      "Crecimiento medio" = "#E59F00",
      "Crecimiento bajo"  = "#A61E4D",
      "Decrecimiento"     = "#8B0000"
    )
  ) +
  scale_x_continuous(
    limits = c(
      min(95, min(educacion_crecimiento$indice_final, na.rm = TRUE) - 8),
      max(educacion_crecimiento$indice_final, na.rm = TRUE) + 24
    ),
    breaks = seq(80, 180, 10)
  ) +
  labs(
    title = paste0(
      "Crecimiento del ingreso laboral por hora real por nivel educativo, ",
      anio_inicio,
      "–",
      anio_final
    ),
    subtitle = paste0(
      "Índice base ",
      anio_inicio,
      " = 100. La etiqueta muestra el crecimiento anualizado"
    ),
    x = paste0("Índice del ingreso laboral por hora, ", anio_inicio, " = 100"),
    y = "Nivel educativo",
    color = "Grupo"
  ) +
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(face = "bold"),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(face = "bold"),
    panel.grid.major.x = element_line(color = "gray90", linewidth = 0.35),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 110, 10, 10)
  )

g_lollipop_educacion_crecimiento

#========================================================
# GRÁFICO 12. Composición porcentual por nivel educativo
# Área apilada 100%
#========================================================

#--------------------------------------------------------
# 26.1. Preparar datos
#--------------------------------------------------------

niveles_educacion_agrupados <- c(
  "Ninguno o preescolar",
  "Básica primaria",
  "Básica secundaria o media",
  "Superior o universitaria"
)

serie_comp_educacion <- geih %>%
  filter(
    !is.na(anio),
    !is.na(educacion),
    educacion != "No sabe, no informa",
    !is.na(fex),
    fex > 0
  ) %>%
  mutate(
    educacion_grupo = case_when(
      educacion %in% c("Ninguno", "Preescolar") ~ "Ninguno o preescolar",
      educacion == "Básica primaria" ~ "Básica primaria",
      educacion %in% c("Básica secundaria", "Media") ~ "Básica secundaria o media",
      educacion == "Superior o universitaria" ~ "Superior o universitaria",
      TRUE ~ NA_character_
    ),
    educacion_grupo = factor(
      educacion_grupo,
      levels = niveles_educacion_agrupados
    )
  ) %>%
  filter(!is.na(educacion_grupo)) %>%
  group_by(anio, educacion_grupo) %>%
  summarise(
    observaciones = n(),
    trabajadores_expandidos = sum(fex, na.rm = TRUE),
    trabajadores_millones = trabajadores_expandidos / 1e6,
    .groups = "drop"
  ) %>%
  group_by(anio) %>%
  mutate(
    total_trabajadores = sum(trabajadores_millones, na.rm = TRUE),
    participacion = trabajadores_millones / total_trabajadores
  ) %>%
  ungroup() %>%
  arrange(anio, educacion_grupo)

serie_comp_educacion_area <- serie_comp_educacion %>%
  group_by(anio) %>%
  arrange(educacion_grupo, .by_group = TRUE) %>%
  mutate(
    ymax = cumsum(participacion),
    ymin = ymax - participacion,
    ymid = (ymin + ymax) / 2
  ) %>%
  ungroup()

anio_inicio <- min(serie_comp_educacion_area$anio, na.rm = TRUE)
anio_final  <- max(serie_comp_educacion_area$anio, na.rm = TRUE)

#--------------------------------------------------------
# 26.2. Etiquetas de inicio y final
#--------------------------------------------------------

labels_inicio_educacion <- serie_comp_educacion_area %>%
  filter(anio == anio_inicio) %>%
  mutate(
    anio_label = anio_inicio + 0.25,
    label = percent(participacion, accuracy = 0.1)
  )

labels_final_educacion <- serie_comp_educacion_area %>%
  filter(anio == anio_final) %>%
  mutate(
    anio_label = anio_final + 0.35,
    label = paste0(
      stringr::str_wrap(as.character(educacion_grupo), width = 20),
      ": ",
      percent(participacion, accuracy = 0.1)
    )
  )

cambio_comp_educacion <- serie_comp_educacion_area %>%
  filter(anio %in% c(anio_inicio, anio_final)) %>%
  select(anio, educacion_grupo, participacion) %>%
  pivot_wider(
    names_from = anio,
    values_from = participacion,
    names_prefix = "y_"
  ) %>%
  mutate(
    participacion_inicio = .data[[paste0("y_", anio_inicio)]],
    participacion_final  = .data[[paste0("y_", anio_final)]],
    cambio_pp = 100 * (participacion_final - participacion_inicio),
    label_cambio = paste0(
      ifelse(cambio_pp >= 0, "+", ""),
      round(cambio_pp, 1),
      " p.p."
    )
  )

anio_label_cambio <- serie_comp_educacion_area$anio[
  which.min(abs(serie_comp_educacion_area$anio - (anio_inicio + 0.58 * (anio_final - anio_inicio))))
]

labels_cambio_educacion <- serie_comp_educacion_area %>%
  filter(anio == anio_label_cambio) %>%
  select(anio, educacion_grupo, ymid) %>%
  left_join(
    cambio_comp_educacion %>%
      select(educacion_grupo, label_cambio),
    by = "educacion_grupo"
  ) %>%
  mutate(
    x_label = anio_label_cambio,
    label = label_cambio
  )


#--------------------------------------------------------
# 26.3. Graficar
#--------------------------------------------------------

colores_educacion <- c(
  "Ninguno o preescolar"       = "#6D597A",
  "Básica primaria"            = "#B56576",
  "Básica secundaria o media"  = "#E56B6F",
  "Superior o universitaria"   = "darkblue"
)

g_composicion_educacion_area <- ggplot(
  serie_comp_educacion_area,
  aes(
    x = anio,
    fill = educacion_grupo
  )
) +
  geom_ribbon(
    aes(
      ymin = ymin,
      ymax = ymax,
      group = educacion_grupo
    ),
    alpha = 0.96,
    linewidth = 0
  ) +
  
  # Líneas divisorias suaves entre niveles educativos
  geom_line(
    aes(
      y = ymax,
      group = educacion_grupo
    ),
    color = "white",
    linewidth = 0.65,
    alpha = 0.85
  ) +
  
  # Etiquetas internas al inicio
  geom_label(
    data = labels_inicio_educacion,
    aes(
      x = anio_label,
      y = ymid,
      label = label,
      fill = educacion_grupo
    ),
    color = "white",
    fontface = "bold",
    size = 3.0,
    label.size = 0.12,
    label.padding = unit(0.13, "lines"),
    lineheight = 0.9,
    show.legend = FALSE
  ) +
  
  # Etiquetas finales directas
  geom_text(
    data = labels_final_educacion,
    aes(
      x = anio_label,
      y = ymid,
      label = label,
      color = educacion_grupo
    ),
    hjust = 0,
    fontface = "bold",
    size = 3.5,
    lineheight = 0.9,
    show.legend = FALSE
  ) +
  
  # Guías hacia etiquetas finales
  geom_segment(
    data = labels_final_educacion,
    aes(
      x = anio_final,
      xend = anio_label - 0.08,
      y = ymid,
      yend = ymid,
      color = educacion_grupo
    ),
    linewidth = 0.55,
    alpha = 0.8,
    show.legend = FALSE
  ) +
  
  # Anotación del cambio de educación superior
  geom_label(
    data = labels_cambio_educacion,
    aes(
      x = x_label,
      y = ymid,
      label = label,
      fill = educacion_grupo
    ),
    color = "white",
    fontface = "bold",
    size = 3.0,
    label.size = 0.12,
    label.padding = unit(0.13, "lines"),
    lineheight = 0.9,
    alpha = 0.98,
    show.legend = FALSE
  ) +
  
  scale_fill_manual(
    values = colores_educacion
  ) +
  scale_color_manual(
    values = colores_educacion
  ) +
  scale_x_continuous(
    breaks = sort(unique(serie_comp_educacion_area$anio)),
    limits = c(anio_inicio, anio_final + 2.4)
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 1),
    breaks = seq(0, 1, 0.25),
    expand = expansion(mult = c(0, 0))
  ) +
  coord_cartesian(clip = "off") +
  labs(
    title = paste0(
      "Composición porcentual del número de trabajadores por nivel educativo, ",
      anio_inicio,
      "–",
      anio_final
    ),
    subtitle = "Participación porcentual sobre el total de ocupados. Cálculo ponderado por factores de expansión",
    x = "",
    y = "Participación en el total de trabajadores",
    fill = NULL,
    color = NULL
  ) +
  theme_classic(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    axis.text.y = element_text(size = 10),
    legend.position = "none",
    panel.grid.major.y = element_line(color = "gray88", linewidth = 0.35),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 170, 10, 10)
  )

g_composicion_educacion_area

#========================================================
# GRÁFICO 13. Ingreso por posición ocupacional: año inicial vs año final
#========================================================

#--------------------------------------------------------
# 13.1. Preparar datos
#--------------------------------------------------------

ocupaciones_seleccionadas <- c(
  "Empleado particular",
  "Empleado gobierno",
  "Servicio doméstico",
  "Cuenta propia",
  "Patrón/empleador"
)

orden_ocupacion <- c(
  "Servicio doméstico",
  "Cuenta propia",
  "Empleado particular",
  "Empleado gobierno",
  "Patrón/empleador"
)

anio_inicio_ocupacion <- min(geih_ingreso$anio, na.rm = TRUE)
anio_final_ocupacion  <- max(geih_ingreso$anio, na.rm = TRUE)

serie_ocupacion_inicio_final <- geih_ingreso %>%
  filter(
    anio %in% c(anio_inicio_ocupacion, anio_final_ocupacion),
    ocupacion_label %in% ocupaciones_seleccionadas
  ) %>%
  mutate(
    ocupacion_label = factor(
      ocupacion_label,
      levels = orden_ocupacion
    ),
    anio = factor(
      anio,
      levels = c(anio_inicio_ocupacion, anio_final_ocupacion)
    )
  ) %>%
  filter(!is.na(ocupacion_label)) %>%
  group_by(anio, ocupacion_label) %>%
  summarise(
    observaciones = n(),
    trabajadores_expandidos = sum(fex, na.rm = TRUE),
    ingreso_hora_real_promedio = weighted_mean(ingreso_hora_real, fex),
    .groups = "drop"
  )

#--------------------------------------------------------
# 13.2. Graficar
#--------------------------------------------------------

colores_anios_ocupacion <- setNames(
  c("darkred", "darkblue"),
  c(as.character(anio_inicio_ocupacion), as.character(anio_final_ocupacion))
)

g_barras_ocupacion_inicio_final <- ggplot(
  serie_ocupacion_inicio_final,
  aes(
    x = ocupacion_label,
    y = ingreso_hora_real_promedio,
    fill = anio
  )
) +
  geom_col(
    position = position_dodge(width = 0.74),
    width = 0.62,
    alpha = 0.95
  ) +
  geom_label(
    aes(
      label = comma(ingreso_hora_real_promedio, accuracy = 1),
      fill = anio
    ),
    position = position_dodge(width = 0.74),
    color = "white",
    fontface = "bold",
    size = 3.2,
    label.size = 0.15,
    label.padding = unit(0.14, "lines"),
    hjust = -0.08,
    show.legend = FALSE
  ) +
  coord_flip(clip = "off") +
  scale_fill_manual(
    values = colores_anios_ocupacion
  ) +
  scale_y_continuous(
    labels = comma,
    expand = expansion(mult = c(0.02, 0.20))
  ) +
  labs(
    title = paste0(
      "Ingreso laboral por hora real por posición ocupacional: ",
      anio_inicio_ocupacion,
      " vs. ",
      anio_final_ocupacion
    ),
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
    legend.title = element_text(face = "bold"),
    legend.text = element_text(face = "bold"),
    panel.grid.major.x = element_line(color = "gray90", linewidth = 0.35),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 75, 10, 10)
  )

g_barras_ocupacion_inicio_final

#========================================================
# GRÁFICO 14. Crecimiento del ingreso por posición ocupacional
# Lollipop - Índice año inicial = 100
# Etiqueta: crecimiento anualizado
#========================================================

#--------------------------------------------------------
# 14.1. Preparar datos
#--------------------------------------------------------

ocupaciones_seleccionadas <- c(
  "Empleado particular",
  "Empleado gobierno",
  "Servicio doméstico",
  "Cuenta propia",
  "Patrón/empleador"
)

orden_ocupacion <- c(
  "Servicio doméstico",
  "Cuenta propia",
  "Empleado particular",
  "Empleado gobierno",
  "Patrón/empleador"
)

serie_ocupacion_anual <- geih_ingreso %>%
  filter(
    ocupacion_label %in% ocupaciones_seleccionadas
  ) %>%
  mutate(
    ocupacion_label = factor(
      ocupacion_label,
      levels = orden_ocupacion
    )
  ) %>%
  filter(!is.na(ocupacion_label)) %>%
  group_by(anio, ocupacion_label) %>%
  summarise(
    observaciones = n(),
    trabajadores_expandidos = sum(fex, na.rm = TRUE),
    ingreso_hora_real_promedio = weighted_mean(ingreso_hora_real, fex),
    .groups = "drop"
  ) %>%
  arrange(ocupacion_label, anio)

anio_inicio_ocupacion <- min(serie_ocupacion_anual$anio, na.rm = TRUE)
anio_final_ocupacion  <- max(serie_ocupacion_anual$anio, na.rm = TRUE)

ocupacion_crecimiento <- serie_ocupacion_anual %>%
  filter(anio %in% c(anio_inicio_ocupacion, anio_final_ocupacion)) %>%
  select(anio, ocupacion_label, ingreso_hora_real_promedio) %>%
  pivot_wider(
    names_from = anio,
    values_from = ingreso_hora_real_promedio,
    names_prefix = "y_"
  ) %>%
  filter(
    !is.na(.data[[paste0("y_", anio_inicio_ocupacion)]]),
    !is.na(.data[[paste0("y_", anio_final_ocupacion)]])
  ) %>%
  mutate(
    ingreso_inicio = .data[[paste0("y_", anio_inicio_ocupacion)]],
    ingreso_final  = .data[[paste0("y_", anio_final_ocupacion)]],
    
    indice_inicio = 100,
    indice_final = 100 * ingreso_final / ingreso_inicio,
    
    crecimiento_anualizado = 100 * (
      (ingreso_final / ingreso_inicio)^(1 / (anio_final_ocupacion - anio_inicio_ocupacion)) - 1
    ),
    
    grupo_crecimiento = case_when(
      crecimiento_anualizado < 0    ~ "Decrecimiento",
      crecimiento_anualizado < 0.75 ~ "Crecimiento bajo",
      crecimiento_anualizado < 1.50 ~ "Crecimiento medio",
      TRUE                          ~ "Crecimiento alto"
    ),
    
    grupo_crecimiento = factor(
      grupo_crecimiento,
      levels = c(
        "Crecimiento alto",
        "Crecimiento medio",
        "Crecimiento bajo",
        "Decrecimiento"
      )
    ),
    
    label_final = paste0(
      round(indice_final, 1),
      " | ",
      ifelse(crecimiento_anualizado >= 0, "+", ""),
      round(crecimiento_anualizado, 2),
      "% anual"
    ),
    
    ocupacion_label = reorder(ocupacion_label, indice_final)
  )

#--------------------------------------------------------
# 14.2. Graficar
#--------------------------------------------------------

g_lollipop_ocupacion_crecimiento <- ggplot(
  ocupacion_crecimiento,
  aes(
    y = ocupacion_label,
    x = indice_final
  )
) +
  geom_vline(
    xintercept = 100,
    linetype = "dashed",
    color = "gray45",
    linewidth = 0.7
  ) +
  geom_segment(
    aes(
      x = 100,
      xend = indice_final,
      y = ocupacion_label,
      yend = ocupacion_label,
      color = grupo_crecimiento
    ),
    linewidth = 1.5,
    lineend = "round"
  ) +
  geom_point(
    aes(color = grupo_crecimiento),
    size = 4.5
  ) +
  geom_text(
    data = ocupacion_crecimiento %>% filter(indice_final >= 100),
    aes(label = label_final),
    hjust = -0.10,
    size = 3.5,
    fontface = "bold"
  ) +
  geom_text(
    data = ocupacion_crecimiento %>% filter(indice_final < 100),
    aes(label = label_final),
    hjust = 1.10,
    size = 3.5,
    fontface = "bold"
  ) +
  scale_color_manual(
    values = c(
      "Crecimiento alto"  = "#0B7285",
      "Crecimiento medio" = "#E59F00",
      "Crecimiento bajo"  = "#A61E4D",
      "Decrecimiento"     = "#8B0000"
    )
  ) +
  scale_x_continuous(
    limits = c(
      min(95, min(ocupacion_crecimiento$indice_final, na.rm = TRUE) - 8),
      max(ocupacion_crecimiento$indice_final, na.rm = TRUE) + 24
    ),
    breaks = seq(80, 180, 10)
  ) +
  labs(
    title = paste0(
      "Crecimiento del ingreso laboral por hora real por posición ocupacional, ",
      anio_inicio_ocupacion,
      "–",
      anio_final_ocupacion
    ),
    subtitle = paste0(
      "Índice base ",
      anio_inicio_ocupacion,
      " = 100. La etiqueta muestra el crecimiento anualizado"
    ),
    x = paste0(
      "Índice del ingreso laboral por hora, ",
      anio_inicio_ocupacion,
      " = 100"
    ),
    y = "Posición ocupacional",
    color = "Grupo"
  ) +
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(face = "bold"),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(face = "bold"),
    panel.grid.major.x = element_line(color = "gray90", linewidth = 0.35),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 110, 10, 10)
  )

g_lollipop_ocupacion_crecimiento

#========================================================
# GRÁFICO 15. Composición porcentual por posición ocupacional
# Área apilada 100% incluyendo otras posiciones
#========================================================

#--------------------------------------------------------
# 15.1. Preparar datos
#--------------------------------------------------------

ocupaciones_seleccionadas <- c(
  "Empleado particular",
  "Empleado gobierno",
  "Servicio doméstico",
  "Cuenta propia",
  "Patrón/empleador"
)

orden_ocupacion_comp <- c(
  "Servicio doméstico",
  "Cuenta propia",
  "Empleado particular",
  "Empleado gobierno",
  "Patrón/empleador",
  "Otras posiciones"
)

serie_comp_ocupacion <- geih %>%
  filter(
    !is.na(anio),
    !is.na(fex),
    fex > 0
  ) %>%
  mutate(
    ocupacion_comp = case_when(
      ocupacion_label %in% ocupaciones_seleccionadas ~ ocupacion_label,
      TRUE ~ "Otras posiciones"
    ),
    ocupacion_comp = factor(
      ocupacion_comp,
      levels = orden_ocupacion_comp
    )
  ) %>%
  group_by(anio, ocupacion_comp) %>%
  summarise(
    observaciones = n(),
    trabajadores_expandidos = sum(fex, na.rm = TRUE),
    trabajadores_millones = trabajadores_expandidos / 1e6,
    .groups = "drop"
  ) %>%
  group_by(anio) %>%
  mutate(
    total_trabajadores = sum(trabajadores_millones, na.rm = TRUE),
    participacion = trabajadores_millones / total_trabajadores
  ) %>%
  ungroup() %>%
  arrange(anio, ocupacion_comp)

serie_comp_ocupacion_area <- serie_comp_ocupacion %>%
  group_by(anio) %>%
  arrange(ocupacion_comp, .by_group = TRUE) %>%
  mutate(
    ymax = cumsum(participacion),
    ymin = ymax - participacion,
    ymid = (ymin + ymax) / 2
  ) %>%
  ungroup()

anio_inicio_ocupacion <- min(serie_comp_ocupacion_area$anio, na.rm = TRUE)
anio_final_ocupacion  <- max(serie_comp_ocupacion_area$anio, na.rm = TRUE)

#--------------------------------------------------------
# 15.2. Etiquetas de inicio
# Solo porcentaje
#--------------------------------------------------------

labels_inicio_ocupacion <- serie_comp_ocupacion_area %>%
  filter(anio == anio_inicio_ocupacion) %>%
  mutate(
    anio_label = anio_inicio_ocupacion + 0.25,
    label = percent(participacion, accuracy = 0.1)
  )

#--------------------------------------------------------
# 15.3. Etiquetas finales
# Nombre + porcentaje
#--------------------------------------------------------

labels_final_ocupacion <- serie_comp_ocupacion_area %>%
  filter(anio == anio_final_ocupacion) %>%
  mutate(
    anio_label = anio_final_ocupacion + 0.35,
    label = paste0(
      stringr::str_wrap(as.character(ocupacion_comp), width = 22),
      ": ",
      percent(participacion, accuracy = 0.1)
    )
  )

#--------------------------------------------------------
# 15.4. Cambio en puntos porcentuales por categoría
#--------------------------------------------------------

cambio_comp_ocupacion <- serie_comp_ocupacion_area %>%
  filter(anio %in% c(anio_inicio_ocupacion, anio_final_ocupacion)) %>%
  select(anio, ocupacion_comp, participacion) %>%
  pivot_wider(
    names_from = anio,
    values_from = participacion,
    names_prefix = "y_"
  ) %>%
  mutate(
    participacion_inicio = .data[[paste0("y_", anio_inicio_ocupacion)]],
    participacion_final  = .data[[paste0("y_", anio_final_ocupacion)]],
    cambio_pp = 100 * (participacion_final - participacion_inicio),
    label_cambio = paste0(
      ifelse(cambio_pp >= 0, "+", ""),
      round(cambio_pp, 1),
      " p.p."
    )
  )

anio_label_cambio_ocupacion <- serie_comp_ocupacion_area$anio[
  which.min(abs(
    serie_comp_ocupacion_area$anio -
      (anio_inicio_ocupacion + 0.58 * (anio_final_ocupacion - anio_inicio_ocupacion))
  ))
]

labels_cambio_ocupacion <- serie_comp_ocupacion_area %>%
  filter(anio == anio_label_cambio_ocupacion) %>%
  select(anio, ocupacion_comp, ymid) %>%
  left_join(
    cambio_comp_ocupacion %>%
      select(ocupacion_comp, label_cambio),
    by = "ocupacion_comp"
  ) %>%
  mutate(
    x_label = anio_label_cambio_ocupacion,
    label = label_cambio
  )

#--------------------------------------------------------
# 15.5. Graficar
#--------------------------------------------------------

colores_ocupacion <- c(
  "Servicio doméstico"    = "#6D597A",
  "Cuenta propia"         = "#B56576",
  "Empleado particular"   = "darkblue",
  "Empleado gobierno"     = "#0B7285",
  "Patrón/empleador"      = "#E59F00",
  "Otras posiciones"      = "#4A4E69"
)

g_composicion_ocupacion_area <- ggplot(
  serie_comp_ocupacion_area,
  aes(
    x = anio,
    fill = ocupacion_comp
  )
) +
  geom_ribbon(
    aes(
      ymin = ymin,
      ymax = ymax,
      group = ocupacion_comp
    ),
    alpha = 0.96,
    linewidth = 0
  ) +
  
  # Líneas divisorias entre posiciones ocupacionales
  geom_line(
    aes(
      y = ymax,
      group = ocupacion_comp
    ),
    color = "white",
    linewidth = 0.65,
    alpha = 0.85
  ) +
  
  # Etiquetas internas al inicio: solo porcentaje
  geom_label(
    data = labels_inicio_ocupacion,
    aes(
      x = anio_label,
      y = ymid,
      label = label,
      fill = ocupacion_comp
    ),
    color = "white",
    fontface = "bold",
    size = 2.9,
    label.size = 0.12,
    label.padding = unit(0.12, "lines"),
    lineheight = 0.9,
    show.legend = FALSE
  ) +
  
  # Cambio en puntos porcentuales por categoría
  geom_label(
    data = labels_cambio_ocupacion,
    aes(
      x = x_label,
      y = ymid,
      label = label,
      fill = ocupacion_comp
    ),
    color = "white",
    fontface = "bold",
    size = 2.9,
    label.size = 0.12,
    label.padding = unit(0.12, "lines"),
    lineheight = 0.9,
    alpha = 0.98,
    show.legend = FALSE
  ) +
  
  # Etiquetas finales directas
  geom_text(
    data = labels_final_ocupacion,
    aes(
      x = anio_label,
      y = ymid,
      label = label,
      color = ocupacion_comp
    ),
    hjust = 0,
    fontface = "bold",
    size = 3.3,
    lineheight = 0.9,
    show.legend = FALSE
  ) +
  
  # Guías hacia etiquetas finales
  geom_segment(
    data = labels_final_ocupacion,
    aes(
      x = anio_final_ocupacion,
      xend = anio_label - 0.08,
      y = ymid,
      yend = ymid,
      color = ocupacion_comp
    ),
    linewidth = 0.55,
    alpha = 0.8,
    show.legend = FALSE
  ) +
  
  scale_fill_manual(
    values = colores_ocupacion
  ) +
  scale_color_manual(
    values = colores_ocupacion
  ) +
  scale_x_continuous(
    breaks = sort(unique(serie_comp_ocupacion_area$anio)),
    limits = c(anio_inicio_ocupacion, anio_final_ocupacion + 2.6)
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 1),
    breaks = seq(0, 1, 0.25),
    expand = expansion(mult = c(0, 0))
  ) +
  coord_cartesian(clip = "off") +
  labs(
    title = paste0(
      "Composición porcentual del número de trabajadores por posición ocupacional, ",
      anio_inicio_ocupacion,
      "–",
      anio_final_ocupacion
    ),
    subtitle = "Participación porcentual sobre el total de ocupados. Cálculo ponderado por factores de expansión",
    x = "",
    y = "Participación en el total de trabajadores",
    fill = NULL,
    color = NULL
  ) +
  theme_classic(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    axis.text.y = element_text(size = 10),
    legend.position = "none",
    panel.grid.major.y = element_line(color = "gray88", linewidth = 0.35),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 185, 10, 10)
  )

g_composicion_ocupacion_area

#========================================================
# GRÁFICO 16. Ingreso por departamento: año inicial vs año final
# Barras en niveles
#========================================================

#--------------------------------------------------------
# 16.1. Preparar datos
#--------------------------------------------------------

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

anio_inicio_depto <- min(geih_ingreso$anio, na.rm = TRUE)
anio_final_depto  <- max(geih_ingreso$anio, na.rm = TRUE)

serie_depto_inicio_final <- geih_ingreso %>%
  filter(
    anio %in% c(anio_inicio_depto, anio_final_depto),
    depto_label %in% deptos_seleccionados
  ) %>%
  group_by(anio, depto_label) %>%
  summarise(
    observaciones = n(),
    trabajadores_expandidos = sum(fex, na.rm = TRUE),
    ingreso_hora_real_promedio = weighted_mean(ingreso_hora_real, fex),
    .groups = "drop"
  ) %>%
  group_by(depto_label) %>%
  filter(n_distinct(anio) == 2) %>%
  ungroup()

orden_depto_final <- serie_depto_inicio_final %>%
  filter(anio == anio_final_depto) %>%
  arrange(ingreso_hora_real_promedio) %>%
  pull(depto_label)

serie_depto_inicio_final <- serie_depto_inicio_final %>%
  mutate(
    depto_label = factor(depto_label, levels = orden_depto_final),
    anio = factor(
      anio,
      levels = c(anio_inicio_depto, anio_final_depto)
    )
  )

colores_anios_depto <- setNames(
  c("darkred", "darkblue"),
  c(as.character(anio_inicio_depto), as.character(anio_final_depto))
)

#--------------------------------------------------------
# 16.2. Graficar
#--------------------------------------------------------

g_barras_depto_inicio_final <- ggplot(
  serie_depto_inicio_final,
  aes(
    x = depto_label,
    y = ingreso_hora_real_promedio,
    fill = anio
  )
) +
  geom_col(
    position = position_dodge(width = 0.74),
    width = 0.62,
    alpha = 0.95
  ) +
  geom_label(
    aes(
      label = comma(ingreso_hora_real_promedio, accuracy = 1),
      fill = anio
    ),
    position = position_dodge(width = 0.74),
    color = "white",
    fontface = "bold",
    size = 3.0,
    label.size = 0.13,
    label.padding = unit(0.13, "lines"),
    hjust = -0.08,
    show.legend = FALSE
  ) +
  coord_flip(clip = "off") +
  scale_fill_manual(
    values = colores_anios_depto
  ) +
  scale_y_continuous(
    labels = comma,
    expand = expansion(mult = c(0.02, 0.22))
  ) +
  labs(
    title = paste0(
      "Ingreso laboral por hora real por departamento: ",
      anio_inicio_depto,
      " vs. ",
      anio_final_depto
    ),
    subtitle = "Departamentos seleccionados. Pesos constantes de 2025. Promedio ponderado por factores de expansión",
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
    legend.title = element_text(face = "bold"),
    legend.text = element_text(face = "bold"),
    panel.grid.major.x = element_line(color = "gray90", linewidth = 0.35),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 90, 10, 10)
  )

g_barras_depto_inicio_final

#========================================================
# GRÁFICO 17. Crecimiento del ingreso por departamento
# Lollipop - Índice año inicial = 100
# Etiqueta: crecimiento anualizado
#========================================================

#--------------------------------------------------------
# 17.1. Preparar datos
#--------------------------------------------------------

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

serie_depto_anual <- geih_ingreso %>%
  filter(
    !is.na(depto_label),
    depto_label %in% deptos_seleccionados
  ) %>%
  group_by(anio, depto_label) %>%
  summarise(
    observaciones = n(),
    trabajadores_expandidos = sum(fex, na.rm = TRUE),
    ingreso_hora_real_promedio = weighted_mean(ingreso_hora_real, fex),
    .groups = "drop"
  ) %>%
  arrange(depto_label, anio)

anio_inicio_depto <- min(serie_depto_anual$anio, na.rm = TRUE)
anio_final_depto  <- max(serie_depto_anual$anio, na.rm = TRUE)

depto_crecimiento <- serie_depto_anual %>%
  filter(anio %in% c(anio_inicio_depto, anio_final_depto)) %>%
  select(anio, depto_label, ingreso_hora_real_promedio) %>%
  pivot_wider(
    names_from = anio,
    values_from = ingreso_hora_real_promedio,
    names_prefix = "y_"
  ) %>%
  filter(
    !is.na(.data[[paste0("y_", anio_inicio_depto)]]),
    !is.na(.data[[paste0("y_", anio_final_depto)]])
  ) %>%
  mutate(
    ingreso_inicio = .data[[paste0("y_", anio_inicio_depto)]],
    ingreso_final  = .data[[paste0("y_", anio_final_depto)]],
    
    indice_inicio = 100,
    indice_final = 100 * ingreso_final / ingreso_inicio,
    
    crecimiento_anualizado = 100 * (
      (ingreso_final / ingreso_inicio)^(1 / (anio_final_depto - anio_inicio_depto)) - 1
    ),
    
    grupo_crecimiento = case_when(
      crecimiento_anualizado < 0    ~ "Decrecimiento",
      crecimiento_anualizado < 0.75 ~ "Crecimiento bajo",
      crecimiento_anualizado < 1.50 ~ "Crecimiento medio",
      TRUE                          ~ "Crecimiento alto"
    ),
    grupo_crecimiento = factor(
      grupo_crecimiento,
      levels = c(
        "Crecimiento alto",
        "Crecimiento medio",
        "Crecimiento bajo",
        "Decrecimiento"
      )
    ),
    
    label_final = paste0(
      round(indice_final, 1),
      " | ",
      ifelse(crecimiento_anualizado >= 0, "+", ""),
      round(crecimiento_anualizado, 2),
      "% anual"
    ),
    
    depto_label = reorder(depto_label, indice_final)
  )

#--------------------------------------------------------
# 17.2. Graficar
#--------------------------------------------------------

g_lollipop_depto_crecimiento <- ggplot(
  depto_crecimiento,
  aes(
    y = depto_label,
    x = indice_final
  )
) +
  geom_vline(
    xintercept = 100,
    linetype = "dashed",
    color = "gray45",
    linewidth = 0.7
  ) +
  geom_segment(
    aes(
      x = 100,
      xend = indice_final,
      y = depto_label,
      yend = depto_label,
      color = grupo_crecimiento
    ),
    linewidth = 1.5,
    lineend = "round"
  ) +
  geom_point(
    aes(color = grupo_crecimiento),
    size = 4.5
  ) +
  geom_text(
    data = depto_crecimiento %>% filter(indice_final >= 100),
    aes(label = label_final),
    hjust = -0.10,
    size = 3.3,
    fontface = "bold"
  ) +
  geom_text(
    data = depto_crecimiento %>% filter(indice_final < 100),
    aes(label = label_final),
    hjust = 1.10,
    size = 3.3,
    fontface = "bold"
  ) +
  scale_color_manual(
    values = c(
      "Crecimiento alto"  = "#0B7285",
      "Crecimiento medio" = "#E59F00",
      "Crecimiento bajo"  = "#A61E4D",
      "Decrecimiento"     = "#8B0000"
    )
  ) +
  scale_x_continuous(
    limits = c(
      min(95, min(depto_crecimiento$indice_final, na.rm = TRUE) - 8),
      max(depto_crecimiento$indice_final, na.rm = TRUE) + 26
    ),
    breaks = seq(70, 200, 10)
  ) +
  labs(
    title = paste0(
      "Crecimiento del ingreso laboral por hora real por departamento, ",
      anio_inicio_depto,
      "–",
      anio_final_depto
    ),
    subtitle = paste0(
      "Índice base ",
      anio_inicio_depto,
      " = 100. La etiqueta muestra el crecimiento anualizado"
    ),
    x = paste0(
      "Índice del ingreso laboral por hora, ",
      anio_inicio_depto,
      " = 100"
    ),
    y = "Departamento",
    color = "Grupo"
  ) +
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(face = "bold"),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(face = "bold"),
    panel.grid.major.x = element_line(color = "gray90", linewidth = 0.35),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 120, 10, 10)
  )

g_lollipop_depto_crecimiento

#========================================================
# GRÁFICO 18. Composición porcentual por departamento
# Área apilada 100% incluyendo otros departamentos
#========================================================

#--------------------------------------------------------
# 18.1. Preparar datos
#--------------------------------------------------------

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

orden_depto_comp <- c(
  "Bogotá D.C.",
  "Antioquia",
  "Valle del Cauca",
  "Atlántico",
  "Santander",
  "Bolívar",
  "Córdoba",
  "Norte de Santander",
  "Nariño",
  "Tolima",
  "Meta",
  "Caldas",
  "Risaralda",
  "Otros departamentos"
)

serie_comp_depto <- geih %>%
  filter(
    !is.na(anio),
    !is.na(depto_label),
    !is.na(fex),
    fex > 0
  ) %>%
  mutate(
    depto_comp = case_when(
      depto_label %in% deptos_seleccionados ~ depto_label,
      TRUE ~ "Otros departamentos"
    ),
    depto_comp = factor(
      depto_comp,
      levels = orden_depto_comp
    )
  ) %>%
  filter(!is.na(depto_comp)) %>%
  group_by(anio, depto_comp) %>%
  summarise(
    observaciones = n(),
    trabajadores_expandidos = sum(fex, na.rm = TRUE),
    trabajadores_millones = trabajadores_expandidos / 1e6,
    .groups = "drop"
  ) %>%
  group_by(anio) %>%
  mutate(
    total_trabajadores = sum(trabajadores_millones, na.rm = TRUE),
    participacion = trabajadores_millones / total_trabajadores
  ) %>%
  ungroup() %>%
  arrange(anio, depto_comp)

serie_comp_depto_area <- serie_comp_depto %>%
  group_by(anio) %>%
  arrange(depto_comp, .by_group = TRUE) %>%
  mutate(
    ymax = cumsum(participacion),
    ymin = ymax - participacion,
    ymid = (ymin + ymax) / 2
  ) %>%
  ungroup()

anio_inicio_depto <- min(serie_comp_depto_area$anio, na.rm = TRUE)
anio_final_depto  <- max(serie_comp_depto_area$anio, na.rm = TRUE)

#--------------------------------------------------------
# 18.2. Etiquetas de inicio
# Solo porcentaje
#--------------------------------------------------------

labels_inicio_depto <- serie_comp_depto_area %>%
  filter(anio == anio_inicio_depto) %>%
  mutate(
    anio_label = anio_inicio_depto + 0.25,
    label = percent(participacion, accuracy = 0.1)
  ) %>%
  filter(participacion >= 0.025)

#--------------------------------------------------------
# 18.3. Etiquetas finales
# Nombre + porcentaje
#--------------------------------------------------------

labels_final_depto <- serie_comp_depto_area %>%
  filter(anio == anio_final_depto) %>%
  mutate(
    anio_label = anio_final_depto + 0.35,
    label = paste0(
      stringr::str_wrap(as.character(depto_comp), width = 20),
      ": ",
      percent(participacion, accuracy = 0.1)
    )
  )

#--------------------------------------------------------
# 18.4. Cambio en puntos porcentuales por departamento
#--------------------------------------------------------

cambio_comp_depto <- serie_comp_depto_area %>%
  filter(anio %in% c(anio_inicio_depto, anio_final_depto)) %>%
  select(anio, depto_comp, participacion) %>%
  pivot_wider(
    names_from = anio,
    values_from = participacion,
    names_prefix = "y_"
  ) %>%
  mutate(
    participacion_inicio = .data[[paste0("y_", anio_inicio_depto)]],
    participacion_final  = .data[[paste0("y_", anio_final_depto)]],
    cambio_pp = 100 * (participacion_final - participacion_inicio),
    label_cambio = paste0(
      ifelse(cambio_pp >= 0, "+", ""),
      round(cambio_pp, 1),
      " p.p."
    )
  )

anio_label_cambio_depto <- serie_comp_depto_area$anio[
  which.min(abs(
    serie_comp_depto_area$anio -
      (anio_inicio_depto + 0.58 * (anio_final_depto - anio_inicio_depto))
  ))
]

labels_cambio_depto <- serie_comp_depto_area %>%
  filter(anio == anio_label_cambio_depto) %>%
  select(anio, depto_comp, ymid, participacion) %>%
  left_join(
    cambio_comp_depto %>%
      select(depto_comp, label_cambio),
    by = "depto_comp"
  ) %>%
  mutate(
    x_label = anio_label_cambio_depto,
    label = label_cambio
  ) %>%
  filter(participacion >= 0.025)

#--------------------------------------------------------
# 18.5. Graficar
#--------------------------------------------------------

colores_depto <- c(
  "Bogotá D.C."           = "darkblue",
  "Antioquia"             = "#0B7285",
  "Valle del Cauca"       = "#E59F00",
  "Atlántico"             = "#A61E4D",
  "Santander"             = "#6D597A",
  "Bolívar"               = "#B56576",
  "Córdoba"               = "#8C1C13",
  "Norte de Santander"    = "#457B9D",
  "Nariño"                = "#2A9D8F",
  "Tolima"                = "#F4A261",
  "Meta"                  = "#7B2CBF",
  "Caldas"                = "#495057",
  "Risaralda"             = "#2F3E46",
  "Otros departamentos"   = "#ADB5BD"
)

g_composicion_depto_area <- ggplot(
  serie_comp_depto_area,
  aes(
    x = anio,
    fill = depto_comp
  )
) +
  geom_ribbon(
    aes(
      ymin = ymin,
      ymax = ymax,
      group = depto_comp
    ),
    alpha = 0.96,
    linewidth = 0
  ) +
  
  geom_line(
    aes(
      y = ymax,
      group = depto_comp
    ),
    color = "white",
    linewidth = 0.55,
    alpha = 0.75
  ) +
  
  # Etiquetas internas al inicio: solo porcentaje
  geom_label(
    data = labels_inicio_depto,
    aes(
      x = anio_label,
      y = ymid,
      label = label,
      fill = depto_comp
    ),
    color = "white",
    fontface = "bold",
    size = 2.7,
    label.size = 0.10,
    label.padding = unit(0.10, "lines"),
    lineheight = 0.9,
    show.legend = FALSE
  ) +
  
  # Cambio en puntos porcentuales
  geom_label(
    data = labels_cambio_depto,
    aes(
      x = x_label,
      y = ymid,
      label = label,
      fill = depto_comp
    ),
    color = "white",
    fontface = "bold",
    size = 2.7,
    label.size = 0.10,
    label.padding = unit(0.10, "lines"),
    lineheight = 0.9,
    alpha = 0.98,
    show.legend = FALSE
  ) +
  
  # Etiquetas finales directas
  geom_text(
    data = labels_final_depto,
    aes(
      x = anio_label,
      y = ymid,
      label = label,
      color = depto_comp
    ),
    hjust = 0,
    fontface = "bold",
    size = 3.0,
    lineheight = 0.9,
    show.legend = FALSE
  ) +
  
  # Guías hacia etiquetas finales
  geom_segment(
    data = labels_final_depto,
    aes(
      x = anio_final_depto,
      xend = anio_label - 0.08,
      y = ymid,
      yend = ymid,
      color = depto_comp
    ),
    linewidth = 0.45,
    alpha = 0.75,
    show.legend = FALSE
  ) +
  
  scale_fill_manual(
    values = colores_depto
  ) +
  scale_color_manual(
    values = colores_depto
  ) +
  scale_x_continuous(
    breaks = sort(unique(serie_comp_depto_area$anio)),
    limits = c(anio_inicio_depto, anio_final_depto + 3.2)
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 1),
    breaks = seq(0, 1, 0.25),
    expand = expansion(mult = c(0, 0))
  ) +
  coord_cartesian(clip = "off") +
  labs(
    title = paste0(
      "Composición porcentual del número de trabajadores por departamento, ",
      anio_inicio_depto,
      "–",
      anio_final_depto
    ),
    subtitle = "Participación porcentual sobre el total de ocupados. Departamentos seleccionados y otros departamentos",
    x = "",
    y = "Participación en el total de trabajadores",
    fill = NULL,
    color = NULL
  ) +
  theme_classic(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    axis.text.y = element_text(size = 10),
    legend.position = "none",
    panel.grid.major.y = element_line(color = "gray88", linewidth = 0.35),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 230, 10, 10)
  )

g_composicion_depto_area

#========================================================
# GRÁFICO 19A y 19B. Ingreso por departamento
# 24 departamentos: 2013 y 2025 por separado
#========================================================

#--------------------------------------------------------
# 19.1. Departamentos seleccionados
#--------------------------------------------------------

deptos_24_info <- tibble::tribble(
  ~depto, ~depto_label_24,
  5, "Antioquia",
  8, "Atlántico",
  11, "Bogotá D.C.",
  13, "Bolívar",
  15, "Boyacá",
  17, "Caldas",
  18, "Caquetá",
  19, "Cauca",
  20, "Cesar",
  23, "Córdoba",
  25, "Cundinamarca",
  27, "Chocó",
  41, "Huila",
  44, "La Guajira",
  47, "Magdalena",
  50, "Meta",
  52, "Nariño",
  54, "Norte de Santander",
  63, "Quindío",
  66, "Risaralda",
  68, "Santander",
  70, "Sucre",
  73, "Tolima",
  76, "Valle del Cauca"
)

anio_inicio_24 <- 2013

anio_final_24 <- geih_ingreso %>%
  filter(
    anio >= anio_inicio_24,
    depto %in% deptos_24_info$depto
  ) %>%
  summarise(
    anio_final = max(anio, na.rm = TRUE)
  ) %>%
  pull(anio_final)

#--------------------------------------------------------
# 19.2. Preparar datos
#--------------------------------------------------------

serie_depto_24_niveles <- geih_ingreso %>%
  filter(
    anio %in% c(anio_inicio_24, anio_final_24),
    depto %in% deptos_24_info$depto
  ) %>%
  group_by(anio, depto) %>%
  summarise(
    observaciones = n(),
    trabajadores_expandidos = sum(fex, na.rm = TRUE),
    ingreso_hora_real_promedio = weighted_mean(ingreso_hora_real, fex),
    .groups = "drop"
  ) %>%
  left_join(deptos_24_info, by = "depto") %>%
  group_by(depto_label_24) %>%
  filter(n_distinct(anio) == 2) %>%
  ungroup()

max_eje_depto_24 <- max(
  serie_depto_24_niveles$ingreso_hora_real_promedio,
  na.rm = TRUE
) * 1.18

#--------------------------------------------------------
# 19.3. Función para graficar un año
#--------------------------------------------------------

crear_grafico_depto_nivel <- function(data, anio_objetivo, color_barra) {
  
  data_plot <- data %>%
    filter(anio == anio_objetivo) %>%
    arrange(ingreso_hora_real_promedio) %>%
    mutate(
      depto_label_24 = factor(
        depto_label_24,
        levels = depto_label_24
      )
    )
  
  ggplot(
    data_plot,
    aes(
      x = depto_label_24,
      y = ingreso_hora_real_promedio
    )
  ) +
    geom_col(
      fill = color_barra,
      width = 0.62,
      alpha = 0.95
    ) +
    geom_label(
      aes(
        label = comma(ingreso_hora_real_promedio, accuracy = 1)
      ),
      fill = color_barra,
      color = "white",
      fontface = "bold",
      size = 2.8,
      label.size = 0.13,
      label.padding = unit(0.12, "lines"),
      hjust = -0.08,
      show.legend = FALSE
    ) +
    coord_flip(clip = "off") +
    scale_y_continuous(
      labels = comma,
      limits = c(0, max_eje_depto_24),
      expand = expansion(mult = c(0.02, 0.02))
    ) +
    labs(
      title = paste0(
        "Ingreso laboral por hora real por departamento, ",
        anio_objetivo
      ),
      subtitle = "24 departamentos seleccionados. Pesos constantes de 2025. Promedio ponderado por factores de expansión",
      x = "Departamento",
      y = "Ingreso laboral por hora promedio"
    ) +
    theme_classic(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 15),
      plot.subtitle = element_text(size = 11),
      axis.title = element_text(face = "bold"),
      axis.text.y = element_text(size = 9.5),
      axis.text.x = element_text(size = 10),
      panel.grid.major.x = element_line(color = "gray90", linewidth = 0.35),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      plot.margin = margin(10, 90, 10, 10)
    )
}

#--------------------------------------------------------
# 19.4. Graficar 2013 y 2025 por separado
#--------------------------------------------------------

g_barras_depto_24_2013 <- crear_grafico_depto_nivel(
  data = serie_depto_24_niveles,
  anio_objetivo = anio_inicio_24,
  color_barra = "darkred"
)

g_barras_depto_24_2025 <- crear_grafico_depto_nivel(
  data = serie_depto_24_niveles,
  anio_objetivo = anio_final_24,
  color_barra = "darkblue"
)

g_barras_depto_24_2013
g_barras_depto_24_2025

#========================================================
# GRÁFICO 20. Crecimiento del ingreso por departamento
# Lollipop - Índice 2013 = 100
# Etiqueta: crecimiento anualizado
#========================================================

#--------------------------------------------------------
# 20.1. Preparar datos
#--------------------------------------------------------

serie_depto_24_anual <- geih_ingreso %>%
  filter(
    anio >= anio_inicio_24,
    depto %in% deptos_24_info$depto
  ) %>%
  group_by(anio, depto) %>%
  summarise(
    observaciones = n(),
    trabajadores_expandidos = sum(fex, na.rm = TRUE),
    ingreso_hora_real_promedio = weighted_mean(ingreso_hora_real, fex),
    .groups = "drop"
  ) %>%
  left_join(deptos_24_info, by = "depto") %>%
  arrange(depto_label_24, anio)

depto_24_crecimiento <- serie_depto_24_anual %>%
  filter(anio %in% c(anio_inicio_24, anio_final_24)) %>%
  select(anio, depto_label_24, ingreso_hora_real_promedio) %>%
  pivot_wider(
    names_from = anio,
    values_from = ingreso_hora_real_promedio,
    names_prefix = "y_"
  ) %>%
  filter(
    !is.na(.data[[paste0("y_", anio_inicio_24)]]),
    !is.na(.data[[paste0("y_", anio_final_24)]])
  ) %>%
  mutate(
    ingreso_inicio = .data[[paste0("y_", anio_inicio_24)]],
    ingreso_final  = .data[[paste0("y_", anio_final_24)]],
    indice_inicio = 100,
    indice_final = 100 * ingreso_final / ingreso_inicio,
    crecimiento_anualizado = 100 * (
      (ingreso_final / ingreso_inicio)^(1 / (anio_final_24 - anio_inicio_24)) - 1
    ),
    grupo_crecimiento = case_when(
      crecimiento_anualizado < 0    ~ "Decrecimiento",
      crecimiento_anualizado < 0.75 ~ "Crecimiento bajo",
      crecimiento_anualizado < 1.50 ~ "Crecimiento medio",
      TRUE                          ~ "Crecimiento alto"
    ),
    grupo_crecimiento = factor(
      grupo_crecimiento,
      levels = c(
        "Crecimiento alto",
        "Crecimiento medio",
        "Crecimiento bajo",
        "Decrecimiento"
      )
    ),
    label_final = paste0(
      round(indice_final, 1),
      " | ",
      ifelse(crecimiento_anualizado >= 0, "+", ""),
      round(crecimiento_anualizado, 2),
      "% anual"
    ),
    depto_label_24 = reorder(depto_label_24, indice_final)
  )

#--------------------------------------------------------
# 20.2. Graficar
#--------------------------------------------------------

g_lollipop_depto_24_crecimiento <- ggplot(
  depto_24_crecimiento,
  aes(
    y = depto_label_24,
    x = indice_final
  )
) +
  geom_vline(
    xintercept = 100,
    linetype = "dashed",
    color = "gray45",
    linewidth = 0.7
  ) +
  geom_segment(
    aes(
      x = 100,
      xend = indice_final,
      y = depto_label_24,
      yend = depto_label_24,
      color = grupo_crecimiento
    ),
    linewidth = 1.5,
    lineend = "round"
  ) +
  geom_point(
    aes(color = grupo_crecimiento),
    size = 4.3
  ) +
  geom_text(
    data = depto_24_crecimiento %>% filter(indice_final >= 100),
    aes(label = label_final),
    hjust = -0.10,
    size = 3.1,
    fontface = "bold"
  ) +
  geom_text(
    data = depto_24_crecimiento %>% filter(indice_final < 100),
    aes(label = label_final),
    hjust = 1.10,
    size = 3.1,
    fontface = "bold"
  ) +
  scale_color_manual(
    values = c(
      "Crecimiento alto"  = "#0B7285",
      "Crecimiento medio" = "#E59F00",
      "Crecimiento bajo"  = "#A61E4D",
      "Decrecimiento"     = "#8B0000"
    )
  ) +
  scale_x_continuous(
    limits = c(
      min(95, min(depto_24_crecimiento$indice_final, na.rm = TRUE) - 8),
      max(depto_24_crecimiento$indice_final, na.rm = TRUE) + 24
    ),
    breaks = seq(70, 200, 10)
  ) +
  labs(
    title = paste0(
      "Crecimiento del ingreso laboral por hora real por departamento, ",
      anio_inicio_24, "–", anio_final_24
    ),
    subtitle = paste0(
      "Índice base ", anio_inicio_24,
      " = 100. La etiqueta muestra el crecimiento anualizado"
    ),
    x = paste0(
      "Índice del ingreso laboral por hora, ",
      anio_inicio_24, " = 100"
    ),
    y = "Departamento",
    color = "Grupo"
  ) +
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(face = "bold"),
    axis.text.y = element_text(size = 9.5),
    axis.text.x = element_text(size = 10),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(face = "bold"),
    panel.grid.major.x = element_line(color = "gray90", linewidth = 0.35),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 120, 10, 10)
  )

g_lollipop_depto_24_crecimiento

#========================================================
# GRÁFICO 21. Cambio en la composición por departamento
# Dumbbell: participación 2013 vs 2025
#========================================================

#--------------------------------------------------------
# 21B.1. Departamentos seleccionados
#--------------------------------------------------------

deptos_24_info <- tibble::tribble(
  ~depto, ~depto_label_24,
  5, "Antioquia",
  8, "Atlántico",
  11, "Bogotá D.C.",
  13, "Bolívar",
  15, "Boyacá",
  17, "Caldas",
  18, "Caquetá",
  19, "Cauca",
  20, "Cesar",
  23, "Córdoba",
  25, "Cundinamarca",
  27, "Chocó",
  41, "Huila",
  44, "La Guajira",
  47, "Magdalena",
  50, "Meta",
  52, "Nariño",
  54, "Norte de Santander",
  63, "Quindío",
  66, "Risaralda",
  68, "Santander",
  70, "Sucre",
  73, "Tolima",
  76, "Valle del Cauca"
)

anio_inicio_24 <- 2013

anio_final_24 <- geih %>%
  filter(
    anio >= anio_inicio_24,
    depto %in% deptos_24_info$depto
  ) %>%
  summarise(
    anio_final = max(anio, na.rm = TRUE)
  ) %>%
  pull(anio_final)

#--------------------------------------------------------
# 21B.2. Preparar datos
#--------------------------------------------------------

#--------------------------------------------------------
# 21B.2. Preparar datos ajustados
#--------------------------------------------------------

serie_comp_depto_24_dumbbell <- geih %>%
  filter(
    !is.na(anio),
    anio %in% c(anio_inicio_24, anio_final_24),
    depto %in% deptos_24_info$depto,
    !is.na(fex),
    fex > 0
  ) %>%
  group_by(anio, depto) %>%
  summarise(
    observaciones = n(),
    trabajadores_expandidos = sum(fex, na.rm = TRUE),
    trabajadores_millones = trabajadores_expandidos / 1e6,
    .groups = "drop"
  ) %>%
  left_join(deptos_24_info, by = "depto") %>%
  group_by(anio) %>%
  mutate(
    total_trabajadores = sum(trabajadores_millones, na.rm = TRUE),
    participacion = trabajadores_millones / total_trabajadores
  ) %>%
  ungroup() %>%
  select(anio, depto_label_24, participacion) %>%
  pivot_wider(
    names_from = anio,
    values_from = participacion,
    names_prefix = "p_"
  ) %>%
  mutate(
    participacion_inicio = .data[[paste0("p_", anio_inicio_24)]],
    participacion_final  = .data[[paste0("p_", anio_final_24)]],
    
    cambio_pp = 100 * (participacion_final - participacion_inicio),
    
    label_cambio = paste0(
      ifelse(cambio_pp >= 0, "+", ""),
      round(cambio_pp, 1),
      " p.p."
    ),
    
    grupo_cambio = case_when(
      cambio_pp > 0.3  ~ "Aumentó participación",
      cambio_pp < -0.3 ~ "Redujo participación",
      TRUE             ~ "Cambio bajo"
    ),
    grupo_cambio = factor(
      grupo_cambio,
      levels = c(
        "Aumentó participación",
        "Cambio bajo",
        "Redujo participación"
      )
    ),
    
    x_min = pmin(100 * participacion_inicio, 100 * participacion_final),
    x_max = pmax(100 * participacion_inicio, 100 * participacion_final),
    
    # Más separación para que no tape los puntos
    x_label = x_max + 1.00,
    
    depto_label_24 = reorder(depto_label_24, participacion_final)
  )

# Datos largos solo para los puntos de año
puntos_depto_24 <- serie_comp_depto_24_dumbbell %>%
  select(
    depto_label_24,
    participacion_inicio,
    participacion_final
  ) %>%
  pivot_longer(
    cols = c(participacion_inicio, participacion_final),
    names_to = "anio_tipo",
    values_to = "participacion"
  ) %>%
  mutate(
    anio = case_when(
      anio_tipo == "participacion_inicio" ~ as.character(anio_inicio_24),
      anio_tipo == "participacion_final"  ~ as.character(anio_final_24)
    ),
    anio = factor(
      anio,
      levels = c(as.character(anio_inicio_24), as.character(anio_final_24))
    )
  )

#--------------------------------------------------------
# 21B.3. Graficar versión más simple e intuitiva
#--------------------------------------------------------

colores_cambio_depto <- c(
  "Aumentó participación" = "#1B9E77",
  "Cambio bajo"           = "#9AA5B1",
  "Redujo participación"  = "#D62828"
)

colores_anio_depto <- setNames(
  c("#8ECAE6", "#1D4ED8"),   # 2013 azul claro, 2025 azul oscuro
  c(as.character(anio_inicio_24), as.character(anio_final_24))
)

g_dumbbell_comp_depto_24 <- ggplot(
  serie_comp_depto_24_dumbbell,
  aes(y = depto_label_24)
) +
  # Línea principal en gris neutro
  geom_segment(
    aes(
      x = 100 * participacion_inicio,
      xend = 100 * participacion_final,
      yend = depto_label_24
    ),
    color = "gray60",
    linewidth = 1.15,
    alpha = 0.9,
    lineend = "round"
  ) +
  
  # Línea corta desde el punto final hacia la etiqueta
  geom_segment(
    aes(
      x = x_max,
      xend = x_label - 0.10,
      yend = depto_label_24
    ),
    color = "gray70",
    linewidth = 0.65,
    alpha = 0.85,
    lineend = "round"
  ) +
  
  # Puntos de ambos años
  geom_point(
    data = puntos_depto_24,
    aes(
      x = 100 * participacion,
      y = depto_label_24,
      fill = anio
    ),
    shape = 21,
    color = "white",
    stroke = 0.8,
    size = 4.0,
    alpha = 0.98,
    inherit.aes = FALSE
  ) +
  
  # Etiqueta del cambio
  geom_label(
    aes(
      x = x_label,
      label = label_cambio,
      fill = grupo_cambio
    ),
    color = "white",
    fontface = "bold",
    size = 2.8,
    label.size = 0.10,
    label.padding = unit(0.11, "lines"),
    show.legend = FALSE
  ) +
  
  scale_fill_manual(
    values = c(
      colores_anio_depto,
      colores_cambio_depto
    ),
    breaks = c(
      as.character(anio_inicio_24),
      as.character(anio_final_24)
    ),
    name = "Año"
  ) +
  scale_x_continuous(
    labels = function(x) paste0(number(x, accuracy = 0.1), "%"),
    expand = expansion(mult = c(0.04, 0.22))
  ) +
  labs(
    title = paste0(
      "Cambio en la composición del número de trabajadores por departamento, ",
      anio_inicio_24,
      " vs. ",
      anio_final_24
    ),
    subtitle = paste0(
      "Participación porcentual sobre el total de ocupados en los 24 departamentos seleccionados. ",
      anio_inicio_24,
      " en azul claro y ",
      anio_final_24,
      " en azul oscuro"
    ),
    x = "Participación en el total de trabajadores",
    y = "Departamento"
  ) +
  guides(
    fill = guide_legend(
      order = 1,
      override.aes = list(shape = 21, size = 4, color = "white")
    )
  ) +
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(face = "bold"),
    axis.text.y = element_text(size = 9.5),
    axis.text.x = element_text(size = 10),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(face = "bold"),
    panel.grid.major.x = element_line(color = "gray90", linewidth = 0.35),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 125, 10, 10)
  )

g_dumbbell_comp_depto_24

#========================================================
# CONFIGURACIÓN GENERAL: SECTORES RAMA2D
#========================================================

sectores_excluir <- c(
  "Extraterritoriales",
  "Organizaciones extraterritoriales",
  "Actividades de organizaciones y órganos extraterritoriales"
)

orden_sector_base <- c(
  "Agricultura",
  "Minas",
  "Manufactura",
  "Servicios públicos",
  "Construcción",
  "Comercio",
  "Alojamiento y comida",
  "Transporte",
  "Información y comunicaciones",
  "Financieras",
  "Inmobiliarias/profesionales",
  "Adm. pública",
  "Educación",
  "Salud",
  "Artes y otros servicios",
  "Hogares empleadores"
)

#========================================================
# GRÁFICO 22. Ingreso por sector Rama2D: año inicial vs año final
# Barras en niveles
#========================================================

#--------------------------------------------------------
# 1.1. Preparar datos
#--------------------------------------------------------

anio_inicio_sector <- min(geih_ingreso$anio, na.rm = TRUE)
anio_final_sector  <- max(geih_ingreso$anio, na.rm = TRUE)

serie_sector_inicio_final <- geih_ingreso %>%
  filter(
    anio %in% c(anio_inicio_sector, anio_final_sector),
    !is.na(sector_label),
    !(sector_label %in% sectores_excluir)
  ) %>%
  group_by(anio, sector_label) %>%
  summarise(
    observaciones = n(),
    trabajadores_expandidos = sum(fex, na.rm = TRUE),
    ingreso_hora_real_promedio = weighted_mean(ingreso_hora_real, fex),
    .groups = "drop"
  ) %>%
  group_by(sector_label) %>%
  filter(n_distinct(anio) == 2) %>%
  ungroup()

orden_sector_final <- serie_sector_inicio_final %>%
  filter(anio == anio_final_sector) %>%
  arrange(ingreso_hora_real_promedio) %>%
  pull(sector_label)

serie_sector_inicio_final <- serie_sector_inicio_final %>%
  mutate(
    sector_label = factor(sector_label, levels = orden_sector_final),
    anio = factor(
      anio,
      levels = c(anio_inicio_sector, anio_final_sector)
    )
  )

colores_anios_sector <- setNames(
  c("darkred", "darkblue"),
  c(as.character(anio_inicio_sector), as.character(anio_final_sector))
)

#--------------------------------------------------------
# 1.2. Graficar
#--------------------------------------------------------

g_barras_sector_inicio_final <- ggplot(
  serie_sector_inicio_final,
  aes(
    x = sector_label,
    y = ingreso_hora_real_promedio,
    fill = anio
  )
) +
  geom_col(
    position = position_dodge(width = 0.74),
    width = 0.62,
    alpha = 0.95
  ) +
  geom_label(
    aes(
      label = comma(ingreso_hora_real_promedio, accuracy = 1),
      fill = anio
    ),
    position = position_dodge(width = 0.74),
    color = "white",
    fontface = "bold",
    size = 2.9,
    label.size = 0.13,
    label.padding = unit(0.12, "lines"),
    hjust = -0.08,
    show.legend = FALSE
  ) +
  coord_flip(clip = "off") +
  scale_fill_manual(
    values = colores_anios_sector
  ) +
  scale_y_continuous(
    labels = comma,
    expand = expansion(mult = c(0.02, 0.22))
  ) +
  labs(
    title = paste0(
      "Ingreso laboral por hora real por sector económico: ",
      anio_inicio_sector,
      " vs. ",
      anio_final_sector
    ),
    subtitle = "Sectores Rama2D. Excluye organizaciones extraterritoriales. Pesos constantes de 2025",
    x = "Sector económico",
    y = "Ingreso laboral por hora promedio",
    fill = "Año"
  ) +
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(face = "bold"),
    axis.text.y = element_text(size = 9.5),
    axis.text.x = element_text(size = 10),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(face = "bold"),
    panel.grid.major.x = element_line(color = "gray90", linewidth = 0.35),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 100, 10, 10)
  )

g_barras_sector_inicio_final

#========================================================
# GRÁFICO 23. Crecimiento del ingreso por sector Rama2D
# Lollipop - Índice año inicial = 100
# Etiqueta: crecimiento anualizado
#========================================================

#--------------------------------------------------------
# 2.1. Preparar datos
#--------------------------------------------------------

serie_sector_anual <- geih_ingreso %>%
  filter(
    !is.na(sector_label),
    !(sector_label %in% sectores_excluir)
  ) %>%
  group_by(anio, sector_label) %>%
  summarise(
    observaciones = n(),
    trabajadores_expandidos = sum(fex, na.rm = TRUE),
    ingreso_hora_real_promedio = weighted_mean(ingreso_hora_real, fex),
    .groups = "drop"
  ) %>%
  arrange(sector_label, anio)

anio_inicio_sector <- min(serie_sector_anual$anio, na.rm = TRUE)
anio_final_sector  <- max(serie_sector_anual$anio, na.rm = TRUE)

sector_crecimiento <- serie_sector_anual %>%
  filter(anio %in% c(anio_inicio_sector, anio_final_sector)) %>%
  select(anio, sector_label, ingreso_hora_real_promedio) %>%
  pivot_wider(
    names_from = anio,
    values_from = ingreso_hora_real_promedio,
    names_prefix = "y_"
  ) %>%
  filter(
    !is.na(.data[[paste0("y_", anio_inicio_sector)]]),
    !is.na(.data[[paste0("y_", anio_final_sector)]])
  ) %>%
  mutate(
    ingreso_inicio = .data[[paste0("y_", anio_inicio_sector)]],
    ingreso_final  = .data[[paste0("y_", anio_final_sector)]],
    
    indice_inicio = 100,
    indice_final = 100 * ingreso_final / ingreso_inicio,
    
    crecimiento_anualizado = 100 * (
      (ingreso_final / ingreso_inicio)^(1 / (anio_final_sector - anio_inicio_sector)) - 1
    ),
    
    grupo_crecimiento = case_when(
      crecimiento_anualizado < 0    ~ "Decrecimiento",
      crecimiento_anualizado < 0.75 ~ "Crecimiento bajo",
      crecimiento_anualizado < 1.50 ~ "Crecimiento medio",
      TRUE                          ~ "Crecimiento alto"
    ),
    grupo_crecimiento = factor(
      grupo_crecimiento,
      levels = c(
        "Crecimiento alto",
        "Crecimiento medio",
        "Crecimiento bajo",
        "Decrecimiento"
      )
    ),
    
    label_final = paste0(
      round(indice_final, 1),
      " | ",
      ifelse(crecimiento_anualizado >= 0, "+", ""),
      round(crecimiento_anualizado, 2),
      "% anual"
    ),
    
    sector_label = reorder(sector_label, indice_final)
  )

lim_inf_sector <- min(
  95,
  min(sector_crecimiento$indice_final, na.rm = TRUE) - 8
)

lim_sup_sector <- max(
  sector_crecimiento$indice_final,
  na.rm = TRUE
) + 28

#--------------------------------------------------------
# 2.2. Graficar
#--------------------------------------------------------

g_lollipop_sector_crecimiento <- ggplot(
  sector_crecimiento,
  aes(
    y = sector_label,
    x = indice_final
  )
) +
  geom_vline(
    xintercept = 100,
    linetype = "dashed",
    color = "gray45",
    linewidth = 0.7
  ) +
  geom_segment(
    aes(
      x = 100,
      xend = indice_final,
      y = sector_label,
      yend = sector_label,
      color = grupo_crecimiento
    ),
    linewidth = 1.5,
    lineend = "round"
  ) +
  geom_point(
    aes(color = grupo_crecimiento),
    size = 4.5
  ) +
  geom_text(
    data = sector_crecimiento %>% filter(indice_final >= 100),
    aes(label = label_final),
    hjust = -0.10,
    size = 3.2,
    fontface = "bold"
  ) +
  geom_text(
    data = sector_crecimiento %>% filter(indice_final < 100),
    aes(label = label_final),
    hjust = 1.10,
    size = 3.2,
    fontface = "bold"
  ) +
  scale_color_manual(
    values = c(
      "Crecimiento alto"  = "#0B7285",
      "Crecimiento medio" = "#E59F00",
      "Crecimiento bajo"  = "#A61E4D",
      "Decrecimiento"     = "#8B0000"
    )
  ) +
  scale_x_continuous(
    limits = c(lim_inf_sector, lim_sup_sector),
    breaks = pretty(c(lim_inf_sector, lim_sup_sector), n = 8)
  ) +
  labs(
    title = paste0(
      "Crecimiento del ingreso laboral por hora real por sector económico, ",
      anio_inicio_sector,
      "–",
      anio_final_sector
    ),
    subtitle = paste0(
      "Índice base ",
      anio_inicio_sector,
      " = 100. La etiqueta muestra el crecimiento anualizado"
    ),
    x = paste0(
      "Índice del ingreso laboral por hora, ",
      anio_inicio_sector,
      " = 100"
    ),
    y = "Sector económico",
    color = "Grupo"
  ) +
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(face = "bold"),
    axis.text.y = element_text(size = 9.5),
    axis.text.x = element_text(size = 10),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(face = "bold"),
    panel.grid.major.x = element_line(color = "gray90", linewidth = 0.35),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 125, 10, 10)
  )

g_lollipop_sector_crecimiento

#========================================================
# GRÁFICO24. Cambio en la composición por sector económico
# Dumbbell: participación año inicial vs año final
#========================================================

#--------------------------------------------------------
# 1. Configuración general
#--------------------------------------------------------

sectores_excluir <- c(
  "Extraterritoriales",
  "Organizaciones extraterritoriales",
  "Actividades de organizaciones y órganos extraterritoriales"
)

#--------------------------------------------------------
# 2. Preparar datos
#--------------------------------------------------------

serie_comp_sector_dumbbell <- geih %>%
  filter(
    !is.na(anio),
    !is.na(sector_label),
    !(sector_label %in% sectores_excluir),
    !is.na(fex),
    fex > 0
  ) %>%
  group_by(anio, sector_label) %>%
  summarise(
    observaciones = n(),
    trabajadores_expandidos = sum(fex, na.rm = TRUE),
    trabajadores_millones = trabajadores_expandidos / 1e6,
    .groups = "drop"
  ) %>%
  group_by(anio) %>%
  mutate(
    total_trabajadores = sum(trabajadores_millones, na.rm = TRUE),
    participacion = trabajadores_millones / total_trabajadores
  ) %>%
  ungroup()

anio_inicio_sector_comp <- min(serie_comp_sector_dumbbell$anio, na.rm = TRUE)
anio_final_sector_comp  <- max(serie_comp_sector_dumbbell$anio, na.rm = TRUE)

sector_dumbbell <- serie_comp_sector_dumbbell %>%
  filter(anio %in% c(anio_inicio_sector_comp, anio_final_sector_comp)) %>%
  select(anio, sector_label, participacion) %>%
  pivot_wider(
    names_from = anio,
    values_from = participacion,
    names_prefix = "p_"
  ) %>%
  filter(
    !is.na(.data[[paste0("p_", anio_inicio_sector_comp)]]),
    !is.na(.data[[paste0("p_", anio_final_sector_comp)]])
  ) %>%
  mutate(
    participacion_inicio = .data[[paste0("p_", anio_inicio_sector_comp)]],
    participacion_final  = .data[[paste0("p_", anio_final_sector_comp)]],
    
    cambio_pp = 100 * (participacion_final - participacion_inicio),
    
    label_cambio = paste0(
      ifelse(cambio_pp >= 0, "+", ""),
      round(cambio_pp, 1),
      " p.p."
    ),
    
    grupo_cambio = case_when(
      cambio_pp > 0.3  ~ "Aumentó participación",
      cambio_pp < -0.3 ~ "Redujo participación",
      TRUE             ~ "Cambio bajo"
    ),
    grupo_cambio = factor(
      grupo_cambio,
      levels = c(
        "Aumentó participación",
        "Cambio bajo",
        "Redujo participación"
      )
    ),
    
    x_min = pmin(100 * participacion_inicio, 100 * participacion_final),
    x_max = pmax(100 * participacion_inicio, 100 * participacion_final),
    x_label = x_max + 0.85,
    
    sector_label = reorder(sector_label, participacion_final)
  )

#--------------------------------------------------------
# 3. Datos para puntos de año
#--------------------------------------------------------

puntos_sector <- sector_dumbbell %>%
  select(
    sector_label,
    participacion_inicio,
    participacion_final
  ) %>%
  pivot_longer(
    cols = c(participacion_inicio, participacion_final),
    names_to = "anio_tipo",
    values_to = "participacion"
  ) %>%
  mutate(
    anio = case_when(
      anio_tipo == "participacion_inicio" ~ as.character(anio_inicio_sector_comp),
      anio_tipo == "participacion_final"  ~ as.character(anio_final_sector_comp)
    ),
    anio = factor(
      anio,
      levels = c(
        as.character(anio_inicio_sector_comp),
        as.character(anio_final_sector_comp)
      )
    )
  )

#--------------------------------------------------------
# 4. Colores
#--------------------------------------------------------

colores_cambio_sector <- c(
  "Aumentó participación" = "#1B9E77",
  "Cambio bajo"           = "#9AA5B1",
  "Redujo participación"  = "#D62828"
)

colores_anio_sector <- setNames(
  c("#8ECAE6", "#1D4ED8"),  # año inicial azul claro, año final azul fuerte
  c(as.character(anio_inicio_sector_comp), as.character(anio_final_sector_comp))
)

#--------------------------------------------------------
# 5. Graficar
#--------------------------------------------------------

g_dumbbell_comp_sector <- ggplot(
  sector_dumbbell,
  aes(y = sector_label)
) +
  # Línea principal entre año inicial y año final
  geom_segment(
    aes(
      x = 100 * participacion_inicio,
      xend = 100 * participacion_final,
      yend = sector_label
    ),
    color = "gray60",
    linewidth = 1.15,
    alpha = 0.90,
    lineend = "round"
  ) +
  
  # Línea corta hacia la etiqueta de cambio
  geom_segment(
    aes(
      x = x_max,
      xend = x_label - 0.10,
      yend = sector_label
    ),
    color = "gray70",
    linewidth = 0.65,
    alpha = 0.85,
    lineend = "round"
  ) +
  
  # Puntos año inicial y año final
  geom_point(
    data = puntos_sector,
    aes(
      x = 100 * participacion,
      y = sector_label,
      fill = anio
    ),
    shape = 21,
    color = "white",
    stroke = 0.8,
    size = 4.0,
    alpha = 0.98,
    inherit.aes = FALSE
  ) +
  
  # Etiqueta del cambio
  geom_label(
    aes(
      x = x_label,
      label = label_cambio,
      fill = grupo_cambio
    ),
    color = "white",
    fontface = "bold",
    size = 2.9,
    label.size = 0.10,
    label.padding = unit(0.11, "lines"),
    show.legend = FALSE
  ) +
  
  scale_fill_manual(
    values = c(
      colores_anio_sector,
      colores_cambio_sector
    ),
    breaks = c(
      as.character(anio_inicio_sector_comp),
      as.character(anio_final_sector_comp)
    ),
    name = "Año"
  ) +
  scale_x_continuous(
    labels = function(x) paste0(number(x, accuracy = 0.1), "%"),
    expand = expansion(mult = c(0.04, 0.20))
  ) +
  labs(
    title = paste0(
      "Cambio en la composición del número de trabajadores por sector económico, ",
      anio_inicio_sector_comp,
      " vs. ",
      anio_final_sector_comp
    ),
    subtitle = paste0(
      "Participación porcentual sobre el total de ocupados. ",
      anio_inicio_sector_comp,
      " en azul claro y ",
      anio_final_sector_comp,
      " en azul oscuro. Excluye organizaciones extraterritoriales"
    ),
    x = "Participación en el total de trabajadores",
    y = "Sector económico"
  ) +
  guides(
    fill = guide_legend(
      order = 1,
      override.aes = list(shape = 21, size = 4, color = "white")
    )
  ) +
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(face = "bold"),
    axis.text.y = element_text(size = 9.5),
    axis.text.x = element_text(size = 10),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(face = "bold"),
    panel.grid.major.x = element_line(color = "gray90", linewidth = 0.35),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 125, 10, 10)
  )

g_dumbbell_comp_sector

#========================================================
# CONFIGURACIÓN GENERAL: SUBRAMA DETALLADA / RAMA 4D
# Variables:
#   código = subrama_det_cod
#   label  = subrama_det_label
#========================================================

#--------------------------------------------------------
# Validación básica
#--------------------------------------------------------

if (!all(c("subrama_det_cod", "subrama_det_label") %in% names(geih))) {
  stop("En geih no encontré subrama_det_cod y/o subrama_det_label.")
}

if (!all(c("subrama_det_cod", "subrama_det_label") %in% names(geih_ingreso))) {
  stop("En geih_ingreso no encontré subrama_det_cod y/o subrama_det_label.")
}

#--------------------------------------------------------
# Base auxiliar con todas las subramas válidas
#--------------------------------------------------------

base_subrama_trabajadores <- geih %>%
  filter(
    !is.na(anio),
    !is.na(fex),
    fex > 0,
    !is.na(subrama_det_cod),
    !is.na(subrama_det_label)
  ) %>%
  mutate(
    subrama_det_label = stringr::str_squish(as.character(subrama_det_label)),
    subrama_det_cod_chr = as.character(subrama_det_cod),
    subrama_id = paste0(subrama_det_cod_chr, " - ", subrama_det_label)
  ) %>%
  filter(
    subrama_det_label != "",
    !stringr::str_detect(
      stringr::str_to_lower(subrama_det_label),
      "extraterritorial|exterritorial|organizaciones extraterritoriales"
    ),
    !stringr::str_detect(subrama_det_cod_chr, "^99")
  )

anio_inicio_subrama <- min(base_subrama_trabajadores$anio, na.rm = TRUE)
anio_final_subrama  <- max(base_subrama_trabajadores$anio, na.rm = TRUE)

#========================================================
# GRÁFICO 25. Ingreso por subrama detallada
# Top 5 ingresos altos y bottom 5 ingresos bajos
# 2010 vs 2025
# Etiqueta lateral con sector Rama2D
#========================================================

#--------------------------------------------------------
# 1. Definir años
#--------------------------------------------------------

anio_inicio_subrama <- 2010

anio_final_subrama <- geih_ingreso %>%
  filter(
    anio >= anio_inicio_subrama,
    !is.na(subrama_det_cod),
    !is.na(subrama_det_label)
  ) %>%
  summarise(
    anio_final = max(anio, na.rm = TRUE)
  ) %>%
  pull(anio_final)

#--------------------------------------------------------
# 2. Preparar datos base
#--------------------------------------------------------

serie_subrama_niveles_all <- geih_ingreso %>%
  filter(
    anio %in% c(anio_inicio_subrama, anio_final_subrama),
    !is.na(subrama_det_cod),
    !is.na(subrama_det_label),
    !is.na(sector_label)
  ) %>%
  mutate(
    subrama_det_label = stringr::str_squish(as.character(subrama_det_label)),
    subrama_det_cod_chr = as.character(subrama_det_cod),
    sector_2d = stringr::str_squish(as.character(sector_label)),
    
    # ID interno para evitar problemas si hay nombres repetidos
    subrama_id = paste0(subrama_det_cod_chr, " - ", subrama_det_label)
  ) %>%
  filter(
    subrama_det_label != "",
    sector_2d != "",
    !stringr::str_detect(
      stringr::str_to_lower(subrama_det_label),
      "extraterritorial|exterritorial|organizaciones extraterritoriales"
    ),
    !stringr::str_detect(subrama_det_cod_chr, "^99")
  ) %>%
  group_by(anio, subrama_id, subrama_det_label, sector_2d) %>%
  summarise(
    observaciones = n(),
    trabajadores_expandidos = sum(fex, na.rm = TRUE),
    ingreso_hora_real_promedio = weighted_mean(ingreso_hora_real, fex),
    .groups = "drop"
  ) %>%
  group_by(subrama_id) %>%
  filter(n_distinct(anio) == 2) %>%
  ungroup()

#--------------------------------------------------------
# 3. Seleccionar top 5 y bottom 5 según ingreso en 2025
#--------------------------------------------------------

subramas_bottom_5 <- serie_subrama_niveles_all %>%
  filter(anio == anio_final_subrama) %>%
  arrange(ingreso_hora_real_promedio) %>%
  slice_head(n = 5) %>%
  mutate(
    grupo_ingreso = "Menor ingreso"
  )

subramas_top_5 <- serie_subrama_niveles_all %>%
  filter(anio == anio_final_subrama) %>%
  arrange(desc(ingreso_hora_real_promedio)) %>%
  slice_head(n = 5) %>%
  mutate(
    grupo_ingreso = "Mayor ingreso"
  )

subramas_top_bottom <- bind_rows(
  subramas_top_5,
  subramas_bottom_5
) %>%
  distinct(subrama_id, .keep_all = TRUE) %>%
  select(
    subrama_id,
    subrama_det_label,
    sector_2d,
    grupo_ingreso,
    ingreso_2025 = ingreso_hora_real_promedio
  )

#--------------------------------------------------------
# 4. Base final del gráfico
#--------------------------------------------------------

serie_subrama_top_bottom <- serie_subrama_niveles_all %>%
  inner_join(
    subramas_top_bottom,
    by = c("subrama_id", "subrama_det_label", "sector_2d")
  )

#--------------------------------------------------------
# 5. Ordenar según ingreso de 2025
#--------------------------------------------------------

orden_subrama_top_bottom <- subramas_top_bottom %>%
  arrange(ingreso_2025) %>%
  pull(subrama_id)

serie_subrama_top_bottom <- serie_subrama_top_bottom %>%
  mutate(
    subrama_id = factor(
      subrama_id,
      levels = orden_subrama_top_bottom
    ),
    anio = factor(
      anio,
      levels = c(anio_inicio_subrama, anio_final_subrama)
    )
  )

subramas_top_bottom <- subramas_top_bottom %>%
  mutate(
    subrama_id = factor(
      subrama_id,
      levels = orden_subrama_top_bottom
    )
  )

#--------------------------------------------------------
# 6. Etiquetas del eje Y: solo nombre de subrama, sin código
#--------------------------------------------------------

labels_subrama_sin_codigo <- subramas_top_bottom %>%
  mutate(
    etiqueta_eje = stringr::str_wrap(subrama_det_label, width = 34)
  ) %>%
  select(subrama_id, etiqueta_eje) %>%
  tibble::deframe()

#--------------------------------------------------------
# 7. Etiqueta lateral del sector Rama2D
#--------------------------------------------------------

labels_sector_lateral <- serie_subrama_top_bottom %>%
  group_by(subrama_id, sector_2d) %>%
  summarise(
    x_sector = max(ingreso_hora_real_promedio, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    x_sector = x_sector + 0.12 * max(serie_subrama_top_bottom$ingreso_hora_real_promedio, na.rm = TRUE),
    sector_2d = stringr::str_wrap(sector_2d, width = 24)
  )

#--------------------------------------------------------
# 8. Colores
#--------------------------------------------------------

colores_anios_subrama <- setNames(
  c("darkred", "darkblue"),
  c(as.character(anio_inicio_subrama), as.character(anio_final_subrama))
)

#--------------------------------------------------------
# 9. Graficar
#--------------------------------------------------------

g_barras_subrama_top_bottom <- ggplot(
  serie_subrama_top_bottom,
  aes(
    x = subrama_id,
    y = ingreso_hora_real_promedio,
    fill = anio
  )
) +
  geom_col(
    position = position_dodge(width = 0.74),
    width = 0.62,
    alpha = 0.95
  ) +
  geom_label(
    aes(
      label = comma(ingreso_hora_real_promedio, accuracy = 1),
      fill = anio
    ),
    position = position_dodge(width = 0.74),
    color = "white",
    fontface = "bold",
    size = 3.0,
    label.size = 0.13,
    label.padding = unit(0.12, "lines"),
    hjust = -0.08,
    show.legend = FALSE
  ) +
  
  # Etiqueta lateral: solo nombre del sector Rama2D
  geom_label(
    data = labels_sector_lateral,
    aes(
      x = subrama_id,
      y = x_sector,
      label = sector_2d
    ),
    inherit.aes = FALSE,
    fill = "gray95",
    color = "gray25",
    fontface = "bold",
    size = 3.0,
    label.size = 0.15,
    label.padding = unit(0.16, "lines"),
    hjust = 0,
    lineheight = 0.9
  ) +
  coord_flip(clip = "off") +
  scale_x_discrete(
    labels = labels_subrama_sin_codigo
  ) +
  scale_fill_manual(
    values = colores_anios_subrama
  ) +
  scale_y_continuous(
    labels = comma,
    expand = expansion(mult = c(0.02, 0.50))
  ) +
  labs(
    title = paste0(
      "Ingreso laboral por hora real por subrama detallada: ",
      anio_inicio_subrama,
      " vs. ",
      anio_final_subrama
    ),
    subtitle = paste0(
      "5 subramas con mayor ingreso y 5 con menor ingreso según ",
      anio_final_subrama,
      ". Pesos constantes de 2025"
    ),
    x = "Subrama detallada",
    y = "Ingreso laboral por hora promedio",
    fill = "Año"
  ) +
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(face = "bold"),
    axis.text.y = element_text(size = 9.0, lineheight = 0.9),
    axis.text.x = element_text(size = 10),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(face = "bold"),
    panel.grid.major.x = element_line(color = "gray90", linewidth = 0.35),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 220, 10, 10)
  )

g_barras_subrama_top_bottom

#========================================================
# GRÁFICO26. Crecimiento del ingreso por subrama detallada
# Top 5 ingresos altos y bottom 5 ingresos bajos
# Lollipop - Índice 2010 = 100
# Etiqueta: crecimiento anualizado
#========================================================

#--------------------------------------------------------
# 1. Definir años
#--------------------------------------------------------

anio_inicio_subrama <- 2010

anio_final_subrama <- geih_ingreso %>%
  filter(
    anio >= anio_inicio_subrama,
    !is.na(subrama_det_cod),
    !is.na(subrama_det_label)
  ) %>%
  summarise(
    anio_final = max(anio, na.rm = TRUE)
  ) %>%
  pull(anio_final)

#--------------------------------------------------------
# 2. Preparar datos base
#--------------------------------------------------------

serie_subrama_anual_all <- geih_ingreso %>%
  filter(
    anio %in% c(anio_inicio_subrama, anio_final_subrama),
    !is.na(subrama_det_cod),
    !is.na(subrama_det_label),
    !is.na(sector_label)
  ) %>%
  mutate(
    subrama_det_label = stringr::str_squish(as.character(subrama_det_label)),
    subrama_det_cod_chr = as.character(subrama_det_cod),
    sector_2d = stringr::str_squish(as.character(sector_label)),
    subrama_id = paste0(subrama_det_cod_chr, " - ", subrama_det_label)
  ) %>%
  filter(
    subrama_det_label != "",
    sector_2d != "",
    !stringr::str_detect(
      stringr::str_to_lower(subrama_det_label),
      "extraterritorial|exterritorial|organizaciones extraterritoriales"
    ),
    !stringr::str_detect(subrama_det_cod_chr, "^99")
  ) %>%
  group_by(anio, subrama_id, subrama_det_label, sector_2d) %>%
  summarise(
    observaciones = n(),
    trabajadores_expandidos = sum(fex, na.rm = TRUE),
    ingreso_hora_real_promedio = weighted_mean(ingreso_hora_real, fex),
    .groups = "drop"
  ) %>%
  group_by(subrama_id) %>%
  filter(n_distinct(anio) == 2) %>%
  ungroup()

#--------------------------------------------------------
# 3. Seleccionar 5 mayores y 5 menores según ingreso en 2025
#--------------------------------------------------------

subramas_bottom_5 <- serie_subrama_anual_all %>%
  filter(anio == anio_final_subrama) %>%
  arrange(ingreso_hora_real_promedio) %>%
  slice_head(n = 5) %>%
  mutate(grupo_ingreso = "Menor ingreso")

subramas_top_5 <- serie_subrama_anual_all %>%
  filter(anio == anio_final_subrama) %>%
  arrange(desc(ingreso_hora_real_promedio)) %>%
  slice_head(n = 5) %>%
  mutate(grupo_ingreso = "Mayor ingreso")

subramas_top_bottom <- bind_rows(
  subramas_top_5,
  subramas_bottom_5
) %>%
  distinct(subrama_id, .keep_all = TRUE) %>%
  select(
    subrama_id,
    subrama_det_label,
    sector_2d,
    grupo_ingreso,
    ingreso_2025 = ingreso_hora_real_promedio
  )

#--------------------------------------------------------
# 4. Calcular índice y crecimiento anualizado
#--------------------------------------------------------

subrama_crecimiento_top_bottom <- serie_subrama_anual_all %>%
  inner_join(
    subramas_top_bottom,
    by = c("subrama_id", "subrama_det_label", "sector_2d")
  ) %>%
  select(
    anio,
    subrama_id,
    subrama_det_label,
    sector_2d,
    ingreso_hora_real_promedio
  ) %>%
  pivot_wider(
    names_from = anio,
    values_from = ingreso_hora_real_promedio,
    names_prefix = "y_"
  ) %>%
  mutate(
    ingreso_inicio = .data[[paste0("y_", anio_inicio_subrama)]],
    ingreso_final  = .data[[paste0("y_", anio_final_subrama)]],
    
    indice_inicio = 100,
    indice_final = 100 * ingreso_final / ingreso_inicio,
    
    crecimiento_anualizado = 100 * (
      (ingreso_final / ingreso_inicio)^(1 / (anio_final_subrama - anio_inicio_subrama)) - 1
    ),
    
    grupo_crecimiento = case_when(
      crecimiento_anualizado < 0    ~ "Decrecimiento",
      crecimiento_anualizado < 0.75 ~ "Crecimiento bajo",
      crecimiento_anualizado < 1.50 ~ "Crecimiento medio",
      TRUE                          ~ "Crecimiento alto"
    ),
    grupo_crecimiento = factor(
      grupo_crecimiento,
      levels = c(
        "Crecimiento alto",
        "Crecimiento medio",
        "Crecimiento bajo",
        "Decrecimiento"
      )
    ),
    
    label_final = paste0(
      round(indice_final, 1),
      " | ",
      ifelse(crecimiento_anualizado >= 0, "+", ""),
      round(crecimiento_anualizado, 2),
      "% anual"
    )
  )

#--------------------------------------------------------
# 5. Ordenar según índice final
#--------------------------------------------------------

orden_subrama_lollipop <- subrama_crecimiento_top_bottom %>%
  arrange(indice_final) %>%
  pull(subrama_id)

subrama_crecimiento_top_bottom <- subrama_crecimiento_top_bottom %>%
  mutate(
    subrama_id = factor(
      subrama_id,
      levels = orden_subrama_lollipop
    )
  )

#--------------------------------------------------------
# 6. Etiquetas del eje Y: solo nombre de subrama, sin código
#--------------------------------------------------------

labels_subrama_sin_codigo_lollipop <- subrama_crecimiento_top_bottom %>%
  mutate(
    etiqueta_eje = stringr::str_wrap(subrama_det_label, width = 34)
  ) %>%
  select(subrama_id, etiqueta_eje) %>%
  tibble::deframe()

#--------------------------------------------------------
# 7. Posiciones de etiquetas
#--------------------------------------------------------

max_indice_subrama <- max(
  subrama_crecimiento_top_bottom$indice_final,
  na.rm = TRUE
)

subrama_crecimiento_top_bottom <- subrama_crecimiento_top_bottom %>%
  mutate(
    x_max = pmax(100, indice_final),
    x_label = x_max + 0.055 * max_indice_subrama,
    x_sector = x_label + 0.18 * max_indice_subrama,
    sector_2d_label = stringr::str_wrap(sector_2d, width = 24)
  )

#--------------------------------------------------------
# 8. Límites del eje X
#--------------------------------------------------------

lim_inf_subrama <- min(
  95,
  min(subrama_crecimiento_top_bottom$indice_final, na.rm = TRUE) - 8
)

lim_sup_subrama <- max(
  subrama_crecimiento_top_bottom$x_sector,
  na.rm = TRUE
) + 12

#--------------------------------------------------------
# 9. Graficar
#--------------------------------------------------------

g_lollipop_subrama_top_bottom <- ggplot(
  subrama_crecimiento_top_bottom,
  aes(
    y = subrama_id,
    x = indice_final
  )
) +
  geom_vline(
    xintercept = 100,
    linetype = "dashed",
    color = "gray45",
    linewidth = 0.7
  ) +
  geom_segment(
    aes(
      x = 100,
      xend = indice_final,
      y = subrama_id,
      yend = subrama_id,
      color = grupo_crecimiento
    ),
    linewidth = 1.5,
    lineend = "round"
  ) +
  geom_point(
    aes(color = grupo_crecimiento),
    size = 4.5
  ) +
  
  # Etiqueta: índice final + crecimiento anualizado
  geom_label(
    aes(
      x = x_label,
      label = label_final,
      fill = grupo_crecimiento
    ),
    color = "white",
    fontface = "bold",
    size = 3.0,
    label.size = 0.12,
    label.padding = unit(0.12, "lines"),
    hjust = 0,
    show.legend = FALSE
  ) +
  scale_y_discrete(
    labels = labels_subrama_sin_codigo_lollipop
  ) +
  scale_color_manual(
    values = c(
      "Crecimiento alto"  = "#0B7285",
      "Crecimiento medio" = "#E59F00",
      "Crecimiento bajo"  = "#A61E4D",
      "Decrecimiento"     = "#8B0000"
    )
  ) +
  scale_fill_manual(
    values = c(
      "Crecimiento alto"  = "#0B7285",
      "Crecimiento medio" = "#E59F00",
      "Crecimiento bajo"  = "#A61E4D",
      "Decrecimiento"     = "#8B0000"
    )
  ) +
  scale_x_continuous(
    limits = c(lim_inf_subrama, lim_sup_subrama),
    breaks = pretty(c(lim_inf_subrama, lim_sup_subrama), n = 8)
  ) +
  coord_cartesian(clip = "off") +
  labs(
    title = paste0(
      "Crecimiento del ingreso laboral por hora real por subrama detallada, ",
      anio_inicio_subrama,
      "–",
      anio_final_subrama
    ),
    subtitle = paste0(
      "5 subramas con mayor ingreso y 5 con menor ingreso según ",
      anio_final_subrama,
      ". Índice base ",
      anio_inicio_subrama,
      " = 100"
    ),
    x = paste0(
      "Índice del ingreso laboral por hora, ",
      anio_inicio_subrama,
      " = 100"
    ),
    y = "Subrama detallada",
    color = "Grupo"
  ) +
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(face = "bold"),
    axis.text.y = element_text(size = 9.0, lineheight = 0.9),
    axis.text.x = element_text(size = 10),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(face = "bold"),
    panel.grid.major.x = element_line(color = "gray90", linewidth = 0.35),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 210, 10, 10)
  )

g_lollipop_subrama_top_bottom

#========================================================
# GRÁFICO27. Composición de subramas dentro de su sector Rama2D
# Top 5 ingresos altos y bottom 5 ingresos bajos
# Participación de cada subrama dentro de su sector
#========================================================

#--------------------------------------------------------
# 1. Definir años
#--------------------------------------------------------

anio_inicio_subrama <- 2010

anio_final_subrama <- geih_ingreso %>%
  filter(
    anio >= anio_inicio_subrama,
    !is.na(subrama_det_cod),
    !is.na(subrama_det_label)
  ) %>%
  summarise(
    anio_final = max(anio, na.rm = TRUE)
  ) %>%
  pull(anio_final)

#--------------------------------------------------------
# 2. Seleccionar las 10 subramas de interés según ingreso en 2025
#    5 con mayor ingreso y 5 con menor ingreso
#--------------------------------------------------------

serie_subrama_ingreso_all <- geih_ingreso %>%
  filter(
    anio %in% c(anio_inicio_subrama, anio_final_subrama),
    !is.na(subrama_det_cod),
    !is.na(subrama_det_label),
    !is.na(sector_label)
  ) %>%
  mutate(
    subrama_det_label = stringr::str_squish(as.character(subrama_det_label)),
    subrama_det_cod_chr = as.character(subrama_det_cod),
    sector_2d = stringr::str_squish(as.character(sector_label)),
    subrama_id = paste0(subrama_det_cod_chr, " - ", subrama_det_label)
  ) %>%
  filter(
    subrama_det_label != "",
    sector_2d != "",
    !stringr::str_detect(
      stringr::str_to_lower(subrama_det_label),
      "extraterritorial|exterritorial|organizaciones extraterritoriales"
    ),
    !stringr::str_detect(subrama_det_cod_chr, "^99")
  ) %>%
  group_by(anio, subrama_id, subrama_det_label, sector_2d) %>%
  summarise(
    observaciones = n(),
    trabajadores_expandidos = sum(fex, na.rm = TRUE),
    ingreso_hora_real_promedio = weighted_mean(ingreso_hora_real, fex),
    .groups = "drop"
  ) %>%
  group_by(subrama_id) %>%
  filter(n_distinct(anio) == 2) %>%
  ungroup()

subramas_bottom_5 <- serie_subrama_ingreso_all %>%
  filter(anio == anio_final_subrama) %>%
  arrange(ingreso_hora_real_promedio) %>%
  slice_head(n = 5) %>%
  mutate(grupo_ingreso = "Menor ingreso")

subramas_top_5 <- serie_subrama_ingreso_all %>%
  filter(anio == anio_final_subrama) %>%
  arrange(desc(ingreso_hora_real_promedio)) %>%
  slice_head(n = 5) %>%
  mutate(grupo_ingreso = "Mayor ingreso")

subramas_top_bottom <- bind_rows(
  subramas_top_5,
  subramas_bottom_5
) %>%
  distinct(subrama_id, .keep_all = TRUE) %>%
  select(
    subrama_id,
    subrama_det_label,
    sector_2d,
    grupo_ingreso
  )

#--------------------------------------------------------
# 3. Denominador: total de trabajadores por sector Rama2D
#--------------------------------------------------------

total_sector_anual <- geih %>%
  filter(
    anio %in% c(anio_inicio_subrama, anio_final_subrama),
    !is.na(sector_label),
    !is.na(fex),
    fex > 0
  ) %>%
  mutate(
    sector_2d = stringr::str_squish(as.character(sector_label))
  ) %>%
  filter(
    sector_2d != "",
    !stringr::str_detect(
      stringr::str_to_lower(sector_2d),
      "extraterritorial|exterritorial|organizaciones extraterritoriales"
    )
  ) %>%
  group_by(anio, sector_2d) %>%
  summarise(
    trabajadores_sector = sum(fex, na.rm = TRUE),
    .groups = "drop"
  )

#--------------------------------------------------------
# 4. Numerador: trabajadores de cada subrama seleccionada
#--------------------------------------------------------

trabajadores_subrama_anual <- geih %>%
  filter(
    anio %in% c(anio_inicio_subrama, anio_final_subrama),
    !is.na(subrama_det_cod),
    !is.na(subrama_det_label),
    !is.na(sector_label),
    !is.na(fex),
    fex > 0
  ) %>%
  mutate(
    subrama_det_label = stringr::str_squish(as.character(subrama_det_label)),
    subrama_det_cod_chr = as.character(subrama_det_cod),
    sector_2d = stringr::str_squish(as.character(sector_label)),
    subrama_id = paste0(subrama_det_cod_chr, " - ", subrama_det_label)
  ) %>%
  filter(
    subrama_id %in% subramas_top_bottom$subrama_id,
    subrama_det_label != "",
    sector_2d != "",
    !stringr::str_detect(
      stringr::str_to_lower(subrama_det_label),
      "extraterritorial|exterritorial|organizaciones extraterritoriales"
    ),
    !stringr::str_detect(subrama_det_cod_chr, "^99")
  ) %>%
  group_by(anio, subrama_id, subrama_det_label, sector_2d) %>%
  summarise(
    trabajadores_subrama = sum(fex, na.rm = TRUE),
    .groups = "drop"
  )

#--------------------------------------------------------
# 5. Participación de cada subrama dentro de su sector
#--------------------------------------------------------

serie_comp_subrama_sector <- trabajadores_subrama_anual %>%
  left_join(
    total_sector_anual,
    by = c("anio", "sector_2d")
  ) %>%
  mutate(
    participacion_sector = trabajadores_subrama / trabajadores_sector
  ) %>%
  left_join(
    subramas_top_bottom %>%
      select(subrama_id, grupo_ingreso),
    by = "subrama_id"
  )

#--------------------------------------------------------
# 6. Ordenar según participación en 2025
#--------------------------------------------------------

orden_subrama_comp_sector <- serie_comp_subrama_sector %>%
  filter(anio == anio_final_subrama) %>%
  arrange(participacion_sector) %>%
  pull(subrama_id)

serie_comp_subrama_sector <- serie_comp_subrama_sector %>%
  mutate(
    subrama_id = factor(
      subrama_id,
      levels = orden_subrama_comp_sector
    ),
    anio = factor(
      anio,
      levels = c(anio_inicio_subrama, anio_final_subrama)
    )
  )

#--------------------------------------------------------
# 7. Etiquetas del eje Y: solo nombre de subrama
#--------------------------------------------------------

labels_subrama_sin_codigo_comp <- serie_comp_subrama_sector %>%
  distinct(subrama_id, subrama_det_label) %>%
  mutate(
    etiqueta_eje = stringr::str_wrap(subrama_det_label, width = 34)
  ) %>%
  select(subrama_id, etiqueta_eje) %>%
  tibble::deframe()

#--------------------------------------------------------
# 8. Etiqueta lateral del sector Rama2D
#--------------------------------------------------------

max_participacion_sector <- max(
  serie_comp_subrama_sector$participacion_sector,
  na.rm = TRUE
)

labels_sector_lateral_comp <- serie_comp_subrama_sector %>%
  group_by(subrama_id, sector_2d) %>%
  summarise(
    x_sector = max(participacion_sector, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    x_sector = x_sector + 0.14 * max_participacion_sector,
    sector_2d = stringr::str_wrap(sector_2d, width = 24)
  )

#--------------------------------------------------------
# 9. Colores
#--------------------------------------------------------

colores_anios_subrama_comp <- setNames(
  c("#8ECAE6", "#1D4ED8"),
  c(as.character(anio_inicio_subrama), as.character(anio_final_subrama))
)

#--------------------------------------------------------
# 10. Graficar
#--------------------------------------------------------

g_comp_subrama_dentro_sector <- ggplot(
  serie_comp_subrama_sector,
  aes(
    x = subrama_id,
    y = participacion_sector,
    fill = anio
  )
) +
  geom_col(
    position = position_dodge(width = 0.74),
    width = 0.62,
    alpha = 0.96
  ) +
  geom_label(
    aes(
      label = percent(participacion_sector, accuracy = 0.1),
      fill = anio
    ),
    position = position_dodge(width = 0.74),
    color = "white",
    fontface = "bold",
    size = 3.0,
    label.size = 0.12,
    label.padding = unit(0.12, "lines"),
    hjust = -0.08,
    show.legend = FALSE
  ) +
  
  # Etiqueta lateral: solo nombre del sector Rama2D
  geom_label(
    data = labels_sector_lateral_comp,
    aes(
      x = subrama_id,
      y = x_sector,
      label = sector_2d
    ),
    inherit.aes = FALSE,
    fill = "gray95",
    color = "gray25",
    fontface = "bold",
    size = 3.0,
    label.size = 0.15,
    label.padding = unit(0.16, "lines"),
    hjust = 0,
    lineheight = 0.9
  ) +
  
  coord_flip(clip = "off") +
  scale_x_discrete(
    labels = labels_subrama_sin_codigo_comp
  ) +
  scale_fill_manual(
    values = colores_anios_subrama_comp
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    expand = expansion(mult = c(0.02, 0.48))
  ) +
  labs(
    title = paste0(
      "Participación de cada subrama dentro de su sector Rama2D, ",
      anio_inicio_subrama,
      " vs. ",
      anio_final_subrama
    ),
    subtitle = paste0(
      "Subramas seleccionadas: 5 con mayor ingreso y 5 con menor ingreso según ",
      anio_final_subrama,
      ". Cálculo ponderado por factores de expansión"
    ),
    x = "Subrama detallada",
    y = "Participación dentro del sector Rama2D",
    fill = "Año"
  ) +
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(face = "bold"),
    axis.text.y = element_text(size = 9.0, lineheight = 0.9),
    axis.text.x = element_text(size = 10),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(face = "bold"),
    panel.grid.major.x = element_line(color = "gray90", linewidth = 0.35),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 230, 10, 10)
  )

g_comp_subrama_dentro_sector

#========================================================
# GRÁFICO28. Ingreso por tamaño de empresa
# Barras en niveles: año inicial vs año final
#========================================================

#--------------------------------------------------------
# 1.1. Preparar datos
#--------------------------------------------------------

orden_tamano <- c(
  "Solo", "2-3", "4-5", "6-10", "11-19",
  "20-30", "31-50", "51-100", "101+"
)

anio_inicio_tamano <- min(geih_ingreso$anio, na.rm = TRUE)
anio_final_tamano  <- max(geih_ingreso$anio, na.rm = TRUE)

serie_tamano_inicio_final <- geih_ingreso %>%
  filter(
    anio %in% c(anio_inicio_tamano, anio_final_tamano),
    !is.na(tamano_empresa)
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
    ingreso_hora_real_promedio = weighted_mean(ingreso_hora_real, fex),
    .groups = "drop"
  ) %>%
  mutate(
    anio = factor(
      anio,
      levels = c(anio_inicio_tamano, anio_final_tamano)
    )
  )

colores_anios_tamano <- setNames(
  c("darkred", "darkblue"),
  c(as.character(anio_inicio_tamano), as.character(anio_final_tamano))
)

#--------------------------------------------------------
# 1.2. Graficar
#--------------------------------------------------------

g_barras_tamano_inicio_final <- ggplot(
  serie_tamano_inicio_final,
  aes(
    x = tamano_empresa,
    y = ingreso_hora_real_promedio,
    fill = anio
  )
) +
  geom_col(
    position = position_dodge(width = 0.74),
    width = 0.62,
    alpha = 0.95
  ) +
  geom_label(
    aes(
      label = comma(ingreso_hora_real_promedio, accuracy = 1),
      fill = anio
    ),
    position = position_dodge(width = 0.74),
    color = "white",
    fontface = "bold",
    size = 3.2,
    label.size = 0.13,
    label.padding = unit(0.13, "lines"),
    hjust = -0.08,
    show.legend = FALSE
  ) +
  coord_flip(clip = "off") +
  scale_fill_manual(
    values = colores_anios_tamano
  ) +
  scale_y_continuous(
    labels = comma,
    expand = expansion(mult = c(0.02, 0.22))
  ) +
  labs(
    title = paste0(
      "Ingreso laboral por hora real según tamaño de empresa: ",
      anio_inicio_tamano,
      " vs. ",
      anio_final_tamano
    ),
    subtitle = "Pesos constantes de 2025. Promedio ponderado por factores de expansión",
    x = "Tamaño de empresa",
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
    legend.title = element_text(face = "bold"),
    legend.text = element_text(face = "bold"),
    panel.grid.major.x = element_line(color = "gray90", linewidth = 0.35),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 95, 10, 10)
  )

g_barras_tamano_inicio_final

#========================================================
# GRÁFICO29. Crecimiento del ingreso por tamaño de empresa
# Lollipop - Índice año inicial = 100
# Etiqueta: crecimiento anualizado
#========================================================

#--------------------------------------------------------
# 2.1. Preparar datos
#--------------------------------------------------------

serie_tamano_anual <- geih_ingreso %>%
  filter(
    !is.na(tamano_empresa)
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
    ingreso_hora_real_promedio = weighted_mean(ingreso_hora_real, fex),
    .groups = "drop"
  ) %>%
  arrange(tamano_empresa, anio)

anio_inicio_tamano <- min(serie_tamano_anual$anio, na.rm = TRUE)
anio_final_tamano  <- max(serie_tamano_anual$anio, na.rm = TRUE)

tamano_crecimiento <- serie_tamano_anual %>%
  filter(anio %in% c(anio_inicio_tamano, anio_final_tamano)) %>%
  select(anio, tamano_empresa, ingreso_hora_real_promedio) %>%
  pivot_wider(
    names_from = anio,
    values_from = ingreso_hora_real_promedio,
    names_prefix = "y_"
  ) %>%
  filter(
    !is.na(.data[[paste0("y_", anio_inicio_tamano)]]),
    !is.na(.data[[paste0("y_", anio_final_tamano)]])
  ) %>%
  mutate(
    ingreso_inicio = .data[[paste0("y_", anio_inicio_tamano)]],
    ingreso_final  = .data[[paste0("y_", anio_final_tamano)]],
    
    indice_inicio = 100,
    indice_final = 100 * ingreso_final / ingreso_inicio,
    
    crecimiento_anualizado = 100 * (
      (ingreso_final / ingreso_inicio)^(1 / (anio_final_tamano - anio_inicio_tamano)) - 1
    ),
    
    grupo_crecimiento = case_when(
      crecimiento_anualizado < 0    ~ "Decrecimiento",
      crecimiento_anualizado < 0.75 ~ "Crecimiento bajo",
      crecimiento_anualizado < 1.50 ~ "Crecimiento medio",
      TRUE                          ~ "Crecimiento alto"
    ),
    grupo_crecimiento = factor(
      grupo_crecimiento,
      levels = c(
        "Crecimiento alto",
        "Crecimiento medio",
        "Crecimiento bajo",
        "Decrecimiento"
      )
    ),
    
    label_final = paste0(
      round(indice_final, 1),
      " | ",
      ifelse(crecimiento_anualizado >= 0, "+", ""),
      round(crecimiento_anualizado, 2),
      "% anual"
    ),
    
    tamano_empresa = reorder(tamano_empresa, indice_final)
  )

lim_inf_tamano <- min(
  95,
  min(tamano_crecimiento$indice_final, na.rm = TRUE) - 8
)

lim_sup_tamano <- max(
  tamano_crecimiento$indice_final,
  na.rm = TRUE
) + 24

#--------------------------------------------------------
# 2.2. Graficar
#--------------------------------------------------------

g_lollipop_tamano_crecimiento <- ggplot(
  tamano_crecimiento,
  aes(
    y = tamano_empresa,
    x = indice_final
  )
) +
  geom_vline(
    xintercept = 100,
    linetype = "dashed",
    color = "gray45",
    linewidth = 0.7
  ) +
  geom_segment(
    aes(
      x = 100,
      xend = indice_final,
      y = tamano_empresa,
      yend = tamano_empresa,
      color = grupo_crecimiento
    ),
    linewidth = 1.5,
    lineend = "round"
  ) +
  geom_point(
    aes(color = grupo_crecimiento),
    size = 4.5
  ) +
  geom_text(
    data = tamano_crecimiento %>% filter(indice_final >= 100),
    aes(label = label_final),
    hjust = -0.10,
    size = 3.4,
    fontface = "bold"
  ) +
  geom_text(
    data = tamano_crecimiento %>% filter(indice_final < 100),
    aes(label = label_final),
    hjust = 1.10,
    size = 3.4,
    fontface = "bold"
  ) +
  scale_color_manual(
    values = c(
      "Crecimiento alto"  = "#0B7285",
      "Crecimiento medio" = "#E59F00",
      "Crecimiento bajo"  = "#A61E4D",
      "Decrecimiento"     = "#8B0000"
    )
  ) +
  scale_x_continuous(
    limits = c(lim_inf_tamano, lim_sup_tamano),
    breaks = pretty(c(lim_inf_tamano, lim_sup_tamano), n = 8)
  ) +
  labs(
    title = paste0(
      "Crecimiento del ingreso laboral por hora real según tamaño de empresa, ",
      anio_inicio_tamano,
      "–",
      anio_final_tamano
    ),
    subtitle = paste0(
      "Índice base ",
      anio_inicio_tamano,
      " = 100. La etiqueta muestra el crecimiento anualizado"
    ),
    x = paste0(
      "Índice del ingreso laboral por hora, ",
      anio_inicio_tamano,
      " = 100"
    ),
    y = "Tamaño de empresa",
    color = "Grupo"
  ) +
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(face = "bold"),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(face = "bold"),
    panel.grid.major.x = element_line(color = "gray90", linewidth = 0.35),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 115, 10, 10)
  )

g_lollipop_tamano_crecimiento

#========================================================
# GRÁFICO30. Composición porcentual por tamaño de empresa
# Área apilada 100%
#========================================================

#--------------------------------------------------------
# 3.1. Preparar datos
#--------------------------------------------------------

serie_comp_tamano <- geih %>%
  filter(
    !is.na(anio),
    !is.na(tamano_empresa),
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
    trabajadores_millones = trabajadores_expandidos / 1e6,
    .groups = "drop"
  ) %>%
  group_by(anio) %>%
  mutate(
    total_trabajadores = sum(trabajadores_millones, na.rm = TRUE),
    participacion = trabajadores_millones / total_trabajadores
  ) %>%
  ungroup() %>%
  arrange(anio, tamano_empresa)

serie_comp_tamano_area <- serie_comp_tamano %>%
  group_by(anio) %>%
  arrange(tamano_empresa, .by_group = TRUE) %>%
  mutate(
    ymax = cumsum(participacion),
    ymin = ymax - participacion,
    ymid = (ymin + ymax) / 2
  ) %>%
  ungroup()

anio_inicio_tamano_comp <- min(serie_comp_tamano_area$anio, na.rm = TRUE)
anio_final_tamano_comp  <- max(serie_comp_tamano_area$anio, na.rm = TRUE)

#--------------------------------------------------------
# 3.2. Etiquetas de inicio
# Solo porcentaje
#--------------------------------------------------------

labels_inicio_tamano <- serie_comp_tamano_area %>%
  filter(anio == anio_inicio_tamano_comp) %>%
  mutate(
    anio_label = anio_inicio_tamano_comp + 0.25,
    label = percent(participacion, accuracy = 0.1)
  )

#--------------------------------------------------------
# 3.3. Etiquetas finales
# Nombre + porcentaje
#--------------------------------------------------------

labels_final_tamano <- serie_comp_tamano_area %>%
  filter(anio == anio_final_tamano_comp) %>%
  mutate(
    anio_label = anio_final_tamano_comp + 0.35,
    label = paste0(
      as.character(tamano_empresa),
      ": ",
      percent(participacion, accuracy = 0.1)
    )
  )

#--------------------------------------------------------
# 3.4. Cambio en puntos porcentuales por categoría
#--------------------------------------------------------

cambio_comp_tamano <- serie_comp_tamano_area %>%
  filter(anio %in% c(anio_inicio_tamano_comp, anio_final_tamano_comp)) %>%
  select(anio, tamano_empresa, participacion) %>%
  pivot_wider(
    names_from = anio,
    values_from = participacion,
    names_prefix = "y_"
  ) %>%
  mutate(
    participacion_inicio = .data[[paste0("y_", anio_inicio_tamano_comp)]],
    participacion_final  = .data[[paste0("y_", anio_final_tamano_comp)]],
    cambio_pp = 100 * (participacion_final - participacion_inicio),
    label_cambio = paste0(
      ifelse(cambio_pp >= 0, "+", ""),
      round(cambio_pp, 1),
      " p.p."
    )
  )

anio_label_cambio_tamano <- serie_comp_tamano_area$anio[
  which.min(abs(
    serie_comp_tamano_area$anio -
      (anio_inicio_tamano_comp + 0.58 * (anio_final_tamano_comp - anio_inicio_tamano_comp))
  ))
]

labels_cambio_tamano <- serie_comp_tamano_area %>%
  filter(anio == anio_label_cambio_tamano) %>%
  select(anio, tamano_empresa, ymid) %>%
  left_join(
    cambio_comp_tamano %>%
      select(tamano_empresa, label_cambio),
    by = "tamano_empresa"
  ) %>%
  mutate(
    x_label = anio_label_cambio_tamano,
    label = label_cambio
  )

#--------------------------------------------------------
# 3.5. Graficar
#--------------------------------------------------------

colores_tamano <- c(
  "Solo"   = "#6D597A",
  "2-3"    = "#B56576",
  "4-5"    = "#A61E4D",
  "6-10"   = "#E59F00",
  "11-19"  = "#0B7285",
  "20-30"  = "#457B9D",
  "31-50"  = "#2A9D8F",
  "51-100" = "#4361EE",
  "101+"   = "darkblue"
)

g_composicion_tamano_area <- ggplot(
  serie_comp_tamano_area,
  aes(
    x = anio,
    fill = tamano_empresa
  )
) +
  geom_ribbon(
    aes(
      ymin = ymin,
      ymax = ymax,
      group = tamano_empresa
    ),
    alpha = 0.96,
    linewidth = 0
  ) +
  
  geom_line(
    aes(
      y = ymax,
      group = tamano_empresa
    ),
    color = "white",
    linewidth = 0.60,
    alpha = 0.80
  ) +
  
  # Etiquetas internas al inicio: solo porcentaje
  geom_label(
    data = labels_inicio_tamano,
    aes(
      x = anio_label,
      y = ymid,
      label = label,
      fill = tamano_empresa
    ),
    color = "white",
    fontface = "bold",
    size = 2.9,
    label.size = 0.10,
    label.padding = unit(0.11, "lines"),
    lineheight = 0.9,
    show.legend = FALSE
  ) +
  
  # Cambio en puntos porcentuales por categoría
  geom_label(
    data = labels_cambio_tamano,
    aes(
      x = x_label,
      y = ymid,
      label = label,
      fill = tamano_empresa
    ),
    color = "white",
    fontface = "bold",
    size = 2.9,
    label.size = 0.10,
    label.padding = unit(0.11, "lines"),
    lineheight = 0.9,
    alpha = 0.98,
    show.legend = FALSE
  ) +
  
  # Etiquetas finales directas
  geom_text(
    data = labels_final_tamano,
    aes(
      x = anio_label,
      y = ymid,
      label = label,
      color = tamano_empresa
    ),
    hjust = 0,
    fontface = "bold",
    size = 3.4,
    lineheight = 0.9,
    show.legend = FALSE
  ) +
  
  # Guías hacia etiquetas finales
  geom_segment(
    data = labels_final_tamano,
    aes(
      x = anio_final_tamano_comp,
      xend = anio_label - 0.08,
      y = ymid,
      yend = ymid,
      color = tamano_empresa
    ),
    linewidth = 0.50,
    alpha = 0.80,
    show.legend = FALSE
  ) +
  
  scale_fill_manual(
    values = colores_tamano
  ) +
  scale_color_manual(
    values = colores_tamano
  ) +
  scale_x_continuous(
    breaks = sort(unique(serie_comp_tamano_area$anio)),
    limits = c(anio_inicio_tamano_comp, anio_final_tamano_comp + 2.4)
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 1),
    breaks = seq(0, 1, 0.25),
    expand = expansion(mult = c(0, 0))
  ) +
  coord_cartesian(clip = "off") +
  labs(
    title = paste0(
      "Composición porcentual del número de trabajadores según tamaño de empresa, ",
      anio_inicio_tamano_comp,
      "–",
      anio_final_tamano_comp
    ),
    subtitle = "Participación porcentual sobre el total de ocupados. Cálculo ponderado por factores de expansión",
    x = "",
    y = "Participación en el total de trabajadores",
    fill = NULL,
    color = NULL
  ) +
  theme_classic(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    axis.text.y = element_text(size = 10),
    legend.position = "none",
    panel.grid.major.y = element_line(color = "gray88", linewidth = 0.35),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 160, 10, 10)
  )

g_composicion_tamano_area

#========================================================
# GRÁFICO. Composición ocupacional por departamento
# 2010 y 2025 por separado
# Versión corregida
#========================================================

#--------------------------------------------------------
# 1. Configuración general
#--------------------------------------------------------

anio_inicio_comp_ocup_depto <- 2010

anio_final_comp_ocup_depto <- geih %>%
  filter(anio >= anio_inicio_comp_ocup_depto) %>%
  summarise(
    anio_final = max(anio, na.rm = TRUE)
  ) %>%
  pull(anio_final)

deptos_seleccionados_comp <- c(
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

ocupaciones_seleccionadas <- c(
  "Empleado particular",
  "Empleado gobierno",
  "Servicio doméstico",
  "Cuenta propia",
  "Patrón/empleador"
)

orden_ocupacion_comp <- c(
  "Empleado particular",
  "Cuenta propia",
  "Empleado gobierno",
  "Patrón/empleador",
  "Servicio doméstico",
  "Otras posiciones"
)

colores_ocupacion_comp <- c(
  "Empleado particular" = "darkblue",
  "Cuenta propia"      = "#E59F00",
  "Empleado gobierno"  = "#0B7285",
  "Patrón/empleador"   = "#6D597A",
  "Servicio doméstico" = "#A61E4D",
  "Otras posiciones"   = "#9AA5B1"
)

#--------------------------------------------------------
# 2. Preparar datos corregidos
#--------------------------------------------------------

serie_comp_ocup_depto <- geih %>%
  filter(
    anio %in% c(anio_inicio_comp_ocup_depto, anio_final_comp_ocup_depto),
    !is.na(depto_label),
    depto_label %in% deptos_seleccionados_comp,
    !is.na(fex),
    fex > 0
  ) %>%
  mutate(
    ocupacion_label_limpia = stringr::str_squish(as.character(ocupacion_label)),
    
    ocupacion_comp = case_when(
      ocupacion_label_limpia == "Empleado particular" ~ "Empleado particular",
      ocupacion_label_limpia == "Empleado gobierno" ~ "Empleado gobierno",
      ocupacion_label_limpia == "Servicio doméstico" ~ "Servicio doméstico",
      ocupacion_label_limpia == "Cuenta propia" ~ "Cuenta propia",
      ocupacion_label_limpia == "Patrón/empleador" ~ "Patrón/empleador",
      TRUE ~ "Otras posiciones"
    ),
    
    ocupacion_comp = factor(
      ocupacion_comp,
      levels = orden_ocupacion_comp
    )
  ) %>%
  group_by(anio, depto_label, ocupacion_comp) %>%
  summarise(
    observaciones = n(),
    trabajadores_expandidos = sum(fex, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  tidyr::complete(
    anio,
    depto_label,
    ocupacion_comp = factor(orden_ocupacion_comp, levels = orden_ocupacion_comp),
    fill = list(
      observaciones = 0,
      trabajadores_expandidos = 0
    )
  ) %>%
  group_by(anio, depto_label) %>%
  mutate(
    total_depto = sum(trabajadores_expandidos, na.rm = TRUE),
    participacion = if_else(
      total_depto > 0,
      trabajadores_expandidos / total_depto,
      0
    )
  ) %>%
  ungroup()

#--------------------------------------------------------
# 3. Verificación rápida de Bogotá en 2010
#--------------------------------------------------------

serie_comp_ocup_depto %>%
  filter(
    anio == anio_inicio_comp_ocup_depto,
    depto_label == "Bogotá D.C."
  ) %>%
  arrange(ocupacion_comp) %>%
  select(
    anio,
    depto_label,
    ocupacion_comp,
    trabajadores_expandidos,
    participacion
  )

#--------------------------------------------------------
# 4. Orden de departamentos
# Según participación de empleado particular en 2025
#--------------------------------------------------------

orden_depto_comp_ocup <- serie_comp_ocup_depto %>%
  filter(
    anio == anio_final_comp_ocup_depto,
    ocupacion_comp == "Empleado particular"
  ) %>%
  arrange(participacion) %>%
  pull(depto_label)

serie_comp_ocup_depto <- serie_comp_ocup_depto %>%
  mutate(
    depto_label = factor(
      depto_label,
      levels = orden_depto_comp_ocup
    )
  )

#--------------------------------------------------------
# 5. Función para graficar un año
#--------------------------------------------------------

crear_grafico_comp_ocup_depto <- function(data, anio_objetivo) {
  
  data_plot <- data %>%
    filter(anio == anio_objetivo) %>%
    mutate(
      label = if_else(
        participacion >= 0.07,
        percent(participacion, accuracy = 0.1),
        ""
      )
    )
  
  ggplot(
    data_plot,
    aes(
      x = depto_label,
      y = participacion,
      fill = ocupacion_comp
    )
  ) +
    geom_col(
      width = 0.72,
      alpha = 0.96,
      color = "white",
      linewidth = 0.25
    ) +
    geom_text(
      aes(label = label),
      position = position_stack(vjust = 0.5),
      color = "white",
      fontface = "bold",
      size = 3.0
    ) +
    coord_flip() +
    coord_cartesian(ylim = c(0, 1), clip = "off") +
    scale_fill_manual(
      values = colores_ocupacion_comp,
      drop = FALSE
    ) +
    scale_y_continuous(
      labels = percent_format(accuracy = 1),
      breaks = seq(0, 1, 0.25),
      expand = expansion(mult = c(0, 0))
    ) +
    labs(
      title = paste0(
        "Composición ocupacional del empleo por departamento, ",
        anio_objetivo
      ),
      subtitle = "Participación porcentual dentro del total de ocupados de cada departamento. Cálculo ponderado por factores de expansión",
      x = "Departamento",
      y = "Participación dentro del empleo departamental",
      fill = "Posición ocupacional"
    ) +
    theme_classic(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 15),
      plot.subtitle = element_text(size = 11),
      axis.title = element_text(face = "bold"),
      axis.text.y = element_text(size = 10),
      axis.text.x = element_text(size = 10),
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      legend.text = element_text(face = "bold", size = 9),
      panel.grid.major.x = element_line(color = "gray88", linewidth = 0.35),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      plot.margin = margin(10, 35, 10, 10)
    )
}

#--------------------------------------------------------
# 6. Crear gráficos separados
#--------------------------------------------------------

g_comp_ocup_depto_2010 <- crear_grafico_comp_ocup_depto(
  data = serie_comp_ocup_depto,
  anio_objetivo = anio_inicio_comp_ocup_depto
)

g_comp_ocup_depto_2025 <- crear_grafico_comp_ocup_depto(
  data = serie_comp_ocup_depto,
  anio_objetivo = anio_final_comp_ocup_depto
)

g_comp_ocup_depto_2010
g_comp_ocup_depto_2025

