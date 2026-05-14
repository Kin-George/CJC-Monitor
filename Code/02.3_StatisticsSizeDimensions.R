# Ingreso laboral por hora promedio segun dimensiones
library(readxl)
library(dplyr)
library(purrr)
library(ggplot2)
library(scales)
library(stringr)
library(readr)
library(tibble)
setwd("~/Trabajo-Profesional/Javeriana")
ruta_graficos <- "Paper\figures"  # <-- cambia esta ruta por la que tú quieras
options(scipen = 999)

#========================================================
# 1. Función para limpiar números aunque vengan como texto
#========================================================

num_clean <- function(x) {
  if (is.numeric(x)) return(x)
  
  x <- as.character(x)
  x <- str_replace_all(x, "\\s", "")
  
  # Detecta formato español: 6.674,5
  if (any(str_detect(x, "\\.\\d{3},\\d+"), na.rm = TRUE)) {
    parse_number(
      x,
      locale = locale(grouping_mark = ".", decimal_mark = ",")
    )
  } else {
    parse_number(
      x,
      locale = locale(grouping_mark = ",", decimal_mark = ".")
    )
  }
}

#========================================================
# 2. Función segura para promedio ponderado
#========================================================

wmean_safe <- function(x, w) {
  ok <- !is.na(x) & !is.na(w) & w > 0
  if (sum(ok) == 0) return(NA_real_)
  weighted.mean(x[ok], w[ok])
}

#========================================================
# Etiquetas cortas de sectores
#========================================================

crear_sector_corto <- function(seccion) {
  case_when(
    seccion == "A" ~ "A - Agro y pesca",
    seccion == "B" ~ "B - Minas",
    seccion == "C" ~ "C - Manufactura",
    seccion == "D" ~ "D - Electricidad y gas",
    seccion == "E" ~ "E - Agua y residuos",
    seccion == "F" ~ "F - Construcción",
    seccion == "G" ~ "G - Comercio",
    seccion == "H" ~ "H - Transporte",
    seccion == "I" ~ "I - Alojamiento y comida",
    seccion == "J" ~ "J - Información y comunicaciones",
    seccion == "K" ~ "K - Finanzas y seguros",
    seccion == "L" ~ "L - Inmobiliarias",
    seccion == "M" ~ "M - Profesionales y técnicas",
    seccion == "N" ~ "N - Servicios administrativos",
    seccion == "O" ~ "O - Administración pública",
    seccion == "P" ~ "P - Educación",
    seccion == "Q" ~ "Q - Salud",
    seccion == "R" ~ "R - Arte y recreación",
    seccion == "S" ~ "S - Otros servicios",
    seccion == "T" ~ "T - Hogares empleadores",
    seccion == "U" ~ "U - Org. extraterritoriales",
    TRUE ~ seccion
  )
}

#========================================================
# Función general para gráfico de línea
#========================================================

grafico_linea_ingreso <- function(data, titulo, subtitulo, xlab) {
  
  ggplot(data, aes(x = orden, y = ingreso_hora_prom)) +
    geom_line(color = "black", linewidth = 0.9) +
    geom_point(color = "darkgreen", size = 3) +
    geom_text(
      aes(label = comma(ingreso_hora_prom, accuracy = 1)),
      vjust = -0.8,
      size = 3.4
    ) +
    scale_x_continuous(
      breaks = data$orden,
      labels = data$categoria
    ) +
    scale_y_continuous(
      labels = comma,
      expand = expansion(mult = c(0.08, 0.18))
    ) +
    labs(
      title = titulo,
      subtitle = subtitulo,
      x = xlab,
      y = "Ingreso laboral por hora promedio"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 11),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
      panel.grid.minor = element_blank()
    )
}

base_tamano <- read_excel("Datos/Processed/INGLABO_secciones_tamano_empresa_2025.xlsx",
                          sheet = "todas_secciones")

names(base_tamano) <- c(
  "seccion",
  "desc_seccion",
  "tamano_cod",
  "tamano_empresa",
  "trabajadores",
  "ingreso_anual_prom",
  "ingreso_mensual_prom",
  "ingreso_anual_med",
  "ingreso_mensual_med",
  "ingreso_hora_prom",
  "ingreso_hora_med",
  "total_ingreso_anual",
  "n_obs"
)

base_educ <- read_excel("Datos/Processed/INGLABO_seccion_educacion_tamano_2025.xlsx",
                        sheet = "todas_secciones")

names(base_educ) <- c(
  "seccion",
  "desc_seccion",
  "nivel_educ_cod",
  "nivel_educativo",
  "tamano_cod",
  "tamano_empresa",
  "trabajadores",
  "ingreso_anual_prom",
  "ingreso_mensual_prom",
  "ingreso_anual_med",
  "ingreso_mensual_med",
  "ingreso_hora_prom",
  "ingreso_hora_med",
  "total_ingreso_anual",
  "n_obs"
)

base_formalidad <- read_excel("Datos/Processed/INGLABO_seccion_tamano_formalidad_2025.xlsx",
                              sheet = "todas_secciones")

names(base_formalidad) <- c(
  "seccion",
  "desc_seccion",
  "tamano_cod",
  "tamano_empresa",
  "formalidad_cod",
  "formalidad",
  "trabajadores",
  "ingreso_anual_prom",
  "ingreso_mensual_prom",
  "ingreso_anual_med",
  "ingreso_mensual_med",
  "ingreso_hora_prom",
  "ingreso_hora_med",
  "total_ingreso_anual",
  "n_obs"
)

graf_tamano_data <- base_tamano %>%
  mutate(
    tamano_cod = num_clean(tamano_cod),
    trabajadores = num_clean(trabajadores),
    peso_trab = trabajadores,
    ingreso_hora_prom = num_clean(ingreso_hora_prom)
  ) %>%
  filter(
    !is.na(tamano_cod),
    tamano_cod >= 1,
    tamano_cod <= 10,
    !is.na(ingreso_hora_prom),
    !is.na(peso_trab),
    peso_trab > 0
  ) %>%
  group_by(tamano_cod, tamano_empresa) %>%
  summarise(
    ingreso_hora_prom = wmean_safe(ingreso_hora_prom, peso_trab),
    trabajadores = sum(peso_trab, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(tamano_cod) %>%
  mutate(
    orden = row_number(),
    categoria = tamano_empresa
  )

g_tamano <- ggplot(
  graf_tamano_data,
  aes(x = orden, y = ingreso_hora_prom)
) +
  geom_line(color = "black", linewidth = 0.9) +
  geom_point(color = "darkgreen", size = 3) +
  geom_text(
    aes(label = comma(ingreso_hora_prom, accuracy = 1)),
    vjust = -0.8,
    size = 3.4
  ) +
  scale_x_continuous(
    breaks = graf_tamano_data$orden,
    labels = graf_tamano_data$categoria
  ) +
  scale_y_continuous(
    labels = comma,
    expand = expansion(mult = c(0.08, 0.18))
  ) +
  labs(
    title = "Ingreso laboral por hora promedio según tamaño de empresa,2025",
    subtitle = "Promedio ponderado por trabajadores expandidos",
    x = "Tamaño de empresa",
    y = "Ingreso laboral por hora promedio"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    panel.grid.minor = element_blank()
  )

g_tamano

# Educacion
graf_educ_tamano <- base_educ %>%
  mutate(
    nivel_educ_cod = num_clean(nivel_educ_cod),
    tamano_cod = num_clean(tamano_cod),
    trabajadores = num_clean(trabajadores),
    ingreso_hora_prom = num_clean(ingreso_hora_prom),
    peso_trab = trabajadores
  ) %>%
  filter(
    !is.na(nivel_educ_cod),
    nivel_educ_cod >= 1,
    nivel_educ_cod <= 13,
    !is.na(tamano_cod),
    tamano_cod >= 1,
    tamano_cod <= 10,
    !is.na(ingreso_hora_prom),
    !is.na(peso_trab),
    peso_trab > 0
  ) %>%
  group_by(nivel_educ_cod, nivel_educativo, tamano_cod, tamano_empresa) %>%
  summarise(
    trabajadores = sum(peso_trab, na.rm = TRUE),
    ingreso_hora_prom = wmean_safe(ingreso_hora_prom, peso_trab),
    .groups = "drop"
  ) %>%
  arrange(nivel_educ_cod, tamano_cod)

g_educ_tamano <- ggplot(
  graf_educ_tamano,
  aes(x = tamano_cod, y = ingreso_hora_prom, group = 1)
) +
  geom_line(color = "black", linewidth = 0.8) +
  geom_point(color = "darkgreen", size = 2.5) +
  facet_wrap(~ nivel_educativo, scales = "free_y") +
  scale_x_continuous(
    breaks = sort(unique(graf_educ_tamano$tamano_cod)),
    labels = unique(graf_educ_tamano$tamano_empresa)[order(unique(graf_educ_tamano$tamano_cod))]
  ) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Ingreso laboral por hora según tamaño de empresa, por nivel educativo",
    subtitle = "Promedio ponderado por trabajadores expandidos, 2025",
    x = "Tamaño de empresa",
    y = "Ingreso laboral por hora promedio"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text = element_text(face = "bold", size = 8),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
    panel.grid.minor = element_blank()
  )

g_educ_tamano

# Sector
graf_sector_tamano <- base_tamano %>%
  mutate(
    tamano_cod = num_clean(tamano_cod),
    trabajadores = num_clean(trabajadores),
    ingreso_hora_prom = num_clean(ingreso_hora_prom),
    peso_trab = trabajadores,
    sector_label_corto = crear_sector_corto(seccion)
  ) %>%
  filter(
    !is.na(seccion),
    !is.na(tamano_cod),
    tamano_cod >= 1,
    tamano_cod <= 10,
    !is.na(ingreso_hora_prom),
    !is.na(peso_trab),
    peso_trab > 0
  ) %>%
  group_by(seccion, sector_label_corto, tamano_cod, tamano_empresa) %>%
  summarise(
    trabajadores = sum(peso_trab, na.rm = TRUE),
    ingreso_hora_prom = wmean_safe(ingreso_hora_prom, peso_trab),
    .groups = "drop"
  ) %>%
  arrange(seccion, tamano_cod)

g_sector_tamano <- ggplot(
  graf_sector_tamano,
  aes(x = tamano_cod, y = ingreso_hora_prom, group = 1)
) +
  geom_line(color = "black", linewidth = 0.8) +
  geom_point(color = "darkgreen", size = 2.3) +
  facet_wrap(~ sector_label_corto, scales = "free_y") +
  scale_x_continuous(
    breaks = sort(unique(graf_sector_tamano$tamano_cod)),
    labels = unique(graf_sector_tamano$tamano_empresa)[order(unique(graf_sector_tamano$tamano_cod))]
  ) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Ingreso laboral por hora según tamaño de empresa, por sector económico",
    subtitle = "Promedio ponderado por trabajadores expandidos, 2025",
    x = "Tamaño de empresa",
    y = "Ingreso laboral por hora promedio"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text = element_text(face = "bold", size = 8),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
    panel.grid.minor = element_blank()
  )

g_sector_tamano

# Por formalidad

graf_formalidad_tamano <- base_formalidad %>%
  mutate(
    formalidad_cod = num_clean(formalidad_cod),
    tamano_cod = num_clean(tamano_cod),
    trabajadores = num_clean(trabajadores),
    ingreso_hora_prom = num_clean(ingreso_hora_prom),
    peso_trab = trabajadores
  ) %>%
  filter(
    !is.na(formalidad_cod),
    !is.na(tamano_cod),
    tamano_cod >= 1,
    tamano_cod <= 10,
    !is.na(ingreso_hora_prom),
    !is.na(peso_trab),
    peso_trab > 0
  ) %>%
  group_by(formalidad_cod, formalidad, tamano_cod, tamano_empresa) %>%
  summarise(
    trabajadores = sum(peso_trab, na.rm = TRUE),
    ingreso_hora_prom = wmean_safe(ingreso_hora_prom, peso_trab),
    .groups = "drop"
  ) %>%
  arrange(formalidad_cod, tamano_cod) %>%
  mutate(
    formalidad = str_wrap(formalidad, width = 30)
  )

g_formalidad_tamano <- ggplot(
  graf_formalidad_tamano,
  aes(x = tamano_cod, y = ingreso_hora_prom, group = 1)
) +
  geom_line(color = "black", linewidth = 0.8) +
  geom_point(color = "darkgreen", size = 2.8) +
  facet_wrap(~ formalidad, scales = "free_y") +
  scale_x_continuous(
    breaks = sort(unique(graf_formalidad_tamano$tamano_cod)),
    labels = unique(graf_formalidad_tamano$tamano_empresa)[order(unique(graf_formalidad_tamano$tamano_cod))]
  ) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Ingreso laboral por hora según tamaño de empresa, por formalidad laboral",
    subtitle = "Promedio ponderado por trabajadores expandidos",
    x = "Tamaño de empresa",
    y = "Ingreso laboral por hora promedio"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text = element_text(face = "bold", size = 9),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    panel.grid.minor = element_blank()
  )

g_formalidad_tamano

# Buburja por tamaño
tamano_labels <- c(
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

# Nivel educativo
graf_educ_tamano <- base_educ %>%
  mutate(
    nivel_educ_cod = num_clean(nivel_educ_cod),
    tamano_cod = num_clean(tamano_cod),
    trabajadores = num_clean(trabajadores),
    ingreso_hora_prom = num_clean(ingreso_hora_prom),
    peso_trab = trabajadores
  ) %>%
  filter(
    !is.na(nivel_educ_cod),
    nivel_educ_cod >= 1,
    nivel_educ_cod <= 13,
    !is.na(tamano_cod),
    tamano_cod >= 1,
    tamano_cod <= 10,
    !is.na(ingreso_hora_prom),
    !is.na(peso_trab),
    peso_trab > 0
  ) %>%
  group_by(nivel_educ_cod, nivel_educativo, tamano_cod) %>%
  summarise(
    trabajadores = sum(peso_trab, na.rm = TRUE),
    ingreso_hora_prom = wmean_safe(ingreso_hora_prom, peso_trab),
    .groups = "drop"
  ) %>%
  arrange(nivel_educ_cod, tamano_cod)

g_educ_tamano <- ggplot(
  graf_educ_tamano,
  aes(x = tamano_cod, y = ingreso_hora_prom, group = 1)
) +
  geom_line(color = "black", linewidth = 0.8) +
  geom_point(
    aes(size = trabajadores),
    color = "darkgreen",
    alpha = 0.75
  ) +
  facet_wrap(~ nivel_educativo, scales = "free_y") +
  scale_x_continuous(
    breaks = 1:10,
    labels = tamano_labels
  ) +
  scale_y_continuous(labels = comma) +
  scale_size_area(
    max_size = 8,
    labels = comma,
    name = "Trabajadores expandidos"
  ) +
  labs(
    title = "Ingreso laboral por hora según tamaño de empresa, por nivel educativo",
    subtitle = "El tamaño de cada burbuja representa el número de trabajadores, 2025",
    x = "Tamaño de empresa",
    y = "Ingreso laboral por hora promedio"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text = element_text(face = "bold", size = 8),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

g_educ_tamano

# Sectores
graf_sector_tamano <- base_tamano %>%
  mutate(
    tamano_cod = num_clean(tamano_cod),
    trabajadores = num_clean(trabajadores),
    ingreso_hora_prom = num_clean(ingreso_hora_prom),
    peso_trab = trabajadores,
    sector_label_corto = crear_sector_corto(seccion)
  ) %>%
  filter(
    !is.na(seccion),
    !is.na(tamano_cod),
    tamano_cod >= 1,
    tamano_cod <= 10,
    !is.na(ingreso_hora_prom),
    !is.na(peso_trab),
    peso_trab > 0
  ) %>%
  group_by(seccion, sector_label_corto, tamano_cod) %>%
  summarise(
    trabajadores = sum(peso_trab, na.rm = TRUE),
    ingreso_hora_prom = wmean_safe(ingreso_hora_prom, peso_trab),
    .groups = "drop"
  ) %>%
  arrange(seccion, tamano_cod)

g_sector_tamano <- ggplot(
  graf_sector_tamano,
  aes(x = tamano_cod, y = ingreso_hora_prom, group = 1)
) +
  geom_line(color = "black", linewidth = 0.8) +
  geom_point(
    aes(size = trabajadores),
    color = "darkgreen",
    alpha = 0.75
  ) +
  facet_wrap(~ sector_label_corto, scales = "free_y") +
  scale_x_continuous(
    breaks = 1:10,
    labels = tamano_labels
  ) +
  scale_y_continuous(labels = comma) +
  scale_size_area(
    max_size = 7,
    labels = comma,
    name = "Trabajadores expandidos"
  ) +
  labs(
    title = "Ingreso laboral por hora según tamaño de empresa, por sector económico",
    subtitle = "El tamaño de cada burbuja representa el número de trabajadores",
    x = "Tamaño de empresa",
    y = "Ingreso laboral por hora promedio"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text = element_text(face = "bold", size = 8),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

g_sector_tamano

# Formalidad
graf_formalidad_tamano <- base_formalidad %>%
  mutate(
    formalidad_cod = num_clean(formalidad_cod),
    tamano_cod = num_clean(tamano_cod),
    trabajadores = num_clean(trabajadores),
    ingreso_hora_prom = num_clean(ingreso_hora_prom),
    peso_trab = trabajadores
  ) %>%
  filter(
    !is.na(formalidad_cod),
    !is.na(tamano_cod),
    tamano_cod >= 1,
    tamano_cod <= 10,
    !is.na(ingreso_hora_prom),
    !is.na(peso_trab),
    peso_trab > 0
  ) %>%
  group_by(formalidad_cod, formalidad, tamano_cod) %>%
  summarise(
    trabajadores = sum(peso_trab, na.rm = TRUE),
    ingreso_hora_prom = wmean_safe(ingreso_hora_prom, peso_trab),
    .groups = "drop"
  ) %>%
  arrange(formalidad_cod, tamano_cod) %>%
  mutate(
    formalidad = str_wrap(formalidad, width = 30)
  )

g_formalidad_tamano <- ggplot(
  graf_formalidad_tamano,
  aes(x = tamano_cod, y = ingreso_hora_prom, group = 1)
) +
  geom_line(color = "black", linewidth = 0.8) +
  geom_point(
    aes(size = trabajadores),
    color = "darkgreen",
    alpha = 0.75
  ) +
  facet_wrap(~ formalidad, scales = "free_y") +
  scale_x_continuous(
    breaks = 1:10,
    labels = tamano_labels
  ) +
  scale_y_continuous(labels = comma) +
  scale_size_area(
    max_size = 8,
    labels = comma,
    name = "Trabajadores expandidos"
  ) +
  labs(
    title = "Ingreso laboral por hora según tamaño de empresa, por formalidad laboral",
    subtitle = "El tamaño de cada burbuja representa el número de trabajadores",
    x = "Tamaño de empresa",
    y = "Ingreso laboral por hora promedio"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text = element_text(face = "bold", size = 9),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

g_formalidad_tamano

# Tamaño

graf_tamano_data <- base_tamano %>%
  mutate(
    tamano_cod = num_clean(tamano_cod),
    trabajadores = num_clean(trabajadores),
    ingreso_hora_prom = num_clean(ingreso_hora_prom),
    peso_trab = trabajadores
  ) %>%
  filter(
    !is.na(tamano_cod),
    tamano_cod >= 1,
    tamano_cod <= 10,
    !is.na(ingreso_hora_prom),
    !is.na(peso_trab),
    peso_trab > 0
  ) %>%
  group_by(tamano_cod, tamano_empresa) %>%
  summarise(
    trabajadores = sum(peso_trab, na.rm = TRUE),
    ingreso_hora_prom = wmean_safe(ingreso_hora_prom, peso_trab),
    .groups = "drop"
  ) %>%
  arrange(tamano_cod)

g_tamano_burbujas <- ggplot(
  graf_tamano_data,
  aes(x = tamano_cod, y = ingreso_hora_prom, group = 1)
) +
  geom_line(color = "black", linewidth = 0.9) +
  geom_point(
    aes(size = trabajadores),
    color = "darkgreen",
    alpha = 0.75
  ) +
  geom_text(
    aes(label = comma(ingreso_hora_prom, accuracy = 1)),
    vjust = -0.9,
    size = 3.5
  ) +
  scale_x_continuous(
    breaks = 1:10,
    labels = tamano_labels
  ) +
  scale_y_continuous(
    labels = comma,
    expand = expansion(mult = c(0.08, 0.18))
  ) +
  scale_size_area(
    max_size = 12,
    labels = comma,
    name = "Trabajadores expandidos"
  ) +
  labs(
    title = "Ingreso laboral por hora promedio según tamaño de empresa",
    subtitle = "El tamaño de cada burbuja representa el número de trabajadores",
    x = "Tamaño de empresa",
    y = "Ingreso laboral por hora promedio"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

g_tamano_burbujas

### Definicion oliver
# Nivel educativo
graf_educ_tamano_rel <- graf_educ_tamano %>%
  group_by(nivel_educ_cod, nivel_educativo) %>%
  mutate(
    participacion = trabajadores / sum(trabajadores, na.rm = TRUE)
  ) %>%
  ungroup()

g_educ_tamano_rel <- ggplot(
  graf_educ_tamano_rel,
  aes(x = tamano_cod, y = ingreso_hora_prom, group = 1)
) +
  geom_line(color = "black", linewidth = 0.8) +
  geom_point(
    aes(size = participacion),
    color = "darkgreen",
    alpha = 0.75
  ) +
  facet_wrap(~ nivel_educativo, scales = "free_y") +
  scale_x_continuous(
    breaks = 1:10,
    labels = tamano_labels
  ) +
  scale_y_continuous(labels = comma) +
  scale_size_area(
    max_size = 8,
    labels = percent,
    name = "% dentro del nivel educativo"
  ) +
  labs(
    title = "Ingreso laboral por hora según tamaño de empresa, por nivel educativo",
    subtitle = "La burbuja representa el porcentaje de trabajadores de cada nivel educativo en cada tamaño de empresa",
    x = "Tamaño de empresa",
    y = "Ingreso laboral por hora promedio"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text = element_text(face = "bold", size = 8),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

g_educ_tamano_rel

#  Sector economico
graf_sector_tamano_rel <- graf_sector_tamano %>%
  group_by(seccion, sector_label_corto) %>%
  mutate(
    participacion = trabajadores / sum(trabajadores, na.rm = TRUE)
  ) %>%
  ungroup()

g_sector_tamano_rel <- ggplot(
  graf_sector_tamano_rel,
  aes(x = tamano_cod, y = ingreso_hora_prom, group = 1)
) +
  geom_line(color = "black", linewidth = 0.8) +
  geom_point(
    aes(size = participacion),
    color = "darkgreen",
    alpha = 0.75
  ) +
  facet_wrap(~ sector_label_corto, scales = "free_y") +
  scale_x_continuous(
    breaks = 1:10,
    labels = tamano_labels
  ) +
  scale_y_continuous(labels = comma) +
  scale_size_area(
    max_size = 7,
    labels = percent,
    name = "% dentro del sector"
  ) +
  labs(
    title = "Ingreso laboral por hora según tamaño de empresa, por sector económico",
    subtitle = "La burbuja representa el porcentaje de trabajadores de cada sector en cada tamaño de empresa",
    x = "Tamaño de empresa",
    y = "Ingreso laboral por hora promedio"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text = element_text(face = "bold", size = 8),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

g_sector_tamano_rel

# Formalidad
graf_formalidad_tamano_rel <- graf_formalidad_tamano %>%
  group_by(formalidad_cod, formalidad) %>%
  mutate(
    participacion = trabajadores / sum(trabajadores, na.rm = TRUE)
  ) %>%
  ungroup()

g_formalidad_tamano_rel <- ggplot(
  graf_formalidad_tamano_rel,
  aes(x = tamano_cod, y = ingreso_hora_prom, group = 1)
) +
  geom_line(color = "black", linewidth = 0.8) +
  geom_point(
    aes(size = participacion),
    color = "darkgreen",
    alpha = 0.75
  ) +
  facet_wrap(~ formalidad, scales = "free_y") +
  scale_x_continuous(
    breaks = 1:10,
    labels = tamano_labels
  ) +
  scale_y_continuous(labels = comma) +
  scale_size_area(
    max_size = 8,
    labels = percent,
    name = "% dentro de formalidad"
  ) +
  labs(
    title = "Ingreso laboral por hora según tamaño de empresa, por formalidad laboral",
    subtitle = "La burbuja representa el porcentaje de trabajadores de cada grupo de formalidad en cada tamaño de empresa",
    x = "Tamaño de empresa",
    y = "Ingreso laboral por hora promedio"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text = element_text(face = "bold", size = 9),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

g_formalidad_tamano_rel

# General
graf_tamano_rel <- base_tamano %>%
  mutate(
    tamano_cod = num_clean(tamano_cod),
    trabajadores = num_clean(trabajadores),
    ingreso_hora_prom = num_clean(ingreso_hora_prom),
    peso_trab = trabajadores
  ) %>%
  filter(
    !is.na(tamano_cod),
    tamano_cod >= 1,
    tamano_cod <= 10,
    !is.na(ingreso_hora_prom),
    !is.na(peso_trab),
    peso_trab > 0
  ) %>%
  group_by(tamano_cod, tamano_empresa) %>%
  summarise(
    trabajadores = sum(peso_trab, na.rm = TRUE),
    ingreso_hora_prom = wmean_safe(ingreso_hora_prom, peso_trab),
    .groups = "drop"
  ) %>%
  ungroup() %>%
  mutate(
    participacion = trabajadores / sum(trabajadores, na.rm = TRUE)
  ) %>%
  arrange(tamano_cod)

g_tamano_rel <- ggplot(
  graf_tamano_rel,
  aes(x = tamano_cod, y = ingreso_hora_prom, group = 1)
) +
  geom_line(color = "black", linewidth = 0.9) +
  geom_point(
    aes(size = participacion),
    color = "darkgreen",
    alpha = 0.75
  ) +
  geom_text(
    aes(label = percent(participacion, accuracy = 0.1)),
    vjust = -1,
    size = 3.5
  ) +
  scale_x_continuous(
    breaks = 1:10,
    labels = tamano_labels
  ) +
  scale_y_continuous(
    labels = comma,
    expand = expansion(mult = c(0.08, 0.18))
  ) +
  scale_size_area(
    max_size = 12,
    labels = percent,
    name = "% del empleo total"
  ) +
  labs(
    title = "Ingreso laboral por hora según tamaño de empresa",
    subtitle = "La burbuja representa la participación de cada tamaño en el empleo total",
    x = "Tamaño de empresa",
    y = "Ingreso laboral por hora promedio"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

g_tamano_rel

# Ingreso laboral anual serie 2008 - 2025
#========================================================
# 1. Función para limpiar números
#========================================================

num_clean <- function(x) {
  if (is.numeric(x)) return(x)
  
  x <- as.character(x)
  x <- str_replace_all(x, "\\s", "")
  
  if (any(str_detect(x, "\\.\\d{3},\\d+"), na.rm = TRUE)) {
    parse_number(
      x,
      locale = locale(grouping_mark = ".", decimal_mark = ",")
    )
  } else {
    parse_number(
      x,
      locale = locale(grouping_mark = ",", decimal_mark = ".")
    )
  }
}

#========================================================
# 2. Años disponibles
#========================================================

anios <- c(2008:2019, 2021:2025)

# Si luego tienes 2020, cambia por:
# anios <- 2008:2025

#========================================================
# 3. Función para leer cada archivo anual
#========================================================

leer_consolidado_anual <- function(anio) {
  
  ruta <- paste0("Datos/Processed/consolidadosector_", anio, ".xlsx")
  
  df <- read_excel(ruta, sheet = 1)
  
  # Renombrar por posición para evitar problemas de tildes/codificación
  names(df) <- c(
    "seccion",
    "desc_seccion",
    "trabajadores",
    "ingreso_anual_prom",
    "ingreso_mensual_prom",
    "ingreso_anual_med",
    "ingreso_mensual_med",
    "ingreso_hora_prom",
    "ingreso_hora_med",
    "total_ingreso_anual",
    "n_obs"
  )
  
  df %>%
    mutate(
      anio = anio,
      trabajadores = num_clean(trabajadores),
      ingreso_anual_prom = num_clean(ingreso_anual_prom),
      ingreso_mensual_prom = num_clean(ingreso_mensual_prom),
      ingreso_anual_med = num_clean(ingreso_anual_med),
      ingreso_mensual_med = num_clean(ingreso_mensual_med),
      ingreso_hora_prom = num_clean(ingreso_hora_prom),
      ingreso_hora_med = num_clean(ingreso_hora_med),
      total_ingreso_anual = num_clean(total_ingreso_anual),
      n_obs = num_clean(n_obs)
    ) %>%
    select(
      anio,
      seccion,
      desc_seccion,
      trabajadores,
      ingreso_anual_prom,
      ingreso_mensual_prom,
      ingreso_anual_med,
      ingreso_mensual_med,
      ingreso_hora_prom,
      ingreso_hora_med,
      total_ingreso_anual,
      n_obs
    )
}

#========================================================
# 4. Consolidar todas las bases
#========================================================

consolidado_geih_2008_2025 <- map_dfr(
  anios,
  leer_consolidado_anual
)

serie_ingreso_hora <- consolidado_geih_2008_2025 %>%
  filter(
    !is.na(ingreso_hora_prom),
    !is.na(trabajadores),
    trabajadores > 0
  ) %>%
  group_by(anio) %>%
  summarise(
    trabajadores_total = sum(trabajadores, na.rm = TRUE),
    ingreso_hora_promedio = weighted.mean(
      ingreso_hora_prom,
      trabajadores,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  arrange(anio)

#========================================================
# 1. IPC diciembre, base 2018 = 100
#========================================================

ipc_dic <- tibble(
  anio = 2008:2025,
  ipc_dic = c(
    69.80, 71.20, 73.45, 76.19, 78.05, 79.56,
    82.47, 88.05, 93.11, 96.92, 100.00, 103.80,
    105.48, 111.41, 126.03, 137.72, 144.88, 152.27
  )
)

ipc_2025 <- ipc_dic %>%
  filter(anio == 2025) %>%
  pull(ipc_dic)

#========================================================
# 2. Deflactar ingreso laboral por hora a pesos de 2025
#========================================================

serie_ingreso_hora_real <- serie_ingreso_hora %>%
  left_join(ipc_dic, by = "anio") %>%
  mutate(
    factor_a_pesos_2025 = ipc_2025 / ipc_dic,
    ingreso_hora_real_2025 = ingreso_hora_promedio * factor_a_pesos_2025
  ) %>%
  select(
    anio,
    trabajadores_total,
    ipc_dic,
    factor_a_pesos_2025,
    ingreso_hora_nominal = ingreso_hora_promedio,
    ingreso_hora_real_2025
  ) %>%
  arrange(anio)

serie_ingreso_hora_real_formateada <- serie_ingreso_hora_real %>%
  mutate(
    trabajadores_total = comma(trabajadores_total, accuracy = 1),
    ipc_dic = comma(ipc_dic, accuracy = 0.01),
    factor_a_pesos_2025 = comma(factor_a_pesos_2025, accuracy = 0.001),
    ingreso_hora_nominal = comma(ingreso_hora_nominal, accuracy = 1),
    ingreso_hora_real_2025 = comma(ingreso_hora_real_2025, accuracy = 1)
  )

g_ingreso_hora_real_2025 <- ggplot(
  serie_ingreso_hora_real,
  aes(x = anio, y = ingreso_hora_real_2025)
) +
  geom_line(color = "black", linewidth = 0.9) +
  geom_point(color = "darkgreen", size = 3) +
  geom_text(
    aes(label = comma(ingreso_hora_real_2025, accuracy = 1)),
    vjust = -0.8,
    size = 3.1
  ) +
  scale_x_continuous(
    breaks = sort(unique(serie_ingreso_hora_real$anio))
  ) +
  scale_y_continuous(
    labels = comma,
    expand = expansion(mult = c(0.08, 0.18))
  ) +
  labs(
    title = "Ingreso laboral por hora promedio real en Colombia, 2008–2025",
    subtitle = "Valores expresados en pesos constantes de 2025. Deflactado con IPC de diciembre",
    x = "Año",
    y = "Ingreso laboral por hora promedio, pesos de 2025"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  )

g_ingreso_hora_real_2025

#========================================================
# Logaritmo del ingreso laboral por hora real
#========================================================

serie_log_ingreso_real <- serie_ingreso_hora_real %>%
  arrange(anio) %>%
  filter(
    !is.na(ingreso_hora_real_2025),
    ingreso_hora_real_2025 > 0
  ) %>%
  mutate(
    log_ingreso_hora_real = log(ingreso_hora_real_2025)
  )


#========================================================
# Gráfico del logaritmo del ingreso laboral por hora real
#========================================================

g_log_ingreso_real <- ggplot(
  serie_log_ingreso_real,
  aes(x = anio, y = log_ingreso_hora_real)
) +
  geom_line(color = "black", linewidth = 0.9) +
  geom_point(color = "darkgreen", size = 3) +
  geom_text(
    aes(label = round(log_ingreso_hora_real, 2)),
    vjust = -0.8,
    size = 3.2
  ) +
  scale_x_continuous(
    breaks = sort(unique(serie_log_ingreso_real$anio))
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0.08, 0.16))
  ) +
  labs(
    title = "Logaritmo del ingreso laboral por hora real en Colombia",
    subtitle = "Ingreso laboral por hora expresado en pesos constantes de 2025",
    x = "Año",
    y = "Logaritmo del ingreso laboral por hora real"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  )

g_log_ingreso_real

# Serie ingreso por hora por tamaño de empresa en terminos reales
serieSize <- read_excel("Datos/Processed/ingreso_hora_por_anio_tamano_sin_homologar.xlsx")

#========================================================
# 2. Deflactar ingreso laboral por hora
#========================================================

serieSize <- serieSize %>%
  left_join(ipc_dic, by = "anio") %>%
  mutate(
    factor_precios_2025 = ipc_2025 / ipc_dic,
    ingreso_hora_real_2025 = ingreso_hora_promedio * factor_precios_2025
  )

#========================================================
# 3. Labels cortos de tamaño de empresa
#========================================================

serieSize <- serieSize %>%
  mutate(
    tamano_label = case_when(
      tamano_empresa_cod == 1 ~ "Solo",
      tamano_empresa_cod == 2 ~ "2-3",
      tamano_empresa_cod == 3 ~ "4-5",
      tamano_empresa_cod == 4 ~ "6-10",
      tamano_empresa_cod == 5 ~ "11-19",
      tamano_empresa_cod == 6 ~ "20-30",
      tamano_empresa_cod == 7 ~ "31-50",
      tamano_empresa_cod == 8 ~ "51-100",
      
      tamano_var_original == "P6870" & tamano_empresa_cod == 9 ~ "101+",
      tamano_var_original == "P3069" & tamano_empresa_cod == 9 ~ "101-200",
      tamano_var_original == "P3069" & tamano_empresa_cod == 10 ~ "201+",
      
      TRUE ~ paste0("Cat. ", tamano_empresa_cod)
    ),
    
    # Variable para ordenar visualmente
    tamano_orden = case_when(
      tamano_empresa_cod == 1 ~ 1,
      tamano_empresa_cod == 2 ~ 2,
      tamano_empresa_cod == 3 ~ 3,
      tamano_empresa_cod == 4 ~ 4,
      tamano_empresa_cod == 5 ~ 5,
      tamano_empresa_cod == 6 ~ 6,
      tamano_empresa_cod == 7 ~ 7,
      tamano_empresa_cod == 8 ~ 8,
      tamano_var_original == "P6870" & tamano_empresa_cod == 9 ~ 9,
      tamano_var_original == "P3069" & tamano_empresa_cod == 9 ~ 10,
      tamano_var_original == "P3069" & tamano_empresa_cod == 10 ~ 11,
      TRUE ~ 99
    ),
    
    tamano_label = factor(
      tamano_label,
      levels = c(
        "Solo", "2-3", "4-5", "6-10", "11-19",
        "20-30", "31-50", "51-100", "101+",
        "101-200", "201+"
      )
    )
  )

#========================================================
# 4. Participación en el empleo total del año
#========================================================

serieSize <- serieSize %>%
  group_by(anio) %>%
  mutate(
    trabajadores_total_anio = sum(trabajadores, na.rm = TRUE),
    participacion_empleo = trabajadores / trabajadores_total_anio
  ) %>%
  ungroup()

# 2008 vs 2025
serieSize_2008_2025 <- serieSize %>%
  filter(anio %in% c(2008, 2025)) %>%
  mutate(
    tamano_label_chr = as.character(tamano_label),
    
    # Aquí NO estamos calculando todavía.
    # Solo estamos diciendo qué categorías deben pertenecer
    # al mismo grupo comparable.
    tamano_comparable = case_when(
      tamano_label_chr %in% c("101+", "101-200", "201+") ~ "101+",
      TRUE ~ tamano_label_chr
    ),
    
    tamano_orden_comparable = case_when(
      tamano_comparable == "Solo" ~ 1,
      tamano_comparable == "2-3" ~ 2,
      tamano_comparable == "4-5" ~ 3,
      tamano_comparable == "6-10" ~ 4,
      tamano_comparable == "11-19" ~ 5,
      tamano_comparable == "20-30" ~ 6,
      tamano_comparable == "31-50" ~ 7,
      tamano_comparable == "51-100" ~ 8,
      tamano_comparable == "101+" ~ 9,
      TRUE ~ 99
    ),
    
    # Numerador del promedio ponderado
    ingreso_real_x_trabajadores = ingreso_hora_real_2025 * trabajadores
  ) %>%
  filter(
    !is.na(tamano_comparable),
    !is.na(ingreso_hora_real_2025),
    !is.na(trabajadores),
    trabajadores > 0
  ) %>%
  group_by(anio, tamano_comparable, tamano_orden_comparable) %>%
  summarise(
    trabajadores = sum(trabajadores, na.rm = TRUE),
    
    ingreso_real_x_trabajadores = sum(
      ingreso_real_x_trabajadores,
      na.rm = TRUE
    ),
    
    ingreso_hora_real_2025 = ingreso_real_x_trabajadores / trabajadores,
    
    observaciones = sum(observaciones, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(anio) %>%
  mutate(
    trabajadores_total_anio = sum(trabajadores, na.rm = TRUE),
    participacion_empleo = trabajadores / trabajadores_total_anio
  ) %>%
  ungroup() %>%
  arrange(anio, tamano_orden_comparable) %>%
  mutate(
    tamano_comparable = factor(
      tamano_comparable,
      levels = c(
        "Solo", "2-3", "4-5", "6-10", "11-19",
        "20-30", "31-50", "51-100", "101+"
      )
    ),
    anio = factor(anio)
  )

# Nivel educativo
serieEducacion <- read_excel("Datos/Processed/ingreso_hora_anio_educacion_tamano.xlsx", 
                                                 sheet = "02_base_final")

#========================================================
# 1. IPC diciembre para llevar a pesos constantes de 2025
#========================================================

ipc_dic <- tibble(
  anio = 2008:2025,
  ipc_dic = c(
    69.80, 71.20, 73.45, 76.19, 78.05, 79.56,
    82.47, 88.05, 93.11, 96.92, 100.00, 103.80,
    105.48, 111.41, 126.03, 137.72, 144.88, 152.27
  )
)

ipc_2025 <- ipc_dic %>%
  filter(anio == 2025) %>%
  pull(ipc_dic)

#========================================================
# 2. Deflactar ingreso laboral por hora
#========================================================

serieEducacion <- serieEducacion %>%
  left_join(ipc_dic, by = "anio") %>%
  mutate(
    factor_precios_2025 = ipc_2025 / ipc_dic,
    ingreso_hora_real_2025 = ingreso_hora_promedio * factor_precios_2025
  )

#========================================================
# 3. Crear categorías comparables:
#    educación y tamaño de empresa
#========================================================

educ_size_2008_2025 <- serieEducacion %>%
  filter(anio %in% c(2008, 2025)) %>%
  mutate(
    
    #-------------------------
    # Educación comparable
    #-------------------------
    educacion_label_chr = as.character(educacion_label),
    
    educacion_comparable = case_when(
      educacion_label_chr == "Ninguno" ~ "Ninguno",
      educacion_label_chr == "Preescolar" ~ "Preescolar",
      educacion_label_chr == "Básica primaria" ~ "Básica primaria",
      educacion_label_chr == "Básica secundaria" ~ "Básica secundaria",
      
      educacion_label_chr %in% c(
        "Media",
        "Media académica",
        "Media técnica"
      ) ~ "Media",
      
      educacion_label_chr %in% c(
        "Superior o universitaria",
        "Normalista",
        "Técnica profesional",
        "Tecnológica",
        "Universitaria",
        "Especialización",
        "Maestría",
        "Doctorado"
      ) ~ "Superior o universitaria",
      
      TRUE ~ NA_character_
    ),
    
    educacion_orden_comparable = case_when(
      educacion_comparable == "Ninguno" ~ 1,
      educacion_comparable == "Preescolar" ~ 2,
      educacion_comparable == "Básica primaria" ~ 3,
      educacion_comparable == "Básica secundaria" ~ 4,
      educacion_comparable == "Media" ~ 5,
      educacion_comparable == "Superior o universitaria" ~ 6,
      TRUE ~ 99
    ),
    
    #-------------------------
    # Tamaño comparable
    #-------------------------
    tamano_label_chr = as.character(tamano_label),
    
    tamano_comparable = case_when(
      tamano_label_chr == "Solo" ~ "Solo",
      tamano_label_chr == "2-3" ~ "2-3",
      tamano_label_chr == "4-5" ~ "4-5",
      tamano_label_chr == "6-10" ~ "6-10",
      tamano_label_chr == "11-19" ~ "11-19",
      tamano_label_chr == "20-30" ~ "20-30",
      tamano_label_chr == "31-50" ~ "31-50",
      tamano_label_chr == "51-100" ~ "51-100",
      tamano_label_chr %in% c("101+", "101-200", "201+") ~ "101+",
      TRUE ~ NA_character_
    ),
    
    tamano_orden_comparable = case_when(
      tamano_comparable == "Solo" ~ 1,
      tamano_comparable == "2-3" ~ 2,
      tamano_comparable == "4-5" ~ 3,
      tamano_comparable == "6-10" ~ 4,
      tamano_comparable == "11-19" ~ 5,
      tamano_comparable == "20-30" ~ 6,
      tamano_comparable == "31-50" ~ 7,
      tamano_comparable == "51-100" ~ 8,
      tamano_comparable == "101+" ~ 9,
      TRUE ~ 99
    ),
    
    # numerador ponderado
    ingreso_real_x_trabajadores = ingreso_hora_real_2025 * trabajadores
  ) %>%
  filter(
    !is.na(educacion_comparable),
    !is.na(tamano_comparable),
    !is.na(ingreso_hora_real_2025),
    !is.na(trabajadores),
    trabajadores > 0
  ) %>%
  group_by(
    anio,
    educacion_comparable, educacion_orden_comparable,
    tamano_comparable, tamano_orden_comparable
  ) %>%
  summarise(
    trabajadores = sum(trabajadores, na.rm = TRUE),
    
    ingreso_real_x_trabajadores = sum(
      ingreso_real_x_trabajadores,
      na.rm = TRUE
    ),
    
    ingreso_hora_real_2025 = ingreso_real_x_trabajadores / trabajadores,
    
    observaciones = sum(observaciones, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(anio, educacion_comparable) %>%
  mutate(
    trabajadores_total_educ_anio = sum(trabajadores, na.rm = TRUE),
    participacion_empleo = trabajadores / trabajadores_total_educ_anio
  ) %>%
  ungroup() %>%
  mutate(
    educacion_comparable = factor(
      educacion_comparable,
      levels = c(
        "Ninguno",
        "Preescolar",
        "Básica primaria",
        "Básica secundaria",
        "Media",
        "Superior o universitaria"
      )
    ),
    
    tamano_comparable = factor(
      tamano_comparable,
      levels = c(
        "Solo", "2-3", "4-5", "6-10", "11-19",
        "20-30", "31-50", "51-100", "101+"
      )
    ),
    
    anio = factor(anio)
  ) %>%
  arrange(educacion_orden_comparable, tamano_orden_comparable, anio)

#========================================================
# 4. Revisar tabla resultante
#========================================================

educ_size_2008_2025 %>%
  select(
    anio, educacion_comparable, tamano_comparable,
    trabajadores, participacion_empleo, ingreso_hora_real_2025
  ) %>%
  arrange(educacion_comparable, tamano_comparable, anio)

#========================================================
# 5. Gráfico:
#    un panel por nivel educativo
#    eje X = tamaño de empresa
#    líneas = 2008 vs 2025
#========================================================
nivel_elegido <- "Superior o universitaria"
#========================================================
# Ninguno
# Preescolar
# Básica primaria
# Básica secundaria
# Media
# Superior o universitaria

df_nivel <- educ_size_2008_2025 %>%
  filter(educacion_comparable == nivel_elegido)
# Formalidad
serieFormalidad <- read_excel("Datos/Processed/ingreso_hora_anio_formalidad_tamano.xlsx", 
                             sheet = "02_base_final")

#========================================================
# 2. IPC diciembre para llevar a pesos constantes de 2025
#========================================================

ipc_dic <- tibble(
  anio = 2008:2025,
  ipc_dic = c(
    69.80, 71.20, 73.45, 76.19, 78.05, 79.56,
    82.47, 88.05, 93.11, 96.92, 100.00, 103.80,
    105.48, 111.41, 126.03, 137.72, 144.88, 152.27
  )
)

ipc_2025 <- ipc_dic %>%
  filter(anio == 2025) %>%
  pull(ipc_dic)


#========================================================
# 3. Deflactar ingreso laboral por hora
#========================================================

serieFormalidad <- serieFormalidad %>%
  left_join(ipc_dic, by = "anio") %>%
  mutate(
    factor_precios_2025 = ipc_2025 / ipc_dic,
    ingreso_hora_real_2025 = ingreso_hora_promedio * factor_precios_2025
  )


#========================================================
# 4. Construir base comparable 2008 vs 2025
#========================================================

formalidad_size_2008_2025 <- serieFormalidad %>%
  filter(anio %in% c(2008, 2025)) %>%
  mutate(
    formalidad_label = as.character(formalidad_label),
    tamano_label_chr = as.character(tamano_label),
    
    tamano_comparable = case_when(
      tamano_label_chr == "Solo" ~ "Solo",
      tamano_label_chr == "2-3" ~ "2-3",
      tamano_label_chr == "4-5" ~ "4-5",
      tamano_label_chr == "6-10" ~ "6-10",
      tamano_label_chr == "11-19" ~ "11-19",
      tamano_label_chr == "20-30" ~ "20-30",
      tamano_label_chr == "31-50" ~ "31-50",
      tamano_label_chr == "51-100" ~ "51-100",
      tamano_label_chr %in% c("101+", "101-200", "201+") ~ "101+",
      TRUE ~ NA_character_
    ),
    
    tamano_orden_comparable = case_when(
      tamano_comparable == "Solo" ~ 1,
      tamano_comparable == "2-3" ~ 2,
      tamano_comparable == "4-5" ~ 3,
      tamano_comparable == "6-10" ~ 4,
      tamano_comparable == "11-19" ~ 5,
      tamano_comparable == "20-30" ~ 6,
      tamano_comparable == "31-50" ~ 7,
      tamano_comparable == "51-100" ~ 8,
      tamano_comparable == "101+" ~ 9,
      TRUE ~ 99
    ),
    
    ingreso_real_x_trabajadores = ingreso_hora_real_2025 * trabajadores
  ) %>%
  filter(
    !is.na(formalidad_label),
    !is.na(tamano_comparable),
    !is.na(ingreso_hora_real_2025),
    !is.na(trabajadores),
    trabajadores > 0
  ) %>%
  group_by(
    anio,
    formalidad_cod,
    formalidad_label,
    formalidad_orden,
    tamano_comparable,
    tamano_orden_comparable
  ) %>%
  summarise(
    trabajadores = sum(trabajadores, na.rm = TRUE),
    
    ingreso_real_x_trabajadores = sum(
      ingreso_real_x_trabajadores,
      na.rm = TRUE
    ),
    
    ingreso_hora_real_2025 = ingreso_real_x_trabajadores / trabajadores,
    
    observaciones = sum(observaciones, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(anio, formalidad_label) %>%
  mutate(
    trabajadores_total_formalidad = sum(trabajadores, na.rm = TRUE),
    participacion_dentro_formalidad = trabajadores / trabajadores_total_formalidad
  ) %>%
  ungroup() %>%
  mutate(
    tamano_comparable = factor(
      tamano_comparable,
      levels = c(
        "Solo", "2-3", "4-5", "6-10", "11-19",
        "20-30", "31-50", "51-100", "101+"
      )
    ),
    anio = factor(anio)
  ) %>%
  arrange(formalidad_orden, tamano_orden_comparable, anio)


#========================================================
# 5. Revisar base comparable
#========================================================

formalidad_size_2008_2025 %>%
  select(
    anio,
    formalidad_label,
    tamano_comparable,
    trabajadores,
    participacion_dentro_formalidad,
    ingreso_hora_real_2025
  ) %>%
  arrange(formalidad_label, tamano_comparable, anio)

# Sexo
serieSexo <- read_excel("Datos/Processed/ingreso_hora_anio_sexo_tamano.xlsx", 
                              sheet = "02_base_final")

#========================================================
# 2. IPC diciembre para llevar a pesos constantes de 2025
#========================================================

ipc_dic <- tibble(
  anio = 2008:2025,
  ipc_dic = c(
    69.80, 71.20, 73.45, 76.19, 78.05, 79.56,
    82.47, 88.05, 93.11, 96.92, 100.00, 103.80,
    105.48, 111.41, 126.03, 137.72, 144.88, 152.27
  )
)

ipc_2025 <- ipc_dic %>%
  filter(anio == 2025) %>%
  pull(ipc_dic)


#========================================================
# 3. Deflactar ingreso laboral por hora
#========================================================

serieSexo <- serieSexo %>%
  left_join(ipc_dic, by = "anio") %>%
  mutate(
    factor_precios_2025 = ipc_2025 / ipc_dic,
    ingreso_hora_real_2025 = ingreso_hora_promedio * factor_precios_2025
  )


#========================================================
# 4. Construir base comparable 2008 vs 2025
#========================================================
# No se recalcula sexo.
# Solo se agrupa tamaño 101-200 y 201+ como 101+
# para que 2025 sea comparable con 2008.

sexo_size_2008_2025 <- serieSexo %>%
  filter(anio %in% c(2008, 2025)) %>%
  mutate(
    sexo_label = as.character(sexo_label),
    tamano_label_chr = as.character(tamano_label),
    
    tamano_comparable = case_when(
      tamano_label_chr == "Solo" ~ "Solo",
      tamano_label_chr == "2-3" ~ "2-3",
      tamano_label_chr == "4-5" ~ "4-5",
      tamano_label_chr == "6-10" ~ "6-10",
      tamano_label_chr == "11-19" ~ "11-19",
      tamano_label_chr == "20-30" ~ "20-30",
      tamano_label_chr == "31-50" ~ "31-50",
      tamano_label_chr == "51-100" ~ "51-100",
      tamano_label_chr %in% c("101+", "101-200", "201+") ~ "101+",
      TRUE ~ NA_character_
    ),
    
    tamano_orden_comparable = case_when(
      tamano_comparable == "Solo" ~ 1,
      tamano_comparable == "2-3" ~ 2,
      tamano_comparable == "4-5" ~ 3,
      tamano_comparable == "6-10" ~ 4,
      tamano_comparable == "11-19" ~ 5,
      tamano_comparable == "20-30" ~ 6,
      tamano_comparable == "31-50" ~ 7,
      tamano_comparable == "51-100" ~ 8,
      tamano_comparable == "101+" ~ 9,
      TRUE ~ 99
    ),
    
    ingreso_real_x_trabajadores = ingreso_hora_real_2025 * trabajadores
  ) %>%
  filter(
    !is.na(sexo_label),
    !is.na(tamano_comparable),
    !is.na(ingreso_hora_real_2025),
    !is.na(trabajadores),
    trabajadores > 0
  ) %>%
  group_by(
    anio,
    sexo_cod,
    sexo_label,
    sexo_orden,
    tamano_comparable,
    tamano_orden_comparable
  ) %>%
  summarise(
    trabajadores = sum(trabajadores, na.rm = TRUE),
    
    ingreso_real_x_trabajadores = sum(
      ingreso_real_x_trabajadores,
      na.rm = TRUE
    ),
    
    ingreso_hora_real_2025 = ingreso_real_x_trabajadores / trabajadores,
    
    observaciones = sum(observaciones, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(anio, sexo_label) %>%
  mutate(
    trabajadores_total_sexo = sum(trabajadores, na.rm = TRUE),
    participacion_dentro_sexo = trabajadores / trabajadores_total_sexo
  ) %>%
  ungroup() %>%
  mutate(
    tamano_comparable = factor(
      tamano_comparable,
      levels = c(
        "Solo", "2-3", "4-5", "6-10", "11-19",
        "20-30", "31-50", "51-100", "101+"
      )
    ),
    anio = factor(anio)
  ) %>%
  arrange(sexo_orden, tamano_orden_comparable, anio)

# Sector
serieSectorHom <- read_excel("Datos/Processed/ingreso_hora_anio_sector_hom_tamano.xlsx", 
                        sheet = "02_base_final")

#========================================================
# 2. Ver sectores disponibles
#========================================================

sectores_disponibles <- serieSectorHom %>%
  distinct(sector_hom_cod, sector_hom_label) %>%
  arrange(sector_hom_cod)

sectores_disponibles

#========================================================
# 3. IPC diciembre para llevar a pesos constantes de 2025
#========================================================

ipc_dic <- tibble(
  anio = 2008:2025,
  ipc_dic = c(
    69.80, 71.20, 73.45, 76.19, 78.05, 79.56,
    82.47, 88.05, 93.11, 96.92, 100.00, 103.80,
    105.48, 111.41, 126.03, 137.72, 144.88, 152.27
  )
)

ipc_2025 <- ipc_dic %>%
  filter(anio == 2025) %>%
  pull(ipc_dic)


#========================================================
# 4. Deflactar ingreso laboral por hora
#========================================================

serieSectorHom <- serieSectorHom %>%
  select(-any_of(c("ipc_dic", "factor_precios_2025", "ingreso_hora_real_2025"))) %>%
  left_join(ipc_dic, by = "anio") %>%
  mutate(
    factor_precios_2025 = ipc_2025 / ipc_dic,
    ingreso_hora_real_2025 = ingreso_hora_promedio * factor_precios_2025
  )


#========================================================
# 5. Seleccionar sector
#========================================================

sector_elegido <- "Industrias manufactureras"


#========================================================
# 6. Construir base comparable 2008 vs 2025
#========================================================

sector_size_2008_2025 <- serieSectorHom %>%
  filter(
    anio %in% c(2008, 2025),
    sector_hom_label == sector_elegido
  ) %>%
  mutate(
    tamano_label_chr = as.character(tamano_label),
    
    tamano_comparable = case_when(
      tamano_label_chr == "Solo" ~ "Solo",
      tamano_label_chr == "2-3" ~ "2-3",
      tamano_label_chr == "4-5" ~ "4-5",
      tamano_label_chr == "6-10" ~ "6-10",
      tamano_label_chr == "11-19" ~ "11-19",
      tamano_label_chr == "20-30" ~ "20-30",
      tamano_label_chr == "31-50" ~ "31-50",
      tamano_label_chr == "51-100" ~ "51-100",
      tamano_label_chr %in% c("101+", "101-200", "201+") ~ "101+",
      TRUE ~ NA_character_
    ),
    
    tamano_orden_comparable = case_when(
      tamano_comparable == "Solo" ~ 1,
      tamano_comparable == "2-3" ~ 2,
      tamano_comparable == "4-5" ~ 3,
      tamano_comparable == "6-10" ~ 4,
      tamano_comparable == "11-19" ~ 5,
      tamano_comparable == "20-30" ~ 6,
      tamano_comparable == "31-50" ~ 7,
      tamano_comparable == "51-100" ~ 8,
      tamano_comparable == "101+" ~ 9,
      TRUE ~ 99
    ),
    
    ingreso_real_x_trabajadores = ingreso_hora_real_2025 * trabajadores
  ) %>%
  filter(
    !is.na(tamano_comparable),
    !is.na(ingreso_hora_real_2025),
    !is.na(trabajadores),
    trabajadores > 0
  ) %>%
  group_by(
    anio,
    sector_hom_cod,
    sector_hom_label,
    tamano_comparable,
    tamano_orden_comparable
  ) %>%
  summarise(
    trabajadores = sum(trabajadores, na.rm = TRUE),
    
    ingreso_real_x_trabajadores = sum(
      ingreso_real_x_trabajadores,
      na.rm = TRUE
    ),
    
    ingreso_hora_real_2025 = ingreso_real_x_trabajadores / trabajadores,
    
    observaciones = sum(observaciones, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(anio, sector_hom_label) %>%
  mutate(
    trabajadores_total_sector = sum(trabajadores, na.rm = TRUE),
    participacion_dentro_sector = trabajadores / trabajadores_total_sector
  ) %>%
  ungroup() %>%
  arrange(anio, tamano_orden_comparable) %>%
  mutate(
    tamano_comparable = factor(
      tamano_comparable,
      levels = c(
        "Solo", "2-3", "4-5", "6-10", "11-19",
        "20-30", "31-50", "51-100", "101+"
      )
    ),
    anio = factor(anio)
  )

#========================================================
# Función general para gráficos 2008 vs 2025
# con brecha porcentual 2025 vs 2008
#========================================================

graficar_2008_2025_tamano <- function(
    data,
    size_var,
    titulo,
    subtitulo,
    size_label,
    facet_var = NULL,
    ncol_facet = NULL,
    free_y = TRUE
) {
  
  library(dplyr)
  library(ggplot2)
  library(scales)
  library(rlang)
  library(grid)
  
  size_var_sym <- sym(size_var)
  
  data_plot <- data %>%
    mutate(
      anio_num = as.numeric(as.character(anio)),
      anio = factor(anio_num, levels = c(2008, 2025)),
      tamano_comparable = factor(
        tamano_comparable,
        levels = c(
          "Solo", "2-3", "4-5", "6-10", "11-19",
          "20-30", "31-50", "51-100", "101+"
        )
      )
    )
  
  #====================================================
  # 1. Calcular brecha 2025 vs 2008
  #====================================================
  
  if (!is.null(facet_var)) {
    
    facet_var_sym <- sym(facet_var)
    
    data_plot <- data_plot %>%
      group_by(!!facet_var_sym, tamano_comparable) %>%
      mutate(
        ingreso_2008 = ingreso_hora_real_2025[anio_num == 2008][1],
        ingreso_2025 = ingreso_hora_real_2025[anio_num == 2025][1],
        diff_pct = 100 * ((ingreso_2025 / ingreso_2008) - 1)
      ) %>%
      ungroup()
    
    data_plot <- data_plot %>%
      group_by(!!facet_var_sym) %>%
      mutate(
        max_y_panel = max(ingreso_hora_real_2025, na.rm = TRUE),
        
        # 2025: etiqueta arriba
        etiqueta_2025_y = ingreso_hora_real_2025 + 0.055 * max_y_panel,
        
        # 2008: etiqueta abajo
        etiqueta_2008_y = ingreso_hora_real_2025 - 0.070 * max_y_panel,
        
        # Diff todavía más arriba
        diff_y = ingreso_hora_real_2025 + 0.160 * max_y_panel
      ) %>%
      ungroup()
    
  } else {
    
    data_plot <- data_plot %>%
      group_by(tamano_comparable) %>%
      mutate(
        ingreso_2008 = ingreso_hora_real_2025[anio_num == 2008][1],
        ingreso_2025 = ingreso_hora_real_2025[anio_num == 2025][1],
        diff_pct = 100 * ((ingreso_2025 / ingreso_2008) - 1)
      ) %>%
      ungroup() %>%
      mutate(
        max_y_panel = max(ingreso_hora_real_2025, na.rm = TRUE),
        etiqueta_2025_y = ingreso_hora_real_2025 + 0.055 * max_y_panel,
        etiqueta_2008_y = ingreso_hora_real_2025 - 0.070 * max_y_panel,
        diff_y = ingreso_hora_real_2025 + 0.160 * max_y_panel
      )
  }
  
  # Etiqueta del porcentaje dentro de la burbuja
  data_plot <- data_plot %>%
    mutate(
      pct_label = percent(!!size_var_sym, accuracy = 0.1)
    )
  
  # Subconjuntos para etiquetas separadas
  data_2008 <- data_plot %>%
    filter(anio_num == 2008)
  
  data_2025 <- data_plot %>%
    filter(anio_num == 2025)
  
  data_diff <- data_plot %>%
    filter(
      anio_num == 2025,
      !is.na(diff_pct),
      is.finite(diff_pct)
    ) %>%
    mutate(
      diff_label = case_when(
        diff_pct > 0 ~ paste0("Diff: +", number(diff_pct, accuracy = 0.1), "%"),
        diff_pct < 0 ~ paste0("Diff: ", number(diff_pct, accuracy = 0.1), "%"),
        TRUE ~ "Diff: 0.0%"
      )
    )
  
  #====================================================
  # 2. Gráfico
  #====================================================
  
  g <- ggplot(
    data_plot,
    aes(
      x = tamano_comparable,
      y = ingreso_hora_real_2025,
      group = anio
    )
  ) +
    geom_line(
      aes(color = anio),
      linewidth = 1.25,
      alpha = 0.95
    ) +
    
    # Burbujas
    geom_point(
      aes(size = !!size_var_sym, fill = anio),
      shape = 21,
      color = "black",
      stroke = 0.5,
      alpha = 0.90
    ) +
    
    # % dentro de la burbuja
    geom_text(
      aes(label = pct_label),
      color = "white",
      fontface = "bold",
      size = 2.8,
      show.legend = FALSE
    ) +
    
    # Etiqueta de ingreso para 2025 (arriba)
    geom_label(
      data = data_2025,
      aes(
        y = etiqueta_2025_y,
        label = comma(ingreso_hora_real_2025, accuracy = 1)
      ),
      fill = "darkblue",
      color = "white",
      fontface = "bold",
      size = 4.0,
      label.size = 0.15,
      label.padding = unit(0.18, "lines"),
      show.legend = FALSE
    ) +
    
    # Etiqueta de ingreso para 2008 (debajo)
    geom_label(
      data = data_2008,
      aes(
        y = etiqueta_2008_y,
        label = comma(ingreso_hora_real_2025, accuracy = 1)
      ),
      fill = "darkred",
      color = "white",
      fontface = "bold",
      size = 4.0,
      label.size = 0.15,
      label.padding = unit(0.18, "lines"),
      show.legend = FALSE
    ) +
    
    # Brecha porcentual arriba de la serie 2025
    geom_label(
      data = data_diff,
      aes(
        x = tamano_comparable,
        y = diff_y,
        label = diff_label
      ),
      inherit.aes = FALSE,
      fill = "black",
      color = "white",
      fontface = "bold",
      size = 4.0,
      label.size = 0.15,
      label.padding = unit(0.18, "lines"),
      show.legend = FALSE
    ) +
    
    scale_color_manual(
      values = c(
        "2008" = "darkred",
        "2025" = "darkblue"
      )
    ) +
    scale_fill_manual(
      values = c(
        "2008" = "darkred",
        "2025" = "darkblue"
      )
    ) +
    scale_y_continuous(
      labels = comma,
      expand = expansion(mult = c(0.18, 0.30))
    ) +
    scale_size_continuous(
      labels = percent_format(accuracy = 1),
      range = c(8, 20)
    ) +
    labs(
      title = titulo,
      subtitle = subtitulo,
      x = "Tamaño de empresa",
      y = "Ingreso laboral por hora real",
      color = "Año",
      fill = "Año",
      size = size_label
    ) +
    theme_classic(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(size = 12),
      axis.title = element_text(face = "bold", size = 13),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
      axis.text.y = element_text(size = 11),
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      legend.text = element_text(size = 11),
      strip.text = element_text(face = "bold", size = 12)
    )
  
  if (!is.null(facet_var)) {
    
    facet_formula <- as.formula(paste("~", facet_var))
    
    g <- g +
      facet_wrap(
        facet_formula,
        scales = ifelse(free_y, "free_y", "fixed"),
        ncol = ncol_facet
      )
  }
  
  return(g)
}

# General por tamaño de empresa
g_2008_2025_tamano <- graficar_2008_2025_tamano(
  data = serieSize_2008_2025,
  size_var = "participacion_empleo",
  titulo = "Ingreso laboral por hora real según tamaño de empresa: 2008 vs. 2025",
  subtitulo = "Pesos constantes de 2025. La categoría 101+ en 2025 se recalcula como promedio ponderado",
  size_label = "% del empleo total"
)

g_2008_2025_tamano

# Nivel educativo
nivel_elegido <- "Superior o universitaria"

# Opciones:
# "Ninguno"
# "Preescolar"
# "Básica primaria"
# "Básica secundaria"
# "Media"
# "Superior o universitaria"

df_nivel <- educ_size_2008_2025 %>%
  filter(educacion_comparable == nivel_elegido)

g_nivel_educativo <- graficar_2008_2025_tamano(
  data = df_nivel,
  size_var = "participacion_empleo",
  titulo = paste0("Ingreso laboral por hora real según tamaño de empresa: ", nivel_elegido),
  subtitulo = "Comparación 2008 vs. 2025. Pesos constantes de 2025",
  size_label = "% dentro del nivel educativo"
)

g_nivel_educativo

# Informalidad

formalidad_elegida <- "Informal"

df_formalidad <- formalidad_size_2008_2025 %>%
  filter(formalidad_label == formalidad_elegida)

g_formalidad_tamano <- graficar_2008_2025_tamano(
  data = df_formalidad,
  size_var = "participacion_dentro_formalidad",
  titulo = paste0("Ingreso laboral por hora real según tamaño de empresa: ", formalidad_elegida),
  subtitulo = "Comparación 2008 vs. 2025. Pesos constantes de 2025",
  size_label = "% dentro de formalidad"
)

g_formalidad_tamano

# Sexo
g_sexo_tamano_facet <- graficar_2008_2025_tamano(
  data = sexo_size_2008_2025,
  size_var = "participacion_dentro_sexo",
  titulo = "Ingreso laboral por hora real según tamaño de empresa",
  subtitulo = "Comparación 2008 vs. 2025 por sexo. Pesos constantes de 2025",
  size_label = "% dentro del sexo",
  facet_var = "sexo_label",
  ncol_facet = 2,
  free_y = TRUE
)

g_sexo_tamano_facet

# Sector
sector_elegido <- "Agricultura, ganadería, silvicultura y pesca"

# Puedes revisar opciones con:
sectores_disponibles

sector_size_2008_2025 <- serieSectorHom %>%
  filter(
    anio %in% c(2008, 2025),
    sector_hom_label == sector_elegido
  ) %>%
  mutate(
    tamano_label_chr = as.character(tamano_label),
    
    tamano_comparable = case_when(
      tamano_label_chr == "Solo" ~ "Solo",
      tamano_label_chr == "2-3" ~ "2-3",
      tamano_label_chr == "4-5" ~ "4-5",
      tamano_label_chr == "6-10" ~ "6-10",
      tamano_label_chr == "11-19" ~ "11-19",
      tamano_label_chr == "20-30" ~ "20-30",
      tamano_label_chr == "31-50" ~ "31-50",
      tamano_label_chr == "51-100" ~ "51-100",
      tamano_label_chr %in% c("101+", "101-200", "201+") ~ "101+",
      TRUE ~ NA_character_
    ),
    
    tamano_orden_comparable = case_when(
      tamano_comparable == "Solo" ~ 1,
      tamano_comparable == "2-3" ~ 2,
      tamano_comparable == "4-5" ~ 3,
      tamano_comparable == "6-10" ~ 4,
      tamano_comparable == "11-19" ~ 5,
      tamano_comparable == "20-30" ~ 6,
      tamano_comparable == "31-50" ~ 7,
      tamano_comparable == "51-100" ~ 8,
      tamano_comparable == "101+" ~ 9,
      TRUE ~ 99
    ),
    
    ingreso_real_x_trabajadores = ingreso_hora_real_2025 * trabajadores
  ) %>%
  filter(
    !is.na(tamano_comparable),
    !is.na(ingreso_hora_real_2025),
    !is.na(trabajadores),
    trabajadores > 0
  ) %>%
  group_by(
    anio,
    sector_hom_cod,
    sector_hom_label,
    tamano_comparable,
    tamano_orden_comparable
  ) %>%
  summarise(
    trabajadores = sum(trabajadores, na.rm = TRUE),
    ingreso_real_x_trabajadores = sum(ingreso_real_x_trabajadores, na.rm = TRUE),
    ingreso_hora_real_2025 = ingreso_real_x_trabajadores / trabajadores,
    observaciones = sum(observaciones, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(anio, sector_hom_label) %>%
  mutate(
    trabajadores_total_sector = sum(trabajadores, na.rm = TRUE),
    participacion_dentro_sector = trabajadores / trabajadores_total_sector
  ) %>%
  ungroup() %>%
  arrange(anio, tamano_orden_comparable) %>%
  mutate(
    tamano_comparable = factor(
      tamano_comparable,
      levels = c(
        "Solo", "2-3", "4-5", "6-10", "11-19",
        "20-30", "31-50", "51-100", "101+"
      )
    ),
    anio = factor(anio)
  )

g_sector_tamano <- graficar_2008_2025_tamano(
  data = sector_size_2008_2025,
  size_var = "participacion_dentro_sector",
  titulo = paste0("Ingreso laboral por hora real según tamaño de empresa: ", sector_elegido),
  subtitulo = "Comparación 2008 vs. 2025. Pesos constantes de 2025",
  size_label = "% dentro del sector"
)

g_sector_tamano
