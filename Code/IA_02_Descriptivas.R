# ==============================================================================
# IA_02_Descriptivas.R
# Objetivo:
#   Generar estadisticas descriptivas basicas de exposicion a IA usando el Excel
#   producido por Code/IA_01_ExposureDatabase.do.
#
# Insumo:
#   Outputs/tables/IA_exposicion_ocupaciones_BaseIA.xlsx
#
# Salidas:
#   - Tablas impresas en consola.
#   - Graficos mostrados en R/RStudio.
#   - Copias PNG en Outputs/Figures/IA_Descriptivas.
# ==============================================================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(ggplot2)
  library(scales)
  library(forcats)
})

find_project_root <- function(start = getwd()) {
  current <- normalizePath(start, mustWork = TRUE)
  repeat {
    if (dir.exists(file.path(current, "Code")) && dir.exists(file.path(current, "Datos"))) return(current)
    parent <- dirname(current)
    if (identical(parent, current)) stop("No pude encontrar la raiz del proyecto desde: ", start)
    current <- parent
  }
}

clean_text <- function(x) {
  x %>%
    as.character() %>%
    str_squish() %>%
    na_if("")
}

short_label <- function(x, width = 58) {
  str_wrap(str_squish(as.character(x)), width = width)
}

theme_ia <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", size = base_size + 3),
      plot.subtitle = element_text(color = "#5E6A71"),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      legend.position = "bottom",
      legend.title = element_blank()
    )
}

project_root <- find_project_root()
input_xlsx <- file.path(project_root, "Outputs", "tables", "IA_exposicion_ocupaciones_BaseIA.xlsx")
fig_dir <- file.path(project_root, "Outputs", "Figures", "IA_Descriptivas")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(input_xlsx)) {
  stop(
    "No existe el archivo: ", input_xlsx, "\n",
    "Primero ejecuta en Stata: Code/IA_01_ExposureDatabase.do"
  )
}

# ------------------------------------------------------------------------------
# 1. Leer y limpiar tablas generadas por Stata
# ------------------------------------------------------------------------------
ocupaciones <- read_excel(input_xlsx, sheet = "01_ocupaciones") %>%
  {
    if (!"Participación en el empleo total del año" %in% names(.)) {
      .[["Participación en el empleo total del año"]] <- NA_real_
    }
    .
  } %>%
  rename(
    anio = `Año`,
    codigo_ocupacion = `Código ocupación`,
    ocupacion = `Ocupación`,
    numero_trabajadores = `Número de trabajadores`,
    participacion_empleo = `Participación en el empleo total del año`,
    exposicion_ia = `Exposición IA`,
    grupo_exposicion_ia = `Grupo de exposición IA`
  ) %>%
  mutate(
    anio = as.integer(anio),
    codigo_ocupacion = str_pad(as.character(codigo_ocupacion), width = 4, side = "left", pad = "0"),
    ocupacion = clean_text(ocupacion),
    grupo_exposicion_ia = clean_text(grupo_exposicion_ia),
    numero_trabajadores = as.numeric(numero_trabajadores),
    participacion_empleo = as.numeric(participacion_empleo),
    exposicion_ia = as.numeric(exposicion_ia)
  ) %>%
  filter(!is.na(anio), !is.na(ocupacion), !is.na(exposicion_ia)) %>%
  group_by(anio) %>%
  mutate(
    participacion_empleo = if_else(
      is.na(participacion_empleo),
      numero_trabajadores / sum(numero_trabajadores, na.rm = TRUE),
      participacion_empleo
    )
  ) %>%
  ungroup()

ocupacion_sector <- read_excel(input_xlsx, sheet = "02_ocupacion_sector") %>%
  {
    if (!"Participación de la ocupación dentro del sector" %in% names(.)) {
      .[["Participación de la ocupación dentro del sector"]] <- NA_real_
    }
    if (!"Participación de la ocupación-sector en el empleo total" %in% names(.)) {
      .[["Participación de la ocupación-sector en el empleo total"]] <- NA_real_
    }
    .
  } %>%
  rename(
    anio = `Año`,
    sector_rama = `Rama o sector económico`,
    codigo_ocupacion = `Código ocupación`,
    ocupacion = `Ocupación`,
    numero_trabajadores = `Número de trabajadores`,
    participacion_en_sector = `Participación de la ocupación dentro del sector`,
    participacion_empleo_total = `Participación de la ocupación-sector en el empleo total`,
    exposicion_ia = `Exposición IA`,
    grupo_exposicion_ia = `Grupo de exposición IA`
  ) %>%
  mutate(
    anio = as.integer(anio),
    sector_rama = clean_text(sector_rama),
    codigo_ocupacion = str_pad(as.character(codigo_ocupacion), width = 4, side = "left", pad = "0"),
    ocupacion = clean_text(ocupacion),
    grupo_exposicion_ia = clean_text(grupo_exposicion_ia),
    numero_trabajadores = as.numeric(numero_trabajadores),
    participacion_en_sector = as.numeric(participacion_en_sector),
    participacion_empleo_total = as.numeric(participacion_empleo_total),
    exposicion_ia = as.numeric(exposicion_ia)
  ) %>%
  filter(!is.na(anio), !is.na(sector_rama), !is.na(ocupacion), !is.na(exposicion_ia)) %>%
  group_by(anio, sector_rama) %>%
  mutate(
    participacion_en_sector = if_else(
      is.na(participacion_en_sector),
      numero_trabajadores / sum(numero_trabajadores, na.rm = TRUE),
      participacion_en_sector
    )
  ) %>%
  ungroup() %>%
  group_by(anio) %>%
  mutate(
    participacion_empleo_total = if_else(
      is.na(participacion_empleo_total),
      numero_trabajadores / sum(numero_trabajadores, na.rm = TRUE),
      participacion_empleo_total
    )
  ) %>%
  ungroup()

ultimo_anio <- max(ocupaciones$anio, na.rm = TRUE)

grupo_levels <- ocupaciones %>%
  distinct(grupo_exposicion_ia) %>%
  mutate(
    orden = case_when(
      str_detect(str_to_lower(grupo_exposicion_ia), "gradient 4|gradiente 4") ~ 5,
      str_detect(str_to_lower(grupo_exposicion_ia), "gradient 3|gradiente 3") ~ 4,
      str_detect(str_to_lower(grupo_exposicion_ia), "gradient 2|gradiente 2") ~ 3,
      str_detect(str_to_lower(grupo_exposicion_ia), "gradient 1|gradiente 1") ~ 2,
      str_detect(str_to_lower(grupo_exposicion_ia), "minimal|minimal exposure") ~ 1,
      TRUE ~ 0
    )
  ) %>%
  arrange(orden) %>%
  pull(grupo_exposicion_ia)

ocupaciones <- ocupaciones %>%
  mutate(grupo_exposicion_ia = factor(grupo_exposicion_ia, levels = grupo_levels))

ocupacion_sector <- ocupacion_sector %>%
  mutate(grupo_exposicion_ia = factor(grupo_exposicion_ia, levels = grupo_levels))

paleta_grupos <- c(
  "Minimal Exposure" = "#6B7280",
  "Gradient 1" = "#5DADE2",
  "Gradient 2" = "#1ABC9C",
  "Gradient 3" = "#F5B041",
  "Gradient 4" = "#C0392B",
  "Sin match" = "#BDBDBD"
)

# ------------------------------------------------------------------------------
# 2. Tablas descriptivas principales
# ------------------------------------------------------------------------------
resumen_anual <- ocupaciones %>%
  group_by(anio) %>%
  summarise(
    ocupaciones = n_distinct(codigo_ocupacion),
    trabajadores = sum(numero_trabajadores, na.rm = TRUE),
    exposicion_promedio_ponderada = weighted.mean(exposicion_ia, numero_trabajadores, na.rm = TRUE),
    .groups = "drop"
  )

tabla_grupos <- ocupaciones %>%
  group_by(anio, grupo_exposicion_ia) %>%
  summarise(
    trabajadores = sum(numero_trabajadores, na.rm = TRUE),
    exposicion_promedio = weighted.mean(exposicion_ia, numero_trabajadores, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(anio) %>%
  mutate(participacion = trabajadores / sum(trabajadores, na.rm = TRUE)) %>%
  ungroup()

top_expuestas <- ocupaciones %>%
  filter(anio == ultimo_anio) %>%
  arrange(desc(exposicion_ia), desc(numero_trabajadores)) %>%
  slice_head(n = 15)

alta_exposicion_con_peso <- ocupaciones %>%
  filter(
    anio == ultimo_anio,
    str_detect(str_to_lower(as.character(grupo_exposicion_ia)), "gradient 3|gradient 4|gradiente 3|gradiente 4")
  ) %>%
  arrange(desc(participacion_empleo), desc(exposicion_ia)) %>%
  slice_head(n = 20)

if (nrow(alta_exposicion_con_peso) == 0) {
  alta_exposicion_con_peso <- ocupaciones %>%
    filter(anio == ultimo_anio) %>%
    arrange(desc(exposicion_ia), desc(participacion_empleo)) %>%
    slice_head(n = 20)
}

ocupaciones_mapa <- ocupaciones %>%
  filter(anio == ultimo_anio) %>%
  mutate(
    label_mapa = if_else(
      participacion_empleo >= quantile(participacion_empleo, 0.92, na.rm = TRUE) |
        exposicion_ia >= quantile(exposicion_ia, 0.94, na.rm = TRUE),
      short_label(ocupacion, 26),
      NA_character_
    )
  )

menos_expuestas <- ocupaciones %>%
  filter(anio == ultimo_anio) %>%
  arrange(exposicion_ia, desc(numero_trabajadores)) %>%
  slice_head(n = 15)

cat("\n=== Resumen anual de exposicion IA ===\n")
print(resumen_anual)

cat("\n=== Participacion del mercado laboral por grupo de exposicion IA ===\n")
print(tabla_grupos)

cat("\n=== Ocupaciones mas expuestas en ", ultimo_anio, " ===\n", sep = "")
print(top_expuestas)

cat("\n=== Ocupaciones de alta exposicion con mayor peso en el empleo, ", ultimo_anio, " ===\n", sep = "")
print(alta_exposicion_con_peso)

cat("\n=== Ocupaciones menos expuestas en ", ultimo_anio, " ===\n", sep = "")
print(menos_expuestas)

# ------------------------------------------------------------------------------
# 3. Grafico: ocupaciones mas expuestas
# ------------------------------------------------------------------------------
g_top <- top_expuestas %>%
  mutate(
    ocupacion_plot = fct_reorder(short_label(ocupacion, 44), exposicion_ia)
  ) %>%
  ggplot(aes(x = ocupacion_plot, y = exposicion_ia, fill = grupo_exposicion_ia)) +
  geom_col(width = 0.72) +
  geom_text(
    aes(label = number(exposicion_ia, accuracy = 0.01, decimal.mark = ",")),
    hjust = -0.15,
    size = 3.5,
    fontface = "bold"
  ) +
  coord_flip() +
  scale_y_continuous(
    labels = number_format(accuracy = 0.1, decimal.mark = ","),
    limits = c(0, max(top_expuestas$exposicion_ia, na.rm = TRUE) * 1.12)
  ) +
  scale_fill_manual(values = paleta_grupos, drop = FALSE) +
  labs(
    title = paste0("Ocupaciones más expuestas a IA generativa, ", ultimo_anio),
    subtitle = "Ordenadas por índice de exposición OIT/ILO asignado a la ocupación CIUO-08/ISCO-08.",
    x = NULL,
    y = "Índice de exposición IA"
  ) +
  theme_ia()

print(g_top)

# ------------------------------------------------------------------------------
# 4. Grafico: ocupaciones de alta exposicion y su peso en el empleo
# ------------------------------------------------------------------------------
g_alta_peso <- alta_exposicion_con_peso %>%
  mutate(
    ocupacion_plot = fct_reorder(short_label(ocupacion, 46), participacion_empleo)
  ) %>%
  ggplot(aes(x = ocupacion_plot, y = participacion_empleo, fill = grupo_exposicion_ia)) +
  geom_col(width = 0.72) +
  geom_text(
    aes(label = percent(participacion_empleo, accuracy = 0.01, decimal.mark = ",")),
    hjust = -0.12,
    size = 3.3,
    fontface = "bold"
  ) +
  coord_flip() +
  scale_y_continuous(
    labels = percent_format(accuracy = 0.1, decimal.mark = ","),
    limits = c(0, max(alta_exposicion_con_peso$participacion_empleo, na.rm = TRUE) * 1.18)
  ) +
  scale_fill_manual(values = paleta_grupos, drop = FALSE) +
  labs(
    title = paste0("Alta exposición IA: ¿qué tanto pesan en el empleo?, ", ultimo_anio),
    subtitle = "Ocupaciones en gradientes altos, ordenadas por participación en el total de trabajadores.",
    x = NULL,
    y = "Participación en el empleo total"
  ) +
  theme_ia()

print(g_alta_peso)

# ------------------------------------------------------------------------------
# 5. Grafico: mapa exposicion IA vs participacion en el empleo
# ------------------------------------------------------------------------------
g_mapa <- ocupaciones_mapa %>%
  ggplot(aes(
    x = participacion_empleo,
    y = exposicion_ia,
    size = numero_trabajadores,
    color = grupo_exposicion_ia
  )) +
  geom_point(alpha = 0.72) +
  geom_text(
    aes(label = label_mapa),
    check_overlap = TRUE,
    size = 3,
    vjust = -0.7,
    color = "#263238"
  ) +
  scale_x_continuous(labels = percent_format(accuracy = 0.1, decimal.mark = ",")) +
  scale_y_continuous(labels = number_format(accuracy = 0.1, decimal.mark = ",")) +
  scale_size_continuous(range = c(2, 12), labels = comma_format(big.mark = ".", decimal.mark = ",")) +
  scale_color_manual(values = paleta_grupos, drop = FALSE) +
  labs(
    title = paste0("Exposición IA y tamaño laboral de las ocupaciones, ", ultimo_anio),
    subtitle = "Arriba: mayor exposición. A la derecha: mayor participación en el empleo total.",
    x = "Participación en el empleo total",
    y = "Índice de exposición IA",
    size = "Trabajadores"
  ) +
  theme_ia()

print(g_mapa)

# ------------------------------------------------------------------------------
# 6. Grafico: ocupaciones menos expuestas
# ------------------------------------------------------------------------------
g_bottom <- menos_expuestas %>%
  mutate(
    ocupacion_plot = fct_reorder(short_label(ocupacion, 44), -exposicion_ia)
  ) %>%
  ggplot(aes(x = ocupacion_plot, y = exposicion_ia, fill = grupo_exposicion_ia)) +
  geom_col(width = 0.72) +
  geom_text(
    aes(label = number(exposicion_ia, accuracy = 0.01, decimal.mark = ",")),
    hjust = -0.15,
    size = 3.5,
    fontface = "bold"
  ) +
  coord_flip() +
  scale_y_continuous(
    labels = number_format(accuracy = 0.1, decimal.mark = ","),
    limits = c(0, max(menos_expuestas$exposicion_ia, na.rm = TRUE) * 1.25)
  ) +
  scale_fill_manual(values = paleta_grupos, drop = FALSE) +
  labs(
    title = paste0("Ocupaciones menos expuestas a IA generativa, ", ultimo_anio),
    subtitle = "La exposición baja no implica ausencia de cambio tecnológico, sino menor exposición directa de tareas.",
    x = NULL,
    y = "Índice de exposición IA"
  ) +
  theme_ia()

print(g_bottom)
# ------------------------------------------------------------------------------
# 7. Grafico: que porcentaje del mercado laboral cae en cada grupo de exposicion
# ------------------------------------------------------------------------------
g_grupos <- tabla_grupos %>%
  ggplot(aes(x = factor(anio), y = participacion, fill = grupo_exposicion_ia)) +
  geom_col(width = 0.72, color = "white", linewidth = 0.25) +
  geom_text(
    data = ~ .x %>% filter(participacion >= 0.035),
    aes(label = percent(participacion, accuracy = 0.1, decimal.mark = ",")),
    position = position_stack(vjust = 0.5),
    color = "white",
    fontface = "bold",
    size = 3.2
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1, decimal.mark = ",")) +
  scale_fill_manual(values = paleta_grupos, drop = FALSE) +
  labs(
    title = "Distribución del mercado laboral por grupo de exposición a IA",
    subtitle = "Participación de ocupados ponderados dentro de cada año.",
    x = NULL,
    y = "Participación del empleo"
  ) +
  theme_ia()

print(g_grupos)

# ------------------------------------------------------------------------------
# 8. Grafico: exposicion promedio ponderada por sector en el ultimo año
# ------------------------------------------------------------------------------
sector_ultimo <- ocupacion_sector %>%
  filter(anio == ultimo_anio) %>%
  group_by(sector_rama) %>%
  summarise(
    trabajadores = sum(numero_trabajadores, na.rm = TRUE),
    exposicion_promedio = weighted.mean(exposicion_ia, numero_trabajadores, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(exposicion_promedio)) %>%
  slice_head(n = 20)

g_sector <- sector_ultimo %>%
  mutate(sector_plot = fct_reorder(short_label(sector_rama, 48), exposicion_promedio)) %>%
  ggplot(aes(x = sector_plot, y = exposicion_promedio)) +
  geom_col(fill = "#21618C", width = 0.72) +
  geom_text(
    aes(label = number(exposicion_promedio, accuracy = 0.01, decimal.mark = ",")),
    hjust = -0.15,
    size = 3.4,
    fontface = "bold"
  ) +
  coord_flip() +
  scale_y_continuous(
    labels = number_format(accuracy = 0.1, decimal.mark = ","),
    limits = c(0, max(sector_ultimo$exposicion_promedio, na.rm = TRUE) * 1.12)
  ) +
  labs(
    title = paste0("Sectores con mayor exposición promedio a IA, ", ultimo_anio),
    subtitle = "Promedio ponderado por trabajadores dentro de cada sector/rama.",
    x = NULL,
    y = "Índice promedio de exposición IA"
  ) +
  theme_ia()

print(g_sector)

# ------------------------------------------------------------------------------
# 9. Grafico: composicion de grupos de exposicion dentro de sectores principales
# ------------------------------------------------------------------------------
sectores_principales <- ocupacion_sector %>%
  filter(anio == ultimo_anio) %>%
  group_by(sector_rama) %>%
  summarise(trabajadores = sum(numero_trabajadores, na.rm = TRUE), .groups = "drop") %>%
  slice_max(trabajadores, n = 12, with_ties = FALSE) %>%
  pull(sector_rama)

sector_grupos <- ocupacion_sector %>%
  filter(anio == ultimo_anio, sector_rama %in% sectores_principales) %>%
  group_by(sector_rama, grupo_exposicion_ia) %>%
  summarise(trabajadores = sum(numero_trabajadores, na.rm = TRUE), .groups = "drop") %>%
  group_by(sector_rama) %>%
  mutate(participacion = trabajadores / sum(trabajadores, na.rm = TRUE)) %>%
  ungroup()

g_sector_grupos <- sector_grupos %>%
  mutate(sector_plot = fct_reorder(short_label(sector_rama, 44), trabajadores, .fun = sum)) %>%
  ggplot(aes(x = sector_plot, y = participacion, fill = grupo_exposicion_ia)) +
  geom_col(width = 0.72, color = "white", linewidth = 0.2) +
  coord_flip() +
  scale_y_continuous(labels = percent_format(accuracy = 1, decimal.mark = ",")) +
  scale_fill_manual(values = paleta_grupos, drop = FALSE) +
  labs(
    title = paste0("Composición de exposición IA en sectores principales, ", ultimo_anio),
    subtitle = "Participación de ocupados por grupo de exposición dentro de cada sector.",
    x = NULL,
    y = "Participación dentro del sector"
  ) +
  theme_ia()

print(g_sector_grupos)
message("Listo. Graficos impresos en RStudio y guardados en: ", fig_dir)
