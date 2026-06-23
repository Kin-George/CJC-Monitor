ROOT <- normalizePath(file.path(getwd()), winslash = "/", mustWork = TRUE)

if (basename(ROOT) != "CJC-Monitor" && dir.exists(file.path(ROOT, "CJC-Monitor"))) {
  ROOT <- normalizePath(file.path(ROOT, "CJC-Monitor"), winslash = "/", mustWork = TRUE)
}

.libPaths(c(file.path(ROOT, ".Rlib"), .libPaths()))

library(ggplot2)
library(dplyr)
library(readr)
library(grid)

paper_fig_dir <- file.path(ROOT, "Paper", "figures")
output_fig_dir <- file.path(ROOT, "Outputs", "Figures")
dir.create(paper_fig_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(output_fig_dir, recursive = TRUE, showWarnings = FALSE)

poly_path <- file.path(ROOT, "DocumentacionAuxiliar", "Geometria", "gadm41_COL_1_polygons.csv")
summary_path <- file.path(ROOT, "Outputs", "tables", "pib_geih_productividad_departamento_summary.csv")
prod_bench_path <- file.path(ROOT, "Outputs", "tables", "pib_geih_productividad_departamento_benchmarks.csv")
rem_path <- file.path(ROOT, "Outputs", "tables", "pib_geih_productividad_departamento_remuneracion.csv")
rem_bench_path <- file.path(ROOT, "Outputs", "tables", "pib_geih_productividad_departamento_remuneracion_benchmarks.csv")

poligonos <- read_csv(poly_path, show_col_types = FALSE) %>%
  mutate(group = paste(gid, group, sep = "_"))

prod_benchmarks <- read_csv(prod_bench_path, show_col_types = FALSE)
crec_pib_hora_agregado <- prod_benchmarks$crec_pib_hora[[1]]
pib_hora_agregado <- prod_benchmarks$pib_hora_2024[[1]]

rem_benchmarks <- read_csv(rem_bench_path, show_col_types = FALSE)
crec_rem_agregado <- rem_benchmarks$crec_rem_trabajador[[1]]
rem_agregado <- rem_benchmarks$rem_por_trabajador_2024[[1]]

datos_productividad <- read_csv(summary_path, show_col_types = FALSE) %>%
  mutate(
    pib_hora = pib_hora_2024 / 1000,
    ocupados = ocupados_2024 / 1e6,
    cuadrante_productividad = case_when(
      pib_hora_2024 >= pib_hora_agregado & crec_pib_hora >= crec_pib_hora_agregado ~ "Líderes en auge",
      pib_hora_2024 >= pib_hora_agregado & crec_pib_hora < crec_pib_hora_agregado ~ "Líderes en declive",
      pib_hora_2024 < pib_hora_agregado & crec_pib_hora >= crec_pib_hora_agregado ~ "Aceleradores",
      TRUE ~ "Rezagados"
    ),
    cuadrante_productividad = factor(
      cuadrante_productividad,
      levels = c("Líderes en auge", "Líderes en declive", "Aceleradores", "Rezagados")
    ),
    comparable = TRUE
  ) %>%
  select(departamento, pib_hora, ocupados, cuadrante_productividad, comparable)

datos <- read_csv(rem_path, show_col_types = FALSE) %>%
  mutate(
    pib_trabajador = pib_trabajador_2024,
    remuneracion = rem_por_trabajador_2024 / 1e6,
    ocupados = ocupados_2024 / 1e6,
    residuo_pct = 100 * residuo_pct_tendencia,
    cuadrante_remuneracion = case_when(
      rem_por_trabajador_2024 >= rem_agregado & crec_rem_trabajador >= crec_rem_agregado ~ "Líderes en auge",
      rem_por_trabajador_2024 >= rem_agregado & crec_rem_trabajador < crec_rem_agregado ~ "Líderes en declive",
      rem_por_trabajador_2024 < rem_agregado & crec_rem_trabajador >= crec_rem_agregado ~ "Aceleradores",
      TRUE ~ "Rezagados"
    ),
    cuadrante_remuneracion = factor(
      cuadrante_remuneracion,
      levels = c("Líderes en auge", "Líderes en declive", "Aceleradores", "Rezagados")
    ),
    comparable = TRUE
  ) %>%
  select(departamento, pib_trabajador, remuneracion, ocupados, residuo_pct, cuadrante_remuneracion, comparable)

mapa <- poligonos %>%
  left_join(datos, by = c("departamento_geo" = "departamento")) %>%
  arrange(group, point)

mapa_productividad <- poligonos %>%
  left_join(datos_productividad, by = c("departamento_geo" = "departamento")) %>%
  arrange(group, point)

ajustes_centros <- data.frame(
  departamento_geo = c("Bogotá D.C.", "Cundinamarca", "Atlántico", "Quindío", "Risaralda"),
  lon_offset = c(0.35, -0.25, 0.10, -0.12, 0.12),
  lat_offset = c(-0.12, 0.12, 0.08, -0.08, 0.10)
)

centros <- poligonos %>%
  group_by(departamento_geo) %>%
  summarise(
    lon = (min(lon, na.rm = TRUE) + max(lon, na.rm = TRUE)) / 2,
    lat = (min(lat, na.rm = TRUE) + max(lat, na.rm = TRUE)) / 2,
    .groups = "drop"
  ) %>%
  left_join(ajustes_centros, by = "departamento_geo") %>%
  mutate(
    lon = lon + coalesce(lon_offset, 0),
    lat = lat + coalesce(lat_offset, 0)
  ) %>%
  select(departamento_geo, lon, lat)

base_map <- function(map_data = mapa) {
  ggplot(map_data, aes(x = lon, y = lat, group = group)) +
    geom_polygon(fill = "#f2f2ef", color = "#ffffff", linewidth = 0.18) +
    coord_fixed(xlim = c(-79.5, -66.5), ylim = c(-4.6, 13.5), expand = FALSE) +
    theme_void(base_family = "serif") +
    theme(
      plot.title = element_text(face = "bold", size = 15, margin = margin(b = 2)),
      plot.subtitle = element_text(size = 10, color = "#555555", margin = margin(b = 8)),
      legend.position = "bottom",
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 7),
      legend.key.width = unit(1.05, "cm"),
      plot.margin = margin(10, 8, 24, 8)
    )
}

quadrant_colors <- c(
  "Líderes en auge" = "#f28e2b",
  "Líderes en declive" = "#9aa7b0",
  "Aceleradores" = "#59a14f",
  "Rezagados" = "#4e79a7"
)

bubble_map_theme <- function() {
  theme_void(base_family = "serif") +
    theme(
      plot.title = element_text(face = "bold", size = 15, margin = margin(b = 2)),
      plot.subtitle = element_text(size = 10, color = "#555555", margin = margin(b = 8)),
      legend.position = "bottom",
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 7),
      legend.key.width = unit(0.85, "cm"),
      plot.margin = margin(10, 8, 24, 8)
    )
}

centros_productividad <- centros %>%
  left_join(datos_productividad, by = c("departamento_geo" = "departamento")) %>%
  filter(!is.na(cuadrante_productividad))

centros_remuneracion <- centros %>%
  left_join(datos, by = c("departamento_geo" = "departamento")) %>%
  filter(!is.na(cuadrante_remuneracion))

mapa_pib <- base_map() +
  geom_polygon(aes(fill = pib_trabajador), color = "#ffffff", linewidth = 0.18) +
  scale_fill_gradientn(
    colors = c("#eff6fb", "#bdd7e7", "#6baed6", "#2171b5", "#08306b"),
    na.value = "#eeeeea",
    name = "Millones de pesos de 2015"
  ) +
  labs(title = "PIB por trabajador", subtitle = "2024pr")

mapa_rem <- base_map() +
  geom_polygon(aes(fill = remuneracion), color = "#ffffff", linewidth = 0.18) +
  scale_fill_gradientn(
    colors = c("#fff5eb", "#fdd0a2", "#fdae6b", "#e6550d", "#7f2704"),
    na.value = "#eeeeea",
    name = "Millones de pesos de 2025 al mes"
  ) +
  labs(title = "Remuneración por trabajador", subtitle = "2024")

out_levels <- file.path(paper_fig_dir, "fig_pib_geih_productividad_departamento_mapa_niveles.png")
png(out_levels, width = 2600, height = 1320, res = 180)
grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 2)))
print(mapa_pib, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(mapa_rem, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
dev.off()

file.copy(
  out_levels,
  file.path(output_fig_dir, "fig_pib_geih_productividad_departamento_mapa_niveles.png"),
  overwrite = TRUE
)

mapa_cuadrantes_prod_area <- base_map(mapa_productividad) +
  geom_polygon(
    data = mapa_productividad %>% filter(!is.na(cuadrante_productividad)),
    aes(fill = cuadrante_productividad),
    color = "#ffffff",
    linewidth = 0.18
  ) +
  scale_fill_manual(values = quadrant_colors, drop = FALSE, name = "Categoría") +
  labs(
    title = "Cuadrantes de productividad por hora",
    subtitle = "Mapa por departamento"
  )

mapa_cuadrantes_prod_burbujas <- ggplot(poligonos, aes(x = lon, y = lat, group = group)) +
  geom_polygon(fill = "#fbfaf6", color = "#ddd8d0", linewidth = 0.18) +
  geom_point(
    data = centros_productividad,
    aes(x = lon, y = lat, size = ocupados, fill = cuadrante_productividad),
    inherit.aes = FALSE,
    shape = 21,
    color = "white",
    stroke = 0.55,
    alpha = 0.9
  ) +
  coord_fixed(xlim = c(-79.5, -66.5), ylim = c(-4.6, 13.5), expand = FALSE) +
  scale_fill_manual(values = quadrant_colors, drop = FALSE, name = "Categoría") +
  scale_size_area(
    max_size = 18,
    breaks = c(0.25, 1, 2, 4),
    labels = c("0,25", "1", "2", "4"),
    name = "Ocupados\n(millones)"
  ) +
  guides(
    fill = "none",
    size = guide_legend(override.aes = list(fill = "#9aa7b0"))
  ) +
  labs(
    title = "Cuadrantes y tamaño del empleo",
    subtitle = "Cada burbuja representa un departamento"
  ) +
  bubble_map_theme()

out_prod_quad <- file.path(paper_fig_dir, "fig_pib_geih_productividad_departamento_mapa_cuadrantes.png")
png(out_prod_quad, width = 2600, height = 1320, res = 180)
grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 2)))
print(mapa_cuadrantes_prod_area, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(mapa_cuadrantes_prod_burbujas, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
dev.off()

file.copy(
  out_prod_quad,
  file.path(output_fig_dir, "fig_pib_geih_productividad_departamento_mapa_cuadrantes.png"),
  overwrite = TRUE
)

resid_limit <- max(abs(datos$residuo_pct), na.rm = TRUE)

mapa_residuo <- base_map() +
  geom_polygon(aes(fill = residuo_pct), color = "#ffffff", linewidth = 0.18) +
  scale_fill_gradient2(
    low = "#b2182b",
    mid = "#f7f7f7",
    high = "#2166ac",
    midpoint = 0,
    limits = c(-resid_limit, resid_limit),
    na.value = "#eeeeea",
    name = "Diferencia frente\na la tendencia",
    labels = function(x) paste0(round(x), "%")
  ) +
  labs(
    title = "Remuneración relativa al PIB por trabajador",
    subtitle = "Azul: por encima de la recta; rojo: por debajo de la recta"
  )

out_resid <- file.path(paper_fig_dir, "fig_pib_geih_productividad_departamento_mapa_residuos.png")
png(out_resid, width = 1500, height = 1320, res = 180)
print(mapa_residuo)
dev.off()

file.copy(
  out_resid,
  file.path(output_fig_dir, "fig_pib_geih_productividad_departamento_mapa_residuos.png"),
  overwrite = TRUE
)

mapa_cuadrantes_rem_area <- base_map() +
  geom_polygon(
    data = mapa %>% filter(!is.na(cuadrante_remuneracion)),
    aes(fill = cuadrante_remuneracion),
    color = "#ffffff",
    linewidth = 0.18
  ) +
  scale_fill_manual(
    values = quadrant_colors,
    name = "Categoría"
  ) +
  labs(
    title = "Cuadrantes de remuneración por trabajador",
    subtitle = "Nivel en 2024pr y crecimiento anualizado 2010--2024pr"
  )

mapa_cuadrantes_rem_burbujas <- ggplot(poligonos, aes(x = lon, y = lat, group = group)) +
  geom_polygon(fill = "#fbfaf6", color = "#ddd8d0", linewidth = 0.18) +
  geom_point(
    data = centros_remuneracion,
    aes(x = lon, y = lat, size = ocupados, fill = cuadrante_remuneracion),
    inherit.aes = FALSE,
    shape = 21,
    color = "white",
    stroke = 0.55,
    alpha = 0.9
  ) +
  coord_fixed(xlim = c(-79.5, -66.5), ylim = c(-4.6, 13.5), expand = FALSE) +
  scale_fill_manual(values = quadrant_colors, name = "Categoría") +
  scale_size_area(
    max_size = 18,
    breaks = c(0.25, 1, 2, 4),
    labels = c("0,25", "1", "2", "4"),
    name = "Ocupados\n(millones)"
  ) +
  guides(
    fill = "none",
    size = guide_legend(override.aes = list(fill = "#9aa7b0"))
  ) +
  labs(
    title = "Cuadrantes y tamaño del empleo",
    subtitle = "Cada burbuja representa un departamento"
  ) +
  bubble_map_theme()

out_rem_quad <- file.path(paper_fig_dir, "fig_geih_remuneracion_departamento_mapa_cuadrantes.png")
png(out_rem_quad, width = 2600, height = 1320, res = 180)
grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 2)))
print(mapa_cuadrantes_rem_area, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(mapa_cuadrantes_rem_burbujas, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
dev.off()

file.copy(
  out_rem_quad,
  file.path(output_fig_dir, "fig_geih_remuneracion_departamento_mapa_cuadrantes.png"),
  overwrite = TRUE
)
