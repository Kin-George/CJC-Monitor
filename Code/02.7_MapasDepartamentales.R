ROOT <- normalizePath(file.path(getwd()), winslash = "/", mustWork = TRUE)

if (basename(ROOT) != "CJC-Monitor" && dir.exists(file.path(ROOT, "CJC-Monitor"))) {
  ROOT <- normalizePath(file.path(ROOT, "CJC-Monitor"), winslash = "/", mustWork = TRUE)
}

.libPaths(c(file.path(ROOT, ".Rlib"), .libPaths()))

library(ggplot2)
library(dplyr)
library(readr)
library(maps)
library(grid)

paper_fig_dir <- file.path(ROOT, "Paper", "figures")
output_fig_dir <- file.path(ROOT, "Outputs", "Figures")
dir.create(paper_fig_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(output_fig_dir, recursive = TRUE, showWarnings = FALSE)

rem_path <- file.path(ROOT, "Outputs", "tables", "pib_geih_productividad_departamento_remuneracion.csv")
datos <- read_csv(rem_path, show_col_types = FALSE)

coords <- tibble::tribble(
  ~depto, ~lon, ~lat,
  5, -75.54, 6.29,
  8, -74.80, 10.96,
  11, -74.09, 4.63,
  13, -75.50, 10.40,
  15, -73.37, 5.55,
  17, -75.52, 5.06,
  18, -75.62, 1.61,
  19, -76.61, 2.42,
  20, -73.25, 10.48,
  23, -75.89, 8.76,
  25, -74.30, 4.90,
  27, -76.66, 5.69,
  41, -75.27, 2.94,
  44, -72.91, 11.54,
  47, -74.19, 11.26,
  50, -73.64, 4.15,
  52, -77.28, 1.21,
  54, -72.51, 7.88,
  63, -75.68, 4.53,
  66, -75.68, 4.81,
  68, -73.13, 7.13,
  70, -75.38, 9.29,
  73, -75.24, 4.45,
  76, -76.52, 3.44
)

mapa <- datos %>%
  mutate(
    pib_trabajador = pib_trabajador_2024,
    remuneracion = rem_por_trabajador_2024 / 1e6
  ) %>%
  inner_join(coords, by = "depto")

colombia <- map_data("world") %>%
  filter(region == "Colombia")

label_depts <- c("Bogotá D.C.", "Antioquia", "Meta", "Santander", "Caldas", "La Guajira")

base_map <- function() {
  ggplot() +
    geom_polygon(
      data = colombia,
      aes(x = long, y = lat, group = group),
      fill = "#f4f4f1",
      color = "#7f7f7f",
      linewidth = 0.25
    ) +
    coord_fixed(xlim = c(-79.5, -66.5), ylim = c(-4.5, 13.5), expand = FALSE) +
    theme_void(base_family = "serif") +
    theme(
      plot.title = element_text(face = "bold", size = 16, margin = margin(b = 3)),
      plot.subtitle = element_text(size = 10, color = "#555555", margin = margin(b = 10)),
      legend.position = "bottom",
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 8),
      plot.caption = element_text(size = 8, color = "#666666", hjust = 0)
    )
}

map_pib <- base_map() +
  geom_point(
    data = mapa,
    aes(x = lon, y = lat, color = pib_trabajador),
    size = 4.7,
    alpha = 0.92
  ) +
  geom_text(
    data = filter(mapa, departamento %in% label_depts),
    aes(x = lon, y = lat, label = departamento),
    nudge_y = 0.42,
    size = 3.1,
    family = "serif",
    color = "#222222"
  ) +
  scale_color_gradientn(
    colors = c("#d7e7f5", "#6baed6", "#2171b5", "#08306b"),
    name = "Millones de pesos de 2015"
  ) +
  labs(
    title = "PIB por trabajador",
    subtitle = "2024pr"
  )

map_rem <- base_map() +
  geom_point(
    data = mapa,
    aes(x = lon, y = lat, color = remuneracion),
    size = 4.7,
    alpha = 0.92
  ) +
  geom_text(
    data = filter(mapa, departamento %in% label_depts),
    aes(x = lon, y = lat, label = departamento),
    nudge_y = 0.42,
    size = 3.1,
    family = "serif",
    color = "#222222"
  ) +
  scale_color_gradientn(
    colors = c("#fee8c8", "#fdbb84", "#e34a33", "#7f0000"),
    name = "Millones de pesos de 2025 al mes"
  ) +
  labs(
    title = "Remuneración por trabajador",
    subtitle = "2024"
  )

out_file <- file.path(paper_fig_dir, "fig_pib_geih_productividad_departamento_mapa_niveles.png")
png(out_file, width = 2600, height = 1320, res = 180)
grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 2)))
print(map_pib, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(map_rem, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
dev.off()

file.copy(
  out_file,
  file.path(output_fig_dir, "fig_pib_geih_productividad_departamento_mapa_niveles.png"),
  overwrite = TRUE
)
