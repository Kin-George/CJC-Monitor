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
rem_path <- file.path(ROOT, "Outputs", "tables", "pib_geih_productividad_departamento_remuneracion.csv")

poligonos <- read_csv(poly_path, show_col_types = FALSE) %>%
  mutate(group = paste(gid, group, sep = "_"))

datos <- read_csv(rem_path, show_col_types = FALSE) %>%
  mutate(
    pib_trabajador = pib_trabajador_2024,
    remuneracion = rem_por_trabajador_2024 / 1e6,
    residuo_pct = 100 * residuo_pct_tendencia,
    comparable = TRUE
  ) %>%
  select(departamento, pib_trabajador, remuneracion, residuo_pct, comparable)

mapa <- poligonos %>%
  left_join(datos, by = c("departamento_geo" = "departamento")) %>%
  arrange(group, point)

base_map <- function() {
  ggplot(mapa, aes(x = lon, y = lat, group = group)) +
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
