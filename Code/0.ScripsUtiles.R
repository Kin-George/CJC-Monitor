setwd("~/Trabajo-Profesional/Javeriana")

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

# =========================
# 1. Cargar bases originales
# =========================

ciiu2d_raw <- read_excel(
  "Outputs/tables/INGLABO_ANUAL_todos_sectores_2024.xlsx", 
  sheet = "ciiu_2d"
)

ciiu3d_raw <- read_excel(
  "Outputs/tables/INGLABO_ANUAL_todos_sectores_2024.xlsx", 
  sheet = "ciiu_3d"
)

ciiu_dic <- read_excel(
  "DocumentacionAuxiliar/Estructura-detallada-CIIU-4AC-2022.xlsx"
)

# =========================
# 2. Diccionarios
# =========================

dic_ciiu_2d <- ciiu_dic %>%
  filter(str_detect(`División`, "^\\d{2}$")) %>%
  transmute(
    RAMA2D_R4 = as.numeric(`División`),
    Nombre_sector_2d = Descripción
  )

dic_ciiu_3d <- ciiu_dic %>%
  filter(str_detect(Grupo, "^\\d{3}$")) %>%
  transmute(
    RAMA3D_R4_chr = Grupo,
    Nombre_sector_3d = Descripción
  )

# =========================
# 3. Pegar nombres
# =========================

ciiu2d_export <- ciiu2d_raw %>%
  left_join(dic_ciiu_2d, by = "RAMA2D_R4") %>%
  relocate(Nombre_sector_2d, .after = RAMA2D_R4)

ciiu3d_export <- ciiu3d_raw %>%
  mutate(
    RAMA3D_R4_chr = str_pad(
      as.character(RAMA3D_R4),
      width = 3,
      side = "left",
      pad = "0"
    )
  ) %>%
  left_join(dic_ciiu_3d, by = "RAMA3D_R4_chr") %>%
  relocate(RAMA3D_R4_chr, Nombre_sector_3d, .after = RAMA3D_R4)

# =========================
# 4. Exportar Excel
# =========================

wb <- createWorkbook()

addWorksheet(wb, "ciiu2d")
addWorksheet(wb, "ciiu3d")

writeData(wb, "ciiu2d", ciiu2d_export)
writeData(wb, "ciiu3d", ciiu3d_export)

setColWidths(wb, "ciiu2d", cols = 1:ncol(ciiu2d_export), widths = "auto")
setColWidths(wb, "ciiu3d", cols = 1:ncol(ciiu3d_export), widths = "auto")

saveWorkbook(
  wb,
  "Outputs/tables/INGLABO_ANUAL_todos_sectores_2024_con_descripciones.xlsx",
  overwrite = TRUE
)


