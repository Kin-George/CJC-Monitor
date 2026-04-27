setwd("~/Trabajo-Profesional/Javeriana")
# Productividad laboral Calculo sectorial y  regional
# Definicion: Productividad laboral: Medida que relaciona el valor agregado y el total de personal ocupado.
#Mide la eficiencia laboral e indica que en promedio, cada empleado produjo determinado monto de
#valor agregado. Organización de las Naciones Unidas (ONU). Departamento de Asuntos Económicos
#y Sociales, División de Estadística, Estudios de métodos, Serie F, No. 85. Manual de contabilidad
#nacional Cuentas nacionales: introducción práctica, 2006. (https://www.dane.gov.co/files/operaciones/EAM/met-EAM.pdf)
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
# Productividad laboral = Valor agregado real / total de personal ocupado 
# 1. Estadisticas descriptivas de la base de datos

eam_panel <- readRDS("Datos/Processed/EAM_panel_completo.rds")
# Quedarme con las variables clave
eam_panel <- select(eam_panel, 1,2,3,4,568, 580, 583, 584)

# Verificar el ciiu
eam_panel <- eam_panel %>%
  mutate(
    CIIU = as.character(CIIU),
    subperiodo = case_when(
      CIIU_REV == 2 ~ "1992-2000",
      CIIU_REV == 3 ~ "2001-2011",
      CIIU_REV == 4 ~ "2012-2024",
      TRUE ~ NA_character_
    )
  )



eam_base <- eam_panel %>%
  mutate(
    CIIU = as.character(CIIU),
    CIIU_2D = substr(CIIU, 1, 2)
  ) %>%
  filter(!ANIO %in% c(1997, 1998, 2020))
# CIIU REV 3
dicc_rev3 <- tribble(
  ~CIIU_REV, ~CIIU_2D, ~nombre_ciiu_2d,
  3, "15", "Elaboración de productos alimenticios y bebidas",
  3, "16", "Fabricación de productos de tabaco",
  3, "17", "Fabricación de productos textiles",
  3, "18", "Confección de prendas de vestir; adobo y teñido de pieles",
  3, "19", "Curtido y adobo de cueros; fabricación de calzado; fabricación de artículos de viaje, maletas, bolsos de mano y similares; artículos de talabartería y guarnicionería",
  3, "20", "Transformación de la madera y fabricación de productos de madera y de corcho, excepto muebles; fabricación de artículos de cestería y espartería",
  3, "21", "Fabricación de papel, cartón y productos de papel y cartón",
  3, "22", "Actividades de edición e impresión y de reproducción de grabaciones",
  3, "23", "Coquización, fabricación de productos de la refinación del petróleo y combustible nuclear",
  3, "24", "Fabricación de sustancias y productos químicos",
  3, "25", "Fabricación de productos de caucho y de plástico",
  3, "26", "Fabricación de otros productos minerales no metálicos",
  3, "27", "Fabricación de productos metalúrgicos básicos",
  3, "28", "Fabricación de productos elaborados de metal, excepto maquinaria y equipo",
  3, "29", "Fabricación de maquinaria y equipo ncp",
  3, "30", "Fabricación de maquinaria de oficina, contabilidad e informática",
  3, "31", "Fabricación de maquinaria y aparatos eléctricos ncp",
  3, "32", "Fabricación de equipo y aparatos de radio, televisión y comunicaciones",
  3, "33", "Fabricación de instrumentos médicos, ópticos y de precisión y fabricación de relojes",
  3, "34", "Fabricación de vehículos automotores, remolques y semirremolques",
  3, "35", "Fabricación de otros tipos de equipos de transporte",
  3, "36", "Fabricación de muebles; industrias manufactureras ncp",
  3, "37", "Reciclaje"
)
# CIUU REV4
dicc_rev4 <- tribble(
  ~CIIU_REV, ~CIIU_2D, ~nombre_ciiu_2d,
  4, "10", "Elaboración de productos alimenticios",
  4, "11", "Elaboración de bebidas",
  4, "12", "Elaboración de productos de tabaco",
  4, "13", "Fabricación de productos textiles",
  4, "14", "Confección de prendas de vestir",
  4, "15", "Curtido y recurtido de cueros; fabricación de calzado; fabricación de artículos de viaje, maletas, bolsos de mano y artículos similares, y fabricación de artículos de talabartería y guarnicionería; adobo y teñido de pieles",
  4, "16", "Transformación de la madera y fabricación de productos de madera y de corcho, excepto muebles; fabricación de artículos de cestería y espartería",
  4, "17", "Fabricación de papel, cartón y productos de papel y cartón",
  4, "18", "Actividades de impresión y de producción de copias a partir de grabaciones originales",
  4, "19", "Coquización, fabricación de productos de la refinación del petróleo y actividad de mezcla de combustibles",
  4, "20", "Fabricación de sustancias y productos químicos",
  4, "21", "Fabricación de productos farmacéuticos, sustancias químicas medicinales y productos botánicos de uso farmacéutico",
  4, "22", "Fabricación de productos de caucho y de plástico",
  4, "23", "Fabricación de otros productos minerales no metálicos",
  4, "24", "Fabricación de productos metalúrgicos básicos",
  4, "25", "Fabricación de productos elaborados de metal, excepto maquinaria y equipo",
  4, "26", "Fabricación de productos informáticos, electrónicos y ópticos",
  4, "27", "Fabricación de aparatos y equipo eléctrico",
  4, "28", "Fabricación de maquinaria y equipo n.c.p.",
  4, "29", "Fabricación de vehículos automotores, remolques y semirremolques",
  4, "30", "Fabricación de otros tipos de equipo de transporte",
  4, "31", "Fabricación de muebles, colchones y somieres",
  4, "32", "Otras industrias manufactureras",
  4, "33", "Instalación, mantenimiento y reparación especializada de maquinaria y equipo"
)

dicc_ciiu_2d <- bind_rows(dicc_rev3, dicc_rev4)
# Se hace necesario hacer una categorizacion homogenea
eam_hom <- eam_panel %>%
  mutate(
    CIIU = as.character(CIIU),
    CIIU_2D = str_sub(str_pad(CIIU, width = 4, side = "left", pad = "0"), 1, 2)
  )

crosswalk_hom <- tribble(
  ~CIIU_REV, ~CIIU_2D, ~SECTOR_HOM,
  
  # Alimentos y bebidas
  3, "15", "Alimentos y bebidas",
  4, "10", "Alimentos y bebidas",
  4, "11", "Alimentos y bebidas",
  
  # Tabaco
  3, "16", "Tabaco",
  4, "12", "Tabaco",
  
  # Textiles
  3, "17", "Textiles",
  4, "13", "Textiles",
  
  # Confecciones, cuero y calzado
  3, "18", "Confecciones, cuero y calzado",
  3, "19", "Confecciones, cuero y calzado",
  4, "14", "Confecciones, cuero y calzado",
  4, "15", "Confecciones, cuero y calzado",
  
  # Madera, papel e impresión
  3, "20", "Madera, papel e impresión",
  3, "21", "Madera, papel e impresión",
  3, "22", "Madera, papel e impresión",   # revisar luego la parte editorial
  4, "16", "Madera, papel e impresión",
  4, "17", "Madera, papel e impresión",
  4, "18", "Madera, papel e impresión",
  
  # Refinación, químicos y farmacéuticos
  3, "23", "Refinación, químicos y farmacéuticos",
  3, "24", "Refinación, químicos y farmacéuticos",
  4, "19", "Refinación, químicos y farmacéuticos",
  4, "20", "Refinación, químicos y farmacéuticos",
  4, "21", "Refinación, químicos y farmacéuticos",
  
  # Caucho y plástico
  3, "25", "Caucho y plástico",
  4, "22", "Caucho y plástico",
  
  # Minerales no metálicos
  3, "26", "Minerales no metálicos",
  4, "23", "Minerales no metálicos",
  
  # Metalurgia y productos metálicos
  3, "27", "Metalurgia y productos metálicos",
  3, "28", "Metalurgia y productos metálicos",
  4, "24", "Metalurgia y productos metálicos",
  4, "25", "Metalurgia y productos metálicos",
  
  # Maquinaria, equipo, eléctricos, electrónicos e instrumentos
  3, "29", "Maquinaria y equipo",
  3, "30", "Maquinaria y equipo",
  3, "31", "Maquinaria y equipo",
  3, "32", "Maquinaria y equipo",
  3, "33", "Maquinaria y equipo",
  4, "26", "Maquinaria y equipo",
  4, "27", "Maquinaria y equipo",
  4, "28", "Maquinaria y equipo",
  
  # Equipo de transporte
  3, "34", "Equipo de transporte",
  3, "35", "Equipo de transporte",
  4, "29", "Equipo de transporte",
  4, "30", "Equipo de transporte",
  
  # Muebles y otras manufactureras
  3, "36", "Muebles y otras manufactureras",
  4, "31", "Muebles y otras manufactureras",
  4, "32", "Muebles y otras manufactureras"
)

eam_hom <- eam_hom %>%
  left_join(crosswalk_hom, by = c("CIIU_REV", "CIIU_2D"))

codigos_sin_mapear <- eam_hom %>%
  filter(is.na(SECTOR_HOM)) %>%
  count(CIIU_REV, CIIU_2D, sort = TRUE)

codigos_sin_mapear
# periodo 1992-2000 basicamente
# Filtremos eso de moento
eam_hom<-filter(eam_hom, eam_hom$subperiodo!="1992-2000")
# Colapsar la base para tener sectores
prod_sector_hom <- eam_hom %>%
  filter(!is.na(SECTOR_HOM), !is.na(VALAGRI), !is.na(PERTOTAL), PERTOTAL > 0) %>%
  group_by(ANIO, CIIU_REV, SECTOR_HOM) %>%
  summarise(
    valagri_total = sum(VALAGRI, na.rm = TRUE),
    personal_total = sum(PERTOTAL, na.rm = TRUE),
    productividad_laboral = valagri_total / personal_total,
    .groups = "drop"
  )

# Estadisticas descriptivas de la base
#--------------------------------------------
# 1) Base de trabajo
#--------------------------------------------

# Usa VALAGRI_real si ya existe; si no, usa VALAGRI
valor_var <- if ("VALAGRI_real" %in% names(eam_hom)) "VALAGRI_real" else "VALAGRI"

base_stats <- eam_hom %>%
  mutate(
    SECTOR_HOM = as.character(SECTOR_HOM)
  ) %>%
  filter(
    !ANIO %in% c(1997, 1998, 2020),
    !is.na(SECTOR_HOM),
    !is.na(NORDEST),
    !is.na(PERTOTAL),
    PERTOTAL > 0,
    !is.na(.data[[valor_var]])
  )

# Chequeo de posibles duplicados establecimiento-año
duplicados_est_anio <- base_stats %>%
  count(NORDEST, ANIO) %>%
  filter(n > 1)

cat("Número de pares establecimiento-año duplicados:", nrow(duplicados_est_anio), "\n")

# Ordenar sectores para que los gráficos queden más legibles
orden_sectores <- base_stats %>%
  group_by(SECTOR_HOM) %>%
  summarise(personal_promedio = mean(PERTOTAL, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(personal_promedio)) %>%
  pull(SECTOR_HOM)

base_stats <- base_stats %>%
  mutate(SECTOR_HOM = factor(SECTOR_HOM, levels = orden_sectores))

#--------------------------------------------
# 2) Tablas de estadísticas
#--------------------------------------------

# 2.1 Cantidad anual de establecimientos únicos (total)
tabla_total_anual <- base_stats %>%
  group_by(ANIO) %>%
  summarise(
    establecimientos_unicos = n_distinct(NORDEST),
    valor_agregado_total = sum(.data[[valor_var]], na.rm = TRUE),
    personal_total = sum(PERTOTAL, na.rm = TRUE),
    productividad_laboral = valor_agregado_total / personal_total,
    .groups = "drop"
  )

# 2.2 Estadísticas anuales por sector
tabla_sector_anual <- base_stats %>%
  group_by(ANIO, SECTOR_HOM) %>%
  summarise(
    establecimientos_unicos = n_distinct(NORDEST),
    valor_agregado_total = sum(.data[[valor_var]], na.rm = TRUE),
    personal_total = sum(PERTOTAL, na.rm = TRUE),
    productividad_laboral = valor_agregado_total / personal_total,
    .groups = "drop"
  ) %>%
  left_join(
    tabla_total_anual %>%
      select(ANIO, total_establecimientos = establecimientos_unicos),
    by = "ANIO"
  ) %>%
  mutate(
    porcentaje_establecimientos = establecimientos_unicos / total_establecimientos
  )

# Guardar tablas
write.xlsx(tabla_total_anual, "Outputs/tables/tabla_total_anual.xlsx")
write.xlsx(tabla_sector_anual, "Outputs/tables/tabla_sector_anual.xlsx")

#--------------------------------------------
# 4) Tema simple para gráficos
#--------------------------------------------
tema_simple <- theme_classic(base_size = 12) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 9),
    legend.key.width = grid::unit(1.4, "cm"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold", size = 13),
    axis.title = element_text(face = "bold")
  )

colores_sectores <- c(
  "Alimentos y bebidas" = "#F4AC45",                   # naranja rojizo
  "Confecciones, cuero y calzado" = "#2D3047",         # rosado púrpura
  "Muebles y otras manufactureras" = "#4357AD",       # violeta
  "Madera, papel e impresión" = "#14080E",            # azul
  "Refinación, químicos y farmacéuticos" = "#2EC4B6", # celeste
  "Metalurgia y productos metálicos" = "#613DC1",     # verde petróleo
  "Caucho y plástico" = "#6B0504",                    # turquesa
  "Maquinaria y equipo" = "#440381",                  # verde fuerte
  "Minerales no metálicos" = "#2A4D14",               # verde oliva
  "Textiles" = "#E3B505",                             # verde claro
  "Equipo de transporte" = "darkblue",                 # mostaza
  "Tabaco" = "darkgreen"                                # amarillo
)

# Para que la leyenda salga ordenada de forma más estable
tabla_sector_anual <- tabla_sector_anual %>%
  group_by(SECTOR_HOM) %>%
  mutate(promedio_est = mean(establecimientos_unicos, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(SECTOR_HOM = forcats::fct_reorder(SECTOR_HOM, promedio_est, .desc = TRUE))

#--------------------------------------------
# 5) Gráficos SIN subplots
#--------------------------------------------

# 5.1 Cantidad anual total de establecimientos únicos
ggplot(tabla_total_anual, aes(x = ANIO, y = establecimientos_unicos)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.8) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Cantidad anual de establecimientos únicos",
    x = NULL,
    y = "Establecimientos"
  ) +
  tema_simple

# 5.2 Evolución anual de establecimientos únicos por sector
ggplot(tabla_sector_anual, aes(x = ANIO, y = establecimientos_unicos, color = SECTOR_HOM)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.8) +
  scale_color_manual(values = colores_sectores) +
  scale_y_continuous(labels = scales::comma) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE)) +
  labs(
    title = "Evolución anual de establecimientos únicos por sector",
    x = NULL,
    y = "Establecimientos"
  ) +
  tema_simple

# 5.3 Porcentaje anual de establecimientos por sector
ggplot(tabla_sector_anual, aes(x = factor(ANIO), y = porcentaje_establecimientos, fill = SECTOR_HOM)) +
  geom_col(width = 0.85) +
  scale_fill_manual(values = colores_sectores) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  labs(
    title = "Participación anual de establecimientos por sector",
    x = "Año",
    y = "% del total anual"
  ) +
  tema_simple +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 5.4 Valor agregado total por sector y año
ggplot(tabla_sector_anual, aes(x = ANIO, y = valor_agregado_total, color = SECTOR_HOM)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.8) +
  scale_color_manual(values = colores_sectores) +
  scale_y_continuous(labels = scales::comma) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE)) +
  labs(
    title = paste("Evolución del valor agregado", ifelse(valor_var == "VALAGRI_real", "real", "nominal"), "por sector"),
    x = NULL,
    y = "Valor agregado"
  ) +
  tema_simple

# 5.5 Personal total por sector y año
ggplot(tabla_sector_anual, aes(x = ANIO, y = personal_total, color = SECTOR_HOM)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.8) +
  scale_color_manual(values = colores_sectores) +
  scale_y_continuous(labels = scales::comma) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE)) +
  labs(
    title = "Evolución del personal total por sector",
    x = NULL,
    y = "Personal total"
  ) +
  tema_simple

# 5.6 Productividad laboral total anual
ggplot(tabla_sector_anual, aes(x = ANIO, y = productividad_laboral, color = SECTOR_HOM)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.8) +
  scale_color_manual(values = colores_sectores) +
  scale_y_continuous(labels = scales::comma) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE)) +
  labs(
    title = "Productividad laboral anual por sector",
    x = NULL,
    y = "Productividad laboral"
  ) +
  tema_simple

# Productividad a nivel de regiones
#--------------------------------------------
# 1) Crear variable region en eam_hom
#--------------------------------------------
eam_region <- eam_hom %>%
  mutate(
    region = case_when(
      # Caribe
      DPTO %in% c(8, 13, 20, 23, 44, 47, 70) ~ "Caribe",
      
      # Insular
      DPTO %in% c(88) ~ "Insular",
      
      # Andina
      DPTO %in% c(5, 11, 15, 17, 25, 41, 54, 63, 66, 68, 73) ~ "Andina",
      
      # Pacífica
      DPTO %in% c(19, 27, 52, 76) ~ "Pacífica",
      
      # Orinoquía
      DPTO %in% c(81, 85, 50, 99) ~ "Orinoquía",
      
      # Amazonía
      DPTO %in% c(91, 18, 94, 95, 86, 97) ~ "Amazonía",
      
      TRUE ~ NA_character_
    )
  )

valor_var <- if ("VALAGRI_real" %in% names(eam_region)) "VALAGRI_real" else "VALAGRI"

base_region_anio <- eam_region %>%
  filter(
    !ANIO %in% c(1997, 1998, 2020),
    !is.na(region),
    !is.na(NORDEST),
    !is.na(PERTOTAL),
    PERTOTAL > 0,
    !is.na(.data[[valor_var]])
  ) %>%
  group_by(ANIO, region) %>%
  summarise(
    establecimientos_unicos = n_distinct(NORDEST),
    valor_agregado_total = sum(.data[[valor_var]], na.rm = TRUE),
    personal_total = sum(PERTOTAL, na.rm = TRUE),
    productividad_laboral = valor_agregado_total / personal_total,
    .groups = "drop"
  )

base_region_anio_sector <- eam_region %>%
  filter(
    !ANIO %in% c(1997, 1998, 2020),
    !is.na(region),
    !is.na(SECTOR_HOM),
    !is.na(NORDEST),
    !is.na(PERTOTAL),
    PERTOTAL > 0,
    !is.na(.data[[valor_var]])
  ) %>%
  group_by(ANIO, region, SECTOR_HOM) %>%
  summarise(
    establecimientos_unicos = n_distinct(NORDEST),
    valor_agregado_total = sum(.data[[valor_var]], na.rm = TRUE),
    personal_total = sum(PERTOTAL, na.rm = TRUE),
    productividad_laboral = valor_agregado_total / personal_total,
    .groups = "drop"
  )

#--------------------------------------------
# 1) Definir variable de valor agregado
#--------------------------------------------
valor_var <- if ("VALAGRI_real" %in% names(eam_region)) "VALAGRI_real" else "VALAGRI"

#--------------------------------------------
# 2) Base para estadísticas regionales
#--------------------------------------------
base_region_stats <- eam_region %>%
  filter(
    !ANIO %in% c(1997, 1998, 2020),
    !is.na(region),
    !is.na(NORDEST),
    !is.na(PERTOTAL),
    PERTOTAL > 0,
    !is.na(.data[[valor_var]])
  )

#--------------------------------------------
# 3) Tabla total anual
#--------------------------------------------
tabla_region_total_anual <- base_region_stats %>%
  group_by(ANIO) %>%
  summarise(
    establecimientos_unicos = n_distinct(NORDEST),
    valor_agregado_total = sum(.data[[valor_var]], na.rm = TRUE),
    personal_total = sum(PERTOTAL, na.rm = TRUE),
    productividad_laboral = valor_agregado_total / personal_total,
    .groups = "drop"
  )

#--------------------------------------------
# 4) Tabla anual por región
#--------------------------------------------
tabla_region_anual <- base_region_stats %>%
  group_by(ANIO, region) %>%
  summarise(
    establecimientos_unicos = n_distinct(NORDEST),
    valor_agregado_total = sum(.data[[valor_var]], na.rm = TRUE),
    personal_total = sum(PERTOTAL, na.rm = TRUE),
    productividad_laboral = valor_agregado_total / personal_total,
    .groups = "drop"
  ) %>%
  left_join(
    tabla_region_total_anual %>%
      select(ANIO, total_establecimientos = establecimientos_unicos),
    by = "ANIO"
  ) %>%
  mutate(
    porcentaje_establecimientos = establecimientos_unicos / total_establecimientos
  )

#--------------------------------------------
# 1) Definir variable de valor agregado
#--------------------------------------------
valor_var <- if ("VALAGRI_real" %in% names(eam_region)) "VALAGRI_real" else "VALAGRI"

#--------------------------------------------
# 2) Base para estadísticas regionales
#--------------------------------------------
base_region_stats <- eam_region %>%
  filter(
    !ANIO %in% c(1997, 1998, 2020),
    !is.na(region),
    !is.na(NORDEST),
    !is.na(PERTOTAL),
    PERTOTAL > 0,
    !is.na(.data[[valor_var]])
  )

#--------------------------------------------
# 3) Tabla total anual
#--------------------------------------------
tabla_region_total_anual <- base_region_stats %>%
  group_by(ANIO) %>%
  summarise(
    establecimientos_unicos = n_distinct(NORDEST),
    valor_agregado_total = sum(.data[[valor_var]], na.rm = TRUE),
    personal_total = sum(PERTOTAL, na.rm = TRUE),
    productividad_laboral = valor_agregado_total / personal_total,
    .groups = "drop"
  )

#--------------------------------------------
# 4) Tabla anual por región
#--------------------------------------------
tabla_region_anual <- base_region_stats %>%
  group_by(ANIO, region) %>%
  summarise(
    establecimientos_unicos = n_distinct(NORDEST),
    valor_agregado_total = sum(.data[[valor_var]], na.rm = TRUE),
    personal_total = sum(PERTOTAL, na.rm = TRUE),
    productividad_laboral = valor_agregado_total / personal_total,
    .groups = "drop"
  ) %>%
  left_join(
    tabla_region_total_anual %>%
      select(ANIO, total_establecimientos = establecimientos_unicos),
    by = "ANIO"
  ) %>%
  mutate(
    porcentaje_establecimientos = establecimientos_unicos / total_establecimientos
  )

# Ordenar regiones por tamaño promedio
tabla_region_anual <- tabla_region_anual %>%
  group_by(region) %>%
  mutate(promedio_est = mean(establecimientos_unicos, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(region = fct_reorder(region, promedio_est, .desc = TRUE))

# Colores manuales fáciles de distinguir
colores_regiones <- c(
  "Andina" = "#1F77B4",
  "Caribe" = "#FF7F0E",
  "Pacífica" = "#2CA02C",
  "Orinoquía" = "#D62728",
  "Amazonía" = "#9467BD",
  "Insular" = "#8C564B"
)

tema_simple <- theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold", size = 13),
    axis.title = element_text(face = "bold")
  )

ggplot(tabla_region_anual, aes(x = ANIO, y = establecimientos_unicos, color = region)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = colores_regiones) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Evolución anual de establecimientos únicos por región",
    x = NULL,
    y = "Establecimientos"
  ) +
  tema_simple

ggplot(tabla_region_anual, aes(x = factor(ANIO), y = porcentaje_establecimientos, fill = region)) +
  geom_col(width = 0.85) +
  scale_fill_manual(values = colores_regiones) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Participación anual de establecimientos por región",
    x = "Año",
    y = "% del total anual"
  ) +
  tema_simple +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(tabla_region_anual, aes(x = ANIO, y = valor_agregado_total, color = region)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = colores_regiones) +
  scale_y_continuous(labels = comma) +
  labs(
    title = paste("Evolución del valor agregado", ifelse(valor_var == "VALAGRI_real", "real", "nominal"), "por región"),
    x = NULL,
    y = "Valor agregado"
  ) +
  tema_simple

ggplot(tabla_region_anual, aes(x = ANIO, y = personal_total, color = region)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = colores_regiones) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Evolución del personal total por región",
    x = NULL,
    y = "Personal total"
  ) +
  tema_simple

ggplot(tabla_region_anual, aes(x = ANIO, y = productividad_laboral, color = region)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = colores_regiones) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Productividad laboral anual por región",
    x = NULL,
    y = "Productividad laboral"
  ) +
  tema_simple

# Por region y sector
#--------------------------------------------
# 1) Definir variable de valor agregado
#--------------------------------------------
valor_var <- if ("VALAGRI_real" %in% names(eam_region)) "VALAGRI_real" else "VALAGRI"

#--------------------------------------------
# 2) Base de trabajo
#--------------------------------------------
base_region_sector_stats <- eam_region %>%
  filter(
    !ANIO %in% c(1997, 1998, 2020),
    !is.na(region),
    !is.na(SECTOR_HOM),
    !is.na(NORDEST),
    !is.na(PERTOTAL),
    PERTOTAL > 0,
    !is.na(.data[[valor_var]])
  )

#--------------------------------------------
# 3) Tabla total anual
#--------------------------------------------
tabla_rs_total_anual <- base_region_sector_stats %>%
  group_by(ANIO) %>%
  summarise(
    establecimientos_unicos = n_distinct(NORDEST),
    valor_agregado_total = sum(.data[[valor_var]], na.rm = TRUE),
    personal_total = sum(PERTOTAL, na.rm = TRUE),
    productividad_laboral = valor_agregado_total / personal_total,
    .groups = "drop"
  )

#--------------------------------------------
# 4) Tabla anual por región y sector
#--------------------------------------------
tabla_region_sector_anual <- base_region_sector_stats %>%
  group_by(ANIO, region, SECTOR_HOM) %>%
  summarise(
    establecimientos_unicos = n_distinct(NORDEST),
    valor_agregado_total = sum(.data[[valor_var]], na.rm = TRUE),
    personal_total = sum(PERTOTAL, na.rm = TRUE),
    productividad_laboral = valor_agregado_total / personal_total,
    .groups = "drop"
  ) %>%
  left_join(
    tabla_rs_total_anual %>%
      select(ANIO, total_establecimientos = establecimientos_unicos),
    by = "ANIO"
  ) %>%
  mutate(
    porcentaje_establecimientos = establecimientos_unicos / total_establecimientos
  )

# Orden de regiones
tabla_region_sector_anual <- tabla_region_sector_anual %>%
  group_by(region) %>%
  mutate(promedio_region = mean(establecimientos_unicos, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(region = fct_reorder(region, promedio_region, .desc = TRUE))

# Colores para regiones
colores_regiones <- c(
  "Andina" = "#1F77B4",
  "Caribe" = "#FF7F0E",
  "Pacífica" = "#2CA02C",
  "Orinoquía" = "#D62728",
  "Amazonía" = "#9467BD",
  "Insular" = "#8C564B"
)

# Colores para sectores
colores_sectores <- c(
  "Alimentos y bebidas" = "#D55E00",
  "Confecciones, cuero y calzado" = "#CC79A7",
  "Muebles y otras manufactureras" = "#7B61FF",
  "Madera, papel e impresión" = "#0072B2",
  "Refinación, químicos y farmacéuticos" = "#56B4E9",
  "Metalurgia y productos metálicos" = "#009E73",
  "Caucho y plástico" = "#00A6A6",
  "Maquinaria y equipo" = "#1B9E77",
  "Minerales no metálicos" = "#66A61E",
  "Textiles" = "#A6D854",
  "Equipo de transporte" = "#E6AB02",
  "Tabaco" = "#F0E442"
)

tema_simple <- theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 9),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold", size = 13),
    axis.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold")
  )

ggplot(tabla_region_sector_anual, aes(x = ANIO, y = establecimientos_unicos, color = SECTOR_HOM)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.6) +
  facet_wrap(~ region, scales = "free_y") +
  scale_color_manual(values = colores_sectores) +
  scale_y_continuous(labels = comma) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE)) +
  labs(
    title = "Evolución anual de establecimientos únicos por región y sector",
    x = NULL,
    y = "Establecimientos"
  ) +
  tema_simple

ggplot(tabla_region_sector_anual, aes(x = factor(ANIO), y = porcentaje_establecimientos, fill = SECTOR_HOM)) +
  geom_col(width = 0.85) +
  facet_wrap(~ region) +
  scale_fill_manual(values = colores_sectores) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  labs(
    title = "Participación anual de establecimientos por región y sector",
    x = "Año",
    y = "% del total anual"
  ) +
  tema_simple +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(tabla_region_sector_anual, aes(x = ANIO, y = valor_agregado_total, color = SECTOR_HOM)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.6) +
  facet_wrap(~ region, scales = "free_y") +
  scale_color_manual(values = colores_sectores) +
  scale_y_continuous(labels = comma) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE)) +
  labs(
    title = paste("Evolución del valor agregado", ifelse(valor_var == "VALAGRI_real", "real", "nominal"), "por región y sector"),
    x = NULL,
    y = "Valor agregado"
  ) +
  tema_simple

ggplot(tabla_region_sector_anual, aes(x = ANIO, y = personal_total, color = SECTOR_HOM)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.6) +
  facet_wrap(~ region, scales = "free_y") +
  scale_color_manual(values = colores_sectores) +
  scale_y_continuous(labels = comma) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE)) +
  labs(
    title = "Evolución del personal total por región y sector",
    x = NULL,
    y = "Personal total"
  ) +
  tema_simple

ggplot(tabla_region_sector_anual, aes(x = ANIO, y = productividad_laboral, color = SECTOR_HOM)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.6) +
  facet_wrap(~ region, scales = "free_y") +
  scale_color_manual(values = colores_sectores) +
  scale_y_continuous(labels = comma) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE)) +
  labs(
    title = "Productividad laboral anual por región y sector",
    x = NULL,
    y = "Productividad laboral"
  ) +
  tema_simple

# ============================================================================
# Script completo en R para procesar datos GEIH y generar visualizaciones
# Industria Manufacturera Colombiana 2008-2025
# ============================================================================

# Cargar librerías necesarias
library(tidyverse)
library(readxl)
library(scales)

# 1. CROSSWALK PARA HOMOGENEIZAR SECTORES ===================================

crosswalk_hom <- tribble(
  ~CIIU_REV, ~CIIU_2D, ~SECTOR_HOM,
  
  # Alimentos y bebidas
  3, "15", "Alimentos y bebidas",
  4, "10", "Alimentos y bebidas",
  4, "11", "Alimentos y bebidas",
  
  # Tabaco
  3, "16", "Tabaco",
  4, "12", "Tabaco",
  
  # Textiles
  3, "17", "Textiles",
  4, "13", "Textiles",
  
  # Confecciones, cuero y calzado
  3, "18", "Confecciones, cuero y calzado",
  3, "19", "Confecciones, cuero y calzado",
  4, "14", "Confecciones, cuero y calzado",
  4, "15", "Confecciones, cuero y calzado",
  
  # Madera, papel e impresión
  3, "20", "Madera, papel e impresión",
  3, "21", "Madera, papel e impresión",
  3, "22", "Madera, papel e impresión",
  4, "16", "Madera, papel e impresión",
  4, "17", "Madera, papel e impresión",
  4, "18", "Madera, papel e impresión",
  
  # Refinación, químicos y farmacéuticos
  3, "23", "Refinación, químicos y farmacéuticos",
  3, "24", "Refinación, químicos y farmacéuticos",
  4, "19", "Refinación, químicos y farmacéuticos",
  4, "20", "Refinación, químicos y farmacéuticos",
  4, "21", "Refinación, químicos y farmacéuticos",
  
  # Caucho y plástico
  3, "25", "Caucho y plástico",
  4, "22", "Caucho y plástico",
  
  # Minerales no metálicos
  3, "26", "Minerales no metálicos",
  4, "23", "Minerales no metálicos",
  
  # Metalurgia y productos metálicos
  3, "27", "Metalurgia y productos metálicos",
  3, "28", "Metalurgia y productos metálicos",
  4, "24", "Metalurgia y productos metálicos",
  4, "25", "Metalurgia y productos metálicos",
  
  # Maquinaria y equipo
  3, "29", "Maquinaria y equipo",
  3, "30", "Maquinaria y equipo",
  3, "31", "Maquinaria y equipo",
  3, "32", "Maquinaria y equipo",
  3, "33", "Maquinaria y equipo",
  4, "26", "Maquinaria y equipo",
  4, "27", "Maquinaria y equipo",
  4, "28", "Maquinaria y equipo",
  
  # Equipo de transporte
  3, "34", "Equipo de transporte",
  3, "35", "Equipo de transporte",
  4, "29", "Equipo de transporte",
  4, "30", "Equipo de transporte",
  
  # Muebles y otras manufactureras
  3, "36", "Muebles y otras manufactureras",
  3, "37", "Muebles y otras manufactureras",
  4, "31", "Muebles y otras manufactureras",
  4, "32", "Muebles y otras manufactureras",
  4, "33", "Muebles y otras manufactureras"
)

# 2. FUNCIÓN PARA LEER TODOS LOS ARCHIVOS ===================================

leer_geih_manufactura <- function(ruta_carpeta = ".") {
  
  # Buscar archivos GEIH
  archivos <- list.files(
    path = ruta_carpeta, 
    pattern = "INGLABO_manufactura_\\d{4}\\.xlsx$", 
    full.names = TRUE
  )
  
  cat("Archivos encontrados:", length(archivos), "\n")
  
  # DATOS POR SECTOR
  datos_sector <- map_dfr(archivos, function(archivo) {
    año <- str_extract(basename(archivo), "\\d{4}") %>% as.integer()
    rev <- ifelse(año < 2020, 3, 4)
    
    cat("  Procesando año:", año, "(CIIU Rev", rev, ")\n")
    
    df <- read_excel(archivo, sheet = "por_sector")
    
    # Normalizar nombre de columna RAMA
    if ("RAMA2D" %in% names(df)) {
      df <- df %>% rename(CIIU_2D = RAMA2D)
    } else if ("RAMA2D_R4" %in% names(df)) {
      df <- df %>% rename(CIIU_2D = RAMA2D_R4)
    }
    
    df %>%
      mutate(
        año = año,
        CIIU_REV = rev,
        CIIU_2D = as.character(CIIU_2D)
      ) %>%
      select(año, CIIU_REV, CIIU_2D, trabajadores, mean_inglabo, median_inglabo)
  })
  
  # DATOS POR REGIÓN
  datos_region <- map_dfr(archivos, function(archivo) {
    año <- str_extract(basename(archivo), "\\d{4}") %>% as.integer()
    
    df <- read_excel(archivo, sheet = "por_region")
    
    df %>%
      mutate(año = año) %>%
      select(año, REGION, trabajadores, mean_inglabo, median_inglabo)
  })
  
  list(
    sector = datos_sector,
    region = datos_region
  )
}

# 3. PROCESAR Y HOMOGENEIZAR DATOS ==========================================

cat("\n=== LEYENDO ARCHIVOS GEIH ===\n")
datos_geih <- leer_geih_manufactura()

cat("\n=== HOMOGENEIZANDO SECTORES ===\n")
# Homogeneizar sectores
datos_sector_hom <- datos_geih$sector %>%
  left_join(crosswalk_hom, by = c("CIIU_REV", "CIIU_2D")) %>%
  filter(!is.na(SECTOR_HOM)) %>%
  group_by(año, SECTOR_HOM) %>%
  summarise(
    trabajadores = sum(trabajadores, na.rm = TRUE),
    # Promedio ponderado: suma(ingreso * trabajadores) / suma(trabajadores)
    mean_inglabo = sum(mean_inglabo * trabajadores, na.rm = TRUE) / sum(trabajadores, na.rm = TRUE),
    median_inglabo = median(median_inglabo, na.rm = TRUE),
    .groups = "drop"
  )

# Limpiar datos de región
datos_region_clean <- datos_geih$region %>%
  mutate(
    REGION = case_when(
      str_detect(REGION, "(?i)amaz") ~ "Amazonía",
      str_detect(REGION, "(?i)andin") ~ "Andina",
      str_detect(REGION, "(?i)caribe|atl") ~ "Caribe",
      str_detect(REGION, "(?i)orin") ~ "Orinoquía",
      str_detect(REGION, "(?i)pac") ~ "Pacífica",
      TRUE ~ REGION
    )
  ) %>%
  filter(REGION %in% c("Amazonía", "Andina", "Caribe", "Orinoquía", "Pacífica"))

cat("\n✓ Datos procesados exitosamente!\n")
cat("  Sectores únicos:", length(unique(datos_sector_hom$SECTOR_HOM)), "\n")
cat("  Registros por sector:", nrow(datos_sector_hom), "\n")
cat("  Regiones únicas:", paste(unique(datos_region_clean$REGION), collapse = ", "), "\n")
cat("  Registros por región:", nrow(datos_region_clean), "\n")

# 4. GENERAR GRÁFICAS ========================================================

cat("\n=== GENERANDO GRÁFICAS ===\n")

# Paleta de colores personalizada
colores_sector <- c(
  "Alimentos y bebidas" = "#FF6B35",
  "Confecciones, cuero y calzado" = "#2C3E50",
  "Muebles y otras manufactureras" = "#6A4C93",
  "Madera, papel e impresión" = "#1A535C",
  "Refinación, químicos y farmacéuticos" = "#00CED1",
  "Metalurgia y productos metálicos" = "#9B59B6",
  "Caucho y plástico" = "#E74C3C",
  "Maquinaria y equipo" = "#3498DB",
  "Minerales no metálicos" = "#95A99C",
  "Textiles" = "#F39C12",
  "Equipo de transporte" = "#16A085",
  "Tabaco" = "#2ECC71"
)

colores_region <- c(
  "Andina" = "#2E86AB",
  "Pacífica" = "#06A77D",
  "Caribe" = "#F77F00",
  "Orinoquía" = "#D62828",
  "Amazonía" = "#6A4C93"
)

# Tema personalizado
tema_custom <- theme_classic() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 8),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "gray30"),
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 9)
  )

# GRÁFICA 1: Trabajadores por sector
cat("  [1/6] Trabajadores por sector...\n")

# Obtener última posición de cada sector para etiquetas
ultimas_pos_trab <- datos_sector_hom %>%
  group_by(SECTOR_HOM) %>%
  filter(año == max(año)) %>%
  ungroup()

ggplot(datos_sector_hom, aes(x = año, y = trabajadores, color = SECTOR_HOM)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  geom_text(data = ultimas_pos_trab, 
            aes(label = SECTOR_HOM), 
            hjust = 0, 
            nudge_x = 0.3,
            size = 2.8,
            show.legend = FALSE) +
  scale_y_continuous(labels = comma_format(big.mark = ",", decimal.mark = ".")) +
  scale_x_continuous(limits = c(min(datos_sector_hom$año), max(datos_sector_hom$año) + 4)) +
  scale_color_manual(values = colores_sector) +
  labs(
    title = "Evolución de trabajadores por sector manufacturero",
    subtitle = "Datos GEIH 2008-2025 - Sectores homogeneizados CIIU Rev 3 y 4",
    x = "Año",
    y = "Número de trabajadores"
  ) +
  tema_custom +
  theme(legend.position = "bottom")

# GRÁFICA 2: Ingreso medio por sector
cat("  [2/6] Ingreso medio por sector...\n")

ultimas_pos_medio <- datos_sector_hom %>%
  group_by(SECTOR_HOM) %>%
  filter(año == max(año)) %>%
  ungroup()

ggplot(datos_sector_hom, aes(x = año, y = mean_inglabo, color = SECTOR_HOM)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  geom_text(data = ultimas_pos_medio, 
            aes(label = SECTOR_HOM), 
            hjust = 0, 
            nudge_x = 0.3,
            size = 2.8,
            show.legend = FALSE) +
  scale_y_continuous(labels = dollar_format(big.mark = ",", decimal.mark = ".", prefix = "$")) +
  scale_x_continuous(limits = c(min(datos_sector_hom$año), max(datos_sector_hom$año) + 4)) +
  scale_color_manual(values = colores_sector) +
  labs(
    title = "Evolución del ingreso medio laboral por sector",
    subtitle = "Datos GEIH 2008-2025 - Sectores homogeneizados (pesos corrientes)",
    x = "Año",
    y = "Ingreso medio"
  ) +
  tema_custom +
  theme(legend.position = "none")

# GRÁFICA 3: Ingreso mediano por sector
cat("  [3/6] Ingreso mediano por sector...\n")

ultimas_pos_mediano <- datos_sector_hom %>%
  group_by(SECTOR_HOM) %>%
  filter(año == max(año)) %>%
  ungroup()

ggplot(datos_sector_hom, aes(x = año, y = median_inglabo, color = SECTOR_HOM)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  geom_text(data = ultimas_pos_mediano, 
            aes(label = SECTOR_HOM), 
            hjust = 0, 
            nudge_x = 0.3,
            size = 2.8,
            show.legend = FALSE) +
  scale_y_continuous(labels = dollar_format(big.mark = ",", decimal.mark = ".", prefix = "$")) +
  scale_x_continuous(limits = c(min(datos_sector_hom$año), max(datos_sector_hom$año) + 4)) +
  scale_color_manual(values = colores_sector) +
  labs(
    title = "Evolución del ingreso mediano laboral por sector",
    subtitle = "Datos GEIH 2008-2025 - Sectores homogeneizados (pesos corrientes)",
    x = "Año",
    y = "Ingreso mediano"
  ) +
  tema_custom +
  theme(legend.position = "none")

# GRÁFICA 4: Trabajadores por región
cat("  [4/6] Trabajadores por región...\n")
ggplot(datos_region_clean, aes(x = año, y = trabajadores, color = REGION)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  scale_y_continuous(labels = comma_format(big.mark = ",", decimal.mark = ".")) +
  scale_color_manual(values = colores_region) +
  labs(
    title = "Evolución de trabajadores por región",
    subtitle = "Sector manufacturero - Datos GEIH 2008-2025",
    x = "Año",
    y = "Número de trabajadores",
    color = "Región"
  ) +
  tema_custom

# GRÁFICA 5: Ingreso medio por región
cat("  [5/6] Ingreso medio por región...\n")
ggplot(datos_region_clean, aes(x = año, y = mean_inglabo, color = REGION)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  scale_y_continuous(labels = dollar_format(big.mark = ",", decimal.mark = ".", prefix = "$")) +
  scale_color_manual(values = colores_region) +
  labs(
    title = "Evolución del ingreso medio laboral por región",
    subtitle = "Sector manufacturero - Datos GEIH 2008-2025 (pesos corrientes)",
    x = "Año",
    y = "Ingreso medio",
    color = "Región"
  ) +
  tema_custom

# GRÁFICA 6: Ingreso mediano por región
cat("  [6/6] Ingreso mediano por región...\n")
ggplot(datos_region_clean, aes(x = año, y = median_inglabo, color = REGION)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  scale_y_continuous(labels = dollar_format(big.mark = ",", decimal.mark = ".", prefix = "$")) +
  scale_color_manual(values = colores_region) +
  labs(
    title = "Evolución del ingreso mediano laboral por región",
    subtitle = "Sector manufacturero - Datos GEIH 2008-2025 (pesos corrientes)",
    x = "Año",
    y = "Ingreso mediano",
    color = "Región"
  ) +
  tema_custom
