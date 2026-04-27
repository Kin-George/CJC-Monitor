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
library(haven)
library(openxlsx)
# Tablas con lo disponible
# Productividad laboral = Valor agregado real / total de personal ocupado 
# 1. Estadisticas descriptivas de la base de datos

eam_2024 <- read_dta("Datos/Raw/EAM/EAM_2024.dta")
# Quedarme con variables relevantes
# Grupos seleccionados
# Profesional, tecnico o tecnologo
# obreros y operarios
# directivos y empleados de administracion y ventas
eam_2024 <- select(eam_2024, "nordemp", "nordest", "dpto", "ciiu4", "periodo",
                   # total personal promedio ocupado en el año 
                   "c4r1c9n", "c4r1c10n", "c4r2c9e", "c4r2c10e",
                   "c4r5c1", "c4r5c2", "c4r5c3", "c4r5c4", "c4r4c9t", "c4r4c10t",
                  # Sueldos, salarios y prestaciones sociales
                  "c3r2pt", "c3r2c1", "c3r2c2", "c3r2c3", "c3r3pt", "c3r3c1",
                  "c3r3c2", "c3r3c3", "c3r4pt", "c3r4c1", "c3r4c2", "c3r4c3",
                  "c3r10pt", "c3r10c1", "c3r10c2", "c3r10c3",
                  # Totales
                  "VALAGRI", "PERTOTAL", "PERTEM3", "PERSOCU")
# Colapsar
vars_excluir <- c("nordemp", "nordest", "dpto", "ciiu4", "periodo")

vars_sumar <- names(eam_2024) |>
  intersect(names(select(eam_2024, where(is.numeric)))) |>
  setdiff(vars_excluir)

eam_2024_ciiu4 <- eam_2024 %>%
  group_by(ciiu4) %>%
  summarise(
    across(
      all_of(vars_sumar),
      ~ sum(.x, na.rm = TRUE)
    ),
    establecimientos = n_distinct(nordest),
    empresas = n_distinct(nordemp),
    .groups = "drop"
  )

# ciiu a 2 digitos
# Diccionario CIIU Rev 4 a 2 dígitos
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

# 1. Crear CIIU a 2 dígitos desde la base a 4 dígitos
eam_2024_ciiu4_dicc <- eam_2024_ciiu4 %>%
  mutate(
    ciiu4 = as.character(ciiu4),
    CIIU_2D = str_sub(ciiu4, 1, 2)
  ) %>%
  left_join(
    dicc_rev4 %>% select(-CIIU_REV),   # quitamos CIIU_REV del diccionario
    by = "CIIU_2D"
  )

# 2. Definir variables numéricas a sumar
vars_excluir <- c("ciiu4", "CIIU_REV")

vars_sumar <- eam_2024_ciiu4_dicc %>%
  select(where(is.numeric)) %>%
  names() %>%
  setdiff(vars_excluir)

# 3. Colapsar a 2 dígitos
eam_2024_ciiu2 <- eam_2024_ciiu4_dicc %>%
  group_by(CIIU_2D, nombre_ciiu_2d) %>%
  summarise(
    across(
      all_of(vars_sumar),
      ~ sum(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  ) %>%
  arrange(CIIU_2D)

# Cambio nombre 
colnames(eam_2024_ciiu2)<-c("CIIU", "Sector", "Nac_Mujeres_Cat1", "Nac_Hombres_Cat1",
                            "Ex_Mujeres_Cat1", "Ex_Hombres_Cat1", "Mujeres_Cat2",
                            "Hombres_Cat2", "Mujeres_Cat3", "Hombres_Cat3", "Ocu_Mujeres",
                            "Ocu_Hombres", "PERMA_Sueldos_Cat1", "PERMA_Sueldos_Cat2", "PERMA_Sueldos_Cat3",
                            "PERMA_Sueldos", "PERMA_Prestaciones_Cat1", "PERMA_Prestaciones_Cat2",
                            "PERMA_Prestaciones_Cat3", "PERMA_Prestaciones", "TEMP_SueldosPresta_Cat1",
                            "TEMP_SueldosPresta_Cat2", "TEMP_SueldosPresta_Cat3", "TEMP_SueldosPresta",
                            "TotalMoney_Cat1", "TotalMoney_Cat2", "TotalMoney_Cat3", "TotalMoney",
                            "ValorAgregado", "PERTOTAL", "PERTEMP", "PERPERMA", "#Establecimientos", "#Empresas")
# Hacer los diferentes calculos
write.xlsx(eam_2024_ciiu2, "EAM_2024Sectores2.xlsx")

