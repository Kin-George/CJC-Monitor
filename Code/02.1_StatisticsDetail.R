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
library(tidyr)
library(readxl)
# Tablas con lo disponible
# Productividad laboral = Valor agregado real / total de personal ocupado 

# Entender la EAM - Replica boletin tecnico
eam_2024 <- read_dta("Datos/Raw/EAM/EAM_2024.dta")
# Grafico 1. Resumen de variables principales (Billones de pesos)
resumen_eam <- eam_2024 %>%
  summarise(
    Producción_Bruta = sum(PRODBR2, na.rm = TRUE),
    Consumo_Intermedio = sum(CONSIN2, na.rm = TRUE),
    Valor_Agregado = sum(VALAGRI, na.rm = TRUE)
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Variable",
    values_to = "Valor_pesos"
  ) %>%
  mutate(
    Valor_billones = Valor_pesos / 1e9
  )


ggplot(resumen_eam, aes(x = Valor_billones, y = Variable, fill = Variable)) +
  geom_col(height = 0.6) +
  geom_text(
    aes(label = round(Valor_billones, 1)),
    hjust = -0.1,
    size = 4
  ) +
  labs(
    title = "EAM 2024 – Resumen de variables principales",
    subtitle = "Billones de pesos corrientes",
    x = "Billones de pesos corrientes",
    y = NULL
  ) +
  scale_x_continuous(
    labels = label_number(decimal.mark = ",", big.mark = ".", accuracy = 0.1),
    expand = expansion(mult = c(0, 0.15))
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold"),
    axis.text.y = element_text(face = "bold")
  )

### Conclusion: Las variables de valor agregado y otras, estan en MILES DE PESOS.

# Numero de establecimientos
resumen_empleo_eam <- eam_2024 %>%
  summarise(
    establecimientos = n_distinct(nordest),
    
    total_empleados = sum(PERTOTAL, na.rm = TRUE),
    
    empleados_directos = sum(PPERYTEM, na.rm = TRUE),
    
    empleados_permanentes = sum(PERSOCU, na.rm = TRUE),
    
    empleados_temporales_directos = sum(PERTEM3, na.rm = TRUE),
    
    empleados_empresas_especializadas = sum(
      c4r4c7t+c4r4c8t,
      na.rm = TRUE
    ),
    
    aprendices = sum(c4r6tm + c4r6th, na.rm = TRUE),
    
    propietarios = sum(c4r4c1t + c4r4c2t, na.rm = TRUE)
  )

resumen_empleo_eam
# No da exacto pero estos son los calculos correctos.

# Gráfico 3. Remuneraciones causadas: sueldos, salarios y prestaciones

resumen_remuneraciones <- eam_2024 %>%
  summarise(
    Prestaciones = sum(PRESPYTE, na.rm = TRUE),
    `Sueldos y salarios` = sum(SALPEYTE, na.rm = TRUE),
    Remuneraciones = sum(Prestaciones+`Sueldos y salarios`, na.rm = TRUE)
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Variable",
    values_to = "Valor_pesos"
  ) %>%
  mutate(
    Valor_billones = Valor_pesos / 1e9,
    Variable = factor(
      Variable,
      levels = c("Remuneraciones", "Sueldos y salarios", "Prestaciones")
    )
  )

ggplot(resumen_remuneraciones, aes(x = Variable, y = Valor_billones, fill = Variable)) +
  geom_col(width = 0.45) +
  geom_text(
    aes(label = label_number(decimal.mark = ",", accuracy = 0.1)(Valor_billones)),
    vjust = -0.4,
    size = 4
  ) +
  labs(
    title = "EAM 2024 – Remuneraciones causadas",
    subtitle = "Sueldos, salarios y prestaciones. Billones de pesos corrientes",
    x = NULL,
    y = "Billones de pesos"
  ) +
  scale_y_continuous(
    labels = label_number(decimal.mark = ",", big.mark = ".", accuracy = 0.1),
    expand = expansion(mult = c(0, 0.15))
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(face = "bold")
  )

### Da muy similar.

# Cuadro producción bruta por grupo industrial CIIU Rev.4
ciiu_dic <- read_excel("DocumentacionAuxiliar/Estructura-detallada-CIIU-4AC-2022.xlsx")

# Filtrar manufacturas: desde SECCIÓN C hasta antes de SECCIÓN D
fila_inicio <- which(ciiu_dic$`División` == "SECCIÓN C")
fila_fin <- which(ciiu_dic$`División` == "SECCIÓN D") - 1

ciiu_manufacturas <- ciiu_dic[fila_inicio:fila_fin, ] %>%
  slice(-1)

# OJO: llenar División y Grupo, pero NO Clase
ciiu_manufacturas_fill <- ciiu_manufacturas %>%
  fill(`División`, Grupo, .direction = "down")

# Diccionario clase 4 dígitos -> grupo 3 dígitos
ciiu_clase_grupo <- ciiu_manufacturas_fill %>%
  filter(!is.na(Clase)) %>%
  select(
    CIIU_4d = Clase,
    CIIU_3d = Grupo
  ) %>%
  distinct(CIIU_4d, .keep_all = TRUE)

# Descripción del grupo a 3 dígitos
ciiu_grupos <- ciiu_manufacturas %>%
  filter(!is.na(Grupo)) %>%
  select(
    CIIU_3d = Grupo,
    DescripcionGrupo = `Descripción`
  ) %>%
  distinct(CIIU_3d, .keep_all = TRUE)

# Pegar a EAM
eam_2024_grupo <- eam_2024 %>%
  mutate(
    CIIU_4d = stringr::str_pad(as.character(ciiu4), 4, pad = "0")
  ) %>%
  left_join(ciiu_clase_grupo, by = "CIIU_4d") %>%
  left_join(ciiu_grupos, by = "CIIU_3d")

# Ya tenemos la seccion de manufactura

# Cuadro 1. Producción bruta por grupo industrial CIIU Rev.4

cuadro_1_base <- eam_2024_grupo %>%
  group_by(CIIU_3d, DescripcionGrupo) %>%
  summarise(
    Produccion_bruta = sum(PRODBR2, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Produccion_bruta_millones = Produccion_bruta / 1000
  ) %>%
  arrange(desc(Produccion_bruta_millones))

total_prod <- sum(cuadro_1_base$Produccion_bruta_millones, na.rm = TRUE)

cuadro_1_base <- cuadro_1_base %>%
  mutate(
    Part_pct = 100 * Produccion_bruta_millones / total_prod
  )

cuadro_1_top <- cuadro_1_base %>%
  slice_head(n = 17)

cuadro_1_resto <- cuadro_1_base %>%
  slice(-(1:17)) %>%
  summarise(
    CIIU_3d = "",
    DescripcionGrupo = "Resto de industria",
    Produccion_bruta_millones = sum(Produccion_bruta_millones, na.rm = TRUE),
    Part_pct = sum(Part_pct, na.rm = TRUE)
  )

cuadro_1_total <- tibble(
  CIIU_3d = "Total",
  DescripcionGrupo = "",
  Produccion_bruta_millones = total_prod,
  Part_pct = 100
)

cuadro_1 <- bind_rows(
  cuadro_1_total,
  cuadro_1_top,
  cuadro_1_resto
) %>%
  mutate(
    Produccion_bruta_millones = round(Produccion_bruta_millones, 0),
    Part_pct = round(Part_pct, 1)
  ) %>%
  rename(
    `Grupo industrial CIIU Rev.4` = CIIU_3d,
    Descripción = DescripcionGrupo,
    `Millones de pesos Producción bruta` = Produccion_bruta_millones,
    `Part.%` = Part_pct
  )

# No se por que no da igual

# =====================================================
# CUADRO 2. Personal ocupado con descripción
# =====================================================

cuadro_2_base <- eam_2024_grupo %>%
  group_by(CIIU_3d, DescripcionGrupo) %>%
  summarise(
    Personal_ocupado = sum(PERTOTAL, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(Personal_ocupado))

total_personal <- sum(cuadro_2_base$Personal_ocupado, na.rm = TRUE)

cuadro_2_base <- cuadro_2_base %>%
  mutate(
    Part_pct = 100 * Personal_ocupado / total_personal
  )

cuadro_2_top <- cuadro_2_base %>%
  slice_head(n = 18)

cuadro_2_resto <- cuadro_2_base %>%
  slice(-(1:18)) %>%
  summarise(
    CIIU_3d = "",
    DescripcionGrupo = "Resto de industria",
    Personal_ocupado = sum(Personal_ocupado, na.rm = TRUE),
    Part_pct = sum(Part_pct, na.rm = TRUE)
  )

cuadro_2_total <- tibble(
  CIIU_3d = "Total",
  DescripcionGrupo = "",
  Personal_ocupado = total_personal,
  Part_pct = 100
)

cuadro_2 <- bind_rows(
  cuadro_2_total,
  cuadro_2_top,
  cuadro_2_resto
) %>%
  mutate(
    Personal_ocupado = round(Personal_ocupado, 0),
    Part_pct = round(Part_pct, 1)
  ) %>%
  rename(
    `Grupo industrial CIIU Rev.4` = CIIU_3d,
    Descripción = DescripcionGrupo,
    `Personal ocupado` = Personal_ocupado,
    `Part.%` = Part_pct
  )

# =====================================================
# GRÁFICO 8. Valor agregado con descripción
# =====================================================

grafico_8_base <- eam_2024_grupo %>%
  group_by(CIIU_3d, DescripcionGrupo) %>%
  summarise(
    Valor_agregado = sum(VALAGRI, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Part_pct = 100 * Valor_agregado / sum(Valor_agregado, na.rm = TRUE)
  ) %>%
  arrange(desc(Part_pct))

grafico_8_top <- grafico_8_base %>%
  slice_head(n = 17)

grafico_8_resto <- grafico_8_base %>%
  slice(-(1:17)) %>%
  summarise(
    CIIU_3d = "",
    DescripcionGrupo = "Resto de industria",
    Valor_agregado = sum(Valor_agregado, na.rm = TRUE),
    Part_pct = sum(Part_pct, na.rm = TRUE)
  )

grafico_8_data <- bind_rows(
  grafico_8_top,
  grafico_8_resto
) %>%
  mutate(
    Part_pct = round(Part_pct, 1),
    DescripcionGrupo = factor(DescripcionGrupo, levels = rev(DescripcionGrupo))
  )

ggplot(grafico_8_data, aes(x = Part_pct, y = DescripcionGrupo, fill = DescripcionGrupo)) +
  geom_col(height = 0.6) +
  geom_text(
    aes(label = label_number(decimal.mark = ",", accuracy = 0.1)(Part_pct)),
    hjust = -0.1,
    size = 3.8
  ) +
  labs(
    title = "EAM 2024 – Grupos industriales con mayor participación en valor agregado",
    subtitle = "Total nacional",
    x = "Porcentaje",
    y = "Grupos industriales"
  ) +
  scale_x_continuous(
    labels = label_number(decimal.mark = ",", accuracy = 0.1),
    expand = expansion(mult = c(0, 0.15))
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold"),
    axis.text.y = element_text(face = "bold")
  )

# Estadisticas descriptivas de establecimientos que reportan 0, no reportan y si reportan 
resumen_valor_agregado_est <- eam_2024 %>%
  summarise(
    total_establecimientos = n_distinct(nordest),
    
    establecimientos_valor_agregado_missing = n_distinct(nordest[is.na(VALAGRI)]),
    
    establecimientos_valor_agregado_cero = n_distinct(nordest[!is.na(VALAGRI) & VALAGRI == 0]),
    
    establecimientos_valor_agregado_positivo = n_distinct(nordest[!is.na(VALAGRI) & VALAGRI > 0]),
    
    establecimientos_valor_agregado_negativo = n_distinct(nordest[!is.na(VALAGRI) & VALAGRI < 0])
  )

resumen_valor_agregado_est



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

