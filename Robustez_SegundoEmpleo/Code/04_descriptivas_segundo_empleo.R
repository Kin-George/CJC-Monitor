# Descriptivas del segundo empleo en la GEIH, con la definición correcta de cada variable.
#
# Población: personas ocupadas (OCI == 1) de los archivos anuales GEIH_<año>_total.dta, sin 2020; todos los meses de cada año.
# Ponderación: factor de expansión mensual / 12 (columna `fex`), el mismo de la base del paper. Todos los porcentajes están ponderados.
# Las columnas `personas_anio_miles` suman las poblaciones anuales de todos los años (miles de personas-año), no una población de un año.
#
# Definiciones (documentación DANE de la GEIH 2025, catálogo 853):
#   P6800  horas A LA SEMANA que trabaja NORMALMENTE en el empleo principal (habituales)
#   P6850  horas trabajadas LA SEMANA PASADA en el empleo principal (efectivas)
#   P7045  horas trabajadas LA SEMANA PASADA en el segundo empleo (efectivas). 0 = ausente esa semana; 98 = trabajó y no sabe cuántas;
#          99 = no sabe si trabajó; rango válido 0-75.
#   P7040  indicador de segundo empleo (1 = sí, 2 = no)
#   P7070  ingreso del mes pasado en el segundo empleo
# (Verificado en la documentación de 2025; falta verificar que la definición sea la misma en los años anteriores.)
#
# Salidas: Robustez_SegundoEmpleo/Outputs/tables/descriptivas_*.csv
# Uso    : Rscript Robustez_SegundoEmpleo/Code/04_descriptivas_segundo_empleo.R   (desde la raíz del repositorio)

source(if (file.exists("Code/_paths.R")) "Code/_paths.R" else "_paths.R")
setwd(PROJECT_ROOT)
suppressMessages({library(haven); library(dplyr); library(tidyr)})
dir_dat <- project_path("Robustez_SegundoEmpleo", "Datos")
dir_out <- project_path("Robustez_SegundoEmpleo", "Outputs", "tables")
dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)

k <- read_dta(file.path(dir_dat, "segundo_empleo_llaves_2008_2025.dta")) %>%
  mutate(
    p7045_valida = !is.na(P7045) & P7045 >= 0 & P7045 <= 75,
    p7045_pos = p7045_valida & P7045 > 0,
    p6850_valida = !is.na(P6850) & P6850 >= 0 & P6850 <= 168,
    p6800_valida = !is.na(P6800) & P6800 > 0 & P6800 <= 168,
    h2_valida = ifelse(p7045_valida, P7045, NA_real_)
  )
wp <- function(x, w) 100 * sum(w * x, na.rm = TRUE) / sum(w[!is.na(x)])

# 1. Frecuencia y calidad de las variables, por año (entre todos los ocupados)
d1 <- k %>% group_by(anio) %>% summarise(
  ocupados_muestra = n(),
  pct_segundo_empleo = wp(segundo_empleo == 1, fex),
  pct_P7045_valido_positivo = wp(p7045_pos, fex),
  pct_P7045_codigo_98_99 = wp(p7045_codigo_especial == 1, fex),
  pct_P7070_positivo = wp(p7070_estado == "positivo", fex),
  .groups = "drop")
write.csv(d1, file.path(dir_out, "descriptivas_segundo_empleo_por_anio.csv"), row.names = FALSE)

# 2. Entre quienes declaran segundo empleo: ¿qué pasa con las horas y con el ingreso? (nulo vs faltante vs positivo)
d2 <- k %>% filter(segundo_empleo == 1) %>% group_by(anio) %>% summarise(
  declaran_segundo_empleo = n(),
  pct_P7045_faltante = wp(is.na(P7045), fex),
  pct_P7045_cero = wp(!is.na(P7045) & P7045 == 0, fex),
  pct_P7045_positivo_valido = wp(p7045_pos, fex),
  pct_P7045_98_99 = wp(p7045_codigo_especial == 1, fex),
  pct_P7070_faltante = wp(p7070_estado == "faltante", fex),
  pct_P7070_cero = wp(p7070_estado == "cero", fex),
  pct_P7070_positivo = wp(p7070_estado == "positivo", fex),
  pct_P7070_otro = wp(p7070_estado %in% c("negativo", "otro"), fex),
  horas_P7045_media_si_positivo = weighted.mean(h2_valida[p7045_pos], fex[p7045_pos]),
  .groups = "drop")
write.csv(d2, file.path(dir_out, "descriptivas_segundo_empleo_horas_ingreso.csv"), row.names = FALSE)

# 3. Combinaciones horas / ingreso del segundo empleo (todos los años juntos)
d3 <- k %>% filter(segundo_empleo == 1) %>% mutate(
  horas2 = case_when(is.na(P7045) ~ "P7045 faltante", P7045 %in% c(98, 99) ~ "P7045 98/99", P7045 == 0 ~ "P7045 = 0", p7045_pos ~ "P7045 positiva", TRUE ~ "P7045 otro"),
  ingreso2 = p7070_estado) %>%
  group_by(horas2, ingreso2) %>% summarise(personas_anio_miles = round(sum(fex) / 1e3, 1), observaciones = n(), .groups = "drop") %>%
  mutate(pct = round(100 * personas_anio_miles / sum(personas_anio_miles), 1))
write.csv(d3, file.path(dir_out, "descriptivas_segundo_empleo_horas_x_ingreso.csv"), row.names = FALSE)

# 4. Aumento de las horas totales si se suman las del segundo empleo (valor válido), con distintas combinaciones
d4 <- k %>% filter(p6800_valida, p6850_valida) %>% group_by(anio) %>% summarise(
  pct_horas_habituales_mas_P7045 = 100 * sum(fex * ifelse(p7045_valida, P7045, 0)) / sum(fex * P6800),
  pct_horas_efectivas_mas_P7045 = 100 * sum(fex * ifelse(p7045_valida, P7045, 0)) / sum(fex * P6850),
  .groups = "drop")
write.csv(d4, file.path(dir_out, "descriptivas_aumento_horas_totales.csv"), row.names = FALSE)

# 5. Dónde se concentra el segundo empleo: tamaño x formalidad x posición ocupacional (base del paper, vía persona_id)
pb <- readRDS(file.path(dir_dat, "paper_base_subconjunto.rds"))
map <- readRDS(file.path(dir_dat, "correspondencia_persona_id.rds"))
m <- pb %>% left_join(map, by = c("persona_id", "anio")) %>%
  left_join(k %>% select(anio, fila_ocupados_anio, segundo_empleo, P6800, P6850, P7045, p7045_valida, p7070_estado, INGLABO), by = c("anio", "fila_ocupados_anio")) %>%
  mutate(formalidad = case_when(formal == 1 ~ "Formal", formal == 0 ~ "Informal", TRUE ~ "Pensionado ocupado"))
d5 <- m %>% group_by(tamano_empresa, formalidad) %>% summarise(
  personas_anio_miles = round(sum(fex) / 1e3, 1), pct_con_segundo_empleo = wp(segundo_empleo == 1, fex),
  personas_anio_con_segundo_empleo_miles = round(sum(fex[segundo_empleo == 1], na.rm = TRUE) / 1e3, 1), .groups = "drop") %>%
  mutate(pct_del_total_con_segundo_empleo = round(100 * personas_anio_con_segundo_empleo_miles / sum(personas_anio_con_segundo_empleo_miles), 1))
write.csv(d5, file.path(dir_out, "descriptivas_segundo_empleo_tamano_formalidad.csv"), row.names = FALSE)
d6 <- m %>% group_by(posicion_ocupacional) %>% summarise(personas_anio_miles = round(sum(fex) / 1e3, 1),
                                                         pct_con_segundo_empleo = wp(segundo_empleo == 1, fex), .groups = "drop")
write.csv(d6, file.path(dir_out, "descriptivas_segundo_empleo_posicion.csv"), row.names = FALSE)

# 6. Efecto sobre el ingreso por hora de la base: ingreso_laboral_hora usa INGLABO / (P6800 x 52/12); comparación con sumar horas del segundo empleo
d7 <- m %>% filter(segundo_empleo == 1, !is.na(horas), horas > 0, p7045_valida, P7045 > 0, !is.na(INGLABO), INGLABO > 0) %>%
  mutate(razon_hora_actual_sobre_hora_total = (INGLABO / horas) / (INGLABO / (horas + P7045))) %>%
  group_by(p7070_estado) %>% summarise(observaciones = n(), personas_anio_miles = round(sum(fex) / 1e3, 1),
    razon_media_ponderada = weighted.mean(razon_hora_actual_sobre_hora_total, fex), .groups = "drop")
write.csv(d7, file.path(dir_out, "descriptivas_ingreso_hora_con_horas_segundo_empleo.csv"), row.names = FALSE)

print(d1, n = 20); print(d2 %>% select(anio, declaran_segundo_empleo, starts_with("pct_")), n = 20); print(d3, n = 30); print(d4); print(d5, n = 40); print(d6); print(d7)
