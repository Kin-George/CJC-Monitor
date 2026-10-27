# Archivo para el coautor: una fila por persona de la base del paper (`persona_id`), con las llaves originales de la GEIH,
# el indicador de segundo empleo, sus horas e ingreso, las horas del empleo principal y el ingreso laboral total.
#
# Variables:
#   persona_id, anio, mes, directorio, secuencia_p, orden, hogar      llaves originales (una persona = anio + mes + directorio + secuencia_p + orden)
#   P7040   segundo empleo (1 sí, 2 no)           P7045  horas trabajadas la semana pasada en el segundo empleo (efectivas; 98/99 = no sabe)
#   P7070   ingreso del segundo empleo (mes pasado)
#   P6800   horas habituales del empleo principal    P6850  horas efectivas (semana pasada) del empleo principal
#   INGLABO ingreso laboral total (suma de todos los componentes, incluido el segundo empleo)
#   segundo_empleo, p7045_codigo_especial, p7070_estado, inglabo_estado   indicadores derivados (ver Docs/Nota_metodologica.md)
#   fex     factor de expansión (mensual / 12), igual al de la base del paper
# Salidas: Robustez_SegundoEmpleo/Datos/entrega_persona_id_segundo_empleo.dta  y  .csv.gz
# Uso    : Rscript Robustez_SegundoEmpleo/Code/05_archivo_para_coautor.R   (desde la raíz del repositorio)

source(if (file.exists("Code/_paths.R")) "Code/_paths.R" else "_paths.R")
setwd(PROJECT_ROOT)
suppressMessages({library(haven); library(dplyr)})
dir_dat <- project_path("Robustez_SegundoEmpleo", "Datos")

map <- readRDS(file.path(dir_dat, "correspondencia_persona_id.rds"))
kk <- read_dta(file.path(dir_dat, "segundo_empleo_llaves_2008_2025.dta"), col_select = c(
  "anio", "fila_ocupados_anio", "mes", "directorio", "secuencia_p", "orden", "hogar",
  "P7040", "P7045", "P7070", "P6800", "P6850", "INGLABO", "segundo_empleo", "p7045_codigo_especial", "p7070_estado", "inglabo_estado", "fex"))
x <- map %>% left_join(kk, by = c("anio", "fila_ocupados_anio")) %>% arrange(persona_id)
stopifnot(nrow(x) == nrow(map), !any(duplicated(x$persona_id)))
cat("filas:", format(nrow(x), big.mark = ","), "| persona_id únicos:", length(unique(x$persona_id)), "\n")
# llave original única (cuando hay HOGAR se usa; si no, anio + mes + directorio + secuencia_p + orden)
dup <- sum(duplicated(x[c("anio", "mes", "directorio", "secuencia_p", "orden")]))
cat("llaves originales (anio, mes, directorio, secuencia_p, orden) repetidas:", dup, "\n")
write_dta(x, file.path(dir_dat, "entrega_persona_id_segundo_empleo.dta"))
write.csv(x, gzfile(file.path(dir_dat, "entrega_persona_id_segundo_empleo.csv.gz")), row.names = FALSE)
