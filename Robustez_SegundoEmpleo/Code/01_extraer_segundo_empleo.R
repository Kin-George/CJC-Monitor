# Extrae de la GEIH cruda las variables del segundo empleo y las llaves originales de cada persona ocupada.
#
# Por qué: la base del paper (Paper-GEIH_base_modelo_personas_2008_2025.dta) no conserva el indicador de segundo empleo ni
# las llaves originales (año, mes, DIRECTORIO, SECUENCIA_P, ORDEN). Este script las trae de los archivos crudos.
#
# Población: personas ocupadas (OCI == 1) de cada archivo anual GEIH_<año>_total.dta, en el MISMO orden en que vienen en el archivo.
# Un archivo anual junta los 12 meses (2009 no trae julio). El factor de expansión mensual (FEX_C18 desde 2021, FEX_C hasta 2019,
# fex_c_2011 en 2008-2009) se divide entre 12 para que sume una población anual, igual que en la base del paper.
# 2020 no se usa (la GEIH 2020 no trae los 12 meses de ocupados; la base del paper tampoco lo incluye).
#
# Salidas (Robustez_SegundoEmpleo/Datos/):
#   por_anio/llaves_<año>.rds                  un archivo por año (permite retomar si se interrumpe)
#   segundo_empleo_llaves_2008_2025.dta        todos los años, una fila por persona ocupada y año
#   segundo_empleo_llaves_2008_2025.csv.gz     lo mismo en csv comprimido
# Uso:  Rscript Robustez_SegundoEmpleo/Code/01_extraer_segundo_empleo.R [año ...]   (desde la raíz del repositorio)

source(if (file.exists("Code/_paths.R")) "Code/_paths.R" else "_paths.R")
setwd(PROJECT_ROOT)
suppressMessages({library(haven); library(dplyr)})

RAW <- "C:/Users/jorge/Documents/Databases/GEIH"
dir_dat <- project_path("Robustez_SegundoEmpleo", "Datos")
dir_anio <- file.path(dir_dat, "por_anio")
dir.create(dir_anio, recursive = TRUE, showWarnings = FALSE)
args <- commandArgs(trailingOnly = TRUE)
anios <- if (length(args)) as.integer(args) else setdiff(2008:2025, 2020)

CLAVES <- c("MES", "PERIODO", "DIRECTORIO", "SECUENCIA_P", "ORDEN", "HOGAR")
VARS <- c("OCI", "P6040", "EDAD", "P6020", "P3271", "SEXO", "DPTO", "AREA", "P6430", "P6800", "P6850", "P7040", "P7045", "P7070", "INGLABO")
FEX <- c("FEX_C18", "FEX_C", "FEX_C_2011")

leer <- function(anio) {
  f <- file.path(RAW, sprintf("GEIH_%d_total.dta", anio))
  nm <- names(read_dta(f, n_max = 0))
  up <- setNames(nm, toupper(nm))
  fex <- FEX[FEX %in% names(up)][1]
  quiero <- up[intersect(c(CLAVES, VARS, fex), names(up))]
  d <- read_dta(f, col_select = all_of(unname(quiero)))
  names(d) <- toupper(names(d))
  d <- d %>% mutate(across(everything(), ~ as.numeric(zap_labels(.x))))
  # algunos años (p. ej. 2022) traen la edad y el sexo como EDAD y SEXO en vez de P6040 y P6020/P3271
  if (!"P6040" %in% names(d) && "EDAD" %in% names(d)) d$P6040 <- d$EDAD
  if (!"P6020" %in% names(d) && !"P3271" %in% names(d) && "SEXO" %in% names(d)) d$P6020 <- d$SEXO
  d <- d[d$OCI == 1 & !is.na(d$OCI), ]
  d$FEX_MENSUAL <- d[[fex]]
  d <- cbind(anio = anio, fila_ocupados_anio = seq_len(nrow(d)), d)
  d$fuente_factor <- fex
  d$fex <- d$FEX_MENSUAL / 12
  d
}

for (a in anios) {
  f_out <- file.path(dir_anio, sprintf("llaves_%d.rds", a))
  if (file.exists(f_out)) { cat(a, "ya extraído\n"); next }
  d <- leer(a)
  saveRDS(d, f_out)
  cat(a, format(nrow(d), big.mark = ","), "ocupados | meses:", paste(sort(unique(d$MES)), collapse = " "), "| factor:", d$fuente_factor[1], "\n")
  rm(d); invisible(gc())
}

x <- bind_rows(lapply(setdiff(2008:2025, 2020), function(a) readRDS(file.path(dir_anio, sprintf("llaves_%d.rds", a)))))
x <- x %>% mutate(
  segundo_empleo = case_when(P7040 == 1 ~ 1, P7040 == 2 ~ 0, TRUE ~ NA_real_),   # P7040: 1 = sí, 2 = no
  p7045_codigo_especial = as.integer(!is.na(P7045) & P7045 %in% c(98, 99)),     # 98 trabajó y no sabe cuántas; 99 no sabe si trabajó
  p7070_estado = case_when(is.na(P7070) ~ "faltante", P7070 == 0 ~ "cero", P7070 > 0 ~ "positivo", TRUE ~ "negativo"),
  inglabo_estado = case_when(is.na(INGLABO) ~ "faltante", INGLABO == 0 ~ "cero", INGLABO > 0 ~ "positivo", TRUE ~ "negativo")
)
names(x)[names(x) %in% CLAVES] <- tolower(names(x)[names(x) %in% CLAVES])
write_dta(x, file.path(dir_dat, "segundo_empleo_llaves_2008_2025.dta"))
write.csv(x, gzfile(file.path(dir_dat, "segundo_empleo_llaves_2008_2025.csv.gz")), row.names = FALSE)
cat("\nfilas:", format(nrow(x), big.mark = ","), "| columnas:", ncol(x), "\n")
