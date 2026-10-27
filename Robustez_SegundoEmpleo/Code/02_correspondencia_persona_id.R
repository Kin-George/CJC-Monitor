# Correspondencia entre `persona_id` (base del paper) y las llaves originales de la GEIH.
#
# `persona_id` es el número de fila (`_n`) de la base final (CreacionPanelLimpio2008-2025.do, línea `gen long persona_id = _n`),
# así que no es una llave original. Aquí se reconstruye la correspondencia así:
#   1. Se toman, por año, los ocupados del archivo crudo en su orden original (01_extraer_segundo_empleo.py).
#   2. La base del paper es una subsecuencia de esos ocupados (mismo orden, con algunas filas descartadas): se empareja en orden
#      por factor de expansión, edad, departamento y sexo.
#   3. Se verifica con variables que no se usaron para emparejar (horas habituales P6800 e ingreso por hora): la correspondencia
#      solo se acepta si coinciden en la gran mayoría de filas (ver la tabla de verificación).
#
# Entradas: Datos/Processed/GEIH/Paper-GEIH_base_modelo_personas_2008_2025.dta
#           Robustez_SegundoEmpleo/Datos/segundo_empleo_llaves_2008_2025.dta
# Salidas : Robustez_SegundoEmpleo/Datos/paper_base_subconjunto.rds       (columnas de la base del paper usadas en la regresión)
#           Robustez_SegundoEmpleo/Datos/correspondencia_persona_id.rds   (persona_id <-> año, mes, DIRECTORIO, SECUENCIA_P, ORDEN, HOGAR)
#           Robustez_SegundoEmpleo/Outputs/tables/correspondencia_persona_id_verificacion.csv
# Uso     : Rscript Robustez_SegundoEmpleo/Code/02_correspondencia_persona_id.R   (desde la raíz del repositorio)

source(if (file.exists("Code/_paths.R")) "Code/_paths.R" else "_paths.R")
setwd(PROJECT_ROOT)
suppressMessages({library(haven); library(dplyr)})

dir_dat <- project_path("Robustez_SegundoEmpleo", "Datos")
dir_out <- project_path("Robustez_SegundoEmpleo", "Outputs", "tables")
dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)

# 1. columnas de la base del paper que usa el modelo (una sola vez)
f_sub <- file.path(dir_dat, "paper_base_subconjunto.rds")
if (!file.exists(f_sub)) {
  pb <- read_dta(geih_personas_data_path(), col_select = c(
    "persona_id", "anio", "edad", "depto", "area", "sector", "posicion_ocupacional", "tamano_empresa", "educacion",
    "mujer", "formal", "fex", "horas", "ingreso_laboral_hora", "log_ingreso_hora_real"))
  pb <- pb %>% mutate(across(c(sector, tamano_empresa, educacion, posicion_ocupacional), ~ as.character(as_factor(.x))))
  saveRDS(pb, f_sub)
}
pb <- readRDS(f_sub)

# 2. llaves originales y variables comparables del archivo crudo
kk <- read_dta(file.path(dir_dat, "segundo_empleo_llaves_2008_2025.dta"),
               col_select = c("anio", "fila_ocupados_anio", "P6040", "P6020", "P3271", "DPTO", "P6800", "INGLABO", "fex"))
cat("persona_id: filas =", nrow(pb), "| ocupados en crudo =", nrow(kk), "\n")

res <- list(); mapa <- list()
fmt <- function(x) ifelse(is.na(x), "NA", formatC(x, format = "f", digits = 6))
for (a in sort(unique(pb$anio))) {
  p <- pb %>% filter(anio == a) %>% arrange(persona_id)
  k <- kk %>% filter(anio == a) %>% arrange(fila_ocupados_anio)
  sexo_k <- ifelse(!is.na(k$P3271), k$P3271, k$P6020)   # P3271 desde 2021; P6020 antes (y SEXO, guardado como P6020, en 2022)
  edad_k <- ifelse(k$P6040 < 0 | k$P6040 > 120, NA, k$P6040)
  key_k <- paste(fmt(k$fex), edad_k, as.numeric(k$DPTO), as.integer(sexo_k == 2), sep = "|")
  key_p <- paste(fmt(p$fex), p$edad, as.numeric(p$depto), as.integer(p$mujer), sep = "|")
  # La base del paper es una SUBSECUENCIA de los ocupados crudos (conserva el orden y descarta algunas filas):
  # se recorre en orden y cada fila del paper se empareja con la primera fila cruda igual que aún no se ha usado.
  uk <- unique(key_k)
  pos <- split(seq_along(key_k), factor(key_k, levels = uk))   # posiciones crudas de cada llave, en orden
  idx <- match(key_p, uk)                                       # búsqueda con hash (no lineal)
  match_k <- rep(NA_integer_, nrow(p)); j <- 1L
  for (i in seq_along(idx)) {
    if (is.na(idx[i])) next
    v <- pos[[idx[i]]]
    l <- v[v >= j][1]
    if (!is.na(l)) { match_k[i] <- l; j <- l + 1L }
  }
  ok <- !is.na(match_k)
  if (!any(ok)) { res[[as.character(a)]] <- data.frame(anio = a, ocupados_crudo = nrow(k), filas_paper = nrow(p), filas_paper_emparejadas = 0); next }
  # verificación con variables que NO se usaron para emparejar: horas habituales (P6800) e ingreso por hora
  h <- k$P6800[match_k[ok]]; h <- ifelse(h <= 0 | h > 168, NA, h)
  ok_h <- mean(abs(h - p$horas[ok]) < 1e-6 | is.na(h) | is.na(p$horas[ok]))
  ing_k <- ifelse(!is.na(k$INGLABO[match_k[ok]]) & k$INGLABO[match_k[ok]] > 0 & !is.na(h), k$INGLABO[match_k[ok]] * 12 / (h * 52), NA)
  ok_i <- mean(abs(ing_k - p$ingreso_laboral_hora[ok]) <= 1e-6 * pmax(1, abs(p$ingreso_laboral_hora[ok])) | (is.na(ing_k) & is.na(p$ingreso_laboral_hora[ok])), na.rm = TRUE)
  res[[as.character(a)]] <- data.frame(anio = a, ocupados_crudo = nrow(k), filas_paper = nrow(p), filas_paper_emparejadas = sum(ok),
                                       pct_paper_emparejado = 100 * mean(ok), filas_crudas_usadas_una_vez = sum(!duplicated(match_k[ok])),
                                       coincide_horas_P6800 = ok_h, coincide_ingreso_hora = ok_i)
  mapa[[as.character(a)]] <- data.frame(persona_id = p$persona_id[ok], anio = a, fila_ocupados_anio = k$fila_ocupados_anio[match_k[ok]])
}
ver <- bind_rows(res)
print(ver, digits = 4)
write.csv(ver, file.path(dir_out, "correspondencia_persona_id_verificacion.csv"), row.names = FALSE)
saveRDS(bind_rows(mapa), file.path(dir_dat, "correspondencia_persona_id.rds"))
