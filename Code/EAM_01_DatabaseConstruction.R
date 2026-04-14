setwd("~/Trabajo-Profesional/Javeriana/Code")

# ==============================================================================
# ENCUESTA ANUAL MANUFACTURERA (EAM) — DANE
# Script de construcción del panel longitudinal 1992–2024
# ==============================================================================
# Fuente   : DANE — Microdatos EAM, catálogos 563 (1992-94), 492 (1995-99),
#            494 (2000-2024)
# Cobertura: 32 años disponibles (falta 2020)
# Formato  : archivos .dta (Stata), uno por año
# Resultado: panel en formato long con una fila por establecimiento-año
# ------------------------------------------------------------------------------

library(haven)     # leer archivos .dta
library(dplyr)     # manipulación de datos
library(purrr)     # map / map2 para iterar sobre años
library(stringr)   # manipulación de strings
library(janitor)   # clean_names() como alternativa

RUTA_DATOS <- "~/Trabajo-Profesional/Javeriana/Datos/Raw/EAM" 
RUTA_Procesado <- "~/Trabajo-Profesional/Javeriana/Datos/Processed" 
ANIOS <- c(1992:2019, 2021:2024)

# Funcion de procesamiento
cargar_eam <- function(anio, ruta = RUTA_DATOS) {
  
  # 2.1 Construir ruta y cargar ------------------------------------------------
  archivo <- file.path(ruta, paste0("EAM_", anio, ".dta"))
  
  if (!file.exists(archivo)) {
    message("  [AVISO] No se encontró: ", archivo)
    return(NULL)
  }
  
  df <- read_dta(archivo)
  
  # 2.2 Convertir todos los nombres a MAYÚSCULAS --------------------------------
  # Este es el paso más crítico: los nombres varían entre mayúsculas y minúsculas
  # de un año a otro (ej: "dpto" en 2014–2024, "DPTO" en 1992–2013)
  names(df) <- toupper(names(df))
  
  # 2.3 Estandarizar variable CIIU ----------------------------------------------
  # Presenta 5 nombres distintos a lo largo de la serie:
  #   CIIU2N4  (1992–1994) — CIIU Rev. 2, 4 dígitos
  #   CIIU2    (1995–2000) — CIIU Rev. 2
  #   CIIU     (2001)      — CIIU Rev. 2/transición
  #   CIIU3    (2002–2011) — CIIU Rev. 3  [incluye ciiu3 en minúsculas]
  #   CIIU4    (2012–2024) — CIIU Rev. 4  [incluye ciiu_4 con guión en 2013]
  # Todos se unifican en CIIU. Se crea CIIU_REV para identificar la revisión.
  
  if ("CIIU2N4" %in% names(df)) {
    df <- df |> rename(CIIU = CIIU2N4)
    df$CIIU_REV <- 2L
  } else if ("CIIU2" %in% names(df)) {
    df <- df |> rename(CIIU = CIIU2)
    df$CIIU_REV <- 2L
  } else if ("CIIU" %in% names(df) && anio == 2001) {
    df$CIIU_REV <- 2L     # ya tiene el nombre correcto
  } else if ("CIIU3" %in% names(df)) {
    df <- df |> rename(CIIU = CIIU3)
    df$CIIU_REV <- 3L
  } else if ("CIIU_4" %in% names(df)) {
    # 2013: nombre con guión bajo
    df <- df |> rename(CIIU = CIIU_4)
    df$CIIU_REV <- 4L
  } else if ("CIIU4" %in% names(df)) {
    df <- df |> rename(CIIU = CIIU4)
    df$CIIU_REV <- 4L
  } else {
    # Si por alguna razón no hay CIIU, crear columna vacía
    df$CIIU     <- NA_real_
    df$CIIU_REV <- NA_integer_
    message("  [AVISO] Variable CIIU no encontrada en año ", anio)
  }
  
  # 2.4 Garantizar variable PERIODO y ANIO ------------------------------------
  # En 1993 y 1994 PERIODO no existe en el archivo original;
  # se construye a partir del año del archivo.
  if ("PERIODO" %in% names(df)) df <- select(df, -PERIODO)

  # 2.5 Convertir etiquetas Stata a numérico ----------------------------------
  # haven importa variables con etiquetas como clase "haven_labelled".
  # En la EAM todas las variables son numéricas, así que se convierten.
  df <- df |>
    mutate(across(everything(), ~ suppressWarnings(as.numeric(as.character(.)))))
  
  # 2.6 Variables de identificación garantizadas ------------------------------
  # NORDEMP y NORDEST siempre existen; garantizar tipo numérico
  df$ANIO <- as.integer(anio)
  df <- df |>
    mutate(
      NORDEMP = as.numeric(NORDEMP),
      NORDEST = as.numeric(NORDEST),
      DPTO    = as.numeric(DPTO)
    )
  
  message("  OK: ", anio, " — ", nrow(df), " establecimientos, ",
          ncol(df), " variables")
  df
}

lista_anual <- map(ANIOS, function(a) {
  message("Procesando ", a, "...")
  cargar_eam(a)
})

names(lista_anual) <- as.character(ANIOS)
lista_anual <- compact(lista_anual)
panel_completo <- bind_rows(lista_anual)

# Guardar panel completo
saveRDS(panel_completo,  file.path(RUTA_Procesado, "EAM_panel_completo.rds"))