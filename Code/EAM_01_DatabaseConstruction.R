setwd("~/Trabajo-Profesional/Javeriana")

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

# ==============================================================================
# ENCUESTA ANUAL MANUFACTURERA — MÓDULO TIC
# Construcción panel longitudinal EAM_TIC 2008–2017
# ==============================================================================

RUTA_DATOS <- "~/Trabajo-Profesional/Javeriana/Datos/Raw/EAM_TIC"
RUTA_OUT   <- "~/Trabajo-Profesional/Javeriana/Datos/Processed"

ANIOS <- 2008:2017

# ------------------------------------------------------------------------------
# 2. Función para cargar cada base anual
# ------------------------------------------------------------------------------

cargar_eam_tic <- function(anio, ruta = RUTA_DATOS) {
  
  archivo <- file.path(ruta, paste0("EAM_TIC_", anio, ".dta"))
  
  if (!file.exists(archivo)) {
    message("[AVISO] No se encontró: ", archivo)
    return(NULL)
  }
  
  df <- read_dta(archivo)
  
  # Estandarizar nombres de variables
  names(df) <- toupper(names(df))
  
  # Convertir variables etiquetadas de Stata
  df <- df |>
    mutate(across(
      everything(),
      ~ suppressWarnings(as.numeric(as.character(.)))
    ))
  
  # Crear variable de año
  df$ANIO <- as.integer(anio)
  
  # Crear PERIODO si no existe
  if (!"PERIODO" %in% names(df)) {
    df$PERIODO <- as.integer(anio)
  }
  
  # Revisar identificador principal
  if (!"NORDEMP" %in% names(df)) {
    message("[AVISO] No se encontró NORDEMP en ", anio)
  } else {
    df$NORDEMP <- as.numeric(df$NORDEMP)
  }
  
  # Ordenar columnas: identificadores primero
  ids <- c("ANIO", "PERIODO", "NORDEMP")
  ids <- ids[ids %in% names(df)]
  
  otras_vars <- setdiff(names(df), ids)
  
  df <- df |>
    select(all_of(ids), all_of(otras_vars))
  
  message(
    "OK: ", anio, " — ",
    nrow(df), " observaciones, ",
    ncol(df), " variables"
  )
  
  return(df)
}

# ------------------------------------------------------------------------------
# 3. Cargar y unir todas las bases
# ------------------------------------------------------------------------------

lista_anual <- map(ANIOS, cargar_eam_tic)

lista_anual <- compact(lista_anual)

panel_eam_tic <- bind_rows(lista_anual)

# Guardar base
saveRDS(
  panel_eam_tic,
  file.path(RUTA_OUT, "EAM_TIC_panel.rds")
)

# ==============================================================================
# ENCUESTA ANUAL DE COMERCIO — EAC
# Construcción panel longitudinal EAC 2003–2024
# ==============================================================================

RUTA_DATOS <- "~/Trabajo-Profesional/Javeriana/Datos/Raw/EAC"
RUTA_OUT   <- "~/Trabajo-Profesional/Javeriana/Datos/Processed"

ANIOS <- 2003:2024

buscar_archivo_eac <- function(anio, ruta = RUTA_DATOS) {
  
  candidatos <- c(
    file.path(ruta, paste0("EAC_", anio, ".dta"))
  )
  
  # Caso especial: el DANE publica una estructura conjunta 2017_2018
  if (anio %in% c(2017, 2018)) {
    candidatos <- c(
      file.path(ruta, paste0("EAC_", anio, ".dta")),
      file.path(ruta, "EAC_2017_2018.dta"),
      candidatos
    )
  }
  
  archivo <- candidatos[file.exists(candidatos)]
  
  if (length(archivo) == 0) {
    return(NA_character_)
  }
  
  archivo[1]
}

# ------------------------------------------------------------------------------
# 3. Función para cargar cada base anual
# ------------------------------------------------------------------------------

cargar_eac <- function(anio, ruta = RUTA_DATOS) {
  
  archivo <- buscar_archivo_eac(anio, ruta)
  
  if (is.na(archivo)) {
    message("[AVISO] No se encontró archivo para el año ", anio)
    return(NULL)
  }
  
  df <- read_dta(archivo)
  
  # Estandarizar nombres de variables
  names(df) <- toupper(names(df))
  
  # Convertir variables etiquetadas de Stata y columnas numéricas
  df <- df |>
    mutate(across(
      everything(),
      ~ suppressWarnings(as.numeric(as.character(.)))
    ))
  
  # ---------------------------------------------------------------------------
  # 3.1 Armonización de identificadores
  # ---------------------------------------------------------------------------
  
  # ID de empresa:
  # 2003–2014: IDNOREMP
  # 2015: IDNOREMP_PUBL
  # 2016–2022: IDNOREMP
  # 2023–2024: V1 en la estructura pública
  if (!"IDNOREMP" %in% names(df)) {
    
    if ("IDNOREMP_PUBL" %in% names(df)) {
      df <- df |> rename(IDNOREMP = IDNOREMP_PUBL)
    }
    
    if ("V1" %in% names(df)) {
      df <- df |> rename(IDNOREMP = V1)
    }
  }
  
  # Departamento:
  # En 2007 aparece IDEPTO; en otros años aparece IDDEPTO
  if (!"IDDEPTO" %in% names(df) && "IDEPTO" %in% names(df)) {
    df <- df |> rename(IDDEPTO = IDEPTO)
  }
  
  # Organización jurídica:
  # En algunos años aparece IDOJ1 en lugar de IDOJ
  if (!"IDOJ" %in% names(df) && "IDOJ1" %in% names(df)) {
    df <- df |> rename(IDOJ = IDOJ1)
  }
  
  # ---------------------------------------------------------------------------
  # 3.2 Año y periodo
  # ---------------------------------------------------------------------------
  
  df$ANIO <- as.integer(anio)
  
  if (!"PERIODO" %in% names(df)) {
    df$PERIODO <- as.integer(anio)
  } else {
    df$PERIODO <- as.integer(df$PERIODO)
  }
  
  # Si el archivo 2017_2018 trae ambos años en una sola base,
  # conservar solo el año que se está procesando
  if ("PERIODO" %in% names(df)) {
    df <- df |> filter(is.na(PERIODO) | PERIODO == anio)
  }
  
  # ---------------------------------------------------------------------------
  # 3.3 Validaciones mínimas
  # ---------------------------------------------------------------------------
  
  if (!"IDNOREMP" %in% names(df)) {
    message("[AVISO] No se encontró IDNOREMP en el año ", anio)
  } else {
    df$IDNOREMP <- as.numeric(df$IDNOREMP)
  }
  
  if ("IDDEPTO" %in% names(df)) {
    df$IDDEPTO <- as.numeric(df$IDDEPTO)
  }
  
  # ---------------------------------------------------------------------------
  # 3.4 Ordenar columnas
  # ---------------------------------------------------------------------------
  
  ids <- c(
    "ANIO",
    "PERIODO",
    "IDNOREMP",
    "IDDEPTO",
    "IDOJ",
    "CORRELA",
    "CORRELA_16",
    "CORRELA_9",
    "CORRE_9",
    "PRIORIDAD",
    "FACEXP"
  )
  
  ids <- ids[ids %in% names(df)]
  otras_vars <- setdiff(names(df), ids)
  
  df <- df |>
    select(all_of(ids), all_of(otras_vars))
  
  message(
    "OK: ", anio,
    " — archivo: ", basename(archivo),
    " — ", nrow(df), " observaciones, ",
    ncol(df), " variables"
  )
  
  return(df)
}

# ------------------------------------------------------------------------------
# 4. Cargar y unir todas las bases
# ------------------------------------------------------------------------------

lista_anual <- map(ANIOS, cargar_eac)

lista_anual <- compact(lista_anual)

panel_eac <- bind_rows(lista_anual)

# Guardar panel
saveRDS(
  panel_eac,
  file.path(RUTA_OUT, "EAC_panel.rds")
)
