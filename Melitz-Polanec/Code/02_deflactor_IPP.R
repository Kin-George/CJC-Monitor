# Deflactor de la industria: IPP "Producción Nacional - Industria" del DANE, promedio anual.
#
# Fuente : Datos/Raw/IPP_DANE/anex-IPP-historicos-sep2026.xlsx  (DANE, serie mensual junio 1999 - septiembre 2026, base diciembre 2014 = 100)
# Por qué un solo índice: el DANE publica el IPP histórico solo para agregados (producción nacional, agricultura, minería, industria),
#   no por división CIIU. Con participaciones de empleo, un deflactor común solo desplaza el componente "dentro de las empresas" y deja
#   intacta la reasignación, la entrada y la salida (ver Docs/03_Etapa1-3_Productividad_laboral.md). Un deflactor por rama
#   (cuentas nacionales, 2014-2024) queda para la etapa de robustez.
# Salida : Melitz-Polanec/Datos/deflactor_IPP_industria_anual.csv  (anio, ipp_industria, deflactor_2015 = ipp / ipp_2015, meses)
# Uso    : Rscript Melitz-Polanec/Code/02_deflactor_IPP.R   (desde la raíz del repositorio)

source(if (file.exists("Code/_paths.R")) "Code/_paths.R" else "_paths.R")
setwd(PROJECT_ROOT)
suppressMessages({library(readxl); library(dplyr)})

d <- read_excel(project_path("Datos", "Raw", "IPP_DANE", "anex-IPP-historicos-sep2026.xlsx"), col_names = FALSE, .name_repair = "minimal")
# Fila 5 del archivo (índice base 1: 6) en adelante son los datos; columna 1 = año (solo en el primer mes), 2 = mes, 6 = Producción Nacional, Industria
x <- data.frame(anio = suppressWarnings(as.numeric(d[[1]])), mes = d[[2]], industria = suppressWarnings(as.numeric(d[[6]])))[-(1:5), ]
x$anio <- zoo::na.locf(x$anio, na.rm = FALSE)
x <- x %>% filter(!is.na(industria), !is.na(anio))
anual <- x %>% group_by(anio) %>% summarise(ipp_industria = mean(industria), meses = n(), .groups = "drop")
base_2015 <- anual$ipp_industria[anual$anio == 2015]
anual$deflactor_2015 <- anual$ipp_industria / base_2015
dir.create(project_path("Melitz-Polanec", "Datos"), showWarnings = FALSE, recursive = TRUE)
write.csv(anual, project_path("Melitz-Polanec", "Datos", "deflactor_IPP_industria_anual.csv"), row.names = FALSE)
print(anual %>% filter(anio >= 2004, anio <= 2025) %>% mutate(across(where(is.numeric), ~ round(.x, 3))), n = 30)
stopifnot(all(anual$meses[anual$anio >= 2000 & anual$anio <= 2025] == 12))
