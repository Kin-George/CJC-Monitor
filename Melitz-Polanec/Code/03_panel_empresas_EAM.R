# Panel de EMPRESAS de la EAM (la unidad del artículo de Melitz y Polanec) para la descomposición de la productividad laboral.
#
# La EAM observa establecimientos (NORDEST). El artículo trabaja con empresas, así que se agregan los establecimientos de una misma
# empresa (NORDEMP) en cada año: valor agregado, producción, consumo intermedio, personal, activos fijos e inversión se suman.
# NORDEMP es estable (mediana de 11 años por empresa; solo 2% de los establecimientos cambia de id de empresa).
#
# Muestra, igual que en el artículo ("mantener las mismas empresas en todas las descomposiciones"): valor agregado > 0, personal > 0 y
# capital (activos fijos) > 0. El capital no entra en la productividad laboral, pero se exige para que después la PTF use la misma muestra.
# Valor agregado real = valor agregado nominal / (IPP industria / IPP industria 2015), a pesos de 2015.
# Productividad laboral de la empresa i en el año t: phi_it = log(valor agregado real / personal).
#
# Entradas: Datos/Processed/EAC_EAM/EAM_panel_completo.rds, Melitz-Polanec/Datos/deflactor_IPP_industria_anual.csv
# Salida  : Melitz-Polanec/Datos/panel_empresas_EAM.rds  y  Outputs/tables/panel_empresas_resumen_anual.csv
# Uso     : Rscript Melitz-Polanec/Code/03_panel_empresas_EAM.R   (desde la raíz del repositorio)

source(if (file.exists("Code/_paths.R")) "Code/_paths.R" else "_paths.R")
setwd(PROJECT_ROOT)
suppressMessages(library(dplyr))
dir_dat <- project_path("Melitz-Polanec", "Datos"); dir_out <- project_path("Melitz-Polanec", "Outputs", "tables")
dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)

x <- readRDS(project_path("Datos", "Processed", "EAC_EAM", "EAM_panel_completo.rds")) %>%
  transmute(anio = as.integer(ANIO), est = as.character(NORDEST), emp = as.character(NORDEMP), ciiu = as.character(CIIU), ciiu_rev = CIIU_REV,
            va = as.numeric(VALAGRI), y = as.numeric(PRODBR2), ci = as.numeric(CONSIN2), l = as.numeric(PERTOTAL), k = as.numeric(ACTIVFI), inv = as.numeric(INVEBRTA))
# una fila por (establecimiento, año): hay una duplicada en todo el panel
x <- x %>% filter(anio >= 2000) %>% group_by(anio, est) %>% slice(1) %>% ungroup()   # el IPP completo empieza en 2000

firmas <- x %>% group_by(anio, emp) %>% summarise(
  va = sum(va, na.rm = TRUE), y = sum(y, na.rm = TRUE), ci = sum(ci, na.rm = TRUE), l = sum(l, na.rm = TRUE),
  k = sum(k, na.rm = TRUE), inv = sum(inv, na.rm = TRUE), n_establecimientos = n_distinct(est),
  ciiu_principal = ciiu[which.max(replace(va, is.na(va), -Inf))], ciiu_rev = ciiu_rev[which.max(replace(va, is.na(va), -Inf))], .groups = "drop")

defl <- read.csv(file.path(dir_dat, "deflactor_IPP_industria_anual.csv")) %>% select(anio, deflactor_2015)
p <- firmas %>% left_join(defl, by = "anio") %>%
  mutate(muestra = va > 0 & l > 0 & k > 0,
         va_real = va / deflactor_2015,
         phi = ifelse(muestra, log(va_real / l), NA_real_))
stopifnot(!any(is.na(p$deflactor_2015)))
saveRDS(p, file.path(dir_dat, "panel_empresas_EAM.rds"))

res <- p %>% group_by(anio) %>% summarise(
  empresas = n(), empresas_muestra = sum(muestra), pct_muestra = round(100 * mean(muestra), 1),
  pct_multiestablecimiento = round(100 * mean(n_establecimientos > 1), 1),
  empleo_muestra = sum(l[muestra]), va_real_millones_2015 = round(sum(va_real[muestra]) / 1e6),
  phi_p1 = quantile(phi, .01, na.rm = TRUE), phi_mediana = median(phi, na.rm = TRUE), phi_p99 = quantile(phi, .99, na.rm = TRUE), .groups = "drop") %>%
  mutate(across(starts_with("phi"), ~ round(.x, 2)))
write.csv(res, file.path(dir_out, "panel_empresas_resumen_anual.csv"), row.names = FALSE)
print(res, n = 40)
