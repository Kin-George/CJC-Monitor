# Primas salariales por tamaño de empresa y formalidad, excluyendo a quienes declaran un segundo empleo (P7040 == 1).
#
# Repite, SIN CAMBIAR NADA de la especificación del paper (Code/Paper_TamanoEmpresa/03_Model.R), estas dos regresiones:
#   - m_full:           log(ingreso hora real) ~ tamaño + mujer + edad + edad^2 + educación + formal | sector x año     (columna 4)
#   - m_formality_size: log(ingreso hora real) ~ tamaño * formal + mujer + edad + edad^2 + educación | sector x año
# con las mismas ponderaciones (fex), el mismo error estándar (agrupado por sector) y la misma medida de ingreso.
# Cuatro muestras: (1) completa, (2) completa sin segundo empleo, (3) asalariados, (4) asalariados sin segundo empleo.
# No modifica el paper ni las estimaciones publicadas: todo se guarda en Robustez_SegundoEmpleo/Outputs/tables/.
#
# Asalariados: obrero o empleado de empresa particular, obrero o empleado del gobierno, empleado doméstico y jornalero o peón.
# Segundo empleo: P7040 == 1. Quienes no responden P7040 (NA) se conservan y se cuentan aparte.
#
# Entradas: Robustez_SegundoEmpleo/Datos/paper_base_subconjunto.rds, correspondencia_persona_id.rds, segundo_empleo_llaves_2008_2025.dta
# Uso     : Rscript Robustez_SegundoEmpleo/Code/03_regresiones_sin_segundo_empleo.R   (desde la raíz del repositorio)

source(if (file.exists("Code/_paths.R")) "Code/_paths.R" else "_paths.R")
setwd(PROJECT_ROOT)
suppressMessages({library(haven); library(dplyr); library(tidyr); library(fixest)})

dir_dat <- project_path("Robustez_SegundoEmpleo", "Datos")
dir_out <- project_path("Robustez_SegundoEmpleo", "Outputs", "tables")
dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)

pb <- readRDS(file.path(dir_dat, "paper_base_subconjunto.rds"))
map <- readRDS(file.path(dir_dat, "correspondencia_persona_id.rds"))
kk <- read_dta(file.path(dir_dat, "segundo_empleo_llaves_2008_2025.dta"), col_select = c("anio", "fila_ocupados_anio", "segundo_empleo"))

geih <- pb %>%
  left_join(map, by = c("persona_id", "anio")) %>%
  left_join(kk, by = c("anio", "fila_ocupados_anio")) %>%
  mutate(
    anio = as.factor(anio), edad = as.numeric(edad), edad2 = edad^2,
    sector = as.factor(sector), tamano_empresa = as.factor(tamano_empresa),
    educacion = as.factor(educacion)
  )
stopifnot(sum(is.na(geih$fila_ocupados_anio)) == 0)

# Misma muestra común que 03_Model.R
geih_model <- geih %>%
  filter(!is.na(log_ingreso_hora_real), !is.na(tamano_empresa), !is.na(mujer), !is.na(edad), !is.na(edad2),
         !is.na(educacion), !is.na(formal), !is.na(sector), !is.na(anio), !is.na(fex), fex > 0) %>%
  mutate(
    anio_num = as.integer(as.character(anio)),
    asalariado = posicion_ocupacional %in% c("Obrero o empleado de empresa particular", "Obrero o empleado del gobierno",
                                             "Empleado doméstico", "Jornalero o peón")
  )
education_ref <- grep("secundaria", levels(geih_model$educacion), ignore.case = TRUE, value = TRUE)[1]
size_levels <- c("2-3", "4-5", "6-10", "11-19", "20-30", "31-50", "51-100", "101+")

muestras <- list(
  "1. Completa" = geih_model,
  "2. Completa sin segundo empleo" = geih_model %>% filter(is.na(segundo_empleo) | segundo_empleo == 0),
  "3. Asalariados" = geih_model %>% filter(asalariado),
  "4. Asalariados sin segundo empleo" = geih_model %>% filter(asalariado, is.na(segundo_empleo) | segundo_empleo == 0)
)
conteo <- bind_rows(lapply(names(muestras), function(m) {
  d <- muestras[[m]]
  data.frame(muestra = m, observaciones = nrow(d), poblacion_expandida_millones_anuales = round(sum(d$fex) / 1e6, 2),
             con_segundo_empleo = sum(d$segundo_empleo == 1, na.rm = TRUE), sin_dato_P7040 = sum(is.na(d$segundo_empleo)))
}))
print(conteo)
write.csv(conteo, file.path(dir_out, "regresiones_muestras.csv"), row.names = FALSE)

ajustar <- function(d, interaccion) {
  f <- if (!interaccion) {
    log_ingreso_hora_real ~ i(tamano_empresa, ref = "Solo") + mujer + edad + edad2 + i(educacion, ref = education_ref) + formal | sector^anio
  } else {
    log_ingreso_hora_real ~ i(tamano_empresa, ref = "Solo") * formal + mujer + edad + edad2 + i(educacion, ref = education_ref) | sector^anio
  }
  feols(f, weights = ~fex, cluster = ~sector, data = d)
}

filas <- list()
for (m in names(muestras)) {
  d <- muestras[[m]]
  # Especificación preferida (columna 4): una prima por tamaño
  b <- ajustar(d, FALSE); co <- coef(b); se <- sqrt(diag(vcov(b)))
  for (s in size_levels) {
    t <- paste0("tamano_empresa::", s)
    if (!t %in% names(co)) next
    filas[[length(filas) + 1]] <- data.frame(muestra = m, modelo = "Columna 4 (tamaño + formal)", grupo = "Todos", tamano = s, estimador = co[[t]], ee = se[[t]], n = nobs(b))
  }
  # Tamaño x formalidad: prima de informales y de formales
  b2 <- ajustar(d, TRUE); co2 <- coef(b2); V <- vcov(b2)
  for (s in size_levels) {
    t1 <- paste0("tamano_empresa::", s)
    t2 <- paste0("formal:tamano_empresa::", s)
    if (t1 %in% names(co2)) {
      filas[[length(filas) + 1]] <- data.frame(muestra = m, modelo = "Tamaño x formalidad", grupo = "Informales", tamano = s, estimador = co2[[t1]], ee = sqrt(V[t1, t1]), n = nobs(b2))
    }
    if (all(c(t1, t2) %in% names(co2))) {
      filas[[length(filas) + 1]] <- data.frame(muestra = m, modelo = "Tamaño x formalidad", grupo = "Formales", tamano = s,
                                               estimador = co2[[t1]] + co2[[t2]], ee = sqrt(V[t1, t1] + V[t2, t2] + 2 * V[t1, t2]), n = nobs(b2))
    }
  }
  # Brecha formal-informal por tamaño de la misma regresión (coeficiente de formal + interacción)
  tf <- "formal"
  filas[[length(filas) + 1]] <- data.frame(muestra = m, modelo = "Tamaño x formalidad", grupo = "Brecha formal-informal", tamano = "Solo",
                                           estimador = co2[[tf]], ee = sqrt(V[tf, tf]), n = nobs(b2))
  for (s in size_levels) {
    t2 <- paste0("formal:tamano_empresa::", s)
    if (t2 %in% names(co2)) {
      filas[[length(filas) + 1]] <- data.frame(muestra = m, modelo = "Tamaño x formalidad", grupo = "Brecha formal-informal", tamano = s,
                                               estimador = co2[[tf]] + co2[[t2]], ee = sqrt(V[tf, tf] + V[t2, t2] + 2 * V[tf, t2]), n = nobs(b2))
    }
  }
  cat("listo:", m, "\n")
}
res <- bind_rows(filas) %>%
  mutate(prima_pct = 100 * (exp(estimador) - 1),
         ic95_bajo_pct = 100 * (exp(estimador - 1.96 * ee) - 1), ic95_alto_pct = 100 * (exp(estimador + 1.96 * ee) - 1))
write.csv(res, file.path(dir_out, "regresiones_primas_por_muestra.csv"), row.names = FALSE)

# Cambio frente a la muestra de referencia correspondiente (completa -> sin segundo empleo; asalariados -> sin segundo empleo)
par <- c("2. Completa sin segundo empleo" = "1. Completa", "4. Asalariados sin segundo empleo" = "3. Asalariados")
cmp <- bind_rows(lapply(names(par), function(m) {
  a <- res %>% filter(muestra == par[[m]]) %>% select(modelo, grupo, tamano, prima_base = prima_pct, ee_base = ee)
  b <- res %>% filter(muestra == m) %>% select(modelo, grupo, tamano, prima_sin = prima_pct, ee_sin = ee)
  inner_join(a, b, by = c("modelo", "grupo", "tamano")) %>% mutate(comparacion = paste(par[[m]], "->", m))
})) %>% mutate(cambio_pp = prima_sin - prima_base)
write.csv(cmp, file.path(dir_out, "regresiones_cambio_prima.csv"), row.names = FALSE)

cat("\nCambio de la prima (pp) al excluir segundo empleo, columna 4:\n")
print(cmp %>% filter(modelo == "Columna 4 (tamaño + formal)") %>% select(comparacion, tamano, prima_base, prima_sin, cambio_pp) %>% mutate(across(where(is.numeric), ~ round(.x, 2))))
cat("\nMayor cambio absoluto (pp) por comparación y modelo:\n")
print(cmp %>% group_by(comparacion, modelo, grupo) %>% summarise(max_abs_cambio_pp = round(max(abs(cambio_pp)), 2), .groups = "drop"))
