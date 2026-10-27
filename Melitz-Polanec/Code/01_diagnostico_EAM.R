# Diagnóstico de factibilidad: ¿el panel de la EAM permite la descomposición dinámica de Olley-Pakes (Melitz y Polanec, 2015)?
#
# Responde con datos: cobertura de las variables necesarias, identificación de sobrevivientes, entrantes y salientes en
# ventanas de varios años, y qué tan estable es el identificador del establecimiento.
#
# Entrada : Datos/Processed/EAC_EAM/EAM_panel_completo.rds   (lo construye Code/EDIT_EAM/EAM_01_DatabaseConstruction.R)
# Salidas : Melitz-Polanec/Outputs/tables/diagnostico_EAM_cobertura_anual.csv
#           Melitz-Polanec/Outputs/tables/diagnostico_EAM_ventanas.csv
#           Melitz-Polanec/Outputs/tables/diagnostico_EAM_resumen_panel.csv
# Uso     : Rscript Melitz-Polanec/Code/01_diagnostico_EAM.R   (desde la raíz del repositorio)

source(if (file.exists("Code/_paths.R")) "Code/_paths.R" else "_paths.R")
setwd(PROJECT_ROOT)
suppressMessages(library(dplyr))

out_dir <- project_path("Melitz-Polanec", "Outputs", "tables")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

x <- readRDS(project_path("Datos", "Processed", "EAC_EAM", "EAM_panel_completo.rds")) %>%
  transmute(
    anio = as.integer(ANIO), est = as.character(NORDEST), emp = as.character(NORDEMP), ciiu_rev = CIIU_REV,
    va = as.numeric(VALAGRI),      # valor agregado
    y = as.numeric(PRODBR2),       # producción bruta
    ci = as.numeric(CONSIN2),      # consumo intermedio
    l = as.numeric(PERTOTAL),      # personal ocupado total
    k = as.numeric(ACTIVFI),       # activos fijos (valor en libros, pesos corrientes)
    inv = as.numeric(INVEBRTA)     # inversión bruta
  )

# 1. Cobertura de las variables necesarias por año
cobertura <- x %>%
  group_by(anio) %>%
  summarise(
    establecimientos = n(),
    pct_va_positivo = round(100 * mean(va > 0, na.rm = TRUE), 1),
    pct_personal_positivo = round(100 * mean(l > 0, na.rm = TRUE), 1),
    pct_capital_positivo = round(100 * mean(k > 0, na.rm = TRUE), 1),
    pct_muestra_completa = round(100 * mean(va > 0 & l > 0 & k > 0, na.rm = TRUE), 1),
    personal_mediano = median(l, na.rm = TRUE),
    .groups = "drop"
  )
write.csv(cobertura, file.path(out_dir, "diagnostico_EAM_cobertura_anual.csv"), row.names = FALSE)

# 2. Sobrevivientes, entrantes y salientes en ventanas (muestra con VA, personal y capital positivos, como en Melitz y Polanec)
u <- x %>% filter(va > 0, l > 0, k > 0)
ventana <- function(a, b) {
  A <- unique(u$est[u$anio == a]); B <- unique(u$est[u$anio == b])
  sal <- setdiff(A, B); ent <- setdiff(B, A)
  # "falsas" salidas: el establecimiento reaparece después de t2; "falsas" entradas: ya había aparecido antes de t1
  vuelven <- unique(u$est[u$anio > b]); antes <- unique(u$est[u$anio < a])
  data.frame(t1 = a, t2 = b, n_t1 = length(A), n_t2 = length(B), sobrevivientes = length(intersect(A, B)),
             salientes = length(sal), entrantes = length(ent),
             pct_salientes_que_reaparecen = if (b < max(u$anio)) round(100 * mean(sal %in% vuelven), 1) else NA_real_,
             pct_entrantes_que_ya_existian = if (a > min(u$anio)) round(100 * mean(ent %in% antes), 1) else NA_real_)
}
ventanas <- bind_rows(ventana(2000, 2005), ventana(2005, 2010), ventana(2010, 2015), ventana(2015, 2019), ventana(2019, 2024),
                      ventana(2022, 2023), ventana(2023, 2024)) %>%
  mutate(pct_entrantes_en_t2 = round(100 * entrantes / n_t2, 1), pct_salientes_de_t1 = round(100 * salientes / n_t1, 1))
write.csv(ventanas, file.path(out_dir, "diagnostico_EAM_ventanas.csv"), row.names = FALSE)

# 3. Estabilidad del identificador y huecos en la serie de cada establecimiento
anios_por_est <- split(u$anio, u$est)
huecos <- vapply(anios_por_est, function(a) { a <- sort(unique(a)); any(diff(a) > 1) }, logical(1))
n_emp <- x %>% group_by(est) %>% summarise(n = n_distinct(emp), .groups = "drop")
resumen <- data.frame(
  indicador = c("filas del panel", "anios cubiertos", "establecimientos distintos (muestra completa)",
                "% con huecos (desaparecen y reaparecen)", "mediana de años observados por establecimiento",
                "% con más de un identificador de empresa", "filas duplicadas (establecimiento, año)"),
  valor = c(nrow(x), paste(range(x$anio), collapse = "-"), length(anios_por_est), round(100 * mean(huecos), 1),
            median(vapply(anios_por_est, function(a) length(unique(a)), numeric(1))),
            round(100 * mean(n_emp$n > 1), 1), sum(duplicated(x[c("est", "anio")])))
)
write.csv(resumen, file.path(out_dir, "diagnostico_EAM_resumen_panel.csv"), row.names = FALSE)
cat("Años presentes:", paste(sort(unique(x$anio)), collapse = " "), "\n")
print(resumen); print(ventanas)
