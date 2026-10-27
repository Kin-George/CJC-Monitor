# Descomposición dinámica de Olley-Pakes (Melitz y Polanec, 2015) de la productividad laboral de la industria colombiana.
#
# Unidad: empresa (NORDEMP de la EAM, establecimientos agregados). Productividad: phi = log(valor agregado real / personal).
# Participación: s_it = personal de la empresa / personal total de la muestra en el año (participaciones de empleo).
# Ventanas: 2010-2015, 2015-2019, 2019-2024 (sin 2020 en la EAM) y 2005-2010, que se reporta aparte porque ahí hay un salto de cobertura de la encuesta.
#
# DOPD (ecuación 6 del artículo):  dPhi = d(media sin ponderar de sobrevivientes) + d(covarianza participación-productividad de sobrevivientes)
#                                         + s_E2 (Phi_E2 - Phi_S2) + s_X1 (Phi_S1 - Phi_X1)
# Se calculan también la descomposición de Griliches-Regev (GR) y la de Foster-Haltiwanger-Krizan (FHK), con las fórmulas del artículo (ecuaciones 3 y 4),
# para ver cuánto sobreestiman la entrada. Todo en puntos logarítmicos x 100 (aprox. puntos porcentuales).
#
# Definiciones de grupo (una empresa cuenta si cumple la muestra: valor agregado, personal y capital positivos):
#   sobreviviente = en la muestra en t1 y en t2; entrante = en la muestra en t2 y no en t1; saliente = en la muestra en t1 y no en t2.
#
# Entrada: Melitz-Polanec/Datos/panel_empresas_EAM.rds
# Salidas: Outputs/tables/dopd_laboral_ventanas.csv (componentes), dopd_laboral_comparacion.csv (DOPD vs GR vs FHK), dopd_laboral_grupos.csv (tamaños)
# Uso    : Rscript Melitz-Polanec/Code/04_DOPD_productividad_laboral.R   (desde la raíz del repositorio)

source(if (file.exists("Code/_paths.R")) "Code/_paths.R" else "_paths.R")
setwd(PROJECT_ROOT)
suppressMessages(library(dplyr))
dir_out <- project_path("Melitz-Polanec", "Outputs", "tables")

# ---- funciones ---------------------------------------------------------------------------------------------------
# d1, d2: data.frame con id, phi (log productividad) y w (peso: personal), una fila por empresa de la muestra en cada año
descomponer <- function(d1, d2) {
  d1$s <- d1$w / sum(d1$w); d2$s <- d2$w / sum(d2$w)
  S <- intersect(d1$id, d2$id)
  a1 <- d1[d1$id %in% S, ]; a2 <- d2[d2$id %in% S, ]
  a1 <- a1[order(a1$id), ]; a2 <- a2[order(a2$id), ]
  stopifnot(identical(a1$id, a2$id))
  e2 <- d2[!d2$id %in% S, ]; x1 <- d1[!d1$id %in% S, ]
  Phi1 <- sum(d1$s * d1$phi); Phi2 <- sum(d2$s * d2$phi)
  # estadísticos de grupo (participaciones normalizadas dentro del grupo)
  grp <- function(d) { sG <- sum(d$s); w <- d$s / sG; list(sG = sG, Phi = sum(w * d$phi), media = mean(d$phi), cov = sum(w * d$phi) - mean(d$phi), n = nrow(d)) }
  S1 <- grp(a1); S2 <- grp(a2); E2 <- grp(e2); X1 <- grp(x1)
  dopd <- c(
    dentro_media_sobrevivientes = S2$media - S1$media,
    reasignacion_covarianza_sobrevivientes = S2$cov - S1$cov,
    entrada = E2$sG * (E2$Phi - S2$Phi),
    salida = X1$sG * (S1$Phi - X1$Phi))
  # Griliches-Regev (ecuación 3), con participaciones totales y referencia = promedio de los dos años
  Pm <- (Phi1 + Phi2) / 2; sb <- (a1$s + a2$s) / 2; pb <- (a1$phi + a2$phi) / 2
  gr <- c(dentro = sum(sb * (a2$phi - a1$phi)), entre = sum((a2$s - a1$s) * (pb - Pm)),
          entrada = sum(e2$s * (e2$phi - Pm)), salida = sum(x1$s * (Pm - x1$phi)))
  # Foster-Haltiwanger-Krizan (ecuación 4), referencia = productividad agregada del año inicial
  fhk <- c(dentro = sum(a1$s * (a2$phi - a1$phi)), entre = sum((a2$s - a1$s) * (a1$phi - Phi1)), cruzado = sum((a2$s - a1$s) * (a2$phi - a1$phi)),
           entrada = sum(e2$s * (e2$phi - Phi1)), salida = sum(x1$s * (Phi1 - x1$phi)))
  tot <- Phi2 - Phi1
  stopifnot(abs(sum(dopd) - tot) < 1e-9, abs(sum(gr) - tot) < 1e-9, abs(sum(fhk) - tot) < 1e-9)   # las tres suman el cambio agregado
  list(Phi1 = Phi1, Phi2 = Phi2, total = tot, dopd = dopd, gr = gr, fhk = fhk,
       grupos = c(n1 = nrow(d1), n2 = nrow(d2), sobrevivientes = S1$n, entrantes = E2$n, salientes = X1$n,
                  s_entrantes_t2 = E2$sG, s_salientes_t1 = X1$sG, Phi_S1 = S1$Phi, Phi_S2 = S2$Phi, Phi_E2 = E2$Phi, Phi_X1 = X1$Phi))
}

# ---- prueba con datos sintéticos: sin entrada ni salida y todos crecen igual, la entrada y la salida deben dar 0 ----
set.seed(1); n <- 500
t1 <- data.frame(id = 1:n, phi = rnorm(n), w = runif(n, 1, 50)); t2 <- transform(t1, phi = phi + 0.10)
r0 <- descomponer(t1, t2)
stopifnot(abs(r0$dopd[["entrada"]]) < 1e-12, abs(r0$dopd[["salida"]]) < 1e-12, abs(r0$total - 0.10) < 1e-12)
# ejemplo del artículo: empresas representativas con crecimiento g y entrada/salida de una parte: la DOPD asigna 0 a entrada y salida
g <- 0.10; ids <- 1:300; ph1 <- rep(1, 300); w1 <- rep(1, 300)
d1 <- data.frame(id = ids[1:270], phi = ph1[1:270], w = w1[1:270])            # salen las empresas 271-300
d2 <- data.frame(id = c(1:270, 301:330), phi = 1 + g, w = 1)                    # entran 30 con el mismo nivel de productividad
r1 <- descomponer(d1, d2)
cat("Prueba del ejemplo del artículo (productividad uniforme que crece 10%):\n")
cat("  DOPD entrada =", round(r1$dopd[["entrada"]], 10), "| salida =", round(r1$dopd[["salida"]], 10), "| GR entrada =", round(r1$gr[["entrada"]], 4), "| FHK entrada =", round(r1$fhk[["entrada"]], 4), "\n")
stopifnot(abs(r1$dopd[["entrada"]]) < 1e-12, abs(r1$dopd[["salida"]]) < 1e-12, r1$gr[["entrada"]] > 0)

# ---- datos reales -------------------------------------------------------------------------------------------------
p <- readRDS(project_path("Melitz-Polanec", "Datos", "panel_empresas_EAM.rds")) %>% filter(muestra)
ventanas <- list(c(2010, 2015), c(2015, 2019), c(2019, 2024), c(2005, 2010))
filas <- list(); comp <- list(); grup <- list()
for (v in ventanas) {
  d1 <- p %>% filter(anio == v[1]) %>% transmute(id = emp, phi, w = l); d2 <- p %>% filter(anio == v[2]) %>% transmute(id = emp, phi, w = l)
  r <- descomponer(d1, d2); lab <- paste0(v[1], "-", v[2])
  tot <- r$total
  filas[[lab]] <- data.frame(ventana = lab, Phi_inicial = 100 * r$Phi1, Phi_final = 100 * r$Phi2, cambio_total = 100 * tot,
                             componente = names(r$dopd), valor_pp = 100 * r$dopd, pct_del_cambio = 100 * r$dopd / tot)
  comp[[lab]] <- data.frame(ventana = lab, cambio_total = 100 * tot,
                            DOPD_entrada = 100 * r$dopd[["entrada"]], GR_entrada = 100 * r$gr[["entrada"]], FHK_entrada = 100 * r$fhk[["entrada"]],
                            DOPD_salida = 100 * r$dopd[["salida"]], GR_salida = 100 * r$gr[["salida"]], FHK_salida = 100 * r$fhk[["salida"]],
                            DOPD_sobrevivientes = 100 * (r$dopd[["dentro_media_sobrevivientes"]] + r$dopd[["reasignacion_covarianza_sobrevivientes"]]),
                            GR_sobrevivientes = 100 * (r$gr[["dentro"]] + r$gr[["entre"]]), FHK_sobrevivientes = 100 * (r$fhk[["dentro"]] + r$fhk[["entre"]] + r$fhk[["cruzado"]]),
                            DOPD_reasignacion = 100 * r$dopd[["reasignacion_covarianza_sobrevivientes"]], GR_entre = 100 * r$gr[["entre"]], FHK_entre = 100 * (r$fhk[["entre"]] + r$fhk[["cruzado"]]))
  grup[[lab]] <- data.frame(ventana = lab, t(round(r$grupos, 4)))
}
res <- bind_rows(filas); cmp <- bind_rows(comp); gru <- bind_rows(grup)
write.csv(res, file.path(dir_out, "dopd_laboral_ventanas.csv"), row.names = FALSE)
write.csv(cmp, file.path(dir_out, "dopd_laboral_comparacion.csv"), row.names = FALSE)
write.csv(gru, file.path(dir_out, "dopd_laboral_grupos.csv"), row.names = FALSE)
options(width = 200)
cat("\n== Grupos\n"); print(gru, row.names = FALSE)
cat("\n== DOPD (puntos log x 100)\n"); print(res %>% mutate(across(where(is.numeric), ~ round(.x, 2))), row.names = FALSE)
cat("\n== DOPD frente a GR y FHK\n"); print(cmp %>% mutate(across(where(is.numeric), ~ round(.x, 2))), row.names = FALSE)
