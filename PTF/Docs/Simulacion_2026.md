# Simulación informada: productividad de 2026 y salario mínimo de 2027

Fecha: 2026-10-24 (tercera versión, con la GEIH 2026). Código: [`16_Simulacion_productividad_salario_2026.py`](../Code/16_Simulacion_productividad_salario_2026.py) (simulación), [`17_Figuras_series_simulacion_2026.py`](../Code/17_Figuras_series_simulacion_2026.py) (figuras), [`18_Trabajo_GEIH_2026.py`](../Code/18_Trabajo_GEIH_2026.py) (trabajo medido con la GEIH), [`06_ExtraerHorasEfectivas.do`](../Code/06_ExtraerHorasEfectivas.do) y [`06c_OcupadosHorasMensuales.do`](../Code/06c_OcupadosHorasMensuales.do) (extracción), [`GEIH_2026_total.do`](../../Code/GEIH_Bases/GEIH_2026_total.do) (consolidación de la base). Tablas: `../Outputs/tables/sim2026_*.csv` y `geih2026_*.csv`. Figuras: `../Outputs/Figures/fig_serie_ptf_2026.png`, `fig_serie_productividad_hora_2026.png`, `fig_sectores_ptf_2026.png`, `fig_serie_salario_minimo_2027.png`, `fig_serie_salario_minimo_pesos_2027.png`, `fig_sim2026_productividad_salario.png`.

## Base de datos nueva
`C:/Users/jorge/Documents/Databases/GEIH/GEIH_2026_total.dta`: 533.851 personas (enero–agosto de 2026), 341 variables. Misma estructura que `GEIH_2025_total.dta` (características generales + no ocupados + fuerza de trabajo + ocupados + migración, unidos por `DIRECTORIO SECUENCIA_P ORDEN HOGAR`) y las variables de `formalidadDANERobusto.do` (`ANIOS`, `OFICIO_C8_2D`, `FORMAL`, `SALUD`, `PENSION`, `EI`). Tiene 6 variables que el DANE agregó en 2026 (`P1879S1`, `P3363S1`, `P3560`–`P3563`) y no tiene `P7440`, que en 2026 viene como `P7440S1` y `P7440S2`. Verificación: los ocupados ponderados por mes coinciden con los boletines del DANE (enero 23,227 M; abril 24,277 M; junio 24,459 M; agosto 23,685 M) y la informalidad de agosto da 54,9%, igual que el boletín.

## Estructura de la simulación
La PTF agregada se obtiene con dos fuentes que se combinan según su precisión; después alimenta el salario mínimo.
1. **Desde los sectores (a priori).** PTF agregada = suma de las PTF de 9 ramas ponderadas por producción bruta / valor agregado (Domar); verificación 2015–2024: diferencia máxima 0,12 pp. A priori de cada rama: promedio 2011–2024 con covarianza entre ramas.
2. **Identidad agregada (medición).** `PTF = g_VA − aporte del trabajo − aporte del capital`. Valor agregado: ISE (enero–agosto) más septiembre simulado. **Trabajo: medido con la GEIH 2026.** Capital: inventario perpetuo con la FBCF del anexo de gasto del II trimestre de 2026.
3. **Combinación gaussiana** de las dos, que reparte la PTF entre ramas.
4. **Salario mínimo.** Piso = inflación esperada (6,87%, consenso de analistas). Reglas: técnica (inflación + max(productividad, 0) + U(0, 1); usa la PTF en 75% de los casos y la productividad por hora en 25%), histórica (prima 2014–2021) y activista (prima 2022–2026); pesos 45/40/15.

## Qué midió la GEIH 2026 (enero–agosto frente a enero–agosto de 2025)
| Variable | Valor |
|---|---|
| Ocupados | +2,31% (mensual: 1,4; 2,7; 2,7; 3,0; 4,1; 3,0; 2,4; −0,5) |
| Horas efectivas (P6850, empleo principal) | −0,98% |
| Horas por ocupado | −3,3% ± 0,2 (en todos los niveles educativos; coherente con la jornada de 44 h y de 42 h desde julio) |
| Aporte del trabajo a la PTF | +1,16 pp: horas +0,01 y composición +1,15 ± 0,19 |

**La composición de +1,15 pp es probablemente una ruptura de la variable de educación** (ver [Pruebas_confiabilidad_2026.md](Pruebas_confiabilidad_2026.md)): la proporción de adultos con educación superior sube 1,8 pp en todos los grupos de edad, incluso en los de 55 años o más, cuando entre 2023 y 2025 estuvo estable. Por eso la simulación usa dos escenarios (75% el salto, 25% una composición típica; ver las pruebas 2 y 4): el DANE publica esa composición (+1,15) o una típica (0,25 ± 0,30).

## Resultados (medianas; intervalo 5–95%)
| Variable | Mediana | 5–95% |
|---|---|---|
| Valor agregado (ene–sep) | 2,1 | 1,8–2,4 |
| Ocupados | 2,0 | 1,8–2,2 |
| Horas trabajadas (corregidas por el sesgo frente al DANE) | −1,7 | −3,5 a 0,1 |
| **Productividad por hora** | **3,8** | 2,0–5,6 |
| Productividad por persona | 0,06 | −0,3 a 0,45 |
| **PTF** | **0,44** | −0,53 a 1,45 |
| Inflación de cierre | 6,87 | 6,5–7,2 |
| **Aumento salario mínimo 2027 (base)** | **8,6** (media 9,4) | 7,1–12,4 |
| Salario mínimo 2027 (pesos) | **$1.902.183** | $1.874.694–$1.968.367 |

PTF según el escenario de la composición: 0,33 si el DANE incluye el salto de la educación y 0,81 si no. A priori sectorial 0,29 ± 0,84.

**El salario depende de dos supuestos míos:** la medida de productividad de la regla técnica (`sim2026_sensibilidad_regla_tecnica.csv`: con 0% de productividad por hora, mediana 8,4%; 25% (base), 8,6%; 100%, 10,0%) y, en mucho menor grado, el escenario de la composición (`sim2026_sensibilidad_ruptura_educacion.csv`: mediana de 8,6% a 8,8%).

## Mejoras de las versiones anteriores
Capital con inventario perpetuo (error 0,33 pp frente a 0,49 del AR(1), 2016–2025; `sim2026_capital_retrospectiva.csv`); PIB por rama del II trimestre observado; inflación con el consenso de analistas (6,87%; rango 6,74–6,93); contraste con proyecciones de otros analistas (`sim2026_consenso_analistas.csv`: 37% de probabilidad de quedar por debajo de 8,0% y 50% por debajo de 8,5%); bloque de datos que se actualizan al inicio del script 16.

## Cómo actualizar
En el bloque `DATOS QUE SE ACTUALIZAN` del script 16: GEIH de septiembre (fines de octubre; se agrega a la base y se corren 06, 06c y 18 con `MAXMES = 9`), IPC de octubre, ISE de septiembre y PIB del III trimestre (mediados de noviembre), y el boletín del DANE para la concertación (fines de noviembre), con el que se compara. Fechas por confirmar en el calendario del DANE.

## Límites
1. **La composición de +1,15 pp parece una ruptura de la educación, no un cambio real.** Se manejó como dos escenarios (75% el salto, 25% una composición típica), un juicio mío apoyado en la prueba 4. Hasta ver la cifra del DANE no se puede saber cuál rige.
2. Las horas son del empleo principal (P6850), igual que la réplica; el segundo empleo no entra.
3. Septiembre (valor agregado, ocupados, horas, aporte del trabajo, inflación del cuarto trimestre y FBCF del III trimestre) es simulado.
4. Los valores por rama de 2026 son estimaciones condicionadas (la PTF por rama del DANE llega a 2024), con intervalos de hasta 5 pp.
5. La retrospectiva de la identidad (error 0,10 pp) usa solo 2023–2025.
6. Los pesos de las reglas del salario (45/40/15) y el 25% de la productividad por hora son juicios míos.
7. El boletín del DANE puede diferir, como en 2024 (1,73% preliminar frente a 1,21% anual).
