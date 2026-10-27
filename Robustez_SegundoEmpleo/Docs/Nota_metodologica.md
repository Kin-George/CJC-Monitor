# Segundo empleo y primas salariales por tamaño de empresa: nota metodológica

Fecha: 2026-10-21. No se modificó el paper ni las estimaciones publicadas; todo está en `Robustez_SegundoEmpleo/`.
Cada cifra de esta nota sale de los scripts de `Code/` y de las tablas de `Outputs/tables/`.

## 1. Qué se pidió y qué se hizo

1. **Código y definiciones de los cálculos previos** (años, meses, población, factores de expansión): sección 2.
2. **Archivo con las variables del segundo empleo y las llaves originales, con la correspondencia a `persona_id`:** sección 3.
3. **Comparación acotada:** repetir la especificación preferida excluyendo a quienes declaran segundo empleo, en la muestra completa y en la de asalariados: sección 4.
4. **Aclaración sobre `P7045` y sobre ingresos nulos frente a faltantes:** secciones 2 y 5.

## 2. Los cálculos previos y sus correcciones

**Qué se calculó antes.** Para 2019, 2022 y 2024 se leyeron los archivos anuales crudos `GEIH_<año>_total.dta` (los 12 meses de cada año),
se dejó la población de ocupados (`OCI == 1`) y se calcularon porcentajes **ponderados con el factor de expansión mensual** (`FEX_C` en 2019, `FEX_C18` en 2022 y 2024).
Dividir ese factor entre 12 no cambia ningún porcentaje. Los cálculos estaban en un script temporal que no se guardó en el repositorio;
los reemplazan `Code/04_descriptivas_segundo_empleo.R` (todos los años) y `Code/01_extraer_segundo_empleo.R`.

**Tres correcciones al cálculo previo:**

1. **`P7045` no son horas habituales.** La documentación del DANE para 2025 la define como las horas **trabajadas la semana pasada** en el segundo empleo
   (pregunta "¿Cuántas horas trabajó la semana pasada en ese segundo trabajo?"; 0 si estuvo ausente; 98 si trabajó y no sabe cuántas; 99 si no sabe si trabajó; rango válido 0 a 75).
   `P6800` son las horas **habituales** del empleo principal ("¿cuántas horas a la semana trabaja normalmente?"). Antes se dijo que ambas medían lo mismo: es incorrecto.
   La pareja coherente de `P7045` es `P6850` (horas del principal la semana pasada), no `P6800`.
   *Solo está verificado para 2025; falta comprobar que la definición sea la misma en 2008 a 2024.*
2. **Se incluían los códigos 98 y 99 como si fueran horas.** El filtro usado entonces (`P7045` entre 1 y 168) los dejaba pasar. Pesan muy poco
   (entre 0,0% y 0,5% de quienes declaran segundo empleo, según el año), pero ya se excluyen: se usa solo el rango 0 a 75.
3. **No se distinguía ingreso nulo de ingreso faltante.** Ahora sí (tabla de la sección 5).

**Cifras corregidas** (todos los ocupados, ponderado; tablas `descriptivas_*.csv`):

| Medida | 2008–2019 | 2021 | 2022–2025 |
|---|---|---|---|
| % de ocupados con segundo empleo (`P7040 = 1`) | 5,5% a 8,7% (máximo en 2012) | 2,4% | 3,8% a 4,1% |
| Aumento de las horas totales al sumar `P7045` a las horas habituales | 1,5% a 2,5% | 0,8% | 1,2% a 1,3% |

Hay una **ruptura en 2021** (de 6,2% en 2019 a 2,4% en 2021), coincidente con el cambio de diseño de la GEIH. Excluir a quienes declaran segundo empleo
afecta más a los años anteriores a 2021 que a los recientes.

## 3. Correspondencia con `persona_id`

`persona_id` es el número de fila (`gen long persona_id = _n`) de la base final (`CreacionPanelLimpio2008-2025.do`), no una llave original.
La base del paper tiene 5.353.440 filas y los archivos crudos tienen 6.128.610 ocupados (2008 a 2025, sin 2020): la base del paper **descarta** algunas filas.

**Cómo se reconstruyó** (`Code/02_correspondencia_persona_id.R`): la base del paper conserva el orden del archivo crudo, así que es una subsecuencia de los ocupados crudos.
Se recorre en orden y cada fila del paper se empareja con la primera fila cruda no usada que tiene el mismo factor de expansión, edad, departamento y sexo.

**Verificación.** En los 17 años, el 100% de las filas del paper encontró su fila cruda, sin que ninguna fila cruda se use dos veces.
Con variables que **no** se usaron para emparejar: las horas habituales (`P6800`) coinciden en 99,98% a 100,00% de las filas según el año y el ingreso por hora en prácticamente 100% (diferencias menores a 0,005%).
Las llaves originales (año, mes, `DIRECTORIO`, `SECUENCIA_P`, `ORDEN`) no se repiten en el archivo de entrega. Tabla: `Outputs/tables/correspondencia_persona_id_verificacion.csv`.

**Archivo de entrega:** `Datos/entrega_persona_id_segundo_empleo.dta` (y `.csv.gz`), 5.353.440 filas, con `persona_id`, año, mes, `directorio`, `secuencia_p`, `orden`, `hogar`,
`P7040`, `P7045`, `P7070`, `P6800`, `P6850`, `INGLABO`, el factor de expansión (`fex`, igual al de la base del paper) e indicadores derivados.
Pesa más de 1 GB; no se versiona en git.

## 4. Resultados: primas excluyendo el segundo empleo

**Especificación (sin cambios respecto de `03_Model.R`).** `log(ingreso por hora real) ~ tamaño + mujer + edad + edad² + educación + formal | sector × año`
(columna 4 del paper) y la versión con la interacción tamaño × formal. Pesos `fex`, errores agrupados por sector (17 sectores), misma medida de ingreso.
Código: `Code/03_regresiones_sin_segundo_empleo.R`. Se excluye a quien declara `P7040 = 1`; no hay observaciones sin dato de `P7040` en la muestra.

**Muestras:**

| Muestra | Observaciones | Con segundo empleo (excluidas) |
|---|---|---|
| 1. Completa | 5.268.514 | — |
| 2. Completa sin segundo empleo | 4.984.346 | 284.168 |
| 3. Asalariados | 2.643.630 | — |
| 4. Asalariados sin segundo empleo | 2.537.143 | 106.487 |

Asalariados: obrero o empleado de empresa particular, del gobierno, empleado doméstico y jornalero o peón.

**Advertencia sobre la línea base.** Con la base actual (`Paper-GEIH_base_modelo_personas_2008_2025.dta`, modificada el 30 de junio de 2026) la muestra de la columna 4 tiene
5.268.514 observaciones, y la tabla publicada indica 5.154.213 (diferencia de 114.301). Las primas replicadas difieren de las publicadas hasta 0,11 pp en la columna 4
y hasta 0,29 pp en el modelo con interacción. Ninguna restricción de años ni la variante `Paper2-` reproducen el 5.154.213: las tablas publicadas salieron de otra versión de la base.
Por eso la comparación usa la **base actual en las cuatro muestras**, y las diferencias entre columnas son consistentes entre sí, pero no son una diferencia contra la tabla publicada.

**Cambio de la prima (en puntos porcentuales) al excluir el segundo empleo:**

Muestra completa, columna 4 (prima frente a trabajadores solos):

| Tamaño | 2-3 | 4-5 | 6-10 | 11-19 | 20-30 | 31-50 | 51-100 | 101+ |
|---|---|---|---|---|---|---|---|---|
| Con segundo empleo | 20,7 | 36,0 | 37,2 | 35,3 | 31,2 | 31,1 | 33,5 | 43,2 |
| Sin segundo empleo | 21,5 | 36,7 | 37,6 | 35,8 | 31,8 | 32,0 | 34,5 | 44,5 |
| Cambio | +0,76 | +0,75 | +0,42 | +0,48 | +0,68 | +0,89 | +1,07 | +1,27 |

Muestra completa, tamaño × formalidad:

| Tamaño | 2-3 | 4-5 | 6-10 | 11-19 | 20-30 | 31-50 | 51-100 | 101+ |
|---|---|---|---|---|---|---|---|---|
| Informales: cambio | +0,88 | +1,03 | +0,57 | +0,64 | +0,40 | +0,13 | +1,39 | +0,76 |
| Formales: cambio | +0,02 | −0,02 | +0,15 | +0,18 | +0,46 | +0,60 | +0,59 | +0,74 |
| Brecha formal–informal: cambio | −0,75 | −0,70 | −0,08 | −0,09 | +0,40 | +0,78 | −0,24 | +0,37 |

Asalariados: los cambios son de −0,26 a +0,16 pp en la columna 4; de −0,47 a +0,87 pp en las primas de informales; de −0,57 a −0,23 pp en las de formales;
y de −0,33 a +1,15 pp en la brecha formal–informal. Detalle en `regresiones_cambio_prima.csv`.

**Lectura.**

- Excluir el segundo empleo **sube todas las primas por tamaño de la muestra completa** (entre 0,4 y 1,3 pp, entre 1% y 4% de la prima), más en las empresas grandes (101+: de 43,2% a 44,5%).
  En asalariados casi no las mueve (−5% a +0,5% de la prima).
- En cada bloque de la tabla, el mayor cambio equivale a entre 0,04 y 0,24 veces el semiancho del intervalo de confianza al 95% de la prima de la muestra de referencia.
  Los cambios son pequeños frente a la incertidumbre de cada estimación; no se hizo una prueba formal de la diferencia entre muestras (las muestras se solapan).
- El orden y la forma de las primas (la U de las primas de los formales y el máximo en 101+) no cambian.
- **Dónde se concentra el segundo empleo** (2008 a 2025, ponderado): 43,4% de quienes lo declaran son **informales que trabajan solos** y 18,5% informales en empresas de 2-3 personas;
  13,5% son formales en empresas de 101+. Los **formales que trabajan solos** son solo 2,2% de quienes declaran segundo empleo, aunque dentro de ese grupo la proporción con segundo empleo es de 5,9%
  (frente a 3,6% a 4,4% en los formales de 6 a 100 personas). Como son la categoría de referencia de la prima de los formales, un cambio en su composición mueve toda esa fila de la tabla.

**Limitaciones.** (i) No se reconstruyó la medida de ingreso: este ejercicio solo mide cuánto dependen los resultados de las personas con segundo empleo, y no resuelve la medición; (ii) con 17 sectores como clusters `fixest` corrige la matriz de covarianzas
(aviso "not positive semi-definite"), algo que depende de la especificación y no de este ejercicio; (iii) hay una ruptura de la serie en 2021.

## 5. Ingreso nulo frente a faltante, y horas del segundo empleo

Entre quienes declaran segundo empleo (`P7040 = 1`), todos los años, ponderado (`descriptivas_segundo_empleo_horas_x_ingreso.csv`):

| | Ingreso `P7070` positivo | Ingreso igual a 0 | Ingreso faltante |
|---|---|---|---|
| `P7045` positiva (1 a 75) | 70,3% | 8,7% | 15,4% |
| `P7045` igual a 0 (ausente esa semana) | 3,8% | 1,2% | 0,5% |
| `P7045` = 98 o 99 | menos de 0,1% | | |

- El **ingreso faltante** pesa entre 10% y 20% de los que declaran segundo empleo según el año; el **ingreso igual a 0**, entre 9% y 12%. Son casos distintos y conviene tratarlos distinto.
- En 3,8% de los casos hay ingreso positivo con 0 horas la semana pasada (ausencia temporal): sumar horas efectivas sin más lo trataría como si no trabajara.
- Una regla de "sumar horas solo si el ingreso del segundo empleo es positivo" deja fuera al 15,4% con horas pero sin ingreso (faltante) y al 8,7% con ingreso cero.

## 6. Pendientes

1. Reconstruir `INGLABO` columna por columna. Sigue sin probarse que `INGLABO − P7070` sea el ingreso del empleo principal.
2. Decidir la medida de horas: habituales del principal (`P6800`, la del paper), o efectivas (`P6850` + `P7045`, mismo período de referencia).
3. Confirmar que la definición de `P7045` es la misma antes de 2025, y revisar la ruptura de 2021.
4. Averiguar qué versión de la base produjo las tablas publicadas (5.154.213 observaciones).
