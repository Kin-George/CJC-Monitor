# La descomposición dinámica de Olley–Pakes (DOPD)

Resumen del método de Melitz y Polanec (2015, *RAND Journal of Economics*, 46(2), 362–375), con las ecuaciones numeradas como en el artículo.

## Productividad agregada

La productividad agregada es el promedio ponderado de la productividad de cada unidad (ecuación 1):

`Φ_t = Σ_i s_it · φ_it`

con participaciones `s_it ≥ 0` que suman 1 y productividad `φ_it` en logaritmos (el cambio se lee como porcentaje). El método no depende de qué peso o qué medida de productividad se use. El artículo usa dos:

- **Productividad laboral:** logaritmo del valor agregado por trabajador, con participaciones de empleo.
- **PTF:** residuo de una función de producción estimada por rama a dos dígitos, `ln PTF_it = ln Y_it − β̂_K ln K_it − β̂_L ln L_it` (ecuación 7), con participaciones del valor agregado nominal.

## Grupos de unidades entre `t = 1` y `t = 2`

- **S**: sobrevivientes (existen en los dos años).
- **E**: entrantes (existen solo en `t = 2`).
- **X**: salientes (existen solo en `t = 1`).

Para un grupo `G`: participación agregada `s_Gt = Σ_{i∈G} s_it` y productividad media del grupo `Φ_Gt = Σ_{i∈G} (s_it / s_Gt) · φ_it`.

## La descomposición (ecuación 6)

`ΔΦ = (Φ_S2 − Φ_S1) + s_E2 · (Φ_E2 − Φ_S2) + s_X1 · (Φ_S1 − Φ_X1)`

y el primer término se parte con Olley y Pakes (1996), en el que la productividad de un grupo es su media sin ponderar más la covarianza entre participación y productividad (ecuación 5):

`Φ_t = φ̄_t + Σ_i (s_it − s̄_t)(φ_it − φ̄_t) = φ̄_t + cov(s_it, φ_it)`

De donde:

`ΔΦ = Δφ̄_S + Δcov_S + s_E2 · (Φ_E2 − Φ_S2) + s_X1 · (Φ_S1 − Φ_X1)`

| Término | Interpretación |
|---|---|
| `Δφ̄_S` | Cambio de la media sin ponderar de la productividad de los sobrevivientes: mejora dentro de la empresa promedio |
| `Δcov_S` | Cambio de la covarianza participación–productividad entre sobrevivientes: reasignación hacia las más productivas |
| `s_E2 (Φ_E2 − Φ_S2)` | Aporte de la entrada: positivo si los entrantes son más productivos que los sobrevivientes **en el año final** |
| `s_X1 (Φ_S1 − Φ_X1)` | Aporte de la salida: positivo si los salientes eran menos productivos que los sobrevivientes **en el año inicial** |

La participación que entra en la covarianza de los sobrevivientes es la participación **dentro del grupo de sobrevivientes** (suma 1 en ese grupo).

La entrada y la salida pueden además partirse en una diferencia de medias y una diferencia de covarianzas (`s_E2 (φ̄_E2 − φ̄_S2)` y `s_E2 (cov_E2 − cov_S2)`).

## Por qué difiere de las otras descomposiciones

Griliches–Regev (GR) y Foster–Haltiwanger–Krizan (FHK) siguen a cada unidad en el tiempo y miden la entrada y la salida contra **un mismo nivel de referencia** (el promedio de los dos años, o el nivel inicial). Cuando hay crecimiento de la productividad, ese nivel de referencia queda por debajo de `Φ_S2`, y eso **sobrestima la contribución de la entrada** y subestima la de los sobrevivientes. La DOPD usa referencias distintas para cada grupo (`Φ_S2` para los entrantes, `Φ_S1` para los salientes), de modo que la contribución de la entrada es lo que cambiaría la productividad al agregar o quitar ese grupo.

| Grupo | GR | FHK | DOPD |
|---|---|---|---|
| Sobrevivientes | `s_S2(Φ_S2 − Φ̄) − s_S1(Φ_S1 − Φ̄)` | `s_S2(Φ_S2 − Φ_1) − s_S1(Φ_S1 − Φ_1)` | `Φ_S2 − Φ_S1` |
| Entrantes | `s_E2(Φ_E2 − Φ̄)` | `s_E2(Φ_E2 − Φ_1)` | `s_E2(Φ_E2 − Φ_S2)` |
| Salientes | `s_X1(Φ̄ − Φ_X1)` | `s_X1(Φ_1 − Φ_X1)` | `s_X1(Φ_S1 − Φ_X1)` |

Con los datos de Eslovenia, en 1995–2000 el sesgo de esas medidas sobre la entrada llegaba a 10 puntos porcentuales del crecimiento agregado, y la reasignación entre sobrevivientes era de dos a tres veces mayor con la DOPD.

## Datos que usa el artículo (para comparar con Colombia)

Panel contable de empresas manufactureras de Eslovenia 1995–2000 (AJPES): ventas, costos de materiales y servicios, capital físico nominal y empleo. Se deflactan ingresos y materiales con el índice de precios del productor a dos dígitos y el capital con el índice de precios de los bienes de inversión. Se conservan las empresas con al menos un trabajador, capital positivo y valor agregado positivo. Con ese criterio había 3.867 empresas en 1995 y 4.580 en 2000: 2.677 sobrevivientes, 1.903 entrantes y 1.191 salientes.
