# Factibilidad de la descomposición dinámica de Olley–Pakes (Melitz y Polanec, 2015) con datos de Colombia

Fecha del análisis: 2026-10-21. Cada afirmación está marcada como **verificada** (se comprobó con los datos del repositorio o con el artículo) o **no verificada** (conocimiento general, falta confirmar).

## 1. Qué exige el método

Melitz y Polanec (2015) descomponen el cambio de la productividad agregada entre dos años, `ΔΦ = Φ₂ − Φ₁`, en cuatro partes:

| Componente | Qué mide |
|---|---|
| Cambio en la media sin ponderar de los sobrevivientes (`Δφ̄_S`) | Mejora de productividad dentro de las empresas que siguen |
| Cambio en la covarianza participación–productividad de los sobrevivientes (`Δcov_S`) | Reasignación de participación de mercado hacia empresas más productivas |
| Contribución de la entrada (`s_E2 (Φ_E2 − Φ_S2)`) | Entrantes frente a los sobrevivientes en el año final |
| Contribución de la salida (`s_X1 (Φ_S1 − Φ_X1)`) | Salientes frente a los sobrevivientes en el año inicial |

Detalle en [`02_Metodologia_DOPD.md`](02_Metodologia_DOPD.md). Lo que necesita en datos:

1. **Un panel de unidades productivas** (empresa o establecimiento) con un identificador estable en el tiempo.
2. **Productividad por unidad**, en logaritmos: valor agregado por trabajador, o productividad total de los factores (PTF), que exige valor agregado real, capital real y trabajo.
3. **Participaciones de mercado** que sumen 1: participación en el empleo (para la productividad laboral) o en el valor agregado nominal (para la PTF).
4. **Clasificar cada unidad** en sobreviviente, entrante o saliente entre el año inicial y el final. Esto exige saber que una unidad que no aparece **no existe**, y no solo que no fue encuestada.
5. **Deflactores** por sector para el valor agregado y para el capital.

El punto 4 es el más exigente y el que más condiciona qué fuente sirve.

## 2. Fuentes de datos disponibles

| Fuente | Sector y unidad | Años | ¿Panel de universo? | Variables para el método | Estado |
|---|---|---|---|---|---|
| **EAM**, Encuesta Anual Manufacturera (DANE) | Industria; establecimiento (`NORDEST`), con id de empresa (`NORDEMP`) | 1992–2024, **sin 2020** | Casi censal: establecimientos con 10 o más personas o con producción sobre un umbral | Valor agregado, producción bruta, consumo intermedio, personal, activos fijos, inversión, salarios | **Verificada y ya está en el repo.** Fuente principal |
| **EAC**, Encuesta Anual de Comercio | Comercio; empresa | 2003–2024 | **No**: es una muestra con factor de expansión | Producción, valor agregado, personal, sueldos | **Verificada.** No trae capital; no sirve para entrada y salida |
| **EAS**, Encuesta Anual de Servicios | Servicios | 2009 en adelante (archivos `.sav`) | No verificado | No verificado | Hay archivos crudos en `Datos/Raw/EAS`; falta revisarlos |
| **EMICROM**, microestablecimientos | Micronegocios | Solo 2024 en el repo | Muestra | No verificado | Falta revisarla |
| **EDIT** (innovación) | Industria | Varias | Muestra | Gasto en innovación | Complemento, no sirve para el cálculo central |
| **Estados financieros de la Superintendencia de Sociedades** | Todos los sectores; empresa | Años recientes | No verificado | Ingresos, activos, personal | **No verificada.** No está en el repo. Cubre solo empresas sobre un tamaño mínimo |
| **PILA** (seguridad social) y **RUES** (registro mercantil) | Todos; empresa | Recientes | No verificado | Empleo (PILA) y nacimiento y muerte de empresas (RUES) | **No verificada.** Acceso restringido o agregado; no se cruzan fácilmente con productividad |

**Conclusión de fuentes.** Solo la EAM tiene hoy todo lo que pide el método (valor agregado, trabajo y capital por unidad, en un panel largo). Es la fuente para empezar. Comercio y servicios entrarían solo para la parte estática (reasignación entre sobrevivientes), no para entrada y salida, y eso después de confirmar el diseño de cada encuesta.

## 3. Diagnóstico con la EAM

Producido por [`Code/01_diagnostico_EAM.R`](../Code/01_diagnostico_EAM.R); tablas en [`Outputs/tables/`](../Outputs/tables/). **Verificado.**

**Cobertura de variables.** El panel tiene 254.031 filas, entre 6.583 y 9.945 establecimientos por año y 19.605 establecimientos distintos con valor agregado, personal y capital positivos. En esa muestra completa están más de 98% de las observaciones de cada año: se pierde muy poco.

**Entrada y salida** (muestra con valor agregado, personal y capital positivos):

| Ventana | Establecimientos al inicio | Al final | Sobrevivientes | Entrantes | Salientes | Entrantes en el año final | Salientes del año inicial |
|---|---|---|---|---|---|---|---|
| 2000 → 2005 | 7.131 | 7.407 | 5.476 | 1.931 | 1.655 | 26,1% | 23,2% |
| 2005 → 2010 | 7.407 | 9.862 | 5.780 | 4.082 | 1.627 | **41,4%** | 22,0% |
| 2010 → 2015 | 9.862 | 8.915 | 7.997 | 918 | 1.865 | 10,3% | 18,9% |
| 2015 → 2019 | 8.915 | 7.518 | 7.010 | 508 | 1.905 | 6,8% | 21,4% |
| 2019 → 2024 | 7.518 | 6.527 | 6.172 | 355 | 1.346 | 5,4% | 17,9% |
| 2022 → 2023 | 6.703 | 6.648 | 6.514 | 134 | 189 | 2,0% | 2,8% |

**Qué muestra el diagnóstico, y por qué importa:**

1. **El panel es largo y casi completo, así que el método es aplicable.** La mediana de años observados por establecimiento es 11, y el identificador de empresa casi no cambia (solo 2% de los establecimientos tiene más de un id de empresa).
2. **Entrar y salir de la EAM no es lo mismo que nacer o morir.** El universo es "10 o más personas o producción sobre un umbral" (documento técnico de la EAM, verificado), y el propio documento advierte que el umbral puede cambiar entre periodos. Un establecimiento que cruza ese umbral aparece como entrante o saliente sin serlo. Esto va a mezclar entrada y salida reales con crecimiento y encogimiento.
3. **46,6% de los establecimientos tienen huecos**: desaparecen de la muestra y reaparecen. En ventanas de cinco años el problema es moderado (entre 4% y 6% de los salientes reaparecen después de la ventana, y entre 2% y 19% de los entrantes ya habían aparecido antes). En ventanas de **un año** el problema es grave: 45% de los "entrantes" 2022→2023 ya existían antes de 2022, en parte por el año 2020 faltante. **Conviene trabajar con ventanas de varios años**, como en el artículo, y no con cambios anuales.
4. **2005→2010 tiene una discontinuidad**: el número de establecimientos sube de 7.407 a 9.862 y 41% de los establecimientos de 2010 son "entrantes". Parece un cambio de cobertura o del marco muestral, no un auge de nuevas plantas. Hay que investigarlo antes de interpretar esa ventana.
5. **Falta 2020**: cualquier ventana que lo use como año inicial o final es imposible, y las que lo atraviesan pierden información intermedia.
6. **El capital es el valor en libros de los activos fijos, a precios corrientes** (`ACTIVFI`). Para una PTF seria hace falta un stock real: deflactar y, si se puede, reconstruirlo con la inversión (`INVEBRTA`) por el método de inventario permanente. Mientras eso no esté hecho, el método se puede aplicar bien a la **productividad laboral** (valor agregado real por trabajador), que no necesita capital.
7. **La clasificación industrial cambia de revisión** (CIIU Rev. 2, 3 y 4 en la serie): para comparar sectores en el tiempo hay que armonizarla.

## 4. Riesgos metodológicos y cómo tratarlos

| Riesgo | Tratamiento propuesto |
|---|---|
| Entrada y salida contaminadas por el umbral de la encuesta | Definir entrantes y salientes de forma más estricta (por ejemplo, que no hayan aparecido antes ni reaparezcan después, y con tamaño lejos del umbral) y reportar el resultado con la definición simple y la estricta |
| Huecos en la serie | Ventanas de cinco años o más; excluir los que reaparecen o reportarlos aparte |
| Discontinuidad 2005–2010 | Revisar el marco; probar ventanas que no la crucen |
| Falta 2020 | Ventanas que no lo usen como extremo (por ejemplo 2015→2019 y 2019→2024) |
| Valores monetarios a precios corrientes | Deflactar con índices de precios del productor por rama (DANE); verificar su disponibilidad y su correspondencia con la CIIU de la EAM |
| Capital contable, no económico | Empezar con productividad laboral; construir el capital real en una etapa posterior |
| Unidad: establecimiento o empresa | Hacer ambos (la EAM trae `NORDEST` y `NORDEMP`) y mostrar la diferencia |
| Representatividad | La EAM no cubre las empresas pequeñas; la descomposición es de la **industria formal de tamaño mediano y grande**, no de toda la economía |

## 5. Hoja de ruta, paso a paso

| Etapa | Qué se hace | Salida | Depende de |
|---|---|---|---|
| 0 | Diagnóstico de factibilidad con la EAM | Este documento y `01_diagnostico_EAM.R` | Hecho |
| 1 | Limpieza del panel: sector armonizado, muestra completa, unidad (establecimiento y empresa), tratamiento del duplicado | Panel analítico de la EAM | Decidir la unidad y la regla de muestra |
| 2 | Deflactores: índices de precios del productor por rama y deflactor de la inversión | Tabla de deflactores | Conseguir los índices |
| 3 | **Productividad laboral:** DOPD con valor agregado real por trabajador y participaciones de empleo, en ventanas de cinco años | Primera descomposición para Colombia | Etapas 1 y 2 |
| 4 | Sensibilidad: definiciones de entrante y saliente, unidad de análisis y ventanas que no crucen 2005–2010 | Tabla de robustez | Etapa 3 |
| 5 | **PTF:** estimar la función de producción por rama (a dos dígitos) y construir el capital real; DOPD con participaciones del valor agregado nominal | Descomposición con PTF | Etapas 2 y 4 |
| 6 | Comparar con otras descomposiciones (Griliches–Regev, Foster–Haltiwanger–Krizan) para mostrar cuánto sesgan la entrada | Cuadro comparativo, como en el artículo | Etapa 3 |
| 7 | Extensión a comercio y servicios (solo reasignación entre sobrevivientes) y vínculo con la PTF agregada del DANE | Documento final | Revisar EAS y EAC |

## 6. Decisiones pendientes

1. **Unidad de análisis:** ¿establecimiento, empresa o ambas?
2. **Primera medida:** ¿empezamos por la productividad laboral (más simple, ya factible) y dejamos la PTF para la etapa 5?
3. **Ventanas:** ¿cinco años como en el artículo (2010→2015, 2015→2019, 2019→2024, con la de 2005→2010 revisada aparte)?
4. **Deflactores:** necesito que me confirmes si dispones de los índices de precios del productor por rama de la CIIU o si los descargamos del DANE.

## 7. Referencia

Melitz, M. J. y Polanec, S. (2015). Dynamic Olley–Pakes productivity decomposition with entry and exit. *RAND Journal of Economics*, 46(2), 362–375. PDF en [`melitz_et_al-2015-the_rand_journal_of_economics.pdf`](melitz_et_al-2015-the_rand_journal_of_economics.pdf).
