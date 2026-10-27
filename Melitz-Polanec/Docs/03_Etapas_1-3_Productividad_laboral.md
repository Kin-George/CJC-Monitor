# Etapas 1 a 3: panel de empresas, deflactor y descomposición de la productividad laboral

Fecha: 2026-10-21. Todo se reproduce con los scripts de [`../Code/`](../Code/). Las cifras salen de [`../Outputs/tables/`](../Outputs/tables/).

## 1. Decisiones

| Tema | Decisión | Por qué |
|---|---|---|
| Unidad de análisis | **Empresa** (`NORDEMP`), agregando sus establecimientos (`NORDEST`) | El artículo trabaja con empresas. `NORDEMP` es estable: mediana de 11 años por empresa y solo 2% de los establecimientos cambia de id de empresa. El 18,1% del empleo está en empresas con más de un establecimiento, así que la unidad sí importa |
| Primera medida | Productividad laboral: `log(valor agregado real / personal ocupado)`, con participaciones de empleo | Es la medida del artículo que no necesita capital real |
| Ventanas | 2010→2015, 2015→2019, 2019→2024; 2005→2010 se reporta aparte | La EAM no tiene 2020; la ventana 2005→2010 cruza un salto de cobertura (41% de "entrantes" en 2010) |
| Muestra | Valor agregado > 0, personal > 0 y activos fijos > 0 | Como en el artículo: las mismas empresas en todas las descomposiciones. El capital no entra aún, pero así la PTF de la etapa 5 usa la misma muestra. Cerca de 99% de las empresas cumple |
| Deflactor | IPP del DANE, "Producción Nacional – Industria", promedio anual, a pesos de 2015 | Ver sección 2 |

## 2. Sobre el deflactor

Se descargó del sitio del DANE la serie histórica del IPP (`anex-IPP-historicos-sep2026.xlsx`, base diciembre 2014 = 100, mensual desde junio de 1999) y el instructivo de empalme, en `Datos/Raw/IPP_DANE/`.
**La serie solo trae agregados** (producción nacional, agricultura, minería, industria), no un índice por división CIIU; el DANE no publica uno largo por CIIU en ese archivo.

Con participaciones de empleo, un deflactor común a todas las empresas de la industria solo desplaza el componente "dentro de las empresas" y deja intactos los componentes de reasignación, entrada y salida (los dos grupos que se comparan en el término de entrada tienen el mismo deflactor en el mismo año). Lo que se pierde son las diferencias de precios relativos entre ramas. Para recuperarlas, en la etapa de robustez se puede usar el deflactor implícito del valor agregado por agrupación de las cuentas nacionales, que el repositorio tiene para **2014–2024** (no cubre la ventana 2010→2015).

## 3. Implementación y pruebas

`Code/04_DOPD_productividad_laboral.R` implementa la ecuación 6 del artículo y, para comparar, las descomposiciones de Griliches–Regev (GR) y de Foster–Haltiwanger–Krizan (FHK) (ecuaciones 3 y 4). Pruebas que corre cada vez:

1. **Identidad:** en cada ventana, los componentes de las tres descomposiciones suman el cambio agregado (tolerancia 1e-9).
2. **Sin entrada ni salida y crecimiento uniforme:** la entrada y la salida de la DOPD son exactamente cero.
3. **Ejemplo del artículo** (empresas idénticas, con crecimiento de 10% y entrada y salida de una parte): la DOPD asigna 0 a entrada y salida; GR le asigna 0,005 y FHK 0,01 a la entrada, que es el sesgo que describe el artículo.

## 4. Resultados

Puntos logarítmicos × 100 (aproximadamente puntos porcentuales). Productividad laboral agregada de la industria, empresas de la EAM.

| Ventana | Cambio total | Mejora dentro de las empresas (media sin ponderar) | Reasignación entre sobrevivientes (covarianza) | Entrada | Salida |
|---|---|---|---|---|---|
| 2010 → 2015 | 8,24 | −0,39 | 6,36 | −1,29 | 3,56 |
| 2015 → 2019 | 10,69 | −6,42 | 12,11 | −0,49 | 5,48 |
| 2019 → 2024 | 10,41 | −0,15 | 7,50 | −0,72 | 3,78 |
| 2005 → 2010 (aparte) | 0,51 | 0,11 | 2,44 | −9,01 | 6,97 |

Tamaño de los grupos (empresas en la muestra):

| Ventana | Empresas en t1 | en t2 | Sobrevivientes | Entrantes | Salientes | Participación de entrantes en t2 | Participación de salientes en t1 |
|---|---|---|---|---|---|---|---|
| 2010 → 2015 | 9.401 | 8.313 | 7.594 | 719 | 1.807 | 6,3% | 11,5% |
| 2015 → 2019 | 8.313 | 6.903 | 6.524 | 379 | 1.789 | 3,9% | 7,7% |
| 2019 → 2024 | 6.903 | 5.941 | 5.647 | 294 | 1.256 | 6,2% | 7,8% |
| 2005 → 2010 | 7.016 | 9.401 | 5.428 | 3.973 | 1.588 | 19,6% | 14,1% |

**Lectura (con cautela, ver sección 5):**

- En las tres ventanas la mayor parte del aumento viene de la **reasignación del empleo hacia sobrevivientes más productivos** (entre 72% y 113% del cambio) y de la **salida** de empresas de menor productividad (entre 36% y 51%).
- La productividad media sin ponderar de los sobrevivientes **no sube**: cae levemente en 2010–2015 y 2019–2024 y cae 6,4 puntos en 2015–2019. La mejora agregada no viene de que la empresa promedio mejore, sino de que el empleo se concentra en las más productivas.
- La entrada resta un poco en todas las ventanas: los entrantes son menos productivos que los sobrevivientes del mismo año (por ejemplo, 11,19 frente a 11,30 en logaritmos en 2019–2024, y 11,07 frente a 11,20 en 2015–2019).
- **Frente a GR y FHK:** la DOPD da a la entrada un valor más bajo (más negativo), en línea con lo que muestra el artículo. En la reasignación entre sobrevivientes la DOPD da mucho más que los otros métodos (en 2015–2019, 12,1 frente a 4,7 en GR y 4,9 en FHK; en 2010–2015, 6,4 frente a −1,1 en ambos). El artículo encuentra una brecha de dos a tres veces en Eslovenia; aquí es mayor, y en 2010–2015 cambia de signo. No lo interpreto todavía: faltan las pruebas de robustez.

## 5. Advertencias

1. **Entrada y salida no son nacimiento y muerte.** La EAM cubre establecimientos con 10 o más personas o producción sobre un umbral que puede cambiar entre periodos. Una empresa que cruza el umbral aparece como entrante o saliente. La salida (+3,6 a +5,5) se interpreta por tanto como "salen de la encuesta empresas menos productivas", y podría ser en parte reducción de tamaño.
2. **La ventana 2005→2010 no es comparable.** La entrada y la salida (−9,0 y +7,0) casi se cancelan y su peso sobre un cambio total de 0,5 no tiene sentido; coincide con el salto de 7.016 a 9.401 empresas de la muestra entre 2005 y 2010.
3. **Productividad por trabajador, no por hora.** La EAM trae personal ocupado, no horas.
4. **Deflactor común** (sección 2): no recoge diferencias de precios entre ramas.
5. **Sin recorte de valores extremos.** No se recortó la distribución de la productividad; en la etapa 4 conviene probar con y sin el 1% extremo.

## 6. Qué sigue

| Etapa | Qué | Estado |
|---|---|---|
| 4 | Robustez: definición estricta de entrante y saliente (sin aparecer antes ni reaparecer después, y lejos del umbral de tamaño), establecimiento frente a empresa, recorte de extremos, deflactor por rama para 2015–2024 | Pendiente |
| 5 | PTF: estimar la función de producción por rama y construir el capital real | Pendiente |
| 6 | Comparación con GR y FHK con más detalle | Parcial (ya está la comparación de entrada, salida y reasignación) |
| 7 | Comercio y servicios | Pendiente |

Decisiones que necesito de usted para la etapa 4: (a) qué umbral de tamaño usar para la definición estricta (por defecto, empresas con al menos 15 personas en los dos extremos de la ventana); (b) si prefiere recortar los extremos al 1% o al 5%.
