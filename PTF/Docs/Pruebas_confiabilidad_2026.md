# Pruebas de confiabilidad de la estimación de 2026

Fecha: 2026-10-24. Código: [`19_Retrospectiva_ene_sep.py`](../Code/19_Retrospectiva_ene_sep.py), [`20_Bootstrap_composicion.py`](../Code/20_Bootstrap_composicion.py), [`18_Trabajo_GEIH_2026.py`](../Code/18_Trabajo_GEIH_2026.py). Tablas: `../Outputs/tables/geih_retrospectiva_ene_sep_2023_2025.csv`, `geih2026_bootstrap_composicion.csv`, `geih2026_composicion_mensual.csv`, `sim2026_sensibilidad_ruptura_educacion.csv`.

## Prueba 1. La receta del trabajo frente a lo que el DANE publicó (enero–septiembre de 2023, 2024 y 2025)
Se aplicó la medición propia del trabajo (GEIH, horas efectivas P6850, composición por sexo, edad y educación) a enero–septiembre de cada año y se comparó con los boletines del DANE para la concertación del salario mínimo (año corrido III trimestre, preliminar). Con el valor agregado y el capital del DANE, se obtiene la PTF y la productividad por hora implícitas.

| | 2023 | 2024 | 2025 |
|---|---|---|---|
| Composición laboral: propia / DANE | 0,30 / 0,35 | −0,03 / 0,10 | −0,26 / −0,20 |
| **Error de la composición** | −0,05 | −0,13 | −0,06 |
| Aporte de las horas: propio / DANE | 1,50 / 0,55 | −0,43 / −0,73 | 1,40 / 1,43 |
| Error del aporte del trabajo (L) | +0,89 | +0,17 | −0,09 |
| **Error de la PTF** (VA y capital del DANE) | −0,90 | −0,18 | +0,10 |
| Horas propias menos horas implícitas del DANE (pp) | +2,4 | +0,6 | +0,5 |
| Error de la productividad por hora | −2,4 | −0,6 | −0,5 |

Lectura: la composición se reproduce muy bien (error medio 0,08). El aporte del trabajo y la PTF se reproducen en 2024 y 2025 (errores menores a 0,2) y fallan en 2023, año en que el empleo del DANE (1,6%) tampoco coincide con la GEIH actual (3,7%), seguramente por revisiones de los factores de expansión. **Las horas de la GEIH crecen más que las implícitas del DANE en los tres años**; ninguna de las definiciones probadas (P6850, P6850 + P7045, P6800) las reproduce. La productividad por hora propia queda por debajo de la del DANE.

## Prueba 2. Composición laboral de 2026 (enero–agosto frente a enero–agosto de 2025)
- **Error de muestreo** (bootstrap por vivienda, 300 réplicas): composición +1,15 ± 0,19 (5–95%: 0,84 a 1,45); horas por ocupado −3,29 ± 0,20%.
- **Mes a mes**: la composición es positiva en los ocho meses (de +0,3 a +2,0). No viene de un solo mes.
- **Pero la fuente es la educación, y parece una ruptura en la variable.** Proporción de adultos de 25 años o más con educación superior (enero–agosto): 28,9% (2023), 29,3% (2024), 29,2% (2025) y **31,0% (2026)**. El salto de +1,8 pp aparece en todos los grupos de edad (25–34: +1,7; 35–44: +2,7; 45–54: +2,4; 55–64: +1,8; 65+: +0,9, cohortes cuya educación no cambia en un año), en ocupados (+2,0) y no ocupados (+1,3), y en zona urbana y rural. Entre 2024 y 2025 la variación fue de −1,0 a +0,8 según grupo. En los códigos de P3042, primaria baja 1,3 pp y universidad, especialización y posgrado suben 1,6 pp en total. No encontré en la ficha metodológica de 2026 un cambio de cuestionario.
- Con solo sexo y edad la composición es 0,11: casi todo el +1,15 sale de la educación.

## Prueba 3. Qué definición de horas reproduce al DANE
Horas implícitas del DANE (VA menos productividad por hora): 0,13% (2023), −1,64% (2024), +2,34% (2025). Las de la GEIH: 2,5%, −1,0% y +2,8% (P6850), casi lo mismo con P6850 + P7045 y P6800 más alto en 2023. El DANE usa horas de las cuentas nacionales, no la suma directa de la GEIH.

## Prueba 4. Documentación del DANE y diagnóstico del salto de la educación (script 21)
Documentos descargados a `Datos/Raw/GEIH_DANE_documentacion/` (catálogo de microdatos del DANE, GEIH 2026): control de cambios del formulario 2022–2026, diccionario de datos 2026 y cuestionario v9.
- **Control de cambios:** las preguntas de educación solo cambiaron en el primer trimestre de 2022 (mejor redacción y más alternativas de respuesta). Los cambios de 2026 son preguntas en periodo de prueba sobre tipo de contrato, tipo de pago y herramientas de los independientes, cuyas variables no están en las bases anonimizadas, y ajustes del módulo de micronegocios. **No hay un cambio de cuestionario que explique el salto.**
- **Variables nuevas de 2026:** `P3560`–`P3563` (RUT y personería jurídica), `P1879S1` y `P3363S1` (texto "otro, ¿cuál?"). Ninguna es de educación.
- **Códigos de P3042:** 7 a 13 son normalista, técnica profesional, tecnológica, universitaria, especialización, maestría y doctorado; es la definición de "superior" que usa la receta.
- **Dónde está el salto:** dentro de cada departamento (1,83 de los 1,85 pp; sube en 23 de 33); en todos los grupos de edad; en ocupados (+2,0) y no ocupados (+1,3). Por código: primaria −1,3 pp; universitaria +0,7, especialización +0,5, maestría +0,4. Entre 2024 y 2025 el cambio de los códigos fue de −0,3 a +0,6 pp.
- **Ocupaciones:** la proporción de directivos, profesionales y técnicos sube 1,3 pp (25,4% a 26,6%) y la educación superior sube dentro de las ocupaciones profesionales (78,7% a 81,4%) y no profesionales (18,9% a 19,7%). Una parte del cambio es consistente con una composición distinta de la muestra o de los ocupados, no solo con un error de codificación. Pero si los despidos se concentraran en trabajos de baja educación, los no ocupados estarían menos educados, no más.
- **No se pudo seguir a las mismas personas** entre 2025 y 2026: los identificadores de vivienda están anonimizados por año y no coinciden.
- **Lectura:** un cambio de +1,8 pp en la educación de los adultos en un año es unas cuatro veces el cambio de 2023–2024 (+0,4) y no tiene explicación demográfica ni de cuestionario documentada. Lo más probable es que sea un cambio de la muestra o de las respuestas. Como mi receta reprodujo la composición del DANE en 2023–2025 con estas mismas bases anonimizadas (error de 0,05 a 0,13), **el DANE probablemente verá el mismo salto en su composición**, salvo que lo corrija. Por eso la probabilidad del escenario con el salto pasa de 50% a 75% (juicio).

## Qué cambió en la simulación
- La composición tiene dos escenarios: con el salto de la educación (+1,15, medido) o con una composición típica (0,25 ± 0,30; el DANE publicó −0,20, 0,10 y 0,35). Se les da 75% al escenario con el salto y 25% al típico (juicio; ver la prueba 4).
- Las horas de la GEIH se corrigen con un sesgo de 0,5 ± 1,0 pp (el DANE queda más bajo).
- El error de la receta del trabajo frente al DANE pasa de 0,25 a 0,30 en el aporte del trabajo y de 0,10 a 0,53 en la PTF (RMSE de la prueba 1).

## Resultados resultantes
| | Antes de las pruebas | Después |
|---|---|---|
| Productividad por hora | 3,3% (2,5–4,0) | **3,8%** (2,0–5,6) |
| PTF | 0,15% (−0,55 a 0,84) | **0,44%** (−0,53 a 1,45); 0,33 si el DANE incluye el salto de la educación y 0,81 si no |
| Aumento del salario mínimo (base) | 8,5% (7,0–11,8) | **8,6%** (7,1–12,4); $1.902.183 |

## Qué falta para confiar más
1. Saber si el DANE corrige o no la ruptura de la educación (la cifra de fines de noviembre lo dirá).
2. ~~Ver el cambio de cuestionario de 2026~~ Hecho (prueba 4): no hay cambio documentado en las preguntas de educación.
3. Más años de comparación: solo hay tres.
4. El valor agregado y el capital siguen sin prueba de nowcast (se usaron los oficiales).
