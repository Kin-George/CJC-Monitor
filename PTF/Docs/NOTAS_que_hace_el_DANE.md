# Qué hace exactamente el DANE en la PTF (hallazgos de la revisión de documentos públicos)

Fuentes: nota metodológica de marzo 2022, boletines de PTF 2022, 2024pr y III trimestre 2025pr, presentación de noviembre de 2020
(`presentacion-PTF-productividad-total-factores-2020.pdf`, `doc-calculo-pr-para-subcomision-de-productividad-30-11-2020.pdf`) y boletín de la Matriz de Trabajo 2024p-2025p.
Todos están en www.dane.gov.co (carpetas `investigaciones/boletines/pib/productividad`, `operaciones/PTF` y `operaciones/MT`).

## Horas: la diferencia clave
- El DANE usa **horas efectivamente trabajadas la semana pasada** ("¿Cuántas horas trabajó durante la semana pasada en este trabajo?"). Suma las horas de los ocupados con el factor de expansión **mensual**, repite para cada mes y promedia los 12 meses.
- Nuestra variable `horas` (`P6800` en `Code/GEIH_Bases/BaseConsolidada.do`) son las horas que la persona dice trabajar **normalmente**.
- Promedios publicados por el DANE (efectivas, total economía): 2021 = 44,0; 2022 = 43,4; 2023 = 42,8; 2024 = 42,1. Los nuestros (habituales): 44,8; 44,7; 44,4; 43,9.
- Horas semanales totales del DANE (GEIH, millones; vintage nov. 2020): 2015 = 922,5; 2016 = 929,8; 2017 = 931,1; 2018 = 939,6; 2019 = 928,7; 2020 = 782,4.
- Prueba: al sustituir el promedio de horas efectivas por rama del DANE (2021–2024) y mantener nuestro índice de composición, el aporte del trabajo pasa de 5,40 / 2,26 / −0,02 a 4,54 / 1,74 / −0,30 (DANE: 4,40 / 1,46 / −0,22).

## Servicios laborales
- 18 categorías (sexo × edad 15-29, 30-49, 50+ × educación básica, media, superior) por actividad, con ponderadores = participación en la remuneración laboral (índice de Törnqvist).
- Se descompone en efecto de composición + efecto de horas (el boletín los publica por separado).
- Insumos GEIH: horas de ocupados, horas de asalariados e ingresos laborales por tipo de empleo.

## Capital
- Stock productivo por inventario permanente con depreciación geométrica (Anexo 1 de la nota) para 10 activos (IT, CT, Soft, TraEq, OMach, OCon, RStruc, Cult, RD, OIPP).
- Tasa de retorno ex-post (ingreso no laboral), costo de usuario por activo y servicios de capital como Törnqvist de los stocks ponderados por el valor de sus servicios. Series de FBKF 1990-2020 en volumen y precios, base 2015 (Dirección de Síntesis y Cuentas Nacionales).
- El DANE reporta capital TIC y no TIC por separado.

## Otros
- Aportes y PTF del total se agregan con ponderaciones de Törnqvist por la composición del valor agregado.
- Cálculos hechos en STATA (LA-KLEMS) y en Python (DANE); el código no es público.
- Para 2019-2020 y los años recientes usan cuentas trimestrales preliminares; las cifras se revisan con cada publicación.
- Desde 2022 la GEIH usa el marco 2018 (Censo 2018); la Matriz de Trabajo reescribió 2021-2022 con ese marco.

## Resultado de la v2 (horas efectivas desde la GEIH cruda)
- `06_ExtraerHorasEfectivas.do` lee `Documents/Databases/GEIH/GEIH_<anio>_total.dta` y deja celdas con P6850.
- Horas efectivas promedio calculadas: 2021 = 44,03; 2022 = 43,36; 2023 = 42,79; 2024 = 42,14 (DANE: 44,0; 43,4; 42,8; 42,1).
- Error medio absoluto del aporte del trabajo / de la PTF: 2022–2024 = 0,07 / 0,08 pp con horas efectivas (0,64 / 0,57 con habituales); 2015–2019 = 0,72 / 0,74 pp (sin mejora).
- 2020 y 2020–21 no son confiables: el archivo crudo de 2020 trae ocupados solo en 8 meses y casi sin variables de educación e ingreso; y el cambio de pregunta de educación entre 2019 y 2021 infla la composición.
