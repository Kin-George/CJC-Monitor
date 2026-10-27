# Réplica de la PTF del DANE (Colombia)

Réplica a nivel agregado de la Productividad Total de los Factores (PTF) que publica el DANE (enfoque de valor
agregado, 9 ramas, 2005–2025), con COU, stock de capital, PIB trimestral y microdatos de la GEIH.

## Qué se logró

| Periodo | Resultado |
|---|---|
| 2022–2024 | Réplica con error menor a 0,1 pp en la PTF total y en 8 de las 9 ramas (horas efectivas, `P6850`). |
| 2025 | PTF estimada 0,74 pp (±0,24) frente a 0,75 del DANE; el capital de 2025 se estima por persistencia de 2024. |
| 2015–2019 | Error medio de 0,74 pp: el DANE usa insumos laborales retroproyectados que no se pueden reconstruir con datos públicos. |
| 2020–2021 | No confiable (la GEIH 2020 trae solo 8 meses de ocupados y cambia la pregunta de educación). |

La serie final empalma las cifras oficiales hasta 2021 con la réplica propia desde 2022
(`Outputs/tables/ptf_serie_empalmada_2005_2025.csv`). El detalle del método del DANE y de lo que se probó está en
[`Docs/NOTAS_que_hace_el_DANE.md`](Docs/NOTAS_que_hace_el_DANE.md).

## Estructura

```
PTF/
├── Code/        scripts numerados en el orden en que se corren
├── Datos/
│   ├── Raw/         anexos del DANE (COU, stock de capital, anexo oficial de PTF); el PIB trimestral está en `Datos/Raw/PIB_DANE/`
│   │   └── DANE_publicaciones/   boletines y metodologías del DANE consultados (no se versionan)
│   └── Processed/   tablas intermedias (celdas de la GEIH, COU y stock por rama)
├── Outputs/
│   ├── tables/      resultados (réplica vs DANE, diagnósticos, serie empalmada)
│   └── Figures/     figuras de validación y de comparación del método
└── Docs/        documento metodológico del DANE y notas
```

Las bases pesadas de la GEIH no están aquí: `04_PTF_v1_GEIHcompleta.py` y `00c_AgregarGEIH.py` leen
`Datos/Processed/GEIH/GEIHfinal.dta` y `Paper-GEIH_base_modelo_personas_2008_2025.dta` desde la raíz del repositorio, y
`06_ExtraerHorasEfectivas.do` lee los archivos crudos `GEIH_<año>_total.dta` de `Documents/Databases/GEIH`.

## Orden de ejecución

| Paso | Script | Qué hace |
|---|---|---|
| 00a | `00a_ExtraerCOU.py` | Valor agregado y remuneración por rama desde los COU (corrientes y constantes) |
| 00b | `00b_ExtraerStockCapitalOficial.py` | Stock de capital productivo por rama (anexo oficial) |
| 00c | `00c_AgregarGEIH.py` | Agrega la GEIH consolidada por rama y categoría laboral |
| 01–03 | `01_ConstruirPanelBase.R`, `02_CalcularPTF_v0.R`, `03_ValidarVsDANE.R` | Versión v0 en R y su validación |
| 04 | `04_PTF_v1_GEIHcompleta.py` | v1: GEIH completa (`GEIHfinal`), horas habituales |
| 05 | `05_Figura_PTF_validacion.py` | Figura DANE vs v1 vs v2 |
| 06 | `06_ExtraerHorasEfectivas.do` | Stata: horas efectivas (`P6850`) desde la GEIH cruda, 2008–2025 |
| 07 | `07_PTF_v2_horasEfectivas.py` | v2: horas efectivas (la réplica que coincide con el DANE en 2022–2024) |
| 08 | `08_PTF_pruebas_definiciones.py` | Pruebas de definiciones (educación, CIIU, asalariado, etc.) |
| 09–10 | `09_Cargar_DANE_por_actividad.py`, `10_Diagnostico_por_actividad.py` | Horas implícitas del DANE por rama y comparación |
| 11 | `11_Serie_PTF_empalmada.py` | Serie empalmada 2005–2025 y figura |
| 12 | `12_PTF_2025_completar.py` | Estimación de 2025 (VA del PIB trimestral, capital por persistencia) |
| 13 | `13_Figura_comparacion_metodo.py` | Figura que explica cómo se compara la réplica con el DANE |
| 14 | `14_Prueba_Matriz_de_Trabajo.py` | Prueba con las horas de la Matriz de Trabajo del DANE (no mejora la réplica) |

Los scripts de Python se corren desde cualquier carpeta: `python PTF/Code/07_PTF_v2_horasEfectivas.py`.
Los de R usan `Code/_paths.R` y se corren desde la raíz del repositorio. El de Stata:
`StataMP-64.exe /e do PTF/Code/06_ExtraerHorasEfectivas.do` (desde PowerShell, con `Start-Process`).

## Pendientes

- Pedir al DANE (Dirección de Síntesis y Cuentas Nacionales) las series de horas efectivas y composición del trabajo
  por actividad, 2005–2021.
- Cuando el DANE publique el COU y el stock de capital de 2025, volver a correr 07, 11 y 12 para pasar 2025 de
  estimado a réplica completa.
- Corrección pendiente en el ingreso por hora (`BaseConsolidada.do`): el numerador (`INGLABO`) incluye el segundo
  empleo y el denominador (`P6800`) solo el principal. Falta reconstruir `INGLABO` desde sus componentes antes de
  decidir cómo corregirlo.
