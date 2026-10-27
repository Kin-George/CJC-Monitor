# Código

Cada subcarpeta es un proyecto. Los scripts conservan su nombre original; solo cambió la carpeta que los agrupa.
Todo se corre **desde la raíz del repositorio** (`Javeriana/`): los scripts de R ubican la raíz con `_paths.R` o con
`setwd("~/Trabajo-Profesional/Javeriana")`, los de Python la calculan desde su propia ubicación y los de Stata usan rutas absolutas.
El mapa completo (producto, entradas, salidas, documento) está en [`../Trazabilidad/MAPA_DEL_PROYECTO.md`](../Trazabilidad/MAPA_DEL_PROYECTO.md).

| Carpeta | Qué contiene | Entradas principales | Salidas principales |
|---|---|---|---|
| `GEIH_Bases/` | Construcción de las bases de la GEIH: consolidación anual, panel limpio de ocupados 2008–2025, formalidad DANE | Microdatos GEIH (`Documents/Databases/GEIH`) | Bases consolidadas, bases para modelos (`Paper-GEIH_base_modelo_personas_*`) y auditorías, en `Outputs/tables` (de ahí se trasladan a `Datos/Processed/GEIH/`) |
| `Paper_TamanoEmpresa/` | Paquete de replicación del paper *Beyond Formality: The Firm-Size Wage Premium in Colombia* | `Datos/Processed/GEIH/Paper-GEIH_base_modelo_personas_2008_2025.dta` | `Paper/tables`, `Paper/sections`, `Paper/figures` |
| `Informes_CJC/Productividad_Sectorial/` | Informe 01: PIB por trabajador y por hora por actividad, burbujas, figuras de LinkedIn, panel de presentación | Base GEIH + `Datos/Raw/PIB_DANE/` | `pib_geih_productividad_*`, `fig_burbujas_*`, `fig_barras_*` |
| `Informes_CJC/Departamentos/` | Productividad y remuneración departamental, mapas, y generación de los `.tex` de los informes departamentales | Base GEIH, `PIB_DANE/anex-PIBDep…`, `DocumentacionAuxiliar/Geometria/` | `dept_*`, `pib_geih_productividad_departamento*`, `Paper/Informes_CJC/Departamentos/*.tex` |
| `Informes_CJC/Remuneracion_Nacional/` | Remuneración laboral por trabajador y por hora (nacional) | Base GEIH | `remuneracion_geih_*` |
| `Informes_CJC/MercadoLaboral/` | Descriptivas del mercado laboral 2008–2025 (gráficos exploratorios) y utilidades de CIIU | Base GEIH | Ninguna en disco: el script no guarda figuras (`No incluye ggsave`) |
| `Informes_CJC/Diapositivas_CJC_Monitor/` | Estadísticas descriptivas para las diapositivas CJC Monitor | Base GEIH | `fig1…figN` |
| `Educacion/` | Informe 04: ocupados por nivel educativo (CINE 11), tasas de educación | GEIH | `Outputs/tables`, `Outputs/educacion/educacion_data.json` |
| `IA/` | Informe 03: exposición ocupacional a IA (índices de la OIT) y descriptivas | `Datos/Processed/IA/BaseIA.dta`, `DocumentacionAuxiliar/IA/` | `IA_exposicion_ocupaciones_BaseIA.xlsx` |
| `EDIT_EAM/` | Bases de innovación (EDIT, EDIT-S) y de la Encuesta Anual Manufacturera (EAM, EAC) | `Datos/Raw/EDIT`, `EDIT-S`, `EAC`, `EAM`, `EAM_TIC` | `Datos/Processed/EDIT/` y `Datos/Processed/EAC_EAM/` |
| `_paths.R` | Funciones comunes de R: raíz del proyecto, rutas y copia de tablas y figuras al paper | | |

El proyecto de productividad total de los factores (PTF) está aparte, en [`../PTF`](../PTF/README.md).

Orden sugerido: `GEIH_Bases` → (`Paper_TamanoEmpresa`, `Informes_CJC/*`, `Educacion`, `IA`).

Los scripts de R `source("Code/_paths.R")` buscan el archivo en `Code/`; no lo muevas de ahí.
