# Outputs

Resultados que producen los scripts. No se editan a mano.

| Carpeta | Contenido |
|---|---|
| `tables/` | Tablas en csv y xlsx de los informes, de educación y de IA, más auditorías de las bases |
| `Figures/` | Figuras en png (y un gif) de los informes |
| `educacion/` | `educacion_data.json`, base de datos de la herramienta de educación |
| `Informes_PDF/` | PDF de informes ya publicados (Informe 01, comunicado, columna y diapositivas) |
| `MapeoDatos/` | Documentos técnicos de las encuestas EAC, EAM y EAM-TIC |

`tables/` y `Figures/` son comunes a varios productos y se nombran por prefijo (`pib_geih_*`, `dept_*`, `fig_dept_*`…).
El archivo [`../Trazabilidad/salidas_trazabilidad.csv`](../Trazabilidad/salidas_trazabilidad.csv) indica qué script escribe cada uno
y qué documento lo usa. Los resultados del proyecto PTF están en [`../PTF/Outputs`](../PTF/README.md).
