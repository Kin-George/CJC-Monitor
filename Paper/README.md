# Paper

Todo el LaTeX del proyecto. La estructura de carpetas **repite la de `Code/`**: el documento de
`Paper/Informes_CJC/Departamentos/` sale del código de `Code/Informes_CJC/Departamentos/`.

```
Paper/
├── Paper_TamanoEmpresa/                 artículo académico: paper.tex y su PDF
├── Informes_CJC/
│   ├── Productividad_Sectorial/         informe_productividad, comunicado_prensa_productividad, presentacion_informe_01_productividad
│   ├── Departamentos/                   informe_productividad_departamentos, informe_02/03/04_* (generados), config_informes_departamentales
│   ├── Remuneracion_Nacional/           informe_02_remuneracion_laboral_geih
│   ├── MercadoLaboral/                  informe, informe_mercado_laboral_geih
│   └── Diapositivas_CJC_Monitor/        slides
├── sections/   secciones y tablas LaTeX reutilizables (comunes a varios documentos)
├── tables/     tablas exportadas (csv/xlsx)
├── figures/    figuras exportadas
└── references.bib
```

Paper académico: <https://papers.ssrn.com/sol3/papers.cfm?abstract_id=6932699>

## Cómo compilar

Desde la **raíz del repositorio** (las rutas internas empiezan por `Paper/…`):

```
pdflatex Paper/Paper_TamanoEmpresa/paper.tex
```

En Overleaf, elegir el `.tex` deseado como documento principal.

## Carpetas comunes

`sections/`, `tables/` y `figures/` las comparten varios documentos, porque los scripts las llenan en bucle y los
documentos las referencian por nombre. Para saber de qué script viene cada archivo y qué documento lo usa:
[`../Trazabilidad/salidas_trazabilidad.csv`](../Trazabilidad/salidas_trazabilidad.csv).

Los scripts guardan las figuras y tablas de publicación en ambos sitios: `Outputs/Figures/` y `Paper/figures/`,
`Outputs/tables/` y `Paper/tables/`.
