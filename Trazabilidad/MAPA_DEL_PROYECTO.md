# Mapa del proyecto

Cómo se conecta cada producto: **microdatos → código → bases → salidas → documento → entregable**.
Se lee de izquierda a derecha; cada fila de la tabla es un producto.

```
Microdatos (fuera del repo)      Datos/Raw/            Code/<proyecto>/          Datos/Processed/<fuente>/
~/Documents/Databases/GEIH   +   Datos/Raw/PIB_DANE +  scripts numerados    ->   bases limpias
                                 DocumentacionAuxiliar                                 |
                                                                                       v
ProductosFinales/NN  <-  Paper/<...>/documento.tex  <-  Paper/{sections,tables,figures} + Outputs/{tables,Figures}
```

## 1. Productos

| Producto | Código | Entradas | Salidas (prefijos) | Documento LaTeX | Entregables |
|---|---|---|---|---|---|
| **Informe 01.** Dinámicas de la productividad laboral por actividad económica 2010–2025 | `Code/Informes_CJC/Productividad_Sectorial/` | `Datos/Processed/GEIH/Paper-GEIH_base_modelo_personas_2008_2025.dta`, `Datos/Raw/PIB_DANE/anex-ProduccionConstantes-Itrim2026.xlsx` | `pib_geih_productividad_{sector,total,25,61}*`, `valor_agregado_*`, `fig_burbujas_*`, `fig_barras_*`, `fig_linkedin_*` | `Paper/Informes_CJC/Productividad_Sectorial/` (`informe_productividad`, `comunicado_prensa_productividad`, `presentacion_informe_01_productividad`) | `ProductosFinales/01`, `Outputs/Informes_PDF/` |
| **Informe 02.** Productividad total de los factores 2005–2025 | `PTF/` (proyecto aparte) | COU, stock de capital, anexo oficial de PTF, GEIH cruda | `PTF/Outputs/` | `PTF/` (sin LaTeX propio) | `ProductosFinales/02` |
| **Informe 03.** Potencial de automatización por IA | `Code/IA/` | `Datos/Processed/IA/BaseIA.dta`, `DocumentacionAuxiliar/IA/`, GEIH 2025 | `IA_exposicion_ocupaciones_BaseIA.xlsx`, `Outputs/Figures/IA_Descriptivas/` | El informe vive en `Trabajo-Profesional/Informe_IA` (fuera de este repo) | `ProductosFinales/03` |
| **Informe 04.** Radar de remuneración educativa | `Code/Educacion/` | `Datos/Processed/GEIH/BaseconCINE11.dta`, GEIH cruda 2021–2025 | `Outputs/educacion/educacion_data.json`, `GEIH_*CINE11*.xlsx` | — | `ProductosFinales/04` |
| **Herramienta 05.** Geografía de la remuneración laboral (departamentos); la herramienta se rotula «Informe 02» | `Code/Informes_CJC/Departamentos/` | `Paper-GEIH_base_modelo_personas_2008_2025.dta`, `Datos/Raw/PIB_DANE/anex-PIBDep-departamento-2024pr.xlsx`, `DocumentacionAuxiliar/Geometria/` | `dept_*`, `pib_geih_productividad_departamento*`, `fig_dept_*` | `Paper/Informes_CJC/Departamentos/` (los `informe_0X_*` los **genera** `02.8_Informes_Departamentales.py`) | `ProductosFinales/05` |

Documentos de trabajo sin producto numerado:

| Documento | Código | Documento LaTeX |
|---|---|---|
| Artículo académico *Beyond Formality: The Firm-Size Wage Premium in Colombia* | `Code/Paper_TamanoEmpresa/` | `Paper/Paper_TamanoEmpresa/paper.tex` (PDF al lado) |
| Mercado laboral, ingresos y brechas 2010–2025 | `Code/Informes_CJC/MercadoLaboral/` | `Paper/Informes_CJC/MercadoLaboral/` |
| Remuneración por trabajador y por hora (nacional) | `Code/Informes_CJC/Remuneracion_Nacional/` | `Paper/Informes_CJC/Remuneracion_Nacional/` |
| Descomposición dinámica de Olley–Pakes (Melitz y Polanec, 2015) para Colombia, en construcción | `Melitz-Polanec/Code/` (entradas: `Datos/Processed/EAC_EAM/EAM_panel_completo.rds`) | `Melitz-Polanec/Docs/` |
| Robustez del paper: primas por tamaño excluyendo segundo empleo | `Robustez_SegundoEmpleo/Code/` (entradas: base del paper y GEIH cruda) | `Robustez_SegundoEmpleo/Docs/` (sin LaTeX; no modifica el paper) |
| CJC Monitor, resultados preliminares (diapositivas) | `Code/Informes_CJC/Diapositivas_CJC_Monitor/` | `Paper/Informes_CJC/Diapositivas_CJC_Monitor/slides.tex` |

Bases que alimentan todo lo anterior: `Code/GEIH_Bases/` (GEIH) y `Code/EDIT_EAM/` (EDIT, EDIT-S, EAM, EAC).

## 2. Dónde está cada cosa

- **Código** `Code/<proyecto>/` y `PTF/Code/`. Los nombres de carpeta de `Code/Informes_CJC/` se repiten en `Paper/Informes_CJC/`.
- **Bases** `Datos/Raw/<fuente>/` (originales) y `Datos/Processed/<fuente>/` (derivadas): `GEIH/`, `IA/`, `EAC_EAM/`, `EDIT/`.
- **Salidas** `Outputs/{tables,Figures}` y su copia de publicación `Paper/{tables,figures,sections}`. Estas carpetas son
  comunes a varios documentos; el archivo [`salidas_trazabilidad.csv`](salidas_trazabilidad.csv) dice, para cada una de
  las 457 salidas, qué script la escribe y qué documentos la usan.
- **Historial** [`historial_reorganizacion_20261021.csv`](historial_reorganizacion_20261021.csv): ruta anterior y nueva de
  cada archivo que se movió.

## 3. Huecos de trazabilidad (lo que no se puede rastrear hoy)

1. **Bases sin script que las escriba.** Ningún script versionado crea con ese nombre `GEIHfinal.dta`,
   `GEIH_base_modelo_{celdas,personas}_2008_2025.dta`, `Paper-GEIH_base_modelo_personas_2008_2025.dta`, `BaseIA.dta` ni
   `PanelOcupadosSinFIltros.xlsx`. Los `.do` de `GEIH_Bases/` guardan sus bases en `Outputs/tables/` y de ahí se
   trasladan a mano a `Datos/Processed/GEIH/`.
2. **Entradas fuera del repositorio.** Los microdatos crudos (`~/Documents/Databases/GEIH/GEIH_<año>_total.dta`) y el
   informe de IA (`Trabajo-Profesional/Informe_IA`). Antes, dos archivos del DANE se leían de `~/Downloads` de otro usuario;
   ya están en `Datos/Raw/PIB_DANE/`.
3. **109 de 457 salidas no tienen origen identificable**: ningún script ni documento las nombra (en su mayoría figuras
   `figNN` generadas en bucles). Están marcadas "sin clasificar" en el manifiesto.
4. **Documentos que no compilan** (ya fallaban antes de reorganizar): `Paper/Informes_CJC/Departamentos/informe_productividad_departamentos.tex`
   y `Paper/Informes_CJC/Diapositivas_CJC_Monitor/slides.tex`.
5. `02.8_Informes_Departamentales.py` escribe cuatro `.tex` con contenido repetido (`informe_02_*` iguales entre sí e `informe_03_*`/`informe_04_*` iguales entre sí).
6. La herramienta de `ProductosFinales/05` se rotula «Informe 02», igual que la PTF de `ProductosFinales/02`.
7. Las carpetas de salida comunes mezclan productos; separarlas exige reescribir las rutas de los scripts de R, que las
   arman con bucles.

## 4. Cómo compilar y correr

- LaTeX: desde la raíz del repo, p. ej. `pdflatex Paper/Paper_TamanoEmpresa/paper.tex`. En Overleaf, elegir ese archivo como principal.
- R: desde la raíz del repo (usan `Code/_paths.R`). Python: desde cualquier carpeta. Stata: rutas absolutas.
