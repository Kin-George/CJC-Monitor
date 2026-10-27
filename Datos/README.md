# Datos

## Raw: fuentes originales (no se editan)

| Carpeta | Contenido |
|---|---|
| `Bogota-Cities/`, `EAC/`, `EAM/`, `EAM_TIC/`, `EAS/`, `EDIT/`, `EDIT-S/`, `EMICROM/` | Encuestas del DANE por fuente |
| `PIB_DANE/` | PIB trimestral por ramas (`anex-ProduccionConstantes-Itrim2026.xlsx`) y PIB departamental (`anex-PIBDep-departamento-2024pr.xlsx`) |

Los microdatos de la GEIH (`GEIH_<año>_total.dta`) **no están en el repositorio**: viven en `~/Documents/Databases/GEIH`.
Los datos del proyecto PTF están en [`../PTF/Datos`](../PTF/README.md).

## Processed: bases derivadas, por fuente

| Archivo | Qué es | Quién la genera |
|---|---|---|
| `GEIH/Paper-GEIH_base_modelo_personas_2008_2025.dta` | Ocupados 2008–2025 limpios para modelos (base del paper y de los informes) | `Code/GEIH_Bases/CreacionPanelLimpio2008-2025*.do` (guardan en `Outputs/tables/`; se trasladan a mano) |
| `GEIH/Paper2-GEIH_base_modelo_personas_2008_2025.dta` | Variante de la anterior | `CreacionPanelLimpio2008-2025_2.do` |
| `GEIH/Paper-GEIH_base_modelo_personas_2021_2025_EI.dta` | Ocupados 2021–2025 con empleo informal (EI) | `CreacionPanelLimpio2021-2025_EI.do` |
| `GEIH/BaseconCINE11.dta` | Ocupados con nivel educativo CINE 11 | `CreacionPanelLimpio2008-2025.do` |
| `GEIH/GEIHfinal.dta`, `GEIH/GEIH_base_modelo_{celdas,personas}_2008_2025.dta`, `GEIH/PanelOcupadosSinFIltros.xlsx` | Bases auxiliares de la GEIH | **Sin script versionado que las escriba** |
| `IA/BaseIA.dta` | Ocupados con ocupación a 4 dígitos para el cruce con exposición a IA | **Sin script versionado que la escriba** |
| `EAC_EAM/*.rds` | Paneles EAC, EAM y EAM-TIC | `Code/EDIT_EAM/EAM_01_DatabaseConstruction.R` |
| `EDIT/` | `EDIT_Panel.dta`, `EDITS_Panel.dta` (se crean al correr los scripts; están en `.gitignore`) | `Code/EDIT_EAM/01_EDIT_DatabaseConstruction.R`, `01.2 EDIT_S-DatabaseConstruction.R` |

Las bases grandes se versionan con Git LFS o están en `.gitignore` (ver `.gitattributes`).
