# Beyond Formality: The Firm-Size Wage Premium in Colombia

This repository contains the paper source, replication code, tables, and figures for:

Oliver Pardo and Jorge Orozco, "Beyond Formality: The Firm-Size Wage Premium in Colombia."

## Working paper

The working paper is available on SSRN:

<https://papers.ssrn.com/sol3/papers.cfm?abstract_id=6932699>

The PDF version stored in this repository is available at:

<Paper/Paper_TamanoEmpresa/Pardo_BeyondFormality.pdf>

## Data and code availability

Replication code and materials are included in this repository. Raw GEIH microdata are not redistributed because they are third-party data produced and distributed by DANE. Researchers should obtain the raw microdata directly from DANE.


## Repository structure

| Folder | Content |
|---|---|
| `Code/` | Scripts grouped by project (`GEIH_Bases`, `Paper_TamanoEmpresa`, `Informes_CJC/*`, `Educacion`, `IA`, `EDIT_EAM`); see [`Code/README.md`](Code/README.md). Run everything from the repository root. |
| `PTF/` | Replication of the DANE total factor productivity (code, data, outputs, notes); see [`PTF/README.md`](PTF/README.md). |
| `Paper/` | All LaTeX, mirroring `Code/`: the paper in `Paper_TamanoEmpresa/`, the reports in `Informes_CJC/*`, shared `sections/`, `tables/`, `figures/`; see [`Paper/README.md`](Paper/README.md). |
| `Datos/` | `Raw/` sources (incl. `PIB_DANE/`) and `Processed/` bases by source (`GEIH/`, `IA/`, `EAC_EAM/`, `EDIT/`); see [`Datos/README.md`](Datos/README.md). Large GEIH files are not tracked. |
| `Outputs/` | Tables and figures produced by the scripts; `Informes_PDF/` holds published report PDFs; see [`Outputs/README.md`](Outputs/README.md). |
| `ProductosFinales/` | Interactive tools, reports, videos and slides delivered by the center, one folder per product. |
| `DocumentacionAuxiliar/`, `Diccionarios/`, `Biblio/` | Classifications, geometries, data dictionaries and references. |
| `Melitz-Polanec/` | Application of the Melitz–Polanec (2015) dynamic Olley–Pakes decomposition to Colombia: feasibility, method, diagnostics; see [`Melitz-Polanec/README.md`](Melitz-Polanec/README.md). |
| `Robustez_SegundoEmpleo/` | Robustness check of the firm-size wage premiums excluding workers with a second job (P7040), with the key file linking `persona_id` to the original GEIH keys; see [`Robustez_SegundoEmpleo/README.md`](Robustez_SegundoEmpleo/README.md). |
| `Trazabilidad/` | Project map (product, code, data, outputs, documents), output manifest and the log of file moves. Start at [`MAPA_DEL_PROYECTO.md`](Trazabilidad/MAPA_DEL_PROYECTO.md). |
