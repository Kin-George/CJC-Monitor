# Paper

LaTeX outputs for the CJC Monitor project.

## Main files

- `paper.tex`: academic paper.
- `informe.tex`: policy or consulting report.
- `slides.tex`: Beamer presentation.

In Overleaf, select the file you want to compile as the main document.

## Folders

- `figures/`: exported figures used in the paper.
- `tables/`: exported tables used in the paper, report, or slides.
- `sections/`: reusable text sections shared across outputs.

All three main files can reference the same `figures/`, `tables/`, and `references.bib`.

The R scripts should write publication assets to both:

- `Outputs/Figures/` and `Paper/figures/`
- `Outputs/tables/` and `Paper/tables/`
