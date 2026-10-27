# Melitz–Polanec: descomposición dinámica de Olley–Pakes para Colombia

Objetivo: aplicar con datos de Colombia la descomposición de Melitz y Polanec (2015), que separa el cambio de la
productividad agregada en mejora dentro de las empresas que siguen, reasignación entre ellas, entrada y salida.
Se construye por etapas.

## Estado

| Etapa | Estado |
|---|---|
| 0. Factibilidad y fuentes de datos | **Hecha** ([`Docs/01_Factibilidad.md`](Docs/01_Factibilidad.md)) |
| 1. Panel de empresas de la EAM (unidad: empresa, `NORDEMP`) | **Hecha** |
| 2. Deflactor | **Hecha** (IPP industria del DANE; por rama, solo 2014-2024, queda para robustez) |
| 3. Productividad laboral (primera descomposición) | **Hecha** ([`Docs/03_Etapas_1-3_Productividad_laboral.md`](Docs/03_Etapas_1-3_Productividad_laboral.md)) |
| 4–7. Robustez, PTF, comparación con otras descomposiciones, comercio y servicios | Pendiente |

Conclusión de la etapa 0: el método es aplicable a la **industria manufacturera** con la EAM (1992–2024, sin 2020), ya
construida en el repositorio. La entrada y la salida deben leerse con cautela, porque la EAM tiene un umbral de tamaño
y casi la mitad de los establecimientos desaparecen y reaparecen; se recomiendan ventanas de cinco años.

## Estructura

```
Melitz-Polanec/
├── Docs/
│   ├── 01_Factibilidad.md           fuentes, diagnóstico con datos, riesgos y hoja de ruta
│   ├── 02_Metodologia_DOPD.md       el método, con las ecuaciones del artículo
│   ├── 03_Etapas_1-3_Productividad_laboral.md   decisiones, resultados y advertencias de la primera descomposición
│   └── melitz_et_al-2015-the_rand_journal_of_economics.pdf
├── Code/
│   ├── 01_diagnostico_EAM.R         diagnóstico de factibilidad (cobertura, entrada y salida, huecos)
│   ├── 02_deflactor_IPP.R           deflactor anual: IPP industria
│   ├── 03_panel_empresas_EAM.R      panel de empresas (establecimientos agregados) con valor agregado real
│   ├── 04_DOPD_productividad_laboral.R   descomposición DOPD, GR y FHK, con pruebas
│   └── 05_figura_DOPD_laboral.py    figura de resultados
├── Datos/                           deflactor y panel de empresas (derivados)
└── Outputs/                         tables/ (diagnóstico y resultados) y Figures/
```

Los datos no se copian aquí: se leen de `Datos/Processed/EAC_EAM/EAM_panel_completo.rds`, que construye
`Code/EDIT_EAM/EAM_01_DatabaseConstruction.R`. Correr desde la raíz del repositorio:

```
Rscript Melitz-Polanec/Code/01_diagnostico_EAM.R
```

Referencia: Melitz, M. J. y Polanec, S. (2015). *Dynamic Olley–Pakes productivity decomposition with entry and exit*.
RAND Journal of Economics, 46(2), 362–375.
