# Robustez: segundo empleo y primas salariales por tamaño de empresa

Responde a la solicitud del coautor sobre el ingreso por hora y el segundo empleo: repetir la especificación preferida del paper
excluyendo a quienes declaran segundo empleo (`P7040 = 1`), en la muestra completa y en la de asalariados, sin cambiar el paper ni las estimaciones publicadas.

**Resultados y respuestas:** [`Docs/Nota_metodologica.md`](Docs/Nota_metodologica.md). Borrador de respuesta: [`Docs/Respuesta_al_coautor.md`](Docs/Respuesta_al_coautor.md).

## Estructura

```
Robustez_SegundoEmpleo/
├── Code/
│   ├── 01_extraer_segundo_empleo.R         llaves originales y variables del segundo empleo desde la GEIH cruda (2008-2025, sin 2020)
│   ├── 02_correspondencia_persona_id.R     persona_id <-> llaves originales, con verificación
│   ├── 03_regresiones_sin_segundo_empleo.R columna 4 y tamaño x formalidad, 4 muestras
│   ├── 04_descriptivas_segundo_empleo.R    frecuencia, horas, ingreso nulo vs faltante, concentración
│   └── 05_archivo_para_coautor.R           archivo con persona_id + llaves + variables del segundo empleo
├── Datos/                                  (no se versiona: pesa más de 3 GB)
│   ├── entrega_persona_id_segundo_empleo.dta / .csv.gz   <- archivo solicitado por el coautor
│   ├── segundo_empleo_llaves_2008_2025.dta / .csv.gz     todos los ocupados crudos con llaves
│   ├── correspondencia_persona_id.rds, paper_base_subconjunto.rds, por_anio/
├── Docs/
└── Outputs/tables/                         regresiones_*.csv, descriptivas_*.csv, correspondencia_persona_id_verificacion.csv
```

## Cómo correrlo (desde la raíz del repositorio, en este orden)

```
Rscript Robustez_SegundoEmpleo/Code/01_extraer_segundo_empleo.R
Rscript Robustez_SegundoEmpleo/Code/02_correspondencia_persona_id.R
Rscript Robustez_SegundoEmpleo/Code/03_regresiones_sin_segundo_empleo.R
Rscript Robustez_SegundoEmpleo/Code/04_descriptivas_segundo_empleo.R
Rscript Robustez_SegundoEmpleo/Code/05_archivo_para_coautor.R
```

Entradas: `Datos/Processed/GEIH/Paper-GEIH_base_modelo_personas_2008_2025.dta` y los archivos crudos `~/Documents/Databases/GEIH/GEIH_<año>_total.dta`.
Los pasos 1 y 2 tardan; el equipo tiene 8 GB de memoria, así que conviene no correr otras tareas pesadas a la vez.
