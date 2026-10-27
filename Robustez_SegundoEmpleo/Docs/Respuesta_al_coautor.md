# Borrador de respuesta

Hola,

Gracias por las observaciones, tienen razón en los dos puntos. Dejo aquí lo que hicimos, sin modificar el paper ni las estimaciones publicadas.

**1. Definiciones.** Revisé la documentación del DANE de 2025: `P6800` son las horas **habituales** del empleo principal y `P7045` son las horas **efectivas de la semana pasada** en el segundo empleo, así que no miden lo mismo. Mi mensaje anterior decía lo contrario; es incorrecto. La pareja coherente de `P7045` sería `P6850`. Solo lo verifiqué para 2025, falta confirmarlo para los años anteriores. También corregí que en mi primer cálculo contaba los códigos 98 y 99 de `P7045` como horas (pesan a lo sumo 0,5% de quienes declaran segundo empleo).

**2. Cálculos previos.** Los hice para 2019, 2022 y 2024, con los 12 meses de cada archivo anual, sobre los ocupados (`OCI == 1`) y ponderando con el factor de expansión mensual. Ahora los repetí con todos los años (2008 a 2025, sin 2020) en `Code/04_descriptivas_segundo_empleo.R`. El porcentaje de ocupados con segundo empleo va de 5,5% a 8,7% hasta 2019, cae a 2,4% en 2021 y está entre 3,8% y 4,1% en 2022 a 2025. Entre quienes lo declaran, el ingreso del segundo empleo es positivo en 70% a 81%, es cero en 9% a 12% y falta en 10% a 20%, por lo que conviene tratar el cero y el faltante por separado.

**3. Archivo y llaves.** `persona_id` es solo el número de fila de la base, pero pude reconstruir la correspondencia: la base del paper es una subsecuencia de los ocupados crudos y la emparejé en orden. Las 5.353.440 filas encontraron su fila original (100%), y las horas y el ingreso por hora que no se usaron para emparejar coinciden en 99,98% a 100%. El archivo con año, mes, `DIRECTORIO`, `SECUENCIA_P`, `ORDEN`, `P7040`, `P7045`, `P7070`, `P6800`, `P6850` e `INGLABO` está en `Robustez_SegundoEmpleo/Datos/entrega_persona_id_segundo_empleo.dta`.

**4. Comparación.** Repetí la columna 4 y el modelo de tamaño por formalidad en las cuatro muestras (completa y asalariados, con y sin segundo empleo). Excluir a quienes declaran segundo empleo sube las primas por tamaño de la muestra completa entre 0,4 y 1,3 pp (de 43,2% a 44,5% en 101+), y en asalariados las mueve entre −0,3 y +0,2 pp. En el modelo por formalidad los cambios van de −0,6 a +1,4 pp y la brecha formal-informal cambia menos de 1,2 pp. No cambia la forma de las primas. Los formales que trabajan solos, que son la referencia de la prima de los formales, son solo 2,2% de quienes declaran segundo empleo; el 43% son informales que trabajan solos.

**Una advertencia.** La base actual (30 de junio) no reproduce exactamente las tablas publicadas: la columna 4 tiene 5.268.514 observaciones y la tabla publicada indica 5.154.213. Las primas replicadas difieren hasta 0,11 pp en la columna 4 y hasta 0,29 pp en el modelo con interacción. Para la comparación usé la base actual en las cuatro muestras. ¿Sabes qué versión de la base produjo las tablas publicadas?

**Pendiente.** Reconstruir `INGLABO` por componentes y decidir la medida de horas. Los detalles están en `Robustez_SegundoEmpleo/Docs/Nota_metodologica.md`.

Jorge
