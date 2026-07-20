* ==============================================================================
* GEIH_2021_2025_NumeroOcupados.do
* Objetivo:
*   Calcular el numero de ocupados (expandido con factor de expansion) para
*   2021 y 2025, a partir de Paper-GEIH_base_modelo_personas_2021_2025_EI.dta.
*
* Notas sobre la base (verificadas antes de escribir este codigo):
*   - A diferencia de las bases GEIH_total crudas de DANE (las que usan los
*     .do de TasasEducacion), esta base "modelo_personas" ya la arma el
*     proyecto (ver CreacionPanelLimpio2021-2025_EI.do /
*     baseConsolidada2021-2025.do) y viene restringida a personas OCUPADAS:
*     posicion_ocupacional_label no tiene NINGUN missing en toda la base
*     (0 de 1,705,585 filas), lo cual es consistente porque esa pregunta del
*     formulario GEIH solo se le hace a quien reporta estar trabajando. Por
*     eso aqui no se filtra por ningun indicador de ocupacion: cada fila ya
*     es una persona ocupada.
*   - fex en esta base ya esta a nivel ANUAL, no hay que dividirlo entre 12
*     (a diferencia de FEX_C/FEX_C18 en las bases crudas mensuales apiladas).
*     Se verifico sumando fex crudo por anio: da ~17.6 millones en 2021 y
*     ~23.0 millones en 2025, del orden de magnitud correcto de los
*     ocupados nacionales que reporta el DANE para esos anios. Si se
*     dividiera entre 12 el numero daria ~1.5 millones, absurdamente bajo.
*   - La base trae tambien una variable "EI" cuyo significado sustantivo no
*     esta documentado en la base misma (se origina de P6430==2 en
*     formalidadDANERobusto.do). No se usa aqui para filtrar; se deja un
*     diagnostico impreso por si se quiere revisar.
*
* Salida:
*   - Imprime el numero de ocupados por anio en consola.
*   - No guarda bases ni exporta archivos.
* ==============================================================================

clear all
set more off

* ------------------------------------------------------------------------------
* 0. Ruta de la base
* ------------------------------------------------------------------------------
local geih_path "Datos/Processed/Paper-GEIH_base_modelo_personas_2021_2025_EI.dta"

capture confirm file "`geih_path'"
if _rc {
    capture confirm file "../`geih_path'"
    if !_rc local geih_path "../`geih_path'"
}

capture confirm file "`geih_path'"
if _rc {
    capture confirm file "../../`geih_path'"
    if !_rc local geih_path "../../`geih_path'"
}

capture confirm file "`geih_path'"
if _rc {
    di as error "No encontre Paper-GEIH_base_modelo_personas_2021_2025_EI.dta."
    di as error "Edita la linea local geih_path con la ruta completa de la base."
    exit 601
}

use anio posicion_ocupacional_label fex EI using "`geih_path'", clear

* ------------------------------------------------------------------------------
* 1. Diagnostico rapido
* ------------------------------------------------------------------------------
di as text "------------------------------------------------------------"
di as text "Diagnostico rapido"
di as text "------------------------------------------------------------"
tab anio
count if missing(posicion_ocupacional_label)
di as text "Filas con posicion_ocupacional_label missing (deberia ser 0): " r(N)
tab EI anio, missing

* ------------------------------------------------------------------------------
* 2. Numero de ocupados por anio (suma de fex, base ya restringida a ocupados)
* ------------------------------------------------------------------------------
preserve
    keep if inlist(anio, 2021, 2025)

    collapse (sum) ocupados = fex (count) observaciones = fex, by(anio)

    format ocupados %12.0fc
    format observaciones %12.0fc
    label variable ocupados "Numero de ocupados (expandido con fex)"
    label variable observaciones "Observaciones (sin expandir)"

    di as text "------------------------------------------------------------"
    di as text "Numero de ocupados, GEIH 2021 y 2025"
    di as text "------------------------------------------------------------"
    list anio observaciones ocupados, noobs separator(0)
restore
