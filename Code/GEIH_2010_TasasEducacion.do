* ==============================================================================
* GEIH_2010_TasasEducacion.do
* Objetivo:
*   Calcular tasa de ocupacion, tasa de desempleo y tasa de participacion
*   por tres niveles educativos para GEIH_2010_total.dta.
*
* Importante:
*   - Este codigo usa siempre factor de expansion.
*   - Como la base contiene meses del año, anualiza el factor como FEX_C / 12.
*   - No elimina observaciones por missing en horas, ingresos laborales ni
*     variables similares.
*   - Solo requiere identificar: PET, PEA, ocupados, desocupados, educacion
*     y factor de expansion.
*   - No guarda bases ni exporta archivos. Imprime resultados en consola.
* ==============================================================================

clear all
set more off

* ------------------------------------------------------------------------------
* 0. Ruta de la base
* ------------------------------------------------------------------------------
* Si Stata no encuentra la base automaticamente, cambia esta ruta manualmente.
local geih_path "C:\Users\jorge\Documents\Databases\GEIH\GEIH_2010_total.dta"

capture confirm file "`geih_path'"
if _rc {
    capture confirm file "../GEIH_2010_total.dta"
    if !_rc local geih_path "../GEIH_2010_total.dta"
}

capture confirm file "`geih_path'"
if _rc {
    capture confirm file "../../GEIH_2010_total.dta"
    if !_rc local geih_path "../../GEIH_2010_total.dta"
}

capture confirm file "`geih_path'"
if _rc {
    di as error "No encontre GEIH_2010_total.dta."
    di as error "Edita la linea local geih_path con la ruta completa de la base."
    di as error `"Ejemplo: local geih_path "C:/ruta/a/GEIH_2010_total.dta""'
    exit 601
}

use "`geih_path'", clear

* ------------------------------------------------------------------------------
* 1. Variables GEIH 2010 usadas
* ------------------------------------------------------------------------------
local pet_var       "PETI"
local pea_var       "PEAI"
local ocupado_var   "OCI"
local desocupado_var "DSI"
local fex_var       "FEX_C"
local educ_var      "P6210"

foreach v in `pet_var' `pea_var' `ocupado_var' `desocupado_var' `fex_var' `educ_var' {
    capture confirm variable `v'
    if _rc {
        di as error "No encuentro la variable requerida: `v'"
        exit 111
    }
}

di as result "Variables usadas:"
di as text "  PET:              `pet_var'"
di as text "  PEA:              `pea_var'"
di as text "  Ocupado:          `ocupado_var'"
di as text "  Desocupado:       `desocupado_var'"
di as text "  Factor expansion: `fex_var'"
di as text "  Educacion:        `educ_var'"

di as text "------------------------------------------------------------"
di as text "Diagnostico rapido de variables laborales"
di as text "Si PETI y PEAI fueran iguales para todos, la TGP daria 100%."
di as text "------------------------------------------------------------"
tab `pet_var', missing
tab `pea_var', missing
tab `ocupado_var', missing
tab `desocupado_var', missing
capture tab `pet_var' `pea_var', missing

capture drop fex_anual
gen double fex_anual = `fex_var' / 12
label variable fex_anual "Factor de expansion anualizado = FEX_C / 12"

* ------------------------------------------------------------------------------
* 2. Construir tres niveles educativos desde P6210
* ------------------------------------------------------------------------------
capture drop educ_3niv

gen byte educ_3niv = .
tempvar educ_num
capture confirm numeric variable `educ_var'
if !_rc {
    gen double `educ_num' = `educ_var'
}
else {
    destring `educ_var', gen(`educ_num') force
}

* Codificacion usual GEIH P6210:
* 1 ninguno, 2 preescolar, 3 basica primaria,
* 4 basica secundaria, 5 media, 6 superior/universitaria,
* 9 no sabe/no informa.
replace educ_3niv = 1 if inlist(`educ_num', 1, 2, 3)
replace educ_3niv = 2 if inlist(`educ_num', 4, 5)
replace educ_3niv = 3 if `educ_num' == 6

label define educ3_lbl 1 "Primaria o menos" 2 "Secundaria" 3 "Universitaria o superior", replace
label values educ_3niv educ3_lbl
label variable educ_3niv "Logro educativo comparable, 3 niveles"

* ------------------------------------------------------------------------------
* 3. Crear indicadores 0/1 y calcular tasas con factor de expansion
* ------------------------------------------------------------------------------
capture drop pet_ind pea_ind ocupado_ind desocupado_ind

gen byte pet_ind = (`pet_var' == 1) if !missing(`pet_var')
gen byte pea_ind = (`pea_var' == 1) if !missing(`pea_var')
gen byte ocupado_ind = (`ocupado_var' == 1) if !missing(`ocupado_var')
gen byte desocupado_ind = (`desocupado_var' == 1) if !missing(`desocupado_var')

label variable pet_ind "PET, indicador 0/1"
label variable pea_ind "PEA, indicador 0/1"
label variable ocupado_ind "Ocupado, indicador 0/1"
label variable desocupado_ind "Desocupado, indicador 0/1"

capture drop pet_exp pea_exp ocupado_exp desocupado_exp obs_count
gen double pet_exp = pet_ind * fex_anual
gen double pea_exp = pea_ind * fex_anual
gen double ocupado_exp = ocupado_ind * fex_anual
gen double desocupado_exp = desocupado_ind * fex_anual
gen double obs_count = 1 if !missing(educ_3niv)

preserve
    keep if inlist(educ_3niv, 1, 2, 3)
    keep if !missing(fex_anual)

    collapse ///
        (sum) pet_total = pet_exp ///
              pea_total = pea_exp ///
              ocupados_total = ocupado_exp ///
              desocupados_total = desocupado_exp ///
              observaciones = obs_count, ///
        by(educ_3niv)

    gen double tasa_ocupacion = 100 * ocupados_total / pet_total
    gen double tasa_desempleo = 100 * desocupados_total / pea_total
    gen double tasa_participacion = 100 * pea_total / pet_total

    format tasa_ocupacion tasa_desempleo tasa_participacion %9.2f
    format pet_total pea_total ocupados_total desocupados_total observaciones %12.0fc

    label variable pet_total "PET"
    label variable pea_total "PEA"
    label variable ocupados_total "Ocupados"
    label variable desocupados_total "Desocupados"
    label variable observaciones "Observaciones"
    label variable tasa_ocupacion "Tasa de ocupacion"
    label variable tasa_desempleo "Tasa de desempleo"
    label variable tasa_participacion "Tasa de participacion"

    di as text "------------------------------------------------------------"
    di as text "Tasas laborales por nivel educativo, GEIH 2010"
    di as text "Calculo expandido con factor anualizado: `fex_var' / 12"
    di as text "------------------------------------------------------------"
    list educ_3niv observaciones pet_total pea_total ocupados_total desocupados_total ///
         tasa_ocupacion tasa_desempleo tasa_participacion, ///
         noobs separator(0)

    di as text "------------------------------------------------------------"
    di as text "Definiciones:"
    di as text "  Tasa de ocupacion     = Ocupados / PET * 100"
    di as text "  Tasa de desempleo     = Desocupados / PEA * 100"
    di as text "  Tasa de participacion = PEA / PET * 100"
    di as text "------------------------------------------------------------"
restore
