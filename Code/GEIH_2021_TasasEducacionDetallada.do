* ==============================================================================
* GEIH_2021_TasasEducacionDetallada.do
* Objetivo:
*   Calcular tasa de ocupacion, tasa de desempleo y tasa de participacion
*   por logro educativo detallado para GEIH_2021_total.dta.
*
* Metodologia:
*   - Usa PET/PETI como poblacion en edad de trabajar.
*   - Usa FT/PEAI como fuerza de trabajo / PEA.
*   - Usa OCI como ocupados.
*   - Usa DSI como desocupados.
*   - Usa factor de expansion anualizado: FEX_C18 / 12 o FEX_C / 12.
*   - Usa P3042 o P6210 como logro educativo detallado.
*
* Salida:
*   - Imprime la tabla en consola.
*   - No guarda bases ni exporta archivos.
* ==============================================================================

clear all
set more off

* ------------------------------------------------------------------------------
* 0. Ruta de la base
* ------------------------------------------------------------------------------
* Si Stata no encuentra la base automaticamente, cambia esta ruta manualmente.
local geih_path "C:\Users\jorge\Documents\Databases\GEIH\GEIH_2021_total.dta"

capture confirm file "`geih_path'"
if _rc {
    capture confirm file "../GEIH_2021_total.dta"
    if !_rc local geih_path "../GEIH_2021_total.dta"
}

capture confirm file "`geih_path'"
if _rc {
    capture confirm file "../../GEIH_2021_total.dta"
    if !_rc local geih_path "../../GEIH_2021_total.dta"
}

capture confirm file "`geih_path'"
if _rc {
    di as error "No encontre GEIH_2021_total.dta."
    di as error "Edita la linea local geih_path con la ruta completa de la base."
    di as error `"Ejemplo: local geih_path "C:/ruta/a/GEIH_2021_total.dta""'
    exit 601
}

use "`geih_path'", clear

* ------------------------------------------------------------------------------
* 1. Detectar variables GEIH 2021 usadas
* ------------------------------------------------------------------------------
local pet_var ""
foreach v in PET PETI {
    capture confirm variable `v'
    if !_rc & "`pet_var'" == "" local pet_var "`v'"
}

local pea_var ""
foreach v in FT PEAI {
    capture confirm variable `v'
    if !_rc & "`pea_var'" == "" local pea_var "`v'"
}

local ocupado_var ""
foreach v in OCI OCU OCUPADO ocupado {
    capture confirm variable `v'
    if !_rc & "`ocupado_var'" == "" local ocupado_var "`v'"
}

local desocupado_var ""
foreach v in DSI DESOCUPADO desocupado {
    capture confirm variable `v'
    if !_rc & "`desocupado_var'" == "" local desocupado_var "`v'"
}

local fex_var ""
foreach v in FEX_C18 FEX_C fex_c18 fex {
    capture confirm numeric variable `v'
    if !_rc & "`fex_var'" == "" local fex_var "`v'"
}

local educ_var ""
foreach v in P3042 P6210 p3042 p6210 {
    capture confirm variable `v'
    if !_rc & "`educ_var'" == "" local educ_var "`v'"
}

foreach loc in pet_var pea_var ocupado_var desocupado_var fex_var educ_var {
    if "``loc''" == "" {
        di as error "No pude detectar la variable requerida: `loc'"
        exit 111
    }
}

di as result "Variables usadas:"
di as text "  PET:              `pet_var'"
di as text "  PEA / FT:         `pea_var'"
di as text "  Ocupado:          `ocupado_var'"
di as text "  Desocupado:       `desocupado_var'"
di as text "  Factor expansion: `fex_var'"
di as text "  Educacion:        `educ_var'"

di as text "------------------------------------------------------------"
di as text "Diagnostico rapido de variables laborales y educacion"
di as text "------------------------------------------------------------"
tab `pet_var', missing
tab `pea_var', missing
tab `ocupado_var', missing
tab `desocupado_var', missing
tab `educ_var', missing
capture tab `pet_var' `pea_var', missing

capture drop fex_anual
gen double fex_anual = `fex_var' / 12
label variable fex_anual "Factor de expansion anualizado = factor mensual / 12"

* ------------------------------------------------------------------------------
* 2. Logro educativo detallado
* ------------------------------------------------------------------------------
capture drop educ_det
gen byte educ_det = .

tempvar educ_num
capture confirm numeric variable `educ_var'
if !_rc {
    gen double `educ_num' = `educ_var'
}
else {
    destring `educ_var', gen(`educ_num') force
}

* Codificacion oficial de P3042 (diccionario DANE, modulo F1 GEIH,
* https://microdatos.dane.gov.co/index.php/catalog/853/data-dictionary/F1):
*   1 Ninguno, 2 Preescolar, 3 Basica primaria, 4 Basica secundaria,
*   5 Media academica, 6 Media tecnica, 7 Normalista,
*   8 Tecnica profesional, 9 Tecnologica, 10 Universitaria,
*   11 Especializacion, 12 Maestria, 13 Doctorado, 99 No sabe/no informa.
* OJO: si `educ_var' termino siendo P6210 (no P3042), esta codificacion NO
* aplica - P6210 solo tiene 6 niveles (1 ninguno...6 superior) + 9 NS/NR;
* revisar el "Educacion: `educ_var'" que imprime el diagnostico antes de
* confiar en esta tabla.
* Ninguno y Preescolar se agrupan en una sola categoria (poblaciones chicas,
* ambas sin escolaridad relevante); el resto de niveles queda 1 a 1, corridos
* una posicion para dejar espacio a Doctorado (codigo 13), que antes quedaba
* excluido por el rango inrange(1,12).
replace educ_det = 1  if inlist(`educ_num', 1, 2)
replace educ_det = 2  if `educ_num' == 3
replace educ_det = 3  if `educ_num' == 4
replace educ_det = 4  if `educ_num' == 5
replace educ_det = 5  if `educ_num' == 6
replace educ_det = 6  if `educ_num' == 7
replace educ_det = 7  if `educ_num' == 8
replace educ_det = 8  if `educ_num' == 9
replace educ_det = 9  if `educ_num' == 10
replace educ_det = 10 if `educ_num' == 11
replace educ_det = 11 if `educ_num' == 12
replace educ_det = 12 if `educ_num' == 13

label define educ_det_lbl ///
    1  "Preescolar o ninguno" ///
    2  "Basica primaria" ///
    3  "Basica secundaria" ///
    4  "Media academica" ///
    5  "Media tecnica" ///
    6  "Normalista" ///
    7  "Tecnica profesional" ///
    8  "Tecnologica" ///
    9  "Universitaria" ///
    10 "Especializacion" ///
    11 "Maestria" ///
    12 "Doctorado", replace

label values educ_det educ_det_lbl
label variable educ_det "Logro educativo detallado"

* ------------------------------------------------------------------------------
* 3. Crear indicadores 0/1 y calcular tasas con factor anualizado
* ------------------------------------------------------------------------------
capture drop pet_ind pea_ind ocupado_ind desocupado_ind

gen byte pet_ind = (`pet_var' == 1) if !missing(`pet_var')
gen byte pea_ind = (`pea_var' == 1) if !missing(`pea_var')
gen byte ocupado_ind = (`ocupado_var' == 1) if !missing(`ocupado_var')
gen byte desocupado_ind = (`desocupado_var' == 1) if !missing(`desocupado_var')

capture drop pet_exp pea_exp ocupado_exp desocupado_exp obs_count
gen double pet_exp = pet_ind * fex_anual
gen double pea_exp = pea_ind * fex_anual
gen double ocupado_exp = ocupado_ind * fex_anual
gen double desocupado_exp = desocupado_ind * fex_anual
gen double obs_count = 1 if !missing(educ_det)

preserve
    keep if inrange(educ_det, 1, 12)
    keep if !missing(fex_anual)

    collapse ///
        (sum) pet_total = pet_exp ///
              pea_total = pea_exp ///
              ocupados_total = ocupado_exp ///
              desocupados_total = desocupado_exp ///
              observaciones = obs_count, ///
        by(educ_det)

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
    di as text "Tasas laborales por logro educativo detallado, GEIH 2021"
    di as text "Calculo expandido con factor anualizado: `fex_var' / 12"
    di as text "------------------------------------------------------------"
    list educ_det observaciones pet_total pea_total ocupados_total desocupados_total ///
         tasa_ocupacion tasa_desempleo tasa_participacion, ///
         noobs separator(0)

    di as text "------------------------------------------------------------"
    di as text "Definiciones:"
    di as text "  Tasa de ocupacion     = Ocupados / PET * 100"
    di as text "  Tasa de desempleo     = Desocupados / PEA * 100"
    di as text "  Tasa de participacion = PEA / PET * 100"
    di as text "------------------------------------------------------------"
restore
