* ==============================================================================
* GEIH_2024_NumeroOcupadosCINE11.do
* Objetivo:
*   Calcular el numero de ocupados para 2024 por nivel educativo segun la
*   Clasificacion Internacional Normalizada de la Educacion adaptada para
*   Colombia (CINE 11), replicando en Stata la sintaxis SAS que el DANE
*   publica en:
*     DocumentacionAuxiliar/anex-GEIHFLE-2024.xlsx
*       hoja "Código_SAS"  -> sintaxis de construccion de CINE 11
*       hoja "Nivel CINE 11" -> cifras oficiales para comparar (miles de
*                               personas, poblacion ocupada nacional)
*
* Metodologia:
*   - La hoja Código_SAS trae dos sintaxis: una para el "marco 2005-2018"
*     (basada en P6210/P6220) y otra para el "marco 2018" en adelante
*     (basada en P3042/P3042S1/P3043). GEIH_2024_total.dta ya es marco 2018
*     (se verifico que no tiene P6210 ni P6220, y si tiene P3042, P3042S1,
*     P3043), asi que aqui se traduce SOLO la sintaxis del marco 2018.
*   - Usa PET/FT/OCI/DSI (indicadores GEIH estandar) y FEX_C18/12 como
*     factor de expansion anualizado, igual que GEIH_2025_TasasEducacion.do
*     (la base de un solo anio apila 12 meses; ver esa nota alli para el
*     detalle de por que se divide entre 12).
*   - No guarda bases ni exporta archivos. Imprime resultados en consola.
*
* Notas de traduccion SAS -> Stata (para que quede clara la equivalencia):
*   1) El bloque SAS que hace
*        IF p3042s1=0 THEN p3042s1=00; IF p3042s1=1 THEN p3042s1=01; ...
*      es un NO-OP: asignarle 05 a una variable NUMERICA es lo mismo que
*      asignarle 5 (el cero a la izquierda no existe en un numero). No se
*      traduce porque no cambia nada.
*   2) El bloque
*        IF p3042=1 THEN p3042=100; IF p3042=2 THEN p3042=200; ...
*        IF p3042=99 THEN p3042=9900;
*      es exactamente "p3042 * 100" para todos los codigos validos de
*      P3042 (1-14 y 99), asi que se reemplaza por esa multiplicacion en
*      vez de escribir 15 lineas.
*   3) OJO CRITICO con missing: en SAS, un valor numerico missing se trata
*      como "menos infinito" en comparaciones (siempre es menor que
*      cualquier numero). En Stata es al reves: missing (.) se trata como
*      "mas infinito" (siempre mayor que cualquier numero). Por eso:
*        - "p3043 not > 1" en SAS (verdadero si p3043 es missing) se
*          traduce como "p3043<=1 | missing(p3043)", NUNCA como
*          "!(p3043>1)", porque en Stata esa negacion daria el resultado
*          contrario cuando p3043 es missing.
*        - Los rangos "educ>=A and educ<=B" se traducen con inrange(),
*          que en Stata devuelve 0 (no encaja) cuando el valor es missing,
*          replicando el comportamiento de SAS en vez de heredar la regla
*          de "missing es el numero mas grande" de Stata.
*        - La ultima rama ("educ>=9900 or p3043=99") no tiene limite
*          superior para usar inrange(), asi que se protege manualmente
*          con "!missing(educ) & educ>=9900".
*   4) Al final, la sintaxis SAS fuerza cine11 a missing si EDUC es
*      missing, sin importar que haya caido en alguna rama antes. Se
*      replica exactamente igual, como ultimo paso.
* ==============================================================================

clear all
set more off

* ------------------------------------------------------------------------------
* 0. Ruta de la base
* ------------------------------------------------------------------------------
* Si Stata no encuentra la base automaticamente, cambia esta ruta manualmente.
local geih_path "C:\Users\jorge\Documents\Databases\GEIH\GEIH_2024_total.dta"

capture confirm file "`geih_path'"
if _rc {
    capture confirm file "../GEIH_2024_total.dta"
    if !_rc local geih_path "../GEIH_2024_total.dta"
}

capture confirm file "`geih_path'"
if _rc {
    capture confirm file "../../GEIH_2024_total.dta"
    if !_rc local geih_path "../../GEIH_2024_total.dta"
}

capture confirm file "`geih_path'"
if _rc {
    di as error "No encontre GEIH_2024_total.dta."
    di as error "Edita la linea local geih_path con la ruta completa de la base."
    di as error `"Ejemplo: local geih_path "C:/ruta/a/GEIH_2024_total.dta""'
    exit 601
}

use PET FT OCI DSI FEX_C18 P3042 P3042S1 P3043 using "`geih_path'", clear

* ------------------------------------------------------------------------------
* 1. Variables usadas y diagnostico rapido
* ------------------------------------------------------------------------------
local pet_var        "PET"
local pea_var        "FT"
local ocupado_var    "OCI"
local desocupado_var "DSI"
local fex_var        "FEX_C18"

foreach v in `pet_var' `pea_var' `ocupado_var' `desocupado_var' `fex_var' P3042 P3042S1 P3043 {
    capture confirm variable `v'
    if _rc {
        di as error "No encuentro la variable requerida: `v'"
        exit 111
    }
}

di as result "Variables usadas:"
di as text "  PET:              `pet_var'"
di as text "  PEA / FT:         `pea_var'"
di as text "  Ocupado:          `ocupado_var'"
di as text "  Desocupado:       `desocupado_var'"
di as text "  Factor expansion: `fex_var'"
di as text "  Educacion:        P3042 / P3042S1 / P3043 (marco 2018)"

di as text "------------------------------------------------------------"
di as text "Diagnostico rapido"
di as text "------------------------------------------------------------"
tab `ocupado_var', missing
tab P3042, missing
tab P3043, missing

capture drop fex_anual
gen double fex_anual = `fex_var' / 12
label variable fex_anual "Factor de expansion anualizado = FEX_C18 / 12"

* ------------------------------------------------------------------------------
* 2. Construir CINE 11 (marco 2018), traduccion de Código_SAS
* ------------------------------------------------------------------------------

* p3042 * 100 reemplaza el bloque de 15 "IF p3042=X THEN p3042=X*100"
capture drop p3042_100
gen long p3042_100 = P3042 * 100

* EDUC = P3042*100 + P3042S1 (el bloque de ceros a la izquierda de P3042S1
* es un no-op numerico, se omite; ver nota 1 en el encabezado)
capture drop educ
gen double educ = p3042_100 + P3042S1

capture drop cine11
gen byte cine11 = .

replace cine11 = 1  if missing(cine11) & inrange(educ, 100, 304)
replace cine11 = 2  if missing(cine11) & inrange(educ, 305, 403)
replace cine11 = 3  if missing(cine11) & inrange(educ, 404, 705) & (P3043 <= 1 | missing(P3043))
replace cine11 = 4  if missing(cine11) & inrange(educ, 502, 1028) & P3043 == 2
replace cine11 = 4  if missing(cine11) & inrange(educ, 602, 1028) & P3043 == 3
replace cine11 = 4  if missing(cine11) & inrange(educ, 704, 1028) & P3043 == 4
replace cine11 = 5  if missing(cine11) & inrange(educ, 802, 1028) & P3043 == 5
replace cine11 = 6  if missing(cine11) & inrange(educ, 904, 1028) & P3043 == 6
replace cine11 = 7  if missing(cine11) & inrange(educ, 1008, 1306) & P3043 == 7
replace cine11 = 8  if missing(cine11) & inrange(educ, 1102, 1306) & P3043 == 8
replace cine11 = 8  if missing(cine11) & inrange(educ, 1204, 1314) & P3043 == 9
replace cine11 = 8  if missing(cine11) & (!missing(educ) & educ >= 1306) & P3043 == 10
replace cine11 = 99 if missing(cine11) & ((!missing(educ) & educ >= 9900) | P3043 == 99)
replace cine11 = 98 if missing(cine11)
replace cine11 = .  if missing(educ)

label define cine11_lbl ///
    1  "Ninguno" ///
    2  "Básica primaria" ///
    3  "Básica secundaria" ///
    4  "Educación media" ///
    5  "Educación técnica profesional y tecnológica" ///
    6  "Educación técnica profesional y tecnológica" ///
    7  "Universitaria" ///
    8  "Posgrado" ///
    98 "No determinado" ///
    99 "No determinado", replace

label values cine11 cine11_lbl
label variable cine11 "Nivel educativo CINE 11 (marco 2018)"

* Agrupacion "de publicacion", igual a como el DANE presenta la hoja
* "Nivel CINE 11" del anexo (5 y 6 en una sola fila de tecnica/tecnologica,
* 98 y 99 en una sola fila de no determinado). cine11 (sin agrupar) se deja
* intacto en la base por si se necesita el detalle 5 vs. 6, o 98 vs. 99.
capture drop cine11_pub
gen byte cine11_pub = cine11
replace cine11_pub = 5 if cine11 == 6
replace cine11_pub = 98 if cine11 == 99
label values cine11_pub cine11_lbl
label variable cine11_pub "Nivel educativo CINE 11, agrupado como la publicación DANE"

* ------------------------------------------------------------------------------
* 3. Numero de ocupados por CINE 11 (suma de fex_anual, filtrado a OCI==1)
* ------------------------------------------------------------------------------
preserve
    keep if `ocupado_var' == 1

    collapse (sum) ocupados = fex_anual (count) observaciones = fex_anual, by(cine11_pub)

    egen double total_ocupados = total(ocupados)
    gen double porcentaje = 100 * ocupados / total_ocupados

    format ocupados %16.0fc
    format observaciones %12.0fc
    format porcentaje %9.2f

    label variable ocupados "Numero de ocupados (expandido con fex)"
    label variable observaciones "Observaciones (sin expandir)"
    label variable porcentaje "% del total de ocupados"

    di as text "------------------------------------------------------------"
    di as text "Numero de ocupados por nivel CINE 11, GEIH 2024"
    di as text "Calculo expandido con factor anualizado: `fex_var' / 12"
    di as text "------------------------------------------------------------"
    list cine11_pub observaciones ocupados porcentaje, noobs separator(0)

    di as text "------------------------------------------------------------"
    di as text "Referencia oficial DANE (miles de personas, anex-GEIHFLE-2024.xlsx,"
    di as text "hoja 'Nivel CINE 11', poblacion ocupada nacional 2024):"
    di as text "  Ninguno:                                     2,616.782"
    di as text "  Educación básica primaria:                   4,204.970"
    di as text "  Educación básica secundaria:                 1,058.316"
    di as text "  Educación media:                             8,274.002"
    di as text "  Educación técnica profesional y tecnológica: 2,862.412"
    di as text "  Educación universitaria:                     2,774.703"
    di as text "  Postgrado:                                   1,237.824"
    di as text "  No informa:                                      7.034"
    di as text "  Total:                                      23,036.043"
    di as text "------------------------------------------------------------"
restore
