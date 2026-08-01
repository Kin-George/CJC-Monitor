* ==============================================================================
* GEIH_2010_NumeroOcupadosCINE11.do
* Objetivo:
*   Calcular el numero de ocupados para 2010 por nivel educativo segun la
*   Clasificacion Internacional Normalizada de la Educacion adaptada para
*   Colombia (CINE 11), replicando en Stata la sintaxis SAS que el DANE
*   publica en:
*     DocumentacionAuxiliar/anex-GEIHFLE-2024.xlsx
*       hoja "Código_SAS"  -> sintaxis de construccion de CINE 11
*       hoja "Nivel CINE 11" -> cifras oficiales para comparar
*
* Metodologia:
*   - La hoja Código_SAS trae dos sintaxis: una para el "marco 2005-2018"
*     (basada en P6210/P6210S1/P6220) y otra para el "marco 2018" en
*     adelante (basada en P3042/P3042S1/P3043, usada en
*     GEIH_2024_NumeroOcupadosCINE11.do). GEIH_2010_total.dta corresponde
*     al marco 2005 (se verifico que trae P6210, P6210S1 y P6220), asi que
*     aqui se traduce la sintaxis vieja.
*   - Usa PETI/PEAI/OCI/DSI (igual que GEIH_2010_TasasEducacion.do) y
*     FEX_C/12 como factor de expansion anualizado (la base de un solo
*     anio apila 12 meses).
*   - No guarda bases ni exporta archivos. Imprime resultados en consola.
*
* Notas de traduccion SAS -> Stata (equivalente a las de la version 2024,
* pero con dos diferencias importantes frente al marco 2018):
*   1) EDUC = SUM((P6210*100), P6210S1) usa la funcion SUM() de SAS, que
*      NO es lo mismo que el operador "+": SUM() trata los missing como 0
*      y solo da missing si TODOS los argumentos son missing. El operador
*      "+" en cambio propaga missing si CUALQUIER operando es missing. Por
*      eso NO se puede traducir como "p6210_100 + P6210S1" sin mas; hay
*      que replicar el comportamiento de SUM() a mano (ver seccion 2).
*   2) El codigo SAS del marco 2005 NO tiene, al final, un "if EDUC missing
*      then CINE11 missing" como si tiene el del marco 2018. Aqui, si EDUC
*      es missing, ninguna de las condiciones numericas hace match y cae
*      directo en "Otherwise CINE11=98", igual que en el SAS original; no
*      se agrega ningun ajuste adicional de missing al final.
*   3) Igual que en 2024: los rangos se traducen con inrange() (maneja
*      missing como "no encaja", igual que SAS), y las condiciones sin
*      limite superior se protegen con "!missing(educ) & educ>=X" en vez
*      de depender de que Stata trate missing como "mas infinito".
*      "p6220<=1" en SAS es verdadero tambien cuando p6220 es missing (SAS
*      trata missing como "menos infinito"), asi que se traduce como
*      "P6220<=1 | missing(P6220)", igual que se hizo con p3043 en 2024.
*
* CINE 11 del marco 2005 (distinto del marco 2018): usa los codigos
* 0,1,2,3,5,6,7,98,99 (no existe el 4), con estas etiquetas oficiales:
*   0 Ninguno
*   1 CINE 1 - educación básica primaria
*   2 CINE 2 - educación básica secundaria
*   3 CINE 3 - educación Media
*   5 CINE 5 - educación técnica profesional y tecnológica
*   6 CINE 6 - educación superior
*   7 CINE 7-8 - postgrado
*   98,99 No determinado-No informa
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

use PETI PEAI OCI DSI FEX_C P6210 P6210S1 P6220 using "`geih_path'", clear

* ------------------------------------------------------------------------------
* 1. Variables usadas y diagnostico rapido
* ------------------------------------------------------------------------------
local pet_var        "PETI"
local pea_var        "PEAI"
local ocupado_var    "OCI"
local desocupado_var "DSI"
local fex_var        "FEX_C"

foreach v in `pet_var' `pea_var' `ocupado_var' `desocupado_var' `fex_var' P6210 P6210S1 P6220 {
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
di as text "  Educacion:        P6210 / P6210S1 / P6220 (marco 2005)"

di as text "------------------------------------------------------------"
di as text "Diagnostico rapido"
di as text "------------------------------------------------------------"
tab `ocupado_var', missing
tab P6210, missing
tab P6220, missing

capture drop fex_anual
gen double fex_anual = `fex_var' / 12
label variable fex_anual "Factor de expansion anualizado = FEX_C / 12"

* ------------------------------------------------------------------------------
* 2. Construir CINE 11 (marco 2005), traduccion de Código_SAS
* ------------------------------------------------------------------------------

* EDUC = SUM(P6210*100, P6210S1): replica la funcion SUM() de SAS, que
* trata missing como 0 y solo da missing si AMBOS son missing (ver nota 1
* del encabezado; no es lo mismo que "p6210_100 + P6210S1").
capture drop p6210_100
gen double p6210_100 = P6210 * 100

capture drop educ
gen double educ = p6210_100 + P6210S1
replace educ = p6210_100 if missing(P6210S1) & !missing(p6210_100)
replace educ = P6210S1   if missing(p6210_100) & !missing(P6210S1)
* si P6210 y P6210S1 son ambos missing, educ ya quedo missing arriba

capture drop cine11
gen byte cine11 = .

replace cine11 = 0  if missing(cine11) & inrange(educ, 100, 304)
replace cine11 = 1  if missing(cine11) & inrange(educ, 305, 408)
replace cine11 = 2  if missing(cine11) & inrange(educ, 409, 513) & (P6220 <= 1 | missing(P6220))
replace cine11 = 2  if missing(cine11) & educ == 511 & P6220 == 1
replace cine11 = 3  if missing(cine11) & (!missing(educ) & educ >= 511) & P6220 == 2
replace cine11 = 5  if missing(cine11) & (!missing(educ) & educ >= 601) & P6220 == 3
replace cine11 = 6  if missing(cine11) & (!missing(educ) & educ >= 604) & P6220 == 4
replace cine11 = 7  if missing(cine11) & (!missing(educ) & educ >= 605) & P6220 == 5
replace cine11 = 99 if missing(cine11) & (!missing(educ) & educ >= 500) & P6220 == 9
replace cine11 = 99 if missing(cine11) & (educ == 900 | educ == 999 | P6220 == 9)
replace cine11 = 98 if missing(cine11)
* OJO: a diferencia de 2024, aqui NO se fuerza cine11=. cuando educ es
* missing (el SAS del marco 2005 no lo hace; cae en "Otherwise CINE11=98").

label define cine11_2005_lbl ///
    0  "Ninguno" ///
    1  "CINE 1 - educación básica primaria" ///
    2  "CINE 2 - educación básica secundaria" ///
    3  "CINE 3 - educación Media" ///
    5  "CINE 5 - educación técnica profesional y tecnológica" ///
    6  "CINE 6 - educación superior" ///
    7  "CINE 7-8 - postgrado" ///
    98 "No determinado-No informa" ///
    99 "No determinado-No informa", replace

label values cine11 cine11_2005_lbl
label variable cine11 "Nivel educativo CINE 11 (marco 2005)"

* Agrupacion "de publicacion": 98 y 99 en una sola fila, igual que en la
* hoja "Nivel CINE 11" del anexo. cine11 (sin agrupar) queda intacto en la
* base por si se necesita el detalle 98 vs. 99.
capture drop cine11_pub
gen byte cine11_pub = cine11
replace cine11_pub = 98 if cine11 == 99
label values cine11_pub cine11_2005_lbl
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
    di as text "Numero de ocupados por nivel CINE 11, GEIH 2010"
    di as text "Calculo expandido con factor anualizado: `fex_var' / 12"
    di as text "------------------------------------------------------------"
    list cine11_pub observaciones ocupados porcentaje, noobs separator(0)

    di as text "------------------------------------------------------------"
    di as text "Referencia oficial DANE (miles de personas, anex-GEIHFLE-2024.xlsx,"
    di as text "hoja 'Nivel CINE 11', poblacion ocupada nacional 2010):"
    di as text "  Ninguno:                                     3,945.359"
    di as text "  Educación básica primaria:                   4,962.675"
    di as text "  Educación básica secundaria:                 1,166.957"
    di as text "  Educación media:                             5,261.522"
    di as text "  Educación técnica profesional y tecnológica: 1,307.216"
    di as text "  Educación universitaria:                     1,342.500"
    di as text "  Postgrado:                                     517.162"
    di as text "  No informa:                                     71.385"
    di as text "  Total:                                      18,574.776"
    di as text "------------------------------------------------------------"
restore
