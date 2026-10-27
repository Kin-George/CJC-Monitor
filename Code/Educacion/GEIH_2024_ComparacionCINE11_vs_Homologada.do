* ==============================================================================
* GEIH_2024_ComparacionCINE11_vs_Homologada.do
* Objetivo:
*   Comparar, solo para 2024 y solo con GEIH_2024_total.dta (no la base
*   panel), el numero de ocupados segun dos clasificaciones educativas:
*     1) La clasificacion homologada "vieja" (educ_hom_cod), la de 6
*        niveles que ya se usaba en CreacionPanelLimpio2008-2025.do /
*        GEIH_2025_TasasEducacion.do: Ninguno, Preescolar, Básica
*        primaria, Básica secundaria, Media, Superior o universitaria,
*        No sabe/informa.
*     2) CINE 11 (marco 2018), la clasificacion nueva construida en
*        GEIH_2024_NumeroOcupadosCINE11.do a partir de la sintaxis SAS
*        oficial del DANE (P3042/P3042S1/P3043).
*   Exporta un Excel con 3 hojas: los totales de cada clasificacion por
*   separado, y una tabla cruzada (cuantos ocupados de cada categoria
*   vieja terminan en cada categoria CINE 11).
*
* Metodologia:
*   - Usa OCI para filtrar ocupados y FEX_C18/12 como factor anualizado,
*     igual que los demas .do de 2024.
*   - La clasificacion vieja (educ_hom_cod) se construye directo desde
*     P3042 (2024 no usa NIVEL_MAS_ALTO, se verifico en
*     GEIH_2024_NumeroOcupadosCINE11.do), replicando exactamente la logica
*     de la seccion "3.2 Educación nueva" de CreacionPanelLimpio2008-2025.do.
*   - CINE 11 se construye igual que en GEIH_2024_NumeroOcupadosCINE11.do
*     (ver ese archivo para el detalle de la traduccion SAS->Stata). Ya
*     esta validado contra las cifras oficiales del DANE (calce exacto).
*
* Salida:
*   Outputs/tables/GEIH_2024_ComparacionCINE11_vs_Homologada.xlsx
* ==============================================================================

clear all
set more off

* ------------------------------------------------------------------------------
* 0. Ruta de la base
* ------------------------------------------------------------------------------
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
    exit 601
}

use OCI FEX_C18 P3042 P3042S1 P3043 using "`geih_path'", clear

foreach v in OCI FEX_C18 P3042 P3042S1 P3043 {
    capture confirm variable `v'
    if _rc {
        di as error "No encuentro la variable requerida: `v'"
        exit 111
    }
}

capture drop fex_anual
gen double fex_anual = FEX_C18 / 12
label variable fex_anual "Factor de expansion anualizado = FEX_C18 / 12"

* ------------------------------------------------------------------------------
* 1. Clasificacion homologada "vieja" (educ_hom_cod), 6 niveles
*    Replica exacta de la seccion "3.2 Educación nueva" de
*    CreacionPanelLimpio2008-2025.do, aplicada directo sobre P3042.
* ------------------------------------------------------------------------------
capture drop educ_hom_cod
gen byte educ_hom_cod = .

replace educ_hom_cod = 1 if P3042 == 1
replace educ_hom_cod = 2 if P3042 == 2
replace educ_hom_cod = 3 if P3042 == 3
replace educ_hom_cod = 4 if P3042 == 4
* Media = media académica + media técnica
replace educ_hom_cod = 5 if inlist(P3042, 5, 6)
* Superior = normalista + técnica + tecnológica + universitaria + posgrados
replace educ_hom_cod = 6 if inrange(P3042, 7, 13)
replace educ_hom_cod = 9 if P3042 == 99

label define educ_hom_lbl ///
    1 "Ninguno" ///
    2 "Preescolar" ///
    3 "Básica primaria" ///
    4 "Básica secundaria" ///
    5 "Media" ///
    6 "Superior o universitaria" ///
    9 "No sabe, no informa", replace

label values educ_hom_cod educ_hom_lbl
label variable educ_hom_cod "Nivel educativo homologado (clasificación vieja, 6 niveles)"

* ------------------------------------------------------------------------------
* 2. CINE 11 (marco 2018), igual que GEIH_2024_NumeroOcupadosCINE11.do
* ------------------------------------------------------------------------------
capture drop p3042_100
gen long p3042_100 = P3042 * 100

* OJO: se usa "educ_cine18" y no "educ" a secas porque "educ" es prefijo
* de "educ_hom_cod" (creada en la seccion 1); un "capture drop educ" con
* "educ" sin existir todavia como nombre exacto lo resuelve Stata como
* abreviatura UNICA de "educ_hom_cod" y la borra sin avisar.
capture drop educ_cine18
gen double educ_cine18 = p3042_100 + P3042S1

capture drop cine11
gen byte cine11 = .

replace cine11 = 1  if missing(cine11) & inrange(educ_cine18, 100, 304)
replace cine11 = 2  if missing(cine11) & inrange(educ_cine18, 305, 403)
replace cine11 = 3  if missing(cine11) & inrange(educ_cine18, 404, 705) & (P3043 <= 1 | missing(P3043))
replace cine11 = 4  if missing(cine11) & inrange(educ_cine18, 502, 1028) & P3043 == 2
replace cine11 = 4  if missing(cine11) & inrange(educ_cine18, 602, 1028) & P3043 == 3
replace cine11 = 4  if missing(cine11) & inrange(educ_cine18, 704, 1028) & P3043 == 4
replace cine11 = 5  if missing(cine11) & inrange(educ_cine18, 802, 1028) & P3043 == 5
replace cine11 = 6  if missing(cine11) & inrange(educ_cine18, 904, 1028) & P3043 == 6
replace cine11 = 7  if missing(cine11) & inrange(educ_cine18, 1008, 1306) & P3043 == 7
replace cine11 = 8  if missing(cine11) & inrange(educ_cine18, 1102, 1306) & P3043 == 8
replace cine11 = 8  if missing(cine11) & inrange(educ_cine18, 1204, 1314) & P3043 == 9
replace cine11 = 8  if missing(cine11) & (!missing(educ_cine18) & educ_cine18 >= 1306) & P3043 == 10
replace cine11 = 99 if missing(cine11) & ((!missing(educ_cine18) & educ_cine18 >= 9900) | P3043 == 99)
replace cine11 = 98 if missing(cine11)
replace cine11 = .  if missing(educ_cine18)

* Agrupacion "de publicacion" (igual que GEIH_2024_NumeroOcupadosCINE11.do):
* CINE 5 y 6 juntos en técnica/tecnológica, 98 y 99 juntos en no determinado.
* OJO: el remapeo SOLO colapsa 6->5 y 99->98; 7 (Universitaria) y 8
* (Posgrado) quedan con su valor original sin correr. Por eso las
* etiquetas de cine11_pub_lbl van en 1,2,3,4,5,7,8,98 (sin el 6), no en
* una numeracion 1-7,98 comprimida.
capture drop cine11_pub
gen byte cine11_pub = cine11
replace cine11_pub = 5 if cine11 == 6
replace cine11_pub = 98 if cine11 == 99

label define cine11_pub_lbl ///
    1  "Ninguno" ///
    2  "Básica primaria" ///
    3  "Básica secundaria" ///
    4  "Educación media" ///
    5  "Educación técnica profesional y tecnológica" ///
    7  "Universitaria" ///
    8  "Posgrado" ///
    98 "No determinado", replace

label values cine11_pub cine11_pub_lbl
label variable cine11_pub "Nivel educativo CINE 11 (marco 2018), agrupado como publicación DANE"

* ------------------------------------------------------------------------------
* 3. Numero de ocupados por cada clasificacion, y tabla cruzada
* ------------------------------------------------------------------------------
keep if OCI == 1

local xlsx "Outputs/tables/GEIH_2024_ComparacionCINE11_vs_Homologada.xlsx"
capture mkdir "Outputs"
capture mkdir "Outputs/tables"

* --- Hoja 1: clasificacion homologada vieja ---
preserve
    collapse (sum) ocupados = fex_anual (count) observaciones = fex_anual, by(educ_hom_cod)
    egen double total_ocupados = total(ocupados)
    gen double porcentaje = 100 * ocupados / total_ocupados
    decode educ_hom_cod, gen(categoria)
    order categoria observaciones ocupados porcentaje
    format ocupados %16.0fc
    format porcentaje %9.2f

    di as text "------------------------------------------------------------"
    di as text "Ocupados por clasificación homologada vieja (6 niveles), GEIH 2024"
    di as text "------------------------------------------------------------"
    list categoria observaciones ocupados porcentaje, noobs separator(0)

    export excel categoria observaciones ocupados porcentaje using "`xlsx'", ///
        sheet("Homologada_vieja") firstrow(variables) replace
restore

* --- Hoja 2: CINE 11 ---
preserve
    collapse (sum) ocupados = fex_anual (count) observaciones = fex_anual, by(cine11_pub)
    egen double total_ocupados = total(ocupados)
    gen double porcentaje = 100 * ocupados / total_ocupados
    decode cine11_pub, gen(categoria)
    order categoria observaciones ocupados porcentaje
    format ocupados %16.0fc
    format porcentaje %9.2f

    di as text "------------------------------------------------------------"
    di as text "Ocupados por CINE 11, GEIH 2024"
    di as text "------------------------------------------------------------"
    list categoria observaciones ocupados porcentaje, noobs separator(0)

    export excel categoria observaciones ocupados porcentaje using "`xlsx'", ///
        sheet("CINE11") firstrow(variables) sheetreplace
restore

* --- Hoja 3: tabla cruzada (homologada vieja x CINE 11) ---
preserve
    collapse (sum) ocupados = fex_anual, by(educ_hom_cod cine11_pub)

    reshape wide ocupados, i(educ_hom_cod) j(cine11_pub)

    capture rename ocupados1  Ninguno
    capture rename ocupados2  BasicaPrimaria
    capture rename ocupados3  BasicaSecundaria
    capture rename ocupados4  EducacionMedia
    capture rename ocupados5  TecnicaTecnologica
    capture rename ocupados7  Universitaria
    capture rename ocupados8  Posgrado
    capture rename ocupados98 NoDeterminado

    decode educ_hom_cod, gen(categoria_homologada_vieja)
    order categoria_homologada_vieja
    drop educ_hom_cod

    foreach v of varlist Ninguno-NoDeterminado {
        capture confirm variable `v'
        if !_rc replace `v' = 0 if missing(`v')
    }

    egen double total_fila = rowtotal(Ninguno-NoDeterminado)
    format Ninguno-NoDeterminado total_fila %16.0fc

    di as text "------------------------------------------------------------"
    di as text "Tabla cruzada: clasificación vieja (filas) x CINE 11 (columnas)"
    di as text "------------------------------------------------------------"
    list, noobs separator(0)

    export excel using "`xlsx'", ///
        sheet("CrossTab_Vieja_x_CINE11") firstrow(variables) sheetreplace
restore

di as text "------------------------------------------------------------"
di as text "Excel guardado en:"
di as text "`xlsx'"
di as text "------------------------------------------------------------"
