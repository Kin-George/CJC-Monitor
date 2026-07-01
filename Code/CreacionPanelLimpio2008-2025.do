****************************************************
* BASE INDIVIDUAL LIMPIA PARA MODELOS ECONOMÉTRICOS
* GEIH 2008-2025
*
* Unidad:
* persona ocupada
****************************************************

clear all
set more off

cd "C:/Users/jorge/Documents/Databases/GEIH"

capture mkdir "Outputs"
capture mkdir "Outputs/tables"

*====================================================
* 0a. Puente ocupacional CIUO-08 -> CIUO-88
*====================================================
* Desde 2021 la GEIH trae OFICIO_C8, codificado en CIUO-08.
* Para mantener una serie comparable con el OFICIO usado hasta 2019,
* se construye un puente a dos dígitos usando la matriz oficial
* CIUO-88 A.C. vs CIUO-08 A.C. disponible en DocumentacionAuxiliar.

tempfile xwalk_ciuo08_ciuo88_unique xwalk_ciuo08_ciuo88_ambiguous

import excel using ///
    "C:/Users/jorge/Documents/Trabajo-Profesional/Javeriana/DocumentacionAuxiliar/Correlativa_CIUO_88_A_C_vs_CIUO_08_A_C.xlsx", ///
    sheet("Matriz 88 A.C. vs 08 A.C.") cellrange(A6:D1590) clear

rename A ciuo88_raw
rename B ciuo88_desc
rename C ciuo08_raw
rename D ciuo08_desc

foreach v in ciuo88_raw ciuo08_raw {
    capture confirm numeric variable `v'
    if !_rc {
        tostring `v', replace force format(%20.0g)
    }
    replace `v' = upper(strtrim(`v'))
    replace `v' = subinstr(`v', "P", "", .)
    replace `v' = subinstr(`v', " ", "", .)
    replace `v' = "" if `v' == "."
}

replace ciuo88_raw = ciuo88_raw[_n-1] if ciuo88_raw == "" & _n > 1
replace ciuo88_desc = ciuo88_desc[_n-1] if missing(ciuo88_desc) & _n > 1

* La matriz tiene filas jerárquicas de 1, 2, 3 y 4 dígitos.
* Para mapear OFICIO_C8 usamos únicamente códigos completos de 4 dígitos.
keep if regexm(ciuo88_raw, "^[0-9][0-9][0-9][0-9]$")
keep if regexm(ciuo08_raw, "^[0-9][0-9][0-9][0-9]$")

gen long ciuo08_4d_cod = real(ciuo08_raw)
gen byte oficio_ciuo88_2d_cod = real(substr(ciuo88_raw, 1, 2))

drop if missing(ciuo08_4d_cod)
drop if missing(oficio_ciuo88_2d_cod)

bysort ciuo08_4d_cod oficio_ciuo88_2d_cod: gen byte tag_destino = _n == 1
bysort ciuo08_4d_cod: egen oficio_hom_n_destinos = total(tag_destino)

preserve
    keep if oficio_hom_n_destinos > 1
    bysort ciuo08_4d_cod: keep if _n == 1
    gen byte oficio_hom_ambiguo = 1
    rename oficio_hom_n_destinos oficio_hom_n_destinos_amb
    keep ciuo08_4d_cod oficio_hom_ambiguo oficio_hom_n_destinos_amb
    save `xwalk_ciuo08_ciuo88_ambiguous', replace
restore

keep if oficio_hom_n_destinos == 1
bysort ciuo08_4d_cod: keep if _n == 1
keep ciuo08_4d_cod oficio_ciuo88_2d_cod oficio_hom_n_destinos
save `xwalk_ciuo08_ciuo88_unique', replace

use "Outputs/tables/GEIH_consolidada_variables_interes_2008_2025.dta", clear


*====================================================
* 0. Normalizar nombres originales
*====================================================

gen str40 sector_var_u = upper(strtrim(sector_var_original))
gen str40 tamano_var_u = upper(strtrim(tamano_var_original))
gen str40 educ_var_u   = upper(strtrim(educ_var_original))

capture confirm variable oficio_var_original
if _rc {
    gen str40 oficio_var_original = ""
}

gen str40 oficio_var_u = upper(strtrim(oficio_var_original))

*====================================================
* 0b. Verificar y limpiar edad
*====================================================
* La base consolidada actualizada ya debe traer una variable común: edad

capture confirm variable edad

if _rc {
    di as error "No se encontró la variable edad en la base consolidada."
    di as error "Revisa que el script de armonización haya conservado edad."
    exit 111
}

* Si edad viene como string, convertirla a numérica
capture confirm numeric variable edad

if _rc {
    tempvar edad_num
    destring edad, gen(`edad_num') force
    drop edad
    rename `edad_num' edad
}

* Limpiar valores improbables
replace edad = . if edad < 0
replace edad = . if edad > 120

capture drop edad2
gen double edad2 = edad^2

foreach v in depto_cod area_cod posicion_ocupacional_cod {
    
    capture confirm variable `v'
    
    if _rc {
        di as error "No se encontró la variable `v' en la base consolidada."
        di as error "Revisa que el script de armonización haya conservado `v'."
        exit 111
    }
    
    capture confirm numeric variable `v'
    
    if _rc {
        tempvar `v'_num
        destring `v', gen(`v'_num) force
        drop `v'
        rename `v'_num `v'
    }
}

capture confirm variable oficio_cod
if _rc {
    gen double oficio_cod = .
}

capture confirm numeric variable oficio_cod
if _rc {
    tempvar oficio_cod_num
    destring oficio_cod, gen(`oficio_cod_num') force
    drop oficio_cod
    rename `oficio_cod_num' oficio_cod
}

* Limpiar códigos imposibles o vacíos
replace depto_cod = . if depto_cod <= 0
replace area_cod = . if area_cod <= 0
replace posicion_ocupacional_cod = . if !inrange(posicion_ocupacional_cod, 1, 9)
replace oficio_cod = . if oficio_cod < 0

label define posicion_ocupacional_lbl ///
    1 "Obrero o empleado de empresa particular" ///
    2 "Obrero o empleado del gobierno" ///
    3 "Empleado doméstico" ///
    4 "Trabajador por cuenta propia" ///
    5 "Patrón o empleador" ///
    6 "Trabajador familiar sin remuneración" ///
    7 "Trabajador sin remuneración en otros hogares" ///
    8 "Jornalero o peón" ///
    9 "Otro", replace

label values posicion_ocupacional_cod posicion_ocupacional_lbl

* Área: si la codificación viene como 5/8, esto ayuda a leerla.
* Si tu base usa otra codificación, igual se conserva el código numérico.
label define area_lbl ///
    1 "Cabecera" ///
    2 "Resto" ///
    5 "Cabecera" ///
    8 "Centro poblado y rural disperso", replace

label values area_cod area_lbl

*====================================================
* 0c. Homologación ocupacional: OFICIO / CNO antiguo
*====================================================
* 2008-2019: OFICIO ya viene en la codificación antigua usada por la GEIH.
* 2021-2025: OFICIO_C8 viene en CIUO-08 a 4 dígitos; se traduce a una
* categoría de dos dígitos comparable usando la matriz CIUO-08 -> CIUO-88.
* Si un código CIUO-08 tiene más de un destino de dos dígitos, se marca
* como ambiguo y no se fuerza una categoría única.

gen double oficio_cod_original = oficio_cod
label variable oficio_cod_original "Código de oficio original preservado por año"

gen long ciuo08_4d_cod = .
replace ciuo08_4d_cod = round(oficio_cod_original) ///
    if inrange(anio, 2021, 2025) & inrange(round(oficio_cod_original), 1000, 9999)

merge m:1 ciuo08_4d_cod using `xwalk_ciuo08_ciuo88_unique', ///
    keep(master match) nogen

merge m:1 ciuo08_4d_cod using `xwalk_ciuo08_ciuo88_ambiguous', ///
    keep(master match) nogen

replace oficio_hom_ambiguo = 0 if missing(oficio_hom_ambiguo)
replace oficio_hom_n_destinos = oficio_hom_n_destinos_amb ///
    if oficio_hom_ambiguo == 1 & !missing(oficio_hom_n_destinos_amb)
drop oficio_hom_n_destinos_amb
replace oficio_hom_n_destinos = 0 if missing(oficio_hom_n_destinos) & !missing(ciuo08_4d_cod)

gen byte oficio_cno70_2d_hom_cod = .

replace oficio_cno70_2d_hom_cod = round(oficio_cod_original) ///
    if inrange(anio, 2008, 2019) & inrange(round(oficio_cod_original), 0, 99)

replace oficio_cno70_2d_hom_cod = floor(round(oficio_cod_original) / 100) ///
    if inrange(anio, 2008, 2019) & inrange(round(oficio_cod_original), 100, 9999)

replace oficio_cno70_2d_hom_cod = oficio_ciuo88_2d_cod ///
    if inrange(anio, 2021, 2025) & oficio_hom_ambiguo == 0

replace oficio_cno70_2d_hom_cod = . if !inrange(oficio_cno70_2d_hom_cod, 0, 99)

gen byte oficio_hom_fuente = .
replace oficio_hom_fuente = 1 if inrange(anio, 2008, 2019) & !missing(oficio_cno70_2d_hom_cod)
replace oficio_hom_fuente = 2 if inrange(anio, 2021, 2025) & !missing(oficio_cno70_2d_hom_cod)

label define oficio_hom_fuente_lbl ///
    1 "OFICIO original 2008-2019" ///
    2 "OFICIO_C8 homologado 2021-2025", replace

label values oficio_hom_fuente oficio_hom_fuente_lbl

label variable ciuo08_4d_cod "Código CIUO-08 a 4 dígitos original, 2021-2025"
label variable oficio_ciuo88_2d_cod "Destino CIUO-88 a 2 dígitos desde matriz oficial"
label variable oficio_hom_ambiguo "CIUO-08 con más de un destino CIUO-88 a 2 dígitos"
label variable oficio_hom_n_destinos "Número de destinos CIUO-88 a 2 dígitos para CIUO-08"
label variable oficio_cno70_2d_hom_cod "Oficio/CNO antiguo a 2 dígitos homologado"
label variable oficio_hom_fuente "Fuente de la homologación ocupacional"


*====================================================
* 1. Marcar observaciones válidas sin eliminarlas
*====================================================
* La base final debe conservar las personas ocupadas aunque tengan
* missings en variables usadas después en modelos o descriptivas.
* Por eso aquí no se filtra por ingreso, horas, factor ni variables
* de control. Solo se crean banderas para que R/Python puedan decidir
* posteriormente qué muestra usar en cada cálculo específico.

gen byte muestra_ingreso_hora_valida = ///
    ingreso_hora_valido == 1 & ///
    !missing(ingreso_laboral_hora) & ///
    ingreso_laboral_hora > 0

gen byte muestra_factor_valido = ///
    !missing(factor_expansion_anual) & ///
    factor_expansion_anual > 0

gen byte muestra_anio_valido = ///
    !missing(anio) & ///
    inrange(anio, 2008, 2025)

gen byte muestra_productividad_valida = ///
    muestra_ingreso_hora_valida == 1 & ///
    muestra_factor_valido == 1 & ///
    muestra_anio_valido == 1


*====================================================
* 2. Homologar tamaño de empresa
*====================================================
* 1 Solo
* 2 2-3
* 3 4-5
* 4 6-10
* 5 11-19
* 6 20-30
* 7 31-50
* 8 51-100
* 9 101+

gen byte tamano_hom_cod = .

replace tamano_hom_cod = 1 if tamano_empresa_cod == 1
replace tamano_hom_cod = 2 if tamano_empresa_cod == 2
replace tamano_hom_cod = 3 if tamano_empresa_cod == 3
replace tamano_hom_cod = 4 if tamano_empresa_cod == 4
replace tamano_hom_cod = 5 if tamano_empresa_cod == 5
replace tamano_hom_cod = 6 if tamano_empresa_cod == 6
replace tamano_hom_cod = 7 if tamano_empresa_cod == 7
replace tamano_hom_cod = 8 if tamano_empresa_cod == 8

* P6870: 9 = 101+
replace tamano_hom_cod = 9 if tamano_var_u == "P6870" & tamano_empresa_cod == 9

* P3069: 9 = 101-200, 10 = 201+
* Para modelos ambas quedan como 101+
replace tamano_hom_cod = 9 if tamano_var_u == "P3069" & inlist(tamano_empresa_cod, 9, 10)

label define tamano_hom_lbl ///
    1 "Solo" ///
    2 "2-3" ///
    3 "4-5" ///
    4 "6-10" ///
    5 "11-19" ///
    6 "20-30" ///
    7 "31-50" ///
    8 "51-100" ///
    9 "101+", replace

label values tamano_hom_cod tamano_hom_lbl


*====================================================
* 3. Homologar educación
*====================================================
* 1 Ninguno
* 2 Preescolar
* 3 Básica primaria
* 4 Básica secundaria
* 5 Media
* 6 Superior o universitaria
* 9 No sabe, no informa

gen byte educ_hom_cod = .

*----------------------------------------------------
* 3.1 Educación antigua: P6210 / NIVEL_MAS_ALTO
* Principalmente 2008-2019
*----------------------------------------------------

replace educ_hom_cod = 1 if inlist(educ_var_u, "P6210", "NIVEL_MAS_ALTO") ///
    & inrange(anio, 2008, 2019) ///
    & educacion_cod == 1

replace educ_hom_cod = 2 if inlist(educ_var_u, "P6210", "NIVEL_MAS_ALTO") ///
    & inrange(anio, 2008, 2019) ///
    & educacion_cod == 2

replace educ_hom_cod = 3 if inlist(educ_var_u, "P6210", "NIVEL_MAS_ALTO") ///
    & inrange(anio, 2008, 2019) ///
    & educacion_cod == 3

replace educ_hom_cod = 4 if inlist(educ_var_u, "P6210", "NIVEL_MAS_ALTO") ///
    & inrange(anio, 2008, 2019) ///
    & educacion_cod == 4

replace educ_hom_cod = 5 if inlist(educ_var_u, "P6210", "NIVEL_MAS_ALTO") ///
    & inrange(anio, 2008, 2019) ///
    & educacion_cod == 5

replace educ_hom_cod = 6 if inlist(educ_var_u, "P6210", "NIVEL_MAS_ALTO") ///
    & inrange(anio, 2008, 2019) ///
    & educacion_cod == 6

replace educ_hom_cod = 9 if inlist(educ_var_u, "P6210", "NIVEL_MAS_ALTO") ///
    & inrange(anio, 2008, 2019) ///
    & inlist(educacion_cod, 7, 9, 99)


*----------------------------------------------------
* 3.2 Educación nueva: P3042 y NIVEL_MAS_ALTO reciente
* Especialmente años 2021-2025
*----------------------------------------------------

replace educ_hom_cod = 1 if ///
    (educ_var_u == "P3042" | (educ_var_u == "NIVEL_MAS_ALTO" & anio >= 2021)) ///
    & educacion_cod == 1

replace educ_hom_cod = 2 if ///
    (educ_var_u == "P3042" | (educ_var_u == "NIVEL_MAS_ALTO" & anio >= 2021)) ///
    & educacion_cod == 2

replace educ_hom_cod = 3 if ///
    (educ_var_u == "P3042" | (educ_var_u == "NIVEL_MAS_ALTO" & anio >= 2021)) ///
    & educacion_cod == 3

replace educ_hom_cod = 4 if ///
    (educ_var_u == "P3042" | (educ_var_u == "NIVEL_MAS_ALTO" & anio >= 2021)) ///
    & educacion_cod == 4

* Media = media académica + media técnica
replace educ_hom_cod = 5 if ///
    (educ_var_u == "P3042" | (educ_var_u == "NIVEL_MAS_ALTO" & anio >= 2021)) ///
    & inlist(educacion_cod, 5, 6)

* Superior = normalista + técnica + tecnológica + universitaria + posgrados
replace educ_hom_cod = 6 if ///
    (educ_var_u == "P3042" | (educ_var_u == "NIVEL_MAS_ALTO" & anio >= 2021)) ///
    & inrange(educacion_cod, 7, 13)

replace educ_hom_cod = 9 if ///
    (educ_var_u == "P3042" | (educ_var_u == "NIVEL_MAS_ALTO" & anio >= 2021)) ///
    & inlist(educacion_cod, 99)

label define educ_hom_lbl ///
    1 "Ninguno" ///
    2 "Preescolar" ///
    3 "Básica primaria" ///
    4 "Básica secundaria" ///
    5 "Media" ///
    6 "Superior o universitaria" ///
    9 "No sabe, no informa", replace

label values educ_hom_cod educ_hom_lbl


*====================================================
* 4. Formalidad
*====================================================
* 1 Formal = cotiza a pensión
* 2 Informal = no cotiza a pensión
* 3 Pensionado ocupado

gen byte formalidad_cod = .

replace formalidad_cod = 1 if cotiza_pension_cod == 1
replace formalidad_cod = 2 if cotiza_pension_cod == 2
replace formalidad_cod = 3 if cotiza_pension_cod == 3

label define formalidad_lbl ///
    1 "Formal" ///
    2 "Informal" ///
    3 "Pensionado ocupado", replace

label values formalidad_cod formalidad_lbl


*====================================================
* 5. Sexo
*====================================================

gen byte sexo_hom_cod = .

replace sexo_hom_cod = 1 if sexo_cod == 1
replace sexo_hom_cod = 2 if sexo_cod == 2

label define sexo_hom_lbl ///
    1 "Hombre" ///
    2 "Mujer", replace

label values sexo_hom_cod sexo_hom_lbl


*====================================================
* 6. Sector homologado CIIU Rev. 3 / Rev. 4
*====================================================

gen byte sector_hom_cod = .

*-------------------------------
* Rev. 3: RAMA2D
*-------------------------------

replace sector_hom_cod = 1  if sector_var_u == "RAMA2D" & inlist(sector_cod, 1, 2, 5)
replace sector_hom_cod = 2  if sector_var_u == "RAMA2D" & inrange(sector_cod, 10, 14)
replace sector_hom_cod = 3  if sector_var_u == "RAMA2D" & inrange(sector_cod, 15, 37)
replace sector_hom_cod = 4  if sector_var_u == "RAMA2D" & inlist(sector_cod, 40, 41, 90)
replace sector_hom_cod = 5  if sector_var_u == "RAMA2D" & sector_cod == 45
replace sector_hom_cod = 6  if sector_var_u == "RAMA2D" & inrange(sector_cod, 50, 52)
replace sector_hom_cod = 7  if sector_var_u == "RAMA2D" & sector_cod == 55
replace sector_hom_cod = 8  if sector_var_u == "RAMA2D" & inrange(sector_cod, 60, 63)
replace sector_hom_cod = 9  if sector_var_u == "RAMA2D" & inlist(sector_cod, 64, 72)
replace sector_hom_cod = 10 if sector_var_u == "RAMA2D" & inrange(sector_cod, 65, 67)
replace sector_hom_cod = 11 if sector_var_u == "RAMA2D" & inlist(sector_cod, 70, 71, 73, 74)
replace sector_hom_cod = 12 if sector_var_u == "RAMA2D" & sector_cod == 75
replace sector_hom_cod = 13 if sector_var_u == "RAMA2D" & sector_cod == 80
replace sector_hom_cod = 14 if sector_var_u == "RAMA2D" & sector_cod == 85
replace sector_hom_cod = 15 if sector_var_u == "RAMA2D" & inrange(sector_cod, 91, 93)
replace sector_hom_cod = 16 if sector_var_u == "RAMA2D" & inrange(sector_cod, 95, 97)
replace sector_hom_cod = 17 if sector_var_u == "RAMA2D" & sector_cod == 99


*-------------------------------
* Rev. 4: RAMA2D_R4
*-------------------------------

replace sector_hom_cod = 1  if sector_var_u == "RAMA2D_R4" & inrange(sector_cod, 1, 3)
replace sector_hom_cod = 2  if sector_var_u == "RAMA2D_R4" & inrange(sector_cod, 5, 9)
replace sector_hom_cod = 3  if sector_var_u == "RAMA2D_R4" & inrange(sector_cod, 10, 33)
replace sector_hom_cod = 4  if sector_var_u == "RAMA2D_R4" & inrange(sector_cod, 35, 39)
replace sector_hom_cod = 5  if sector_var_u == "RAMA2D_R4" & inrange(sector_cod, 41, 43)
replace sector_hom_cod = 6  if sector_var_u == "RAMA2D_R4" & inrange(sector_cod, 45, 47)
replace sector_hom_cod = 7  if sector_var_u == "RAMA2D_R4" & inrange(sector_cod, 55, 56)
replace sector_hom_cod = 8  if sector_var_u == "RAMA2D_R4" & inrange(sector_cod, 49, 53)
replace sector_hom_cod = 9  if sector_var_u == "RAMA2D_R4" & inrange(sector_cod, 58, 63)
replace sector_hom_cod = 10 if sector_var_u == "RAMA2D_R4" & inrange(sector_cod, 64, 66)
replace sector_hom_cod = 11 if sector_var_u == "RAMA2D_R4" & ///
    (sector_cod == 68 | inrange(sector_cod, 69, 75) | inrange(sector_cod, 77, 82))
replace sector_hom_cod = 12 if sector_var_u == "RAMA2D_R4" & sector_cod == 84
replace sector_hom_cod = 13 if sector_var_u == "RAMA2D_R4" & sector_cod == 85
replace sector_hom_cod = 14 if sector_var_u == "RAMA2D_R4" & inrange(sector_cod, 86, 88)
replace sector_hom_cod = 15 if sector_var_u == "RAMA2D_R4" & inrange(sector_cod, 90, 96)
replace sector_hom_cod = 16 if sector_var_u == "RAMA2D_R4" & inrange(sector_cod, 97, 98)
replace sector_hom_cod = 17 if sector_var_u == "RAMA2D_R4" & sector_cod == 99

label define sector_hom_lbl ///
    1  "Agricultura, ganadería, silvicultura y pesca" ///
    2  "Minas y canteras" ///
    3  "Industrias manufactureras" ///
    4  "Electricidad, gas, agua y saneamiento" ///
    5  "Construcción" ///
    6  "Comercio y reparación" ///
    7  "Alojamiento y servicios de comida" ///
    8  "Transporte y almacenamiento" ///
    9  "Información y comunicaciones" ///
    10 "Actividades financieras y de seguros" ///
    11 "Inmobiliarias, profesionales y administrativas" ///
    12 "Administración pública y defensa" ///
    13 "Educación" ///
    14 "Salud y asistencia social" ///
    15 "Artes, recreación y otros servicios" ///
    16 "Hogares como empleadores" ///
    17 "Organizaciones extraterritoriales", replace

label values sector_hom_cod sector_hom_lbl

*====================================================
* 6b. Subrama detallada homologada desde RAMA4D
*====================================================
* Objetivo:
* - Mantener sector_hom_cod sin cambios.
* - Crear una clasificación más desagregada y comparable
*   usando RAMA4D / RAMA4D_R4.
*
* Variables creadas:
* - rama4d
* - rama4d_clase
* - rama3d
* - rama4d_div
* - ciiu_revision_rama4d
* - subrama_det_cod
*====================================================

capture confirm variable rama4d_cod

if _rc {
    di as error "No se encontró rama4d_cod en la base consolidada."
    di as error "Primero debes actualizar el script de consolidación para conservar RAMA4D/RAMA4D_R4."
    exit 111
}

capture confirm variable rama4d_var_original

if _rc {
    di as error "No se encontró rama4d_var_original en la base consolidada."
    di as error "Primero debes actualizar el script de consolidación para conservar el nombre original de RAMA4D."
    exit 111
}

capture drop rama4d_var_u
gen str40 rama4d_var_u = upper(strtrim(rama4d_var_original))

capture drop rama4d
gen int rama4d = rama4d_cod
replace rama4d = . if rama4d <= 0

capture drop rama4d_clase
gen str4 rama4d_clase = string(rama4d, "%04.0f") if !missing(rama4d)

capture drop rama3d
gen int rama3d = floor(rama4d / 10) if !missing(rama4d)

capture drop rama4d_div
gen int rama4d_div = floor(rama4d / 100) if !missing(rama4d)

capture drop ciiu_revision_rama4d
gen str20 ciiu_revision_rama4d = ""

replace ciiu_revision_rama4d = "CIIU Rev. 3" ///
    if inlist(rama4d_var_u, "RAMA4D", "RAMA4D_R3")

replace ciiu_revision_rama4d = "CIIU Rev. 4" ///
    if rama4d_var_u == "RAMA4D_R4"


*====================================================
* 6c. Clasificación detallada homologada
*====================================================

capture drop subrama_det_cod
gen byte subrama_det_cod = .


*----------------------------------------------------
* CIIU Rev. 3 / Rev. 3.1
*----------------------------------------------------

* Agricultura, silvicultura y pesca
replace subrama_det_cod = 1  if inlist(rama4d_var_u, "RAMA4D", "RAMA4D_R3") & rama4d_div == 1
replace subrama_det_cod = 2  if inlist(rama4d_var_u, "RAMA4D", "RAMA4D_R3") & rama4d_div == 2
replace subrama_det_cod = 3  if inlist(rama4d_var_u, "RAMA4D", "RAMA4D_R3") & rama4d_div == 5

* Minas
replace subrama_det_cod = 4  if inlist(rama4d_var_u, "RAMA4D", "RAMA4D_R3") & inlist(rama4d_div, 10, 11)
replace subrama_det_cod = 5  if inlist(rama4d_var_u, "RAMA4D", "RAMA4D_R3") & inrange(rama4d_div, 12, 14)

* Manufactura
replace subrama_det_cod = 6  if inlist(rama4d_var_u, "RAMA4D", "RAMA4D_R3") & inrange(rama4d_div, 15, 16)
replace subrama_det_cod = 7  if inlist(rama4d_var_u, "RAMA4D", "RAMA4D_R3") & inrange(rama4d_div, 17, 18)
replace subrama_det_cod = 8  if inlist(rama4d_var_u, "RAMA4D", "RAMA4D_R3") & rama4d_div == 19
replace subrama_det_cod = 9  if inlist(rama4d_var_u, "RAMA4D", "RAMA4D_R3") & inrange(rama4d_div, 20, 21)
replace subrama_det_cod = 10 if inlist(rama4d_var_u, "RAMA4D", "RAMA4D_R3") & rama4d_div == 22
replace subrama_det_cod = 11 if inlist(rama4d_var_u, "RAMA4D", "RAMA4D_R3") & inrange(rama4d_div, 23, 24)
replace subrama_det_cod = 12 if inlist(rama4d_var_u, "RAMA4D", "RAMA4D_R3") & inrange(rama4d_div, 25, 26)
replace subrama_det_cod = 13 if inlist(rama4d_var_u, "RAMA4D", "RAMA4D_R3") & inrange(rama4d_div, 27, 28)
replace subrama_det_cod = 14 if inlist(rama4d_var_u, "RAMA4D", "RAMA4D_R3") & inrange(rama4d_div, 29, 33)
replace subrama_det_cod = 15 if inlist(rama4d_var_u, "RAMA4D", "RAMA4D_R3") & inrange(rama4d_div, 34, 35)
replace subrama_det_cod = 16 if inlist(rama4d_var_u, "RAMA4D", "RAMA4D_R3") & inrange(rama4d_div, 36, 37)

* Servicios públicos
replace subrama_det_cod = 17 if inlist(rama4d_var_u, "RAMA4D", "RAMA4D_R3") & rama4d_div == 40
replace subrama_det_cod = 18 if inlist(rama4d_var_u, "RAMA4D", "RAMA4D_R3") & inlist(rama4d_div, 41, 90)

* Construcción
replace subrama_det_cod = 19 if inlist(rama4d_var_u, "RAMA4D", "RAMA4D_R3") & rama4d_div == 45

* Comercio
replace subrama_det_cod = 20 if inlist(rama4d_var_u, "RAMA4D", "RAMA4D_R3") & rama4d_div == 50
replace subrama_det_cod = 21 if inlist(rama4d_var_u, "RAMA4D", "RAMA4D_R3") & rama4d_div == 51
replace subrama_det_cod = 22 if inlist(rama4d_var_u, "RAMA4D", "RAMA4D_R3") & rama4d_div == 52

* Alojamiento y comida
replace subrama_det_cod = 23 if inlist(rama4d_var_u, "RAMA4D", "RAMA4D_R3") & rama3d == 551
replace subrama_det_cod = 24 if inlist(rama4d_var_u, "RAMA4D", "RAMA4D_R3") & inlist(rama3d, 552, 553)

* Transporte, correo y telecomunicaciones
replace subrama_det_cod = 25 if inlist(rama4d_var_u, "RAMA4D", "RAMA4D_R3") & rama4d_div == 60
replace subrama_det_cod = 26 if inlist(rama4d_var_u, "RAMA4D", "RAMA4D_R3") & inlist(rama4d_div, 61, 62)
replace subrama_det_cod = 27 if inlist(rama4d_var_u, "RAMA4D", "RAMA4D_R3") & rama4d_div == 63
replace subrama_det_cod = 27 if inlist(rama4d_var_u, "RAMA4D", "RAMA4D_R3") & rama3d == 641
replace subrama_det_cod = 28 if inlist(rama4d_var_u, "RAMA4D", "RAMA4D_R3") & rama3d == 642

* Información y comunicaciones más desagregado
* Editorial se reubica como contenido para comparabilidad con Rev. 4
replace subrama_det_cod = 29 if inlist(rama4d_var_u, "RAMA4D", "RAMA4D_R3") & rama3d == 221

* Audiovisual y noticias
replace subrama_det_cod = 30 if inlist(rama4d_var_u, "RAMA4D", "RAMA4D_R3") ///
    & inlist(rama4d, 9211, 9212, 9213, 9220)

* Software, informática y sistemas
replace subrama_det_cod = 31 if inlist(rama4d_var_u, "RAMA4D", "RAMA4D_R3") ///
    & rama4d_div == 72 ///
    & !inlist(rama3d, 724)

* Servicios de información y bases de datos
replace subrama_det_cod = 32 if inlist(rama4d_var_u, "RAMA4D", "RAMA4D_R3") ///
    & rama3d == 724

* Finanzas
replace subrama_det_cod = 33 if inlist(rama4d_var_u, "RAMA4D", "RAMA4D_R3") & rama4d_div == 65
replace subrama_det_cod = 34 if inlist(rama4d_var_u, "RAMA4D", "RAMA4D_R3") & inlist(rama4d_div, 66, 67)

* Inmobiliarias y servicios empresariales
replace subrama_det_cod = 35 if inlist(rama4d_var_u, "RAMA4D", "RAMA4D_R3") & rama4d_div == 70
replace subrama_det_cod = 36 if inlist(rama4d_var_u, "RAMA4D", "RAMA4D_R3") ///
    & rama4d_div == 74 ///
    & inlist(rama3d, 741, 742, 743)

replace subrama_det_cod = 37 if inlist(rama4d_var_u, "RAMA4D", "RAMA4D_R3") & rama4d_div == 73

replace subrama_det_cod = 38 if inlist(rama4d_var_u, "RAMA4D", "RAMA4D_R3") ///
    & (rama4d_div == 71 | (rama4d_div == 74 & rama3d == 749))

* Administración pública, educación, salud y cuidado
replace subrama_det_cod = 39 if inlist(rama4d_var_u, "RAMA4D", "RAMA4D_R3") & rama4d_div == 75
replace subrama_det_cod = 40 if inlist(rama4d_var_u, "RAMA4D", "RAMA4D_R3") & rama4d_div == 80
replace subrama_det_cod = 41 if inlist(rama4d_var_u, "RAMA4D", "RAMA4D_R3") & inlist(rama3d, 851, 852)
replace subrama_det_cod = 42 if inlist(rama4d_var_u, "RAMA4D", "RAMA4D_R3") & rama3d == 853

* Artes, recreación y otros servicios
replace subrama_det_cod = 43 if inlist(rama4d_var_u, "RAMA4D", "RAMA4D_R3") ///
    & (inlist(rama3d, 923, 924) | inlist(rama4d, 9214, 9219))

replace subrama_det_cod = 44 if inlist(rama4d_var_u, "RAMA4D", "RAMA4D_R3") ///
    & (rama4d_div == 91 | rama3d == 930)

* Hogares y extraterritoriales
replace subrama_det_cod = 45 if inlist(rama4d_var_u, "RAMA4D", "RAMA4D_R3") & inrange(rama4d_div, 95, 97)
replace subrama_det_cod = 46 if inlist(rama4d_var_u, "RAMA4D", "RAMA4D_R3") & rama4d_div == 99


*----------------------------------------------------
* CIIU Rev. 4
*----------------------------------------------------

* Agricultura, silvicultura y pesca
replace subrama_det_cod = 1  if rama4d_var_u == "RAMA4D_R4" & rama4d_div == 1
replace subrama_det_cod = 2  if rama4d_var_u == "RAMA4D_R4" & rama4d_div == 2
replace subrama_det_cod = 3  if rama4d_var_u == "RAMA4D_R4" & rama4d_div == 3

* Minas
replace subrama_det_cod = 4  if rama4d_var_u == "RAMA4D_R4" & inlist(rama4d_div, 5, 6, 9)
replace subrama_det_cod = 5  if rama4d_var_u == "RAMA4D_R4" & inlist(rama4d_div, 7, 8)

* Manufactura
replace subrama_det_cod = 6  if rama4d_var_u == "RAMA4D_R4" & inrange(rama4d_div, 10, 12)
replace subrama_det_cod = 7  if rama4d_var_u == "RAMA4D_R4" & inrange(rama4d_div, 13, 14)
replace subrama_det_cod = 8  if rama4d_var_u == "RAMA4D_R4" & rama4d_div == 15
replace subrama_det_cod = 9  if rama4d_var_u == "RAMA4D_R4" & inrange(rama4d_div, 16, 17)
replace subrama_det_cod = 10 if rama4d_var_u == "RAMA4D_R4" & rama4d_div == 18
replace subrama_det_cod = 11 if rama4d_var_u == "RAMA4D_R4" & inrange(rama4d_div, 19, 21)
replace subrama_det_cod = 12 if rama4d_var_u == "RAMA4D_R4" & inrange(rama4d_div, 22, 23)
replace subrama_det_cod = 13 if rama4d_var_u == "RAMA4D_R4" & inrange(rama4d_div, 24, 25)
replace subrama_det_cod = 14 if rama4d_var_u == "RAMA4D_R4" & inrange(rama4d_div, 26, 28)
replace subrama_det_cod = 15 if rama4d_var_u == "RAMA4D_R4" & inrange(rama4d_div, 29, 30)
replace subrama_det_cod = 16 if rama4d_var_u == "RAMA4D_R4" & inrange(rama4d_div, 31, 33)

* Servicios públicos
replace subrama_det_cod = 17 if rama4d_var_u == "RAMA4D_R4" & rama4d_div == 35
replace subrama_det_cod = 18 if rama4d_var_u == "RAMA4D_R4" & inrange(rama4d_div, 36, 39)

* Construcción
replace subrama_det_cod = 19 if rama4d_var_u == "RAMA4D_R4" & inrange(rama4d_div, 41, 43)

* Comercio
replace subrama_det_cod = 20 if rama4d_var_u == "RAMA4D_R4" & rama4d_div == 45
replace subrama_det_cod = 21 if rama4d_var_u == "RAMA4D_R4" & rama4d_div == 46
replace subrama_det_cod = 22 if rama4d_var_u == "RAMA4D_R4" & rama4d_div == 47

* Alojamiento y comida
replace subrama_det_cod = 23 if rama4d_var_u == "RAMA4D_R4" & rama4d_div == 55
replace subrama_det_cod = 24 if rama4d_var_u == "RAMA4D_R4" & rama4d_div == 56

* Transporte, correo y telecomunicaciones
replace subrama_det_cod = 25 if rama4d_var_u == "RAMA4D_R4" & rama4d_div == 49
replace subrama_det_cod = 26 if rama4d_var_u == "RAMA4D_R4" & inlist(rama4d_div, 50, 51)
replace subrama_det_cod = 27 if rama4d_var_u == "RAMA4D_R4" & inlist(rama4d_div, 52, 53)
replace subrama_det_cod = 28 if rama4d_var_u == "RAMA4D_R4" & rama4d_div == 61

* Información y comunicaciones más desagregado
replace subrama_det_cod = 29 if rama4d_var_u == "RAMA4D_R4" & rama4d_div == 58
replace subrama_det_cod = 30 if rama4d_var_u == "RAMA4D_R4" & inlist(rama4d_div, 59, 60)
replace subrama_det_cod = 31 if rama4d_var_u == "RAMA4D_R4" & rama4d_div == 62
replace subrama_det_cod = 32 if rama4d_var_u == "RAMA4D_R4" & rama4d_div == 63

* Finanzas
replace subrama_det_cod = 33 if rama4d_var_u == "RAMA4D_R4" & rama4d_div == 64
replace subrama_det_cod = 34 if rama4d_var_u == "RAMA4D_R4" & inlist(rama4d_div, 65, 66)

* Inmobiliarias, profesionales y administrativas
replace subrama_det_cod = 35 if rama4d_var_u == "RAMA4D_R4" & rama4d_div == 68
replace subrama_det_cod = 36 if rama4d_var_u == "RAMA4D_R4" & inlist(rama4d_div, 69, 70, 71, 73, 74, 75)
replace subrama_det_cod = 37 if rama4d_var_u == "RAMA4D_R4" & rama4d_div == 72
replace subrama_det_cod = 38 if rama4d_var_u == "RAMA4D_R4" & inrange(rama4d_div, 77, 82)

* Administración pública, educación, salud y cuidado
replace subrama_det_cod = 39 if rama4d_var_u == "RAMA4D_R4" & rama4d_div == 84
replace subrama_det_cod = 40 if rama4d_var_u == "RAMA4D_R4" & rama4d_div == 85
replace subrama_det_cod = 41 if rama4d_var_u == "RAMA4D_R4" & rama4d_div == 86
replace subrama_det_cod = 42 if rama4d_var_u == "RAMA4D_R4" & inrange(rama4d_div, 87, 88)

* Artes, recreación y otros servicios
replace subrama_det_cod = 43 if rama4d_var_u == "RAMA4D_R4" & inrange(rama4d_div, 90, 93)
replace subrama_det_cod = 44 if rama4d_var_u == "RAMA4D_R4" & inrange(rama4d_div, 94, 96)

* Hogares y extraterritoriales
replace subrama_det_cod = 45 if rama4d_var_u == "RAMA4D_R4" & inrange(rama4d_div, 97, 98)
replace subrama_det_cod = 46 if rama4d_var_u == "RAMA4D_R4" & rama4d_div == 99


label define subrama_det_lbl ///
    1  "Agricultura y ganadería" ///
    2  "Silvicultura" ///
    3  "Pesca y acuicultura" ///
    4  "Carbón, petróleo y gas" ///
    5  "Otras minas y canteras" ///
    6  "Alimentos, bebidas y tabaco" ///
    7  "Textiles y confecciones" ///
    8  "Cuero y calzado" ///
    9  "Madera y papel" ///
    10 "Impresión y reproducción" ///
    11 "Refinación, químicos y farmacéuticos" ///
    12 "Caucho, plástico y minerales no metálicos" ///
    13 "Metales y productos metálicos" ///
    14 "Maquinaria, equipo eléctrico y electrónico" ///
    15 "Vehículos y equipo de transporte" ///
    16 "Muebles y otras manufacturas" ///
    17 "Electricidad y gas" ///
    18 "Agua, saneamiento y residuos" ///
    19 "Construcción" ///
    20 "Comercio de vehículos" ///
    21 "Comercio al por mayor" ///
    22 "Comercio al por menor" ///
    23 "Alojamiento" ///
    24 "Restaurantes y servicios de comida" ///
    25 "Transporte terrestre" ///
    26 "Transporte acuático y aéreo" ///
    27 "Auxiliares transporte, almacenamiento y correo" ///
    28 "Telecomunicaciones" ///
    29 "Editorial y publicaciones" ///
    30 "Audiovisual, radio, TV y noticias" ///
    31 "Software, informática y sistemas" ///
    32 "Servicios de información y datos" ///
    33 "Intermediación financiera" ///
    34 "Seguros y auxiliares financieros" ///
    35 "Actividades inmobiliarias" ///
    36 "Profesionales, científicas y técnicas" ///
    37 "Investigación y desarrollo" ///
    38 "Administrativas y servicios de apoyo" ///
    39 "Administración pública y defensa" ///
    40 "Educación" ///
    41 "Salud humana" ///
    42 "Servicios sociales y cuidado" ///
    43 "Artes, entretenimiento y recreación" ///
    44 "Otros servicios personales y asociaciones" ///
    45 "Hogares como empleadores" ///
    46 "Organizaciones extraterritoriales", replace

label values subrama_det_cod subrama_det_lbl


*====================================================
* 7. Auditoría de homologación antes de eliminar missings
*====================================================

gen byte miss_tamano   = missing(tamano_hom_cod)
gen byte miss_educ     = missing(educ_hom_cod)
gen byte miss_form     = missing(formalidad_cod)
gen byte miss_sexo     = missing(sexo_hom_cod)
gen byte miss_sector   = missing(sector_hom_cod)
gen byte miss_subrama  = missing(subrama_det_cod)
gen byte miss_edad     = missing(edad)
gen byte miss_depto    = missing(depto_cod)
gen byte miss_area     = missing(area_cod)
gen byte miss_posicion = missing(posicion_ocupacional_cod)
gen byte miss_oficio   = missing(oficio_cno70_2d_hom_cod)
gen byte amb_oficio    = oficio_hom_ambiguo == 1 if !missing(oficio_hom_ambiguo)
replace amb_oficio = 0 if missing(amb_oficio)
gen byte fila_audit    = 1

preserve

collapse ///
    (sum) obs_validas_ingreso = fila_audit ///
    (sum) miss_tamano = miss_tamano ///
    (sum) miss_educ = miss_educ ///
    (sum) miss_form = miss_form ///
    (sum) miss_sexo = miss_sexo ///
    (sum) miss_sector = miss_sector ///
    (sum) miss_subrama = miss_subrama ///
    (sum) miss_edad = miss_edad ///
    (sum) miss_depto = miss_depto ///
    (sum) miss_area = miss_area ///
    (sum) miss_posicion = miss_posicion ///
    (sum) miss_oficio = miss_oficio ///
    (sum) amb_oficio = amb_oficio, ///
    by(anio)

export excel using "Outputs/tables/auditoria_base_modelo_personas.xlsx", ///
    sheet("00_homologacion") ///
    firstrow(variables) ///
    replace

restore


*====================================================
* 8. Conservar observaciones con missings de interés
*====================================================
* Antes este bloque eliminaba casos con missings en tamaño, educación,
* formalidad, sexo, sector, edad, departamento o posición ocupacional.
* Ahora esos casos se conservan y se identifican con banderas para
* diagnosticar la calidad de la muestra sin perder observaciones.

gen byte muestra_controles_completos = ///
    !missing(tamano_hom_cod) & ///
    !missing(educ_hom_cod) & ///
    !missing(formalidad_cod) & ///
    !missing(sexo_hom_cod) & ///
    !missing(sector_hom_cod) & ///
    !missing(edad) & ///
    !missing(depto_cod) & ///
    !missing(posicion_ocupacional_cod) & ///
    !missing(oficio_cno70_2d_hom_cod)

gen byte muestra_sector_completo = ///
    !missing(sector_hom_cod) & ///
    !missing(subrama_det_cod)

count if missing(subrama_det_cod)
if r(N) > 0 {
    di as error "Advertencia: quedan observaciones sin subrama_det_cod despues de la homologacion."
    di as error "Se conservan en la base final para no eliminar observaciones con missings."
    tab anio rama4d_div if missing(subrama_det_cod), missing
}

*====================================================
* 9. IPC y salario real
*====================================================

gen double ipc_dic = .

replace ipc_dic = 69.80  if anio == 2008
replace ipc_dic = 71.20  if anio == 2009
replace ipc_dic = 73.45  if anio == 2010
replace ipc_dic = 76.19  if anio == 2011
replace ipc_dic = 78.05  if anio == 2012
replace ipc_dic = 79.56  if anio == 2013
replace ipc_dic = 82.47  if anio == 2014
replace ipc_dic = 88.05  if anio == 2015
replace ipc_dic = 93.11  if anio == 2016
replace ipc_dic = 96.92  if anio == 2017
replace ipc_dic = 100.00 if anio == 2018
replace ipc_dic = 103.80 if anio == 2019
replace ipc_dic = 105.48 if anio == 2020
replace ipc_dic = 111.41 if anio == 2021
replace ipc_dic = 126.03 if anio == 2022
replace ipc_dic = 137.72 if anio == 2023
replace ipc_dic = 144.88 if anio == 2024
replace ipc_dic = 152.27 if anio == 2025

gen double factor_precios_2025 = 152.27 / ipc_dic

gen double ingreso_hora_real = ingreso_laboral_hora * factor_precios_2025
gen double log_ingreso_hora_real = log(ingreso_hora_real)


*====================================================
* 10. Crear variables limpias para R / fixest
*====================================================

gen double fex = factor_expansion_anual

gen double horas = horas_semana
label variable horas "Horas trabajadas por semana"

label values sector_hom_cod sector_hom_lbl
label values tamano_hom_cod tamano_hom_lbl
label values educ_hom_cod educ_hom_lbl
label values sexo_hom_cod sexo_hom_lbl
label values formalidad_cod formalidad_lbl

decode sector_hom_cod, gen(sector)
decode subrama_det_cod, gen(subrama_det)

label variable rama4d "Código CIIU original a 4 dígitos"
label variable rama3d "Grupo CIIU derivado de RAMA4D"
label variable rama4d_div "División CIIU derivada de RAMA4D"
label variable rama4d_clase "Código CIIU a 4 dígitos como texto"
label variable ciiu_revision_rama4d "Revisión CIIU de RAMA4D"
label variable subrama_det_cod "Subrama detallada homologada"
label variable subrama_det "Subrama detallada homologada"

decode tamano_hom_cod, gen(tamano_empresa)
decode educ_hom_cod, gen(educacion)
decode sexo_hom_cod, gen(sexo)
decode formalidad_cod, gen(formalidad)

* Departamento, área y posición ocupacional para modelos
gen int depto = depto_cod
gen byte area = area_cod

decode area_cod, gen(area_label)
decode posicion_ocupacional_cod, gen(ocupacion)

* Alias más explícito, por si quieres usar ambos nombres
clonevar posicion_ocupacional = posicion_ocupacional_cod
decode posicion_ocupacional_cod, gen(posicion_ocupacional_label)

clonevar oficio_cno70_2d = oficio_cno70_2d_hom_cod
label variable oficio_cno70_2d "Oficio/CNO antiguo a 2 dígitos homologado"

gen byte mujer = sexo_hom_cod == 2 if !missing(sexo_hom_cod)

* Dummy formal:
* 1 = formal
* 0 = informal
* missing = pensionado ocupado
gen byte formal = .
replace formal = 1 if formalidad_cod == 1
replace formal = 0 if formalidad_cod == 2

gen byte pensionado_ocupado = formalidad_cod == 3 if !missing(formalidad_cod)

gen long persona_id = _n


*====================================================
* 11. Auditoría final por año
*====================================================

preserve

gen byte fila = 1

collapse ///
    (sum) fex_total = fex ///
    (sum) observaciones = fila ///
    (mean) ingreso_hora_promedio_simple = ingreso_laboral_hora ///
    [aw = fex], ///
    by(anio)

export excel using "Outputs/tables/auditoria_base_modelo_personas.xlsx", ///
    sheet("01_totales_anio") ///
    firstrow(variables) ///
    sheetreplace

restore


*====================================================
* 12. Ordenar y dejar base final
*====================================================

order persona_id anio ///
      edad edad2 ///
      depto area area_label ///
      sector_hom_cod sector ///
      rama4d rama4d_clase rama3d rama4d_div ciiu_revision_rama4d ///
      subrama_det_cod subrama_det ///
      oficio_cod_original oficio_cno70_2d_hom_cod oficio_cno70_2d ///
      ciuo08_4d_cod oficio_ciuo88_2d_cod oficio_hom_ambiguo ///
      oficio_hom_n_destinos oficio_hom_fuente ///
      posicion_ocupacional posicion_ocupacional_label ocupacion ///
      tamano_hom_cod tamano_empresa ///
      educ_hom_cod educacion ///
      sexo_hom_cod sexo mujer ///
      formalidad_cod formalidad formal pensionado_ocupado ///
      fex ///
      horas ///
      ingreso_laboral_hora ingreso_hora_real log_ingreso_hora_real ///
      muestra_ingreso_hora_valida muestra_factor_valido muestra_anio_valido ///
      muestra_productividad_valida muestra_controles_completos muestra_sector_completo ///
      ipc_dic factor_precios_2025

keep persona_id anio ///
     edad edad2 ///
     depto area area_label ///
     sector_hom_cod sector ///
     rama4d rama4d_clase rama3d rama4d_div ciiu_revision_rama4d ///
     subrama_det_cod subrama_det ///
     oficio_cod_original oficio_cno70_2d_hom_cod oficio_cno70_2d ///
     ciuo08_4d_cod oficio_ciuo88_2d_cod oficio_hom_ambiguo ///
     oficio_hom_n_destinos oficio_hom_fuente ///
     posicion_ocupacional posicion_ocupacional_label ocupacion ///
     tamano_hom_cod tamano_empresa ///
     educ_hom_cod educacion ///
     sexo_hom_cod sexo mujer ///
     formalidad_cod formalidad formal pensionado_ocupado ///
     fex ///
     horas ///
     ingreso_laboral_hora ingreso_hora_real log_ingreso_hora_real ///
     muestra_ingreso_hora_valida muestra_factor_valido muestra_anio_valido ///
     muestra_productividad_valida muestra_controles_completos muestra_sector_completo ///
     ipc_dic factor_precios_2025

sort anio persona_id

compress


*====================================================
* 13. Guardar base individual
*====================================================

save "Outputs/tables/Paper-GEIH_base_modelo_personas_2008_2025.dta", replace

di "===================================================="
di "BASE INDIVIDUAL PARA MODELOS CREADA CORRECTAMENTE"
di "Unidad: persona ocupada"
di "Archivo:"
di "Outputs/tables/Paper-GEIH_base_modelo_personas_2008_2025.dta"
di "Auditoría:"
di "Outputs/tables/auditoria_base_modelo_personas.xlsx"
di "===================================================="


