****************************************************
* BASE INDIVIDUAL LIMPIA PARA MODELOS ECONOMÉTRICOS
* GEIH 2021-2025
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
* 0a. Labels ocupacionales CIUO-08 A.C.
*====================================================
* La GEIH 2021-2025 trae OFICIO_C8 y OFICIO_C8_2D en CIUO-08 A.C.
* Este bloque no homologa ocupaciones; solo trae la descripción oficial
* de cada código desde la estructura CIUO-08 A.C. del DANE.

tempfile ciuo08_4d_labels ciuo08_2d_labels

import excel using ///
    "C:/Users/jorge/Documents/Trabajo-Profesional/Javeriana/DocumentacionAuxiliar/Correlativa_CIUO_88_A_C_vs_CIUO_08_A_C.xlsx", ///
    sheet("Estructura CIUO-08 A.C.") cellrange(A5:B975) clear

rename A ciuo08_raw
rename B ciuo08_desc

foreach v in ciuo08_raw ciuo08_desc {
    capture confirm numeric variable `v'
    if !_rc {
        tostring `v', replace force format(%20.0g)
    }
    replace `v' = strtrim(`v')
    replace `v' = "" if `v' == "."
}

preserve
    keep if regexm(ciuo08_raw, "^[0-9][0-9][0-9][0-9]$")
    gen double oficio_cod = real(ciuo08_raw)
    gen str240 oficio_c8_label = ciuo08_desc
    keep oficio_cod oficio_c8_label
    duplicates drop oficio_cod, force
    save `ciuo08_4d_labels', replace
restore

keep if regexm(ciuo08_raw, "^[0-9][0-9]$")
gen double oficio_c8_2d_cod = real(ciuo08_raw)
gen str240 oficio_c8_2d_label = ciuo08_desc
keep oficio_c8_2d_cod oficio_c8_2d_label
duplicates drop oficio_c8_2d_cod, force
save `ciuo08_2d_labels', replace

use "Outputs/tables/GEIH_consolidada_variables_interes_2021_2025.dta", clear


*====================================================
* 0. Normalizar nombres originales
*====================================================

gen str40 sector_var_u = upper(strtrim(sector_var_original))
gen str40 tamano_var_u = upper(strtrim(tamano_var_original))
gen str40 educ_var_u   = upper(strtrim(educ_var_original))


*====================================================
* 0a. Verificar y limpiar EI
*====================================================

capture confirm variable EI

if _rc {
    di as error "No se encontró la variable EI en la base consolidada 2021-2025."
    di as error "Revisa que el script BaseConsolidada_2021_2025 haya conservado EI."
    exit 111
}

capture confirm numeric variable EI

if _rc {
    tempvar EI_num
    destring EI, gen(`EI_num') force
    drop EI
    rename `EI_num' EI
}

label variable EI "Variable EI original armonizada"


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

foreach v in oficio_cod oficio_c8_2d_cod {

    capture confirm variable `v'

    if _rc {
        di as error "No se encontró la variable `v' en la base consolidada."
        di as error "Vuelve a ejecutar baseConsolidada2021-2025.do para conservar OFICIO_C8 y OFICIO_C8_2D."
        exit 111
    }

    capture confirm numeric variable `v'

    if _rc {
        tempvar tmp_oficio
        destring `v', gen(`tmp_oficio') force
        drop `v'
        rename `tmp_oficio' `v'
    }
}

* Limpiar códigos imposibles o vacíos
replace depto_cod = . if depto_cod <= 0
replace area_cod = . if area_cod <= 0
replace posicion_ocupacional_cod = . if !inrange(posicion_ocupacional_cod, 1, 9)
replace oficio_cod = . if !inrange(oficio_cod, 0, 9999)
replace oficio_c8_2d_cod = . if !inrange(oficio_c8_2d_cod, 0, 99)

merge m:1 oficio_cod using `ciuo08_4d_labels', keep(master match) nogen
merge m:1 oficio_c8_2d_cod using `ciuo08_2d_labels', keep(master match) nogen

label variable oficio_cod "Código ocupacional OFICIO_C8, CIUO-08 A.C. a 4 dígitos"
label variable oficio_c8_label "Descripción OFICIO_C8, CIUO-08 A.C. a 4 dígitos"
label variable oficio_c8_2d_cod "Código ocupacional OFICIO_C8_2D, CIUO-08 A.C. a 2 dígitos"
label variable oficio_c8_2d_label "Descripción OFICIO_C8_2D, CIUO-08 A.C. a 2 dígitos"

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
* 1. Filtrar observaciones válidas
*====================================================
* OJO: antes esta seccion tambien exigia ingreso_hora_valido==1,
* ingreso_laboral_hora no missing y > 0. Eso descartaba de la base a
* cualquier persona ocupada (ya viene de baseConsolidada con
* ocupado_cod==1) que no tuviera un ingreso por hora valido -por ejemplo
* trabajador familiar sin remuneracion, o item-missing en el ingreso-,
* aunque para contar "numero de ocupados" no se necesita el ingreso.
* Se quita ese filtro aqui para conservar la base lo mas completa posible;
* el indicador ingreso_hora_valido queda intacto para que cualquier
* analisis que si necesite ingreso por hora (ej. brecha salarial) filtre
* por su cuenta en el momento de usarlo, sin tener que rehacer la base.
*
* Se mantienen unicamente los filtros que son indispensables para
* cualquier conteo expandido: anio valido en el rango del panel, y un
* factor de expansion valido y positivo (sin fex no hay como expandir esa
* fila a poblacion, expandida o no expandida esa fila no aporta a ningun
* total).

keep if !missing(factor_expansion_anual)
keep if factor_expansion_anual > 0

keep if !missing(anio)
keep if inrange(anio, 2021, 2025)


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
replace subrama_det_cod = 36 if rama4d_var_u == "RAMA4D_R4" & inlist(rama4d_div, 69, 70, 71, 73)
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
gen byte miss_edad     = missing(edad)
gen byte miss_depto    = missing(depto_cod)
gen byte miss_area     = missing(area_cod)
gen byte miss_posicion = missing(posicion_ocupacional_cod)
gen byte miss_oficio4d = missing(oficio_cod)
gen byte miss_oficio2d = missing(oficio_c8_2d_cod)
gen byte miss_label_oficio4d = missing(oficio_c8_label) if !missing(oficio_cod)
gen byte miss_label_oficio2d = missing(oficio_c8_2d_label) if !missing(oficio_c8_2d_cod)
replace miss_label_oficio4d = 0 if missing(miss_label_oficio4d)
replace miss_label_oficio2d = 0 if missing(miss_label_oficio2d)
gen byte miss_ei       = missing(EI)
gen byte fila_audit    = 1

preserve

collapse ///
    (sum) obs_validas_ingreso = fila_audit ///
    (sum) miss_tamano = miss_tamano ///
    (sum) miss_educ = miss_educ ///
    (sum) miss_form = miss_form ///
    (sum) miss_sexo = miss_sexo ///
    (sum) miss_sector = miss_sector ///
    (sum) miss_edad = miss_edad ///
    (sum) miss_depto = miss_depto ///
    (sum) miss_area = miss_area ///
    (sum) miss_posicion = miss_posicion ///
    (sum) miss_oficio4d = miss_oficio4d ///
    (sum) miss_oficio2d = miss_oficio2d ///
    (sum) miss_label_oficio4d = miss_label_oficio4d ///
    (sum) miss_label_oficio2d = miss_label_oficio2d ///
    (sum) miss_ei = miss_ei, ///
    by(anio)

export excel using "Outputs/tables/auditoria_base_modelo_personas_2021_2025_EI.xlsx", ///
    sheet("00_homologacion") ///
    firstrow(variables) ///
    replace

restore


*====================================================
* 8. Ya NO se eliminan observaciones por variables incompletas
*====================================================
* Antes esta seccion eliminaba cualquier fila (persona ocupada) que
* tuviera missing en tamano de empresa, educacion, formalidad, sexo,
* sector, edad, departamento, posicion ocupacional o EI. Cada uno de esos
* missing por separado le quitaba gente genuinamente ocupada al conteo
* total, sin que esa informacion faltante tuviera nada que ver con si la
* persona esta o no ocupada.
*
* La base ahora conserva a todo ocupado (ocupado_cod==1, ya filtrado en
* baseConsolidada2021-2025.do) con fex valido, sin importar que le falten
* estas variables. Los indicadores miss_* generados en la seccion 7 siguen
* disponibles para que cualquier analisis que sí necesite, por ejemplo,
* sector_hom_cod completo, filtre por su cuenta con
* "keep if !missing(sector_hom_cod)" en ese momento, en vez de perder esas
* observaciones para todo el mundo desde la base madre.

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

clonevar oficio_c8_4d = oficio_cod
label variable oficio_c8_4d "Código ocupacional OFICIO_C8, CIUO-08 A.C. a 4 dígitos"

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

export excel using "Outputs/tables/auditoria_base_modelo_personas_2021_2025_EI.xlsx", ///
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
      oficio_c8_4d oficio_c8_label oficio_c8_2d_cod oficio_c8_2d_label ///
      posicion_ocupacional posicion_ocupacional_label ocupacion ///
      tamano_hom_cod tamano_empresa ///
      educ_hom_cod educacion ///
      sexo_hom_cod sexo mujer ///
      formalidad_cod formalidad formal pensionado_ocupado ///
      EI ///
      fex ///
      horas ///
      ingreso_laboral_hora ingreso_hora_real log_ingreso_hora_real ///
      ipc_dic factor_precios_2025

keep persona_id anio ///
     edad edad2 ///
     depto area area_label ///
     sector_hom_cod sector ///
     rama4d rama4d_clase rama3d rama4d_div ciiu_revision_rama4d ///
     subrama_det_cod subrama_det ///
     oficio_c8_4d oficio_c8_label oficio_c8_2d_cod oficio_c8_2d_label ///
     posicion_ocupacional posicion_ocupacional_label ocupacion ///
     tamano_hom_cod tamano_empresa ///
     educ_hom_cod educacion ///
     sexo_hom_cod sexo mujer ///
     formalidad_cod formalidad formal pensionado_ocupado ///
     EI ///
     fex ///
     horas ///
     ingreso_laboral_hora ingreso_hora_real log_ingreso_hora_real ///
     ipc_dic factor_precios_2025

sort anio persona_id

compress


*====================================================
* 13. Guardar base individual
*====================================================

save "Outputs/tables/Paper-GEIH_base_modelo_personas_2021_2025_EI.dta", replace

* Checks rápidos
tab anio, missing
tab anio EI, missing

di "===================================================="
di "BASE INDIVIDUAL PARA MODELOS 2021-2025 CON EI CREADA CORRECTAMENTE"
di "Unidad: persona ocupada"
di "Archivo:"
di "Outputs/tables/Paper-GEIH_base_modelo_personas_2021_2025_EI.dta"
di "Auditoría:"
di "Outputs/tables/auditoria_base_modelo_personas_2021_2025_EI.xlsx"
di "===================================================="


