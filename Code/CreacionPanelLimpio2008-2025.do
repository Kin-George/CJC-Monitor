****************************************************
* BASE DE CELDAS PARA MODELOS
* GEIH 2008-2025
*
* Unidad:
* año x sector x tamaño x educación x sexo x formalidad
****************************************************

clear all
set more off

cd "C:/Users/jorge/Documents/Databases/GEIH"

capture mkdir "Outputs"
capture mkdir "Outputs/tables"

use "Outputs/tables/GEIH_consolidada_variables_interes_2008_2025.dta", clear


*====================================================
* 0. Normalizar nombres originales
*====================================================

gen str40 sector_var_u = upper(strtrim(sector_var_original))
gen str40 tamano_var_u = upper(strtrim(tamano_var_original))
gen str40 educ_var_u   = upper(strtrim(educ_var_original))


*====================================================
* 1. Filtrar observaciones válidas
*====================================================

keep if ingreso_hora_valido == 1
keep if !missing(ingreso_laboral_hora)
keep if ingreso_laboral_hora > 0
keep if !missing(factor_expansion_anual)
keep if factor_expansion_anual > 0

keep if !missing(anio)
keep if inrange(anio, 2008, 2025)


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
* Ambas se homologan como 101+
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
* 3. Homologar educación - CORREGIDO
*====================================================
* Categorías comparables:
* 1 Ninguno
* 2 Preescolar
* 3 Básica primaria
* 4 Básica secundaria
* 5 Media
* 6 Superior o universitaria
* 9 No sabe, no informa

capture drop educ_hom_cod
gen byte educ_hom_cod = .


*----------------------------------------------------
* 3.1. Educación antigua:
* P6210 y NIVEL_MAS_ALTO cuando usa códigos 1-6 y 9
* Aplica principalmente a 2008-2019
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
* 3.2. Educación nueva:
* P3042 y NIVEL_MAS_ALTO cuando usa códigos desagregados
* Aplica para años recientes, especialmente 2022
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

* Superior = normalista + técnica profesional + tecnológica + universitaria + posgrados
replace educ_hom_cod = 6 if ///
    (educ_var_u == "P3042" | (educ_var_u == "NIVEL_MAS_ALTO" & anio >= 2021)) ///
    & inrange(educacion_cod, 7, 13)

* No sabe / no informa
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
* 7. Auditoría antes de eliminar missings
*====================================================

gen byte miss_tamano = missing(tamano_hom_cod)
gen byte miss_educ   = missing(educ_hom_cod)
gen byte miss_form   = missing(formalidad_cod)
gen byte miss_sexo   = missing(sexo_hom_cod)
gen byte miss_sector = missing(sector_hom_cod)
gen byte fila_audit  = 1

preserve

collapse ///
    (sum) obs_validas_ingreso = fila_audit ///
    (sum) miss_tamano = miss_tamano ///
    (sum) miss_educ = miss_educ ///
    (sum) miss_form = miss_form ///
    (sum) miss_sexo = miss_sexo ///
    (sum) miss_sector = miss_sector, ///
    by(anio)

export excel using "Outputs/tables/auditoria_base_modelo_celdas.xlsx", ///
    sheet("00_homologacion") ///
    firstrow(variables) ///
    replace

restore


*====================================================
* 8. Mantener observaciones con categorías completas
*====================================================

drop if missing(tamano_hom_cod)
drop if missing(educ_hom_cod)
drop if missing(formalidad_cod)
drop if missing(sexo_hom_cod)
drop if missing(sector_hom_cod)


*====================================================
* 9. Preparar variables para collapse
*====================================================

gen double ingreso_hora_expandido = ingreso_laboral_hora * factor_expansion_anual
gen byte fila = 1


*====================================================
* 10. Guardar auditoría individual antes del collapse
*====================================================

preserve

collapse ///
    (sum) fex_ind = factor_expansion_anual ///
    (sum) ingreso_hora_total_ind = ingreso_hora_expandido ///
    (sum) obs_ind = fila, ///
    by(anio)

gen double ingreso_hora_promedio_ind = ingreso_hora_total_ind / fex_ind

tempfile auditoria_individual
save `auditoria_individual', replace

restore


*====================================================
* 11. Crear base agregada para modelos
*====================================================
* Aquí se repondera 101+:
* P3069 9 y 10 ya quedaron como tamano_hom_cod = 9.

collapse ///
    (sum) fex = factor_expansion_anual ///
    (sum) ingreso_hora_total_expandido = ingreso_hora_expandido ///
    (sum) observaciones = fila, ///
    by(anio sector_hom_cod tamano_hom_cod educ_hom_cod sexo_hom_cod formalidad_cod)

gen double ingreso_hora_promedio = ingreso_hora_total_expandido / fex


*====================================================
* 12. IPC y salario real
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

gen double ingreso_hora_real = ingreso_hora_promedio * factor_precios_2025
gen double log_ingreso_hora_real = log(ingreso_hora_real)


*====================================================
* 13. Crear variables limpias para modelos
*====================================================

label values sector_hom_cod sector_hom_lbl
label values tamano_hom_cod tamano_hom_lbl
label values educ_hom_cod educ_hom_lbl
label values sexo_hom_cod sexo_hom_lbl
label values formalidad_cod formalidad_lbl

decode sector_hom_cod, gen(sector)
decode tamano_hom_cod, gen(tamano_empresa)
decode educ_hom_cod, gen(educacion)
decode sexo_hom_cod, gen(sexo)
decode formalidad_cod, gen(formalidad)

gen byte mujer = sexo_hom_cod == 2 if !missing(sexo_hom_cod)

* Dummy formal:
* 1 = formal
* 0 = informal
* missing = pensionado ocupado
gen byte formal = .
replace formal = 1 if formalidad_cod == 1
replace formal = 0 if formalidad_cod == 2

gen byte pensionado_ocupado = formalidad_cod == 3 if !missing(formalidad_cod)

egen celda_id = group(sector_hom_cod tamano_hom_cod educ_hom_cod sexo_hom_cod formalidad_cod)

gen byte muestra_baja = observaciones < 30


*====================================================
* 14. Validación: agregado vs individual
*====================================================

preserve

collapse ///
    (sum) fex_agg = fex ///
    (sum) ingreso_hora_total_agg = ingreso_hora_total_expandido ///
    (sum) obs_agg = observaciones, ///
    by(anio)

gen double ingreso_hora_promedio_agg = ingreso_hora_total_agg / fex_agg

merge 1:1 anio using `auditoria_individual', nogen

gen double dif_fex = fex_agg - fex_ind
gen double dif_ingreso_total = ingreso_hora_total_agg - ingreso_hora_total_ind
gen double dif_ingreso_promedio = ingreso_hora_promedio_agg - ingreso_hora_promedio_ind
gen double dif_obs = obs_agg - obs_ind

format fex_agg fex_ind dif_fex %18.4f
format ingreso_hora_promedio_agg ingreso_hora_promedio_ind dif_ingreso_promedio %18.8f
format obs_agg obs_ind dif_obs %18.0fc

export excel using "Outputs/tables/auditoria_base_modelo_celdas.xlsx", ///
    sheet("01_totales_anio") ///
    firstrow(variables) ///
    sheetreplace

restore


*====================================================
* 15. Dejar base final limpia
*====================================================

order celda_id anio ///
      sector_hom_cod sector ///
      tamano_hom_cod tamano_empresa ///
      educ_hom_cod educacion ///
      sexo_hom_cod sexo mujer ///
      formalidad_cod formalidad formal pensionado_ocupado ///
      fex observaciones muestra_baja ///
      ingreso_hora_promedio ingreso_hora_real log_ingreso_hora_real ///
      ingreso_hora_total_expandido ///
      ipc_dic factor_precios_2025

keep celda_id anio ///
     sector_hom_cod sector ///
     tamano_hom_cod tamano_empresa ///
     educ_hom_cod educacion ///
     sexo_hom_cod sexo mujer ///
     formalidad_cod formalidad formal pensionado_ocupado ///
     fex observaciones muestra_baja ///
     ingreso_hora_promedio ingreso_hora_real log_ingreso_hora_real ///
     ingreso_hora_total_expandido ///
     ipc_dic factor_precios_2025

sort celda_id anio

compress


*====================================================
* 16. Guardar base final
*====================================================

save "Outputs/tables/GEIH_base_modelo_celdas_2008_2025.dta", replace

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

use "Outputs/tables/GEIH_consolidada_variables_interes_2008_2025.dta", clear


*====================================================
* 0. Normalizar nombres originales
*====================================================

gen str40 sector_var_u = upper(strtrim(sector_var_original))
gen str40 tamano_var_u = upper(strtrim(tamano_var_original))
gen str40 educ_var_u   = upper(strtrim(educ_var_original))

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

* Limpiar códigos imposibles o vacíos
replace depto_cod = . if depto_cod <= 0
replace area_cod = . if area_cod <= 0
replace posicion_ocupacional_cod = . if !inrange(posicion_ocupacional_cod, 1, 9)

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

keep if ingreso_hora_valido == 1
keep if !missing(ingreso_laboral_hora)
keep if ingreso_laboral_hora > 0
keep if !missing(factor_expansion_anual)
keep if factor_expansion_anual > 0

keep if !missing(anio)
keep if inrange(anio, 2008, 2025)


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
    (sum) miss_posicion = miss_posicion, ///
    by(anio)

export excel using "Outputs/tables/auditoria_base_modelo_personas.xlsx", ///
    sheet("00_homologacion") ///
    firstrow(variables) ///
    replace

restore


*====================================================
* 8. Mantener observaciones completas
*====================================================

drop if missing(tamano_hom_cod)
drop if missing(educ_hom_cod)
drop if missing(formalidad_cod)
drop if missing(sexo_hom_cod)
drop if missing(sector_hom_cod)
drop if missing(edad)
drop if missing(depto_cod)
drop if missing(area_cod)
drop if missing(posicion_ocupacional_cod)

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

label values sector_hom_cod sector_hom_lbl
label values tamano_hom_cod tamano_hom_lbl
label values educ_hom_cod educ_hom_lbl
label values sexo_hom_cod sexo_hom_lbl
label values formalidad_cod formalidad_lbl

decode sector_hom_cod, gen(sector)
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
      posicion_ocupacional posicion_ocupacional_label ocupacion ///
      tamano_hom_cod tamano_empresa ///
      educ_hom_cod educacion ///
      sexo_hom_cod sexo mujer ///
      formalidad_cod formalidad formal pensionado_ocupado ///
      fex ///
      ingreso_laboral_hora ingreso_hora_real log_ingreso_hora_real ///
      ipc_dic factor_precios_2025

keep persona_id anio ///
     edad edad2 ///
     depto area area_label ///
     sector_hom_cod sector ///
     posicion_ocupacional posicion_ocupacional_label ocupacion ///
     tamano_hom_cod tamano_empresa ///
     educ_hom_cod educacion ///
     sexo_hom_cod sexo mujer ///
     formalidad_cod formalidad formal pensionado_ocupado ///
     fex ///
     ingreso_laboral_hora ingreso_hora_real log_ingreso_hora_real ///
     ipc_dic factor_precios_2025

sort anio persona_id

compress


*====================================================
* 13. Guardar base individual
*====================================================

save "Outputs/tables/GEIH_base_modelo_personas_2008_2025.dta", replace

di "===================================================="
di "BASE INDIVIDUAL PARA MODELOS CREADA CORRECTAMENTE"
di "Unidad: persona ocupada"
di "Archivo:"
di "Outputs/tables/GEIH_base_modelo_personas_2008_2025.dta"
di "Auditoría:"
di "Outputs/tables/auditoria_base_modelo_personas.xlsx"
di "===================================================="


