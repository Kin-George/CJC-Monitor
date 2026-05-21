
****************************************************
* BASE CONSOLIDADA GEIH 2008-2025
* Armonización mínima de nombres de columnas
* No homologa categorías
* No usa labels originales
****************************************************

clear all
set more off

cd "C:/Users/jorge/Documents/Databases/GEIH"

capture mkdir "Outputs"
capture mkdir "Outputs/tables"

*====================================================
* 0. PARÁMETROS
*====================================================

local solo_ocupados 1

local anios 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 2021 2022 2023 2024 2025


*====================================================
* 1. DICCIONARIO DE POSIBLES NOMBRES
*====================================================
* Aquí agregamos nombres alternativos si aparecen nuevos.

local cand_factor   "fex_c_2011 FEX_C_2011 FEX_C18 FEX_C fex_c18 fex_c"
local cand_ingreso  "INGLABO inglabo"
local cand_sector   "RAMA2D_R4 RAMA2D rama2d_r4 rama2d"
local cand_ocupado  "OCI oci"
local cand_pension  "P6920 p6920"
local cand_tamano   "P6870 P3069 p6870 p3069"
local cand_horas    "P6800 p6800"
local cand_sexo     "P6020 SEXO sexo p6020 P3271 p3271"
local cand_salud    "P6090 p6090"
local cand_educ     "P6210 P3042 p6210 p3042 NIVEL_MAS_ALTO"
local cand_edad     "P6040 EDAD p6040 edad"
local cand_depto      "DPTO dpto"
local cand_area       "AREA area"
local cand_posicion   "P6430 p6430"


*====================================================
* 2. PROGRAMAS AUXILIARES
*====================================================

capture program drop find_first_var

program define find_first_var, rclass
    syntax, CANDIDATES(string)

    return local var ""

    ds
    local allvars `r(varlist)'

    foreach cand of local candidates {

        local cand_trim = strtrim("`cand'")
        local cand_upper = upper("`cand_trim'")

        foreach v of local allvars {
            local v_upper = upper("`v'")

            if "`v_upper'" == "`cand_upper'" {
                return local var "`v'"
                exit
            }
        }
    }
end


capture program drop make_double_from_var

program define make_double_from_var
    syntax, NEWname(name) VARname(string)

    capture drop `newname'
    gen double `newname' = .

    if "`varname'" != "" {

        capture confirm numeric variable `varname'

        if !_rc {
            replace `newname' = `varname'
        }

        else {
            tempvar tmp
            quietly destring `varname', gen(`tmp') force
            replace `newname' = `tmp'
        }
    }
end


*====================================================
* 3. ARCHIVOS TEMPORALES
*====================================================

tempfile master one auditfile
local first = 1

tempname postaudit

postfile `postaudit' ///
    int anio ///
    str40 var_factor ///
    str40 var_ingreso ///
    str40 var_sector ///
    str40 var_ocupado ///
    str40 var_pension ///
    str40 var_tamano ///
    str40 var_horas ///
    str40 var_sexo ///
    str40 var_salud ///
    str40 var_educ ///
	str40 var_edad ///
	str40 var_depto ///
    str40 var_area ///
    str40 var_posicion ///
    byte procesado ///
    str200 observacion ///
    using `auditfile', replace


*====================================================
* 4. LOOP PRINCIPAL
*====================================================

foreach year of local anios {

    di "===================================================="
    di "Procesando año: `year'"
    di "===================================================="

    local archivo_base "GEIH_`year'_TOTAL.dta"

    capture confirm file "`archivo_base'"

	if _rc {
    post `postaudit' ///
        (`year') ///
        ("") ("") ("") ("") ("") ("") ("") ("") ("") ("") ///
        ("") ("") ("") ("") ///
        (0) ///
        ("No existe el archivo")
    continue
}

    use "`archivo_base'", clear


    *================================================
    * 4.1. Buscar nombre real de cada variable
    *================================================

    find_first_var, candidates("`cand_factor'")
    local var_factor "`r(var)'"

    find_first_var, candidates("`cand_ingreso'")
    local var_ingreso "`r(var)'"

    find_first_var, candidates("`cand_sector'")
    local var_sector "`r(var)'"

    find_first_var, candidates("`cand_ocupado'")
    local var_ocupado "`r(var)'"

    find_first_var, candidates("`cand_pension'")
    local var_pension "`r(var)'"

    find_first_var, candidates("`cand_tamano'")
    local var_tamano "`r(var)'"

    find_first_var, candidates("`cand_horas'")
    local var_horas "`r(var)'"

    find_first_var, candidates("`cand_sexo'")
    local var_sexo "`r(var)'"

    find_first_var, candidates("`cand_salud'")
    local var_salud "`r(var)'"

    find_first_var, candidates("`cand_educ'")
    local var_educ "`r(var)'"
	
	local cand_edad_year ""

	if inrange(`year', 2008, 2014) | inlist(`year', 2021, 2023, 2024, 2025) {
    local cand_edad_year "P6040 p6040 EDAD edad"
	}

	else if inrange(`year', 2015, 2019) | `year' == 2022 {
    local cand_edad_year "EDAD edad P6040 p6040"
	}

	else {
    local cand_edad_year "`cand_edad'"
	}

	find_first_var, candidates("`cand_edad_year'")
	local var_edad "`r(var)'"
	
	find_first_var, candidates("`cand_depto'")
	local var_depto "`r(var)'"

	find_first_var, candidates("`cand_area'")
	local var_area "`r(var)'"

	find_first_var, candidates("`cand_posicion'")
	local var_posicion "`r(var)'"


    *================================================
    * 4.2. Validar variables esenciales
    *================================================
    * Para construir la base son esenciales:
    * factor, ingreso y ocupado.
    * Las demás se dejan en missing si no se encuentra
    * el nombre todavía.

    local observacion ""

    if "`var_factor'" == "" {
        local observacion "`observacion' Falta factor;"
    }

    if "`var_ingreso'" == "" {
        local observacion "`observacion' Falta ingreso;"
    }

    if "`var_ocupado'" == "" {
        local observacion "`observacion' Falta ocupado;"
    }

    if "`observacion'" != "" {

    post `postaudit' ///
    (`year') ///
    ("`var_factor'") ///
    ("`var_ingreso'") ///
    ("`var_sector'") ///
    ("`var_ocupado'") ///
    ("`var_pension'") ///
    ("`var_tamano'") ///
    ("`var_horas'") ///
    ("`var_sexo'") ///
    ("`var_salud'") ///
    ("`var_educ'") ///
    ("`var_edad'") ///
    ("`var_depto'") ///
    ("`var_area'") ///
    ("`var_posicion'") ///
    (0) ///
    ("`observacion'")

        di as error "Se omite `year': `observacion'"
        continue
    }

    if "`var_sector'" == "" {
        local observacion "`observacion' Falta sector;"
    }

    if "`var_pension'" == "" {
        local observacion "`observacion' Falta pensión;"
    }

    if "`var_tamano'" == "" {
        local observacion "`observacion' Falta tamaño;"
    }

    if "`var_horas'" == "" {
        local observacion "`observacion' Falta horas;"
    }

    if "`var_sexo'" == "" {
        local observacion "`observacion' Falta sexo;"
    }

    if "`var_salud'" == "" {
        local observacion "`observacion' Falta salud;"
    }

    if "`var_educ'" == "" {
        local observacion "`observacion' Falta educación;"
    }
	
	if "`var_edad'" == "" {
    local observacion "`observacion' Falta edad;"
	}
	
	if "`var_depto'" == "" {
    local observacion "`observacion' Falta depto;"
	}

	if "`var_area'" == "" {
    local observacion "`observacion' Falta area;"
	}

	if "`var_posicion'" == "" {
    local observacion "`observacion' Falta posicion ocupacional;"
	}

    if "`observacion'" == "" {
        local observacion "Completo"
    }
	

    post `postaudit' ///
        (`year') ///
        ("`var_factor'") ///
        ("`var_ingreso'") ///
        ("`var_sector'") ///
        ("`var_ocupado'") ///
        ("`var_pension'") ///
        ("`var_tamano'") ///
        ("`var_horas'") ///
        ("`var_sexo'") ///
        ("`var_salud'") ///
        ("`var_educ'") ///
		("`var_edad'") ///
		("`var_depto'") ///
		("`var_area'") ///
		("`var_posicion'") ///
        (1) ///
        ("`observacion'")

    di as text "Variables usadas en `year':"
    di as result "Factor:    `var_factor'"
    di as result "Ingreso:   `var_ingreso'"
    di as result "Sector:    `var_sector'"
    di as result "Ocupado:   `var_ocupado'"
    di as result "Pensión:   `var_pension'"
    di as result "Tamaño:    `var_tamano'"
    di as result "Horas:     `var_horas'"
    di as result "Sexo:      `var_sexo'"
    di as result "Salud:     `var_salud'"
    di as result "Educación: `var_educ'"
	di as result "Edad:     `var_edad'"
	di as result "Depto:    `var_depto'"
	di as result "Área:     `var_area'"
	di as result "P6430:    `var_posicion'"


    *================================================
    * 4.3. Crear variables con nombres comunes
    *================================================

    gen int anio = `year'

    gen str40 factor_var_original  = "`var_factor'"
    gen str40 ingreso_var_original = "`var_ingreso'"
    gen str40 sector_var_original  = "`var_sector'"
    gen str40 ocupado_var_original = "`var_ocupado'"
    gen str40 pension_var_original = "`var_pension'"
    gen str40 tamano_var_original  = "`var_tamano'"
    gen str40 horas_var_original   = "`var_horas'"
    gen str40 sexo_var_original    = "`var_sexo'"
    gen str40 salud_var_original   = "`var_salud'"
    gen str40 educ_var_original    = "`var_educ'"
	gen str40 edad_var_original    = "`var_edad'"
	gen str40 depto_var_original    = "`var_depto'"
	gen str40 area_var_original     = "`var_area'"
	gen str40 posicion_var_original = "`var_posicion'"


    *================================================
    * 4.4. Copiar variables a formato numérico estándar
    *================================================

    make_double_from_var, newname(factor_expansion_original) varname("`var_factor'")
    make_double_from_var, newname(ingresos_laborales) varname("`var_ingreso'")
    make_double_from_var, newname(ocupado_cod) varname("`var_ocupado'")
    make_double_from_var, newname(sector_cod) varname("`var_sector'")
    make_double_from_var, newname(cotiza_pension_cod) varname("`var_pension'")
    make_double_from_var, newname(tamano_empresa_cod) varname("`var_tamano'")
    make_double_from_var, newname(horas_semana) varname("`var_horas'")
    make_double_from_var, newname(sexo_cod) varname("`var_sexo'")
    make_double_from_var, newname(cotiza_salud_cod) varname("`var_salud'")
    make_double_from_var, newname(educacion_cod) varname("`var_educ'")
	make_double_from_var, newname(edad_tmp) varname("`var_edad'")
	make_double_from_var, newname(depto_cod) varname("`var_depto'")
	make_double_from_var, newname(area_cod) varname("`var_area'")
	make_double_from_var, newname(posicion_ocupacional_cod) varname("`var_posicion'")

	capture drop edad
	rename edad_tmp edad

	replace edad = . if edad < 0
	replace edad = . if edad > 120


    *================================================
    * 4.5. Filtro de ocupados
    *================================================

    if `solo_ocupados' == 1 {
        keep if ocupado_cod == 1
    }


    *================================================
    * 4.6. Ingreso, horas y factor
    *================================================

    gen double factor_expansion_anual = factor_expansion_original / 12

    gen byte ingreso_valido = !missing(ingresos_laborales) & ingresos_laborales > 0

    gen double ingreso_laboral_anual = .
    replace ingreso_laboral_anual = ingresos_laborales * 12 if ingreso_valido == 1

    replace horas_semana = . if horas_semana <= 0
    replace horas_semana = . if horas_semana > 168

    gen byte horas_validas = !missing(horas_semana)

    gen double ingreso_laboral_hora = .
    replace ingreso_laboral_hora = ingreso_laboral_anual / (horas_semana * 52) ///
        if ingreso_valido == 1 & horas_validas == 1

    gen byte ingreso_hora_valido = !missing(ingreso_laboral_hora)


    *================================================
    * 4.7. Revisión CIIU informativa
    *================================================

    gen str20 ciiu_revision = ""

    replace ciiu_revision = "CIIU Rev. 3" if upper(sector_var_original) == "RAMA2D"
    replace ciiu_revision = "CIIU Rev. 4" if upper(sector_var_original) == "RAMA2D_R4"


    *================================================
    * 4.8. Quedarse solo con variables de interés
    *================================================

    keep ///
        anio ///
        factor_var_original ingreso_var_original sector_var_original ///
        ocupado_var_original pension_var_original tamano_var_original ///
        horas_var_original sexo_var_original salud_var_original educ_var_original ///
        factor_expansion_original factor_expansion_anual ///
        ocupado_cod ///
        ingresos_laborales ingreso_valido ///
        ingreso_laboral_anual ///
        horas_semana horas_validas ///
        ingreso_laboral_hora ingreso_hora_valido ///
        sector_cod ciiu_revision ///
        cotiza_pension_cod ///
        tamano_empresa_cod ///
        sexo_cod ///
        cotiza_salud_cod ///
        educacion_cod ///
		edad ///
		depto_cod ///
		area_cod ///
		posicion_ocupacional_cod

    compress

    save `one', replace


    *================================================
    * 4.9. Append al master
    *================================================

    if `first' == 1 {
        use `one', clear
        save `master', replace
        local first = 0
    }

    else {
        use `master', clear
        append using `one'
        save `master', replace
    }
}


postclose `postaudit'


*====================================================
* 5. Guardar base final
*====================================================

if `first' == 1 {
    di as error "No se procesó ningún año."
    exit 111
}

use `master', clear

sort anio

save "Outputs/tables/GEIH_consolidada_variables_interes_2008_2025.dta", replace

export delimited using "Outputs/tables/GEIH_consolidada_variables_interes_2008_2025.csv", ///
    replace delimiter(";")


*====================================================
* 6. Guardar auditoría
*====================================================

use `auditfile', clear

save "Outputs/tables/auditoria_variables_geih_2008_2025.dta", replace

export excel using "Outputs/tables/auditoria_variables_geih_2008_2025.xlsx", ///
    firstrow(variables) replace


di "===================================================="
di "BASE CONSOLIDADA CREADA CORRECTAMENTE"
di "DTA:"
di "Outputs/tables/GEIH_consolidada_variables_interes_2008_2025.dta"
di ""
di "CSV:"
di "Outputs/tables/GEIH_consolidada_variables_interes_2008_2025.csv"
di ""
di "AUDITORÍA:"
di "Outputs/tables/auditoria_variables_geih_2008_2025.xlsx"
di "===================================================="


*====================================================
* 1. Quedarse con observaciones válidas
*====================================================

keep if ingreso_hora_valido == 1
keep if !missing(ingreso_laboral_hora)
keep if !missing(factor_expansion_anual)
keep if factor_expansion_anual > 0


*====================================================
* 2. Crear ingreso por hora expandido
*====================================================

gen ingreso_hora_expandido = ingreso_laboral_hora * factor_expansion_anual


*====================================================
* 3. Colapsar por año
*====================================================

collapse ///
    (sum) trabajadores = factor_expansion_anual ///
    (sum) ingreso_hora_total_expandido = ingreso_hora_expandido ///
    (count) observaciones = ingreso_laboral_hora, ///
    by(anio)


*====================================================
* 4. Calcular promedio ponderado
*====================================================

gen ingreso_hora_promedio = ingreso_hora_total_expandido / trabajadores


*====================================================
* 5. Ordenar y formatear
*====================================================

sort anio

format trabajadores %15.0fc
format ingreso_hora_total_expandido %18.0fc
format ingreso_hora_promedio %12.2fc
format observaciones %12.0fc

list anio trabajadores ingreso_hora_promedio observaciones, sep(0)

****************************************************
* INGRESO LABORAL POR HORA POR AÑO Y TAMAÑO EMPRESA
* Sin homologar categorías
****************************************************

clear all
set more off

cd "C:/Users/jorge/Documents/Databases/GEIH"

use "Outputs/tables/GEIH_consolidada_variables_interes_2008_2025.dta", clear

*====================================================
* 1. Quedarse con observaciones válidas
*====================================================

keep if ingreso_hora_valido == 1
keep if !missing(ingreso_laboral_hora)
keep if !missing(factor_expansion_anual)
keep if factor_expansion_anual > 0

keep if !missing(tamano_empresa_cod)

*====================================================
* 2. Ingreso por hora expandido
*====================================================

gen ingreso_hora_expandido = ingreso_laboral_hora * factor_expansion_anual

*====================================================
* 3. Colapsar por año, variable original y código
*====================================================

collapse ///
    (sum) trabajadores = factor_expansion_anual ///
    (sum) ingreso_hora_total_expandido = ingreso_hora_expandido ///
    (count) observaciones = ingreso_laboral_hora, ///
    by(anio tamano_var_original tamano_empresa_cod)

*====================================================
* 4. Promedio ponderado
*====================================================

gen ingreso_hora_promedio = ingreso_hora_total_expandido / trabajadores

sort anio tamano_var_original tamano_empresa_cod

format trabajadores %15.0fc
format ingreso_hora_promedio %12.2fc
format observaciones %12.0fc

list anio tamano_var_original tamano_empresa_cod trabajadores ingreso_hora_promedio observaciones, sepby(anio)

export excel using "Outputs/tables/ingreso_hora_por_anio_tamano_sin_homologar.xlsx", ///
    firstrow(variables) replace
	
	
****************************************************
* REGENERAR TABLA:
* INGRESO HORA POR AÑO, EDUCACIÓN Y TAMAÑO
* DESDE BASE ARMONIZADA
* CORREGIDO: incluye NIVEL_MAS_ALTO
****************************************************

clear all
set more off

cd "C:/Users/jorge/Documents/Databases/GEIH"

use "Outputs/tables/GEIH_consolidada_variables_interes_2008_2025.dta", clear


*====================================================
* 0. Revisar años disponibles en la base armonizada
*====================================================

di "===================================================="
di "AÑOS DISPONIBLES EN LA BASE ARMONIZADA"
di "===================================================="

tab anio, missing
tab anio educ_var_original, missing
tab anio tamano_var_original, missing


*====================================================
* 1. Normalizar nombres originales
*====================================================

gen str40 educ_var_u = upper(strtrim(educ_var_original))
gen str40 tamano_var_u = upper(strtrim(tamano_var_original))


*====================================================
* 2. Crear labels limpias de educación
*====================================================

gen str60 educacion_label = ""

*-------------------------------
* P6210
* Categorías agregadas
*-------------------------------

replace educacion_label = "Ninguno" if educ_var_u == "P6210" & educacion_cod == 1
replace educacion_label = "Preescolar" if educ_var_u == "P6210" & educacion_cod == 2
replace educacion_label = "Básica primaria" if educ_var_u == "P6210" & educacion_cod == 3
replace educacion_label = "Básica secundaria" if educ_var_u == "P6210" & educacion_cod == 4
replace educacion_label = "Media" if educ_var_u == "P6210" & educacion_cod == 5
replace educacion_label = "Superior o universitaria" if educ_var_u == "P6210" & educacion_cod == 6
replace educacion_label = "No sabe, no informa" if educ_var_u == "P6210" & educacion_cod == 9


*-------------------------------
* NIVEL_MAS_ALTO para 2015-2019
* Misma estructura agregada de P6210
*-------------------------------

replace educacion_label = "Ninguno" if educ_var_u == "NIVEL_MAS_ALTO" & inrange(anio, 2015, 2019) & educacion_cod == 1
replace educacion_label = "Preescolar" if educ_var_u == "NIVEL_MAS_ALTO" & inrange(anio, 2015, 2019) & educacion_cod == 2
replace educacion_label = "Básica primaria" if educ_var_u == "NIVEL_MAS_ALTO" & inrange(anio, 2015, 2019) & educacion_cod == 3
replace educacion_label = "Básica secundaria" if educ_var_u == "NIVEL_MAS_ALTO" & inrange(anio, 2015, 2019) & educacion_cod == 4
replace educacion_label = "Media" if educ_var_u == "NIVEL_MAS_ALTO" & inrange(anio, 2015, 2019) & educacion_cod == 5
replace educacion_label = "Superior o universitaria" if educ_var_u == "NIVEL_MAS_ALTO" & inrange(anio, 2015, 2019) & educacion_cod == 6
replace educacion_label = "No sabe, no informa" if educ_var_u == "NIVEL_MAS_ALTO" & inrange(anio, 2015, 2019) & educacion_cod == 9


*-------------------------------
* NIVEL_MAS_ALTO para 2022
* OJO: solo usar este bloque si en 2022 sí viene detallada
*-------------------------------

replace educacion_label = "Ninguno" if educ_var_u == "NIVEL_MAS_ALTO" & anio == 2022 & educacion_cod == 1
replace educacion_label = "Preescolar" if educ_var_u == "NIVEL_MAS_ALTO" & anio == 2022 & educacion_cod == 2
replace educacion_label = "Básica primaria" if educ_var_u == "NIVEL_MAS_ALTO" & anio == 2022 & educacion_cod == 3
replace educacion_label = "Básica secundaria" if educ_var_u == "NIVEL_MAS_ALTO" & anio == 2022 & educacion_cod == 4
replace educacion_label = "Media académica" if educ_var_u == "NIVEL_MAS_ALTO" & anio == 2022 & educacion_cod == 5
replace educacion_label = "Media técnica" if educ_var_u == "NIVEL_MAS_ALTO" & anio == 2022 & educacion_cod == 6
replace educacion_label = "Normalista" if educ_var_u == "NIVEL_MAS_ALTO" & anio == 2022 & educacion_cod == 7
replace educacion_label = "Técnica profesional" if educ_var_u == "NIVEL_MAS_ALTO" & anio == 2022 & educacion_cod == 8
replace educacion_label = "Tecnológica" if educ_var_u == "NIVEL_MAS_ALTO" & anio == 2022 & educacion_cod == 9
replace educacion_label = "Universitaria" if educ_var_u == "NIVEL_MAS_ALTO" & anio == 2022 & educacion_cod == 10
replace educacion_label = "Especialización" if educ_var_u == "NIVEL_MAS_ALTO" & anio == 2022 & educacion_cod == 11
replace educacion_label = "Maestría" if educ_var_u == "NIVEL_MAS_ALTO" & anio == 2022 & educacion_cod == 12
replace educacion_label = "Doctorado" if educ_var_u == "NIVEL_MAS_ALTO" & anio == 2022 & educacion_cod == 13
replace educacion_label = "No sabe, no informa" if educ_var_u == "NIVEL_MAS_ALTO" & anio == 2022 & educacion_cod == 99


*-------------------------------
* P3042
* Categorías detalladas
*-------------------------------

replace educacion_label = "Ninguno" if educ_var_u == "P3042" & educacion_cod == 1
replace educacion_label = "Preescolar" if educ_var_u == "P3042" & educacion_cod == 2
replace educacion_label = "Básica primaria" if educ_var_u == "P3042" & educacion_cod == 3
replace educacion_label = "Básica secundaria" if educ_var_u == "P3042" & educacion_cod == 4
replace educacion_label = "Media académica" if educ_var_u == "P3042" & educacion_cod == 5
replace educacion_label = "Media técnica" if educ_var_u == "P3042" & educacion_cod == 6
replace educacion_label = "Normalista" if educ_var_u == "P3042" & educacion_cod == 7
replace educacion_label = "Técnica profesional" if educ_var_u == "P3042" & educacion_cod == 8
replace educacion_label = "Tecnológica" if educ_var_u == "P3042" & educacion_cod == 9
replace educacion_label = "Universitaria" if educ_var_u == "P3042" & educacion_cod == 10
replace educacion_label = "Especialización" if educ_var_u == "P3042" & educacion_cod == 11
replace educacion_label = "Maestría" if educ_var_u == "P3042" & educacion_cod == 12
replace educacion_label = "Doctorado" if educ_var_u == "P3042" & educacion_cod == 13
replace educacion_label = "No sabe, no informa" if educ_var_u == "P3042" & educacion_cod == 99


*====================================================
* 3. Crear labels limpias de tamaño de empresa
*====================================================

gen str40 tamano_label = ""


*-------------------------------
* P6870
*-------------------------------

replace tamano_label = "Solo" if tamano_var_u == "P6870" & tamano_empresa_cod == 1
replace tamano_label = "2-3" if tamano_var_u == "P6870" & tamano_empresa_cod == 2
replace tamano_label = "4-5" if tamano_var_u == "P6870" & tamano_empresa_cod == 3
replace tamano_label = "6-10" if tamano_var_u == "P6870" & tamano_empresa_cod == 4
replace tamano_label = "11-19" if tamano_var_u == "P6870" & tamano_empresa_cod == 5
replace tamano_label = "20-30" if tamano_var_u == "P6870" & tamano_empresa_cod == 6
replace tamano_label = "31-50" if tamano_var_u == "P6870" & tamano_empresa_cod == 7
replace tamano_label = "51-100" if tamano_var_u == "P6870" & tamano_empresa_cod == 8
replace tamano_label = "101+" if tamano_var_u == "P6870" & tamano_empresa_cod == 9


*-------------------------------
* P3069
*-------------------------------

replace tamano_label = "Solo" if tamano_var_u == "P3069" & tamano_empresa_cod == 1
replace tamano_label = "2-3" if tamano_var_u == "P3069" & tamano_empresa_cod == 2
replace tamano_label = "4-5" if tamano_var_u == "P3069" & tamano_empresa_cod == 3
replace tamano_label = "6-10" if tamano_var_u == "P3069" & tamano_empresa_cod == 4
replace tamano_label = "11-19" if tamano_var_u == "P3069" & tamano_empresa_cod == 5
replace tamano_label = "20-30" if tamano_var_u == "P3069" & tamano_empresa_cod == 6
replace tamano_label = "31-50" if tamano_var_u == "P3069" & tamano_empresa_cod == 7
replace tamano_label = "51-100" if tamano_var_u == "P3069" & tamano_empresa_cod == 8
replace tamano_label = "101-200" if tamano_var_u == "P3069" & tamano_empresa_cod == 9
replace tamano_label = "201+" if tamano_var_u == "P3069" & tamano_empresa_cod == 10


*====================================================
* 4. Crear orden de categorías
*====================================================

gen double educacion_orden = .

replace educacion_orden = 1 if educacion_label == "Ninguno"
replace educacion_orden = 2 if educacion_label == "Preescolar"
replace educacion_orden = 3 if educacion_label == "Básica primaria"
replace educacion_orden = 4 if educacion_label == "Básica secundaria"
replace educacion_orden = 5 if educacion_label == "Media"
replace educacion_orden = 5.1 if educacion_label == "Media académica"
replace educacion_orden = 5.2 if educacion_label == "Media técnica"
replace educacion_orden = 5.3 if educacion_label == "Normalista"
replace educacion_orden = 6 if educacion_label == "Superior o universitaria"
replace educacion_orden = 6.1 if educacion_label == "Técnica profesional"
replace educacion_orden = 6.2 if educacion_label == "Tecnológica"
replace educacion_orden = 6.3 if educacion_label == "Universitaria"
replace educacion_orden = 6.4 if educacion_label == "Especialización"
replace educacion_orden = 6.5 if educacion_label == "Maestría"
replace educacion_orden = 6.6 if educacion_label == "Doctorado"
replace educacion_orden = 99 if educacion_label == "No sabe, no informa"


gen double tamano_orden = .

replace tamano_orden = 1 if tamano_label == "Solo"
replace tamano_orden = 2 if tamano_label == "2-3"
replace tamano_orden = 3 if tamano_label == "4-5"
replace tamano_orden = 4 if tamano_label == "6-10"
replace tamano_orden = 5 if tamano_label == "11-19"
replace tamano_orden = 6 if tamano_label == "20-30"
replace tamano_orden = 7 if tamano_label == "31-50"
replace tamano_orden = 8 if tamano_label == "51-100"
replace tamano_orden = 9 if tamano_label == "101+"
replace tamano_orden = 10 if tamano_label == "101-200"
replace tamano_orden = 11 if tamano_label == "201+"


*====================================================
* 5. Auditoría antes de filtrar
*====================================================

gen byte sin_label_educ = educacion_label == "" & !missing(educacion_cod)
gen byte sin_label_tamano = tamano_label == "" & !missing(tamano_empresa_cod)
gen byte fila = 1

preserve

collapse ///
    (sum) filas = fila ///
    (sum) ingreso_hora_valido_total = ingreso_hora_valido ///
    (sum) sin_label_educ = sin_label_educ ///
    (sum) sin_label_tamano = sin_label_tamano, ///
    by(anio educ_var_original tamano_var_original)

sort anio

export excel using "Outputs/tables/ingreso_hora_anio_educacion_tamano.xlsx", ///
    sheet("00_auditoria") ///
    firstrow(variables) ///
    replace

restore


*====================================================
* 6. Filtrar observaciones válidas
*====================================================

keep if ingreso_hora_valido == 1
keep if !missing(ingreso_laboral_hora)
keep if !missing(factor_expansion_anual)
keep if factor_expansion_anual > 0

keep if !missing(educacion_cod)
keep if !missing(tamano_empresa_cod)

drop if educacion_label == ""
drop if tamano_label == ""


*====================================================
* 7. Calcular ingreso hora expandido
*====================================================

gen double ingreso_hora_expandido = ingreso_laboral_hora * factor_expansion_anual


*====================================================
* 8. Colapsar por año, educación y tamaño
*====================================================

collapse ///
    (sum) trabajadores = factor_expansion_anual ///
    (sum) ingreso_hora_total_expandido = ingreso_hora_expandido ///
    (count) observaciones = ingreso_laboral_hora, ///
    by(anio ///
       educ_var_original educacion_cod educacion_label educacion_orden ///
       tamano_var_original tamano_empresa_cod tamano_label tamano_orden)

gen double ingreso_hora_promedio = ingreso_hora_total_expandido / trabajadores

bysort anio: egen double trabajadores_total_anio = total(trabajadores)

gen double participacion_empleo = trabajadores / trabajadores_total_anio

sort anio educacion_orden tamano_orden


*====================================================
* 9. Verificar años finales
*====================================================

di "===================================================="
di "AÑOS EN TABLA FINAL EXPORTADA"
di "===================================================="

tab anio, missing

preserve

collapse ///
    (sum) trabajadores = trabajadores ///
    (count) filas = ingreso_hora_promedio, ///
    by(anio)

sort anio

export excel using "Outputs/tables/ingreso_hora_anio_educacion_tamano.xlsx", ///
    sheet("01_check_anios") ///
    firstrow(variables) ///
    sheetreplace

restore


*====================================================
* 10. Ordenar y exportar tabla principal
*====================================================

order anio ///
      educ_var_original educacion_cod educacion_label educacion_orden ///
      tamano_var_original tamano_empresa_cod tamano_label tamano_orden ///
      trabajadores trabajadores_total_anio participacion_empleo ///
      ingreso_hora_promedio ingreso_hora_total_expandido observaciones

format trabajadores %15.0fc
format trabajadores_total_anio %15.0fc
format participacion_empleo %9.4f
format ingreso_hora_promedio %15.2fc
format ingreso_hora_total_expandido %18.0fc
format observaciones %12.0fc

export excel using "Outputs/tables/ingreso_hora_anio_educacion_tamano.xlsx", ///
    sheet("02_base_final") ///
    firstrow(variables) ///
    sheetreplace
	
****************************************************
* REGENERAR TABLA:
* INGRESO HORA POR AÑO, FORMALIDAD Y TAMAÑO
* DESDE BASE ARMONIZADA
****************************************************

clear all
set more off

cd "C:/Users/jorge/Documents/Databases/GEIH"

use "Outputs/tables/GEIH_consolidada_variables_interes_2008_2025.dta", clear


*====================================================
* 0. Revisar años disponibles
*====================================================

di "===================================================="
di "AÑOS DISPONIBLES EN LA BASE ARMONIZADA"
di "===================================================="

tab anio, missing
tab anio pension_var_original, missing
tab anio tamano_var_original, missing


*====================================================
* 1. Normalizar nombres originales
*====================================================

gen str40 pension_var_u = upper(strtrim(pension_var_original))
gen str40 tamano_var_u  = upper(strtrim(tamano_var_original))


*====================================================
* 2. Crear variable de formalidad
*====================================================
* P6920:
* 1 = Sí cotiza a pensión
* 2 = No cotiza a pensión
* 3 = Ya es pensionado

gen str40 formalidad_label = ""
gen byte formalidad_cod = .

replace formalidad_cod = 1 if cotiza_pension_cod == 1
replace formalidad_label = "Formal" if cotiza_pension_cod == 1

replace formalidad_cod = 2 if cotiza_pension_cod == 2
replace formalidad_label = "Informal" if cotiza_pension_cod == 2

replace formalidad_cod = 3 if cotiza_pension_cod == 3
replace formalidad_label = "Pensionado ocupado" if cotiza_pension_cod == 3


*====================================================
* 3. Crear labels limpias de tamaño de empresa
*====================================================

gen str40 tamano_label = ""


*-------------------------------
* P6870
*-------------------------------

replace tamano_label = "Solo" if tamano_var_u == "P6870" & tamano_empresa_cod == 1
replace tamano_label = "2-3" if tamano_var_u == "P6870" & tamano_empresa_cod == 2
replace tamano_label = "4-5" if tamano_var_u == "P6870" & tamano_empresa_cod == 3
replace tamano_label = "6-10" if tamano_var_u == "P6870" & tamano_empresa_cod == 4
replace tamano_label = "11-19" if tamano_var_u == "P6870" & tamano_empresa_cod == 5
replace tamano_label = "20-30" if tamano_var_u == "P6870" & tamano_empresa_cod == 6
replace tamano_label = "31-50" if tamano_var_u == "P6870" & tamano_empresa_cod == 7
replace tamano_label = "51-100" if tamano_var_u == "P6870" & tamano_empresa_cod == 8
replace tamano_label = "101+" if tamano_var_u == "P6870" & tamano_empresa_cod == 9


*-------------------------------
* P3069
*-------------------------------

replace tamano_label = "Solo" if tamano_var_u == "P3069" & tamano_empresa_cod == 1
replace tamano_label = "2-3" if tamano_var_u == "P3069" & tamano_empresa_cod == 2
replace tamano_label = "4-5" if tamano_var_u == "P3069" & tamano_empresa_cod == 3
replace tamano_label = "6-10" if tamano_var_u == "P3069" & tamano_empresa_cod == 4
replace tamano_label = "11-19" if tamano_var_u == "P3069" & tamano_empresa_cod == 5
replace tamano_label = "20-30" if tamano_var_u == "P3069" & tamano_empresa_cod == 6
replace tamano_label = "31-50" if tamano_var_u == "P3069" & tamano_empresa_cod == 7
replace tamano_label = "51-100" if tamano_var_u == "P3069" & tamano_empresa_cod == 8
replace tamano_label = "101-200" if tamano_var_u == "P3069" & tamano_empresa_cod == 9
replace tamano_label = "201+" if tamano_var_u == "P3069" & tamano_empresa_cod == 10


*====================================================
* 4. Crear orden de categorías
*====================================================

gen byte formalidad_orden = .

replace formalidad_orden = 1 if formalidad_label == "Formal"
replace formalidad_orden = 2 if formalidad_label == "Informal"
replace formalidad_orden = 3 if formalidad_label == "Pensionado ocupado"


gen double tamano_orden = .

replace tamano_orden = 1 if tamano_label == "Solo"
replace tamano_orden = 2 if tamano_label == "2-3"
replace tamano_orden = 3 if tamano_label == "4-5"
replace tamano_orden = 4 if tamano_label == "6-10"
replace tamano_orden = 5 if tamano_label == "11-19"
replace tamano_orden = 6 if tamano_label == "20-30"
replace tamano_orden = 7 if tamano_label == "31-50"
replace tamano_orden = 8 if tamano_label == "51-100"
replace tamano_orden = 9 if tamano_label == "101+"
replace tamano_orden = 10 if tamano_label == "101-200"
replace tamano_orden = 11 if tamano_label == "201+"


*====================================================
* 5. Auditoría antes de filtrar
*====================================================

gen byte sin_label_formalidad = formalidad_label == "" & !missing(cotiza_pension_cod)
gen byte sin_label_tamano = tamano_label == "" & !missing(tamano_empresa_cod)
gen byte fila = 1

preserve

collapse ///
    (sum) filas = fila ///
    (sum) ingreso_hora_valido_total = ingreso_hora_valido ///
    (sum) sin_label_formalidad = sin_label_formalidad ///
    (sum) sin_label_tamano = sin_label_tamano, ///
    by(anio pension_var_original tamano_var_original)

sort anio

export excel using "Outputs/tables/ingreso_hora_anio_formalidad_tamano.xlsx", ///
    sheet("00_auditoria") ///
    firstrow(variables) ///
    replace

restore


*====================================================
* 6. Filtrar observaciones válidas
*====================================================

keep if ingreso_hora_valido == 1
keep if !missing(ingreso_laboral_hora)
keep if !missing(factor_expansion_anual)
keep if factor_expansion_anual > 0

keep if !missing(cotiza_pension_cod)
keep if !missing(tamano_empresa_cod)

drop if formalidad_label == ""
drop if tamano_label == ""


*====================================================
* 7. Calcular ingreso hora expandido
*====================================================

gen double ingreso_hora_expandido = ingreso_laboral_hora * factor_expansion_anual


*====================================================
* 8. Colapsar por año, formalidad y tamaño
*====================================================

collapse ///
    (sum) trabajadores = factor_expansion_anual ///
    (sum) ingreso_hora_total_expandido = ingreso_hora_expandido ///
    (count) observaciones = ingreso_laboral_hora, ///
    by(anio ///
       pension_var_original formalidad_cod formalidad_label formalidad_orden ///
       tamano_var_original tamano_empresa_cod tamano_label tamano_orden)

gen double ingreso_hora_promedio = ingreso_hora_total_expandido / trabajadores

bysort anio: egen double trabajadores_total_anio = total(trabajadores)

gen double participacion_empleo = trabajadores / trabajadores_total_anio

bysort anio formalidad_label: egen double trab_total_form_anio = total(trabajadores)

gen double part_dentro_formalidad = trabajadores / trab_total_form_anio

sort anio formalidad_orden tamano_orden


*====================================================
* 9. Verificar años finales
*====================================================

di "===================================================="
di "AÑOS EN TABLA FINAL EXPORTADA"
di "===================================================="

tab anio, missing

preserve

collapse ///
    (sum) trabajadores = trabajadores ///
    (count) filas = ingreso_hora_promedio, ///
    by(anio)

sort anio

export excel using "Outputs/tables/ingreso_hora_anio_formalidad_tamano.xlsx", ///
    sheet("01_check_anios") ///
    firstrow(variables) ///
    sheetreplace

restore


*====================================================
* 10. Ordenar y exportar tabla principal
*====================================================

order anio ///
      pension_var_original formalidad_cod formalidad_label formalidad_orden ///
      tamano_var_original tamano_empresa_cod tamano_label tamano_orden ///
      trabajadores trabajadores_total_anio participacion_empleo ///
      trab_total_form_anio part_dentro_formalidad ///
      ingreso_hora_promedio ingreso_hora_total_expandido observaciones

format trabajadores %15.0fc
format trabajadores_total_anio %15.0fc
format participacion_empleo %9.4f
format trab_total_form_anio %15.0fc
format part_dentro_formalidad %9.4f
format ingreso_hora_promedio %15.2fc
format ingreso_hora_total_expandido %18.0fc
format observaciones %12.0fc

save "Outputs/tables/ingreso_hora_anio_formalidad_tamano.dta", replace

export excel using "Outputs/tables/ingreso_hora_anio_formalidad_tamano.xlsx", ///
    sheet("02_base_final") ///
    firstrow(variables) ///
    sheetreplace


di "===================================================="
di "TABLA REGENERADA CORRECTAMENTE"
di "Archivo:"
di "Outputs/tables/ingreso_hora_anio_formalidad_tamano.xlsx"
di "Hojas:"
di "00_auditoria"
di "01_check_anios"
di "02_base_final"
di "===================================================="

****************************************************
* REGENERAR TABLA:
* INGRESO HORA POR AÑO, SEXO Y TAMAÑO
* DESDE BASE ARMONIZADA
****************************************************

clear all
set more off

cd "C:/Users/jorge/Documents/Databases/GEIH"

use "Outputs/tables/GEIH_consolidada_variables_interes_2008_2025.dta", clear


*====================================================
* 0. Revisar años disponibles
*====================================================

di "===================================================="
di "AÑOS DISPONIBLES EN LA BASE ARMONIZADA"
di "===================================================="

tab anio, missing
tab anio sexo_var_original, missing
tab anio tamano_var_original, missing


*====================================================
* 1. Normalizar nombres originales
*====================================================

gen str40 sexo_var_u   = upper(strtrim(sexo_var_original))
gen str40 tamano_var_u = upper(strtrim(tamano_var_original))


*====================================================
* 2. Crear variable de sexo
*====================================================
* Usualmente:
* 1 = Hombre
* 2 = Mujer

gen str20 sexo_label = ""
gen byte sexo_orden = .

replace sexo_label = "Hombre" if sexo_cod == 1
replace sexo_label = "Mujer" if sexo_cod == 2

replace sexo_orden = 1 if sexo_label == "Hombre"
replace sexo_orden = 2 if sexo_label == "Mujer"


*====================================================
* 3. Crear labels limpias de tamaño de empresa
*====================================================

gen str40 tamano_label = ""


*-------------------------------
* P6870
*-------------------------------

replace tamano_label = "Solo" if tamano_var_u == "P6870" & tamano_empresa_cod == 1
replace tamano_label = "2-3" if tamano_var_u == "P6870" & tamano_empresa_cod == 2
replace tamano_label = "4-5" if tamano_var_u == "P6870" & tamano_empresa_cod == 3
replace tamano_label = "6-10" if tamano_var_u == "P6870" & tamano_empresa_cod == 4
replace tamano_label = "11-19" if tamano_var_u == "P6870" & tamano_empresa_cod == 5
replace tamano_label = "20-30" if tamano_var_u == "P6870" & tamano_empresa_cod == 6
replace tamano_label = "31-50" if tamano_var_u == "P6870" & tamano_empresa_cod == 7
replace tamano_label = "51-100" if tamano_var_u == "P6870" & tamano_empresa_cod == 8
replace tamano_label = "101+" if tamano_var_u == "P6870" & tamano_empresa_cod == 9


*-------------------------------
* P3069
*-------------------------------

replace tamano_label = "Solo" if tamano_var_u == "P3069" & tamano_empresa_cod == 1
replace tamano_label = "2-3" if tamano_var_u == "P3069" & tamano_empresa_cod == 2
replace tamano_label = "4-5" if tamano_var_u == "P3069" & tamano_empresa_cod == 3
replace tamano_label = "6-10" if tamano_var_u == "P3069" & tamano_empresa_cod == 4
replace tamano_label = "11-19" if tamano_var_u == "P3069" & tamano_empresa_cod == 5
replace tamano_label = "20-30" if tamano_var_u == "P3069" & tamano_empresa_cod == 6
replace tamano_label = "31-50" if tamano_var_u == "P3069" & tamano_empresa_cod == 7
replace tamano_label = "51-100" if tamano_var_u == "P3069" & tamano_empresa_cod == 8
replace tamano_label = "101-200" if tamano_var_u == "P3069" & tamano_empresa_cod == 9
replace tamano_label = "201+" if tamano_var_u == "P3069" & tamano_empresa_cod == 10


*====================================================
* 4. Crear orden de tamaño
*====================================================

gen double tamano_orden = .

replace tamano_orden = 1 if tamano_label == "Solo"
replace tamano_orden = 2 if tamano_label == "2-3"
replace tamano_orden = 3 if tamano_label == "4-5"
replace tamano_orden = 4 if tamano_label == "6-10"
replace tamano_orden = 5 if tamano_label == "11-19"
replace tamano_orden = 6 if tamano_label == "20-30"
replace tamano_orden = 7 if tamano_label == "31-50"
replace tamano_orden = 8 if tamano_label == "51-100"
replace tamano_orden = 9 if tamano_label == "101+"
replace tamano_orden = 10 if tamano_label == "101-200"
replace tamano_orden = 11 if tamano_label == "201+"


*====================================================
* 5. Auditoría antes de filtrar
*====================================================

gen byte sin_label_sexo = sexo_label == "" & !missing(sexo_cod)
gen byte sin_label_tamano = tamano_label == "" & !missing(tamano_empresa_cod)
gen byte fila = 1

preserve

collapse ///
    (sum) filas = fila ///
    (sum) ingreso_hora_valido_total = ingreso_hora_valido ///
    (sum) sin_label_sexo = sin_label_sexo ///
    (sum) sin_label_tamano = sin_label_tamano, ///
    by(anio sexo_var_original tamano_var_original)

sort anio

export excel using "Outputs/tables/ingreso_hora_anio_sexo_tamano.xlsx", ///
    sheet("00_auditoria") ///
    firstrow(variables) ///
    replace

restore


*====================================================
* 6. Filtrar observaciones válidas
*====================================================

keep if ingreso_hora_valido == 1
keep if !missing(ingreso_laboral_hora)
keep if !missing(factor_expansion_anual)
keep if factor_expansion_anual > 0

keep if !missing(sexo_cod)
keep if !missing(tamano_empresa_cod)

drop if sexo_label == ""
drop if tamano_label == ""


*====================================================
* 7. Calcular ingreso hora expandido
*====================================================

gen double ingreso_hora_expandido = ingreso_laboral_hora * factor_expansion_anual


*====================================================
* 8. Colapsar por año, sexo y tamaño
*====================================================

collapse ///
    (sum) trabajadores = factor_expansion_anual ///
    (sum) ingreso_hora_total_expandido = ingreso_hora_expandido ///
    (count) observaciones = ingreso_laboral_hora, ///
    by(anio ///
       sexo_var_original sexo_cod sexo_label sexo_orden ///
       tamano_var_original tamano_empresa_cod tamano_label tamano_orden)

gen double ingreso_hora_promedio = ingreso_hora_total_expandido / trabajadores

bysort anio: egen double trabajadores_total_anio = total(trabajadores)

gen double participacion_empleo = trabajadores / trabajadores_total_anio

bysort anio sexo_label: egen double trab_total_sexo_anio = total(trabajadores)

gen double part_dentro_sexo = trabajadores / trab_total_sexo_anio

sort anio sexo_orden tamano_orden


*====================================================
* 9. Verificar años finales
*====================================================

di "===================================================="
di "AÑOS EN TABLA FINAL EXPORTADA"
di "===================================================="

tab anio, missing

preserve

collapse ///
    (sum) trabajadores = trabajadores ///
    (count) filas = ingreso_hora_promedio, ///
    by(anio)

sort anio

export excel using "Outputs/tables/ingreso_hora_anio_sexo_tamano.xlsx", ///
    sheet("01_check_anios") ///
    firstrow(variables) ///
    sheetreplace

restore


*====================================================
* 10. Ordenar y exportar tabla principal
*====================================================

order anio ///
      sexo_var_original sexo_cod sexo_label sexo_orden ///
      tamano_var_original tamano_empresa_cod tamano_label tamano_orden ///
      trabajadores trabajadores_total_anio participacion_empleo ///
      trab_total_sexo_anio part_dentro_sexo ///
      ingreso_hora_promedio ingreso_hora_total_expandido observaciones

format trabajadores %15.0fc
format trabajadores_total_anio %15.0fc
format participacion_empleo %9.4f
format trab_total_sexo_anio %15.0fc
format part_dentro_sexo %9.4f
format ingreso_hora_promedio %15.2fc
format ingreso_hora_total_expandido %18.0fc
format observaciones %12.0fc

export excel using "Outputs/tables/ingreso_hora_anio_sexo_tamano.xlsx", ///
    sheet("02_base_final") ///
    firstrow(variables) ///
    sheetreplace
	
****************************************************
* REGENERAR TABLA:
* INGRESO HORA POR AÑO, SECTOR HOMOLOGADO Y TAMAÑO
* DESDE BASE ARMONIZADA
****************************************************

clear all
set more off

cd "C:/Users/jorge/Documents/Databases/GEIH"

use "Outputs/tables/GEIH_consolidada_variables_interes_2008_2025.dta", clear


*====================================================
* 0. Revisar años y variables disponibles
*====================================================

di "===================================================="
di "AÑOS DISPONIBLES EN LA BASE ARMONIZADA"
di "===================================================="

tab anio, missing
tab anio sector_var_original, missing
tab anio tamano_var_original, missing


*====================================================
* 1. Normalizar nombres originales
*====================================================

gen str40 sector_var_u = upper(strtrim(sector_var_original))
gen str40 tamano_var_u = upper(strtrim(tamano_var_original))


*====================================================
* 2. Crear sector homologado CIIU Rev. 3 / Rev. 4
*====================================================
* Esta variable es analítica y comparable.
* No corresponde exactamente a las letras oficiales
* de cada revisión, sino a grupos comparables entre revisiones.

gen double sector_hom_cod = .
gen str120 sector_hom_label = ""
gen double sector_hom_orden = .


*====================================================
* 2.1. Rev. 3: RAMA2D
*====================================================

* 1. Agricultura, ganadería, silvicultura y pesca
replace sector_hom_cod = 1 if sector_var_u == "RAMA2D" & inlist(sector_cod, 1, 2, 5)

* 2. Minas y canteras
replace sector_hom_cod = 2 if sector_var_u == "RAMA2D" & inrange(sector_cod, 10, 14)

* 3. Industrias manufactureras
replace sector_hom_cod = 3 if sector_var_u == "RAMA2D" & inrange(sector_cod, 15, 37)

* 4. Electricidad, gas, agua y saneamiento
* Rev. 3: electricidad/gas/agua = 40,41
* Rev. 3: saneamiento = 90
replace sector_hom_cod = 4 if sector_var_u == "RAMA2D" & inlist(sector_cod, 40, 41, 90)

* 5. Construcción
replace sector_hom_cod = 5 if sector_var_u == "RAMA2D" & sector_cod == 45

* 6. Comercio y reparación
replace sector_hom_cod = 6 if sector_var_u == "RAMA2D" & inrange(sector_cod, 50, 52)

* 7. Alojamiento y servicios de comida
replace sector_hom_cod = 7 if sector_var_u == "RAMA2D" & sector_cod == 55

* 8. Transporte y almacenamiento
replace sector_hom_cod = 8 if sector_var_u == "RAMA2D" & inrange(sector_cod, 60, 63)

* 9. Información y comunicaciones
* Rev. 3: comunicaciones = 64
* Rev. 3: informática y actividades conexas = 72
replace sector_hom_cod = 9 if sector_var_u == "RAMA2D" & inlist(sector_cod, 64, 72)

* 10. Actividades financieras y de seguros
replace sector_hom_cod = 10 if sector_var_u == "RAMA2D" & inrange(sector_cod, 65, 67)

* 11. Inmobiliarias, profesionales y administrativas
* Rev. 3: inmobiliarias = 70
* alquiler maquinaria/equipo = 71
* investigación y desarrollo = 73
* otras actividades empresariales = 74
replace sector_hom_cod = 11 if sector_var_u == "RAMA2D" & inlist(sector_cod, 70, 71, 73, 74)

* 12. Administración pública y defensa
replace sector_hom_cod = 12 if sector_var_u == "RAMA2D" & sector_cod == 75

* 13. Educación
replace sector_hom_cod = 13 if sector_var_u == "RAMA2D" & sector_cod == 80

* 14. Salud y asistencia social
replace sector_hom_cod = 14 if sector_var_u == "RAMA2D" & sector_cod == 85

* 15. Artes, recreación y otros servicios
* Rev. 3: asociaciones, entretenimiento, otros servicios = 91-93
replace sector_hom_cod = 15 if sector_var_u == "RAMA2D" & inrange(sector_cod, 91, 93)

* 16. Hogares como empleadores
replace sector_hom_cod = 16 if sector_var_u == "RAMA2D" & inrange(sector_cod, 95, 97)

* 17. Organizaciones extraterritoriales
replace sector_hom_cod = 17 if sector_var_u == "RAMA2D" & sector_cod == 99


*====================================================
* 2.2. Rev. 4: RAMA2D_R4
*====================================================

* 1. Agricultura, ganadería, silvicultura y pesca
replace sector_hom_cod = 1 if sector_var_u == "RAMA2D_R4" & inrange(sector_cod, 1, 3)

* 2. Minas y canteras
replace sector_hom_cod = 2 if sector_var_u == "RAMA2D_R4" & inrange(sector_cod, 5, 9)

* 3. Industrias manufactureras
replace sector_hom_cod = 3 if sector_var_u == "RAMA2D_R4" & inrange(sector_cod, 10, 33)

* 4. Electricidad, gas, agua y saneamiento
replace sector_hom_cod = 4 if sector_var_u == "RAMA2D_R4" & inrange(sector_cod, 35, 39)

* 5. Construcción
replace sector_hom_cod = 5 if sector_var_u == "RAMA2D_R4" & inrange(sector_cod, 41, 43)

* 6. Comercio y reparación
replace sector_hom_cod = 6 if sector_var_u == "RAMA2D_R4" & inrange(sector_cod, 45, 47)

* 7. Alojamiento y servicios de comida
replace sector_hom_cod = 7 if sector_var_u == "RAMA2D_R4" & inrange(sector_cod, 55, 56)

* 8. Transporte y almacenamiento
replace sector_hom_cod = 8 if sector_var_u == "RAMA2D_R4" & inrange(sector_cod, 49, 53)

* 9. Información y comunicaciones
replace sector_hom_cod = 9 if sector_var_u == "RAMA2D_R4" & inrange(sector_cod, 58, 63)

* 10. Actividades financieras y de seguros
replace sector_hom_cod = 10 if sector_var_u == "RAMA2D_R4" & inrange(sector_cod, 64, 66)

* 11. Inmobiliarias, profesionales y administrativas
replace sector_hom_cod = 11 if sector_var_u == "RAMA2D_R4" & ///
    (sector_cod == 68 | inrange(sector_cod, 69, 75) | inrange(sector_cod, 77, 82))

* 12. Administración pública y defensa
replace sector_hom_cod = 12 if sector_var_u == "RAMA2D_R4" & sector_cod == 84

* 13. Educación
replace sector_hom_cod = 13 if sector_var_u == "RAMA2D_R4" & sector_cod == 85

* 14. Salud y asistencia social
replace sector_hom_cod = 14 if sector_var_u == "RAMA2D_R4" & inrange(sector_cod, 86, 88)

* 15. Artes, recreación y otros servicios
replace sector_hom_cod = 15 if sector_var_u == "RAMA2D_R4" & inrange(sector_cod, 90, 96)

* 16. Hogares como empleadores
replace sector_hom_cod = 16 if sector_var_u == "RAMA2D_R4" & inrange(sector_cod, 97, 98)

* 17. Organizaciones extraterritoriales
replace sector_hom_cod = 17 if sector_var_u == "RAMA2D_R4" & sector_cod == 99


*====================================================
* 2.3. Labels y orden del sector homologado
*====================================================

replace sector_hom_label = "Agricultura, ganadería, silvicultura y pesca" if sector_hom_cod == 1
replace sector_hom_label = "Minas y canteras" if sector_hom_cod == 2
replace sector_hom_label = "Industrias manufactureras" if sector_hom_cod == 3
replace sector_hom_label = "Electricidad, gas, agua y saneamiento" if sector_hom_cod == 4
replace sector_hom_label = "Construcción" if sector_hom_cod == 5
replace sector_hom_label = "Comercio y reparación" if sector_hom_cod == 6
replace sector_hom_label = "Alojamiento y servicios de comida" if sector_hom_cod == 7
replace sector_hom_label = "Transporte y almacenamiento" if sector_hom_cod == 8
replace sector_hom_label = "Información y comunicaciones" if sector_hom_cod == 9
replace sector_hom_label = "Actividades financieras y de seguros" if sector_hom_cod == 10
replace sector_hom_label = "Inmobiliarias, profesionales y administrativas" if sector_hom_cod == 11
replace sector_hom_label = "Administración pública y defensa" if sector_hom_cod == 12
replace sector_hom_label = "Educación" if sector_hom_cod == 13
replace sector_hom_label = "Salud y asistencia social" if sector_hom_cod == 14
replace sector_hom_label = "Artes, recreación y otros servicios" if sector_hom_cod == 15
replace sector_hom_label = "Hogares como empleadores" if sector_hom_cod == 16
replace sector_hom_label = "Organizaciones extraterritoriales" if sector_hom_cod == 17

replace sector_hom_orden = sector_hom_cod


*====================================================
* 3. Crear labels limpias de tamaño de empresa
*====================================================

gen str40 tamano_label = ""

replace tamano_label = "Solo" if tamano_var_u == "P6870" & tamano_empresa_cod == 1
replace tamano_label = "2-3" if tamano_var_u == "P6870" & tamano_empresa_cod == 2
replace tamano_label = "4-5" if tamano_var_u == "P6870" & tamano_empresa_cod == 3
replace tamano_label = "6-10" if tamano_var_u == "P6870" & tamano_empresa_cod == 4
replace tamano_label = "11-19" if tamano_var_u == "P6870" & tamano_empresa_cod == 5
replace tamano_label = "20-30" if tamano_var_u == "P6870" & tamano_empresa_cod == 6
replace tamano_label = "31-50" if tamano_var_u == "P6870" & tamano_empresa_cod == 7
replace tamano_label = "51-100" if tamano_var_u == "P6870" & tamano_empresa_cod == 8
replace tamano_label = "101+" if tamano_var_u == "P6870" & tamano_empresa_cod == 9

replace tamano_label = "Solo" if tamano_var_u == "P3069" & tamano_empresa_cod == 1
replace tamano_label = "2-3" if tamano_var_u == "P3069" & tamano_empresa_cod == 2
replace tamano_label = "4-5" if tamano_var_u == "P3069" & tamano_empresa_cod == 3
replace tamano_label = "6-10" if tamano_var_u == "P3069" & tamano_empresa_cod == 4
replace tamano_label = "11-19" if tamano_var_u == "P3069" & tamano_empresa_cod == 5
replace tamano_label = "20-30" if tamano_var_u == "P3069" & tamano_empresa_cod == 6
replace tamano_label = "31-50" if tamano_var_u == "P3069" & tamano_empresa_cod == 7
replace tamano_label = "51-100" if tamano_var_u == "P3069" & tamano_empresa_cod == 8
replace tamano_label = "101-200" if tamano_var_u == "P3069" & tamano_empresa_cod == 9
replace tamano_label = "201+" if tamano_var_u == "P3069" & tamano_empresa_cod == 10

gen double tamano_orden = .

replace tamano_orden = 1 if tamano_label == "Solo"
replace tamano_orden = 2 if tamano_label == "2-3"
replace tamano_orden = 3 if tamano_label == "4-5"
replace tamano_orden = 4 if tamano_label == "6-10"
replace tamano_orden = 5 if tamano_label == "11-19"
replace tamano_orden = 6 if tamano_label == "20-30"
replace tamano_orden = 7 if tamano_label == "31-50"
replace tamano_orden = 8 if tamano_label == "51-100"
replace tamano_orden = 9 if tamano_label == "101+"
replace tamano_orden = 10 if tamano_label == "101-200"
replace tamano_orden = 11 if tamano_label == "201+"


*====================================================
* 4. Auditoría antes de filtrar
*====================================================

gen byte sin_sector_hom = missing(sector_hom_cod) & !missing(sector_cod)
gen byte sin_label_tamano = tamano_label == "" & !missing(tamano_empresa_cod)
gen byte fila = 1

preserve

collapse ///
    (sum) filas = fila ///
    (sum) ingreso_hora_valido_total = ingreso_hora_valido ///
    (sum) sin_sector_hom = sin_sector_hom ///
    (sum) sin_label_tamano = sin_label_tamano, ///
    by(anio)

sort anio

export excel using "Outputs/tables/ingreso_hora_anio_sector_hom_tamano.xlsx", ///
    sheet("00_auditoria") ///
    firstrow(variables) ///
    replace

restore


*====================================================
* 4b. Códigos sin homologación
*====================================================

preserve

keep if sin_sector_hom == 1

gen fila_sin_hom = 1

collapse ///
    (sum) filas = fila_sin_hom, ///
    by(anio sector_cod)

sort anio sector_cod

export excel using "Outputs/tables/ingreso_hora_anio_sector_hom_tamano.xlsx", ///
    sheet("00b_codigos_sin_hom") ///
    firstrow(variables) ///
    sheetreplace

restore


*====================================================
* 5. Filtrar observaciones válidas
*====================================================

keep if ingreso_hora_valido == 1
keep if !missing(ingreso_laboral_hora)
keep if !missing(factor_expansion_anual)
keep if factor_expansion_anual > 0

keep if !missing(sector_cod)
keep if !missing(tamano_empresa_cod)

drop if missing(sector_hom_cod)
drop if tamano_label == ""


*====================================================
* 6. Crear código numérico para tamaño de empresa
*====================================================

gen double tamano_hom_cod = .

replace tamano_hom_cod = 1 if tamano_label == "Solo"
replace tamano_hom_cod = 2 if tamano_label == "2-3"
replace tamano_hom_cod = 3 if tamano_label == "4-5"
replace tamano_hom_cod = 4 if tamano_label == "6-10"
replace tamano_hom_cod = 5 if tamano_label == "11-19"
replace tamano_hom_cod = 6 if tamano_label == "20-30"
replace tamano_hom_cod = 7 if tamano_label == "31-50"
replace tamano_hom_cod = 8 if tamano_label == "51-100"
replace tamano_hom_cod = 9 if tamano_label == "101+"
replace tamano_hom_cod = 10 if tamano_label == "101-200"
replace tamano_hom_cod = 11 if tamano_label == "201+"

drop if missing(tamano_hom_cod)


*====================================================
* 7. Calcular ingreso hora expandido
*====================================================

gen double ingreso_hora_expandido = ingreso_laboral_hora * factor_expansion_anual


*====================================================
* 8. Colapsar por año, sector homologado y tamaño
*    OJO: solo variables numéricas en by()
*====================================================

collapse ///
    (sum) trabajadores = factor_expansion_anual ///
    (sum) ingreso_hora_total_expandido = ingreso_hora_expandido ///
    (count) observaciones = ingreso_laboral_hora, ///
    by(anio sector_hom_cod tamano_hom_cod)

gen double ingreso_hora_promedio = ingreso_hora_total_expandido / trabajadores


*====================================================
* 9. Reconstruir labels después del collapse
*====================================================

gen str120 sector_hom_label = ""
gen double sector_hom_orden = sector_hom_cod

replace sector_hom_label = "Agricultura, ganadería, silvicultura y pesca" if sector_hom_cod == 1
replace sector_hom_label = "Minas y canteras" if sector_hom_cod == 2
replace sector_hom_label = "Industrias manufactureras" if sector_hom_cod == 3
replace sector_hom_label = "Electricidad, gas, agua y saneamiento" if sector_hom_cod == 4
replace sector_hom_label = "Construcción" if sector_hom_cod == 5
replace sector_hom_label = "Comercio y reparación" if sector_hom_cod == 6
replace sector_hom_label = "Alojamiento y servicios de comida" if sector_hom_cod == 7
replace sector_hom_label = "Transporte y almacenamiento" if sector_hom_cod == 8
replace sector_hom_label = "Información y comunicaciones" if sector_hom_cod == 9
replace sector_hom_label = "Actividades financieras y de seguros" if sector_hom_cod == 10
replace sector_hom_label = "Inmobiliarias, profesionales y administrativas" if sector_hom_cod == 11
replace sector_hom_label = "Administración pública y defensa" if sector_hom_cod == 12
replace sector_hom_label = "Educación" if sector_hom_cod == 13
replace sector_hom_label = "Salud y asistencia social" if sector_hom_cod == 14
replace sector_hom_label = "Artes, recreación y otros servicios" if sector_hom_cod == 15
replace sector_hom_label = "Hogares como empleadores" if sector_hom_cod == 16
replace sector_hom_label = "Organizaciones extraterritoriales" if sector_hom_cod == 17


gen str40 tamano_label = ""
gen double tamano_orden = tamano_hom_cod

replace tamano_label = "Solo" if tamano_hom_cod == 1
replace tamano_label = "2-3" if tamano_hom_cod == 2
replace tamano_label = "4-5" if tamano_hom_cod == 3
replace tamano_label = "6-10" if tamano_hom_cod == 4
replace tamano_label = "11-19" if tamano_hom_cod == 5
replace tamano_label = "20-30" if tamano_hom_cod == 6
replace tamano_label = "31-50" if tamano_hom_cod == 7
replace tamano_label = "51-100" if tamano_hom_cod == 8
replace tamano_label = "101+" if tamano_hom_cod == 9
replace tamano_label = "101-200" if tamano_hom_cod == 10
replace tamano_label = "201+" if tamano_hom_cod == 11


*====================================================
* 10. Participaciones
*====================================================

bysort anio: egen double trabajadores_total_anio = total(trabajadores)

gen double participacion_empleo = trabajadores / trabajadores_total_anio

bysort anio sector_hom_cod: egen double trab_total_sector_anio = total(trabajadores)

gen double part_dentro_sector = trabajadores / trab_total_sector_anio

sort anio sector_hom_orden tamano_orden


*====================================================
* 11. Verificar años finales
*====================================================

di "===================================================="
di "AÑOS EN TABLA FINAL EXPORTADA"
di "===================================================="

tab anio, missing

preserve

collapse ///
    (sum) trabajadores = trabajadores ///
    (count) filas = ingreso_hora_promedio, ///
    by(anio)

sort anio

export excel using "Outputs/tables/ingreso_hora_anio_sector_hom_tamano.xlsx", ///
    sheet("01_check_anios") ///
    firstrow(variables) ///
    sheetreplace

restore


*====================================================
* 12. Ordenar y exportar tabla principal
*====================================================

order anio ///
      sector_hom_cod sector_hom_label sector_hom_orden ///
      tamano_hom_cod tamano_label tamano_orden ///
      trabajadores trabajadores_total_anio participacion_empleo ///
      trab_total_sector_anio part_dentro_sector ///
      ingreso_hora_promedio ingreso_hora_total_expandido observaciones

format trabajadores %15.0fc
format trabajadores_total_anio %15.0fc
format participacion_empleo %9.4f
format trab_total_sector_anio %15.0fc
format part_dentro_sector %9.4f
format ingreso_hora_promedio %15.2fc
format ingreso_hora_total_expandido %18.0fc
format observaciones %12.0fc

export excel using "Outputs/tables/ingreso_hora_anio_sector_hom_tamano.xlsx", ///
    sheet("02_base_final") ///
    firstrow(variables) ///
    sheetreplace
