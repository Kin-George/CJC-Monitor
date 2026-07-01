
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
local cand_rama4d "RAMA4D_R4 RAMA4D rama4d_r4 rama4d"
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
local cand_oficio     "OFICIO_C8 oficio_c8 OFICIO oficio"


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
	str40 var_rama4d ///
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
    str40 var_oficio ///
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
        ("") ("") ("") ("") ("") ("") ///
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
	
	find_first_var, candidates("`cand_rama4d'")
	local var_rama4d "`r(var)'"

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

	find_first_var, candidates("`cand_oficio'")
	local var_oficio "`r(var)'"


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
	("`var_rama4d'") ///
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
    ("`var_oficio'") ///
    (0) ///
    ("`observacion'")

        di as error "Se omite `year': `observacion'"
        continue
    }

    if "`var_sector'" == "" {
        local observacion "`observacion' Falta sector;"
    }
	
	if "`var_rama4d'" == "" {
		local observacion "`observacion' Falta rama4d;"
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

	if "`var_oficio'" == "" {
    local observacion "`observacion' Falta oficio/CNO;"
	}

    if "`observacion'" == "" {
        local observacion "Completo"
    }
	

    post `postaudit' ///
        (`year') ///
        ("`var_factor'") ///
        ("`var_ingreso'") ///
        ("`var_sector'") ///
		("`var_rama4d'") ///
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
		("`var_oficio'") ///
        (1) ///
        ("`observacion'")

    di as text "Variables usadas en `year':"
    di as result "Factor:    `var_factor'"
    di as result "Ingreso:   `var_ingreso'"
    di as result "Sector:    `var_sector'"
	di as result "Sector2:    `var_rama4d'"
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
	di as result "Oficio:   `var_oficio'"


    *================================================
    * 4.3. Crear variables con nombres comunes
    *================================================

    gen int anio = `year'

    gen str40 factor_var_original  = "`var_factor'"
    gen str40 ingreso_var_original = "`var_ingreso'"
    gen str40 sector_var_original  = "`var_sector'"
	gen str40 rama4d_var_original  = "`var_rama4d'"
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
	gen str40 oficio_var_original   = "`var_oficio'"


    *================================================
    * 4.4. Copiar variables a formato numérico estándar
    *================================================

    make_double_from_var, newname(factor_expansion_original) varname("`var_factor'")
    make_double_from_var, newname(ingresos_laborales) varname("`var_ingreso'")
    make_double_from_var, newname(ocupado_cod) varname("`var_ocupado'")
    make_double_from_var, newname(sector_cod) varname("`var_sector'")
	make_double_from_var, newname(rama4d_cod) varname("`var_rama4d'")
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
	make_double_from_var, newname(oficio_cod) varname("`var_oficio'")

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
	gen str20 ciiu_revision_rama2d = ""
	gen str20 ciiu_revision_rama4d = ""

	* Revisión CIIU para RAMA2D
	replace ciiu_revision_rama2d = "CIIU Rev. 3" ///
    if inlist(upper(sector_var_original), "RAMA2D", "RAMA2D_R3")

	replace ciiu_revision_rama2d = "CIIU Rev. 4" ///
    if upper(sector_var_original) == "RAMA2D_R4"

	* Revisión CIIU para RAMA4D
	replace ciiu_revision_rama4d = "CIIU Rev. 3" ///
    if inlist(upper(rama4d_var_original), "RAMA4D", "RAMA4D_R3")

	replace ciiu_revision_rama4d = "CIIU Rev. 4" ///
    if upper(rama4d_var_original) == "RAMA4D_R4"

	* Mantener variable antigua para no romper código posterior
	replace ciiu_revision = ciiu_revision_rama2d


    *================================================
    * 4.8. Quedarse solo con variables de interés
    *================================================

    keep ///
        anio ///
        factor_var_original ingreso_var_original sector_var_original rama4d_var_original ///
        ocupado_var_original pension_var_original tamano_var_original ///
        horas_var_original sexo_var_original salud_var_original educ_var_original ///
        oficio_var_original ///
        factor_expansion_original factor_expansion_anual ///
        ocupado_cod ///
        ingresos_laborales ingreso_valido ///
        ingreso_laboral_anual ///
        horas_semana horas_validas ///
        ingreso_laboral_hora ingreso_hora_valido ///
        sector_cod rama4d_cod ciiu_revision ciiu_revision_rama2d ciiu_revision_rama4d ///
        cotiza_pension_cod ///
        tamano_empresa_cod ///
        sexo_cod ///
        cotiza_salud_cod ///
        educacion_cod ///
		edad ///
		depto_cod ///
		area_cod ///
		posicion_ocupacional_cod ///
		oficio_cod

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