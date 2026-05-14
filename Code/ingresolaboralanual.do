****************************************************
* CONSOLIDADO POR SECTOR / SECCIÓN CIIU
* GEIH 2008
* Sin discriminación por tamaño de empresa
* Usando RAMA2D - CIIU Rev. 3
****************************************************

clear all
set more off

cd "C:/Users/jorge/Documents/Databases/GEIH"

capture mkdir "Outputs"
capture mkdir "Outputs/tables"

local archivo "Outputs/tables/consolidadosector_2008.xlsx"
capture erase "`archivo'"

use "GEIH_2008_TOTAL.dta", clear


*===================================================
* 0. IDENTIFICAR FACTOR DE EXPANSIÓN
*===================================================
* En algunos años puede llamarse FEX_C18 y en otros FEX_C.
* Este bloque usa el que exista.

capture confirm variable fex_c_2011
if !_rc {
    local fexvar "fex_c_2011"
}
else {
    capture confirm variable fex_c_2011
    if !_rc {
        local fexvar "fex_c_2011"
    }
    else {
        di as error "No existe FEX_C18 ni FEX_C en la base."
        exit 111
    }
}

di as text "Factor de expansión usado: `fexvar'"


*===================================================
* 1. VERIFICAR VARIABLES NECESARIAS
*===================================================

foreach v in OCI `fexvar' INGLABO RAMA2D P6800 {
    capture confirm variable `v'
    if _rc {
        di as error "No existe la variable `v' en la base."
        exit 111
    }
}


*===================================================
* 2. ASEGURAR VARIABLES NUMÉRICAS
*===================================================

foreach v in OCI `fexvar' INGLABO RAMA2D P6800 {
    capture confirm numeric variable `v'
    if _rc {
        destring `v', replace force
    }
}


*===================================================
* 3. SOLO OCUPADOS
*===================================================

keep if OCI == 1


*===================================================
* 4. DEFINIR FACTOR DE EXPANSIÓN
*===================================================
* Como se trabaja con base anual construida desde meses,
* se divide el factor entre 12 para obtener trabajadores
* promedio del año.

gen FEX = `fexvar'/12
local fac "FEX"


*===================================================
* 5. FILTROS BASE
*===================================================

keep if !missing(INGLABO)
keep if INGLABO > 0

keep if !missing(`fac')
keep if `fac' > 0

keep if !missing(RAMA2D)


*===================================================
* 6. CREAR SECCIÓN CIIU REV. 3
*===================================================

gen str1 seccion = ""
gen str120 desc_seccion = ""

replace seccion = "A" if inlist(RAMA2D, 1, 2)
replace desc_seccion = "Agricultura, ganadería, caza y silvicultura" if seccion == "A"

replace seccion = "B" if RAMA2D == 5
replace desc_seccion = "Pesca" if seccion == "B"

replace seccion = "C" if inrange(RAMA2D, 10, 14)
replace desc_seccion = "Explotación de minas y canteras" if seccion == "C"

replace seccion = "D" if inrange(RAMA2D, 15, 37)
replace desc_seccion = "Industrias manufactureras" if seccion == "D"

replace seccion = "E" if inrange(RAMA2D, 40, 41)
replace desc_seccion = "Suministro de electricidad, gas y agua" if seccion == "E"

replace seccion = "F" if RAMA2D == 45
replace desc_seccion = "Construcción" if seccion == "F"

replace seccion = "G" if inrange(RAMA2D, 50, 52)
replace desc_seccion = "Comercio; reparación de vehículos y efectos personales" if seccion == "G"

replace seccion = "H" if RAMA2D == 55
replace desc_seccion = "Hoteles y restaurantes" if seccion == "H"

replace seccion = "I" if inrange(RAMA2D, 60, 64)
replace desc_seccion = "Transporte, almacenamiento y comunicaciones" if seccion == "I"

replace seccion = "J" if inrange(RAMA2D, 65, 67)
replace desc_seccion = "Intermediación financiera" if seccion == "J"

replace seccion = "K" if inrange(RAMA2D, 70, 74)
replace desc_seccion = "Actividades inmobiliarias, empresariales y de alquiler" if seccion == "K"

replace seccion = "L" if RAMA2D == 75
replace desc_seccion = "Administración pública y defensa; seguridad social obligatoria" if seccion == "L"

replace seccion = "M" if RAMA2D == 80
replace desc_seccion = "Educación" if seccion == "M"

replace seccion = "N" if RAMA2D == 85
replace desc_seccion = "Servicios sociales y de salud" if seccion == "N"

replace seccion = "O" if inrange(RAMA2D, 90, 93)
replace desc_seccion = "Otras actividades de servicios comunitarios, sociales y personales" if seccion == "O"

replace seccion = "P" if RAMA2D == 95
replace desc_seccion = "Hogares privados con servicio doméstico" if seccion == "P"

replace seccion = "Q" if RAMA2D == 99
replace desc_seccion = "Organizaciones y órganos extraterritoriales" if seccion == "Q"

drop if seccion == ""


*===================================================
* 7. INGRESO LABORAL ANUAL E INGRESO POR HORA
*===================================================

gen INGLABO_ANUAL = INGLABO * 12

gen horas_semana = P6800
replace horas_semana = . if missing(horas_semana)
replace horas_semana = . if horas_semana <= 0
replace horas_semana = . if horas_semana > 168

gen INGLABO_HORA = INGLABO_ANUAL / (horas_semana * 52) if !missing(horas_semana)


*===================================================
* 8. CALCULAR RESULTADOS POR SECCIÓN
*===================================================

tempfile resultados
tempname memhold

capture postutil clear

postfile `memhold' ///
    str1 seccion ///
    str120 desc_seccion ///
    double n_obs ///
    double trabajadores ///
    double total_inglabo_anual ///
    double mean_inglabo_anual ///
    double mean_inglabo_mensual ///
    double median_inglabo_anual ///
    double median_inglabo_mensual ///
    double mean_inglabo_hora ///
    double median_inglabo_hora ///
    using `resultados', replace


levelsof seccion, local(lista_sec)

foreach s of local lista_sec {

    preserve

        keep if seccion == "`s'"

        count
        local nobs = r(N)

        local desc "`=desc_seccion[1]'"

        * Trabajadores expandidos
        quietly summarize `fac', meanonly
        local trabajadores = r(sum)

        * Total ingreso laboral anual expandido
        gen ingreso_anual_expandido = INGLABO_ANUAL * `fac'
        quietly summarize ingreso_anual_expandido, meanonly
        local total_inglabo_anual = r(sum)

        * Promedio ponderado ingreso anual
        local mean_inglabo_anual = `total_inglabo_anual' / `trabajadores'

        * Promedio mensual equivalente
        local mean_inglabo_mensual = `mean_inglabo_anual' / 12

        * Mediana ponderada ingreso anual
        sort INGLABO_ANUAL
        gen peso = `fac'
        egen peso_total = total(peso)
        gen peso_acum = sum(peso)
        gen frac_acum = peso_acum / peso_total

        quietly summarize INGLABO_ANUAL if frac_acum >= 0.5, meanonly
        local median_inglabo_anual = r(min)

        * Mediana mensual equivalente
        local median_inglabo_mensual = `median_inglabo_anual' / 12

        * Promedio y mediana ingreso por hora
        local mean_inglabo_hora = .
        local median_inglabo_hora = .

        quietly count if !missing(INGLABO_HORA)

        if r(N) > 0 {

            gen ingreso_hora_expandido = INGLABO_HORA * `fac' if !missing(INGLABO_HORA)

            quietly summarize ingreso_hora_expandido, meanonly
            local total_inglabo_hora = r(sum)

            quietly summarize `fac' if !missing(INGLABO_HORA), meanonly
            local trabajadores_hora = r(sum)

            local mean_inglabo_hora = `total_inglabo_hora' / `trabajadores_hora'

            sort INGLABO_HORA
            gen peso_hora = `fac' if !missing(INGLABO_HORA)
            egen peso_total_hora = total(peso_hora)
            gen peso_acum_hora = sum(peso_hora) if !missing(INGLABO_HORA)
            gen frac_acum_hora = peso_acum_hora / peso_total_hora

            quietly summarize INGLABO_HORA if frac_acum_hora >= 0.5 & !missing(INGLABO_HORA), meanonly
            local median_inglabo_hora = r(min)
        }

        post `memhold' ///
            ("`s'") ///
            ("`desc'") ///
            (`nobs') ///
            (`trabajadores') ///
            (`total_inglabo_anual') ///
            (`mean_inglabo_anual') ///
            (`mean_inglabo_mensual') ///
            (`median_inglabo_anual') ///
            (`median_inglabo_mensual') ///
            (`mean_inglabo_hora') ///
            (`median_inglabo_hora')

    restore
}

postclose `memhold'


*===================================================
* 9. ORGANIZAR RESULTADOS
*===================================================

use `resultados', clear

sort seccion

order seccion desc_seccion ///
      trabajadores ///
      mean_inglabo_anual mean_inglabo_mensual ///
      median_inglabo_anual median_inglabo_mensual ///
      mean_inglabo_hora median_inglabo_hora ///
      total_inglabo_anual n_obs

label variable seccion "Sección CIIU Rev. 3"
label variable desc_seccion "Descripción de sección"
label variable trabajadores "Trabajadores expandidos"
label variable mean_inglabo_anual "Ingreso laboral anual promedio"
label variable mean_inglabo_mensual "Ingreso laboral mensual promedio"
label variable median_inglabo_anual "Ingreso laboral anual mediano"
label variable median_inglabo_mensual "Ingreso laboral mensual mediano"
label variable mean_inglabo_hora "Ingreso laboral por hora promedio"
label variable median_inglabo_hora "Ingreso laboral por hora mediano"
label variable total_inglabo_anual "Total ingreso laboral anual expandido"
label variable n_obs "Observaciones no expandidas"

format trabajadores %15.0fc
format total_inglabo_anual %18.0fc
format mean_inglabo_anual %15.2fc
format mean_inglabo_mensual %15.2fc
format median_inglabo_anual %15.2fc
format median_inglabo_mensual %15.2fc
format mean_inglabo_hora %15.2fc
format median_inglabo_hora %15.2fc


*===================================================
* 10. EXPORTAR A EXCEL Y GUARDAR DTA
*===================================================

save "Outputs/tables/consolidadosector_2008.dta", replace

export excel using "`archivo'", ///
    sheet("consolidado_sector_2008") ///
    firstrow(varlabels) ///
    replace

di "===================================================="
di "Archivo exportado correctamente:"
di "`archivo'"
di "Hoja: consolidado_sector_2008"
di "Clasificación: CIIU Rev. 3 usando RAMA2D"
di "===================================================="

****************************************************
* CONSOLIDADO POR SECTOR / SECCIÓN CIIU
* GEIH 2009
* Sin discriminación por tamaño de empresa
* Usando RAMA2D - CIIU Rev. 3
****************************************************

clear all
set more off

cd "C:/Users/jorge/Documents/Databases/GEIH"

capture mkdir "Outputs"
capture mkdir "Outputs/tables"

local archivo "Outputs/tables/consolidadosector_2009.xlsx"
capture erase "`archivo'"

use "GEIH_2009_TOTAL.dta", clear


*===================================================
* 0. IDENTIFICAR FACTOR DE EXPANSIÓN
*===================================================

capture confirm variable fex_c_2011
if !_rc {
    local fexvar "fex_c_2011"
}
else {
    capture confirm variable fex_c_2011
    if !_rc {
        local fexvar "fex_c_2011"
    }
    else {
        di as error "No existe FEX_C18 ni FEX_C en la base."
        exit 111
    }
}

di as text "Factor de expansión usado: `fexvar'"


*===================================================
* 1. VERIFICAR VARIABLES NECESARIAS
*===================================================

foreach v in OCI `fexvar' INGLABO RAMA2D P6800 {
    capture confirm variable `v'
    if _rc {
        di as error "No existe la variable `v' en la base."
        exit 111
    }
}


*===================================================
* 2. ASEGURAR VARIABLES NUMÉRICAS
*===================================================

foreach v in OCI `fexvar' INGLABO RAMA2D P6800 {
    capture confirm numeric variable `v'
    if _rc {
        destring `v', replace force
    }
}


*===================================================
* 3. SOLO OCUPADOS
*===================================================

keep if OCI == 1


*===================================================
* 4. DEFINIR FACTOR DE EXPANSIÓN
*===================================================

gen FEX = `fexvar'/12
local fac "FEX"


*===================================================
* 5. FILTROS BASE
*===================================================

keep if !missing(INGLABO)
keep if INGLABO > 0

keep if !missing(`fac')
keep if `fac' > 0

keep if !missing(RAMA2D)


*===================================================
* 6. CREAR SECCIÓN CIIU REV. 3
*===================================================

gen str1 seccion = ""
gen str120 desc_seccion = ""

replace seccion = "A" if inlist(RAMA2D, 1, 2)
replace desc_seccion = "Agricultura, ganadería, caza y silvicultura" if seccion == "A"

replace seccion = "B" if RAMA2D == 5
replace desc_seccion = "Pesca" if seccion == "B"

replace seccion = "C" if inrange(RAMA2D, 10, 14)
replace desc_seccion = "Explotación de minas y canteras" if seccion == "C"

replace seccion = "D" if inrange(RAMA2D, 15, 37)
replace desc_seccion = "Industrias manufactureras" if seccion == "D"

replace seccion = "E" if inrange(RAMA2D, 40, 41)
replace desc_seccion = "Suministro de electricidad, gas y agua" if seccion == "E"

replace seccion = "F" if RAMA2D == 45
replace desc_seccion = "Construcción" if seccion == "F"

replace seccion = "G" if inrange(RAMA2D, 50, 52)
replace desc_seccion = "Comercio; reparación de vehículos y efectos personales" if seccion == "G"

replace seccion = "H" if RAMA2D == 55
replace desc_seccion = "Hoteles y restaurantes" if seccion == "H"

replace seccion = "I" if inrange(RAMA2D, 60, 64)
replace desc_seccion = "Transporte, almacenamiento y comunicaciones" if seccion == "I"

replace seccion = "J" if inrange(RAMA2D, 65, 67)
replace desc_seccion = "Intermediación financiera" if seccion == "J"

replace seccion = "K" if inrange(RAMA2D, 70, 74)
replace desc_seccion = "Actividades inmobiliarias, empresariales y de alquiler" if seccion == "K"

replace seccion = "L" if RAMA2D == 75
replace desc_seccion = "Administración pública y defensa; seguridad social obligatoria" if seccion == "L"

replace seccion = "M" if RAMA2D == 80
replace desc_seccion = "Educación" if seccion == "M"

replace seccion = "N" if RAMA2D == 85
replace desc_seccion = "Servicios sociales y de salud" if seccion == "N"

replace seccion = "O" if inrange(RAMA2D, 90, 93)
replace desc_seccion = "Otras actividades de servicios comunitarios, sociales y personales" if seccion == "O"

replace seccion = "P" if RAMA2D == 95
replace desc_seccion = "Hogares privados con servicio doméstico" if seccion == "P"

replace seccion = "Q" if RAMA2D == 99
replace desc_seccion = "Organizaciones y órganos extraterritoriales" if seccion == "Q"

drop if seccion == ""


*===================================================
* 7. INGRESO LABORAL ANUAL E INGRESO POR HORA
*===================================================

gen INGLABO_ANUAL = INGLABO * 12

gen horas_semana = P6800
replace horas_semana = . if missing(horas_semana)
replace horas_semana = . if horas_semana <= 0
replace horas_semana = . if horas_semana > 168

gen INGLABO_HORA = INGLABO_ANUAL / (horas_semana * 52) if !missing(horas_semana)


*===================================================
* 8. CALCULAR RESULTADOS POR SECCIÓN
*===================================================

tempfile resultados
tempname memhold

capture postutil clear

postfile `memhold' ///
    str1 seccion ///
    str120 desc_seccion ///
    double n_obs ///
    double trabajadores ///
    double total_inglabo_anual ///
    double mean_inglabo_anual ///
    double mean_inglabo_mensual ///
    double median_inglabo_anual ///
    double median_inglabo_mensual ///
    double mean_inglabo_hora ///
    double median_inglabo_hora ///
    using `resultados', replace


levelsof seccion, local(lista_sec)

foreach s of local lista_sec {

    preserve

        keep if seccion == "`s'"

        count
        local nobs = r(N)

        local desc "`=desc_seccion[1]'"

        quietly summarize `fac', meanonly
        local trabajadores = r(sum)

        gen ingreso_anual_expandido = INGLABO_ANUAL * `fac'
        quietly summarize ingreso_anual_expandido, meanonly
        local total_inglabo_anual = r(sum)

        local mean_inglabo_anual = `total_inglabo_anual' / `trabajadores'
        local mean_inglabo_mensual = `mean_inglabo_anual' / 12

        sort INGLABO_ANUAL
        gen peso = `fac'
        egen peso_total = total(peso)
        gen peso_acum = sum(peso)
        gen frac_acum = peso_acum / peso_total

        quietly summarize INGLABO_ANUAL if frac_acum >= 0.5, meanonly
        local median_inglabo_anual = r(min)

        local median_inglabo_mensual = `median_inglabo_anual' / 12

        local mean_inglabo_hora = .
        local median_inglabo_hora = .

        quietly count if !missing(INGLABO_HORA)

        if r(N) > 0 {

            gen ingreso_hora_expandido = INGLABO_HORA * `fac' if !missing(INGLABO_HORA)

            quietly summarize ingreso_hora_expandido, meanonly
            local total_inglabo_hora = r(sum)

            quietly summarize `fac' if !missing(INGLABO_HORA), meanonly
            local trabajadores_hora = r(sum)

            local mean_inglabo_hora = `total_inglabo_hora' / `trabajadores_hora'

            sort INGLABO_HORA
            gen peso_hora = `fac' if !missing(INGLABO_HORA)
            egen peso_total_hora = total(peso_hora)
            gen peso_acum_hora = sum(peso_hora) if !missing(INGLABO_HORA)
            gen frac_acum_hora = peso_acum_hora / peso_total_hora

            quietly summarize INGLABO_HORA if frac_acum_hora >= 0.5 & !missing(INGLABO_HORA), meanonly
            local median_inglabo_hora = r(min)
        }

        post `memhold' ///
            ("`s'") ///
            ("`desc'") ///
            (`nobs') ///
            (`trabajadores') ///
            (`total_inglabo_anual') ///
            (`mean_inglabo_anual') ///
            (`mean_inglabo_mensual') ///
            (`median_inglabo_anual') ///
            (`median_inglabo_mensual') ///
            (`mean_inglabo_hora') ///
            (`median_inglabo_hora')

    restore
}

postclose `memhold'


*===================================================
* 9. ORGANIZAR RESULTADOS
*===================================================

use `resultados', clear

sort seccion

order seccion desc_seccion ///
      trabajadores ///
      mean_inglabo_anual mean_inglabo_mensual ///
      median_inglabo_anual median_inglabo_mensual ///
      mean_inglabo_hora median_inglabo_hora ///
      total_inglabo_anual n_obs

label variable seccion "Sección CIIU Rev. 3"
label variable desc_seccion "Descripción de sección"
label variable trabajadores "Trabajadores expandidos"
label variable mean_inglabo_anual "Ingreso laboral anual promedio"
label variable mean_inglabo_mensual "Ingreso laboral mensual promedio"
label variable median_inglabo_anual "Ingreso laboral anual mediano"
label variable median_inglabo_mensual "Ingreso laboral mensual mediano"
label variable mean_inglabo_hora "Ingreso laboral por hora promedio"
label variable median_inglabo_hora "Ingreso laboral por hora mediano"
label variable total_inglabo_anual "Total ingreso laboral anual expandido"
label variable n_obs "Observaciones no expandidas"

format trabajadores %15.0fc
format total_inglabo_anual %18.0fc
format mean_inglabo_anual %15.2fc
format mean_inglabo_mensual %15.2fc
format median_inglabo_anual %15.2fc
format median_inglabo_mensual %15.2fc
format mean_inglabo_hora %15.2fc
format median_inglabo_hora %15.2fc


*===================================================
* 10. EXPORTAR A EXCEL Y GUARDAR DTA
*===================================================

save "Outputs/tables/consolidadosector_2009.dta", replace

export excel using "`archivo'", ///
    sheet("consolidado_sector_2009") ///
    firstrow(varlabels) ///
    replace

di "===================================================="
di "Archivo exportado correctamente:"
di "`archivo'"
di "Hoja: consolidado_sector_2009"
di "Clasificación: CIIU Rev. 3 usando RAMA2D"
di "===================================================="

****************************************************
* CONSOLIDADO POR SECTOR / SECCIÓN CIIU
* GEIH 2010
* Sin discriminación por tamaño de empresa
* Usando RAMA2D - CIIU Rev. 3
****************************************************

clear all
set more off

cd "C:/Users/jorge/Documents/Databases/GEIH"

local year 2010

capture mkdir "Outputs"
capture mkdir "Outputs/tables"

local archivo "Outputs/tables/consolidadosector_`year'.xlsx"
capture erase "`archivo'"

use "GEIH_`year'_TOTAL.dta", clear


*===================================================
* 0. IDENTIFICAR FACTOR DE EXPANSIÓN
*===================================================

capture confirm variable fex_c_2011
if !_rc {
    local fexvar "fex_c_2011"
}
else {
    capture confirm variable fex_c_2011
    if !_rc {
        local fexvar "fex_c_2011"
    }
    else {
        di as error "No existe FEX_C18 ni FEX_C en la base."
        exit 111
    }
}

di as text "Factor de expansión usado: `fexvar'"


*===================================================
* 1. VERIFICAR VARIABLES NECESARIAS
*===================================================

foreach v in OCI `fexvar' INGLABO RAMA2D P6800 {
    capture confirm variable `v'
    if _rc {
        di as error "No existe la variable `v' en la base."
        exit 111
    }
}


*===================================================
* 2. ASEGURAR VARIABLES NUMÉRICAS
*===================================================

foreach v in OCI `fexvar' INGLABO RAMA2D P6800 {
    capture confirm numeric variable `v'
    if _rc {
        destring `v', replace force
    }
}


*===================================================
* 3. SOLO OCUPADOS
*===================================================

keep if OCI == 1


*===================================================
* 4. DEFINIR FACTOR DE EXPANSIÓN
*===================================================

gen FEX = `fexvar'/12
local fac "FEX"


*===================================================
* 5. FILTROS BASE
*===================================================

keep if !missing(INGLABO)
keep if INGLABO > 0

keep if !missing(`fac')
keep if `fac' > 0

keep if !missing(RAMA2D)


*===================================================
* 6. CREAR SECCIÓN CIIU REV. 3
*===================================================

gen str1 seccion = ""
gen str120 desc_seccion = ""

replace seccion = "A" if inlist(RAMA2D, 1, 2)
replace desc_seccion = "Agricultura, ganadería, caza y silvicultura" if seccion == "A"

replace seccion = "B" if RAMA2D == 5
replace desc_seccion = "Pesca" if seccion == "B"

replace seccion = "C" if inrange(RAMA2D, 10, 14)
replace desc_seccion = "Explotación de minas y canteras" if seccion == "C"

replace seccion = "D" if inrange(RAMA2D, 15, 37)
replace desc_seccion = "Industrias manufactureras" if seccion == "D"

replace seccion = "E" if inrange(RAMA2D, 40, 41)
replace desc_seccion = "Suministro de electricidad, gas y agua" if seccion == "E"

replace seccion = "F" if RAMA2D == 45
replace desc_seccion = "Construcción" if seccion == "F"

replace seccion = "G" if inrange(RAMA2D, 50, 52)
replace desc_seccion = "Comercio; reparación de vehículos y efectos personales" if seccion == "G"

replace seccion = "H" if RAMA2D == 55
replace desc_seccion = "Hoteles y restaurantes" if seccion == "H"

replace seccion = "I" if inrange(RAMA2D, 60, 64)
replace desc_seccion = "Transporte, almacenamiento y comunicaciones" if seccion == "I"

replace seccion = "J" if inrange(RAMA2D, 65, 67)
replace desc_seccion = "Intermediación financiera" if seccion == "J"

replace seccion = "K" if inrange(RAMA2D, 70, 74)
replace desc_seccion = "Actividades inmobiliarias, empresariales y de alquiler" if seccion == "K"

replace seccion = "L" if RAMA2D == 75
replace desc_seccion = "Administración pública y defensa; seguridad social obligatoria" if seccion == "L"

replace seccion = "M" if RAMA2D == 80
replace desc_seccion = "Educación" if seccion == "M"

replace seccion = "N" if RAMA2D == 85
replace desc_seccion = "Servicios sociales y de salud" if seccion == "N"

replace seccion = "O" if inrange(RAMA2D, 90, 93)
replace desc_seccion = "Otras actividades de servicios comunitarios, sociales y personales" if seccion == "O"

replace seccion = "P" if RAMA2D == 95
replace desc_seccion = "Hogares privados con servicio doméstico" if seccion == "P"

replace seccion = "Q" if RAMA2D == 99
replace desc_seccion = "Organizaciones y órganos extraterritoriales" if seccion == "Q"

drop if seccion == ""


*===================================================
* 7. INGRESO LABORAL ANUAL E INGRESO POR HORA
*===================================================

gen INGLABO_ANUAL = INGLABO * 12

gen horas_semana = P6800
replace horas_semana = . if missing(horas_semana)
replace horas_semana = . if horas_semana <= 0
replace horas_semana = . if horas_semana > 168

gen INGLABO_HORA = INGLABO_ANUAL / (horas_semana * 52) if !missing(horas_semana)


*===================================================
* 8. CALCULAR RESULTADOS POR SECCIÓN
*===================================================

tempfile resultados
tempname memhold

capture postutil clear

postfile `memhold' ///
    str1 seccion ///
    str120 desc_seccion ///
    double n_obs ///
    double trabajadores ///
    double total_inglabo_anual ///
    double mean_inglabo_anual ///
    double mean_inglabo_mensual ///
    double median_inglabo_anual ///
    double median_inglabo_mensual ///
    double mean_inglabo_hora ///
    double median_inglabo_hora ///
    using `resultados', replace


levelsof seccion, local(lista_sec)

foreach s of local lista_sec {

    preserve

        keep if seccion == "`s'"

        count
        local nobs = r(N)

        local desc "`=desc_seccion[1]'"

        quietly summarize `fac', meanonly
        local trabajadores = r(sum)

        gen ingreso_anual_expandido = INGLABO_ANUAL * `fac'
        quietly summarize ingreso_anual_expandido, meanonly
        local total_inglabo_anual = r(sum)

        local mean_inglabo_anual = `total_inglabo_anual' / `trabajadores'
        local mean_inglabo_mensual = `mean_inglabo_anual' / 12

        sort INGLABO_ANUAL
        gen peso = `fac'
        egen peso_total = total(peso)
        gen peso_acum = sum(peso)
        gen frac_acum = peso_acum / peso_total

        quietly summarize INGLABO_ANUAL if frac_acum >= 0.5, meanonly
        local median_inglabo_anual = r(min)

        local median_inglabo_mensual = `median_inglabo_anual' / 12

        local mean_inglabo_hora = .
        local median_inglabo_hora = .

        quietly count if !missing(INGLABO_HORA)

        if r(N) > 0 {

            gen ingreso_hora_expandido = INGLABO_HORA * `fac' if !missing(INGLABO_HORA)

            quietly summarize ingreso_hora_expandido, meanonly
            local total_inglabo_hora = r(sum)

            quietly summarize `fac' if !missing(INGLABO_HORA), meanonly
            local trabajadores_hora = r(sum)

            local mean_inglabo_hora = `total_inglabo_hora' / `trabajadores_hora'

            sort INGLABO_HORA
            gen peso_hora = `fac' if !missing(INGLABO_HORA)
            egen peso_total_hora = total(peso_hora)
            gen peso_acum_hora = sum(peso_hora) if !missing(INGLABO_HORA)
            gen frac_acum_hora = peso_acum_hora / peso_total_hora

            quietly summarize INGLABO_HORA if frac_acum_hora >= 0.5 & !missing(INGLABO_HORA), meanonly
            local median_inglabo_hora = r(min)
        }

        post `memhold' ///
            ("`s'") ///
            ("`desc'") ///
            (`nobs') ///
            (`trabajadores') ///
            (`total_inglabo_anual') ///
            (`mean_inglabo_anual') ///
            (`mean_inglabo_mensual') ///
            (`median_inglabo_anual') ///
            (`median_inglabo_mensual') ///
            (`mean_inglabo_hora') ///
            (`median_inglabo_hora')

    restore
}

postclose `memhold'


*===================================================
* 9. ORGANIZAR RESULTADOS
*===================================================

use `resultados', clear

sort seccion

order seccion desc_seccion ///
      trabajadores ///
      mean_inglabo_anual mean_inglabo_mensual ///
      median_inglabo_anual median_inglabo_mensual ///
      mean_inglabo_hora median_inglabo_hora ///
      total_inglabo_anual n_obs

label variable seccion "Sección CIIU Rev. 3"
label variable desc_seccion "Descripción de sección"
label variable trabajadores "Trabajadores expandidos"
label variable mean_inglabo_anual "Ingreso laboral anual promedio"
label variable mean_inglabo_mensual "Ingreso laboral mensual promedio"
label variable median_inglabo_anual "Ingreso laboral anual mediano"
label variable median_inglabo_mensual "Ingreso laboral mensual mediano"
label variable mean_inglabo_hora "Ingreso laboral por hora promedio"
label variable median_inglabo_hora "Ingreso laboral por hora mediano"
label variable total_inglabo_anual "Total ingreso laboral anual expandido"
label variable n_obs "Observaciones no expandidas"

format trabajadores %15.0fc
format total_inglabo_anual %18.0fc
format mean_inglabo_anual %15.2fc
format mean_inglabo_mensual %15.2fc
format median_inglabo_anual %15.2fc
format median_inglabo_mensual %15.2fc
format mean_inglabo_hora %15.2fc
format median_inglabo_hora %15.2fc


*===================================================
* 10. EXPORTAR A EXCEL Y GUARDAR DTA
*===================================================

save "Outputs/tables/consolidadosector_`year'.dta", replace

export excel using "`archivo'", ///
    sheet("consolidado_sector_`year'") ///
    firstrow(varlabels) ///
    replace

di "===================================================="
di "Archivo exportado correctamente:"
di "`archivo'"
di "Hoja: consolidado_sector_`year'"
di "Clasificación: CIIU Rev. 3 usando RAMA2D"
di "===================================================="

****************************************************
* CONSOLIDADO POR SECTOR / SECCIÓN CIIU
* GEIH 2011
* Sin discriminación por tamaño de empresa
* Usando RAMA2D - CIIU Rev. 3
****************************************************

clear all
set more off

cd "C:/Users/jorge/Documents/Databases/GEIH"

local year 2011

capture mkdir "Outputs"
capture mkdir "Outputs/tables"

local archivo "Outputs/tables/consolidadosector_`year'.xlsx"
capture erase "`archivo'"

use "GEIH_`year'_TOTAL.dta", clear


*===================================================
* 0. IDENTIFICAR FACTOR DE EXPANSIÓN
*===================================================

capture confirm variable FEX_C_2011
if !_rc {
    local fexvar "FEX_C_2011"
}
else {
    capture confirm variable FEX_C_2011
    if !_rc {
        local fexvar "FEX_C_2011"
    }
    else {
        di as error "No existe FEX_C18 ni FEX_C en la base."
        exit 111
    }
}

di as text "Factor de expansión usado: `fexvar'"


*===================================================
* 1. VERIFICAR VARIABLES NECESARIAS
*===================================================

foreach v in OCI `fexvar' INGLABO RAMA2D P6800 {
    capture confirm variable `v'
    if _rc {
        di as error "No existe la variable `v' en la base."
        exit 111
    }
}


*===================================================
* 2. ASEGURAR VARIABLES NUMÉRICAS
*===================================================

foreach v in OCI `fexvar' INGLABO RAMA2D P6800 {
    capture confirm numeric variable `v'
    if _rc {
        destring `v', replace force
    }
}


*===================================================
* 3. SOLO OCUPADOS
*===================================================

keep if OCI == 1


*===================================================
* 4. DEFINIR FACTOR DE EXPANSIÓN
*===================================================
* Como se trabaja con base anual construida desde meses,
* se divide el factor entre 12 para obtener trabajadores
* promedio del año.

gen FEX = `fexvar'/12
local fac "FEX"


*===================================================
* 5. FILTROS BASE
*===================================================

keep if !missing(INGLABO)
keep if INGLABO > 0

keep if !missing(`fac')
keep if `fac' > 0

keep if !missing(RAMA2D)


*===================================================
* 6. CREAR SECCIÓN CIIU REV. 3
*===================================================

gen str1 seccion = ""
gen str120 desc_seccion = ""

replace seccion = "A" if inlist(RAMA2D, 1, 2)
replace desc_seccion = "Agricultura, ganadería, caza y silvicultura" if seccion == "A"

replace seccion = "B" if RAMA2D == 5
replace desc_seccion = "Pesca" if seccion == "B"

replace seccion = "C" if inrange(RAMA2D, 10, 14)
replace desc_seccion = "Explotación de minas y canteras" if seccion == "C"

replace seccion = "D" if inrange(RAMA2D, 15, 37)
replace desc_seccion = "Industrias manufactureras" if seccion == "D"

replace seccion = "E" if inrange(RAMA2D, 40, 41)
replace desc_seccion = "Suministro de electricidad, gas y agua" if seccion == "E"

replace seccion = "F" if RAMA2D == 45
replace desc_seccion = "Construcción" if seccion == "F"

replace seccion = "G" if inrange(RAMA2D, 50, 52)
replace desc_seccion = "Comercio; reparación de vehículos y efectos personales" if seccion == "G"

replace seccion = "H" if RAMA2D == 55
replace desc_seccion = "Hoteles y restaurantes" if seccion == "H"

replace seccion = "I" if inrange(RAMA2D, 60, 64)
replace desc_seccion = "Transporte, almacenamiento y comunicaciones" if seccion == "I"

replace seccion = "J" if inrange(RAMA2D, 65, 67)
replace desc_seccion = "Intermediación financiera" if seccion == "J"

replace seccion = "K" if inrange(RAMA2D, 70, 74)
replace desc_seccion = "Actividades inmobiliarias, empresariales y de alquiler" if seccion == "K"

replace seccion = "L" if RAMA2D == 75
replace desc_seccion = "Administración pública y defensa; seguridad social obligatoria" if seccion == "L"

replace seccion = "M" if RAMA2D == 80
replace desc_seccion = "Educación" if seccion == "M"

replace seccion = "N" if RAMA2D == 85
replace desc_seccion = "Servicios sociales y de salud" if seccion == "N"

replace seccion = "O" if inrange(RAMA2D, 90, 93)
replace desc_seccion = "Otras actividades de servicios comunitarios, sociales y personales" if seccion == "O"

replace seccion = "P" if RAMA2D == 95
replace desc_seccion = "Hogares privados con servicio doméstico" if seccion == "P"

replace seccion = "Q" if RAMA2D == 99
replace desc_seccion = "Organizaciones y órganos extraterritoriales" if seccion == "Q"

drop if seccion == ""


*===================================================
* 7. INGRESO LABORAL ANUAL E INGRESO POR HORA
*===================================================

gen INGLABO_ANUAL = INGLABO * 12

gen horas_semana = P6800
replace horas_semana = . if missing(horas_semana)
replace horas_semana = . if horas_semana <= 0
replace horas_semana = . if horas_semana > 168

gen INGLABO_HORA = INGLABO_ANUAL / (horas_semana * 52) if !missing(horas_semana)


*===================================================
* 8. CALCULAR RESULTADOS POR SECCIÓN
*===================================================

tempfile resultados
tempname memhold

capture postutil clear

postfile `memhold' ///
    str1 seccion ///
    str120 desc_seccion ///
    double n_obs ///
    double trabajadores ///
    double total_inglabo_anual ///
    double mean_inglabo_anual ///
    double mean_inglabo_mensual ///
    double median_inglabo_anual ///
    double median_inglabo_mensual ///
    double mean_inglabo_hora ///
    double median_inglabo_hora ///
    using `resultados', replace


levelsof seccion, local(lista_sec)

foreach s of local lista_sec {

    preserve

        keep if seccion == "`s'"

        count
        local nobs = r(N)

        local desc "`=desc_seccion[1]'"

        * Trabajadores expandidos
        quietly summarize `fac', meanonly
        local trabajadores = r(sum)

        * Total ingreso laboral anual expandido
        gen ingreso_anual_expandido = INGLABO_ANUAL * `fac'
        quietly summarize ingreso_anual_expandido, meanonly
        local total_inglabo_anual = r(sum)

        * Promedio ponderado ingreso anual
        local mean_inglabo_anual = `total_inglabo_anual' / `trabajadores'

        * Promedio mensual equivalente
        local mean_inglabo_mensual = `mean_inglabo_anual' / 12

        * Mediana ponderada ingreso anual
        sort INGLABO_ANUAL
        gen peso = `fac'
        egen peso_total = total(peso)
        gen peso_acum = sum(peso)
        gen frac_acum = peso_acum / peso_total

        quietly summarize INGLABO_ANUAL if frac_acum >= 0.5, meanonly
        local median_inglabo_anual = r(min)

        * Mediana mensual equivalente
        local median_inglabo_mensual = `median_inglabo_anual' / 12

        * Promedio y mediana ingreso por hora
        local mean_inglabo_hora = .
        local median_inglabo_hora = .

        quietly count if !missing(INGLABO_HORA)

        if r(N) > 0 {

            gen ingreso_hora_expandido = INGLABO_HORA * `fac' if !missing(INGLABO_HORA)

            quietly summarize ingreso_hora_expandido, meanonly
            local total_inglabo_hora = r(sum)

            quietly summarize `fac' if !missing(INGLABO_HORA), meanonly
            local trabajadores_hora = r(sum)

            local mean_inglabo_hora = `total_inglabo_hora' / `trabajadores_hora'

            sort INGLABO_HORA
            gen peso_hora = `fac' if !missing(INGLABO_HORA)
            egen peso_total_hora = total(peso_hora)
            gen peso_acum_hora = sum(peso_hora) if !missing(INGLABO_HORA)
            gen frac_acum_hora = peso_acum_hora / peso_total_hora

            quietly summarize INGLABO_HORA if frac_acum_hora >= 0.5 & !missing(INGLABO_HORA), meanonly
            local median_inglabo_hora = r(min)
        }

        post `memhold' ///
            ("`s'") ///
            ("`desc'") ///
            (`nobs') ///
            (`trabajadores') ///
            (`total_inglabo_anual') ///
            (`mean_inglabo_anual') ///
            (`mean_inglabo_mensual') ///
            (`median_inglabo_anual') ///
            (`median_inglabo_mensual') ///
            (`mean_inglabo_hora') ///
            (`median_inglabo_hora')

    restore
}

postclose `memhold'


*===================================================
* 9. ORGANIZAR RESULTADOS
*===================================================

use `resultados', clear

sort seccion

order seccion desc_seccion ///
      trabajadores ///
      mean_inglabo_anual mean_inglabo_mensual ///
      median_inglabo_anual median_inglabo_mensual ///
      mean_inglabo_hora median_inglabo_hora ///
      total_inglabo_anual n_obs

label variable seccion "Sección CIIU Rev. 3"
label variable desc_seccion "Descripción de sección"
label variable trabajadores "Trabajadores expandidos"
label variable mean_inglabo_anual "Ingreso laboral anual promedio"
label variable mean_inglabo_mensual "Ingreso laboral mensual promedio"
label variable median_inglabo_anual "Ingreso laboral anual mediano"
label variable median_inglabo_mensual "Ingreso laboral mensual mediano"
label variable mean_inglabo_hora "Ingreso laboral por hora promedio"
label variable median_inglabo_hora "Ingreso laboral por hora mediano"
label variable total_inglabo_anual "Total ingreso laboral anual expandido"
label variable n_obs "Observaciones no expandidas"

format trabajadores %15.0fc
format total_inglabo_anual %18.0fc
format mean_inglabo_anual %15.2fc
format mean_inglabo_mensual %15.2fc
format median_inglabo_anual %15.2fc
format median_inglabo_mensual %15.2fc
format mean_inglabo_hora %15.2fc
format median_inglabo_hora %15.2fc


*===================================================
* 10. EXPORTAR A EXCEL Y GUARDAR DTA
*===================================================

save "Outputs/tables/consolidadosector_`year'.dta", replace

export excel using "`archivo'", ///
    sheet("consolidado_sector_`year'") ///
    firstrow(varlabels) ///
    replace

di "===================================================="
di "Archivo exportado correctamente:"
di "`archivo'"
di "Hoja: consolidado_sector_`year'"
di "Clasificación: CIIU Rev. 3 usando RAMA2D"
di "===================================================="

****************************************************
* CONSOLIDADO POR SECTOR / SECCIÓN CIIU
* GEIH 2012
* Sin discriminación por tamaño de empresa
* Usando RAMA2D - CIIU Rev. 3
****************************************************

clear all
set more off

cd "C:/Users/jorge/Documents/Databases/GEIH"

local year 2012

capture mkdir "Outputs"
capture mkdir "Outputs/tables"

local archivo "Outputs/tables/consolidadosector_`year'.xlsx"
capture erase "`archivo'"

use "GEIH_`year'_TOTAL.dta", clear


*===================================================
* 0. IDENTIFICAR FACTOR DE EXPANSIÓN
*===================================================

capture confirm variable FEX_C_2011
if !_rc {
    local fexvar "FEX_C_2011"
}
else {
    capture confirm variable FEX_C_2011
    if !_rc {
        local fexvar "FEX_C_2011"
    }
    else {
        di as error "No existe FEX_C18 ni FEX_C en la base."
        exit 111
    }
}

di as text "Factor de expansión usado: `fexvar'"


*===================================================
* 1. VERIFICAR VARIABLES NECESARIAS
*===================================================

foreach v in OCI `fexvar' INGLABO RAMA2D P6800 {
    capture confirm variable `v'
    if _rc {
        di as error "No existe la variable `v' en la base."
        exit 111
    }
}


*===================================================
* 2. ASEGURAR VARIABLES NUMÉRICAS
*===================================================

foreach v in OCI `fexvar' INGLABO RAMA2D P6800 {
    capture confirm numeric variable `v'
    if _rc {
        destring `v', replace force
    }
}


*===================================================
* 3. SOLO OCUPADOS
*===================================================

keep if OCI == 1


*===================================================
* 4. DEFINIR FACTOR DE EXPANSIÓN
*===================================================
* Como se trabaja con base anual construida desde meses,
* se divide el factor entre 12 para obtener trabajadores
* promedio del año.

gen FEX = `fexvar'/12
local fac "FEX"


*===================================================
* 5. FILTROS BASE
*===================================================

keep if !missing(INGLABO)
keep if INGLABO > 0

keep if !missing(`fac')
keep if `fac' > 0

keep if !missing(RAMA2D)


*===================================================
* 6. CREAR SECCIÓN CIIU REV. 3
*===================================================

gen str1 seccion = ""
gen str120 desc_seccion = ""

replace seccion = "A" if inlist(RAMA2D, 1, 2)
replace desc_seccion = "Agricultura, ganadería, caza y silvicultura" if seccion == "A"

replace seccion = "B" if RAMA2D == 5
replace desc_seccion = "Pesca" if seccion == "B"

replace seccion = "C" if inrange(RAMA2D, 10, 14)
replace desc_seccion = "Explotación de minas y canteras" if seccion == "C"

replace seccion = "D" if inrange(RAMA2D, 15, 37)
replace desc_seccion = "Industrias manufactureras" if seccion == "D"

replace seccion = "E" if inrange(RAMA2D, 40, 41)
replace desc_seccion = "Suministro de electricidad, gas y agua" if seccion == "E"

replace seccion = "F" if RAMA2D == 45
replace desc_seccion = "Construcción" if seccion == "F"

replace seccion = "G" if inrange(RAMA2D, 50, 52)
replace desc_seccion = "Comercio; reparación de vehículos y efectos personales" if seccion == "G"

replace seccion = "H" if RAMA2D == 55
replace desc_seccion = "Hoteles y restaurantes" if seccion == "H"

replace seccion = "I" if inrange(RAMA2D, 60, 64)
replace desc_seccion = "Transporte, almacenamiento y comunicaciones" if seccion == "I"

replace seccion = "J" if inrange(RAMA2D, 65, 67)
replace desc_seccion = "Intermediación financiera" if seccion == "J"

replace seccion = "K" if inrange(RAMA2D, 70, 74)
replace desc_seccion = "Actividades inmobiliarias, empresariales y de alquiler" if seccion == "K"

replace seccion = "L" if RAMA2D == 75
replace desc_seccion = "Administración pública y defensa; seguridad social obligatoria" if seccion == "L"

replace seccion = "M" if RAMA2D == 80
replace desc_seccion = "Educación" if seccion == "M"

replace seccion = "N" if RAMA2D == 85
replace desc_seccion = "Servicios sociales y de salud" if seccion == "N"

replace seccion = "O" if inrange(RAMA2D, 90, 93)
replace desc_seccion = "Otras actividades de servicios comunitarios, sociales y personales" if seccion == "O"

replace seccion = "P" if RAMA2D == 95
replace desc_seccion = "Hogares privados con servicio doméstico" if seccion == "P"

replace seccion = "Q" if RAMA2D == 99
replace desc_seccion = "Organizaciones y órganos extraterritoriales" if seccion == "Q"

drop if seccion == ""


*===================================================
* 7. INGRESO LABORAL ANUAL E INGRESO POR HORA
*===================================================

gen INGLABO_ANUAL = INGLABO * 12

gen horas_semana = P6800
replace horas_semana = . if missing(horas_semana)
replace horas_semana = . if horas_semana <= 0
replace horas_semana = . if horas_semana > 168

gen INGLABO_HORA = INGLABO_ANUAL / (horas_semana * 52) if !missing(horas_semana)


*===================================================
* 8. CALCULAR RESULTADOS POR SECCIÓN
*===================================================

tempfile resultados
tempname memhold

capture postutil clear

postfile `memhold' ///
    str1 seccion ///
    str120 desc_seccion ///
    double n_obs ///
    double trabajadores ///
    double total_inglabo_anual ///
    double mean_inglabo_anual ///
    double mean_inglabo_mensual ///
    double median_inglabo_anual ///
    double median_inglabo_mensual ///
    double mean_inglabo_hora ///
    double median_inglabo_hora ///
    using `resultados', replace


levelsof seccion, local(lista_sec)

foreach s of local lista_sec {

    preserve

        keep if seccion == "`s'"

        count
        local nobs = r(N)

        local desc "`=desc_seccion[1]'"

        * Trabajadores expandidos
        quietly summarize `fac', meanonly
        local trabajadores = r(sum)

        * Total ingreso laboral anual expandido
        gen ingreso_anual_expandido = INGLABO_ANUAL * `fac'
        quietly summarize ingreso_anual_expandido, meanonly
        local total_inglabo_anual = r(sum)

        * Promedio ponderado ingreso anual
        local mean_inglabo_anual = `total_inglabo_anual' / `trabajadores'

        * Promedio mensual equivalente
        local mean_inglabo_mensual = `mean_inglabo_anual' / 12

        * Mediana ponderada ingreso anual
        sort INGLABO_ANUAL
        gen peso = `fac'
        egen peso_total = total(peso)
        gen peso_acum = sum(peso)
        gen frac_acum = peso_acum / peso_total

        quietly summarize INGLABO_ANUAL if frac_acum >= 0.5, meanonly
        local median_inglabo_anual = r(min)

        * Mediana mensual equivalente
        local median_inglabo_mensual = `median_inglabo_anual' / 12

        * Promedio y mediana ingreso por hora
        local mean_inglabo_hora = .
        local median_inglabo_hora = .

        quietly count if !missing(INGLABO_HORA)

        if r(N) > 0 {

            gen ingreso_hora_expandido = INGLABO_HORA * `fac' if !missing(INGLABO_HORA)

            quietly summarize ingreso_hora_expandido, meanonly
            local total_inglabo_hora = r(sum)

            quietly summarize `fac' if !missing(INGLABO_HORA), meanonly
            local trabajadores_hora = r(sum)

            local mean_inglabo_hora = `total_inglabo_hora' / `trabajadores_hora'

            sort INGLABO_HORA
            gen peso_hora = `fac' if !missing(INGLABO_HORA)
            egen peso_total_hora = total(peso_hora)
            gen peso_acum_hora = sum(peso_hora) if !missing(INGLABO_HORA)
            gen frac_acum_hora = peso_acum_hora / peso_total_hora

            quietly summarize INGLABO_HORA if frac_acum_hora >= 0.5 & !missing(INGLABO_HORA), meanonly
            local median_inglabo_hora = r(min)
        }

        post `memhold' ///
            ("`s'") ///
            ("`desc'") ///
            (`nobs') ///
            (`trabajadores') ///
            (`total_inglabo_anual') ///
            (`mean_inglabo_anual') ///
            (`mean_inglabo_mensual') ///
            (`median_inglabo_anual') ///
            (`median_inglabo_mensual') ///
            (`mean_inglabo_hora') ///
            (`median_inglabo_hora')

    restore
}

postclose `memhold'


*===================================================
* 9. ORGANIZAR RESULTADOS
*===================================================

use `resultados', clear

sort seccion

order seccion desc_seccion ///
      trabajadores ///
      mean_inglabo_anual mean_inglabo_mensual ///
      median_inglabo_anual median_inglabo_mensual ///
      mean_inglabo_hora median_inglabo_hora ///
      total_inglabo_anual n_obs

label variable seccion "Sección CIIU Rev. 3"
label variable desc_seccion "Descripción de sección"
label variable trabajadores "Trabajadores expandidos"
label variable mean_inglabo_anual "Ingreso laboral anual promedio"
label variable mean_inglabo_mensual "Ingreso laboral mensual promedio"
label variable median_inglabo_anual "Ingreso laboral anual mediano"
label variable median_inglabo_mensual "Ingreso laboral mensual mediano"
label variable mean_inglabo_hora "Ingreso laboral por hora promedio"
label variable median_inglabo_hora "Ingreso laboral por hora mediano"
label variable total_inglabo_anual "Total ingreso laboral anual expandido"
label variable n_obs "Observaciones no expandidas"

format trabajadores %15.0fc
format total_inglabo_anual %18.0fc
format mean_inglabo_anual %15.2fc
format mean_inglabo_mensual %15.2fc
format median_inglabo_anual %15.2fc
format median_inglabo_mensual %15.2fc
format mean_inglabo_hora %15.2fc
format median_inglabo_hora %15.2fc


*===================================================
* 10. EXPORTAR A EXCEL Y GUARDAR DTA
*===================================================

save "Outputs/tables/consolidadosector_`year'.dta", replace

export excel using "`archivo'", ///
    sheet("consolidado_sector_`year'") ///
    firstrow(varlabels) ///
    replace

di "===================================================="
di "Archivo exportado correctamente:"
di "`archivo'"
di "Hoja: consolidado_sector_`year'"
di "Clasificación: CIIU Rev. 3 usando RAMA2D"
di "===================================================="

****************************************************
* CONSOLIDADO POR SECTOR / SECCIÓN CIIU
* GEIH 2013
* Sin discriminación por tamaño de empresa
* Usando RAMA2D - CIIU Rev. 3
****************************************************

clear all
set more off

cd "C:/Users/jorge/Documents/Databases/GEIH"

local year 2013

capture mkdir "Outputs"
capture mkdir "Outputs/tables"

local archivo "Outputs/tables/consolidadosector_`year'.xlsx"
capture erase "`archivo'"

use "GEIH_`year'_TOTAL.dta", clear


*===================================================
* 0. IDENTIFICAR FACTOR DE EXPANSIÓN
*===================================================

capture confirm variable FEX_C_2011
if !_rc {
    local fexvar "FEX_C_2011"
}
else {
    capture confirm variable FEX_C_2011
    if !_rc {
        local fexvar "FEX_C_2011"
    }
    else {
        di as error "No existe FEX_C18 ni FEX_C en la base."
        exit 111
    }
}

di as text "Factor de expansión usado: `fexvar'"


*===================================================
* 1. VERIFICAR VARIABLES NECESARIAS
*===================================================

foreach v in OCI `fexvar' INGLABO RAMA2D P6800 {
    capture confirm variable `v'
    if _rc {
        di as error "No existe la variable `v' en la base."
        exit 111
    }
}


*===================================================
* 2. ASEGURAR VARIABLES NUMÉRICAS
*===================================================

foreach v in OCI `fexvar' INGLABO RAMA2D P6800 {
    capture confirm numeric variable `v'
    if _rc {
        destring `v', replace force
    }
}


*===================================================
* 3. SOLO OCUPADOS
*===================================================

keep if OCI == 1


*===================================================
* 4. DEFINIR FACTOR DE EXPANSIÓN
*===================================================
* Como se trabaja con base anual construida desde meses,
* se divide el factor entre 12 para obtener trabajadores
* promedio del año.

gen FEX = `fexvar'/12
local fac "FEX"


*===================================================
* 5. FILTROS BASE
*===================================================

keep if !missing(INGLABO)
keep if INGLABO > 0

keep if !missing(`fac')
keep if `fac' > 0

keep if !missing(RAMA2D)


*===================================================
* 6. CREAR SECCIÓN CIIU REV. 3
*===================================================

gen str1 seccion = ""
gen str120 desc_seccion = ""

replace seccion = "A" if inlist(RAMA2D, 1, 2)
replace desc_seccion = "Agricultura, ganadería, caza y silvicultura" if seccion == "A"

replace seccion = "B" if RAMA2D == 5
replace desc_seccion = "Pesca" if seccion == "B"

replace seccion = "C" if inrange(RAMA2D, 10, 14)
replace desc_seccion = "Explotación de minas y canteras" if seccion == "C"

replace seccion = "D" if inrange(RAMA2D, 15, 37)
replace desc_seccion = "Industrias manufactureras" if seccion == "D"

replace seccion = "E" if inrange(RAMA2D, 40, 41)
replace desc_seccion = "Suministro de electricidad, gas y agua" if seccion == "E"

replace seccion = "F" if RAMA2D == 45
replace desc_seccion = "Construcción" if seccion == "F"

replace seccion = "G" if inrange(RAMA2D, 50, 52)
replace desc_seccion = "Comercio; reparación de vehículos y efectos personales" if seccion == "G"

replace seccion = "H" if RAMA2D == 55
replace desc_seccion = "Hoteles y restaurantes" if seccion == "H"

replace seccion = "I" if inrange(RAMA2D, 60, 64)
replace desc_seccion = "Transporte, almacenamiento y comunicaciones" if seccion == "I"

replace seccion = "J" if inrange(RAMA2D, 65, 67)
replace desc_seccion = "Intermediación financiera" if seccion == "J"

replace seccion = "K" if inrange(RAMA2D, 70, 74)
replace desc_seccion = "Actividades inmobiliarias, empresariales y de alquiler" if seccion == "K"

replace seccion = "L" if RAMA2D == 75
replace desc_seccion = "Administración pública y defensa; seguridad social obligatoria" if seccion == "L"

replace seccion = "M" if RAMA2D == 80
replace desc_seccion = "Educación" if seccion == "M"

replace seccion = "N" if RAMA2D == 85
replace desc_seccion = "Servicios sociales y de salud" if seccion == "N"

replace seccion = "O" if inrange(RAMA2D, 90, 93)
replace desc_seccion = "Otras actividades de servicios comunitarios, sociales y personales" if seccion == "O"

replace seccion = "P" if RAMA2D == 95
replace desc_seccion = "Hogares privados con servicio doméstico" if seccion == "P"

replace seccion = "Q" if RAMA2D == 99
replace desc_seccion = "Organizaciones y órganos extraterritoriales" if seccion == "Q"

drop if seccion == ""


*===================================================
* 7. INGRESO LABORAL ANUAL E INGRESO POR HORA
*===================================================

gen INGLABO_ANUAL = INGLABO * 12

gen horas_semana = P6800
replace horas_semana = . if missing(horas_semana)
replace horas_semana = . if horas_semana <= 0
replace horas_semana = . if horas_semana > 168

gen INGLABO_HORA = INGLABO_ANUAL / (horas_semana * 52) if !missing(horas_semana)


*===================================================
* 8. CALCULAR RESULTADOS POR SECCIÓN
*===================================================

tempfile resultados
tempname memhold

capture postutil clear

postfile `memhold' ///
    str1 seccion ///
    str120 desc_seccion ///
    double n_obs ///
    double trabajadores ///
    double total_inglabo_anual ///
    double mean_inglabo_anual ///
    double mean_inglabo_mensual ///
    double median_inglabo_anual ///
    double median_inglabo_mensual ///
    double mean_inglabo_hora ///
    double median_inglabo_hora ///
    using `resultados', replace


levelsof seccion, local(lista_sec)

foreach s of local lista_sec {

    preserve

        keep if seccion == "`s'"

        count
        local nobs = r(N)

        local desc "`=desc_seccion[1]'"

        * Trabajadores expandidos
        quietly summarize `fac', meanonly
        local trabajadores = r(sum)

        * Total ingreso laboral anual expandido
        gen ingreso_anual_expandido = INGLABO_ANUAL * `fac'
        quietly summarize ingreso_anual_expandido, meanonly
        local total_inglabo_anual = r(sum)

        * Promedio ponderado ingreso anual
        local mean_inglabo_anual = `total_inglabo_anual' / `trabajadores'

        * Promedio mensual equivalente
        local mean_inglabo_mensual = `mean_inglabo_anual' / 12

        * Mediana ponderada ingreso anual
        sort INGLABO_ANUAL
        gen peso = `fac'
        egen peso_total = total(peso)
        gen peso_acum = sum(peso)
        gen frac_acum = peso_acum / peso_total

        quietly summarize INGLABO_ANUAL if frac_acum >= 0.5, meanonly
        local median_inglabo_anual = r(min)

        * Mediana mensual equivalente
        local median_inglabo_mensual = `median_inglabo_anual' / 12

        * Promedio y mediana ingreso por hora
        local mean_inglabo_hora = .
        local median_inglabo_hora = .

        quietly count if !missing(INGLABO_HORA)

        if r(N) > 0 {

            gen ingreso_hora_expandido = INGLABO_HORA * `fac' if !missing(INGLABO_HORA)

            quietly summarize ingreso_hora_expandido, meanonly
            local total_inglabo_hora = r(sum)

            quietly summarize `fac' if !missing(INGLABO_HORA), meanonly
            local trabajadores_hora = r(sum)

            local mean_inglabo_hora = `total_inglabo_hora' / `trabajadores_hora'

            sort INGLABO_HORA
            gen peso_hora = `fac' if !missing(INGLABO_HORA)
            egen peso_total_hora = total(peso_hora)
            gen peso_acum_hora = sum(peso_hora) if !missing(INGLABO_HORA)
            gen frac_acum_hora = peso_acum_hora / peso_total_hora

            quietly summarize INGLABO_HORA if frac_acum_hora >= 0.5 & !missing(INGLABO_HORA), meanonly
            local median_inglabo_hora = r(min)
        }

        post `memhold' ///
            ("`s'") ///
            ("`desc'") ///
            (`nobs') ///
            (`trabajadores') ///
            (`total_inglabo_anual') ///
            (`mean_inglabo_anual') ///
            (`mean_inglabo_mensual') ///
            (`median_inglabo_anual') ///
            (`median_inglabo_mensual') ///
            (`mean_inglabo_hora') ///
            (`median_inglabo_hora')

    restore
}

postclose `memhold'


*===================================================
* 9. ORGANIZAR RESULTADOS
*===================================================

use `resultados', clear

sort seccion

order seccion desc_seccion ///
      trabajadores ///
      mean_inglabo_anual mean_inglabo_mensual ///
      median_inglabo_anual median_inglabo_mensual ///
      mean_inglabo_hora median_inglabo_hora ///
      total_inglabo_anual n_obs

label variable seccion "Sección CIIU Rev. 3"
label variable desc_seccion "Descripción de sección"
label variable trabajadores "Trabajadores expandidos"
label variable mean_inglabo_anual "Ingreso laboral anual promedio"
label variable mean_inglabo_mensual "Ingreso laboral mensual promedio"
label variable median_inglabo_anual "Ingreso laboral anual mediano"
label variable median_inglabo_mensual "Ingreso laboral mensual mediano"
label variable mean_inglabo_hora "Ingreso laboral por hora promedio"
label variable median_inglabo_hora "Ingreso laboral por hora mediano"
label variable total_inglabo_anual "Total ingreso laboral anual expandido"
label variable n_obs "Observaciones no expandidas"

format trabajadores %15.0fc
format total_inglabo_anual %18.0fc
format mean_inglabo_anual %15.2fc
format mean_inglabo_mensual %15.2fc
format median_inglabo_anual %15.2fc
format median_inglabo_mensual %15.2fc
format mean_inglabo_hora %15.2fc
format median_inglabo_hora %15.2fc


*===================================================
* 10. EXPORTAR A EXCEL Y GUARDAR DTA
*===================================================

save "Outputs/tables/consolidadosector_`year'.dta", replace

export excel using "`archivo'", ///
    sheet("consolidado_sector_`year'") ///
    firstrow(varlabels) ///
    replace

di "===================================================="
di "Archivo exportado correctamente:"
di "`archivo'"
di "Hoja: consolidado_sector_`year'"
di "Clasificación: CIIU Rev. 3 usando RAMA2D"
di "===================================================="

****************************************************
* CONSOLIDADO POR SECTOR / SECCIÓN CIIU
* GEIH 2014
* Sin discriminación por tamaño de empresa
* Usando RAMA2D - CIIU Rev. 3
****************************************************

clear all
set more off

cd "C:/Users/jorge/Documents/Databases/GEIH"

local year 2014

capture mkdir "Outputs"
capture mkdir "Outputs/tables"

local archivo "Outputs/tables/consolidadosector_`year'.xlsx"
capture erase "`archivo'"

use "GEIH_`year'_TOTAL.dta", clear


*===================================================
* 0. IDENTIFICAR FACTOR DE EXPANSIÓN
*===================================================

capture confirm variable FEX_C_2011
if !_rc {
    local fexvar "FEX_C_2011"
}
else {
    capture confirm variable FEX_C_2011
    if !_rc {
        local fexvar "FEX_C_2011"
    }
    else {
        di as error "No existe FEX_C18 ni FEX_C en la base."
        exit 111
    }
}

di as text "Factor de expansión usado: `fexvar'"


*===================================================
* 1. VERIFICAR VARIABLES NECESARIAS
*===================================================

foreach v in OCI `fexvar' INGLABO RAMA2D P6800 {
    capture confirm variable `v'
    if _rc {
        di as error "No existe la variable `v' en la base."
        exit 111
    }
}


*===================================================
* 2. ASEGURAR VARIABLES NUMÉRICAS
*===================================================

foreach v in OCI `fexvar' INGLABO RAMA2D P6800 {
    capture confirm numeric variable `v'
    if _rc {
        destring `v', replace force
    }
}


*===================================================
* 3. SOLO OCUPADOS
*===================================================

keep if OCI == 1


*===================================================
* 4. DEFINIR FACTOR DE EXPANSIÓN
*===================================================
* Como se trabaja con base anual construida desde meses,
* se divide el factor entre 12 para obtener trabajadores
* promedio del año.

gen FEX = `fexvar'/12
local fac "FEX"


*===================================================
* 5. FILTROS BASE
*===================================================

keep if !missing(INGLABO)
keep if INGLABO > 0

keep if !missing(`fac')
keep if `fac' > 0

keep if !missing(RAMA2D)


*===================================================
* 6. CREAR SECCIÓN CIIU REV. 3
*===================================================

gen str1 seccion = ""
gen str120 desc_seccion = ""

replace seccion = "A" if inlist(RAMA2D, 1, 2)
replace desc_seccion = "Agricultura, ganadería, caza y silvicultura" if seccion == "A"

replace seccion = "B" if RAMA2D == 5
replace desc_seccion = "Pesca" if seccion == "B"

replace seccion = "C" if inrange(RAMA2D, 10, 14)
replace desc_seccion = "Explotación de minas y canteras" if seccion == "C"

replace seccion = "D" if inrange(RAMA2D, 15, 37)
replace desc_seccion = "Industrias manufactureras" if seccion == "D"

replace seccion = "E" if inrange(RAMA2D, 40, 41)
replace desc_seccion = "Suministro de electricidad, gas y agua" if seccion == "E"

replace seccion = "F" if RAMA2D == 45
replace desc_seccion = "Construcción" if seccion == "F"

replace seccion = "G" if inrange(RAMA2D, 50, 52)
replace desc_seccion = "Comercio; reparación de vehículos y efectos personales" if seccion == "G"

replace seccion = "H" if RAMA2D == 55
replace desc_seccion = "Hoteles y restaurantes" if seccion == "H"

replace seccion = "I" if inrange(RAMA2D, 60, 64)
replace desc_seccion = "Transporte, almacenamiento y comunicaciones" if seccion == "I"

replace seccion = "J" if inrange(RAMA2D, 65, 67)
replace desc_seccion = "Intermediación financiera" if seccion == "J"

replace seccion = "K" if inrange(RAMA2D, 70, 74)
replace desc_seccion = "Actividades inmobiliarias, empresariales y de alquiler" if seccion == "K"

replace seccion = "L" if RAMA2D == 75
replace desc_seccion = "Administración pública y defensa; seguridad social obligatoria" if seccion == "L"

replace seccion = "M" if RAMA2D == 80
replace desc_seccion = "Educación" if seccion == "M"

replace seccion = "N" if RAMA2D == 85
replace desc_seccion = "Servicios sociales y de salud" if seccion == "N"

replace seccion = "O" if inrange(RAMA2D, 90, 93)
replace desc_seccion = "Otras actividades de servicios comunitarios, sociales y personales" if seccion == "O"

replace seccion = "P" if RAMA2D == 95
replace desc_seccion = "Hogares privados con servicio doméstico" if seccion == "P"

replace seccion = "Q" if RAMA2D == 99
replace desc_seccion = "Organizaciones y órganos extraterritoriales" if seccion == "Q"

drop if seccion == ""


*===================================================
* 7. INGRESO LABORAL ANUAL E INGRESO POR HORA
*===================================================

gen INGLABO_ANUAL = INGLABO * 12

gen horas_semana = P6800
replace horas_semana = . if missing(horas_semana)
replace horas_semana = . if horas_semana <= 0
replace horas_semana = . if horas_semana > 168

gen INGLABO_HORA = INGLABO_ANUAL / (horas_semana * 52) if !missing(horas_semana)


*===================================================
* 8. CALCULAR RESULTADOS POR SECCIÓN
*===================================================

tempfile resultados
tempname memhold

capture postutil clear

postfile `memhold' ///
    str1 seccion ///
    str120 desc_seccion ///
    double n_obs ///
    double trabajadores ///
    double total_inglabo_anual ///
    double mean_inglabo_anual ///
    double mean_inglabo_mensual ///
    double median_inglabo_anual ///
    double median_inglabo_mensual ///
    double mean_inglabo_hora ///
    double median_inglabo_hora ///
    using `resultados', replace


levelsof seccion, local(lista_sec)

foreach s of local lista_sec {

    preserve

        keep if seccion == "`s'"

        count
        local nobs = r(N)

        local desc "`=desc_seccion[1]'"

        * Trabajadores expandidos
        quietly summarize `fac', meanonly
        local trabajadores = r(sum)

        * Total ingreso laboral anual expandido
        gen ingreso_anual_expandido = INGLABO_ANUAL * `fac'
        quietly summarize ingreso_anual_expandido, meanonly
        local total_inglabo_anual = r(sum)

        * Promedio ponderado ingreso anual
        local mean_inglabo_anual = `total_inglabo_anual' / `trabajadores'

        * Promedio mensual equivalente
        local mean_inglabo_mensual = `mean_inglabo_anual' / 12

        * Mediana ponderada ingreso anual
        sort INGLABO_ANUAL
        gen peso = `fac'
        egen peso_total = total(peso)
        gen peso_acum = sum(peso)
        gen frac_acum = peso_acum / peso_total

        quietly summarize INGLABO_ANUAL if frac_acum >= 0.5, meanonly
        local median_inglabo_anual = r(min)

        * Mediana mensual equivalente
        local median_inglabo_mensual = `median_inglabo_anual' / 12

        * Promedio y mediana ingreso por hora
        local mean_inglabo_hora = .
        local median_inglabo_hora = .

        quietly count if !missing(INGLABO_HORA)

        if r(N) > 0 {

            gen ingreso_hora_expandido = INGLABO_HORA * `fac' if !missing(INGLABO_HORA)

            quietly summarize ingreso_hora_expandido, meanonly
            local total_inglabo_hora = r(sum)

            quietly summarize `fac' if !missing(INGLABO_HORA), meanonly
            local trabajadores_hora = r(sum)

            local mean_inglabo_hora = `total_inglabo_hora' / `trabajadores_hora'

            sort INGLABO_HORA
            gen peso_hora = `fac' if !missing(INGLABO_HORA)
            egen peso_total_hora = total(peso_hora)
            gen peso_acum_hora = sum(peso_hora) if !missing(INGLABO_HORA)
            gen frac_acum_hora = peso_acum_hora / peso_total_hora

            quietly summarize INGLABO_HORA if frac_acum_hora >= 0.5 & !missing(INGLABO_HORA), meanonly
            local median_inglabo_hora = r(min)
        }

        post `memhold' ///
            ("`s'") ///
            ("`desc'") ///
            (`nobs') ///
            (`trabajadores') ///
            (`total_inglabo_anual') ///
            (`mean_inglabo_anual') ///
            (`mean_inglabo_mensual') ///
            (`median_inglabo_anual') ///
            (`median_inglabo_mensual') ///
            (`mean_inglabo_hora') ///
            (`median_inglabo_hora')

    restore
}

postclose `memhold'


*===================================================
* 9. ORGANIZAR RESULTADOS
*===================================================

use `resultados', clear

sort seccion

order seccion desc_seccion ///
      trabajadores ///
      mean_inglabo_anual mean_inglabo_mensual ///
      median_inglabo_anual median_inglabo_mensual ///
      mean_inglabo_hora median_inglabo_hora ///
      total_inglabo_anual n_obs

label variable seccion "Sección CIIU Rev. 3"
label variable desc_seccion "Descripción de sección"
label variable trabajadores "Trabajadores expandidos"
label variable mean_inglabo_anual "Ingreso laboral anual promedio"
label variable mean_inglabo_mensual "Ingreso laboral mensual promedio"
label variable median_inglabo_anual "Ingreso laboral anual mediano"
label variable median_inglabo_mensual "Ingreso laboral mensual mediano"
label variable mean_inglabo_hora "Ingreso laboral por hora promedio"
label variable median_inglabo_hora "Ingreso laboral por hora mediano"
label variable total_inglabo_anual "Total ingreso laboral anual expandido"
label variable n_obs "Observaciones no expandidas"

format trabajadores %15.0fc
format total_inglabo_anual %18.0fc
format mean_inglabo_anual %15.2fc
format mean_inglabo_mensual %15.2fc
format median_inglabo_anual %15.2fc
format median_inglabo_mensual %15.2fc
format mean_inglabo_hora %15.2fc
format median_inglabo_hora %15.2fc


*===================================================
* 10. EXPORTAR A EXCEL Y GUARDAR DTA
*===================================================

save "Outputs/tables/consolidadosector_`year'.dta", replace

export excel using "`archivo'", ///
    sheet("consolidado_sector_`year'") ///
    firstrow(varlabels) ///
    replace

di "===================================================="
di "Archivo exportado correctamente:"
di "`archivo'"
di "Hoja: consolidado_sector_`year'"
di "Clasificación: CIIU Rev. 3 usando RAMA2D"
di "===================================================="

****************************************************
* CONSOLIDADO POR SECTOR / SECCIÓN CIIU
* GEIH 2015
* Sin discriminación por tamaño de empresa
* Usando RAMA2D - CIIU Rev. 3
****************************************************

clear all
set more off

cd "C:/Users/jorge/Documents/Databases/GEIH"

local year 2015

capture mkdir "Outputs"
capture mkdir "Outputs/tables"

local archivo "Outputs/tables/consolidadosector_`year'.xlsx"
capture erase "`archivo'"

use "GEIH_`year'_TOTAL.dta", clear


*===================================================
* 0. IDENTIFICAR FACTOR DE EXPANSIÓN
*===================================================

capture confirm variable FEX_C18
if !_rc {
    local fexvar "FEX_C18"
}
else {
    capture confirm variable FEX_C
    if !_rc {
        local fexvar "FEX_C"
    }
    else {
        di as error "No existe FEX_C18 ni FEX_C en la base."
        exit 111
    }
}

di as text "Factor de expansión usado: `fexvar'"


*===================================================
* 1. VERIFICAR VARIABLES NECESARIAS
*===================================================

foreach v in OCI `fexvar' INGLABO RAMA2D P6800 {
    capture confirm variable `v'
    if _rc {
        di as error "No existe la variable `v' en la base."
        exit 111
    }
}


*===================================================
* 2. ASEGURAR VARIABLES NUMÉRICAS
*===================================================

foreach v in OCI `fexvar' INGLABO RAMA2D P6800 {
    capture confirm numeric variable `v'
    if _rc {
        destring `v', replace force
    }
}


*===================================================
* 3. SOLO OCUPADOS
*===================================================

keep if OCI == 1


*===================================================
* 4. DEFINIR FACTOR DE EXPANSIÓN
*===================================================
* Como se trabaja con base anual construida desde meses,
* se divide el factor entre 12 para obtener trabajadores
* promedio del año.

gen FEX = `fexvar'/12
local fac "FEX"


*===================================================
* 5. FILTROS BASE
*===================================================

keep if !missing(INGLABO)
keep if INGLABO > 0

keep if !missing(`fac')
keep if `fac' > 0

keep if !missing(RAMA2D)


*===================================================
* 6. CREAR SECCIÓN CIIU REV. 3
*===================================================

gen str1 seccion = ""
gen str120 desc_seccion = ""

replace seccion = "A" if inlist(RAMA2D, 1, 2)
replace desc_seccion = "Agricultura, ganadería, caza y silvicultura" if seccion == "A"

replace seccion = "B" if RAMA2D == 5
replace desc_seccion = "Pesca" if seccion == "B"

replace seccion = "C" if inrange(RAMA2D, 10, 14)
replace desc_seccion = "Explotación de minas y canteras" if seccion == "C"

replace seccion = "D" if inrange(RAMA2D, 15, 37)
replace desc_seccion = "Industrias manufactureras" if seccion == "D"

replace seccion = "E" if inrange(RAMA2D, 40, 41)
replace desc_seccion = "Suministro de electricidad, gas y agua" if seccion == "E"

replace seccion = "F" if RAMA2D == 45
replace desc_seccion = "Construcción" if seccion == "F"

replace seccion = "G" if inrange(RAMA2D, 50, 52)
replace desc_seccion = "Comercio; reparación de vehículos y efectos personales" if seccion == "G"

replace seccion = "H" if RAMA2D == 55
replace desc_seccion = "Hoteles y restaurantes" if seccion == "H"

replace seccion = "I" if inrange(RAMA2D, 60, 64)
replace desc_seccion = "Transporte, almacenamiento y comunicaciones" if seccion == "I"

replace seccion = "J" if inrange(RAMA2D, 65, 67)
replace desc_seccion = "Intermediación financiera" if seccion == "J"

replace seccion = "K" if inrange(RAMA2D, 70, 74)
replace desc_seccion = "Actividades inmobiliarias, empresariales y de alquiler" if seccion == "K"

replace seccion = "L" if RAMA2D == 75
replace desc_seccion = "Administración pública y defensa; seguridad social obligatoria" if seccion == "L"

replace seccion = "M" if RAMA2D == 80
replace desc_seccion = "Educación" if seccion == "M"

replace seccion = "N" if RAMA2D == 85
replace desc_seccion = "Servicios sociales y de salud" if seccion == "N"

replace seccion = "O" if inrange(RAMA2D, 90, 93)
replace desc_seccion = "Otras actividades de servicios comunitarios, sociales y personales" if seccion == "O"

replace seccion = "P" if RAMA2D == 95
replace desc_seccion = "Hogares privados con servicio doméstico" if seccion == "P"

replace seccion = "Q" if RAMA2D == 99
replace desc_seccion = "Organizaciones y órganos extraterritoriales" if seccion == "Q"

drop if seccion == ""


*===================================================
* 7. INGRESO LABORAL ANUAL E INGRESO POR HORA
*===================================================

gen INGLABO_ANUAL = INGLABO * 12

gen horas_semana = P6800
replace horas_semana = . if missing(horas_semana)
replace horas_semana = . if horas_semana <= 0
replace horas_semana = . if horas_semana > 168

gen INGLABO_HORA = INGLABO_ANUAL / (horas_semana * 52) if !missing(horas_semana)


*===================================================
* 8. CALCULAR RESULTADOS POR SECCIÓN
*===================================================

tempfile resultados
tempname memhold

capture postutil clear

postfile `memhold' ///
    str1 seccion ///
    str120 desc_seccion ///
    double n_obs ///
    double trabajadores ///
    double total_inglabo_anual ///
    double mean_inglabo_anual ///
    double mean_inglabo_mensual ///
    double median_inglabo_anual ///
    double median_inglabo_mensual ///
    double mean_inglabo_hora ///
    double median_inglabo_hora ///
    using `resultados', replace


levelsof seccion, local(lista_sec)

foreach s of local lista_sec {

    preserve

        keep if seccion == "`s'"

        count
        local nobs = r(N)

        local desc "`=desc_seccion[1]'"

        * Trabajadores expandidos
        quietly summarize `fac', meanonly
        local trabajadores = r(sum)

        * Total ingreso laboral anual expandido
        gen ingreso_anual_expandido = INGLABO_ANUAL * `fac'
        quietly summarize ingreso_anual_expandido, meanonly
        local total_inglabo_anual = r(sum)

        * Promedio ponderado ingreso anual
        local mean_inglabo_anual = `total_inglabo_anual' / `trabajadores'

        * Promedio mensual equivalente
        local mean_inglabo_mensual = `mean_inglabo_anual' / 12

        * Mediana ponderada ingreso anual
        sort INGLABO_ANUAL
        gen peso = `fac'
        egen peso_total = total(peso)
        gen peso_acum = sum(peso)
        gen frac_acum = peso_acum / peso_total

        quietly summarize INGLABO_ANUAL if frac_acum >= 0.5, meanonly
        local median_inglabo_anual = r(min)

        * Mediana mensual equivalente
        local median_inglabo_mensual = `median_inglabo_anual' / 12

        * Promedio y mediana ingreso por hora
        local mean_inglabo_hora = .
        local median_inglabo_hora = .

        quietly count if !missing(INGLABO_HORA)

        if r(N) > 0 {

            gen ingreso_hora_expandido = INGLABO_HORA * `fac' if !missing(INGLABO_HORA)

            quietly summarize ingreso_hora_expandido, meanonly
            local total_inglabo_hora = r(sum)

            quietly summarize `fac' if !missing(INGLABO_HORA), meanonly
            local trabajadores_hora = r(sum)

            local mean_inglabo_hora = `total_inglabo_hora' / `trabajadores_hora'

            sort INGLABO_HORA
            gen peso_hora = `fac' if !missing(INGLABO_HORA)
            egen peso_total_hora = total(peso_hora)
            gen peso_acum_hora = sum(peso_hora) if !missing(INGLABO_HORA)
            gen frac_acum_hora = peso_acum_hora / peso_total_hora

            quietly summarize INGLABO_HORA if frac_acum_hora >= 0.5 & !missing(INGLABO_HORA), meanonly
            local median_inglabo_hora = r(min)
        }

        post `memhold' ///
            ("`s'") ///
            ("`desc'") ///
            (`nobs') ///
            (`trabajadores') ///
            (`total_inglabo_anual') ///
            (`mean_inglabo_anual') ///
            (`mean_inglabo_mensual') ///
            (`median_inglabo_anual') ///
            (`median_inglabo_mensual') ///
            (`mean_inglabo_hora') ///
            (`median_inglabo_hora')

    restore
}

postclose `memhold'


*===================================================
* 9. ORGANIZAR RESULTADOS
*===================================================

use `resultados', clear

sort seccion

order seccion desc_seccion ///
      trabajadores ///
      mean_inglabo_anual mean_inglabo_mensual ///
      median_inglabo_anual median_inglabo_mensual ///
      mean_inglabo_hora median_inglabo_hora ///
      total_inglabo_anual n_obs

label variable seccion "Sección CIIU Rev. 3"
label variable desc_seccion "Descripción de sección"
label variable trabajadores "Trabajadores expandidos"
label variable mean_inglabo_anual "Ingreso laboral anual promedio"
label variable mean_inglabo_mensual "Ingreso laboral mensual promedio"
label variable median_inglabo_anual "Ingreso laboral anual mediano"
label variable median_inglabo_mensual "Ingreso laboral mensual mediano"
label variable mean_inglabo_hora "Ingreso laboral por hora promedio"
label variable median_inglabo_hora "Ingreso laboral por hora mediano"
label variable total_inglabo_anual "Total ingreso laboral anual expandido"
label variable n_obs "Observaciones no expandidas"

format trabajadores %15.0fc
format total_inglabo_anual %18.0fc
format mean_inglabo_anual %15.2fc
format mean_inglabo_mensual %15.2fc
format median_inglabo_anual %15.2fc
format median_inglabo_mensual %15.2fc
format mean_inglabo_hora %15.2fc
format median_inglabo_hora %15.2fc


*===================================================
* 10. EXPORTAR A EXCEL Y GUARDAR DTA
*===================================================

save "Outputs/tables/consolidadosector_`year'.dta", replace

export excel using "`archivo'", ///
    sheet("consolidado_sector_`year'") ///
    firstrow(varlabels) ///
    replace

di "===================================================="
di "Archivo exportado correctamente:"
di "`archivo'"
di "Hoja: consolidado_sector_`year'"
di "Clasificación: CIIU Rev. 3 usando RAMA2D"
di "===================================================="

****************************************************
* CONSOLIDADO POR SECTOR / SECCIÓN CIIU
* GEIH 2016
* Sin discriminación por tamaño de empresa
* Usando RAMA2D - CIIU Rev. 3
****************************************************

clear all
set more off

cd "C:/Users/jorge/Documents/Databases/GEIH"

local year 2016

capture mkdir "Outputs"
capture mkdir "Outputs/tables"

local archivo "Outputs/tables/consolidadosector_`year'.xlsx"
capture erase "`archivo'"

use "GEIH_`year'_TOTAL.dta", clear


*===================================================
* 0. IDENTIFICAR FACTOR DE EXPANSIÓN
*===================================================

capture confirm variable FEX_C18
if !_rc {
    local fexvar "FEX_C18"
}
else {
    capture confirm variable FEX_C
    if !_rc {
        local fexvar "FEX_C"
    }
    else {
        di as error "No existe FEX_C18 ni FEX_C en la base."
        exit 111
    }
}

di as text "Factor de expansión usado: `fexvar'"


*===================================================
* 1. VERIFICAR VARIABLES NECESARIAS
*===================================================

foreach v in OCI `fexvar' INGLABO RAMA2D P6800 {
    capture confirm variable `v'
    if _rc {
        di as error "No existe la variable `v' en la base."
        exit 111
    }
}


*===================================================
* 2. ASEGURAR VARIABLES NUMÉRICAS
*===================================================

foreach v in OCI `fexvar' INGLABO RAMA2D P6800 {
    capture confirm numeric variable `v'
    if _rc {
        destring `v', replace force
    }
}


*===================================================
* 3. SOLO OCUPADOS
*===================================================

keep if OCI == 1


*===================================================
* 4. DEFINIR FACTOR DE EXPANSIÓN
*===================================================
* Como se trabaja con base anual construida desde meses,
* se divide el factor entre 12 para obtener trabajadores
* promedio del año.

gen FEX = `fexvar'/12
local fac "FEX"


*===================================================
* 5. FILTROS BASE
*===================================================

keep if !missing(INGLABO)
keep if INGLABO > 0

keep if !missing(`fac')
keep if `fac' > 0

keep if !missing(RAMA2D)


*===================================================
* 6. CREAR SECCIÓN CIIU REV. 3
*===================================================

gen str1 seccion = ""
gen str120 desc_seccion = ""

replace seccion = "A" if inlist(RAMA2D, 1, 2)
replace desc_seccion = "Agricultura, ganadería, caza y silvicultura" if seccion == "A"

replace seccion = "B" if RAMA2D == 5
replace desc_seccion = "Pesca" if seccion == "B"

replace seccion = "C" if inrange(RAMA2D, 10, 14)
replace desc_seccion = "Explotación de minas y canteras" if seccion == "C"

replace seccion = "D" if inrange(RAMA2D, 15, 37)
replace desc_seccion = "Industrias manufactureras" if seccion == "D"

replace seccion = "E" if inrange(RAMA2D, 40, 41)
replace desc_seccion = "Suministro de electricidad, gas y agua" if seccion == "E"

replace seccion = "F" if RAMA2D == 45
replace desc_seccion = "Construcción" if seccion == "F"

replace seccion = "G" if inrange(RAMA2D, 50, 52)
replace desc_seccion = "Comercio; reparación de vehículos y efectos personales" if seccion == "G"

replace seccion = "H" if RAMA2D == 55
replace desc_seccion = "Hoteles y restaurantes" if seccion == "H"

replace seccion = "I" if inrange(RAMA2D, 60, 64)
replace desc_seccion = "Transporte, almacenamiento y comunicaciones" if seccion == "I"

replace seccion = "J" if inrange(RAMA2D, 65, 67)
replace desc_seccion = "Intermediación financiera" if seccion == "J"

replace seccion = "K" if inrange(RAMA2D, 70, 74)
replace desc_seccion = "Actividades inmobiliarias, empresariales y de alquiler" if seccion == "K"

replace seccion = "L" if RAMA2D == 75
replace desc_seccion = "Administración pública y defensa; seguridad social obligatoria" if seccion == "L"

replace seccion = "M" if RAMA2D == 80
replace desc_seccion = "Educación" if seccion == "M"

replace seccion = "N" if RAMA2D == 85
replace desc_seccion = "Servicios sociales y de salud" if seccion == "N"

replace seccion = "O" if inrange(RAMA2D, 90, 93)
replace desc_seccion = "Otras actividades de servicios comunitarios, sociales y personales" if seccion == "O"

replace seccion = "P" if RAMA2D == 95
replace desc_seccion = "Hogares privados con servicio doméstico" if seccion == "P"

replace seccion = "Q" if RAMA2D == 99
replace desc_seccion = "Organizaciones y órganos extraterritoriales" if seccion == "Q"

drop if seccion == ""


*===================================================
* 7. INGRESO LABORAL ANUAL E INGRESO POR HORA
*===================================================

gen INGLABO_ANUAL = INGLABO * 12

gen horas_semana = P6800
replace horas_semana = . if missing(horas_semana)
replace horas_semana = . if horas_semana <= 0
replace horas_semana = . if horas_semana > 168

gen INGLABO_HORA = INGLABO_ANUAL / (horas_semana * 52) if !missing(horas_semana)


*===================================================
* 8. CALCULAR RESULTADOS POR SECCIÓN
*===================================================

tempfile resultados
tempname memhold

capture postutil clear

postfile `memhold' ///
    str1 seccion ///
    str120 desc_seccion ///
    double n_obs ///
    double trabajadores ///
    double total_inglabo_anual ///
    double mean_inglabo_anual ///
    double mean_inglabo_mensual ///
    double median_inglabo_anual ///
    double median_inglabo_mensual ///
    double mean_inglabo_hora ///
    double median_inglabo_hora ///
    using `resultados', replace


levelsof seccion, local(lista_sec)

foreach s of local lista_sec {

    preserve

        keep if seccion == "`s'"

        count
        local nobs = r(N)

        local desc "`=desc_seccion[1]'"

        * Trabajadores expandidos
        quietly summarize `fac', meanonly
        local trabajadores = r(sum)

        * Total ingreso laboral anual expandido
        gen ingreso_anual_expandido = INGLABO_ANUAL * `fac'
        quietly summarize ingreso_anual_expandido, meanonly
        local total_inglabo_anual = r(sum)

        * Promedio ponderado ingreso anual
        local mean_inglabo_anual = `total_inglabo_anual' / `trabajadores'

        * Promedio mensual equivalente
        local mean_inglabo_mensual = `mean_inglabo_anual' / 12

        * Mediana ponderada ingreso anual
        sort INGLABO_ANUAL
        gen peso = `fac'
        egen peso_total = total(peso)
        gen peso_acum = sum(peso)
        gen frac_acum = peso_acum / peso_total

        quietly summarize INGLABO_ANUAL if frac_acum >= 0.5, meanonly
        local median_inglabo_anual = r(min)

        * Mediana mensual equivalente
        local median_inglabo_mensual = `median_inglabo_anual' / 12

        * Promedio y mediana ingreso por hora
        local mean_inglabo_hora = .
        local median_inglabo_hora = .

        quietly count if !missing(INGLABO_HORA)

        if r(N) > 0 {

            gen ingreso_hora_expandido = INGLABO_HORA * `fac' if !missing(INGLABO_HORA)

            quietly summarize ingreso_hora_expandido, meanonly
            local total_inglabo_hora = r(sum)

            quietly summarize `fac' if !missing(INGLABO_HORA), meanonly
            local trabajadores_hora = r(sum)

            local mean_inglabo_hora = `total_inglabo_hora' / `trabajadores_hora'

            sort INGLABO_HORA
            gen peso_hora = `fac' if !missing(INGLABO_HORA)
            egen peso_total_hora = total(peso_hora)
            gen peso_acum_hora = sum(peso_hora) if !missing(INGLABO_HORA)
            gen frac_acum_hora = peso_acum_hora / peso_total_hora

            quietly summarize INGLABO_HORA if frac_acum_hora >= 0.5 & !missing(INGLABO_HORA), meanonly
            local median_inglabo_hora = r(min)
        }

        post `memhold' ///
            ("`s'") ///
            ("`desc'") ///
            (`nobs') ///
            (`trabajadores') ///
            (`total_inglabo_anual') ///
            (`mean_inglabo_anual') ///
            (`mean_inglabo_mensual') ///
            (`median_inglabo_anual') ///
            (`median_inglabo_mensual') ///
            (`mean_inglabo_hora') ///
            (`median_inglabo_hora')

    restore
}

postclose `memhold'


*===================================================
* 9. ORGANIZAR RESULTADOS
*===================================================

use `resultados', clear

sort seccion

order seccion desc_seccion ///
      trabajadores ///
      mean_inglabo_anual mean_inglabo_mensual ///
      median_inglabo_anual median_inglabo_mensual ///
      mean_inglabo_hora median_inglabo_hora ///
      total_inglabo_anual n_obs

label variable seccion "Sección CIIU Rev. 3"
label variable desc_seccion "Descripción de sección"
label variable trabajadores "Trabajadores expandidos"
label variable mean_inglabo_anual "Ingreso laboral anual promedio"
label variable mean_inglabo_mensual "Ingreso laboral mensual promedio"
label variable median_inglabo_anual "Ingreso laboral anual mediano"
label variable median_inglabo_mensual "Ingreso laboral mensual mediano"
label variable mean_inglabo_hora "Ingreso laboral por hora promedio"
label variable median_inglabo_hora "Ingreso laboral por hora mediano"
label variable total_inglabo_anual "Total ingreso laboral anual expandido"
label variable n_obs "Observaciones no expandidas"

format trabajadores %15.0fc
format total_inglabo_anual %18.0fc
format mean_inglabo_anual %15.2fc
format mean_inglabo_mensual %15.2fc
format median_inglabo_anual %15.2fc
format median_inglabo_mensual %15.2fc
format mean_inglabo_hora %15.2fc
format median_inglabo_hora %15.2fc


*===================================================
* 10. EXPORTAR A EXCEL Y GUARDAR DTA
*===================================================

save "Outputs/tables/consolidadosector_`year'.dta", replace

export excel using "`archivo'", ///
    sheet("consolidado_sector_`year'") ///
    firstrow(varlabels) ///
    replace

di "===================================================="
di "Archivo exportado correctamente:"
di "`archivo'"
di "Hoja: consolidado_sector_`year'"
di "Clasificación: CIIU Rev. 3 usando RAMA2D"
di "===================================================="

****************************************************
* CONSOLIDADO POR SECTOR / SECCIÓN CIIU
* GEIH 2017
* Sin discriminación por tamaño de empresa
* Usando RAMA2D - CIIU Rev. 3
****************************************************

clear all
set more off

cd "C:/Users/jorge/Documents/Databases/GEIH"

local year 2017

capture mkdir "Outputs"
capture mkdir "Outputs/tables"

local archivo "Outputs/tables/consolidadosector_`year'.xlsx"
capture erase "`archivo'"

use "GEIH_`year'_TOTAL.dta", clear


*===================================================
* 0. IDENTIFICAR FACTOR DE EXPANSIÓN
*===================================================

capture confirm variable FEX_C18
if !_rc {
    local fexvar "FEX_C18"
}
else {
    capture confirm variable FEX_C
    if !_rc {
        local fexvar "FEX_C"
    }
    else {
        di as error "No existe FEX_C18 ni FEX_C en la base."
        exit 111
    }
}

di as text "Factor de expansión usado: `fexvar'"


*===================================================
* 1. VERIFICAR VARIABLES NECESARIAS
*===================================================

foreach v in OCI `fexvar' INGLABO RAMA2D P6800 {
    capture confirm variable `v'
    if _rc {
        di as error "No existe la variable `v' en la base."
        exit 111
    }
}


*===================================================
* 2. ASEGURAR VARIABLES NUMÉRICAS
*===================================================

foreach v in OCI `fexvar' INGLABO RAMA2D P6800 {
    capture confirm numeric variable `v'
    if _rc {
        destring `v', replace force
    }
}


*===================================================
* 3. SOLO OCUPADOS
*===================================================

keep if OCI == 1


*===================================================
* 4. DEFINIR FACTOR DE EXPANSIÓN
*===================================================
* Como se trabaja con base anual construida desde meses,
* se divide el factor entre 12 para obtener trabajadores
* promedio del año.

gen FEX = `fexvar'/12
local fac "FEX"


*===================================================
* 5. FILTROS BASE
*===================================================

keep if !missing(INGLABO)
keep if INGLABO > 0

keep if !missing(`fac')
keep if `fac' > 0

keep if !missing(RAMA2D)


*===================================================
* 6. CREAR SECCIÓN CIIU REV. 3
*===================================================

gen str1 seccion = ""
gen str120 desc_seccion = ""

replace seccion = "A" if inlist(RAMA2D, 1, 2)
replace desc_seccion = "Agricultura, ganadería, caza y silvicultura" if seccion == "A"

replace seccion = "B" if RAMA2D == 5
replace desc_seccion = "Pesca" if seccion == "B"

replace seccion = "C" if inrange(RAMA2D, 10, 14)
replace desc_seccion = "Explotación de minas y canteras" if seccion == "C"

replace seccion = "D" if inrange(RAMA2D, 15, 37)
replace desc_seccion = "Industrias manufactureras" if seccion == "D"

replace seccion = "E" if inrange(RAMA2D, 40, 41)
replace desc_seccion = "Suministro de electricidad, gas y agua" if seccion == "E"

replace seccion = "F" if RAMA2D == 45
replace desc_seccion = "Construcción" if seccion == "F"

replace seccion = "G" if inrange(RAMA2D, 50, 52)
replace desc_seccion = "Comercio; reparación de vehículos y efectos personales" if seccion == "G"

replace seccion = "H" if RAMA2D == 55
replace desc_seccion = "Hoteles y restaurantes" if seccion == "H"

replace seccion = "I" if inrange(RAMA2D, 60, 64)
replace desc_seccion = "Transporte, almacenamiento y comunicaciones" if seccion == "I"

replace seccion = "J" if inrange(RAMA2D, 65, 67)
replace desc_seccion = "Intermediación financiera" if seccion == "J"

replace seccion = "K" if inrange(RAMA2D, 70, 74)
replace desc_seccion = "Actividades inmobiliarias, empresariales y de alquiler" if seccion == "K"

replace seccion = "L" if RAMA2D == 75
replace desc_seccion = "Administración pública y defensa; seguridad social obligatoria" if seccion == "L"

replace seccion = "M" if RAMA2D == 80
replace desc_seccion = "Educación" if seccion == "M"

replace seccion = "N" if RAMA2D == 85
replace desc_seccion = "Servicios sociales y de salud" if seccion == "N"

replace seccion = "O" if inrange(RAMA2D, 90, 93)
replace desc_seccion = "Otras actividades de servicios comunitarios, sociales y personales" if seccion == "O"

replace seccion = "P" if RAMA2D == 95
replace desc_seccion = "Hogares privados con servicio doméstico" if seccion == "P"

replace seccion = "Q" if RAMA2D == 99
replace desc_seccion = "Organizaciones y órganos extraterritoriales" if seccion == "Q"

drop if seccion == ""


*===================================================
* 7. INGRESO LABORAL ANUAL E INGRESO POR HORA
*===================================================

gen INGLABO_ANUAL = INGLABO * 12

gen horas_semana = P6800
replace horas_semana = . if missing(horas_semana)
replace horas_semana = . if horas_semana <= 0
replace horas_semana = . if horas_semana > 168

gen INGLABO_HORA = INGLABO_ANUAL / (horas_semana * 52) if !missing(horas_semana)


*===================================================
* 8. CALCULAR RESULTADOS POR SECCIÓN
*===================================================

tempfile resultados
tempname memhold

capture postutil clear

postfile `memhold' ///
    str1 seccion ///
    str120 desc_seccion ///
    double n_obs ///
    double trabajadores ///
    double total_inglabo_anual ///
    double mean_inglabo_anual ///
    double mean_inglabo_mensual ///
    double median_inglabo_anual ///
    double median_inglabo_mensual ///
    double mean_inglabo_hora ///
    double median_inglabo_hora ///
    using `resultados', replace


levelsof seccion, local(lista_sec)

foreach s of local lista_sec {

    preserve

        keep if seccion == "`s'"

        count
        local nobs = r(N)

        local desc "`=desc_seccion[1]'"

        * Trabajadores expandidos
        quietly summarize `fac', meanonly
        local trabajadores = r(sum)

        * Total ingreso laboral anual expandido
        gen ingreso_anual_expandido = INGLABO_ANUAL * `fac'
        quietly summarize ingreso_anual_expandido, meanonly
        local total_inglabo_anual = r(sum)

        * Promedio ponderado ingreso anual
        local mean_inglabo_anual = `total_inglabo_anual' / `trabajadores'

        * Promedio mensual equivalente
        local mean_inglabo_mensual = `mean_inglabo_anual' / 12

        * Mediana ponderada ingreso anual
        sort INGLABO_ANUAL
        gen peso = `fac'
        egen peso_total = total(peso)
        gen peso_acum = sum(peso)
        gen frac_acum = peso_acum / peso_total

        quietly summarize INGLABO_ANUAL if frac_acum >= 0.5, meanonly
        local median_inglabo_anual = r(min)

        * Mediana mensual equivalente
        local median_inglabo_mensual = `median_inglabo_anual' / 12

        * Promedio y mediana ingreso por hora
        local mean_inglabo_hora = .
        local median_inglabo_hora = .

        quietly count if !missing(INGLABO_HORA)

        if r(N) > 0 {

            gen ingreso_hora_expandido = INGLABO_HORA * `fac' if !missing(INGLABO_HORA)

            quietly summarize ingreso_hora_expandido, meanonly
            local total_inglabo_hora = r(sum)

            quietly summarize `fac' if !missing(INGLABO_HORA), meanonly
            local trabajadores_hora = r(sum)

            local mean_inglabo_hora = `total_inglabo_hora' / `trabajadores_hora'

            sort INGLABO_HORA
            gen peso_hora = `fac' if !missing(INGLABO_HORA)
            egen peso_total_hora = total(peso_hora)
            gen peso_acum_hora = sum(peso_hora) if !missing(INGLABO_HORA)
            gen frac_acum_hora = peso_acum_hora / peso_total_hora

            quietly summarize INGLABO_HORA if frac_acum_hora >= 0.5 & !missing(INGLABO_HORA), meanonly
            local median_inglabo_hora = r(min)
        }

        post `memhold' ///
            ("`s'") ///
            ("`desc'") ///
            (`nobs') ///
            (`trabajadores') ///
            (`total_inglabo_anual') ///
            (`mean_inglabo_anual') ///
            (`mean_inglabo_mensual') ///
            (`median_inglabo_anual') ///
            (`median_inglabo_mensual') ///
            (`mean_inglabo_hora') ///
            (`median_inglabo_hora')

    restore
}

postclose `memhold'


*===================================================
* 9. ORGANIZAR RESULTADOS
*===================================================

use `resultados', clear

sort seccion

order seccion desc_seccion ///
      trabajadores ///
      mean_inglabo_anual mean_inglabo_mensual ///
      median_inglabo_anual median_inglabo_mensual ///
      mean_inglabo_hora median_inglabo_hora ///
      total_inglabo_anual n_obs

label variable seccion "Sección CIIU Rev. 3"
label variable desc_seccion "Descripción de sección"
label variable trabajadores "Trabajadores expandidos"
label variable mean_inglabo_anual "Ingreso laboral anual promedio"
label variable mean_inglabo_mensual "Ingreso laboral mensual promedio"
label variable median_inglabo_anual "Ingreso laboral anual mediano"
label variable median_inglabo_mensual "Ingreso laboral mensual mediano"
label variable mean_inglabo_hora "Ingreso laboral por hora promedio"
label variable median_inglabo_hora "Ingreso laboral por hora mediano"
label variable total_inglabo_anual "Total ingreso laboral anual expandido"
label variable n_obs "Observaciones no expandidas"

format trabajadores %15.0fc
format total_inglabo_anual %18.0fc
format mean_inglabo_anual %15.2fc
format mean_inglabo_mensual %15.2fc
format median_inglabo_anual %15.2fc
format median_inglabo_mensual %15.2fc
format mean_inglabo_hora %15.2fc
format median_inglabo_hora %15.2fc


*===================================================
* 10. EXPORTAR A EXCEL Y GUARDAR DTA
*===================================================

save "Outputs/tables/consolidadosector_`year'.dta", replace

export excel using "`archivo'", ///
    sheet("consolidado_sector_`year'") ///
    firstrow(varlabels) ///
    replace

di "===================================================="
di "Archivo exportado correctamente:"
di "`archivo'"
di "Hoja: consolidado_sector_`year'"
di "Clasificación: CIIU Rev. 3 usando RAMA2D"
di "===================================================="

****************************************************
* CONSOLIDADO POR SECTOR / SECCIÓN CIIU
* GEIH 2018
* Sin discriminación por tamaño de empresa
* Usando RAMA2D - CIIU Rev. 3
****************************************************

clear all
set more off

cd "C:/Users/jorge/Documents/Databases/GEIH"

local year 2018

capture mkdir "Outputs"
capture mkdir "Outputs/tables"

local archivo "Outputs/tables/consolidadosector_`year'.xlsx"
capture erase "`archivo'"

use "GEIH_`year'_TOTAL.dta", clear


*===================================================
* 0. IDENTIFICAR FACTOR DE EXPANSIÓN
*===================================================

capture confirm variable FEX_C18
if !_rc {
    local fexvar "FEX_C18"
}
else {
    capture confirm variable FEX_C
    if !_rc {
        local fexvar "FEX_C"
    }
    else {
        di as error "No existe FEX_C18 ni FEX_C en la base."
        exit 111
    }
}

di as text "Factor de expansión usado: `fexvar'"


*===================================================
* 1. VERIFICAR VARIABLES NECESARIAS
*===================================================

foreach v in OCI `fexvar' INGLABO RAMA2D P6800 {
    capture confirm variable `v'
    if _rc {
        di as error "No existe la variable `v' en la base."
        exit 111
    }
}


*===================================================
* 2. ASEGURAR VARIABLES NUMÉRICAS
*===================================================

foreach v in OCI `fexvar' INGLABO RAMA2D P6800 {
    capture confirm numeric variable `v'
    if _rc {
        destring `v', replace force
    }
}


*===================================================
* 3. SOLO OCUPADOS
*===================================================

keep if OCI == 1


*===================================================
* 4. DEFINIR FACTOR DE EXPANSIÓN
*===================================================
* Como se trabaja con base anual construida desde meses,
* se divide el factor entre 12 para obtener trabajadores
* promedio del año.

gen FEX = `fexvar'/12
local fac "FEX"


*===================================================
* 5. FILTROS BASE
*===================================================

keep if !missing(INGLABO)
keep if INGLABO > 0

keep if !missing(`fac')
keep if `fac' > 0

keep if !missing(RAMA2D)


*===================================================
* 6. CREAR SECCIÓN CIIU REV. 3
*===================================================

gen str1 seccion = ""
gen str120 desc_seccion = ""

replace seccion = "A" if inlist(RAMA2D, 1, 2)
replace desc_seccion = "Agricultura, ganadería, caza y silvicultura" if seccion == "A"

replace seccion = "B" if RAMA2D == 5
replace desc_seccion = "Pesca" if seccion == "B"

replace seccion = "C" if inrange(RAMA2D, 10, 14)
replace desc_seccion = "Explotación de minas y canteras" if seccion == "C"

replace seccion = "D" if inrange(RAMA2D, 15, 37)
replace desc_seccion = "Industrias manufactureras" if seccion == "D"

replace seccion = "E" if inrange(RAMA2D, 40, 41)
replace desc_seccion = "Suministro de electricidad, gas y agua" if seccion == "E"

replace seccion = "F" if RAMA2D == 45
replace desc_seccion = "Construcción" if seccion == "F"

replace seccion = "G" if inrange(RAMA2D, 50, 52)
replace desc_seccion = "Comercio; reparación de vehículos y efectos personales" if seccion == "G"

replace seccion = "H" if RAMA2D == 55
replace desc_seccion = "Hoteles y restaurantes" if seccion == "H"

replace seccion = "I" if inrange(RAMA2D, 60, 64)
replace desc_seccion = "Transporte, almacenamiento y comunicaciones" if seccion == "I"

replace seccion = "J" if inrange(RAMA2D, 65, 67)
replace desc_seccion = "Intermediación financiera" if seccion == "J"

replace seccion = "K" if inrange(RAMA2D, 70, 74)
replace desc_seccion = "Actividades inmobiliarias, empresariales y de alquiler" if seccion == "K"

replace seccion = "L" if RAMA2D == 75
replace desc_seccion = "Administración pública y defensa; seguridad social obligatoria" if seccion == "L"

replace seccion = "M" if RAMA2D == 80
replace desc_seccion = "Educación" if seccion == "M"

replace seccion = "N" if RAMA2D == 85
replace desc_seccion = "Servicios sociales y de salud" if seccion == "N"

replace seccion = "O" if inrange(RAMA2D, 90, 93)
replace desc_seccion = "Otras actividades de servicios comunitarios, sociales y personales" if seccion == "O"

replace seccion = "P" if RAMA2D == 95
replace desc_seccion = "Hogares privados con servicio doméstico" if seccion == "P"

replace seccion = "Q" if RAMA2D == 99
replace desc_seccion = "Organizaciones y órganos extraterritoriales" if seccion == "Q"

drop if seccion == ""


*===================================================
* 7. INGRESO LABORAL ANUAL E INGRESO POR HORA
*===================================================

gen INGLABO_ANUAL = INGLABO * 12

gen horas_semana = P6800
replace horas_semana = . if missing(horas_semana)
replace horas_semana = . if horas_semana <= 0
replace horas_semana = . if horas_semana > 168

gen INGLABO_HORA = INGLABO_ANUAL / (horas_semana * 52) if !missing(horas_semana)


*===================================================
* 8. CALCULAR RESULTADOS POR SECCIÓN
*===================================================

tempfile resultados
tempname memhold

capture postutil clear

postfile `memhold' ///
    str1 seccion ///
    str120 desc_seccion ///
    double n_obs ///
    double trabajadores ///
    double total_inglabo_anual ///
    double mean_inglabo_anual ///
    double mean_inglabo_mensual ///
    double median_inglabo_anual ///
    double median_inglabo_mensual ///
    double mean_inglabo_hora ///
    double median_inglabo_hora ///
    using `resultados', replace


levelsof seccion, local(lista_sec)

foreach s of local lista_sec {

    preserve

        keep if seccion == "`s'"

        count
        local nobs = r(N)

        local desc "`=desc_seccion[1]'"

        * Trabajadores expandidos
        quietly summarize `fac', meanonly
        local trabajadores = r(sum)

        * Total ingreso laboral anual expandido
        gen ingreso_anual_expandido = INGLABO_ANUAL * `fac'
        quietly summarize ingreso_anual_expandido, meanonly
        local total_inglabo_anual = r(sum)

        * Promedio ponderado ingreso anual
        local mean_inglabo_anual = `total_inglabo_anual' / `trabajadores'

        * Promedio mensual equivalente
        local mean_inglabo_mensual = `mean_inglabo_anual' / 12

        * Mediana ponderada ingreso anual
        sort INGLABO_ANUAL
        gen peso = `fac'
        egen peso_total = total(peso)
        gen peso_acum = sum(peso)
        gen frac_acum = peso_acum / peso_total

        quietly summarize INGLABO_ANUAL if frac_acum >= 0.5, meanonly
        local median_inglabo_anual = r(min)

        * Mediana mensual equivalente
        local median_inglabo_mensual = `median_inglabo_anual' / 12

        * Promedio y mediana ingreso por hora
        local mean_inglabo_hora = .
        local median_inglabo_hora = .

        quietly count if !missing(INGLABO_HORA)

        if r(N) > 0 {

            gen ingreso_hora_expandido = INGLABO_HORA * `fac' if !missing(INGLABO_HORA)

            quietly summarize ingreso_hora_expandido, meanonly
            local total_inglabo_hora = r(sum)

            quietly summarize `fac' if !missing(INGLABO_HORA), meanonly
            local trabajadores_hora = r(sum)

            local mean_inglabo_hora = `total_inglabo_hora' / `trabajadores_hora'

            sort INGLABO_HORA
            gen peso_hora = `fac' if !missing(INGLABO_HORA)
            egen peso_total_hora = total(peso_hora)
            gen peso_acum_hora = sum(peso_hora) if !missing(INGLABO_HORA)
            gen frac_acum_hora = peso_acum_hora / peso_total_hora

            quietly summarize INGLABO_HORA if frac_acum_hora >= 0.5 & !missing(INGLABO_HORA), meanonly
            local median_inglabo_hora = r(min)
        }

        post `memhold' ///
            ("`s'") ///
            ("`desc'") ///
            (`nobs') ///
            (`trabajadores') ///
            (`total_inglabo_anual') ///
            (`mean_inglabo_anual') ///
            (`mean_inglabo_mensual') ///
            (`median_inglabo_anual') ///
            (`median_inglabo_mensual') ///
            (`mean_inglabo_hora') ///
            (`median_inglabo_hora')

    restore
}

postclose `memhold'


*===================================================
* 9. ORGANIZAR RESULTADOS
*===================================================

use `resultados', clear

sort seccion

order seccion desc_seccion ///
      trabajadores ///
      mean_inglabo_anual mean_inglabo_mensual ///
      median_inglabo_anual median_inglabo_mensual ///
      mean_inglabo_hora median_inglabo_hora ///
      total_inglabo_anual n_obs

label variable seccion "Sección CIIU Rev. 3"
label variable desc_seccion "Descripción de sección"
label variable trabajadores "Trabajadores expandidos"
label variable mean_inglabo_anual "Ingreso laboral anual promedio"
label variable mean_inglabo_mensual "Ingreso laboral mensual promedio"
label variable median_inglabo_anual "Ingreso laboral anual mediano"
label variable median_inglabo_mensual "Ingreso laboral mensual mediano"
label variable mean_inglabo_hora "Ingreso laboral por hora promedio"
label variable median_inglabo_hora "Ingreso laboral por hora mediano"
label variable total_inglabo_anual "Total ingreso laboral anual expandido"
label variable n_obs "Observaciones no expandidas"

format trabajadores %15.0fc
format total_inglabo_anual %18.0fc
format mean_inglabo_anual %15.2fc
format mean_inglabo_mensual %15.2fc
format median_inglabo_anual %15.2fc
format median_inglabo_mensual %15.2fc
format mean_inglabo_hora %15.2fc
format median_inglabo_hora %15.2fc


*===================================================
* 10. EXPORTAR A EXCEL Y GUARDAR DTA
*===================================================

save "Outputs/tables/consolidadosector_`year'.dta", replace

export excel using "`archivo'", ///
    sheet("consolidado_sector_`year'") ///
    firstrow(varlabels) ///
    replace

di "===================================================="
di "Archivo exportado correctamente:"
di "`archivo'"
di "Hoja: consolidado_sector_`year'"
di "Clasificación: CIIU Rev. 3 usando RAMA2D"
di "===================================================="

****************************************************
* CONSOLIDADO POR SECTOR / SECCIÓN CIIU
* GEIH 2019
* Sin discriminación por tamaño de empresa
* Usando RAMA2D - CIIU Rev. 3
****************************************************

clear all
set more off

cd "C:/Users/jorge/Documents/Databases/GEIH"

local year 2019

capture mkdir "Outputs"
capture mkdir "Outputs/tables"

local archivo "Outputs/tables/consolidadosector_`year'.xlsx"
capture erase "`archivo'"

use "GEIH_`year'_TOTAL.dta", clear


*===================================================
* 0. IDENTIFICAR FACTOR DE EXPANSIÓN
*===================================================

capture confirm variable FEX_C18
if !_rc {
    local fexvar "FEX_C18"
}
else {
    capture confirm variable FEX_C
    if !_rc {
        local fexvar "FEX_C"
    }
    else {
        di as error "No existe FEX_C18 ni FEX_C en la base."
        exit 111
    }
}

di as text "Factor de expansión usado: `fexvar'"


*===================================================
* 1. VERIFICAR VARIABLES NECESARIAS
*===================================================

foreach v in OCI `fexvar' INGLABO RAMA2D P6800 {
    capture confirm variable `v'
    if _rc {
        di as error "No existe la variable `v' en la base."
        exit 111
    }
}


*===================================================
* 2. ASEGURAR VARIABLES NUMÉRICAS
*===================================================

foreach v in OCI `fexvar' INGLABO RAMA2D P6800 {
    capture confirm numeric variable `v'
    if _rc {
        destring `v', replace force
    }
}


*===================================================
* 3. SOLO OCUPADOS
*===================================================

keep if OCI == 1


*===================================================
* 4. DEFINIR FACTOR DE EXPANSIÓN
*===================================================
* Como se trabaja con base anual construida desde meses,
* se divide el factor entre 12 para obtener trabajadores
* promedio del año.

gen FEX = `fexvar'/12
local fac "FEX"


*===================================================
* 5. FILTROS BASE
*===================================================

keep if !missing(INGLABO)
keep if INGLABO > 0

keep if !missing(`fac')
keep if `fac' > 0

keep if !missing(RAMA2D)


*===================================================
* 6. CREAR SECCIÓN CIIU REV. 3
*===================================================

gen str1 seccion = ""
gen str120 desc_seccion = ""

replace seccion = "A" if inlist(RAMA2D, 1, 2)
replace desc_seccion = "Agricultura, ganadería, caza y silvicultura" if seccion == "A"

replace seccion = "B" if RAMA2D == 5
replace desc_seccion = "Pesca" if seccion == "B"

replace seccion = "C" if inrange(RAMA2D, 10, 14)
replace desc_seccion = "Explotación de minas y canteras" if seccion == "C"

replace seccion = "D" if inrange(RAMA2D, 15, 37)
replace desc_seccion = "Industrias manufactureras" if seccion == "D"

replace seccion = "E" if inrange(RAMA2D, 40, 41)
replace desc_seccion = "Suministro de electricidad, gas y agua" if seccion == "E"

replace seccion = "F" if RAMA2D == 45
replace desc_seccion = "Construcción" if seccion == "F"

replace seccion = "G" if inrange(RAMA2D, 50, 52)
replace desc_seccion = "Comercio; reparación de vehículos y efectos personales" if seccion == "G"

replace seccion = "H" if RAMA2D == 55
replace desc_seccion = "Hoteles y restaurantes" if seccion == "H"

replace seccion = "I" if inrange(RAMA2D, 60, 64)
replace desc_seccion = "Transporte, almacenamiento y comunicaciones" if seccion == "I"

replace seccion = "J" if inrange(RAMA2D, 65, 67)
replace desc_seccion = "Intermediación financiera" if seccion == "J"

replace seccion = "K" if inrange(RAMA2D, 70, 74)
replace desc_seccion = "Actividades inmobiliarias, empresariales y de alquiler" if seccion == "K"

replace seccion = "L" if RAMA2D == 75
replace desc_seccion = "Administración pública y defensa; seguridad social obligatoria" if seccion == "L"

replace seccion = "M" if RAMA2D == 80
replace desc_seccion = "Educación" if seccion == "M"

replace seccion = "N" if RAMA2D == 85
replace desc_seccion = "Servicios sociales y de salud" if seccion == "N"

replace seccion = "O" if inrange(RAMA2D, 90, 93)
replace desc_seccion = "Otras actividades de servicios comunitarios, sociales y personales" if seccion == "O"

replace seccion = "P" if RAMA2D == 95
replace desc_seccion = "Hogares privados con servicio doméstico" if seccion == "P"

replace seccion = "Q" if RAMA2D == 99
replace desc_seccion = "Organizaciones y órganos extraterritoriales" if seccion == "Q"

drop if seccion == ""


*===================================================
* 7. INGRESO LABORAL ANUAL E INGRESO POR HORA
*===================================================

gen INGLABO_ANUAL = INGLABO * 12

gen horas_semana = P6800
replace horas_semana = . if missing(horas_semana)
replace horas_semana = . if horas_semana <= 0
replace horas_semana = . if horas_semana > 168

gen INGLABO_HORA = INGLABO_ANUAL / (horas_semana * 52) if !missing(horas_semana)


*===================================================
* 8. CALCULAR RESULTADOS POR SECCIÓN
*===================================================

tempfile resultados
tempname memhold

capture postutil clear

postfile `memhold' ///
    str1 seccion ///
    str120 desc_seccion ///
    double n_obs ///
    double trabajadores ///
    double total_inglabo_anual ///
    double mean_inglabo_anual ///
    double mean_inglabo_mensual ///
    double median_inglabo_anual ///
    double median_inglabo_mensual ///
    double mean_inglabo_hora ///
    double median_inglabo_hora ///
    using `resultados', replace


levelsof seccion, local(lista_sec)

foreach s of local lista_sec {

    preserve

        keep if seccion == "`s'"

        count
        local nobs = r(N)

        local desc "`=desc_seccion[1]'"

        * Trabajadores expandidos
        quietly summarize `fac', meanonly
        local trabajadores = r(sum)

        * Total ingreso laboral anual expandido
        gen ingreso_anual_expandido = INGLABO_ANUAL * `fac'
        quietly summarize ingreso_anual_expandido, meanonly
        local total_inglabo_anual = r(sum)

        * Promedio ponderado ingreso anual
        local mean_inglabo_anual = `total_inglabo_anual' / `trabajadores'

        * Promedio mensual equivalente
        local mean_inglabo_mensual = `mean_inglabo_anual' / 12

        * Mediana ponderada ingreso anual
        sort INGLABO_ANUAL
        gen peso = `fac'
        egen peso_total = total(peso)
        gen peso_acum = sum(peso)
        gen frac_acum = peso_acum / peso_total

        quietly summarize INGLABO_ANUAL if frac_acum >= 0.5, meanonly
        local median_inglabo_anual = r(min)

        * Mediana mensual equivalente
        local median_inglabo_mensual = `median_inglabo_anual' / 12

        * Promedio y mediana ingreso por hora
        local mean_inglabo_hora = .
        local median_inglabo_hora = .

        quietly count if !missing(INGLABO_HORA)

        if r(N) > 0 {

            gen ingreso_hora_expandido = INGLABO_HORA * `fac' if !missing(INGLABO_HORA)

            quietly summarize ingreso_hora_expandido, meanonly
            local total_inglabo_hora = r(sum)

            quietly summarize `fac' if !missing(INGLABO_HORA), meanonly
            local trabajadores_hora = r(sum)

            local mean_inglabo_hora = `total_inglabo_hora' / `trabajadores_hora'

            sort INGLABO_HORA
            gen peso_hora = `fac' if !missing(INGLABO_HORA)
            egen peso_total_hora = total(peso_hora)
            gen peso_acum_hora = sum(peso_hora) if !missing(INGLABO_HORA)
            gen frac_acum_hora = peso_acum_hora / peso_total_hora

            quietly summarize INGLABO_HORA if frac_acum_hora >= 0.5 & !missing(INGLABO_HORA), meanonly
            local median_inglabo_hora = r(min)
        }

        post `memhold' ///
            ("`s'") ///
            ("`desc'") ///
            (`nobs') ///
            (`trabajadores') ///
            (`total_inglabo_anual') ///
            (`mean_inglabo_anual') ///
            (`mean_inglabo_mensual') ///
            (`median_inglabo_anual') ///
            (`median_inglabo_mensual') ///
            (`mean_inglabo_hora') ///
            (`median_inglabo_hora')

    restore
}

postclose `memhold'


*===================================================
* 9. ORGANIZAR RESULTADOS
*===================================================

use `resultados', clear

sort seccion

order seccion desc_seccion ///
      trabajadores ///
      mean_inglabo_anual mean_inglabo_mensual ///
      median_inglabo_anual median_inglabo_mensual ///
      mean_inglabo_hora median_inglabo_hora ///
      total_inglabo_anual n_obs

label variable seccion "Sección CIIU Rev. 3"
label variable desc_seccion "Descripción de sección"
label variable trabajadores "Trabajadores expandidos"
label variable mean_inglabo_anual "Ingreso laboral anual promedio"
label variable mean_inglabo_mensual "Ingreso laboral mensual promedio"
label variable median_inglabo_anual "Ingreso laboral anual mediano"
label variable median_inglabo_mensual "Ingreso laboral mensual mediano"
label variable mean_inglabo_hora "Ingreso laboral por hora promedio"
label variable median_inglabo_hora "Ingreso laboral por hora mediano"
label variable total_inglabo_anual "Total ingreso laboral anual expandido"
label variable n_obs "Observaciones no expandidas"

format trabajadores %15.0fc
format total_inglabo_anual %18.0fc
format mean_inglabo_anual %15.2fc
format mean_inglabo_mensual %15.2fc
format median_inglabo_anual %15.2fc
format median_inglabo_mensual %15.2fc
format mean_inglabo_hora %15.2fc
format median_inglabo_hora %15.2fc


*===================================================
* 10. EXPORTAR A EXCEL Y GUARDAR DTA
*===================================================

save "Outputs/tables/consolidadosector_`year'.dta", replace

export excel using "`archivo'", ///
    sheet("consolidado_sector_`year'") ///
    firstrow(varlabels) ///
    replace

di "===================================================="
di "Archivo exportado correctamente:"
di "`archivo'"
di "Hoja: consolidado_sector_`year'"
di "Clasificación: CIIU Rev. 3 usando RAMA2D"
di "===================================================="

****************************************************
* CONSOLIDADO POR SECTOR / SECCIÓN CIIU
* GEIH 2021
* Sin discriminación por tamaño de empresa
* Detecta RAMA2D_R4 o RAMA2D automáticamente
****************************************************

clear all
set more off

cd "C:/Users/jorge/Documents/Databases/GEIH"

local year 2021

capture mkdir "Outputs"
capture mkdir "Outputs/tables"

local archivo "Outputs/tables/consolidadosector_`year'.xlsx"
capture erase "`archivo'"

use "GEIH_`year'_TOTAL.dta", clear


*===================================================
* 0. IDENTIFICAR FACTOR DE EXPANSIÓN
*===================================================

capture confirm variable FEX_C18
if !_rc {
    local fexvar "FEX_C18"
}
else {
    capture confirm variable FEX_C
    if !_rc {
        local fexvar "FEX_C"
    }
    else {
        di as error "No existe FEX_C18 ni FEX_C en la base."
        exit 111
    }
}

di as text "Factor de expansión usado: `fexvar'"


*===================================================
* 1. IDENTIFICAR VARIABLE DE RAMA ECONÓMICA
*===================================================

capture confirm variable RAMA2D_R4
if !_rc {
    local ramavar "RAMA2D_R4"
    local ciiu_rev "rev4"
}
else {
    capture confirm variable RAMA2D
    if !_rc {
        local ramavar "RAMA2D"
        local ciiu_rev "rev3"
    }
    else {
        di as error "No existe RAMA2D_R4 ni RAMA2D en la base."
        exit 111
    }
}

di as text "Variable de rama usada: `ramavar'"
di as text "Clasificación usada: `ciiu_rev'"


*===================================================
* 2. VERIFICAR VARIABLES NECESARIAS
*===================================================

foreach v in OCI `fexvar' INGLABO `ramavar' P6800 {
    capture confirm variable `v'
    if _rc {
        di as error "No existe la variable `v' en la base."
        exit 111
    }
}


*===================================================
* 3. ASEGURAR VARIABLES NUMÉRICAS
*===================================================

foreach v in OCI `fexvar' INGLABO `ramavar' P6800 {
    capture confirm numeric variable `v'
    if _rc {
        destring `v', replace force
    }
}


*===================================================
* 4. SOLO OCUPADOS
*===================================================

keep if OCI == 1


*===================================================
* 5. DEFINIR FACTOR DE EXPANSIÓN
*===================================================

gen FEX = `fexvar'/12
local fac "FEX"


*===================================================
* 6. FILTROS BASE
*===================================================

keep if !missing(INGLABO)
keep if INGLABO > 0

keep if !missing(`fac')
keep if `fac' > 0

keep if !missing(`ramavar')


*===================================================
* 7. CREAR SECCIÓN CIIU
*===================================================

gen str1 seccion = ""
gen str120 desc_seccion = ""

if "`ciiu_rev'" == "rev4" {

    *===================================================
    * CIIU Rev. 4
    *===================================================

    replace seccion = "A" if inrange(`ramavar', 1, 3)
    replace desc_seccion = "Agricultura, ganadería, caza, silvicultura y pesca" if seccion == "A"

    replace seccion = "B" if inrange(`ramavar', 5, 9)
    replace desc_seccion = "Explotación de minas y canteras" if seccion == "B"

    replace seccion = "C" if inrange(`ramavar', 10, 33)
    replace desc_seccion = "Industrias manufactureras" if seccion == "C"

    replace seccion = "D" if `ramavar' == 35
    replace desc_seccion = "Suministro de electricidad, gas, vapor y aire acondicionado" if seccion == "D"

    replace seccion = "E" if inrange(`ramavar', 36, 39)
    replace desc_seccion = "Distribución de agua; evacuación y tratamiento de aguas residuales" if seccion == "E"

    replace seccion = "F" if inrange(`ramavar', 41, 43)
    replace desc_seccion = "Construcción" if seccion == "F"

    replace seccion = "G" if inrange(`ramavar', 45, 47)
    replace desc_seccion = "Comercio al por mayor y al por menor; reparación de vehículos" if seccion == "G"

    replace seccion = "H" if inrange(`ramavar', 49, 53)
    replace desc_seccion = "Transporte y almacenamiento" if seccion == "H"

    replace seccion = "I" if inrange(`ramavar', 55, 56)
    replace desc_seccion = "Alojamiento y servicios de comida" if seccion == "I"

    replace seccion = "J" if inrange(`ramavar', 58, 63)
    replace desc_seccion = "Información y comunicaciones" if seccion == "J"

    replace seccion = "K" if inrange(`ramavar', 64, 66)
    replace desc_seccion = "Actividades financieras y de seguros" if seccion == "K"

    replace seccion = "L" if `ramavar' == 68
    replace desc_seccion = "Actividades inmobiliarias" if seccion == "L"

    replace seccion = "M" if inrange(`ramavar', 69, 75)
    replace desc_seccion = "Actividades profesionales, científicas y técnicas" if seccion == "M"

    replace seccion = "N" if inrange(`ramavar', 77, 82)
    replace desc_seccion = "Actividades de servicios administrativos y de apoyo" if seccion == "N"

    replace seccion = "O" if `ramavar' == 84
    replace desc_seccion = "Administración pública y defensa; seguridad social obligatoria" if seccion == "O"

    replace seccion = "P" if `ramavar' == 85
    replace desc_seccion = "Educación" if seccion == "P"

    replace seccion = "Q" if inrange(`ramavar', 86, 88)
    replace desc_seccion = "Actividades de atención de la salud humana y de asistencia social" if seccion == "Q"

    replace seccion = "R" if inrange(`ramavar', 90, 93)
    replace desc_seccion = "Actividades artísticas, de entretenimiento y recreación" if seccion == "R"

    replace seccion = "S" if inrange(`ramavar', 94, 96)
    replace desc_seccion = "Otras actividades de servicios" if seccion == "S"

    replace seccion = "T" if inrange(`ramavar', 97, 98)
    replace desc_seccion = "Actividades de los hogares como empleadores" if seccion == "T"

    replace seccion = "U" if `ramavar' == 99
    replace desc_seccion = "Actividades de organizaciones y entidades extraterritoriales" if seccion == "U"
}

else {

    *===================================================
    * CIIU Rev. 3
    *===================================================

    replace seccion = "A" if inlist(`ramavar', 1, 2)
    replace desc_seccion = "Agricultura, ganadería, caza y silvicultura" if seccion == "A"

    replace seccion = "B" if `ramavar' == 5
    replace desc_seccion = "Pesca" if seccion == "B"

    replace seccion = "C" if inrange(`ramavar', 10, 14)
    replace desc_seccion = "Explotación de minas y canteras" if seccion == "C"

    replace seccion = "D" if inrange(`ramavar', 15, 37)
    replace desc_seccion = "Industrias manufactureras" if seccion == "D"

    replace seccion = "E" if inrange(`ramavar', 40, 41)
    replace desc_seccion = "Suministro de electricidad, gas y agua" if seccion == "E"

    replace seccion = "F" if `ramavar' == 45
    replace desc_seccion = "Construcción" if seccion == "F"

    replace seccion = "G" if inrange(`ramavar', 50, 52)
    replace desc_seccion = "Comercio; reparación de vehículos y efectos personales" if seccion == "G"

    replace seccion = "H" if `ramavar' == 55
    replace desc_seccion = "Hoteles y restaurantes" if seccion == "H"

    replace seccion = "I" if inrange(`ramavar', 60, 64)
    replace desc_seccion = "Transporte, almacenamiento y comunicaciones" if seccion == "I"

    replace seccion = "J" if inrange(`ramavar', 65, 67)
    replace desc_seccion = "Intermediación financiera" if seccion == "J"

    replace seccion = "K" if inrange(`ramavar', 70, 74)
    replace desc_seccion = "Actividades inmobiliarias, empresariales y de alquiler" if seccion == "K"

    replace seccion = "L" if `ramavar' == 75
    replace desc_seccion = "Administración pública y defensa; seguridad social obligatoria" if seccion == "L"

    replace seccion = "M" if `ramavar' == 80
    replace desc_seccion = "Educación" if seccion == "M"

    replace seccion = "N" if `ramavar' == 85
    replace desc_seccion = "Servicios sociales y de salud" if seccion == "N"

    replace seccion = "O" if inrange(`ramavar', 90, 93)
    replace desc_seccion = "Otras actividades de servicios comunitarios, sociales y personales" if seccion == "O"

    replace seccion = "P" if `ramavar' == 95
    replace desc_seccion = "Hogares privados con servicio doméstico" if seccion == "P"

    replace seccion = "Q" if `ramavar' == 99
    replace desc_seccion = "Organizaciones y órganos extraterritoriales" if seccion == "Q"
}

drop if seccion == ""


*===================================================
* 8. INGRESO LABORAL ANUAL E INGRESO POR HORA
*===================================================

gen INGLABO_ANUAL = INGLABO * 12

gen horas_semana = P6800
replace horas_semana = . if missing(horas_semana)
replace horas_semana = . if horas_semana <= 0
replace horas_semana = . if horas_semana > 168

gen INGLABO_HORA = INGLABO_ANUAL / (horas_semana * 52) if !missing(horas_semana)


*===================================================
* 9. CALCULAR RESULTADOS POR SECCIÓN
*===================================================

tempfile resultados
tempname memhold

capture postutil clear

postfile `memhold' ///
    str1 seccion ///
    str120 desc_seccion ///
    double n_obs ///
    double trabajadores ///
    double total_inglabo_anual ///
    double mean_inglabo_anual ///
    double mean_inglabo_mensual ///
    double median_inglabo_anual ///
    double median_inglabo_mensual ///
    double mean_inglabo_hora ///
    double median_inglabo_hora ///
    using `resultados', replace


levelsof seccion, local(lista_sec)

foreach s of local lista_sec {

    preserve

        keep if seccion == "`s'"

        count
        local nobs = r(N)

        local desc "`=desc_seccion[1]'"

        quietly summarize `fac', meanonly
        local trabajadores = r(sum)

        gen ingreso_anual_expandido = INGLABO_ANUAL * `fac'
        quietly summarize ingreso_anual_expandido, meanonly
        local total_inglabo_anual = r(sum)

        local mean_inglabo_anual = `total_inglabo_anual' / `trabajadores'
        local mean_inglabo_mensual = `mean_inglabo_anual' / 12

        sort INGLABO_ANUAL
        gen peso = `fac'
        egen peso_total = total(peso)
        gen peso_acum = sum(peso)
        gen frac_acum = peso_acum / peso_total

        quietly summarize INGLABO_ANUAL if frac_acum >= 0.5, meanonly
        local median_inglabo_anual = r(min)

        local median_inglabo_mensual = `median_inglabo_anual' / 12

        local mean_inglabo_hora = .
        local median_inglabo_hora = .

        quietly count if !missing(INGLABO_HORA)

        if r(N) > 0 {

            gen ingreso_hora_expandido = INGLABO_HORA * `fac' if !missing(INGLABO_HORA)

            quietly summarize ingreso_hora_expandido, meanonly
            local total_inglabo_hora = r(sum)

            quietly summarize `fac' if !missing(INGLABO_HORA), meanonly
            local trabajadores_hora = r(sum)

            local mean_inglabo_hora = `total_inglabo_hora' / `trabajadores_hora'

            sort INGLABO_HORA
            gen peso_hora = `fac' if !missing(INGLABO_HORA)
            egen peso_total_hora = total(peso_hora)
            gen peso_acum_hora = sum(peso_hora) if !missing(INGLABO_HORA)
            gen frac_acum_hora = peso_acum_hora / peso_total_hora

            quietly summarize INGLABO_HORA if frac_acum_hora >= 0.5 & !missing(INGLABO_HORA), meanonly
            local median_inglabo_hora = r(min)
        }

        post `memhold' ///
            ("`s'") ///
            ("`desc'") ///
            (`nobs') ///
            (`trabajadores') ///
            (`total_inglabo_anual') ///
            (`mean_inglabo_anual') ///
            (`mean_inglabo_mensual') ///
            (`median_inglabo_anual') ///
            (`median_inglabo_mensual') ///
            (`mean_inglabo_hora') ///
            (`median_inglabo_hora')

    restore
}

postclose `memhold'


*===================================================
* 10. ORGANIZAR RESULTADOS
*===================================================

use `resultados', clear

sort seccion

order seccion desc_seccion ///
      trabajadores ///
      mean_inglabo_anual mean_inglabo_mensual ///
      median_inglabo_anual median_inglabo_mensual ///
      mean_inglabo_hora median_inglabo_hora ///
      total_inglabo_anual n_obs

label variable seccion "Sección CIIU"
label variable desc_seccion "Descripción de sección"
label variable trabajadores "Trabajadores expandidos"
label variable mean_inglabo_anual "Ingreso laboral anual promedio"
label variable mean_inglabo_mensual "Ingreso laboral mensual promedio"
label variable median_inglabo_anual "Ingreso laboral anual mediano"
label variable median_inglabo_mensual "Ingreso laboral mensual mediano"
label variable mean_inglabo_hora "Ingreso laboral por hora promedio"
label variable median_inglabo_hora "Ingreso laboral por hora mediano"
label variable total_inglabo_anual "Total ingreso laboral anual expandido"
label variable n_obs "Observaciones no expandidas"

format trabajadores %15.0fc
format total_inglabo_anual %18.0fc
format mean_inglabo_anual %15.2fc
format mean_inglabo_mensual %15.2fc
format median_inglabo_anual %15.2fc
format median_inglabo_mensual %15.2fc
format mean_inglabo_hora %15.2fc
format median_inglabo_hora %15.2fc


*===================================================
* 11. EXPORTAR A EXCEL Y GUARDAR DTA
*===================================================

save "Outputs/tables/consolidadosector_`year'.dta", replace

export excel using "`archivo'", ///
    sheet("consolidado_sector_`year'") ///
    firstrow(varlabels) ///
    replace

di "===================================================="
di "Archivo exportado correctamente:"
di "`archivo'"
di "Hoja: consolidado_sector_`year'"
di "Variable rama usada: `ramavar'"
di "Clasificación usada: `ciiu_rev'"
di "===================================================="

****************************************************
* CONSOLIDADO POR SECTOR / SECCIÓN CIIU
* GEIH 2022
* Sin discriminación por tamaño de empresa
* Detecta RAMA2D_R4 o RAMA2D automáticamente
****************************************************

clear all
set more off

cd "C:/Users/jorge/Documents/Databases/GEIH"

local year 2022

capture mkdir "Outputs"
capture mkdir "Outputs/tables"

local archivo "Outputs/tables/consolidadosector_`year'.xlsx"
capture erase "`archivo'"

use "GEIH_`year'_TOTAL.dta", clear


*===================================================
* 0. IDENTIFICAR FACTOR DE EXPANSIÓN
*===================================================

capture confirm variable FEX_C18
if !_rc {
    local fexvar "FEX_C18"
}
else {
    capture confirm variable FEX_C
    if !_rc {
        local fexvar "FEX_C"
    }
    else {
        di as error "No existe FEX_C18 ni FEX_C en la base."
        exit 111
    }
}

di as text "Factor de expansión usado: `fexvar'"


*===================================================
* 1. IDENTIFICAR VARIABLE DE RAMA ECONÓMICA
*===================================================

capture confirm variable RAMA2D_R4
if !_rc {
    local ramavar "RAMA2D_R4"
    local ciiu_rev "rev4"
}
else {
    capture confirm variable RAMA2D
    if !_rc {
        local ramavar "RAMA2D"
        local ciiu_rev "rev3"
    }
    else {
        di as error "No existe RAMA2D_R4 ni RAMA2D en la base."
        exit 111
    }
}

di as text "Variable de rama usada: `ramavar'"
di as text "Clasificación usada: `ciiu_rev'"


*===================================================
* 2. VERIFICAR VARIABLES NECESARIAS
*===================================================

foreach v in OCI `fexvar' INGLABO `ramavar' P6800 {
    capture confirm variable `v'
    if _rc {
        di as error "No existe la variable `v' en la base."
        exit 111
    }
}


*===================================================
* 3. ASEGURAR VARIABLES NUMÉRICAS
*===================================================

foreach v in OCI `fexvar' INGLABO `ramavar' P6800 {
    capture confirm numeric variable `v'
    if _rc {
        destring `v', replace force
    }
}


*===================================================
* 4. SOLO OCUPADOS
*===================================================

keep if OCI == 1


*===================================================
* 5. DEFINIR FACTOR DE EXPANSIÓN
*===================================================
* Como se trabaja con base anual construida desde meses,
* se divide el factor entre 12 para obtener trabajadores
* promedio del año.

gen FEX = `fexvar'/12
local fac "FEX"


*===================================================
* 6. FILTROS BASE
*===================================================

keep if !missing(INGLABO)
keep if INGLABO > 0

keep if !missing(`fac')
keep if `fac' > 0

keep if !missing(`ramavar')


*===================================================
* 7. CREAR SECCIÓN CIIU
*===================================================

gen str1 seccion = ""
gen str120 desc_seccion = ""

if "`ciiu_rev'" == "rev4" {

    *===================================================
    * CIIU Rev. 4
    *===================================================

    replace seccion = "A" if inrange(`ramavar', 1, 3)
    replace desc_seccion = "Agricultura, ganadería, caza, silvicultura y pesca" if seccion == "A"

    replace seccion = "B" if inrange(`ramavar', 5, 9)
    replace desc_seccion = "Explotación de minas y canteras" if seccion == "B"

    replace seccion = "C" if inrange(`ramavar', 10, 33)
    replace desc_seccion = "Industrias manufactureras" if seccion == "C"

    replace seccion = "D" if `ramavar' == 35
    replace desc_seccion = "Suministro de electricidad, gas, vapor y aire acondicionado" if seccion == "D"

    replace seccion = "E" if inrange(`ramavar', 36, 39)
    replace desc_seccion = "Distribución de agua; evacuación y tratamiento de aguas residuales" if seccion == "E"

    replace seccion = "F" if inrange(`ramavar', 41, 43)
    replace desc_seccion = "Construcción" if seccion == "F"

    replace seccion = "G" if inrange(`ramavar', 45, 47)
    replace desc_seccion = "Comercio al por mayor y al por menor; reparación de vehículos" if seccion == "G"

    replace seccion = "H" if inrange(`ramavar', 49, 53)
    replace desc_seccion = "Transporte y almacenamiento" if seccion == "H"

    replace seccion = "I" if inrange(`ramavar', 55, 56)
    replace desc_seccion = "Alojamiento y servicios de comida" if seccion == "I"

    replace seccion = "J" if inrange(`ramavar', 58, 63)
    replace desc_seccion = "Información y comunicaciones" if seccion == "J"

    replace seccion = "K" if inrange(`ramavar', 64, 66)
    replace desc_seccion = "Actividades financieras y de seguros" if seccion == "K"

    replace seccion = "L" if `ramavar' == 68
    replace desc_seccion = "Actividades inmobiliarias" if seccion == "L"

    replace seccion = "M" if inrange(`ramavar', 69, 75)
    replace desc_seccion = "Actividades profesionales, científicas y técnicas" if seccion == "M"

    replace seccion = "N" if inrange(`ramavar', 77, 82)
    replace desc_seccion = "Actividades de servicios administrativos y de apoyo" if seccion == "N"

    replace seccion = "O" if `ramavar' == 84
    replace desc_seccion = "Administración pública y defensa; seguridad social obligatoria" if seccion == "O"

    replace seccion = "P" if `ramavar' == 85
    replace desc_seccion = "Educación" if seccion == "P"

    replace seccion = "Q" if inrange(`ramavar', 86, 88)
    replace desc_seccion = "Actividades de atención de la salud humana y de asistencia social" if seccion == "Q"

    replace seccion = "R" if inrange(`ramavar', 90, 93)
    replace desc_seccion = "Actividades artísticas, de entretenimiento y recreación" if seccion == "R"

    replace seccion = "S" if inrange(`ramavar', 94, 96)
    replace desc_seccion = "Otras actividades de servicios" if seccion == "S"

    replace seccion = "T" if inrange(`ramavar', 97, 98)
    replace desc_seccion = "Actividades de los hogares como empleadores" if seccion == "T"

    replace seccion = "U" if `ramavar' == 99
    replace desc_seccion = "Actividades de organizaciones y entidades extraterritoriales" if seccion == "U"
}

else {

    *===================================================
    * CIIU Rev. 3
    *===================================================

    replace seccion = "A" if inlist(`ramavar', 1, 2)
    replace desc_seccion = "Agricultura, ganadería, caza y silvicultura" if seccion == "A"

    replace seccion = "B" if `ramavar' == 5
    replace desc_seccion = "Pesca" if seccion == "B"

    replace seccion = "C" if inrange(`ramavar', 10, 14)
    replace desc_seccion = "Explotación de minas y canteras" if seccion == "C"

    replace seccion = "D" if inrange(`ramavar', 15, 37)
    replace desc_seccion = "Industrias manufactureras" if seccion == "D"

    replace seccion = "E" if inrange(`ramavar', 40, 41)
    replace desc_seccion = "Suministro de electricidad, gas y agua" if seccion == "E"

    replace seccion = "F" if `ramavar' == 45
    replace desc_seccion = "Construcción" if seccion == "F"

    replace seccion = "G" if inrange(`ramavar', 50, 52)
    replace desc_seccion = "Comercio; reparación de vehículos y efectos personales" if seccion == "G"

    replace seccion = "H" if `ramavar' == 55
    replace desc_seccion = "Hoteles y restaurantes" if seccion == "H"

    replace seccion = "I" if inrange(`ramavar', 60, 64)
    replace desc_seccion = "Transporte, almacenamiento y comunicaciones" if seccion == "I"

    replace seccion = "J" if inrange(`ramavar', 65, 67)
    replace desc_seccion = "Intermediación financiera" if seccion == "J"

    replace seccion = "K" if inrange(`ramavar', 70, 74)
    replace desc_seccion = "Actividades inmobiliarias, empresariales y de alquiler" if seccion == "K"

    replace seccion = "L" if `ramavar' == 75
    replace desc_seccion = "Administración pública y defensa; seguridad social obligatoria" if seccion == "L"

    replace seccion = "M" if `ramavar' == 80
    replace desc_seccion = "Educación" if seccion == "M"

    replace seccion = "N" if `ramavar' == 85
    replace desc_seccion = "Servicios sociales y de salud" if seccion == "N"

    replace seccion = "O" if inrange(`ramavar', 90, 93)
    replace desc_seccion = "Otras actividades de servicios comunitarios, sociales y personales" if seccion == "O"

    replace seccion = "P" if `ramavar' == 95
    replace desc_seccion = "Hogares privados con servicio doméstico" if seccion == "P"

    replace seccion = "Q" if `ramavar' == 99
    replace desc_seccion = "Organizaciones y órganos extraterritoriales" if seccion == "Q"
}

drop if seccion == ""


*===================================================
* 8. INGRESO LABORAL ANUAL E INGRESO POR HORA
*===================================================

gen INGLABO_ANUAL = INGLABO * 12

gen horas_semana = P6800
replace horas_semana = . if missing(horas_semana)
replace horas_semana = . if horas_semana <= 0
replace horas_semana = . if horas_semana > 168

gen INGLABO_HORA = INGLABO_ANUAL / (horas_semana * 52) if !missing(horas_semana)


*===================================================
* 9. CALCULAR RESULTADOS POR SECCIÓN
*===================================================

tempfile resultados
tempname memhold

capture postutil clear

postfile `memhold' ///
    str1 seccion ///
    str120 desc_seccion ///
    double n_obs ///
    double trabajadores ///
    double total_inglabo_anual ///
    double mean_inglabo_anual ///
    double mean_inglabo_mensual ///
    double median_inglabo_anual ///
    double median_inglabo_mensual ///
    double mean_inglabo_hora ///
    double median_inglabo_hora ///
    using `resultados', replace


levelsof seccion, local(lista_sec)

foreach s of local lista_sec {

    preserve

        keep if seccion == "`s'"

        count
        local nobs = r(N)

        local desc "`=desc_seccion[1]'"

        * Trabajadores expandidos
        quietly summarize `fac', meanonly
        local trabajadores = r(sum)

        * Total ingreso laboral anual expandido
        gen ingreso_anual_expandido = INGLABO_ANUAL * `fac'
        quietly summarize ingreso_anual_expandido, meanonly
        local total_inglabo_anual = r(sum)

        * Promedio ponderado ingreso anual
        local mean_inglabo_anual = `total_inglabo_anual' / `trabajadores'

        * Promedio mensual equivalente
        local mean_inglabo_mensual = `mean_inglabo_anual' / 12

        * Mediana ponderada ingreso anual
        sort INGLABO_ANUAL
        gen peso = `fac'
        egen peso_total = total(peso)
        gen peso_acum = sum(peso)
        gen frac_acum = peso_acum / peso_total

        quietly summarize INGLABO_ANUAL if frac_acum >= 0.5, meanonly
        local median_inglabo_anual = r(min)

        * Mediana mensual equivalente
        local median_inglabo_mensual = `median_inglabo_anual' / 12

        * Promedio y mediana ingreso por hora
        local mean_inglabo_hora = .
        local median_inglabo_hora = .

        quietly count if !missing(INGLABO_HORA)

        if r(N) > 0 {

            gen ingreso_hora_expandido = INGLABO_HORA * `fac' if !missing(INGLABO_HORA)

            quietly summarize ingreso_hora_expandido, meanonly
            local total_inglabo_hora = r(sum)

            quietly summarize `fac' if !missing(INGLABO_HORA), meanonly
            local trabajadores_hora = r(sum)

            local mean_inglabo_hora = `total_inglabo_hora' / `trabajadores_hora'

            sort INGLABO_HORA
            gen peso_hora = `fac' if !missing(INGLABO_HORA)
            egen peso_total_hora = total(peso_hora)
            gen peso_acum_hora = sum(peso_hora) if !missing(INGLABO_HORA)
            gen frac_acum_hora = peso_acum_hora / peso_total_hora

            quietly summarize INGLABO_HORA if frac_acum_hora >= 0.5 & !missing(INGLABO_HORA), meanonly
            local median_inglabo_hora = r(min)
        }

        post `memhold' ///
            ("`s'") ///
            ("`desc'") ///
            (`nobs') ///
            (`trabajadores') ///
            (`total_inglabo_anual') ///
            (`mean_inglabo_anual') ///
            (`mean_inglabo_mensual') ///
            (`median_inglabo_anual') ///
            (`median_inglabo_mensual') ///
            (`mean_inglabo_hora') ///
            (`median_inglabo_hora')

    restore
}

postclose `memhold'


*===================================================
* 10. ORGANIZAR RESULTADOS
*===================================================

use `resultados', clear

sort seccion

order seccion desc_seccion ///
      trabajadores ///
      mean_inglabo_anual mean_inglabo_mensual ///
      median_inglabo_anual median_inglabo_mensual ///
      mean_inglabo_hora median_inglabo_hora ///
      total_inglabo_anual n_obs

label variable seccion "Sección CIIU"
label variable desc_seccion "Descripción de sección"
label variable trabajadores "Trabajadores expandidos"
label variable mean_inglabo_anual "Ingreso laboral anual promedio"
label variable mean_inglabo_mensual "Ingreso laboral mensual promedio"
label variable median_inglabo_anual "Ingreso laboral anual mediano"
label variable median_inglabo_mensual "Ingreso laboral mensual mediano"
label variable mean_inglabo_hora "Ingreso laboral por hora promedio"
label variable median_inglabo_hora "Ingreso laboral por hora mediano"
label variable total_inglabo_anual "Total ingreso laboral anual expandido"
label variable n_obs "Observaciones no expandidas"

format trabajadores %15.0fc
format total_inglabo_anual %18.0fc
format mean_inglabo_anual %15.2fc
format mean_inglabo_mensual %15.2fc
format median_inglabo_anual %15.2fc
format median_inglabo_mensual %15.2fc
format mean_inglabo_hora %15.2fc
format median_inglabo_hora %15.2fc


*===================================================
* 11. EXPORTAR A EXCEL Y GUARDAR DTA
*===================================================

save "Outputs/tables/consolidadosector_`year'.dta", replace

export excel using "`archivo'", ///
    sheet("consolidado_sector_`year'") ///
    firstrow(varlabels) ///
    replace

di "===================================================="
di "Archivo exportado correctamente:"
di "`archivo'"
di "Hoja: consolidado_sector_`year'"
di "Variable rama usada: `ramavar'"
di "Clasificación usada: `ciiu_rev'"
di "===================================================="

****************************************************
* CONSOLIDADO POR SECTOR / SECCIÓN CIIU
* GEIH 2023
* Sin discriminación por tamaño de empresa
* Detecta RAMA2D_R4 o RAMA2D automáticamente
****************************************************

clear all
set more off

cd "C:/Users/jorge/Documents/Databases/GEIH"

local year 2023

capture mkdir "Outputs"
capture mkdir "Outputs/tables"

local archivo "Outputs/tables/consolidadosector_`year'.xlsx"
capture erase "`archivo'"

use "GEIH_`year'_TOTAL.dta", clear


*===================================================
* 0. IDENTIFICAR FACTOR DE EXPANSIÓN
*===================================================

capture confirm variable FEX_C18
if !_rc {
    local fexvar "FEX_C18"
}
else {
    capture confirm variable FEX_C
    if !_rc {
        local fexvar "FEX_C"
    }
    else {
        di as error "No existe FEX_C18 ni FEX_C en la base."
        exit 111
    }
}

di as text "Factor de expansión usado: `fexvar'"


*===================================================
* 1. IDENTIFICAR VARIABLE DE RAMA ECONÓMICA
*===================================================

capture confirm variable RAMA2D_R4
if !_rc {
    local ramavar "RAMA2D_R4"
    local ciiu_rev "rev4"
}
else {
    capture confirm variable RAMA2D
    if !_rc {
        local ramavar "RAMA2D"
        local ciiu_rev "rev3"
    }
    else {
        di as error "No existe RAMA2D_R4 ni RAMA2D en la base."
        exit 111
    }
}

di as text "Variable de rama usada: `ramavar'"
di as text "Clasificación usada: `ciiu_rev'"


*===================================================
* 2. VERIFICAR VARIABLES NECESARIAS
*===================================================

foreach v in OCI `fexvar' INGLABO `ramavar' P6800 {
    capture confirm variable `v'
    if _rc {
        di as error "No existe la variable `v' en la base."
        exit 111
    }
}


*===================================================
* 3. ASEGURAR VARIABLES NUMÉRICAS
*===================================================

foreach v in OCI `fexvar' INGLABO `ramavar' P6800 {
    capture confirm numeric variable `v'
    if _rc {
        destring `v', replace force
    }
}


*===================================================
* 4. SOLO OCUPADOS
*===================================================

keep if OCI == 1


*===================================================
* 5. DEFINIR FACTOR DE EXPANSIÓN
*===================================================
* Como se trabaja con base anual construida desde meses,
* se divide el factor entre 12 para obtener trabajadores
* promedio del año.

gen FEX = `fexvar'/12
local fac "FEX"


*===================================================
* 6. FILTROS BASE
*===================================================

keep if !missing(INGLABO)
keep if INGLABO > 0

keep if !missing(`fac')
keep if `fac' > 0

keep if !missing(`ramavar')


*===================================================
* 7. CREAR SECCIÓN CIIU
*===================================================

gen str1 seccion = ""
gen str120 desc_seccion = ""

if "`ciiu_rev'" == "rev4" {

    *===================================================
    * CIIU Rev. 4
    *===================================================

    replace seccion = "A" if inrange(`ramavar', 1, 3)
    replace desc_seccion = "Agricultura, ganadería, caza, silvicultura y pesca" if seccion == "A"

    replace seccion = "B" if inrange(`ramavar', 5, 9)
    replace desc_seccion = "Explotación de minas y canteras" if seccion == "B"

    replace seccion = "C" if inrange(`ramavar', 10, 33)
    replace desc_seccion = "Industrias manufactureras" if seccion == "C"

    replace seccion = "D" if `ramavar' == 35
    replace desc_seccion = "Suministro de electricidad, gas, vapor y aire acondicionado" if seccion == "D"

    replace seccion = "E" if inrange(`ramavar', 36, 39)
    replace desc_seccion = "Distribución de agua; evacuación y tratamiento de aguas residuales" if seccion == "E"

    replace seccion = "F" if inrange(`ramavar', 41, 43)
    replace desc_seccion = "Construcción" if seccion == "F"

    replace seccion = "G" if inrange(`ramavar', 45, 47)
    replace desc_seccion = "Comercio al por mayor y al por menor; reparación de vehículos" if seccion == "G"

    replace seccion = "H" if inrange(`ramavar', 49, 53)
    replace desc_seccion = "Transporte y almacenamiento" if seccion == "H"

    replace seccion = "I" if inrange(`ramavar', 55, 56)
    replace desc_seccion = "Alojamiento y servicios de comida" if seccion == "I"

    replace seccion = "J" if inrange(`ramavar', 58, 63)
    replace desc_seccion = "Información y comunicaciones" if seccion == "J"

    replace seccion = "K" if inrange(`ramavar', 64, 66)
    replace desc_seccion = "Actividades financieras y de seguros" if seccion == "K"

    replace seccion = "L" if `ramavar' == 68
    replace desc_seccion = "Actividades inmobiliarias" if seccion == "L"

    replace seccion = "M" if inrange(`ramavar', 69, 75)
    replace desc_seccion = "Actividades profesionales, científicas y técnicas" if seccion == "M"

    replace seccion = "N" if inrange(`ramavar', 77, 82)
    replace desc_seccion = "Actividades de servicios administrativos y de apoyo" if seccion == "N"

    replace seccion = "O" if `ramavar' == 84
    replace desc_seccion = "Administración pública y defensa; seguridad social obligatoria" if seccion == "O"

    replace seccion = "P" if `ramavar' == 85
    replace desc_seccion = "Educación" if seccion == "P"

    replace seccion = "Q" if inrange(`ramavar', 86, 88)
    replace desc_seccion = "Actividades de atención de la salud humana y de asistencia social" if seccion == "Q"

    replace seccion = "R" if inrange(`ramavar', 90, 93)
    replace desc_seccion = "Actividades artísticas, de entretenimiento y recreación" if seccion == "R"

    replace seccion = "S" if inrange(`ramavar', 94, 96)
    replace desc_seccion = "Otras actividades de servicios" if seccion == "S"

    replace seccion = "T" if inrange(`ramavar', 97, 98)
    replace desc_seccion = "Actividades de los hogares como empleadores" if seccion == "T"

    replace seccion = "U" if `ramavar' == 99
    replace desc_seccion = "Actividades de organizaciones y entidades extraterritoriales" if seccion == "U"
}

else {

    *===================================================
    * CIIU Rev. 3
    *===================================================

    replace seccion = "A" if inlist(`ramavar', 1, 2)
    replace desc_seccion = "Agricultura, ganadería, caza y silvicultura" if seccion == "A"

    replace seccion = "B" if `ramavar' == 5
    replace desc_seccion = "Pesca" if seccion == "B"

    replace seccion = "C" if inrange(`ramavar', 10, 14)
    replace desc_seccion = "Explotación de minas y canteras" if seccion == "C"

    replace seccion = "D" if inrange(`ramavar', 15, 37)
    replace desc_seccion = "Industrias manufactureras" if seccion == "D"

    replace seccion = "E" if inrange(`ramavar', 40, 41)
    replace desc_seccion = "Suministro de electricidad, gas y agua" if seccion == "E"

    replace seccion = "F" if `ramavar' == 45
    replace desc_seccion = "Construcción" if seccion == "F"

    replace seccion = "G" if inrange(`ramavar', 50, 52)
    replace desc_seccion = "Comercio; reparación de vehículos y efectos personales" if seccion == "G"

    replace seccion = "H" if `ramavar' == 55
    replace desc_seccion = "Hoteles y restaurantes" if seccion == "H"

    replace seccion = "I" if inrange(`ramavar', 60, 64)
    replace desc_seccion = "Transporte, almacenamiento y comunicaciones" if seccion == "I"

    replace seccion = "J" if inrange(`ramavar', 65, 67)
    replace desc_seccion = "Intermediación financiera" if seccion == "J"

    replace seccion = "K" if inrange(`ramavar', 70, 74)
    replace desc_seccion = "Actividades inmobiliarias, empresariales y de alquiler" if seccion == "K"

    replace seccion = "L" if `ramavar' == 75
    replace desc_seccion = "Administración pública y defensa; seguridad social obligatoria" if seccion == "L"

    replace seccion = "M" if `ramavar' == 80
    replace desc_seccion = "Educación" if seccion == "M"

    replace seccion = "N" if `ramavar' == 85
    replace desc_seccion = "Servicios sociales y de salud" if seccion == "N"

    replace seccion = "O" if inrange(`ramavar', 90, 93)
    replace desc_seccion = "Otras actividades de servicios comunitarios, sociales y personales" if seccion == "O"

    replace seccion = "P" if `ramavar' == 95
    replace desc_seccion = "Hogares privados con servicio doméstico" if seccion == "P"

    replace seccion = "Q" if `ramavar' == 99
    replace desc_seccion = "Organizaciones y órganos extraterritoriales" if seccion == "Q"
}

drop if seccion == ""


*===================================================
* 8. INGRESO LABORAL ANUAL E INGRESO POR HORA
*===================================================

gen INGLABO_ANUAL = INGLABO * 12

gen horas_semana = P6800
replace horas_semana = . if missing(horas_semana)
replace horas_semana = . if horas_semana <= 0
replace horas_semana = . if horas_semana > 168

gen INGLABO_HORA = INGLABO_ANUAL / (horas_semana * 52) if !missing(horas_semana)


*===================================================
* 9. CALCULAR RESULTADOS POR SECCIÓN
*===================================================

tempfile resultados
tempname memhold

capture postutil clear

postfile `memhold' ///
    str1 seccion ///
    str120 desc_seccion ///
    double n_obs ///
    double trabajadores ///
    double total_inglabo_anual ///
    double mean_inglabo_anual ///
    double mean_inglabo_mensual ///
    double median_inglabo_anual ///
    double median_inglabo_mensual ///
    double mean_inglabo_hora ///
    double median_inglabo_hora ///
    using `resultados', replace


levelsof seccion, local(lista_sec)

foreach s of local lista_sec {

    preserve

        keep if seccion == "`s'"

        count
        local nobs = r(N)

        local desc "`=desc_seccion[1]'"

        * Trabajadores expandidos
        quietly summarize `fac', meanonly
        local trabajadores = r(sum)

        * Total ingreso laboral anual expandido
        gen ingreso_anual_expandido = INGLABO_ANUAL * `fac'
        quietly summarize ingreso_anual_expandido, meanonly
        local total_inglabo_anual = r(sum)

        * Promedio ponderado ingreso anual
        local mean_inglabo_anual = `total_inglabo_anual' / `trabajadores'

        * Promedio mensual equivalente
        local mean_inglabo_mensual = `mean_inglabo_anual' / 12

        * Mediana ponderada ingreso anual
        sort INGLABO_ANUAL
        gen peso = `fac'
        egen peso_total = total(peso)
        gen peso_acum = sum(peso)
        gen frac_acum = peso_acum / peso_total

        quietly summarize INGLABO_ANUAL if frac_acum >= 0.5, meanonly
        local median_inglabo_anual = r(min)

        * Mediana mensual equivalente
        local median_inglabo_mensual = `median_inglabo_anual' / 12

        * Promedio y mediana ingreso por hora
        local mean_inglabo_hora = .
        local median_inglabo_hora = .

        quietly count if !missing(INGLABO_HORA)

        if r(N) > 0 {

            gen ingreso_hora_expandido = INGLABO_HORA * `fac' if !missing(INGLABO_HORA)

            quietly summarize ingreso_hora_expandido, meanonly
            local total_inglabo_hora = r(sum)

            quietly summarize `fac' if !missing(INGLABO_HORA), meanonly
            local trabajadores_hora = r(sum)

            local mean_inglabo_hora = `total_inglabo_hora' / `trabajadores_hora'

            sort INGLABO_HORA
            gen peso_hora = `fac' if !missing(INGLABO_HORA)
            egen peso_total_hora = total(peso_hora)
            gen peso_acum_hora = sum(peso_hora) if !missing(INGLABO_HORA)
            gen frac_acum_hora = peso_acum_hora / peso_total_hora

            quietly summarize INGLABO_HORA if frac_acum_hora >= 0.5 & !missing(INGLABO_HORA), meanonly
            local median_inglabo_hora = r(min)
        }

        post `memhold' ///
            ("`s'") ///
            ("`desc'") ///
            (`nobs') ///
            (`trabajadores') ///
            (`total_inglabo_anual') ///
            (`mean_inglabo_anual') ///
            (`mean_inglabo_mensual') ///
            (`median_inglabo_anual') ///
            (`median_inglabo_mensual') ///
            (`mean_inglabo_hora') ///
            (`median_inglabo_hora')

    restore
}

postclose `memhold'


*===================================================
* 10. ORGANIZAR RESULTADOS
*===================================================

use `resultados', clear

sort seccion

order seccion desc_seccion ///
      trabajadores ///
      mean_inglabo_anual mean_inglabo_mensual ///
      median_inglabo_anual median_inglabo_mensual ///
      mean_inglabo_hora median_inglabo_hora ///
      total_inglabo_anual n_obs

label variable seccion "Sección CIIU"
label variable desc_seccion "Descripción de sección"
label variable trabajadores "Trabajadores expandidos"
label variable mean_inglabo_anual "Ingreso laboral anual promedio"
label variable mean_inglabo_mensual "Ingreso laboral mensual promedio"
label variable median_inglabo_anual "Ingreso laboral anual mediano"
label variable median_inglabo_mensual "Ingreso laboral mensual mediano"
label variable mean_inglabo_hora "Ingreso laboral por hora promedio"
label variable median_inglabo_hora "Ingreso laboral por hora mediano"
label variable total_inglabo_anual "Total ingreso laboral anual expandido"
label variable n_obs "Observaciones no expandidas"

format trabajadores %15.0fc
format total_inglabo_anual %18.0fc
format mean_inglabo_anual %15.2fc
format mean_inglabo_mensual %15.2fc
format median_inglabo_anual %15.2fc
format median_inglabo_mensual %15.2fc
format mean_inglabo_hora %15.2fc
format median_inglabo_hora %15.2fc


*===================================================
* 11. EXPORTAR A EXCEL Y GUARDAR DTA
*===================================================

save "Outputs/tables/consolidadosector_`year'.dta", replace

export excel using "`archivo'", ///
    sheet("consolidado_sector_`year'") ///
    firstrow(varlabels) ///
    replace

di "===================================================="
di "Archivo exportado correctamente:"
di "`archivo'"
di "Hoja: consolidado_sector_`year'"
di "Variable rama usada: `ramavar'"
di "Clasificación usada: `ciiu_rev'"
di "===================================================="

****************************************************
* CONSOLIDADO POR SECTOR / SECCIÓN CIIU
* GEIH 2024
* Sin discriminación por tamaño de empresa
* Detecta RAMA2D_R4 o RAMA2D automáticamente
****************************************************

clear all
set more off

cd "C:/Users/jorge/Documents/Databases/GEIH"

local year 2024

capture mkdir "Outputs"
capture mkdir "Outputs/tables"

local archivo "Outputs/tables/consolidadosector_`year'.xlsx"
capture erase "`archivo'"

use "GEIH_`year'_TOTAL.dta", clear


*===================================================
* 0. IDENTIFICAR FACTOR DE EXPANSIÓN
*===================================================

capture confirm variable FEX_C18
if !_rc {
    local fexvar "FEX_C18"
}
else {
    capture confirm variable FEX_C
    if !_rc {
        local fexvar "FEX_C"
    }
    else {
        di as error "No existe FEX_C18 ni FEX_C en la base."
        exit 111
    }
}

di as text "Factor de expansión usado: `fexvar'"


*===================================================
* 1. IDENTIFICAR VARIABLE DE RAMA ECONÓMICA
*===================================================

capture confirm variable RAMA2D_R4
if !_rc {
    local ramavar "RAMA2D_R4"
    local ciiu_rev "rev4"
}
else {
    capture confirm variable RAMA2D
    if !_rc {
        local ramavar "RAMA2D"
        local ciiu_rev "rev3"
    }
    else {
        di as error "No existe RAMA2D_R4 ni RAMA2D en la base."
        exit 111
    }
}

di as text "Variable de rama usada: `ramavar'"
di as text "Clasificación usada: `ciiu_rev'"


*===================================================
* 2. VERIFICAR VARIABLES NECESARIAS
*===================================================

foreach v in OCI `fexvar' INGLABO `ramavar' P6800 {
    capture confirm variable `v'
    if _rc {
        di as error "No existe la variable `v' en la base."
        exit 111
    }
}


*===================================================
* 3. ASEGURAR VARIABLES NUMÉRICAS
*===================================================

foreach v in OCI `fexvar' INGLABO `ramavar' P6800 {
    capture confirm numeric variable `v'
    if _rc {
        destring `v', replace force
    }
}


*===================================================
* 4. SOLO OCUPADOS
*===================================================

keep if OCI == 1


*===================================================
* 5. DEFINIR FACTOR DE EXPANSIÓN
*===================================================
* Como se trabaja con base anual construida desde meses,
* se divide el factor entre 12 para obtener trabajadores
* promedio del año.

gen FEX = `fexvar'/12
local fac "FEX"


*===================================================
* 6. FILTROS BASE
*===================================================

keep if !missing(INGLABO)
keep if INGLABO > 0

keep if !missing(`fac')
keep if `fac' > 0

keep if !missing(`ramavar')


*===================================================
* 7. CREAR SECCIÓN CIIU
*===================================================

gen str1 seccion = ""
gen str120 desc_seccion = ""

if "`ciiu_rev'" == "rev4" {

    *===================================================
    * CIIU Rev. 4
    *===================================================

    replace seccion = "A" if inrange(`ramavar', 1, 3)
    replace desc_seccion = "Agricultura, ganadería, caza, silvicultura y pesca" if seccion == "A"

    replace seccion = "B" if inrange(`ramavar', 5, 9)
    replace desc_seccion = "Explotación de minas y canteras" if seccion == "B"

    replace seccion = "C" if inrange(`ramavar', 10, 33)
    replace desc_seccion = "Industrias manufactureras" if seccion == "C"

    replace seccion = "D" if `ramavar' == 35
    replace desc_seccion = "Suministro de electricidad, gas, vapor y aire acondicionado" if seccion == "D"

    replace seccion = "E" if inrange(`ramavar', 36, 39)
    replace desc_seccion = "Distribución de agua; evacuación y tratamiento de aguas residuales" if seccion == "E"

    replace seccion = "F" if inrange(`ramavar', 41, 43)
    replace desc_seccion = "Construcción" if seccion == "F"

    replace seccion = "G" if inrange(`ramavar', 45, 47)
    replace desc_seccion = "Comercio al por mayor y al por menor; reparación de vehículos" if seccion == "G"

    replace seccion = "H" if inrange(`ramavar', 49, 53)
    replace desc_seccion = "Transporte y almacenamiento" if seccion == "H"

    replace seccion = "I" if inrange(`ramavar', 55, 56)
    replace desc_seccion = "Alojamiento y servicios de comida" if seccion == "I"

    replace seccion = "J" if inrange(`ramavar', 58, 63)
    replace desc_seccion = "Información y comunicaciones" if seccion == "J"

    replace seccion = "K" if inrange(`ramavar', 64, 66)
    replace desc_seccion = "Actividades financieras y de seguros" if seccion == "K"

    replace seccion = "L" if `ramavar' == 68
    replace desc_seccion = "Actividades inmobiliarias" if seccion == "L"

    replace seccion = "M" if inrange(`ramavar', 69, 75)
    replace desc_seccion = "Actividades profesionales, científicas y técnicas" if seccion == "M"

    replace seccion = "N" if inrange(`ramavar', 77, 82)
    replace desc_seccion = "Actividades de servicios administrativos y de apoyo" if seccion == "N"

    replace seccion = "O" if `ramavar' == 84
    replace desc_seccion = "Administración pública y defensa; seguridad social obligatoria" if seccion == "O"

    replace seccion = "P" if `ramavar' == 85
    replace desc_seccion = "Educación" if seccion == "P"

    replace seccion = "Q" if inrange(`ramavar', 86, 88)
    replace desc_seccion = "Actividades de atención de la salud humana y de asistencia social" if seccion == "Q"

    replace seccion = "R" if inrange(`ramavar', 90, 93)
    replace desc_seccion = "Actividades artísticas, de entretenimiento y recreación" if seccion == "R"

    replace seccion = "S" if inrange(`ramavar', 94, 96)
    replace desc_seccion = "Otras actividades de servicios" if seccion == "S"

    replace seccion = "T" if inrange(`ramavar', 97, 98)
    replace desc_seccion = "Actividades de los hogares como empleadores" if seccion == "T"

    replace seccion = "U" if `ramavar' == 99
    replace desc_seccion = "Actividades de organizaciones y entidades extraterritoriales" if seccion == "U"
}

else {

    *===================================================
    * CIIU Rev. 3
    *===================================================

    replace seccion = "A" if inlist(`ramavar', 1, 2)
    replace desc_seccion = "Agricultura, ganadería, caza y silvicultura" if seccion == "A"

    replace seccion = "B" if `ramavar' == 5
    replace desc_seccion = "Pesca" if seccion == "B"

    replace seccion = "C" if inrange(`ramavar', 10, 14)
    replace desc_seccion = "Explotación de minas y canteras" if seccion == "C"

    replace seccion = "D" if inrange(`ramavar', 15, 37)
    replace desc_seccion = "Industrias manufactureras" if seccion == "D"

    replace seccion = "E" if inrange(`ramavar', 40, 41)
    replace desc_seccion = "Suministro de electricidad, gas y agua" if seccion == "E"

    replace seccion = "F" if `ramavar' == 45
    replace desc_seccion = "Construcción" if seccion == "F"

    replace seccion = "G" if inrange(`ramavar', 50, 52)
    replace desc_seccion = "Comercio; reparación de vehículos y efectos personales" if seccion == "G"

    replace seccion = "H" if `ramavar' == 55
    replace desc_seccion = "Hoteles y restaurantes" if seccion == "H"

    replace seccion = "I" if inrange(`ramavar', 60, 64)
    replace desc_seccion = "Transporte, almacenamiento y comunicaciones" if seccion == "I"

    replace seccion = "J" if inrange(`ramavar', 65, 67)
    replace desc_seccion = "Intermediación financiera" if seccion == "J"

    replace seccion = "K" if inrange(`ramavar', 70, 74)
    replace desc_seccion = "Actividades inmobiliarias, empresariales y de alquiler" if seccion == "K"

    replace seccion = "L" if `ramavar' == 75
    replace desc_seccion = "Administración pública y defensa; seguridad social obligatoria" if seccion == "L"

    replace seccion = "M" if `ramavar' == 80
    replace desc_seccion = "Educación" if seccion == "M"

    replace seccion = "N" if `ramavar' == 85
    replace desc_seccion = "Servicios sociales y de salud" if seccion == "N"

    replace seccion = "O" if inrange(`ramavar', 90, 93)
    replace desc_seccion = "Otras actividades de servicios comunitarios, sociales y personales" if seccion == "O"

    replace seccion = "P" if `ramavar' == 95
    replace desc_seccion = "Hogares privados con servicio doméstico" if seccion == "P"

    replace seccion = "Q" if `ramavar' == 99
    replace desc_seccion = "Organizaciones y órganos extraterritoriales" if seccion == "Q"
}

drop if seccion == ""


*===================================================
* 8. INGRESO LABORAL ANUAL E INGRESO POR HORA
*===================================================

gen INGLABO_ANUAL = INGLABO * 12

gen horas_semana = P6800
replace horas_semana = . if missing(horas_semana)
replace horas_semana = . if horas_semana <= 0
replace horas_semana = . if horas_semana > 168

gen INGLABO_HORA = INGLABO_ANUAL / (horas_semana * 52) if !missing(horas_semana)


*===================================================
* 9. CALCULAR RESULTADOS POR SECCIÓN
*===================================================

tempfile resultados
tempname memhold

capture postutil clear

postfile `memhold' ///
    str1 seccion ///
    str120 desc_seccion ///
    double n_obs ///
    double trabajadores ///
    double total_inglabo_anual ///
    double mean_inglabo_anual ///
    double mean_inglabo_mensual ///
    double median_inglabo_anual ///
    double median_inglabo_mensual ///
    double mean_inglabo_hora ///
    double median_inglabo_hora ///
    using `resultados', replace


levelsof seccion, local(lista_sec)

foreach s of local lista_sec {

    preserve

        keep if seccion == "`s'"

        count
        local nobs = r(N)

        local desc "`=desc_seccion[1]'"

        * Trabajadores expandidos
        quietly summarize `fac', meanonly
        local trabajadores = r(sum)

        * Total ingreso laboral anual expandido
        gen ingreso_anual_expandido = INGLABO_ANUAL * `fac'
        quietly summarize ingreso_anual_expandido, meanonly
        local total_inglabo_anual = r(sum)

        * Promedio ponderado ingreso anual
        local mean_inglabo_anual = `total_inglabo_anual' / `trabajadores'

        * Promedio mensual equivalente
        local mean_inglabo_mensual = `mean_inglabo_anual' / 12

        * Mediana ponderada ingreso anual
        sort INGLABO_ANUAL
        gen peso = `fac'
        egen peso_total = total(peso)
        gen peso_acum = sum(peso)
        gen frac_acum = peso_acum / peso_total

        quietly summarize INGLABO_ANUAL if frac_acum >= 0.5, meanonly
        local median_inglabo_anual = r(min)

        * Mediana mensual equivalente
        local median_inglabo_mensual = `median_inglabo_anual' / 12

        * Promedio y mediana ingreso por hora
        local mean_inglabo_hora = .
        local median_inglabo_hora = .

        quietly count if !missing(INGLABO_HORA)

        if r(N) > 0 {

            gen ingreso_hora_expandido = INGLABO_HORA * `fac' if !missing(INGLABO_HORA)

            quietly summarize ingreso_hora_expandido, meanonly
            local total_inglabo_hora = r(sum)

            quietly summarize `fac' if !missing(INGLABO_HORA), meanonly
            local trabajadores_hora = r(sum)

            local mean_inglabo_hora = `total_inglabo_hora' / `trabajadores_hora'

            sort INGLABO_HORA
            gen peso_hora = `fac' if !missing(INGLABO_HORA)
            egen peso_total_hora = total(peso_hora)
            gen peso_acum_hora = sum(peso_hora) if !missing(INGLABO_HORA)
            gen frac_acum_hora = peso_acum_hora / peso_total_hora

            quietly summarize INGLABO_HORA if frac_acum_hora >= 0.5 & !missing(INGLABO_HORA), meanonly
            local median_inglabo_hora = r(min)
        }

        post `memhold' ///
            ("`s'") ///
            ("`desc'") ///
            (`nobs') ///
            (`trabajadores') ///
            (`total_inglabo_anual') ///
            (`mean_inglabo_anual') ///
            (`mean_inglabo_mensual') ///
            (`median_inglabo_anual') ///
            (`median_inglabo_mensual') ///
            (`mean_inglabo_hora') ///
            (`median_inglabo_hora')

    restore
}

postclose `memhold'


*===================================================
* 10. ORGANIZAR RESULTADOS
*===================================================

use `resultados', clear

sort seccion

order seccion desc_seccion ///
      trabajadores ///
      mean_inglabo_anual mean_inglabo_mensual ///
      median_inglabo_anual median_inglabo_mensual ///
      mean_inglabo_hora median_inglabo_hora ///
      total_inglabo_anual n_obs

label variable seccion "Sección CIIU"
label variable desc_seccion "Descripción de sección"
label variable trabajadores "Trabajadores expandidos"
label variable mean_inglabo_anual "Ingreso laboral anual promedio"
label variable mean_inglabo_mensual "Ingreso laboral mensual promedio"
label variable median_inglabo_anual "Ingreso laboral anual mediano"
label variable median_inglabo_mensual "Ingreso laboral mensual mediano"
label variable mean_inglabo_hora "Ingreso laboral por hora promedio"
label variable median_inglabo_hora "Ingreso laboral por hora mediano"
label variable total_inglabo_anual "Total ingreso laboral anual expandido"
label variable n_obs "Observaciones no expandidas"

format trabajadores %15.0fc
format total_inglabo_anual %18.0fc
format mean_inglabo_anual %15.2fc
format mean_inglabo_mensual %15.2fc
format median_inglabo_anual %15.2fc
format median_inglabo_mensual %15.2fc
format mean_inglabo_hora %15.2fc
format median_inglabo_hora %15.2fc


*===================================================
* 11. EXPORTAR A EXCEL Y GUARDAR DTA
*===================================================

save "Outputs/tables/consolidadosector_`year'.dta", replace

export excel using "`archivo'", ///
    sheet("consolidado_sector_`year'") ///
    firstrow(varlabels) ///
    replace

di "===================================================="
di "Archivo exportado correctamente:"
di "`archivo'"
di "Hoja: consolidado_sector_`year'"
di "Variable rama usada: `ramavar'"
di "Clasificación usada: `ciiu_rev'"
di "===================================================="

****************************************************
* CONSOLIDADO POR SECTOR / SECCIÓN CIIU
* GEIH 2025
* Sin discriminación por tamaño de empresa
* Detecta RAMA2D_R4 o RAMA2D automáticamente
****************************************************

clear all
set more off

cd "C:/Users/jorge/Documents/Databases/GEIH"

local year 2025

capture mkdir "Outputs"
capture mkdir "Outputs/tables"

local archivo "Outputs/tables/consolidadosector_`year'.xlsx"
capture erase "`archivo'"

use "GEIH_`year'_TOTAL.dta", clear


*===================================================
* 0. IDENTIFICAR FACTOR DE EXPANSIÓN
*===================================================

capture confirm variable FEX_C18
if !_rc {
    local fexvar "FEX_C18"
}
else {
    capture confirm variable FEX_C
    if !_rc {
        local fexvar "FEX_C"
    }
    else {
        di as error "No existe FEX_C18 ni FEX_C en la base."
        exit 111
    }
}

di as text "Factor de expansión usado: `fexvar'"


*===================================================
* 1. IDENTIFICAR VARIABLE DE RAMA ECONÓMICA
*===================================================

capture confirm variable RAMA2D_R4
if !_rc {
    local ramavar "RAMA2D_R4"
    local ciiu_rev "rev4"
}
else {
    capture confirm variable RAMA2D
    if !_rc {
        local ramavar "RAMA2D"
        local ciiu_rev "rev3"
    }
    else {
        di as error "No existe RAMA2D_R4 ni RAMA2D en la base."
        exit 111
    }
}

di as text "Variable de rama usada: `ramavar'"
di as text "Clasificación usada: `ciiu_rev'"


*===================================================
* 2. VERIFICAR VARIABLES NECESARIAS
*===================================================

foreach v in OCI `fexvar' INGLABO `ramavar' P6800 {
    capture confirm variable `v'
    if _rc {
        di as error "No existe la variable `v' en la base."
        exit 111
    }
}


*===================================================
* 3. ASEGURAR VARIABLES NUMÉRICAS
*===================================================

foreach v in OCI `fexvar' INGLABO `ramavar' P6800 {
    capture confirm numeric variable `v'
    if _rc {
        destring `v', replace force
    }
}


*===================================================
* 4. SOLO OCUPADOS
*===================================================

keep if OCI == 1


*===================================================
* 5. DEFINIR FACTOR DE EXPANSIÓN
*===================================================
* Como se trabaja con base anual construida desde meses,
* se divide el factor entre 12 para obtener trabajadores
* promedio del año.

gen FEX = `fexvar'/12
local fac "FEX"


*===================================================
* 6. FILTROS BASE
*===================================================

keep if !missing(INGLABO)
keep if INGLABO > 0

keep if !missing(`fac')
keep if `fac' > 0

keep if !missing(`ramavar')


*===================================================
* 7. CREAR SECCIÓN CIIU
*===================================================

gen str1 seccion = ""
gen str120 desc_seccion = ""

if "`ciiu_rev'" == "rev4" {

    *===================================================
    * CIIU Rev. 4
    *===================================================

    replace seccion = "A" if inrange(`ramavar', 1, 3)
    replace desc_seccion = "Agricultura, ganadería, caza, silvicultura y pesca" if seccion == "A"

    replace seccion = "B" if inrange(`ramavar', 5, 9)
    replace desc_seccion = "Explotación de minas y canteras" if seccion == "B"

    replace seccion = "C" if inrange(`ramavar', 10, 33)
    replace desc_seccion = "Industrias manufactureras" if seccion == "C"

    replace seccion = "D" if `ramavar' == 35
    replace desc_seccion = "Suministro de electricidad, gas, vapor y aire acondicionado" if seccion == "D"

    replace seccion = "E" if inrange(`ramavar', 36, 39)
    replace desc_seccion = "Distribución de agua; evacuación y tratamiento de aguas residuales" if seccion == "E"

    replace seccion = "F" if inrange(`ramavar', 41, 43)
    replace desc_seccion = "Construcción" if seccion == "F"

    replace seccion = "G" if inrange(`ramavar', 45, 47)
    replace desc_seccion = "Comercio al por mayor y al por menor; reparación de vehículos" if seccion == "G"

    replace seccion = "H" if inrange(`ramavar', 49, 53)
    replace desc_seccion = "Transporte y almacenamiento" if seccion == "H"

    replace seccion = "I" if inrange(`ramavar', 55, 56)
    replace desc_seccion = "Alojamiento y servicios de comida" if seccion == "I"

    replace seccion = "J" if inrange(`ramavar', 58, 63)
    replace desc_seccion = "Información y comunicaciones" if seccion == "J"

    replace seccion = "K" if inrange(`ramavar', 64, 66)
    replace desc_seccion = "Actividades financieras y de seguros" if seccion == "K"

    replace seccion = "L" if `ramavar' == 68
    replace desc_seccion = "Actividades inmobiliarias" if seccion == "L"

    replace seccion = "M" if inrange(`ramavar', 69, 75)
    replace desc_seccion = "Actividades profesionales, científicas y técnicas" if seccion == "M"

    replace seccion = "N" if inrange(`ramavar', 77, 82)
    replace desc_seccion = "Actividades de servicios administrativos y de apoyo" if seccion == "N"

    replace seccion = "O" if `ramavar' == 84
    replace desc_seccion = "Administración pública y defensa; seguridad social obligatoria" if seccion == "O"

    replace seccion = "P" if `ramavar' == 85
    replace desc_seccion = "Educación" if seccion == "P"

    replace seccion = "Q" if inrange(`ramavar', 86, 88)
    replace desc_seccion = "Actividades de atención de la salud humana y de asistencia social" if seccion == "Q"

    replace seccion = "R" if inrange(`ramavar', 90, 93)
    replace desc_seccion = "Actividades artísticas, de entretenimiento y recreación" if seccion == "R"

    replace seccion = "S" if inrange(`ramavar', 94, 96)
    replace desc_seccion = "Otras actividades de servicios" if seccion == "S"

    replace seccion = "T" if inrange(`ramavar', 97, 98)
    replace desc_seccion = "Actividades de los hogares como empleadores" if seccion == "T"

    replace seccion = "U" if `ramavar' == 99
    replace desc_seccion = "Actividades de organizaciones y entidades extraterritoriales" if seccion == "U"
}

else {

    *===================================================
    * CIIU Rev. 3
    *===================================================

    replace seccion = "A" if inlist(`ramavar', 1, 2)
    replace desc_seccion = "Agricultura, ganadería, caza y silvicultura" if seccion == "A"

    replace seccion = "B" if `ramavar' == 5
    replace desc_seccion = "Pesca" if seccion == "B"

    replace seccion = "C" if inrange(`ramavar', 10, 14)
    replace desc_seccion = "Explotación de minas y canteras" if seccion == "C"

    replace seccion = "D" if inrange(`ramavar', 15, 37)
    replace desc_seccion = "Industrias manufactureras" if seccion == "D"

    replace seccion = "E" if inrange(`ramavar', 40, 41)
    replace desc_seccion = "Suministro de electricidad, gas y agua" if seccion == "E"

    replace seccion = "F" if `ramavar' == 45
    replace desc_seccion = "Construcción" if seccion == "F"

    replace seccion = "G" if inrange(`ramavar', 50, 52)
    replace desc_seccion = "Comercio; reparación de vehículos y efectos personales" if seccion == "G"

    replace seccion = "H" if `ramavar' == 55
    replace desc_seccion = "Hoteles y restaurantes" if seccion == "H"

    replace seccion = "I" if inrange(`ramavar', 60, 64)
    replace desc_seccion = "Transporte, almacenamiento y comunicaciones" if seccion == "I"

    replace seccion = "J" if inrange(`ramavar', 65, 67)
    replace desc_seccion = "Intermediación financiera" if seccion == "J"

    replace seccion = "K" if inrange(`ramavar', 70, 74)
    replace desc_seccion = "Actividades inmobiliarias, empresariales y de alquiler" if seccion == "K"

    replace seccion = "L" if `ramavar' == 75
    replace desc_seccion = "Administración pública y defensa; seguridad social obligatoria" if seccion == "L"

    replace seccion = "M" if `ramavar' == 80
    replace desc_seccion = "Educación" if seccion == "M"

    replace seccion = "N" if `ramavar' == 85
    replace desc_seccion = "Servicios sociales y de salud" if seccion == "N"

    replace seccion = "O" if inrange(`ramavar', 90, 93)
    replace desc_seccion = "Otras actividades de servicios comunitarios, sociales y personales" if seccion == "O"

    replace seccion = "P" if `ramavar' == 95
    replace desc_seccion = "Hogares privados con servicio doméstico" if seccion == "P"

    replace seccion = "Q" if `ramavar' == 99
    replace desc_seccion = "Organizaciones y órganos extraterritoriales" if seccion == "Q"
}

drop if seccion == ""


*===================================================
* 8. INGRESO LABORAL ANUAL E INGRESO POR HORA
*===================================================

gen INGLABO_ANUAL = INGLABO * 12

gen horas_semana = P6800
replace horas_semana = . if missing(horas_semana)
replace horas_semana = . if horas_semana <= 0
replace horas_semana = . if horas_semana > 168

gen INGLABO_HORA = INGLABO_ANUAL / (horas_semana * 52) if !missing(horas_semana)


*===================================================
* 9. CALCULAR RESULTADOS POR SECCIÓN
*===================================================

tempfile resultados
tempname memhold

capture postutil clear

postfile `memhold' ///
    str1 seccion ///
    str120 desc_seccion ///
    double n_obs ///
    double trabajadores ///
    double total_inglabo_anual ///
    double mean_inglabo_anual ///
    double mean_inglabo_mensual ///
    double median_inglabo_anual ///
    double median_inglabo_mensual ///
    double mean_inglabo_hora ///
    double median_inglabo_hora ///
    using `resultados', replace


levelsof seccion, local(lista_sec)

foreach s of local lista_sec {

    preserve

        keep if seccion == "`s'"

        count
        local nobs = r(N)

        local desc "`=desc_seccion[1]'"

        * Trabajadores expandidos
        quietly summarize `fac', meanonly
        local trabajadores = r(sum)

        * Total ingreso laboral anual expandido
        gen ingreso_anual_expandido = INGLABO_ANUAL * `fac'
        quietly summarize ingreso_anual_expandido, meanonly
        local total_inglabo_anual = r(sum)

        * Promedio ponderado ingreso anual
        local mean_inglabo_anual = `total_inglabo_anual' / `trabajadores'

        * Promedio mensual equivalente
        local mean_inglabo_mensual = `mean_inglabo_anual' / 12

        * Mediana ponderada ingreso anual
        sort INGLABO_ANUAL
        gen peso = `fac'
        egen peso_total = total(peso)
        gen peso_acum = sum(peso)
        gen frac_acum = peso_acum / peso_total

        quietly summarize INGLABO_ANUAL if frac_acum >= 0.5, meanonly
        local median_inglabo_anual = r(min)

        * Mediana mensual equivalente
        local median_inglabo_mensual = `median_inglabo_anual' / 12

        * Promedio y mediana ingreso por hora
        local mean_inglabo_hora = .
        local median_inglabo_hora = .

        quietly count if !missing(INGLABO_HORA)

        if r(N) > 0 {

            gen ingreso_hora_expandido = INGLABO_HORA * `fac' if !missing(INGLABO_HORA)

            quietly summarize ingreso_hora_expandido, meanonly
            local total_inglabo_hora = r(sum)

            quietly summarize `fac' if !missing(INGLABO_HORA), meanonly
            local trabajadores_hora = r(sum)

            local mean_inglabo_hora = `total_inglabo_hora' / `trabajadores_hora'

            sort INGLABO_HORA
            gen peso_hora = `fac' if !missing(INGLABO_HORA)
            egen peso_total_hora = total(peso_hora)
            gen peso_acum_hora = sum(peso_hora) if !missing(INGLABO_HORA)
            gen frac_acum_hora = peso_acum_hora / peso_total_hora

            quietly summarize INGLABO_HORA if frac_acum_hora >= 0.5 & !missing(INGLABO_HORA), meanonly
            local median_inglabo_hora = r(min)
        }

        post `memhold' ///
            ("`s'") ///
            ("`desc'") ///
            (`nobs') ///
            (`trabajadores') ///
            (`total_inglabo_anual') ///
            (`mean_inglabo_anual') ///
            (`mean_inglabo_mensual') ///
            (`median_inglabo_anual') ///
            (`median_inglabo_mensual') ///
            (`mean_inglabo_hora') ///
            (`median_inglabo_hora')

    restore
}

postclose `memhold'


*===================================================
* 10. ORGANIZAR RESULTADOS
*===================================================

use `resultados', clear

sort seccion

order seccion desc_seccion ///
      trabajadores ///
      mean_inglabo_anual mean_inglabo_mensual ///
      median_inglabo_anual median_inglabo_mensual ///
      mean_inglabo_hora median_inglabo_hora ///
      total_inglabo_anual n_obs

label variable seccion "Sección CIIU"
label variable desc_seccion "Descripción de sección"
label variable trabajadores "Trabajadores expandidos"
label variable mean_inglabo_anual "Ingreso laboral anual promedio"
label variable mean_inglabo_mensual "Ingreso laboral mensual promedio"
label variable median_inglabo_anual "Ingreso laboral anual mediano"
label variable median_inglabo_mensual "Ingreso laboral mensual mediano"
label variable mean_inglabo_hora "Ingreso laboral por hora promedio"
label variable median_inglabo_hora "Ingreso laboral por hora mediano"
label variable total_inglabo_anual "Total ingreso laboral anual expandido"
label variable n_obs "Observaciones no expandidas"

format trabajadores %15.0fc
format total_inglabo_anual %18.0fc
format mean_inglabo_anual %15.2fc
format mean_inglabo_mensual %15.2fc
format median_inglabo_anual %15.2fc
format median_inglabo_mensual %15.2fc
format mean_inglabo_hora %15.2fc
format median_inglabo_hora %15.2fc


*===================================================
* 11. EXPORTAR A EXCEL Y GUARDAR DTA
*===================================================

save "Outputs/tables/consolidadosector_`year'.dta", replace

export excel using "`archivo'", ///
    sheet("consolidado_sector_`year'") ///
    firstrow(varlabels) ///
    replace

di "===================================================="
di "Archivo exportado correctamente:"
di "`archivo'"
di "Hoja: consolidado_sector_`year'"
di "Variable rama usada: `ramavar'"
di "Clasificación usada: `ciiu_rev'"
di "===================================================="