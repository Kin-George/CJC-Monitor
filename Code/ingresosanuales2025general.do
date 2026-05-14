****************************************************
* INGRESO LABORAL POR SECCIÓN CIIU Y TAMAÑO EMPRESA
* GEIH 2025
* Sección CIIU Rev. 4 + P3069 + ingreso por hora
****************************************************

clear all
set more off

cd "C:/Users/jorge/Documents/Databases/GEIH"

capture mkdir "Outputs"
capture mkdir "Outputs/tables"

local archivo "Outputs/tables/INGLABO_secciones_tamano_empresa_2025.xlsx"
capture erase "`archivo'"

use "GEIH_2025_TOTAL.dta", clear


*===================================================
* 0. VERIFICAR VARIABLES NECESARIAS
*===================================================

foreach v in OCI FEX_C18 INGLABO RAMA2D_R4 P3069 P6800 {
    capture confirm variable `v'
    if _rc {
        di as error "No existe la variable `v' en la base."
        exit 111
    }
}


*===================================================
* 1. ASEGURAR VARIABLES NUMÉRICAS
*===================================================

foreach v in OCI FEX_C18 INGLABO RAMA2D_R4 P3069 P6800 {
    capture confirm numeric variable `v'
    if _rc {
        destring `v', replace force
    }
}


*===================================================
* 2. SOLO OCUPADOS
*===================================================

keep if OCI == 1


*===================================================
* 3. DEFINIR FACTOR DE EXPANSIÓN
*===================================================
* Como se trabaja con la base anual construida a partir
* de meses, se divide FEX_C18 entre 12 para obtener
* trabajadores promedio del año.

gen FEX = FEX_C18/12
local fac "FEX"


*===================================================
* 4. FILTROS BASE
*===================================================

keep if !missing(INGLABO)
keep if INGLABO > 0

keep if !missing(`fac')
keep if `fac' > 0

keep if !missing(RAMA2D_R4)


*===================================================
* 5. CREAR SECCIÓN CIIU REV. 4
*===================================================

gen str1 seccion = ""
gen str120 desc_seccion = ""

replace seccion = "A" if inrange(RAMA2D_R4, 1, 3)
replace desc_seccion = "Agricultura, ganadería, caza, silvicultura y pesca" if seccion == "A"

replace seccion = "B" if inrange(RAMA2D_R4, 5, 9)
replace desc_seccion = "Explotación de minas y canteras" if seccion == "B"

replace seccion = "C" if inrange(RAMA2D_R4, 10, 33)
replace desc_seccion = "Industrias manufactureras" if seccion == "C"

replace seccion = "D" if RAMA2D_R4 == 35
replace desc_seccion = "Suministro de electricidad, gas, vapor y aire acondicionado" if seccion == "D"

replace seccion = "E" if inrange(RAMA2D_R4, 36, 39)
replace desc_seccion = "Distribución de agua; evacuación y tratamiento de aguas residuales" if seccion == "E"

replace seccion = "F" if inrange(RAMA2D_R4, 41, 43)
replace desc_seccion = "Construcción" if seccion == "F"

replace seccion = "G" if inrange(RAMA2D_R4, 45, 47)
replace desc_seccion = "Comercio al por mayor y al por menor; reparación de vehículos" if seccion == "G"

replace seccion = "H" if inrange(RAMA2D_R4, 49, 53)
replace desc_seccion = "Transporte y almacenamiento" if seccion == "H"

replace seccion = "I" if inrange(RAMA2D_R4, 55, 56)
replace desc_seccion = "Alojamiento y servicios de comida" if seccion == "I"

replace seccion = "J" if inrange(RAMA2D_R4, 58, 63)
replace desc_seccion = "Información y comunicaciones" if seccion == "J"

replace seccion = "K" if inrange(RAMA2D_R4, 64, 66)
replace desc_seccion = "Actividades financieras y de seguros" if seccion == "K"

replace seccion = "L" if RAMA2D_R4 == 68
replace desc_seccion = "Actividades inmobiliarias" if seccion == "L"

replace seccion = "M" if inrange(RAMA2D_R4, 69, 75)
replace desc_seccion = "Actividades profesionales, científicas y técnicas" if seccion == "M"

replace seccion = "N" if inrange(RAMA2D_R4, 77, 82)
replace desc_seccion = "Actividades de servicios administrativos y de apoyo" if seccion == "N"

replace seccion = "O" if RAMA2D_R4 == 84
replace desc_seccion = "Administración pública y defensa; seguridad social obligatoria" if seccion == "O"

replace seccion = "P" if RAMA2D_R4 == 85
replace desc_seccion = "Educación" if seccion == "P"

replace seccion = "Q" if inrange(RAMA2D_R4, 86, 88)
replace desc_seccion = "Actividades de atención de la salud humana y de asistencia social" if seccion == "Q"

replace seccion = "R" if inrange(RAMA2D_R4, 90, 93)
replace desc_seccion = "Actividades artísticas, de entretenimiento y recreación" if seccion == "R"

replace seccion = "S" if inrange(RAMA2D_R4, 94, 96)
replace desc_seccion = "Otras actividades de servicios" if seccion == "S"

replace seccion = "T" if inrange(RAMA2D_R4, 97, 98)
replace desc_seccion = "Actividades de los hogares como empleadores" if seccion == "T"

replace seccion = "U" if RAMA2D_R4 == 99
replace desc_seccion = "Actividades de organizaciones y entidades extraterritoriales" if seccion == "U"

drop if seccion == ""


*===================================================
* 6. CREAR CATEGORÍAS DE TAMAÑO DE EMPRESA - P3069
*===================================================

gen byte tamano_cod = P3069
replace tamano_cod = 99 if !inrange(P3069, 1, 10)

label define tamano_lbl ///
    1  "Trabaja solo" ///
    2  "2 a 3 personas" ///
    3  "4 a 5 personas" ///
    4  "6 a 10 personas" ///
    5  "11 a 19 personas" ///
    6  "20 a 30 personas" ///
    7  "31 a 50 personas" ///
    8  "51 a 100 personas" ///
    9  "101 a 200 personas" ///
    10 "201 o más personas" ///
    99 "Sin información", replace

label values tamano_cod tamano_lbl
decode tamano_cod, gen(tamano_empresa)


*===================================================
* 7. INGRESO LABORAL ANUAL E INGRESO POR HORA
*===================================================

gen INGLABO_ANUAL = INGLABO * 12

* P6800 se interpreta como horas trabajadas semanalmente.
* Ingreso por hora = ingreso anual / horas anuales.
* Horas anuales = P6800 * 52.

gen horas_semana = P6800
replace horas_semana = . if missing(horas_semana)
replace horas_semana = . if horas_semana <= 0
replace horas_semana = . if horas_semana > 168

gen INGLABO_HORA = INGLABO_ANUAL / (horas_semana * 52) if !missing(horas_semana)


*===================================================
* 8. CALCULAR RESULTADOS POR SECCIÓN Y TAMAÑO
*===================================================

tempfile resultados
tempname memhold

capture postutil clear

postfile `memhold' ///
    str1 seccion ///
    byte tamano_cod ///
    double n_obs ///
    double trabajadores ///
    double total_inglabo_anual ///
    double mean_inglabo_anual ///
    double median_inglabo_anual ///
    double mean_inglabo_hora ///
    double median_inglabo_hora ///
    using `resultados', replace


levelsof seccion, local(lista_sec)

foreach s of local lista_sec {

    levelsof tamano_cod if seccion == "`s'", local(lista_tam)

    foreach t of local lista_tam {

        preserve

            keep if seccion == "`s'" & tamano_cod == `t'

            count
            local nobs = r(N)

            * Trabajadores expandidos
            quietly summarize `fac', meanonly
            local trabajadores = r(sum)

            * Total ingreso laboral anual expandido
            gen ingreso_anual_expandido = INGLABO_ANUAL * `fac'
            quietly summarize ingreso_anual_expandido, meanonly
            local total_inglabo_anual = r(sum)

            * Promedio ponderado ingreso laboral anual
            local mean_inglabo_anual = `total_inglabo_anual' / `trabajadores'

            * Mediana ponderada ingreso laboral anual
            sort INGLABO_ANUAL
            gen peso = `fac'
            egen peso_total = total(peso)
            gen peso_acum = sum(peso)
            gen frac_acum = peso_acum / peso_total

            quietly summarize INGLABO_ANUAL if frac_acum >= 0.5, meanonly
            local median_inglabo_anual = r(min)

            * Inicializar ingreso laboral por hora
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
                (`t') ///
                (`nobs') ///
                (`trabajadores') ///
                (`total_inglabo_anual') ///
                (`mean_inglabo_anual') ///
                (`median_inglabo_anual') ///
                (`mean_inglabo_hora') ///
                (`median_inglabo_hora')

        restore
    }
}

postclose `memhold'


*===================================================
* 9. RECONSTRUIR DESCRIPCIONES DE SECCIÓN Y TAMAÑO
*===================================================

use `resultados', clear

gen str120 desc_seccion = ""

replace desc_seccion = "Agricultura, ganadería, caza, silvicultura y pesca" if seccion == "A"
replace desc_seccion = "Explotación de minas y canteras" if seccion == "B"
replace desc_seccion = "Industrias manufactureras" if seccion == "C"
replace desc_seccion = "Suministro de electricidad, gas, vapor y aire acondicionado" if seccion == "D"
replace desc_seccion = "Distribución de agua; evacuación y tratamiento de aguas residuales" if seccion == "E"
replace desc_seccion = "Construcción" if seccion == "F"
replace desc_seccion = "Comercio al por mayor y al por menor; reparación de vehículos" if seccion == "G"
replace desc_seccion = "Transporte y almacenamiento" if seccion == "H"
replace desc_seccion = "Alojamiento y servicios de comida" if seccion == "I"
replace desc_seccion = "Información y comunicaciones" if seccion == "J"
replace desc_seccion = "Actividades financieras y de seguros" if seccion == "K"
replace desc_seccion = "Actividades inmobiliarias" if seccion == "L"
replace desc_seccion = "Actividades profesionales, científicas y técnicas" if seccion == "M"
replace desc_seccion = "Actividades de servicios administrativos y de apoyo" if seccion == "N"
replace desc_seccion = "Administración pública y defensa; seguridad social obligatoria" if seccion == "O"
replace desc_seccion = "Educación" if seccion == "P"
replace desc_seccion = "Actividades de atención de la salud humana y de asistencia social" if seccion == "Q"
replace desc_seccion = "Actividades artísticas, de entretenimiento y recreación" if seccion == "R"
replace desc_seccion = "Otras actividades de servicios" if seccion == "S"
replace desc_seccion = "Actividades de los hogares como empleadores" if seccion == "T"
replace desc_seccion = "Actividades de organizaciones y entidades extraterritoriales" if seccion == "U"


gen str40 tamano_empresa = ""

replace tamano_empresa = "Trabaja solo" if tamano_cod == 1
replace tamano_empresa = "2 a 3 personas" if tamano_cod == 2
replace tamano_empresa = "4 a 5 personas" if tamano_cod == 3
replace tamano_empresa = "6 a 10 personas" if tamano_cod == 4
replace tamano_empresa = "11 a 19 personas" if tamano_cod == 5
replace tamano_empresa = "20 a 30 personas" if tamano_cod == 6
replace tamano_empresa = "31 a 50 personas" if tamano_cod == 7
replace tamano_empresa = "51 a 100 personas" if tamano_cod == 8
replace tamano_empresa = "101 a 200 personas" if tamano_cod == 9
replace tamano_empresa = "201 o más personas" if tamano_cod == 10
replace tamano_empresa = "Sin información" if tamano_cod == 99


*===================================================
* 10. ORGANIZAR RESULTADOS
*===================================================

sort seccion tamano_cod

order seccion desc_seccion tamano_cod tamano_empresa ///
      trabajadores mean_inglabo_anual median_inglabo_anual ///
      mean_inglabo_hora median_inglabo_hora ///
      total_inglabo_anual n_obs

label variable seccion "Sección"
label variable desc_seccion "Descripción de sección"
label variable tamano_cod "Código tamaño empresa"
label variable tamano_empresa "Tamaño empresa P3069"
label variable trabajadores "Trabajadores expandidos"
label variable mean_inglabo_anual "Ingreso laboral anual promedio"
label variable median_inglabo_anual "Ingreso laboral anual mediano"
label variable mean_inglabo_hora "Ingreso laboral por hora promedio"
label variable median_inglabo_hora "Ingreso laboral por hora mediano"
label variable total_inglabo_anual "Total ingreso laboral anual expandido"
label variable n_obs "Observaciones no expandidas"

format trabajadores %15.0fc
format total_inglabo_anual %18.0fc
format mean_inglabo_anual %15.2fc
format median_inglabo_anual %15.2fc
format mean_inglabo_hora %15.2fc
format median_inglabo_hora %15.2fc


*===================================================
* 11. EXPORTAR A EXCEL
*===================================================

local archivo "Outputs/tables/INGLABO_secciones_tamano_empresa_2025.xlsx"

save "Outputs/tables/INGLABO_secciones_tamano_empresa_2025.dta", replace

export excel using "`archivo'", ///
    sheet("todas_secciones") ///
    firstrow(varlabels) ///
    replace

levelsof seccion, local(lista_sec_export)

foreach s of local lista_sec_export {

    preserve

        keep if seccion == "`s'"
        sort tamano_cod

        export excel using "`archivo'", ///
            sheet("seccion_`s'") ///
            firstrow(varlabels) ///
            sheetreplace

    restore
}

di "===================================================="
di "Archivo exportado correctamente:"
di "`archivo'"
di "===================================================="


****************************************************
* CONSOLIDADO POR SECTOR / SECCIÓN CIIU
* GEIH 2025
* Sin discriminación por tamaño de empresa
****************************************************

clear all
set more off

cd "C:/Users/jorge/Documents/Databases/GEIH"

capture mkdir "Outputs"
capture mkdir "Outputs/tables"

local archivo "Outputs/tables/consolidadosector.xlsx"
capture erase "`archivo'"

use "GEIH_2025_TOTAL.dta", clear


*===================================================
* 0. VERIFICAR VARIABLES NECESARIAS
*===================================================

foreach v in OCI FEX_C18 INGLABO RAMA2D_R4 P6800 {
    capture confirm variable `v'
    if _rc {
        di as error "No existe la variable `v' en la base."
        exit 111
    }
}


*===================================================
* 1. ASEGURAR VARIABLES NUMÉRICAS
*===================================================

foreach v in OCI FEX_C18 INGLABO RAMA2D_R4 P6800 {
    capture confirm numeric variable `v'
    if _rc {
        destring `v', replace force
    }
}


*===================================================
* 2. SOLO OCUPADOS
*===================================================

keep if OCI == 1


*===================================================
* 3. DEFINIR FACTOR DE EXPANSIÓN
*===================================================
* Como se trabaja con base anual construida desde meses,
* se divide FEX_C18 entre 12 para obtener trabajadores
* promedio del año.

gen FEX = FEX_C18/12
local fac "FEX"


*===================================================
* 4. FILTROS BASE
*===================================================

keep if !missing(INGLABO)
keep if INGLABO > 0

keep if !missing(`fac')
keep if `fac' > 0

keep if !missing(RAMA2D_R4)


*===================================================
* 5. CREAR SECCIÓN CIIU REV. 4
*===================================================

gen str1 seccion = ""
gen str120 desc_seccion = ""

replace seccion = "A" if inrange(RAMA2D_R4, 1, 3)
replace desc_seccion = "Agricultura, ganadería, caza, silvicultura y pesca" if seccion == "A"

replace seccion = "B" if inrange(RAMA2D_R4, 5, 9)
replace desc_seccion = "Explotación de minas y canteras" if seccion == "B"

replace seccion = "C" if inrange(RAMA2D_R4, 10, 33)
replace desc_seccion = "Industrias manufactureras" if seccion == "C"

replace seccion = "D" if RAMA2D_R4 == 35
replace desc_seccion = "Suministro de electricidad, gas, vapor y aire acondicionado" if seccion == "D"

replace seccion = "E" if inrange(RAMA2D_R4, 36, 39)
replace desc_seccion = "Distribución de agua; evacuación y tratamiento de aguas residuales" if seccion == "E"

replace seccion = "F" if inrange(RAMA2D_R4, 41, 43)
replace desc_seccion = "Construcción" if seccion == "F"

replace seccion = "G" if inrange(RAMA2D_R4, 45, 47)
replace desc_seccion = "Comercio al por mayor y al por menor; reparación de vehículos" if seccion == "G"

replace seccion = "H" if inrange(RAMA2D_R4, 49, 53)
replace desc_seccion = "Transporte y almacenamiento" if seccion == "H"

replace seccion = "I" if inrange(RAMA2D_R4, 55, 56)
replace desc_seccion = "Alojamiento y servicios de comida" if seccion == "I"

replace seccion = "J" if inrange(RAMA2D_R4, 58, 63)
replace desc_seccion = "Información y comunicaciones" if seccion == "J"

replace seccion = "K" if inrange(RAMA2D_R4, 64, 66)
replace desc_seccion = "Actividades financieras y de seguros" if seccion == "K"

replace seccion = "L" if RAMA2D_R4 == 68
replace desc_seccion = "Actividades inmobiliarias" if seccion == "L"

replace seccion = "M" if inrange(RAMA2D_R4, 69, 75)
replace desc_seccion = "Actividades profesionales, científicas y técnicas" if seccion == "M"

replace seccion = "N" if inrange(RAMA2D_R4, 77, 82)
replace desc_seccion = "Actividades de servicios administrativos y de apoyo" if seccion == "N"

replace seccion = "O" if RAMA2D_R4 == 84
replace desc_seccion = "Administración pública y defensa; seguridad social obligatoria" if seccion == "O"

replace seccion = "P" if RAMA2D_R4 == 85
replace desc_seccion = "Educación" if seccion == "P"

replace seccion = "Q" if inrange(RAMA2D_R4, 86, 88)
replace desc_seccion = "Actividades de atención de la salud humana y de asistencia social" if seccion == "Q"

replace seccion = "R" if inrange(RAMA2D_R4, 90, 93)
replace desc_seccion = "Actividades artísticas, de entretenimiento y recreación" if seccion == "R"

replace seccion = "S" if inrange(RAMA2D_R4, 94, 96)
replace desc_seccion = "Otras actividades de servicios" if seccion == "S"

replace seccion = "T" if inrange(RAMA2D_R4, 97, 98)
replace desc_seccion = "Actividades de los hogares como empleadores" if seccion == "T"

replace seccion = "U" if RAMA2D_R4 == 99
replace desc_seccion = "Actividades de organizaciones y entidades extraterritoriales" if seccion == "U"

drop if seccion == ""


*===================================================
* 6. INGRESO LABORAL ANUAL E INGRESO POR HORA
*===================================================

gen INGLABO_ANUAL = INGLABO * 12

gen horas_semana = P6800
replace horas_semana = . if missing(horas_semana)
replace horas_semana = . if horas_semana <= 0
replace horas_semana = . if horas_semana > 168

gen INGLABO_HORA = INGLABO_ANUAL / (horas_semana * 52) if !missing(horas_semana)


*===================================================
* 7. CALCULAR RESULTADOS POR SECCIÓN
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

        local desc = desc_seccion[1]

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
* 8. ORGANIZAR RESULTADOS
*===================================================

use `resultados', clear

sort seccion

order seccion desc_seccion ///
      trabajadores ///
      mean_inglabo_anual mean_inglabo_mensual ///
      median_inglabo_anual median_inglabo_mensual ///
      mean_inglabo_hora median_inglabo_hora ///
      total_inglabo_anual n_obs

label variable seccion "Sección"
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
* 9. EXPORTAR A EXCEL Y GUARDAR DTA
*===================================================

save "Outputs/tables/consolidadosector.dta", replace

export excel using "`archivo'", ///
    sheet("consolidado_sector") ///
    firstrow(varlabels) ///
    replace

di "===================================================="
di "Archivo exportado correctamente:"
di "`archivo'"
di "Hoja: consolidado_sector"
di "===================================================="

****************************************************
* INGRESO LABORAL POR SECCIÓN, NIVEL EDUCATIVO
* Y TAMAÑO DE EMPRESA
* GEIH 2025
* Sección CIIU Rev. 4 + P3042 + P3069
****************************************************

clear all
set more off

cd "C:/Users/jorge/Documents/Databases/GEIH"

capture mkdir "Outputs"
capture mkdir "Outputs/tables"

local archivo "Outputs/tables/INGLABO_seccion_educacion_tamano_2025.xlsx"
capture erase "`archivo'"

use "GEIH_2025_TOTAL.dta", clear


*===================================================
* 0. VERIFICAR VARIABLES NECESARIAS
*===================================================

foreach v in OCI FEX_C18 INGLABO RAMA2D_R4 P3069 P6800 P3042 {
    capture confirm variable `v'
    if _rc {
        di as error "No existe la variable `v' en la base."
        exit 111
    }
}


*===================================================
* 1. ASEGURAR VARIABLES NUMÉRICAS
*===================================================

foreach v in OCI FEX_C18 INGLABO RAMA2D_R4 P3069 P6800 P3042 {
    capture confirm numeric variable `v'
    if _rc {
        destring `v', replace force
    }
}


*===================================================
* 2. SOLO OCUPADOS
*===================================================

keep if OCI == 1


*===================================================
* 3. DEFINIR FACTOR DE EXPANSIÓN
*===================================================

gen FEX = FEX_C18/12
local fac "FEX"


*===================================================
* 4. FILTROS BASE
*===================================================

keep if !missing(INGLABO)
keep if INGLABO > 0

keep if !missing(`fac')
keep if `fac' > 0

keep if !missing(RAMA2D_R4)


*===================================================
* 5. CREAR SECCIÓN CIIU REV. 4
*===================================================

gen str1 seccion = ""
gen str120 desc_seccion = ""

replace seccion = "A" if inrange(RAMA2D_R4, 1, 3)
replace desc_seccion = "Agricultura, ganadería, caza, silvicultura y pesca" if seccion == "A"

replace seccion = "B" if inrange(RAMA2D_R4, 5, 9)
replace desc_seccion = "Explotación de minas y canteras" if seccion == "B"

replace seccion = "C" if inrange(RAMA2D_R4, 10, 33)
replace desc_seccion = "Industrias manufactureras" if seccion == "C"

replace seccion = "D" if RAMA2D_R4 == 35
replace desc_seccion = "Suministro de electricidad, gas, vapor y aire acondicionado" if seccion == "D"

replace seccion = "E" if inrange(RAMA2D_R4, 36, 39)
replace desc_seccion = "Distribución de agua; evacuación y tratamiento de aguas residuales" if seccion == "E"

replace seccion = "F" if inrange(RAMA2D_R4, 41, 43)
replace desc_seccion = "Construcción" if seccion == "F"

replace seccion = "G" if inrange(RAMA2D_R4, 45, 47)
replace desc_seccion = "Comercio al por mayor y al por menor; reparación de vehículos" if seccion == "G"

replace seccion = "H" if inrange(RAMA2D_R4, 49, 53)
replace desc_seccion = "Transporte y almacenamiento" if seccion == "H"

replace seccion = "I" if inrange(RAMA2D_R4, 55, 56)
replace desc_seccion = "Alojamiento y servicios de comida" if seccion == "I"

replace seccion = "J" if inrange(RAMA2D_R4, 58, 63)
replace desc_seccion = "Información y comunicaciones" if seccion == "J"

replace seccion = "K" if inrange(RAMA2D_R4, 64, 66)
replace desc_seccion = "Actividades financieras y de seguros" if seccion == "K"

replace seccion = "L" if RAMA2D_R4 == 68
replace desc_seccion = "Actividades inmobiliarias" if seccion == "L"

replace seccion = "M" if inrange(RAMA2D_R4, 69, 75)
replace desc_seccion = "Actividades profesionales, científicas y técnicas" if seccion == "M"

replace seccion = "N" if inrange(RAMA2D_R4, 77, 82)
replace desc_seccion = "Actividades de servicios administrativos y de apoyo" if seccion == "N"

replace seccion = "O" if RAMA2D_R4 == 84
replace desc_seccion = "Administración pública y defensa; seguridad social obligatoria" if seccion == "O"

replace seccion = "P" if RAMA2D_R4 == 85
replace desc_seccion = "Educación" if seccion == "P"

replace seccion = "Q" if inrange(RAMA2D_R4, 86, 88)
replace desc_seccion = "Actividades de atención de la salud humana y de asistencia social" if seccion == "Q"

replace seccion = "R" if inrange(RAMA2D_R4, 90, 93)
replace desc_seccion = "Actividades artísticas, de entretenimiento y recreación" if seccion == "R"

replace seccion = "S" if inrange(RAMA2D_R4, 94, 96)
replace desc_seccion = "Otras actividades de servicios" if seccion == "S"

replace seccion = "T" if inrange(RAMA2D_R4, 97, 98)
replace desc_seccion = "Actividades de los hogares como empleadores" if seccion == "T"

replace seccion = "U" if RAMA2D_R4 == 99
replace desc_seccion = "Actividades de organizaciones y entidades extraterritoriales" if seccion == "U"

drop if seccion == ""


*===================================================
* 6. CREAR CATEGORÍAS DE TAMAÑO DE EMPRESA - P3069
*===================================================

gen byte tamano_cod = P3069
replace tamano_cod = 99 if missing(P3069) | !inrange(P3069, 1, 10)

label define tamano_lbl ///
    1  "Trabaja solo" ///
    2  "2 a 3 personas" ///
    3  "4 a 5 personas" ///
    4  "6 a 10 personas" ///
    5  "11 a 19 personas" ///
    6  "20 a 30 personas" ///
    7  "31 a 50 personas" ///
    8  "51 a 100 personas" ///
    9  "101 a 200 personas" ///
    10 "201 o más personas" ///
    99 "Sin información", replace

label values tamano_cod tamano_lbl


*===================================================
* 7. CREAR NIVEL EDUCATIVO - P3042
*===================================================

gen int nivel_educ_cod = P3042
replace nivel_educ_cod = 99 if missing(P3042) | ///
    (!inrange(P3042, 1, 13) & P3042 != 99)

label define nivel_educ_lbl ///
    1  "Ninguno" ///
    2  "Preescolar" ///
    3  "Básica primaria" ///
    4  "Básica secundaria" ///
    5  "Media académica" ///
    6  "Media técnica" ///
    7  "Normalista" ///
    8  "Técnica profesional" ///
    9  "Tecnológica" ///
    10 "Universitaria" ///
    11 "Especialización" ///
    12 "Maestría" ///
    13 "Doctorado" ///
    99 "No sabe, no informa", replace

label values nivel_educ_cod nivel_educ_lbl


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
* 9. CALCULAR RESULTADOS:
*    SECCIÓN X NIVEL EDUCATIVO X TAMAÑO EMPRESA
*===================================================

tempfile resultados
tempname memhold

capture postutil clear

postfile `memhold' ///
    str1 seccion ///
    int nivel_educ_cod ///
    byte tamano_cod ///
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

    levelsof nivel_educ_cod if seccion == "`s'", local(lista_educ)

    foreach e of local lista_educ {

        levelsof tamano_cod if seccion == "`s'" & nivel_educ_cod == `e', local(lista_tam)

        foreach t of local lista_tam {

            preserve

                keep if seccion == "`s'" & nivel_educ_cod == `e' & tamano_cod == `t'

                count
                local nobs = r(N)

                * Trabajadores expandidos
                quietly summarize `fac', meanonly
                local trabajadores = r(sum)

                * Total ingreso laboral anual expandido
                gen ingreso_anual_expandido = INGLABO_ANUAL * `fac'
                quietly summarize ingreso_anual_expandido, meanonly
                local total_inglabo_anual = r(sum)

                * Promedio ponderado ingreso laboral anual
                local mean_inglabo_anual = `total_inglabo_anual' / `trabajadores'

                * Promedio mensual equivalente
                local mean_inglabo_mensual = `mean_inglabo_anual' / 12

                * Mediana ponderada ingreso laboral anual
                sort INGLABO_ANUAL
                gen peso = `fac'
                egen peso_total = total(peso)
                gen peso_acum = sum(peso)
                gen frac_acum = peso_acum / peso_total

                quietly summarize INGLABO_ANUAL if frac_acum >= 0.5, meanonly
                local median_inglabo_anual = r(min)

                * Mediana mensual equivalente
                local median_inglabo_mensual = `median_inglabo_anual' / 12

                * Promedio y mediana de ingreso laboral por hora
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
                    (`e') ///
                    (`t') ///
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
    }
}

postclose `memhold'


*===================================================
* 10. RECONSTRUIR DESCRIPCIONES
*===================================================

use `resultados', clear

gen str120 desc_seccion = ""

replace desc_seccion = "Agricultura, ganadería, caza, silvicultura y pesca" if seccion == "A"
replace desc_seccion = "Explotación de minas y canteras" if seccion == "B"
replace desc_seccion = "Industrias manufactureras" if seccion == "C"
replace desc_seccion = "Suministro de electricidad, gas, vapor y aire acondicionado" if seccion == "D"
replace desc_seccion = "Distribución de agua; evacuación y tratamiento de aguas residuales" if seccion == "E"
replace desc_seccion = "Construcción" if seccion == "F"
replace desc_seccion = "Comercio al por mayor y al por menor; reparación de vehículos" if seccion == "G"
replace desc_seccion = "Transporte y almacenamiento" if seccion == "H"
replace desc_seccion = "Alojamiento y servicios de comida" if seccion == "I"
replace desc_seccion = "Información y comunicaciones" if seccion == "J"
replace desc_seccion = "Actividades financieras y de seguros" if seccion == "K"
replace desc_seccion = "Actividades inmobiliarias" if seccion == "L"
replace desc_seccion = "Actividades profesionales, científicas y técnicas" if seccion == "M"
replace desc_seccion = "Actividades de servicios administrativos y de apoyo" if seccion == "N"
replace desc_seccion = "Administración pública y defensa; seguridad social obligatoria" if seccion == "O"
replace desc_seccion = "Educación" if seccion == "P"
replace desc_seccion = "Actividades de atención de la salud humana y de asistencia social" if seccion == "Q"
replace desc_seccion = "Actividades artísticas, de entretenimiento y recreación" if seccion == "R"
replace desc_seccion = "Otras actividades de servicios" if seccion == "S"
replace desc_seccion = "Actividades de los hogares como empleadores" if seccion == "T"
replace desc_seccion = "Actividades de organizaciones y entidades extraterritoriales" if seccion == "U"


gen str50 nivel_educativo = ""

replace nivel_educativo = "Ninguno" if nivel_educ_cod == 1
replace nivel_educativo = "Preescolar" if nivel_educ_cod == 2
replace nivel_educativo = "Básica primaria" if nivel_educ_cod == 3
replace nivel_educativo = "Básica secundaria" if nivel_educ_cod == 4
replace nivel_educativo = "Media académica" if nivel_educ_cod == 5
replace nivel_educativo = "Media técnica" if nivel_educ_cod == 6
replace nivel_educativo = "Normalista" if nivel_educ_cod == 7
replace nivel_educativo = "Técnica profesional" if nivel_educ_cod == 8
replace nivel_educativo = "Tecnológica" if nivel_educ_cod == 9
replace nivel_educativo = "Universitaria" if nivel_educ_cod == 10
replace nivel_educativo = "Especialización" if nivel_educ_cod == 11
replace nivel_educativo = "Maestría" if nivel_educ_cod == 12
replace nivel_educativo = "Doctorado" if nivel_educ_cod == 13
replace nivel_educativo = "No sabe, no informa" if nivel_educ_cod == 99


gen str40 tamano_empresa = ""

replace tamano_empresa = "Trabaja solo" if tamano_cod == 1
replace tamano_empresa = "2 a 3 personas" if tamano_cod == 2
replace tamano_empresa = "4 a 5 personas" if tamano_cod == 3
replace tamano_empresa = "6 a 10 personas" if tamano_cod == 4
replace tamano_empresa = "11 a 19 personas" if tamano_cod == 5
replace tamano_empresa = "20 a 30 personas" if tamano_cod == 6
replace tamano_empresa = "31 a 50 personas" if tamano_cod == 7
replace tamano_empresa = "51 a 100 personas" if tamano_cod == 8
replace tamano_empresa = "101 a 200 personas" if tamano_cod == 9
replace tamano_empresa = "201 o más personas" if tamano_cod == 10
replace tamano_empresa = "Sin información" if tamano_cod == 99


*===================================================
* 11. ORGANIZAR RESULTADOS
*===================================================

sort seccion nivel_educ_cod tamano_cod

order seccion desc_seccion ///
      nivel_educ_cod nivel_educativo ///
      tamano_cod tamano_empresa ///
      trabajadores ///
      mean_inglabo_anual mean_inglabo_mensual ///
      median_inglabo_anual median_inglabo_mensual ///
      mean_inglabo_hora median_inglabo_hora ///
      total_inglabo_anual n_obs

label variable seccion "Sección"
label variable desc_seccion "Descripción de sección"
label variable nivel_educ_cod "Código nivel educativo P3042"
label variable nivel_educativo "Nivel educativo P3042"
label variable tamano_cod "Código tamaño empresa P3069"
label variable tamano_empresa "Tamaño empresa P3069"
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
* 12. EXPORTAR A EXCEL
*===================================================

save "Outputs/tables/INGLABO_seccion_educacion_tamano_2025.dta", replace

export excel using "`archivo'", ///
    sheet("todas_secciones") ///
    firstrow(varlabels) ///
    replace

levelsof seccion, local(lista_sec_export)

foreach s of local lista_sec_export {

    preserve

        keep if seccion == "`s'"
        sort nivel_educ_cod tamano_cod

        export excel using "`archivo'", ///
            sheet("seccion_`s'") ///
            firstrow(varlabels) ///
            sheetreplace

    restore
}

di "===================================================="
di "Archivo exportado correctamente:"
di "`archivo'"
di ""
di "Nivel de agregación:"
di "Sección x Nivel educativo P3042 x Tamaño empresa P3069"
di "===================================================="


****************************************************
* INGRESO LABORAL POR SECCIÓN, TAMAÑO DE EMPRESA
* Y FORMALIDAD LABORAL
* GEIH 2025
* Sección CIIU Rev. 4 + P3069 + P6090 + P6920
****************************************************

clear all
set more off

cd "C:/Users/jorge/Documents/Databases/GEIH"

capture mkdir "Outputs"
capture mkdir "Outputs/tables"

local archivo "Outputs/tables/INGLABO_seccion_tamano_formalidad_2025.xlsx"
capture erase "`archivo'"

use "GEIH_2025_TOTAL.dta", clear


*===================================================
* 0. VERIFICAR VARIABLES NECESARIAS
*===================================================

foreach v in OCI FEX_C18 INGLABO RAMA2D_R4 P3069 P6800 P6090 P6920 {
    capture confirm variable `v'
    if _rc {
        di as error "No existe la variable `v' en la base."
        exit 111
    }
}


*===================================================
* 1. ASEGURAR VARIABLES NUMÉRICAS
*===================================================

foreach v in OCI FEX_C18 INGLABO RAMA2D_R4 P3069 P6800 P6090 P6920 {
    capture confirm numeric variable `v'
    if _rc {
        destring `v', replace force
    }
}


*===================================================
* 2. SOLO OCUPADOS
*===================================================

keep if OCI == 1


*===================================================
* 3. DEFINIR FACTOR DE EXPANSIÓN
*===================================================
* Como se trabaja con base anual construida desde meses,
* se divide FEX_C18 entre 12 para obtener trabajadores
* promedio del año.

gen FEX = FEX_C18/12
local fac "FEX"


*===================================================
* 4. FILTROS BASE
*===================================================

keep if !missing(INGLABO)
keep if INGLABO > 0

keep if !missing(`fac')
keep if `fac' > 0

keep if !missing(RAMA2D_R4)


*===================================================
* 5. CREAR SECCIÓN CIIU REV. 4
*===================================================

gen str1 seccion = ""
gen str120 desc_seccion = ""

replace seccion = "A" if inrange(RAMA2D_R4, 1, 3)
replace desc_seccion = "Agricultura, ganadería, caza, silvicultura y pesca" if seccion == "A"

replace seccion = "B" if inrange(RAMA2D_R4, 5, 9)
replace desc_seccion = "Explotación de minas y canteras" if seccion == "B"

replace seccion = "C" if inrange(RAMA2D_R4, 10, 33)
replace desc_seccion = "Industrias manufactureras" if seccion == "C"

replace seccion = "D" if RAMA2D_R4 == 35
replace desc_seccion = "Suministro de electricidad, gas, vapor y aire acondicionado" if seccion == "D"

replace seccion = "E" if inrange(RAMA2D_R4, 36, 39)
replace desc_seccion = "Distribución de agua; evacuación y tratamiento de aguas residuales" if seccion == "E"

replace seccion = "F" if inrange(RAMA2D_R4, 41, 43)
replace desc_seccion = "Construcción" if seccion == "F"

replace seccion = "G" if inrange(RAMA2D_R4, 45, 47)
replace desc_seccion = "Comercio al por mayor y al por menor; reparación de vehículos" if seccion == "G"

replace seccion = "H" if inrange(RAMA2D_R4, 49, 53)
replace desc_seccion = "Transporte y almacenamiento" if seccion == "H"

replace seccion = "I" if inrange(RAMA2D_R4, 55, 56)
replace desc_seccion = "Alojamiento y servicios de comida" if seccion == "I"

replace seccion = "J" if inrange(RAMA2D_R4, 58, 63)
replace desc_seccion = "Información y comunicaciones" if seccion == "J"

replace seccion = "K" if inrange(RAMA2D_R4, 64, 66)
replace desc_seccion = "Actividades financieras y de seguros" if seccion == "K"

replace seccion = "L" if RAMA2D_R4 == 68
replace desc_seccion = "Actividades inmobiliarias" if seccion == "L"

replace seccion = "M" if inrange(RAMA2D_R4, 69, 75)
replace desc_seccion = "Actividades profesionales, científicas y técnicas" if seccion == "M"

replace seccion = "N" if inrange(RAMA2D_R4, 77, 82)
replace desc_seccion = "Actividades de servicios administrativos y de apoyo" if seccion == "N"

replace seccion = "O" if RAMA2D_R4 == 84
replace desc_seccion = "Administración pública y defensa; seguridad social obligatoria" if seccion == "O"

replace seccion = "P" if RAMA2D_R4 == 85
replace desc_seccion = "Educación" if seccion == "P"

replace seccion = "Q" if inrange(RAMA2D_R4, 86, 88)
replace desc_seccion = "Actividades de atención de la salud humana y de asistencia social" if seccion == "Q"

replace seccion = "R" if inrange(RAMA2D_R4, 90, 93)
replace desc_seccion = "Actividades artísticas, de entretenimiento y recreación" if seccion == "R"

replace seccion = "S" if inrange(RAMA2D_R4, 94, 96)
replace desc_seccion = "Otras actividades de servicios" if seccion == "S"

replace seccion = "T" if inrange(RAMA2D_R4, 97, 98)
replace desc_seccion = "Actividades de los hogares como empleadores" if seccion == "T"

replace seccion = "U" if RAMA2D_R4 == 99
replace desc_seccion = "Actividades de organizaciones y entidades extraterritoriales" if seccion == "U"

drop if seccion == ""


*===================================================
* 6. CREAR CATEGORÍAS DE TAMAÑO DE EMPRESA - P3069
*===================================================

gen byte tamano_cod = P3069
replace tamano_cod = 99 if missing(P3069) | !inrange(P3069, 1, 10)

label define tamano_lbl ///
    1  "Trabaja solo" ///
    2  "2 a 3 personas" ///
    3  "4 a 5 personas" ///
    4  "6 a 10 personas" ///
    5  "11 a 19 personas" ///
    6  "20 a 30 personas" ///
    7  "31 a 50 personas" ///
    8  "51 a 100 personas" ///
    9  "101 a 200 personas" ///
    10 "201 o más personas" ///
    99 "Sin información", replace

label values tamano_cod tamano_lbl


*===================================================
* 7. CREAR VARIABLE DE FORMALIDAD
*===================================================
* P6090:
* 1 Sí
* 2 No
* 9 No sabe, no informa
*
* P6920:
* 1 Sí cotiza a pensión
* 2 No cotiza
* 3 Ya es pensionado
*
* Criterio principal:
* Formal = P6090 == 1 y P6920 == 1
*
* Manejo de pensionados:
* Se dejan como categoría separada si están afiliados a salud.
* Esto evita clasificarlos automáticamente como informales,
* porque pueden seguir ocupados aunque ya no coticen a pensión.

gen byte formalidad_cod = .

* Excluir no sabe/no informa o respuestas no válidas
replace formalidad_cod = . if missing(P6090) | missing(P6920)
replace formalidad_cod = . if P6090 == 9
replace formalidad_cod = . if !inlist(P6090, 1, 2)
replace formalidad_cod = . if !inlist(P6920, 1, 2, 3)

* Formal estricto
replace formalidad_cod = 1 if P6090 == 1 & P6920 == 1

* Pensionado ocupado con afiliación a salud
replace formalidad_cod = 2 if P6090 == 1 & P6920 == 3

* Informal o no cumple doble condición
replace formalidad_cod = 3 if missing(formalidad_cod) ///
    & inlist(P6090, 1, 2) ///
    & inlist(P6920, 1, 2, 3)

label define formalidad_lbl ///
    1 "Formal: salud y pensión" ///
    2 "Pensionado ocupado afiliado a salud" ///
    3 "Informal / no cumple doble condición", replace

label values formalidad_cod formalidad_lbl

drop if missing(formalidad_cod)


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
* 9. CALCULAR RESULTADOS:
*    SECCIÓN X TAMAÑO EMPRESA X FORMALIDAD
*===================================================

tempfile resultados
tempname memhold

capture postutil clear

postfile `memhold' ///
    str1 seccion ///
    byte tamano_cod ///
    byte formalidad_cod ///
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

    levelsof tamano_cod if seccion == "`s'", local(lista_tam)

    foreach t of local lista_tam {

        levelsof formalidad_cod if seccion == "`s'" & tamano_cod == `t', local(lista_form)

        foreach f of local lista_form {

            preserve

                keep if seccion == "`s'" & tamano_cod == `t' & formalidad_cod == `f'

                count
                local nobs = r(N)

                * Trabajadores expandidos
                quietly summarize `fac', meanonly
                local trabajadores = r(sum)

                * Total ingreso laboral anual expandido
                gen ingreso_anual_expandido = INGLABO_ANUAL * `fac'
                quietly summarize ingreso_anual_expandido, meanonly
                local total_inglabo_anual = r(sum)

                * Promedio ponderado ingreso laboral anual
                local mean_inglabo_anual = `total_inglabo_anual' / `trabajadores'

                * Promedio mensual equivalente
                local mean_inglabo_mensual = `mean_inglabo_anual' / 12

                * Mediana ponderada ingreso laboral anual
                sort INGLABO_ANUAL
                gen peso = `fac'
                egen peso_total = total(peso)
                gen peso_acum = sum(peso)
                gen frac_acum = peso_acum / peso_total

                quietly summarize INGLABO_ANUAL if frac_acum >= 0.5, meanonly
                local median_inglabo_anual = r(min)

                * Mediana mensual equivalente
                local median_inglabo_mensual = `median_inglabo_anual' / 12

                * Promedio y mediana de ingreso laboral por hora
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
                    (`t') ///
                    (`f') ///
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
    }
}

postclose `memhold'


*===================================================
* 10. RECONSTRUIR DESCRIPCIONES
*===================================================

use `resultados', clear

gen str120 desc_seccion = ""

replace desc_seccion = "Agricultura, ganadería, caza, silvicultura y pesca" if seccion == "A"
replace desc_seccion = "Explotación de minas y canteras" if seccion == "B"
replace desc_seccion = "Industrias manufactureras" if seccion == "C"
replace desc_seccion = "Suministro de electricidad, gas, vapor y aire acondicionado" if seccion == "D"
replace desc_seccion = "Distribución de agua; evacuación y tratamiento de aguas residuales" if seccion == "E"
replace desc_seccion = "Construcción" if seccion == "F"
replace desc_seccion = "Comercio al por mayor y al por menor; reparación de vehículos" if seccion == "G"
replace desc_seccion = "Transporte y almacenamiento" if seccion == "H"
replace desc_seccion = "Alojamiento y servicios de comida" if seccion == "I"
replace desc_seccion = "Información y comunicaciones" if seccion == "J"
replace desc_seccion = "Actividades financieras y de seguros" if seccion == "K"
replace desc_seccion = "Actividades inmobiliarias" if seccion == "L"
replace desc_seccion = "Actividades profesionales, científicas y técnicas" if seccion == "M"
replace desc_seccion = "Actividades de servicios administrativos y de apoyo" if seccion == "N"
replace desc_seccion = "Administración pública y defensa; seguridad social obligatoria" if seccion == "O"
replace desc_seccion = "Educación" if seccion == "P"
replace desc_seccion = "Actividades de atención de la salud humana y de asistencia social" if seccion == "Q"
replace desc_seccion = "Actividades artísticas, de entretenimiento y recreación" if seccion == "R"
replace desc_seccion = "Otras actividades de servicios" if seccion == "S"
replace desc_seccion = "Actividades de los hogares como empleadores" if seccion == "T"
replace desc_seccion = "Actividades de organizaciones y entidades extraterritoriales" if seccion == "U"


gen str40 tamano_empresa = ""

replace tamano_empresa = "Trabaja solo" if tamano_cod == 1
replace tamano_empresa = "2 a 3 personas" if tamano_cod == 2
replace tamano_empresa = "4 a 5 personas" if tamano_cod == 3
replace tamano_empresa = "6 a 10 personas" if tamano_cod == 4
replace tamano_empresa = "11 a 19 personas" if tamano_cod == 5
replace tamano_empresa = "20 a 30 personas" if tamano_cod == 6
replace tamano_empresa = "31 a 50 personas" if tamano_cod == 7
replace tamano_empresa = "51 a 100 personas" if tamano_cod == 8
replace tamano_empresa = "101 a 200 personas" if tamano_cod == 9
replace tamano_empresa = "201 o más personas" if tamano_cod == 10
replace tamano_empresa = "Sin información" if tamano_cod == 99


gen str50 formalidad = ""

replace formalidad = "Formal: salud y pensión" if formalidad_cod == 1
replace formalidad = "Pensionado ocupado afiliado a salud" if formalidad_cod == 2
replace formalidad = "Informal / no cumple doble condición" if formalidad_cod == 3


*===================================================
* 11. ORGANIZAR RESULTADOS
*===================================================

sort seccion tamano_cod formalidad_cod

order seccion desc_seccion ///
      tamano_cod tamano_empresa ///
      formalidad_cod formalidad ///
      trabajadores ///
      mean_inglabo_anual mean_inglabo_mensual ///
      median_inglabo_anual median_inglabo_mensual ///
      mean_inglabo_hora median_inglabo_hora ///
      total_inglabo_anual n_obs

label variable seccion "Sección"
label variable desc_seccion "Descripción de sección"
label variable tamano_cod "Código tamaño empresa P3069"
label variable tamano_empresa "Tamaño empresa P3069"
label variable formalidad_cod "Código formalidad laboral"
label variable formalidad "Formalidad laboral"
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
* 12. EXPORTAR A EXCEL
*===================================================

save "Outputs/tables/INGLABO_seccion_tamano_formalidad_2025.dta", replace

export excel using "`archivo'", ///
    sheet("todas_secciones") ///
    firstrow(varlabels) ///
    replace

levelsof seccion, local(lista_sec_export)

foreach s of local lista_sec_export {

    preserve

        keep if seccion == "`s'"
        sort tamano_cod formalidad_cod

        export excel using "`archivo'", ///
            sheet("seccion_`s'") ///
            firstrow(varlabels) ///
            sheetreplace

    restore
}

di "===================================================="
di "Archivo exportado correctamente:"
di "`archivo'"
di ""
di "Nivel de agregación:"
di "Sección x Tamaño empresa P3069 x Formalidad laboral"
di "===================================================="