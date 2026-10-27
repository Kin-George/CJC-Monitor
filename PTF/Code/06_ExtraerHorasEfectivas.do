****************************************************
* PTF - extraccion de horas efectivas desde la GEIH cruda (2008-2025, incluye 2020)
*
* Por que: el DANE mide el trabajo con las HORAS EFECTIVAS de la semana pasada
* (P6850: "cuantas horas trabajo durante la semana pasada en este trabajo"),
* sumadas con el factor de expansion mensual y promediadas en los 12 meses.
* Nuestras bases usan P6800 (horas que la persona dice trabajar normalmente).
* Este script lee los archivos crudos de GEIH_<anio>_total.dta, se queda con los
* ocupados y deja, por celda anio x rama PTF x sexo x edad x educacion x
* asalariado, los totales de personas y horas (P6800 y P6850) y el ingreso laboral.
*
* Salida: PTF/Datos/Processed/GEIH_ptf_celdas_efectivas.csv
* Uso (Stata 17, desde PowerShell):
*   StataMP-64.exe /e do PTF/Code/06_ExtraerHorasEfectivas.do
****************************************************

clear all
set more off
set varabbrev off

local root "C:/Users/jorge/Documents/Trabajo-Profesional/Javeriana"
local geih "C:/Users/jorge/Documents/Databases/GEIH"
local anios 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 2020 2021 2022 2023 2024 2025
* se puede acotar desde otro do con: global ANIOS 2020  /  global SALIDA "ruta.csv"  /  global MAXMES 8
if "$ANIOS" != "" local anios $ANIOS
local salida "`root'/PTF/Datos/Processed/GEIH_ptf_celdas_efectivas.csv"
if "$SALIDA" != "" local salida "$SALIDA"

local cand_factor   "fex_c_2011 FEX_C_2011 FEX_C18 FEX_C fex_c18 fex_c"
local cand_ingreso  "INGLABO inglabo"
local cand_sector   "RAMA2D_R4 RAMA2D rama2d_r4 rama2d"
* CIIU Rev. 3 (nativa hasta 2019): se usa como segunda clasificacion para probar si la ruptura Rev.3 -> Rev.4 de 2015 afecta el indice de trabajo
local cand_sector3  "RAMA2D rama2d RAMA2D_R3 rama2d_r3"
local cand_ocupado  "OCI oci"
local cand_horas    "P6800 p6800"
local cand_horas_ef "P6850 p6850"
local cand_sexo     "P6020 SEXO sexo p6020 P3271 p3271"
local cand_edad     "P6040 EDAD p6040 edad"
local cand_posicion "P6430 p6430"
* 2020: P6210 trae el grado y NIVEL_MAS_ALTO el nivel (etiquetas cruzadas en el archivo crudo)
local cand_educ     "P6210 P3042 p6210 p3042 NIVEL_MAS_ALTO"
local cand_educ_2020 "NIVEL_MAS_ALTO"
local cand_mes      "MES mes"
* 2020: usar el factor del marco 2018, igual que 2021 en adelante
local cand_factor_2020 "FEX_C18 fex_c18"

capture program drop find_first_var
program define find_first_var, rclass
    syntax, CANDIDATES(string)
    return local var ""
    ds
    local allvars `r(varlist)'
    foreach cand of local candidates {
        local cand_upper = upper(strtrim("`cand'"))
        foreach v of local allvars {
            if upper("`v'") == "`cand_upper'" {
                return local var "`v'"
                exit
            }
        }
    }
end

capture program drop to_double
program define to_double
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

capture program drop map_rama
program define map_rama
    * map_rama, SRC(variable de CIIU a 2 digitos) REV(3|4) GEN(nueva variable con la rama PTF 1-9)
    syntax, SRC(string) REV(integer) GEN(name)
    tempvar sh
    gen byte `sh' = .
    if `rev' == 4 {
        replace `sh' = 1  if inrange(`src', 1, 3)
        replace `sh' = 2  if inrange(`src', 5, 9)
        replace `sh' = 3  if inrange(`src', 10, 33)
        replace `sh' = 4  if inrange(`src', 35, 39)
        replace `sh' = 5  if inrange(`src', 41, 43)
        replace `sh' = 6  if inrange(`src', 45, 47)
        replace `sh' = 7  if inrange(`src', 55, 56)
        replace `sh' = 8  if inrange(`src', 49, 53)
        replace `sh' = 9  if inrange(`src', 58, 63)
        replace `sh' = 10 if inrange(`src', 64, 66)
        replace `sh' = 11 if `src' == 68 | inrange(`src', 69, 75) | inrange(`src', 77, 82)
        replace `sh' = 12 if `src' == 84
        replace `sh' = 13 if `src' == 85
        replace `sh' = 14 if inrange(`src', 86, 88)
        replace `sh' = 15 if inrange(`src', 90, 96)
        replace `sh' = 16 if inrange(`src', 97, 98)
        replace `sh' = 17 if `src' == 99
    }
    else {
        replace `sh' = 1  if inlist(`src', 1, 2, 5)
        replace `sh' = 2  if inrange(`src', 10, 14)
        replace `sh' = 3  if inrange(`src', 15, 37)
        replace `sh' = 4  if inlist(`src', 40, 41, 90)
        replace `sh' = 5  if `src' == 45
        replace `sh' = 6  if inrange(`src', 50, 52)
        replace `sh' = 7  if `src' == 55
        replace `sh' = 8  if inrange(`src', 60, 63)
        replace `sh' = 9  if inlist(`src', 64, 72)
        replace `sh' = 10 if inrange(`src', 65, 67)
        replace `sh' = 11 if inlist(`src', 70, 71, 73, 74)
        replace `sh' = 12 if `src' == 75
        replace `sh' = 13 if `src' == 80
        replace `sh' = 14 if `src' == 85
        replace `sh' = 15 if inrange(`src', 91, 93)
        replace `sh' = 16 if inrange(`src', 95, 97)
        replace `sh' = 17 if `src' == 99
    }
    capture drop `gen'
    gen byte `gen' = 0
    replace `gen' = 1 if `sh' == 1
    replace `gen' = 2 if `sh' == 2
    replace `gen' = 3 if `sh' == 3
    replace `gen' = 4 if `sh' == 4
    replace `gen' = 5 if `sh' == 5
    replace `gen' = 6 if inlist(`sh', 6, 7)
    replace `gen' = 7 if inlist(`sh', 8, 9)
    replace `gen' = 8 if inlist(`sh', 10, 11)
    replace `gen' = 9 if inrange(`sh', 12, 17)
end

tempfile master one
local first = 1

foreach year of local anios {
    di "=== `year'"
    local archivo "`geih'/GEIH_`year'_total.dta"
    capture confirm file "`archivo'"
    if _rc {
        di as error "No existe `archivo'"
        continue
    }
    quietly use "`archivo'" in 1, clear

    local nombres ""
    foreach k in factor ingreso sector sector3 ocupado horas horas_ef sexo edad posicion mes {
        if `year' == 2020 & "`k'" == "factor" {
            find_first_var, candidates("`cand_factor_2020'")
        }
        else {
            find_first_var, candidates("`cand_`k''")
        }
        local var_`k' "`r(var)'"
    }
    if `year' == 2020 {
        find_first_var, candidates("`cand_educ_2020'")
    }
    else {
        find_first_var, candidates("`cand_educ'")
    }
    local var_educ "`r(var)'"
    di "vars: factor=`var_factor' ingreso=`var_ingreso' sector=`var_sector' oci=`var_ocupado' h6800=`var_horas' h6850=`var_horas_ef' sexo=`var_sexo' edad=`var_edad' educ=`var_educ' pos=`var_posicion' mes=`var_mes'"

    local cargar ""
    foreach v in `var_factor' `var_ingreso' `var_sector' `var_sector3' `var_ocupado' `var_horas' `var_horas_ef' `var_sexo' `var_edad' `var_educ' `var_posicion' `var_mes' {
        if "`v'" != "" local cargar "`cargar' `v'"
    }
    use `cargar' using "`archivo'", clear

    to_double, newname(fex) varname("`var_factor'")
    to_double, newname(inglabo) varname("`var_ingreso'")
    to_double, newname(sector_cod) varname("`var_sector'")
    if "`var_sector3'" != "" {
        to_double, newname(sector3_cod) varname("`var_sector3'")
    }
    else {
        gen double sector3_cod = .
    }
    to_double, newname(oci) varname("`var_ocupado'")
    to_double, newname(h6800) varname("`var_horas'")
    to_double, newname(h6850) varname("`var_horas_ef'")
    to_double, newname(sexo_cod) varname("`var_sexo'")
    to_double, newname(edad) varname("`var_edad'")
    to_double, newname(educ_cod) varname("`var_educ'")
    to_double, newname(pos) varname("`var_posicion'")
    to_double, newname(mes_n) varname("`var_mes'")

    keep if oci == 1 & fex > 0 & !missing(fex)
    * se puede acotar a los primeros meses del anio (comparar enero-agosto de dos anios) con: global MAXMES 8
    if "$MAXMES" != "" keep if mes_n <= $MAXMES

    * algunos archivos crudos no traen ocupados en los 12 meses (2020 solo tiene 8): se promedia sobre los meses presentes
    local nmes_div 12
    if "`var_mes'" != "" {
        quietly levelsof `var_mes', local(ml)
        local nmes_div : word count `ml'
        if `nmes_div' > 12 local nmes_div 12
        di "meses con ocupados en el archivo: `nmes_div'"
    }
    keep fex inglabo sector_cod sector3_cod h6800 h6850 sexo_cod edad educ_cod pos
    gen int anio = `year'
    gen double w = fex / `nmes_div'

    * horas validas
    replace h6800 = . if h6800 <= 0 | h6800 > 168
    replace h6850 = . if h6850 < 0 | h6850 > 168
    gen byte ok6800 = !missing(h6800)
    gen byte ok6850 = !missing(h6850)
    gen byte ok_ing = !missing(inglabo) & inglabo > 0

    * sector homologado -> 9 ramas PTF (misma regla de CreacionPanelLimpio2008-2025.do)
    local rev = 3
    if upper("`var_sector'") == "RAMA2D_R4" local rev = 4
    map_rama, src(sector_cod) rev(`rev') gen(rama)
    map_rama, src(sector3_cod) rev(3) gen(rama3)
    drop if rama == 0

    * educacion con 6 niveles homologados (0 sin dato): 1 ninguno, 2 preescolar, 3 primaria, 4 secundaria, 5 media, 6 superior.
    * Los grupos de 3 niveles se arman despues (Python), asi se pueden probar distintas definiciones de "basica, media, superior".
    gen byte educ_h = 0
    if `year' >= 2021 {
        replace educ_h = educ_cod if inrange(educ_cod, 1, 4)
        replace educ_h = 5 if inlist(educ_cod, 5, 6)
        replace educ_h = 6 if inrange(educ_cod, 7, 13)
    }
    else {
        replace educ_h = educ_cod if inrange(educ_cod, 1, 6)
    }
    * edad en 3 grupos (0 = menor de 15 o sin dato)
    gen byte edad_g = 0
    replace edad_g = 1 if inrange(edad, 15, 29)
    replace edad_g = 2 if inrange(edad, 30, 49)
    replace edad_g = 3 if edad >= 50 & !missing(edad)
    gen byte sexo = 0
    replace sexo = 1 if sexo_cod == 1
    replace sexo = 2 if sexo_cod == 2
    * asalariado: 1 particular, 2 gobierno, 3 domestico y jornalero (8 hasta 2020, 7 desde 2021)
    gen byte asal = inlist(pos, 1, 2, 3)
    if `year' >= 2021 {
        replace asal = 1 if pos == 7
    }
    else {
        replace asal = 1 if pos == 8
    }

    gen double pers = w
    gen double pers6800 = w * ok6800
    gen double pers6850 = w * ok6850
    gen double H6800 = w * h6800
    gen double H6850 = w * h6850
    gen double pers_ing = w * ok_ing
    gen double ing = w * inglabo * ok_ing
    * horas efectivas ponderadas por el ingreso (para ponderadores de Tornqvist)
    gen double ing_h6850 = w * inglabo * ok_ing * ok6850

    collapse (sum) pers pers6800 pers6850 H6800 H6850 pers_ing ing ing_h6850 (count) n = w, by(anio rama rama3 sexo edad_g educ_h asal)
    save `one', replace
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

use `master', clear
export delimited using "`salida'", replace
di "Listo: " _N " celdas"
exit, clear
