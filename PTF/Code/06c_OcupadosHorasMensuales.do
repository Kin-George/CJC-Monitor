****************************************************
* PTF - ocupados y horas por mes (GEIH 2025 y 2026)
*
* Para cada mes: ocupados (suma del factor de expansion de quienes tienen OCI == 1), personas de la muestra,
* horas efectivas del empleo principal (P6850) y horas efectivas del segundo empleo (P7045), expandidas.
* Sirve para (i) verificar contra los boletines del DANE (ocupados de enero, abril, junio y agosto de 2026)
* y (ii) medir la variacion anual de cada mes de enero a agosto de 2026.
*
* Entrada: GEIH_2025_total.dta y GEIH_2026_total.dta (carpeta de bases de la GEIH)
* Salida : PTF/Datos/Processed/GEIH_ocupados_horas_mensual_2025_2026.csv
* Uso    : StataMP-64.exe /e do PTF/Code/06c_OcupadosHorasMensuales.do   (se puede acotar con: global ANIOS 2023 2024  /  global SALIDA "ruta.csv")
****************************************************
clear all
set more off

local root "C:/Users/jorge/Documents/Trabajo-Profesional/Javeriana"
local geih "C:/Users/jorge/Documents/Databases/GEIH"
tempfile master
local first 1
local anios 2025 2026
if "$ANIOS" != "" local anios $ANIOS
local salida "`root'/PTF/Datos/Processed/GEIH_ocupados_horas_mensual_2025_2026.csv"
if "$SALIDA" != "" local salida "$SALIDA"
foreach year of local anios {
    use MES FEX_C18 OCI P6850 P7045 P6800 using "`geih'/GEIH_`year'_total.dta", clear
    destring MES, replace force
    foreach v in OCI P6850 P7045 P6800 {
        capture confirm string variable `v'
        if !_rc destring `v', replace force
    }
    keep if OCI == 1 & FEX_C18 > 0
    gen double ok6850 = !missing(P6850) & P6850 >= 0 & P6850 <= 168
    gen double ok7045 = !missing(P7045) & P7045 >= 0 & P7045 <= 75
    gen double ok6800 = !missing(P6800) & P6800 > 0 & P6800 <= 168
    gen double ocupados = FEX_C18
    gen double h6850 = FEX_C18 * P6850 * ok6850
    gen double h7045 = FEX_C18 * P7045 * ok7045
    gen double h6800 = FEX_C18 * P6800 * ok6800
    gen double seg_empleo = FEX_C18 * (P7045 > 0 & ok7045)
    gen double personas_con_h6850 = FEX_C18 * ok6850
    gen long muestra = 1
    collapse (sum) ocupados h6850 h7045 h6800 seg_empleo personas_con_h6850 muestra, by(MES)
    gen int anio = `year'
    if `first' == 1 {
        save `master', replace
        local first 0
    }
    else {
        append using `master'
        save `master', replace
    }
}
use `master', clear
sort anio MES
export delimited using "`salida'", replace
list, clean
exit, clear
