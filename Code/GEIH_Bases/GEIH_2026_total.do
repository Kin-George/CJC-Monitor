*==============================================================================
* GEIH_2026_total.do
* Consolida los microdatos mensuales de la GEIH 2026 (enero-agosto) en una sola base,
* con la misma estructura de GEIH_2025_total.dta:
*   Caracteristicas generales + No ocupados + Fuerza de trabajo + Ocupados + Migracion
*   (unidos 1:1 por DIRECTORIO SECUENCIA_P ORDEN HOGAR, en ese orden; quedan todas las personas
*   de Caracteristicas generales) y las variables derivadas de formalidadDANERobusto.do
*   (ANIOS, OFICIO_C8_2D, FORMAL, SALUD, PENSION, EI).
* No se usan "Datos del hogar y la vivienda", "Otras formas de trabajo" ni "Otros ingresos e impuestos",
* igual que en la base de 2025.
* Entrada : carpeta construccion, subcarpeta 2026, una subcarpeta por mes con los modulos .DTA (descarga de microdatos.dane.gov.co, catalogo 900)
* Salida  : C:/Users/jorge/Documents/Databases/GEIH/GEIH_2026_total.dta
* Uso     : StataMP-64.exe /e do Code/GEIH_Bases/GEIH_2026_total.do
*==============================================================================
clear all
set more off

local base   "C:/Users/jorge/Documents/Databases/GEIH/construccion/2026"
local salida "C:/Users/jorge/Documents/Databases/GEIH/GEIH_2026_total.dta"
local formal "C:/Users/jorge/Documents/Trabajo-Profesional/Javeriana/Code/GEIH_Bases/formalidadDANERobusto.do"
local meses  Enero Febrero Marzo Abril Mayo Junio Julio Agosto

tempfile acum t_noc t_ft t_ocu t_mig
local nm 0

foreach m of local meses {
    local ++nm
    * ---- localizar y normalizar los cinco modulos del mes
    foreach k in car noc ft ocu mig {
        if "`k'" == "car" local patron "Caracter*"
        if "`k'" == "noc" local patron "No ocupados*"
        if "`k'" == "ft"  local patron "Fuerza*"
        if "`k'" == "ocu" local patron "Ocupados*"
        if "`k'" == "mig" local patron "Migraci*"
        local lst : dir "`base'/`m'" files "`patron'"
        local n : word count `lst'
        if `n' != 1 {
            di as error "`m': el patron `patron' encontro `n' archivos"
            exit 459
        }
        local fn : word 1 of `lst'
        local fn = subinstr(`"`fn'"', `"""', "", .)
        use "`base'/`m'/`fn'", clear
        quietly compress
        capture confirm string variable HOGAR
        if !_rc destring HOGAR, replace
        foreach v in DIRECTORIO SECUENCIA_P ORDEN {
            capture confirm string variable `v'
            if !_rc destring `v', replace
        }
        if "`k'" != "car" save "`t_`k''", replace
    }
    * ---- en memoria esta Migracion (la ultima leida); volver a cargar Caracteristicas
    local lst : dir "`base'/`m'" files "Caracter*"
    local fn : word 1 of `lst'
    local fn = subinstr(`"`fn'"', `"""', "", .)
    use "`base'/`m'/`fn'", clear
    quietly compress
    capture confirm string variable HOGAR
    if !_rc destring HOGAR, replace
    foreach v in DIRECTORIO SECUENCIA_P ORDEN {
        capture confirm string variable `v'
        if !_rc destring `v', replace
    }
    local n_car = _N
    foreach k in noc ft ocu mig {
        merge 1:1 DIRECTORIO SECUENCIA_P ORDEN HOGAR using "`t_`k''", keep(master match) gen(_m_`k')
    }
    assert _N == `n_car'
    drop _m_noc _m_ft _m_ocu
    rename _m_mig _MERGE
    quietly compress
    di as result "`m': `n_car' personas"
    if `nm' > 1 append using "`acum'"
    save "`acum'", replace
}

use "`acum'", clear
sort MES DIRECTORIO SECUENCIA_P ORDEN HOGAR

* ---- variables derivadas (mismo codigo que la base de 2025)
capture drop ANIOS FORMAL SALUD PENSION EI OFICIO_C8_2D
do "`formal'"
capture drop RAMA2D_R4_num
capture drop RAMA2D_R4_str
label var _MERGE "Resultado del ultimo merge (Migracion)"

compress
label data "GEIH 2026 enero-agosto: Caracteristicas + No ocupados + Fuerza de trabajo + Ocupados + Migracion"
save "`salida'", replace

* ---- verificacion: ocupados ponderados por mes (millones)
preserve
keep if OCI == 1
collapse (sum) ocupados = FEX_C18 (count) muestra = DIRECTORIO, by(MES)
replace ocupados = ocupados / 1e6
list, clean
restore
di as result "LISTO: `salida'"
