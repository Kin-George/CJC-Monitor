****************************************************
* INGRESO LABORAL ANUAL POR TRABAJADOR - GEIH 2024
* TODOS LOS SECTORES
* CIIU REV. 4: 2 dígitos y 3 dígitos
****************************************************

cd "C:/Users/jorge/Documents/Databases/GEIH"

use "GEIH_2024_TOTAL.dta", clear

* Solo ocupados
keep if OCI == 1

* Solo empresas de 10 o más trabajadores
keep if P3069 >= 5

*===================================================
* 0. DEFINIR FACTOR DE EXPANSION
*===================================================

gen FEX = FEX_C18/12
local fac "FEX"

*===================================================
* 1. ASEGURAR VARIABLES NUMERICAS
*===================================================

destring INGLABO, replace force
destring RAMA2D_R4, replace force
destring RAMA4D_R4, replace force

*===================================================
* 2. CREAR CIIU A 3 DIGITOS DESDE RAMA4D_R4
*===================================================

gen RAMA3D_R4 = floor(RAMA4D_R4/10)

*===================================================
* 3. EXCLUIR MISSINGS Y VALORES NO VALIDOS
*===================================================

keep if !missing(INGLABO)
keep if INGLABO > 0
keep if !missing(`fac')
keep if `fac' > 0
keep if !missing(RAMA2D_R4)
keep if !missing(RAMA3D_R4)

*===================================================
* 4. INGRESO LABORAL ANUAL
*===================================================

gen INGLABO_ANUAL = INGLABO * 12

tempfile base_trabajo
save `base_trabajo', replace


****************************************************
* RESULTADOS POR CIIU 2 DIGITOS: RAMA2D_R4
****************************************************

use `base_trabajo', clear

tempname memhold1

postfile `memhold1' ///
    int RAMA2D_R4 ///
    double trabajadores ///
    double total_inglabo_anual ///
    double ingreso_anual_por_trabajador ///
    double mean_inglabo_anual ///
    double median_inglabo_anual ///
    using resultados_ciiu_2d_tmp.dta, replace

levelsof RAMA2D_R4, local(lista_2d)

foreach c of local lista_2d {

    preserve

        keep if RAMA2D_R4 == `c'

        * Trabajadores expandidos
        quietly summarize `fac'
        local trabajadores = r(sum)

        * Total ingreso laboral anual expandido
        gen ingreso_anual_expandido = INGLABO_ANUAL * `fac'
        quietly summarize ingreso_anual_expandido
        local total_inglabo_anual = r(sum)

        * Ingreso laboral anual por trabajador
        local ingreso_pt_anual = `total_inglabo_anual' / `trabajadores'

        * Promedio ponderado
        quietly mean INGLABO_ANUAL [pw=`fac']
        matrix M = e(b)
        local media_anual = M[1,1]

        * Mediana ponderada
        sort INGLABO_ANUAL
        gen peso = `fac'
        egen peso_total = total(peso)
        gen peso_acum = sum(peso)
        gen frac_acum = peso_acum / peso_total

        quietly summarize INGLABO_ANUAL if frac_acum >= 0.5, meanonly
        local mediana_anual = r(min)

        post `memhold1' ///
            (`c') ///
            (`trabajadores') ///
            (`total_inglabo_anual') ///
            (`ingreso_pt_anual') ///
            (`media_anual') ///
            (`mediana_anual')

    restore
}

postclose `memhold1'

use resultados_ciiu_2d_tmp.dta, clear
sort RAMA2D_R4
save resultados_ciiu_2d.dta, replace


****************************************************
* RESULTADOS POR CIIU 3 DIGITOS: RAMA3D_R4
****************************************************

use `base_trabajo', clear

tempname memhold2

postfile `memhold2' ///
    int RAMA3D_R4 ///
    double trabajadores ///
    double total_inglabo_anual ///
    double ingreso_anual_por_trabajador ///
    double mean_inglabo_anual ///
    double median_inglabo_anual ///
    using resultados_ciiu_3d_tmp.dta, replace

levelsof RAMA3D_R4, local(lista_3d)

foreach c of local lista_3d {

    preserve

        keep if RAMA3D_R4 == `c'

        * Trabajadores expandidos
        quietly summarize `fac'
        local trabajadores = r(sum)

        * Total ingreso laboral anual expandido
        gen ingreso_anual_expandido = INGLABO_ANUAL * `fac'
        quietly summarize ingreso_anual_expandido
        local total_inglabo_anual = r(sum)

        * Ingreso laboral anual por trabajador
        local ingreso_pt_anual = `total_inglabo_anual' / `trabajadores'

        * Promedio ponderado
        quietly mean INGLABO_ANUAL [pw=`fac']
        matrix M = e(b)
        local media_anual = M[1,1]

        * Mediana ponderada
        sort INGLABO_ANUAL
        gen peso = `fac'
        egen peso_total = total(peso)
        gen peso_acum = sum(peso)
        gen frac_acum = peso_acum / peso_total

        quietly summarize INGLABO_ANUAL if frac_acum >= 0.5, meanonly
        local mediana_anual = r(min)

        post `memhold2' ///
            (`c') ///
            (`trabajadores') ///
            (`total_inglabo_anual') ///
            (`ingreso_pt_anual') ///
            (`media_anual') ///
            (`mediana_anual')

    restore
}

postclose `memhold2'

use resultados_ciiu_3d_tmp.dta, clear
sort RAMA3D_R4
save resultados_ciiu_3d.dta, replace


****************************************************
* EXPORTAR A EXCEL
****************************************************

use resultados_ciiu_2d.dta, clear

export excel using "INGLABO_ANUAL_todos_sectores_2024.xlsx", ///
    sheet("ciiu_2d") firstrow(variables) sheetreplace

use resultados_ciiu_3d.dta, clear

export excel using "INGLABO_ANUAL_todos_sectores_2024.xlsx", ///
    sheet("ciiu_3d") firstrow(variables) sheetreplace

di "========================================="
di "Archivo exportado: INGLABO_ANUAL_todos_sectores_2024.xlsx"
di "Hojas: ciiu_2d y ciiu_3d"
di "========================================="