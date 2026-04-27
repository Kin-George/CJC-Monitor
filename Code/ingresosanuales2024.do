****************************************************
* INGRESO LABORAL ANUAL POR TRABAJADOR EN MANUFACTURA - GEIH 2025
* SOLO POR SECTOR
* CIIU REV 4
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

* Verificacion rapida
describe RAMA2D_R4 INGLABO DPTO `fac'
destring DPTO, replace
destring INGLABO, replace

*===================================================
* 1. ASEGURAR QUE RAMA2D_R4 SEA NUMERICA
*===================================================
capture confirm numeric variable RAMA2D_R4
if _rc != 0 {
    destring RAMA2D_R4, replace force
}

*===================================================
* 2. QUEDARSE SOLO CON MANUFACTURA (CIIU REV 4: 10-33)
*===================================================
keep if inrange(RAMA2D_R4, 10, 33)

* Excluir missings y valores no válidos
keep if !missing(INGLABO)
keep if INGLABO > 0
keep if !missing(`fac')
keep if `fac' > 0

*===================================================
* 3. CONSTRUIR INGRESO LABORAL ANUAL
*===================================================
gen INGLABO_ANUAL = INGLABO * 12

*===================================================
* 4. ETIQUETAS DE RAMA2D_R4 (MANUFACTURA REV 4)
*===================================================
gen CIIU_NOMBRE = ""

replace CIIU_NOMBRE = "10. Alimentos" if RAMA2D_R4 == 10
replace CIIU_NOMBRE = "11. Bebidas" if RAMA2D_R4 == 11
replace CIIU_NOMBRE = "12. Tabaco" if RAMA2D_R4 == 12
replace CIIU_NOMBRE = "13. Textiles" if RAMA2D_R4 == 13
replace CIIU_NOMBRE = "14. Confecciones" if RAMA2D_R4 == 14
replace CIIU_NOMBRE = "15. Cuero y calzado" if RAMA2D_R4 == 15
replace CIIU_NOMBRE = "16. Madera y corcho" if RAMA2D_R4 == 16
replace CIIU_NOMBRE = "17. Papel y carton" if RAMA2D_R4 == 17
replace CIIU_NOMBRE = "18. Impresion" if RAMA2D_R4 == 18
replace CIIU_NOMBRE = "19. Refinacion de petroleo" if RAMA2D_R4 == 19
replace CIIU_NOMBRE = "20. Quimicos" if RAMA2D_R4 == 20
replace CIIU_NOMBRE = "21. Farmaceuticos" if RAMA2D_R4 == 21
replace CIIU_NOMBRE = "22. Caucho y plastico" if RAMA2D_R4 == 22
replace CIIU_NOMBRE = "23. Minerales no metalicos" if RAMA2D_R4 == 23
replace CIIU_NOMBRE = "24. Metalurgia basica" if RAMA2D_R4 == 24
replace CIIU_NOMBRE = "25. Productos metalicos" if RAMA2D_R4 == 25
replace CIIU_NOMBRE = "26. Informatica, electronicos y opticos" if RAMA2D_R4 == 26
replace CIIU_NOMBRE = "27. Aparatos electricos" if RAMA2D_R4 == 27
replace CIIU_NOMBRE = "28. Maquinaria y equipo" if RAMA2D_R4 == 28
replace CIIU_NOMBRE = "29. Vehiculos automotores" if RAMA2D_R4 == 29
replace CIIU_NOMBRE = "30. Otros equipos de transporte" if RAMA2D_R4 == 30
replace CIIU_NOMBRE = "31. Muebles" if RAMA2D_R4 == 31
replace CIIU_NOMBRE = "32. Otras manufactureras" if RAMA2D_R4 == 32
replace CIIU_NOMBRE = "33. Reparacion e instalacion de maquinaria" if RAMA2D_R4 == 33

*===================================================
* 5. GUARDAR BASE TEMPORAL DE TRABAJO
*===================================================
tempfile base_manuf
save `base_manuf', replace

****************************************************
* RESULTADOS POR SECTOR (CIIU)
****************************************************

use `base_manuf', clear

tempname memhold1
postfile `memhold1' int RAMA2D_R4 str60 CIIU_NOMBRE ///
    double trabajadores total_inglabo_anual ingreso_anual_por_trabajador ///
    mean_inglabo_anual median_inglabo_anual ///
    using resultados_sector_tmp.dta, replace

levelsof RAMA2D_R4, local(lista_ciiu)

foreach c of local lista_ciiu {

    preserve
        use `base_manuf', clear
        keep if RAMA2D_R4 == `c'

        * Trabajadores expandidos
        quietly summarize `fac'
        local trabajadores = r(sum)

        * Ingreso laboral anual total expandido del sector
        gen ingreso_anual_expandido = INGLABO_ANUAL * `fac'
        quietly summarize ingreso_anual_expandido
        local total_inglabo_anual = r(sum)

        * Ingreso laboral anual por trabajador
        local ingreso_pt_anual = `total_inglabo_anual' / `trabajadores'

        * Promedio ponderado anual
        quietly mean INGLABO_ANUAL [pw=`fac']
        matrix M = e(b)
        local media_anual = M[1,1]

        * Mediana ponderada anual
        sort INGLABO_ANUAL
        gen peso = `fac'
        egen peso_total = total(peso)
        gen peso_acum = sum(peso)
        gen frac_acum = peso_acum / peso_total
        quietly summarize INGLABO_ANUAL if frac_acum >= 0.5, meanonly
        local mediana_anual = r(min)

        * Nombre
        quietly levelsof CIIU_NOMBRE, local(nombre_ciiu)
        local nombre_txt "`nombre_ciiu'"

        post `memhold1' (`c') ("`nombre_txt'") ///
            (`trabajadores') (`total_inglabo_anual') (`ingreso_pt_anual') ///
            (`media_anual') (`mediana_anual')
    restore
}

postclose `memhold1'

use resultados_sector_tmp.dta, clear
sort RAMA2D_R4
save resultados_por_sector.dta, replace

****************************************************
* EXPORTAR A EXCEL
****************************************************

export excel using "INGLABO_ANUAL_manufactura_2024.xlsx", ///
    sheet("por_sector") firstrow(variables) sheetreplace

di "========================================="
di "Archivo exportado: INGLABO_ANUAL_manufactura_2024.xlsx"
di "========================================="