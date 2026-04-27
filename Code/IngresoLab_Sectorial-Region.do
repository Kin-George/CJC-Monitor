cd "C:\Users\jorge\Documents\Trabajo-Profesional\Javeriana"

****************************************************
* INGRESO LABORAL EN MANUFACTURA - GEIH 2008
****************************************************
cd "C:/Users/jorge/Documents/Databases/GEIH"

use "GEIH_2008_TOTAL.dta", clear

*===================================================
* 0. DEFINIR FACTOR DE EXPANSION
* Cambia esto por el nombre correcto en tu base
* Ejemplos posibles: FEX_C, FEX_DPT, PESO_FINAL, etc.
*===================================================
gen FEX = fex_c_2011/12
local fac "FEX"

* Verificacion rapida
describe RAMA2D INGLABO DPTO `fac'
destring DPTO, replace
*===================================================
* 1. ASEGURAR QUE RAMA2D SEA NUMERICA
*===================================================
capture confirm numeric variable RAMA2D
if _rc != 0 {
    destring RAMA2D, replace force
}

*===================================================
* 2. CREAR REGION
*===================================================
gen REGION = ""

* Caribe
replace REGION = "Caribe" if inlist(DPTO, 8, 13, 20, 23, 44, 47, 70)

* Insular
replace REGION = "Insular" if DPTO == 88

* Andina
replace REGION = "Andina" if inlist(DPTO, 5, 11, 15, 17, 25, 41, 54, 63, 66, 68, 73)

* Pacifica
replace REGION = "Pacifica" if inlist(DPTO, 19, 27, 52, 76)

* Orinoquia
replace REGION = "Orinoquia" if inlist(DPTO, 50, 81, 85, 99)

* Amazonia
replace REGION = "Amazonia" if inlist(DPTO, 18, 86, 91, 94, 95, 97)

*===================================================
* 3. QUEDARSE SOLO CON MANUFACTURA (CIIU REV 3: 15-37)
*===================================================
keep if inrange(RAMA2D, 15, 37)

* Si quieres solo ocupados con ingreso laboral observado:
keep if !missing(INGLABO)
keep if !missing(`fac')
keep if `fac' > 0
keep if !missing(REGION)

*===================================================
* 4. ETIQUETAS DE RAMA2D (MANUFACTURA REV 3)
*===================================================
gen CIIU_NOMBRE = ""

replace CIIU_NOMBRE = "15. Alimentos y bebidas" if RAMA2D == 15
replace CIIU_NOMBRE = "16. Tabaco" if RAMA2D == 16
replace CIIU_NOMBRE = "17. Textiles" if RAMA2D == 17
replace CIIU_NOMBRE = "18. Confecciones" if RAMA2D == 18
replace CIIU_NOMBRE = "19. Cuero y calzado" if RAMA2D == 19
replace CIIU_NOMBRE = "20. Madera y corcho" if RAMA2D == 20
replace CIIU_NOMBRE = "21. Papel y carton" if RAMA2D == 21
replace CIIU_NOMBRE = "22. Edicion e impresion" if RAMA2D == 22
replace CIIU_NOMBRE = "23. Refinacion de petroleo" if RAMA2D == 23
replace CIIU_NOMBRE = "24. Quimicos" if RAMA2D == 24
replace CIIU_NOMBRE = "25. Caucho y plastico" if RAMA2D == 25
replace CIIU_NOMBRE = "26. Minerales no metalicos" if RAMA2D == 26
replace CIIU_NOMBRE = "27. Metalurgia basica" if RAMA2D == 27
replace CIIU_NOMBRE = "28. Productos metalicos" if RAMA2D == 28
replace CIIU_NOMBRE = "29. Maquinaria y equipo" if RAMA2D == 29
replace CIIU_NOMBRE = "30. Maquinaria de oficina e informatica" if RAMA2D == 30
replace CIIU_NOMBRE = "31. Maquinaria y aparatos electricos" if RAMA2D == 31
replace CIIU_NOMBRE = "32. Radio, TV y comunicaciones" if RAMA2D == 32
replace CIIU_NOMBRE = "33. Instrumentos medicos y precision" if RAMA2D == 33
replace CIIU_NOMBRE = "34. Vehiculos automotores" if RAMA2D == 34
replace CIIU_NOMBRE = "35. Otros equipos de transporte" if RAMA2D == 35
replace CIIU_NOMBRE = "36. Muebles y otras manufactureras" if RAMA2D == 36
replace CIIU_NOMBRE = "37. Reciclaje" if RAMA2D == 37

*===================================================
* 5. GUARDAR BASE TEMPORAL DE TRABAJO
*===================================================
tempfile base_manuf
save `base_manuf', replace

****************************************************
* HOJA 1: RESULTADOS POR SECTOR (CIIU)
****************************************************

tempname memhold1
postfile `memhold1' int RAMA2D str60 CIIU_NOMBRE ///
    double trabajadores mean_inglabo median_inglabo ///
    using resultados_sector_tmp.dta, replace

levelsof RAMA2D, local(lista_ciiu)

foreach c of local lista_ciiu {

    preserve
        use `base_manuf', clear
        keep if RAMA2D == `c'

        * trabajadores expandidos
        quietly summarize `fac'
        local trabajadores = r(sum)

        * promedio ponderado
        quietly mean INGLABO [pw=`fac']
        matrix M = e(b)
        local media = M[1,1]

        * mediana ponderada manual
        sort INGLABO
        gen peso = `fac'
        egen peso_total = total(peso)
        gen peso_acum = sum(peso)
        gen frac_acum = peso_acum / peso_total
        quietly summarize INGLABO if frac_acum >= 0.5, meanonly
        local mediana = r(min)

        * nombre
        quietly levelsof CIIU_NOMBRE, local(nombre_ciiu)
        local nombre_txt "`nombre_ciiu'"

        post `memhold1' (`c') ("`nombre_txt'") ///
            (`trabajadores') (`media') (`mediana')
    restore
}

postclose `memhold1'

use resultados_sector_tmp.dta, clear
sort RAMA2D
save resultados_por_sector.dta, replace

****************************************************
* HOJA 2: RESULTADOS POR REGION
****************************************************
use `base_manuf', clear
tempname memhold2
postfile `memhold2' str20 REGION ///
    double trabajadores mean_inglabo median_inglabo ///
    using resultados_region_tmp.dta, replace

levelsof REGION, local(lista_regiones)

foreach r of local lista_regiones {

    preserve
        use `base_manuf', clear
        keep if REGION == "`r'"

        * trabajadores expandidos
        quietly summarize `fac'
        local trabajadores = r(sum)

        * promedio ponderado
        quietly mean INGLABO [pw=`fac']
        matrix M = e(b)
        local media = M[1,1]

        * mediana ponderada manual
        sort INGLABO
        gen peso = `fac'
        egen peso_total = total(peso)
        gen peso_acum = sum(peso)
        gen frac_acum = peso_acum / peso_total
        quietly summarize INGLABO if frac_acum >= 0.5, meanonly
        local mediana = r(min)

        post `memhold2' ("`r'") (`trabajadores') (`media') (`mediana')
    restore
}

postclose `memhold2'

use resultados_region_tmp.dta, clear
sort REGION
save resultados_por_region.dta, replace

****************************************************
* EXPORTAR A EXCEL CON 2 HOJAS
****************************************************

use resultados_por_sector.dta, clear
export excel using "INGLABO_manufactura_2008.xlsx", ///
    sheet("por_sector") firstrow(variables) sheetreplace

use resultados_por_region.dta, clear
export excel using "INGLABO_manufactura_2008.xlsx", ///
    sheet("por_region") firstrow(variables) sheetmodify

di "========================================="
di "Archivo exportado: INGLABO_manufactura_2008.xlsx"
di "========================================="

****************************************************
* INGRESO LABORAL EN MANUFACTURA - GEIH 2009
****************************************************
cd "C:/Users/jorge/Documents/Databases/GEIH"

use "GEIH_2009_TOTAL.dta", clear

*===================================================
* 0. DEFINIR FACTOR DE EXPANSION
* Cambia esto por el nombre correcto en tu base
*===================================================
gen FEX = fex_c_2011/12
local fac "FEX"

* Verificacion rapida
describe RAMA2D INGLABO DPTO `fac'
destring DPTO, replace

*===================================================
* 1. ASEGURAR QUE RAMA2D SEA NUMERICA
*===================================================
capture confirm numeric variable RAMA2D
if _rc != 0 {
    destring RAMA2D, replace force
}

*===================================================
* 2. CREAR REGION
*===================================================
gen REGION = ""

* Caribe
replace REGION = "Caribe" if inlist(DPTO, 8, 13, 20, 23, 44, 47, 70)

* Insular
replace REGION = "Insular" if DPTO == 88

* Andina
replace REGION = "Andina" if inlist(DPTO, 5, 11, 15, 17, 25, 41, 54, 63, 66, 68, 73)

* Pacifica
replace REGION = "Pacifica" if inlist(DPTO, 19, 27, 52, 76)

* Orinoquia
replace REGION = "Orinoquia" if inlist(DPTO, 50, 81, 85, 99)

* Amazonia
replace REGION = "Amazonia" if inlist(DPTO, 18, 86, 91, 94, 95, 97)

*===================================================
* 3. QUEDARSE SOLO CON MANUFACTURA (CIIU REV 3: 15-37)
*===================================================
keep if inrange(RAMA2D, 15, 37)

* Solo ocupados con ingreso laboral observado
keep if !missing(INGLABO)
keep if !missing(`fac')
keep if `fac' > 0
keep if !missing(REGION)

*===================================================
* 4. ETIQUETAS DE RAMA2D (MANUFACTURA REV 3)
*===================================================
gen CIIU_NOMBRE = ""

replace CIIU_NOMBRE = "15. Alimentos y bebidas" if RAMA2D == 15
replace CIIU_NOMBRE = "16. Tabaco" if RAMA2D == 16
replace CIIU_NOMBRE = "17. Textiles" if RAMA2D == 17
replace CIIU_NOMBRE = "18. Confecciones" if RAMA2D == 18
replace CIIU_NOMBRE = "19. Cuero y calzado" if RAMA2D == 19
replace CIIU_NOMBRE = "20. Madera y corcho" if RAMA2D == 20
replace CIIU_NOMBRE = "21. Papel y carton" if RAMA2D == 21
replace CIIU_NOMBRE = "22. Edicion e impresion" if RAMA2D == 22
replace CIIU_NOMBRE = "23. Refinacion de petroleo" if RAMA2D == 23
replace CIIU_NOMBRE = "24. Quimicos" if RAMA2D == 24
replace CIIU_NOMBRE = "25. Caucho y plastico" if RAMA2D == 25
replace CIIU_NOMBRE = "26. Minerales no metalicos" if RAMA2D == 26
replace CIIU_NOMBRE = "27. Metalurgia basica" if RAMA2D == 27
replace CIIU_NOMBRE = "28. Productos metalicos" if RAMA2D == 28
replace CIIU_NOMBRE = "29. Maquinaria y equipo" if RAMA2D == 29
replace CIIU_NOMBRE = "30. Maquinaria de oficina e informatica" if RAMA2D == 30
replace CIIU_NOMBRE = "31. Maquinaria y aparatos electricos" if RAMA2D == 31
replace CIIU_NOMBRE = "32. Radio, TV y comunicaciones" if RAMA2D == 32
replace CIIU_NOMBRE = "33. Instrumentos medicos y precision" if RAMA2D == 33
replace CIIU_NOMBRE = "34. Vehiculos automotores" if RAMA2D == 34
replace CIIU_NOMBRE = "35. Otros equipos de transporte" if RAMA2D == 35
replace CIIU_NOMBRE = "36. Muebles y otras manufactureras" if RAMA2D == 36
replace CIIU_NOMBRE = "37. Reciclaje" if RAMA2D == 37

*===================================================
* 5. GUARDAR BASE TEMPORAL DE TRABAJO
*===================================================
tempfile base_manuf
save `base_manuf', replace

****************************************************
* HOJA 1: RESULTADOS POR SECTOR (CIIU)
****************************************************

use `base_manuf', clear

tempname memhold1
postfile `memhold1' int RAMA2D str60 CIIU_NOMBRE ///
    double trabajadores mean_inglabo median_inglabo ///
    using resultados_sector_tmp.dta, replace

levelsof RAMA2D, local(lista_ciiu)

foreach c of local lista_ciiu {

    preserve
        use `base_manuf', clear
        keep if RAMA2D == `c'

        * trabajadores expandidos
        quietly summarize `fac'
        local trabajadores = r(sum)

        * promedio ponderado
        quietly mean INGLABO [pw=`fac']
        matrix M = e(b)
        local media = M[1,1]

        * mediana ponderada manual
        sort INGLABO
        gen peso = `fac'
        egen peso_total = total(peso)
        gen peso_acum = sum(peso)
        gen frac_acum = peso_acum / peso_total
        quietly summarize INGLABO if frac_acum >= 0.5, meanonly
        local mediana = r(min)

        * nombre
        quietly levelsof CIIU_NOMBRE, local(nombre_ciiu)
        local nombre_txt "`nombre_ciiu'"

        post `memhold1' (`c') ("`nombre_txt'") ///
            (`trabajadores') (`media') (`mediana')
    restore
}

postclose `memhold1'

use resultados_sector_tmp.dta, clear
sort RAMA2D
save resultados_por_sector.dta, replace

****************************************************
* HOJA 2: RESULTADOS POR REGION
****************************************************

use `base_manuf', clear

tempname memhold2
postfile `memhold2' str20 REGION ///
    double trabajadores mean_inglabo median_inglabo ///
    using resultados_region_tmp.dta, replace

levelsof REGION, local(lista_regiones)

foreach r of local lista_regiones {

    preserve
        use `base_manuf', clear
        keep if REGION == "`r'"

        * trabajadores expandidos
        quietly summarize `fac'
        local trabajadores = r(sum)

        * promedio ponderado
        quietly mean INGLABO [pw=`fac']
        matrix M = e(b)
        local media = M[1,1]

        * mediana ponderada manual
        sort INGLABO
        gen peso = `fac'
        egen peso_total = total(peso)
        gen peso_acum = sum(peso)
        gen frac_acum = peso_acum / peso_total
        quietly summarize INGLABO if frac_acum >= 0.5, meanonly
        local mediana = r(min)

        post `memhold2' ("`r'") (`trabajadores') (`media') (`mediana')
    restore
}

postclose `memhold2'

use resultados_region_tmp.dta, clear
sort REGION
save resultados_por_region.dta, replace

****************************************************
* EXPORTAR A EXCEL CON 2 HOJAS
****************************************************

use resultados_por_sector.dta, clear
export excel using "INGLABO_manufactura_2009.xlsx", ///
    sheet("por_sector") firstrow(variables) sheetreplace

use resultados_por_region.dta, clear
export excel using "INGLABO_manufactura_2009.xlsx", ///
    sheet("por_region") firstrow(variables) sheetmodify

di "========================================="
di "Archivo exportado: INGLABO_manufactura_2009.xlsx"
di "========================================="

****************************************************
* INGRESO LABORAL EN MANUFACTURA - GEIH 2010
****************************************************
cd "C:/Users/jorge/Documents/Databases/GEIH"

use "GEIH_2010_TOTAL.dta", clear

*===================================================
* 0. DEFINIR FACTOR DE EXPANSION
* Cambia esto por el nombre correcto en tu base
*===================================================
gen FEX = fex_c_2011/12
local fac "FEX"

* Verificacion rapida
describe RAMA2D INGLABO DPTO `fac'
destring DPTO, replace

*===================================================
* 1. ASEGURAR QUE RAMA2D SEA NUMERICA
*===================================================
capture confirm numeric variable RAMA2D
if _rc != 0 {
    destring RAMA2D, replace force
}

*===================================================
* 2. CREAR REGION
*===================================================
gen REGION = ""

* Caribe
replace REGION = "Caribe" if inlist(DPTO, 8, 13, 20, 23, 44, 47, 70)

* Insular
replace REGION = "Insular" if DPTO == 88

* Andina
replace REGION = "Andina" if inlist(DPTO, 5, 11, 15, 17, 25, 41, 54, 63, 66, 68, 73)

* Pacifica
replace REGION = "Pacifica" if inlist(DPTO, 19, 27, 52, 76)

* Orinoquia
replace REGION = "Orinoquia" if inlist(DPTO, 50, 81, 85, 99)

* Amazonia
replace REGION = "Amazonia" if inlist(DPTO, 18, 86, 91, 94, 95, 97)

*===================================================
* 3. QUEDARSE SOLO CON MANUFACTURA (CIIU REV 3: 15-37)
*===================================================
keep if inrange(RAMA2D, 15, 37)

* Solo ocupados con ingreso laboral observado
keep if !missing(INGLABO)
keep if !missing(`fac')
keep if `fac' > 0
keep if !missing(REGION)

*===================================================
* 4. ETIQUETAS DE RAMA2D (MANUFACTURA REV 3)
*===================================================
gen CIIU_NOMBRE = ""

replace CIIU_NOMBRE = "15. Alimentos y bebidas" if RAMA2D == 15
replace CIIU_NOMBRE = "16. Tabaco" if RAMA2D == 16
replace CIIU_NOMBRE = "17. Textiles" if RAMA2D == 17
replace CIIU_NOMBRE = "18. Confecciones" if RAMA2D == 18
replace CIIU_NOMBRE = "19. Cuero y calzado" if RAMA2D == 19
replace CIIU_NOMBRE = "20. Madera y corcho" if RAMA2D == 20
replace CIIU_NOMBRE = "21. Papel y carton" if RAMA2D == 21
replace CIIU_NOMBRE = "22. Edicion e impresion" if RAMA2D == 22
replace CIIU_NOMBRE = "23. Refinacion de petroleo" if RAMA2D == 23
replace CIIU_NOMBRE = "24. Quimicos" if RAMA2D == 24
replace CIIU_NOMBRE = "25. Caucho y plastico" if RAMA2D == 25
replace CIIU_NOMBRE = "26. Minerales no metalicos" if RAMA2D == 26
replace CIIU_NOMBRE = "27. Metalurgia basica" if RAMA2D == 27
replace CIIU_NOMBRE = "28. Productos metalicos" if RAMA2D == 28
replace CIIU_NOMBRE = "29. Maquinaria y equipo" if RAMA2D == 29
replace CIIU_NOMBRE = "30. Maquinaria de oficina e informatica" if RAMA2D == 30
replace CIIU_NOMBRE = "31. Maquinaria y aparatos electricos" if RAMA2D == 31
replace CIIU_NOMBRE = "32. Radio, TV y comunicaciones" if RAMA2D == 32
replace CIIU_NOMBRE = "33. Instrumentos medicos y precision" if RAMA2D == 33
replace CIIU_NOMBRE = "34. Vehiculos automotores" if RAMA2D == 34
replace CIIU_NOMBRE = "35. Otros equipos de transporte" if RAMA2D == 35
replace CIIU_NOMBRE = "36. Muebles y otras manufactureras" if RAMA2D == 36
replace CIIU_NOMBRE = "37. Reciclaje" if RAMA2D == 37

*===================================================
* 5. GUARDAR BASE TEMPORAL DE TRABAJO
*===================================================
tempfile base_manuf
save `base_manuf', replace

****************************************************
* HOJA 1: RESULTADOS POR SECTOR (CIIU)
****************************************************

use `base_manuf', clear

tempname memhold1
postfile `memhold1' int RAMA2D str60 CIIU_NOMBRE ///
    double trabajadores mean_inglabo median_inglabo ///
    using resultados_sector_tmp.dta, replace

levelsof RAMA2D, local(lista_ciiu)

foreach c of local lista_ciiu {

    preserve
        use `base_manuf', clear
        keep if RAMA2D == `c'

        * trabajadores expandidos
        quietly summarize `fac'
        local trabajadores = r(sum)

        * promedio ponderado
        quietly mean INGLABO [pw=`fac']
        matrix M = e(b)
        local media = M[1,1]

        * mediana ponderada manual
        sort INGLABO
        gen peso = `fac'
        egen peso_total = total(peso)
        gen peso_acum = sum(peso)
        gen frac_acum = peso_acum / peso_total
        quietly summarize INGLABO if frac_acum >= 0.5, meanonly
        local mediana = r(min)

        * nombre
        quietly levelsof CIIU_NOMBRE, local(nombre_ciiu)
        local nombre_txt "`nombre_ciiu'"

        post `memhold1' (`c') ("`nombre_txt'") ///
            (`trabajadores') (`media') (`mediana')
    restore
}

postclose `memhold1'

use resultados_sector_tmp.dta, clear
sort RAMA2D
save resultados_por_sector.dta, replace

****************************************************
* HOJA 2: RESULTADOS POR REGION
****************************************************

use `base_manuf', clear

tempname memhold2
postfile `memhold2' str20 REGION ///
    double trabajadores mean_inglabo median_inglabo ///
    using resultados_region_tmp.dta, replace

levelsof REGION, local(lista_regiones)

foreach r of local lista_regiones {

    preserve
        use `base_manuf', clear
        keep if REGION == "`r'"

        * trabajadores expandidos
        quietly summarize `fac'
        local trabajadores = r(sum)

        * promedio ponderado
        quietly mean INGLABO [pw=`fac']
        matrix M = e(b)
        local media = M[1,1]

        * mediana ponderada manual
        sort INGLABO
        gen peso = `fac'
        egen peso_total = total(peso)
        gen peso_acum = sum(peso)
        gen frac_acum = peso_acum / peso_total
        quietly summarize INGLABO if frac_acum >= 0.5, meanonly
        local mediana = r(min)

        post `memhold2' ("`r'") (`trabajadores') (`media') (`mediana')
    restore
}

postclose `memhold2'

use resultados_region_tmp.dta, clear
sort REGION
save resultados_por_region.dta, replace

****************************************************
* EXPORTAR A EXCEL CON 2 HOJAS
****************************************************

use resultados_por_sector.dta, clear
export excel using "INGLABO_manufactura_2010.xlsx", ///
    sheet("por_sector") firstrow(variables) sheetreplace

use resultados_por_region.dta, clear
export excel using "INGLABO_manufactura_2010.xlsx", ///
    sheet("por_region") firstrow(variables) sheetmodify

di "========================================="
di "Archivo exportado: INGLABO_manufactura_2010.xlsx"
di "========================================="

****************************************************
* INGRESO LABORAL EN MANUFACTURA - GEIH 2011
****************************************************
cd "C:/Users/jorge/Documents/Databases/GEIH"

use "GEIH_2011_TOTAL.dta", clear

*===================================================
* 0. DEFINIR FACTOR DE EXPANSION
* Cambia esto por el nombre correcto en tu base
*===================================================
gen FEX = FEX_C_2011/12
local fac "FEX"

* Verificacion rapida
describe RAMA2D INGLABO DPTO `fac'
destring DPTO, replace

*===================================================
* 1. ASEGURAR QUE RAMA2D SEA NUMERICA
*===================================================
capture confirm numeric variable RAMA2D
if _rc != 0 {
    destring RAMA2D, replace force
}

*===================================================
* 2. CREAR REGION
*===================================================
gen REGION = ""

* Caribe
replace REGION = "Caribe" if inlist(DPTO, 8, 13, 20, 23, 44, 47, 70)

* Insular
replace REGION = "Insular" if DPTO == 88

* Andina
replace REGION = "Andina" if inlist(DPTO, 5, 11, 15, 17, 25, 41, 54, 63, 66, 68, 73)

* Pacifica
replace REGION = "Pacifica" if inlist(DPTO, 19, 27, 52, 76)

* Orinoquia
replace REGION = "Orinoquia" if inlist(DPTO, 50, 81, 85, 99)

* Amazonia
replace REGION = "Amazonia" if inlist(DPTO, 18, 86, 91, 94, 95, 97)

*===================================================
* 3. QUEDARSE SOLO CON MANUFACTURA (CIIU REV 3: 15-37)
*===================================================
keep if inrange(RAMA2D, 15, 37)

* Solo ocupados con ingreso laboral observado
keep if !missing(INGLABO)
keep if !missing(`fac')
keep if `fac' > 0
keep if !missing(REGION)

*===================================================
* 4. ETIQUETAS DE RAMA2D (MANUFACTURA REV 3)
*===================================================
gen CIIU_NOMBRE = ""

replace CIIU_NOMBRE = "15. Alimentos y bebidas" if RAMA2D == 15
replace CIIU_NOMBRE = "16. Tabaco" if RAMA2D == 16
replace CIIU_NOMBRE = "17. Textiles" if RAMA2D == 17
replace CIIU_NOMBRE = "18. Confecciones" if RAMA2D == 18
replace CIIU_NOMBRE = "19. Cuero y calzado" if RAMA2D == 19
replace CIIU_NOMBRE = "20. Madera y corcho" if RAMA2D == 20
replace CIIU_NOMBRE = "21. Papel y carton" if RAMA2D == 21
replace CIIU_NOMBRE = "22. Edicion e impresion" if RAMA2D == 22
replace CIIU_NOMBRE = "23. Refinacion de petroleo" if RAMA2D == 23
replace CIIU_NOMBRE = "24. Quimicos" if RAMA2D == 24
replace CIIU_NOMBRE = "25. Caucho y plastico" if RAMA2D == 25
replace CIIU_NOMBRE = "26. Minerales no metalicos" if RAMA2D == 26
replace CIIU_NOMBRE = "27. Metalurgia basica" if RAMA2D == 27
replace CIIU_NOMBRE = "28. Productos metalicos" if RAMA2D == 28
replace CIIU_NOMBRE = "29. Maquinaria y equipo" if RAMA2D == 29
replace CIIU_NOMBRE = "30. Maquinaria de oficina e informatica" if RAMA2D == 30
replace CIIU_NOMBRE = "31. Maquinaria y aparatos electricos" if RAMA2D == 31
replace CIIU_NOMBRE = "32. Radio, TV y comunicaciones" if RAMA2D == 32
replace CIIU_NOMBRE = "33. Instrumentos medicos y precision" if RAMA2D == 33
replace CIIU_NOMBRE = "34. Vehiculos automotores" if RAMA2D == 34
replace CIIU_NOMBRE = "35. Otros equipos de transporte" if RAMA2D == 35
replace CIIU_NOMBRE = "36. Muebles y otras manufactureras" if RAMA2D == 36
replace CIIU_NOMBRE = "37. Reciclaje" if RAMA2D == 37

*===================================================
* 5. GUARDAR BASE TEMPORAL DE TRABAJO
*===================================================
tempfile base_manuf
save `base_manuf', replace

****************************************************
* HOJA 1: RESULTADOS POR SECTOR (CIIU)
****************************************************

use `base_manuf', clear

tempname memhold1
postfile `memhold1' int RAMA2D str60 CIIU_NOMBRE ///
    double trabajadores mean_inglabo median_inglabo ///
    using resultados_sector_tmp.dta, replace

levelsof RAMA2D, local(lista_ciiu)

foreach c of local lista_ciiu {

    preserve
        use `base_manuf', clear
        keep if RAMA2D == `c'

        * trabajadores expandidos
        quietly summarize `fac'
        local trabajadores = r(sum)

        * promedio ponderado
        quietly mean INGLABO [pw=`fac']
        matrix M = e(b)
        local media = M[1,1]

        * mediana ponderada manual
        sort INGLABO
        gen peso = `fac'
        egen peso_total = total(peso)
        gen peso_acum = sum(peso)
        gen frac_acum = peso_acum / peso_total
        quietly summarize INGLABO if frac_acum >= 0.5, meanonly
        local mediana = r(min)

        * nombre
        quietly levelsof CIIU_NOMBRE, local(nombre_ciiu)
        local nombre_txt "`nombre_ciiu'"

        post `memhold1' (`c') ("`nombre_txt'") ///
            (`trabajadores') (`media') (`mediana')
    restore
}

postclose `memhold1'

use resultados_sector_tmp.dta, clear
sort RAMA2D
save resultados_por_sector.dta, replace

****************************************************
* HOJA 2: RESULTADOS POR REGION
****************************************************

use `base_manuf', clear

tempname memhold2
postfile `memhold2' str20 REGION ///
    double trabajadores mean_inglabo median_inglabo ///
    using resultados_region_tmp.dta, replace

levelsof REGION, local(lista_regiones)

foreach r of local lista_regiones {

    preserve
        use `base_manuf', clear
        keep if REGION == "`r'"

        * trabajadores expandidos
        quietly summarize `fac'
        local trabajadores = r(sum)

        * promedio ponderado
        quietly mean INGLABO [pw=`fac']
        matrix M = e(b)
        local media = M[1,1]

        * mediana ponderada manual
        sort INGLABO
        gen peso = `fac'
        egen peso_total = total(peso)
        gen peso_acum = sum(peso)
        gen frac_acum = peso_acum / peso_total
        quietly summarize INGLABO if frac_acum >= 0.5, meanonly
        local mediana = r(min)

        post `memhold2' ("`r'") (`trabajadores') (`media') (`mediana')
    restore
}

postclose `memhold2'

use resultados_region_tmp.dta, clear
sort REGION
save resultados_por_region.dta, replace

****************************************************
* EXPORTAR A EXCEL CON 2 HOJAS
****************************************************

use resultados_por_sector.dta, clear
export excel using "INGLABO_manufactura_2011.xlsx", ///
    sheet("por_sector") firstrow(variables) sheetreplace

use resultados_por_region.dta, clear
export excel using "INGLABO_manufactura_2011.xlsx", ///
    sheet("por_region") firstrow(variables) sheetmodify

di "========================================="
di "Archivo exportado: INGLABO_manufactura_2011.xlsx"
di "========================================="

****************************************************
* INGRESO LABORAL EN MANUFACTURA - GEIH 2012
****************************************************
cd "C:/Users/jorge/Documents/Databases/GEIH"

use "GEIH_2012_TOTAL.dta", clear

*===================================================
* 0. DEFINIR FACTOR DE EXPANSION
* Cambia esto por el nombre correcto en tu base
*===================================================
gen FEX = FEX_C_2011/12
local fac "FEX"

* Verificacion rapida
describe RAMA2D INGLABO DPTO `fac'
destring DPTO, replace

*===================================================
* 1. ASEGURAR QUE RAMA2D SEA NUMERICA
*===================================================
capture confirm numeric variable RAMA2D
if _rc != 0 {
    destring RAMA2D, replace force
}

*===================================================
* 2. CREAR REGION
*===================================================
gen REGION = ""

* Caribe
replace REGION = "Caribe" if inlist(DPTO, 8, 13, 20, 23, 44, 47, 70)

* Insular
replace REGION = "Insular" if DPTO == 88

* Andina
replace REGION = "Andina" if inlist(DPTO, 5, 11, 15, 17, 25, 41, 54, 63, 66, 68, 73)

* Pacifica
replace REGION = "Pacifica" if inlist(DPTO, 19, 27, 52, 76)

* Orinoquia
replace REGION = "Orinoquia" if inlist(DPTO, 50, 81, 85, 99)

* Amazonia
replace REGION = "Amazonia" if inlist(DPTO, 18, 86, 91, 94, 95, 97)

*===================================================
* 3. QUEDARSE SOLO CON MANUFACTURA (CIIU REV 3: 15-37)
*===================================================
keep if inrange(RAMA2D, 15, 37)

* Solo ocupados con ingreso laboral observado
keep if !missing(INGLABO)
keep if !missing(`fac')
keep if `fac' > 0
keep if !missing(REGION)

*===================================================
* 4. ETIQUETAS DE RAMA2D (MANUFACTURA REV 3)
*===================================================
gen CIIU_NOMBRE = ""

replace CIIU_NOMBRE = "15. Alimentos y bebidas" if RAMA2D == 15
replace CIIU_NOMBRE = "16. Tabaco" if RAMA2D == 16
replace CIIU_NOMBRE = "17. Textiles" if RAMA2D == 17
replace CIIU_NOMBRE = "18. Confecciones" if RAMA2D == 18
replace CIIU_NOMBRE = "19. Cuero y calzado" if RAMA2D == 19
replace CIIU_NOMBRE = "20. Madera y corcho" if RAMA2D == 20
replace CIIU_NOMBRE = "21. Papel y carton" if RAMA2D == 21
replace CIIU_NOMBRE = "22. Edicion e impresion" if RAMA2D == 22
replace CIIU_NOMBRE = "23. Refinacion de petroleo" if RAMA2D == 23
replace CIIU_NOMBRE = "24. Quimicos" if RAMA2D == 24
replace CIIU_NOMBRE = "25. Caucho y plastico" if RAMA2D == 25
replace CIIU_NOMBRE = "26. Minerales no metalicos" if RAMA2D == 26
replace CIIU_NOMBRE = "27. Metalurgia basica" if RAMA2D == 27
replace CIIU_NOMBRE = "28. Productos metalicos" if RAMA2D == 28
replace CIIU_NOMBRE = "29. Maquinaria y equipo" if RAMA2D == 29
replace CIIU_NOMBRE = "30. Maquinaria de oficina e informatica" if RAMA2D == 30
replace CIIU_NOMBRE = "31. Maquinaria y aparatos electricos" if RAMA2D == 31
replace CIIU_NOMBRE = "32. Radio, TV y comunicaciones" if RAMA2D == 32
replace CIIU_NOMBRE = "33. Instrumentos medicos y precision" if RAMA2D == 33
replace CIIU_NOMBRE = "34. Vehiculos automotores" if RAMA2D == 34
replace CIIU_NOMBRE = "35. Otros equipos de transporte" if RAMA2D == 35
replace CIIU_NOMBRE = "36. Muebles y otras manufactureras" if RAMA2D == 36
replace CIIU_NOMBRE = "37. Reciclaje" if RAMA2D == 37

*===================================================
* 5. GUARDAR BASE TEMPORAL DE TRABAJO
*===================================================
tempfile base_manuf
save `base_manuf', replace

****************************************************
* HOJA 1: RESULTADOS POR SECTOR (CIIU)
****************************************************

use `base_manuf', clear

tempname memhold1
postfile `memhold1' int RAMA2D str60 CIIU_NOMBRE ///
    double trabajadores mean_inglabo median_inglabo ///
    using resultados_sector_tmp.dta, replace

levelsof RAMA2D, local(lista_ciiu)

foreach c of local lista_ciiu {

    preserve
        use `base_manuf', clear
        keep if RAMA2D == `c'

        * trabajadores expandidos
        quietly summarize `fac'
        local trabajadores = r(sum)

        * promedio ponderado
        quietly mean INGLABO [pw=`fac']
        matrix M = e(b)
        local media = M[1,1]

        * mediana ponderada manual
        sort INGLABO
        gen peso = `fac'
        egen peso_total = total(peso)
        gen peso_acum = sum(peso)
        gen frac_acum = peso_acum / peso_total
        quietly summarize INGLABO if frac_acum >= 0.5, meanonly
        local mediana = r(min)

        * nombre
        quietly levelsof CIIU_NOMBRE, local(nombre_ciiu)
        local nombre_txt "`nombre_ciiu'"

        post `memhold1' (`c') ("`nombre_txt'") ///
            (`trabajadores') (`media') (`mediana')
    restore
}

postclose `memhold1'

use resultados_sector_tmp.dta, clear
sort RAMA2D
save resultados_por_sector.dta, replace

****************************************************
* HOJA 2: RESULTADOS POR REGION
****************************************************

use `base_manuf', clear

tempname memhold2
postfile `memhold2' str20 REGION ///
    double trabajadores mean_inglabo median_inglabo ///
    using resultados_region_tmp.dta, replace

levelsof REGION, local(lista_regiones)

foreach r of local lista_regiones {

    preserve
        use `base_manuf', clear
        keep if REGION == "`r'"

        * trabajadores expandidos
        quietly summarize `fac'
        local trabajadores = r(sum)

        * promedio ponderado
        quietly mean INGLABO [pw=`fac']
        matrix M = e(b)
        local media = M[1,1]

        * mediana ponderada manual
        sort INGLABO
        gen peso = `fac'
        egen peso_total = total(peso)
        gen peso_acum = sum(peso)
        gen frac_acum = peso_acum / peso_total
        quietly summarize INGLABO if frac_acum >= 0.5, meanonly
        local mediana = r(min)

        post `memhold2' ("`r'") (`trabajadores') (`media') (`mediana')
    restore
}

postclose `memhold2'

use resultados_region_tmp.dta, clear
sort REGION
save resultados_por_region.dta, replace

****************************************************
* EXPORTAR A EXCEL CON 2 HOJAS
****************************************************

use resultados_por_sector.dta, clear
export excel using "INGLABO_manufactura_2012.xlsx", ///
    sheet("por_sector") firstrow(variables) sheetreplace

use resultados_por_region.dta, clear
export excel using "INGLABO_manufactura_2012.xlsx", ///
    sheet("por_region") firstrow(variables) sheetmodify

di "========================================="
di "Archivo exportado: INGLABO_manufactura_2012.xlsx"
di "========================================="

****************************************************
* INGRESO LABORAL EN MANUFACTURA - GEIH 2013
****************************************************
cd "C:/Users/jorge/Documents/Databases/GEIH"

use "GEIH_2013_TOTAL.dta", clear

*===================================================
* 0. DEFINIR FACTOR DE EXPANSION
* Cambia esto por el nombre correcto en tu base
*===================================================
gen FEX = FEX_C_2011/12
local fac "FEX"

* Verificacion rapida
describe RAMA2D INGLABO DPTO `fac'
destring DPTO, replace

*===================================================
* 1. ASEGURAR QUE RAMA2D SEA NUMERICA
*===================================================
capture confirm numeric variable RAMA2D
if _rc != 0 {
    destring RAMA2D, replace force
}

*===================================================
* 2. CREAR REGION
*===================================================
gen REGION = ""

* Caribe
replace REGION = "Caribe" if inlist(DPTO, 8, 13, 20, 23, 44, 47, 70)

* Insular
replace REGION = "Insular" if DPTO == 88

* Andina
replace REGION = "Andina" if inlist(DPTO, 5, 11, 15, 17, 25, 41, 54, 63, 66, 68, 73)

* Pacifica
replace REGION = "Pacifica" if inlist(DPTO, 19, 27, 52, 76)

* Orinoquia
replace REGION = "Orinoquia" if inlist(DPTO, 50, 81, 85, 99)

* Amazonia
replace REGION = "Amazonia" if inlist(DPTO, 18, 86, 91, 94, 95, 97)

*===================================================
* 3. QUEDARSE SOLO CON MANUFACTURA (CIIU REV 3: 15-37)
*===================================================
keep if inrange(RAMA2D, 15, 37)

* Solo ocupados con ingreso laboral observado
keep if !missing(INGLABO)
keep if !missing(`fac')
keep if `fac' > 0
keep if !missing(REGION)

*===================================================
* 4. ETIQUETAS DE RAMA2D (MANUFACTURA REV 3)
*===================================================
gen CIIU_NOMBRE = ""

replace CIIU_NOMBRE = "15. Alimentos y bebidas" if RAMA2D == 15
replace CIIU_NOMBRE = "16. Tabaco" if RAMA2D == 16
replace CIIU_NOMBRE = "17. Textiles" if RAMA2D == 17
replace CIIU_NOMBRE = "18. Confecciones" if RAMA2D == 18
replace CIIU_NOMBRE = "19. Cuero y calzado" if RAMA2D == 19
replace CIIU_NOMBRE = "20. Madera y corcho" if RAMA2D == 20
replace CIIU_NOMBRE = "21. Papel y carton" if RAMA2D == 21
replace CIIU_NOMBRE = "22. Edicion e impresion" if RAMA2D == 22
replace CIIU_NOMBRE = "23. Refinacion de petroleo" if RAMA2D == 23
replace CIIU_NOMBRE = "24. Quimicos" if RAMA2D == 24
replace CIIU_NOMBRE = "25. Caucho y plastico" if RAMA2D == 25
replace CIIU_NOMBRE = "26. Minerales no metalicos" if RAMA2D == 26
replace CIIU_NOMBRE = "27. Metalurgia basica" if RAMA2D == 27
replace CIIU_NOMBRE = "28. Productos metalicos" if RAMA2D == 28
replace CIIU_NOMBRE = "29. Maquinaria y equipo" if RAMA2D == 29
replace CIIU_NOMBRE = "30. Maquinaria de oficina e informatica" if RAMA2D == 30
replace CIIU_NOMBRE = "31. Maquinaria y aparatos electricos" if RAMA2D == 31
replace CIIU_NOMBRE = "32. Radio, TV y comunicaciones" if RAMA2D == 32
replace CIIU_NOMBRE = "33. Instrumentos medicos y precision" if RAMA2D == 33
replace CIIU_NOMBRE = "34. Vehiculos automotores" if RAMA2D == 34
replace CIIU_NOMBRE = "35. Otros equipos de transporte" if RAMA2D == 35
replace CIIU_NOMBRE = "36. Muebles y otras manufactureras" if RAMA2D == 36
replace CIIU_NOMBRE = "37. Reciclaje" if RAMA2D == 37

*===================================================
* 5. GUARDAR BASE TEMPORAL DE TRABAJO
*===================================================
tempfile base_manuf
save `base_manuf', replace

****************************************************
* HOJA 1: RESULTADOS POR SECTOR (CIIU)
****************************************************

use `base_manuf', clear

tempname memhold1
postfile `memhold1' int RAMA2D str60 CIIU_NOMBRE ///
    double trabajadores mean_inglabo median_inglabo ///
    using resultados_sector_tmp.dta, replace

levelsof RAMA2D, local(lista_ciiu)

foreach c of local lista_ciiu {

    preserve
        use `base_manuf', clear
        keep if RAMA2D == `c'

        * trabajadores expandidos
        quietly summarize `fac'
        local trabajadores = r(sum)

        * promedio ponderado
        quietly mean INGLABO [pw=`fac']
        matrix M = e(b)
        local media = M[1,1]

        * mediana ponderada manual
        sort INGLABO
        gen peso = `fac'
        egen peso_total = total(peso)
        gen peso_acum = sum(peso)
        gen frac_acum = peso_acum / peso_total
        quietly summarize INGLABO if frac_acum >= 0.5, meanonly
        local mediana = r(min)

        * nombre
        quietly levelsof CIIU_NOMBRE, local(nombre_ciiu)
        local nombre_txt "`nombre_ciiu'"

        post `memhold1' (`c') ("`nombre_txt'") ///
            (`trabajadores') (`media') (`mediana')
    restore
}

postclose `memhold1'

use resultados_sector_tmp.dta, clear
sort RAMA2D
save resultados_por_sector.dta, replace

****************************************************
* HOJA 2: RESULTADOS POR REGION
****************************************************

use `base_manuf', clear

tempname memhold2
postfile `memhold2' str20 REGION ///
    double trabajadores mean_inglabo median_inglabo ///
    using resultados_region_tmp.dta, replace

levelsof REGION, local(lista_regiones)

foreach r of local lista_regiones {

    preserve
        use `base_manuf', clear
        keep if REGION == "`r'"

        * trabajadores expandidos
        quietly summarize `fac'
        local trabajadores = r(sum)

        * promedio ponderado
        quietly mean INGLABO [pw=`fac']
        matrix M = e(b)
        local media = M[1,1]

        * mediana ponderada manual
        sort INGLABO
        gen peso = `fac'
        egen peso_total = total(peso)
        gen peso_acum = sum(peso)
        gen frac_acum = peso_acum / peso_total
        quietly summarize INGLABO if frac_acum >= 0.5, meanonly
        local mediana = r(min)

        post `memhold2' ("`r'") (`trabajadores') (`media') (`mediana')
    restore
}

postclose `memhold2'

use resultados_region_tmp.dta, clear
sort REGION
save resultados_por_region.dta, replace

****************************************************
* EXPORTAR A EXCEL CON 2 HOJAS
****************************************************

use resultados_por_sector.dta, clear
export excel using "INGLABO_manufactura_2013.xlsx", ///
    sheet("por_sector") firstrow(variables) sheetreplace

use resultados_por_region.dta, clear
export excel using "INGLABO_manufactura_2013.xlsx", ///
    sheet("por_region") firstrow(variables) sheetmodify

di "========================================="
di "Archivo exportado: INGLABO_manufactura_2013.xlsx"
di "========================================="

****************************************************
* INGRESO LABORAL EN MANUFACTURA - GEIH 2014
****************************************************
cd "C:/Users/jorge/Documents/Databases/GEIH"

use "GEIH_2014_TOTAL.dta", clear

*===================================================
* 0. DEFINIR FACTOR DE EXPANSION
* Cambia esto por el nombre correcto en tu base
*===================================================
gen FEX = FEX_C_2011/12
local fac "FEX"

* Verificacion rapida
describe RAMA2D INGLABO DPTO `fac'
destring DPTO, replace

*===================================================
* 1. ASEGURAR QUE RAMA2D SEA NUMERICA
*===================================================
capture confirm numeric variable RAMA2D
if _rc != 0 {
    destring RAMA2D, replace force
}

*===================================================
* 2. CREAR REGION
*===================================================
gen REGION = ""

* Caribe
replace REGION = "Caribe" if inlist(DPTO, 8, 13, 20, 23, 44, 47, 70)

* Insular
replace REGION = "Insular" if DPTO == 88

* Andina
replace REGION = "Andina" if inlist(DPTO, 5, 11, 15, 17, 25, 41, 54, 63, 66, 68, 73)

* Pacifica
replace REGION = "Pacifica" if inlist(DPTO, 19, 27, 52, 76)

* Orinoquia
replace REGION = "Orinoquia" if inlist(DPTO, 50, 81, 85, 99)

* Amazonia
replace REGION = "Amazonia" if inlist(DPTO, 18, 86, 91, 94, 95, 97)

*===================================================
* 3. QUEDARSE SOLO CON MANUFACTURA (CIIU REV 3: 15-37)
*===================================================
keep if inrange(RAMA2D, 15, 37)

* Solo ocupados con ingreso laboral observado
keep if !missing(INGLABO)
keep if !missing(`fac')
keep if `fac' > 0
keep if !missing(REGION)

*===================================================
* 4. ETIQUETAS DE RAMA2D (MANUFACTURA REV 3)
*===================================================
gen CIIU_NOMBRE = ""

replace CIIU_NOMBRE = "15. Alimentos y bebidas" if RAMA2D == 15
replace CIIU_NOMBRE = "16. Tabaco" if RAMA2D == 16
replace CIIU_NOMBRE = "17. Textiles" if RAMA2D == 17
replace CIIU_NOMBRE = "18. Confecciones" if RAMA2D == 18
replace CIIU_NOMBRE = "19. Cuero y calzado" if RAMA2D == 19
replace CIIU_NOMBRE = "20. Madera y corcho" if RAMA2D == 20
replace CIIU_NOMBRE = "21. Papel y carton" if RAMA2D == 21
replace CIIU_NOMBRE = "22. Edicion e impresion" if RAMA2D == 22
replace CIIU_NOMBRE = "23. Refinacion de petroleo" if RAMA2D == 23
replace CIIU_NOMBRE = "24. Quimicos" if RAMA2D == 24
replace CIIU_NOMBRE = "25. Caucho y plastico" if RAMA2D == 25
replace CIIU_NOMBRE = "26. Minerales no metalicos" if RAMA2D == 26
replace CIIU_NOMBRE = "27. Metalurgia basica" if RAMA2D == 27
replace CIIU_NOMBRE = "28. Productos metalicos" if RAMA2D == 28
replace CIIU_NOMBRE = "29. Maquinaria y equipo" if RAMA2D == 29
replace CIIU_NOMBRE = "30. Maquinaria de oficina e informatica" if RAMA2D == 30
replace CIIU_NOMBRE = "31. Maquinaria y aparatos electricos" if RAMA2D == 31
replace CIIU_NOMBRE = "32. Radio, TV y comunicaciones" if RAMA2D == 32
replace CIIU_NOMBRE = "33. Instrumentos medicos y precision" if RAMA2D == 33
replace CIIU_NOMBRE = "34. Vehiculos automotores" if RAMA2D == 34
replace CIIU_NOMBRE = "35. Otros equipos de transporte" if RAMA2D == 35
replace CIIU_NOMBRE = "36. Muebles y otras manufactureras" if RAMA2D == 36
replace CIIU_NOMBRE = "37. Reciclaje" if RAMA2D == 37

*===================================================
* 5. GUARDAR BASE TEMPORAL DE TRABAJO
*===================================================
tempfile base_manuf
save `base_manuf', replace

****************************************************
* HOJA 1: RESULTADOS POR SECTOR (CIIU)
****************************************************

use `base_manuf', clear

tempname memhold1
postfile `memhold1' int RAMA2D str60 CIIU_NOMBRE ///
    double trabajadores mean_inglabo median_inglabo ///
    using resultados_sector_tmp.dta, replace

levelsof RAMA2D, local(lista_ciiu)

foreach c of local lista_ciiu {

    preserve
        use `base_manuf', clear
        keep if RAMA2D == `c'

        * trabajadores expandidos
        quietly summarize `fac'
        local trabajadores = r(sum)

        * promedio ponderado
        quietly mean INGLABO [pw=`fac']
        matrix M = e(b)
        local media = M[1,1]

        * mediana ponderada manual
        sort INGLABO
        gen peso = `fac'
        egen peso_total = total(peso)
        gen peso_acum = sum(peso)
        gen frac_acum = peso_acum / peso_total
        quietly summarize INGLABO if frac_acum >= 0.5, meanonly
        local mediana = r(min)

        * nombre
        quietly levelsof CIIU_NOMBRE, local(nombre_ciiu)
        local nombre_txt "`nombre_ciiu'"

        post `memhold1' (`c') ("`nombre_txt'") ///
            (`trabajadores') (`media') (`mediana')
    restore
}

postclose `memhold1'

use resultados_sector_tmp.dta, clear
sort RAMA2D
save resultados_por_sector.dta, replace

****************************************************
* HOJA 2: RESULTADOS POR REGION
****************************************************

use `base_manuf', clear

tempname memhold2
postfile `memhold2' str20 REGION ///
    double trabajadores mean_inglabo median_inglabo ///
    using resultados_region_tmp.dta, replace

levelsof REGION, local(lista_regiones)

foreach r of local lista_regiones {

    preserve
        use `base_manuf', clear
        keep if REGION == "`r'"

        * trabajadores expandidos
        quietly summarize `fac'
        local trabajadores = r(sum)

        * promedio ponderado
        quietly mean INGLABO [pw=`fac']
        matrix M = e(b)
        local media = M[1,1]

        * mediana ponderada manual
        sort INGLABO
        gen peso = `fac'
        egen peso_total = total(peso)
        gen peso_acum = sum(peso)
        gen frac_acum = peso_acum / peso_total
        quietly summarize INGLABO if frac_acum >= 0.5, meanonly
        local mediana = r(min)

        post `memhold2' ("`r'") (`trabajadores') (`media') (`mediana')
    restore
}

postclose `memhold2'

use resultados_region_tmp.dta, clear
sort REGION
save resultados_por_region.dta, replace

****************************************************
* EXPORTAR A EXCEL CON 2 HOJAS
****************************************************

use resultados_por_sector.dta, clear
export excel using "INGLABO_manufactura_2014.xlsx", ///
    sheet("por_sector") firstrow(variables) sheetreplace

use resultados_por_region.dta, clear
export excel using "INGLABO_manufactura_2014.xlsx", ///
    sheet("por_region") firstrow(variables) sheetmodify

di "========================================="
di "Archivo exportado: INGLABO_manufactura_2014.xlsx"
di "========================================="

****************************************************
* INGRESO LABORAL EN MANUFACTURA - GEIH 2015
****************************************************
cd "C:/Users/jorge/Documents/Databases/GEIH"

use "GEIH_2015_TOTAL.dta", clear

*===================================================
* 0. DEFINIR FACTOR DE EXPANSION
* Cambia esto por el nombre correcto en tu base
*===================================================
gen FEX = FEX_C18
local fac "FEX"

* Verificacion rapida
describe RAMA2D INGLABO DPTO `fac'
destring DPTO, replace
destring INGLABO, replace

*===================================================
* 1. ASEGURAR QUE RAMA2D SEA NUMERICA
*===================================================
capture confirm numeric variable RAMA2D
if _rc != 0 {
    destring RAMA2D, replace force
}

*===================================================
* 2. CREAR REGION
*===================================================
gen REGION = ""

* Caribe
replace REGION = "Caribe" if inlist(DPTO, 8, 13, 20, 23, 44, 47, 70)

* Insular
replace REGION = "Insular" if DPTO == 88

* Andina
replace REGION = "Andina" if inlist(DPTO, 5, 11, 15, 17, 25, 41, 54, 63, 66, 68, 73)

* Pacifica
replace REGION = "Pacifica" if inlist(DPTO, 19, 27, 52, 76)

* Orinoquia
replace REGION = "Orinoquia" if inlist(DPTO, 50, 81, 85, 99)

* Amazonia
replace REGION = "Amazonia" if inlist(DPTO, 18, 86, 91, 94, 95, 97)

*===================================================
* 3. QUEDARSE SOLO CON MANUFACTURA (CIIU REV 3: 15-37)
*===================================================
keep if inrange(RAMA2D, 15, 37)

* Solo ocupados con ingreso laboral observado
keep if !missing(INGLABO)
keep if !missing(`fac')
keep if `fac' > 0
keep if !missing(REGION)

*===================================================
* 4. ETIQUETAS DE RAMA2D (MANUFACTURA REV 3)
*===================================================
gen CIIU_NOMBRE = ""

replace CIIU_NOMBRE = "15. Alimentos y bebidas" if RAMA2D == 15
replace CIIU_NOMBRE = "16. Tabaco" if RAMA2D == 16
replace CIIU_NOMBRE = "17. Textiles" if RAMA2D == 17
replace CIIU_NOMBRE = "18. Confecciones" if RAMA2D == 18
replace CIIU_NOMBRE = "19. Cuero y calzado" if RAMA2D == 19
replace CIIU_NOMBRE = "20. Madera y corcho" if RAMA2D == 20
replace CIIU_NOMBRE = "21. Papel y carton" if RAMA2D == 21
replace CIIU_NOMBRE = "22. Edicion e impresion" if RAMA2D == 22
replace CIIU_NOMBRE = "23. Refinacion de petroleo" if RAMA2D == 23
replace CIIU_NOMBRE = "24. Quimicos" if RAMA2D == 24
replace CIIU_NOMBRE = "25. Caucho y plastico" if RAMA2D == 25
replace CIIU_NOMBRE = "26. Minerales no metalicos" if RAMA2D == 26
replace CIIU_NOMBRE = "27. Metalurgia basica" if RAMA2D == 27
replace CIIU_NOMBRE = "28. Productos metalicos" if RAMA2D == 28
replace CIIU_NOMBRE = "29. Maquinaria y equipo" if RAMA2D == 29
replace CIIU_NOMBRE = "30. Maquinaria de oficina e informatica" if RAMA2D == 30
replace CIIU_NOMBRE = "31. Maquinaria y aparatos electricos" if RAMA2D == 31
replace CIIU_NOMBRE = "32. Radio, TV y comunicaciones" if RAMA2D == 32
replace CIIU_NOMBRE = "33. Instrumentos medicos y precision" if RAMA2D == 33
replace CIIU_NOMBRE = "34. Vehiculos automotores" if RAMA2D == 34
replace CIIU_NOMBRE = "35. Otros equipos de transporte" if RAMA2D == 35
replace CIIU_NOMBRE = "36. Muebles y otras manufactureras" if RAMA2D == 36
replace CIIU_NOMBRE = "37. Reciclaje" if RAMA2D == 37

*===================================================
* 5. GUARDAR BASE TEMPORAL DE TRABAJO
*===================================================
tempfile base_manuf
save `base_manuf', replace

****************************************************
* HOJA 1: RESULTADOS POR SECTOR (CIIU)
****************************************************

use `base_manuf', clear

tempname memhold1
postfile `memhold1' int RAMA2D str60 CIIU_NOMBRE ///
    double trabajadores mean_inglabo median_inglabo ///
    using resultados_sector_tmp.dta, replace

levelsof RAMA2D, local(lista_ciiu)

foreach c of local lista_ciiu {

    preserve
        use `base_manuf', clear
        keep if RAMA2D == `c'

        * trabajadores expandidos
        quietly summarize `fac'
        local trabajadores = r(sum)

        * promedio ponderado
        quietly mean INGLABO [pw=`fac']
        matrix M = e(b)
        local media = M[1,1]

        * mediana ponderada manual
        sort INGLABO
        gen peso = `fac'
        egen peso_total = total(peso)
        gen peso_acum = sum(peso)
        gen frac_acum = peso_acum / peso_total
        quietly summarize INGLABO if frac_acum >= 0.5, meanonly
        local mediana = r(min)

        * nombre
        quietly levelsof CIIU_NOMBRE, local(nombre_ciiu)
        local nombre_txt "`nombre_ciiu'"

        post `memhold1' (`c') ("`nombre_txt'") ///
            (`trabajadores') (`media') (`mediana')
    restore
}

postclose `memhold1'

use resultados_sector_tmp.dta, clear
sort RAMA2D
save resultados_por_sector.dta, replace

****************************************************
* HOJA 2: RESULTADOS POR REGION
****************************************************

use `base_manuf', clear

tempname memhold2
postfile `memhold2' str20 REGION ///
    double trabajadores mean_inglabo median_inglabo ///
    using resultados_region_tmp.dta, replace

levelsof REGION, local(lista_regiones)

foreach r of local lista_regiones {

    preserve
        use `base_manuf', clear
        keep if REGION == "`r'"

        * trabajadores expandidos
        quietly summarize `fac'
        local trabajadores = r(sum)

        * promedio ponderado
        quietly mean INGLABO [pw=`fac']
        matrix M = e(b)
        local media = M[1,1]

        * mediana ponderada manual
        sort INGLABO
        gen peso = `fac'
        egen peso_total = total(peso)
        gen peso_acum = sum(peso)
        gen frac_acum = peso_acum / peso_total
        quietly summarize INGLABO if frac_acum >= 0.5, meanonly
        local mediana = r(min)

        post `memhold2' ("`r'") (`trabajadores') (`media') (`mediana')
    restore
}

postclose `memhold2'

use resultados_region_tmp.dta, clear
sort REGION
save resultados_por_region.dta, replace

****************************************************
* EXPORTAR A EXCEL CON 2 HOJAS
****************************************************

use resultados_por_sector.dta, clear
export excel using "INGLABO_manufactura_2015.xlsx", ///
    sheet("por_sector") firstrow(variables) sheetreplace

use resultados_por_region.dta, clear
export excel using "INGLABO_manufactura_2015.xlsx", ///
    sheet("por_region") firstrow(variables) sheetmodify

di "========================================="
di "Archivo exportado: INGLABO_manufactura_2015.xlsx"
di "========================================="

****************************************************
* INGRESO LABORAL EN MANUFACTURA - GEIH 2016
****************************************************
cd "C:/Users/jorge/Documents/Databases/GEIH"

use "GEIH_2016_TOTAL.dta", clear

*===================================================
* 0. DEFINIR FACTOR DE EXPANSION
* Cambia esto por el nombre correcto en tu base
*===================================================
gen FEX = FEX_C18/12
local fac "FEX"

* Verificacion rapida
describe RAMA2D INGLABO DPTO `fac'
destring DPTO, replace
destring INGLABO, replace

*===================================================
* 1. ASEGURAR QUE RAMA2D SEA NUMERICA
*===================================================
capture confirm numeric variable RAMA2D
if _rc != 0 {
    destring RAMA2D, replace force
}

*===================================================
* 2. CREAR REGION
*===================================================
gen REGION = ""

* Caribe
replace REGION = "Caribe" if inlist(DPTO, 8, 13, 20, 23, 44, 47, 70)

* Insular
replace REGION = "Insular" if DPTO == 88

* Andina
replace REGION = "Andina" if inlist(DPTO, 5, 11, 15, 17, 25, 41, 54, 63, 66, 68, 73)

* Pacifica
replace REGION = "Pacifica" if inlist(DPTO, 19, 27, 52, 76)

* Orinoquia
replace REGION = "Orinoquia" if inlist(DPTO, 50, 81, 85, 99)

* Amazonia
replace REGION = "Amazonia" if inlist(DPTO, 18, 86, 91, 94, 95, 97)

*===================================================
* 3. QUEDARSE SOLO CON MANUFACTURA (CIIU REV 3: 15-37)
*===================================================
keep if inrange(RAMA2D, 15, 37)

* Solo ocupados con ingreso laboral observado
keep if !missing(INGLABO)
keep if !missing(`fac')
keep if `fac' > 0
keep if !missing(REGION)

*===================================================
* 4. ETIQUETAS DE RAMA2D (MANUFACTURA REV 3)
*===================================================
gen CIIU_NOMBRE = ""

replace CIIU_NOMBRE = "15. Alimentos y bebidas" if RAMA2D == 15
replace CIIU_NOMBRE = "16. Tabaco" if RAMA2D == 16
replace CIIU_NOMBRE = "17. Textiles" if RAMA2D == 17
replace CIIU_NOMBRE = "18. Confecciones" if RAMA2D == 18
replace CIIU_NOMBRE = "19. Cuero y calzado" if RAMA2D == 19
replace CIIU_NOMBRE = "20. Madera y corcho" if RAMA2D == 20
replace CIIU_NOMBRE = "21. Papel y carton" if RAMA2D == 21
replace CIIU_NOMBRE = "22. Edicion e impresion" if RAMA2D == 22
replace CIIU_NOMBRE = "23. Refinacion de petroleo" if RAMA2D == 23
replace CIIU_NOMBRE = "24. Quimicos" if RAMA2D == 24
replace CIIU_NOMBRE = "25. Caucho y plastico" if RAMA2D == 25
replace CIIU_NOMBRE = "26. Minerales no metalicos" if RAMA2D == 26
replace CIIU_NOMBRE = "27. Metalurgia basica" if RAMA2D == 27
replace CIIU_NOMBRE = "28. Productos metalicos" if RAMA2D == 28
replace CIIU_NOMBRE = "29. Maquinaria y equipo" if RAMA2D == 29
replace CIIU_NOMBRE = "30. Maquinaria de oficina e informatica" if RAMA2D == 30
replace CIIU_NOMBRE = "31. Maquinaria y aparatos electricos" if RAMA2D == 31
replace CIIU_NOMBRE = "32. Radio, TV y comunicaciones" if RAMA2D == 32
replace CIIU_NOMBRE = "33. Instrumentos medicos y precision" if RAMA2D == 33
replace CIIU_NOMBRE = "34. Vehiculos automotores" if RAMA2D == 34
replace CIIU_NOMBRE = "35. Otros equipos de transporte" if RAMA2D == 35
replace CIIU_NOMBRE = "36. Muebles y otras manufactureras" if RAMA2D == 36
replace CIIU_NOMBRE = "37. Reciclaje" if RAMA2D == 37

*===================================================
* 5. GUARDAR BASE TEMPORAL DE TRABAJO
*===================================================
tempfile base_manuf
save `base_manuf', replace

****************************************************
* HOJA 1: RESULTADOS POR SECTOR (CIIU)
****************************************************

use `base_manuf', clear

tempname memhold1
postfile `memhold1' int RAMA2D str60 CIIU_NOMBRE ///
    double trabajadores mean_inglabo median_inglabo ///
    using resultados_sector_tmp.dta, replace

levelsof RAMA2D, local(lista_ciiu)

foreach c of local lista_ciiu {

    preserve
        use `base_manuf', clear
        keep if RAMA2D == `c'

        * trabajadores expandidos
        quietly summarize `fac'
        local trabajadores = r(sum)

        * promedio ponderado
        quietly mean INGLABO [pw=`fac']
        matrix M = e(b)
        local media = M[1,1]

        * mediana ponderada manual
        sort INGLABO
        gen peso = `fac'
        egen peso_total = total(peso)
        gen peso_acum = sum(peso)
        gen frac_acum = peso_acum / peso_total
        quietly summarize INGLABO if frac_acum >= 0.5, meanonly
        local mediana = r(min)

        * nombre
        quietly levelsof CIIU_NOMBRE, local(nombre_ciiu)
        local nombre_txt "`nombre_ciiu'"

        post `memhold1' (`c') ("`nombre_txt'") ///
            (`trabajadores') (`media') (`mediana')
    restore
}

postclose `memhold1'

use resultados_sector_tmp.dta, clear
sort RAMA2D
save resultados_por_sector.dta, replace

****************************************************
* HOJA 2: RESULTADOS POR REGION
****************************************************

use `base_manuf', clear

tempname memhold2
postfile `memhold2' str20 REGION ///
    double trabajadores mean_inglabo median_inglabo ///
    using resultados_region_tmp.dta, replace

levelsof REGION, local(lista_regiones)

foreach r of local lista_regiones {

    preserve
        use `base_manuf', clear
        keep if REGION == "`r'"

        * trabajadores expandidos
        quietly summarize `fac'
        local trabajadores = r(sum)

        * promedio ponderado
        quietly mean INGLABO [pw=`fac']
        matrix M = e(b)
        local media = M[1,1]

        * mediana ponderada manual
        sort INGLABO
        gen peso = `fac'
        egen peso_total = total(peso)
        gen peso_acum = sum(peso)
        gen frac_acum = peso_acum / peso_total
        quietly summarize INGLABO if frac_acum >= 0.5, meanonly
        local mediana = r(min)

        post `memhold2' ("`r'") (`trabajadores') (`media') (`mediana')
    restore
}

postclose `memhold2'

use resultados_region_tmp.dta, clear
sort REGION
save resultados_por_region.dta, replace

****************************************************
* EXPORTAR A EXCEL CON 2 HOJAS
****************************************************

use resultados_por_sector.dta, clear
export excel using "INGLABO_manufactura_2016.xlsx", ///
    sheet("por_sector") firstrow(variables) sheetreplace

use resultados_por_region.dta, clear
export excel using "INGLABO_manufactura_2016.xlsx", ///
    sheet("por_region") firstrow(variables) sheetmodify

di "========================================="
di "Archivo exportado: INGLABO_manufactura_2016.xlsx"
di "========================================="

****************************************************
* INGRESO LABORAL EN MANUFACTURA - GEIH 2017
****************************************************
cd "C:/Users/jorge/Documents/Databases/GEIH"

use "GEIH_2017_TOTAL.dta", clear

*===================================================
* 0. DEFINIR FACTOR DE EXPANSION
*===================================================
gen FEX = FEX_C18/12
local fac "FEX"

* Verificacion rapida
describe RAMA2D INGLABO DPTO `fac'
destring DPTO, replace
destring INGLABO, replace

*===================================================
* 1. ASEGURAR QUE RAMA2D SEA NUMERICA
*===================================================
capture confirm numeric variable RAMA2D
if _rc != 0 {
    destring RAMA2D, replace force
}

*===================================================
* 2. CREAR REGION
*===================================================
gen REGION = ""

* Caribe
replace REGION = "Caribe" if inlist(DPTO, 8, 13, 20, 23, 44, 47, 70)

* Insular
replace REGION = "Insular" if DPTO == 88

* Andina
replace REGION = "Andina" if inlist(DPTO, 5, 11, 15, 17, 25, 41, 54, 63, 66, 68, 73)

* Pacifica
replace REGION = "Pacifica" if inlist(DPTO, 19, 27, 52, 76)

* Orinoquia
replace REGION = "Orinoquia" if inlist(DPTO, 50, 81, 85, 99)

* Amazonia
replace REGION = "Amazonia" if inlist(DPTO, 18, 86, 91, 94, 95, 97)

*===================================================
* 3. QUEDARSE SOLO CON MANUFACTURA (CIIU REV 3: 15-37)
*===================================================
keep if inrange(RAMA2D, 15, 37)

* Solo ocupados con ingreso laboral observado
keep if !missing(INGLABO)
keep if !missing(`fac')
keep if `fac' > 0
keep if !missing(REGION)

*===================================================
* 4. ETIQUETAS DE RAMA2D (MANUFACTURA REV 3)
*===================================================
gen CIIU_NOMBRE = ""

replace CIIU_NOMBRE = "15. Alimentos y bebidas" if RAMA2D == 15
replace CIIU_NOMBRE = "16. Tabaco" if RAMA2D == 16
replace CIIU_NOMBRE = "17. Textiles" if RAMA2D == 17
replace CIIU_NOMBRE = "18. Confecciones" if RAMA2D == 18
replace CIIU_NOMBRE = "19. Cuero y calzado" if RAMA2D == 19
replace CIIU_NOMBRE = "20. Madera y corcho" if RAMA2D == 20
replace CIIU_NOMBRE = "21. Papel y carton" if RAMA2D == 21
replace CIIU_NOMBRE = "22. Edicion e impresion" if RAMA2D == 22
replace CIIU_NOMBRE = "23. Refinacion de petroleo" if RAMA2D == 23
replace CIIU_NOMBRE = "24. Quimicos" if RAMA2D == 24
replace CIIU_NOMBRE = "25. Caucho y plastico" if RAMA2D == 25
replace CIIU_NOMBRE = "26. Minerales no metalicos" if RAMA2D == 26
replace CIIU_NOMBRE = "27. Metalurgia basica" if RAMA2D == 27
replace CIIU_NOMBRE = "28. Productos metalicos" if RAMA2D == 28
replace CIIU_NOMBRE = "29. Maquinaria y equipo" if RAMA2D == 29
replace CIIU_NOMBRE = "30. Maquinaria de oficina e informatica" if RAMA2D == 30
replace CIIU_NOMBRE = "31. Maquinaria y aparatos electricos" if RAMA2D == 31
replace CIIU_NOMBRE = "32. Radio, TV y comunicaciones" if RAMA2D == 32
replace CIIU_NOMBRE = "33. Instrumentos medicos y precision" if RAMA2D == 33
replace CIIU_NOMBRE = "34. Vehiculos automotores" if RAMA2D == 34
replace CIIU_NOMBRE = "35. Otros equipos de transporte" if RAMA2D == 35
replace CIIU_NOMBRE = "36. Muebles y otras manufactureras" if RAMA2D == 36
replace CIIU_NOMBRE = "37. Reciclaje" if RAMA2D == 37

*===================================================
* 5. GUARDAR BASE TEMPORAL DE TRABAJO
*===================================================
tempfile base_manuf
save `base_manuf', replace

****************************************************
* HOJA 1: RESULTADOS POR SECTOR (CIIU)
****************************************************

use `base_manuf', clear

tempname memhold1
postfile `memhold1' int RAMA2D str60 CIIU_NOMBRE ///
    double trabajadores mean_inglabo median_inglabo ///
    using resultados_sector_tmp.dta, replace

levelsof RAMA2D, local(lista_ciiu)

foreach c of local lista_ciiu {

    preserve
        use `base_manuf', clear
        keep if RAMA2D == `c'

        * trabajadores expandidos
        quietly summarize `fac'
        local trabajadores = r(sum)

        * promedio ponderado
        quietly mean INGLABO [pw=`fac']
        matrix M = e(b)
        local media = M[1,1]

        * mediana ponderada manual
        sort INGLABO
        gen peso = `fac'
        egen peso_total = total(peso)
        gen peso_acum = sum(peso)
        gen frac_acum = peso_acum / peso_total
        quietly summarize INGLABO if frac_acum >= 0.5, meanonly
        local mediana = r(min)

        * nombre
        quietly levelsof CIIU_NOMBRE, local(nombre_ciiu)
        local nombre_txt "`nombre_ciiu'"

        post `memhold1' (`c') ("`nombre_txt'") ///
            (`trabajadores') (`media') (`mediana')
    restore
}

postclose `memhold1'

use resultados_sector_tmp.dta, clear
sort RAMA2D
save resultados_por_sector.dta, replace

****************************************************
* HOJA 2: RESULTADOS POR REGION
****************************************************

use `base_manuf', clear

tempname memhold2
postfile `memhold2' str20 REGION ///
    double trabajadores mean_inglabo median_inglabo ///
    using resultados_region_tmp.dta, replace

levelsof REGION, local(lista_regiones)

foreach r of local lista_regiones {

    preserve
        use `base_manuf', clear
        keep if REGION == "`r'"

        * trabajadores expandidos
        quietly summarize `fac'
        local trabajadores = r(sum)

        * promedio ponderado
        quietly mean INGLABO [pw=`fac']
        matrix M = e(b)
        local media = M[1,1]

        * mediana ponderada manual
        sort INGLABO
        gen peso = `fac'
        egen peso_total = total(peso)
        gen peso_acum = sum(peso)
        gen frac_acum = peso_acum / peso_total
        quietly summarize INGLABO if frac_acum >= 0.5, meanonly
        local mediana = r(min)

        post `memhold2' ("`r'") (`trabajadores') (`media') (`mediana')
    restore
}

postclose `memhold2'

use resultados_region_tmp.dta, clear
sort REGION
save resultados_por_region.dta, replace

****************************************************
* EXPORTAR A EXCEL CON 2 HOJAS
****************************************************

use resultados_por_sector.dta, clear
export excel using "INGLABO_manufactura_2017.xlsx", ///
    sheet("por_sector") firstrow(variables) sheetreplace

use resultados_por_region.dta, clear
export excel using "INGLABO_manufactura_2017.xlsx", ///
    sheet("por_region") firstrow(variables) sheetmodify

di "========================================="
di "Archivo exportado: INGLABO_manufactura_2017.xlsx"
di "========================================="

****************************************************
* INGRESO LABORAL EN MANUFACTURA - GEIH 2018
****************************************************
cd "C:/Users/jorge/Documents/Databases/GEIH"

use "GEIH_2018_TOTAL.dta", clear

*===================================================
* 0. DEFINIR FACTOR DE EXPANSION
*===================================================
gen FEX = FEX_C18/12
local fac "FEX"

* Verificacion rapida
describe RAMA2D INGLABO DPTO `fac'
destring DPTO, replace
destring INGLABO, replace

*===================================================
* 1. ASEGURAR QUE RAMA2D SEA NUMERICA
*===================================================
capture confirm numeric variable RAMA2D
if _rc != 0 {
    destring RAMA2D, replace force
}

*===================================================
* 2. CREAR REGION
*===================================================
gen REGION = ""

* Caribe
replace REGION = "Caribe" if inlist(DPTO, 8, 13, 20, 23, 44, 47, 70)

* Insular
replace REGION = "Insular" if DPTO == 88

* Andina
replace REGION = "Andina" if inlist(DPTO, 5, 11, 15, 17, 25, 41, 54, 63, 66, 68, 73)

* Pacifica
replace REGION = "Pacifica" if inlist(DPTO, 19, 27, 52, 76)

* Orinoquia
replace REGION = "Orinoquia" if inlist(DPTO, 50, 81, 85, 99)

* Amazonia
replace REGION = "Amazonia" if inlist(DPTO, 18, 86, 91, 94, 95, 97)

*===================================================
* 3. QUEDARSE SOLO CON MANUFACTURA (CIIU REV 3: 15-37)
*===================================================
keep if inrange(RAMA2D, 15, 37)

* Solo ocupados con ingreso laboral observado
keep if !missing(INGLABO)
keep if !missing(`fac')
keep if `fac' > 0
keep if !missing(REGION)

*===================================================
* 4. ETIQUETAS DE RAMA2D (MANUFACTURA REV 3)
*===================================================
gen CIIU_NOMBRE = ""

replace CIIU_NOMBRE = "15. Alimentos y bebidas" if RAMA2D == 15
replace CIIU_NOMBRE = "16. Tabaco" if RAMA2D == 16
replace CIIU_NOMBRE = "17. Textiles" if RAMA2D == 17
replace CIIU_NOMBRE = "18. Confecciones" if RAMA2D == 18
replace CIIU_NOMBRE = "19. Cuero y calzado" if RAMA2D == 19
replace CIIU_NOMBRE = "20. Madera y corcho" if RAMA2D == 20
replace CIIU_NOMBRE = "21. Papel y carton" if RAMA2D == 21
replace CIIU_NOMBRE = "22. Edicion e impresion" if RAMA2D == 22
replace CIIU_NOMBRE = "23. Refinacion de petroleo" if RAMA2D == 23
replace CIIU_NOMBRE = "24. Quimicos" if RAMA2D == 24
replace CIIU_NOMBRE = "25. Caucho y plastico" if RAMA2D == 25
replace CIIU_NOMBRE = "26. Minerales no metalicos" if RAMA2D == 26
replace CIIU_NOMBRE = "27. Metalurgia basica" if RAMA2D == 27
replace CIIU_NOMBRE = "28. Productos metalicos" if RAMA2D == 28
replace CIIU_NOMBRE = "29. Maquinaria y equipo" if RAMA2D == 29
replace CIIU_NOMBRE = "30. Maquinaria de oficina e informatica" if RAMA2D == 30
replace CIIU_NOMBRE = "31. Maquinaria y aparatos electricos" if RAMA2D == 31
replace CIIU_NOMBRE = "32. Radio, TV y comunicaciones" if RAMA2D == 32
replace CIIU_NOMBRE = "33. Instrumentos medicos y precision" if RAMA2D == 33
replace CIIU_NOMBRE = "34. Vehiculos automotores" if RAMA2D == 34
replace CIIU_NOMBRE = "35. Otros equipos de transporte" if RAMA2D == 35
replace CIIU_NOMBRE = "36. Muebles y otras manufactureras" if RAMA2D == 36
replace CIIU_NOMBRE = "37. Reciclaje" if RAMA2D == 37

*===================================================
* 5. GUARDAR BASE TEMPORAL DE TRABAJO
*===================================================
tempfile base_manuf
save `base_manuf', replace

****************************************************
* HOJA 1: RESULTADOS POR SECTOR (CIIU)
****************************************************

use `base_manuf', clear

tempname memhold1
postfile `memhold1' int RAMA2D str60 CIIU_NOMBRE ///
    double trabajadores mean_inglabo median_inglabo ///
    using resultados_sector_tmp.dta, replace

levelsof RAMA2D, local(lista_ciiu)

foreach c of local lista_ciiu {

    preserve
        use `base_manuf', clear
        keep if RAMA2D == `c'

        * trabajadores expandidos
        quietly summarize `fac'
        local trabajadores = r(sum)

        * promedio ponderado
        quietly mean INGLABO [pw=`fac']
        matrix M = e(b)
        local media = M[1,1]

        * mediana ponderada manual
        sort INGLABO
        gen peso = `fac'
        egen peso_total = total(peso)
        gen peso_acum = sum(peso)
        gen frac_acum = peso_acum / peso_total
        quietly summarize INGLABO if frac_acum >= 0.5, meanonly
        local mediana = r(min)

        * nombre
        quietly levelsof CIIU_NOMBRE, local(nombre_ciiu)
        local nombre_txt "`nombre_ciiu'"

        post `memhold1' (`c') ("`nombre_txt'") ///
            (`trabajadores') (`media') (`mediana')
    restore
}

postclose `memhold1'

use resultados_sector_tmp.dta, clear
sort RAMA2D
save resultados_por_sector.dta, replace

****************************************************
* HOJA 2: RESULTADOS POR REGION
****************************************************

use `base_manuf', clear

tempname memhold2
postfile `memhold2' str20 REGION ///
    double trabajadores mean_inglabo median_inglabo ///
    using resultados_region_tmp.dta, replace

levelsof REGION, local(lista_regiones)

foreach r of local lista_regiones {

    preserve
        use `base_manuf', clear
        keep if REGION == "`r'"

        * trabajadores expandidos
        quietly summarize `fac'
        local trabajadores = r(sum)

        * promedio ponderado
        quietly mean INGLABO [pw=`fac']
        matrix M = e(b)
        local media = M[1,1]

        * mediana ponderada manual
        sort INGLABO
        gen peso = `fac'
        egen peso_total = total(peso)
        gen peso_acum = sum(peso)
        gen frac_acum = peso_acum / peso_total
        quietly summarize INGLABO if frac_acum >= 0.5, meanonly
        local mediana = r(min)

        post `memhold2' ("`r'") (`trabajadores') (`media') (`mediana')
    restore
}

postclose `memhold2'

use resultados_region_tmp.dta, clear
sort REGION
save resultados_por_region.dta, replace

****************************************************
* EXPORTAR A EXCEL CON 2 HOJAS
****************************************************

use resultados_por_sector.dta, clear
export excel using "INGLABO_manufactura_2018.xlsx", ///
    sheet("por_sector") firstrow(variables) sheetreplace

use resultados_por_region.dta, clear
export excel using "INGLABO_manufactura_2018.xlsx", ///
    sheet("por_region") firstrow(variables) sheetmodify

di "========================================="
di "Archivo exportado: INGLABO_manufactura_2018.xlsx"
di "========================================="

****************************************************
* INGRESO LABORAL EN MANUFACTURA - GEIH 2019
****************************************************
cd "C:/Users/jorge/Documents/Databases/GEIH"

use "GEIH_2019_TOTAL.dta", clear

*===================================================
* 0. DEFINIR FACTOR DE EXPANSION
*===================================================
gen FEX = FEX_C18/12
local fac "FEX"

* Verificacion rapida
describe RAMA2D INGLABO DPTO `fac'
destring DPTO, replace
destring INGLABO, replace

*===================================================
* 1. ASEGURAR QUE RAMA2D SEA NUMERICA
*===================================================
capture confirm numeric variable RAMA2D
if _rc != 0 {
    destring RAMA2D, replace force
}

*===================================================
* 2. CREAR REGION
*===================================================
gen REGION = ""

* Caribe
replace REGION = "Caribe" if inlist(DPTO, 8, 13, 20, 23, 44, 47, 70)

* Insular
replace REGION = "Insular" if DPTO == 88

* Andina
replace REGION = "Andina" if inlist(DPTO, 5, 11, 15, 17, 25, 41, 54, 63, 66, 68, 73)

* Pacifica
replace REGION = "Pacifica" if inlist(DPTO, 19, 27, 52, 76)

* Orinoquia
replace REGION = "Orinoquia" if inlist(DPTO, 50, 81, 85, 99)

* Amazonia
replace REGION = "Amazonia" if inlist(DPTO, 18, 86, 91, 94, 95, 97)

*===================================================
* 3. QUEDARSE SOLO CON MANUFACTURA (CIIU REV 3: 15-37)
*===================================================
keep if inrange(RAMA2D, 15, 37)

* Solo ocupados con ingreso laboral observado
keep if !missing(INGLABO)
keep if !missing(`fac')
keep if `fac' > 0
keep if !missing(REGION)

*===================================================
* 4. ETIQUETAS DE RAMA2D (MANUFACTURA REV 3)
*===================================================
gen CIIU_NOMBRE = ""

replace CIIU_NOMBRE = "15. Alimentos y bebidas" if RAMA2D == 15
replace CIIU_NOMBRE = "16. Tabaco" if RAMA2D == 16
replace CIIU_NOMBRE = "17. Textiles" if RAMA2D == 17
replace CIIU_NOMBRE = "18. Confecciones" if RAMA2D == 18
replace CIIU_NOMBRE = "19. Cuero y calzado" if RAMA2D == 19
replace CIIU_NOMBRE = "20. Madera y corcho" if RAMA2D == 20
replace CIIU_NOMBRE = "21. Papel y carton" if RAMA2D == 21
replace CIIU_NOMBRE = "22. Edicion e impresion" if RAMA2D == 22
replace CIIU_NOMBRE = "23. Refinacion de petroleo" if RAMA2D == 23
replace CIIU_NOMBRE = "24. Quimicos" if RAMA2D == 24
replace CIIU_NOMBRE = "25. Caucho y plastico" if RAMA2D == 25
replace CIIU_NOMBRE = "26. Minerales no metalicos" if RAMA2D == 26
replace CIIU_NOMBRE = "27. Metalurgia basica" if RAMA2D == 27
replace CIIU_NOMBRE = "28. Productos metalicos" if RAMA2D == 28
replace CIIU_NOMBRE = "29. Maquinaria y equipo" if RAMA2D == 29
replace CIIU_NOMBRE = "30. Maquinaria de oficina e informatica" if RAMA2D == 30
replace CIIU_NOMBRE = "31. Maquinaria y aparatos electricos" if RAMA2D == 31
replace CIIU_NOMBRE = "32. Radio, TV y comunicaciones" if RAMA2D == 32
replace CIIU_NOMBRE = "33. Instrumentos medicos y precision" if RAMA2D == 33
replace CIIU_NOMBRE = "34. Vehiculos automotores" if RAMA2D == 34
replace CIIU_NOMBRE = "35. Otros equipos de transporte" if RAMA2D == 35
replace CIIU_NOMBRE = "36. Muebles y otras manufactureras" if RAMA2D == 36
replace CIIU_NOMBRE = "37. Reciclaje" if RAMA2D == 37

*===================================================
* 5. GUARDAR BASE TEMPORAL DE TRABAJO
*===================================================
tempfile base_manuf
save `base_manuf', replace

****************************************************
* HOJA 1: RESULTADOS POR SECTOR (CIIU)
****************************************************

use `base_manuf', clear

tempname memhold1
postfile `memhold1' int RAMA2D str60 CIIU_NOMBRE ///
    double trabajadores mean_inglabo median_inglabo ///
    using resultados_sector_tmp.dta, replace

levelsof RAMA2D, local(lista_ciiu)

foreach c of local lista_ciiu {

    preserve
        use `base_manuf', clear
        keep if RAMA2D == `c'

        * trabajadores expandidos
        quietly summarize `fac'
        local trabajadores = r(sum)

        * promedio ponderado
        quietly mean INGLABO [pw=`fac']
        matrix M = e(b)
        local media = M[1,1]

        * mediana ponderada manual
        sort INGLABO
        gen peso = `fac'
        egen peso_total = total(peso)
        gen peso_acum = sum(peso)
        gen frac_acum = peso_acum / peso_total
        quietly summarize INGLABO if frac_acum >= 0.5, meanonly
        local mediana = r(min)

        * nombre
        quietly levelsof CIIU_NOMBRE, local(nombre_ciiu)
        local nombre_txt "`nombre_ciiu'"

        post `memhold1' (`c') ("`nombre_txt'") ///
            (`trabajadores') (`media') (`mediana')
    restore
}

postclose `memhold1'

use resultados_sector_tmp.dta, clear
sort RAMA2D
save resultados_por_sector.dta, replace

****************************************************
* HOJA 2: RESULTADOS POR REGION
****************************************************

use `base_manuf', clear

tempname memhold2
postfile `memhold2' str20 REGION ///
    double trabajadores mean_inglabo median_inglabo ///
    using resultados_region_tmp.dta, replace

levelsof REGION, local(lista_regiones)

foreach r of local lista_regiones {

    preserve
        use `base_manuf', clear
        keep if REGION == "`r'"

        * trabajadores expandidos
        quietly summarize `fac'
        local trabajadores = r(sum)

        * promedio ponderado
        quietly mean INGLABO [pw=`fac']
        matrix M = e(b)
        local media = M[1,1]

        * mediana ponderada manual
        sort INGLABO
        gen peso = `fac'
        egen peso_total = total(peso)
        gen peso_acum = sum(peso)
        gen frac_acum = peso_acum / peso_total
        quietly summarize INGLABO if frac_acum >= 0.5, meanonly
        local mediana = r(min)

        post `memhold2' ("`r'") (`trabajadores') (`media') (`mediana')
    restore
}

postclose `memhold2'

use resultados_region_tmp.dta, clear
sort REGION
save resultados_por_region.dta, replace

****************************************************
* EXPORTAR A EXCEL CON 2 HOJAS
****************************************************

use resultados_por_sector.dta, clear
export excel using "INGLABO_manufactura_2019.xlsx", ///
    sheet("por_sector") firstrow(variables) sheetreplace

use resultados_por_region.dta, clear
export excel using "INGLABO_manufactura_2019.xlsx", ///
    sheet("por_region") firstrow(variables) sheetmodify

di "========================================="
di "Archivo exportado: INGLABO_manufactura_2019.xlsx"
di "========================================="

****************************************************
* INGRESO LABORAL EN MANUFACTURA - GEIH 2020
* CIIU REV 4
****************************************************
cd "C:/Users/jorge/Documents/Databases/GEIH"

use "GEIH_2020_TOTAL.dta", clear

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
* 2. CREAR REGION
*===================================================
gen REGION = ""

* Caribe
replace REGION = "Caribe" if inlist(DPTO, 8, 13, 20, 23, 44, 47, 70)

* Insular
replace REGION = "Insular" if DPTO == 88

* Andina
replace REGION = "Andina" if inlist(DPTO, 5, 11, 15, 17, 25, 41, 54, 63, 66, 68, 73)

* Pacifica
replace REGION = "Pacifica" if inlist(DPTO, 19, 27, 52, 76)

* Orinoquia
replace REGION = "Orinoquia" if inlist(DPTO, 50, 81, 85, 99)

* Amazonia
replace REGION = "Amazonia" if inlist(DPTO, 18, 86, 91, 94, 95, 97)

*===================================================
* 3. QUEDARSE SOLO CON MANUFACTURA (CIIU REV 4: 10-33)
*===================================================
keep if inrange(RAMA2D_R4, 10, 33)

* Solo ocupados con ingreso laboral observado
keep if !missing(INGLABO)
keep if !missing(`fac')
keep if `fac' > 0
keep if !missing(REGION)

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
* HOJA 1: RESULTADOS POR SECTOR (CIIU)
****************************************************

use `base_manuf', clear

tempname memhold1
postfile `memhold1' int RAMA2D_R4 str60 CIIU_NOMBRE ///
    double trabajadores mean_inglabo median_inglabo ///
    using resultados_sector_tmp.dta, replace

levelsof RAMA2D_R4, local(lista_ciiu)

foreach c of local lista_ciiu {

    preserve
        use `base_manuf', clear
        keep if RAMA2D_R4 == `c'

        * trabajadores expandidos
        quietly summarize `fac'
        local trabajadores = r(sum)

        * promedio ponderado
        quietly mean INGLABO [pw=`fac']
        matrix M = e(b)
        local media = M[1,1]

        * mediana ponderada manual
        sort INGLABO
        gen peso = `fac'
        egen peso_total = total(peso)
        gen peso_acum = sum(peso)
        gen frac_acum = peso_acum / peso_total
        quietly summarize INGLABO if frac_acum >= 0.5, meanonly
        local mediana = r(min)

        * nombre
        quietly levelsof CIIU_NOMBRE, local(nombre_ciiu)
        local nombre_txt "`nombre_ciiu'"

        post `memhold1' (`c') ("`nombre_txt'") ///
            (`trabajadores') (`media') (`mediana')
    restore
}

postclose `memhold1'

use resultados_sector_tmp.dta, clear
sort RAMA2D_R4
save resultados_por_sector.dta, replace

****************************************************
* HOJA 2: RESULTADOS POR REGION
****************************************************

use `base_manuf', clear

tempname memhold2
postfile `memhold2' str20 REGION ///
    double trabajadores mean_inglabo median_inglabo ///
    using resultados_region_tmp.dta, replace

levelsof REGION, local(lista_regiones)

foreach r of local lista_regiones {

    preserve
        use `base_manuf', clear
        keep if REGION == "`r'"

        * trabajadores expandidos
        quietly summarize `fac'
        local trabajadores = r(sum)

        * promedio ponderado
        quietly mean INGLABO [pw=`fac']
        matrix M = e(b)
        local media = M[1,1]

        * mediana ponderada manual
        sort INGLABO
        gen peso = `fac'
        egen peso_total = total(peso)
        gen peso_acum = sum(peso)
        gen frac_acum = peso_acum / peso_total
        quietly summarize INGLABO if frac_acum >= 0.5, meanonly
        local mediana = r(min)

        post `memhold2' ("`r'") (`trabajadores') (`media') (`mediana')
    restore
}

postclose `memhold2'

use resultados_region_tmp.dta, clear
sort REGION
save resultados_por_region.dta, replace

****************************************************
* EXPORTAR A EXCEL CON 2 HOJAS
****************************************************

use resultados_por_sector.dta, clear
export excel using "INGLABO_manufactura_2020.xlsx", ///
    sheet("por_sector") firstrow(variables) sheetreplace

use resultados_por_region.dta, clear
export excel using "INGLABO_manufactura_2020.xlsx", ///
    sheet("por_region") firstrow(variables) sheetmodify

di "========================================="
di "Archivo exportado: INGLABO_manufactura_2020.xlsx"
di "========================================="

****************************************************
* INGRESO LABORAL EN MANUFACTURA - GEIH 2021
* CIIU REV 4
****************************************************
cd "C:/Users/jorge/Documents/Databases/GEIH"

use "GEIH_2021_TOTAL.dta", clear

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
* 2. CREAR REGION
*===================================================
gen REGION = ""

* Caribe
replace REGION = "Caribe" if inlist(DPTO, 8, 13, 20, 23, 44, 47, 70)

* Insular
replace REGION = "Insular" if DPTO == 88

* Andina
replace REGION = "Andina" if inlist(DPTO, 5, 11, 15, 17, 25, 41, 54, 63, 66, 68, 73)

* Pacifica
replace REGION = "Pacifica" if inlist(DPTO, 19, 27, 52, 76)

* Orinoquia
replace REGION = "Orinoquia" if inlist(DPTO, 50, 81, 85, 99)

* Amazonia
replace REGION = "Amazonia" if inlist(DPTO, 18, 86, 91, 94, 95, 97)

*===================================================
* 3. QUEDARSE SOLO CON MANUFACTURA (CIIU REV 4: 10-33)
*===================================================
keep if inrange(RAMA2D_R4, 10, 33)

* Solo ocupados con ingreso laboral observado
keep if !missing(INGLABO)
keep if !missing(`fac')
keep if `fac' > 0
keep if !missing(REGION)

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
* HOJA 1: RESULTADOS POR SECTOR (CIIU)
****************************************************

use `base_manuf', clear

tempname memhold1
postfile `memhold1' int RAMA2D_R4 str60 CIIU_NOMBRE ///
    double trabajadores mean_inglabo median_inglabo ///
    using resultados_sector_tmp.dta, replace

levelsof RAMA2D_R4, local(lista_ciiu)

foreach c of local lista_ciiu {

    preserve
        use `base_manuf', clear
        keep if RAMA2D_R4 == `c'

        * trabajadores expandidos
        quietly summarize `fac'
        local trabajadores = r(sum)

        * promedio ponderado
        quietly mean INGLABO [pw=`fac']
        matrix M = e(b)
        local media = M[1,1]

        * mediana ponderada manual
        sort INGLABO
        gen peso = `fac'
        egen peso_total = total(peso)
        gen peso_acum = sum(peso)
        gen frac_acum = peso_acum / peso_total
        quietly summarize INGLABO if frac_acum >= 0.5, meanonly
        local mediana = r(min)

        * nombre
        quietly levelsof CIIU_NOMBRE, local(nombre_ciiu)
        local nombre_txt "`nombre_ciiu'"

        post `memhold1' (`c') ("`nombre_txt'") ///
            (`trabajadores') (`media') (`mediana')
    restore
}

postclose `memhold1'

use resultados_sector_tmp.dta, clear
sort RAMA2D_R4
save resultados_por_sector.dta, replace

****************************************************
* HOJA 2: RESULTADOS POR REGION
****************************************************

use `base_manuf', clear

tempname memhold2
postfile `memhold2' str20 REGION ///
    double trabajadores mean_inglabo median_inglabo ///
    using resultados_region_tmp.dta, replace

levelsof REGION, local(lista_regiones)

foreach r of local lista_regiones {

    preserve
        use `base_manuf', clear
        keep if REGION == "`r'"

        * trabajadores expandidos
        quietly summarize `fac'
        local trabajadores = r(sum)

        * promedio ponderado
        quietly mean INGLABO [pw=`fac']
        matrix M = e(b)
        local media = M[1,1]

        * mediana ponderada manual
        sort INGLABO
        gen peso = `fac'
        egen peso_total = total(peso)
        gen peso_acum = sum(peso)
        gen frac_acum = peso_acum / peso_total
        quietly summarize INGLABO if frac_acum >= 0.5, meanonly
        local mediana = r(min)

        post `memhold2' ("`r'") (`trabajadores') (`media') (`mediana')
    restore
}

postclose `memhold2'

use resultados_region_tmp.dta, clear
sort REGION
save resultados_por_region.dta, replace

****************************************************
* EXPORTAR A EXCEL CON 2 HOJAS
****************************************************

use resultados_por_sector.dta, clear
export excel using "INGLABO_manufactura_2021.xlsx", ///
    sheet("por_sector") firstrow(variables) sheetreplace

use resultados_por_region.dta, clear
export excel using "INGLABO_manufactura_2021.xlsx", ///
    sheet("por_region") firstrow(variables) sheetmodify

di "========================================="
di "Archivo exportado: INGLABO_manufactura_2021.xlsx"
di "========================================="

****************************************************
* INGRESO LABORAL EN MANUFACTURA - GEIH 2022
* CIIU REV 4
****************************************************
cd "C:/Users/jorge/Documents/Databases/GEIH"

use "GEIH_2022_TOTAL.dta", clear

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
* 2. CREAR REGION
*===================================================
gen REGION = ""

* Caribe
replace REGION = "Caribe" if inlist(DPTO, 8, 13, 20, 23, 44, 47, 70)

* Insular
replace REGION = "Insular" if DPTO == 88

* Andina
replace REGION = "Andina" if inlist(DPTO, 5, 11, 15, 17, 25, 41, 54, 63, 66, 68, 73)

* Pacifica
replace REGION = "Pacifica" if inlist(DPTO, 19, 27, 52, 76)

* Orinoquia
replace REGION = "Orinoquia" if inlist(DPTO, 50, 81, 85, 99)

* Amazonia
replace REGION = "Amazonia" if inlist(DPTO, 18, 86, 91, 94, 95, 97)

*===================================================
* 3. QUEDARSE SOLO CON MANUFACTURA (CIIU REV 4: 10-33)
*===================================================
keep if inrange(RAMA2D_R4, 10, 33)

* Solo ocupados con ingreso laboral observado
keep if !missing(INGLABO)
keep if !missing(`fac')
keep if `fac' > 0
keep if !missing(REGION)

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
* HOJA 1: RESULTADOS POR SECTOR (CIIU)
****************************************************

use `base_manuf', clear

tempname memhold1
postfile `memhold1' int RAMA2D_R4 str60 CIIU_NOMBRE ///
    double trabajadores mean_inglabo median_inglabo ///
    using resultados_sector_tmp.dta, replace

levelsof RAMA2D_R4, local(lista_ciiu)

foreach c of local lista_ciiu {

    preserve
        use `base_manuf', clear
        keep if RAMA2D_R4 == `c'

        * trabajadores expandidos
        quietly summarize `fac'
        local trabajadores = r(sum)

        * promedio ponderado
        quietly mean INGLABO [pw=`fac']
        matrix M = e(b)
        local media = M[1,1]

        * mediana ponderada manual
        sort INGLABO
        gen peso = `fac'
        egen peso_total = total(peso)
        gen peso_acum = sum(peso)
        gen frac_acum = peso_acum / peso_total
        quietly summarize INGLABO if frac_acum >= 0.5, meanonly
        local mediana = r(min)

        * nombre
        quietly levelsof CIIU_NOMBRE, local(nombre_ciiu)
        local nombre_txt "`nombre_ciiu'"

        post `memhold1' (`c') ("`nombre_txt'") ///
            (`trabajadores') (`media') (`mediana')
    restore
}

postclose `memhold1'

use resultados_sector_tmp.dta, clear
sort RAMA2D_R4
save resultados_por_sector.dta, replace

****************************************************
* HOJA 2: RESULTADOS POR REGION
****************************************************

use `base_manuf', clear

tempname memhold2
postfile `memhold2' str20 REGION ///
    double trabajadores mean_inglabo median_inglabo ///
    using resultados_region_tmp.dta, replace

levelsof REGION, local(lista_regiones)

foreach r of local lista_regiones {

    preserve
        use `base_manuf', clear
        keep if REGION == "`r'"

        * trabajadores expandidos
        quietly summarize `fac'
        local trabajadores = r(sum)

        * promedio ponderado
        quietly mean INGLABO [pw=`fac']
        matrix M = e(b)
        local media = M[1,1]

        * mediana ponderada manual
        sort INGLABO
        gen peso = `fac'
        egen peso_total = total(peso)
        gen peso_acum = sum(peso)
        gen frac_acum = peso_acum / peso_total
        quietly summarize INGLABO if frac_acum >= 0.5, meanonly
        local mediana = r(min)

        post `memhold2' ("`r'") (`trabajadores') (`media') (`mediana')
    restore
}

postclose `memhold2'

use resultados_region_tmp.dta, clear
sort REGION
save resultados_por_region.dta, replace

****************************************************
* EXPORTAR A EXCEL CON 2 HOJAS
****************************************************

use resultados_por_sector.dta, clear
export excel using "INGLABO_manufactura_2022.xlsx", ///
    sheet("por_sector") firstrow(variables) sheetreplace

use resultados_por_region.dta, clear
export excel using "INGLABO_manufactura_2022.xlsx", ///
    sheet("por_region") firstrow(variables) sheetmodify

di "========================================="
di "Archivo exportado: INGLABO_manufactura_2022.xlsx"
di "========================================="

****************************************************
* INGRESO LABORAL EN MANUFACTURA - GEIH 2023
* CIIU REV 4
****************************************************
cd "C:/Users/jorge/Documents/Databases/GEIH"

use "GEIH_2023_TOTAL.dta", clear

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
* 2. CREAR REGION
*===================================================
gen REGION = ""

* Caribe
replace REGION = "Caribe" if inlist(DPTO, 8, 13, 20, 23, 44, 47, 70)

* Insular
replace REGION = "Insular" if DPTO == 88

* Andina
replace REGION = "Andina" if inlist(DPTO, 5, 11, 15, 17, 25, 41, 54, 63, 66, 68, 73)

* Pacifica
replace REGION = "Pacifica" if inlist(DPTO, 19, 27, 52, 76)

* Orinoquia
replace REGION = "Orinoquia" if inlist(DPTO, 50, 81, 85, 99)

* Amazonia
replace REGION = "Amazonia" if inlist(DPTO, 18, 86, 91, 94, 95, 97)

*===================================================
* 3. QUEDARSE SOLO CON MANUFACTURA (CIIU REV 4: 10-33)
*===================================================
keep if inrange(RAMA2D_R4, 10, 33)

* Solo ocupados con ingreso laboral observado
keep if !missing(INGLABO)
keep if !missing(`fac')
keep if `fac' > 0
keep if !missing(REGION)

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
* HOJA 1: RESULTADOS POR SECTOR (CIIU)
****************************************************

use `base_manuf', clear

tempname memhold1
postfile `memhold1' int RAMA2D_R4 str60 CIIU_NOMBRE ///
    double trabajadores mean_inglabo median_inglabo ///
    using resultados_sector_tmp.dta, replace

levelsof RAMA2D_R4, local(lista_ciiu)

foreach c of local lista_ciiu {

    preserve
        use `base_manuf', clear
        keep if RAMA2D_R4 == `c'

        * trabajadores expandidos
        quietly summarize `fac'
        local trabajadores = r(sum)

        * promedio ponderado
        quietly mean INGLABO [pw=`fac']
        matrix M = e(b)
        local media = M[1,1]

        * mediana ponderada manual
        sort INGLABO
        gen peso = `fac'
        egen peso_total = total(peso)
        gen peso_acum = sum(peso)
        gen frac_acum = peso_acum / peso_total
        quietly summarize INGLABO if frac_acum >= 0.5, meanonly
        local mediana = r(min)

        * nombre
        quietly levelsof CIIU_NOMBRE, local(nombre_ciiu)
        local nombre_txt "`nombre_ciiu'"

        post `memhold1' (`c') ("`nombre_txt'") ///
            (`trabajadores') (`media') (`mediana')
    restore
}

postclose `memhold1'

use resultados_sector_tmp.dta, clear
sort RAMA2D_R4
save resultados_por_sector.dta, replace

****************************************************
* HOJA 2: RESULTADOS POR REGION
****************************************************

use `base_manuf', clear

tempname memhold2
postfile `memhold2' str20 REGION ///
    double trabajadores mean_inglabo median_inglabo ///
    using resultados_region_tmp.dta, replace

levelsof REGION, local(lista_regiones)

foreach r of local lista_regiones {

    preserve
        use `base_manuf', clear
        keep if REGION == "`r'"

        * trabajadores expandidos
        quietly summarize `fac'
        local trabajadores = r(sum)

        * promedio ponderado
        quietly mean INGLABO [pw=`fac']
        matrix M = e(b)
        local media = M[1,1]

        * mediana ponderada manual
        sort INGLABO
        gen peso = `fac'
        egen peso_total = total(peso)
        gen peso_acum = sum(peso)
        gen frac_acum = peso_acum / peso_total
        quietly summarize INGLABO if frac_acum >= 0.5, meanonly
        local mediana = r(min)

        post `memhold2' ("`r'") (`trabajadores') (`media') (`mediana')
    restore
}

postclose `memhold2'

use resultados_region_tmp.dta, clear
sort REGION
save resultados_por_region.dta, replace

****************************************************
* EXPORTAR A EXCEL CON 2 HOJAS
****************************************************

use resultados_por_sector.dta, clear
export excel using "INGLABO_manufactura_2023.xlsx", ///
    sheet("por_sector") firstrow(variables) sheetreplace

use resultados_por_region.dta, clear
export excel using "INGLABO_manufactura_2023.xlsx", ///
    sheet("por_region") firstrow(variables) sheetmodify

di "========================================="
di "Archivo exportado: INGLABO_manufactura_2023.xlsx"
di "========================================="

****************************************************
* INGRESO LABORAL EN MANUFACTURA - GEIH 2024
* CIIU REV 4
****************************************************
cd "C:/Users/jorge/Documents/Databases/GEIH"

use "GEIH_2024_TOTAL.dta", clear

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
* 2. CREAR REGION
*===================================================
gen REGION = ""

* Caribe
replace REGION = "Caribe" if inlist(DPTO, 8, 13, 20, 23, 44, 47, 70)

* Insular
replace REGION = "Insular" if DPTO == 88

* Andina
replace REGION = "Andina" if inlist(DPTO, 5, 11, 15, 17, 25, 41, 54, 63, 66, 68, 73)

* Pacifica
replace REGION = "Pacifica" if inlist(DPTO, 19, 27, 52, 76)

* Orinoquia
replace REGION = "Orinoquia" if inlist(DPTO, 50, 81, 85, 99)

* Amazonia
replace REGION = "Amazonia" if inlist(DPTO, 18, 86, 91, 94, 95, 97)

*===================================================
* 3. QUEDARSE SOLO CON MANUFACTURA (CIIU REV 4: 10-33)
*===================================================
keep if inrange(RAMA2D_R4, 10, 33)

* Solo ocupados con ingreso laboral observado
keep if !missing(INGLABO)
keep if !missing(`fac')
keep if `fac' > 0
keep if !missing(REGION)

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
* HOJA 1: RESULTADOS POR SECTOR (CIIU)
****************************************************

use `base_manuf', clear

tempname memhold1
postfile `memhold1' int RAMA2D_R4 str60 CIIU_NOMBRE ///
    double trabajadores mean_inglabo median_inglabo ///
    using resultados_sector_tmp.dta, replace

levelsof RAMA2D_R4, local(lista_ciiu)

foreach c of local lista_ciiu {

    preserve
        use `base_manuf', clear
        keep if RAMA2D_R4 == `c'

        * trabajadores expandidos
        quietly summarize `fac'
        local trabajadores = r(sum)

        * promedio ponderado
        quietly mean INGLABO [pw=`fac']
        matrix M = e(b)
        local media = M[1,1]

        * mediana ponderada manual
        sort INGLABO
        gen peso = `fac'
        egen peso_total = total(peso)
        gen peso_acum = sum(peso)
        gen frac_acum = peso_acum / peso_total
        quietly summarize INGLABO if frac_acum >= 0.5, meanonly
        local mediana = r(min)

        * nombre
        quietly levelsof CIIU_NOMBRE, local(nombre_ciiu)
        local nombre_txt "`nombre_ciiu'"

        post `memhold1' (`c') ("`nombre_txt'") ///
            (`trabajadores') (`media') (`mediana')
    restore
}

postclose `memhold1'

use resultados_sector_tmp.dta, clear
sort RAMA2D_R4
save resultados_por_sector.dta, replace

****************************************************
* HOJA 2: RESULTADOS POR REGION
****************************************************

use `base_manuf', clear

tempname memhold2
postfile `memhold2' str20 REGION ///
    double trabajadores mean_inglabo median_inglabo ///
    using resultados_region_tmp.dta, replace

levelsof REGION, local(lista_regiones)

foreach r of local lista_regiones {

    preserve
        use `base_manuf', clear
        keep if REGION == "`r'"

        * trabajadores expandidos
        quietly summarize `fac'
        local trabajadores = r(sum)

        * promedio ponderado
        quietly mean INGLABO [pw=`fac']
        matrix M = e(b)
        local media = M[1,1]

        * mediana ponderada manual
        sort INGLABO
        gen peso = `fac'
        egen peso_total = total(peso)
        gen peso_acum = sum(peso)
        gen frac_acum = peso_acum / peso_total
        quietly summarize INGLABO if frac_acum >= 0.5, meanonly
        local mediana = r(min)

        post `memhold2' ("`r'") (`trabajadores') (`media') (`mediana')
    restore
}

postclose `memhold2'

use resultados_region_tmp.dta, clear
sort REGION
save resultados_por_region.dta, replace

****************************************************
* EXPORTAR A EXCEL CON 2 HOJAS
****************************************************

use resultados_por_sector.dta, clear
export excel using "INGLABO_manufactura_2024.xlsx", ///
    sheet("por_sector") firstrow(variables) sheetreplace

use resultados_por_region.dta, clear
export excel using "INGLABO_manufactura_2024.xlsx", ///
    sheet("por_region") firstrow(variables) sheetmodify

di "========================================="
di "Archivo exportado: INGLABO_manufactura_2024.xlsx"
di "========================================="

****************************************************
* INGRESO LABORAL EN MANUFACTURA - GEIH 2025
* CIIU REV 4
****************************************************
cd "C:/Users/jorge/Documents/Databases/GEIH"

use "GEIH_2025_TOTAL.dta", clear

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
* 2. CREAR REGION
*===================================================
gen REGION = ""

* Caribe
replace REGION = "Caribe" if inlist(DPTO, 8, 13, 20, 23, 44, 47, 70)

* Insular
replace REGION = "Insular" if DPTO == 88

* Andina
replace REGION = "Andina" if inlist(DPTO, 5, 11, 15, 17, 25, 41, 54, 63, 66, 68, 73)

* Pacifica
replace REGION = "Pacifica" if inlist(DPTO, 19, 27, 52, 76)

* Orinoquia
replace REGION = "Orinoquia" if inlist(DPTO, 50, 81, 85, 99)

* Amazonia
replace REGION = "Amazonia" if inlist(DPTO, 18, 86, 91, 94, 95, 97)

*===================================================
* 3. QUEDARSE SOLO CON MANUFACTURA (CIIU REV 4: 10-33)
*===================================================
keep if inrange(RAMA2D_R4, 10, 33)

* Solo ocupados con ingreso laboral observado
keep if !missing(INGLABO)
keep if !missing(`fac')
keep if `fac' > 0
keep if !missing(REGION)

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
* HOJA 1: RESULTADOS POR SECTOR (CIIU)
****************************************************

use `base_manuf', clear

tempname memhold1
postfile `memhold1' int RAMA2D_R4 str60 CIIU_NOMBRE ///
    double trabajadores mean_inglabo median_inglabo ///
    using resultados_sector_tmp.dta, replace

levelsof RAMA2D_R4, local(lista_ciiu)

foreach c of local lista_ciiu {

    preserve
        use `base_manuf', clear
        keep if RAMA2D_R4 == `c'

        * trabajadores expandidos
        quietly summarize `fac'
        local trabajadores = r(sum)

        * promedio ponderado
        quietly mean INGLABO [pw=`fac']
        matrix M = e(b)
        local media = M[1,1]

        * mediana ponderada manual
        sort INGLABO
        gen peso = `fac'
        egen peso_total = total(peso)
        gen peso_acum = sum(peso)
        gen frac_acum = peso_acum / peso_total
        quietly summarize INGLABO if frac_acum >= 0.5, meanonly
        local mediana = r(min)

        * nombre
        quietly levelsof CIIU_NOMBRE, local(nombre_ciiu)
        local nombre_txt "`nombre_ciiu'"

        post `memhold1' (`c') ("`nombre_txt'") ///
            (`trabajadores') (`media') (`mediana')
    restore
}

postclose `memhold1'

use resultados_sector_tmp.dta, clear
sort RAMA2D_R4
save resultados_por_sector.dta, replace

****************************************************
* HOJA 2: RESULTADOS POR REGION
****************************************************

use `base_manuf', clear

tempname memhold2
postfile `memhold2' str20 REGION ///
    double trabajadores mean_inglabo median_inglabo ///
    using resultados_region_tmp.dta, replace

levelsof REGION, local(lista_regiones)

foreach r of local lista_regiones {

    preserve
        use `base_manuf', clear
        keep if REGION == "`r'"

        * trabajadores expandidos
        quietly summarize `fac'
        local trabajadores = r(sum)

        * promedio ponderado
        quietly mean INGLABO [pw=`fac']
        matrix M = e(b)
        local media = M[1,1]

        * mediana ponderada manual
        sort INGLABO
        gen peso = `fac'
        egen peso_total = total(peso)
        gen peso_acum = sum(peso)
        gen frac_acum = peso_acum / peso_total
        quietly summarize INGLABO if frac_acum >= 0.5, meanonly
        local mediana = r(min)

        post `memhold2' ("`r'") (`trabajadores') (`media') (`mediana')
    restore
}

postclose `memhold2'

use resultados_region_tmp.dta, clear
sort REGION
save resultados_por_region.dta, replace

****************************************************
* EXPORTAR A EXCEL CON 2 HOJAS
****************************************************

use resultados_por_sector.dta, clear
export excel using "INGLABO_manufactura_2025.xlsx", ///
    sheet("por_sector") firstrow(variables) sheetreplace

use resultados_por_region.dta, clear
export excel using "INGLABO_manufactura_2025.xlsx", ///
    sheet("por_region") firstrow(variables) sheetmodify

di "========================================="
di "Archivo exportado: INGLABO_manufactura_2025.xlsx"
di "========================================="
