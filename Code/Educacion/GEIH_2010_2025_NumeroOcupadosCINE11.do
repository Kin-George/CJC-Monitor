* ==============================================================================
* GEIH_2010_2025_NumeroOcupadosCINE11.do
* Objetivo:
*   Revisar el numero de ocupados por nivel educativo CINE 11, año por año,
*   de 2010 a 2025, usando el panel ya armado con la variable homologada
*   (Outputs/tables/BaseconCINE11.dta, generado por
*   CreacionPanelLimpio2008-2025.do a partir de BaseConsolidada.do).
*
* Metodologia:
*   - Usa "fex" (ya es el factor de expansion anualizado, viene armado en
*     el panel) y "cine11_hom_cod", la version de CINE 11 ya homologada en
*     una sola escala para todo el panel (1 Ninguno, 2 Básica primaria,
*     3 Básica secundaria, 4 Educación media, 5 Técnica/tecnológica,
*     6 Universitaria, 7 Posgrado, 98 No determinado). No hace falta
*     volver a construir CINE 11 aqui, ni distinguir marco 2005/2018: el
*     panel ya lo dejo resuelto.
*   - No filtra por OCI porque BaseconCINE11.dta ya es, de por si, la base
*     de personas ocupadas (unidad "persona ocupada").
*
* Salida:
*   - Imprime en consola una tabla año x categoria (numero de ocupados).
*   - Exporta la misma tabla a
*     Outputs/tables/GEIH_2010_2025_NumeroOcupadosCINE11.xlsx
* ==============================================================================

clear all
set more off

* ------------------------------------------------------------------------------
* 0. Ruta de la base
* ------------------------------------------------------------------------------
local base_path "C:\Users\jorge\Documents\Databases\GEIH\Outputs\tables\BaseconCINE11.dta"

capture confirm file "`base_path'"
if _rc {
    capture confirm file "Outputs/tables/BaseconCINE11.dta"
    if !_rc local base_path "Outputs/tables/BaseconCINE11.dta"
}

capture confirm file "`base_path'"
if _rc {
    capture confirm file "../Outputs/tables/BaseconCINE11.dta"
    if !_rc local base_path "../Outputs/tables/BaseconCINE11.dta"
}

capture confirm file "`base_path'"
if _rc {
    di as error "No encontre BaseconCINE11.dta."
    di as error "Edita la linea local base_path con la ruta completa de la base."
    exit 601
}

use anio cine11_hom_cod fex using "`base_path'", clear

foreach v in anio cine11_hom_cod fex {
    capture confirm variable `v'
    if _rc {
        di as error "No encuentro la variable requerida: `v'"
        exit 111
    }
}

* ------------------------------------------------------------------------------
* 1. Diagnostico rapido
* ------------------------------------------------------------------------------
di as text "------------------------------------------------------------"
di as text "Diagnostico rapido"
di as text "------------------------------------------------------------"
tab anio, missing
tab cine11_hom_cod, missing

* ------------------------------------------------------------------------------
* 2. Numero de ocupados por año x CINE 11
* ------------------------------------------------------------------------------
collapse (sum) ocupados = fex, by(anio cine11_hom_cod)

reshape wide ocupados, i(anio) j(cine11_hom_cod)

capture rename ocupados1  Ninguno
capture rename ocupados2  BasicaPrimaria
capture rename ocupados3  BasicaSecundaria
capture rename ocupados4  EducacionMedia
capture rename ocupados5  TecnicaTecnologica
capture rename ocupados6  Universitaria
capture rename ocupados7  Posgrado
capture rename ocupados98 NoDeterminado

foreach v of varlist Ninguno-NoDeterminado {
    capture confirm variable `v'
    if !_rc replace `v' = 0 if missing(`v')
}

egen double TotalOcupados = rowtotal(Ninguno-NoDeterminado)

format Ninguno-TotalOcupados %16.0fc

di as text "------------------------------------------------------------"
di as text "Numero de ocupados por año y nivel CINE 11, 2010-2025"
di as text "------------------------------------------------------------"
list, noobs separator(0)

* ------------------------------------------------------------------------------
* 3. Exportar a Excel
* ------------------------------------------------------------------------------
capture mkdir "Outputs"
capture mkdir "Outputs/tables"

export excel using "Outputs/tables/GEIH_2010_2025_NumeroOcupadosCINE11.xlsx", ///
    firstrow(variables) replace

di as text "------------------------------------------------------------"
di as text "Excel guardado en:"
di as text "Outputs/tables/GEIH_2010_2025_NumeroOcupadosCINE11.xlsx"
di as text "------------------------------------------------------------"
