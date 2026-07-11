* ==============================================================================
* IA_01_ExposureDatabase.do
* Objetivo:
*   Asignar a BaseIA los indicadores de exposicion a IA generativa de la OIT/ILO
*   usando la correlativa local:
*     DocumentacionAuxiliar/correlativa_IA_ISCO08_GEIH_OFICIO_C8.xlsx
*
* Resultado:
*   - Merge a 4 digitos: GEIH oficio_c8_4d <-> ISCO-08 oficio_c8
*   - Merge a 2 digitos: GEIH oficio_c8_2d_cod <-> resumen ISCO-08 oficio_2d
*   - Genera variables nuevas de exposicion IA en memoria.
*
* Nota importante:
*   Este do-file NO guarda ni sobrescribe BaseIA.dta. Al finalizar, la base queda
*   abierta en memoria con las nuevas variables para inspeccion.
* ==============================================================================

clear all
set more off

* ------------------------------------------------------------------------------
* 0. Rutas del proyecto
* ------------------------------------------------------------------------------
local project_root = c(pwd)
local base_ia      "`project_root'/Datos/Processed/BaseIA.dta"
local ia_xlsx      "`project_root'/DocumentacionAuxiliar/correlativa_IA_ISCO08_GEIH_OFICIO_C8.xlsx"

capture confirm file "`base_ia'"
if _rc {
    di as error "No encuentro BaseIA.dta en: `base_ia'"
    di as error "Ejecuta este do-file desde la raiz del proyecto: Javeriana"
    exit 601
}

capture confirm file "`ia_xlsx'"
if _rc {
    di as error "No encuentro la correlativa IA en: `ia_xlsx'"
    exit 601
}

tempfile ia_4d ia_2d

* ------------------------------------------------------------------------------
* 1. Preparar correlativa IA a 4 digitos
* ------------------------------------------------------------------------------
import excel using "`ia_xlsx'", sheet("correlativa_4d") firstrow clear

keep oficio_c8 oficio_2d occupation_name_isco08 ai_exposure_mean ///
     ai_exposure_sd ai_exposure_group ai_exposure_order ///
     high_exposure_g3_g4 source_note

tempvar raw4 raw2

capture confirm numeric variable oficio_c8
if !_rc {
    gen str20 `raw4' = string(oficio_c8, "%20.0f")
}
else {
    gen str20 `raw4' = strtrim(oficio_c8)
}

capture confirm numeric variable oficio_2d
if !_rc {
    gen str20 `raw2' = string(oficio_2d, "%20.0f")
}
else {
    gen str20 `raw2' = strtrim(oficio_2d)
}

replace `raw4' = subinstr(`raw4', ".0", "", .)
replace `raw2' = subinstr(`raw2', ".0", "", .)
replace `raw4' = "" if `raw4' == "."
replace `raw2' = "" if `raw2' == "."

gen str4 oficio_c8_key = substr("0000" + `raw4', length("0000" + `raw4') - 3, 4) if `raw4' != ""
gen str2 oficio_2d_key = substr("00" + `raw2', length("00" + `raw2') - 1, 2) if `raw2' != ""

rename occupation_name_isco08 ia_occupation_name_4d
rename ai_exposure_mean ia_exposure_mean_4d
rename ai_exposure_sd ia_exposure_sd_4d
rename ai_exposure_group ia_exposure_group_4d
rename ai_exposure_order ia_exposure_order_4d
rename high_exposure_g3_g4 ia_high_exposure_g3_g4_4d
rename source_note ia_source_note_4d

keep oficio_c8_key ia_occupation_name_4d ia_exposure_mean_4d ///
     ia_exposure_sd_4d ia_exposure_group_4d ia_exposure_order_4d ///
     ia_high_exposure_g3_g4_4d ia_source_note_4d

duplicates drop oficio_c8_key, force
isid oficio_c8_key
save `ia_4d', replace

* ------------------------------------------------------------------------------
* 2. Preparar correlativa IA agregada a 2 digitos
* ------------------------------------------------------------------------------
import excel using "`ia_xlsx'", sheet("resumen_2d") firstrow clear

keep oficio_2d n_isco08_4d_occupations ai_exposure_mean_unweighted_2d ///
     ai_exposure_min_2d ai_exposure_max_2d modal_exposure_group_2d ///
     share_high_exposure_g3_g4_2d note

tempvar raw2b

capture confirm numeric variable oficio_2d
if !_rc {
    gen str20 `raw2b' = string(oficio_2d, "%20.0f")
}
else {
    gen str20 `raw2b' = strtrim(oficio_2d)
}

replace `raw2b' = subinstr(`raw2b', ".0", "", .)
replace `raw2b' = "" if `raw2b' == "."
gen str2 oficio_2d_key = substr("00" + `raw2b', length("00" + `raw2b') - 1, 2) if `raw2b' != ""

rename n_isco08_4d_occupations ia_n_isco08_4d_2d
rename ai_exposure_mean_unweighted_2d ia_exposure_mean_2d
rename ai_exposure_min_2d ia_exposure_min_2d
rename ai_exposure_max_2d ia_exposure_max_2d
rename modal_exposure_group_2d ia_exposure_group_2d
rename share_high_exposure_g3_g4_2d ia_share_high_g3_g4_2d
rename note ia_source_note_2d

keep oficio_2d_key ia_n_isco08_4d_2d ia_exposure_mean_2d ///
     ia_exposure_min_2d ia_exposure_max_2d ia_exposure_group_2d ///
     ia_share_high_g3_g4_2d ia_source_note_2d

duplicates drop oficio_2d_key, force
isid oficio_2d_key
save `ia_2d', replace

* ------------------------------------------------------------------------------
* 3. Abrir BaseIA y construir llaves ocupacionales GEIH
* ------------------------------------------------------------------------------
use "`base_ia'", clear

local oficio4 ""
foreach v in oficio_c8_4d OFICIO_C8 oficio_c8 oficio_cod OFICIO oficio {
    capture confirm variable `v'
    if !_rc & "`oficio4'" == "" local oficio4 "`v'"
}

if "`oficio4'" == "" {
    di as error "No encuentro variable de ocupacion a 4 digitos en BaseIA."
    di as error "Variables buscadas: oficio_c8_4d, OFICIO_C8, oficio_c8, oficio_cod, OFICIO, oficio"
    exit 111
}

di as result "Variable usada como ocupacion 4 digitos: `oficio4'"

tempvar geih_raw4
capture confirm numeric variable `oficio4'
if !_rc {
    gen str20 `geih_raw4' = string(`oficio4', "%20.0f")
}
else {
    gen str20 `geih_raw4' = strtrim(`oficio4')
}

replace `geih_raw4' = subinstr(`geih_raw4', ".0", "", .)
replace `geih_raw4' = "" if `geih_raw4' == "."
gen str4 oficio_c8_key = substr("0000" + `geih_raw4', length("0000" + `geih_raw4') - 3, 4) if `geih_raw4' != ""

local oficio2 ""
foreach v in oficio_c8_2d_cod OFICIO_C8_2D oficio_c8_2d oficio2d_cod oficio_2d {
    capture confirm variable `v'
    if !_rc & "`oficio2'" == "" local oficio2 "`v'"
}

if "`oficio2'" != "" {
    di as result "Variable usada como ocupacion 2 digitos: `oficio2'"

    tempvar geih_raw2
    capture confirm numeric variable `oficio2'
    if !_rc {
        gen str20 `geih_raw2' = string(`oficio2', "%20.0f")
    }
    else {
        gen str20 `geih_raw2' = strtrim(`oficio2')
    }

    replace `geih_raw2' = subinstr(`geih_raw2', ".0", "", .)
    replace `geih_raw2' = "" if `geih_raw2' == "."
    gen str2 oficio_2d_key = substr("00" + `geih_raw2', length("00" + `geih_raw2') - 1, 2) if `geih_raw2' != ""
}
else {
    di as text "No encontre variable 2 digitos; se construye como los dos primeros digitos de oficio_c8_key."
    gen str2 oficio_2d_key = substr(oficio_c8_key, 1, 2) if oficio_c8_key != ""
}

label variable oficio_c8_key "Llave ocupacion CIUO/ISCO-08 4 digitos para merge IA"
label variable oficio_2d_key "Llave ocupacion CIUO/ISCO-08 2 digitos para merge IA"

* ------------------------------------------------------------------------------
* 4. Merge IA a 4 digitos
* ------------------------------------------------------------------------------
merge m:1 oficio_c8_key using `ia_4d', keep(master match) gen(_merge_ia_4d)

gen byte ia_match_4d = (_merge_ia_4d == 3)
label variable ia_match_4d "Indicador de match IA-OIT a 4 digitos"

label variable ia_occupation_name_4d "OIT/ILO ISCO-08 occupation name, 4 digits"
label variable ia_exposure_mean_4d "Exposicion IA generativa OIT/ILO, media, 4 digitos"
label variable ia_exposure_sd_4d "Exposicion IA generativa OIT/ILO, desviacion estandar, 4 digitos"
label variable ia_exposure_group_4d "Grupo/gradiente de exposicion IA OIT/ILO, 4 digitos"
label variable ia_exposure_order_4d "Orden numerico del gradiente IA OIT/ILO, 4 digitos"
label variable ia_high_exposure_g3_g4_4d "Alta exposicion IA, gradientes 3-4, 4 digitos"
label variable ia_source_note_4d "Fuente del indice IA OIT/ILO, 4 digitos"

* ------------------------------------------------------------------------------
* 5. Merge IA a 2 digitos
* ------------------------------------------------------------------------------
merge m:1 oficio_2d_key using `ia_2d', keep(master match) gen(_merge_ia_2d)

gen byte ia_match_2d = (_merge_ia_2d == 3)
label variable ia_match_2d "Indicador de match IA-OIT agregado a 2 digitos"

label variable ia_n_isco08_4d_2d "Numero ocupaciones ISCO-08 4d usadas en agregado IA 2d"
label variable ia_exposure_mean_2d "Exposicion IA media no ponderada, agregado 2 digitos"
label variable ia_exposure_min_2d "Exposicion IA minima, agregado 2 digitos"
label variable ia_exposure_max_2d "Exposicion IA maxima, agregado 2 digitos"
label variable ia_exposure_group_2d "Grupo modal de exposicion IA, agregado 2 digitos"
label variable ia_share_high_g3_g4_2d "Proporcion ocupaciones 4d con alta exposicion, agregado 2d"
label variable ia_source_note_2d "Nota metodologica del agregado IA 2 digitos"

* ------------------------------------------------------------------------------
* 6. Variable recomendada de trabajo: usa 4 digitos cuando existe; 2 digitos como fallback
* ------------------------------------------------------------------------------
gen double ia_exposure_mean = ia_exposure_mean_4d
replace ia_exposure_mean = ia_exposure_mean_2d if missing(ia_exposure_mean) & !missing(ia_exposure_mean_2d)
label variable ia_exposure_mean "Exposicion IA recomendada: 4d si existe, 2d como fallback"

gen str8 ia_exposure_source = ""
replace ia_exposure_source = "4d" if !missing(ia_exposure_mean_4d)
replace ia_exposure_source = "2d" if missing(ia_exposure_mean_4d) & !missing(ia_exposure_mean_2d)
replace ia_exposure_source = "sin_match" if missing(ia_exposure_mean_4d) & missing(ia_exposure_mean_2d)
label variable ia_exposure_source "Fuente usada para ia_exposure_mean"

gen str40 ia_exposure_group = ia_exposure_group_4d
replace ia_exposure_group = ia_exposure_group_2d if ia_exposure_group == "" & ia_exposure_group_2d != ""
replace ia_exposure_group = "Sin match" if ia_exposure_group == ""
label variable ia_exposure_group "Grupo de exposicion IA recomendado: 4d si existe, 2d como fallback"

* ------------------------------------------------------------------------------
* 7. Diagnostico rapido en pantalla
* ------------------------------------------------------------------------------
di as text "------------------------------------------------------------"
di as text "Diagnostico merge IA - BaseIA en memoria, NO guardada"
di as text "------------------------------------------------------------"
tab _merge_ia_4d
tab _merge_ia_2d
tab ia_exposure_source
summ ia_exposure_mean_4d ia_exposure_mean_2d ia_exposure_mean

* ------------------------------------------------------------------------------
* 8. Detectar variables para tablas de salida
* ------------------------------------------------------------------------------
local fex_var ""
foreach v in fex fex_c18 FEX_C18 factor_expansion fex_2018 {
    capture confirm numeric variable `v'
    if !_rc & "`fex_var'" == "" local fex_var "`v'"
}

local label4 ""
foreach v in oficio_c8_label ia_occupation_name_4d occupation_name_isco08 {
    capture confirm variable `v'
    if !_rc & "`label4'" == "" local label4 "`v'"
}

if "`label4'" == "" {
    gen str80 oficio_c8_label_tabla = "Sin label ocupacional"
    local label4 "oficio_c8_label_tabla"
}

local anio_var ""
foreach v in anio año ano year ANIO {
    capture confirm numeric variable `v'
    if !_rc & "`anio_var'" == "" local anio_var "`v'"
}

if "`anio_var'" == "" {
    di as error "No encuentro variable de año en BaseIA."
    di as error "Variables buscadas: anio, año, ano, year, ANIO"
    exit 111
}

local sector_var ""
foreach v in sector_label sector_hom_label rama_label rama2d_label sector_2d subrama_det_label {
    capture confirm variable `v'
    if !_rc & "`sector_var'" == "" local sector_var "`v'"
}

if "`sector_var'" == "" {
    foreach v in sector_hom_cod sector_cod rama2d_cod rama4d_cod subrama_det_cod {
        capture confirm numeric variable `v'
        if !_rc & "`sector_var'" == "" {
            capture decode `v', gen(sector_label_tabla)
            if _rc {
                gen str40 sector_label_tabla = string(`v', "%12.0f")
            }
            replace sector_label_tabla = string(`v', "%12.0f") if sector_label_tabla == "" & !missing(`v')
            local sector_var "sector_label_tabla"
        }
    }
}

if "`sector_var'" == "" {
    gen str40 sector_label_tabla = "Sin sector/rama disponible"
    local sector_var "sector_label_tabla"
}

* ------------------------------------------------------------------------------
* 9. Tabla en pantalla: ocupaciones ordenadas por exposicion IA de mayor a menor
* ------------------------------------------------------------------------------
preserve
    keep if !missing(ia_exposure_mean)

    if "`fex_var'" != "" {
        gen double ocupados_ponderados = `fex_var'
        collapse ///
            (mean) ia_exposure_mean ia_exposure_mean_4d ia_exposure_mean_2d ///
            (sum) ocupados_ponderados ///
            (count) trabajadores_muestra = ia_exposure_mean, ///
            by(oficio_c8_key `label4' ia_exposure_source)
    }
    else {
        gen double ocupados_ponderados = .
        collapse ///
            (mean) ia_exposure_mean ia_exposure_mean_4d ia_exposure_mean_2d ///
            (count) trabajadores_muestra = ia_exposure_mean, ///
            by(oficio_c8_key `label4' ia_exposure_source)
    }

    gsort -ia_exposure_mean -trabajadores_muestra

    format ia_exposure_mean ia_exposure_mean_4d ia_exposure_mean_2d %6.3f
    format ocupados_ponderados %15.0fc

    di as text "------------------------------------------------------------"
    di as text "Ocupaciones ordenadas por exposicion IA, de mayor a menor"
    di as text "Label ocupacional tomado de: `label4'"
    if "`fex_var'" != "" {
        di as text "Ocupados ponderados calculados con factor: `fex_var'"
    }
    else {
        di as text "No se encontro factor de expansion; se reporta solo muestra."
    }
    di as text "------------------------------------------------------------"

    list oficio_c8_key `label4' ia_exposure_mean ia_exposure_source ///
         trabajadores_muestra ocupados_ponderados, ///
         noobs abbreviate(32) separator(0)
restore

* ------------------------------------------------------------------------------
* 10. Exportar Excel ordenado en Outputs/tables
* ------------------------------------------------------------------------------
capture mkdir "`project_root'/Outputs"
capture mkdir "`project_root'/Outputs/tables"

local xlsx_out "`project_root'/Outputs/tables/IA_exposicion_ocupaciones_BaseIA.xlsx"

preserve
    keep if !missing(ia_exposure_mean)

    if "`fex_var'" != "" {
        gen double numero_trabajadores = `fex_var'
    }
    else {
        di as text "Advertencia: no se encontro factor de expansion; numero_trabajadores se calcula como conteo muestral."
        gen double numero_trabajadores = 1
    }

    collapse ///
        (sum) numero_trabajadores ///
        (mean) exposicion_ia = ia_exposure_mean, ///
        by(`anio_var' oficio_c8_key `label4' ia_exposure_group)

    rename `anio_var' anio
    rename oficio_c8_key codigo_ocupacion
    rename `label4' ocupacion
    rename ia_exposure_group grupo_exposicion_ia

    bysort anio: egen double total_trabajadores_anio = total(numero_trabajadores)
    gen double participacion_empleo = numero_trabajadores / total_trabajadores_anio

    order anio codigo_ocupacion ocupacion numero_trabajadores participacion_empleo exposicion_ia grupo_exposicion_ia
    gsort anio -exposicion_ia -numero_trabajadores

    label variable anio "Año"
    label variable codigo_ocupacion "Código ocupación"
    label variable ocupacion "Ocupación"
    label variable numero_trabajadores "Número de trabajadores"
    label variable participacion_empleo "Participación en el empleo total del año"
    label variable exposicion_ia "Exposición IA"
    label variable grupo_exposicion_ia "Grupo de exposición IA"

    format numero_trabajadores %15.0fc
    format participacion_empleo %9.4f
    format exposicion_ia %6.3f

    export excel using "`xlsx_out'", sheet("01_ocupaciones") firstrow(varlabels) replace
restore

preserve
    keep if !missing(ia_exposure_mean)

    if "`fex_var'" != "" {
        gen double numero_trabajadores = `fex_var'
    }
    else {
        gen double numero_trabajadores = 1
    }

    collapse ///
        (sum) numero_trabajadores ///
        (mean) exposicion_ia = ia_exposure_mean, ///
        by(`anio_var' `sector_var' oficio_c8_key `label4' ia_exposure_group)

    rename `anio_var' anio
    rename `sector_var' sector_rama
    rename oficio_c8_key codigo_ocupacion
    rename `label4' ocupacion
    rename ia_exposure_group grupo_exposicion_ia

    bysort anio sector_rama: egen double total_trabajadores_sector = total(numero_trabajadores)
    bysort anio: egen double total_trabajadores_anio = total(numero_trabajadores)
    gen double participacion_en_sector = numero_trabajadores / total_trabajadores_sector
    gen double participacion_empleo_total = numero_trabajadores / total_trabajadores_anio

    order anio sector_rama codigo_ocupacion ocupacion numero_trabajadores participacion_en_sector participacion_empleo_total exposicion_ia grupo_exposicion_ia
    gsort anio sector_rama -exposicion_ia -numero_trabajadores

    label variable anio "Año"
    label variable sector_rama "Rama o sector económico"
    label variable codigo_ocupacion "Código ocupación"
    label variable ocupacion "Ocupación"
    label variable numero_trabajadores "Número de trabajadores"
    label variable participacion_en_sector "Participación de la ocupación dentro del sector"
    label variable participacion_empleo_total "Participación de la ocupación-sector en el empleo total"
    label variable exposicion_ia "Exposición IA"
    label variable grupo_exposicion_ia "Grupo de exposición IA"

    format numero_trabajadores %15.0fc
    format participacion_en_sector participacion_empleo_total %9.4f
    format exposicion_ia %6.3f

    export excel using "`xlsx_out'", sheet("02_ocupacion_sector") firstrow(varlabels) sheetreplace
restore

di as result "Excel generado: `xlsx_out'"

di as result "Listo: BaseIA queda en memoria con variables de exposicion IA a 4d y 2d."
di as text   "Recuerda: este do-file no guarda la base. Usa save manualmente solo si ya verificaste los resultados."
