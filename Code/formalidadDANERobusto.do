** Codigo STATA DANE Informalidad **

// EI = 0; ocupados informales; EI = 1; ocupados formales

/*SECTOR INFORMAL*/								

gen ANIOS = PER - 1

*====================================================
* Crear OFICIO_C8_2D robusto
* Funciona si OFICIO_C8 viene string o numérica
*====================================================

capture drop OFICIO_C8_2D

capture confirm string variable OFICIO_C8

if !_rc {
    
    gen str2 OFICIO_C8_2D = substr(strtrim(OFICIO_C8), 1, 2) ///
        if !missing(OFICIO_C8)
}

else {
    
    gen str2 OFICIO_C8_2D = substr(string(OFICIO_C8, "%04.0f"), 1, 2) ///
        if !missing(OFICIO_C8)
}

* Convertir a numérica si el resto del código la usa como número
destring OFICIO_C8_2D, replace

label var OFICIO_C8_2D "Código oficio C8 a 2 dígitos"			

destring OFICIO_C8_2D, replace		

				

gen FORMAL=. if P6430==3				

replace FORMAL=0 if P6430==6		

*====================================================
* Crear RAMA2D_R4 numérica robusta
* Funciona si RAMA2D_R4 viene string o numérica
*====================================================

capture drop RAMA2D_R4_num

capture confirm numeric variable RAMA2D_R4

if !_rc {
    
    gen double RAMA2D_R4_num = RAMA2D_R4
}

else {
    
    gen str10 RAMA2D_R4_str = strtrim(RAMA2D_R4)
    destring RAMA2D_R4_str, gen(RAMA2D_R4_num) force
}

label var RAMA2D_R4_num "RAMA2D_R4 numérica para homologación"		

replace FORMAL = 1 if inlist(RAMA2D_R4_num, 84, 99)				

replace FORMAL=0 if P6430==8 				

				

/*ASALARIADOS*/				

				

replace FORMAL=1 if P6430 ==2				

replace FORMAL=1 if (P6430 ==1 |  P6430 ==7) & (P3045S1==1)				

replace FORMAL=1 if (P6430 ==1 |  P6430 ==7) & ((P3045S1==2  | P3045S1==9 ) & P3046 == 1)				

replace FORMAL=0 if (P6430 ==1  | P6430 ==7) & ((P3045S1==2 | P3045S1==9 ) & P3046 == 2)				

replace FORMAL=1 if (P6430 ==1 |  P6430 ==7) & ((P3045S1==2 | P3045S1==9 ) & P3046 == 9) & (P3069>= 4)				

replace FORMAL=0 if (P6430 ==1  | P6430 ==7) & ((P3045S1==2 | P3045S1==9 ) & P3046 == 9) & (P3069 <= 3)				

				

/*INDEPENDIENTES*/				

/*SIN NEGOCIO*/				

				

replace FORMAL=1 if (P6430 ==4 | P6430 ==5) & (P6765 ==1 |P6765 ==2 |P6765 ==3 |P6765 ==4 |P6765 ==5 |P6765 ==6 |P6765 ==8) & P3065==1				

replace FORMAL=1 if (P6430 ==4 | P6430 ==5) & (P6765 ==1 |P6765 ==2 |P6765 ==3 |P6765 ==4 |P6765 ==5 |P6765 ==6 |P6765 ==8) & (P3065==2 | P3065==9) & P3066==1				

replace FORMAL=0 if (P6430 ==4 | P6430 ==5) & (P6765 ==1 |P6765 ==2 |P6765 ==3 |P6765 ==4 |P6765 ==5 |P6765 ==6 |P6765 ==8) & (P3065==2 |  P3065==9) & P3066==2				

replace FORMAL=1 if (P6430 ==5) & (P6765 ==1 |P6765 ==2 |P6765 ==3 |P6765 ==4 |P6765 ==5 |P6765 ==6 |P6765 ==8) & (P3065==2 | P3065==9) & P3066==9 & P3069 >= 4				

replace FORMAL=0 if (P6430 ==5) & (P6765 ==1 |P6765 ==2 |P6765 ==3 |P6765 ==4 |P6765 ==5 |P6765 ==6 |P6765 ==8) & (P3065==2  | P3065==9) & P3066==9 & P3069 <= 3				

replace FORMAL=1 if (P6430 ==4) & (P6765 ==1 |P6765 ==2 |P6765 ==3 |P6765 ==4 |P6765 ==5 |P6765 ==6 |P6765 ==8) & (P3065==2  | P3065==9) & P3066==9 & (OFICIO_C8_2D >=00 &  OFICIO_C8_2D <=20)				

replace FORMAL=0 if (P6430 ==4) & (P6765 ==1 |P6765 ==2 |P6765 ==3 |P6765 ==4 |P6765 ==5 |P6765 ==6 |P6765 ==8) & (P3065==2  | P3065==9) & P3066==9 & (OFICIO_C8_2D >=21)				

				

/*CON NEGOCIO*/				

/*CON REGISTRO MERCANTIL*/				

				

replace FORMAL=1 if (P6430 ==4 | P6430 ==5) & (P6765 == 7) & P3067==1 & P3067S1==1 & P3067S2 >= ANIOS				

replace FORMAL=0 if (P6430 ==4 | P6430 ==5) & (P6765 == 7) & P3067==1 & P3067S1==1 & P3067S2 < ANIOS				

replace FORMAL=1 if (P6430 ==4 | P6430 ==5) & (P6765 == 7) & P3067==1 & P3067S1==2 & P6775==1				

replace FORMAL=1 if (P6430 ==4 | P6430 ==5) & (P6765 == 7) & P3067==1 & P3067S1==2 & P6775==3 & (OFICIO_C8_2D >=00 &  OFICIO_C8_2D <=20)				

replace FORMAL=0 if (P6430 ==4 | P6430 ==5) & (P6765 == 7) & P3067==1 & P3067S1==2 & P6775==3 & (OFICIO_C8_2D >=21)				

replace FORMAL=0 if (P6430 ==4 | P6430 ==5) & (P6765 == 7) & P3067==1 & P3067S1==2 & P6775==2				

replace FORMAL=1 if (P6430 ==4 ) & (P6765 == 7) & P3067==1 & P3067S1==2 & P6775==9 & (OFICIO_C8_2D >=00 &  OFICIO_C8_2D <=20)				

replace FORMAL=0 if (P6430 ==4 ) & (P6765 == 7) & P3067==1 & P3067S1==2 & P6775==9 & (OFICIO_C8_2D >=21)				

replace FORMAL=1 if (P6430 ==5 ) & (P6765 == 7) & P3067==1 & P3067S1==2 & P6775==9 & P3069 >= 4 				

replace FORMAL=0 if (P6430 ==5 ) & (P6765 == 7) & P3067==1 & P3067S1==2 & P6775==9 & P3069 <= 3 				

				

				

/*SIN REGISTRO MERCANTIL*/				

				

replace FORMAL=1 if (P6430 ==4 | P6430 ==5) & (P6765 == 7) & P3067==2 & P6775 ==1 & P3068==1				

replace FORMAL=0 if (P6430 ==4 | P6430 ==5) & (P6765 == 7) & P3067==2 & P6775 ==1 & P3068==2				

replace FORMAL=1 if (P6430 ==4 | P6430 ==5) & (P6765 == 7) & P3067==2 & P6775 ==3 & (OFICIO_C8_2D >=00 &  OFICIO_C8_2D <=20)				

replace FORMAL=0 if (P6430 ==4 | P6430 ==5) & (P6765 == 7) & P3067==2 & P6775 ==3 & (OFICIO_C8_2D >=21)				

replace FORMAL=0 if (P6430 ==4 | P6430 ==5) & (P6765 == 7) & P3067==2 & P6775==1 & (P3068==9 |P3068==3)			

/*Nota: Se agrega la opción 3 ya que esta reemplaza al 9 desde 2023*/	

replace FORMAL=0 if (P6430 ==4 | P6430 ==5) & (P6765 == 7) & P3067==2 & P6775==2				

replace FORMAL=1 if (P6430 ==5) & (P6765 == 7) & P3067==2 & P6775==9 & P3069 >= 4				

replace FORMAL=0 if (P6430 ==5) & (P6765 == 7) & P3067==2 & P6775==9 & P3069 <= 3				

replace FORMAL=1 if (P6430 ==4) & (P6765 == 7) & P3067==2 & P6775==9 & (OFICIO_C8_2D >=00 &  OFICIO_C8_2D <=20)				

replace FORMAL=0 if (P6430 ==4) & (P6765 == 7) & P3067==2 & P6775==9 & (OFICIO_C8_2D >=21)				

				

/*SALUD*/				

				

gen SALUD=0				

replace SALUD=1 if (P6430 ==1 | P6430 ==3 | P6430 ==7 ) & (P6100 ==1 |P6100 ==2) & (P6110 ==1 | P6110 ==2 | P6110 ==4)				

replace SALUD=1 if (P6430 ==1 | P6430 ==3 | P6430 ==7 ) & (P6100==9) & (P6450==2)				

replace SALUD=1 if (P6430 ==1 | P6430 ==3 | P6430 ==7 ) & (P6110==9) & (P6450==2)				

				

				

/*PENSIÓN*/				

				

gen PENSION=0				

replace PENSION=1 if (P6430 ==1 | P6430 ==3 | P6430 ==7 ) & P6920==3				

replace PENSION=1 if (P6430 ==1 | P6430 ==3 | P6430 ==7 ) & P6920==1 & (P6930 ==1 |P6930 ==2 |P6930 ==3) & (P6940 ==1 | P6940 ==3)				

				

/*OCUPACIÓN INFORMAL*/				

				

gen EI=0				

replace EI=1 if P6430==2				

replace EI=FORMAL if (P6430 ==4 | P6430 ==5)				

replace EI=1 if (P6430 ==1 | P6430 ==2 | P6430 ==3 | P6430 ==7 ) & SALUD==1 & PENSION==1				

* Crear versión numérica robusta de RAMA2D_R4
capture drop RAMA2D_R4_num
capture drop RAMA2D_R4_str

capture confirm numeric variable RAMA2D_R4

if !_rc {
    gen double RAMA2D_R4_num = RAMA2D_R4
}
else {
    gen str10 RAMA2D_R4_str = strtrim(RAMA2D_R4)
    destring RAMA2D_R4_str, gen(RAMA2D_R4_num) force
}

label var RAMA2D_R4_num "RAMA2D_R4 numérica para homologación"

replace EI = 1 if ///
    inlist(P6430, 1, 2, 3, 4, 5, 7) ///
    & inlist(RAMA2D_R4_num, 84, 99)