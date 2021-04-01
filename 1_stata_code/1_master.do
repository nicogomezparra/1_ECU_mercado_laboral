// Autor: NICOLAS GOMEZ PARRA
// Analisis del mercado laboral ecuatoriano en el periodo 2012-13 con datos
// trimestrales
// Version: 1.0
// Fecha: 31 de Marzo, 2011
// Repositorio online en: https://github.com/nicogomezparra/1_ECU_mercado_laboral

// Github settings

// Para acceder a la base de datos, es necesario incluir carpeta/symbolic link
// con el nombre datastore. El acceso a los datos es restringido. Datastore en este
// caso es un symbolink link de Github al repositorio en Dropbox.
	* i.e. ln-s /Users/UserName/Dropbox/1_ECU_mercado_laboral_datastore datastore

	* local
		* cd "C:\Users\wb530118\Dropbox\1_ECU_mercado_laboral\1_stata_code" // location of the code for reference
		 use "C:\Users\wb530118\Dropbox\1_ECU_mercado_laboral_datastore\Ecuador_panel_test.dta",clear

	* use "../datastore/Ecuador_panel_test.dta",clear // conexion al repositorio de datos (privado)

// Analisis inicial de la base de datos

// Notas:
	* Revisar detalles del factor de expansion -estratificacion y diseno muestral
	* pueden no corresponder al usado en este codigo. Por ahora se asume que los
	* pesos no deben ser ajustados por el diseno de la encuesta.
	
	* Algunas categorizaciones laborales son discutibles y pueden cambiar dependiendo del contexto.
	* El debate se puede abrir en terminos de definiciones de trabajadores independiente, formales/informales,
	* y trabajadores no remunerados. Los calculos presentados en este analisis son explorativos
	* del mercado laboral ecuatoriano, pero es importante un debate en la conceptualizacion de algunos
	* supuestos.

isid id_pers trim ano 			// verificar key variables
destring *, replace				// cambiar formato de las var numericas
* misstable sum * , all			// obs missing definidas por subpoblaciones - ningun supuesto por ahora
	* tab pea p20,m				// 		ejemplo
	mvdecode p47b,mv(99)			// consistente con misstable y el diccionario, hay encoded missings
	mvdecode p33 p51a p51b p51c,mv(999)
	mvdecode p63 p64b p65 p66 p67 p69 p72b,mv(999999) // self-note: esto puede definir la muestra para los indicadores
	compress
	tempfile dta_indic			// preserve
	save `dta_indic'
	
// Definir poblaciones y variables del analisis
local s1 sexo!=. & inrange(edad,15,64) & // total
local s2 sexo==1 & inrange(edad,15,64) & // hombres
local s3 sexo==2 & inrange(edad,15,64) & // mujeres
local s4 sexo!=. & inrange(edad,15,24) & // jovenes
local s5 sexo!=. & inrange(edad,25,64) & // adultos

forvalues i=1(1)5 { 

keep if `s`i'' ano==2013 & trim==4 	 // Definir subpoblacion

tab pea condact,m 				// Poblacion economicamente activa (PEA)
replace pea=0 if condact==7 	// Tasa de participación : porcentaje de PEA
gen po=inrange(condact,0,3) 	// Poblacion ocupada (PO):
tab po condact,m 				// Poblacion ocupada en %
gen pd=inrange(condact,5,7) 	// Poblacion desocupada (PD)
replace pd=. if pea==0 			// Tasa de desempleo
gen pd_ld=(pd*(inrange(p33,52,10000000))) // Proporción de desempleados de larga duracion 
gen po_form1=socsec if po!=1 	// Tasa de formalidad1: Cotizantes o afiliados / ocupados
gen po_form2=socsec if inrange(p42,1,3) 		//Tasa de formalidad2: Cotizantes o afiliados / asalariados
gen po_auto=(p42==6) if po==1 					// Tasa de autoempleo (REVISAR)
gen po_sub=inrange(condact,2,3) if pea==1 		// Tasa de subempleo
gen ing_princ=p66 if inrange(p42,1,3) & po==1 	// Ingreso laboral monetario en la actividad principal (promedio)
	replace ing_princ=p63 if inrange(p42,5,6) & po==1
gen ing_total=ing_princ	if po==1 				// Ingreso laboral monetario total (promedio) - total es missing si alguno de los dos valores es missing
	replace ing_total=ing_total+p69 if inrange(p51b,1,10000000) & po==1
gen ing_salmm=(ing_total<salmm_ci) if ing_total!=. // Porcentaje de ocupados con ingresos laborales por debajo del salario minimo 
												// nota: en este calculo se entiende por ingreso laboral el ingreso total, no solo el del la actividad principal
gen educ_ano=0 if nivinst==0					//años de educación promedio de la PEA - tenemos que saber cuantos anos de educacion tene cada nivel educativo
	replace educ_ano=anoinst if nivinst==1
	replace educ_ano=4 + anoinst if nivinst==2			// se define como anios totales de un nivel menor de educacion mas los anios definidos en nivinst.
	replace educ_ano=4 + anoinst if nivinst==4			// se utilizo el siguiente documento como referencia 
	replace educ_ano=4 + anoinst if nivinst==5
	replace educ_ano=10 + anoinst if nivinst==6
	replace educ_ano=13 + anoinst if nivinst==7
	replace educ_ano=16 + anoinst if nivinst==8
	replace educ_ano=16 + anoinst if nivinst==9
	replace educ_ano=21 + anoinst if nivinst==10
gen emp_5less=inrange(p47b,1,5) if inrange(p42,1,3) & p47b!=. //Proporcion de asalariados en empresas de 5 o menos trabajadores

collapse 	(sum) pea (mean) rr_pea=pea (sum) po (mean) rr_po=po (sum) pd 		///
			(mean) rr_pd=pd pd_ld po_form1 po_form2 po_auto po_sub ing_princ 	///
			ing_total ing_salmm educ_ano emp_5less [pw=fexp],by(ano)
drop ano
mkmat *,matrix(table`i')		// preparar resultados para exportar
	
	use `dta_indic',clear  // restore
}

matrix define table = [table1 \ table2 \ table3 \ table4 \ table5]

putexcel set "Tabla de Indicadores.xlsx",modify sheet("1.Descriptivos")
putexcel B5 = matrix(table') 

	* Parte 2 de la tabla

// Definir poblaciones y variables del analisis
local s1 sexo!=. & inrange(edad,65,150) // total
local s2 sexo==1 & inrange(edad,65,150) // hombres
local s3 sexo==2 & inrange(edad,65,150) // mujeres

forvalues i=1(1)3 {

keep if `s`i'' & inrange(edad,65,150) & ano==2013 & trim==4 	 // Definir subpoblacion

//nota: la tabla tiene que ser corregida ya que este indicador no es consistente
//con la subpoblacion definida en las columnas - i.e. edad. Los valores seran reportados
//solamente para las tres primeras columnas.

gen p65_pens=(p72a==1) if inrange(edad,65,150) & p72a!=. 	//Porcentaje de personas de 65 y más años que recibe pensión - balanced data para adultos mayores de 65
sum p72b salmm_ci 											//Pensión promedio / salario mínimo: se calcula despues del collapse

collapse (mean) p65_pens p72b salmm_ci [pw=fexp],by(ano) 
replace p72b=p72b/salmm_ci
drop ano salmm_ci
mkmat *,matrix(table`i')		// preparar resultados para exportar

	use `dta_indic',clear  		// restore
}

matrix define table = [table1 \ table2 \ table3 ]

putexcel B21 = matrix(table') 
putexcel clear

	* Tarea 2: 
	
gen q=yq(ano,trim) // date var in stata format
xtset id_pers q,quarterly

gen trans_cat=1 if condact==7 						// Inactivos
replace trans_cat=2 if inrange(condact,5,6) 		// Desempleados
replace trans_cat=3 if inrange(p42,1,3) & socsec==1 & trans_cat==.
replace trans_cat=4 if inrange(p42,1,3) & socsec==0 & trans_cat==.
replace trans_cat=5 if inrange(p42,4,6) & trans_cat==. // Por el momento se van a asignar los jornaleros, patronos y cuenta propia como trabajadores independientes - esto es discutible (referencia Bertranou 2009 - Trabajadores independientes y proteccion social en America Latina
replace trans_cat=6 if inrange(p42,7,10) & trans_cat==. // Por el momento se van a asignar los trabajadores de servicio domestico como no remunerados- esto es discutible (referencia Bertranou 2009 - Trabajadores independientes y proteccion social en America Latina

gen trans_cat_YOY=l4.trans_cat
gen trans_cat_QOQ=l.trans_cat // por definicion, solamente calcularemos la matriz con personas que hayan estado mas de una vez

rcolp2mat trans_cat_QOQ trans_cat [aw=fexp],matrix(tab1) row total // analytical weights can be used here instead of pweights -> check using svy linearized : tabulate trans_cat_YOY trans_cat,row after setting the data as survey
rcolp2mat trans_cat_YOY trans_cat [aw=fexp],matrix(tab2) row total 
rcolp2mat trans_cat_YOY trans_cat [aw=fexp] if sexo==2,matrix(tab3) row total 

putexcel set "Tabla de Indicadores.xlsx",modify sheet("2.Calculos_transiciones")
putexcel B5 = matrix(tab1)
putexcel B15 = matrix(tab2)
putexcel B25 = matrix(tab3)

egen counter=count(id_pers),by(id_pers) // Restringir poblacion - lo que mas tiene sentido es tener en cuenta solo poblacion en edad de trabajar en todos los anios que aparecen
gen pet=inrange(edad,15,64)
egen pet_panel=min(pet),by(id_pers)

keep if inrange(counter,3,4) & pet_panel==1
gen pd=inrange(condact,5,7) & condact!=.

	collapse (max) pd,by(sexo id_pers) // max define las personas que estuvieron al menos una vez en desempleo

local s1 sexo!=. 
local s2 sexo==1
local s3 sexo==2

forvalues i=1(1)3 {
preserve
	keep if `s`i''
	gen full=1
	collapse (mean) pd,by(full)
	drop full
	mkmat *,matrix(table`i')		// preparar resultados para exportar
restore
}

// Unweighted value - i.e. debido a que los pesos son para cada periodo de encuesta, no pueden ser utilizados en este calculo 
// sin hacer algun supuesto de los pesos mostrales. Por ahora se calcula el porcentaje muestral sin ponderacion.

matrix define table = [table1 \ table2 \ table3 ]

putexcel B34 = matrix(table) 
putexcel clear
