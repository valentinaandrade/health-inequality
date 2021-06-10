* Paper impuestos libro Atria *
*******************************

* Dos secciones: A. impuestos Chile, B. Chile en comparación internacional

**********************
* A. Impuestos Chile *
**********************

* Data: Merged CEP 1999 -2009 (tiene más variables, ya recodificado en partisan cleavages)

cd "C:\Users\Juank\Dropbox\proyectos y papers compartidos\libro impuestos Atria\data"
cd "C:\Users\usuario\Dropbox\proyectos y papers compartidos\libro impuestos Atria\data"

use "merged 1999 2009"


* Definición  y homogenización de variables dependientes (sentido de 2009 a 1999)

	* Creencia en mas impuestos de los más ricos

		tab te2p17_a // 2009
		tab te8 // 1999
		gen cre_tax= te2p17_a
		replace cre_tax=te8 if year==1999
		recode cre_tax 8 9=. 
		label val cre_tax te2p17_a
		tab cre_tax
		recode cre_tax (1=5) (2=4) (4=2) (5=1) 
		label val cre_tax te2p17_a
		label define revprop_imp 1 "una proporción mucho menor", modify
		label define revprop_imp 2 "una proporción menor", modify
		label define revprop_imp 3 "la misma proporción", modify
		label define revprop_imp 4 "una proporción mayor", modify
		label define revprop_imp 5 "una proporción mucho mayor", modify
		label val cre_tax revprop_imp
		label var cre_tax "Impuestos a los ricos"
		tab cre_tax
		
		bysort year: sum cre_tax
		
		
		*Dummy
		recode cre_tax 1/3=0 4 5=1, gen(prop_ir)  

		* Descriptive
		bysort year: tab cre_tax
			** GRAFICO 1
				catplot cre_tax, over(year) stack perc(year) asyvars ///
					ytitle(" " "Porcentaje") ///
					scheme (s1mono)
					
			*Educación
			tab dat_6 //1999
			tab  ddp06_ni  //2009
			recode ddp06_ni 0=1 99=., gen(edulevel)
			tab eduleve
			recode dat_6 1 2=1 3=2 4=3 5=4 8=5 10=6 8=7 7=8 9=.
			replace eduleve=dat_6 if year==1999		

			lab def educ ///
			1 "educación básica incompleta" ///
			2 "educación básica completa"  ///
			3 "educación media incompleta"  ///
            4 "educación media completa" ///
			5 "educación superior no universitaria" ///
			6 "educación superior no universitaria" ///
			7 "educación universitaria incompleta" ///
			8 "educación universitaria completa" ///
			9 "post grado, magíster, doctorado"
                       
			label val edulevel educ
		
			*recodificacion en menos categorías
			recode edulevel 1 2=1 3 4=2 5 6=3 7 8 9=4, gen(edu4)
			lab def edu42 ///
			1 "educación básica completa"  ///
            2 "educación media completa" ///
			3 "educación superior no universitaria" ///	
			4 "educación universitaria completa" 
			lab val edu4 edu42
			tabulate edu4, gen(edu4)
		
		* descriptives
		** GRAPH 2
		graph hbar cre_tax, over(year) over(edu4) // previous version
			* new version line
	 		
		bysort edu4: egen var99=mean(cre_tax) if year==1999 
		bysort edu4: egen var09=mean(cre_tax) if year==2009
		
		sort edu4 cre_tax
		graph twoway connected var99 var09 edu4, scheme(s1mono) ///
				ytitle("Proporción impuesto ricos" " ") ///
				xtitle(" " "Nivel educacional") ///				
				ylabel(, labsize(small)) 
				
		*Ingreso
			* 1999: dat_23=ingreso, dat_25: Nº personas hogar
			*Chile monthly hhincome in Pesos		// opcion 2: absolute income a partir de categorias
			mvdecode income, mv(0 999996/999999=.) 
			gen hheink=income
			recode hheink 1=45000 if v3==30
			recode hheink 2=105000 if v3==30
			recode hheink 3=135000 if v3==30
			recode hheink 4=165000 if v3==30
			recode hheink 5=195000 if v3==30
			recode hheink 6=225000 if v3==30
			recode hheink 7=265000 if v3==30
			recode hheink 8=340000 if v3==30
			recode hheink 9=495000 if v3==30
			recode hheink 10=800000 if v3==30
			recode hheink 11=1250000 if v3==30
			recode hheink 12=1750000 if v3==30
			recode hheink 13=2500000 if v3==30
			recode hheink 14=3000000 if v3==30

			recode hheink *=. if year==2009
			recode hompop 0 99=.
			gen pchhinc=hheink/hompop

			*2009 
			tab ddp34  // tramos hogar
				recode ddp34 88 99=. ///
				1=17500 ///
				2=45500 ///
				3=67000 ///
				4=89500 ///
				5=117500 ///
				6=158500 ///
				7=201500 ///
				8=257000 ///
				9=324500 ///
				10=403000 ///
				11=724500 ///
				12=1500000 ///
				13=2500000 ///
				14=3500000 ///
				, gen (inghog_2009)

				tab ddp35  // personas por hogar
				tab ddp35, nol
				recode ddp35 99=.
				replace pchhinc=inghog_2009/ddp35 if year==2009

			gen logpchhinc=log(pchhinc) 
		
			* crear variable con ingresos ajustados (ver artículo políticas públicas UC)
				gen pchhinc_a=pchhinc *142.85/100 if year==1999
				replace pchhinc_a=pchhinc if year==2009
				tab pchhinc_a
			
			
			* descrptives
				* Graph 3
				bysort year: table cre_tax, c(mean pchhinc)
				graph hbar pchhinc_a, over(cre_tax) over(year)

				gen inc99=pchhinc_a if year==1999
				gen inc09=pchhinc_a if year==2009
				
					* new version
					graph dot inc99 inc09, over(cre_tax) scheme(s1mono) ///
							ytitle(" " "Ingresos") 
				
				
	* Estatus subjetivo (Solo 2009)
	tab te2p20_a
	recode te2p20_a 88 99=., gen(sstatus)
	
	graph hbar sstatus, over(cre_tax)
	
	graph dot sstatus, over(cre_tax) scheme(s1mono)
	
	* Izquierda / derecha (solo 2009)
	tab mbp20
	recode mbp20 88 99=., gen(izder)
	graph hbar cre_tax, over(izder)
	
	*New version graph 5
	
	bysort izder: egen idtax=mean(cre_tax) 
		
	sort izder  cre_tax
	sort cre_tax izder 
		graph twoway connected idtax izder, scheme(s1mono) ///
				ytitle("Proporción impuesto ricos" " ") ///
				xtitle(" " "Identificación izquierda - derecha") ///				
				ylabel(, labsize(small)) 	
	
	
	* Responsabilidad gobierno reducir diferencias
	gen resp_gov= te2p16_b 
	replace resp_gov=te7b if year==1999
	recode resp_gov 8 9=.
	tab resp_gov			
	recode resp_gov 5=1 4=2 2=4 1=5
	label val resp_gov revper_g
	label var resp_gov "es responsabilidad del gobierno reducir diferencias de ingreso"

	tab resp_gov year, col nofreq
	
	
	graph hbar cre_tax, over(resp_gov) scheme(s2mono)
	
	* Justicia salarial
	table te16
	recode te16 6 8 9=., gen(jusal)
	lab val jusal te16
	label define te16 1 "Mucho menos de lo justo", modify
	label define te16 2 "Menos de lo justo", modify
	label define te16 3 "Aprox. lo justo", modify
	label define te16 4 "Mas de lo justo", modify	
	label define te16 5 "Mucho más de lo justo", modify
	graph hbar cre_tax, over(jusal)	scheme(s2mono) ///
		ytitle ( " " "proporción impuestos a los ricos")
	
	
******************************************
* B. Impuestos comparación internacional *
******************************************


	clear
    use "C:\Users\usuario\Dropbox\Data\ISSP cumulative inequality\data\ZA5400_v3-0-0.dta" // ISUC
    use "C:\Users\Juank\Dropbox\Data\ISSP cumulative inequality\data\ZA5400_v3-0-0.dta" // HP
	
	tolower  V1- WEIGHT

	// v5: country variable, chile=152

* Definición de variables dependientes

	* Impuestos
	tab v36 // redistributive taxes (also in 1999) 
	tab v37 // taxes for people with large incomes 

		* recodes
		gen cre_tax=6-v36 //  1= rich  should pay much smaller share 5=much larger share
		lab def cre 1 "ricos deberían pagar menos" 5"ricos deberían pagar más"
		lab val cre_tax cre	
						
		gen per_tax=6-v37 // 1=much too low  5=much too high
		lab def per 1 "ricos pagan muy poco" 5"ricos pagan mucho"
		lab val per_tax per	
	
	* redistribucion // es responsabilidad del estado reducir las diferencias de ingreso
	table v33
	gen redist=6-v33
	tab redist
	
	*edulevel
	tab degree
	recode degree .n .d=.
	table degree, c(mean per_tax n per_tax) nol
	
		

* Analisis

	* Descriptivos generales chile vs todos 
		tab cre_tax	
		tab cre_tax if v5==152
		table cre_tax if v5==152 [aw=weight], row format(%9.2f)
		tab per_tax	
		tab per if v5==152

	
		table v5 [aw=weight], c(mean cre_tax mean per_tax)  format(%9.2f)

		bysort degree: egen meaneduper=mean(per_tax)
		sort degree
		graph twoway connected meaneduper degree 
		
			
	* Re-label para gráficos
	
			lab list V5
			cap lab drop relab
			lab def relab ///	
			  32 "Arg" /// 
			  36 "Austra" ///
			  40 "Austri" /// 
			  56 "Bel"  ///
			 100 "Bul"  ///
			 152 "Chil" ///
			 156 "Chin"  ///
			 158 "Taiw" ///
			 191 "Croa" ///
			 196 "Chip" ///
			 203 "Rep.Ch"  ///
			 208 "Din" ///
			 233 "Est"  ///
			 246 "Fin"  ///
			 250 "Fra"  ///
			 276 "Alem" ///
			 348 "Hun" ///
			 352 "Islan" ///
			 376 "Isr"  ///
			 380 "Ita" ///
			 392 "Jap" ///
			 410 "Cor" ///
			 428 "Lat"  ///
			 554 "N.Zel"  ///
			 578 "Nor"  ///
			 608 "Fil"  ///
			 616 "Pol" ///
			 620 "Port" ///
			 643 "Rus"  ///
			 703 "Eslova"  ///
			 705 "Eslove"  ///
			 710 "Sudaf"  ///
			 724 "Esp"  ///
			 752 "Sue" ///
			 756 "Sui" ///
			 792 "Tur" ///
			 804 "Ucr"  ///
			 826 "GB" ///
			 840 "USA" ///
			 862 "Ven"
		 	
		lab val v5 relab

	
	
	
* Descriptivos

			preserve
			
			collapse cre_tax per_tax redist degree, by(v5)
			
			graph hbar cre_tax, over(v5, gap (*.2) sort(cre_tax) label(nolabels)) ///			
				ytitle(" " "Proporción impuesto ricos") /// 
				ylabel(, labsize(small)) ///
				ysc(r(2(1)5)) ///
				blabel(group, position(base) color(white) size(vsmall)) ///
				bargap (50) ///
				ysize(6)
				
			graph hbar per_tax if per_tax>=1.9 & per_tax<=3.7, over(v5, gap (*.2) sort(per_tax) label(nolabels)) ///			
				ytitle(" " "Percepción impuesto a los ricos") /// 
				ylabel(, labsize(small)) ///
				ysc(r(1.8 4)) ///
				blabel(group, position(base) color(white) size(vsmall)) ///
				bargap (50) ///
				ysize(6)			

			sort degree
			graph twoway connected per_tax degree
			
			table degree, c(mean per_tax)
				
		* Gráfico c gini p
			gen gini=v5
			recode gini /// gini 2009 de acuerdo a Solt excepto cuando se indica
			  32 /* "Arg"*/ 	=42.28	/// 
			  36 /*"Austra"*/ 	=33.48 	/// 
			  40 /*Austri*/ 	=27.11  /// 
			  56 /*Bel*/ 		=24.72	///
			 100 /*Bul*/ 		=  35.17	///
			 152 /*Chil*/ 		= 49.70	///
			 156 /*Chin*/ 		= 39.6 /// 2005
			 158 /*Taiw*/ 		= 30.5 /// 2005
			 191 /*Croa*/ 		= 27.57	///
			 196 /*Chip*/ 		=  29.33	///
			 203 /*Rep.Ch*/ 	= 24.85	///
			 208 /*Din*/ 		=  26.47	///
			 233 /*Est*/ 		=  31.09	///
			 246 /*Fin*/ 		= 25.35	///
			 250 /*Fra*/ 		=  28.56	///
			 276 /*Alem*/		= 30.23 ///
			 348 /*Hun*/ 		=  26.10	///
			 352 /*Islan*/		=28.29 ///
			 376 /*Isr*/ 		= 37 	/// 2005
			 380 /*Ital*/		= 32.99 ///
			 392 /*Jap*/ 		=  30.71	///
			 410 /*Cor*/ 		=  31.42	///
			 428 /*Lat*/ 		=  36.49	///
			 554 /*N.Zel*/ 		=  32.52  	/// 2007
			 578 /*Nor*/ 		= 22.28	///
			 608 /*Fil*/ 		=  41.27	///
			 616 /*Pol*/ 		=  29.36	///
			 620 /*Port*/ 		=  34.03	///
			 643 /*Rus*/ 		=  45.24	///
			 703 /*Eslova*/ 	=  23.44	///
			 705 /*Eslove*/ 	=  23.42	///
			 710 /*Sudaf*/ 		=  63.5 ///  2005
			 724 /*Esp*/ 		=  32.08	///
			 752 /*Sue*/ 		=  22.48	///
			 756 /*Sui*/ 		=  30.2	///
			 792 /*tur*/		=  37.47 ///
			 804 /*Ucr*/ 		=  29.49 /// 2007
			 826 /*GB*/ 		= 35.74	///
			 840 /*USA*/		= 35.74 ///
			 862 /*Ven*/		= 38.91
								 
		scatter cre_tax gini, mlabel(v5) || lfit cre_tax gini, ///
			xtitle("Indice de Gini") ///
			ytitle("ricos deberían pagar más impuestos" " ") ///
			legend(off)
		
		scatter per_tax gini, mlabel(v5) || lfit per_tax gini, ///
			xtitle("Indice de Gini") ///
			ytitle("ricos pagan muchos impuesots" " ") ///
			legend(off)

		pwcorr per_tax gini
		pwcorr per_tax gini if v5!= 710 & v5!= 152
		
	* Aggregated beliefs
		scatter cre_tax redist, mlabel(v5) || lfit cre_tax redist, ///
			xtitle(" " "Redistribución estatal") ///
			ytitle("ricos deberían pagar más impuestos" " ") ///
			legend(off)
	
	
	* Tax revenue as percentage of GDP
			gen taxrev =v5  // sourre: Heritage foundation (from Wikipedia, please document)
			recode taxrev /// gini 2009 de acuerdo a Solt excepto cuando se indica
			  32 /* "Arg"*/ 	=37.2	/// 
			  36 /*"Austra"*/ 	=30.8 	/// 
			  40 /*Austri*/ 	=43.4  /// 
			  56 /*Bel*/ 		=46.8	///
			 100 /*Bul*/ 		=  34.4	///
			 152 /*Chil*/ 		=21.0	///
			 156 /*Chin*/ 		= 17.0 /// 
			 158 /*Taiw*/ 		= . /// 
			 191 /*Croa*/ 		= 26.6	///
			 196 /*Chip*/ 		=  39.2	///
			 203 /*Rep.Ch*/ 	= 36.3	///
			 208 /*Din*/ 		=  49.0	///
			 233 /*Est*/ 		=  32.3	///
			 246 /*Fin*/ 		= 43.6	///
			 250 /*Fra*/ 		=  44.6	///
			 276 /*Alem*/		= 40.6 ///
			 348 /*Hun*/ 		=  39.1	///
			 352 /*Islan*/		= 40.4 ///
			 376 /*Isr*/ 		= 36.8	/// 
			 380 /*Ital*/		= 42.6 ///
			 392 /*Jap*/ 		=  28.3	///
			 410 /*Cor*/ 		=  26.8	///
			 428 /*Lat*/ 		=  30.4	///
			 554 /*N.Zel*/ 		=  34.5  	/// 
			 578 /*Nor*/ 		= 43.6	///
			 608 /*Fil*/ 		=  14.4	///
			 616 /*Pol*/ 		=  33.8	///
			 620 /*Port*/ 		=  37.0	///
			 643 /*Rus*/ 		=  36.9	///
			 703 /*Eslova*/ 	=  29.5	///
			 705 /*Eslove*/ 	=  39.3	///
			 710 /*Sudaf*/ 		=  26.9 ///  
			 724 /*Esp*/ 		=  37.3	///
			 752 /*Sue*/ 		=  45.8	///
			 756 /*Sui*/ 		=  29.4	///
			 792 /*tur*/		=  32.5 ///
			 804 /*Ucr*/ 		=  28.1 /// 
			 826 /*GB*/ 		= 39.0	///
			 840 /*USA*/		= 26.9 ///
			 862 /*Ven*/		= 25.0
	
		scatter cre_tax taxrev, mlabel(v5) || lfit cre_tax taxrev, ///
			xtitle("Tax revenue as % GDP") ///
			ytitle("ricos deberían pagar más impuesots" " ") ///
			legend(off)
			// Nichts
		
		scatter per_tax taxrev, mlabel(v5) || lfit per_tax taxrev, ///
			xtitle("Tax revenue as % GDP") ///
			ytitle("ricos deberían pagar más impuesots" " ") ///
			legend(off)
			// auch Nichts	
		
		
	* Tablas correlaciones comparadas chile/resto del mundo

		* Variables

			* Escala izquierda/derecha (derived from party affiliation
				tab party_lr
				recode party_lr 6 7 =. // se pierden el 25% de los casos, and no effects
				
			* Evaluación de diferencias de ingreso (percepción general de desigualdad) (per_g)
				tab v32
				gen per_des=6-v32
				label define revper_g 1 "muy en desacuerdo"2 "en desacuerdo" ///
					3 "ni de acuerdo ni en desacuerdo" 4 "de acuerdo" 5 "muy de acuerdo"
				lab val per_des revper_g	
				label var per_des "diferencias de ingreso muy grandes"

			* Responsabilidad gobierno reducir diferencias
				tab v33
				gen resp_gov=6-v33
				label val resp_gov revper_g
				label var resp_gov "es responsabilidad del gobierno reducir diferencias de ingreso"
				
			* Salud 
				tab v38
				gen salud= 6-v38 // higher values: + correct/just that richer have better health
		
			* Educación
				tab v39
				gen educa=6-v39
			
		* Tablas	
		
			sum per_des resp_gov salud educa
			
			pwcorr cre_tax per_tax per_des resp_gov salud educa, obs 
			pwcorr cre_tax per_tax per_des resp_gov salud educa if v5==152, obs 			
			
		
			* polychoric cre_tax per_tax party_lr per_des resp_gov salud educa, pw // muy lento
			* polychoric cre_tax per_tax if v5==152

			stata2mplus cre_tax per_tax per_des resp_gov salud educa using impuestos // run mplus no funciona?
			
			
			party_lr per_des resp_gov salud educa, ///
			categorical(cre_tax per_tax party_lr per_des resp_gov salud educa) output(sampstat)
			
			
			bysort v5: polychoric cre_tax per_tax  // chile entre los paises donde la asociación es menor

	
		* Gráfico correlaciones (nicht wirklich eindeutig)			
		sort cre_tax per_tax
		twoway ///
		(lfit cre_tax per_tax if v5==32 ) ///
		(lfit cre_tax per_tax if v5== 36) ///
		(lfit cre_tax per_tax if v5==40 ) ///
		(lfit cre_tax per_tax if v5==56 ) ///
		(lfit cre_tax per_tax if v5==100 ) ///
		(lfit cre_tax per_tax if v5==152, lwidth(thick)) ///
		(lfit cre_tax per_tax if v5==156 ) ///
		(lfit cre_tax per_tax if v5==158 ) ///
		(lfit cre_tax per_tax if v5==191 ) ///
		(lfit cre_tax per_tax if v5==196 ) ///
		(lfit cre_tax per_tax if v5==203 ) ///
		(lfit cre_tax per_tax if v5==208 ) ///
		(lfit cre_tax per_tax if v5==233 ) ///
		(lfit cre_tax per_tax if v5==246 ) ///
		(lfit cre_tax per_tax if v5==250 ) ///
		(lfit cre_tax per_tax if v5==348 ) ///
		(lfit cre_tax per_tax if v5==376 ) ///
		(lfit cre_tax per_tax if v5==392 ) ///
		(lfit cre_tax per_tax if v5==410 ) ///
		(lfit cre_tax per_tax if v5==428 ) ///
		(lfit cre_tax per_tax if v5==554 ) ///
		(lfit cre_tax per_tax if v5==578 ) ///
		(lfit cre_tax per_tax if v5==608 ) ///
		(lfit cre_tax per_tax if v5==616 ) ///
		(lfit cre_tax per_tax if v5==620 ) ///
		(lfit cre_tax per_tax if v5==703 ) ///
		(lfit cre_tax per_tax if v5==705 ) ///
		(lfit cre_tax per_tax if v5==710 ) ///
		(lfit cre_tax per_tax if v5==724 ) ///
		(lfit cre_tax per_tax if v5==752 ) ///
		(lfit cre_tax per_tax if v5==756 ) ///
		(lfit cre_tax per_tax if v5==804 ) ///
		(lfit cre_tax per_tax if v5==826 ), ///
		legend(off)

		/* La correlación entre impuesto percibido y demanda por redistribuciòn es negativa en términos
		agregados */
		
		
	* #################################### Esto de abajo es deo otra sintaxis de comp internacional		
	
		
		* educación
			tab degree
	
		* Ingreso: 
			/* problema: la variable ingreso es categórica para algunos paises ... de todas maneras el value 
			es el punto medio de la escala.
			Se puede intentar una quintilización, pero con precauciones */

			recode ar_inc- za_inc (.a .b .c .d=.) 

			*tab1 *_inc  // ver casos extraños: hungría 348, si(slovenia) 705, 
			tab hu_inc, nol
			recode hu_inc 999990=.
			recode si_inc 9999990=.  9996=. 
			
			* Percapita household income
			tab hompop
			
			foreach var of varlist *_inc {
			gen `var'h= `var'/hompop
			}			

			* Quintiles

			foreach var of varlist *_inch {
			xtile qu`var'= `var', nq(5)
			}

			*comprobar para chile
			xtile quchile= cl_inch, nq(5)
			tab quchile
			tab qucl_in // ok
			drop quchile

			* crear unica variable
			egen quintil=rowtotal( quar_inch- quza_inch)
			tab quintil
			recode quintil 0=.	
				
			
	
	
*### Francisco Olivos	
		
*Merge con variables macro

use "C:\Users\Francisco Olivos\Dropbox\ISSP cumulative inequality\data\issp09_formerge.dta", clear

merge m:1 v5 using "Tax_revenue_.dta"

		scatter cre_tax tax_revenue, mlabel(v5) || lfit cre_tax tax_revenue, ///
			xtitle(" " "Tax revenue") ///
			ytitle("ricos deberían pagar más impuesots" " ") ///
			legend(off)

		scatter per_tax tax_revenue, mlabel(v5) || lfit per_tax tax_revenue, ///
			xtitle("Tax revenue") ///
			ytitle("ricos pagan muchos impuesots" " ") ///
			legend(off)


*Impuesto como % de GDP. Datos del banco mundial para 2009. 

rename _merge _merge1
merge m:1 v5 using "Tax-benefit.dta"
tab 

*Datos de Goñi et al (2011) publicado en World Development. Autores del Banco Mundial y del Interamericano de Desarrollo. Atria nos mandó la versión
//Workin Paper. Son los Gini calculados para sueldos de mercado, después del pago de impuestos y después de transferencias. Ellos presentan utilizan cifras de
// distintos años y fuentes (detalle en el apéndice de su artículo). 

destring Disposableincome, replace

destring  Marketincome, replace

generate taxbenefit = (Marketincome) - (Disposableincome)

tab taxbenefit


save "C:\Users\Francisco Olivos\Documents\FONDAP\issp09_macrotax.dta", replace file C:\Users\Francisco Olivos\Documents\FONDAP\issp09_macrotax.dta saved


