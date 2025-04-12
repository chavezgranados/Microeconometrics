*====================================================================================
* UNALM
* Microeconometría
* Profesor : Luis Chávez
*-----------------------------
*©copyright 2024
*All other rights are reserved
*====================================================================================


*====================================================================================
* Tópico 08: Modelos de panel
*==================================================================================== 

*PARTE I: paneles estáticos
*-----------------------------------------------------------------------------

*Preámbulo (2004 - 2021)
clear all
* Insertar su global ................ y cargar su data

* Esquema gráfico
graph set window fontface       "Times New Roman"
graph set window fontfacemono   "Times New Roman"
graph set window fontfacesans   "Times New Roman"
graph set window fontfaceserif  "Times New Roman"
*ssc install schemepack, replace
set scheme white_jet


* Concatenando
egen var=concat(Ano trimestre)
encode var, gen(vartime)
gen t=vartime+175 //para llevar de 1960 a 2004
drop vartime var
xtset dpto t, quarterly //seteamos

*Transformación de variables
gen cl=L.c
gen cf=F.c
gen pl=L.p
gen pf=F.p
label var cl "Consumo rezagado"
label var cf "Consumo adelantado"
label var pl "Precio rezagado"
label var pf "Precio adelantado"

gen g1=ln(g) //gasto en logaritmo 

forvalues i=2004/2021 { //generamos las dummys de años
	gen y`i'=0
	replace y`i'=1 if Ano==`i'
}

order dpto t Ano trimestre c cl cf


*Descriptivas
xtdescribe, patterns(9) 
xtsum
xttab sx

xtline p, overlay //Gráfico por unidad individual del precio

*Gráficas
xtline c, plotregion(lcolor(none)) mlabcolor() ytitle(c) xtitle("") subtitle(, bcolor(white)) ytitle(consumo (litros)) ylabel(, nogrid) xlabel(, nogrid) note(" ") legend(off)

* Estimación de modelos
*Modelo de efectos fijos (y_it=alpha_i+x'_it*beta+u_it) 
xtreg c p g1 ar po, fe
xtreg c p g ar po, fe
xtreg c p y ar po, fe
xtreg c p g1 po sx ar sc ed ec, fe
xtreg c p g1 po sx ar sc ed ec, fe vce(robust)

*Modelo pooled (y_it=alpha+x'_it*beta+u_it)
regress c p g1 po sx ar sc ed ec  
regress c p g1 po sx ar sc ed ec, vce(robust)

*Mata: (y_it=alpha+x'_it*beta_i+u_it)




*PARTE II: paneles dinámicos
*-----------------------------------------------------------------------------
*validación cruzada k-fold y regresión lasso
cvlasso c cl cf p ar sc sx ed ec po td, seed(123)
cvlasso, lopt


xtivreg c cl cf p ar sc sx ed ec po td (cl cf= pl pf g1), fd 	//fdtsls_m1
estimates store fdtsls_m1

xtivreg c cl cf p ar sc po td (cl cf= pl pf g), fd 				//fdtsls_m2
estimates store fdtsls_m2

xtivreg c cl cf p ar sc po (cl cf= pl pf g), fd vce(robust) 	//fdtsls_m3
estimates store fdtsls_m3

xtivreg c cl cf p L1.g sc po (cl cf= pl pf g), fd vce(robust) 	//fdtsls_m4
estimates store fdtsls_m4

*ssc install estout
esttab fdtsls_m1 fdtsls_m2 fdtsls_m3 fdtsls_m4, not compress mtitles(m1 m2 m3 m4) nonumbers


*Arellano Bond 1-------------
xtabond c p cf, lags(1) pre(po ar g1) maxldep(10) maxlags(2) vce(rob) //gmm_m2
estimates store gmm_m1


*Arellano Bond-2-------------- (los mejores modelos)
*ssc install xtabond2, replace
xtabond2 c cl cf p sc ar po g1, iv(pl pf y po ar sc, eq(level))  gmm(l.p l.g1) robust
estimates store gmm_m2

xtabond2 c cl cf p sc ar po g1, iv(pl pf, eq(level))  gmm(l.g1 l.po) robust
estimates store gmm_m3  //mejor modelo

xtabond2 c cl cf p sc ar po g1, iv(pl pf, eq(level)) gmm(l.p l.g1 l.po, eq(both)) robust
estimates store gmm_m4

xtabond2 c cl cf p sc ar po g1, iv(pl pf, eq(both))  gmm(l.p l.g1 l.po) robust
estimates store gmm_m5

esttab gmm_m1 gmm_m2 gmm_m3 gmm_m4 gmm_m5, not compress mtitles(m1 m2 m3 m4) nonumbers

*****con dummys de años (no se considero porque los resultados son analogos)
xtabond2 c cl cf p sc ar g1 y*, iv(pl pf, eq(level))  gmm(l.p l.y) robust
// regresión con dummys de todos los años y2004, y2005,...


* Predicción del consumo (panel)
predict xb
xtline c xb, recast(line) lcolor(teal) /// Predicción del consumo


*Multicolinealidad------------
collin cl cf p ar sc po g1


*Autocorrelación--------------
*   Viene en cada regresión
predict residuos, residuals
rvfplot residuos cl
margins



*Regresiones dinámicas para miopes
*-----------------------------------------------------------------------------

*Arellano Bond-2-------------- (los mejores modelos)
xtabond2 c cl p sc ar po g1, iv(pl pf y po ar sc, eq(level))  gmm(l.p l.g1) robust
estimates store gmm_m1

xtabond2 c cl p sc ar po g1, iv(pl pf, eq(level))  gmm(l.g1 l.po) robust
estimates store gmm_m2  //mejor modelo

xtabond2 c cl p sc ar po g1, iv(pl pf, eq(level)) gmm(l.p l.g1 l.po, eq(both)) robust
estimates store gmm_m3

xtabond2 c cl p sc ar po g1, iv(pl pf, eq(both))  gmm(l.p l.g1 l.po) robust
estimates store gmm_m4