*====================================================================================
* UNALM
* Microeconometría
* Profesor : Luis Chávez
*-----------------------------
*©copyright 2024
*All other rights are reserved
*====================================================================================



*====================================================================================
* Tópico 01: OLS
*====================================================================================
* Ejemplo 1: gasto en salud de personas elegibles para Programa Medicare (USA)
* Fuente : Cameron, A. y Trivedi, P. (2005). Microeconometrics Using Stata. Stata Press.
clear all
net from http://www.stata-press.com/data/mus
net describe mus
net install mus
net get mus


* Parte I : Estadísticos preliminares
*-----------------------------------------------------------------------------------
clear all
gl metria "C:\Users\LENOVO\Documents\UNALM\Pregrado\Micrometría"

use mus03data.dta, clear
describe totexp ltotexp posexp suppins phylim actlim totchr age female income
summarize totexp ltotexp posexp suppins phylim actlim totchr age female income
tabulate income if income <= 0
summarize totexp , detail
table female totchr

*One-way table
tabulate female 

*Two-way table
tabulate female suppins, row col chi2

*Three-way table of frequencies
table female totchr suppins

*Two-way table of summary statistics
table female suppins, stat(mean income)

*Summary statistics with tabstat
tabstat totexp ltotexp, stat (count mean p50 sd skew kurt) col (stat)

*Test de significancia
ttest totexp, by(suppins)

*Kernel density plots with adjustment for highly skewed data
kdensity totexp if posexp==1, generate (kx1 kd1) n(500) 
graph twoway (line kd1 kx1) if kx1 < 40000, name(levels)
kdensity ltotexp if posexp==1, generate (kx2 kd2) n(500) 
graph twoway (line kd2 kx2) if kx2 < ln(40000), name(logs)
graph combine levels logs, iscale(1.0)

*Pairwise correlations
correlate ltotexp suppins phylim actlim totchr age female income



* Parte II : Análisis de regresión
*-----------------------------------------------------------------------------------

*OLS
regress ltotexp suppins phylim actlim totchr age female income

*Display stored results and list available postestimation commands
ereturn list
help regress postestimation

*OLS sin constante
regress ltotexp suppins phylim actlim totchr age female income, noconst

*OLS con errores robustos
regress ltotexp suppins phylim actlim totchr age female income, vce(robust)

* Heterocedasticidad: test Breusch-Pagan
estat hettest

* Test de Wald de igualdad de coeficientes
quietly regress ltotexp suppins phylim actlim totchr age female income, vce(robust)
test phylim = actlim

* Prueba conjunta de significancia estadística de varias variables
test phylim actlim totchr

* Almacenamiento y tabulación de resultados de regresiones múltiples
quietly regress ltotexp suppins phylim actlim totchr age female income, vce(robust)
estimates store mod1
quietly regress ltotexp suppins phylim actlim totchr age female educyr, vce(robust)
estimates store mod2

*net install st0085_2, from(http://www.stata-journal.com/software/sj14-2)
estimates table mod1 mod2, b(%9.4f) se stats(N r2 F ll) keep(suppins income educyr)

* Adición de algun estadístico adicional
estimates drop mod1 mod2
quietly regress ltotexp suppins phylim actlim totchr age female income, vce(robust)
estadd scalar pvalue = Ftail(e(df_r),e(df_m),e(F))
estimates store mod1
quietly regress ltotexp suppins phylim actlim totchr age female educyr, vce(robust)
estadd scalar pvalue = Ftail(e(df_r),e(df_m),e(F))
estimates store mod2
esttab mod1 mod2, b(%10.4f) se scalars(F pvalue) mtitles keep(suppins)

*Plot de residuos y valores ajustados
quietly regress ltotexp suppins phylim actlim totchr age female income, vce(robust)
rvfplot

* Details on the outlier residuals
predict uhat, residual
predict yhat, xb
list totexp ltotexp yhat uhat if uhat < -5, clean



* Parte III : Regresión via álgebra matricial
*-----------------------------------------------------------------------------------

* OLS con errores estándar robustos blancos usando la función Mata
use mus03data.dta, clear
keep if totexp > 0   // Analysis for positive medical expenditures only 
generate cons = 1
local y ltotexp
local xlist suppins phylim actlim totchr age female income cons
mata
  // Create y vector and X matrix from Stata dataset
  st_view(y=., ., "`y'")             // y is nx1
  st_view(X=., ., tokens("`xlist'")) // X is nxk
  XXinv = cholinv(cross(X,X))         // XXinv is inverse of X'X 
  b = XXinv*cross(X,y)               // b = [(X'X)^-1]*X'y
  b
  e = y - X*b
  n = rows(X)
  k = cols(X)
  s2 = (e'e)/(n-k)
  vdef = s2*XXinv               // default VCE not used here
  vwhite = XXinv*((e:*X)'(e:*X)*n/(n-k))*XXinv  // robust VCE
  st_matrix("b",b')             // pass results from Mata to Stata
  st_matrix("V",vwhite)         // pass results from Mata to Stata
end
*Use la pantalla de restauración de Stata para presentar resultados de forma amigable
matrix colnames b = `xlist'
matrix colnames V = `xlist'
matrix rownames V = `xlist'
ereturn post b V
ereturn display

* Revisar con los resultados iniciales
regress ltotexp suppins phylim actlim totchr age female income, vce(robust) noheader