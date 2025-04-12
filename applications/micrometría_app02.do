*====================================================================================
* UNALM
* Microeconometría
* Profesor : Luis Chávez
*-----------------------------
*©copyright 2024
*All other rights are reserved
*====================================================================================



*====================================================================================
* Tópico 02: ML
*====================================================================================
* Ejemplo 2: análisis de las visitas al consultorio médico 
* Fuente: Deb, P., Munkin, M. y Trivedi, P. (2006) Bayesian Analysis of the Two-Part
* Model with Endogeneity: Application to Health Care Expenditure. Journal of Applied
* Econometrics, Vol. 21, No. 7, pp. 1081-1099.

clear all
gl data "C:\Users\LENOVO\Documents\UNALM\Pregrado\Micrometría"

use "$data/mledata.dta", clear


* Parte I : Entendiendo el comando ML
* ------------------------------------------------------------------------------

*OLS
regress docvis private chronic female income

* ML para la regresión normal clasica
mlexp (ln(normalden(docvis, {b0} + {b1}*private + {b2}*chronic + {b3}*female + {b4}*income, exp({theta}))))

* También podemos recuperar el estimador de sigma
nlcom (sigma: exp(_b[/theta]))

* Programa para estimar un otras formas funcionales: Función Poisson
program lfpois
  version 11
  args lnf theta1                  // theta1=x'b, lnf=ln(y)
  tempvar lnyfact mu
  local y "$ML_y1"                 // Define y de tal forma que el programa sea más entendible
  generate double `lnyfact' = lnfactorial(`y')
  generate double `mu'      = exp(`theta1')
  quietly replace `lnf'     = -`mu' + `y'*`theta1' - `lnyfact'
end

* Comando ML incluyendo variables y e x
ml model lf lfpois (docvis = private chronic female income), vce(robust)
ml check

* Buscando mejores valores iniciales
ml search

* Computando el estimador
ml maximize 



* Parte II : Test asintóticos
*-------------------------------------------------------------------------------

* Wald Test (W)

* Estimación del modelo tipo Poisson
use "$data/mledata.dta", clear
quietly keep if year02==1
poisson docvis private chronic female income, vce(robust) nolog

* Probando un solo coeficiente igual a 0
test female

* Probando dos hipótesis conjuntamente
test (female) (private + chronic = 1)

* Probando cada hipótesis de manera separada, así como conjuntamente
test (female) (private + chronic = 1), mtest

* Test de Wald de significancia global
test private chronic female income

* Calculando manualmente la prueba general de significancia usando la fórmula para W
quietly poisson docvis private chronic female income, vce(robust)
matrix b = e(b)'   
matrix V = e(V)
matrix R = (1,0,0,0,0 \ 0,1,0,0,0 \ 0,0,1,0,0 \ 0,0,0,1,0 )
matrix r = (0 \ 0 \ 0 \ 0)
matrix W = (R*b-r)'*invsym(R*V*R')*(R*b-r)
scalar Wald = W[1,1]
scalar h = rowsof(R)
display "Wald test statistic: " Wald "  with p-value: " chi2tail(h,Wald)


* Lagrange Multiplier (LM)

* Ejecutando el test de LM tal que b_private=0, b_chronic=0 usando una regresión auxiliar
use "$data/mledata.dta", clear
quietly keep if year02==1
generate one = 1
constraint define 1 private = 0
constraint define 2 chronic = 0
quietly nbreg docvis female income private chronic, constraints(1 2)
predict eqscore ascore, scores
generate s1restb = eqscore*one
generate s2restb = eqscore*female
generate s3restb = eqscore*income
generate s4restb = eqscore*private
generate s5restb = eqscore*chronic
generate salpha = ascore*one
quietly regress one s1restb s2restb s3restb s4restb s5restb salpha, noconstant
scalar lm = e(N)*e(r2)
display "LM = N x uncentered Rsq = " lm " and p = " chi2tail(2,lm)

* Likelihood Ratio (LR)

* LR test usando el comando lrtest
quietly nbreg docvis private chronic female income
estimates store unrestrict
quietly nbreg docvis female income
estimates store restrict
lrtest unrestrict restrict

* Wald test para la misma hipótesis
quietly nbreg docvis private chronic female income
test chronic private
