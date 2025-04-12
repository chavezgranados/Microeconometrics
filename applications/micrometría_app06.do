*====================================================================================
* UNALM
* Microeconometría
* Profesor : Luis Chávez
*-----------------------------
*©copyright 2024
*All other rights are reserved
*====================================================================================


*====================================================================================
* Tópico 06: Modelos politómicos
*====================================================================================
*Ejemplo 1: entrevistas telefónicas aleatorias en el sur de California para identificar a los pescadores recreativos, 1989.
* Fuente: J. A. Herriges and C. L. Kling, "Nonlinear Income Effects in Random Utility Models"

*Ejemplo 2: Rand Health Insurance Experiment data 
* Fuente: P. Deb and P.K. Trivedi (2002), "The Structure of Demand for Medical Care: Latent Class Two-Part Models.


clear all

cd "C:\Users\LENOVO\Documents\UNALM\Pregrado\Micrometría\cameron (2009)"
* Abrir data: tipo de pescadores
use mus15data.dta, clear
describe
summarize, separator(0) 

* Tabulamos la variable target
tabulate mode

* Tabla de ingreso por modalidad de pesca
table mode, stat(count income) stat(mean income) stat(sd income)

* Tabla de precio de pesca por modalidad de pesca
table (result) mode, stat(mean pbeach ppier pprivate pcharter) nformat(%6.0f)




* Parte I : Modelos Logit multinomiales
*-------------------------------------------------------------------------------

* Multinomial logit con respuesta 1
label list modetype

mlogit mode income, baseoutcome(1) nolog
*nota: "baseoutcome" especifica el valor de depvar que se utilizará como categoría base.
*La significancia de "income" se necesita validar con un test conjunto ya que puede variar según la categoría de comparación.

* Wald test para significancia de income
test income

* Riesgo relativo reports exp(b) rather than b
*RRR= ratio risk-relative
mlogit mode income, rr baseoutcome(1) nolog

// Following used below
estimates store MNL

* Predict probabilities of choice of each mode and compare to actual freqs
predict pmlogit1 pmlogit2 pmlogit3 pmlogit4, pr
summarize pmlogit* dbeach dpier dprivate dcharter, separator(4)

* Marginal effect at mean of income change for outcome 3
mfx, predict(pr outcome(3))

* Average marginal effect of income change for outcome 3
margeff, outcome(2)     // Use 2 as outcome; 3 is 2nd after baseoutcome(1)





* Parte II : Modelos Probit multinomiales
*-------------------------------------------------------------------------------

* Multinomial Probit with independent errors and case-invariant regressors
use mus15data.dta, clear
mprobit mode income, baseoutcome(1)

* Multinomial Probit with exchangable errors did not give SEs for variance parameters
* use mus15datalong.dta, clear
* asmprobit d p q, case(id) alternatives(fishmode) casevars(income) correlation(exchangeable) shownrtolerance basealternative(charter)

* Multinomial probit wuth unstructured errors when charter is dropped  
use mus15datalong.dta, clear
drop if fishmode=="charter" | mode == 4
asmprobit d p q, case(id) alternatives(fishmode) casevars(income) correlation(unstructured) structural vce(robust)

* Show correlations and covariance
estat correlation
estat covariance 

* Predicted probabilities and marginal effect of changes in prices 
predict pasmprobit, pr
table fishmode, statistic(mean d) statistic(mean pasmprobit) statistic(sd pasmprobit) nformat(%9.3f)

estat alternatives
estat mfx, varlist(p)

* Same estimator without option structural
use mus15datalong.dta, clear
drop if fishmode=="charter" | mode == 4
asmprobit d p q, case(id) alternatives(fishmode) correlation(unstructured) vce(robust)
estat correlation
estat covariance 

* Compare to conditional logit
use mus15datalong.dta, clear
drop if fishmode=="charter" | mode == 4
asclogit d p q, case(id) alternatives(fishmode) casevars(income) basealternative(pier)




* Parte III : Modelos Ordenados
*-------------------------------------------------------------------------------

* Create multinomial ordered outcome variables takes values y = 1, 2, 3
use mus18data.dta, clear
quietly keep if year==2
generate hlthpf = hlthp + hlthf
generate hlthe = (1 - hlthpf - hlthg)
quietly generate hlthstat = 1 if hlthpf == 1
quietly replace hlthstat = 2 if hlthg == 1
quietly replace hlthstat = 3 if hlthe == 1
label variable hlthstat "health status"
label define hsvalue 1 poor_or_fair 2 good 3 excellent
label values hlthstat hsvalue
tabulate hlthstat

* Summarize dependent and explanatory variables
summarize hlthstat age linc ndisease

* Ordered logit estimates
ologit hlthstat age linc ndisease, nolog

* Calculate predicted probability that y=1, 2 or 3 for each person
predict p1ologit p2ologit p3ologit, pr
summarize hlthpf hlthg hlthe p1ologit p2ologit p3ologit, separator(0)

* Marginal effect at mean for 3rd outcome (health status excellent)
mfx, predict(outcome(3))




* Parte IV : Modelos de con múltiples variables de respuesta
*-------------------------------------------------------------------------------

* Two binary dependent variables: hlthe and dmdvs
tabulate hlthe dmdu
correlate hlthe dmdu

* Bivariate probit estimates
biprobit hlthe dmdu age linc ndisease, nolog

* Predicted probabilities
predict biprob1, pmarg1
predict biprob2, pmarg2
predict biprob11, p11
predict biprob10, p10
predict biprob01, p01
predict biprob00, p00
summarize hlthe dmdu biprob1 biprob2 biprob11 biprob10 biprob01 biprob00

// Following not included in book
* Bivariate probit with different sets of regressors
biprobit (hlthe = age linc) (dmdu = age linc ndisease), nolog

* Nonlinear seemingly unrelated regressions estimator
nlsur (hlthe = normal({a1}*age+{a2}*linc+{a3}*ndisease+{a4})) (dmdu = normal({b1}*age+{b2}*linc+{b3}*ndisease+{b4})), vce(robust) nolog

// Following not included in book
* Separate probits
probit hlthe age linc ndisease
probit dmdu age linc ndisease
