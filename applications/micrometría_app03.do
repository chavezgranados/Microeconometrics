*====================================================================================
* UNALM
* Microeconometría
* Profesor : Luis Chávez
*-----------------------------
*©copyright 2024
*All other rights are reserved
*====================================================================================



*====================================================================================
* Tópico 03: IV y endogeneidad
*====================================================================================
* Ejemplo 3 : Análisis del gasto en medicamentos prescritos a mayores de 65 años
* Fuente: Cameron, A. y Trivedi, P. (2005) Microeconometrics Using Stata. Stata Press.


* Parte I : Variables instrumentables
*-----------------------------------------------------------------------------------
clear all
*Definiendo x2list
use example_iv.dta
global x2list totchr age female blhisp linc 
summarize ldrugexp hi_empunion $x2list

* Resumen de las variables disponibles 
summarize ssiratio lowincome multlc firmsz if linc!=.

* Estimación IV de un modelo identificado con un único regresor endógeno
ivregress 2sls ldrugexp (hi_empunion = ssiratio) $x2list, vce(robust) first

* Comparando 5 estimadores y estimaciones de varianza para modelos sobreidentificados
global ivmodel "ldrugexp (hi_empunion = ssiratio multlc) $x2list"
quietly ivregress 2sls $ivmodel, vce(robust)
estimates store TwoSLS
quietly ivregress gmm  $ivmodel, wmatrix(robust) 
estimates store GMM_het
quietly ivregress gmm  $ivmodel, wmatrix(robust) igmm
estimates store GMM_igmm
quietly ivregress gmm  $ivmodel, wmatrix(cluster age) 
estimates store GMM_clu
quietly ivregress 2sls  $ivmodel
estimates store TwoSLS_def
estimates table TwoSLS GMM_het GMM_igmm GMM_clu TwoSLS_def, b(%9.5f) se  

* Obtenemos el estimador OLS para comparar con las estimaciones IV anteriores
regress ldrugexp hi_empunion $x2list, vce(robust) 

* Aplicando el Test de Restricciones Sobreidentificadas
quietly ivregress gmm ldrugexp (hi_empunion = ssiratio multlc) $x2list, wmatrix(robust) 
estat overid

* Aplicando el Test de Sargan
ivreg2 ldrugexp (hi_empunion = ssiratio) $x2list

* Aplicando el Test de Hausman
quietly ivregress 2sls $ivmodel
estimates store TwoSLS
quietly regress ldrugexp hi_empunion $x2list
estimates store OLS
hausman TwoSLS OLS, sigmamore



* Parte II : Instrumentos débiles
*-----------------------------------------------------------------------------------

* Correlaciones de regresores endógenos con instrumentos
correlate hi_empunion ssiratio lowincome multlc firmsz if linc!=.

* Tests de instrumentos débiles - Modelo exactamente identificado
quietly ivregress 2sls ldrugexp (hi_empunion = ssiratio) $x2list, vce(robust)
estat firststage, forcenonrobust all  

* Tests de instrumentos débiles - Mooelos con dos o más restricciones sobreidentificadas
quietly ivregress gmm ldrugexp (hi_empunion = ssiratio lowincome multlc firmsz)  $x2list, vce(robust)
estat firststage, forcenonrobust

* Comparando 4 estimaciones de modelo identificados con diferentes instrumentos
quietly regress ldrugexp hi_empunion $x2list, vce(robust)
estimates store OLS0
quietly ivregress 2sls ldrugexp (hi_empunion=ssiratio) $x2list, vce(robust)
estimates store IV_INST1
quietly estat firststage, forcenonrobust
scalar me1 = r(mineig)
quietly ivregress 2sls ldrugexp (hi_empunion=lowincome) $x2list, vce(robust)
estimates store IV_INST2
quietly estat firststage, forcenonrobust
scalar me2 = r(mineig)
quietly ivregress 2sls ldrugexp (hi_empunion=multlc) $x2list, vce(robust) 
estimates store IV_INST3
quietly estat firststage, forcenonrobust
scalar me3 = r(mineig)
quietly ivregress 2sls ldrugexp (hi_empunion=firmsz) $x2list, vce(robust)
estimates store IV_INST4
quietly estat firststage, forcenonrobust
scalar me4 = r(mineig)
estimates table OLS0 IV_INST1 IV_INST2 IV_INST3 IV_INST4, b(%8.4f) se  
display "Minimum eigenvalues are:     " me1 _s(2) me2 _s(2) me3 _s(2) me4



* Parte III : Mejor inferencia con instrumentos débiles
*-----------------------------------------------------------------------------------

* Prueba condicional e intervalos de confianza para instrumentos débiles
condivreg ldrugexp (hi_empunion = ssiratio) $x2list, lm ar 2sls test(0)

* Variantes del estimador IV: 2SLS, LIML, JIVE, GMM_het, GMM-het usando IVREG2
global ivmodel "ldrugexp (hi_empunion = ssiratio lowincome multlc firmsz) $x2list"
quietly ivregress 2sls $ivmodel, vce(robust)
estimates store TWOSLS
quietly ivregress liml $ivmodel, vce(robust)
estimates store LIML
quietly jive $ivmodel, robust
estimates store JIVE
quietly ivregress gmm $ivmodel, wmatrix(robust) 
estimates store GMM_het
quietly ivreg2 $ivmodel, gmm robust
estimates store IVREG2
estimates table TWOSLS LIML JIVE GMM_het IVREG2, b(%7.4f) se 



* Parte IV : Estimación por 3SLS
*-----------------------------------------------------------------------------------

reg3 (ldrugexp hi_empunion totchr age female blhisp linc) (hi_empunion ldrugexp totchr female blhisp ssiratio)