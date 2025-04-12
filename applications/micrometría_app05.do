*====================================================================================
* UNALM
* Microeconometría
* Profesor : Luis Chávez
*-----------------------------
*©copyright 2024
*All other rights are reserved
*====================================================================================


*====================================================================================
* Tópico 05: Modelos binarios
*====================================================================================
*Ejemplo 1: estudio de Salud y Jubilación (HRS), encuesta a beneficiarios de Medicare
* Fuente: Cameron, A. y Trivedi, P. (2005) Microeconometrics Using Stata. Stata Press.

* priv: compra de seguros privados.
* hstatusg: estado de salud bueno, muy bueno o ecelente.
* adl: número de limitaciones en actividades de la vida diaria.
* chronic: número de afecciones crónicas.
* hhincom. ingreso del hogar.
* linc: log del ingreso familiar, en caso sea positivo.

clear all

cd "C:\Users\LENOVO\Documents\UNALM\Pregrado\Micrometría\cameron (2009)"
* Abrir data
use mus14data.dta

* Dropeamos interaction variables
*drop age2 agefem agechr agewhi

* Resumen estadístico
global xlist age hstatusg hhincome educyear married hisp
generate linc = ln(hhinc)
global extralist linc female white chronic adl sretire
summarize ins retire $xlist $extralist



* Parte I : Modelos Logit y probit
*-------------------------------------------------------------------------------

*COMANDO: logit depvar [ indepvars ] [ if ] [ in ] [ weight ] [, options]

*Regresión logistica
logit ins retire $xlist
 
* Estimación de diferentes modelos
quietly logit ins retire $xlist
estimates store blogit
quietly probit ins retire $xlist 
estimates store bprobit
quietly regress ins retire $xlist 
estimates store bols
quietly logit ins retire $xlist, vce(robust)
estimates store blogitr
quietly probit ins retire $xlist, vce(robust)
estimates store bprobitr
quietly regress ins retire $xlist, vce(robust)
estimates store bolsr

* Comparación de significancia de modelos
esttab blogit blogitr bprobit bprobitr bols bolsr

* Estadísticas de selección de modelos
estimates stats blogit blogitr bprobit bprobitr bols bolsr




* Parte II : Test de especificación
*-------------------------------------------------------------------------------

* Wald test
*HO: coeficiente de las variables interacción con ''age'' son cero
generate age2 = age*age
generate agefem = age*female
generate agechr = age*chronic
generate agewhi = age*white
global intlist age2 agefem agechr agewhi //lista alterna
quietly logit ins retire $xlist $intlist
test $intlist

*Otra forma:
global inlist c.age#c.age c.age#i.hstatusg c.age#c.hhincome c.age#c.educyear c.age#i.married c.age#i.hisp // "i." permite crear diferencias en los grupos
quietly logit ins retire $xlist $intlist, vce(robust) nolog
testparm $intlist

* Likelihood-ratio test (el equivalente asintótico de Wald)
quietly logit ins retire $xlist $intlist
estimates store B 
quietly logit ins retire $xlist
lrtest B 

* LM test para la familia logit
quietly logit ins retire $xlist
predict xbhat, xb
generate xbhatsq = xbhat^2 //añadimos forma cuadrática al modelo
quietly logit ins retire $xlist xbhatsq
test xbhatsq //testeamos forma cuadrática (si es significativa, el modelo no es lineal)

* Ajustando heteroscedasticidad en el probit
hetprob ins retire $xlist, het(chronic) nolog




* Parte III : Bondad de ajuste y predicción
*-------------------------------------------------------------------------------

* Test de Hosmer-Lemeshow (GOF) con 4 grupos
*H0: el modelo se ajusta bien a los datos (no hay diferencia significativa entre los valores observados y predichos en los diferentes grupos)
quietly logit ins retire $xlist
estat gof, group(4) table // Hosmer-Lemeshow gof test
*Setear el número de grupos (a 10 o 15) y analizar nuevamente para ratificar conclusión.

* Matriz de confusión ( comparamos valores predichos y actuales)
quietly logit ins retire $xlist
estat classification

*Tasa de exactitud (accuracy):
display (345+1657)/3206*100
* Evaluar: verdaderos positivos, verdaderos negativos, falsos positivos y falsos negativos. Tabién evaluar precisión, sesgo, sensibilidad, especificidad.

* Probabilidades predichas
quietly logit ins hhincome
predict plogit, pr
quietly probit ins hhincome  
predict pprobit, pr
quietly regress ins hhincome
predict pols, xb
summarize ins plogit pprobit pols

* Gráfica de predicción
sort hhincome
graph twoway (scatter ins hhincome, msize(vsmall) jitter(3)) /*
  */ (line plogit hhincome, clstyle(p1)) /*
  */ (line pprobit hhincome, clstyle(p2)) /*
  */ (line pols hhincome, clstyle(p3)), /*
  */ scale (1.2) plotregion(style(none)) /*
  */ title("Probabilidades ajustadas según modelos") /*
  */ xtitle("HHINCOME (hhincome)", size(medlarge)) xscale(titlegap(*5)) /* 
  */ ytitle("Probabilidad predicha", size(medlarge)) yscale(titlegap(*5)) /*
  */ legend(pos(1) ring(0) col(1)) legend(size(small)) /*
  */ legend(label(1 "Actual Data (jittered)") label(2 "Logit") /*
  */         label(3 "Probit") label(4 "OLS"))
*graph export fig1.eps, replace

* Gráfica de sensibilidad
quietly logit ins hhincome
lroc, lwidth(thick) title("ROC") msize(tiny)





* Parte IV : Efectos marginales
*-------------------------------------------------------------------------------

*Probabilidad individual:
quietly logit ins retire $xlist
margins, at(age=65 retire=0 hstatusg=1 hhincome=1 educyear=17 married=1 hisp=0)
*Interpretación: para esas caracterist., la probabilidad de tener seguro es 0.5427
*Alternativa: comando "prvalue".

* Marginal effects (MER) en logit
*MER mide el efecto de puntos específicos en los datos
quietly logit ins retire $xlist  
mfx, at(1 75 1 35 12 1 1) // MER (Marginal Effects at Representative)

* Marginal effects (MEM) en logit
*MEM mide el efecto marginal de una regresora cuando el resto toma su valor medio.
quietly logit ins retire $xlist 
mfx  // MEM (Marginal Effects at the Mean)

* Marginal effects (AME) after logit
*AME mide el promedio de los efectos marginales de cada observación.
quietly logit ins retire $xlist 
margins, dydx(*) // AME (Average marginal effect)

* Cambio en la probabilidad, logit
quietly logit ins retire $xlist
* Requiere install (Long & Freese, 2006)
prchange hhincome




* Parte V : Endegeneidad
*-------------------------------------------------------------------------------

use mus14data.dta, clear
* Probit endógeno usando inconsistente probit MLE
generate linc = log(hhincome)
global xlist2 female age age2 educyear married hisp white chronic adl hstatusg
probit ins linc $xlist2, vce(robust) nolog

* Probit endógeno using ivprobit ML estimator
global ivlist2 retire sretire
ivprobit ins $xlist2 (linc = $ivlist2), vce(robust) nolog

* Probit endógeno using ivprobit 2-step estimator
ivprobit ins $xlist2 (linc = $ivlist2), twostep first

* Probit endógeno using ivregress para 2SLS estimator
ivregress 2sls ins $xlist2 (linc = $ivlist2), vce(robust) noheader
estat overid
*H0: instrumentos válidos y no correlacionados con el error




* Parte V : Datos agrupados
*-------------------------------------------------------------------------------

* Usando mus14data.dta para agrupar datos 
sort age
collapse av_ret=retire av_hhinc=hhincome av_educyear=educyear av_mar=married av_adl=adl av_hisp=hisp av_hstatusg=hstatusg av_ins=ins, by(age)

generate logins = log(av_ins/(1-av_ins))
save mus14gdata.dta, replace

*Summarize grouped data
summarize logins  $xlistg

* Regresión con grouped data
regress logins av_ret av_hstatusg av_hhinc av_educyear av_mar av_hisp, vce(robust)

