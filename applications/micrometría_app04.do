*====================================================================================
* UNALM
* Microeconometría
* Profesor : Luis Chávez
*-----------------------------
*©copyright 2024
*All other rights are reserved
*====================================================================================



*====================================================================================
* Tópico 04: GMM
*====================================================================================

* Ejemplo 3 : Análisis del gasto en medicamentos prescritos a mayores de 65 años
* Fuente: Cameron, A. y Trivedi, P. (2005) Microeconometrics Using Stata. Stata Press.

* Ejemplo 4 : Data de Automóviles

use example1_gmm.dta, clear
gmm (mpg - {b1}*weight - {b2}*length - {b0}), instruments(weight length)

* Ejemplo 5 : Tasas de Alquiler

use example2_gmm.dta, clear
gmm (rent - {xb:hsngval pcturban} - {b0}), instruments(pcturban faminc reg2-reg4) vce(unadjusted) onestep // Estimador de 1 sólo paso
gmm (rent - {xb:hsngval pcturban} - {b0}), inst(pcturban faminc reg2-reg4) // Estimador de 2 pasos
 
* Ejemplo 6 : Relación entre visitas al médico y tener un seguro privado

use example3_gmm.dta, clear
gmm (docvis - exp({xb:private chronic female income}+{b0})), ///
     instruments(private chronic female age black hispanic) twostep

* Derivadas para un modelo de una sola ecuación
gmm (docvis - exp({b0} + {b1}*income)), instruments(income) ///
     deriv(/b0 = -1*exp({b0} + {b1}*income)) ///
     deriv(/b1 = -1*income*exp({b0}+{b1}*income)) onestep

* Derivadas con una combinación lineal
gmm (docvis - exp({xb:private chronic female income}+{b0})), ///
     instruments(private chronic female age black hispanic) ///
     deriv(/xb = -1*exp({xb:} + {b0})) ///
     deriv(/b0 = -1*exp({xb:} + {b0})) 

* Ejemplo 7 : Ecuación de Euler

* Fuente : Hansen, P. y Singleton, J. (1982) Generalized instrumental variables estimation 
* of nonlinear rational expectations models. Econometrica 50: 1269-1286.

use example4_gmm.dta, clear
generate cgrowth = c / L.c
gmm (1 - {b=1}*(1+F.r)*(F.c/c)^(-1*{gamma=1})), inst(L.r L2.r cgrowth L.cgrowth) wmat(hac nw 4) twostep

* Ejemplo 8 : Estimación por 3SLS

use example5_gmm.dta, clear
gmm (eq1: consump - {b0} - {xb: wagepriv wagegovt}) (eq2: wagepriv - {c0} - {xc: consump govt capital1}), ///
     instruments(eq1: wagegovt govt capital1) ///
     instruments(eq2: wagegovt govt capital1) ///
     winitial(unadjusted, independent) wmatrix(unadjusted) twostep

* Ejemplo 9 : Estimación por GMM usando la función Mata

* Definiendo la data
use example6_gmm.dta, clear
quietly keep if year02 == 1
generate cons = 1
local y docvis
local xlist private chronic female income cons

* Definiendo la función Mata
mata: mata set matastrict off

local y docvis
local xlist private chronic female income cons
local zlist firmsize chronic female income cons

/*
mata
    Xb = X*b'                    // b para el comando optimize es un vector fila de 1xK 
    mu = exp(Xb)
    h = Z'(y-mu)                // h es un vector columna de Rx1
    W = cholinv(Z'Z)             // W es una matriz de RxR
    G = -(mu:*Z)'X               // G es una matriz de RxK
    S = ((y-mu):*Z)'((y-mu):*Z)  // S es una matriz de RxR
    Qb = h'W*h                   // Q(b) es un escalar
    g = G'W*h                    // El gradiente para el comando optimize es un vector fila de 1xK 
    H = G'W*G                    // El hessiano para el comando optimize es una matriz de KxK 
    V = luinv(G'W*G)*G'W*S*W*G*luinv(G'W*G)  
end
*/

* Función Mata para obtener el estimador GMM para la función tipo Poisson usando el comando optimize
mata 
  void pgmm(todo, b, y, X, Z, Qb, g, H)
  {
    Xb = X*b'
    mu = exp(Xb)
    h = Z'(y-mu)
    W = cholinv(cross(Z,Z))
    Qb = h'W*h
    if (todo == 0) return
    G = -(mu:*Z)'X
    g = (G'W*h)'
    if (todo == 1) return
    H = G'W*G
    _makesymmetric(H)
   }
  st_view(y=., ., "`y'")  
  st_view(X=., ., tokens("`xlist'"))
  st_view(Z=., ., tokens("`zlist'"))
  S = optimize_init()
  optimize_init_which(S,"min")
  optimize_init_evaluator(S, &pgmm())
  optimize_init_evaluatortype(S, "d2")
  optimize_init_argument(S, 1, y)
  optimize_init_argument(S, 2, X)
  optimize_init_argument(S, 3, Z)
  optimize_init_params(S, J(1,cols(X),0))
  optimize_init_technique(S,"nr")
  b = optimize(S)  
  // Computamos los estimadores robustos de VCE y SEs
  Xb = X*b'
  mu = exp(Xb)
  h = Z'(y-mu)
  W = cholinv(cross(Z,Z))
  G = -(mu:*Z)'X
  Shat = ((y-mu):*Z)'((y-mu):*Z)*rows(X)/(rows(X)-cols(X))
  Vb = luinv(G'W*G)*G'W*Shat*W*G*luinv(G'W*G)
  seb = (sqrt(diagonal(Vb)))'
  b \ seb 
end

*Fin