*====================================================================================
* UNALM
* Microeconometría
* Profesor : Luis Chávez
*-----------------------------
*©copyright 2024
*All other rights are reserved
*====================================================================================


*====================================================================================
* Tópico 09: Evaluación de impacto
*==================================================================================== 

*Fuente: S. Khandker, G. Koolwal and H. Samad (2010). Handbook on Impact Evaluation: Quantitative Methods and Practices
*Base: https://microdata.worldbank.org/index.php/catalog/436/get-microdata

*Diccionario: https://microdata.worldbank.org/index.php/catalog/436/data-dictionary

*outcome -> exptot (gasto total por Household - HH)
*hhland -> tierra del HH antes de conseguir el microcrédito
*thanaid -> ID del distrito (conjunto de aldeas)
*villid -> ID de las aldeas
*dmmfd -> hogar tiene un varon 	que participa en microcrédito 1=si, 0=no
*dfmfd -> hogar tiene una dama	que participa en microcrédito 1=si, 0=no



*PARTE I: Randomización
*-----------------------------------------------------------------------------

global data "C:\Users\LENOVO\Documents\UNALM\Pregrado\Micrometría\Bangladesh2009\data"

use "$data\hh_98.dta", clear
describe
list famsize educhead if (sexhead==0 & agehead<45)

gen lexptot=ln(1+exptot)
gen lnland=ln(1+hhland/100) //se pasa a acre

*Creación de dummies según ubicación del programa por género 
gen vill=thanaid*10+villid
egen progvillm=max(dmmfd), by(vill) //programas en masculinos
tab progvillm
egen progvillf=max(dfmfd), by(vill) //programas en femeninos
tab progvillf

*Impactos de colocación del programa en aldeas (ATE) ------------------
*Prueba de medias y t-test
ttest lexptot, by(progvillm)
ttest lexptot, by(progvillf)

*Alternativamente se puede:
reg lexptot progvillm
reg lexptot progvillf

*Regresión expandida
reg lexptot progvillm sexhead agehead educhead lnland vaccess pcirr rice wheat milk oil egg [pw=weight]
reg lexptot progvillf sexhead agehead educhead lnland vaccess pcirr rice wheat milk oil egg [pw=weight]


*Impactos de participación en el programa (ATE) ---------------------
*(cuando la participación no es random)
*t-test
ttest lexptot, by(dmmfd)
ttest lexptot, by(dfmfd)

*Alternativamente se puede:
reg lexptot dmmfd
reg lexptot dfmfd

*Regresión expandida
reg lexptot dmmfd sexhead agehead educhead lnland vaccess pcirr rice wheat milk oil egg [pw=weight]
reg lexptot dfmfd sexhead agehead educhead lnland vaccess pcirr rice wheat milk oil egg [pw=weight]


*Capturando colocación y participación en el programa (ATE) ---------------------
reg lexptot dmmfd progvillm sexhead agehead educhead lnland vaccess pcirr rice wheat milk oil egg [pw=weight]
reg lexptot dfmfd progvillf sexhead agehead educhead lnland vaccess pcirr rice wheat milk oil egg [pw=weight]


*Impactos de la participación en el programa en las aldeas con programa
reg lexptot dmmfd if progvillm==1 [pw=weight]
reg lexptot dfmfd if progvillf==1 [pw=weight]
reg lexptot dmmfd sexhead agehead educhead lnland vaccess pcirr rice wheat milk oil egg if progvillm==1 [pw=weight]
reg lexptot dfmfd sexhead agehead educhead lnland vaccess pcirr rice wheat milk oil egg if progvillf==1 [pw=weight]

*Efectos indirectos de la colocación de programas de microcrédito (spillovers)
reg lexptot progvillm if dmmfd==0 [pw=weight]
reg lexptot progvillf if dfmfd==0 [pw=weight]

reg lexptot progvillm sexhead agehead educhead lnland vaccess pcirr rice wheat milk oil egg if dmmfd==0 [pw=weight]
reg lexptot progvillf sexhead agehead educhead lnland vaccess pcirr rice wheat milk oil egg if dfmfd==0 [pw=weight]




*PARTE II: Difference in Difference
*-----------------------------------------------------------------------------

use "$data\hh_9198.dta", clear
gen exptot0=exptot if year==0
egen exptot91=max(exptot0), by(nh)
keep if year==1
gen lexptot91=ln(1+exptot91)
gen lexptot98=ln(1+exptot)
gen lexptot9891=lexptot98-lexptot91

*ATE
ttest lexptot9891 if year==1, by(dfmfd)
ttest lexptot9891 if year==1, by(dmmfd)

*Regresión ------------------------------------------------
use "$data\hh_9198.dta", clear
gen lexptot=ln(1+exptot)
gen lnland=ln(1+hhland/100)
gen dmmfd1=dmmfd==1 & year==1
egen dmmfd98=max(dmmfd1), by(nh)
gen dfmfd1=dfmfd==1 & year==1
egen dfmfd98=max(dfmfd1), by(nh)
gen dmmfdyr=dmmfd98*year
gen dfmfdyr=dfmfd98*year

* Modelo básico
* y_it=alpha+beta(T_i*d_t)+theta(T_i)+delta(t_i)+epsilon_it
*beta captura el DiD
reg lexptot year dmmfd98 dmmfdyr
reg lexptot year dfmfd98 dfmfdyr

*Modelo completo
reg lexptot year dmmfd98 dmmfdyr sexhead agehead educhead lnland vaccess pcirr rice wheat milk oil egg [pw=weight]
reg lexptot year dfmfd98 dfmfdyr sexhead agehead educhead lnland vaccess pcirr rice wheat milk oil egg [pw=weight]

*Efectos fijos (m básico)
xtreg lexptot year dmmfd98 dmmfdyr, fe i(nh)
xtreg lexptot year dfmfd98 dfmfdyr, fe i(nh)

*Efectos fijos (m completo)
xtreg lexptot year dmmfd98 dmmfdyr sexhead agehead educhead lnland vaccess pcirr rice wheat milk oil egg, fe i(nh)
xtreg lexptot year dfmfd98 dfmfdyr sexhead agehead educhead lnland vaccess pcirr rice wheat milk oil egg, fe i(nh)


***DiD en cross-sectional data -----------------------------------
use "$data\hh_91.dta",clear
gen vill=thanaid*10+villid
gen lexptot=ln(1+exptot)
gen lnland=ln(1+hhland/100)
gen target=hhland<50
gen progvill=thanaid<25
gen progtarget=progvill*target

sum target if progvill==1

reg lexptot progvill target progtarget
reg lexptot progvill target progtarget sexhead agehead educhead lnland vaccess pcirr rice wheat milk oil egg [pw=weight]
xtreg lexptot progvill target progtarget, fe i(vill)
xtreg lexptot progvill target progtarget sexhead agehead educhead lnland, fe i(vill)


*Tomando en cuenta las condiciones iniciales
use "$data\hh_9198.dta", clear

gen lexptot=ln(1+exptot)
gen lnland=ln(1+hhland/100)
gen dmmfd1=dmmfd==1 & year==1
egen dmmfd98=max(dmmfd1), by(nh)
gen dfmfd1=dfmfd==1 & year==1
egen dfmfd98=max(dfmfd1), by(nh)
gen dmmfdyr=dmmfd98*year
gen dfmfdyr=dfmfd98*year
drop dmmfd1 dfmfd1

sort nh year
by nh: gen dlexptot=lexptot[2]-lexptot[1]
by nh: gen ddmmfd98= dmmfd98[2]- dmmfd98[1]
by nh: gen ddfmfd98= dfmfd98[2]- dfmfd98[1]
by nh: gen ddmmfdyr= dmmfdyr[2]- dmmfdyr[1]
by nh: gen ddfmfdyr= dfmfdyr[2]- dfmfdyr[1]
by nh: gen dsexhead= sexhead[2]- sexhead[1]
by nh: gen dagehead= agehead[2]- agehead[1]
by nh: gen deduchead= educhead[2]- educhead[1]
by nh: gen dlnland= lnland[2]- lnland[1]
by nh: gen dvaccess= vaccess[2]- vaccess[1]
by nh: gen dpcirr= pcirr[2]- pcirr[1]
by nh: gen drice= rice[2]- rice[1]
by nh: gen dwheat= wheat[2]- wheat[1]
by nh: gen dmilk= milk[2]- milk[1]
by nh: gen doil= oil[2]- oil[1]
by nh: gen degg= egg[2]- egg[1]

reg dlexptot ddmmfd98 ddmmfdyr dsexhead dagehead deduchead dlnland dvaccess dpcirr drice dwheat dmilk doil degg sexhead agehead educhead lnland vaccess pcirr rice wheat milk oil egg if year==0 [pw=weight]

reg dlexptot ddfmfd98 ddfmfdyr dsexhead dagehead deduchead dlnland dvaccess dpcirr drice dwheat dmilk doil degg sexhead agehead educhead lnland vaccess pcirr rice wheat milk oil egg if year==0 [pw=weight]



* DiD con  PSM -------------------------------------------------
* Male paticipants
use "$data\hh_9198.dta", clear
gen lnland=ln(1+hhland/100)
gen dmmfd1=dmmfd==1 & year==1
egen dmmfd98=max(dmmfd1), by(nh)
keep if year==0
*install st0026_2.pkg, from(http://www.stata-journal.com/software/sj3-3/)
pscore dmmfd98 sexhead agehead educhead lnland vaccess pcirr rice wheat milk oil egg [pw=weight], pscore(ps98) blockid(blockf1) comsup level(0.001)
keep if blockf1!=.
keep nh
sort nh
merge 1:m nh using "$data\hh_9198.dta"
keep if _merge==3
gen lexptot=ln(1+exptot)
gen lnland=ln(1+hhland/100)
gen dmmfd1=dmmfd==1 & year==1
egen dmmfd98=max(dmmfd1), by(nh)
gen dmmfdyr=dmmfd98*year

xtreg lexptot year dmmfd98 dmmfdyr sexhead agehead educhead lnland vaccess pcirr rice wheat milk oil egg, fe i(nh)

* Female paticipants
use "$data\hh_9198.dta", clear
gen lnland=ln(1+hhland/100)
gen dfmfd1=dfmfd==1 & year==1
egen dfmfd98=max(dfmfd1), by(nh)
keep if year==0

pscore dfmfd98 sexhead agehead educhead lnland vaccess pcirr rice wheat milk oil egg [pw=weight], pscore(ps98) blockid(blockf1) comsup level(0.001)
keep if blockf1!=.
keep nh
sort nh
merge 1:m nh using "$data\hh_9198.dta"
keep if _merge==3
gen lexptot=ln(1+exptot)
gen lnland=ln(1+hhland/100)
gen dfmfd1=dfmfd==1 & year==1
egen dfmfd98=max(dfmfd1), by(nh)
gen dfmfdyr=dfmfd98*year

xtreg lexptot year dfmfd98 dfmfdyr sexhead agehead educhead lnland vaccess pcirr rice wheat milk oil egg, fe i(nh)



*PARTE III: PSM
*-----------------------------------------------------------------------------
use "$data\hh_9198.dta", clear
gen lexptot=ln(1+exptot)
gen lnland=ln(1+hhland/100)


****Impactos de participación en programa --------------------

***Participación de varones
*pscore
pscore dmmfd sexhead agehead educhead lnland vaccess pcirr rice wheat milk oil egg [pw=weight], pscore(ps98) blockid(blockf1) comsup level(0.001)
drop ps98 blockf1

pscore dmmfd sexhead agehead educhead vaccess pcirr rice wheat milk oil [pw=weight], pscore(ps98) blockid(blockf1) comsup level(0.001)

*Nearest Neighbor Matching
attnd lexptot dmmfd [pweight=weight], pscore(ps98) comsup

*Stratification Matching
atts lexptot dmmfd, pscore(ps98) blockid(blockf1) comsup

*Radius Matching
attr lexptot dmmfd, pscore(ps98) radius(0.001) comsup

*Kernel Matching;
attk lexptot dmmfd, pscore(ps98) comsup bootstrap reps(50)
drop ps98 blockf1


***Participación de mujeres
use "$data\hh_9198.dta", clear
gen lexptot=ln(1+exptot)
gen lnland=ln(1+hhland/100)
*pscore
pscore dfmfd sexhead agehead educhead lnland vaccess pcirr rice wheat milk oil egg [pw=weight], pscore(ps98) blockid(blockf1) comsup level(0.001)

*Nearest Neighbor Matching
attnd lexptot dfmfd [pweight=weight], pscore(ps98) comsup

*Stratification Matching
atts lexptot dfmfd, pscore(ps98) blockid(blockf1) comsup

*Radius Matching
attr lexptot dfmfd, pscore(ps98) radius(0.001) comsup

*Kernel Matching
attk lexptot dfmfd, pscore(ps98) comsup bootstrap reps(50)


*Direct Matching using Nearest neighbor

*install st0072/nnmatch.ado
nnmatch lexptot dmmfd sexhead agehead educhead lnland vaccess pcirr rice wheat milk oil egg [pw=weight], tc(att) m(1)

nnmatch lexptot dfmfd sexhead agehead educhead lnland vaccess pcirr rice wheat milk oil egg [pw=weight], tc(att) m(1)