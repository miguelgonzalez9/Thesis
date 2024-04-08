*********************************************************************************
* 	This do file creates tables1-table5											*		
*																				*
*	This do file: 																*
*	- Produces figure 1, which is the McCrary test graphic. 					*
*																				*
*																				*
*	Creates McCrary.pdf and baseline_RDplot_linear.pdf and 						*
*	baseline_RDplot_quadratic.pdf, being the first the Figure 2 and the last 	*
*	two the plots presented on figure 3.										*
*																				*
*	Runs baseline regression on outcome variables and outputs tables			*																
*	1-5. First two correspond to effects on spending while 3-5 effects on 		*																	
*	leaders																		*
*																				*																	
*********************************************************************************

// Load data
cd "D:\Documents\GitHub\Thesis\Data\Final_data"
use "RD_data", replace

// Create new variables
gen lead1_lid_dummy = 0
replace lead1_lid_dummy = 1 if lead1_pop_lid_asis  > 0

gen win_left = 0
replace win_left = 1 if share_diff1_l > 0

gen win_right = 0
replace win_right = 1 if share_diff2_r > 0

// Local macros

global geo_controls altura discapital dismdo disbogota gcaribe gpacifica gorinoquia gamazonia areaoficialkm2
global social_controls ind_rural left_share_94 right_share_94 IPM Violencia_48_a_53 conflicto
global SD_vars  lead*_pop_lid_SD 
global SIVel_outcomes lead*_pop_vio lead*pop_lid_asis lead*pop_threat lead*pop_collective_violence lead*_pop_failed_asis lead*_pop_comunal_sector
global electoral_vars *_r *_l
global spending_outcomes V_ln_payments_lead1 V_ln_payments_lead2 V1_ln_payments_lead1 V_payment_diff1 V_payment_diff2 V1_payment_diff1


label variable V_payment_diff1 "Change in victims' spending"
label variable win_left "Left wing mayor elected"
label variable win_right "Right wing mayor elected"


cd "D:\Documents\GitHub\Thesis\Figures"

* Delete existing files to overwrite.
cap erase "table_1a.tex"
cap erase "table_1a.txt"
cap erase "table_1b.tex"
cap erase "table_1b.txt"
 cap erase "table_2a.tex"
cap erase "table_2a.txt"
 cap erase "table_3a.tex"
cap erase "table_3a.txt"
cap erase "table_4a.tex"
cap erase "table_4a.txt"
cap erase "table_5a.tex"
cap erase "table_5a.txt"

// Table 1. Descriptive Stats 
est clear
estpost tabstat `SIVel_outcomes' `social_controls', ///
 c(stat) statistics(count mean sd min max)  

ereturn list // list the stored locals

esttab using "D:\Documents\GitHub\Thesis\Figures\table1.tex", replace ///
   cells("count mean(fmt(%6.2fc)) sd(fmt(%6.2fc)) min max") nonumber ///
   nomtitle nonote noobs gap label  /// 
   collabels("N" "Mean" "SD" "Min" "Max") ///
   title("Table 1 Descriptive Statistics \label{table1stata}")
   

 
// 4. Figure 1. McCarry and density tst Test

DCdensity share_diff2_r, breakpoint(0) generate(Xj Yj r0 fhat se_fhat) 
local jump = round(`r(theta)',0.001)
local jump_se= round(`r(se)',0.001)
local binsize= round(`r(binsize)',0.001)
* Graph adjustments
gr_edit .style.editstyle boxstyle(shadestyle(color(white))) editcopy
gr_edit .note.text = {}
gr_edit .note.text.Arrpush Each point represents a bin. Bin size is `binsize'
gr_edit .note.text.Arrpush Discontinuity estimate (standard error): `jump' (`jump_se')
gr_edit .xaxis1.reset_rule -0.5 0.5 .1 , tickset(major) ruletype(range)
gr_edit .xaxis1.title.text = {}
gr_edit .xaxis1.title.text.Arrpush Relative vote share for right party 2015, 2019
gr_edit .yaxis1.title.text = {}
gr_edit .yaxis1.title.text.Arrpush Density

* drop not longer needed variables
drop Xj Yj r0 fhat se_fhat
* save graph
graph export "D:\Documents\GitHub\Thesis\Figures\McCrary_Right.pdf", replace


DCdensity share_diff1_l, breakpoint(0) generate(Xj Yj r0 fhat se_fhat) 
local jump = round(`r(theta)',0.001)
local jump_se= round(`r(se)',0.001)
local binsize= round(`r(binsize)',0.001)
* Graph adjustments
gr_edit .style.editstyle boxstyle(shadestyle(color(white))) editcopy
gr_edit .note.text = {}
gr_edit .note.text.Arrpush Each point represents a bin. Bin size is `binsize'
gr_edit .note.text.Arrpush Discontinuity estimate (standard error): `jump' (`jump_se')
gr_edit .xaxis1.reset_rule -0.5 0.5 .1 , tickset(major) ruletype(range)
gr_edit .xaxis1.title.text = {}
gr_edit .xaxis1.title.text.Arrpush Relative vote share for left party 2015, 2019
gr_edit .yaxis1.title.text = {}
gr_edit .yaxis1.title.text.Arrpush Density

* drop not longer needed variables
drop Xj Yj r0 fhat se_fhat
* save graph
graph export "D:\Documents\GitHub\Thesis\Figures\McCrary_Right_Left.pdf", replace


*****************************************
*	Baseline results: 					*
*	Effect on armed victims spending	*
*	Effect on social leader's killings	*
*****************************************	

************************************** 
* Spending on armed conflict victims**
**************************************


******************* Non paremetric estimates

****** Linear polynomial
global geo_controls altura discapital dismdo disbogota gcaribe gpacifica gorinoquia gamazonia areaoficialkm2
global social_controls ind_rural left_share_94 right_share_94 IPM Violencia_48_a_53 conflicto
lead*_pop_vio lead*pop_lid_asis lead*pop_threat lead*pop_collective_violence lead*_pop_failed_asis lead*_pop_comunal_sector


* Linear polynomials but with no covariates
rdrobust V_payment_diff1 share_diff2_r, all p(1)  bwselect(mserd) 
local bw= round(`e(h_l)',0.001)
local bw_lin = round(`e(h_l)',0.001)
local obs =`e(N_h_l)'+`e(N_h_r)'
local poly = `e(p)'
outreg2 using table_1a.tex, tex(frag) ctitle(" ") nonotes addstat(Observations, `obs', Bandwidth, `bw', (Local) polynomial order, e(p)) afmt(fc) append noobs addtext(Geographic Controls, NO, Social Economic Controls, NO)
* Linear polynomials but with geographical covariates
rdrobust V_payment_diff1 share_diff2_r, all p(1)  bwselect(mserd) covs(`geo_controls')
local bw=round(`e(h_l)',0.001)
local obs =`e(N_h_l)'+`e(N_h_r)'
outreg2 using table_1a.tex, tex(frag) ctitle(" ") nonotes addstat(Observations, `obs', Bandwidth, `bw', (Local) polynomial order, e(p)) afmt(fc) append noobs addtext(Geographic Controls, YES, Social Economic Controls, NO) append
* Linear polynomials with socio-economic covariates
rdrobust V_payment_diff1 share_diff2_r, all p(1) bwselect(mserd) covs(`social_controls')
local bw=round(`e(h_l)',0.001)
local obs =`e(N_h_l)'+`e(N_h_r)'
outreg2 using table_1a.tex,  tex(frag) ctitle(" ") nonotes addstat(Observations, `obs', Bandwidth, `bw', (Local) polynomial order, e(p)) afmt(fc) append noobs addtext(Geographic Controls, NO, Social Economic Controls, YES)
* Linear polynomials with all covariates
rdrobust V_payment_diff1 share_diff2_r, all p(1) bwselect(mserd) covs(`geo_controls' `social_controls')
local bw=round(`e(h_l)',0.001)
local obs =`e(N_h_l)'+`e(N_h_r)'
outreg2 using table_1a.tex,  tex(frag) ctitle(" ") nonotes addstat(Observations, `obs', Bandwidth, `bw', (Local) polynomial order, e(p)) afmt(fc) append noobs addtext(Geographic Controls, YES, Social Economic Controls, YES)

* Quadratic polynomials
rdrobust V_payment_diff1 share_diff2_r, all p(2) bwselect(mserd) 
local bw=round(`e(h_l)',0.001)
local bw_quad = round(`e(h_l)',0.001)
display `bw'
local poly2 = `e(p)'
local obs =`e(N_h_l)'+`e(N_h_r)'
outreg2 using table_1a.tex, tex(frag) ctitle(" ") nonotes addstat(Observations, `obs', Bandwidth, `bw', (Local) polynomial order, e(p)) afmt(fc) append label noobs addtext(Geographic Controls, NO, Social Economic Controls, NO)
* Quadratic polynomials but with geographical covariates
rdrobust V_payment_diff1 share_diff2_r, all p(2) bwselect(mserd) covs(`geo_controls')
local bw=round(`e(h_l)',0.001)
local obs =`e(N_h_l)'+`e(N_h_r)'
outreg2 using table_1a.tex,  tex(frag) ctitle(" ") nonotes addstat(Observations, `obs', Bandwidth, `bw', (Local) polynomial order, e(p)) afmt(fc) append noobs addtext(Geographic Controls, YES, Social Economic Controls, NO)
* Quadratic polynomials with socio-economic covariates
rdrobust V_payment_diff1 share_diff2_r, all p(2) bwselect(mserd) covs(`social_controls')
local bw=round(`e(h_l)',0.001)
local obs =`e(N_h_l)'+`e(N_h_r)'
outreg2 using table_1a.tex,  tex(frag) ctitle(" ") nonotes addstat(Observations, `obs', Bandwidth, `bw', (Local) polynomial order, e(p)) afmt(fc) append noobs addtext(Geographic Controls, NO, Social Economic Controls, YES)
* Quadratic polynomials with all covariates
rdrobust V_payment_diff1 X_it if ideol_incum != 2, all p(2) bwselect(mserd) covs(`geo_controls' `social_controls')
local bw=round(`e(h_l)',0.001)
local obs =`e(N_h_l)'+`e(N_h_r)'
outreg2 using table_1a.tex,  tex(frag) ctitle(" ") nonotes addstat(Observations, `obs', Bandwidth, `bw', (Local) polynomial order, e(p)) afmt(fc) append noobs addtext(Geographic Controls, YES, Social Economic Controls, YES)


******************* Parametric estimates

* Generate the triangular Kernel Weights for the parametric estimates.
gen u = . 
gen bandw = `bw_lin' 
replace u=X_it/bandw if X_it<=0 
replace u=X_it/bandw if X_it>0
g tweight=1-abs(u)
gen X_it_TR = Tr*X_it
***** Linear polynomials
 * No covariates
reg V_payment_diff1 Tr X_it X_it_TR [pweight=tweight]  if ideol_incum != 2 &  abs(X_it)<=`bw_lin', robust
outreg2 using table_1b.tex,  tex(frag) ctitle(" ") nonotes keep(Tr) addstat(Bandwidth, `bw_lin', (Local) polynomial order, `poly') afmt(fc) append nocons addtext(Geographic Controls, NO, Social Economic Controls, NO)
* Geo controls 
reg V_payment_diff1 Tr X_it X_it_TR `geo_controls' [pweight=tweight]  if ideol_incum != 2 &  abs(X_it)<=`bw_lin', robust
outreg2 using table_1b.tex,  tex(frag) ctitle(" ") nonotes keep(Tr) addstat(Bandwidth, `bw_lin', (Local) polynomial order, `poly') afmt(fc) append nocons addtext(Geographic Controls, YES, Social Economic Controls, NO)
* Social controls
reg V_payment_diff1 Tr X_it X_it_TR `social_controls' [pweight=tweight]  if ideol_incum != 2 &  abs(X_it)<=`bw_lin', robust
outreg2 using table_1b.tex,  tex(frag) ctitle(" ") nonotes keep(Tr) addstat(Bandwidth, `bw_lin', (Local) polynomial order, `poly') afmt(fc) append nocons addtext(Geographic Controls, NO, Social Economic Controls, YES)
* All controls
reg V_payment_diff1 Tr X_it X_it_TR `geo_controls' `social_controls' [pweight=tweight]  if ideol_incum != 2 &  abs(X_it)<=`bw_lin', robust
outreg2 using table_1b.tex,  tex(frag) ctitle(" ") nonotes keep(Tr) addstat(Bandwidth, `bw_lin', (Local) polynomial order, `poly') afmt(fc) append nocons  addtext(Geographic Controls, YES, Social Economic Controls, YES)

***** Quadratic polynomials

gen X_it_2 = X_it^2
gen X_it_TR_2 = X_it_TR^2
 * No covariates
reg V_payment_diff1 Tr X_it X_it_TR X_it_2 X_it_TR_2[pweight=tweight]  if ideol_incum != 2 &  abs(X_it)<=`bw_lin', robust
outreg2 using table_1b.tex,  tex(frag) ctitle(" ") nonotes keep(Tr) addstat(Bandwidth, `bw_lin', (Local) polynomial order, `poly2') afmt(fc) append nocons addtext(Geographic Controls, NO, Social Economic Controls, NO)
* Geo controls 
reg V_payment_diff1 Tr X_it X_it_TR X_it_TR X_it_2 X_it_TR_2 `geo_controls' [pweight=tweight]  if ideol_incum != 2 &  abs(X_it)<=`bw_lin', robust
outreg2 using table_1b.tex,  tex(frag) ctitle(" ") nonotes keep(Tr) addstat(Bandwidth, `bw_lin', (Local) polynomial order, `poly2') afmt(fc) append nocons addtext(Geographic Controls, YES, Social Economic Controls, NO)
* Social controls
reg V_payment_diff1 Tr X_it X_it_TR X_it_TR X_it_2 X_it_TR_2 `social_controls'[pweight=tweight]  if ideol_incum != 2 &  abs(X_it)<=`bw_lin', robust
outreg2 using table_1b.tex,  tex(frag) ctitle(" ") nonotes keep(Tr) addstat(Bandwidth, `bw_lin', (Local) polynomial order, `poly2') afmt(fc) append nocons addtext(Geographic Controls, NO, Social Economic Controls, YES)
* All controls
reg V_payment_diff1 Tr X_it X_it_TR X_it_TR X_it_2 X_it_TR_2 `geo_controls' `social_controls' [pweight=tweight]  if ideol_incum != 2 &  abs(X_it)<=`bw_lin', robust
outreg2 using table_1b.tex,  tex(frag) ctitle(" ") nonotes keep(Tr) addstat(Bandwidth, `bw_lin', (Local) polynomial order, `poly2') afmt(fc) append nocons  addtext(Geographic Controls, YES, Social Economic Controls, YES)


capture erase table_1a.txt
capture erase table_1b.txt




************************************************
* 		Effect on social leader killings
* 	Killings normalized by population 
************************************************

******************* Non paremetric estimates. Table 2a-5a (Somos defensors, INDEPAZ, SD_comunal, SD left, SD dummy)

****** Linear polynomial

local i = 1
foreach x in `lead_out' {
	
	local i = `i' + 1
	* Linear polynomials but with no covariates
	rdrobust `x' X_it if ideol_incum != 2, all p(1) bwselect(mserd) 
	local bw= round(`e(h_l)',0.001)
	local bw_lin = round(`e(h_l)',0.001)
	local obs =`e(N_h_l)'+`e(N_h_r)'
	local poly = `e(p)'
	outreg2 using table_`i'a.tex, tex(frag) ctitle(" ") nonotes addstat(Observations, `obs', Bandwidth, `bw', (Local) polynomial order, e(p)) afmt(fc) append noobs addtext(Geographic Controls, NO, Social Economic Controls, NO)
	
	
	* Linear polynomials but with geographical covariates
	rdrobust `x' X_it if ideol_incum != 2, all p(1) bwselect(mserd) covs(`geo_controls')
	local bw=round(`e(h_l)',0.001)
	local obs =`e(N_h_l)'+`e(N_h_r)'
	outreg2 using table_`i'a.tex,  tex(frag) ctitle(" ") nonotes addstat(Observations, `obs', Bandwidth, `bw', (Local) polynomial order, e(p)) afmt(fc) append noobs addtext(Geographic Controls, YES, Social Economic Controls, NO)
	
	* Linear polynomials with socio-economic covariates
	rdrobust `x' X_it if ideol_incum != 2, all p(1) bwselect(mserd) covs(`social_controls')
	local bw=round(`e(h_l)',0.001)
	local obs =`e(N_h_l)'+`e(N_h_r)'
	outreg2 using table_`i'a.tex,  tex(frag) ctitle(" ") nonotes addstat(Observations, `obs', Bandwidth, `bw', (Local) polynomial order, e(p)) afmt(fc) append noobs addtext(Geographic Controls, NO, Social Economic Controls, YES)
	
	* Linear polynomials with all covariates
	rdrobust `x' X_it if ideol_incum != 2, all p(1) bwselect(mserd) covs(`social_controls' `geo_controls')
	local bw=round(`e(h_l)',0.001)
	local obs =`e(N_h_l)'+`e(N_h_r)'
	outreg2 using table_`i'a.tex,  tex(frag) ctitle(" ") nonotes addstat(Observations, `obs', Bandwidth, `bw', (Local) polynomial order, e(p)) afmt(fc) append noobs addtext(Geographic Controls, YES, Social Economic Controls, YES)
	
		* Quadratic polynomials
	rdrobust `x' X_it if ideol_incum != 2, all p(2) bwselect(mserd) 
	local bw=round(`e(h_l)',0.001)
	local bw_quad = round(`e(h_l)',0.001)
	display `bw'
	local poly2 = `e(p)'
	local obs =`e(N_h_l)'+`e(N_h_r)'
	outreg2 using table_`i'a.tex, tex(frag) ctitle(" ") nonotes  addstat(Observations, `obs', Bandwidth, `bw', (Local) polynomial order, e(p)) afmt(fc) append label noobs addtext(Geographic Controls, NO, Social Economic Controls, NO)
	
	* Quadratic polynomials but with geographical covariates
	rdrobust `x' X_it if ideol_incum != 2, all p(2)  bwselect(mserd) covs(`geo_controls')
	local bw=round(`e(h_l)',0.001)
	local obs =`e(N_h_l)'+`e(N_h_r)'
	outreg2 using table_`i'a.tex,  tex(frag) ctitle(" ")  nonotes addstat(Observations, `obs', Bandwidth, `bw', (Local) polynomial order, e(p)) afmt(fc) append noobs addtext(Geographic Controls, YES, Social Economic Controls, NO)
	
	* Quadratic polynomials with socio-economic covariates
	rdrobust `x' X_it if ideol_incum != 2, all p(2) bwselect(mserd) covs(`social_controls')
	local bw=round(`e(h_l)',0.001)
	local obs =`e(N_h_l)'+`e(N_h_r)'
	outreg2 using table_`i'a.tex,  tex(frag) ctitle(" ")  nonotes addstat(Observations, `obs', Bandwidth, `bw', (Local) polynomial order, e(p)) afmt(fc) append noobs addtext(Geographic Controls, NO, Social Economic Controls, YES)
	
	* Quadratic polynomials with all covariates
	rdrobust `x' X_it if ideol_incum != 2, all p(2) bwselect(mserd) covs(`social_controls' `geo_controls')
	local bw=round(`e(h_l)',0.001)
	local obs =`e(N_h_l)'+`e(N_h_r)'
	outreg2 using table_`i'a.tex,  tex(frag) ctitle(" ")  nonotes addstat(Observations, `obs', Bandwidth, `bw', (Local) polynomial order, e(p)) afmt(fc) append noobs addtext(Geographic Controls, YES, Social Economic Controls, YES)
	
		
}

drop u bandw tweight

*****************************************
* Baseline results on general violence**
****************************************

local vio_vars lead1_homic_caso lead1_homic_vict lead1_terrorismot lead1_homicidios lead1_o_homic

local i = 6
foreach x in `vio_vars' {
	
	local i = `i' + 1
	* Linear polynomials but with no covariates
	rdrobust `x' X_it if ideol_incum != 2, all p(1) bwselect(mserd) 
	local bw= round(`e(h_l)',0.001)
	local bw_lin = round(`e(h_l)',0.001)
	local obs =`e(N_h_l)'+`e(N_h_r)'
	local poly = `e(p)'
	outreg2 using table_`i'a.tex, tex(frag) ctitle(" ") nonotes addstat(Observations, `obs', Bandwidth, `bw', (Local) polynomial order, e(p)) afmt(fc) append noobs addtext(Geographic Controls, NO, Social Economic Controls, NO)
	
	
	* Linear polynomials but with geographical covariates
	rdrobust `x' X_it if ideol_incum != 2, all p(1) bwselect(mserd) covs(`geo_controls')
	local bw=round(`e(h_l)',0.001)
	local obs =`e(N_h_l)'+`e(N_h_r)'
	outreg2 using table_`i'a.tex,  tex(frag) ctitle(" ") nonotes addstat(Observations, `obs', Bandwidth, `bw', (Local) polynomial order, e(p)) afmt(fc) append noobs addtext(Geographic Controls, YES, Social Economic Controls, NO)
	
	* Linear polynomials with socio-economic covariates
	rdrobust `x' X_it if ideol_incum != 2, all p(1) bwselect(mserd) covs(`social_controls')
	local bw=round(`e(h_l)',0.001)
	local obs =`e(N_h_l)'+`e(N_h_r)'
	outreg2 using table_`i'a.tex,  tex(frag) ctitle(" ") nonotes addstat(Observations, `obs', Bandwidth, `bw', (Local) polynomial order, e(p)) afmt(fc) append noobs addtext(Geographic Controls, NO, Social Economic Controls, YES)
	
	* Linear polynomials with all covariates
	rdrobust `x' X_it if ideol_incum != 2, all p(1) bwselect(mserd) covs(`social_controls' `geo_controls')
	local bw=round(`e(h_l)',0.001)
	local obs =`e(N_h_l)'+`e(N_h_r)'
	outreg2 using table_`i'a.tex,  tex(frag) ctitle(" ") nonotes addstat(Observations, `obs', Bandwidth, `bw', (Local) polynomial order, e(p)) afmt(fc) append noobs addtext(Geographic Controls, YES, Social Economic Controls, YES)
	
	* Quadratic polynomials
	rdrobust `x' X_it if ideol_incum != 2, all p(2) bwselect(mserd) 
	local bw=round(`e(h_l)',0.001)
	local bw_quad = round(`e(h_l)',0.001)
	display `bw'
	local poly2 = `e(p)'
	local obs =`e(N_h_l)'+`e(N_h_r)'
	outreg2 using table_`i'a.tex, tex(frag) ctitle(" ") nonotes  addstat(Observations, `obs', Bandwidth, `bw', (Local) polynomial order, e(p)) afmt(fc) append label noobs addtext(Geographic Controls, NO, Social Economic Controls, NO)
	
	* Quadratic polynomials but with geographical covariates
	rdrobust `x' X_it if ideol_incum != 2, all p(2)  bwselect(mserd) covs(`geo_controls')
	local bw=round(`e(h_l)',0.001)
	local obs =`e(N_h_l)'+`e(N_h_r)'
	outreg2 using table_`i'a.tex,  tex(frag) ctitle(" ")  nonotes addstat(Observations, `obs', Bandwidth, `bw', (Local) polynomial order, e(p)) afmt(fc) append noobs addtext(Geographic Controls, YES, Social Economic Controls, NO)
	
	* Quadratic polynomials with socio-economic covariates
	rdrobust `x' X_it if ideol_incum != 2, all p(2) bwselect(mserd) covs(`social_controls')
	local bw=round(`e(h_l)',0.001)
	local obs =`e(N_h_l)'+`e(N_h_r)'
	outreg2 using table_`i'a.tex,  tex(frag) ctitle(" ")  nonotes addstat(Observations, `obs', Bandwidth, `bw', (Local) polynomial order, e(p)) afmt(fc) append noobs addtext(Geographic Controls, NO, Social Economic Controls, YES)
	
	* Quadratic polynomials with all covariates
	rdrobust `x' X_it if ideol_incum != 2, all p(2) bwselect(mserd) covs(`social_controls' `geo_controls')
	local bw=round(`e(h_l)',0.001)
	local obs =`e(N_h_l)'+`e(N_h_r)'
	outreg2 using table_`i'a.tex,  tex(frag) ctitle(" ")  nonotes addstat(Observations, `obs', Bandwidth, `bw', (Local) polynomial order, e(p)) afmt(fc) append noobs addtext(Geographic Controls, YES, Social Economic Controls, YES)
	
		
}

drop u bandw tweight



 



