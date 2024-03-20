*********************************************************************************
* 	Figure 2 and Figure 3									*	
*																				*
*	This do file: 																*
*	- Produces figure 2, which is the McCrary test graphic. 					*
*	- Produces two graphs which are graphical representation of baseline 		*
*	  results, using linear and quadratic polynomials in the estimation. These	*
*	  two plots constitute figure 3.											*
*																				*
*	Creates McCrary.pdf and baseline_RDplot_linear.pdf and 						*
*	baseline_RDplot_quadratic.pdf, being the first the Figure 2 and the last 	*
*	two the plots presented on figure 3.										*
*																				*
*	Runs baseline regression on outcome variables and outputs table 1.																			*
*																				*
*																				*
*	File: 21_Figures_2&3 - Written on Stata 14 MP.								*																	
*********************************************************************************
local geo_controls altura discapital dismdo gcaribe gpacifica gorinoquia gamazonia
local social_controls indrural 


// Baseline results. 
cd "D:\Documents\GitHub\Thesis\Data\Final_data"
use "baseline_data", replace


gen left_lose = 0
replace left_lose = 1 if ideologia_0 == 1

gen other_lose = 0
replace other_lose = 1 if ideologia_0 == 3

gen no_info_lose = 0
replace no_info_lose = 1 if ideologia_0 == 4

gen payment_mill = payments/1000000


gen lid_SD_chg = (lead1_lid_SD - lid_SD)/lid_SD
gen lid_INDEPAZ_chg = (lead1_lid_INDEPAZ -lid_INDEPAZ)/lid_INDEPAZ

gen lid_SD_pop = lid_SD/pobl_tot
gen lid_INDEPAZ_pop = lid_INDEPAZ/pobl_tot
// Table 1. Descriptive Stats 

est clear
estpost tabstat payment_mill  terrorismot desplazados_expulsion desplazados_recepcion left_lose other_lose no_info_lose, ///
 c(stat) statistics(count mean sd min max)  

ereturn list // list the stored locals

esttab using "D:\Documents\GitHub\Thesis\Figures\table1.tex", replace ///
   cells("count mean(fmt(%6.2fc)) sd(fmt(%6.2fc)) min max") nonumber ///
   nomtitle nonote noobs gap label  /// 
   collabels("N" "Mean" "SD" "Min" "Max") ///
   title("Table 1 Descriptive Statistics \label{table1stata}")
   
 cd "D:\Documents\GitHub\Thesis\Figures"
 // Table 2. Baseline results. Investment in armed conflict victims

 
// 2.1 Linear Specification

// All elections
rdrobust V_payment_diff1 X_it, all p(1) h(0.09) covs(altura discapital dismdo indrural pobl_tot gcaribe gpacifica gorinoquia gamazonia)
local bw=round(`e(h_l)',0.001)
local obs =`e(N_h_l)'+`e(N_h_r)'
outreg2 using Table_2a.tex, tex(frag) ctitle("Payment Trend") nonotes addstat(Observations, `obs', Bandwidth, `bw', (Local) polynomial order, e(p)) afmt(fc) append noobs

// Elections with incumbent non-right.
rdrobust V_payment_diff1 X_it if ideol_incum != 2, all p(1) h(0.09) covs(altura discapital dismdo indrural pobl_tot gcaribe gpacifica gorinoquia gamazonia)

// Elections incumbent non-right and security
rdrobust V1_payment_diff1 X_it if ideol_incum != 2, all p(1) h(0.09) covs(altura discapital dismdo indrural pobl_tot gcaribe gpacifica gorinoquia gamazonia)


// 2.2 Quadratic Specification 
rdrobust payment_diff1 X_it, all p(2) h(0.09) covs(altura discapital dismdo indrural pobl_tot gcaribe gpacifica gorinoquia gamazonia)
local bw=round(`e(h_l)',0.001)
local obs =`e(N_h_l)'+`e(N_h_r)'
outreg2 using Table_2a.tex, tex(frag) ctitle(" ") nonotes addstat(Observations, `obs', Bandwidth, `bw', (Local) polynomial order, e(p)) afmt(fc) append noob

// 4. Table 3. McCarry Test

DCdensity X_it, breakpoint(0) generate(Xj Yj r0 fhat se_fhat) 
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
gr_edit .xaxis1.title.text.Arrpush Relative vote share for left party
gr_edit .yaxis1.title.text = {}
gr_edit .yaxis1.title.text.Arrpush Density
* drop not longer needed variables
drop Xj Yj r0 fhat se_fhat
* save graph
graph export "D:\Documents\GitHub\Thesis\Figures\McCrary.pdf", replace

// 4. Effect on social leaders killings and violence. 

// 4.1 Linear Specification
rdrobust lid_SD X_it if ideol_incum != 2, all p(1) h(0.09) covs(altura discapital dismdo indrural pobl_tot gcaribe gpacifica gorinoquia gamazonia)
rdrobust lid_INDEPAZ X_it if ideol_incum != 2, all p(1) h(0.09) covs(altura discapital dismdo indrural pobl_tot gcaribe gpacifica gorinoquia gamazonia)
rdrobust lid_SD_pop X_it if ideol_incum != 2, all p(1) h(0.09) covs(altura discapital dismdo indrural pobl_tot gcaribe gpacifica gorinoquia gamazonia)

rdrobust lead1_lid_SD X_it, all p(1) h(0.09) covs(altura discapital dismdo indrural pobl_tot gcaribe gpacifica gorinoquia gamazonia)
rdrobust lid_SD_pop X_it, all p(2) h(0.09) covs(altura discapital dismdo indrural pobl_tot gcaribe gpacifica gorinoquia gamazonia)


local bw=round(`e(h_l)',0.001)
local obs =`e(N_h_l)'+`e(N_h_r)'
outreg2 using Table_2a.tex, tex(frag) ctitle(" ") nonotes addstat(Observations, `obs', Bandwidth, `bw', (Local) polynomial order, e(p)) afmt(fc) append noobs

// 4.2 Quadratic Specification 
rdrobust payment_diff1 X_it, all p(2) h(0.09) covs(altura discapital dismdo indrural pobl_tot gcaribe gpacifica gorinoquia gamazonia)
local bw=round(`e(h_l)',0.001)
local obs =`e(N_h_l)'+`e(N_h_r)'
outreg2 using Table_2a.tex, tex(frag) ctitle(" ") nonotes addstat(Observations, `obs', Bandwidth, `bw', (Local) polynomial order, e(p)) afmt(fc) append noob

// 5. Graphs RD
rdplot payment_diff1 X_it, ci(95) nbins(5) p(2)
rdplot ln_payments X_it, ci(95) nbins(5) p(2)
rdplot payment_diff2 X_it, ci(95) nbins(5) p(2)


 



