// Baseline results. 
cd "D:\Documents\GitHub\Thesis\Data\Final_data"
use "baseline_data", replace


gen right_win = 

// Descriptive Stats

est clear
estpost tabstat  ln_payments terrorismot asalt_pob desplazados_expulsion desplazados_recepcion H_coca homic_vict homic_caso, /// 
c(stat) statistics(count mean sd min max)  

ereturn list // list the stored locals

esttab using "table1.tex", replace ///
   cells("count mean(fmt(%6.2fc)) sd(fmt(%6.2fc)) min max") nonumber ///
   nomtitle nonote noobs gap label  /// 
   collabels("N" "Mean" "SD" "Min" "Max") ///
   title("Table 1 Descriptive Statistics \label{table1stata}")