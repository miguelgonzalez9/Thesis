// Previous Elections 2015
cd "D:\Documents\GitHub\Thesis\Data\Elections"
use "Alcaldia\2015_alcaldia", replace

keep if curules == 1
merge m:1 codigo_partido using "CEDE\Microdatos\clasificacion_partidos_v1.dta",  keepusing(ideologia) keep(match master)

keep codmpio codigo_partido ideologia
rename codigo_partido codigo_partido_2015
rename ideologia ideologia_2015

save "Alcaldia/2015_alcaldia_ideol.dta", replace