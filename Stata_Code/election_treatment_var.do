// Load previous elections 2019. 
cd "D:\Documents\GitHub\Thesis\Data\Elections"
use "Alcaldia\2019_alcaldia", replace

// Select top two candidates only. 
bysort codmpio: egen rank_cand = rank(-votos)
keep if rank_cand <= 2.5
gen prop_votes = votos/censoe_total
// Merge with political party dataset. 

merge m:1 codigo_partido using "CEDE\Microdatos\clasificacion_partidos_v1.dta", keep(match  master) keepusing(tradicional partidos_coalicion temporalidad ideologia paso_ideologia grupo_representativo_1 grupo_representativo_2 part_*)

merge m:1 codmpio using "Alcaldia/2015_alcaldia_ideol.dta", keep(match  master) generate(merge_2)

// Drop obs without classification
drop if ideologia == 4

// Treatment variable. RD Design.

// Running variable. Difference in poportion of votes between non-right and right.




