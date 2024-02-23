// Load previous elections 2019. 
cd "D:\Documents\GitHub\Thesis\Data\Elections\Alcaldia"
use "2019_alcaldia", replace

// Select top two candidates only. 
bysort codmpio: egen rank_cand = rank(-votos)
keep if rank_cand <= 2.5
gen prop_votes = votos/censoe_total

// Left-wing right-wing variable.
