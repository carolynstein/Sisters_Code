********************************************************************************
* GSS Clean
********************************************************************************

clear all

set more off, permanently 
set maxvar 6000
cd "/Users/carolynstein/Dropbox (MIT)/Research/Sisters"

use "/Users/carolynstein/Dropbox (MIT)/Research/Sisters/Data/GSS/GSS7214_R6b.dta" 

* GENERATE SIBLING VARIABLES ***************************************************

// keep if gender of sibling is known
keep if sbsex1 == 1 | sbsex1 == 2

// keep if birth year of sibling is known
drop if sbyrbrn1 > 2000

// keep if birth year of respondant is known
drop if cohort > 2000

// birth order
forval i = 1/9 {
	gen age_gap`i' = sbyrbrn`i' - cohort
}

gen birth_order = 0
replace birth_order = 1 if age_gap1 >= 0
replace birth_order = 2 if age_gap1 < 0 & age_gap2 >= 0
replace birth_order = 3 if age_gap2 < 0 & age_gap3 >= 0
replace birth_order = 4 if age_gap3 < 0 & age_gap4 >= 0
replace birth_order = 5 if age_gap4 < 0 & age_gap5 >= 0
replace birth_order = 6 if age_gap5 < 0 & age_gap6 >= 0
replace birth_order = 7 if age_gap6 < 0 & age_gap7 >= 0
replace birth_order = 8 if age_gap7 < 0 & age_gap8 >= 0
replace birth_order = 9 if age_gap8 < 0 & age_gap9 >= 0
replace birth_order = 10 if age_gap9 < 0
assert birth_order != 0

// sibling dummies
gen older_sibling = (birth_order >= 2)
gen sister = (sbsex1 == 2 | sbsex2 == 2 | sbsex3 == 2 | sbsex4 == 2 | sbsex5 == 2 | sbsex6 == 2 | sbsex7 == 2 | sbsex8 == 2 | sbsex9 == 2)
gen older_sister = (sister == 1 & birth_order == 2)
gen younger_sister = (sister == 1 & birth_order == 1)

* CLEAN UP COVARIATES **********************************************************

// work status
gen working_ft = (wrkstat == 1)
gen working_pt = (wrkstat == 2)
gen not_working = (wrkstat != 1 & wrkstat != 2)

// marital status
gen married = (marital == 1)
gen div_sep = (marital == 3 | marital == 4)
gen never_marr = (marital == 5)

// race
gen white = (race == 1)
gen black = (race == 2)
drop other
gen other = (race ==3)

// region living at age 16
gen foreign = (reg16 == 0)
gen new_engl = (reg16 == 1)
gen mid_atl = (reg16 == 2)
gen e_nor_cent = (reg16 == 3)
gen w_nor_cent = (reg16 == 4)
gen sou_atl = (reg16 == 5)
gen e_sou_central = (reg16 == 6)
gen w_sou_central = (reg16 == 7)
gen mtn = (reg16 == 8)
gen pacific = (reg16 == 9)
