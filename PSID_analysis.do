********************************************************************************
* PSID Analysis
********************************************************************************

clear all

set more off, permanently 
set maxvar 6000
cd "/Users/carolynstein/Dropbox (MIT)/Research/Sisters/Data/PSID/"

use "PSID_clean.dta"

* SAMPLE RESTRICTIONS **********************************************************

// keep men
keep if sex == 0

// keep if wife currently in family unit
keep if wife_in_fu == 1

// keep if at least 1 sibling
keep if sibs >= 1

* CREATE VARIABLES *************************************************************

// grew up in liberal or conservative state (based on current Gallup poll)
gen head_liberal_state = (head_s_grewup == 50 | head_s_grewup == 25 | head_s_grewup == 9 | head_s_grewup == 36 | head_s_grewup == 34 | head_s_grewup == 24 | head_s_grewup == 41 | head_s_grewup == 6 | head_s_grewup == 53 | head_s_grewup == 15)
gen head_cons_state = (head_s_grewup == 28 | head_s_grewup == 1 | head_s_grewup == 22 | head_s_grewup == 5 | head_s_grewup == 47 | head_s_grewup == 45 | head_s_grewup == 49 | head_s_grewup == 56 | head_s_grewup == 16 | head_s_grewup == 40 )

// create state-income cells, predict income and housework fractions
reg wife_frac_income i.head_s_grewup##i.parents_poor, r
predict income_hat

reg head_frac_hw i.head_s_grewup##i.parents_poor, r
predict hw_hat


* EFFECT OF NEXT SIBLING *******************************************************

// table parameters
local nrows = 12

// COLUMN 1

// regress
reg wife_frac_income next_sib i.older_sib_permut, vce(cluster parent_ID)

// store
matrix col1 = J(`nrows', 1, 0)
local row = 1
matrix col1[`row', 1] = _b[next_sib]
local ++row
matrix col1[`row', 1] = _se[next_sib]
matrix col1[`nrows'-1, 1] = e(r2)
matrix col1[`nrows', 1] = e(N)

// COLUMN 2
reg wife_frac_income next_sib##head_liberal_state i.older_sib_permut, vce(cluster parent_ID)

// generate interaction 
gen next_sib_liberal = next_sib * head_liberal_state

// regress
reg wife_frac_income next_sib head_liberal_state next_sib_liberal i.older_sib_permut, vce(cluster parent_ID)

// store
matrix col2 = J(`nrows', 1, 0)
local row = 1
matrix col2[`row', 1] = _b[next_sib]
local ++row
matrix col2[`row', 1] = _se[next_sib]
local ++row
matrix col2[`row', 1] = _b[head_liberal_state]
local ++row
matrix col2[`row', 1] = _se[head_liberal_state]
local ++row
matrix col2[`row', 1] = _b[next_sib_liberal]
local ++row
matrix col2[`row', 1] = _se[next_sib_liberal]
matrix col2[`nrows'-1, 1] = e(r2)
matrix col2[`nrows', 1] = e(N)

// COLUMN 3

// generate interaction
gen next_sib_income_hat = next_sib * income_hat

// regress
reg wife_frac_income next_sib income_hat next_sib_income_hat i.older_sib_permut, vce(cluster parent_ID)

// store
matrix col3 = J(`nrows', 1, 0)
local row = 1
matrix col3[`row', 1] = _b[next_sib]
local ++row
matrix col3[`row', 1] = _se[next_sib]
local ++row
matrix col3[`row', 1] = 0
local ++row
matrix col3[`row', 1] = 0
local ++row
matrix col3[`row', 1] = 0
local ++row
matrix col3[`row', 1] = 0
local ++row
matrix col3[`row', 1] = _b[income_hat]
local ++row
matrix col3[`row', 1] = _se[income_hat]
local ++row
matrix col3[`row', 1] = _b[next_sib_income_hat]
local ++row
matrix col3[`row', 1] = _se[next_sib_income_hat]
matrix col3[`nrows'-1, 1] = e(r2)
matrix col3[`nrows', 1] = e(N)

// COLUMN 4

// regress
reg head_frac_hw next_sib i.older_sib_permut, vce(cluster parent_ID)

// store
matrix col4 = J(`nrows', 1, 0)
local row = 1
matrix col4[`row', 1] = _b[next_sib]
local ++row
matrix col4[`row', 1] = _se[next_sib]
matrix col4[`nrows'-1, 1] = e(r2)
matrix col4[`nrows', 1] = e(N)

// COLUMN 5
reg head_frac_hw next_sib##head_liberal_state i.older_sib_permut, vce(cluster parent_ID)


// COLUMN 6
reg head_frac_hw next_sib##c.hw_hat i.older_sib_permut, vce(cluster parent_ID)

* EFFECT OF ANY OLDER SISTER ***************************************************

reg wife_frac_income any_older_sister i.sibs i.birth_order, vce(cluster parent_ID)
reg wife_frac_income any_older_sister##head_liberal_state i.older_sib_permut, vce(cluster parent_ID)
reg wife_frac_income any_older_sister##c.income_hat i.sibs i.birth_order, vce(cluster parent_ID)

reg head_frac_hw any_older_sister i.sibs i.birth_order, vce(cluster parent_ID)
reg head_frac_hw any_older_sister##head_liberal_state i.sibs i.birth_order, vce(cluster parent_ID)
reg head_frac_hw any_older_sister##c.hw_hat i.sibs i.birth_order, vce(cluster parent_ID)




