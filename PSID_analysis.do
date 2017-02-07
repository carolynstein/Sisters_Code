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
reg wife_frac_income i.head_s_grewup#i.parents_poor, r
predict income_hat

reg head_frac_hw i.head_s_grewup#i.parents_poor, r
predict hw_hat

* GRAPHS ***********************************************************************

cd "/Users/carolynstein/Dropbox (MIT)/Research/Sisters/Results"

hist wife_frac_income, width(0.05) xtitle("Wife's Fraction of Total Income") scheme(s2mono)
graph export "PSID_frac_income.pdf", replace

hist head_frac_hw, width(0.05) xtitle("Husband's Fraction of Total Housework") scheme(s2mono)
graph export "PSID_frac_hw.pdf", replace


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

// regress
reg head_frac_hw next_sib head_liberal_state next_sib_liberal i.older_sib_permut, vce(cluster parent_ID)

// store
matrix col5 = J(`nrows', 1, 0)
local row = 1
matrix col5[`row', 1] = _b[next_sib]
local ++row
matrix col5[`row', 1] = _se[next_sib]
local ++row
matrix col5[`row', 1] = _b[head_liberal_state]
local ++row
matrix col5[`row', 1] = _se[head_liberal_state]
local ++row
matrix col5[`row', 1] = _b[next_sib_liberal]
local ++row
matrix col5[`row', 1] = _se[next_sib_liberal]
matrix col5[`nrows'-1, 1] = e(r2)
matrix col5[`nrows', 1] = e(N)


// COLUMN 6

// generate interaction
gen next_sib_hw_hat = next_sib * hw_hat

// regress
reg head_frac_hw next_sib hw_hat next_sib_hw_hat i.older_sib_permut, vce(cluster parent_ID)

// store
matrix col6 = J(`nrows', 1, 0)
local row = 1
matrix col6[`row', 1] = _b[next_sib]
local ++row
matrix col6[`row', 1] = _se[next_sib]
local ++row
matrix col6[`row', 1] = 0
local ++row
matrix col6[`row', 1] = 0
local ++row
matrix col6[`row', 1] = 0
local ++row
matrix col6[`row', 1] = 0
local ++row
matrix col6[`row', 1] = _b[hw_hat]
local ++row
matrix col6[`row', 1] = _se[hw_hat]
local ++row
matrix col6[`row', 1] = _b[next_sib_hw_hat]
local ++row
matrix col6[`row', 1] = _se[next_sib_hw_hat]
matrix col6[`nrows'-1, 1] = e(r2)
matrix col6[`nrows', 1] = e(N)

// MAKE TABLE and SAVE

// append columns
matrix PSID_next_sib = col1, col2, col3, col4, col5, col6

// save matrix
preserve
clear
svmat PSID_next_sib

// export file
export excel "/Users/carolynstein/Dropbox (MIT)/Research/Sisters/Results/tables_raw.xlsx", sheet(PSID_next_sib) sheetreplace
restore

* EFFECT OF ANY OLDER SISTER ***************************************************

// table parameters
local nrows = 12

// COLUMN 1

// regress
reg wife_frac_income any_older_sister i.sibs#i.birth_order, vce(cluster parent_ID)

// store
matrix col1 = J(`nrows', 1, 0)
local row = 1
matrix col1[`row', 1] = _b[any_older_sister]
local ++row
matrix col1[`row', 1] = _se[any_older_sister]
matrix col1[`nrows'-1, 1] = e(r2)
matrix col1[`nrows', 1] = e(N)

// COLUMN 2

// generate interaction 
gen older_sister_liberal = any_older_sister * head_liberal_state

// regress
reg wife_frac_income any_older_sister head_liberal_state older_sister_liberal i.sibs#i.birth_order, vce(cluster parent_ID)

// store
matrix col2 = J(`nrows', 1, 0)
local row = 1
matrix col2[`row', 1] = _b[any_older_sister]
local ++row
matrix col2[`row', 1] = _se[any_older_sister]
local ++row
matrix col2[`row', 1] = _b[head_liberal_state]
local ++row
matrix col2[`row', 1] = _se[head_liberal_state]
local ++row
matrix col2[`row', 1] = _b[older_sister_liberal]
local ++row
matrix col2[`row', 1] = _se[older_sister_liberal]
matrix col2[`nrows'-1, 1] = e(r2)
matrix col2[`nrows', 1] = e(N)

// COLUMN 3

// generate interaction
gen older_sister_income_hat = any_older_sister * income_hat

// regress
reg wife_frac_income any_older_sister income_hat older_sister_income_hat i.sibs#i.birth_order, vce(cluster parent_ID)

// store
matrix col3 = J(`nrows', 1, 0)
local row = 1
matrix col3[`row', 1] = _b[any_older_sister]
local ++row
matrix col3[`row', 1] = _se[any_older_sister]
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
matrix col3[`row', 1] = _b[older_sister_income_hat]
local ++row
matrix col3[`row', 1] = _se[older_sister_income_hat]
matrix col3[`nrows'-1, 1] = e(r2)
matrix col3[`nrows', 1] = e(N)

// COLUMN 4

// regress
reg head_frac_hw any_older_sister i.sibs#i.birth_order, vce(cluster parent_ID)

// store
matrix col4 = J(`nrows', 1, 0)
local row = 1
matrix col4[`row', 1] = _b[any_older_sister]
local ++row
matrix col4[`row', 1] = _se[any_older_sister]
matrix col4[`nrows'-1, 1] = e(r2)
matrix col4[`nrows', 1] = e(N)

// COLUMN 5

// regress
reg head_frac_hw any_older_sister head_liberal_state older_sister_liberal i.sibs#i.birth_order, vce(cluster parent_ID)

// store
matrix col5 = J(`nrows', 1, 0)
local row = 1
matrix col5[`row', 1] = _b[any_older_sister]
local ++row
matrix col5[`row', 1] = _se[any_older_sister]
local ++row
matrix col5[`row', 1] = _b[head_liberal_state]
local ++row
matrix col5[`row', 1] = _se[head_liberal_state]
local ++row
matrix col5[`row', 1] = _b[older_sister_liberal]
local ++row
matrix col5[`row', 1] = _se[older_sister_liberal]
matrix col5[`nrows'-1, 1] = e(r2)
matrix col5[`nrows', 1] = e(N)


// COLUMN 6

// generate interaction
gen older_sister_hw_hat = any_older_sister * hw_hat

// regress
reg head_frac_hw any_older_sister hw_hat older_sister_hw_hat i.sibs#i.birth_order, vce(cluster parent_ID)

// store
matrix col6 = J(`nrows', 1, 0)
local row = 1
matrix col6[`row', 1] = _b[any_older_sister]
local ++row
matrix col6[`row', 1] = _se[any_older_sister]
local ++row
matrix col6[`row', 1] = 0
local ++row
matrix col6[`row', 1] = 0
local ++row
matrix col6[`row', 1] = 0
local ++row
matrix col6[`row', 1] = 0
local ++row
matrix col6[`row', 1] = _b[hw_hat]
local ++row
matrix col6[`row', 1] = _se[hw_hat]
local ++row
matrix col6[`row', 1] = _b[older_sister_hw_hat]
local ++row
matrix col6[`row', 1] = _se[older_sister_hw_hat]
matrix col6[`nrows'-1, 1] = e(r2)
matrix col6[`nrows', 1] = e(N)

// MAKE TABLE and SAVE

// append columns
matrix PSID_older_sister = col1, col2, col3, col4, col5, col6

// save matrix
preserve
clear
svmat PSID_older_sister

// export file
export excel "/Users/carolynstein/Dropbox (MIT)/Research/Sisters/Results/tables_raw.xlsx", sheet(PSID_older_sister) sheetreplace
restore

* EFFECT OF OLDER SISTER, USING FAMILY FEs *************************************
set matsize 2000

// count families where there is variation in any older sister and # older sisters
bys parent_ID: gen count = _N
egen sum_any_older_sister = total(any_older_sister), by(parent_ID)
gen var_in_any_older_sister = (sum_any_older_sister != 0 & sum_any_older_sister != count)

egen min_older_sisters = min(older_sisters), by(parent_ID)
egen max_older_sisters = max(older_sisters), by(parent_ID)
gen var_in_older_sisters = (min_older_sisters != max_older_sisters)

xtset parent_ID

// table parameters
local nrows = 5

// COLUMN 1

// regress
reg wife_frac_income any_older_sister i.birth_order i.parent_ID if var_in_any_older_sister == 1, vce(cluster parent_ID)

// store
matrix col1 = J(`nrows', 1, 0)
local row = 1
matrix col1[`row', 1] = _b[any_older_sister]
local ++row
matrix col1[`row', 1] = _se[any_older_sister] 
local ++row
matrix col1[`row', 1] = e(r2)
local ++row
matrix col1[`row', 1] = e(N_clust)
local ++row
matrix col1[`row', 1] = e(N)

// COLUMN 2

// regress
reg wife_frac_income older_sisters i.birth_order i.parent_ID if var_in_older_sisters == 1, vce(cluster parent_ID)

// store
matrix col2 = J(`nrows', 1, 0)
local row = 1
matrix col2[`row', 1] = _b[older_sisters]
local ++row
matrix col2[`row', 1] = _se[older_sisters] 
local ++row
matrix col2[`row', 1] = e(r2)
local ++row
matrix col2[`row', 1] = e(N_clust)
local ++row
matrix col2[`row', 1] = e(N)

// COLUMN 3

// regress
reg head_frac_hw any_older_sister i.birth_order i.parent_ID if var_in_any_older_sister == 1, vce(cluster parent_ID)

// store
matrix col3 = J(`nrows', 1, 0)
local row = 1
matrix col3[`row', 1] = _b[any_older_sister]
local ++row
matrix col3[`row', 1] = _se[any_older_sister] 
local ++row
matrix col3[`row', 1] = e(r2)
local ++row
matrix col3[`row', 1] = e(N_clust)
local ++row
matrix col3[`row', 1] = e(N)

// COLUMN 4

// regress
reg head_frac_hw older_sisters i.birth_order i.parent_ID if var_in_older_sisters == 1, vce(cluster parent_ID)

// store
matrix col4 = J(`nrows', 1, 0)
local row = 1
matrix col4[`row', 1] = _b[older_sisters]
local ++row
matrix col4[`row', 1] = _se[older_sisters] 
local ++row
matrix col4[`row', 1] = e(r2)
local ++row
matrix col4[`row', 1] = e(N_clust)
local ++row
matrix col4[`row', 1] = e(N)

// MAKE TABLE and SAVE

// append columns
matrix PSID_family_fes = col1, col2, col3, col4

// save matrix
preserve
clear
svmat PSID_family_fes

// export file
export excel "/Users/carolynstein/Dropbox (MIT)/Research/Sisters/Results/tables_raw.xlsx", sheet(PSID_family_fes) sheetreplace
restore






