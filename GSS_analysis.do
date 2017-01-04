********************************************************************************
* GSS Analysis
********************************************************************************

clear all

set more off, permanently 
set maxvar 6000
cd "/Users/carolynstein/Dropbox (MIT)/Research/Sisters"

use "/Users/carolynstein/Dropbox (MIT)/Research/Sisters/Data/GSS/GSS7214_R6b.dta" 

* SAMPLE RESTRICTIONS **********************************************************

* SAMPLE: men who grew up in a family with one sister (either older or younger)

// men
keep if sex == 1

// 1 sibling
keep if sibs == 1

// keep if gender of sibling is known
keep if sbsex1 == 1 | sbsex1 == 2

// keep if birth year of sibling is known
drop if sbyrbrn1 > 2000

// dummy for sister
gen sister = (sbsex1 == 2)

// dummy for whether sister is older or younger
gen age_gap = cohort - sbyrbrn1
gen birth_order = 0
replace birth_order = 1 if age_gap >= 0
replace birth_order = 2 if age_gap < 0
assert birth_order != 0
gen older_sibling = (birth_order == 2)
gen older_sister = (sister == 1 & birth_order == 2)
gen younger_sister = (sister == 1 & birth_order == 1)

* SUMMARY STATISTICS ***********************************************************

// clean up covariates
gen working_ft = (wrkstat == 1)
gen working_pt = (wrkstat == 2)
gen not_working = (wrkstat != 1 & wrkstat != 2)

gen married = (marital == 1)
gen div_sep = (marital == 3 | marital == 4)
gen never_marr = (marital == 5)

gen white = (race == 1)
gen black = (race == 2)
drop other
gen other = (race ==3)

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

// list the variables I want to summarize
local sum_stats age black white other new_engl mid_atl e_nor_cent w_nor_cent sou_atl e_sou_central w_sou_central mtn pacific educ working_ft working_pt not_working married div_sep never_marr

// samples
gen all = 1
local samples all older_sibling sister older_sister

// table layout parameters
local n_cols = 5
local n_rows = 2

// initialize blank matrix
matrix sum_stats = J(`n_rows', `n_cols', 0)

// loop over covariates, which vary with rows
foreach var of local sum_stats {

	// initialize blank row
	matrix row = J(`n_rows', `n_cols', 0)
	
	// loop over samples, which vary with columns
	local col = 1
	foreach s of local samples {
	
		// store control mean and sample size
		sum `var' if `s' 
		matrix row[1, `col'] = r(mean)
		matrix row[2, `col'] = r(sd)
		
		// increment column
		local ++col

	} // end of sample loop (completes a row)
	
	// append row to base matrix
	matrix sum_stats = sum_stats \ row
}

// add sample counts to bottow row
matrix N = J(1, `n_cols', 0)

// loop over samples
local col = 1
foreach s of local samples {
	count if `s'
	matrix N[1, `col'] = r(N)
	local ++col
}

matrix sum_stats = sum_stats \ N

// save matrix
preserve
clear
svmat sum_stats

// label rows
drop if _n == 1
gen stat = ""
order stat
local row = 1
foreach var of local sum_stats {
	replace stat = "`var'" if (_n == `row')
	local row = `row' + 2
}

// label cols
rename sum_stats1 full_sample
rename sum_stats2 sisters
rename sum_stats3 older_sisters

// export file
export excel "/Users/carolynstein/Dropbox (MIT)/Research/Sisters/Results/tables_raw.xlsx", sheet(GSS_summary_stats) sheetreplace
restore


* SPOUSE WORK & EDUCATION REGRESSIONS ******************************************

// clean variables

// code work full-time as 2, part-time as 1, not work as 0
rename spwrksta spwrksta1
gen spwrksta = .
replace spwrksta = 2 if spwrksta1 == 1
replace spwrksta = 1 if spwrksta1 == 2
replace spwrksta = 0 if spwrksta1 >=3 & marital == 1
drop spwrksta1

// code wives who don't work as 0 hours, not IAP
replace sphrs1 = 0 if sphrs1 == .i & marital == 1

// generate an age squared variable
drop age2
gen age2 = age*age

// generate a spouse income variable
gen spinc = .
replace spinc = 1 - rincome/income if marital == 1

// outcomes
local spouse_outcomes spwrksta sphrs1 speduc spdeg

// covariates
local covs age age2 i.race i.reg16 educ

// table layout parameters
local n_rows = 12
local n_cols = 1

// initialize results matrix
matrix spouse_outcomes = J(`n_rows', `n_cols', 0)

// loop over outcomes
foreach y of local spouse_outcomes {

	// initialize a blank column
	matrix col = J(`n_rows', `n_cols', 0)
	local row = 1
	
	// older brother vs. older sister regression
	reg `y' older_sister `covs' if birth_order == 2, robust
	matrix col[`row', 1] = _b[older_sister]
	local ++row
	matrix col[`row', 1] = _se[older_sister]
	local ++row
	matrix col[`row', 1] = e(r2)
	local ++row
	matrix col[`row', 1] = e(N)
	local ++row
	
	// younger brother vs. younger sister regression
	reg `y' younger_sister `covs' if birth_order == 1, robust
	matrix col[`row', 1] = _b[younger_sister]
	local ++row
	matrix col[`row', 1] = _se[younger_sister]
	local ++row
	matrix col[`row', 1] = e(r2)
	local ++row
	matrix col[`row', 1] = e(N)
	local ++row
	
	// younger sister vs. older sister regression
	reg `y' older_sister `covs' if sister == 1, robust
	matrix col[`row', 1] = _b[older_sister]
	local ++row
	matrix col[`row', 1] = _se[older_sister]
	local ++row
	matrix col[`row', 1] = e(r2)
	local ++row
	matrix col[`row', 1] = e(N)
	local ++row
	
	// append column
	matrix spouse_outcomes = spouse_outcomes, col
}

// save matrix
preserve
clear
svmat spouse_outcomes

// export file
export excel "/Users/carolynstein/Dropbox (MIT)/Research/Sisters/Results/tables_raw.xlsx", sheet(GSS_spouse_outcomes) sheetreplace
restore


* HOUSEHOLD DUTIES REGRESSIONS *************************************************

// clean variables

// code repairs as missing if done by outside person
replace repairs = . if repairs == 6

// outcomes
local hh_outcomes laundry dinner shopfood repairs

// covariates
local covs age age2 i.race i.reg16 educ

// table layout parameters
local n_rows = 12
local n_cols = 1

// initialize results matrix
matrix hh_outcomes = J(`n_rows', `n_cols', 0)

// loop over outcomes
foreach y of local hh_outcomes {

	// initialize a blank column
	matrix col = J(`n_rows', `n_cols', 0)
	local row = 1
	
	// older brother vs. older sister regression
	reg `y' older_sister `covs' if birth_order == 2 & `y' != 7, robust // drop unmarried
	matrix col[`row', 1] = _b[older_sister]
	local ++row
	matrix col[`row', 1] = _se[older_sister]
	local ++row
	matrix col[`row', 1] = e(r2)
	local ++row
	matrix col[`row', 1] = e(N)
	local ++row
	
	// younger brother vs. younger sister regression
	reg `y' younger_sister `covs' if birth_order == 1 & `y' != 7, robust
	matrix col[`row', 1] = _b[younger_sister]
	local ++row
	matrix col[`row', 1] = _se[younger_sister]
	local ++row
	matrix col[`row', 1] = e(r2)
	local ++row
	matrix col[`row', 1] = e(N)
	local ++row
	
	// younger sister vs. older sister regression
	reg `y' older_sister `covs' if sister == 1 & `y' != 7, robust
	matrix col[`row', 1] = _b[older_sister]
	local ++row
	matrix col[`row', 1] = _se[older_sister]
	local ++row
	matrix col[`row', 1] = e(r2)
	local ++row
	matrix col[`row', 1] = e(N)
	local ++row
	
	// append column
	matrix hh_outcomes = hh_outcomes, col
}

// save matrix
preserve
clear
svmat hh_outcomes

// export file
export excel "/Users/carolynstein/Dropbox (MIT)/Research/Sisters/Results/tables_raw.xlsx", sheet(GSS_hh_outcomes) sheetreplace
restore


* ATTITUDES TOWARD WOMEN REGRESSIONS *******************************************

// clean variables - make more feminist answers HIGH and binary variables 0 or 1

// should women work - 0 = disapprove, 1 = approve
rename fework fework1
gen fework = .
replace fework = 0 if fework1 == 2
replace fework = 1 if fework1 == 1

// women not suited for politics - 0 = agree, 1 = disagree
rename fepol fepol1
gen fepol = .
replace fepol = 0 if fepol1 == 1
replace fepol = 1 if fepol1 == 2

// vote for a woman president - 0 = no, 1 = yes
rename fepres fepres1
gen fepres = .
replace fepres = 0 if fepres1 == 2
replace fepres = 1 if fepres1 == 1

// husband and wife should contribute to family income
drop twoincs1
rename twoincs twoincs1
gen twoincs = .
replace twoincs = 1 if twoincs1 == 5
replace twoincs = 2 if twoincs1 == 4
replace twoincs = 3 if twoincs1 == 3
replace twoincs = 4 if twoincs1 == 2
replace twoincs = 5 if twoincs1 == 1

// all others look correct, though scales are different (either 1-4 or 1-5)

// outcomes 
local att_outcomes fework fepol fepres fehelp hubbywk1 mrmom famsuffr twoincs

// covariates
local covs age age2 i.race i.reg16 educ i.marital

// table layout parameters
local n_rows = 12
local n_cols = 1

// initialize results matrix
matrix att_outcomes = J(`n_rows', `n_cols', 0)

// loop over outcomes
foreach y of local att_outcomes {

	// initialize a blank column
	matrix col = J(`n_rows', `n_cols', 0)
	local row = 1
	
	// older brother vs. older sister regression
	reg `y' older_sister `covs' if birth_order == 2, robust
	matrix col[`row', 1] = _b[older_sister]
	local ++row
	matrix col[`row', 1] = _se[older_sister]
	local ++row
	matrix col[`row', 1] = e(r2)
	local ++row
	matrix col[`row', 1] = e(N)
	local ++row
	
	// younger brother vs. younger sister regression
	reg `y' younger_sister `covs' if birth_order == 1, robust
	matrix col[`row', 1] = _b[younger_sister]
	local ++row
	matrix col[`row', 1] = _se[younger_sister]
	local ++row
	matrix col[`row', 1] = e(r2)
	local ++row
	matrix col[`row', 1] = e(N)
	local ++row
	
	// younger sister vs. older sister regression
	reg `y' older_sister `covs' if sister == 1, robust
	matrix col[`row', 1] = _b[older_sister]
	local ++row
	matrix col[`row', 1] = _se[older_sister]
	local ++row
	matrix col[`row', 1] = e(r2)
	local ++row
	matrix col[`row', 1] = e(N)
	local ++row
	
	// append column
	matrix att_outcomes = att_outcomes, col
}

// save matrix
preserve
clear
svmat att_outcomes

// export file
export excel "/Users/carolynstein/Dropbox (MIT)/Research/Sisters/Results/tables_raw.xlsx", sheet(GSS_att_outcomes) sheetreplace
restore


