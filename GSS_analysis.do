********************************************************************************
* GSS Analysis
********************************************************************************

clear all

set more off, permanently 
set maxvar 6000
cd "/Users/carolynstein/Dropbox (MIT)/Research/Sisters"

use "/Users/carolynstein/Dropbox (MIT)/Research/Sisters/Data/GSS/GSS_clean.dta" 

* SAMPLE RESTRICTIONS **********************************************************

* SAMPLE: men who grew up in a family with one sister (either older or younger)

// men
keep if sex == 1

// 1 sibling
keep if sibs == 1

* SUMMARY STATISTICS ***********************************************************

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


