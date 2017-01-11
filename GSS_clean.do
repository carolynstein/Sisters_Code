********************************************************************************
* GSS Clean
********************************************************************************

clear all

set more off, permanently 
set maxvar 6000
cd "/Users/carolynstein/Dropbox (MIT)/Research/Sisters"

use "/Users/carolynstein/Dropbox (MIT)/Research/Sisters/Data/GSS/GSS7214_R6b.dta" 

* GENERATE SIBLING VARIABLES ***************************************************

// only 1994 data
keep if year == 1994

// keep if birth year of respondant is known
drop if cohort > 2000

// drop anyone with more than 9 siblings (data only covers first 9)
drop if sibs > 9

// for every person who lists X siblings, keep observations where we know age and sex of each sibling
forval i = 1/9 {
	drop if sibs >= `i' & (sbsex`i' == .d | sbsex`i' == .i | sbsex`i' == .n)
	drop if sibs >= `i' & sbyrbrn`i' > 2000
}

// for every person who lists X siblings, make sure they don't then list age and sex of extra siblings
forval i = 1/9 {
	drop if sibs < `i' & sbsex`i' != .i
	drop if sibs < `i' & sbyrbrn`i' != .i
}

// generate birth order variables

	// if births appear out of order, drop
	forval i = 1/8 {
		local j = `i' + 1
		drop if sbyrbrn`i' > sbyrbrn`j'	
	}

	// generate the age gap - positive age gap = older
	forval i = 1/9 {
		gen age_gap`i' = sbyrbrn`i' - cohort
	}

	// check that siblings are now listed in order
	forval i = 1/8 {
		local j = `i' + 1
		assert age_gap`j' >= age_gap`i'
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

// sibling variables
	
	// sibling type (1 = older sister, 2 = younger sister, 3 = older brother, 4 = younger brother)
	forval i = 1/9 {
		gen sbtype`i' = .
		replace sbtype`i' = 0 if sbsex`i' != . & age_gap`i' != .
		replace sbtype`i' = 1 if sbsex`i' == 2 & age_gap`i' < 0
		replace sbtype`i' = 2 if sbsex`i' == 2 & age_gap`i' >= 0
		replace sbtype`i' = 3 if sbsex`i' == 1 & age_gap`i' < 0
		replace sbtype`i' = 4 if sbsex`i' == 1 & age_gap`i' >= 0
		assert sbtype`i' != 0
		assert sbtype`i' != . if sibs >= `i'
	}
	
	// count older sisters
	gen older_sisters = 0
	forval i = 1/9 {
		replace older_sisters = older_sisters + 1 if sbtype`i' == 1
	}
	
	// count younger sisters
	gen younger_sisters = 0
	forval i = 1/9 {
		replace younger_sisters = younger_sisters + 1 if sbtype`i' == 2
	}
	
	// count older brothers
	gen older_brothers = 0
	forval i = 1/9 {
		replace older_brothers = older_brothers + 1 if sbtype`i' == 3
	}	
	
	// count younger brothers
	gen younger_brothers = 0
	forval i = 1/9 {
		replace younger_brothers = younger_brothers + 1 if sbtype`i' == 4
	}
	
	// check
	assert sibs == older_sisters + younger_sisters + older_brothers + younger_brothers
	
	// sisters and brothers
	gen sisters = older_sisters + younger_sisters
	gen brothers = older_brothers + younger_brothers
	
	// any sibling type var
	gen any_older_sister = (older_sisters > 0)
	gen any_younger_sister = (younger_sisters > 0)
	gen any_older_brother = (older_brothers > 0)
	gen any_younger_brother = (younger_brothers > 0)
	
// exact older sibling permutation dummies
	gen sibling_permut = ""
	replace sibling_permut = "no older sibs" if birth_order == 1
	replace sibling_permut = "g" if birth_order == 2 & sbtype1 == 1
	replace sibling_permut = "b" if birth_order == 2 & sbtype1 == 3
	replace sibling_permut = "gg" if birth_order == 3 & sbtype1 == 1 & sbtype2 == 1
	replace sibling_permut = "gb" if birth_order == 3 & sbtype1 == 1 & sbtype2 == 3
	replace sibling_permut = "bb" if birth_order == 3 & sbtype1 == 3 & sbtype2 == 3
	replace sibling_permut = "bg" if birth_order == 3 & sbtype1 == 3 & sbtype2 == 1
	replace sibling_permut = "ggg" if birth_order == 4 & sbtype1 == 1 & sbtype2 == 1 & sbtype3 == 1
	replace sibling_permut = "ggb" if birth_order == 4 & sbtype1 == 1 & sbtype2 == 1 & sbtype3 == 3
	replace sibling_permut = "gbg" if birth_order == 4 & sbtype1 == 1 & sbtype2 == 3 & sbtype3 == 1
	replace sibling_permut = "gbb" if birth_order == 4 & sbtype1 == 1 & sbtype2 == 3 & sbtype3 == 3
	replace sibling_permut = "bbb" if birth_order == 4 & sbtype1 == 3 & sbtype2 == 3 & sbtype3 == 3
	replace sibling_permut = "bbg" if birth_order == 4 & sbtype1 == 3 & sbtype2 == 3 & sbtype3 == 1
	replace sibling_permut = "bgb" if birth_order == 4 & sbtype1 == 3 & sbtype2 == 1 & sbtype3 == 3
	replace sibling_permut = "bgg" if birth_order == 4 & sbtype1 == 3 & sbtype2 == 1 & sbtype3 == 1
	encode sibling_permut, gen(older_sib_permut)
	
	gen next_sib = .
	forval i = 1/9 {
		if birth_order == `i' {
			replace next_sib = 1 if sbsex`i' == 2
			replace next_sib = 0 if sbsex`i' == 1
		}
	}

	

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

// generate an age squared variable
drop age2
gen age2 = age*age

* INDICATORS FOR PROGRESSIVE ***************************************************

gen progressive = (reg16 == 1 | reg16 == 2 | reg16 == 9) & cohort >= 1960

* CLEAN UP SPOUSE OUTCOMES *****************************************************

// code work full-time as 2, part-time as 1, not work as 0
rename spwrksta spwrksta1
gen spwrksta = .
replace spwrksta = 2 if spwrksta1 == 1
replace spwrksta = 1 if spwrksta1 == 2
replace spwrksta = 0 if spwrksta1 >=3 & marital == 1
drop spwrksta1

// code wives who don't work as 0 hours, not IAP
replace sphrs1 = 0 if sphrs1 == .i & marital == 1

// generate a spouse income variable
gen spinc = .
replace spinc = 1 - rincome/income if marital == 1

* CLEAN UP HOUSEHOLD OUTCOMES **************************************************

// code repairs as missing if done by outside person
replace repairs = . if repairs == 6


* CLEAN UP ATTITUTE OUTCOMES ***************************************************

// make more feminist answers HIGH and binary variables 0 or 1

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

save "/Users/carolynstein/Dropbox (MIT)/Research/Sisters/Data/GSS/GSS_clean.dta", replace 
