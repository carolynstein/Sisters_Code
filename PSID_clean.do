********************************************************************************
* PSID Clean
********************************************************************************

clear all

set more off, permanently 
set maxvar 6000
cd "/Users/carolynstein/Dropbox (MIT)/Research/Sisters/Data/PSID/"

* CLEAN INDIVIDUAL DATA *****************************************************************

use "PSID_individual_raw.dta" 

// ID variables
gen parent_ID = ER30001*1000 + ER30002
gen ID = ER30001*1000 + ER30002

// where did head and wife grow up
rename ER57651 head_sob
replace head_sob = . if head_sob == 99

rename ER57654 head_s_grewup
replace head_s_grewup = . if head_s_grewup == 99

rename ER58219 head_r_grewup
replace head_r_grewup = . if head_r_grewup == 99

rename ER57541 wife_sob
replace wife_sob = . if wife_sob == 99

rename ER57544 wife_s_grewup
replace wife_s_grewup = . if wife_s_grewup == 99

rename ER58221 wife_r_grewup
replace wife_r_grewup = . if wife_r_grewup == 99

// where did head's and wife's parents grow up
rename ER57509 wife_father_sob
rename ER57510 wife_father_grewup
rename ER57519 wife_mother_sob
rename ER57520 wife_mother_grewup


rename ER57619 head_father_sob
rename ER57620 head_father_grewup
rename ER57629 head_mother_sob
rename ER57630 head_mother_grewup

// education of head's and wife's parents
rename ER57622 head_father_educ
replace head_father_educ = . if head_father_educ == 0 | head_father_educ == 99

rename ER57632 head_mother_educ
replace head_mother_educ = . if head_mother_educ == 0 | head_mother_educ == 99

rename ER57512 wife_father_educ
rename ER57522 wife_mother_educ

// parents poor?
rename ER57656 parents_poor
replace parents_poor = 2 if parents_poor == 3
replace parents_poor = 3 if parents_poor == 5
replace parents_poor = . if parents_poor == 9

// houswork
rename ER53674 wife_hw
rename ER53676 head_hw

// income
rename ER54306 wife_any_income

rename ER58038 head_income
rename ER58050 wife_income

// education
rename ER58223 head_educ
rename ER58224 wife_educ

// wife in family unit?
rename ER54305 wife_in_fu



keep ID wife_* head_* parents_poor

// save
save "PSID_individual_clean.dta", replace

* CLEAN CHILDREN DATA *****************************************************************

use "PSID_children_raw.dta" 

drop if CAH11 == 0

// parent variables
gen parent_ID = CAH3*1000 + CAH4

gen parent_sex = .
replace parent_sex = 1 if CAH5 == 2
replace parent_sex = 0 if CAH5 == 1

rename CAH7 parent_yob
rename CAH6 parent_mob

// child variables
gen ID = CAH10*1000 + CAH11

gen sex = .
replace sex = 1 if CAH12 == 2
replace sex = 0 if CAH12 == 1

rename CAH9 birth_order
rename CAH15 yob
rename CAH13 mob
rename CAH28 race
rename CAH35 multiple
rename CAH36 multiple_type
rename CAH98 wanted_mother
rename CAH100 wanted_father
rename CAH103 last_reported
rename CAH104 num_kids
gen sibs = num_kids - 1
order parent_ID ID num_kids birth_order yob mob
 
// sort
sort parent_ID birth_order
 
// drop entire family if missing a kid's birthday or gender
gen temp = (yob > 2013)
egen yob_missing = max(temp), by(parent_ID)
drop if yob_missing == 1
drop temp yob_missing

gen temp = (sex == .)
egen sex_missing = max(temp), by(parent_ID)
drop if sex_missing == 1
drop temp sex_missing

// drop families missing birth order
gen temp = (birth_order > 18)
egen order_missing = max(temp), by(parent_ID)
drop if order_missing == 1
drop temp order_missing

// drop families with twins
gen temp = (multiple == 1)
egen multiple_birth = max(temp), by(parent_ID)
drop if multiple_birth == 1
drop temp multiple_birth

// make YOB a decimal (ie include month)
replace mob = 1 if mob == 21
replace mob = 4 if mob == 22
replace mob = 7 if mob == 23
replace mob = 10 if mob == 24
replace mob = 6 if mob == 99

gen yob_mob = yob + mob/12


// generate kid order variables
	
	// drop if more than 10 siblings
	drop if num_kids > 10

	// generate blank variables for each kid's sex and YOB
	forval i = 1/10 {
		gen kidsex`i' = .
		gen kidyob`i' = .
	
	}
		
	// for each kid, fill in sex and yob for all kids in the family
	replace kidsex1 = sex if birth_order == 1
	replace kidsex2 = sex[_n+1] if parent_ID[_n] == parent_ID[_n+1] & birth_order == 1
	replace kidsex3 = sex[_n+2] if parent_ID[_n] == parent_ID[_n+2] & birth_order == 1
	replace kidsex4 = sex[_n+3] if parent_ID[_n] == parent_ID[_n+3] & birth_order == 1
	replace kidsex5 = sex[_n+4] if parent_ID[_n] == parent_ID[_n+4] & birth_order == 1
	replace kidsex6 = sex[_n+5] if parent_ID[_n] == parent_ID[_n+5] & birth_order == 1
	replace kidsex7 = sex[_n+6] if parent_ID[_n] == parent_ID[_n+6] & birth_order == 1
	replace kidsex8 = sex[_n+7] if parent_ID[_n] == parent_ID[_n+7] & birth_order == 1
	replace kidsex9 = sex[_n+8] if parent_ID[_n] == parent_ID[_n+8] & birth_order == 1
	replace kidsex10 = sex[_n+9] if parent_ID[_n] == parent_ID[_n+9] & birth_order == 1
	
	replace kidyob1 = yob_mob if birth_order == 1
	replace kidyob2 = yob_mob[_n+1] if parent_ID[_n] == parent_ID[_n+1] & birth_order == 1
	replace kidyob3 = yob_mob[_n+2] if parent_ID[_n] == parent_ID[_n+2] & birth_order == 1
	replace kidyob4 = yob_mob[_n+3] if parent_ID[_n] == parent_ID[_n+3] & birth_order == 1
	replace kidyob5 = yob_mob[_n+4] if parent_ID[_n] == parent_ID[_n+4] & birth_order == 1
	replace kidyob6 = yob_mob[_n+5] if parent_ID[_n] == parent_ID[_n+5] & birth_order == 1
	replace kidyob7 = yob_mob[_n+6] if parent_ID[_n] == parent_ID[_n+6] & birth_order == 1
	replace kidyob8 = yob_mob[_n+7] if parent_ID[_n] == parent_ID[_n+7] & birth_order == 1
	replace kidyob9 = yob_mob[_n+8] if parent_ID[_n] == parent_ID[_n+8] & birth_order == 1
	replace kidyob10 = yob_mob[_n+9] if parent_ID[_n] == parent_ID[_n+9] & birth_order == 1
	
	forval i = 1/10 {
		egen kdsex`i' = min(kidsex`i'), by(parent_ID)
		egen kdyrbrn`i' = min(kidyob`i'), by(parent_ID)
	}
	
	drop kidsex*
	drop kidyob*
	
// generate sibling order variables

	// generate blank variables for sibling sex and YOB
	forval i = 1/9 {
		gen sbsex`i' = .
		gen sbyrbrn`i' = .
	}
	
	// generate sibling variables
	forval i = 1/9 {
		local j = `i'+1
		replace sbsex`i' = kdsex`i' if birth_order > `i' & sibs >= `i'
		replace sbsex`i' = kdsex`j' if birth_order <= `i' & sibs >= `i'
		replace sbyrbrn`i' = kdyrbrn`i' if birth_order > `i' & sibs >= `i'
		replace sbyrbrn`i' = kdyrbrn`j' if birth_order <= `i' & sibs >= `i'
	}
	
	
// sibling variables

	// generate the age gap - positive age gap = older	
	forval i = 1/9 {
		gen age_gap`i' = sbyrbrn`i' - yob_mob
	}
	
	// sibling type (1 = older sister, 2 = younger sister, 3 = older brother, 4 = younger brother)
	forval i = 1/9 {
		gen sbtype`i' = .
		replace sbtype`i' = 0 if sbsex`i' != . & age_gap`i' != .
		replace sbtype`i' = 1 if sbsex`i' == 1 & age_gap`i' < 0
		replace sbtype`i' = 2 if sbsex`i' == 1 & age_gap`i' >= 0
		replace sbtype`i' = 3 if sbsex`i' == 0 & age_gap`i' < 0
		replace sbtype`i' = 4 if sbsex`i' == 0 & age_gap`i' >= 0
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
	
	// only sisters variable
	gen girl_dominated = (brothers == 0) & sibs >=2 & older_sisters >= 1
	
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
			replace next_sib = 1 if sbsex`i' == 1
			replace next_sib = 0 if sbsex`i' == 0
		}
	}
	
// sort
sort parent_ID birth_order

// drop duplicates (kids often show up twice, attached to both parents)
duplicates drop ID, force
	
* MERGE INDIVIDUAL DATA *****************************************************************

merge 1:1 ID using "PSID_individual_clean.dta"

// sort
sort parent_ID birth_order

// keep matches
keep if _merge == 3
drop _merge


* CLEAN OUTCOMES *****************************************************************

// incomes
replace wife_income = 1 if wife_income == 0
replace head_income = 1 if head_income == 0
gen wife_frac_income = wife_income / (wife_income + head_income)

// any income
replace wife_any_income = . if wife_any_income == 9
replace wife_any_income = 0 if wife_any_income == 5

// education
replace wife_educ = . if wife_educ == 99
replace head_educ = . if head_educ == 99
gen educ_diff = wife_educ - head_educ

// housework
replace wife_hw = . if wife_hw == 998 | wife_hw == 999
replace wife_hw = 0.1 if wife_hw == 0
replace head_hw = . if head_hw == 998 | head_hw == 999 
replace head_hw = 0.1 if head_hw == 0
gen head_frac_hw = head_hw / (head_hw + wife_hw)

save "PSID_clean.dta", replace

erase "PSID_individual_clean.dta"







