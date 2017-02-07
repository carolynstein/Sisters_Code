********************************************************************************
* GSS Analysis
********************************************************************************

clear all

set more off, permanently 
set maxvar 6000
cd "/Users/carolynstein/Dropbox (MIT)/Research/Sisters"

use "/Users/carolynstein/Dropbox (MIT)/Research/Sisters/Data/GSS/GSS_clean.dta" 

* SAMPLE RESTRICTIONS **********************************************************

// men
keep if sex == 1

// 1 sibling
keep if sibs >= 1

* CREATE VARIABLES *************************************************************

// grew up in liberal or conservative state (based on current Gallup poll)
gen liberal_region = (reg16 == 1 | reg16 == 2 | reg16 == 9)
gen cons_region = (reg16 == 6 | reg16 == 7 | reg16 == 8)

// create state-income cells, predict outcomes of interest

replace incom16 = 6 if incom16 == .a | incom16 == .i

reg spwrksta i.reg16#incom16, r
predict spwrksta_hat

reg sp_frac_hrs i.reg16#incom16, r
predict hrs_hat

reg laundry i.reg16#incom16, r
predict laundry_hat

reg shopfood i.reg16#incom16, r
predict shopfood_hat

reg dinner i.reg16#incom16, r
predict dinner_hat

reg repairs i.reg16#incom16, r
predict repairs_hat

reg fework i.reg16#incom16, r
predict fework_hat

reg fepol i.reg16#incom16, r
predict fepol_hat

reg hubbywk1 i.reg16#incom16, r
predict hubbywk1_hat

reg mrmom i.reg16#incom16, r
predict mrmom_hat

* GRAPHS ***********************************************************************

cd "/Users/carolynstein/Dropbox (MIT)/Research/Sisters/Results"

label define spwrksta 0 "not working" 1 "works part-time" 2 "works full-time"
label val spwrksta spwrksta
hist spwrksta, scheme(s2mono) xlabel(0/2, valuelabel noticks) xtitle("Wife's Employment Status") discrete xscale(range(0 3))
graph export "GSS_spwrksta.pdf", replace

hist sp_frac_hrs, width(0.05) xtitle("Wife's Fraction of Total Work Hours") scheme(s2mono)
graph export "GSS_frac_hours.pdf", replace

label define hw 1 "wife always" 2 "wife usually" 3 "husband or wife" 4 "husband usually" 5 "husband always"
label val laundry hw
hist laundry, scheme(s2mono) xlabel(1/5, valuelabel noticks labsize(small)) xtitle("Who Does Task?") discrete xscale(range(0 6))
graph export "GSS_laundry.pdf", replace

label val shopfood hw
hist shopfood, scheme(s2mono) xlabel(1/5, valuelabel noticks labsize(small)) xtitle("Who Does Task?") discrete xscale(range(0 6))
graph export "GSS_shopfood.pdf", replace

label val dinner hw
hist dinner, scheme(s2mono) xlabel(1/5, valuelabel noticks labsize(small)) xtitle("Who Does Task?") discrete xscale(range(0 6))
graph export "GSS_dinner.pdf", replace

label val repairs hw
hist repairs, scheme(s2mono) xlabel(1/5, valuelabel noticks labsize(small)) xtitle("Who Does Task?") discrete xscale(range(0 6))
graph export "GSS_repairs.pdf", replace

label define approve 1 "disapprove" 2 "not sure" 3 "approve"
label val fework approve
hist fework, scheme(s2mono) xlabel(1/3, valuelabel noticks) xtitle("Approve or Disapprove?") discrete xscale(range(0 4))
graph export "GSS_fework.pdf", replace

label define agree1 1 "agree" 2 "not sure" 3 "disagree"
label val fepol agree1
hist fepol, scheme(s2mono) xlabel(1/3, valuelabel noticks) xtitle("Agree or Disagree?") discrete xscale(range(0 4))
graph export "GSS_fepol.pdf", replace

label define agree2 1 "strongly agree" 2 "agree" 3 "neither" 4 "disagree" 5 "strongly disagree"
label val hubbywk1 agree2
hist hubbywk1, scheme(s2mono) xlabel(1/5, valuelabel noticks labsize(small)) xtitle("Agree or Disagree?") discrete xscale(range(0 6))
graph export "GSS_hubbywk.pdf", replace

label val mrmom agree2
hist mrmom, scheme(s2mono) xlabel(1/5, valuelabel noticks labsize(small)) xtitle("Agree or Disagree?") discrete xscale(range(0 6))
graph export "GSS_mrmom.pdf", replace

* EFFECT OF NEXT SIBLING on WIFE WORK ******************************************

// table parameters
local nrows = 12

// COLUMN 1

// regress
reg spwrksta next_sib i.older_sib_permut, robust

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
gen next_sib_liberal = next_sib * liberal_region

// regress
reg spwrksta next_sib liberal_region next_sib_liberal i.older_sib_permut, robust

// store
matrix col2 = J(`nrows', 1, 0)
local row = 1
matrix col2[`row', 1] = _b[next_sib]
local ++row
matrix col2[`row', 1] = _se[next_sib]
local ++row
matrix col2[`row', 1] = _b[liberal_region]
local ++row
matrix col2[`row', 1] = _se[liberal_region]
local ++row
matrix col2[`row', 1] = _b[next_sib_liberal]
local ++row
matrix col2[`row', 1] = _se[next_sib_liberal]
matrix col2[`nrows'-1, 1] = e(r2)
matrix col2[`nrows', 1] = e(N)

// COLUMN 3

// generate interaction
gen next_sib_spwrksta_hat = next_sib * spwrksta_hat

// regress
reg spwrksta next_sib spwrksta_hat next_sib_spwrksta_hat i.older_sib_permut, robust

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
matrix col3[`row', 1] = _b[spwrksta_hat]
local ++row
matrix col3[`row', 1] = _se[spwrksta_hat]
local ++row
matrix col3[`row', 1] = _b[next_sib_spwrksta_hat]
local ++row
matrix col3[`row', 1] = _se[next_sib_spwrksta_hat]
matrix col3[`nrows'-1, 1] = e(r2)
matrix col3[`nrows', 1] = e(N)

// COLUMN 4

// regress
reg sp_frac_hrs next_sib i.older_sib_permut, robust

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
reg sp_frac_hrs next_sib liberal_region next_sib_liberal i.older_sib_permut, robust

// store
matrix col5 = J(`nrows', 1, 0)
local row = 1
matrix col5[`row', 1] = _b[next_sib]
local ++row
matrix col5[`row', 1] = _se[next_sib]
local ++row
matrix col5[`row', 1] = _b[liberal_region]
local ++row
matrix col5[`row', 1] = _se[liberal_region]
local ++row
matrix col5[`row', 1] = _b[next_sib_liberal]
local ++row
matrix col5[`row', 1] = _se[next_sib_liberal]
matrix col5[`nrows'-1, 1] = e(r2)
matrix col5[`nrows', 1] = e(N)


// COLUMN 6

// generate interaction
gen next_sib_hrs_hat = next_sib * hrs_hat

// regress
reg sp_frac_hrs next_sib hrs_hat next_sib_hrs_hat i.older_sib_permut, robust

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
matrix col6[`row', 1] = _b[hrs_hat]
local ++row
matrix col6[`row', 1] = _se[hrs_hat]
local ++row
matrix col6[`row', 1] = _b[next_sib_hrs_hat]
local ++row
matrix col6[`row', 1] = _se[next_sib_hrs_hat]
matrix col6[`nrows'-1, 1] = e(r2)
matrix col6[`nrows', 1] = e(N)

// MAKE TABLE and SAVE

// append columns
matrix GSS_next_sib1 = col1, col2, col3, col4, col5, col6

// save matrix
preserve
clear
svmat GSS_next_sib1

// export file
export excel "/Users/carolynstein/Dropbox (MIT)/Research/Sisters/Results/tables_raw.xlsx", sheet(GSS_next_sib1) sheetreplace
restore

* EFFECT OF NEXT SIBLING on HOUSEHOLD DUTIES ******************************************

// table parameters
local nrows = 12

// COLUMN 1

// regress
reg laundry next_sib i.older_sib_permut, robust

// store
matrix col1 = J(`nrows', 1, 0)
local row = 1
matrix col1[`row', 1] = _b[next_sib]
local ++row
matrix col1[`row', 1] = _se[next_sib]
matrix col1[`nrows'-1, 1] = e(r2)
matrix col1[`nrows', 1] = e(N)

// COLUMN 2

// regress
reg laundry next_sib liberal_region next_sib_liberal i.older_sib_permut, robust

// store
matrix col2 = J(`nrows', 1, 0)
local row = 1
matrix col2[`row', 1] = _b[next_sib]
local ++row
matrix col2[`row', 1] = _se[next_sib]
local ++row
matrix col2[`row', 1] = _b[liberal_region]
local ++row
matrix col2[`row', 1] = _se[liberal_region]
local ++row
matrix col2[`row', 1] = _b[next_sib_liberal]
local ++row
matrix col2[`row', 1] = _se[next_sib_liberal]
matrix col2[`nrows'-1, 1] = e(r2)
matrix col2[`nrows', 1] = e(N)

// COLUMN 3

// generate interaction
gen next_sib_laundry_hat = next_sib * laundry_hat

// regress
reg laundry next_sib laundry_hat next_sib_laundry_hat i.older_sib_permut, robust

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
matrix col3[`row', 1] = _b[laundry_hat]
local ++row
matrix col3[`row', 1] = _se[laundry_hat]
local ++row
matrix col3[`row', 1] = _b[next_sib_laundry_hat]
local ++row
matrix col3[`row', 1] = _se[next_sib_laundry_hat]
matrix col3[`nrows'-1, 1] = e(r2)
matrix col3[`nrows', 1] = e(N)

// COLUMN 4

// regress
reg shopfood next_sib i.older_sib_permut, robust

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
reg shopfood next_sib liberal_region next_sib_liberal i.older_sib_permut, robust

// store
matrix col5 = J(`nrows', 1, 0)
local row = 1
matrix col5[`row', 1] = _b[next_sib]
local ++row
matrix col5[`row', 1] = _se[next_sib]
local ++row
matrix col5[`row', 1] = _b[liberal_region]
local ++row
matrix col5[`row', 1] = _se[liberal_region]
local ++row
matrix col5[`row', 1] = _b[next_sib_liberal]
local ++row
matrix col5[`row', 1] = _se[next_sib_liberal]
matrix col5[`nrows'-1, 1] = e(r2)
matrix col5[`nrows', 1] = e(N)


// COLUMN 6

// generate interaction
gen next_sib_shopfood_hat = next_sib * shopfood_hat

// regress
reg shopfood next_sib shopfood_hat next_sib_shopfood_hat i.older_sib_permut, robust

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
matrix col6[`row', 1] = _b[shopfood_hat]
local ++row
matrix col6[`row', 1] = _se[shopfood_hat]
local ++row
matrix col6[`row', 1] = _b[next_sib_shopfood_hat]
local ++row
matrix col6[`row', 1] = _se[next_sib_shopfood_hat]
matrix col6[`nrows'-1, 1] = e(r2)
matrix col6[`nrows', 1] = e(N)

// COLUMN 7

// regress
reg dinner next_sib i.older_sib_permut, robust

// store
matrix col7 = J(`nrows', 1, 0)
local row = 1
matrix col7[`row', 1] = _b[next_sib]
local ++row
matrix col7[`row', 1] = _se[next_sib]
matrix col7[`nrows'-1, 1] = e(r2)
matrix col7[`nrows', 1] = e(N)

// COLUMN 8

// regress
reg dinner next_sib liberal_region next_sib_liberal i.older_sib_permut, robust

// store
matrix col8 = J(`nrows', 1, 0)
local row = 1
matrix col8[`row', 1] = _b[next_sib]
local ++row
matrix col8[`row', 1] = _se[next_sib]
local ++row
matrix col8[`row', 1] = _b[liberal_region]
local ++row
matrix col8[`row', 1] = _se[liberal_region]
local ++row
matrix col8[`row', 1] = _b[next_sib_liberal]
local ++row
matrix col8[`row', 1] = _se[next_sib_liberal]
matrix col8[`nrows'-1, 1] = e(r2)
matrix col8[`nrows', 1] = e(N)

// COLUMN 9

// generate interaction
gen next_sib_dinner_hat = next_sib * dinner_hat

// regress
reg dinner next_sib dinner_hat next_sib_dinner_hat i.older_sib_permut, robust

// store
matrix col9 = J(`nrows', 1, 0)
local row = 1
matrix col9[`row', 1] = _b[next_sib]
local ++row
matrix col9[`row', 1] = _se[next_sib]
local ++row
matrix col9[`row', 1] = 0
local ++row
matrix col9[`row', 1] = 0
local ++row
matrix col9[`row', 1] = 0
local ++row
matrix col9[`row', 1] = 0
local ++row
matrix col9[`row', 1] = _b[dinner_hat]
local ++row
matrix col9[`row', 1] = _se[dinner_hat]
local ++row
matrix col9[`row', 1] = _b[next_sib_dinner_hat]
local ++row
matrix col9[`row', 1] = _se[next_sib_dinner_hat]
matrix col9[`nrows'-1, 1] = e(r2)
matrix col9[`nrows', 1] = e(N)

// COLUMN 10

// regress
reg repairs next_sib i.older_sib_permut, robust

// store
matrix col10 = J(`nrows', 1, 0)
local row = 1
matrix col10[`row', 1] = _b[next_sib]
local ++row
matrix col10[`row', 1] = _se[next_sib]
matrix col10[`nrows'-1, 1] = e(r2)
matrix col10[`nrows', 1] = e(N)

// COLUMN 11

// regress
reg repairs next_sib liberal_region next_sib_liberal i.older_sib_permut, robust

// store
matrix col11 = J(`nrows', 1, 0)
local row = 1
matrix col11[`row', 1] = _b[next_sib]
local ++row
matrix col11[`row', 1] = _se[next_sib]
local ++row
matrix col11[`row', 1] = _b[liberal_region]
local ++row
matrix col11[`row', 1] = _se[liberal_region]
local ++row
matrix col11[`row', 1] = _b[next_sib_liberal]
local ++row
matrix col11[`row', 1] = _se[next_sib_liberal]
matrix col11[`nrows'-1, 1] = e(r2)
matrix col11[`nrows', 1] = e(N)

// COLUMN 12

// generate interaction
gen next_sib_repairs_hat = next_sib * repairs_hat

// regress
reg repairs next_sib repairs_hat next_sib_repairs_hat i.older_sib_permut, robust

// store
matrix col12 = J(`nrows', 1, 0)
local row = 1
matrix col12[`row', 1] = _b[next_sib]
local ++row
matrix col12[`row', 1] = _se[next_sib]
local ++row
matrix col12[`row', 1] = 0
local ++row
matrix col12[`row', 1] = 0
local ++row
matrix col12[`row', 1] = 0
local ++row
matrix col12[`row', 1] = 0
local ++row
matrix col12[`row', 1] = _b[repairs_hat]
local ++row
matrix col12[`row', 1] = _se[repairs_hat]
local ++row
matrix col12[`row', 1] = _b[next_sib_repairs_hat]
local ++row
matrix col12[`row', 1] = _se[next_sib_repairs_hat]
matrix col12[`nrows'-1, 1] = e(r2)
matrix col12[`nrows', 1] = e(N)

// MAKE TABLE and SAVE

// append columns
matrix GSS_next_sib2 = col1, col2, col3, col4, col5, col6, col7, col8, col9, col10, col11, col12

// save matrix
preserve
clear
svmat GSS_next_sib2

// export file
export excel "/Users/carolynstein/Dropbox (MIT)/Research/Sisters/Results/tables_raw.xlsx", sheet(GSS_next_sib2) sheetreplace
restore


* EFFECT OF NEXT SIBLING on ATTITUDES ******************************************

// table parameters
local nrows = 12

// COLUMN 1

// regress
reg fework next_sib i.older_sib_permut, robust

// store
matrix col1 = J(`nrows', 1, 0)
local row = 1
matrix col1[`row', 1] = _b[next_sib]
local ++row
matrix col1[`row', 1] = _se[next_sib]
matrix col1[`nrows'-1, 1] = e(r2)
matrix col1[`nrows', 1] = e(N)

// COLUMN 2

// regress
reg fework next_sib liberal_region next_sib_liberal i.older_sib_permut, robust

// store
matrix col2 = J(`nrows', 1, 0)
local row = 1
matrix col2[`row', 1] = _b[next_sib]
local ++row
matrix col2[`row', 1] = _se[next_sib]
local ++row
matrix col2[`row', 1] = _b[liberal_region]
local ++row
matrix col2[`row', 1] = _se[liberal_region]
local ++row
matrix col2[`row', 1] = _b[next_sib_liberal]
local ++row
matrix col2[`row', 1] = _se[next_sib_liberal]
matrix col2[`nrows'-1, 1] = e(r2)
matrix col2[`nrows', 1] = e(N)

// COLUMN 3

// generate interaction
gen next_sib_fework_hat = next_sib * fework_hat

// regress
reg fework next_sib fework_hat next_sib_fework_hat i.older_sib_permut, robust

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
matrix col3[`row', 1] = _b[fework_hat]
local ++row
matrix col3[`row', 1] = _se[fework_hat]
local ++row
matrix col3[`row', 1] = _b[next_sib_fework_hat]
local ++row
matrix col3[`row', 1] = _se[next_sib_fework_hat]
matrix col3[`nrows'-1, 1] = e(r2)
matrix col3[`nrows', 1] = e(N)

// COLUMN 4

// regress
reg fepol next_sib i.older_sib_permut, robust

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
reg fepol next_sib liberal_region next_sib_liberal i.older_sib_permut, robust

// store
matrix col5 = J(`nrows', 1, 0)
local row = 1
matrix col5[`row', 1] = _b[next_sib]
local ++row
matrix col5[`row', 1] = _se[next_sib]
local ++row
matrix col5[`row', 1] = _b[liberal_region]
local ++row
matrix col5[`row', 1] = _se[liberal_region]
local ++row
matrix col5[`row', 1] = _b[next_sib_liberal]
local ++row
matrix col5[`row', 1] = _se[next_sib_liberal]
matrix col5[`nrows'-1, 1] = e(r2)
matrix col5[`nrows', 1] = e(N)


// COLUMN 6

// generate interaction
gen next_sib_fepol_hat = next_sib * fepol_hat

// regress
reg fepol next_sib fepol_hat next_sib_fepol_hat i.older_sib_permut, robust

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
matrix col6[`row', 1] = _b[fepol_hat]
local ++row
matrix col6[`row', 1] = _se[fepol_hat]
local ++row
matrix col6[`row', 1] = _b[next_sib_fepol_hat]
local ++row
matrix col6[`row', 1] = _se[next_sib_fepol_hat]
matrix col6[`nrows'-1, 1] = e(r2)
matrix col6[`nrows', 1] = e(N)

// COLUMN 7

// regress
reg hubbywk1 next_sib i.older_sib_permut, robust

// store
matrix col7 = J(`nrows', 1, 0)
local row = 1
matrix col7[`row', 1] = _b[next_sib]
local ++row
matrix col7[`row', 1] = _se[next_sib]
matrix col7[`nrows'-1, 1] = e(r2)
matrix col7[`nrows', 1] = e(N)

// COLUMN 8

// regress
reg hubbywk1 next_sib liberal_region next_sib_liberal i.older_sib_permut, robust

// store
matrix col8 = J(`nrows', 1, 0)
local row = 1
matrix col8[`row', 1] = _b[next_sib]
local ++row
matrix col8[`row', 1] = _se[next_sib]
local ++row
matrix col8[`row', 1] = _b[liberal_region]
local ++row
matrix col8[`row', 1] = _se[liberal_region]
local ++row
matrix col8[`row', 1] = _b[next_sib_liberal]
local ++row
matrix col8[`row', 1] = _se[next_sib_liberal]
matrix col8[`nrows'-1, 1] = e(r2)
matrix col8[`nrows', 1] = e(N)

// COLUMN 9

// generate interaction
gen next_sib_hubbywk1_hat = next_sib * hubbywk1_hat

// regress
reg hubbywk1 next_sib hubbywk1_hat next_sib_hubbywk1_hat i.older_sib_permut, robust

// store
matrix col9 = J(`nrows', 1, 0)
local row = 1
matrix col9[`row', 1] = _b[next_sib]
local ++row
matrix col9[`row', 1] = _se[next_sib]
local ++row
matrix col9[`row', 1] = 0
local ++row
matrix col9[`row', 1] = 0
local ++row
matrix col9[`row', 1] = 0
local ++row
matrix col9[`row', 1] = 0
local ++row
matrix col9[`row', 1] = _b[hubbywk1_hat]
local ++row
matrix col9[`row', 1] = _se[hubbywk1_hat]
local ++row
matrix col9[`row', 1] = _b[next_sib_hubbywk1_hat]
local ++row
matrix col9[`row', 1] = _se[next_sib_hubbywk1_hat]
matrix col9[`nrows'-1, 1] = e(r2)
matrix col9[`nrows', 1] = e(N)

// COLUMN 10

// regress
reg mrmom next_sib i.older_sib_permut, robust

// store
matrix col10 = J(`nrows', 1, 0)
local row = 1
matrix col10[`row', 1] = _b[next_sib]
local ++row
matrix col10[`row', 1] = _se[next_sib]
matrix col10[`nrows'-1, 1] = e(r2)
matrix col10[`nrows', 1] = e(N)

// COLUMN 11

// regress
reg mrmom next_sib liberal_region next_sib_liberal i.older_sib_permut, robust

// store
matrix col11 = J(`nrows', 1, 0)
local row = 1
matrix col11[`row', 1] = _b[next_sib]
local ++row
matrix col11[`row', 1] = _se[next_sib]
local ++row
matrix col11[`row', 1] = _b[liberal_region]
local ++row
matrix col11[`row', 1] = _se[liberal_region]
local ++row
matrix col11[`row', 1] = _b[next_sib_liberal]
local ++row
matrix col11[`row', 1] = _se[next_sib_liberal]
matrix col11[`nrows'-1, 1] = e(r2)
matrix col11[`nrows', 1] = e(N)

// COLUMN 12

// generate interaction
gen next_sib_mrmom_hat = next_sib * mrmom_hat

// regress
reg mrmom next_sib mrmom_hat next_sib_mrmom_hat i.older_sib_permut, robust

// store
matrix col12 = J(`nrows', 1, 0)
local row = 1
matrix col12[`row', 1] = _b[next_sib]
local ++row
matrix col12[`row', 1] = _se[next_sib]
local ++row
matrix col12[`row', 1] = 0
local ++row
matrix col12[`row', 1] = 0
local ++row
matrix col12[`row', 1] = 0
local ++row
matrix col12[`row', 1] = 0
local ++row
matrix col12[`row', 1] = _b[mrmom_hat]
local ++row
matrix col12[`row', 1] = _se[mrmom_hat]
local ++row
matrix col12[`row', 1] = _b[next_sib_mrmom_hat]
local ++row
matrix col12[`row', 1] = _se[next_sib_mrmom_hat]
matrix col12[`nrows'-1, 1] = e(r2)
matrix col12[`nrows', 1] = e(N)

// MAKE TABLE and SAVE

// append columns
matrix GSS_next_sib3 = col1, col2, col3, col4, col5, col6, col7, col8, col9, col10, col11, col12

// save matrix
preserve
clear
svmat GSS_next_sib3

// export file
export excel "/Users/carolynstein/Dropbox (MIT)/Research/Sisters/Results/tables_raw.xlsx", sheet(GSS_next_sib3) sheetreplace
restore

* EFFECT OF OLDER SISTER on WIFE WORK ******************************************

// table parameters
local nrows = 12

// COLUMN 1

// regress
reg spwrksta any_older_sister i.sibs#i.birth_order, robust

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
gen older_sister_liberal = any_older_sister * liberal_region

// regress
reg spwrksta any_older_sister liberal_region older_sister_liberal i.sibs#i.birth_order, robust

// store
matrix col2 = J(`nrows', 1, 0)
local row = 1
matrix col2[`row', 1] = _b[any_older_sister]
local ++row
matrix col2[`row', 1] = _se[any_older_sister]
local ++row
matrix col2[`row', 1] = _b[liberal_region]
local ++row
matrix col2[`row', 1] = _se[liberal_region]
local ++row
matrix col2[`row', 1] = _b[older_sister_liberal]
local ++row
matrix col2[`row', 1] = _se[older_sister_liberal]
matrix col2[`nrows'-1, 1] = e(r2)
matrix col2[`nrows', 1] = e(N)

// COLUMN 3

// generate interaction
gen older_sister_spwrksta_hat = any_older_sister * spwrksta_hat

// regress
reg spwrksta any_older_sister spwrksta_hat older_sister_spwrksta_hat i.sibs#i.birth_order, robust

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
matrix col3[`row', 1] = _b[spwrksta_hat]
local ++row
matrix col3[`row', 1] = _se[spwrksta_hat]
local ++row
matrix col3[`row', 1] = _b[older_sister_spwrksta_hat]
local ++row
matrix col3[`row', 1] = _se[older_sister_spwrksta_hat]
matrix col3[`nrows'-1, 1] = e(r2)
matrix col3[`nrows', 1] = e(N)

// COLUMN 4

// regress
reg sp_frac_hrs any_older_sister i.sibs#i.birth_order, robust

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
reg sp_frac_hrs any_older_sister liberal_region older_sister_liberal i.sibs#i.birth_order, robust

// store
matrix col5 = J(`nrows', 1, 0)
local row = 1
matrix col5[`row', 1] = _b[any_older_sister]
local ++row
matrix col5[`row', 1] = _se[any_older_sister]
local ++row
matrix col5[`row', 1] = _b[liberal_region]
local ++row
matrix col5[`row', 1] = _se[liberal_region]
local ++row
matrix col5[`row', 1] = _b[older_sister_liberal]
local ++row
matrix col5[`row', 1] = _se[older_sister_liberal]
matrix col5[`nrows'-1, 1] = e(r2)
matrix col5[`nrows', 1] = e(N)


// COLUMN 6

// generate interaction
gen older_sister_hrs_hat = any_older_sister * hrs_hat

// regress
reg sp_frac_hrs any_older_sister hrs_hat older_sister_hrs_hat i.sibs#i.birth_order, robust

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
matrix col6[`row', 1] = _b[hrs_hat]
local ++row
matrix col6[`row', 1] = _se[hrs_hat]
local ++row
matrix col6[`row', 1] = _b[older_sister_hrs_hat]
local ++row
matrix col6[`row', 1] = _se[older_sister_hrs_hat]
matrix col6[`nrows'-1, 1] = e(r2)
matrix col6[`nrows', 1] = e(N)

// MAKE TABLE and SAVE

// append columns
matrix GSS_older_sister1 = col1, col2, col3, col4, col5, col6

// save matrix
preserve
clear
svmat GSS_older_sister1

// export file
export excel "/Users/carolynstein/Dropbox (MIT)/Research/Sisters/Results/tables_raw.xlsx", sheet(GSS_older_sister1) sheetreplace
restore


* EFFECT OF OLDER SISTER on HOUSEHOLD DUTIES ******************************************

// table parameters
local nrows = 12

// COLUMN 1

// regress
reg laundry any_older_sister i.sibs#i.birth_order, robust

// store
matrix col1 = J(`nrows', 1, 0)
local row = 1
matrix col1[`row', 1] = _b[any_older_sister]
local ++row
matrix col1[`row', 1] = _se[any_older_sister]
matrix col1[`nrows'-1, 1] = e(r2)
matrix col1[`nrows', 1] = e(N)

// COLUMN 2

// regress
reg laundry any_older_sister liberal_region older_sister_liberal i.sibs#i.birth_order, robust

// store
matrix col2 = J(`nrows', 1, 0)
local row = 1
matrix col2[`row', 1] = _b[any_older_sister]
local ++row
matrix col2[`row', 1] = _se[any_older_sister]
local ++row
matrix col2[`row', 1] = _b[liberal_region]
local ++row
matrix col2[`row', 1] = _se[liberal_region]
local ++row
matrix col2[`row', 1] = _b[older_sister_liberal]
local ++row
matrix col2[`row', 1] = _se[older_sister_liberal]
matrix col2[`nrows'-1, 1] = e(r2)
matrix col2[`nrows', 1] = e(N)

// COLUMN 3

// generate interaction
gen older_sister_laundry_hat = any_older_sister * laundry_hat

// regress
reg laundry any_older_sister laundry_hat older_sister_laundry_hat i.sibs#i.birth_order, robust

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
matrix col3[`row', 1] = _b[laundry_hat]
local ++row
matrix col3[`row', 1] = _se[laundry_hat]
local ++row
matrix col3[`row', 1] = _b[older_sister_laundry_hat]
local ++row
matrix col3[`row', 1] = _se[older_sister_laundry_hat]
matrix col3[`nrows'-1, 1] = e(r2)
matrix col3[`nrows', 1] = e(N)

// COLUMN 4

// regress
reg shopfood any_older_sister i.sibs#i.birth_order, robust

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
reg shopfood any_older_sister liberal_region older_sister_liberal i.sibs#i.birth_order, robust

// store
matrix col5 = J(`nrows', 1, 0)
local row = 1
matrix col5[`row', 1] = _b[any_older_sister]
local ++row
matrix col5[`row', 1] = _se[any_older_sister]
local ++row
matrix col5[`row', 1] = _b[liberal_region]
local ++row
matrix col5[`row', 1] = _se[liberal_region]
local ++row
matrix col5[`row', 1] = _b[older_sister_liberal]
local ++row
matrix col5[`row', 1] = _se[older_sister_liberal]
matrix col5[`nrows'-1, 1] = e(r2)
matrix col5[`nrows', 1] = e(N)


// COLUMN 6

// generate interaction
gen older_sister_shopfood_hat = any_older_sister * shopfood_hat

// regress
reg shopfood any_older_sister shopfood_hat older_sister_shopfood_hat i.sibs#i.birth_order, robust

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
matrix col6[`row', 1] = _b[shopfood_hat]
local ++row
matrix col6[`row', 1] = _se[shopfood_hat]
local ++row
matrix col6[`row', 1] = _b[older_sister_shopfood_hat]
local ++row
matrix col6[`row', 1] = _se[older_sister_shopfood_hat]
matrix col6[`nrows'-1, 1] = e(r2)
matrix col6[`nrows', 1] = e(N)

// COLUMN 7

// regress
reg dinner any_older_sister i.sibs#i.birth_order, robust

// store
matrix col7 = J(`nrows', 1, 0)
local row = 1
matrix col7[`row', 1] = _b[any_older_sister]
local ++row
matrix col7[`row', 1] = _se[any_older_sister]
matrix col7[`nrows'-1, 1] = e(r2)
matrix col7[`nrows', 1] = e(N)

// COLUMN 8

// regress
reg dinner any_older_sister liberal_region older_sister_liberal i.sibs#i.birth_order, robust

// store
matrix col8 = J(`nrows', 1, 0)
local row = 1
matrix col8[`row', 1] = _b[any_older_sister]
local ++row
matrix col8[`row', 1] = _se[any_older_sister]
local ++row
matrix col8[`row', 1] = _b[liberal_region]
local ++row
matrix col8[`row', 1] = _se[liberal_region]
local ++row
matrix col8[`row', 1] = _b[older_sister_liberal]
local ++row
matrix col8[`row', 1] = _se[older_sister_liberal]
matrix col8[`nrows'-1, 1] = e(r2)
matrix col8[`nrows', 1] = e(N)

// COLUMN 9

// generate interaction
gen older_sister_dinner_hat = any_older_sister * dinner_hat

// regress
reg dinner any_older_sister dinner_hat older_sister_dinner_hat i.sibs#i.birth_order, robust

// store
matrix col9 = J(`nrows', 1, 0)
local row = 1
matrix col9[`row', 1] = _b[any_older_sister]
local ++row
matrix col9[`row', 1] = _se[any_older_sister]
local ++row
matrix col9[`row', 1] = 0
local ++row
matrix col9[`row', 1] = 0
local ++row
matrix col9[`row', 1] = 0
local ++row
matrix col9[`row', 1] = 0
local ++row
matrix col9[`row', 1] = _b[dinner_hat]
local ++row
matrix col9[`row', 1] = _se[dinner_hat]
local ++row
matrix col9[`row', 1] = _b[older_sister_dinner_hat]
local ++row
matrix col9[`row', 1] = _se[older_sister_dinner_hat]
matrix col9[`nrows'-1, 1] = e(r2)
matrix col9[`nrows', 1] = e(N)

// COLUMN 10

// regress
reg repairs any_older_sister i.sibs#i.birth_order, robust

// store
matrix col10 = J(`nrows', 1, 0)
local row = 1
matrix col10[`row', 1] = _b[any_older_sister]
local ++row
matrix col10[`row', 1] = _se[any_older_sister]
matrix col10[`nrows'-1, 1] = e(r2)
matrix col10[`nrows', 1] = e(N)

// COLUMN 11

// regress
reg repairs any_older_sister liberal_region older_sister_liberal i.sibs#i.birth_order, robust

// store
matrix col11 = J(`nrows', 1, 0)
local row = 1
matrix col11[`row', 1] = _b[any_older_sister]
local ++row
matrix col11[`row', 1] = _se[any_older_sister]
local ++row
matrix col11[`row', 1] = _b[liberal_region]
local ++row
matrix col11[`row', 1] = _se[liberal_region]
local ++row
matrix col11[`row', 1] = _b[older_sister_liberal]
local ++row
matrix col11[`row', 1] = _se[older_sister_liberal]
matrix col11[`nrows'-1, 1] = e(r2)
matrix col11[`nrows', 1] = e(N)

// COLUMN 12

// generate interaction
gen older_sister_repairs_hat = any_older_sister * repairs_hat

// regress
reg repairs any_older_sister repairs_hat older_sister_repairs_hat i.sibs#i.birth_order, robust

// store
matrix col12 = J(`nrows', 1, 0)
local row = 1
matrix col12[`row', 1] = _b[any_older_sister]
local ++row
matrix col12[`row', 1] = _se[any_older_sister]
local ++row
matrix col12[`row', 1] = 0
local ++row
matrix col12[`row', 1] = 0
local ++row
matrix col12[`row', 1] = 0
local ++row
matrix col12[`row', 1] = 0
local ++row
matrix col12[`row', 1] = _b[repairs_hat]
local ++row
matrix col12[`row', 1] = _se[repairs_hat]
local ++row
matrix col12[`row', 1] = _b[older_sister_repairs_hat]
local ++row
matrix col12[`row', 1] = _se[older_sister_repairs_hat]
matrix col12[`nrows'-1, 1] = e(r2)
matrix col12[`nrows', 1] = e(N)

// MAKE TABLE and SAVE

// append columns
matrix GSS_older_sister2 = col1, col2, col3, col4, col5, col6, col7, col8, col9, col10, col11, col12

// save matrix
preserve
clear
svmat GSS_older_sister2

// export file
export excel "/Users/carolynstein/Dropbox (MIT)/Research/Sisters/Results/tables_raw.xlsx", sheet(GSS_older_sister2) sheetreplace
restore


* EFFECT OF OLDER SISTER on ATTITUDES ******************************************

// table parameters
local nrows = 12

// COLUMN 1

// regress
reg fework any_older_sister i.sibs#i.birth_order, robust

// store
matrix col1 = J(`nrows', 1, 0)
local row = 1
matrix col1[`row', 1] = _b[any_older_sister]
local ++row
matrix col1[`row', 1] = _se[any_older_sister]
matrix col1[`nrows'-1, 1] = e(r2)
matrix col1[`nrows', 1] = e(N)

// COLUMN 2

// regress
reg fework any_older_sister liberal_region older_sister_liberal i.sibs#i.birth_order, robust

// store
matrix col2 = J(`nrows', 1, 0)
local row = 1
matrix col2[`row', 1] = _b[any_older_sister]
local ++row
matrix col2[`row', 1] = _se[any_older_sister]
local ++row
matrix col2[`row', 1] = _b[liberal_region]
local ++row
matrix col2[`row', 1] = _se[liberal_region]
local ++row
matrix col2[`row', 1] = _b[older_sister_liberal]
local ++row
matrix col2[`row', 1] = _se[older_sister_liberal]
matrix col2[`nrows'-1, 1] = e(r2)
matrix col2[`nrows', 1] = e(N)

// COLUMN 3

// generate interaction
gen older_sister_fework_hat = any_older_sister * fework_hat

// regress
reg fework any_older_sister fework_hat older_sister_fework_hat i.sibs#i.birth_order, robust

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
matrix col3[`row', 1] = _b[fework_hat]
local ++row
matrix col3[`row', 1] = _se[fework_hat]
local ++row
matrix col3[`row', 1] = _b[older_sister_fework_hat]
local ++row
matrix col3[`row', 1] = _se[older_sister_fework_hat]
matrix col3[`nrows'-1, 1] = e(r2)
matrix col3[`nrows', 1] = e(N)

// COLUMN 4

// regress
reg fepol any_older_sister i.sibs#i.birth_order, robust

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
reg fepol any_older_sister liberal_region older_sister_liberal i.sibs#i.birth_order, robust

// store
matrix col5 = J(`nrows', 1, 0)
local row = 1
matrix col5[`row', 1] = _b[any_older_sister]
local ++row
matrix col5[`row', 1] = _se[any_older_sister]
local ++row
matrix col5[`row', 1] = _b[liberal_region]
local ++row
matrix col5[`row', 1] = _se[liberal_region]
local ++row
matrix col5[`row', 1] = _b[older_sister_liberal]
local ++row
matrix col5[`row', 1] = _se[older_sister_liberal]
matrix col5[`nrows'-1, 1] = e(r2)
matrix col5[`nrows', 1] = e(N)


// COLUMN 6

// generate interaction
gen older_sister_fepol_hat = any_older_sister * fepol_hat

// regress
reg fepol any_older_sister fepol_hat older_sister_fepol_hat i.sibs#i.birth_order, robust

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
matrix col6[`row', 1] = _b[fepol_hat]
local ++row
matrix col6[`row', 1] = _se[fepol_hat]
local ++row
matrix col6[`row', 1] = _b[older_sister_fepol_hat]
local ++row
matrix col6[`row', 1] = _se[older_sister_fepol_hat]
matrix col6[`nrows'-1, 1] = e(r2)
matrix col6[`nrows', 1] = e(N)

// COLUMN 7

// regress
reg hubbywk1 any_older_sister i.sibs#i.birth_order, robust

// store
matrix col7 = J(`nrows', 1, 0)
local row = 1
matrix col7[`row', 1] = _b[any_older_sister]
local ++row
matrix col7[`row', 1] = _se[any_older_sister]
matrix col7[`nrows'-1, 1] = e(r2)
matrix col7[`nrows', 1] = e(N)

// COLUMN 8

// regress
reg hubbywk1 any_older_sister liberal_region older_sister_liberal i.sibs#i.birth_order, robust

// store
matrix col8 = J(`nrows', 1, 0)
local row = 1
matrix col8[`row', 1] = _b[any_older_sister]
local ++row
matrix col8[`row', 1] = _se[any_older_sister]
local ++row
matrix col8[`row', 1] = _b[liberal_region]
local ++row
matrix col8[`row', 1] = _se[liberal_region]
local ++row
matrix col8[`row', 1] = _b[older_sister_liberal]
local ++row
matrix col8[`row', 1] = _se[older_sister_liberal]
matrix col8[`nrows'-1, 1] = e(r2)
matrix col8[`nrows', 1] = e(N)

// COLUMN 9

// generate interaction
gen older_sister_hubbywk1_hat = any_older_sister * hubbywk1_hat

// regress
reg hubbywk1 any_older_sister hubbywk1_hat older_sister_hubbywk1_hat i.sibs#i.birth_order, robust

// store
matrix col9 = J(`nrows', 1, 0)
local row = 1
matrix col9[`row', 1] = _b[any_older_sister]
local ++row
matrix col9[`row', 1] = _se[any_older_sister]
local ++row
matrix col9[`row', 1] = 0
local ++row
matrix col9[`row', 1] = 0
local ++row
matrix col9[`row', 1] = 0
local ++row
matrix col9[`row', 1] = 0
local ++row
matrix col9[`row', 1] = _b[hubbywk1_hat]
local ++row
matrix col9[`row', 1] = _se[hubbywk1_hat]
local ++row
matrix col9[`row', 1] = _b[older_sister_hubbywk1_hat]
local ++row
matrix col9[`row', 1] = _se[older_sister_hubbywk1_hat]
matrix col9[`nrows'-1, 1] = e(r2)
matrix col9[`nrows', 1] = e(N)

// COLUMN 10

// regress
reg mrmom any_older_sister i.sibs#i.birth_order, robust

// store
matrix col10 = J(`nrows', 1, 0)
local row = 1
matrix col10[`row', 1] = _b[any_older_sister]
local ++row
matrix col10[`row', 1] = _se[any_older_sister]
matrix col10[`nrows'-1, 1] = e(r2)
matrix col10[`nrows', 1] = e(N)

// COLUMN 11

// regress
reg mrmom any_older_sister liberal_region older_sister_liberal i.sibs#i.birth_order, robust

// store
matrix col11 = J(`nrows', 1, 0)
local row = 1
matrix col11[`row', 1] = _b[any_older_sister]
local ++row
matrix col11[`row', 1] = _se[any_older_sister]
local ++row
matrix col11[`row', 1] = _b[liberal_region]
local ++row
matrix col11[`row', 1] = _se[liberal_region]
local ++row
matrix col11[`row', 1] = _b[older_sister_liberal]
local ++row
matrix col11[`row', 1] = _se[older_sister_liberal]
matrix col11[`nrows'-1, 1] = e(r2)
matrix col11[`nrows', 1] = e(N)

// COLUMN 12

// generate interaction
gen older_sister_mrmom_hat = any_older_sister * mrmom_hat

// regress
reg mrmom any_older_sister mrmom_hat older_sister_mrmom_hat i.sibs#i.birth_order, robust

// store
matrix col12 = J(`nrows', 1, 0)
local row = 1
matrix col12[`row', 1] = _b[any_older_sister]
local ++row
matrix col12[`row', 1] = _se[any_older_sister]
local ++row
matrix col12[`row', 1] = 0
local ++row
matrix col12[`row', 1] = 0
local ++row
matrix col12[`row', 1] = 0
local ++row
matrix col12[`row', 1] = 0
local ++row
matrix col12[`row', 1] = _b[mrmom_hat]
local ++row
matrix col12[`row', 1] = _se[mrmom_hat]
local ++row
matrix col12[`row', 1] = _b[older_sister_mrmom_hat]
local ++row
matrix col12[`row', 1] = _se[older_sister_mrmom_hat]
matrix col12[`nrows'-1, 1] = e(r2)
matrix col12[`nrows', 1] = e(N)

// MAKE TABLE and SAVE

// append columns
matrix GSS_older_sister3 = col1, col2, col3, col4, col5, col6, col7, col8, col9, col10, col11, col12

// save matrix
preserve
clear
svmat GSS_older_sister3

// export file
export excel "/Users/carolynstein/Dropbox (MIT)/Research/Sisters/Results/tables_raw.xlsx", sheet(GSS_older_sister3) sheetreplace
restore

