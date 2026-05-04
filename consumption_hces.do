cd"/Users/ruohanhu/Library/CloudStorage/OneDrive-HarvardUniversity/2_Classes/GHP201/GHP201_git"

clear all
set more off


use "level_15.dta", clear
collapse (mean) hh_size (firstnm) multiplier, by(hhid)
gen whhsize = hh_size * multiplier
quietly summarize whhsize
local num = r(sum)
quietly summarize multiplier
local den = r(sum)
display `num' / `den'

* Household size + rural/urban household and population ratios

use "level_15.dta", clear

drop if missing(hh_size) | hh_size <= 0

by hhid, sort: egen has_visit3 = max(visit == 3)
gen use_record = (visit == 3 & has_visit3 == 1) | has_visit3 == 0
keep if use_record

collapse (mean) hh_size, by(hhid)

tempfile hhsize
save `hhsize'

use "level_01.dta", clear
keep hhid sector multiplier

keep if inlist(sector, 1, 2)
drop if missing(multiplier) | multiplier <= 0

merge 1:1 hhid using `hhsize', keep(match) nogen

gen weighted_hh_size = hh_size * multiplier
gen weighted_people = multiplier * hh_size

collapse ///
    (count) sample_households = hhid ///
    (sum) weighted_households = multiplier ///
          weighted_people = weighted_people ///
          sum_weighted_hh_size = weighted_hh_size ///
    (mean) mean_hh_size_unweighted = hh_size, ///
    by(sector)

gen mean_hh_size_weighted = sum_weighted_hh_size / weighted_households

label define sector_lbl 1 "Rural" 2 "Urban", replace
label values sector sector_lbl

list sector sample_households weighted_households weighted_people ///
    mean_hh_size_unweighted mean_hh_size_weighted, clean

* Rural:Urban household ratio
quietly summarize weighted_households if sector == 1
local rural_hh = r(sum)

quietly summarize weighted_households if sector == 2
local urban_hh = r(sum)

display "Rural:Urban household ratio = " `rural_hh' / `urban_hh' ":1"

* Rural:Urban population ratio
quietly summarize weighted_people if sector == 1
local rural_pop = r(sum)

quietly summarize weighted_people if sector == 2
local urban_pop = r(sum)

display "Rural:Urban population ratio = " `rural_pop' / `urban_pop' ":1"
