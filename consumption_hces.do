cd"/Users/ruohanhu/Library/CloudStorage/OneDrive-HarvardUniversity/2_Classes/GHP201/GHP201_git"

clear all
set more off
set seed 20260504

/*
Empirical weighted resampling of 100,000 households from HCES.

Inputs:
    level_01.dta  Household sector and survey expansion weight
    level_15.dta  Visit-level household summary consumption and size

Important interpretation warning:
    hh_usual_cons_exp_mnth is used as a proxy for monthly household
    consumption. This may not exactly reproduce official HCES MPCE, which is
    constructed from detailed FDQ, CSQ, and DGQ item-level modules.

Method:
    - One analytic row per hhid.
    - Use Level 01 multiplier as the household weight.
    - Prefer Level 15 visit == 3; if unavailable, average valid visits.
    - Do not sum consumption across visits.
    - Construct sector-specific household-weighted quintiles by
      hh_usual_cons_exp_mnth.
    - Simulate 100,000 households by sampling observed households with
      replacement, proportional to household weight within sector.
*/

local n_sim = 100000

tempfile level01_hh level15_hh analytic_hh

*----------------------------
* Level 01 household weights
*----------------------------
use "level_01.dta", clear

foreach v in hhid sector multiplier {
    capture confirm variable `v'
    if _rc {
        display as error "Required variable `v' not found in level_01.dta"
        exit 111
    }
}

keep hhid sector multiplier
rename multiplier household_weight

duplicates tag hhid, gen(dup_hhid)
count if dup_hhid > 0
if r(N) > 0 {
    display as error "Duplicate hhid values found in level_01.dta."
    duplicates list hhid if dup_hhid > 0
    exit 459
}
drop dup_hhid

count if !inlist(sector, 1, 2)
display as text "Level 01 households dropped for sector not equal to 1 or 2: " r(N)

count if missing(household_weight) | household_weight <= 0
display as text "Level 01 households dropped for missing/non-positive multiplier: " r(N)

keep if inlist(sector, 1, 2)
drop if missing(household_weight) | household_weight <= 0

label define sector_lbl 1 "Rural" 2 "Urban", replace
label values sector sector_lbl
isid hhid
save `level01_hh'

*-----------------------------------------
* Level 15 household consumption and size
*-----------------------------------------
use "level_15.dta", clear

foreach v in hhid visit hh_usual_cons_exp_mnth hh_size {
    capture confirm variable `v'
    if _rc {
        display as error "Required variable `v' not found in level_15.dta"
        exit 111
    }
}

capture confirm variable level
if !_rc {
    capture confirm numeric variable level
    if !_rc {
        count if level != 15 & !missing(level)
        display as text "Level 15 rows dropped because level is not 15: " r(N)
        keep if level == 15 | missing(level)
    }
}

count if missing(hh_size) | hh_size <= 0
display as text "Level 15 rows dropped for missing/non-positive hh_size: " r(N)

count if missing(hh_usual_cons_exp_mnth) | hh_usual_cons_exp_mnth < 0
display as text "Level 15 rows dropped for missing/negative consumption: " r(N)

drop if missing(hh_size) | hh_size <= 0
drop if missing(hh_usual_cons_exp_mnth) | hh_usual_cons_exp_mnth < 0

by hhid, sort: egen has_visit3 = max(visit == 3)
gen use_for_hh_record = (visit == 3 & has_visit3 == 1) | has_visit3 == 0
keep if use_for_hh_record

gen used_visit3 = has_visit3 == 1
gen used_fallback_avg_visits = has_visit3 == 0

collapse ///
    (mean) hh_size hh_usual_cons_exp_mnth ///
    (count) n_level15_records_used = hh_usual_cons_exp_mnth ///
    (max) used_visit3 used_fallback_avg_visits, ///
    by(hhid)

duplicates tag hhid, gen(dup_hhid)
count if dup_hhid > 0
if r(N) > 0 {
    display as error "Duplicate hhid values remain after Level 15 collapse."
    duplicates list hhid if dup_hhid > 0
    exit 459
}
drop dup_hhid

isid hhid
save `level15_hh'

*-----------------------------
* Merge analytic household file
*-----------------------------
use `level01_hh', clear
merge 1:1 hhid using `level15_hh', gen(_merge_level01_level15)

tab _merge_level01_level15
keep if _merge_level01_level15 == 3
drop _merge_level01_level15

label define sector_lbl 1 "Rural" 2 "Urban", replace
label values sector sector_lbl

* Household-weighted quintiles within Rural and Urban.
sort sector hh_usual_cons_exp_mnth hhid
by sector: egen sector_weight = total(household_weight)
by sector: gen cum_weight = sum(household_weight)
gen weight_midpoint = (cum_weight - household_weight / 2) / sector_weight
gen household_quintile = floor(5 * weight_midpoint) + 1
replace household_quintile = 1 if household_quintile < 1
replace household_quintile = 5 if household_quintile > 5

label variable household_quintile ///
    "Sector-specific household-weighted consumption quintile"

save `analytic_hh'

*-----------------------------------------------
* Draw exactly 100,000 simulated households.
* Rural/Urban counts follow weighted household shares exactly up to rounding.
* Within sector, households are sampled with replacement proportional to weight.
*-----------------------------------------------
use `analytic_hh', clear
keep sector hh_size hh_usual_cons_exp_mnth household_weight household_quintile

mata:
real scalar weighted_pick(real colvector cumw, real scalar u)
{
    real scalar lo, hi, mid

    lo = 1
    hi = rows(cumw)

    while (lo < hi) {
        mid = floor((lo + hi) / 2)
        if (cumw[mid] >= u) {
            hi = mid
        }
        else {
            lo = mid + 1
        }
    }

    return(lo)
}

void simulate_households()
{
    real scalar n_sim, total_weight, base_total, remainder_n
    real scalar s, k, n_s, draw_row, sampled_row, u, cum_total
    real colvector sector, hh_size, hh_cons, weight, quintile
    real colvector sectors, sector_weight, raw_n, n_by_sector, remainder
    real colvector idx, w_s, cumw
    real matrix out

    n_sim = strtoreal(st_local("n_sim"))

    sector = st_data(., "sector")
    hh_size = st_data(., "hh_size")
    hh_cons = st_data(., "hh_usual_cons_exp_mnth")
    weight = st_data(., "household_weight")
    quintile = st_data(., "household_quintile")

    sectors = (1 \ 2)
    sector_weight = J(2, 1, 0)

    for (s = 1; s <= 2; s++) {
        idx = selectindex(sector :== sectors[s])
        sector_weight[s] = sum(weight[idx])
    }

    total_weight = sum(sector_weight)
    raw_n = n_sim :* sector_weight :/ total_weight
    n_by_sector = floor(raw_n)
    remainder = raw_n :- n_by_sector
    base_total = sum(n_by_sector)
    remainder_n = n_sim - base_total

    if (remainder_n == 1) {
        if (remainder[1] >= remainder[2]) {
            n_by_sector[1] = n_by_sector[1] + 1
        }
        else {
            n_by_sector[2] = n_by_sector[2] + 1
        }
    }

    out = J(n_sim, 4, .)
    draw_row = 1

    for (s = 1; s <= 2; s++) {
        idx = selectindex(sector :== sectors[s])
        w_s = weight[idx]
        cumw = J(rows(w_s), 1, .)
        cum_total = 0

        for (k = 1; k <= rows(w_s); k++) {
            cum_total = cum_total + w_s[k]
            cumw[k] = cum_total
        }

        n_s = n_by_sector[s]

        for (k = 1; k <= n_s; k++) {
            u = runiform(1, 1) * cum_total
            sampled_row = idx[weighted_pick(cumw, u)]

            out[draw_row, 1] = sector[sampled_row]
            out[draw_row, 2] = hh_size[sampled_row]
            out[draw_row, 3] = hh_cons[sampled_row]
            out[draw_row, 4] = quintile[sampled_row]
            draw_row = draw_row + 1
        }
    }

    stata("clear")
    stata("set obs " + strofreal(n_sim))
    stata("gen sector = .")
    stata("gen double hh_size = .")
    stata("gen double hh_usual_cons_exp_mnth = .")
    stata("gen household_quintile = .")

    st_store(., ("sector", "hh_size", "hh_usual_cons_exp_mnth", "household_quintile"), out)
}

simulate_households()
end

label define sector_lbl 1 "Rural" 2 "Urban", replace
label values sector sector_lbl
label variable hh_size "Household size"
label variable hh_usual_cons_exp_mnth "Monthly household consumption proxy"
label variable household_quintile ///
    "Sector-specific household-weighted consumption quintile"

keep sector hh_size hh_usual_cons_exp_mnth household_quintile
save "hces_simulated_100k_households.dta", replace
export delimited using "hces_simulated_100k_households.csv", replace
