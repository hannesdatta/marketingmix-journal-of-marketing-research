
capture cd "d:\DATTA\Dropbox\Tilburg\Projects\GfK Singapore\shared\code\analysis\code\"

cap log close

set linesize 80

clear all
set more off
set seed 04071984
set sortseed 04071984
set scheme s2mono
set matsize 1000


import delimited ../temp/preclean.csv, clear
sort category country date

*gen date = td(4oct2009) + 7 * (week-1)
*format date  %d
*gen month=month(date)

*g week_num = week
*replace week = week + tw(2009w40) - 1
*format week %tw

*xtset panel_id week, week

* Label main explanatory variables
label var category "Category"
label var country "Country"
label var brand "Brand"
label var date "Date"

*foreach v of varlist iv_* {
*	display "`v'"
*	quietly replace `v' = 0 if `v' == .
*	}

* determine benchmark brand: brand with max observations; if multiple brands qualify, take first one in alphabet
gen N = 1
egen obs = total(N), by(category country brand_id)
egen maxobs = max(obs)
egen tag = tag(category country brand_id) if obs == maxobs
replace tag = 1-tag
sort market_id tag brand
bysort market_id: gen benchmark = brand[1] 
drop N obs maxobs tag
sort category country brand date

	
sort category country brand date

save ../temp/preclean, replace
