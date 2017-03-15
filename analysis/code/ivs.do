./**********************************************************

#     _____    __   _  __                               _                 _       
#    / ____|  / _| | |/ /       /\                     | |               (_)      
#   | |  __  | |_  | ' /       /  \     _ __     __ _  | |  _   _   ___   _   ___ 
#   | | |_ | |  _| |  <       / /\ \   | '_ \   / _` | | | | | | | / __| | | / __|
#   | |__| | | |   | . \     / ____ \  | | | | | (_| | | | | |_| | \__ \ | | \__ \
#    \_____| |_|   |_|\_\   /_/    \_\ |_| |_|  \__,_| |_|  \__, | |___/ |_| |___/
#                                                            __/ |                
#                                                           |___/                 
#
#  _________________________
# |  _______________  |     |
# | |               | |1 2 3|
# | |               | |4 5 6|
# | |               | |7 8 9|
# | |               | |# # #|
# | |               | | ___ |
# | |_______________| ||___|| 
# |___________________|_____|


 * tables.do 
 *
 * GfK Singapore
 * Hannes Datta
 * bug reports: h.datta@tilburguniversity.edu
 
/ **********************************************************
 **********************************************************/

capture cd "d:\DATTA\Dropbox\Tilburg\Projects\GfK Singapore\shared\code\analysis\code\"

cap log close
log using ../temp/tables.txt, replace

set linesize 80

clear all
set more off
set seed 04071984
set sortseed 04071984
set scheme s2mono
set matsize 1000

*****
import delimited ../temp/tmp_ivs.csv







************************
***** DESCRIPTIVES *****
************************
	ivreg2 y acer_llen acer_nov3 acer_lagunitsales_sh allothers1_llen allothers1_nov3 allothers1_lagunitsales_sh ///
		   (acer_rwpspr acer_wpswdst allothers1_rwpspr allothers1_wpswdst = instr*), sfirst

matrix A = e(first)

* add sargan test stats
// get original row names of matrix (and row count)
local rownames : rowfullnames A
local c : word count `rownames'

// get original column names of matrix and substitute out _cons
local names : colfullnames A
local newnames : subinstr local names "_cons" "cons", word

// rename columns of matrix
matrix colnames A = `newnames'

// convert to dataset
clear
svmat A, names(col)

// add matrix row names to dataset
gen rownames = ""
forvalues i = 1/`c' {
    replace rownames = "`:word `i' of `rownames''" in `i'
}

* add predicted values



tset period

	ivreg2 y acer_llen acer_nov3 acer_lagunitsales_sh allothers1_llen allothers1_nov3 allothers1_lagunitsales_sh ///
		   (acer_rwpspr acer_wpswdst allothers1_rwpspr allothers1_wpswdst = d.instriv_cam_rwpsprd d.instriv_cam_wpswdst d.instriv_ph_rwpsprd d.instriv_ph_wpswdst d.instriv_tvdvd_rwpsprd d.instriv_tvdvd_wpswdst d.instriv_wte_rwpsprd d.instriv_wte_wpswdst d.instriv_linc_rwpsprd d.instriv_linc_wpswdst), sfirst












cap program drop ivs


program ivs
	syntax, panel_id(integer)
	*local panel_id = 1
	
	* STATIC CONTROLS: will be entered once in the equation (and not differenced and lagged)
	local controls_static trend dum_xmas dum_easter i.month
	* cos_t sin_t
	* DYNAMIC CONTROLS: will be entered twice in the equation (once differenced, once lagged)
	local controls_dynamic lgas cci
	* DEPENDENT VARIABLE: usually sales
	local depvar tot_lsales
	* ENDOGENOUS REGRESSORS: will be entered as lagged values only)
	local endog tot_lskus 
	*  pl_lpprom tot_lpprom
	*tot_ladv
	*tot_ladv
	* EXOGENOUS REGRESSORS: will be entered twice in the equation (once differenced, once lagged)
	local exog  
	* INSTRUMENTS: will be lagged as instruments, see van Heerde et al. 2013 (will be assembled automatically)
	local instr 
	
*lpl_padv
	* ASSEMBLE SYSTEM
	local tmp_exvars = ""
	local tmp_envars = ""
	local tmp_instr = ""

	foreach v in `controls_dynamic' `exog' {
		local tmp_exvars = `"`tmp_exvars' d.`v' l.`v'"'
		}
		
	foreach v in `endog' {
		local tmp_exvars = `"`tmp_exvars' l.`v'"'
		local tmp_envars = `"`tmp_envars' d.`v'"'
		local tmp_instr = `"`tmp_instr' l.osoc*_`v'"'
		}
	
	* add lagged dependent variable to model
	local tmp_exvars `tmp_exvars' l.`depvar'
	
	display "`tmp_exvars'"
	display "`tmp_envars'"
	display "`tmp_instr'"

	ivreg2 `depvar' `tmp_exvars' `controls_static' (`tmp_envars' = `tmp_instr') if panel_id == `panel_id', sfirst
	*robust
	
	* save values for predicted endogenous variables
end



********************
*     EXECUTION    *
********************


* build system to estimate

cap program drop get_data

program get_data
	use ../temp/preclean, replace
	
*# assert that all variables are measured for the benchmark brand
*	if (length(unique(melted_panel[brand==res$benchmark_brand]$variable)) < length(unique(melted_panel$variable))) {
*		stop('Not all variables measured for selected benchmark brand.')
*		}

* setup system 	

	* generate trend variable
	bysort category country brand (date): gen trend = _n

*format week %tw

	end
	
program 3sls
	syntax, category_id(integer)
	
	get_data
	keep if market_id == 1

	* CONTROLS
	local controls trend
	* DEPENDENT VARIABLE: market share (unit sales)
	local depvar usalessh
	* EXOGENOUS REGRESSORS
	local exog 
	* ENDOGENOUS REGRESSORS
	local endog wpspr llen 
	*nov3 wpswdst wpsun
	* INSTRUMENTS
	local instruments iv*wpsprd iv*llen 
	*iv*nov3 iv*wpswdst iv*wpsun
	
	
	gen br = "_" + brand
	
	local benchmark = benchmark[1]
	display "Benchmark brand is `benchmark'"
	
	* Keep only variables for reshaping
	keep `depvar' `endog' `exog' `instruments' month_no `controls' br
	
	reshape wide `depvar' `endog' `exog' `controls', i(month_no) j(br) string
	
	
	* Stuff to do: 
	* - save instruments such that they are without brand names (i.e., mege them to reshaped wide data set)
		
	
	* Drop trend for benchmark
	drop trend_`benchmark'
	
	* Replace all variables by their logs
	foreach var of varlist *wpspr* *trend* {
		*display "`var'"
		quietly replace `var' = log(`var')
		}
	
	* Generate dependent variables
	foreach var of varlist `depvar'* {
		display "`var'"
		* get brand 
		local brandname = subinstr("`var'", "`depvar'_", "", 1) 
		display "`brandname'"
		gen dv_`brandname' = log(`var'/usalessh_`benchmark')
		}

	* Drop DV for benchmark brand
	drop dv_`benchmark'
	
	tsset month_no
	
	ivreg2 dv_asus trend_asus wpspr_asus wpspr_acer, sfirst
	ivreg2 dv_asus trend_asus (wpspr_asus wpspr_acer = iv_cam_wpsprd_asus iv_ph_wpsprd_asus iv_tvdvd_wpsprd_asus iv_wte_wpsprd_asus), sfirst
	
	
	ivreg2 dv_asus trend_asus (wpspr_asus wpspr_acer = iv_cam_wpsprd_asus iv_ph_wpsprd_asus iv_tvdvd_wpsprd_asus iv_wte_wpsprd_asus), sfirst
	
	iv_linc_wpsprd_asus
	
		ivreg2 `depvar' `tmp_exvars' `controls_static' (`tmp_envars' = `tmp_instr') if panel_id == `panel_id', sfirst

	(dv_apple wpspr_apple)
	
	
	
	
	* ASSEMBLE SYSTEM
	local total_eq = ""
	
	forvalues sid = 1(1)5 {
		display "assembling variables for system `sid'"
		
		local tmp_exvars = ""
		local tmp_envars = ""
		local tmp_instr = ""

		foreach v in `controls_dynamic' {
			local tmp_exvars = `"`tmp_exvars' d.`v' l.`v'"'
			}
		foreach v in `exog' {
			local tmp_exvars = `"`tmp_exvars' d.`v'`sid' l.`v'`sid'"'
			}		
		
		*foreach v in `endog' {
		*	local tmp_exvars = `"`tmp_exvars' l.`v'"'
		*	local tmp_envars = `"`tmp_envars' d.`v'"'
		*	local tmp_instr = `"`tmp_instr' l.osoc*_`v'"'
		*	}
	
		* add lagged dependent variable to model
		local tmp_exvars `tmp_exvars' l.`depvar'`sid'
	
		*display "`tmp_exvars'"
		*display "`tmp_envars'"
		*display "`tmp_instr'"

		local total_eq = "`total_eq' (`depvar'`sid' `tmp_exvars' `controls_static')"
		
		*ivreg2 `depvar' `tmp_exvars' `controls_static' (`tmp_envars' = `tmp_instr') if panel_id == `panel_id', sfirst

		}
	
		display "`total_eq'"
		reg3 `total_eq'
		
	* ESTIMATE
		
	reg3 (tot_lsales1 d.lgas l.lgas d.cci l.cci d.tot_lskus1 l.tot_lskus1 l.tot_lsales1 trend dum_xmas dum_easter sin_t cos_t) (tot_lsales2 d.lgas l.lgas d.cci l.cci d.tot_lskus2 l.tot_lskus2 l.tot_lsales2 trend dum_xmas dum_easter sin_t cos_t) (tot_lsales3 d.lgas l.lgas d.cci l.cci d.tot_lskus3 l.tot_lskus3 l.tot_lsales3 trend dum_xmas dum_easter sin_t cos_t) (tot_lsales4 d.lgas l.lgas d.cci l.cci d.tot_lskus4 l.tot_lskus4 l.tot_lsales4 trend dum_xmas dum_easter sin_t cos_t) (tot_lsales5 d.lgas l.lgas d.cci l.cci d.tot_lskus5 l.tot_lskus5 l.tot_lsales5 trend dum_xmas dum_easter sin_t cos_t)
	
	matrix C = e(b)

	local cnames : colnames C
	local ceq: coleq C
	
	local i 0
	
	capture matrix drop results
		
	foreach name of local cnames {
		local ++i
		local eq: word `i' of `ceq'
		local var: word `i' of `cnames'
		
		display "`eq' :: `var'"
		
		* all lag values (except the lag sales value) need to be need to be divided by the sales estimate
		if (substr("`var'", 1, 2) == "L." & "L.`eq'"!="`var'") {
			local salesvar L.`eq'
			nlcom [`eq']`var' / [`eq']`salesvar'
			} 
			else 
			{
			* for all others: do nothing
			nlcom [`eq']`var'
			}
		
		* write matrix here
		return list
			
			file write myfile %12.0g (r(estimate)) _tab ///
							  %12.0g (r(se)) _tab 
			

			
		matrix tmp = C[`i', 2*`j'-1]
		
		}
	
		matrix tmp = C[`i', 2*`j'-1]
			if tmp[1,1]<. {
				matrix colnames tmp = `model'
				matrix b = nullmat(b), tmp
				matrix tmp[1,1] = C[`i', 2*`j']
				matrix se = nullmat(se), tmp
		
		
	* Next step is to spit out the results from above in new tables (e.g., a collection of betas, SEs, etc. by equation. Then 
	* save that stuff.
	
	
	
	* I need an algorithm that builds the equations
ivs, panel_id(2)


* pre-determine endogenous regressors (!!!!)

* determine regressors for log price







constraint 1 [y1]b2 = [y2]b2
constraint 2 [y1]b3 = [y2]b3
constraint 3 [y1]b4 = [y2]b4
constraint 4 [y1]b5 = [y2]b5
constraint 5 [y1]b6 = [y2]b6


sureg (y1 var2-var7 b2-b6) (y2 bar2-bar7 b2-b6), const(1 2 3 4 5)


* OLS
reg(y1 var2-var7 b2-b6)
reg(y2 bar2-bar7 b2-b6)


reg3 (y1 var2-var7 b2-b6) (y2 bar2-bar7 b2-b6),  const(1 2 3 4 5) ols



reg y1 var2-var7 b2-b6


constraint 1 [allothers]acer_rwpsprice = [compaq]acer_rwpsprice
constraint 2 [hp]acer_rwpsprice = [compaq]acer_rwpsprice

constraint 3 [allothers]acer_llength = [compaq]acer_llength
constraint 4 [hp]acer_llength = [compaq]acer_llength

constraint 5 [allothers]acer_wpswdist1 = [compaq]acer_wpswdist1
constraint 6 [hp]acer_wpswdist1 = [compaq]acer_wpswdist1

constraint 7 [allothers]acer_novel1 = [compaq]acer_novel1
constraint 8 [hp]acer_novel1 = [compaq]acer_novel1

constraint 9 [allothers]acer_ylag_1 = [compaq]acer_ylag_1
constraint 10 [hp]acer_ylag_1 = [compaq]acer_ylag_1

reg3 (allothers allothers_* acer_*) (compaq compaq_* acer_*)  (hp hp_* acer_*) ,  const(1 2 3 4 5 6 7 8 9 10) ols




sureg (allothers allothers_* acer_*) (compaq compaq_* acer_*)  (hp hp_* acer_*) ,  const(1 2 3 4 5 6 7 8 9 10)





* new v


constraint 1 [allothers]acer_rwpsprice = [compaq]acer_rwpsprice

constraint 3 [allothers]acer_llength = [compaq]acer_llength

constraint 5 [allothers]acer_wpswdist1 = [compaq]acer_wpswdist1

constraint 7 [allothers]acer_novel1 = [compaq]acer_novel1

constraint 9 [allothers]acer_ylag_1 = [compaq]acer_ylag_1

reg3 (allothers allothers_* acer_*) (compaq compaq_* acer_*),  const(1 3 5 7 9) ols

