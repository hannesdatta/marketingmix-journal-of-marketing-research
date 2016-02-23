
' #################################
' #                            SETUP
' #################################

' load data set from R
	wfopen "d:\DATTA\Dropbox\Tilburg\Projects\GfK Singapore\SVN_HomeAppliance\derived\out\eviews_REGULAR MOBILE PHONES_VIETNAM_SAMSUNG_0_NA_0_0_0_0_0.csv"
	' create cross-sectional data structure by date
	pagestruct @date(date) 

' setup of models and specs for code
!setup_pval = .05 ' at which significance level should the analysis be computed?!
!setup_maxlag=12 ' what's the maximum lag for Unit Root Tests (ADF), and calibration of lag length vor the VAR models

%model_endog = "unitsales_regr line_length_regr weightedprice_regr wdist_regr weighted_uniqueness_regr brand_dispersion_regr"
%model_exog = "comp_unitsales_regr comp_line_length_regr comp_weightedprice_regr comp_wdist_regr comp_wunique_regr comp_bdispers_regr"
'%model = "unitsales_regr line_length_regr weightedprice_regr wdist_regr weighted_uniqueness_regr brand_dispersion_regr @ comp_unitsales_regr comp_line_length_regr comp_weightedprice_regr comp_wdist_regr comp_wunique_regr comp_bdispers_regr"
'%model = {%model_endog} {%model_exog}

' //////////////////////////////////
' /// EXECUTION   ///
' //////////////////////////////////

INCLUDE "d:\DATTA\Dropbox\Tilburg\Projects\GfK Singapore\SVN_HomeAppliance\analysis\common_routines.prg" ' include some general code and routines for this project

'equation eq10.ls d(unitsales) c unitsales(-1) d(unitsales(-1)) @trend("2004M1") @expand(@month, @drop(1))
'scalar stuff = eq10.@tstats(15)

rename weighteddistribution wdist
rename comp_weighteddistributio comp_wdist
rename comp_weighted_uniqueness comp_wunique
rename comp_brand_dispersion comp_bdispers

' STEP 1: perform unit root tests
	'CALL UNITROOT("unitsales",12,.05)
	'SCALAR result=!unitroot_result
	matrix (14,3) results_unitroot
	!o=1
	for %m UNITSALES LINE_LENGTH WEIGHTEDPRICE WDIST WEIGHTED_UNIQUENESS BRAND_DISPERSION comp_unitsales comp_line_length comp_weightedprice comp_wdist comp_wunique comp_bdispers
		CALL UNITROOT(%m,!setup_maxlag,!setup_pval)
		CALL SAVEUNITROOT(!o,%m)
		!o=!o+1
	next
' Code automatically saves differenced series into {varname}_regr if unit root is rejected (i.e., if series is non-stationary). In this way, the regression in the VAR model can be readily run using the new variable names.

' STEP 2: Run var models
	' Estimate VAR with maximum number of lags
	var brandvar_maxlag.ls 1 !setup_maxlag {%model_endog} @ {%model_exog} ' estimate model with endogenous variables: brand-specific variables; exogenous variables: constant + competitor-specific variables

	' Now, determine lag length, based on SBC (=BIC)
	brandvar_maxlag.laglen(12, vname=results_laglength) ' result of SBC test's lag length stored in results_laglength(4,1) (row 4, column 1)
	!laglength_sic=results_laglength(4,1) ' create variable which only contains lag length
	
	' Re-estimate var using the selected lag length
	var brandvar_selectedlag.ls 1 !laglength_sic {%model_endog} @ {%model_exog} 
	' --> based on this model, I can now check for cointegration

' STEP 3: Test for cointegration BUT ONLY IF AT LEAST ONE SERIES HAS A UNIT ROOT!!!
	' Do a Johansen Test for Cointegration
	'brandvar_selectedlag.coint(c,!laglength_sic,cvsize=!setup_pval,save=brandvar_coint) '@ {%model_endog}
     ' model currently contains exogenous variables!

	'so, rather estimate cointegration var model without exogenous variables, using this specification
	var brandvar_coint.ls 1 !laglength_sic {%model_endog} '@ {%model_exog} 
	freeze(brandvar_coint_table) brandvar_coint.coint(c,!laglength_sic,cvsize=!setup_pval,save=brandvar_coint_res) '{%model_endog}

	' @Marnik:
	' - exclude exogenous variables?
	' - only put in variables for which we have found a unit root??! (right?)
	
	'store results: 
'	FOR %i {%model_endog}
		GROUP VARM1 {%model_endog}
'		VARM1.add {%i}
	!varcount = VARM1.@count

	matrix (!varcount, 2) brandvar_coint_result
	!startrow=13
	!j=!startrow
	!stop=0
	WHILE !j<=!startrow+!varcount-1 AND !stop=0
			%step1=brandvar_coint_table(!j,5)
			'SCALAR statval=%step1
			SCALAR statval=@VAL(%step1)
			IF statval < !setup_pval THEN
				brandvar_coint_result(!j-!startrow+1,1)=statval
				brandvar_coint_result(!j-!startrow+1,2)=!j-!startrow
				' rank (j) is significant, continue!
				!j=!j+1
			ELSE
				brandvar_coint_result(!j-!startrow+1,1)=statval
				brandvar_coint_result(!j-!startrow+1,2)=!j-!startrow
				!stop=1
				ENDIF
	WEND

	' Extract results from co-integration
	SCALAR results_coint_rank = !j-!startrow ' saves rank


' STEP 4: Estimate, or leave it with VAR

' IF rank = 0, keep VAR
' IF rank > 0, estimate VEC with rank equal to the variable results_coint_rank
IF results_coint_rank > 0 THEN
	' Crea
	var brandvar_vec.ec(c, results_coint_rank) 1 !laglength_sic {%model_endog} @ {%model_exog}
ENDIF

' STEP 5: Forecasting from VAR or VEC
brandvar_vec.impulse(12,t, a, imp=gen, se=a)
brandvar_selectedlag.impulse(12,t, a, imp=gen, se=a)
'a accumulates them!
'g graph, t table




%name="unitsales"
'equation tmp_eq.ls d(unitsales) c {stuffla2} 
%eqstring="d(unitsales) c D(unitsales)(-1) D(unitsales)(-2) D(unitsales)(-3) D(unitsales)(-4)"



equation tmp_eq.ls D(unitsales) c D(unitsales)(-1)


d(unitsales)(-1)c
' check significance of last lag

		' save result
	ENDSUB
CALL UNITROOTTEST("unitsales")


		SERIES series1 = log({%namestring}) 'converted to logs
		series1.uroot(adf,trend,info=sic,save=tmp_uroot)
		' check conditions and supply output vector
		!res_unitroot = @subextract(tmp_uroot, @rows(tmp_uroot)-1,2,@rows(tmp_uroot)-1,2)
		if !res_unitroot < .05 THEN
			SERIES res_saveseries = d(series1)
			ELSE
			SERIES res_saveseries = d(series1)
		ENDIF
		'%namestring="unit_d"
		SERIES {%namestring}_x = res_saveseries
		' save result of test to result_series
	ENDSUB



stepdum ftrend trenddum krant(-1) d(krant(-1)) d(krant(-2)) d(krant(-3)) d(krant(-4)) d(krant(-5)) d(krant(-6)) d(krant(-7)) d(krant(-8)) d(krant(-9)) d(krant(-10)) gseasd
           



' STEP 5: WRITE OUTPUT TO FILE FOR FURTHER PROCESSING IN R
' @Marnik: which stuff do we want to save? "how should a report to the project team look like?"




'Output to be defined:

' Included variables:
' Unit Root present? --> describe how they are entered in the model
' (and how it is being computed!)

' Cointegration results: present/yes no
' Estimated model: also give details about the lags being used
' Impulse response functions
' Output per country-category; include in R output.


' CODE SOURCES: 
' https://sites.google.com/site/davesmant/courses/working-with-eviews/intro-eviews-programming
' http://www.ssc.upenn.edu/~fdiebold/Teaching221/EviewsIntroduction.pdf
' http://www.sscnet.ucla.edu/labs/Eviews5/EviewsCommandReference.pdf
'https://remote.bus.brocku.ca/files/Published_Resources/EViews_7/Docs/EViews%207%20Command%20Ref.pdf
'http://www.eviews.com/Learning/

' other questions: 
'Questions to Marnik:

' !!! 'Brand dispersion measure doesnt really work
' E.g., dispersion: level differences. If its low, how can it be taken into consideration.
'What if dispersion doesnt change?! e.g., SAST in china, no dispersion.

https://dl.dropboxusercontent.com/u/4900810/uvt_singapore/index.html

' MUELLHALDE


