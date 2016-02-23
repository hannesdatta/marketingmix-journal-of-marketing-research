
' #################################
' #                            SETUP
' #################################

' Model setup
%setup_endog =   "log(unitsales+1) log(rwprice) log(llength+1) log(wdist*100+1) log(wunique*100+1) log(novel*100+1) log(radv)"
%setup_exog = "log(c_rwprice) log(c_llength+1) log(c_wdist*100+1) log(c_wunique*100+1) log(c_novel*100+1) log(c_radv)"

' Read command-line arguments
' If arguments are empty, specify defaults. If not, take from command prompt

IF %0 = "" THEN
	%filename="d:\DATTA\Dropbox\Tilburg\Projects\GfK Singapore\SVN_GfkSingapore\analysis\temp\datasets\eviews_REGULAR MOBILE PHONES_AUSTRALIA_LG.csv"
	%filename="d:\DATTA\Dropbox\Tilburg\Projects\GfK Singapore\SVN_GfkSingapore\analysis\temp\datasets\eviews_CAMERAS STANDARD_INDONESIA_SONY.csv"

	%filename="d:\DATTA\Dropbox\Tilburg\Projects\GfK Singapore\SVN_GfkSingapore\analysis\temp\datasets\eviews_MOBILE COMPUTING_INDONESIA_TOSHIBA.csv"
	%filename="d:\DATTA\Dropbox\Tilburg\Projects\GfK Singapore\SVN_GfkSingapore\analysis\temp\datasets\eviews_CAMERAS ADVANCED LENSES_HONG KONG_SONY.csv"
	%filename="d:\DATTA\Dropbox\Tilburg\Projects\GfK Singapore\SVN_GfkSingapore\analysis\temp\datasets\eviews_DVDPLAYERREC_INDIA_LOCAL.csv"
	%filename="d:\DATTA\Dropbox\Tilburg\Projects\GfK Singapore\SVN_GfkSingapore\analysis\temp\datasets\eviews_WASHINGMACHINES_INDONESIA_LG.csv"
	%filename="d:\DATTA\Dropbox\Tilburg\Projects\GfK Singapore\SVN_GfkSingapore\analysis\temp\datasets\eviews_SMARTPHONES_PHILIPPINES_BLACKBERRY.csv"

	%filename="d:\DATTA\Dropbox\Tilburg\Projects\GfK Singapore\SVN_GfkSingapore\analysis\temp\datasets\eviews_CAMERAS ADVANCED LENSES_AUSTRALIA_SONY.csv"

	%outdir="""d:\DATTA\Dropbox\Tilburg\Projects\GfK Singapore\SVN_GfkSingapore\analysis\temp"""
	%name="""manualrun"""
	!setup_pval = .05
ELSE
	%folder=%0
	%name=%1
	%filename={%folder}+{%name}+".csv"
	%outdir=%2
	
	' setup of models and specs for code
	!setup_pval = @val(%3) 'read from command prompt .05 ' at which significance level should the analysis be computed?!
	setmaxerrs 50000 ' ignores any error in execution
ENDIF

!setup_maxlag=3 ''what's the maximum lag for Unit Root Tests (ADF), and calibration of lag length vor the VAR models

' //////////////////////////////////
' /// EXECUTION   ///
' //////////////////////////////////


' Open file
	wfopen %filename colhead=1
' create cross-sectional data structure by date
	pagestruct @date(date) 

sample mysample if selected="TRUE"
pagecopy(smpl=mysample, page="analysis")
pagedelete untitled
delete mysample

' Load prerequisites	
	INCLUDE "d:\DATTA\Dropbox\Tilburg\Projects\GfK Singapore\SVN_GfkSingapore\analysis\code\common_routines.prg" ' include some general code and routines for this project
	INCLUDE "d:\DATTA\Dropbox\Tilburg\Projects\GfK Singapore\SVN_GfkSingapore\analysis\code\routine_unitroot.prg" '

' Check number of observations for the sales variable; if it's lower than 36 (3 years), abort this program
	SCALAR numberofobs = unitsales.@obs
	if numberofobs < 36 THEN EXIT
	ENDIF
	DELETE numberofobs

' Check whether there are sufficient number of observations in all other variables; if so, add to estimation vector(s), if not, drop.
CALL VERIFY_OBS(%setup_endog, %model_endog, 36)
CALL VERIFY_OBS(%setup_exog, %model_exog, 36)

' Create new variable groups to split up the endogenous variables into variables WITH and WITHOUT unit root
GROUP model_endog_UR
GROUP model_endog_UR_d
GROUP model_endog_noUR
GROUP model_endog_noUR_c

'GROUP model_exog_UR
GROUP model_exog_UR_d
GROUP model_exog_noUR

' ====================================
' STEP 1: perform unit root tests
' ====================================

	' set up result table for unit root tests
	TABLE(14+1,9) results_unitroot
	' set column headers
     results_unitroot(1,1)="selected lag length"
     results_unitroot(1,2)="SBC"
     results_unitroot(1,3)="ADF t-stat"
     results_unitroot(1,4)="unitroot_yesno"
     results_unitroot(1,5)="testorder"
     results_unitroot(1,6)="integrationorder"
     results_unitroot(1,7)="variable"
	results_unitroot(1,8)="endog_vs_exog"
	results_unitroot(1,9)="N"

	'///////////////////////////////////////////////////////////////////////
	' Tests for ENDOGENOUS VARIABLES
	'///////////////////////////////////////////////////////////////////////
	!o=1 ' specifies where to store the results in the table (i.e., row 1)

	for %m {%model_endog}
		matrix (1,1) tmp_ur
		SERIES emptyseries
		CALL UNITROOT_SBC_FULL({%m},!setup_maxlag,!setup_pval, tmp_ur, emptyseries)
		DELETE emptyseries
		SERIES tmpser = {%m}
		!tmpnobs = tmpser.@obs
		CALL SAVEUNITROOT(!o,%m, results_unitroot, tmp_ur, "endog",!tmpnobs)
		IF tmp_ur(1,4)=1 THEN
			' UNIT ROOT FOUND:
			' This part 'inserts' a variable in a group of variables which can later be used
			' to flexibly specify our VARX/VEC models and cointegration tests.

			!tmpintegrorder = tmp_ur(1,4)+tmp_ur(1,5)
			IF !tmpintegrorder > 1 THEN
				' It is found that the variable is integrated of order I(2) and higher.
				' Therefore, these variables need to be differenced; usually, it is 
				' possible to difference them up to I(1) processes. However,
				' we decide to only difference them ONCE (i.e., treat I(2), I(3) processes AS IF
				' there were I(1).

				' model_endog_UR.add D({%m}, !tmpintegrorder-1) --> this differences the variable up to !tmpintegrorder-1 times, so that it IS I(1).
				model_endog_UR.add {%m} 
				' used to perform cointegration tests (VAR model with non-differenced I(1) variables), and used in VEC model (where series will be automatically differenced)
				model_endog_UR_d.add D({%m}) 
				'used to estimate final VAR model without cointegration
				ELSE
				' Integration of I(1)
				model_endog_UR.add {%m} 
				' same as above
				model_endog_UR_d.add D({%m})
			ENDIF

			ELSE
			' NO UNIT ROOT FOUND
			model_endog_noUR.add {%m} 
			' used to enter variables in VAR models ("stationary series")
			model_endog_noUR_c.add @cumsum({%m}) 
			' cumsum: used to be entered as endogenous variables that - when differenced (it's the cumsum) - will be equal to their original series

		ENDIF
		DELETE tmp_ur
		!o=!o+1
	next

	'///////////////////////////////////////////////////////////////////////
	' Tests for EXOGENOUS VARIABLES
	'///////////////////////////////////////////////////////////////////////
	!o=8

	for %m {%model_exog}
		matrix (1,1) tmp_ur
		SERIES emptyseries
		CALL UNITROOT_SBC_FULL({%m},!setup_maxlag,!setup_pval, tmp_ur, emptyseries)
		DELETE emptyseries
		SERIES tmpser = {%m}
		!tmpnobs = tmpser.@obs
		CALL SAVEUNITROOT(!o,%m, results_unitroot, tmp_ur, "exog",!tmpnobs)
		IF tmp_ur(1,4)=1 THEN
			' execute statement if unit root has been found (yes = 1, no = 0)
			' in this case, add no group which features the unit root endogenous variables

			' If integration order is higher than 1, lag the series so that a integration of 1 is reached. (Used for Johansen).
			!tmpintegrorder = tmp_ur(1,4)+tmp_ur(1,5)
			IF !tmpintegrorder > 1 THEN
				' Exogenous variables are less problematic than endogenous variables when estimating models
				' In VAR and VEC models, they consistently enter in their differenced form.

				model_exog_UR_d.add D({%m})
				ELSE
				' Integration of I(1)
				model_exog_UR_d.add D({%m})
			ENDIF

			ELSE
			' NO UNIT ROOT FOUND
			model_exog_noUR.add {%m} 
		ENDIF
		DELETE tmp_ur
		!o=!o+1
	next

' Save output of test

freeze(urresults)  results_unitroot

%savename = {%outdir} + "\" + {%name} + "_unitroot.csv"
urresults.save(t=csv, n="NA") %savename
DELETE urresults

' ====================================
' STEP 2: Do a Johansen cointegration test
' ====================================

''INCLUDE "d:\DATTA\Dropbox\Tilburg\Projects\GfK Singapore\SVN_GfkSingapore\analysis\code\routine_unitroot.prg" ' include 
''!setup_pval = .05 ' at which significance level should the analysis be computed?!
''!setup_maxlag=6  ' what's the maximum lag for Unit Root Tests (ADF), and calibration of lag length vor the VAR models\
''DELETE brandvar*
''CALL coint_test(model_endog_UR, model_exog_UR, model_exog_noUR, !setup_pval, !setup_maxlag)
' Call procedure ONLY if there are at least two variables with a unit root!
!varcount = model_endog_ur.@count

'@Marnik: verify entered lags for cointegration test!

IF !varcount>1 THEN
	' create seasonal dummies
	series d_q1 = (@quarter=1)-(1/4)
	series d_q2 = (@quarter=2)-(1/4)
	series d_q3 = (@quarter=3)-(1/4)

	CALL coint_test(model_endog_UR, !setup_pval, !setup_maxlag)
	%savename = {%outdir} + "\" + {%name} + "_cointegration.csv"
	brandvar_coint_table.save(t=csv, n="NA") %savename
ENDIF

'@Marnik: add trend to cointegration test?!?! No.

	'///////////////////////////////////////////////////////////////////////
	' Save rank result from test
	'///////////////////////////////////////////////////////////////////////

	'store results: 
	!varcount = model_endog_ur.@count
	matrix (!varcount, 2) brandvar_coint_result
	!startrow=15
	!j=!startrow
	!stop=0
	WHILE !j<=!startrow+!varcount-1 AND !stop=0
			%step1=brandvar_coint_table(!j,5)
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
	DELETE brandvar_coint_result

' ====================================
' STEP 3: Conduct the final analysis
' ====================================

' A) Determine which model to be estimated:


'///////////////////////////////////////////////////////////////////////
' VAR model without cointegration; possibly with only no unit root variables (differencing is done automatically)
'///////////////////////////////////////////////////////////////////////

SUBROUTINE estimate_varvec(scalar maxlag, STRING type)
	'!maxlag=3 'maxlag
	'STRING type = "vec"
	var var1_maxlag.ls 1 !maxlag model_endog_nour model_endog_ur_d  @ d_q1 d_q2 d_q3 d_q4 model_exog_nour model_exog_ur_d @trend
	' Now, determine lag length, based on SBC (=BIC)
	var1_maxlag.laglen(!maxlag, vname=results_laglength) ' result of SBC test's lag length stored in results_laglength
	!laglength_sic=results_laglength(4,1) ' create variable which only contains lag length
	delete results_laglength

'	IF type=="var" THEN
		' Reestimate var model on selected lag length
		var var1_selectedlag.ls 1 !laglength_sic model_endog_nour model_endog_ur_d  @ model_exog_nour model_exog_ur_d d_q1 d_q2 d_q3 d_q4 @trend
		'Derive impulse response
'	ENDIF

	'IF type=="vec" THEN
	'restrict to cointegration rank 1
	'	var vec1_selectedlag.ec(c, 1) 1 !laglength_sic model_endog_nour_c model_endog_ur  @ model_exog_nour model_exog_ur_d d_q1 d_q2 d_q3 d_q4 @trend
		'Derive impulse response
	'ENDIF

	' all non-UR / non-differenced variables will be used for accumulated response
	STRING imp_resp_accum = model_endog_ur_d.@members
	' all UR processess will be given in levels (they ARE already accumulated)
	STRING imp_resp_level = model_endog_nour.@members
	' responses to shocks in all endogenous variables
	GROUP tmp model_endog_ur_d model_endog_nour 'model_exog_ur_d 'model_exog_nour 
	STRING imp_impul = tmp.@members

	' Impulse
	IF model_endog_ur_d.@count>0 THEN
		freeze(results_irf_accum) var1_selectedlag.impulse(12,t, a, imp=gen, se=a) {imp_resp_accum} @ {imp_impul}
		%savename = {%outdir} + "\" + {%name} + "_irf_accum.csv"
		results_irf_accum.save(t=csv, n="NA") %savename
	ENDIF

	IF model_endog_nour.@count>0 THEN
		freeze(results_irf_level) var1_selectedlag.impulse(12,t, imp=gen, se=a) {imp_resp_level} @ {imp_impul}
		%savename = {%outdir} + "\" + {%name} + "_irf_level.csv"
		results_irf_level.save(t=csv, n="NA") %savename
	ENDIF
	' I cannot enter exogenous variables here; the (rather long) fix is to specify my own estimates of shokcing coefficients to derive the impulse response functions.
ENDSUB

IF (results_coint_rank==0) THEN
	CALL estimate_varec(!setup_maxlag)
ENDIF


' ====================================
' STEP X: Exit Eviews
' ====================================

IF %0 = "" THEN
	'nothing. end here. do not close. it didn't run from the command prompt.
ELSE
	wfclose(noerr)
	exit
ENDIF


