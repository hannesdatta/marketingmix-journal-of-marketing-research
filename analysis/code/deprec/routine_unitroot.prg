' Checks minimum no. of obs. per variable and, if necessary, removes it
SUBROUTINE VERIFY_OBS(STRING %checkstring, STRING %savestring, SCALAR minob)
	for %loopvar {%checkstring}
		SERIES countvar = {%loopvar}
		SCALAR numberofobs = countvar.@obs
		IF numberofobs > minob THEN 
			%savestring= %savestring+ %loopvar + " "
		ENDIF
	delete countvar
	next
ENDSUB


' Auxilary function to build lag structure to estimate ADF test
SUBROUTINE LOCAL BUILD_LAGS(SCALAR laglength, STRING %return)
	%varname="varname"
	!length=laglength
	!i=1
	%return = ""
	WHILE !i<=!length
		%is=@str(!i)
		%lagstring=%varname+"(-"+%is+")"
		%return = %return + "D(" + %lagstring + ")" + " "
	 	!i=!i+1
	WEND
	' returns result of lag length in the variable %return
ENDSUB

SUBROUTINE LOCAL RUN_ADF(SERIES varname, STRING %lagterms, SCALAR pval, MATRIX resultmatrix, SCALAR offset_lag)
'		' runs the ADF regression on %varname, with %return lagged terms, and a p-value of pval
		' stores results in a 1x4 matrix
		matrix(1,3) resultmatrix 'tmp_adf_results
 		
		' It is crucial to only estimate the series on the set of observations that is NON-MISSING, plus
		' accounting for the proper lag structure.

		' Are there any missings at the beginning of the series, then, if... at what observation does it start?
		smpl @all
		series temp = @recode(varname<>na,@trend,na)
		!first = @min(temp)
		'' local smpl
		smpl @first+!first+offset_lag @last
		equation tmp_eq.ls d(varname) c varname(-1) {%lagterms} @trend("2004M1") @expand(@quarter, @drop(1))
		'equation tmp_eq_!lags.ls d({%varname}) c {%varname}(-1) {%return} @trend("2004M1") '@expand(@month, @drop(1))
		
		' structure of results table: SBC, t-stat
		 resultmatrix(1, 1) = tmp_eq.@schwarz
		 resultmatrix(1, 2) = tmp_eq.@tstats(2) '+!lags
		' determine significance
		IF pval=.05 THEN 
			!teststat = -3.454031582977846
		ENDIF

		IF pval=.1 THEN 
			!teststat = -3.152652017511932
		ENDIF

		'' test for significance
		IF tmp_eq.@tstats(2)<=!teststat THEN '+!lags
				!unitroot_result=0 'no unit root
				ELSE
				!unitroot_result =1'unit root
		ENDIF
		 resultmatrix(1, 3) =!unitroot_result 
 		smpl @all
ENDSUB

SUBROUTINE LOCAL UNITROOT_SBC(SERIES varname, SCALAR maxlag, SCALAR pval, SCALAR order, MATRIX tmp_result_ur)
	' Testing for Unit Roots, using the Augmented-Dickey-Fuller test
	' and automatic lag-length, based on BIC/SBC.

	' ORDER: whether unit root is tested on levels (0), or first-/second differences (1/2).
	IF order = 0 THEN
		SERIES VARNAME_ur = varname
	ENDIF
	
	IF order = 1 THEN
		SERIES VARNAME_ur = D(varname)
	ENDIF

	IF order = 2 THEN
		SERIES VARNAME_ur = D(D(varname))
	ENDIF

	%varname = %VARNAMEX+"_ur"

	' Run all tests for all lags, save t-values for lagged regressor (ADF test statistic)
	matrix(maxlag+1,4) tmp_unitroottest
	!j=0
	' restrict sample, so that all models are estimated on the same number of observations
	WHILE !j<=maxlag
		!lags = maxlag - !j
		CALL BUILD_LAGS(!lags, %return_buildlags)
		matrix(1,1) tmp_adf_results
		CALL RUN_ADF(VARNAME_ur, %return_buildlags, pval, tmp_adf_results, maxlag+order)
		scalar save_max=VARNAME_ur.@obs
		tmp_unitroottest(!j+1, 1) = !lags
		tmp_unitroottest(!j+1, 2) = tmp_adf_results(1,1)
		tmp_unitroottest(!j+1, 3) = tmp_adf_results(1,2)
		tmp_unitroottest(!j+1, 4) = tmp_adf_results(1,3)
		!j=!j+1
	WEND
	
	' SELECT model with MIN Schwartz
	!j=2
	!target = !j-1 ' set 1 to current mimimum, and then find another minimum
	!minvalue = tmp_unitroottest(!j-1,2)

	WHILE !j<=maxlag+1 
		!testval=  tmp_unitroottest(!j,2)
		IF !testval<!minvalue THEN
			!minvalue = !testval
			!target=!j
		ENDIF
		!j=!j+1
	WEND
	
	' store result of test
	''SCALAR final = !target
	' run test again, on the unrestricted sample
	'' smpl @all
	CALL BUILD_LAGS(tmp_unitroottest(!target,1),%return)
	matrix(1,1) tmp_adf_results
	CALL RUN_ADF(VARNAME_ur, %return, pval, tmp_adf_results, tmp_unitroottest(!target,1))

	matrix (1,5) tmp_result_ur
	tmp_result_ur(1,1)=  tmp_unitroottest(!target,1)
	tmp_result_ur(1,2)= tmp_adf_results(1,1)
	tmp_result_ur(1,3)= tmp_adf_results(1,2)
	tmp_result_ur(1,4)= tmp_adf_results(1,3)
	tmp_result_ur(1,5)=order
	
	'' DELETE tmp_unitroottest
	'' DELETE tmp_adf_results
	'DELETE tmp_eq
	'DELETE {%VARNAMEX}_ur
ENDSUB

SUBROUTINE LOCAL UNITROOT_SBC_FULL(SERIES varname, SCALAR maxlag, SCALAR pval, MATRIX result, SERIES tmp_varname)
	' Routine to test for unit roots, of order up to !maxorder-1.
	' This is the main routine to call!
	!maxorder = 3
	!j=1
	!stop=0
	matrix(1,5) tmp_oldresults
	SERIES tmp_varname
	tmp_varname = varname
	WHILE !j<=!maxorder AND !stop<1
		!index=!j-1
		SCALAR indexv = !j
		matrix(1,1) tmp_result_ur
		CALL UNITROOT_SBC(tmp_varname, maxlag, pval,!index, tmp_result_ur)

		if !index = 0 THEN
			tmp_oldresults = tmp_result_ur
		ENDIF
			
		IF tmp_result_ur(1,4)=0 then 
			!stop = 1 ' Unit root rejected. If unit root from previous step has been found, take these results. 
			tmp_result_ur = tmp_oldresults
			ELSE
			' Unit root has been found. Go on and try to find one at a higher order of integration.
			!j=!j+1
			tmp_oldresults = tmp_result_ur
		ENDIF
	WEND
	result = tmp_result_ur
	'DELETE tmp_oldresults
	'DELETE tmp_result_ur
ENDSUB


SUBROUTINE LOCAL SAVEUNITROOT(SCALAR indexvec, STRING %varname, TABLE results_unitroot, MATRIX tmp_result_ur, STRING %vartype, SCALAR nobs)
	!k=indexvec+1
	' results_unitroot: COLUMN NAMES: lags, sbc, tstat, unit root
	results_unitroot(!k,1)=tmp_result_ur(1,1)
	results_unitroot(!k,2)=tmp_result_ur(1,2)
	results_unitroot(!k,3)=tmp_result_ur(1,3)
	results_unitroot(!k,4)=tmp_result_ur(1,4)
	results_unitroot(!k,5)=tmp_result_ur(1,5)
	results_unitroot(!k,6)=tmp_result_ur(1,5)+tmp_result_ur(1,4)
	results_unitroot(!k,7)=%varname
	results_unitroot(!k,8)=%vartype 'exog vs. endog
	results_unitroot(!k,9)=nobs 'nobs
	
'	IF tmp_result_ur(1,4)=1 THEN	'unit root found
'		SERIES {%varname}_regr=d({%varname})
'	ELSE
'		SERIES {%varname}_regr={%varname}
'	ENDIF
ENDSUB

''CALL UNITROOT_SBC("unitsales", 1, .5)





'''''''''''''''''''''''''''''''''''''''''''''''''''''''''' JOHANSEN COINTEGRATION PART ''''''''''''''''''''''''''''''''''''''

SUBROUTINE COINT_TEST(GROUP endogvar1, SCALAR pval, SCALAR maxlag)
' COINTEGRATION TEST
' on variable group ''vars1''; will be set to endogenous variables which have a unit root

!tmpmaxlag=maxlag
!tmppval=pval

' First, estimate VAR model to determine lag length
var brandvar_maxlag.ls 1 !tmpmaxlag endogvar1 @ d_q1 d_q2 d_q3
' Now, determine lag length, based on SBC (=BIC)
brandvar_maxlag.laglen(!tmpmaxlag, vname=results_laglength) ' result of SBC test's lag length stored in results_laglength
!laglength_sic=results_laglength(4,1) ' create variable which only contains lag length
delete results_laglength

' Reestimate var model on selected lag length
var brandvar_selectedlag.ls 1 !laglength_sic endogvar1 @ d_q1 d_q2 d_q3 'exogvar1 exogvar2
'{%model_exog}  no exogenous variables, no trends here...

' STEP 3: Test for cointegration
var brandvar_coint.ls 1 !laglength_sic model_endog_ur @ d_q1 d_q2 d_q3
' verify lag length

'var brandvar_coint.ls 1 !laglength_sic model_endog_ur @ d_q1 d_q2 d_q3 d_q4 
freeze(brandvar_coint_table) brandvar_coint.coint(c,!laglength_sic,cvsize=!setup_pval,save=brandvar_coint_res) 

''delete brandvar_maxlag
''delete brandvar_selectedlag

ENDSUB


