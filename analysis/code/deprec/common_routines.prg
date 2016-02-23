' Routines for the GfK Singapore Project



SUBROUTINE UNITROOT_tstat(STRING %varname, SCALAR maxlag, SCALAR pval)
	!j=0
	!stop=0
	!tstat = 1.959964
	if pval=.05 THEN 
		!tstat= 1.959964
	ENDIF
	if pval=.1 THEN 
		!tstat= 1.644854
	ENDIF

	WHILE !j<=maxlag and !stop=0
		!lags=maxlag-!j
		CALL BUILD_LAGS(%varname, !lags)
		equation tmp_eq.ls d({%varname}) c {%varname}(-1) {%return} @trend("2004M1") @expand(@month, @drop(1))
		' stop if lag is significant
		'SCALAR statsave = tmp_eq.@tstats(2+!lags)
		!n=tmp_eq.@tstats(2+!lags)
		if @abs(!n)>!tstat THEN
			' lag is significant; save!
			!stop=1
			ELSE
			!j=!j+1
		ENDIF

	WEND
	IF pval=.05 THEN
		IF tmp_eq.@tstats(2)<=-3.454031582977846 THEN
			' it's a test on the lagged DV term.
			' if it's SIGNIFICANT, it means that the NULL of unit root is rejected.
			!unitroot_result = 0 ' no unit root
			ELSE
			!unitroot_result = 1 ' unit root
		ENDIF
	ENDIF

	IF pval=.1 THEN
		IF tmp_eq.@tstats(2)<=-3.152652017511932 THEN
			!unitroot_result = 0
			ELSE
			!unitroot_result = 1
		ENDIF
	ENDIF
	matrix (1,3) result
	result(1,1)=!unitroot_result
	result(1,2)= tmp_eq.@tstats(2)
	result(1,3)=maxlag-!j
ENDSUB



SUBROUTINE SAVEUNITROOT(SCALAR indexvec, STRING %varname)
	!k=indexvec
	results_unitroot(!k,1)=result(1,1)
	results_unitroot(!k,2)=result(1,2)
	results_unitroot(!k,3)=result(1,3)
	IF result(1,1)=1 THEN	'unit root found
		SERIES {%varname}_regr=d(log({%varname}+1))
	ELSE
		SERIES {%varname}_regr=log({%varname}+1)
	ENDIF
ENDSUB


