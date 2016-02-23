'' CALL UNITROOT_SBC_FULL(ser01log,!maxlag,.05,tmp_ur)
CALL UNITROOT_SBC(ser01log,!maxlag,.05,0,tmp_ur)
'' smpl @all
'' scalar obstest = ser01log.@obs
'' STRING obstest = ser01log.@first
smpl @all


'' smpl @first+6+12 @last '' varname.@first+maxlag + varname.@last
smpl ser01log.@first @last
STRING smpl2 = ser01log.@first
smpl @dateval(smpl2, "yyyyfmm") @last

smpl @dateval("200407", "YYYYMM") @last
@dateval("12/1/1999", "dd/mm/yyyy")



smpl @dateval(ser01log.@first, "yyyyfmm") @last

SCALAR test = @dateval(ser01log.@first, "yyyyfmm")
SCALAR test2 = @dateval(unitsales.@first, "yyyyfmm")


%ser = "ser01log"
smpl @all
series temp = @recode({%ser}<>na,@trend,na)+1 
scalar first = @min(temp)
scalar last = @max(temp)
%firstdate=@otod(first)
%lastdate=@otod(last) 
STRING firstdate=%firstdate
STRING lastdate =%lastdate

smpl @first+first @last



%return=""

INCLUDE "d:\DATTA\Dropbox\Tilburg\Projects\GfK Singapore\SVN_GfkSingapore\analysis\code\routine_unitroot.prg" ' include 
!maxlag=1
matrix (1,1) tmp_ur
CALL BUILD_LAGS(!maxlag, %return)
matrix(1,1) resultmatrix
CALL RUN_ADF(ser01log, %return, .05, resultmatrix,1)
