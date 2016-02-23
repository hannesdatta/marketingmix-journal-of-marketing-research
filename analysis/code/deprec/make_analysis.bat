REM ****************************************************
REM * make.bat: double-click to run all scripts
REM ****************************************************


SET LOG=..\output\make.log


REM LOG START
ECHO make.bat started	>%LOG%
ECHO %DATE%		>>%LOG%
ECHO %TIME%		>>%LOG%
dir ..\output\ >>%LOG%


REM DELETE OUTPUT & TEMP FILES
RMDIR ..\..\..\temp /S /Q
mkdir ..\..\..\temp

RMDIR ..\temp /S /Q
mkdir ..\temp
RMDIR ..\output /S /Q
mkdir ..\output


REM GET_EXTERNALS
REM CALL get_externals	..\external\ >>%LOG% 2>&1
REM COPY %LOG%+get_externals.log %LOG%
REM DEL get_externals.log


REM "%uvt_r_bin32%Rterm.exe" --vanilla --args <"analysis.R" > %LOG%
"%uvt_r_bin%Rterm.exe" --vanilla --args <"analysis.R" 
REM "%uvt_r_bin%Rterm.exe" --vanilla --args <"reporting.R"

REM > %LOG%

REM Clean up temp files
RMDIR ..\..\..\temp /S /Q
mkdir ..\..\..\temp

REM LOG END
ECHO make.bat completed	>>%LOG%
ECHO %DATE%				>>%LOG%
ECHO %TIME%				>>%LOG%

COPY %LOG%+ *.log %LOG%
DEL *.log

REM CLOSE LOG
ECHO make.bat completed	>>%LOG%
ECHO %DATE%		>>%LOG%
ECHO %TIME%		>>%LOG%

PAUSE
TIMEOUT 60
EXIT
