REM Fetch data/results from previous modules

CALL RMDIR ..\externals /S /Q
CALL RMDIR ..\temp /S /Q
CALL RMDIR ..\output /S /Q
CALL mkdir ..\externals
CALL mkdir ..\temp
CALL mkdir ..\output

copy ..\..\analysis\temp\preclean*.* ..\externals\
copy ..\..\analysis\output\*.csv ..\externals\
copy ..\..\analysis\output\*.txt ..\externals\
copy ..\..\analysis\output\*.html ..\externals\

REM Pause script to view whether it has processed everything well

pause