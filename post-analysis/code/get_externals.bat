CALL RMDIR ..\externals /S /Q
CALL RMDIR ..\temp /S /Q
CALL mkdir ..\externals
CALL mkdir ..\temp
copy ..\..\analysis\temp\preclean*.* ..\externals\
copy ..\..\analysis\output\*.csv ..\externals\
copy ..\..\analysis\output\*.txt ..\externals\
copy ..\..\analysis\output\*.html ..\externals\
pause