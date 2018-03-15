CALL RMDIR ..\external /S /Q
CALL RMDIR ..\temp /S /Q
CALL mkdir ..\external
CALL mkdir ..\temp
copy ..\..\analysis\output\*.* ..\externals\
pause