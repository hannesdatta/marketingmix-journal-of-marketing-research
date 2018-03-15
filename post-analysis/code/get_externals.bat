CALL RMDIR ..\externals /S /Q
CALL RMDIR ..\temp /S /Q
CALL mkdir ..\externals
CALL mkdir ..\temp
copy ..\..\analysis\output\*.* ..\externals\
pause