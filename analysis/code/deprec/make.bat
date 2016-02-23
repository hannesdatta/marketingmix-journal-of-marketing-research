REM PVALUE OF .10


REM DELETE OUTPUT & TEMP FILES
REM DEL /F /Q ..\output\*

RMDIR ..\temp /S /Q
mkdir ..\temp
mkdir ..\temp\datasets
mkdir ..\temp\results
mkdir ..\temp\run

REM get external datasets for eviews


REM unzip files
..\..\tools\zip\unzip.exe "..\external\datasets.zip" -d "..\temp\datasets"

python create_temp.py 18 "d:\DATTA\Dropbox\Tilburg\Projects\GfK Singapore\SVN_GfkSingapore\analysis\code\analysis.prg" "d:\DATTA\Dropbox\Tilburg\Projects\GfK Singapore\SVN_GfkSingapore\analysis\temp\results\" .05

REM DELETE OUTPUT & TEMP FILES
DEL /F /Q ..\output\pval10\
REM RMDIR ..\temp /S /Q
REM mkdir ..\temp

FOR %%C in ("d:\DATTA\Dropbox\Tilburg\Projects\GfK Singapore\SVN_GfkSingapore\analysis\temp\execute\*.bat") DO START "RUN" "%%C"
pause