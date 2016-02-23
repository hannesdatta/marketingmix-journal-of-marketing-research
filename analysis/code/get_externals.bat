set svnpath=https://svn.uvt.nl/tisem-datta/DATTA_GfkSingapore
RMDIR ..\external /S /Q
mkdir ..\external
mkdir ..\external\datasets

REM DATA FROM REPOSITORY 
set rev=HEAD
REM alternatively, rev=1 (e.g., revision 1)
REM svn export "%svnpath%/derived/out/@%rev%" "../external/datasets/" --force -r %rev%
pause