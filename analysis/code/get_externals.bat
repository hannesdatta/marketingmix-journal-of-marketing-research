@echo off

ECHO.
ECHO ...............................................
ECHO PRESS 1 or 2 to select your task, or 3 to EXIT.
ECHO ...............................................
ECHO.
ECHO 1 - Retrieve externals from S3
ECHO 2 - Retrieve externals from \derived (if you have built the project completely on your system)
ECHO 3 - EXIT
ECHO.

SET /P M=Type 1, 2, or 3 then press ENTER:
IF %M%==1 GOTO AWS
IF %M%==2 GOTO RAW
IF %M%==3 GOTO EOF


:AWS
RMDIR ..\external /S /Q
mkdir ..\external
echo Download externals from AWS S3

REM curl -o ../external/matching_stats.txt https://s3.eu-central-1.amazonaws.com/uvt-spotify/builds/pre-analysis/2016_09_18/matching_stats.txt -k

GOTO EXIT_PART

:RAW
RMDIR ..\external /S /Q
mkdir ..\external
 
echo Copy externals from local repository
copy ..\..\derived\output\*.csv ..\external\

GOTO EXIT_PART

:EXIT_PART
pause
