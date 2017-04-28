cls
@ECHO OFF
SET test_dir=..\test\
ECHO %test_dir%
for /f  "delims=" %%d in ('dir /b %test_dir%') do (
ECHO --------------------------
ECHO Test Directory: %%d
    for /f  "delims=" %%f in ('dir /b "%test_dir%%%d"') do (
        ECHO File:%test_dir%%%d\%%f
        TYPE %test_dir%%%d\%%f 
        CALL cisp.exe %test_dir%%%d\%%f
    )
)
PAUSE
EXIT
