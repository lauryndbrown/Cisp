cls
@ECHO OFF
for /f "delims=" %%i in ('dir /b  "..\test\*.lisp"') do (
    ECHO cisp.exe ..\test\%%i
    TYPE ..\test\%%i
    CALL cisp.exe ..\test\%%i
    ECHO --------------------------
)
PAUSE
EXIT
