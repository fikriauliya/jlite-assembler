@echo off
set OCAMLRUNPARAM=b
:START
call make.bat
echo.
echo.
call exec.bat
goto START
