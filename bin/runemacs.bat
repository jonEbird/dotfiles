set BASE=C:\Users\millej95
set EMACSVERSION=24.1.50
REM
REM
set "HOME=%BASE%\soft\config"
cd %HOME%
REM Note, we actually start in the HOME directory via the shortcut
del /P "*.lock"
set PATH=%PATH%;"C:\Program Files (x86)\Git\bin\"
set PATH=%PATH%;%BASE%\soft\emacs-%EMACSVERSION%\bin\
set PATH=%PATH%;%BASE%\Downloads\
REM
REM pageant.exe "C:\Documents and Settings\millej95\soft\putty_keys\private.ppk"
REM runemacs.exe
runemacs.exe -mm
REM pause
