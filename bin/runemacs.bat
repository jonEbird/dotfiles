set BASE=C:\Users\jsmiller
set EMACSVERSION=24.2
REM
REM
REM set "HOME=%BASE%\soft\config"
set HOME=U:\
cd %HOME%
REM Note, we actually start in the HOME directory via the shortcut
del /P "*.lock"
REM set PATH=%PATH%;"C:\Program Files (x86)\Git\bin\"
set PATH=%PATH%;%BASE%\software\emacs-%EMACSVERSION%\bin\
set PATH=%PATH%;%BASE%\Downloads\
REM
REM pageant.exe "C:\Documents and Settings\millej95\soft\putty_keys\private.ppk"
REM runemacs.exe
runemacs.exe -mm
pause
