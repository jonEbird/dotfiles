set "HOME=C:\Documents and Settings\millej95\Soft\emacs24\config"
REM Note, we actually start in the HOME directory via the shortcut
del /P "*.lock"
set PATH=%PATH%;C:\Program Files\Git\bin\
set PATH=%PATH%;C:\Documents and Settings\millej95\Soft\emacs24\Emacs\bin\
set PATH=%PATH%;C:\Documents and Settings\millej95\Soft\miktex\bin\
set PATH=%PATH%;C:\Documents and Settings\millej95\My Documents\Downloads\
set PATH=%PATH%;C:\Documents and Settings\millej95\Soft\utility\bin\
REM
REM pageant.exe "C:\Documents and Settings\millej95\soft\putty_keys\private.ppk"
REM runemacs.exe
runemacs.exe -mm
REM pause
REM
REM
REM set HOME=%CD%\config
REM %CD%\ntemacs23\bin\runemacs.exe
REM set PATH=C:\Program Files\Git\bin\;%PATH%
