@echo off

rem Modified once more by Frank Schmitt (ich@Frank-Schmitt.net)
rem Modified by ShengHuo Zhu (zsh@cs.rochester.edu)
rem Originally from make.bat by David Charlap (shamino@writeme.com)

rem Clear PWD so emacs doesn't get confused
set GNUS_PWD_SAVE=%PWD%
set PWD=

if "%1" == "" goto usage

rem Directory where the info files are installed
set GNUS_INFO_DIR=%1\..\..\xemacs-packages\info

set emacs=xemacs.exe
if "%2" == "" set copy="false"
if "%2" == "copy" set copy=true
if "%2" == "/copy" set copy=true

set EMACSBATCH=call %1\%emacs% -batch -q -no-site-file

cd lisp
%EMACSBATCH% -l ./dgnushack.el -f dgnushack-compile
if not %copy%==true goto info
attrib -r %1\..\..\xemacs-packages\lisp\gnus\*.*
copy *.el? %1\..\..\xemacs-packages\lisp\gnus

:info
set EMACSINFO=%EMACSBATCH% -l infohack.el -f batch-makeinfo
cd ..\texi
%EMACSINFO% message.texi
%EMACSINFO% emacs-mime.texi
%EMACSINFO% gnus.texi
if not %copy%==true goto done
copy gnus       %GNUS_INFO_DIR%
copy gnus-?     %GNUS_INFO_DIR%
copy gnus-??    %GNUS_INFO_DIR%
copy message    %GNUS_INFO_DIR%
copy message-?  %GNUS_INFO_DIR%
copy emacs-mime %GNUS_INFO_DIR%
copy sieve      %GNUS_INFO_DIR%
copy pgg        %GNUS_INFO_DIR%
echo Maybe you should add the following line to %GNUS_INFO_DIR%\dir:
echo.
echo * PGG: (pgg).		Emacs interface to various PGP implementations.
echo * Sieve: (sieve).	Managing Sieve scripts in Emacs.
echo.

:etc
cd ..\etc
copy gnus-tut.txt %1\..\..\xemacs-packages\etc

:done
cd ..
goto end

:usage
echo Usage: make-x.bat :xemacs-dir: [/copy]
echo.
echo where: :xemacs-dir: is the directory you installed xemacs in 
echo                     (the directory where xemacs.exe is situated)
echo                    eg. C:\Programme\XEmacs\XEmacs-21.4.3\i586-pc-win32
echo        /copy indicates that the compiled files should be copied to your
echo             emacs lisp, info, and etc directories
echo.
echo Note: If you have Emacs/w3 you should set the environment variable 
echo       W3DIR to the directory where w3 is installed eg.
echo                 set W3DIR=C:\Progra~1\XEmacs\xemacs-packages\lisp\w3

rem Restore PWD so whoever called this batch file doesn't get confused
set PWD=%GNUS_PWD_SAVE%
set GNUS_PWD_SAVE=
set EMACSBATCH=
set GNUS_INFO_DIR=
:end
