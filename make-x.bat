@echo off

rem Modified once more by Frank Schmitt (ich@Frank-Schmitt.net)
rem Modified by ShengHuo Zhu (zsh@cs.rochester.edu)
rem Originally from make.bat by David Charlap (shamino@writeme.com)
rem

rem Clear PWD so emacs doesn't get confused
set GNUS_PWD_SAVE=%PWD%
set PWD=

if "%1" == "" goto usage

set emacs=xemacs.exe

cd lisp
call %1\%emacs% -batch -q -no-site-file -l ./dgnushack.el -f dgnushack-compile
if not "%2" == "copy" goto info
attrib -r %1\..\..\xemacs-packages\lisp\gnus\*.*
copy *.el* %1\..\..\xemacs-packages\lisp\gnus

:info
set EMACSINFO=call %1\%emacs% -no-site-file -no-init-file -batch -q -l infohack.el -f batch-makeinfo
cd ..\texi
%EMACSINFO% message.texi
%EMACSINFO% emacs-mime.texi
%EMACSINFO% gnus.texi
if not "%2" == "copy" goto done
copy gnus %1\..\..\xemacs-packages\info
copy gnus-?? %1\..\..\xemacs-packages\info
copy message %1\..\..\xemacs-packages\info
copy emacs-mime %1\..\..\xemacs-packages\info

:etc
cd ..\etc
copy gnus-tut.txt %1\..\..\xemacs-packages\etc

:done
cd ..
goto end

:usage
echo Usage: make :xemacs-dir: [copy]
echo.
echo where: :xemacs-dir: is the directory you installed xemacs in 
echo                     (the directory where xemacs.exe is situated)
echo                    eg. C:\Programme\XEmacs\XEmacs-21.4.3\i586-pc-win32
echo        copy indicates that the compiled files should be copied to your
echo             emacs lisp, info, and etc directories
echo.
echo Note: If you have Emacs/w3 you should set the environment variable 
echo       W3DIR to the directory where w3 is installed eg.
echo                 set W3DIR=C:\Progra~1\XEmacs\xemacs-packages\lisp\w3

rem Restore PWD so whoever called this batch file doesn't get confused
set PWD=%GNUS_PWD_SAVE%
set GNUS_PWD_SAVE=
:end
