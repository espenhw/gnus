@echo off

rem Modified by ShengHuo Zhu (zsh@cs.rochester.edu)
rem Originally from make.bat by David Charlap (shamino@writeme.com)
rem

rem Clear PWD so emacs doesn't get confused
set GNUS_PWD_SAVE=%PWD%
set PWD=

if "%1" == "" goto usage
if "%2" == "" goto usage

set emacs=xemacs.exe

cd lisp
call %1\%2\%emacs% -batch -q -no-site-file -l ./dgnushack.el -f dgnushack-compile
if not "%3" == "copy" goto info
attrib -r %1\lisp\gnus\*
copy *.el* %1\lisp\gnus

:info
set EMACSINFO=call %1\%2\%emacs% -no-site-file -no-init-file -batch -q -l infohack.el -f batch-makeinfo
cd ..\texi
%EMACSINFO% message.texi
%EMACSINFO% emacs-mime.texi
%EMACSINFO% gnus.texi
if not "%3" == "copy" goto done
copy gnus %1\info
copy gnus-?? %1\info
copy message %1\info
copy emacs-mime %1\info

:etc
cd ..\etc
copy gnus-tut.txt %1\etc

:done
cd ..
goto end

:usage
echo Usage: make :xemacs-dir: :xemacs-arch: [copy]
echo.
echo where: :xemacs-dir: is the directory you installed xemacs in
echo                    eg. C:\Progra~1\XEmacs\XEmacs-21.4.3
echo        :xemacs-arch: is the xemacs architecture you installed
echo                    eg. i586-pc-win32
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
