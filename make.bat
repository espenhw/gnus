@echo off

rem Written by David Charlap <shamino@writeme.com>

rem There are two catches, however.  The emacs.bat batch file may not exist
rem in all distributions.  It is part of the Voelker build of Emacs 19.34
rem (http://www.cs.washington.edu/homes/voelker/ntemacs.html).  If the user
rem installs Gnus with some other build, he may have to replace calls to
rem %1\emacs.bat with something else.
rem 
rem Also, the emacs.bat file that Voelker ships does not accept more than 9
rem parameters, so the attempts to compile the .texi files will fail.  To
rem fix that (at least on NT.  I don't know about Win95), the following
rem change should be made to emacs.bat:
rem 
rem     %emacs_dir%\bin\emacs.exe %1 %2 %3 %4 %5 %6 %7 %8 %9
rem 
rem should become
rem 
rem     %emacs_dir%\bin\emacs.exe %*
rem 
rem which will allow the batch file to accept an unlimited number of
rem parameters.

rem Clear PWD so emacs doesn't get confused
set GNUS_PWD_SAVE=%PWD%
set PWD=

if "%1" == "" goto usage

cd lisp
call %1\bin\emacs.bat -batch -q -no-site-file -l ./dgnushack.el -f dgnushack-compile
if not "%2" == "copy" goto info
copy *.el* %1\lisp

:info
cd ..\texi
call %1\bin\emacs.bat -batch -q -no-site-file gnus.texi -l texinfmt -f texinfo-every-node-update -f texinfo-format-buffer -f save-buffer
call %1\bin\emacs.bat -batch -q -no-site-file message.texi -l texinfmt -f texinfo-every-node-update -f texinfo-format-buffer -f save-buffer
if not "%2" == "copy" goto done
copy gnus %1\info
copy gnus-?? %1\info
copy message %1\info

:etc
cd ..\etc
copy gnus-tut.txt %1\etc

:done
cd ..
goto end

:usage
echo Usage: make ^<emacs-dir^> [copy]
echo.
echo where: ^<emacs-dir^> is the directory you installed emacs in
echo                    eg. d:\emacs\19.34
echo        copy indicates that the compiled files should be copied to your
echo             emacs lisp, info, and etc directories

rem Restore PWD so whoever called this batch file doesn't get confused
set PWD=%GNUS_PWD_SAVE%
set GNUS_PWD_SAVE=
:end
