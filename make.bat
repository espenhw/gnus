@echo off

rem Written by David Charlap (shamino@writeme.com)
rem
rem There are two possible problems with this batch file.  The emacs.bat batch
rem file may not exist in all distributions.  It is part of the GNU build of
rem Emacs 20.4 (http://www.gnu.org/softare/emacs/windows.ntemacs.html)  If you
rem install Gnus with some other build, you may have to replace calls to
rem %1\emacs.bat with something else.
rem 
rem Also, the emacs.bat file that comes with Emacs does not accept more than 9
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

rem Emacs 20.7 no longer includes emacs.bat. Use emacs.exe if the batch file is
rem not present -- this also fixes the problem about too many parameters on Win9x.
set emacs=emacs.exe
if exist %1\bin\emacs.bat set emacs=emacs.bat

cd lisp
call %1\bin\%emacs% -batch -q -no-site-file -l ./dgnushack.el -f dgnushack-compile
if not "%2" == "copy" goto info
attrib -r %1\lisp\gnus\*
copy *.el* %1\lisp\gnus

:info
set EMACSINFOHACK="(while (re-search-forward \"@\\(end \\)?ifnottex\" nil t) (replace-match \"\"))"
cd ..\texi
call %1\bin\%emacs% -batch -q -no-site-file message.texi -eval %EMACSINFOHACK% -f texinfo-every-node-update -f texinfo-format-buffer -f save-buffer
call %1\bin\%emacs% -batch -q -no-site-file emacs-mime.texi -eval %EMACSINFOHACK% -f texinfo-every-node-update -f texinfo-format-buffer -f save-buffer
call %1\bin\%emacs% -batch -q -no-site-file gnus.texi -eval %EMACSINFOHACK% -eval "(setq max-lisp-eval-depth 600)" -f texinfo-every-node-update -f texinfo-format-buffer -f save-buffer
if not "%2" == "copy" goto done
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
echo Usage: make :emacs-dir: [copy]
echo.
echo where: :emacs-dir: is the directory you installed emacs in
echo                    eg. d:\emacs\20.4
echo        copy indicates that the compiled files should be copied to your
echo             emacs lisp, info, and etc directories
echo.
echo Note: If you have Emacs/w3 you should set the environment variable 
echo       W3DIR to the directory where w3 is installed eg.
echo                 set W3DIR=d:\lisp\w3-4.0pre46\lisp

rem Restore PWD so whoever called this batch file doesn't get confused
set PWD=%GNUS_PWD_SAVE%
set GNUS_PWD_SAVE=
:end
