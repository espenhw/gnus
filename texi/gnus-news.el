;;; gnus-news.el --- a hack to create GNUS-NEWS from texinfo source
;; Copyright (C)  2004  Free Software Foundation, Inc.

;; Author: Reiner Steib  <Reiner.Steib@gmx.de>
;; Keywords: tools

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(defvar gnus-news-header-disclaimer
"GNUS NEWS -- history of user-visible changes.
Copyright (C) 1999, 2000, 2001, 2002, 2003, 2004 Free Software Foundation, Inc.
See the end for copying conditions.

Please send Gnus bug reports to bugs\@gnus.org.
For older news, see Gnus info node \"New Features\".



* Changes in No Gnus

")

(defvar gnus-news-trailer
"
* For older news, see Gnus info node \"New Features\".

----------------------------------------------------------------------
Copyright information:

Copyright (C) 1999, 2000, 2001, 2002, 2003, 2004 Free Software Foundation, Inc.

   Permission is granted to anyone to make or distribute verbatim copies
   of this document as received, in any medium, provided that the
   copyright notice and this permission notice are preserved,
   thus giving the recipient permission to redistribute in turn.

   Permission is granted to distribute modified versions
   of this document, or of portions of it,
   under the above conditions, provided also that they
   carry prominent notices stating who last changed them.
\nLocal variables:\nmode: outline
paragraph-separate: \"[ 	]*$\"\nend:\n")

(defvar gnus-news-makeinfo-command "makeinfo")

(defvar gnus-news-fill-column 72)

(defvar gnus-news-makeinfo-switches
  (concat " --no-headers --paragraph-indent=0"
	  " --fill-column=" (number-to-string
			     (+ 3 ;; will strip leading spaces later
				(or gnus-news-fill-column 80)))))

(defun batch-gnus-news ()
  "Make GNUS-NEWS in batch mode."
  (let (infile outfile)
    (setq infile (car command-line-args-left)
	  command-line-args-left (cdr command-line-args-left)
	  outfile (car command-line-args-left)
	  command-line-args-left nil)
    (if (and infile outfile)
	(message "Creating `%s' from `%s'..." outfile infile)
      (error "Not enough files given."))
    (gnus-news-translate-file infile outfile)))

(defun gnus-news-translate-file (infile outfile)
  "Translate INFILE (texinfo) to OUTFILE (GNUS-NEWS)."
  (let* ((dir (concat (or (getenv "srcdir") ".") "/"))
	 (infile (concat dir infile))
	 (buffer (find-file-noselect (concat dir outfile))))
    (with-temp-buffer
      ;; Could be done using `texinfmt' stuff as in `infohack.el'.
      (insert
       (shell-command-to-string
	(concat gnus-news-makeinfo-command " "
		gnus-news-makeinfo-switches " " infile)))
      (goto-char (point-max))
      (delete-char -1)
      (goto-char (point-min))
      (save-excursion
	(while (re-search-forward "^   \\* " nil t)
	  (replace-match "** ")))
      (save-excursion
	(while (re-search-forward "^     " nil t)
	  (replace-match "")))
      (goto-char (point-min))
      (insert gnus-news-header-disclaimer)
      (goto-char (point-max))
      (insert gnus-news-trailer)
      (write-region (point-min) (point-max) outfile))))

;;; arch-tag: e23cdd27-eafd-4ba0-816f-98f5edb0dc29
;;; gnus-news.el ends here
