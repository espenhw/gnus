;;; dgnushack.el --- a hack to set the load path for byte-compiling
;; Copyright (C) 1994,95,96 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@ifi.uio.no>
;; Version: 4.19
;; Keywords: news, path

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

(require 'cl)
(setq load-path (cons "." load-path))

(setq custom-file "/THIS FILE DOES NOT eXiST!")

(defun dgnushack-compile ()
  ;(setq byte-compile-dynamic t)
  (let ((files (directory-files "." nil ".el$"))
	(xemacs (string-match "XEmacs" emacs-version))
	byte-compile-warnings file)
    (while files
      (setq file (car files)
	    files (cdr files))
      (cond 
       ((or (string= file "custom.el") (string= file "browse-url.el"))
	(setq byte-compile-warnings nil))
       (xemacs
	(setq byte-compile-warnings 
	      '(free-vars unresolved callargs redefine)))
       (t
	(setq byte-compile-warnings 
	      '(free-vars unresolved callargs redefine obsolete))))
      (when (or (not (member file '("gnus-xmas.el" "gnus-picon.el" 
				    "messagexmas.el" "nnheaderxm.el"
				    "smiley.el")))
		xemacs)
	(condition-case ()
	    (byte-compile-file file)
	  (error nil))))))

(defun dgnushack-recompile ()
  (require 'gnus)
  (byte-recompile-directory "." 0))

;;; dgnushack.el ends here  

