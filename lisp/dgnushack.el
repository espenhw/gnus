;;; dgnushack.el --- a hack to set the load path for byte-compiling
;; Copyright (C) 1994,95,96,97 Free Software Foundation, Inc.

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

(fset 'facep 'ignore)

(require 'cl)
(require 'bytecomp)
(push "." load-path)
(push "~/lisp/custom" load-path)
(require 'lpath)

(defalias 'device-sound-enabled-p 'ignore)
(defalias 'play-sound-file 'ignore)
(defalias 'nndb-request-article 'ignore)
(defalias 'efs-re-read-dir 'ignore)
(defalias 'ange-ftp-re-read-dir 'ignore)
(defalias 'define-mail-user-agent 'ignore)

(eval-and-compile
  (unless (string-match "XEmacs" emacs-version)
    (fset 'get-popup-menu-response 'ignore)
    (fset 'event-object 'ignore)
    (fset 'x-defined-colors 'ignore)
    (fset 'read-color 'ignore)))

(setq byte-compile-warnings
      '(free-vars unresolved callargs redefine))

(defun dgnushack-compile ()
  ;;(setq byte-compile-dynamic t)
  (unless (locate-library "cus-edit")
    (error "You do not seem to have Custom installed.
Fetch it from <URL:http://www.dina.kvl.dk/~abraham/custom/>.
You also then need to add the following to the lisp/dgnushack.el file:

     (push \"~/lisp/custom\" load-path)

Modify to suit your needs."))
  (let ((files (directory-files "." nil "^[^=].*\\.el$"))
	(xemacs (string-match "XEmacs" emacs-version))
	;;(byte-compile-generate-call-tree t)
	file elc)
    (condition-case ()
	(require 'w3-forms)
      (error (setq files (delete "nnweb.el" files))))
    (while (setq file (pop files))
      (when (or (and (not xemacs)
		     (not (member file '("gnus-xmas.el" "gnus-picon.el"
					 "messagexmas.el" "nnheaderxm.el"
					 "smiley.el" "x-overlay.el"))))
		(and xemacs
		     (not (member file '("md5.el")))))
	(when (or (not (file-exists-p (setq elc (concat file "c"))))
		  (file-newer-than-file-p file elc))
	  (ignore-errors
	    (byte-compile-file file)))))))

(defun dgnushack-recompile ()
  (require 'gnus)
  (byte-recompile-directory "." 0))

;;; dgnushack.el ends here

