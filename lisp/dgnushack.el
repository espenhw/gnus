;;; dgnushack.el --- a hack to set the load path for byte-compiling
;; Copyright (C) 1994, 1995, 1996, 1997, 1998, 1999, 2000
;;        Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
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

(defalias 'facep 'ignore)

(require 'cl)

(defvar srcdir (or (getenv "srcdir") "."))

(push (or (getenv "lispdir") 
	  "/usr/share/emacs/site-lisp")
      load-path)

(push (or (getenv "W3DIR") (expand-file-name "../../w3/lisp/" srcdir)) 
      load-path)

(unless (featurep 'xemacs)
  (define-compiler-macro last (&whole form x &optional n)
    (if (and (fboundp 'last)
	     (subrp (symbol-function 'last)))
	form
      (if n
	  `(let* ((x ,x)
		  (n ,n)
		  (m 0)
		  (p x))
	     (while (consp p)
	       (incf m)
	       (pop p))
	     (if (<= n 0)
		 p
	       (if (< n m)
		   (nthcdr (- m n) x)
		 x)))
	`(let ((x ,x))
	   (while (consp (cdr x))
	     (pop x))
	   x))))
  )

;; If we are building w3 in a different directory than the source
;; directory, we must read *.el from source directory and write *.elc
;; into the building directory.  For that, we define this function
;; before loading bytecomp.  Bytecomp doesn't overwrite this function.
(defun byte-compile-dest-file (filename)
  "Convert an Emacs Lisp source file name to a compiled file name.
 In addition, remove directory name part from FILENAME."
  (setq filename (byte-compiler-base-file-name filename))
  (setq filename (file-name-sans-versions filename))
  (setq filename (file-name-nondirectory filename))
  (if (memq system-type '(win32 w32 mswindows windows-nt))
      (setq filename (downcase filename)))
  (cond ((eq system-type 'vax-vms)
	 (concat (substring filename 0 (string-match ";" filename)) "c"))
	((string-match emacs-lisp-file-regexp filename)
	 (concat (substring filename 0 (match-beginning 0)) ".elc"))
	(t (concat filename ".elc"))))

(require 'bytecomp)

(push srcdir load-path)
(load (expand-file-name "lpath.el" srcdir) nil t)

(defalias 'device-sound-enabled-p 'ignore)
(defalias 'play-sound-file 'ignore)
(defalias 'nndb-request-article 'ignore)
(defalias 'efs-re-read-dir 'ignore)
(defalias 'ange-ftp-re-read-dir 'ignore)
(defalias 'define-mail-user-agent 'ignore)

(eval-and-compile
  (unless (featurep 'xemacs)
    (defalias 'get-popup-menu-response 'ignore)
    (defalias 'event-object 'ignore)
    (defalias 'x-defined-colors 'ignore)
    (defalias 'read-color 'ignore)))

(defun dgnushack-compile (&optional warn)
  ;;(setq byte-compile-dynamic t)
  (unless warn
    (setq byte-compile-warnings
	  '(free-vars unresolved callargs redefine)))
  (unless (locate-library "cus-edit")
    (error "You do not seem to have Custom installed.
Fetch it from <URL:http://www.dina.kvl.dk/~abraham/custom/>.
You also then need to add the following to the lisp/dgnushack.el file:

     (push \"~/lisp/custom\" load-path)

Modify to suit your needs."))
  (let ((files (directory-files srcdir nil "^[^=].*\\.el$"))
	;;(byte-compile-generate-call-tree t)
	file elc)
    (dolist (file '("dgnushack.el" "lpath.el"))
      (setq files (delete file files)))
    (when (featurep 'base64)
      (setq files (delete "base64.el" files)))
    (condition-case code
	(require 'w3-forms)
      (error
       (message "No w3: %s %s" code (locate-library "w3-forms"))
       (dolist (file '("nnweb.el" "nnlistserv.el" "nnultimate.el"
		       "nnslashdot.el" "nnwarchive.el" "webmail.el"
		       "nnwfm.el"))
	 (setq files (delete file files)))))
    (dolist (file 
	     (if (featurep 'xemacs)
		 '("md5.el" "smiley-ems.el")
	       '("gnus-xmas.el" "gnus-picon.el" "messagexmas.el" 
		 "nnheaderxm.el" "smiley.el")))
      (setq files (delete file files)))

    (dolist (file files)
      (setq file (expand-file-name file srcdir))
      (when (and (file-exists-p 
		  (setq elc (concat (file-name-nondirectory file) "c")))
		 (file-newer-than-file-p file elc))
	(delete-file elc)))
    
    (while (setq file (pop files))
      (setq file (expand-file-name file srcdir))
      (when (or (not (file-exists-p 
		      (setq elc (concat (file-name-nondirectory file) "c"))))
		(file-newer-than-file-p file elc))
	(ignore-errors
	  (byte-compile-file file))))))

(defun dgnushack-recompile ()
  (require 'gnus)
  (byte-recompile-directory "." 0))

;;; dgnushack.el ends here

