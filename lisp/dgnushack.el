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

(fset 'facep 'ignore)

(require 'cl)
(push "." load-path)

(defalias 'device-sound-enabled-p 'ignore)
(defalias 'play-sound-file 'ignore)
(defalias 'nndb-request-article 'ignore)
(defalias 'efs-re-read-dir 'ignore)
(defalias 'ange-ftp-re-read-dir 'ignore)

(defadvice require (before require-avoid-compiled activate)
  ;; (feature filename)
  "Avoid loading .elc files."
  ;; Ensure a second argument to require is supplied that explicitly
  ;; specifies loading the .el version of the file.
  (let ((filename (ad-get-arg 1)))
    (or filename (setq filename (symbol-name (ad-get-arg 0))))
    (save-match-data
      (cond ((string-match "\\.el\\'" filename)
             nil)
            ((string-match "\\.elc\\'" filename)
             (setq filename (replace-match ".el" t t filename)))
            (t
             (setq filename (concat filename ".el")))))
    (ad-set-arg 1 filename)))

(eval-and-compile
  (unless (string-match "XEmacs" emacs-version)
    (fset 'get-popup-menu-response 'ignore)
    (fset 'event-object 'ignore)
    (fset 'x-defined-colors 'ignore)
    (fset 'read-color 'ignore)))

(defun dgnushack-compile ()
  ;;(setq byte-compile-dynamic t)
  (let ((files (directory-files "." nil ".el$"))
	(xemacs (string-match "XEmacs" emacs-version))
	;;(byte-compile-generate-call-tree t)
	byte-compile-warnings file)
    (condition-case ()
	(require 'w3-forms)
      (error (setq files (delete "nnweb.el" files))))
    (while (setq file (pop files))
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
	(when (or (not (file-exists-p (concat file "c")))
		  (file-newer-than-file-p file (concat file "c")))
	  (condition-case ()
	      (byte-compile-file file)
	    (error nil)))))))

(defun dgnushack-recompile ()
  (require 'gnus)
  (byte-recompile-directory "." 0))

;;; dgnushack.el ends here  

