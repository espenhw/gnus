;;; mml-sec.el --- A package with security functions for MML documents
;; Copyright (C) 2000 Free Software Foundation, Inc.

;; Author: Simon Josefsson <simon@josefsson.org>
;; This file is not part of GNU Emacs, but the same permissions apply.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(require 'smime)
(require 'mml2015)
(require 'mml-smime)
(eval-when-compile (require 'cl))

(defvar mml-sign-alist
  '(("smime"     mml-smime-sign-buffer     mml-smime-sign-query)
    ("pgpmime"   mml-pgpmime-sign-buffer   list))
  "Alist of MIME signer functions.")

(defvar mml-default-sign-method (caar mml-sign-alist)
  "Default sign method.")

(defvar mml-encrypt-alist
  '(("smime"     mml-smime-encrypt-buffer     mml-smime-encrypt-query)
    ("pgpmime"   mml-pgpmime-encrypt-buffer   list))
  "Alist of MIME encryption functions.")

(defvar mml-default-encrypt-method (caar mml-encrypt-alist)
  "Default encryption method.")

;;; Security functions

(defun mml-smime-sign-buffer (cont)
  (or (mml-smime-sign cont)
      (error "Signing failed... inspect message logs for errors")))

(defun mml-smime-encrypt-buffer (cont)
  (or (mml-smime-encrypt cont)
      (error "Encryption failed... inspect message logs for errors")))

(defun mml-pgpmime-sign-buffer (cont)
  (or (mml2015-sign cont)
      (error "Signing failed... inspect message logs for errors")))

(defun mml-pgpmime-encrypt-buffer (cont)
  (or (mml2015-encrypt cont)
      (error "Encryption failed... inspect message logs for errors")))

(defun mml-secure-part (method &optional sign)
  (save-excursion
    (let ((tags (funcall (nth 2 (assoc method (if sign mml-sign-alist
						mml-encrypt-alist))))))
      (cond ((re-search-backward
	      "<#\\(multipart\\|part\\|external\\|mml\\)" nil t)
	     (goto-char (match-end 0))
	     (insert (if sign " sign=" " encrypt=") method)
	     (while tags
	       (let ((key (pop tags))
		     (value (pop tags)))
		 (when value
		   ;; Quote VALUE if it contains suspicious characters.
		   (when (string-match "[\"'\\~/*;() \t\n]" value)
		     (setq value (prin1-to-string value)))
		   (insert (format " %s=%s" key value))))))
	    ((or (re-search-backward 
		  (concat "^" (regexp-quote mail-header-separator) "\n") nil t)
		 (re-search-forward
		  (concat "^" (regexp-quote mail-header-separator) "\n") nil t))
	     (goto-char (match-end 0))
	     (apply 'mml-insert-tag 'part (cons (if sign 'sign 'encrypt)
						(cons method tags))))
	    (t (error "The message is corrupted. No mail header separator."))))))

(defun mml-secure-sign-pgpmime ()
  "Add MML tags to PGP/MIME sign this MML part."
  (interactive)
  (mml-secure-part "pgpmime" 'sign))

(defun mml-secure-sign-smime ()
  "Add MML tags to S/MIME sign this MML part."
  (interactive)
  (mml-secure-part "smime" 'sign))

(defun mml-secure-encrypt-pgpmime ()
  "Add MML tags to PGP/MIME encrypt this MML part."
  (interactive)
  (mml-secure-part "pgpmime"))

(defun mml-secure-encrypt-smime ()
  "Add MML tags to S/MIME encrypt this MML part."
  (interactive)
  (mml-secure-part "smime"))

(provide 'mml-sec)

;;; mml-sec.el ends here
