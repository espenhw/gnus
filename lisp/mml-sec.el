;;; mml-sec.el --- A package with security functions for MML documents
;; Copyright (C) 2000, 2001, 2002 Free Software Foundation, Inc.

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

(require 'mml2015)
(require 'mml1991)
(require 'mml-smime)
(eval-when-compile (require 'cl))

(defvar mml-sign-alist
  '(("smime"     mml-smime-sign-buffer     mml-smime-sign-query)
    ("pgp"       mml-pgp-sign-buffer       list)
    ("pgpmime"   mml-pgpmime-sign-buffer   list))
  "Alist of MIME signer functions.")

(defvar mml-default-sign-method (caar mml-sign-alist)
  "Default sign method.")

(defvar mml-encrypt-alist
  '(("smime"     mml-smime-encrypt-buffer     mml-smime-encrypt-query)
    ("pgp"       mml-pgp-encrypt-buffer       list)
    ("pgpmime"   mml-pgpmime-encrypt-buffer   list))
  "Alist of MIME encryption functions.")

(defvar mml-default-encrypt-method (caar mml-encrypt-alist)
  "Default encryption method.")

(defvar mml-signencrypt-style-alist
  '(("smime"   separate)
    ("pgp"     separate)
    ("pgpmime" separate))
  "Alist specifying whether or not a single sign & encrypt
operation should be perfomed when requesting signencrypt.
Note that combined sign & encrypt is NOT supported by pgp v2!
Also note that you should access this with mml-signencrypt-style")

;;; Configuration/helper functions

(defun mml-signencrypt-style (method &optional style)
  "Function for setting/getting the signencrypt-style used.  Takes two
arguments, the method (e.g. \"pgp\") and optionally the mode
(e.g. combined).  If the mode is omitted, the current value is returned.

For example, if you prefer to use combined sign & encrypt with
smime, putting the following in your Gnus startup file will
enable that behavior:

 (mml-set-signencrypt-style \"smime\" combined)"
  (let ((style-item (assoc method mml-signencrypt-style-alist)))
    (if style-item
	(if (or (eq style 'separate)
		(eq style 'combined))
	    ;; valid style setting?
	    (setf (second style-item) style)
	  ;; otherwise, just return the current value
	  (second style-item))
      (gnus-message 3 "Warning, attempt to set invalid signencrypt-style"))))

;;; Security functions

(defun mml-smime-sign-buffer (cont)
  (or (mml-smime-sign cont)
      (error "Signing failed... inspect message logs for errors")))

(defun mml-smime-encrypt-buffer (cont)
  (or (mml-smime-encrypt cont)
      (error "Encryption failed... inspect message logs for errors")))

(defun mml-pgp-sign-buffer (cont)
  (or (mml1991-sign cont)
      (error "Signing failed... inspect message logs for errors")))

(defun mml-pgp-encrypt-buffer (cont)
  (or (mml1991-encrypt cont)
      (error "Encryption failed... inspect message logs for errors")))

(defun mml-pgpmime-sign-buffer (cont)
  (or (mml2015-sign cont)
      (error "Signing failed... inspect message logs for errors")))

(defun mml-pgpmime-encrypt-buffer (cont &optional sign)
  (or (mml2015-encrypt cont sign)
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
	    (t (error "The message is corrupted. No mail header separator"))))))

(defun mml-secure-sign-pgp ()
  "Add MML tags to PGP sign this MML part."
  (interactive)
  (mml-secure-part "pgp" 'sign))

(defun mml-secure-sign-pgpmime ()
  "Add MML tags to PGP/MIME sign this MML part."
  (interactive)
  (mml-secure-part "pgpmime" 'sign))

(defun mml-secure-sign-smime ()
  "Add MML tags to S/MIME sign this MML part."
  (interactive)
  (mml-secure-part "smime" 'sign))

(defun mml-secure-encrypt-pgp ()
  "Add MML tags to PGP encrypt this MML part."
  (interactive)
  (mml-secure-part "pgp"))

(defun mml-secure-encrypt-pgpmime ()
  "Add MML tags to PGP/MIME encrypt this MML part."
  (interactive)
  (mml-secure-part "pgpmime"))

(defun mml-secure-encrypt-smime ()
  "Add MML tags to S/MIME encrypt this MML part."
  (interactive)
  (mml-secure-part "smime"))

;; defuns that add the proper <#secure ...> tag to the top of the message body
(defun mml-secure-message (method &optional modesym)
  (let ((mode (prin1-to-string modesym))
	insert-loc)
    (mml-unsecure-message)
    (save-excursion
      (goto-char (point-min))
      (cond ((re-search-forward
	      (concat "^" (regexp-quote mail-header-separator) "\n") nil t)
	     (goto-char (setq insert-loc (match-end 0)))
	     (unless (looking-at "<#secure")
	       (mml-insert-tag
		'secure 'method method 'mode mode)))
	    (t (error
		"The message is corrupted. No mail header separator"))))
    (when (eql insert-loc (point))
      (forward-line 1))))

(defun mml-unsecure-message ()
  "Remove security related MML tags from message."
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (when (re-search-backward "^<#secure.*>\n" nil t)
      (kill-region (match-beginning 0) (match-end 0)))))

(defun mml-secure-message-sign-smime ()
  "Add MML tag to encrypt/sign the entire message."
  (interactive)
  (mml-secure-message "smime" 'sign))

(defun mml-secure-message-sign-pgp ()
  "Add MML tag to encrypt/sign the entire message."
  (interactive)
  (mml-secure-message "pgp" 'sign))

(defun mml-secure-message-sign-pgpmime ()
  "Add MML tag to encrypt/sign the entire message."
  (interactive)
  (mml-secure-message "pgpmime" 'sign))

(defun mml-secure-message-encrypt-smime (&optional dontsign)
  "Add MML tag to encrypt and sign the entire message.
If called with a prefix argument, only encrypt (do NOT sign)."
  (interactive "P")
  (mml-secure-message "smime" (if dontsign 'encrypt 'signencrypt)))

(defun mml-secure-message-encrypt-pgp (&optional dontsign)
  "Add MML tag to encrypt and sign the entire message.
If called with a prefix argument, only encrypt (do NOT sign)."
  (interactive "P")
  (mml-secure-message "pgp" (if dontsign 'encrypt 'signencrypt)))

(defun mml-secure-message-encrypt-pgpmime (&optional dontsign)
  "Add MML tag to encrypt and sign the entire message.
If called with a prefix argument, only encrypt (do NOT sign)."
  (interactive "P")
  (mml-secure-message "pgpmime" (if dontsign 'encrypt 'signencrypt)))

(provide 'mml-sec)

;;; mml-sec.el ends here
