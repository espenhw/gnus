;;; mml-gpg-old.el --- Old PGP message format (RFC 1991) support for MML
;; Copyright (C) 1998, 1999, 2000, 2001 Free Software Foundation, Inc.

;; Author: Sascha Lüdecke <sascha@meta-x.de>,
;;	Simon Josefsson <simon@josefsson.org> (Mailcrypt interface, Gnus glue)
;; Keywords PGP

;; This file is (not yet) part of GNU Emacs.

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

;; RCS: $Id: mml1991.el,v 6.5 2001/12/18 16:14:19 huber Exp $

;;; Code:

(defvar mml1991-use mml2015-use
  "The package used for PGP.")

(defvar mml1991-function-alist
  '((mailcrypt mml1991-mailcrypt-sign
	       mml1991-mailcrypt-encrypt)
    (gpg mml1991-gpg-sign
	 mml1991-gpg-encrypt))
  "Alist of PGP/MIME functions.")

;;; mailcrypt wrapper

(eval-and-compile
  (autoload 'mc-sign-generic "mc-toplev"))

(defvar mml1991-decrypt-function 'mailcrypt-decrypt)
(defvar mml1991-verify-function 'mailcrypt-verify)

(defun mml1991-mailcrypt-sign (cont)
  (let ((text (current-buffer))
	headers signature
	(result-buffer (get-buffer-create "*GPG Result*")))
    ;; Save MIME Content[^ ]+: headers from signing
    (goto-char (point-min))
    (while (looking-at "^Content[^ ]+:") (forward-line))
    (if (> (point) (point-min))
	(progn
	  (setq headers (buffer-substring (point-min) (point)))
	  (kill-region (point-min) (point))))
    (goto-char (point-max))
    (unless (bolp)
      (insert "\n"))
    (quoted-printable-decode-region (point-min) (point-max))
    (with-temp-buffer
      (setq signature (current-buffer))
      (insert-buffer text)
      (unless (mc-sign-generic (message-options-get 'message-sender)
			       nil nil nil nil)
	(unless (> (point-max) (point-min))
	  (pop-to-buffer result-buffer)
	  (error "Sign error")))
      (goto-char (point-min))
      (while (re-search-forward "\r+$" nil t)
	(replace-match "" t t))
      (quoted-printable-encode-region (point-min) (point-max))
      (set-buffer text)
      (kill-region (point-min) (point-max))
      (if headers (insert headers))
      (insert "\n")
      (insert-buffer signature)
      (goto-char (point-max)))))

(defun mml1991-mailcrypt-encrypt (cont)
  (let ((text (current-buffer))
	cipher
	(result-buffer (get-buffer-create "*GPG Result*")))
    ;; Strip MIME Content[^ ]: headers since it will be ASCII ARMOURED
    (goto-char (point-min))
    (while (looking-at "^Content[^ ]+:") (forward-line))
    (if (> (point) (point-min))
	(progn
	  (kill-region (point-min) (point))))
    (mm-with-unibyte-current-buffer-mule4
      (with-temp-buffer
	(setq cipher (current-buffer))
	(insert-buffer text)
	(unless (mc-encrypt-generic
		 (or
		  (message-options-get 'message-recipients)
		  (message-options-set 'message-recipients
				       (read-string "Recipients: ")))
		 nil
		 (point-min) (point-max)
		 (message-options-get 'message-sender)
		 'sign)
	  (unless (> (point-max) (point-min))
	    (pop-to-buffer result-buffer)
	    (error "Encrypt error")))
	(goto-char (point-min))
	(while (re-search-forward "\r+$" nil t)
	  (replace-match "" t t))
	(set-buffer text)
	(kill-region (point-min) (point-max))
	;;(insert "Content-Type: application/pgp-encrypted\n\n")
	;;(insert "Version: 1\n\n")
	(insert "\n")
	(insert-buffer cipher)
	(goto-char (point-max))))))

;;; gpg wrapper

(eval-and-compile
  (autoload 'gpg-sign-cleartext "gpg"))

(defun mml1991-gpg-sign (cont)
  (let ((text (current-buffer))
	headers signature
	(result-buffer (get-buffer-create "*GPG Result*")))
    ;; Save MIME Content[^ ]+: headers from signing
    (goto-char (point-min))
    (while (looking-at "^Content[^ ]+:") (forward-line))
    (if (> (point) (point-min))
	(progn
	  (setq headers (buffer-substring (point-min) (point)))
	  (kill-region (point-min) (point))))
    (goto-char (point-max))
    (unless (bolp)
      (insert "\n"))
    (quoted-printable-decode-region (point-min) (point-max))
    (with-temp-buffer
      (unless (gpg-sign-cleartext text (setq signature (current-buffer))
				  result-buffer
				  nil
				  (message-options-get 'message-sender))
	(unless (> (point-max) (point-min))
	  (pop-to-buffer result-buffer)
	  (error "Sign error")))
      (goto-char (point-min))
      (while (re-search-forward "\r+$" nil t)
	(replace-match "" t t))
      (quoted-printable-encode-region (point-min) (point-max))
      (set-buffer text)
      (kill-region (point-min) (point-max))
      (if headers (insert headers))
      (insert "\n")
      (insert-buffer signature)
      (goto-char (point-max)))))

(defun mml1991-gpg-encrypt (cont)
  (let ((text (current-buffer))
	cipher
	(result-buffer (get-buffer-create "*GPG Result*")))
    ;; Strip MIME Content[^ ]: headers since it will be ASCII ARMOURED
    (goto-char (point-min))
    (while (looking-at "^Content[^ ]+:") (forward-line))
    (if (> (point) (point-min))
	(progn
	  (kill-region (point-min) (point))))
    (mm-with-unibyte-current-buffer-mule4
      (with-temp-buffer
	(unless (gpg-sign-encrypt
		 text (setq cipher (current-buffer))
		 result-buffer
		 (split-string
		  (or
		   (message-options-get 'message-recipients)
		   (message-options-set 'message-recipients
					(read-string "Recipients: ")))
		  "[ \f\t\n\r\v,]+")
		 nil
		 (message-options-get 'message-sender)
		 t t) ; armor & textmode
	  (unless (> (point-max) (point-min))
	    (pop-to-buffer result-buffer)
	    (error "Encrypt error")))
	(goto-char (point-min))
	(while (re-search-forward "\r+$" nil t)
	  (replace-match "" t t))
	(set-buffer text)
	(kill-region (point-min) (point-max))
	;;(insert "Content-Type: application/pgp-encrypted\n\n")
	;;(insert "Version: 1\n\n")
	(insert "\n")
	(insert-buffer cipher)
	(goto-char (point-max))))))

;;;###autoload
(defun mml1991-encrypt (cont)
  (let ((func (nth 2 (assq mml1991-use mml1991-function-alist))))
    (if func
	(funcall func cont)
      (error "Cannot find encrypt function"))))

;;;###autoload
(defun mml1991-sign (cont)
  (let ((func (nth 1 (assq mml1991-use mml1991-function-alist))))
    (if func
	(funcall func cont)
      (error "Cannot find sign function"))))

(provide 'mml1991)

;; Local Variables:
;; coding: iso-8859-1
;; End:

;;; mml1991.el ends here
