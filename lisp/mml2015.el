;;; mml2015.el --- MIME Security with Pretty Good Privacy (PGP)
;; Copyright (C) 2000 Free Software Foundation, Inc.

;; Author: Shenghuo Zhu <zsh@cs.rochester.edu>
;; Keywords: PGP MIME MML

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
;; option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(eval-when-compile (require 'cl))
(require 'mm-decode)

(defvar mml2015-decrypt-function 'mailcrypt-decrypt)
(defvar mml2015-verify-function 'mailcrypt-verify)
(defvar mml2015-encrypt-function 'mml2015-mailcrypt-encrypt)
(defvar mml2015-sign-function 'mml2015-mailcrypt-sign)

;;;###autoload
(defun mml2015-decrypt (handle ctl)
  (let (child handles result)
    (unless (setq child (mm-find-part-by-type (cdr handle) 
					      "application/octet-stream"))
      (error "Corrupted pgp-encrypted part."))
    (with-temp-buffer
      (mm-insert-part child)
      (setq result (funcall mml2015-decrypt-function))
      (unless (car result)
	(error "Decrypting error."))
      (setq handles (mm-dissect-buffer t)))
    (mm-destroy-parts handle)
    (if (listp (car handles))
	handles
      (list handles))))

(defun mml2015-fix-micalg (alg)
  (upcase
   (if (and alg (string-match "^pgp-" alg))
       (substring alg (match-end 0))
     alg)))

;;;###autoload
(defun mml2015-verify (handle ctl)
  (let (part)
    (unless (setq part (mm-find-raw-part-by-type 
			 ctl "application/pgp-signature" t))
      (error "Corrupted pgp-signature part."))
    (with-temp-buffer
      (insert "-----BEGIN PGP SIGNED MESSAGE-----\n")
      (insert (format "Hash: %s\n\n" 
		      (or (mml2015-fix-micalg
			   (mail-content-type-get ctl 'micalg))
			  "SHA1")))
      (insert part "\n")
      (goto-char (point-max))
      (unless (setq part (mm-find-part-by-type 
			   (cdr handle) "application/pgp-signature"))
	(error "Corrupted pgp-signature part."))
      (mm-insert-part part)
      (unless (funcall mml2015-verify-function)
	(error "Verify error.")))))

(eval-and-compile
  (autoload 'mc-encrypt-generic "mc-toplev")
  (autoload 'mc-cleanup-recipient-headers "mc-toplev")
  (autoload 'mc-sign-generic "mc-toplev"))

(eval-when-compile
  (defvar mc-default-scheme)
  (defvar mc-schemes))

(defun mml2015-mailcrypt-sign (cont)
  (mc-sign-generic (message-options-get 'message-sender)
		   nil nil nil nil)
  (let ((boundary 
	 (funcall mml-boundary-function (incf mml-multipart-number)))
	(scheme-alist (funcall (or mc-default-scheme 
				   (cdr (car mc-schemes)))))
	hash)
    (goto-char (point-min))
    (unless (re-search-forward (cdr (assq 'signed-begin-line scheme-alist)))
      (error "Cannot find signed begin line." ))
    (goto-char (match-beginning 0))
    (forward-line 1)
    (unless (looking-at "Hash:[ \t]*\\([a-zA-Z0-9]+\\)")
      (error "Cannot not find PGP hash." ))
    (setq hash (match-string 1))
    (unless (re-search-forward "^$" nil t)
      (error "Cannot not find PGP message." ))
    (forward-line 1)
    (delete-region (point-min) (point))
    (insert (format "Content-Type: multipart/signed; boundary=\"%s\";\n"
		    boundary))
    (insert (format "\tmicalg=pgp-%s; protocol=\"application/pgp-signature\"\n"
		    (downcase hash)))
    (insert (format "\n--%s\n" boundary))
    (goto-char (point-max))
    (unless (re-search-backward (cdr (assq 'signed-end-line scheme-alist)))
      (error "Cannot find signature part." ))
    (goto-char (match-beginning 0))
    (unless (re-search-backward "^-+BEGIN" nil t)
      (error "Cannot find signature part." ))
    (goto-char (match-beginning 0))
    (insert (format "--%s\n" boundary))
    (insert "Content-Type: application/pgp-signature\n\n")
    (goto-char (point-max))
    (insert (format "--%s--\n" boundary))
    (goto-char (point-max))))

(defun mml2015-mailcrypt-encrypt (cont)
  (mc-encrypt-generic 
   (or (message-options-get 'message-recipients)
       (message-options-set 'message-recipients
			    (mc-cleanup-recipient-headers 
			     (read-string "Recipients: ")))))
  (let ((boundary 
	 (funcall mml-boundary-function (incf mml-multipart-number))))
    (goto-char (point-min))
    (insert (format "Content-Type: multipart/encrypted; boundary=\"%s\";\n"
		    boundary))
    (insert "\tprotocol=\"application/pgp-encrypted\"\n\n")
    (insert (format "--%s\n" boundary))
    (insert "Content-Type: application/pgp-encrypted\n\n")
    (insert "Version: 1\n\n")
    (insert (format "--%s\n" boundary))
    (insert "Content-Type: application/octet-stream\n\n")
    (goto-char (point-max))
    (insert (format "--%s--\n" boundary))
    (goto-char (point-max))))

;;;###autoload
(defun mml2015-encrypt (cont)
  (funcall mml2015-encrypt-function cont))

;;;###autoload
(defun mml2015-sign (cont)
  (funcall mml2015-sign-function cont))

(provide 'mml2015)

;;; mml2015.el ends here
