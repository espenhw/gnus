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
(eval-when-compile (require 'cl))

(defvar mml-sign-alist
  '(("smime"     mml-smime-sign-buffer     mml-secure-part-smime-sign)
    ("pgpmime"   mml-pgpmime-sign-buffer   list))
  "Alist of MIME signer functions.")

(defvar mml-default-sign-method (caar mml-sign-alist)
  "Default sign method.")

(defvar mml-encrypt-alist
  '(("smime"     mml-smime-encrypt-buffer mml-secure-part-smime-encrypt)
    ("pgpmime"   mml-pgpmime-encrypt-buffer   list))
  "Alist of MIME encryption functions.")

(defvar mml-default-encrypt-method (caar mml-encrypt-alist)
  "Default encryption method.")

;;; Security functions

(defun mml-smime-sign-buffer (cont)
  (or (smime-sign-buffer (cdr (assq 'keyfile cont)))
      (error "Signing failed... inspect message logs for errors")))

(defun mml-smime-encrypt-buffer (cont)
  (let (certnames certfiles tmp file tmpfiles)
    (while (setq tmp (pop cont))
      (if (and (consp tmp) (eq (car tmp) 'certfile))
	  (push (cdr tmp) certnames)))
    (while (setq tmp (pop certnames))
      (if (not (and (not (file-exists-p tmp))
		    (get-buffer tmp)))
	  (push tmp certfiles)
	(setq file (make-temp-name mm-tmp-directory))
	(with-current-buffer tmp
	  (write-region (point-min) (point-max) file))
	(push file certfiles)
	(push file tmpfiles)))
    (if (smime-encrypt-buffer certfiles)
	(while (setq tmp (pop tmpfiles))
	  (delete-file tmp))
      (while (setq tmp (pop tmpfiles))
	(delete-file tmp))
      (error "Encryption failed... inspect message logs for errors"))))

(defun mml-pgpmime-sign-buffer (cont)
  (or (mml2015-sign cont)
      (error "Signing failed... inspect message logs for errors")))

(defun mml-pgpmime-encrypt-buffer (cont)
  (or (mml2015-encrypt cont)
      (error "Encryption failed... inspect message logs for errors")))

(defun mml-secure-part-smime-sign ()
  (when (null smime-keys)
    (customize-variable 'smime-keys)
    (error "No S/MIME keys configured, use customize to add your key"))
  (list 'keyfile
	(if (= (length smime-keys) 1)
	    (cadar smime-keys)
	  (or (let ((from (cadr (funcall gnus-extract-address-components 
					 (or (save-excursion
					       (save-restriction
						 (message-narrow-to-headers)
						 (message-fetch-field "from")))
					     "")))))
		(and from (smime-get-key-by-email from)))
	      (smime-get-key-by-email
	       (completing-read "Sign this part with what signature? "
				smime-keys nil nil
				(and (listp (car-safe smime-keys)) 
				     (caar smime-keys))))))))

(defun mml-secure-part-smime-encrypt-by-file ()
  (ignore-errors
    (list 'certfile (read-file-name
		     "File with recipient's S/MIME certificate: "
		     smime-certificate-directory nil t ""))))


(defun mml-secure-part-smime-encrypt-by-dns ()
  ;; todo: deal with comma separated multiple recipients
  (let (result who bad cert)
    (condition-case ()
	(while (not result)
	  (setq who (read-from-minibuffer
		     (format "%sLookup certificate for: " (or bad ""))
		     (cadr (funcall gnus-extract-address-components 
				    (or (save-excursion
					  (save-restriction
					    (message-narrow-to-headers)
					    (message-fetch-field "to")))
					"")))))
	  (if (setq cert (smime-cert-by-dns who))
	      (setq result (list 'certfile (buffer-name cert)))
	    (setq bad (format "`%s' not found. " who))))
      (quit))
    result))

(defun mml-secure-part-smime-encrypt ()
  ;; todo: add ldap support (xemacs ldap api?)
  ;; todo: try dns/ldap automatically first, before prompting user
  (let (certs done)
    (while (not done)
      (ecase (read (gnus-completing-read "dns" "Fetch certificate from"
					 '(("dns") ("file")) nil t))
	(dns (setq certs (append certs
				 (mml-secure-part-smime-encrypt-by-dns))))
	(file (setq certs (append certs
				  (mml-secure-part-smime-encrypt-by-file)))))
      (setq done (not (y-or-n-p "Add more recipients? "))))
    certs))

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
	    (t (error "Can't find where this part begin"))))))

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
