;;; rfc2015.el --- MIME Security with Pretty Good Privacy (PGP)
;; Copyright (c) 2000 Free Software Foundation, Inc.

;; Author: Shenghuo Zhu <zsh@cs.rochester.edu>
;; Keywords: PGP MIME

;; This file is a part of GNU Emacs.

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

;; Usage:
;;    (rfc2015-setup)
;; 
;; Insert an attribute, postprocess=pgp-sign (or pgp-encrypt), into
;; the mml tag to be signed (or encrypted).

;;; Code:

(defvar rfc2015-decrypt-function 'mailcrypt-decrypt)
(defvar rfc2015-verify-function 'mailcrypt-verify)

(defun rfc2015-decrypt (handle)
  (let (child)
    (cond 
     ((setq child (mm-find-part-by-type (cdr handle) 
					"application/octet-stream"))
      (let (handles result)
	(with-temp-buffer
	  (mm-insert-part child)
	  (setq result (funcall rfc2015-decrypt-function))
	  (unless (car result)
	    (error "Decrypting error."))
	  (setq handles (mm-dissect-buffer t)))
	(setq gnus-article-mime-handles
	      (append (if (listp (car gnus-article-mime-handles))
			  gnus-article-mime-handles
			(list gnus-article-mime-handles))
		      (if (listp (car handles))
			  handles
			(list handles))))
	(gnus-mime-display-part handles)))
     (t
      (if (y-or-n-p "Corrupted pgp-encrypted part. Abort?" )
	  (error "Corrupted pgp-encrypted part.")
	(gnus-mime-display-mixed (cdr handle)))))))

;; FIXME: mm-dissect-buffer loses information of micalg and the
;; original header of signed part.

(defun rfc2015-verify (handle)
  (if (y-or-n-p "Verify signed part?" )
      (let (child result hash)
	(with-temp-buffer
	  (unless (setq child (mm-find-part-by-type 
			       (cdr handle) "application/pgp-signature" t))
	    (error "Corrupted pgp-signature part."))
	  (insert "-----BEGIN PGP SIGNED MESSAGE-----\n")
	  (insert (format "Hash: %s\n\n" (read-string "Hash: " "SHA1")))
	  (mm-insert-part child)
	  (goto-char (point-max))
	  (unless (bolp)
	    (insert "\n"))
	  (unless (setq child (mm-find-part-by-type 
			       (cdr handle) "application/pgp-signature"))
	    (error "Corrupted pgp-signature part."))
	  (mm-insert-part child)
	  (setq result (funcall rfc2015-verify-function))
	  (unless result
	    (error "Verify error.")))))
  (gnus-mime-display-part 
   (mm-find-part-by-type 
    (cdr handle) "application/pgp-signature" t)))

(defvar rfc2015-mailcrypt-prefix 0)

(defun rfc2015-mailcrypt-sign (cont)
  (mailcrypt-sign rfc2015-mailcrypt-prefix)
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
		    hash))
    (insert "\n")
    (insert (format "--%s\n" boundary))
    (unless (re-search-forward (cdr (assq 'signed-end-line scheme-alist)))
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

(defun rfc2015-mailcrypt-encrypt (cont)
  ;; FIXME:
  ;; You have to input the receiptant.
  (mailcrypt-encrypt rfc2015-mailcrypt-prefix)
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

;; The following code might be moved into mml.el or gnus-art.el.

(defvar mml-postprocess-alist
  '(("pgp-sign" . rfc2015-mailcrypt-sign)
    ("pgp-encrypt" . rfc2015-mailcrypt-encrypt))
  "Alist of postprocess functions.")

(defun mml-postprocess (cont)
  (let ((pp (cdr (or (assq 'postprocess cont)
		     (assq 'pp cont))))
	item)
    (if (and pp (setq item (assoc pp mml-postprocess-alist)))
	(funcall (cdr item) cont))))

(defun rfc2015-setup ()
  (setq mml-generate-mime-postprocess-function 'mml-postprocess)
;  (push '("multipart/signed" . rfc2015-verify)
;  	gnus-mime-multipart-functions)
  (push '("multipart/encrypted" . rfc2015-decrypt)
	gnus-mime-multipart-functions))

;; The following code might be moved into mm-decode.el.

(defun mm-find-part-by-type (handles type &optional notp) 
  (let (handle)
    (while handles
      (if (if notp
	      (not (equal (mm-handle-media-type (car handles)) type))
	    (equal (mm-handle-media-type (car handles)) type))
	  (setq handle (car handles)
		handles nil))
      (setq handles (cdr handles)))
    handle))

(provide 'rfc2015)

;;; rfc2015.el ends here
