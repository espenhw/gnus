;;; smime.el --- S/MIME support library
;; Copyright (c) 2000 Free Software Foundation, Inc.

;; Author: Simon Josefsson <simon@josefsson.org>
;; Keywords: SMIME X.509 PEM OpenSSL

;; This file is not a part of GNU Emacs, but the same permissions apply.

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

;; This library perform S/MIME operations from within Emacs.
;;
;; Functions for fetching certificates from public repositories are
;; NOT provided (yet).
;;
;; It uses OpenSSL (tested with version 0.9.5a) for signing,
;; encryption and decryption.
;;
;; Some general knowledge of S/MIME, X.509, PKCS#12, PEM etc is
;; probably required to use this library in any useful way.
;; Especially, don't expect this library to buy security for you.  If
;; you don't understand what you are doing, you're as likely to lose
;; security than gain any by using this library.

;;; Quick introduction:

;; Get your S/MIME certificate from VeriSign or someplace.  I used
;; Netscape to generate the key and certificate request and stuff, and
;; Netscape can export the key into PKCS#12 format.
;;
;; Enter OpenSSL.  To be able to use this library, it need to have the
;; SMIME key readable in PEM format.  OpenSSL is used to convert the
;; key:
;;
;; $ openssl pkcs12 -in mykey.p12 -clcerts -nodes > mykey.pem
;; ...
;;
;; Now, use M-x customize-variable smime-keys and add mykey.pem as
;; a key.
;;
;; Now you should be able to sign messages!  Create a buffer and write
;; something and run M-x smime-sign-buffer RET RET and you should see
;; your message MIME armoured and a signature.  Encryption, M-x
;; smime-encrypt-buffer, should also work.
;;
;; To be able to verify messages you need to build up trust with
;; someone.  Perhaps you trust the CA that issued your certificate, at
;; least I did, so I export it's certificates from my PKCS#12
;; certificate with:
;;
;; $ openssl pkcs12 -in mykey.p12 -cacerts -nodes > cacert.pem
;; ...
;;
;; Now, use M-x customize-variable smime-CAs and add cacert.pem as a
;; CA certificate.
;;
;; You should now be able to sign messages, and even verify messages
;; sent by others that use the same CA as you.

;; Bugs:
;;
;; Don't complain that this package doesn't do encrypted PEM files,
;; submit a patch instead.  I store my keys in a safe place, so I
;; didn't need the encryption.  Also, programming this was made a lot
;; easier by that decision.  One might think that this even influenced
;; were I store my keys, and one would probably be right. :-)
;;
;; Suggestions and comments are appreciated, mail me at simon@josefsson.org.

;; <rant>
;;
;; I would include pointers to introductory text on concepts used in
;; this library here, but the material I've read are so horrible I
;; don't want to recomend them.
;;
;; Why can't someone write a simple introduction to all this stuff?
;; Until then, much of this resemble security by obscurity.
;;
;; Also, I'm not going to mention anything about the wonders of
;; cryptopolitics.  Oops, I just did.
;;
;; </rant>

;;; Revision history:

;; version 0 not released

;;; Code:

(defgroup smime nil
  "S/MIME configuration.")

(defcustom smime-keys nil
  "Map your mail addresses to a file with your certified key.
The file is assumed to be in PEM format and not encrypted."
  :type '(repeat (list (string :tag "Mail address")
		       (file :tag "File name")))
  :group 'smime)

(defcustom smime-CAs nil
  "List of directories/files containing certificates for CAs you trust.
Files should be in PEM format.
Directories should contain files (in PEM format) named to the X.509
hash of the certificate."
  :type '(repeat (radio (directory :tag "Trusted CA directory")
			(file :tag "Trusted CA file")))
  :group 'smime)

(defcustom smime-certificate-directory "~/Mail/certs/"
  "Directory containing other people's certificates.
It should contain files named to the X.509 hash of the certificate,
and the files themself should be in PEM format.
The S/MIME library provide simple functionality for fetching
certificates into this directory, so there is no need to populate it
manually."
  :type 'directory
  :group 'smime)

(defcustom smime-openssl-program "openssl"
  "Name of OpenSSL binary."
  :type 'string
  :group 'smime)

;; OpenSSL wrappers.

(defun smime-call-openssl-region (b e buf &rest args)
  (case (apply 'call-process-region b e smime-openssl-program nil buf nil args)
    (0 t)
    (1 (error "OpenSSL: An error occurred parsing the command options."))
    (2 (error "OpenSSL: One of the input files could not be read."))
    (3 (error "OpenSSL: an error occurred creating the PKCS#7 file or when reading the MIME message."))
    (4 (error "OpenSSL: an error occurred decrypting or verifying the message."))
    (t (error "Unknown OpenSSL exitcode %s" exitcode))))

(defun smime-sign-region (b e keyfile)
  "Sign region with certified key in KEYFILE.
If signing fails, the buffer is not modified.  Region is assumed to
have proper MIME tags.  KEYFILE is expected to contain a PEM encoded
private key and certificate."
  (let* ((buffer (generate-new-buffer (generate-new-buffer-name " *smime*"))))
    (when (smime-call-openssl-region b e buffer "smime" "-sign"
				     "-signer" (expand-file-name keyfile))
      (delete-region b e)
      (insert-buffer buffer)
      (kill-buffer buffer)
      t)))

(defun smime-encrypt-region (b e certfiles)
  "Encrypt region for recipients specified in CERTFILES.
If encryption fails, the buffer is not modified.  Region is assumed to
have proper MIME tags.  CERTFILES is a list of filenames, each file
is expected to contain of a PEM encoded certificate."
  (let ((buffer (generate-new-buffer (generate-new-buffer-name " *smime*"))))
    (when (apply 'smime-call-openssl-region b e buffer "smime" "-encrypt"
		 (mapcar 'expand-file-name certfiles))
      (delete-region b e)
      (insert-buffer buffer)
      (kill-buffer buffer)
      t)))

(defun smime-sign-buffer (&optional keyfile buffer)
  "S/MIME sign BUFFER with key in KEYFILE.
KEYFILE should contain a PEM encoded key and certificate."
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (smime-sign-region
     (point-min) (point-max) 
     (or keyfile
	 (smime-get-key-by-email
	  (completing-read "Sign using which signature? " smime-keys nil nil
			   (and (listp (car-safe smime-keys)) (caar smime-keys))))))))

(defun smime-encrypt-buffer (&optional certfiles buffer)
  "S/MIME encrypt BUFFER for recipients specified in CERTFILES.
CERTFILES is a list of filenames, each file is expected to consist of
a PEM encoded key and certificate.  Uses current buffer if BUFFER is
nil."
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (smime-encrypt-region 
     (point-min) (point-max)
     (or certfiles
	 (list (read-file-name "Recipient's S/MIME certificate: "
			       smime-certificate-directory nil))))))

;; User interface.

(defvar smime-buffer "*SMIME*")

(defvar smime-mode-map nil)
(put 'smime-mode 'mode-class 'special)

(unless smime-mode-map
  (setq smime-mode-map (make-sparse-keymap))
  (suppress-keymap smime-mode-map)

  (define-key smime-mode-map "q" 'smime-exit)
  (define-key smime-mode-map "f" 'smime-certificate-info))

(defun smime-mode ()
  "Major mode for browsing, viewing and fetching certificates.

All normal editing commands are switched off.
\\<smime-mode-map>

The following commands are available:

\\{smime-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'smime-mode)
  (setq mode-name "SMIME")
  (setq mode-line-process nil)
  (use-local-map smime-mode-map)
  (buffer-disable-undo)
  (setq truncate-lines t)
  (setq buffer-read-only t))

(defun smime-certificate-info (certfile)
  (interactive "fCertificate file: ")
  (let ((buffer (get-buffer-create (format "*certificate %s*" certfile))))
    (switch-to-buffer buffer)
    (erase-buffer)
    (call-process smime-openssl-program nil buffer 'display
		  "x509" "-in" (expand-file-name certfile) "-text")
    (fundamental-mode)
    (set-buffer-modified-p nil)
    (toggle-read-only t)
    (goto-char (point-min))))

(defun smime-draw-buffer ()
  (with-current-buffer smime-buffer
    (let (buffer-read-only)
      (erase-buffer)
      (insert "\nYour keys:\n")
      (dolist (key smime-keys)
	(insert 
	 (format "\t\t%s: %s\n" (car key) (cadr key))))
      (insert "\nTrusted Certificate Authoritys:\n")
      (insert "\nKnown Certificates:\n"))))

(defun smime ()
  "Go to the SMIME buffer."
  (interactive)
  (unless (get-buffer smime-buffer)
    (save-excursion
      (set-buffer (get-buffer-create smime-buffer))
      (smime-mode)))
  (smime-draw-buffer)
  (switch-to-buffer smime-buffer))

(defun smime-exit ()
  "Quit the S/MIME buffer."
  (interactive)
  (kill-buffer (current-buffer)))

;; Other functions

(defun smime-get-key-by-email (email)
  (cadr (assoc email smime-keys)))

(provide 'smime)

;;; smime.el ends here
