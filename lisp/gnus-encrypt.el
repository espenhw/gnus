;;; gnus-encrypt.el --- file encryption routines for Gnus
;; Copyright (C) 2002, 2003, 2004 Free Software Foundation, Inc.

;; Author: Teodor Zlatanov <tzz@lifelogs.com>
;; Created: 2003/01/24
;; Keywords: files

;; This file is part of GNU Emacs.

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

;;; This module addresses data encryption under Gnus.  Page breaks are
;;; used for grouping declarations and documentation relating to each
;;; particular aspect.

;;; Code:

;; autoload password
(eval-and-compile
  (autoload 'password-read "password"))

(defgroup gnus-encrypt nil
  "Gnus encryption configuration.")

(defcustom gnus-encrypt-password-cache-expiry 200
  "Gnus encryption password timeout.
When set, directly sets password-cache-expiry"
  :type 'integer
  :group 'gnus-encrypt
  :set (lambda (symbol value)
	 (set symbol value)
	 (setq password-cache-expiry value)))

(defcustom gnus-encrypt-file-alist nil
  "List of file names or regexes matched with encryptions.
Format example:
 '((\"beta\"
    (gpg \"AES\"))
   (\"/home/tzz/alpha\"
    (gnus-encrypt-xor \"Semi-Secret\")))"

  :type '(repeat
	  (list :tag "Encryption entry"
	   (radio :tag "What to encrypt"
		  (file :tag "Filename")
		  (regexp :tag "Regular expression match"))
	   (radio :tag "How to encrypt it"
		  (list
		   :tag "GPG Encryption"
		   (const :tag "GPG Program" gpg)
		   (radio :tag "Choose a cipher"
			  (const :tag "3DES Encryption" "3DES")
			  (const :tag "CAST5 Encryption" "CAST5")
			  (const :tag "Blowfish Encryption" "BLOWFISH")
			  (const :tag "AES Encryption" "AES")
			  (const :tag "AES192 Encryption" "AES192")
			  (const :tag "AES256 Encryption" "AES256")
			  (const :tag "Twofish Encryption" "TWOFISH")
			  (string :tag "Cipher Name")))
		  (list
		   :tag "Built-in simple XOR"
		   (const :tag "XOR Encryption" gnus-encrypt-xor)
		   (string :tag "XOR Cipher Value (seed value)")))))
  :group 'gnus-encrypt)

;; TODO: now, load gencrypt.el and if successful, modify the
;; custom-type of gnus-encrypt-file-alist to add the gencrypt.el options

;; (plist-get (symbol-plist 'gnus-encrypt-file-alist) 'custom-type)
;; then use plist-put

(defcustom gnus-encrypt-gpg-path (executable-find "gpg")
  "Path to the GPG program."
  :type '(radio
	  (file :tag "Location of the GPG executable")
	  (const :tag "GPG is not installed" nil))
  :group 'gnus-encrypt)

(defvar gnus-encrypt-temp-prefix "gnus-encrypt"
  "Prefix for temporary filenames")

(defun gnus-encrypt-find-model (filename)
  "Given a filename, find a gnus-encrypt-file-alist entry"
  (dolist (entry gnus-encrypt-file-alist)
    (let ((match (nth 0 entry))
	  (model (nth 1 entry)))
      (when (or (eq match filename)
		(string-match match filename))
	(return model)))))

(defun gnus-encrypt-insert-file-contents (file &optional model)
  "Decrypt FILE into the current buffer."
  (interactive "fFile to insert: ")
  (let* ((model (or model (gnus-encrypt-find-model file)))
	 (method (nth 0 model))
	 (cipher (nth 1 model))
	 (password-key (format "gnus-encrypt-password-%s-%s"
			       (symbol-name method) cipher))
	 (passphrase
	  (password-read-and-add
	   (format "%s password for cipher %s? "
		   (symbol-name method) cipher)
	   password-key))
	  (buffer-file-coding-system 'binary)
	 (coding-system-for-read 'binary)
	 outdata)

    ;; note we only insert-file-contents if the method is known to be valid
    (cond
     ((eq method 'gpg)
      (insert-file-contents file)
      (setq outdata (gnus-encrypt-gpg-decode-buffer passphrase cipher)))
     ((eq method 'gnus-encrypt-xor)
      (insert-file-contents file)
      (setq outdata (gnus-encrypt-xor-decode-buffer passphrase cipher))))

    (if outdata
	(progn
	  (gnus-message 9 "%s was decrypted with %s (cipher %s)"
			file (symbol-name method) cipher)
	  (delete-region (point-min) (point-max))
	  (goto-char (point-min))
	  (insert outdata))
      (gnus-error 5 "%s was NOT decrypted with %s (cipher %s)"
		  file (symbol-name method) cipher))))

(defun gnus-encrypt-get-file-contents (file &optional model)
  "Decrypt FILE and return the contents."
  (interactive "fFile to decrypt: ")
  (with-temp-buffer
    (gnus-encrypt-insert-file-contents file model)
    (buffer-string)))

(defun gnus-encrypt-put-file-contents (file data &optional model)
  "Encrypt the DATA to FILE, then continue normally."
  (with-temp-buffer
    (insert data)
    (gnus-encrypt-write-file-contents file model)))

(defun gnus-encrypt-write-file-contents (file &optional model)
  "Encrypt the current buffer to FILE, then continue normally."
  (interactive "fFile to write: ")
  (let* ((model (or model (gnus-encrypt-find-model file)))
	 (method (nth 0 model))
	 (cipher (nth 1 model))
	 (passphrase
	  (password-read
	   (format "%s password for cipher %s? "
		   (symbol-name method) cipher)
	   (format "gnus-encrypt-password-%s-%s"
		   (symbol-name method) cipher)))
	 outdata)

    (cond
     ((eq method 'gpg)
      (setq outdata (gnus-encrypt-gpg-encode-buffer passphrase cipher)))
     ((eq method 'gnus-encrypt-xor)
      (setq outdata (gnus-encrypt-xor-encode-buffer passphrase cipher))))

    (if outdata
	(progn
	  (gnus-message 9 "%s was encrypted with %s (cipher %s)"
			file (symbol-name method) cipher)
	  (delete-region (point-min) (point-max))
	  (goto-char (point-min))
	  (insert outdata)
	  ;; do not confirm overwrites
	  (write-file file nil))
      (gnus-error 5 "%s was NOT encrypted with %s (cipher %s)"
		  file (symbol-name method) cipher))))

(defun gnus-encrypt-xor-encode-buffer (passphrase cipher)
  (gnus-encrypt-xor-process-buffer passphrase cipher t))

(defun gnus-encrypt-xor-decode-buffer (passphrase cipher)
  (gnus-encrypt-xor-process-buffer passphrase cipher nil))

(defun gnus-encrypt-xor-process-buffer (passphrase
					cipher
					&optional encode)
  "Given PASSPHRASE, xor-encode or decode the contents of the current buffer."
  (let* ((bs (buffer-substring-no-properties (point-min) (point-max)))
	 ;; passphrase-sum is a simple additive checksum of the
	 ;; passphrase and the cipher
	(passphrase-sum
	 (when (stringp passphrase)
	   (apply '+ (append cipher passphrase nil))))
	new-list)

    (with-temp-buffer
      (if encode
	  (progn
	    (dolist (x (append bs nil))
	      (setq new-list (cons (logxor x passphrase-sum) new-list)))

	    (dolist (x new-list)
	      (insert (format "%d " x))))
	(progn
	  (setq new-list (reverse (split-string bs)))
	  (dolist (x new-list)
	    (setq x (string-to-int x))
	    (insert (format "%c" (logxor x passphrase-sum))))))
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun gnus-encrypt-gpg-encode-buffer (passphrase cipher)
  (gnus-encrypt-gpg-process-buffer passphrase cipher t))

(defun gnus-encrypt-gpg-decode-buffer (passphrase cipher)
  (gnus-encrypt-gpg-process-buffer passphrase cipher nil))

(defun gnus-encrypt-gpg-process-buffer (passphrase 
					cipher 
					&optional encode)
  "With PASSPHRASE, use GPG to encode or decode the current buffer."
  (let* ((program gnus-encrypt-gpg-path)
	 (input (buffer-substring-no-properties (point-min) (point-max)))
	 (temp-maker (if (fboundp 'make-temp-file) 
			 'make-temp-file 
		       'make-temp-name))
	 (temp-file (funcall temp-maker gnus-encrypt-temp-prefix))
	 (default-enable-multibyte-characters nil)
	 (args `("--cipher-algo" ,cipher
		 "--status-fd" "2"
		 "--logger-fd" "2"
		 "--passphrase-fd" "0"
		 "--no-tty"))
	 exit-status exit-data)
    
    (when encode
      (setq args
	    (append args
		    '("--symmetric"
		      "--armor"))))

    (if program
	(with-temp-buffer
	  (when passphrase
	    (insert passphrase "\n"))
	  (insert input)
	  (setq exit-status
		(apply #'call-process-region (point-min) (point-max) program
		       t `(t ,temp-file) nil args))
	  (if (equal exit-status 0)
	      (setq exit-data
		    (buffer-substring-no-properties (point-min) (point-max)))
	    (with-temp-buffer
	      (when (file-exists-p temp-file)
		(insert-file-contents temp-file))
	      (gnus-error 5 (format "%s exited abnormally: '%s' [%s]"
				    program exit-status (buffer-string)))))
	  (delete-file temp-file))
      (gnus-error 5 "GPG is not installed."))
    exit-data))

(provide 'gnus-encrypt)
;;; gnus-encrypt.el ends here

;; (defcustom netrc-encrypting-method nil
;;   "Decoding method used for the netrc file.
;; Use the OpenSSL symmetric ciphers here.  Leave nil for no
;; decoding.  Encrypt the file with netrc-encrypt, but make sure you
;; have set netrc-encrypting-method to a non-nil value."
;;   :type '(choice
;; 	  (const :tag "DES-3" "des3")
;; 	  (const :tag "IDEA" "idea")
;; 	  (const :tag "RC4" "rc4")
;; 	  (string :tag "Explicit cipher name")
;; 	  (const :tag "None" nil))
;;   :group 'netrc)

;; (defcustom netrc-openssl-path (executable-find "openssl")
;;   "File path of the OpenSSL shell."
;;   :type '(choice (file :tag "Location of openssl")
;; 		 (const :tag "openssl is not installed" nil))
;;   :group 'netrc)

;; (defun netrc-encrypt (plain-file encrypted-file)
;;   (interactive "fPlain File: \nFEncrypted File: ")
;;   "Encrypt FILE to ENCRYPTED-FILE with netrc-encrypting-method cipher."
;;   (when (and (file-exists-p plain-file)
;; 	     (stringp encrypted-file)
;; 	     netrc-encrypting-method
;; 	     netrc-openssl-path)
;;     (let ((buffer-file-coding-system 'binary)
;; 	  (coding-system-for-read 'binary)
;; 	  (coding-system-for-write 'binary)
;; 	  (password 
;; 	   (password-read
;; 	    (format "OpenSSL Password for cipher %s? "
;; 		    netrc-encrypting-method)
;; 	    (format "netrc-openssl-password-%s"
;; 		    netrc-encrypting-method))))
;;       (when password
;; 	(with-temp-buffer
;; 	  (insert-file-contents plain-file)
;; 	  (setenv "NETRC_OPENSSL_PASSWORD" password)
;; 	  (shell-command-on-region 
;; 	   (point-min) 
;; 	   (point-max)
;; 	   (format "%s %s -pass env:NETRC_OPENSSL_PASSWORD -e"
;; 		   netrc-openssl-path
;; 		   netrc-encrypting-method)
;; 	   t
;; 	   t)
;; 	  (write-file encrypted-file t))))))

;; 	(if (and netrc-encrypting-method
;; 		 netrc-openssl-path)
;; 	    (let ((buffer-file-coding-system 'binary)
;; 		  (coding-system-for-read 'binary)
;; 		  (coding-system-for-write 'binary)
;; 		  (password 
;; 		   (password-read
;; 		    (format "OpenSSL Password for cipher %s? "
;; 			    netrc-encrypting-method)
;; 		    (format "netrc-openssl-password-%s" 
;; 			    netrc-encrypting-method))))
;; 	      (when password
;; 		(insert-file-contents file)
;; 		(setenv "NETRC_OPENSSL_PASSWORD" password)
;; 		(shell-command-on-region
;; 		 (point-min) 
;; 		 (point-max)
;; 		 (format "%s %s -pass env:NETRC_OPENSSL_PASSWORD -d"
;; 			 netrc-openssl-path
;; 			 netrc-encrypting-method)
;; 		 t
;; 		 t)))

