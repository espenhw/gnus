;;; netrc.el --- .netrc parsing functionality
;; Copyright (C) 1996, 1997, 1998, 1999, 2000, 2001, 2002
;;        Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Modularizer: Ted Zlatanov <tzz@lifelogs.com>
;; Keywords: news

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

;; Just the .netrc parsing functionality, abstracted so other packages
;; besides Gnus can use it.

;;; Code:

;;;
;;; .netrc and .authinfo rc parsing
;;;

;; autoload password
(eval-and-compile
  (autoload 'password-read "password"))

(defgroup netrc nil
 "Netrc configuration.")

(defcustom netrc-encrypting-method nil
  "Decoding method used for the netrc file.
Use the OpenSSL symmetric ciphers here.  Leave nil for no
decoding.  Encrypt the file with netrc-encrypt, but make sure you
have set netrc-encrypting-method to a non-nil value."
  :type '(choice
	  (const :tag "DES-3" "des3")
	  (const :tag "IDEA" "idea")
	  (const :tag "RC4" "rc4")
	  (string :tag "Explicit cipher name")
	  (const :tag "None" nil))
  :group 'netrc)

(defcustom netrc-openssl-path (executable-find "openssl")
  "File path of the OpenSSL shell."
  :type '(choice (file :tag "Location of openssl")
		 (const :tag "openssl is not installed" nil))
  :group 'netrc)

(defun netrc-encrypt (plain-file encrypted-file)
  (interactive "fPlain File: \nFEncrypted File: ")
  "Encrypt FILE to ENCRYPTED-FILE with netrc-encrypting-method cipher."
  (when (and (file-exists-p plain-file)
	     (stringp encrypted-file)
	     netrc-encrypting-method
	     netrc-openssl-path)
    (let ((buffer-file-coding-system 'binary)
	  (coding-system-for-read 'binary)
	  (coding-system-for-write 'binary)
	  (password 
	   (password-read
	    (format "OpenSSL Password for cipher %s? "
		    netrc-encrypting-method)
	    (format "netrc-openssl-password-%s"
		    netrc-encrypting-method))))
      (when password
	(with-temp-buffer
	  (insert-file-contents plain-file)
	  (setenv "NETRC_OPENSSL_PASSWORD" password)
	  (shell-command-on-region 
	   (point-min) 
	   (point-max)
	   (format "%s %s -pass env:NETRC_OPENSSL_PASSWORD -e"
		   netrc-openssl-path
		   netrc-encrypting-method)
	   t
	   t)
	  (write-file encrypted-file t))))))

(defun netrc-parse (file)
  (interactive "fFile to Parse: ")
  "Parse FILE and return an list of all entries in the file."
  (when (file-exists-p file)
    (with-temp-buffer
      (let ((tokens '("machine" "default" "login"
		      "password" "account" "macdef" "force"
		      "port"))
	    alist elem result pair)
	(if (and netrc-encrypting-method
		 netrc-openssl-path)
	    (let ((buffer-file-coding-system 'binary)
		  (coding-system-for-read 'binary)
		  (coding-system-for-write 'binary)
		  (password 
		   (password-read
		    (format "OpenSSL Password for cipher %s? "
			    netrc-encrypting-method)
		    (format "netrc-openssl-password-%s" 
			    netrc-encrypting-method))))
	      (when password
		(insert-file-contents file)
		(setenv "NETRC_OPENSSL_PASSWORD" password)
		(shell-command-on-region
		 (point-min) 
		 (point-max)
		 (format "%s %s -pass env:NETRC_OPENSSL_PASSWORD -d"
			 netrc-openssl-path
			 netrc-encrypting-method)
		 t
		 t)))
	  (insert-file-contents file))
	(goto-char (point-min))
	;; Go through the file, line by line.
	(while (not (eobp))
	  (narrow-to-region (point) (point-at-eol))
	  ;; For each line, get the tokens and values.
	  (while (not (eobp))
	    (skip-chars-forward "\t ")
	    ;; Skip lines that begin with a "#".
	    (if (eq (char-after) ?#)
		(goto-char (point-max))
	      (unless (eobp)
		(setq elem
		      (if (= (following-char) ?\")
			  (read (current-buffer))
			(buffer-substring
			 (point) (progn (skip-chars-forward "^\t ")
					(point)))))
		(cond
		 ((equal elem "macdef")
		  ;; We skip past the macro definition.
		  (widen)
		  (while (and (zerop (forward-line 1))
			      (looking-at "$")))
		  (narrow-to-region (point) (point)))
		 ((member elem tokens)
		  ;; Tokens that don't have a following value are ignored,
		  ;; except "default".
		  (when (and pair (or (cdr pair)
				      (equal (car pair) "default")))
		    (push pair alist))
		  (setq pair (list elem)))
		 (t
		  ;; Values that haven't got a preceding token are ignored.
		  (when pair
		    (setcdr pair elem)
		    (push pair alist)
		    (setq pair nil)))))))
	  (when alist
	    (push (nreverse alist) result))
	  (setq alist nil
		pair nil)
	  (widen)
	  (forward-line 1))
	(nreverse result)))))

(defun netrc-machine (list machine &optional port defaultport)
  "Return the netrc values from LIST for MACHINE or for the default entry.
If PORT specified, only return entries with matching port tokens.
Entries without port tokens default to DEFAULTPORT."
  (let ((rest list)
	result)
    (while list
      (when (equal (cdr (assoc "machine" (car list))) machine)
	(push (car list) result))
      (pop list))
    (unless result
      ;; No machine name matches, so we look for default entries.
      (while rest
	(when (assoc "default" (car rest))
	  (push (car rest) result))
	(pop rest)))
    (when result
      (setq result (nreverse result))
      (while (and result
		  (not (equal (or port defaultport "nntp")
			      (or (netrc-get (car result) "port")
				  defaultport "nntp"))))
	(pop result))
      (car result))))

(defun netrc-get (alist type)
  "Return the value of token TYPE from ALIST."
  (cdr (assoc type alist)))

(provide 'netrc)

;;; netrc.el ends here
