;;; pgg-gpg.el --- GnuPG support for PGG.

;; Copyright (C) 1999, 2000, 2003 Free Software Foundation, Inc.

;; Author: Daiki Ueno <ueno@unixuser.org>
;; Created: 1999/10/28
;; Keywords: PGP, OpenPGP, GnuPG

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

;;; Code:

(eval-when-compile (require 'pgg))

(defgroup pgg-gpg ()
  "GnuPG interface"
  :group 'pgg)

(defcustom pgg-gpg-program "gpg" 
  "The GnuPG executable."
  :group 'pgg-gpg
  :type 'string)

(defcustom pgg-gpg-extra-args nil
  "Extra arguments for every GnuPG invocation."
  :group 'pgg-gpg
  :type '(choice
	  (const :tag "None" nil)
	  (string :tag "Arguments")))

(defvar pgg-gpg-user-id nil
  "GnuPG ID of your default identity.")

(defun pgg-gpg-process-region (start end passphrase program args)
  (let* ((output-file-name
	  (expand-file-name (make-temp-name "pgg-output") 
			    pgg-temporary-file-directory))
	 (args
	  `("--status-fd" "2"
	    ,@(if passphrase '("--passphrase-fd" "0"))
	    "--output" ,output-file-name
	    ,@pgg-gpg-extra-args ,@args))
	 (output-buffer pgg-output-buffer)
	 (errors-buffer pgg-errors-buffer)
	 (orig-mode (default-file-modes))
	 (process-connection-type nil)
	 exit-status)
    (with-current-buffer (get-buffer-create errors-buffer)
      (buffer-disable-undo)
      (erase-buffer))
    (unwind-protect
	(progn
	  (set-default-file-modes 448)
          (let* ((coding-system-for-write 'binary)
                 (input (buffer-substring-no-properties start end)))
            (with-temp-buffer
              (when passphrase
                (insert passphrase "\n"))
              (insert input)
              (setq exit-status
                    (apply #'call-process-region (point-min) (point-max) program
                           nil errors-buffer nil args))))
	  (with-current-buffer (get-buffer-create output-buffer)
	    (buffer-disable-undo)
	    (erase-buffer)
	    (if (file-exists-p output-file-name)
		(let ((coding-system-for-read 'raw-text-dos))
		  (insert-file-contents output-file-name)))
	    (set-buffer errors-buffer)
	    (if (not (equal exit-status 0))
		(insert (format "\n%s exited abnormally: '%s'\n"
                                program exit-status)))))
      (if (file-exists-p output-file-name)
	  (delete-file output-file-name))
      (set-default-file-modes orig-mode))))

(defun pgg-gpg-possibly-cache-passphrase (passphrase)
  (if (and pgg-cache-passphrase
	   (progn
	     (goto-char (point-min))
	     (re-search-forward "^\\[GNUPG:] GOOD_PASSPHRASE\\>" nil t)))
      (pgg-add-passphrase-cache
       (progn
	 (goto-char (point-min))
	 (if (re-search-forward
	      "^\\[GNUPG:] NEED_PASSPHRASE \\w+ ?\\w*" nil t)
	     (substring (match-string 0) -8)))
       passphrase)))

(defun pgg-gpg-lookup-key (string &optional type)
  "Search keys associated with STRING."
  (let ((args (list "--with-colons" "--no-greeting" "--batch"
		    (if type "--list-secret-keys" "--list-keys")
		    string)))
    (with-temp-buffer
      (apply #'call-process pgg-gpg-program nil t nil args)
      (goto-char (point-min))
      (if (re-search-forward "^\\(sec\\|pub\\):"  nil t)
	  (substring
	   (nth 3 (split-string
		   (buffer-substring (match-end 0)
				     (progn (end-of-line)(point)))
		   ":")) 8)))))

(defun pgg-gpg-encrypt-region (start end recipients &optional sign)
  "Encrypt the current region between START and END.
If optional argument SIGN is non-nil, do a combined sign and encrypt."
  (let* ((pgg-gpg-user-id (or pgg-gpg-user-id pgg-default-user-id))
	 (passphrase
	  (when sign
	    (pgg-read-passphrase
	     (format "GnuPG passphrase for %s: " pgg-gpg-user-id)
	     (pgg-gpg-lookup-key pgg-gpg-user-id 'encrypt))))
	 (args
	  (append
	   (list "--batch" "--armor" "--always-trust" "--encrypt")
	   (if sign (list "--sign" "--local-user" pgg-gpg-user-id))
	   (if recipients
	       (apply #'nconc
		      (mapcar (lambda (rcpt)
				(list "--remote-user" rcpt))
			      (append recipients
				      (if pgg-encrypt-for-me
					  (list pgg-gpg-user-id)))))))))
    (pgg-as-lbt start end 'CRLF
      (pgg-gpg-process-region start end passphrase pgg-gpg-program args))
    (when sign
      (with-current-buffer pgg-errors-buffer
	(pgg-gpg-possibly-cache-passphrase passphrase)))
    (pgg-process-when-success)))

(defun pgg-gpg-decrypt-region (start end)
  "Decrypt the current region between START and END."
  (let* ((pgg-gpg-user-id (or pgg-gpg-user-id pgg-default-user-id))
	 (passphrase
	  (pgg-read-passphrase
	   (format "GnuPG passphrase for %s: " pgg-gpg-user-id)
	   (pgg-gpg-lookup-key pgg-gpg-user-id 'encrypt)))
	 (args '("--batch" "--decrypt")))
    (pgg-gpg-process-region start end passphrase pgg-gpg-program args)
    (with-current-buffer pgg-errors-buffer
      (pgg-gpg-possibly-cache-passphrase passphrase)
      (goto-char (point-min))
      (re-search-forward "^\\[GNUPG:] DECRYPTION_OKAY\\>" nil t))))

(defun pgg-gpg-sign-region (start end &optional cleartext)
  "Make detached signature from text between START and END."
  (let* ((pgg-gpg-user-id (or pgg-gpg-user-id pgg-default-user-id))
	 (passphrase
	  (pgg-read-passphrase
	   (format "GnuPG passphrase for %s: " pgg-gpg-user-id)
	   (pgg-gpg-lookup-key pgg-gpg-user-id 'sign)))
	 (args
	  (list (if cleartext "--clearsign" "--detach-sign")
		"--armor" "--batch" "--verbose"
		"--local-user" pgg-gpg-user-id))
	 (inhibit-read-only t)
	 buffer-read-only)
    (pgg-as-lbt start end 'CRLF
      (pgg-gpg-process-region start end passphrase pgg-gpg-program args))
    (with-current-buffer pgg-errors-buffer
      (pgg-gpg-possibly-cache-passphrase passphrase))
    (pgg-process-when-success)))

(defun pgg-gpg-verify-region (start end &optional signature)
  "Verify region between START and END as the detached signature SIGNATURE."
  (let ((args '("--batch" "--verify")))
    (when (stringp signature)
      (setq args (append args (list signature))))
    (setq args (append args '("-")))
    (pgg-gpg-process-region start end nil pgg-gpg-program args)
    (with-current-buffer pgg-errors-buffer
      (goto-char (point-min))
      (while (re-search-forward "^gpg: \\(.*\\)\n" nil t)
	(with-current-buffer pgg-output-buffer
	  (insert-buffer-substring pgg-errors-buffer
				   (match-beginning 1) (match-end 0)))
	(delete-region (match-beginning 0) (match-end 0)))
      (goto-char (point-min))
      (re-search-forward "^\\[GNUPG:] GOODSIG\\>" nil t))))

(defun pgg-gpg-insert-key ()
  "Insert public key at point."
  (let* ((pgg-gpg-user-id (or pgg-gpg-user-id pgg-default-user-id))
	 (args (list "--batch" "--export" "--armor"
		     pgg-gpg-user-id)))
    (pgg-gpg-process-region (point)(point) nil pgg-gpg-program args)
    (insert-buffer-substring pgg-output-buffer)))

(defun pgg-gpg-snarf-keys-region (start end)
  "Add all public keys in region between START and END to the keyring."
  (let ((args '("--import" "--batch" "-")) status)
    (pgg-gpg-process-region start end nil pgg-gpg-program args)
    (set-buffer pgg-errors-buffer)
    (goto-char (point-min))
    (when (re-search-forward "^\\[GNUPG:] IMPORT_RES\\>" nil t)
      (setq status (buffer-substring (match-end 0)
				     (progn (end-of-line)(point)))
	    status (vconcat (mapcar #'string-to-int (split-string status))))
      (erase-buffer)
      (insert (format "Imported %d key(s).
\tArmor contains %d key(s) [%d bad, %d old].\n"
		      (+ (aref status 2)
			 (aref status 10))
		      (aref status 0)
		      (aref status 1)
		      (+ (aref status 4)
			 (aref status 11)))
	      (if (zerop (aref status 9))
		  ""
		"\tSecret keys are imported.\n")))
    (append-to-buffer pgg-output-buffer (point-min)(point-max))
    (pgg-process-when-success)))

(provide 'pgg-gpg)

;;; pgg-gpg.el ends here
