;;; canlock.el --- Functions for Cancel-Lock feature.
;; Copyright (C) 1998,1999 Katsumi Yamaoka

;; Author: Katsumi Yamaoka   <yamaoka@jpl.org>
;;         Yuuichi Teranishi <teranisi@gohome.org>
;;         Hideyuki SHIRAI   <shirai@rdmg.mgcs.mei.co.jp>
;;         Hidekazu Nakamura <u90121@uis-inf.co.jp>
;;         Ken'ichi Okada    <kokada@tamaru.kuee.kyoto-u.ac.jp>
;;         Shuhei KOBAYASHI  <shuhei@aqua.ocn.ne.jp>
;; Created: 1998-11-24
;; Revised: 1999-06-14
;; Keywords: news, cancel-lock, hmac, sha1, rfc2104

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
;; USA.

;;; Commentary:

;; This library is based on draft-ietf-usefor-cancel-lock-01.txt,
;; released on 1998-11-03.

;;; Code:

(defconst canlock-version "0.6")

(eval-when-compile (require 'cl))
(require 'custom)
(require 'mail-utils)

(autoload 'sha1-encode-binary "sha1")
(autoload 'base64-encode "base64")

(defgroup canlock nil
  "Cancel-Lock feature."
  :prefix "canlock-"
  :group 'applications)

(defcustom canlock-base64-encode-function 'base64-encode-string
  "*Function called to encode string to base64."
  :type '(radio (function-item base64-encode-string)
		(function-item base64-encode)
		(function-item canlock-base64-encode-string-with-mmencode)
		(function :tag "Other"))
  :group 'canlock)

(defcustom canlock-mmencode-program "mmencode"
  "*Name of mmencode program."
  :type 'string
  :group 'canlock)

(defcustom canlock-mmencode-args-for-encoding nil
  "*Arguments passed to mmencode program for encoding."
  :type 'sexp
  :group 'canlock)

(defcustom canlock-sha1-function 'sha1-encode-binary
  "*Function called to make a SHA1 digest from a message (string)."
  :type '(radio (function-item sha1-encode-binary)
		(function-item canlock-sha1-with-ssleay)
		(function :tag "Other"))
  :group 'canlock)

(defcustom canlock-sha1-function-for-verify canlock-sha1-function
  "*Function called to make a SHA1 digest for verifying."
  :type '(radio (function-item sha1-encode-binary)
		(function-item canlock-sha1-with-ssleay)
		(function :tag "Other"))
  :group 'canlock)

(defcustom canlock-ssleay-program "ssleay"
  "*Name of SSLeay program."
  :type 'string
  :group 'canlock)

(defcustom canlock-ssleay-args '("sha1")
  "*Arguments passed to SSLeay program."
  :type 'sexp
  :group 'canlock)

(defcustom canlock-ignore-errors nil
  "*If non-nil, ignore any error signals."
  :type 'boolean
  :group 'canlock)

(defcustom canlock-load-hook nil
  "*Hook to be run after the canlock package has been loaded."
  :type 'hook
  :group 'canlock)

;;; Internal variables.

(defvar canlock-password nil
  "*Password to use when signing a Cancel-Lock or a Cancel-Key header.")

(defvar canlock-password-for-verify canlock-password
  "*Password to use when verifying a Cancel-Lock or a Cancel-Key header.")

(defvar canlock-force-insert-header nil
  "*If non-nil, insert a Cancel-Lock or a Cancel-Key header even though the
buffer does not contain a news message.")

;;; Functions.

(defun canlock-base64-encode-string-with-mmencode (string)
  "Encode string to base64 with mmencode."
  (with-temp-buffer
    (let ((coding-system-for-read 'raw-text)
	  (coding-system-for-write 'binary)
	  ;; For Mule 2 with APEL 9.12 or later.
	  (default-process-coding-system '(raw-text . binary))
	  mc-flag program-coding-system-alist)
      (insert string)
      (apply 'call-process-region (point-min) (point-max)
	     canlock-mmencode-program t t nil
	     canlock-mmencode-args-for-encoding)
      (goto-char (point-max))
      (skip-chars-backward "\n")
      (buffer-substring (point-min) (point)))))

(defun canlock-hex-string-to-int (string)
  "Convert hexadecimal string to integer."
  (let ((integer 0))
    (mapcar
     (lambda (hex)
       (setq integer (+ (* 16 integer)
			(logand hex 15)
			(* (lsh hex -6) 9))))
     string)
    integer))

(defun canlock-sha1-with-ssleay (message)
  "Make a SHA1 digest from a specified message (string) with SSLeay."
  (with-temp-buffer
    (let ((coding-system-for-read 'binary)
	  (coding-system-for-write 'binary)
	  ;; For Mule 2 with APEL 9.12 or later.
	  (default-process-coding-system '(binary . binary))
	  mc-flag program-coding-system-alist
	  (case-fold-search t))
      (insert message)
      (apply 'call-process-region (point-min) (point-max)
	     canlock-ssleay-program t t nil canlock-ssleay-args)
      (goto-char (point-min))
      (while (re-search-forward "[0-9A-F][0-9A-F]" nil t)
	(goto-char (match-beginning 0))
	(insert-char (canlock-hex-string-to-int (match-string 0)) 1)
	(delete-char 2))
      (buffer-substring (point-min) (point)))))

(defvar canlock-read-passwd nil)
(defun canlock-read-passwd (prompt &rest args)
  "Read a password using PROMPT.
If ARGS, PROMPT is used as an argument to `format'."
  (let ((prompt
	 (if args
	     (apply 'format prompt args)
	   prompt)))
    (unless canlock-read-passwd
      (if (or (fboundp 'read-passwd) (load "passwd" t))
	  (setq canlock-read-passwd 'read-passwd)
	(unless (fboundp 'ange-ftp-read-passwd)
	  (autoload 'ange-ftp-read-passwd "ange-ftp"))
	(setq canlock-read-passwd 'ange-ftp-read-passwd)))
    (funcall canlock-read-passwd prompt)))

(defun canlock-make-cancel-key (message-id password)
  "Make a Cancel-Key header."
  (cond ((> (length password) 20)
	 (setq password (funcall canlock-sha1-function password)))
	((< (length password) 20)
	 (setq password (concat
			 password
			 (make-string (- 20 (length password)) 0)))))
  (setq password (concat password (make-string 44 0)))
  (let ((ipad (mapconcat (lambda (char)
			   (char-to-string (logxor 54 char)))
			 password ""))
	(opad (mapconcat (lambda (char)
			   (char-to-string (logxor 92 char)))
			 password "")))
    (funcall canlock-base64-encode-function
	     (funcall canlock-sha1-function
		      (concat
		       opad
		       (funcall canlock-sha1-function
				(concat ipad message-id)))))))

(defun canlock-narrow-to-header ()
  "Narrow to the message header."
  (let (case-fold-search)
    (narrow-to-region
     (goto-char (point-min))
     (goto-char (if (re-search-forward
		     (format "^$\\|^%s$"
			     (regexp-quote mail-header-separator))
		     nil t)
		    (match-beginning 0)
		  (point-max))))))

(defun canlock-delete-headers ()
  "Delete Canlock headers if they already exist.
The buffer is expected to be narrowed to just the headers of the message."
  (let ((case-fold-search t))
    (goto-char (point-min))
    (while (re-search-forward "^Cancel-\\(Key\\|Lock\\):" nil t)
      (delete-region (match-beginning 0)
		     (if (re-search-forward "^[^\t ]" nil t)
			 (goto-char (match-beginning 0))
		       (point-max))))))

(defun canlock-fetch-fields (&optional key)
  "Return the list of values of Cancel-Lock field.
If the optional arg KEY is non-nil, Cancel-Key field will be fetched.
The buffer is expected to be narrowed to just the headers of the message."
  (let ((feild (mail-fetch-field (if key "Cancel-Key" "Cancel-Lock")))
	(case-fold-search t))
    (when feild
      (mapcar (lambda (str)
		(string-match "^sha1:" str)
		(substring str (match-end 0)))
	      (split-string feild "[\t\n\r ,]+")))))

(defun canlock-fetch-id-for-key ()
  "Return the Message-ID for Cancel-Key.
The buffer is expected to be narrowed to just the headers of the message."
  (let ((cancel (mail-fetch-field "Control")))
    (if cancel
	(progn
	  (string-match "^cancel[\t ]+\\(<[^\t\n @<>]+@[^\t\n @<>]+>\\)"
			cancel)
	  (match-string 1 cancel))
      (or (mail-fetch-field "Supersedes")
	  (mail-fetch-field "Replaces")))))

;;;###autoload
(defun canlock-insert-header (&optional id-for-key id-for-lock password)
  "Insert a Cancel-Key and/or a Cancel-Lock header if possible."
  (let (news control key-for-key key-for-lock)
    (save-excursion
      (save-restriction
	(canlock-narrow-to-header)
	(when (setq news (or canlock-force-insert-header
			     (mail-fetch-field "Newsgroups")))
	  (unless id-for-key
	    (setq id-for-key (canlock-fetch-id-for-key)))
	  (if (and (setq control (mail-fetch-field "Control"))
		   (string-match
		    "^cancel[\t ]+\\(<[^\t\n @<>]+@[^\t\n @<>]+>\\)"
		    control))
	      (setq id-for-lock nil)
	    (unless id-for-lock
	      (setq id-for-lock (mail-fetch-field "Message-ID"))))
	  (canlock-delete-headers)
	  (goto-char (point-max))))
      (when news
	(if (not (or id-for-key id-for-lock))
	    (message "There are no Message-ID(s).")
	  (unless password
	    (setq password (or canlock-password
			       (canlock-read-passwd
				"Password for Canlock: "))))
	  (if (or (not (stringp password)) (zerop (length password)))
	      (message "Password for Canlock is bad.")
	    (setq key-for-key (when id-for-key
				(canlock-make-cancel-key
				 id-for-key password))
		  key-for-lock (when id-for-lock
				 (canlock-make-cancel-key
				  id-for-lock password)))
	    (if (not (or key-for-key key-for-lock))
		(message "Couldn't insert Canlock header.")
	      (when key-for-key
		(insert "Cancel-Key: sha1:" key-for-key "\n"))
	      (when key-for-lock
		(insert "Cancel-Lock: sha1:"
			(funcall canlock-base64-encode-function
				 (funcall canlock-sha1-function
					  key-for-lock))
			"\n")))))))))

;;;###autoload
(defun canlock-verify (&optional buffer)
  "Verify Cancel-Lock or Cancel-Key. If failed, returns non-nil or signals
an error if `canlock-ignore-errors' is nil.  If the optional arg BUFFER
is not specified, it runs in place."
  (interactive)
  (let ((canlock-sha1-function (or canlock-sha1-function-for-verify
				   canlock-sha1-function))
	keys locks errmsg id-for-key id-for-lock password
	key-for-key key-for-lock match)
    (save-excursion
      (when buffer
	(set-buffer buffer))
      (save-restriction
	(widen)
	(canlock-narrow-to-header)
	(setq keys (canlock-fetch-fields 'key)
	      locks (canlock-fetch-fields))
	(if (not (or keys locks))
	    (setq errmsg
		  "There are neither Cancel-Lock nor Cancel-Key fields.")
	  (setq id-for-key (canlock-fetch-id-for-key)
		id-for-lock (mail-fetch-field "Message-ID"))
	  (or id-for-key id-for-lock
	      (setq errmsg "There are no Message-ID(s).")))))

    (if errmsg
	(if canlock-ignore-errors
	    errmsg
	  (error "%s" errmsg))

      (setq password (or canlock-password-for-verify
			 (canlock-read-passwd "Password for Canlock: ")))
      (if (or (not (stringp password)) (zerop (length password)))
	  (progn
	    (setq errmsg "Password for Canlock is bad.")
	    (if canlock-ignore-errors
		errmsg
	      (error "%s" errmsg)))

	(when keys
	  (when id-for-key
	    (setq key-for-key (canlock-make-cancel-key id-for-key password))
	    (while (and keys (not match))
	      (setq match (string-equal key-for-key (pop keys)))))
	  (setq keys (if match "good" "bad")))
	(setq match nil)

	(when locks
	  (when id-for-lock
	    (setq key-for-lock
		  (funcall canlock-base64-encode-function
			   (funcall canlock-sha1-function
				    (canlock-make-cancel-key
				     id-for-lock password))))
	    (when (and locks (not match))
	      (setq match (string-equal key-for-lock (pop locks)))))
	  (setq locks (if match "good" "bad")))

	(prog1
	    (when (member "bad" (list keys locks))
	      "bad")
	  (cond ((and keys locks)
		 (message "Cancel-Key is %s, Cancel-Lock is %s." keys locks))
		(locks
		 (message "Cancel-Lock is %s." locks))
		(keys
		 (message "Cancel-Key is %s." keys))))))))

;; Avoid byte compile warnings.
(defvar gnus-show-all-headers)
(defvar gnus-original-article-buffer)
(defvar mh-show-buffer)
(defvar vm-mail-buffer)
(defvar vm-message-pointer)
(defvar cmail-current-folder)
(defvar rmail-buffer)

;;;###autoload
(defun gnus-summary-canlock-verify ()
  "Run `canlock-verify' from gnus summary buffer."
  (interactive)
  (gnus-summary-select-article gnus-show-all-headers)
  (canlock-verify gnus-original-article-buffer))

;;;###autoload
(defun wl-summary-canlock-verify ()
  "Run `canlock-verify' from Wanderlust summary buffer."
  (interactive)
  (wl-summary-set-message-buffer-or-redisplay)
  (canlock-verify (wl-message-get-original-buffer)))

(eval-when-compile
  (if (or (featurep 'use-mew-1.94b20-or-later)
	  (and (fboundp 'function-max-args)
	       (or (fboundp 'mew-summary-display)
		   (load "mew-summary" t))
	       (eq 2 (function-max-args 'mew-summary-display))))
      (progn
	(defmacro canlock-mew-summary-display ()
	  '(mew-summary-display t))
	(message "Use mew-1.94b20 or later."))
    (defmacro canlock-mew-summary-display ()
      '(condition-case nil
	   (mew-summary-display)
	 (wrong-number-of-arguments
	  (mew-summary-display t))))
    ))

;;;###autoload
(defun mew-summary-canlock-verify ()
  "Run `canlock-verify' from Mew summary buffer."
  (interactive)
  (canlock-mew-summary-display)
  (canlock-verify (mew-buffer-message)))

;;;###autoload
(defun mh-summary-canlock-verify ()
  "Run `canlock-verify' from MH folder buffer."
  (interactive)
  (mh-header-display)
  (canlock-verify mh-show-buffer))

;;;###autoload
(defun vm-summary-canlock-verify ()
  "Run `canlock-verify' from VM summary buffer."
  (interactive)
  (vm-follow-summary-cursor)
  (if (and vm-mail-buffer (buffer-name vm-mail-buffer))
      (save-excursion
	(set-buffer vm-mail-buffer)
	(let* ((mp (car vm-message-pointer))
	       (header (save-restriction
			 (widen)
			 (buffer-substring
			  (aref (aref mp 0) 0) (vm-text-of mp)))))
	  (with-temp-buffer
	    (insert header)
	    (canlock-verify))))
    (or canlock-ignore-errors
	(error "Folder buffer has been killed."))))

;;;###autoload
(defun cmail-summary-canlock-verify ()
  "Run `canlock-verify' from cmail summary buffer."
  (interactive)
  (let* ((page (cmail-get-page-number-from-summary))
	 (header (save-excursion
		   (set-buffer (cmail-folder-buffer cmail-current-folder))
		   (cmail-n-page page)
		   (buffer-substring (point)
				     (if (search-forward "\n\n" nil t)
					 (1- (point))
				       (point-max))))))
    (with-temp-buffer
      (insert header)
      (canlock-verify))))

;;;###autoload
(defun rmail-summary-canlock-verify ()
  "Run `canlock-verify' from RMAIL summary buffer."
  (interactive)
  (rmail-summary-rmail-update)
  (let ((header (save-excursion
		  (set-buffer rmail-buffer)
		  (goto-char (point-min))
		  (save-restriction
		    (widen)
		    (search-backward "\n\C-_\C-l\n") ;; ^_^L
		    (re-search-forward "^[^\t\n ]+:")
		    (buffer-substring
		     (goto-char (match-beginning 0))
		     (progn (search-forward "\n\n")
			    (1- (point))))))))
    (with-temp-buffer
      (insert header)
      (canlock-verify))))

(provide 'canlock)

(run-hooks 'canlock-load-hook)

;;; canlock.el ends here
