;;; mm-encode.el --- Functions for encoding MIME things
;; Copyright (C) 1998,99 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;;	MORIOKA Tomohiko <morioka@jaist.ac.jp>
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

;;; Code:

(require 'mail-parse)
(require 'mailcap)

(defvar mm-content-transfer-encoding-defaults
  '(("text/.*" quoted-printable)
    ("message/rfc822" quoted-printable)
    ("application/emacs-lisp" 8bit)
    ("application/x-patch" 8bit)
    (".*" base64))
  "Alist of regexps that match MIME types and their encodings.")

(defun mm-insert-rfc822-headers (charset encoding)
  "Insert text/plain headers with CHARSET and ENCODING."
  (insert "MIME-Version: 1.0\n")
  (insert "Content-Type: text/plain; charset="
	  (mail-quote-string (downcase (symbol-name charset))) "\n")
  (insert "Content-Transfer-Encoding: "
	  (downcase (symbol-name encoding)) "\n"))

(defun mm-insert-multipart-headers ()
  "Insert multipart/mixed headers."
  (let ((boundary "=-=-="))
    (insert "MIME-Version: 1.0\n")
    (insert (format "Content-Type: multipart/mixed; boundary=\"%s\"\n"
		    boundary))
    boundary))

(defun mm-default-file-encoding (file)
  "Return a default encoding for FILE."
  (if (not (string-match "\\.[^.]+$" file))
      "application/octet-stream"
    (mailcap-extension-to-mime (match-string 0 file))))

(defun mm-encode-content-transfer-encoding (encoding &optional type)
  (cond
   ((eq encoding 'quoted-printable)
    (quoted-printable-encode-region (point-min) (point-max)))
   ((eq encoding 'base64)
    (when (equal type "text/plain")
      (goto-char (point-min))
      (while (search-forward "\n" nil t)
	(replace-match "\r\n" t t)))
    (condition-case error
	(base64-encode-region (point-min) (point-max))
      (error
       (message "Error while decoding: %s" error)
       nil)))
   ((memq encoding '(7bit 8bit binary))
    )
   ((null encoding)
    )
   ((functionp encoding)
    (ignore-errors (funcall encoding (point-min) (point-max))))
   (t
    (message "Unknown encoding %s; defaulting to 8bit" encoding))))

(defun mm-encode-buffer (type)
  "Encode the buffer which contains data of TYPE.
The encoding used is returned."
  (let* ((mime-type (if (stringp type) type (car type)))
	 (encoding
	  (or (and (listp type)
		   (cadr (assq 'encoding type)))
	      (mm-content-transfer-encoding mime-type))))
    (mm-encode-content-transfer-encoding encoding mime-type)
    encoding))

(defun mm-insert-headers (type encoding &optional file)
  "Insert headers for TYPE."
  (insert "Content-Type: " type)
  (when file
    (insert ";\n\tname=\"" (file-name-nondirectory file) "\""))
  (insert "\n")
  (insert (format "Content-Transfer-Encoding: %s\n" encoding))
  (insert "Content-Disposition: inline")
  (when file
    (insert ";\n\tfilename=\"" (file-name-nondirectory file) "\""))
  (insert "\n")
  (insert "\n"))

(defun mm-content-transfer-encoding (type)
  "Return a CTE suitable for TYPE."
  (let ((rules mm-content-transfer-encoding-defaults))
    (catch 'found
      (while rules
	(when (string-match (caar rules) type)
	  (throw 'found (cadar rules)))
	(pop rules)))))

(provide 'mm-encode)

;;; mm-encode.el ends here
