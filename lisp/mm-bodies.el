;;; mm-bodies.el --- Functions for decoding MIME things
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

(eval-and-compile
  (or (fboundp  'base64-decode-region)
      (require 'base64))
  (autoload 'binhex-decode-region "binhex"))

(require 'mm-util)
(require 'rfc2047)
(require 'qp)
(require 'uudecode)

;; 8bit treatment gets any char except: 0x32 - 0x7f, CR, LF, TAB, BEL,
;; BS, vertical TAB, form feed, and ^_
(defvar mm-8bit-char-regexp "[^\x20-\x7f\r\n\t\x7\x8\xb\xc\x1f]")

(defvar mm-body-charset-encoding-alist
  '((us-ascii . 7bit)
    (iso-8859-1 . quoted-printable)
    (iso-8859-2 . quoted-printable)
    (iso-8859-3 . quoted-printable)
    (iso-8859-4 . quoted-printable)
    (iso-8859-5 . base64)
    (koi8-r . 8bit)
    (iso-8859-7 . quoted-printable)
    (iso-8859-8 . quoted-printable)
    (iso-8859-9 . quoted-printable)
    (iso-2022-jp . base64)
    (iso-2022-kr . base64)
    (gb2312 . base64)
    (cn-gb . base64)
    (cn-gb-2312 . base64)
    (euc-kr . 8bit)
    (iso-2022-jp-2 . base64)
    (iso-2022-int-1 . base64))
  "Alist of MIME charsets to encodings.
Valid encodings are `7bit', `8bit', `quoted-printable' and `base64'.")

(defun mm-encode-body ()
  "Encode a body.
Should be called narrowed to the body that is to be encoded.
If there is more than one non-ASCII MULE charset, then list of found
MULE charsets are returned.
If successful, the MIME charset is returned.
If no encoding was done, nil is returned."
  (if (not (featurep 'mule))
      ;; In the non-Mule case, we search for non-ASCII chars and
      ;; return the value of `mm-default-charset' if any are found.
      (save-excursion
	(goto-char (point-min))
	(if (re-search-forward "[^\x0-\x7f]" nil t)
	    (or mail-parse-charset
		(mm-read-charset "Charset used in the article: "))
	  ;; The logic in `mml-generate-mime-1' confirms that it's OK
	  ;; to return nil here.
	  nil))
    (save-excursion
      (goto-char (point-min))
      (let ((charsets (mm-find-mime-charset-region (point-min) (point-max)))
	    charset)
	(cond
	 ;; No encoding.
	 ((null charsets)
	  nil)
	 ;; Too many charsets.
	 ((> (length charsets) 1)
	  charsets)
	 ;; We encode.
	 (t
	  (let ((charset (car charsets))
		start)
	    (when (or t
		      ;; We always decode.
		      (not (mm-coding-system-equal
			    charset buffer-file-coding-system)))
	      (while (not (eobp))
		(if (eq (char-charset (char-after)) 'ascii)
		    (when start
		      (save-restriction
			(narrow-to-region start (point))
			(mm-encode-coding-region start (point) charset)
			(goto-char (point-max)))
		      (setq start nil))
		  (unless start
		    (setq start (point))))
		(forward-char 1))
	      (when start
		(mm-encode-coding-region start (point) charset)
		(setq start nil)))
	    charset)))))))

(defun mm-body-encoding (charset)
  "Do Content-Transfer-Encoding and return the encoding of the current buffer."
  (let ((bits (mm-body-7-or-8)))
    (cond
     ((eq bits '7bit)
      bits)
     ((eq charset mail-parse-charset)
      bits)
     (t
      (let ((encoding (or (cdr (assq charset mm-body-charset-encoding-alist))
			  'quoted-printable)))
	(mm-encode-content-transfer-encoding encoding "text/plain")
	encoding)))))

(defun mm-body-7-or-8 ()
  "Say whether the body is 7bit or 8bit."
  (cond
   ((not (featurep 'mule))
    (if (save-excursion
	  (goto-char (point-min))
	  (re-search-forward mm-8bit-char-regexp nil t))
	'8bit
      '7bit))
   (t
    ;; Mule version
    (if (and (null (delq 'ascii
			 (mm-find-charset-region (point-min) (point-max))))
	     ;;!!!The following is necessary because the function
	     ;;!!!above seems to return the wrong result under
	     ;;!!!Emacs 20.3.  Sometimes.
	     (save-excursion
	       (goto-char (point-min))
	       (skip-chars-forward "\0-\177")
	       (eobp)))
	'7bit
      '8bit))))

;;;
;;; Functions for decoding
;;;

(defun mm-decode-content-transfer-encoding (encoding &optional type)
  (prog1
      (condition-case error
	  (cond
	   ((eq encoding 'quoted-printable)
	    (quoted-printable-decode-region (point-min) (point-max)))
	   ((eq encoding 'base64)
	    (base64-decode-region (point-min) (point-max)))
	   ((memq encoding '(7bit 8bit binary))
	    )
	   ((null encoding)
	    )
	   ((eq encoding 'x-uuencode)
	    (funcall mm-uu-decode-function (point-min) (point-max)))
	   ((eq encoding 'x-binhex)
	    (funcall mm-uu-binhex-decode-function (point-min) (point-max)))
	   ((functionp encoding)
	    (funcall encoding (point-min) (point-max)))
	   (t
	    (message "Unknown encoding %s; defaulting to 8bit" encoding)))
	(error
	 (message "Error while decoding: %s" error)
	 nil))
    (when (and
	   (memq encoding '(base64 x-uuencode x-binhex))
	   (equal type "text/plain"))
      (goto-char (point-min))
      (while (search-forward "\r\n" nil t)
	(replace-match "\n" t t)))))

(defun mm-decode-body (charset &optional encoding type)
  "Decode the current article that has been encoded with ENCODING.
The characters in CHARSET should then be decoded."
  (setq charset (or charset mail-parse-charset))
  (save-excursion
    (when encoding
      (mm-decode-content-transfer-encoding encoding type))
    (when (featurep 'mule)
      (let (mule-charset)
	(when (and charset
		   (setq mule-charset (mm-charset-to-coding-system charset))
		   ;; buffer-file-coding-system
		   ;;Article buffer is nil coding system
		   ;;in XEmacs
		   enable-multibyte-characters
		   (or (not (eq mule-charset 'ascii))
		       (setq mule-charset mail-parse-charset)))
	  (mm-decode-coding-region (point-min) (point-max) mule-charset))))))

(defun mm-decode-string (string charset)
  "Decode STRING with CHARSET."
  (setq charset (or charset mail-parse-charset))
  (or
   (when (featurep 'mule)
     (let (mule-charset)
       (when (and charset
		  (setq mule-charset (mm-charset-to-coding-system charset))
		  enable-multibyte-characters
		  (or (not (eq mule-charset 'ascii))
		      (setq mule-charset mail-parse-charset)))
	 (mm-decode-coding-string string mule-charset))))
   string))

(provide 'mm-bodies)

;; mm-bodies.el ends here
