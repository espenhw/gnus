;;; mm-bodies.el --- Functions for decoding MIME things
;; Copyright (C) 1998 Free Software Foundation, Inc.

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
	    mm-default-charset
	  ;; The logic in `mml-generate-mime-1' confirms that it's OK
	  ;; to return nil here.
	  nil))
    (save-excursion
      (goto-char (point-min))
      (let ((charsets
	     (delq 'ascii (mm-find-charset-region (point-min) (point-max))))
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
	  (let ((mime-charset 
		 (mm-mime-charset (car charsets) (point-min) (point-max)))
		start)
	    (when (or t
		      ;; We always decode.
		      (not (mm-coding-system-equal
			    mime-charset buffer-file-coding-system)))
	      (while (not (eobp))
		(if (eq (char-charset (char-after)) 'ascii)
		    (when start
		      (save-restriction
			(narrow-to-region start (point))
			(mm-encode-coding-region start (point) mime-charset)
			(goto-char (point-max)))
		      (setq start nil))
		  (unless start
		    (setq start (point))))
		(forward-char 1))
	      (when start
		(mm-encode-coding-region start (point) mime-charset)
		(setq start nil)))
	    mime-charset)))))))

(defun mm-body-encoding ()
  "Return the encoding of the current buffer."
  (cond
   ((not (featurep 'mule))
    (if (save-excursion
	  (goto-char (point-min))
	  (re-search-forward mm-8bit-char-regexp nil t))
	'8bit
      '7bit))
   (t
    ;; Mule version
    (if (and (null (delq 'ascii (find-charset-region (point-min) (point-max))))
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
  (cond
   ((eq encoding 'quoted-printable)
    (quoted-printable-decode-region (point-min) (point-max)))
   ((eq encoding 'base64)
    (prog1
	(condition-case ()
	    (base64-decode-region (point-min) (point-max))
	  (error nil))
      (when (equal type "text/plain")
	(goto-char (point-min))
	(while (search-forward "\r\n" nil t)
	  (replace-match "\n" t t)))))
   ((memq encoding '(7bit 8bit binary))
    )
   ((null encoding)
    )
   ((eq encoding 'x-uuencode)
    (condition-case ()
	(uudecode-decode-region (point-min) (point-max))
      (error nil)))
   ((eq encoding 'x-binhex)
    (condition-case ()
	(binhex-decode-region (point-min) (point-max))
      (error nil)))
   ((functionp encoding)
    (condition-case ()
	(funcall encoding (point-min) (point-max))
      (error nil)))
   (t
    (message "Unknown encoding %s; defaulting to 8bit" encoding))))

(defun mm-decode-body (charset &optional encoding type)
  "Decode the current article that has been encoded with ENCODING.
The characters in CHARSET should then be decoded."
  (setq charset (or charset rfc2047-default-charset))
  (save-excursion
    (when encoding
      (mm-decode-content-transfer-encoding encoding type))
    (when (featurep 'mule)
      (let (mule-charset)
	(when (and charset
		   (setq mule-charset (mm-charset-to-coding-system charset))
		   ;; buffer-file-coding-system 
					;Article buffer is nil coding system
					;in XEmacs
		   enable-multibyte-characters
		   (or (not (eq mule-charset 'ascii))
		       (setq mule-charset rfc2047-default-charset)))
	  (mm-decode-coding-region (point-min) (point-max) mule-charset))))))

(defun mm-decode-string (string charset)
  "Decode STRING with CHARSET."
  (setq charset (or charset rfc2047-default-charset))
  (when (featurep 'mule)
    (let (mule-charset)
      (when (and charset
		 (setq mule-charset (mm-charset-to-coding-system charset))
		 enable-multibyte-characters
		 (or (not (eq mule-charset 'ascii))
		     (setq mule-charset rfc2047-default-charset)))
	(mm-decode-coding-string string mule-charset)))))

(provide 'mm-bodies)

;; mm-bodies.el ends here
