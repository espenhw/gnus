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
  (if (not (fboundp 'base64-encode-string))
      (require 'base64)))
(require 'mm-util)
(require 'rfc2047)
(require 'qp)

(defun mm-encode-body ()
  "Encode a body.
Should be called narrowed to the body that is to be encoded.
If there is more than one non-ASCII MULE charset, then list of found
MULE charsets are returned.
If successful, the MIME charset is returned.
If no encoding was done, nil is returned."
  (save-excursion
    (goto-char (point-min))
    (let ((charsets
	   (delq 'ascii (find-charset-region (point-min) (point-max))))
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
	      (if (eq (char-charset (following-char)) 'ascii)
		  (when start
		    (mm-encode-coding-region start (point) mime-charset)
		    (setq start nil))
		(unless start
		  (setq start (point))))
	      (forward-char 1))
	    (when start
	      (mm-encode-coding-region start (point) mime-charset)
	      (setq start nil)))
	  mime-charset))))))

(defun mm-body-encoding ()
  "Return the encoding of the current buffer."
  (if (and  
       (null (delq 'ascii (find-charset-region (point-min) (point-max))))
       ;;;!!!The following is necessary because the function
       ;;;!!!above seems to return the wrong result under Emacs 20.3.
       ;;;!!!Sometimes.
       (save-excursion
	 (goto-char (point-min))
	 (skip-chars-forward "\0-\177")
	 (eobp)))
      '7bit
    '8bit))

;;;
;;; Functions for decoding
;;;

(defun mm-decode-content-transfer-encoding (encoding)
  (cond
   ((eq encoding 'quoted-printable)
    (quoted-printable-decode-region (point-min) (point-max)))
   ((eq encoding 'base64)
    (condition-case ()
	(base64-decode-region (point-min) (point-max))
      (error nil)))
   ((memq encoding '(7bit 8bit binary))
    )
   ((null encoding)
    )
   (t
    (error "Can't decode encoding %s" encoding))))

(defun mm-decode-body (charset encoding)
  "Decode the current article that has been encoded with ENCODING.
The characters in CHARSET should then be decoded."
  (setq charset (or charset rfc2047-default-charset))
  (save-excursion
    (when encoding
      (mm-decode-content-transfer-encoding encoding))
    (when (featurep 'mule)
      (let (mule-charset)
	(when (and charset
		   (setq mule-charset (mm-charset-to-coding-system charset))
		   buffer-file-coding-system
		   ;;(not (mm-coding-system-equal
		   ;;	 buffer-file-coding-system mule-charset))
		   )
	  (mm-decode-coding-region (point-min) (point-max) mule-charset))))))

(provide 'mm-bodies)

;; mm-bodies.el ends here
