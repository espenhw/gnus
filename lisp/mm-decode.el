;;; mm-decode.el --- Function for decoding MIME things
;; Copyright (C) 1998 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; This file is not yet part of GNU Emacs.

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

(require 'base64)
(require 'qp)
(require 'nnheader)

(defvar mm-charset-regexp (concat "[^" "][\000-\040()<>@,\;:\\\"/?.=" "]+"))

(defvar mm-encoded-word-regexp
  (concat "=\\?\\(" mm-charset-regexp "\\)\\?\\(B\\|Q\\)\\?"
	  "\\([!->@-~]+\\)\\?="))

(defun mm-decode-words-region (start end)
  "Decode MIME-encoded words in region between START and END."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      ;; Remove whitespace between encoded words.
      (while (re-search-forward
	      (concat "\\(" mm-encoded-word-regexp "\\)"
		      "\\(\n?[ \t]\\)+"
		      "\\(" mm-encoded-word-regexp "\\)")
	      nil t)
	(delete-region (goto-char (match-end 1)) (match-beginning 6)))
      ;; Decode the encoded words.
      (goto-char (point-min))
      (while (re-search-forward mm-encoded-word-regexp nil t)
	(insert (mm-decode-word
		 (prog1
		     (match-string 0)
		   (delete-region (match-beginning 0) (match-end 0)))))))))

(defun mm-decode-words-string (string)
 "Decode the quoted-printable-encoded STRING and return the results."
 (with-temp-buffer
   (insert string)
   (inline
     (mm-decode-words-region (point-min) (point-max)))
   (buffer-string)))

(defun mm-decode-word (word)
  "Decode WORD and return it if it is an encoded word.
Return WORD if not."
  (if (not (string-match mm-encoded-word-regexp word))
      word
    (or
     (condition-case nil
	 (mm-decode-text
	  (match-string 1 word)
	  (upcase (match-string 2 word))
	  (match-string 3 word))
       (error word))
     word)))

(eval-and-compile
  (if (fboundp 'decode-coding-string)
      (fset 'mm-decode-coding-string 'decode-coding-string)
    (fset 'mm-decode-coding-string (lambda (s a) s))))

(defun mm-decode-text (charset encoding string)
  "Decode STRING as an encoded text.
Valid ENCODINGs are \"B\" and \"Q\".
If your Emacs implementation can't decode CHARSET, it returns nil."
  (let ((cs (mm-charset-to-coding-system charset)))
    (when cs
      (mm-decode-coding-string
       (cond
	((equal "B" encoding)
	 (base64-decode string))
	((equal "Q" encoding)
	 (quoted-printable-decode-string
	  (nnheader-replace-chars-in-string string ?_ ? )))
	(t (error "Invalid encoding: %s" encoding)))
       cs))))

(defvar mm-charset-coding-system-alist
  (let ((rest
	 '((us-ascii . iso-8859-1)
	   (gb2312 . cn-gb-2312)
	   (iso-2022-jp-2 . iso-2022-7bit-ss2)
	   (x-ctext . ctext)))
	(systems (coding-system-list))
	dest)
    (while rest
      (let ((pair (car rest)))
	(unless (memq (car pair) systems)
	  (setq dest (cons pair dest))))
      (setq rest (cdr rest)))
    dest)
  "Charset/coding system alist.")

(defun mm-charset-to-coding-system (charset &optional lbt)
  "Return coding-system corresponding to CHARSET.
CHARSET is a symbol naming a MIME charset.
If optional argument LBT (`unix', `dos' or `mac') is specified, it is
used as the line break code type of the coding system."
  (when (stringp charset)
    (setq charset (intern (downcase charset))))
  (setq charset
	(or (cdr (assq charset mm-charset-coding-system-alist))
	    charset))
  (when lbt
    (setq charset (intern (format "%s-%s" charset lbt))))
  (when (memq charset (coding-system-list))
    charset))

(provide 'mm-decode)

;; qp.el ends here
