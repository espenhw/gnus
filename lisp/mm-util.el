;;; mm-util.el --- Utility functions for MIME things
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

(defvar mm-known-charsets '(iso-8859-1)
  "List of known charsets.")

(defvar mm-mime-mule-charset-alist
  '((us-ascii ascii)
    (iso-8859-1 latin-iso8859-1)
    (iso-8859-2 latin-iso8859-2)
    (iso-8859-3 latin-iso8859-3)
    (iso-8859-4 latin-iso8859-4)
    (iso-8859-5 cyrillic-iso8859-5)
    (koi8-r cyrillic-iso8859-5)
    (iso-8859-6 arabic-iso8859-6)
    (iso-8859-7 greek-iso8859-7)
    (iso-8859-8 hebrew-iso8859-8)
    (iso-8859-9 latin-iso8859-9)
    (iso-2022-jp latin-jisx0201
		 japanese-jisx0208-1978 japanese-jisx0208)
    (euc-kr korean-ksc5601)
    (cn-gb-2312 chinese-gb2312)
    (cn-big5 chinese-big5-1 chinese-big5-2)
    (iso-2022-jp-2 latin-iso8859-1 greek-iso8859-7
		   latin-jisx0201 japanese-jisx0208-1978
		   chinese-gb2312 japanese-jisx0208
		   korean-ksc5601 japanese-jisx0212)
    (iso-2022-int-1 latin-iso8859-1 greek-iso8859-7
		    latin-jisx0201 japanese-jisx0208-1978
		    chinese-gb2312 japanese-jisx0208
		    korean-ksc5601 japanese-jisx0212
		    chinese-cns11643-1 chinese-cns11643-2)
    (iso-2022-int-1 latin-iso8859-1 latin-iso8859-2
		    cyrillic-iso8859-5 greek-iso8859-7
		    latin-jisx0201 japanese-jisx0208-1978
		    chinese-gb2312 japanese-jisx0208
		    korean-ksc5601 japanese-jisx0212
		    chinese-cns11643-1 chinese-cns11643-2
		    chinese-cns11643-3 chinese-cns11643-4
		    chinese-cns11643-5 chinese-cns11643-6
		    chinese-cns11643-7))
  "Alist of MIME-charset/MULE-charsets.")


(eval-and-compile
  (mapcar
   (lambda (elem)
     (let ((nfunc (intern (format "mm-%s" (car elem)))))
       (if (fboundp (car elem))
	   (fset nfunc (car elem))
	 (fset nfunc (cdr elem)))))
   '((decode-coding-string . (lambda (s a) s))
     (encode-coding-string . (lambda (s a) s))
     (encode-coding-region . ignore)
     (decode-coding-region . ignore)
     (coding-system-list . ignore)
     (char-int . identity)
     (device-type . ignore)
     (coding-system-equal . equal)
     (annotationp . ignore)
     (make-char
      . (lambda (charset int)
	  (int-to-char int)))
     (read-coding-system
      . (lambda (prompt)
	  "Prompt the user for a coding system."
	  (completing-read
	   prompt (mapcar (lambda (s) (list (symbol-name (car s))))
			  mm-mime-mule-charset-alist)))))))

(defvar mm-charset-coding-system-alist
  (let ((rest
	 '((us-ascii . iso-8859-1)
	   (gb2312 . cn-gb-2312)
	   (iso-2022-jp-2 . iso-2022-7bit-ss2)
	   (x-ctext . ctext)))
	(systems (mm-coding-system-list))
	dest)
    (while rest
      (let ((pair (car rest)))
	(unless (memq (car pair) systems)
	  (setq dest (cons pair dest))))
      (setq rest (cdr rest)))
    dest)
  "Charset/coding system alist.")


(defun mm-mule-charset-to-mime-charset (charset)
  "Return the MIME charset corresponding to MULE CHARSET."
  (let ((alist mm-mime-mule-charset-alist)
	out)
    (while alist
      (when (memq charset (cdar alist))
	(setq out (caar alist)
	      alist nil))
      (pop alist))
    out))

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
  (cond
   ;; Running in a non-MULE environment.
   ((and (null (mm-coding-system-list))
	 (memq charset mm-known-charsets))
    charset)
   ;; Check to see whether we can handle this charset.
   ((memq charset (mm-coding-system-list))
    charset)
   ;; Nope.
   (t
    nil)))

(defun mm-replace-chars-in-string (string from to)
  "Replace characters in STRING from FROM to TO."
  (let ((string (substring string 0))	;Copy string.
	(len (length string))
	(idx 0))
    ;; Replace all occurrences of FROM with TO.
    (while (< idx len)
      (when (= (aref string idx) from)
	(aset string idx to))
      (setq idx (1+ idx)))
    string))

(defsubst mm-enable-multibyte ()
  "Enable multibyte in the current buffer."
  (when (fboundp 'set-buffer-multibyte)
    (set-buffer-multibyte t)))

(defun mm-insert-rfc822-headers (charset encoding)
  "Insert text/plain headers with CHARSET and ENCODING."
  (insert "MIME-Version: 1.0\n")
  (insert "Content-Type: text/plain; charset=\""
	  (downcase (symbol-name charset)) "\"\n")
  (insert "Content-Transfer-Encoding: "
	  (downcase (symbol-name encoding)) "\n"))

(defun mm-mime-charset (charset b e)
  (if (fboundp 'coding-system-get)
      (or
       (coding-system-get
	(get-charset-property charset 'prefered-coding-system)
	'mime-charset)
       (car (memq charset (find-coding-systems-region
			   (point-min) (point-max)))))
    (mm-mule-charset-to-mime-charset charset)))

(defsubst mm-multibyte-p ()
  "Say whether multibyte is enabled."
  (and (boundp 'enable-multibyte-characters)
       enable-multibyte-characters))

(defmacro mm-with-unibyte-buffer (&rest forms)
  "Create a temporary buffer, and evaluate FORMS there like `progn'.
See also `with-temp-file' and `with-output-to-string'."
  (let ((temp-buffer (make-symbol "temp-buffer"))
	(multibyte (make-symbol "multibyte")))
    `(if (not (boundp 'enable-multibyte-characters))
	 (with-temp-buffer ,@forms)
       (let ((,multibyte (default-value enable-multibyte-characters))
	     ,temp-buffer)
	 (setq-default enable-multibyte-characters nil)
	 (setq ,temp-buffer
	       (get-buffer-create (generate-new-buffer-name " *temp*")))
	 (unwind-protect
	     (with-current-buffer ,temp-buffer
	       ,@forms)
	   (and (buffer-name ,temp-buffer)
		(kill-buffer ,temp-buffer))
	   (setq-default enable-multibyte-characters ,multibyte))))))
(put 'mm-with-unibyte-buffer 'lisp-indent-function 0)
(put 'mm-with-unibyte-buffer 'edebug-form-spec '(body))


(provide 'mm-util)

;;; mm-util.el ends here
