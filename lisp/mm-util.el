;;; mm-util.el --- Utility functions for MIME things
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

(defvar mm-running-xemacs (string-match "XEmacs" emacs-version))

(defvar mm-running-ntemacs 
  (and (not mm-running-xemacs) 
       (string-match "nt" system-configuration)))

(defvar mm-binary-coding-system 
  (if mm-running-xemacs
      'binary 'no-conversion)
  "100% binary coding system.")   

(defvar mm-text-coding-system 
  (cond 
   ((not (fboundp 'coding-system-p)) nil)
   (mm-running-xemacs  ;; XEmacs
    (and (coding-system-p 'no-conversion) 'no-conversion))
   (mm-running-ntemacs ;; NTEmacs
    (and (coding-system-p 'raw-text-dos) 'raw-text-dos))
   ((coding-system-p 'raw-text) 'raw-text) ;; Emacs
   (t nil))
  "100% text coding system, for removing ^M.")

(defvar mm-default-coding-system nil
  "The default coding system to use.")  

(defvar mm-known-charsets '(iso-8859-1)
  "List of known charsets.
Use this under non-Mule Emacsen to specify which charsets your Emacs
can display.  Also see `mm-default-charset'.")

(defvar mm-default-charset 'iso-8859-1
  "Default charset assumed to be used when viewing non-ASCII characters.
This variable is used only in non-Mule Emacsen.")

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
    (iso-2022-jp-2 japanese-jisx0208)
    (iso-2022-jp latin-jisx0201
		 japanese-jisx0208-1978)
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
     (coding-system-list . ignore)
     (decode-coding-region . ignore)
     (char-int . identity)
     (device-type . ignore)
     (coding-system-equal . equal)
     (annotationp . ignore)
     (set-buffer-file-coding-system . ignore)
     (make-char
      . (lambda (charset int)
	  (int-to-char int)))
     (read-coding-system
      . (lambda (prompt)
	  "Prompt the user for a coding system."
	  (completing-read
	   prompt (mapcar (lambda (s) (list (symbol-name (car s))))
			  mm-mime-mule-charset-alist)))))))

(defvar mm-coding-system-list nil)
(defun mm-get-coding-system-list ()
  "Get the coding system list."
  (or mm-coding-system-list
      (setq mm-coding-system-list (mm-coding-system-list))))

(defvar mm-charset-coding-system-alist
  (let ((rest
	 '((gb2312 . cn-gb-2312)
	   (iso-2022-jp-2 . iso-2022-7bit-ss2)
	   (x-ctext . ctext)))
	(systems (mm-get-coding-system-list))
	dest)
    (while rest
      (let ((pair (car rest)))
	(unless (memq (car pair) systems)
	  (setq dest (cons pair dest))))
      (setq rest (cdr rest)))
    dest)
  "Charset/coding system alist.")

;;;Internal variable
(defvar mm-charset-iso-8859-1-forced nil)

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
  (if (and mm-charset-iso-8859-1-forced 
	   (eq charset 'iso-8859-1))
      (setq charset mm-charset-iso-8859-1-forced))
  (setq charset
	(or (cdr (assq charset mm-charset-coding-system-alist))
	    charset))
  (when lbt
    (setq charset (intern (format "%s-%s" charset lbt))))
  (cond
   ;; Running in a non-MULE environment.
   ((and (null (mm-get-coding-system-list))
	 (or (eq charset mm-default-charset)
	     (memq charset mm-known-charsets)))
    charset)
   ;; ascii
   ((eq charset 'us-ascii)
    'ascii)
   ;; Check to see whether we can handle this charset.
   ((memq charset (mm-get-coding-system-list))
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
  (when (and (fboundp 'set-buffer-multibyte)
	     (default-value 'enable-multibyte-characters))
    (set-buffer-multibyte t)))

(defsubst mm-disable-multibyte ()
  "Disable multibyte in the current buffer."
  (when (fboundp 'set-buffer-multibyte)
    (set-buffer-multibyte nil)))

(defun mm-mime-charset (charset b e)
  (if (fboundp 'coding-system-get)
      (or
       (and
	mm-default-coding-system
	(let ((safe (coding-system-get mm-default-coding-system
				       'safe-charsets)))
	  (or (eq safe t) (memq charset safe)))
	(coding-system-get mm-default-coding-system 'mime-charset))
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
       (let ((,multibyte (default-value 'enable-multibyte-characters))
	     ,temp-buffer)
	 (unwind-protect
	     (progn
	       (setq-default enable-multibyte-characters nil)
	       (setq ,temp-buffer
		     (get-buffer-create (generate-new-buffer-name " *temp*")))
	       (unwind-protect
		   (with-current-buffer ,temp-buffer
		     (let ((buffer-file-coding-system mm-binary-coding-system)
			   (coding-system-for-read mm-binary-coding-system)
			   (coding-system-for-write mm-binary-coding-system))
		       ,@forms))
		 (and (buffer-name ,temp-buffer)
		      (kill-buffer ,temp-buffer))))
	   (setq-default enable-multibyte-characters ,multibyte))))))
(put 'mm-with-unibyte-buffer 'lisp-indent-function 0)
(put 'mm-with-unibyte-buffer 'edebug-form-spec '(body))

(defun mm-find-charset-region (b e)
  "Return a list of charsets in the region."
  (cond
   ((and (boundp 'enable-multibyte-characters)
	 enable-multibyte-characters
	 (fboundp 'find-charset-region))
    (find-charset-region b e))
   ((not (boundp 'current-language-environment))
    (save-excursion
      (save-restriction
	(narrow-to-region b e)
	(goto-char (point-min))
	(skip-chars-forward "\0-\177")
	(if (eobp)
	    '(ascii)
	  ;;;!!!bogus
	  (list 'ascii 'latin-iso8859-1)))))
   (t
    ;; We are in a unibyte buffer, so we futz around a bit.
    (save-excursion
      (save-restriction
	(narrow-to-region b e)
	(goto-char (point-min))
	(let ((entry (assoc (capitalize current-language-environment)
			    language-info-alist)))
	  (skip-chars-forward "\0-\177")
	  (if (eobp)
	      '(ascii)
	    (list 'ascii (car (last (assq 'charset entry)))))))))))

(provide 'mm-util)

;;; mm-util.el ends here
