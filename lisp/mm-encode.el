;;; mm-encode.el --- Functions for encoding MIME things
;; Copyright (C) 1998 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;;	MORIOKA Tomohiko <morioka@jaist.ac.jp>
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

(defvar mm-header-encoding-alist
  '(("X-Nsubject" . iso-2022-jp-2)
    ("Newsgroups" . nil)
    ("Message-ID" . nil)
    (t . mime))
  "*Header/encoding method alist.
The list is traversed sequentially.  The keys can either be a
header regexp or `t'.

The values can be:

1) nil, in which case no encoding is done;
2) `mime', in which case the header will be encoded according to RFC1522;
3) a charset, in which case it will be encoded as that charse;
4) `default', in which case the field will be encoded as the rest
   of the article.")

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

(defvar mm-mime-charset-encoding-alist
  '((us-ascii . nil)
    (iso-8859-1 . Q)
    (iso-8859-2 . Q)
    (iso-8859-3 . Q)
    (iso-8859-4 . Q)
    (iso-8859-5 . Q)
    (koi8-r . Q)
    (iso-8859-7 . Q)
    (iso-8859-8 . Q)
    (iso-8859-9 . Q)
    (iso-2022-jp . B)
    (iso-2022-kr . B)
    (gb2312 . B)
    (cn-gb . B)
    (cn-gb-2312 . B)
    (euc-kr . B)
    (iso-2022-jp-2 . B)
    (iso-2022-int-1 . B))
  "Alist of MIME charsets to MIME encodings.
Valid encodings are nil, `Q' and `B'.")

(defvar mm-mime-encoding-function-alist
  '((Q . quoted-printable-encode-region)
    (B . base64-encode-region)
    (nil . ignore))
  "Alist of MIME encodings to encoding functions.")

(defun mm-encode-message-header ()
  "Encode the message header according to `mm-header-encoding-alist'."
  (when (featurep 'mule)
    (save-excursion
      (save-restriction
	(message-narrow-to-headers)
	(let ((alist mm-header-encoding-alist)
	      elem method)
	  (while (not (eobp))
	    (save-restriction
	      (message-narrow-to-field)
	      (when (find-non-ascii-charset-region (point-min) (point-max))
		;; We found something that may perhaps be encoded.
		(while (setq elem (pop alist))
		  (when (or (and (stringp (car elem))
				 (looking-at (car elem)))
			    (eq (car elem) t))
		    (setq alist nil
			  method (cdr elem))))
		(when method
		  (cond
		   ((eq method 'mime)
		    (mm-encode-words-region (point-min) (point-max)))
		   ;; Hm.
		   (t))))
	      (goto-char (point-max)))))))))

(defun mm-encode-words-region (b e)
  "Encode all encodable words in REGION."
  (let (prev c start qstart qprev qend)
    (save-excursion
      (goto-char b)
      (while (re-search-forward "[^ \t\n]+" nil t)
	(save-restriction
	  (narrow-to-region (match-beginning 0) (match-end 0))
	  (goto-char (setq start (point-min)))
	  (setq prev nil)
	  (while (not (eobp))
	    (unless (eq (setq c (char-charset (following-char))) 'ascii)
	      (cond
	       ((eq c prev)
		)
	       ((null prev)
		(setq qstart (or qstart start)
		      qend (point-max)
		      qprev c)
		(setq prev c))
	       (t
		;(mm-encode-word-region start (setq start (point)) prev)
		(setq prev c)
		)))
	    (forward-char 1)))
	(when (and (not prev) qstart)
	  (mm-encode-word-region qstart qend qprev)
	  (setq qstart nil)))
      (when qstart
	(mm-encode-word-region qstart qend qprev)
	(setq qstart nil)))))

(defun mm-encode-words-string (string)
  "Encode words in STRING."
  (with-temp-buffer
    (insert string)
    (mm-encode-words-region (point-min) (point-max))
    (buffer-string)))

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

(defun mm-encode-word-region (b e charset)
  "Encode the word in the region with CHARSET."
  (let* ((mime-charset (mm-mule-charset-to-mime-charset charset))
	 (encoding (cdr (assq mime-charset mm-mime-charset-encoding-alist))))
    (save-restriction
      (narrow-to-region b e)
      (funcall (cdr (assq encoding mm-mime-encoding-function-alist))
	       b e)
      (goto-char (point-min))
      (insert "=?" (upcase (symbol-name mime-charset)) "?"
	      (symbol-name encoding) "?")
      (goto-char (point-max))
      (insert "?="))))

(provide 'mm-encode)

;;; mm-encode.el ends here
