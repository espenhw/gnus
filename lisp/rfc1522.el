;;; rfc1522.el --- Functions for encoding and decoding rfc1522 messages
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

(require 'base64)
(require 'qp)
(require 'mm-util)

(defvar rfc1522-header-encoding-alist
  '(("Newsgroups" . nil)
    ("Message-ID" . nil)
    (t . mime))
  "*Header/encoding method alist.
The list is traversed sequentially.  The keys can either be
header regexps or `t'.

The values can be:

1) nil, in which case no encoding is done;
2) `mime', in which case the header will be encoded according to RFC1522;
3) a charset, in which case it will be encoded as that charse;
4) `default', in which case the field will be encoded as the rest
   of the article.")

(defvar rfc1522-charset-encoding-alist
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
  "Alist of MIME charsets to RFC1522 encodings.
Valid encodings are nil, `Q' and `B'.")

(defvar rfc1522-encoding-function-alist
  '((Q . rfc1522-q-encode-region)
    (B . base64-encode-region)
    (nil . ignore))
  "Alist of RFC1522 encodings to encoding functions.")

(defvar rfc1522-q-encoding-alist
  '(("\\(From\\|Cc\\|To\\|Bcc\||Reply-To\\):" . "[^-A-Za-z0-9!*+/=_]")
    ("." . "[\000-\007\013\015-\037\200-\377=_?]"))
  "Alist of header regexps and valid Q characters.")

;;;
;;; Functions for encoding RFC1522 messages
;;;

(defun rfc1522-narrow-to-field ()
  "Narrow the buffer to the header on the current line."
  (beginning-of-line)
  (narrow-to-region
   (point)
   (progn
     (forward-line 1)
     (if (re-search-forward "^[^ \n\t]" nil t)
	 (progn
	   (beginning-of-line)
	   (point))
       (point-max))))
  (goto-char (point-min)))

;;;###autoload
(defun rfc1522-encode-message-header ()
  "Encode the message header according to `rfc1522-header-encoding-alist'.
Should be called narrowed to the head of the message."
  (interactive "*")
  (when (featurep 'mule)
    (save-excursion
      (let ((alist rfc1522-header-encoding-alist)
	    elem method)
	(while (not (eobp))
	  (save-restriction
	    (rfc1522-narrow-to-field)
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
		  (rfc1522-encode-region (point-min) (point-max)))
		 ;; Hm.
		 (t))))
	    (goto-char (point-max))))))))

(defun rfc1522-encode-region (b e)
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
		;(rfc1522-encode start (setq start (point)) prev)
		(setq prev c))))
	    (forward-char 1)))
	(when (and (not prev) qstart)
	  (rfc1522-encode qstart qend qprev)
	  (setq qstart nil)))
      (when qstart
	(rfc1522-encode qstart qend qprev)
	(setq qstart nil)))))

(defun rfc1522-encode-string (string)
  "Encode words in STRING."
  (with-temp-buffer
    (insert string)
    (rfc1522-encode-region (point-min) (point-max))
    (buffer-string)))

(defun rfc1522-encode (b e charset)
  "Encode the word in the region with CHARSET."
  (let* ((mime-charset (mm-mule-charset-to-mime-charset charset))
	 (encoding (cdr (assq mime-charset
			      rfc1522-charset-encoding-alist)))
	 (start (concat
		 "=?" (downcase (symbol-name mime-charset)) "?"
		 (downcase (symbol-name encoding)) "?")))
    (save-restriction
      (narrow-to-region b e)
      (insert
       (prog1
	   (mm-encode-coding-string (buffer-string) mime-charset)
	 (delete-region (point-min) (point-max))))
      (funcall (cdr (assq encoding rfc1522-encoding-function-alist))
	       (point-min) (point-max))
      (goto-char (point-min))
      (insert start)
      (goto-char (point-max))
      (insert "?=")
      ;; Encoded words can't be more than 75 chars long, so we have to
      ;; split the long ones up.
      (end-of-line)
      (while (> (current-column) 74)
	(beginning-of-line)
	(forward-char 73)
	(insert "?=\n " start)
	(end-of-line)))))

(defun rfc1522-q-encode-region (b e)
  "Encode the header contained in REGION with the Q encoding."
  (save-excursion
    (save-restriction
      (narrow-to-region (goto-char b) e)
      (let ((alist rfc1522-q-encoding-alist))
	(while alist
	  (when (looking-at (caar alist))
	    (quoted-printable-encode-region b e nil (cdar alist))
	    (subst-char-in-region (point-min) (point-max) ?  ?_))
	  (pop alist))))))

;;;
;;; Functions for decoding RFC1522 messages
;;;

(defvar rfc1522-encoded-word-regexp
  "=\\?\\([^][\000-\040()<>@,\;:\\\"/?.=]+\\)\\?\\(B\\|Q\\)\\?\\([!->@-~]+\\)\\?=")

;;;###autoload
(defun rfc1522-decode-region (start end)
  "Decode MIME-encoded words in region between START and END."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      ;; Remove whitespace between encoded words.
      (while (re-search-forward
	      (concat "\\(" rfc1522-encoded-word-regexp "\\)"
		      "\\(\n?[ \t]\\)+"
		      "\\(" rfc1522-encoded-word-regexp "\\)")
	      nil t)
	(delete-region (goto-char (match-end 1)) (match-beginning 6)))
      ;; Decode the encoded words.
      (goto-char (point-min))
      (while (re-search-forward rfc1522-encoded-word-regexp nil t)
	(insert (rfc1522-parse-and-decode
		 (prog1
		     (match-string 0)
		   (delete-region (match-beginning 0) (match-end 0)))))))))

;;;###autoload
(defun rfc1522-decode-string (string)
 "Decode the quoted-printable-encoded STRING and return the results."
 (with-temp-buffer
   (insert string)
   (inline
     (rfc1522-decode-region (point-min) (point-max)))
   (buffer-string)))

(defun rfc1522-parse-and-decode (word)
  "Decode WORD and return it if it is an encoded word.
Return WORD if not."
  (if (not (string-match rfc1522-encoded-word-regexp word))
      word
    (or
     (condition-case nil
	 (rfc1522-decode
	  (match-string 1 word)
	  (upcase (match-string 2 word))
	  (match-string 3 word))
       (error word))
     word)))

(defun rfc1522-decode (charset encoding string)
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
	  (mm-replace-chars-in-string string ?_ ? )))
	(t (error "Invalid encoding: %s" encoding)))
       cs))))

(provide 'rfc1522)

;;; rfc1522.el ends here
