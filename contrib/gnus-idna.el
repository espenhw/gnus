;;; gnus-idna.el --- Internationalized domain names support for Gnus.

;; Copyright (C) 2003 Free Software Foundation, Inc.

;; Author: Simon Josefsson
;; Keywords: news, mail

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

;; This package implement crude support for internationalized
;; (non-ASCII) domain names in Gnus.  It is meant as a proof of
;; concept.

;; Theory of Operation:

;; RFC 2822 RHS's inside the From:, To:, and CC: headers are encoded
;; using IDNA ToASCII() when you send mail using Message.  The hook
;; used is message-send-hook.
;;
;; For incoming articles, when QP in headers are decoded, it searches
;; for "xn--" prefixes and decode them using IDNA ToUnicode().  The
;; hook used is gnus-article-decode-hook.

;; Usage:

;; Simply put (require 'gnus-idna) in your ~/.gnus or ~/.emacs and it
;; should work.  You need to install GNU Libidn (0.1.11 or later) and
;; make sure the idna.el installed by it is found by emacs.

;;; Code:

(require 'gnus)
(require 'rfc822)
(require 'idna)

(eval-and-compile
  (cond
   ((fboundp 'replace-in-string)
    (defalias 'gnus-replace-in-string 'replace-in-string))
   ((fboundp 'replace-regexp-in-string)
    (defun gnus-replace-in-string  (string regexp newtext &optional literal)
      (replace-regexp-in-string regexp newtext string nil literal)))
   (t
    (defun gnus-replace-in-string (string regexp newtext &optional literal)
      (let ((start 0) tail)
	(while (string-match regexp string start)
	  (setq tail (- (length string) (match-end 0)))
	  (setq string (replace-match newtext nil literal string))
	  (setq start (- (length string) tail))))
      string))))

(defun gnus-idna-to-ascii-rhs-1 (header)
  (save-excursion
    (save-restriction
      (let (address header-data new-header-data rhs ace)
	(message-narrow-to-head)
	(setq header-data (message-fetch-field header))
	(when header-data
	  (dolist (element (message-tokenize-header header-data))
	    (setq address (car (rfc822-addresses element)))
	    (when (string-match "\\(.*\\)@\\([^@]+\\)" address)
	      (setq ace (if (setq rhs (match-string 2 address))
			    (idna-to-ascii rhs)))
	      (push (if (string= rhs ace)
			element
		      (gnus-replace-in-string
		       element (regexp-quote rhs) ace t))
		    new-header-data)))
	  (message-remove-header header)
	  (message-position-on-field header)
	  (dolist (addr (reverse new-header-data))
	    (insert addr ", "))
	  (when new-header-data
	    (delete-backward-char 2)))))))

(defun gnus-idna-to-ascii-rhs ()
  (gnus-idna-to-ascii-rhs-1 "From")
  (gnus-idna-to-ascii-rhs-1 "To")
  (gnus-idna-to-ascii-rhs-1 "Cc"))

(add-hook 'message-send-hook 'gnus-idna-to-ascii-rhs)

(defun gnus-idna-to-unicode-rhs ()
  (let ((inhibit-point-motion-hooks t)
	buffer-read-only)
    (goto-char (point-min))
    (while (re-search-forward "xn--.*[ \t\n\r.,<>()@!]" nil t)
      ;(or (eobp) (forward-char))
      (let (ace unicode)
	(when (setq ace (match-string 0))
	  (setq unicode (idna-to-unicode ace))
	  (unless (string= ace unicode)
	    (replace-match unicode)))))))

(add-hook 'gnus-article-decode-hook 'gnus-idna-to-unicode-rhs 'append)

(provide 'gnus-idna)

;; gnus-idna.el ends here
