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

;; This package implement crude support for internationalized domain
;; names in Gnus.

;; Theory of Operation:

;; RFC 2822 RHS's inside the From:, To:, and CC: headers are encoded
;; using IDNA ToASCII() when you send mail using Message.  The hook
;; used is message-send-hook.
;;
;; For incoming articles, when QP in headers are decoded (i.e., when
;; gnus-article-decode-hook is invoked), it searches for "xn--"
;; prefixes and decode them if they are found inside (heuristically
;; determined) RHS in From:, To: and Cc:, using IDNA ToUnicode().

;; Usage:

;; You need to install GNU Libidn (0.1.11 or later) and make sure the
;; idna.el installed by it is found by emacs.

;; If you use an older Gnus, you may need to put the following in your
;; init scripts too, but keep in mind that most older Gnuses either
;; doesn't have these hooks or are buggy in other regards so it
;; doesn't work anyway.  (The window of Gnus versions that this works
;; on is a few weeks during the Oort CVS in winter 2003.)  Update to a
;; recent Gnus instead, then you don't have to do anything.

;; (add-hook 'message-send-hook 'message-idna-to-ascii-rhs)
;; (add-hook 'gnus-article-decode-hook 'gnus-idna-to-unicode-rhs 'append)

;; Revision history:

;; 2003-02-26 Initial release
;;
;; 2003-03-19 Cleanup. Fixes a bug that may corrupt outgoing mail if
;;            it contains From:, To: or Cc: headers in the body.

;;; Code:

(require 'gnus)
(require 'gnus-util)
(require 'rfc822)
(autoload 'idna-to-ascii "idna")
(autoload 'idna-to-unicode "idna")

(defcustom message-use-idna 'ask
  "Whether to encode non-ASCII in domain names into ASCII according to IDNA."
  :type '(choice (const :tag "Ask" ask)
		 (const :tag "Never" nil)
		 (const :tag "Always" t)))

(defun message-idna-inside-rhs-p ()
  "Return t iff point is inside a RHS (heuristically).
Only works properly if header contains mailbox-list or address-list.
I.e., calling it on a Subject: header is useless."
  (if (re-search-backward
       "[\\\n\r\t ]" (save-excursion (search-backward "@" nil t)) t)
      ;; whitespace between @ and point
      nil
    (let ((dquote 1) (paren 1))
      (while (save-excursion (re-search-backward "[^\\]\"" nil t dquote))
	(incf dquote))
      (while (save-excursion (re-search-backward "[^\\]\(" nil t paren))
	(incf paren))
      (and (= (% dquote 2) 1) (= (% paren 2) 1)))))

(defun message-idna-to-ascii-rhs-1 (header)
  "Interactively potentially IDNA encode domain names in HEADER."
  (let (rhs ace start end startpos endpos)
    (goto-char (point-min))
    (setq start (re-search-forward (concat "^" header) nil t)
	  end (or (save-excursion (re-search-forward "^[ \t]" nil t))
		  (point-max)))
    (when (and start end)
      (while (re-search-forward "@\\([^ \t\r\n>]+\\)" end t)
	(setq rhs (match-string-no-properties 1)
	      startpos (match-beginning 1)
	      endpos (match-end 1))
	(when (save-match-data
		(and (message-idna-inside-rhs-p)
		     (setq ace (idna-to-ascii rhs))
		     (not (string= rhs ace))
		     (if (eq message-use-idna 'ask)
			 (unwind-protect
			     (progn
			       (replace-highlight startpos endpos)
			       (y-or-n-p
				(format "Replace with `%s'? " ace)))
			   (message "")
			   (replace-dehighlight))
		       message-use-idna)))
	  (replace-match (concat "@" ace)))))))

;;;###autoload
(defun message-idna-to-ascii-rhs ()
  "Possibly IDNA encode non-ASCII domain names in From:, To: and Cc: headers.
See `message-idna-encode'."
  (interactive)
  (when (condition-case nil (require 'idna) (file-error))
    (save-excursion
      (save-restriction
	(message-narrow-to-head)
	(message-idna-to-ascii-rhs-1 "From")
	(message-idna-to-ascii-rhs-1 "To")
	(message-idna-to-ascii-rhs-1 "Cc")))))

;;;###autoload
(defun gnus-idna-to-unicode-rhs ()
  "Decode IDNA strings in RHS in From:, To: and Cc: headers in current buffer."
  (when (condition-case nil (require 'idna) (file-error))
    (let ((inhibit-point-motion-hooks t)
	  buffer-read-only)
      (article-narrow-to-head)
      (goto-char (point-min))
      (while (re-search-forward "\\(xn--.*\\)[ \t\n\r,>]" nil t)
	(let (ace unicode)
	  (when (save-match-data
		  (and (setq ace (match-string 1))
		       (save-excursion (and (re-search-backward "^[^ \t]" nil t)
					    (looking-at "From\\|To\\|Cc")))
		       (save-excursion (backward-char)
				       (message-idna-inside-rhs-p))
		       (setq unicode (idna-to-unicode ace))))
	    (unless (string= ace unicode)
	      (replace-match unicode nil nil nil 1))))))))

(provide 'gnus-idna)

;; gnus-idna.el ends here
