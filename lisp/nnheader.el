;;; nnheader.el --- header access macros for Gnus and its backends
;; Copyright (C) 1987,88,89,90,93,94,95 Free Software Foundation, Inc.

;; Author: Masanobu UMEDA <umerin@flab.flab.fujitsu.junet>
;; 	Lars Magne Ingebrigtsen <larsi@ifi.uio.no>
;; Keywords: news

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; These macros may look very much like the ones in GNUS 4.1. They
;; are, in a way, but you should note that the indices they use have
;; been changed from the internal GNUS format to the NOV format. Makes
;; it possible to read headers from XOVER much faster.
;;
;; The format of a header is now:
;; [number subject from date id references chars lines xref]
;;
;; (That last entry is defined as "misc" in the NOV format, but Gnus
;; uses it for xrefs.)

;;; Code:

(defalias 'nntp-header-number 'header-number)
(defmacro header-number (header)
  "Return article number in HEADER."
  (` (aref (, header) 0)))

(defalias 'nntp-set-header-number 'header-set-number)
(defmacro header-set-number (header number)
  "Set article number of HEADER to NUMBER."
  (` (aset (, header) 0 (, number))))

(defalias 'nntp-header-subject 'header-subject)
(defmacro header-subject (header)
  "Return subject string in HEADER."
  (` (aref (, header) 1)))

(defalias 'nntp-set-header-subject 'header-set-subject)
(defmacro header-set-subject (header subject)
  "Set article subject of HEADER to SUBJECT."
  (` (aset (, header) 1 (, subject))))

(defalias 'nntp-header-from 'header-from)
(defmacro header-from (header)
  "Return author string in HEADER."
  (` (aref (, header) 2)))

(defalias 'nntp-set-header-from 'header-set-from)
(defmacro header-set-from (header from)
  "Set article author of HEADER to FROM."
  (` (aset (, header) 2 (, from))))

(defalias 'nntp-header-xref 'header-xref)
(defmacro header-xref (header)
  "Return xref string in HEADER."
  (` (aref (, header) 8)))

(defalias 'nntp-set-header-xref 'header-set-xref)
(defmacro header-set-xref (header xref)
  "Set article xref of HEADER to xref."
  (` (aset (, header) 8 (, xref))))

(defalias 'nntp-header-lines 'header-lines)
(defmacro header-lines (header)
  "Return lines in HEADER."
  (` (aref (, header) 7)))

(defalias 'nntp-set-header-lines 'header-set-lines)
(defmacro header-set-lines (header lines)
  "Set article lines of HEADER to LINES."
  (` (aset (, header) 7 (, lines))))

(defalias 'nntp-header-date 'header-date)
(defmacro header-date (header)
  "Return date in HEADER."
  (` (aref (, header) 3)))

(defalias 'nntp-set-header-date 'header-set-date)
(defmacro header-set-date (header date)
  "Set article date of HEADER to DATE."
  (` (aset (, header) 3 (, date))))

(defalias 'nntp-header-id 'header-id)
(defmacro header-id (header)
  "Return Id in HEADER."
  (` (aref (, header) 4)))

(defalias 'nntp-set-header-id 'header-set-id)
(defmacro header-set-id (header id)
  "Set article Id of HEADER to ID."
  (` (aset (, header) 4 (, id))))

(defalias 'nntp-header-references 'header-references)
(defmacro header-references (header)
  "Return references in HEADER."
  (` (aref (, header) 5)))

(defalias 'nntp-set-header-references 'header-set-references)
(defmacro header-set-references (header ref)
  "Set article references of HEADER to REF."
  (` (aset (, header) 5 (, ref))))

(defalias 'nntp-header-chars 'header-chars)
(defmacro header-chars (header)
  "Return number of chars of article in HEADER."
  (` (aref (, header) 6)))

(defalias 'nntp-set-header-chars 'header-set-chars)
(defmacro header-set-chars (header chars)
  "Set number of chars in article of HEADER to CHARS."
  (` (aset (, header) 6 (, chars))))

;; Various cruft the backends and Gnus need to communicate.

(defvar nntp-server-buffer nil)
(defvar gnus-verbose-backends t
  "*If non-nil, Gnus backends will generate lots of comments.")
(defvar gnus-nov-is-evil nil
  "If non-nil, Gnus backends will never output headers in the NOV format.")
(defvar news-reply-yank-from nil)
(defvar news-reply-yank-message-id nil)

;; All backends use this function, so I moved it to this file.

(defun nnheader-init-server-buffer ()
  (save-excursion
    (setq nntp-server-buffer (get-buffer-create " *nntpd*"))
    (set-buffer nntp-server-buffer)
    (buffer-disable-undo (current-buffer))
    (erase-buffer)
    (kill-all-local-variables)
    (setq case-fold-search t)		;Should ignore case.
    t))

(defun nnheader-set-init-variables (server defs)
  (let ((s server)
	val)
    ;; First we set the server variables in the sequence required.  We
    ;; use the definitions from the `defs' list where that is
    ;; possible. 
    (while s
      (set (car (car s)) 
	   (if (setq val (assq (car (car s)) defs))
	       (nth 1 val)
	     (nth 1 (car s))))
      (setq s (cdr s)))
    ;; The we go through the defs list and set any variables that were
    ;; not set in the first sweep.
    (while defs
      (if (not (assq (car (car defs)) server))
	  (set (car (car defs)) 
	       (if (and (symbolp (nth 1 (car defs)))
			(not (boundp (nth 1 (car defs)))))
		   (nth 1 (car defs))
		 (eval (nth 1 (car defs))))))
      (setq defs (cdr defs)))))

(defun nnheader-save-variables (server)
  (let (out)
    (while server
      (setq out (cons (list (car (car server)) 
			    (symbol-value (car (car server))))
		      out))
      (setq server (cdr server)))
    (nreverse out)))

(defun nnheader-restore-variables (state)
  (while state
    (set (car (car state)) (nth 1 (car state)))
    (setq state (cdr state))))

;; Read the head of an article.
(defun nnheader-insert-head (file)
  (let ((beg 0)
	(chop 1024))
    (while (and (eq chop (nth 1 (insert-file-contents 
				 file nil beg (setq beg (+ chop beg)))))
		(prog1 (not (search-backward "\n\n" nil t)) 
		  (goto-char (point-max)))))))

(defun nnheader-article-p ()
  (goto-char (point-min))
  (if (not (search-forward "\n\n" nil t))
      nil
    (narrow-to-region (point-min) (1- (point)))
    (goto-char (point-min))
    (while (looking-at "[A-Z][^ \t]+:.*\n\\([ \t].*\n\\)*\\|From .*\n")
      (goto-char (match-end 0)))
    (prog1
	(eobp)
      (widen))))    

(defun nnheader-find-file-noselect (file)
  "Basically does the same as `find-file-noselect', but avoids that function."
  (save-excursion
    (or
     (get-file-buffer file)
     (prog1
	 (set-buffer (get-buffer-create (create-file-buffer file)))
       (and (file-exists-p file)
	    (not (file-directory-p file))
	    (insert-file-contents file))
       (set-visited-file-name file)))))

(provide 'nnheader)

;;; nnheader.el ends here
