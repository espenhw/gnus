;;; mml.el --- A package for parsing and validating MML documents
;; Copyright (C) 1998 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
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

(defvar mml-syntax-table
  (let ((table (copy-syntax-table emacs-lisp-mode-syntax-table)))
    (modify-syntax-entry ?\\ "/" table)
    (modify-syntax-entry ?< "(" table)
    (modify-syntax-entry ?> ")" table)
    (modify-syntax-entry ?@ "w" table)
    (modify-syntax-entry ?/ "w" table)
    (modify-syntax-entry ?= " " table)
    (modify-syntax-entry ?* " " table)
    (modify-syntax-entry ?\; " " table)
    (modify-syntax-entry ?\' " " table)
    table))

(defun mml-parse ()
  "Parse the current buffer as an MML document."
  (goto-char (point-min))
  (let ((table (syntax-table)))
    (unwind-protect
	(progn
	  (set-syntax-table mml-syntax-table)
	  (mml-parse-1))
      (set-syntax-table table))))
  
(defun mml-parse-1 ()
  "Parse the current buffer as an MML document."
  (let (struct)
    (while (and (not (eobp))
		(not (looking-at "</multipart")))
      (cond
       ((looking-at "<multipart")
	(push (nconc (mml-read-tag) (mml-parse-1)) struct))
       ((looking-at "<part")
	(push (nconc (mml-read-tag) (list (cons 'contents (mml-read-part))))
	      struct))
       (t
	(push (list 'part '(type . "text/plain")
		    (cons 'contents (mml-read-part))) struct))))
    (unless (eobp)
      (forward-line 1))
    (nreverse struct)))

(defun mml-read-tag ()
  "Read a tag and return the contents."
  (let (contents name elem val)
    (forward-char 1)
    (setq name (buffer-substring (point) (progn (forward-sexp 1) (point))))
    (skip-chars-forward " \t\n")
    (while (not (looking-at ">"))
      (setq elem (buffer-substring (point) (progn (forward-sexp 1) (point))))
      (skip-chars-forward "= \t\n")
      (setq val (buffer-substring (point) (progn (forward-sexp 1) (point))))
      (when (string-match "^\"\\(.*\\)\"$" val)
	(setq val (match-string 1 val)))
      (push (cons (intern elem) val) contents)
      (skip-chars-forward " \t\n"))
    (forward-char 1)
    (cons (intern name) (nreverse contents))))

(defun mml-read-part ()
  "Return the buffer up till the next part, multipart or closing part or multipart."
  (let ((beg (point)))
    (if (re-search-forward "</?\\(multi\\)?part." nil t)
	(prog1
	    (buffer-substring beg (match-beginning 0))
	  (unless (equal (match-string 0) "</part>")
	    (goto-char (match-beginning 0))))
      (buffer-substring beg (goto-char (point-max))))))

(defvar mml-boundary nil)

(defun mml-generate-mime ()
  "Generate a MIME message based on the current MML document."
  (setq mml-boundary "=-=-=")
  (let ((cont (mml-parse)))
    (with-temp-buffer
      (if (and (consp (car cont))
	       (= (length cont) 1))
	  (mml-generate-mime-1 (car cont))
	(mml-generate-mime-1 (nconc (list 'multipart '(type . "mixed"))
				    cont)))
      (buffer-string))))

(defun mml-generate-mime-1 (cont)
  (cond
   ((eq (car cont) 'part)
    (let (coded encoding charset filename type)
      (setq type (or (cdr (assq 'type cont)) "text/plain"))
      (with-temp-buffer
	(if (setq filename (cdr (assq 'filename cont)))
	    (insert-file-contents-literally filename)
	  (insert (cdr (assq 'contents cont))))
	(if (equal (car (split-string type "/")) "text")
	    (setq charset (mm-encode-body)
		  encoding (mm-body-encoding))
	  (setq encoding (mm-encode-buffer type)))
	(setq coded (buffer-string)))
      (when (or charset
		(not (equal type "text/plain")))
	(insert "Content-Type: " type))
      (when charset
	(insert (format "; charset=\"%s\"" charset)))
      (insert "\n")
      (unless (eq encoding '7bit)
	(insert (format "Content-Transfer-Encoding: %s\n" encoding)))
      (insert "\n")
      (insert coded)))
   ((eq (car cont) 'multipart)
    (let ((mml-boundary (concat "=" mml-boundary)))
      (insert (format "Content-Type: multipart/%s; boundary=\"%s\"\n"
		      (or (cdr (assq 'type cont)) "mixed")
		      mml-boundary))
      (insert "\n")
      (setq cont (cddr cont))
      (while cont
	(insert "--" mml-boundary "\n")
	(mml-generate-mime-1 (pop cont)))
      (insert "--" mml-boundary "--\n")))
   (t
    (error "%S" cont))))

(provide 'mml)

;;; mml.el ends here
