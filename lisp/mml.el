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

(require 'mm-util)
(require 'mm-bodies)
(require 'mm-encode)

(eval-and-compile
  (autoload 'message-make-message-id "message"))

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
		(not (looking-at "<#/multipart")))
      (cond
       ((looking-at "<#multipart")
	(push (nconc (mml-read-tag) (mml-parse-1)) struct))
       ((looking-at "<#part")
	(push (nconc (mml-read-tag) (list (cons 'contents (mml-read-part))))
	      struct))
       ((looking-at "<#external")
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
    (forward-char 2)
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
    ;; If the tag ended at the end of the line, we go to the next line.
    (when (looking-at "[ \t]*\n")
      (forward-line 1))
    (if (re-search-forward
	 "<#\\(/\\)?\\(multipart\\|part\\|external\\)." nil t)
	(prog1
	    (buffer-substring beg (match-beginning 0))
	  (if (or (not (match-beginning 1))
		  (equal (match-string 2) "multipart"))
	      (goto-char (match-beginning 0))
	    (when (looking-at "[ \t]*\n")
	      (forward-line 1))))
      (buffer-substring beg (goto-char (point-max))))))

(defvar mml-boundary nil)
(defvar mml-base-boundary "=-=-=")
(defvar mml-multipart-number 0)

(defun mml-generate-mime ()
  "Generate a MIME message based on the current MML document."
  (let ((cont (mml-parse))
	(mml-multipart-number 0))
    (if (not cont)
	nil
      (with-temp-buffer
	(if (and (consp (car cont))
		 (= (length cont) 1))
	    (mml-generate-mime-1 (car cont))
	  (mml-generate-mime-1 (nconc (list 'multipart '(type . "mixed"))
				      cont)))
	(buffer-string)))))

(defun mml-generate-mime-1 (cont)
  (cond
   ((eq (car cont) 'part)
    (let (coded encoding charset filename type)
      (setq type (or (cdr (assq 'type cont)) "text/plain"))
      (if (equal (car (split-string type "/")) "text")
	  (with-temp-buffer
	    (if (setq filename (cdr (assq 'filename cont)))
		(insert-file-contents-literally filename)
	      (save-restriction
		(narrow-to-region (point) (point))
		(insert (cdr (assq 'contents cont)))
		;; Remove quotes from quoted tags.
		(goto-char (point-min))
		(while (re-search-forward
			"<#!+/?\\(part\\|multipart\\|external\\)" nil t)
		  (delete-region (+ (match-beginning 0) 2)
				 (+ (match-beginning 0) 3)))))
	    (setq charset (mm-encode-body)
		  encoding (mm-body-encoding))
	    (setq coded (buffer-string)))
	(mm-with-unibyte-buffer
	  (if (setq filename (cdr (assq 'filename cont)))
	      (insert-file-contents-literally filename)
	    (insert (cdr (assq 'contents cont))))
	  (setq encoding (mm-encode-buffer type)
		coded (buffer-string))))
      (mml-insert-mime-headers cont type charset encoding)
      (insert "\n")
      (insert coded)))
   ((eq (car cont) 'external)
    (insert "Content-Type: message/external-body")
    (let ((parameters (mml-parameter-string
		       cont '(expiration size permission)))
	  (name (cdr (assq 'name cont))))
      (when name
	(setq name (mml-parse-file-name name))
	(if (stringp name)
	    (insert ";\n " (mail-header-encode-parameter "name" name)
		    "\";\n access-type=local-file")
	  (insert
	   (format ";\n "
		   (mail-header-encode-parameter
		    "name" (file-name-nondirectory (nth 2 name)))
		   (mail-header-encode-parameter "site" (nth 1 name))
		   (mail-header-encode-parameter
		    "directory" (file-name-directory (nth 2 name)))))
	  (insert ";\n access-type="
		  (if (member (nth 0 name) '("ftp@" "anonymous@"))
		      "anon-ftp"
		    "ftp"))))
      (when parameters
	(insert parameters)))
    (insert "\n\n")
    (insert "Content-Type: " (cdr (assq 'type cont)) "\n")
    (insert "Content-ID: " (message-make-message-id) "\n")
    (insert "Content-Transfer-Encoding: "
	    (or (cdr (assq 'encoding cont)) "binary"))
    (insert "\n\n")
    (insert (or (cdr (assq 'contents cont))))
    (insert "\n"))
   ((eq (car cont) 'multipart)
    (let ((mml-boundary (mml-compute-boundary cont)))
      (insert (format "Content-Type: multipart/%s; boundary=\"%s\"\n"
		      (or (cdr (assq 'type cont)) "mixed")
		      mml-boundary))
      (insert "\n")
      (setq cont (cddr cont))
      (while cont
	(insert "\n--" mml-boundary "\n")
	(mml-generate-mime-1 (pop cont)))
      (insert "\n--" mml-boundary "--\n")))
   (t
    (error "Invalid element: %S" cont))))

(defun mml-compute-boundary (cont)
  "Return a unique boundary that does not exist in CONT."
  (let ((mml-boundary (mml-make-boundary)))
    ;; This function tries again and again until it has found
    ;; a unique boundary.
    (while (not (catch 'not-unique
		  (mml-compute-boundary-1 cont))))
    mml-boundary))

(defun mml-compute-boundary-1 (cont)
  (let (filename)
    (cond
     ((eq (car cont) 'part)
      (with-temp-buffer
	(if (setq filename (cdr (assq 'filename cont)))
	    (insert-file-contents-literally filename)
	  (insert (cdr (assq 'contents cont))))
	(goto-char (point-min))
	(when (re-search-forward (concat "^--" mml-boundary) nil t)
	  (setq mml-boundary (mml-make-boundary))
	  (throw 'not-unique nil))))
     ((eq (car cont) 'multipart)
      (mapcar 'mml-compute-boundary-1 (cddr cont))))
    t))

(defun mml-make-boundary ()
  (concat (make-string (% (incf mml-multipart-number) 60) ?=)
	  (if (> mml-multipart-number 17)
	      (format "%x" mml-multipart-number)
	    "")
	  mml-base-boundary))

(defun mml-make-string (num string)
  (let ((out ""))
    (while (not (zerop (decf num)))
      (setq out (concat out string)))
    out))

(defun mml-insert-mime-headers (cont type charset encoding)
  (let (parameters disposition description)
    (when (or charset
	      (setq parameters
		    (mml-parameter-string
		     cont '(name access-type expiration size permission)))
	      (not (equal type "text/plain")))
      (when (listp charset)
	(error
	 "Can't encode a part with several charsets.  Insert a <#part>."))
      (insert "Content-Type: " type)
      (when charset
	(insert "; " (mail-header-encode-parameter
		      "charset" (symbol-name charset))))
      (when parameters
	(insert parameters))
      (insert "\n"))
    (when (or (setq disposition (cdr (assq 'disposition cont)))
	      (setq parameters
		    (mml-parameter-string
		     cont '(filename creation-date modification-date
				     read-date))))
      (insert "Content-Disposition: " (or disposition "inline"))
      (when parameters
	(insert parameters))
      (insert "\n"))
    (unless (eq encoding '7bit)
      (insert (format "Content-Transfer-Encoding: %s\n" encoding)))
    (when (setq description (cdr (assq 'description cont)))
      (insert "Content-Description: " description "\n"))))

(defun mml-parameter-string (cont types)
  (let ((string "")
	value type)
    (while (setq type (pop types))
      (when (setq value (cdr (assq type cont)))
	(setq string (concat string ";\n "
			     (mail-header-encode-parameter
			      (symbol-name type) value)))))
    (when (not (zerop (length string)))
      string)))

(defvar ange-ftp-path-format)
(defvar efs-path-regexp)
(defun mml-parse-file-name (path)
  (if (if (boundp 'efs-path-regexp)
	  (string-match efs-path-regexp path)
	(if (boundp 'ange-ftp-path-format)
	    (string-match (car ange-ftp-path-format))))
      (list (match-string 1 path) (match-string 2 path)
	    (substring path (1+ (match-end 2))))
    path))

(provide 'mml)

;;; mml.el ends here
