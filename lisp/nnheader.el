;;; nnheader.el --- header access macros for Gnus and its backends
;; Copyright (C) 1987,88,89,90,93,94,95,96 Free Software Foundation, Inc.

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
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; These macros may look very much like the ones in GNUS 4.1.  They
;; are, in a way, but you should note that the indices they use have
;; been changed from the internal GNUS format to the NOV format.  The
;; makes it possible to read headers from XOVER much faster.
;;
;; The format of a header is now:
;; [number subject from date id references chars lines xref]
;;
;; (That last entry is defined as "misc" in the NOV format, but Gnus
;; uses it for xrefs.)

;;; Code:

(require 'mail-utils)
(require 'sendmail)
(require 'rmail)
(eval-when-compile (require 'cl))

(defvar nnheader-max-head-length 4096
  "*Max length of the head of articles.")

(defvar nnheader-file-name-translation-alist nil
  "*Alist that says how to translate characters in file names.
For instance, if \":\" is illegal as a file character in file names
on your system, you could say something like:

\(setq nnheader-file-name-translation-alist '((?: . ?_)))")

;;; Header access macros.

(defmacro mail-header-number (header)
  "Return article number in HEADER."
  `(aref ,header 0))

(defmacro mail-header-set-number (header number)
  "Set article number of HEADER to NUMBER."
  `(aset ,header 0 ,number))

(defmacro mail-header-subject (header)
  "Return subject string in HEADER."
  `(aref ,header 1))

(defmacro mail-header-set-subject (header subject)
  "Set article subject of HEADER to SUBJECT."
  `(aset ,header 1 ,subject))

(defmacro mail-header-from (header)
  "Return author string in HEADER."
  `(aref ,header 2))

(defmacro mail-header-set-from (header from)
  "Set article author of HEADER to FROM."
  `(aset ,header 2 ,from))

(defmacro mail-header-date (header)
  "Return date in HEADER."
  `(aref ,header 3))

(defmacro mail-header-set-date (header date)
  "Set article date of HEADER to DATE."
  `(aset ,header 3 ,date))

(defalias 'mail-header-message-id 'mail-header-id)
(defmacro mail-header-id (header)
  "Return Id in HEADER."
  `(aref ,header 4))

(defalias 'mail-header-set-message-id 'mail-header-set-id)
(defmacro mail-header-set-id (header id)
  "Set article Id of HEADER to ID."
  `(aset ,header 4 ,id))

(defmacro mail-header-references (header)
  "Return references in HEADER."
  `(aref ,header 5))

(defmacro mail-header-set-references (header ref)
  "Set article references of HEADER to REF."
  `(aset ,header 5 ,ref))

(defmacro mail-header-chars (header)
  "Return number of chars of article in HEADER."
  `(aref ,header 6))

(defmacro mail-header-set-chars (header chars)
  "Set number of chars in article of HEADER to CHARS."
  `(aset ,header 6 ,chars))

(defmacro mail-header-lines (header)
  "Return lines in HEADER."
  `(aref ,header 7))

(defmacro mail-header-set-lines (header lines)
  "Set article lines of HEADER to LINES."
  `(aset ,header 7 ,lines))

(defmacro mail-header-xref (header)
  "Return xref string in HEADER."
  `(aref ,header 8))

(defmacro mail-header-set-xref (header xref)
  "Set article xref of HEADER to xref."
  `(aset ,header 8 ,xref))

(defun make-mail-header (&optional init)
  "Create a new mail header structure initialized with INIT."
  (make-vector 9 init))

;; Various cruft the backends and Gnus need to communicate.

(defvar nntp-server-buffer nil)
(defvar gnus-verbose-backends 7
  "*A number that says how talkative the Gnus backends should be.")
(defvar gnus-nov-is-evil nil
  "If non-nil, Gnus backends will never output headers in the NOV format.")
(defvar news-reply-yank-from nil)
(defvar news-reply-yank-message-id nil)

(defvar nnheader-callback-function nil)

(defun nnheader-init-server-buffer ()
  "Initialize the Gnus-backend communication buffer."
  (save-excursion
    (setq nntp-server-buffer (get-buffer-create " *nntpd*"))
    (set-buffer nntp-server-buffer)
    (buffer-disable-undo (current-buffer))
    (erase-buffer)
    (kill-all-local-variables)
    (setq case-fold-search t)		;Should ignore case.
    t))

;;; Virtual server functions.

(defun nnheader-set-init-variables (server defs)
  (let ((s server)
	val)
    ;; First we set the server variables in the sequence required.  We
    ;; use the definitions from the `defs' list where that is
    ;; possible. 
    (while s
      (set (caar s) 
	   (if (setq val (assq (caar s) defs))
	       (nth 1 val)
	     (nth 1 (car s))))
      (setq s (cdr s)))
    ;; The we go through the defs list and set any variables that were
    ;; not set in the first sweep.
    (while defs
      (if (not (assq (caar defs) server))
	  (set (caar defs) 
	       (if (and (symbolp (nth 1 (car defs)))
			(not (boundp (nth 1 (car defs)))))
		   (nth 1 (car defs))
		 (eval (nth 1 (car defs))))))
      (setq defs (cdr defs)))))

(defun nnheader-save-variables (server)
  (let (out)
    (while server
      (push (list (caar server) (symbol-value (caar server))) out)
      (setq server (cdr server)))
    (nreverse out)))

(defun nnheader-restore-variables (state)
  (while state
    (set (caar state) (nth 1 (car state)))
    (setq state (cdr state))))

(defun nnheader-change-server (backend server defs)
  (nnheader-init-server-buffer)
  (let ((current-server (intern (format "%s-current-server" backend)))
	(alist (intern (format "%s-server-alist" backend)))
	(variables (intern (format "%s-server-variables" backend))))

    (when (and (symbol-value current-server)
	       (not (equal server (symbol-value current-server))))
      (set alist
	   (cons (list (symbol-value current-server)
		       (nnheader-save-variables (symbol-value variables)))
		 (symbol-value alist))))
    (let ((state (assoc server (symbol-value alist))))
      (if (not state)
	  (nnheader-set-init-variables (symbol-value variables) defs)
	(nnheader-restore-variables (nth 1 state))
	(set alist (delq state (symbol-value alist)))))
    (set current-server server)
    t))

;;; Various functions the backends use.

(defun nnheader-insert-head (file)
  "Insert the head of the article."
  (if (eq nnheader-max-head-length t)
      ;; Just read the entire file.
      (insert-file-contents-literally file)
    ;; Read 1K blocks until we find a separator.
    (let ((beg 0)
	  format-alist 
	  (chop 1024))
      (while (and (eq chop (nth 1 (insert-file-contents
				   file nil beg (incf beg chop))))
		  (prog1 (not (search-forward "\n\n" nil t)) 
		    (goto-char (point-max)))
		  (or (null nnheader-max-head-length)
		      (< beg nnheader-max-head-length)))))))

(defun nnheader-article-p ()
  "Say whether the current buffer looks like an article."
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

(defun nnheader-insert-references (references message-id)
  "Insert a References header based on REFERENCES and MESSAGE-ID."
  (if (and (not references) (not message-id)) 
      ()	; This is illegal, but not all articles have Message-IDs.
    (mail-position-on-field "References")
    (let ((begin (save-excursion (beginning-of-line) (point)))
	  (fill-column 78)
	  (fill-prefix "\t"))
      (if references (insert references))
      (if (and references message-id) (insert " "))
      (if message-id (insert message-id))
      ;; Fold long References lines to conform to RFC1036 (sort of).
      ;; The region must end with a newline to fill the region
      ;; without inserting extra newline.
      (fill-region-as-paragraph begin (1+ (point))))))

(defun nnheader-replace-header (header new-value)
  "Remove HEADER and insert the NEW-VALUE."
  (save-excursion
    (save-restriction
      (nnheader-narrow-to-headers)
      (prog1
	  (message-remove-header header)
	(goto-char (point-max))
	(insert header ": " new-value "\n")))))

(defun nnheader-narrow-to-headers ()
  "Narrow to the head of an article."
  (widen)
  (narrow-to-region
   (goto-char (point-min))
   (if (search-forward "\n\n" nil t)
       (1- (point))
     (point-max)))
  (goto-char (point-min)))

(defun nnheader-set-temp-buffer (name)
  "Set-buffer to an empty (possibly new) buffer called NAME with undo disabled."
  (set-buffer (get-buffer-create name))
  (buffer-disable-undo (current-buffer))
  (erase-buffer)
  (current-buffer))

(defmacro nnheader-temp-write (file &rest forms)
  "Create a new buffer, evaluate FORM there, and write the buffer to FILE."
  `(save-excursion
     (let ((nnheader-temp-file ,file)
	   (nnheader-temp-cur-buffer
	    (nnheader-set-temp-buffer
	     (generate-new-buffer-name " *nnheader temp*"))))
       (when (and nnheader-temp-file 
		  (not (file-directory-p (file-name-directory 
					  nnheader-temp-file))))
	 (make-directory (file-name-directory nnheader-temp-file) t))
       (unwind-protect
	   (prog1
	       (progn
		 ,@forms)
	     (when nnheader-temp-file
	       (set-buffer nnheader-temp-cur-buffer)
	       (write-region (point-min) (point-max) 
			     nnheader-temp-file nil 'nomesg)))
	 (when (buffer-name nnheader-temp-cur-buffer)
	   (kill-buffer nnheader-temp-cur-buffer))))))

(put 'nnheader-temp-write 'lisp-indent-function 1)
(put 'nnheader-temp-write 'lisp-indent-hook 1)
(put 'nnheader-temp-write 'edebug-form-spec '(file &rest form))

(defvar jka-compr-compression-info-list)
(defvar nnheader-numerical-files
  (if (boundp 'jka-compr-compression-info-list)
      (concat "\\([0-9]+\\)\\(" 
	      (mapconcat (lambda (i) (aref i 0))
			 jka-compr-compression-info-list "\\|")
	      "\\)?")
    "[0-9]+$")
  "Regexp that match numerical files.")

(defvar nnheader-numerical-short-files (concat "^" nnheader-numerical-files)
  "Regexp that matches numerical file names.")

(defvar nnheader-numerical-full-files (concat "/" nnheader-numerical-files)
  "Regexp that matches numerical full file paths.")

(defsubst nnheader-file-to-number (file)
  "Take a file name and return the article number."
  (if (not (boundp 'jka-compr-compression-info-list))
      (string-to-int file)
    (string-match nnheader-numerical-short-files file)
    (string-to-int (match-string 0 file))))

(defun nnheader-directory-articles (dir)
  "Return a list of all article files in a directory."
  (mapcar 'nnheader-file-to-number
	  (directory-files dir nil nnheader-numerical-short-files t)))

(defun nnheader-article-to-file-alist (dir)
  "Return an alist of article/file pairs in DIR."
  (mapcar (lambda (file) (cons (nnheader-file-to-number file) file))
	  (directory-files dir nil nnheader-numerical-short-files t)))

(defun nnheader-fold-continuation-lines ()
  "Fold continuation lines in the current buffer."
  (goto-char (point-min))
  (while (re-search-forward "\\(\r?\n[ \t]+\\)+" nil t)
    (replace-match " " t t)))

(defun nnheader-translate-file-chars (file)
  (if (null nnheader-file-name-translation-alist)
      ;; No translation is necessary.
      file 
    ;; We translate -- but only the file name.  We leave the directory
    ;; alone.
    (let* ((new (file-name-nondirectory file))
	   (len (length new))
	   (i 0)
	  trans)
      (while (< i len)
	(when (setq trans (cdr (assq (aref new i)
				     nnheader-file-name-translation-alist)))
	  (aset new i trans))
	(incf i))
      (concat (file-name-directory file) new))))

(defun nnheader-report (backend &rest args)
  "Report an error from the BACKEND.
The first string in ARGS can be a format string."
  (set (intern (format "%s-status-string" backend))
       (if (< (length args) 2)
	   (car args)
	 (apply 'format args)))
  nil)

(defun nnheader-get-report (backend)
  (message "%s" (symbol-value (intern (format "%s-status-string" backend)))))

(defun nnheader-insert (format &rest args)
  "Clear the communicaton buffer and insert FORMAT and ARGS into the buffer.
If FORMAT isn't a format string, it and all ARGS will be inserted
without formatting."
  (save-excursion
    (set-buffer nntp-server-buffer)
    (erase-buffer)
    (if (string-match "%" format)
	(insert (apply 'format format args))
      (apply 'insert format args))
    t))

(defun nnheader-mail-file-mbox-p (file)
  "Say whether FILE looks like an Unix mbox file."
  (when (and (file-exists-p file)
	     (file-readable-p file)
	     (file-regular-p file))
    (save-excursion
      (nnheader-set-temp-buffer " *mail-file-mbox-p*")
      (insert-file-contents-literally file)
      (goto-char (point-min))
      (prog1
	  (looking-at rmail-unix-mail-delimiter)
	(kill-buffer (current-buffer))))))

(defun nnheader-replace-chars-in-string (string from to)
  "Replace characters in STRING from FROM to TO."
  (let ((string (substring string 0))	;Copy string.
	(len (length string))
	(idx 0))
    ;; Replace all occurrences of FROM with TO.
    (while (< idx len)
      (if (= (aref string idx) from)
	  (aset string idx to))
      (setq idx (1+ idx)))
    string))

(defun nnheader-file-to-group (file &optional top)
  "Return a group name based on FILE and TOP."
  (nnheader-replace-chars-in-string 
   (if (not top)
       file
     (condition-case ()
	 (substring (expand-file-name file)
		    (length 
		     (expand-file-name
		      (file-name-as-directory top))))
       (error "")))
   ?/ ?.))

(defun nnheader-message (level &rest args)
  "Message if the Gnus backends are talkative."
  (if (or (not (numberp gnus-verbose-backends))
	  (<= level gnus-verbose-backends))
      (apply 'message args)
    (apply 'format args)))

(defun nnheader-be-verbose (level)
  "Return whether the backends should be verbose on LEVEL."
  (or (not (numberp gnus-verbose-backends))
      (<= level gnus-verbose-backends)))

(defun nnheader-group-pathname (group dir &optional file)
  "Make pathname for GROUP."
  (concat
   (let ((dir (file-name-as-directory (expand-file-name dir))))
     ;; If this directory exists, we use it directly.
     (if (file-directory-p (concat dir group))
	 (concat dir group "/")
       ;; If not, we translate dots into slashes.
       (concat dir (nnheader-replace-chars-in-string group ?. ?/) "/")))
   (cond ((null file) "")
	 ((numberp file) (int-to-string file))
	 (t file))))

(defun nnheader-functionp (form)
  "Return non-nil if FORM is funcallable."
  (or (and (symbolp form) (fboundp form))
      (and (listp form) (eq (car form) 'lambda))))

(fset 'nnheader-find-file-noselect 'find-file-noselect)
  
(provide 'nnheader)

;;; nnheader.el ends here
