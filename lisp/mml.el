;;; mml.el --- A package for parsing and validating MML documents
;; Copyright (C) 1998,99 Free Software Foundation, Inc.

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
(require 'mm-decode)

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
  (let (struct tag point contents charsets warn)
    (while (and (not (eobp))
		(not (looking-at "<#/multipart")))
      (cond
       ((looking-at "<#multipart")
	(push (nconc (mml-read-tag) (mml-parse-1)) struct))
       ((looking-at "<#external")
	(push (nconc (mml-read-tag) (list (cons 'contents (mml-read-part))))
	      struct))
       (t
	(if (looking-at "<#part")
	    (setq tag (mml-read-tag))
	  (setq tag (list 'part '(type . "text/plain"))
		warn t))
	(setq point (point)
	      contents (mml-read-part)
	      charsets (mm-find-mime-charset-region point (point)))
	(if (< (length charsets) 2)
	    (push (nconc tag (list (cons 'contents contents)))
		  struct)
	  (let ((nstruct (mml-parse-singlepart-with-multiple-charsets
			  tag point (point))))
	    (when (and warn
		       (not
			(y-or-n-p
			 (format
			  "Warning: Your message contains %d parts.  Really send? "
			  (length nstruct)))))
	      (error "Edit your message to use only one charset"))
	    (setq struct (nconc nstruct struct)))))))
    (unless (eobp)
      (forward-line 1))
    (nreverse struct)))

(defun mml-parse-singlepart-with-multiple-charsets (orig-tag beg end)
  (save-excursion
    (narrow-to-region beg end)
    (goto-char (point-min))
    (let ((current (mm-mime-charset (char-charset (following-char))))
	  charset struct space newline paragraph)
      (while (not (eobp))
	(cond
	 ;; The charset remains the same.
	 ((or (eq (setq charset (mm-mime-charset
				 (char-charset (following-char)))) 'us-ascii)
	      (eq charset current)))
	 ;; The initial charset was ascii.
	 ((eq current 'us-ascii)
	  (setq current charset
		space nil
		newline nil
		paragraph nil))
	 ;; We have a change in charsets.
	 (t
	  (push (append
		 orig-tag
		 (list (cons 'contents
			     (buffer-substring-no-properties
			      beg (or paragraph newline space (point))))))
		struct)
	  (setq beg (or paragraph newline space (point))
		current charset
		space nil
		newline nil
		paragraph nil)))
	;; Compute places where it might be nice to break the part.
	(cond
	 ((memq (following-char) '(?  ?\t))
	  (setq space (1+ (point))))
	 ((eq (following-char) ?\n)
	  (setq newline (1+ (point))))
	 ((and (eq (following-char) ?\n)
	       (not (bobp))
	       (eq (char-after (1- (point))) ?\n))
	  (setq paragraph (point))))
	(forward-char 1))
      ;; Do the final part.
      (unless (= beg (point))
	(push (append orig-tag
		      (list (cons 'contents
				  (buffer-substring-no-properties
				   beg (point)))))
	      struct))
      struct)))

(defun mml-read-tag ()
  "Read a tag and return the contents."
  (let (contents name elem val)
    (forward-char 2)
    (setq name (buffer-substring-no-properties
		(point) (progn (forward-sexp 1) (point))))
    (skip-chars-forward " \t\n")
    (while (not (looking-at ">"))
      (setq elem (buffer-substring-no-properties
		  (point) (progn (forward-sexp 1) (point))))
      (skip-chars-forward "= \t\n")
      (setq val (buffer-substring-no-properties
		 (point) (progn (forward-sexp 1) (point))))
      (when (string-match "^\"\\(.*\\)\"$" val)
	(setq val (match-string 1 val)))
      (push (cons (intern elem) val) contents)
      (skip-chars-forward " \t\n"))
    (forward-char 1)
    (skip-chars-forward " \t\n")
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
	    (buffer-substring-no-properties beg (match-beginning 0))
	  (if (or (not (match-beginning 1))
		  (equal (match-string 2) "multipart"))
	      (goto-char (match-beginning 0))
	    (when (looking-at "[ \t]*\n")
	      (forward-line 1))))
      (buffer-substring-no-properties beg (goto-char (point-max))))))

(defvar mml-boundary nil)
(defvar mml-base-boundary "-=-=")
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
      (if (member (car (split-string type "/")) '("text" "message"))
	  (with-temp-buffer
	    (cond
	     ((cdr (assq 'buffer cont))
	      (insert-buffer-substring (cdr (assq 'buffer cont))))
	     ((and (setq filename (cdr (assq 'filename cont)))
		   (not (equal (cdr (assq 'nofile cont)) "yes")))
	      (mm-insert-file-contents filename))
	     (t
	      (save-restriction
		(narrow-to-region (point) (point))
		(insert (cdr (assq 'contents cont)))
		;; Remove quotes from quoted tags.
		(goto-char (point-min))
		(while (re-search-forward
			"<#!+/?\\(part\\|multipart\\|external\\)" nil t)
		  (delete-region (+ (match-beginning 0) 2)
				 (+ (match-beginning 0) 3))))))
	    (setq charset (mm-encode-body))
	    (setq encoding (mm-body-encoding charset))
	    (setq coded (buffer-string)))
	(mm-with-unibyte-buffer
	  (cond
	   ((cdr (assq 'buffer cont))
	    (insert-buffer-substring (cdr (assq 'buffer cont))))
	   ((and (setq filename (cdr (assq 'filename cont)))
		 (not (equal (cdr (assq 'nofile cont)) "yes")))
	    (mm-insert-file-contents filename))
	   (t
	    (insert (cdr (assq 'contents cont)))))
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
	    (mml-insert-parameter
	     (mail-header-encode-parameter "name" name)
	     "access-type=local-file")
	  (mml-insert-parameter
	   (mail-header-encode-parameter
	    "name" (file-name-nondirectory (nth 2 name)))
	   (mail-header-encode-parameter "site" (nth 1 name))
	   (mail-header-encode-parameter
	    "directory" (file-name-directory (nth 2 name))))
	  (mml-insert-parameter
	   (concat "access-type="
		   (if (member (nth 0 name) '("ftp@" "anonymous@"))
		       "anon-ftp"
		     "ftp")))))      
      (when parameters
	(mml-insert-parameter-string
	 cont '(expiration size permission))))
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
	(cond
	 ((cdr (assq 'buffer cont))
	  (insert-buffer-substring (cdr (assq 'buffer cont))))
	 ((and (setq filename (cdr (assq 'filename cont)))
	       (not (equal (cdr (assq 'nofile cont)) "yes")))
	  (mm-insert-file-contents filename))
	 (t
	  (insert (cdr (assq 'contents cont)))))
	(goto-char (point-min))
	(when (re-search-forward (concat "^--" (regexp-quote mml-boundary))
				 nil t)
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
    (setq parameters
	  (mml-parameter-string
	   cont '(name access-type expiration size permission)))
    (when (or charset
	      parameters
	      (not (equal type "text/plain")))
      (when (consp charset)
	(error
	 "Can't encode a part with several charsets."))
      (insert "Content-Type: " type)
      (when charset
	(insert "; " (mail-header-encode-parameter
		      "charset" (symbol-name charset))))
      (when parameters
	(mml-insert-parameter-string
	 cont '(name access-type expiration size permission)))
      (insert "\n"))
    (setq parameters
	  (mml-parameter-string
	   cont '(filename creation-date modification-date read-date)))
    (when (or (setq disposition (cdr (assq 'disposition cont)))
	      parameters)
      (insert "Content-Disposition: " (or disposition "inline"))
      (when parameters
	(mml-insert-parameter-string
	 cont '(filename creation-date modification-date read-date)))
      (insert "\n"))
    (unless (eq encoding '7bit)
      (insert (format "Content-Transfer-Encoding: %s\n" encoding)))
    (when (setq description (cdr (assq 'description cont)))
      (insert "Content-Description: "
	      (mail-encode-encoded-word-string description) "\n"))))

(defun mml-parameter-string (cont types)
  (let ((string "")
	value type)
    (while (setq type (pop types))
      (when (setq value (cdr (assq type cont)))
	;; Strip directory component from the filename parameter.
	(when (eq type 'filename)
	  (setq value (file-name-nondirectory value)))
	(setq string (concat string "; "
			     (mail-header-encode-parameter
			      (symbol-name type) value)))))
    (when (not (zerop (length string)))
      string)))

(defun mml-insert-parameter-string (cont types)
  (let (value type)
    (while (setq type (pop types))
      (when (setq value (cdr (assq type cont)))
	;; Strip directory component from the filename parameter.
	(when (eq type 'filename)
	  (setq value (file-name-nondirectory value)))
	(mml-insert-parameter
	 (mail-header-encode-parameter
	  (symbol-name type) value))))))

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

(defun mml-insert-buffer (buffer)
  "Insert BUFFER at point and quote any MML markup."
  (save-restriction
    (narrow-to-region (point) (point))
    (insert-buffer-substring buffer)
    (mml-quote-region (point-min) (point-max))
    (goto-char (point-max))))

;;;
;;; Transforming MIME to MML
;;;

(defun mime-to-mml ()
  "Translate the current buffer (which should be a message) into MML."
  ;; First decode the head.
  (save-restriction
    (message-narrow-to-head)
    (mail-decode-encoded-word-region (point-min) (point-max)))
  (let ((handles (mm-dissect-buffer t)))
    (goto-char (point-min))
    (search-forward "\n\n" nil t)
    (delete-region (point) (point-max))
    (if (stringp (car handles))
	(mml-insert-mime handles)
      (mml-insert-mime handles t))
    (mm-destroy-parts handles)))

(defun mml-to-mime ()
  "Translate the current buffer from MML to MIME."
  (message-encode-message-body)
  (save-restriction
    (message-narrow-to-headers)
    (mail-encode-encoded-word-buffer)))

(defun mml-insert-mime (handle &optional no-markup)
  (let (textp buffer)
    ;; Determine type and stuff.
    (unless (stringp (car handle))
      (unless (setq textp (equal (mm-handle-media-supertype handle)
				 "text"))
	(save-excursion
	  (set-buffer (setq buffer (generate-new-buffer " *mml*")))
	  (mm-insert-part handle))))
    (unless no-markup
      (mml-insert-mml-markup handle buffer textp))
    (cond
     ((stringp (car handle))
      (mapcar 'mml-insert-mime (cdr handle))
      (insert "<#/multipart>\n"))
     (textp
      (mm-insert-part handle)
      (goto-char (point-max)))
     (t
      (insert "<#/part>\n")))))

(defun mml-insert-mml-markup (handle &optional buffer nofile)
  "Take a MIME handle and insert an MML tag."
  (if (stringp (car handle))
      (insert "<#multipart type=" (mm-handle-media-subtype handle)
	      ">\n")
    (insert "<#part type=" (mm-handle-media-type handle))
    (dolist (elem (append (cdr (mm-handle-type handle))
			  (cdr (mm-handle-disposition handle))))
      (insert " " (symbol-name (car elem)) "=\"" (cdr elem) "\""))
    (when (mm-handle-disposition handle)
      (insert " disposition=" (car (mm-handle-disposition handle))))
    (when buffer
      (insert " buffer=\"" (buffer-name buffer) "\""))
    (when nofile
      (insert " nofile=yes"))
    (when (mm-handle-description handle)
      (insert " description=\"" (mm-handle-description handle) "\""))
    (insert ">\n")))

(defun mml-insert-parameter (&rest parameters)
  "Insert PARAMETERS in a nice way."
  (dolist (param parameters)
    (insert ";")
    (let ((point (point)))
      (insert " " param)
      (when (> (current-column) 71)
	(goto-char point)
	(insert "\n ")
	(end-of-line)))))

;;;
;;; Mode for inserting and editing MML forms
;;;

(defvar mml-mode-map
  (let ((map (make-sparse-keymap))
	(main (make-sparse-keymap)))
    (define-key map "f" 'mml-attach-file)
    (define-key map "b" 'mml-attach-buffer)
    (define-key map "e" 'mml-attach-external)
    (define-key map "q" 'mml-quote-region)
    (define-key map "m" 'mml-insert-multipart)
    (define-key map "p" 'mml-insert-part)
    (define-key map "v" 'mml-validate)
    (define-key map "P" 'mml-preview)
    (define-key main "\M-m" map)
    main))

(easy-menu-define
 mml-menu mml-mode-map ""
 '("MML"
   ("Attach"
    ["File" mml-attach-file t]
    ["Buffer" mml-attach-buffer t]
    ["External" mml-attach-external t])
   ("Insert"
    ["Multipart" mml-insert-multipart t]
    ["Part" mml-insert-part t])
   ["Quote" mml-quote-region t]
   ["Validate" mml-validate t]
   ["Preview" mml-preview t]))

(defvar mml-mode nil
  "Minor mode for editing MML.")

(defun mml-mode (&optional arg)
  "Minor mode for editing MML.

\\{mml-mode-map}"
  (interactive "P")
  (if (not (set (make-local-variable 'mml-mode)
		(if (null arg) (not mml-mode)
		  (> (prefix-numeric-value arg) 0))))
      nil
    (set (make-local-variable 'mml-mode) t)
    (unless (assq 'mml-mode minor-mode-alist)
      (push `(mml-mode " MML") minor-mode-alist))
    (unless (assq 'mml-mode minor-mode-map-alist)
      (push (cons 'mml-mode mml-mode-map)
	    minor-mode-map-alist)))
  (run-hooks 'mml-mode-hook))

;;;
;;; Helper functions for reading MIME stuff from the minibuffer and
;;; inserting stuff to the buffer.
;;;

(defun mml-minibuffer-read-file (prompt)
  (let ((file (read-file-name prompt nil nil t)))
    ;; Prevent some common errors.  This is inspired by similar code in
    ;; VM.
    (when (file-directory-p file)
      (error "%s is a directory, cannot attach" file))
    (unless (file-exists-p file)
      (error "No such file: %s" file))
    (unless (file-readable-p file)
      (error "Permission denied: %s" file))
    file))

(defun mml-minibuffer-read-type (name &optional default)
  (let* ((default (or default
		      (mm-default-file-encoding name)
		      ;; Perhaps here we should check what the file
		      ;; looks like, and offer text/plain if it looks
		      ;; like text/plain.
		      "application/octet-stream"))
	 (string (completing-read
		  (format "Content type (default %s): " default)
		  (mapcar
		   'list
		   (delete-duplicates
		    (nconc
		     (mapcar (lambda (m) (cdr m))
			     mailcap-mime-extensions)
		     (apply
		      'nconc
		      (mapcar
		       (lambda (l)
			 (delq nil
			       (mapcar
				(lambda (m)
				  (let ((type (cdr (assq 'type (cdr m)))))
				    (if (equal (cadr (split-string type "/"))
					       "*")
					nil
				      type)))
				(cdr l))))
		       mailcap-mime-data)))
		    :test 'equal)))))
    (if (not (equal string ""))
	string
      default)))

(defun mml-minibuffer-read-description ()
  (let ((description (read-string "One line description: ")))
    (when (string-match "\\`[ \t]*\\'" description)
      (setq description nil))
    description))

(defun mml-quote-region (beg end)
  "Quote the MML tags in the region."
  (interactive "r")
  (save-excursion
    (save-restriction
      ;; Temporarily narrow the region to defend from changes
      ;; invalidating END.
      (narrow-to-region beg end)
      (goto-char (point-min))
      ;; Quote parts.
      (while (re-search-forward
	      "<#/?!*\\(multipart\\|part\\|external\\)" nil t)
	(goto-char (match-beginning 1))
	(insert "!")))))

(defun mml-insert-tag (name &rest plist)
  "Insert an MML tag described by NAME and PLIST."
  (when (symbolp name)
    (setq name (symbol-name name)))
  (insert "<#" name)
  (while plist
    (let ((key (pop plist))
	  (value (pop plist)))
      (when value
	;; Quote VALUE if it contains suspicious characters.
	(when (string-match "[\"\\~/* \t\n]" value)
	  (setq value (prin1-to-string value)))
	(insert (format " %s=%s" key value)))))
  (insert ">\n<#/" name ">\n"))

;;; Attachment functions.

(defun mml-attach-file (file &optional type description)
  "Attach a file to the outgoing MIME message.
The file is not inserted or encoded until you send the message with
`\\[message-send-and-exit]' or `\\[message-send]'.

FILE is the name of the file to attach.  TYPE is its content-type, a
string of the form \"type/subtype\".  DESCRIPTION is a one-line
description of the attachment."
  (interactive
   (let* ((file (mml-minibuffer-read-file "Attach file: "))
	  (type (mml-minibuffer-read-type file))
	  (description (mml-minibuffer-read-description)))
     (list file type description)))
  (mml-insert-tag 'part 'type type 'filename file 'disposition "attachment"
		  'description description))

(defun mml-attach-buffer (buffer &optional type description)
  "Attach a buffer to the outgoing MIME message.
See `mml-attach-file' for details of operation."
  (interactive
   (let* ((buffer (read-buffer "Attach buffer: "))
	  (type (mml-minibuffer-read-type buffer "text/plain"))
	  (description (mml-minibuffer-read-description)))
     (list buffer type description)))
  (mml-insert-tag 'part 'type type 'buffer buffer 'disposition "attachment"
		  'description description))

(defun mml-attach-external (file &optional type description)
  "Attach an external file into the buffer.
FILE is an ange-ftp/efs specification of the part location.
TYPE is the MIME type to use."
  (interactive
   (let* ((file (mml-minibuffer-read-file "Attach external file: "))
	  (type (mml-minibuffer-read-type file))
	  (description (mml-minibuffer-read-description)))
     (list file type description)))
  (mml-insert-tag 'external 'type type 'name file 'disposition "attachment"
		  'description description))

(defun mml-insert-multipart (&optional type)
  (interactive (list (completing-read "Multipart type (default mixed): "
		     '(("mixed") ("alternative") ("digest") ("parallel")
		       ("signed") ("encrypted"))
		     nil nil "mixed")))
  (or type
      (setq type "mixed"))
  (mml-insert-tag "multipart" 'type type)
  (forward-line -1))

(defun mml-preview (&optional raw)
 "Display current buffer with Gnus, in a new buffer.
If RAW, don't highlight the article."
 (interactive "P")
 (let ((buf (current-buffer)))
   (switch-to-buffer (get-buffer-create 
                     (concat (if raw "*Raw MIME preview of "
                               "*MIME preview of ") (buffer-name))))
   (erase-buffer)
   (insert-buffer buf)
   (mml-to-mime)
   (unless raw
     (run-hooks 'gnus-article-decode-hook)
     (let ((gnus-newsgroup-name "dummy"))
      (gnus-article-prepare-display)))
   (fundamental-mode)
   (setq buffer-read-only t)
   (goto-char (point-min))))

(defun mml-validate ()
  "Validate the current MML document."
  (interactive)
  (mml-parse))

(provide 'mml)

;;; mml.el ends here
