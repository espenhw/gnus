;;; nndoc.el --- single file access for Gnus
;; Copyright (C) 1995,96 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@ifi.uio.no>
;; 	Masanobu UMEDA <umerin@flab.flab.fujitsu.junet>
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

;;; Code:

(require 'nnheader)
(require 'rmail)
(require 'nnmail)

(defvar nndoc-article-type 'guess
  "*Type of the file.
One of `mbox', `babyl', `digest', `news', `rnews', `mmdf',
`forward', `mime-digest', `standard-digest', `slack-digest', or
`guess'.")

(defvar nndoc-type-alist 
  `((mmdf 
     (article-begin .  "^\^A\^A\^A\^A\n")
     (body-end .  "^\^A\^A\^A\^A\n"))
    (news
     (article-begin . "^Path:"))
    (rnews
     (article-begin . "^#! *rnews +\\([0-9]\\)+ *\n")
     (body-end-function . nndoc-rnews-body-end))
    (mbox 
     (article-begin . 
		    ,(let ((delim (concat "^" rmail-unix-mail-delimiter)))
		       (if (string-match "\n\\'" delim)
			   (substring delim 0 (match-beginning 0))
			 delim)))
     (body-end-function . nndoc-mbox-body-end))
    (babyl 
     (article-begin . "\^_\^L *\n")
     (body-end . "\^_")
     (head-begin . "^[0-9].*\n"))
    (forward
     (article-begin . "^-+ Start of forwarded message -+\n+")
     (body-end . "^-+ End of forwarded message -+\n"))
    (clari-briefs
     (article-begin . "^ \\*")
     (body-end . "^\t------*[ \t]^*\n^ \\*")
     (body-begin . "^\t")
     (head-end . "^\t")
     (generate-head . nndoc-generate-clari-briefs-head)
     (article-transform . nndoc-transform-clari-briefs))
    (slack-digest
     (article-begin . "^------------------------------*[\n \t]+")
     (head-end . "^ ?$")
     (body-begin . "^ ?$")
     (file-end . "^End of")
     (prepare-body . nndoc-prepare-digest-body))
    (mime-digest
     (article-begin . "")
     (head-end . "^ ?$")
     (body-end . "")
     (file-end . ""))
    (standard-digest
     (first-article . ,(concat "^" (make-string 70 ?-) "\n\n"))
     (article-begin . ,(concat "\n\n" (make-string 30 ?-) "\n\n"))
     (prepare-body . nndoc-prepare-digest-body)
     (body-end-function . nndoc-digest-body-end)
     (file-end . "^End of .* Digest"))
    (guess 
     (guess . nndoc-guess-type))
    (digest
     (guess . nndoc-guess-digest-type))
    ))



(defvar nndoc-file-begin nil)
(defvar nndoc-first-article nil)
(defvar nndoc-article-end nil)
(defvar nndoc-article-begin nil)
(defvar nndoc-head-begin nil)
(defvar nndoc-head-end nil)
(defvar nndoc-file-end nil)
(defvar nndoc-body-begin nil)
(defvar nndoc-body-end-function nil)
(defvar nndoc-body-end nil)
(defvar nndoc-dissection-alist nil)
(defvar nndoc-prepare-body nil)
(defvar nndoc-generate-head nil)
(defvar nndoc-article-transform nil)

(defvar nndoc-current-server nil)
(defvar nndoc-server-alist nil)
(defvar nndoc-server-variables
  (list
   (list 'nndoc-article-type nndoc-article-type)
   '(nndoc-article-begin nil)
   '(nndoc-article-end nil)
   '(nndoc-head-begin nil)
   '(nndoc-head-end nil)
   '(nndoc-first-article nil)
   '(nndoc-current-buffer nil)
   '(nndoc-group-alist nil)
   '(nndoc-end-of-file nil)
   '(nndoc-body-begin nil)
   '(nndoc-address nil)))

(defconst nndoc-version "nndoc 1.0"
  "nndoc version.")

(defvar nndoc-current-buffer nil
  "Current nndoc news buffer.")

(defvar nndoc-address nil)



(defvar nndoc-status-string "")

(defvar nndoc-group-alist nil)

;;; Interface functions

(defun nndoc-retrieve-headers (articles &optional newsgroup server fetch-old)
  (when (nndoc-possibly-change-buffer newsgroup server)
    (save-excursion
      (set-buffer nntp-server-buffer)
      (erase-buffer)
      (let (article entry)
	(if (stringp (car articles))
	    'headers
	  (while articles
	    (setq entry (cdr (assq (setq article (pop articles))
				   nndoc-dissection-alist)))
	    (insert (format "221 %d Article retrieved.\n" article))
	    (if nndoc-generate-head
		(funcall nndoc-generate-head article)
	      (insert-buffer-substring
	       nndoc-current-buffer (car entry) (nth 1 entry)))
	    (goto-char (point-max))
	    (or (= (char-after (1- (point))) ?\n) (insert "\n"))
	    (insert (format "Lines: %d\n" (nth 4 entry)))
	    (insert ".\n"))

	  (nnheader-fold-continuation-lines)
	  'headers)))))

(defun nndoc-open-server (server &optional defs)
  (nnheader-init-server-buffer)
  (if (equal server nndoc-current-server)
      t
    (if nndoc-current-server
	(setq nndoc-server-alist 
	      (cons (list nndoc-current-server
			  (nnheader-save-variables nndoc-server-variables))
		    nndoc-server-alist)))
    (let ((state (assoc server nndoc-server-alist)))
      (if state 
	  (progn
	    (nnheader-restore-variables (nth 1 state))
	    (setq nndoc-server-alist (delq state nndoc-server-alist)))
	(nnheader-set-init-variables nndoc-server-variables defs)))
    (setq nndoc-current-server server)
    t))

(defun nndoc-close-server (&optional server)
  t)

(defun nndoc-server-opened (&optional server)
  (and (equal server nndoc-current-server)
       nntp-server-buffer
       (buffer-name nntp-server-buffer)))

(defun nndoc-status-message (&optional server)
  nndoc-status-string)

(defun nndoc-request-article (article &optional newsgroup server buffer)
  (nndoc-possibly-change-buffer newsgroup server)
  (save-excursion
    (let ((buffer (or buffer nntp-server-buffer))
	  (entry (cdr (assq article nndoc-dissection-alist)))
	  beg)
      (set-buffer buffer)
      (erase-buffer)
      (if (stringp article)
	  nil
	(insert-buffer-substring 
	 nndoc-current-buffer (car entry) (nth 1 entry))
	(insert "\n")
	(setq beg (point))
	(insert-buffer-substring 
	 nndoc-current-buffer (nth 2 entry) (nth 3 entry))
	(goto-char beg)
	(when nndoc-prepare-body
	  (funcall nndoc-prepare-body))
	(when nndoc-article-transform
	  (funcall nndoc-article-transform article))
	t))))

(defun nndoc-request-group (group &optional server dont-check)
  "Select news GROUP."
  (save-excursion
    (let (number)
      (cond 
       ((not (nndoc-possibly-change-buffer group server))
	(nnheader-report 'nndoc "No such file or buffer: %s"
			 nndoc-address))
       (dont-check
	(nnheader-report 'nndoc "Selected group %s" group)
	t)
       ((zerop (setq number (length nndoc-dissection-alist)))
	(nndoc-close-group group)
	(nnheader-report 'nndoc "No articles in group %s" group))
       (t
	(save-excursion
	  (set-buffer nntp-server-buffer)
	  (erase-buffer)
	  (insert (format "211 %d %d %d %s\n" number 1 number group))
	  t))))))

(defun nndoc-close-group (group &optional server)
  (nndoc-possibly-change-buffer group server)
  (and nndoc-current-buffer
       (buffer-name nndoc-current-buffer)
       (kill-buffer nndoc-current-buffer))
  (setq nndoc-group-alist (delq (assoc group nndoc-group-alist)
				nndoc-group-alist))
  (setq nndoc-current-buffer nil)
  (setq nndoc-current-server nil)
  (setq nndoc-dissection-alist nil)
  t)

(defun nndoc-request-list (&optional server)
  nil)

(defun nndoc-request-newgroups (date &optional server)
  nil)

(defun nndoc-request-list-newsgroups (&optional server)
  nil)

(defalias 'nndoc-request-post 'nnmail-request-post)


;;; Internal functions.

(defun nndoc-possibly-change-buffer (group source)
  (let (buf)
    (cond 
     ;; The current buffer is this group's buffer.
     ((and nndoc-current-buffer
	   (buffer-name nndoc-current-buffer)
	   (eq nndoc-current-buffer 
	       (setq buf (cdr (assoc group nndoc-group-alist))))))
     ;; We change buffers by taking an old from the group alist.
     ;; `source' is either a string (a file name) or a buffer object. 
     (buf
      (setq nndoc-current-buffer buf))
     ;; It's a totally new group.    
     ((or (and (bufferp nndoc-address)
	       (buffer-name nndoc-address))
	  (and (stringp nndoc-address)
	       (file-exists-p nndoc-address)
	       (not (file-directory-p nndoc-address))))
      (setq nndoc-group-alist 
	    (cons (cons group (setq nndoc-current-buffer 
				    (get-buffer-create 
				     (concat " *nndoc " group "*"))))
		  nndoc-group-alist))
      (setq nndoc-dissection-alist nil)
      (save-excursion
	(set-buffer nndoc-current-buffer)
	(buffer-disable-undo (current-buffer))
	(erase-buffer)
	(if (stringp nndoc-address)
	    (insert-file-contents nndoc-address)
	  (insert-buffer-substring nndoc-address)))))
    (when (and nndoc-current-buffer
	       (not nndoc-dissection-alist))
      (save-excursion
	(set-buffer nndoc-current-buffer)
	(nndoc-set-delims)
	(nndoc-dissect-buffer)))
    t))

;; MIME (RFC 1341) digest hack by Ulrik Dickow <dickow@nbi.dk>.
(defun nndoc-guess-digest-type ()
  (let ((case-fold-search t)		; We match a bit too much, keep it simple.
	boundary-id b-delimiter entry)
    (goto-char (point-min))
    (cond 
     ;; MIME digest.
     ((and
       (re-search-forward
	(concat "^Content-Type: *multipart/digest;[ \t\n]*[ \t]"
		"boundary=\"\\([^\"\n]*[^\" \t\n]\\)\"")
	nil t)
       (match-beginning 1))
      (setq boundary-id (match-string 1)
	    b-delimiter (concat "\n--" boundary-id "[\n \t]+"))
      (setq entry (assq 'mime-digest nndoc-type-alist))
      (setcdr entry
	      (list
	       (cons 'head-end "^ ?$")
	       (cons 'body-begin "^ \n")
	       (cons 'article-begin b-delimiter)
	       (cons 'body-end-function 'nndoc-digest-body-end)
;	       (cons 'body-end 
;		     (concat "\n--" boundary-id "\\(--\\)?[\n \t]+"))
	       (cons 'file-end (concat "\n--" boundary-id "--[ \t]*$"))))
      'mime-digest)
     ((and (re-search-forward (concat "^" (make-string 70 ?-) "\n\n") nil t)
	   (re-search-forward 
	    (concat "\n\n" (make-string 30 ?-) "\n\n") nil t))
      'standard-digest)
     ;; Stupid digest.
     (t
      'slack-digest))))

(defun nndoc-guess-type ()
  "Guess what document type is in the current buffer."
  (goto-char (point-min))
  (cond 
   ((looking-at rmail-unix-mail-delimiter)
    'mbox)
   ((looking-at "\^A\^A\^A\^A$")
    'mmdf)
   ((looking-at "^Path:.*\n")
    'news)
   ((looking-at "#! *rnews")
    'rnews)
   ((re-search-forward "\^_\^L *\n" nil t)
    'babyl)
   ((save-excursion
      (and (re-search-forward "^-+ Start of forwarded message -+\n+" nil t)
	   (not (re-search-forward "^Subject:.*digest" nil t))))
    'forward)
   ((let ((case-fold-search nil))
      (re-search-forward "^\t[^a-z]+ ([^a-z]+) --" nil t))
    'clari-briefs)
   (t 
    'digest)))

(defun nndoc-set-delims ()
  (let ((vars '(nndoc-file-begin 
		nndoc-first-article 
		nndoc-article-end nndoc-head-begin nndoc-head-end
		nndoc-file-end nndoc-article-begin
		nndoc-body-begin nndoc-body-end-function nndoc-body-end
		nndoc-prepare-body nndoc-article-transform
		nndoc-generate-head)))
    (while vars
      (set (pop vars) nil)))
  (let* (defs guess)
    ;; Guess away until we find the real file type.
    (while (setq defs (cdr (assq nndoc-article-type nndoc-type-alist))
		 guess (assq 'guess defs))
      (setq nndoc-article-type (funcall (cdr guess))))
    (while defs
      (set (intern (format "nndoc-%s" (car (car defs))))
	   (cdr (pop defs))))))

(defun nndoc-search (regexp)
  (prog1
      (re-search-forward regexp nil t)
    (beginning-of-line)))

(defun nndoc-dissect-buffer ()
  (let ((i 0)
	(first t)
	head-begin head-end body-begin body-end)
    (setq nndoc-dissection-alist nil)
    (save-excursion
      (set-buffer nndoc-current-buffer)
      (goto-char (point-min))
      ;; Find the beginning of the file.
      (when nndoc-file-begin
	(nndoc-search nndoc-file-begin))
      ;; Go through the file.
      (while (if (and first nndoc-first-article)
		 (nndoc-search nndoc-first-article)
	       (nndoc-search nndoc-article-begin))
	(setq first nil)
	(when nndoc-head-begin
	  (nndoc-search nndoc-head-begin))
	(setq head-begin (point))
	(nndoc-search (or nndoc-head-end "^$"))
	(setq head-end (point))
	(nndoc-search (or nndoc-body-begin "^\n"))
	(setq body-begin (point))
	(or (and nndoc-body-end-function
		 (funcall nndoc-body-end-function))
	    (and nndoc-body-end
		 (nndoc-search nndoc-body-end))
	    (nndoc-search nndoc-article-begin)
	    (progn
	      (goto-char (point-max))
	      (when nndoc-file-end
		(and (re-search-backward nndoc-file-end nil t)
		     (beginning-of-line)))))
	(setq body-end (point))
	(push (list (incf i) head-begin head-end body-begin body-end
		    (count-lines body-begin body-end))
	      nndoc-dissection-alist)))))

(defun nndoc-prepare-digest-body ()
  "Unquote quoted non-separators in digests."
  (while (re-search-forward "^- -"nil t)
    (replace-match "-" t t)))

(defun nndoc-digest-body-end ()
  (and (re-search-forward nndoc-article-begin nil t)
       (goto-char (match-beginning 0))))

(defun nndoc-mbox-body-end ()
  (let ((beg (point))
	len end)
    (when
	(save-excursion
	  (and (re-search-backward nndoc-article-begin nil t)
	       (setq end (point))
	       (search-forward "\n\n" beg t)
	       (re-search-backward "^Content-Length: \\([0-9]+\\) *$" end t)
	       (setq len (string-to-int (match-string 1)))
	       (search-forward "\n\n" beg t)
	       (or (= (setq len (+ (point) len)) (point-max))
		   (and (< len (point-max))
			(goto-char len)
			(looking-at nndoc-article-begin)))))
      (goto-char len))))

(defun nndoc-rnews-body-end ()
  (save-excursion
    (and (re-search-backward nndoc-article-begin nil t)
	 (goto-char (+ (point) (string-to-int (match-string 1)))))))  

(defun nndoc-transform-clari-briefs (article)
  (goto-char (point-min))
  (when (looking-at " *\\*\\(.*\\)\n")
    (replace-match "" t t))
  (nndoc-generate-clari-briefs-head article))

(defun nndoc-generate-clari-briefs-head (article)
  (let ((entry (cdr (assq article nndoc-dissection-alist)))
	subject from)
    (save-excursion
      (set-buffer nndoc-current-buffer)
      (save-restriction
	(narrow-to-region (car entry) (nth 3 entry))
	(goto-char (point-min))
	(when (looking-at " *\\*\\(.*\\)$")
	  (setq subject (match-string 1)))
	(when
	    (let ((case-fold-search nil))
	      (re-search-forward
	       "^\t\\([^a-z]+\\(,[^(]+\\)? ([^a-z]+)\\) --" nil t))
	  (setq from (match-string 1)))))
    (insert "From: " "clari@clari.net (" (or from "unknown") ")"
	    "\nSubject: " (or subject "(no subject)") "\n")))

(provide 'nndoc)

;;; nndoc.el ends here
