;;; nnml.el --- mail spool access for Gnus
;; Copyright (C) 1995 Free Software Foundation, Inc.

;; Author: Lars Ingebrigtsen <larsi@ifi.uio.no>
;; 	Masanobu UMEDA <umerin@flab.flab.fujitsu.junet>
;; Keywords: news, mail

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

;; Based on nnspool.el by Masanobu UMEDA <umerin@flab.flab.fujitsu.junet>.

;;; Code:

(require 'nnheader)
(require 'nnmail)

(defvar nnml-directory "~/Mail/"
  "Mail directory.")

(defvar nnml-active-file (concat nnml-directory "active")
  "Mail active file.")

(defvar nnml-newsgroups-file (concat nnml-directory "newsgroups")
  "Mail newsgroups description file.")

(defvar nnml-get-new-mail t
  "If non-nil, nnml will check the incoming mail file and split the mail.")

(defvar nnml-nov-is-evil nil
  "If non-nil, Gnus will never generate and use nov databases for mail groups.
Using nov databases will speed up header fetching considerably.
This variable shouldn't be flipped much. If you have, for some reason,
set this to t, and want to set it to nil again, you should always run
the `nnml-generate-nov-databases' command. The function will go
through all nnml directories and generate nov databases for them
all. This may very well take some time.")



(defconst nnml-version "nnml 0.2"
  "nnml version.")

(defvar nnml-current-directory nil
  "Current news group directory.")

(defvar nnml-status-string "")

(defvar nnml-nov-buffer-alist nil)

(defvar nnml-group-alist nil)



;;; Interface functions.

(defun nnml-retrieve-headers (sequence &optional newsgroup server)
  "Retrieve the headers for the articles in SEQUENCE.
Newsgroup must be selected before calling this function."
  (save-excursion
    (set-buffer nntp-server-buffer)
    (erase-buffer)
    (let ((file nil)
	  (number (length sequence))
	  (count 0)
	  beg article)
      (nnml-possibly-change-directory newsgroup)
      (if (nnml-retrieve-headers-with-nov sequence)
	  'nov
	(while sequence
	  (setq article (car sequence))
	  (setq file
		(concat nnml-current-directory (prin1-to-string article)))
	  (if (and (file-exists-p file)
		   (not (file-directory-p file)))
	      (progn
		(insert (format "221 %d Article retrieved.\n" article))
		(setq beg (point))
		(insert-file-contents file)
		(goto-char beg)
		(if (search-forward "\n\n" nil t)
		    (forward-char -1)
		  (goto-char (point-max))
		  (insert "\n\n"))
		(insert ".\n")
		(delete-region (point) (point-max))))
	  (setq sequence (cdr sequence))
	  (setq count (1+ count))
	  (and (numberp nnmail-large-newsgroup)
	       (> number nnmail-large-newsgroup)
	       (zerop (% count 20))
	       gnus-verbose-backends
	       (message "nnml: Receiving headers... %d%%"
			(/ (* count 100) number))))

	(and (numberp nnmail-large-newsgroup)
	     (> number nnmail-large-newsgroup)
	     gnus-verbose-backends
	     (message "nnml: Receiving headers... done"))

	;; Fold continuation lines.
	(goto-char 1)
	(while (re-search-forward "\\(\r?\n[ \t]+\\)+" nil t)
	  (replace-match " " t t))
	'headers))))

(defun nnml-open-server (host &optional service)
  "Open news server on HOST.
If HOST is nil, use value of environment variable `NNTPSERVER'.
If optional argument SERVICE is non-nil, open by the service name."
  (let ((host (or host (getenv "NNTPSERVER"))))
    (setq nnml-status-string "")
    (nnml-open-server-internal host service)))

(defun nnml-close-server (&optional server)
  "Close news server."
  (nnml-close-server-internal))

(fset 'nnml-request-quit (symbol-function 'nnml-close-server))

(defun nnml-server-opened (&optional server)
  "Return server process status, T or NIL.
If the stream is opened, return T, otherwise return NIL."
  (and nntp-server-buffer
       (get-buffer nntp-server-buffer)))

(defun nnml-status-message ()
  "Return server status response as string."
  nnml-status-string)

(defun nnml-request-article (id &optional newsgroup server buffer)
  "Select article by message ID (or number)."
  (nnml-possibly-change-directory newsgroup)
  (let ((file (if (stringp id)
		  nil
		(concat nnml-current-directory (prin1-to-string id))))
	(nntp-server-buffer (or buffer nntp-server-buffer)))
    (if (and (stringp file)
	     (file-exists-p file)
	     (not (file-directory-p file)))
	(save-excursion
	  (nnmail-find-file file)))))

(defun nnml-request-group (group &optional server dont-check)
  "Select news GROUP."
  (if (not dont-check)
      (nnml-get-new-mail))
  (let ((pathname (nnmail-article-pathname group nnml-directory))
	dir)
    (if (file-directory-p pathname)
	(progn
	  (setq nnml-current-directory pathname)
	  (if (not dont-check)
	      (progn
		(setq dir 
		      (sort
		       (mapcar
			(function
			 (lambda (name)
			   (string-to-int name)))
			(directory-files pathname nil "^[0-9]+$" t))
		       '<))
		(save-excursion
		  (set-buffer nntp-server-buffer)
		  (erase-buffer)
		  (if dir
		      (insert (format "211 %d %d %d %s\n" (length dir) 
				      (car dir)
				      (progn (while (cdr dir)
					       (setq dir (cdr dir)))
					     (car dir))
				      group))
		    (insert (format "211 0 1 0 %s\n" group))))))
	  t))))

(defun nnml-close-group (group &optional server)
  t)

(defun nnml-request-list (&optional server)
  "List active newsgoups."
  (if server (nnml-get-new-mail))
  (save-excursion
    (nnmail-find-file nnml-active-file)))

(defun nnml-request-newgroups (date &optional server)
  "List groups created after DATE."
  (nnml-request-list server))

(defun nnml-request-list-newsgroups (&optional server)
  "List newsgroups (defined in NNTP2)."
  (save-excursion
    (nnmail-find-file nnml-newsgroups-file)))

(defun nnml-request-post (&optional server)
  "Post a new news in current buffer."
  (mail-send-and-exit nil))

(fset 'nnml-request-post-buffer 'nnmail-request-post-buffer)

(defun nnml-request-expire-articles (articles newsgroup &optional server force)
  "Expire all articles in the ARTICLES list in group GROUP.
The list of unexpired articles will be returned (ie. all articles that
were too fresh to be expired).
If FORCE is non-nil, ARTICLES will be deleted whether they are old or not."
  (nnml-possibly-change-directory newsgroup)
  (let* ((days (or (and nnmail-expiry-wait-function
			(funcall nnmail-expiry-wait-function newsgroup))
		   nnmail-expiry-wait))
	 article rest mod-time)
    (while articles
      (setq article (concat nnml-current-directory (int-to-string
						      (car articles))))
      (if (setq mod-time (nth 5 (file-attributes article)))
	  (if (or force
		  (> (nnmail-days-between
		      (current-time-string)
		      (current-time-string mod-time))
		     days))
	      (progn
		(and gnus-verbose-backends (message "Deleting %s..." article))
		(condition-case ()
		    (delete-file article)
		  (file-error nil))
		(nnml-nov-delete-article newsgroup (car articles)))
	    (setq rest (cons (car articles) rest))))
      (setq articles (cdr articles)))
    (nnml-save-nov)
    rest))

(defun nnml-request-move-article (article group server accept-form)
  (let ((buf (get-buffer-create " *nnml move*"))
	result)
    (and 
     (nnml-request-article article group server)
     (save-excursion
       (set-buffer buf)
       (insert-buffer-substring nntp-server-buffer)
       (setq result (eval accept-form))
       (kill-buffer (current-buffer))
       result)
     (progn
       (condition-case ()
	   (delete-file (concat nnml-current-directory 
				(int-to-string article)))
	 (file-error nil))
       (nnml-nov-delete-article group article)
       (nnml-save-nov)))
    result))

(defun nnml-request-accept-article (group)
  (let (result)
    (if (stringp group)
	(and 
	 (nnml-request-list)
	 (setq nnml-group-alist (nnmail-get-active))
	 ;; We trick the choosing function into believing that only one
	 ;; group is availiable.  
	 (let ((nnmail-split-methods (list (list group ""))))
	   (setq result (car (nnml-save-mail))))
	 (progn
	   (nnmail-save-active nnml-group-alist nnml-active-file)
	   (nnml-save-nov)))
      (and
       (nnml-request-list)
       (setq nnml-group-alist (nnmail-get-active))
       (setq result (car (nnml-save-mail)))
       (progn
	 (nnmail-save-active nnml-group-alist nnml-active-file)
	 (nnml-save-nov))))
    result))


;;; Low-Level Interface

(defun nnml-retrieve-headers-with-nov (articles)
  (if (or gnus-nov-is-evil nnml-nov-is-evil)
      nil
    (let ((first (car articles))
	  (last (progn (while (cdr articles) (setq articles (cdr articles)))
		       (car articles)))
	  (nov (concat nnml-current-directory ".nov")))
      (if (file-exists-p nov)
	  (save-excursion
	    (set-buffer nntp-server-buffer)
	    (erase-buffer)
	    (insert-file-contents nov)
	    (goto-char 1)
	    (while (and (not (eobp)) (< first (read (current-buffer))))
	      (forward-line 1))
	    (beginning-of-line)
	    (if (not (eobp)) (delete-region 1 (point)))
	    (while (and (not (eobp)) (>= last (read (current-buffer))))
	      (forward-line 1))
	    (beginning-of-line)
	    (if (not (eobp)) (delete-region (point) (point-max)))
	    t)))))

(defun nnml-open-server-internal (host &optional service)
  "Open connection to news server on HOST by SERVICE."
  (save-excursion
    ;; Initialize communication buffer.
    (setq nntp-server-buffer (get-buffer-create " *nntpd*"))
    (set-buffer nntp-server-buffer)
    (buffer-disable-undo (current-buffer))
    (erase-buffer)
    (kill-all-local-variables)
    (setq case-fold-search t)		;Should ignore case.
    t))

(defun nnml-close-server-internal ()
  "Close connection to news server."
  nil)

(defun nnml-possibly-change-directory (newsgroup)
  (if newsgroup
      (let ((pathname (nnmail-article-pathname newsgroup nnml-directory)))
	(if (file-directory-p pathname)
	    (setq nnml-current-directory pathname)
	  (error "No such newsgroup: %s" newsgroup)))))

(defun nnml-create-directories ()
  (let ((methods nnmail-split-methods)
	dir dirs)
    (while methods
      (setq dir (nnmail-article-pathname (car (car methods)) nnml-directory))
      (while (not (file-directory-p dir))
	(setq dirs (cons dir dirs))
	(setq dir (file-name-directory (directory-file-name dir))))
      (while dirs
	(if (make-directory (directory-file-name (car dirs)))
	    (error "Could not create directory %s" (car dirs)))
	(and gnus-verbose-backends 
	     (message "Creating mail directory %s" (car dirs)))
	(setq dirs (cdr dirs)))
      (setq methods (cdr methods)))))

(defun nnml-save-mail ()
  "Called narrowed to an article."
  (let ((group-art (nreverse (nnmail-article-group 'nnml-active-number)))
	chars nov-line)
    (setq chars (nnmail-insert-lines))
    (nnmail-insert-xref group-art)
    (goto-char (point-min))
    (while (looking-at "From ")
      (replace-match "X-From-Line: ")
      (forward-line 1))
    ;; We save the article in all the newsgroups it belongs in.
    (let ((ga group-art)
	  first)
      (while ga
	(let ((file (concat (nnmail-article-pathname 
			     (car (car ga)) nnml-directory)
			    (int-to-string (cdr (car ga))))))
	  (if first
	      ;; It was already saved, so we just make a hard link.
	      (add-name-to-file first file t)
	    ;; Save the article.
	    (write-region (point-min) (point-max) file nil 
			  (if gnus-verbose-backends nil 'nomesg))
	    (setq first file)))
	(setq ga (cdr ga))))
    ;; Generate a nov line for this article. We generate the nov
    ;; line after saving, because nov generation destroys the
    ;; header. 
    (setq nov-line (nnml-make-nov-line chars))
    ;; Output the nov line to all nov databases that should have it.
    (let ((ga group-art))
      (while ga
	(nnml-add-nov (car (car ga)) (cdr (car ga)) nov-line)
	(setq ga (cdr ga))))
    group-art))

(defun nnml-active-number (group)
  "Compute the next article number in GROUP."
  (let ((active (car (cdr (assoc group nnml-group-alist)))))
    (setcdr active (1+ (cdr active)))
    (let (file)
      (while (file-exists-p
	      (setq file (concat (nnmail-article-pathname 
				  group nnml-directory)
				 (int-to-string (cdr active)))))
	(setcdr active (1+ (cdr active)))))
    (cdr active)))

(defun nnml-get-new-mail ()
  "Read new incoming mail."
  (let (incoming)
    (nnml-create-directories)
    (if (and nnml-get-new-mail nnmail-spool-file
	     (file-exists-p nnmail-spool-file)
	     (> (nth 7 (file-attributes nnmail-spool-file)) 0))
	(progn
	  (and gnus-verbose-backends 
	       (message "nnml: Reading incoming mail..."))
	  (setq incoming 
		(nnmail-move-inbox nnmail-spool-file 
				   (concat nnml-directory "Incoming")))
	  (nnml-request-list)
	  (setq nnml-group-alist (nnmail-get-active))
	  (nnmail-split-incoming incoming 'nnml-save-mail)
	  (nnmail-save-active nnml-group-alist nnml-active-file)
	  (nnml-save-nov)
	  (run-hooks 'nnmail-read-incoming-hook)
;;         (delete-file incoming)
	  (and gnus-verbose-backends
	       (message "nnml: Reading incoming mail...done"))))))


(defun nnml-add-nov (group article line)
  "Add a nov line for the GROUP base."
  (save-excursion 
    (set-buffer (nnml-open-nov group))
    (goto-char (point-max))
    (insert (int-to-string article) line)))

(defsubst nnml-header-value ()
  (buffer-substring (match-end 0) (save-excursion (end-of-line) (point))))

(defun nnml-make-nov-line (chars)
  "Create a nov from the current headers."
  (let ((case-fold-search t)
	subject from date id references lines xref in-reply-to char)
    (save-excursion
      (save-restriction
	(goto-char (point-min))
	(narrow-to-region 
	 (point)
	 (1- (or (search-forward "\n\n" nil t) (point-max))))
	;; Fold continuation lines.
	(goto-char (point-min))
	(while (re-search-forward "\\(\r?\n[ \t]+\\)+" nil t)
	  (replace-match " " t t))
	(subst-char-in-region (point-min) (point-max) ?\t ? )
	;; [number subject from date id references chars lines xref]
	(save-excursion
	  (goto-char (point-min))
	  (while (re-search-forward "^\\(from\\|subject\\|message-id\\|date\\|lines\\|xref\\|references\\|in-reply-to\\): "
				    nil t)
	    (beginning-of-line)
	    (setq char (downcase (following-char))) 
	    (cond
	     ((eq char ?s)
	      (setq subject (nnml-header-value)))
	     ((eq char ?f)
	      (setq from (nnml-header-value)))
	     ((eq char ?x)
	      (setq xref (nnml-header-value)))
	     ((eq char ?l)
	      (setq lines (nnml-header-value)))
	     ((eq char ?d)
	      (setq date (nnml-header-value)))
	     ((eq char ?m)
	      (setq id (setq id (nnml-header-value))))
	     ((eq char ?r)
	      (setq references (nnml-header-value)))
	     ((eq char ?i)
	      (setq in-reply-to (nnml-header-value))))
	    (forward-line 1))
      
	  (and (not references)
	       in-reply-to
	       (string-match "<[^>]+>" in-reply-to)
	       (setq references
		     (substring in-reply-to (match-beginning 0)
				(match-end 0)))))
	;; [number subject from date id references chars lines xref]
	(format "\t%s\t%s\t%s\t%s\t%s\t%d\t%s\t%s\n"
		(or subject "(none)")
		(or from "(nobody)") (or date "")
		(or id "") (or references "")
		(or chars 0) (or lines "0") (or xref ""))))))

(defun nnml-open-nov (group)
  (or (cdr (assoc group nnml-nov-buffer-alist))
      (let ((buffer (find-file-noselect 
		     (concat (nnmail-article-pathname 
			      group nnml-directory) ".nov"))))
	(save-excursion
	  (set-buffer buffer)
	  (buffer-disable-undo (current-buffer)))
	(setq nnml-nov-buffer-alist 
	      (cons (cons group buffer) nnml-nov-buffer-alist))
	buffer)))

(defun nnml-save-nov ()
  (save-excursion
    (while nnml-nov-buffer-alist
      (if (buffer-name (cdr (car nnml-nov-buffer-alist)))
	  (progn
	    (set-buffer (cdr (car nnml-nov-buffer-alist)))
	    (write-region 1 (point-max) (buffer-file-name) nil 'nomesg)
	    (set-buffer-modified-p nil)
	    (kill-buffer (current-buffer))))
      (setq nnml-nov-buffer-alist (cdr nnml-nov-buffer-alist)))))

(defun nnml-generate-nov-databases (dir)
  "Generate nov databases in all nnml mail newsgroups."
  (interactive 
   (progn   
     (setq nnml-group-alist nil)
     (list nnml-directory)))
  (nnml-open-server (system-name))
  (let ((dirs (directory-files dir t nil t)))
    (while dirs 
      (if (and (not (string-match "/\\.\\.$" (car dirs)))
	       (not (string-match "/\\.$" (car dirs)))
	       (file-directory-p (car dirs)))
	  (nnml-generate-nov-databases (car dirs)))
      (setq dirs (cdr dirs))))
  (let ((files (sort
		(mapcar
		 (function
		  (lambda (name)
		    (string-to-int name)))
		 (directory-files dir nil "^[0-9]+$" t))
		(function <)))
	(nov (concat dir "/.nov"))
	(nov-buffer (get-buffer-create "*nov*"))
	nov-line chars)
    (if files
	(setq nnml-group-alist 
	      (cons (list (nnmail-replace-chars-in-string 
			   (substring (expand-file-name dir)
				      (length (expand-file-name 
					       nnml-directory)))
			   ?/ ?.)
			  (cons (car files)
				(let ((f files))
				  (while (cdr f) (setq f (cdr f)))
				  (car f))))
		    nnml-group-alist)))
    (if files
	(save-excursion
	  (set-buffer nntp-server-buffer)
	  (if (file-exists-p nov)
	      (delete-file nov))
	  (save-excursion
	    (set-buffer nov-buffer)
	    (buffer-disable-undo (current-buffer))
	    (erase-buffer))
	  (while files
	    (erase-buffer)
	    (insert-file-contents (concat dir "/" (int-to-string (car files))))
	    (goto-char 1)
	    (narrow-to-region 1 (save-excursion (search-forward "\n\n" nil t)
						(setq chars (- (point-max) 
							       (point)))
						(point)))
	    (setq nov-line (nnml-make-nov-line chars))
	    (save-excursion
	      (set-buffer nov-buffer)
	      (goto-char (point-max))
	      (insert (int-to-string (car files)) nov-line))
	    (widen)
	    (setq files (cdr files)))
	  (save-excursion
	    (set-buffer nov-buffer)
	    (write-region 1 (point-max) (expand-file-name nov) nil
			  'nomesg)
	    (kill-buffer (current-buffer)))))
    (nnmail-save-active nnml-group-alist nnml-active-file)))

(defun nnml-nov-delete-article (group article)
  (save-excursion
    (set-buffer (nnml-open-nov group))
    (goto-char 1)
    (if (re-search-forward (concat "^" (int-to-string article) "\t"))
	(delete-region (match-beginning 0) (progn (forward-line 1) (point))))
    t))

(provide 'nnml)

;;; nnml.el ends here
