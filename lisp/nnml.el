;;; nnml.el --- mail spool access for Gnus

;; Copyright (C) 1994 Free Software Foundation, Inc.

;; Author: Lars Ingebrigtsen <larsi@ifi.uio.no>
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

;; Based on nnspool.el by Masanobu UMEDA <umerin@flab.flab.fujitsu.junet>.

;;; Code:

(require 'nnheader)
(require 'rmail)
(require 'nnmail)

(defvar nnml-directory "~/Mail/"
  "*Mail directory.")

(defvar nnml-active-file (concat nnml-directory "active")
  "*Mail active file.")

(defvar nnml-newsgroups-file (concat nnml-directory "newsgroups")
  "*Mail newsgroups description file.")

(defvar nnml-nov-is-evil nil
  "If non-nil, Gnus will never generate and use nov databases for mail groups.
Using nov databases will speed up header fetching considerably.
This variable shouldn't be flipped much. If you have, for some reason,
set this to t, and want to set it to nil again, you should always run
the `nnml-generate-nov-databases' command. The function will go
through all nnml directories and generate nov databases for them
all. This may very well take some time.")

(defvar nnml-large-newsgroup 50
  "*The number of the articles which indicates a large newsgroup.
If the number of the articles is greater than the value, verbose
messages will be shown to indicate the current status.")



(defconst nnml-version "nnml 0.2"
  "nnml version.")

(defvar nnml-current-directory nil
  "Current news group directory.")

(defvar nnml-status-string "")

(defvar nnml-nov-buffer-alist nil)



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
      (if (nnml-retrieve-header-with-nov sequence)
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
	  (and (numberp nnml-large-newsgroup)
	       (> number nnml-large-newsgroup)
	       (zerop (% count 20))
	       (message "NNML: Receiving headers... %d%%"
			(/ (* count 100) number))))

	(and (numberp nnml-large-newsgroup)
	     (> number nnml-large-newsgroup)
	     (message "NNML: Receiving headers... done"))

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
    (nnmail-open-server-internal host service)))

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
	  (nnml-find-file file)))))

(defun nnml-request-group (group &optional server dont-check)
  "Select news GROUP."
  (if (not dont-check)
      (nnml-get-new-mail))
  (let ((pathname (nnml-article-pathname group))
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

(defun nnml-request-list (&optional server)
  "List active newsgoups."
  (save-excursion
    (nnml-find-file nnml-active-file)))

(defun nnml-request-list-newsgroups (&optional server)
  "List newsgroups (defined in NNTP2)."
  (save-excursion
    (nnml-find-file nnml-newsgroups-file)))

(defun nnml-request-post (&optional server)
  "Post a new news in current buffer."
  (mail-send-and-exit nil))

(fset 'nnml-request-post-buffer 'nnmail-request-post-buffer)

(defun nnml-request-expire-articles (articles newsgroup &optional server)
  "Expire all articles in the ARTICLES list in group GROUP.
The list of unexpired articles will be returned (ie. all articles that
were too fresh to be expired)."
  (nnml-possibly-change-directory newsgroup)
  (let* ((days (or (and nnmail-expiry-wait-function
			(funcall nnmail-expiry-wait-function newsgroup))
		   nnmail-expiry-wait))
	 (cur-time (current-time))
	 (day-sec (* 24 60 60 days))
	 (day-time (list nil nil))
	 mod-time article rest)
    (setcar day-time (/ day-sec 65536))
    (setcar (cdr day-time) (- day-sec (* (car day-time) 65536)))
    (if (< (car (cdr cur-time)) (car (cdr day-time)))
	(progn
	  (setcar day-time (+ 1 (- (car cur-time) (car day-time))))
	  (setcar (cdr day-time) (- (+ 65536 (car (cdr cur-time)))
				    (car (cdr day-time)))))
      (setcar day-time (- (car cur-time) (car day-time)))
      (setcar (cdr day-time) (- (car (cdr cur-time)) (car (cdr day-time)))))
    (while articles
      (setq article (concat nnml-current-directory (int-to-string
						      (car articles))))
      (if (setq mod-time (nth 5 (file-attributes article)))
	  (if (or (< (car mod-time) (car day-time))
		  (and (= (car mod-time) (car day-time))
		       (< (car (cdr mod-time)) (car (cdr day-time)))))
	      (progn
		(message "Deleting %s..." article)
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
     (and (condition-case ()
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
	 (nnml-get-active)
	 ;; We trick the choosing function into believing that only one
	 ;; group is availiable.  
	 (let ((nnmail-split-methods '(group "")))
	   (setq result 
		 (cons group (nnml-choose-mail (point-min) (point-max)))))
	 (nnml-save-active))
      (and
       (nnml-get-active)
       (setq result (nnml-choose-mail (point-min) (point-max)))
       (nnml-save-active)))
    result))


;;; Low-Level Interface

(defun nnml-retrieve-header-with-nov (articles)
  (if nnml-nov-is-evil
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
    (if (not (string-equal host (system-name)))
	(error "nnml: cannot talk to %s." host))
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

(defun nnml-find-file (file)
  "Insert FILE in server buffer safely."
  (set-buffer nntp-server-buffer)
  (erase-buffer)
  (condition-case ()
      (progn (insert-file-contents file) t)
    (file-error nil)))

(defun nnml-possibly-change-directory (newsgroup)
  (if newsgroup
      (let ((pathname (nnml-article-pathname newsgroup)))
	(if (file-directory-p pathname)
	    (setq nnml-current-directory pathname)
	  (error "No such newsgroup: %s" newsgroup)))))

(defun nnml-article-pathname (group)
  "Make pathname for GROUP."
  (concat (file-name-as-directory (expand-file-name nnml-directory))
	  (nnml-replace-chars-in-string group ?. ?/) "/"))

(defun nnml-replace-chars-in-string (string from to)
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

(defun nnml-create-directories ()
  (let ((methods nnmail-split-methods)
	dir dirs)
    (while methods
      (setq dir (nnml-article-pathname (car (car methods))))
      (while (not (file-directory-p dir))
	(setq dirs (cons dir dirs))
	(setq dir (file-name-directory (directory-file-name dir))))
      (while dirs
	(if (make-directory (directory-file-name (car dirs)))
	    (error "Could not create directory %s" (car dirs)))
	(message "Creating mail directory %s" (car dirs))
	(setq dirs (cdr dirs)))
      (setq methods (cdr methods)))))

;; Most of this function was taken from rmail.el
(defun nnml-move-inbox ()
  (let ((inbox (expand-file-name nnmail-spool-file))
	tofile errors)
    (setq tofile (make-temp-name
		  (expand-file-name (concat nnml-directory "Incoming"))))
    (unwind-protect
	(save-excursion
	  (setq errors (generate-new-buffer " *nnml loss*"))
	  (buffer-disable-undo errors)
	  (call-process
	   (expand-file-name "movemail" exec-directory)
	   nil errors nil inbox tofile)
	  (if (not (buffer-modified-p errors))
	      ;; No output => movemail won
	      nil
	    (set-buffer errors)
	    (subst-char-in-region (point-min) (point-max) ?\n ?\  )
	    (goto-char (point-max))
	    (skip-chars-backward " \t")
	    (delete-region (point) (point-max))
	    (goto-char (point-min))
	    (if (looking-at "movemail: ")
		(delete-region (point-min) (match-end 0)))
	    (error (concat "movemail: "
			   (buffer-substring (point-min)
					     (point-max)))))))
    (if (buffer-name errors)
	(kill-buffer errors))
    tofile))

(defvar nnml-newsgroups nil)

(defun nnml-get-active ()
  (let ((methods nnmail-split-methods))
    (setq nnml-newsgroups nil)
    (if (nnml-request-list)
	(save-excursion
	  (set-buffer (get-buffer-create " *nntpd*"))
	  (goto-char 1)
	  (while (re-search-forward 
		  "^\\([^ \t]+\\)[ \t]+\\([0-9]+\\)[ \t]+\\([0-9]+\\)" nil t)
	    (setq nnml-newsgroups 
		  (cons (list (buffer-substring (match-beginning 1) 
						(match-end 1))
			      (cons (string-to-int 
				     (buffer-substring (match-beginning 3)
						       (match-end 3)))
				    (string-to-int 
				     (buffer-substring (match-beginning 2)
						       (match-end 2)))))
			nnml-newsgroups)))))
    (while methods
      (if (not (assoc (car (car methods)) nnml-newsgroups))
	  (setq nnml-newsgroups
		(cons (list (car (car methods)) (cons 1 0)) 
		      nnml-newsgroups)))
      (setq methods (cdr methods)))
    t))

(defun nnml-save-active ()
  (let ((groups nnml-newsgroups)
	group)
    (save-excursion
      (set-buffer (get-buffer-create " *nnml*"))
      (buffer-disable-undo (current-buffer))
      (erase-buffer)
      (while groups
	(setq group (car groups))
	(insert (format "%s %d %d y\n" (car group) (cdr (car (cdr group)) )
			(car (car (cdr group)))))
	(setq groups (cdr groups)))
      (write-region 1 (point-max) (expand-file-name nnml-active-file) nil 
		    'nomesg)
      (kill-buffer (current-buffer)))))

(defun nnml-split-incoming (incoming)
  "Go through the entire INCOMING file and pick out each individual mail."
  (let (start)
    (nnml-get-active)
    (save-excursion
      (set-buffer (get-buffer-create "*(ding) Gnus mail*"))
      (buffer-disable-undo (current-buffer))
      (erase-buffer)
      (insert-file-contents incoming)
      (goto-char 1)
      ;; Go to the beginning of the first mail...
      (if (and (re-search-forward (concat "^" rmail-unix-mail-delimiter) nil t)
	       (goto-char (match-beginning 0)))
	  ;; and then carry on until the bitter end.
	  (while (not (eobp))
	    (setq start (point))
	    (forward-line 1)
	    (if (re-search-forward 
		 (concat "^" rmail-unix-mail-delimiter) nil t)
		(goto-char (match-beginning 0))
	      (goto-char (point-max)))
	    (nnml-choose-mail start (point))))
      (kill-buffer (current-buffer)))))

;; Mail crossposts syggested by Brian Edmonds <edmonds@cs.ubc.ca>. 
(defun nnml-article-group (beg end)
  (let ((methods nnmail-split-methods)
	found group-art)
    (save-excursion
      (save-restriction
	(narrow-to-region beg end)
	(while methods
	  (goto-char (point-max))
	  (if (or (cdr methods)
		  (not (string= "" (nth 1 (car methods)))))
	      (if (re-search-backward (car (cdr (car methods))) nil t)
		  (setq group-art
			(cons 
			 (cons (car (car methods))
			       (nnml-active-number (car (car methods))))
			 group-art)))
	    (or group-art
		(setq group-art 
		      (list (cons (car (car methods)) 
				  (nnml-active-number (car (car methods))))))))
	  (setq methods (cdr methods)))
	group-art))))

(defun nnml-choose-mail (beg end)
  "Find out what mail group the mail between BEG and END belongs in."
  (let ((group-art (nreverse (nnml-article-group beg end)))
	chars nov-line lines)
    (save-excursion
      (save-restriction
	(narrow-to-region beg end)
	;; First fix headers.
	(goto-char (point-min))
	(save-excursion
	  (save-restriction
	    (narrow-to-region (point)
			      (progn (search-forward "\n\n" nil t) 
				     (setq chars (- (point-max) (point)))
				     (setq lines (- (count-lines 
						     (point) (point-max)) 1))
				     (1- (point))))
	    ;; Insert Lines.
	    (if (not (save-excursion (re-search-backward "^Lines:" beg t)))
		(insert (format "Lines: %d\n" lines)))
	    ;; Make an Xref header.
	    (save-excursion
	      (goto-char (point-max))
	      (if (re-search-backward "^Xref:" nil t)
		  (delete-region (match-beginning 0) 
				 (progn (forward-line 1) (point)))))
	    (insert (format "Xref: %s" (system-name)))
	    (let ((ga group-art))
	      (while ga
		(insert (format " %s:%d" (car (car ga)) (cdr (car ga))))
		(setq ga (cdr ga))))
	    (insert "\n")
	    ;; Generate a nov line for this article.
	    (setq nov-line (nnml-make-nov-line chars))))
	;; Then we actually save the article.
	(let ((ga group-art)
	      first)
	  (while ga
	    (nnml-add-nov (car (car ga)) (cdr (car ga)) nov-line)
	    (let ((file (concat (nnml-article-pathname 
				 (car (car ga)))
				(int-to-string (cdr (car ga))))))
	      (if first
		  ;; It was already saved, so we just make a hard link.
		  (add-name-to-file first file t)
		;; Save the article.
		(write-region (point-min) (point-max) file nil nil)
		(setq first file)))
	    (setq ga (cdr ga))))
	group-art))))

(defun nnml-active-number (group)
  "Compute the next article number in GROUP."
  (let ((active (car (cdr (assoc group nnml-newsgroups)))))
    (setcdr active (1+ (cdr active)))
    (let (file)
      (while (file-exists-p
	      (setq file (concat (nnml-article-pathname group)
				 (int-to-string (cdr active)))))
	(setcdr active (1+ (cdr active)))))
    (cdr active)))

(defun nnml-get-new-mail ()
  "Read new incoming mail."
  (let (incoming)
    (nnml-create-directories)
    (if (and (file-exists-p nnmail-spool-file)
	     (> (nth 7 (file-attributes nnmail-spool-file)) 0))
	(progn
	  (message "nnml: Reading incoming mail...")
	  (setq incoming (nnml-move-inbox))
	  (nnml-split-incoming incoming)
	  (nnml-save-active)
	  (nnml-save-nov)
;;         (delete-file incoming)
	  (message "nnml: Reading incoming mail...done")))))


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
  (let (subject from date id references lines xref in-reply-to char)
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
	      chars (or lines "0") (or xref ""))))

(defun nnml-open-nov (group)
  (or (cdr (assoc group nnml-nov-buffer-alist))
      (let ((buffer (find-file-noselect 
		     (concat (nnml-article-pathname group) ".nov"))))
	(save-excursion
	  (set-buffer buffer)
	  (buffer-disable-undo (current-buffer)))
	(setq nnml-nov-buffer-alist (cons (cons group buffer)
					  nnml-nov-buffer-alist))
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
  (interactive (list nnml-directory))
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
	    (kill-buffer (current-buffer)))))))

(defun nnml-nov-delete-article (group article)
  (save-excursion
    (set-buffer (nnml-open-nov group))
    (goto-char 1)
    (if (re-search-forward (concat "^" (int-to-string article) "\t"))
	(delete-region (match-beginning 0) (progn (forward-line 1) (point))))))

(provide 'nnml)

;;; nnml.el ends here
