;;; nnmh.el --- mail spool access for Gnus
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
(require 'rmail)
(require 'nnmail)
(require 'gnus)

(defvar nnmh-directory "~/Mail/"
  "*Mail directory.")

(defvar nnmh-large-newsgroup 50
  "*The number of the articles which indicates a large newsgroup.
If the number of the articles is greater than the value, verbose
messages will be shown to indicate the current status.")



(defconst nnmh-version "nnmh 0.1"
  "nnmh version.")

(defvar nnmh-current-directory nil
  "Current news group directory.")

(defvar nnmh-status-string "")



;;; Interface functions.

(defun nnmh-retrieve-headers (sequence &optional newsgroup server)
  "Retrieve the headers for the articles in SEQUENCE.
Newsgroup must be selected before calling this function."
  (save-excursion
    (set-buffer nntp-server-buffer)
    (erase-buffer)
    (let ((file nil)
	  (number (length sequence))
	  (count 0)
	  beg article)
      (nnmh-possibly-change-directory newsgroup)
      (while sequence
	(setq article (car sequence))
	(setq file
	      (concat nnmh-current-directory (prin1-to-string article)))
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
	(and (numberp nnmh-large-newsgroup)
	     (> number nnmh-large-newsgroup)
	     (zerop (% count 20))
	     (message "NNMH: Receiving headers... %d%%"
		      (/ (* count 100) number))))

      (and (numberp nnmh-large-newsgroup)
	   (> number nnmh-large-newsgroup)
	   (message "NNMH: Receiving headers... done"))

      ;; Fold continuation lines.
      (goto-char 1)
      (while (re-search-forward "\\(\r?\n[ \t]+\\)+" nil t)
	(replace-match " " t t))
      'headers)))

(defun nnmh-open-server (host &optional service)
  "Open news server on HOST.
If HOST is nil, use value of environment variable `NNTPSERVER'.
If optional argument SERVICE is non-nil, open by the service name."
  (setq nnmh-status-string "")
  (nnmail-open-server-internal host service))

(defun nnmh-close-server (&optional server)
  "Close news server."
  (nnmh-close-server-internal))

(fset 'nnmh-request-quit (symbol-function 'nnmh-close-server))

(defun nnmh-server-opened (&optional server)
  "Return server process status, T or NIL.
If the stream is opened, return T, otherwise return NIL."
  (and nntp-server-buffer
       (get-buffer nntp-server-buffer)))

(defun nnmh-status-message ()
  "Return server status response as string."
  nnmh-status-string)

(defun nnmh-request-article (id &optional newsgroup server buffer)
  "Select article by message ID (or number)."
  (nnmh-possibly-change-directory newsgroup)
  (let ((file (if (stringp id)
		  nil
		(concat nnmh-current-directory (prin1-to-string id))))
	(nntp-server-buffer (or buffer nntp-server-buffer)))
    (if (and (stringp file)
	     (file-exists-p file)
	     (not (file-directory-p file)))
	(save-excursion
	  (nnmh-find-file file)))))

(defun nnmh-request-group (group &optional server dont-check)
  "Select news GROUP."
  (if (not dont-check)
      (nnmh-get-new-mail))
  (let ((pathname (nnmh-article-pathname group))
	dir)
    (if (file-directory-p pathname)
	(progn
	  (setq nnmh-current-directory pathname)
	  (nnmh-update-gnus-unreads group)
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

(defun nnmh-request-list (&optional server dir)
  "Get list of active articles in all newsgroups."
  (or dir
      (save-excursion
	(set-buffer nntp-server-buffer)
	(erase-buffer)
	(setq dir nnmh-directory)))
  ;; Recurse down all directories.
  (let ((dirs (directory-files dir t nil t)))
    (while dirs 
      (if (and (not (string-match "/\\.\\.$" (car dirs)))
	       (not (string-match "/\\.$" (car dirs)))
	       (file-directory-p (car dirs)))
	  (nnmh-request-list server (car dirs)))
      (setq dirs (cdr dirs))))
  ;; For each directory, generate an active file line.
  (if (not (string= (expand-file-name nnmh-directory) (expand-file-name dir)))
      (let ((files (mapcar
		    (function
		     (lambda (name)
		       (string-to-int name)))
		    (directory-files dir nil "^[0-9]+$" t))))
	(save-excursion
	  (set-buffer nntp-server-buffer)
	  (insert (format "%s %d %d y\n" 
			  (progn
			    (string-match 
			     (expand-file-name nnmh-directory) dir)
			    (nnmh-replace-chars-in-string
			     (substring (expand-file-name dir)
					(match-end 0)) ?/ ?.))
			  (if files (apply (function max) files) 0)
			  (if files (apply (function min) files) 0)))))))

(defun nnmh-request-post (&optional server)
  "Post a new news in current buffer."
  (mail-send-and-exit nil))

(fset 'nnmh-request-post-buffer 'nnmail-request-post-buffer)

(defun nnmh-request-expire-articles (articles newsgroup &optional server force)
  "Expire all articles in the ARTICLES list in group GROUP.
The list of unexpired articles will be returned (ie. all articles that
were too fresh to be expired).
If FORCE is non-nil, ARTICLES will be deleted whether they are old or not."
  (nnmh-possibly-change-directory newsgroup)
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
      (setq article (concat nnmh-current-directory (int-to-string
						      (car articles))))
      (if (setq mod-time (nth 5 (file-attributes article)))
	  (if (or force
		  (< (car mod-time) (car day-time))
		  (and (= (car mod-time) (car day-time))
		       (< (car (cdr mod-time)) (car (cdr day-time)))))
	      (progn
		(message "Deleting %s..." article)
		(condition-case ()
		    (delete-file article)
		  (file-error nil)))
	    (setq rest (cons (car articles) rest))))
      (setq articles (cdr articles)))
    rest))

(defun nnmh-request-move-article (article group server accept-form)
  (let ((buf (get-buffer-create " *nnmh move*"))
	result)
    (and 
     (nnmh-request-article article group server)
     (save-excursion
       (set-buffer buf)
       (insert-buffer-substring nntp-server-buffer)
       (setq result (eval accept-form))
       (kill-buffer (current-buffer))
       result)
     (condition-case ()
	 (delete-file (concat nnmh-current-directory 
			      (int-to-string article)))
       (file-error nil)))
 result))

(defun nnmh-request-accept-article (group)
  (let (result)
    (if (stringp group)
	(and 
	 (nnmh-get-active)
	 ;; We trick the choosing function into believing that only one
	 ;; group is availiable.  
	 (let ((nnmail-split-methods '(group "")))
	   (setq result 
		 (cons group (nnmh-choose-mail (point-min) (point-max))))))
      (and
       (nnmh-get-active)
       (setq result (nnmh-choose-mail (point-min) (point-max)))))
    result))


;;; Low-Level Interface

(defun nnmh-open-server-internal (host &optional service)
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

(defun nnmh-close-server-internal ()
  "Close connection to news server."
  nil)

(defun nnmh-find-file (file)
  "Insert FILE in server buffer safely."
  (set-buffer nntp-server-buffer)
  (erase-buffer)
  (condition-case ()
      (progn (insert-file-contents file) t)
    (file-error nil)))

(defun nnmh-possibly-change-directory (newsgroup)
  (if newsgroup
      (let ((pathname (nnmh-article-pathname newsgroup)))
	(if (file-directory-p pathname)
	    (setq nnmh-current-directory pathname)
	  (error "No such newsgroup: %s" newsgroup)))))

(defun nnmh-article-pathname (group)
  "Make pathname for GROUP."
  (concat (file-name-as-directory (expand-file-name nnmh-directory))
	  (nnmh-replace-chars-in-string group ?. ?/) "/"))

(defun nnmh-replace-chars-in-string (string from to)
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

(defun nnmh-create-directories ()
  (let ((methods nnmail-split-methods)
	dir dirs)
    (while methods
      (setq dir (nnmh-article-pathname (car (car methods))))
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
(defun nnmh-move-inbox ()
  (let ((inbox (expand-file-name nnmail-spool-file))
	tofile errors)
    (setq tofile (make-temp-name
		  (expand-file-name (concat nnmh-directory "Incoming"))))
    (unwind-protect
	(save-excursion
	  (setq errors (generate-new-buffer " *nnmh loss*"))
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

(defvar nnmh-newsgroups nil)

(defun nnmh-get-active ()
  (let ((methods nnmail-split-methods))
    (setq nnmh-newsgroups nil)
    (if (nnmh-request-list)
	(save-excursion
	  (set-buffer (get-buffer-create " *nntpd*"))
	  (goto-char 1)
	  (while (re-search-forward 
		  "^\\([^ \t]+\\)[ \t]+\\([0-9]+\\)[ \t]+\\([0-9]+\\)" nil t)
	    (setq nnmh-newsgroups 
		  (cons (list (buffer-substring (match-beginning 1) 
						(match-end 1))
			      (cons (string-to-int 
				     (buffer-substring (match-beginning 3)
						       (match-end 3)))
				    (string-to-int 
				     (buffer-substring (match-beginning 2)
						       (match-end 2)))))
			nnmh-newsgroups)))))
    (while methods
      (if (not (assoc (car (car methods)) nnmh-newsgroups))
	  (setq nnmh-newsgroups
		(cons (list (car (car methods)) (cons 1 0)) 
		      nnmh-newsgroups)))
      (setq methods (cdr methods)))
    t))

(defun nnmh-split-incoming (incoming)
  "Go through the entire INCOMING file and pick out each individual mail."
  (let (start)
    (nnmh-get-active)
    (save-excursion
      (set-buffer (get-buffer-create "*(ding) Gnus mail*"))
      (buffer-disable-undo (current-buffer))
      (erase-buffer)
      (insert-file-contents incoming)
      (goto-char 1)
      (save-excursion
	(run-hooks 'nnmail-prepare-incoming-hook))
      ;; Go to the beginning of the first mail...
      (if (and (re-search-forward (concat "^" rmail-unix-mail-delimiter) nil t)
	       (goto-char (match-beginning 0)))
	  ;; and then carry on until the bitter end.
	  (while (not (eobp))
	    (setq start (point))
	    ;; Skip all the headers in case there are mode "From "s...
	    (if (not (search-forward "\n\n" nil t))
		(forward-line 1))
	    (if (re-search-forward 
		 (concat "^" rmail-unix-mail-delimiter) nil t)
		(goto-char (match-beginning 0))
	      (goto-char (point-max)))
	    (nnmh-choose-mail start (point))))
      (kill-buffer (current-buffer)))))

;; Mail crossposts syggested by Brian Edmonds <edmonds@cs.ubc.ca>. 
(defun nnmh-article-group (beg end)
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
			       (nnmh-active-number (car (car methods))))
			 group-art)))
	    (or group-art
		(setq group-art 
		      (list (cons (car (car methods)) 
				  (nnmh-active-number (car (car methods))))))))
	  (setq methods (cdr methods)))
	group-art))))

(defun nnmh-choose-mail (beg end)
  "Find out what mail group the mail between BEG and END belongs in."
  (let ((group-art (nreverse (nnmh-article-group beg end)))
	lines)
    (save-excursion
      (save-restriction
	(narrow-to-region beg end)
	;; First fix headers.
	(goto-char (point-min))
	(save-excursion
	  (save-restriction
	    (narrow-to-region (point)
			      (progn (search-forward "\n\n" nil t) 
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
	    (insert "\n")))
	;; Then we actually save the article.
	(let ((ga group-art)
	      first)
	  (while ga
	    (let ((file (concat (nnmh-article-pathname 
				 (car (car ga)))
				(int-to-string (cdr (car ga))))))
	      (if first
		  ;; It was already saved, so we just make a hard link.
		  (add-name-to-file first file t)
		;; Save the article.
		(write-region (point-min) (point-max) file nil nil)
		(sleep-for 1)
		(setq first file)))
	    (setq ga (cdr ga))))
	group-art))))

(defun nnmh-active-number (group)
  "Compute the next article number in GROUP."
  (let ((active (car (cdr (assoc group nnmh-newsgroups)))))
    (setcdr active (1+ (cdr active)))
    (let (file)
      (while (file-exists-p
	      (setq file (concat (nnmh-article-pathname group)
				 (int-to-string (cdr active)))))
	(setcdr active (1+ (cdr active)))))
    (cdr active)))

(defun nnmh-get-new-mail ()
  "Read new incoming mail."
  (let (incoming)
    (nnmh-create-directories)
    (if (and (file-exists-p nnmail-spool-file)
	     (> (nth 7 (file-attributes nnmail-spool-file)) 0))
	(progn
	  (message "nnmh: Reading incoming mail...")
	  (setq incoming (nnmh-move-inbox))
	  (nnmh-split-incoming incoming)
	  (run-hooks 'nnmail-read-incoming-hook)
;;         (delete-file incoming)
	  (message "nnmh: Reading incoming mail...done")))))

(defun nnmh-update-gnus-unreads (group)
  ;; Go through the .nnmh-articles file and compare with the actual
  ;; articles in this folder. The articles that are "new" will be
  ;; marked as unread by Gnus.
  (let* ((dir nnmh-current-directory)
	 (files (sort (mapcar (function (lambda (name) (string-to-int name)))
			      (directory-files nnmh-current-directory 
					       nil "^[0-9]+$" t)) '<))
	 (nnmh-file (concat dir ".nnmh-articles"))
	 new articles)
    ;; Load the .nnmh-articles file.
    (if (file-exists-p nnmh-file)
	(setq articles 
	      (let (nnmh-newsgroup-articles)
		(condition-case nil (load nnmh-file nil t t) (error nil))
		nnmh-newsgroup-articles)))
    ;; Add all new articles to the `new' list.
    (let ((art files))
      (while art
	(if (not (assq (car art) articles)) (setq new (cons (car art) new)))
	(setq art (cdr art))))
    ;; Remove all deleted articles.
    (let ((art articles))
      (while art
	(if (not (memq (car (car art)) files))
	    (setq articles (delq (car art) articles)))
	(setq art (cdr art))))
    ;; Check whether the highest-numbered articles really are the ones
    ;; that Gnus thinks they are by looking at the time-stamps.
    (let ((art articles))
      (while (and art 
		  (not (equal 
			(nth 5 (file-attributes 
				(concat dir (int-to-string (car (car art))))))
			(cdr (car art)))))
	(setq articles (delq (car art) articles))
	(setq new (cons (car (car art)) new))
	(setq art (cdr art))))
    ;; Go through all the new articles and add them, and their
    ;; time-stamps to the list.
    (let ((n new))
      (while n
	(setq articles 
	      (cons (cons 
		     (car n)
		     (nth 5 (file-attributes 
			     (concat dir (int-to-string (car n))))))
		    articles))
	(setq n (cdr n))))
    ;; Make Gnus mark all new articles as unread.
    (save-excursion
      (gnus-make-articles-unread group (setq new (sort new '<))))
    ;; Sort the article list with highest numbers first.
    (setq articles (sort articles (lambda (art1 art2) 
				    (> (car art1) (car art2)))))
    ;; Finally write this list back to the .nnmh-articles file.
    (save-excursion
      (set-buffer (get-buffer-create "*nnmh out*"))
      (insert ";; Gnus article active file for " group "\n\n")
      (insert "(setq nnmh-newsgroup-articles '")
      (insert (prin1-to-string articles) ")\n")
      (write-region (point-min) (point-max) nnmh-file nil 'nomesg)
      (kill-buffer (current-buffer)))))

(provide 'nnmh)

;;; nnmh.el ends here
