;;; nnfolder.el --- mail folder access for Gnus
;; Copyright (C) 1995,96 Free Software Foundation, Inc.

;; Author: Scott Byer <byer@mv.us.adobe.com>
;;	Lars Magne Ingebrigtsen <larsi@ifi.uio.no>
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
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; For an overview of what the interface functions do, please see the
;; Gnus sources.  

;; Various enhancements by byer@mv.us.adobe.com (Scott Byer).

;;; Code:

(require 'nnheader)
(require 'rmail)
(require 'nnmail)
(eval-when-compile (require 'cl))

(defvar nnfolder-directory (expand-file-name "~/Mail/")
  "The name of the nnfolder directory.")

(defvar nnfolder-active-file 
  (concat (file-name-as-directory nnfolder-directory) "active")
  "The name of the active file.")

;; I renamed this variable to something more in keeping with the general GNU
;; style. -SLB

(defvar nnfolder-ignore-active-file nil
  "If non-nil, causes nnfolder to do some extra work in order to determine the true active ranges of an mbox file.  
Note that the active file is still saved, but it's values are not
used.  This costs some extra time when scanning an mbox when opening
it.")

(defvar nnfolder-newsgroups-file 
  (concat (file-name-as-directory nnfolder-directory) "newsgroups")
  "Mail newsgroups description file.")

(defvar nnfolder-get-new-mail t
  "If non-nil, nnfolder will check the incoming mail file and split the mail.")

(defvar nnfolder-prepare-save-mail-hook nil
  "Hook run narrowed to an article before saving.")

(defvar nnfolder-inhibit-expiry nil
  "If non-nil, inhibit expiry.")



(defconst nnfolder-version "nnfolder 1.0"
  "nnfolder version.")

(defconst nnfolder-article-marker "X-Gnus-Article-Number: "
  "String used to demarcate what the article number for a message is.")

(defvar nnfolder-current-group nil)
(defvar nnfolder-current-buffer nil)
(defvar nnfolder-status-string "")
(defvar nnfolder-group-alist nil)
(defvar nnfolder-buffer-alist nil)
(defvar nnfolder-active-timestamp nil)



(defvar nnfolder-current-server nil)
(defvar nnfolder-server-alist nil)
(defvar nnfolder-server-variables 
  `((nnfolder-directory ,nnfolder-directory)
    (nnfolder-active-file ,nnfolder-active-file)
    (nnfolder-newsgroups-file ,nnfolder-newsgroups-file)
    (nnfolder-get-new-mail ,nnfolder-get-new-mail)
    (nnfolder-inhibit-expiry ,nnfolder-inhibit-expiry) 
    (nnfolder-current-group nil)
    (nnfolder-current-buffer nil)
    (nnfolder-status-string "")
    (nnfolder-group-alist nil)
    (nnfolder-buffer-alist nil)
    (nnfolder-active-timestamp nil)))



;;; Interface functions

(defun nnfolder-retrieve-headers (sequence &optional newsgroup server fetch-old)
  (save-excursion
    (set-buffer nntp-server-buffer)
    (erase-buffer)
    (let ((delim-string (concat "^" rmail-unix-mail-delimiter))
	  article art-string start stop)
      (nnfolder-possibly-change-group newsgroup server)
      (set-buffer nnfolder-current-buffer)
      (goto-char (point-min))
      (if (stringp (car sequence))
	  'headers
	(while sequence
	  (setq article (car sequence))
	  (setq art-string (nnfolder-article-string article))
	  (set-buffer nnfolder-current-buffer)
	  (if (or (search-forward art-string nil t)
		  ;; Don't search the whole file twice!  Also, articles
		  ;; probably have some locality by number, so searching
		  ;; backwards will be faster.  Especially if we're at the
		  ;; beginning of the buffer :-). -SLB
		  (search-backward art-string nil t))
	      (progn
		(setq start (or (re-search-backward delim-string nil t)
				(point)))
		(search-forward "\n\n" nil t)
		(setq stop (1- (point)))
		(set-buffer nntp-server-buffer)
		(insert (format "221 %d Article retrieved.\n" article))
		(insert-buffer-substring nnfolder-current-buffer start stop)
		(goto-char (point-max))
		(insert ".\n")))
	  (setq sequence (cdr sequence)))

	(set-buffer nntp-server-buffer)
	(nnheader-fold-continuation-lines)
	'headers))))

(defun nnfolder-open-server (server &optional defs)
  (nnheader-change-server 'nnfolder server defs)
  (when (not (file-exists-p nnfolder-directory))
    (condition-case ()
	(make-directory nnfolder-directory t)
      (error t)))
  (cond 
   ((not (file-exists-p nnfolder-directory))
    (nnfolder-close-server)
    (nnheader-report 'nnfolder "Couldn't create directory: %s"
		     nnfolder-directory))
   ((not (file-directory-p (file-truename nnfolder-directory)))
    (nnfolder-close-server)
    (nnheader-report 'nnfolder "Not a directory: %s" nnfolder-directory))
   (t
    (nnheader-report 'nnfolder "Opened server %s using directory %s"
		     server nnfolder-directory)
    t)))

(defun nnfolder-close-server (&optional server)
  (setq nnfolder-current-server nil
	nnfolder-group-alist nil)
  t)

(defun nnfolder-server-opened (&optional server)
  (and (equal server nnfolder-current-server)
       nntp-server-buffer
       (buffer-name nntp-server-buffer)))

(defun nnfolder-request-close ()
  (let ((alist nnfolder-buffer-alist))
    (while alist
      (nnfolder-close-group (caar alist) nil t)
      (setq alist (cdr alist))))
  (setq nnfolder-buffer-alist nil
	nnfolder-current-server nil
	nnfolder-group-alist nil))

(defun nnfolder-status-message (&optional server)
  nnfolder-status-string)

(defun nnfolder-request-article (article &optional newsgroup server buffer)
  (nnfolder-possibly-change-group newsgroup server)
  (save-excursion
    (set-buffer nnfolder-current-buffer)
    (goto-char (point-min))
    (if (search-forward (nnfolder-article-string article) nil t)
	(let (start stop)
	  (re-search-backward (concat "^" rmail-unix-mail-delimiter) nil t)
	  (setq start (point))
	  (forward-line 1)
	  (or (and (re-search-forward 
		    (concat "^" rmail-unix-mail-delimiter) nil t)
		   (forward-line -1))
	      (goto-char (point-max)))
	  (setq stop (point))
	  (let ((nntp-server-buffer (or buffer nntp-server-buffer)))
	    (set-buffer nntp-server-buffer)
	    (erase-buffer)
	    (insert-buffer-substring nnfolder-current-buffer start stop)
	    (goto-char (point-min))
	    (while (looking-at "From ")
	      (delete-char 5)
	      (insert "X-From-Line: ")
	      (forward-line 1))
	    (if (numberp article) 
		(cons nnfolder-current-group article)
	      (goto-char (point-min))
	      (search-forward (concat "\n" nnfolder-article-marker))
	      (cons nnfolder-current-group
		    (string-to-int 
		     (buffer-substring 
		      (point) (progn (end-of-line) (point)))))))))))

(defun nnfolder-request-group (group &optional server dont-check)
  (save-excursion
    (nnmail-activate 'nnfolder)
    (if (not (assoc group nnfolder-group-alist))
	(nnheader-report 'nnfolder "No such group: %s" group)
      (nnfolder-possibly-change-group group server)
      (if dont-check
	  (progn 
	    (nnheader-report 'nnfolder "Selected group %s" group)
	    t)
	(let* ((active (assoc group nnfolder-group-alist))
	       (group (car active))
	       (range (cadr active))
	       (minactive (car range))
	       (maxactive (cdr range)))
	  (cond 
	   ((null active)
	    (nnheader-report 'nnfolder "No such group: %s" group))
	   (t
	    (nnheader-report 'nnfolder "Selected group %s" group)
	    (nnheader-insert "211 %d %d %d %s\n" 
			     (1+ (- maxactive minactive))
			     minactive maxactive group))))))))

(defun nnfolder-request-scan (&optional group server)
  (nnmail-get-new-mail
   'nnfolder 
   (lambda ()
     (let ((bufs nnfolder-buffer-alist))
       (save-excursion
	 (while bufs
	   (if (not (buffer-name (nth 1 (car bufs))))
	       (setq nnfolder-buffer-alist 
		     (delq (car bufs) nnfolder-buffer-alist))
	     (set-buffer (nth 1 (car bufs)))
	     (and (buffer-modified-p) (save-buffer)))
	   (setq bufs (cdr bufs))))))
   nnfolder-directory
   group))

;; Don't close the buffer if we're not shutting down the server.  This way,
;; we can keep the buffer in the group buffer cache, and not have to grovel
;; over the buffer again unless we add new mail to it or modify it in some
;; way.

(defun nnfolder-close-group (group &optional server force)
  ;; Make sure we _had_ the group open.
  (when (or (assoc group nnfolder-buffer-alist)
	    (equal group nnfolder-current-group))
    (nnfolder-possibly-change-group group server)
    (save-excursion
      (set-buffer nnfolder-current-buffer)
      ;; If the buffer was modified, write the file out now.
      (and (buffer-modified-p) (save-buffer))
      ;; If we're shutting the server down, we need to kill the
      ;; buffer and remove it from the open buffer list.  Or, of
      ;; course, if we're trying to minimize our space impact.
      (kill-buffer (current-buffer))
      (setq nnfolder-buffer-alist (delq (assoc group nnfolder-buffer-alist)
					nnfolder-buffer-alist))))
  (setq nnfolder-current-group nil
	nnfolder-current-buffer nil)
  t)

(defun nnfolder-request-create-group (group &optional server) 
  (nnmail-activate 'nnfolder)
  (unless (assoc group nnfolder-group-alist)
    (push (list group (cons 1 0)) nnfolder-group-alist)
    (nnmail-save-active nnfolder-group-alist nnfolder-active-file))
  t)

(defun nnfolder-request-list (&optional server)
  (save-excursion
    (nnmail-find-file nnfolder-active-file)
    (setq nnfolder-group-alist (nnmail-get-active))))

(defun nnfolder-request-newgroups (date &optional server)
  (nnfolder-request-list server))

(defun nnfolder-request-list-newsgroups (&optional server)
  (save-excursion
    (nnmail-find-file nnfolder-newsgroups-file)))

(defun nnfolder-request-expire-articles 
  (articles newsgroup &optional server force)
  (nnfolder-possibly-change-group newsgroup server)
  (let* ((is-old t)
	 rest)
    (nnmail-activate 'nnfolder)

    (save-excursion 
      (set-buffer nnfolder-current-buffer)
      (while (and articles is-old)
	(goto-char (point-min))
	(if (search-forward (nnfolder-article-string (car articles)) nil t)
	    (if (setq is-old
		      (nnmail-expired-article-p 
		       newsgroup
		       (buffer-substring 
			(point) (progn (end-of-line) (point))) 
		       force nnfolder-inhibit-expiry))
		(progn
		  (nnheader-message 5 "Deleting article %d..." 
				    (car articles) newsgroup)
		  (nnfolder-delete-mail))
	      (setq rest (cons (car articles) rest))))
	(setq articles (cdr articles)))
      (and (buffer-modified-p) (save-buffer))
      ;; Find the lowest active article in this group.
      (let* ((active (cadr (assoc newsgroup nnfolder-group-alist)))
	     (marker (concat "\n" nnfolder-article-marker))
	     (number "[0-9]+")
	     (activemin (cdr active)))
	(goto-char (point-min))
	(while (and (search-forward marker nil t)
		    (re-search-forward number nil t))
	  (setq activemin (min activemin
			       (string-to-number (buffer-substring
						  (match-beginning 0)
						  (match-end 0))))))
	(setcar active activemin))
      (nnmail-save-active nnfolder-group-alist nnfolder-active-file)
      (nconc rest articles))))

(defun nnfolder-request-move-article
  (article group server accept-form &optional last)
  (nnfolder-possibly-change-group group server)
  (let ((buf (get-buffer-create " *nnfolder move*"))
	result)
    (and 
     (nnfolder-request-article article group server)
     (save-excursion
       (set-buffer buf)
       (buffer-disable-undo (current-buffer))
       (erase-buffer)
       (insert-buffer-substring nntp-server-buffer)
       (goto-char (point-min))
       (while (re-search-forward 
	       (concat "^" nnfolder-article-marker)
	       (save-excursion (search-forward "\n\n" nil t) (point)) t)
	 (delete-region (progn (beginning-of-line) (point))
			(progn (forward-line 1) (point))))
       (setq result (eval accept-form))
       (kill-buffer buf)
       result)
     (save-excursion
       (nnfolder-possibly-change-group group server)
       (set-buffer nnfolder-current-buffer)
       (goto-char (point-min))
       (if (search-forward (nnfolder-article-string article) nil t)
	   (nnfolder-delete-mail))
       (and last 
	    (buffer-modified-p)
	    (save-buffer))))
    result))

(defun nnfolder-request-accept-article (group &optional last)
  (and (stringp group) (nnfolder-possibly-change-group group))
  (let ((buf (current-buffer))
	result)
    (goto-char (point-min))
    (when (looking-at "X-From-Line: ")
      (replace-match "From "))
    (and 
     (nnfolder-request-list)
     (save-excursion
       (set-buffer buf)
       (goto-char (point-min))
       (search-forward "\n\n" nil t)
       (forward-line -1)
       (while (re-search-backward (concat "^" nnfolder-article-marker) nil t)
	 (delete-region (point) (progn (forward-line 1) (point))))
       (setq result (car (nnfolder-save-mail (and (stringp group) group)))))
     (save-excursion
       (set-buffer nnfolder-current-buffer)
       (and last (buffer-modified-p) (save-buffer))))
    (nnmail-save-active nnfolder-group-alist nnfolder-active-file)
    result))

(defun nnfolder-request-replace-article (article group buffer)
  (nnfolder-possibly-change-group group)
  (save-excursion
    (set-buffer nnfolder-current-buffer)
    (goto-char (point-min))
    (if (not (search-forward (nnfolder-article-string article) nil t))
	nil
      (nnfolder-delete-mail t t)
      (insert-buffer-substring buffer)
      (and (buffer-modified-p) (save-buffer))
      t)))

(defun nnfolder-request-delete-group (group &optional force server)
  (nnfolder-close-group group server t)
  ;; Delete all articles in GROUP.
  (if (not force)
      ()				; Don't delete the articles.
    ;; Delete the file that holds the group.
    (condition-case nil
	(delete-file (concat (file-name-as-directory nnfolder-directory)
			     group))
      (error nil)))
  ;; Remove the group from all structures.
  (setq nnfolder-group-alist 
	(delq (assoc group nnfolder-group-alist) nnfolder-group-alist)
	nnfolder-current-group nil
	nnfolder-current-buffer nil)
  ;; Save the active file.
  (nnmail-save-active nnfolder-group-alist nnfolder-active-file)
  t)

(defun nnfolder-request-rename-group (group new-name &optional server)
  (nnfolder-possibly-change-group group server)
  (save-excursion
    (set-buffer nnfolder-current-buffer)
    (and (file-writable-p buffer-file-name)
	 (condition-case ()
	     (progn
	       (rename-file buffer-file-name
			    (concat (file-name-as-directory nnfolder-directory)
				    new-name))
	       t)
	   (error nil))
	 ;; That went ok, so we change the internal structures.
	 (let ((entry (assoc group nnfolder-group-alist)))
	   (and entry (setcar entry new-name))
	   (setq nnfolder-current-buffer nil
		 nnfolder-current-group nil)
	   ;; Save the new group alist.
	   (nnmail-save-active nnfolder-group-alist nnfolder-active-file)
	   ;; We kill the buffer instead of renaming it and stuff.
	   (kill-buffer (current-buffer))
	   t))))


;;; Internal functions.

(defun nnfolder-article-string (article)
  (if (numberp article)
      (concat "\n" nnfolder-article-marker (int-to-string article) " ")
    (concat "\nMessage-ID: " article)))

(defun nnfolder-delete-mail (&optional force leave-delim)
  ;; Beginning of the article.
  (save-excursion
    (save-restriction
      (narrow-to-region
       (save-excursion
	 (re-search-backward (concat "^" rmail-unix-mail-delimiter) nil t)
	 (if leave-delim (progn (forward-line 1) (point))
	   (match-beginning 0)))
       (progn
	 (forward-line 1)
	 (or (and (re-search-forward (concat "^" rmail-unix-mail-delimiter) 
				     nil t)
		  (if (and (not (bobp)) leave-delim)
		      (progn (forward-line -2) (point))
		    (match-beginning 0)))
	     (point-max))))
      (delete-region (point-min) (point-max)))))

(defun nnfolder-possibly-change-group (group &optional server)
  (when (and server
	     (not (nnfolder-server-opened server)))
    (nnfolder-open-server server))
  (unless (file-exists-p nnfolder-directory)
    (make-directory (directory-file-name nnfolder-directory) t))
  (nnfolder-possibly-activate-groups nil)
  (or (assoc group nnfolder-group-alist)
      (not (file-exists-p (concat (file-name-as-directory nnfolder-directory)
				  group)))
      (progn
	(setq nnfolder-group-alist 
	      (cons (list group (cons 1 0)) nnfolder-group-alist))
	(nnmail-save-active nnfolder-group-alist nnfolder-active-file)))
  (let (inf file)
    (if (and (equal group nnfolder-current-group)
	     nnfolder-current-buffer
	     (buffer-name nnfolder-current-buffer))
	()
      (setq nnfolder-current-group group)

      ;; If we have to change groups, see if we don't already have the mbox
      ;; in memory.  If we do, verify the modtime and destroy the mbox if
      ;; needed so we can rescan it.
      (if (setq inf (assoc group nnfolder-buffer-alist))
	  (setq nnfolder-current-buffer (nth 1 inf)))

      ;; If the buffer is not live, make sure it isn't in the alist.  If it
      ;; is live, verify that nobody else has touched the file since last
      ;; time.
      (if (or (not (and nnfolder-current-buffer
			(buffer-name nnfolder-current-buffer)))
	      (not (and (bufferp nnfolder-current-buffer)
			(verify-visited-file-modtime 
			 nnfolder-current-buffer))))
	  (progn
	    (if (and nnfolder-current-buffer
		     (buffer-name nnfolder-current-buffer)
		     (bufferp nnfolder-current-buffer))
		(kill-buffer nnfolder-current-buffer))
	    (setq nnfolder-buffer-alist (delq inf nnfolder-buffer-alist))
	    (setq inf nil)))
      
      (if inf
	  ()
	(save-excursion
	  (setq file (concat (file-name-as-directory nnfolder-directory)
			     group))
	  (if (file-directory-p (file-truename file))
	      ()
	    (if (not (file-exists-p file))
		(write-region 1 1 file t 'nomesg))
	    (setq nnfolder-current-buffer 
		  (set-buffer (nnfolder-read-folder file)))
	    (setq nnfolder-buffer-alist (cons (list group (current-buffer))
					      nnfolder-buffer-alist)))))))
  (setq nnfolder-current-group group))

(defun nnfolder-save-mail (&optional group)
  "Called narrowed to an article."
  (let* ((nnmail-split-methods 
	  (if group (list (list group "")) nnmail-split-methods))
	 (group-art-list
	  (nreverse (nnmail-article-group 'nnfolder-active-number)))
	 (delim (concat "^" rmail-unix-mail-delimiter))
	 save-list group-art)
    (goto-char (point-min))
    ;; This might come from somewhere else.
    (unless (looking-at delim)
      (insert "From nobody " (current-time-string) "\n")
      (goto-char (point-min)))
    ;; Quote all "From " lines in the article.
    (forward-line 1)
    (while (re-search-forward delim nil t)
      (beginning-of-line)
      (insert "> "))
    (setq save-list group-art-list)
    (nnmail-insert-lines)
    (nnmail-insert-xref group-art-list)
    (run-hooks 'nnfolder-prepare-save-mail-hook)

    ;; Insert the mail into each of the destination groups.
    (while group-art-list
      (setq group-art (car group-art-list)
	    group-art-list (cdr group-art-list))

      ;; Kill the previous newsgroup markers.
      (goto-char (point-min))
      (search-forward "\n\n" nil t)
      (forward-line -1)
      (while (search-backward (concat "\n" nnfolder-article-marker) nil t)
	(delete-region (1+ (point)) (progn (forward-line 2) (point))))

      ;; Insert the new newsgroup marker.
      (nnfolder-possibly-change-group (car group-art))
      (nnfolder-insert-newsgroup-line group-art)
      (let ((beg (point-min))
	    (end (point-max))
	    (obuf (current-buffer)))
	(set-buffer nnfolder-current-buffer)
	(goto-char (point-max))
	(insert-buffer-substring obuf beg end)
	(set-buffer obuf)))

    ;; Did we save it anywhere?
    save-list))

(defun nnfolder-insert-newsgroup-line (group-art)
  (save-excursion
    (goto-char (point-min))
    (if (search-forward "\n\n" nil t)
	(progn
	  (forward-char -1)
	  (insert (format (concat nnfolder-article-marker "%d   %s\n")
			  (cdr group-art) (current-time-string)))))))

(defun nnfolder-possibly-activate-groups (&optional group)
  (save-excursion
    ;; If we're looking for the activation of a specific group, find out
    ;; its real name and switch to it.
    (if group (nnfolder-possibly-change-group group))
    ;; If the group alist isn't active, activate it now.
    (nnmail-activate 'nnfolder)))

(defun nnfolder-active-number (group)
  (save-excursion 
    ;; Find the next article number in GROUP.
    (prog1
	(let ((active (cadr (assoc group nnfolder-group-alist))))
	  (if active
	      (setcdr active (1+ (cdr active)))
	    ;; This group is new, so we create a new entry for it.
	    ;; This might be a bit naughty... creating groups on the drop of
	    ;; a hat, but I don't know...
	    (setq nnfolder-group-alist 
		  (cons (list group (setq active (cons 1 1)))
			nnfolder-group-alist)))
	  (cdr active))
      (nnmail-save-active nnfolder-group-alist nnfolder-active-file)
      (nnfolder-possibly-activate-groups group))))


;; This method has a problem if you've accidentally let the active list get
;; out of sync with the files.  This could happen, say, if you've
;; accidentally gotten new mail with something other than Gnus (but why
;; would _that_ ever happen? :-).  In that case, we will be in the middle of
;; processing the file, ready to add new X-Gnus article number markers, and
;; we'll run across a message with no ID yet - the active list _may_not_ be
;; ready for us yet.

;; To handle this, I'm modifying this routine to maintain the maximum ID seen
;; so far, and when we hit a message with no ID, we will _manually_ scan the
;; rest of the message looking for any more, possibly higher IDs.  We'll
;; assume the maximum that we find is the highest active.  Note that this
;; shouldn't cost us much extra time at all, but will be a lot less
;; vulnerable to glitches between the mbox and the active file.

(defun nnfolder-read-folder (file)
  (save-excursion
    (nnfolder-possibly-activate-groups nil)
    ;; We should be paranoid here and make sure the group is in the alist,
    ;; and add it if it isn't.
    ;;(if (not (assoc nnfoler-current-group nnfolder-group-alist)
    (set-buffer (setq nnfolder-current-buffer 
		      (nnheader-find-file-noselect file nil 'raw)))
    (buffer-disable-undo (current-buffer))
    (let ((delim (concat "^" rmail-unix-mail-delimiter))
	  (marker (concat "\n" nnfolder-article-marker))
	  (number "[0-9]+")
	  (active (cadr (assoc nnfolder-current-group 
				   nnfolder-group-alist)))
	  activenumber activemin start end)
      (goto-char (point-min))
      ;;
      ;; Anytime the active number is 1 or 0, it is suspect.  In that case,
      ;; search the file manually to find the active number.  Or, of course,
      ;; if we're being paranoid.  (This would also be the place to build
      ;; other lists from the header markers, such as expunge lists, etc., if
      ;; we ever desired to abandon the active file entirely for mboxes.)
      (setq activenumber (cdr active))
      (if (or nnfolder-ignore-active-file
	      (< activenumber 2))
	  (progn
	    (setq activemin (max (1- (lsh 1 23)) 
				 (1- (lsh 1 24)) 
				 (1- (lsh 1 25))))
	    (while (and (search-forward marker nil t)
			(re-search-forward number nil t))
	      (let ((newnum (string-to-number (buffer-substring
					       (match-beginning 0)
					       (match-end 0)))))
		(setq activenumber (max activenumber newnum))
		(setq activemin (min activemin newnum))))
	    (setcar active (max 1 (min activemin activenumber)))
	    (setcdr active (max activenumber (cdr active)))
	    (goto-char (point-min))))

      ;; Keep track of the active number on our own, and insert it back into
      ;; the active list when we're done. Also, prime the pump to cut down on
      ;; the number of searches we do.
      (setq end (point-marker))
      (set-marker end (or (and (re-search-forward delim nil t)
			       (match-beginning 0))
			  (point-max)))
      (while (not (= end (point-max)))
	(setq start (marker-position end))
	(goto-char end)
	;; There may be more than one "From " line, so we skip past
	;; them.  
	(while (looking-at delim) 
	  (forward-line 1))
	(set-marker end (or (and (re-search-forward delim nil t)
				 (match-beginning 0))
			    (point-max)))
	(goto-char start)
	(if (not (search-forward marker end t))
	    (progn
	      (narrow-to-region start end)
	      (nnmail-insert-lines)
	      (nnfolder-insert-newsgroup-line
	       (cons nil (nnfolder-active-number nnfolder-current-group)))
	      (widen))))

      ;; Make absolutely sure that the active list reflects reality!
      (nnmail-save-active nnfolder-group-alist nnfolder-active-file)
      (current-buffer))))

;;;###autoload
(defun nnfolder-generate-active-file ()
  "Look for mbox folders in the nnfolder directory and make them into groups."
  (interactive)
  (nnmail-activate 'nnfolder)
  (let ((files (directory-files nnfolder-directory))
	file group)
    (while (setq file (pop files))
      (when (nnheader-mail-file-mbox-p file)
	(nnheader-message 5 "Adding group %s..." file)
	(push (list file (cons 1 0)) nnfolder-group-alist)
	(nnfolder-read-folder file)
	(nnfolder-close-group file))
      (message ""))))

(provide 'nnfolder)

;;; nnfolder.el ends here
