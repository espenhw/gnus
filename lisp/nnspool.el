;;; nnspool.el --- spool access for GNU Emacs
;; Copyright (C) 1988,89,90,93,94,95 Free Software Foundation, Inc.

;; Author: Masanobu UMEDA <umerin@flab.flab.fujitsu.junet>
;; 	Lars Ingebrigtsen <larsi@ifi.uio.no>
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

;; All the Gnus backends have the same interface, and should return
;; data in a similar format. Below is an overview of what functions
;; these packages must supply and what results they should return.
;;
;; Variables:
;;
;; `nntp-server-buffer' - All data should be returned to Gnus in this
;; buffer. 
;;
;; Functions for the imaginary backend `choke':
;;
;; `choke-retrieve-headers ARTICLES &optional GROUP SERVER'
;; Should return all headers for all ARTICLES, or return NOV lines for
;; the same.
;;
;; `choke-request-group GROUP &optional SERVER DISCARD'
;; Switch to GROUP. If DISCARD is nil, active information on the group
;; must be returned.
;;
;; `choke-close-group GROUP &optional SERVER'
;; Close group. Most backends won't have to do anything with this
;; call, but it is an opportunity to clean up, if that is needed. It
;; is called when Gnus exits a group.
;;
;; `choke-request-article ARTICLE &optional GROUP SERVER'
;; Return ARTICLE, which is either an article number or
;; message-id. Note that not all backends can return articles based on
;; message-id. 
;;
;; `choke-request-list SERVER'
;; Return a list of all active newsgroups on SERVER.
;;
;; `choke-request-list-newsgroups SERVER'
;; Return a list of descriptions of all newsgroups on SERVER.
;;
;; `choke-request-newgroups DATE &optional SERVER'
;; Return a list of all groups that have arrived after DATE on
;; SERVER. Note that the date doesn't have to be respected - Gnus will
;; always check whether the groups are old or not. Backends that do
;; not store date information may just return the entire list of
;; groups. 
;;
;; `choke-request-post-buffer METHOD HEADER ARTICLE-BUFFER GROUP INFO'
;; Should return a buffer that is suitable for "posting". nnspool and
;; nntp return a `*post-buffer*', and nnmail return a `*mail*'
;; buffer. This function should fill out the appropriate headers. 
;;
;; `choke-request-post &optional SERVER'
;; Function that will be called from a buffer to be posted. 
;;
;; `choke-open-server SERVER &optional ARGUMENT'
;; Open a connection to SERVER.
;;
;; `choke-close-server &optional SERVER'
;; Close the connection to SERVER.
;;
;; `choke-server-opened &optional SERVER'
;; Whether the conenction to SERVER is opened or not.
;;
;; `choke-server-status &optional SERVER'
;; Should return a status string (not in the nntp buffer, but as the
;; result of the function).
;;
;; The following functions are optional and apply only to backends
;; that are able to control the contents of their groups totally
;; (ie. mail backends.)  Backends that aren't able to do that
;; shouldn't define these functions at all. Gnus will check for their
;; presence before attempting to call them.
;;
;; `choke-request-expire-articles ARTICLES &optional NEWSGROUP SERVER'
;; Should expire (according to some aging scheme) all ARTICLES. Most
;; backends will not be able to expire articles. Should return a list
;; of all articles that were not expired.
;;
;; `choke-request-move-article ARTICLE GROUP SERVER ACCEPT-FORM'
;; Should move ARTICLE from GROUP on SERVER by using ACCEPT-FORM.
;; Removes any information it has added to the article (extra headers,
;; whatever - make it as clean as possible), and then passes the
;; article on by evaling ACCEPT-FORM, which is normally a call to the
;; function described below. If the ACCEPT-FORM returns a non-nil
;; value, the article should then be deleted.
;;
;; `choke-request-accept-article GROUP'
;; The contents of the current buffer will be put into GROUP. There
;; should, of course, be an article in the current buffer. This
;; function is normally only called by the function described above. 
;;
;; `choke-request-replace-article ARTICLE GROUP BUFFER'
;; Replace ARTICLE in GROUP with the contents of BUFFER.
;; This provides an easy interface for allowing editing of
;; articles. Note that even headers may be edited, so the backend has
;; to update any tables (nov buffers, etc) that it maintains after
;; replacing the article.
;;
;; All these functions must return nil if they couldn't service the
;; request. If the optional arguments are not supplied, some "current"
;; or "default" values should be used. In short, one should emulate an
;; NNTP server, in a way.
;;
;; If you want to write a new backend, you just have to supply the
;; functions listed above. In addition, you must enter the new backend
;; into the list of valid select methods:
;; (setq gnus-valid-select-methods 
;;       (cons '("choke" mail) gnus-valid-select-methods))
;; The first element in this list is the name of the backend. Other
;; elemnets may be `mail' (for mail groups),  `post' (for news
;; groups), `none' (neither), `respool' (for groups that can control
;; their contents). 

;;; Code:

(require 'nnheader)
(require 'nntp)

(defvar nnspool-inews-program news-inews-program
  "Program to post news.")

(defvar nnspool-inews-switches '("-h")
  "Switches for nnspool-request-post to pass to `inews' for posting news.")

(defvar nnspool-spool-directory news-path
  "Local news spool directory.")

(defvar nnspool-lib-dir "/usr/lib/news/"
  "Where the local news library files are stored.")

(defvar nnspool-active-file (concat nnspool-lib-dir "active")
  "Local news active file.")

(defvar nnspool-newsgroups-file (concat nnspool-lib-dir "newsgroups")
  "Local news newsgroups file.")

(defvar nnspool-distributions-file (concat nnspool-lib-dir "distributions")
  "Local news distributions file.")

(defvar nnspool-history-file (concat nnspool-lib-dir "history")
  "Local news history file.")

(defvar nnspool-active-times-file (concat nnspool-lib-dir "active.times")
  "Local news active date file.")

(defvar nnspool-large-newsgroup 50
  "The number of the articles which indicates a large newsgroup.
If the number of the articles is greater than the value, verbose
messages will be shown to indicate the current status.")

(defvar nnspool-nov-is-evil nil
  "Non-nil means that nnspool will never return NOV lines instead of headers.")



(defconst nnspool-version "nnspool 2.0"
  "Version numbers of this version of NNSPOOL.")

(defvar nnspool-current-directory nil
  "Current news group directory.")

(defvar nnspool-status-string "")



;;; Interface functions.

(defun nnspool-retrieve-headers (sequence &optional newsgroup server)
  "Retrieve the headers for the articles in SEQUENCE.
Newsgroup must be selected before calling this function."
  (save-excursion
    (set-buffer nntp-server-buffer)
    (erase-buffer)
    (let* ((number (length sequence))
	   (count 0)
	   (do-message (and (numberp nnspool-large-newsgroup)
			    (> number nnspool-large-newsgroup)))
	   file beg article)
      (nnspool-possibly-change-directory newsgroup)
      (if (nnspool-retrieve-headers-with-nov sequence)
	  'nov
	(while sequence
	  (setq article (car sequence))
	  (setq file (concat nnspool-current-directory 
			     (int-to-string article)))
	  (and (file-exists-p file)
	       (progn
		 (insert (format "221 %d Article retrieved.\n" article))
		 (setq beg (point))
		 (insert-file-contents file)
		 (goto-char beg)
		 (search-forward "\n\n" nil t)
		 (forward-char -1)
		 (insert ".\n")
		 (delete-region (point) (point-max))))
	  (setq sequence (cdr sequence))

	  (and do-message
	       (zerop (% (setq count (1+ count)) 20))
	       (message "NNSPOOL: Receiving headers... %d%%"
			(/ (* count 100) number))))

	(and do-message (message "NNSPOOL: Receiving headers... done"))

	;; Fold continuation lines.
	(goto-char 1)
	(while (re-search-forward "\\(\r?\n[ \t]+\\)+" nil t)
	  (replace-match " " t t))
	'headers))))

(defun nnspool-open-server (host &optional service)
  "Open local spool."
  (setq nnspool-status-string "")
  (cond ((and (file-directory-p nnspool-spool-directory)
	      (file-exists-p nnspool-active-file))
	 (nnheader-init-server-buffer))
	(t
	 (setq nnspool-status-string
	       (format "NNSPOOL: cannot talk to %s." host))
	 nil)))

(defun nnspool-close-server (&optional server)
  "Close news server."
  t)

(defun nnspool-server-opened (&optional server)
  "Return server process status, T or NIL.
If the stream is opened, return T, otherwise return NIL."
  (and nntp-server-buffer
       (get-buffer nntp-server-buffer)))

(defun nnspool-status-message ()
  "Return server status response as string."
  nnspool-status-string)

(defun nnspool-request-article (id &optional newsgroup server buffer)
  "Select article by message ID (or number)."
  (nnspool-possibly-change-directory newsgroup)
  (let ((file (if (stringp id)
		  (nnspool-find-article-by-message-id id)
		(concat nnspool-current-directory (prin1-to-string id))))
	(nntp-server-buffer (or buffer nntp-server-buffer)))
    (if (and (stringp file)
	     (file-exists-p file)
	     (not (file-directory-p file)))
	(save-excursion
	  (nnspool-find-file file)))))

(defun nnspool-request-body (id &optional newsgroup server)
  "Select article body by message ID (or number)."
  (nnspool-possibly-change-directory newsgroup)
  (if (nnspool-request-article id)
      (save-excursion
	(set-buffer nntp-server-buffer)
	(goto-char (point-min))
	(if (search-forward "\n\n" nil t)
	    (delete-region (point-min) (point)))
	t)))

(defun nnspool-request-head (id &optional newsgroup server)
  "Select article head by message ID (or number)."
  (nnspool-possibly-change-directory newsgroup)
  (if (nnspool-request-article id)
      (save-excursion
	(set-buffer nntp-server-buffer)
	(goto-char (point-min))
	(if (search-forward "\n\n" nil t)
	    (delete-region (1- (point)) (point-max)))
	t)))

(defun nnspool-request-group (group &optional server dont-check)
  "Select news GROUP."
  (let ((pathname (nnspool-article-pathname
		   (nnspool-replace-chars-in-string group ?. ?/)))
	dir)
    (if (file-directory-p pathname)
	(progn
	  (setq nnspool-current-directory pathname)
	  (if (not dont-check)
  	      (progn
 		(setq dir (directory-files pathname nil "^[0-9]+$" t))
 		;; yes, completely empty spool directories *are* possible
		;; Fix by Sudish Joseph <joseph@cis.ohio-state.edu>
 		(and dir
 		    (setq dir
 			  (sort 
 			   (mapcar
 			    (function
 			     (lambda (name)
 			       (string-to-int name)))
 			    dir)
 			   '<)))
  		(save-excursion
  		  (set-buffer nntp-server-buffer)
  		  (erase-buffer)
 		  (if dir
 		      (insert
 		       (format "211 %d %d %d %s\n" (length dir) (car dir)
 			       (progn (while (cdr dir) (setq dir (cdr dir)))
 				      (car dir))
 			       group))
 		    (insert (format "211 0 0 0 %s\n" group))))))
	  t))))

(defun nnspool-close-group (group &optional server)
  t)

(defun nnspool-request-list (&optional server)
  "List active newsgoups."
  (save-excursion
    (nnspool-find-file nnspool-active-file)))

(defun nnspool-request-list-newsgroups (&optional server)
  "List newsgroups (defined in NNTP2)."
  (save-excursion
    (nnspool-find-file nnspool-newsgroups-file)))

(defun nnspool-request-list-distributions (&optional server)
  "List distributions (defined in NNTP2)."
  (save-excursion
    (nnspool-find-file nnspool-distributions-file)))

(defun nnspool-request-newgroups (date &optional server)
  "List groups created after DATE."
  (save-excursion
    (nnspool-find-file nnspool-active-times-file)
    (setq nnspool-status-string "NEWGROUPS is not supported.")
    nil))

(defun nnspool-request-post (&optional server)
  "Post a new news in current buffer."
  (save-excursion
    ;; We have to work in the server buffer because of NEmacs hack.
    (copy-to-buffer nntp-server-buffer (point-min) (point-max))
    (set-buffer nntp-server-buffer)
    (apply (function call-process-region)
	   (point-min) (point-max)
	   nnspool-inews-program 'delete t nil nnspool-inews-switches)
    (prog1
	(or (zerop (buffer-size))
	    ;; If inews returns strings, it must be error message 
	    ;;  unless SPOOLNEWS is defined.  
	    ;; This condition is very weak, but there is no good rule 
	    ;;  identifying errors when SPOOLNEWS is defined.  
	    ;; Suggested by ohm@kaba.junet.
	    (string-match "spooled" (buffer-string)))
      ;; Make status message by unfolding lines.
      (subst-char-in-region (point-min) (point-max) ?\n ?\\ 'noundo)
      (setq nnspool-status-string (buffer-string))
      (erase-buffer))))

(fset 'nnspool-request-post-buffer 'nntp-request-post-buffer)


;;; Internal functions.

(defun nnspool-retrieve-headers-with-nov (articles)
  (if (or gnus-nov-is-evil nnspool-nov-is-evil)
      nil
    (let ((nov (concat nnspool-current-directory ".overview"))
	  article)
      (if (file-exists-p nov)
	  (save-excursion
	    (set-buffer nntp-server-buffer)
	    (erase-buffer)
	    (insert-file-contents nov)
	    ;; First we find the first wanted line. We issue a number
	    ;; of search-forwards - the first article we are lookign
	    ;; for may be expired, so we have to go on searching until
	    ;; we find one of the articles we want.
	    (while (and articles
			(setq article (concat (int-to-string 
					       (car articles) "\t")))
			(not (or (looking-at article)
				 (search-forward (concat "\n" article) 
						 nil t))))
	      (setq articles (cdr articles)))
	    (if (not articles)
		()
	      (beginning-of-line)
	      (delete-region (point-min) (point))
	      ;; Then we find the last wanted line. We go to the end
	      ;; of the buffer and search backward much the same way
	      ;; we did to find the first article.
	      ;; !!! Perhaps it would be better just to do a (last articles), 
	      ;; and go forward successively over each line and
	      ;; compare to avoid this (reverse), like this:
	      ;; (while (and (>= last (read nntp-server-buffer)))
	      ;;             (zerop (forward-line 1))))
	      (setq articles (reverse articles))
	      (goto-char (point-max))
	      (while (and articles
			  (not (search-backward 
				(concat "\n" (int-to-string (car articles))
					"\t") nil t)))
		(setq articles (cdr articles)))
	      (if articles
		  (progn
		    (forward-line 2)
		    (delete-region (point) (point-max)))))
	    (or articles (progn (erase-buffer) nil)))))))

(defun nnspool-find-article-by-message-id (id)
  "Return full pathname of an article identified by message-ID."
  (save-excursion
    (set-buffer nntp-server-buffer)
    (erase-buffer)
    (call-process "grep" nil t nil id nnspool-history-file)
    (goto-char (point-min))
    (if (looking-at "<[^>]+>[ \t]+[-0-9~]+[ \t]+\\(.*\\)$")
	(concat nnspool-spool-directory
		(nnspool-replace-chars-in-string 
		 (buffer-substring (match-beginning 1) (match-end 1)) 
		 ?. ?/)))))

(defun nnspool-find-file (file)
  "Insert FILE in server buffer safely."
  (set-buffer nntp-server-buffer)
  (erase-buffer)
  (condition-case ()
      (progn (insert-file-contents file) t)
    (file-error nil)))

(defun nnspool-possibly-change-directory (newsgroup)
  (if newsgroup
      (let ((pathname (nnspool-article-pathname
		       (nnspool-replace-chars-in-string newsgroup ?. ?/))))
	(if (file-directory-p pathname)
	    (setq nnspool-current-directory pathname)
	  (error "No such newsgroup: %s" newsgroup)))))

(defun nnspool-article-pathname (group)
  "Make pathname for GROUP."
  (concat (file-name-as-directory nnspool-spool-directory) group "/"))

(defun nnspool-replace-chars-in-string (string from to)
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

(provide 'nnspool)

;;; nnspool.el ends here
