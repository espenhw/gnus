;;; nnspool.el --- spool access for GNU Emacs

;; Copyright (C) 1988, 89, 90, 93, 94, 95 Free Software Foundation, Inc.

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
;; data in a similar format. Below is and overview of what functions
;; these packages must supply and what result they should return.
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
;; `choke-request-article ARTICLE &optional GROUP SERVER'
;; Return ARTICLE, which is either an article number or id.
;;
;; `choke-request-list SERVER'
;; Return a list of all active newsgroups on SERVER.
;;
;; `choke-request-list-newsgroups SERVER'
;; Return a list of descriptions of all newsgroups on SERVER.
;;
;; `choke-request-post-buffer METHOD HEADER ARTICLE-BUFFER GROUP INFO'
;; Should return a buffer that is suitable for "posting". nnspool and
;; nntp return a `*post-buffer*', and nnmail return a `*mail*'
;; buffer. This function should fill out the appropriate header
;; fields. 
;;
;; `choke-request-post &optional SERVER'
;; Function that will be called from a buffer to be posted. 
;;
;; `choke-open-server SERVER &optional ARGUMENT'
;; Open a connection to SERVER.
;;
;; `choke-close-server &optional SERVER'
;; Close the connection to server.
;;
;; `choke-server-opened &optional SERVER'
;; Whether the server is opened or not.
;;
;; `choke-server-status &optional SERVER'
;; Should return a status string (not in nntp buffer, but as the
;; result of the function).
;;
;; `choke-request-expire-articles ARTICLES &optional NEWSGROUP SERVER'
;; Should expire (according to some aging scheme) all ARTICLES. Most
;; backends will not be able to expire articles. Should return a list
;; of all articles that were not expired.
;;
;; All these functions must return nil if they couldn't service the
;; request. If the optional arguments are not supplied, some "current"
;; or "default" values should be used. In short, one should emulate an
;; NNTP server, in a way. All results should be returned in the NNTP
;; format. (See RFC977).

;;; Code:

(require 'nnheader)
(require 'nntp)

(defvar nnspool-inews-program news-inews-program
  "*Program to post news.")

(defvar nnspool-inews-switches '("-h")
  "*Switches for nnspool-request-post to pass to `inews' for posting news.")

(defvar nnspool-spool-directory news-path
  "*Local news spool directory.")

(defvar nnspool-active-file "/usr/lib/news/active"
  "*Local news active file.")

(defvar nnspool-newsgroups-file "/usr/lib/news/newsgroups"
  "*Local news newsgroups file.")

(defvar nnspool-distributions-file "/usr/lib/news/distributions"
  "*Local news distributions file.")

(defvar nnspool-history-file "/usr/lib/news/history"
  "*Local news history file.")

(defvar nnspool-large-newsgroup 50
  "*The number of the articles which indicates a large newsgroup.
If the number of the articles is greater than the value, verbose
messages will be shown to indicate the current status.")



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
      (while sequence
	(setq article (car sequence))
	(setq file
	      (concat nnspool-current-directory (prin1-to-string article)))
	(if (file-exists-p file)
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

      (if do-message (message "NNSPOOL: Receiving headers... done"))

      ;; Fold continuation lines.
      (goto-char 1)
      (while (re-search-forward "\\(\r?\n[ \t]+\\)+" nil t)
	(replace-match " " t t))
      'headers)))

(defun nnspool-open-server (host &optional service)
  "Open local spool."
  (setq nnspool-status-string "")
  (cond ((and (file-directory-p nnspool-spool-directory)
	      (file-exists-p nnspool-active-file))
	 (nnspool-open-server-internal host service))
	(t
	 (setq nnspool-status-string
	       (format "NNSPOOL: cannot talk to %s." host))
	 nil)))

(defun nnspool-close-server (&optional server)
  "Close news server."
  (nnspool-close-server-internal))

(fset 'nnspool-request-quit (symbol-function 'nnspool-close-server))

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


;;; Low-Level Interface.

(defun nnspool-open-server-internal (host &optional service)
  "Open connection to news server on HOST by SERVICE (default is nntp)."
  (save-excursion
    ;; Initialize communication buffer.
    (setq nntp-server-buffer (get-buffer-create " *nntpd*"))
    (set-buffer nntp-server-buffer)
    (buffer-disable-undo (current-buffer))
    (erase-buffer)
    (kill-all-local-variables)
    (setq case-fold-search t)		;Should ignore case.
    t))

(defun nnspool-close-server-internal ()
  "Close connection to news server."
  (if (get-file-buffer nnspool-history-file)
      (kill-buffer (get-file-buffer nnspool-history-file))))

(defun nnspool-find-article-by-message-id (id)
  "Return full pathname of an article identified by message-ID."
  (save-excursion
    (let ((buffer (get-file-buffer nnspool-history-file)))
      (if buffer
	  (set-buffer buffer)
	;; Finding history file may take lots of time.
	(message "Reading history file...")
	(set-buffer (find-file-noselect nnspool-history-file))
	(message "Reading history file... done")))
    ;; Search from end of the file. I think this is much faster than
    ;; do from the beginning of the file.
    (goto-char (point-max))
    (if (re-search-backward
	 (concat "^" (regexp-quote id)
		 "[ \t].*[ \t]\\([^ \t/]+\\)/\\([0-9]+\\)[ \t]*$") nil t)
	(let ((group (buffer-substring (match-beginning 1) (match-end 1)))
	      (number (buffer-substring (match-beginning 2) (match-end 2))))
	  (concat (nnspool-article-pathname
		   (nnspool-replace-chars-in-string group ?. ?/))
		  number)))))

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
