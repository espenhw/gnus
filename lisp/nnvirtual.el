;;;; nnvirtual.el --- Virtual newsgroups access for (ding) Gnus
;; Copyright (C) 1994,95 Free Software Foundation, Inc.

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

;; The other access methods (nntp, nnspool, etc) are general news
;; access methods. This module relies on Gnus and can not be used
;; separately.

;;; Code:

(require 'nntp)
(require 'nnheader)
(require 'gnus)

(defconst nnvirtual-version "nnvirtual 0.0"
  "Version numbers of this version of nnvirual.")

(defvar nnvirtual-large-newsgroup 50
  "The number of the articles which indicates a large newsgroup.
If the number of the articles is greater than the value, verbose
messages will be shown to indicate the current status.")



(defvar nnvirtual-group-alist nil)
(defvar nnvirtual-current-group nil)
(defvar nnvirtual-current-groups nil)
(defvar nnvirtual-current-mapping nil)

(defvar nnvirtual-do-not-open nil)

(defvar nnvirtual-status-string "")



;;; Interface functions.

(defun nnvirtual-retrieve-headers (sequence &optional newsgroup server)
  "Retrieve the headers for the articles in SEQUENCE."
  (nnvirtual-possibly-change-newsgroups newsgroup server)
  (save-excursion
    (set-buffer (get-buffer-create "*virtual headers*"))
    (erase-buffer)
    (let ((number (length sequence))
	  (count 0)
	  (gnus-nov-is-evil t)
	  (i 0)
	  prev articles group-articles beg art-info article group)
      (if sequence (setq prev (car (aref nnvirtual-current-mapping 
					 (car sequence)))))
      (while sequence
	(setq art-info (aref nnvirtual-current-mapping (car sequence)))
	(if (not (equal prev (car art-info)))
	    (progn
	      (setq group-articles (cons (list prev (nreverse articles)) 
					 group-articles))
	      (setq articles nil)
	      (setq prev (car art-info))))
	(setq articles (cons (cdr art-info) articles))
	(setq sequence (cdr sequence)))
      (if prev
	  (setq group-articles (cons (list prev (nreverse articles)) 
				     group-articles)))
      (setq group-articles (nreverse group-articles))
      (while group-articles
	(setq group (car (car group-articles)))
	(gnus-retrieve-headers (car (cdr (car group-articles))) group)
	(save-excursion
	  (set-buffer nntp-server-buffer)
	  (goto-char 1)
	  (insert "\n.\n")
	  (goto-char 1)
	  (while (search-forward "\n.\n" nil t)
	    (if (not (looking-at ".[0-9]+ \\([0-9]+\\) "))
		()
	      (setq article (string-to-int (gnus-buffer-substring 1 1)))
	      (setq i 1)
	      (while (/= article (cdr (aref nnvirtual-current-mapping i)))
		(setq i (1+ i)))
	      (goto-char (match-beginning 1))
	      (looking-at "[0-9]+ ")
	      (replace-match (format "%d " i))
	      (setq beg (point))
	      (search-forward "\n.\n" nil t)
	      (if (not (re-search-backward "^Xref: " beg t))
		  (progn
		    (forward-char -2)
		    (insert (format "Xref: %s %s:%d\n" (system-name) 
				    group article))
		    (forward-char -1)))
	      )))
	(goto-char (point-max))
	(insert-buffer-substring nntp-server-buffer 4)
	(setq group-articles (cdr group-articles)))
      ;; The headers are ready for reading, so they are inserted into
      ;; the nntp-server-buffer, which is where Gnus expects to find
      ;; them.
      (prog1
	  (save-excursion
	    (if (not nntp-server-buffer)
		(setq nntp-server-buffer (get-buffer-create " *nntpd*")))
	    (set-buffer nntp-server-buffer)
	    (erase-buffer)
	    (insert-buffer-substring "*virtual headers*")
	    'headers)
	(kill-buffer (current-buffer))))))

(defun nnvirtual-open-server (newsgroups &optional something)
  "Open a virtual newsgroup that contains NEWSGROUPS."
  (nnvirtual-open-server-internal))

(defun nnvirtual-close-server (&rest dum)
  "Close news server."
  (nnvirtual-close-server-internal))

(fset 'nnvirtual-request-quit (symbol-function 'nnvirtual-close-server))

(defun nnvirtual-server-opened (&optional server)
  "Return server process status, T or NIL.
If the stream is opened, return T, otherwise return NIL."
  (and nntp-server-buffer
       (get-buffer nntp-server-buffer)))

(defun nnvirtual-status-message ()
  "Return server status response as string."
  nnvirtual-status-string)

(defun nnvirtual-request-article (id &optional newsgroup server buffer)
  "Select article by message ID (or number)."
  (nnvirtual-possibly-change-newsgroups newsgroup server)
  (let (art)
    (setq art (aref nnvirtual-current-mapping id))
    (gnus-request-group (car art))
    (gnus-request-article (cdr art) (car art) buffer)))

(defun nnvirtual-request-group (group &optional server dont-check)
  "Make GROUP the current newsgroup."
  (nnvirtual-possibly-change-newsgroups group server dont-check)
  (let ((total (length nnvirtual-current-mapping)))
    (save-excursion
      (set-buffer nntp-server-buffer)
      (erase-buffer)
      (insert (format "211 %d %d %d %s\n" total 1 (1- total) group)))
    t))

(defun nnvirtual-close-group (group &optional server)
  (nnvirtual-possibly-change-newsgroups group server)
  (nnvirtual-update-marked)
  (setq nnvirtual-current-group nil)
  (setq nnvirtual-current-groups nil)
  (setq nnvirtual-current-mapping nil)
  (let ((inf (member group nnvirtual-group-alist)))
    (setq nnvirtual-group-alist (delq inf nnvirtual-group-alist))))

(defun nnvirtual-request-list (&optional server) 
  (setq nnvirtual-status-string "nnvirtual: LIST is not implemented.")
  nil)

(defun nnvirtual-request-newgroups (date &optional server)
  "List new groups."
  (setq nnvirtual-status-string "NEWGROUPS is not supported.")
  nil)

(defun nnvirtual-request-list-newsgroups (&optional server)
  (setq nnvirtual-status-string "nnvirtual: LIST NEWSGROUPS is not implemented.")
  nil)

(fset 'nnvirtual-request-post 'nntp-request-post)

(fset 'nnvirtual-request-post-buffer 'nntp-request-post-buffer)


;;; Low-level functions.

(defun nnvirtual-open-server-internal ()
  "Fix some internal variables."
  (save-excursion
    ;; Initialize communication buffer.
    (setq nntp-server-buffer (get-buffer-create " *nntpd*"))
    (set-buffer nntp-server-buffer)
    (buffer-disable-undo (current-buffer))
    (kill-all-local-variables)
    (setq case-fold-search t)))

(defun nnvirtual-close-server-internal (&rest dum)
  "Close connection to news server."
  nil)

(defun nnvirtual-possibly-change-newsgroups (group regexp &optional dont-check)
  (let (inf)
    (or (not group)
	(and nnvirtual-current-group
	     (string= group nnvirtual-current-group))
	(and (setq inf (member group nnvirtual-group-alist))
	     (string= (nth 3 inf) regexp)
	     (progn
	       (setq nnvirtual-current-group (car inf))
	       (setq nnvirtual-current-groups (nth 1 inf))
	       (setq nnvirtual-current-mapping (nth 2 inf)))))
    (if (or (not dont-check) (not inf))
	(progn
	  (and inf (setq nnvirtual-group-alist 
			 (delq inf nnvirtual-group-alist)))
	  (setq nnvirtual-current-mapping nil)
	  (setq nnvirtual-current-group group)
	  (let ((newsrc gnus-newsrc-assoc))
	    (setq nnvirtual-current-groups nil)
	    (while newsrc
	      (and (string-match regexp (car (car newsrc)))
		   (setq nnvirtual-current-groups
			 (cons (car (car newsrc)) nnvirtual-current-groups)))
	      (setq newsrc (cdr newsrc))))
	  (if nnvirtual-current-groups
	      (progn
		(nnvirtual-create-mapping group)
		(setq nnvirtual-group-alist
		      (cons (list group nnvirtual-current-groups 
				  nnvirtual-current-mapping regexp)
			    nnvirtual-group-alist)))
	    (setq nnvirtual-status-string 
		  (format 
		   "nnvirtual: No newsgroups for this virtual newsgroup"))))))
  nnvirtual-current-groups)

(defun nnvirtual-create-mapping (group)
  (let* ((group (gnus-group-prefixed-name group (list 'nnvirtual "")))
	 (info (nth 2 (gnus-gethash group gnus-newsrc-hashtb)))
	 (groups nnvirtual-current-groups)
	 (i 1)
	 (total 0)
	 unread igroup)
    ;; The virtual group doesn't exist. (?)
    (or info (error "No such group: %s" group))
    ;; Set the list of read articles to nil.
    (setcar (nthcdr 2 info) nil)
    (while groups
      ;; Added by Sudish Joseph <joseph@cis.ohio-state.edu>.
      (setq igroup (car groups))
      (let ((info (nth 2 (gnus-gethash igroup gnus-newsrc-hashtb)))
	    (active (gnus-gethash igroup gnus-active-hashtb)))
	;; see if the group has had its active list read this session
	;; if not, we do it now
	(if (null active)
	    (if (gnus-activate-newsgroup igroup)
		(gnus-get-unread-articles-in-group
		 info (gnus-gethash igroup gnus-active-hashtb))
	      (message "Couldn't request newsgroup %s" group)
	      (ding))))
      (setq unread (car (gnus-gethash (car groups) gnus-newsrc-hashtb)))
      (setq total (+ total unread))
      (setq groups (cdr groups)))
    ;; We create a mapping from nnvirtual article numbers (starting at
    ;; 1) to the actual groups numbers.
    (setq nnvirtual-current-mapping (make-vector (1+ total) nil))
    (let ((groups nnvirtual-current-groups)
	  (marks '(tick dormant reply expire))
	  tick dormant reply expire marked)
      (while groups
	(setq igroup (car groups))
	(setq marked (nth 3 (nth 2 (gnus-gethash igroup gnus-newsrc-hashtb))))
	(setq unread (gnus-list-of-unread-articles igroup))
	(while unread
	  (aset nnvirtual-current-mapping i (cons igroup (car unread)))
	  ;; Find out if the article is marked, and enter the marks in
	  ;; the proper lists. 
	  (let ((m marks))
	    (while m
	      (and (memq (car unread) (assq (car m) marked))
		   (set (car m) (cons i (symbol-value (car m)))))
	      (setq m (cdr m))))
	  (setq i (1+ i))
	  (setq unread (cdr unread)))
	(setq groups (cdr groups)))
      ;; Put the list of marked articles in the info of the virtual group.
      (let ((m marks)
	    marked)
	(while m
	  (and (symbol-value (car m))
	       (setq marked (cons (cons (car m) (symbol-value (car m)))
				  marked)))
	  (setq m (cdr m)))
	(if (nthcdr 3 info)
	    (setcar (nthcdr 3 info) marked)
	  (setcdr (nthcdr 2 info) (list marked)))))))

(defun nnvirtual-update-marked ()
  (let ((mark-lists '((gnus-newsgroup-marked . tick)
		      (gnus-newsgroup-dormant . dormant)
		      (gnus-newsgroup-expirable . expire)
		      (gnus-newsgroup-replied . reply)))
	marks art-group group-alist g)
    (while mark-lists
      (setq marks (symbol-value (car (car mark-lists))))
      (while marks
	(setq art-group (aref nnvirtual-current-mapping (car marks)))
	(if (setq g (assoc (car art-group) group-alist))
	    (nconc g (list (cdr art-group)))
	  (setq group-alist (cons (list (car art-group) (cdr art-group)) 
				  group-alist)))
	(setq marks (cdr marks)))
      (while group-alist
	(gnus-add-marked-articles (car (car group-alist)) 
				  (cdr (car mark-lists))
				  (cdr (car group-alist)))
	(gnus-group-update-group (car (car group-alist)))
	(setq group-alist (cdr group-alist)))
      (setq mark-lists (cdr mark-lists)))))

(provide 'nnvirtual)

;;; nnvirtual.el ends here
