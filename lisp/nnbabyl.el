;;; nnbabyl.el --- mail mbox access for Gnus
;; Copyright (C) 1995 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@ifi.uio.no>
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

;;; Code:

(require 'nnheader)
(require 'rmail)
(require 'nnmail)

(defvar nnbabyl-mbox-file (expand-file-name "~/RMAIL")
  "The name of the mail box file in the users home directory.")

(defvar nnbabyl-active-file (expand-file-name "~/.rmail-active")
  "The name of the active file for the mail box.")

(defvar nnbabyl-get-new-mail t
  "If non-nil, nnml will check the incoming mail file and split the mail.")



(defvar nnbabyl-mail-delimiter "\^_")

(defconst nnbabyl-version "nnbabyl 0.1"
  "nnbabyl version.")

(defvar nnbabyl-current-group nil
  "Current nnbabyl news group directory.")

(defconst nnbabyl-mbox-buffer " *nnbabyl mbox buffer*")

(defvar nnbabyl-status-string "")

(defvar nnbabyl-group-alist nil)

;;; Interface functions

(defun nnbabyl-retrieve-headers (sequence &optional newsgroup server)
  "Retrieve the headers for the articles in SEQUENCE.
Newsgroup must be selected before calling this function."
  (save-excursion
    (set-buffer nntp-server-buffer)
    (erase-buffer)
    (let ((file nil)
	  (number (length sequence))
	  (count 0)
	  beg article art-string start stop)
      (nnbabyl-possibly-change-newsgroup newsgroup)
      (while sequence
	(setq article (car sequence))
	(setq art-string (nnbabyl-article-string article))
	(set-buffer nnbabyl-mbox-buffer)
	(if (or (search-forward art-string nil t)
		(progn (goto-char 1)
		       (search-forward art-string nil t)))
	    (progn
	      (setq start 
		    (save-excursion
		      (re-search-backward 
		       (concat "^" nnbabyl-mail-delimiter) nil t)
		      (while (and (not (looking-at ".+:"))
				  (zerop (forward-line 1))))
		      (point)))
	      (search-forward "\n\n" nil t)
	      (setq stop (1- (point)))
	      (set-buffer nntp-server-buffer)
	      (insert (format "221 %d Article retrieved.\n" article))
	      (setq beg (point))
	      (insert-buffer-substring nnbabyl-mbox-buffer start stop)
	      (goto-char (point-max))
	      (insert ".\n")))
	(setq sequence (cdr sequence))
	(setq count (1+ count))
	(and (numberp nnmail-large-newsgroup)
	     (> number nnmail-large-newsgroup)
	     (zerop (% count 20))
	     gnus-verbose-backends
	     (message "nnbabyl: Receiving headers... %d%%"
		      (/ (* count 100) number))))

      (and (numberp nnmail-large-newsgroup)
	   (> number nnmail-large-newsgroup)
	   gnus-verbose-backends
	   (message "nnbabyl: Receiving headers... done"))

      ;; Fold continuation lines.
      (goto-char 1)
      (while (re-search-forward "\\(\r?\n[ \t]+\\)+" nil t)
	(replace-match " " t t))
      'headers)))

(defun nnbabyl-open-server (host &optional service)
  "Open mbox backend."
  (setq nnbabyl-status-string "")
  (setq nnbabyl-group-alist nil)
  (nnheader-init-server-buffer))

(defun nnbabyl-close-server (&optional server)
  "Close news server."
  t)

(defun nnbabyl-server-opened (&optional server)
  "Return server process status, T or NIL.
If the stream is opened, return T, otherwise return NIL."
  (and nntp-server-buffer
       (get-buffer nntp-server-buffer)))

(defun nnbabyl-status-message (&optional server)
  "Return server status response as string."
  nnbabyl-status-string)

(defun nnbabyl-request-article (article &optional newsgroup server buffer)
  "Select ARTICLE by number."
  (nnbabyl-possibly-change-newsgroup newsgroup)
  (if (stringp article)
      nil
    (save-excursion
      (set-buffer nnbabyl-mbox-buffer)
      (goto-char 1)
      (if (search-forward (nnbabyl-article-string article) nil t)
	  (let (start stop)
	    (re-search-backward (concat "^" nnbabyl-mail-delimiter) nil t)
	    (while (and (not (looking-at ".+:"))
			(zerop (forward-line 1))))
	    (setq start (point))
	    (or (and (re-search-forward 
		      (concat "^" nnbabyl-mail-delimiter) nil t)
		     (forward-line -1))
		(goto-char (point-max)))
	    (setq stop (point))
	    (let ((nntp-server-buffer (or buffer nntp-server-buffer)))
	      (set-buffer nntp-server-buffer)
	      (erase-buffer)
	      (insert-buffer-substring nnbabyl-mbox-buffer start stop)
	      (goto-char (point-min))
	      (if (search-forward "\n*** EOOH ***" nil t)
		  (progn
		    (delete-region (progn (beginning-of-line) (point))
				   (or (search-forward "\n\n" nil t)
				       (point)))))
	      t))))))

(defun nnbabyl-request-group (group &optional server dont-check)
  "Select news GROUP."
  (save-excursion
    (if (nnbabyl-possibly-change-newsgroup group)
	(if dont-check
	    t
	  (nnbabyl-get-new-mail)
	  (save-excursion
	    (set-buffer nntp-server-buffer)
	    (erase-buffer)
	    (let ((active (assoc group nnbabyl-group-alist)))
	      (insert (format "211 %d %d %d %s\n" 
			      (1+ (- (cdr (car (cdr active)))
				     (car (car (cdr active)))))
			      (car (car (cdr active)))
			      (cdr (car (cdr active)))
			      (car active))))
	    t)))))

(defun nnbabyl-close-group (group &optional server)
  t)

(defun nnbabyl-request-list (&optional server)
  "List active newsgoups."
  (if server (nnbabyl-get-new-mail))
  (nnmail-find-file nnbabyl-active-file))

(defun nnbabyl-request-newgroups (date &optional server)
  "List groups created after DATE."
  (nnbabyl-request-list server))

(defun nnbabyl-request-list-newsgroups (&optional server)
  "List newsgroups (defined in NNTP2)."
  (setq nnbabyl-status-string "nnbabyl: LIST NEWSGROUPS is not implemented.")
  nil)

(defun nnbabyl-request-post (&optional server)
  "Post a new news in current buffer."
  (mail-send-and-exit nil))

(fset 'nnbabyl-request-post-buffer 'nnmail-request-post-buffer)

(defun nnbabyl-request-expire-articles (articles newsgroup &optional server force)
  "Expire all articles in the ARTICLES list in group GROUP.
The list of unexpired articles will be returned (ie. all articles that
were too fresh to be expired).
If FORCE is non-nil, the ARTICLES will be deleted without looking at
the date."
  (nnbabyl-possibly-change-newsgroup newsgroup)
  (let* ((days (or (and nnmail-expiry-wait-function
			(funcall nnmail-expiry-wait-function newsgroup))
		   nnmail-expiry-wait))
	 article rest)
    (save-excursion 
      (set-buffer nnbabyl-mbox-buffer)
      (while articles
	(goto-char 1)
	(if (search-forward (nnbabyl-article-string (car articles)) nil t)
	    (if (or force
		    (> (nnmail-days-between 
			(current-time-string)
			(buffer-substring 
			 (point) (progn (end-of-line) (point))))
		       days))
		(progn
		  (and gnus-verbose-backends
		       (message "Deleting: %s" (car articles)))
		  (nnbabyl-delete-mail))
	      (setq rest (cons (car articles) rest))))
	(setq articles (cdr articles)))
      (save-buffer)
      rest)))

(defun nnbabyl-request-move-article 
  (article group server accept-form &optional last)
  (nnbabyl-possibly-change-newsgroup group)
  (let ((buf (get-buffer-create " *nnbabyl move*"))
	result)
    (and 
     (nnbabyl-request-article article group server)
     (save-excursion
       (set-buffer buf)
       (insert-buffer-substring nntp-server-buffer)
       (goto-char (point-min))
       (if (re-search-forward 
	    "^X-Gnus-Newsgroup:" 
	    (save-excursion (search-forward "\n\n" nil t) (point)) t)
	   (delete-region (progn (beginning-of-line) (point))
			  (progn (forward-line 1) (point))))
       (setq result (eval accept-form))
       (kill-buffer (current-buffer))
       result)
     (save-excursion
       (set-buffer nnbabyl-mbox-buffer)
       (goto-char 1)
       (if (search-forward (nnbabyl-article-string article) nil t)
	   (nnbabyl-delete-mail))
       (and last (save-buffer))))
    result))

(defun nnbabyl-request-accept-article (group &optional last)
  (let ((buf (current-buffer))
	result beg)
    (and 
     (setq nnbabyl-group-alist (nnmail-get-active))
     (save-excursion
       (set-buffer nnbabyl-mbox-buffer)
       (setq beg (goto-char (point-max)))
       (insert-buffer-substring buf)
       (goto-char beg)
       (if (stringp group)
	   (progn
	     (search-forward "\n\n" nil t)
	     (forward-line -1)
	     (save-excursion
	       (while (re-search-backward "^X-Gnus-Newsgroup: " beg t)
		 (delete-region (point) (progn (forward-line 1) (point)))))
	     (setq result (nnbabyl-insert-newsgroup-line group)))
	 (setq result (nnbabyl-save-mail)))
       (and last (save-buffer))
       result)
     (nnmail-save-active nnbabyl-group-alist nnbabyl-active-file))
    result))

(defun nnbabyl-request-replace-article (article group buffer)
  (nnbabyl-possibly-change-newsgroup group)
  (save-excursion
    (set-buffer nnbabyl-mbox-buffer)
    (goto-char 1)
    (if (not (search-forward (nnbabyl-article-string article) nil t))
	nil
      (nnbabyl-delete-mail t t)
      (insert-buffer-substring buffer)
      (save-buffer)
      t)))


;;; Low-Level Interface

(defun nnbabyl-delete-mail (&optional force leave-delim)
  "If FORCE, delete article no matter how many X-Gnus-Newsgroup
headers there are. If LEAVE-DELIM, don't delete the Unix mbox
delimeter line."
  ;; Delete the current X-Gnus-Newsgroup line.
  (or force
      (delete-region
       (progn (beginning-of-line) (point))
       (progn (forward-line 1) (point))))
  ;; Beginning of the article.
  (save-excursion
    (save-restriction
      (narrow-to-region
       (save-excursion
	 (re-search-backward (concat "^" nnbabyl-mail-delimiter) nil t)
	 (if leave-delim (progn (forward-line 1) (point))
	   (match-beginning 0)))
       (progn
	 (forward-line 1)
	 (or (and (re-search-forward (concat "^" nnbabyl-mail-delimiter) 
				     nil t)
		  (if (and (not (bobp)) leave-delim)
		      (progn (forward-line -2) (point))
		    (match-beginning 0)))
	     (point-max))))
      (goto-char (point-min))
      ;; Only delete the article if no other groups owns it as well.
      (if (or force (not (re-search-forward "^X-Gnus-Newsgroup: " nil t)))
	  (delete-region (point-min) (point-max))))))

(defun nnbabyl-possibly-change-newsgroup (newsgroup)
  (if (or (not nnbabyl-mbox-buffer)
	  (not (buffer-name nnbabyl-mbox-buffer)))
      (save-excursion
	(nnbabyl-read-mbox)))
  (if (not nnbabyl-group-alist)
      (setq nnbabyl-group-alist (nnmail-get-active)))
  (if newsgroup
      (if (assoc newsgroup nnbabyl-group-alist)
	  (setq nnbabyl-current-group newsgroup)
	(setq nnbabyl-status-string "No such group in file")
	nil)))

(defun nnbabyl-article-string (article)
  (concat "\nX-Gnus-Newsgroup: " nnbabyl-current-group ":" 
	  (int-to-string article)))

(defun nnbabyl-save-mail ()
  "Called narrowed to an article."
  (let ((group-art (nreverse (nnmail-article-group 'nnbabyl-active-number))))
    (nnmail-insert-lines)
    (nnmail-insert-xref group-art)
    (nnbabyl-insert-newsgroup-line group-art)))

(defun nnbabyl-insert-newsgroup-line (group-art)
  (save-excursion
    (goto-char (point-min))
    (or (looking-at "\^_")
	(insert "\^_\^L\n0, unseen,,\n*** EOOH ***\n"))
    (while (looking-at "From ")
      (replace-match "Mail-from: ")
      (forward-line 1))
    (if (search-forward "\n\n" nil t)
	(progn
	  (forward-char -1)
	  (while group-art
	    (insert (format "X-Gnus-Newsgroup: %s:%d   %s\n" 
			    (car (car group-art)) (cdr (car group-art))
			    (current-time-string)))
	    (setq group-art (cdr group-art)))))))

(defun nnbabyl-active-number (group)
  "Find the next article number in GROUP."
  (let ((active (car (cdr (assoc group nnbabyl-group-alist)))))
    (setcdr active (1+ (cdr active)))
    (cdr active)))

(defun nnbabyl-read-mbox ()
  (nnbabyl-request-list)
  (setq nnbabyl-group-alist (nnmail-get-active))
  (if (not (file-exists-p nnbabyl-mbox-file))
      (write-region 1 1 nnbabyl-mbox-file t 'nomesg))
  (if (and nnbabyl-mbox-buffer
	   (get-buffer nnbabyl-mbox-buffer)
	   (buffer-name nnbabyl-mbox-buffer)
	   (save-excursion
	     (set-buffer nnbabyl-mbox-buffer)
	     (= (buffer-size) (nth 7 (file-attributes nnbabyl-mbox-file)))))
      ()
    (save-excursion
      (let ((delim (concat "^" nnbabyl-mail-delimiter))
	    (buf (or (get-buffer (file-name-nondirectory nnbabyl-mbox-file))
		     (create-file-buffer nnbabyl-mbox-file)))
	    start end)
	(set-buffer (setq nnbabyl-mbox-buffer buf))
	(buffer-disable-undo (current-buffer))

	(insert-file-contents nnbabyl-mbox-file)
	(setq buffer-file-name nnbabyl-mbox-file)
	(set-buffer-modified-p nil)

	(goto-char (point-min))
	(while (re-search-forward delim nil t)
	  (setq start (match-beginning 0))
	  (if (and
	       (save-excursion (re-search-forward delim nil t))
	       (not (search-forward 
		     "\nX-Gnus-Newsgroup: " 
		     (save-excursion 
		       (setq end (or (and (re-search-forward delim nil t)
					  (match-beginning 0))
				     (point-max)))) t)))
	      (progn
		(goto-char end)
		(save-excursion
		  (save-restriction
		    (goto-char start)
		    (narrow-to-region start end)
		    (nnbabyl-save-mail))))))
	(save-buffer)
	(nnmail-save-active nnbabyl-group-alist nnbabyl-active-file)))))

(defun nnbabyl-get-new-mail ()
  (let (incoming)
    (nnbabyl-read-mbox)
    (if (and nnmail-spool-file nnbabyl-get-new-mail
	     (file-exists-p nnmail-spool-file)
	     (> (nth 7 (file-attributes nnmail-spool-file)) 0))
	(progn
	  (and gnus-verbose-backends
	       (message "nnbabyl: Reading incoming mail..."))
	  (setq incoming 
		(nnmail-move-inbox nnmail-spool-file
				   (concat nnbabyl-mbox-file "-Incoming")))
	  (save-excursion
	    (let ((in-buf (nnmail-split-incoming 
			   incoming 'nnbabyl-save-mail t)))
	      (set-buffer nnbabyl-mbox-buffer)
	      (goto-char (point-max))
	      (search-backward "\n\^_" nil t)
	      (insert-buffer-substring in-buf)
	      (kill-buffer in-buf)))
	  (run-hooks 'nnmail-read-incoming-hook)
	  (and gnus-verbose-backends
	       (message "nnbabyl: Reading incoming mail...done"))))
    (and (buffer-modified-p nnbabyl-mbox-buffer) 
	 (save-excursion
	   (nnmail-save-active nnbabyl-group-alist nnbabyl-active-file)
	   (set-buffer nnbabyl-mbox-buffer)
	   (save-buffer)))
    ;; (if incoming (delete-file incoming))
    ))

(provide 'nnbabyl)

;;; nnbabyl.el ends here
