;;; nnbabyl.el --- rmail mbox access for Gnus
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

;; For an overview of what the interface functions do, please see the
;; Gnus sources.  

;;; Code:

(require 'nnheader)
(require 'rmail)
(require 'nnmail)

(defvar nnbabyl-mbox-file (expand-file-name "~/RMAIL")
  "The name of the rmail box file in the users home directory.")

(defvar nnbabyl-active-file (expand-file-name "~/.rmail-active")
  "The name of the active file for the rmail box.")

(defvar nnbabyl-get-new-mail t
  "If non-nil, nnbabyl will check the incoming mail file and split the mail.")

(defvar nnbabyl-prepare-save-mail-hook nil
  "Hook run narrowed to an article before saving.")



(defvar nnbabyl-mail-delimiter "\^_")

(defconst nnbabyl-version "nnbabyl 1.0"
  "nnbabyl version.")

(defvar nnbabyl-mbox-buffer nil)
(defvar nnbabyl-current-group nil)
(defvar nnbabyl-status-string "")
(defvar nnbabyl-group-alist nil)
(defvar nnbabyl-active-timestamp nil)

(defvar nnbabyl-previous-buffer-mode nil)



(defvar nnbabyl-current-server nil)
(defvar nnbabyl-server-alist nil)
(defvar nnbabyl-server-variables 
  (list
   (list 'nnbabyl-mbox-file nnbabyl-mbox-file)
   (list 'nnbabyl-active-file nnbabyl-active-file)
   (list 'nnbabyl-get-new-mail nnbabyl-get-new-mail)
   '(nnbabyl-current-group nil)
   '(nnbabyl-status-string "")
   '(nnbabyl-previous-buffer-mode nil)
   '(nnbabyl-group-alist nil)))



;;; Interface functions

(defun nnbabyl-retrieve-headers (sequence &optional newsgroup server fetch-old)
  (save-excursion
    (set-buffer nntp-server-buffer)
    (erase-buffer)
    (let ((number (length sequence))
	  (count 0)
	  article art-string start stop)
      (nnbabyl-possibly-change-newsgroup newsgroup)
      (while sequence
	(setq article (car sequence))
	(setq art-string (nnbabyl-article-string article))
	(set-buffer nnbabyl-mbox-buffer)
	(if (or (search-forward art-string nil t)
		(search-backward art-string nil t))
	    (progn
	      (re-search-backward (concat "^" nnbabyl-mail-delimiter) nil t)
	      (while (and (not (looking-at ".+:"))
			  (zerop (forward-line 1))))
	      (setq start (point))
	      (search-forward "\n\n" nil t)
	      (setq stop (1- (point)))
	      (set-buffer nntp-server-buffer)
	      (insert "221 " (int-to-string article) " Article retrieved.\n")
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
	   (message "nnbabyl: Receiving headers...done"))

      ;; Fold continuation lines.
      (set-buffer nntp-server-buffer)
      (goto-char (point-min))
      (while (re-search-forward "\\(\r?\n[ \t]+\\)+" nil t)
	(replace-match " " t t))
      'headers)))

(defun nnbabyl-open-server (server &optional defs)
  (nnheader-init-server-buffer)
  (if (equal server nnbabyl-current-server)
      t
    (if nnbabyl-current-server
	(setq nnbabyl-server-alist 
	      (cons (list nnbabyl-current-server
			  (nnheader-save-variables nnbabyl-server-variables))
		    nnbabyl-server-alist)))
    (let ((state (assoc server nnbabyl-server-alist)))
      (if state 
	  (progn
	    (nnheader-restore-variables (nth 1 state))
	    (setq nnbabyl-server-alist (delq state nnbabyl-server-alist)))
	(nnheader-set-init-variables nnbabyl-server-variables defs)))
    (setq nnbabyl-current-server server)))

(defun nnbabyl-close-server (&optional server)
  ;; Restore buffer mode.
  (when (and (nnbabyl-server-opened)
	     nnbabyl-previous-buffer-mode)
    (save-excursion
      (set-buffer nnbabyl-mbox-buffer)
      (narrow-to-region
       (car (car nnbabyl-previous-buffer-mode))
       (cdr (car nnbabyl-previous-buffer-mode)))
      (funcall (cdr nnbabyl-previous-buffer-mode))))
  (setq nnbabyl-current-server nil
	nnbabyl-mbox-buffer nil)
  t)

(defun nnbabyl-server-opened (&optional server)
  (and (equal server nnbabyl-current-server)
       nnbabyl-mbox-buffer
       (buffer-name nnbabyl-mbox-buffer)
       nntp-server-buffer
       (buffer-name nntp-server-buffer)))

(defun nnbabyl-status-message (&optional server)
  nnbabyl-status-string)

(defun nnbabyl-request-article (article &optional newsgroup server buffer)
  (nnbabyl-possibly-change-newsgroup newsgroup)
  (save-excursion
    (set-buffer nnbabyl-mbox-buffer)
    (goto-char (point-min))
    (if (search-forward (nnbabyl-article-string article) nil t)
	(let (start stop summary-line)
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
	    ;; If there is an EOOH header, then we have to remove some
	    ;; duplicated headers. 
	    (setq summary-line (looking-at "Summary-line:"))
	    (when (search-forward "\n*** EOOH ***" nil t)
	      (if summary-line
		  ;; The headers to be deleted are located before the
		  ;; EOOH line...
		  (delete-region (point-min) (progn (forward-line 1)
		  (point)))
		;; ...or after.
		(delete-region (progn (beginning-of-line) (point))
			       (or (search-forward "\n\n" nil t)
				   (point)))))
	    (if (numberp article) 
		(cons nnbabyl-current-group article)
	      (nnbabyl-article-group-number)))))))

(defun nnbabyl-request-group (group &optional server dont-check)
  (save-excursion
    (if (nnbabyl-possibly-change-newsgroup group)
	(if dont-check
	    t
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

(defun nnbabyl-request-scan (&optional group server)
  (nnbabyl-read-mbox)
  (nnmail-get-new-mail 
   'nnbabyl 
   (lambda ()
     (save-excursion
       (set-buffer nnbabyl-mbox-buffer)
       (save-buffer)))
   nnbabyl-mbox-file group
   (lambda ()
     (save-excursion
       (let ((in-buf (current-buffer)))
	 (goto-char (point-min))
	 (while (search-forward "\n\^_\n" nil t)
	   (delete-char -1))
	 (set-buffer nnbabyl-mbox-buffer)
	 (goto-char (point-max))
	 (search-backward "\n\^_" nil t)
	 (goto-char (match-end 0))
	 (insert-buffer-substring in-buf))))))

(defun nnbabyl-close-group (group &optional server)
  t)

(defun nnbabyl-request-create-group (group &optional server) 
  (nnmail-activate 'nnbabyl)
  (or (assoc group nnbabyl-group-alist)
      (let (active)
	(setq nnbabyl-group-alist (cons (list group (setq active (cons 1 0)))
					nnbabyl-group-alist))
	(nnmail-save-active nnbabyl-group-alist nnbabyl-active-file)))
  t)

(defun nnbabyl-request-list (&optional server)
  (save-excursion
    (or (nnmail-find-file nnbabyl-active-file)
	(progn
	  (setq nnbabyl-group-alist (nnmail-get-active))
	  (nnmail-save-active nnbabyl-group-alist nnbabyl-active-file)
	  (nnmail-find-file nnbabyl-active-file)))))

(defun nnbabyl-request-newgroups (date &optional server)
  (nnbabyl-request-list server))

(defun nnbabyl-request-list-newsgroups (&optional server)
  (setq nnbabyl-status-string "nnbabyl: LIST NEWSGROUPS is not implemented.")
  nil)

(defun nnbabyl-request-post (&optional server)
  (mail-send-and-exit nil))

(defun nnbabyl-request-expire-articles
  (articles newsgroup &optional server force)
  (nnbabyl-possibly-change-newsgroup newsgroup)
  (let* ((days (or (and nnmail-expiry-wait-function
			(funcall nnmail-expiry-wait-function newsgroup))
		   nnmail-expiry-wait))
	 (is-old t)
	 rest)
    (nnmail-activate 'nnbabyl)

    (save-excursion 
      (set-buffer nnbabyl-mbox-buffer)
      (set-text-properties (point-min) (point-max) nil)
      (while (and articles is-old)
	(goto-char (point-min))
	(if (search-forward (nnbabyl-article-string (car articles)) nil t)
	    (if (or force
		    (setq is-old
			  (> (nnmail-days-between 
			      (current-time-string)
			      (buffer-substring 
			       (point) (progn (end-of-line) (point))))
			     days)))
		(progn
		  (and gnus-verbose-backends
		       (message "Deleting article %d in %s..." 
				(car articles) newsgroup))
		  (nnbabyl-delete-mail))
	      (setq rest (cons (car articles) rest))))
	(setq articles (cdr articles)))
      (save-buffer)
      ;; Find the lowest active article in this group.
      (let ((active (nth 1 (assoc newsgroup nnbabyl-group-alist))))
	(goto-char (point-min))
	(while (and (not (search-forward
			  (nnbabyl-article-string (car active)) nil t))
		    (<= (car active) (cdr active)))
	  (setcar active (1+ (car active)))
	  (goto-char (point-min))))
      (nnmail-save-active nnbabyl-group-alist nnbabyl-active-file)
      (nconc rest articles))))

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
       (goto-char (point-min))
       (if (search-forward (nnbabyl-article-string article) nil t)
	   (nnbabyl-delete-mail))
       (and last (save-buffer))))
    result))

(defun nnbabyl-request-accept-article (group &optional last)
  (let ((buf (current-buffer))
	result beg)
    (and 
     (nnmail-activate 'nnbabyl)
     (save-excursion
       (goto-char (point-min))
       (search-forward "\n\n" nil t)
       (forward-line -1)
       (save-excursion
	 (while (re-search-backward "^X-Gnus-Newsgroup: " beg t)
	   (delete-region (point) (progn (forward-line 1) (point)))))
       (let ((nnmail-split-methods
	      (if (stringp group) (list (list group "")) 
		nnmail-split-methods)))
	 (setq result (car (nnbabyl-save-mail))))
       (set-buffer nnbabyl-mbox-buffer)
       (goto-char (point-max))
       (search-backward "\n\^_")
       (goto-char (match-end 0))
       (insert-buffer-substring buf)
       (and last (progn 
		   (save-buffer)
		   (nnmail-save-active
		    nnbabyl-group-alist nnbabyl-active-file)))
       result))))

(defun nnbabyl-request-replace-article (article group buffer)
  (nnbabyl-possibly-change-newsgroup group)
  (save-excursion
    (set-buffer nnbabyl-mbox-buffer)
    (goto-char (point-min))
    (if (not (search-forward (nnbabyl-article-string article) nil t))
	nil
      (nnbabyl-delete-mail t t)
      (insert-buffer-substring buffer)
      (save-buffer)
      t)))

(defun nnbabyl-request-delete-group (group &optional force server)
  (nnbabyl-possibly-change-newsgroup group)
  ;; Delete all articles in GROUP.
  (if (not force)
      ()				; Don't delete the articles.
    (save-excursion
      (set-buffer nnbabyl-mbox-buffer)
      (goto-char (point-min))
      ;; Delete all articles in this group.
      (let ((ident (concat "\nX-Gnus-Newsgroup: " nnbabyl-current-group ":"))
	    found)
	(while (search-forward ident nil t)
	  (setq found t)
	  (nnbabyl-delete-mail))
	(and found (save-buffer)))))
  ;; Remove the group from all structures.
  (setq nnbabyl-group-alist 
	(delq (assoc group nnbabyl-group-alist) nnbabyl-group-alist)
	nnbabyl-current-group nil)
  ;; Save the active file.
  (nnmail-save-active nnbabyl-group-alist nnbabyl-active-file)
  t)

(defun nnbabyl-request-rename-group (group new-name &optional server)
  (nnbabyl-possibly-change-newsgroup group)
  (save-excursion
    (set-buffer nnbabyl-mbox-buffer)
    (goto-char (point-min))
    (let ((ident (concat "\nX-Gnus-Newsgroup: " nnbabyl-current-group ":"))
	  (new-ident (concat "\nX-Gnus-Newsgroup: " new-name ":"))
	  found)
      (while (search-forward ident nil t)
	(replace-match new-ident t t)
	(setq found t))
      (and found (save-buffer))))
  (let ((entry (assoc group nnbabyl-group-alist)))
    (and entry (setcar entry new-name))
    (setq nnbabyl-current-group nil)
    ;; Save the new group alist.
    (nnmail-save-active nnbabyl-group-alist nnbabyl-active-file)
    t))


;;; Internal functions.

;; If FORCE, delete article no matter how many X-Gnus-Newsgroup
;; headers there are. If LEAVE-DELIM, don't delete the Unix mbox
;; delimeter line.
(defun nnbabyl-delete-mail (&optional force leave-delim)
  ;; Delete the current X-Gnus-Newsgroup line.
  (or force
      (delete-region
       (progn (beginning-of-line) (point))
       (progn (forward-line 1) (point))))
  ;; Beginning of the article.
  (save-excursion
    (save-restriction
      (widen)
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
      (save-excursion (nnbabyl-read-mbox)))
  (or nnbabyl-group-alist
      (nnmail-activate 'nnbabyl))
  (if newsgroup
      (if (assoc newsgroup nnbabyl-group-alist)
	  (setq nnbabyl-current-group newsgroup)
	(setq nnbabyl-status-string "No such group in file")
	nil)))

(defun nnbabyl-article-string (article)
  (if (numberp article)
      (concat "\nX-Gnus-Newsgroup: " nnbabyl-current-group ":" 
	      (int-to-string article) " ")
    (concat "\nMessage-ID: " article)))

(defun nnbabyl-article-group-number ()
  (save-excursion
    (goto-char (point-min))
    (and (re-search-forward "^X-Gnus-Newsgroup: +\\([^:]+\\):\\([0-9]+\\) "
			    nil t)
	 (cons (buffer-substring (match-beginning 1) (match-end 1))
	       (string-to-int
		(buffer-substring (match-beginning 2) (match-end 2)))))))

(defun nnbabyl-insert-lines ()
  "Insert how many lines and chars there are in the body of the mail."
  (let (lines chars)
    (save-excursion
      (goto-char (point-min))
      (if (search-forward "\n\n" nil t) 
	  (progn
	    ;; There may be an EOOH line here...
	    (if (looking-at "\\*\\*\\* EOOH \\*\\*\\*")
		(search-forward "\n\n" nil t))
	    (setq chars (- (point-max) (point)))
	    (setq lines (- (count-lines (point) (point-max)) 1))
	    ;; Move back to the end of the headers. 
	    (goto-char (point-min))
	    (search-forward "\n\n" nil t)
	    (forward-char -1)
	    (save-excursion
	      (if (re-search-backward "^Lines: " nil t)
		  (delete-region (point) (progn (forward-line 1) (point)))))
	    (insert (format "Lines: %d\n" lines))
	    chars)))))

(defun nnbabyl-save-mail ()
  ;; Called narrowed to an article.
  (let ((group-art (nreverse (nnmail-article-group 'nnbabyl-active-number))))
    (nnbabyl-insert-lines)
    (nnmail-insert-xref group-art)
    (nnbabyl-insert-newsgroup-line group-art)
    (run-hooks 'nnbabyl-prepare-save-mail-hook)
    group-art))

(defun nnbabyl-insert-newsgroup-line (group-art)
  (save-excursion
    (goto-char (point-min))
    (while (looking-at "From ")
      (replace-match "Mail-from: From " t t)
      (forward-line 1))
    ;; If there is a C-l at the beginning of the narrowed region, this
    ;; isn't really a "save", but rather a "scan".
    (goto-char (point-min))
    (or (looking-at "\^L")
	(save-excursion
	  (insert "\^L\n0, unseen,,\n*** EOOH ***\n")
	  (goto-char (point-max))
	  (insert "\^_\n")))
    (if (search-forward "\n\n" nil t)
	(progn
	  (forward-char -1)
	  (while group-art
	    (insert (format "X-Gnus-Newsgroup: %s:%d   %s\n" 
			    (car (car group-art)) (cdr (car group-art))
			    (current-time-string)))
	    (setq group-art (cdr group-art)))))
    t))

(defun nnbabyl-active-number (group)
  ;; Find the next article number in GROUP.
  (let ((active (car (cdr (assoc group nnbabyl-group-alist)))))
    (if active
	(setcdr active (1+ (cdr active)))
      ;; This group is new, so we create a new entry for it.
      ;; This might be a bit naughty... creating groups on the drop of
      ;; a hat, but I don't know...
      (setq nnbabyl-group-alist (cons (list group (setq active (cons 1 1)))
				      nnbabyl-group-alist)))
    (cdr active)))

(defun nnbabyl-read-mbox ()
  (nnmail-activate 'nnbabyl)
  (or (file-exists-p nnbabyl-mbox-file)
      (save-excursion
	(set-buffer (setq nnbabyl-mbox-buffer
			  (create-file-buffer nnbabyl-mbox-file)))
	(setq buffer-file-name nnbabyl-mbox-file)
	(insert "BABYL OPTIONS:\n\n\^_")
	(write-region (point-min) (point-max) nnbabyl-mbox-file t 'nomesg)))

  (if (and nnbabyl-mbox-buffer
	   (buffer-name nnbabyl-mbox-buffer)
	   (save-excursion
	     (set-buffer nnbabyl-mbox-buffer)
	     (= (buffer-size) (nth 7 (file-attributes nnbabyl-mbox-file)))))
      ()
    (save-excursion
      (let ((delim (concat "^" nnbabyl-mail-delimiter))
	    start end)
	(set-buffer (setq nnbabyl-mbox-buffer 
			  (nnheader-find-file-noselect 
			   nnbabyl-mbox-file nil 'raw)))
	;; Save buffer mode.
	(setq nnbabyl-previous-buffer-mode 
	      (cons (cons (point-min) (point-max))
		    major-mode))

	(buffer-disable-undo (current-buffer))
	(widen)
	(setq buffer-read-only nil)
	(fundamental-mode)
	
	(goto-char (point-min))
	(re-search-forward delim nil t)
	(setq start (match-end 0))
	(while (re-search-forward delim nil t)
	  (setq end (match-end 0))
	  (or (search-backward "\nX-Gnus-Newsgroup: " start t)
	      (progn
		(goto-char end)
		(save-excursion
		  (save-restriction
		    (goto-char start)
		    (narrow-to-region start end)
		    (nnbabyl-save-mail)
		    (setq end (point-max))))))
	  (goto-char (setq start end)))
	(and (buffer-modified-p (current-buffer)) (save-buffer))
	(nnmail-save-active nnbabyl-group-alist nnbabyl-active-file)))))

(defun nnbabyl-remove-incoming-delims ()
  (goto-char (point-min))
  (while (search-forward "\^_" nil t)
    (replace-match "?" t t)))

(provide 'nnbabyl)

;;; nnbabyl.el ends here
