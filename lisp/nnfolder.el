;;; nnfolder.el --- mail folder access for Gnus
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

(defvar nnfolder-directory (expand-file-name "~/Mail/")
  "The name of the mail box file in the users home directory.")

(defvar nnfolder-active-file (concat nnfolder-directory  "active")
  "The name of the active file.")

(defvar nnfolder-newsgroups-file (concat nnfolder-directory "newsgroups")
  "Mail newsgroups description file.")

(defvar nnfolder-get-new-mail t
  "If non-nil, nnml will check the incoming mail file and split the mail.")



(defconst nnfolder-version "nnfolder 0.1"
  "nnfolder version.")

(defvar nnfolder-current-group nil)
(defvar nnfolder-current-buffer nil)
(defvar nnfolder-status-string "")
(defvar nnfolder-group-alist nil)
(defvar nnfolder-buffer-alist nil)

;;; Interface functions

(defun nnfolder-retrieve-headers (sequence &optional newsgroup server)
  (save-excursion
    (set-buffer nntp-server-buffer)
    (erase-buffer)
    (let ((file nil)
	  (number (length sequence))
	  beg article art-string start stop)
      (nnfolder-possibly-change-group newsgroup)
      (while sequence
	(setq article (car sequence))
	(setq art-string (nnfolder-article-string article))
	(set-buffer nnfolder-current-buffer)
	(if (or (search-forward art-string nil t)
		(progn (goto-char 1)
		       (search-forward art-string nil t)))
	    (progn
	      (setq start 
		    (save-excursion
		      (re-search-backward 
		       (concat "^" rmail-unix-mail-delimiter) nil t)
		      (point)))
	      (search-forward "\n\n" nil t)
	      (setq stop (1- (point)))
	      (set-buffer nntp-server-buffer)
	      (insert (format "221 %d Article retrieved.\n" article))
	      (setq beg (point))
	      (insert-buffer-substring nnfolder-current-buffer start stop)
	      (goto-char (point-max))
	      (insert ".\n")))
	(setq sequence (cdr sequence)))

      ;; Fold continuation lines.
      (goto-char 1)
      (while (re-search-forward "\\(\r?\n[ \t]+\\)+" nil t)
	(replace-match " " t t))
      'headers)))

(defun nnfolder-open-server (host &optional service)
  (setq nnfolder-status-string "")
  (setq nnfolder-group-alist nil)
  (nnheader-init-server-buffer))

(defun nnfolder-close-server (&optional server)
  t)

(defun nnfolder-server-opened (&optional server)
  (and nntp-server-buffer
       (buffer-name nntp-server-buffer)))

(defun nnfolder-status-message (&optional server)
  nnfolder-status-string)

(defun nnfolder-request-article (article &optional newsgroup server buffer)
  (nnfolder-possibly-change-group newsgroup)
  (if (stringp article)
      nil
    (save-excursion
      (set-buffer nnfolder-current-buffer)
      (goto-char 1)
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
	      t))))))

(defun nnfolder-request-group (group &optional server dont-check)
  (save-excursion
    (nnfolder-possibly-change-group group)
    (and (assoc group nnfolder-group-alist)
	 (save-excursion
	   (set-buffer nntp-server-buffer)
	   (erase-buffer)
	   (if dont-check
	       t
	     (nnfolder-get-new-mail)
	     (let ((active (assoc group nnfolder-group-alist)))
	       (insert (format "211 %d %d %d %s\n" 
			       (1+ (- (cdr (car (cdr active)))
				      (car (car (cdr active)))))
			       (car (car (cdr active)))
			       (cdr (car (cdr active)))
			       (car active))))
	     t)))))

(defun nnfolder-close-group (group &optional server)
  t)

(defun nnfolder-request-list (&optional server)
  (if server (nnfolder-get-new-mail))
  (or (nnmail-find-file nnfolder-active-file)
      (progn
	(setq nnfolder-group-alist (nnmail-get-active))
	(nnmail-save-active nnfolder-group-alist nnfolder-active-file)
	(nnmail-find-file nnfolder-active-file))))

(defun nnfolder-request-newgroups (date &optional server)
  (nnfolder-request-list server))

(defun nnfolder-request-list-newsgroups (&optional server)
  (nnmail-find-file nnfolder-newsgroups-file))

(defun nnfolder-request-post (&optional server)
  (mail-send-and-exit nil))

(fset 'nnfolder-request-post-buffer 'nnmail-request-post-buffer)

(defun nnfolder-request-expire-articles (articles newsgroup &optional server force)
  (nnfolder-possibly-change-group newsgroup)
  (let* ((days (or (and nnmail-expiry-wait-function
			(funcall nnmail-expiry-wait-function newsgroup))
		   nnmail-expiry-wait))
	 article rest)
    (save-excursion 
      (set-buffer nnfolder-current-buffer)
      (while articles
	(goto-char 1)
	(if (search-forward (nnfolder-article-string (car articles)) nil t)
	    (if (or force
		    (> (nnmail-days-between 
			(current-time-string)
			(buffer-substring 
			 (point) (progn (end-of-line) (point))))
		       days))
		(progn
		  (and gnus-verbose-backends
		       (message "Deleting: %s" (car articles)))
		  (nnfolder-delete-mail))
	      (setq rest (cons (car articles) rest))))
	(setq articles (cdr articles)))
      (save-buffer)
      ;; Find the lowest active article in this group.
      (let ((active (nth 1 (assoc newsgroup nnfolder-group-alist))))
	(goto-char (point-min))
	(while (not (search-forward
		     (nnfolder-article-string (car active)) nil t))
	  (setcar (car active) (1+ (car active)))
	  (goto-char (point-min))))
      (nnmail-save-active nnfolder-group-alist nnfolder-active-file)
      rest)))

(defun nnfolder-request-move-article
  (article group server accept-form &optional last)
  (nnfolder-possibly-change-group group)
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
	       "^X-Gnus-Newsgroup:" 
	       (save-excursion (search-forward "\n\n" nil t) (point)) t)
	 (delete-region (progn (beginning-of-line) (point))
			(progn (forward-line 1) (point))))
       (setq result (eval accept-form))
       (kill-buffer buf)
       result)
     (save-excursion
       (nnfolder-possibly-change-group group)
       (set-buffer nnfolder-current-buffer)
       (goto-char 1)
       (if (search-forward (nnfolder-article-string article) nil t)
	   (nnfolder-delete-mail))
       (and last (save-buffer))))
    result))

(defun nnfolder-request-accept-article (group &optional last)
  (nnfolder-possibly-change-group group)
  (let ((buf (current-buffer))
	result beg)
    (goto-char (point-min))
    (if (looking-at "X-From-Line: ")
	(replace-match "From ")
      (insert "From nobody " (current-time-string) "\n"))
    (and 
     (nnfolder-request-list)
     (setq nnfolder-group-alist (nnmail-get-active))
     (progn
       (set-buffer buf)
       (goto-char (point-min))
       (search-forward "\n\n" nil t)
       (forward-line -1)
       (while (re-search-backward "^X-Gnus-Newsgroup: " nil t)
	 (delete-region (point) (progn (forward-line 1) (point))))
       (setq result (nnfolder-save-mail (and (stringp group) group))))
     (save-excursion
       (set-buffer nnfolder-current-buffer)
       (insert-buffer-substring buf)
       (and last (save-buffer))
       result)
     (nnmail-save-active nnfolder-group-alist nnfolder-active-file))
    (car result)))

(defun nnfolder-request-replace-article (article group buffer)
  (nnfolder-possibly-change-group group)
  (save-excursion
    (set-buffer nnfolder-current-buffer)
    (goto-char 1)
    (if (not (search-forward (nnfolder-article-string article) nil t))
	nil
      (nnfolder-delete-mail t t)
      (insert-buffer-substring buffer)
      (save-buffer)
      t)))


;;; Internal functions.

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

(defun nnfolder-possibly-change-group (group)
  (or (file-exists-p nnfolder-directory)
      (make-directory (directory-file-name nnfolder-directory)))
  (if (not nnfolder-group-alist)
      (progn
	(nnfolder-request-list)
	(setq nnfolder-group-alist (nnmail-get-active))))
  (or (assoc group nnfolder-group-alist)
      (not (file-exists-p (concat nnfolder-directory group)))
      (progn
	(setq nnfolder-group-alist 
	      (cons (list group (cons 1 0)) nnfolder-group-alist))
	(nnmail-save-active nnfolder-group-alist nnfolder-active-file)))
  (let (inf file)
    (if (and (equal group nnfolder-current-group)
	     (buffer-name nnfolder-current-buffer))
	()
      (if (setq inf (member group nnfolder-buffer-alist))
	  (setq nnfolder-current-buffer (nth 1 inf)))
      (setq nnfolder-current-group group)
      (if (not (buffer-name nnfolder-current-buffer))
	  (progn
	    (setq nnfolder-buffer-alist (delq inf nnfolder-buffer-alist))
	    (setq inf nil)))
      (if inf
	  ()
	(save-excursion
	  (setq file (concat nnfolder-directory group))
	  (if (not (file-exists-p file))
	      (write-region 1 1 file t 'nomesg))
	  (set-buffer (nnfolder-read-folder file))
	  (setq nnfolder-buffer-alist (cons (list group (current-buffer))
					    nnfolder-buffer-alist))))))
  (setq nnfolder-current-group group))

(defun nnfolder-article-string (article)
  (concat "\nX-Gnus-Article-Number: " (int-to-string article) " "))

(defun nnfolder-save-mail (&optional group)
  "Called narrowed to an article."
  (let* ((nnmail-split-methods 
	  (if group (list (list group "")) nnmail-split-methods))
	 (group-art-list
	  (nreverse (nnmail-article-group 'nnfolder-active-number)))
	 group-art)
    (nnmail-insert-lines)
    (nnmail-insert-xref group-art-list)
    (while group-art-list
      (setq group-art (car group-art-list)
	    group-art-list (cdr group-art-list))
      (nnfolder-possibly-change-group (car group-art))
      (nnfolder-insert-newsgroup-line group-art)
      (let ((beg (point-min))
	    (end (point-max))
	    (obuf (current-buffer)))
	(save-excursion
	  (set-buffer nnfolder-current-buffer)
	  (goto-char (point-max))
	  (insert-buffer-substring obuf beg end)))
      (goto-char (point-min))
      (search-forward "\nX-Gnus-Article-Number: ")
      (delete-region (progn (beginning-of-line) (point))
		     (progn (forward-line 1) (point))))))

(defun nnfolder-insert-newsgroup-line (group-art)
  (save-excursion
    (goto-char (point-min))
    (if (search-forward "\n\n" nil t)
	(progn
	  (forward-char -1)
	  (insert (format "X-Gnus-Article-Number: %d   %s\n" 
			  (cdr group-art) (current-time-string)))))))

(defun nnfolder-active-number (group)
  (let ((active (car (cdr (assoc group nnfolder-group-alist)))))
    (setcdr active (1+ (cdr active)))
    (cdr active)))

(defun nnfolder-read-folder (file)
  (nnfolder-request-list)
  (setq nnfolder-group-alist (nnmail-get-active))
  (save-excursion
    (set-buffer
     (setq nnfolder-current-buffer 
	   (find-file-noselect file)))
    (buffer-disable-undo (current-buffer))
    (let ((delim (concat "^" rmail-unix-mail-delimiter))
	  start end)
      (goto-char (point-min))
      (while (re-search-forward delim nil t)
	(setq start (match-beginning 0))
	(if (not (search-forward "\nX-Gnus-Article-Number: " 
				 (save-excursion 
				   (setq end
					 (or
					  (and
					   (re-search-forward delim nil t)
					   (match-beginning 0))
					  (point-max))))
				 t))
	    (save-excursion
	      (save-restriction
		(narrow-to-region start end)
		(nnmail-insert-lines)
		(nnfolder-insert-newsgroup-line 
		 (cons nil (nnfolder-active-number nnfolder-current-group))))))
	(goto-char end)))
    (nnmail-save-active nnfolder-group-alist nnfolder-active-file)
    (current-buffer)))

(defun nnfolder-get-new-mail ()
  (let (incoming)
    (if (and nnmail-spool-file
	     nnfolder-get-new-mail
	     (file-exists-p nnmail-spool-file)
	     (> (nth 7 (file-attributes nnmail-spool-file)) 0))
	(progn
	  (and gnus-verbose-backends
	       (message "nnfolder: Reading incoming mail..."))
	  (setq incoming 
		(nnmail-move-inbox nnmail-spool-file
				   (concat nnfolder-directory "Incoming")))
	  (nnmail-split-incoming incoming 'nnfolder-save-mail)
	  (run-hooks 'nnmail-read-incoming-hook)
	  (nnmail-save-active nnfolder-group-alist nnfolder-active-file)
	  (and gnus-verbose-backends
	       (message "nnfolder: Reading incoming mail...done"))))
    (let ((bufs nnfolder-buffer-alist))
      (save-excursion
	(while bufs
	  (if (not (buffer-name (nth 1 (car bufs))))
	      (setq nnfolder-buffer-alist 
		    (delq (car bufs) nnfolder-buffer-alist))
	    (set-buffer (nth 1 (car bufs)))
	    (save-buffer))
	  (setq bufs (cdr bufs)))))
    ;; (if incoming (delete-file incoming))
    ))

(provide 'nnfolder)

;;; nnfolder.el ends here
