;;; nnsoup.el --- SOUP packet reading access for Gnus
;; Copyright (C) 1995 Free Software Foundation, Inc.

;; Author: Per Abrahamsen <abraham@iesd.auc.dk>
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
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; For an overview of what the interface functions do, please see the
;; Gnus sources.  

;; For more information on SOUP, see the comments in the file 
;; `gnus-soup.el'. 

;;; Code:

(require 'gnus-soup)
(require 'nnheader)
(require 'rmail)
(require 'nnmail)

(defvar nnsoup-directory (expand-file-name "~/SOUP/")
  "The name of the directory containing the unpacket SOUP packet.")



(defconst nnsoup-version "nnsoup 0.0"
  "nnsoup version.")

(defconst nnsoup-areas-file (concat nnsoup-directory  "AREAS"))
(defconst nnsoup-list-file (concat nnsoup-directory "LIST"))
(defconst nnsoup-gnus-file (concat nnsoup-directory "gnus.touched"))

(defvar nnsoup-current-group nil)
(defvar nnsoup-current-buffer nil)
(defvar nnsoup-status-string "")
(defvar nnsoup-group-alist nil)
(defvar nnsoup-buffer-alist nil)
(defconst nnsoup-areas-list nil)

;;; Interface functions

(defun nnsoup-retrieve-headers (sequence &optional newsgroup server)
  (save-excursion
    (set-buffer nntp-server-buffer)
    (erase-buffer)
    (let ((file nil)
	  (number (length sequence))
	  beg article art-string start stop)
      (nnsoup-possibly-change-group newsgroup)
      (while sequence
	(setq article (car sequence))
	(setq art-string (nnsoup-article-string article))
	(set-buffer nnsoup-current-buffer)
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
	      (insert-buffer-substring nnsoup-current-buffer start stop)
	      (goto-char (point-max))
	      (insert ".\n")))
	(setq sequence (cdr sequence)))

      ;; Fold continuation lines.
      (goto-char 1)
      (while (re-search-forward "\\(\r?\n[ \t]+\\)+" nil t)
	(replace-match " " t t))
      'headers)))

(defun nnsoup-open-server (host &optional service)
  (setq nnsoup-status-string "")
  (setq nnsoup-group-alist nil)
  (nnheader-init-server-buffer))

(defun nnsoup-close-server (&optional server)
  t)

(defun nnsoup-server-opened (&optional server)
  (and nntp-server-buffer
       (buffer-name nntp-server-buffer)))

(defun nnsoup-status-message (&optional server)
  nnsoup-status-string)

(defun nnsoup-request-article (article &optional newsgroup server buffer)
  (nnsoup-possibly-change-group newsgroup)
  (if (stringp article)
      nil
    (save-excursion
      (set-buffer nnsoup-current-buffer)
      (goto-char 1)
      (if (search-forward (nnsoup-article-string article) nil t)
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
	      (insert-buffer-substring nnsoup-current-buffer start stop)
	      (goto-char (point-min))
	      (while (looking-at "From ")
		(delete-char 5)
		(insert "X-From-Line: ")
		(forward-line 1))
	      t))))))

(defun nnsoup-request-group (group &optional server dont-check)
  (save-excursion
    (nnsoup-possibly-change-group group)
    (and (assoc group nnsoup-group-alist)
	 (save-excursion
	   (set-buffer nntp-server-buffer)
	   (erase-buffer)
	   (if dont-check
	       t
	     (nnsoup-request-list)
	     (setq nnsoup-group-alist (nnmail-get-active))
	     (let ((active (assoc group nnsoup-group-alist)))
	       (insert (format "211 %d %d %d %s\n" 
			       (1+ (- (cdr (car (cdr active)))
				      (car (car (cdr active)))))
			       (car (car (cdr active)))
			       (cdr (car (cdr active)))
			       (car active))))
	     t)))))

(defun nnsoup-close-group (group &optional server)
  t)

(defun nnsoup-request-list (&optional server)
  (if server
      (if (or (file-exists-p nnsoup-gnus-file)
	      (not (file-directory-p nnsoup-directory)))
	  ()
	(write-region 1 1 nnsoup-gnus-file)
	(setq nnsoup-areas-list nil
	      nnsoup-current-group nil
	      nnsoup-current-buffer nil
	      nnsoup-group-alist nil)
	(let ((buffer (get-file-buffer nnsoup-areas-file))
	      (groups gnus-newsrc-assoc)
	      group)
	  (while groups
	    (setq group (car groups)
		  groups (cdr groups))
	    (if (eq (car (gnus-group-method-name (car group))) 'nnsoup)
		(progn 
		  (setcar (nthcdr 2 group) nil)
		  (setcar (nthcdr 3 group) nil))))
	  (gnus-make-hashtable-from-newsrc-alist)
	  (if buffer
	      (kill-buffer buffer))
	  (while nnsoup-buffer-alist 
	    (setq buffer (nth 1 (car nnsoup-buffer-alist))
		  nnsoup-buffer-alist (cdr nnsoup-buffer-alist))
	    (if (buffer-name buffer)
		(kill-buffer buffer))))))
  (nnsoup-find-active))

(defun nnsoup-request-newgroups (date &optional server)
  (nnsoup-request-list server))

(defun nnsoup-request-list-newsgroups (&optional server)
  (nnmail-find-file nnsoup-newsgroups-file))

(defun nnsoup-request-post (&optional server)
  (mail-send-and-exit nil))

(fset 'nnsoup-request-post-buffer 'nnmail-request-post-buffer)

(defun nnsoup-request-expire-articles (articles newsgroup &optional server force)
  (setq nnsoup-status-string "nnsoup: expire not possible")
  nil)

(defun nnsoup-request-move-article (article group server accept-form)
  (setq nnsoup-status-string "nnsoup: move not possible")
  nil)

(defun nnsoup-request-accept-article (group)
  (setq nnsoup-status-string "nnsoup: accept not possible")
  nil)


;;; Internal functions.

(defun nnsoup-possibly-change-group (group)
  (or (file-exists-p nnsoup-directory)
      (make-directory (directory-file-name nnsoup-directory)))
  (if (not nnsoup-group-alist)
      (progn
	(nnsoup-request-list)
	(setq nnsoup-group-alist (nnmail-get-active))))
  (let (inf file)
    (if (and (equal group nnsoup-current-group)
	     (buffer-name nnsoup-current-buffer))
	()
      (if (setq inf (member group nnsoup-buffer-alist))
	  (setq nnsoup-current-buffer (nth 1 inf)))
      (setq nnsoup-current-group group)
      (if (not (buffer-name nnsoup-current-buffer))
	  (progn
	    (setq nnsoup-buffer-alist (delq inf nnsoup-buffer-alist))
	    (setq inf nil)))
      (if inf
	  ()
	(save-excursion
	  (setq file (nnsoup-group-file group))
;;;;	  (if (not (file-exists-p file))
;;;;	      (write-region 1 1 file t 'nomesg))
	  (set-buffer (nnsoup-read-folder file))
	  (setq nnsoup-buffer-alist (cons (list group (current-buffer))
					    nnsoup-buffer-alist))))))
  (setq nnsoup-current-group group))

(defun nnsoup-article-string (article)
  (concat "\nX-Gnus-Article-Number: " (int-to-string article) " "))

(defun nnsoup-read-folder (file)
  (nnsoup-request-list)
  (setq nnsoup-group-alist (nnmail-get-active))
  (save-excursion
    (set-buffer
     (setq nnsoup-current-buffer 
	   (find-file-noselect file)))
    (buffer-disable-undo (current-buffer))
    (let ((delim (concat "^" rmail-unix-mail-delimiter))
	  start end 
	  (number 1))
      (goto-char (point-min))
      (while (re-search-forward delim nil t)
	(setq start (match-beginning 0))
	(save-excursion 
	  (setq end (or (and (re-search-forward delim nil t)
			     (match-beginning 0))
			(point-max))))
	(save-excursion
	  (save-restriction
	    (narrow-to-region start end)
	    (nnmail-insert-lines)
	    (save-excursion
	      (goto-char (point-min))
	      (if (search-forward "\n\n" nil t)
		  (progn
		    (forward-char -1)
		    (insert (format "X-Gnus-Article-Number: %d   %s\n" 
				    number (current-time-string))))))
	    (setq number (1+ number))))
	(goto-char end)))
    (set-buffer-modified-p nil)
    (current-buffer)))

(defun nnsoup-find-active ()
  (set-buffer nntp-server-buffer)
  (erase-buffer)
  (or nnsoup-areas-list (nnsoup-read-areas))
  (condition-case ()
      (progn 
	(let ((areas nnsoup-areas-list)
	      area)
	  (while areas 
	    (setq area (car areas)
		  areas (cdr areas))
	    (insert (format "%s %s 1 y\n" (aref area 1) (aref area 4)))))
	t)
    (file-error nil)))

(defun nnsoup-read-areas ()
  (setq nnsoup-areas-list (gnus-soup-parse-areas nnsoup-areas-file))
  (let ((areas nnsoup-areas-list)
	area)
    (while areas 
      (setq area (car areas)
	    areas (cdr areas))
      (aset area 4 (nnsoup-count-area area)))))

(defun nnsoup-count-area (area)
  (or (aref area 4)
      (number-to-string 
       (nnsoup-count-mbox (concat nnsoup-directory (aref area 0) ".MSG")))))

(defun nnsoup-count-mbox (file)
  (let ((delete (find-buffer-visiting file))
	(num 0)
	(delim (concat "^" rmail-unix-mail-delimiter)))
    (save-excursion
      (set-buffer (find-file-noselect file))
      (goto-char (point-min))
      (while (re-search-forward delim nil t)
	(setq num (1+ num)))
      (if delete (kill-buffer delete))
      num)))

(defun nnsoup-group-file (group)
  (let ((areas nnsoup-areas-list)
	area result)
    (while areas 
      (setq area (car areas)
	    areas (cdr areas))
      (if (equal (aref area 1) group)
	  (setq result (concat nnsoup-directory (aref area 0) ".MSG"))))
    result))

(provide 'nnsoup)

;;; nnsoup.el ends here
