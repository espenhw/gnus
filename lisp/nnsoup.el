;;; nnsoup.el --- SOUP access for Gnus
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
(require 'nnmail)
(require 'gnus-soup)
(require 'gnus-msg)

(defvar nnsoup-directory "~/SOUP/"
  "*SOUP packet directory directory.")

(defvar nnsoup-replies-directory (concat nnsoup-directory "replies/")
  "*Directory where outgoing packets will be composed.")

(defvar nnsoup-replies-format-type ?n
  "*Format of the replies packages.")

(defvar nnsoup-replies-index-type ?n
  "*Index type of the replies packages.")

(defvar nnsoup-active-file (concat nnsoup-directory "active")
  "Active file.")

(defvar nnsoup-packer "tar cf - %s | gzip > $HOME/Soupin%d.tgz"
  "Format string command for packing a SOUP packet.
The SOUP files will be inserted where the %s is in the string.
This string MUST contain both %s and %d. The file number will be
inserted where %d appears.")

(defvar nnsoup-unpacker "gunzip -c %s | tar xvf -"
  "*Format string command for unpacking a SOUP packet.
The SOUP packet file name will be inserted at the %s.")

(defvar nnsoup-packet-directory "~/"
  "*Where nnsoup will look for incoming packets.")

(defvar nnsoup-packet-regexp "Soupout"
  "*Regular expression matching SOUP packets in `nnsoup-packet-directory'.")



(defconst nnsoup-version "nnsoup 0.0"
  "nnsoup version.")

(defvar nnsoup-status-string "")
(defvar nnsoup-group-alist nil)
(defvar nnsoup-replies-list nil)
(defvar nnsoup-buffers nil)
(defvar nnsoup-current-group nil)



;; Server variables.

(defvar nnsoup-current-server nil)
(defvar nnsoup-server-alist nil)
(defvar nnsoup-server-variables 
  (list 
   (list 'nnsoup-directory nnsoup-directory)
   (list 'nnsoup-active-file nnsoup-active-file)
   '(nnsoup-status-string "")
   '(nnsoup-group-alist nil)))



;;; Interface functions.

(defun nnsoup-retrieve-headers (sequence &optional group server)
  (nnsoup-possibly-change-group group)
  (save-excursion
    (set-buffer nntp-server-buffer)
    (erase-buffer)
    (let ((count 0)
	  (areas (cdr (assoc nnsoup-current-group nnsoup-group-alist)))
	  (articles sequence)
	  (use-nov t)
	  beg article useful-areas this-area-seq)
      (if (stringp (car sequence))
	  'headers
	;; We go through all the areas and find which files the
	;; articles in SEQUENCE come from.
	(while (and areas sequence)
	  ;; Peel off areas that are below sequence.
	  (while (and areas (< (cdr (car (car areas))) (car sequence)))
	    (setq areas (cdr areas)))
	  (if (not areas)
	      ()
	    ;; This is a useful area.
	    (setq useful-areas (cons (car areas) useful-areas)
		  this-area-seq nil)
	    ;; We take note whether this MSG has a corresponding IDX
	    ;; for later use.
	    (if (or (= (gnus-soup-encoding-index 
			(gnus-soup-area-encoding (nth 1 (car areas)))) ?n)
		    (not (file-exists-p
			  (nnsoup-file
			   (gnus-soup-area-prefix (nth 1 (car areas)))))))
		(setq use-nov nil))
	    ;; We assing the portion of `sequence' that is relevant to
	    ;; this MSG packet to this packet.
	    (while (and sequence (<= (car sequence) (cdr (car (car areas)))))
	      (setq this-area-seq (cons (car sequence) this-area-seq)
		    sequence (cdr sequence)))
	    (setcar useful-areas (cons (nreverse this-area-seq)
				       (car useful-areas)))))

	;; We now have a list of article numbers and corresponding
	;; areas. 
	(setq useful-areas (nreverse useful-areas))

	;; Two different approaches depending on whether all the MSG
	;; files have corresponding IDX files.  If they all do, we
	;; simply return the relevant IDX files and let Gnus sort out
	;; what lines are relevant.  If some of the IDX files are
	;; missing, we must return HEADs for all the articles.
	(if use-nov
	    (while useful-areas
	      (goto-char (point-max))
	      (let ((b (point))
		    (number (car (nth 1 (car useful-areas)))))
		(insert-buffer-substring
		 (nnsoup-index-buffer
		  (gnus-soup-area-prefix
		   (nth 2 (car useful-areas)))))
		(goto-char b)
		;; We have to remove the index number entires and
		;; insert article numbers instead.
		(while (looking-at "[0-9]+")
		  (replace-match (int-to-string number) t t)
		  (setq number (1+ number))
		  (forward-line 1)))
	      (setq useful-areas (cdr useful-areas)))
	  ;; We insert HEADs.
	  (while useful-areas
	    (setq articles (car (car useful-areas))
		  useful-areas (cdr useful-areas))
	    (while articles
	      (goto-char (point-max))
	      (insert (format "221 %d Article retrieved.\n" (car articles)))
	      (insert-buffer-substring
	       (nnsoup-narrow-to-article 
		(car articles) (cdr (car useful-areas)) 'head))
	      (goto-char (point-max))
	      (insert ".\n")
	      (setq articles (cdr articles))))

	  ;; Fold continuation lines.
	  (goto-char (point-min))
	  (while (re-search-forward "\\(\r?\n[ \t]+\\)+" nil t)
	    (replace-match " " t t)))
	(if use-nov 'nov 'headers)))))

(defun nnsoup-open-server (server &optional defs)
  (nnsoup-set-variables)
  (nnheader-init-server-buffer)
  (if (equal server nnsoup-current-server)
      t
    (if nnsoup-current-server
	(setq nnsoup-server-alist 
	      (cons (list nnsoup-current-server
			  (nnheader-save-variables nnsoup-server-variables))
		    nnsoup-server-alist)))
    (let ((state (assoc server nnsoup-server-alist)))
      (if state 
	  (progn
	    (nnheader-restore-variables (nth 1 state))
	    (setq nnsoup-server-alist (delq state nnsoup-server-alist)))
	(nnheader-set-init-variables nnsoup-server-variables defs)))
    (setq nnsoup-current-server server)))

(defun nnsoup-request-close ()
  (nnsoup-write-replies)
  (while nnsoup-buffers
    (and (car nnsoup-buffers)
	 (buffer-name (car nnsoup-buffers))
	 (kill-buffer (car nnsoup-buffers)))
    (setq nnsoup-buffers (cdr nnsoup-buffers)))
  (setq nnsoup-group-alist nil
	nnsoup-current-group nil
	nnsoup-current-server nil
	nnsoup-server-alist nil)
  t)

(defun nnsoup-close-server (&optional server)
  t)

(defun nnsoup-server-opened (&optional server)
  (and (equal server nnsoup-current-server)
       nntp-server-buffer
       (buffer-name nntp-server-buffer)))

(defun nnsoup-status-message (&optional server)
  nnsoup-status-string)

(defun nnsoup-request-article (id &optional newsgroup server buffer)
  (nnsoup-possibly-change-group newsgroup)
  (let ((buffer (or buffer nntp-server-buffer)))
    (save-excursion
      (set-buffer buffer)
      (erase-buffer)
      (if (stringp id)
	  ()
	(insert-buffer-substring
	 (nnsoup-narrow-to-article id))
	t))))

(defun nnsoup-request-group (group &optional server dont-check)
  (nnsoup-possibly-change-group group)
  (if dont-check 
      ()
    (let ((area (cdr (assoc group nnsoup-group-alist)))
	  min max)
      (setq min (car (car (car area))))
      (while (cdr area)
	(setq area (cdr area)))
      (setq max (cdr (car (car area))))
      (insert (format "211 %d %d %d %s\n" 
		      (max (1+ (- max min)) 0) min max group))))
  t)

(defun nnsoup-close-group (group &optional server)
  t)

(defun nnsoup-request-list (&optional server)
  (or nnsoup-group-alist (nnsoup-read-areas))
  (nnsoup-unpack-packets)
  (save-excursion
    (set-buffer nntp-server-buffer)
    (erase-buffer)
    (let ((alist nnsoup-group-alist)
	  min)
      (while alist
	(setq min (car (car (nth 1 (car alist)))))
	(insert (format "%s %d %d y\n" (car (car alist))
			(let ((areas (car alist)))
			  (while (cdr areas)
			    (setq areas (cdr areas)))
			  (cdr (car (car areas)))) min))
	(setq alist (cdr alist)))
      t)))

(defun nnsoup-request-newgroups (date &optional server)
  (nnsoup-request-list))

(defun nnsoup-request-list-newsgroups (&optional server)
  nil)

(defun nnsoup-request-post (&optional server)
  (nnsoup-store-reply "news")
  t)

(defun nnsoup-request-mail ()
  (nnsoup-store-reply "mail")
  t)

(defun nnsoup-request-post-buffer (post group &rest args)
  (nnsoup-possibly-change-group group)
  (apply
   ;; Find out whether the source for this group is a mail or a news
   ;; group and call the right function for getting a buffer.
   (let ((enc (nth 1 (car (cdr (assoc nnsoup-current-group
				      nnsoup-group-alist))))))
     (if (and enc
	      (= (gnus-soup-encoding-kind (gnus-soup-area-encoding enc)) ?m))
	 'nnmail-request-post-buffer 
       'nntp-request-post-buffer))
   post group args))


;;; Internal functions

(defun nnsoup-possibly-change-group (group &optional force)
  (if group
      (setq nnsoup-current-group group)
    t))

(defun nnsoup-read-active-file ()
  (if (file-exists-p nnsoup-active-file)
      (condition-case ()
	  (load nnsoup-active-file)
	(error nil))))

(defun nnsoup-write-active-file ()
  (save-excursion
    (set-buffer (get-buffer-create " *nnsoup work*"))
    (buffer-disable-undo (current-buffer))
    (erase-buffer)
    (insert (format "(setq nnsoup-group-alist '%S)\n" nnsoup-group-alist))
    (write-region (point-min) (point-max) nnsoup-active-file
		  nil 'silent)
    (kill-buffer (current-buffer))))

(defun nnsoup-read-areas ()
  (save-excursion
    (set-buffer nntp-server-buffer)
    (let ((areas (gnus-soup-parse-areas (concat nnsoup-directory "AREAS")))
	  entry number area lnum)
      ;; Go through all areas in the new AREAS file.
      (while areas
	(setq area (car areas)
	      areas (cdr areas))
	;; Find the number of new articles in this area.
	(setq number (nnsoup-number-of-articles area))
	(if (not (setq entry (assoc (gnus-soup-area-name area)
				    nnsoup-group-alist)))
	    ;; If this is a new area (group), we just add this info to
	    ;; the group alist. 
	    (setq nnsoup-group-alist
		  (cons (list (gnus-soup-area-name area)
			      (list (cons 1 number) area))
			nnsoup-group-alist))
	  ;; There are already articles in this group, so we add this
	  ;; info to the end of the entry.
	  (let ((e (cdr entry)))
	    (while (cdr e)
	      (setq e (cdr e)))
	    (setcdr e (list (list (cons (setq lnum (1+ (cdr (nth 1 (car e)))))
					(+ lnum number)) 
				  area)))))))))

(defun nnsoup-number-of-articles (area)
  (save-excursion
    (cond 
     ;; If the number is in the area info, we just return it.
     ((gnus-soup-area-number area)
      (gnus-soup-area-number area))
     ;; If there is an index file, we just count the lines.
     ((/= (gnus-soup-encoding-index (gnus-soup-area-encoding area)) ?n)
      (set-buffer (nnsoup-index-buffer (gnus-soup-area-prefix area)))
      (count-lines (point-min) (point-max)))
     ;; We do it the hard way - re-searching through the message
     ;; buffer. 
     (t
      (set-buffer (nnsoup-message-buffer (gnus-soup-area-prefix area)))
      (goto-char (point-min))
      (let ((regexp (nnsoup-header (gnus-soup-encoding-format 
				    (gnus-soup-area-encoding area))))
	    (num 0))
	(while (re-search-forward regexp nil t)
	  (setq num (1+ num)))
	num)))))

(defun nnsoup-index-buffer (prefix &optional message)
  (let* ((file (concat prefix (if message ".MSG" ".IDX")))
	 (buffer-name (concat " *nnsoup " file "*")))
    (or (get-buffer buffer-name)	; File aready loaded.
	(save-excursion			; Load the file.
	  (set-buffer (get-buffer-create buffer-name))
	  (setq nnsoup-buffers (cons (current-buffer) nnsoup-buffers))
	  (insert-file-contents (concat nnsoup-directory file))
	  (current-buffer)))))

(defun nnsoup-file (prefix &optional message)
  (concat nnsoup-directory prefix (if message ".MSG" ".IDX")))

(defun nnsoup-message-buffer (prefix)
  (nnsoup-index-buffer prefix 'msg))

(defun nnsoup-unpack-packets ()
  (let ((packets (directory-files
		  nnsoup-packet-directory t nnsoup-packet-regexp))
	msg)
    (while packets
      (message (setq msg (format "nnsoup: unpacking %s..." (car packets))))
      (gnus-soup-unpack-packet nnsoup-directory nnsoup-unpacker (car packets))
      (delete-file (car packets))
      (nnsoup-read-areas)
      (message "%sdone" msg)
      (setq packets (cdr packets)))))

(defun nnsoup-narrow-to-article (article &optional area head)
  (let* ((area (or area (nnsoup-article-to-area article nnsoup-current-group)))
	 (prefix (gnus-soup-area-prefix (nth 1 area)))
	 beg end msg-buf)
    (setq msg-buf (nnsoup-index-buffer prefix 'msg))
    (save-excursion
      (cond
       ;; We use the index file to find out where the article begins and ends. 
       ((and (= (gnus-soup-encoding-index 
		 (gnus-soup-area-encoding (nth 1 area)))
		?c)
	     (file-exists-p (nnsoup-file prefix)))
	(set-buffer (nnsoup-index-buffer prefix))
	(goto-char (point-min))
	(forward-line (- article (car (car area))))
	(setq beg (read (current-buffer)))
	(forward-line 1)
	(if (looking-at "[0-9]+")
	    (progn
	      (setq end (read (current-buffer)))
	      (set-buffer msg-buf)
	      (widen)
	      (let ((format (gnus-soup-encoding-format
			     (gnus-soup-area-encoding (nth 1 area)))))
		(goto-char end)
		(if (or (= format ?n) (= format ?m))
		    (setq end (progn (forward-line -2) (point))))))
	  (set-buffer msg-buf))
	(widen)
	(narrow-to-region beg (or end (point-max))))
       (t
	(set-buffer msg-buf)
	(widen)
	(goto-char (point-min))
	(let ((header (nnsoup-header 
		       (gnus-soup-encoding-format 
			(gnus-soup-area-encoding (nth 1 area))))))
	  (re-search-forward header nil t (- article (car (car area))))
	  (narrow-to-region
	   (match-beginning 0)
	   (if (re-search-forward header nil t)
	       (match-beginning 0)
	     (point-max))))))
      (goto-char (point-min))
      (if (not head)
	  ()
	(narrow-to-region
	 (point-min)
	 (if (search-forward "\n\n" nil t)
	     (1- (point))
	   (point-max))))
      msg-buf)))

(defun nnsoup-header (format)
  (cond 
   ((= format ?n)
    "^#! *rnews +[0-9]+ *$")
   ((= format ?m)
    (concat "^" rmail-unix-mail-delimiter))
   ((= format ?M)
    "^\^A\^A\^A\^A\n")
   (t
    (error "Unknown format: %c" format))))

(defun nnsoup-pack-replies ()
  "Make an outbound package of SOUP replies."
  (interactive)
  (nnsoup-write-replies)
  (gnus-soup-pack nnsoup-replies-directory nnsoup-packer))

(defun nnsoup-write-replies ()
  (gnus-soup-write-replies nnsoup-replies-directory nnsoup-replies-list))

(defun nnsoup-article-to-area (article group)
  (let ((areas (cdr (assoc group nnsoup-group-alist))))
    (while (and areas (< (cdr (car (car areas))) article))
      (setq areas (cdr areas)))
    (and areas (car areas))))

(defun nnsoup-set-variables ()
  (setq gnus-inews-article-function 'nnsoup-request-post)
  (setq gnus-mail-send-method 'nnsoup-request-mail)
  (setq send-mail-function 'nnsoup-request-mail))

(defun nnsoup-store-reply (kind)
  ;; Mostly stolen from `sendmail.el'.
  (let ((tembuf (generate-new-buffer " sendmail temp"))
	(case-fold-search nil)
	(mailbuf (current-buffer))
	delimline
	prefix)
    (save-excursion
      (set-buffer tembuf)
      (erase-buffer)
      (insert-buffer-substring mailbuf)
      (goto-char (point-max))
      ;; require one newline at the end.
      (or (= (preceding-char) ?\n)
	  (insert ?\n))
      ;; Change header-delimiter to be what sendmail expects.
      (goto-char (point-min))
      (re-search-forward
	(concat "^" (regexp-quote mail-header-separator) "\n"))
      (replace-match "\n")
      (backward-char 1)
      (setq delimline (point-marker))
      (if mail-aliases
	  (expand-mail-aliases (point-min) delimline))
      (goto-char (point-min))
      ;; ignore any blank lines in the header
      (while (and (re-search-forward "\n\n\n*" delimline t)
		  (< (point) delimline))
	(replace-match "\n"))
      (let ((case-fold-search t))
	(goto-char (point-min))
	;; Find and handle any FCC fields.
	(goto-char (point-min))
	(if (re-search-forward "^FCC:" delimline t)
	    (mail-do-fcc delimline))
	(goto-char (point-min))
	;; "S:" is an abbreviation for "Subject:".
	(goto-char (point-min))
	(if (re-search-forward "^S:" delimline t)
	    (replace-match "Subject:"))
	;; Don't send out a blank subject line
	(goto-char (point-min))
	(if (re-search-forward "^Subject:[ \t]*\n" delimline t)
	    (replace-match ""))
	;; Insert an extra newline if we need it to work around
	;; Sun's bug that swallows newlines.
	(goto-char (1+ delimline))
	(if (eval mail-mailer-swallows-blank-line)
	    (newline)))
      (gnus-soup-store 
       nnsoup-replies-directory 
       (nnsoup-kind-to-prefix kind) nil nnsoup-replies-format-type
       nnsoup-replies-index-type)
      (kill-buffer tembuf))))

(defun nnsoup-kind-to-prefix (kind)
  (or nnsoup-replies-list
      (setq nnsoup-replies-list
	    (gnus-soup-parse-replies 
	     (concat nnsoup-replies-directory "REPLIES"))))
  (let ((replies nnsoup-replies-list))
    (while (and replies 
		(not (string= kind (gnus-soup-reply-kind (car replies)))))
      (setq replies (cdr replies)))
    (if replies
	(gnus-soup-reply-prefix (car replies))
      (setq nnsoup-replies-list
	    (cons (vector (gnus-soup-unique-prefix nnsoup-replies-directory)
			  kind 
			  (format "%c%c%c"
				  nnsoup-replies-format-type
				  nnsoup-replies-index-type
				  (if (string= kind "news")
				      ?n ?m)))
		  nnsoup-replies-list))
      (gnus-soup-reply-prefix (car nnsoup-replies-list)))))
	
(provide 'nnsoup)

;;; nnsoup.el ends here
