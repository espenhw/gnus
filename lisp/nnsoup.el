;;; nnsoup.el --- SOUP access for Gnus
;; Copyright (C) 1995,96 Free Software Foundation, Inc.

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
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(require 'nnheader)
(require 'nnmail)
(require 'gnus-soup)
(require 'gnus-msg)
(eval-when-compile (require 'cl))

(defvar nnsoup-directory "~/SOUP/"
  "*SOUP packet directory.")

(defvar nnsoup-tmp-directory "/tmp/"
  "*Where nnsoup will store temporary files.")

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
(defvar nnsoup-current-prefix 0)
(defvar nnsoup-replies-list nil)
(defvar nnsoup-buffers nil)
(defvar nnsoup-current-group nil)



;; Server variables.

(defvar nnsoup-current-server nil)
(defvar nnsoup-server-alist nil)
(defvar nnsoup-server-variables 
  `((nnsoup-directory ,nnsoup-directory)
    (nnsoup-active-file ,nnsoup-active-file)
    (nnsoup-status-string "")
    (nnsoup-current-prefix 0)
    (nnsoup-group-alist nil)))



;;; Interface functions.

(defun nnsoup-retrieve-headers (sequence &optional group server fetch-old)
  (nnsoup-possibly-change-group group)
  (save-excursion
    (set-buffer nntp-server-buffer)
    (erase-buffer)
    (let ((areas (cddr (assoc nnsoup-current-group nnsoup-group-alist)))
	  (articles sequence)
	  (use-nov t)
	  useful-areas this-area-seq msg-buf)
      (if (stringp (car sequence))
	  ;; We don't support fetching by Message-ID.
	  'headers
	;; We go through all the areas and find which files the
	;; articles in SEQUENCE come from.
	(while (and areas sequence)
	  ;; Peel off areas that are below sequence.
	  (while (and areas (< (cdr (car (car areas))) (car sequence)))
	    (setq areas (cdr areas)))
	  (when areas
	    ;; This is a useful area.
	    (push (car areas) useful-areas)
	    (setq this-area-seq nil)
	    ;; We take note whether this MSG has a corresponding IDX
	    ;; for later use.
	    (when (or (= (gnus-soup-encoding-index 
			  (gnus-soup-area-encoding (nth 1 (car areas)))) ?n)
		      (not (file-exists-p
			    (nnsoup-file
			     (gnus-soup-area-prefix (nth 1 (car areas)))))))
	      (setq use-nov nil))
	    ;; We assign the portion of `sequence' that is relevant to
	    ;; this MSG packet to this packet.
	    (while (and sequence (<= (car sequence) (cdr (car (car areas)))))
	      (push (car sequence) this-area-seq)
	      (setq sequence (cdr sequence)))
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
	    ;; We have IDX files for all areas.
	    (progn
	      (while useful-areas
		(goto-char (point-max))
		(let ((b (point))
		      (number (car (nth 1 (car useful-areas))))
		      (index-buffer (nnsoup-index-buffer
				     (gnus-soup-area-prefix
				      (nth 2 (car useful-areas))))))
		  (when index-buffer
		    (insert-buffer-substring index-buffer)
		    (goto-char b)
		    ;; We have to remove the index number entires and
		    ;; insert article numbers instead.
		    (while (looking-at "[0-9]+")
		      (replace-match (int-to-string number) t t)
		      (incf number)
		      (forward-line 1))))
		(setq useful-areas (cdr useful-areas)))
	      'nov)
	  ;; We insert HEADs.
	  (while useful-areas
	    (setq articles (car (car useful-areas))
		  useful-areas (cdr useful-areas))
	    (while articles
	      (when (setq msg-buf
			  (nnsoup-narrow-to-article 
			   (car articles) (cdr (car useful-areas)) 'head))
		(goto-char (point-max))
		(insert (format "221 %d Article retrieved.\n" (car articles)))
		(insert-buffer-substring msg-buf)
		(goto-char (point-max))
		(insert ".\n"))
	      (setq articles (cdr articles))))

	  (nnheader-fold-continuation-lines)
	  'headers)))))

(defun nnsoup-open-server (server &optional defs)
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
    (setq nnsoup-current-server server))
  (nnsoup-read-active-file))

(defun nnsoup-request-close ()
  (nnsoup-write-active-file)
  (nnsoup-write-replies)
  (gnus-soup-save-areas)
  ;; Kill all nnsoup buffers.
  (let (buffer)
    (while nnsoup-buffers
      (setq buffer (cdr (pop nnsoup-buffers)))
      (and buffer
	   (buffer-name buffer)
	   (kill-buffer buffer))))
  (setq nnsoup-group-alist nil
	nnsoup-current-group nil
	nnsoup-current-server nil
	nnsoup-server-alist nil
	nnsoup-replies-list nil)
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
      t
    (let ((active (cadr (assoc group nnsoup-group-alist))))
      (if (not active)
	  (nnheader-report 'nnsoup "No such group: %s" group)
	(nnheader-insert 
	 "211 %d %d %d %s\n" 
	 (max (1+ (- (cdr active) (car active))) 0) 
	 (car active) (cdr active) group)))))

(defun nnsoup-request-type (group &optional article)
  (nnsoup-possibly-change-group group)
  (if (not article)
      'unknown
    (let ((kind (gnus-soup-encoding-kind 
		 (gnus-soup-area-encoding
		  (nth 1 (nnsoup-article-to-area
			  article nnsoup-current-group))))))
      (cond ((= kind ?m) 'mail)
	    ((= kind ?n) 'news)
	    (t 'unknown)))))

(defun nnsoup-close-group (group &optional server)
  ;; Kill all nnsoup buffers.
  (let ((buffers nnsoup-buffers)
	elem)
    (while buffers
      (when (equal (car (setq elem (pop buffers))) group)
	(setq nnsoup-buffers (delq elem nnsoup-buffers))
	(and (cdr elem) (buffer-name (cdr elem))
	     (kill-buffer (cdr elem))))))
  t)

(defun nnsoup-request-list (&optional server)
  (save-excursion
    (set-buffer nntp-server-buffer)
    (erase-buffer)
    (let ((alist nnsoup-group-alist)
	  entry)
      (while (setq entry (pop alist))
	(insert (format "%s %d %d y\n" (car entry)
			(cdadr entry) (caadr entry))))
      t)))

(defun nnsoup-request-scan (group &optional server)
  (nnsoup-unpack-packets))

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

(defun nnsoup-request-expire-articles (articles group &optional server force)
  (nnsoup-possibly-change-group group)
  (let* ((total-infolist (assoc group nnsoup-group-alist))
	 (active (cadr total-infolist))
	 (infolist (cddr total-infolist))
	 info range-list mod-time prefix)
    (while infolist
      (setq info (pop infolist)
	    range-list (gnus-uncompress-range (car info))
	    prefix (gnus-soup-area-prefix (nth 1 info)))
      (when ;; All the articles in this file are marked for expiry.
	  (and (or (setq mod-time (nth 5 (file-attributes
					  (nnsoup-file prefix))))
		   (setq mod-time (nth 5 (file-attributes
					  (nnsoup-file prefix t)))))
	       (gnus-sublist-p articles range-list)
	       ;; This file is old enough. 
	       (nnmail-expired-article-p group mod-time force))
	;; Ok, we delete this file.
	(when (condition-case nil
		  (progn
		    (nnheader-message 
		     5 "Deleting %s..." (nnsoup-file prefix))
		    (when (file-exists-p (nnsoup-file prefix))
		      (delete-file (nnsoup-file prefix)))
		    (nnheader-message 
		     5 "Deleting %s..." (nnsoup-file prefix t))
		    (when (file-exists-p (nnsoup-file prefix t))
		      (delete-file (nnsoup-file prefix t)))
		    t)
		(error nil))
	  (setcdr (cdr total-infolist) (delq info (cddr total-infolist)))
	  (setq articles (gnus-sorted-complement articles range-list))))
      (when (not mod-time)
	(setcdr (cdr total-infolist) (delq info (cddr total-infolist)))))
    (if (cddr total-infolist)
	(setcar active (car (car (car (cdr (cdr total-infolist))))))
      (setcar active (1+ (cdr active))))
    (nnsoup-write-active-file)
    ;; Return the articles that weren't expired.
    articles))


;;; Internal functions

(defun nnsoup-possibly-change-group (group &optional force)
  (if group
      (setq nnsoup-current-group group)
    t))

(defun nnsoup-read-active-file ()
  (setq nnsoup-group-alist)
  (when (file-exists-p nnsoup-active-file)
    (condition-case ()
	(load nnsoup-active-file)
      (error nil))
    ;; Be backwards compatible.
    (when (and nnsoup-group-alist
	       (not (atom (caadar nnsoup-group-alist))))
      (let ((alist nnsoup-group-alist)
	    entry e min max)
	(while (setq e (cdr (setq entry (pop alist))))
	  (setq min (caaar e))
	  (while (cdr e)
	    (setq e (cdr e)))
	  (setq max (cdaar e))
	  (setcdr entry (cons (cons min max) (cdr entry))))))
    nnsoup-group-alist))

(defun nnsoup-write-active-file ()
  (when nnsoup-group-alist
    (save-excursion
      (set-buffer (get-buffer-create " *nnsoup work*"))
      (buffer-disable-undo (current-buffer))
      (erase-buffer)
      (insert (format "(setq nnsoup-group-alist '%S)\n" nnsoup-group-alist))
      (insert (format "(setq nnsoup-current-prefix %d)\n"
		      nnsoup-current-prefix))
      (write-region (point-min) (point-max) nnsoup-active-file
		    nil 'silent)
      (kill-buffer (current-buffer)))))

(defun nnsoup-next-prefix ()
  "Return the next free prefix."
  (let (prefix)
    (while (or (file-exists-p 
		(nnsoup-file (setq prefix (int-to-string
					   nnsoup-current-prefix))))
	       (file-exists-p (nnsoup-file prefix t)))
      (incf nnsoup-current-prefix))
    (incf nnsoup-current-prefix)
    prefix))

(defun nnsoup-read-areas ()
  (save-excursion
    (set-buffer nntp-server-buffer)
    (let ((areas (gnus-soup-parse-areas (concat nnsoup-tmp-directory "AREAS")))
	  entry number area lnum cur-prefix file)
      ;; Go through all areas in the new AREAS file.
      (while (setq area (pop areas))
	;; Change the name to the permanent name and move the files.
	(setq cur-prefix (nnsoup-next-prefix))
	(message "Incorporating file %s..." cur-prefix)
	(when (file-exists-p 
	       (setq file (concat nnsoup-tmp-directory
				  (gnus-soup-area-prefix area) ".IDX")))
	  (rename-file file (nnsoup-file cur-prefix)))
	(when (file-exists-p 
	       (setq file (concat nnsoup-tmp-directory 
				  (gnus-soup-area-prefix area) ".MSG")))
	  (rename-file file (nnsoup-file cur-prefix t)))
	(gnus-soup-set-area-prefix area cur-prefix)
	;; Find the number of new articles in this area.
	(setq number (nnsoup-number-of-articles area))
	(if (not (setq entry (assoc (gnus-soup-area-name area)
				    nnsoup-group-alist)))
	    ;; If this is a new area (group), we just add this info to
	    ;; the group alist. 
	    (push (list (gnus-soup-area-name area)
			(cons 1 number)
			(list (cons 1 number) area))
		  nnsoup-group-alist)
	  ;; There are already articles in this group, so we add this
	  ;; info to the end of the entry.
	  (nconc entry (list (list (cons (1+ (setq lnum (cdadr entry)))
					 (+ lnum number))
				   area)))
	  (setcdr (cadr entry) (+ lnum number)))))
    (nnsoup-write-active-file)))

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
	(when (file-exists-p (concat nnsoup-directory file))
	  (save-excursion			; Load the file.
	    (set-buffer (get-buffer-create buffer-name))
	    (buffer-disable-undo (current-buffer))
	    (push (cons nnsoup-current-group (current-buffer)) nnsoup-buffers)
	    (insert-file-contents (concat nnsoup-directory file))
	    (current-buffer))))))

(defun nnsoup-file (prefix &optional message)
  (expand-file-name
   (concat nnsoup-directory prefix (if message ".MSG" ".IDX"))))

(defun nnsoup-message-buffer (prefix)
  (nnsoup-index-buffer prefix 'msg))

(defun nnsoup-unpack-packets ()
  "Unpack all packets in `nnsoup-packet-directory'."
  (let ((packets (directory-files
		  nnsoup-packet-directory t nnsoup-packet-regexp))
	packet msg)
    (while (setq packet (pop packets))
      (message (setq msg (format "nnsoup: unpacking %s..." packet)))
      (if (not (gnus-soup-unpack-packet 
		nnsoup-tmp-directory nnsoup-unpacker packet))
	  (message "Couldn't unpack %s" packet)
	(delete-file packet)
	(nnsoup-read-areas)
	(message "%sdone" msg)))))

(defun nnsoup-narrow-to-article (article &optional area head)
  (let* ((area (or area (nnsoup-article-to-area article nnsoup-current-group)))
	 (prefix (gnus-soup-area-prefix (nth 1 area)))
	 (msg-buf (nnsoup-index-buffer prefix 'msg))
	 beg end)
    (save-excursion
      (cond
       ;; There is no MSG file.
       ((null msg-buf)
	nil)
       
       ;; We use the index file to find out where the article begins and ends. 
       ((and (= (gnus-soup-encoding-index 
		 (gnus-soup-area-encoding (nth 1 area)))
		?c)
	     (file-exists-p (nnsoup-file prefix)))
	(set-buffer (nnsoup-index-buffer prefix))
	(widen)
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
		    (setq end (progn (forward-line -1) (point))))))
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

;;;###autoload
(defun nnsoup-pack-replies ()
  "Make an outbound package of SOUP replies."
  (interactive)
  ;; Write all data buffers.
  (gnus-soup-save-areas)
  ;; Write the active file.
  (nnsoup-write-active-file)
  ;; Write the REPLIES file.
  (nnsoup-write-replies)
  ;; Pack all these files into a SOUP packet.
  (gnus-soup-pack nnsoup-replies-directory nnsoup-packer))

(defun nnsoup-write-replies ()
  "Write the REPLIES file."
  (when nnsoup-replies-list
    (gnus-soup-write-replies nnsoup-replies-directory nnsoup-replies-list)
    (setq nnsoup-replies-list nil)))

(defun nnsoup-article-to-area (article group)
  "Return the area that ARTICLE in GROUP is located in."
  (let ((areas (cddr (assoc group nnsoup-group-alist))))
    (while (and areas (< (cdr (car (car areas))) article))
      (setq areas (cdr areas)))
    (and areas (car areas))))

(defvar nnsoup-old-functions
  (list gnus-inews-article-function send-mail-function))

;;;###autoload
(defun nnsoup-set-variables ()
  "Use the SOUP methods for posting news and mailing mail."
  (interactive)
  (setq gnus-inews-article-function 'nnsoup-request-post)
  (setq send-mail-function 'nnsoup-request-mail))

;;;###autoload
(defun nnsoup-revert-variables ()
  "Revert posting and mailing methods to the standard Emacs methods."
  (interactive)
  (setq gnus-inews-article-function (car nnsoup-old-functions))
  (setq send-mail-function (cadr nnsoup-old-functions)))

(defun nnsoup-store-reply (kind)
  ;; Mostly stolen from `sendmail.el'.
  (let ((tembuf (generate-new-buffer " sendmail temp"))
	(case-fold-search nil)
	(mailbuf (current-buffer))
	delimline)
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
      (if (re-search-forward
	   (concat "^" (regexp-quote mail-header-separator) "\n") nil t)
	  (replace-match "\n")
	(search-forward "\n\n" nil t))
      (backward-char 1)
      (setq delimline (point-marker))
      (if mail-aliases (expand-mail-aliases (point-min) delimline))
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
      (let ((msg-buf
	     (gnus-soup-store 
	      nnsoup-replies-directory 
	      (nnsoup-kind-to-prefix kind) nil nnsoup-replies-format-type
	      nnsoup-replies-index-type))
	    (num 0))
	(when (and msg-buf (bufferp msg-buf))
	  (save-excursion
	    (set-buffer msg-buf)
	    (goto-char (point-min))
	    (while (re-search-forward "^#! *rnews" nil t)
	      (incf num)))
	  (message "Stored %d messages" num)))
      (nnsoup-write-replies)
      (kill-buffer tembuf))))

(defun nnsoup-kind-to-prefix (kind)
  (unless nnsoup-replies-list
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

(defun nnsoup-make-active ()
  "(Re-)create the SOUP active file."
  (interactive)
  (let ((files (sort (directory-files nnsoup-directory t "IDX$")
		     (lambda (f1 f2)
		       (< (progn (string-match "/\\([0-9]+\\)\\." f1)
				 (string-to-int (match-string 1 f1)))
			  (progn (string-match "/\\([0-9]+\\)\\." f2)
				 (string-to-int (match-string 1 f2)))))))
	active group lines ident elem min)
    (set-buffer (get-buffer-create " *nnsoup work*"))
    (buffer-disable-undo (current-buffer))
    (while files
      (message "Doing %s..." (car files))
      (erase-buffer)
      (insert-file-contents (car files))
      (goto-char (point-min))
      (end-of-line)
      (re-search-backward "[ \t]\\([^ ]+\\):[0-9]")
      (setq group (buffer-substring (match-beginning 1) (match-end 1)))
      (setq lines (count-lines (point-min) (point-max)))
      (setq ident (progn (string-match
			  "/\\([0-9]+\\)\\." (car files))
			 (substring 
			  (car files) (match-beginning 1)
			  (match-end 1))))
      (if (not (setq elem (assoc group active)))
	  (push (list group (cons 1 lines)
		      (list (cons 1 lines) 
			    (vector ident group "ncm" "" lines)))
		active)
	(nconc elem
	       (list
		(list (cons (setq min (1+ (cdr (car (car
						     (cdr elem))))))
			    (+ min lines))
		      (vector ident group "ncm" "" lines))))
	(setcdr (cadr elem) (+ min lines)))
      (setq files (cdr files)))
    (message "")
    (setq nnsoup-group-alist active)
    (while active
      (setcdr (car active) (nreverse (cdr (car active))))
      (setq active (cdr active)))
    (nnsoup-write-active-file)))

(defun nnsoup-delete-unreferenced-message-files ()
  "Delete any *.MSG and *.IDX files that aren't known by nnsoup."
  (interactive)
  (let* ((known (apply 'nconc (mapcar 
			       (lambda (ga)
				 (mapcar
				  (lambda (area)
				    (gnus-soup-area-prefix (cadr area)))
				  (cddr ga)))
			       nnsoup-group-alist)))
	 (regexp "\\.MSG$\\|\\.IDX$")
	 (files (directory-files nnsoup-directory nil regexp))
	 non-files file)
    ;; Find all files that aren't known by nnsoup.
    (while (setq file (pop files))
      (string-match regexp file)
      (unless (member (substring file 0 (match-beginning 0)) known)
	(push file non-files)))
    ;; Sort and delete the files.
    (setq non-files (sort non-files 'string<))
    (map-y-or-n-p "Delete file %s? "
		  (lambda (file) (delete-file (concat nnsoup-directory file)))
		  non-files)))

(provide 'nnsoup)

;;; nnsoup.el ends here
