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
  "*The number of the articles which indicates a large newsgroup.
If the number of the articles is greater than the value, verbose
messages will be shown to indicate the current status.")



(defvar nnvirtual-newsgroups nil
  "The newsgroups that belong to this virtual newsgroup.")

(defvar nnvirtual-newsgroups-regexp nil
  "The newsgroups that belong to this virtual newsgroup.")

(defvar nnvirtual-mapping nil)

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
	  (nntp-xover-is-evil t)
	  (i 0)
	  prev articles group-articles beg art-info article group)
      (if sequence (setq prev (car (aref nnvirtual-mapping (car sequence)))))
      (while sequence
	(setq art-info (aref nnvirtual-mapping (car sequence)))
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
	      (while (/= article (cdr (aref nnvirtual-mapping i)))
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
      ;; Weed out articles that appear twice because of cross-posting.
      ;; Suggested by Stephane Laveau <laveau@corse.inria.fr>.
      (let ((id-hashtb (make-vector number 0))
	    id)
	(goto-char (point-min))
	;; We look at the message-ids...
	(while (search-forward "\nMessage-ID: " nil t)
	  ;; ... and check if they are entered into the hash table.
	  (if (boundp (setq id (intern (buffer-substring 
					(point) (progn (end-of-line) (point)))
				       id-hashtb)))
	      ;; Yup, so we delete this header.
	      (delete-region
	       (if (search-backward "\n.\n" nil t) (1+ (point)) (point-min))
	       (if (search-forward "\n.\n" nil t) (1+ (match-beginning 0))
		 (point-max))))
	  ;; Nope, so we just enter it into the hash table.
	  (set id t)))
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
  (let ((newsrc gnus-newsrc-assoc))
    (setq nnvirtual-newsgroups nil)
    (setq nnvirtual-newsgroups-regexp newsgroups)
    (while newsrc
      (if (string-match newsgroups (car (car newsrc)))
	  (setq nnvirtual-newsgroups (cons (car (car newsrc)) 
					   nnvirtual-newsgroups)))
      (setq newsrc (cdr newsrc)))
    (if (null nnvirtual-newsgroups)
	(setq nnvirtual-status-string 
	      (format 
	       "nnvirtual: No newsgroups for this virtual newsgroup"))
      (nnvirtual-open-server-internal))
    nnvirtual-newsgroups))

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
    (setq art (aref nnvirtual-mapping id))
    (gnus-request-group (car art))
    (gnus-request-article (cdr art) (car art) buffer)))

(defun nnvirtual-request-group (group &optional server dont-check)
  "Make GROUP the current newsgroup."
  (nnvirtual-possibly-change-newsgroups nil server)
  (let* ((group (concat gnus-foreign-group-prefix group))
	 (info (nth 2 (gnus-gethash group gnus-newsrc-hashtb)))
	 (groups nnvirtual-newsgroups)
	 (i 0)
	 (total 0)
	 unread igroup)
    (if (not info)
	(error "No such group: %s" group))
    (setcar (nthcdr 2 info) nil)
    (while groups
      (setq unread (car (gnus-gethash (car groups) gnus-newsrc-hashtb)))
      (if (numberp unread) (setq total (+ total unread)))
      (setq groups (cdr groups)))
    (setq nnvirtual-mapping (make-vector (+ 3 total) nil))
    (setq groups nnvirtual-newsgroups)
    (while groups
      (setq igroup (car groups))
      (setq unread (gnus-list-of-unread-articles igroup))
      (while unread
	(aset nnvirtual-mapping (setq i (1+ i)) (cons igroup (car unread)))
	(setq unread (cdr unread)))
      (setq groups (cdr groups)))
    (save-excursion
      (set-buffer nntp-server-buffer)
      (erase-buffer)
      (insert (format "211 %d %d %d %s\n" (1+ total) 1 total group)))
    t))

(defun nnvirtual-request-list (&optional server) 
  "List active newsgoups."
  (setq nnvirtual-status-string "nnvirtual: LIST is not implemented.")
  nil)

(defun nnvirtual-request-list-newsgroups (&optional server)
  "List newsgroups (defined in NNTP2)."
  (setq nnvirtual-status-string "nnvirtual: LIST NEWSGROUPS is not implemented.")
  nil)

(fset 'nnvirtual-request-post 'nntp-request-post)

(fset 'nnvirtual-request-post-buffer 'nntp-request-post-buffer)


;;; Low-Level Interface

(defun nnvirtual-open-server-internal ()
  "Fix some internal variables."
  (save-excursion
    ;; Initialize communicatin buffer.
    (setq nnvirtual-mapping nil)
    (setq nntp-server-buffer (get-buffer-create " *nntpd*"))
    (set-buffer nntp-server-buffer)
    (buffer-disable-undo (current-buffer))
    (kill-all-local-variables)
    (setq case-fold-search t)))

(defun nnvirtual-close-server-internal (&rest dum)
  "Close connection to news server."
  nil)

(defun nnvirtual-possibly-change-newsgroups (group groups-regexp)
  (if (and groups-regexp
	   (not (and nnvirtual-newsgroups-regexp
		     (string= groups-regexp nnvirtual-newsgroups-regexp))))
      (nnvirtual-open-server groups-regexp)))

(provide 'nnvirtual)

;;; nnvirtual.el ends here
