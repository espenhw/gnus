;;; nnultimate.el --- interfacing with the Ultimate Bulletin Board
;; Copyright (C) 1999 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
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
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Note: You need to have `url' and `w3' installed for this
;; backend to work.

;;; Code:

(eval-when-compile (require 'cl))

(require 'nnoo)
(require 'message)
(require 'gnus-util)
(require 'gnus)
(require 'nnmail)
(require 'mm-util)
(require 'nnweb)
(eval-when-compile
  (ignore-errors
    (require 'w3)
    (require 'url)
    (require 'w3-forms)))
;; Report failure to find w3 at load time if appropriate.
(eval '(progn
	 (require 'w3)
	 (require 'url)
	 (require 'w3-forms)))

(nnoo-declare nnultimate)

(defvoo nnultimate-directory (nnheader-concat gnus-directory "ultimate/")
  "Where nnultimate will save its files.")

(defvoo nnultimate-address-1 "http://debet.solbors.no/cgi-bin/billboard/"
  "The address of the Ultimate bulletin board.")

;;; Internal variables

(defvar nnultimate-groups nil)
(defvar nnultimate-buffer nil)
(defvar nnultimate-headers nil)
(defvar nnultimate-articles nil)

;;; Interface functions

(nnoo-define-basics nnultimate)

(deffoo nnultimate-retrieve-headers (articles &optional group server fetch-old)
  (nnultimate-possibly-change-server group server)
  (let* ((last (car (last articles)))
	 (did nil)
	 (start 1)
	 (entry (gnus-copy-sequence (assoc group nnultimate-groups)))
	 (sid (nth 2 entry))
	 (topics (nth 4 entry))
	 (mapping (nth 5 entry))
	 (old-total (or (nth 6 entry) 0))
	 (furl "forumdisplay.cgi?action=topics&number=%d&DaysPrune=1000")
	 (turl "http://debet.solbors.no/billboard/Forum%d/HTML/%06d.html")
	 headers article subject score from date lines parent point
	 contents tinfo fetchers map elem)
    (save-excursion
      (set-buffer nnultimate-buffer)
      (erase-buffer)
      (url-insert-file-contents
       (concat nnultimate-address-1 (format furl sid)))
      (setq buffer-file-name nil)
      (goto-char (point-min))
      (setq contents (nth 2 (car (nth 2
				      (nnultimate-find-forum-table
				       (w3-parse-buffer (current-buffer)))))))
      ;; The main idea here is to map Gnus article numbers to
      ;; nnultimate article numbers.  Say there are three topics in
      ;; this forum, the first with 4 articles, the seconds with 2,
      ;; and the third with 1.  Then this will translate into 7 Gnus
      ;; article numbers, where 1-4 comes from the first topic, 5-6
      ;; from the second and 7 from the third.  Now, then next time
      ;; the group is entered, there's 2 new articles in topic one and
      ;; 1 in topic three.  Then Gnus article number 8-9 be 5-6 in
      ;; topic one and 10 will be the 2 in topic three.
      (dolist (row (cdr contents))
	(setq row (nth 2 row))
	(when (setq a (nnultimate-descend 'a (nth 2 row)))
	  (setq subject (car (last (nnultimate-text a)))
		href (cdr (assq 'href (nth 1 a))))
	  (setq garticles (1+ (string-to-number (car (last (nnultimate-text
							    (nth 4 row)))))))
	  (string-match "/\\([0-9]+\\).html" href)
	  (setq topic (string-to-number (match-string 1 href)))
	  (if (setq tinfo (assq topic topics))
	      (progn
		(setq old-max (cadr tinfo))
		(setcar (cdr tinfo) garticles))
	    (setq old-max 0)
	    (push (list topic garticles subject) topics)
	    (setcar (nthcdr 4 entry) topics))
	  (when (not (= old-max garticles))
	    (setq inc (- garticles old-max))
	    (setq mapping (nconc mapping
				 (list
				  (list
				   (setq old-total (+ old-total inc))
				   topic (1+ old-max)))))
	    (incf old-max inc)
	    (setcar (nthcdr 5 entry) mapping))))
      (setq map mapping)
      (while (and (setq article (car articles))
		  map)
	(while (and map
		    (> article (caar map)))
	  (pop map))
	(while (and article
		    map
		    (<= article (caar map)))
	  (if (setq elem (assq (cadar map) fetchers))
	      (nconc elem (list (cons article
				      (+ (caddar map)
					 (- (caar map) article)))))
	    (push (list (cadar map) (cons article
					  (+ (caddar map)
					     (- (caar map) article))))
		  fetchers))
	  (setq article (car (setq articles (cdr articles))))))
      ;; Now we have the mapping from/to Gnus/nnultimate article numbers,
      ;; so we start fetching the topics that we need to satisfy the
      ;; request.
      (if (not fetchers)
	  (save-excursion
	    (set-buffer nntp-server-buffer)
	    (erase-buffer))
	(setq nnultimate-articles nil)
	(with-temp-buffer
	  (dolist (elem fetchers)
	    (erase-buffer)
	    (setq subject (nth 2 (assq (car elem) topics)))
	    (url-insert-file-contents (format turl sid (car elem)))
	    (setq buffer-file-name nil)
	    (goto-char (point-min))
	    (setq contents
		  (cdr
		   (nth 2 (car (nth 2
				    (nnultimate-find-forum-table
				     (w3-parse-buffer (current-buffer))))))))
	    (dolist (art (cdr elem))
	      (push (list (car art)
			  (nth (1- (cdr art)) contents)
			  subject)
		    nnultimate-articles))))
	(setq nnultimate-articles
	      (sort nnultimate-articles 'car-less-than-car))
	;; Now we have all the articles, conveniently in an alist
	;; where the key is the Gnus article number.
	(dolist (articlef nnultimate-articles)
	  (setq article (nth 0 articlef)
		contents (nth 1 articlef)
		subject (nth 2 articlef))
	  (setq from (mapconcat 'identity
				(nnultimate-text (car (nth 2 contents)))
				" ")
		datel (nnultimate-text (nth 2 (car (cdr (nth 2 contents))))))
	  (while datel
	    (when (string-match "Posted" (car datel))
	      (setq date (substring (car datel) (match-end 0))
		    datel nil))
	    (pop datel))
	  (setq date (delete "" (split-string date "[- \n\t\r    ]")))
	  (setq date (format "%s %s %s %s"
			     (car (rassq (string-to-number (nth 1 date))
					 parse-time-months))
			     (nth 0 date) (nth 2 date) (nth 3 date)))
	  (push
	   (cons
	    article
	    (make-full-mail-header
	     article subject
	     from (or date "")
	     (concat "<" (number-to-string sid) "%"
		     (number-to-string article) 
		     "@ultimate>")
	     "" 0 0 nil nil))
	   headers))
	(setq nnultimate-headers (sort headers 'car-less-than-car))
	(save-excursion
	  (set-buffer nntp-server-buffer)
	  (erase-buffer)
	  (dolist (header nnultimate-headers)
	    (nnheader-insert-nov (cdr header))))))
    'nov))

(deffoo nnultimate-request-group (group &optional server dont-check)
  (nnultimate-possibly-change-server nil server)
  (let ((elem (assoc group nnultimate-groups)))
    (cond
     ((not elem)
      (nnheader-report 'nnultimate "Group does not exist"))
     (t
      (nnheader-report 'nnultimate "Opened group %s" group)
      (nnheader-insert
       "211 %d %d %d %s\n" (cadr elem) 1 (cadr elem)
       (prin1-to-string group))))))

(deffoo nnultimate-close-group (group &optional server)
  (nnultimate-possibly-change-server group server)
  (when (gnus-buffer-live-p nnultimate-buffer)
    (save-excursion
      (set-buffer nnultimate-buffer)
      (kill-buffer nnultimate-buffer)))
  t)

(deffoo nnultimate-request-article (article &optional group server buffer)
  (nnultimate-possibly-change-server group server)
  (let ((contents (cdr (assq article nnultimate-articles))))
    (setq contents (cdr (nth 2 (nth 1 (nth 2 (car contents))))))
    (when contents
      (save-excursion
	(set-buffer (or buffer nntp-server-buffer))
	(erase-buffer)
	(apply 'insert (nnultimate-text contents))
	(goto-char (point-min))
	(insert "Content-Type: text/html\nMIME-Version: 1.0\n")
	(let ((header (cdr (assq article nnultimate-headers))))
	  (nnheader-insert-header header))
	(nnheader-report 'nnultimate "Fetched article %s" article)
	(cons group article)))))

(deffoo nnultimate-close-server (&optional server)
  (when (and (nnultimate-server-opened server)
	     (gnus-buffer-live-p nnultimate-buffer))
    (save-excursion
      (set-buffer nnultimate-buffer)
      (kill-buffer nnultimate-buffer)))
  (nnoo-close-server 'nnultimate server))

(deffoo nnultimate-request-list (&optional server)
  (nnultimate-possibly-change-server nil server)
  (with-temp-buffer
    (url-insert-file-contents (concat nnultimate-address-1 "Ultimate.cgi"))
    (setq buffer-file-name nil)
    (let ((contents (nth 2 (car (nth 2
				     (nnultimate-find-forum-table
				      (w3-parse-buffer (current-buffer)))))))
	  sid elem description articles a href group)
      (dolist (row contents)
	(setq row (nth 2 row))
	(when (setq a (nnultimate-descend 'a row))
	  (setq group (car (last (nnultimate-text a)))
		href (cdr (assq 'href (nth 1 a))))
	  (setq description (car (last (nnultimate-text (nth 1 row)))))
	  (setq articles (string-to-number (car (last (nnultimate-text
						       (nth 2 row))))))
	  (when href
	    (string-match "number=\\([0-9]+\\)" href)
	    (setq forum (string-to-number (match-string 1 href)))
	    (if (setq elem (assoc group nnultimate-groups))
		(setcar (cdr elem) articles)
	      (push (list group articles forum description nil nil nil)
		    nnultimate-groups))))))
    (nnultimate-write-groups)
    (nnultimate-generate-active)
    t))

(deffoo nnultimate-request-newgroups (date &optional server)
  (nnultimate-possibly-change-server nil server)
  (nnultimate-generate-active)
  t)

(deffoo nnultimate-asynchronous-p ()
  nil)

(nnoo-define-skeleton nnultimate)

;;; Internal functions

(defun nnultimate-possibly-change-server (&optional group server)
  (nnultimate-init server)
  (when (and server
	     (not (nnultimate-server-opened server)))
    (nnultimate-open-server server))
  (unless nnultimate-groups
    (nnultimate-read-groups)))

(defun nnultimate-read-groups ()
  (let ((file (expand-file-name "groups" nnultimate-directory)))
    (when (file-exists-p file)
      (with-temp-buffer
	(insert-file-contents file)
	(goto-char (point-min))
	(setq nnultimate-groups (read (current-buffer)))))))

(defun nnultimate-write-groups ()
  (with-temp-file (expand-file-name "groups" nnultimate-directory)
    (prin1 nnultimate-groups (current-buffer))))
    
(defun nnultimate-init (server)
  "Initialize buffers and such."
  (unless (file-exists-p nnultimate-directory)
    (gnus-make-directory nnultimate-directory))
  (unless (gnus-buffer-live-p nnultimate-buffer)
    (setq nnultimate-buffer
	  (save-excursion
	    (nnheader-set-temp-buffer
	     (format " *nnultimate %s*" server))))))

(defun nnultimate-encode-www-form-urlencoded (pairs)
  "Return PAIRS encoded for forms."
  (mapconcat
   (function
    (lambda (data)
      (concat (w3-form-encode-xwfu (car data)) "="
	      (w3-form-encode-xwfu (cdr data)))))
   pairs "&"))

(defun nnultimate-fetch-form (url pairs)
  (let ((url-request-data (nnultimate-encode-www-form-urlencoded pairs))
	(url-request-method "POST")
	(url-request-extra-headers
	 '(("Content-type" . "application/x-www-form-urlencoded"))))
    (url-insert-file-contents url)
    (setq buffer-file-name nil))
  t)

(defun nnultimate-decode-entities ()
  (goto-char (point-min))
  (while (re-search-forward "&\\([a-z]+\\);" nil t)
    (replace-match (char-to-string (or (cdr (assq (intern (match-string 1))
						  w3-html-entities))
				       ?#))
		   t t)))

(defun nnultimate-remove-markup ()
  (goto-char (point-min))
  (while (search-forward "<!--" nil t)
    (delete-region (match-beginning 0)
		   (or (search-forward "-->" nil t)
		       (point-max))))
  (goto-char (point-min))
  (while (re-search-forward "<[^>]+>" nil t)
    (replace-match "" t t)))

(defun nnultimate-date-to-date (sdate)
  (let ((elem (split-string sdate)))
    (concat (substring (nth 0 elem) 0 3) " "
	    (substring (nth 1 elem) 0 3) " "
	    (substring (nth 2 elem) 0 2) " "
	    (substring (nth 3 elem) 1 6) " "
	    (format-time-string "%Y") " "
	    (nth 4 elem))))

(defun nnultimate-generate-active ()
  (save-excursion
    (set-buffer nntp-server-buffer)
    (erase-buffer)
    (dolist (elem nnultimate-groups)
      (insert (prin1-to-string (car elem))
	      " " (number-to-string (cadr elem)) " 1 y\n"))))

(defun nnultimate-find-forum-table (contents)
  (catch 'found
    (nnultimate-find-forum-table-1 contents)))

(defun nnultimate-find-forum-table-1 (contents)
  (dolist (element contents)
    (unless (stringp element)
      (when (and (eq (car element) 'table)
		 (equalp (cdr (assq 'width (cadr element))) "100%"))
	(throw 'found element))
      (when (nth 2 element)
	(nnultimate-find-forum-table-1 (nth 2 element))))))

(defun nnultimate-descend (type contents)
  (catch 'found
    (nnultimate-descend-1 type contents)))

(defun nnultimate-descend-1 (type contents)
  (when (consp contents)
    (when (eq (car contents) type)
      (throw 'found contents))
    (when (listp (cdr contents))
      (dolist (element contents)
	(when (consp element)
	  (nnultimate-descend-1 type element))))))

(defvar nnultimate-text)
(defun nnultimate-text (contents)
  (let ((nnultimate-text nil))
    (nnultimate-text-1 contents)
    (nreverse nnultimate-text)))

(defun nnultimate-text-1 (contents)
  (when (consp (car contents))
    (dolist (element contents)
      (if (stringp element)
	  (push element nnultimate-text)
	(when (consp element)
	  (nnultimate-text-1 (nth 2 element)))))))


(defun nnultimate-text-1 (contents)
  (dolist (element contents)
    (if (stringp element)
	(push element nnultimate-text)
      (when (and (consp element)
		 (listp (cdr element)))
	(nnultimate-text-1 element)))))

(provide 'nnultimate)

;;; nnultimate.el ends here
