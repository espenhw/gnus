;;; nnultimate.el --- interfacing with the Ultimate Bulletin Board system
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

(defvoo nnultimate-address ""
  "The address of the Ultimate bulletin board.")

;;; Internal variables

(defvar nnultimate-groups-alist nil)
(defvoo nnultimate-groups nil)
(defvoo nnultimate-headers nil)
(defvoo nnultimate-articles nil)

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
	 headers article subject score from date lines parent point
	 contents tinfo fetchers map elem a href garticles topic old-max
	 inc datel)
    (with-temp-buffer
      (nnweb-insert (concat nnultimate-address (format furl sid)))
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
	(when (setq a (nnweb-parse-find 'a row))
	  (setq subject (car (last (nnweb-text a)))
		href (cdr (assq 'href (nth 1 a))))
	  (let ((artlist (nreverse (nnweb-text row)))
		art)
	    (while (and (not art)
			artlist)
	      (when (string-match "^[0-9]+$" (car artlist))
		(setq art (1+ (string-to-number (car artlist)))))
	      (pop artlist))
	    (setq garticles art))
	  (string-match "/\\([0-9]+\\).html" href)
	  (setq topic (string-to-number (match-string 1 href)))
	  (if (setq tinfo (assq topic topics))
	      (progn
		(setq old-max (cadr tinfo))
		(setcar (cdr tinfo) garticles))
	    (setq old-max 0)
	    (push (list topic garticles subject href) topics)
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
	    (nnweb-insert (nth 3 (assq (car elem) topics)))
	    (goto-char (point-min))
	    (setq a (w3-parse-buffer (current-buffer)))
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
				(nnweb-text (car (nth 2 contents)))
				" ")
		datel (nnweb-text (nth 2 (car (cdr (nth 2 contents))))))
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
	     "" 0
	     (/ (length (mapconcat
			 'identity
			 (nnweb-text
			  (cdr (nth 2 (nth 1 (nth 2 contents)))))
			 ""))
		70)
	     nil nil))
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
  (when (or (not dont-check)
	    (not nnultimate-groups))
    (nnultimate-request-list))
  (let ((elem (assoc group nnultimate-groups)))
    (cond
     ((not elem)
      (nnheader-report 'nnultimate "Group does not exist"))
     (t
      (nnheader-report 'nnultimate "Opened group %s" group)
      (nnheader-insert
       "211 %d %d %d %s\n" (cadr elem) 1 (cadr elem)
       (prin1-to-string group))))))

(deffoo nnultimate-request-article (article &optional group server buffer)
  (nnultimate-possibly-change-server group server)
  (let ((contents (cdr (assq article nnultimate-articles))))
    (setq contents (cddr (nth 2 (nth 1 (nth 2 (car contents))))))
    (when contents
      (save-excursion
	(set-buffer (or buffer nntp-server-buffer))
	(erase-buffer)
	(nnweb-insert-html (cons 'p (cons nil (list contents))))
	(goto-char (point-min))
	(insert "Content-Type: text/html\nMIME-Version: 1.0\n")
	(let ((header (cdr (assq article nnultimate-headers))))
	  (nnheader-insert-header header))
	(nnheader-report 'nnultimate "Fetched article %s" article)
	(cons group article)))))

(deffoo nnultimate-request-list (&optional server)
  (nnultimate-possibly-change-server nil server)
  (with-temp-buffer
    (nnweb-insert (concat nnultimate-address "Ultimate.cgi"))
    (let ((contents (nth 2 (car (nth 2
				     (nnultimate-find-forum-table
				      (w3-parse-buffer (current-buffer)))))))
	  sid elem description articles a href group forum
	  a1 a2)
      (dolist (row contents)
	(setq row (nth 2 row))
	(when (setq a (nnweb-parse-find 'a row))
	  (setq group (car (last (nnweb-text a)))
		href (cdr (assq 'href (nth 1 a))))
	  (setq description (car (last (nnweb-text (nth 1 row)))))
	  (setq a1 (car (last (nnweb-text (nth 2 row)))))
	  (setq a2 (car (last (nnweb-text (nth 3 row)))))
	  (when (string-match "^[0-9]+$" a1)
	    (setq articles (string-to-number a1)))
	  (when (and a2 (string-match "^[0-9]+$" a2))
	    (setq articles (max articles (string-to-number a2))))
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

(nnoo-define-skeleton nnultimate)

;;; Internal functions

(defun nnultimate-possibly-change-server (&optional group server)
  (nnultimate-init server)
  (when (and server
	     (not (nnultimate-server-opened server)))
    (nnultimate-open-server server))
  (unless nnultimate-groups-alist
    (nnultimate-read-groups))
  (setq nnultimate-groups (cdr (assoc nnultimate-address
				      nnultimate-groups-alist))))

(deffoo nnultimate-open-server (server &optional defs connectionless)
  (nnheader-init-server-buffer)
  (if (nnultimate-server-opened server)
      t
    (unless (assq 'nnultimate-address defs)
      (setq defs (append defs (list (list 'nnultimate-address server)))))
    (nnoo-change-server 'nnultimate server defs)))

(defun nnultimate-read-groups ()
  (let ((file (expand-file-name "groups" nnultimate-directory)))
    (when (file-exists-p file)
      (with-temp-buffer
	(insert-file-contents file)
	(goto-char (point-min))
	(setq nnultimate-groups-alist (read (current-buffer)))))))

(defun nnultimate-write-groups ()
  (setq nnultimate-groups-alist
	(delq (assoc nnultimate-address nnultimate-groups-alist)
	      nnultimate-groups-alist))
  (push (cons nnultimate-address nnultimate-groups)
	nnultimate-groups-alist)
  (with-temp-file (expand-file-name "groups" nnultimate-directory)
    (prin1 nnultimate-groups-alist (current-buffer))))
    
(defun nnultimate-init (server)
  "Initialize buffers and such."
  (unless (file-exists-p nnultimate-directory)
    (gnus-make-directory nnultimate-directory)))

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
		 (nnultimate-forum-table-p element))
	(throw 'found element))
      (when (nth 2 element)
	(nnultimate-find-forum-table-1 (nth 2 element))))))

(defun nnultimate-forum-table-p (parse)
  (when (not (apply 'gnus-or
		    (mapcar
		     (lambda (p)
		       (nnweb-parse-find 'table p))
		     (nth 2 parse))))
    (let ((href (cdr (assq 'href (nth 1 (nnweb-parse-find 'a parse 20)))))
	  case-fold-search)
      (when (and href (string-match "forumdisplay\\|Forum[0-9]+/HTML\\|getbio"
				    href))
	t))))

(provide 'nnultimate)

;;; nnultimate.el ends here
