;;; nnslashdot.el --- interfacing with Slashdot
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

(nnoo-declare nnslashdot)

(defvoo nnslashdot-directory (nnheader-concat gnus-directory "slashdot/")
  "Where nnslashdot will save its files.")

(defvoo nnslashdot-active-url "http://slashdot.org/search.pl?section=&min=%d"
  "Where nnslashdot will fetch the active file from.")

(defvoo nnslashdot-comments-url "http://slashdot.org/comments.pl?sid=%s&threshold=%d&commentsort=4&mode=flat&startat=%d"
  "Where nnslashdot will fetch comments from.")

(defvoo nnslashdot-article-url
    "http://slashdot.org/article.pl?sid=%s&mode=nocomment"
  "Where nnslashdot will fetch the article from.")

(defvoo nnslashdot-threshold 0
  "The article threshold.")

(defvoo nnslashdot-group-number 30
  "The number of groups to keep updated.")

;;; Internal variables

(defvar nnslashdot-groups nil)
(defvar nnslashdot-buffer nil)
(defvar nnslashdot-headers nil)

;;; Interface functions

(nnoo-define-basics nnslashdot)

(deffoo nnslashdot-thread-retrieve-headers (articles &optional group server fetch-old)
  (nnslashdot-possibly-change-server group server)
  (let ((last (car (last articles)))
	(did nil)
	(start 1)
	(sid (caddr (assoc group nnslashdot-groups)))
	headers article subject score from date lines parent point
	startats s)
    (unless gnus-nov-is-evil
      (save-excursion
	(set-buffer nnslashdot-buffer)
	(let ((case-fold-search t))
	  (erase-buffer)
	  (when (= start 1)
	    (nnweb-insert (format nnslashdot-article-url sid))
	    (goto-char (point-min))
	    (search-forward "Posted by ")
	    (when (looking-at "<a[^>]+>\\([^<]+\\)")
	      (setq from (match-string 1)))
	    (search-forward " on ")
	    (setq date (nnslashdot-date-to-date
			(buffer-substring (point) (1- (search-forward "<")))))
	    (forward-line 2)
	    (setq lines (count-lines
			 (point)
			 (search-forward
			  "A href=http://slashdot.org/article.pl")))
	    (push
	     (cons
	      1
	      (make-full-mail-header
	       1 group from date (concat "<" sid "%1@slashdot>")
	       "" 0 lines nil nil))
	     headers)
	    (goto-char (point-max))
	    (while (re-search-backward "startat=\\([0-9]+\\)" nil t)
	      (setq s (match-string 1))
	      (unless (memq s startats)
		(push s startats)))
	    (unless startats
	      (push 1 startats)))
	  (setq startats (sort startats '<))
	  (while (and (setq start (pop startats))
		      (< start last))
	    (setq point (goto-char (point-max)))
	    (nnweb-insert
	     (format nnslashdot-comments-url sid nnslashdot-threshold start))
	    (goto-char point)
	    (while (re-search-forward
		    "<a name=\"\\([0-9]+\\)\"><b>\\([^<]+\\)</b>.*score:\\([^)]+\\))"
		    nil t)
	      (setq article (string-to-number (match-string 1))
		    subject (match-string 2)
		    score (match-string 3))
	      (when (string-match "^Re: *" subject)
		(setq subject (concat "Re: " (substring subject (match-end 0)))))
	      (forward-line 1)
	      (if (looking-at
		   "by <a[^>]+>\\([^<]+\\)</a>[ \t\n]*.*(\\([^)]+\\))")
		  (setq from (concat (match-string 1)
				     " <" (match-string 2) ">"))
		(looking-at "by \\(.+\\) on ")
		(setq from (match-string 1)))
	      (goto-char (- (match-end 0) 5))
	      (search-forward " on ")
	      (setq date
		    (nnslashdot-date-to-date
		     (buffer-substring (point) (progn (end-of-line) (point)))))
	      (setq lines (/ (abs (- (search-forward "<td ")
				     (search-forward "</td>")))
			     70))
	      (forward-line 2)
	      (setq parent
		    (if (looking-at ".*cid=\\([0-9]+\\)")
			(match-string 1)
		      nil))
	      (setq did t)
	      (push
	       (cons
		(1+ article)
		(make-full-mail-header
		 (1+ article) subject
		 from date
		 (concat "<" sid "%"
			 (number-to-string (1+ article)) 
			 "@slashdot>")
		 (if parent
		     (concat "<" sid "%"
			     (number-to-string (1+ (string-to-number parent)))
			     "@slashdot>")
		   "")
		 0 (string-to-number score) nil nil))
	       headers)))))
      (setq nnslashdot-headers (sort headers 'car-less-than-car))
      (save-excursion
	(set-buffer nntp-server-buffer)
	(erase-buffer)
	(dolist (header nnslashdot-headers)
	  (nnheader-insert-nov (cdr header))))
      'nov)))

(deffoo nnslashdot-retrieve-headers (articles &optional group
					      server fetch-old)
  (unless gnus-nov-is-evil
    (nnslashdot-possibly-change-server group server)
    (let ((last (car (last articles)))
	  (did nil)
	  (start (max (1- (car articles)) 1))
	  (sid (caddr (assoc group nnslashdot-groups)))
	  headers article subject score from date lines parent point)
      (save-excursion
	(set-buffer nnslashdot-buffer)
	(erase-buffer)
	(when (= start 1)
	  (nnweb-insert (format nnslashdot-article-url sid))
	  (goto-char (point-min))
	  (search-forward "Posted by ")
	  (when (looking-at "<a[^>]+>\\([^<]+\\)")
	    (setq from (match-string 1)))
	  (search-forward " on ")
	  (setq date (nnslashdot-date-to-date
		      (buffer-substring (point) (1- (search-forward "<")))))
	  (forward-line 2)
	  (setq lines (count-lines (point)
				   (search-forward
				    "A href=http://slashdot.org/article.pl")))
	  (push
	   (cons
	    1
	    (make-full-mail-header
	     1 group from date (concat "<" sid "%1@slashdot>")
	     "" 0 lines nil nil))
	   headers))
	(while (or (not article)
		   (and did
			(< article last)))
	  (when article
	    (setq start (1+ article)))
	  (setq point (goto-char (point-max)))
	  (nnweb-insert
	   (format nnslashdot-comments-url sid nnslashdot-threshold start))
	  (goto-char point)
	  (while (re-search-forward
		  "<a name=\"\\([0-9]+\\)\"><b>\\([^<]+\\)</b>.*score\\([^)]+\\))"
		  nil t)
	    (setq article (string-to-number (match-string 1))
		  subject (match-string 2)
		  score (match-string 3))
	    (forward-line 1)
	    (if (looking-at
		 "by <a[^>]+>\\([^<]+\\)</a>[ \t\n]*.*(\\([^)]+\\))")
		(setq from (concat (match-string 1) " <" (match-string 2) ">"))
	      (looking-at "by \\(.+\\) on ")
	      (setq from (match-string 1)))
	    (goto-char (- (match-end 0) 5))
	    (search-forward " on ")
	    (setq date
		  (nnslashdot-date-to-date
		   (buffer-substring (point) (progn (end-of-line) (point)))))
	    (setq lines (/ (abs (- (search-forward "<td ")
				   (search-forward "</td>")))
			   70))
	    (forward-line 2)
	    (setq parent
		  (if (looking-at ".*cid=\\([0-9]+\\)")
		      (match-string 1)
		    nil))
	    (setq did t)
	    (push
	     (cons
	      (1+ article)
	      (make-full-mail-header
	       (1+ article) (concat subject " (" score ")")
	       from date
	       (concat "<" sid "%"
		       (number-to-string (1+ article)) 
		       "@slashdot>")
	       (if parent
		   (concat "<" sid "%"
			   (number-to-string (1+ (string-to-number parent)))
			   "@slashdot>")
		 "")
	       0 lines nil nil))
	     headers))))
      (setq nnslashdot-headers
	    (sort headers (lambda (s1 s2) (< (car s1) (car s2)))))
      (save-excursion
	(set-buffer nntp-server-buffer)
	(erase-buffer)
	(dolist (header nnslashdot-headers)
	  (nnheader-insert-nov (cdr header))))
      'nov)))

(deffoo nnslashdot-request-group (group &optional server dont-check)
  (nnslashdot-possibly-change-server nil server)
  (let ((elem (assoc group nnslashdot-groups)))
    (cond
     ((not elem)
      (nnheader-report 'nnslashdot "Group does not exist"))
     (t
      (nnheader-report 'nnslashdot "Opened group %s" group)
      (nnheader-insert
       "211 %d %d %d %s\n" (cadr elem) 1 (cadr elem)
       (prin1-to-string group))))))

(deffoo nnslashdot-close-group (group &optional server)
  (nnslashdot-possibly-change-server group server)
  (when (gnus-buffer-live-p nnslashdot-buffer)
    (save-excursion
      (set-buffer nnslashdot-buffer)
      (kill-buffer nnslashdot-buffer)))
  t)

(deffoo nnslashdot-request-article (article &optional group server buffer)
  (nnslashdot-possibly-change-server group server)
  (let (contents)
    (save-excursion
      (set-buffer nnslashdot-buffer)
      (let ((case-fold-search t))
	(goto-char (point-min))
	(when (and (stringp article)
		   (string-match "%\\([0-9]+\\)@" article))
	  (setq article (string-to-number (match-string 1 article))))
	(when (numberp article)
	  (if (= article 1)
	      (progn
		(re-search-forward "Posted by .* on ")
		(forward-line 1)
		(setq contents
		      (buffer-substring
		       (point)
		       (progn
			 (re-search-forward
			  "^<p>.*A href=http://slashdot.org/article.pl")
			 (match-beginning 0)))))
	    (search-forward (format "<a name=\"%d\">" (1- article)))
	    (setq contents
		  (buffer-substring
		   (re-search-forward "<td[^>]+>")
		   (search-forward "</td>")))))))
    (when contents
      (save-excursion
	(set-buffer (or buffer nntp-server-buffer))
	(erase-buffer)
	(insert contents)
	(goto-char (point-min))
	(insert "Content-Type: text/html\nMIME-Version: 1.0\n")
	(let ((header (cdr (assq article nnslashdot-headers))))
	  (nnheader-insert-header header))
	(nnheader-report 'nnslashdot "Fetched article %s" article)
	(cons group article)))))

(deffoo nnslashdot-close-server (&optional server)
  (when (and (nnslashdot-server-opened server)
	     (gnus-buffer-live-p nnslashdot-buffer))
    (save-excursion
      (set-buffer nnslashdot-buffer)
      (kill-buffer nnslashdot-buffer)))
  (nnoo-close-server 'nnslashdot server))

(deffoo nnslashdot-request-list (&optional server)
  (nnslashdot-possibly-change-server nil server)
  (let ((number 0)
	sid elem description articles gname)
    (while (> (- nnslashdot-group-number number) 0)
      (with-temp-buffer
	(let ((case-fold-search t))
	  (nnweb-insert (format nnslashdot-active-url number))
	  (goto-char (point-min))
	  (while (re-search-forward
		  "article.pl\\?sid=\\([^&]+\\).*<b>\\([^<]+\\)</b>" nil t)
	    (setq sid (match-string 1)
		  description (match-string 2))
	    (forward-line 1)
	    (when (re-search-forward "<b>\\([0-9]+\\)</b>" nil t)
	      (setq articles (string-to-number (match-string 1))))
	    (setq gname (concat description " (" sid ")"))
	    (if (setq elem (assoc gname nnslashdot-groups))
		(setcar (cdr elem) articles)
	      (push (list gname articles sid) nnslashdot-groups)))))
      (incf number 30))
    (nnslashdot-write-groups)
    (nnslashdot-generate-active)
    t))

(deffoo nnslashdot-request-newgroups (date &optional server)
  (nnslashdot-possibly-change-server nil server)
  (nnslashdot-generate-active)
  t)

(nnoo-define-skeleton nnslashdot)

;;; Internal functions

(defun nnslashdot-possibly-change-server (&optional group server)
  (nnslashdot-init server)
  (when (and server
	     (not (nnslashdot-server-opened server)))
    (nnslashdot-open-server server))
  (unless nnslashdot-groups
    (nnslashdot-read-groups)))

(defun nnslashdot-read-groups ()
  (let ((file (expand-file-name "groups" nnslashdot-directory)))
    (when (file-exists-p file)
      (with-temp-buffer
	(insert-file-contents file)
	(goto-char (point-min))
	(setq nnslashdot-groups (read (current-buffer)))))))

(defun nnslashdot-write-groups ()
  (with-temp-file (expand-file-name "groups" nnslashdot-directory)
    (prin1 nnslashdot-groups (current-buffer))))
    
(defun nnslashdot-init (server)
  "Initialize buffers and such."
  (unless (file-exists-p nnslashdot-directory)
    (gnus-make-directory nnslashdot-directory))
  (unless (gnus-buffer-live-p nnslashdot-buffer)
    (setq nnslashdot-buffer
	  (save-excursion
	    (nnheader-set-temp-buffer
	     (format " *nnslashdot %s*" server))))))

(defun nnslashdot-date-to-date (sdate)
  (let ((elem (split-string sdate)))
    (concat (substring (nth 0 elem) 0 3) " "
	    (substring (nth 1 elem) 0 3) " "
	    (substring (nth 2 elem) 0 2) " "
	    (substring (nth 3 elem) 1 6) " "
	    (format-time-string "%Y") " "
	    (nth 4 elem))))

(defun nnslashdot-generate-active ()
  (save-excursion
    (set-buffer nntp-server-buffer)
    (erase-buffer)
    (dolist (elem nnslashdot-groups)
      (insert (prin1-to-string (car elem))
	      " " (number-to-string (cadr elem)) " 1 y\n"))))

(provide 'nnslashdot)

;;; nnslashdot.el ends here
