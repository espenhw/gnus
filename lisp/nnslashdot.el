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

(defvoo nnslashdot-articles-url "http://slashdot.org/article.pl?sid=%s&threshold=%d&commentsort=0&mode=flat&startat=%d"
  "Where nnslashdot will fetch articles from.")

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

(deffoo nnslashdot-retrieve-headers (articles &optional group server fetch-old)
  (nnslashdot-possibly-change-server group server)
  (let ((last (car (last articles)))
	(did nil)
	(start 1)
	headers article subject score from date lines parent point)
    (save-excursion
      (set-buffer nnslashdot-buffer)
      (erase-buffer)
      (while (or (not article)
		 (and did
		      (< (string-to-number article) last)))
	(when article
	  (setq start (1+ (string-to-number article))))
	(setq point (goto-char (point-max)))
	(url-insert-file-contents
	 (format nnslashdot-articles-url
		 (caddr (assoc group nnslashdot-groups))
		 nnslashdot-threshold start))
	(setq buffer-file-name nil)
	(goto-char point)
	(while (re-search-forward
		"<a name=\"\\([0-9]+\\)\"><b>\\([^<]+\\)</b>.*score\\([^)]+\\))"
		nil t)
	  (setq article (match-string 1)
		subject (match-string 2)
		score (match-string 3))
	  (forward-line 1)
	  (if (looking-at "by <a[^>]+>\\([^<]+\\)</a>[ \t\n]*.*(\\([^)]+\\))")
	      (setq from (concat (match-string 1) " <" (match-string 2) ">"))
	    (looking-at "by \\([^ ]+\\) ")
	    (setq from (match-string 1)))
	  (goto-char (match-end 0))
	  (search-forward "on ")
	  (setq date
		(nnslashdot-date-to-date
		 (buffer-substring (point) (progn (end-of-line) (point)))))
	  (setq lines (count-lines (search-forward "<td")
				   (search-forward "</td>")))
	  (forward-line 2)
	  (setq parent
		(if (looking-at ".*cid=\\([0-9]+\\)")
		    (match-string 1)
		  nil))
	  (setq did t)
	  (push
	   (cons
	    (string-to-number article)
	    (make-full-mail-header
	     (string-to-number article) (concat subject " (" score ")")
	     from date (concat "<" group "%" article "@slashdot>")
	     (if parent (concat "<" group "%" parent "@slashdot>") "")
	     0 lines nil nil))
	   headers))))
    (setq nnslashdot-headers
	  (sort headers (lambda (s1 s2) (< (car s1) (car s2)))))
    (save-excursion
      (set-buffer nntp-server-buffer)
      (erase-buffer)
      (dolist (header nnslashdot-headers)
	(nnheader-insert-nov (cdr header))))
    'nov))

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
      (goto-char (point-min))
      (when (and (numberp article)
	       (search-forward (format "<a name=\"%d\">" article)))
	(setq contents
	      (buffer-substring
	       (re-search-forward "<td[^>]+>")
	       (search-forward "</td>")))))
    (when contents
      (save-excursion
	(set-buffer (or buffer nntp-server-buffer))
	(erase-buffer)
	(insert contents)
	;;(nnweb-remove-markup)
	;;(nnweb-decode-entities)
	(goto-char (point-min))
	(insert "Content-Type: text/html\nMIME-Version: 1.0\n")
	(let ((header (cdr (assq article nnslashdot-headers))))
	  (nnheader-insert-header header))
	(nnheader-report 'nnslashdot "Fetched article %s" article)
	t))))

(deffoo nnslashdot-close-server (&optional server)
  (when (and (nnslashdot-server-opened server)
	     (gnus-buffer-live-p nnslashdot-buffer))
    (save-excursion
      (set-buffer nnslashdot-buffer)
      (kill-buffer nnslashdot-buffer)))
  (nnoo-close-server 'nnslashdot server))

(deffoo nnslashdot-request-list (&optional server)
  (nnslashdot-possibly-change-server nil server)
  (let ((case-fold-search t)
	(number 0)
	sid elem description articles gname)
    (while (> (- nnslashdot-group-number number) 0)
      (with-temp-buffer
	(url-insert-file-contents (format nnslashdot-active-url number))
	(setq buffer-file-name nil)
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
	    (push (list gname articles sid) nnslashdot-groups))))
      (incf number 30))
    (nnslashdot-write-groups)
    (save-excursion
      (set-buffer nntp-server-buffer)
      (erase-buffer)
      (dolist (elem nnslashdot-groups)
	(insert (prin1-to-string (car elem))
		" " (number-to-string (cadr elem)) " 1 m\n")))
    t))

(deffoo nnslashdot-asynchronous-p ()
  nil)

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

(defun nnslashdot-encode-www-form-urlencoded (pairs)
  "Return PAIRS encoded for forms."
  (mapconcat
   (function
    (lambda (data)
      (concat (w3-form-encode-xwfu (car data)) "="
	      (w3-form-encode-xwfu (cdr data)))))
   pairs "&"))

(defun nnslashdot-fetch-form (url pairs)
  (let ((url-request-data (nnslashdot-encode-www-form-urlencoded pairs))
	(url-request-method "POST")
	(url-request-extra-headers
	 '(("Content-type" . "application/x-www-form-urlencoded"))))
    (url-insert-file-contents url)
    (setq buffer-file-name nil))
  t)

(defun nnslashdot-decode-entities ()
  (goto-char (point-min))
  (while (re-search-forward "&\\([a-z]+\\);" nil t)
    (replace-match (char-to-string (or (cdr (assq (intern (match-string 1))
						  w3-html-entities))
				       ?#))
		   t t)))

(defun nnslashdot-remove-markup ()
  (goto-char (point-min))
  (while (search-forward "<!--" nil t)
    (delete-region (match-beginning 0)
		   (or (search-forward "-->" nil t)
		       (point-max))))
  (goto-char (point-min))
  (while (re-search-forward "<[^>]+>" nil t)
    (replace-match "" t t)))

(defun nnslashdot-date-to-date (sdate)
  (let ((elem (split-string sdate)))
    (concat (substring (nth 0 elem) 0 3) " "
	    (substring (nth 1 elem) 0 3) " "
	    (substring (nth 2 elem) 0 2) " "
	    (substring (nth 3 elem) 1 6) " "
	    (nth 4 elem))))

(provide 'nnslashdot)

;;; nnslashdot.el ends here
