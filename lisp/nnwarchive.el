;;; nnwarchive.el --- interfacing with web archives
;; Copyright (C) 1999 Free Software Foundation, Inc.

;; Author: Shenghuo Zhu <zsh@cs.rochester.edu>
;; Keywords: news

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
;; option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Note: You need to have `url' and `w3' installed for this backend to
;; work.

;; A lot of codes stolen from mail-source, nnslashdot, nnweb.

;; Todo: 
;; 1. To support more web archives.
;; 2. Support nnwarchive-xover-is-evil.

;; Known bugs: in w3 0.44, there are two copies of url-maybe-relative.
;; If it is loaded from w3.el, (load-library "url").  Update to w3 0.46 
;; or greater version.

;;; Code:

(eval-when-compile (require 'cl))

(require 'nnoo)
(require 'message)
(require 'gnus-util)
(require 'gnus)
(require 'nnmail)
(require 'mm-util)
(require 'mail-source)
(eval-when-compile
  (ignore-errors
    (require 'w3)
    (require 'url)
    (require 'w3-forms)
    (require 'nnweb)))
;; Report failure to find w3 at load time if appropriate.
(eval '(progn
	 (require 'w3)
	 (require 'url)
	 (require 'w3-forms)
	 (require 'nnweb)))

(nnoo-declare nnwarchive)

(defvar nnwarchive-type-definition
  '((egroups
     (address . "www.egroups.com")
     (open-url 
      "http://www.egroups.com/register?method=loginAction&email=%s&password=%s" 
      nnwarchive-login nnwarchive-passwd)
     (list-url 
      "http://www.egroups.com/UserGroupsPage?")
     (list-dissect . nnwarchive-egroups-list)
     (list-groups . nnwarchive-egroups-list-groups)
     (xover-url 
      "http://www.egroups.com/group/%s/?fetchForward=1&start=%d" group aux)
     (xover-last-url 
      "http://www.egroups.com/group/%s/?fetchForward=1" group)
     (xover-page-size . 13)
     (xover-dissect . nnwarchive-egroups-xover)
     (article-url 
      "http://www.egroups.com/group/%s/%d.html?raw=1" group article)
     (article-dissect . nnwarchive-egroups-article)
     (authentication . t)
     (xover-files . nnwarchive-egroups-xover-files))
    (mail-archive
     (address . "www.mail-archive.com")
     (list-url 
      "http://www.mail-archive.com/lists.html")
     (list-dissect . nnwarchive-mail-archive-list)
     (list-groups . nnwarchive-mail-archive-list-groups)
     (xover-url 
      "http://www.mail-archive.com/%s/mail%d.html" group aux)
     (xover-last-url 
      "http://www.mail-archive.com/%s/maillist.html" group)
     (xover-dissect . nnwarchive-mail-archive-xover)
     (article-url 
      "http://www.mail-archive.com/%s/msg%05d.html" group article1)
     (article-dissect . nnwarchive-mail-archive-article)
     (xover-files . nnwarchive-mail-archive-xover-files)
     (article-offset . 1))))

(defvar nnwarchive-default-type 'egroups)

(defvoo nnwarchive-directory (nnheader-concat gnus-directory "warchive/")
  "Where nnwarchive will save its files.")

(defvoo nnwarchive-type nil
    "The type of nnwarchive.")

(defvoo nnwarchive-address ""
  "The address of nnwarchive.")

(defvoo nnwarchive-login nil
  "Your login name for the group.")

(defvoo nnwarchive-passwd nil
  "Your password for the group.")

(defvoo nnwarchive-groups nil)

(defvoo nnwarchive-headers-cache nil)

(defvoo nnwarchive-opened nil)

(defvoo nnwarchive-authentication nil)

(defvoo nnwarchive-xover-is-evil nil) ;; not implemented

(defconst nnwarchive-version "nnwarchive 1.0")

;;; Internal variables

(defvoo nnwarchive-open-url nil)
(defvoo nnwarchive-open-dissect nil)

(defvoo nnwarchive-list-url nil)
(defvoo nnwarchive-list-dissect nil)
(defvoo nnwarchive-list-groups nil)

(defvoo nnwarchive-xover-files nil)
(defvoo nnwarchive-xover-url nil)
(defvoo nnwarchive-xover-last-url nil)
(defvoo nnwarchive-xover-dissect nil)
(defvoo nnwarchive-xover-page-size nil)

(defvoo nnwarchive-article-url nil)
(defvoo nnwarchive-article-dissect nil)
(defvoo nnwarchive-xover-files nil)
(defvoo nnwarchive-article-offset 0)

(defvoo nnwarchive-buffer nil)

(defvar nnwarchive-headers nil)


;;; Interface functions

(nnoo-define-basics nnwarchive)

(defun nnwarchive-set-default (type)
  (let ((defs (cdr (assq type nnwarchive-type-definition)))
	def)
    (dolist (def defs)
      (set (intern (concat "nnwarchive-" (symbol-name (car def)))) 
	   (cdr def)))))

(deffoo nnwarchive-retrieve-headers (articles &optional group server fetch-old)
  (nnwarchive-possibly-change-server group server)
  (setq nnwarchive-headers (cdr (assoc group nnwarchive-headers-cache)))
  (save-excursion
    (set-buffer nnwarchive-buffer)
    (erase-buffer)
    (funcall nnwarchive-xover-files group articles))
  (save-excursion
    (set-buffer nntp-server-buffer)
    (erase-buffer)
    (let (header)
      (dolist (art articles)
	(if (setq header (assq art nnwarchive-headers))
	    (nnheader-insert-nov (cdr header))))))
  (let ((elem (assoc group nnwarchive-headers-cache)))
    (if elem
	(setcdr elem nnwarchive-headers)
      (push (cons group nnwarchive-headers) nnwarchive-headers-cache)))
  'nov)

(deffoo nnwarchive-retrieve-groups (groups &optional server)
  "Retrieve group info on GROUPS."
  (nnwarchive-possibly-change-server nil server)
  (if nnwarchive-list-groups
      (funcall nnwarchive-list-groups groups))
  (nnwarchive-write-groups)
  (nnwarchive-generate-active)
  'active)

(deffoo nnwarchive-request-group (group &optional server dont-check)
  (nnwarchive-possibly-change-server nil server)
  (if nnwarchive-list-groups
      (funcall nnwarchive-list-groups (list group)))
  (nnwarchive-write-groups)
  (let ((elem (assoc group nnwarchive-groups)))
    (cond
     ((not elem)
      (nnheader-report 'nnwarchive "Group does not exist"))
     (t
      (nnheader-report 'nnwarchive "Opened group %s" group)
      (nnheader-insert
       "211 %d %d %d %s\n" (or (cadr elem) 0) 1 (or (cadr elem) 0)
       (prin1-to-string group))
      t))))

(deffoo nnwarchive-close-group (group &optional server)
  (nnwarchive-possibly-change-server group server)
  (when (gnus-buffer-live-p nnwarchive-buffer)
    (save-excursion
      (set-buffer nnwarchive-buffer)
      (kill-buffer nnwarchive-buffer)))
  t)

(deffoo nnwarchive-request-article (article &optional group server buffer)
  (nnwarchive-possibly-change-server group server)
  (let (contents)
    (save-excursion
      (set-buffer nnwarchive-buffer)
      (goto-char (point-min))
      (let ((article1 (- article nnwarchive-article-offset)))
	(nnwarchive-url nnwarchive-article-url))
      (setq contents (funcall nnwarchive-article-dissect group article)))
    (when contents
      (save-excursion
	(set-buffer (or buffer nntp-server-buffer))
	(erase-buffer)
	(insert contents)
	(nnheader-report 'nnwarchive "Fetched article %s" article)
	(cons group article)))))

(deffoo nnwarchive-close-server (&optional server)
  (when (and (nnwarchive-server-opened server)
	     (gnus-buffer-live-p nnwarchive-buffer))
    (save-excursion
      (set-buffer nnwarchive-buffer)
      (kill-buffer nnwarchive-buffer)))
  (nnoo-close-server 'nnwarchive server))

(deffoo nnwarchive-request-list (&optional server)
  (nnwarchive-possibly-change-server nil server)
  (save-excursion
    (set-buffer nnwarchive-buffer)
    (erase-buffer)
    (if nnwarchive-list-url
	(nnwarchive-url nnwarchive-list-url))
    (if nnwarchive-list-dissect
	(funcall nnwarchive-list-dissect))
    (nnwarchive-write-groups)
    (nnwarchive-generate-active))
  'active)

(deffoo nnwarchive-request-newgroups (date &optional server)
  (nnwarchive-possibly-change-server nil server)
  (nnwarchive-write-groups)
  (nnwarchive-generate-active)
  'active)

(deffoo nnwarchive-asynchronous-p ()
  nil)

(deffoo nnwarchive-server-opened (&optional server)
  nnwarchive-opened)

(deffoo nnwarchive-open-server (server &optional defs connectionless)
  (nnwarchive-init server)
  (unless (nnwarchive-server-opened server)
    (when nnwarchive-authentication
      (setq nnwarchive-login
	    (or nnwarchive-login
		(read-string
		 (format "Login at %s: " server)
		 user-mail-address)))
      (setq nnwarchive-passwd
	    (or nnwarchive-passwd
		(mail-source-read-passwd
		 (format "Password for %s at %s: " 
			 nnwarchive-login server)))))
    (unless nnwarchive-groups
      (nnwarchive-read-groups))
    (save-excursion
      (set-buffer nnwarchive-buffer)
      (erase-buffer)
      (if nnwarchive-open-url
	  (nnwarchive-url nnwarchive-open-url))
      (if nnwarchive-open-dissect
	  (funcall nnwarchive-open-dissect))
      (setq nnwarchive-opened t)))
  t)

(nnoo-define-skeleton nnwarchive)

;;; Internal functions

(defun nnwarchive-possibly-change-server (&optional group server)
  (nnwarchive-init server)
  (when (and server
	     (not (nnwarchive-server-opened server)))
    (nnwarchive-open-server server)))

(defun nnwarchive-read-groups ()
  (let ((file (expand-file-name (concat "groups-" nnwarchive-address) 
				nnwarchive-directory)))
    (when (file-exists-p file)
      (with-temp-buffer
	(insert-file-contents file)
	(goto-char (point-min))
	(setq nnwarchive-groups (read (current-buffer)))))))

(defun nnwarchive-write-groups ()
  (with-temp-file (expand-file-name (concat "groups-" nnwarchive-address) 
				    nnwarchive-directory)
    (prin1 nnwarchive-groups (current-buffer))))

(defun nnwarchive-init (server)
  "Initialize buffers and such."
  (let ((type (intern server)) (defs nnwarchive-type-definition) def)
    (cond 
     ((equal server "")
      (setq type nnwarchive-default-type))
     ((assq type nnwarchive-type-definition) t)
     (t
      (setq type nil)
      (while (setq def (pop defs))
	(when (equal (cdr (assq 'address (cdr def))) server)
	  (setq defs nil)
	  (setq type (car def))))
      (unless type
	(error "Undefined server %s" server))))
    (setq nnwarchive-type type))
  (unless (file-exists-p nnwarchive-directory)
    (gnus-make-directory nnwarchive-directory))
  (unless (gnus-buffer-live-p nnwarchive-buffer)
    (setq nnwarchive-buffer
	  (save-excursion
	    (nnheader-set-temp-buffer
	     (format " *nnwarchive %s %s*" nnwarchive-type server)))))
  (nnwarchive-set-default nnwarchive-type))

(defun nnwarchive-encode-www-form-urlencoded (pairs)
  "Return PAIRS encoded for forms."
  (mapconcat
   (function
    (lambda (data)
      (concat (w3-form-encode-xwfu (car data)) "="
	      (w3-form-encode-xwfu (cdr data)))))
   pairs "&"))

(defun nnwarchive-fetch-form (url pairs)
  (let ((url-request-data (nnwarchive-encode-www-form-urlencoded pairs))
	(url-request-method "POST")
	(url-request-extra-headers
	 '(("Content-type" . "application/x-www-form-urlencoded"))))
    (nnweb-insert url))
  t)

(defun nnwarchive-eval (expr)
  (cond
   ((consp expr)
    (cons (nnwarchive-eval (car expr)) (nnwarchive-eval (cdr expr))))
   ((symbolp expr)
    (eval expr))
   (t
    expr)))

(defun nnwarchive-url (xurl)
  (let ((url-confirmation-func 'identity))
    (cond 
     ((eq (car xurl) 'post)
      (pop xurl)
      (nnwarchive-fetch-form (car xurl) (nnwarchive-eval (cdr xurl))))
     (t
      (nnweb-insert (apply 'format (nnwarchive-eval xurl)))))))

(defun nnwarchive-decode-entities ()
  (goto-char (point-min))
  (while (re-search-forward "&\\(#[0-9]+\\|[a-z]+\\);" nil t)
    (replace-match (char-to-string 
		    (if (eq (aref (match-string 1) 0) ?\#)
			(string-to-number (substring (match-string 1) 1))
		      (or (cdr (assq (intern (match-string 1))
				     w3-html-entities))
			  ?#)))
		   t t)))

(defun nnwarchive-decode-entities-string (str)
  (with-temp-buffer
    (insert str)
    (nnwarchive-decode-entities)
    (buffer-substring (point-min) (point-max))))

(defun nnwarchive-remove-markup ()
  (goto-char (point-min))
  (while (search-forward "<!--" nil t)
    (delete-region (match-beginning 0)
		   (or (search-forward "-->" nil t)
		       (point-max))))
  (goto-char (point-min))
  (while (re-search-forward "<[^>]+>" nil t)
    (replace-match "" t t)))

(defun nnwarchive-date-to-date (sdate)
  (let ((elem (split-string sdate)))
    (concat (substring (nth 0 elem) 0 3) " "
	    (substring (nth 1 elem) 0 3) " "
	    (substring (nth 2 elem) 0 2) " "
	    (substring (nth 3 elem) 1 6) " "
	    (format-time-string "%Y") " "
	    (nth 4 elem))))

(defun nnwarchive-generate-active ()
  (save-excursion
    (set-buffer nntp-server-buffer)
    (erase-buffer)
    (dolist (elem nnwarchive-groups)
      (insert (prin1-to-string (car elem))
	      " " (number-to-string (or (cadr elem) 0)) " 1 y\n"))))

(defun nnwarchive-paged (articles)
  (let (art narts next)
    (while (setq art (pop articles))
      (when (and (>= art (or next 0))
		 (not (assq art nnwarchive-headers)))
	(push art narts)
	(setq next (+ art nnwarchive-xover-page-size))))
    narts))

;; egroups

(defun nnwarchive-egroups-list-groups (groups)
  (save-excursion
    (let (articles)
      (set-buffer nnwarchive-buffer)
      (dolist (group groups) 
	(erase-buffer)
	(nnwarchive-url nnwarchive-xover-last-url)
	(goto-char (point-min))
	(when (re-search-forward "of \\([0-9]+\\)</title>" nil t)
	  (setq articles (string-to-number (match-string 1)))) 
	(let ((elem (assoc group nnwarchive-groups)))
	  (if elem
	      (setcar (cdr elem) articles)
	    (push (list group articles "") nnwarchive-groups)))
	(setq nnwarchive-headers (cdr (assoc group nnwarchive-headers-cache)))
	(nnwarchive-egroups-xover group)
	(let ((elem (assoc group nnwarchive-headers-cache)))
	  (if elem
	      (setcdr elem nnwarchive-headers)
	    (push (cons group nnwarchive-headers) nnwarchive-headers-cache)))))))

(defun nnwarchive-egroups-list ()
  (let ((case-fold-search t)
	group description elem articles)
    (goto-char (point-min))
    (while 
	(re-search-forward
	 "/group/\\([^/]+\\)/info\\.html[^>]+>[^>]+>[\040\t]*-[\040\t]*\\([^<]+\\)<"
	 nil t)
      (setq group (match-string 1)
	    description (match-string 2))
      (forward-line 1)
      (when (re-search-forward ">\\([0-9]+\\)<" nil t)
	(setq articles (string-to-number (match-string 1)))) 
      (if (setq elem (assoc group nnwarchive-groups))
	  (setcar (cdr elem) articles)
	(push (list group articles description) nnwarchive-groups))))
  t)

(defun nnwarchive-egroups-xover (group)
  (let (article subject from date)
    (goto-char (point-min))
    (while (re-search-forward
	    "<a href=\"/group/\\([^/]+\\)/\\([0-9]+\\)\\.html[^>]+>\\([^<]+\\)<"
	    nil t)
      (setq group  (match-string 1)
	    article (string-to-number (match-string 2))
	    subject (match-string 3))
      (forward-line 1)
      (unless (assq article nnwarchive-headers)
	(if (looking-at "<td[^>]+><font[^>]+>\\([^<]+\\)</font>")
	    (setq from (match-string 1)))
	(forward-line 1)
	(if (looking-at "<td[^>]+><font[^>]+>\\([^<]+\\)</font>")
	    (setq date (identity (match-string 1))))
	(push (cons
	       article
	       (make-full-mail-header
		article 
		(nnwarchive-decode-entities-string subject)
		(nnwarchive-decode-entities-string from)
		date
		(concat "<" group "%"
			(number-to-string article) 
			"@egroup.com>")
		""
		0 0 "")) nnwarchive-headers))))
  nnwarchive-headers)

(defun nnwarchive-egroups-article (group articles)
  (goto-char (point-min))
  (if (search-forward "<pre>" nil t)
      (delete-region (point-min) (point)))
  (goto-char (point-max))
  (if (search-backward "</pre>" nil t)
      (delete-region (point) (point-max)))
  (goto-char (point-min))
  (while (re-search-forward "<a[^>]+>\\([^<]+\\)</a>" nil t)
    (replace-match "<\\1>"))
  (nnwarchive-decode-entities)
  (buffer-string))

(defun nnwarchive-egroups-xover-files (group articles)
  (let (aux auxs)
    (setq auxs (nnwarchive-paged (sort articles '<)))
    (while (setq aux (pop auxs))
      (goto-char (point-max))
      (nnwarchive-url nnwarchive-xover-url))
    (if nnwarchive-xover-dissect
	(nnwarchive-egroups-xover group))))

;; mail-archive

(defun nnwarchive-mail-archive-list-groups (groups)
  (save-excursion
    (let (articles)
      (set-buffer nnwarchive-buffer)
      (dolist (group groups)
	(erase-buffer)
	(nnwarchive-url nnwarchive-xover-last-url)
	(goto-char (point-min))
	(when (re-search-forward "msg\\([0-9]+\\)\\.html" nil t)
	  (setq articles (1+ (string-to-number (match-string 1)))))
	(let ((elem (assoc group nnwarchive-groups)))
	  (if elem
	      (setcar (cdr elem) articles)
	    (push (list group articles "") nnwarchive-groups)))
	(setq nnwarchive-headers (cdr (assoc group nnwarchive-headers-cache)))
	(nnwarchive-mail-archive-xover group)
	(let ((elem (assoc group nnwarchive-headers-cache)))
	  (if elem
	      (setcdr elem nnwarchive-headers)
	    (push (cons group nnwarchive-headers) 
		  nnwarchive-headers-cache)))))))

(defun nnwarchive-mail-archive-list ()
  (let ((case-fold-search t)
	group description elem articles)
    (goto-char (point-min))
    (while (re-search-forward "<a href=\"\\([^/]+\\)/\">\\([^>]+\\)<" nil t)
      (setq group (match-string 1)
	    description (match-string 2))
      (forward-line 1)
      (setq articles 0)
      (if (setq elem (assoc group nnwarchive-groups))
	  (setcar (cdr elem) articles)
	(push (list group articles description) nnwarchive-groups))))
  t)

(defun nnwarchive-mail-archive-xover (group)
  (let (article subject from date)
    (goto-char (point-min))
    (while (re-search-forward
	    "<A[^>]*HREF=\"msg\\([0-9]+\\)\\.html[^>]+>\\([^<]+\\)<"
	    nil t)
      (setq article (1+ (string-to-number (match-string 1)))
	    subject (match-string 2))
      (forward-line 1)
      (unless (assq article nnwarchive-headers)
	(if (looking-at "<UL><LI><EM>From</EM>:\\([^&]+\\)&lt;\\([^&]+\\)&gt;")
	    (progn
	      (setq from (match-string 1)
		    date (identity (match-string 2))))
	  (setq from "" date ""))
	(push (cons
	       article
	       (make-full-mail-header
		article 
		(nnwarchive-decode-entities-string subject)
		(nnwarchive-decode-entities-string from)
		date
		(format "<%05d%%%s>\n" (1- article) group)
		""
		0 0 "")) nnwarchive-headers))))
  nnwarchive-headers)

(defun nnwarchive-mail-archive-xover-files (group articles)
  (unless nnwarchive-headers
    (erase-buffer)
    (nnwarchive-url nnwarchive-xover-last-url)
    (goto-char (point-min))
    (nnwarchive-mail-archive-xover group))
  (let ((minart (apply 'min articles))
	(min (apply 'min (mapcar 'car nnwarchive-headers)))
	(aux 2))
    (while (> min minart)
      (erase-buffer)
      (nnwarchive-url nnwarchive-xover-url)
      (nnwarchive-mail-archive-xover group)
      (setq min (apply 'min (mapcar 'car nnwarchive-headers))))))

(defun nnwarchive-mail-archive-article (group article)
  (let (p refs url mime file e)
    (save-restriction
      (goto-char (point-min))
      (when (search-forward "<ul>" nil t)
	(forward-line)
	(delete-region (point-min) (point))
	(search-forward "</ul>" nil t)
	(forward-line)
	(narrow-to-region (point-min) (point))
	(nnwarchive-remove-markup)
	(nnwarchive-decode-entities)
	(goto-char (point-min))
	(delete-blank-lines)
	(goto-char (point-max))
	(widen)
	(insert "\n"))
      (setq p (point)) 
      (when (search-forward "X-Body-of-Message" nil t)
	  (forward-line)
	  (delete-region p (point))
	  (search-forward "X-Body-of-Message-End" nil t)
	  (beginning-of-line)
	  (save-restriction
	    (narrow-to-region p (point))
	    (goto-char (point-min))
	    (if (looking-at "<PRE>") 
		(progn
		  (delete-char 5)
		  (setq p (point))
		  (when (search-forward "</PRE>" nil t)
		    (goto-char (match-beginning 0))
		    (delete-char 6)
		    (save-restriction
		      (narrow-to-region p (point))
		      (nnwarchive-remove-markup)
		      (nnwarchive-decode-entities)
		      (goto-char (point-max))))
		  (while (looking-at
			  "[\040\n\r\t]*<P><A HREF=\"\\([^\"]+\\)[^>]*><[^>]*>\\([^<]+\\)")
		    (setq url (match-string 1)
			  file (match-string 2))
		    (goto-char (match-beginning 1))
		    (beginning-of-line)
		    (setq p (point))
		    (delete-region p (progn (forward-line) (point)))
		    (insert (format "http://www.mail-archive.com/%s/%s\n"
				    group url))))
	      (setq mime t))
	    (goto-char (point-max))))
      (setq p (point))
      (when (search-forward "X-References-End" nil t)
	(setq e (point))
	(beginning-of-line)
	(search-backward "X-References" p t)
	(while (re-search-forward "msg\\([0-9]+\\)\\.html" e t)
	  (push (concat "<" (match-string 1) "%" group ">") refs)))
      (delete-region p (point-max))
      (goto-char (point-min))
      (insert (format "Message-ID: <%05d%%%s>\n" (1- article) group))
      (when refs
	(insert "References:")
	(while refs
	  (insert " " (pop refs)))
	(insert "\n"))
      (when mime
	(insert "MIME-Version: 1.0\n"
		"Content-Type: text/html\n")))
    (buffer-string)))

(provide 'nnwarchive)

;;; nnwarchive.el ends here
