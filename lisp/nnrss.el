;;; nnrss.el --- interfacing with RSS
;; Copyright (C) 2001  Free Software Foundation, Inc.

;; Author: Shenghuo Zhu <zsh@cs.rochester.edu>
;; Keywords: RSS

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

;;; Code:

(eval-when-compile (require 'cl))

(require 'nnoo)
(require 'nnmail)
(require 'message)
(require 'mm-util)
(require 'gnus-util)
(require 'time-date)
(eval-when-compile
  (ignore-errors
    (require 'xml)
    (require 'w3)
    (require 'w3-forms)
    (require 'nnweb)))
;; Report failure to find w3 at load time if appropriate.
(eval '(progn
	 (require 'xml)
	 (require 'w3)
	 (require 'w3-forms)
	 (require 'nnweb)))

(nnoo-declare nnrss)

(defvoo nnrss-directory (nnheader-concat gnus-directory "rss/")
  "Where nnrss will save its files.")

;; (group max rss-url)
(defvoo nnrss-server-data nil)

;; (num timestamp url subject author date extra)
(defvoo nnrss-group-data nil)
(defvoo nnrss-group-max 0)
(defvoo nnrss-group-min 1)
(defvoo nnrss-group nil)
(defvoo nnrss-group-hashtb nil)
(defvoo nnrss-status-string "")

(defconst nnrss-version "nnrss 1.0")

(defvar nnrss-group-alist
  '(("MacWeek"
     "http://macweek.zdnet.com/macweek.xml")
    ("Linux.Weekly.News"
     "http://lwn.net/headlines/rss")
    ("Motley.Fool"
     "http://www.fool.com/About/headlines/rss_headlines.asp")
    ("NewsForge.rdf"
     "http://www.newsforge.com/newsforge.rdf")
    ("Slashdot"
     "http://www.slashdot.com/slashdot.rdf")
    ("CNN"
     "http://www.cnn.com/cnn.rss")
    ("FreshMeat"
     "http://freshmeat.net/backend/fm.rdf")
    ("The.Guardian.newspaper"
     "http://www.guardianunlimited.co.uk/rss/1,,,00.xml")
    ("MonkeyFist.rdf"
     "http://monkeyfist.com/rdf.php3")
    ("NewsForge"
     "http://www.newsforge.com/newsforge.rss")
    ("Reuters.Health"
     "http://www.reutershealth.com/eline.rss")
    ("Salon"
     "http://www.salon.com/feed/RDF/salon_use.rdf")
    ("Wired"
     "http://www.wired.com/news_drop/netcenter/netcenter.rdf")
    ("ITN"
     "http://www.itn.co.uk/itn.rdf")
    ("Meerkat"
     "http://www.oreillynet.com/meerkat/?_fl=rss10")
    ("MonkeyFist"
     "http://monkeyfist.com/rss1.php3")
    ("Reuters.Health.rdf"
     "http://www.reutershealth.com/eline.rdf")))

(defvar nnrss-use-local nil)

(nnoo-define-basics nnrss)

;;; Interface functions

(deffoo nnrss-retrieve-headers (articles &optional group server fetch-old)
  (nnrss-possibly-change-group group server)
  (let (e)
    (save-excursion
      (set-buffer nntp-server-buffer)
      (erase-buffer)
      (dolist (article articles)
	(if (setq e (assq article nnrss-group-data))
	    (insert (number-to-string (car e)) "\t" ;; number
		    (if (nth 3 e)
			(nnrss-string-as-multibyte (nth 3 e)) "")
		    "\t" ;; subject
		    (if (nth 4 e)
			(nnrss-string-as-multibyte (nth 4 e)) "")
		    "\t" ;;from
		    (or (nth 5 e) "")
		    "\t" ;; date
		    (format "<%d@%s.nnrss>" (car e) group)
		    "\t" ;; id
		    "\t" ;; refs
		    "0" "\t" ;; chars
		    "0" "\t" ;; lines
		    "\n")))))
  'nov)

(deffoo nnrss-request-group (group &optional server dont-check)
  (nnrss-possibly-change-group group server)
  (if dont-check
      t
    (nnrss-check-group group server)
    (nnheader-report 'nnrss "Opened group %s" group)
    (nnheader-insert
     "211 %d %d %d %s\n" nnrss-group-max nnrss-group-min nnrss-group-max
     (prin1-to-string group)
     t)))

(deffoo nnrss-close-group (group &optional server)
  t)

(deffoo nnrss-request-article (article &optional group server buffer)
  (nnrss-possibly-change-group group server)
  (let ((e (assq article nnrss-group-data))
	(nntp-server-buffer (or buffer nntp-server-buffer))
	post err)
    (when e
      (catch 'error
	(with-current-buffer nntp-server-buffer
	  (erase-buffer)
	  (goto-char (point-min))
	  (if (nth 3 e)
	      (insert "Subject: " (nnrss-string-as-multibyte (nth 3 e)) "\n"))
	  (if (nth 4 e)
	      (insert "From: " (nnrss-string-as-multibyte (nth 4 e)) "\n"))
	  (if (nth 5 e)
	      (insert "Date: " (nnrss-string-as-multibyte (nth 5 e)) "\n"))
	  (insert "Message-ID: " (format "<%d@%s.nnrss>" (car e) group) "\n")
	  (insert "\n")
	  (if (nth 6 e)
	      (let ((point (point)))
		(insert (nnrss-string-as-multibyte (nth 6 e)) "\n\n")
		(fill-region point (point))))
	  (if (nth 2 e)
	      (insert (nth 2 e) "\n")))))
    (cond
     (err
      (nnheader-report 'nnrss err))
     ((not e)
      (nnheader-report 'nnrss "No such id: %d" article))
     (t
      (nnheader-report 'nnrss "Article %s retrieved" (car e))
      ;; We return the article number.
      (cons nnrss-group (car e))))))

(deffoo nnrss-request-list (&optional server)
  (nnrss-possibly-change-group nil server)
  (nnrss-generate-active)
  t)

(deffoo nnrss-open-server (server &optional defs connectionless)
  (nnoo-change-server 'nnrss server defs)
  t)

(deffoo nnrss-request-expire-articles
    (articles group &optional server force)
  (nnrss-possibly-change-group group server)
  (let (e changed days)
    (dolist (art articles)
      (when (setq e (assq art nnrss-group-data))
      (if (nnmail-expired-article-p
	   group
	   (if (listp (setq days (nth 1 e))) days (days-to-time days))
	   force)
	  (setq nnrss-group-data (delq e nnrss-group-data)
		changed t))))
    (if changed
	(nnrss-save-group-data group server))))

(deffoo nnrss-request-delete-group (group &optional force server)
  (nnrss-possibly-change-group group server)
  (setq nnrss-server-data
	(delq (assoc group nnrss-server-data) nnrss-server-data))
  (nnrss-save-server-data server)
  (let ((file (expand-file-name (concat group (and server
						   (not (equal server ""))
						   "-")
					server ".el") nnrss-directory)))
    (delete-file file))
  t)

(nnoo-define-skeleton nnrss)

;;; Internal functions

(defun nnrss-possibly-change-group (&optional group server)
  (when (and server
	     (not (nnrss-server-opened server)))
    (nnrss-read-server-data server)
    (nnrss-open-server server))
  (when (and group (not (equal group nnrss-group)))
    (nnrss-read-group-data group server)
    (setq nnrss-group group)))

(defun nnrss-generate-active ()
  (save-excursion
    (set-buffer nntp-server-buffer)
    (erase-buffer)
    (dolist (elem nnrss-group-alist)
      (insert (prin1-to-string (car elem)) " 0 1 y\n"))
    (dolist (elem nnrss-server-data)
      (unless (assoc (car elem) nnrss-group-alist)
	(insert (prin1-to-string (car elem)) " 0 1 y\n")))))

;;; Data functions

(defun nnrss-read-server-data (server)
  (setq nnrss-server-data nil)
  (let ((file (expand-file-name (concat "nnrss" (and server
						     (not (equal server ""))
						     "-")
					server
					".el")
				nnrss-directory)))
    (when (file-exists-p file)
      (with-temp-buffer
	(let ((coding-system-for-read 'binary))
	  (insert-file-contents file))
	(goto-char (point-min))
	(eval-buffer)))))

(defun nnrss-save-server-data (server)
  (gnus-make-directory nnrss-directory)
  (let ((file (expand-file-name (concat "nnrss" (and server
						     (not (equal server ""))
						     "-")
					server ".el")
				nnrss-directory)))
    (let ((coding-system-for-write 'binary))
      (with-temp-file file
	(insert "(setq nnrss-server-data '"
		(prin1-to-string nnrss-server-data)
		")\n")))))

(defun nnrss-read-group-data (group server)
  (setq nnrss-group-data nil)
  (setq nnrss-group-hashtb (gnus-make-hashtable))
  (let ((pair (assoc group nnrss-server-data)))
    (setq nnrss-group-max (or (cadr pair) 0))
    (setq nnrss-group-min (+ nnrss-group-max 1)))
  (let ((file (expand-file-name (concat group (and server
						   (not (equal server ""))
						   "-")
					server ".el")
				nnrss-directory)))
    (when (file-exists-p file)
      (with-temp-buffer
	(let ((coding-system-for-read 'binary))
	  (insert-file-contents file))
	(goto-char (point-min))
	(eval-buffer))
      (dolist (e nnrss-group-data)
	(gnus-sethash (nth 2 e) e nnrss-group-hashtb)
	(if (and (car e) (> nnrss-group-min (car e)))
	    (setq nnrss-group-min (car e)))
	(if (and (car e) (< nnrss-group-max (car e)))
	    (setq nnrss-group-max (car e)))))))

(defun nnrss-save-group-data (group server)
  (gnus-make-directory nnrss-directory)
  (let ((file (expand-file-name (concat group (and server
						   (not (equal server ""))
						   "-")
					server ".el")
				nnrss-directory)))
    (let ((coding-system-for-write 'binary))
      (with-temp-file file
	(insert "(setq nnrss-group-data '"
		(prin1-to-string nnrss-group-data)
		")\n")))))

;;; URL interface

(defun nnrss-no-cache (url)
  "")

;; TODO:: disable cache.
;;
;; (defun nnrss-insert-w3 (url)
;;   (require 'url)
;;   (require 'url-cache)
;;   (let ((url-cache-creation-function 'nnrss-no-cache))
;;     (mm-with-unibyte-current-buffer
;;       (nnweb-insert url))))

(defun nnrss-insert-w3 (url)
  (mm-with-unibyte-current-buffer
    (nnweb-insert url)))

(defun nnrss-decode-entities-unibyte-string (string)
  (mm-with-unibyte-buffer
    (insert string)
    (nnweb-decode-entities)
    (buffer-substring (point-min) (point-max))))

(defalias 'nnrss-insert 'nnrss-insert-w3)

(if (featurep 'xemacs)
    (defalias 'nnrss-string-as-multibyte 'identity)
  (defalias 'nnrss-string-as-multibyte 'string-as-multibyte))

;;; Snarf functions

(defun nnrss-check-group (group server)
  (let ((w3-html-entities (cons '(nbsp . 32) w3-html-entities))
	file xml subject url extra changed author date)
    (mm-with-unibyte-buffer
      (if (and nnrss-use-local
	       (file-exists-p (setq file (expand-file-name
					  (concat group ".xml")
					  nnrss-directory))))
	  (insert-file-contents file)
	(setq url (or (nth 2 (assoc group nnrss-server-data))
		      (second (assoc group nnrss-group-alist))))
	(unless url
	  (setq url
		(read-string (format "RSS url of %s: " group "http://")))
	  (let ((pair (assoc group nnrss-server-data)))
	    (if pair
		(setcdr (cdr pair) (list url))
	      (push (list group nnrss-group-max url) nnrss-server-data)))
	  (setq changed t))
	(nnrss-insert url))
      (goto-char (point-min))
      (while (re-search-forward "\r\n?" nil t)
	(replace-match "\n"))
      (goto-char (point-min))
      (if (re-search-forward "<rdf\\|<rss" nil t)
	  (goto-char (match-beginning 0)))
      (setq xml (xml-parse-region (point) (point-max))))
    (while (and xml (not (assq 'item xml)))
      (unless (listp (car (setq xml (cddar xml))))
	(setq xml nil)))
    (dolist (item xml)
       (when (and (listp item)
		  (eq 'item (car item))
		  (setq url (caddr (assq 'link (cddr item))))
		  (setq url (nnrss-decode-entities-unibyte-string url))
		  (not (gnus-gethash url nnrss-group-hashtb)))
	 (setq subject (caddr (assq 'title (cddr item))))
	 (setq extra (or (caddr (assq 'description (cddr item)))
			 (caddr (assq 'dc:description (cddr item)))))
	 (setq author (caddr (assq 'dc:creator (cddr item))))
	 (setq date (or (caddr (assq 'dc:date (cddr item)))
			(message-make-date)))
	 (push
	  (list
	   (incf nnrss-group-max)
	   (time-to-days (current-time))
	   url
	   (and subject (nnrss-decode-entities-unibyte-string subject))
	   (and author (nnrss-decode-entities-unibyte-string author))
	   date
	   (and extra (nnrss-decode-entities-unibyte-string extra)))
	  nnrss-group-data)
	 (gnus-sethash url (car nnrss-group-data) nnrss-group-hashtb)
	 (setq changed t)))
    (when changed
	(nnrss-save-group-data group server)
	(let ((pair (assoc group nnrss-server-data)))
	  (if pair
	      (setcar (cdr pair) nnrss-group-max)
	    (push (list group nnrss-group-max) nnrss-server-data)))
	(nnrss-save-server-data server))))

(provide 'nnrss)

;;; nnrss.el ends here
