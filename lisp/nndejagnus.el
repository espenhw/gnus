;;; nndejagnus.el --- retrieving articles via DejaNews
;; Copyright (C) 1996 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@ifi.uio.no>
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

(require 'nnoo)
(require 'message)
(require 'gnus-util)
(require 'w3-forms)
(require 'url)

(nnoo-declare nndejagnus)

(defvoo nndejagnus-address "http://www.dejagnus.com/"
  "Base URL of the DejaNews search engine.")

(defvoo nndejagnus-search nil
  "Search string to feed to DejaNews.")

(defvoo nndejagnus-max-hits 100
  "Maximum number of hits to display.")

;;; Internal variables

(defvoo nndejagnus-articles nil)
(defvoo nndejagnus-buffer nil)
(defvoo nndejagnus-async-buffer nil)
(defvar nndejagnus-callback-function nil)
(defvar nndejagnus-to-buffer nil)
(defvar nndejagnus-start-point nil)
(defvar nndejagnus-inside-change-function nil)

;;; Interface functions

(nnoo-define-basics nndejagnus)

(deffoo nndejagnus-retrieve-headers (articles &optional group server fetch-old)
  (nndejagnus-possibly-change-server server)
  (save-excursion
    (set-buffer nntp-server-buffer)
    (erase-buffer)
    (let (article header)
      (while (setq article (pop articles))
	(when (setq header (cadr (assq article nndejagnus-articles)))
	  (nnheader-insert-nov header)))
      'nov)))

(deffoo nndejagnus-request-group (group &optional server dont-check)
  (nndejagnus-possibly-change-server server)
  (when (or (not dont-check)
	    (not nndejagnus-articles))
    (nndejagnus-create-mapping group))
  (cond
   ((not nndejagnus-articles)
    (nnheader-report 'nndejagnus "Couldn't request search"))
   (t
    (nnheader-report 'nndejagnus "Opened group %s" group)
    (nnheader-insert
     "211 %d %d %d %s\n" (length nndejagnus-articles)
     (caar nndejagnus-articles) (caar (last nndejagnus-articles))
      group))))

(deffoo nndejagnus-request-article (article &optional group server buffer)
  (nndejagnus-possibly-change-server server)
  (save-excursion
    (set-buffer (or buffer nntp-server-buffer))
    (let ((url (caddr (assq article nndejagnus-articles))))
      (when (and url
		 (nndejagnus-fetch-url url))
	(unless nnheader-callback-function
	  (nndejagnus-decode-article)
	  (nndejagnus-decode-entities))
	(nnheader-report 'nndejagnus "Fetched article %s" article)
	t))))

(deffoo nndejagnus-close-server (&optional server)
  (when (nndejagnus-server-opened server)
    (gnus-kill-buffer nndejagnus-buffer))
  (nnoo-close-server 'nndejagnus server))

(deffoo nndejagnus-request-update-info (group info &optional server)
  (nndejagnus-possibly-change-server server)
  (setcar (cddr info) nil))

(deffoo nndejagnus-asynchronous-p ()
  t)

(nnoo-define-skeleton nndejagnus)

;;; Internal functions

(defun nndejagnus-possibly-change-server (&optional server)
  (nndejagnus-init server)
  (when server
    (unless (nndejagnus-server-opened server)
      (nndejagnus-open-server server))))

(defun nndejagnus-init (server)
  "Initialize buffers and such."
  (unless (gnus-buffer-live-p nndejagnus-buffer)
    (setq nndejagnus-buffer
	  (save-excursion
	    (nnheader-set-temp-buffer
	     (format " *nndejagnus %s*" server))))))

(defun nndejagnus-create-mapping (group)
  "Perform the search and create an number-to-url alist."
  (save-excursion
    (set-buffer nndejagnus-buffer)
    (erase-buffer)
    (when (nndejagnus-fetch-search nndejagnus-search)
      (let ((i 0)
	    (more t)
	    Subject Score Date Newsgroup Author
	    map url)
	(while more
	  ;; Go through all the article hits on this page.
	  (goto-char (point-min))
	  (nndejagnus-decode-entities)
	  (goto-char (point-min))
	  (while (re-search-forward "^ +[0-9]+\\." nil t)
	    (narrow-to-region 
	     (point) 
	     (if (re-search-forward "^ +[0-9]+\\." nil t)
		 (match-beginning 0)
	       (point-max)))
	    (goto-char (point-min))
	    (when (looking-at ".*HREF=\"\\([^\"]+\\)\"")
	      (setq url (match-string 1)))
	    (while (re-search-forward "<[^>]+>" nil t)
	      (replace-match "" t t))
	    (goto-char (point-min))
	    (while (search-forward "\t" nil t)
	      (replace-match " "))
	    (goto-char (point-min))
	    (while (re-search-forward "^ +\\([^:]+\\): +\\(.*\\)$" nil t)
	      (set (intern (match-string 1)) (match-string 2)))
	    (widen)
	    (when (string-match "#[0-9]+/[0-9]+ *$" Subject)
	      (setq Subject (substring Subject 0 (match-beginning 0))))
	    (push
	     (list
	      (incf i)
	      (make-full-mail-header
	       i (concat  "(" Newsgroup ") " Subject) Author Date
	       (concat "<" (message-unique-id) "-" (int-to-string i)
		       "@dejanews>")
	       nil 0 (string-to-int Score) nil)
	      url)
	     map))
	  ;; See whether there is a "Get next 20 hits" button here.
	  (if (or (not (re-search-forward
			"HREF=\"\\([^\"]+\\)\">Get next" nil t))
		  (> i nndejagnus-max-hits))
	      (setq more nil)
	    ;; Yup -- fetch it.
	    (setq more (match-string 1))
	    (erase-buffer)
	    (url-insert-file-contents more)))
	;; Return the articles in the right order.
	(setq nndejagnus-articles (nreverse map))))))

(defun nndejagnus-fetch-url (url)
  (save-excursion
    (if (not nnheader-callback-function)
	(let ((buf (current-buffer)))
	  (save-excursion
	    (set-buffer nndejagnus-buffer)
	    (erase-buffer)
	    (prog1
		(url-insert-file-contents url)
	      (copy-to-buffer buf (point-min) (point-max)))))
      (nndejagnus-url-retrieve-asynch
       url 'nndejanews-callback (current-buffer) nnheader-callback-function)
      t)))

(defun nndejanews-callback (buffer callback)
  (save-excursion
    (set-buffer url-working-buffer)
    (nndejagnus-decode-article)
    (nndejagnus-decode-entities)
    (set-buffer buffer)
    (goto-char (point-max))
    (insert-buffer-substring url-working-buffer))
  (funcall callback t)
  (gnus-kill-buffer url-working-buffer))

(defun nndejagnus-url-retrieve-asynch (url callback &rest data)
  (let ((url-request-method "GET")
	(old-asynch url-be-asynchronous)
	(url-request-data nil)
	(url-request-extra-headers nil)
	(url-working-buffer (generate-new-buffer-name " *dejanews*")))
    (setq-default url-be-asynchronous t)
    (save-excursion
      (set-buffer (get-buffer-create url-working-buffer))
      (setq url-current-callback-data data
	    url-be-asynchronous t
	    url-current-callback-func callback)
      (url-retrieve url))
    (setq-default url-be-asynchronous old-asynch)))

(defun nndejagnus-decode-article ()
  (goto-char (point-min))
  (re-search-forward "<PRE>" nil t)
  (delete-region (point-min) (point))
  (re-search-forward "</PRE>" nil t)
  (delete-region (point) (point-max))
  (goto-char (point-min))
  (while (re-search-forward "<[^>]+>" nil t)
    (replace-match "" t t))
  (goto-char (point-min))
  (while (looking-at " *$")
    (gnus-delete-line))
  (while (looking-at "\\(^[^ ]+:\\) *")
    (replace-match "\\1 " t)
    (forward-line 1))
  (when (re-search-forward "\n\n+" nil t)
    (replace-match "\n" t t)))

(defun nndejagnus-encode-www-form-urlencoded (pairs)
  "Return PAIRS encoded for forms."
  (mapconcat 
    (function
      (lambda (data)
        (concat (w3-form-encode-xwfu (car data)) "="
                (w3-form-encode-xwfu (cdr data))))) pairs "&"))

(defun nndejagnus-fetch-form (url pairs)
  (let ((url-request-data (nndejagnus-encode-www-form-urlencoded pairs))
	(url-request-method 'POST)
	(url-request-extra-headers 
	 '(("Content-type" . "application/x-www-form-urlencoded"))))
    (url-insert-file-contents url)))

(defun nndejagnus-fetch-search (search)
  (nndejagnus-fetch-form 
   "http://xp6.dejanews.com/dnquery.xp"
   `(("query" . ,search)
     ("defaultOp" . "AND")
     ("svcclass" . "dncurrent")
     ("maxhits" . "25")
     ("format" . "verbose")
     ("threaded" . "0")
     ("showsort" . "score")
     ("agesign" . "1")
     ("ageweight" . "1"))))

(defun nndejagnus-decode-entities ()
  (goto-char (point-min))
  (while (re-search-forward "&\\([a-z]+\\);" nil t)
    (replace-match (char-to-string (or (cdr (assq (intern (match-string 1))
						  w3-html-entities ))
				       ?#))
		   t t)))

;		"^ +\\([0-9]+\\)\\. +\\([0-9]+\\)/+\\([0-9]+\\)/+\\([0-9]+\\) +\\([0-9]+\\).+HREF=\"\\([^\"]+\\)\">\\([^<]+\\)<.*<B> *\\(.*\\)</B>.+>\\([^<>]+\\)</A> *$"

(provide 'nndejagnus)

;;; nndejagnus.el ends here
