;;; webmail.el --- interfacing with web mail
;; Copyright (C) 1999 Free Software Foundation, Inc.

;; Author: Shenghuo Zhu <zsh@cs.rochester.edu>
;; Keywords: hotmail

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

;; Todo: To support more web mail.

;; Known bugs: 
;; 1. In w3, there are two copies of url-maybe-relative.
;;    If it is loaded from w3.el, (load-library "url"). 
;;    Fixed in w3 4.0pre46.
;; 2. Hotmail only accept one line cookie, while w3 breaks cookies 
;;    into lines.
;;    Maybe fixed in w3 4.0pre47+?.

;; Warning:
;; webmail is an experimental function, which means NO WARRANTY.

;;; Code:

(eval-when-compile (require 'cl))

(require 'nnoo)
(require 'message)
(require 'gnus-util)
(require 'gnus)
(require 'nnmail)
(require 'mm-util)
(require 'mail-source)
(require 'mml)
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


;;;

(defvar webmail-type-definition
  '((hotmail
     ;; Hotmail hate other HTTP user agents and use one line cookie
     (paranoid agent cookie)
     (address . "www.hotmail.com")
     (open-url "http://www.hotmail.com")
     (open-snarf . webmail-hotmail-open)
     ;; W3 hate redirect POST
     (login-url
      "http://%s/cgi-bin/dologin?login=%s&passwd=%s&enter=Sign+in&sec=no&curmbox=ACTIVE&_lang=&js=yes&id=2&tw=-10000&beta="
       webmail-aux user password)
     (trash-url 
      "%s&login=%s&f=33792&curmbox=ACTIVE&_lang=&js=&foo=inbox&page=&%s=on&Move+To.x=Move+To&tobox=trAsH" 
      webmail-aux user id)    
     (list-snarf . webmail-hotmail-list)
     (article-snarf . webmail-hotmail-article))))

(defvar webmail-variables
  '(address article-snarf article-url list-snarf list-url 
	    login-url login-snarf open-url open-snarf site articles
	    post-process paranoid trash-url))

(defconst webmail-version "webmail 1.0")

(defvar webmail-newmail-only nil
  "Only fetch new mails.")

(defvar webmail-move-to-trash-can t
  "Move mail to trash can after fetch it.")

;;; Internal variables

(defvar webmail-address nil)
(defvar webmail-paranoid nil)
(defvar webmail-aux nil)
(defvar webmail-article-snarf nil)
(defvar webmail-article-url nil)
(defvar webmail-list-snarf nil)
(defvar webmail-list-url nil)
(defvar webmail-login-url nil)
(defvar webmail-login-snarf nil)
(defvar webmail-open-snarf nil)
(defvar webmail-open-url nil)
(defvar webmail-trash-url nil)
(defvar webmail-articles nil)
(defvar webmail-post-process nil)

(defvar webmail-buffer nil)
;;; Interface functions

(defun webmail-setdefault (type)
  (let ((type-def (cdr (assq type webmail-type-definition)))
	(vars webmail-variables)
	pair)
    (dolist (var vars)
      (if (setq pair (assq var type-def))
	  (set (intern (concat "webmail-" (symbol-name var))) (cdr pair))
	(set (intern (concat "webmail-" (symbol-name var))) nil)))))

(defun webmail-encode-www-form-urlencoded (pairs)
  "Return PAIRS encoded for forms."
  (mapconcat
   (function
    (lambda (data)
      (concat (w3-form-encode-xwfu (car data)) "="
	      (w3-form-encode-xwfu (cdr data)))))
   pairs "&"))

(defun webmail-fetch-simple (url content)
  (let ((url-request-data content)
	(url-request-method "POST")
	(url-request-extra-headers
	 '(("Content-type" . "application/x-www-form-urlencoded"))))
    (nnweb-insert url))
  t)

(defun webmail-fetch-form (url pairs)
  (let ((url-request-data (webmail-encode-www-form-urlencoded pairs))
	(url-request-method "POST")
	(url-request-extra-headers
	 '(("Content-type" . "application/x-www-form-urlencoded"))))
    (nnweb-insert url))
  t)

(defun webmail-eval (expr)
  (cond
   ((consp expr)
    (cons (webmail-eval (car expr)) (webmail-eval (cdr expr))))
   ((symbolp expr)
    (eval expr))
   (t
    expr)))

(defun webmail-url (xurl)
  (let ((url-confirmation-func 'identity))
    (cond 
     ((eq (car xurl) 'content)
      (pop xurl)
      (webmail-fetch-simple (if (stringp (car xurl))
				(car xurl)
			      (apply 'format (webmail-eval (car xurl))))
			    (apply 'format (webmail-eval (cdr xurl)))))
     ((eq (car xurl) 'post)
      (pop xurl)
      (webmail-fetch-form (car xurl) (webmail-eval (cdr xurl))))
     (t
      (nnweb-insert (apply 'format (webmail-eval xurl)))))))

(defun webmail-decode-entities ()
  (goto-char (point-min))
  (while (re-search-forward "&\\(#[0-9]+\\|[a-z]+\\);" nil t)
    (replace-match (char-to-string 
		    (if (eq (aref (match-string 1) 0) ?\#)
			(string-to-number (substring (match-string 1) 1))
		      (or (cdr (assq (intern (match-string 1))
				     w3-html-entities))
			  ?#)))
		   t t)))

(defun webmail-decode-entities-string (str)
  (with-temp-buffer
    (insert str)
    (webmail-decode-entities)
    (buffer-substring (point-min) (point-max))))

(defun webmail-remove-markup ()
  (goto-char (point-min))
  (while (search-forward "<!--" nil t)
    (delete-region (match-beginning 0)
		   (or (search-forward "-->" nil t)
		       (point-max))))
  (goto-char (point-min))
  (while (re-search-forward "<[^>]+>" nil t)
    (replace-match "" t t)))

(defun webmail-init ()
  "Initialize buffers and such."
  (if (gnus-buffer-live-p webmail-buffer)
      (set-buffer webmail-buffer)
    (setq webmail-buffer
	  (nnheader-set-temp-buffer " *webmail*"))))

(defvar url-package-name)
(defvar url-package-version)
(defvar url-cookie-multiple-line)

(defun webmail-fetch (file wmtype user password)
  (webmail-setdefault wmtype)
  (let ((url-package-name (if (memq 'agent webmail-paranoid)
			      "Mozilla"
			    url-package-name))
	(url-package-version (if (memq 'agent webmail-paranoid)
				 "4.0"
			       url-package-version))
	(url-cookie-multiple-line (if (memq 'cookie webmail-paranoid)
				      nil
				    url-cookie-multiple-line)))
    (webmail-init)
    (when webmail-open-url 
      (erase-buffer)
      (webmail-url webmail-open-url))
    (if webmail-open-snarf (funcall webmail-open-snarf))
    (when webmail-login-url 
      (erase-buffer)
      (webmail-url webmail-login-url))
    (if webmail-login-snarf 
	(funcall webmail-login-snarf))
    (when webmail-list-url 
      (erase-buffer)
      (webmail-url webmail-list-url))
    (if webmail-list-snarf 
	(funcall webmail-list-snarf))
    (let (item id (n 0))
      (while (setq item (pop webmail-articles))
	(message "Fetching mail #%d..." (setq n (1+ n)))
	(erase-buffer)
	(nnweb-insert (cdr item))
	(setq id (car item))
	(if webmail-article-snarf 
	    (funcall webmail-article-snarf file id))
	(when (and webmail-trash-url webmail-move-to-trash-can)
	  (message "Move mail #%d to trash can..." n)
	  (webmail-url webmail-trash-url))))
    (if webmail-post-process
	(funcall webmail-post-process))))

;;; hotmail

(defun webmail-hotmail-open ()
  (goto-char (point-min))
  (if (re-search-forward 
       "action=\"https?://\\([^/]+\\)/cgi-bin/dologin" nil t)
      (setq webmail-aux (match-string 1))
    (error "Can't find login url (open@1)")))

(defun webmail-hotmail-list ()
  (let (site url newp)
    (goto-char (point-min))
    (if (re-search-forward "[0-9]+ messages, [0-9]+ new") 
	(message "Found %s" (match-string 0)))
    (goto-char (point-min))
    (if (re-search-forward 
	 "action=\"https?://\\([^/]+\\)/cgi-bin/HoTMaiL" nil t)
	(setq site (match-string 1))
      (error "Can't find server url (list@1)"))
    (goto-char (point-min))
    (if (re-search-forward "disk=\\([^&]+\\)&" nil t)
	(setq webmail-aux 
	      (concat "http://" site "/cgi-bin/HoTMaiL?disk=" 
		      (match-string 1)))
      (error "Can't find disk (list@2)"))
    (goto-char (point-max))
    (while (re-search-backward 
	    "newmail\\.gif\\|href=\"\\(/cgi-bin/getmsg\\?[^\"]+\\)\"" 
	    nil t)
      (if (setq url (match-string 1))
	  (progn
	    (if (or newp (not webmail-newmail-only))
		(let (id)
		  (if (string-match "msg=\\([^&]+\\)" url)
		      (setq id (match-string 1 url)))
		  (push (cons id (concat "http://" site url)) 
			webmail-articles)))
	    (setq newp nil))
	(setq newp t)))))

(defun webmail-hotmail-article (file id)
  (let (p attachment count tbufs mime)
    (save-restriction
      (goto-char (point-min))
      (if (not (search-forward "FILE: wc_pnames.asp -->" nil t))
	  (error "Can't find start label (article@1)"))
      (setq p (match-end 0))
      (search-backward "<table" nil t)
      (narrow-to-region (point-min) p)
      (delete-region (point-min) (match-beginning 0)) 
      (while (search-forward "<a href=" nil t)
	(setq p (match-beginning 0))
	(search-forward "</a>" nil t)
	(delete-region p (match-end 0)))
      (webmail-remove-markup)
      (webmail-decode-entities)
      (goto-char (point-min))
      (delete-blank-lines)
      (goto-char (point-max))
      (widen)
      (insert "\n")
      (setq p (point))
      (while (re-search-forward "<div>\\|\\(http://[^/]+/cgi-bin/getmsg/\\([^\?]+\\)\?[^\"]*\\)\"" nil t)
	(if (setq attachment (match-string 1))
	    (let ((filename (match-string 2))
		  bufname) ;; Attachment
	      (delete-region p (match-end 0))
	      (save-excursion
		(set-buffer (generate-new-buffer " *webmail-att*"))
		(nnweb-insert attachment)
		(push (current-buffer) tbufs)
		(setq bufname (buffer-name)))
	      (setq mime t)
	      (insert "<#part type=" 
		      (or (and filename
			       (string-match "\\.[^\\.]+$" filename)
			       (mailcap-extension-to-mime
				(match-string 0 filename)))
			  "application/octet-stream"))
	      (insert " buffer=\"" bufname "\"")
	      (insert " filename=\"" filename "\"")
	      (insert " description=\"inline\"")
	      (insert "><#/part>\n")
	      (setq p (point)))
	  (delete-region p (match-end 0))
	  (setq count 1)
	  (while (and (> count 0) 
		      (re-search-forward "</div>\\|\\(<div>\\)" nil t))
	    (if (match-string 1)
		(setq count (1+ count))
	      (if (= (setq count (1- count)) 0)
		  (delete-region (match-beginning 0)
				 (match-end 0)))))
	  (narrow-to-region p (point))
	  (goto-char (point-min))
	  (cond 
	   ((looking-at "<pre>")
	    (goto-char (match-end 0))
	    (if (looking-at "$") (forward-char))
	    (delete-region (point-min) (point))
	    (webmail-remove-markup)
	    (webmail-decode-entities)
	    nil)
	   (t
	    (setq mime t)
	    (insert "<#part type=\"text/html\" disposition=inline>")
	    (goto-char (point-max))
	    (insert "<#/part>")))
	  (goto-char (point-max))
	  (setq p (point))
	  (widen)))
      (delete-region p (point-max))
      (goto-char (point-min))
      ;; Some blank line to seperate mails.
      (insert "\n\nFrom nobody " (current-time-string) "\n")
      (if id
	  (insert "Message-ID: <" id "@hotmail.com>\n"))
      (unless (looking-at "$") 
	(search-forward "\n\n" nil t)
	(forward-line -1))
      (narrow-to-region (point) (point-max))
      (if mime
	  (insert "MIME-Version: 1.0\n"
		  (prog1
		      (mml-generate-mime)
		    (delete-region (point-min) (point-max)))))
      (goto-char (point-min))
      (widen)
      (let (case-fold-search)
	(while (re-search-forward "^From " nil t)
	  (beginning-of-line)
	  (insert ">"))))
    (mm-append-to-file (point-min) (point-max) file)
    (dolist (buf tbufs)
      (kill-buffer buf))))

(provide 'webmail)

;;; webmail.el ends here
