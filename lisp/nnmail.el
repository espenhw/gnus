;;; nnmail.el --- mail mbox access for Gnus

;; Copyright (C) 1994 Free Software Foundation, Inc.

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

;;; Code:

(require 'nnheader)
(require 'rmail)

(defvar nnmail-split-methods
  '(("mail.misc" ""))
  "nnmail will split incoming mail into the groups detailed in this variable.")

(defvar nnmail-mbox-file (expand-file-name "~/mbox")
  "The name of the mail box file in the users home directory.")

(defvar nnmail-active-file (expand-file-name "~/.mbox-active")
  "The name of the active file for the mail box.")

(defvar nnmail-expiry-wait 7
  "Articles that are older than `nnmail-expiry-wait' days will be expired.")

;; Quote fix by Sudish Joseph <joseph@cis.ohio-state.edu>.
(defvar nnmail-expiry-wait-function nil
  "Variable that holds funtion to specify how old articles should be before they are expired.
  The function will be called with the name of the group that the
expiry is to be performed in, and it should return an integer that
says how many days an article can be stored before it is considered
'old'. 

Eg.:

(setq nnmail-expiry-wait-function
  (function
    (lambda (newsgroup)
      (cond ((string-match \"private\" newsgroup) 31)
            ((string-match \"junk\" newsgroup) 1)
	    (t 7)))))")

(defvar nnmail-spool-file 
  (or (getenv "MAIL")
      (concat "/usr/spool/mail/" (user-login-name))))

(defvar nnmail-read-incoming-hook nil
  "Hook that will be run after the incoming mail has been transferred.
The incoming mail is moved from `nnmail-spool-file' (which normally is
something like \"/usr/spool/mail/$user\") to the user's home
directory. This hook is called after the incoming mail box has been
emptied, and can be used to call any mail box programs you have
running (\"xwatch\", etc.)

Eg.

(add-hook 'nnmail-read-incoming-hook 
	  (function
	   (lambda () 
	     (start-process \"mailsend\" nil 
			    \"/local/bin/mailsend\" \"read\" \"mbox\"))))")

(defvar nnmail-large-newsgroup 50
  "*The number of the articles which indicates a large newsgroup.
If the number of the articles is greater than the value, verbose
messages will be shown to indicate the current status.")



(defconst nnmail-version "nnmail 0.1"
  "nnmail version.")

(defvar nnmail-current-group nil
  "Current nnmail news group directory.")

(defconst nnmail-mbox-buffer "*nnmail mbox buffer*")

(defvar nnmail-active-alist nil)

(defvar nnmail-status-string "")

;;; Interface functions

(defun nnmail-retrieve-headers (sequence &optional newsgroup server)
  "Retrieve the headers for the articles in SEQUENCE.
Newsgroup must be selected before calling this function."
  (save-excursion
    (set-buffer nntp-server-buffer)
    (erase-buffer)
    (let ((file nil)
	  (number (length sequence))
	  (count 0)
	  beg article art-string start stop)
      (nnmail-possibly-change-newsgroup newsgroup)
      (while sequence
	(setq article (car sequence))
	(setq art-string (nnmail-article-string article))
	(set-buffer nnmail-mbox-buffer)
	(if (or (search-forward art-string nil t)
		(progn (goto-char 1)
		       (search-forward art-string nil t)))
	    (progn
	      (setq start 
		    (save-excursion
		      (re-search-backward 
		       (concat "^" rmail-unix-mail-delimiter) nil t)
		      (point)))
	      (search-forward "\n\n" nil t)
	      (setq stop (1- (point)))
	      (set-buffer nntp-server-buffer)
	      (insert (format "221 %d Article retrieved.\n" article))
	      (setq beg (point))
	      (insert-buffer-substring nnmail-mbox-buffer start stop)
	      (goto-char (point-max))
	      (insert ".\n")))
	(setq sequence (cdr sequence))
	(setq count (1+ count))
	(and (numberp nnmail-large-newsgroup)
	     (> number nnmail-large-newsgroup)
	     (zerop (% count 20))
	     (message "NNMAIL: Receiving headers... %d%%"
		      (/ (* count 100) number))))

      (and (numberp nnmail-large-newsgroup)
	   (> number nnmail-large-newsgroup)
	   (message "NNMAIL: Receiving headers... done"))

      ;; Fold continuation lines.
      (goto-char 1)
      (while (re-search-forward "\\(\r?\n[ \t]+\\)+" nil t)
	(replace-match " " t t))
      'headers)))

(defun nnmail-open-server (host &optional service)
  "Open mbox backend."
  (setq nnmail-status-string "")
  (nnmail-open-server-internal host service))

(defun nnmail-close-server (&optional server)
  "Close news server."
  (nnmail-close-server-internal))

(fset 'nnmail-request-quit (symbol-function 'nnmail-close-server))

(defun nnmail-server-opened (&optional server)
  "Return server process status, T or NIL.
If the stream is opened, return T, otherwise return NIL."
  (and nntp-server-buffer
       (get-buffer nntp-server-buffer)))

(defun nnmail-status-message ()
  "Return server status response as string."
  nnmail-status-string)

(defun nnmail-request-article (article &optional newsgroup server buffer)
  "Select ARTICLE by number."
  (nnmail-possibly-change-newsgroup newsgroup)
  (if (stringp article)
      nil
    (save-excursion
      (set-buffer nnmail-mbox-buffer)
      (goto-char 1)
      (if (search-forward (nnmail-article-string article) nil t)
	  (let (start stop)
	    (re-search-backward (concat "^" rmail-unix-mail-delimiter) nil t)
	    (setq start (point))
	    (forward-line 1)
	    (or (and (re-search-forward 
		      (concat "^" rmail-unix-mail-delimiter) nil t)
		     (forward-line -1))
		(goto-char (point-max)))
	    (setq stop (point))
	    (let ((nntp-server-buffer (or buffer nntp-server-buffer)))
	      (set-buffer nntp-server-buffer)
	      (erase-buffer)
	      (insert-buffer-substring nnmail-mbox-buffer start stop)
	      t))))))

(defun nnmail-request-group (group &optional server dont-check)
  "Select news GROUP."
  (if (nnmail-possibly-change-newsgroup group)
      (if dont-check
	  t
	(nnmail-get-new-mail)
	(save-excursion
	  (set-buffer nntp-server-buffer)
	  (erase-buffer)
	  (let ((active (assoc group nnmail-active-alist)))
	    (insert (format "211 %d %d %d %s\n" 
			    (1+ (- (cdr (car (cdr active)))
				   (car (car (cdr active)))))
			    (car (car (cdr active)))
			    (cdr (car (cdr active)))
			    (car active))))
	  t))))

(defun nnmail-request-list (&optional server)
  "List active newsgoups."
  (nnmail-find-file nnmail-active-file))

(defun nnmail-request-list-newsgroups (&optional server)
  "List newsgroups (defined in NNTP2)."
  (setq nntp-status-string "NNMAIL: LIST NEWSGROUPS is not implemented.")
  nil)

(defun nnmail-request-post (&optional server)
  "Post a new news in current buffer."
  (mail-send-and-exit nil))

(defun nnmail-request-post-buffer (method header article-buffer group info)
  (let ((method-address (nth 1 (nth 4 info)))
	from subject date to reply-to message-of
	references message-id sender follow-to)
    (setq method-address
	  (if (and (stringp method-address) 
		   (string= method-address ""))
	      nil method-address))
    (save-excursion
      (set-buffer (get-buffer-create "*mail*"))
      (mail-mode)
      (local-set-key "\C-c\C-c" 'gnus-mail-send-and-exit)
      (local-set-key "\C-c\C-y" 'gnus-mail-yank-original)
      (if (and (buffer-modified-p)
	       (> (buffer-size) 0)
	       (not (y-or-n-p "Unsent mail being composed; erase it? ")))
	  ()
	(erase-buffer)
	(if (eq method 'post)
	    (mail-setup method-address nil nil nil nil nil)
	  (save-excursion
	    (set-buffer article-buffer)
	    (goto-char (point-min))
	    (narrow-to-region (point-min)
			      (progn (search-forward "\n\n") (point)))
	    (set-text-properties (point-min) (point-max) nil)
	    (if (and (boundp 'gnus-followup-to-function)
		     gnus-followup-to-function)
		(setq follow-to (funcall gnus-followup-to-function group)))
	    (setq from (header-from header))
	    (setq date (header-date header))
	    (and from
		 (let ((stop-pos 
			(string-match "  *at \\|  *@ \\| *(\\| *<" from)))
		   (setq message-of
			 (concat (if stop-pos (substring from 0 stop-pos) from)
				 "'s message of " date))))
	    (setq sender (mail-fetch-field "sender"))
	    (setq subject (header-subject header))
	    (or (string-match "^[Rr][Ee]:" subject)
		(setq subject (concat "Re: " subject)))
	    (setq reply-to (mail-fetch-field "reply-to"))
	    (setq references (header-references header))
	    (setq message-id (header-id header))
	    (widen))
	  (setq news-reply-yank-from from)
	  (setq news-reply-yank-message-id message-id)
	  (mail-setup (or follow-to method-address sender reply-to from)
		      subject message-of nil article-buffer nil)
	  ;; Fold long references line to follow RFC1036.
	  (mail-position-on-field "References")
	  (let ((begin (- (point) (length "References: ")))
		(fill-column 78)
		(fill-prefix "\t"))
	    (if references (insert references))
	    (if (and references message-id) (insert " "))
	    (if message-id (insert message-id))
	    ;; The region must end with a newline to fill the region
	    ;; without inserting extra newline.
	    (fill-region-as-paragraph begin (1+ (point))))
	  ))
      (current-buffer))))

(defun nnmail-request-expire-articles (articles newsgroup &optional server)
  "Expire all articles in the ARTICLES list in group GROUP.
The list of unexpired articles will be returned (ie. all articles that
were too fresh to be expired)."
  (nnmail-possibly-change-newsgroup newsgroup)
  (let* ((days (or (and nnmail-expiry-wait-function
			(funcall nnmail-expiry-wait-function newsgroup))
		   nnmail-expiry-wait))
	 (cur-time (current-time))
	 (day-sec (* 24 60 60 days))
	 (day-time (list nil nil))
	 mod-time article rest)
    (setcar day-time (/ day-sec 65536))
    (setcar (cdr day-time) (- day-sec (* (car day-time) 65536)))
    (if (< (car (cdr cur-time)) (car (cdr day-time)))
	(progn
	  (setcar day-time (+ 1 (- (car cur-time) (car day-time))))
	  (setcar (cdr day-time) (- (+ 65536 (car (cdr cur-time)))
				    (car (cdr day-time)))))
      (setcar day-time (- (car cur-time) (car day-time)))
      (setcar (cdr day-time) (- (car (cdr cur-time)) (car (cdr day-time)))))
    (save-excursion 
      (set-buffer nnmail-mbox-buffer)
      (while articles
	(goto-char 1)
	(if (and (search-forward (nnmail-article-string (car articles)) nil t)
		 (setq mod-time (read (current-buffer)))
		 (or (< (car mod-time) (car day-time))
		     (and (= (car mod-time) (car day-time))
			  (< (car (cdr mod-time)) (car (cdr day-time))))))
	    (progn
	      (message "Deleting: %s" article)
	      (nnmail-delete-mail))
	  (setq rest (cons (car articles) rest)))
	(setq articles (cdr articles)))
      (save-buffer)
      rest)))

(defun nnmail-request-move-article (article group server accept-form)
  (let ((buf (get-buffer-create " *nnmail move*"))
	result)
    (and 
     (nnmail-request-article article group server)
     (save-excursion
       (set-buffer buf)
       (insert-buffer-substring nntp-server-buffer)
       (goto-char (point-min))
       (if (re-search-forward 
	    "^X-Gnus-Newsgroup:" 
	    (save-excursion (search-forward "\n\n" nil t) (point)) t)
	   (delete-region (progn (beginning-of-line) (point))
			  (progn (forward-line 1) (point))))
       (setq result (eval accept-form))
       (kill-buffer (current-buffer))
       result)
     (save-excursion
       (set-buffer nnmail-mbox-buffer)
       (goto-char 1)
       (if (search-forward (nnmail-article-string article) nil t)
	   (nnmail-delete-mail))
       (save-buffer)))
    result))

(defun nnmail-request-accept-article (group)
  (let ((buf (current-buffer))
	result beg)
    (and 
     (nnmail-get-active)
     (save-excursion
       (set-buffer nnmail-mbox-buffer)
       (setq beg (goto-char (point-max)))
       (insert-buffer-substring buf)
       (goto-char beg)
       (if (stringp group)
	   (progn
	     (search-forward "\n\n" nil t)
	     (forward-line -1)
	     (setq result (nnmail-insert-newsgroup-line group beg (point))))
	 (setq result (nnmail-choose-mail beg (point-max))))
       (save-buffer)
       result)
     (nnmail-save-active))
    (debug)
    result))


;;; Low-Level Interface

(defun nnmail-delete-mail ()
  (re-search-backward (concat "^" rmail-unix-mail-delimiter) nil t)
  (delete-region 
   (point)
   (progn
     (forward-line 1)
     (or (and (re-search-forward 
	       (concat "^" rmail-unix-mail-delimiter) nil t)
	      (forward-line -1)
	      (point))
	 (point-max)))))

(defun nnmail-open-server-internal (host &optional service)
  "Open connection to news server on HOST by SERVICE (default is nntp)."
  (save-excursion
    (if (not (string-equal host (system-name)))
	(error "NNMAIL: cannot talk to %s." host))
    ;; Initialize communication buffer.
    (setq nntp-server-buffer (get-buffer-create " *nntpd*"))
    (set-buffer nntp-server-buffer)
    (buffer-disable-undo (current-buffer))
    (erase-buffer)
    (kill-all-local-variables)
    (setq case-fold-search t)		;Should ignore case.
    t))

(defun nnmail-close-server-internal ()
  "Close connection to news server."
  nil)

(defun nnmail-find-file (file)
  "Insert FILE in server buffer safely."
  (save-excursion
    (set-buffer nntp-server-buffer)
    (erase-buffer)
    (condition-case ()
	(progn (insert-file-contents file) t)
      (file-error nil))))

(defun nnmail-possibly-change-newsgroup (newsgroup)
  (if (not (get-buffer nnmail-mbox-buffer))
      (save-excursion
	(set-buffer (setq nnmail-mbox-buffer 
			  (find-file-noselect nnmail-mbox-file)))
	(buffer-disable-undo (current-buffer))))
  (if (not nnmail-active-alist)
      (nnmail-get-active))
  (if newsgroup
      (if (assoc newsgroup nnmail-active-alist)
	  (setq nnmail-current-group newsgroup))))

;; Most of this function was taken from rmail.el
(defun nnmail-move-inbox ()
  (let ((inbox (expand-file-name nnmail-spool-file))
	tofile errors)
    (setq tofile (make-temp-name
		  (expand-file-name (concat nnmail-mbox-file "-Incoming"))))
    (unwind-protect
	(save-excursion
	  (setq errors (generate-new-buffer " *nnmail loss*"))
	  (buffer-disable-undo errors)
	  (call-process
	   (expand-file-name "movemail" exec-directory)
	   nil errors nil inbox tofile)
	  (if (not (buffer-modified-p errors))
	      ;; No output => movemail won
	      nil
	    (set-buffer errors)
	    (subst-char-in-region (point-min) (point-max) ?\n ?\  )
	    (goto-char (point-max))
	    (skip-chars-backward " \t")
	    (delete-region (point) (point-max))
	    (goto-char (point-min))
	    (if (looking-at "movemail: ")
		(delete-region (point-min) (match-end 0)))
	    (error (concat "movemail: "
			   (buffer-substring (point-min)
					     (point-max)))))))
    tofile))

(defun nnmail-article-string (article)
  (concat "\nX-Gnus-Newsgroup: " nnmail-current-group ":" 
	  (int-to-string article) " ("))

(defun nnmail-choose-mail (beg end)
  (let (result)
    (save-excursion
      (goto-char end)
      (let ((methods nnmail-split-methods)
	    found)
	(while (and (not found) methods)
	  (if (re-search-backward (car (cdr (car methods))) beg t)
	      (progn
		(setq result (nnmail-insert-newsgroup-line 
			      (car (car methods)) beg end))
		(setq found t))
	    (setq methods (cdr methods))))
	(if (not found)
	    (setq result (nnmail-insert-newsgroup-line 
			  (car (car nnmail-split-methods)) beg end)))))
    result))

(defun nnmail-insert-newsgroup-line (group beg end)
  (let ((active (car (cdr (assoc group nnmail-active-alist))))
	(time (current-time)))
    (if (not active)
	(progn
	  (setq nnmail-active-alist 
		(cons (list group (cons 1 0)) nnmail-active-alist))
	  (setq active (car (cdr (car nnmail-active-alist))))))
    (setcdr active (1+ (cdr active)))
    (insert (format "X-Gnus-Newsgroup: %s:%d (%d %d)\n" group (cdr active)
		    (car time) (car (cdr time))))
    (cons group (cdr active))))

(defun nnmail-split-region (beg end)
  (goto-char beg)
  (let ((delim (concat "^" rmail-unix-mail-delimiter))
	start stop)
    (while (re-search-forward delim nil t)
      (setq start (point))
      (search-forward "\n\n" nil t)
      (save-excursion
	(forward-char -1)
	(if (not (save-excursion (re-search-backward "^Lines:" start t)))
	    (insert 
	     (format "Lines: %d\n" 
		     (count-lines 
		      (point) 
		      (or (re-search-forward rmail-unix-mail-delimiter nil t)
			  (point-max)))))))
      (setq stop (1- (point)))
      (if (not (search-backward "X-Gnus-Newsgroup: " start t))
	  (nnmail-choose-mail start stop)))))

(defun nnmail-read-mbox ()
  (if (and nnmail-mbox-buffer
	   (get-buffer nnmail-mbox-buffer)
	   (buffer-name nnmail-mbox-buffer)
	   (save-excursion
	     (set-buffer nnmail-mbox-buffer)
	     (= (buffer-size) (nth 7 (file-attributes nnmail-mbox-file)))))
      ()
    (save-excursion
      (set-buffer (setq nnmail-mbox-buffer 
			(find-file-noselect nnmail-mbox-file)))
      (buffer-disable-undo (current-buffer))
      (nnmail-split-region (point-min) (point-max)))))

(defun nnmail-split-incoming (incoming)
  (save-excursion
    (set-buffer nnmail-mbox-buffer)
    (goto-char (point-max))
    (let ((start (point)))
      (insert-file-contents incoming)
      (nnmail-split-region start (point-max)))))

(defun nnmail-get-active ()
  (let ((methods nnmail-split-methods))
    (setq nnmail-active-alist nil)
    (if (nnmail-request-list)
	(save-excursion
	  (set-buffer (get-buffer-create " *nntpd*"))
	  (goto-char 1)
	  (while (re-search-forward 
		  "^\\([^ \t]+\\)[ \t]+\\([0-9]+\\)[ \t]+\\([0-9]+\\)" nil t)
	    (setq nnmail-active-alist 
		  (cons (list (buffer-substring (match-beginning 1) 
						(match-end 1))
			      (cons (string-to-int 
				     (buffer-substring (match-beginning 3)
						       (match-end 3)))
				    (string-to-int 
				     (buffer-substring (match-beginning 2)
						       (match-end 2)))))
			nnmail-active-alist)))))
    (while methods
      (if (not (assoc (car (car methods)) nnmail-active-alist))
	  (setq nnmail-active-alist
		(cons (list (car (car methods)) (cons 1 0)) 
		      nnmail-active-alist)))
      (setq methods (cdr methods)))
    t))

(defun nnmail-save-active ()
  (let ((groups nnmail-active-alist)
	group)
    (save-excursion
      (set-buffer (get-buffer-create " *nnmail*"))
      (buffer-disable-undo (current-buffer))
      (erase-buffer)
      (while groups
	(setq group (car groups))
	(insert (format "%s %d %d y\n" (car group) (cdr (car (cdr group)) )
			(car (car (cdr group)))))
	(setq groups (cdr groups)))
      (write-region 1 (point-max) (expand-file-name nnmail-active-file) nil 
		    'nomesg)
      (kill-buffer (current-buffer)))))

(defun nnmail-get-new-mail ()
  (let (incoming)
    (nnmail-get-active)
    (nnmail-read-mbox)
    (if (and (file-exists-p nnmail-spool-file)
	     (> (nth 7 (file-attributes nnmail-spool-file)) 0))
	(progn
	  (setq incoming (nnmail-move-inbox))
	  (nnmail-split-incoming incoming)
	  (run-hooks 'nnmail-read-incoming-hook)))
    (and (buffer-modified-p nnmail-mbox-buffer) 
	 (save-excursion
	   (nnmail-save-active)
	   (set-buffer nnmail-mbox-buffer)
	   (save-buffer)))
;    (if incoming
;	(delete-file incoming))
    ))

(provide 'nnmail)

;;; nnmail.el ends here
