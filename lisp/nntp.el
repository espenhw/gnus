;;; nntp.el --- NNTP (RFC977) Interface for GNU Emacs
;; Copyright (C) 1987,88,89,90,92,93,94,95 Free Software Foundation, Inc.

;; Author: Masanobu UMEDA <umerin@flab.flab.fujitsu.junet>
;; 	Lars Magne Ingebrigtsen <larsi@ifi.uio.no>
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

(require 'rnews)
(require 'nnheader)

(eval-and-compile
  (autoload 'news-setup "rnewspost")
  (autoload 'news-reply-mode "rnewspost")
  (autoload 'nnmail-request-post-buffer "nnmail")
  (autoload 'cancel-timer "timer"))

(defvar nntp-server-hook nil
  "*Hooks for the NNTP server.
If the kanji code of the NNTP server is different from the local kanji
code, the correct kanji code of the buffer associated with the NNTP
server must be specified as follows:

\(setq nntp-server-hook
      (function
       (lambda ()
	 ;; Server's Kanji code is EUC (NEmacs hack).
	 (make-local-variable 'kanji-fileio-code)
	 (setq kanji-fileio-code 0))))

If you'd like to change something depending on the server in this
hook, use the variable `nntp-address'.")

(defvar nntp-server-opened-hook nil
  "*Hook used for sending commands to the server at startup.  
The default value is `nntp-send-mode-reader', whick makes an innd
server spawn an nnrpd server.  Another useful function to put in this
hook might be `nntp-send-authinfo', which will prompt for a password
to allow posting from the server.  Note that this is only necessary to
do on servers that use strict access control.")  (add-hook
'nntp-server-opened-hook 'nntp-send-mode-reader)

(defvar nntp-open-server-function 'nntp-open-network-stream
  "*Function used for connecting to a remote system.
It will be called with the address of the remote system.

Two pre-made functions are `nntp-open-network-stream', which is the
default, and simply connects to some port or other on the remote
system (see nntp-port-number).  The other is `nntp-open-rlogin', which
does an rlogin on the remote system, and then does a telnet to the
NNTP server available there (see nntp-rlogin-parameters).")

(defvar nntp-rlogin-parameters '("telnet" "${NNTPSERVER:=localhost}" "nntp")
  "*Parameters to `nntp-open-login'.
That function may be used as `nntp-open-server-function'.  In that
case, this list will be used as the parameter list given to rsh.")

(defvar nntp-rlogin-user-name nil
  "*User name on remote system when using the rlogin connect method.")

(defvar nntp-address nil
  "*The name of the NNTP server.")

(defvar nntp-port-number "nntp"
  "*Port number to connect to.")

(defvar nntp-large-newsgroup 50
  "*The number of the articles which indicates a large newsgroup.
If the number of the articles is greater than the value, verbose
messages will be shown to indicate the current status.")

(defvar nntp-buggy-select (memq system-type '(fujitsu-uts))
  "*t if your select routine is buggy.
If the select routine signals error or fall into infinite loop while
waiting for the server response, the variable must be set to t.  In
case of Fujitsu UTS, it is set to T since `accept-process-output'
doesn't work properly.")

(defvar nntp-maximum-request 400
  "*The maximum number of the requests sent to the NNTP server at one time.
If Emacs hangs up while retrieving headers, set the variable to a
lower value.")

(defvar nntp-debug-read 10000
  "*Display '...' every 10Kbytes of a message being received if it is non-nil.
If it is a number, dots are displayed per the number.")

(defvar nntp-nov-is-evil nil
  "*If non-nil, nntp will never attempt to use XOVER when talking to the server.")

(defvar nntp-xover-commands '("XOVER" "XOVERVIEW")
  "*List of strings that are used as commands to fetch NOV lines from a server.
The strings are tried in turn until a positive response is gotten. If
none of the commands are successful, nntp will just grab headers one
by one.")

(defvar nntp-connection-timeout nil
  "*Number of seconds to wait before an nntp connection times out.
If this variable is nil, which is the default, no timers are set.")

(defvar nntp-news-default-headers nil
  "*If non-nil, override `mail-default-headers' when posting news.")

(defvar nntp-prepare-server-hook nil
  "*Hook run before a server is opened.
If can be used to set up a server remotely, for instance.  Say you
have an account at the machine \"other.machine\".  This machine has
access to an NNTP server that you can't access locally.  You could
then use this hook to rsh to the remote machine and start a proxy NNTP
server there that you can connect to.")



(defconst nntp-version "nntp 4.0"
  "Version numbers of this version of NNTP.")

(defvar nntp-server-buffer nil
  "Buffer associated with the NNTP server process.")

(defvar nntp-server-process nil
  "The NNTP server process.
You'd better not use this variable in NNTP front-end program, but
instead use `nntp-server-buffer'.")

(defvar nntp-status-string nil
  "Save the server response message.
You'd better not use this variable in NNTP front-end program but
instead call function `nntp-status-message' to get status message.")

(defvar nntp-server-xover t)
(defvar nntp-server-list-active-group 'try)
(defvar nntp-current-group "")


(defvar nntp-current-server nil)
(defvar nntp-server-alist nil)
(defvar nntp-server-variables 
  (list
   (list 'nntp-server-hook nntp-server-hook)
   (list 'nntp-server-opened-hook nntp-server-opened-hook)
   (list 'nntp-port-number nntp-port-number)
   (list 'nntp-address nntp-address)
   (list 'nntp-large-newsgroup nntp-large-newsgroup)
   (list 'nntp-buggy-select nntp-buggy-select)
   (list 'nntp-maximum-request nntp-maximum-request)
   (list 'nntp-debug-read nntp-debug-read)
   (list 'nntp-nov-is-evil nntp-nov-is-evil)
   (list 'nntp-xover-commands nntp-xover-commands)
   (list 'nntp-connection-timeout nntp-connection-timeout)
   (list 'nntp-news-default-headers nntp-news-default-headers)
   (list 'nntp-prepare-server-hook nntp-prepare-server-hook) 
   '(nntp-server-process nil)
   '(nntp-status-string nil)
   '(nntp-server-xover t)
   '(nntp-server-list-active-group 'try)
   '(nntp-current-group "")))


;;; Interface funtions.

(defun nntp-retrieve-headers (sequence &optional newsgroup server)
  "Retrieve the headers to the articles in SEQUENCE."
  (nntp-possibly-change-server newsgroup server)
  (save-excursion
    (set-buffer nntp-server-buffer)
    (erase-buffer)
    (if (and (not gnus-nov-is-evil) 
	     (not nntp-nov-is-evil)
	     (nntp-retrieve-headers-with-xover sequence))
        'nov
      (let ((number (length sequence))
	    (count 0)
	    (received 0)
	    (last-point (point-min)))
	;; Send HEAD command.
	(while sequence
	  (nntp-send-strings-to-server "HEAD" (car sequence))
	  (setq sequence (cdr sequence))
	  (setq count (1+ count))
	  ;; Every 400 header requests we have to read stream in order
	  ;;  to avoid deadlock.
	  (if (or (null sequence)	;All requests have been sent.
		  (zerop (% count nntp-maximum-request)))
	      (progn
		(accept-process-output)
		(while (progn
			 (goto-char last-point)
			 ;; Count replies.
			 (while (re-search-forward "^[0-9]" nil t)
			   (setq received (1+ received)))
			 (setq last-point (point))
			 (< received count))
		  ;; If number of headers is greater than 100, give
		  ;;  informative messages.
		  (and (numberp nntp-large-newsgroup)
		       (> number nntp-large-newsgroup)
		       (zerop (% received 20))
		       (message "NNTP: Receiving headers... %d%%"
				(/ (* received 100) number)))
		  (nntp-accept-response)))))
	;; Wait for text of last command.
	(goto-char (point-max))
	(re-search-backward "^[0-9]" nil t)
	(if (looking-at "^[23]")
	    (while (progn
		     (goto-char (- (point-max) 3))
		     (not (looking-at "^\\.\r$")))
	      (nntp-accept-response)))
	(and (numberp nntp-large-newsgroup)
	     (> number nntp-large-newsgroup)
	     (message "NNTP: Receiving headers... done"))

	;; Now all of replies are received.
	(setq received number)
	;; First, fold continuation lines.
	(goto-char (point-min))
	(while (re-search-forward "\\(\r?\n[ \t]+\\)+" nil t)
	  (replace-match " "))
	;; Remove all "\r"'s
	(goto-char (point-min))
	(while (search-forward "\r" nil t)
	  (replace-match ""))
	'headers))))


(defun nntp-retrieve-groups (groups &optional server)
  (nntp-possibly-change-server nil server)
  (save-excursion
    (set-buffer nntp-server-buffer)
    (and (eq nntp-server-list-active-group 'try)
	 (nntp-try-list-active (car groups)))
    (erase-buffer)
    (let ((count 0)
	  (received 0)
	  (last-point (point-min))
	  (command (if nntp-server-list-active-group
		       "LIST ACTIVE" "GROUP")))
	(while groups
	  (nntp-send-strings-to-server "HEAD" (car groups))
	  (setq groups (cdr groups))
	  (setq count (1+ count))
	  ;; Every 400 requests we have to read the stream in
	  ;; order to avoid deadlocks.
	  (if (or (null groups)       ;All requests have been sent.
		  (zerop (% count nntp-maximum-request)))
	      (progn
		(accept-process-output)
		(while (progn
			 (goto-char last-point)
			 ;; Count replies.
			 (while (re-search-forward "^[0-9]" nil t)
			   (setq received (1+ received)))
			 (setq last-point (point))
			 (< received count))
		  (nntp-accept-response)))))
	;; Wait for the reply from the final command.
	(goto-char (point-max))
	(re-search-backward "^[0-9]" nil t)
	(if (looking-at "^[23]")
	    (while (progn
		     (goto-char (- (point-max) 3))
		     (not (looking-at "^\\.\r$")))
	      (nntp-accept-response)))

	;; Now all replies are received. We remove CRs.
	(goto-char (point-min))
	(while (search-forward "\r" nil t)
	  (replace-match ""))

	(if nntp-server-list-active-group
	    (progn
	      ;; We have read active entries, so we just delete the
	      ;; superfluos gunk.
	      (goto-char (point-min))
	      (while (re-search-forward "^[\\.234]" nil t)
		(delete-region (match-beginning 0) 
			       (progn (forward-line 1) (point))))
	      'active)
	  'group))))

(defun nntp-open-server (server &optional defs)
  (nnheader-init-server-buffer)
  (if (nntp-server-opened server)
      t
    (if (or (stringp (car defs))
	    (numberp (car defs)))
	(setq defs (cons (list 'nntp-server-port (car defs)) (cdr defs))))
    (or (assq 'nntp-address defs)
	(setq defs (append defs (list (list 'nntp-address server)))))
    (if (and nntp-current-server
	     (not (equal server nntp-current-server)))
	(setq nntp-server-alist 
	      (cons (list nntp-current-server
			  (nnheader-save-variables nntp-server-variables))
		    nntp-server-alist)))
    (let ((state (assoc server nntp-server-alist)))
      (if state 
	  (progn
	    (nnheader-restore-variables (nth 1 state))
	    (setq nntp-server-alist (delq state nntp-server-alist)))
	(nnheader-set-init-variables nntp-server-variables defs)))
    (setq nntp-current-server server)
    (run-hooks 'nntp-prepare-server-hook)
    (nntp-open-server-semi-internal nntp-address)))

(defun nntp-close-server (&optional server)
  "Close connection to SERVER."
  (nntp-possibly-change-server nil server)
  (unwind-protect
      (progn
	;; Un-set default sentinel function before closing connection.
	(and nntp-server-process
	     (eq 'nntp-default-sentinel
		 (process-sentinel nntp-server-process))
	     (set-process-sentinel nntp-server-process nil))
	;; We cannot send QUIT command unless the process is running.
	(if (nntp-server-opened)
	    (nntp-send-command nil "QUIT")))
    (nntp-close-server-internal server)))

(fset 'nntp-request-quit (symbol-function 'nntp-close-server))

(defun nntp-request-close ()
  "Close all server connections."
  (let (proc)
    (while nntp-server-alist
      (setq proc (nth 1 (assq 'nntp-server-process (car nntp-server-alist))))
      (and proc (delete-process proc))
      (setq nntp-server-alist (cdr nntp-server-alist)))
    (setq nntp-current-server nil)))

(defun nntp-server-opened (&optional server)
  "Say whether a connection to SERVER has been opened."
  (and (equal server nntp-current-server)
       nntp-server-buffer
       (buffer-name nntp-server-buffer)
       nntp-server-process
       (memq (process-status nntp-server-process) '(open run))))

(defun nntp-status-message (&optional server)
  "Return server status as a string."
  (if (and nntp-status-string
	   ;; NNN MESSAGE
	   (string-match "[0-9][0-9][0-9][ \t]+\\([^\r]*\\).*$"
			 nntp-status-string))
      (substring nntp-status-string (match-beginning 1) (match-end 1))
    ;; Empty message if nothing.
    nntp-status-string))

(defun nntp-request-article (id &optional newsgroup server buffer)
  "Request article ID (message-id or number)."
  (nntp-possibly-change-server newsgroup server)
  (unwind-protect
      (progn
	(if buffer (set-process-buffer nntp-server-process buffer))
	(let ((nntp-server-buffer (or buffer nntp-server-buffer))
	      (id (or (and (numberp id) (int-to-string id)) id)))
	  ;; If NEmacs, end of message may look like: "\256\215" (".^M")
	  (prog1
	      (nntp-send-command "^\\.\r$" "ARTICLE" id)
	    (nntp-decode-text))))
    (if buffer (set-process-buffer nntp-server-process nntp-server-buffer))))

(defun nntp-request-body (id &optional newsgroup server)
  "Request body of article ID (message-id or number)."
  (nntp-possibly-change-server newsgroup server)
  (prog1
      ;; If NEmacs, end of message may look like: "\256\215" (".^M")
      (nntp-send-command
       "^\\.\r$" "BODY" (or (and (numberp id) (int-to-string id)) id))
    (nntp-decode-text)))

(defun nntp-request-head (id &optional newsgroup server)
  "Request head of article ID (message-id or number)."
  (nntp-possibly-change-server newsgroup server)
  (prog1
      (nntp-send-command 
       "^\\.\r$" "HEAD" (or (and (numberp id) (int-to-string id)) id))
    (nntp-decode-text)))

(defun nntp-request-stat (id &optional newsgroup server)
  "Request STAT of article ID (message-id or number)."
  (nntp-possibly-change-server newsgroup server)
  (nntp-send-command 
   "^[23].*\r$" "STAT" (or (and (numberp id) (int-to-string id)) id)))

(defun nntp-request-group (group &optional server dont-check)
  "Select GROUP."
  (if dont-check
      (nntp-send-command "^.*\r$" "GROUP" group)
    (cond ((eq nntp-server-list-active-group 'try)
	   (or (nntp-try-list-active group)
	       (nntp-send-command "^.*\r$" "GROUP" group)))
	  (nntp-server-list-active-group
	   (nntp-list-active-group group))
	  (t
	   (nntp-send-command "^.*\r$" "GROUP" group)))
    (if nntp-server-list-active-group
	(save-excursion
	  (set-buffer nntp-server-buffer)
	  (goto-char (point-min))
	  (forward-line 1)
	  (if (looking-at "[^ ]+[ \t]+\\([0-9]\\)[ \t]+\\([0-9]\\)")
	      (let ((end (progn (goto-char (match-beginning 1))
				(read (current-buffer))))
		    (beg (read (current-buffer))))
		(and (> beg end)
		     (setq end 0
			   beg 0))
		(erase-buffer)
		(insert (format "211 %s %d %d %d\n"
				group (max (- (1+ end) beg) 0)
				beg end))))))
    (save-excursion
      (set-buffer nntp-server-buffer)
      (goto-char (point-min))
      (looking-at "[23]"))))

(defun nntp-list-active-group (group &optional server)
  (nntp-send-command "^.*\r$" "LIST ACTIVE" group))

(defun nntp-request-group-description (group &optional server)
  "Get description of GROUP."
  (if (nntp-possibly-change-server nil server)
      (prog1
	  (nntp-send-command "^.*\r$" "XGTITLE" group)
	(nntp-decode-text))))

(defun nntp-close-group (group &optional server)
  t)

(defun nntp-request-list (&optional server)
  "List active groups."
  (nntp-possibly-change-server nil server)
  (prog1
      (nntp-send-command "^\\.\r$" "LIST")
    (nntp-decode-text)))

(defun nntp-request-list-newsgroups (&optional server)
  "List groups."
  (nntp-possibly-change-server nil server)
  (prog1
      (nntp-send-command "^\\.\r$" "LIST NEWSGROUPS")
    (nntp-decode-text)))

(defun nntp-request-newgroups (date &optional server)
  "List new groups."
  (nntp-possibly-change-server nil server)
  (let* ((date (timezone-parse-date date))
	 (time-string
	  (format "%s%02d%02d %s%s%s"
		  (substring (aref date 0) 2) (string-to-int (aref date 1)) 
		  (string-to-int (aref date 2)) (substring (aref date 3) 0 2)
		  (substring 
		   (aref date 3) 3 5) (substring (aref date 3) 6 8))))
    (prog1
	(nntp-send-command "^\\.\r$" "NEWGROUPS" time-string)
      (nntp-decode-text))))

(defun nntp-request-list-distributions (&optional server)
  "List distributions."
  (nntp-possibly-change-server nil server)
  (prog1
      (nntp-send-command "^\\.\r$" "LIST DISTRIBUTIONS")
    (nntp-decode-text)))

(defun nntp-request-last (&optional newsgroup server)
  "Decrease the current article pointer."
  (nntp-possibly-change-server newsgroup server)
  (nntp-send-command "^[23].*\r$" "LAST"))

(defun nntp-request-next (&optional newsgroup server)
  "Advance the current article pointer."
  (nntp-possibly-change-server newsgroup server)
  (nntp-send-command "^[23].*\r$" "NEXT"))

(defun nntp-request-post (&optional server)
  "Post the current buffer."
  (nntp-possibly-change-server nil server)
  (if (nntp-send-command "^[23].*\r$" "POST")
      (progn
	(nntp-encode-text)
	(nntp-send-region-to-server (point-min) (point-max))
	;; 1.2a NNTP's post command is buggy. "^M" (\r) is not
	;;  appended to end of the status message.
	(nntp-wait-for-response "^[23].*$"))))

(defun nntp-request-post-buffer 
  (post group subject header article-buffer info follow-to respect-poster)
  "Request a buffer suitable for composing an article.
If POST, this is an original article; otherwise it's a followup.
GROUP is the group to be posted to, the article should have subject
SUBJECT.  HEADER is a Gnus header vector.  ARTICLE-BUFFER contains the
article being followed up.  INFO is a Gnus info list.  If FOLLOW-TO,
post to this group instead.  If RESPECT-POSTER, heed the special
\"poster\" value of the Followup-to header."
  (if (assq 'to-address (nth 4 info))
      (nnmail-request-post-buffer 
       post group subject header article-buffer info follow-to respect-poster)
    (let ((mail-default-headers 
	   (or nntp-news-default-headers mail-default-headers))
	  from date to followup-to newsgroups message-of
	  references distribution message-id)
      (save-excursion
	(set-buffer (get-buffer-create "*post-news*"))
	(news-reply-mode)
	(if (and (buffer-modified-p)
		 (> (buffer-size) 0)
		 (not (y-or-n-p "Unsent article being composed; erase it? ")))
	    ()
	  (erase-buffer)
	  (if post
	      (news-setup nil subject nil group nil)
	    (save-excursion
	      (set-buffer article-buffer)
	      (goto-char (point-min))
	      (narrow-to-region (point-min)
				(progn (search-forward "\n\n") (point)))
	      (setq from (header-from header))
	      (setq date (header-date header))
	      (and from
		   (let ((stop-pos 
			  (string-match "  *at \\|  *@ \\| *(\\| *<" from)))
		     (setq 
		      message-of
		      (concat (if stop-pos (substring from 0 stop-pos) from) 
			      "'s message of " date))))
	      (setq subject (or subject (header-subject header)))
	      (or (string-match "^[Rr][Ee]:" subject)
		  (setq subject (concat "Re: " subject)))
	      (setq followup-to (mail-fetch-field "followup-to"))
	      (if (or (null respect-poster) ;Ignore followup-to: field.
		      (string-equal "" followup-to) ;Bogus header.
		      (string-equal "poster" followup-to)) ;Poster
		  (setq followup-to nil))
	      (setq newsgroups
		    (or follow-to followup-to (mail-fetch-field "newsgroups")))
	      (setq references (header-references header))
	      (setq distribution (mail-fetch-field "distribution"))
	      ;; Remove bogus distribution.
	      (and (stringp distribution)
		   (string-match "world" distribution)
		   (setq distribution nil))
	      (setq message-id (header-id header))
	      (widen))
	    (setq news-reply-yank-from from)
	    (setq news-reply-yank-message-id message-id)
	    (news-setup to subject message-of 
			(if (stringp newsgroups) newsgroups "") 
			article-buffer)
	    (if (and newsgroups (listp newsgroups))
		(progn
		  (goto-char (point-min))
		  (while newsgroups
		    (insert (car (car newsgroups)) ": " 
			    (cdr (car newsgroups)) "\n")
		    (setq newsgroups (cdr newsgroups)))))
	    ;; Fold long references line to follow RFC1036.
	    (mail-position-on-field "References")
	    (let ((begin (- (point) (length "References: ")))
		  (fill-column 79)
		  (fill-prefix "\t"))
	      (if references (insert references))
	      (if (and references message-id) (insert " "))
	      (if message-id (insert message-id))
	      ;; The region must end with a newline to fill the region
	      ;; without inserting extra newline.
	      (fill-region-as-paragraph begin (1+ (point))))
	    (if distribution
		(progn
		  (mail-position-on-field "Distribution")
		  (insert distribution)))))
	(current-buffer)))))

;;; Internal functions.

(defun nntp-send-mode-reader ()
  "Send the MODE READER command to the nntp server.
This function is supposed to be called from `nntp-server-opened-hook'.
It will make innd servers spawn an nnrpd process to allow actual article
reading."
  (nntp-send-command "^.*\r$" "MODE READER"))

(defun nntp-send-authinfo ()
  "Send the AUTHINFO to the nntp server.
This function is supposed to be called from `nntp-server-opened-hook'.
It will prompt for a password."
  (nntp-send-command "^.*\r$" "AUTHINFO USER" (user-login-name))
  (nntp-send-command "^.*\r$" "AUTHINFO PASS" (read-string "NNTP password: ")))

(defun nntp-default-sentinel (proc status)
  "Default sentinel function for NNTP server process."
  (let ((servers nntp-server-alist))
    ;; Go through the alist of server names and find the name of the
    ;; server that the process that sent the signal is connected to.
    ;; If you get my drift.
    (while (and servers 
		(not (equal proc (nth 1 (assq 'nntp-server-process
					      (car servers))))))
      (setq servers (cdr servers)))
    (message "nntp: Connection closed to server %s." 
	     (or (car (car servers)) "(none)"))
    (ding)))

(defun nntp-kill-connection (server)
  (let ((proc (nth 1 (assq 'nntp-server-process 
			   (assoc server nntp-server-alist)))))
    (and proc (delete-process (process-name proc)))
    (nntp-close-server server)
    (setq nntp-status-string 
	  (message "Connection timed out to server %s." server))
    (ding)))

;; Encoding and decoding of NNTP text.

(defun nntp-decode-text ()
  "Decode text transmitted by NNTP.
0. Delete status line.
1. Delete `^M' at end of line.
2. Delete `.' at end of buffer (end of text mark).
3. Delete `.' at beginning of line."
  (save-excursion
    (set-buffer nntp-server-buffer)
    ;; Insert newline at end of buffer.
    (goto-char (point-max))
    (or (bolp) (insert "\n"))
    ;; Delete status line.
    (goto-char (point-min))
    (delete-region (point) (progn (forward-line 1) (point)))
    ;; Delete `^M' at the end of lines.
    (while (not (eobp))
      (end-of-line)
      (and (= (preceding-char) ?\r)
	   (delete-char -1))
      (forward-line 1))
    ;; Delete `.' at end of the buffer (end of text mark).
    (goto-char (point-max))
    (forward-line -1)
    (if (looking-at "^\\.$")
	(delete-region (point) (progn (forward-line 1) (point))))
    ;; Replace `..' at beginning of line with `.'.
    (goto-char (point-min))
    ;; (replace-regexp "^\\.\\." ".")
    (while (search-forward "\n.." nil t)
      (delete-char -1))))

(defun nntp-encode-text ()
  "Encode text in current buffer for NNTP transmission.
1. Insert `.' at beginning of line.
2. Insert `.' at end of buffer (end of text mark)."
  (save-excursion
    ;; Insert newline at end of buffer.
    (goto-char (point-max))
    (or (bolp) (insert "\n"))
    ;; Replace `.' at beginning of line with `..'.
    (goto-char (point-min))
    ;; (replace-regexp "^\\." "..")
    (while (search-forward "\n." nil t)
      (insert "."))
    ;; Insert `.' at end of buffer (end of text mark).
    (goto-char (point-max))
    (insert ".\r\n")))


;;;
;;; Synchronous Communication with NNTP Server.
;;;

(defun nntp-send-command (response cmd &rest args)
  "Wait for server RESPONSE after sending CMD and optional ARGS to server."
  (save-excursion
    ;; Clear communication buffer.
    (set-buffer nntp-server-buffer)
    (erase-buffer)
    (apply 'nntp-send-strings-to-server cmd args)
    (if response
	(nntp-wait-for-response response)
      t)))

(defun nntp-wait-for-response (regexp)
  "Wait for server response which matches REGEXP."
  (save-excursion
    (let ((status t)
	  (wait t)
	  (dotnum 0)			;Number of "." being displayed.
	  (dotsize			;How often "." displayed.
	   (if (numberp nntp-debug-read) nntp-debug-read 10000)))
      (set-buffer nntp-server-buffer)
      ;; Wait for status response (RFC977).
      ;; 1xx - Informative message.
      ;; 2xx - Command ok.
      ;; 3xx - Command ok so far, send the rest of it.
      ;; 4xx - Command was correct, but couldn't be performed for some
      ;;       reason.
      ;; 5xx - Command unimplemented, or incorrect, or a serious
      ;;       program error occurred.
      (nntp-accept-response)
      (while wait
	(goto-char (point-min))
	(cond ((looking-at "[23]")
	       (setq wait nil))
	      ((looking-at "[45]")
	       (setq status nil)
	       (setq wait nil))
	      (t (nntp-accept-response))))
      ;; Save status message.
      (end-of-line)
      (setq nntp-status-string
	    (buffer-substring (point-min) (point)))
      (if status
	  (progn
	    (setq wait t)
	    (while wait
	      (goto-char (point-max))
	      (forward-line -1)		;(beginning-of-line)
	      ;;(message (buffer-substring
	      ;;	 (point)
	      ;;	 (save-excursion (end-of-line) (point))))
	      (if (looking-at regexp)
		  (setq wait nil)
		(if nntp-debug-read
		    (let ((newnum (/ (buffer-size) dotsize)))
		      (if (not (= dotnum newnum))
			  (progn
			    (setq dotnum newnum)
			    (message "NNTP: Reading %s"
				     (make-string dotnum ?.))))))
		(nntp-accept-response)))
	    ;; Remove "...".
	    (if (and nntp-debug-read (> dotnum 0))
		(message ""))
	    ;; Successfully received server response.
	    t)))))


;;;
;;; Low-Level Interface to NNTP Server.
;;; 

(defun nntp-retrieve-headers-with-xover (sequence)
  (if (not nntp-server-xover)
      ()
    (let ((range (format "%d-%d" (car sequence)
			 (nntp-last-element sequence))))
      (prog1
	  (if (stringp nntp-server-xover)
	      (nntp-send-command "^\\.\r$" nntp-server-xover range)
	    (let ((commands nntp-xover-commands))
	      (while (and commands
			  (eq t nntp-server-xover))
		(nntp-send-command "^\\.\r$" (car commands) range)
		(save-excursion
		  (set-buffer nntp-server-buffer)
		  (goto-char 1)
		  (if (looking-at "[23]") 
		      (setq nntp-server-xover (car commands))))
		(setq commands (cdr commands)))
	      (if (eq t nntp-server-xover)
		  (setq nntp-server-xover nil))
	      nntp-server-xover))
	(if nntp-server-xover (nntp-decode-text) (erase-buffer))))))

(defun nntp-send-strings-to-server (&rest strings)
  "Send list of STRINGS to news server as command and its arguments."
  (let ((cmd (car strings))
	(strings (cdr strings)))
    ;; Command and each argument must be separated by one or more spaces.
    (while strings
      (setq cmd (concat cmd " " (car strings)))
      (setq strings (cdr strings)))
    ;; Command line must be terminated by a CR-LF.
    (if (not (nntp-server-opened nntp-current-server))
	(progn
	  (nntp-close-server nntp-address)
	  (if (not (nntp-open-server nntp-address))
	      (error (nntp-status-message)))
	  (save-excursion
	    (set-buffer nntp-server-buffer)
	    (erase-buffer))))
    (process-send-string nntp-server-process (concat cmd "\r\n"))))

(defun nntp-send-region-to-server (begin end)
  "Send current buffer region (from BEGIN to END) to news server."
  (save-excursion
    ;; We have to work in the buffer associated with NNTP server
    ;;  process because of NEmacs hack.
    (copy-to-buffer nntp-server-buffer begin end)
    (set-buffer nntp-server-buffer)
    (setq begin (point-min))
    (setq end (point-max))
    ;; `process-send-region' does not work if text to be sent is very
    ;;  large. I don't know maximum size of text sent correctly.
    (let ((last nil)
	  (size 100))			;Size of text sent at once.
      (save-restriction
	(narrow-to-region begin end)
	(goto-char begin)
	(while (not (eobp))
	  ;;(setq last (min end (+ (point) size)))
	  ;; NEmacs gets confused if character at `last' is Kanji.
	  (setq last (save-excursion
		       (goto-char (min end (+ (point) size)))
		       (or (eobp) (forward-char 1)) ;Adjust point
		       (point)))
	  (process-send-region nntp-server-process (point) last)
	  ;; I don't know whether the next codes solve the known
	  ;;  problem of communication error of GNU Emacs.
	  (accept-process-output)
	  ;;(sit-for 0)
	  (goto-char last))))
    ;; We cannot erase buffer, because reply may be received.
    (delete-region begin end)))

(defun nntp-open-server-semi-internal (server &optional service)
  "Open SERVER.
If SERVER is nil, use value of environment variable `NNTPSERVER'.
If SERVICE, this this as the port number."
  (let ((server (or server (getenv "NNTPSERVER")))
	(status nil)
	(timer 
	 (and nntp-connection-timeout 
	      (run-at-time nntp-connection-timeout
			   nil 'nntp-kill-connection server))))
    (setq nntp-status-string "")
    (message "nntp: Connecting to server on %s..." server)
    (cond ((and server (nntp-open-server-internal server service))
	   (setq nntp-address server)
	   (setq status
		 (condition-case nil
		     (nntp-wait-for-response "^[23].*\r$")
		   (error nil)
		   (quit nil)))
	   (or status (nntp-close-server-internal server))
	   (and nntp-server-process
		(progn
		  (set-process-sentinel 
		   nntp-server-process 'nntp-default-sentinel)
		  ;; You can send commands at startup like AUTHINFO here.
		  ;; Added by Hallvard B Furuseth <h.b.furuseth@usit.uio.no>
		  (run-hooks 'nntp-server-opened-hook))))
	  ((null server)
	   (setq nntp-status-string "NNTP server is not specified.")))
    (and timer (cancel-timer timer))
    (message "")
    (or status
	(setq nntp-current-server nil))
    status))

(defun nntp-open-server-internal (server &optional service)
  "Open connection to news server on SERVER by SERVICE (default is nntp)."
  (let (proc)
    (save-excursion
      ;; Use TCP/IP stream emulation package if needed.
      (or (fboundp 'open-network-stream)
	  (require 'tcp))
      ;; Initialize communication buffer.
      (nnheader-init-server-buffer)
      (set-buffer nntp-server-buffer)
      (if (setq proc
		(condition-case nil
		    (funcall nntp-open-server-function server)
		  (error nil)))
	  (progn
	    (setq nntp-server-process proc)
	    ;; Suggested by Hallvard B Furuseth <h.b.furuseth@usit.uio.no>.
	    (process-kill-without-query proc)
	    (setq nntp-server-xover t)
	    (setq nntp-server-list-active-group t)
	    (setq nntp-address server)
	    ;; It is possible to change kanji-fileio-code in this hook.
	    (run-hooks 'nntp-server-hook)
	    nntp-server-process)))))

(defun nntp-open-network-stream (server)
  (open-network-stream "nntpd" nntp-server-buffer server nntp-port-number))

(defun nntp-open-rlogin-stream (server)
  (apply 'start-process "nntpd" nntp-server-buffer "rsh" 
	 "-l" (or nntp-rlogin-user-name (user-login-name))
	 server nntp-rlogin-parameters))

(defun nntp-close-server-internal (&optional server)
  "Close connection to news server."
  (nntp-possibly-change-server nil server)
  (if nntp-server-process
      (delete-process nntp-server-process))
  (setq nntp-server-process nil)
  (setq nntp-address ""))

(defun nntp-accept-response ()
  "Read response of server.
It is well-known that the communication speed will be much improved by
defining this function as macro."
  ;; To deal with server process exiting before
  ;;  accept-process-output is called.
  ;; Suggested by Jason Venner <jason@violet.berkeley.edu>.
  ;; This is a copy of `nntp-default-sentinel'.
  (if (or (not nntp-server-process)
	  (not (memq (process-status nntp-server-process) '(open run))))
      (error "nntp: Process connection closed; %s" (nntp-status-message))
    (if nntp-buggy-select
	(progn
	  ;; We cannot use `accept-process-output'.
	  ;; Fujitsu UTS requires messages during sleep-for. I don't know why.
	  (message "NNTP: Reading...")
	  (sleep-for 1)
	  (message ""))
      (condition-case errorcode
	  (accept-process-output nntp-server-process)
	(error
	 (cond ((string-equal "select error: Invalid argument" 
			      (nth 1 errorcode))
		;; Ignore select error.
		nil)
	       (t
		(signal (car errorcode) (cdr errorcode)))))))))

(defun nntp-last-element (list)
  "Return last element of LIST."
  (while (cdr list)
    (setq list (cdr list)))
  (car list))

(defun nntp-possibly-change-server (newsgroup server)
  (let ((result t))
    ;; We see whether it is necessary to change newsgroup.
    (and newsgroup 
	 (or (not (string= newsgroup nntp-current-group)))
	 (progn
	   (setq result (nntp-request-group newsgroup server))
	   (setq nntp-current-group newsgroup)))
    result))

(defun nntp-try-list-active (group)
  (nntp-list-active-group group)
  (save-excursion
    (set-buffer nntp-server-buffer)
    (goto-char (point-min))
    (cond ((looking-at "5[0-9]+")
	   (setq nntp-server-list-active-group nil))
	  (t
	   (setq nntp-server-list-active-group t)))))

(provide 'nntp)

;;; nntp.el ends here
