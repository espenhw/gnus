;;; nntp.el --- nntp access for Gnus
;; Copyright (C) 1987,88,89,90,92,93,94,95,96 Free Software Foundation, Inc.

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
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;;; Code:

(require 'rnews)
(require 'sendmail)
(require 'nnheader)

(eval-and-compile
  (unless (fboundp 'open-network-stream)
    (require 'tcp)))

(eval-when-compile (require 'cl))

(defvar nntp-address nil
  "Address of the physical nntp server.")

(defvar nntp-port-number "nntp"
  "Port number on the physical nntp server.")

(defvar nntp-server-hook nil
  "*Hooks for the NNTP server.
If the kanji code of the NNTP server is different from the local kanji
code, the correct kanji code of the buffer associated with the NNTP
server must be specified as follows:

\(setq nntp-server-hook
       (lambda ()
	 ;; Server's Kanji code is EUC (NEmacs hack).
	 (make-local-variable 'kanji-fileio-code)
	 (setq kanji-fileio-code 0)))

If you'd like to change something depending on the server in this
hook, use the variable `nntp-address'.")

(defvar nntp-server-opened-hook nil
  "*Hook used for sending commands to the server at startup.  
The default value is `nntp-send-mode-reader', which makes an innd
server spawn an nnrpd server.  Another useful function to put in this
hook might be `nntp-send-authinfo', which will prompt for a password
to allow posting from the server.  Note that this is only necessary to
do on servers that use strict access control.")  
(add-hook 'nntp-server-opened-hook 'nntp-send-mode-reader)

(defvar nntp-server-action-alist 
  '(("nntpd 1\\.5\\.11t" 
     (remove-hook 'nntp-server-opened-hook 'nntp-send-mode-reader)))
  "Alist of regexps to match on server types and actions to be taken.
For instance, if you want Gnus to beep every time you connect
to innd, you could say something like:

\(setq nntp-server-action-alist
       '((\"innd\" (ding))))

You probably don't want to do that, though.")

(defvar nntp-open-connection-function 'nntp-open-network-stream
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

(defvar nntp-large-newsgroup 50
  "*The number of the articles which indicates a large newsgroup.
If the number of the articles is greater than the value, verbose
messages will be shown to indicate the current status.")

(defvar nntp-maximum-request 400
  "*The maximum number of the requests sent to the NNTP server at one time.
If Emacs hangs up while retrieving headers, set the variable to a
lower value.")

(defvar nntp-nov-is-evil nil
  "*If non-nil, nntp will never attempt to use XOVER when talking to the server.")

(defvar nntp-xover-commands '("XOVER" "XOVERVIEW")
  "*List of strings that are used as commands to fetch NOV lines from a server.
The strings are tried in turn until a positive response is gotten. If
none of the commands are successful, nntp will just grab headers one
by one.")

(defvar nntp-nov-gap 20
  "*Maximum allowed gap between two articles.
If the gap between two consecutive articles is bigger than this
variable, split the XOVER request into two requests.")

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

(defvar nntp-warn-about-losing-connection t
  "*If non-nil, beep when a server closes connection.")



;;; Internal variables.

(defvar nntp-connection-alist nil)
(defvar nntp-status-string "")
(defconst nntp-version "nntp 5.0")
(defvar nntp-inhibit-erase nil)

(defvar nntp-server-xover 'try)
(defvar nntp-server-list-active-group 'try)

;; Virtual server defs.
(defvar nntp-current-server nil)
(defvar nntp-server-alist nil)
(defvar nntp-server-variables 
  `((nntp-address ,nntp-address)
    (nntp-open-connection-function ,nntp-open-connection-function)
    (nntp-port-number ,nntp-port-number)
    (nntp-status-string ,nntp-status-string)
    (nntp-connection-alist nil)))



;;; Interface functions.

(defun nntp-retrieve-headers (articles &optional group server fetch-old)
  "Retrieve the headers of ARTICLES."
  (nntp-possibly-change-group group server)
  (save-excursion
    (set-buffer nntp-server-buffer)
    (erase-buffer)
    (if (and (not gnus-nov-is-evil) 
	     (not nntp-nov-is-evil)
	     (nntp-retrieve-headers-with-xover articles fetch-old))
	;; We successfully retrieved the headers via XOVER.
        'nov
      ;; XOVER didn't work, so we do it the hard, slow and inefficient
      ;; way.  
      (let ((number (length articles))
	    (count 0)
	    (received 0)
	    (last-point (point-min)))
	;; Send HEAD command.
	(while articles
	  (nntp-send-command 
	   nil
	   "HEAD" (if (numberp (car articles)) 
		      (int-to-string (car articles))
		    ;; `articles' is either a list of article numbers
		    ;; or a list of article IDs.
		    (car articles)))
	  (setq articles (cdr articles)
		count (1+ count))
	  ;; Every 400 header requests we have to read the stream in
	  ;; order to avoid deadlocks.
	  (when (or (null articles)	;All requests have been sent.
		    (zerop (% count nntp-maximum-request)))
	    (nntp-accept-response)
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
	      (nntp-accept-response))))
	;; Wait for text of last command.
	(goto-char (point-max))
	(re-search-backward "^[0-9]" nil t)
	(when (looking-at "^[23]")
	  (while (progn
		   (goto-char (- (point-max) 3))
		   (not (looking-at "^\\.\r?\n")))
	    (nntp-accept-response)))
	(and (numberp nntp-large-newsgroup)
	     (> number nntp-large-newsgroup)
	     (message "NNTP: Receiving headers...done"))

	;; Now all of replies are received.  Fold continuation lines.
	(nnheader-fold-continuation-lines)
	;; Remove all "\r"'s.
	(goto-char (point-min))
	(while (search-forward "\r" nil t)
	  (replace-match "" t t))
	'headers))))

(defun nntp-request-article (article &optional group server buffer)
  (let ((nntp-server-buffer (or buffer nntp-server-buffer)))
    (nntp-possibly-change-group group server)
    (nntp-send-command-and-decode
     "\r\n\\.\r\n" "ARTICLE"
     (if (numberp article) (int-to-string article) article))))

(defun nntp-request-body (article &optional group server)
  (nntp-possibly-change-group group server)
  (nntp-send-command
   "\r\n\\.\r\n" "BODY"
   (if (numberp article) (int-to-string article) article)))

(defun nntp-request-group (group &optional server dont-check)
  (nntp-possibly-change-group nil server)
  (when (nntp-send-command "^2.*\r\n" "GROUP" group)
    (let ((entry (nntp-find-connection-entry nntp-server-buffer)))
      (setcar (cddr entry) group))))

(defun nntp-close-group (group &optional server)
  t)

(defun nntp-server-opened (server)
  (and (equal server nntp-current-server)
       nntp-server-buffer
       (buffer-name nntp-server-buffer)))

(defun nntp-open-server (server &optional defs connectionless)
  (nnheader-init-server-buffer)
  (if (nntp-server-opened server)
      t
    (when (or (stringp (car defs))
	      (numberp (car defs)))
      (setq defs (cons (list 'nntp-port-number (car defs)) (cdr defs))))
    (unless (assq 'nntp-address defs)
      (setq defs (append defs (list (list 'nntp-address server)))))
    (nnheader-change-server 'nntp server defs)
    (or (nntp-find-connection nntp-server-buffer)
	(nntp-open-connection nntp-server-buffer))))

(defun nntp-close-server (&optional server)
  (nntp-possibly-change-group nil server t)
  (let (process)
    (while (setq process (car (pop nntp-connection-alist)))
      (when (memq (process-status process) '(open run))
	(set-process-sentinel process nil)
	(set-process-filter process nil)
	(nntp-send-string process "QUIT"))
      (when (buffer-name (process-buffer process))
	(kill-buffer (process-buffer process))))))

(defun nntp-request-list (&optional server)
  (nntp-possibly-change-group nil server)
  (nntp-send-command "\r\n\\.\r\n" "LIST"))

(defun nntp-request-list-newsgroups (&optional server)
  (nntp-possibly-change-group nil server)
  (nntp-send-command "\r\n\\.\r\n" "LIST NEWSGROUPS"))

(defun nntp-asynchronous-p ()
  t)
  

;;; Hooky functions.

(defun nntp-send-mode-reader ()
  "Send the MODE READER command to the nntp server.
This function is supposed to be called from `nntp-server-opened-hook'.
It will make innd servers spawn an nnrpd process to allow actual article
reading."
  (nntp-send-command "^.*\r\n" "MODE READER"))

(defun nntp-send-nosy-authinfo ()
  "Send the AUTHINFO to the nntp server.
This function is supposed to be called from `nntp-server-opened-hook'.
It will prompt for a password."
  (nntp-send-command "^.*\r\n" "AUTHINFO USER"
		     (read-string "NNTP user name: "))
  (nntp-send-command "^.*\r\n" "AUTHINFO PASS" 
		     (read-string "NNTP password: ")))

(defun nntp-send-authinfo ()
  "Send the AUTHINFO to the nntp server.
This function is supposed to be called from `nntp-server-opened-hook'.
It will prompt for a password."
  (nntp-send-command "^.*\r\n" "AUTHINFO USER" (user-login-name))
  (nntp-send-command "^.*\r\n" "AUTHINFO PASS" 
		     (read-string "NNTP password: ")))

(defun nntp-send-authinfo-from-file ()
  "Send the AUTHINFO to the nntp server.
This function is supposed to be called from `nntp-server-opened-hook'.
It will prompt for a password."
  (when (file-exists-p "~/.nntp-authinfo")
    (save-excursion
      (set-buffer (get-buffer-create " *authinfo*"))
      (buffer-disable-undo (current-buffer))
      (erase-buffer)
      (insert-file-contents "~/.nntp-authinfo")
      (goto-char (point-min))
      (nntp-send-command "^.*\r\n" "AUTHINFO USER" (user-login-name))
      (nntp-send-command 
       "^.*\r\n" "AUTHINFO PASS" 
       (buffer-substring (point) (progn (end-of-line) (point))))
      (kill-buffer (current-buffer)))))

;;; Internal functions.

(defun nntp-send-command (wait-for &rest strings)
  "Send STRINGS to server and wait until WAIT-FOR returns."
  (nntp-retrieve-data
   (mapconcat 'identity strings " ") 
   nntp-address nntp-port-number nntp-server-buffer
   wait-for nnheader-callback-function))

(defun nntp-send-command-and-decode (wait-for &rest strings)
  "Send STRINGS to server and wait until WAIT-FOR returns."
  (nntp-retrieve-data
   (mapconcat 'identity strings " ") 
   nntp-address nntp-port-number nntp-server-buffer
   wait-for nnheader-callback-function t))

(defun nntp-find-connection (buffer)
  "Find the connection delivering to BUFFER."
  (let ((alist nntp-connection-alist)
	process entry)
    (while (setq entry (pop alist))
      (when (eq buffer (cadr entry))
	(setq process (car entry)
	      alist nil)))
    (when process
      (if (memq (process-status process) '(open run))
	  process
	(when (buffer-name (process-buffer process))
	  (kill-buffer (process-buffer process)))
	(setq nntp-connection-alist (delq entry nntp-connection-alist))
	nil))))

(defun nntp-find-connection-entry (buffer)
  "Return the entry for the connection to BUFFER."
  (assq (nntp-find-connection buffer) nntp-connection-alist))

(defun nntp-open-connection (buffer)
  "Open a connection to PORT on ADDRESS delivering output to BUFFER."
  (let* ((pbuffer (save-excursion
		    (set-buffer 
		     (generate-new-buffer
		      (format " *nntpd %s %s %s*"
			      nntp-address nntp-port-number
			      (buffer-name (get-buffer buffer)))))
		    (buffer-disable-undo (current-buffer))
		    (current-buffer)))
	 (process (funcall nntp-open-connection-function pbuffer)))
    (when process
      (process-kill-without-query process)
      (nntp-wait-for process "^.*\r\n" buffer)
      (if (memq (process-status process) '(open run))
	  (caar (push (list process buffer nil) 
		      nntp-connection-alist))
	(when (buffer-name (process-buffer process))
	  (kill-buffer (process-buffer process)))
	nil))))

(defun nntp-open-network-stream (buffer)
  (open-network-stream "nntpd" buffer nntp-address nntp-port-number))

(defvar nntp-tmp-first)
(defvar nntp-tmp-wait-for)
(defvar nntp-tmp-callback)
(defvar nntp-tmp-buffer)

(defun nntp-make-process-filter (wait-for callback buffer decode)
  `(lambda (proc string)
     (let ((nntp-tmp-wait-for ,wait-for)
	   (nntp-tmp-callback ,callback)
	   (nntp-tmp-buffer ,buffer))
       (nntp-process-filter proc string))))

(defun nntp-process-filter (proc string)
  (let ((old-buffer (current-buffer)))
    (unwind-protect
	(let (point)
	  (set-buffer (process-buffer proc))
	  ;; Insert the text, moving the process-marker.
	  (setq point (goto-char (process-mark proc)))
	  (insert string)
	  (set-marker (process-mark proc) (point))
	  (if (and (= point (point-min))
		   (string-match "^45" string))
	      (progn
		(nntp-snarf-error-message)
		(funcall nntp-tmp-callback nil)
		(set-process-filter proc nil))
	    (setq nntp-tmp-first nil)
	    (if (re-search-backward nntp-tmp-wait-for nil t)
		(progn
		  (if (buffer-name (get-buffer nntp-tmp-buffer))
		      (save-excursion
			(set-buffer (get-buffer nntp-tmp-buffer))
			(insert-buffer-substring (process-buffer proc))))
		  (funcall nntp-tmp-callback t)
		  (set-process-filter proc nil)
		  (erase-buffer)))))
      (set-buffer old-buffer))))

(defun nntp-retrieve-data (command address port buffer
				   &optional wait-for callback decode)
  "Use COMMAND to retrieve data into BUFFER from PORT on ADDRESS."
  (let ((process (or (nntp-find-connection buffer)
		     (nntp-open-connection buffer))))
    (if (not process)
	(nnheader-report 'nntp "Couldn't open connection to %a" address)
      (unless nntp-inhibit-erase
	(save-excursion
	  (set-buffer (process-buffer process))
	  (erase-buffer)))
      (nntp-send-string process command)
      (cond 
       ((eq callback 'ignore)
	t)
       ((and callback wait-for)
	(set-process-filter
	 process (nntp-make-process-filter wait-for callback buffer decode))
	t)
       (wait-for 
	(nntp-wait-for process wait-for buffer decode))
       (t t)))))

(defun nntp-send-string (process string)
  "Send STRING to PROCESS."
  (process-send-string process (concat string "\r\n")))

(defun nntp-wait-for (process wait-for buffer &optional decode)
  "Wait for WAIT-FOR to arrive from PROCESS."
  (save-excursion
    (set-buffer (process-buffer process))
    (goto-char (point-min))
    (while (not (looking-at "[2345]"))
      (nntp-accept-process-output process)
      (goto-char (point-min)))
    (prog1
	(if (looking-at "[345]")
	    (progn
	      (nntp-snarf-error-message)
	      nil)
	  (goto-char (point-max))
	  (while (not (re-search-backward wait-for nil t))
	    (nntp-accept-process-output process))
	  (nntp-decode-text (not decode))
	  (save-excursion
	    (set-buffer buffer)
	    (insert-buffer-substring (process-buffer process))
	    t))
      (erase-buffer))))

(defun nntp-snarf-error-message ()
  "Save the error message in the current buffer."
  (setq nntp-status-string (buffer-string)))

(defun nntp-accept-process-output (process)
  "Wait for output from PROCESS and message some dots."
  (message "Reading%s" (make-string (/ (point-max) 1000) ?.))
  (accept-process-output process))

(defun nntp-accept-response ()
  "Wait for output from the process that outputs to BUFFER."
  (nntp-accept-process-output (nntp-find-connection nntp-server-buffer)))

(defun nntp-possibly-change-group (group server &optional connectionless)
  (when server
    (or (nntp-server-opened server)
	(nntp-open-server server nil connectionless)))
  
  (or (nntp-find-connection nntp-server-buffer)
      (nntp-open-connection nntp-server-buffer))

  (when group
    (let ((entry (nntp-find-connection-entry nntp-server-buffer)))
      (when (not (equal group (caddr entry)))
	(nntp-request-group group)))))

(defun nntp-decode-text (&optional cr-only)
  "Decode the text in the current buffer."
  (goto-char (point-min))
  (while (search-forward "\r" nil t)
    (delete-char -1))
  (unless cr-only
    (goto-char (point-max))
    (forward-line -1)
    (when (looking-at ".\n")
      (delete-char 2))
    (goto-char (point-min))
    (delete-region (point) (progn (forward-line 1) (point)))
    (while (search-forward "\n.." nil t)
      (delete-char -1))))

(defun nntp-retrieve-headers-with-xover (articles &optional fetch-old)
  (erase-buffer)
  (cond 

   ;; This server does not talk NOV.
   ((not nntp-server-xover)
    nil)

   ;; We don't care about gaps.
   ((or (not nntp-nov-gap)
	fetch-old)
    (nntp-send-xover-command 
     (if fetch-old
	 (if (numberp fetch-old) 
	     (max 1 (- (car articles) fetch-old)) 
	   1)
       (car articles))
     (last articles) 'wait)

    (goto-char (point-min))
    (when (looking-at "[1-5][0-9][0-9] ")
      (delete-region (point) (progn (forward-line 1) (point))))
    (while (search-forward "\r" nil t)
      (replace-match "" t t))
    (goto-char (point-max))
    (forward-line -1)
    (when (looking-at "\\.")
      (delete-region (point) (progn (forward-line 1) (point)))))

   ;; We do it the hard way.  For each gap, an XOVER command is sent
   ;; to the server.  We do not wait for a reply from the server, we
   ;; just send them off as fast as we can.  That means that we have
   ;; to count the number of responses we get back to find out when we
   ;; have gotten all we asked for.
   ((numberp nntp-nov-gap)
    (let ((count 0)
	  (received 0)
	  (last-point (point-min))
	  (buf nntp-server-buffer) ;(process-buffer (nntp-find-connection (current-buffer))))
	  first)
      ;; We have to check `nntp-server-xover'.  If it gets set to nil,
      ;; that means that the server does not understand XOVER, but we
      ;; won't know that until we try.
      (while (and nntp-server-xover articles)
	(setq first (car articles))
	;; Search forward until we find a gap, or until we run out of
	;; articles. 
	(while (and (cdr articles) 
		    (< (- (nth 1 articles) (car articles)) nntp-nov-gap))
	  (setq articles (cdr articles)))

	(when (nntp-send-xover-command first (car articles))
	  (setq articles (cdr articles)
		count (1+ count))

	  ;; Every 400 requests we have to read the stream in
	  ;; order to avoid deadlocks.
	  (when (or (null articles)	;All requests have been sent.
		    (zerop (% count nntp-maximum-request)))
	    (accept-process-output)
	    ;; On some Emacs versions the preceding function has
	    ;; a tendency to change the buffer. Perhaps. It's
	    ;; quite difficult to reproduce, because it only
	    ;; seems to happen once in a blue moon. 
	    (set-buffer buf) 
	    (while (progn
		     (goto-char last-point)
		     ;; Count replies.
		     (while (re-search-forward "^[0-9][0-9][0-9] " nil t)
		       (setq received (1+ received)))
		     (setq last-point (point))
		     (< received count))
	      (accept-process-output)
	      (set-buffer buf)))))

      (when nntp-server-xover
	;; Wait for the reply from the final command.
	(goto-char (point-max))
	(re-search-backward "^[0-9][0-9][0-9] " nil t)
	(when (looking-at "^[23]")
	  (while (progn
		   (goto-char (point-max))
		   (forward-line -1)
		   (not (looking-at "^\\.\r?\n")))
	    (nntp-accept-response)))
	
	;; We remove any "." lines and status lines.
	(goto-char (point-min))
	(while (search-forward "\r" nil t)
	  (delete-char -1))
	(goto-char (point-min))
	(delete-matching-lines "^\\.$\\|^[1-5][0-9][0-9] ")
	;(save-excursion
	;  (set-buffer nntp-server-buffer)
	;  (insert-buffer-substring buf))
	;(erase-buffer)
	))))

  nntp-server-xover)

(defun nntp-send-xover-command (beg end &optional wait-for-reply)
  "Send the XOVER command to the server."
  (let ((range (format "%d-%d" beg end))
	(nntp-inhibit-erase t))
    (if (stringp nntp-server-xover)
	;; If `nntp-server-xover' is a string, then we just send this
	;; command.
	(if wait-for-reply
	    (nntp-send-command "\r\n\\.\r\n" nntp-server-xover range)
	  ;; We do not wait for the reply.
	  (nntp-send-command "\r\n\\.\r\n" nntp-server-xover range))
      (let ((commands nntp-xover-commands))
	;; `nntp-xover-commands' is a list of possible XOVER commands.
	;; We try them all until we get at positive response. 
	(while (and commands (eq nntp-server-xover 'try))
	  (nntp-send-command "\r\n\\.\r\n" (car commands) range)
	  (save-excursion
	    (set-buffer nntp-server-buffer)
	    (goto-char (point-min))
	    (and (looking-at "[23]") ; No error message.
		 ;; We also have to look at the lines.  Some buggy
		 ;; servers give back simple lines with just the
		 ;; article number.  How... helpful.
		 (progn
		   (forward-line 1)
		   (looking-at "[0-9]+\t...")) ; More text after number.
		 (setq nntp-server-xover (car commands))))
	  (setq commands (cdr commands)))
	;; If none of the commands worked, we disable XOVER.
	(when (eq nntp-server-xover 'try)
	  (save-excursion
	    (set-buffer nntp-server-buffer)
	    (erase-buffer)
	    (setq nntp-server-xover nil)))
	nntp-server-xover))))

(provide 'nntp)

;;; nntp.el ends here
