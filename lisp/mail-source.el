;;; mail-source.el --- functions for fetching mail
;; Copyright (C) 1999 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: news, mail

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

;;; Code:

(eval-when-compile (require 'cl))
(eval-and-compile
  (autoload 'pop3-movemail "pop3"))
(require 'format-spec)

(defgroup mail-source nil
  "The mail-fetching library."
  :group 'gnus)

(defcustom mail-sources nil
  "*Where the mail backends will look for incoming mail.
This variable is a list of mail source specifiers."
  :group 'mail-source
  :type 'sexp)

(defcustom mail-source-crash-box "~/.emacs-mail-crash-box"
  "File where mail will be stored while processing it."
  :group 'mail-source
  :type 'file)

(defcustom mail-source-directory "~/Mail/"
  "Directory where files (if any) will be stored."
  :group 'mail-source
  :type 'directory)

(defcustom mail-source-default-file-modes 384
  "Set the mode bits of all new mail files to this integer."
  :group 'mail-source
  :type 'integer)

(defcustom mail-source-delete-incoming nil
  "*If non-nil, delete incoming files after handling."
  :group 'mail-source
  :type 'boolean)

;;; Internal variables.

(defvar mail-source-string ""
  "A dynamically bound string that says what the current mail source is.")

(eval-and-compile
  (defvar mail-source-keyword-map
    '((file
       (:prescript)
       (:prescript-delay)
       (:postscript)
       (:path (or (getenv "MAIL")
		  (concat "/usr/spool/mail/" (user-login-name)))))
      (directory
       (:path)
       (:suffix ".spool")
       (:predicate identity))
      (pop
       (:prescript)
       (:prescript-delay)
       (:postscript)
       (:server (getenv "MAILHOST"))
       (:port 110)
       (:user (or (user-login-name) (getenv "LOGNAME") (getenv "USER")))
       (:program)
       (:function)
       (:password)
       (:authentication password))
      (maildir
       (:path "~/Maildir/new/")))
    "Mapping from keywords to default values.
All keywords that can be used must be listed here."))

(defvar mail-source-fetcher-alist
  '((file mail-source-fetch-file)
    (directory mail-source-fetch-directory)
    (pop mail-source-fetch-pop)
    (maildir mail-source-fetch-maildir))
  "A mapping from source type to fetcher function.")

(defvar mail-source-password-cache nil)

;;; Functions

(eval-and-compile
  (defun mail-source-strip-keyword (keyword)
  "Strip the leading colon off the KEYWORD."
  (intern (substring (symbol-name keyword) 1))))

(eval-and-compile
  (defun mail-source-bind-1 (type)
    (let* ((defaults (cdr (assq type mail-source-keyword-map)))
	   default bind)
      (while (setq default (pop defaults))
	(push (list (mail-source-strip-keyword (car default))
		    nil)
	      bind))
      bind)))

(defmacro mail-source-bind (type-source &rest body)
  "Return a `let' form that binds all variables in source TYPE.
TYPE-SOURCE is a list where the first element is the TYPE, and
the second variable is the SOURCE.
At run time, the mail source specifier SOURCE will be inspected,
and the variables will be set according to it.  Variables not
specified will be given default values.

After this is done, BODY will be executed in the scope
of the `let' form.

The variables bound and their default values are described by
the `mail-source-keyword-map' variable."
  `(let ,(mail-source-bind-1 (car type-source))
     (mail-source-set-1 ,(cadr type-source))
     ,@body))

(put 'mail-source-bind 'lisp-indent-function 1)
(put 'mail-source-bind 'edebug-form-spec '(form body))

(defun mail-source-set-1 (source)
  (let* ((type (pop source))
	 (defaults (cdr (assq type mail-source-keyword-map)))
	 default value keyword)
    (while (setq default (pop defaults))
      (set (mail-source-strip-keyword (setq keyword (car default)))
	   (if (setq value (plist-get source keyword))
	       (mail-source-value value)
	     (mail-source-value (cadr default)))))))

(defun mail-source-value (value)
  "Return the value of VALUE."
  (cond
   ;; String
   ((stringp value)
    value)
   ;; Function
   ((and (listp value)
	 (functionp (car value)))
    (eval value))
   ;; Just return the value.
   (t
    value)))

(defun mail-source-fetch (source callback)
  "Fetch mail from SOURCE and call CALLBACK zero or more times.
CALLBACK will be called with the name of the file where (some of)
the mail from SOURCE is put.
Return the number of files that were found."
  (save-excursion
    (let ((function (cadr (assq (car source) mail-source-fetcher-alist)))
	  (found 0))
      (unless function
	(error "%S is an invalid mail source specification" source))
      ;; If there's anything in the crash box, we do it first.
      (when (file-exists-p mail-source-crash-box)
	(message "Processing mail from %s..." mail-source-crash-box)
	(setq found (mail-source-callback
                     callback mail-source-crash-box)))
      (+ found
         (condition-case err
             (funcall function source callback)
           (error
            (unless (yes-or-no-p
		     (format "Mail source error (%s).  Continue? " err))
              (error "Cannot get new mail."))
            0))))))

(defun mail-source-make-complex-temp-name (prefix)
  (let ((newname (make-temp-name prefix))
	(newprefix prefix))
    (while (file-exists-p newname)
      (setq newprefix (concat newprefix "x"))
      (setq newname (make-temp-name newprefix)))
    newname))

(defun mail-source-callback (callback info)
  "Call CALLBACK on the mail file, and then remove the mail file.
Pass INFO on to CALLBACK."
  (if (or (not (file-exists-p mail-source-crash-box))
	  (zerop (nth 7 (file-attributes mail-source-crash-box))))
      (progn
	(when (file-exists-p mail-source-crash-box)
	  (delete-file mail-source-crash-box))
	0)
    (prog1
	(funcall callback mail-source-crash-box info)
      (when (file-exists-p mail-source-crash-box)
	;; Delete or move the incoming mail out of the way.
	(if mail-source-delete-incoming
	    (delete-file mail-source-crash-box)
	  (let ((incoming
		 (mail-source-make-complex-temp-name
		  (expand-file-name
		   "Incoming" mail-source-directory))))
	    (unless (file-exists-p (file-name-directory incoming))
	      (make-directory (file-name-directory incoming) t))
	    (rename-file mail-source-crash-box incoming t)))))))

(defun mail-source-movemail (from to)
  "Move FROM to TO using movemail."
  (if (not (file-writable-p to))
      (error "Can't write to crash box %s.  Not moving mail" to)
    (let ((to (file-truename (expand-file-name to)))
	  errors result)
      (setq to (file-truename to)
	    from (file-truename from))
      ;; Set TO if have not already done so, and rename or copy
      ;; the file FROM to TO if and as appropriate.
      (cond
       ((file-exists-p to)
	;; The crash box exists already.
	t)
       ((not (file-exists-p from))
	;; There is no inbox.
	(setq to nil))
       ((zerop (nth 7 (file-attributes from)))
	;; Empty file.
	(setq to nil))
       (t
	;; If getting from mail spool directory, use movemail to move
	;; rather than just renaming, so as to interlock with the
	;; mailer.
	(unwind-protect
	    (save-excursion
	      (setq errors (generate-new-buffer " *mail source loss*"))
	      (let ((default-directory "/"))
		(setq result
		      (apply
		       'call-process
		       (append
			(list
			 (expand-file-name "movemail" exec-directory)
			 nil errors nil from to)))))
	      (when (file-exists-p to)
		(set-file-modes to mail-source-default-file-modes))
	      (if (and (not (buffer-modified-p errors))
		       (zerop result))
		  ;; No output => movemail won.
		  t
		(set-buffer errors)
		;; There may be a warning about older revisions.  We
		;; ignore that.
		(goto-char (point-min))
		(if (search-forward "older revision" nil t)
		    t
		  ;; Probably a real error.
		  (subst-char-in-region (point-min) (point-max) ?\n ?\  )
		  (goto-char (point-max))
		  (skip-chars-backward " \t")
		  (delete-region (point) (point-max))
		  (goto-char (point-min))
		  (when (looking-at "movemail: ")
		    (delete-region (point-min) (match-end 0)))
		  (unless (yes-or-no-p
			   (format "movemail: %s (%d return).  Continue? "
				   (buffer-string) result))
		    (error "%s" (buffer-string)))
		  (setq to nil)))))))
      (when (and errors
		 (buffer-name errors))
	(kill-buffer errors))
      ;; Return whether we moved successfully or not.
      to)))

(defvar mail-source-read-passwd nil)
(defun mail-source-read-passwd (prompt &rest args)
  "Read a password using PROMPT.
If ARGS, PROMPT is used as an argument to `format'."
  (let ((prompt
	 (if args
	     (apply 'format prompt args)
	   prompt)))
    (unless mail-source-read-passwd
      (if (or (fboundp 'read-passwd) (load "passwd" t))
	  (setq mail-source-read-passwd 'read-passwd)
	(unless (fboundp 'ange-ftp-read-passwd)
	  (autoload 'ange-ftp-read-passwd "ange-ftp"))
	(setq mail-source-read-passwd 'ange-ftp-read-passwd)))
    (funcall mail-source-read-passwd prompt)))

(defun mail-source-fetch-with-program (program)
  (zerop (call-process shell-file-name nil nil nil
		       shell-command-switch program)))

(defun mail-source-run-script (script spec &optional delay)
  (when script
    (if (and (symbolp script) (fboundp script))
	(funcall script)
      (mail-source-call-script
       (format-spec script spec))))
  (when delay
    (sleep-for delay)))

(defun mail-source-call-script (script)
  (let ((background nil))
    (when (string-match "& *$" script)
      (setq script (substring script 0 (match-beginning 0))
	    background 0))
    (call-process shell-file-name nil background nil
		  shell-command-switch script)))

;;;
;;; Different fetchers
;;;

(defun mail-source-fetch-file (source callback)
  "Fetcher for single-file sources."
  (mail-source-bind (file source)
    (mail-source-run-script
     prescript (format-spec-make ?t mail-source-crash-box)
     prescript-delay)
    (let ((mail-source-string (format "file:%s" path)))
      (if (mail-source-movemail path mail-source-crash-box)
	  (prog1
	      (mail-source-callback callback path)
	    (mail-source-run-script
	     postscript (format-spec-make ?t mail-source-crash-box)))
	0))))

(defun mail-source-fetch-directory (source callback)
  "Fetcher for directory sources."
  (mail-source-bind (directory source)
    (let ((found 0)
	  (mail-source-string (format "directory:%s" path)))
      (dolist (file (directory-files
		     path t (concat (regexp-quote suffix) "$")))
	(when (and (file-regular-p file)
		   (funcall predicate file)
		   (mail-source-movemail file mail-source-crash-box))
	  (incf found (mail-source-callback callback file))))
      found)))

(defun mail-source-fetch-pop (source callback)
  "Fetcher for single-file sources."
  (mail-source-bind (pop source)
    (mail-source-run-script
     prescript
     (format-spec-make ?p password ?t mail-source-crash-box
				      ?s server ?P port ?u user)
     prescript-delay)
    (let ((from (format "%s:%s:%s" server user port))
	  (mail-source-string (format "pop:%s@%s" user server))
	  result)
      (when (eq authentication 'password)
	(setq password
	      (or password
		  (cdr (assoc from mail-source-password-cache))
		  (mail-source-read-passwd
		   (format "Password for %s at %s: " user server))))
	(unless (assoc from mail-source-password-cache)
	  (push (cons from password) mail-source-password-cache)))
      (when server
	(setenv "MAILHOST" server))
      (setq result
	    (cond
	     (program
	      (mail-source-fetch-with-program
	       (format-spec
		program
		(format-spec-make ?p password ?t mail-source-crash-box
				  ?s server ?P port ?u user))))
	     (function
	      (funcall function mail-source-crash-box))
	     ;; The default is to use pop3.el.
	     (t
	      (let ((pop3-password password)
		    (pop3-maildrop user)
		    (pop3-mailhost server)
		    (pop3-port port)
		    (pop3-authentication-scheme
		     (if (eq authentication 'apop) 'apop 'pass)))
		(save-excursion (pop3-movemail mail-source-crash-box))))))
      (if result
	  (prog1
	      (mail-source-callback callback server)
	    (mail-source-run-script
	     postscript
	     (format-spec-make ?p password ?t mail-source-crash-box
			       ?s server ?P port ?u user)))
	;; We nix out the password in case the error
	;; was because of a wrong password being given.
	(setq mail-source-password-cache
	      (delq (assoc from mail-source-password-cache)
		    mail-source-password-cache))
	0))))

(defun mail-source-fetch-maildir (source callback)
  "Fetcher for maildir sources."
  (mail-source-bind (maildir source)
    (let ((found 0)
	  (mail-source-string (format "maildir:%s" path)))
      (dolist (file (directory-files path t))
	(when (and (file-regular-p file)
		   (not (rename-file file mail-source-crash-box)))
	  (incf found (mail-source-callback callback file))))
      found)))

(provide 'mail-source)

;;; mail-source.el ends here
