;;; nndir.el --- single directory newsgroup access for Gnus
;; Copyright (C) 1995 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@ifi.uio.no>
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
(require 'nnmh)
(require 'nnml)

(eval-and-compile
  (autoload 'mail-send-and-exit "sendmail"))

(defvar nndir-directory nil)



(defconst nndir-version "nndir 1.0")

(defvar nndir-status-string "")

(defvar nndir-nov-is-evil nil
  "*Non-nil means that nndir will never retrieve NOV headers.")



(defvar nndir-current-server nil)
(defvar nndir-server-alist nil)
(defvar nndir-server-variables 
  (list
   '(nndir-directory nil)
   '(nndir-status-string "")
   '(nndir-group-alist)))



;;; Interface functions.


(defun nndir-retrieve-headers (sequence &optional group server fetch-old)
  (nndir-execute-nnml-command
   (` (nnml-retrieve-headers 
       (quote (, sequence)) (, group) (, server) (, fetch-old)))))

(defun nndir-open-server (server &optional defs)
  (nnheader-init-server-buffer)
  (if (equal server nndir-current-server)
      t
    (if nndir-current-server
	(setq nndir-server-alist 
	      (cons (list nndir-current-server
			  (nnheader-save-variables nndir-server-variables))
		    nndir-server-alist)))
    (let ((state (assoc server nndir-server-alist)))
      (if state 
	  (progn
	    (nnheader-restore-variables (nth 1 state))
	    (setq nndir-server-alist (delq state nndir-server-alist)))
	(nnheader-set-init-variables nndir-server-variables defs))
      (or (assq 'nndir-directory defs)
	  (setq nndir-directory server)))
    (setq nndir-current-server server)))

(defun nndir-close-server (&optional server)
  t)

(defun nndir-server-opened (&optional server)
  (and nntp-server-buffer
       (get-buffer nntp-server-buffer)
       nndir-current-server
       (equal nndir-current-server server)))

(defun nndir-status-message (&optional server)
  nndir-status-string)

(defun nndir-request-article (id &optional group server buffer)
  (nndir-execute-nnmh-command
   (` (nnmh-request-article (, id) (, group) (, server) (, buffer)))))

(defun nndir-request-group (group &optional server dont-check)
  (nndir-execute-nnmh-command
   (` (nnmh-request-group (, group) "" (, dont-check)))))

(defun nndir-request-list (&optional server dir)
  (nndir-execute-nnmh-command
   (` (nnmh-request-list nil (, dir)))))

(defun nndir-request-newgroups (date &optional server)
  (nndir-execute-nnmh-command
   (` (nnmh-request-newgroups (, date) (, server)))))

(defun nndir-request-post (&optional server)
  (mail-send-and-exit nil))

(defun nndir-request-expire-articles 
  (articles group &optional server force)
  (nndir-execute-nnmh-command
   (` (nnmh-request-expire-articles (, articles) (, group) 
				    (, server) (, force)))))

(defun nndir-request-accept-article (group &optional last)
  (nndir-execute-nnmh-command
   (` (nnmh-request-accept-article (, group) (, last)))))

(defun nndir-close-group (group &optional server)
  t)

(defun nndir-request-create-group (group &optional server)
  (if (file-exists-p nndir-directory)
      (if (file-directory-p nndir-directory)
	  t
	nil)
    (condition-case ()
	(progn
	  (make-directory nndir-directory t)
	  t)
      (file-error nil))))


;;; Low-Level Interface

(defun nndir-execute-nnmh-command (command)
  (let ((dir (expand-file-name nndir-directory)))
    (and (string-match "/$" dir)
	 (setq dir (substring dir 0 (match-beginning 0))))
    (string-match "/[^/]+$" dir)
    (let ((group (substring dir (1+ (match-beginning 0))))
	  (nnmh-directory (substring dir 0 (1+ (match-beginning 0))))
	  (nnmh-get-new-mail nil))
      (eval command))))

(defun nndir-execute-nnml-command (command)
  (let ((dir (expand-file-name nndir-directory)))
    (and (string-match "/$" dir)
	 (setq dir (substring dir 0 (match-beginning 0))))
    (string-match "/[^/]+$" dir)
    (let ((group (substring dir (1+ (match-beginning 0))))
	  (nnml-directory (substring dir 0 (1+ (match-beginning 0))))
	  (nnml-nov-is-evil nndir-nov-is-evil)
	  (nnml-get-new-mail nil))
      (eval command))))

(provide 'nndir)

;;; nndir.el ends here
