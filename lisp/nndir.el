;;; nndir.el --- single directory newsgroup access for Gnus
;; Copyright (C) 1995,96 Free Software Foundation, Inc.

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
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(require 'nnheader)
(require 'nnmh)
(require 'nnml)
(eval-when-compile (require 'cl))

(defvar nndir-directory nil
  "Where nndir will look for groups.")

(defvar nndir-nov-is-evil nil
  "*Non-nil means that nndir will never retrieve NOV headers.")



(defconst nndir-version "nndir 1.0")

(defvar nndir-status-string "")

(defvar nndir-group "blououUOUOuuubhbh")



(defvar nndir-current-server nil)
(defvar nndir-server-alist nil)
(defvar nndir-server-variables 
  `((nndir-directory nil)
    (nndir-status-string "")
    (nndir-nov-is-evil ,nndir-nov-is-evil)
    (nndir-group-alist nil)))



;;; Interface functions.


(defun nndir-retrieve-headers (sequence &optional 
					nndir-group server fetch-old)
  (nndir-execute-nnml-command
   `(nnml-retrieve-headers ',sequence nndir-group ,server ,fetch-old)))

(defun nndir-open-server (server &optional defs)
  (nnheader-change-server 'nndir server defs)
  (unless (assq 'nndir-directory defs)
    (setq nndir-directory server))
  (let (err)
    (cond 
     ((not (condition-case arg
	       (file-exists-p nndir-directory)
	     (ftp-error (setq err (format "%s" arg)))))
      (nndir-close-server)
      (nnheader-report 
       'nndir (or err "No such file or directory: %s" nndir-directory)))
     ((not (file-directory-p (file-truename nndir-directory)))
      (nndir-close-server)
      (nnheader-report 'nndir "Not a directory: %s" nndir-directory))
     (t
      (nnheader-report 'nndir "Opened server %s using directory %s"
		       server nndir-directory)
      t))))

(defun nndir-close-server (&optional server)
  (setq nndir-current-server nil)
  t)

(defun nndir-server-opened (&optional server)
  (and nntp-server-buffer
       (get-buffer nntp-server-buffer)
       nndir-current-server
       (equal nndir-current-server server)))

(defun nndir-status-message (&optional server)
  nndir-status-string)

(defun nndir-request-article (id &optional nndir-group server buffer)
  (nndir-execute-nnmh-command
   `(nnmh-request-article ,id nndir-group ,server ,buffer)))

(defun nndir-request-group (nndir-group &optional server dont-check)
  (nndir-execute-nnmh-command
   `(nnmh-request-group nndir-group "" ,dont-check)))

(defun nndir-request-list (&optional server dir)
  (let ((nndir-directory (concat (file-name-as-directory
				  nndir-directory) "dummy")))
    (nndir-execute-nnmh-command
     `(nnmh-request-list ,(concat "nndir+" (or server "")) ,dir))))

(defun nndir-request-newgroups (date &optional server)
  (nndir-execute-nnmh-command
   `(nnmh-request-newgroups ,date ,server)))

(defun nndir-request-expire-articles 
  (articles nndir-group &optional server force)
  (nndir-execute-nnmh-command
   `(nnmh-request-expire-articles ',articles nndir-group ,server ,force)))

(defun nndir-request-accept-article (nndir-group &optional last)
  (nndir-execute-nnmh-command
   `(nnmh-request-accept-article nndir-group ,last)))

(defun nndir-close-group (nndir-group &optional server)
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
  (let ((dir (file-name-as-directory (expand-file-name nndir-directory))))
    (if (and (not (file-directory-p nndir-group))
	     (or (file-directory-p (concat dir nndir-group))
		 (file-directory-p
		  (concat dir (nnheader-replace-chars-in-string 
			       nndir-group ?. ?/)))))
	(let ((nnmh-directory nndir-directory)
	      (nnmh-get-new-mail nil))
	  (eval command))
      (let ((dir (directory-file-name (expand-file-name nndir-directory))))
	(string-match "/[^/]+$" dir)
	(let ((nndir-group (substring dir (1+ (match-beginning 0))))
	      (nnmh-directory (substring dir 0 (1+ (match-beginning 0))))
	      (nnmh-get-new-mail nil))
	  (eval command))))))

(defun nndir-execute-nnml-command (command)
  (let ((dir (file-name-as-directory (expand-file-name nndir-directory))))
    (if (and (not (file-directory-p nndir-group))
	     (or (file-directory-p (concat dir nndir-group))
		 (file-directory-p
		  (concat dir (nnheader-replace-chars-in-string 
			       nndir-group ?. ?/)))))
	(let ((nnml-directory nndir-directory)
	      (nnml-nov-is-evil nndir-nov-is-evil)
	      (nnml-get-new-mail nil))
	  (eval command))
      (let ((dir (directory-file-name (expand-file-name nndir-directory))))
	(string-match "/[^/]+$" dir)
	(let ((nndir-group (substring dir (1+ (match-beginning 0))))
	      (nnml-directory (substring dir 0 (1+ (match-beginning 0))))
	      (nnml-nov-is-evil nndir-nov-is-evil)
	      (nnml-get-new-mail nil))
	  (eval command))))))

(provide 'nndir)

;;; nndir.el ends here
