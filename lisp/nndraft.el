;;; nndraft.el --- draft article access for Gnus
;; Copyright (C) 1995 Free Software Foundation, Inc.

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

(require 'nnheader)
(require 'nnmh)
(eval-and-compile (require 'cl))

(eval-and-compile
  (autoload 'mail-send-and-exit "sendmail"))

(defvar nndraft-directory nil)



(defconst nndraft-version "nndraft 1.0")
(defvar nndraft-status-string "")



(defvar nndraft-current-server nil)
(defvar nndraft-server-alist nil)
(defvar nndraft-server-variables 
  (list
   '(nndraft-directory nil)
   '(nndraft-status-string "")
   '(nndraft-group-alist)))



;;; Interface functions.


(defun nndraft-retrieve-headers (articles &optional group server fetch-old)
  (save-excursion
    (set-buffer nntp-server-buffer)
    (erase-buffer)
    (let* ((file nil)
	   (buf (get-buffer-create " *draft headers*"))
	   beg article)
      (set-buffer buf)
      (buffer-disable-undo (current-buffer))
      (erase-buffer)
      ;; We don't support fetching by Message-ID.
      (if (stringp (car articles))
	  'headers
	(while articles
	  (set-buffer buf)
	  (when (nndraft-request-article 
		 (setq article (pop articles)) group server (current-buffer))
	    (goto-char (point-min))
	    (if (search-forward "\n\n" nil t)
		(forward-line -1)
	      (goto-char (point-max)))
	    (delete-region (point) (point-max))
	    (set-buffer nntp-server-buffer)
	    (goto-char (point-max))
	    (insert (format "221 %d Article retrieved.\n" article))
	    (insert-buffer-substring buf)
	    (insert ".\n")))

	(nnheader-fold-continuation-lines)
	'headers))))

(defun nndraft-open-server (server &optional defs)
  (nnheader-init-server-buffer)
  (if (equal server nndraft-current-server)
      t
    (if nndraft-current-server
	(setq nndraft-server-alist 
	      (cons (list nndraft-current-server
			  (nnheader-save-variables nndraft-server-variables))
		    nndraft-server-alist)))
    (let ((state (assoc server nndraft-server-alist)))
      (if state 
	  (progn
	    (nnheader-restore-variables (nth 1 state))
	    (setq nndraft-server-alist (delq state nndraft-server-alist)))
	(nnheader-set-init-variables nndraft-server-variables defs))
      (or (assq 'nndraft-directory defs)
	  (setq nndraft-directory server)))
    (setq nndraft-current-server server)))

(defun nndraft-close-server (&optional server)
  t)

(defun nndraft-server-opened (&optional server)
  (and nntp-server-buffer
       (get-buffer nntp-server-buffer)
       nndraft-current-server
       (equal nndraft-current-server server)))

(defun nndraft-status-message (&optional server)
  nndraft-status-string)

(defun nndraft-request-article (id &optional group server buffer)
  (when (numberp id)
    ;; We get the newest file of the auto-saved file and the 
    ;; "real" file.
    (let* ((file (nndraft-article-filename id))
	   (auto (nndraft-auto-save-file-name file))
	   (newest (if (file-newer-than-file-p file auto) file auto))
	   (nntp-server-buffer (or buffer nntp-server-buffer)))
      (when (and (file-exists-p newest)
		 (nnmail-find-file newest))
	(save-excursion 
	  (set-buffer nntp-server-buffer)
	  (goto-char (point-min))
	  ;; If there's a mail header separator in this file, 
	  ;; we remove it.
	  (when (re-search-forward
		 (concat "^" mail-header-separator "$") nil t)
	    (replace-match "" t t)))
	t))))

(defun nndraft-request-restore-buffer (article &optional group server)
  "Request a new buffer that is restored to the state of ARTICLE."
  (let ((file (nndraft-article-filename article ".state"))
	nndraft-point nndraft-mode nndraft-buffer-name)
    (when (file-exists-p file)
      (load file t t t)
      (when nndraft-buffer-name
	(set-buffer (get-buffer-create
		     (generate-new-buffer-name nndraft-buffer-name)))
	(nndraft-request-article article group server (current-buffer))
	(funcall nndraft-mode)
	(let ((gnus-verbose-backends nil))
	  (nndraft-request-expire-articles (list article) group server t))
	(goto-char nndraft-point))
      nndraft-buffer-name)))

(defun nndraft-request-update-info (group info &optional server)
  (setcar (cdr (cdr info)) nil)
  (when (nth 3 info)
    (setcar (nthcdr 3 info) nil)))

(defun nndraft-request-associate-buffer (group)
  "Associate the current buffer with some article in the draft group."
  (let* ((gnus-verbose-backends nil)
	 (article (cdr (nndraft-request-accept-article group t 'noinsert)))
	 (file (nndraft-article-filename article)))
    (setq buffer-file-name file)
    (setq buffer-auto-save-file-name (make-auto-save-file-name))
    article))

(defun nndraft-request-group (group &optional server dont-check)
  (nndraft-execute-nnmh-command
   (` (nnmh-request-group group "" (, dont-check)))))

(defun nndraft-request-list (&optional server dir)
  (nndraft-execute-nnmh-command
   (` (nnmh-request-list nil (, dir)))))

(defun nndraft-request-newgroups (date &optional server)
  (nndraft-execute-nnmh-command
   (` (nnmh-request-newgroups (, date) (, server)))))

(defun nndraft-request-post (&optional server)
  (mail-send-and-exit nil))

(defun nndraft-request-expire-articles 
  (articles group &optional server force)
  (let ((res (nndraft-execute-nnmh-command
	      (` (nnmh-request-expire-articles
		  (quote (, articles)) group (, server) (, force)))))
	article)
    ;; Delete all the "state" files of articles that have been expired.
    (while articles
      (unless (memq (setq article (pop articles)) res)
	(let ((file (nndraft-article-filename article ".state"))
	      (auto (nndraft-auto-save-file-name
		     (nndraft-article-filename article))))
	  (when (file-exists-p file)
	    (funcall nnmail-delete-file-function file))
	  (when (file-exists-p auto)
	    (funcall nnmail-delete-file-function auto)))))
    res))

(defun nndraft-request-accept-article (group &optional last noinsert)
  (let* ((point (point))
	 (mode major-mode)
	 (name (buffer-name))
	 (gart (nndraft-execute-nnmh-command
		(` (nnmh-request-accept-article group (, last) noinsert))))
	 (state
	  (nndraft-article-filename (cdr gart) ".state")))
    ;; Write the "state" file.
    (save-excursion
      (nnheader-set-temp-buffer " *draft state*")
      (insert (format "%S\n" `(setq nndraft-mode (quote ,mode)
				    nndraft-point ,point
				    nndraft-buffer-name ,name)))
      (write-region (point-min) (point-max) state nil 'silent)
      (kill-buffer (current-buffer)))
    gart))

(defun nndraft-close-group (group &optional server)
  t)

(defun nndraft-request-create-group (group &optional server)
  (if (file-exists-p nndraft-directory)
      (if (file-directory-p nndraft-directory)
	  t
	nil)
    (condition-case ()
	(progn
	  (make-directory nndraft-directory t)
	  t)
      (file-error nil))))


;;; Low-Level Interface

(defun nndraft-execute-nnmh-command (command)
  (let ((dir (expand-file-name nndraft-directory)))
    (and (string-match "/$" dir)
	 (setq dir (substring dir 0 (match-beginning 0))))
    (string-match "/[^/]+$" dir)
    (let ((group (substring dir (1+ (match-beginning 0))))
          (nnmh-directory (substring dir 0 (1+ (match-beginning 0))))
	  (nnmh-get-new-mail nil))
      (eval command))))

(defun nndraft-article-filename (article &rest args)
  (apply 'concat
	 (file-name-as-directory nndraft-directory)
	 (int-to-string article)
	 args))

(defun nndraft-auto-save-file-name (file)
  (save-excursion
    (prog1
	(progn
	  (set-buffer (get-buffer-create " *draft tmp*"))
	  (setq buffer-file-name file)
	  (make-auto-save-file-name))
      (kill-buffer (current-buffer)))))

(provide 'nndraft)

;;; nndraft.el ends here
