;;; nndoc.el --- single file access for Gnus
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
(require 'rmail)
(require 'nnmail)



(defconst nndoc-version "nndoc 0.1"
  "nndoc version.")

(defvar nndoc-current-buffer nil
  "Current nndoc news buffer.")

(defvar nndoc-status-string "")

(defvar nndoc-group-alist nil)

;;; Interface functions

(defun nndoc-retrieve-headers (sequence &optional newsgroup server)
  "Retrieve the headers for the articles in SEQUENCE.
Newsgroup must be selected before calling this function."
  (save-excursion
    (set-buffer nntp-server-buffer)
    (erase-buffer)
    (let ((file nil)
	  (number (length sequence))
	  (count 0)
	  beg article art-string start stop lines)
      (nndoc-possibly-change-buffer newsgroup server)
      (while sequence
	(setq article (car sequence))
	(set-buffer nndoc-current-buffer)
	(if (nndoc-search-for-article article)
	    (progn
	      (setq start 
		    (save-excursion
		      (or 
		       (re-search-backward 
			(concat "^" rmail-unix-mail-delimiter) nil t)
		       (point-min))))
	      (search-forward "\n\n" nil t)
	      (setq lines (count-lines 
			   (point)
			   (or
			    (save-excursion
			      (re-search-forward 
			       (concat "^" rmail-unix-mail-delimiter) nil t))
			    (point-max))))
	      (setq stop (1- (point)))
	      (set-buffer nntp-server-buffer)
	      (insert (format "221 %d Article retrieved.\n" article))
	      (setq beg (point))
	      (insert-buffer-substring nndoc-current-buffer start stop)
	      (goto-char (point-max))
	      (insert (format "Lines: %d\n" lines))
	      (insert ".\n")))
	(setq sequence (cdr sequence)))

      ;; Fold continuation lines.
      (goto-char 1)
      (while (re-search-forward "\\(\r?\n[ \t]+\\)+" nil t)
	(replace-match " " t t))
      'headers)))

(defun nndoc-open-server (host &optional service)
  "Open mbox backend."
  (setq nndoc-status-string "")
  (setq nndoc-group-alist nil)
  (nnheader-init-server-buffer))

(defun nndoc-close-server (&optional server)
  "Close news server."
  t)

(defun nndoc-server-opened (&optional server)
  "Return server process status."
  (and nntp-server-buffer
       (get-buffer nntp-server-buffer)))

(defun nndoc-status-message (&optional server)
  "Return server status response as string."
  nndoc-status-string)

(defun nndoc-request-article (article &optional newsgroup server buffer)
  "Select ARTICLE by number."
  (nndoc-possibly-change-buffer newsgroup server)
  (if (stringp article)
      nil
    (save-excursion
      (set-buffer nndoc-current-buffer)
      (if (nndoc-search-for-article article)
	  (let (start stop)
	    (re-search-backward (concat "^" rmail-unix-mail-delimiter) nil t)
	    (forward-line 1)
	    (setq start (point))
	    (or (and (re-search-forward 
		      (concat "^" rmail-unix-mail-delimiter) nil t)
		     (forward-line -1))
		(goto-char (point-max)))
	    (setq stop (point))
	    (let ((nntp-server-buffer (or buffer nntp-server-buffer)))
	      (set-buffer nntp-server-buffer)
	      (erase-buffer)
	      (insert-buffer-substring nndoc-current-buffer start stop)
	      t))))))

(defun nndoc-request-group (group &optional server dont-check)
  "Select news GROUP."
  (save-excursion
    (if (not (nndoc-possibly-change-buffer group server))
	(progn
	  (setq nndoc-status-string "No such file")
	  nil)
      (if dont-check
	  t
	(save-excursion
	  (set-buffer nntp-server-buffer)
	  (erase-buffer)
	  (let ((number (nndoc-number-of-articles)))
	    (if (zerop number)
		(progn
		  (nndoc-close-group group)
		  nil)
	      (insert (format "211 %d %d %d %s\n" 
			      number 1 number group))
	      t)))))))

(defun nndoc-close-group (group &optional server)
  (nndoc-possibly-change-buffer group server)
  (kill-buffer nndoc-current-buffer)
  (setq nndoc-group-alist (delq (assoc group nndoc-group-alist)
				nndoc-group-alist))
  (setq nndoc-current-buffer nil)
  t)

(defun nndoc-request-list (&optional server)
  nil)

(defun nndoc-request-newgroups (date &optional server)
  nil)

(defun nndoc-request-list-newsgroups (&optional server)
  nil)

(defun nndoc-request-post (&optional server)
  (mail-send-and-exit nil))

(fset 'nndoc-request-post-buffer 'nnmail-request-post-buffer)


;;; Internal functions.

(defun nndoc-possibly-change-buffer (group file)
  (let (buf)
    (or (and nndoc-current-buffer
	     (eq nndoc-current-buffer 
		 (setq buf (cdr (assoc group nndoc-group-alist)))))
	(if buf 
	    (setq nndoc-current-buffer buf)
	  (if (or (not (file-exists-p file))
		  (file-directory-p file))
	      ()
	    (setq nndoc-group-alist 
		  (cons (cons group (setq nndoc-current-buffer 
					  (get-buffer-create 
					   (concat " *nndoc " group "*"))))
			nndoc-group-alist))
	    (save-excursion
	      (set-buffer nndoc-current-buffer)
	      (buffer-disable-undo (current-buffer))
	      (erase-buffer)
	      (insert-file-contents file)
	      t))))))

(defun nndoc-number-of-articles ()
  (save-excursion
    (set-buffer nndoc-current-buffer)
    (goto-char (point-min))
    (let ((num 0)
	  (delim (concat "^" rmail-unix-mail-delimiter)))
      (while (re-search-forward delim nil t)
	(setq num (1+ num)))
      num)))

(defun nndoc-search-for-article (article)
  (let ((obuf (current-buffer)))
    (set-buffer nndoc-current-buffer)
    (goto-char (point-min))
    (let ((delim (concat "^" rmail-unix-mail-delimiter)))
      (while (and (re-search-forward delim nil t)
		  (not (zerop (setq article (1- article))))))
      (set-buffer obuf)
      (if (zerop article)
	  (progn
	    (forward-line 1)
	    t)
	nil))))

(provide 'nndoc)

;;; nndoc.el ends here
