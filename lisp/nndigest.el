;;; nndigest.el --- digest access for Gnus
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



(defconst nndigest-version "nndigest 0.0"
  "nndigest version.")

(defvar nndigest-current-buffer nil
  "Current digest "group" buffer.")

(defvar nndigest-status-string "")

(defvar nndigest-group-alist nil)

(defconst nndigest-separator 
  "^------------------------------[\n \t]*\n[^ ]+: ")



;;; Interface functions.

(defun nndigest-retrieve-headers (sequence &optional newsgroup server)
  (save-excursion
    (set-buffer nntp-server-buffer)
    (erase-buffer)
    (let ((file nil)
	  (number (length sequence))
	  (count 0)
	  range
	  beg article)
      (nndigest-possibly-change-buffer newsgroup)
      (while sequence
	(setq article (car sequence))
	(if (setq range (nndigest-narrow-to-article article))
	    (progn
	      (insert (format "221 %d Article retrieved.\n" article))
	      (setq beg (point))
	      (insert-buffer-substring nndigest-current-buffer 
				       (car range) (cdr range))
	      (goto-char beg)
	      (if (search-forward "\n\n" nil t)
		  (forward-char -1)
		(goto-char (point-max))
		(insert "\n\n"))
	      (insert (format "Lines: %d\n" (count-lines (point) (point-max))))
	      (insert ".\n")
	      (delete-region (point) (point-max))))
	(setq sequence (cdr sequence)))

      ;; Fold continuation lines.
      (goto-char 1)
      (while (re-search-forward "\\(\r?\n[ \t]+\\)+" nil t)
	(replace-match " " t t))
      'headers)))

(defun nndigest-open-server (host &optional service)
  (setq nndigest-status-string "")
  (nnheader-init-server-buffer))

(defun nndigest-close-server (&optional server)
  t)

(defun nndigest-server-opened (&optional server)
  (and nntp-server-buffer
       (get-buffer nntp-server-buffer)))

(defun nndigest-status-message ()
  nndigest-status-string)

(defun nndigest-request-article (id &optional newsgroup server buffer)
  (nndigest-possibly-change-buffer newsgroup)
  (let ((range (nndigest-narrow-to-article id)))
    (and range
	 (save-excursion
	   (set-buffer (or buffer nntp-server-buffer))
	   (erase-buffer)
	   (insert-buffer-substring 
	    nndigest-current-buffer (car range) (cdr range))
	   t))))

(defun nndigest-request-group (group &optional server dont-check)
  (let ((entry (assoc group nndigest-group-alist)))
    (and entry (setq nndigest-group-alist (delq entry nndigest-group-alist))))
  (let ((buffer (get-buffer-create (concat " *nndigest " group "*"))))
    (setq nndigest-group-alist 
	  (cons (cons group buffer) nndigest-group-alist))
    (save-excursion
      (set-buffer buffer)
      (erase-buffer)
      (insert-buffer-substring server)))
  (nndigest-possibly-change-buffer group)
  (let ((num 0))
    (save-excursion
      (set-buffer nndigest-current-buffer)
      (widen)
      (goto-char (point-min))
      (while (re-search-forward nndigest-separator nil t)
	(setq num (1+ num)))
      (set-buffer nntp-server-buffer)
      (erase-buffer)
      (insert (format "211 %d %d %d %s\n" num 1 num group))
      t)))

(defun nndigest-close-group (group &optional server)
  (nndigest-possibly-change-buffer group)
  (kill-buffer nndigest-current-buffer)
  (setq nndigest-group-alist (delq (assoc group nndigest-group-alist)
				   nndigest-group-alist))
  (setq nndigest-current-buffer nil)
  t)

(defun nndigest-request-list (&optional server)
  (save-excursion
    (set-buffer nntp-server-buffer)
    (erase-buffer)
    t))

(defun nndigest-request-newgroups (date &optional server)
  (save-excursion
    (set-buffer nntp-server-buffer)
    (erase-buffer)
    t))

(defun nndigest-request-list-newsgroups (&optional server)
  (save-excursion
    (set-buffer nntp-server-buffer)
    (erase-buffer)
    t))

(defun nndigest-request-post (&optional server)
  (mail-send-and-exit nil))

(fset 'nndigest-request-post-buffer 'nnmail-request-post-buffer)



;;; Internal functions

(defun nndigest-possibly-change-buffer (group)
  (and group
       (not (equal (cdr (assoc group nndigest-group-alist)) 
		   nndigest-current-buffer))
       (setq nndigest-current-buffer 
	     (cdr (assoc group nndigest-group-alist)))))

(defun nndigest-narrow-to-article (article) 
  (save-excursion
    (set-buffer nndigest-current-buffer)
    (widen)
    (goto-char (point-min))
    (while (and (not (zerop article))
		(re-search-forward nndigest-separator nil t))
      (setq article (1- article)))
    (if (zerop article)
	(progn
	  (goto-char (match-end 0))
	  (beginning-of-line)
	  (narrow-to-region 
	   (point)
	   (or (and (re-search-forward nndigest-separator nil t)
		    (match-beginning 0))
	       (point-max)))
	  (cons (point-min) (point-max)))
      nil)))
      

(provide 'nndigest)

;;; nndigest.el ends here
