;;; gnus-dup.el --- suppression of duplicate articles in Gnus
;; Copyright (C) 1996 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@ifi.uio.no>
;; Keywords: news

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This package tries to mark articles as read the second time the
;; user reads a copy.  This is useful if the server doesn't support
;; Xref properly, or if the user reads the same group from several
;; servers.

;;; Code:

(require 'gnus-load)
(require 'gnus-art)
(require 'gnus)

(defvar gnus-save-duplicate-list nil
  "*If non-nil, save the duplicate list when shutting down Gnus.
If nil, duplicate suppression will only work on duplicates
seen in the same session.")

(defvar gnus-duplicate-list-length 10000
  "*The number of Message-IDs to keep in the duplicate suppression list.")

(defvar gnus-duplicate-file (nnheader-concat gnus-directory "suppression")
  "*The name of the file to store the duplicate suppression list.")

;;; Internal variables

(defvar gnus-dup-list nil)
(defvar gnus-dup-hashtb nil)

;;;
;;; Starting and stopping
;;;

(gnus-add-shutdown 'gnus-dup-close 'gnus)

(defun gnus-dup-close ()
  "Possibly save the duplicate suppression list and shut down the subsystem."
  (when gnus-save-duplicate-list
    (gnus-dup-save))
  (setq gnus-dup-list nil
	gnus-dup-hashtb nil))

(defun gnus-dup-open ()
  "Possibly read the duplicate suppression list and start the subsystem."
  (if gnus-save-duplicate-list
      (gnus-dup-read)
    (setq gnus-dup-list nil))
  (setq gnus-dup-hashtb (gnus-make-hashtable gnus-duplicate-list-length))
  ;; Enter all Message-IDs into the hash table.
  (let ((list gnus-dup-list)
	(obarray gnus-dup-hashtb))
    (while list 
      (intern (pop list)))))

(defun gnus-dup-read ()
  "Read the duplicate suppression list."
  (setq gnus-dup-list nil)
  (when (file-exists-p gnus-duplicate-file)
    (load gnus-duplicate-file t t t)))

(defun gnus-dup-save ()
  "Save the duplicate suppression list."
  (nnheader-temp-write gnus-duplicate-file
    (gnus-prin1 `(setq gnus-duplicate-file ',gnus-duplicate-file))))

;;;
;;; Interface functions
;;;

(defun gnus-dup-enter-articles ()
  "Enter articles from the current group for future duplicate suppression."
  (unless gnus-dup-list
    (gnus-dup-open))
  (let ((data gnus-newsgroup-data)
	id)
    ;; Enter the Message-IDs of all read articles into the list
    ;; and hash table.
    (while data
      (when (gnus-data-read-p (car data))
	(intern (car (push (mail-header-id (gnus-data-header (car data)))
			   gnus-dup-list))
		gnus-dup-hashtb))
      (pop data))
    ;; Chop off excess Message-IDs from the list.
    (let ((end (nthcdr gnus-duplicate-list-length gnus-dup-list)))
      (when end 
	(setcdr end nil)))))

(defun gnus-dup-suppress-articles ()
  "Mark duplicate articles as read."
  (unless gnus-dup-list
    (gnus-dup-open))
  (gnus-message 6 "Suppressing duplicates...")
  (let ((headers gnus-newsgroup-headers)
	number)
    (while headers
      (when (intern-soft (mail-header-id (car headers)) gnus-dup-hashtb)
	(setq gnus-newsgroup-unreads 
	      (delq (setq number (mail-header-number (car headers)))
		    gnus-newsgroup-unreads))
	(push (cons number gnus-duplicate-mark)
	      gnus-newsgroup-reads))
      (pop headers)))
  (gnus-message 6 "Suppressing duplicates...done"))

(defun gnus-dup-unsuppress-article (article)
  "Stop suppression of ARTICLE."
  (let ((id (mail-header-id (gnus-data-header (gnus-data-find article)))))
    (when id
      (setq gnus-dup-list (delete id gnus-dup-list))
      (unintern id gnus-dup-hashtb))))

(provide 'gnus-dup)

;;; gnus-dup.el ends here
