;;; ecomplete.el --- electric completion of addresses and the like
;; Copyright (C) 2006 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: mail

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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'cl))

(defgroup ecomplete nil
  "Suppression of duplicate articles."
  :group 'mail)

(defcustom ecomplete-database-file "~/.ecompleterc"
  "*The name of the file to store the ecomplete data."
  :group 'ecomplete
  :type 'file)

;;; Internal variables.

(defvar ecomplete-database nil)

(defun ecomplete-setup ()
  (when (file-exists-p ecomplete-database-file)
    (with-temp-buffer
      (insert-file-contents ecomplete-database-file)
      (setq ecomplete-database (read (current-buffer))))
    (save-excursion
      (loop for (type . elems) in ecomplete-database
	    do (let ((buffer (get-buffer-create
			      (format " *ecomplete %s*" type))))
		 (set-buffer buffer)
		 (erase-buffer)
		 (loop for (key count time text) in elems
		       do (insert text "\n")))))))

(defun ecomplete-add-item (type key text)
  (let ((elems (assq type ecomplete-database))
	(now (time-to-seconds (current-time)))
	entry)
    (unless elems
      (push (setq elems (list type)) ecomplete-database))
    (if (setq entry (assoc key (cdr elems)))
	(setcdr entry (list (1+ (cadr entry)) now text))
      (nconc elems (list (list key 1 now text))))))

(defun ecomplete-get-item (type key)
  (assoc key (cdr (assq type ecomplete-database))))

(defun ecomplete-save ()
  (with-temp-buffer
    (insert "(")
    (loop for (type . elems) in ecomplete-database
	  do
	  (insert (format "(%s\n" type))
	  (dolist (entry elems)
	    (prin1 entry (current-buffer))
	    (insert "\n"))
	  (insert ")\n"))
    (insert ")")
    (write-region (point-min) (point-max) ecomplete-database-file nil 'silent)))

(provide 'ecomplete)

;;; ecomplete.el ends here
