;;; qp.el --- Quoted-printable functions
;; Copyright (C) 1998 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
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

;;; Code:

(defvar quoted-printable-encoding-characters
  (mapcar 'identity "0123456789ABCDEF"))

(defun quoted-printable-decode-region (from to)
  "Decode quoted-printable in the region between FROM and TO."
  (interactive "r")
  (save-excursion
    (goto-char from)
    (while (search-forward "=" to t)
      (cond ((eq (following-char) ?\n)
	     (delete-char -1)
	     (delete-char 1))
	    ((and
	      (memq (following-char) quoted-printable-encoding-characters)
	      (memq (char-after (1+ (point)))
		    quoted-printable-encoding-characters))
	     (subst-char-in-region
	      (1- (point)) (point) ?=
	      (string-to-number
	       (buffer-substring (point) (+ 2 (point)))
	       16))
	     (delete-char 2))
	    ((looking-at "=")
	     (delete-char 1))
	    ((message "Malformed MIME quoted-printable message"))))))

(defun quoted-printable-decode-string (string)
 "Decode the quoted-printable-encoded STRING and return the results."
 (with-temp-buffer
   (insert string)
   (quoted-printable-decode-region (point-min) (point-max))
   (buffer-string)))

(defun quoted-printable-encode-region (from to &optional fold)
  "QP-encode the region between FROM and TO.
If FOLD, fold long lines."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region from to)
      (goto-char (point-min))
      (while (re-search-forward "[\000-\007\013\015-\037\200-\377_=]" nil t)
	(insert
	 (prog1
	     (upcase (format "=%x" (char-after (1- (point)))))
	   (delete-char -1))))
      (when fold
	;; Fold long lines.
	(goto-char (point-min))
	(end-of-line)
	(while (> (current-column) 72)
	  (beginning-of-line)
	  (forward-char 72)
	  (search-backward "=" (- (point) 2) t)
	  (insert "=\n")
	  (end-of-line))))))

(defun quoted-printable-encode-string (string)
 "QP-encode STRING and return the results."
 (with-temp-buffer
   (insert string)
   (quoted-printable-encode-region (point-min) (point-max))
   (buffer-string)))

(provide 'qp)

;; qp.el ends here
