;;; mm-view.el --- Functions for viewing MIME objects
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

(require 'mail-parse)
(require 'mailcap)
(require 'mm-bodies)
(require 'mm-decode)

;;;
;;; Functions for displaying various formats inline
;;;

(defun mm-inline-image (handle)
  (let ((annot (make-annotation (mm-get-image handle) nil 'text))
	buffer-read-only)
    (mm-insert-inline handle ".\n")
    (set-extent-property annot 'mm t)
    (set-extent-property annot 'duplicable t)))

(defvar mm-w3-setup nil)
(defun mm-setup-w3 ()
  (unless mm-w3-setup
    (w3-do-setup)
    (require 'url)
    (require 'w3-vars)
    (load "url-misc.el")
    (setq mm-w3-setup t)))

(defun mm-inline-text (handle)
  (let ((type (cadr (split-string (car (mm-handle-type handle)) "/")))
	text buffer-read-only)
    (cond
     ((equal type "plain")
      (setq text (mm-get-part handle))
      (let ((b (point)))
	(insert text)
	(save-restriction
	  (narrow-to-region b (point))
	  (let ((charset (mail-content-type-get
			  (mm-handle-type handle) 'charset)))
	    (mm-decode-body charset nil))
	  (mm-handle-set-undisplayer
	   handle
	   `(lambda ()
	      (let (buffer-read-only)
		(delete-region
		 ,(set-marker (make-marker) (point-min))
		 ,(set-marker (make-marker) (point-max)))))))))
     ((equal type "html")
      (mm-setup-w3)
      (setq text (mm-get-part handle))
      (let ((b (point))
	    (width (window-width)))
	(save-excursion
	  (insert text)
	  (save-restriction
	    (narrow-to-region b (point))
	    (save-window-excursion
	      (let ((w3-strict-width width))
		(w3-region (point-min) (point-max)))))
	  (mm-handle-set-undisplayer
	   handle
	   `(lambda ()
	      (let (buffer-read-only)
		(delete-region
		 ,(set-marker (make-marker) (point-min))
		 ,(set-marker (make-marker) (point-max)))))))))
     ((or (equal type "enriched")
	  (equal type "richtext"))
      (save-excursion
	(mm-with-unibyte-buffer
	  (insert-buffer-substring (mm-handle-buffer handle))
	  (mm-decode-content-transfer-encoding
	   (mm-handle-encoding handle)
	   (car (mm-handle-type handle)))
	  (save-window-excursion
	    (enriched-decode (point-min) (point-max))
	    (setq text (buffer-string)))))
      (mm-insert-inline handle text))
     (t
      (save-excursion
	(mm-with-unibyte-buffer
	  (insert-buffer-substring (mm-handle-buffer handle))
	  (mm-decode-content-transfer-encoding
	   (mm-handle-encoding handle)
	   (car (mm-handle-type handle)))
	  (setq text (buffer-string))))
      (mm-insert-inline handle text)))))

(defun mm-insert-inline (handle text)
  "Insert TEXT inline from HANDLE."
  (let ((b (point)))
    (insert text)
    (mm-handle-set-undisplayer
     handle
     `(lambda ()
	(let (buffer-read-only)
	  (delete-region ,(set-marker (make-marker) b)
			 ,(set-marker (make-marker) (point))))))))
  
(defun mm-inline-audio (handle)
  (message "Not implemented"))

(defun mm-view-sound-file ()
  (message "Not implemented"))

(defun mm-w3-prepare-buffer ()
  (require 'w3)
  (w3-prepare-buffer))

(provide 'mm-view)

;; mm-view.el ends here
