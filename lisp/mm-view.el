;;; mm-view.el --- Functions for viewing MIME objects
;; Copyright (C) 1998,99 Free Software Foundation, Inc.

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

(eval-and-compile
  (autoload 'gnus-article-prepare-display "gnus-art")
  (autoload 'vcard-parse-string "vcard")
  (autoload 'vcard-format-string "vcard"))

;;;
;;; Functions for displaying various formats inline
;;;

(defun mm-inline-image (handle)
  (let ((b (point))
	(annot (make-annotation (mm-get-image handle) nil 'text))
	buffer-read-only)
    (insert "\n")
    (mm-handle-set-undisplayer
     handle
     `(lambda ()
	(let (buffer-read-only)
	  (delete-annotation ,annot)
	  (delete-region ,(set-marker (make-marker) b)
			 ,(set-marker (make-marker) (point))))))
    (set-extent-property annot 'mm t)
    (set-extent-property annot 'duplicable t)))

(defvar mm-w3-setup nil)
(defun mm-setup-w3 ()
  (unless mm-w3-setup
    (require 'w3)
    (w3-do-setup)
    (require 'url)
    (require 'w3-vars)
    (require 'url-vars)
    (setq mm-w3-setup t)))

(defun mm-inline-text (handle)
  (let ((type (mm-handle-media-subtype handle))
	text buffer-read-only)
    (cond
     ((equal type "html")
      (mm-setup-w3)
      (setq text (mm-get-part handle))
      (let ((b (point))
	    (url-standalone-mode t)
	    (url-current-object
	     (url-generic-parse-url (format "cid:%s" (mm-handle-id handle))))
	    (width (window-width))
	    (charset (mail-content-type-get
		      (mm-handle-type handle) 'charset)))
	(save-excursion
	  (insert text)
	  (save-restriction
	    (narrow-to-region b (point))
	    (goto-char (point-min))
	    (if (or (and (boundp 'w3-meta-content-type-charset-regexp)
			 (re-search-forward
			  w3-meta-content-type-charset-regexp nil t))
		    (and (boundp 'w3-meta-charset-content-type-regexp)
			 (re-search-forward
			  w3-meta-charset-content-type-regexp nil t)))
		(setq charset (w3-coding-system-for-mime-charset 
			       (buffer-substring-no-properties 
				(match-beginning 2) 
				(match-end 2)))))
	    (delete-region (point-min) (point-max))
	    (insert (mm-decode-string text charset))
	    (save-window-excursion
	      (save-restriction
		(let ((w3-strict-width width)
		      (url-standalone-mode t))
		  (condition-case var
		      (w3-region (point-min) (point-max))
		    (error)))))
	    (mm-handle-set-undisplayer
	     handle
	     `(lambda ()
		(let (buffer-read-only)
		  (if (functionp 'remove-specifier)
		      (mapc (lambda (prop)
			      (remove-specifier
			       (face-property 'default prop) (current-buffer)))
			    '(background background-pixmap foreground)))
		  (delete-region ,(point-min-marker)
				 ,(point-max-marker)))))))))
     ((or (equal type "enriched")
	  (equal type "richtext"))
      (save-excursion
	(mm-with-unibyte-buffer
	  (mm-insert-part handle)
	  (save-window-excursion
	    (enriched-decode (point-min) (point-max))
	    (setq text (buffer-string)))))
      (mm-insert-inline handle text))
     ((equal type "x-vcard")
      (mm-insert-inline
       handle
       (concat "\n-- \n"
	       (vcard-format-string
		(vcard-parse-string (mm-get-part handle)
				    'vcard-standard-filter)))))
     (t
      (setq text (mm-get-part handle))
      (let ((b (point))
	    (charset (mail-content-type-get
		      (mm-handle-type handle) 'charset)))
	(insert (mm-decode-string text charset))
	(save-restriction
	  (narrow-to-region b (point))
	  (set-text-properties (point-min) (point-max) nil)
	  (mm-handle-set-undisplayer
	   handle
	   `(lambda ()
	      (let (buffer-read-only)
		(delete-region ,(point-min-marker)
			       ,(point-max-marker)))))))))))

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
  (let ((url-standalone-mode t))
    (w3-prepare-buffer)))

(defun mm-view-message ()
  (mm-enable-multibyte)
  (gnus-article-prepare-display)
  (run-hooks 'gnus-article-decode-hook)
  (fundamental-mode)
  (goto-char (point-min)))

(defun mm-inline-message (handle)
  (let ((b (point))
	gnus-displaying-mime handles)
    (save-excursion
      (save-restriction
	(narrow-to-region b b)
	(mm-insert-part handle)
	(let (gnus-article-mime-handles)
	  (run-hooks 'gnus-article-decode-hook)
	  (gnus-article-prepare-display)
	  (setq handles gnus-article-mime-handles))
	(when handles
	  (setq gnus-article-mime-handles
		(append gnus-article-mime-handles handles)))
	(mm-handle-set-undisplayer
	 handle
	 `(lambda ()
	    (let (buffer-read-only)
	      (ignore-errors
		;; This is only valid on XEmacs.
		(mapc (lambda (prop)
			(remove-specifier
			 (face-property 'default prop) (current-buffer)))
		      '(background background-pixmap foreground)))
	      (delete-region ,(point-min-marker) ,(point-max-marker)))))))))

(provide 'mm-view)

;; mm-view.el ends here
