;;; smiley-ems.el --- displaying smiley faces

;; Copyright (C) 2000, 2001 Free Software Foundation, Inc.

;; Author: Dave Love <fx@gnu.org>
;; Keywords: news mail multimedia

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

;; A re-written, simplified version of Wes Hardaker's XEmacs smiley.el
;; which might be merged back to smiley.el if we get an assignment for
;; that.  We don't have assignments for the images smiley.el uses, but
;; I'm not sure we need that degree of rococoness and defaults like a
;; yellow background.  Also, using PBM means we can display the images
;; more generally.  -- fx

;;; Test smileys:  :-) :-\ :-( :-/

;;; Code:

(eval-when-compile (require 'cl))
(require 'nnheader)

(defgroup smiley nil
  "Turn :-)'s into real images."
  :group 'gnus-visual)

;; Maybe this should go.
(defcustom smiley-data-directory (nnheader-find-etc-directory "smilies")
  "*Location of the smiley faces files."
  :type 'directory
  :group 'smiley)

;; The XEmacs version has a baroque, if not rococo, set of these.
(defcustom smiley-regexp-alist
  '(("\\(:-?)\\)\\W" 1 "smile")
    ("\\(;-?)\\)\\W" 1 "blink")
    ("\\(:-]\\)\\W" 1 "forced")
    ("\\(8-)\\)\\W" 1 "braindamaged")
    ("\\(:-|\\)\\W" 1 "indifferent")
    ("\\(:-[/\\]\\)\\W" 1 "wry")
    ("\\(:-(\\)\\W" 1 "sad")
    ("\\(:-{\\)\\W" 1 "frown"))
  "*A list of regexps to map smilies to images.
The elements are (REGEXP MATCH FILE), where MATCH is the submatch in
regexp to replace with IMAGE.  IMAGE is the name of a PBM file in
`smiley-data-directory'."
  :type '(repeat (list regexp
		       (integer :tag "Regexp match number")
		       (string :tag "Image name")))
  :set (lambda (symbol value)
	 (set-default symbol value)
	 (smiley-update-cache))
  :initialize 'custom-initialize-default
  :group 'smiley)

(defcustom gnus-smiley-file-types
  (let ((types (list "pbm")))
    (when (gnus-image-type-available-p 'xpm)
      (push "xpm" types))
    types)
  "*List of suffixes on picon file names to try."
  :type '(repeat string)
  :group 'smiley)

(defvar smiley-cached-regexp-alist nil)

(defun smiley-update-cache ()
  (dolist (elt (if (symbolp smiley-regexp-alist)
		   (symbol-value smiley-regexp-alist)
		 smiley-regexp-alist))
    (let ((types gnus-smiley-file-types)
	  file type)
      (while (and (not file)
		  (setq type (pop types)))
	(unless (file-exists-p
		 (setq file (expand-file-name (concat (nth 2 elt) "." type)
					      smiley-data-directory)))
	  (setq file nil)))
      (when type
	(let ((image (find-image (list (list :type (intern type) 
					     :file file
					     :ascent 'center)))))
	  (when image
	    (push (list (car elt) (cadr elt) image)
		  smiley-cached-regexp-alist)))))))

(defvar smiley-active nil
  "Non-nil means smilies in the buffer will be displayed.")
(make-variable-buffer-local 'smiley-active)

(defvar smiley-mouse-map
  (let ((map (make-sparse-keymap)))
    (define-key map [down-mouse-2] 'ignore) ; override widget
    (define-key map [mouse-2]
      'smiley-mouse-toggle-buffer)
    map))

;;;###autoload
(defun smiley-region (start end)
  "Replace in the region `smiley-regexp-alist' matches with corresponding images.
A list of images is returned."
  (interactive "r")
  (when (and (fboundp 'display-graphic-p)
	     (display-graphic-p))
    (mapcar (lambda (o)
	      (if (eq 'smiley (overlay-get o 'smiley))
		  (delete-overlay o)))
	    (overlays-in start end))
    (unless smiley-cached-regexp-alist
      (smiley-update-cache))
    (setq smiley-active t)
    (save-excursion
      (let ((beg (or start (point-min)))
	    group overlay image images)
	(dolist (entry smiley-cached-regexp-alist)
	  (setq group (nth 1 entry)
		image (nth 2 entry))
	  (goto-char beg)
	  (while (re-search-forward (car entry) end t)
	    (when image
	      (push image images)
	      (add-text-properties
	       (match-beginning group) (match-end group)
	       `(display ,image
			 mouse-face highlight
			 smiley t
			 help-echo "mouse-2: toggle smilies in buffer"
			 keymap smiley-mouse-map)))))
	images))))

(defun smiley-toggle-buffer (&optional arg)
  "Toggle displaying smiley faces.
With arg, turn displaying on if and only if arg is positive."
  (interactive "P")
  (if (numberp arg)
      (setq smiley-active (> arg 0))
    (setq smiley-active (not smiley-active))))

(defun smiley-mouse-toggle-buffer (event)
  "Toggle displaying smiley faces.
With arg, turn displaying on if and only if arg is positive."
  (interactive "e")
  (save-excursion
    (save-window-excursion
      (mouse-set-point event)
      (smiley-toggle-buffer))))

(eval-when-compile (defvar gnus-article-buffer))

(defun gnus-smiley-display (&optional arg)
  "Display textual emoticaons (\"smilies\") as small graphical icons.
With arg, turn displaying on if and only if arg is positive."
  (interactive "P")
  (gnus-with-article-buffer
    (if (memq 'smiley gnus-article-wash-types)
	(gnus-delete-images 'smiley)
      (article-goto-body)
      (let ((images (smiley-region (point) (point-max))))
	(when images
	  (gnus-add-wash-type 'smiley)
	  (dolist (image images)
	    (gnus-add-image 'smiley image))))
      (when (and (numberp arg)
		 (<= arg 0))
	(smiley-toggle-buffer arg)))))

(provide 'smiley)

;;; smiley-ems.el ends here
