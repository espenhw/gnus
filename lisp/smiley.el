;;; smiley.el --- displaying smiley faces

;; Copyright (C) 1996, 1997, 1998, 1999, 2000, 2001
;;        Free Software Foundation, Inc.

;; Author: Wes Hardaker <hardaker@ece.ucdavis.edu>
;; Keywords: fun

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

;;
;; comments go here.
;;

;;; Test smileys:  :-] :-o :-) ;-) :-\ :-| :-d :-P 8-| :-(

;; To use:
;; (require 'smiley)
;; (setq gnus-treat-display-smileys t)

;; The smilies were drawn by Joe Reiss <jreiss@vt.edu>.

;;; Code:

(require 'cl)
(require 'custom)

(eval-and-compile
  (when (featurep 'xemacs)
    (require 'annotations)
    (require 'messagexmas)))

(defgroup smiley nil
  "Turn :-)'s into real images."
  :group 'gnus-visual)

;; FIXME: Where is the directory when using Emacs?
(defcustom smiley-data-directory
  (if (featurep 'xemacs)
    (message-xmas-find-glyph-directory "smilies")
    "/usr/local/lib/xemacs/xemacs-packages/etc/smilies")
  "*Location of the smiley faces files."
  :type 'directory
  :group 'smiley)

;; Notice the subtle differences in the regular expressions in the
;; two alists below.

(defcustom smiley-deformed-regexp-alist
  '(("\\(\\^_\\^;;;\\)\\W" 1 "WideFaceAse3.xbm")
    ("\\(\\^_\\^;;\\)\\W" 1 "WideFaceAse2.xbm")
    ("\\(\\^_\\^;\\)\\W" 1 "WideFaceAse1.xbm")
    ("\\(\\^_\\^\\)\\W" 1 "WideFaceSmile.xbm")
    ("\\(;_;\\)\\W" 1 "WideFaceWeep.xbm")
    ("\\(T_T\\)\\W" 1 "WideFaceWeep.xbm")
    ("\\(:-*[<«]+\\)\\W" 1 "FaceAngry.xpm")
    ("\\(:-+\\]+\\)\\W" 1 "FaceGoofy.xpm")
    ("\\(:-*D\\)\\W" 1 "FaceGrinning.xpm")
    ("\\(:-*[)>}»]+\\)\\W" 1 "FaceHappy.xpm")
    ("\\(=[)»]+\\)\\W" 1 "FaceHappy.xpm")
    ("\\(:-*[/\\\"]\\)[^/]\\W" 1 "FaceIronic.xpm")
    ("[^.0-9]\\([8|]-*[|Oo%]\\)\\W" 1 "FaceKOed.xpm")
    ("\\([:|]-*#+\\)\\W" 1 "FaceNyah.xpm")
    ("\\(:-*[({]+\\)\\W" 1 "FaceSad.xpm")
    ("\\(=[({]+\\)\\W" 1 "FaceSad.xpm")
    ("\\(:-*[Oo\*]\\)\\W" 1 "FaceStartled.xpm")
    ("\\(:-*|\\)\\W" 1 "FaceStraight.xpm")
    ("\\(:-*p\\)\\W" 1 "FaceTalking.xpm")
    ("\\(:-*d\\)\\W" 1 "FaceTasty.xpm")
    ("\\(;-*[>)}»]+\\)\\W" 1 "FaceWinking.xpm")
    ("\\(:-*[Vvµ]\\)\\W" 1 "FaceWry.xpm")
    ("\\([:|]-*P\\)\\W" 1 "FaceYukky.xpm"))
  "*Normal and deformed faces for smilies."
  :type '(repeat (list regexp
		       (integer :tag "Match")
		       (string :tag "Image")))
  :group 'smiley)

(defcustom smiley-nosey-regexp-alist
  '(("\\(:-+[<«]+\\)\\W" 1 "FaceAngry.xpm")
    ("\\(:-+\\]+\\)\\W" 1 "FaceGoofy.xpm")
    ("\\(:-+D\\)\\W" 1 "FaceGrinning.xpm")
    ("\\(:-+[}»]+\\)\\W" 1 "FaceHappy.xpm")
    ("\\(:-*)+\\)\\W" 1 "FaceHappy.xpm")
    ("\\(=[)]+\\)\\W" 1 "FaceHappy.xpm")
    ("\\(:-+[/\\\"]+\\)\\W" 1 "FaceIronic.xpm")
    ("\\([8|]-+[|Oo%]\\)\\W" 1 "FaceKOed.xpm")
    ("\\([:|]-+#+\\)\\W" 1 "FaceNyah.xpm")
    ("\\(:-+[({]+\\)\\W" 1 "FaceSad.xpm")
    ("\\(=[({]+\\)\\W" 1 "FaceSad.xpm")
    ("\\(:-+[Oo\*]\\)\\W" 1 "FaceStartled.xpm")
    ("\\(:-+|\\)\\W" 1 "FaceStraight.xpm")
    ("\\(:-+p\\)\\W" 1 "FaceTalking.xpm")
    ("\\(:-+d\\)\\W" 1 "FaceTasty.xpm")
    ("\\(;-+[>)}»]+\\)\\W" 1 "FaceWinking.xpm")
    ("\\(:-+[Vvµ]\\)\\W" 1 "FaceWry.xpm")
    ("\\(][:8B]-[)>]\\)\\W" 1 "FaceDevilish.xpm")
    ("\\([:|]-+P\\)\\W" 1 "FaceYukky.xpm"))
  "*Smileys with noses.  These get less false matches."
  :type '(repeat (list regexp
		       (integer :tag "Match")
		       (string :tag "Image")))
  :group 'smiley)

(defcustom smiley-regexp-alist smiley-deformed-regexp-alist
  "*A list of regexps to map smilies to real images.
Defaults to the contents of `smiley-deformed-regexp-alist'.
An alternative is `smiley-nosey-regexp-alist' that matches less
aggressively.
If this is a symbol, take its value."
  :type '(radio (variable-item smiley-deformed-regexp-alist)
		(variable-item smiley-nosey-regexp-alist)
		symbol
		(repeat (list regexp
			      (integer :tag "Match")
			      (string :tag "Image"))))
  :group 'smiley)

(defcustom smiley-flesh-color "yellow"
  "*Flesh color."
  :type 'string
  :group 'smiley)

(defcustom smiley-features-color "black"
  "*Features color."
  :type 'string
  :group 'smiley)

(defcustom smiley-tongue-color "red"
  "*Tongue color."
  :type 'string
  :group 'smiley)

(defcustom smiley-circle-color "black"
  "*Circle color."
  :type 'string
  :group 'smiley)

(defcustom smiley-mouse-face 'highlight
  "*Face used for mouse highlighting in the smiley buffer.

Smiley buttons will be displayed in this face when the cursor is
above them."
  :type 'face
  :group 'smiley)

(defvar smiley-glyph-cache nil)

(defvar smiley-map (make-sparse-keymap "smiley-keys")
  "Keymap to toggle smiley states.")

(define-key smiley-map [(button2)] 'smiley-toggle-extent)
(define-key smiley-map [(button3)] 'smiley-popup-menu)

(defun smiley-popup-menu (e)
  (interactive "e")
  (popup-menu
   `("Smilies"
     ["Toggle This Smiley" (smiley-toggle-extent ,e) t]
     ["Toggle All Smilies" (smiley-toggle-extents ,e) t])))

(defun smiley-create-glyph (smiley pixmap)
  (or
   (cdr-safe (assoc pixmap smiley-glyph-cache))
   (let* ((xpm-color-symbols
	   (and (featurep 'xpm)
		(append `(("flesh" ,smiley-flesh-color)
			  ("features" ,smiley-features-color)
			  ("tongue" ,smiley-tongue-color))
			xpm-color-symbols)))
	  (glyph (make-glyph
		  (list
		   (cons (if (featurep 'gtk) 'gtk 'x)
			 (expand-file-name pixmap smiley-data-directory))
		   (cons 'mswindows
			 (expand-file-name pixmap smiley-data-directory))
		   (cons 'tty smiley)))))
     (setq smiley-glyph-cache (cons (cons pixmap glyph) smiley-glyph-cache))
     (set-glyph-face glyph 'default)
     glyph)))

(defun smiley-create-glyph-ems (smiley pixmap)
  (condition-case e
      (create-image (expand-file-name pixmap smiley-data-directory))
    (error nil)))


;;;###autoload
(defun smiley-region (beg end)
  "Smilify the region between point and mark."
  (interactive "r")
  (smiley-buffer (current-buffer) beg end))

(defun smiley-toggle-extent (event)
  "Toggle smiley at given point."
  (interactive "e")
  (let* ((ant (event-glyph-extent event))
	 (pt (event-closest-point event))
	 ext)
    (if (annotationp ant)
	(when (extentp (setq ext (extent-property ant 'smiley-extent)))
	  (set-extent-property ext 'invisible nil)
	  (hide-annotation ant))
      (when pt
	(while (setq ext (extent-at pt (event-buffer event) nil ext 'at))
	  (when (annotationp (setq ant
				   (extent-property ext 'smiley-annotation)))
	    (reveal-annotation ant)
	    (set-extent-property ext 'invisible t)))))))

;; FIXME::
(defun smiley-toggle-extent-ems (event)
  "Toggle smiley at given point.
Note -- this function hasn't been implemented yet."
  (interactive "e")
  (error "This function hasn't been implemented yet"))

(defun smiley-toggle-extents (e)
  (interactive "e")
  (map-extents
   (lambda (e void)
     (let (ant)
       (if (annotationp (setq ant (extent-property e 'smiley-annotation)))
	   (if (eq (extent-property e 'invisible) nil)
	       (progn
		 (reveal-annotation ant)
		 (set-extent-property e 'invisible t)
		 )
	     (hide-annotation ant)
	     (set-extent-property e 'invisible nil)))
       nil))
   (event-buffer e)))

;; FIXME::
(defun smiley-toggle-extents-ems (e)
  (interactive "e")
  (error "This function hasn't been implemented yet"))

;;;###autoload
(defun smiley-buffer (&optional buffer st nd)
  (interactive)
  (when (featurep '(or x gtk mswindows))
    (save-excursion
      (when buffer
	(set-buffer buffer))
      (let ((buffer-read-only nil)
	    (alist (if (symbolp smiley-regexp-alist)
		       (symbol-value smiley-regexp-alist)
		     smiley-regexp-alist))
	    (case-fold-search nil)
	    entry regexp beg group file)
	(map-extents
	 (lambda (e void)
	   (when (or (extent-property e 'smiley-extent)
		     (extent-property e 'smiley-annotation))
	     (delete-extent e)))
	 buffer st nd)
	(goto-char (or st (point-min)))
	(setq beg (point))
	;; loop through alist
	(while (setq entry (pop alist))
	  (setq regexp (car entry)
		group (cadr entry)
		file (caddr entry))
	  (goto-char beg)
	  (while (re-search-forward regexp nd t)
	    (let* ((start (match-beginning group))
		   (end (match-end group))
		   (glyph (smiley-create-glyph (buffer-substring start end)
					       file)))
	      (when glyph
		(mapcar 'delete-annotation (annotations-at end))
		(let ((ext (make-extent start end))
		      (ant (make-annotation glyph end 'text)))
		  ;; set text extent params
		  (set-extent-property ext 'end-open t)
		  (set-extent-property ext 'start-open t)
		  (set-extent-property ext 'invisible t)
		  (set-extent-property ext 'keymap smiley-map)
		  (set-extent-property ext 'mouse-face smiley-mouse-face)
		  (set-extent-property ext 'intangible t)
		  ;; set annotation params
		  (set-extent-property ant 'mouse-face smiley-mouse-face)
		  (set-extent-property ant 'keymap smiley-map)
		  ;; remember each other
		  (set-extent-property ant 'smiley-extent ext)
		  (set-extent-property ext 'smiley-annotation ant)
		  ;; Help
		  (set-extent-property
		   ext 'help-echo
		   "button2 toggles smiley, button3 pops up menu")
		  (set-extent-property
		   ant 'help-echo
		   "button2 toggles smiley, button3 pops up menu")
		  (set-extent-property ext 'balloon-help
				       "Mouse button2 - toggle smiley
Mouse button3 - menu")
		  (set-extent-property ant 'balloon-help
				       "Mouse button2 - toggle smiley
Mouse button3 - menu"))
		(when (smiley-end-paren-p start end)
		  (make-annotation ")" end 'text))
		(goto-char end)))))))))

;; FIXME: No popup menu, no customized color
(defun smiley-buffer-ems (&optional buffer st nd)
  (interactive)
  (when window-system
    (save-excursion
      (when buffer
	(set-buffer buffer))
      (let ((buffer-read-only nil)
	    (alist (if (symbolp smiley-regexp-alist)
		       (symbol-value smiley-regexp-alist)
		     smiley-regexp-alist))
	    (case-fold-search nil)
	    entry regexp beg group file)
	(dolist (overlay (overlays-in (or st (point-min))
				      (or nd (point-max))))
	  (when (overlay-get overlay 'smiley)
	    (remove-text-properties (overlay-start overlay)
				    (overlay-end overlay) '(display))
	    (delete-overlay overlay)))
	(goto-char (or st (point-min)))
	(setq beg (point))
	;; loop through alist
	(while (setq entry (pop alist))
	  (setq regexp (car entry)
		group (cadr entry)
		file (caddr entry))
	  (goto-char beg)
	  (while (re-search-forward regexp nd t)
	    (let* ((start (match-beginning group))
		   (end (match-end group))
		   (glyph (smiley-create-glyph nil file))
		   (overlay (make-overlay start end)))
	      (when glyph
		(add-text-properties start end
				     `(display ,glyph))
		(overlay-put overlay 'smiley glyph)
		(goto-char end)))))))))

(defun smiley-end-paren-p (start end)
  "Try to guess whether the current smiley is an end-paren smiley."
  (save-excursion
    (goto-char start)
    (when (and (re-search-backward "[()]" nil t)
	       (eq (char-after) ?\()
	       (goto-char end)
	       (or (not (re-search-forward "[()]" nil t))
		   (eq (char-after (1- (point))) ?\()))
      t)))

(defun smiley-toggle-buffer (&optional arg buffer st nd)
  "Toggle displaying smiley faces.
With arg, turn displaying on if and only if arg is positive."
  (interactive "P")
  (let (on off)
    (map-extents
     (lambda (e void)
       (let (ant)
	 (if (annotationp (setq ant (extent-property e 'smiley-annotation)))
	     (if (eq (extent-property e 'invisible) nil)
		 (setq off (cons (cons ant e) off))
	       (setq on (cons (cons ant e) on)))))
       nil)
     buffer st nd)
    (if (and (not (and (numberp arg) (< arg 0)))
	     (or (and (numberp arg) (> arg 0))
		 (null on)))
	(if off
	    (while off
	      (reveal-annotation (caar off))
	      (set-extent-property (cdar off) 'invisible t)
	      (setq off (cdr off)))
	  (smiley-buffer))
      (while on
	(hide-annotation (caar on))
	(set-extent-property (cdar on) 'invisible nil)
	(setq on (cdr on))))))

;; Simply removing all smiley if existing.
;; FIXME: make it work as the one in XEmacs.
(defun smiley-toggle-buffer-ems (&optional arg buffer st nd)
  "Toggle displaying smiley faces.
With arg, turn displaying on if and only if arg is positive."
  (interactive "P")
  (save-excursion
    (when buffer
      (set-buffer buffer))
    (let (found)
      (dolist (overlay (overlays-in (or st (point-min))
				    (or nd (point-max))))
	(when (overlay-get overlay 'smiley)
	  (remove-text-properties (overlay-start overlay)
				  (overlay-end overlay) '(display))
	  (setq found t)))
      (unless found
	(smiley-buffer buffer st nd)))))

(unless (featurep 'xemacs)
  (defalias 'smiley-create-glyph 'smiley-create-glyph-ems)
  (defalias 'smiley-toggle-extent 'smiley-toggle-extent-ems)
  (defalias 'smiley-toggle-extents 'smiley-toggle-extents-ems)
  (defalias 'smiley-buffer 'smiley-buffer-ems)
  (defalias 'smiley-toggle-buffer 'smiley-toggle-buffer-ems))

(defvar gnus-article-buffer)
;;;###autoload
(defun gnus-smiley-display (&optional arg)
  "Display \"smileys\" as small graphical icons.
With arg, turn displaying on if and only if arg is positive."
  (interactive "P")
  (save-excursion
    (article-goto-body)
    (let (buffer-read-only)
      (smiley-toggle-buffer arg (current-buffer) (point) (point-max)))))

(provide 'smiley)

;; Local Variables:
;; coding: iso-8859-1
;; End:

;;; smiley.el ends here
