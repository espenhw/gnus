;;; gnus-ems.el --- functions for making Gnus work under different Emacsen
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

(defvar gnus-mouse-2 [mouse-2])
(defvar gnus-easymenu 'easymenu)
(defvar gnus-group-mode-hook ())
(defvar gnus-summary-mode-hook ())
(defvar gnus-article-mode-hook ())

;; We do not byte-compile this file, because error messages are such a
;; bore.  

(eval
 '(cond 
   ((string-match "XEmacs\\|Lucid" emacs-version)
    ;; XEmacs definitions.

    (setq gnus-mouse-2 [button2])
    (setq gnus-easymenu 'auc-menu)

    (or (memq 'underline (list-faces))
	(funcall (intern "make-face") 'underline))
    ;; Must avoid calling set-face-underline-p directly, because it
    ;; is a defsubst in emacs19, and will make the .elc files non
    ;; portable!
    (or (face-differs-from-default-p 'underline)
  	(funcall 'set-face-underline-p 'underline t))
    (or (fboundp 'set-text-properties)
	(defun set-text-properties (start end props &optional buffer)
	  (if props
	      (put-text-property start end (car props) (cadr props) buffer)
	    (remove-text-properties start end ()))))
    
    (or (fboundp 'make-overlay) (fset 'make-overlay 'make-extent))
    (or (fboundp 'overlay-put) (fset 'overlay-put 'set-extent-property))
    (or (fboundp 'move-overlay) 
        (defun move-overlay (extent start end &optional buffer)
          (set-extent-endpoints extent start end)))
    (or (boundp 'standard-display-table) (setq standard-display-table nil))
    (or (boundp 'read-event) (fset 'read-event 'next-command-event))

    (if (not gnus-visual)
	()
      (setq gnus-group-mode-hook
	    (cons
	     (lambda ()
	       (easy-menu-add gnus-group-reading-menu)
	       (easy-menu-add gnus-group-group-menu)
	       (easy-menu-add gnus-group-post-menu)
	       (easy-menu-add gnus-group-misc-menu)
	       (gnus-install-mouse-tracker)) 
	     gnus-group-mode-hook))
      (setq gnus-summary-mode-hook
	    (cons
	     (lambda ()
	       (easy-menu-add gnus-summary-mark-menu)
	       (easy-menu-add gnus-summary-move-menu)
	       (easy-menu-add gnus-summary-article-menu)
	       (easy-menu-add gnus-summary-thread-menu)
	       (easy-menu-add gnus-summary-misc-menu)
	       (easy-menu-add gnus-summary-post-menu)
	       (easy-menu-add gnus-summary-kill-menu)
	       (gnus-install-mouse-tracker)) 
	     gnus-summary-mode-hook))
      (setq gnus-article-mode-hook
	    (cons
	     (lambda ()
	       (easy-menu-add gnus-article-article-menu)
	       (easy-menu-add gnus-article-treatment-menu))
	     gnus-article-mode-hook)))

    (defun gnus-install-mouse-tracker ()
      (require 'mode-motion)
      (setq mode-motion-hook 'mode-motion-highlight-line)))

   ((and (not (string-match "28.9" emacs-version)) 
	 (not (string-match "29" emacs-version)))
    (setq gnus-hidden-properties '(invisible t)))
   
   ((boundp 'MULE)
    (provide 'gnusutil))
   
   ))

(eval-and-compile
  (cond
   ((not window-system)
    (defun gnus-dummy-func (&rest args))
    (let ((funcs '(mouse-set-point set-face-foreground
				   set-face-background x-popup-menu)))
      (while funcs
	(or (fboundp (car funcs))
	    (fset (car funcs) 'gnus-dummy-func))
	(setq funcs (cdr funcs))))))
  (or (fboundp 'file-regular-p)
      (defun file-regular-p (file)
	(and (not (file-directory-p file))
	     (not (file-symlink-p file))
	     (file-exists-p file))))
  (or (fboundp 'face-list)
      (defun face-list (&rest args)))
  )

(defun gnus-ems-redefine ()
  (cond 
   ((string-match "XEmacs\\|Lucid" emacs-version)
    ;; XEmacs definitions.
    (fset 'gnus-set-mouse-face (lambda (string) string))

    (defun gnus-summary-make-display-table ()
      ;; We start from the standard display table, if any.
      (let* ((table (window-display-table)))
	(and (not table)
	     (setq table (make-vector 261 ())))
	;; Nix out all the control chars...
	(let ((i 32))
	  (while (>= (setq i (1- i)) 0)
	    (aset table i [??])))
	;; ... but not newline, of course.
	(aset table ?\n nil)
	;; We nix out any glyphs over 126 that are not set already.  
	(let ((i 256))
	  (while (>= (setq i (1- i)) 127)
	    (or (aref table i)
		(aset table i [??]))))
	(set-window-display-table (get-buffer-window (current-buffer)) table)))

    (defun gnus-highlight-selected-summary ()
      ;; Added by Per Abrahamsen <amanda@iesd.auc.dk>.
      ;; Highlight selected article in summary buffer
      (if gnus-summary-selected-face
	  (save-excursion
	    (let* ((beg (progn (beginning-of-line) (point)))
		   (end (progn (end-of-line) (point)))
		   (to (max 1 (1- (or (previous-single-property-change
				       end 'mouse-face nil beg) end))))
		   (from (1+ (or (next-single-property-change 
				  beg 'mouse-face nil end) beg))))
	      (if (< to beg)
		  (progn
		    (setq from beg)
		    (setq to end)))
	      (if gnus-newsgroup-selected-overlay
 		  (delete-extent gnus-newsgroup-selected-overlay))
 	      (setq gnus-newsgroup-selected-overlay
 		    (make-extent from to))
 	      (set-extent-face gnus-newsgroup-selected-overlay
 			       gnus-summary-selected-face)))))

    )
   ((boundp 'MULE)
    ;; Mule definitions
    (defun top-short-string (str width)
      "Return a substring of STRING, starting at top and its length is
equal or smaller than WIDTH. This function doesn't split in the middle
of multi-octet character. [tl-str]"
      (substring str 0
		 (let ((i 0) (w 0) chr (len (length str)))
		   (catch 'label
		     (while (< i len)
		       (setq chr (elt str i))
		       (setq w (+ w (char-width chr)))
		       (if (> w width)
			   (throw 'label i))
		       (setq i (+ i (char-bytes chr)))
		       )
		     i))
		 ))
    
    (defun gnus-format-max-width (form length)
      (let* ((val (eval form))
	     (valstr (if (numberp val) (int-to-string val) val)))
	(if (> (length valstr) length)
	    (top-short-string valstr length)
	  valstr)))
    
    (defun gnus-summary-make-display-table ())
    )
   ))

(provide 'gnus-ems)

;; Local Variables:
;; byte-compile-warnings: nil
;; End:

;;; gnus-ems.el ends here
