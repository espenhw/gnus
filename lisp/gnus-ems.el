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
    (or (face-differs-from-default-p 'underline)
	(set-face-underline-p 'underline t))
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
      (let* ((table (window-display-table)))
	(and (not table)
	     (setq table (make-vector 261 ())))
	(let ((i 32))
	  (while (>= (setq i (1- i)) 0)
	    (aset table i [??])))
	(aset table ?\n nil)
	(let ((i 160))
	  (while (>= (setq i (1- i)) 127)
	    (aset table i [??])))
	(setq gnus-summary-display-table table)))

    )

   ))

(provide 'gnus-ems)

;; Local Variables:
;; byte-compile-warnings: nil
;; End:

;;; gnus-ems.el ends here
