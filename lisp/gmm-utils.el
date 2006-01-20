;;; gmm-utils.el --- Utility functions for Gnus, Message and MML

;; Copyright (C) 2006 Free Software Foundation, Inc.

;; Author: Reiner Steib <reiner.steib@gmx.de>
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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This library provides self-contained utility functions.  The functions are
;; used in Gnus, Message and MML, but within this library there are no
;; dependencies on Gnus, Message, or MML or Gnus.

;;; Code:

(defgroup gmm nil
  "Utility functions for Gnus, Message and MML"
  :prefix "gmm-"
  :group 'lisp)

;; Helper functions from `gnus-utils.el': gmm-verbose, gmm-message, gmm-error

(defcustom gmm-verbose 7
  "Integer that says how verbose gmm should be.
The higher the number, the more messages will flash to say what
it done.  At zero, it will be totally mute; at five, it will
display most important messages; and at ten, it will keep on
jabbering all the time."
  :type 'integer)

;;;###autoload
(defun gmm-message (level &rest args)
  "If LEVEL is lower than `gmm-verbose' print ARGS using `message'.

Guideline for numbers:
1 - error messages, 3 - non-serious error messages, 5 - messages for things
that take a long time, 7 - not very important messages on stuff, 9 - messages
inside loops."
  (if (<= level gmm-verbose)
      (apply 'message args)
    ;; We have to do this format thingy here even if the result isn't
    ;; shown - the return value has to be the same as the return value
    ;; from `message'.
    (apply 'format args)))

;;;###autoload
(defun gmm-error (level &rest args)
  "Beep an error if LEVEL is equal to or less than `gmm-verbose'."
  (when (<= (floor level) gmm-verbose)
    (apply 'message args)
    (ding)
    (let (duration)
      (when (and (floatp level)
		 (not (zerop (setq duration (* 10 (- level (floor level)))))))
	(sit-for duration))))
  nil)

;;;###autoload
(defun gmm-widget-p (symbol)
  "Non-nil iff SYMBOL is a widget."
  (get symbol 'widget-type))

;; Copy of the `nnmail-lazy' code from `nnmail.el':
(define-widget 'gmm-lazy 'default
  "Base widget for recursive datastructures.

This is copy of the `lazy' widget in Emacs 22.1 provided for compatibility."
  :format "%{%t%}: %v"
  :convert-widget 'widget-value-convert-widget
  :value-create (lambda (widget)
                  (let ((value (widget-get widget :value))
                        (type (widget-get widget :type)))
                    (widget-put widget :children
                                (list (widget-create-child-value
                                       widget (widget-convert type) value)))))
  :value-delete 'widget-children-value-delete
  :value-get (lambda (widget)
               (widget-value (car (widget-get widget :children))))
  :value-inline (lambda (widget)
                  (widget-apply (car (widget-get widget :children))
                                :value-inline))
  :default-get (lambda (widget)
                 (widget-default-get
                  (widget-convert (widget-get widget :type))))
  :match (lambda (widget value)
           (widget-apply (widget-convert (widget-get widget :type))
                         :match value))
  :validate (lambda (widget)
              (widget-apply (car (widget-get widget :children)) :validate)))

;; Note: The format of `gmm-tool-bar-item' may change if some future Emacs
;; version will provide customizable tool bar buttons using a different
;; interface.

;;;###autoload
(define-widget 'gmm-tool-bar-item (if (gmm-widget-p 'lazy) 'lazy 'gmm-lazy)
  "Tool bar list item."
  :tag "Tool bar item"
  :type '(list (function :tag "Menu Command")
	       (string   :tag "Icon file")
	       (choice (const :tag "Default map" nil)
		       ;; Note: Usually we need non-nil attributes if map is
		       ;; t.
		       (const :tag "No menu" t)
		       (sexp :tag "Other map"))
	       (plist :inline t :tag "Properties")))

(defvar tool-bar-map)

;;;###autoload
(defun gmm-tool-bar-from-list (icon-list zap-list default-map)
  "Make a tool bar from ICON-LIST.

Within each entry of ICON-LIST, the first element is a menu
command, the second element is an icon file name and the third
element is a test function.  You can use \\[describe-key]
<menu-entry> to find out the name of a menu command.  The fourth
and all following elements are passed a the PROPS argument to the
function `tool-bar-local-item'.

If ZAP-LIST is a list, remove those item from the default
`tool-bar-map'.  If it is t, start with a new sparse map.

DEFAULT-MAP specifies the default key map for ICON-LIST."
  (let (;; For Emacs 21, we must let-bind `tool-bar-map'.  In Emacs 22, we
	;; could use some other local variable.
	(tool-bar-map (if (eq zap-list t)
			  (make-sparse-keymap)
			(copy-keymap tool-bar-map))))
    (when (listp zap-list)
      ;; Zap some items which aren't relevant for this mode and take up space.
      (dolist (key zap-list)
	(define-key tool-bar-map (vector key) nil)))
    (mapc (lambda (el)
	    (let ((command (car el))
		  (icon (nth 1 el))
		  (fmap (or (nth 2 el) default-map))
		  (props  (cdr (cdr (cdr el)))) )
	      ;; command may stem from different from-maps:
	      (cond ((eq command 'ignore)
		     ;; FIXME: How to get no tool tip at all?
		     (if (fboundp 'tool-bar-local-item)
			 (apply 'tool-bar-local-item icon nil nil
				tool-bar-map props)
		       ;; (tool-bar-local-item ICON DEF KEY MAP &rest PROPS)
		       ;; (tool-bar-add-item ICON DEF KEY &rest PROPS)
		       (apply 'tool-bar-add-item icon nil nil props)))
		    ((equal fmap t) ;; Not a menu command
		     (if (fboundp 'tool-bar-local-item)
			 (apply 'tool-bar-local-item
				icon command
				(intern icon) ;; reuse icon or fmap here?
				tool-bar-map props)
		       ;; Emacs 21 compatibility:
		       (apply 'tool-bar-add-item
			      icon command
			      (intern icon)
			      props)))
		    (t ;; A menu command
		     (if (fboundp 'tool-bar-local-item-from-menu)
			 (apply 'tool-bar-local-item-from-menu
				;; (apply 'tool-bar-local-item icon def key
				;; tool-bar-map props)
				command icon tool-bar-map (symbol-value fmap)
				props)
		       ;; Emacs 21 compatibility:
		       (apply 'tool-bar-add-item-from-menu
			      command icon (symbol-value fmap)
			      props))))
	      t))
	  (if (symbolp icon-list)
	      (eval icon-list)
	    icon-list))
    tool-bar-map))

(provide 'gmm-utils)

;;; gmm-utils.el ends here
