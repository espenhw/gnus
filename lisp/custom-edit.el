;;; custom-edit.el --- Tools for customization Emacs.
;;
;; Copyright (C) 1996 Free Software Foundation, Inc.
;;
;; Author: Per Abrahamsen <abraham@dina.kvl.dk>
;; Keywords: help, faces
;; Version: 0.992
;; X-URL: http://www.dina.kvl.dk/~abraham/custom/

;;; Commentary:
;;
;; See `custom.el'.

;;; Code:

(require 'custom)
(require 'widget-edit)

(define-widget-keywords :custom-show :custom-magic
  :custom-state :custom-level :custom-form
  :custom-apply :custom-set-default :custom-reset)

;;; Utilities.

(defun custom-quote (sexp)
  "Quote SEXP iff it is not self quoting."
  (if (or (memq sexp '(t nil))
	  (and (symbolp sexp)
	       (eq (aref (symbol-name sexp) 0) ?:))
	  (and (listp sexp)
	       (memq (car sexp) '(lambda)))
	  (stringp sexp)
	  (numberp sexp))
      sexp
    (list 'quote sexp)))

;;; Modification of Basic Widgets.
;;
;; We add extra properties to the basic widgets needed here.  This is
;; fine, as long as we are careful to stay within out own namespace.
;;
;; We want simple widgets to be displayed by default, but complex
;; widgets to be hidden.

(widget-put (get 'item 'widget-type) :custom-show t)
(widget-put (get 'editable-field 'widget-type)
	    :custom-show (lambda (widget value)
			   (let ((pp (pp-to-string value)))
			     (cond ((string-match "\n" pp)
				    nil)
				   ((> (length pp) 40)
				    nil)
				   (t t)))))
(widget-put (get 'menu-choice 'widget-type) :custom-show t)

;;; The `custom-magic' Widget

(define-widget 'custom-magic 'item
  "Status feedback for customization option."
  :format "%[%v%]"
  :action 'widget-choice-item-action
  :value-create 'custom-magic-value-create)

(defface custom-invalid-face '((((class color))
				(:foreground "yellow" :background "red"))
			       (t
				(:bold t :italic t :underline t)))
  "Face used when the customize item is invalid."
  :group 'customize)

(defface custom-rogue-face '((((class color))
			      (:foreground "pink" :background "black"))
			     (t
			      (:underline t)))
  "Face used when the customize item is not defined for customization."
  :group 'customize)

(defface custom-modified-face '((((class color)) 
				 (:foreground "white" :background "blue"))
				(t
				 (:italic t :bold)))
  "Face used when the customize item has been modified."
  :group 'customize)

(defface custom-applied-face '((((class color)) 
				(:foreground "blue" :background "white"))
			       (t
				(:italic t)))
  "Face used when the customize item has been applied."
  :group 'customize)

(defface custom-saved-face '((t (:underline t)))
  "Face used when the customize item has been saved."
  :group 'customize)

(defcustom custom-magic-alist '((nil "#" underline)
				(unknown "?" italic)
				(hidden "-" default)
				(invalid "x" custom-invalid-face)
				(modified "*" custom-modified-face)
				(applied "+" custom-applied-face)
				(saved "!" custom-saved-face)
				(rogue "@" custom-rogue-face)
				(factory " " nil))
  "Alist of magic representing a customize items status.
Each entry is of the form (STATE MAGIC FACE), where 

STATE is one of the following symbols:

`nil'
   For internal use, should never occur.
`unknown'
   For internal use, should never occur.
`hidden'
   This item is not being displayed. 
`invalid'
   This item is modified, but has an invalid form.
`modified'
   This item is modified, and has a valid form.
`applied'
   This items current value has been changed temporarily.
`saved'
   This item is marked for saving.
`rogue'
   This item has no customization information.
`factory'
   This item is unchanged from the factory default.

MAGIC is a string used to present that state.

FACE is a face used to present the state.

The list should be sorted most significant first."
  :type '(repeat (list (choice (const nil)
			       (const unknown)
			       (const hidden)
			       (const invalid)
			       (const modified)
			       (const applied)
			       (const saved)
			       (const rogue)
			       (const factory))
		       string face))
  :group 'customize)

(defun custom-magic-value-create (widget)
  ;; Create compact status report for WIDGET.
  (let* ((parent (widget-get widget :parent))
	 (state (widget-get parent :custom-state))
	 (entry (assq state custom-magic-alist))
	 (magic (nth 1 entry))
	 (face (nth 2 entry)))
    (if (eq (widget-get parent :custom-form) 'lisp)
	(widget-insert "(" magic ")")
      (widget-insert "[" magic "]"))
    (widget-put widget :button-face face)))

(defun custom-magic-reset (widget)
  "Redraw the :custom-magic property of WIDGET."
  (let ((magic (widget-get widget :custom-magic)))
    (widget-value-set magic (widget-value magic))))

;;; The `custom-level' Widget.

(define-widget 'custom-level 'item
  "The custom level buttons."
  :format "%[%t%]"
  :help-echo "Push me to expand or collapse this item."
  :action 'custom-level-action)

(defun custom-level-action (widget &optional event)
  "Toggle visibility for parent to WIDGET."
  (let* ((parent (widget-get widget :parent))
	 (state (widget-get parent :custom-state)))
    (cond ((memq state '(invalid modified))
	   (error "There are unapplied changes"))
	  ((eq state 'hidden)
	   (widget-put parent :custom-state 'unknown))
	  (t
	   (widget-put parent :custom-state 'hidden)))
    (custom-redraw parent)))

;;; The `custom' Widget.

(defvar custom-save-needed-p nil
  "Non-nil if any customizations need to be saved.")

(add-hook 'kill-emacs-hook 'custom-save-maybe)

(defun custom-save-maybe ()
  (and custom-save-needed-p
       (y-or-n-p "You have unsaved customizations, save them now? ")
       (custom-save)))

(define-widget 'custom 'default
  "Customize a user option."
  :convert-widget 'widget-item-convert-widget
  :format "%l%[%t%]: %v%m %h"
  :format-handler 'custom-format-handler
  :notify 'custom-notify
  :custom-level 1
  :custom-state 'hidden
  :documentation-property 'widget-subclass-responsibility
  :value-create 'widget-subclass-responsibility
  :value-delete 'widget-radio-value-delete
  :value-get 'widget-item-value-get
  :validate 'widget-editable-list-validate
  :match (lambda (widget value) (symbolp value)))

(defun custom-format-handler (widget escape)
  ;; We recognize extra escape sequences.
  (let* ((buttons (widget-get widget :buttons))
	 (level (widget-get widget :custom-level)))
    (cond ((eq escape ?l)
	   (when level 
	     (push (widget-create-child-and-convert
		    widget 'custom-level (make-string level ?*))
		   buttons)
	     (widget-insert " ")
	     (widget-put widget :buttons buttons)))
	  ((eq escape ?m)
	   (and (eq (preceding-char) ?\n)
		(widget-get widget :indent)
		(insert-char ?  (widget-get widget :indent)))
	   (let ((magic (widget-create-child-and-convert
			 widget 'custom-magic nil)))
	     (widget-put widget :custom-magic magic)
	     (push magic buttons)
	     (widget-put widget :buttons buttons)))
	  (t 
	   (widget-default-format-handler widget escape)))))

(defun custom-notify (widget &rest args)
  "Keep track of changes."
  (widget-put widget :custom-state 'modified)
  (let ((buffer-undo-list t))
    (custom-magic-reset widget))
  (apply 'widget-default-notify widget args))

(defun custom-redraw (widget)
  "Redraw WIDGET with current settings."
  (widget-value-set widget (widget-value widget))
  (custom-redraw-magic widget))

(defun custom-redraw-magic (widget)
  "Redraw WIDGET state with current settings."
  (while widget 
    (let ((magic (widget-get widget :custom-magic)))
      (unless magic 
	(debug))
      (widget-value-set magic (widget-value magic))
      (when (setq widget (widget-get widget :group))
	(custom-group-state-update widget))))
  (widget-setup))

(defun custom-show (widget value)
  "Non-nil if WIDGET should be shown with VALUE by default."
  (let ((show (widget-get widget :custom-show)))
    (cond ((null show)
	   nil)
	  ((eq t show)
	   t)
	  (t
	   (funcall show widget value)))))

;;; The `custom-variable' Widget.

(define-widget 'custom-variable 'custom
  "Customize variable."
  :format "%l%v%m %h"
  :help-echo "Push me to set or reset this variable."
  :documentation-property 'variable-documentation
  :custom-state nil
  :custom-form 'edit
  :value-create 'custom-variable-value-create
  :action 'custom-variable-action
  :custom-apply 'custom-variable-apply
  :custom-set-default 'custom-variable-set-default
  :custom-reset 'custom-redraw)

(defun custom-variable-value-create (widget)
  "Here is where you edit the variables value."
  (let* ((buttons (widget-get widget :buttons))
	 (children (widget-get widget :children))
	 (form (widget-get widget :custom-form))
	 (state (widget-get widget :custom-state))
	 (symbol (widget-get widget :value))
	 (child-type (or (get symbol 'custom-type) 'sexp))
	 (type (if (listp child-type)
		   child-type
		 (list child-type)))
	 (conv (widget-convert type))
	 (value (if (boundp symbol)
		    (symbol-value symbol)
		  (widget-get conv :value))))
    ;; If the widget is new, the child determine whether it is hidden.
    (cond (state)
	  ((custom-show type value)
	   (setq state 'unknown))
	  (t
	   (setq state 'hidden)))
    ;; If we don't know the state, see if we need to edit it in lisp form.
    (when (eq state 'unknown)
      (unless (widget-apply (widget-convert type) :match value)
	(setq form 'lisp)))
    ;; Now we can create the child widget.
    (cond ((eq state 'hidden)
	   ;; Make hidden value easy to show.
	   (push (widget-create-child-and-convert
		  widget 'custom-level
		  :tag (symbol-name symbol)
		  :format "%t: %[show%]")
		 buttons))
	  ((eq form 'lisp)
	   ;; In lisp mode edit the saved value when possible.
	   (let* ((value (cond ((get symbol 'saved-value)
				(car (get symbol 'saved-value)))
			       ((get symbol 'factory-value)
				(car (get symbol 'factory-value)))
			       ((boundp symbol)
				(custom-quote (symbol-value symbol)))
			       (t
				(custom-quote (widget-get conv :value))))))
	     (push (widget-create-child-and-convert widget 'sexp 
						    :tag (symbol-name symbol)
						    :parent widget
						    :value value)
		   children)))
	  (t
	   ;; Edit mode.
	   (push (widget-create-child-and-convert widget type 
						  :tag (symbol-name symbol)
						  :value value)
		 children)))
    ;; Now update the state.
    (unless (eq (preceding-char) ?\n)
      (widget-insert "\n"))
    (if (eq state 'hidden)
	(widget-put widget :custom-state state)
      (custom-variable-state-set widget))
    (widget-put widget :custom-form form)	     
    (widget-put widget :buttons buttons)
    (widget-put widget :children children)))

(defun custom-variable-state-set (widget)
  "Set the state of WIDGET."
  (let* ((symbol (widget-value widget))
	 (value (symbol-value symbol))
	 (state (if (get symbol 'saved-value)
		    (if (condition-case nil
			    (equal value
				   (eval (car (get symbol 'saved-value))))
			  (error nil))
			'saved
		      'applied)
		  (if (get symbol 'factory-value)
		      (if (condition-case nil
			      (equal value
				     (eval (car (get symbol 'factory-value))))
			    (error nil))
			  'factory
			'applied)
		    'rogue))))
    (widget-put widget :custom-state state)))

(defvar custom-variable-menu 
  '(("Edit" . custom-variable-edit)
    ("Edit Default" . custom-variable-edit-lisp)
    ("Apply" . custom-variable-apply)
    ("Set Default" . custom-variable-set-default)
    ("Reset" . custom-redraw)
    ("Reset to Default" . custom-variable-default)
    ("Reset to Factory Settings" . custom-variable-factory))
  "Alist of actions for the `custom-variable' widget.
The key is a string containing the name of the action, the value is a
lisp function taking the widget as an element which will be called
when the action is chosen.")

(defun custom-variable-action (widget &optional event)
  "Show the menu for `custom-variable' WIDGET.
Optional EVENT is the location for the menu."
  (let* ((completion-ignore-case t)
	 (answer (widget-choose (symbol-name (widget-get widget :value))
				custom-variable-menu
				event)))
    (if answer
	(funcall answer widget))))

(defun custom-variable-edit (widget)
  "Edit value of WIDGET."
  (widget-put widget :custom-state 'unknown)
  (widget-put widget :custom-form 'edit)
  (custom-redraw widget))

(defun custom-variable-edit-lisp (widget)
  "Edit the lisp representation of the value of WIDGET."
  (widget-put widget :custom-state 'unknown)
  (widget-put widget :custom-form 'lisp)
  (custom-redraw widget))

(defun custom-variable-apply (widget)
  "Set the current value for the variable being edited by WIDGET."
  (let ((form (widget-get widget :custom-form))
	(state (widget-get widget :custom-state))
	(child (car (widget-get widget :children)))
	(symbol (widget-value widget))
	val)
    (cond ((eq state 'hidden)
	   (error "Cannot apply hidden variable."))
	  ((setq val (widget-apply child :validate))
	   (error "Invalid %S" val))
	  ((eq form 'lisp)
	   (set symbol (eval (widget-value child))))
	  (t
	   (set symbol (widget-value child))))
    (custom-variable-state-set widget)
    (custom-redraw-magic widget)))

(defun custom-variable-set-default (widget)
  "Set the default value for the variable being edited by WIDGET."
  (let ((form (widget-get widget :custom-form))
	(state (widget-get widget :custom-state))
	(child (car (widget-get widget :children)))
	(symbol (widget-value widget))
	val)
    (cond ((eq state 'hidden)
	   (error "Cannot apply hidden variable."))
	  ((setq val (widget-apply child :validate))
	   (error "Invalid %S" val))
	  ((eq form 'lisp)
	   (setq custom-save-needed-p (cons symbol custom-save-needed-p))
	   (put symbol 'saved-value (list (widget-value child)))
	   (set symbol (eval (widget-value child))))
	  (t
	   (setq custom-save-needed-p (cons symbol custom-save-needed-p))
	   (put symbol
		'saved-value (list (custom-quote (widget-value
						  child))))
	   (set symbol (widget-value child))))
    (custom-variable-state-set widget)
    (custom-redraw-magic widget)))

(defun custom-variable-default (widget)
  "Restore the default value for the variable being edited by WIDGET."
  (let ((symbol (widget-value widget)))
    (if (get symbol 'saved-value)
	(set symbol (car (get symbol 'saved-value)))
      (error "No default value for %s" symbol))
    (widget-put widget :custom-state 'unknown)
    (custom-redraw widget)))

(defun custom-variable-factory (widget)
  "Restore the factory setting for the variable being edited by WIDGET."
  (let ((symbol (widget-value widget)))
    (if (get symbol 'factory-value)
	(set symbol (eval (car (get symbol 'factory-value))))
      (error "No factory default for %S" symbol))
    (when (get symbol 'saved-value)
      (setq custom-save-needed-p (cons symbol custom-save-needed-p))
      (put symbol 'saved-value nil))
    (widget-put widget :custom-state 'unknown)
    (custom-redraw widget)))

;;; The `custom-face-edit' Widget.

(defvar custom-face-edit-args
  (mapcar (lambda (att)
	    (list 'group 
		  :inline t
		  (list 'const :format "" :value (nth 0 att)) 
		  (nth 1 att)))
	  custom-face-attributes))

(define-widget 'custom-face-edit 'checklist
  "Edit face attributes."
  :format "%t: %v"
  :tag "Attributes"
  :extra-offset 12
  :args (mapcar (lambda (att)
		  (list 'group 
			:inline t
			(list 'const :format "" :value (nth 0 att)) 
			(nth 1 att)))
		custom-face-attributes))

;;; The `custom-display' Widget.

(define-widget 'custom-display 'menu-choice
  "Select a display type."
  :tag "Display"
  :value t
  :args '((const :tag "all" t)
	  (checklist :offset 0
		     :extra-offset 9
		     :args ((group (const :format "Type: " type)
				   (checklist :inline t
					      :offset 0
					      (const :format "X "
						     x)
					      (const :format "TTY%n"
						     tty)))
			    (group (const :format "Class: " class)
				   (checklist :inline t
					      :offset 0
					      (const :format "Color "
						     color)
					      (const :format
						     "Grayscale "
						     grayscale)
					      (const :format "Monochrome%n"
						     mono)))
			    (group  (const :format "Background: " background)
				    (checklist :inline t
					       :offset 0
					       (const :format "Light "
						      light)
					       (const :format "Dark\n"
						      dark)))))))

;;; The `custom-face' Widget.

(define-widget 'custom-face 'custom
  "Customize face."
  :format "%l%[%t%]: %s%m %h%v"
  :format-handler 'custom-face-format-handler
  :help-echo "Push me to set or reset this face."
  :documentation-property 'face-documentation
  :value-create 'custom-face-value-create
  :action 'custom-face-action
  :custom-apply 'custom-face-apply
  :custom-set-default 'custom-face-set-default
  :custom-reset 'custom-redraw)

(defun custom-face-format-handler (widget escape)
  ;; We recognize extra escape sequences.
  (let* (child 
	 (symbol (widget-get widget :value)))
    (cond ((eq escape ?s)
	   (setq child (widget-create-child-and-convert 
			widget 'custom-level
			:format "(%[sample%])\n"
			:button-face symbol)))
	  (t 
	   (custom-format-handler widget escape)))
    (when child
      (widget-put widget
		  :buttons (cons child (widget-get widget :buttons))))))

(defun custom-face-value-create (widget)
  ;; Create a list of the display specifications.
  (unless (eq (preceding-char) ?\n)
    (insert "\n"))
  (when (not (eq (widget-get widget :custom-state) 'hidden))
    (let* ((symbol (widget-value widget))
	   (edit (widget-create-child-and-convert
		  widget 'editable-list
		  :entry-format "%i %d %v"
		  :value (or (get symbol 'saved-face)
			     (get symbol 'factory-face))
		  '(group :format "%v"
			  custom-display custom-face-edit))))
      (custom-face-state-set widget)
      (widget-put widget :children (list edit)))))

(defvar custom-face-menu 
  '(("Apply" . custom-face-apply)
    ("Set Default" . custom-face-set-default)
    ("Reset to Default" . custom-face-default)
    ("Reset to Factory Setting" . custom-face-factory))
  "Alist of actions for the `custom-face' widget.
The key is a string containing the name of the action, the value is a
lisp function taking the widget as an element which will be called
when the action is chosen.")

(defun custom-face-state-set (widget)
  "Set the state of WIDGET."
  (let ((symbol (widget-value widget)))
    (widget-put widget :custom-state (cond ((get symbol 'saved-face)
					    'saved)
					   ((get symbol 'factory-face)
					    'factory)
					   (t 
					    'rogue)))))

(defun custom-face-action (widget &optional event)
  "Show the menu for `custom-face' WIDGET.
Optional EVENT is the location for the menu."
  (when (eq (widget-get widget :custom-state) 'hidden)
    (error "You cannot edit a hidden face"))
  (let* ((completion-ignore-case t)
	 (symbol (widget-get widget :value))
	 (answer (widget-choose (symbol-name symbol) custom-face-menu event)))
    (if answer
	(funcall answer widget))
    (custom-face-state-set widget)
    (custom-redraw-magic widget)))

(defun custom-face-apply (widget)
  "Make the face attributes in WIDGET take effect."
  (let* ((symbol (widget-value widget))
	 (child (car (widget-get widget :children)))
	 (value (widget-value child)))
    (custom-face-display-set symbol value)))

(defun custom-face-set-default (widget)
  "Make the face attributes in WIDGET default."
  (let* ((symbol (widget-value widget))
	 (child (car (widget-get widget :children)))
	 (value (widget-value child)))
    (put symbol 'saved-face value)))

(defun custom-face-default (widget)
  "Restore WIDGET to the face's default attributes."
  (let* ((symbol (widget-value widget))
	 (child (car (widget-get widget :children))))
    (unless (get symbol 'saved-face)
      (error "No saved value for this face")
    (widget-value-set child (get symbol 'saved-face)))))

(defun custom-face-factory (widget)
  "Restore WIDGET to the face's factory settings."
  (let* ((symbol (widget-value widget))
	 (child (car (widget-get widget :children))))
    (unless (get symbol 'factory-face)
      (error "No factory default for this face"))
    (when (get symbol 'saved-face)
      (put symbol 'saved-face nil))
    (widget-value-set child (get symbol 'factory-face))))

;;; The `face' Widget.

(define-widget 'face 'default
  "Select and customize a face."
  :convert-widget 'widget-item-convert-widget
  :format "%[%t%]: %v"
  :tag "Face"
  :value 'default
  :value-create 'widget-face-value-create
  :value-delete 'widget-radio-value-delete
  :value-get 'widget-item-value-get
  :validate 'widget-editable-list-validate
  :action 'widget-face-action
  :match '(lambda (widget value) (symbolp value)))

(defun widget-face-value-create (widget)
  ;; Create a `custom-face' child.
  (let* ((symbol (widget-value widget))
	 (child (widget-create-child-and-convert
		 widget 'custom-face
		 :format "%t %s%m %h%v"
		 :custom-level nil
		 :value symbol)))
    (custom-magic-reset child)
    (widget-put widget :children (list child))))

(defvar face-history nil
  "History of entered face names.")

(defun widget-face-action (widget &optional event)
  "Prompt for a face."
  (let ((answer (completing-read "Face: "
				 (mapcar (lambda (face)
					   (list (symbol-name face)))
					 (face-list))
				 nil nil nil				 
				 'face-history)))
    (unless (zerop (length answer))
      (widget-value-set widget (intern answer))
      (widget-apply widget :notify widget event)
      (widget-setup))))

;;; The `custom-group' Widget.

(define-widget 'custom-group 'custom
  "Customize group."
  :format "%l%[%t%]:\n%m %h%v"
  :documentation-property 'group-documentation
  :help-echo "Push me to set or reset all members of this group."
  :value-create 'custom-group-value-create
  :action 'custom-group-action
  :custom-apply 'custom-group-apply
  :custom-set-default 'custom-group-set-default
  :custom-reset 'custom-group-reset)

(defun custom-group-value-create (widget)
  (let* ((state (widget-get widget :custom-state))
	 (level (widget-get widget :custom-level))
	 (symbol (widget-value widget))
	 (members (get symbol 'custom-group)))
    (unless (eq state 'hidden)
      (let* ((children (mapcar (lambda (entry)
				 (widget-insert "\n")
				 (prog1
				     (widget-create-child-and-convert
				      widget (nth 1 entry)
				      :group widget
				      :custom-level (1+ level)
				      :value (nth 0 entry))
				   (unless (eq (preceding-char) ?\n)
				     (widget-insert "\n"))))
			       members)))
	(mapcar 'custom-magic-reset children)
	(widget-put widget :children children)
	(custom-group-state-update widget)))))

(defvar custom-group-menu 
  '(("Apply" . custom-group-apply)
    ("Set Default" . custom-group-set-default)
    ("Reset" . custom-group-reset))
  "Alist of actions for the `custom-group' widget.
The key is a string containing the name of the action, the value is a
lisp function taking the widget as an element which will be called
when the action is chosen.")

(defun custom-group-action (widget &optional event)
  "Show the menu for `custom-group' WIDGET.
Optional EVENT is the location for the menu."
  (let* ((completion-ignore-case t)
	 (answer (widget-choose (symbol-name (widget-get widget :value))
				custom-group-menu
				event)))
    (if answer
	(funcall answer widget))))

(defun custom-group-apply (widget)
  "Apply changes in all modified group members."
  (let ((children (widget-get widget :children)))
    (mapcar (lambda (child)
	      (when (eq (widget-get child :custom-state) 'modified)
		(widget-apply child :custom-apply)))
	    children )))

(defun custom-group-set-default (widget)
  "Set default in all modified group members."
  (let ((children (widget-get widget :children)))
    (mapcar (lambda (child)
	      (when (eq (widget-get child :custom-state) 'modified)
		(widget-apply child :custom-set-default)))
	    children )))

(defun custom-group-reset (widget)
  "Reset all modified group members."
  (let ((children (widget-get widget :children)))
    (mapcar (lambda (child)
	      (when (eq (widget-get child :custom-state) 'modified)
		(widget-apply child :custom-reset)))
	    children )))

(defun custom-group-state-update (widget)
  "Update magic."
  (unless (eq (widget-get widget :custom-state) 'hidden)
    (let* ((children (widget-get widget :children))
	   (states (mapcar (lambda (child)
			     (widget-get child :custom-state))
			   children))
	   (magics custom-magic-alist)
	   (found 'factory))
      (while magics
	(let ((magic (car (car magics))))
	  (if (and (not (eq magic 'hidden))
		   (memq magic states))
	      (setq found magic
		    magics nil)
	    (setq magics (cdr magics)))))
      (widget-put widget :custom-state found)))
  (custom-magic-reset widget))

;;; The `custom-save' Command.

(defcustom custom-file "~/.emacs"
  "File used for storing customization information.
If you change this from the default \"~/.emacs\" you need to
explicitly load that file for the settings to take effect."
  :type 'file
  :group 'customize)

(defun custom-save-delete (symbol)
  "Delete the call to SYMBOL form `custom-file'.
Leave point at the location of the call, or after the last expression."
  (set-buffer (find-file-noselect custom-file))
  (goto-char (point-min))
  (catch 'found
    (while t
      (let ((sexp (condition-case nil
		      (read (current-buffer))
		    (end-of-file (throw 'found nil)))))
	(when (and (listp sexp)
		   (eq (car sexp) symbol))
	  (delete-region (save-excursion
			   (backward-sexp)
			   (point))
			 (point))
	  (throw 'found nil))))))

(defun custom-save-variables ()
  "Save all customized variables in `custom-file'."
  (save-excursion
    (custom-save-delete 'custom-set-variables)
    (let ((standard-output (current-buffer)))
      (unless (bolp)
	(princ "\n"))
      (princ "(custom-set-variables")
      (mapatoms (lambda (symbol)
		  (let ((value (get symbol 'saved-value)))
		    (when value
		      (princ "\n '")
		      (princ symbol)
		      (princ " '")
		      (prin1 (car value))))))
      (princ ")")
      (unless (eolp)
	(princ "\n")))))

(defun custom-save-faces ()
  "Save all customized faces in `custom-file'."
  (save-excursion
    (custom-save-delete 'custom-set-faces)
    (let ((standard-output (current-buffer)))
      (unless (bolp)
	(princ "\n"))
      (princ "(custom-set-faces")
      (mapatoms (lambda (symbol)
		  (let ((value (get symbol 'saved-face)))
		    (when value
		      (princ "\n '")
		      (princ symbol)
		      (princ " '")
		      (prin1 value)))))
      (princ ")")
      (unless (eolp)
	(princ "\n")))))

(defun custom-save ()
  "Save all customizations in `custom-file'."
  (interactive)
  (custom-save-variables)
  (custom-save-faces)
  (setq custom-save-needed-p nil)
  (save-excursion
    (set-buffer (find-file-noselect custom-file))
    (save-buffer)))

;;; The Custom Mode.

(defvar custom-options nil
  "Customization widgets in the current buffer.")

(defvar custom-mode-map nil
  "Keymap for `custom-mode'.")
  
(unless custom-mode-map
  (setq custom-mode-map (make-sparse-keymap))
  (set-keymap-parent custom-mode-map widget-keymap))

(easy-menu-define custom-mode-menu 
    custom-mode-map
  "Menu used in customization buffers."
    '("Custom"
      ["Apply" custom-apply t]
      ["Set Default" custom-set-default t]
      ["Reset" custom-reset t]
      ["Save" custom-save t]))

(defun custom-mode ()
  "Major mode for editing customization buffers.

The following commands are available:

\\[widget-forward]		Move to next button or editable field.
\\[widget-backward]		Move to previous button or editable field.
\\[widget-button-click]		Activate button under the mouse pointer.
\\[widget-button-press]		Activate button under point.
\\[custom-apply]		Apply all modifications.
\\[custom-set-default]		Make all modifications default.
\\[custom-reset]		Undo all modifications.
\\[custom-save]			Save defaults for future emacs sessions.

Entry to this mode calls the value of `custom-mode-hook'
if that value is non-nil."
  (kill-all-local-variables)
  (setq major-mode 'custom-mode
	mode-name "Custom")
  (use-local-map custom-mode-map)
  (make-local-variable 'custom-options)
  (run-hooks 'custom-mode-hook))

;;; Custom Mode Commands.

(defun custom-apply ()
  "Apply changes in all modified options."
  (interactive)
  (let ((children custom-options))
    (mapcar (lambda (child)
	      (when (eq (widget-get child :custom-state) 'modified)
		(widget-apply child :custom-apply)))
	    children)))

(defun custom-set-default ()
  "Set default in all modified group members."
  (interactive)
  (let ((children custom-options))
    (mapcar (lambda (child)
	      (when (eq (widget-get child :custom-state) 'modified)
		(widget-apply child :custom-set-default)))
	    children)))

(defun custom-reset ()
  "Reset all modified group members."
  (interactive)
  (let ((children custom-options))
    (mapcar (lambda (child)
	      (when (eq (widget-get child :custom-state) 'modified)
		(widget-apply child :custom-reset)))
	    children)))

;;; The Customize Commands

;;;###autoload
(defun customize (symbol)
  "Customize SYMBOL, which must be a customization group."
  (interactive (list (completing-read "Customize group: (default emacs) "
				      obarray 
				      (lambda (symbol)
					(get symbol 'custom-group))
				      t)))

  (when (stringp symbol)
    (if (string-equal "" symbol)
	(setq symbol 'emacs)
      (setq symbol (intern symbol))))
  (custom-buffer-create (list (list symbol 'custom-group))))

;;;###autoload
(defun customize-variable (symbol)
  "Customize SYMBOL, which must be a variable."
  (interactive
   ;; Code stolen from `help.el'.
   (let ((v (variable-at-point))
	 (enable-recursive-minibuffers t)
	 val)
     (setq val (completing-read 
		(if v
		    (format "Customize variable (default %s): " v)
		  "Customize variable: ")
		obarray 'boundp t))
     (list (if (equal val "")
	       v (intern val)))))
  (custom-buffer-create (list (list symbol 'custom-variable))))

;;;###autoload
(defun customize-face (symbol)
  "Customize FACE."
  (interactive (list (completing-read "Customize face: " obarray 'facep)))
  (unless (symbolp symbol)
    (error "Should be a symbol %S" symbol))
  (custom-buffer-create (list (list symbol 'custom-face))))

;;;###autoload
(defun customize-apropos (regexp &optional all)
  "Customize all user options matching REGEXP.
If ALL (e.g., started with a prefix key), include options which are not
user-settable."
  (interactive "sCustomize regexp: \nP")
  (let ((found nil))
    (mapatoms (lambda (symbol)
		(when (string-match regexp (symbol-name symbol))
		  (when (get symbol 'custom-group)
		    (setq found (cons (list symbol 'custom-group) found)))
		  (when (facep symbol)
		    (setq found (cons (list symbol 'custom-face) found)))
		  (when (and (boundp symbol)
			     (or (get symbol 'default-value)
				 (get symbol 'factory-value)
				 (if all
				     (get symbol 'variable-documentation)
				   (user-variable-p symbol))))
		    (setq found
			  (cons (list symbol 'custom-variable) found))))))
    (if found 
	(custom-buffer-create found)
      (error "No matches"))))

(defun custom-buffer-create (options)
  "Create a buffer containing OPTIONS.
OPTIONS should be an alist of the form ((SYMBOL WIDGET)...), where
SYMBOL is a customization option, and WIDGET is a widget for editing
that option."
  (kill-buffer (get-buffer-create "*Customization*"))
  (switch-to-buffer (get-buffer-create "*Customization*"))
  (custom-mode)
  (widget-insert "This is a customization buffer.
Push RET or click mouse-2 on the word ")
  (widget-create 'info-link 
		 :tag "help"
		 :help-echo "Push me for help."
		 "(custom)The Customization Buffer")
  (widget-insert " for more information.\n\n")
  (setq custom-options 
	(mapcar (lambda (entry)
		  (prog1 
		      (widget-create (nth 1 entry)
				     :value (nth 0 entry))
		    (unless (eq (preceding-char) ?\n)
		      (widget-insert "\n"))
		    (widget-insert "\n")))
		options))
  (widget-create 'push-button
		 :tag "Apply"
		 :help-echo "Push me to apply all modifications."
		 :action (lambda (widget &optional event)
			   (custom-apply)))
  (widget-insert " ")
  (widget-create 'push-button
		 :tag "Set Default"
		 :help-echo "Push me to make the modifications default."
		 :action (lambda (widget &optional event)
			   (custom-set-default)))
  (widget-insert " ")
  (widget-create 'push-button
		 :tag "Reset"
		 :help-echo "Push me to undo all modifications.."
		 :action (lambda (widget &optional event)
			   (custom-reset)))
  (widget-insert " ")
  (widget-create 'push-button
		 :tag "Save"
		 :help-echo "Push me to store the new defaults permanently."
		 :action (lambda (widget &optional event)
			   (custom-save)))
  (widget-insert "\n")
  (widget-setup))

;;; The End.

(provide 'custom-edit)

;; custom-edit.el ends here
