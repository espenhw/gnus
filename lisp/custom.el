;;; custom.el -- Tools for declaring and initializing options.
;;
;; Copyright (C) 1996 Free Software Foundation, Inc.
;;
;; Author: Per Abrahamsen <abraham@dina.kvl.dk>
;; Keywords: help, faces
;; Version: 0.991
;; X-URL: http://www.dina.kvl.dk/~abraham/custom/

;;; Commentary:
;;
;; If you want to use this code, please visit the URL above.
;;
;; This file only contain the code needed to declare and initialize
;; user options.  The code to customize options is autoloaded from
;; `custom-edit.el'. 

;;; Code:

(require 'widget)

(define-widget-keywords :type :group)

;; These autoloads should be deleted when the file is added to Emacs
(autoload 'customize "custom-edit" nil t)
(autoload 'customize-variable "custom-edit" nil t)
(autoload 'customize-face "custom-edit" nil t)
(autoload 'customize-apropos "custom-edit" nil t)

;;; Compatibility.

(fset 'custom-x-color-values 
      (if (fboundp 'x-color-values)
	  'x-color-values
	(lambda (color)
	  (color-instance-rgb-components
	   (make-color-instance color)))))

(defun custom-background-mode ()
  "Kludge to detext background mode."
  (let* ((bg-resource 
	  (condition-case ()
	      (x-get-resource ".backgroundMode" "BackgroundMode" 'string)
	    (error nil)))
	 (params (frame-parameters))
	 (color (condition-case ()
		    (or (assq 'background-color params)
			(color-instance-name
			 (specifier-instance
			  (face-background 'default))))
		  (error nil))))
    (cond (bg-resource (intern (downcase bg-resource)))
	  ((and color
		(< (apply '+ (custom-x-color-values color))
		   (/ (apply '+ (custom-x-color-values "white")) 3)))
	   'dark)
	  (t 'light))))

;;; The `defcustom' Macro.

;;;###autoload
(defun custom-declare-variable (symbol value doc &rest args)
  "Like `defcustom', but SYMBOL and VALUE are evaluated as notmal arguments."
  (unless (default-boundp symbol)
    (set-default symbol (eval value)))
  (put symbol 'factory-value (list value))
  (when doc
    (put symbol 'variable-documentation doc))
  (while args 
    (let ((arg (car args)))
      (setq args (cdr args))
      (unless (symbolp arg)
	(error "Junk in args %S" args))
      (let ((keyword arg)
	    (value (car args)))
	(unless args
	  (error "Keyword %s is missing an argument" keyword))
	(setq args (cdr args))
	(cond ((eq keyword :type)
	       (put symbol 'custom-type value))
	      ((eq keyword :group)
	       (custom-add-to-group value symbol 'custom-variable))
	      (t
	       (error "Unknown keyword %s" symbol)))))))

;;;###autoload
(defmacro defcustom (symbol value doc &rest args)
  "Declare SYMBOL as a customizable variable that defaults to VALUE.
DOC is the variable documentation.

Neither SYMBOL nor VALUE needs to be quoted.
If SYMBOL is not already bound, initialize it to VALUE.
The remaining arguments should have the form

   [KEYWORD VALUE]... 

The following KEYWORD's are defined:

:type	VALUE should be a sexp widget.
:group  VALUE should be a customization group.  
        Add SYMBOL to that group.

Read the section about customization in the emacs lisp manual for more
information."
  `(eval-and-compile
     (custom-declare-variable (quote ,symbol) (quote ,value) ,doc ,@args)))

;;; The `defface' Macro.

;;;###autoload
(defun custom-declare-face (face spec doc &rest args)
  "Like `defface', but FACE is evaluated as a normal argument."
  (put face 'factory-face spec)
  (let ((value (or (get face 'saved-face) spec)))
    (custom-face-display-set face value))
  (when doc
    (put face 'face-documentation doc))
  (while args 
    (let ((arg (car args)))
      (setq args (cdr args))
      (unless (symbolp arg)
	(error "Junk in args %S" args))
      (let ((keyword arg)
	    (value (car args)))
	(unless args
	  (error "Keyword %s is missing an argument" :type))
	(setq args (cdr args))
	(cond ((eq keyword :group)
	       (custom-add-to-group value face 'custom-face))
	      (t
	       (error "Unknown keyword %s" face)))))))

;;;###autoload
(defmacro defface (face spec doc &rest args)
  "Declare FACE as a customizable face that defaults to SPEC.
FACE does not need to be quoted.

Third argument DOC is the face documentation.

If FACE has been set with `custom-set-face', set the face attributes
as specified by that function, otherwise set the face attributes
according to SPEC.

The remaining arguments should have the form

   [KEYWORD VALUE]...

The following KEYWORD's are defined:

:group  VALUE should be a customization group.
        Add FACE to that group.

SPEC should be an alist of the form ((DISPLAY ATTS)...).

ATTS is a list of face attributes and their values.  The possible
attributes are defined in the variable `custom-face-attributes'.
Alternatively, ATTS can be a face in which case the attributes of that
face is used.

The ATTS of the first entry in SPEC where the DISPLAY matches the
frame should take effect in that frame.  DISPLAY can either be the
symbol `t', which will match all frames, or an alist of the form
\((REQ ITEM...)...)

For the DISPLAY to match a FRAME, the REQ property of the frame must
match one of the ITEM.  The following REQ are defined:

`type' (the value of (window-system))
  Should be one of `x' or `tty'.

`class' (the frame's color support)
  Should be one of `color', `grayscale', or `mono'.

`background' (what color is used for the background text)
  Should be one of `light' or `dark'.

Read the section about customization in the emacs lisp manual for more
information."
  `(custom-declare-face (quote ,face) ,spec ,doc ,@args))

;;; The `defgroup' Macro.

;;;###autoload
(defun custom-declare-group (symbol members doc &rest args)
  "Like `defgroup', but SYMBOL is evaluated as a normal argument."
  (put symbol 'custom-group (nconc members (get symbol 'custom-group)))
  (when doc
    (put symbol 'group-documentation doc))
  (while args 
    (let ((arg (car args)))
      (setq args (cdr args))
      (unless (symbolp arg)
	(error "Junk in args %S" args))
      (let ((keyword arg)
	    (value (car args)))
	(unless args
	  (error "Keyword %s is missing an argument" :type))
	(setq args (cdr args))
	(cond ((eq keyword :group)
	       (custom-add-to-group value symbol 'custom-group))
	      (t
	       (error "Unknown keyword %s" symbol)))))))

;;;###autoload
(defmacro defgroup (symbol members doc &rest args)
  "Declare SYMBOL as a customization group containing MEMBERS.
SYMBOL does not need to be quoted.

Third arg DOC is the group documentation.

MEMBERS should be an alist of the form ((NAME WIDGET)...) where
NAME is a symbol and WIDGET is a widget is a widget for editing that
symbol.  Useful widgets are `custom-variable' for editing variables,
`custom-face' for edit faces, and `custom-group' for editing groups.

The remaining arguments should have the form

   [KEYWORD VALUE]... 

The following KEYWORD's are defined:

:group  VALUE should be a customization group.
        Add SYMBOL to that group.

Read the section about customization in the emacs lisp manual for more
information."
  `(custom-declare-group (quote ,symbol) ,members ,doc ,@args))

;;;###autoload
(defun custom-add-to-group (group option widget)
  "To existing GROUP add a new OPTION of type WIDGET,
If there already is an entry for that option, overwrite it."
  (let* ((members (get group 'custom-group))
	 (old (assq option members)))
    (if old
	(setcar (cdr old) widget)
      (put group 'custom-group (nconc members (list (list option widget)))))))

;;; Face Utilities.

(and (fboundp 'make-face)
     (make-face 'custom-face-empty))

(defun custom-face-display-set (face spec &optional frame)
  "Set FACE to the attributes to the first matching entry in SPEC.
Iff optional FRAME is non-nil, set it for that frame only.
See `defface' for information about SPEC."
  (when (fboundp 'make-face)
    (make-face face)
    (copy-face 'custom-face-empty face)
    (while spec 
      (let* ((entry (car spec))
	     (display (nth 0 entry))
	     (atts (nth 1 entry)))
	(setq spec (cdr spec))
	(when (custom-display-match-frame display frame)
	  (apply 'custom-face-attribites-set face frame atts)
	  (setq spec nil))))))

(defcustom custom-background-mode nil
  "The brightness of the background.
Set this to the symbol dark if your background color is dark, light if
your background is light, or nil (default) if you want Emacs to
examine the brightness for you."
  :group 'customize
  :type '(choice (choice-item dark) 
		 (choice-item light)
		 (choice-item :tag "default" nil)))

(defun custom-display-match-frame (display frame)
  "Non-nil iff DISPLAY matches FRAME.
If FRAME is nil, the current FRAME is used."
  ;; This is a kludge to get started, we realle should use specifiers!
  (unless frame 
    (setq frame (selected-frame)))
  (if (eq display t)
      t
    (let ((match t)
	  (pars (frame-parameters frame)))
      (while (and display match)
	(let* ((entry (car display))
	       (req (car entry))
	       (options (cdr entry)))
	  (setq display (cdr display))
	  (cond ((eq req 'type)
		 (setq match (if (fboundp 'device-type)
				 (device-type frame)
			       (memq window-system options))))
		((eq req 'class)
		 (let ((class (if (fboundp 'device-class)
				  (device-class frame)
				(cdr (assq 'display-type pars)))))
		   (setq match (memq class options))))
		((eq req 'background)
		 (let ((background (or custom-background-mode
				       (cdr (assq 'background-mode pars))
				       (custom-background-mode))))
		   (setq match (memq background options))))
		(t
		 (error "Unknown req `%S' with options `%S'" req options)))))
      match)))

(defvar custom-face-attributes
  '((:bold (toggle :format "Bold: %v") custom-set-face-bold)
    (:italic (toggle :format "Italic: %v") custom-set-face-italic)
    (:underline
     (toggle :format "Underline: %v") set-face-underline-p)
    (:foreground (color :tag "Foreground") set-face-foreground)
    (:background (color :tag "Background") set-face-background)
    (:stipple (editable-field :format "Stipple: %v") set-face-stipple))
  "Alist of face attributes. 

The elements are of the form (KEY TYPE SET) where KEY is a symbol
identifying the attribute, TYPE is a widget type for editing the
attibute, SET is a function for setting the attribute value.

The SET function should take three arguments, the face to modify, the
value of the attribute, and optionally the frame where the face should
be changed.")

(defun custom-face-attribites-set (face frame &rest atts)
  "For FACE on FRAME set the attributes [KEYWORD VALUE]....
Each keyword should be listed in `custom-face-attributes'.

If FRAME is nil, set the default face."
  (while atts 
    (let* ((name (nth 0 atts))
	   (value (nth 1 atts))
	   (fun (nth 2 (assq name custom-face-attributes))))
      (setq atts (cdr (cdr atts))) 
      (funcall fun face value))))

(defun custom-set-face-bold (face value &optional frame)
  "Set the bold property of FACE to VALUE."
  (condition-case nil
      (if value
	  (make-face-bold face frame)
	(make-face-unbold face frame))
    (error nil)))

(defun custom-set-face-italic (face value &optional frame)
  "Set the italic property of FACE to VALUE."
  (condition-case nil
      (if value
	  (make-face-italic face frame)
	(make-face-unitalic face frame))
    (error nil)))

;;;###autoload
(defun custom-initialize-faces (&optional frame)
  "Initialize all custom faces for FRAME.
If FRAME is nil or omitted, initialize them for all frames."
  (mapatoms (lambda (symbol)
	      (let ((spec (or (get symbol 'saved-face)
			      (get symbol 'factory-face))))
		(when spec 
		  (custom-face-display-set symbol spec frame))))))

;;; Initializing.

;;;###autoload
(defun custom-set-variables (&rest args)
  "Initialize variables according to user preferences.  
The arguments should have the form [SYMBOL VALUE]...
For each symbol, VALUE is evaluated and bound as the default value for
the symbol.  The unevaluated VALUE is also stored as the saved value
for that symbol."
  (while args 
    (let ((symbol (nth 0 args))
	  (value (nth 1 args)))
      (set-default symbol (eval value))
      (put symbol 'saved-value (list value)))
    (setq args (cdr (cdr args)))))

;;;###autoload
(defun custom-set-faces (&rest args)
  "Initialize faces according to user preferences.
The arguments should have the form [SYMBOL SPEC]...
For each symbol, a face with that name is created according to SPEC.
See `defface' for the format of SPEC."
  (while args
    (let ((face (nth 0 args))
	  (spec (nth 1 args)))
      (put face 'saved-face spec)
      (custom-face-display-set face spec))
    (setq args (cdr (cdr args)))))

;;; Meta Customization

(defgroup emacs nil
  "Customization of the One True Editor.")

(defgroup customize nil
  "Customization of the Customization support."
  :group 'emacs)

;;; The End.

(provide 'custom)

;; custom.el ends here
