;;; custom.el -- Tools for declaring and initializing options.
;;
;; Copyright (C) 1996 Free Software Foundation, Inc.
;;
;; Author: Per Abrahamsen <abraham@dina.kvl.dk>
;; Keywords: help, faces
;; Version: 1.20
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

(define-widget-keywords :prefix :tag :load :link :options :type :group)

;; These autoloads should be deleted when the file is added to Emacs
(autoload 'customize "custom-edit" nil t)
(autoload 'customize-variable "custom-edit" nil t)
(autoload 'customize-face "custom-edit" nil t)
(autoload 'customize-apropos "custom-edit" nil t)
(autoload 'customize-customized "custom-edit" nil t)
(autoload 'custom-buffer-create "custom-edit")
(autoload 'custom-menu-update "custom-edit")
(autoload 'custom-make-dependencies "custom-edit")

;;; Compatibility.

(unless (fboundp 'x-color-values)
  ;; Emacs function missing in XEmacs 19.14.
  (defun x-color-values  (color)
    "Return a description of the color named COLOR on frame FRAME.
The value is a list of integer RGB values--(RED GREEN BLUE).
These values appear to range from 0 to 65280 or 65535, depending
on the system; white is (65280 65280 65280) or (65535 65535 65535).
If FRAME is omitted or nil, use the selected frame."
    (color-instance-rgb-components (make-color-instance color))))

(unless (fboundp 'frame-property)
  ;; XEmacs function missing in Emacs 19.34.
  (defun frame-property (frame property &optional default)
    "Return FRAME's value for property PROPERTY."
    (or (cdr (assq property (frame-parameters frame)))
	default)))

(defun custom-background-mode ()
  "Kludge to detext background mode."
  (let* ((bg-resource 
	  (condition-case ()
	      (x-get-resource ".backgroundMode" "BackgroundMode" 'string)
	    (error nil)))
	 color
	 (mode (cond (bg-resource
		      (intern (downcase bg-resource)))
		     ((and (setq color (condition-case ()
					   (or (frame-property
						(selected-frame)
						'background-color)
					       (color-instance-name
						(specifier-instance
						 (face-background 'default))))
					 (error nil)))
			   (< (apply '+ (x-color-values color))
			      (/ (apply '+ (x-color-values "white"))
				 3)))
		      'dark)
		     (t 'light))))
    (modify-frame-parameters (selected-frame)
			     (list (cons 'background-mode mode)))
    mode))

;; XEmacs and Emacs have different definitions of `facep'.  
;; The Emacs definition is the useful one, so emulate that. 
(cond ((not (fboundp 'facep))
       (defun custom-facep (face) 
	 "No faces"
	 nil))
      ((string-match "XEmacs" emacs-version)
       (defun custom-facep (face) 
	 "Face symbol or object."
	 (or (facep face)
	     (find-face face))))
      (t
       (defalias 'custom-facep 'facep)))

;;; The `defcustom' Macro.

;;;###autoload
(defun custom-declare-variable (symbol value doc &rest args)
  "Like `defcustom', but SYMBOL and VALUE are evaluated as notmal arguments."
  (unless (and (default-boundp symbol)
	       (not (get symbol 'saved-value)))
    (set-default symbol (if (get symbol 'saved-value)
			    (eval (car (get symbol 'saved-value)))
			  (eval value))))
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
	      ((eq keyword :options)
	       (if (get symbol 'custom-options)
		   ;; Slow safe code to avoid duplicates.
		   (mapcar (lambda (option)
			     (custom-add-option symbol option))
			   value)
		 ;; Fast code for the common case.
		 (put symbol 'custom-options (copy-list value))))
	      (t
	       (custom-handle-keyword symbol keyword value
				      'custom-variable))))))
  (run-hooks 'custom-define-hook)
  symbol)

;;;###autoload
(defmacro defcustom (symbol value doc &rest args)
  "Declare SYMBOL as a customizable variable that defaults to VALUE.
DOC is the variable documentation.

Neither SYMBOL nor VALUE needs to be quoted.
If SYMBOL is not already bound, initialize it to VALUE.
The remaining arguments should have the form

   [KEYWORD VALUE]... 

The following KEYWORD's are defined:

:type	VALUE should be a widget type.
:options VALUE should be a list of valid members of the widget type.
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
  (when (fboundp 'facep)
    (unless (and (custom-facep face)
		 (not (get face 'saved-face)))
      ;; If the user has already created the face, respect that.
      (let ((value (or (get face 'saved-face) spec)))
	(custom-face-display-set face value))))
  (when doc
    (put face 'face-documentation doc))
  (custom-handle-all-keywords face args 'custom-face)
  (run-hooks 'custom-define-hook)
  face)

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
	  (error "Keyword %s is missing an argument" keyword))
	(setq args (cdr args))
	(cond ((eq keyword :prefix)
	       (put symbol 'custom-prefix value))
	      (t
	       (custom-handle-keyword symbol keyword value
				      'custom-group))))))
  (run-hooks 'custom-define-hook)
  symbol)

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

;;; Properties.

(defun custom-handle-all-keywords (symbol args type)
  "For customization option SYMBOL, handle keyword arguments ARGS.
Third argument TYPE is the custom option type."
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
	(custom-handle-keyword symbol keyword value type)))))  

(defun custom-handle-keyword (symbol keyword value type)
  "For customization option SYMBOL, handle KEYWORD with VALUE.
Fourth argument TYPE is the custom option type."
  (cond ((eq keyword :group)
	 (custom-add-to-group value symbol type))
	((eq keyword :link)
	 (custom-add-link symbol value))
	((eq keyword :load)
	 (custom-add-load symbol value))
	((eq keyword :tag)
	 (put symbol 'custom-tag value))
	(t
	 (error "Unknown keyword %s" symbol))))  

(defun custom-add-option (symbol option)
  "To the variable SYMBOL add OPTION.

If SYMBOL is a hook variable, OPTION should be a hook member.
For other types variables, the effect is undefined."
  (let ((options (get symbol 'custom-options)))
    (unless (member option options)
      (put symbol 'custom-options (cons option options)))))

(defun custom-add-link (symbol widget)
  "To the custom option SYMBOL add the link WIDGET."
  (let ((links (get symbol 'custom-links)))
    (unless (member widget links)
      (put symbol 'custom-links (cons widget links)))))

(defun custom-add-load (symbol load)
  "To the custom option SYMBOL add the dependency LOAD.
LOAD should be either a library file name, or a feature name."
  (let ((loads (get symbol 'custom-loads)))
    (unless (member load loads)
      (put symbol 'custom-loads (cons load loads)))))

;;; Face Utilities.

(and (fboundp 'make-face)
     (make-face 'custom-face-empty))

(defun custom-face-display-set (face spec &optional frame)
  "Set FACE to the attributes to the first matching entry in SPEC.
Iff optional FRAME is non-nil, set it for that frame only.
See `defface' for information about SPEC."
  (when (fboundp 'copy-face)
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
  ;; This is a kludge to get started, we really should use specifiers!
  (unless frame 
    (setq frame (selected-frame)))
  (if (eq display t)
      t
    (let ((match t))
      (while (and display match)
	(let* ((entry (car display))
	       (req (car entry))
	       (options (cdr entry)))
	  (setq display (cdr display))
	  (cond ((eq req 'type)
		 (let ((type (if (fboundp 'device-type)
				 (device-type (frame-device frame))
			       window-system)))
		   (setq match (memq type options))))
		((eq req 'class)
		 (let ((class (if (fboundp 'device-class)
				  (device-class (frame-device frame))
				(frame-property frame 'display-type))))
		   (setq match (memq class options))))
		((eq req 'background)
		 (let ((background (or custom-background-mode
				       (frame-property frame 'background-mode)
				       (custom-background-mode))))
		   (setq match (memq background options))))
		(t
		 (error "Unknown req `%S' with options `%S'" req options)))))
      match)))

(defconst custom-face-attributes
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

(when (string-match "XEmacs" emacs-version)
  ;; Support for special XEmacs font attributes.
  (require 'font)

  (unless (fboundp 'face-font-name)
    (defun face-font-name (face &rest args)
      (apply 'face-font face args)))

  (defun set-face-font-size (face size &rest args)
    "Set the font of FACE to SIZE"
    (let* ((font (apply 'face-font-name face args))
	   (fontobj (font-create-object font)))
      (set-font-size fontobj size)
      (apply 'set-face-font face fontobj args)))

  (defun set-face-font-family (face family &rest args)
    "Set the font of FACE to FAMILY"
    (let* ((font (apply 'face-font-name face args))
	   (fontobj (font-create-object font)))
      (set-font-family fontobj family)
      (apply 'set-face-font face fontobj args)))

  (nconc custom-face-attributes
	 '((:family (editable-field :format "Family: %v") 
		    set-face-font-family)
	   (:size (editable-field :format "Size: %v")
		  set-face-font-size))))

(defun custom-face-attribites-set (face frame &rest atts)
  "For FACE on FRAME set the attributes [KEYWORD VALUE]....
Each keyword should be listed in `custom-face-attributes'.

If FRAME is nil, set the default face."
  (while atts 
    (let* ((name (nth 0 atts))
	   (value (nth 1 atts))
	   (fun (nth 2 (assq name custom-face-attributes))))
      (setq atts (cdr (cdr atts)))
      (condition-case nil
	  (funcall fun face value)
	(error nil)))))

(defun custom-set-face-bold (face value &optional frame)
  "Set the bold property of FACE to VALUE."
  (if value
      (make-face-bold face frame)
    (make-face-unbold face frame)))

(defun custom-set-face-italic (face value &optional frame)
  "Set the italic property of FACE to VALUE."
  (if value
      (make-face-italic face frame)
    (make-face-unitalic face frame)))

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

The arguments should be a list where each entry has the form:

  (SYMBOL VALUE [NOW])

The unevaluated VALUE is stored as the saved value for SYMBOL.
If NOW is present and non-nil, VALUE is also evaluated and bound as
the default value for the SYMBOL."
  (while args 
    (let ((entry (car args)))
      (if (listp entry)
	  (let ((symbol (nth 0 entry))
		(value (nth 1 entry))
		(now (nth 2 entry)))
	    (put symbol 'saved-value (list value))
	    (when now 
	      (put symbol 'force-value t)
	      (set-default symbol (eval value)))
	    (setq args (cdr args)))
	;; Old format, a plist of SYMBOL VALUE pairs.
	(let ((symbol (nth 0 args))
	      (value (nth 1 args)))
	  (put symbol 'saved-value (list value)))
	(setq args (cdr (cdr args)))))))

;;;###autoload
(defun custom-set-faces (&rest args)
  "Initialize faces according to user preferences.
The arguments should be a list where each entry has the form:

  (FACE SPEC [NOW])

SPEC will be stored as the saved value for FACE.  If NOW is present
and non-nil, FACE will also be created according to SPEC.

See `defface' for the format of SPEC."
  (while args
    (let ((entry (car args)))
      (if (listp entry)
	  (let ((face (nth 0 entry))
		(spec (nth 1 entry))
		(now (nth 2 entry)))
	    (put face 'saved-face spec)
	    (when now
	      (put face 'force-face t)
	      (custom-face-display-set face spec))
	    (setq args (cdr args)))
	;; Old format, a plist of FACE SPEC pairs.
	(let ((face (nth 0 args))
	      (spec (nth 1 args)))
	  (put face 'saved-face spec))
	(setq args (cdr (cdr args)))))))

;;; Meta Customization

(defgroup emacs nil
  "Customization of the One True Editor."
  :link '(custom-manual "(emacs)Top"))

(defgroup customize nil
  "Customization of the Customization support."
  :link '(custom-manual "(custom)Top")
  :link '(url-link :tag "Development Page" 
		   "http://www.dina.kvl.dk/~abraham/custom/")
  :prefix "custom-"
  :group 'emacs)

(defcustom custom-define-hook nil
  "Hook called after defining each customize option."
  :group 'customize
  :type 'hook)

;;; Menu support

(defconst custom-help-menu '("Customize"
			     ["Update menu..." custom-menu-update t]
			     ["Group..." customize t]
			     ["Variable..." customize-variable t]
			     ["Face..." customize-face t]
			     ["Saved..." customize-customized t]
			     ["Apropos..." customize-apropos t])
  "Customize menu")

(defun custom-menu-reset ()
  "Reset customize menu."
  (remove-hook 'custom-define-hook 'custom-menu-reset)
  (if (fboundp 'add-submenu)
      (add-submenu '("Help") custom-help-menu)
    (define-key global-map [menu-bar help-menu customize-menu]
      (cons (car custom-help-menu)
	    (easy-menu-create-keymaps (car custom-help-menu)
				      (cdr custom-help-menu))))))

(custom-menu-reset)

;;; The End.

(provide 'custom)

;; custom.el ends here
