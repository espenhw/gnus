;;; cus-face.el -- XEmacs specific custom support.
;;
;; Copyright (C) 1996, 1997 Free Software Foundation, Inc.
;;
;; Author: Per Abrahamsen <abraham@dina.kvl.dk>
;; Keywords: help, faces
;; Version: 1.48
;; X-URL: http://www.dina.kvl.dk/~abraham/custom/

;;; Commentary:
;;
;; See `custom.el'.

;;; Code:

(require 'custom)

;;; Compatibility.

(unless (fboundp 'frame-property)
  ;; XEmacs function missing in Emacs 19.34.
  (defun frame-property (frame property &optional default)
    "Return FRAME's value for property PROPERTY."
    (or (cdr (assq property (frame-parameters frame)))
	default)))

(unless (fboundp 'x-color-values)
  ;; Emacs function missing in XEmacs 19.14.
  (defun x-color-values  (color &optional frame)
    "Return a description of the color named COLOR on frame FRAME.
The value is a list of integer RGB values--(RED GREEN BLUE).
These values appear to range from 0 to 65280 or 65535, depending
on the system; white is (65280 65280 65280) or (65535 65535 65535).
If FRAME is omitted or nil, use the selected frame."
    (color-instance-rgb-components (make-color-instance color))))

;; XEmacs and Emacs have different definitions of `facep'.  
;; The Emacs definition is the useful one, so emulate that. 
(cond ((not (fboundp 'facep))
       (defun custom-facep (face) 
	 "No faces"
	 nil))
      ((string-match "XEmacs" emacs-version)
       (defalias 'custom-facep 'find-face))
      (t
       (defalias 'custom-facep 'facep)))

;; Overwrite Emacs definition.
(if (string-match "XEmacs" emacs-version)
    (progn 
      (defun custom-extract-frame-properties (frame)
	"Return a plist with the frame properties of FRAME used by custom."
	(list 'type (device-type (frame-device frame))
	      'class (device-class (frame-device frame))
	      'background (or custom-background-mode
			      (frame-property frame
					      'background-mode)
			      (custom-background-mode frame))))

      (defun get-face-documentation (face)
	"Get the documentation string for FACE."
	(face-property face 'doc-string))

      (defun set-face-documentation (face string)
	"Set the documentation string for FACE to STRING."
	(set-face-property face 'doc-string string)))
  
  (defun custom-extract-frame-properties (frame)
    "Return a plist with the frame properties of FRAME used by custom."
    (list 'type window-system
	  'class (frame-property frame 'display-type)
	  'background (or custom-background-mode
			  (frame-property frame
					  'background-mode)
			  (custom-background-mode frame))))  

  (defun get-face-documentation (face)
    "Get the documentation string for FACE."
    (get face 'face-documentation))

  (defun set-face-documentation (face string)
    "Set the documentation string for FACE to STRING."
    (put face 'face-documentation string)))

;;; Declaring a face.

;;;###autoload
(defun custom-declare-face (face spec doc &rest args)
  "Like `defface', but FACE is evaluated as a normal argument."
  (when (fboundp 'load-gc)
    ;; This should be allowed, somehow.
    (error "Attempt to declare a face during dump"))
  (unless (get face 'factory-face)
    (put face 'factory-face spec)
    (when (fboundp 'facep)
      (unless (and (custom-facep face)
		   (not (get face 'saved-face)))
	;; If the user has already created the face, respect that.
	(let ((value (or (get face 'saved-face) spec))
	      (frames (custom-relevant-frames))
	      frame)
	  ;; Create global face.
	  (custom-face-display-set face value)
	  ;; Create frame local faces
	  (while frames
	    (setq frame (car frames)
		  frames (cdr frames))
	    (custom-face-display-set face value frame)))))
    (when (and doc (null (get-face-documentation face)))
      (set-face-documentation face doc))
    (custom-handle-all-keywords face args 'custom-face)
    (run-hooks 'custom-define-hook))
  face)

;;; Font Attributes.

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
	  (funcall fun face value frame)
	(error nil)))))

(defconst custom-face-attributes
  '((:bold (toggle :format "Bold: %[%v%]\n") custom-set-face-bold)
    (:italic (toggle :format "Italic: %[%v%]\n") custom-set-face-italic)
    (:underline
     (toggle :format "Underline: %[%v%]\n") set-face-underline-p)
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

(when (string-match "XEmacs" emacs-version)
  ;; Support for special XEmacs font attributes.
  (autoload 'font-create-object "font" nil)

  (unless (fboundp 'face-font-name)
    (defun face-font-name (face &rest args)
      (apply 'face-font face args)))

  (defun custom-set-face-font-size (face size &rest args)
    "Set the font of FACE to SIZE"
    (let* ((font (apply 'face-font-name face args))
	   (fontobj (font-create-object font)))
      (set-font-size fontobj size)
      (apply 'set-face-font face fontobj args)))

  (defun custom-set-face-font-family (face family &rest args)
    "Set the font of FACE to FAMILY"
    (let* ((font (apply 'face-font-name face args))
	   (fontobj (font-create-object font)))
      (set-font-family fontobj family)
      (apply 'set-face-font face fontobj args)))

  (nconc custom-face-attributes
	 '((:family (editable-field :format "Family: %v") 
		    custom-set-face-font-family)
	   (:size (editable-field :format "Size: %v")
		  custom-set-face-font-size)))

  ;; Disable frame local faces.
  (setq custom-relevant-frames nil)
  (remove-hook 'after-make-frame-hook 'custom-initialize-frame))

;;; Frames.

(and (fboundp 'make-face)
     (make-face 'custom-face-empty))

(defun custom-face-display-set (face spec &optional frame)
  "Set FACE to the attributes to the first matching entry in SPEC.
Iff optional FRAME is non-nil, set it for that frame only.
See `defface' for information about SPEC."
  (when (fboundp 'copy-face)
    (while spec 
      (let* ((entry (car spec))
	     (display (nth 0 entry))
	     (atts (nth 1 entry)))
	(setq spec (cdr spec))
	(when (custom-display-match-frame display frame)
	  ;; Avoid creating frame local duplicates of the global face.
	  (unless (and frame (eq display (get face 'custom-face-display)))
	    (copy-face 'custom-face-empty face frame)
	    (apply 'custom-face-attribites-set face frame atts))
	  (unless frame
	    (put face 'custom-face-display display))
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

(defun custom-background-mode (frame)
  "Kludge to detect background mode for FRAME."
  (let* ((bg-resource 
	  (condition-case ()
	      (x-get-resource ".backgroundMode" "BackgroundMode" 'string)
	    (error nil)))
	 color
	 (mode (cond (bg-resource
		      (intern (downcase bg-resource)))
		     ((and (setq color (condition-case ()
					   (or (frame-property
						frame
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
    (modify-frame-parameters frame (list (cons 'background-mode mode)))
    mode))

(defvar custom-default-frame-properties nil
  "The frame properties used for the global faces.
Frames who doesn't match these propertiess should have frame local faces.
The value should be nil, if uninitialized, or a plist otherwise.  
See `defface' for a list of valid keys and values for the plist.")

(defun custom-get-frame-properties (&optional frame)
  "Return a plist with the frame properties of FRAME used by custom.
If FRAME is nil, return the default frame properties."
  (cond (frame
	 ;; Try to get from cache.
	 (let ((cache (frame-property frame 'custom-properties)))
	   (unless cache
	     ;; Oh well, get it then.
	     (setq cache (custom-extract-frame-properties frame))
	     ;; and cache it...
	     (modify-frame-parameters frame 
				      (list (cons 'custom-properties cache))))
	   cache))
	(custom-default-frame-properties)
	(t
	 (setq custom-default-frame-properties
	       (custom-extract-frame-properties (selected-frame))))))

(defun custom-display-match-frame (display frame)
  "Non-nil iff DISPLAY matches FRAME.
If FRAME is nil, the current FRAME is used."
  ;; This is a kludge to get started, we really should use specifiers!
  (if (eq display t)
      t
    (let* ((props (custom-get-frame-properties frame))
	   (type (plist-get props 'type))
	   (class (plist-get props 'class))
	   (background (plist-get props 'background))
	   (match t)
	   (entries display)
	   entry req options)
      (while (and entries match)
	(setq entry (car entries)
	      entries (cdr entries)
	      req (car entry)
	      options (cdr entry)
	      match (cond ((eq req 'type)
			   (memq type options))
			  ((eq req 'class)
			   (memq class options))
			  ((eq req 'background)
			   (memq background options))
			  (t
			   (error "Unknown req `%S' with options `%S'" 
				  req options)))))
      match)))

(defvar custom-relevant-frames t
  "List of frames whose custom properties differ from the default.")

(defun custom-relevant-frames ()
  "List of frames whose custom properties differ from the default."
  (when (eq custom-relevant-frames t)
    (setq custom-relevant-frames nil)
    (let ((default (custom-get-frame-properties))
	  (frames (frame-list))
	  frame)
      (while frames
	(setq frame (car frames)
	      frames (cdr frames))
	(unless (equal default (custom-get-frame-properties frame))
	  (push frame custom-relevant-frames)))))
  custom-relevant-frames)

(defun custom-initialize-faces (&optional frame)
  "Initialize all custom faces for FRAME.
If FRAME is nil or omitted, initialize them for all frames."
  (mapatoms (lambda (symbol)
	      (let ((spec (or (get symbol 'saved-face)
			      (get symbol 'factory-face))))
		(when spec 
		  (custom-face-display-set symbol spec frame))))))

(defun custom-initialize-frame (&optional frame)
  "Initialize local faces for FRAME if necessary.
If FRAME is missing or nil, the first member (frame-list) is used."
  (unless frame
    (setq frame (car (frame-list))))
  (unless (equal (custom-get-frame-properties) 
		 (custom-get-frame-properties frame))
    (custom-initialize-faces frame)
    (push frame custom-relevant-frames)))

;; Enable.  This should go away when bundled with Emacs.
(add-hook 'after-make-frame-hook 'custom-initialize-frame)

;;; Initializing.

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

;;; The End.

(provide 'cus-face)

;; cus-face.el ends here
