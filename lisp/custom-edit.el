;;; custom-edit.el --- Tools for customization Emacs.
;;
;; Copyright (C) 1996 Free Software Foundation, Inc.
;;
;; Author: Per Abrahamsen <abraham@dina.kvl.dk>
;; Keywords: help, faces
;; Version: 0.9
;; X-URL: http://www.dina.kvl.dk/~abraham/custom/

;;; Commentary:
;;
;; See `custom.el'.

;;; Code:

(require 'custom)
(require 'widget-edit)

(let ((keywords '(:custom-show 
		  :custom-documentation-show 
		  :custom-documentation-property 
		  :custom-level
		  :custom-status)))
  (while keywords
    (or (boundp (car keywords))
	(set (car keywords) (car keywords)))
    (setq keywords (cdr keywords))))

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

;;; The `custom-level' Widget.

(define-widget 'custom-level 'item
  "The custom level buttons."
  :format "%[%t%]"
  :help-echo "Push me to expand or collapse this item."
  :action 'custom-level-action)

(defun custom-level-action (widget &optional event)
  "Toggle visibility for parent to WIDGET."
  (let* ((parent (widget-get widget :parent))
	 (show (widget-get parent :custom-show)))
    (widget-apply parent :validate)
    (widget-put parent :custom-show (not show))
    (custom-reset parent)))

;;; The `custom-help' Widget.

(define-widget 'custom-help 'push
  "The custom documentation button."
  :help-echo "Push me to toggle the documentation."
  :action 'custom-help-action)

(defun custom-help-action (widget  &optional event)
  "Toggle documentation visibility for parent to WIDGET."
  (let* ((parent (widget-get widget :parent))
	 (symbol (widget-get parent :value))
	 (property (widget-get parent :custom-documentation-property))
	 (text (or (widget-get parent :doc)
		   (documentation-property symbol property)))
	 (newline (string-match "\n." text))
	 (old (widget-get parent :custom-documentation-show))
	 (new (cond ((eq old t)
		     nil)
		    ((null old)
		     (if newline 
			 'first-line
		       t))
		    (t 
		     (if newline
			 t
		       nil)))))
    (widget-apply parent :validate)
    (widget-put parent :custom-documentation-show new)
    (custom-reset parent)))

;;; The `custom' Widget.

(define-widget 'custom 'default
  "Customize a user option."
  :convert-widget 'widget-item-convert-widget
  :format "%l%h%[%t%]: %v%x"
  :format-handler 'custom-format-handler
  :custom-level 1
  :custom-show nil
  :custom-documentation-show 'first-line
  :custom-documentation-property 'widget-subclass-responsibility
  :value-create 'widget-subclass-responsibility
  :value-delete 'widget-radio-value-delete
  :value-get 'widget-item-value-get
  :validate 'widget-repeat-validate
  :match (lambda (widget value) (symbolp value)))

(defun custom-format-handler (widget escape)
  ;; We recognize extra escape sequences.
  (let* (child 
	 (symbol (widget-get widget :value))
	 (level (widget-get widget :custom-level))
	 (doc-property (widget-get widget :custom-documentation-property))
	 (doc-try (or (widget-get widget :doc)
		      (documentation-property symbol doc-property)))
	 (doc-text (and (stringp doc-try)
			(> (length doc-try) 1)
			doc-try))
	 (doc-show (widget-get widget :custom-documentation-show)))
    (cond ((eq escape ?l)
	   (when level 
	     (setq child
		   (widget-create 'custom-level
				  :parent widget
				  (make-string level ?*)))
	     (widget-insert " ")))
	  ((eq escape ?h)
	   (when doc-text
	     (setq child (widget-create 'custom-help 
					:parent widget
					"?"))
	     (widget-insert " ")))
	  ((eq escape ?x)
	   (and doc-text doc-show
		(let ((start (point)))
		  ;; The first "*" in a doc string means interactively
		  ;; user editable.  Since we are providing a facility
		  ;; for the user to interactively edit the variable,
		  ;; that information is redundant.  Remove it.
		  (cond ((eq doc-show t)
			 (if (eq (aref doc-text  0) ?*)
			     (widget-insert (substring doc-text 1))
			   (widget-insert doc-text)))
			((eq (aref doc-text 0) ?*)
			 (string-match "\\`.\\(.*\\)" doc-text)
			 (widget-insert (match-string 1 doc-text)))
			(t
			 (string-match "\\`.*" doc-text)
			 (widget-insert (match-string 0 doc-text))))
		  (unless (eq (preceding-char) ?\n)
		    (widget-insert "\n"))
		  (widget-specify-doc widget start (point)))))
	  (t 
	   (widget-default-format-handler widget escape)))
    (when child
      (widget-put widget
		  :buttons (cons child (widget-get widget :buttons))))))

(defun custom-unimplemented (&rest ignore)
  "Apologize for my laziness."
  (error "Sorry, not implemented"))

(defun custom-reset (widget)
  "Redraw WIDGET with current settings."
  (widget-value-set widget (widget-value widget))
  (widget-setup))

;;; The `custom-variable' Widget.

(define-widget 'custom-variable 'custom
  "Customize variable."
  :help-echo "Push me to set or reset this variable."
  :custom-documentation-property 'variable-documentation
  :custom-show 'child
  :custom-status 'edit
  :value-create 'custom-variable-value-create
  :action 'custom-variable-action)

(widget-put (get 'default 'widget-type) :custom-show t)

(defun custom-variable-value-create (widget)
  "Here is where you edit the variables value."
  (let* ((status (widget-get widget :custom-status))
	 (child-show (widget-get widget :custom-show))
	 (symbol (widget-get widget :value))
	 (child-type (or (get symbol 'custom-type) 'sexp))
	 (type (if (listp child-type) child-type (list child-type)))
	 (show (if (eq child-show 'child)
		   (widget-get type :custom-show)
		 child-show))
	 (dummy (widget-put widget :custom-show show))
	 (child (cond ((not show)
		       (widget-create 'custom-level
				      "[show]"))
		      ((eq status 'lisp)
		       (let ((value (cond ((get symbol 'saved-value)
					   (car (get symbol 'saved-value)))
					  ((get symbol 'factory-value)
					   (car (get symbol 'factory-value)))
					  ((boundp symbol)
					   (custom-quote
					    (symbol-value symbol)))
					  (t
					   (custom-quote
					    (widget-get type :value))))))
			 (widget-create 'sexp :value value)))
		      (t
		       (let ((value (if (boundp symbol)
					(symbol-value symbol)
				      (widget-get type :value))))
			 (widget-create type :value value))))))
    (unless (eq (preceding-char) ?\n)
      (widget-insert "\n"))
    (widget-put child :parent widget)
    (widget-put widget :children (list child))))

(defvar custom-variable-menu 
  '(("Edit" . custom-variable-edit)
    ("Edit Lisp" . custom-variable-edit-lisp)
    ("Apply" . custom-variable-apply)
    ("Set Default" . custom-variable-set-default)
    ("Reset" . custom-reset)
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
  (custom-variable-apply widget)
  (widget-put widget :custom-show t)
  (widget-put widget :custom-status 'edit)
  (custom-reset widget))

(defun custom-variable-edit-lisp (widget)
  "Edit the lisp representation of the value of WIDGET."
  (custom-variable-apply widget)
  (widget-put widget :custom-show t)
  (widget-put widget :custom-status 'lisp)
  (custom-reset widget))

(defun custom-variable-apply (widget)
  "Set the current value for the variable being edited by WIDGET."
  (let ((status (widget-get widget :custom-status))
	(show (widget-get widget :custom-show))
	(child (car (widget-get widget :children)))
	(symbol (widget-value widget)))
    (unless show
      (error "You can only apply visible options"))
    (widget-apply child :validate)
    (if (eq status 'lisp)
	(set symbol (eval (widget-value child)))
      (set symbol (widget-value child)))))

(defun custom-variable-set-default (widget)
  "Set the default value for the variable being edited by WIDGET."
  (let ((status (widget-get widget :custom-status))
	(show (widget-get widget :custom-show))
	(child (car (widget-get widget :children)))
	(symbol (widget-value widget)))
    (unless show
      (error "Can't apply hidden value."))
    (widget-apply child :validate)
    (if (eq status 'lisp)
	(put symbol 'saved-value (list (widget-value child)))
      (put symbol 'saved-value (list (custom-quote (widget-value child)))))))

(defun custom-variable-default (widget)
  "Restore the default value for the variable being edited by WIDGET."
  (let ((symbol (widget-value widget)))
    (cond ((get symbol 'saved-value)
	   (set symbol (car (get symbol 'saved-value))))
	  ((get symbol 'factory-value)
	   (set symbol (car (get symbol 'factory-value))))
	  (t
	   (error "No default value for %s" symbol))))
  (custom-reset widget))

(defun custom-variable-factory (widget)
  "Restore the factory setting for the variable being edited by WIDGET."
  (let ((symbol (widget-value widget)))
    (if (get symbol 'factory-value)
	(set symbol (car (get symbol 'factory-value)))
      (error "No factory default for %S" symbol)))
  (custom-reset widget))

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
  :format "%t:\n%v"
  :tag "Attributes"
  :args (mapcar (lambda (att)
		  (list 'group 
			:inline t
			(list 'const :format "" :value (nth 0 att)) 
			(nth 1 att)))
		custom-face-attributes))

;;; The `custom-display' Widget.

(define-widget 'custom-display 'choice
  "Select a display type."
  :tag "Display"
  :value t
  :args '((const :tag "all" t)
	  (checklist :entry-format "\n%b %v"
		     :args ((list (const :format "Type: " type)
				  (checklist :inline t
					     (const :format "X "
						    x)
					     (const :format "TTY"
						    tty)))
			    (list (const :format "Class: " class)
				  (checklist :inline t
					     (const :format "Color "
						    color)
					     (const :format
						    "Grayscale "
						    grayscale)
					     (const :format "Monochrome"
						    mono)))
			    (list (const :format "Background: " background)
				  (checklist :inline t
					     (const :format "Light "
						    light)
					     (const :format "Dark\n"
						    dark)))))))

;;; The `custom-face' Widget.

(define-widget 'custom-face 'custom
  "Customize face."
  :format "%l%h%[%t%]: %s%x%v"
  :format-handler 'custom-face-format-handler
  :help-echo "Push me to set or reset this face."
  :custom-documentation-property 'face-documentation
  :value-create 'custom-face-value-create
  :action 'custom-face-action)

(defun custom-face-format-handler (widget escape)
  ;; We recognize extra escape sequences.
  (let* (child 
	 (symbol (widget-get widget :value)))
    (cond ((eq escape ?s)
	   (setq child (widget-create 'choice-item
				      :parent widget
				      :format "(%[sample%])\n"
				      :button-face symbol)))
	  (t 
	   (custom-format-handler widget escape)))
    (when child
      (widget-put widget
		  :buttons (cons child (widget-get widget :buttons))))))

(defun custom-face-value-create (widget)
  ;; Create a list of the display specifications.
  (when (widget-get widget :custom-show)
    (let* ((symbol (widget-value widget))
	   (edit (widget-create 'repeat 
				:entry-format "%i %d %v"
				:parent widget
				:value (or (get symbol 'saved-face)
					   (get symbol 'factory-face))
				'(list custom-display custom-face-edit))))
      (widget-put widget :children (list edit)))))

(defvar custom-face-menu 
  '(("Apply" . custom-face-apply)
    ("Set Default" . custom-face-set-default)
    ("Default" . custom-face-default)
    ("Factory" . custom-face-factory))
  "Alist of actions for the `custom-face' widget.
The key is a string containing the name of the action, the value is a
lisp function taking the widget as an element which will be called
when the action is chosen.")

(defun custom-face-action (widget &optional event)
  "Show the menu for `custom-face' WIDGET.
Optional EVENT is the location for the menu."
  (let* ((completion-ignore-case t)
	 (answer (widget-choose (symbol-name (widget-get widget :value))
				custom-face-menu
			       event)))
    (if answer
	(funcall answer widget))))

(defun custom-face-apply (widget)
  "Make the face attributes in WIDGET take effect."
  (unless (widget-get widget :custom-show)
    (error "You cannot apply hidden face"))
  (let* ((symbol (widget-value widget))
	 (child (car (widget-get widget :children)))
	 (value (widget-value child)))
    (custom-face-display-set symbol value)))

(defun custom-face-set-default (widget)
  "Make the face attributes in WIDGET default."
  (unless (widget-get widget :custom-show)
    (error "You cannot set default for hidden face"))
  (let* ((symbol (widget-value widget))
	 (child (car (widget-get widget :children)))
	 (value (widget-value child)))
    (put symbol 'saved-face value)))

(defun custom-face-default (widget)
  "Restore WIDGET to the face's default attributes."
  (unless (widget-get widget :custom-show)
    (error "You cannot reset to default for hidden face"))
  (let* ((symbol (widget-value widget))
	 (child (car (widget-get widget :children))))
    (widget-value-set child (or (get symbol 'saved-face)
				 (get symbol 'factory-face)))
    (widget-setup)))

(defun custom-face-factory (widget)
  "Restore WIDGET to the face's factory settings."
  (unless (widget-get widget :custom-show)
    (error "You cannot reset to factory setting for hidden face"))
  (let* ((symbol (widget-value widget))
	 (child (car (widget-get widget :children))))
    (widget-value-set child (or (get symbol 'factory-face)
				 (get symbol 'saved-face)))
    (widget-setup)))

;;; The `face' Widget.

(define-widget 'face 'default
  "Select and customize a face."
  :convert-widget 'widget-item-convert-widget
  :custom-show nil
  :format "%[%t%]\n%v"
  :value-create 'widget-face-value-create
  :value-delete 'widget-radio-value-delete
  :value-get 'widget-item-value-get
  :validate 'widget-repeat-validate
  :action 'widget-face-action
  :match '(lambda (widget value) (symbolp value)))

(defun widget-face-value-create (widget)
  ;; Create a `custom-face' child.
  (let* ((symbol (widget-value widget))
	 (child (widget-create 'custom-face
			       :custom-level nil
			       :custom-show t
			       :tag "Face"
			       :value symbol)))
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
      (widget-setup))))

;;; The `custom-group' Widget.

(define-widget 'custom-group 'custom
  "Customize group."
  :format "%l%h%[%t%]:\n%x%v"
  :custom-documentation-property 'group-documentation
  :help-echo "Push me to set or reset all members of this group."
  :value-create 'custom-group-value-create
  :action 'custom-group-action)

(defun custom-group-value-create (widget)
  (let* ((show (widget-get widget :custom-show))
	 (level (widget-get widget :custom-level))
	 (symbol (widget-value widget))
	 (members (get symbol 'custom-group)))
    (when show
      (widget-put widget
		  :children (mapcar (lambda (entry)
				      (widget-insert "\n")
				      (prog1
					  (widget-create (nth 1 entry)
							 :parent widget
							 :custom-level 
							 (1+ level)
							 :value (nth 0 entry))
					(unless (eq (preceding-char) ?\n)
					  (widget-insert "\n"))))
				    members)))))

(defvar custom-group-menu 
  '(("Apply" . custom-unimplemented)
    ("Set Default" . custom-unimplemented)
    ("Reset" . custom-reset)
    ("Reset to Default" . custom-unimplemented)
    ("Reset to Factory Settings" . custom-unimplemented))
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

;;; The `custom-save' Command.

(defcustom custom-file "~/.emacs"
  :type 'file
  :group 'customize
  "File used for storing customization information.")

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

(defun custom-mode ()
  "Major mode for editing customization buffers.

Read the non-existing manual for information about how to use it.

\\{custom-mode-map}"
  (kill-all-local-variables)
  (setq major-mode 'custom-mode
	mode-name "Custom")
  (use-local-map custom-mode-map)
  (make-local-variable 'custom-options))

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
(defun customize-apropos (regexp)
  "Customize all user options matching REGEXP"
  (interactive "sCustomize regexp: ")
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
				 (get symbol 'variable-documentation)))
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
  (switch-to-buffer-other-window (get-buffer-create "*Customization*"))
  (custom-mode)
  (widget-insert "This is a customization buffer. 
Press `C-h m' for to get help.

")
  (setq custom-options 
	(mapcar (lambda (entry)
		  (widget-create (nth 1 entry)
				 :value (nth 0 entry))
		  (unless (eq (preceding-char) ?\n)
		    (widget-insert "\n"))
		  (widget-insert "\n"))
		options))
  (widget-create 'push 
		 :tag "Save"
		 :help-echo "Push me to store the new defaults permanently."
		 :action (lambda (widget &optional event) (custom-save)))
  (widget-setup))

;;; The End.

(provide 'custom-edit)

;; custom-edit.el ends here
