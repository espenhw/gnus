;;; widget-example.el -- example of using the widget library

;; Copyright (C) 1996 Free Software Foundation, Inc.
;;
;; Author: Per Abrahamsen <abraham@dina.kvl.dk>
;; Keywords: help, extensions, faces, hypermedia
;; Version: 0.9
;; X-URL: http://www.dina.kvl.dk/~abraham/custom/

(require 'widget)

(eval-when-compile
  (require 'widget-edit))

(defvar widget-example-repeat)

(defun widget-example ()
  "Create the widgets from the Widget manual."
  (interactive)
  (switch-to-buffer "*Widget Example*")
  (kill-all-local-variables)
  (make-local-variable 'widget-example-repeat)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (widget-insert "Here is some documentation.\n\nName: ")
  (widget-create 'field
		 :size 13
		 "My Name")
  (widget-create 'choice
		 :tag "Choose"
		 :value "This"
		 :help-echo "Choose me, please!"
		 :notify (lambda (widget &rest ignore)
			   (message "%s is a good choice!"
				    (widget-value widget)))
		 '(item :tag "This option" :value "This")
		 '(choice-item "That option")
		 '(field :menu-tag "No option" "Thus option"))
  (widget-insert "Address: ")
  (widget-create 'field
		 "Some Place\nIn some City\nSome country.")
  (widget-insert "\nSee also ")
  (widget-create 'link
		 :notify (lambda (&rest ignore)
			   (widget-value-set widget-example-repeat 
					     '("En" "To" "Tre"))
			   (widget-setup))
		 "other work")
  (widget-insert " for more information.\n\nNumbers: count to three below\n")
  (setq widget-example-repeat
	(widget-create 'repeat
		       :entry-format "%i %d %v"
		       :notify (lambda (widget &rest ignore)
				 (let ((old (widget-get widget
							':example-length))
				       (new (length (widget-value widget))))
				   (unless (eq old new)
				     (widget-put widget ':example-length new)
				     (message "You can count to %d." new))))
		       :value '("One" "Eh, two?" "Five!")
		       '(field :value "three")))
  (widget-insert "\n\nSelect multiple:\n\n")
  (widget-create 'checkbox t)
  (widget-insert " This\n")
  (widget-create 'checkbox nil)
  (widget-insert " That\n")
  (widget-create 'checkbox
		 :notify (lambda (&rest ignore) (message "Tickle"))
		 t)
  (widget-insert " Thus\n\nSelect one:\n\n")
  (widget-create 'radio
		 :value "One"
		 :notify (lambda (widget &rest ignore)
			   (message "You selected %s"
				    (widget-value widget)))
		 '(item "One") '(item "Anthor One.") '(item "A Final One."))
  (widget-insert "\n")
  (widget-create 'push
		 :notify (lambda (&rest ignore) 
			   (if (= (length (widget-value widget-example-repeat))
				  3)
			       (message "Congratulation!")
			     (error "Three was the count!")))
		 "Apply Form")
  (widget-insert " ")
  (widget-create 'push
		 :notify (lambda (&rest ignore)
			   (widget-example))
		 "Reset Form")
  (widget-insert "\n")
  (use-local-map widget-keymap)
  (widget-setup))
