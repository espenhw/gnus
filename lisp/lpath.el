;; Shut up.

(defvar byte-compile-default-warnings)

(defun maybe-fbind (args)
  (while args 
    (or (fboundp (car args))
	(fset (car args) 'ignore))
    (setq args (cdr args))))

(if (string-match "XEmacs" emacs-version)
    (progn 
      (defvar track-mouse nil)
      (maybe-fbind '(posn-point event-start x-popup-menu
		     facemenu-get-face window-at
		     coordinates-in-window-p compute-motion
		     x-defined-colors easy-menu-create-keymaps)) 
      ;; XEmacs thinks writting compatible code is obsolete.
      (require 'bytecomp)
      (setq byte-compile-default-warnings
	    (delq 'obsolete byte-compile-default-warnings)))
  (defvar browse-url-browser-function nil)
  (maybe-fbind '(color-instance-rgb-components make-color-instance
	         color-instance-name specifier-instance device-type
		 device-class get-popup-menu-response event-object
		 x-defined-colors read-color add-submenu set-font-family
		 font-create-object set-font-size frame-device find-face
		 set-extent-property make-extent characterp display-error)))

(setq load-path (cons "." load-path))
(require 'custom)

(provide 'lpath)
