;; Shut up.

(defvar byte-compile-default-warnings)

(defun maybe-fbind (args)
  (while args
    (or (fboundp (car args))
	(fset (car args) 'ignore))
    (setq args (cdr args))))

(defun maybe-bind (args)
  (mapcar (lambda (var) (unless (boundp var) (set var nil))) args))

(if (string-match "XEmacs" emacs-version)
    (progn
      (defvar track-mouse nil)
      (maybe-fbind '(posn-point
		     event-start x-popup-menu
		     facemenu-get-face window-at coordinates-in-window-p
		     compute-motion x-defined-colors easy-menu-create-keymaps
		     read-event internal-find-face internal-next-face-id
		     make-face-internal set-frame-face-alist frame-face-alist
		     facemenu-add-new-face make-face-x-resource-internal
		     set-font-size set-font-family posn-window
		     run-with-idle-timer mouse-minibuffer-check window-edges
		     event-click-count track-mouse read-event mouse-movement-p
		     event-end mouse-scroll-subr overlay-lists delete-overlay
		     set-face-stipple mail-abbrevs-setup char-int
		     make-char-table set-char-table-range font-create-object
		     x-color-values widget-make-intangible error-message-string
		     w3-form-encode-xwfu
		     ))
      (maybe-bind '(global-face-data
		    mark-active transient-mark-mode mouse-selection-click-count
		    mouse-selection-click-count-buffer buffer-display-table
		    font-lock-defaults user-full-name user-login-name
		    gnus-newsgroup-name gnus-article-x-face-too-ugly)))
  (defvar browse-url-browser-function nil)
  (maybe-fbind '(color-instance-rgb-components
		 make-color-instance color-instance-name specifier-instance
		 device-type device-class get-popup-menu-response event-object
		 x-defined-colors read-color add-submenu set-font-family
		 font-create-object set-font-size frame-device find-face
		 set-extent-property make-extent characterp display-error
		 set-face-doc-string frame-property face-doc-string
		 button-press-event-p next-command-event
		 widget-make-intangible glyphp make-glyph set-glyph-image
		 set-glyph-property event-glyph glyph-property event-point
		 device-on-window-system-p make-gui-button Info-goto-node
		 pp-to-string color-name)))

(setq load-path (cons "." load-path))
(require 'custom)

(provide 'lpath)
