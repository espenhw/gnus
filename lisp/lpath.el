;; Shut up.

(defvar byte-compile-default-warnings)

(defun maybe-fbind (args)
  (while args
    (or (fboundp (car args))
	(defalias (car args) 'ignore))
    (setq args (cdr args))))

(defun maybe-bind (args)
  (mapcar (lambda (var) (unless (boundp var) (set var nil))) args))

(maybe-fbind '(babel-fetch
	       babel-wash create-image decode-coding-string display-graphic-p
	       find-image font-create-object gnus-mule-get-coding-system
	       font-lock-set-defaults
	       image-size image-type-available-p insert-image
	       make-temp-file message-xmas-redefine
	       mail-aliases-setup mm-copy-tree
	       mule-write-region-no-coding-system put-image
	       ring-elements
	       rmail-select-summary rmail-summary-exists rmail-update-summary
	       sc-cite-regexp set-font-family set-font-size temp-directory
	       string-as-multibyte
	       tool-bar-add-item tool-bar-add-item-from-menu
	       url-view-url vcard-pretty-print
	       url-insert-file-contents
	       w3-coding-system-for-mime-charset w3-prepare-buffer w3-region
	       widget-make-intangible x-defined-colors))

(maybe-bind '(adaptive-fill-first-line-regexp
	      adaptive-fill-regexp babel-history babel-translations
	      default-enable-multibyte-characters
	      display-time-mail-function imap-password mail-mode-hook
	      mc-pgp-always-sign
	      nnoo-definition-alist
	      url-current-callback-func url-be-asynchronous 
	      url-current-callback-data url-working-buffer
	      url-current-mime-headers w3-meta-charset-content-type-regexp
	      w3-meta-content-type-charset-regexp))

(if (featurep 'xemacs)
    (progn
      (defvar track-mouse nil)
      (maybe-fbind '(char-charset
		     coding-system-get compute-motion coordinates-in-window-p
		     delete-overlay easy-menu-create-keymaps
		     error-message-string event-click-count event-end
		     event-start facemenu-add-new-face facemenu-get-face
		     find-charset-region find-coding-systems-for-charsets
		     find-coding-systems-region find-non-ascii-charset-region
		     frame-face-alist get-charset-property internal-find-face
		     internal-next-face-id mail-abbrevs-setup make-char-table
		     make-face-internal make-face-x-resource-internal
		     make-overlay mouse-minibuffer-check mouse-movement-p
		     mouse-scroll-subr overlay-buffer overlay-end
		     overlay-get overlay-lists overlay-put
		     overlay-start posn-point posn-window
		     read-event read-event run-with-idle-timer
		     set-buffer-multibyte set-char-table-range
		     set-face-stipple set-frame-face-alist track-mouse
		     url-retrieve w3-form-encode-xwfu window-at
		     window-edges x-color-values x-popup-menu browse-url
		     frame-char-height frame-char-width))
      (maybe-bind '(buffer-display-table 
		    buffer-file-coding-system font-lock-defaults
		    global-face-data gnus-article-x-face-too-ugly
		    gnus-newsgroup-charset gnus-newsgroup-emphasis-alist
		    gnus-newsgroup-name mark-active
		    mouse-selection-click-count
		    mouse-selection-click-count-buffer
		    temporary-file-directory transient-mark-mode
		    url-current-mime-type
		    user-full-name user-login-name
		    w3-image-mappings)))
  (maybe-bind '(browse-url-browser-function
		enable-multibyte-characters help-echo-owns-message))
  (maybe-fbind '(Info-goto-node
		 add-submenu annotation-glyph annotationp babel-as-string
		 button-press-event-p char-int characterp color-instance-name
		 color-instance-rgb-components color-name delete-annotation
		 device-class device-on-window-system-p device-type
		 display-error event-glyph event-object event-point
		 events-to-keys face-doc-string find-face frame-device
		 frame-property get-popup-menu-response glyph-height
		 glyph-property glyph-width glyphp make-annotation
		 make-event
		 make-color-instance make-extent make-glyph make-gui-button
		 make-image-specifier map-extents next-command-event
		 pp-to-string read-color set-extent-property
		 set-face-doc-string set-glyph-image set-glyph-property
		 specifier-instance url-generic-parse-url
		 valid-image-instantiator-format-p w3-do-setup
		 window-pixel-height window-pixel-width)))

(require 'custom)

(defun md5 (a &optional b c)
  )

(defun nnkiboze-score-file (a)
)

(provide 'lpath)
