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
	       replace-regexp-in-string
	       bbdb-complete-name
	       display-time-event-handler
	       find-image font-create-object gnus-mule-get-coding-system
	       font-lock-set-defaults
	       find-coding-systems-string
	       image-size image-type-available-p insert-image
	       image-type-from-file-header
	       make-temp-file message-xmas-redefine
	       mail-aliases-setup mm-copy-tree
	       mule-write-region-no-coding-system put-image
	       ring-elements
	       charsetp sort-coding-systems
	       coding-system-p coding-system-list
	       propertize make-mode-line-mouse2-map
	       frames-on-display-list
	       make-mode-line-mouse-map
	       rmail-select-summary rmail-summary-exists rmail-update-summary
	       rmail-toggle-header
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
	      enable-multibyte-characters
	      display-time-mail-function imap-password mail-mode-hook
	      filladapt-mode
	      mc-pgp-always-sign
	      gpg-unabbrev-trust-alist
	      nnoo-definition-alist
	      current-language-environment   
	      language-info-alist
	      url-current-callback-func url-be-asynchronous
	      url-current-callback-data url-working-buffer
	      url-current-mime-headers w3-meta-charset-content-type-regexp
	      rmail-enable-mime-composing 
	      rmail-insert-mime-forwarded-message-function 
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
		     overlays-in
		     overlay-start posn-point posn-window
		     read-event read-event run-with-idle-timer
		     set-buffer-multibyte set-char-table-range
		     set-face-stipple set-frame-face-alist track-mouse
		     url-retrieve w3-form-encode-xwfu window-at
		     window-edges x-color-values x-popup-menu browse-url
		     frame-char-height frame-char-width
		     url-generic-parse-url xml-parse-region))
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
		 button-press-event-p characterp color-instance-name
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
		 window-pixel-height window-pixel-width
		 xml-parse-region)))

(require 'custom)

(defun md5 (a &optional b c)
  )

(defun nnkiboze-score-file (a)
)

(provide 'lpath)
