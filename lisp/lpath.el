;; Shut up.

(defun maybe-fbind (args)
  (while args
    (or (fboundp (car args))
	(defalias (car args) 'ignore))
    (setq args (cdr args))))

(defun maybe-bind (args)
  (mapcar (lambda (var) (unless (boundp var) (set var nil))) args))

(unless (featurep 'xemacs)
  (maybe-fbind '(pgg-display-output-buffer url-generic-parse-url))
  (maybe-bind '(help-xref-stack-item
		url-version w3-meta-charset-content-type-regexp
		w3-meta-content-type-charset-regexp))

  (when (<= emacs-major-version 22)
    (defun ecomplete-add-item (type key text))
    (defun ecomplete-save nil)
    (defun hashcash-wait-async (&optional buffer))
    (defun mail-add-payment (&optional arg async))
    (defun mail-add-payment-async (&optional arg))
    (defun netrc-get (alist type))
    (defun netrc-machine (list machine &optional port defaultport))
    (defun netrc-machine-user-or-password (mode authinfo-file-or-list machines
						ports defaults))
    (defun netrc-parse (file))
    (defun nnkiboze-score-file (a))
    (maybe-fbind
     '(Info-index
       Info-index-next Info-menu bbdb-complete-name bookmark-default-handler
       bookmark-get-bookmark-record bookmark-make-record-default
       bookmark-prop-get display-time-event-handler epg-check-configuration
       find-coding-system frame-device recenter-top-bottom
       rmail-swap-buffers-maybe w3-do-setup w3-prepare-buffer w3-region
       w32-focus-frame w3m-detect-meta-charset w3m-region))
    (maybe-bind
     '(w3m-link-map)))

  (when (= emacs-major-version 21)
    (defun canlock-insert-header (&optional id-for-key id-for-lock password))
    (defun split-line (&optional arg))
    (maybe-fbind
     '(clear-string
       coding-system-aliasee coding-system-from-name custom-autoload
       delete-annotation delete-extent device-connection dfw-device
       events-to-keys find-face font-lock-set-defaults get-char-table
       glyph-height glyph-width help-buffer int-to-char ldap-search-entries
       mail-aliases-setup make-annotation make-event make-glyph
       make-network-process map-extents message-xmas-redefine put-char-table
       run-mode-hooks set-extent-property set-itimer-function
       set-keymap-default-binding temp-directory time-to-seconds ucs-to-char
       unicode-precedence-list unicode-to-char url-generic-parse-url
       url-http-file-exists-p valid-image-instantiator-format-p
       vcard-pretty-print w3-coding-system-for-mime-charset window-pixel-height
       window-pixel-width))
    (maybe-bind
     '(eudc-protocol
       filladapt-mode help-echo-owns-message itimer-list ps-print-color-p
       smtpmail-default-smtp-server w3-meta-charset-content-type-regexp
       w3-meta-content-type-charset-regexp))))

(when (featurep 'xemacs)
  (defun canlock-insert-header (&optional id-for-key id-for-lock password))
  (defun ecomplete-add-item (type key text))
  (defun ecomplete-save nil)
  (defun hashcash-wait-async (&optional buffer))
  (defun mail-add-payment (&optional arg async))
  (defun mail-add-payment-async (&optional arg))
  (defun netrc-get (alist type))
  (defun netrc-machine (list machine &optional port defaultport))
  (defun netrc-machine-user-or-password (mode authinfo-file-or-list machines
					      ports defaults))
  (defun netrc-parse (file))
  (defun nnkiboze-score-file (a))
  (defun split-line (&optional arg))
  (eval-after-load "rmail"
    '(defun rmail-toggle-header (&optional arg)))
  (maybe-fbind
   '(bookmark-default-handler
     bookmark-get-bookmark-record bookmark-make-record-default
     bookmark-prop-get clear-string codepage-setup coding-system-from-name
     cp-supported-codepages create-image delete-overlay detect-coding-string
     display-time-event-handler epg-check-configuration event-click-count
     event-end event-start find-coding-systems-for-charsets
     find-coding-systems-region find-coding-systems-string find-image
     float-time help-buffer image-size image-type-available-p insert-image
     mail-abbrevs-setup make-mode-line-mouse-map make-network-process
     mouse-minibuffer-check mouse-movement-p mouse-scroll-subr overlay-lists
     pgg-display-output-buffer posn-point posn-window put-image read-event
     recenter-top-bottom rmail-msg-restore-non-pruned-header
     rmail-swap-buffers-maybe select-safe-coding-system sort-coding-systems
     track-mouse ucs-to-char url-generic-parse-url url-http-file-exists-p
     url-insert-file-contents vcard-pretty-print w3m-detect-meta-charset
     w3m-region window-edges))
  (maybe-bind
   '(adaptive-fill-first-line-regexp
     buffer-display-table cursor-in-non-selected-windows
     default-enable-multibyte-characters default-file-name-coding-system
     eudc-protocol filladapt-mode gnus-agent-expire-current-dirs
     help-xref-stack-item idna-program installation-directory
     line-spacing mark-active mouse-selection-click-count
     mouse-selection-click-count-buffer ps-print-color-p rmail-default-file
     rmail-default-rmail-file rmail-insert-mime-forwarded-message-function
     show-trailing-whitespace smtpmail-default-smtp-server
     temporary-file-directory tool-bar-mode transient-mark-mode url-version
     w3-meta-charset-content-type-regexp w3m-link-map
     w3-meta-content-type-charset-regexp))

  (when (or (and (= emacs-major-version 21) (= emacs-minor-version 4))
	    (featurep 'sxemacs))
    (maybe-fbind
     '(custom-autoload
       decode-char display-graphic-p display-images-p display-visual-class
       get-display-table make-temp-file multibyte-string-p
       next-single-char-property-change put-display-table
       select-frame-set-input-focus set-buffer-multibyte string-as-multibyte
       timer-set-function unicode-precedence-list unicode-to-char
       w32-focus-frame x-focus-frame))
    (maybe-bind
     '(scroll-margin
       timer-list)))

  (when (and (= emacs-major-version 21) (= emacs-minor-version 4))
    (maybe-fbind
     '(propertize)))

  (unless (featurep 'mule)
    (maybe-fbind
     '(ccl-execute-on-string
       char-charset charsetp coding-system-get find-charset-region
       get-charset-property pgg-display-output-buffer pgg-parse-crc24-string
       unicode-precedence-list))
    (maybe-bind
     '(current-language-environment
       language-info-alist pgg-parse-crc24)))

  (unless (featurep 'file-coding)
    (maybe-fbind
     '(coding-system-aliasee
       coding-system-base coding-system-change-eol-conversion coding-system-list
       coding-system-p decode-coding-region decode-coding-string
       detect-coding-region encode-coding-region encode-coding-string
       find-coding-system))
    (maybe-bind
     '(buffer-file-coding-system
       coding-system-for-read coding-system-for-write
       enable-multibyte-characters file-name-coding-system))))

(provide 'lpath)

;;; arch-tag: d1ad864f-dca6-4d21-aa3f-be3248e66dba
