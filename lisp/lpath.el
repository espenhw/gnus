;; Shut up.

(defun maybe-fbind (args)
  (while args
    (or (fboundp (car args))
	(defalias (car args) 'ignore))
    (setq args (cdr args))))

(defun maybe-bind (args)
  (mapcar (lambda (var) (unless (boundp var) (set var nil))) args))

(maybe-fbind '(Info-directory
	       Info-menu bbdb-create-internal bbdb-records create-image
	       display-graphic-p display-time-event-handler find-coding-system
	       find-image image-size image-type-available-p insert-image
	       make-mode-line-mouse-map make-temp-file open-ssl-stream
	       propertize put-image replace-regexp-in-string
	       rmail-msg-is-pruned rmail-msg-restore-non-pruned-header
	       sort-coding-systems spam-BBDB-register-routine
	       spam-enter-ham-BBDB string-to-multibyte tool-bar-add-item
	       tool-bar-add-item-from-menu tool-bar-local-item-from-menu
	       url-http-file-exists-p vcard-pretty-print w32-focus-frame
	       w3m-charset-to-coding-system x-focus-frame))
(maybe-bind '(filladapt-mode
	      mc-pgp-always-sign mm-w3m-mode-map rmail-enable-mime-composing
	      rmail-insert-mime-forwarded-message-function
	      w3m-cid-retrieve-function-alist w3m-current-buffer
	      w3m-display-inline-images w3m-meta-content-type-charset-regexp
	      w3m-minor-mode-map))

(if (featurep 'xemacs)
    (progn
      (maybe-fbind '(Info-directory
		     Info-menu ccl-execute-on-string char-charset charsetp
		     coding-system-get coding-system-list coding-system-p
		     decode-coding-region decode-coding-string
		     define-ccl-program delete-overlay detect-coding-region
		     encode-coding-region encode-coding-string
		     event-click-count event-end event-start
		     find-charset-region find-coding-systems-for-charsets
		     find-coding-systems-region find-coding-systems-string
		     get-charset-property mail-abbrevs-setup
		     mouse-minibuffer-check mouse-movement-p mouse-scroll-subr
		     overlay-lists pgg-parse-crc24-string posn-point
		     posn-window read-event set-buffer-multibyte track-mouse
		     window-edges w3m-region))
      (maybe-bind '(adaptive-fill-first-line-regexp
		    buffer-display-table buffer-file-coding-system
		    current-language-environment
		    default-enable-multibyte-characters
		    enable-multibyte-characters gnus-agent-expire-current-dirs
		    language-info-alist mark-active mouse-selection-click-count
		    mouse-selection-click-count-buffer pgg-parse-crc24
		    temporary-file-directory transient-mark-mode
		    w3-meta-content-type-charset-regexp
		    w3-meta-charset-content-type-regexp)))
  (maybe-fbind '(bbdb-complete-name
		 delete-annotation device-connection dfw-device
		 events-to-keys font-lock-set-defaults frame-device
		 glyph-height glyph-width mail-aliases-setup make-annotation
		 make-event make-glyph make-network-process map-extents
		 message-xmas-redefine set-extent-property temp-directory
		 url-generic-parse-url url-insert-file-contents
		 valid-image-instantiator-format-p
		 w3-coding-system-for-mime-charset w3-do-setup
		 w3-prepare-buffer w3-region w3m-region window-pixel-height
		 window-pixel-width))
  (maybe-bind '(help-echo-owns-message
		mail-mode-hook
		url-current-object url-package-name url-package-version
		w3-meta-charset-content-type-regexp
		w3-meta-content-type-charset-regexp)))

(when (and (featurep 'xemacs)
	   (not (featurep 'mule)))
  (progn
    (maybe-fbind '(coding-system-base find-charset-string))))


(defun nnkiboze-score-file (a)
  )

(defun split-line (&optional arg)
  )

(provide 'lpath)
