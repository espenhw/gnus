;;; mm-decode.el --- Functions for decoding MIME things
;; Copyright (C) 1998 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;;	MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(require 'mail-parse)
(require 'mailcap)
(require 'mm-bodies)

;;; Convenience macros.

(defmacro mm-handle-buffer (handle)
  `(nth 0 ,handle))
(defmacro mm-handle-type (handle)
  `(nth 1 ,handle))
(defmacro mm-handle-encoding (handle)
  `(nth 2 ,handle))
(defmacro mm-handle-undisplayer (handle)
  `(nth 3 ,handle))
(defmacro mm-handle-set-undisplayer (handle function)
  `(setcar (nthcdr 3 ,handle) ,function))
(defmacro mm-handle-disposition (handle)
  `(nth 4 ,handle))
(defmacro mm-handle-description (handle)
  `(nth 5 ,handle))
(defmacro mm-handle-cache (handle)
  `(nth 6 ,handle))
(defmacro mm-handle-set-cache (handle contents)
  `(setcar (nthcdr 6 ,handle) ,contents))
(defmacro mm-make-handle (&optional buffer type encoding undisplayer
				    disposition description cache)
  `(list ,buffer ,type ,encoding ,undisplayer
	 ,disposition ,description ,cache))

(defvar mm-inline-media-tests
  '(("image/jpeg" mm-inline-image
     (and window-system (featurep 'jpeg) (mm-image-fit-p handle)))
    ("image/png" mm-inline-image
     (and window-system (featurep 'png) (mm-image-fit-p handle)))
    ("image/gif" mm-inline-image
     (and window-system (featurep 'gif) (mm-image-fit-p handle)))
    ("image/tiff" mm-inline-image
     (and window-system (featurep 'tiff) (mm-image-fit-p handle)))
    ("image/xbm" mm-inline-image
     (and window-system (fboundp 'device-type)
	  (eq (device-type) 'x)))
    ("image/x-xbitmap" mm-inline-image
     (and window-system (fboundp 'device-type)
	  (eq (device-type) 'x)))
    ("image/xpm" mm-inline-image
     (and window-system (featurep 'xpm)))
    ("image/x-pixmap" mm-inline-image
     (and window-system (featurep 'xpm)))
    ("image/bmp" mm-inline-image
     (and window-system (featurep 'bmp)))
    ("text/plain" mm-inline-text t)
    ("text/enriched" mm-inline-text t)
    ("text/richtext" mm-inline-text t)
    ("text/html" mm-inline-text (locate-library "w3"))
    ("message/delivery-status" mm-inline-text t)
    ("audio/wav" mm-inline-audio
     (and (or (featurep 'nas-sound) (featurep 'native-sound))
	  (device-sound-enabled-p)))
    ("audio/au" mm-inline-audio
     (and (or (featurep 'nas-sound) (featurep 'native-sound))
	  (device-sound-enabled-p)))
    ("multipart/alternative" ignore t)
    ("multipart/mixed" ignore t)
    ("multipart/related" ignore t))
  "Alist of media types/test that say whether the media types can be displayed inline.")

(defvar mm-user-display-methods
  '(("image/.*" . inline)
    ("text/.*" . inline)
    ("message/delivery-status" . inline)))

(defvar mm-user-automatic-display
  '("text/plain" "text/enriched" "text/richtext" "text/html" 
    "image/.*" "message/delivery-status" "multipart/.*"))

(defvar mm-user-automatic-external-display nil
  "List of MIME type regexps that will be displayed externally automatically.")

(defvar mm-alternative-precedence
  '("multipart/related" "multipart/mixed" "multipart/alternative"
    "image/jpeg" "image/gif" "text/html" "text/enriched"
    "text/richtext" "text/plain")
  "List that describes the precedence of alternative parts.")

(defvar mm-tmp-directory "/tmp/"
  "Where mm will store its temporary files.")

(defvar mm-all-images-fit nil
  "If non-nil, then all images fit in the buffer.")

;;; Internal variables.

(defvar mm-dissection-list nil)
(defvar mm-last-shell-command "")
(defvar mm-content-id-alist nil)

;;; The functions.

(defun mm-dissect-buffer (&optional no-strict-mime)
  "Dissect the current buffer and return a list of MIME handles."
  (save-excursion
    (let (ct ctl type subtype cte cd description id result)
      (save-restriction
	(mail-narrow-to-head)
	(when (or no-strict-mime
		  (mail-fetch-field "mime-version"))
	  (setq ct (mail-fetch-field "content-type")
		ctl (condition-case () (mail-header-parse-content-type ct)
		      (error nil))
		cte (mail-fetch-field "content-transfer-encoding")
		cd (mail-fetch-field "content-disposition")
		description (mail-fetch-field "content-description")
		id (mail-fetch-field "content-id"))))
      (if (not ctl)
	  (mm-dissect-singlepart
	   '("text/plain") nil no-strict-mime
	   (and cd (condition-case ()
		       (mail-header-parse-content-disposition cd)
		     (error nil)))
	   description)
	(setq type (split-string (car ctl) "/"))
	(setq subtype (cadr type)
	      type (pop type))
	(setq
	 result
	 (cond
	  ((equal type "multipart")
	   (cons (car ctl) (mm-dissect-multipart ctl)))
	  (t
	   (mm-dissect-singlepart
	    ctl
	    (and cte (intern (downcase (mail-header-remove-whitespace
					(mail-header-remove-comments
					 cte)))))
	    no-strict-mime
	    (and cd (condition-case ()
			(mail-header-parse-content-disposition cd)
		      (error nil)))
	    description))))
	(when id
	  (when (string-match " *<\\(.*\\)> *" id)
	    (setq id (match-string 1 id)))
	  (push (cons id result) mm-content-id-alist))
	result))))

(defun mm-dissect-singlepart (ctl cte &optional force cdl description)
  (when (or force
	    (not (equal "text/plain" (car ctl))))
    (let ((res (mm-make-handle
		(mm-copy-to-buffer) ctl cte nil cdl description)))
      (push (car res) mm-dissection-list)
      res)))

(defun mm-remove-all-parts ()
  "Remove all MIME handles."
  (interactive)
  (mapcar 'mm-remove-part mm-dissection-list)
  (setq mm-dissection-list nil))

(defun mm-dissect-multipart (ctl)
  (goto-char (point-min))
  (let* ((boundary (concat "\n--" (mail-content-type-get ctl 'boundary)))
	(close-delimiter (concat (regexp-quote boundary) "--[ \t]*$"))
	start parts 
	(end (save-excursion	
	       (goto-char (point-max))
	       (if (re-search-backward close-delimiter nil t)
		   (match-beginning 0)
		 (point-max)))))
    (while (search-forward boundary end t)
      (goto-char (match-beginning 0))
      (when start
	(save-excursion
	  (save-restriction
	    (narrow-to-region start (point))
	    (setq parts (nconc (list (mm-dissect-buffer t)) parts)))))
      (forward-line 2)
      (setq start (point)))
    (when start
      (save-excursion
	(save-restriction
	  (narrow-to-region start end)
	  (setq parts (nconc (list (mm-dissect-buffer t)) parts)))))
    (nreverse parts)))

(defun mm-copy-to-buffer ()
  "Copy the contents of the current buffer to a fresh buffer."
  (save-excursion
    (let ((obuf (current-buffer))
	  beg)
      (goto-char (point-min))
      (search-forward-regexp "^\n" nil t)
      (setq beg (point))
      (set-buffer (generate-new-buffer " *mm*"))
      (insert-buffer-substring obuf beg)
      (current-buffer))))

(defun mm-inlinable-part-p (type)
  "Say whether TYPE can be displayed inline."
  (eq (mm-user-method type) 'inline))

(defun mm-display-part (handle &optional no-default)
  "Display the MIME part represented by HANDLE.
Returns nil if the part is removed; inline if displayed inline;
external if displayed external."
  (save-excursion
    (mailcap-parse-mailcaps)
    (if (mm-handle-displayed-p handle)
	(mm-remove-part handle)
      (let* ((type (car (mm-handle-type handle)))
	     (method (mailcap-mime-info type))
	     (user-method (mm-user-method type)))
	(if (eq user-method 'inline)
	    (progn
	      (forward-line 1)
	      (mm-display-inline handle)
	      'inline)
	  (when (or user-method
		    method
		    (not no-default))
	    (if (and (not user-method)
		     (not method)
		     (equal "text" (car (split-string type))))
		(progn
		  (forward-line 1)
		  (mm-insert-inline handle (mm-get-part handle))
		  'inline)
	      (mm-display-external
	       handle (or user-method method
			  'mailcap-save-binary-file))
	      'external)))))))

(defun mm-display-external (handle method)
  "Display HANDLE using METHOD."
  (mm-with-unibyte-buffer
    (if (functionp method)
	(let ((cur (current-buffer)))
	  (if (eq method 'mailcap-save-binary-file)
	      (progn
		(set-buffer (generate-new-buffer "*mm*"))
		(setq method nil))
	    (insert-buffer-substring (mm-handle-buffer handle))
	    (mm-decode-content-transfer-encoding
	     (mm-handle-encoding handle) (car (mm-handle-type handle)))
	    (let ((win (get-buffer-window cur t)))
	      (when win
		(select-window win)))
	    (switch-to-buffer (generate-new-buffer "*mm*")))
	  (buffer-disable-undo)
	  (mm-set-buffer-file-coding-system mm-binary-coding-system)
	  (insert-buffer-substring cur)
	  (message "Viewing with %s" method)
	  (let ((mm (current-buffer)))
	    (unwind-protect
		(if method
		    (funcall method)
		  (mm-save-part handle))
	      (mm-handle-set-undisplayer handle mm))))
      ;; The function is a string to be executed.
      (insert-buffer-substring (mm-handle-buffer handle))
      (mm-decode-content-transfer-encoding
       (mm-handle-encoding handle) (car (mm-handle-type handle)))
      (let* ((dir (make-temp-name (expand-file-name "emm." mm-tmp-directory)))
	     (filename (mail-content-type-get
			(mm-handle-disposition handle) 'filename))
	     (needsterm (assoc "needsterm"
			       (mailcap-mime-info
				(car (mm-handle-type handle)) t)))
	     process file)
	;; We create a private sub-directory where we store our files.
	(make-directory dir)
	(set-file-modes dir 448)
	(if filename
	    (setq file (expand-file-name (file-name-nondirectory filename)
					 dir))
	  (setq file (make-temp-name (expand-file-name "mm." dir))))
	(write-region (point-min) (point-max) file nil 'nomesg)
	(message "Viewing with %s" method)
	(unwind-protect
	    (setq process
		  (if needsterm
		      (start-process "*display*" nil
				     "xterm"
				     "-e" shell-file-name "-c"
				     (format method
					     (mm-quote-arg file)))
		    (start-process "*display*" (generate-new-buffer "*mm*")
				   shell-file-name
				   "-c" (format method
						(mm-quote-arg file)))))
	  (mm-handle-set-undisplayer handle (cons file process)))
	(message "Displaying %s..." (format method file))))))

(defun mm-remove-parts (handles)
  "Remove the displayed MIME parts represented by HANDLE."
  (if (and (listp handles)
	   (bufferp (car handles)))
      (mm-remove-part handles)
    (let (handle)
      (while (setq handle (pop handles))
	(cond
	 ((stringp handle)
	  )
	 ((and (listp handle)
	       (stringp (car handle)))
	  (mm-remove-parts (cdr handle)))
	 (t
	  (mm-remove-part handle)))))))

(defun mm-destroy-parts (handles)
  "Remove the displayed MIME parts represented by HANDLE."
  (if (and (listp handles)
	   (bufferp (car handles)))
      (mm-destroy-part handles)
    (let (handle)
      (while (setq handle (pop handles))
	(cond
	 ((stringp handle)
	  )
	 ((and (listp handle)
	       (stringp (car handle)))
	  (mm-destroy-parts (cdr handle)))
	 (t
	  (mm-destroy-part handle)))))))

(defun mm-remove-part (handle)
  "Remove the displayed MIME part represented by HANDLE."
  (when (listp handle)
    (let ((object (mm-handle-undisplayer handle)))
      (condition-case ()
	  (cond
	   ;; Internally displayed part.
	   ((mm-annotationp object)
	    (delete-annotation object))
	   ((or (functionp object)
		(and (listp object)
		     (eq (car object) 'lambda)))
	    (funcall object))
	   ;; Externally displayed part.
	   ((consp object)
	    (condition-case ()
		(delete-file (car object))
	      (error nil))
	    (condition-case ()
		(delete-directory (file-name-directory (car object)))
	      (error nil))
	    (condition-case ()
		(kill-process (cdr object))
	      (error nil)))
	   ((bufferp object)
	    (when (buffer-live-p object)
	      (kill-buffer object))))
	(error nil))
      (mm-handle-set-undisplayer handle nil))))

(defun mm-display-inline (handle)
  (let* ((type (car (mm-handle-type handle)))
	 (function (cadr (assoc type mm-inline-media-tests))))
    (funcall function handle)
    (goto-char (point-min))))

(defun mm-inlinable-p (type)
  "Say whether TYPE can be displayed inline."
  (let ((alist mm-inline-media-tests)
	test)
    (while alist
      (when (equal type (caar alist))
	(setq test (caddar alist)
	      alist nil)
	(setq test (eval test)))
      (pop alist))
    test))

(defun mm-user-method (type)
  "Return the user-defined method for TYPE."
  (let ((methods mm-user-display-methods)
	method result)
    (while (setq method (pop methods))
      (when (string-match (car method) type)
	(when (or (not (eq (cdr method) 'inline))
		  (mm-inlinable-p type))
	  (setq result (cdr method)
		methods nil))))
    result))

(defun mm-automatic-display-p (type)
  "Return the user-defined method for TYPE."
  (let ((methods mm-user-automatic-display)
	method result)
    (while (setq method (pop methods))
      (when (and (string-match method type)
		 (mm-inlinable-p type))
	(setq result t
	      methods nil)))
    result))

(defun mm-automatic-external-display-p (type)
  "Return the user-defined method for TYPE."
  (let ((methods mm-user-automatic-external-display)
	method result)
    (while (setq method (pop methods))
      (when (string-match method type)
	(setq result t
	      methods nil)))
    result))

(defun add-mime-display-method (type method)
  "Make parts of TYPE be displayed with METHOD.
This overrides entries in the mailcap file."
  (push (cons type method) mm-user-display-methods))

(defun mm-destroy-part (handle)
  "Destroy the data structures connected to HANDLE."
  (when (listp handle)
    (mm-remove-part handle)
    (when (buffer-live-p (mm-handle-buffer handle))
      (kill-buffer (mm-handle-buffer handle)))))

(defun mm-handle-displayed-p (handle)
  "Say whether HANDLE is displayed or not."
  (mm-handle-undisplayer handle))
  
(defun mm-quote-arg (arg)
  "Return a version of ARG that is safe to evaluate in a shell."
  (let ((pos 0) new-pos accum)
    ;; *** bug: we don't handle newline characters properly
    (while (setq new-pos (string-match "[;!`\"$\\& \t{} |()<>]" arg pos))
      (push (substring arg pos new-pos) accum)
      (push "\\" accum)
      (push (list (aref arg new-pos)) accum)
      (setq pos (1+ new-pos)))
    (if (= pos 0)
        arg
      (apply 'concat (nconc (nreverse accum) (list (substring arg pos)))))))

;;;
;;; Functions for outputting parts
;;;

(defun mm-get-part (handle)
  "Return the contents of HANDLE as a string."
  (mm-with-unibyte-buffer
    (insert-buffer-substring (mm-handle-buffer handle))
    (mm-decode-content-transfer-encoding
     (mm-handle-encoding handle)
     (car (mm-handle-type handle)))
    (buffer-string)))

(defvar mm-default-directory nil)

(defun mm-save-part (handle)
  "Write HANDLE to a file."
  (let* ((name (mail-content-type-get (mm-handle-type handle) 'name))
	 (filename (mail-content-type-get
		    (mm-handle-disposition handle) 'filename))
	 file)
    (when filename
      (setq filename (file-name-nondirectory filename)))
    (setq file
	  (read-file-name "Save MIME part to: "
			  (expand-file-name
			   (or filename name "")
			   (or mm-default-directory default-directory))))
    (setq mm-default-directory (file-name-directory file))
    (mm-with-unibyte-buffer
      (insert-buffer-substring (mm-handle-buffer handle))
      (mm-decode-content-transfer-encoding
       (mm-handle-encoding handle)
       (car (mm-handle-type handle)))
      (when (or (not (file-exists-p file))
		(yes-or-no-p (format "File %s already exists; overwrite? "
				     file)))
	;; Now every coding system is 100% binary within mm-with-unibyte-buffer
	;; Is text still special?
      (let ((coding-system-for-write
             (if (equal "text" (car (split-string
                                     (car (mm-handle-type handle)) "/")))
                 buffer-file-coding-system
               'binary)))
        (write-region (point-min) (point-max) file))))))

(defun mm-pipe-part (handle)
  "Pipe HANDLE to a process."
  (let* ((name (mail-content-type-get (mm-handle-type handle) 'name))
	 (command
	  (read-string "Shell command on MIME part: " mm-last-shell-command)))
    (mm-with-unibyte-buffer
      (insert-buffer-substring (mm-handle-buffer handle))
      (mm-decode-content-transfer-encoding
       (mm-handle-encoding handle)
       (car (mm-handle-type handle)))
      (shell-command-on-region (point-min) (point-max) command nil))))

(defun mm-interactively-view-part (handle)
  "Display HANDLE using METHOD."
  (let* ((type (car (mm-handle-type handle)))
	 (methods
	  (mapcar (lambda (i) (list (cdr (assoc 'viewer i))))
		  (mailcap-mime-info type 'all)))
	 (method (completing-read "Viewer: " methods)))
    (mm-display-external (copy-sequence handle) method)))

(defun mm-preferred-alternative (handles &optional preferred)
  "Say which of HANDLES are preferred."
  (let ((prec (if preferred (list preferred) mm-alternative-precedence))
	p h result type handle)
    (while (setq p (pop prec))
      (setq h handles)
      (while h
	(setq type
	      (if (stringp (caar h))
		  (caar h)
		(car (mm-handle-type (car h)))))
	(setq handle (car h))
	(when (and (equal p type)
		   (mm-automatic-display-p type)
		   (or (stringp (caar h))
		       (not (mm-handle-disposition (car h)))
		       (equal (car (mm-handle-disposition (car h)))
			      "inline")))
	  (setq result (car h)
		h nil
		prec nil))
	(pop h)))
    result))

(defun mm-get-content-id (id)
  "Return the handle(s) referred to by ID."
  (cdr (assoc id mm-content-id-alist)))

(defun mm-get-image (handle)
  "Return an image instance based on HANDLE."
  (let ((type (cadr (split-string (car (mm-handle-type handle)) "/")))
	spec)
    ;; Allow some common translations.
    (setq type
	  (cond
	   ((equal type "x-pixmap")
	    "xpm")
	   ((equal type "x-xbitmap")
	    "xbm")
	   (t type)))
    (or (mm-handle-cache handle)
	(mm-with-unibyte-buffer
	  (insert-buffer-substring (mm-handle-buffer handle))
	  (mm-decode-content-transfer-encoding
	   (mm-handle-encoding handle)
	   (car (mm-handle-type handle)))
	  (prog1
	      (setq spec
		    (make-glyph `[,(intern type) :data ,(buffer-string)]))
	    (mm-handle-set-cache handle spec))))))

(defun mm-image-fit-p (handle)
  "Say whether the image in HANDLE will fit the current window."
  (let ((image (mm-get-image handle)))
    (or mm-all-images-fit
	(and (< (glyph-width image) (window-pixel-width))
	     (< (glyph-height image) (window-pixel-height))))))

(provide 'mm-decode)

;; mm-decode.el ends here
