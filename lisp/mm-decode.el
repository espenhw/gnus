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

(require 'drums)
(require 'mailcap)
(require 'mm-bodies)

(defvar mm-inline-media-tests
  '(("image/jpeg" mm-inline-image (featurep 'jpeg))
    ("image/png" mm-inline-image (featurep 'png))
    ("image/gif" mm-inline-image (featurep 'gif))
    ("image/tiff" mm-inline-image (featurep 'tiff))
    ("image/xbm" mm-inline-image (eq (device-type) 'x))
    ("image/xpm" mm-inline-image (featurep 'xpm))
    ("image/bmp" mm-inline-image (featurep 'bmp))
    ("text/plain" mm-inline-text t)
    ("text/html" mm-inline-text (featurep 'w3))
    ("audio/wav" mm-inline-audio
     (and (or (featurep 'nas-sound) (featurep 'native-sound))
	  (device-sound-enabled-p)))
    ("audio/au" mm-inline-audio
     (and (or (featurep 'nas-sound) (featurep 'native-sound))
	  (device-sound-enabled-p))))
  "Alist of media types/test that say whether the media types can be displayed inline.")

(defvar mm-user-display-methods
  '(("image/.*" . inline)
    ("text/.*" . inline)))

(defvar mm-user-automatic-display
  '("text/plain" "image/gif"))

(defvar mm-tmp-directory "/tmp/"
  "Where mm will store its temporary files.")

;;; Internal variables.

(defvar mm-dissection-list nil)
(defvar mm-last-shell-command "")

(defun mm-dissect-buffer (&optional no-strict-mime)
  "Dissect the current buffer and return a list of MIME handles."
  (save-excursion
    (let (ct ctl type subtype cte)
      (save-restriction
	(drums-narrow-to-header)
	(when (and (or no-strict-mime
		       (mail-fetch-field "mime-version"))
		   (setq ct (mail-fetch-field "content-type")))
	  (setq ctl (drums-parse-content-type ct))
	  (setq cte (mail-fetch-field "content-transfer-encoding"))))
      (when ctl
	(setq type (split-string (car ctl) "/"))
	(setq subtype (cadr type)
	      type (pop type))
	(cond
	 ((equal type "multipart")
	  (mm-dissect-multipart ctl))
	 (t
	  (mm-dissect-singlepart ctl (and cte (intern cte))
				 no-strict-mime)))))))

(defun mm-dissect-singlepart (ctl cte &optional force)
  (when (or force
	    (not (equal "text/plain" (car ctl))))
    (let ((res (list (list (mm-copy-to-buffer) ctl cte nil))))
      (push (car res) mm-dissection-list)
      res)))

(defun mm-remove-all-parts ()
  "Remove all MIME handles."
  (interactive)
  (mapcar 'mm-remove-part mm-dissection-list)
  (setq mm-dissection-list nil))

(defun mm-dissect-multipart (ctl)
  (goto-char (point-min))
  (let ((boundary (concat "\n--" (drums-content-type-get ctl 'boundary)))
	start parts end)
    (while (search-forward boundary nil t)
      (forward-line -1)
      (when start
	(save-excursion
	  (save-restriction
	    (narrow-to-region start (point))
	    (setq parts (nconc (mm-dissect-buffer t) parts)))))
      (forward-line 2)
      (setq start (point)))
    (nreverse parts)))

(defun mm-copy-to-buffer ()
  "Copy the contents of the current buffer to a fresh buffer."
  (save-excursion
    (let ((obuf (current-buffer))
	  beg)
      (goto-char (point-min))
      (search-forward "\n\n" nil t)
      (setq beg (point))
      (set-buffer (generate-new-buffer " *mm*"))
      (insert-buffer-substring obuf beg)
      (current-buffer))))

(defun mm-display-part (handle)
  "Display the MIME part represented by HANDLE."
  (save-excursion
    (mailcap-parse-mailcaps)
    (if (nth 3 handle)
	(mm-remove-part handle)
      (let* ((type (caadr handle))
	     (method (mailcap-mime-info type))
	     (user-method (mm-user-method type)))
	(if (eq user-method 'inline)
	    (progn
	      (forward-line 1)
	      (mm-display-inline handle))
	  (mm-display-external handle (or user-method method)))))))

(defun mm-display-external (handle method)
  "Display HANDLE using METHOD."
  (mm-with-unibyte-buffer
    (insert-buffer-substring (car handle))
    (mm-decode-content-transfer-encoding (nth 2 handle))
    (if (functionp method)
	(let ((cur (current-buffer)))
	  (switch-to-buffer (generate-new-buffer "*mm*"))
	  (insert-buffer-substring cur)
	  (funcall method)
	  (setcar (nthcdr 3 handle) (current-buffer)))
      (let* ((file (make-temp-name (expand-file-name "emm." mm-tmp-directory)))
	     process)
	(write-region (point-min) (point-max)
		      file nil 'nomesg nil 'no-conversion)
	(setq process
	      (start-process "*display*" nil shell-file-name
			     "-c" (format method file)))
	(setcar (nthcdr 3 handle) (cons file process))
	(message "Displaying %s..." (format method file))))))

(defun mm-remove-part (handle)
  "Remove the displayed MIME part represented by HANDLE."
  (let ((object (nth 3 handle)))
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
	  (kill-process (cdr object))
	(error nil)))
     ((bufferp object)
      (when (buffer-live-p object)
	(kill-buffer object))))
    (setcar (nthcdr 3 handle) nil)))

(defun mm-display-inline (handle)
  (let* ((type (caadr handle))
	 (function (cadr (assoc type mm-inline-media-tests))))
    (funcall function handle)))
	 
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
  (mm-remove-part handle)
  (when (buffer-live-p (car handle))
    (kill-buffer (car handle))))

(defun mm-quote-arg (arg)
  "Return a version of ARG that is safe to evaluate in a shell."
  (let ((pos 0) new-pos accum)
    ;; *** bug: we don't handle newline characters properly
    (while (setq new-pos (string-match "[!`\"$\\& \t{}]" arg pos))
      (push (substring arg pos new-pos) accum)
      (push "\\" accum)
      (push (list (aref arg new-pos)) accum)
      (setq pos (1+ new-pos)))
    (if (= pos 0)
        arg
      (apply 'concat (nconc (nreverse accum) (list (substring arg pos)))))))

;;;
;;; Functions for displaying various formats inline
;;;

(defun mm-inline-image (handle)
  (let ((type (cadr (split-string (caadr handle) "/")))
	image)
    (mm-with-unibyte-buffer
      (insert-buffer-substring (car handle))
      (mm-decode-content-transfer-encoding (nth 2 handle))
      (setq image (make-image-specifier
		   (vector (intern type) :data (buffer-string)))))
    (let ((annot (make-annotation image nil 'text)))
      (set-extent-property annot 'mm t)
      (set-extent-property annot 'duplicable t)
      (setcar (nthcdr 3 handle) annot))))

(defun mm-inline-text (handle)
  (let ((type (cadr (split-string (caadr handle) "/")))
	text buffer-read-only)
    (mm-with-unibyte-buffer
      (insert-buffer-substring (car handle))
      (mm-decode-content-transfer-encoding (nth 2 handle))
      (setq text (buffer-string)))
    (cond
     ((equal type "plain")
      (let ((b (point)))
	(insert text)
	(save-restriction
	  (narrow-to-region b (point))
	  (let ((charset (drums-content-type-get (nth 1 handle) 'charset)))
	    (when charset
	      (mm-decode-body charset nil)))
	  (setcar
	   (nthcdr 3 handle)
	   `(lambda ()
	      (let (buffer-read-only)
		(delete-region ,(set-marker (make-marker) (point-min))
			       ,(set-marker (make-marker) (point-max)))))))))
     )))

(defun mm-inline-audio (handle)
  (message "Not implemented"))

;;;
;;; Functions for outputting parts
;;;

(defun mm-save-part (handle)
  "Write HANDLE to a file."
  (let* ((name (drums-content-type-get (cadr handle) 'name))
	 (file (read-file-name "Save MIME part to: "
			       (expand-file-name
				(or name "") default-directory))))
    (mm-with-unibyte-buffer
      (insert-buffer-substring (car handle))
      (mm-decode-content-transfer-encoding (nth 2 handle))
      (when (or (not (file-exists-p file))
		(yes-or-no-p (format "File %s already exists; overwrite? ")))
	(write-region (point-min) (point-max) file)))))

(defun mm-pipe-part (handle)
  "Pipe HANDLE to a process."
  (let* ((name (drums-content-type-get (cadr handle) 'name))
	 (command
	  (read-string "Shell command on MIME part: " mm-last-shell-command)))
    (mm-with-unibyte-buffer
      (insert-buffer-substring (car handle))
      (mm-decode-content-transfer-encoding (nth 2 handle))
      (shell-command-on-region (point-min) (point-max) command nil))))

(defun mm-interactively-view-part (handle)
  "Display HANDLE using METHOD."
  (let* ((type (caadr handle))
	 (methods
	  (mapcar (lambda (i) (list (cdr (assoc "viewer" i))))
		  (mailcap-mime-info type 'all)))
	 (method (completing-read "Viewer: " methods)))
    (mm-display-external (copy-sequence handle) method)))

(provide 'mm-decode)

;; mm-decode.el ends here
