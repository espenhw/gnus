;;; gnus-ems.el --- functions for making Gnus work under different Emacsen
;; Copyright (C) 1995 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@ifi.uio.no>
;; Keywords: news

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;;; Code:

(defvar gnus-mouse-2 [mouse-2])
(defvar gnus-easymenu 'easymenu)
(defvar gnus-group-mode-hook ())
(defvar gnus-summary-mode-hook ())
(defvar gnus-article-mode-hook ())

;; We do not byte-compile this file, because error messages are such a
;; bore.  

(eval
 '(cond 
   ((string-match "XEmacs\\|Lucid" emacs-version)
    ;; XEmacs definitions.

    (defvar gnus-summary-highlight
      '(((> score default) . bold)
	((< score default) . italic))
      "*Alist of `(FORM . FACE)'.
Summary lines are highlighted with the FACE for the first FORM which
evaluate to a non-nil value.  

Point will be at the beginning of the line when FORM is evaluated.
The following can be used for convenience:

score:   (gnus-summary-article-score)
default: gnus-summary-default-score
below:   gnus-summary-mark-below

To check for marks, e.g. to underline replied articles, use
`gnus-summary-article-mark': 

   ((= (gnus-summary-article-mark) gnus-replied-mark) . underline)")

    (setq gnus-mouse-2 [button2])
    (setq gnus-easymenu 'auc-menu)

    (or (memq 'underline (list-faces))
	(funcall (intern "make-face") 'underline))
    ;; Must avoid calling set-face-underline-p directly, because it
    ;; is a defsubst in emacs19, and will make the .elc files non
    ;; portable!
    (or (face-differs-from-default-p 'underline)
  	(funcall 'set-face-underline-p 'underline t))
    (or (fboundp 'set-text-properties)
	(defun set-text-properties (start end props &optional buffer)
	  (if (or (null buffer) (bufferp buffer))
	      (if props
		  (put-text-property start end (car props) (cdr props) buffer)
		(remove-text-properties start end ())))))

    (defvar gnus-header-face-alist 
      '(("" bold italic)))
    
    (or (fboundp 'make-overlay) (fset 'make-overlay 'make-extent))
    (or (fboundp 'overlay-put) (fset 'overlay-put 'set-extent-property))
    (or (fboundp 'move-overlay) 
        (defun move-overlay (extent start end &optional buffer)
          (set-extent-endpoints extent start end)))
    (or (boundp 'standard-display-table) (setq standard-display-table nil))
    (or (boundp 'read-event) (fset 'read-event 'next-command-event))

    (setq gnus-display-type 
	  (let ((display-resource 
		 (x-get-resource ".displayType" "DisplayType" 'string)))
	    (cond (display-resource (intern (downcase display-resource)))
		  ((x-display-color-p) 'color)
		  ((x-display-grayscale-p) 'grayscale)
		  (t 'mono))))

    (setq gnus-background-mode 
	  (let ((bg-resource 
		 (x-get-resource ".backgroundMode" "BackgroundMode" 'string))
		(params (frame-parameters)))
	    (cond (bg-resource (intern (downcase bg-resource)))
;		  ((< (apply '+ (x-color-values
;				 (cdr (assq 'background-color params))))
;		      (/ (apply '+ (x-color-values "white")) 3))
;		   'dark)
		  (t 'light))))

    (if (not gnus-visual)
	()
      (setq gnus-group-mode-hook
	    (cons
	     '(lambda ()
	       (easy-menu-add gnus-group-reading-menu)
	       (easy-menu-add gnus-group-group-menu)
	       (easy-menu-add gnus-group-post-menu)
	       (easy-menu-add gnus-group-misc-menu)
	       (gnus-install-mouse-tracker)) 
	     gnus-group-mode-hook))
      (setq gnus-summary-mode-hook
	    (cons
	     '(lambda ()
	       (easy-menu-add gnus-summary-mark-menu)
	       (easy-menu-add gnus-summary-move-menu)
	       (easy-menu-add gnus-summary-article-menu)
	       (easy-menu-add gnus-summary-thread-menu)
	       (easy-menu-add gnus-summary-misc-menu)
	       (easy-menu-add gnus-summary-post-menu)
	       (easy-menu-add gnus-summary-kill-menu)
	       (gnus-install-mouse-tracker)) 
	     gnus-summary-mode-hook))
      (setq gnus-article-mode-hook
	    (cons
	     '(lambda ()
	       (easy-menu-add gnus-article-article-menu)
	       (easy-menu-add gnus-article-treatment-menu))
	     gnus-article-mode-hook)))

    (defun gnus-install-mouse-tracker ()
      (require 'mode-motion)
      (setq mode-motion-hook 'mode-motion-highlight-line)))

   ((and (not (string-match "28.9" emacs-version)) 
	 (not (string-match "29" emacs-version)))
    (setq gnus-hidden-properties '(invisible t))
    (or (fboundp 'buffer-substring-no-properties)
	(defun buffer-substring-no-properties (beg end)
	  (format "%s" (buffer-substring beg end)))))
   
   ((boundp 'MULE)
    (provide 'gnusutil))
   
   ))

(eval-and-compile
  (cond
   ((not window-system)
    (defun gnus-dummy-func (&rest args))
    (let ((funcs '(mouse-set-point set-face-foreground
				   set-face-background x-popup-menu)))
      (while funcs
	(or (fboundp (car funcs))
	    (fset (car funcs) 'gnus-dummy-func))
	(setq funcs (cdr funcs))))))
  (or (fboundp 'file-regular-p)
      (defun file-regular-p (file)
	(and (not (file-directory-p file))
	     (not (file-symlink-p file))
	     (file-exists-p file))))
  (or (fboundp 'face-list)
      (defun face-list (&rest args)))
  )

(defun gnus-ems-redefine ()
  (cond 
   ((string-match "XEmacs\\|Lucid" emacs-version)
    ;; XEmacs definitions.
    (fset 'gnus-set-mouse-face 'identity)
    (fset 'gnus-summary-make-display-table (lambda () nil))
    (fset 'gnus-visual-turn-off-edit-menu 'identity)

    (defun gnus-highlight-selected-summary ()
      ;; Added by Per Abrahamsen <amanda@iesd.auc.dk>.
      ;; Highlight selected article in summary buffer
      (if gnus-summary-selected-face
	  (save-excursion
	    (let* ((beg (progn (beginning-of-line) (point)))
		   (end (progn (end-of-line) (point)))
		   (to (max 1 (1- (or (previous-single-property-change
				       end 'mouse-face nil beg) end))))
		   (from (1+ (or (next-single-property-change 
				  beg 'mouse-face nil end) beg))))
	      (if (< to beg)
		  (progn
		    (setq from beg)
		    (setq to end)))
	      (if gnus-newsgroup-selected-overlay
		  (delete-extent gnus-newsgroup-selected-overlay))
	      (setq gnus-newsgroup-selected-overlay
		    (make-extent from to))
	      (set-extent-face gnus-newsgroup-selected-overlay
			       gnus-summary-selected-face)))))


    (defun gnus-summary-recenter ()
      (let* ((top (cond ((< (window-height) 4) 0)
			((< (window-height) 7) 1)
			(t 2)))
	     (height (- (window-height) 2))
	     (bottom (save-excursion (goto-char (point-max))
				     (forward-line (- height))
				     (point)))
	     (window (get-buffer-window (current-buffer))))
	(and 
	 ;; The user has to want it,
	 gnus-auto-center-summary 
	 ;; the article buffer must be displayed,
	 (get-buffer-window gnus-article-buffer)
	 ;; Set the window start to either `bottom', which is the biggest
	 ;; possible valid number, or the second line from the top,
	 ;; whichever is the least.
	 (set-window-start
	  window (min bottom (save-excursion (forward-line (- top)) 
					     (point)))))))

    (defun gnus-group-insert-group-line-info (group)
      (let ((entry (gnus-gethash group gnus-newsrc-hashtb)) 
	    (beg (point))
	    active info)
	(if entry
	    (progn
	      (setq info (nth 2 entry))
	      (gnus-group-insert-group-line 
	       nil group (nth 1 info) (nth 3 info) (car entry) (nth 4 info)))
	  (setq active (gnus-gethash group gnus-active-hashtb))
	  
	  (gnus-group-insert-group-line 
	   nil group (if (member group gnus-zombie-list) gnus-level-zombie
		       gnus-level-killed)
	   nil (if active (- (1+ (cdr active)) (car active)) 0) nil))
	(save-excursion
	 (goto-char beg)
	 (remove-text-properties 
	  (1+ (gnus-point-at-bol)) (1+ (gnus-point-at-eol))
	  '(gnus-group nil)))))

    (defun gnus-copy-article-buffer (&optional article-buffer)
      (setq gnus-article-copy (get-buffer-create " *gnus article copy*"))
      (buffer-disable-undo gnus-article-copy)
      (or (memq gnus-article-copy gnus-buffer-list)
	  (setq gnus-buffer-list (cons gnus-article-copy gnus-buffer-list)))
      (let ((article-buffer (or article-buffer gnus-article-buffer))
	    buf)
	(if (and (get-buffer article-buffer)
		 (buffer-name (get-buffer article-buffer)))
	    (save-excursion
	      (set-buffer article-buffer)
	      (widen)
	      (setq buf (buffer-substring (point-min) (point-max)))
	      (set-buffer gnus-article-copy)
	      (erase-buffer)
	      (insert (format "%s" buf))))))

    (defun gnus-summary-refer-article (message-id)
      "Refer article specified by MESSAGE-ID.
NOTE: This command only works with newsgroups that use real or simulated NNTP."
      (interactive "sMessage-ID: ")
      (if (or (not (stringp message-id))
	      (zerop (length message-id)))
	  ()
	;; Construct the correct Message-ID if necessary.
	;; Suggested by tale@pawl.rpi.edu.
	(or (string-match "^<" message-id)
	    (setq message-id (concat "<" message-id)))
	(or (string-match ">$" message-id)
	    (setq message-id (concat message-id ">")))
	(let ((header (car (gnus-gethash (downcase message-id)
					 gnus-newsgroup-dependencies))))
	  (if header
	      (or (gnus-summary-goto-article (header-number header))
		  ;; The header has been read, but the article had been
		  ;; expunged, so we insert it again.
		  (let ((beg (point)))
		    (gnus-summary-insert-line
		     nil header 0 nil gnus-read-mark nil nil
		     (header-subject header))
		    (save-excursion
		      (goto-char beg)
		      (remove-text-properties
		       (1+ (gnus-point-at-bol)) (1+ (gnus-point-at-eol))
		       '(gnus-number nil gnus-mark nil gnus-level nil)))
		    (forward-line -1)
		    (header-number header)))
	    (let ((gnus-override-method gnus-refer-article-method)
		  (gnus-ancient-mark gnus-read-mark)
		  (tmp-buf (get-buffer-create " *gnus refer"))
		  (tmp-point (window-start
			      (get-buffer-window gnus-article-buffer)))
		  number)
	      (and gnus-refer-article-method
		   (or (gnus-server-opened gnus-refer-article-method)
		       (gnus-open-server gnus-refer-article-method)))
	      ;; Save the old article buffer.
	      (save-excursion
		(set-buffer tmp-buf)
		(buffer-disable-undo (current-buffer))
		(insert-buffer-substring gnus-article-buffer))
	      (prog1
		  (if (gnus-article-prepare 
		       message-id nil (gnus-read-header message-id))
		      (progn
			(setq number (header-number gnus-current-headers))
			(gnus-rebuild-thread message-id)
			(gnus-summary-goto-subject number)
			(gnus-summary-recenter)
			(gnus-article-set-window-start 
			 (cdr (assq number gnus-newsgroup-bookmarks)))
			message-id)
		    ;; We restore the old article buffer.
		    (save-excursion
		      (set-buffer gnus-article-buffer)
		      (let ((buffer-read-only nil))
			(insert-buffer-substring tmp-buf)
			(and tmp-point
			     (set-window-start (get-buffer-window (current-buffer))
					       tmp-point))))
		    nil)
		(kill-buffer tmp-buf)))))))

    (defun gnus-summary-insert-pseudos (pslist &optional not-view)
      (let ((buffer-read-only nil)
	    (article (gnus-summary-article-number))
	    b)
	(or (gnus-summary-goto-subject article)
	    (error (format "No such article: %d" article)))
	(or gnus-newsgroup-headers-hashtb-by-number
	    (gnus-make-headers-hashtable-by-number))
	(gnus-summary-position-cursor)
	;; If all commands are to be bunched up on one line, we collect
	;; them here.  
	(if gnus-view-pseudos-separately
	    ()
	  (let ((ps (setq pslist (sort pslist 'gnus-pseudos<)))
		files action)
	    (while ps
	      (setq action (cdr (assq 'action (car ps))))
	      (setq files (list (cdr (assq 'name (car ps)))))
	      (while (and ps (cdr ps)
			  (string= (or action "1")
				   (or (cdr (assq 'action (car (cdr ps)))) "2")))
		(setq files (cons (cdr (assq 'name (car (cdr ps)))) files))
		(setcdr ps (cdr (cdr ps))))
	      (if (not files)
		  ()
		(if (not (string-match "%s" action))
		    (setq files (cons " " files)))
		(setq files (cons " " files))
		(and (assq 'execute (car ps))
		     (setcdr (assq 'execute (car ps))
			     (funcall (if (string-match "%s" action)
					  'format 'concat)
				      action 
				      (mapconcat (lambda (f) f) files " ")))))
	      (setq ps (cdr ps)))))
	(if (and gnus-view-pseudos (not not-view))
	    (while pslist
	      (and (assq 'execute (car pslist))
		   (gnus-execute-command (cdr (assq 'execute (car pslist)))
					 (eq gnus-view-pseudos 'not-confirm)))
	      (setq pslist (cdr pslist)))
	  (save-excursion
	    (while pslist
	      (gnus-summary-goto-subject (or (cdr (assq 'article (car pslist)))
					     (gnus-summary-article-number)))
	      (forward-line 1)
	      (setq b (point))
	      (insert "          " (file-name-nondirectory 
				    (cdr (assq 'name (car pslist))))
		      ": " (or (cdr (assq 'execute (car pslist))) "") "\n")
	      (add-text-properties 
	       b (1+ b) (list 'gnus-number gnus-reffed-article-number
			      'gnus-mark gnus-unread-mark 
			      'gnus-level 0
			      'gnus-pseudo (car pslist)))
	      (remove-text-properties (b) (gnus-point-at-eol)
				      '(gnus-number nil gnus-mark nil gnus-level nil))
	      (forward-line -1)
	      (gnus-sethash (int-to-string gnus-reffed-article-number)
			    (car pslist) gnus-newsgroup-headers-hashtb-by-number)
	      (setq gnus-reffed-article-number (1- gnus-reffed-article-number))
	      (setq pslist (cdr pslist)))))))



    )

   ((boundp 'MULE)
    ;; Mule definitions
    (if (not (fboundp 'truncate-string))
	(defun truncate-string (str width)
	  (let ((w (string-width str))
		(col 0) (idx 0) (p-idx 0) chr)
	    (if (<= w width)
		str
	      (while (< col width)
		(setq chr (aref str idx)
		      col (+ col (char-width chr))
		      p-idx idx
		      idx (+ idx (char-bytes chr))
		      ))
	      (substring str 0 (if (= col width)
				   idx
				 p-idx))
	      )))
      )
    (defalias 'gnus-truncate-string 'truncate-string)

    (fset 
     'gnus-format-max-width 
     (lambda (form length)
       (let* ((val (eval form))
	      (valstr (if (numberp val) (int-to-string val) val)))
	 (if (> (length valstr) length)
	     (truncate-string valstr length)
	   valstr))))

    (fset 'gnus-summary-make-display-table (lambda () nil))
    )
   ))

(provide 'gnus-ems)

;; Local Variables:
;; byte-compile-warnings: nil
;; End:

;;; gnus-ems.el ends here
