;;; gnus-xmas.el --- Gnus functions for XEmacs
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

(require 'text-props)

(defvar gnus-xmas-glyph-directory nil
  "*Directory where Gnus logos and icons are located.
If this variable is nil, Gnus will try to locate the directory
automatically.")

;;; Internal variables.

(defvar gnus-xmas-logo (make-glyph (make-specifier 'image)))

;; Don't warn about these undefined variables.

(defvar gnus-group-mode-hook)
(defvar gnus-summary-mode-hook)
(defvar gnus-article-mode-hook)

;;defined in gnus.el
(defvar gnus-active-hashtb)
(defvar gnus-article-buffer)
(defvar gnus-auto-center-summary)
(defvar gnus-buffer-list)
(defvar gnus-current-headers)
(defvar gnus-level-killed)
(defvar gnus-level-zombie)
(defvar gnus-newsgroup-bookmarks)
(defvar gnus-newsgroup-dependencies)
(defvar gnus-newsgroup-selected-overlay)
(defvar gnus-newsrc-hashtb)
(defvar gnus-read-mark)
(defvar gnus-refer-article-method)
(defvar gnus-reffed-article-number)
(defvar gnus-unread-mark)
(defvar gnus-version)
(defvar gnus-view-pseudos)
(defvar gnus-view-pseudos-separately)
(defvar gnus-visual)
(defvar gnus-zombie-list)
;;defined in gnus-msg.el
(defvar gnus-article-copy)
(defvar gnus-check-before-posting)
;;defined in gnus-vis.el
(defvar gnus-article-button-face)
(defvar gnus-article-mouse-face)
(defvar gnus-summary-selected-face)
(defvar gnus-group-reading-menu)
(defvar gnus-group-group-menu)
(defvar gnus-group-misc-menu)
(defvar gnus-summary-article-menu)
(defvar gnus-summary-thread-menu)
(defvar gnus-summary-misc-menu)
(defvar gnus-summary-post-menu)
(defvar gnus-summary-kill-menu)
(defvar gnus-article-article-menu)
(defvar gnus-article-treatment-menu)
(defvar gnus-mouse-2)
(defvar standard-display-table)

(defun gnus-xmas-set-text-properties (start end props &optional buffer)
  "You should NEVER use this function.  It is ideologically blasphemous.
It is provided only to ease porting of broken FSF Emacs programs."
  (if (and (stringp buffer) (not (setq buffer (get-buffer buffer))))
      nil
    (map-extents (lambda (extent ignored)
		   (remove-text-properties 
		    start end
		    (list (extent-property extent 'text-prop) nil)
		    buffer))
		 buffer start end nil nil 'text-prop)
    (add-text-properties start end props buffer)))

(defun gnus-xmas-highlight-selected-summary ()
  ;; Highlight selected article in summary buffer
  (if gnus-summary-selected-face
      (progn
	(if gnus-newsgroup-selected-overlay
	    (delete-extent gnus-newsgroup-selected-overlay))
	(setq gnus-newsgroup-selected-overlay 
	      (make-extent (gnus-point-at-bol) (gnus-point-at-eol)))
	(set-extent-face gnus-newsgroup-selected-overlay
			 gnus-summary-selected-face))))

(defun gnus-xmas-summary-recenter ()
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
      window (min bottom (save-excursion
			   (forward-line (- top)) (point)))))))

(defun gnus-xmas-group-insert-group-line-info (group)
  (let ((entry (gnus-gethash group gnus-newsrc-hashtb)) 
	(beg (point))
	active info)
    (if entry
	(progn
	  (setq info (nth 2 entry))
	  (gnus-group-insert-group-line 
	   nil group (gnus-info-level info) (gnus-info-marks info)
	   (car entry) (gnus-info-method info)))
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

(defun gnus-xmas-copy-article-buffer (&optional article-buffer)
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

(defun gnus-xmas-article-push-button (event)
  "Check text under the mouse pointer for a callback function.
If the text under the mouse pointer has a `gnus-callback' property,
call it with the value of the `gnus-data' text property."
  (interactive "e")
  (set-buffer (window-buffer (event-window event)))
  (let* ((pos (event-closest-point event))
	 (data (get-text-property pos 'gnus-data))
	 (fun (get-text-property pos 'gnus-callback)))
    (if fun (funcall fun data))))

(defun gnus-xmas-move-overlay (extent start end &optional buffer)
  (set-extent-endpoints extent start end))

;; Fixed by Christopher Davis <ckd@loiosh.kei.com>.
(defun gnus-xmas-article-add-button (from to fun &optional data)
  "Create a button between FROM and TO with callback FUN and data DATA."
  (and gnus-article-button-face
       (gnus-overlay-put (gnus-make-overlay from to) 
			 'face gnus-article-button-face))
  (add-text-properties 
   from to
   (nconc
    (and gnus-article-mouse-face
	 (list 'mouse-face gnus-article-mouse-face))
    (list 'gnus-callback fun)
    (and data (list 'gnus-data data))
    (list 'highlight t))))

(defun gnus-xmas-window-top-edge (&optional window)
  (nth 1 (window-pixel-edges window)))

;; Select the lowest window on the frame.
(defun gnus-xmas-appt-select-lowest-window ()
  (let* ((lowest-window (selected-window))
	 (bottom-edge (car (cdr (cdr (cdr (window-pixel-edges))))))
         (last-window (previous-window))
         (window-search t))
    (while window-search
      (let* ((this-window (next-window))
             (next-bottom-edge (car (cdr (cdr (cdr 
                                               (window-pixel-edges 
						this-window)))))))
        (if (< bottom-edge next-bottom-edge)
            (progn
              (setq bottom-edge next-bottom-edge)
              (setq lowest-window this-window)))

        (select-window this-window)
        (if (eq last-window this-window)
            (progn
              (select-window lowest-window)
              (setq window-search nil)))))))

(defun gnus-xmas-group-menu-add ()
  (easy-menu-add gnus-group-reading-menu)
  (easy-menu-add gnus-group-group-menu)
  (easy-menu-add gnus-group-misc-menu))

(defun gnus-xmas-summary-menu-add ()
  (easy-menu-add gnus-summary-article-menu)
  (easy-menu-add gnus-summary-thread-menu)
  (easy-menu-add gnus-summary-misc-menu)
  (easy-menu-add gnus-summary-post-menu)
  (easy-menu-add gnus-summary-kill-menu)) 

(defun gnus-xmas-article-menu-add ()
  (easy-menu-add gnus-article-article-menu)
  (easy-menu-add gnus-article-treatment-menu))


(defun gnus-xmas-define ()
  (setq gnus-mouse-2 [button2])

  (or (memq 'underline (list-faces))
      (and (fboundp 'make-face)
	   (funcall (intern "make-face") 'underline)))
  ;; Must avoid calling set-face-underline-p directly, because it
  ;; is a defsubst in emacs19, and will make the .elc files non
  ;; portable!
  (or (face-differs-from-default-p 'underline)
      (funcall (intern "set-face-underline-p") 'underline t))

  (fset 'gnus-make-overlay 'make-extent)
  (fset 'gnus-overlay-put 'set-extent-property)
  (fset 'gnus-move-overlay 'gnus-xmas-move-overlay)
      
  (fset 'set-text-properties 'gnus-xmas-set-text-properties)

  (or (boundp 'standard-display-table) (setq standard-display-table nil))
  (or (boundp 'read-event) (fset 'read-event 'next-command-event))

  (defvar gnus-mouse-face-prop 'highlight)
      
  ;; Fix by "jeff (j.d.) sparkes" <jsparkes@bnr.ca>.
  (defvar gnus-display-type (device-class)
    "A symbol indicating the display Emacs is running under.
The symbol should be one of `color', `grayscale' or `mono'. If Emacs
guesses this display attribute wrongly, either set this variable in
your `~/.emacs' or set the resource `Emacs.displayType' in your
`~/.Xdefaults'. See also `gnus-background-mode'.

This is a meta-variable that will affect what default values other
variables get.  You would normally not change this variable, but
pounce directly on the real variables themselves.")


  (or (fboundp 'x-color-values)
      (fset 'x-color-values 
	    (lambda (color)
	      (color-instance-rgb-components
	       (make-color-instance color)))))
    
  (defvar gnus-background-mode 
    (let ((bg-resource 
	   (condition-case ()
	       (x-get-resource ".backgroundMode" "BackgroundMode" 'string)
	     (error nil)))
	  (params (frame-parameters)))
      (cond (bg-resource (intern (downcase bg-resource)))
	    ((and (assq 'background-color params)
		  (< (apply '+ (x-color-values
				(cdr (assq 'background-color params))))
		     (/ (apply '+ (x-color-values "white")) 3)))
	     'dark)
	    (t 'light)))
    "A symbol indicating the Emacs background brightness.
The symbol should be one of `light' or `dark'.
If Emacs guesses this frame attribute wrongly, either set this variable in
your `~/.emacs' or set the resource `Emacs.backgroundMode' in your
`~/.Xdefaults'.
See also `gnus-display-type'.

This is a meta-variable that will affect what default values other
variables get.  You would normally not change this variable, but
pounce directly on the real variables themselves.")
  )



(defun gnus-xmas-redefine ()
  (fset 'gnus-mouse-face-function 'identity)
  (fset 'gnus-summary-make-display-table (lambda () nil))
  (fset 'gnus-visual-turn-off-edit-menu 'identity)
  (fset 'gnus-highlight-selected-summary
	'gnus-xmas-highlight-selected-summary)
  (fset 'gnus-summary-recenter 'gnus-xmas-summary-recenter)
  (fset 'gnus-group-insert-group-line-info
	'gnus-xmas-group-insert-group-line-info)
  (fset 'gnus-copy-article-buffer 'gnus-xmas-copy-article-buffer)
  (fset 'gnus-article-push-button 'gnus-xmas-article-push-button)
  (fset 'gnus-article-add-button 'gnus-xmas-article-add-button)
  (fset 'gnus-window-top-edge 'gnus-xmas-window-top-edge)
  (fset 'set-text-properties 'gnus-xmas-set-text-properties)

  (or (fboundp 'appt-select-lowest-window)
      (fset 'appt-select-lowest-window 
	    'gnus-xmas-appt-select-lowest-window))

  (add-hook 'gnus-group-mode-hook 'gnus-xmas-group-menu-add)
  (add-hook 'gnus-summary-mode-hook 'gnus-xmas-summary-menu-add)
  (add-hook 'gnus-article-mode-hook 'gnus-xmas-article-menu-add)

  (add-hook 'gnus-group-mode-hook 'gnus-xmas-setup-group-toolbar)
  (add-hook 'gnus-summary-mode-hook 'gnus-xmas-setup-summary-toolbar))


;;; XEmacs logo and toolbar.

(defun gnus-xmas-find-glyph-directory ()
  (or gnus-xmas-glyph-directory ; We have a dir already...
      (let ((path load-path)
	    dir)
	;; We try to find the dir by looking at the load path,
	;; stripping away the last component and adding "etc/".
	(while path
	  (setq dir (concat
		     (file-name-directory (directory-file-name (car path)))
		     "etc/"))
	  (if (and (file-exists-p dir)
		   (file-directory-p dir)
		   (file-exists-p (concat dir "gnus-group-exit-icon-up.xpm")))
	      (setq gnus-xmas-glyph-directory dir
		    path nil)
	    (setq path (cdr path))))
	gnus-xmas-glyph-directory)))

(defun gnus-xmas-group-startup (&optional x y)
  "Insert startup message in current buffer."
  ;; Insert the message.
  (erase-buffer)
  (if (featurep 'xpm)
      (progn
	(set-glyph-property gnus-xmas-logo 'image  "~/tmp/gnus.xpm")
	(set-glyph-image gnus-xmas-logo "~/tmp/gnus.xpm" 'global 'x)

	(insert " ")
	(set-extent-begin-glyph (make-extent (point) (point)) gnus-xmas-logo)
	(insert "
   Gnus * A newsreader for Emacsen
 A Praxis Release * larsi@ifi.uio.no")
	(goto-char (point-min))
	(while (not (eobp))
	  (insert (make-string (/ (max (- (window-width) (or x 35)) 0) 2)
			       ? ))
	  (forward-line 1))
	(goto-char (point-min))
	;; +4 is fuzzy factor.
	(insert-char ?\n (/ (max (- (window-height) (or y 24)) 0) 2)))

    (insert
     (format "
     %s
           A newsreader 
      for GNU Emacs

        Based on GNUS 
             written by 
     Masanobu UMEDA

       A Praxis Release
      larsi@ifi.uio.no
" 
	     gnus-version))
    ;; And then hack it.
    ;; 18 is the longest line.
    (indent-rigidly (point-min) (point-max) 
		    (/ (max (- (window-width) (or x 28)) 0) 2))
    (goto-char (point-min))
    ;; +4 is fuzzy factor.
    (insert-char ?\n (/ (max (- (window-height) (or y 12)) 0) 2)))

  ;; Fontify some.
  (goto-char (point-min))
  (search-forward "Praxis")
  (put-text-property (match-beginning 0) (match-end 0) 'face 'bold)
  (goto-char (point-min)))

;;; The toolbar.

(defvar gnus-use-toolbar 'default-toolbar
  "*If nil, do not use a toolbar.
If it is non-nil, it must be a toolbar.  The five legal values are
`default-toolbar', `top-toolbar', `bottom-toolbar',
`right-toolbar', and `left-toolbar'.")

(defvar gnus-group-toolbar 
  '([gnus-group-exit-icon gnus-group-exit t "Exit Gnus"]
    [gnus-group-kill-group-icon gnus-group-kill-group t "Kill group"]
    [gnus-group-get-new-news-icon gnus-group-get-new-news t "Get new news"]
    [gnus-group-get-new-news-this-group-icon 
     gnus-group-get-new-news-this-group t "Get new news in this group"]
    [gnus-group-catchup-current-icon 
     gnus-group-catchup-current t "Catchup group"]
    [gnus-group-describe-group-icon 
     gnus-group-describe-group t "Describe group"])
  "The group buffer toolbar.")

(defvar gnus-summary-toolbar 
  '([gnus-summary-post-news-icon 
     gnus-summary-post-news t "Post an article"]
    [gnus-summary-save-article-file-icon
     gnus-summary-save-article-file t "Save article in file"]
    [gnus-summary-save-article-icon
     gnus-summary-save-article t "Save article"]
    [gnus-summary-reply-icon 
     gnus-summary-reply t "Mail a reply"]
    [gnus-summary-reply-with-original-icon
     gnus-summary-reply-with-original t "Mail a reply and yank the original"]
    [gnus-summary-followup-icon 
     gnus-summary-followup t "Post a followup"]
    [gnus-summary-followup-with-original-icon
     gnus-summary-followup-with-original t 
     "Post a followup and yank the original"]
    [gnus-uu-decode-uu-icon
     gnus-uu-decode-uu t "Decode uuencoded articles"]
    [gnus-uu-post-news-icon 
     gnus-uu-post-news t "Post an uuencoded article"]
    [gnus-summary-caesar-message-icon
     gnus-summary-caesar-message t "Rot 13"]
    [gnus-summary-cancel-article-icon
     gnus-summary-cancel-article t "Cancel article"])
  "The summary buffer toolbar.")

(defun gnus-xmas-setup-toolbar (bar &optional force)
  (let ((dir (gnus-xmas-find-glyph-directory))
	icon up down disabled name)
    (if (not dir)
	()
      (if (and (not force)
	       (boundp (aref (car bar) 0)))
	  dir
	(while bar
	  (setq icon (aref (car bar) 0)
		name (symbol-name icon)
		bar (cdr bar))
	  (setq up (concat dir name "-up.xpm"))
	  (setq down (concat dir name "-down.xpm"))
	  (setq disabled (concat dir name "-disabled.xpm"))
	  (if (not (file-exists-p up))
	      (set icon nil)
	    (set icon (toolbar-make-button-list
		       up (and (file-exists-p down) down)
		       (and (file-exists-p disabled) disabled)))))
	dir))))

(defun gnus-xmas-setup-group-toolbar ()
  (and gnus-use-toolbar
       (gnus-xmas-setup-toolbar gnus-group-toolbar)
       (set-specifier (symbol-value gnus-use-toolbar)
		      (cons (current-buffer) gnus-group-toolbar))))

(defun gnus-xmas-setup-summary-toolbar ()
  (and gnus-use-toolbar
       (gnus-xmas-setup-toolbar gnus-summary-toolbar)
       (set-specifier (symbol-value gnus-use-toolbar)
		      (cons (current-buffer) gnus-summary-toolbar))))


;;; gnus-xmas.el ends here
