;;; gnus-xmas.el --- Gnus functions for XEmacs
;; Copyright (C) 1995,96 Free Software Foundation, Inc.

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
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(require 'text-props)
(eval-when-compile (require 'cl))
(defvar menu-bar-mode t)

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
  (if (stringp buffer) 
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

(defun gnus-xmas-group-remove-excess-properties ()
  (let ((end (point))
	(beg (progn (forward-line -1) (point))))
    (remove-text-properties (1+ beg) end '(gnus-group nil))
    (remove-text-properties 
     beg end 
     '(gnus-topic nil gnus-topic-level nil gnus-topic-visible nil))
    (goto-char end)))
		  
(defun gnus-xmas-topic-remove-excess-properties ()
  (let ((end (point))
	(beg (progn (forward-line -1) (point))))
    (remove-text-properties beg end '(gnus-group nil))
    (goto-char end)))

(defun gnus-xmas-extent-start-open (point)
  (map-extents (lambda (extent arg)
		 (set-extent-property extent 'start-open t))
	       nil point (min (1+ (point)) (point-max))))
		  
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
	  (insert (format "%s" buf))))
    gnus-article-copy))

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

(defun gnus-xmas-tree-minimize ()
  (when (and gnus-tree-minimize-window
	     (not (one-window-p)))
    (let* ((window-min-height 2)
	   (height (1+ (count-lines (point-min) (point-max))))
	   (min (max (1- window-min-height) height))
	   (tot (if (numberp gnus-tree-minimize-window)
		    (min gnus-tree-minimize-window min)
		  min))
	   (win (get-buffer-window (current-buffer)))
	   (wh (and win (1- (window-height win)))))
      (when (and win
		 (not (eq tot wh)))
	(let ((selected (selected-window)))
	  (select-window win)
	  (enlarge-window (- tot wh))
	  (select-window selected))))))

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

(defmacro gnus-xmas-menu-add (type &rest menus)
  `(gnus-xmas-menu-add-1 ',type ',menus))
(put 'gnus-xmas-menu-add 'lisp-indent-function 1)
(put 'gnus-xmas-menu-add 'lisp-indent-hook 1)

(defun gnus-xmas-menu-add-1 (type menus)
  (when (and menu-bar-mode
	     (gnus-visual-p (intern (format "%s-menu" type)) 'menu))
    (while menus
      (easy-menu-add (symbol-value (pop menus))))))

(defun gnus-xmas-group-menu-add ()
  (gnus-xmas-menu-add group
    gnus-group-reading-menu gnus-group-group-menu gnus-group-misc-menu))

(defun gnus-xmas-summary-menu-add ()
  (gnus-xmas-menu-add summary
    gnus-summary-article-menu gnus-summary-thread-menu
    gnus-summary-misc-menu gnus-summary-post-menu gnus-summary-kill-menu))

(defun gnus-xmas-article-menu-add ()
  (gnus-xmas-menu-add article
    gnus-article-article-menu gnus-article-treatment-menu))

(defun gnus-xmas-pick-menu-add ()
  (gnus-xmas-menu-add pick
    gnus-pick-menu))

(defun gnus-xmas-binary-menu-add ()
  (gnus-xmas-menu-add binary
    gnus-binary-menu))

(defun gnus-xmas-tree-menu-add ()
  (gnus-xmas-menu-add tree
    gnus-tree-menu))

(defun gnus-xmas-grouplens-menu-add ()
  (gnus-xmas-menu-add grouplens
    gnus-grouplens-menu))

(defun gnus-xmas-read-event-char ()
  "Get the next event."
  (let ((event (next-event)))
    ;; We junk all non-key events.  Is this naughty?
    (while (not (key-press-event-p event))
      (setq event (next-event)))
    (cons (and (key-press-event-p event) 
	      ; (numberp (event-key event))
	       (event-to-character event)) 
	  event)))

(defun gnus-xmas-seconds-since-epoch (date)
  "Return a floating point number that says how many seconds have lapsed between Jan 1 12:00:00 1970 and DATE."
  (let* ((tdate (mapcar (lambda (ti) (and ti (string-to-int ti)))
			(timezone-parse-date date)))
	 (ttime (mapcar (lambda (ti) (and ti (string-to-int ti)))
			(timezone-parse-time
			 (aref (timezone-parse-date date) 3))))
	 (edate (mapcar (lambda (ti) (and ti (string-to-int ti)))
			(timezone-parse-date "Jan 1 12:00:00 1970")))
	 (tday (- (timezone-absolute-from-gregorian 
		   (nth 1 tdate) (nth 2 tdate) (nth 0 tdate))
		  (timezone-absolute-from-gregorian 
		   (nth 1 edate) (nth 2 edate) (nth 0 edate)))))
    (+ (nth 2 ttime)
       (* (nth 1 ttime) 60)
       (* (float (nth 0 ttime)) 60 60)
       (* (float tday) 60 60 24))))

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
  (fset 'gnus-overlay-end 'extent-end-position)
  (fset 'gnus-extent-detached-p 'extent-detached-p)
      
  (require 'text-props)
  (if (< emacs-minor-version 14)
      (fset 'gnus-set-text-properties 'gnus-xmas-set-text-properties))

  (fset 'nnheader-find-file-noselect 'gnus-xmas-find-file-noselect)

  (or (boundp 'standard-display-table) (setq standard-display-table nil))

  (defvar gnus-mouse-face-prop 'highlight)

  (unless (fboundp 'encode-time)
    (defun encode-time (sec minute hour day month year &optional zone)
      (let ((seconds
	     (gnus-xmas-seconds-since-epoch
	      (timezone-make-arpa-date 
	       year month day (timezone-make-time-string hour minute sec)
	       zone))))
	(list (floor (/ seconds (expt 2 16)))
	      (round (mod seconds (expt 2 16)))))))
      
  (defun gnus-byte-code (func)
    "Return a form that can be `eval'ed based on FUNC."
    (let ((fval (symbol-function func)))
      (if (byte-code-function-p fval)
	  (list 'funcall fval)
	(cons 'progn (cdr (cdr fval))))))
      
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


  (fset 'gnus-x-color-values 
	(if (fboundp 'x-color-values)
	    'x-color-values
	  (lambda (color)
	    (color-instance-rgb-components
	     (make-color-instance color)))))
    
  (defvar gnus-background-mode 
    (let* ((bg-resource 
	    (condition-case ()
		(x-get-resource ".backgroundMode" "BackgroundMode" 'string)
	      (error nil)))
	   (params (frame-parameters))
	   (color (condition-case ()
		      (or (assq 'background-color params)
			  (color-instance-name
			   (specifier-instance
			    (face-background 'default))))
		    (error nil))))
      (cond (bg-resource (intern (downcase bg-resource)))
	    ((and color
		  (< (apply '+ (gnus-x-color-values color))
		     (/ (apply '+ (gnus-x-color-values "white")) 3)))
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
  "Redefine lots of Gnus functions for XEmacs."
  (fset 'gnus-summary-make-display-table 'ignore)
  (fset 'gnus-visual-turn-off-edit-menu 'identity)
  (fset 'gnus-highlight-selected-summary
	'gnus-xmas-highlight-selected-summary)
  (fset 'gnus-summary-recenter 'gnus-xmas-summary-recenter)
  (fset 'gnus-group-remove-excess-properties
	'gnus-xmas-group-remove-excess-properties)
  (fset 'gnus-topic-remove-excess-properties
	'gnus-xmas-topic-remove-excess-properties)
  (fset 'gnus-extent-start-open 'gnus-xmas-extent-start-open)
  (fset 'gnus-copy-article-buffer 'gnus-xmas-copy-article-buffer)
  (fset 'gnus-article-push-button 'gnus-xmas-article-push-button)
  (fset 'gnus-article-add-button 'gnus-xmas-article-add-button)
  (fset 'gnus-window-top-edge 'gnus-xmas-window-top-edge)
  (fset 'gnus-read-event-char 'gnus-xmas-read-event-char)
  (fset 'gnus-group-startup-message 'gnus-xmas-group-startup-message)
  (fset 'gnus-tree-minimize 'gnus-xmas-tree-minimize)
  (fset 'gnus-appt-select-lowest-window 
	'gnus-xmas-appt-select-lowest-window)
  (fset 'gnus-mail-strip-quoted-names 'gnus-xmas-mail-strip-quoted-names)
  (fset 'gnus-make-local-hook 'make-local-variable)
  (fset 'gnus-character-to-event 'character-to-event)

  (add-hook 'gnus-group-mode-hook 'gnus-xmas-group-menu-add)
  (add-hook 'gnus-summary-mode-hook 'gnus-xmas-summary-menu-add)
  (add-hook 'gnus-article-mode-hook 'gnus-xmas-article-menu-add)

  (add-hook 'gnus-pick-mode-hook 'gnus-xmas-pick-menu-add)
  (add-hook 'gnus-tree-mode-hook 'gnus-xmas-tree-menu-add)
  (add-hook 'gnus-binary-mode-hook 'gnus-xmas-binary-menu-add)
  (add-hook 'gnus-grouplens-mode-hook 'gnus-xmas-grouplens-menu-add)

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
	  (if (and (car path)
		   (file-exists-p
		    (setq dir (concat
			       (file-name-directory
				(directory-file-name (car path)))
			       "etc/gnus/")))
		   (file-directory-p dir)
		   (file-exists-p (concat dir "gnus-group-exit-icon-up.xpm")))
	      (setq gnus-xmas-glyph-directory dir
		    path nil)
	    (setq path (cdr path))))
	gnus-xmas-glyph-directory)))

(defun gnus-xmas-group-startup-message (&optional x y)
  "Insert startup message in current buffer."
  ;; Insert the message.
  (gnus-xmas-find-glyph-directory)
  (erase-buffer)
  (let ((file (and gnus-xmas-glyph-directory
		   (concat 
		    (file-name-as-directory gnus-xmas-glyph-directory)
		    "gnus.xpm"))))
    (if (and (featurep 'xpm)
	     (not (equal (device-type) 'tty))
	     file (file-exists-p file))
	(progn
	  (set-glyph-property gnus-xmas-logo 'image file)
	  (set-glyph-image gnus-xmas-logo file 'global 'x)

	  (insert " ")
	  (set-extent-begin-glyph (make-extent (point) (point)) gnus-xmas-logo)
	  (goto-char (point-min))
	  (while (not (eobp))
	    (insert (make-string (/ (max (- (window-width) (or x 35)) 0) 2)
				 ? ))
	    (forward-line 1))
	  (goto-char (point-min))
	  (let* ((pheight (+ 20 (count-lines (point-min) (point-max))))
		 (wheight (window-height))
		 (rest (- wheight pheight)))
	    (insert (make-string (max 0 (* 2 (/ rest 3))) ?\n))))

      (insert
       (format "              %s
          _    ___ _             _      
          _ ___ __ ___  __    _ ___     
          __   _     ___    __  ___     
              _           ___     _     
             _  _ __             _      
             ___   __            _      
                   __           _       
                    _      _   _        
                   _      _    _        
                      _  _    _         
                  __  ___               
                 _   _ _     _          
                _   _                   
              _    _                    
             _    _                     
            _                         
          __                             

" 
	       ""))
      ;; And then hack it.
      (gnus-indent-rigidly (point-min) (point-max) 
			   (/ (max (- (window-width) (or x 46)) 0) 2))
      (goto-char (point-min))
      (forward-line 1)
      (let* ((pheight (count-lines (point-min) (point-max)))
	     (wheight (window-height))
	     (rest (- wheight pheight)))
	(insert (make-string (max 0 (* 2 (/ rest 3))) ?\n))))
    ;; Fontify some.
    (goto-char (point-min))
    (and (search-forward "Praxis" nil t)
	 (put-text-property (match-beginning 0) (match-end 0) 'face 'bold))
    (goto-char (point-min))
    (let* ((mode-string (gnus-group-set-mode-line)))
      (setq mode-line-buffer-identification 
	    (list (concat gnus-version (substring (car mode-string) 4))))
      (set-buffer-modified-p t))))


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

;; Written by Erik Naggum <erik@naggum.no>.
;; Saved by Steve Baur <steve@miranova.com>.
(or (fboundp 'insert-file-contents-literally)
(defun insert-file-contents-literally (filename &optional visit beg end replace)
  "Like `insert-file-contents', q.v., but only reads in the file.
A buffer may be modified in several ways after reading into the buffer due
to advanced Emacs features, such as file-name-handlers, format decoding,
find-file-hooks, etc.
  This function ensures that none of these modifications will take place."
  (let (                                ; (file-name-handler-alist nil)
        (format-alist nil)
        (after-insert-file-functions nil)
        (find-buffer-file-type-function 
         (if (fboundp 'find-buffer-file-type)
             (symbol-function 'find-buffer-file-type)
           nil)))
    (unwind-protect
        (progn
          (fset 'find-buffer-file-type (lambda (filename) t))
          (insert-file-contents filename visit beg end replace))
      (if find-buffer-file-type-function
          (fset 'find-buffer-file-type find-buffer-file-type-function)
        (fmakunbound 'find-buffer-file-type))))))

(defun gnus-xmas-find-file-noselect (filename &optional nowarn rawfile)
  "Read file FILENAME into a buffer and return the buffer.
If a buffer exists visiting FILENAME, return that one, but
verify that the file has not changed since visited or saved.
The buffer is not selected, just returned to the caller."
  (setq filename
	(abbreviate-file-name
	 (expand-file-name filename)))
  (if (file-directory-p filename)
      (if find-file-run-dired
	  (dired-noselect filename)
	(error "%s is a directory." filename))
    (let* ((buf (get-file-buffer filename))
	   (truename (abbreviate-file-name (file-truename filename)))
	   (number (nthcdr 10 (file-attributes truename)))
	   ;; Find any buffer for a file which has same truename.
	   (other (and (not buf) 
		       (if (fboundp 'find-buffer-visiting)
			   (find-buffer-visiting filename)
			 (get-file-buffer filename))))
	   error)
      ;; Let user know if there is a buffer with the same truename.
      (if other
	  (progn
	    (or nowarn
		(string-equal filename (buffer-file-name other))
		(message "%s and %s are the same file"
			 filename (buffer-file-name other)))
	    ;; Optionally also find that buffer.
	    (if (or (and (boundp 'find-file-existing-other-name)
			 find-file-existing-other-name)
		    find-file-visit-truename)
		(setq buf other))))
      (if buf
	  (or nowarn
	      (verify-visited-file-modtime buf)
	      (cond ((not (file-exists-p filename))
		     (error "File %s no longer exists!" filename))
		    ((yes-or-no-p
		      (if (string= (file-name-nondirectory filename)
				   (buffer-name buf))
			  (format
			   (if (buffer-modified-p buf)
			       "File %s changed on disk.  Discard your edits? "
			     "File %s changed on disk.  Reread from disk? ")
			   (file-name-nondirectory filename))
			(format
			 (if (buffer-modified-p buf)
			     "File %s changed on disk.  Discard your edits in %s? "
			   "File %s changed on disk.  Reread from disk into %s? ")
			 (file-name-nondirectory filename)
			 (buffer-name buf))))
		     (save-excursion
		       (set-buffer buf)
		       (revert-buffer t t)))))
	(save-excursion
;;; The truename stuff makes this obsolete.
;;;	  (let* ((link-name (car (file-attributes filename)))
;;;		 (linked-buf (and (stringp link-name)
;;;				  (get-file-buffer link-name))))
;;;	    (if (bufferp linked-buf)
;;;		(message "Symbolic link to file in buffer %s"
;;;			 (buffer-name linked-buf))))
	  (setq buf (create-file-buffer filename))
	  ;;	  (set-buffer-major-mode buf)
	  (set-buffer buf)
	  (erase-buffer)
	  (if rawfile
	      (condition-case ()
		  (insert-file-contents-literally filename t)
		(file-error
		 ;; Unconditionally set error
		 (setq error t)))
	    (condition-case ()
		(insert-file-contents filename t)
	      (file-error
	       ;; Run find-file-not-found-hooks until one returns non-nil.
	       (or t			; (run-hook-with-args-until-success 'find-file-not-found-hooks)
		   ;; If they fail too, set error.
		   (setq error t)))))
	  ;; Find the file's truename, and maybe use that as visited name.
	  (setq buffer-file-truename truename)
	  (setq buffer-file-number number)
	  ;; On VMS, we may want to remember which directory in a search list
	  ;; the file was found in.
	  (and (eq system-type 'vax-vms)
	       (let (logical)
		 (if (string-match ":" (file-name-directory filename))
		     (setq logical (substring (file-name-directory filename)
					      0 (match-beginning 0))))
		 (not (member logical find-file-not-true-dirname-list)))
	       (setq buffer-file-name buffer-file-truename))
	  (if find-file-visit-truename
	      (setq buffer-file-name
		    (setq filename
			  (expand-file-name buffer-file-truename))))
	  ;; Set buffer's default directory to that of the file.
	  (setq default-directory (file-name-directory filename))
	  ;; Turn off backup files for certain file names.  Since
	  ;; this is a permanent local, the major mode won't eliminate it.
	  (and (not (funcall backup-enable-predicate buffer-file-name))
	       (progn
		 (make-local-variable 'backup-inhibited)
		 (setq backup-inhibited t)))
	  (if rawfile
	      nil
	    (after-find-file error (not nowarn)))))
      buf)))

(defun gnus-xmas-mail-strip-quoted-names (address)
  "Protect mail-strip-quoted-names from NIL input.
XEmacs compatibility workaround."
  (if (null address)
      nil
    (mail-strip-quoted-names address)))

;;; gnus-xmas.el ends here
