;;; gnus.el --- a newsreader for GNU Emacs
;; Copyright (C) 1987,88,89,90,93,94,95,96 Free Software Foundation, Inc.

;; Author: Masanobu UMEDA <umerin@flab.flab.fujitsu.junet>
;;	Lars Magne Ingebrigtsen <larsi@ifi.uio.no>
;; Keywords: news

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

(eval '(run-hooks 'gnus-load-hook))

(defconst gnus-version-number "0.8"
  "Version number for this version of Gnus.")

(defconst gnus-version (format "Red Gnus v%s" gnus-version-number)
  "Version string for this version of Gnus.")

(defvar gnus-inhibit-startup-message nil
  "*If non-nil, the startup message will not be displayed.")

;;; Internal variables

(defvar gnus-group-buffer "*Group*")

;;; Splash screen.

(defun gnus-splash ()
  (save-excursion
    (switch-to-buffer gnus-group-buffer)
    (let ((buffer-read-only nil))
      (erase-buffer)
      (unless gnus-inhibit-startup-message
	(gnus-group-startup-message)
	(sit-for 0)))))

(defun gnus-indent-rigidly (start end arg)
  "Indent rigidly using only spaces and no tabs."
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (indent-rigidly start end arg)
      ;; We translate tabs into spaces -- not everybody uses
      ;; an 8-character tab.
      (goto-char (point-min))
      (while (search-forward "\t" nil t)
	(replace-match "        " t t)))))

(defun gnus-group-startup-message (&optional x y)
  "Insert startup message in current buffer."
  ;; Insert the message.
  (erase-buffer)
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
    (insert (make-string (max 0 (* 2 (/ rest 3))) ?\n)))
  ;; Fontify some.
  (goto-char (point-min))
  (and (search-forward "Praxis" nil t)
       (put-text-property (match-beginning 0) (match-end 0) 'face 'bold))
  (goto-char (point-min))
  (setq mode-line-buffer-identification gnus-version)
  (set-buffer-modified-p t))

(eval-when (load)
  (gnus-splash))

;;; Do the rest.

(require 'gnus-load)



;; Fix by Hallvard B Furuseth <h.b.furuseth@usit.uio.no>.
;; If you want the cursor to go somewhere else, set these two
;; functions in some startup hook to whatever you want.
(defalias 'gnus-summary-position-point 'gnus-goto-colon)
(defalias 'gnus-group-position-point 'gnus-goto-colon)

;;; Various macros and substs.

(defun gnus-header-from (header)
  (mail-header-from header))

(defmacro gnus-gethash (string hashtable)
  "Get hash value of STRING in HASHTABLE."
  `(symbol-value (intern-soft ,string ,hashtable)))

(defmacro gnus-sethash (string value hashtable)
  "Set hash value.  Arguments are STRING, VALUE, and HASHTABLE."
  `(set (intern ,string ,hashtable) ,value))

(defmacro gnus-group-unread (group)
  "Get the currently computed number of unread articles in GROUP."
  `(car (gnus-gethash ,group gnus-newsrc-hashtb)))

(defmacro gnus-group-entry (group)
  "Get the newsrc entry for GROUP."
  `(gnus-gethash ,group gnus-newsrc-hashtb))

(defmacro gnus-active (group)
  "Get active info on GROUP."
  `(gnus-gethash ,group gnus-active-hashtb))

(defmacro gnus-set-active (group active)
  "Set GROUP's active info."
  `(gnus-sethash ,group ,active gnus-active-hashtb))

(defun gnus-alive-p ()
  "Say whether Gnus is running or not."
  (and gnus-group-buffer
       (get-buffer gnus-group-buffer)
       (save-excursion
	 (set-buffer gnus-group-buffer)
	 (eq major-mode 'gnus-group-mode))))

;; Info access macros.

(defmacro gnus-info-group (info)
  `(nth 0 ,info))
(defmacro gnus-info-rank (info)
  `(nth 1 ,info))
(defmacro gnus-info-read (info)
  `(nth 2 ,info))
(defmacro gnus-info-marks (info)
  `(nth 3 ,info))
(defmacro gnus-info-method (info)
  `(nth 4 ,info))
(defmacro gnus-info-params (info)
  `(nth 5 ,info))

(defmacro gnus-info-level (info)
  `(let ((rank (gnus-info-rank ,info)))
     (if (consp rank)
	 (car rank)
       rank)))
(defmacro gnus-info-score (info)
  `(let ((rank (gnus-info-rank ,info)))
     (or (and (consp rank) (cdr rank)) 0)))

(defmacro gnus-info-set-group (info group)
  `(setcar ,info ,group))
(defmacro gnus-info-set-rank (info rank)
  `(setcar (nthcdr 1 ,info) ,rank))
(defmacro gnus-info-set-read (info read)
  `(setcar (nthcdr 2 ,info) ,read))
(defmacro gnus-info-set-marks (info marks &optional extend)
  (if extend
      `(gnus-info-set-entry ,info ,marks 3)
    `(setcar (nthcdr 3 ,info) ,marks)))
(defmacro gnus-info-set-method (info method &optional extend)
  (if extend
      `(gnus-info-set-entry ,info ,method 4)
    `(setcar (nthcdr 4 ,info) ,method)))
(defmacro gnus-info-set-params (info params &optional extend)
  (if extend
      `(gnus-info-set-entry ,info ,params 5)
    `(setcar (nthcdr 5 ,info) ,params)))

(defun gnus-info-set-entry (info entry number)
  ;; Extend the info until we have enough elements.
  (while (< (length info) number)
    (nconc info (list nil)))
  ;; Set the entry.
  (setcar (nthcdr number info) entry))

(defmacro gnus-info-set-level (info level)
  `(let ((rank (cdr ,info)))
     (if (consp (car rank))
	 (setcar (car rank) ,level)
       (setcar rank ,level))))
(defmacro gnus-info-set-score (info score)
  `(let ((rank (cdr ,info)))
     (if (consp (car rank))
	 (setcdr (car rank) ,score)
       (setcar rank (cons (car rank) ,score)))))

(defmacro gnus-get-info (group)
  `(nth 2 (gnus-gethash ,group gnus-newsrc-hashtb)))

;; Byte-compiler warning.
(defvar gnus-visual)
;; Find out whether the gnus-visual TYPE is wanted.
(defun gnus-visual-p (&optional type class)
  (and gnus-visual			; Has to be non-nil, at least.
       (if (not type)			; We don't care about type.
	   gnus-visual
	 (if (listp gnus-visual)	; It's a list, so we check it.
	     (or (memq type gnus-visual)
		 (memq class gnus-visual))
	   t))))

;;; Load the compatability functions.

(require 'gnus-cus)
(require 'gnus-ems)


;;;
;;; Shutdown
;;;

(defvar gnus-shutdown-alist nil)

(defun gnus-add-shutdown (function &rest symbols)
  "Run FUNCTION whenever one of SYMBOLS is shut down."
  (push (cons function symbols) gnus-shutdown-alist))

(defun gnus-shutdown (symbol)
  "Shut down everything that waits for SYMBOL."
  (let ((alist gnus-shutdown-alist)
	entry)
    (while (setq entry (pop alist))
      (when (memq symbol (cdr entry))
	(funcall (car entry))))))


;;;
;;; Gnus Utility Functions
;;;

;; Add the current buffer to the list of buffers to be killed on exit.
(defun gnus-add-current-to-buffer-list ()
  (or (memq (current-buffer) gnus-buffer-list)
      (setq gnus-buffer-list (cons (current-buffer) gnus-buffer-list))))

(defun gnus-version (&optional arg)
  "Version number of this version of Gnus.
If ARG, insert string at point."
  (interactive "P")
  (let ((methods gnus-valid-select-methods)
	(mess gnus-version)
	meth)
    ;; Go through all the legal select methods and add their version
    ;; numbers to the total version string.  Only the backends that are
    ;; currently in use will have their message numbers taken into
    ;; consideration.
    (while methods
      (setq meth (intern (concat (caar methods) "-version")))
      (and (boundp meth)
	   (stringp (symbol-value meth))
	   (setq mess (concat mess "; " (symbol-value meth))))
      (setq methods (cdr methods)))
    (if arg
	(insert (message mess))
      (message mess))))

(defun gnus-continuum-version (version)
  "Return VERSION as a floating point number."
  (when (or (string-match "^\\([^ ]+\\)? ?Gnus v?\\([0-9.]+\\)$" version)
	    (string-match "^\\(.?\\)gnus-\\([0-9.]+\\)$" version))
    (let* ((alpha (and (match-beginning 1) (match-string 1 version)))
	   (number (match-string 2 version))
	   major minor least)
      (string-match "\\([0-9]\\)\\.\\([0-9]+\\)\\.?\\([0-9]+\\)?" number)
      (setq major (string-to-number (match-string 1 number)))
      (setq minor (string-to-number (match-string 2 number)))
      (setq least (if (match-beginning 3)
		      (string-to-number (match-string 3 number))
		    0))
      (string-to-number
       (if (zerop major)
	   (format "%s00%02d%02d"
		   (cond 
		    ((member alpha '("(ding)" "d")) "4.99")
		    ((member alpha '("September" "s")) "5.01")
		    ((member alpha '("Red" "r")) "5.03"))
		   minor least)
	 (format "%d.%02d%02d" major minor least))))))

(defun gnus-info-find-node ()
  "Find Info documentation of Gnus."
  (interactive)
  ;; Enlarge info window if needed.
  (let ((mode major-mode)
	gnus-info-buffer)
    (Info-goto-node (cadr (assq mode gnus-info-nodes)))
    (setq gnus-info-buffer (current-buffer))
    (gnus-configure-windows 'info)))

;;; More various functions.

(defun gnus-group-read-only-p (&optional group)
  "Check whether GROUP supports editing or not.
If GROUP is nil, `gnus-newsgroup-name' will be checked instead.	 Note
that that variable is buffer-local to the summary buffers."
  (let ((group (or group gnus-newsgroup-name)))
    (not (gnus-check-backend-function 'request-replace-article group))))

(defun gnus-group-total-expirable-p (group)
  "Check whether GROUP is total-expirable or not."
  (let ((params (gnus-info-params (gnus-get-info group))))
    (or (memq 'total-expire params)
	(cdr (assq 'total-expire params)) ; (total-expire . t)
	(and gnus-total-expirable-newsgroups ; Check var.
	     (string-match gnus-total-expirable-newsgroups group)))))

(defun gnus-group-auto-expirable-p (group)
  "Check whether GROUP is total-expirable or not."
  (let ((params (gnus-info-params (gnus-get-info group))))
    (or (memq 'auto-expire params)
	(cdr (assq 'auto-expire params)) ; (auto-expire . t)
	(and gnus-auto-expirable-newsgroups ; Check var.
	     (string-match gnus-auto-expirable-newsgroups group)))))

(defun gnus-virtual-group-p (group)
  "Say whether GROUP is virtual or not."
  (memq 'virtual (assoc (symbol-name (car (gnus-find-method-for-group group)))
			gnus-valid-select-methods)))

(defun gnus-news-group-p (group &optional article)
  "Return non-nil if GROUP (and ARTICLE) come from a news server."
  (or (gnus-member-of-valid 'post group) ; Ordinary news group.
      (and (gnus-member-of-valid 'post-mail group) ; Combined group.
	   (eq (gnus-request-type group article) 'news))))

;; Returns a list of writable groups.
(defun gnus-writable-groups ()
  (let ((alist gnus-newsrc-alist)
	groups group)
    (while (setq group (car (pop alist)))
      (unless (gnus-group-read-only-p group)
	(push group groups)))
    (nreverse groups)))

;; Check whether to use long file names.
(defun gnus-use-long-file-name (symbol)
  ;; The variable has to be set...
  (and gnus-use-long-file-name
       ;; If it isn't a list, then we return t.
       (or (not (listp gnus-use-long-file-name))
	   ;; If it is a list, and the list contains `symbol', we
	   ;; return nil.
	   (not (memq symbol gnus-use-long-file-name)))))

;; Generate a unique new group name.
(defun gnus-generate-new-group-name (leaf)
  (let ((name leaf)
	(num 0))
    (while (gnus-gethash name gnus-newsrc-hashtb)
      (setq name (concat leaf "<" (int-to-string (setq num (1+ num))) ">")))
    name))

(defun gnus-ephemeral-group-p (group)
  "Say whether GROUP is ephemeral or not."
  (gnus-group-get-parameter group 'quit-config))

(defun gnus-group-quit-config (group)
  "Return the quit-config of GROUP."
  (gnus-group-get-parameter group 'quit-config))

(defun gnus-simplify-mode-line ()
  "Make mode lines a bit simpler."
  (setq mode-line-modified "-- ")
  (when (listp mode-line-format)
    (make-local-variable 'mode-line-format)
    (setq mode-line-format (copy-sequence mode-line-format))
    (when (equal (nth 3 mode-line-format) "   ")
      (setcar (nthcdr 3 mode-line-format) " "))))

;;; Servers and groups.

(defsubst gnus-server-add-address (method)
  (let ((method-name (symbol-name (car method))))
    (if (and (memq 'address (assoc method-name gnus-valid-select-methods))
	     (not (assq (intern (concat method-name "-address")) method)))
	(append method (list (list (intern (concat method-name "-address"))
				   (nth 1 method))))
      method)))

(defsubst gnus-server-get-method (group method)
  ;; Input either a server name, and extended server name, or a
  ;; select method, and return a select method.
  (cond ((stringp method)
	 (gnus-server-to-method method))
	((equal method gnus-select-method)
	 gnus-select-method)
	((and (stringp (car method)) group)
	 (gnus-server-extend-method group method))
	((and method (not group)
	      (equal (cadr method) ""))
	 method)
	(t
	 (gnus-server-add-address method))))

(defun gnus-server-to-method (server)
  "Map virtual server names to select methods."
  (or 
   ;; Is this a method, perhaps?
   (and server (listp server) server)
   ;; Perhaps this is the native server?
   (and (equal server "native") gnus-select-method)
   ;; It should be in the server alist.
   (cdr (assoc server gnus-server-alist))
   ;; If not, we look through all the opened server
   ;; to see whether we can find it there.
   (let ((opened gnus-opened-servers))
     (while (and opened
		 (not (equal server (format "%s:%s" (caaar opened)
					    (cadaar opened)))))
       (pop opened))
     (caar opened))))

(defmacro gnus-method-equal (ss1 ss2)
  "Say whether two servers are equal."
  `(let ((s1 ,ss1)
	 (s2 ,ss2))
     (or (equal s1 s2)
	 (and (= (length s1) (length s2))
	      (progn
		(while (and s1 (member (car s1) s2))
		  (setq s1 (cdr s1)))
		(null s1))))))

(defun gnus-server-equal (m1 m2)
  "Say whether two methods are equal."
  (let ((m1 (cond ((null m1) gnus-select-method)
		  ((stringp m1) (gnus-server-to-method m1))
		  (t m1)))
	(m2 (cond ((null m2) gnus-select-method)
		  ((stringp m2) (gnus-server-to-method m2))
		  (t m2))))
    (gnus-method-equal m1 m2)))

(defun gnus-servers-using-backend (backend)
  "Return a list of known servers using BACKEND."
  (let ((opened gnus-opened-servers)
	out)
    (while opened
      (when (eq backend (caaar opened))
	(push (caar opened) out))
      (pop opened))
    out))

(defun gnus-archive-server-wanted-p ()
  "Say whether the user wants to use the archive server."
  (cond 
   ((or (not gnus-message-archive-method)
	(not gnus-message-archive-group))
    nil)
   ((and gnus-message-archive-method gnus-message-archive-group)
    t)
   (t
    (let ((active (cadr (assq 'nnfolder-active-file
			      gnus-message-archive-method))))
      (and active
	   (file-exists-p active))))))

(defun gnus-group-prefixed-name (group method)
  "Return the whole name from GROUP and METHOD."
  (and (stringp method) (setq method (gnus-server-to-method method)))
  (if (not method)
      group
    (concat (format "%s" (car method))
	    (if (and
		 (or (assoc (format "%s" (car method)) 
			    (gnus-methods-using 'address))
		     (gnus-server-equal method gnus-message-archive-method))
		 (nth 1 method)
		 (not (string= (nth 1 method) "")))
		(concat "+" (nth 1 method)))
	    ":" group)))

(defun gnus-group-real-prefix (group)
  "Return the prefix of the current group name."
  (if (string-match "^[^:]+:" group)
      (substring group 0 (match-end 0))
    ""))

(defun gnus-group-method (group)
  "Return the server or method used for selecting GROUP."
  (let ((prefix (gnus-group-real-prefix group)))
    (if (equal prefix "")
	gnus-select-method
      (let ((servers gnus-opened-servers)
	    (server "")
	    backend possible found)
	(if (string-match "^[^\\+]+\\+" prefix)
	    (setq backend (intern (substring prefix 0 (1- (match-end 0))))
		  server (substring prefix (match-end 0) (1- (length prefix))))
	  (setq backend (intern (substring prefix 0 (1- (length prefix))))))
	(while servers
	  (when (eq (caaar servers) backend)
	    (setq possible (caar servers))
	    (when (equal (cadaar servers) server)
	      (setq found (caar servers))))
	  (pop servers))
	(or (car (rassoc found gnus-server-alist))
	    found
	    (car (rassoc possible gnus-server-alist))
	    possible
	    (list backend server))))))

(defsubst gnus-secondary-method-p (method)
  "Return whether METHOD is a secondary select method."
  (let ((methods gnus-secondary-select-methods)
	(gmethod (gnus-server-get-method nil method)))
    (while (and methods
		(not (equal (gnus-server-get-method nil (car methods))
			    gmethod)))
      (setq methods (cdr methods)))
    methods))

(defun gnus-group-foreign-p (group)
  "Say whether a group is foreign or not."
  (and (not (gnus-group-native-p group))
       (not (gnus-group-secondary-p group))))

(defun gnus-group-native-p (group)
  "Say whether the group is native or not."
  (not (string-match ":" group)))

(defun gnus-group-secondary-p (group)
  "Say whether the group is secondary or not."
  (gnus-secondary-method-p (gnus-find-method-for-group group)))

(defun gnus-group-get-parameter (group &optional symbol)
  "Returns the group parameters for GROUP.
If SYMBOL, return the value of that symbol in the group parameters."
  (let ((params (gnus-info-params (gnus-get-info group))))
    (if symbol
	(gnus-group-parameter-value params symbol)
      params)))

(defun gnus-group-parameter-value (params symbol)
  "Return the value of SYMBOL in group PARAMS."
  (or (car (memq symbol params))	; It's either a simple symbol
      (cdr (assq symbol params))))	; or a cons.

(defun gnus-group-add-parameter (group param)
  "Add parameter PARAM to GROUP."
  (let ((info (gnus-get-info group)))
    (if (not info)
	() ; This is a dead group.  We just ignore it.
      ;; Cons the new param to the old one and update.
      (gnus-group-set-info (cons param (gnus-info-params info))
			   group 'params))))

(defun gnus-group-set-parameter (group name value)
  "Set parameter NAME to VALUE in GROUP."
  (let ((info (gnus-get-info group)))
    (if (not info)
	() ; This is a dead group.  We just ignore it.
      (let ((old-params (gnus-info-params info))
	    (new-params (list (cons name value))))
	(while old-params
	  (if (or (not (listp (car old-params)))
		  (not (eq (caar old-params) name)))
	      (setq new-params (append new-params (list (car old-params)))))
	  (setq old-params (cdr old-params)))
	(gnus-group-set-info new-params group 'params)))))

(defun gnus-group-add-score (group &optional score)
  "Add SCORE to the GROUP score.
If SCORE is nil, add 1 to the score of GROUP."
  (let ((info (gnus-get-info group)))
    (when info
      (gnus-info-set-score info (+ (gnus-info-score info) (or score 1))))))

;; Function written by Stainless Steel Rat <ratinox@peorth.gweep.net>
(defun gnus-short-group-name (group &optional levels)
  "Collapse GROUP name LEVELS.
Select methods are stripped and any remote host name is stripped down to
just the host name."
  (let* ((name "") (foreign "") (depth -1) (skip 1)
	 (levels (or levels
		     (progn
		       (while (string-match "\\." group skip)
			 (setq skip (match-end 0)
			       depth (+ depth 1)))
		       depth))))
    ;; separate foreign select method from group name and collapse.
    ;; if method contains a server, collapse to non-domain server name,
    ;; otherwise collapse to select method
    (if (string-match ":" group)
	(cond ((string-match "+" group)
	       (let* ((plus (string-match "+" group))
		      (colon (string-match ":" group))
		      (dot (string-match "\\." group)))
		 (setq foreign (concat
				(substring group (+ 1 plus)
					   (cond ((< colon dot) colon)
						 ((< dot colon) dot))) ":")
		       group (substring group (+ 1 colon))
		       )))
	      (t
	       (let* ((colon (string-match ":" group)))
		 (setq foreign (concat (substring group 0 (+ 1 colon)))
		       group (substring group (+ 1 colon)))
		 ))))
    ;; collapse group name leaving LEVELS uncollapsed elements
    (while group
      (if (and (string-match "\\." group) (> levels 0))
	  (setq name (concat name (substring group 0 1))
		group (substring group (match-end 0))
		levels (- levels 1)
		name (concat name "."))
	(setq name (concat foreign name group)
	      group nil)))
    name))



;;;
;;; Kill file handling.
;;;

(defun gnus-apply-kill-file ()
  "Apply a kill file to the current newsgroup.
Returns the number of articles marked as read."
  (if (or (file-exists-p (gnus-newsgroup-kill-file nil))
	  (file-exists-p (gnus-newsgroup-kill-file gnus-newsgroup-name)))
      (gnus-apply-kill-file-internal)
    0))

(defun gnus-kill-save-kill-buffer ()
  (let ((file (gnus-newsgroup-kill-file gnus-newsgroup-name)))
    (when (get-file-buffer file)
      (save-excursion
	(set-buffer (get-file-buffer file))
	(and (buffer-modified-p) (save-buffer))
	(kill-buffer (current-buffer))))))

(defvar gnus-kill-file-name "KILL"
  "Suffix of the kill files.")

(defun gnus-newsgroup-kill-file (newsgroup)
  "Return the name of a kill file name for NEWSGROUP.
If NEWSGROUP is nil, return the global kill file name instead."
  (cond 
   ;; The global KILL file is placed at top of the directory.
   ((or (null newsgroup)
	(string-equal newsgroup ""))
    (expand-file-name gnus-kill-file-name
		      gnus-kill-files-directory))
   ;; Append ".KILL" to newsgroup name.
   ((gnus-use-long-file-name 'not-kill)
    (expand-file-name (concat (gnus-newsgroup-savable-name newsgroup)
			      "." gnus-kill-file-name)
		      gnus-kill-files-directory))
   ;; Place "KILL" under the hierarchical directory.
   (t
    (expand-file-name (concat (gnus-newsgroup-directory-form newsgroup)
			      "/" gnus-kill-file-name)
		      gnus-kill-files-directory))))

;;; Server things.

(defun gnus-member-of-valid (symbol group)
  "Find out if GROUP has SYMBOL as part of its \"valid\" spec."
  (memq symbol (assoc
		(symbol-name (car (gnus-find-method-for-group group)))
		gnus-valid-select-methods)))

(defun gnus-method-option-p (method option)
  "Return non-nil if select METHOD has OPTION as a parameter."
  (when (stringp method)
    (setq method (gnus-server-to-method method)))
  (memq option (assoc (format "%s" (car method))
		      gnus-valid-select-methods)))

(defun gnus-server-extend-method (group method)
  ;; This function "extends" a virtual server.	If the server is
  ;; "hello", and the select method is ("hello" (my-var "something"))
  ;; in the group "alt.alt", this will result in a new virtual server
  ;; called "hello+alt.alt".
  (let ((entry
	 (gnus-copy-sequence
	  (if (equal (car method) "native") gnus-select-method
	    (cdr (assoc (car method) gnus-server-alist))))))
    (setcar (cdr entry) (concat (nth 1 entry) "+" group))
    (nconc entry (cdr method))))

(defun gnus-server-status (method)
  "Return the status of METHOD."
  (nth 1 (assoc method gnus-opened-servers)))

(defun gnus-group-name-to-method (group)
  "Return a select method suitable for GROUP."
  (if (string-match ":" group)
      (let ((server (substring group 0 (match-beginning 0))))
	(if (string-match "\\+" server)
	    (list (intern (substring server 0 (match-beginning 0)))
		  (substring server (match-end 0)))
	  (list (intern server) "")))
    gnus-select-method))

(defun gnus-find-method-for-group (group &optional info)
  "Find the select method that GROUP uses."
  (or gnus-override-method
      (and (not group)
	   gnus-select-method)
      (let ((info (or info (gnus-get-info group)))
	    method)
	(if (or (not info)
		(not (setq method (gnus-info-method info)))
		(equal method "native"))
	    gnus-select-method
	  (setq method
		(cond ((stringp method)
		       (gnus-server-to-method method))
		      ((stringp (car method))
		       (gnus-server-extend-method group method))
		      (t
		       method)))
	  (cond ((equal (cadr method) "")
		 method)
		((null (cadr method))
		 (list (car method) ""))
		(t
		 (gnus-server-add-address method)))))))

(defun gnus-check-backend-function (func group)
  "Check whether GROUP supports function FUNC."
  (let ((method (if (stringp group) (car (gnus-find-method-for-group group))
		  group)))
    (fboundp (intern (format "%s-%s" method func)))))

(defun gnus-methods-using (feature)
  "Find all methods that have FEATURE."
  (let ((valids gnus-valid-select-methods)
	outs)
    (while valids
      (if (memq feature (car valids))
	  (setq outs (cons (car valids) outs)))
      (setq valids (cdr valids)))
    outs))

(defun gnus-read-method (prompt)
  "Prompt the user for a method.
Allow completion over sensible values."
  (let ((method
	 (completing-read
	  prompt (append gnus-valid-select-methods gnus-server-alist)
	  nil t nil 'gnus-method-history)))
    (cond 
     ((equal method "")
      (setq method gnus-select-method))
     ((assoc method gnus-valid-select-methods)
      (list method
	    (if (memq 'prompt-address
		      (assoc method gnus-valid-select-methods))
		(read-string "Address: ")
	      "")))
     ((assoc method gnus-server-alist)
      (list method))
     (t
      (list method "")))))

;;; User-level commands.

;;;###autoload
(defun gnus-slave-no-server (&optional arg)
  "Read network news as a slave, without connecting to local server"
  (interactive "P")
  (gnus-no-server arg t))

;;;###autoload
(defun gnus-no-server (&optional arg slave)
  "Read network news.
If ARG is a positive number, Gnus will use that as the
startup level.	If ARG is nil, Gnus will be started at level 2.
If ARG is non-nil and not a positive number, Gnus will
prompt the user for the name of an NNTP server to use.
As opposed to `gnus', this command will not connect to the local server."
  (interactive "P")
  (gnus-no-server-1 arg slave))

;;;###autoload
(defun gnus-slave (&optional arg)
  "Read news as a slave."
  (interactive "P")
  (gnus arg nil 'slave))

;;;###autoload
(defun gnus-other-frame (&optional arg)
  "Pop up a frame to read news."
  (interactive "P")
  (if (get-buffer gnus-group-buffer)
      (let ((pop-up-frames t))
	(gnus arg))
    (select-frame (make-frame))
    (gnus arg)))

;;;###autoload
(defun gnus (&optional arg dont-connect slave)
  "Read network news.
If ARG is non-nil and a positive number, Gnus will use that as the
startup level.	If ARG is non-nil and not a positive number, Gnus will
prompt the user for the name of an NNTP server to use."
  (interactive "P")
  (gnus-1 arg dont-connect slave))

;; Allow redefinition of Gnus functions.

(gnus-ems-redefine)

(provide 'gnus)

;;; gnus.el ends here
