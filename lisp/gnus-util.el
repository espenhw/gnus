;;; gnus-util.el --- utility functions for Gnus
;; Copyright (C) 1996 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@ifi.uio.no>
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

;; Nothing in this file depends on any other parts of Gnus -- all
;; functions and macros in this file are utility functions that are
;; used by Gnus and may be used by any other package without loading
;; Gnus first.

;;; Code:

(require 'cl)
(require 'nnheader)
(require 'timezone)
(require 'message)

(defmacro gnus-eval-in-buffer-window (buffer &rest forms)
  "Pop to BUFFER, evaluate FORMS, and then return to the original window."
  (let ((tempvar (make-symbol "GnusStartBufferWindow"))
	(w (make-symbol "w"))
	(buf (make-symbol "buf")))
    `(let* ((,tempvar (selected-window))
	    (,buf ,buffer)
	    (,w (get-buffer-window ,buf 'visible)))
       (unwind-protect
	   (progn
	     (if ,w
		 (select-window ,w)
	       (pop-to-buffer ,buf))
	     ,@forms)
	 (select-window ,tempvar)))))

(put 'gnus-eval-in-buffer-window 'lisp-indent-function 1)
(put 'gnus-eval-in-buffer-window 'lisp-indent-hook 1)
(put 'gnus-eval-in-buffer-window 'edebug-form-spec '(form body))

(defmacro gnus-intern-safe (string hashtable)
  "Set hash value.  Arguments are STRING, VALUE, and HASHTABLE."
  `(let ((symbol (intern ,string ,hashtable)))
     (or (boundp symbol)
	 (set symbol nil))
     symbol))

;; modified by MORIOKA Tomohiko <morioka@jaist.ac.jp>
;;   function `substring' might cut on a middle of multi-octet
;;   character.
(defun gnus-truncate-string (str width)
  (substring str 0 width))

;; Added by Geoffrey T. Dairiki <dairiki@u.washington.edu>.  A safe way
;; to limit the length of a string.  This function is necessary since
;; `(substr "abc" 0 30)' pukes with "Args out of range".
(defsubst gnus-limit-string (str width)
  (if (> (length str) width)
      (substring str 0 width)
    str))

(defsubst gnus-functionp (form)
  "Return non-nil if FORM is funcallable."
  (or (and (symbolp form) (fboundp form))
      (and (listp form) (eq (car form) 'lambda))))

(defsubst gnus-goto-char (point)
  (and point (goto-char point)))

(defmacro gnus-buffer-exists-p (buffer)
  `(let ((buffer ,buffer))
     (and buffer
	  (funcall (if (stringp buffer) 'get-buffer 'buffer-name)
		   buffer))))

(defmacro gnus-kill-buffer (buffer)
  `(let ((buf ,buffer))
     (if (gnus-buffer-exists-p buf)
	 (kill-buffer buf))))

(defsubst gnus-point-at-bol ()
  "Return point at the beginning of the line."
  (let ((p (point)))
    (beginning-of-line)
    (prog1
	(point)
      (goto-char p))))

(defsubst gnus-point-at-eol ()
  "Return point at the end of the line."
  (let ((p (point)))
    (end-of-line)
    (prog1
	(point)
      (goto-char p))))

(defun gnus-delete-first (elt list)
  "Delete by side effect the first occurrence of ELT as a member of LIST."
  (if (equal (car list) elt)
      (cdr list)
    (let ((total list))
      (while (and (cdr list)
		  (not (equal (cadr list) elt)))
	(setq list (cdr list)))
      (when (cdr list)
	(setcdr list (cddr list)))
      total)))

;; Delete the current line (and the next N lines).
(defmacro gnus-delete-line (&optional n)
  `(delete-region (progn (beginning-of-line) (point))
		  (progn (forward-line ,(or n 1)) (point))))

(defun gnus-byte-code (func)
  "Return a form that can be `eval'ed based on FUNC."
  (let ((fval (symbol-function func)))
    (if (byte-code-function-p fval)
	(let ((flist (append fval nil)))
	  (setcar flist 'byte-code)
	  flist)
      (cons 'progn (cddr fval)))))

(defun gnus-extract-address-components (from)
  (let (name address)
    ;; First find the address - the thing with the @ in it.  This may
    ;; not be accurate in mail addresses, but does the trick most of
    ;; the time in news messages.
    (if (string-match "\\b[^@ \t<>]+[!@][^@ \t<>]+\\b" from)
	(setq address (substring from (match-beginning 0) (match-end 0))))
    ;; Then we check whether the "name <address>" format is used.
    (and address
	 ;; Fix by MORIOKA Tomohiko <morioka@jaist.ac.jp>
	 ;; Linear white space is not required.
	 (string-match (concat "[ \t]*<" (regexp-quote address) ">") from)
	 (and (setq name (substring from 0 (match-beginning 0)))
	      ;; Strip any quotes from the name.
	      (string-match "\".*\"" name)
	      (setq name (substring name 1 (1- (match-end 0))))))
    ;; If not, then "address (name)" is used.
    (or name
	(and (string-match "(.+)" from)
	     (setq name (substring from (1+ (match-beginning 0))
				   (1- (match-end 0)))))
	(and (string-match "()" from)
	     (setq name address))
	;; Fix by MORIOKA Tomohiko <morioka@jaist.ac.jp>.
	;; XOVER might not support folded From headers.
	(and (string-match "(.*" from)
	     (setq name (substring from (1+ (match-beginning 0))
				   (match-end 0)))))
    ;; Fix by Hallvard B Furuseth <h.b.furuseth@usit.uio.no>.
    (list (or name from) (or address from))))

(defun gnus-fetch-field (field)
  "Return the value of the header FIELD of current article."
  (save-excursion
    (save-restriction
      (let ((case-fold-search t)
	    (inhibit-point-motion-hooks t))
	(nnheader-narrow-to-headers)
	(message-fetch-field field)))))

(defun gnus-goto-colon ()
  (beginning-of-line)
  (search-forward ":" (gnus-point-at-eol) t))

(defun gnus-remove-text-with-property (prop)
  "Delete all text in the current buffer with text property PROP."
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (while (get-text-property (point) prop)
	(delete-char 1))
      (goto-char (next-single-property-change (point) prop nil (point-max))))))

(defun gnus-newsgroup-directory-form (newsgroup)
  "Make hierarchical directory name from NEWSGROUP name."
  (let ((newsgroup (gnus-newsgroup-savable-name newsgroup))
	(len (length newsgroup))
	idx)
    ;; If this is a foreign group, we don't want to translate the
    ;; entire name.
    (if (setq idx (string-match ":" newsgroup))
	(aset newsgroup idx ?/)
      (setq idx 0))
    ;; Replace all occurrences of `.' with `/'.
    (while (< idx len)
      (if (= (aref newsgroup idx) ?.)
	  (aset newsgroup idx ?/))
      (setq idx (1+ idx)))
    newsgroup))

(defun gnus-newsgroup-savable-name (group)
  ;; Replace any slashes in a group name (eg. an ange-ftp nndoc group)
  ;; with dots.
  (nnheader-replace-chars-in-string group ?/ ?.))

(defun gnus-string> (s1 s2)
  (not (or (string< s1 s2)
	   (string= s1 s2))))

;;; Time functions.

(defun gnus-days-between (date1 date2)
  ;; Return the number of days between date1 and date2.
  (- (gnus-day-number date1) (gnus-day-number date2)))

(defun gnus-day-number (date)
  (let ((dat (mapcar (lambda (s) (and s (string-to-int s)) )
		     (timezone-parse-date date))))
    (timezone-absolute-from-gregorian
     (nth 1 dat) (nth 2 dat) (car dat))))

(defun gnus-encode-date (date)
  "Convert DATE to internal time."
  (let* ((parse (timezone-parse-date date))
	 (date (mapcar (lambda (d) (and d (string-to-int d))) parse))
	 (time (mapcar 'string-to-int (timezone-parse-time (aref parse 3)))))
    (encode-time (caddr time) (cadr time) (car time)
		 (caddr date) (cadr date) (car date) (nth 4 date))))

(defun gnus-time-minus (t1 t2)
  "Subtract two internal times."
  (let ((borrow (< (cadr t1) (cadr t2))))
    (list (- (car t1) (car t2) (if borrow 1 0))
	  (- (+ (if borrow 65536 0) (cadr t1)) (cadr t2)))))

(defun gnus-time-less (t1 t2)
  "Say whether time T1 is less than time T2."
  (or (< (car t1) (car t2))
      (and (= (car t1) (car t2))
	   (< (nth 1 t1) (nth 1 t2)))))

(defun gnus-file-newer-than (file date)
  (let ((fdate (nth 5 (file-attributes file))))
    (or (> (car fdate) (car date))
	(and (= (car fdate) (car date))
	     (> (nth 1 fdate) (nth 1 date))))))

;;; Keymap macros.

(defmacro gnus-local-set-keys (&rest plist)
  "Set the keys in PLIST in the current keymap."
  `(gnus-define-keys-1 (current-local-map) ',plist))

(defmacro gnus-define-keys (keymap &rest plist)
  "Define all keys in PLIST in KEYMAP."
  `(gnus-define-keys-1 (quote ,keymap) (quote ,plist)))

(put 'gnus-define-keys 'lisp-indent-function 1)
(put 'gnus-define-keys 'lisp-indent-hook 1)
(put 'gnus-define-keymap 'lisp-indent-function 1)
(put 'gnus-define-keymap 'lisp-indent-hook 1)

(defmacro gnus-define-keymap (keymap &rest plist)
  "Define all keys in PLIST in KEYMAP."
  `(gnus-define-keys-1 ,keymap (quote ,plist)))

(defun gnus-define-keys-1 (keymap plist)
  (when (null keymap)
    (error "Can't set keys in a null keymap"))
  (cond ((symbolp keymap)
	 (setq keymap (symbol-value keymap)))
	((keymapp keymap))
	((listp keymap)
	 (set (car keymap) nil)
	 (define-prefix-command (car keymap))
	 (define-key (symbol-value (caddr keymap)) (cadr keymap) (car keymap))
	 (setq keymap (symbol-value (car keymap)))))
  (let (key)
    (while plist
      (when (symbolp (setq key (pop plist)))
	(setq key (symbol-value key)))
      (define-key keymap key (pop plist)))))

(defun gnus-completing-read (default prompt &rest args)
  ;; Like `completing-read', except that DEFAULT is the default argument.
  (let* ((prompt (if default 
		     (concat prompt " (default " default ") ")
		   (concat prompt " ")))
	 (answer (apply 'completing-read prompt args)))
    (if (or (null answer) (zerop (length answer)))
	default
      answer)))

;; Two silly functions to ensure that all `y-or-n-p' questions clear
;; the echo area.
(defun gnus-y-or-n-p (prompt)
  (prog1
      (y-or-n-p prompt)
    (message "")))

(defun gnus-yes-or-no-p (prompt)
  (prog1
      (yes-or-no-p prompt)
    (message "")))

;; I suspect there's a better way, but I haven't taken the time to do
;; it yet. -erik selberg@cs.washington.edu
(defun gnus-dd-mmm (messy-date)
  "Return a string like DD-MMM from a big messy string"
  (let ((datevec (condition-case () (timezone-parse-date messy-date) 
		   (error nil))))
    (if (not datevec)
	"??-???"
      (format "%2s-%s"
	      (condition-case ()
		  ;; Make sure leading zeroes are stripped.
		  (number-to-string (string-to-number (aref datevec 2)))
		(error "??"))
	      (capitalize
	       (or (car
		    (nth (1- (string-to-number (aref datevec 1)))
			 timezone-months-assoc))
		   "???"))))))

(defun gnus-mode-string-quote (string)
  "Quote all \"%\" in STRING."
  (save-excursion
    (gnus-set-work-buffer)
    (insert string)
    (goto-char (point-min))
    (while (search-forward "%" nil t)
      (insert "%"))
    (buffer-string)))

;; Make a hash table (default and minimum size is 255).
;; Optional argument HASHSIZE specifies the table size.
(defun gnus-make-hashtable (&optional hashsize)
  (make-vector (if hashsize (max (gnus-create-hash-size hashsize) 255) 255) 0))

;; Make a number that is suitable for hashing; bigger than MIN and one
;; less than 2^x.
(defun gnus-create-hash-size (min)
  (let ((i 1))
    (while (< i min)
      (setq i (* 2 i)))
    (1- i)))

(defvar gnus-verbose 7
  "*Integer that says how verbose Gnus should be.
The higher the number, the more messages Gnus will flash to say what
it's doing.  At zero, Gnus will be totally mute; at five, Gnus will
display most important messages; and at ten, Gnus will keep on
jabbering all the time.")

;; Show message if message has a lower level than `gnus-verbose'.
;; Guideline for numbers:
;; 1 - error messages, 3 - non-serious error messages, 5 - messages
;; for things that take a long time, 7 - not very important messages
;; on stuff, 9 - messages inside loops.
(defun gnus-message (level &rest args)
  (if (<= level gnus-verbose)
      (apply 'message args)
    ;; We have to do this format thingy here even if the result isn't
    ;; shown - the return value has to be the same as the return value
    ;; from `message'.
    (apply 'format args)))

(defun gnus-error (level &rest args)
  "Beep an error if LEVEL is equal to or less than `gnus-verbose'."
  (when (<= (floor level) gnus-verbose)
    (apply 'message args)
    (ding)
    (let (duration)
      (when (and (floatp level)
		 (not (zerop (setq duration (* 10 (- level (floor level)))))))
	(sit-for duration))))
  nil)

(defun gnus-parent-id (references)
  "Return the last Message-ID in REFERENCES."
  (when (and references
	     (string-match "\\(<[^\n<>]+>\\)[ \t\n]*\\'" references))
    (substring references (match-beginning 1) (match-end 1))))

(defun gnus-split-references (references)
  "Return a list of Message-IDs in REFERENCES."
  (let ((beg 0)
	ids)
    (while (string-match "<[^>]+>" references beg)
      (push (substring references (match-beginning 0) (setq beg (match-end 0)))
	    ids))
    (nreverse ids)))

(defun gnus-buffer-live-p (buffer)
  "Say whether BUFFER is alive or not."
  (and buffer
       (get-buffer buffer)
       (buffer-name (get-buffer buffer))))

(defun gnus-horizontal-recenter ()
  "Recenter the current buffer horizontally."
  (if (< (current-column) (/ (window-width) 2))
      (set-window-hscroll (get-buffer-window (current-buffer) t) 0)
    (let* ((orig (point))
	   (end (window-end (get-buffer-window (current-buffer) t)))
	   (max 0))
      ;; Find the longest line currently displayed in the window.
      (goto-char (window-start))
      (while (and (not (eobp)) 
		  (< (point) end))
	(end-of-line)
	(setq max (max max (current-column)))
	(forward-line 1))
      (goto-char orig)
      ;; Scroll horizontally to center (sort of) the point.
      (if (> max (window-width))
	  (set-window-hscroll 
	   (get-buffer-window (current-buffer) t)
	   (min (- (current-column) (/ (window-width) 3))
		(+ 2 (- max (window-width)))))
	(set-window-hscroll (get-buffer-window (current-buffer) t) 0))
      max)))

(defun gnus-read-event-char ()
  "Get the next event."
  (let ((event (read-event)))
    (cons (and (numberp event) event) event)))

(defun gnus-sortable-date (date)
  "Make sortable string by string-lessp from DATE.
Timezone package is used."
  (condition-case ()
      (progn
	(setq date (inline (timezone-fix-time 
			    date nil 
			    (aref (inline (timezone-parse-date date)) 4))))
	(inline
	  (timezone-make-sortable-date
	   (aref date 0) (aref date 1) (aref date 2)
	   (inline
	     (timezone-make-time-string
	      (aref date 3) (aref date 4) (aref date 5))))))
    (error "")))
  
(defun gnus-copy-file (file &optional to)
  "Copy FILE to TO."
  (interactive
   (list (read-file-name "Copy file: " default-directory)
	 (read-file-name "Copy file to: " default-directory)))
  (or to (setq to (read-file-name "Copy file to: " default-directory)))
  (and (file-directory-p to)
       (setq to (concat (file-name-as-directory to)
			(file-name-nondirectory file))))
  (copy-file file to))

(defun gnus-kill-all-overlays ()
  "Delete all overlays in the current buffer."
  (when (fboundp 'overlay-lists)
    (let* ((overlayss (overlay-lists))
	   (buffer-read-only nil)
	   (overlays (nconc (car overlayss) (cdr overlayss))))
      (while overlays
	(delete-overlay (pop overlays))))))

(defvar gnus-work-buffer " *gnus work*")

(defun gnus-set-work-buffer ()
  "Put point in the empty Gnus work buffer."
  (if (get-buffer gnus-work-buffer)
      (progn
	(set-buffer gnus-work-buffer)
	(erase-buffer))
    (set-buffer (get-buffer-create gnus-work-buffer))
    (kill-all-local-variables)
    (buffer-disable-undo (current-buffer))))

(defmacro gnus-group-real-name (group)
  "Find the real name of a foreign newsgroup."
  `(let ((gname ,group))
     (if (string-match ":[^:]+$" gname)
	 (substring gname (1+ (match-beginning 0)))
       gname)))

(defun gnus-make-sort-function (funs)
  "Return a composite sort condition based on the functions in FUNC."
  (if (cdr funs)
      `(or (,(car funs) t1 t2)
	   (and (not (,(car funs) t2 t1))
		,(gnus-make-sort-function (cdr funs))))
    `(,(car funs) t1 t2)))
		 
(provide 'gnus-util)

;;; gnus-util.el ends here
