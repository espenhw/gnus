;;; gnus-picon.el --- displaying pretty icons in Gnus
;; Copyright (C) 1996,97 Free Software Foundation, Inc.

;; Author: Wes Hardaker <hardaker@ece.ucdavis.edu>
;; Keywords: news xpm annotation glyph faces

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

(require 'gnus)
(require 'xpm)
(require 'annotations)
(require 'custom)
(require 'gnus-art)
(require 'gnus-win)

(defgroup picons nil
  "Show pictures of people, domains, and newsgroups (XEmacs).
For this to work, you must add gnus-group-display-picons to the
gnus-summary-display-hook or to the gnus-article-display-hook
depending on what gnus-picons-display-where is set to.  You must
also add gnus-article-display-picons to gnus-article-display-hook."
  :group 'gnus-visual)

(defcustom gnus-picons-buffer "*Icon Buffer*"
  "Buffer name to display the icons in if gnus-picons-display-where is 'picons."
  :type 'string
  :group 'picons)

(defcustom gnus-picons-display-where 'picons
  "Where to display the group and article icons.
Legal values are `article' and `picons'."
  :type '(choice symbol string)
  :group 'picons)

(defcustom gnus-picons-database "/usr/local/faces"
  "Defines the location of the faces database.
For information on obtaining this database of pretty pictures, please
see http://www.cs.indiana.edu/picons/ftp/index.html"
  :type 'directory
  :group 'picons)

(defcustom gnus-picons-news-directory "news"
  "Sub-directory of the faces database containing the icons for newsgroups."
  :type 'string
  :group 'picons)

(defcustom gnus-picons-user-directories '("local" "users" "usenix" "misc")
  "List of directories to search for user faces."
  :type '(repeat string)
  :group 'picons)

(defcustom gnus-picons-domain-directories '("domains")
  "List of directories to search for domain faces.
Some people may want to add \"unknown\" to this list."
  :type '(repeat string)
  :group 'picons)

(defcustom gnus-picons-refresh-before-display nil
  "If non-nil, display the article buffer before computing the picons."
  :type 'boolean
  :group 'picons)

(defcustom gnus-picons-x-face-file-name
  (format "/tmp/picon-xface.%s.xbm" (user-login-name))
  "The name of the file in which to store the converted X-face header."
  :type 'string
  :group 'picons)

(defcustom gnus-picons-convert-x-face (format "{ echo '/* Width=48, Height=48 */'; uncompface; } | icontopbm | pbmtoxbm > %s" gnus-picons-x-face-file-name)
  "Command to convert the x-face header into a xbm file."
  :type 'string
  :group 'picons)

(defcustom gnus-picons-display-as-address t
  "*If t display textual email addresses along with pictures."
  :type 'boolean
  :group 'picons)

(defcustom gnus-picons-file-suffixes
  (when (featurep 'x)
    (let ((types (list "xbm")))
      (when (featurep 'gif)
	(push "gif" types))
      (when (featurep 'xpm)
	(push "xpm" types))
      types))
  "List of suffixes on picon file names to try."
  :type '(repeat string)
  :group 'picons)

(defcustom gnus-picons-display-article-move-p t
  "*Whether to move point to first empty line when displaying picons.
This has only an effect if `gnus-picons-display-where' has value `article'."
  :type 'boolean
  :group 'picons)

(defvar gnus-picons-map (make-sparse-keymap "gnus-picons-keys")
 "keymap to hide/show picon glyphs")

(define-key gnus-picons-map [(button2)] 'gnus-picons-toggle-extent)

;;; Internal variables.

(defvar gnus-group-annotations nil
  "List of annotations added/removed when selecting/exiting a group")
(defvar gnus-article-annotations nil
  "List of annotations added/removed when selecting an article")
(defvar gnus-x-face-annotations nil
  "List of annotations added/removed when selecting an article with an X-Face.")

(defun gnus-picons-remove (symbol)
  "Remove all annotations/processes in variable named SYMBOL.
This function is careful to set it to nil before removing anything so that
asynchronous process don't get crazy."
  (let ((listitems (symbol-value symbol)))
    (set symbol nil)
    (while listitems
      (let ((item (pop listitems)))
	(cond ((annotationp item)
	       (delete-annotation item))
	      ((processp item)
	       ;; kill the process, ignore any output.
	       (set-process-sentinel item (function (lambda (p e))))
	       (delete-process item)))))))

(defun gnus-picons-remove-all ()
  "Removes all picons from the Gnus display(s)."
  (interactive)
  (gnus-picons-remove 'gnus-article-annotations)
  (gnus-picons-remove 'gnus-group-annotations)
  (gnus-picons-remove 'gnus-x-face-annotations)
  (when (bufferp gnus-picons-buffer)
    (kill-buffer gnus-picons-buffer)))

(defun gnus-get-buffer-name (variable)
  "Returns the buffer name associated with the contents of a variable."
  (cond ((symbolp variable)
         (let ((newvar (cdr (assq variable gnus-window-to-buffer))))
           (cond ((symbolp newvar)
                  (symbol-value newvar))
                 ((stringp newvar) newvar))))
        ((stringp variable)
         variable)))

(defun gnus-picons-prepare-for-annotations (annotations)
  "Prepare picons buffer for puting annotations memorized in ANNOTATIONS.
ANNOTATIONS should be a symbol naming a variable wich contains a list of
annotations.  Sets buffer to `gnus-picons-display-where'."
  ;; let drawing catch up
  (when gnus-picons-refresh-before-display
    (sit-for 0))
  (set-buffer (get-buffer-create
	       (gnus-get-buffer-name gnus-picons-display-where)))
  (gnus-add-current-to-buffer-list)
  (goto-char (point-min))
  (if (and (eq gnus-picons-display-where 'article)
	   gnus-picons-display-article-move-p)
      (when (search-forward "\n\n" nil t)
	(forward-line -1)))
  (gnus-picons-remove annotations))

(defun gnus-picons-article-display-x-face ()
  "Display the x-face header bitmap in the 'gnus-picons-display-where buffer."
  ;; delete any old ones.
  ;; This is needed here because gnus-picons-display-x-face will not
  ;; be called if there is no X-Face header
  (gnus-picons-remove 'gnus-x-face-annotations)
  ;; display the new one.
  (let ((gnus-article-x-face-command 'gnus-picons-display-x-face))
    (gnus-article-display-x-face)))

(defun gnus-picons-x-face-sentinel (process event)
  ;; don't call gnus-picons-prepare-for-annotations, it would reset
  ;; gnus-x-face-annotations.
  (set-buffer (get-buffer-create
	       (gnus-get-buffer-name gnus-picons-display-where)))
  (gnus-add-current-to-buffer-list)
  (goto-char (point-min))
  (if (and (eq gnus-picons-display-where 'article)
	   gnus-picons-display-article-move-p)
      (when (search-forward "\n\n" nil t)
	(forward-line -1)))
  ;; If the process is still in the list, insert this icon
  (let ((myself (member process gnus-x-face-annotations)))
    (when myself
      (setcar myself
	      (make-annotation gnus-picons-x-face-file-name nil 'text))
      (delete-file gnus-picons-x-face-file-name))))

(defun gnus-picons-display-x-face (beg end)
  "Function to display the x-face header in the picons window.
To use:  (setq gnus-article-x-face-command 'gnus-picons-display-x-face)"
  (interactive)
  (if (featurep 'xface)
      ;; Use builtin support
      (let ((buf (current-buffer)))
	(save-excursion
	  (gnus-picons-prepare-for-annotations 'gnus-x-face-annotations)
	  (setq gnus-x-face-annotations
		(cons (make-annotation (concat "X-Face: "
					       (buffer-substring beg end buf))
				       nil 'text)
		      gnus-x-face-annotations))))
    ;; convert the x-face header to a .xbm file
    (let* ((process-connection-type nil)
	   (process (start-process "gnus-x-face" nil
				   shell-file-name shell-command-switch
				   gnus-picons-convert-x-face)))
      (process-kill-without-query process)
      (setq gnus-x-face-annotations (list process))
      (set-process-sentinel process 'gnus-picons-x-face-sentinel)
      (process-send-region process beg end)
      (process-send-eof process))))

(defun gnus-article-display-picons ()
  "Display faces for an author and his/her domain in gnus-picons-display-where."
  (interactive)
  (let (from at-idx)
    (when (and (featurep 'xpm)
	       (or (not (fboundp 'device-type)) (equal (device-type) 'x))
	       (setq from (mail-fetch-field "from"))
	       (setq from (downcase
			   (or (cadr (mail-extract-address-components from))
			       "")))
	       (or (setq at-idx (string-match "@" from))
		   (setq at-idx (length from))))
      (save-excursion
	(let ((username (substring from 0 at-idx))
	      (addrs (if (eq at-idx (length from))
			 (if gnus-local-domain
			     (message-tokenize-header gnus-local-domain ".")
			   nil)
		       (message-tokenize-header (substring from (1+ at-idx))
						"."))))
	  (gnus-picons-prepare-for-annotations 'gnus-article-annotations)
	  (setq gnus-article-annotations
		(nconc gnus-article-annotations
		       ;; look for domain paths.
		       (gnus-picons-display-pairs
			(gnus-picons-lookup-pairs addrs
					       gnus-picons-domain-directories)
			(not (or gnus-picons-display-as-address
				 gnus-article-annotations))
			nil "." t)
		       ;; add an '@' if displaying as address
		       (if  (and gnus-picons-display-as-address addrs)
			 (list (make-annotation "@" nil 'text nil nil nil t)))
		       ;; then do user directories,
		       (gnus-picons-display-picon-or-name
			(gnus-picons-lookup-user (downcase username) addrs)
			username nil t)))

	  (add-hook 'gnus-summary-exit-hook 'gnus-picons-remove-all))))))

(defun gnus-group-display-picons ()
  "Display icons for the group in the gnus-picons-display-where buffer."
  (interactive)
  (when (and (featurep 'xpm)
	     (or (not (fboundp 'device-type)) (equal (device-type) 'x)))
    (save-excursion
      (gnus-picons-prepare-for-annotations 'gnus-group-annotations)
      (setq gnus-group-annotations
	    (gnus-picons-display-pairs
	     (gnus-picons-lookup-pairs (reverse (message-tokenize-header
					      gnus-newsgroup-name "."))
				    gnus-picons-news-directory)
	     t nil "."))
      (add-hook 'gnus-summary-exit-hook 'gnus-picons-remove-all))))

(defun gnus-picons-make-path (dir subdirs)
  "Make a directory name from a base DIR and a list of SUBDIRS.
Returns a directory name build by concatenating DIR and all elements of
SUBDIRS with \"/\" between elements."
  (while subdirs
    (setq dir (file-name-as-directory (concat dir (pop subdirs)))))
  dir)

(defsubst gnus-picons-try-suffixes (file)
  (let ((suffixes gnus-picons-file-suffixes)
	f)
    (while (and suffixes
		(not (file-exists-p (setq f (concat file (pop suffixes))))))
      (setq f nil))
    f))

(defun gnus-picons-lookup (addrs dirs)
  "Lookup the picon for ADDRS in databases DIRS.
Returns the picon filename or NIL if none found."
  (let (result)
    (while (and dirs (null result))
      (setq result
	    (gnus-picons-try-suffixes
	     (expand-file-name "face."
			       (gnus-picons-make-path
				(file-name-as-directory
				 (concat
				  (file-name-as-directory gnus-picons-database)
				  (pop dirs)))
				(reverse addrs))))))
    result))

(defun gnus-picons-lookup-user-internal (user domains)
  (let ((dirs gnus-picons-user-directories)
	picon)
    (while (and dirs (null picon))
      (let ((dir (list (pop dirs)))
	    (domains domains))
	(while (and domains (null picon))
	  (setq picon (gnus-picons-lookup (cons user domains) dir))
	  (pop domains))
	;; Also make a try MISC subdir
	(unless picon
	  (setq picon (gnus-picons-lookup (list user "MISC") dir)))))

    picon))

(defun gnus-picons-lookup-user (user domains)
  "Lookup the picon for USER at DOMAINS.
USER is a string containing a name.
DOMAINS is a list of strings from the fully qualified domain name."
  (or (gnus-picons-lookup-user-internal user domains)
      (gnus-picons-lookup-user-internal "unknown" domains)))

(defun gnus-picons-lookup-pairs (domains directories)
  "Lookup picons for DOMAINS and all its parents in DIRECTORIES.
Returns a list of PAIRS whose CAR is the picon filename or NIL if
none, and whose CDR is the corresponding element of DOMAINS."
  (let (picons)
    (while domains
      (push (list (gnus-picons-lookup (cons "unknown" domains)
				      (if (listp directories)
					  directories
					(list directories)))
		  (pop domains))
	    picons))
    picons))

(defun gnus-picons-display-picon-or-name (picon name &optional xface-p right-p)
  (if picon
      (gnus-picons-try-to-find-face picon xface-p name right-p)
    (list (make-annotation name nil 'text nil nil nil right-p))))

(defun gnus-picons-display-pairs (pairs &optional bar-p xface-p dot-p right-p)
  "Display picons in list PAIRS."
  (let ((bar (and bar-p (or gnus-picons-display-as-address
			  (annotations-in-region (point)
						 (min (point-max) (1+ (point)))
						 (current-buffer)))))
	(domain-p (and gnus-picons-display-as-address dot-p))
	picons)
    (while pairs
      (let ((pair (pop pairs)))
	(setq picons (nconc (if (and domain-p picons (not right-p))
				(list (make-annotation
				       dot-p nil 'text nil nil nil right-p)))
			    (gnus-picons-display-picon-or-name (car pair)
							       (cadr pair)
							       xface-p
							       right-p)
			    (if (and domain-p pairs right-p)
				(list (make-annotation
				       dot-p nil 'text nil nil nil right-p)))
			    (when (and bar domain-p)
			      (setq bar nil)
			      (gnus-picons-try-to-find-face
			       (expand-file-name "bar.xbm"
						 gnus-xmas-glyph-directory)
			       nil nil t))
			    picons))))
    picons))

(defvar gnus-picons-glyph-alist nil)

(defun gnus-picons-try-to-find-face (path &optional xface-p part rightp)
  "If PATH exists, display it as a bitmap.  Returns t if succeeded."
  (let ((glyph (and (not xface-p)
		    (cdr (assoc path gnus-picons-glyph-alist)))))
    (when (or glyph (file-exists-p path))
      (unless glyph
	(setq glyph (make-glyph path))
	(unless xface-p
	  (push (cons path glyph) gnus-picons-glyph-alist))
	(set-glyph-face glyph 'default))
      (let ((new (make-annotation glyph (point) 'text nil nil nil rightp)))
	(nconc
	 (list new)
	 (when (and (eq major-mode 'gnus-article-mode)
		    (not gnus-picons-display-as-address)
		    (not part))
	   (list (make-annotation " " (point) 'text nil nil nil rightp)))
	 (when (and part gnus-picons-display-as-address)
	   (let ((txt (make-annotation part (point) 'text nil nil nil rightp)))
	     (hide-annotation txt)
	     (set-extent-property txt 'its-partner new)
	     (set-extent-property txt 'keymap gnus-picons-map)
	     (set-extent-property txt 'mouse-face gnus-article-mouse-face)
	     (set-extent-property new 'its-partner txt)
	     (set-extent-property new 'keymap gnus-picons-map)
	     (list txt))))))))

(defun gnus-picons-toggle-extent (event)
  "Toggle picon glyph at given point"
  (interactive "e")
  (let* ((ant1 (event-glyph-extent event))
	 (ant2 (extent-property ant1 'its-partner)))
    (when (and (annotationp ant1) (annotationp ant2))
      (reveal-annotation ant2)
      (hide-annotation ant1))))

(gnus-add-shutdown 'gnus-picons-close 'gnus)

(defun gnus-picons-close ()
  "Shut down the picons."
  (setq gnus-picons-glyph-alist nil))

(provide 'gnus-picon)

;;; gnus-picon.el ends here
