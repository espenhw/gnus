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

(defcustom gnus-picons-user-directories '("local" "users" "usenix" "misc/MISC")
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
This has only an effect if `gnus-picons-display-where' hs value article."
  :type 'boolean
  :group 'picons)

(defvar gnus-picons-map (make-sparse-keymap "gnus-picons-keys")
 "keymap to hide/show picon glyphs")

(define-key gnus-picons-map [(button2)] 'gnus-picons-toggle-extent)

;;; Internal variables.

(defvar gnus-group-annotations nil)
(defvar gnus-article-annotations nil)
(defvar gnus-x-face-annotations nil)

(defun gnus-picons-remove (plist)
  (let ((listitem (car plist)))
    (while (setq listitem (car plist))
      (when (annotationp listitem)
	(delete-annotation listitem))
      (setq plist (cdr plist)))))

(defun gnus-picons-remove-all ()
  "Removes all picons from the Gnus display(s)."
  (interactive)
  (gnus-picons-remove gnus-article-annotations)
  (gnus-picons-remove gnus-group-annotations)
  (gnus-picons-remove gnus-x-face-annotations)
  (setq gnus-article-annotations nil
        gnus-group-annotations nil
	gnus-x-face-annotations nil)
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

(defun gnus-picons-article-display-x-face ()
  "Display the x-face header bitmap in the 'gnus-picons-display-where buffer."
  ;; delete any old ones.
  (gnus-picons-remove gnus-x-face-annotations)
  (setq gnus-x-face-annotations nil)
  ;; display the new one.
  (let ((gnus-article-x-face-command 'gnus-picons-display-x-face))
    (gnus-article-display-x-face)))

(defun gnus-picons-display-x-face (beg end)
  "Function to display the x-face header in the picons window.
To use:  (setq gnus-article-x-face-command 'gnus-picons-display-x-face)"
  (interactive)
  ;; convert the x-face header to a .xbm file
  (let ((process-connection-type nil)
	(process nil))
    (process-kill-without-query
     (setq process (start-process
		    "gnus-x-face" nil shell-file-name shell-command-switch
		    gnus-picons-convert-x-face)))
    (process-send-region "gnus-x-face" beg end)
    (process-send-eof "gnus-x-face")
    ;; wait for it.
    (while (not (equal (process-status process) 'exit))
      (sleep-for .1)))
  ;; display it
  (save-excursion
    (set-buffer (get-buffer-create (gnus-get-buffer-name
				    gnus-picons-display-where)))
    (gnus-add-current-to-buffer-list)
    (goto-char (point-min))
    (let (buffer-read-only)
      (unless (eolp)
	(push (make-annotation "\n" (point) 'text)
	      gnus-x-face-annotations))
      ;; append the annotation to gnus-article-annotations for deletion.
      (setq gnus-x-face-annotations
	    (append
	     (gnus-picons-try-to-find-face gnus-picons-x-face-file-name t)
	     gnus-x-face-annotations)))
    ;; delete the tmp file
    (delete-file gnus-picons-x-face-file-name)))

(defun gnus-article-display-picons ()
  "Display faces for an author and his/her domain in gnus-picons-display-where."
  (interactive)
  ;; let drawing catch up
  (when gnus-picons-refresh-before-display
    (sit-for 0))
  (let ((first t)
	from at-idx databases)
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
			     (nreverse (message-tokenize-header
					gnus-local-domain "."))
			   '("."))
		       (nreverse (message-tokenize-header
				  (substring from (1+ at-idx)) ".")))))
	  (set-buffer (get-buffer-create
		       (gnus-get-buffer-name gnus-picons-display-where)))
	  (gnus-add-current-to-buffer-list)
	  (goto-char (point-min))
	  (if (and (eq gnus-picons-display-where 'article)
		   gnus-picons-display-article-move-p)
	      (when (search-forward "\n\n" nil t)
		(forward-line -1))
	    (unless (eolp)
	      (push (make-annotation "\n" (point) 'text)
		    gnus-article-annotations)))

	  (gnus-picons-remove gnus-article-annotations)
	  (setq gnus-article-annotations nil)

	  ;; look for domain paths.
	  (setq databases gnus-picons-domain-directories)
	  (while databases
	    (setq gnus-article-annotations
		  (nconc (gnus-picons-insert-face-if-exists
			  (car databases)
			  addrs
			  "unknown" (or gnus-picons-display-as-address
					gnus-article-annotations) t t)
			 gnus-article-annotations))
	    (setq databases (cdr databases)))

	  ;; add an '@' if displaying as address
	  (when gnus-picons-display-as-address
	    (setq gnus-article-annotations
		  (nconc gnus-article-annotations
			 (list
			  (make-annotation "@" (point) 'text nil nil nil t)))))

	  ;; then do user directories,
	  (let (found)
	    (setq databases gnus-picons-user-directories)
	    (setq username (downcase username))
	    (while databases
	      (setq found
		    (nconc (gnus-picons-insert-face-if-exists
			    (car databases) addrs username
			    (or gnus-picons-display-as-address
				gnus-article-annotations) nil t)
			   found))
	      (setq databases (cdr databases)))
	    ;; add their name if no face exists
	    (when (and gnus-picons-display-as-address (not found))
	      (setq found
		    (list
		     (make-annotation username (point) 'text nil nil nil t))))
	    (setq gnus-article-annotations
		  (nconc found gnus-article-annotations)))

	  (add-hook 'gnus-summary-exit-hook 'gnus-picons-remove-all))))))

(defun gnus-group-display-picons ()
  "Display icons for the group in the gnus-picons-display-where buffer."
  (interactive)
  ;; let display catch up so far
  (when gnus-picons-refresh-before-display
    (sit-for 0))
  (when (and (featurep 'xpm)
	     (or (not (fboundp 'device-type)) (equal (device-type) 'x)))
    (save-excursion
      (set-buffer (get-buffer-create
		   (gnus-get-buffer-name gnus-picons-display-where)))
      (gnus-add-current-to-buffer-list)
      (goto-char (point-min))
      (if (and (eq gnus-picons-display-where 'article)
	       gnus-picons-display-article-move-p)
	  (when (search-forward "\n\n" nil t)
	    (forward-line -1))
	(unless (eolp)
	  (push (make-annotation "\n" (point) 'text)
		gnus-group-annotations)))
      (cond
       ((listp gnus-group-annotations)
	(mapc #'(lambda (ext) (when (extent-live-p ext)
				(delete-annotation ext)))
	      gnus-group-annotations)
	(setq gnus-group-annotations nil))
       ((annotationp gnus-group-annotations)
	(delete-annotation gnus-group-annotations)
	(setq gnus-group-annotations nil)))
      (gnus-picons-remove gnus-group-annotations)
      (setq gnus-group-annotations
	    (gnus-picons-insert-face-if-exists
	     gnus-picons-news-directory
	     (message-tokenize-header gnus-newsgroup-name ".")
	     "unknown" nil t))
      (add-hook 'gnus-summary-exit-hook 'gnus-picons-remove-all))))

(defsubst gnus-picons-try-suffixes (file)
  (let ((suffixes gnus-picons-file-suffixes)
	f)
    (while (and suffixes
		(not (file-exists-p (setq f (concat file (pop suffixes))))))
      (setq f nil))
    f))

(defun gnus-picons-insert-face-if-exists (database addrs filename &optional
						   nobar-p dots rightp)
  "Inserts a face at point if I can find one"
  ;; '(gnus-picons-insert-face-if-exists
  ;;    "Database" '("edu" "indiana" "cs") "Name")
  ;; looks for:
  ;;  1. edu/indiana/cs/Name
  ;;  2. edu/indiana/Name
  ;;  3. edu/Name
  ;; '(gnus-picons-insert-face-if-exists
  ;;     "Database/MISC" '("edu" "indiana" "cs") "Name")
  ;; looks for:
  ;;  1. MISC/Name
  ;; The special treatment of MISC doesn't conform with the conventions for
  ;; picon databases, but otherwise we would always see the MISC/unknown face.
  (let ((bar (and (not nobar-p)
		  (or gnus-picons-display-as-address
		      (annotations-in-region
		       (point) (min (point-max) (1+ (point)))
		       (current-buffer)))))
	(path (concat (file-name-as-directory gnus-picons-database)
		      database "/"))
	(domainp (and gnus-picons-display-as-address dots))
	picons found bar-ann cur first)
    (when (string-match "/MISC" database)
      (setq addrs '(".")))
    (while (and addrs
		(file-accessible-directory-p path))
      (setq cur (pop addrs)
	    path (concat path cur "/"))
      (if (setq found
		(gnus-picons-try-suffixes (concat path filename "/face.")))
	  (progn
	    (setq picons (nconc (when (and domainp first rightp)
				  (list (make-annotation
					 "." (point) 'text
					 nil nil nil rightp)
					picons))
				(gnus-picons-try-to-find-face
				 found nil (if domainp cur filename) rightp)
				(when (and domainp first (not rightp))
				  (list (make-annotation
					 "." (point) 'text
					 nil nil nil rightp)
					picons))
				picons)))
	(when domainp
	  (setq picons
		(nconc
		 (list (make-annotation
			(if first (concat (if (not rightp) ".") cur
					  (if rightp ".")) cur)
			(point) 'text nil nil nil rightp))
		 picons))))
      (when (and bar (or domainp found))
	(setq bar-ann (gnus-picons-try-to-find-face
		       (concat gnus-xmas-glyph-directory "bar.xbm")
		       nil nil t))
	(when bar-ann
	  (setq picons (nconc picons bar-ann))
	  (setq bar nil)))
      (setq first t))
    (when (and addrs domainp)
      (let ((it (mapconcat 'downcase (nreverse addrs) ".")))
	(setq picons
	      (nconc picons (list (make-annotation
				   (if first
				       (concat (if (not rightp) ".")
					       it (if rightp "."))
				     it)
				   (point) 'text
				   nil nil nil rightp))))))
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
	     (set-extent-property new 'keymap gnus-picons-map))))))))

(defun gnus-picons-reverse-domain-path (str)
  "a/b/c/d -> d/c/b/a"
  (mapconcat 'downcase (nreverse (message-tokenize-header str "/")) "/"))

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
