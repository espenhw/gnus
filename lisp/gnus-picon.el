;;; gnus-picon.el --- displaying pretty icons in Gnus
;; Copyright (C) 1996 Free Software Foundation, Inc.

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

;; Usage:
;;     - You must have XEmacs (19.12 or above I think) to use this.
;;     - Read the variable descriptions below.
;;
;;     - chose a setup:
;;
;;       1) display the icons in its own buffer:
;;
;;          (add-hook 'gnus-article-display-hook 'gnus-article-display-picons t)
;;          (add-hook 'gnus-summary-prepare-hook 'gnus-group-display-picons t)
;;          (setq gnus-picons-display-where 'picons)
;;
;;          Then add the picons buffer to your display configuration:
;;          The picons buffer needs to be at least 48 pixels high,
;;          which for me is 5 lines:
;;
;;          (gnus-add-configuration
;;           '(article (vertical 1.0 
;;                             (group 6)
;;                             (picons 5)
;;                             (summary .25 point)
;;                             (article 1.0))))
;;
;;          (gnus-add-configuration
;;           '(summary (vertical 1.0 (group 6)
;;                      (picons 5)
;;                      (summary 1.0 point))))
;;
;;       2) display the icons in the summary buffer
;;
;;          (add-hook 'gnus-article-display-hook 'gnus-article-display-picons t)
;;          (add-hook 'gnus-summary-prepare-hook 'gnus-group-display-picons t)
;;          (setq gnus-picons-display-where 'summary)
;;
;;       3) display the icons in the article buffer
;;
;;          (add-hook 'gnus-article-display-hook 'gnus-article-display-picons t)
;;          (add-hook 'gnus-article-prepare-hook 'gnus-group-display-picons t)
;;          (setq gnus-picons-display-where 'article)
;;
;;
;; Warnings:
;;     - I'm not even close to being a lisp expert.
;;     - The 't' (append) flag MUST be in the add-hook line
;;
;; TODO:
;;     - Remove the TODO section in the headers.
;;

;;; Code:

(require 'xpm)
(require 'annotations)
(eval-when-compile (require 'cl))

(defvar gnus-picons-buffer "*Icon Buffer*"
  "Buffer name to display the icons in if gnus-picons-display-where is 'picons.")

(defvar gnus-picons-display-where 'picons
  "Where to display the group and article icons.")

(defvar gnus-picons-database "/usr/local/faces"
  "Defines the location of the faces database.  
For information on obtaining this database of pretty pictures, please
see http://www.cs.indiana.edu/picons/ftp/index.html" )

(defvar gnus-picons-news-directory "news"
  "Sub-directory of the faces database containing the icons for newsgroups."
)

(defvar gnus-picons-user-directories '("local" "users" "usenix" "misc/MISC")
  "List of directories to search for user faces."
)

(defvar gnus-picons-domain-directories '("domains")
  "List of directories to search for domain faces.  
Some people may want to add \"unknown\" to this list."
)

(defvar gnus-picons-x-face-file-name 
  (format "/tmp/picon-xface.%s.xbm" (user-login-name))
  "The name of the file in which to store the converted X-face header.")

(defvar gnus-picons-convert-x-face (format "{ echo '/* Width=48, Height=48 */'; uncompface; } | icontopbm | pbmtoxbm > %s" gnus-picons-x-face-file-name)
  "Command to convert the x-face header into a xbm file."
)

(defvar gnus-picons-file-suffixes
  (when (featurep 'x)
    (let ((types (list "xbm")))
      (when (featurep 'gif)
	(push "gif" types))
      (when (featurep 'xpm)
	(push "xpm" types))
      types))
  "List of suffixes on picon file names to try.")

(defvar gnus-picons-display-article-move-p t
  "*Whether to move point to first empty line when displaying picons.
This has only an effect if `gnus-picons-display-where' hs value article.")

;;; Internal variables.
       
(defvar gnus-group-annotations nil)
(defvar gnus-article-annotations nil)
(defvar gnus-x-face-annotations nil)

(defun gnus-picons-remove (plist)
  (let ((listitem (car plist)))
    (while (setq listitem (car plist))
      (if (annotationp listitem)
          (delete-annotation listitem))
      (setq plist (cdr plist))))
  )

(defun gnus-picons-remove-all ()
  "Removes all picons from the Gnus display(s)."
  (interactive)
  (gnus-picons-remove gnus-article-annotations)
  (gnus-picons-remove gnus-group-annotations)
  (gnus-picons-remove gnus-x-face-annotations)
  (setq gnus-article-annotations nil
        gnus-group-annotations nil
	gnus-x-face-annotations nil)
  (if (bufferp gnus-picons-buffer)
      (kill-buffer gnus-picons-buffer))
  )

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
  (let (from at-idx databases)
    (when (and (featurep 'xpm) 
	       (or (not (fboundp 'device-type)) (equal (device-type) 'x))
	       (setq from (mail-fetch-field "from"))
	       (setq from (downcase (or (cadr (mail-extract-address-components
					       from))
					""))
		     at-idx (string-match "@" from)))
      (save-excursion
	(let ((username (substring from 0 at-idx))
	      (addrs (nreverse
		      (message-tokenize-header (substring from (1+ at-idx))
					       "."))))
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

	  (setq databases (append gnus-picons-user-directories
				  gnus-picons-domain-directories))
	  (while databases
	    (setq gnus-article-annotations
		  (nconc (gnus-picons-insert-face-if-exists
			  (car databases)
			  addrs
			  "unknown")
			 (gnus-picons-insert-face-if-exists
			  (car databases)
			  addrs
			  (downcase username) t)
			 gnus-article-annotations))
	    (setq databases (cdr databases)))
	  (add-hook 'gnus-summary-exit-hook 'gnus-picons-remove-all))))))

(defun gnus-group-display-picons ()
  "Display icons for the group in the gnus-picons-display-where buffer." 
  (interactive)
  (when (and (featurep 'xpm) 
	     (or (not (fboundp 'device-type)) (equal (device-type) 'x)))
    (save-excursion
      (set-buffer (get-buffer-create
		   (gnus-get-buffer-name gnus-picons-display-where)))
      (gnus-add-current-to-buffer-list)
      (goto-char (point-min))
      (if (and (eq gnus-picons-display-where 'article)
	       gnus-picons-display-article-move-p)
	  (if (search-forward "\n\n" nil t)
	      (forward-line -1))
	(unless (eolp)
	  (push (make-annotation "\n" (point) 'text)
		gnus-group-annotations)))
      (cond 
       ((listp gnus-group-annotations)
	(mapcar 'delete-annotation gnus-group-annotations)
	(setq gnus-group-annotations nil))
       ((annotationp gnus-group-annotations)
	(delete-annotation gnus-group-annotations)
	(setq gnus-group-annotations nil)))
      (gnus-picons-remove gnus-group-annotations)
      (setq gnus-group-annotations
	    (gnus-picons-insert-face-if-exists
	     gnus-picons-news-directory
	     (message-tokenize-header gnus-newsgroup-name ".")
	     "unknown"))
      (add-hook 'gnus-summary-exit-hook 'gnus-picons-remove-all))))

(defsubst gnus-picons-try-suffixes (file)
  (let ((suffixes gnus-picons-file-suffixes)
	f)
    (while (and suffixes
		(not (file-exists-p (setq f (concat file (pop suffixes))))))
      (setq f nil))
    f))

(defun gnus-picons-insert-face-if-exists (database addrs filename &optional
						   nobar-p)
  "Inserts a face at point if I can find one"
  ;; '(gnus-picons-insert-face-if-exists
  ;     "Database" '("edu" "indiana" "cs") "Name")
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
		  (annotations-in-region 
		   (point) (min (point-max) (1+ (point)))
		   (current-buffer))))
	(path (concat (file-name-as-directory gnus-picons-database)
		      database "/"))
	picons found bar-ann)
    (if (string-match "/MISC" database)
	(setq addrs '("")))
    (while (and addrs
		(file-accessible-directory-p path))
      (setq path (concat path (pop addrs) "/"))
      (when (setq found
		  (gnus-picons-try-suffixes
		   (concat path filename "/face.")))
	(when bar
	  (setq bar-ann (gnus-picons-try-to-find-face 
			 (concat gnus-xmas-glyph-directory "bar.xbm")))
	  (when bar-ann
	    (setq picons (nconc picons bar-ann))
	    (setq bar nil)))
	(setq picons (nconc (gnus-picons-try-to-find-face found)
			    picons))))
    (nreverse picons)))

(defvar gnus-picons-glyph-alist nil)
      
(defun gnus-picons-try-to-find-face (path &optional xface-p)
  "If PATH exists, display it as a bitmap.  Returns t if succedded."
  (let ((glyph (and (not xface-p)
		    (cdr (assoc path gnus-picons-glyph-alist)))))
    (when (or glyph (file-exists-p path))
      (unless glyph
	(setq glyph (make-glyph path))
	(unless xface-p
	  (push (cons path glyph) gnus-picons-glyph-alist))
	(set-glyph-face glyph 'default))
      (nconc
       (list (make-annotation glyph (point) 'text))
       (when (eq major-mode 'gnus-article-mode)
	 (list (make-annotation " " (point) 'text)))))))

(defun gnus-picons-reverse-domain-path (str)
  "a/b/c/d -> d/c/b/a"
  (mapconcat 'downcase (nreverse (message-tokenize-header str "/")) "/"))

(gnus-add-shutdown 'gnus-picons-close 'gnus)

(defun gnus-picons-close ()
  "Shut down the picons."
  (setq gnus-picons-glyph-alist nil))

(provide 'gnus-picon)

;;; gnus-picon.el ends here
