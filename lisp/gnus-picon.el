;;; gnus-picon.el --- displaying pretty icons in Gnus

;; Copyright (C) 1996, 1997, 1998, 1999, 2000, 2001
;;      Free Software Foundation, Inc.

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

;; There are three picon types relevant to Gnus:
;;
;; Persons: person@subdomain.dom
;;          users/dom/subdomain/person/face.gif
;;          usenix/dom/subdomain/person/face.gif
;;          misc/MISC/person/face.gif
;; Domains: subdomain.dom
;;          domain/dom/subdomain/unknown/face.gif
;; Groups:  comp.lang.lisp
;;          news/comp/lang/lisp/unknown/face.gif

;;; Code:

(require 'gnus)
(require 'custom)
(require 'gnus-art)

;;; User variables:

(defgroup picon nil
  "Show pictures of people, domains, and newsgroups."
  :group 'gnus-visual)

(defcustom gnus-picon-databases '("/usr/lib/picon" "/usr/local/faces")
  "*Defines the location of the faces database.
For information on obtaining this database of pretty pictures, please
see http://www.cs.indiana.edu/picons/ftp/index.html"
  :type 'directory
  :group 'picon)

(defcustom gnus-picon-news-directories '("news")
  "*List of directories to search for newsgroups faces."
  :type '(repeat string)
  :group 'picon)

(defcustom gnus-picon-user-directories '("users" "usenix" "local" "misc")
  "*List of directories to search for user faces."
  :type '(repeat string)
  :group 'picon)

(defcustom gnus-picon-domain-directories '("domains")
  "*List of directories to search for domain faces.
Some people may want to add \"unknown\" to this list."
  :type '(repeat string)
  :group 'picon)

(defcustom gnus-picon-file-types
  (let ((types (list "xbm")))
    (when (gnus-image-type-available-p 'gif)
      (push "gif" types))
    (when (gnus-image-type-available-p 'xpm)
      (push "xpm" types))
    types)
  "*List of suffixes on picon file names to try."
  :type '(repeat string)
  :group 'picon)

(defface gnus-picon-xbm-face '((t (:foreground "black" :background "white")))
  "Face to show xbm picon in."
  :group 'picon)

(defface gnus-picon-face '((t (:foreground "black" :background "white")))
  "Face to show picon in."
  :group 'picon)

;;; Internal variables:

(defvar gnus-picon-setup-p nil)
(defvar gnus-picon-glyph-alist nil
  "Picon glyphs cache.
List of pairs (KEY . GLYPH) where KEY is either a filename or an URL.")

;;; Functions:

(defsubst gnus-picon-split-address (address)
  (setq address (split-string address "@"))
  (if (stringp (cadr address))
      (cons (car address) (split-string (cadr address) "\\."))
    (if (stringp (car address))
	(split-string (car address) "\\."))))

(defun gnus-picon-find-face (address directories &optional exact)
  (let* ((databases gnus-picon-databases)
	 (address (gnus-picon-split-address address))
	 (user (pop address))
	 database directory found instance base)
    (while (and (not found)
		(setq database (pop databases)))
      (while (and (not found)
		  (setq directory (pop directories)))
	(setq base (expand-file-name directory database))
	;; Kludge to search misc/MISC for users.
	(when (string= directory "misc")
	  (setq address '("MISC")))
	(while (and (not found)
		    address)
	  (setq found (gnus-picon-find-image
		       (concat base "/" (mapconcat 'identity
						   (reverse address)
						   "/")
			       "/" user "/")))
	  (if exact
	      (setq address nil)
	    (pop address)))))
    found))

(defun gnus-picon-find-image (directory)
  (let ((types gnus-picon-file-types)
	found type file)
    (while (and (not found)
		(setq type (pop types)))
      (setq found (file-exists-p (setq file (concat directory "face." type)))))
    (if found
	file
      nil)))

(defun gnus-picon-insert-glyph (glyph category)
  "Insert GLYPH into the buffer.
GLYPH can be either a glyph or a string."
  (if (stringp glyph)
      (insert glyph)
    (gnus-add-wash-type category)
    (gnus-add-image category (car glyph))
    (gnus-put-image (car glyph) (cdr glyph))))

(defun gnus-picon-create-glyph (file)
  (or (cdr (assoc file gnus-picon-glyph-alist))
      (cdar (push (cons file (gnus-create-image file))
		  gnus-picon-glyph-alist))))

;;; Functions that does picon transformations:

(defun gnus-picon-transform-address (header category)
  (gnus-with-article-headers
    (let ((addresses
	   (mail-header-parse-addresses (mail-fetch-field header)))
	  first spec file)
      (dolist (address addresses)
	(setq address (car address)
	      first t)
	(when (and (stringp address)
		   (setq spec (gnus-picon-split-address address)))
	  (when (setq file (gnus-picon-find-face
			    address gnus-picon-user-directories))
	    (setcar spec (cons (gnus-picon-create-glyph file)
			       (car spec))))
	  (dotimes (i (1- (length spec)))
	    (when (setq file (gnus-picon-find-face
			      (concat "unknown@"
				      (mapconcat
				       'identity (nthcdr (1+ i) spec) "."))
			      gnus-picon-domain-directories t))
	      (setcar (nthcdr (1+ i) spec)
		      (cons (gnus-picon-create-glyph file)
			    (nth (1+ i) spec)))))
	  
	  (gnus-article-goto-header header)
	  (mail-header-narrow-to-field)
	  (when (search-forward address nil t)
	    (delete-region (match-beginning 0) (match-end 0))
	    (while spec
	      (gnus-picon-insert-glyph (pop spec) category)
	      (when spec
		(if (not first)
		    (insert ".")
		  (insert "@")
		  (setq first nil))))))))))

(defun gnus-picon-transform-newsgroups (header)
  (interactive)
  (gnus-with-article-headers
    (let ((groups
	   (sort
	    (message-tokenize-header (mail-fetch-field header))
	    (lambda (g1 g2) (> (length g1) (length g2)))))
	  spec file)
      (dolist (group groups)
	(setq spec (nreverse (split-string group "[.]")))
	(dotimes (i (length spec))
	  (when (setq file (gnus-picon-find-face
			    (concat "unknown@"
				    (mapconcat
				     'identity (nthcdr i spec) "."))
			    gnus-picon-news-directories t))
	    (setcar (nthcdr i spec)
		    (cons (gnus-picon-create-glyph file)
			  (nth i spec)))))
	
	(gnus-article-goto-header header)
	(mail-header-narrow-to-field)
	(when (search-forward group nil t)
	  (delete-region (match-beginning 0) (match-end 0))
	  (setq spec (nreverse spec))
	  (while spec
	    (gnus-picon-insert-glyph (pop spec) 'newsgroups-picon)
	    (when spec
	      (insert "."))))))))

;;; Commands:

;;;###autoload
(defun gnus-treat-from-picon ()
  "Display picons in the From header.
If picons are already displayed, remove them."
  (interactive)
  (gnus-with-article-buffer
    (if (memq 'from-picon gnus-article-wash-types)
	(gnus-delete-images 'from-picon)
      (gnus-picon-transform-address "from" 'from-picon))))

;;;###autoload
(defun gnus-treat-mail-picon ()
  "Display picons in the Cc and To headers.
If picons are already displayed, remove them."
  (interactive)
  (gnus-with-article-buffer
    (if (memq 'mail-picon gnus-article-wash-types)
	(gnus-delete-images 'mail-picon)
      (gnus-picon-transform-address "cc" 'mail-picon)
      (gnus-picon-transform-address "to" 'mail-picon))))

;;;###autoload
(defun gnus-treat-newsgroups-picon ()
  "Display picons in the Newsgroups and Followup-To headers.
If picons are already displayed, remove them."
  (interactive)
  (gnus-with-article-buffer
    (if (memq 'newsgroups-picon gnus-article-wash-types)
	(gnus-delete-images 'newsgroups-picon)
      (gnus-picon-transform-newsgroups "newsgroups")
      (gnus-picon-transform-newsgroups "followup-to"))))

(provide 'gnus-picon)

;;; gnus-picon.el ends here
