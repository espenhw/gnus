;; gnus-picon.el:  Copyright (C) 1995 Wes Hardaker
;; Icon hacks for displaying pretty icons in Gnus.
;;
;; Author:  Wes hardaker
;;          hardaker@ece.ucdavis.edu
;; 
;; Usage:
;;     - You must have XEmacs to use this.
;;     - (add-hook 'gnus-article-display-hook 'gnus-article-display-picons t)
;;       This HAS to have the 't' flag above to make sure it appends the hook.
;;     - Read the variable descriptions below.
;;
;; Warnings:
;;     - I'm not even close to being a lisp expert.
;;
;; TODO:
;;     - Following the Gnus motto: We've got to build him bigger,
;;       better, stronger, faster than before...  errr....  sorry.
;;     - Create a seperate frame to store icons in so icons are
;;       visibile immediately upon entering a group rather than just
;;       at the top of the article buffer.
;;
;; 

(require 'xpm)
(require 'annotations)

(defvar gnus-picons-database "/usr/local/faces"
  "defines the location of the faces database.  For information on
  obtaining this database of pretty pictures, please see
  http://www.cs.indiana.edu/picons/ftp/index.html"
)

(defvar gnus-picons-news-directory "news"
  "Sub-directory of the faces database containing the icons for
  newsgroups."
)

(defvar gnus-picons-user-directories '("local" "users" "usenix" "misc/MISC")
  "List of directories to search for user faces."
)

(defvar gnus-picons-domain-directories '("domains")
  "List of directories to search for domain faces.  Some people may
  want to add \"unknown\" to this list."
)

(defun gnus-article-display-picons ()
  "prepare article buffer with pretty pictures"
  (interactive)
  (if (featurep 'xpm)
      (save-excursion
	(beginning-of-buffer)
	(open-line 1)
	(let* ((iconpoint (point)) (from (mail-fetch-field "from"))
	       (username 
		(progn
		  (string-match "\\([-_a-zA-Z0-9]+\\)@" from)
		  (match-string 1 from)))
	       (hostpath
		(gnus-picons-reverse-domain-path
		 (replace-in-string
		  (replace-in-string from ".*@\\([_a-zA-Z0-9-.]+\\).*" "\\1") 
		  "\\." "/"))))
	  (if (equal username from)
	      (setq username (replace-in-string from 
						".*<\\([_a-zA-Z0-9-.]+\\)>.*" 
						"\\1")))
	  (insert username)
	  (gnus-picons-insert-face-if-exists 
	   (concat gnus-picons-database "/" gnus-picons-news-directory)
	   (concat (replace-in-string gnus-newsgroup-name "\\." "/") "/unknown")
	   iconpoint)
	  (mapcar '(lambda (pathpart) 
		     (gnus-picons-insert-face-if-exists 
		      (concat gnus-picons-database "/" pathpart)
		      (concat hostpath "/" username) 
		      iconpoint)) 
		  gnus-picons-user-directories)
	  (mapcar '(lambda (pathpart) 
		     (gnus-picons-insert-face-if-exists 
		      (concat gnus-picons-database "/" pathpart)
		      (concat hostpath "/" "unknown") 
		      iconpoint)) 
		  gnus-picons-domain-directories)
	  ))))

(defun gnus-picons-insert-face-if-exists (path filename ipoint)
  "inserts a face at point if I can find one"
  (let ((pathfile (concat path "/" filename "/face")))
    (let ((newfilename 
	   (replace-in-string filename 
			      "[_a-zA-Z0-9-]+/\\([_A-Za-z0-9-]+\\)$" "\\1")))
      (if (not (equal filename newfilename))
	  (gnus-picons-insert-face-if-exists path newfilename ipoint)))
    (if (not (gnus-picons-try-to-find-face (concat pathfile ".xpm") ipoint))
	(gnus-picons-try-to-find-face (concat pathfile ".xbm") ipoint))
    )
  )
  

(defun gnus-picons-try-to-find-face (path ipoint)
  "if path exists, display it as a bitmap.  Returns t if succedded."
    (if (file-exists-p path)
	(progn
	  (setq gl (make-glyph path))
	  (set-glyph-face gl 'default)
	  (setq annot (make-annotation gl ipoint 'text))
	  t)
;      (insert (format "no:  %s\n" path))
      nil))

(defun gnus-picons-reverse-domain-path (str)
  "a/b/c/d -> d/c/b/a"
  (if (equal (replace-in-string str "^[^/]*$" "") "")
      str
    (concat (replace-in-string str "^.*/\\([_a-zA-Z0-9-]+\\)$" "\\1") "/"
	    (gnus-picons-reverse-domain-path 
	     (replace-in-string str "^\\(.*\\)/[_a-zA-Z0-9-]+$" "\\1")))))


