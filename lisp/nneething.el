;;; nneething.el --- random file access for Gnus
;; Copyright (C) 1995,96 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@ifi.uio.no>
;; 	Masanobu UMEDA <umerin@flab.flab.fujitsu.junet>
;; Keywords: news, mail

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

;; Based on nnspool.el by Masanobu UMEDA <umerin@flab.flab.fujitsu.junet>.
;; For an overview of what the interface functions do, please see the
;; Gnus sources.  

;;; Code:

(require 'nnheader)
(require 'nnmail)
(eval-when-compile (require 'cl))

(defvar nneething-map-file-directory "~/.nneething/"
  "*Where nneething stores the map files.")

(defvar nneething-map-file ".nneething"
  "*Name of the map files.")

(defvar nneething-exclude-files nil
  "*Regexp saying what files to exclude from the group.
If this variable is nil, no files will be excluded.")



;;; Internal variables. 

(defconst nneething-version "nneething 1.0"
  "nneething version.")

(defvar nneething-current-directory nil
  "Current news group directory.")

(defvar nneething-status-string "")
(defvar nneething-group-alist nil)

(defvar nneething-message-id-number 0)
(defvar nneething-work-buffer " *nneething work*")

(defvar nneething-directory nil)
(defvar nneething-group nil)
(defvar nneething-map nil)
(defvar nneething-read-only nil)
(defvar nneething-active nil)



(defvar nneething-current-server nil)
(defvar nneething-server-alist)
(defvar nneething-server-variables 
  `((nneething-directory ,nneething-directory)
    (nneething-current-directory nil)
    (nneething-status-string "")
    (nneething-active nil)
    (nneething-read-only nil)
    (nneething-map nil)
    (nneething-group nil)
    (nneething-work-buffer ,nneething-work-buffer)
    (nneething-message-id-number ,nneething-message-id-number)
    (nneething-exclude-files nil)
    (nneething-map-file ,nneething-map-file)
    (nneething-map-file-directory ,nneething-map-file-directory)
    (nneething-group-alist nil)))



;;; Interface functions.

(defun nneething-retrieve-headers (articles &optional group server fetch-old)
  (nneething-possibly-change-directory group)

  (save-excursion
    (set-buffer nntp-server-buffer)
    (erase-buffer)
    (let* ((number (length articles))
	   (count 0)
	   (large (and (numberp nnmail-large-newsgroup)
		       (> number nnmail-large-newsgroup)))
	   article file)

      (if (stringp (car articles))
	  'headers

	(while (setq article (pop articles))
	  (setq file (nneething-file-name article))

	  (when (and (file-exists-p file)
		     (or (file-directory-p file)
			 (not (zerop (nth 7 (file-attributes file))))))
	    (insert (format "221 %d Article retrieved.\n" article))
	    (nneething-insert-head file)
	    (insert ".\n"))

	  (incf count)

	  (and large
	       (zerop (% count 20))
	       (message "nneething: Receiving headers... %d%%"
			(/ (* count 100) number))))

	(when large
	  (message "nneething: Receiving headers...done"))

	(nnheader-fold-continuation-lines)
	'headers))))

(defun nneething-open-server (server &optional defs)
  (nnheader-report 'nneething "")
  (nnheader-init-server-buffer))

(defun nneething-close-server (&optional server)
  (setq nneething-current-directory nil)
  t)

(defun nneething-server-opened (&optional server)
  nneething-current-directory)

(defun nneething-status-message (&optional server)
  nneething-status-string)

(defun nneething-request-article (id &optional group server buffer)
  (nneething-possibly-change-directory group)
  (let ((file (unless (stringp id) (nneething-file-name id)))
	(nntp-server-buffer (or buffer nntp-server-buffer)))
    (and (stringp file)			; We did not request by Message-ID.
	 (file-exists-p file)		; The file exists.
	 (not (file-directory-p file))	; It's not a dir.
	 (save-excursion
	   (nnmail-find-file file)	; Insert the file in the nntp buf.
	   (or (nnheader-article-p)	; Either it's a real article...
	       (progn
		 (goto-char (point-min))
		 (nneething-make-head file) ; ... or we fake some headers.
		 (insert "\n")))
	   t))))

(defun nneething-request-group (group &optional dir dont-check)
  (nneething-possibly-change-directory group dir)
  (unless dont-check
    (nneething-create-mapping)
    (if (> (car nneething-active) (cdr nneething-active))
	(nnheader-insert "211 0 1 0 %s\n" group)
      (nnheader-insert
       "211 %d %d %d %s\n" 
       (- (1+ (cdr nneething-active)) (car nneething-active))
       (car nneething-active) (cdr nneething-active)
       group)))
  t)

(defun nneething-request-list (&optional server dir)
  (nnheader-report 'nneething "LIST is not implemented."))

(defun nneething-request-newgroups (date &optional server)
  (nnheader-report 'nneething "NEWSGROUPS is not implemented."))

(defun nneething-request-type (group &optional article)
  'unknown)

(defun nneething-close-group (group &optional server)
  (setq nneething-current-directory nil)
  t)


;;; Internal functions.

(defun nneething-possibly-change-directory (group &optional dir)
  (when group
    (if (and nneething-group
	     (string= group nneething-group))
	t
      (let (entry)
	(if (setq entry (assoc group nneething-group-alist))
	    (progn
	      (setq nneething-group group)
	      (setq nneething-directory (nth 1 entry))
	      (setq nneething-map (nth 2 entry))
	      (setq nneething-active (nth 3 entry)))
	  (setq nneething-group group)
	  (setq nneething-directory dir)
	  (setq nneething-map nil)
	  (setq nneething-active (cons 1 0))
	  (nneething-create-mapping)
	  (push (list group dir nneething-map nneething-active)
		nneething-group-alist))))))

(defun nneething-map-file ()
  ;; We make sure that the .nneething directory exists. 
  (unless (file-exists-p nneething-map-file-directory)
    (make-directory nneething-map-file-directory 'parents))
  ;; We store it in a special directory under the user's home dir.
  (concat (file-name-as-directory nneething-map-file-directory)
	  nneething-group nneething-map-file))

(defun nneething-create-mapping ()
  ;; Read nneething-active and nneething-map.
  (let ((map-file (nneething-map-file))
	(files (directory-files nneething-directory))
	touched map-files)
    (if (file-exists-p map-file)
	(condition-case nil
	    (load map-file nil t t)
	  (error nil)))
    (or nneething-active (setq nneething-active (cons 1 0)))
    ;; Old nneething had a different map format.
    (when (and (cdar nneething-map)
	       (atom (cdar nneething-map)))
      (setq nneething-map
	    (mapcar (lambda (n)
		      (list (cdr n) (car n) 
			    (nth 5 (file-attributes 
				    (nneething-file-name (car n))))))
		    nneething-map)))
    ;; Remove files matching the exclusion regexp.
    (when nneething-exclude-files
      (let ((f files)
	    prev)
	(while f
	  (if (string-match nneething-exclude-files (car f))
	      (if prev (setcdr prev (cdr f))
		(setq files (cdr files)))
	    (setq prev f))
	  (setq f (cdr f)))))
    ;; Remove deleted files from the map.
    (let ((map nneething-map)
	  prev)
      (while map
	(if (and (member (cadar map) files)
		 ;; We also remove files that have changed mod times.
		 (equal (nth 5 (file-attributes
				(nneething-file-name (cadar map))))
			(caddar map)))
	    (progn
	      (push (cadar map) map-files)
	      (setq prev map))
	  (setq touched t)
	  (if prev
	      (setcdr prev (cdr map))
	    (setq nneething-map (cdr nneething-map))))
	(setq map (cdr map))))
    ;; Find all new files and enter them into the map.
    (while files
      (unless (member (car files) map-files) 
	;; This file is not in the map, so we enter it.
	(setq touched t)
	(setcdr nneething-active (1+ (cdr nneething-active)))
	(push (list (cdr nneething-active) (car files) 
		    (nth 5 (file-attributes
			    (nneething-file-name (car files)))))
	      nneething-map))
      (setq files (cdr files)))
    (when (and touched 
	       (not nneething-read-only))
      (save-excursion
	(nnheader-set-temp-buffer " *nneething map*")
	(insert "(setq nneething-map '" (prin1-to-string nneething-map) ")\n"
		"(setq nneething-active '" (prin1-to-string nneething-active)
		")\n")
	(write-region (point-min) (point-max) map-file nil 'nomesg)
	(kill-buffer (current-buffer))))))

(defun nneething-insert-head (file)
  "Insert the head of FILE."
  (when (nneething-get-head file)
    (insert-buffer-substring nneething-work-buffer)))

(defun nneething-make-head (file)
  "Create a head by looking at the file attributes of FILE."
  (let ((atts (file-attributes file)))
    (insert 
     "Subject: " (file-name-nondirectory file) "\n"
     "Message-ID: <nneething-" 
     (int-to-string (incf nneething-message-id-number))
     "@" (system-name) ">\n"
     "Date: " (current-time-string (nth 5 atts)) "\n"
     (nneething-from-line (nth 2 atts))
     "Chars: " (int-to-string (nth 7 atts)) "\n")))

(defun nneething-from-line (uid)
  "Return a From header based of UID."
  (let ((login (condition-case nil 
		   (user-login-name uid)
		 (error 
		  (cond ((= uid (user-uid)) (user-login-name))
			((zerop uid) "root")
			(t (int-to-string uid))))))
	(name (condition-case nil 
		  (user-full-name uid)
		(error 
		 (cond ((= uid (user-uid)) (user-full-name))
		       ((zerop uid) "Ms. Root"))))))
    (concat "From: " login "@" (system-name) 
	    (if name (concat " (" name ")") "") "\n")))

(defun nneething-get-head (file)
  "Either find the head in FILE or make a head for FILE."
  (save-excursion
    (set-buffer (get-buffer-create nneething-work-buffer))
    (setq case-fold-search nil)
    (buffer-disable-undo (current-buffer))
    (erase-buffer)
    (cond 
     ((not (file-exists-p file))
      ;; The file do not exist. 
      nil)
     ((or (file-directory-p file)
	  (file-symlink-p file))
      ;; It's a dir, so we fudge a head.
      (nneething-make-head file) t)
     (t 
      ;; We examine the file.
      (nnheader-insert-head file)
      (if (nnheader-article-p)
	  (delete-region 
	   (progn
	     (goto-char (point-min))
	     (or (and (search-forward "\n\n" nil t)
		      (1- (point)))
		 (point-max)))
	   (point-max))
	(erase-buffer)
	(nneething-make-head file))
      t))))

(defun nneething-file-name (article)
  "Return the file name of ARTICLE."
  (concat (file-name-as-directory nneething-directory)
	  (if (numberp article)
	      (cadr (assq article nneething-map))
	    article)))

(provide 'nneething)

;;; nneething.el ends here
