;;; gnus-soup.el --- SOUP packet writing support for Gnus
;; Copyright (C) 1995 Free Software Foundation, Inc.

;; Author: Per Abrahamsen <abraham@iesd.auc.dk>
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
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; This file contain support for storing articles in SOUP format from
;; within Gnus.  Support for reading SOUP packets is provided in
;; `nnsoup.el'. 

;; SOUP is a format for offline reading of news and mail.  See the
;; file `soup12.zip' in one of the Simtel mirrors
;; (e.g. `ftp.funet.fi') for the specification of SOUP.

;; Only a subset of the SOUP protocol is supported, and the minimal
;; conformance requirements in the SOUP document is *not* meet.  
;; Most annoyingly, replying and posting are not supported.

;; Insert 
;;   (require 'gnus-soup)
;; in your `.emacs' file to enable the SOUP support.

;; Type `V o s' to add articles to the SOUP packet.  
;; Use a prefix argument or the process mark to add multiple articles.

;; The variable `gnus-soup-directory' should point to the directory
;; where you want to store the SOUP component files.  You must
;; manually `zip' the directory to generate a conforming SOUP packet.

;; Add `nnsoup' to `gnus-secondary-select-methods' in order to read a
;; SOUP packet. The variable `nnmail-directory' should point to the
;; directory containing the unziped SOUP packet.

;; Check out `uqwk' or `yarn' for two alterative solutions to
;; generating or reading SOUP packages respectively, they should both
;; be available at a Simtel mirror near you.  There are plenty of
;; other SOUP-aware programs available as well, look in the group
;; `alt.usenet.offline-reader' and its FAQ for more information.

;; Since `gnus-soup.el' does not fulfill the minimal conformance
;; requirements, expect some problems when using other SOUP packeges.
;; More importantly, the author haven't tested any of them.

;;; Code:

;;; Hack `gnus.el':

(require 'gnus)

;;; User Variables:

(defvar gnus-soup-directory "~/SOUP/"
  "*Directory containing unpacked SOUP packet.")

(defvar gnus-soup-prefix-file "gnus-prefix"
  "*Name of the file where Gnus stores the last used prefix.")

(defvar gnus-soup-packer "tar cf - %s | gzip > $HOME/Soupout%d.tgz"
  "Format string command for packing a SOUP packet.
The SOUP files will be inserted where the %s is in the string.
This string MUST contain both %s and %d. The file number will be
inserted where %d appears.")

;;; Internal Variables:

(defvar gnus-soup-encoding-type ?n
  "*Soup encoding type.
`n' is news format, `m' is Unix mbox format, and `M' is MMDF mailbox
format.")

(defvar gnus-soup-index-type ?c
  "*Soup index type.
`n' means no index file and `c' means standard Cnews overview
format.") 

(defvar gnus-soup-group-type ?u
  "*Soup message area type.
`u' is unknown, `m' is private mail, and `n' is news.
Gnus will determine by itself what type to use in what group, so
setting this variable won't do much.")

(defconst gnus-soup-areas nil)
(defvar gnus-soup-last-prefix nil)
(defvar gnus-soup-buffers nil)

;;; Commands:

(defun gnus-soup-add-article (n)
  "Add the current article to SOUP packet.
If N is a positive number, add the N next articles.
If N is a negative number, add the N previous articles.
If N is nil and any articles have been marked with the process mark,
move those articles instead."
  (interactive "P")
  (gnus-set-global-variables)
  (add-hook 'gnus-exit-gnus-hook 'gnus-soup-save)
  (or (file-directory-p gnus-soup-directory)
      (gnus-make-directory gnus-soup-directory))
  (let* ((articles (gnus-summary-work-articles n))
	 (tmp-buf (get-buffer-create "*soup work*"))
	 (prefix (aref (gnus-soup-area gnus-newsgroup-name) 0))
	 (msg-buf (find-file-noselect
		   (concat gnus-soup-directory prefix ".MSG")))
	 (idx-buf (find-file-noselect
		   (concat gnus-soup-directory prefix ".IDX")))
	 from head-line beg type headers)
    (setq gnus-soup-buffers (cons msg-buf (cons idx-buf gnus-soup-buffers)))
    (buffer-disable-undo tmp-buf)
    (buffer-disable-undo msg-buf)
    (buffer-disable-undo idx-buf)
    (save-excursion
      (while articles
	;; Put the article in a buffer.
	(set-buffer tmp-buf)
	(gnus-request-article-this-buffer 
	 (car articles) gnus-newsgroup-name)
	;; Make sure the last char in the buffer is a newline.
	(goto-char (point-max))
	(or (= (current-column) 0)
	    (insert "\n"))
	;; Find the "from".
	(goto-char (point-min))
	(setq from
	      (mail-strip-quoted-names
	       (or (mail-fetch-field "from")
		   (mail-fetch-field "really-from")
		   (mail-fetch-field "sender"))))
	(goto-char (point-min))
	;; Depending on what encoding is supposed to be used, we make
	;; a soup header. 
	(setq head-line
	      (cond 
	       ((= gnus-soup-encoding-type ?n)
		(format "#! rnews %d\n" (buffer-size)))
	       ((= gnus-soup-encoding-type ?m)
		(while (search-forward "\nFrom " nil t)
		  (replace-match "\n>From " t t))
		(concat "From " (or from "unknown")
			" " (current-time-string) "\n"))
	       ((= gnus-soup-encoding-type ?M)
		"\^a\^a\^a\^a\n")
	       (t (error "Unsupported type: %c" gnus-soup-encoding-type))))
	;; Find the header of the article.
	(set-buffer gnus-summary-buffer)
	(setq headers (gnus-get-header-by-number (car articles)))
	;; Insert the soup header and the article in the MSG buf.
	(set-buffer msg-buf)
	(goto-char (point-max))
	(insert head-line)
	(setq beg (point))
	(insert-buffer tmp-buf)
	;; Insert the index in the IDX buf.
	(cond ((= gnus-soup-index-type ?c)
	       (set-buffer idx-buf)
	       (gnus-soup-insert-idx beg headers))
	      ((/= gnus-soup-index-type ?n)
	       (error "Unknown index type: %c" type)))
	(set-buffer gnus-summary-buffer)
	(gnus-summary-remove-process-mark (car articles))
	(gnus-summary-mark-as-read (car articles) "F")
	(setq articles (cdr articles)))
      (kill-buffer tmp-buf))))

(defun gnus-soup-group-brew (group)
  (let ((gnus-expert-user t)
	(gnus-large-newsgroup nil))
    (and (gnus-summary-read-group group)
	 (let ((gnus-newsgroup-processable 
		(gnus-sorted-complement 
		 gnus-newsgroup-unreads
		 (append gnus-newsgroup-dormant gnus-newsgroup-marked))))
	   (gnus-soup-add-article nil)))
    (gnus-summary-exit)))

(defun gnus-group-brew-soup (n)
  "Make a soup packet from the current group."
  (interactive "P")
  (let ((groups (gnus-group-process-prefix n)))
    (while groups
      (gnus-group-remove-mark (car groups))
      (gnus-soup-group-brew (car groups))
      (setq groups (cdr groups)))
    (gnus-soup-save)))

(defun gnus-brew-soup (&optional level)
  "Go through all groups on LEVEL or less and make a soup packet."
  (interactive "P")
  (let ((level (or level gnus-level-subscribed))
	(newsrc (cdr gnus-newsrc-alist)))
    (while newsrc
      (and (<= (nth 1 (car newsrc)) level)
	   (gnus-soup-group-brew (car (car newsrc))))
      (setq newsrc (cdr newsrc)))
    (gnus-soup-save)))
  
;;; Internal Functions:

(defun gnus-soup-insert-idx (offset header)
  ;; [number subject from date id references chars lines xref]
  (goto-char (point-max))
  (insert
   (format "%d\t%s\t%s\t%s\t%s\t%s\t%d\t%s\t%s\t\n"
	   offset
	   (or (header-subject header) "(none)")
	   (or (header-from header) "(nobody)")
	   (or (header-date header) "")
	   (or (header-id header)
	       (concat "soup-dummy-id-" 
		       (mapconcat 
			(lambda (time) (int-to-string time))
			(current-time) "-")))
	   (or (header-references header) "")
	   (or (header-chars header) 0) 
	   (or (header-lines header) "0") 
	   (or (header-xref header) ""))))

(defun gnus-soup-save ()
  (gnus-soup-write-areas)
  (save-excursion
    (let (buf)
      (while gnus-soup-buffers
	(setq buf (car gnus-soup-buffers)
	      gnus-soup-buffers (cdr gnus-soup-buffers))
	(if (not (buffer-name buf))
	    ()
	  (set-buffer buf)
	  (and (buffer-modified-p) (save-buffer))
	  (kill-buffer (current-buffer)))))
    (gnus-set-work-buffer)
    (insert (format "(setq gnus-soup-last-prefix %d)\n" 
		    gnus-soup-last-prefix))
    (write-region (point-min) (point-max) gnus-soup-prefix-file nil 'nomesg)))

(defun gnus-soup-pack ()
  (let* ((dir (file-name-nondirectory 
	       (directory-file-name
		(file-name-as-directory gnus-soup-directory))))
	 (top (file-name-directory
	       (directory-file-name
		(file-name-as-directory gnus-soup-directory))))
	 (files (mapconcat (lambda (f) (concat dir "/" f))
			   '("AREAS" "*.MSG" "*.IDX" "INFO"
			     "LIST" "REPLIES" "COMMANDS" "ERRORS")
			   " "))
	 (packer (if (< (string-match "%s" gnus-soup-packer)
			(string-match "%d" gnus-soup-packer))
		     (format gnus-soup-packer files
			     (string-to-int (gnus-soup-unique-prefix)))
		   (format gnus-soup-packer 
			   (string-to-int (gnus-soup-unique-prefix)) files))))
    (if (zerop (call-process "sh" nil nil nil "-c" 
			     (concat "cd " top " ; " packer)))
	(call-process "sh" nil nil nil "-c" 
		      (concat "cd " top " ; rm " files))
      (error "Couldn't pack packet."))))

(defun gnus-soup-parse-areas (file)
  "Parse soup area file FILE.
The result is a of vectors, each containing one entry from the AREA file.
The vector contain five strings, 
  [prefix name encoding description number]
though the two last may be nil if they are missing."
  (let (areas)
    (save-excursion
      (set-buffer (find-file-noselect file))
      (buffer-disable-undo)
      (goto-char (point-min))
      (while (not (eobp))
	(setq areas
	      (cons (vector (gnus-soup-field) 
			    (gnus-soup-field)
			    (gnus-soup-field)
			    (and (eq (preceding-char) ?\t) (gnus-soup-field))
			    (and (eq (preceding-char) ?\t) (gnus-soup-field)))
		    areas))
	(if (eq (preceding-char) ?\t)
	    (beginning-of-line 2))))
    areas))

(defun gnus-soup-field ()
  (prog1
      (buffer-substring (point) (progn (skip-chars-forward "^\t\n") (point)))
    (forward-char 1)))

(defun gnus-soup-read-areas ()
  (or gnus-soup-areas
      (setq gnus-soup-areas
	    (gnus-soup-parse-areas (concat gnus-soup-directory "AREAS")))))

(defun gnus-soup-write-areas ()
  (save-excursion
    (set-buffer (find-file-noselect (concat gnus-soup-directory "AREAS")))
    (erase-buffer)
    (let ((areas gnus-soup-areas)
	  area)
      (while areas
	(setq area (car areas)
	      areas (cdr areas))
	(insert (aref area 0) ?\t (aref area 1) ?\t (aref area 2) ?\n)))
    (write-region (point-min) (point-max)
		  (concat gnus-soup-directory "AREAS"))
    (set-buffer-modified-p nil)
    (kill-buffer (current-buffer))))

(defun gnus-soup-area (group)
  (gnus-soup-read-areas)
  (let ((areas gnus-soup-areas)
	area result)
    (while areas
      (setq area (car areas)
	    areas (cdr areas))
      (if (equal (aref area 1) group)
	  (setq result area)))
    (or result
	(setq result
	      (vector (gnus-soup-unique-prefix)
		      group 
		      (format "%c%c%c"
			      gnus-soup-encoding-type
			      gnus-soup-index-type
			      (if (gnus-member-of-valid 'mail group) ?m ?n)
			      nil nil))
	      gnus-soup-areas (cons result gnus-soup-areas)))
    result))

(defun gnus-soup-unique-prefix ()
  (if gnus-soup-last-prefix
      ()
    (if (file-exists-p gnus-soup-prefix-file)
	(condition-case nil
	    (load-file gnus-soup-prefix-file)
	  (error 0))
      (setq gnus-soup-last-prefix 0)))
  (int-to-string (setq gnus-soup-last-prefix (1+ gnus-soup-last-prefix))))

(provide 'gnus-soup)

;;; gnus-soup.el ends here



