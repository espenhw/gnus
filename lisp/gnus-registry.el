;;; gnus-registry.el --- article registry for Gnus
;; Copyright (C) 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003
;;        Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
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

(eval-when-compile (require 'cl))

(require 'gnus)
(require 'gnus-int)
(require 'gnus-sum)
(require 'nnmail)

(defgroup gnus-registry nil
  "The Gnus registry."
  :group 'gnus)

(defvar gnus-registry-hashtb nil
  "*The article registry by Message ID.")

(defcustom gnus-registry-unfollowed-groups '("delayed" "drafts" "queue")
  "List of groups that gnus-registry-split-fancy-with-parent won't follow.
The group names are matched, they don't have to be fully qualified."
  :group 'gnus-registry
  :type '(repeat string))

(defcustom gnus-registry-cache-file "~/.gnus.registry.eld"
  "File where the Gnus registry will be stored."
  :group 'gnus-registry
  :type 'file)

;; Function(s) missing in Emacs 20
(when (memq nil (mapcar 'fboundp '(puthash)))
  (require 'cl)
  (unless (fboundp 'puthash)
    ;; alias puthash is missing from Emacs 20 cl-extra.el
    (defalias 'puthash 'cl-puthash)))

(defun gnus-registry-cache-read ()
  "Read the registry cache file."
  (interactive)
  (let ((file gnus-registry-cache-file))
    (when (file-exists-p file)
      (gnus-message 5 "Reading %s..." file)
      (gnus-load file)
      (gnus-message 5 "Reading %s...done" file))))

(defun gnus-registry-cache-save ()
  "Save the registry cache file."
  (interactive)
  (let ((file gnus-registry-cache-file))
    (save-excursion
      ;; Save .newsrc.eld.
      (set-buffer (gnus-get-buffer-create " *Gnus-registry-cache*"))
      (make-local-variable 'version-control)
    (setq version-control gnus-backup-startup-file)
    (setq buffer-file-name file)
    (setq default-directory (file-name-directory buffer-file-name))
    (buffer-disable-undo)
    (erase-buffer)
    (gnus-message 5 "Saving %s..." file)
    (if gnus-save-startup-file-via-temp-buffer
	(let ((coding-system-for-write gnus-ding-file-coding-system)
	      (standard-output (current-buffer)))
	  (gnus-gnus-to-quick-newsrc-format t "gnus registry startup file" 'gnus-registry-alist)
	  (gnus-registry-cache-whitespace file)
	  (save-buffer))
      (let ((coding-system-for-write gnus-ding-file-coding-system)
	    (version-control gnus-backup-startup-file)
	    (startup-file file)
	    (working-dir (file-name-directory file))
	    working-file
	    (i -1))
	;; Generate the name of a non-existent file.
	(while (progn (setq working-file
			    (format
			     (if (and (eq system-type 'ms-dos)
				      (not (gnus-long-file-names)))
				 "%s#%d.tm#" ; MSDOS limits files to 8+3
			       (if (memq system-type '(vax-vms axp-vms))
				   "%s$tmp$%d"
				 "%s#tmp#%d"))
			     working-dir (setq i (1+ i))))
		      (file-exists-p working-file)))
	
	(unwind-protect
	    (progn
	      (gnus-with-output-to-file working-file
		(gnus-gnus-to-quick-newsrc-format t "gnus registry startup file" 'gnus-registry-alist))
	      
	      ;; These bindings will mislead the current buffer
	      ;; into thinking that it is visiting the startup
	      ;; file.
	      (let ((buffer-backed-up nil)
		    (buffer-file-name startup-file)
		    (file-precious-flag t)
		    (setmodes (file-modes startup-file)))
		;; Backup the current version of the startup file.
		(backup-buffer)
		
		;; Replace the existing startup file with the temp file.
		(rename-file working-file startup-file t)
		(set-file-modes startup-file setmodes)))
	  (condition-case nil
	      (delete-file working-file)
	    (file-error nil)))))
    
    (gnus-kill-buffer (current-buffer))
    (gnus-message 5 "Saving %s...done" file))))

;; Idea from Dan Christensen <jdc@chow.mat.jhu.edu>
;; Save the gnus-registry file with extra line breaks.
(defun gnus-registry-cache-whitespace (filename)
  (gnus-message 4 "Adding whitespace to %s" filename)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^(\\|(\\\"" nil t)
      (replace-match "\n\\&" t))
    (goto-char (point-min))
    (while (re-search-forward " $" nil t)
      (replace-match "" t t))))

(defun gnus-registry-save ()
  (setq gnus-registry-alist (hashtable-to-alist gnus-registry-hashtb))
  (gnus-registry-cache-save))

(defun gnus-registry-read ()
  (gnus-registry-cache-read)
  (setq gnus-registry-hashtb (alist-to-hashtable gnus-registry-alist)))

(defun alist-to-hashtable (alist)
  "Build a hashtable from the values in ALIST."
  (let ((ht (make-hash-table 			    
	     :size 4096
	     :test 'equal)))
    (mapc
     (lambda (kv-pair)
       (puthash (car kv-pair) (cdr kv-pair) ht))
     alist)
     ht))

(defun hashtable-to-alist (hash)
  "Build an alist from the values in HASH."
  (let ((list nil))
    (maphash
     (lambda (key value)
       (setq list (cons (cons key value) list)))
     hash)
    list))

(defun gnus-register-action (action data-header from &optional to method)
  (let* ((id (mail-header-id data-header))
	(from (gnus-group-guess-full-name from))
	(to (if to (gnus-group-guess-full-name to) nil))
	(to-name (if to to "the Bit Bucket"))
	(old-entry (gethash id gnus-registry-hashtb)))
    (gnus-message 5 "Registry: article %s %s from %s to %s"
		  id
		  (if method "respooling" "going")
		  from
		  to)

    ;; All except copy will need a delete
    (gnus-registry-delete-group id from)

    (when (equal 'copy action) 
      (gnus-registry-add-group id from)) ; undo the delete

    (gnus-registry-add-group id to)))

(defun gnus-register-spool-action (id group)
  ;; do not process the draft IDs
;  (unless (string-match "totally-fudged-out-message-id" id)
;    (let ((group (gnus-group-guess-full-name group)))
  (when (string-match "\r$" id)
    (setq id (substring id 0 -1)))
  (gnus-message 5 "Registry: article %s spooled to %s"
		id
		group)
  (gnus-registry-add-group id group))
;)

;; Function for nn{mail|imap}-split-fancy: look up all references in
;; the cache and if a match is found, return that group.
(defun gnus-registry-split-fancy-with-parent ()
  "Split this message into the same group as its parent.  The parent
is obtained from the registry.  This function can be used as an entry
in `nnmail-split-fancy' or `nnimap-split-fancy', for example like
this: (: gnus-registry-split-fancy-with-parent) 

For a message to be split, it looks for the parent message in the
References or In-Reply-To header and then looks in the registry to
see which group that message was put in.  This group is returned.

See the Info node `(gnus)Fancy Mail Splitting' for more details."
  (let ((refstr (or (message-fetch-field "references")
		    (message-fetch-field "in-reply-to")))
	(nnmail-split-fancy-with-parent-ignore-groups
	 (if (listp nnmail-split-fancy-with-parent-ignore-groups)
	     nnmail-split-fancy-with-parent-ignore-groups
	   (list nnmail-split-fancy-with-parent-ignore-groups)))
	references res)
    (when refstr
      (setq references (nreverse (gnus-split-references refstr)))
      (mapcar (lambda (x)
		(setq res (or (gnus-registry-fetch-group x) res))
		(when (or (gnus-registry-grep-in-list 
			   res
			   gnus-registry-unfollowed-groups)
			  (gnus-registry-grep-in-list 
			   res 
			   nnmail-split-fancy-with-parent-ignore-groups))
		  (setq res nil)))
	      references)
      (gnus-message 
       5 
       "gnus-registry-split-fancy-with-parent traced %s to group %s"
       refstr (if res res "nil"))
      res)))

(defun gnus-registry-register-message-ids ()
  "Register the Message-ID of every article in the group"
  (unless (gnus-parameter-registry-ignore gnus-newsgroup-name)
    (dolist (article gnus-newsgroup-articles)
      (let ((id (gnus-registry-fetch-message-id-fast article)))
	(unless (gnus-registry-fetch-group id)
	  (gnus-message 9 "Registry: Registering article %d with group %s" 
			article gnus-newsgroup-name)
	  (gnus-registry-add-group (gnus-registry-fetch-message-id-fast article)
				   gnus-newsgroup-name))))))

(defun gnus-registry-fetch-message-id-fast (article)
  "Fetch the Message-ID quickly, using the internal gnus-data-list function"
  (if (and (numberp article)
	   (assoc article (gnus-data-list nil)))
      (mail-header-id (gnus-data-header (assoc article (gnus-data-list nil))))
    nil))

(defun gnus-registry-grep-in-list (word list)
  (when word
    (memq nil
	  (mapcar 'not
		  (mapcar 
		   (lambda (x)
		     (string-match x word))
		   list)))))

(defun gnus-registry-fetch-extra (id)
  "Get the extra data of a message, based on the message ID.
Returns the first place where the trail finds a nonstring."
  (let ((trail (gethash id gnus-registry-hashtb)))
    (dolist (crumb trail)
      (unless (stringp crumb)
	(return crumb)))))

(defun gnus-registry-store-extra (id extra)
  "Store the extra data of a message, based on the message ID.
The message must have at least one group name."
  (when (gnus-registry-group-count id)
    ;; we now know the trail has at least 1 group name, so it's not empty
    (let ((trail (gethash id gnus-registry-hashtb))
	  (old-extra (gnus-registry-fetch-extra id)))
      (puthash id (cons extra (delete old-extra trail))
	       gnus-registry-hashtb))))

(defun gnus-registry-fetch-group (id)
  "Get the group of a message, based on the message ID.
Returns the first place where the trail finds a group name."
  (when (gnus-registry-group-count id)
    ;; we now know the trail has at least 1 group name
    (let ((trail (gethash id gnus-registry-hashtb)))
      (dolist (crumb trail)
	(when (stringp crumb)
	  (return crumb))))))

(defun gnus-registry-group-count (id)
  "Get the number of groups of a message, based on the message ID."
  (let ((trail (gethash id gnus-registry-hashtb)))
    (if (and trail (listp trail))
	(apply '+ (mapcar (lambda (x) (if (stringp x) 1 0)) trail))
      0)))

(defun gnus-registry-delete-group (id group)
  "Delete a group for a message, based on the message ID."
  (when group
    (when id
      (let ((trail (gethash id gnus-registry-hashtb))
	    (group (gnus-group-short-name group)))
	(puthash id (if trail
			(delete group trail)
		      nil)
		 gnus-registry-hashtb))
      ;; now, clear the entry if there are no more groups
      (unless (gnus-registry-group-count id)
	(remhash id gnus-registry-hashtb)))))

(defun gnus-registry-add-group (id group &rest extra)
  "Add a group for a message, based on the message ID."
  ;; make sure there are no duplicate entries
  (when group
    (when (and id
	       (not (string-match "totally-fudged-out-message-id" id)))
      (let ((group (gnus-group-short-name group)))
	(gnus-registry-delete-group id group)	
	(let ((trail (gethash id gnus-registry-hashtb)))
	  (puthash id (if trail
			  (cons group trail)
			(list group))
		   gnus-registry-hashtb)
	  (when extra (gnus-registry-store-extra id extra)))))))

(defun gnus-registry-clear ()
  "Clear the Gnus registry."
  (interactive)
  (setq gnus-registry-alist nil)
  (setq gnus-registry-hashtb (alist-to-hashtable gnus-registry-alist)))

; also does copy, respool, and crosspost
(add-hook 'gnus-summary-article-move-hook 'gnus-register-action) 
(add-hook 'gnus-summary-article-delete-hook 'gnus-register-action)
(add-hook 'gnus-summary-article-expire-hook 'gnus-register-action)
(add-hook 'nnmail-spool-hook 'gnus-register-spool-action)

(add-hook 'gnus-save-newsrc-hook 'gnus-registry-save)
(add-hook 'gnus-read-newsrc-el-hook 'gnus-registry-read)

(add-hook 'gnus-summary-prepare-hook 'gnus-registry-register-message-ids)

;; TODO: a lot of things

(provide 'gnus-registry)

;;; gnus-registry.el ends here
