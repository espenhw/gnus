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

(defvar gnus-registry-headers-hashtb nil
  "*The article header registry by Message ID.")

(defcustom gnus-registry-unfollowed-groups '("delayed" "drafts" "queue")
  "List of groups that gnus-registry-split-fancy-with-parent won't follow.
The group names are matched, they don't have to be fully qualified."
  :group 'gnus-registry
  :type '(repeat string))

;; Function(s) missing in Emacs 20
(when (memq nil (mapcar 'fboundp '(puthash)))
  (require 'cl)
  (unless (fboundp 'puthash)
    ;; alias puthash is missing from Emacs 20 cl-extra.el
    (defalias 'puthash 'cl-puthash)))

(defun gnus-registry-translate-to-alist ()
  (setq gnus-registry-alist (hashtable-to-alist gnus-registry-hashtb))
  (setq gnus-registry-headers-alist (hashtable-to-alist 
				     gnus-registry-headers-hashtb)))

(defun gnus-registry-translate-from-alist ()
  (setq gnus-registry-hashtb (alist-to-hashtable gnus-registry-alist))
  (setq gnus-registry-headers-hashtb (alist-to-hashtable 
				      gnus-registry-headers-alist)))

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
	(to-name (if to to "the Bit Bucket")))
    (gnus-message 5 "Registry: article %s %s from %s to %s"
		  id
		  (if method "respooling" "going")
		  from
		  to)   
    (unless (gethash id gnus-registry-headers-hashtb)
      (puthash id (list data-header) gnus-registry-headers-hashtb))
    (puthash id (cons (list action from to)
		      (gethash id gnus-registry-hashtb)) 
	     gnus-registry-hashtb)))

(defun gnus-register-spool-action (id group)
  ;; do not process the draft IDs
;  (unless (string-match "totally-fudged-out-message-id" id)
    (let ((group (gnus-group-guess-full-name group)))
    (when (string-match "$" id)
      (setq id (substring id 0 -1)))
    (gnus-message 5 "Registry: article %s spooled to %s"
		  id
		  group)
    (puthash id (cons (list 'spool nil group) 
		      (gethash id gnus-registry-hashtb)) 
	     gnus-registry-hashtb)))
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
	(references nil)
	(res nil))
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
      res)))

(defun gnus-registry-grep-in-list (word list)
  (memq nil
	(mapcar 'not
	 (mapcar 
	  (lambda (x)
	    (string-match x word))
	  list))))


(defun gnus-registry-fetch-group (id)
  "Get the group of a message, based on the message ID.
Returns the first place where the trail finds a spool action."
  (let ((trail (gethash id gnus-registry-hashtb)))
    (dolist (crumb trail)
      (let ((action (nth 0 crumb))
	    (from (nth 1 crumb))
	    (to (nth 2 crumb)))
	(when (eq action 'spool)
	  (return to))))))

(defun gnus-registry-clear ()
  "Clear the Gnus registry."
  (interactive)
  (setq gnus-registry-alist nil 
	gnus-registry-headers-alist nil)
  (gnus-registry-translate-from-alist))

; also does copy, respool, and crosspost
(add-hook 'gnus-summary-article-move-hook 'gnus-register-action) 
(add-hook 'gnus-summary-article-delete-hook 'gnus-register-action)
(add-hook 'gnus-summary-article-expire-hook 'gnus-register-action)
(add-hook 'nnmail-spool-hook 'gnus-register-spool-action)

(add-hook 'gnus-save-newsrc-hook 'gnus-registry-translate-to-alist)
(add-hook 'gnus-read-newsrc-el-hook 'gnus-registry-translate-from-alist)

;; TODO: a lot of things

(provide 'gnus-registry)

;;; gnus-registry.el ends here
