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

(defvar gnus-registry-hashtb nil
  "*The article registry by Message ID.")

(defvar gnus-registry-headers-hashtb nil
  "*The article header registry by Message ID.")
;; (setq gnus-registry-hashtb (make-hash-table 
;; 			    :size 4096
;; 			    :test 'equal)) ; we test message ID strings equality

;; sample data-header
;; (defvar tzz-header '(49 "Re[2]: good news" "\"Jonathan Pryor\" <offerlm@aol.com>" "Mon, 17 Feb 2003 10:41:46 +-0800" "<88288020@dytqq>" "" 896 18 "lockgroove.bwh.harvard.edu spam.asian:49" nil))

;; (maphash (lambda (key value) (message "key: %s value: %s" key value)) gnus-registry-hashtb)
;; (clrhash gnus-registry-hashtb)
;; (setq gnus-registry-alist nil)

;; Function(s) missing in Emacs 20
(when (memq nil (mapcar 'fboundp '(puthash)))
  (require 'cl)
  (unless (fboundp 'puthash)
    ;; alias puthash is missing from Emacs 20 cl-extra.el
    (defalias 'puthash 'cl-puthash)))

(defun gnus-registry-translate-to-alist ()
  (setq gnus-registry-alist (hashtable-to-alist gnus-registry-hashtb))
  (setq gnus-registry-headers-alist (hashtable-to-alist gnus-registry-headers-hashtb)))

(defun gnus-registry-translate-from-alist ()
  (setq gnus-registry-hashtb (alist-to-hashtable gnus-registry-alist))
  (setq gnus-registry-headers-hashtb (alist-to-hashtable gnus-registry-headers-alist)))

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
  (let* ((id (mail-header-id data-header)))
    (gnus-message 5 "Registry: article %s %s from %s to %s"
	     id
	     (if method "respooling" "going")
	     (gnus-group-guess-full-name from)
	     (if to (gnus-group-guess-full-name to) "the Bit Bucket"))
    (unless (gethash id gnus-registry-headers-hashtb)
      (puthash id (list data-header) gnus-registry-headers-hashtb))
    (puthash id (cons (list action from to method)
		      (gethash id gnus-registry-hashtb)) gnus-registry-hashtb)))

(defun gnus-register-spool-action (id group)
  (when (string-match "$" id)
    (setq id (substring id 0 -1)))
  (gnus-message 5 "Registry: article %s spooled to %s"
	   id
	   (gnus-group-prefixed-name 
	    group 
	    gnus-internal-registry-spool-current-method 
	    t))
  (puthash id (cons (list 'spool nil group nil) 
		    (gethash id gnus-registry-hashtb)) gnus-registry-hashtb))

(add-hook 'gnus-summary-article-move-hook 'gnus-register-action) ; also does copy, respool, and crosspost
(add-hook 'gnus-summary-article-delete-hook 'gnus-register-action)
(add-hook 'gnus-summary-article-expire-hook 'gnus-register-action)
(add-hook 'nnmail-spool-hook 'gnus-register-spool-action)

(add-hook 'gnus-save-newsrc-hook 'gnus-registry-translate-to-alist)
(add-hook 'gnus-read-newsrc-el-hook 'gnus-registry-translate-from-alist)

;; TODO: a lot of things

(provide 'gnus-registry)

;;; gnus-registry.el ends here
