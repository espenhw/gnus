;;; gnus-async.el --- asynchronous support for Gnus
;; Copyright (C) 1996 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@ifi.uio.no>
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

(require 'gnus-load)
(require 'gnus-sum)
(require 'nntp)

(defvar gnus-asynchronous t
  "*If nil, inhibit all Gnus asynchronicity.
If non-nil, let the other asynch variables be heeded.")

(defvar gnus-use-article-prefetch 30
  "*If non-nil, prefetch articles in groups that allow this.
If a number, prefetch only that many articles forward;
if t, prefetch as many articles as possible.")

(defvar gnus-prefetched-article-deletion-strategy '(read exit)
  "List of symbols that say when to remove articles from the prefetch buffer.
Possible values in this list are `read', which means that 
articles are removed as they are read, and `exit', which means
that all articles belonging to a group are removed on exit
from that group.")

(defvar gnus-use-header-prefetch nil
  "*If non-nil, prefetch the headers to the next group.")

;;; Internal variables.

(defvar gnus-async-prefetch-article-buffer " *Async Prefetch Article*")
(defvar gnus-async-article-alist nil)
(defvar gnus-async-article-semaphore '(nil))
(defvar gnus-async-fetch-list nil)

(defvar gnus-async-prefetch-headers-buffer " *Async Prefetch Headers*")
(defvar gnus-async-header-prefetched nil)

;;; Utility functions.

(defun gnus-group-asynchronous-p (group)
  "Say whether GROUP is fetched from a server that supports asynchronocity."
  (gnus-asynchronous-p (gnus-find-method-for-group group)))

;;; Somewhat bogus semaphores.

(defun gnus-async-get-semaphore (semaphore)
  "Wait until SEMAPHORE is released."
  (while (/= (length (nconc (symbol-value semaphore) (list nil))) 2)
    (sleep-for 1)))

(defun gnus-async-release-semaphore (semaphore)
  "Release SEMAPHORE."
  (setcdr (symbol-value semaphore) nil))

;;;
;;; Article prefetch
;;;

(gnus-add-shutdown 'gnus-async-close 'gnus)
(defun gnus-async-close ()
  (gnus-kill-buffer gnus-async-prefetch-article-buffer)
  (gnus-kill-buffer gnus-async-prefetch-headers-buffer)
  (setq gnus-async-article-alist nil
	gnus-async-header-prefetched nil))

(defun gnus-async-set-buffer ()
  (nnheader-set-temp-buffer gnus-async-prefetch-article-buffer t))

(defun gnus-async-prefetch-next (group article summary)
  "Possibly prefetch several articles starting with the article after ARTICLE."
  (let ((next (caadr (gnus-data-find-list article))))
    (when next
      (gnus-async-prefetch-article group next summary))))

(defun gnus-async-prefetch-article (group article summary &optional next)
  "Possibly prefetch several articles starting with ARTICLE."
  (when (and gnus-asynchronous
	     (gnus-alive-p))
    (when next
      (gnus-async-get-semaphore 'gnus-async-article-semaphore)
      (pop gnus-async-fetch-list)
      (gnus-async-release-semaphore 'gnus-async-article-semaphore))
    (let ((do-fetch next))
      (when (and (gnus-group-asynchronous-p group)
		 (gnus-buffer-live-p summary)
		 (or (not next)
		     gnus-async-fetch-list))
	(unwind-protect
	    (progn
	      (gnus-async-get-semaphore 'gnus-async-article-semaphore)
	      (unless next
		(setq do-fetch (not gnus-async-fetch-list))
		;; Nix out any outstanding requests.
		(setq gnus-async-fetch-list nil)
		;; Fill in the new list.
		(let ((n gnus-use-article-prefetch)
		      (data (gnus-data-find-list article))
		      d)
		  (while (and (setq d (pop data))
			      (if (numberp n) 
				  (natnump (decf n))
				n))
		    (unless (or (gnus-async-prefetched-article-entry
				 group (setq article (gnus-data-number d)))
				(not (natnump article)))
		      ;; Not already fetched -- so we add it to the list.
		      (push article gnus-async-fetch-list)))
		  (setq gnus-async-fetch-list
			(nreverse gnus-async-fetch-list))))

	      (when do-fetch
		(setq article (car gnus-async-fetch-list))))
	
	  (gnus-async-release-semaphore 'gnus-async-article-semaphore))
    
	(when (and do-fetch article)
	  ;; We want to fetch some more articles.
	  (save-excursion
	    (set-buffer summary)
	    (let (mark)
	      (gnus-async-set-buffer)
	      (goto-char (point-max))
	      (setq mark (point-marker))
	      (let ((nnheader-callback-function
		     (gnus-make-async-article-function 
		      group article mark summary next))
		    (nntp-server-buffer (get-buffer
					 gnus-async-prefetch-article-buffer)))
		(gnus-message 7 "Prefetching article %d in group %s"
			      article group)
		(gnus-request-article article group)))))))))

(defun gnus-make-async-article-function (group article mark summary next)
  "Return a callback function."
  `(lambda (arg)
     (save-excursion
       (gnus-async-set-buffer)
       (gnus-async-get-semaphore 'gnus-async-article-semaphore)
       (push (list ',(intern (format "%s-%d" group article))
		   ,mark (set-marker (make-marker)
				     (point-max))
		   ,group ,article)
	     gnus-async-article-alist)
       (gnus-async-release-semaphore
	'gnus-async-article-semaphore)
       (when (gnus-buffer-live-p ,summary)
	 (gnus-async-prefetch-article 
	  ,group ,next ,summary t)))))

(defun gnus-async-request-fetched-article (group article buffer)
  "See whether we have ARTICLE from GROUP and put it in BUFFER."
  (when (numberp article)
    (let ((entry (gnus-async-prefetched-article-entry group article)))
      (when entry
	(save-excursion
	  (gnus-async-set-buffer)
	  (copy-to-buffer buffer (cadr entry) (caddr entry))
	  ;; Remove the read article from the prefetch buffer.
	  (when (memq 'read gnus-prefetched-article-deletion-strategy)
	    (gnus-async-delete-prefected-entry entry))
	  t)))))

(defun gnus-async-delete-prefected-entry (entry)
  "Delete ENTRY from buffer and alist."
  (delete-region (cadr entry) (caddr entry))
  (set-marker (cadr entry) nil)
  (set-marker (caddr entry) nil)
  (gnus-async-get-semaphore 'gnus-async-article-semaphore)
  (setq gnus-async-article-alist 
	(delq entry gnus-async-article-alist))
  (gnus-async-release-semaphore 'gnus-async-article-semaphore))

(defun gnus-async-prefetch-remove-group (group)
  "Remove all articles belonging to GROUP from the prefetch buffer."
  (when (and (gnus-group-asynchronous-p group)
	     (memq 'exit gnus-prefetched-article-deletion-strategy))
    (let ((alist gnus-async-article-alist))
      (save-excursion
	(gnus-async-set-buffer)
	(while alist
	  (when (equal group (nth 3 (car alist)))
	    (gnus-async-delete-prefected-entry (car alist)))
	  (pop alist))))))
	  
(defun gnus-async-prefetched-article-entry (group article)
  "Return the entry for ARTICLE in GROUP iff it has been prefetched."
  (assq (intern (format "%s-%d" group article))
	gnus-async-article-alist))

;;;
;;; Header prefetch
;;;

(defun gnus-async-prefetch-headers (group)
  "Prefetch the headers for group GROUP."
  (save-excursion
    (let (unread)
      (when (and gnus-use-header-prefetch
		 gnus-asynchronous
		 (gnus-group-asynchronous-p group)
		 (listp gnus-async-header-prefetched)
		 (setq unread (gnus-list-of-unread-articles group)))
	;; Mark that a fetch is in progress.
	(setq gnus-async-header-prefetched t)
	(nnheader-set-temp-buffer gnus-async-prefetch-headers-buffer t)
	(erase-buffer)
	(let ((nntp-server-buffer (current-buffer))
	      (nnheader-callback-function
	       `(lambda (arg)
		  (setq gnus-async-header-prefetched
			,(cons group unread)))))
	  (gnus-retrieve-headers unread group gnus-fetch-old-headers))))))

(defun gnus-async-retrieve-fetched-headers (articles group)
  "See whether we have prefetched headers."
  (when (and gnus-use-header-prefetch
	     (gnus-group-asynchronous-p group)
	     (listp gnus-async-header-prefetched)
	     (equal group (car gnus-async-header-prefetched))
	     (equal articles (cdr gnus-async-header-prefetched)))
    (save-excursion
      (nnheader-set-temp-buffer gnus-async-prefetch-headers-buffer t)
      (nntp-decode-text)
      (copy-to-buffer nntp-server-buffer (point-min) (point-max))
      (erase-buffer)
      (setq gnus-async-header-prefetched nil)
      t)))
  
(provide 'gnus-async)

;;; gnus-async.el ends here
