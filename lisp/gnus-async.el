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

;;; Internal variables.

(defvar gnus-async-article-alist nil)

(defvar gnus-async-prefetch-article-buffer " *Async Prefetch Article*")

;;; Utility functions.

(defun gnus-group-asynchronous-p (group)
  "Say whether GROUP is fetched from a server that supports asynchronocity."
  (gnus-asynchronous-p (gnus-find-method-for-group group)))

;;; Article prefetch

(gnus-add-shutdown 'gnus-async-close 'gnus)
(defun gnus-async-close ()
  (gnus-kill-buffer gnus-async-prefetch-article-buffer)
  (setq gnus-async-article-alist nil))

(defun gnus-async-set-prefetch-buffer ()
  (if (get-buffer gnus-async-prefetch-article-buffer)
      (set-buffer gnus-async-prefetch-article-buffer)
    (set-buffer (get-buffer-create gnus-async-prefetch-article-buffer))
    (buffer-disable-undo (current-buffer))
    (gnus-add-current-to-buffer-list)))

(defun gnus-async-prefetch-next (group article summary)
  "Possibly prefetch several articles starting with the article after ARTICLE."
  (let ((next (caadr (gnus-data-find-list article))))
    (when next
      (gnus-async-prefetch-article group next summary))))

(defun gnus-async-prefetch-article (group article summary &optional number)
  "Possibly prefetch several articles starting with ARTICLE."
  (unless number
    (setq number gnus-use-article-prefetch))
  (when (and number
	     (or (not (numberp number))
		 (> number 0))
	     (gnus-group-asynchronous-p group)
	     (gnus-buffer-live-p summary))
    (when (numberp number)
      (decf number))
    (while (and article (gnus-async-prefetched-article-entry group article))
      (setq article (caadr (gnus-data-find-list article)))
      (when (numberp number)
	(decf number)))
    (when article
      ;; We want to fetch some more articles.
      (save-excursion
	(set-buffer summary)
	(let ((next (caadr (gnus-data-find-list article)))
	      mark)
	  (gnus-async-set-prefetch-buffer)
	  (goto-char (point-max))
	  (setq mark (point-marker))
	  (let ((nnheader-callback-function
		 `(lambda (arg)
		    (save-excursion
		      (gnus-async-set-prefetch-buffer)
		      (push (list ',(intern (format "%s-%d" group article))
				  ,mark (set-marker (make-marker) (point-max))
				  ,group ,article)
			    gnus-async-article-alist)
		      (when (gnus-buffer-live-p ,summary)
			,(when next
			   `(gnus-async-prefetch-article 
			     ,group ,next ,summary ,number))))))
		(nntp-server-buffer (get-buffer
				     gnus-async-prefetch-article-buffer)))
	    (gnus-message 7 "Prefetching article %d in group %s"
			  article group)
	    (gnus-request-article article group)))))))

(defun gnus-async-request-fetched-article (group article buffer)
  "See whether we have ARTICLE from GROUP and put it in BUFFER."
  (let ((entry (gnus-async-prefetched-article-entry group article)))
    (when entry
      (save-excursion
	(gnus-async-set-prefetch-buffer)
	(copy-to-buffer buffer (cadr entry) (caddr entry))
	;; Remove the read article from the prefetch buffer.
	(when (memq 'read gnus-prefetched-article-deletion-strategy)
	  (gnus-asynch-delete-prefected-entry entry))
	;; Decode the article.  Perhaps this shouldn't be done
	;; here?
	(set-buffer buffer)
	(nntp-decode-text)
	(goto-char (point-min))
	(gnus-delete-line)
	t))))

(defun gnus-asynch-delete-prefected-entry (entry)
  "Delete ENTRY from buffer and alist."
  (delete-region (cadr entry) (caddr entry))
  (set-marker (cadr entry) nil)
  (set-marker (caddr entry) nil)
  (setq gnus-async-article-alist 
	(delq entry gnus-async-article-alist)))

(defun gnus-async-prefetch-remove-group (group)
  "Remove all articles belonging to GROUP from the prefetch buffer."
  (when (and (gnus-group-asynchronous-p group)
	     (memq 'exit gnus-prefetched-article-deletion-strategy))
    (let ((alist gnus-async-article-alist))
      (save-excursion
	(gnus-async-set-prefetch-buffer)
	(while alist
	  (when (equal group (nth 3 (car alist)))
	    (gnus-asynch-delete-prefected-entry (car alist)))
	  (pop alist))))))
	  
(defun gnus-async-prefetched-article-entry (group article)
  "Return the entry for ARTICLE in GROUP iff it has been prefetched."
  (assq (intern (format "%s-%d" group article))
	gnus-async-article-alist))

(provide 'gnus-async)

;;; gnus-async.el ends here
