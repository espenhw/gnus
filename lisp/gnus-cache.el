;;; gnus-cache.el --- cache interface for Gnus
;; Copyright (C) 1995 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@ifi.uio.no>
;; Keywords: news

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

;;; Code:

(require 'gnus)

(defvar gnus-cache-directory (concat gnus-article-save-directory "cache/")
  "*The directory where cached articles will be stored.")

(defvar gnus-cache-enter-articles '(ticked dormant)
  "*Classes of articles to enter into the cache.")

(defvar gnus-cache-remove-articles '(read)
  "*Classes of articles to remove from the cache.")



(defvar gnus-cache-buffer nil)



(defun gnus-cache-change-buffer (group)
  (save-excursion
    (cond ((null gnus-cache-buffer)
	   ;; No current cache, so we create and init the buffer.
	   (setq gnus-cache-buffer
		 (cons group (get-buffer-create " *gnus-cache-overview*")))
	   (set-buffer (cdr gnus-cache-buffer))
	   (buffer-disable-undo (current-buffer))
	   (erase-buffer)
	   (gnus-add-current-to-buffer-list)
	   (let ((file (gnus-cache-file-name group ".overview")))
	     (and (file-exists-p file)
		  (insert-file-contents file))))
	  ((not (string= group (car gnus-cache-buffer)))
	   ;; If a different overview cache is the current, we
	   ;; (possibly) save it and change to this groups.
	   (set-buffer (cdr gnus-cache-buffer))
	   (and (buffer-modified-p)
		(write-region (point-min) (point-max)
			      (gnus-cache-file-name
			       (car gnus-cache-buffer) ".overview")
			      nil 'quiet))
	   (erase-buffer)
	   (setcar gnus-cache-buffer group)
	   (let ((file (gnus-cache-file-name group ".overview")))
	     (and (file-exists-p file)
		  (insert-file-contents file)))))))


;; Just save the overview buffer.
(defun gnus-cache-save-buffers ()
  (and gnus-cache-buffer
       (save-excursion
	 (set-buffer (cdr gnus-cache-buffer))
	 (and (buffer-modified-p)
	      (write-region (point-min) (point-max)
			    (gnus-cache-file-name (car gnus-cache-buffer)
						  ".overview")
			    nil 'quiet))))
  (setq gnus-cache-buffer nil))

;; Return whether an article is a member of a class.
(defun gnus-cache-member-of-class (class ticked dormant unread)
  (or (and ticked (memq 'ticked class))
      (and dormant (memq 'dormant class))
      (and unread (memq 'unread class))
      (and (not unread) (memq 'read class))))

(defun gnus-cache-file-name (group article)
  (concat (file-name-as-directory gnus-cache-directory)
	  (if (gnus-use-long-file-name 'not-cache)
	      group (gnus-replace-chars-in-string group ?. ?/))
	  "/" (if (stringp article) article (int-to-string article))))

(defun gnus-cache-possibly-enter-article 
  (group article headers ticked dormant unread)
  (let ((number (header-number headers))
	file dir)
    (if (or (not (gnus-cache-member-of-class
		  gnus-cache-enter-articles ticked dormant unread))
	    (file-exists-p (setq file (gnus-cache-file-name group article))))
	()
      (gnus-summary-select-article)
      (or (file-exists-p (setq dir (file-name-directory file)))
	  (gnus-make-directory dir))
      (if (file-exists-p file)
	  t
	(save-excursion
	  (set-buffer gnus-article-buffer)
	  (write-region (point-min) (point-max) file nil 'quiet)
	  (gnus-cache-change-buffer group)
	  (set-buffer (cdr gnus-cache-buffer))
	  (goto-char (point-max))
	  (forward-line -1)
	  (while (and (not (bobp))
		      (> (read (current-buffer)) number))
	    (forward-line -1))
	  (if (bobp) 
	      (if (not (eobp))
		  (progn
		    (beginning-of-line)
		    (if (< (read (current-buffer)) number)
			(forward-line 1)))
		(beginning-of-line))
	    (forward-line 1))
	  ;; [number subject from date id references chars lines xref]
	  (insert (format "%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t\n"
			  (header-number headers)
			  (header-subject headers)
			  (header-from headers)
			  (header-date headers)
			  (header-id headers)
			  (or (header-references headers) "")
			  (or (header-chars headers) "")
			  (or (header-lines headers) "")
			  (or (header-xref headers) ""))))
	t))))

(defun gnus-cache-possibly-remove-article 
  (group article ticked dormant unread)
  (let ((file (gnus-cache-file-name group article)))
    (if (or (not (file-exists-p file))
	    (not (gnus-cache-member-of-class
		  gnus-cache-remove-articles ticked dormant unread)))
	nil
      (save-excursion
	(delete-file file)
	(gnus-cache-change-buffer group)
	(set-buffer (cdr gnus-cache-buffer))
	(goto-char (point-min))
	(if (or (looking-at (concat (int-to-string article) "\t"))
		(search-forward (concat "\n" (int-to-string article) "\t")))
	    (delete-region (progn (beginning-of-line) (point))
			   (progn (forward-line 1) (point))))))))

(defun gnus-cache-request-article (article group)
  (let ((file (gnus-cache-file-name group article)))
    (if (not (file-exists-p file))
	()
      (erase-buffer)
      (insert-file-contents file)
      t)))

(defun gnus-cache-articles-in-group (group)
  (let ((dir (file-name-directory (gnus-cache-file-name group 1)))
	articles)
    (if (not (file-exists-p dir))
	nil
      (setq articles (directory-files dir nil "^[0-9]+$" t))
      (if (not articles)
	  nil
	(sort (mapcar (function (lambda (name)
				  (string-to-int name))) 
		      articles)
	      '<)))))

(defun gnus-cache-active-articles (group)
  (let ((articles (gnus-cache-articles-in-group group)))
    (and articles
	 (cons (car articles) (gnus-last-element articles)))))

(defun gnus-cache-possibly-alter-active (group active)
  (let ((cache-active (gnus-cache-active-articles group)))
    (and cache-active (< (car cache-active) (car active))
	 (setcar active (car cache-active)))
    (and cache-active (> (cdr cache-active) (cdr active))
	 (setcdr active (cdr cache-active)))))

(defun gnus-cache-retrieve-headers (articles group)
  (let* ((cached (gnus-cache-articles-in-group group))
	 (articles (gnus-sorted-complement articles cached))
	 (cache-file (gnus-cache-file-name group ".overview"))
	 type)
    (let ((gnus-use-cache nil))
      (setq type (and articles (gnus-retrieve-headers articles group))))
    (gnus-cache-save-buffers)
    (save-excursion
      (cond ((not (file-exists-p cache-file))
	     type)
	    ((null type)
	     (set-buffer nntp-server-buffer)
	     (erase-buffer)
	     (insert-file-contents cache-file)
	     'nov)
	    ((eq type 'nov)
	     (gnus-cache-braid-nov group cached)
	     type)
	    (t
	     (gnus-cache-braid-heads group cached)
	     type)))))

(defun gnus-cache-braid-nov (group cached)
  (let ((cache-buf (get-buffer-create " *gnus-cache*"))
	beg end)
    (gnus-cache-save-buffers)
    (save-excursion
      (set-buffer cache-buf)
      (buffer-disable-undo (current-buffer))
      (erase-buffer)
      (insert-file-contents (gnus-cache-file-name group ".overview"))
      (goto-char (point-min))
      (insert "\n")
      (goto-char (point-min)))
    (set-buffer nntp-server-buffer)
    (goto-char (point-min))
    (while cached
      (while (and (not (eobp))
		  (< (read (current-buffer)) (car cached)))
	(forward-line 1))
      (beginning-of-line)
      (save-excursion
	(set-buffer cache-buf)
	(if (search-forward (concat "\n" (int-to-string (car cached)) "\t")
			    nil t)
	    (setq beg (progn (beginning-of-line) (point))
		  end (progn (forward-line 1) (point)))
	  (setq beg nil)))
      (and beg (insert-buffer-substring cache-buf beg end))
      (setq cached (cdr cached)))
    (kill-buffer cache-buf)))

(defun gnus-cache-braid-heads (group cached)
  (let ((cache-buf (get-buffer-create " *gnus-cache*"))
	beg end)
    (save-excursion
      (set-buffer cache-buf)
      (buffer-disable-undo (current-buffer))
      (erase-buffer))
    (set-buffer nntp-server-buffer)
    (goto-char (point-min))
    (while cached
      (while (and (not (eobp))
		  (looking-at "2.. +\\([0-9]+\\) ")
		  (< (progn (goto-char (match-beginning 1))
			    (read (current-buffer)))
		     (car cached)))
	(search-forward "\n.\n" nil 'move))
      (beginning-of-line)
      (save-excursion
	(set-buffer cache-buf)
	(erase-buffer)
	(insert-file-contents (gnus-cache-file-name group (car cached)))
	(goto-char (point-min))
	(insert "220 " (int-to-string (car cached)) " Article retrieved.\n")
	(search-forward "\n\n" nil 'move)
	(delete-region (point) (point-max))
	(forward-char -1)
	(insert "."))
      (insert-buffer-substring cache-buf)
      (setq cached (cdr cached)))
    (kill-buffer cache-buf)))

(defun gnus-jog-cache ()
  "Go through all groups and put the articles into the cache."
  (interactive)
  (let ((newsrc (cdr gnus-newsrc-alist))
	(gnus-cache-enter-articles '(unread))
	(gnus-mark-article-hook nil)
	(gnus-expert-user t)
	(gnus-large-newsgroup nil))
    (while newsrc
      (gnus-summary-read-group (car (car newsrc)))
      (if (not (eq major-mode 'gnus-summary-mode))
	  ()
	(while gnus-newsgroup-unreads
	  (gnus-summary-select-article t t nil (car gnus-newsgroup-unreads))
	  (setq gnus-newsgroup-unreads (cdr gnus-newsgroup-unreads)))
	(kill-buffer (current-buffer)))
      (setq newsrc (cdr newsrc)))))

(provide 'gnus-cache)
	      
;;; gnus-cache.el ends here
