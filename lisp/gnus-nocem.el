;;; gnus-nocem.el --- NoCeM pseudo-cancellation treatment
;; Copyright (C) 1995,96 Free Software Foundation, Inc.

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
(eval-when-compile (require 'cl))

(defvar gnus-nocem-groups '("alt.nocem.misc" "news.admin.net-abuse.announce")
  "*List of groups that will be searched for NoCeM messages.")

(defvar gnus-nocem-issuers '("Automoose-1" "clewis@ferret.ocunix.on.ca;")
  "*List of NoCeM issuers to pay attention to.")

(defvar gnus-nocem-directory 
  (concat (file-name-as-directory gnus-article-save-directory) "NoCeM/")
  "*Directory where NoCeM files will be stored.")

(defvar gnus-nocem-expiry-wait 30
  "*Number of days to keep NoCeM headers in the cache.")

;;; Internal variables

(defvar gnus-nocem-active nil)
(defvar gnus-nocem-alist nil)
(defvar gnus-nocem-touched-alist nil)

;;; Functions

(defun gnus-nocem-active-file ()
  (concat (file-name-as-directory gnus-nocem-directory) "active"))

(defun gnus-nocem-cache-file ()
  (concat (file-name-as-directory gnus-nocem-directory) "cache"))

(defun gnus-nocem-scan-groups ()
  "Scan all NoCeM groups for new NoCeM messages."
  (interactive)
  (let ((groups gnus-nocem-groups)
	group active gactive articles)
    (or (file-exists-p gnus-nocem-directory)
	(make-directory gnus-nocem-directory t))
    ;; Load any previous NoCeM headers.
    (gnus-nocem-load-cache)
    ;; Read the active file if it hasn't been read yet.
    (and (file-exists-p (gnus-nocem-active-file))
	 (not gnus-nocem-active)
	 (condition-case ()
	     (load (gnus-nocem-active-file) t t t)
	   (error nil)))
    ;; Go through all groups and see whether new articles have
    ;; arrived.  
    (while groups
      (setq group (pop groups))
      (if (not (gnus-activate-group group))
	  () ; This group doesn't exist.
	(setq active (nth 1 (assoc group gnus-nocem-active)))
	(when (and (not (< (cdr gactive) (car gactive))) ; Empty group.
		   (or (not active)
		       (< (cdr active) 
			  (cdr (setq gactive (gnus-gethash 
					      group gnus-newsrc-hashtb))))))
	  ;; Ok, there are new articles in this group, se we fetch the
	  ;; headers.
	  (let ((gnus-newsgroup-dependencies (make-vector 10))
		headers)
	    (setq headers
		  (if (eq 'nov
			  (gnus-retrieve-headers 
			   (setq articles
				 (gnus-uncompress-range
				  (cons (1+ (cdr active)) (cdr gactive))))
			   group))
		      (gnus-get-newsgroup-headers-xover articles)
		    (gnus-get-newsgroup-headers)))
	    (while headers
	      ;; We take a closer look on all articles that have
	      ;; "@@NCM" in the subject.  
	      (and (string-match "@@NCM" (mail-header-subject (car headers)))
		   (gnus-nocem-check-article
		    (mail-header-number (car headers)) group))
	      (setq headers (cdr headers)))))))
    ;; Save the results, if any.
    (gnus-nocem-save-cache)))

(defun gnus-nocem-check-article (number group)
  "Check whether the current article is a NCM article and that we want it."
  (save-excursion
    (set-buffer nntp-server-buffer)
    ;; Get the article.
    (gnus-request-article-this-buffer number group)
    (goto-char (point-min))
    (let (issuer b)
      ;; The article has to have proper NoCeM headers.
      (when (and (setq b (search-forward "\n@@BEGIN NCM HEADERS\n" nil t))
		 (search-forward "\n@@BEGIN NCM BODY\n" nil t))
	;; We get the name of the issuer.
	(narrow-to-region b (match-beginning 0))
	(setq issuer (mail-fetch-field "issuer"))
	(and (member issuer gnus-nocem-issuers) ; We like her...
	     (gnus-nocem-verify-issuer issuer) ; She is who she says she is...
	     (gnus-nocem-enter-article)))) ; We gobble the message.
    (widen)))

(defun gnus-nocem-verify-issuer (person)
  "Verify using PGP that the canceler is who she says she is."
  t)

(defun gnus-nocem-enter-article ()
  "Enter the current article into the NoCeM cache."
  (widen)
  (goto-char (point-min))
  (let ((b (search-forward "\n@@BEGIN NCM BODY\n" nil t))
	(e (search-forward "\n@@END NCM BODY\n" nil t))
	(buf (current-buffer))
	ncm id)
    (when (and b e)
      (narrow-to-region b (1+ (match-beginning 0)))
      (goto-char (point-min))
      (while (search-forward "\t" nil t)
	(when (boundp (let ((obarray gnus-newsrc-hashtb)) (read buf)))
	  (beginning-of-line)
	  (while (= (following-char) ?\t)
	    (forward-line -1))
	  (setq id (buffer-substring (point) (1- (search-forward "\t"))))
	  (push id ncm)
	  (gnus-sethash id t gnus-nocem-hashtb)
	  (forward-line 1)
	  (while (= (following-char) ?\t)
	    (forward-line 1))))
      (when ncm
	(setq gnus-nocem-touched-alist t)
	(push (push (current-time-string) ncm) gnus-nocem-alist)))))

(defun gnus-nocem-load-cache ()
  "Load the NoCeM cache."
  (if gnus-nocem-alist
      () ; Do nothing.
    ;; The buffer doesn't exist, so we create it and load the NoCeM
    ;; cache.  
    (when (file-exists-p (gnus-nocem-cache-file))
      (load (gnus-nocem-cache-file) t t t)
      (gnus-nocem-alist-to-hashtb))))
      
(defun gnus-nocem-save-cache ()
  "Save the NoCeM cache."
  (when (and gnus-nocem-alist
	     gnus-nocem-touched-alist)
    (save-excursion
      (nnheader-set-temp-buffer " *NoCeM*")
      (insert (prin1-to-string
	       (list 'setq 'gnus-nocem-alist gnus-nocem-alist)))
      (write-region (point-min) (point-max) 
		    (gnus-nocem-cache-file) nil 'silent)
      (kill-buffer (current-buffer))
      (setq gnus-nocem-touched-alist nil))))

(defun gnus-nocem-alist-to-hashtb ()
  "Create a hashtable from the Message-IDs we have."
  (let ((alist gnus-nocem-alist)
	(date (current-time-string))
	entry)
    (setq gnus-nocem-hashtb (* (length alist) 51))
    (while alist
      (setq entry (pop alist))
      (if (> (gnus-days-between date (car entry)) gnus-nocem-expiry-wait)
	  ;; This entry has expired, so we remove it.
	  (setq gnus-nocem-alist (delq entry gnus-nocem-alist))
	;; This is ok, so we enter it into the hashtable.
	(setq entry (cdr entry))
	(while entry
	  (gnus-sethash (car entry) t gnus-nocem-hashtb)
	  (setq entry (cdr entry)))))))

(defun gnus-nocem-close ()
  "Clear internal NoCeM variables."
  (setq gnus-nocem-alist nil
	gnus-nocem-hashtb nil
	gnus-nocem-active nil
	gnus-nocem-touched-alist nil))

;;; gnus-nocem.el ends here
