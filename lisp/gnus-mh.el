;;; gnus-mh: mh-e interface for Gnus
;; Copyright (C) 1994,95 Free Software Foundation, Inc.

;; Author: Masanobu UMEDA <umerin@flab.flab.fujitsu.junet>
;;	Lars Magne Ingebrigtsen <larsi@ifi.uio.no>
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

;;; Send mail using mh-e.

;; The following mh-e interface is all cooperative works of
;; tanaka@flab.fujitsu.CO.JP (TANAKA Hiroshi), kawabe@sra.CO.JP
;; (Yoshikatsu Kawabe), and shingu@casund.cpr.canon.co.jp (Toshiaki
;; SHINGU).

;;; Code:

(require 'mh-e)
(require 'mh-comp)
(require 'gnus)

(defun gnus-summary-save-in-folder (&optional folder)
  "Save this article to MH folder (using `rcvstore' in MH library).
Optional argument FOLDER specifies folder name."
  ;; Thanks to yuki@flab.Fujitsu.JUNET and ohm@kaba.junet.
  (mh-find-path)
  (let ((folder
	 (or folder
	     (mh-prompt-for-folder 
	      "Save article in"
	      (funcall gnus-folder-save-name gnus-newsgroup-name
		       gnus-current-headers gnus-newsgroup-last-folder)
	      t)))
	(errbuf (get-buffer-create " *Gnus rcvstore*")))
    (gnus-eval-in-buffer-window 
     gnus-article-buffer
     (save-restriction
       (widen)
       (unwind-protect
	   (call-process-region (point-min) (point-max)
				(expand-file-name "rcvstore" mh-lib)
				nil errbuf nil folder)
	 (set-buffer errbuf)
	 (if (zerop (buffer-size))
	     (message "Article saved in folder: %s" folder)
	   (message "%s" (buffer-string)))
	 (kill-buffer errbuf))))
    (setq gnus-newsgroup-last-folder folder)))

(defun gnus-mail-reply-using-mhe (&optional yank)
  "Compose reply mail using mh-e.
Optional argument YANK means yank original article.
The command \\[mh-yank-cur-msg] yank the original message into current buffer."
  ;; First of all, prepare mhe mail buffer.
  ;; Bug fix by Timo METZEMAKERS <metzemakers@labri.u-bordeaux.fr>.
  (pop-to-buffer gnus-article-buffer)
  (let (from cc subject date to reply-to (buffer (current-buffer)))
    (save-restriction
      (gnus-article-show-all-headers)	;I don't think this is really needed.
      (setq from (gnus-fetch-field "from")
	    subject (let ((subject (or (gnus-fetch-field "subject")
				       "(None)")))
		      (if (and subject
			       (not (string-match "^[Rr][Ee]:.+$" subject)))
			  (concat "Re: " subject) subject))
	    reply-to (gnus-fetch-field "reply-to")
	    cc (gnus-fetch-field "cc")
	    date (gnus-fetch-field "date"))
      (setq mh-show-buffer buffer)
      (setq to (or reply-to from))
      (mh-find-path)
      (mh-send to (or cc "") subject)
      (save-excursion
	(mh-insert-fields
	 "In-reply-to:"
	 (concat
	  (substring from 0 (string-match "  *at \\|  *@ \\| *(\\| *<" from))
	  "'s message of " date)))
      (setq mh-sent-from-folder buffer)
      (setq mh-sent-from-msg 1)
      ))
  ;; Then, yank original article if requested.
  (if yank
      (let ((last (point)))
	(mh-yank-cur-msg)
	(goto-char last)
	)))

;; gnus-mail-forward-using-mhe is contributed by Jun-ichiro Itoh
;; <itojun@ingram.mt.cs.keio.ac.jp>

(defun gnus-mail-forward-using-mhe ()
  "Forward the current message to another user using mh-e."
  ;; First of all, prepare mhe mail buffer.
  (let ((to (read-string "To: "))
 	(cc (read-string "Cc: "))
 	(buffer (current-buffer))
 	subject beg)
    ;;(gnus-article-show-all-headers)
    (setq subject
	  (concat "[" gnus-newsgroup-name "] "
		  ;;(mail-strip-quoted-names (gnus-fetch-field "From")) ": "
		  (or (gnus-fetch-field "subject") "")))
    (setq mh-show-buffer buffer)
    (mh-find-path)
    (mh-send to (or cc "") subject)
    (save-excursion
      (goto-char (point-max))
      (setq beg (point))
      (insert "\n------- Forwarded Message\n\n")
      (insert-buffer buffer)
      (goto-char (point-max))
      (insert "\n------- End of Forwarded Message\n")
      (goto-char beg)
      (while (setq beg (next-single-property-change (point) 'invisible))
	(goto-char beg)
	(delete-region beg (or (next-single-property-change 
				(point) 'invisible)
			       (point-max))))
      (setq mh-sent-from-folder buffer)
      (setq mh-sent-from-msg 1))))

(defun gnus-mail-other-window-using-mhe ()
  "Compose mail other window using mh-e."
  (let ((to (read-string "To: "))
	(cc (read-string "Cc: "))
	(subject (read-string "Subject: ")))
    (gnus-article-show-all-headers)	;I don't think this is really needed.
    (setq mh-show-buffer (current-buffer))
    (mh-find-path)
    (mh-send-other-window to cc subject)
    (setq mh-sent-from-folder (current-buffer))
    (setq mh-sent-from-msg 1)))

(defun gnus-Folder-save-name (newsgroup headers &optional last-folder)
  "Generate folder name from NEWSGROUP, HEADERS, and optional LAST-FOLDER.
If variable `gnus-use-long-file-name' is nil, it is +News.group.
Otherwise, it is like +news/group."
  (or last-folder
      (concat "+"
	      (if gnus-use-long-file-name
		  (gnus-capitalize-newsgroup newsgroup)
		(gnus-newsgroup-directory-form newsgroup)))))

(defun gnus-folder-save-name (newsgroup headers &optional last-folder)
  "Generate folder name from NEWSGROUP, HEADERS, and optional LAST-FOLDER.
If variable `gnus-use-long-file-name' is nil, it is +news.group.
Otherwise, it is like +news/group."
  (or last-folder
      (concat "+"
	      (if gnus-use-long-file-name
		  newsgroup
		(gnus-newsgroup-directory-form newsgroup)))))

;;; gnus-mh.el ends here
