;;; message-utils.el -- utils for message-mode

;; Copyright (C) 2002 Free Software Foundation, Inc.

;; Author: Holger Schauer <Holger.Schauer@gmx.de>
;; Keywords: utils message

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This file contains some small additions to message mode:
;;    * inserting files in a message and explicit marking it
;;      as something somebody else has created,
;;    * change Subject: header and add (was: <old subject>)
;;    * strip (was: <old subject>) from Subject: headers
;;    * add a X-No-Archieve: Yes header and a note in the body
;;    * a function for cross-post and followup-to messages
;;    * replace To: header with contents of Cc: or Bcc: header.
;;

;; This file is adopt from the link below when the revision is 0.8.
;;  http://www.coling.uni-freiburg.de/~schauer/resources/emacs/message-utils.el.gz

;;; Installation: (TODO: merge into message.el)

;; .. is easy as in most cases. Add
;; (autoload 'message-mark-inserted-region "message-utils" nil t)
;; (autoload 'message-mark-insert-file "message-utils" nil t)
;; (autoload 'message-strip-subject-was "message-utils" nil t)
;; (autoload 'message-change-subject "message-utils" nil t)
;; (autoload 'message-xpost-fup2 "message-utils" nil t)
;; (autoload 'message-add-archive-header "message-utils" nil t)
;; (autoload 'message-reduce-to-to-cc "message-utils" nil t)
;; as well as some keybindings like
;; (define-key message-mode-map '[(control c) m] 'message-mark-inserted-region)
;; (define-key message-mode-map '[(control c) f] 'message-mark-insert-file)
;; (define-key message-mode-map '[(control c) x] 'message-xpost-fup2)
;; (define-key message-mode-map '[(control c) s] 'message-change-subject)
;; (define-key message-mode-map '[(control c) a] 'message-add-archive-header)
;; (define-key message-mode-map '[(control c) t] 'message-reduce-to-to-cc)
;; (add-hook 'message-header-setup-hook 'message-strip-subject-was)
;; to your .gnus or to your .emacs.
;; You might also want to add something along the following lines:
;; (defun message-utils-setup ()
;;  "Add menu-entries for message-utils."
;;  (easy-menu-add-item nil '("Message")
;;   ["Insert Region Marked" message-mark-inserted-region t] "Spellcheck")
;;  (easy-menu-add-item nil '("Message")
;;   ["Insert File Marked" message-mark-insert-file t] "Spellcheck")
;;  (easy-menu-add-item nil '("Field")
;;   ["Crosspost / Followup" message-xpost-fup2 t] "----")
;;  (easy-menu-add-item nil '("Field")
;;   ["New Subject" message-change-subject t] "----")
;;  (easy-menu-add-item nil '("Field")
;;   ["Reduce To: to Cc:" message-reduce-to-to-cc t] "----")
;;  (easy-menu-add-item nil '("Field")
;;   [ "X-No-Archive:" message-add-archive-header t ]))
;; (add-hook 'message-mode-hook 'message-utils-setup)

;;; Code:

(require 'message)

;;; **************
;;; Inserting and marking ...

; We try to hook the vars into the message customize group

(defcustom message-begin-inserted-text-mark
"--8<------------------------schnipp------------------------->8---\n"
"How to mark the beginning of some inserted text."
 :type 'string
 :group 'message-various)

(defcustom message-end-inserted-text-mark
"--8<------------------------schnapp------------------------->8---\n"
"How to mark the end of some inserted text."
 :type 'string
 :group 'message-various)

;;;###autoload
(defun message-mark-inserted-region (beg end)
  "Mark some region in the current article with enclosing tags.
See `message-begin-inserted-text-mark' and `message-end-inserted-text-mark'."
  (interactive "r")
  (save-excursion
    ; add to the end of the region first, otherwise end would be invalid
    (goto-char end)
    (insert message-end-inserted-text-mark)
    (goto-char beg)
    (insert message-begin-inserted-text-mark)))

;;;###autoload
(defun message-mark-insert-file (file)
  "Inserts FILE at point, marking it with enclosing tags.
See `message-begin-inserted-text-mark' and `message-end-inserted-text-mark'."
  (interactive "fFile to insert: ")
    ;; reverse insertion to get correct result.
  (let ((p (point)))
    (insert message-end-inserted-text-mark)
    (goto-char p)
    (insert-file-contents file)
    (goto-char p)
    (insert message-begin-inserted-text-mark)))

;;; **************
;;; Subject mangling

(defcustom message-subject-was-regexp
  "[ \t]*\\((*[Ww][Aa][SsRr]:[ \t]*.*)\\)"
  "*Regexp matching \"(was: <old subject>)\" in the subject line."
  :group 'message-various
  :type 'regexp)

;;;###autoload
(defun message-strip-subject-was ()
  "Remove trailing \"(Was: <old subject>)\" from subject lines."
  (message-narrow-to-head)
  (let* ((subject (message-fetch-field "Subject"))
	 (pos))
    (cond (subject
	   (setq pos (or (string-match message-subject-was-regexp subject) 0))
	   (cond ((> pos 0)
		  (message-goto-subject)
		  (message-delete-line)
		  (insert (concat "Subject: "
				  (substring subject 0 pos) "\n")))))))
    (widen))

;;; Suggested by Jonas Steverud  @  www.dtek.chalmers.se/~d4jonas/
;;;###autoload
(defun message-change-subject (new-subject)
  "Ask for new Subject: header, append (was: <Old Subject>)."
  (interactive
   (list
    (read-from-minibuffer "New subject: ")))
  (cond ((and (not (or (null new-subject) ; new subject not empty
		       (zerop (string-width new-subject))
		       (string-match "^[ \t]*$" new-subject))))
	 (save-excursion
	   (let ((old-subject (message-fetch-field "Subject")))
	     (cond ((not (string-match
			  (concat "^[ \t]*"
				  (regexp-quote new-subject)
				  " \t]*$")
			  old-subject))  ; yes, it really is a new subject
		    ;; delete eventual Re: prefix
		    (setq old-subject
			  (message-strip-subject-re old-subject))
		    (message-goto-subject)
		    (message-delete-line)
		    (insert (concat "Subject: "
				    new-subject
				    " (was: "
				    old-subject ")\n")))))))))


;;; **************
;;; X-Archive-Header: No

(defcustom message-archive-header
  "X-No-Archive: Yes\n"
  "Header to insert when you don't want your article to be archived by deja.com."
  :type 'string
  :group 'message-various)

(defcustom message-archive-note
  "X-No-Archive: Yes - save http://deja.com/"
  "Note to insert why you wouldn't want this posting archived."
  :type 'string
  :group 'message-various)

(defun message-add-archive-header ()
  "Insert \"X-No-Archive: Yes\" in the header and a note in the body.
When called with a prefix argument, ask for a text to insert."
  (interactive)
  (if current-prefix-arg
      (setq message-archive-note
	    (read-from-minibuffer "Reason for No-Archive: "
				  (cons message-archive-note 0))))
  (save-excursion
    (insert message-archive-note)
    (newline)
    (message-add-header message-archive-header)
    (message-sort-headers)))

;;; **************
;;; Crossposts and Followups

; inspired by JoH-followup-to by Jochem Huhman <joh  at gmx.de>
; new suggestions by R. Weikusat <rw at another.de>

(defvar message-xpost-old-target nil
  "Old target for cross-posts or follow-ups.")
(make-variable-buffer-local 'message-xpost-old-target)

(defcustom message-xpost-default t
  "When non-nil `mesage-xpost-fup2' will normally perform a crosspost.
If nil, `message-xpost-fup2' will only do a followup. Note that you
can explicitly override this setting by calling `message-xpost-fup2'
with a prefix."
  :type 'boolean
  :group 'message-various)

(defun message-xpost-fup2-header (target-group)
  "Mangles FollowUp-To and Newsgroups header to point to TARGET-GROUP.
With prefix-argument just set Follow-Up, don't cross-post."
  (interactive
   (list ; Completion based on Gnus
    (completing-read "Follwup To: "
		     (if (boundp 'gnus-newsrc-alist)
			 gnus-newsrc-alist)
		     nil nil '("poster" . 0)
		     (if (boundp 'gnus-group-history)
			 'gnus-group-history))))
  (message-remove-header "Follow[Uu]p-[Tt]o" t)
  (message-goto-newsgroups)
  (beginning-of-line)
  ;; if we already did a crosspost before, kill old target
  (if (and message-xpost-old-target
	   (re-search-forward
	    (regexp-quote (concat "," message-xpost-old-target))
	    nil t))
      (replace-match ""))
  ;; unless (followup is to poster or user explicitly asked not
  ;; to cross-post, or target-group is already in Newsgroups)
  ;; add target-group to Newsgroups line.
  (cond ((and (or (and message-xpost-default (not current-prefix-arg))  ; def: xpost, req:no
		  (and (not message-xpost-default) current-prefix-arg)) ; def: no-xpost, req:yes
	      (not (string-match "poster" target-group))
	      (not (string-match (regexp-quote target-group)
				 (message-fetch-field "Newsgroups"))))
	 (end-of-line)
	 (insert-string (concat "," target-group))))
  (end-of-line) ; ensure Followup: comes after Newsgroups:
  ;; unless new followup would be identical to Newsgroups line
  ;; make a new Followup-To line
  (if (not (string-match (concat "^[ \t]*"
				 target-group
				 "[ \t]*$")
			 (message-fetch-field "Newsgroups")))
      (insert (concat "\nFollowup-To: " target-group)))
  (setq message-xpost-old-target target-group))


(defcustom message-xpost-note
  "Crosspost & Followup-To: "
  "Note to insert before signature to notify of xpost and follow-up."
 :type 'string
 :group 'message-various)

(defcustom message-fup2-note
  "Followup-To: "
  "Note to insert before signature to notify of follow-up only."
 :type 'string
 :group 'message-various)

(defun message-xpost-insert-note (target-group xpost in-old old-groups)
  "Insert a in message body note about a set Followup or Crosspost.
If there have been previous notes, delete them. TARGET-GROUP specifies the
group to Followup-To. When XPOST is t, insert note about
crossposting. IN-OLD specifies whether TARGET-GROUP is a member of
OLD-GROUPS. OLD-GROUPS lists the old-groups the posting would have
been made to before the user asked for a Crosspost."
  ;; start scanning body for previous uses
  (message-goto-signature)
  (let ((head (re-search-backward
	       (concat "^" mail-header-separator)
	       nil t))) ; just search in body
    (message-goto-signature)
    (while (re-search-backward
	    (concat "^" (regexp-quote message-xpost-note) ".*")
	    head t)
      (message-delete-line))
    (message-goto-signature)
    (while (re-search-backward
	    (concat "^" (regexp-quote message-fup2-note) ".*")
	    head t)
      (message-delete-line))
  ;; insert new note
  (message-goto-signature)
  (previous-line 2)
  (open-line 1)
  (if (or in-old
	  (not xpost)
	  (string-match "^[ \t]*poster[ \t]*$" target-group))
      (insert (concat message-fup2-note target-group "\n"))
    (insert (concat message-xpost-note target-group "\n")))))

(defcustom message-xpost-note-function
  'message-xpost-insert-note
  "Function to use to insert note about Crosspost or Followup-To.
The function will be called with four arguments. The function should not
only insert a note, but also ensure old notes are deleted. See the
documentation for `message-xpost-insert-note'. "
  :type 'function
  :group 'message-various)

;;;###autoload
(defun message-xpost-fup2 (target-group)
  "Crossposts message and sets Followup-To to TARGET-GROUP.
With prefix-argument just set Follow-Up, don't cross-post."
  (interactive
   (list ; Completion based on Gnus
    (completing-read "Follwup To: "
		     (if (boundp 'gnus-newsrc-alist)
			 gnus-newsrc-alist)
		     nil nil '("poster" . 0)
		     (if (boundp 'gnus-group-history)
			 'gnus-group-history))))
  (cond ((not (or (null target-group) ; new subject not empty
		  (zerop (string-width target-group))
		  (string-match "^[ \t]*$" target-group)))
	 (save-excursion
	   (let* ((old-groups (message-fetch-field "Newsgroups"))
		  (in-old (string-match
			   (regexp-quote target-group) old-groups)))
	     ;; check whether target exactly matches old Newsgroups
	     (cond ((or (not in-old)
			(not (string-match
			      (concat "^[ \t]*"
				      (regexp-quote target-group)
				      "[ \t]*$")
			      old-groups)))
		    ;; yes, Newsgroups line must change
		    (message-xpost-fup2-header target-group)
		    ;; insert note whether we do xpost or fup2
		    (funcall message-xpost-note-function
			     target-group
			     (if (or (and message-xpost-default (not current-prefix-arg))
				     (and (not message-xpost-default) current-prefix-arg))
				 t)
			     in-old old-groups))))))))


;;; **************
;;; Reduce To: to Cc: or Bcc: header

(defun message-reduce-to-to-cc ()
 "Replace contents of To: header with contents of Cc: or Bcc: header."
 (interactive)
 (let ((cc-content (message-fetch-field "cc"))
       (bcc nil))
   (if (and (not cc-content)
	    (setq cc-content (message-fetch-field "bcc")))
       (setq bcc t))
   (cond (cc-content
	  (save-excursion
	    (message-goto-to)
	    (message-delete-line)
	    (insert (concat "To: " cc-content "\n"))
	    (message-remove-header (if bcc
				       "bcc"
				     "cc")))))))

;;; provide ourself
(provide 'message-utils)

;;; message-utils.el ends here
