;;; nnmail.el --- mail support functions for the Gnus mail backends
;; Copyright (C) 1995 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@ifi.uio.no>
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

;;; Code:

(require 'nnheader)
(require 'rmail)
(require 'timezone)

(defvar nnmail-split-methods
  '(("mail.misc" ""))
  "Incoming mail will be split according to this variable.

If you'd like, for instance, one mail group for mail from the
\"4ad-l\" mailing list, one group for junk mail and one for everything
else, you could do something like this:

 (setq nnmail-split-methods
       '((\"mail.4ad\" \"From:.*4ad\")
         (\"mail.junk\" \"From:.*Lars\\\\|Subject:.*buy\")
         (\"mail.misc\" \"\")))

As you can see, this variable is a list of lists, where the first
element in each \"rule\" is the name of the group (which, by the way,
does not have to be called anything beginning with \"mail\",
\"yonka.zow\" is a fine, fine name), and the second is a regexp that
nnmail will try to match on the header to find a fit.

The second element can also be a function.  In that case, it will be
called narrowed to the headers with the first element of the rule as
the argument.  It should return a non-nil value if it thinks that the
mail belongs in that group.

The last element should always have \"\" as the regexp.")

;; Suggested by Erik Selberg <speed@cs.washington.edu>.
(defvar nnmail-crosspost t
  "If non-nil, do crossposting if several split methods match the mail.
If nil, the first match found will be used.")

;; Added by gord@enci.ucalgary.ca (Gordon Matzigkeit).
(defvar nnmail-keep-last-article nil
  "If non-nil, nnmail will never delete the last expired article in a
directory.  You may need to set this variable if other programs are putting
new mail into folder numbers that Gnus has marked as expired.")

(defvar nnmail-expiry-wait 7
  "Articles that are older than `nnmail-expiry-wait' days will be expired.")

(defvar nnmail-expiry-wait-function nil
  "Variable that holds funtion to specify how old articles should be before they are expired.
  The function will be called with the name of the group that the
expiry is to be performed in, and it should return an integer that
says how many days an article can be stored before it is considered
'old'. 

Eg.:

(setq nnmail-expiry-wait-function
      (lambda (newsgroup)
        (cond ((string-match \"private\" newsgroup) 31)
              ((string-match \"junk\" newsgroup) 1)
	      (t 7))))")

(defvar nnmail-spool-file 
  (or (getenv "MAIL")
      (concat "/usr/spool/mail/" (user-login-name)))
  "Where the mail backends will look for incoming mail.
This variable is \"/usr/spool/mail/$user\" by default.
If this variable is nil, no mail backends will read incoming mail.")

(defvar nnmail-read-incoming-hook nil
  "Hook that will be run after the incoming mail has been transferred.
The incoming mail is moved from `nnmail-spool-file' (which normally is
something like \"/usr/spool/mail/$user\") to the user's home
directory. This hook is called after the incoming mail box has been
emptied, and can be used to call any mail box programs you have
running (\"xwatch\", etc.)

Eg.

(add-hook 'nnmail-read-incoming-hook 
	   (lambda () 
	     (start-process \"mailsend\" nil 
			    \"/local/bin/mailsend\" \"read\" \"mbox\")))")

;; Suggested by Erik Selberg <speed@cs.washington.edu>.
(defvar nnmail-prepare-incoming-hook nil
  "Hook called before treating incoming mail.
The hook is run in a buffer with all the new, incoming mail.")

;; Suggested by Mejia Pablo J <pjm9806@usl.edu>.
(defvar nnmail-tmp-directory nil
  "If non-nil, use this directory for temporary storage when reading incoming mail.")

(defvar nnmail-large-newsgroup 50
  "The number of the articles which indicates a large newsgroup.
If the number of the articles is greater than the value, verbose
messages will be shown to indicate the current status.")



(defconst nnmail-version "nnml 0.0"
  "nnmail version.")



(defun nnmail-request-post-buffer (post group subject header article-buffer
					info follow-to respect-poster)
  (let ((method-address (cdr (assq 'to-address (nth 5 info))))
	from subject date to reply-to message-of
	references message-id sender cc sendto elt)
    (setq method-address
	  (if (and (stringp method-address) 
		   (string= method-address ""))
	      nil method-address))
    (save-excursion
      (set-buffer (get-buffer-create "*mail*"))
      (mail-mode)
      (local-set-key "\C-c\C-c" 'gnus-mail-send-and-exit)
      (if (and (buffer-modified-p)
	       (> (buffer-size) 0)
	       (not (y-or-n-p "Unsent mail being composed; erase it? ")))
	  ()
	(erase-buffer)
	(if post
	    (mail-setup method-address subject nil nil nil nil)
	  (save-excursion
	    (set-buffer article-buffer)
	    (goto-char (point-min))
	    (narrow-to-region (point-min)
			      (progn (search-forward "\n\n") (point)))
	    (let ((buffer-read-only nil))
	      (set-text-properties (point-min) (point-max) nil))
	    (setq from (header-from header))
	    (setq date (header-date header))
	    (and from
		 (let ((stop-pos 
			(string-match "  *at \\|  *@ \\| *(\\| *<" from)))
		   (setq message-of
			 (concat (if stop-pos (substring from 0 stop-pos) from)
				 "'s message of " date))))
	    (setq sender (mail-fetch-field "sender"))
	    (setq cc (mail-fetch-field "cc"))
	    (setq to (mail-fetch-field "to"))
	    (setq subject (header-subject header))
	    (or (string-match "^[Rr][Ee]:" subject)
		(setq subject (concat "Re: " subject)))
	    (setq reply-to (mail-fetch-field "reply-to"))
	    (setq references (header-references header))
	    (setq message-id (header-id header))
	    (widen))
	  (setq news-reply-yank-from from)
	  (setq news-reply-yank-message-id message-id)
	  
	  ;; Gather the "to" addresses out of the follow-to list and remove
	  ;; them as we go.
	  (if (and follow-to (listp follow-to))
	      (while (setq elt (assoc "To" follow-to))
		(setq sendto (concat sendto (and sendto ", ") (cdr elt)))
		(setq follow-to (delq elt follow-to))))
	  (mail-setup (if (and follow-to (listp follow-to)) sendto
			(or method-address 
			    (concat (or sender reply-to from "")
				    (if to (concat ", " to) "")
				    (if cc (concat ", " cc) ""))))
		      subject message-of nil article-buffer nil)
	  ;; Note that "To" elements should already be in the message.
	  (if (and follow-to (listp follow-to))
	      (progn
		(goto-char (point-min))
		(re-search-forward "^To:" nil t)
		(beginning-of-line)
		(forward-line 1)
		(while follow-to
		  (insert 
		   (car (car follow-to)) ": " (cdr (car follow-to)) "\n")
		  (setq follow-to (cdr follow-to)))))
	  ;; Fold long references line to follow RFC1036.
	  (mail-position-on-field "References")
	  (let ((begin (- (point) (length "References: ")))
		(fill-column 78)
		(fill-prefix "\t"))
	    (if references (insert references))
	    (if (and references message-id) (insert " "))
	    (if message-id (insert message-id))
	    ;; The region must end with a newline to fill the region
	    ;; without inserting extra newline.
	    (fill-region-as-paragraph begin (1+ (point))))))
      (current-buffer))))

(defun nnmail-find-file (file)
  "Insert FILE in server buffer safely."
  (set-buffer nntp-server-buffer)
  (erase-buffer)
  (condition-case ()
      (progn (insert-file-contents file) t)
    (file-error nil)))

(defun nnmail-article-pathname (group mail-dir)
  "Make pathname for GROUP."
  (concat (file-name-as-directory (expand-file-name mail-dir))
	  (nnmail-replace-chars-in-string group ?. ?/) "/"))

(defun nnmail-replace-chars-in-string (string from to)
  "Replace characters in STRING from FROM to TO."
  (let ((string (substring string 0))	;Copy string.
	(len (length string))
	(idx 0))
    ;; Replace all occurrences of FROM with TO.
    (while (< idx len)
      (if (= (aref string idx) from)
	  (aset string idx to))
      (setq idx (1+ idx)))
    string))

(defun nnmail-days-between (date1 date2)
  ;; Return the number of days between date1 and date2.
  (let ((d1 (mapcar (lambda (s) (and s (string-to-int s)) )
		    (timezone-parse-date date1)))
	(d2 (mapcar (lambda (s) (and s (string-to-int s)) )
		    (timezone-parse-date date2))))
    (- (timezone-absolute-from-gregorian 
	(nth 1 d1) (nth 2 d1) (car d1))
       (timezone-absolute-from-gregorian 
	(nth 1 d2) (nth 2 d2) (car d2)))))

;; Function taken from rmail.el.
(defun nnmail-move-inbox (inbox tofile)
  (let ((inbox (file-truename
		(expand-file-name (substitute-in-file-name inbox))))
	(tofile (nnmail-make-complex-temp-name (expand-file-name tofile)))
	movemail popmail errors)
    ;; Check whether the inbox is to be moved to the special tmp dir. 
    (if nnmail-tmp-directory
	(setq tofile (concat (file-name-as-directory nnmail-tmp-directory)
			     (file-name-nondirectory tofile))))
    ;; If getting from mail spool directory,
    ;; use movemail to move rather than just renaming,
    ;; so as to interlock with the mailer.
    (setq movemail (string= (file-name-directory inbox)
			    (file-truename rmail-spool-directory))
	  popmail (string-match "^po:" (file-name-nondirectory inbox)))
    (if popmail (setq inbox (file-name-nondirectory inbox)))
    (if movemail
	;; On some systems, /usr/spool/mail/foo is a directory
	;; and the actual inbox is /usr/spool/mail/foo/foo.
	(if (file-directory-p inbox)
	    (setq inbox (expand-file-name (user-login-name) inbox))))
    (if popmail
	(message "Getting mail from post office ...")
      (if (or (and (file-exists-p tofile)
		   (/= 0 (nth 7 (file-attributes tofile))))
	      (and (file-exists-p inbox)
		   (/= 0 (nth 7 (file-attributes inbox)))))
	  (message "Getting mail from %s..." inbox)))
    ;; Set TOFILE if have not already done so, and
    ;; rename or copy the file INBOX to TOFILE if and as appropriate.
    (cond ((or (file-exists-p tofile) (and (not popmail)
					   (not (file-exists-p inbox))))
	   nil)
	  ((and (not movemail) (not popmail))
	   ;; Try copying.  If that fails (perhaps no space),
	   ;; rename instead.
	   (condition-case nil
	       (copy-file inbox tofile nil)
	     (error
	      ;; Third arg is t so we can replace existing file TOFILE.
	      (rename-file inbox tofile t)))
	   ;; Make the real inbox file empty.
	   ;; Leaving it deleted could cause lossage
	   ;; because mailers often won't create the file.
	   (condition-case ()
	       (write-region (point) (point) inbox)
	     (file-error nil)))
	  (t
	   (unwind-protect
	       (save-excursion
		 (setq errors (generate-new-buffer " *nnmail loss*"))
		 (buffer-disable-undo errors)
		 (call-process
		  (expand-file-name "movemail" exec-directory)
		  nil errors nil inbox tofile)
		 (if (not (buffer-modified-p errors))
		     ;; No output => movemail won
		     nil
		   (set-buffer errors)
		   (subst-char-in-region (point-min) (point-max)
					 ?\n ?\  )
		   (goto-char (point-max))
		   (skip-chars-backward " \t")
		   (delete-region (point) (point-max))
		   (goto-char (point-min))
		   (if (looking-at "movemail: ")
		       (delete-region (point-min) (match-end 0)))
		   (beep t)
		   (message (concat "movemail: "
				    (buffer-substring (point-min)
						      (point-max))))
		   (sit-for 3)
		   nil)))))
    (and errors
	 (buffer-name errors)
	 (kill-buffer errors))
    tofile))


(defun nnmail-get-active ()
  "Returns an assoc of group names and active ranges.
nn*-request-list should have been called before calling this function."
  (let (group-assoc)
    ;; Go through all groups from the active list.
    (save-excursion
      (set-buffer nntp-server-buffer)
      (goto-char 1)
      (while (re-search-forward 
	      "^\\([^ \t]+\\)[ \t]+\\([0-9]+\\)[ \t]+\\([0-9]+\\)" nil t)
	(setq group-assoc
	      (cons (list (buffer-substring (match-beginning 1) 
					    (match-end 1))
			  (cons (string-to-int 
				 (buffer-substring (match-beginning 3)
						   (match-end 3)))
				(string-to-int 
				 (buffer-substring (match-beginning 2)
						   (match-end 2)))))
		    group-assoc))))
    ;; In addition, add all groups mentioned in `nnmail-split-methods'.
    (let ((methods (and (not (symbolp nnmail-split-methods))
			nnmail-split-methods)))
      (while methods
	(if (not (assoc (car (car methods)) group-assoc))
	    (setq group-assoc
		  (cons (list (car (car methods)) (cons 1 0)) 
			group-assoc)))
	(setq methods (cdr methods))))
    group-assoc))

(defun nnmail-save-active (group-assoc file-name)
  (let (group)
    (save-excursion
      (set-buffer (get-buffer-create " *nnmail active*"))
      (buffer-disable-undo (current-buffer))
      (erase-buffer)
      (while group-assoc
	(setq group (car group-assoc))
	(insert (format "%s %d %d y\n" (car group) (cdr (car (cdr group)) )
			(car (car (cdr group)))))
	(setq group-assoc (cdr group-assoc)))
      (write-region 1 (point-max) (expand-file-name file-name) nil 'nomesg)
      (kill-buffer (current-buffer)))))

(defun nnmail-split-incoming (incoming func &optional dont-kill)
  "Go through the entire INCOMING file and pick out each individual mail.
FUNC will be called with the buffer narrowed to each mail."
  (let ((delim (concat "^" rmail-unix-mail-delimiter))
	start)
    (save-excursion
      (set-buffer (get-buffer-create " *nnmail incoming*"))
      (buffer-disable-undo (current-buffer))
      (erase-buffer)
      (insert-file-contents incoming)
      (goto-char 1)
      (save-excursion (run-hooks 'nnmail-prepare-incoming-hook))
      ;; Go to the beginning of the first mail...
      (if (and (re-search-forward delim nil t)
	       (goto-char (match-beginning 0)))
	  ;; and then carry on until the bitter end.
	  (while (not (eobp))
	    (setq start (point))
	    ;; Skip all the headers in case there are more "From "s...
	    (if (not (search-forward "\n\n" nil t))
		(forward-line 1))
	    (if (re-search-forward delim nil t)
		(goto-char (match-beginning 0))
	      (goto-char (point-max)))
	    (save-excursion
	      (save-restriction
		(narrow-to-region start (point))
		(goto-char (point-min))
		(funcall func)))))
      (if dont-kill
	  (current-buffer)
	(kill-buffer (current-buffer))))))

;; Mail crossposts syggested by Brian Edmonds <edmonds@cs.ubc.ca>. 
(defun nnmail-article-group (func)
  "Look at the headers and return an alist of groups that match.
FUNC will be called with the group name to determine the article number."
  (let ((methods nnmail-split-methods)
	(obuf (current-buffer))
	(beg (point-min))
	end found group-art)
    (save-excursion
      ;; Find headers.
      (goto-char beg)
      (setq end (if (search-forward "\n\n" nil t) (point) (point-max)))
      (set-buffer (get-buffer-create " *nnmail work*"))
      (buffer-disable-undo (current-buffer))
      (erase-buffer)
      ;; Copy the headers into the work buffer.
      (insert-buffer-substring obuf beg end)
      ;; Fold continuation lines.
      (goto-char (point-min))
      (while (re-search-forward "\\(\r?\n[ \t]+\\)+" nil t)
	(replace-match " " t t))
      (if (and (symbolp nnmail-split-methods)
	       (fboundp nnmail-split-methods))
	  (setq group-art
		(mapcar
		 (lambda (group) (cons group (funcall func group)))
		 (funcall nnmail-split-methods)))
	;; Go throught the split methods to find a match.
	(while (and methods (or nnmail-crosspost (not group-art)))
	  (goto-char (point-max))
	  (if (or (cdr methods)
		  (not (equal "" (nth 1 (car methods)))))
	      (if (and (condition-case () 
			   (if (stringp (nth 1 (car methods)))
			       (re-search-backward
				(car (cdr (car methods))) nil t)
			     ;; Suggested by Brian Edmonds <edmonds@cs.ubc.ca>.
			     (funcall (nth 1 (car methods)) 
				      (car (car methods))))
			 (error nil))
		       ;; Don't enter the article into the same group twice.
		       (not (assoc (car (car methods)) group-art)))
		  (setq group-art
			(cons (cons (car (car methods))
				    (funcall func (car (car methods)))) 
			      group-art)))
	    (or group-art
		(setq group-art 
		      (list (cons (car (car methods)) 
				  (funcall func (car (car methods))))))))
	  (setq methods (cdr methods))))
      (kill-buffer (current-buffer))
      group-art)))

(defun nnmail-insert-lines ()
  "Insert how many lines and chars there are in the body of the mail."
  (let (lines chars)
    (save-excursion
      (goto-char (point-min))
      (if (search-forward "\n\n" nil t) 
	  (progn
	    (setq chars (- (point-max) (point)))
	    (setq lines (- (count-lines (point) (point-max)) 1))
	    (forward-char -1)
	    (save-excursion
	      (if (re-search-backward "^Lines: " nil t)
		  (delete-region (point) (progn (forward-line 1) (point)))))
	    (insert (format "Lines: %d\n" lines))
	    chars)))))

(defun nnmail-insert-xref (group-alist)
  "Insert an Xref line based on the (group . article) alist."
  (save-excursion
    (goto-char (point-min))
    (if (search-forward "\n\n" nil t) 
	(progn
	  (forward-char -1)
	  (if (re-search-backward "^Xref: " nil t)
	      (delete-region (match-beginning 0) 
			     (progn (forward-line 1) (point))))
	  (insert (format "Xref: %s" (system-name)))
	  (while group-alist
	    (insert (format " %s:%d" (car (car group-alist)) 
			    (cdr (car group-alist))))
	    (setq group-alist (cdr group-alist)))
	  (insert "\n")))))

;; Written by byer@mv.us.adobe.com (Scott Byer).
(defun nnmail-make-complex-temp-name (prefix)
  (let ((newname (make-temp-name prefix))
	(newprefix prefix))
    (while (file-exists-p newname)
      (setq newprefix (concat newprefix "x"))
      (setq newname (make-temp-name newprefix)))
    newname))

(provide 'nnmail)

;;; nnml.el ends here
