;;; gnus-visual: display-oriented parts of Gnus.
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
(require (if gnus-xemacs 'auc-menu 'easymenu))

(defvar gnus-summary-selected-face 'underline
  "*Face used for highlighting the current article in the summary buffer.")

(defvar gnus-visual-summary-highlight
  '(((> score default) . bold)
    ((< score default) . italic))
  "*Alist of `(FORM . FACE)'.
Summary lines are highlighted with the FACE for the first FORM which
evaluate to a non-nil value.  

Point will be at the beginning of the line when FORM is evaluated.
The following can be used for convenience:

score:   (gnus-summary-article-score)
default: gnus-summary-default-score
below:   gnus-summary-mark-below

To check for marks, e.g. to underline replied articles, use
`gnus-summary-article-mark': 

   ((= (gnus-summary-article-mark) gnus-replied-mark) . underline)")

(eval-and-compile
  (autoload 'nnkiboze-generate-groups "nnkiboze"))

;; Newsgroup buffer

;; Make a menu bar item.
(defun gnus-group-make-menu-bar ()
  (easy-menu-define
   gnus-group-reading-menu
   gnus-group-mode-map
   ""
   '("Group"
     ["Read" gnus-group-read-group t]
     ["Select" gnus-group-select-group t]
     ["Catch up" gnus-group-catchup-current t]
     ["Catch up all articles" gnus-group-catchup-current-all t]
     ["Check for new articles" gnus-group-get-new-news-this-group t]
     ["Toggle subscription" gnus-group-unsubscribe-current-group t]
     ["Kill" gnus-group-kill-group t]
     ["Yank" gnus-group-yank-group t]
     ["Describe" gnus-group-describe-group t]
     ["Fetch FAQ" gnus-group-fetch-faq t]
     ["Edit kill file" gnus-group-edit-local-kill t]
     ["Expire articles" gnus-group-expire-articles t]
     ["Set group level" gnus-group-set-current-level t]
     ))
  
  (easy-menu-define
   gnus-group-group-menu
   gnus-group-mode-map
   ""
   '("Groups"
     ("Listing"
      ["List subscribed groups" gnus-group-list-groups t]
      ["List all groups" gnus-group-list-all-groups t]
      ["List groups matching..." gnus-group-list-matching t]
      ["List killed groups" gnus-group-list-killed t]
      ["List zombie groups" gnus-group-list-zombies t]
      ["Describe all groups" gnus-group-describe-all-groups t]
      ["Group apropos" gnus-group-apropos t]
      ["Group and description apropos" gnus-group-description-apropos t]
      ["List groups matching..." gnus-group-list-matching t])
     ("Subscribe"
      ["Subscribe to random group" gnus-group-unsubscribe-group t]
      ["Kill all newsgroups in region" gnus-group-kill-region t]
      ["Kill all zombie groups" gnus-group-kill-all-zombies t])
     ("Foreign groups"
      ["Make a foreign group" gnus-group-make-group t]
      ["Edit a group entry" gnus-group-edit-group t]
      ["Add a directory group" gnus-group-make-directory-group t]
      ["Add the help group" gnus-group-make-help-group t]
      ["Add the archive group" gnus-group-make-archive-group t]
      ["Make a kiboze group" gnus-group-make-kiboze-group t])
     ["Jump to group" gnus-group-jump-to-group t]
     ["Best unread group" gnus-group-best-unread-group t]
     ))

  (easy-menu-define
   gnus-group-post-menu
   gnus-group-mode-map
   ""
   '("Post"
     ["Send a mail" gnus-group-mail t]
     ["Post an article" gnus-group-post-news t]
     ))
  
  (easy-menu-define
   gnus-group-misc-menu
   gnus-group-mode-map
   ""
   '("Misc"
     ["Send a bug report" gnus-bug t]
     ["Check for new news" gnus-group-get-new-news t]     
     ["Delete bogus groups" gnus-group-check-bogus-groups t]
     ["Find new newsgroups" gnus-find-new-newsgroups t]
     ["Restart Gnus" gnus-group-restart t]
     ["Read init file" gnus-group-read-init-file t]
     ["Browse foreign server" gnus-group-browse-foreign-server t]
     ["Edit the global kill file" gnus-group-edit-global-kill t]
     ["Expire all expirable articles" gnus-group-expire-all-groups t]
     ["Generate any kiboze groups" nnkiboze-generate-groups t]
     ["Gnus version" gnus-version t]
     ["Save .newsrc files" gnus-group-save-newsrc t]
     ["Suspend Gnus" gnus-group-suspend t]
     ["Clear dribble buffer" gnus-group-clear-dribble t]
     ["Exit from Gnus" gnus-group-exit t]
     ["Exit without saving" gnus-group-quit t]
     ["Sort group buffer" gnus-group-sort-groups t]
     ["Edit global KILL file" gnus-group-edit-global-kill t]
     ))

  )

;; Summary buffer
(defun gnus-summary-make-menu-bar ()

  (easy-menu-define
   gnus-summary-mark-menu
   gnus-summary-mode-map
   ""
   '("Mark"
     ("Read"
      ["Mark as read" gnus-summary-mark-as-read-forward t]
      ["Mark same subject and select" gnus-summary-kill-same-subject-and-select t]
      ["Mark same subject" gnus-summary-kill-same-subject t]
      ["Catchup" gnus-summary-catchup t]
      ["Catchup all" gnus-summary-catchup-all t]
      ["Catchup to here" gnus-summary-catchup-to-here t]
      ["Catchup region" gnus-summary-mark-region-as-read t])
     ("Various"
      ["Tick" gnus-summary-tick-article-forward t]
      ["Mark as dormant" gnus-summary-mark-as-dormant t]
      ["Remove marks" gnus-summary-clear-mark-forward t]
      ["Set expirable mark" gnus-summary-mark-as-expirable t]
      ["Set bookmark" gnus-summary-set-bookmark t]
      ["Remove bookmark" gnus-summary-remove-bookmark t])
     ("Score"
      ["Raise score" gnus-summary-raise-score t]
      ["Lower score" gnus-summary-lower-score t]
      ["Set score" gnus-summary-set-score t])
     ("Display"
      ["Remove lines marked as read" gnus-summary-remove-lines-marked-as-read t]
      ["Remove lines marked with..." gnus-summary-remove-lines-marked-with t]
      ["Show dormant articles" gnus-summary-show-all-dormant t]
      ["Hide dormant articles" gnus-summary-hide-all-dormant t]
      ["Show expunged articles" gnus-summary-show-all-expunged t])
     ("Process mark"
      ["Set mark" gnus-summary-mark-as-processable t]
      ["Remove mark" gnus-summary-unmark-as-processable t]
      ["Remove all marks" gnus-summary-unmark-all-processable t]
      ["Mark series" gnus-uu-mark-series t]
      ["Mark region" gnus-uu-mark-region t]
      ["Mark by regexp" gnus-uu-mark-by-regexp t]
      ["Mark all" gnus-uu-mark-all t]
      ["Mark sparse" gnus-uu-mark-sparse t]
      ["Mark thread" gnus-uu-mark-thread t]
      )
     ))

  (easy-menu-define
   gnus-summary-move-menu
   gnus-summary-mode-map
   ""
   '("Move"
     ["Scroll article forwards" gnus-summary-next-page t]
     ["Next unread article" gnus-summary-next-unread-article t]
     ["Previous unread article" gnus-summary-prev-unread-article t]
     ["Next article" gnus-summary-next-article t]
     ["Previous article" gnus-summary-prev-article t]
     ["Next article same subject" gnus-summary-next-same-subject t]
     ["Previous article same subject" gnus-summary-prev-same-subject t]
     ["First unread article" gnus-summary-first-unread-article t]
     ["Go to subject number..." gnus-summary-goto-subject t]
     ["Go to the last article" gnus-summary-goto-last-article t]
     ["Pop article off history" gnus-summary-pop-article t]
     ))

  (easy-menu-define
   gnus-summary-article-menu
   gnus-summary-mode-map
   ""
   '("Article"
     ("Hide"
      ("Date"
       ["Local" gnus-article-date-local t]
       ["UT" gnus-article-date-local t]
       ["Lapsed" gnus-article-date-local t])
      ["Headers" gnus-article-hide-headers t]
      ["Signature" gnus-article-hide-signature t]
      ["Citation" gnus-article-hide-citation t]
      ["Overstrike" gnus-article-treat-overstrike t]
      ["Word wrap" gnus-article-word-wrap t]
      ["CR" gnus-article-remove-cr t]
      ["Show X-Face" gnus-article-display-x-face t]
      ["Quoted-Printable" gnus-article-de-quoted-unreadable t])
     ("Extract"
      ["Uudecode" gnus-uu-decode-uu t]
      ["Uudecode and save" gnus-uu-decode-uu-and-save t]
      ["Unshar" gnus-uu-decode-unshar t]
      ["Unshar and save" gnus-uu-decode-unshar-and-save t]
      ["Save" gnus-uu-decode-save t]
      ["Binhex" gnus-uu-decode-binhex t])
     ["Enter digest buffer" gnus-summary-enter-digest-group t]
     ["Isearch article" gnus-summary-isearch-article t]
     ["Search all articles" gnus-summary-search-article-forward t]
     ["Beginning of the article" gnus-summary-beginning-of-article t]
     ["End of the article" gnus-summary-end-of-article t]
     ["Fetch parent of article" gnus-summary-refer-parent-article t]
     ["Fetch article with id..." gnus-summary-refer-article t]
     ["Stop page breaking" gnus-summary-stop-page-breaking t]
     ["Caesar rotate" gnus-summary-caesar-message t]
     ["Redisplay" gnus-summary-show-article t]
     ["Toggle header" gnus-summary-toggle-header t]
     ["Toggle MIME" gnus-summary-toggle-mime t]
     ["Save" gnus-summary-save-article t]
     ["Save in mail format" gnus-summary-save-article-mail t]
     ["Pipe through a filter" gnus-summary-pipe-output t]
     ("Mail articles"
      ["Respool article" gnus-summary-respool-article t]
      ["Move article" gnus-summary-move-article t]
      ["Edit article" gnus-summary-edit-article t]
      ["Delete article" gnus-summary-delete-article t])
     ))

  (easy-menu-define
   gnus-summary-thread-menu
   gnus-summary-mode-map
   ""
   '("Threads"
     ["Toggle threading" gnus-summary-toggle-threads t]
     ["Display hidden thread" gnus-summary-show-thread t]
     ["Hide thread" gnus-summary-hide-thread t]
     ["Go to next thread" gnus-summary-next-thread t]
     ["Go to previous thread" gnus-summary-prev-thread t]
     ["Go down thread" gnus-summary-down-thread t]
     ["Go up thread" gnus-summary-up-thread t]
     ["Mark thread as read" gnus-summary-kill-thread t]
     ["Lower thread score" gnus-summary-lower-thread t]
     ["Raise thread score" gnus-summary-raise-thread t]
     ))

  (easy-menu-define
   gnus-summary-misc-menu
   gnus-summary-mode-map
   ""
   '("Misc"
     ("Sort"
      ["Sort by number" gnus-summary-sort-by-number t]
      ["Sort by author" gnus-summary-sort-by-author t]
      ["Sort by subject" gnus-summary-sort-by-subject t]
      ["Sort by date" gnus-summary-sort-by-date t])
     ("Exit"
      ["Catchup and exit" gnus-summary-catchup-and-exit t]
      ["Catchup and goto next" gnus-summary-catchup-and-goto-next-group t]
      ["Exit group" gnus-summary-exit t]
      ["Exit group without updating" gnus-summary-quit t]
      ["Reselect group" gnus-summary-reselect-current-group t]
      ["Rescan group" gnus-summary-rescan-group t])
     ["Fetch group FAQ" gnus-summary-fetch-faq t]
     ["Filter articles" gnus-summary-execute-command t]
     ["Toggle line truncation" gnus-summary-toggle-truncation t]
     ["Expire expirable articles" gnus-summary-expire-articles t]
     ["Describe group" gnus-summary-describe-group t]
     ["Edit local kill file" gnus-summary-edit-local-kill t]
     ["Edit global kill file" gnus-summary-edit-global-kill t]
     ))

  (easy-menu-define
   gnus-summary-post-menu
   gnus-summary-mode-map
   ""
   '("Post"
     ["Post an article" gnus-summary-post-news t]
     ["Followup" gnus-summary-followup t]
     ["Followup and yank" gnus-summary-followup-with-original t]
     ["Supersede article" gnus-summary-supersede-article t]
     ["Cancel article" gnus-summary-cancel-article t]
     ["Reply" gnus-summary-reply t]
     ["Reply and yank" gnus-summary-reply-with-original t]
     ["Mail forward" gnus-summary-mail-forward t]
     ["Post forward" gnus-summary-post-forward t]
     ["Digest and mail" gnus-uu-digest-mail-forward t]
     ["Digest and post" gnus-uu-digest-post-forward t]
     ["Send a mail" gnus-summary-mail-other-window t]
     ["Reply & followup" gnus-summary-followup-and-reply t]
     ["Reply & followup and yank" gnus-summary-followup-and-reply-with-original t]
     ["Uuencode and post" gnus-uu-post-news t]
     ))

  (easy-menu-define
   gnus-summary-kill-menu
   gnus-summary-mode-map
   ""
   '("Score"
     ("Score file"
      ["Switch current score file" gnus-score-change-score-file t]
      ["Set mark below" gnus-score-set-mark-below t]
      ["Set expunge below" gnus-score-set-expunge-below t]
      ["Edit current score file" gnus-score-edit-alist t]
      ["Edit score file" gnus-score-edit-file t])
     ["Raise score with current subject" 
      gnus-summary-temporarily-raise-by-subject t]
     ["Raise score with current author" 
      gnus-summary-temporarily-raise-by-author t]
     ["Raise score with current thread" 
      gnus-summary-temporarily-raise-by-thread t]
     ["Raise score with current crossposting" 
      gnus-summary-temporarily-raise-by-xref t]
     ["Permanently raise score with current subject"
      gnus-summary-raise-by-subject t]
     ["Permanently raise score with current author" 
      gnus-summary-raise-by-author t]
     ["Permanently raise score with current crossposting" 
      gnus-summary-raise-by-xref t]
     ["Permanently raise score for followups to current author"
      gnus-summary-raise-followups-to-author t]
     ["Lower score with current subject" 
      gnus-summary-temporarily-lower-by-subject t]
     ["Lower score with current author" 
      gnus-summary-temporarily-lower-by-author t]
     ["Lower score with current thread" 
      gnus-summary-temporarily-lower-by-thread t]
     ["Lower score with current crossposting" 
      gnus-summary-temporarily-lower-by-xref t]
     ["Permanently lower score with current subject"
      gnus-summary-lower-by-subject t]
     ["Permanently lower score with current author" 
      gnus-summary-lower-by-author t]
     ["Permanently lower score with current crossposting" 
      gnus-summary-lower-by-xref t]
     ["Permanently lower score for followups to current author"
      gnus-summary-lower-followups-to-author t]
     ))
  )
 
;; Article buffer
(defun gnus-article-make-menu-bar ()

 (easy-menu-define
   gnus-article-article-menu
   gnus-article-mode-map
   ""
   '("Article"
     ["Scroll forwards" gnus-article-next-page t]
     ["Scroll backwards" gnus-article-prev-page t]
     ["Show summary" gnus-article-show-summary t]
     ["Fetch Message-ID at point" gnus-article-refer-article t]
     ["Mail to address at point" gnus-article-mail t]
     ["Mail to address at point and include original"
      gnus-article-mail-with-original t]
     ))

 (easy-menu-define
   gnus-article-treatment-menu
   gnus-article-mode-map
   ""
   '("Treatment"
     ["Hide headers" gnus-article-hide-headers t]
     ["Hide signature" gnus-article-hide-signature t]
     ["Hide citation" gnus-article-hide-citation t]
     ["Treat overstrike" gnus-article-treat-overstrike t]
     ["Remove carriage return" gnus-article-remove-cr t]
     ["Remove quoted-unreadble" gnus-article-de-quoted-unreadable t]
     ))
 )

(if gnus-xemacs
    (defun gnus-visual-highlight-selected-summary ()
      (if gnus-summary-selected-face
	  (save-excursion
	    (let* ((beg (progn (beginning-of-line) (point)))
		   (end (progn (end-of-line) (point)))
		   (from (or
			  (next-single-property-change beg 'mouse-face nil end)
			  beg))
		   (to (or (next-single-property-change from 'mouse-face nil end)
			   end)))
	      (if gnus-newsgroup-selected-overlay
		  (move-overlay gnus-newsgroup-selected-overlay 
				from to (current-buffer))
		(setq gnus-newsgroup-selected-overlay (make-overlay from to))
		(overlay-put gnus-newsgroup-selected-overlay 'face 
			     gnus-summary-selected-face))))))

(defun gnus-visual-highlight-selected-summary ()
    ;; Added by Per Abrahamsen <amanda@iesd.auc.dk>.
    ;; Highlight selected article in summary buffer
    (if gnus-summary-selected-face
	(save-excursion
	  (let* ((beg (progn (beginning-of-line) (point)))
		 (end (progn (end-of-line) (point)))
		 (to (max 1 (1- (previous-single-property-change
				 end 'mouse-face nil beg))))
		 (from (1+ (next-single-property-change 
			    beg 'mouse-face nil end))))
	    (if (< to beg)
		(progn
		  (setq from beg)
		  (setq to end)))
	    (if gnus-newsgroup-selected-overlay
		(move-overlay gnus-newsgroup-selected-overlay 
			      from to (current-buffer))
	      (setq gnus-newsgroup-selected-overlay (make-overlay from to))
	      (overlay-put gnus-newsgroup-selected-overlay 'face 
			   gnus-summary-selected-face))))))
)


;; New implementation by Christian Limpach <Christian.Limpach@nice.ch>.
(defun gnus-visual-summary-highlight-line ()
  "Highlight current line according to `gnus-visual-summary-highlight'."
  (let* ((list gnus-visual-summary-highlight)
	 (p (point))
	 (end (progn (end-of-line) (point)))
	 ;; now find out where the line starts and leave point there.
	 (beg (progn (beginning-of-line) (point)))
	 (score (or (cdr (assq (or (car (get-text-property beg 'gnus))
				   gnus-current-article)
			       gnus-newsgroup-scored))
		    gnus-summary-default-score 0))
	 (default gnus-summary-default-score)
	 (mark (get-text-property beg 'gnus-mark))
	 (inhibit-read-only t))
    (while (and list (not (eval (car (car list)))))
      (setq list (cdr list)))
    (let ((face (and list (cdr (car list)))))
      ;; BUG! For some reason the text properties of the first
      ;; characters get mangled.
      (or (eq face (get-text-property (+ beg 10) 'face))
	  (put-text-property beg end 'face face)))
    (goto-char p)))

(defvar mode-motion-hook nil)
(defun gnus-install-mouse-tracker ()
  (require 'mode-motion)
  (setq mode-motion-hook 'mode-motion-highlight-line))

(if (not gnus-xemacs)
    ()
  (setq gnus-group-mode-hook
	(cons
	 (lambda ()
	   (easy-menu-add gnus-group-reading-menu)
	   (easy-menu-add gnus-group-group-menu)
	   (easy-menu-add gnus-group-post-menu)
	   (easy-menu-add gnus-group-misc-menu)
           (gnus-install-mouse-tracker)) 
	 gnus-group-mode-hook))
  (setq gnus-summary-mode-hook
	(cons
	 (lambda ()
	   (easy-menu-add gnus-summary-mark-menu)
	   (easy-menu-add gnus-summary-move-menu)
	   (easy-menu-add gnus-summary-article-menu)
	   (easy-menu-add gnus-summary-thread-menu)
	   (easy-menu-add gnus-summary-misc-menu)
	   (easy-menu-add gnus-summary-post-menu)
	   (easy-menu-add gnus-summary-kill-menu)
           (gnus-install-mouse-tracker)) 
	 gnus-summary-mode-hook))
  (setq gnus-article-mode-hook
	(cons
	 (lambda ()
	   (easy-menu-add gnus-article-article-menu)
	   (easy-menu-add gnus-article-treatment-menu)) 
	 gnus-article-mode-hook)))

(provide 'gnus-vis)

;;; gnus-visual.el ends here
