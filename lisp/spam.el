;;; spam.el --- Identifying spam
;; Copyright (C) 2002, 2003 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: network

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

;;; This module addresses a few aspects of spam control under Gnus.  Page
;;; breaks are used for grouping declarations and documentation relating to
;;; each particular aspect.

;;; The integration with Gnus is not yet complete.  See various `FIXME'
;;; comments, below, for supplementary explanations or discussions.

;;; Several TODO items are marked as such

;;; Code:

(eval-when-compile (require 'cl))

(require 'gnus-sum)

(require 'gnus-uu)			; because of key prefix issues
(require 'gnus)	; for the definitions of group content classification and spam processors
(require 'message)			;for the message-fetch-field functions

;; for nnimap-split-download-body-default
(eval-when-compile (require 'nnimap))

;; autoload executable-find
(eval-and-compile
  ;; executable-find is not autoloaded in Emacs 20
  (autoload 'executable-find "executable"))

;; autoload query-dig
(eval-and-compile
  (autoload 'query-dig "dig"))

;; autoload spam-report
(eval-and-compile
  (autoload 'spam-report-gmane "spam-report"))

;; autoload query-dns
(eval-and-compile
  (autoload 'query-dns "dns"))

;;; Main parameters.

(defgroup spam nil
  "Spam configuration.")

(defcustom spam-directory "~/News/spam/"
  "Directory for spam whitelists and blacklists."
  :type 'directory
  :group 'spam)

(defcustom spam-move-spam-nonspam-groups-only t
  "Whether spam should be moved in non-spam groups only.
When t, only ham and unclassified groups will have their spam moved
to the spam-process-destination.  When nil, spam will also be moved from
spam groups."
  :type 'boolean
  :group 'spam)

(defcustom spam-process-ham-in-nonham-groups nil
  "Whether ham should be processed in non-ham groups."
  :type 'boolean
  :group 'spam)

(defcustom spam-process-ham-in-spam-groups nil
  "Whether ham should be processed in spam groups."
  :type 'boolean
  :group 'spam)

(defcustom spam-mark-only-unseen-as-spam t
  "Whether only unseen articles should be marked as spam in spam
groups.  When nil, all unread articles in a spam group are marked as
spam.  Set this if you want to leave an article unread in a spam group
without losing it to the automatic spam-marking process."
  :type 'boolean
  :group 'spam)

(defcustom spam-mark-ham-unread-before-move-from-spam-group nil
  "Whether ham should be marked unread before it's moved out of a spam
group according to ham-process-destination.  This variable is an
official entry in the international Longest Variable Name
Competition."
  :type 'boolean
  :group 'spam)

(defcustom spam-whitelist (expand-file-name "whitelist" spam-directory)
  "The location of the whitelist.
The file format is one regular expression per line.
The regular expression is matched against the address."
  :type 'file
  :group 'spam)

(defcustom spam-blacklist (expand-file-name "blacklist" spam-directory)
  "The location of the blacklist.
The file format is one regular expression per line.
The regular expression is matched against the address."
  :type 'file
  :group 'spam)

(defcustom spam-use-dig t
  "Whether query-dig should be used instead of query-dns."
  :type 'boolean
  :group 'spam)

(defcustom spam-use-blacklist nil
  "Whether the blacklist should be used by spam-split."
  :type 'boolean
  :group 'spam)

(defcustom spam-use-whitelist nil
  "Whether the whitelist should be used by spam-split."
  :type 'boolean
  :group 'spam)

(defcustom spam-use-whitelist-exclusive nil
  "Whether whitelist-exclusive should be used by spam-split.
Exclusive whitelisting means that all messages from senders not in the whitelist
are considered spam."
  :type 'boolean
  :group 'spam)

(defcustom spam-use-blackholes nil
  "Whether blackholes should be used by spam-split."
  :type 'boolean
  :group 'spam)

(defcustom spam-use-hashcash nil
  "Whether hashcash payments should be detected by spam-split."
  :type 'boolean
  :group 'spam)

(defcustom spam-use-regex-headers nil
  "Whether a header regular expression match should be used by spam-split.
Also see the variables `spam-regex-headers-spam' and `spam-regex-headers-ham'."
  :type 'boolean
  :group 'spam)

(defcustom spam-use-regex-body nil
  "Whether a body regular expression match should be used by spam-split.
Also see the variables `spam-regex-body-spam' and `spam-regex-body-ham'."
  :type 'boolean
  :group 'spam)

(defcustom spam-use-bogofilter-headers nil
  "Whether bogofilter headers should be used by spam-split.
Enable this if you pre-process messages with Bogofilter BEFORE Gnus sees them."
  :type 'boolean
  :group 'spam)

(defcustom spam-use-bogofilter nil
  "Whether bogofilter should be invoked by spam-split.
Enable this if you want Gnus to invoke Bogofilter on new messages."
  :type 'boolean
  :group 'spam)

(defcustom spam-use-BBDB nil
  "Whether BBDB should be used by spam-split."
  :type 'boolean
  :group 'spam)

(defcustom spam-use-BBDB-exclusive nil
  "Whether BBDB-exclusive should be used by spam-split.
Exclusive BBDB means that all messages from senders not in the BBDB are 
considered spam."
  :type 'boolean
  :group 'spam)

(defcustom spam-use-ifile nil
  "Whether ifile should be used by spam-split."
  :type 'boolean
  :group 'spam)

(defcustom spam-use-stat nil
  "Whether spam-stat should be used by spam-split."
  :type 'boolean
  :group 'spam)

(defcustom spam-use-spamoracle nil
  "Whether spamoracle should be used by spam-split."
  :type 'boolean
  :group 'spam)

(defcustom spam-install-hooks (or
			       spam-use-dig
			       spam-use-blacklist
			       spam-use-whitelist 
			       spam-use-whitelist-exclusive 
			       spam-use-blackholes 
			       spam-use-hashcash 
			       spam-use-regex-headers 
			       spam-use-regex-body 
			       spam-use-bogofilter-headers 
			       spam-use-bogofilter 
			       spam-use-BBDB 
			       spam-use-BBDB-exclusive 
			       spam-use-ifile 
			       spam-use-stat
			       spam-use-spamoracle)
  "Whether the spam hooks should be installed, default to t if one of
the spam-use-* variables is set."
  :group 'gnus-registry
  :type 'boolean)

(defcustom spam-split-group "spam"
  "Group name where incoming spam should be put by spam-split."
  :type 'string
  :group 'spam)

;;; TODO: deprecate this variable, it's confusing since it's a list of strings, not regular expressions
(defcustom spam-junk-mailgroups (cons spam-split-group '("mail.junk" "poste.pourriel"))
  "Mailgroups with spam contents.
All unmarked article in such group receive the spam mark on group entry."
  :type '(repeat (string :tag "Group"))
  :group 'spam)

(defcustom spam-blackhole-servers '("bl.spamcop.net" "relays.ordb.org" 
				    "dev.null.dk" "relays.visi.com")
  "List of blackhole servers."
  :type '(repeat (string :tag "Server"))
  :group 'spam)

(defcustom spam-blackhole-good-server-regex nil
  "String matching IP addresses that should not be checked in the blackholes"
  :type '(radio (const nil)
		(regexp :format "%t: %v\n" :size 0))
  :group 'spam)

(defcustom spam-face 'gnus-splash-face
  "Face for spam-marked articles"
  :type 'face
  :group 'spam)

(defcustom spam-regex-headers-spam '("^X-Spam-Flag: YES")
  "Regular expression for positive header spam matches"
  :type '(repeat (regexp :tag "Regular expression to match spam header"))
  :group 'spam)

(defcustom spam-regex-headers-ham '("^X-Spam-Flag: NO")
  "Regular expression for positive header ham matches"
  :type '(repeat (regexp :tag "Regular expression to match ham header"))
  :group 'spam)

(defcustom spam-regex-body-spam '()
  "Regular expression for positive body spam matches"
  :type '(repeat (regexp :tag "Regular expression to match spam body"))
  :group 'spam)

(defcustom spam-regex-body-ham '()
  "Regular expression for positive body ham matches"
  :type '(repeat (regexp :tag "Regular expression to match ham body"))
  :group 'spam)

(defgroup spam-ifile nil
  "Spam ifile configuration."
  :group 'spam)

(defcustom spam-ifile-path (executable-find "ifile")
  "File path of the ifile executable program."
  :type '(choice (file :tag "Location of ifile")
		 (const :tag "ifile is not installed"))
  :group 'spam-ifile)

(defcustom spam-ifile-database-path nil
  "File path of the ifile database."
  :type '(choice (file :tag "Location of the ifile database")
		 (const :tag "Use the default"))
  :group 'spam-ifile)

(defcustom spam-ifile-spam-category "spam"
  "Name of the spam ifile category."  
  :type 'string
  :group 'spam-ifile)

(defcustom spam-ifile-ham-category nil
  "Name of the ham ifile category.  If nil, the current group name will
be used."
  :type '(choice (string :tag "Use a fixed category")
                (const :tag "Use the current group name"))
  :group 'spam-ifile)

(defcustom spam-ifile-all-categories nil
  "Whether the ifile check will return all categories, or just spam.
Set this to t if you want to use the spam-split invocation of ifile as
your main source of newsgroup names."
  :type 'boolean
  :group 'spam-ifile)

(defgroup spam-bogofilter nil
  "Spam bogofilter configuration."
  :group 'spam)

(defcustom spam-bogofilter-path (executable-find "bogofilter")
  "File path of the Bogofilter executable program."
  :type '(choice (file :tag "Location of bogofilter")
		 (const :tag "Bogofilter is not installed"))
  :group 'spam-bogofilter)

(defcustom spam-bogofilter-header "X-Bogosity"
  "The header that Bogofilter inserts in messages."
  :type 'string
  :group 'spam-bogofilter)

(defcustom spam-bogofilter-spam-switch "-s"
  "The switch that Bogofilter uses to register spam messages."
  :type 'string
  :group 'spam-bogofilter)

(defcustom spam-bogofilter-ham-switch "-n"
  "The switch that Bogofilter uses to register ham messages."
  :type 'string
  :group 'spam-bogofilter)

(defcustom spam-bogofilter-bogosity-positive-spam-header "^\\(Yes\\|Spam\\)"
  "The regex on `spam-bogofilter-header' for positive spam identification."
  :type 'regexp
  :group 'spam-bogofilter)

(defcustom spam-bogofilter-database-directory nil
  "Directory path of the Bogofilter databases."
  :type '(choice (directory :tag "Location of the Bogofilter database directory")
		 (const :tag "Use the default"))
  :group 'spam-ifile)

(defgroup spam-spamoracle nil
  "Spam ifile configuration."
  :group 'spam)

(defcustom spam-spamoracle-database nil 
  "Location of spamoracle database file. When nil, use the default
spamoracle database."
  :type '(choice (directory :tag "Location of spamoracle database file.")
		 (const :tag "Use the default"))
  :group 'spam-spamoracle)

(defcustom spam-spamoracle-binary (executable-find "spamoracle")
  "Location of the spamoracle binary."
  :type '(choice (directory :tag "Location of the spamoracle binary")
		 (const :tag "Use the default"))
  :group 'spam-spamoracle)

;;; Key bindings for spam control.

(gnus-define-keys gnus-summary-mode-map
  "St" spam-bogofilter-score
  "Sx" gnus-summary-mark-as-spam
  "Mst" spam-bogofilter-score
  "Msx" gnus-summary-mark-as-spam
  "\M-d" gnus-summary-mark-as-spam)

;; convenience functions
(defun spam-group-ham-mark-p (group mark &optional spam)
  (when (stringp group)
    (let* ((marks (spam-group-ham-marks group spam))
	   (marks (if (symbolp mark) 
		      marks 
		    (mapcar 'symbol-value marks))))
      (memq mark marks))))

(defun spam-group-spam-mark-p (group mark)
  (spam-group-ham-mark-p group mark t))

(defun spam-group-ham-marks (group &optional spam)
  (when (stringp group)
    (let* ((marks (if spam
		     (gnus-parameter-spam-marks group)
		   (gnus-parameter-ham-marks group)))
	   (marks (car marks))
	   (marks (if (listp (car marks)) (car marks) marks)))
      marks)))

(defun spam-group-spam-marks (group)
  (spam-group-ham-marks group t))

(defun spam-group-spam-contents-p (group)
  (if (stringp group)
      (or (member group spam-junk-mailgroups)
	  (memq 'gnus-group-spam-classification-spam 
		(gnus-parameter-spam-contents group)))
    nil))
  
(defun spam-group-ham-contents-p (group)
  (if (stringp group)
      (memq 'gnus-group-spam-classification-ham 
	    (gnus-parameter-spam-contents group))
    nil))

(defun spam-group-processor-p (group processor)
  (if (and (stringp group)
	   (symbolp processor))
      (member processor (car (gnus-parameter-spam-process group)))
    nil))

(defun spam-group-spam-processor-report-gmane-p (group)
  (spam-group-processor-p group 'gnus-group-spam-exit-processor-report-gmane))

(defun spam-group-spam-processor-bogofilter-p (group)
  (spam-group-processor-p group 'gnus-group-spam-exit-processor-bogofilter))

(defun spam-group-spam-processor-blacklist-p (group)
  (spam-group-processor-p group 'gnus-group-spam-exit-processor-blacklist))

(defun spam-group-spam-processor-ifile-p (group)
  (spam-group-processor-p group 'gnus-group-spam-exit-processor-ifile))

(defun spam-group-ham-processor-ifile-p (group)
  (spam-group-processor-p group 'gnus-group-ham-exit-processor-ifile))

(defun spam-group-spam-processor-spamoracle-p (group)
  (spam-group-processor-p group 'gnus-group-spam-exit-processor-spamoracle))

(defun spam-group-ham-processor-bogofilter-p (group)
  (spam-group-processor-p group 'gnus-group-ham-exit-processor-bogofilter))

(defun spam-group-spam-processor-stat-p (group)
  (spam-group-processor-p group 'gnus-group-spam-exit-processor-stat))

(defun spam-group-ham-processor-stat-p (group)
  (spam-group-processor-p group 'gnus-group-ham-exit-processor-stat))

(defun spam-group-ham-processor-whitelist-p (group)
  (spam-group-processor-p group 'gnus-group-ham-exit-processor-whitelist))

(defun spam-group-ham-processor-BBDB-p (group)
  (spam-group-processor-p group 'gnus-group-ham-exit-processor-BBDB))

(defun spam-group-ham-processor-copy-p (group)
  (spam-group-processor-p group 'gnus-group-ham-exit-processor-copy))

(defun spam-group-ham-processor-spamoracle-p (group)
  (spam-group-processor-p group 'gnus-group-ham-exit-processor-spamoracle))

;;; Summary entry and exit processing.

(defun spam-summary-prepare ()
  (spam-mark-junk-as-spam-routine))

;; The spam processors are invoked for any group, spam or ham or neither
(defun spam-summary-prepare-exit ()
  (unless gnus-group-is-exiting-without-update-p
    (gnus-message 6 "Exiting summary buffer and applying spam rules")
    (when (and spam-bogofilter-path
	       (spam-group-spam-processor-bogofilter-p gnus-newsgroup-name))
      (gnus-message 5 "Registering spam with bogofilter")
      (spam-bogofilter-register-spam-routine))
  
    (when (and spam-ifile-path
	       (spam-group-spam-processor-ifile-p gnus-newsgroup-name))
      (gnus-message 5 "Registering spam with ifile")
      (spam-ifile-register-spam-routine))
  
    (when (spam-group-spam-processor-spamoracle-p gnus-newsgroup-name)
      (gnus-message 5 "Registering spam with spamoracle")
      (spam-spamoracle-learn-spam))

    (when (spam-group-spam-processor-stat-p gnus-newsgroup-name)
      (gnus-message 5 "Registering spam with spam-stat")
      (spam-stat-register-spam-routine))

    (when (spam-group-spam-processor-blacklist-p gnus-newsgroup-name)
      (gnus-message 5 "Registering spam with the blacklist")
      (spam-blacklist-register-routine))

    (when (spam-group-spam-processor-report-gmane-p gnus-newsgroup-name)
      (gnus-message 5 "Registering spam with the Gmane report")
      (spam-report-gmane-register-routine))

    (if spam-move-spam-nonspam-groups-only      
	(when (not (spam-group-spam-contents-p gnus-newsgroup-name))
	  (spam-mark-spam-as-expired-and-move-routine
	   (gnus-parameter-spam-process-destination gnus-newsgroup-name)))
      (gnus-message 5 "Marking spam as expired and moving it to %s" gnus-newsgroup-name)
      (spam-mark-spam-as-expired-and-move-routine 
       (gnus-parameter-spam-process-destination gnus-newsgroup-name)))

    ;; now we redo spam-mark-spam-as-expired-and-move-routine to only
    ;; expire spam, in case the above did not expire them
    (gnus-message 5 "Marking spam as expired without moving it")
    (spam-mark-spam-as-expired-and-move-routine nil)

    (when (or (spam-group-ham-contents-p gnus-newsgroup-name)
	      (and (spam-group-spam-contents-p gnus-newsgroup-name)
		   spam-process-ham-in-spam-groups)
	      spam-process-ham-in-nonham-groups)
      (when (spam-group-ham-processor-whitelist-p gnus-newsgroup-name)
	(gnus-message 5 "Registering ham with the whitelist")
	(spam-whitelist-register-routine))
      (when (spam-group-ham-processor-ifile-p gnus-newsgroup-name)
	(gnus-message 5 "Registering ham with ifile")
	(spam-ifile-register-ham-routine))
      (when (spam-group-ham-processor-bogofilter-p gnus-newsgroup-name)
	(gnus-message 5 "Registering ham with Bogofilter")
	(spam-bogofilter-register-ham-routine))
      (when (spam-group-ham-processor-stat-p gnus-newsgroup-name)
	(gnus-message 5 "Registering ham with spam-stat")
	(spam-stat-register-ham-routine))
      (when (spam-group-ham-processor-BBDB-p gnus-newsgroup-name)
	(gnus-message 5 "Registering ham with the BBDB")
	(spam-BBDB-register-routine))
      (when (spam-group-ham-processor-spamoracle-p gnus-newsgroup-name)
	(gnus-message 5 "Registering ham with spamoracle")
	(spam-spamoracle-learn-ham)))

    (when (spam-group-ham-processor-copy-p gnus-newsgroup-name)
      (gnus-message 5 "Copying ham")
      (spam-ham-copy-routine
       (gnus-parameter-ham-process-destination gnus-newsgroup-name)))

    ;; now move all ham articles out of spam groups
    (when (spam-group-spam-contents-p gnus-newsgroup-name)
      (gnus-message 5 "Moving ham messages from spam group")
      (spam-ham-move-routine
       (gnus-parameter-ham-process-destination gnus-newsgroup-name)))))

(defun spam-mark-junk-as-spam-routine ()
  ;; check the global list of group names spam-junk-mailgroups and the
  ;; group parameters
  (when (spam-group-spam-contents-p gnus-newsgroup-name)
    (gnus-message 5 "Marking %s articles as spam"
		  (if spam-mark-only-unseen-as-spam 
		      "unseen"
		    "unread"))
    (let ((articles (if spam-mark-only-unseen-as-spam 
			gnus-newsgroup-unseen
		      gnus-newsgroup-unreads)))
      (dolist (article articles)
	(gnus-summary-mark-article article gnus-spam-mark)))))

(defun spam-mark-spam-as-expired-and-move-routine (&rest groups)
  (gnus-summary-kill-process-mark)
  (let ((articles gnus-newsgroup-articles)
	article tomove deletep)
    (dolist (article articles)
      (when (eq (gnus-summary-article-mark article) gnus-spam-mark)
	(gnus-summary-mark-article article gnus-expirable-mark)
	(push article tomove)))
    
    ;; now do the actual copies
    (dolist (group groups)
      (when (and tomove
		 (stringp group))
	(dolist (article tomove)
	  (gnus-summary-set-process-mark article))
	(when tomove
	  (if (> (length groups) 1)
	      (progn 
		(gnus-summary-copy-article nil group)
		(setq deletep t))
	    (gnus-summary-move-article nil group)))))
    
    ;; now delete the articles, if there was a copy done
    (when deletep
      (dolist (article tomove)
	(gnus-summary-set-process-mark article))
      (when tomove
	(let ((gnus-novice-user nil))	; don't ask me if I'm sure
	  (gnus-summary-delete-article nil))))
    
    (gnus-summary-yank-process-mark)))
 
(defun spam-ham-copy-or-move-routine (copy groups)
  (gnus-summary-kill-process-mark)
  (let ((articles gnus-newsgroup-articles)
	article mark todo deletep)
    (dolist (article articles)
      (when (spam-group-ham-mark-p gnus-newsgroup-name
				   (gnus-summary-article-mark article))
	(push article todo)))

    ;; now do the actual move
    (dolist (group groups)
      (when (and todo (stringp group))
	(dolist (article todo)
	  (when spam-mark-ham-unread-before-move-from-spam-group
	    (gnus-summary-mark-article article gnus-unread-mark))
	  (gnus-summary-set-process-mark article))

	(if (> (length groups) 1)
	    (progn 
	      (gnus-summary-copy-article nil group)
	      (setq deletep t))
	  (gnus-summary-move-article nil group))))
  
    ;; now delete the articles, unless a) copy is t, and when there was a copy done
    ;;                                 b) a move was done to a single group
    (unless copy
      (when deletep
	(dolist (article todo)
	  (gnus-summary-set-process-mark article))
	(when todo
	  (let ((gnus-novice-user nil))	; don't ask me if I'm sure
	    (gnus-summary-delete-article nil))))))
  
  (gnus-summary-yank-process-mark))
 
(defun spam-ham-copy-routine (&rest groups)
  (spam-ham-copy-or-move-routine t groups))
 
(defun spam-ham-move-routine (&rest groups)
  (spam-ham-copy-or-move-routine nil groups))
 
(defun spam-generic-register-routine (spam-func ham-func)
  (let ((articles gnus-newsgroup-articles)
	article mark ham-articles spam-articles)

    (while articles
      (setq article (pop articles)
	    mark (gnus-summary-article-mark article))
      (cond ((spam-group-spam-mark-p gnus-newsgroup-name mark) 
	     (push article spam-articles))
	    ((memq article gnus-newsgroup-saved))
	    ((spam-group-ham-mark-p gnus-newsgroup-name mark)
	     (push article ham-articles))))

    (when (and ham-articles ham-func)
      (mapc ham-func ham-articles))	; we use mapc because unlike
					; mapcar it discards the
					; return values
    (when (and spam-articles spam-func)
      (mapc spam-func spam-articles))))	; we use mapc because unlike
					; mapcar it discards the
					; return values

(eval-and-compile
  (defalias 'spam-point-at-eol (if (fboundp 'point-at-eol)
				   'point-at-eol
				 'line-end-position)))

(defun spam-get-article-as-string (article)
  (let ((article-buffer (spam-get-article-as-buffer article))
			article-string)
    (when article-buffer
      (save-window-excursion
	(set-buffer article-buffer)
	(setq article-string (buffer-string))))
  article-string))

(defun spam-get-article-as-buffer (article)
  (let ((article-buffer))
    (when (numberp article)
      (save-window-excursion
	(gnus-summary-goto-subject article)
	(gnus-summary-show-article t)
	(setq article-buffer (get-buffer gnus-article-buffer))))
    article-buffer))

;; disabled for now
;; (defun spam-get-article-as-filename (article)
;;   (let ((article-filename))
;;     (when (numberp article)
;;       (nnml-possibly-change-directory (gnus-group-real-name gnus-newsgroup-name))
;;       (setq article-filename (expand-file-name (int-to-string article) nnml-current-directory)))
;;     (if (file-exists-p article-filename)
;; 	article-filename
;;       nil)))

(defun spam-fetch-field-from-fast (article)
  "Fetch the `from' field quickly, using the internal gnus-data-list function"
  (if (and (numberp article)
	   (assoc article (gnus-data-list nil)))
      (mail-header-from (gnus-data-header (assoc article (gnus-data-list nil))))
    nil))

(defun spam-fetch-field-subject-fast (article)
  "Fetch the `subject' field quickly, using the internal gnus-data-list function"
  (if (and (numberp article)
	   (assoc article (gnus-data-list nil)))
      (mail-header-subject (gnus-data-header (assoc article (gnus-data-list nil))))
    nil))


;;;; Spam determination.

(defvar spam-list-of-checks
  '((spam-use-blacklist  		. 	spam-check-blacklist)
    (spam-use-regex-headers  		. 	spam-check-regex-headers)
    (spam-use-regex-body  		. 	spam-check-regex-body)
    (spam-use-whitelist  		. 	spam-check-whitelist)
    (spam-use-BBDB	 		. 	spam-check-BBDB)
    (spam-use-ifile	 		. 	spam-check-ifile)
    (spam-use-spamoracle                .       spam-check-spamoracle)
    (spam-use-stat	 		. 	spam-check-stat)
    (spam-use-blackholes 		. 	spam-check-blackholes)
    (spam-use-hashcash  		. 	spam-check-hashcash)
    (spam-use-bogofilter-headers 	. 	spam-check-bogofilter-headers)
    (spam-use-bogofilter 		. 	spam-check-bogofilter))
"The spam-list-of-checks list contains pairs associating a parameter
variable with a spam checking function.  If the parameter variable is
true, then the checking function is called, and its value decides what
happens.  Each individual check may return nil, t, or a mailgroup
name.  The value nil means that the check does not yield a decision,
and so, that further checks are needed.  The value t means that the
message is definitely not spam, and that further spam checks should be
inhibited.  Otherwise, a mailgroup name is returned where the mail
should go, and further checks are also inhibited.  The usual mailgroup
name is the value of `spam-split-group', meaning that the message is
definitely a spam.")

(defvar spam-list-of-statistical-checks
  '(spam-use-ifile spam-use-regex-body spam-use-stat spam-use-bogofilter spam-use-spamoracle)
"The spam-list-of-statistical-checks list contains all the mail
splitters that need to have the full message body available.")

;;;TODO: modify to invoke self with each specific check if invoked without specific checks
(defun spam-split (&rest specific-checks)
  "Split this message into the `spam' group if it is spam.
This function can be used as an entry in `nnmail-split-fancy', for
example like this: (: spam-split).  It can take checks as parameters.

See the Info node `(gnus)Fancy Mail Splitting' for more details."
  (interactive)
  (save-excursion
    (save-restriction
      (dolist (check spam-list-of-statistical-checks)
	(when (symbol-value check)
	  (widen)
	  (gnus-message 8 "spam-split: widening the buffer (%s requires it)"
			(symbol-name check))
	  (return)))
      ;;   (progn (widen) (debug (buffer-string)))
      (let ((list-of-checks spam-list-of-checks)
	    decision)
	(while (and list-of-checks (not decision))
	  (let ((pair (pop list-of-checks)))
	    (when (and (symbol-value (car pair))
		       (or (null specific-checks)
			   (memq (car pair) specific-checks)))
	      (gnus-message 5 "spam-split: calling the %s function" (symbol-name (cdr pair)))
	      (setq decision (funcall (cdr pair))))))
	(if (eq decision t)
	    nil
	  decision)))))
  
(defun spam-setup-widening ()
  (dolist (check spam-list-of-statistical-checks)
    (when (symbol-value check)
      (setq nnimap-split-download-body-default t))))


;;;; Regex body

(defun spam-check-regex-body ()
  (let ((spam-regex-headers-ham spam-regex-body-ham)
	(spam-regex-headers-spam spam-regex-body-spam))
    (spam-check-regex-headers t)))


;;;; Regex headers

(defun spam-check-regex-headers (&optional body)
  (let ((type (if body "body" "header"))
	 ret found)
    (dolist (h-regex spam-regex-headers-ham)
      (unless found
	(goto-char (point-min))
	(when (re-search-forward h-regex nil t)
	  (message "Ham regex %s search positive." type)
	  (setq found t))))
    (dolist (s-regex spam-regex-headers-spam)
      (unless found
	(goto-char (point-min))
	(when (re-search-forward s-regex nil t)
	  (message "Spam regex %s search positive." type)
	  (setq found t)
	  (setq ret spam-split-group))))
    ret))


;;;; Blackholes.

(defun spam-reverse-ip-string (ip)
  (when (stringp ip)
    (mapconcat 'identity
	       (nreverse (split-string ip "\\."))
	       ".")))

(defun spam-check-blackholes ()
  "Check the Received headers for blackholed relays."
  (let ((headers (nnmail-fetch-field "received"))
	ips matches)
    (when headers
      (with-temp-buffer
	(insert headers)
	(goto-char (point-min))
	(gnus-message 5 "Checking headers for relay addresses")
	(while (re-search-forward
		"\\([0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+\\)" nil t)
	  (gnus-message 9 "Blackhole search found host IP %s." (match-string 1))
	  (push (spam-reverse-ip-string (match-string 1))
		ips)))
      (dolist (server spam-blackhole-servers)
	(dolist (ip ips)
	  (unless (and spam-blackhole-good-server-regex
		       ;; match the good-server-regex against the reversed (again) IP string
		       (string-match 
			spam-blackhole-good-server-regex
			(spam-reverse-ip-string ip)))
	    (unless matches
	      (let ((query-string (concat ip "." server)))
		(if spam-use-dig
		    (let ((query-result (query-dig query-string)))
		      (when query-result
			(gnus-message 5 "(DIG): positive blackhole check '%s'" 
				      query-result)
			(push (list ip server query-result)
			      matches)))
		  ;; else, if not using dig.el
		  (when (query-dns query-string)
		    (gnus-message 5 "positive blackhole check")
		    (push (list ip server (query-dns query-string 'TXT))
			  matches)))))))))
    (when matches
      spam-split-group)))

;;;; Hashcash.

(condition-case nil
    (progn
      (require 'hashcash)
      
      (defun spam-check-hashcash ()
	"Check the headers for hashcash payments."
	(mail-check-payment)))		;mail-check-payment returns a boolean

  (file-error (progn
		(defalias 'mail-check-payment 'ignore)
		(defalias 'spam-check-hashcash 'ignore))))

;;;; BBDB 

;;; original idea for spam-check-BBDB from Alexander Kotelnikov
;;; <sacha@giotto.sj.ru>

;; all this is done inside a condition-case to trap errors

(condition-case nil
    (progn
      (require 'bbdb)
      (require 'bbdb-com)
      
  (defun spam-enter-ham-BBDB (from)
    "Enter an address into the BBDB; implies ham (non-spam) sender"
    (when (stringp from)
      (let* ((parsed-address (gnus-extract-address-components from))
	     (name (or (car parsed-address) "Ham Sender"))
	     (net-address (car (cdr parsed-address))))
	(gnus-message 5 "Adding address %s to BBDB" from)
	(when (and net-address
		   (not (bbdb-search-simple nil net-address)))
	  (bbdb-create-internal name nil net-address nil nil 
				"ham sender added by spam.el")))))

  (defun spam-BBDB-register-routine ()
    (spam-generic-register-routine 
     ;; spam function
     nil
     ;; ham function
     (lambda (article)
       (spam-enter-ham-BBDB (spam-fetch-field-from-fast article)))))

  (defun spam-check-BBDB ()
    "Mail from people in the BBDB is classified as ham or non-spam"
    (let ((who (nnmail-fetch-field "from")))
      (when who
	(setq who (cadr (gnus-extract-address-components who)))
	(if (bbdb-search-simple nil who)
	    t 
	  (if spam-use-BBDB-exclusive
	      spam-split-group
	    nil))))))

  (file-error (progn
		(defalias 'bbdb-search-simple 'ignore)
		(defalias 'spam-check-BBDB 'ignore)
		(defalias 'spam-BBDB-register-routine 'ignore)
		(defalias 'spam-enter-ham-BBDB 'ignore)
		(defalias 'bbdb-create-internal 'ignore)
		(defalias 'bbdb-records 'ignore))))


;;;; ifile

;;; check the ifile backend; return nil if the mail was NOT classified
;;; as spam

(defun spam-get-ifile-database-parameter ()
  "Get the command-line parameter for ifile's database from spam-ifile-database-path."
  (if spam-ifile-database-path
      (format "--db-file=%s" spam-ifile-database-path)
    nil))
    
(defun spam-check-ifile ()
  "Check the ifile backend for the classification of this message"
  (let ((article-buffer-name (buffer-name)) 
	category return)
    (with-temp-buffer
      (let ((temp-buffer-name (buffer-name))
	    (db-param (spam-get-ifile-database-parameter)))
	(save-excursion
	  (set-buffer article-buffer-name)
	  (if db-param
	      (call-process-region (point-min) (point-max) spam-ifile-path
				   nil temp-buffer-name nil "-q" "-c" db-param)
	    (call-process-region (point-min) (point-max) spam-ifile-path
				 nil temp-buffer-name nil "-q" "-c")))
	(goto-char (point-min))
	(if (not (eobp))
	    (setq category (buffer-substring (point) (spam-point-at-eol))))
	(when (not (zerop (length category))) ; we need a category here
	  (if spam-ifile-all-categories
	      (setq return category)
	    ;; else, if spam-ifile-all-categories is not set...
	    (when (string-equal spam-ifile-spam-category category)
	      (setq return spam-split-group))))))
    return))

(defun spam-ifile-register-with-ifile (article-string category)
  "Register an article, given as a string, with a category.
Uses `gnus-newsgroup-name' if category is nil (for ham registration)."
  (when (stringp article-string)
    (let ((category (or category gnus-newsgroup-name))
          (db-param (spam-get-ifile-database-parameter)))
      (with-temp-buffer
	(insert article-string)
	(if db-param
            (call-process-region (point-min) (point-max) spam-ifile-path 
                                 nil nil nil 
                                 "-h" "-i" category db-param)
          (call-process-region (point-min) (point-max) spam-ifile-path 
                               nil nil nil 
                               "-h" "-i" category))))))

(defun spam-ifile-register-spam-routine ()
  (spam-generic-register-routine 
   (lambda (article)
     (spam-ifile-register-with-ifile 
      (spam-get-article-as-string article) spam-ifile-spam-category))
   nil))

(defun spam-ifile-register-ham-routine ()
  (spam-generic-register-routine 
   nil
   (lambda (article)
     (spam-ifile-register-with-ifile 
      (spam-get-article-as-string article) spam-ifile-ham-category))))


;;;; spam-stat

(condition-case nil
    (progn
      (let ((spam-stat-install-hooks nil))
	(require 'spam-stat))
      
      (defun spam-check-stat ()
	"Check the spam-stat backend for the classification of this message"
	(let ((spam-stat-split-fancy-spam-group spam-split-group) ; override
	      (spam-stat-buffer (buffer-name)) ; stat the current buffer
	      category return)
	  (spam-stat-split-fancy)))

      (defun spam-stat-register-spam-routine ()
	(spam-generic-register-routine 
	 (lambda (article)
	   (let ((article-string (spam-get-article-as-string article)))
	     (with-temp-buffer
	       (insert article-string)
	       (spam-stat-buffer-is-spam))))
	 nil))

      (defun spam-stat-register-ham-routine ()
	(spam-generic-register-routine 
	 nil
	 (lambda (article)
	   (let ((article-string (spam-get-article-as-string article)))
	     (with-temp-buffer
	       (insert article-string)
	       (spam-stat-buffer-is-non-spam))))))

      (defun spam-maybe-spam-stat-load ()
	(when spam-use-stat (spam-stat-load)))
      
      (defun spam-maybe-spam-stat-save ()
	(when spam-use-stat (spam-stat-save))))

  (file-error (progn
		(defalias 'spam-maybe-spam-stat-load 'ignore)
		(defalias 'spam-maybe-spam-stat-save 'ignore)
		(defalias 'spam-stat-register-ham-routine 'ignore)
		(defalias 'spam-stat-register-spam-routine 'ignore)
		(defalias 'spam-stat-buffer-is-spam 'ignore)
		(defalias 'spam-stat-buffer-is-non-spam 'ignore)
		(defalias 'spam-stat-split-fancy 'ignore)
		(defalias 'spam-stat-load 'ignore)
		(defalias 'spam-stat-save 'ignore)
		(defalias 'spam-check-stat 'ignore))))



;;;; Blacklists and whitelists.

(defvar spam-whitelist-cache nil)
(defvar spam-blacklist-cache nil)

(defun spam-enter-whitelist (address)
  "Enter ADDRESS into the whitelist."
  (interactive "sAddress: ")
  (spam-enter-list address spam-whitelist)
  (setq spam-whitelist-cache nil))

(defun spam-enter-blacklist (address)
  "Enter ADDRESS into the blacklist."
  (interactive "sAddress: ")
  (spam-enter-list address spam-blacklist)
  (setq spam-blacklist-cache nil))

(defun spam-enter-list (address file)
  "Enter ADDRESS into the given FILE, either the whitelist or the blacklist."
  (unless (file-exists-p (file-name-directory file))
    (make-directory (file-name-directory file) t))
  (save-excursion
    (set-buffer
     (find-file-noselect file))
    (goto-char (point-min))
    (unless (re-search-forward (regexp-quote address) nil t)
      (goto-char (point-max))
      (unless (bobp)
	(insert "\n"))
      (insert address "\n")
      (save-buffer))))

;;; returns t if the sender is in the whitelist, nil or spam-split-group otherwise
(defun spam-check-whitelist ()
  ;; FIXME!  Should it detect when file timestamps change?
  (unless spam-whitelist-cache
    (setq spam-whitelist-cache (spam-parse-list spam-whitelist)))
  (if (spam-from-listed-p spam-whitelist-cache) 
      t
    (if spam-use-whitelist-exclusive
	spam-split-group
      nil)))

(defun spam-check-blacklist ()
  ;; FIXME!  Should it detect when file timestamps change?
  (unless spam-blacklist-cache
    (setq spam-blacklist-cache (spam-parse-list spam-blacklist)))
  (and (spam-from-listed-p spam-blacklist-cache) spam-split-group))

(defun spam-parse-list (file)
  (when (file-readable-p file)
    (let (contents address)
      (with-temp-buffer
	(insert-file-contents file)
	(while (not (eobp))
	  (setq address (buffer-substring (point) (spam-point-at-eol)))
	  (forward-line 1)
	  ;; insert the e-mail address if detected, otherwise the raw data
	  (unless (zerop (length address))
	    (let ((pure-address (cadr (gnus-extract-address-components address))))
	      (push (or pure-address address) contents)))))
      (nreverse contents))))

(defun spam-from-listed-p (cache)
  (let ((from (nnmail-fetch-field "from"))
	found)
    (while cache
      (let ((address (pop cache)))
	(unless (zerop (length address)) ; 0 for a nil address too
	  (setq address (regexp-quote address))
	  ;; fix regexp-quote's treatment of user-intended regexes
	  (while (string-match "\\\\\\*" address)
	    (setq address (replace-match ".*" t t address))))
	(when (and address (string-match address from))
	  (setq found t
		cache nil))))
    found))

(defun spam-blacklist-register-routine ()
  (spam-generic-register-routine 
   ;; the spam function
   (lambda (article)
     (let ((from (spam-fetch-field-from-fast article)))
       (when (stringp from)
	   (spam-enter-blacklist from))))
   ;; the ham function
   nil))

(defun spam-whitelist-register-routine ()
  (spam-generic-register-routine 
   ;; the spam function
   nil 
   ;; the ham function
   (lambda (article)
     (let ((from (spam-fetch-field-from-fast article)))
       (when (stringp from)
	   (spam-enter-whitelist from))))))


;;;; Spam-report glue
(defun spam-report-gmane-register-routine ()
  (spam-generic-register-routine
   'spam-report-gmane
   nil))


;;;; Bogofilter
(defun spam-check-bogofilter-headers (&optional score)
  (let ((header (nnmail-fetch-field spam-bogofilter-header)))
    (when header			; return nil when no header
      (if score				; scoring mode
	  (if (string-match "spamicity=\\([0-9.]+\\)" header)
	      (match-string 1 header)
	    "0")
	;; spam detection mode
	(when (string-match spam-bogofilter-bogosity-positive-spam-header
			    header)
	  spam-split-group)))))

;; return something sensible if the score can't be determined
(defun spam-bogofilter-score ()
  "Get the Bogofilter spamicity score"
  (interactive)
  (save-window-excursion
    (gnus-summary-show-article t)
    (set-buffer gnus-article-buffer)
    (let ((score (or (spam-check-bogofilter-headers t)
		     (spam-check-bogofilter t))))
      (message "Spamicity score %s" score)
      (or score "0"))
    (gnus-summary-show-article)))

(defun spam-check-bogofilter (&optional score)
  "Check the Bogofilter backend for the classification of this message"
  (let ((article-buffer-name (buffer-name)) 
	return)
    (with-temp-buffer
      (let ((temp-buffer-name (buffer-name)))
	(save-excursion
	  (set-buffer article-buffer-name)
	  (if spam-bogofilter-database-directory
	      (call-process-region (point-min) (point-max) 
				   spam-bogofilter-path
				   nil temp-buffer-name nil "-v"
				   "-d" spam-bogofilter-database-directory)
	    (call-process-region (point-min) (point-max) spam-bogofilter-path
				 nil temp-buffer-name nil "-v")))
	(setq return (spam-check-bogofilter-headers score))))
    return))

(defun spam-bogofilter-register-with-bogofilter (article-string spam)
  "Register an article, given as a string, as spam or non-spam."
  (when (stringp article-string)
    (let ((switch (if spam spam-bogofilter-spam-switch 
		    spam-bogofilter-ham-switch)))
      (with-temp-buffer
	(insert article-string)
	(if spam-bogofilter-database-directory
	    (call-process-region (point-min) (point-max) 
				 spam-bogofilter-path
				 nil nil nil "-v" switch
				 "-d" spam-bogofilter-database-directory)
	  (call-process-region (point-min) (point-max) spam-bogofilter-path
			       nil nil nil "-v" switch))))))

(defun spam-bogofilter-register-spam-routine ()
  (spam-generic-register-routine 
   (lambda (article)
     (spam-bogofilter-register-with-bogofilter
      (spam-get-article-as-string article) t))
   nil))

(defun spam-bogofilter-register-ham-routine ()
  (spam-generic-register-routine 
   nil
   (lambda (article)
     (spam-bogofilter-register-with-bogofilter
      (spam-get-article-as-string article) nil))))


;;;; spamoracle
(defun spam-check-spamoracle ()
  "Run spamoracle on an article to determine whether it's spam."
  (let ((article-buffer-name (buffer-name)))
    (with-temp-buffer
      (let ((temp-buffer-name (buffer-name)))
	(save-excursion
	  (set-buffer article-buffer-name)
	  (let ((status 
		 (apply 'call-process-region 
			(point-min) (point-max)
			spam-spamoracle-binary 
			nil temp-buffer-name nil
			(if spam-spamoracle-database
			    `("-f" ,spam-spamoracle-database "mark")
			  '("mark")))))
	    (if (zerop status)
		(progn
		  (set-buffer temp-buffer-name)
		  (goto-char (point-min))
		  (when (re-search-forward "^X-Spam: yes;" nil t)
		    spam-split-group))
	      (error "Error running spamoracle" status))))))))

(defun spam-spamoracle-learn (article article-is-spam-p)
  "Run spamoracle in training mode."
  (with-temp-buffer
    (let ((temp-buffer-name (buffer-name)))
      (save-excursion
	(goto-char (point-min))
	(insert (spam-get-article-as-string article))
	(let* ((arg (if article-is-spam-p "-spam" "-good"))
	       (status 
		(apply 'call-process-region
		       (point-min) (point-max)
		       spam-spamoracle-binary
		       nil temp-buffer-name nil
		       (if spam-spamoracle-database
			   `("-f" ,spam-spamoracle-database 
			     "add" ,arg)
			 `("add" ,arg)))))
	  (when (not (zerop status))
	    (error "Error running spamoracle" status)))))))
  
(defun spam-spamoracle-learn-ham ()
  (spam-generic-register-routine 
   nil
   (lambda (article)
     (spam-spamoracle-learn article nil))))

(defun spam-spamoracle-learn-spam ()
  (spam-generic-register-routine 
   (lambda (article)
     (spam-spamoracle-learn article t))
   nil))

;;;; Hooks

;;;###autoload
(defun spam-initialize ()
  "Install the spam.el hooks and do other initialization"
  (interactive)
  (setq spam-install-hooks t)
  ;; TODO: How do we redo this every time spam-face is customized?
  (push '((eq mark gnus-spam-mark) . spam-face)
	gnus-summary-highlight)
  ;; Add hooks for loading and saving the spam stats
  (when spam-use-stat
    (add-hook 'gnus-save-newsrc-hook 'spam-maybe-spam-stat-save)
    (add-hook 'gnus-get-top-new-news-hook 'spam-maybe-spam-stat-load)
    (add-hook 'gnus-startup-hook 'spam-maybe-spam-stat-load))
  (add-hook 'gnus-summary-prepare-exit-hook 'spam-summary-prepare-exit)
  (add-hook 'gnus-summary-prepare-hook 'spam-summary-prepare)
  (add-hook 'gnus-get-new-news-hook 'spam-setup-widening))

(defun spam-unload-hook ()
  "Uninstall the spam.el hooks"
  (interactive)
  (remove-hook 'gnus-save-newsrc-hook 'spam-maybe-spam-stat-save)
  (remove-hook 'gnus-get-top-new-news-hook 'spam-maybe-spam-stat-load)
  (remove-hook 'gnus-startup-hook 'spam-maybe-spam-stat-load)
  (remove-hook 'gnus-summary-prepare-exit-hook 'spam-summary-prepare-exit)
  (remove-hook 'gnus-summary-prepare-hook 'spam-summary-prepare)
  (remove-hook 'gnus-get-new-news-hook 'spam-setup-widening))

(when spam-install-hooks
  (spam-initialize))

(provide 'spam)

;;; spam.el ends here.
