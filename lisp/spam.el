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

(require 'gnus-sum)

(require 'gnus-uu)			; because of key prefix issues
(require 'gnus)	; for the definitions of group content classification and spam processors
(require 'message)			;for the message-fetch-field functions

;; autoload executable-find
(eval-and-compile
  ;; executable-find is not autoloaded in Emacs 20
  (autoload 'executable-find "executable"))

;; autoload query-dig
(eval-and-compile
  (autoload 'query-dig "dig"))

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
When nil, only ham and unclassified groups will have their spam moved
to the spam-process-destination.  When t, spam will also be moved from
spam groups."
  :type 'boolean
  :group 'spam-ifile)

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

(defcustom spam-use-regex-headers nil
  "Whether a header regular expression match should be used by spam-split.
Also see the variable `spam-spam-regex-headers' and `spam-ham-regex-headers'."
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

(defcustom spam-split-group "spam"
  "Group name where incoming spam should be put by spam-split."
  :type 'string
  :group 'spam)

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
  :type 'regexp
  :group 'spam)

(defcustom spam-ham-marks (list 'gnus-del-mark 'gnus-read-mark 
				'gnus-killed-mark 'gnus-kill-file-mark 
				'gnus-low-score-mark)
  "Marks considered as being ham (positively not spam).
Such articles will be processed as ham (non-spam) on group exit."
  :type '(set
	  (variable-item gnus-del-mark)
	  (variable-item gnus-read-mark)
	  (variable-item gnus-killed-mark)
	  (variable-item gnus-kill-file-mark)
	  (variable-item gnus-low-score-mark))
  :group 'spam)

(defcustom spam-spam-marks (list 'gnus-spam-mark)
  "Marks considered as being spam (positively spam).
Such articles will be transmitted to `bogofilter -s' on group exit."
  :type '(set 
	  (variable-item gnus-spam-mark)
	  (variable-item gnus-killed-mark)
	  (variable-item gnus-kill-file-mark)
	  (variable-item gnus-low-score-mark))
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

(defcustom spam-bogofilter-bogosity-positive-spam-header "^\\(Yes\\|Spam\\)"
  "The regex on `spam-bogofilter-header' for positive spam identification."
  :type 'regexp
  :group 'spam-bogofilter)

(defcustom spam-bogofilter-database-directory nil
  "Directory path of the Bogofilter databases."
  :type '(choice (directory :tag "Location of the Bogofilter database directory")
		 (const :tag "Use the default"))
  :group 'spam-ifile)

;;; Key bindings for spam control.

(gnus-define-keys gnus-summary-mode-map
  "St" spam-bogofilter-score
  "Sx" gnus-summary-mark-as-spam
  "Mst" spam-bogofilter-score
  "Msx" gnus-summary-mark-as-spam
  "\M-d" gnus-summary-mark-as-spam)

;;; How to highlight a spam summary line.

;; TODO: How do we redo this every time spam-face is customized?

(push '((eq mark gnus-spam-mark) . spam-face)
      gnus-summary-highlight)

;; convenience functions
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

(defun spam-group-spam-processor-bogofilter-p (group)
  (spam-group-processor-p group 'gnus-group-spam-exit-processor-bogofilter))

(defun spam-group-spam-processor-blacklist-p (group)
  (spam-group-processor-p group 'gnus-group-spam-exit-processor-blacklist))

(defun spam-group-spam-processor-ifile-p (group)
  (spam-group-processor-p group 'gnus-group-spam-exit-processor-ifile))

(defun spam-group-ham-processor-ifile-p (group)
  (spam-group-processor-p group 'gnus-group-ham-exit-processor-ifile))

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

;;; Summary entry and exit processing.

(defun spam-summary-prepare ()
  (spam-mark-junk-as-spam-routine))

(add-hook 'gnus-summary-prepare-hook 'spam-summary-prepare)

(defun spam-summary-prepare-exit ()
  ;; The spam processors are invoked for any group, spam or ham or neither
  (gnus-message 6 "Exiting summary buffer and applying spam rules")
  (when (and spam-bogofilter-path
	     (spam-group-spam-processor-bogofilter-p gnus-newsgroup-name))
    (gnus-message 5 "Registering spam with bogofilter")
    (spam-bogofilter-register-spam-routine))
  
  (when (and spam-ifile-path
	     (spam-group-spam-processor-ifile-p gnus-newsgroup-name))
    (gnus-message 5 "Registering spam with ifile")
    (spam-ifile-register-spam-routine))
  
  (when (spam-group-spam-processor-stat-p gnus-newsgroup-name)
    (gnus-message 5 "Registering spam with spam-stat")
    (spam-stat-register-spam-routine))

  (when (spam-group-spam-processor-blacklist-p gnus-newsgroup-name)
    (gnus-message 5 "Registering spam with the blacklist")
    (spam-blacklist-register-routine))

  (if spam-move-spam-nonspam-groups-only      
      (when (not (spam-group-spam-contents-p gnus-newsgroup-name))
	(spam-mark-spam-as-expired-and-move-routine
	 (gnus-parameter-spam-process-destination gnus-newsgroup-name)))
    (gnus-message 5 "Marking spam as expired and moving it")
    (spam-mark-spam-as-expired-and-move-routine 
     (gnus-parameter-spam-process-destination gnus-newsgroup-name)))

  ;; now we redo spam-mark-spam-as-expired-and-move-routine to only
  ;; expire spam, in case the above did not expire them
  (spam-mark-spam-as-expired-and-move-routine nil)

  (when (spam-group-ham-contents-p gnus-newsgroup-name)
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
      (spam-BBDB-register-routine)))

  ;; now move all ham articles out of spam groups
  (when (spam-group-spam-contents-p gnus-newsgroup-name)
    (gnus-message 5 "Moving ham messages from spam group")
    (spam-ham-move-routine
     (gnus-parameter-ham-process-destination gnus-newsgroup-name))))

(add-hook 'gnus-summary-prepare-exit-hook 'spam-summary-prepare-exit)

(defun spam-mark-junk-as-spam-routine ()
  ;; check the global list of group names spam-junk-mailgroups and the
  ;; group parameters
  (when (spam-group-spam-contents-p gnus-newsgroup-name)
    (gnus-message 5 "Marking unread articles as spam")
    (let ((articles gnus-newsgroup-articles)
	  article)
      (while articles
	(setq article (pop articles))
	(when (eq (gnus-summary-article-mark article) gnus-unread-mark)
	  (gnus-summary-mark-article article gnus-spam-mark))))))

(defun spam-mark-spam-as-expired-and-move-routine (&optional group)
  (let ((articles gnus-newsgroup-articles)
	article)
    (while articles
      (setq article (pop articles))
      (when (eq (gnus-summary-article-mark article) gnus-spam-mark)
	(gnus-summary-mark-article article gnus-expirable-mark)
	(when (stringp group)
	  (let ((gnus-current-article article))
	    (gnus-summary-move-article nil group)))))))
 
(defun spam-ham-move-routine (&optional group)
  (let ((articles gnus-newsgroup-articles)
	article ham-mark-values mark)

    (dolist (mark spam-ham-marks)
      (push (symbol-value mark) ham-mark-values))
    
    (dolist (article articles)
      (when (and (memq (gnus-summary-article-mark article) ham-mark-values)
		 (stringp group))
	(let ((gnus-current-article article))
	  (gnus-summary-move-article nil group))))))
 
(defun spam-generic-register-routine (spam-func ham-func)
  (let ((articles gnus-newsgroup-articles)
	article mark ham-articles spam-articles spam-mark-values 
	ham-mark-values)

    ;; marks are stored as symbolic values, so we have to dereference
    ;; them for memq to work.  we wouldn't have to do this if
    ;; gnus-summary-article-mark returned a symbol.
    (dolist (mark spam-ham-marks)
      (push (symbol-value mark) ham-mark-values))

    (dolist (mark spam-spam-marks)
      (push (symbol-value mark) spam-mark-values))

    (while articles
      (setq article (pop articles)
	    mark (gnus-summary-article-mark article))
      (cond ((memq mark spam-mark-values) (push article spam-articles))
	    ((memq article gnus-newsgroup-saved))
	    ((memq mark ham-mark-values) (push article ham-articles))))
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

(defun spam-get-article-as-filename (article)
  (let ((article-filename))
    (when (numberp article)
      (nnml-possibly-change-directory (gnus-group-real-name gnus-newsgroup-name))
      (setq article-filename (expand-file-name (int-to-string article) nnml-current-directory)))
    (if (file-exists-p article-filename)
	article-filename
      nil)))

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
    (spam-use-whitelist  		. 	spam-check-whitelist)
    (spam-use-BBDB	 		. 	spam-check-BBDB)
    (spam-use-ifile	 		. 	spam-check-ifile)
    (spam-use-stat	 		. 	spam-check-stat)
    (spam-use-blackholes 		. 	spam-check-blackholes)
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

(defun spam-split ()
  "Split this message into the `spam' group if it is spam.
This function can be used as an entry in `nnmail-split-fancy', for
example like this: (: spam-split)

See the Info node `(gnus)Fancy Mail Splitting' for more details."
  (interactive)
  
  ;; load the spam-stat tables if needed
  (when spam-use-stat (spam-stat-load))

  (let ((list-of-checks spam-list-of-checks)
	decision)
    (while (and list-of-checks (not decision))
      (let ((pair (pop list-of-checks)))
	(when (symbol-value (car pair))
	  (gnus-message 5 "spam-split: calling the %s function" (symbol-name (cdr pair)))
	  (setq decision (funcall (cdr pair))))))
    (if (eq decision t)
	nil
      decision)))

;;;; Regex headers

(defun spam-check-regex-headers ()
  (let (ret found)
    (dolist (h-regex spam-regex-headers-ham)
      (unless found
	(goto-char (point-min))
	(when (re-search-forward h-regex nil t)
	  (message "Ham regex header search positive.")
	  (setq found t))))
    (dolist (s-regex spam-regex-headers-spam)
      (unless found
	(goto-char (point-min))
	(when (re-search-forward s-regex nil t)
	  (message "Spam regex header search positive." (match-string 1))
	  (setq found t)
	  (setq ret spam-split-group))))
    ret))


;;;; Blackholes.

(defun spam-check-blackholes ()
  "Check the Received headers for blackholed relays."
  (let ((headers (message-fetch-field "received"))
	ips matches)
    (when headers
      (with-temp-buffer
	(insert headers)
	(goto-char (point-min))
	(gnus-message 5 "Checking headers for relay addresses")
	(while (re-search-forward
		"\\[\\([0-9]+.[0-9]+.[0-9]+.[0-9]+\\)\\]" nil t)
	  (gnus-message 9 "Blackhole search found host IP %s." (match-string 1))
	  (push (mapconcat 'identity
			   (nreverse (split-string (match-string 1) "\\."))
			   ".")
		ips)))
      (dolist (server spam-blackhole-servers)
	(dolist (ip ips)
	  (unless (and spam-blackhole-good-server-regex
		       (string-match spam-blackhole-good-server-regex ip))
	    (let ((query-string (concat ip "." server)))
	      (if spam-use-dig
		  (let ((query-result (query-dig query-string)))
		    (when query-result
		      (gnus-message 5 "(DIG): positive blackhole check '%s'" query-result)
		      (push (list ip server query-result)
			    matches)))
		;; else, if not using dig.el
		(when (query-dns query-string)
		  (gnus-message 5 "positive blackhole check")
		  (push (list ip server (query-dns query-string 'TXT))
			matches))))))))
    (when matches
      spam-split-group)))

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
    (let ((who (message-fetch-field "from")))
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
	(insert-string article-string)
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
	       (insert-string article-string)
	       (spam-stat-buffer-is-spam))))
	 nil)
	(spam-stat-save))

      (defun spam-stat-register-ham-routine ()
	(spam-generic-register-routine 
	 nil
	 (lambda (article)
	   (let ((article-string (spam-get-article-as-string article)))
	     (with-temp-buffer
	       (insert-string article-string)
	       (spam-stat-buffer-is-non-spam)))))
	(spam-stat-save)))

  (file-error (progn
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
    (goto-char (point-max))
    (unless (bobp)
      (insert "\n"))
    (insert address "\n")
    (save-buffer)))

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
	  (unless (zerop (length address))
	    (setq address (regexp-quote address))
	    (while (string-match "\\\\\\*" address)
	      (setq address (replace-match ".*" t t address)))
	    (push address contents))))
      (nreverse contents))))

(defun spam-from-listed-p (cache)
  (let ((from (message-fetch-field "from"))
	found)
    (while cache
      (when (string-match (pop cache) from)
	(setq found t
	      cache nil)))
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


;;;; Bogofilter

(defun spam-check-bogofilter-headers (&optional score)
  (let ((header (message-fetch-field spam-bogofilter-header)))
      (when (and header
		 (string-match spam-bogofilter-bogosity-positive-spam-header
			       header))
	  (if score
	      (when (string-match "spamicity=\\([0-9.]+\\)" header)
		(match-string 1 header))
	    spam-split-group))))

;; return something sensible if the score can't be determined
(defun spam-bogofilter-score ()
  "Get the Bogofilter spamicity score"
  (interactive)
  (save-window-excursion
    (gnus-summary-show-article t)
    (set-buffer gnus-article-buffer)
    (let ((score (spam-check-bogofilter t)))
      (message "Spamicity score %s" score)
      (or score "0"))))

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
    (let ((switch (if spam "-s" "-n")))
      (with-temp-buffer
	(insert-string article-string)
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

(provide 'spam)

;;; spam.el ends here.
