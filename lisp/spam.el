;;; spam.el --- Identifying spam
;; Copyright (C) 2002 Free Software Foundation, Inc.

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

;; FIXME!  We should not require `message' until we actually need
;; them.  Best would be to declare needed functions as auto-loadable.
(require 'message)

;; Attempt to load BBDB macros
(eval-when-compile
  (condition-case nil
      (require 'bbdb-com)
    (file-error (defalias 'bbdb-search 'ignore))))

;; autoload executable-find
(eval-and-compile
  ;; executable-find is not autoloaded in Emacs 20
  (autoload 'executable-find "executable"))

;; autoload ifile-spam-filter
(eval-and-compile
  (autoload 'ifile-spam-filter "ifile-gnus"))

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

(defcustom spam-use-blacklist t
  "Whether the blacklist should be used by spam-split."
  :type 'boolean
  :group 'spam)

(defcustom spam-use-whitelist nil
  "Whether the whitelist should be used by spam-split."
  :type 'boolean
  :group 'spam)

(defcustom spam-use-blackholes nil
  "Whether blackholes should be used by spam-split."
  :type 'boolean
  :group 'spam)

(defcustom spam-use-bogofilter nil
  "Whether bogofilter should be used by spam-split."
  :type 'boolean
  :group 'spam)

(defcustom spam-use-bbdb nil
  "Whether BBDB should be used by spam-split."
  :type 'boolean
  :group 'spam)

(defcustom spam-use-ifile nil
  "Whether ifile should be used by spam-split."
  :type 'boolean
  :group 'spam)

(defcustom spam-split-group "spam"
  "Group name where incoming spam should be put by spam-split."
  :type 'string
  :group 'spam)

;; FIXME!  The mailgroup list evidently depends on other choices made by the
;; user, so the built-in default below is not likely to be appropriate.
(defcustom spam-junk-mailgroups (cons spam-split-group '("mail.junk" "poste.pourriel"))
  "Mailgroups with spam contents.
All unmarked article in such group receive the spam mark on group entry."
  :type '(repeat (string :tag "Group"))
  :group 'spam)

(defcustom spam-blackhole-servers '("bl.spamcop.net" "relays.ordb.org" "dev.null.dk" "relays.visi.com")
  "List of blackhole servers."
  :type '(repeat (string :tag "Server"))
  :group 'spam)

(defcustom spam-ham-marks (list gnus-del-mark gnus-read-mark gnus-killed-mark gnus-kill-file-mark gnus-low-score-mark)
  "Marks considered as being ham (positively not spam).
Such articles will be processed as ham (non-spam) on group exit."
  :type '(repeat (character :tag "Mark"))
  :group 'spam)

(defcustom spam-spam-marks (list gnus-spam-mark)
  "Marks considered as being spam (positively spam).
Such articles will be transmitted to `bogofilter -s' on group exit."
  :type '(repeat (character :tag "Mark"))
  :group 'spam)

(defcustom spam-face 'gnus-splash-face
  "Face for spam-marked articles"
  :type 'face
  :group 'spam)

(defgroup spam-bogofilter nil
  "Spam bogofilter configuration."
  :group 'spam)

(defcustom spam-bogofilter-output-buffer-name "*Bogofilter Output*"
  "Name of buffer when displaying `bogofilter -v' output."  
  :type 'string
  :group 'spam-bogofilter)

(defcustom spam-bogofilter-initial-timeout 40
  "Timeout in seconds for the initial reply from the `bogofilter' program."
  :type 'integer
  :group 'spam-bogofilter)

(defcustom spam-bogofilter-subsequent-timeout 15
  "Timeout in seconds for any subsequent reply from the `bogofilter' program."
  :type 'integer
  :group 'spam-bogofilter)

(defcustom spam-bogofilter-path (executable-find "bogofilter")
  "File path of the Bogofilter executable program."
  :type '(choice (file :tag "Location of bogofilter")
		 (const :tag "Bogofilter is not installed"))
  :group 'spam-bogofilter)

;; FIXME!  In the following regexp, we should explain which tool produces
;; which kind of header.  I do not even remember them all by now.  X-Junk
;; (and previously X-NoSpam) are produced by the `NoSpam' tool, which has
;; never been published, so it might not be reasonable leaving it in the
;; list.
(defcustom spam-bogofilter-spaminfo-header-regexp "^X-\\(jf\\|Junk\\|NoSpam\\|Spam\\|SB\\)[^:]*:"
  "Regexp for spam markups in headers.
Markup from spam recognisers, as well as `Xref', are to be removed from
articles before they get registered by Bogofilter."
  :type 'regexp
  :group 'spam-bogofilter)

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

;;; Hooks dispatching.  A bit raw for now.

(defun spam-summary-prepare ()
  (spam-mark-junk-as-spam-routine))

(defun spam-summary-prepare-exit ()
  (spam-bogofilter-register-routine))

(add-hook 'gnus-summary-prepare-hook 'spam-summary-prepare)
(add-hook 'gnus-summary-prepare-exit-hook 'spam-summary-prepare-exit)

(defun spam-mark-junk-as-spam-routine ()
  (when (member gnus-newsgroup-name spam-junk-mailgroups)
    (let ((articles gnus-newsgroup-articles)
	  article)
      (while articles
	(setq article (pop articles))
	(when (eq (gnus-summary-article-mark article) gnus-unread-mark)
	  (gnus-summary-mark-article article gnus-spam-mark))))))

;;;; Spam determination.


(defvar spam-list-of-checks
  '((spam-use-blacklist  . spam-check-blacklist)
    (spam-use-whitelist  . spam-check-whitelist)
    (spam-use-bbdb	 . spam-check-bbdb)
    (spam-use-ifile	 . spam-check-ifile)
    (spam-use-blackholes . spam-check-blackholes)
    (spam-use-bogofilter . spam-check-bogofilter))
"The spam-list-of-checks list contains pairs associating a parameter
variable with a spam checking function.  If the parameter variable is
true, then the checking function is called, and its value decides what
happens.  Each individual check may return `nil', `t', or a mailgroup
name.  The value `nil' means that the check does not yield a decision,
and so, that further checks are needed.  The value `t' means that the
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

  (let ((list-of-checks spam-list-of-checks)
	decision)
    (while (and list-of-checks (not decision))
      (let ((pair (pop list-of-checks)))
	(when (symbol-value (car pair))
	  (setq decision (funcall (cdr pair))))))
    (if (eq decision t)
	nil
      decision)))

;;;; Blackholes.

(defun spam-check-blackholes ()
  "Check the Received headers for blackholed relays."
  (let ((headers (message-fetch-field "received"))
	ips matches)
    (when headers
      (with-temp-buffer
	(insert headers)
	(goto-char (point-min))
	(while (re-search-forward
		"\\[\\([0-9]+.[0-9]+.[0-9]+.[0-9]+\\)\\]" nil t)
	  (message "Blackhole search found host IP %s." (match-string 1))
	  (push (mapconcat 'identity
			   (nreverse (split-string (match-string 1) "\\."))
			   ".")
		ips)))
      (dolist (server spam-blackhole-servers)
	(dolist (ip ips)
	  (let ((query-string (concat ip "." server)))
	    (if spam-use-dig
		(let ((query-result (query-dig query-string)))
		  (when query-result
		    (message "spam detected with blackhole check of relay %s (dig query result '%s')" query-string query-result)
		    (push (list ip server query-result)
			  matches)))
	      ;; else, if not using dig.el
	      (when (query-dns query-string)
		(push (list ip server (query-dns query-string 'TXT))
		      matches)))))))
    (when matches
      spam-split-group)))

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

;;; returns nil if the sender is in the whitelist, spam-split-group otherwise
(defun spam-check-whitelist ()
  ;; FIXME!  Should it detect when file timestamps change?
  (unless spam-whitelist-cache
    (setq spam-whitelist-cache (spam-parse-list spam-whitelist)))
  (if (spam-from-listed-p spam-whitelist-cache) nil spam-split-group))

;;; original idea from Alexander Kotelnikov <sacha@giotto.sj.ru>
(condition-case nil
    (progn
      (require 'bbdb-com)
      (defun spam-check-bbdb ()
	"We want messages from people who are in the BBDB not to be split to spam"
	(let ((who (message-fetch-field "from")))
	  (when who
	    (setq who (regexp-quote (cadr (gnus-extract-address-components who))))
	    (if (bbdb-search (bbdb-records) nil nil who) nil spam-split-group)))))
  (file-error (setq spam-list-of-checks
		    (delete (assoc 'spam-use-bbdb spam-list-of-checks)
			    spam-list-of-checks))))

;;; check the ifile backend; return nil if the mail was NOT classified as spam
;;; TODO: we can't (require) ifile, because it will insinuate itself automatically
(defun spam-check-ifile ()
  (let ((ifile-primary-spam-group spam-split-group))
    (ifile-spam-filter nil)))

(defun spam-check-blacklist ()
  ;; FIXME!  Should it detect when file timestamps change?
  (unless spam-blacklist-cache
    (setq spam-blacklist-cache (spam-parse-list spam-blacklist)))
  (and (spam-from-listed-p spam-blacklist-cache) spam-split-group))

(eval-and-compile
  (defalias 'spam-point-at-eol (if (fboundp 'point-at-eol)
				   'point-at-eol
				 'line-end-position)))

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


;;;; Training via Bogofilter.   Last updated 2002-09-02.

;;; See Paul Graham article, at `http://www.paulgraham.com/spam.html'.

;;; This page is for those wanting to control spam with the help of Eric
;;; Raymond's speedy Bogofilter, see http://www.tuxedo.org/~esr/bogofilter.
;;; This has been tested with a locally patched copy of version 0.4.

;;; Make sure Bogofilter is installed.  Bogofilter internally uses Judy fast
;;; associative arrays, so you need to install Judy first, and Bogofilter
;;; next.  Fetch both distributions by visiting the following links and
;;; downloading the latest version of each:
;;;
;;;     http://sourceforge.net/projects/judy/
;;;     http://www.tuxedo.org/~esr/bogofilter/
;;;
;;; Unpack the Judy distribution and enter its main directory.  Then do:
;;;
;;;     ./configure
;;;     make
;;;     make install
;;;
;;; You will likely need to become super-user for the last step.  Then, unpack
;;; the Bogofilter distribution and enter its main directory:
;;;
;;;     make
;;;     make install
;;;
;;; Here as well, you need to become super-user for the last step.  Now,
;;; initialize your word lists by doing, under your own identity:
;;;
;;;     mkdir ~/.bogofilter
;;;     touch ~/.bogofilter/badlist
;;;     touch ~/.bogofilter/goodlist
;;;
;;; These two files are text files you may edit, but you normally don't!

;;; The `M-d' command gets added to Gnus summary mode, marking current article
;;; as spam, showing it with the `H' mark.  Whenever you see a spam article,
;;; make sure to mark its summary line with `M-d' before leaving the group.
;;; Some groups, as per variable `spam-junk-mailgroups' below, receive articles
;;; from Gnus splitting on clues added by spam recognisers, so for these
;;; groups, we tack an `H' mark at group entry for all summary lines which
;;; would otherwise have no other mark.  Make sure to _remove_ `H' marks for
;;; any article which is _not_ genuine spam, before leaving such groups: you
;;; may use `M-u' to "unread" the article, or `d' for declaring it read the
;;; non-spam way.  When you leave a group, all `H' marked articles, saved or
;;; unsaved, are sent to Bogofilter which will study them as spam samples.

;;; Messages may also be deleted in various other ways, and unless
;;; `spam-ham-marks-form' gets overridden below, marks `R' and `r' for default
;;; read or explicit delete, marks `X' and 'K' for automatic or explicit
;;; kills, as well as mark `Y' for low scores, are all considered to be
;;; associated with articles which are not spam.  This assumption might be
;;; false, in particular if you use kill files or score files as means for
;;; detecting genuine spam, you should then adjust `spam-ham-marks-form'.  When
;;; you leave a group, all _unsaved_ articles bearing any the above marks are
;;; sent to Bogofilter which will study these as not-spam samples.  If you
;;; explicit kill a lot, you might sometimes end up with articles marked `K'
;;; which you never saw, and which might accidentally contain spam.  Best is
;;; to make sure that real spam is marked with `H', and nothing else.

;;; All other marks do not contribute to Bogofilter pre-conditioning.  In
;;; particular, ticked, dormant or souped articles are likely to contribute
;;; later, when they will get deleted for real, so there is no need to use
;;; them prematurely.  Explicitly expired articles do not contribute, command
;;; `E' is a way to get rid of an article without Bogofilter ever seeing it.

;;; In a word, with a minimum of care for associating the `H' mark for spam
;;; articles only, Bogofilter training all gets fairly automatic.  You should
;;; do this until you get a few hundreds of articles in each category, spam
;;; or not.  The shell command `head -1 ~/.bogofilter/*' shows both article
;;; counts.  The command `S S' in summary mode, either for debugging or for
;;; curiosity, triggers Bogofilter into displaying in another buffer the
;;; "spamicity" score of the current article (between 0.0 and 1.0), together
;;; with the article words which most significantly contribute to the score.

;;; The real way for using Bogofilter, however, is to have some use tool like
;;; `procmail' for invoking it on message reception, then adding some
;;; recognisable header in case of detected spam.  Gnus splitting rules might
;;; later trip on these added headers and react by sorting such articles into
;;; specific junk folders as per `spam-junk-mailgroups'.  Here is a possible
;;; `.procmailrc' contents (still untested -- please tell me how it goes):
;;;
;;; :0HBf:
;;; * ? bogofilter
;;; | formail -bfI "X-Spam-Status: Yes"

(defun spam-check-bogofilter ()
  ;; Dynamic spam check.  I do not know how to check the exit status,
  ;; so instead, read `bogofilter -v' output.
  (when (and spam-use-bogofilter spam-bogofilter-path)
    (spam-bogofilter-articles nil "-v" (list (gnus-summary-article-number)))
    (when (save-excursion
	    (set-buffer spam-bogofilter-output-buffer-name)
	    (goto-char (point-min))
	    (re-search-forward "Spamicity: \\(0\\.9\\|1\\.0\\)" nil t))
      spam-split-group)))

(defun spam-bogofilter-score ()
  "Use `bogofilter -v' on the current article.
This yields the 15 most discriminant words for this article and the
spamicity coefficient of each, and the overall article spamicity."
  (interactive)
  (when (and spam-use-bogofilter spam-bogofilter-path)
    (spam-bogofilter-articles nil "-v" (list (gnus-summary-article-number)))
    (with-current-buffer spam-bogofilter-output-buffer-name
      (unless (zerop (buffer-size))
	(if (<= (count-lines (point-min) (point-max)) 1)
	    (progn
	      (goto-char (point-max))
	      (when (bolp)
		(backward-char 1))
	      (message "%s" (buffer-substring (point-min) (point))))
	  (goto-char (point-min))
	  (display-buffer (current-buffer)))))))

(defun spam-bogofilter-register-routine ()
  (when (and spam-use-bogofilter spam-bogofilter-path)
    (let ((articles gnus-newsgroup-articles)
	  article mark ham-articles spam-articles)
      (while articles
	(setq article (pop articles)
	      mark (gnus-summary-article-mark article))
	(cond ((memq mark spam-spam-marks) (push article spam-articles))
	      ((memq article gnus-newsgroup-saved))
	      ((memq mark spam-ham-marks) (push article ham-articles))))
      (when ham-articles
	(spam-bogofilter-articles "ham" "-n" ham-articles))
      (when spam-articles
	(spam-bogofilter-articles "SPAM" "-s" spam-articles)))))

(defun spam-bogofilter-articles (type option articles)
  (let ((output-buffer (get-buffer-create spam-bogofilter-output-buffer-name))
	(article-copy (get-buffer-create " *Bogofilter Article Copy*"))
	(remove-regexp (concat spam-bogofilter-spaminfo-header-regexp "\\|Xref:"))
	(counter 0)
	prefix process article)
    (when type
      (setq prefix (format "Studying %d articles as %s..." (length articles)
			   type))
      (message "%s" prefix))
    (save-excursion (set-buffer output-buffer) (erase-buffer))
    (setq process (start-process "bogofilter" output-buffer
				 spam-bogofilter-path "-F" option))
    (process-kill-without-query process t)
    (unwind-protect
	(save-window-excursion
	  (while articles
	    (setq counter (1+ counter))
	    (when prefix
	      (message "%s %d" prefix counter))
	    (setq article (pop articles))
	    (gnus-summary-goto-subject article)
	    (gnus-summary-show-article t)
	    (gnus-eval-in-buffer-window article-copy
	      (insert-buffer-substring gnus-original-article-buffer)
	      ;; Remove spam classification redundant headers: they may induce
	      ;; unwanted biases in later analysis.
	      (message-remove-header remove-regexp t)
	      ;; Bogofilter really wants From envelopes for counting articles.
	      ;; Fake one at the beginning, make sure there will be no other.
	      (goto-char (point-min))
	      (if (looking-at "From ")
		  (forward-line 1)
		(insert "From nobody " (current-time-string) "\n"))
	      (let (case-fold-search)
		(while (re-search-forward "^From " nil t)
		  (beginning-of-line)
		  (insert ">")))
	      (process-send-region process (point-min) (point-max))
	      (erase-buffer))))
      ;; Sending the EOF is unwind-protected.  This is to prevent lost copies
      ;; of `bogofilter', hung on reading their standard input, in case the
      ;; whole registering process gets interrupted by the user.
      (process-send-eof process))
    (kill-buffer article-copy)
    ;; Receive process output.  It sadly seems that we still have to protect
    ;; ourselves against hung `bogofilter' processes.
    (let ((status (process-status process))
	  (timeout (* 1000 spam-bogofilter-initial-timeout))
	  (quanta 200))			; also counted in milliseconds
      (while (and (not (eq status 'exit)) (> timeout 0))
	;; `accept-process-output' timeout is counted in microseconds.
	(setq timeout (if (accept-process-output process 0 (* 1000 quanta))
			  (* 1000 spam-bogofilter-subsequent-timeout)
			(- timeout quanta))
	      status (process-status process)))
      (if (eq status 'exit)
	  (when prefix
	    (message "%s done!" prefix))
	;; Sigh!  The process did time out...  Become brutal!
	(interrupt-process process)
	(message "%s %d INTERRUPTED!  (Article %d, status %s)"
		 (or prefix "Bogofilter process...")
		 counter article status)
	;; Give some time for user to read.  Sitting redisplays but gives up
	;; if input is pending.  Sleeping does not give up, but it does not
	;; redisplay either.  Mix both: let's redisplay and not give up.
	(sit-for 1)
	(sleep-for 3)))))

(provide 'spam)

;;; spam.el ends here.
