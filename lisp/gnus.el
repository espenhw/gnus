;;; (ding) Gnus --- a newsreader for GNU Emacs
;; Copyright (C) 1987,88,89,90,93,94,95 Free Software Foundation, Inc.

;; Author: Masanobu UMEDA <umerin@flab.flab.fujitsu.junet>
;;	Lars Ingebrigtsen <larsi@ifi.uio.no>
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

;; Although (ding) Gnus looks suspiciously like GNUS, it isn't quite
;; the same beast. Most internal structures have been changed. If you
;; have written packages that depend on any of the hash tables,
;; `gnus-newsrc-assoc', `gnus-killed-assoc', marked lists, the .newsrc
;; buffer, or internal knowledge of the `nntp-header-' macros, or
;; dependence on the buffers having a certain format, your code will
;; fail.

;;; Code:

(require 'mail-utils)
(require 'timezone)
(require 'rnews)
(require 'rmail)

(require 'nnheader)

;; Customization variables

(defvar gnus-select-method 
  (list 'nntp (or (getenv "NNTPSERVER") 
		  (if (and gnus-default-nntp-server
			   (not (string= gnus-default-nntp-server "")))
		      gnus-default-nntp-server)
		  (system-name))
	"nntp")
  "Default method for selecting a newsgroup.
This variable should be a list, where the first element is how the
news is to be fetched, the second is the address, and the optional
third element is the \"port number\", if nntp is used.

For instance, if you want to get your news via NNTP from
\"flab.flab.edu\" on port 23, you could say:

(setq gnus-select-method '(nntp \"flab.flab.edu\" 23))

If you want to use your local spool, say:

(setq gnus-select-method (list 'nnspool (system-name)))

If you use this variable, you must set `gnus-nntp-server' to nil.")

;; Added by Sudish Joseph <joseph@cis.ohio-state.edu>.
(defvar gnus-post-method nil
  "Preferred method for posting USENET news.
If this variable is nil, GNUS will use the current method to decide
which method to use when posting.  If it is non-nil, it will override
the current method. This method will not be used in mail groups and
the like, only in \"real\" newsgroups.

The value must be a valid method as discussed in the documentation of
`gnus-select-method'.")

(defvar gnus-secondary-select-methods nil
  "A list of secondary methods that will be used for reading news.")

(defvar gnus-default-nntp-server nil
  "Specify a default NNTP server.
This variable should be defined in paths.el, and should never be set
by the user.
If you want to change servers, you should use `gnus-select-method'.
See the documentation to that variable.")

(defvar gnus-secondary-servers nil
  "List of NNTP servers that the user can choose between interactively.
To make Gnus query you for a server, you have to give `gnus' a
non-numeric prefix - `C-u M-x gnus', in short.")

(defvar gnus-nntp-server nil
  "*The name of the host running the NNTP server.
This variable is semi-obsolete. Use the `gnus-select-method'
variable instead.")

(defvar gnus-nntp-service "nntp"
  "NNTP service name (\"nntp\" or 119).
This is an obsolete variable, which is scarcely used. If you use an
nntp server for your newsgroup and want to change the port number
used to 899, you would say something along these lines:

(setq gnus-select-method '(nntp \"my.nntp.server\" 899))")

(defvar gnus-startup-file "~/.newsrc"
  "Your `.newsrc' file.  Use `.newsrc-SERVER' instead if it exists.")

(defvar gnus-signature-file "~/.signature"
  "Your signature file.
If the variable is a string that doesn't correspond to a file, the
string itself is inserted.")

(defvar gnus-signature-function nil
  "A function that should return a signature file name.
The function will be called with the name of the newsgroup being
posted to.
If the function returns a string that doesn't correspond to a file, the
string itself is inserted.
If the function returns nil, the `gnus-signature-file' variable will
be used instead.")

(defvar gnus-init-file "~/.gnus"
  "Your Gnus elisp startup file.
If a file with the .el or .elc suffixes exist, they will be read
instead.") 

(defvar gnus-default-subscribed-newsgroups nil
  "This variable lists what newsgroups should be susbcribed the first time Gnus is used.
It should be a list of strings.
If it is `t', Gnus will not do anything special the first time it is
started; it'll just use the normal newsgroups subscription methods.")

(defconst gnus-backup-default-subscribed-newsgroups 
  '("news.announce.newusers" "news.groups.questions" "gnu.emacs.gnus")
  "Default default new newsgroups the first time Gnus is run.")

(defvar gnus-post-prepare-function nil
  "Function that is run after a post buffer has been prepared.
It is called with the name of the newsgroup that is posted to. It
might be use, for instance, for inserting signatures based on the
newsgroup name. (In that case, `gnus-signature-file' and
`mail-signature' should both be set to nil).")

(defvar gnus-use-cross-reference t
  "Non-nil means that cross referenced articles will be marked as read.
If nil, ignore cross references.  If t, mark articles as read in
subscribed newsgroups. If neither t nor nil, mark as read in all
newsgroups.") 

(defvar gnus-use-dribble-file t
  "Non-nil means that Gnus will use a dribble file to store user updates.
If Emacs should crash without saving the .newsrc files, complete
information can be restored from the dribble file.")

(defvar gnus-use-followup-to 'use
  "Specifies what to do with Followup-To header.
If nil, ignore the header. If it is t, use its value, but ignore 
`poster'. If it is neither nil nor t, always use the value.")

(defvar gnus-followup-to-function nil
  "A variable that contains a function that returns a followup address.
The function will be called in the buffer of the article that is being
followed up. The buffer will be narrowed to the headers of the
article. To pick header headers, one might use `mail-fetch-field'.  The
function will be called with the name of the current newsgroup as the
argument.

Here's an example `gnus-followup-to-function':

(setq gnus-followup-to-function
      (lambda (group)
	(cond ((string= group \"mail.list\")
	       (or (mail-fetch-field \"sender\") 
		   (mail-fetch-field \"from\")))
	      (t
	       (or (mail-fetch-field \"reply-to\") 
		   (mail-fetch-field \"from\"))))))")

(defvar gnus-reply-to-function nil
  "A variable that contains a function that returns a reply address.
See the `gnus-followup-to-function' variable for an explanation of how
this variable is used.")

(defvar gnus-large-newsgroup 200
  "The number of articles which indicates a large newsgroup.
If the number of articles in a newsgroup is greater than the value,
confirmation is required for selecting the newsgroup.")

(defvar gnus-author-copy (getenv "AUTHORCOPY")
  "Name of the file the article will be saved before it is posted using the FCC header.
Initialized from the AUTHORCOPY environment variable.

Articles are saved using a function specified by the the variable
`gnus-author-copy-saver' (`rmail-output' is default) if a file name is
given.  Instead, if the first character of the name is `|', the
contents of the article is piped out to the named program. It is
possible to save an article in an MH folder as follows:

\(setq gnus-author-copy \"|/usr/local/lib/mh/rcvstore +Article\")")

(defvar gnus-mail-self-blind nil
  "Non-nil means insert BCC to self in messages to be sent.
This is done when the message is initialized,
so you can remove or alter the BCC header to override the default.")

(defvar gnus-author-copy-saver (function rmail-output)
  "A function called with a file name to save an author copy to.
The default function is `rmail-output' which saves in Unix mailbox format.")

(defvar gnus-use-long-file-name (not (memq system-type '(usg-unix-v xenix)))
  "Non-nil means that the default name of a file to save articles in is the newsgroup name.
If it's nil, the directory form of the newsgroup name is used instead.")

(defvar gnus-article-save-directory (or (getenv "SAVEDIR") "~/News/")
  "Name of the directory articles will be saved in (default \"~/News\").
Initialized from the SAVEDIR environment variable.")

(defvar gnus-kill-files-directory (or (getenv "SAVEDIR") "~/News/")
  "Name of the directory where kill files will be stored (default \"~/News\").
Initialized from the SAVEDIR environment variable.")

(defvar gnus-kill-expiry-days 7
  "*Number of days before unused kill file entries are expired.")

(defvar gnus-default-article-saver (function gnus-summary-save-in-rmail)
  "A function to save articles in your favorite format.
The function must be interactively callable (in other words, it must
be an Emacs command).

Gnus provides the following functions:

* gnus-summary-save-in-rmail (Rmail format)
* gnus-summary-save-in-mail (Unix mail format)
* gnus-summary-save-in-folder (MH folder)
* gnus-summary-save-in-file (article format).")

(defvar gnus-rmail-save-name (function gnus-plain-save-name)
  "A function generating a file name to save articles in Rmail format.
The function is called with NEWSGROUP, HEADERS, and optional LAST-FILE.")

(defvar gnus-mail-save-name (function gnus-plain-save-name)
  "A function generating a file name to save articles in Unix mail format.
The function is called with NEWSGROUP, HEADERS, and optional LAST-FILE.")

(defvar gnus-folder-save-name (function gnus-folder-save-name)
  "A function generating a file name to save articles in MH folder.
The function is called with NEWSGROUP, HEADERS, and optional LAST-FOLDER.")

(defvar gnus-file-save-name (function gnus-numeric-save-name)
  "A function generating a file name to save articles in article format.
The function is called with NEWSGROUP, HEADERS, and optional LAST-FILE.")

(defvar gnus-kill-file-name "KILL"
  "Suffix of the kill files.")

(defvar gnus-fetch-old-headers nil
  "Non-nil means that Gnus will try to build threads by grabbing old headers.
If an unread article in the group refers to an older, already read (or
just marked as read) article, the old article will not normally be
displayed in the Summary buffer. If this variable is non-nil, Gnus
will attempt to grab the headers to the old articles, and thereby
build complete threads. `gnus-nov-is-evil' has to be nil if this is
to work.  If it has the value `some', only enough headers to connect
otherwise loose threads will be displayed.")

(defvar gnus-visual t
  "*If non-nil, will do various highlighting.
If nil, no mouse highlight (or any other) will be performed. This
might speed up Gnus some when generating large group and summary
buffers.")

(defvar gnus-novice-user t
  "*Non-nil means that you are a usenet novice.
If non-nil, verbose messages may be displayed and confirmations may be
required.")

(defvar gnus-expert-user nil
  "*Non-nil means that you will never be asked for confirmation about anything.
And that means *anything*.")

(defvar gnus-keep-same-level nil
  "Non-nil means that the next newsgroup after the current will be on the same level.
When you type, for instance, `n' after reading the last article in the
current newsgroup, you will go to the next newsgroup. If this variable
is nil, the next newsgroup will be the next from the group
buffer. 
If this variable is non-nil, Gnus will either put you in the
next newsgroup with the same level, or, if no such newsgroup is
available, the next newsgroup with the lowest possible level higher
than the current level.
If this variable is `best', Gnus will make the next newsgroup the one
with the best level.")

(defvar gnus-summary-make-false-root 'adopt
  "nil means that Gnus won't gather loose threads.
If the root of a thread has expired or been read in a previous
session, the information necessary to build a complete thread has been
lost. Instead of having many small sub-threads from this original thread
scattered all over the summary buffer, Gnus can gather them. 

If non-nil, Gnus will try to gather all loose sub-threads from an
original thread into one large thread.

If this variable is non-nil, it should be one of `none', `adopt',
`dummy' or `empty'.

If this variable is `none', Gnus will not make a false root, but just
present the sub-threads after another.
If this variable is `dummy', Gnus will create a dummy root that will
have all the sub-threads as children.
If this variable is `adopt', Gnus will make one of the \"children\"
the parent and mark all the step-children as such.
If this variable is `empty', the \"children\" are printed with empty
subject fields.")

(defvar gnus-summary-gather-subject-limit nil
  "*Maximum length of subject to compare when gathering loose threads.
Use nil to compare the whole subject.")

(defvar gnus-check-new-newsgroups t
  "Non-nil means that Gnus will add new newsgroups at startup.
If this variable is `ask-server', Gnus will ask the server for new
groups since the last time it checked. This means that the killed list
is no longer necessary, so you could set `gnus-save-killed-list' to
nil. 
A variant is to have this variable be a list of select methods. Then
Gnus will use the `ask-server' method on all these select methods to
query for new groups from all those servers.

Eg.
  (setq gnus-check-new-newsgroups 
        '((nntp \"some.server\") (nntp \"other.server\")))

If this variable is nil, then you have to tell Gnus explicitly to
check for new newsgroups with \\<gnus-group-mode-map>\\[gnus-find-new-newsgroups].")

(defvar gnus-check-bogus-newsgroups nil
  "Non-nil means that Gnus will check and remove bogus newsgroup at startup.
If this variable is nil, then you have to tell Gnus explicitly to
check for bogus newsgroups with \\<gnus-group-mode-map>\\[gnus-group-check-bogus-groups].")

(defvar gnus-read-active-file t
  "Non-nil means that Gnus will read the entire active file at startup.
If this variable is nil, Gnus will only read parts of the active file.")

(defvar gnus-activate-foreign-newsgroups nil
  "If nil, Gnus will not check foreign newsgroups at startup.
If it is non-nil, it should be a number between one and nine. Foreign
newsgroups that have a level lower or equal to this number will be
activated on startup. For instance, if you want to active all
subscribed newsgroups, but not the rest, you'd set this variable to 5.

If you subscribe to lots of newsgroups from different servers, startup
might take a while. By setting this variable to nil, you'll save time,
but you won't be told how many unread articles there are in the
newsgroups.")

(defvar gnus-save-newsrc-file t
  "Non-nil means that Gnus will save a .newsrc file.
Gnus always saves its own startup file, which is called \".newsrc.el\".
The file called \".newsrc\" is in a format that can be readily
understood by other newsreaders. If you don't plan on using other
newsreaders, set this variable to nil to save some time on exit.")

(defvar gnus-save-killed-list t
  "If non-nil, save the list of killed groups to the startup file.
This will save both time (when starting and quitting) and space (on
disk), but it will also mean that Gnus has no record of what
newsgroups are new or old, so the automatic new newsgroups
subscription methods become meaningless. You should always set
`gnus-check-new-newsgroups' to nil if you set this variable to nil.") 

(defvar gnus-interactive-catchup t
  "Require your confirmation when catching up a newsgroup if non-nil.")

(defvar gnus-interactive-post t
  "Group and subject will be asked for if non-nil.")

(defvar gnus-interactive-exit t
  "Require your confirmation when exiting Gnus if non-nil.")

(defvar gnus-kill-killed nil
  "If non-nil, Gnus will apply kill files to already killd articles.
If it is nil, Gnus will never apply kill files to articles that have
already been through the scoring process, which might very well save lots
of time.")

;; Added by Per Abrahamsen <amanda@iesd.auc.dk>.
(defvar gnus-summary-same-subject ""
  "String indicating that the current article has the same subject as the previous.")

(defvar gnus-score-interactive-default-score 1000
  "Scoring commands will raise/lower with this number as the default.")

(defvar gnus-summary-default-score 0
  "Default article score level.
If this variable is nil, score levels will not be used.")

(defvar gnus-user-login-name nil
  "The login name of the user.
Got from the function `user-login-name' if undefined.")

(defvar gnus-user-full-name nil
  "The full name of the user.
Got from the NAME environment variable if undefined.")

(defvar gnus-show-mime nil
  "*Show MIME message if non-nil.")

(defvar gnus-show-threads t
  "*Show conversation threads in summary mode if non-nil.")

(defvar gnus-thread-hide-subtree nil
  "Non-nil means hide thread subtrees initially.
If non-nil, you have to run the command `gnus-summary-show-thread' by
hand or by using `gnus-select-article-hook' to show hidden threads.")

(defvar gnus-thread-hide-killed t
  "Non-nil means hide killed thread subtrees automatically.")

(defvar gnus-thread-ignore-subject nil
  "Don't take care of subject differences, but only references if non-nil.
If it is non-nil, some commands work with subjects do not work properly.")

(defvar gnus-thread-indent-level 4
  "Indentation of thread subtrees.")

;; jwz: nuke newsgroups whose name is all digits - that means that
;; some loser has let articles get into the root of the news spool,
;; which is toxic. Lines beginning with whitespace also tend to be
;; toxic.
(defvar gnus-ignored-newsgroups
  (purecopy (mapconcat 'identity
		       '("^to\\."	; not "real" groups
			 "^[0-9. \t]+ "	; all digits in name
			 "[][\"#'()	;\\]"	; bogus characters
			 )
		       "\\|"))
  "A regexp to match uninteresting newsgroups in the active file.
Any lines in the active file matching this regular expression are
removed from the newsgroup list before anything else is done to it,
thus making them effectively non-existant.")

(defvar gnus-ignored-headers
  "^Path:\\|^Posting-Version:\\|^Article-I.D.:\\|^Expires:\\|^Date-Received:\\|^References:\\|^Control:\\|^Xref:\\|^Lines:\\|^Posted:\\|^Relay-Version:\\|^Message-ID:\\|^Nf-ID:\\|^Nf-From:\\|^Approved:\\|^Sender:\\|^Received:\\|^Mail-from:"
  "All headers that match this regexp will be hidden.
Also see `gnus-visible-headers'.")

(defvar gnus-visible-headers "^From:\\|^Newsgroups:\\|^Subject:\\|^Date:\\|^Followup-To:\\|^Reply-To:\\|^Organization:\\|^Summary:\\|^Keywords:\\|^To:\\|^Cc:"
  "All headers that do not match this regexp will be hidden.
Also see `gnus-ignored-headers'.")

(defvar gnus-sorted-header-list
  '("^From:" "^Subject:" "^Summary:" "^Keywords:" "^Newsgroups:" "^To:" 
    "^Cc:" "^Date:" "^Organization:")
  "This variable is a list of regular expressions.
If it is non-nil, headers that match the regular expressions will
be placed first in the article buffer in the sequence specified by
this list.")

(defvar gnus-required-headers
  '(From Date Newsgroups Subject Message-ID Organization Lines X-Newsreader)
  ;; changed by jwz because it's not so nice to do "Lines: 0" by default.
  ;; and to remove Path, since it's incorrect for Gnus to try
  ;; and generate that - it is the responsibility of inews or nntpd.
  "All required headers for articles you post.
RFC977 and RFC1036 require From, Date, Newsgroups, Subject, Message-ID
and Path headers.  Organization, Lines and X-Newsreader are optional.
If you want Gnus not to insert some header, remove it from this
variable.")

(defvar gnus-show-all-headers nil
  "*Show all headers of an article if non-nil.")

(defvar gnus-save-all-headers t
  "*Save all headers of an article if non-nil.")

(defvar gnus-inhibit-startup-message nil
  "The startup message will not be displayed if this function is non-nil.")

(defvar gnus-auto-extend-newsgroup t
  "Extend visible articles to forward and backward if non-nil.")

(defvar gnus-auto-select-first t
  "Select the first unread article automagically if non-nil.
If you want to prevent automatic selection of the first unread article
in some newsgroups, set the variable to nil in `gnus-select-group-hook'
or `gnus-apply-kill-hook'.")

(defvar gnus-auto-select-next t
  "Select the next newsgroup automagically if non-nil.
If the value is t and the next newsgroup is empty, Gnus will exit
summary mode and go back to group mode.  If the value is neither nil
nor t, Gnus will select the following unread newsgroup.  Especially, if
the value is the symbol `quietly', the next unread newsgroup will be
selected without any confirmations.")

(defvar gnus-auto-select-same nil
  "Select the next article with the same subject automagically if non-nil.")

(defvar gnus-auto-center-summary t
  "*Always center the current summary in Gnus summary window if non-nil.")

(defvar gnus-auto-mail-to-author nil
  "Insert `To: author' of the article when following up if non-nil.
If this variable is `ask', the user will be prompted.
Mail is sent using the function specified by the variable
`gnus-mail-send-method'.")

(defvar gnus-break-pages t
  "*Break an article into pages if non-nil.
Page delimiter is specified by the variable `gnus-page-delimiter'.")

(defvar gnus-page-delimiter "^\^L"
  "Regexp describing line-beginnings that separate pages of news article.")

(defvar gnus-use-full-window t
  "*Non-nil means to take up the entire screen of Emacs.")

(defvar gnus-window-configuration
  '((summary (0 1 0))
    (newsgroups (1 0 0))
    (article (0 3 10)))
  "Specify window configurations for each action.
The format of the variable is either a list of (ACTION (G S A)), where
G, S, and A are the relative height of group, summary, and article
windows, respectively, or a list of (ACTION FUNCTION), where FUNCTION
is a function that will be called with ACTION as an argument. ACTION
can be `summary', `newsgroups', or `article'.")

(defvar gnus-show-mime-method (function metamail-buffer)
  "Function to process a MIME message.
The function is expected to process current buffer as a MIME message.")

(defvar gnus-mail-reply-method
  (function gnus-mail-reply-using-mail)
  "Function to compose reply mail.
The function `gnus-mail-reply-using-mail' uses usual sendmail mail
program.  The function `gnus-mail-reply-using-mhe' uses the MH-E mail
program.  You can use yet another program by customizing this variable.")

(defvar gnus-mail-forward-method
  (function gnus-mail-forward-using-mail)
  "Function to forward current message to another user.
The function `gnus-mail-reply-using-mail' uses usual sendmail mail
program.  You can use yet another program by customizing this variable.")

(defvar gnus-mail-other-window-method
  (function gnus-mail-other-window-using-mail)
  "Function to compose mail in other window.
The function `gnus-mail-other-window-using-mail' uses the usual sendmail
mail program.  The function `gnus-mail-other-window-using-mhe' uses the MH-E
mail program.  You can use yet another program by customizing this variable.")

(defvar gnus-mail-send-method send-mail-function
  "Function to mail a message too which is being posted as an article.
The message must have To or Cc header.  The default is copied from
the variable `send-mail-function'.")

(defvar gnus-subscribe-newsgroup-method
  (function gnus-subscribe-zombies)
  "Function called with a newsgroup name when new newsgroup is found.
The function `gnus-subscribe-randomly' inserts a new newsgroup a the
beginning of newsgroups.  The function `gnus-subscribe-alphabetically'
inserts it in strict alphabetic order.  The function
`gnus-subscribe-hierarchically' inserts it in hierarchical newsgroup
order.  The function `gnus-subscribe-interactively' asks for your decision.")

;; Suggested by a bug report by Hallvard B Furuseth
;; <h.b.furuseth@usit.uio.no>. 
(defvar gnus-subscribe-options-newsgroup-method
  (function gnus-subscribe-alphabetically)
  "This function is called to subscribe newsgroups mentioned on \"options -n\" lines.
If, for instance, you want to subscribe to all newsgroups in the
\"no\" and \"alt\" hierarchies, you'd put the following in your
.newsrc file:

options -n no.all alt.all

Gnus will the subscribe all new newsgroups in these hierarchies with
the subscription method in this variable.")

;; Mark variables suggested by Thomas Michanek
;; <Thomas.Michanek@telelogic.se>. 
(defvar gnus-unread-mark ? 
  "Mark used for unread articles.")
(defvar gnus-ticked-mark ?!
  "Mark used for ticked articles.")
(defvar gnus-dormant-mark ??
  "Mark used for dormant articles.")
(defvar gnus-dread-mark ?D
  "Mark used for read articles.")
(defvar gnus-read-mark ?d
  "Mark used for read articles.")
(defvar gnus-expirable-mark ?E
  "Mark used for expirable articles.")
(defvar gnus-killed-mark ?K
  "Mark used for killed articles.")
(defvar gnus-kill-file-mark ?X
  "Mark used for articles killed by kill files.")
(defvar gnus-low-score-mark ?Y
  "Mark used for articles with a low score.")
(defvar gnus-catchup-mark ?C
  "Mark used for articles that are caught up.")
(defvar gnus-replied-mark ?R
  "Mark used for articles that have been replied to.")
(defvar gnus-process-mark ?#
  "Mark used for marking articles as processable.")
(defvar gnus-ancient-mark ?A
  "Mark used for ancient articles.")
(defvar gnus-canceled-mark ?G
  "Mark used for cancelled articles.")
(defvar gnus-score-over-mark ?+
  "Score mark used for articles with high scores.")
(defvar gnus-score-below-mark ?-
  "Score mark used for articles with low scores.")

(defvar gnus-view-pseudo-asynchronously nil
  "*If non-nil, Gnus will view pseudo-articles asynchronously.")

(defvar gnus-view-pseudos nil
  "If `automatic', pseudo-articles will be viewed automatically.
If `not-confirm', pseudos will be viewed automatically, and the user
will not be asked to confirm the command.")

(defvar gnus-group-mode-hook nil
  "A hook for Gnus group mode.")

(defvar gnus-summary-mode-hook nil
  "A hook for Gnus summary mode.")

(defvar gnus-article-mode-hook nil
  "A hook for Gnus article mode.")

(defvar gnus-kill-file-mode-hook nil
  "A hook for Gnus KILL File mode.")

(defvar gnus-open-server-hook nil
  "A hook called just before opening connection to news server.")

(defvar gnus-startup-hook nil
  "A hook called at startup time.
This hook is called after Gnus is connected to the NNTP server. So, it
is possible to change the behavior of Gnus according to the selected
NNTP server.")

(defvar gnus-group-prepare-function 'gnus-group-prepare-flat
  "A function that is called to set up the group buffer.
The function is called with three arguments: The first is a number;
all group with a level less or equal to that number should be listed,
if the second is non-nil, empty groups should also be displayed. If
the third is non-nil, it is a number. No groups with a level lower
than this number should be displayed.")

(defvar gnus-group-prepare-hook nil
  "A hook called after the newsgroup list is created in the group buffer.
If you want to modify the group buffer, you can use this hook.")

(defvar gnus-summary-prepare-hook nil
  "A hook called after summary list is created in the summary buffer.
If you want to modify the summary buffer, you can use this hook.")

(defvar gnus-article-prepare-hook nil
  "A hook called after an article is prepared in the article buffer.
If you want to run a special decoding program like nkf, use this hook.")

(defvar gnus-article-display-hook nil
  "A hook called after the article is displayed in the article buffer.
The hook is designed to change the contents of the article
buffer. Typical functions that this hook may contain are
`gnus-article-hide-headers' (hide selected headers),
`gnus-article-hide-signature' (hide signature) and
`gnus-article-treat-overstrike' (turn \"^H_\" into bold characters).")
(add-hook 'gnus-article-display-hook 'gnus-article-hide-headers-if-wanted)
(add-hook 'gnus-article-display-hook 'gnus-article-treat-overstrike)

(defvar gnus-select-group-hook nil
  "A hook called when a newsgroup is selected.

If you'd like to simplify subjects like the
`gnus-summary-next-same-subject' command does, you can use the
following hook:

 (setq gnus-select-group-hook
      (list
	(lambda ()
	  (mapcar (lambda (header)
		     (header-set-subject
		      header
		      (gnus-simplify-subject
		       (header-subject header) 're-only)))
		  gnus-newsgroup-headers))))
")

(defvar gnus-select-article-hook
  '(gnus-summary-show-thread)
  "A hook called when an article is selected.
The default hook shows conversation thread subtrees of the selected
article automatically using `gnus-summary-show-thread'.")

(defvar gnus-apply-kill-hook '(gnus-apply-kill-file)
  "A hook called when a newsgroup is selected and summary list is prepared.
This hook is intended to apply a kill file to the selected newsgroup.
The function `gnus-apply-kill-file' is called by default.

Since a general kill file is too heavy to use only for a few
newsgroups, I recommend you to use a lighter hook function. For
example, if you'd like to apply a kill file to articles which contains
a string `rmgroup' in subject in newsgroup `control', you can use the
following hook:

\(setq gnus-apply-kill-hook
      (list
	(lambda ()
	  (cond ((string-match \"control\" gnus-newsgroup-name)
		 (gnus-kill \"Subject\" \"rmgroup\")
		 (gnus-expunge \"X\"))))))")

(defvar gnus-visual-mark-article-hook 
  (list 'gnus-visual-highlight-selected-summary)
  "Hook run after selecting an article in the summary buffer.
It is meant to be used for highlighting the article in some way. It is
not run if `gnus-visual' is nil.")

(defvar gnus-prepare-article-hook (list (function gnus-inews-insert-signature))
  "A hook called after preparing body, but before preparing header headers.
The default hook (`gnus-inews-insert-signature') inserts a signature
file specified by the variable `gnus-signature-file'.")

(defvar gnus-inews-article-hook (list (function gnus-inews-do-fcc))
  "A hook called before finally posting an article.
The default hook (`gnus-inews-do-fcc') does FCC processing (save article
to a file).")

(defvar gnus-inews-article-header-hook nil
  "A hook called after inserting the headers in an article to be posted.
The hook is called from the *post-news* buffer, narrowed to the
headers.")

(defvar gnus-exit-group-hook nil
  "A hook called when exiting (not quitting) summary mode.
If your machine is so slow that exiting from summary mode takes very
long time, set the variable `gnus-use-cross-reference' to nil. This
inhibits marking articles as read using cross-reference information.")

(defvar gnus-suspend-gnus-hook nil
  "A hook called when suspending (not exiting) Gnus.")

(defvar gnus-exit-gnus-hook (list 'nntp-request-close)
  "A hook called when exiting Gnus.")

(defvar gnus-save-newsrc-hook nil
  "A hook called when saving the newsrc file.
This hook is called before saving the `.newsrc' file.")

(defvar gnus-auto-expirable-newsgroups nil
  "All newsgroups that match this regexp will have all read articles automatically marked as expirable.")

(defvar gnus-subscribe-hierarchical-interactive nil
  "If non-nil, Gnus will offer to subscribe hierarchically.
When a new hierarchy appears, Gnus will ask the user:

'alt.binaries': Do you want to subscribe to this hierarchy? ([d]ys):

If the user pressed `d', Gnus will descend the hierarchy, `y' will
subscribe to all newsgroups in the hierarchy and `s' will skip this
hierarchy in its entirety.")

(defvar gnus-group-line-format "%M%S%5y: %(%g%)\n"
  "Format of groups lines.
It works along the same lines as a normal formatting string,
with some simple extrensions.

%M    Only marked articles (character, \"*\" or \" \")
%S    Whether the group is subscribed (character, \"U\", \"K\", \"Z\" or \" \")
%L    Level of subscribedness (integer, 1-9)
%N    Number of unread articles (integer)
%I    Number of dormant articles (integer)
%i    Number of ticked and dormant (integer)
%T    Number of ticked articles (integer)
%R    Number of read articles (integer)
%t    Total number of articles (integer)
%y    Number of unread, unticked articles (integer)
%G    Group name (string)
%g    Qualified group name (string)
%D    Group description (string)
%s    Select method (string)
%o    Moderated group (char, \"m\")
%O    Moderated group (string, \"(m)\" or \"\")
%n    Select from where (string)
%z    A string that look like `<%s:%n>' if a foreign select method is used
%u    User defined specifier. The next character in the format string should
      be a letter.  GNUS will call the function gnus-user-format-function-X,
      where X is the letter following %u. The function will be passed the
      current header as argument. The function should return a string, which
      will be inserted into the summary just like information from any other
      summary specifier.

Text between %( and %) will be highlighted with `gnus-mouse-face' when
the mouse point move inside the area.  There can only be one such area.

Note that this format specification is not always respected. For
reasons of efficiency, when listing killed groups, this specification
is ignored altogether. If the spec is changed considerably, your
output may end up looking strange when listing both alive and killed
groups.

If you use %o or %O, reading the active file will be slower and quite
a bit of extra memory will be used. %D will also worsen performance.
Also note that if you change the format specification to include any
of these specs, you must probably re-start Gnus to see them go into
effect.") 

(defvar gnus-summary-line-format "%U%R%z%I%(%[%4L: %-20,20n%]%) %s\n"
  "The format specification of the lines in the summary buffer.

It works along the same lines as a normal formatting string,
with some simple extensions.

%N   Article number, left padded with spaces (string)
%S   Subject (string)
%s   Subject if it is at the root of a thread, and \"\" otherwise (string)
%n   Name of the poster (string)
%A   Address of the poster (string)
%L   Number of lines in the article (integer)
%c   Number of characters in the article (integer)
%D   Date of the article (string)
%I   Indentation based on thread level (a string of spaces)
%T   A string with two possible values: 80 spaces if the article
     is on thread level two or larger and 0 spaces on level one
%U   Status of this article (character, \"D\", \"K\", \"-\" or \" \") 
%[   Opening bracket (character, \"[\" or \"<\")
%]   Closing bracket (character, \"]\" or \">\")
%>   Spaces of length thread-level (string)
%<   Spaces of length (- 20 thread-level) (string)
%i   Article score (number)
%z   Article zcore (character)
%u   User defined specifier. The next character in the format string should
     be a letter.  GNUS will call the function gnus-user-format-function-X,
     where X is the letter following %u. The function will be passed the
     current header as argument. The function should return a string, which
     will be inserted into the summary just like information from any other
     summary specifier.

Text between %( and %) will be highlighted with `gnus-mouse-face'
when the mouse point is placed inside the area.  There can only be one
such area.

The %U (status), %R (replied) and %z (zcore) specs have to be handled
with care. For reasons of efficiency, Gnus will compute what column
these characters will end up in, and "hard-code" that. This means that
it is illegal to have these specs after a variable-length spec. Well,
you might not be arrested, but your summary buffer will look strange,
which is bad enough.

The smart choice is to have these specs as for to the left as
possible. 

This restriction may disappear in later versions of Gnus.")

(defconst gnus-summary-dummy-line-format "*   :                          : %S\n"
  "The format specification for the dummy roots in the summary buffer.
It works along the same lines as a normal formatting string,
with some simple extensions.

%S  The subject")

(defvar gnus-summary-mode-line-format "(ding) %G/%A %Z"
  "The format specification for the summary mode line.")

(defvar gnus-article-mode-line-format "(ding) %G/%A %S"
  "The format specification for the article mode line.")

(defconst gnus-group-mode-line-format "(ding) List of groups   {%M:%S}"
  "The format specification for the group mode line.")



;; Site dependent variables. You have to define these variables in
;;  site-init.el, default.el or your .emacs.

(defvar gnus-local-timezone nil
  "Local time zone.
This value is used only if `current-time-zone' does not work in your Emacs.
It specifies the GMT offset, i.e. a decimal integer
of the form +-HHMM giving the hours and minutes ahead of (i.e. east of) GMT.
For example, +0900 should be used in Japan, since it is 9 hours ahead of GMT.

For backwards compatibility, it may also be a string like \"JST\",
but strings are obsolescent: you should use numeric offsets instead.")

(defvar gnus-local-domain nil
  "Local domain name without a host name.
The DOMAINNAME environment variable is used instead if it is defined.
If the `system-name' function returns the full Internet name, there is
no need to set this variable.")

(defvar gnus-local-organization nil
  "String with a description of what organization (if any) the user belongs to.
The ORGANIZATION environment variable is used instead if it is defined.
If this variable contains a function, this function will be called
with the current newsgroup name as the argument. The function should
return a string.
In any case, if the string (either in the variable, in the environment
variable, or returned by the function) is a file name, the contents of
this file will be used as the organization.")

(defvar gnus-use-generic-from nil
  "If nil, the full host name will be the system name prepended to the domain name.
If this is a string, the full host name will be this string.
If this is non-nil, non-string, the domain name will be used as the
full host name.")

(defvar gnus-use-generic-path nil
  "If nil, use the NNTP server name in the Path header.
If stringp, use this; if non-nil, use no host name (user name only).")

(defvar gnus-valid-select-methods
  '(("nntp" post address) ("nnspool" post) ("nnvirtual" none virtual) 
    ("nnmbox" mail respool) ("nnml" mail respool)
    ("nnmh" mail respool) ("nndir" none) ("nndigest" none)
    ("nndoc" none) ("nnbabyl" mail respool)
    ("nnkiboze" none virtual))
  "A list of valid select methods.
Each element in this list should be a list. The first element of these
lists should be a string with the name of the select method. The
other elements may be be the category of this method (ie. `post',
`mail', `none' or whatever) or other properties that this method has
 (like being respoolable). 
If you implement a new select method, all you should have to change is
this variable. I think.")

(defvar gnus-updated-mode-lines '(group article summary)
  "This variable is a list of buffers that should keep their mode lines updated.
The list may contain the symbols `group', `article' and `summary'. If
the corresponding symbol is present, Gnus will keep that mode line
updated with information that may be pertinent. 
If this variable is nil, screen refresh may be quicker.")

;; Added by Per Abrahamsen <amanda@iesd.auc.dk>.
(defvar gnus-mouse-face 'highlight
  "Face used for mouse highlighting in Gnus.
No mouse highlights will be done if `gnus-visual' is nil.")

(defvar gnus-visual-summary-update-hook 
  (list 'gnus-visual-summary-highlight-line)
  "A hook called when a summary line is changed.
The hook will not be called if `gnus-visual' is nil.

Point will be at the beginning of the line, and the following free
variables can be used for convenience:

score:   (gnus-summary-article-score)
default: gnus-summary-default-score
below:   gnus-summary-mark-below

The default hook `gnus-visual-summary-highlight-line' will highlight the line
according to the `gnus-visual-summary-highlight' variable.")

(defvar gnus-summary-mark-below nil
  "Score below which articles automatically become marked.
This variable is local to each summary buffer and usually set in the
score file.")  

(defvar gnus-thread-sort-functions '(gnus-thread-sort-by-number)
  "List of functions used for thread roots in the summary buffer.

Each function takes two threads and return non-nil if the first thread
should be sorted before the other.  If you use more than one function,
list the function you want to act as the primary sort key last.

Functions you can use are:
- gnus-thread-sort-by-number
- gnus-thread-sort-by-author
- gnus-thread-sort-by-subject
- gnus-thread-sort-by-date
- gnus-thread-sort-by-score
- gnus-thread-sort-by-total-score (see `gnus-thread-score-function').

The two later only works on articles that have already been scored prior
to entering the newsgroup.")

(defvar gnus-thread-score-function '+
  "Function used for calculating the total score of a thread.

The function is called with the scores of the article and each
subthread and should then return the score of the thread.

Some functions you can use are `+', `max', or `min'.")

(defvar gnus-score-find-score-files-function 'gnus-score-find-bnews
  "Function used to find SCORE files.
The value should be a function that takes a group name as an argument,
and return a list of SCORE files that applies to that group.

Predefined values are:

gnus-score-find-single: Only apply the groups own SCORE file.
gnus-score-find-hierarchical: Also apply SCORE files from parent groups.
gnus-score-find-bnews: Apply SCORE files whose names matches.

See the definition of the individual functions for more information.")

(defvar gnus-options-subscribe nil
  "All new groups matching this regexp will be subscribed.
Note that this variable deals only with new newsgroups. This variable
does not affect old newsgroups.")

(defvar gnus-options-not-subscribe nil
  "All new groups matching this regexp will be ignored.
Note that this variable deals only with new newsgroups. This variable
does not affect old (already subscribed) newsgroups.")


;; Internal variables

;; Avoid highlighting in kill files.
(defvar gnus-summary-inhibit-highlight nil)

(defvar caesar-translate-table nil)

(defvar gnus-dribble-buffer nil)
(defvar gnus-headers-retrieved-by nil)

(defvar gnus-article-reply nil)
(defvar gnus-article-check-size nil)

(defvar gnus-score-file-list nil)
(defvar gnus-score-alist nil
  "Alist containing score information.
The keys can be symbols or strings.  The following symbols are defined. 

touched: If this alist has been modified.
mark:    Automatically mark articles below this.
expunge: Automatically expunge articles below this.
files:   List of other SCORE files to load when loading this one.
eval:    Sexp to be evaluated when the score file is loaded.

String entries have the form (HEADER (MATCH TYPE SCORE DATE) ...) 
where HEADER is the header being scored, MATCH is the string we are
looking for, TYPE is a flag indicating whether it should use regexp or
substring matching, SCORE is the score to add and DATE is the date
of the last succesful match.")

(defvar gnus-score-cache nil)
;; Alist containing the content of all loaded SCORE files.
(defvar gnus-scores-lists nil)
(defvar gnus-scores-articles nil)
(defvar gnus-header-index nil)
(defvar gnus-score-index nil)

(defvar gnus-newsgroup-dependencies nil)

(defconst gnus-group-edit-buffer "*Gnus edit newsgroup*")

(defvar gnus-default-subscribe-level 2
  "Default subscription level.")

(defvar gnus-default-unsubscribe-level 6
  "Default unsubscription level.")

(defvar gnus-default-kill-level 9
  "Default kill level.")

(defconst gnus-group-line-format-alist
  (list (list ?M 'marked ?c)
	(list ?S 'subscribed ?c)
	(list ?L 'level ?d)
	(list ?N 'number ?s)
	(list ?I 'number-of-dormant ?d)
	(list ?T 'number-of-ticked ?d)
	(list ?R 'number-of-read ?s)
	(list ?t 'number-total ?d)
	(list ?y 'number-of-unread-unticked ?s)
	(list ?i 'number-of-ticked-and-dormant ?d)
	(list ?g 'group ?s)
	(list ?G 'qualified-group ?s)
	(list ?D 'newsgroup-description ?s)
	(list ?o 'moderated ?c)
	(list ?O 'moderated-string ?s)
	(list ?s 'news-server ?s)
	(list ?n 'news-method ?s)
	(list ?z 'news-method-string ?s)
	(list ?u 'user-defined ?s)))

(defconst gnus-summary-line-format-alist 
  (list (list ?N 'number ?s)
	(list ?S 'subject ?s)
	(list ?s 'subject-or-nil ?s)
	(list ?n 'name ?s)
	(list ?A 'address ?s)
	(list ?F 'from ?s)
	(list ?x (macroexpand '(header-xref header)) ?s)
	(list ?D (macroexpand '(header-date header)) ?s)
	(list ?M (macroexpand '(header-id header)) ?s)
	(list ?r (macroexpand '(header-references header)) ?s)
	(list ?c (macroexpand '(header-chars header)) ?d)
	(list ?L 'lines ?d)
	(list ?I 'indentation ?s)
	(list ?T '(if (< level 1) "" (make-string (frame-width) ? )) ?s)
	(list ?R 'replied ?c)
	(list ?\[ 'opening-bracket ?c)
	(list ?\] 'closing-bracket ?c)
	(list ?\> '(make-string level ? ) ?s)
	(list ?\< '(make-string (max 0 (- 20 level)) ? ) ?s)
	(list ?i 'score ?s)
	(list ?z 'score-char ?c)
	(list ?U 'unread ?c)
	(list ?u 'user-defined ?s))
  "An alist of format specifications that can appear in summary lines,
and what variables they correspond with, along with the type of the
variable (string, integer, character, etc).")

(defconst gnus-summary-dummy-line-format-alist
  (list (list ?S 'subject ?s)
	(list ?N 'number ?d)))

(defconst gnus-summary-mode-line-format-alist 
  (list (list ?G 'group-name ?s)
	(list ?A 'article-number ?d)
	(list ?Z 'unread-and-unselected ?s)
	(list ?V 'gnus-version ?s)
	(list ?U 'unread ?d)
	(list ?S 'subject ?s)
	(list ?u 'unselected ?d)))

(defconst gnus-group-mode-line-format-alist 
  (list (list ?S 'news-server ?s)
	(list ?M 'news-method ?s)))

(defvar gnus-have-read-active-file nil)

(defconst gnus-maintainer "Lars Magne Ingebrigtsen <larsi@ifi.uio.no>"
  "The mail address of the Gnus maintainer.")

(defconst gnus-version "(ding) Gnus v0.30"
  "Version number for this version of Gnus.")

(defvar gnus-info-nodes
  '((gnus-group-mode		"(gnus)Group Commands")
    (gnus-summary-mode		"(gnus)Summary Commands")
    (gnus-article-mode		"(gnus)Article Commands")
    (gnus-kill-file-mode	"(gnus)Kill File"))
  "Assoc list of major modes and related Info nodes.")

(defvar gnus-documentation-group-file "~/dgnus/lisp/doc.txt"
  "The location of the (ding) Gnus documentation group.")

(defvar gnus-group-buffer "*Group*")
(defvar gnus-summary-buffer "*Summary*")
(defvar gnus-article-buffer "*Article*")

(defvar gnus-buffer-list nil
  "Gnus buffers that should be killed when exiting.")

(defvar gnus-variable-list
  '(gnus-newsrc-options 
    gnus-newsrc-options-n-yes gnus-newsrc-options-n-no
    gnus-newsrc-last-checked-date
    gnus-newsrc-assoc gnus-killed-list gnus-zombie-list)
  "Gnus variables saved in the quick startup file.")

(defvar gnus-overload-functions
  '((news-inews gnus-inews-news "rnewspost")
    (caesar-region gnus-caesar-region "rnews"))
  "Functions overloaded by gnus.
It is a list of `(original overload &optional file)'.")

(defvar gnus-newsrc-options nil
  "Options line in the .newsrc file.")

(defvar gnus-newsrc-options-n-yes nil
  "Regexp representing groups to be subscribed to unconditionally.")

(defvar gnus-newsrc-options-n-no nil
  "Regexp representing group to be ignored unconditionally.")

(defvar gnus-newsrc-last-checked-date nil
  "Date Gnus last asked server for new newsgroups.")

(defvar gnus-newsrc-assoc nil
  "Assoc list of read articles.
gnus-newsrc-hashtb should be kept so that both hold the same information.")

(defvar gnus-newsrc-hashtb nil
  "Hashtable of gnus-newsrc-assoc.")

(defvar gnus-killed-list nil
  "List of killed newsgroups.")

(defvar gnus-killed-hashtb nil
  "Hash table equivalent of gnus-killed-list.")

(defvar gnus-zombie-list nil
  "List of almost dead newsgroups.")

(defvar gnus-description-hashtb nil
  "Descriptions of newsgroups.")

(defvar gnus-list-of-killed-groups nil
  "List of newsgroups that have recently been killed by the user.")

(defvar gnus-active-hashtb nil
  "Hashtable of active articles.")

(defvar gnus-moderated-list nil
  "List of moderated newsgroups.")

(defvar gnus-current-startup-file nil
  "Startup file for the current host.")

(defvar gnus-last-search-regexp nil
  "Default regexp for article search command.")

(defvar gnus-last-shell-command nil
  "Default shell command on article.")

(defvar gnus-current-select-method nil
  "The current method for selecting a newsgroup.")

(defvar gnus-have-all-newsgroups nil)

(defvar gnus-article-internal-prepare-hook nil)

(defvar gnus-newsgroup-name nil)
(defvar gnus-newsgroup-begin nil)
(defvar gnus-newsgroup-end nil)
(defvar gnus-newsgroup-last-rmail nil)
(defvar gnus-newsgroup-last-mail nil)
(defvar gnus-newsgroup-last-folder nil)
(defvar gnus-newsgroup-last-file nil)
(defvar gnus-newsgroup-auto-expire nil
  "If non-nil, all read articles will be marked as expirable.")

(defvar gnus-newsgroup-selected-overlay nil)

(defvar gnus-newsgroup-unreads nil
  "List of unread articles in the current newsgroup.")

(defvar gnus-newsgroup-unselected nil
  "List of unselected unread articles in the current newsgroup.")

(defvar gnus-newsgroup-marked nil
  "List of ticked articles in the current newsgroup (a subset of unread art).")

(defvar gnus-newsgroup-killed nil
  "List of ranges of articles that have been through the scoring process.")

(defvar gnus-newsgroup-kill-headers nil)

(defvar gnus-newsgroup-replied nil
  "List of articles that have been replied to in the current newsgroup.")

(defvar gnus-newsgroup-expirable nil
  "List of articles in the current newsgroup that can be expired.")

(defvar gnus-newsgroup-processable nil
  "List of articles in the current newsgroup that can be processed.")

(defvar gnus-newsgroup-bookmarks nil
  "List of articles in the current newsgroup that have bookmarks.")

(defvar gnus-newsgroup-dormant nil
  "List of dormant articles in the current newsgroup.")

(defvar gnus-newsgroup-scored nil
  "List of scored articles in the current newsgroup.")

(defvar gnus-newsgroup-headers nil
  "List of article headers in the current newsgroup.")
(defvar gnus-newsgroup-headers-hashtb-by-number nil)

(defvar gnus-newsgroup-ancient nil
  "List of `gnus-fetch-old-headers' articles in the current newsgroup.")

(defvar gnus-current-article nil)
(defvar gnus-article-current nil)
(defvar gnus-current-headers nil)
(defvar gnus-have-all-headers nil "Must be either T or NIL.")
(defvar gnus-last-article nil)
(defvar gnus-current-kill-article nil)
(defvar gnus-newsgroup-dormant-subjects nil)
(defvar gnus-newsgroup-expunged-buffer nil)

;; Save window configuration.
(defvar gnus-winconf-kill-file nil)

(defconst gnus-group-mode-map nil)
(defvar gnus-article-mode-map nil)
(defvar gnus-kill-file-mode-map nil)

;; Format specs
(defvar gnus-summary-line-format-spec nil)
(defvar gnus-summary-dummy-line-format-spec nil)
(defvar gnus-group-line-format-spec nil)
(defvar gnus-summary-mode-line-format-spec nil)
(defvar gnus-article-mode-line-format-spec nil)
(defvar gnus-group-mode-line-format-spec nil)
(defvar gnus-summary-expunge-below nil)
(defvar gnus-reffed-article-number nil)

(defvar gnus-summary-mark-positions nil)

(defvar rmail-default-file (expand-file-name "~/XMBOX"))
(defvar rmail-default-rmail-file (expand-file-name "~/XNEWS"))

(defconst gnus-summary-local-variables 
  '(gnus-newsgroup-name 
    gnus-newsgroup-begin gnus-newsgroup-end 
    gnus-newsgroup-last-rmail gnus-newsgroup-last-mail 
    gnus-newsgroup-last-folder gnus-newsgroup-last-file 
    gnus-newsgroup-auto-expire gnus-newsgroup-unreads 
    gnus-newsgroup-unselected gnus-newsgroup-marked
    gnus-newsgroup-replied gnus-newsgroup-expirable
    gnus-newsgroup-processable gnus-newsgroup-killed
    gnus-newsgroup-bookmarks gnus-newsgroup-dormant
    gnus-newsgroup-dormant-subjects gnus-newsgroup-expunged-buffer
    gnus-newsgroup-headers gnus-newsgroup-headers-hashtb-by-number
    gnus-current-article gnus-current-headers gnus-have-all-headers
    gnus-last-article gnus-article-internal-prepare-hook
    gnus-newsgroup-selected-overlay gnus-newsgroup-dependencies
    gnus-newsgroup-scored gnus-newsgroup-kill-headers
    gnus-score-alist gnus-summary-expunge-below 
    gnus-summary-mark-below gnus-newsgroup-ancient)
  "Variables that are buffer-local to the summary buffers.")

(defvar gnus-mark-article-hook
  (list
   (lambda ()
     (or (memq gnus-current-article gnus-newsgroup-marked)
	 (memq gnus-current-article gnus-newsgroup-dormant)
	 (memq gnus-current-article gnus-newsgroup-expirable)
	 (gnus-summary-mark-as-read gnus-current-article gnus-read-mark))))
  "A hook called when an article is selected at the first time.
The hook is intended to mark an article as read (or unread)
automatically when it is selected.

If you'd like to tick articles instead, use the following hook:

\(setq gnus-mark-article-hook
      (list
        (lambda ()
	  (gnus-summary-tick-article gnus-current-article))))")

;; Define some autoload functions Gnus may use.
(eval-and-compile
  (autoload 'metamail-buffer "metamail")
  (autoload 'Info-goto-node "info")
  
  (autoload 'timezone-make-date-arpa-standard "timezone")
  (autoload 'timezone-fix-time "timezone")
  (autoload 'timezone-make-sortable-date "timezone")
  (autoload 'timezone-make-time-string "timezone")
  
  (autoload 'rmail-output "rmailout"
    "Append this message to Unix mail file named FILE-NAME." t)
  (autoload 'mail-position-on-field "sendmail")
  (autoload 'mail-setup "sendmail")

  (autoload 'gnus-mail-reply-using-mhe "gnus-mh")
  (autoload 'gnus-mail-forward-using-mhe "gnus-mh")
  (autoload 'gnus-mail-other-window-using-mhe "gnus-mh")
  (autoload 'gnus-summary-save-in-folder "gnus-mh")
  (autoload 'gnus-Folder-save-name "gnus-mh")
  (autoload 'gnus-folder-save-name "gnus-mh")
  
  (autoload 'gnus-group-make-menu-bar "gnus-visual")
  (autoload 'gnus-summary-make-menu-bar "gnus-visual")
  (autoload 'gnus-article-make-menu-bar "gnus-visual")
  (autoload 'gnus-visual-highlight-selected-summary "gnus-visual")
  (autoload 'gnus-visual-summary-highlight-line "gnus-visual")

  (autoload 'gnus-uu-decode-map "gnus-uu" nil nil 'keymap)
  (autoload 'gnus-uu-mark-by-regexp "gnus-uu")
  (autoload 'gnus-uu-mark-region "gnus-uu")
  (autoload 'gnus-uu-mark-thread "gnus-uu")
  (autoload 'gnus-uu-mark-sparse "gnus-uu")
  (autoload 'gnus-uu-mark-series "gnus-uu")
  (autoload 'gnus-uu-mark-all "gnus-uu")
  (autoload 'gnus-uu-post-news "gnus-uu")
  (autoload 'gnus-uu-digest-and-forward "gnus-uu")

  (autoload 'gnus-uu-decode-uu "gnus-uu")
  (autoload 'gnus-uu-decode-uu-and-save "gnus-uu")
  (autoload 'gnus-uu-decode-unshar "gnus-uu")
  (autoload 'gnus-uu-decode-unshar-and-save "gnus-uu")
  (autoload 'gnus-uu-decode-save "gnus-uu")
  (autoload 'gnus-uu-decode-binhex "gnus-uu")
  (autoload 'gnus-uu-decode-uu-view "gnus-uu")
  (autoload 'gnus-uu-decode-uu-and-save-view "gnus-uu")
  (autoload 'gnus-uu-decode-unshar-view "gnus-uu")
  (autoload 'gnus-uu-decode-unshar-and-save-view "gnus-uu")
  (autoload 'gnus-uu-decode-save-view "gnus-uu")
  (autoload 'gnus-uu-decode-binhex-view "gnus-uu")

  (autoload 'pp "pp")
  )

(put 'gnus-group-mode 'mode-class 'special)
(put 'gnus-summary-mode 'mode-class 'special)
(put 'gnus-article-mode 'mode-class 'special)



;; Fix by Hallvard B Furuseth <h.b.furuseth@usit.uio.no>.
(defun gnus-summary-position-cursor () nil)
(defun gnus-group-position-cursor () nil)
(fset 'gnus-summary-position-cursor 'gnus-goto-colon)
(fset 'gnus-group-position-cursor 'gnus-goto-colon)

(defmacro gnus-eval-in-buffer-window (buffer &rest forms)
  "Pop to BUFFER, evaluate FORMS, and then returns to original window."
  (` (let ((GnusStartBufferWindow (selected-window)))
       (unwind-protect
	   (progn
	     (pop-to-buffer (, buffer))
	     (,@ forms))
	 (select-window GnusStartBufferWindow)))))

(defun gnus-make-hashtable (&optional hashsize)
  "Make a hash table (default and minimum size is 255).
Optional argument HASHSIZE specifies the table size."
  (make-vector (if hashsize 
		   (max (gnus-create-hash-size hashsize) 255)
		 255) 0))

(defmacro gnus-gethash (string hashtable)
  "Get hash value of STRING in HASHTABLE."
  ;;(` (symbol-value (abbrev-symbol (, string) (, hashtable))))
  ;;(` (abbrev-expansion (, string) (, hashtable)))
  (` (symbol-value (intern-soft (, string) (, hashtable)))))

(defmacro gnus-sethash (string value hashtable)
  "Set hash value. Arguments are STRING, VALUE, and HASHTABLE."
  ;; We cannot use define-abbrev since it only accepts string as value.
					;  (set (intern string hashtable) value))
  (` (set (intern (, string) (, hashtable)) (, value))))

(defsubst gnus-buffer-substring (beg end)
  (buffer-substring (match-beginning beg) (match-end end)))

(defsubst gnus-simplify-subject-re (subject)
  "Remove \"Re:\" from subject lines."
  (let ((case-fold-search t))
    (if (string-match "^re: *" subject)
	(substring subject (match-end 0))
      subject)))

(defsubst gnus-goto-char (point)
  (and point (goto-char point)))


;;;
;;; Gnus Utility Functions
;;;

(defun gnus-extract-address-components (from)
  (let (name address)
    (if (string-match "([^)]+)" from)
	(setq name (substring from (1+ (match-beginning 0)) 
			      (1- (match-end 0)))))
    ;; Fix by Hallvard B Furuseth <h.b.furuseth@usit.uio.no>.
    (if (string-match "\\b[^@ \t<>]+[!@][^@ \t<>]+\\b" from)
	(setq address (substring from (match-beginning 0) (match-end 0))))
    (if (and (not name) address)
	(if (string-match (concat "<" (regexp-quote address) ">") from)
	    (setq name (substring from 0 (1- (match-beginning 0))))))
    (list (or name from) (or address from))))

(defun gnus-fetch-field (field)
  "Return the value of the header FIELD of current article."
  (save-excursion
    (save-restriction
      (gnus-narrow-to-headers)
      (mail-fetch-field field))))

(defun gnus-goto-colon ()
  (beginning-of-line)
  (search-forward ":" (save-excursion (end-of-line) (point)) t))

(defun gnus-narrow-to-headers ()
  (widen)
  (save-excursion
    (goto-char 1)
    (if (search-forward "\n\n")
	(narrow-to-region 1 (1- (point))))))

;; Get a number that is suitable for hashing; bigger than MIN
(defun gnus-create-hash-size (min)
  (let ((i 1))
    (while (< i min)
      (setq i (* 2 i)))
    (1- i)))

(defun gnus-update-format-specifications ()
  (setq gnus-summary-line-format-spec 
	(gnus-parse-format
	 gnus-summary-line-format gnus-summary-line-format-alist))
  (gnus-update-summary-mark-positions)
  (setq gnus-summary-dummy-line-format-spec 
	(gnus-parse-format gnus-summary-dummy-line-format 
			   gnus-summary-dummy-line-format-alist))
  (setq gnus-group-line-format-spec
	(gnus-parse-format 
	 gnus-group-line-format 
	 gnus-group-line-format-alist))
  (if (and (string-match "%D" gnus-group-line-format)
	   (not gnus-description-hashtb)
	   gnus-read-active-file)
      (gnus-read-descriptions-file))
  (setq gnus-summary-mode-line-format-spec 
	(gnus-parse-format gnus-summary-mode-line-format 
			   gnus-summary-mode-line-format-alist))
  (setq gnus-article-mode-line-format-spec 
	(gnus-parse-format gnus-article-mode-line-format 
			   gnus-summary-mode-line-format-alist))
  (setq gnus-group-mode-line-format-spec 
	(gnus-parse-format gnus-group-mode-line-format 
			   gnus-group-mode-line-format-alist)))

(defun gnus-update-summary-mark-positions ()
  (save-excursion
    (let ((gnus-replied-mark 129)
	  (gnus-score-below-mark 130)
	  (gnus-score-over-mark 130)
	  pos)
      (set-buffer (get-buffer-create " *gnus work*"))
      (buffer-disable-undo (current-buffer))
      (erase-buffer)
      (gnus-summary-insert-line 
       nil ["" "" "" "" "" "" 0 0 ""]  0 nil 128 t nil "" nil 1)
      (goto-char (point-min))
      (setq pos (list (cons 'unread (and (search-forward "\200" nil t)
					 (- (point) 2)))))
      (goto-char (point-min))
      (setq pos (cons (cons 'replied (and (search-forward "\201" nil t)
					  (- (point) 2))) pos))
      (goto-char (point-min))
      (setq pos (cons (cons 'score (and (search-forward "\202" nil t)
					(- (point) 2))) pos))
      (setq gnus-summary-mark-positions pos)
      (kill-buffer (current-buffer)))))

(defun gnus-format-max-width (var length)
  (let (result)
    (if (> (length (setq result (eval var))) length)
	(format "%s" (substring result 0 length))
      (format "%s" result))))

(defun gnus-set-mouse-face (string)
  ;; Set mouse face property on STRING.
  (put-text-property 0 (length string) 'mouse-face gnus-mouse-face string)
  string)

(defun gnus-parse-format (format spec-alist)
  ;; This function parses the FORMAT string with the help of the
  ;; SPEC-ALIST and returns a list that can be eval'ed to return the
  ;; string.  If the FORMAT string contains the specifiers %( and %)
  ;; the text between them will have the mouse-face text property.
  (if (string-match "\\`\\(.*\\)%(\\(.*\\)%)\\(.*\n?\\)\\'" format)
      (if (and gnus-visual gnus-mouse-face)
	  (let ((pre (substring format (match-beginning 1) (match-end 1)))
		(button (substring format (match-beginning 2) (match-end 2)))
		(post (substring format (match-beginning 3) (match-end 3))))
	    (list 'concat
		  (gnus-parse-simple-format pre spec-alist)
		  (list 'gnus-set-mouse-face
			(gnus-parse-simple-format button spec-alist))
		  (gnus-parse-simple-format post spec-alist)))
	(gnus-parse-simple-format
	 (concat (substring format (match-beginning 1) (match-end 1))
		 (substring format (match-beginning 2) (match-end 2))
		 (substring format (match-beginning 3) (match-end 3)))
	 spec-alist))
    (gnus-parse-simple-format format spec-alist)))

(defun gnus-parse-simple-format (format spec-alist)
  ;; This function parses the FORMAT string with the help of the
  ;; SPEC-ALIST and returns a list that can be eval'ed to return the
  ;; string. The list will consist of the symbol `format', a format
  ;; specification string, and a list of forms depending on the
  ;; SPEC-ALIST.
  (let ((max-width 0)
	spec flist fstring b newspec max-width elem beg)
    (save-excursion
      (set-buffer (get-buffer-create "*gnus work*"))
      (buffer-disable-undo (current-buffer))
      (gnus-add-current-to-buffer-list)
      (erase-buffer)
      (insert format)
      (goto-char 1)
      (while (re-search-forward "%[-0-9]*\\(,[0-9]*\\)*\\(.\\)\\(.\\)?" nil t)
	(setq spec (string-to-char (buffer-substring (match-beginning 2)
						     (match-end 2))))
	;; First check if there are any specs that look anything like
	;; "%12,12A", ie. with a "max width specification". These have
	;; to be treated specially.
	(if (setq beg (match-beginning 1))
	    (setq max-width 
		  (string-to-int 
		   (buffer-substring (1+ (match-beginning 1)) (match-end 1))))
	  (setq max-width 0)
	  (setq beg (match-beginning 2)))
	;; Find the specification from `spec-alist'.
	(if (not (setq elem (cdr (assq spec spec-alist))))
	    (setq elem '("*" ?s)))
	;; Treat user defined format specifiers specially
	(and (eq (car elem) 'user-defined)
	     (setq elem
		   (list 
		    (list (intern (concat "gnus-user-format-function-"
					  (buffer-substring
					   (match-beginning 3)
					   (match-end 3))))
			  'header)
		    ?s))
	     (delete-region (match-beginning 3) (match-end 3)))
	(if (not (zerop max-width))
	    (let ((el (car elem)))
	      (cond ((= (car (cdr elem)) ?c) 
		     (setq el (list 'char-to-string el)))
		    ((= (car (cdr elem)) ?d)
		     (numberp el) (setq el (list 'int-to-string el))))
	      (setq flist (cons (list 'gnus-format-max-width 
				      el max-width) 
				flist))
	      (setq newspec ?s))
	  (setq flist (cons (car elem) flist))
	  (setq newspec (car (cdr elem))))
	;; Remove the old specification (and possibly a ",12" string).
	(delete-region beg (match-end 2))
	;; Insert the new specification.
	(goto-char beg)
	(insert newspec))
      (setq fstring (buffer-substring 1 (point-max))))
    (cons 'format (cons fstring (nreverse flist)))))

;; Suggested by Brian Edmonds <edmonds@cs.ubc.ca>.
(defun gnus-read-init-file ()
  (and gnus-init-file
       (or (file-exists-p gnus-init-file)
	   (file-exists-p (concat gnus-init-file ".el"))
	   (file-exists-p (concat gnus-init-file ".elc")))
       (load gnus-init-file nil t)))

;; Article file names when saving.

(defun gnus-Numeric-save-name (newsgroup headers &optional last-file)
  "Generate file name from NEWSGROUP, HEADERS, and optional LAST-FILE.
If variable `gnus-use-long-file-name' is nil, it is ~/News/News.group/num.
Otherwise, it is like ~/News/news/group/num."
  (let ((default
	  (expand-file-name
	   (concat (if gnus-use-long-file-name
		       (gnus-capitalize-newsgroup newsgroup)
		     (gnus-newsgroup-directory-form newsgroup))
		   "/" (int-to-string (header-number headers)))
	   (or gnus-article-save-directory "~/News"))))
    (if (and last-file
	     (string-equal (file-name-directory default)
			   (file-name-directory last-file))
	     (string-match "^[0-9]+$" (file-name-nondirectory last-file)))
	default
      (or last-file default))))

(defun gnus-numeric-save-name (newsgroup headers &optional last-file)
  "Generate file name from NEWSGROUP, HEADERS, and optional LAST-FILE.
If variable `gnus-use-long-file-name' is nil, it is ~/News/news.group/num.
Otherwise, it is like ~/News/news/group/num."
  (let ((default
	  (expand-file-name
	   (concat (if gnus-use-long-file-name
		       newsgroup
		     (gnus-newsgroup-directory-form newsgroup))
		   "/" (int-to-string (header-number headers)))
	   (or gnus-article-save-directory "~/News"))))
    (if (and last-file
	     (string-equal (file-name-directory default)
			   (file-name-directory last-file))
	     (string-match "^[0-9]+$" (file-name-nondirectory last-file)))
	default
      (or last-file default))))

(defun gnus-Plain-save-name (newsgroup headers &optional last-file)
  "Generate file name from NEWSGROUP, HEADERS, and optional LAST-FILE.
If variable `gnus-use-long-file-name' is nil, it is ~/News/News.group.
Otherwise, it is like ~/News/news/group/news."
  (or last-file
      (expand-file-name
       (if gnus-use-long-file-name
	   (gnus-capitalize-newsgroup newsgroup)
	 (concat (gnus-newsgroup-directory-form newsgroup) "/news"))
       (or gnus-article-save-directory "~/News"))))

(defun gnus-plain-save-name (newsgroup headers &optional last-file)
  "Generate file name from NEWSGROUP, HEADERS, and optional LAST-FILE.
If variable `gnus-use-long-file-name' is nil, it is ~/News/news.group.
Otherwise, it is like ~/News/news/group/news."
  (or last-file
      (expand-file-name
       (if gnus-use-long-file-name
	   newsgroup
	 (concat (gnus-newsgroup-directory-form newsgroup) "/news"))
       (or gnus-article-save-directory "~/News"))))

;; For subscribing new newsgroup

(defun gnus-subscribe-hierarchical-interactive (groups)
  (let ((groups (sort groups 'string<))
	prefixes prefix start rest ans group starts)
    (while groups
      (setq prefixes (list "^"))
      (while (and groups prefixes)
	(while (not (string-match (car prefixes) (car groups)))
	  (setq prefixes (cdr prefixes)))
	(setq prefix (car prefixes))
	(setq start (1- (length prefix)))
	(if (and (string-match "[^\\.]\\." (car groups) start)
		 (cdr groups)
		 (setq prefix 
		       (concat "^" (substring (car groups) 0 (match-end 0))))
		 (string-match prefix (car (cdr groups))))
	    (progn
	      (setq prefixes (cons prefix prefixes))
	      (message "Descend hierarchy %s? ([y]nsq): " 
		       (substring prefix 1 (1- (length prefix))))
	      (setq ans (read-char))
	      (cond ((= ans ?n)
		     (while (and groups 
				 (string-match prefix 
					       (setq group (car groups))))
		       (setq gnus-killed-list 
			     (cons group gnus-killed-list))
		       (gnus-sethash group group gnus-killed-hashtb)
		       (setq groups (cdr groups)))
		     (setq starts (cdr starts)))
		    ((= ans ?s)
		     (while (and groups 
				 (string-match prefix 
					       (setq group (car groups))))
		       (gnus-sethash group group gnus-killed-hashtb)
		       (gnus-subscribe-alphabetically (car groups))
		       (setq groups (cdr groups)))
		     (setq starts (cdr starts)))
		    ((= ans ?q)
		     (while groups
		       (setq group (car groups))
		       (setq gnus-killed-list (cons group gnus-killed-list))
		       (gnus-sethash group group gnus-killed-hashtb)
		       (setq groups (cdr groups))))
		    (t nil)))
	  (message "Subscribe %s? ([n]yq)" (car groups))
	  (setq ans (read-char))
	  (setq group (car groups))
	  (cond ((= ans ?y)
		 (gnus-subscribe-alphabetically (car groups))
		 (gnus-sethash group group gnus-killed-hashtb))
		((= ans ?q)
		 (while groups
		   (setq group (car groups))
		   (setq gnus-killed-list (cons group gnus-killed-list))
		   (gnus-sethash group group gnus-killed-hashtb)
		   (setq groups (cdr groups))))
		(t 
		 (setq gnus-killed-list (cons group gnus-killed-list))
		 (gnus-sethash group group gnus-killed-hashtb)))
	  (setq groups (cdr groups)))))))

(defun gnus-subscribe-randomly (newsgroup)
  "Subscribe new NEWSGROUP by making it the first newsgroup."
  (gnus-subscribe-newsgroup newsgroup))

(defun gnus-subscribe-alphabetically (newgroup)
  "Subscribe new NEWSGROUP and insert it in alphabetical order."
  ;; Basic ideas by mike-w@cs.aukuni.ac.nz (Mike Williams)
  (let ((groups (cdr gnus-newsrc-assoc))
	before)
    (while (and (not before) groups)
      (if (string< newgroup (car (car groups)))
	  (setq before (car (car groups)))
	(setq groups (cdr groups))))
    (gnus-subscribe-newsgroup newgroup before)))

(defun gnus-subscribe-hierarchically (newgroup)
  "Subscribe new NEWSGROUP and insert it in hierarchical newsgroup order."
  ;; Basic ideas by mike-w@cs.aukuni.ac.nz (Mike Williams)
  (save-excursion
    (set-buffer (find-file-noselect gnus-current-startup-file))
    (let ((groupkey newgroup)
	  before)
      (while (and (not before) groupkey)
	(goto-char (point-min))
	(let ((groupkey-re
	       (concat "^\\(" (regexp-quote groupkey) ".*\\)[!:]")))
	  (while (and (re-search-forward groupkey-re nil t)
		      (progn
			(setq before (buffer-substring
				      (match-beginning 1) (match-end 1)))
			(string< before newgroup)))))
	;; Remove tail of newsgroup name (eg. a.b.c -> a.b)
	(setq groupkey
	      (if (string-match "^\\(.*\\)\\.[^.]+$" groupkey)
		  (substring groupkey (match-beginning 1) (match-end 1)))))
      (gnus-subscribe-newsgroup newgroup before))))

(defun gnus-subscribe-interactively (newsgroup)
  "Subscribe new NEWSGROUP interactively.
It is inserted in hierarchical newsgroup order if subscribed. If not,
it is killed."
  (if (gnus-y-or-n-p (format "Subscribe new newsgroup: %s " newsgroup))
      (gnus-subscribe-hierarchically newsgroup)
    (setq gnus-killed-list (cons newsgroup gnus-killed-list))))

(defun gnus-subscribe-zombies (newsgroup)
  "Make new NEWSGROUP a zombie group."
  (setq gnus-zombie-list (cons newsgroup gnus-zombie-list)))

(defun gnus-subscribe-newsgroup (newsgroup &optional next)
  "Subscribe new NEWSGROUP.
If NEXT is non-nil, it is inserted before NEXT. Otherwise it is made
the first newsgroup."
  ;; We subscribe the group by changing its level to 3.
  (gnus-group-change-level 
   newsgroup 3 9 (gnus-gethash (or next "dummy.group") gnus-newsrc-hashtb))
  (message "Subscribe newsgroup: %s" newsgroup))

;; For directories

(defun gnus-newsgroup-directory-form (newsgroup)
  "Make hierarchical directory name from NEWSGROUP name."
  (let ((newsgroup (substring newsgroup 0)) ;Copy string.
	(len (length newsgroup))
	(idx 0))
    ;; Replace all occurrences of `.' with `/'.
    (while (< idx len)
      (if (= (aref newsgroup idx) ?.)
	  (aset newsgroup idx ?/))
      (setq idx (1+ idx)))
    newsgroup
    ))

(defun gnus-make-directory (dir)
  "Make DIRECTORY recursively."
  (let* ((dir (expand-file-name dir default-directory))
	 dirs)
    (if (string-match "/$" dir)
	(setq dir (substring dir 0 (match-beginning 0))))
    (while (not (file-exists-p dir))
      (setq dirs (cons dir dirs))
      (string-match "/[^/]+$" dir)
      (setq dir (substring dir 0 (match-beginning 0))))
    (while dirs
      (make-directory (car dirs))
      (setq dirs (cdr dirs)))))

(defun gnus-capitalize-newsgroup (newsgroup)
  "Capitalize NEWSGROUP name."
  (and (not (zerop (length newsgroup)))
       (concat (char-to-string (upcase (aref newsgroup 0)))
	       (substring newsgroup 1))))

;; Var

(defun gnus-simplify-subject (subject &optional re-only)
  "Remove `Re:' and words in parentheses.
If optional argument RE-ONLY is non-nil, strip `Re:' only."
  (let ((case-fold-search t))		;Ignore case.
    ;; Remove `Re:' and `Re^N:'.
    (if (string-match "^re:[ \t]*" subject)
	(setq subject (substring subject (match-end 0))))
    ;; Remove words in parentheses from end.
    (or re-only
	(while (string-match "[ \t\n]*([^()]*)[ \t\n]*\\'" subject)
	  (setq subject (substring subject 0 (match-beginning 0)))))
    ;; Return subject string.
    subject
    ))

(defun gnus-add-current-to-buffer-list ()
  (setq gnus-buffer-list (cons (current-buffer) gnus-buffer-list)))

;; Functions accessing headers.
;; Functions are more convenient than macros in some case.

(defun gnus-header-number (header)
  "Return article number in HEADER."
  (header-number header))

(defun gnus-header-subject (header)
  "Return subject string in HEADER."
  (header-subject header))

(defun gnus-header-from (header)
  "Return author string in HEADER."
  (header-from header))

(defun gnus-header-xref (header)
  "Return xref string in HEADER."
  (header-xref header))

(defun gnus-header-lines (header)
  "Return lines in HEADER."
  (header-lines header))

(defun gnus-header-date (header)
  "Return date in HEADER."
  (header-date header))

(defun gnus-header-id (header)
  "Return Id in HEADER."
  (header-id header))

(defun gnus-header-references (header)
  "Return references in HEADER."
  (header-references header))

(defun gnus-clear-system ()
  "Clear all variables and buffers."
  ;; Clear Gnus variables.
  (let ((variables gnus-variable-list))
    (while variables
      (set (car variables) nil)
      (setq variables (cdr variables))))
  ;; Clear other internal variables.
  (setq gnus-list-of-killed-groups nil
	gnus-have-read-active-file nil
	gnus-newsrc-assoc nil
	gnus-newsrc-hashtb nil
	gnus-killed-list nil
	gnus-zombie-list nil
	gnus-killed-hashtb nil
	gnus-active-hashtb nil
	gnus-moderated-list nil
	gnus-description-hashtb nil
	gnus-newsgroup-headers nil
	gnus-score-cache nil
	gnus-newsgroup-headers-hashtb-by-number nil
	gnus-current-select-method nil)
  ;; Kill the startup file.
  (and gnus-current-startup-file
       (get-file-buffer gnus-current-startup-file)
       (kill-buffer (get-file-buffer gnus-current-startup-file)))
  (setq gnus-current-startup-file nil)
  (gnus-dribble-clear)
  ;; Kill global KILL file buffer.
  (if (get-file-buffer (gnus-newsgroup-kill-file nil))
      (kill-buffer (get-file-buffer (gnus-newsgroup-kill-file nil))))
  ;; Kill Gnus buffers.
  (while gnus-buffer-list
    (if (and (get-buffer (car gnus-buffer-list))
	     (buffer-name (get-buffer (car gnus-buffer-list))))
	(kill-buffer (car gnus-buffer-list)))
    (setq gnus-buffer-list (cdr gnus-buffer-list))))

(defun gnus-configure-windows (action &optional force)
  "Configure Gnus windows according to the next ACTION.
The ACTION is either a symbol, such as `summary', or a
configuration list such as `(1 1 2)'.  If ACTION is not a list,
configuration list is got from the variable gnus-window-configuration.
If FORCE is non-nil, the updating will be done whether it is necessary
or not."
  (let* ((windows
	  (if (listp action) action 
	    (if (listp gnus-window-configuration)
		(car (cdr (assq action gnus-window-configuration)))
	      gnus-window-configuration)))
	 (grpwin (get-buffer-window gnus-group-buffer))
	 (subwin (get-buffer-window gnus-summary-buffer))
	 (artwin (get-buffer-window gnus-article-buffer))
	 (winsum nil)
	 (height nil)
	 (grpheight 0)
	 (subheight 0)
	 (artheight 0)

	 ;; Make split-window-vertically leave focus in upper window.
	 (split-window-keep-point t))
    (if (and (symbolp windows) (fboundp windows))
	(funcall windows action)
      (if (and (not force)
	       (or (null windows)		;No configuration is specified.
		   (and (eq (null grpwin)
			    (zerop (nth 0 windows)))
			(eq (null subwin)
			    (zerop (nth 1 windows)))
			(eq (null artwin)
			    (zerop (nth 2 windows))))))
	  ;; No need to change window configuration.
	  nil
	(select-window (or grpwin subwin artwin (selected-window)))
	;; First of all, compute the height of each window.
	(cond (gnus-use-full-window
	       ;; Take up the entire screen.
	       (delete-other-windows)
	       (setq height (window-height (selected-window))))
	      (t
	       (setq height (+ (if grpwin (window-height grpwin) 0)
			       (if subwin (window-height subwin) 0)
			       (if artwin (window-height artwin) 0)))))
	;; The group buffer exits always. So, use it to extend the
	;; group window so as to get enough window space.
	(switch-to-buffer gnus-group-buffer 'norecord)
	(and (get-buffer gnus-summary-buffer)
	     (delete-windows-on gnus-summary-buffer))
	(and (get-buffer gnus-article-buffer)
	     (delete-windows-on gnus-article-buffer))
	;; Compute expected window height.
	(setq winsum (apply (function +) windows))
	(if (not (zerop (nth 0 windows)))
	    (setq grpheight (max window-min-height
				 (/ (* height (nth 0 windows)) winsum))))
	(if (not (zerop (nth 1 windows)))
	    (setq subheight (max window-min-height
				 (/ (* height (nth 1 windows)) winsum))))
	(if (not (zerop (nth 2 windows)))
	    (setq artheight (max window-min-height
				 (/ (* height (nth 2 windows)) winsum))))
	(setq height (+ grpheight subheight artheight))
	(enlarge-window (max 0 (- height (window-height (selected-window)))))
	;; Then split the window.
	(and (not (zerop artheight))
	     (or (not (zerop grpheight))
		 (not (zerop subheight)))
	     (split-window-vertically (+ grpheight subheight)))
	(and (not (zerop grpheight))
	     (not (zerop subheight))
	     (split-window-vertically grpheight))
	;; Then select buffers in each window.
	(or (zerop grpheight)
	    (progn
	      (switch-to-buffer gnus-group-buffer 'norecord)
	      (other-window 1)))
	(or (zerop subheight)
	    (progn
	      (switch-to-buffer gnus-summary-buffer 'norecord)
	      (other-window 1)))
	(or (zerop artheight)
	    (progn
	      ;; If article buffer does not exist, it will be created
	      ;; and initialized.
	      (gnus-article-setup-buffer)
	      (switch-to-buffer gnus-article-buffer 'norecord)
	      (setq buffer-read-only t) ; !!! Why!?! 
	      (bury-buffer gnus-summary-buffer)
	      (bury-buffer gnus-group-buffer)))
	(or (zerop subheight)
	    (progn
	      (pop-to-buffer gnus-summary-buffer)
	      ;; It seems that some code in this function will set
	      ;; buffer-read-only to nil. I have absolutely no idea
	      ;; why. 
	      (setq buffer-read-only t))) ; !!! Why!?! 
	))))

(defun gnus-window-configuration-split (action)
  (switch-to-buffer gnus-group-buffer t)
  (delete-other-windows)
  (split-window-horizontally)
  (cond ((or (eq action 'newsgroups) (eq action 'summary))
	 (if (and (get-buffer gnus-summary-buffer)
		  (buffer-name gnus-summary-buffer))
	     (switch-to-buffer-other-window gnus-summary-buffer)))
	((eq action 'article)
	 (switch-to-buffer gnus-summary-buffer t)
	 (other-window 1)
	 (gnus-article-setup-buffer)
	 (switch-to-buffer gnus-article-buffer t))))

(defun gnus-version ()
  "Version numbers of this version of Gnus."
  (interactive)
  (let ((methods gnus-valid-select-methods)
	(mess gnus-version)
	meth)
    ;; Go through all the legal select methods and add their version
    ;; numbers to the total version string. Only the backends that are
    ;; currently in use will have their message numbers taken into
    ;; consideration. 
    (while methods
      (setq meth (intern (concat (car (car methods)) "-version")))
      (and (boundp meth)
	   (stringp (symbol-value meth))
	   (setq mess (concat mess "; " (symbol-value meth))))
      (setq methods (cdr methods)))
    (message mess)))

(defun gnus-info-find-node ()
  "Find Info documentation of Gnus."
  (interactive)
  ;; Enlarge info window if needed.
  (cond ((eq major-mode 'gnus-group-mode)
	 (gnus-configure-windows '(1 0 0)) ;Take all windows.
	 (pop-to-buffer gnus-group-buffer))
	((eq major-mode 'gnus-summary-mode)
	 (gnus-configure-windows '(0 1 0)) ;Take all windows.
	 (pop-to-buffer gnus-summary-buffer)))
  (Info-goto-node (car (cdr (assq major-mode gnus-info-nodes)))))

(defun gnus-bug ()
  "Send a bug report to the Gnus maintainers."
  (interactive)
  (pop-to-buffer "*Gnus Bug*")
  (erase-buffer)
  (mail-setup gnus-maintainer "[Gnus Bug Report] " nil nil nil nil)
  (goto-char (point-min))
  (search-forward mail-header-separator)
  (forward-line 1)
  (insert (format "%s\n%s\n\n" (gnus-version) (emacs-version)))
  (gnus-debug)
  (mail-mode)
  (message ""))

(defun gnus-debug ()
  "Attemps to go through the Gnus source file and report what variables have been changed.
The source file has to be in the Emacs load path."
  (interactive)
  (let ((dirs load-path)
	file expr olist)
    (while dirs
      (if (file-exists-p (setq file (concat (car dirs) "/gnus.el")))
	  (save-excursion
	    (setq dirs nil)
	    (set-buffer (get-buffer-create "*gnus bug info*"))
	    (buffer-disable-undo (current-buffer))
	    (erase-buffer)
	    (insert-file-contents file)
	    (goto-char (point-min))
	    (or (search-forward "\n;; Internal variables" nil t)
		(error "Malformed sources"))
	    (narrow-to-region (point-min) (point))
	    (goto-char (point-min))
	    (while (setq expr (condition-case () 
				  (read (current-buffer)) (error nil)))
	      (and (eq (car expr) 'defvar)
		   (stringp (nth 3 expr))
		   (not (equal (eval (nth 2 expr))
			       (symbol-value (nth 1 expr))))
		   (setq olist (cons (nth 1 expr) olist))))
	    (kill-buffer (current-buffer)))
	(setq dirs (cdr dirs))))
    (while olist
      (insert "(setq " (symbol-name (car olist)) " '" 
	      (prin1-to-string (symbol-value (car olist))) ")\n")
      (setq olist (cdr olist)))
    (insert "\n\n")))

(defun gnus-overload-functions (&optional overloads)
  "Overload functions specified by optional argument OVERLOADS.
If nothing is specified, use the variable gnus-overload-functions."
  (let ((defs nil)
	(overloads (or overloads gnus-overload-functions)))
    (while overloads
      (setq defs (car overloads))
      (setq overloads (cdr overloads))
      ;; Load file before overloading function if necessary.  Make
      ;; sure we cannot use `require' always.
      (and (not (fboundp (car defs)))
	   (car (cdr (cdr defs)))
	   (load (car (cdr (cdr defs))) nil 'nomessage))
      (fset (car defs) (car (cdr defs)))
      )))

(defun gnus-replace-chars-in-string (string from to)
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

(defun gnus-days-between (date1 date2)
  ;; Return the number of days between date1 and date2.
  (let ((d1 (mapcar (lambda (s) (and s (string-to-int s)) )
		    (timezone-parse-date date1)))
	(d2 (mapcar (lambda (s) (and s (string-to-int s)) )
		    (timezone-parse-date date2))))
    (- (timezone-absolute-from-gregorian 
	(nth 1 d1) (nth 2 d1) (car d1))
       (timezone-absolute-from-gregorian 
	(nth 1 d2) (nth 2 d2) (car d2)))))

(defun gnus-file-newer-than (file date)
  (let ((fdate (nth 5 (file-attributes file))))
    (or (> (car fdate) (car date))
	(and (= (car fdate) (car date))
	     (> (nth 1 fdate) (nth 1 date))))))

(defun gnus-y-or-n-p (prompt)
  (prog1
      (y-or-n-p prompt)
    (message "")))

(defun gnus-yes-or-no-p (prompt)
  (prog1
      (yes-or-no-p prompt)
    (message "")))

;; Return a string of length POS+1 representing NUMber in BASE. The
;; resulting string will be left padded with zeds.
(defun gnus-number-base-x (num pos base)
  (if (< pos 0)
      ""
    (concat 
     (char-to-string
      (aref "zyxwvutsrqponmlkjihgfedcba9876543210" (/ num (expt base pos))))
     (gnus-number-base-x 
      (% num (expt base pos)) (1- pos) base))))

;; List and range functions

(defun gnus-last-element (list)
  "Return last element of LIST."
  (while (cdr list)
    (setq list (cdr list)))
  (car list))

(defun gnus-set-difference (list1 list2)
  "Return a list of elements of LIST1 that do not appear in LIST2."
  (let ((list1 (copy-sequence list1)))
    (while list2
      (setq list1 (delq (car list2) list1))
      (setq list2 (cdr list2)))
    list1
    ))

(defun gnus-intersection (list1 list2)      
  (let ((result nil))
    (while list2
      (if (memq (car list2) list1)
	  (setq result (cons (car list2) result)))
      (setq list2 (cdr list2)))
    result
    ))

(defun gnus-compress-sequence (numbers &optional always-list)
  "Convert list of numbers to a list of ranges or a single range.
If ALWAYS-LIST is non-nil, this function will always release a list of
ranges."
  (let* ((first (car numbers))
	 (last (car numbers))
	 result)
    (if (null numbers)
	nil
      (while numbers
	(cond ((= last (car numbers)) nil) ;Omit duplicated number
	      ((= (1+ last) (car numbers)) ;Still in sequence
	       (setq last (car numbers)))
	      (t				;End of one sequence
	       (setq result (cons (cons first last) result))
	       (setq first (car numbers))
	       (setq last  (car numbers))))
	(setq numbers (cdr numbers)))
      (if (and (not always-list) (null result))
	  (cons first last)
	(nreverse (cons (cons first last) result))))))

(defun gnus-uncompress-sequence (ranges)
  "Expand a list of ranges into a list of numbers.
RANGES is either a single range on the form `(num . num)' or a list of
these ranges."
  (let (first last result)
    (if (null ranges)
	nil
      (if (atom (car ranges))
	  (progn
	    (setq first (car ranges))
	    (setq last (cdr ranges))
	    (while (<= first last)
	      (setq result (cons first result))
	      (setq first (1+ first))))
	(while ranges
	  (setq first (car (car ranges)))
	  (setq last  (cdr (car ranges)))
	  (while (<= first last)
	    (setq result (cons first result))
	    (setq first (1+ first)))
	  (setq ranges (cdr ranges))))
      (nreverse result))))

(defun gnus-add-to-range (ranges list)
  "Return a list of ranges that has all articles from both RANGES and LIST.
Note: LIST has to be sorted over `<'."
  (let* ((ranges (if (and ranges (atom (car ranges))) (list ranges) ranges))
	 (inrange ranges)
	 did-one
	 range nranges first last)
    (if (not list)
	ranges
      (if (not ranges)
	  (gnus-compress-sequence list t)
	(and ranges 
	     (> (car (car ranges)) 1)
	     (progn
	       (setq did-one t)
	       (setq inrange (setq ranges (cons (cons 1 1) ranges)))))
	(while (and ranges list)
	  (setq range (car ranges))
	  (while (and list (>= (car list) (car range))
		      (<= (car list) (cdr range)))
	    (setq list (cdr list)))
	  (while (and list (= (1- (car list)) (cdr range)))
	    (setcdr range (car list))
	    (setq list (cdr list)))
	  (if (and list (and (> (car list) (cdr range)) 
			     (cdr ranges)
			     (< (car list) (car (car (cdr ranges))))))
	      (setcdr ranges (cons (cons (car list) (car list)) (cdr ranges))))
	  (setq ranges (cdr ranges)))
	(if (and list (not ranges))
	    (setq inrange (nconc inrange (gnus-compress-sequence list t))))
	(if did-one
	    (if (eq (cdr (car inrange)) 1)
		(setq inrange (cdr inrange))
	      (setcar (car inrange) 2)))
	(setq ranges inrange)
	(while ranges
	  (if (and (cdr ranges) (>= (1+ (cdr (car ranges)))
				    (car (car (cdr ranges)))))
	      (progn
		(setcdr (car ranges) (cdr (car (cdr ranges))))
		(setcdr ranges (cdr (cdr ranges))))
	    (setq ranges (cdr ranges))))
	(if (not (cdr inrange))
	    (car inrange)
	  inrange)))))

(defun gnus-remove-from-range (ranges list)
  "Return a list of ranges that has all articles from LIST removed from RANGES.
Note: LIST has to be sorted over `<'."
  ;; !!! This function shouldn't look like this, but I've got a headache.
  (gnus-compress-sequence 
   (gnus-set-difference 
    (gnus-uncompress-sequence ranges) list)))

(defun gnus-member-of-range (number ranges)
  (let ((not-stop t))
    (while (and ranges not-stop)
      (if (and (>= number (car (car ranges)))
	       (<= number (cdr (car ranges))))
	  (setq not-stop nil))
      (setq ranges (cdr ranges)))
    (not not-stop)))


;;;
;;; Gnus group mode
;;;

(if gnus-group-mode-map
    nil
  (setq gnus-group-mode-map (make-keymap))
  (suppress-keymap gnus-group-mode-map)
  (define-key gnus-group-mode-map " " 'gnus-group-read-group)
  (define-key gnus-group-mode-map "=" 'gnus-group-select-group)
  (define-key gnus-group-mode-map "j" 'gnus-group-jump-to-group)
  (define-key gnus-group-mode-map "n" 'gnus-group-next-unread-group)
  (define-key gnus-group-mode-map "p" 'gnus-group-prev-unread-group)
  (define-key gnus-group-mode-map [del] 'gnus-group-prev-unread-group)
  (define-key gnus-group-mode-map "N" 'gnus-group-next-group)
  (define-key gnus-group-mode-map "P" 'gnus-group-prev-group)
  (define-key gnus-group-mode-map "\M-n" 'gnus-group-next-unread-group-same-level)
  (define-key gnus-group-mode-map "\M-p" 'gnus-group-prev-unread-group-same-level)
  (define-key gnus-group-mode-map "," 'gnus-group-best-unread-group)
  (define-key gnus-group-mode-map "\r" 'gnus-group-select-group)
  (define-key gnus-group-mode-map "u" 'gnus-group-unsubscribe-current-group)
  (define-key gnus-group-mode-map "U" 'gnus-group-unsubscribe-group)
  (define-key gnus-group-mode-map "c" 'gnus-group-catchup-current)
  (define-key gnus-group-mode-map "C" 'gnus-group-catchup-current-all)
  (define-key gnus-group-mode-map "l" 'gnus-group-list-groups)
  (define-key gnus-group-mode-map "L" 'gnus-group-list-all-groups)
  (define-key gnus-group-mode-map "m" 'gnus-group-mail)
  (define-key gnus-group-mode-map "g" 'gnus-group-get-new-news)
  (define-key gnus-group-mode-map "\M-g" 'gnus-group-get-new-news-this-group)
  (define-key gnus-group-mode-map "R" 'gnus-group-restart)
  (define-key gnus-group-mode-map "r" 'gnus-group-read-init-file)
  (define-key gnus-group-mode-map "B" 'gnus-group-browse-foreign-server)
  (define-key gnus-group-mode-map "b" 'gnus-group-check-bogus-groups)
  (define-key gnus-group-mode-map "F" 'gnus-find-new-newsgroups)
  (define-key gnus-group-mode-map "\C-c\C-d" 'gnus-group-describe-group)
  (define-key gnus-group-mode-map "\M-d" 'gnus-group-describe-all-groups)
  (define-key gnus-group-mode-map "\C-c\C-a" 'gnus-group-apropos)
  (define-key gnus-group-mode-map "\C-c\M-C-a" 'gnus-group-description-apropos)
  (define-key gnus-group-mode-map "D" 'gnus-group-make-doc-group)
  (define-key gnus-group-mode-map "d" 'gnus-group-make-directory-group)
  (define-key gnus-group-mode-map "a" 'gnus-group-post-news)
  (define-key gnus-group-mode-map "\M-a" 'gnus-group-add-group)
  (define-key gnus-group-mode-map "\M-e" 'gnus-group-edit-group)
  (define-key gnus-group-mode-map "\ek" 'gnus-group-edit-local-kill)
  (define-key gnus-group-mode-map "\eK" 'gnus-group-edit-global-kill)
  (define-key gnus-group-mode-map "\C-k" 'gnus-group-kill-group)
  (define-key gnus-group-mode-map "\C-y" 'gnus-group-yank-group)
  (define-key gnus-group-mode-map "\C-w" 'gnus-group-kill-region)
  (define-key gnus-group-mode-map "\M-z" 'gnus-group-kill-all-zombies)
  (define-key gnus-group-mode-map "\C-x\C-t" 'gnus-group-transpose-groups)
  (define-key gnus-group-mode-map "\C-c\C-l" 'gnus-group-list-killed)
  (define-key gnus-group-mode-map "\C-c\C-k" 'gnus-group-list-killed)
  (define-key gnus-group-mode-map "\C-c\C-z" 'gnus-group-list-zombies)
  (define-key gnus-group-mode-map "\C-c\C-x" 'gnus-group-expire-articles)
  (define-key gnus-group-mode-map "\C-c\M-\C-x" 'gnus-group-expire-all-groups)
  (define-key gnus-group-mode-map "V" 'gnus-version)
  (define-key gnus-group-mode-map "S" 'gnus-group-set-current-level)
  (define-key gnus-group-mode-map "s" 'gnus-group-save-newsrc)
  (define-key gnus-group-mode-map "z" 'gnus-group-suspend)
  (define-key gnus-group-mode-map "Z" 'gnus-group-clear-dribble)
  (define-key gnus-group-mode-map "q" 'gnus-group-exit)
  (define-key gnus-group-mode-map "Q" 'gnus-group-quit)
  (define-key gnus-group-mode-map "?" 'gnus-group-describe-briefly)
  (define-key gnus-group-mode-map "\C-c\C-i" 'gnus-info-find-node)
  (define-key gnus-group-mode-map [mouse-2] 'gnus-mouse-pick-group))

(defun gnus-group-mode ()
  "Major mode for reading news.
All normal editing commands are switched off.
The following commands are available:

\\<gnus-group-mode-map>
\\[gnus-group-read-group]\t Choose the current group
\\[gnus-group-select-group]\t Select the current group without selecting the first article
\\[gnus-group-jump-to-group]\t Go to some group
\\[gnus-group-next-unread-group]\t Go to the next unread group
\\[gnus-group-prev-unread-group]\t Go to the previous unread group
\\[gnus-group-next-group]\t Go to the next group
\\[gnus-group-prev-group]\t Go to the previous group
\\[gnus-group-next-unread-group-same-level]\t Go to the next unread group on the same level
\\[gnus-group-prev-unread-group-same-level]\t Go to the previous unread group un the same level
\\[gnus-group-unsubscribe-current-group]\t (Un)subscribe to the current group
\\[gnus-group-unsubscribe-group]\t (Un)subscribe to some group
\\[gnus-group-catchup-current]\t Mark all unread articles in the current group as read
\\[gnus-group-catchup-current-all]\t Mark all alrticles in the current group as read
\\[gnus-group-list-groups]\t List groups that have unread articles
\\[gnus-group-list-all-groups]\t List all groups
\\[gnus-group-mail]\t Compose a mail
\\[gnus-group-get-new-news]\t Look for new news
\\[gnus-group-get-new-news-this-group]\t Look for new news for the current group
\\[gnus-group-restart]\t Restart Gnus
\\[gnus-group-save-newsrc]\t Save the startup file(s)
\\[gnus-group-browse-foreign-server]\t Browse a foreign (NNTP) server
\\[gnus-group-check-bogus-groups]\t Check for and remove bogus newsgroups
\\[gnus-find-new-newsgroups]\t Find new newsgroups
\\[gnus-group-describe-group]\t Describe the current newsgroup
\\[gnus-group-describe-all-groups]\t Describe all newsgroups
\\[gnus-group-post-news]\t Post an article to some newsgroup
\\[gnus-group-add-group]\t Add a newsgroup entry
\\[gnus-group-edit-group]\t Edit a newsgroup entry
\\[gnus-group-make-directory-group]\t Read a directory as a newsgroups
\\[gnus-group-edit-local-kill]\t Edit a local kill file
\\[gnus-group-edit-global-kill]\t Edit the global kill file
\\[gnus-group-kill-group]\t Kill the current newsgroup
\\[gnus-group-yank-group]\t Yank a previously killed newsgroup
\\[gnus-group-kill-region]\t Kill all newsgroups between point and mark
\\[gnus-group-kill-all-zombies]\t Kill all zombie newsgroups
\\[gnus-group-transpose-groups]\t Transpose two newsgroups
\\[gnus-group-list-killed]\t List all killed newsgroups
\\[gnus-group-list-zombies]\t List all zombie newsgroups
\\[gnus-group-expire-articles]\t Expire the expirable articles in the current newsgroup
\\[gnus-group-expire-all-groups]\t Expire expirable articles in all newsgroups
\\[gnus-version]\t Display the current Gnus version
\\[gnus-group-set-current-level]\t Set the level of the current newsgroup
\\[gnus-group-suspend]\t Suspend Gnus
\\[gnus-group-clear-dribble]\t Clear the dribble buffer
\\[gnus-group-exit]\t Stop reading news
\\[gnus-group-quit]\t Stop reading news without saving the startup files
\\[gnus-group-describe-briefly]\t Give a brief description of the current mode
\\[gnus-info-find-node]\t Find the info pages for Gnus
"
  (interactive)
  (if gnus-visual (gnus-group-make-menu-bar))
  (kill-all-local-variables)
  (setq mode-line-modified "-- ")
  (make-local-variable 'mode-line-format)
  (setq mode-line-format (copy-sequence mode-line-format))
  (and (equal (nth 3 mode-line-format) "   ")
       (setcar (nthcdr 3 mode-line-format) ""))
  (setq major-mode 'gnus-group-mode)
  (setq mode-name "Group")
  (gnus-group-set-mode-line)
  (setq mode-line-process nil)
  (use-local-map gnus-group-mode-map)
  (buffer-disable-undo (current-buffer))
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (run-hooks 'gnus-group-mode-hook))

(defun gnus-mouse-pick-group (e)
  (interactive "e")
  (mouse-set-point e)
  (gnus-group-read-group nil))

;;;###autoload
(defun gnus-no-server (&optional arg)
  "Read network news.
If ARG is a positive number, Gnus will use that as the
startup level. If ARG is nil, Gnus will be started at level 2. 
If ARG is non-nil and not a positive number, Gnus will
prompt the user for the name of an NNTP server to use.
As opposed to `gnus', this command will not connect to the local server."
  (interactive "P")
  (gnus (or arg 2) t))

(defalias '\(ding\) 'gnus)

;;;###autoload
(defun gnus (&optional arg dont-connect)
  "Read network news.
If ARG is non-nil and a positive number, Gnus will use that as the
startup level. If ARG is non-nil and not a positive number, Gnus will
prompt the user for the name of an NNTP server to use."
  (interactive "P")
  (if (get-buffer gnus-group-buffer)
      (progn
	(switch-to-buffer gnus-group-buffer)
	(gnus-group-get-new-news))
    (gnus-clear-system)
    (gnus-read-init-file)
    (let ((level (and arg (numberp arg) (> arg 0) arg)))
      (unwind-protect
	  (progn
	    (switch-to-buffer (get-buffer-create gnus-group-buffer))
	    (gnus-add-current-to-buffer-list)
	    (gnus-group-mode)
	    (or dont-connect (gnus-start-news-server (and arg (not level)))))
	(if (and (not dont-connect) 
		 (not (gnus-server-opened gnus-select-method)))
	    (gnus-group-quit)
	  ;; NNTP server is successfully open. 
	  (gnus-update-format-specifications)
	  (let ((buffer-read-only nil))
	    (erase-buffer)
	    (if (not gnus-inhibit-startup-message)
		(progn
		  (gnus-group-startup-message)
		  (sit-for 0))))
	  (run-hooks 'gnus-startup-hook)
	  (gnus-setup-news nil level)
	  (and gnus-use-dribble-file (gnus-dribble-open))
	  (or t (not gnus-novice-user)
	      gnus-expert-user
	      (gnus-group-describe-briefly)) ;Show brief help message.
	  (gnus-group-list-groups (or level 5)))))))

(defun gnus-group-startup-message (&optional x y)
  "Insert startup message in current buffer."
  ;; Insert the message.
  (erase-buffer)
  (insert
   (format "
%s
       A newsreader 
  for GNU Emacs

    Based on GNUS 
         written by 
 Masanobu UMEDA

Lars Ingebrigtsen 
  larsi@ifi.uio.no
" 
	   gnus-version))
  ;; And then hack it.
  ;; 18 is the longest line.
  (indent-rigidly (point-min) (point-max) 
		  (/ (max (- (window-width) (or x 22)) 0) 2))
  (goto-char (point-min))
  ;; +4 is fuzzy factor.
  (insert-char ?\n (/ (max (- (window-height) (or y 12)) 0) 2)))

(defun gnus-group-setup-buffer ()
  (or (get-buffer gnus-group-buffer)
      (progn
	(switch-to-buffer (get-buffer-create gnus-group-buffer))
	(gnus-add-current-to-buffer-list)
	(gnus-group-mode))))

(defun gnus-group-list-groups (level &optional unread)
  "List newsgroups with level LEVEL or lower that have unread alticles.
Default is 5, which lists all subscribed groups.
If argument UNREAD is non-nil, groups with no unread articles are also listed."
  (interactive "P")
  (setq level (or level 5))
  (gnus-group-setup-buffer)	;May call from out of group buffer
  (let ((case-fold-search nil)
	(group (gnus-group-group-name)))
    (funcall gnus-group-prepare-function level unread nil)
    (if (zerop (buffer-size))
	;; Suggested by Andrew Eskilsson <pi92ae@lelle.pt.hk-r.se>.
	(message "No news is horrible news")
      (goto-char (point-min))
      (if (not group)
	  ;; Go to the first group with unread articles.
	  (gnus-group-search-forward nil nil nil t)
	;; Find the right group to put point on. If the current group
	;; has disapeared in the new listing, try to find the next
	;; one. If no next one can be found, just leave point at the
	;; first newsgroup in the buffer.
	(if (not (gnus-goto-char
		  (text-property-any (point-min) (point-max) 
				     'gnus-group (intern group))))
	    (let ((newsrc (nthcdr 3 (gnus-gethash group gnus-newsrc-hashtb))))
	      (while (and newsrc
			  (not (gnus-goto-char 
				(text-property-any 
				 (point-min) (point-max) 'gnus-group 
				 (intern group)))))
		(setq newsrc (cdr newsrc))))))
      ;; Adjust cursor point.
      (gnus-group-position-cursor))))

(defun gnus-group-prepare-flat (level &optional all lowest) 
  "List all newsgroups with unread articles of level LEVEL or lower.
If ALL is non-nil, list groups that have no unread articles.
If LOWEST is non-nil, list all newsgroups of level LOWEST or higher."
  (set-buffer gnus-group-buffer)
  (let ((buffer-read-only nil)
	(newsrc (cdr gnus-newsrc-assoc))
	(lowest (or lowest 1))
	info clevel unread group)
    (erase-buffer)
    (if (< lowest 8)
	;; List living groups.
	(while newsrc
	  (setq info (car newsrc)
		group (car info)
		newsrc (cdr newsrc)
		unread (car (gnus-gethash group gnus-newsrc-hashtb)))
	  (and unread ; This group might be bogus
	       (<= (setq clevel (car (cdr info))) level) 
	       (>= clevel lowest)
	       (or all            ; We list all groups?
		   (eq unread t)  ; We list unactivated groups
		   (> unread 0)   ; We list groups with unread articles
		   (cdr (assq 'tick (nth 3 info)))) ; And groups with tickeds
	       (gnus-group-insert-group-line 
		nil group (car (cdr info)) (nth 3 info) unread (nth 4 info)))))

    ;; List dead groups.
    (and (>= level 8) (<= lowest 8)
	 (gnus-group-prepare-flat-list-dead gnus-zombie-list 8 ?Z))
    (and (>= level 9) (<= lowest 9)
	 (gnus-group-prepare-flat-list-dead gnus-killed-list 9 ?K))

    (gnus-group-set-mode-line)
    (setq gnus-have-all-newsgroups all)
    (run-hooks 'gnus-group-prepare-hook)))

(defun gnus-group-prepare-flat-list-dead (groups level mark)
  ;; List zombies and killed lists somehwat faster, which was
  ;; suggested by Jack Vinson <vinson@unagi.cis.upenn.edu>. It does
  ;; this by ignoring the group format specification altogether.
  (let (group beg)
    (while groups
      (setq group (car groups)
	    groups (cdr groups))
      (setq beg (point))
      (insert (format " %c    *: %s\n" mark group))
      (add-text-properties 
       beg (1+ beg) 
       (list 'gnus-group (intern group)
	     'gnus-unread t
	     'gnus-level level)))))

(defun gnus-group-real-name (group)
  "Find the real name of a foreign newsgroup."
  (if (string-match "^[^:]+:" group)
      (substring group (match-end 0))
    group))

(defun gnus-group-prefixed-name (group method)
  "Return the whole name from GROUP and METHOD."
  (concat (format "%s" (car method))
	  (if (assoc (format "%s" (car method)) (gnus-methods-using 'address))
	      (concat "+" (nth 1 method)))
	  ":" group))

(defun gnus-group-real-prefix (group)
  "Return the prefix of the current group name."
  (if (string-match "^[^:]+:" group)
      (substring group 0 (match-end 0))
    ""))

(defun gnus-group-method-name (group)
  "Return the method used for selecting GROUP."
  (let ((prefix (gnus-group-real-prefix group)))
    (if (equal prefix "")
	gnus-select-method
      (if (string-match "^[^\\+]+\\+" prefix)
	  (list (intern (substring prefix 0 (1- (match-end 0))))
		(substring prefix (match-end 0) (1- (length prefix))))
	(list (intern (substring prefix 0 (1- (length prefix)))) "")))))

(defun gnus-group-foreign-p (group)
  "Return nil if GROUP is native, non-nil if it is foreign."
  (string-match ":" group))

(defun gnus-group-set-info (info)
  (let ((entry (gnus-gethash (car info) gnus-newsrc-hashtb)))
    (if entry
	(progn
	  (setcar (nthcdr 2 entry) info)
	  (if (and (not (eq (car entry) t)) 
		   (gnus-gethash (car info) gnus-active-hashtb))
	      (let ((marked (nth 3 info)))
		(setcar entry 
			(max 0 (- (length (gnus-list-of-unread-articles 
					   (car info)))
				  (length (cdr (assq 'tick marked)))
				  (length (cdr (assq 'dormant marked)))))))))
      (error "No such group: %s" (car info)))))

(defun gnus-group-update-group-line ()
  "This function updates the current line in the newsgroup buffer and
moves the point to the colon."
  (let* ((buffer-read-only nil)
	 (group (gnus-group-group-name))
	 (entry (and group (gnus-gethash group gnus-newsrc-hashtb))))
    (if entry
	(gnus-dribble-enter 
	 (concat "(gnus-group-set-info '" (prin1-to-string (nth 2 entry))
		 ")")))
    (beginning-of-line)
    (delete-region (point) (save-excursion (forward-line 1) (point)))
    (gnus-group-insert-group-line-info group)
    (forward-line -1)
    (gnus-group-position-cursor)))

(defun gnus-group-insert-group-line-info (group)
  (let ((entry (gnus-gethash group gnus-newsrc-hashtb)) 
	active info)
    (if entry
	(progn
	  (setq info (nth 2 entry))
	  (gnus-group-insert-group-line 
	   nil group (nth 1 info) (nth 3 info) (car entry) (nth 4 info)))
      (setq active (gnus-gethash group gnus-active-hashtb))
      (gnus-group-insert-group-line 
       nil group (if (member group gnus-zombie-list) 8 9)
       nil (- (1+ (cdr active)) (car active)) nil))))

(defun gnus-group-insert-group-line (gformat group level marked number method)
  (let* ((gformat (or gformat gnus-group-line-format-spec))
	 (active (gnus-gethash group gnus-active-hashtb))
	 (number-total (if active (1+ (- (cdr active) (car active))) 0))
	 (number-of-dormant (length (cdr (assq 'dormant marked))))
	 (number-of-ticked (length (cdr (assq 'tick marked))))
	 (number-of-ticked-and-dormant
	  (+ number-of-ticked number-of-dormant))
	 (number-of-unread-unticked 
	  (if (numberp number) (int-to-string (max 0 number))
	    "*"))
	 (number-of-read
	  (if (numberp number)
	      (max 0 (- number-total number))
	    "*"))
	 (subscribed (cond ((< level 6) ? )
			   ((< level 8) ?U)
			   ((= level 8) ?Z)
			   (t ?K)))
	 (qualified-group (gnus-group-real-name group))
	 (newsgroup-description 
	  (if gnus-description-hashtb
	      (or (gnus-gethash group gnus-description-hashtb) "")
	    ""))
	 (moderated (if (member group gnus-moderated-list) ?m ? ))
	 (moderated-string (if (eq moderated ?m) "(m)" ""))
	 (news-server (or (car (cdr method)) ""))
	 (news-method (or (car method) ""))
	 (news-method-string 
	  (if method (format "(%s:%s)" (car method) (car (cdr method))) ""))
	 (marked (if (and 
		      (numberp number) 
		      (zerop number)
		      (> number-of-ticked 0))
		     ?* ? ))
	 (number (if (eq number t) "*" (+ number number-of-dormant 
					  number-of-ticked)))
	 (buffer-read-only nil)
	 b)
    (beginning-of-line)
    (setq b (point))
    ;; Insert the visible text.
    (insert-before-markers (eval gformat))
    (add-text-properties 
     b (1+ b) (list 'gnus-group (intern group)
		    'gnus-unread (if (numberp number)
				     (string-to-int number-of-unread-unticked)
				   t)
		    'gnus-marked marked
		    'gnus-level level))))

(defun gnus-group-update-group (group &optional visible-only)
  "Update newsgroup info of GROUP.
If VISIBLE-ONLY is non-nil, the group won't be displayed if it isn't already."
  (save-excursion
    (set-buffer gnus-group-buffer)
    (let ((buffer-read-only nil)
	  (visible nil))
      (let ((entry (gnus-gethash group gnus-newsrc-hashtb)))
	(if entry
	    (gnus-dribble-enter 
	     (concat "(gnus-group-set-info '" (prin1-to-string (nth 2 entry))
		     ")"))))
      ;; Buffer may be narrowed.
      (save-restriction
	(widen)
	;; Search a line to modify.  If the buffer is large, the search
	;; takes long time.  In most cases, current point is on the line
	;; we are looking for.  So, first of all, check current line. 
	(if (or (progn
		  (beginning-of-line)
		  (eq (get-text-property (point) 'gnus-group)
		      (intern group)))
		(progn
		  (gnus-goto-char 
		   (text-property-any 
		    (point-min) (point-max) 'gnus-group (intern group)))))
	    ;; GROUP is listed in current buffer. So, delete old line.
	    (progn
	      (setq visible t)
	      (beginning-of-line)
	      (delete-region (point) (progn (forward-line 1) (point))))
	  ;; No such line in the buffer, find out where it's supposed to
	  ;; go, and insert it there (or at the end of the buffer).
	  ;; Fix by Per Abrahamsen <amanda@iesd.auc.dk>.
	  (or visible-only
	      (let ((entry (cdr (gnus-gethash group gnus-newsrc-hashtb))))
		(while (and entry
			    (not
			     (gnus-goto-char
			      (text-property-any
			       (point-min) (point-max) 
			       'gnus-group (intern (car (car entry)))))))
		  (setq entry (cdr entry)))
		(or entry (goto-char (point-max))))))
	(if (or visible (not visible-only))
	    (progn
	      (gnus-group-insert-group-line-info group)
	      (forward-line -1)		; Move point back to the inserted line.
	      ))))
    (gnus-group-set-mode-line)))

(defun gnus-group-set-mode-line ()
  (if (memq 'group gnus-updated-mode-lines)
      (let* ((gformat (or gnus-group-mode-line-format-spec
			  (setq gnus-group-mode-line-format-spec
				(gnus-parse-format 
				 gnus-group-mode-line-format 
				 gnus-group-mode-line-format-alist))))
	     (news-server (car (cdr gnus-select-method)))
	     (news-method (car gnus-select-method))
	     (mode-string (eval gformat))
	     (max-len 60))
	(if (> (length mode-string) max-len) 
	    (setq mode-string (substring mode-string 0 (- max-len 4))))
	(setq mode-line-buffer-identification mode-string)
	(set-buffer-modified-p t))))

(defun gnus-group-group-name ()
  "Get the name of the newsgroup on the current line."
  (let ((group (get-text-property 
		(save-excursion (beginning-of-line) (point)) 'gnus-group)))
    (and group (symbol-name group))))

(defun gnus-group-group-level ()
  "Get the level of the newsgroup on the current line."
  (get-text-property (save-excursion (beginning-of-line) (point)) 'gnus-level))

(defun gnus-group-search-forward (&optional backward all level first-too)
  "Find the next newsgroup with unread articles.
If BACKWARD is non-nil, find the previous newsgroup instead.
If ALL is non-nil, just find any newsgroup.
If LEVEL is non-nil, find group with level LEVEL, or higher if no such
group exists.
If FIRST-TOO, the current line is also eligeble as a target."
  (let ((way (if backward -1 1))
	(low 10)
	(beg (point))
	pos found)
    (or first-too (forward-line way))
    (while (and 
	    (not (eobp))
	    (not (setq 
		  found 
		  (and (or all
			   (and
			    (let ((unread 
				   (get-text-property (point) 'gnus-unread)))
			      (or (eq unread t) (and unread (> unread 0))))
			    (> 5 (get-text-property (point) 'gnus-level))))
		       (or (not level)
			   (let ((lev (get-text-property (point) 'gnus-level)))
			     (if (<= lev level)
				 t
			       (if (< lev low)
				   (progn
				     (setq low lev)
				     (setq pos (point))))
			       nil))))))
	    (zerop (forward-line way))))
    (if found 
	(progn (gnus-group-position-cursor) t)
      (if pos (goto-char pos) (goto-char beg))
      nil)))

;; Gnus group mode commands

(defun gnus-group-read-group (all &optional no-article group)
  "Read news in this newsgroup.
If argument ALL is non-nil, already read articles become readable.
If optional argument NO-ARTICLE is non-nil, no article body is displayed."
  (interactive "P")
  (let ((group (or group (gnus-group-group-name)))
	number active marked entry)
    (or group (error "No group on current line"))
    (setq marked 
	  (nth 3 (nth 2 (setq entry (gnus-gethash group gnus-newsrc-hashtb)))))
    ;; This group might be a dead group. In that case we have to get
    ;; the number of unread articles from `gnus-active-hashtb'.
    (if entry
	(setq number (car entry))
      (if (setq active (gnus-gethash group gnus-active-hashtb))
	  (setq number (- (1+ (cdr active)) (car active)))))
    (gnus-summary-read-group 
     group (or all (and (numberp number) 
			(zerop (+ number (length (cdr (assq 'tick marked)))
				  (length (cdr (assq 'dormant marked)))))))
     no-article)))

(defun gnus-group-select-group (all)
  "Select this newsgroup.
No article is selected automatically.
If argument ALL is non-nil, already read articles become readable."
  (interactive "P")
  (gnus-group-read-group all t))

(defun gnus-group-jump-to-group (group)
  "Jump to newsgroup GROUP."
  (interactive
   (list 
    (completing-read "Group: " gnus-active-hashtb nil t)))
  (let (b)
    ;; Either go to the line in the group buffer...
    (or (and (setq b (text-property-any (point-min) (point-max) 
					'gnus-group (intern group)))
	     (goto-char b))
	;; ... or insert the line.
	(progn (gnus-group-update-group group)
	       (goto-char (text-property-any (point-min) (point-max) 
					     'gnus-group (intern group))))))
  ;; Adjust cursor point.
  (gnus-group-position-cursor))

(defun gnus-group-next-group (n)
  "Go to next N'th newsgroup.
If N is negative, search backward instead.
Returns the difference between N and the number of skips actually
done."
  (interactive "p")
  (gnus-group-next-unread-group n t))

(defun gnus-group-next-unread-group (n &optional all level)
  "Go to next N'th unread newsgroup.
If N is negative, search backward instead.
If ALL is non-nil, choose any newsgroup, unread or not.
If LEVEL is non-nil, choose the next group with level LEVEL, or, if no
such group can be found, the next group with a level higher than
LEVEL.
Returns the difference between N and the number of skips actually
made."
  (interactive "p")
  (let ((backward (< n 0))
	(n (abs n)))
    (while (and (> n 0)
		(gnus-group-search-forward backward all level))
      (setq n (1- n)))
    (if (/= 0 n) (message "No more%s newsgroups%s" (if all "" " unread")
			  (if level " on this level or higher" "")))
    n))

(defun gnus-group-prev-group (n)
  "Go to previous N'th newsgroup.
Returns the difference between N and the number of skips actually
done."
  (interactive "p")
  (gnus-group-next-unread-group (- n) t))

(defun gnus-group-prev-unread-group (n)
  "Go to previous N'th unread newsgroup.
Returns the difference between N and the number of skips actually
done."  
  (interactive "p")
  (gnus-group-next-unread-group (- n)))

(defun gnus-group-next-unread-group-same-level (n)
  "Go to next N'th unread newsgroup on the same level.
If N is negative, search backward instead.
Returns the difference between N and the number of skips actually
done."
  (interactive "p")
  (gnus-group-next-unread-group n t (gnus-group-group-level))
  (gnus-group-position-cursor))

(defun gnus-group-prev-unread-group-same-level (n)
  "Go to next N'th unread newsgroup on the same level.
Returns the difference between N and the number of skips actually
done."
  (interactive "p")
  (gnus-group-next-unread-group (- n) t (gnus-group-group-level))
  (gnus-group-position-cursor))

(defun gnus-group-best-unread-group ()
  "Go to the group with the highest level."
  (interactive)
  (goto-char (point-min))
  (let ((best 10)
	unread best-point)
    (while (setq unread (get-text-property (point) 'gnus-unread))
      (if (and (numberp unread) (> unread 0))
	  (progn
	    (or best-point (setq best-point (point)))
	    (if (< (get-text-property (point) 'gnus-level) best)
		(progn 
		  (setq best (get-text-property (point) 'gnus-level))
		  (setq best-point (point))))))
      (forward-line 1))
    (if best-point (goto-char best-point))
    (gnus-summary-position-cursor)
    (and best-point (gnus-group-group-name))))

(defun gnus-group-add-group (name method address)
  "Add a new newsgroup.
The user will be prompted for a NAME, for a select METHOD, and an
ADDRESS."
  (interactive
   (list 
    (read-string "Group name: ")
    (completing-read (format "%s method: " name) 
		     gnus-valid-select-methods nil t)
    (read-string (format "Get %s by method %s from: " name method))))
  (let ((nname (gnus-group-prefixed-name name (list (intern method) address))))
    (gnus-group-change-level 
     (list t nname 3 nil nil (list (intern method) address))
     3 9 (gnus-gethash (or (gnus-group-group-name) "dummy.group")
		       gnus-newsrc-hashtb) t)
    (gnus-group-insert-group-line-info nname)))

(defun gnus-group-edit-group ()
  (interactive)
  (let ((group (gnus-group-group-name))
	info)
    (if group (setq info (nth 2 (gnus-gethash group gnus-newsrc-hashtb)))
      (error "No group on current line"))
    (switch-to-buffer (get-buffer-create gnus-group-edit-buffer))
    (gnus-add-current-to-buffer-list)
    (emacs-lisp-mode)
    ;; Suggested by Hallvard B Furuseth <h.b.furuseth@usit.uio.no>.
    (use-local-map (copy-keymap emacs-lisp-mode-map))
    (local-set-key "\C-c\C-c" 'gnus-group-edit-group-done)
    (erase-buffer)
    (insert ";; Type `C-c C-c' after you have edited the newsgroup entry.\n\n")
    (insert (format "(gnus-group-set-info\n  '%S)\n" info))))

(defun gnus-group-edit-group-done ()
  (interactive)
  (set-buffer (get-buffer-create gnus-group-edit-buffer))
  (eval-current-buffer)
  (kill-buffer (current-buffer))
  (set-buffer gnus-group-buffer)
  (gnus-group-update-group (gnus-group-group-name))
  (gnus-group-position-cursor))

(defun gnus-group-make-doc-group ()
  "Create the (ding) Gnus documentation group."
  (interactive)
  (let ((path load-path))
    (while (and path
		(not (file-exists-p (concat (file-name-as-directory (car path))
					    "doc.txt"))))
      (setq path (cdr path)))
    (or path (error "Couldn't find doc group"))
    (gnus-group-add-group 
     "gnus-doc" "nndoc" 
     (concat (file-name-as-directory (car path)) "doc.txt"))))

(defun gnus-group-make-directory-group (dir)
  "Create an nndir group.
The user will be prompted for a directory. The contents of this
directory will be used as a newsgroup. The directory should contain
mail messages or news articles in files that have numeric names."
  (interactive
   (list (read-file-name "Create group from directory: ")))
  (or (file-exists-p dir) (error "No such directory"))
  (or (file-directory-p dir) (error "Not a directory"))
  (gnus-group-add-group dir "nndir" dir))

(defun gnus-group-catchup-current (n &optional all)
  "Mark all articles not marked as unread in current newsgroup as read.
If prefix argument N is numeric, the ARG next newsgroups will be
caught up. If ALL is non-nil, marked articles will also be marked as
read. Cross references (Xref: header) of articles are ignored.
The difference between N and actual number of newsgroups that were
caught up is returned."
  (interactive "p")
  (if (or (not gnus-interactive-catchup) ;Without confirmation?
	  gnus-expert-user
	  (gnus-y-or-n-p
	   (if all
	       "Do you really want to mark all articles as read? "
	     "Mark all unread articles as read? ")))
      (progn
	(while 
	    (and (> n 0)
		 (progn
		   (setq n (1- n))
		   (gnus-group-catchup (gnus-group-group-name) all)
		   (gnus-group-update-group-line)
		   t)
		 (zerop (gnus-group-next-unread-group 1))))))
  n)

(defun gnus-group-catchup-current-all (n)
  "Mark all articles in current newsgroup as read.
Cross references (Xref: header) of articles are ignored."
  (interactive "p")
  (gnus-group-catchup-current n 'all))

(defun gnus-group-catchup (group &optional all)
  "Mark all articles in GROUP as read.
If ALL is non-nil, all articles are marked as read.
The return value is the number of articles that were marked as read,
or nil if no action could be taken."
  (let* ((entry (gnus-gethash group gnus-newsrc-hashtb))
	 (num (car entry))
	 (marked (nth 3 (nth 2 entry)))
	 ticked)
    (if (not (numberp (car entry)))
	(message "Can't catch up; non-active group")
      ;; Do the updating only if the newsgroup isn't killed
      (if entry
	  (progn
	    (setq ticked (if all nil (cdr (assq 'tick marked))))
	    (gnus-update-read-articles group ticked nil ticked)
	    (if (and all marked)
		(setcar (nthcdr 3 (nth 2 entry)) 
			(delq (assq 'dormant marked) marked))))))
    num))

(defun gnus-group-expire-articles (newsgroup)
  "Expire all expirable articles in the current newsgroup."
  (interactive (list (gnus-group-group-name)))
  (if (not newsgroup) (error "No current newsgroup"))
  (let ((expirable 
	 (assq 'expire (nth 3 (nth 2 (gnus-gethash newsgroup 
						   gnus-newsrc-hashtb))))))
 (and expirable 
      (gnus-check-backend-function 'request-expire-articles newsgroup)
      (setcdr expirable
	      (gnus-request-expire-articles (cdr expirable) newsgroup)))))

(defun gnus-group-expire-all-groups ()
  "Expire all expirable articles in all newsgroups."
  (interactive)
  (message "Expiring...")
  (let ((newsrc (cdr gnus-newsrc-assoc)))
    (while newsrc
      (gnus-group-expire-articles (car (car newsrc)))
      (setq newsrc (cdr newsrc))))
  (message "Expiring...done"))

(defun gnus-group-set-current-level (n)
  "Set the level of the current group to the numeric prefix."
  (interactive "P")
  (setq n (or n (string-to-int 
		 (completing-read 
		  "Level: " 
		  (mapcar (lambda (n) (list (char-to-string n))) "123456789")
		  nil t))))
  (let ((group (gnus-group-group-name)))
    (if (not group) (error "No newsgroup on current line.")
    (if (and (numberp n) (>= n 1) (<= n 9))
	(progn
	  (message "Changed level of %s from %d to %d" 
		   group (gnus-group-group-level) n)
	  (gnus-group-change-level group n (gnus-group-group-level))
	  (gnus-group-update-group-line))
      (error "Illegal level: %s" n))))
  (forward-line 1)
  (gnus-group-position-cursor))

(defun gnus-group-unsubscribe-current-group (arg)
  "Toggle subscribe from/to unsubscribe current group."
  (interactive "P")
  (let ((group (gnus-group-group-name)))
    (or group (error "No newsgroup on current line"))
    (or arg (setq arg (if (<= (gnus-group-group-level) 5) 6 3)))
    (gnus-group-unsubscribe-group group arg)))

(defun gnus-group-unsubscribe-group (group &optional level)
  "Toggle subscribe from/to unsubscribe GROUP.
New newsgroup is added to .newsrc automatically."
  (interactive
   (list (completing-read "Group: " gnus-active-hashtb nil 
			  gnus-have-read-active-file)))
  (let ((newsrc (gnus-gethash group gnus-newsrc-hashtb)))
    (cond (newsrc
	   ;; Toggle subscription flag.
	   (gnus-group-change-level 
	    newsrc (if level level (if (< (nth 1 (nth 2 newsrc)) 6) 6 4)))
	   (gnus-group-update-group group))
	  ((and (stringp group)
		(gnus-gethash group gnus-active-hashtb))
	   ;; Add new newsgroup.
	   (gnus-group-change-level 
	    group 
	    (if level level 3) 
	    (if (member group gnus-zombie-list) 8 9)
	    (or (and (gnus-group-group-name)
		     (gnus-gethash (gnus-group-group-name) gnus-newsrc-hashtb))
		(gnus-gethash (car (car gnus-newsrc-assoc)) 
			      gnus-newsrc-hashtb)))
	   (gnus-group-update-group group))
	  (t (error "No such newsgroup: %s" group)))
    (gnus-group-position-cursor)))

(defun gnus-group-transpose-groups (arg)
  "Exchange current newsgroup and previous newsgroup.
With argument ARG, takes previous newsgroup and moves it past ARG newsgroup."
  (interactive "p")
  ;; BUG: last newsgroup and the last but one cannot be transposed
  ;; since gnus-group-search-forward does not move forward beyond the
  ;; last.  If we instead use forward-line, no problem, but I don't
  ;; want to use it for later extension.
  (while (> arg 0)
    (gnus-group-search-forward t t)
    (gnus-group-kill-group 1)
    (gnus-group-search-forward nil t)
    (gnus-group-yank-group)
    (gnus-group-search-forward nil t)
    (setq arg (1- arg))))

(defun gnus-group-kill-all-zombies ()
  "Kill all zombie newsgroups."
  (interactive)
  (setq gnus-killed-list (nconc gnus-zombie-list gnus-killed-list))
  (setq gnus-zombie-list nil)
  (funcall gnus-group-prepare-function 5 nil nil)
  (goto-char (point-min))
  (gnus-group-position-cursor))

(defun gnus-group-kill-region (begin end)
  "Kill newsgroups in current region (excluding current point).
The killed newsgroups can be yanked by using \\[gnus-group-yank-group]."
  (interactive "r")
  (let ((lines
	 ;; Exclude a line where current point is on.
	 (1-
	  ;; Count lines.
	  (save-excursion
	    (count-lines
	     (progn
	       (goto-char begin)
	       (beginning-of-line)
	       (point))
	     (progn
	       (goto-char end)
	       (end-of-line)
	       (point)))))))
    (goto-char begin)
    (beginning-of-line)			;Important when LINES < 1
    (gnus-group-kill-group lines)))

(defun gnus-group-kill-group (n)
  "The the next N groups.
The killed newsgroups can be yanked by using \\[gnus-group-yank-group].
However, only groups that were alive can be yanked; already killed 
groups or zombie groups can't be yanked.
The return value is the name of the (last) newsgroup that was killed."
  (interactive "p")
  (let ((buffer-read-only nil)
	group entry level)
    (while (>= (setq n  (1- n)) 0)
      (setq group (gnus-group-group-name))
      (or group
	  (signal 'end-of-buffer nil))
      (setq level (gnus-group-group-level))
      (beginning-of-line)
      (delete-region (point) (progn (forward-line 1) (point)))
      (if (setq entry (gnus-gethash group gnus-newsrc-hashtb))
	  (setq gnus-list-of-killed-groups 
		(cons (cons (car entry) (nth 2 entry)) 
		      gnus-list-of-killed-groups)))
      (gnus-group-change-level (if entry entry group) 9 (if entry nil level)))
    (if (eobp)
	(forward-line -1))
    (gnus-group-position-cursor)
    group))

(defun gnus-group-yank-group (&optional arg)
  "Yank the last newsgroups killed with \\[gnus-group-kill-group],
inserting it before the current newsgroup.  The numeric ARG specifies
how many newsgroups are to be yanked.  The name of the (last)
newsgroup yanked is returned."
  (interactive "p")
  (if (not arg) (setq arg 1))
  (let (info group prev)
    (while (>= (setq arg (1- arg)) 0)
      (if (not (setq info (car gnus-list-of-killed-groups)))
	  (error "No more newsgroups to yank"))
      (setq group (nth 2 info))
      ;; Find which newsgroup to insert this one before - search
      ;; backward until something suitable is found. If there are no
      ;; other newsgroups in this buffer, just make this newsgroup the
      ;; first newsgroup.
      (while (and (not (setq prev (gnus-group-group-name)))
		  (zerop (forward-line -1))))
      (if (not prev)
	  (setq prev (car (car gnus-newsrc-assoc))))
      (gnus-group-change-level 
       info (nth 2 info) 9 
       (gnus-gethash prev gnus-newsrc-hashtb)
       t)
      (gnus-group-insert-group-line-info (nth 1 info))
      (setq gnus-list-of-killed-groups 
	    (cdr gnus-list-of-killed-groups)))
    (forward-line -1)
    (gnus-group-position-cursor)
    group))
      
(defun gnus-group-list-all-groups (arg)
  "List all newsgroups with level ARG or lower.
Default is 7, which lists all subscribed and most unsubscribed groups."
  (interactive "P")
  (setq arg (or arg 7))
  (gnus-group-list-groups arg t))

(defun gnus-group-list-killed ()
  "List all killed newsgroups in the group buffer."
  (interactive)
  (if (not gnus-killed-list)
      (message "No killed groups")
    (funcall gnus-group-prepare-function 9 t 9)
    (goto-char (point-min)))
  (gnus-group-position-cursor))

(defun gnus-group-list-zombies ()
  "List all zombie newsgroups in the group buffer."
  (interactive)
  (if (not gnus-zombie-list)
      (message "No zombie groups")
    (funcall gnus-group-prepare-function 8 t 8)
    (goto-char (point-min)))
  (gnus-group-position-cursor))

(defun gnus-group-get-new-news (&optional arg)
  "Get newly arrived articles.
If ARG is non-nil, it should be a number between one and nine to
specify which levels you are interested in re-scanning."
  (interactive "P")
  (if (and gnus-read-active-file (not arg))
      (progn
	(gnus-read-active-file)
	(gnus-get-unread-articles (or arg 6)))
    (let ((gnus-read-active-file nil))
      (gnus-get-unread-articles (or arg 6))))
  (gnus-group-list-groups 5 gnus-have-all-newsgroups))

(defun gnus-group-get-new-news-this-group (n)
  "Check for newly arrived news in the current group (and the N-1 next groups).
The difference between N and the number of newsgroup checked is returned.
If N is negative, this group and the N-1 previous groups will be checked."
  (interactive "p")
  (let ((way (if (< n 0) -1 1))
	(n (abs n))
	(w-p (window-start))
	group)
    (while (and (> n 0)
		(gnus-get-new-news-in-group (gnus-group-group-name))
		(zerop (gnus-group-next-group way)))
      (setq n (1- n)))
    (if (/= 0 n) (message "No more newsgroups"))
    ;; !!! I don't know why the buffer scrolls forward when updating
    ;; the first line in the group buffer, but it does. So we set the
    ;; window start forcibly.
    (set-window-start (get-buffer-window (current-buffer)) w-p)
    n))

(defun gnus-get-new-news-in-group (group)
  (if (and group (gnus-activate-newsgroup group))
      (progn
	(gnus-get-unread-articles-in-group 
	 (nth 2 (gnus-gethash group gnus-newsrc-hashtb))
	 (gnus-gethash group gnus-active-hashtb))
	(gnus-group-update-group-line)))
  t)
  
(defun gnus-group-describe-group (&optional group)
  "Display a description of the current newsgroup."
  (interactive)
  (let ((group (or group (gnus-group-group-name))))
    (if (not group)
	(message "No group on current line")
      (and (or gnus-description-hashtb
	       (gnus-read-descriptions-file))
	   (message
	    (or (gnus-gethash group gnus-description-hashtb)
		"No description available"))))))

;; Suggested by Per Abrahamsen <amanda@iesd.auc.dk>.
(defun gnus-group-describe-all-groups ()
  "Pop up a buffer with descriptons of all newsgroups."
  (interactive)
  (if (not (or gnus-description-hashtb
	       (gnus-read-descriptions-file)))
      (error "Couldn't request descriptions file"))
  (let ((buffer-read-only nil)
	b)
    (erase-buffer)
    (mapatoms
     (lambda (group)
       (setq b (point))
       (insert (format "      *: %-20s %s\n" (symbol-name group)
		       (symbol-value group)))
       (add-text-properties 
	b (1+ b) (list 'gnus-group group
		       'gnus-unread t 'gnus-marked nil 'gnus-level 6)))
     gnus-description-hashtb)
    (goto-char (point-min))
    (gnus-group-position-cursor)))

;; Suggested by by Daniel Quinlan <quinlan@best.com>.
(defun gnus-group-apropos (regexp &optional search-description)
  "List all newsgroups that have names that match a regexp."
  (interactive "sGnus apropos (regexp): ")
  (let ((prev "")
	(obuf (current-buffer))
	groups des prev)
    ;; Go through all newsgroups that are known to Gnus.
    (mapatoms 
     (lambda (group)
       (and (string-match regexp (symbol-name group))
	    (setq groups (cons (symbol-name group) groups))))
     gnus-active-hashtb)
    ;; Go through all descriptions that are known to Gnus. 
    (if search-description
	(mapatoms 
	 (lambda (group)
	   (and (string-match regexp (symbol-value group))
		(gnus-gethash (symbol-name group) gnus-active-hashtb)
		(setq groups (cons (symbol-name group) groups))))
	 gnus-description-hashtb))
    (if (not groups)
	(message "No groups matched \"%s\"." regexp)
      ;; Print out all the groups.
      (save-excursion
	(pop-to-buffer (get-buffer-create "*Gnus Help*"))
	(buffer-disable-undo (current-buffer))
	(erase-buffer)
	(setq groups (sort groups 'string<))
	(while groups
	  ;; Groups may be entered twice into the list of groups.
	  (if (not (string= (car groups) prev))
	      (progn
		(insert (setq prev (car groups)) "\n")
		(if (and gnus-description-hashtb
			 (setq des (gnus-gethash (car groups) 
						 gnus-description-hashtb)))
		    (insert "  " des "\n"))))
	  (setq groups (cdr groups)))
	(goto-char 1)))
    (pop-to-buffer obuf)))

(defun gnus-group-description-apropos (regexp)
  "List all newsgroups that have names or desccriptions that match a regexp."
  (interactive "sGnus description apropos (regexp): ")
  (if (not (or gnus-description-hashtb
	       (gnus-read-descriptions-file)))
      (error "Couldn't request descriptions file"))
  (gnus-group-apropos regexp t))

;; Written by Per Abrahamsen <amanda@iesd.auc.dk>.
(defun gnus-group-list-matching (regexp) 
  "List all newsgroups with unread articles that match REGEXP."
  (interactive "sList newsgroups matching: ")
  (set-buffer gnus-group-buffer)
  (let ((buffer-read-only nil)
	(newsrc (cdr gnus-newsrc-assoc))
	(zombie gnus-zombie-list)
	(killed gnus-killed-list)
	info unread active group)
    (erase-buffer)

    ;; List alive newsgroups.
    (while newsrc
      (setq info (car newsrc)
	    group (car info)
	    newsrc (cdr newsrc)
	    unread (car (gnus-gethash group gnus-newsrc-hashtb)))
      (if (and unread ; This group might be bogus
	       (string-match regexp group))
	  (gnus-group-insert-group-line 
	   nil group (car (cdr info)) (nth 3 info) unread
	   (nth 4 info))))

    ;; List zombies and killed lists.
    (let ((lists (list 'gnus-zombie-list 'gnus-killed-list))
	  mark b)
      (while lists
	(if (eq (car lists) 'gnus-zombie-list)
	    (setq mark ?Z)
	  (setq mark ?K))
	(setq newsrc (set (car lists)
			  (sort (symbol-value (car lists)) 
				(function string<))))
	(while newsrc
	  (setq group (car newsrc)
		newsrc (cdr newsrc))
	  (if (not (string-match regexp group))
	      ()
	    (setq b (point))
	    (insert (format " %c    *: %s" mark group))
	    (add-text-properties 
	     b (1+ b) 
	     (list 'gnus-group (intern group)
		   'gnus-unread t
		   'gnus-level (if (= mark ?Z) 8 9)))))
	(setq lists (cdr lists))))

    (gnus-group-set-mode-line)
    (setq gnus-have-all-newsgroups t)
    (run-hooks 'gnus-group-prepare-hook))
  (goto-char (point-min))
  (gnus-group-position-cursor))

;; Suggested by Jack Vinson <vinson@unagi.cis.upenn.edu>.
(defun gnus-group-save-newsrc ()
  "Save the Gnus startup files."
  (interactive)
  (gnus-save-newsrc-file))

(defun gnus-group-restart (&optional arg)
  "Force Gnus to read the .newsrc file."
  (interactive "P")
  (gnus-save-newsrc-file)
  (gnus-setup-news 'force)
  (gnus-group-list-groups (or arg 5) gnus-have-all-newsgroups))

(defun gnus-group-read-init-file ()
  "Read the Gnus elisp init file."
  (interactive)
  (gnus-read-init-file))

(defun gnus-group-check-bogus-groups ()
  "Check bogus newsgroups."
  (interactive)
  (gnus-check-bogus-newsgroups (not gnus-expert-user))	;Require confirmation.
  (gnus-group-list-groups 5 gnus-have-all-newsgroups))

(defun gnus-group-mail ()
  "Start composing a mail."
  (interactive)
  (mail))

(defun gnus-group-edit-global-kill ()
  "Edit a global kill file."
  (interactive)
  (setq gnus-current-kill-article nil)	;No articles selected.
  (gnus-kill-file-edit-file nil) 	;Nil stands for global KILL file.
  (message
   (substitute-command-keys
    "Editing a global kill file (Type \\[gnus-kill-file-exit] to exit)")))

(defun gnus-group-edit-local-kill ()
  "Edit a local kill file."
  (interactive)
  (setq gnus-current-kill-article nil)	;No articles selected.
  (gnus-kill-file-edit-file (gnus-group-group-name))
  (message
   (substitute-command-keys
    "Editing a local kill file (Type \\[gnus-kill-file-exit] to exit)")))

(defun gnus-group-force-update ()
  "Update `.newsrc' file."
  (interactive)
  (gnus-save-newsrc-file))

(defun gnus-group-suspend ()
  "Suspend the current Gnus session.
In fact, cleanup buffers except for group mode buffer.
The hook gnus-suspend-gnus-hook is called before actually suspending."
  (interactive)
  (run-hooks 'gnus-suspend-gnus-hook)
  ;; Kill Gnus buffers except for group mode buffer.
  (let ((group-buf (get-buffer gnus-group-buffer)))
    (while gnus-buffer-list
      (and (not (eq (get-buffer (car gnus-buffer-list)) group-buf))
	   (get-buffer (car gnus-buffer-list))
	   (buffer-name (get-buffer (car gnus-buffer-list)))
	   (kill-buffer (car gnus-buffer-list)))
      (setq gnus-buffer-list (cdr gnus-buffer-list)))
    (setq gnus-buffer-list (list group-buf))
    (bury-buffer group-buf)
    (delete-windows-on group-buf t)))

(defun gnus-group-clear-dribble ()
  "Clear all information from the dribble buffer."
  (interactive)
  (gnus-dribble-clear))

(defun gnus-group-exit ()
  "Quit reading news after updating .newsrc.eld and .newsrc.
The hook `gnus-exit-gnus-hook' is called before actually exiting."
  (interactive)
  (if (or noninteractive		;For gnus-batch-kill
	  (zerop (buffer-size))		;No news is good news.
	  (not (gnus-server-opened gnus-select-method)) ;NNTP connection closed
	  (not gnus-interactive-exit)	;Without confirmation
	  gnus-expert-user
	  (gnus-y-or-n-p "Are you sure you want to quit reading news? "))
      (progn
	(run-hooks 'gnus-exit-gnus-hook)
	(gnus-save-newsrc-file)
	(gnus-clear-system))))

(defun gnus-group-quit ()
  "Quit reading news without updating .newsrc.eld or .newsrc.
The hook `gnus-exit-gnus-hook' is called before actually exiting."
  (interactive)
  (if (or noninteractive		;For gnus-batch-kill
	  (zerop (buffer-size))
	  (not (gnus-server-opened gnus-select-method))
	  gnus-expert-user
	  (not gnus-current-startup-file)
	  (gnus-yes-or-no-p
	   (format "Quit reading news without saving %s? "
		   (file-name-nondirectory gnus-current-startup-file))))
      (progn
	(run-hooks 'gnus-exit-gnus-hook)
	(gnus-dribble-save)
	(gnus-clear-system))))

(defun gnus-group-describe-briefly ()
  "Give a one line description of the group mode commands."
  (interactive)
  (message
   (substitute-command-keys "\\<gnus-group-mode-map>\\[gnus-group-read-group]:Select  \\[gnus-group-next-unread-group]:Forward  \\[gnus-group-prev-unread-group]:Backward  \\[gnus-group-exit]:Exit  \\[gnus-info-find-node]:Run Info  \\[gnus-group-describe-briefly]:This help")))

(defun gnus-group-browse-foreign-server (method)
  "Browse a foreign news server.
If called interactively, this function will ask for a select method
 (nntp, nnspool, etc.) and a server address (eg. nntp.some.where). 
If not, METHOD should be a list where the first element is the method
and the second element is the address."
  (interactive
   (list (list (intern (completing-read 
			"Select method: "
			gnus-valid-select-methods nil t "nntp"))
	       ;; Suggested by mapjph@bath.ac.uk.
	       (completing-read 
		"Server name: " 
		(mapcar (lambda (server) (list server))
			gnus-secondary-servers)))))
  (gnus-browse-foreign-server method))


;;;
;;; Browse Server Mode
;;;

(defvar gnus-browse-server-mode-hook nil)
(defvar gnus-browse-server-mode-map nil)

(if gnus-browse-server-mode-map
    nil
  (setq gnus-browse-server-mode-map (make-keymap))
  (suppress-keymap gnus-browse-server-mode-map)
  (define-key gnus-browse-server-mode-map " " 'gnus-browse-read-group)
  (define-key gnus-browse-server-mode-map "=" 'gnus-browse-read-group)
  (define-key gnus-browse-server-mode-map "n" 'gnus-browse-next-group)
  (define-key gnus-browse-server-mode-map "p" 'gnus-browse-prev-group)
  (define-key gnus-browse-server-mode-map [del] 'gnus-browse-prev-group)
  (define-key gnus-browse-server-mode-map "N" 'gnus-browse-next-group)
  (define-key gnus-browse-server-mode-map "P" 'gnus-group-prev-group)
  (define-key gnus-browse-server-mode-map "\M-n" 'gnus-browse-next-group)
  (define-key gnus-browse-server-mode-map "\M-p" 'gnus-browse-prev-group)
  (define-key gnus-browse-server-mode-map "\r" 'gnus-browse-read-group)
  (define-key gnus-browse-server-mode-map "u" 'gnus-browse-unsubscribe-current-group)
  (define-key gnus-browse-server-mode-map "q" 'gnus-browse-exit)
  (define-key gnus-browse-server-mode-map "Q" 'gnus-browse-exit)
  (define-key gnus-browse-server-mode-map "\C-c\C-c" 'gnus-browse-quit)
  (define-key gnus-browse-server-mode-map "?" 'gnus-browse-describe-briefly)
  (define-key gnus-browse-server-mode-map "\C-c\C-i" 'gnus-info-find-node)
  )

(defvar gnus-browse-current-method nil)

(defun gnus-browse-foreign-server (method)
  (setq gnus-browse-current-method method)
  (let ((gnus-select-method method)
	groups group)
    (message "Connecting to %s..." (nth 1 method))
    (if (not (gnus-request-list method))
	(error "Unable to contact server: " (gnus-status-message method)))
    (set-buffer (get-buffer-create "*Gnus Browse Server*"))
    (gnus-add-current-to-buffer-list)
    (buffer-disable-undo (current-buffer))
    (let ((buffer-read-only nil))
      (erase-buffer))
    (gnus-browse-server-mode)
    (setq mode-line-buffer-identification
	  (format
	   "(ding) Browse Server {%s:%s}" (car method) (car (cdr method))))
    (save-excursion
      (set-buffer nntp-server-buffer)
      (let ((cur (current-buffer)))
	(goto-char 1)
	(while (re-search-forward 
		"\\(^[^ \t]+\\)[ \t]+[0-9]+[ \t]+[0-9]+" nil t)
	  (goto-char (match-end 1))
	  (setq groups (cons (cons (buffer-substring (match-beginning 1)
						     (match-end 1))
				   (- (read cur) (read cur)))
			     groups)))))
    (setq groups (sort groups 
		       (lambda (l1 l2)
			 (string< (car l1) (car l2)))))
    (let ((buffer-read-only nil))
      (while groups
	(setq group (car groups))
	(insert 
	 (format "K%7d: %s\n" (cdr group) (car group)))
	(setq groups (cdr groups))))
    (switch-to-buffer (current-buffer))
    (goto-char 1)
    (gnus-group-position-cursor)))

(defun gnus-browse-server-mode ()
  "Major mode for reading network news."
  (interactive)
  (kill-all-local-variables)
  (setq mode-line-modified "-- ")
  (make-local-variable 'mode-line-format)
  (setq mode-line-format (copy-sequence mode-line-format))
  (and (equal (nth 3 mode-line-format) "   ")
       (setcar (nthcdr 3 mode-line-format) ""))
  (setq major-mode 'gnus-browse-server-mode)
  (setq mode-name "Browse Server")
  (setq mode-line-process nil)
  (use-local-map gnus-browse-server-mode-map)
  (buffer-disable-undo (current-buffer))
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (run-hooks 'gnus-browse-server-mode-hook))

(defun gnus-browse-read-group ()
  "Not implemented, and will probably never be."
  (interactive)
  (error "You can't read while browsing"))

(defun gnus-browse-next-group (n)
  "Go to the next group."
  (interactive "p")
  (prog1
      (forward-line n)
    (gnus-group-position-cursor)))

(defun gnus-browse-prev-group (n)
  "Go to the next group."
  (interactive "p")
  (gnus-browse-next-group (- n)))

(defun gnus-browse-unsubscribe-current-group (arg)
  "(Un)subscribe to the next ARG groups."
  (interactive "p")
  (and (eobp)
       (error "No group at current line."))
  (let ((ward (if (< arg 0) -1 1))
	(arg (abs arg)))
    (while (and (> arg 0)
		(not (eobp))
		(gnus-browse-unsubscribe-group)
		(zerop (gnus-browse-next-group ward)))
      (setq arg (1- arg)))
    (gnus-group-position-cursor)
    (if (/= 0 arg) (message "No more newsgroups" ))
    arg))
  
(defun gnus-browse-unsubscribe-group ()
  (let ((sub nil)
	(buffer-read-only nil)
	group)
    (save-excursion
      (beginning-of-line)
      (if (= (following-char) ?K) (setq sub t))
      (re-search-forward ": \\(.*\\)$" nil t)
      (setq group (gnus-group-prefixed-name 
		   (buffer-substring (match-beginning 1) (match-end 1))
		   gnus-browse-current-method))
      (beginning-of-line)
      (delete-char 1)
      (if sub
	  (progn
	    (gnus-group-change-level 
	     (list t group 3 nil nil gnus-browse-current-method) 3 9 
	     (gnus-gethash (car (nth 1 gnus-newsrc-assoc)) gnus-newsrc-hashtb)
	     t)
	    (insert ? ))
	(gnus-group-change-level group 9 3)
	(insert ?K)))
    t))

(defun gnus-browse-exit ()
  "Quit browsing and return to the group buffer."
  (interactive)
  (if (eq major-mode 'gnus-browse-server-mode)
      (kill-buffer (current-buffer)))
  (switch-to-buffer gnus-group-buffer)
  (gnus-group-list-groups 5))

(defun gnus-browse-describe-briefly ()
  "Give a one line description of the group mode commands."
  (interactive)
  (message
   (substitute-command-keys "\\<gnus-browse-server-mode-map>\\[gnus-group-next-group]:Forward  \\[gnus-group-prev-group]:Backward  \\[gnus-browse-exit]:Exit  \\[gnus-info-find-node]:Run Info  \\[gnus-browse-describe-briefly]:This help")))
      

;;;
;;; Gnus summary mode
;;;

(defvar gnus-summary-mode-map nil)
(defvar gnus-summary-mark-map nil)
(defvar gnus-summary-send-map nil)
(defvar gnus-summary-extract-map nil)
(defvar gnus-summary-extract-view-map nil)
(defvar gnus-summary-article-map nil)
(defvar gnus-summary-thread-map nil)
(defvar gnus-summary-goto-map nil)
(defvar gnus-summary-exit-map nil)
(defvar gnus-summary-various-map nil)
(defvar gnus-summary-interest-map nil)
(defvar gnus-summary-process-map nil)
(defvar gnus-summary-sort-map nil)
(defvar gnus-summary-mgroup-map nil)
(defvar gnus-summary-vkill-map nil)
(defvar gnus-summary-increase-map nil)
(defvar gnus-summary-inc-subject-map nil)
(defvar gnus-summary-inc-author-map nil)
(defvar gnus-summary-inc-xref-map nil)
(defvar gnus-summary-inc-thread-map nil)
(defvar gnus-summary-inc-fol-map nil)
(defvar gnus-summary-lower-map nil)
(defvar gnus-summary-low-subject-map nil)
(defvar gnus-summary-low-author-map nil)
(defvar gnus-summary-low-xref-map nil)
(defvar gnus-summary-low-thread-map nil)
(defvar gnus-summary-low-fol-map nil)

(if gnus-summary-mode-map
    nil
  (setq gnus-summary-mode-map (make-keymap))
  (suppress-keymap gnus-summary-mode-map)

  ;;Non-orthogonal keys

  (define-key gnus-summary-mode-map " " 'gnus-summary-next-page)
  (define-key gnus-summary-mode-map "\177" 'gnus-summary-prev-page)
  (define-key gnus-summary-mode-map "\r" 'gnus-summary-scroll-up)
  (define-key gnus-summary-mode-map "n" 'gnus-summary-next-unread-article)
  (define-key gnus-summary-mode-map "p" 'gnus-summary-prev-unread-article)
  (define-key gnus-summary-mode-map "N" 'gnus-summary-next-article)
  (define-key gnus-summary-mode-map "P" 'gnus-summary-prev-article)
  (define-key gnus-summary-mode-map "\M-\C-n" 'gnus-summary-next-same-subject)
  (define-key gnus-summary-mode-map "\M-\C-p" 'gnus-summary-prev-same-subject)
  (define-key gnus-summary-mode-map "\M-n" 'gnus-summary-next-unread-subject)
  (define-key gnus-summary-mode-map "\M-p" 'gnus-summary-prev-unread-subject)
  (define-key gnus-summary-mode-map "." 'gnus-summary-first-unread-article)
  (define-key gnus-summary-mode-map "," 'gnus-summary-best-unread-article)
  (define-key gnus-summary-mode-map "\M-s" 'gnus-summary-search-article-forward)
  (define-key gnus-summary-mode-map "\M-r" 'gnus-summary-search-article-backward)
  (define-key gnus-summary-mode-map "<" 'gnus-summary-beginning-of-article)
  (define-key gnus-summary-mode-map ">" 'gnus-summary-end-of-article)
  (define-key gnus-summary-mode-map "j" 'gnus-summary-goto-subject)
  (define-key gnus-summary-mode-map "^" 'gnus-summary-refer-parent-article)
  (define-key gnus-summary-mode-map "\M-^" 'gnus-summary-refer-article)
  (define-key gnus-summary-mode-map "u" 'gnus-summary-tick-article-forward)
  (define-key gnus-summary-mode-map "!" 'gnus-summary-tick-article-forward)
  (define-key gnus-summary-mode-map "U" 'gnus-summary-tick-article-backward)
  (define-key gnus-summary-mode-map "d" 'gnus-summary-mark-as-read-forward)
  (define-key gnus-summary-mode-map "D" 'gnus-summary-mark-as-read-backward)
  (define-key gnus-summary-mode-map "E" 'gnus-summary-mark-as-expirable)
  (define-key gnus-summary-mode-map "\M-u" 'gnus-summary-clear-mark-forward)
  (define-key gnus-summary-mode-map "\M-U" 'gnus-summary-clear-mark-backward)
  (define-key gnus-summary-mode-map "k" 'gnus-summary-kill-same-subject-and-select)
  (define-key gnus-summary-mode-map "\C-k" 'gnus-summary-kill-same-subject)
  (define-key gnus-summary-mode-map "\M-\C-k" 'gnus-summary-kill-thread)
  (define-key gnus-summary-mode-map "\M-\C-l" 'gnus-summary-lower-thread)
  (define-key gnus-summary-mode-map "e" 'gnus-summary-edit-article)
  (define-key gnus-summary-mode-map "#" 'gnus-summary-mark-as-processable)
  (define-key gnus-summary-mode-map "\M-#" 'gnus-summary-unmark-as-processable)
  (define-key gnus-summary-mode-map "\M-\C-t" 'gnus-summary-toggle-threads)
  (define-key gnus-summary-mode-map "\M-\C-s" 'gnus-summary-show-thread)
  (define-key gnus-summary-mode-map "\M-\C-h" 'gnus-summary-hide-thread)
  (define-key gnus-summary-mode-map "\M-\C-f" 'gnus-summary-next-thread)
  (define-key gnus-summary-mode-map "\M-\C-b" 'gnus-summary-prev-thread)
  (define-key gnus-summary-mode-map "\M-\C-u" 'gnus-summary-up-thread)
  (define-key gnus-summary-mode-map "\M-\C-d" 'gnus-summary-down-thread)
  (define-key gnus-summary-mode-map "&" 'gnus-summary-execute-command)
  (define-key gnus-summary-mode-map "c" 'gnus-summary-catchup-and-exit)
  (define-key gnus-summary-mode-map "\C-t" 'gnus-summary-toggle-truncation)
  (define-key gnus-summary-mode-map "?" 'gnus-summary-mark-as-dormant)
  (define-key gnus-summary-mode-map "\C-c\M-\C-s" 'gnus-summary-show-all-expunged)
  (define-key gnus-summary-mode-map "\C-c\C-s\C-n" 'gnus-summary-sort-by-number)
  (define-key gnus-summary-mode-map "\C-c\C-s\C-a" 'gnus-summary-sort-by-author)
  (define-key gnus-summary-mode-map "\C-c\C-s\C-s" 'gnus-summary-sort-by-subject)
  (define-key gnus-summary-mode-map "\C-c\C-s\C-d" 'gnus-summary-sort-by-date)
  (define-key gnus-summary-mode-map "\C-c\C-s\C-i" 'gnus-summary-sort-by-score)
  (define-key gnus-summary-mode-map "=" 'gnus-summary-expand-window)
  (define-key gnus-summary-mode-map "\C-x\C-s" 'gnus-summary-reselect-current-group)
  (define-key gnus-summary-mode-map "\M-g" 'gnus-summary-rescan-group)
  (define-key gnus-summary-mode-map "w" 'gnus-summary-stop-page-breaking)
  (define-key gnus-summary-mode-map "\C-c\C-r" 'gnus-summary-caesar-message)
  (define-key gnus-summary-mode-map "\M-t" 'gnus-summary-toggle-mime)
  (define-key gnus-summary-mode-map "\C-d" 'gnus-summary-enter-digest-group)
  (define-key gnus-summary-mode-map "f" 'gnus-summary-followup)
  (define-key gnus-summary-mode-map "F" 'gnus-summary-followup-with-original)
  (define-key gnus-summary-mode-map "C" 'gnus-summary-cancel-article)
  (define-key gnus-summary-mode-map "r" 'gnus-summary-reply)
  (define-key gnus-summary-mode-map "R" 'gnus-summary-reply-with-original)
  (define-key gnus-summary-mode-map "\C-c\C-f" 'gnus-summary-mail-forward)
  (define-key gnus-summary-mode-map "o" 'gnus-summary-save-article)
  (define-key gnus-summary-mode-map "\C-o" 'gnus-summary-save-article-mail)
  (define-key gnus-summary-mode-map "|" 'gnus-summary-pipe-output)
  (define-key gnus-summary-mode-map "\M-k" 'gnus-summary-edit-local-kill)
  (define-key gnus-summary-mode-map "\M-K" 'gnus-summary-edit-global-kill)
  (define-key gnus-summary-mode-map "V" 'gnus-version)
  (define-key gnus-summary-mode-map "\C-c\C-d" 'gnus-summary-describe-group)
  (define-key gnus-summary-mode-map "q" 'gnus-summary-exit)
  (define-key gnus-summary-mode-map "Q" 'gnus-summary-quit)
  (define-key gnus-summary-mode-map "\C-c\C-i" 'gnus-info-find-node)
  (define-key gnus-summary-mode-map [mouse-2] 'gnus-mouse-pick-article)
  (define-key gnus-summary-mode-map "m" 'gnus-summary-mail-other-window)
  (define-key gnus-summary-mode-map "a" 'gnus-summary-post-news)
  (define-key gnus-summary-mode-map "x" 'gnus-summary-delete-marked-as-read)
  (define-key gnus-summary-mode-map "X" 'gnus-summary-remove-lines-marked-with)
  (define-key gnus-summary-mode-map "s" 'gnus-summary-isearch-article)
  (define-key gnus-summary-mode-map "t" 'gnus-summary-toggle-header)
  (define-key gnus-summary-mode-map "g" 'gnus-summary-show-article)
;  (define-key gnus-summary-mode-map "?" 'gnus-summary-describe-briefly)
  (define-key gnus-summary-mode-map "l" 'gnus-summary-goto-last-article)
  (define-key gnus-summary-mode-map "\C-c\C-v\C-v" 'gnus-uu-decode-uu)


  ;; Orthogonal keymap
  (define-prefix-command 'gnus-summary-mark-map)
  (define-key gnus-summary-mode-map "M" 'gnus-summary-mark-map)
  (define-key gnus-summary-mark-map "t" 'gnus-summary-tick-article-forward)
  (define-key gnus-summary-mark-map "!" 'gnus-summary-tick-article-forward)
  (define-key gnus-summary-mark-map "d" 'gnus-summary-mark-as-read-forward)
  (define-key gnus-summary-mark-map "r" 'gnus-summary-mark-as-read-forward)
  (define-key gnus-summary-mark-map "c" 'gnus-summary-clear-mark-forward)
  (define-key gnus-summary-mark-map " " 'gnus-summary-clear-mark-forward)
  (define-key gnus-summary-mark-map "e" 'gnus-summary-mark-as-expirable)
  (define-key gnus-summary-mark-map "x" 'gnus-summary-mark-as-expirable)
  (define-key gnus-summary-mark-map "?" 'gnus-summary-mark-as-dormant)
  (define-key gnus-summary-mark-map "b" 'gnus-summary-set-bookmark)
  (define-key gnus-summary-mark-map "B" 'gnus-summary-remove-bookmark)
  (define-key gnus-summary-mark-map "#" 'gnus-summary-mark-as-processable)
  (define-key gnus-summary-mark-map "\M-#" 'gnus-summary-unmark-as-processable)
  (define-key gnus-summary-mark-map "\M-r" 'gnus-summary-remove-lines-marked-as-read)
  (define-key gnus-summary-mark-map "\M-\C-r" 'gnus-summary-remove-lines-marked-with)
  (define-key gnus-summary-mark-map "D" 'gnus-summary-show-all-dormant)
  (define-key gnus-summary-mark-map "S" 'gnus-summary-show-all-expunged)
  (define-key gnus-summary-mark-map "C" 'gnus-summary-catchup)
  (define-key gnus-summary-mark-map "H" 'gnus-summary-catchup-to-here)
  (define-key gnus-summary-mark-map "\C-c" 'gnus-summary-catchup-all)
  (define-key gnus-summary-mark-map "a" 'gnus-summary-clear-above)
  (define-key gnus-summary-mark-map "A" 'gnus-summary-tick-above)

  (define-prefix-command 'gnus-summary-process-map)
  (define-key gnus-summary-mark-map "p" 'gnus-summary-process-map)
  (define-key gnus-summary-process-map "p" 'gnus-summary-mark-as-processable)
  (define-key gnus-summary-process-map "u" 'gnus-summary-unmark-as-processable)
  (define-key gnus-summary-process-map "U" 'gnus-summary-unmark-all-processable)
  (define-key gnus-summary-process-map "s" 'gnus-uu-mark-series)
  (define-key gnus-summary-process-map "r" 'gnus-uu-mark-region)
  (define-key gnus-summary-process-map "R" 'gnus-uu-mark-by-regexp)
  (define-key gnus-summary-process-map "t" 'gnus-uu-mark-thread)
  (define-key gnus-summary-process-map "a" 'gnus-uu-mark-all)
  (define-key gnus-summary-process-map "S" 'gnus-uu-mark-sparse)
  

  (define-prefix-command 'gnus-summary-send-map)
  (define-key gnus-summary-mode-map "S" 'gnus-summary-send-map)
  (define-key gnus-summary-send-map "p" 'gnus-summary-post-news)
  (define-key gnus-summary-send-map "f" 'gnus-summary-followup)
  (define-key gnus-summary-send-map "F" 'gnus-summary-followup-with-original)
  (define-key gnus-summary-send-map "b" 'gnus-summary-followup-and-reply)
  (define-key gnus-summary-send-map "B" 'gnus-summary-followup-and-reply-with-original)
  (define-key gnus-summary-send-map "c" 'gnus-summary-cancel-article)
  (define-key gnus-summary-send-map "s" 'gnus-summary-supersede-article)
  (define-key gnus-summary-send-map "r" 'gnus-summary-reply)
  (define-key gnus-summary-send-map "R" 'gnus-summary-reply-with-original)
  (define-key gnus-summary-send-map "\C-f" 'gnus-summary-mail-forward)
  (define-key gnus-summary-send-map "m" 'gnus-summary-mail-other-window)
  (define-key gnus-summary-send-map "u" 'gnus-uu-post-news)
  (define-key gnus-summary-send-map "\M-f" 'gnus-uu-digest-and-forward)

  
  (define-prefix-command 'gnus-summary-goto-map)
  (define-key gnus-summary-mode-map "G" 'gnus-summary-goto-map)
  (define-key gnus-summary-goto-map "n" 'gnus-summary-next-unread-article)
  (define-key gnus-summary-goto-map "p" 'gnus-summary-prev-unread-article)
  (define-key gnus-summary-goto-map "N" 'gnus-summary-next-article)
  (define-key gnus-summary-goto-map "P" 'gnus-summary-prev-article)
  (define-key gnus-summary-goto-map "\C-n" 'gnus-summary-next-same-subject)
  (define-key gnus-summary-goto-map "\C-p" 'gnus-summary-prev-same-subject)
  (define-key gnus-summary-goto-map "\M-n" 'gnus-summary-next-unread-subject)
  (define-key gnus-summary-goto-map "\M-p" 'gnus-summary-prev-unread-subject)
  (define-key gnus-summary-goto-map "f" 'gnus-summary-first-unread-article)
  (define-key gnus-summary-goto-map "b" 'gnus-summary-best-unread-article)
  (define-key gnus-summary-goto-map "g" 'gnus-summary-goto-subject)
  (define-key gnus-summary-goto-map "l" 'gnus-summary-goto-last-article)


  (define-prefix-command 'gnus-summary-thread-map)
  (define-key gnus-summary-mode-map "T" 'gnus-summary-thread-map)
  (define-key gnus-summary-thread-map "k" 'gnus-summary-kill-thread)
  (define-key gnus-summary-thread-map "l" 'gnus-summary-lower-thread)
  (define-key gnus-summary-thread-map "r" 'gnus-summary-raise-thread)
  (define-key gnus-summary-thread-map "T" 'gnus-summary-toggle-threads)
  (define-key gnus-summary-thread-map "s" 'gnus-summary-show-thread)
  (define-key gnus-summary-thread-map "S" 'gnus-summary-show-all-threads)
  (define-key gnus-summary-thread-map "h" 'gnus-summary-hide-thread)
  (define-key gnus-summary-thread-map "H" 'gnus-summary-hide-all-threads)
  (define-key gnus-summary-thread-map "n" 'gnus-summary-next-thread)
  (define-key gnus-summary-thread-map "p" 'gnus-summary-prev-thread)
  (define-key gnus-summary-thread-map "u" 'gnus-summary-up-thread)
  (define-key gnus-summary-thread-map "d" 'gnus-summary-down-thread)
  (define-key gnus-summary-thread-map "#" 'gnus-uu-mark-thread)

  
  (define-prefix-command 'gnus-summary-exit-map)
  (define-key gnus-summary-mode-map "\M-e" 'gnus-summary-exit-map)
  (define-key gnus-summary-exit-map "c" 'gnus-summary-catchup-and-exit)
  (define-key gnus-summary-exit-map "\C-c" 'gnus-summary-catchup-all-and-exit)
  (define-key gnus-summary-exit-map "q" 'gnus-summary-exit)
  (define-key gnus-summary-exit-map "e" 'gnus-summary-exit)
  (define-key gnus-summary-exit-map "Q" 'gnus-summary-quit)
  (define-key gnus-summary-exit-map "E" 'gnus-summary-quit)


  (define-prefix-command 'gnus-summary-article-map)
  (define-key gnus-summary-mode-map "A" 'gnus-summary-article-map)
  (define-key gnus-summary-article-map " " 'gnus-summary-next-page)
  (define-key gnus-summary-article-map "n" 'gnus-summary-next-page)
  (define-key gnus-summary-article-map "\177" 'gnus-summary-prev-page)
  (define-key gnus-summary-article-map "p" 'gnus-summary-prev-page)
  (define-key gnus-summary-article-map "\r" 'gnus-summary-scroll-up)
  (define-key gnus-summary-article-map "<" 'gnus-summary-beginning-of-article)
  (define-key gnus-summary-article-map ">" 'gnus-summary-end-of-article)
  (define-key gnus-summary-article-map "b" 'gnus-summary-beginning-of-article)
  (define-key gnus-summary-article-map "e" 'gnus-summary-end-of-article)
  (define-key gnus-summary-article-map "^" 'gnus-summary-refer-parent-article)
  (define-key gnus-summary-article-map "r" 'gnus-summary-refer-parent-article)
  (define-key gnus-summary-article-map "w" 'gnus-summary-stop-page-breaking)
  (define-key gnus-summary-article-map "c" 'gnus-summary-caesar-message)
  (define-key gnus-summary-article-map "g" 'gnus-summary-show-article)
  (define-key gnus-summary-article-map "t" 'gnus-summary-toggle-header)
  (define-key gnus-summary-article-map "hh" 'gnus-article-hide-headers)
  (define-key gnus-summary-article-map "hs" 'gnus-article-hide-signature)
  (define-key gnus-summary-article-map "hc" 'gnus-article-hide-citation)
  (define-key gnus-summary-article-map "ho" 'gnus-article-treat-overstrike)
  (define-key gnus-summary-article-map "hw" 'gnus-article-word-wrap)
  (define-key gnus-summary-article-map "hd" 'gnus-article-remove-cr)
  (define-key gnus-summary-article-map "hq" 'gnus-article-de-quoted-unreadable)
  (define-key gnus-summary-article-map "m" 'gnus-summary-toggle-mime)
  (define-key gnus-summary-article-map "s" 'gnus-summary-isearch-article)


  (define-prefix-command 'gnus-summary-extract-map)
  (define-key gnus-summary-mode-map "X" 'gnus-summary-extract-map)
;  (define-key gnus-summary-extract-map "x" 'gnus-summary-extract-any)
;  (define-key gnus-summary-extract-map "m" 'gnus-summary-extract-mime)

  (define-key gnus-summary-extract-map "u" 'gnus-uu-decode-uu)
  (define-key gnus-summary-extract-map "U" 'gnus-uu-decode-uu-and-save)
  (define-key gnus-summary-extract-map "s" 'gnus-uu-decode-unshar)
  (define-key gnus-summary-extract-map "S" 'gnus-uu-decode-unshar-and-save)
  (define-key gnus-summary-extract-map "o" 'gnus-uu-decode-save)
  (define-key gnus-summary-extract-map "O" 'gnus-uu-decode-save)
  (define-key gnus-summary-extract-map "b" 'gnus-uu-decode-binhex)
  (define-key gnus-summary-extract-map "B" 'gnus-uu-decode-binhex)

  (define-prefix-command 'gnus-summary-extract-view-map)
  (define-key gnus-summary-extract-map "v" 'gnus-summary-extract-view-map)
  (define-key gnus-summary-extract-view-map "u" 'gnus-uu-decode-uu-view)
  (define-key gnus-summary-extract-view-map "U" 'gnus-uu-decode-uu-and-save-view)
  (define-key gnus-summary-extract-view-map "s" 'gnus-uu-decode-unshar-view)
  (define-key gnus-summary-extract-view-map "S" 'gnus-uu-decode-unshar-and-save-view)
  (define-key gnus-summary-extract-view-map "o" 'gnus-uu-decode-save-view)
  (define-key gnus-summary-extract-view-map "O" 'gnus-uu-decode-save-view)
  (define-key gnus-summary-extract-view-map "b" 'gnus-uu-decode-binhex-view)
  (define-key gnus-summary-extract-view-map "B" 'gnus-uu-decode-binhex-view)
  
  
  (define-prefix-command 'gnus-summary-various-map)
  (define-key gnus-summary-mode-map "V" 'gnus-summary-various-map)
  (define-key gnus-summary-various-map "u" 'gnus-summary-universal-argument)
  (define-key gnus-summary-various-map "\M-s" 'gnus-summary-search-article-forward)
  (define-key gnus-summary-various-map "\M-r" 'gnus-summary-search-article-backward)
  (define-key gnus-summary-various-map "r" 'gnus-summary-refer-article)
  (define-key gnus-summary-various-map "&" 'gnus-summary-execute-command)
  (define-key gnus-summary-various-map "\C-t" 'gnus-summary-toggle-truncation)
  (define-key gnus-summary-various-map "=" 'gnus-summary-expand-window)
  (define-key gnus-summary-various-map "\C-s" 'gnus-summary-reselect-current-group)
  (define-key gnus-summary-various-map "g" 'gnus-summary-rescan-group)
  (define-key gnus-summary-various-map "o" 'gnus-summary-save-article)
  (define-key gnus-summary-various-map "\C-o" 'gnus-summary-save-article-mail)
  (define-key gnus-summary-various-map "|" 'gnus-summary-pipe-output)
  (define-key gnus-summary-various-map "V" 'gnus-version)
  (define-key gnus-summary-various-map "d" 'gnus-summary-describe-group)
  (define-key gnus-summary-various-map "?" 'gnus-summary-describe-briefly)
  (define-key gnus-summary-various-map "i" 'gnus-info-find-node)
  (define-key gnus-summary-various-map "S" 'gnus-summary-set-score)
  (define-key gnus-summary-various-map "b" 'gnus-summary-set-mark-below)
  (define-key gnus-summary-various-map "D" 'gnus-summary-enter-digest-group)

  (define-prefix-command 'gnus-summary-sort-map)
  (define-key gnus-summary-various-map "s" 'gnus-summary-sort-map)
  (define-key gnus-summary-sort-map "n" 'gnus-summary-sort-by-number)
  (define-key gnus-summary-sort-map "a" 'gnus-summary-sort-by-author)
  (define-key gnus-summary-sort-map "s" 'gnus-summary-sort-by-subject)
  (define-key gnus-summary-sort-map "d" 'gnus-summary-sort-by-date)
  (define-key gnus-summary-sort-map "i" 'gnus-summary-sort-by-score)

  (define-prefix-command 'gnus-summary-mgroup-map)
  (define-key gnus-summary-various-map "m" 'gnus-summary-mgroup-map)
  (define-key gnus-summary-mgroup-map "e" 'gnus-summary-expire-articles)
  (define-key gnus-summary-mgroup-map "\177" 'gnus-summary-delete-article)
  (define-key gnus-summary-mgroup-map "m" 'gnus-summary-move-article)
  (define-key gnus-summary-mgroup-map "r" 'gnus-summary-respool-article)
  (define-key gnus-summary-mgroup-map "w" 'gnus-summary-edit-article)

  (define-prefix-command 'gnus-summary-vkill-map)
  (define-key gnus-summary-various-map "k" 'gnus-summary-vkill-map)
  (define-key gnus-summary-vkill-map "k" 'gnus-summary-kill-same-subject-and-select)
  (define-key gnus-summary-vkill-map "K" 'gnus-summary-kill-same-subject)
  (define-key gnus-summary-vkill-map "\M-k" 'gnus-summary-edit-local-kill)
  (define-key gnus-summary-vkill-map "\M-K" 'gnus-summary-edit-global-kill)
  (define-key gnus-summary-vkill-map "x" 'gnus-kill-file-set-expunge-below)
  (define-key gnus-summary-vkill-map "m" 'gnus-kill-file-set-mark-below)



  (define-prefix-command 'gnus-summary-increase-map)
  (define-key gnus-summary-mode-map "I" 'gnus-summary-increase-map)
  (define-key gnus-summary-increase-map "i" 'gnus-summary-raise-same-subject-and-select)
  (define-key gnus-summary-increase-map "I" 'gnus-summary-raise-same-subject)
  (define-key gnus-summary-increase-map "\C-i" 'gnus-summary-raise-score)

  (define-prefix-command 'gnus-summary-inc-subject-map)
  (define-key gnus-summary-increase-map "s" 'gnus-summary-inc-subject-map)
  (define-key gnus-summary-increase-map "S" 'gnus-summary-temporarily-raise-by-subject)
  (define-key gnus-summary-inc-subject-map "s" 'gnus-summary-temporarily-raise-by-subject)
  (define-key gnus-summary-inc-subject-map "S" 'gnus-summary-raise-by-subject)
  (define-key gnus-summary-inc-subject-map "t" 'gnus-summary-temporarily-raise-by-subject)
  (define-key gnus-summary-inc-subject-map "p" 'gnus-summary-raise-by-subject)

  (define-prefix-command 'gnus-summary-inc-author-map)
  (define-key gnus-summary-increase-map "a" 'gnus-summary-inc-author-map)
  (define-key gnus-summary-increase-map "A" 'gnus-summary-temporarily-raise-by-author)
  (define-key gnus-summary-inc-author-map "a" 'gnus-summary-temporarily-raise-by-author)
  (define-key gnus-summary-inc-author-map "A" 'gnus-summary-raise-by-author)
  (define-key gnus-summary-inc-author-map "t" 'gnus-summary-temporarily-raise-by-author)
  (define-key gnus-summary-inc-author-map "p" 'gnus-summary-raise-by-author)

  (define-prefix-command 'gnus-summary-inc-thread-map)
  (define-key gnus-summary-increase-map "t" 'gnus-summary-inc-thread-map)
  (define-key gnus-summary-increase-map "T" 'gnus-summary-temporarily-raise-by-thread)
  (define-key gnus-summary-inc-thread-map "t" 'gnus-summary-temporarily-raise-by-thread)
  (define-key gnus-summary-inc-thread-map "T" 'gnus-summary-raise-by-thread)
  (define-key gnus-summary-inc-thread-map "t" 'gnus-summary-temporarily-raise-by-thread)
  (define-key gnus-summary-inc-thread-map "p" 'gnus-summary-raise-by-thread)

  (define-prefix-command 'gnus-summary-inc-xref-map)
  (define-key gnus-summary-increase-map "x" 'gnus-summary-inc-xref-map)
  (define-key gnus-summary-increase-map "X" 'gnus-summary-temporarily-raise-by-xref)
  (define-key gnus-summary-inc-xref-map "x" 'gnus-summary-temporarily-raise-by-xref)
  (define-key gnus-summary-inc-xref-map "X" 'gnus-summary-raise-by-xref)
  (define-key gnus-summary-inc-xref-map "t" 'gnus-summary-temporarily-raise-by-xref)
  (define-key gnus-summary-inc-xref-map "p" 'gnus-summary-raise-by-xref)

  (define-prefix-command 'gnus-summary-inc-fol-map)
  (define-key gnus-summary-increase-map "f" 'gnus-summary-inc-fol-map)
  (define-key gnus-summary-increase-map "F" 'gnus-summary-raise-followups-to-author)
  (define-key gnus-summary-inc-fol-map "f" 'gnus-summary-raise-followups-to-author)
  (define-key gnus-summary-inc-fol-map "F" 'gnus-summary-raise-followups-to-author)
  (define-key gnus-summary-inc-fol-map "t" 'gnus-summary-raise-followups-to-author)
  (define-key gnus-summary-inc-fol-map "p" 'gnus-summary-raise-followups-to-author)

  (define-prefix-command 'gnus-summary-lower-map)
  (define-key gnus-summary-mode-map "L" 'gnus-summary-lower-map)
  (define-key gnus-summary-lower-map "l" 'gnus-summary-lower-same-subject-and-select)
  (define-key gnus-summary-lower-map "L" 'gnus-summary-lower-same-subject)
  (define-key gnus-summary-lower-map "\C-l" 'gnus-summary-lower-score)

  (define-prefix-command 'gnus-summary-low-subject-map)
  (define-key gnus-summary-lower-map "s" 'gnus-summary-low-subject-map)
  (define-key gnus-summary-lower-map "S" 'gnus-summary-temporarily-lower-by-subject)
  (define-key gnus-summary-low-subject-map "s" 'gnus-summary-temporarily-lower-by-subject)
  (define-key gnus-summary-low-subject-map "S" 'gnus-summary-lower-by-subject)
  (define-key gnus-summary-low-subject-map "t" 'gnus-summary-temporarily-lower-by-subject)
  (define-key gnus-summary-low-subject-map "p" 'gnus-summary-lower-by-subject)

  (define-prefix-command 'gnus-summary-low-author-map)
  (define-key gnus-summary-lower-map "a" 'gnus-summary-low-author-map)
  (define-key gnus-summary-lower-map "A" 'gnus-summary-temporarily-lower-by-author)
  (define-key gnus-summary-low-author-map "a" 'gnus-summary-temporarily-lower-by-author)
  (define-key gnus-summary-low-author-map "A" 'gnus-summary-lower-by-author)
  (define-key gnus-summary-low-author-map "t" 'gnus-summary-temporarily-lower-by-author)
  (define-key gnus-summary-low-author-map "p" 'gnus-summary-lower-by-author)

  (define-prefix-command 'gnus-summary-low-thread-map)
  (define-key gnus-summary-lower-map "t" 'gnus-summary-low-thread-map)
  (define-key gnus-summary-lower-map "T" 'gnus-summary-temporarily-lower-by-thread)
  (define-key gnus-summary-low-thread-map "t" 'gnus-summary-temporarily-lower-by-thread)
  (define-key gnus-summary-low-thread-map "T" 'gnus-summary-lower-by-thread)
  (define-key gnus-summary-low-thread-map "t" 'gnus-summary-temporarily-lower-by-thread)
  (define-key gnus-summary-low-thread-map "p" 'gnus-summary-lower-by-thread)

  (define-prefix-command 'gnus-summary-low-xref-map)
  (define-key gnus-summary-lower-map "x" 'gnus-summary-low-xref-map)
  (define-key gnus-summary-lower-map "X" 'gnus-summary-temporarily-lower-by-xref)
  (define-key gnus-summary-low-xref-map "x" 'gnus-summary-temporarily-lower-by-xref)
  (define-key gnus-summary-low-xref-map "X" 'gnus-summary-lower-by-xref)
  (define-key gnus-summary-low-xref-map "t" 'gnus-summary-temporarily-lower-by-xref)
  (define-key gnus-summary-low-xref-map "p" 'gnus-summary-lower-by-xref)

  (define-prefix-command 'gnus-summary-low-fol-map)
  (define-key gnus-summary-lower-map "f" 'gnus-summary-low-fol-map)
  (define-key gnus-summary-lower-map "F" 'gnus-summary-lower-followups-to-author)
  (define-key gnus-summary-low-fol-map "f" 'gnus-summary-lower-followups-to-author)
  (define-key gnus-summary-low-fol-map "F" 'gnus-summary-lower-followups-to-author)
  (define-key gnus-summary-low-fol-map "t" 'gnus-summary-lower-followups-to-author)
  (define-key gnus-summary-low-fol-map "p" 'gnus-summary-lower-followups-to-author)
  )




(defun gnus-summary-mode ()
  "Major mode for reading articles in this newsgroup.
All normal editing commands are switched off.
The following commands are available:

\\<gnus-summary-mode-map>
\\[gnus-summary-next-page]\t Scroll the article buffer a page forwards
\\[gnus-summary-prev-page]\t Scroll the article buffer a page backwards
\\[gnus-summary-scroll-up]\t Scroll the article buffer one line forwards
\\[gnus-summary-next-unread-article]\t Go to the next unread article
\\[gnus-summary-prev-unread-article]\t Go to the previous unread article
\\[gnus-summary-next-article]\t Go to the next article
\\[gnus-summary-prev-article]\t Go to the previous article
\\[gnus-summary-next-same-subject]\t Go to the next summary line with the same subject
\\[gnus-summary-prev-same-subject]\t Go to the previous summary line with the same subject
\\[gnus-summary-next-subject]\t Go to the next summary line
\\[gnus-summary-prev-subject]\t Go to the previous summary line
\\[gnus-summary-next-unread-subject]\t Go to the next unread summary line
\\[gnus-summary-prev-unread-subject]\t Go to the previous unread summary line
\\[gnus-summary-first-unread-article]\t Go to the first unread article
\\[gnus-summary-best-unread-article]\t Go to the unread article with the highest score
\\[gnus-summary-goto-subject]\t Go to some subject
\\[gnus-summary-goto-last-article]\t Go to the previous article

\\[gnus-summary-beginning-of-article]\t Go to the beginning of the article
\\[gnus-summary-end-of-article]\t Go to the end of the article

\\[gnus-summary-refer-parent-article]\t Get the parent of the current article from the server
\\[gnus-summary-refer-article]\t Request some article by Message-ID from the server

\\[gnus-summary-isearch-article]\t Do an interactive search on the current article
\\[gnus-summary-search-article-forward]\t Search all articles forward for a regular expression
\\[gnus-summary-search-article-backward]\t Search all articles backward for a regular expression

\\[gnus-summary-tick-article-forward]\t Tick current article and move forward
\\[gnus-summary-tick-article-backward]\t Tick current article and move backward
\\[gnus-summary-mark-as-read-forward]\t Mark the current article as read and move forward
\\[gnus-summary-mark-as-read-backward]\t Mark the current article as read and move backward
\\[gnus-summary-clear-mark-forward]\t Clear tick and read marks and move forward
\\[gnus-summary-clear-mark-backward]\t Clear tick and read marks and move backward
\\[gnus-summary-mark-as-processable]\t Set the process mark on the current article
\\[gnus-summary-unmark-as-processable]\t Remove the process mark from the current article
\\[gnus-summary-unmark-all-processable]\t Remove the process mark from all articles

\\[gnus-summary-raise-same-subject-and-select]\t Raise all articles with the current subject and select the next article
\\[gnus-summary-raise-same-subject]\t Raise all articles with the current subject
\\[gnus-summary-lower-same-subject-and-select]\t Lower all articles with the current subject and select the next article
\\[gnus-summary-lower-same-subject]\t Lower all articles with the current subject

\\[gnus-summary-toggle-threads]\t Toggle thread display
\\[gnus-summary-show-thread]\t Show the current thread
\\[gnus-summary-hide-thread]\t Hide the current thread
\\[gnus-summary-next-thread]\t Go to the next thread
\\[gnus-summary-prev-thread]\t Go to the previous thread
\\[gnus-summary-up-thread]\t Go up the current thread
\\[gnus-summary-down-thread]\t Descend the current thread
\\[gnus-summary-raise-thread]\t Raise the current thread
\\[gnus-summary-lower-thread]\t Lower the current thread
\\[gnus-summary-mark-as-expirable]\t Mark the current artivles as expirable
\\[gnus-summary-remove-lines-marked-as-read]\t Remove all articles that are marked as read
\\[gnus-summary-remove-lines-marked-with]\t Remove all articles that have some mark

\\[gnus-summary-execute-command]\t Execute a command
\\[gnus-summary-catchup-and-exit]\t Mark all unread articles as read and exit
\\[gnus-summary-toggle-truncation]\t Toggle truncation of summary lines
\\[gnus-summary-expand-window]\t Expand the summary window
\\[gnus-summary-universal-argument]\t Run a command on all articles with the process mark

\\[gnus-summary-sort-by-number]\t Sort the summary buffer by article number
\\[gnus-summary-sort-by-author]\t Sort the summary buffer by author
\\[gnus-summary-sort-by-subject]\t Sort the summary buffer by subject
\\[gnus-summary-sort-by-date]\t Sort the summary buffer by date

\\[gnus-summary-reselect-current-group]\t Exit and reselect the current group
\\[gnus-summary-rescan-group]\t Exit, get new articles and reselect the group
\\[gnus-summary-stop-page-breaking]\t Stop page breaking of the current article
\\[gnus-summary-caesar-message]\t Caesar rotate (rot13) the current article
\\[gnus-summary-show-article]\t Reselect the current article
\\[gnus-summary-toggle-header]\t Toggle header display
\\[gnus-summary-toggle-mime]\t Toggle whether to use MIME
\\[gnus-summary-enter-digest-group]\t Enter a newsgroup based on the current digest
\\[gnus-summary-post-news]\t Post an article to the current group
\\[gnus-summary-followup]\t Post a followup to the current article
\\[gnus-summary-followup-with-original]\t Post a followup and include the original article
\\[gnus-summary-cancel-article]\t Cancel the current article
\\[gnus-summary-supersede-article]\t Supersede the current article
\\[gnus-summary-reply]\t Mail a reply to the author of the current article
\\[gnus-summary-reply-with-original]\t Mail a reply and include the current article
\\[gnus-summary-mail-forward]\t Forward the current article
\\[gnus-summary-mail-other-window]\t Mail in the other window
\\[gnus-summary-save-article]\t Save the current article
\\[gnus-summary-save-article-mail]\t Save the current article in rmail format
\\[gnus-summary-pipe-output]\t Pipe the current article to a process
\\[gnus-summary-move-article]\t Move the article to a different newsgroup
\\[gnus-summary-respool-article]\t Respool the article
\\[gnus-summary-edit-local-kill]\t Edit the local kill file
\\[gnus-summary-edit-global-kill]\t Edit the global kill file
\\[gnus-version]\t Display the current Gnus version
\\[gnus-summary-exit]\t Exit the summary buffer 
\\[gnus-summary-quit]\t Exit the summary buffer without saving any changes
\\[gnus-summary-describe-group]\t Describe the current newsgroup
\\[gnus-summary-describe-briefly]\t Give a brief key overview
\\[gnus-info-find-node]\t Go to the Gnus info node

\\[gnus-kill-file-set-expunge-below]	Automatically expunge articles below LEVEL.

\\[gnus-kill-file-set-mark-below]	Automatically mark articles below LEVEL.
\\[gnus-summary-temporarily-raise-by-subject]\t Temporarily raise score for articles with the current subject
\\[gnus-summary-temporarily-raise-by-author]\t Temporarily raise score for articles from the current author
\\[gnus-summary-temporarily-raise-by-xref]\t Temporarily raise score for articles with the current cross-posting
\\[gnus-summary-raise-by-subject]\t Permanently raise score for articles with the current subject
\\[gnus-summary-raise-by-author]\t Permanently raise score for articles from the current author
\\[gnus-summary-raise-followups-to-author]\t Permanently raise score for followups to the current author
\\[gnus-summary-raise-by-xref]\t Permanently raise score for articles with the current cross-posting
\\[gnus-summary-temporarily-lower-by-subject]\t Temporarily lower score for articles with the current subject
\\[gnus-summary-temporarily-lower-by-author]\t Temporarily lower score for articles from the current author
\\[gnus-summary-temporarily-lower-by-xref]\t Temporarily lower score for articles with the current cross-posting
\\[gnus-summary-lower-by-subject]\t Permanently lower score for articles with the current subject
\\[gnus-summary-lower-by-author]\t Permanently lower score for articles from the current author
\\[gnus-summary-lower-followups-to-author]\t Permanently lower score for followups to the current author
\\[gnus-summary-lower-by-thread]\t Permanently lower score for articles in the current thread
\\[gnus-summary-lower-by-xref]\t Permanently lower score for articles with the current cross-posting
"
  (interactive)
  (if gnus-visual (gnus-summary-make-menu-bar))
  (kill-all-local-variables)
  (let ((locals gnus-summary-local-variables))
    (while locals
      (if (consp (car locals))
	  (progn
	    (make-local-variable (car (car locals)))
	    (set (car (car locals)) (eval (cdr (car locals)))))
	(make-local-variable (car locals))
	(set (car locals) nil))
      (setq locals (cdr locals))))
  (gnus-update-format-specifications)
  (setq mode-line-modified "-- ")
  (make-local-variable 'mode-line-format)
  (setq mode-line-format (copy-sequence mode-line-format))
  (and (equal (nth 3 mode-line-format) "   ")
       (setcar (nthcdr 3 mode-line-format) ""))
  (setq major-mode 'gnus-summary-mode)
  (setq mode-name "Summary")
  (make-local-variable 'minor-mode-alist)
;  (or (assq 'gnus-show-threads minor-mode-alist)
;      (setq minor-mode-alist
;	    (cons (list 'gnus-show-threads " Thread") minor-mode-alist)))
  (gnus-set-mode-line 'summary)
  (use-local-map gnus-summary-mode-map)
  (buffer-disable-undo (current-buffer))
  (setq buffer-read-only t)		;Disable modification
  (setq truncate-lines t)
  (setq selective-display t)
  (setq selective-display-ellipses t)	;Display `...'
  (run-hooks 'gnus-summary-mode-hook))

(defun gnus-mouse-pick-article (e)
  (interactive "e")
  (mouse-set-point e)
  (gnus-summary-next-page nil t))

(defun gnus-summary-setup-buffer (group)
  "Initialize summary buffer."
  (let ((buffer (concat "*Summary " group "*")))
    ;; Fix by Sudish Joseph <joseph@cis.ohio-state.edu>
    (setq gnus-summary-buffer (set-buffer (get-buffer-create buffer)))
    (gnus-add-current-to-buffer-list)
    (gnus-summary-mode)))

(defun gnus-set-global-variables ()
  ;; Set the global equivalents of the summary buffer-local variables
  ;; to the latest values they had. These reflect the summary buffer
  ;; that was in action when the last article was fetched.
  (if (eq major-mode 'gnus-summary-mode) 
      (progn
	(setq gnus-summary-buffer (current-buffer))
	(let ((name gnus-newsgroup-name)
	      (marked gnus-newsgroup-marked)
	      (unread gnus-newsgroup-unreads)
	      (headers gnus-current-headers))
	  (save-excursion
	    (set-buffer gnus-group-buffer)
	    (setq gnus-newsgroup-name name)
	    (setq gnus-newsgroup-marked marked)
	    (setq gnus-newsgroup-unreads unread)
	    (setq gnus-current-headers headers))))))

(defun gnus-summary-insert-dummy-line (sformat subject number)
  (if (not sformat) 
      (setq sformat gnus-summary-dummy-line-format-spec))
  (let (b)
    (beginning-of-line)
    (setq b (point))
    (insert (eval sformat))
    (add-text-properties
     b (1+ b)
     (list 'gnus-subject (gnus-simplify-subject-re subject)
	   'gnus-number number
	   'gnus-mark ?Z
	   'gnus-thread 0))))

(defun gnus-summary-insert-line 
  (sformat header level current unread replied expirable subject-or-nil
	   &optional dummy score)
  (or sformat (setq sformat gnus-summary-line-format-spec))
  (let* ((indentation 
	  (make-string (* level gnus-thread-indent-level) ? ))
	 (lines (or (header-lines header) 0))
	 (score (or score gnus-summary-default-score 0))
	 (score-char (if (= score gnus-summary-default-score) ? 
		       (if (< score gnus-summary-default-score) 
			   gnus-score-below-mark gnus-score-over-mark)))
	 (replied (if replied gnus-replied-mark ? ))
	 (from (header-from header))
	 (name-address (gnus-extract-address-components from))
	 (address (car (cdr name-address)))
	 (name (or (car name-address) (car (cdr name-address))))
	 (number (header-number header))
	 (subject (header-subject header))
	 (buffer-read-only nil)
	 (opening-bracket (if dummy ?\< ?\[))
	 (closing-bracket (if dummy ?\> ?\]))
	 b)
    ;; Suggested by Brian Edmonds <bedmonds@prodigy.bc.ca>.
    (if (not (numberp lines)) (setq lines 0))
    (beginning-of-line)
    (setq b (point))
    (insert-before-markers (eval sformat))
    (add-text-properties
     b (1+ b)
     (list 'gnus-subject (gnus-simplify-subject-re subject)
	   'gnus-number number
	   'gnus-mark (or unread gnus-unread-mark ? )
	   'gnus-thread level))))

(defun gnus-summary-update-line ()
  ;; Update summary line after change.
  (or (not gnus-summary-default-score)
      gnus-summary-inhibit-highlight
      (save-excursion
	(beginning-of-line 1)
	(let ((score (gnus-summary-article-score))
	      (default gnus-summary-default-score)
	      (below gnus-summary-mark-below))
	  (save-excursion
	    (if (< score below)
		(if (eq (following-char) gnus-unread-mark)
		    (gnus-summary-mark-article nil gnus-low-score-mark))
	      (if (eq (following-char) gnus-low-score-mark)
		  (gnus-summary-mark-article nil gnus-unread-mark))))
	  (if  gnus-visual
	      (run-hooks 'gnus-visual-summary-update-hook))))))

(defun gnus-summary-update-lines ()
  ;; Rehighlight summary buffer according to `gnus-summary-highlight'.
  (if (and gnus-visual gnus-visual-summary-update-hook)
      (save-excursion
	(set-buffer gnus-summary-buffer)
	(goto-char (point-min))
	(while (not (eobp))
	  (gnus-summary-update-line)
	  (forward-line 1)))))

(defun gnus-summary-read-group (group &optional show-all no-article kill-buffer)
  "Start reading news in newsgroup GROUP.
If SHOW-ALL is non-nil, already read articles are also listed.
If NO-ARTICLE is non-nil, no article is selected initially."
  (message "Retrieving newsgroup: %s..." group)
  (gnus-summary-setup-buffer group)
  (if (gnus-select-newsgroup group show-all)
      (progn
	;; You can change the subjects in this hook.
	(run-hooks 'gnus-select-group-hook)
	;; Do Score Processing.
	(gnus-score-headers)
	;; Update the format specifiers.
	(gnus-update-format-specifications)
	(gnus-summary-prepare)
	(if (and (zerop (buffer-size))
		 gnus-newsgroup-dormant)
	    (gnus-summary-show-all-dormant))
	(gnus-set-global-variables)
	;; Function `gnus-apply-kill-file' must be called in this hook.
	(run-hooks 'gnus-apply-kill-hook)
	(if (zerop (buffer-size))
	    (progn
	      ;; This newsgroup is empty.
	      (gnus-summary-catchup-and-exit nil t) ;Without confirmations.
	      (message "No unread news"))
	  ;; Hide conversation thread subtrees.  We cannot do this in
	  ;; gnus-summary-prepare-hook since kill processing may not
	  ;; work with hidden articles.
	  (and gnus-show-threads
	       gnus-thread-hide-subtree
	       (gnus-summary-hide-all-threads))
	  ;; Show first unread article if requested.
	  (goto-char (point-min))
	  (if (and (not no-article)
		   gnus-auto-select-first
		   (gnus-summary-first-unread-article))
	      (gnus-configure-windows 'article)
	    (gnus-configure-windows 'summary))
	  (pop-to-buffer gnus-summary-buffer)
	  (gnus-set-mode-line 'summary)
	  (gnus-summary-position-cursor)
	  (if (and kill-buffer
		   (get-buffer kill-buffer)
		   (buffer-name (get-buffer kill-buffer)))
	      (kill-buffer kill-buffer))))
    ;; Cannot select newsgroup GROUP.
    (message "Couldn't select newsgroup")
    (and (eq major-mode 'gnus-summary-mode)
	 (kill-buffer (current-buffer)))
    (switch-to-buffer gnus-group-buffer)))

(defun gnus-summary-prepare ()
  "Prepare summary list of current newsgroup in summary buffer."
  (let ((buffer-read-only nil))
    (erase-buffer)
    (gnus-summary-prepare-threads 
     (if gnus-show-threads
	 (gnus-gather-threads (gnus-sort-threads (gnus-make-threads)))
       gnus-newsgroup-headers)
     0)
    (gnus-summary-remove-dormant-lines)
    ;; Erase header retrieval message.
    (message "")
    ;; Call hooks for modifying summary buffer.
    ;; Suggested by sven@tde.LTH.Se (Sven Mattisson).
    (goto-char (point-min))
    (run-hooks 'gnus-summary-prepare-hook)))

(defun gnus-summary-remove-dormant-lines ()
  (let ((int gnus-newsgroup-dormant)
	(buffer-read-only nil)
	beg cur-level)
    (while int
      (if (gnus-summary-goto-subject (car int))
	  (progn
	    (beginning-of-line)
	    (setq cur-level (gnus-summary-thread-level))
	    (setq beg (point))
	    (re-search-forward "[\n\r]")
	    (if (<= (gnus-summary-thread-level) cur-level)
		;; If the level of the next article is greater than the
		;; level of this article, then it has to be the child of this
		;; article, so we do not delete this article.
		(progn
		  (setq gnus-newsgroup-dormant-subjects
			(cons (cons (car int) (buffer-substring beg (point)))
			      gnus-newsgroup-dormant-subjects))
		  (delete-region beg (point))))))
      (setq int (cdr int)))))

(defun gnus-gather-threads (threads)
  "Gather threads that have lost their roots."
  (if (not gnus-summary-make-false-root)
      threads 
    (let ((hashtb (gnus-make-hashtable 1023))
	  (prev threads)
	  (result threads)
	  thread subject hthread unre-subject whole-subject)
      (while threads
	(setq subject (header-subject (car (car threads)))
	      whole-subject subject)
	(and gnus-summary-gather-subject-limit
	     (> (length subject) gnus-summary-gather-subject-limit)
	     (setq subject
		   (substring subject 0 gnus-summary-gather-subject-limit)))
	(if (setq hthread 
		  (gnus-gethash 
		   (setq unre-subject (gnus-simplify-subject-re subject))
		   hashtb))
	    (progn
	      (or (stringp (car (car hthread)))
		  (setcar hthread (list whole-subject (car hthread))))
	      (setcdr (car hthread) (nconc (cdr (car hthread)) 
					   (list (car threads))))
	      (setcdr prev (cdr threads))
	      (setq threads prev))
	  (gnus-sethash unre-subject threads hashtb))
	(setq prev threads)
	(setq threads (cdr threads)))
      result)))

(defun gnus-make-threads ()
  ;; This function takes the dependencies already made by 
  ;; `gnus-get-newsgroup-headers' and builds the trees. First we go
  ;; through the dependecies in the hash table and finds all the
  ;; roots. Roots do not refer back to any valid articles.
  (let (roots)
    (and gnus-fetch-old-headers (eq gnus-headers-retrieved-by 'nov)
	 (gnus-build-old-threads))
    (mapatoms
     (lambda (refs)
       (if (not (car (symbol-value refs)))
	   (setq roots (append (cdr (symbol-value refs)) roots))
	 ;; Ok, these refer back to valid articles, but if
	 ;; `gnus-thread-ignore-subject' is nil, we have to check that
	 ;; the root has the same subject as its children. The children
	 ;; that do not are made into roots and removed from the list
	 ;; of children. 
	 (or gnus-thread-ignore-subject
	     (let* ((prev (symbol-value refs))
		    (subject (gnus-simplify-subject-re 
			      (header-subject (car prev))))
		    (headers (cdr prev)))
	       (while headers
		 (if (not (string= subject
				   (gnus-simplify-subject-re 
				    (header-subject (car headers)))))
		     (progn
		       (setq roots (cons (car headers) roots))
		       (setcdr prev (cdr headers)))
		   (setq prev headers))
		 (setq headers (cdr headers)))))))
     gnus-newsgroup-dependencies)

    (mapcar (lambda (root) (gnus-trim-thread (gnus-make-sub-thread root)))
	    roots)))

(defun gnus-trim-thread (thread)
  (if (and (eq gnus-fetch-old-headers 'some)
	   (memq (header-number (car thread)) gnus-newsgroup-ancient)
	   (= (length thread) 2))
      (gnus-trim-thread (nth 1 thread))
    thread))

(defun gnus-make-sub-thread (root)
  ;; This function makes a sub-tree for a node in the tree.
  (let ((children (reverse (cdr (gnus-gethash (downcase (header-id root))
					      gnus-newsgroup-dependencies)))))
    (cons root (mapcar 'gnus-make-sub-thread children))))

(defun gnus-build-old-threads ()
  ;; Look at all the articles that refer back to old articles, and
  ;; fetch the headers for the articles that aren't there. This will
  ;; build complete threads - if the roots haven't been expired by the
  ;; server, that is.
  (let (id heads)
    (mapatoms
     (lambda (refs)
       (if (not (car (symbol-value refs)))
	   (progn
	     (setq heads (cdr (symbol-value refs)))
	     (while heads
	       (if (not (memq (header-number (car heads))
			      gnus-newsgroup-dormant))
		   (progn
		     (setq id (symbol-name refs))
		     (while (and (setq id (gnus-build-get-header id))
				 (not (car (gnus-gethash 
					    id gnus-newsgroup-dependencies)))))
		     (setq heads nil))
		 (setq heads (cdr heads)))))))
     gnus-newsgroup-dependencies)))

(defun gnus-build-get-header (id)
  ;; Look through the buffer of NOV lines and find the header to
  ;; ID. Enter this line into the dependencies hash table, and return
  ;; the id of the parent article (if any).
  (let ((deps gnus-newsgroup-dependencies)
	found header)
    (prog1
	(save-excursion
	  (set-buffer nntp-server-buffer)
	  (goto-char (point-min))
	  (while (and (not found) (search-forward id nil t))
	    (beginning-of-line)
	    (setq found (looking-at (format "^[^\t]*\t[^\t]*\t[^\t]*\t[^\t]*\t%s"
					    (regexp-quote id))))
	    (or found (beginning-of-line 2)))
	  (if found
	      (let (ref)
		(beginning-of-line)
		(and
		 (setq header (gnus-nov-parse-line 
			       (read (current-buffer)) deps))
		 (setq ref (header-references header))
		 (string-match "\\(<[^>]+>\\) *$" ref)
		 (substring ref (match-beginning 1) (match-end 1))))))
      (and header
	   (setq gnus-newsgroup-headers (cons header gnus-newsgroup-headers)
		 gnus-newsgroup-ancient (cons (header-number header)
					      gnus-newsgroup-ancient))))))

(defun gnus-sort-threads (threads)
  ;; Sort threads as specified in `gnus-thread-sort-functions'.
  (let ((fun gnus-thread-sort-functions))
    (while fun
      (setq threads (sort threads (car fun))
	    fun (cdr fun))))
  threads)

(defun gnus-thread-header (thread)
  ;; Return header of first article in THREAD.
  (if (consp thread)
      (if (stringp (car thread))
	  (car (car (cdr thread)))
	(car thread))
    thread))

(defun gnus-thread-sort-by-number (h1 h2)
  "Sort threads by root article number."
  (let ((h1 (gnus-thread-header h1))
	(h2 (gnus-thread-header h2)))
    (< (header-number h1) (header-number h2))))

(defun gnus-thread-sort-by-author (h1 h2)
  "Sort threads by root author."
  (let ((h1 (gnus-thread-header h1))
	(h2 (gnus-thread-header h2)))
    (string-lessp
     (let ((extract (gnus-extract-address-components (header-from h1))))
       (or (car extract) (cdr extract)))
     (let ((extract (gnus-extract-address-components (header-from h2))))
       (or (car extract) (cdr extract))))))

(defun gnus-thread-sort-by-subject (h1 h2)
  "Sort threads by root subject."
  (let ((h1 (gnus-thread-header h1))
	(h2 (gnus-thread-header h2)))
    (string-lessp
     (downcase (gnus-simplify-subject (header-subject h1)))
     (downcase (gnus-simplify-subject (header-subject h2))))))

(defun gnus-thread-sort-by-date (h1 h2)
  "Sort threads by root article date."
  (let ((h1 (gnus-thread-header h1))
	(h2 (gnus-thread-header h2)))
    (string-lessp
     (gnus-sortable-date (header-date h1))
     (gnus-sortable-date (header-date h2)))))

(defun gnus-thread-sort-by-score (h1 h2)
  "Sort threads by root article score.
Unscored articles will be counted as havin a score of zero."
  (let ((h1 (gnus-thread-header h1))
	(h2 (gnus-thread-header h2)))
    (let ((s1 (assq (header-number h1) gnus-newsgroup-scored))
	  (s2 (assq (header-number h2) gnus-newsgroup-scored)))
      (> (or (cdr s1) gnus-summary-default-score 0)
	 (or (cdr s2) gnus-summary-default-score 0)))))

(defun gnus-thread-sort-by-total-score (h1 h2)
  "Sort threads by the sum of all scores in the thread.
Unscored articles will be counted as havin a score of zero."
  (> (gnus-thread-total-score h1) (gnus-thread-total-score h2)))

(defun gnus-thread-total-score (thread)
  ;;  This function find the total score of  THREAD.
  (if (consp thread)
      (if (stringp (car thread))
	  (apply gnus-thread-score-function 0
		 (mapcar 'gnus-thread-total-score-1 (cdr thread)))
	(gnus-thread-total-score-1 thread))
    (gnus-thread-total-score-1 (list thread))))

(defun gnus-thread-total-score-1 (root)
  ;; This function find the total score of the thread below ROOT.
  (setq root (car root))
  (apply gnus-thread-score-function
	 (or (cdr (assq (header-number root) gnus-newsgroup-scored))
	     gnus-summary-default-score 0)
	 (mapcar 'gnus-thread-total-score
		 (cdr (gnus-gethash (downcase (header-id root))
				    gnus-newsgroup-dependencies)))))

;; Added by Per Abrahamsen <amanda@iesd.auc.dk>.
(defvar gnus-tmp-prev-subject "")
(defvar gnus-tmp-prev-dormant nil)

;; Basic ideas by Paul Dworkin <paul@media-lab.media.mit.edu>.
(defun gnus-summary-prepare-threads 
  (threads level &optional not-child no-subject)
  "Prepare summary buffer from THREADS and indentation LEVEL.  
THREADS is either a list of `(PARENT [(CHILD1 [(GRANDCHILD ...]...) ...])'  
or a straight list of headers."
  (let (thread header number subject clevel)
    (while threads
      (setq thread (car threads)
	    threads (cdr threads))
      ;; If `thread' is a cons, hierarchical threads are used.  If not,
      ;; `thread' is the header.
      (if (consp thread)
	  (setq header (car thread))
	(setq header thread))
      (if (stringp header)
	  ;; The header is a dummy root.
	  (progn
	    (cond ((eq gnus-summary-make-false-root 'adopt)
		   ;; We let the first article adopt the rest.
		   (gnus-summary-prepare-threads (list (car (cdr thread))) 0)
		   (setq thread (cdr (cdr thread)))
		   (while thread
		     (gnus-summary-prepare-threads (list (car thread)) 1 t)
		     (setq thread (cdr thread))))
		  ((eq gnus-summary-make-false-root 'dummy)
		   ;; We output a dummy root.
		   (gnus-summary-insert-dummy-line 
		    nil header (header-number (car (car (cdr thread)))))
		   (setq clevel 1))
		  ((eq gnus-summary-make-false-root 'empty)
		   ;; We print the articles with empty subject fields. 
		   (gnus-summary-prepare-threads (list (car (cdr thread))) 0)
		   (setq thread (cdr (cdr thread)))
		   (while thread
		     (gnus-summary-prepare-threads (list (car thread)) 0 nil
						   (not gnus-tmp-prev-dormant))
		     (setq thread (cdr thread))))
		  (t
		   ;; We do not make a root for the gathered
		   ;; sub-threads at all.  
		   (setq clevel 0)))
	    ;; Print the sub-threads.
	    (and (consp thread) (cdr thread)
		 (gnus-summary-prepare-threads (cdr thread) clevel)))
	;; The header is a real article.
	(setq number (header-number header)
	      subject (header-subject header)
	      gnus-tmp-prev-dormant (memq number gnus-newsgroup-dormant))
	(gnus-summary-insert-line
	 nil header level nil 
	 (cond ((memq number gnus-newsgroup-marked) gnus-ticked-mark)
	       ((memq number gnus-newsgroup-dormant) gnus-dormant-mark)
	       ((memq number gnus-newsgroup-unreads) gnus-unread-mark)
	       ((memq number gnus-newsgroup-expirable) gnus-expirable-mark)
	       (t gnus-ancient-mark))
	 (memq number gnus-newsgroup-replied)
	 (memq number gnus-newsgroup-expirable)
	 (if no-subject gnus-summary-same-subject
	   (if (or (zerop level)
		   (and gnus-thread-ignore-subject
			(not (string= 
			      (gnus-simplify-subject-re gnus-tmp-prev-subject)
			      (gnus-simplify-subject-re subject)))))
	       subject
	     gnus-summary-same-subject))
	 not-child
	 (cdr (assq number gnus-newsgroup-scored)))
 	(setq gnus-tmp-prev-subject subject)
	;; Recursively print subthreads.
	(and (consp thread) (cdr thread)
	     (gnus-summary-prepare-threads (cdr thread) (1+ level)))))))

(defun gnus-select-newsgroup (group &optional read-all)
  "Select newsgroup GROUP.
If READ-ALL is non-nil, all articles in the group are selected."
  (let* ((entry (gnus-gethash group gnus-newsrc-hashtb))
	 (info (nth 2 entry))
	 articles header-marks)
    (and (eq (car entry) t)
	 (or (gnus-activate-newsgroup (car info))
	     (progn
	       (kill-buffer (current-buffer))
	       (error "Couldn't request newsgroup %s" group))))
    (setq gnus-current-select-method (or (nth 4 info) gnus-select-method))
    (gnus-check-news-server (nth 4 info))
    (or (gnus-request-group group t)
	(progn
	  (kill-buffer (current-buffer))
	  (error "Couldn't request newsgroup %s" group)))

    ;; Initialize the buffer that holds lines that have been removed
    ;; from the summary buffer.
    (save-excursion
      (set-buffer
       (setq gnus-newsgroup-expunged-buffer 
	     (get-buffer-create (format " *gnus expunge %s*" group))))
      (buffer-disable-undo (current-buffer))
      (erase-buffer)
      (gnus-add-current-to-buffer-list))
    
    (setq gnus-newsgroup-name group)
    (setq gnus-newsgroup-unselected nil)
    (setq gnus-newsgroup-unreads (gnus-list-of-unread-articles group))

    (and info
	 (let (marked)
	   (gnus-adjust-marked-articles info)
	   (setq gnus-newsgroup-marked 
		 (cdr (assq 'tick (setq marked (nth 3 info)))))
	   (setq gnus-newsgroup-replied (cdr (assq 'reply marked)))
	   (setq gnus-newsgroup-expirable (cdr (assq 'expire marked)))
	   (setq gnus-newsgroup-killed (cdr (assq 'killed marked)))
	   (setq gnus-newsgroup-bookmarks (cdr (assq 'bookmark marked)))
	   (setq gnus-newsgroup-dormant (cdr (assq 'dormant marked)))
	   (setq gnus-newsgroup-scored (cdr (assq 'score marked)))
	   (setq gnus-newsgroup-processable nil)))

    (if (not (setq articles (gnus-articles-to-read group read-all)))
	nil
      ;; Init the dependencies hash table.
      (setq gnus-newsgroup-dependencies 
	    (gnus-make-hashtable (length gnus-newsgroup-unreads)))
      ;; Retrieve the headers and read them in.
      (setq gnus-newsgroup-headers 
	    (if (eq 'nov (setq gnus-headers-retrieved-by
			       (gnus-retrieve-headers 
				(if gnus-fetch-old-headers 
				    (cons 1 articles) articles) 
				gnus-newsgroup-name)))
		(progn
		  (gnus-get-newsgroup-headers-xover articles))
	      (gnus-get-newsgroup-headers)))
      ;; If we were to fetch old headers, but the backend didn't
      ;; support XOVER, then it is possible we fetched one article
      ;; that we shouldn't have. If that's the case, we pop it off the
      ;; list of headers.
      (and (not (eq gnus-headers-retrieved-by 'nov))
	   gnus-fetch-old-headers
	   gnus-newsgroup-headers
	   (/= (header-number (car gnus-newsgroup-headers)) (car articles))
	   (setq gnus-newsgroup-headers (cdr gnus-newsgroup-headers)))
      ;; Remove cancelled articles from the list of unread articles.
      (setq gnus-newsgroup-unreads
	    (gnus-intersection 
	     gnus-newsgroup-unreads
	     (mapcar (lambda (headers) (header-number headers))
		     gnus-newsgroup-headers)))
      ;; Check whether auto-expire is to be done in this group.
      (setq gnus-newsgroup-auto-expire
	    (and (stringp gnus-auto-expirable-newsgroups)
		 (string-match gnus-auto-expirable-newsgroups 
			       (gnus-group-real-name group))))
      ;; First and last article in this newsgroup.
      (and gnus-newsgroup-headers
	   (setq gnus-newsgroup-begin 
		 (header-number (car gnus-newsgroup-headers)))
	   (setq gnus-newsgroup-end
		 (header-number (gnus-last-element gnus-newsgroup-headers))))
      (setq gnus-reffed-article-number -1)
      ;; GROUP is successfully selected.
      (or gnus-newsgroup-headers t))))

(defun gnus-articles-to-read (group read-all)
  ;; Find out what articles the user wants to read.
  (let* ((articles
	  ;; Select all articles if `read-all' is non-nil, or if all the
	  ;; unread articles are dormant articles.
	  (if (or read-all
		  (= (length gnus-newsgroup-unreads) 
		     (length gnus-newsgroup-scored)))
	      (gnus-uncompress-sequence 
	       (gnus-gethash group gnus-active-hashtb))
	    gnus-newsgroup-unreads))
	 (scored-list (gnus-killed-articles gnus-newsgroup-killed articles))
	 (scored (length scored-list))
	 (number (length articles))
	 (marked (+ (length gnus-newsgroup-marked)
		    (length gnus-newsgroup-dormant)))
	 (select
	  (condition-case ()
	      (cond ((and (or (<= scored marked)
			      (= scored number))
			  (numberp gnus-large-newsgroup)
			  (> number gnus-large-newsgroup))
		     (let ((input
			    (read-string
			     (format
			      "How many articles from %s (default %d): "
			      gnus-newsgroup-name number))))
		       (if (string-equal input "")
			   number input)))
		    ((and (> scored marked) (< scored number))
		     (let ((input
			    (read-string
			     (format 
			      "%s %s (%d scored, %d total, %d default): "
			      "How many articles from"
			      group scored number scored))))
		       (if (string-equal input "")
			   scored input)))
		    (t number))
	    (quit 0))))
    (setq select (if (numberp select) select (string-to-number select)))
    (if (zerop select)
	()
      (if (and (not (zerop scored)) (<= (abs select) scored))
	  (progn
	    (setq articles (sort scored-list '<))
	    (setq number (length articles)))
	(setq articles (copy-sequence articles)))

      (if (< (abs select) number)
	  (if (< select 0) 
	      ;; Select the N oldest articles.
	      (setcdr (nthcdr (1- (abs select)) articles) nil)
	    ;; Select the N most recent articles.
	    (setq articles (nthcdr (- number select) articles))))
      (setq gnus-newsgroup-unselected
	    (gnus-set-difference gnus-newsgroup-unreads articles))
      articles)))

(defun gnus-killed-articles (killed articles)
  (let (out)
    (while articles
      (if (inline (gnus-member-of-range (car articles) killed))
	  (setq out (cons (car articles) out)))
      (setq articles (cdr articles)))
    out))

(defun gnus-adjust-marked-articles (info)
  "Remove all marked articles that are no longer legal."
  (let ((marked-lists (nth 3 info))
	(active (gnus-gethash (car info) gnus-active-hashtb))
	marked m prev)
    ;; There are four types of marked articles - ticked, replied,
    ;; expirable and dormant.  
    (while marked-lists
      (setq m (cdr (setq prev (car marked-lists))))
      (cond ((or (eq 'tick (car prev)) (eq 'dormant (car prev)))
	     ;; Make sure that all ticked articles are a subset of the
	     ;; unread/unselected articles.
	     (while m
	       (if (or (memq (car m) gnus-newsgroup-unreads)
		       (memq (car m) gnus-newsgroup-unselected))
		   (setq prev m)
		 (setcdr prev (cdr m)))
	       (setq m (cdr m))))
	    ((eq 'score (car prev))
	     ;; Scored articles should be a subset of
	     ;; unread/unselected articles. 
	     (while m
	       (if (or (memq (car (car m)) gnus-newsgroup-unreads)
		       (memq (car (car m)) gnus-newsgroup-unreads))
		   (setq prev m)
		 (setcdr prev (cdr m)))
	       (setq m (cdr m))))
	    ((eq 'bookmark (car prev))
	     ;; Bookmarks should be a subset of active articles.
	     (while m
	       (if (< (car (car m)) (car active))
		   (setcdr prev (cdr m))
		 (setq prev m))
	       (setq m (cdr m))))
	    ((eq 'killed (car prev))
	     ;; Articles that have been through the kill process are
	     ;; to be a subset of active articles.
	     (while (and m (< (cdr (car m)) (car active)))
	       (setcdr prev (cdr m))
	       (setq m (cdr m)))
	     (if (and m (< (car (car m)) (car active))) 
		 (setcar (car m) (car active))))
	    ((or (eq 'reply (car marked)) (eq 'expire (car marked)))
	     ;; The replied and expirable articles have to be articles
	     ;; that are active. 
	     (while m
	       (if (< (car m) (car active))
		   (setcdr prev (cdr m))
		 (setq prev m))
	       (setq m (cdr m)))))
      (setq marked-lists (cdr marked-lists)))
    ;; Remove all lists that are empty.
    (setq marked-lists (nth 3 info))
    (if marked-lists
	(progn
	  (while (= 1 (length (car marked-lists)))
	    (setq marked-lists (cdr marked-lists)))
	  (setq m (cdr (setq prev marked-lists)))
	  (while m
	    (if (= 1 (length (car m)))
		(setcdr prev (cdr m))
	      (setq prev m))
	    (setq m (cdr m)))
	  (setcar (nthcdr 3 info) marked-lists)))
    ;; Finally, if there are no marked lists at all left, and if there
    ;; are no elements after the lists in the info list, we just chop
    ;; the info list off before the marked lists.
    (if (and (null marked-lists) (not (nthcdr 4 info)))
	(setcdr (nthcdr 2 info) nil)))
  info)

(defun gnus-set-marked-articles 
  (info ticked replied expirable killed dormant bookmark score) 
  "Enter the various lists of marked articles into the newsgroup info list."
  (let (newmarked)
    (and ticked (setq newmarked (cons (cons 'tick ticked) nil)))
    (and replied (setq newmarked (cons (cons 'reply replied) newmarked)))
    (and expirable (setq newmarked (cons (cons 'expire expirable) 
					 newmarked)))
    (and killed (setq newmarked (cons (cons 'killed killed) newmarked)))
    (and dormant (setq newmarked (cons (cons 'dormant dormant) newmarked)))
    (and bookmark (setq newmarked (cons (cons 'bookmark bookmark) 
					newmarked)))
    (and score (setq newmarked (cons (cons 'score score) newmarked)))
    (if (nthcdr 3 info)
	(if newmarked
	    (setcar (nthcdr 3 info) newmarked)
	  (if (not (nthcdr 4 info))
	      (setcdr (nthcdr 2 info) nil)
	    (setcar (nthcdr 3 info) nil)))
      (if newmarked
	  (setcdr (nthcdr 2 info) (cons newmarked nil))))))

(defun gnus-add-marked-articles (group type articles &optional info force)
  ;; Add ARTICLES of TYPE to the info of GROUP.
  ;; If INFO is non-nil, use that info. If FORCE is non-nil, don't
  ;; add, but replace this marked articles of TYPE with ARTICLES.
  (let ((info (or info (nth 2 (gnus-gethash group gnus-newsrc-hashtb))))
	marked m)
    (or (not info)
	(and (not (setq marked (nthcdr 3 info)))
	     (setcdr (nthcdr 2 info) (list (list (cons type articles)))))
	(and (not (setq m (assq type (car marked))))
	     (setcar marked (cons (cons type articles) (car marked))))
	(if force
	    (setcdr m articles)
	  (nconc m articles)))))
	 
(defun gnus-set-mode-line (where)
  "This function sets the mode line of the article or summary buffers.
If WHERE is `summary', the summary mode line format will be used."
  (if (memq where gnus-updated-mode-lines)
      (let (mode-string)
	(save-excursion
	  (set-buffer gnus-summary-buffer)
	  (let* ((mformat (if (eq where 'article) 
			      gnus-article-mode-line-format-spec
			    gnus-summary-mode-line-format-spec))
		 (group-name gnus-newsgroup-name)
		 (article-number (or gnus-current-article 0))
		 (unread (- (length gnus-newsgroup-unreads)
			    (length gnus-newsgroup-dormant)))
		 (unselected (length gnus-newsgroup-unselected))
		 (unread-and-unselected
		  (cond ((and (zerop unread) (zerop unselected)) "")
			((zerop unselected) (format "{%d more}" unread))
			(t (format "{%d(+%d) more}" unread unselected))))
		 (subject
		  (if gnus-current-headers
		      (header-subject gnus-current-headers) ""))
		 (max-len (if (eq where 'summary) 59 59)))
	    (setq mode-string (eval mformat))
	    (if (> (length mode-string) max-len) 
		(setq mode-string 
		      (concat (substring mode-string 0 (- max-len 3)) "...")))
	    (setq mode-string (format (format "%%-%ds" max-len 5)
				      mode-string))))
	(setq mode-line-buffer-identification mode-string)
	(set-buffer-modified-p t))))

(defun gnus-create-xref-hashtb (from-newsgroup headers unreads)
  "Go through the HEADERS list and add all Xrefs to a hash table.
The resulting hash table is returned, or nil if no Xrefs were found."
  (let* ((from-method (gnus-find-method-for-group from-newsgroup))
	 (prefix (if (and 
		      (gnus-group-foreign-p from-newsgroup)
		      (not (memq 'virtual 
				 (assoc (symbol-name (car from-method))
					gnus-valid-select-methods))))
		     (gnus-group-real-prefix from-newsgroup)))
	 (xref-hashtb (make-vector 63 0))
	 start group entry number xrefs header)
    (while headers
      (setq header (car headers))
      (if (and (setq xrefs (header-xref header))
	       (not (memq (header-number header) unreads)))
	  (progn
	    (setq start 0)
	    (while (string-match "\\([^ ]+\\):\\([0-9]+\\)" xrefs start)
	      (setq start (match-end 0))
	      (setq group (concat prefix (substring xrefs (match-beginning 1) 
						    (match-end 1))))
	      (setq number 
		    (string-to-int (substring xrefs (match-beginning 2) 
					      (match-end 2))))
	      (if (setq entry (gnus-gethash group xref-hashtb))
		  (setcdr entry (cons number (cdr entry)))
		(gnus-sethash group (cons number nil) xref-hashtb)))))
      (setq headers (cdr headers)))
    (if start xref-hashtb nil)))

(defun gnus-mark-xrefs-as-read (from-newsgroup headers unreads expirable)
  "Look through all the headers and mark the Xrefs as read."
  (let (name entry read info xref-hashtb idlist active num range exps method)
    (set-buffer gnus-group-buffer)
    (if (setq xref-hashtb 
	      (gnus-create-xref-hashtb from-newsgroup headers unreads))
	(mapatoms 
	 (lambda (group)
	   (if (string= from-newsgroup (setq name (symbol-name group)))
	       ()
	     (setq idlist (symbol-value group))
	     ;; Dead groups are not updated.
	     (if (and (setq entry (gnus-gethash name gnus-newsrc-hashtb))
		      ;; Only do the xrefs if the group has the same
		      ;; select method as the group we have just read.
		      (or (and (not (nth 4 (setq info (nth 2 entry))))
			       (eq (gnus-find-method-for-group from-newsgroup)
				   gnus-select-method))
			  (memq 'virtual 
				(assoc (symbol-name 
					(car (gnus-find-method-for-group 
					      from-newsgroup)))
				       gnus-valid-select-methods))
			  (equal (nth 4 info) 
				 (setq method (gnus-find-method-for-group 
					       from-newsgroup)))
			  (and (equal (car (nth 4 info)) (car method))
			       (equal (nth 1 (nth 4 info)) (nth 1 method))))
		      gnus-use-cross-reference
		      (or (not (eq gnus-use-cross-reference t))
			  (<= (nth 1 info) 5)))
		 (progn
		   (setq num 0)
		   ;; Set the new list of read articles in this group.
		   (setq active (gnus-gethash name gnus-active-hashtb))
		   ;; First peel off all illegal article numbers.
		   (if active
		       (let ((ids idlist)
			     (ticked (cdr (memq 'tick (nth 3 info))))
			     (dormant (cdr (memq 'dormant (nth 3 info))))
			     id)
			 (setq exps nil)
			 (while ids
			   (setq id (car ids))
			   (if (or (> id (cdr active))
				   (< id (car active))
				   (memq id ticked)
				   (memq id dormant))
			       (setq idlist (delq id idlist)))
			   (and (memq id expirable)
				(setq exps (cons id exps)))
			   (setq ids (cdr ids)))))
		   ;; Update expirable articles.
		   (gnus-add-marked-articles nil 'expirable exps info)
		   (setcar (nthcdr 2 info)
			   (setq range
				 (gnus-add-to-range 
				  (nth 2 info) 
				  (setq idlist (sort idlist '<)))))
		   ;; Then we have to re-compute how many unread
		   ;; articles there are in this group.
		   (if active
		       (progn
			 (if (atom (car range))
			     (if (not range)
				 (setq num (- (1+ (cdr active)) (car active)))
			       (setq num (- (cdr active) (- (1+ (cdr range)) 
							    (car range)))))
			   (while range
			     (setq num (+ num (- (1+ (cdr (car range))) 
						 (car (car range)))))
			     (setq range (cdr range)))
			   (setq num (- (cdr active) num)))
			 ;; Update the number of unread articles.
			 (setcar 
			  entry 
			  (max 0 (- num (length 
					 (cdr (memq 'tick (nth 3 info))) )
				    (length (cdr (memq 'dormant 
						       (nth 3 info)))))))
			 ;; Update the group buffer.
			 (gnus-group-update-group name t)))))))
	 xref-hashtb))))

(defsubst gnus-header-value ()
  (buffer-substring (match-end 0) (save-excursion (end-of-line) (point))))

(defun gnus-get-newsgroup-headers ()
  (setq gnus-article-internal-prepare-hook nil)
  (let ((cur nntp-server-buffer)
	(dependencies gnus-newsgroup-dependencies)
	(none-id 0)
	headers char article id dep end)
    (save-excursion
      (set-buffer nntp-server-buffer)
      (goto-char 1)
      ;; Search to the beginning of the next header. Error messages
      ;; do not begin with 2 or 3.
      (while (re-search-forward "^[23][0-9]+ " nil t)
	(let ((header (make-vector 9 nil))
	      (c (following-char))
	      (case-fold-search t)
	      (p (point))
	      from subject in-reply-to references ref)
	  (setq id nil
		ref nil
		references nil
		subject nil
		from nil)
	  (header-set-number header (setq article (read cur)))
	  ;; This implementation of this function, with nine
	  ;; search-forwards instead of the one re-search-forward and
	  ;; a case (which basically was the old function) is actually
	  ;; about twice as fast, even though it looks messier. You
	  ;; can't have everything, I guess. Speed and elegance
	  ;; doesn't always come hand in hand.
	  (save-restriction
	    (narrow-to-region (point) (save-excursion 
					(search-forward "\n.\n" nil t)))
	    (if (search-forward "\nfrom: " nil t)
		(header-set-from header (gnus-header-value))
	      (header-set-from header "(nobody)"))
	    (goto-char p)
	    (if (search-forward "\nsubject: " nil t)
		(header-set-subject header (gnus-header-value))
	      (header-set-subject header "(none)"))
	    (goto-char p)
	    (and (search-forward "\nxref: " nil t)
		 (header-set-xref header (gnus-header-value)))
	    (goto-char p)
	    (and (search-forward "\nlines: " nil t)
		 (header-set-lines header (read cur)))
	    (goto-char p)
	    (and (search-forward "\ndate: " nil t)
		 (header-set-date header (gnus-header-value)))
	    (goto-char p)
	    (if (search-forward "\nmessage-id: " nil t)
		(header-set-id header (setq id (gnus-header-value)))
	      ;; If there was no message-id, we just fake one to make
	      ;; subsequent routines simpler.
	      (header-set-id 
	       header 
	       (setq id (concat "none+" (int-to-string 
					 (setq none-id (1+ none-id)))))))
	    (goto-char p)
	    (if (search-forward "\nreferences: " nil t)
		(progn
		  (header-set-references header (gnus-header-value))
		  (setq end (match-end 0))
		  (save-excursion
		    (setq ref 
			  (downcase
			   (buffer-substring
			    (progn 
			      (end-of-line)
			      (search-backward ">" end t)
			      (1+ (point)))
			    (progn
			      (search-backward "<" end t)
			      (point)))))))
	      ;; Get the references from the in-reply-to header if there
	      ;; ware no references and the in-reply-to header looks
	      ;; promising. 
	      (if (and (search-forward "\nin-reply-to: " nil t)
		       (setq in-reply-to (gnus-header-value))
		       (string-match "<[^>]+>" in-reply-to))
		  (progn
		    (header-set-references 
		     header 
		     (setq ref (substring in-reply-to (match-beginning 0)
					  (match-end 0))))
		    (setq ref (downcase ref)))
		(setq ref "none")))
	    ;; We do some threading while we read the headers. The
	    ;; message-id and the last reference are both entered into
	    ;; the same hash table. Some tippy-toeing around has to be
	    ;; done in case an article has arrived before the article
	    ;; which it refers to.
	    (if (boundp (setq dep (intern (downcase id) dependencies)))
		(if (car (symbol-value dep))
		    ;; An article with this Message-ID has already
		    ;; been seen, so we ignore this one, except we add
		    ;; any additional Xrefs (in case the two articles
		    ;; came from different servers.
		    (progn
		      (header-set-xref 
		       (car (symbol-value dep))
		       (concat (or (header-xref (car (symbol-value dep))) "")
			       (or (header-xref header) "")))
		      (setq header nil))
		  (setcar (symbol-value dep) header))
	      (set dep (list header)))
	    (if header
		(progn
		  (if (boundp (setq dep (intern ref dependencies)))
		      (setcdr (symbol-value dep) 
			      (cons header (cdr (symbol-value dep))))
		    (set dep (list nil header)))
		  (setq headers (cons header headers))))
	    (goto-char (point-max))))))
    (nreverse headers)))

;; The following macros and functions were written by Felix Lee
;; <flee@cse.psu.edu>. 

;; This is almost 4x faster than (string-to-int (buffer-substring ... ))
;; primarily because of garbage collection.  -jwz
(defmacro gnus-read-integer (&optional point move-p)
  (` ((, (if move-p 'progn 'save-excursion))
      (,@ (if point (list (list 'goto-char point))))
      (if (and (<= (following-char) ?9)
	       (>= (following-char) ?0))
	  (read (current-buffer))
	0))))

(defmacro gnus-nov-skip-field ()
  '(search-forward "\t" eol 'end))

(defmacro gnus-nov-field ()
  '(buffer-substring
    (point)
    (progn (gnus-nov-skip-field) (1- (point)))))

;; Goes through the xover lines and returns a list of vectors
(defun gnus-get-newsgroup-headers-xover (sequence)
  "Parse the news overview data in the server buffer, and return a
list of headers that match SEQUENCE (see `nntp-retrieve-headers')."
  ;; Get the Xref when the users reads the articles since most/some
  ;; NNTP servers do not include Xrefs when using XOVER.
  (setq gnus-article-internal-prepare-hook '(gnus-article-get-xrefs))
  (let ((cur nntp-server-buffer)
	(dependencies gnus-newsgroup-dependencies)
	(none 0)
	number headers header)
    (save-excursion
      (set-buffer nntp-server-buffer)
      (goto-char (point-min))
      (while (and sequence (not (eobp)))
	(setq number (read cur))
	(while (and sequence (< (car sequence) number))
	  (setq sequence (cdr sequence)))
	(and sequence 
	     (eq number (car sequence))
	     (progn
	       (setq sequence (cdr sequence))
	       (if (setq header 
			 (inline (gnus-nov-parse-line number dependencies)))
		   (setq headers (cons header headers)))))
	(forward-line 1))
      (setq headers (nreverse headers)))
    headers))

(defun gnus-nov-parse-line (number dependencies)
  "Point has to be after the number on the beginning of the line."
  (let ((none 0)
	header eol ref id dep)
    (save-excursion
      (end-of-line)
      (setq eol (point)))
    (forward-char)
    ;; overview: [num subject from date id refs chars lines misc]
    (setq header
	  (vector 
	   number			; number
	   (gnus-nov-field)		; subject
	   (gnus-nov-field)		; from
	   (gnus-nov-field)		; date
	   (setq id (gnus-nov-field))	; id
	   (progn
	     (save-excursion
	       (let ((beg (point)))
		 (search-forward "\t" eol)
		 (if (search-backward ">" beg t)
		     (setq ref 
			   (downcase 
			    (buffer-substring 
			     (1+ (point))
			     (progn
			       (search-backward "<" beg t)
			       (point)))))
		   (setq ref nil))))
	     (gnus-nov-field))		; refs
	   (read (current-buffer))      ; chars
	   (read (current-buffer))	; lines
	   (if (/= (following-char) ?\t)
	       nil
	     (forward-char 1)
	     (gnus-nov-field))		; misc
	   ))
    ;; We build the thread tree.
    (if (boundp 
	 (setq dep 
	       (intern 
		(downcase 
		 (or id (concat "none+"
				(int-to-string 
				 (setq none (1+ none))))))
		dependencies)))
	(if (car (symbol-value dep))
	    ;; An article with this Message-ID has already been seen,
	    ;; so we ignore this one, except we add any additional
	    ;; Xrefs (in case the two articles came from different
	    ;; servers.
	    (progn
	      (header-set-xref 
	       (car (symbol-value dep))
	       (concat (or (header-xref (car (symbol-value dep))) "")
		       (or (header-xref header) "")))
	      (setq header nil))
	  (setcar (symbol-value dep) header))
      (set dep (list header)))
    (if header
	(progn
	  (if (boundp (setq dep (intern (or ref "none") 
					dependencies)))
	      (setcdr (symbol-value dep) 
		      (cons header (cdr (symbol-value dep))))
	    (set dep (list nil header)))))
    header))

(defun gnus-article-get-xrefs ()
  "Fill in the Xref value in `gnus-current-headers', if necessary.
This is meant to be called in `gnus-article-internal-prepare-hook'."
  (or (not gnus-use-cross-reference)
      (header-xref gnus-current-headers)
      (let ((case-fold-search t)
	    xref)
	(save-restriction
	  (gnus-narrow-to-headers)
	  (goto-char (point-min))
	  (if (or (and (eq (downcase (following-char)) ?x)
		       (looking-at "Xref:"))
		  (search-forward "\nXref:" nil t))
	      (progn
		(goto-char (1+ (match-end 0)))
		(setq xref (buffer-substring (point) 
					     (progn (end-of-line) (point))))
		(save-excursion
		  (set-buffer gnus-summary-buffer)
		  (header-set-xref gnus-current-headers xref))))))))

(defalias 'gnus-find-header-by-number 'gnus-get-header-by-number)
(make-obsolete 'gnus-find-header-by-number 'gnus-get-header-by-number)

;; Return a header specified by a NUMBER.
(defun gnus-get-header-by-number (number)
  (save-excursion
    (set-buffer gnus-summary-buffer)
    (or gnus-newsgroup-headers-hashtb-by-number
	(gnus-make-headers-hashtable-by-number))
    (gnus-gethash (int-to-string number)
		  gnus-newsgroup-headers-hashtb-by-number)))

(defun gnus-make-headers-hashtable-by-number ()
  "Make hashtable for the variable gnus-newsgroup-headers by number."
  (save-excursion
    (set-buffer gnus-summary-buffer)
    (let ((headers gnus-newsgroup-headers)
	  header)
      (setq gnus-newsgroup-headers-hashtb-by-number
	    (gnus-make-hashtable (length headers)))
      (while headers
	(setq header (car headers))
	(gnus-sethash (int-to-string (header-number header))
		      header gnus-newsgroup-headers-hashtb-by-number)
	(setq headers (cdr headers))))))

(defun gnus-more-header-backward ()
  "Find new header backward."
  (let ((first (car (gnus-gethash gnus-newsgroup-name gnus-active-hashtb)))
	(artnum gnus-newsgroup-begin)
	(header nil))
    (while (and (not header)
		(> artnum first))
      (setq artnum (1- artnum))
      (setq header (gnus-read-header artnum)))
    header))

(defun gnus-more-header-forward ()
  "Find new header forward."
  (let ((last (cdr (gnus-gethash gnus-newsgroup-name gnus-active-hashtb)))
	(artnum gnus-newsgroup-end)
	(header nil))
    (while (and (not header)
		(< artnum last))
      (setq artnum (1+ artnum))
      (setq header (gnus-read-header artnum)))
    header))

(defun gnus-extend-newsgroup (header &optional backward)
  "Extend newsgroup selection with HEADER.
Optional argument BACKWARD means extend toward backward."
  (if header
      (let ((artnum (header-number header)))
	(setq gnus-newsgroup-headers
	      (if backward
		  (cons header gnus-newsgroup-headers)
		(nconc gnus-newsgroup-headers (list header))))
	(setq gnus-newsgroup-unselected
	      (delq artnum gnus-newsgroup-unselected))
	(setq gnus-newsgroup-begin (min gnus-newsgroup-begin artnum))
	(setq gnus-newsgroup-end (max gnus-newsgroup-end artnum)))))

(defun gnus-summary-work-articles (n)
  "Return a list of articles to be worked upon. The prefix argument,
the list of process marked articles, and the current article will be
taken into consideration."
  (let (articles)
    (if (and n (numberp n))
	(let ((backward (< n 0))
	      (n (abs n)))
	  (save-excursion
	    (while (and (> n 0)
			(setq articles (cons (gnus-summary-article-number) 
					     articles))
			(gnus-summary-search-forward nil nil backward))
	      (setq n (1- n))))
	  (sort articles (function <)))
      (or (reverse gnus-newsgroup-processable)
	  (list (gnus-summary-article-number))))))

(defun gnus-summary-search-group (&optional backward use-level)
  "Search for next unread newsgroup.
If optional argument BACKWARD is non-nil, search backward instead."
  (save-excursion
    (set-buffer gnus-group-buffer)
    (save-excursion
      ;; We don't want to alter current point of group mode buffer.
      (if (gnus-group-search-forward 
	   backward nil
	   (if use-level (gnus-group-group-level) nil))
	  (gnus-group-group-name)))))

(defun gnus-summary-best-group ()
  "Find the name of the best unread group."
  (save-excursion
    (set-buffer gnus-group-buffer)
    (save-excursion
      (gnus-group-best-unread-group))))

(defun gnus-summary-search-subject (&optional backward unread subject)
  "Search for article forward.
If BACKWARD is non-nil, search backward.
If UNREAD is non-nil, only unread articles are selected.
If SUBJECT is non-nil, the article which has the same subject will be
searched for." 
  (let ((func (if backward 'previous-single-property-change
		'next-single-property-change))
	(beg (point))
	(did t)
	pos)
    (beginning-of-line)
    (forward-char (if backward (if (bobp) 0 -1) (if (eobp) 0 1)))
    (while (and (setq pos (funcall func (point) 'gnus-number))
		(goto-char (if backward (1- pos) pos))
		(setq did
		      (not (and (or (not unread)
				    (eq (get-text-property (point) 'gnus-mark) 
					gnus-unread-mark))
				(or (not subject)
				    (equal (gnus-simplify-subject-re 
					    subject)
					   (gnus-simplify-subject-re
					    (get-text-property 
					     (point) 
					     'gnus-subject)))))))
		(if backward (if (bobp) nil (forward-char -1) t)
		  (if (eobp) nil (forward-char 1) t))))
    (if did
	(progn (goto-char beg) nil)
      (prog1
	  (get-text-property (point) 'gnus-number)
	(gnus-summary-position-cursor)))))

(defun gnus-summary-search-forward (&optional unread subject backward)
  "Search for article forward.
If UNREAD is non-nil, only unread articles are selected.
If SUBJECT is non-nil, the article which has the same subject will be
searched for. 
If BACKWARD is non-nil, the search will be performed backwards instead."
  (gnus-summary-search-subject backward unread subject))

(defun gnus-summary-search-backward (&optional unread subject)
  "Search for article backward.
If 1st optional argument UNREAD is non-nil, only unread article is selected.
If 2nd optional argument SUBJECT is non-nil, the article which has
the same subject will be searched for."
  (gnus-summary-search-forward unread subject t))

(defun gnus-summary-article-number (&optional number-or-nil)
  "The article number of the article on the current line.
If there isn's an article number here, then we return the current
article number."
  (let ((number (get-text-property (save-excursion (beginning-of-line) (point))
				   'gnus-number)))
    (if number-or-nil number (or number gnus-current-article))))

(defun gnus-summary-thread-level ()
  "The thread level of the article on the current line."
  (or (get-text-property (save-excursion (beginning-of-line) (point))
			 'gnus-thread)
      0))

(defun gnus-summary-pseudo-article ()
  "The thread level of the article on the current line."
  (get-text-property (save-excursion (beginning-of-line) (point)) 
		     'gnus-pseudo))

(defun gnus-summary-article-mark ()
  "The mark on the current line."
  (get-text-property (save-excursion (beginning-of-line) (point))
		     'gnus-mark))

(defun gnus-summary-subject-string ()
  "Return current subject string or nil if nothing."
  (get-text-property (save-excursion (beginning-of-line) (point))
		     'gnus-subject))

(defalias 'gnus-summary-score 'gnus-summary-article-score)
(make-obsolete 'gnus-summary-score 'gnus-summary-article-score)
(defun gnus-summary-article-score ()
  "Return current article score."
  (or (cdr (assq (gnus-summary-article-number) gnus-newsgroup-scored))
      gnus-summary-default-score))

(defun gnus-summary-recenter ()
  "Center point in summary window."
  ;; Suggested by earle@mahendo.JPL.NASA.GOV (Greg Earle).
  ;; Recenter only when requested. Suggested by popovich@park.cs.columbia.edu.
  (let ((half (/ (- (window-height) 2) 2)))
    (and 
     ;; It has to be wanted,
     gnus-auto-center-summary 
     ;; the article buffer must be displayed,
     (get-buffer-window gnus-article-buffer)
     ;; there must be lines left to scroll forward,
     (zerop (save-excursion (forward-line (1+ half))))
     ;; so we recenter.
     (recenter half))))

(defun gnus-summary-jump-to-group (newsgroup)
  "Move point to NEWSGROUP in group mode buffer."
  ;; Keep update point of group mode buffer if visible.
  (if (eq (current-buffer) (get-buffer gnus-group-buffer))
      (save-window-excursion
	;; Take care of tree window mode.
	(if (get-buffer-window gnus-group-buffer)
	    (pop-to-buffer gnus-group-buffer))
	(gnus-group-jump-to-group newsgroup))
    (save-excursion
      ;; Take care of tree window mode.
      (if (get-buffer-window gnus-group-buffer)
	  (pop-to-buffer gnus-group-buffer)
	(set-buffer gnus-group-buffer))
      (gnus-group-jump-to-group newsgroup))))

;; This function returns a list of article numbers based on the
;; difference between the ranges of read articles in this group and
;; the range of active articles.
(defun gnus-list-of-unread-articles (group)
  (let* ((read (nth 2 (nth 2 (gnus-gethash group gnus-newsrc-hashtb))))
	 (active (gnus-gethash group gnus-active-hashtb))
	 (last (cdr active))
	 unread first nlast unread)
    ;; If none are read, then all are unread. 
    (if (not read)
	  (setq first (car active))
      ;; If the range of read articles is a single range, then the
      ;; first unread article is the article after the last read
      ;; article. Sounds logical, doesn't it?
      (if (atom (car read))
	  (setq first (1+ (cdr read)))
	;; `read' is a list of ranges.
	(while read
	  (if first 
	      (while (< first nlast)
		(setq unread (cons first unread))
		(setq first (1+ first))))
	  (setq first (1+ (cdr (car read))))
	  (setq nlast (car (car (cdr read))))
	  (setq read (cdr read)))))
    ;; And add the last unread articles.
    (while (<= first last)
      (setq unread (cons first unread))
      (setq first (1+ first)))
    ;; Return the list of unread articles.
    (nreverse unread)))


;; Various summary commands

(defun gnus-summary-universal-argument ()
  "Perform any operation on all articles marked with the process mark."
  (interactive)
  (gnus-set-global-variables)
  (let ((articles (reverse gnus-newsgroup-processable))
	key func)
    (or articles (error "No articles marked"))
    (or (setq func (key-binding (read-key-sequence "C-c C-u")))
	(error "Undefined key"))
    (while articles
      (gnus-summary-goto-subject (car articles))
      (command-execute func)
      (gnus-summary-remove-process-mark (car articles))
      (setq articles (cdr articles)))))

(defun gnus-summary-toggle-truncation (arg)
  "Toggle truncation of summary lines.
With arg, turn line truncation on iff arg is positive."
  (interactive "P")
  (setq truncate-lines
	(if (null arg) (not truncate-lines)
	  (> (prefix-numeric-value arg) 0)))
  (redraw-display))

(defun gnus-summary-reselect-current-group (show-all)
  "Once exit and then reselect the current newsgroup.
Prefix argument SHOW-ALL means to select all articles."
  (interactive "P")
  (gnus-set-global-variables)
  (let ((current-subject (gnus-summary-article-number)))
    (gnus-summary-exit t)
    ;; We have to adjust the point of group mode buffer because the
    ;; current point was moved to the next unread newsgroup by
    ;; exiting.
    (gnus-summary-jump-to-group gnus-newsgroup-name)
    (gnus-group-read-group show-all t)
    (gnus-summary-goto-subject current-subject)
    ))

(defun gnus-summary-rescan-group (all)
  "Exit the newsgroup, ask for new articles, and select the newsgroup."
  (interactive "P")
  (gnus-set-global-variables)
  ;; Fix by Ilja Weis <kult@uni-paderborn.de>.
  (let ((group gnus-newsgroup-name))
    (gnus-summary-exit t)
    (gnus-summary-jump-to-group group)
    (save-excursion
      (set-buffer gnus-group-buffer)
      (gnus-group-get-new-news-this-group 1))
    (gnus-summary-jump-to-group group)
    (gnus-group-read-group all)))

(defun gnus-summary-exit (&optional temporary)
  "Exit reading current newsgroup, and then return to group selection mode.
gnus-exit-group-hook is called with no arguments if that value is non-nil."
  (interactive)
  (gnus-set-global-variables)
  (gnus-kill-save-kill-buffer)
  (let* ((group gnus-newsgroup-name)
	 (quit-buffer (cdr (assoc 'quit-buffer (gnus-find-method-for-group
						gnus-newsgroup-name))))
	 (mode major-mode)
	 (method (car (gnus-find-method-for-group group)))
	 (buf (current-buffer)))
    (if gnus-newsgroup-kill-headers
	(setq gnus-newsgroup-killed
	      (gnus-compress-sequence
	       (nconc
		(gnus-intersection
		 (gnus-uncompress-sequence gnus-newsgroup-killed)
		 (setq gnus-newsgroup-unselected
		       (sort gnus-newsgroup-unselected '<)))
		(setq gnus-newsgroup-unreads
		      (sort gnus-newsgroup-unreads '<))))))
    (or (listp (cdr gnus-newsgroup-killed))
	(setq gnus-newsgroup-killed (list gnus-newsgroup-killed)))
    (let ((updated nil)
	  (headers gnus-newsgroup-headers))
      (gnus-close-group group)
      (run-hooks 'gnus-exit-group-hook)
      (gnus-score-save)
      (and gnus-newsgroup-expunged-buffer 
	   (buffer-name gnus-newsgroup-expunged-buffer)
	   (kill-buffer gnus-newsgroup-expunged-buffer))
      (gnus-update-read-articles 
       group gnus-newsgroup-unreads gnus-newsgroup-unselected 
       gnus-newsgroup-marked
       t gnus-newsgroup-replied gnus-newsgroup-expirable
       gnus-newsgroup-killed gnus-newsgroup-dormant
       gnus-newsgroup-bookmarks gnus-newsgroup-scored)
      ;; t means ignore unsubscribed newsgroups.
      (and gnus-use-cross-reference
	   (gnus-mark-xrefs-as-read 
	    group headers gnus-newsgroup-unreads gnus-newsgroup-expirable))
      ;; Do not switch windows but change the buffer to work.
      (set-buffer gnus-group-buffer)
      (or (eq 'nndigest method)
	  (gnus-group-update-group group)))
    ;; Make sure where I was, and go to next newsgroup.
    (if (eq method 'nndigest)
	()
      (gnus-group-jump-to-group group)
      (gnus-group-next-unread-group 1))
    (if temporary
	;; If exiting temporary, caller should adjust group mode
	;; buffer point by itself.
	nil				;Nothing to do.
      ;; Return to group mode buffer. 
      (gnus-configure-windows 'newsgroups t)
      (and (get-buffer buf) 
	   (eq mode 'gnus-summary-mode)
	   (kill-buffer buf))
      (if (get-buffer gnus-article-buffer)
	  (bury-buffer gnus-article-buffer))
      (setq gnus-current-select-method gnus-select-method)
      (pop-to-buffer gnus-group-buffer)
      (if (and quit-buffer (buffer-name quit-buffer))
	  (progn
	    (switch-to-buffer quit-buffer)
	    (gnus-set-global-variables)
	    (gnus-configure-windows 'summary))))))

(defun gnus-summary-quit (&optional no-questions)
  "Quit reading current newsgroup without updating read article info."
  (interactive)
  (let ((group gnus-newsgroup-name)
	(quit-buffer (cdr (assoc 'quit-buffer (gnus-find-method-for-group
					       gnus-newsgroup-name)))))
    (if (or no-questions
	    gnus-expert-user
	    (gnus-y-or-n-p "Do you really wanna quit reading this group? "))
	(progn
	  (gnus-close-group group)
	  ;; Return to group selection mode.
	  (and gnus-newsgroup-expunged-buffer 
	       (buffer-name gnus-newsgroup-expunged-buffer)
	       (kill-buffer gnus-newsgroup-expunged-buffer))
	  (gnus-configure-windows 'newsgroups)
	  (if (get-buffer gnus-summary-buffer)
	      (kill-buffer gnus-summary-buffer))
	  (if (get-buffer gnus-article-buffer)
	      (bury-buffer gnus-article-buffer))
	  (pop-to-buffer gnus-group-buffer)
	  (gnus-group-jump-to-group gnus-newsgroup-name)
	  (gnus-group-next-group 1)
	  (if (and quit-buffer (buffer-name quit-buffer))
	      (progn
		(switch-to-buffer quit-buffer)
		(gnus-configure-windows 'summary)))))))

;; Suggested by Per Abrahamsen <amanda@iesd.auc.dk>.
(defun gnus-summary-describe-group ()
  "Describe the current newsgroup."
  (interactive)
  (gnus-group-describe-group gnus-newsgroup-name))

(defun gnus-summary-describe-briefly ()
  "Describe summary mode commands briefly."
  (interactive)
  (message
    (substitute-command-keys "\\<gnus-summary-mode-map>\\[gnus-summary-next-page]:Select  \\[gnus-summary-next-unread-article]:Forward  \\[gnus-summary-prev-unread-article]:Backward  \\[gnus-summary-exit]:Exit  \\[gnus-info-find-node]:Run Info  \\[gnus-summary-describe-briefly]:This help")))

;; Walking around group mode buffer from summary mode.

(defun gnus-summary-next-group (&optional no-article group)
  "Exit current newsgroup and then select next unread newsgroup.
If prefix argument NO-ARTICLE is non-nil, no article is selected initially."
  (interactive "P")
  (gnus-set-global-variables)
  (let ((ingroup gnus-newsgroup-name)
	(sumbuf (current-buffer)))
    (gnus-summary-exit t)		;Update all information.
    (gnus-group-jump-to-group ingroup)
    (let ((group (or group (gnus-summary-search-group)))
	  (buf gnus-summary-buffer))
      (if (null group)
	  (gnus-summary-quit t)
	(message "Selecting %s..." group)
	;; We are now in group mode buffer.
	;; Make sure group mode buffer point is on GROUP.
	(gnus-group-jump-to-group group)
	(unwind-protect
	    (gnus-summary-read-group group nil no-article buf)
	  (and (string= gnus-newsgroup-name ingroup)
	       (progn
		 (set-buffer sumbuf)
		 (gnus-summary-quit t))))))))

(defun gnus-summary-prev-group (no-article)
  "Exit current newsgroup and then select previous unread newsgroup.
If prefix argument NO-ARTICLE is non-nil, no article is selected initially."
  (interactive "P")
  ;; Make sure group mode buffer point is on current newsgroup.
  (gnus-summary-jump-to-group gnus-newsgroup-name)
  (let ((group (gnus-summary-search-group t)))
    (if (null group)
	(progn
	  (message "Exiting %s..." gnus-newsgroup-name)  
	  (gnus-summary-exit)
	  (message ""))
      (message "Selecting %s..." group)
      (gnus-summary-exit t)		;Exit summary mode temporary.
      ;; We are now in group mode buffer.
      ;; We have to adjust point of group mode buffer because current
      ;; point is moved to next unread newsgroup by exiting.
      (gnus-summary-jump-to-group group)
      (gnus-summary-read-group group nil no-article)
      (or (eq (current-buffer)
	      (get-buffer gnus-summary-buffer))
	  (eq gnus-auto-select-next t)
	  ;; Expected newsgroup has nothing to read since the articles
	  ;; are marked as read by cross-referencing. So, try next
	  ;; newsgroup. (Make sure we are in group mode buffer now.)
	  (and (eq (current-buffer)
		   (get-buffer gnus-group-buffer))
	       (gnus-summary-search-group t)
	       (gnus-summary-read-group
		(gnus-summary-search-group t) nil no-article))
	  )
      )))

;; Walking around summary lines.

(defun gnus-summary-first-subject (unread)
  "Go to the first unread subject.
If UNREAD is non-nil, go to the first unread article.
Returns nil if there are no unread articles."
  (let ((begin (point)))
    (if unread
	(if (not (gnus-goto-char 
		  (text-property-any (point-min) (point-max)
				     'gnus-mark gnus-unread-mark)))
	    (progn
	      ;; If there is no unread articles, stay where you are.
	      (goto-char begin)
	      (message "No more unread articles")
	      nil)
	  t)
      (goto-char (point-min)))))

(defun gnus-summary-next-subject (n &optional unread)
  "Go to next N'th summary line.
If N is negative, go to the previous N'th subject line.
If UNREAD is non-nil, only unread articles are selected.
The difference between N and the actual number of steps taken is
returned."
  (interactive "p")
  (let ((backward (< n 0))
	(n (abs n)))
  (while (and (> n 0)
	      (gnus-summary-search-forward unread nil backward))
    (setq n (1- n)))
  (gnus-summary-recenter)
  (if (/= 0 n) (message "No more%s articles" (if unread " unread" "")))
  (gnus-summary-position-cursor)
  n))

(defun gnus-summary-next-unread-subject (n)
  "Go to next N'th unread summary line."
  (interactive "p")
  (gnus-summary-next-subject n t))

(defun gnus-summary-prev-subject (n &optional unread)
  "Go to previous N'th summary line.
If optional argument UNREAD is non-nil, only unread article is selected."
  (interactive "p")
  (gnus-summary-next-subject (- n) unread))

(defun gnus-summary-prev-unread-subject (n)
  "Go to previous N'th unread summary line."
  (interactive "p")
  (gnus-summary-next-subject (- n) t))

(defun gnus-summary-goto-subject (article)
  "Go the subject line of ARTICLE."
  (interactive
   (list
    (string-to-int
     (completing-read "Article number: "
		      (mapcar
		       (lambda (headers)
			 (list
			  (int-to-string (header-number headers))))
		       gnus-newsgroup-headers)
		      nil 'require-match))))
  (or article (error "No article number"))
  (if (or (eq article (gnus-summary-article-number t))
	  (gnus-goto-char
	   (text-property-any
	    (point-min) (point-max) 'gnus-number article)))
      article))

;; Walking around summary lines with displaying articles.

(defun gnus-summary-expand-window ()
  "Expand summary window to show headers full window."
  (interactive)
  (gnus-set-global-variables)
  (gnus-configure-windows 'summary)
  (pop-to-buffer gnus-summary-buffer))

(defun gnus-summary-display-article (article &optional all-header)
  "Display ARTICLE in article buffer."
  (gnus-set-global-variables)
  (if (null article)
      nil
    (gnus-article-prepare article all-header)
    (if (= (gnus-summary-article-mark) ?Z) 
	(progn
	  (forward-line 1)
	  (gnus-summary-position-cursor)))
    (run-hooks 'gnus-select-article-hook)
    (gnus-summary-recenter)
;    (set-window-point (get-buffer-window (current-buffer)) (point-max))
    (sit-for 0)
    (gnus-summary-goto-subject article)
    ;; Successfully display article.
    t))

(defun gnus-summary-select-article (&optional all-headers force pseudo)
  "Select the current article.
If ALL-HEADERS is non-nil, show all header fields.  If FORCE is
non-nil, the article will be re-fetched even if it already present in
the article buffer.  If PSEUDO is non-nil, pseudo-articles will also
be displayed."
  (and (not pseudo) (gnus-summary-pseudo-article)
       (error "This is a pseudo-article."))
  (let ((article (gnus-summary-article-number))
	(all-headers (not (not all-headers)))) ;Must be T or NIL.
    (if (or (null gnus-current-article)
	    (null gnus-article-current)
	    (/= article (cdr gnus-article-current))
	    (not (equal (car gnus-article-current) gnus-newsgroup-name))
	    force)
	;; The requested article is different from the current article.
	(progn
	  (gnus-summary-display-article article all-headers)
	  article)
      (if all-headers (gnus-article-show-all-headers))
      (gnus-configure-windows 'article)
      (pop-to-buffer gnus-summary-buffer)
      nil)))

(defun gnus-summary-set-current-mark (&optional current-mark)
  "Obsolete function."
  nil)

(defun gnus-summary-next-article (unread &optional subject)
  "Select the article after the current one.
If UNREAD is non-nil, only unread articles are selected."
  (interactive "P")
  (let ((header nil)
	(method (car (gnus-find-method-for-group gnus-newsgroup-name))))
    (cond ((gnus-summary-display-article
	    (gnus-summary-search-forward unread subject)))
	  ((and subject
		gnus-auto-select-same
		(gnus-set-difference gnus-newsgroup-unreads
				     (append gnus-newsgroup-marked
					     gnus-newsgroup-dormant))
		(memq this-command
		      '(gnus-summary-next-unread-article
			gnus-summary-next-page
			gnus-summary-kill-same-subject-and-select
			;;gnus-summary-next-article
			;;gnus-summary-next-same-subject
			;;gnus-summary-next-unread-same-subject
			)))
	   ;; Wrap article pointer if there are unread articles.
	   (let ((buffer (current-buffer))
		 (last-point (point)))
	     ;; No more articles with same subject, so jump to the first
	     ;; unread article.
	     (gnus-summary-first-unread-article)
	     ;;(and (eq buffer (current-buffer))
	     ;;	(= (point) last-point)
	     ;;	;; Ignore given SUBJECT, and try again.
	     ;;	(gnus-summary-next-article unread nil))
	     (and (eq buffer (current-buffer))
		  (< (point) last-point)
		  (message "Wrapped"))
	     ))
	  ((and gnus-auto-extend-newsgroup
		(not unread)		;Not unread only
		(not subject)		;Only if subject is not specified.
		(setq header (gnus-more-header-forward)))
	   ;; Extend to next article if possible.
	   ;; Basic ideas by himacdonald@watdragon.waterloo.edu
	   (gnus-extend-newsgroup header nil)
	   ;; Threads feature must be turned off.
	   (let ((buffer-read-only nil))
	     (goto-char (point-max))
	     (gnus-summary-prepare-threads (list header) 0))
	   (gnus-summary-goto-article gnus-newsgroup-end))
	  (t
	   ;; Select next newsgroup automatically if requested.
	   (gnus-summary-jump-to-group gnus-newsgroup-name)
	   (let ((cmd (aref (this-command-keys) 0))
		 (group (if (eq gnus-keep-same-level 'best)
			    (gnus-summary-best-group)
			  (gnus-summary-search-group 
			   nil gnus-keep-same-level)))
		 (auto-select
		  (and gnus-auto-select-next
		       ;;(null (gnus-set-difference gnus-newsgroup-unreads
		       ;;				gnus-newsgroup-marked))
		       (memq this-command
			     '(gnus-summary-next-unread-article
			       gnus-summary-next-article
			       gnus-summary-next-page
			       gnus-summary-next-same-subject
			       gnus-summary-next-unread-same-subject
			       gnus-summary-kill-same-subject
			       gnus-summary-kill-same-subject-and-select
			       ))
		       ;; Ignore characters typed ahead.
		       (not (input-pending-p))
		       )))
	     ;; Keep just the event type of CMD.
	     (if (listp cmd)
		 (setq cmd (car cmd)))
	     (message "No more%s articles%s"
		      (if unread " unread" "")
		      (if (and auto-select
			       (not (eq gnus-auto-select-next 'quietly)))
			  (if (and group (not (eq method 'nndigest)))
			      (format " (Type %s for %s [%s])"
				      (single-key-description cmd)
				      group
				      (car (gnus-gethash 
					    group gnus-newsrc-hashtb)))
			    (format " (Type %s to exit %s)"
				    (single-key-description cmd)
				    gnus-newsgroup-name))
			""))
	     ;; Select next unread newsgroup automagically.
	     (cond ((and auto-select
			 (eq gnus-auto-select-next 'quietly))
		    ;; Select quietly.
		    (if (eq method 'nndigest)
			(gnus-summary-exit)
		      (gnus-summary-next-group)))
		   (auto-select
		    ;; Confirm auto selection.
		    (let* ((event (read-event))
			   (type
			    (if (listp event)
				(car event)
			      event)))
		      (if (and (eq event type) (eq event cmd))
			  (if (eq method 'nndigest)
			      (gnus-summary-exit)
			    (gnus-summary-next-group))
			(setq unread-command-events (list event)))))
		   )
	     ))
	  )))

(defun gnus-summary-next-unread-article ()
  "Select unread article after current one."
  (interactive)
  (gnus-summary-next-article t (and gnus-auto-select-same
				    (gnus-summary-subject-string)))
  (gnus-summary-position-cursor))

(defun gnus-summary-prev-article (unread &optional subject)
  "Select the article after the current one.
If UNREAD is non-nil, only unread articles are selected."
  (interactive "P")
  (let ((header nil))
    (cond ((gnus-summary-display-article
	    (gnus-summary-search-backward unread subject)))
	  ((and subject
		gnus-auto-select-same
		(gnus-set-difference gnus-newsgroup-unreads
				     (append gnus-newsgroup-marked
					     gnus-newsgroup-dormant))
		(memq this-command
		      '(gnus-summary-prev-unread-article
			gnus-summary-prev-page)))
	   ;; Wrap article pointer if there are unread articles.
	   (let ((buffer (current-buffer))
		 (last-point (point)))
	     ;; No more articles with same subject, so jump to the first
	     ;; unread article.
	     (gnus-summary-first-unread-article)
	     (and (eq buffer (current-buffer))
		  (< (point) last-point)
		  (message "Wrapped"))
	     ))
	  ((and gnus-auto-extend-newsgroup
		(not unread)		;Not unread only
		(not subject)		;Only if subject is not specified.
		(setq header (gnus-more-header-backward)))
	   ;; Extend to next article if possible.
	   ;; Basic ideas by himacdonald@watdragon.waterloo.edu
	   (gnus-extend-newsgroup header t)
	   ;; Threads feature must be turned off.
	   (let ((buffer-read-only nil))
	     (goto-char (point-min))
	     (gnus-summary-prepare-threads (list header) 0))
	   (gnus-summary-goto-article gnus-newsgroup-begin))
	  (t
	   ;; Select prev newsgroup automatically if requested.
	   (gnus-summary-jump-to-group gnus-newsgroup-name)
	   (let ((cmd (aref (this-command-keys) 0))
		 (group (if (eq gnus-keep-same-level 'best)
			    (gnus-summary-best-group)
			  (gnus-summary-search-group 
			   t gnus-keep-same-level)))
		 (auto-select
		  (and gnus-auto-select-next
		       (memq this-command
			     '(gnus-summary-prev-unread-article
			       gnus-summary-prev-article
			       gnus-summary-prev-page))
		       ;; Ignore characters typed ahead.
		       (not (input-pending-p)))))
	     ;; Keep just the event type of CMD.
	     (if (listp cmd)
		 (setq cmd (car cmd)))
	     (message "No more%s articles%s"
		      (if unread " unread" "")
		      (if (and auto-select
			       (not (eq gnus-auto-select-next 'quietly)))
			  (if group
			      (format " (Type %s for %s [%s])"
				      (single-key-description cmd)
				      group
				      (car (gnus-gethash 
					    group gnus-newsrc-hashtb)))
			    (format " (Type %s to exit %s)"
				    (single-key-description cmd)
				    gnus-newsgroup-name))
			""))
	     ;; Select next unread newsgroup automagically.
	     (cond ((and auto-select
			 (eq gnus-auto-select-next 'quietly))
		    ;; Select quietly.
		    (gnus-summary-prev-group 1))
		   (auto-select
		    ;; Confirm auto selection.
		    (let* ((event (read-event))
			   (type
			    (if (listp event)
				(car event)
			      event)))
		      (if (and (eq event type) (eq event cmd))
			  (gnus-summary-prev-group 1)
			(setq unread-command-events (list event)))))
		   )
	     ))
	  )))

(defun gnus-summary-prev-unread-article ()
  "Select unred article before current one."
  (interactive)
  (gnus-summary-prev-article t (and gnus-auto-select-same
				    (gnus-summary-subject-string))))

(defun gnus-summary-next-page (lines &optional circular)
  "Show next page of selected article.
If end of article, select next article.
Argument LINES specifies lines to be scrolled up.
If CIRCULAR is non-nil, go to the start of the article instead of 
instead of selecting the next article when reaching the end of the
current article." 
  (interactive "P")
  (setq gnus-summary-buffer (current-buffer))
  (let ((article (gnus-summary-article-number))
	(endp nil))
    (if (or (null gnus-current-article)
	    (null gnus-article-current)
	    (/= article (cdr gnus-article-current))
	    (not (equal (car gnus-article-current) gnus-newsgroup-name)))
	;; Selected subject is different from current article's.
	(gnus-summary-display-article article)
      (gnus-configure-windows 'article)
      (pop-to-buffer gnus-summary-buffer)
      (gnus-eval-in-buffer-window
       gnus-article-buffer
       (setq endp (gnus-article-next-page lines)))
      (if endp
 	  (cond (circular
 		 (gnus-summary-beginning-of-article))
 		(lines
 		 (message "End of message"))
 		((null lines)
 		 (gnus-summary-next-unread-article))))))
  (gnus-summary-position-cursor))

(defun gnus-summary-prev-page (lines)
  "Show previous page of selected article.
Argument LINES specifies lines to be scrolled down."
  (interactive "P")
  (let ((article (gnus-summary-article-number)))
    (if (or (null gnus-current-article)
	    (null gnus-article-current)
	    (/= article (cdr gnus-article-current))
	    (not (equal (car gnus-article-current) gnus-newsgroup-name)))
	;; Selected subject is different from current article's.
	(gnus-summary-display-article article)
      (gnus-configure-windows 'article)
      (pop-to-buffer gnus-summary-buffer)
      (gnus-eval-in-buffer-window gnus-article-buffer
	(gnus-article-prev-page lines))))
  (gnus-summary-position-cursor))

(defun gnus-summary-scroll-up (lines)
  "Scroll up (or down) one line current article.
Argument LINES specifies lines to be scrolled up (or down if negative)."
  (interactive "p")
  (or (gnus-summary-select-article nil nil 'pseudo)
      (gnus-eval-in-buffer-window 
       gnus-article-buffer
       (cond ((> lines 0)
	      (if (gnus-article-next-page lines)
		  (message "End of message")))
	     ((< lines 0)
	      (gnus-article-prev-page (- lines))))))
  (gnus-summary-position-cursor))

(defun gnus-summary-next-same-subject ()
  "Select next article which has the same subject as current one."
  (interactive)
  (gnus-summary-next-article nil (gnus-summary-subject-string)))

(defun gnus-summary-prev-same-subject ()
  "Select previous article which has the same subject as current one."
  (interactive)
  (gnus-summary-prev-article nil (gnus-summary-subject-string)))

(defun gnus-summary-next-unread-same-subject ()
  "Select next unread article which has the same subject as current one."
  (interactive)
  (gnus-summary-next-article t (gnus-summary-subject-string)))

(defun gnus-summary-prev-unread-same-subject ()
  "Select previous unread article which has the same subject as current one."
  (interactive)
  (gnus-summary-prev-article t (gnus-summary-subject-string)))

(defun gnus-summary-first-unread-article ()
  "Select the first unread article. 
Return nil if there are no unread articles."
  (interactive)
  (prog1
      (if (gnus-summary-first-subject t)
	  (gnus-summary-display-article (gnus-summary-article-number)))
    (gnus-summary-position-cursor)))

(defun gnus-summary-best-unread-article ()
  "Select the unread article with the highest score."
  (interactive)
  (gnus-set-global-variables)
  (let ((scored gnus-newsgroup-scored)
	(best -1000000)
	article art)
    (while scored
      (or (> best (cdr (car scored)))
	  (and (memq (setq art (car (car scored))) gnus-newsgroup-unreads)
	       (not (memq art gnus-newsgroup-marked))
	       (not (memq art gnus-newsgroup-dormant))
	       (if (= best (cdr (car scored)))
		   (setq article (min art article))
		 (setq article art)
		 (setq best (cdr (car scored))))))
      (setq scored (cdr scored)))
    (if article 
	(gnus-summary-goto-article article)
      (gnus-summary-first-unread-article))
    (gnus-summary-position-cursor)))

(defun gnus-summary-goto-article (article &optional all-headers)
  "Fetch ARTICLE and display it if it exists.
If ALL-HEADERS is non-nil, no header lines are hidden."
  (interactive
   (list
    (string-to-int
     (completing-read 
      "Article number: "
      (mapcar (lambda (headers) (list (int-to-string (header-number headers))))
	      gnus-newsgroup-headers) 
      nil 'require-match))))
  (if (gnus-summary-goto-subject article)
      (gnus-summary-display-article article all-headers))
  (gnus-summary-position-cursor))

(defun gnus-summary-goto-last-article ()
  "Go to the last article."
  (interactive)
  (if gnus-last-article
      (gnus-summary-goto-article gnus-last-article))
  (gnus-summary-position-cursor))

;; Summary article oriented commands

(defun gnus-summary-refer-parent-article (n)
  "Refer parent article N times.
The difference between N and the number of articles fetched is returned."
  (interactive "p")
  (gnus-set-global-variables)
  (while 
      (and 
       (> n 0)
       (let ((ref (header-references (gnus-get-header-by-number
				      (gnus-summary-article-number)))))
	 (if (and ref (not (equal ref ""))
		  (string-match "<[^<>]*>[ \t]*$" ref))
	     (gnus-summary-refer-article 
	      (substring ref (match-beginning 0) (match-end 0))))))
    (setq n (1- n)))
  (or (zerop n) (message "No references in article or expired article."))
  (gnus-summary-position-cursor)
  n)
    
(defun gnus-summary-refer-article (message-id)
  "Refer article specified by MESSAGE-ID.
NOTE: This command only works with newsgroup that use NNTP."
  (interactive "sMessage-ID: ")
  (if (or (not (stringp message-id))
	  (zerop (length message-id)))
      ()
    ;; Construct the correct Message-ID if necessary.
    ;; Suggested by tale@pawl.rpi.edu.
    (or (string-match "^<" message-id)
	(setq message-id (concat "<" message-id)))
    (or (string-match ">$" message-id)
	(setq message-id (concat message-id ">")))
    (let ((header (car (gnus-gethash message-id gnus-newsgroup-dependencies))))
      (if header
	  (gnus-summary-goto-article (header-number header))
	(if (gnus-article-prepare message-id nil (gnus-read-header message-id))
	    (progn
	      (gnus-summary-insert-line 
	       nil gnus-current-headers 0 nil gnus-read-mark nil nil 
	       (header-subject gnus-current-headers))
	      (forward-line -1)
	      (gnus-summary-position-cursor)
	      (gnus-summary-update-line)
	      message-id)
	  (message "No such references")
	  nil)))))

(defun gnus-summary-enter-digest-group ()
  "Enter a digest group based on the current article."
  (interactive)
  (gnus-summary-select-article)
  (let ((name (format "%s/%d" 
		      (gnus-group-prefixed-name 
		       gnus-newsgroup-name (list 'nndigest "")) 
		      gnus-current-article))
	(buf (current-buffer)))
    (set-buffer gnus-group-buffer)
    (gnus-sethash 
     name 
     (list t nil (list name 3 nil nil 
		       (list 'nndigest gnus-article-buffer
			     (cons 'quit-buffer buf))))
     gnus-newsrc-hashtb)
    (gnus-group-read-group t nil name)))
  
(defun gnus-summary-isearch-article ()
  "Do incremental search forward on current article."
  (interactive)
  (gnus-summary-select-article)
  (gnus-eval-in-buffer-window gnus-article-buffer
			      (isearch-forward)))

(defun gnus-summary-search-article-forward (regexp)
  "Search for an article containing REGEXP forward.
gnus-select-article-hook is not called during the search."
  (interactive
   (list (read-string
	  (concat "Search forward (regexp): "
		  (if gnus-last-search-regexp
		      (concat "(default " gnus-last-search-regexp ") "))))))
  (if (string-equal regexp "")
      (setq regexp (or gnus-last-search-regexp ""))
    (setq gnus-last-search-regexp regexp))
  (if (gnus-summary-search-article regexp nil)
      (gnus-eval-in-buffer-window gnus-article-buffer
	(recenter 0)
	;;(sit-for 1)
	)
    (error "Search failed: \"%s\"" regexp)
    ))

(defun gnus-summary-search-article-backward (regexp)
  "Search for an article containing REGEXP backward.
gnus-select-article-hook is not called during the search."
  (interactive
   (list (read-string
	  (concat "Search backward (regexp): "
		  (if gnus-last-search-regexp
		      (concat "(default " gnus-last-search-regexp ") "))))))
  (if (string-equal regexp "")
      (setq regexp (or gnus-last-search-regexp ""))
    (setq gnus-last-search-regexp regexp))
  (if (gnus-summary-search-article regexp t)
      (gnus-eval-in-buffer-window gnus-article-buffer
	(recenter 0)
	;;(sit-for 1)
	)
    (error "Search failed: \"%s\"" regexp)
    ))

(defun gnus-summary-search-article (regexp &optional backward)
  "Search for an article containing REGEXP.
Optional argument BACKWARD means do search for backward.
gnus-select-article-hook is not called during the search."
  (let ((gnus-select-article-hook nil)	;Disable hook.
	(gnus-mark-article-hook nil)	;Inhibit marking as read.
	(re-search
	 (if backward
	     (function re-search-backward) (function re-search-forward)))
	(found nil)
	(last nil))
    ;; Hidden thread subtrees must be searched for ,too.
    (gnus-summary-show-all-threads)
    (if (eobp) (forward-line -1))
    ;; First of all, search current article.
    ;; We don't want to read article again from NNTP server nor reset
    ;; current point.
    (gnus-summary-select-article)
    (message "Searching article: %d..." gnus-current-article)
    (setq last gnus-current-article)
    (gnus-eval-in-buffer-window gnus-article-buffer
      (save-restriction
	(widen)
	;; Begin search from current point.
	(setq found (funcall re-search regexp nil t))))
    ;; Then search next articles.
    (while (and (not found)
		(gnus-summary-display-article 
		 (gnus-summary-search-subject backward nil nil)))
      (message "Searching article: %d..." gnus-current-article)
      (gnus-eval-in-buffer-window gnus-article-buffer
	(save-restriction
	  (widen)
	  (goto-char (if backward (point-max) (point-min)))
	  (setq found (funcall re-search regexp nil t)))
	))
    (message "")
    ;; Adjust article pointer.
    (or (eq last gnus-current-article)
	(setq gnus-last-article last))
    ;; Return T if found such article.
    found
    ))

(defun gnus-summary-execute-command (field regexp command &optional backward)
  "If FIELD of article header matches REGEXP, execute a COMMAND string.
If FIELD is an empty string (or nil), entire article body is searched for.
If optional (prefix) argument BACKWARD is non-nil, do backward instead."
  (interactive
   (list (let ((completion-ignore-case t))
	   (completing-read "Field name: "
			    '(("Number")("Subject")("From")
			      ("Lines")("Date")("Id")
			      ("Xref")("References"))
			    nil 'require-match))
	 (read-string "Regexp: ")
	 (read-key-sequence "Command: ")
	 current-prefix-arg))
  ;; Hidden thread subtrees must be searched for ,too.
  (gnus-summary-show-all-threads)
  ;; We don't want to change current point nor window configuration.
  (save-excursion
    (save-window-excursion
      (message "Executing %s..." (key-description command))
      ;; We'd like to execute COMMAND interactively so as to give arguments.
      (gnus-execute field regexp
		    (` (lambda ()
			 (call-interactively '(, (key-binding command)))))
		    backward)
      (message "Executing %s... done" (key-description command)))))

(defun gnus-summary-beginning-of-article ()
  "Scroll the article back to the beginning."
  (interactive)
  (gnus-summary-select-article)
  (gnus-eval-in-buffer-window gnus-article-buffer
    (widen)
    (goto-char (point-min))
    (if gnus-break-pages
	(gnus-narrow-to-page))
    ))

(defun gnus-summary-end-of-article ()
  "Scroll to the end of the article."
  (interactive)
  (gnus-summary-select-article)
  (gnus-eval-in-buffer-window gnus-article-buffer
    (widen)
    (goto-char (point-max))
    (if gnus-break-pages
	(gnus-narrow-to-page))
    ))

(defun gnus-summary-show-article ()
  "Force re-fetching of the current article."
  (interactive)
  (gnus-summary-select-article gnus-have-all-headers t t))

(defun gnus-summary-toggle-header (arg)
  "Show the headers if they are hidden, or hide them if they are shown.
If ARG is a positive number, show the entire header.
If ARG is a negative number, hide the unwanted header lines."
  (interactive "P")
  (gnus-set-global-variables)
  (save-excursion
    (set-buffer gnus-article-buffer)
    (let ((buffer-read-only nil))
      (if (numberp arg) 
	  (if (> arg 0) (remove-text-properties 1 (point-max) '(invisible t))
	    (if (< arg 0) (run-hooks 'gnus-article-display-hook)))
	(if (text-property-any 1 (point-max) 'invisible t)
	    (remove-text-properties 1 (point-max) '(invisible t))
	  (run-hooks 'gnus-article-display-hook))))))

(defun gnus-summary-show-all-headers ()
  "Make all header lines visible."
  (interactive)
  (gnus-article-show-all-headers))

(defun gnus-summary-toggle-mime (arg)
  "Toggle MIME processing.
If ARG is a positive number, turn MIME processing on."
  (interactive "P")
  (setq gnus-show-mime
	(if (null arg) (not gnus-show-mime)
	  (> (prefix-numeric-value arg) 0)))
  (gnus-summary-select-article t 'force))

(defun gnus-summary-caesar-message (rotnum)
  "Caesar rotates all letters of current message by 13/47 places.
With prefix arg, specifies the number of places to rotate each letter forward.
Caesar rotates Japanese letters by 47 places in any case."
  (interactive "P")
  (gnus-summary-select-article)
  (let ((mail-header-separator "")) ; !!! Is this necessary?
    (gnus-overload-functions)
    (gnus-eval-in-buffer-window 
     gnus-article-buffer
     (save-restriction
       (widen)
       ;; We don't want to jump to the beginning of the message.
       ;; `save-excursion' does not do its job.
       (move-to-window-line 0)
       (let ((last (point)))
	 (news-caesar-buffer-body rotnum)
	 (goto-char last)
	 (recenter 0))))))

(defun gnus-summary-stop-page-breaking ()
  "Stop page breaking in the current article."
  (interactive)
  (gnus-summary-select-article)
  (gnus-eval-in-buffer-window gnus-article-buffer (widen)))

;; Suggested by Brian Edmonds <bedmonds@prodigy.bc.ca>.

(defun gnus-summary-move-article (n &optional to-newsgroup select-method)
  "Move the current article to a different newsgroup.
If N is a positive number, move the N next articles.
If N is a negative number, move the N previous articles.
If N is nil and any articles have been marked with the process mark,
move those articles instead.
If TO-NEWSGROUP is string, do not prompt for a newsgroup to move to. 
If SELECT-METHOD is symbol, do not move to a specific newsgroup, but
re-spool using this method.
For this function to work, both the current newsgroup and the
newsgroup that you want to move to have to support the `request-move'
and `request-accept' functions. (Ie. mail newsgroups at present.)"
  (interactive "P")
  (gnus-set-global-variables)
  (or (gnus-check-backend-function 'request-move-article gnus-newsgroup-name)
      (error "The current newsgroup does not support article moving"))
  (let ((articles (gnus-summary-work-articles n))
	art-group)
    (if (and (not to-newsgroup) (not select-method))
	(setq to-newsgroup
	      (completing-read 
	       (format "Where do you want to move %s? "
		       (if (> (length articles) 1)
			   (format "these %d articles" (length articles))
			 "this article"))
	       gnus-active-hashtb nil t)))
    (or (gnus-check-backend-function 'request-accept-article 
				     (or select-method to-newsgroup))
	(error "%s does not support article moving" to-newsgroup))
    (message "Moving to %s: %s..." (or select-method to-newsgroup) articles)
    (while articles
      (if (setq art-group
		(gnus-request-move-article 
		 (car articles)
		 gnus-newsgroup-name 
		 (nth 1 (gnus-find-method-for-group gnus-newsgroup-name))
		 (list 'gnus-request-accept-article 
		       (if select-method
			   (quote select-method)
			 to-newsgroup))))
	  (let* ((buffer-read-only nil)
		 (entry 
		  (or
		   (gnus-gethash (car art-group) gnus-newsrc-hashtb)
		   (gnus-gethash 
		    (gnus-group-prefixed-name 
		     (car art-group) 
		     (if select-method (list select-method "")
		       (gnus-find-method-for-group to-newsgroup)))
		    gnus-newsrc-hashtb)))
		 (info (nth 2 entry))
		 (article (car articles))
		 (marked (nth 3 info)))
	    (gnus-summary-goto-subject article)
	    (delete-region (progn (beginning-of-line) (point))
			   (progn (forward-line 1) (point)))
	    (if (not (memq article gnus-newsgroup-unreads))
		(setcar (cdr (cdr info))
			(gnus-add-to-range (nth 2 info) 
					   (list (cdr art-group)))))
	    ;; !!! Here one should copy all the marks over to the new
	    ;; newsgroup, but I couldn't be bothered. nth on that!
	    )
	(message "Couldn't move article %s" (car articles)))
      (gnus-summary-remove-process-mark (car articles))
      (setq articles (cdr articles)))))

(defun gnus-summary-respool-article (n &optional respool-method)
  "Respool the current article.
The article will be squeezed through the mail spooling process again,
which means that it will be put in some mail newsgroup or other
depending on `nnmail-split-methods'.
If N is a positive number, respool the N next articles.
If N is a negative number, respool the N previous articles.
If N is nil and any articles have been marked with the process mark,
respool those articles instead.
For this function to work, both the current newsgroup and the
newsgroup that you want to move to have to support the `request-move'
and `request-accept' functions. (Ie. mail newsgroups at present.)"
  (interactive "P")
  (gnus-set-global-variables)
  (or respool-method
      (setq respool-method
	    (completing-read
	     "What method do you want to use when respooling? "
	     (gnus-methods-using 'respool) nil t)))
  (gnus-summary-move-article n nil (intern respool-method)))

;; Summary score commands.

;; Suggested by boubaker@cenatls.cena.dgac.fr.

(defun gnus-summary-raise-score (n)
  "Raise the score of the current article by N."
  (interactive "p")
  (gnus-summary-set-score (+ (gnus-summary-article-score) n)))

(defun gnus-summary-lower-score (n)
  "Lower the score of the current article by N."
  (interactive "p")
  (gnus-summary-raise-score (- n)))

(defun gnus-summary-set-score (n)
  "Set the score of the current article to N."
  (interactive "p")
  ;; Skip dummy header line.
  (save-excursion
    (if (= (gnus-summary-article-mark) ?Z) (forward-line 1))
    (let ((buffer-read-only nil))
      ;; Set score.
      (gnus-summary-update-mark
       (if (= n gnus-summary-default-score) ? 
	 (if (< n gnus-summary-default-score) 
	     gnus-score-below-mark gnus-score-over-mark)) 'score))
    (let* ((article (gnus-summary-article-number))
	   (score (assq article gnus-newsgroup-scored)))
      (if score (setcdr score n)
	(setq gnus-newsgroup-scored 
	      (cons (cons article n) gnus-newsgroup-scored))))
    (gnus-summary-update-line)))

(defmacro gnus-raise (field expression level)
  (` (gnus-kill (, field) (, expression)
		(function (gnus-summary-raise-score (, level))) t)))

(defmacro gnus-lower (field expression level)
  (` (gnus-kill (, field) (, expression)
		(function (gnus-summary-raise-score (- (, level)))) t)))

;; Summary marking commands.

(defun gnus-summary-raise-same-subject-and-select (score)
  "Raise articles which has the same subject with SCORE and select the next."
  (interactive "p")
  (let ((subject (gnus-summary-subject-string)))
    (gnus-summary-raise-score score)
    (while (gnus-summary-search-subject nil nil subject)
      (gnus-summary-raise-score score))
    (gnus-summary-next-article t)))

(defun gnus-summary-raise-same-subject (score)
  "Raise articles which has the same subject with SCORE."
  (interactive "p")
  (let ((subject (gnus-summary-subject-string)))
    (gnus-summary-raise-score score)
    (while (gnus-summary-search-subject nil nil subject)
      (gnus-summary-raise-score score))
    (gnus-summary-next-subject 1 t)))

(defun gnus-summary-raise-thread (score)
  "Raise articles under current thread with SCORE."
  (interactive "p")
  (let (e)
    (save-excursion
      (let ((level (gnus-summary-thread-level)))
	(gnus-summary-raise-score score)
	(while (and (zerop (gnus-summary-next-subject 1))
		    (> (gnus-summary-thread-level) level))
	  (gnus-summary-raise-score score))
	(setq e (point))))
    (or (zerop (gnus-summary-next-subject 1 t))
	(goto-char e)))
  (gnus-summary-position-cursor)
  (gnus-set-mode-line 'summary))

(defun gnus-summary-lower-same-subject-and-select (score)
  "Raise articles which has the same subject with SCORE and select the next."
  (interactive "p")
  (gnus-summary-raise-same-subject-and-select (- score)))

(defun gnus-summary-lower-same-subject (score)
  "Raise articles which has the same subject with SCORE."
  (interactive "p")
  (gnus-summary-raise-same-subject (- score)))

(defun gnus-summary-lower-thread (score)
  "Raise articles under current thread with SCORE."
  (interactive "p")
  (gnus-summary-raise-thread (- score)))

(defun gnus-summary-kill-same-subject-and-select (unmark)
  "Mark articles which has the same subject as read, and then select the next.
If UNMARK is positive, remove any kind of mark.
If UNMARK is negative, tick articles."
  (interactive "P")
  (if unmark
      (setq unmark (prefix-numeric-value unmark)))
  (let ((count
	 (gnus-summary-mark-same-subject
	  (gnus-summary-subject-string) unmark)))
    ;; Select next unread article. If auto-select-same mode, should
    ;; select the first unread article.
    (gnus-summary-next-article t (and gnus-auto-select-same
				      (gnus-summary-subject-string)))
    (message "%d articles are marked as %s"
	     count (if unmark "unread" "read"))
    ))

(defun gnus-summary-kill-same-subject (unmark)
  "Mark articles which has the same subject as read. 
If UNMARK is positive, remove any kind of mark.
If UNMARK is negative, tick articles."
  (interactive "P")
  (if unmark
      (setq unmark (prefix-numeric-value unmark)))
  (let ((count
	 (gnus-summary-mark-same-subject
	  (gnus-summary-subject-string) unmark)))
    ;; If marked as read, go to next unread subject.
    (if (null unmark)
	;; Go to next unread subject.
	(gnus-summary-next-subject 1 t))
    (message "%d articles are marked as %s"
	     count (if unmark "unread" "read"))
    ))

(defun gnus-summary-mark-same-subject (subject &optional unmark)
  "Mark articles with same SUBJECT as read, and return marked number.
If optional argument UNMARK is positive, remove any kinds of marks.
If optional argument UNMARK is negative, mark articles as unread instead."
  (let ((count 1))
    (save-excursion
      (cond ((null unmark)
	     (gnus-summary-mark-as-read nil gnus-killed-mark))
	    ((> unmark 0)
	     (gnus-summary-tick-article nil t))
	    (t
	     (gnus-summary-tick-article)))
      (while (and subject
		  (gnus-summary-search-forward nil subject))
	(cond ((null unmark)
	       (gnus-summary-mark-as-read nil gnus-killed-mark))
	      ((> unmark 0)
	       (gnus-summary-tick-article nil t))
	      (t
	       (gnus-summary-tick-article)))
	(setq count (1+ count))
	))
    ;; Hide killed thread subtrees.  Does not work properly always.
    ;;(and (null unmark)
    ;;     gnus-thread-hide-killed
    ;;	   (gnus-summary-hide-thread))
    ;; Return number of articles marked as read.
    count
    ))

(defun gnus-summary-mark-as-processable (n &optional unmark)
  "Set the process mark on the next N articles.
If N is negative, mark backward instead.  If UNMARK is non-nil, remove
the process mark instead.  The difference between N and the actual
number of articles marked is returned."
  (interactive "p")
  (let ((backward (< n 0))
	(n (abs n)))
  (while (and 
	  (> n 0)
	  (if unmark
	      (gnus-summary-remove-process-mark (gnus-summary-article-number))
	    (gnus-summary-set-process-mark (gnus-summary-article-number)))
	  (zerop (gnus-summary-next-subject (if backward -1 1))))
    (setq n (1- n)))
  (if (/= 0 n) (message "No more articles"))
  n))

(defun gnus-summary-unmark-as-processable (n)
  "Remove the process mark from the next N articles.
If N is negative, mark backward instead.  The difference between N and
the actual number of articles marked is returned."
  (interactive "p")
  (gnus-summary-mark-as-processable n t))

(defun gnus-summary-unmark-all-processable ()
  "Remove the process mark from all articles."
  (interactive)
  (save-excursion
    (while gnus-newsgroup-processable
      (gnus-summary-remove-process-mark (car gnus-newsgroup-processable))))
  (gnus-summary-position-cursor))

(defun gnus-summary-mark-as-expirable (n)
  "Mark N articles forward as expirable.
If N is negative, mark backward instead. The difference between N and
the actual number of articles marked is returned."
  (interactive "p")
  (gnus-summary-mark-forward n gnus-expirable-mark))

(defun gnus-summary-expire-articles ()
  "Expire all articles that are marked as expirable in the current group."
  (interactive)
  (if (and gnus-newsgroup-expirable
	   (gnus-check-backend-function 
	    'request-expire-articles gnus-newsgroup-name))
      (let ((expirable gnus-newsgroup-expirable))
	;; The list of articles that weren't expired is returned.
	(setq gnus-newsgroup-expirable 
	      (gnus-request-expire-articles gnus-newsgroup-expirable
					    gnus-newsgroup-name))
	;; We go through the old list of expirable, and mark all
	;; really expired articles as non-existant.
	(while expirable
	  (or (memq (car expirable) gnus-newsgroup-expirable)
	      (gnus-summary-mark-as-read (car expirable) "%"))
	  (setq expirable (cdr expirable))))))

;; Suggested by Jack Vinson <vinson@unagi.cis.upenn.edu>.
(defun gnus-summary-delete-article (n)
  "Delete the N next (mail) articles.
This command actually deletes articles. This is not a marking
command. The article will disappear forever from you life, never to
return. 
If N is negative, delete backwards.
If N is nil and articles have been marked with the process mark,
delete these instead."
  (interactive "P")
  (or (gnus-check-backend-function 'request-expire-articles 
				   gnus-newsgroup-name)
      (error "The current newsgroup does not support article deletion."))
  ;; Compute the list of articles to delete.
  (let (articles)
    (if (and n (numberp n))
	(let ((backward (< n 0))
	      (n (abs n)))
	  (save-excursion
	    (while (and (> n 0)
			(setq articles (cons (gnus-summary-article-number) 
					     articles))
			(gnus-summary-search-forward nil nil backward))
	      (setq n (1- n))))
	  (setq articles (sort articles (function <))))
      (setq articles (or (setq gnus-newsgroup-processable
			       (sort gnus-newsgroup-processable (function <)))
			 (list (gnus-summary-article-number)))))
    (if (and gnus-novice-user
	     (not (gnus-y-or-n-p 
		   (format "Do you really want to delete %s forever? "
			   (if (> (length articles) 1) "these articles"
			     "this article")))))
	()
      ;; Delete the articles.
      (setq gnus-newsgroup-expirable 
	    (gnus-request-expire-articles 
	     articles gnus-newsgroup-name 'force))
      (while articles
	(gnus-summary-mark-as-read (car articles) gnus-canceled-mark)
	(setq articles (cdr articles))))))

(defun gnus-summary-edit-article ()
  "Enter into a buffer and edit the current article.
This will have permanent effect only in mail groups."
  (interactive)
  (or (gnus-check-backend-function 
       'request-replace-article gnus-newsgroup-name)
      (error "The current newsgroup does not support article editing."))
  (gnus-summary-select-article t)
  (other-window 1)
  (message "C-c C-c to end edits")
  (setq buffer-read-only nil)
  (text-mode)
  (use-local-map (copy-keymap (current-local-map)))
  (local-set-key "\C-c\C-c" 'gnus-summary-edit-article-done)
  (goto-char 1)
  (search-forward "\n\n" nil t))

(defun gnus-summary-edit-article-done ()
  "Make edits to the current article permanent."
  (interactive)
  (if (not (gnus-request-replace-article 
	    (cdr gnus-article-current) (car gnus-article-current) 
	    (current-buffer)))
      (error "Couldn't replace article.")
    (gnus-article-mode)
    (use-local-map gnus-article-mode-map)
    (setq buffer-read-only t)
    (pop-to-buffer gnus-summary-buffer)))

(defun gnus-summary-mark-article-as-replied (article)
  "Mark ARTICLE replied and update the summary line."
  (setq gnus-newsgroup-replied (cons article gnus-newsgroup-replied))
  (let ((buffer-read-only nil))
    (if (gnus-summary-goto-subject article)
	(progn
	  (gnus-summary-update-mark gnus-replied-mark 'replied)
	  t))))

(defun gnus-summary-set-bookmark (article)
  "Set a bookmark in current article."
  (interactive (list (gnus-summary-article-number)))
  (if (or (not (get-buffer gnus-article-buffer))
	  (not gnus-current-article)
	  (not gnus-article-current)
	  (not (equal gnus-newsgroup-name (car gnus-article-current))))
      (error "No current article selected"))
  ;; Remove old bookmark, if one exists.
  (let ((old (assq article gnus-newsgroup-bookmarks)))
    (if old (setq gnus-newsgroup-bookmarks 
		  (delq old gnus-newsgroup-bookmarks))))
  ;; Set the new bookmark, which is on the form 
  ;; (article-number . line-number-in-body).
  (setq gnus-newsgroup-bookmarks 
	(cons 
	 (cons article 
	       (save-excursion
		 (set-buffer gnus-article-buffer)
		 (count-lines
		  (min (point)
		       (save-excursion
			 (goto-char 1)
			 (search-forward "\n\n" nil t)
			 (point)))
		  (point))))
	 gnus-newsgroup-bookmarks))
  (message "A bookmark has been added to the current article."))

(defun gnus-summary-remove-bookmark (article)
  "Remove the bookmark from the current article."
  (interactive (list (gnus-summary-article-number)))
  ;; Remove old bookmark, if one exists.
  (let ((old (assq article gnus-newsgroup-bookmarks)))
    (if old 
	(progn
	  (setq gnus-newsgroup-bookmarks 
		(delq old gnus-newsgroup-bookmarks))
	  (message "Removed bookmark."))
      (message "No bookmark in current article."))))

;; Suggested by Daniel Quinlan <quinlan@best.com>.
(defun gnus-summary-mark-as-dormant (n)
  "Mark N articles forward as dormant.
If N is negative, mark backward instead.  The difference between N and
the actual number of articles marked is returned."
  (interactive "p")
  (gnus-summary-mark-forward n gnus-dormant-mark))

(defun gnus-summary-set-process-mark (article)
  "Set the process mark on ARTICLE and update the summary line."
  (setq gnus-newsgroup-processable (cons article gnus-newsgroup-processable))
  (let ((buffer-read-only nil))
    (if (gnus-summary-goto-subject article)
	(progn
	  (if (= (gnus-summary-article-mark) ?Z) (forward-line 1))
	  (gnus-summary-update-mark gnus-process-mark 'replied)
	  t))))

(defun gnus-summary-remove-process-mark (article)
  "Remove the process mark from ARTICLE and update the summary line."
  (setq gnus-newsgroup-processable (delq article gnus-newsgroup-processable))
  (let ((buffer-read-only nil))
    (if (gnus-summary-goto-subject article)
	(progn
	  (and (= (gnus-summary-article-mark) ?Z) (forward-line 1))
	  (gnus-summary-update-mark ?  'replied)
	  (if (memq article gnus-newsgroup-replied) 
	      (gnus-summary-update-mark gnus-replied-mark 'replied))
	  t))))

(defun gnus-summary-mark-forward (n &optional mark)
  "Mark N articles as read forwards.
If N is negative, mark backwards instead.
Mark with MARK. If MARK is ? , ?! or ??, articles will be
marked as unread. 
The difference between N and the actual number of articles marked is
returned."
  (interactive "p")
  (gnus-set-global-variables)
  (let ((backward (< n 0))
	(n (abs n))
	(mark (or mark gnus-dread-mark)))
  (while (and (> n 0)
	      (gnus-summary-mark-article nil mark)
	      (zerop (gnus-summary-next-subject (if backward -1 1))))
    (setq n (1- n)))
  (if (/= 0 n) (message "No more %sarticles" (if mark "" "unread ")))
  (gnus-set-mode-line 'summary)
  n))

(defun gnus-summary-mark-article (&optional article mark)
  "Mark ARTICLE with MARK.
MARK can be any character.
Five MARK strings are reserved: ?  (unread), 
?! (ticked), ?? (dormant), ?D (read), ?E (expirable).
If MARK is nil, then the default character ?D is used.
If ARTICLE is nil, then the article on the current line will be
marked." 
  ;; If no mark is given, then we check auto-expiring.
  (and (or (not mark)
	   (and (numberp mark) (or (= mark gnus-killed-mark)
				   (= mark gnus-read-mark))))
       (and gnus-newsgroup-auto-expire (setq mark gnus-expirable-mark)))
  (let* ((buffer-read-only nil)
	 (mark (or (and (stringp mark) (aref mark 0)) mark gnus-dread-mark))
	 (article (or article (gnus-summary-article-number))))
    (if (or (= mark gnus-unread-mark) 
	    (= mark gnus-ticked-mark) 
	    (= mark gnus-dormant-mark))
	(gnus-mark-article-as-unread article mark)
      (gnus-mark-article-as-read article mark))
    (if (gnus-summary-goto-subject article)
	(progn
	  (gnus-summary-show-thread)
	  (beginning-of-line)
	  (if (= (gnus-summary-article-mark) ?Z) (forward-line 1))
	  ;; Fix the mark.
	  (gnus-summary-update-mark mark 'unread)
	  t))))

(defun gnus-summary-update-mark (mark type)
  (beginning-of-line)
  (let ((forward (cdr (assq type gnus-summary-mark-positions)))
	plist)
    (if (not forward)
	()
      (forward-char forward)
      (setq plist (text-properties-at (point)))
      (delete-char 1)
      (and (memq 'gnus-mark plist) (setcar (cdr (memq 'gnus-mark plist)) mark))
      (insert mark)
      (and plist (add-text-properties (1- (point)) (point) plist)))))
  
(defun gnus-mark-article-as-read (article &optional mark)
  "Enter ARTICLE in the pertinent lists and remove it from others."
  ;; Make the article expirable.
  (let ((mark (or (and (stringp mark) (aref mark 0)) mark gnus-dread-mark)))
    (if (= mark gnus-expirable-mark)
	(setq gnus-newsgroup-expirable (cons article gnus-newsgroup-expirable))
      (setq gnus-newsgroup-expirable (delq article gnus-newsgroup-expirable)))
    ;; Remove from unread and marked lists.
    (setq gnus-newsgroup-unreads
	  (delq article gnus-newsgroup-unreads))
    (setq gnus-newsgroup-marked
	  (delq article gnus-newsgroup-marked))
    (setq gnus-newsgroup-dormant
	  (delq article gnus-newsgroup-dormant))))

(defun gnus-mark-article-as-unread (article &optional mark)
  "Enter ARTICLE in the pertinent lists and remove it from others."
  (let ((mark (or (and (stringp mark) (aref mark 0)) mark gnus-ticked-mark)))
    ;; Add to unread list.
    (or (memq article gnus-newsgroup-unreads)
	(setq gnus-newsgroup-unreads
	      (cons article gnus-newsgroup-unreads)))
    ;; If CLEAR-MARK is non-nil, the article must be removed from marked
    ;; list.  Otherwise, it must be added to the list.
    (setq gnus-newsgroup-marked
	  (delq article gnus-newsgroup-marked))
    (setq gnus-newsgroup-dormant
	  (delq article gnus-newsgroup-dormant))
    (setq gnus-newsgroup-expirable 
	  (delq article gnus-newsgroup-expirable))
    (if (= mark gnus-ticked-mark)
	(setq gnus-newsgroup-marked 
	      (cons article gnus-newsgroup-marked)))
    (if (= mark gnus-dormant-mark)
	(setq gnus-newsgroup-dormant 
	      (cons article gnus-newsgroup-dormant)))))

(defalias 'gnus-summary-mark-as-unread-forward 
  'gnus-summary-tick-article-forward)
(make-obsolete 'gnus-summary-mark-as-unread-forward 
	       'gnus-summary-tick-article--forward)
(defun gnus-summary-tick-article-forward (n)
  "Tick N articles forwards.
If N is negative, tick backwards instead.
The difference between N and the number of articles ticked is returned."
  (interactive "p")
  (gnus-summary-mark-forward n gnus-ticked-mark))

(defalias 'gnus-summary-mark-as-unread-backward 'gnus-summary-tick-article-backward)
(make-obsolete 'gnus-summary-mark-as-unread-backward 'gnus-summary-tick-article-backward)
(defun gnus-summary-tick-article-backward (n)
  "Tick N articles backwards.
The difference between N and the number of articles ticked is returned."
  (interactive "p")
  (gnus-summary-mark-forward (- n) gnus-ticked-mark))

(defalias 'gnus-summary-mark-as-unread 'gnus-summary-tick-article)
(make-obsolete 'gnus-summary-mark-as-unread 'gnus-summary-tick-article)
(defun gnus-summary-tick-article (&optional article clear-mark)
  "Mark current article as unread.
Optional 1st argument ARTICLE specifies article number to be marked as unread.
Optional 2nd argument CLEAR-MARK remove any kinds of mark."
  (gnus-summary-mark-article article (if clear-mark gnus-unread-mark
				       gnus-ticked-mark)))

(defun gnus-summary-mark-as-read-forward (n)
  "Mark N articles as read forwards.
If N is negative, mark backwards instead.
The difference between N and the actual number of articles marked is
returned."
  (interactive "p")
  (gnus-summary-mark-forward n))

(defun gnus-summary-mark-as-read-backward (n)
  "Mark the N articles as read backwards.
The difference between N and the actual number of articles marked is
returned."
  (interactive "p")
  (gnus-summary-mark-forward (- n)))

(defun gnus-summary-mark-as-read (&optional article mark)
  "Mark current article as read.
ARTICLE specifies the article to be marked as read.
MARK specifies a string to be inserted at the beginning of the line.
Any kind of string (length 1) except for a space and `-' is ok."
  (gnus-summary-mark-article article mark))

(defun gnus-summary-clear-mark-forward (n)
  "Clear marks from N articles forward.
If N is negative, clear backward instead.
The difference between N and the number of marks cleared is returned."
  (interactive "p")
  (gnus-summary-mark-forward n gnus-unread-mark))

(defun gnus-summary-clear-mark-backward (n)
  "Clear marks from N articles backward.
The difference between N and the number of marks cleared is returned."
  (interactive "p")
  (gnus-summary-mark-forward (- n) gnus-unread-mark))

;; Fix by Per Abrahamsen <amanda@iesd.auc.dk>.
(defalias 'gnus-summary-delete-marked-as-read 
  'gnus-summary-remove-lines-marked-as-read)
(make-obsolete 'gnus-summary-delete-marked-as-read 
	       'gnus-summary-remove-lines-marked-as-read)
(defun gnus-summary-remove-lines-marked-as-read ()
  "Remove lines that are marked as read."
  (interactive)
  (gnus-summary-remove-lines-marked-with 
   (concat (mapconcat
	    (lambda (char) (char-to-string (symbol-value char)))
	    '(gnus-dread-mark gnus-read-mark
	      gnus-killed-mark gnus-kill-file-mark
	      gnus-low-score-mark gnus-expirable-mark)
	    ""))))

(defalias 'gnus-summary-delete-marked-with 
  'gnus-summary-remove-lines-marked-with)
(make-obsolete 'gnus-summary-delete-marked-with 
	       'gnus-summary-remove-lines-marked-with)
;; Rewrite by Daniel Quinlan <quinlan@best.com>.
(defun gnus-summary-remove-lines-marked-with (marks)
  "Remove lines that are marked with MARKS (e.g. \"DK\")."
  (interactive "sMarks: ")
  ;; Fix by Sudish Joseph <joseph@cis.ohio-state.edu>.
  (save-excursion
    (set-buffer gnus-summary-buffer)
    (let ((buffer-read-only nil)
          (marks (concat "^[" marks "]"))
          beg)
      (goto-char (point-min))
      (while (search-forward-regexp marks (point-max) t)
	(progn
	  (move-to-column 0)
	  (setq beg (point))
	  (forward-line 1)
	  ;; Fix by Hallvard B Furuseth <h.b.furuseth@usit.uio.no>
	  (append-to-buffer gnus-newsgroup-expunged-buffer beg (point))
	  (delete-region beg (point)))))
    (or (zerop (buffer-size))
	(if (eobp)
	    (gnus-summary-prev-subject 1)
	  (gnus-summary-position-cursor)))))

(defun gnus-summary-expunge-below (score)
  "Remove articles with score less than SCORE."
  (interactive "P")
  (setq score (if score
		  (prefix-numeric-value score)
		gnus-summary-default-score))
  (save-excursion
    (set-buffer gnus-summary-buffer)
    (goto-char (point-min))
    (let ((buffer-read-only nil)
	  beg)
      (while (not (eobp))
	(if (< (gnus-summary-article-score) score)
	    (progn
	      (setq beg (point))
	      (forward-line 1)
	      (append-to-buffer gnus-newsgroup-expunged-buffer beg (point))
	      (delete-region beg (point)))
	  (forward-line 1)))
      ;; Adjust point.
      (or (zerop (buffer-size))
	  (if (eobp)
	      (gnus-summary-prev-subject 1)
	    (gnus-summary-position-cursor))))))

(defun gnus-summary-mark-below (score mark)
  "Mark articles with score less than SCORE with MARK."
  (interactive "P\ncMark: ")
  (setq score (if score
		  (prefix-numeric-value score)
		gnus-summary-default-score))
  (save-excursion
    (set-buffer gnus-summary-buffer)
    (goto-char (point-min))
    (while (not (eobp))
      (if (< (gnus-summary-article-score) score)
	  (progn
	    (gnus-summary-mark-article nil (char-to-string mark))
	    (forward-line 1))
	(forward-line 1)))))

;; Added by Per Abrahamsen <amanda@iesd.auc.dk>.
(defun gnus-summary-set-mark-below (score)
  "Automatically mark articles with score below SCORE as read."
  (interactive "P")
  (setq score (if score
		  (prefix-numeric-value score)
		gnus-summary-default-score))
  (setq gnus-summary-mark-below score)
  (gnus-summary-update-lines))

(defun gnus-summary-kill-below (score)
  "Mark articles with score below SCORE as read."
  (interactive "P")
  (gnus-summary-mark-below score gnus-killed-mark))

(defun gnus-summary-clear-above (score)
  "Clear all marks from articles with score above SCORE."
  (interactive "P")
  (gnus-summary-mark-above score gnus-unread-mark))

(defun gnus-summary-tick-above (score)
  "Tick all articles with score above SCORE."
  (interactive "P")
  (gnus-summary-mark-above score gnus-ticked-mark))

(defun gnus-summary-mark-above (score mark)
  "Mark articles with score less than SCORE with MARK."
  (interactive "P\ncMark: ")
  (setq score (if score
		  (prefix-numeric-value score)
		gnus-summary-default-score))
  (save-excursion
    (set-buffer gnus-summary-buffer)
    (goto-char (point-min))
    (while (not (eobp))
      (if (> (gnus-summary-article-score) score)
	  (progn
	    (gnus-summary-mark-article nil mark)
	    (forward-line 1))
	(forward-line 1)))))

;; Suggested by Daniel Quinlan <quinlan@best.com>.  
(defun gnus-summary-show-all-expunged ()
  "Show all previously expunge articles."
  (interactive)
  (let ((buffer-read-only nil))
    (save-excursion
      (if (and gnus-newsgroup-expunged-buffer
	       (progn
		 (set-buffer gnus-newsgroup-expunged-buffer)
		 (not (zerop (buffer-size)))))
	  (progn
	    (append-to-buffer gnus-summary-buffer (point-min) (point-max))
	    (erase-buffer))
	(error "No lines expunged")))))

(defun gnus-summary-show-all-dormant ()
  "Display all the hidden articles that are marked as dormant."
  (interactive)
  (let ((int gnus-newsgroup-dormant-subjects)
	(buffer-read-only nil))
    (if (not int)
	(error "No dormant articles hidden."))
    (goto-char (point-min))
    (save-excursion
      (while int
	(insert (cdr (car int)))
	(setq int (cdr int))))
    (gnus-summary-position-cursor)
    (setq gnus-newsgroup-dormant-subjects nil)))

(defun gnus-summary-catchup (all &optional quietly to-here)
  "Mark all articles not marked as unread in this newsgroup as read.
If prefix argument ALL is non-nil, all articles are marked as read.
If QUIETLY is non-nil, no questions will be asked.
If TO-HERE is non-nil, it should be a point in the buffer. All
articles before this point will be marked as read.
The number of articles marked as read is returned."
  (interactive "P")
  (if (or quietly
	  (not gnus-interactive-catchup) ;Without confirmation?
	  gnus-expert-user
	  (gnus-y-or-n-p
	   (if all
	       "Mark absolutely all articles as read? "
	     "Mark all unread articles as read? ")))
      (let ((unreads (length gnus-newsgroup-unreads)))
	(if (gnus-summary-first-subject (not all))
	    (while (and (gnus-summary-mark-as-read nil gnus-catchup-mark)
			(if to-here (< (point) to-here) t)
			(gnus-summary-search-subject nil (not all)))))
	(- unreads (length gnus-newsgroup-unreads))))
  (gnus-summary-position-cursor))

(defun gnus-summary-catchup-to-here (&optional all)
  "Mark all unticked articles before the current one as read.
If ALL is non-nil, also mark ticked and dormant articles as read."
  (interactive)
  (beginning-of-line)
  (gnus-summary-catchup all nil (point))
  (gnus-summary-position-cursor))

(defun gnus-summary-catchup-all (&optional quietly)
  "Mark all articles in this newsgroup as read."
  (interactive)
  (gnus-summary-catchup t quietly))

(defun gnus-summary-catchup-and-exit (all &optional quietly)
  "Mark all articles not marked as unread in this newsgroup as read, then exit.
If prefix argument ALL is non-nil, all articles are marked as read."
  (interactive "P")
  (gnus-summary-catchup all quietly)
  ;; Select next newsgroup or exit.
  (if (eq gnus-auto-select-next 'quietly)
      (gnus-summary-next-group nil)
    (gnus-summary-exit)))

(defun gnus-summary-catchup-all-and-exit (&optional quietly)
  "Mark all articles in this newsgroup as read, and then exit."
  (interactive)
  (gnus-summary-catchup-and-exit t quietly))

;; Thread-based commands.

(defun gnus-summary-toggle-threads (arg)
  "Toggle showing conversation threads.
If ARG is positive number, turn showing conversation threads on."
  (interactive "P")
  (let ((current (gnus-summary-article-number)))
    (setq gnus-show-threads
	  (if (null arg) (not gnus-show-threads)
	    (> (prefix-numeric-value arg) 0)))
    (gnus-summary-prepare)
    (gnus-summary-goto-subject current)))

(defun gnus-summary-show-all-threads ()
  "Show all threads."
  (interactive)
  (save-excursion
    (let ((buffer-read-only nil))
      (subst-char-in-region (point-min) (point-max) ?\^M ?\n t)))
  (gnus-summary-position-cursor))

(defun gnus-summary-show-thread ()
  "Show thread subtrees.
Returns nil if no thread was there to be shown."
  (interactive)
  (prog1
      (save-excursion
	(let ((buffer-read-only nil)
	      (beg (progn (beginning-of-line) (point)))
	      (end (save-excursion (end-of-line) (point))))
	  (prog1
	      ;; Any hidden lines here?
	      (search-forward "\r" end t)
	    (subst-char-in-region beg end ?\^M ?\n t))))
    (gnus-summary-position-cursor)))

(defun gnus-summary-hide-all-threads ()
  "Hide all thread subtrees."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (gnus-summary-hide-thread)
    (while (and (not (eobp)) (zerop (forward-line 1)))
      (gnus-summary-hide-thread))
    )
  (gnus-summary-position-cursor))

(defun gnus-summary-hide-thread ()
  "Hide thread subtrees.
Returns nil if no threads were there to be hidden."
  (interactive)
  (let ((buffer-read-only nil)
	(start (point))
	(level (gnus-summary-thread-level))
	(end (point)))
    ;; Go forward until either the buffer ends or the subthread
    ;; ends. 
    (if (eobp)
	()
      (while (and (zerop (forward-line 1))
		  (> (gnus-summary-thread-level) level))
	(setq end (point)))
      (prog1
	  (save-excursion
	    (goto-char end)
	    (search-backward "\n" start t))
	(subst-char-in-region start end ?\n ?\^M t)
	(forward-line -1)))))

(defun gnus-summary-go-to-next-thread (&optional previous)
  "Go to the same level (or less) next thread.
If PREVIOUS is non-nil, go to previous thread instead.
Return the article number moved to, or nil if moving was impossible."
  (let ((level (gnus-summary-thread-level))
	(article (gnus-summary-article-number)))
    (if previous 
	(while (and (zerop (gnus-summary-prev-subject 1))
		    (> (gnus-summary-thread-level) level)))
      (while (and (zerop (gnus-summary-next-subject 1))
		  (> (gnus-summary-thread-level) level))))
    (let ((oart (gnus-summary-article-number)))
      (and (/= oart article) oart))))

(defun gnus-summary-next-thread (n)
  "Go to the same level next N'th thread.
If N is negative, search backward instead.
Returns the difference between N and the number of skips actually
done."
  (interactive "p")
  (let ((backward (< n 0))
	(n (abs n)))
  (while (and (> n 0)
	      (gnus-summary-go-to-next-thread backward))
    (setq n (1- n)))
  (gnus-summary-position-cursor)
  (if (/= 0 n) (message "No more threads" ))
  n))

(defun gnus-summary-prev-thread (n)
  "Go to the same level previous N'th thread.
Returns the difference between N and the number of skips actually
done."
  (interactive "p")
  (gnus-summary-next-thread (- n)))

(defun gnus-summary-go-down-thread (&optional same)
  "Go down one level in the current thread.
If SAME is non-nil, also move to articles of the same level."
  (let ((level (gnus-summary-thread-level))
	(start (point)))
    (if (and (zerop (forward-line 1))
	     (> (gnus-summary-thread-level) level))
	t
      (goto-char start)
      nil)))

(defun gnus-summary-go-up-thread ()
  "Go up one level in the current thread."
  (let ((level (gnus-summary-thread-level))
	(start (point)))
    (while (and (zerop (forward-line -1))
		(>= (gnus-summary-thread-level) level)))
    (if (>= (gnus-summary-thread-level) level)
	(progn
	  (goto-char start)
	  nil)
      t)))

(defun gnus-summary-down-thread (n)
  "Go down thread N steps.
If N is negative, go up instead.
Returns the difference between N and how many steps down that were
taken."
  (interactive "p")
  (let ((up (< n 0))
	(n (abs n)))
  (while (and (> n 0)
	      (if up (gnus-summary-go-up-thread)
		(gnus-summary-go-down-thread)))
    (setq n (1- n)))
  (gnus-summary-position-cursor)
  (if (/= 0 n) (message "Can't go further" ))
  n))

(defun gnus-summary-up-thread (n)
  "Go up thread N steps.
If N is negative, go up instead.
Returns the difference between N and how many steps down that were
taken."
  (interactive "p")
  (gnus-summary-down-thread (- n)))

(defun gnus-summary-kill-thread (unmark)
  "Mark articles under current thread as read.
If the prefix argument is positive, remove any kinds of marks.
If the prefix argument is negative, tick articles instead."
  (interactive "P")
  (if unmark
      (setq unmark (prefix-numeric-value unmark)))
  (let ((killing t)
	(level (gnus-summary-thread-level)))
    (save-excursion
      (while killing
	;; Mark the article...
	(cond ((null unmark) (gnus-summary-mark-as-read nil gnus-killed-mark))
	      ((> unmark 0) (gnus-summary-tick-article nil t))
	      (t (gnus-summary-tick-article)))
	;; ...and go forward until either the buffer ends or the subtree
	;; ends. 
	(if (not (and (zerop (forward-line 1))
		      (> (gnus-summary-thread-level) level)))
	    (setq killing nil))))
    ;; Hide killed subtrees.
    (and (null unmark)
	 gnus-thread-hide-killed
	 (gnus-summary-hide-thread))
    ;; If marked as read, go to next unread subject.
    (if (null unmark)
	;; Go to next unread subject.
	(gnus-summary-next-subject 1 t)))
  (gnus-set-mode-line 'summary))

;; Summary sorting commands

(defun gnus-summary-sort-by-number (reverse)
  "Sort summary buffer by article number.
Argument REVERSE means reverse order."
  (interactive "P")
  (gnus-summary-sort 'gnus-summary-article-number reverse))

(defun gnus-summary-sort-by-author (reverse)
  "Sort summary buffer by author name alphabetically.
If case-fold-search is non-nil, case of letters is ignored.
Argument REVERSE means reverse order."
  (interactive "P")
  (gnus-summary-sort
   (lambda ()
     (let ((extract (gnus-extract-address-components
		     (header-from (gnus-get-header-by-number
				   (gnus-summary-article-number))))))
       (or (car extract) (cdr extract))))
   reverse))

(defun gnus-summary-sort-by-subject (reverse)
  "Sort summary buffer by subject alphabetically. `Re:'s are ignored.
If case-fold-search is non-nil, case of letters is ignored.
Argument REVERSE means reverse order."
  (interactive "P")
  (gnus-summary-sort
   (lambda ()
     (downcase (gnus-simplify-subject (gnus-summary-subject-string))))
   reverse))

(defun gnus-summary-sort-by-date (reverse)
  "Sort summary buffer by date.
Argument REVERSE means reverse order."
  (interactive "P")
  (gnus-summary-sort
   (lambda ()
     (gnus-sortable-date
      (header-date (gnus-get-header-by-number (gnus-summary-article-number)))))
   reverse))

(defun gnus-summary-sort-by-score (reverse)
  "Sort summary buffer by score.
Argument REVERSE means reverse order."
  (interactive "P")
  (gnus-summary-sort 'gnus-summary-article-score (not reverse)))

(defun gnus-summary-sort (predicate reverse)
  ;; Sort summary buffer by PREDICATE.  REVERSE means reverse order. 
  (let (buffer-read-only)
    (goto-char (point-min))
    (sort-subr reverse 'forward-line 'end-of-line predicate)))

(defun gnus-sortable-date (date)
  "Make sortable string by string-lessp from DATE.
Timezone package is used."
  (let* ((date   (timezone-fix-time date nil nil)) ;[Y M D H M S]
	 (year   (aref date 0))
	 (month  (aref date 1))
	 (day    (aref date 2)))
    (timezone-make-sortable-date year month day 
				 (timezone-make-time-string
				  (aref date 3) (aref date 4) (aref date 5)))
    ))


;; Summary saving commands.

(defun gnus-summary-save-article (n)
  "Save the current article using the default saver function.
If N is a positive number, save the N next articles.
If N is a negative number, save the N previous articles.
If N is nil and any articles have been marked with the process mark,
save those articles instead.
The variable `gnus-default-article-saver' specifies the saver function."
  (interactive "P")
  (let (articles process)
    (if (and n (numberp n))
	(let ((backward (< n 0))
	      (n (abs n)))
	  (save-excursion
	    (while (and (> n 0)
			(setq articles (cons (gnus-summary-article-number) 
					     articles))
			(gnus-summary-search-forward nil nil backward))
	      (setq n (1- n))))
	  (setq articles (sort articles (function <))))
      (if gnus-newsgroup-processable
	  (progn
	    (setq articles (setq gnus-newsgroup-processable
				 (nreverse gnus-newsgroup-processable)))
	    (setq process t))
	(setq articles (list (gnus-summary-article-number)))))
    (while articles
      (let ((header (gnus-gethash (int-to-string (car articles))
				  gnus-newsgroup-headers-hashtb-by-number)))
	(if (vectorp header)
	    (progn
	      (gnus-summary-display-article (car articles) t)
	      (if (not gnus-save-all-headers)
		  (gnus-article-hide-headers t))
	      (if gnus-default-article-saver
		  (funcall gnus-default-article-saver)
		(error "No default saver is defined.")))
	  (if (assq 'name header)
	      (gnus-copy-file (cdr (assq 'name header)))
	    (message "Article %d is unsaveable" (car articles)))))
      (if process
	  (gnus-summary-remove-process-mark (car articles)))
      (setq articles (cdr articles)))
    (if process (setq gnus-newsgroup-processable 
		      (nreverse gnus-newsgroup-processable)))
    (gnus-summary-position-cursor)
    n))

(defun gnus-summary-pipe-output (arg)
  "Pipe the current article to a subprocess.
If N is a positive number, pipe the N next articles.
If N is a negative number, pipe the N previous articles.
If N is nil and any articles have been marked with the process mark,
pipe those articles instead."
  (interactive "P")
  (let ((gnus-default-article-saver 'gnus-summary-save-in-pipe))
    (gnus-summary-save-article arg)))

(defun gnus-summary-save-article-mail (arg)
  "Append the current article to an mail file.
If N is a positive number, save the N next articles.
If N is a negative number, save the N previous articles.
If N is nil and any articles have been marked with the process mark,
save those articles instead."
  (interactive "P")
  (let ((gnus-default-article-saver 'gnus-summary-save-in-mail))
    (gnus-summary-save-article arg)))

(defun gnus-summary-save-in-rmail (&optional filename)
  "Append this article to Rmail file.
Optional argument FILENAME specifies file name.
Directory to save to is default to `gnus-article-save-directory' which
is initialized from the SAVEDIR environment variable."
  (interactive)
  (let ((default-name
	  (funcall gnus-rmail-save-name gnus-newsgroup-name
		   gnus-current-headers gnus-newsgroup-last-rmail)))
    (or filename
	(setq filename
	      (read-file-name
	       (concat "Save article in rmail file: (default "
		       (file-name-nondirectory default-name) ") ")
	       (file-name-directory default-name)
	       default-name)))
    (gnus-make-directory (file-name-directory filename))
    (gnus-eval-in-buffer-window 
     gnus-article-buffer
     (save-excursion
       (save-restriction
	 (widen)
	 (gnus-output-to-rmail filename))))
    ;; Remember the directory name to save articles.
    (setq gnus-newsgroup-last-rmail filename)))

(defun gnus-summary-save-in-mail (&optional filename)
  "Append this article to Unix mail file.
Optional argument FILENAME specifies file name.
Directory to save to is default to `gnus-article-save-directory' which
is initialized from the SAVEDIR environment variable."
  (interactive)
  (let ((default-name
	  (funcall gnus-mail-save-name gnus-newsgroup-name
		   gnus-current-headers gnus-newsgroup-last-mail)))
    (or filename
	(setq filename
	      (read-file-name
	       (concat "Save article in Unix mail file: (default "
		       (file-name-nondirectory default-name) ") ")
	       (file-name-directory default-name)
	       default-name)))
    (setq filename
	  (expand-file-name filename
			    (and default-name
				 (file-name-directory default-name))))
    (gnus-make-directory (file-name-directory filename))
    (gnus-eval-in-buffer-window 
     gnus-article-buffer
     (save-excursion
       (save-restriction
	 (widen)
	 (if (and (file-readable-p filename) (rmail-file-p filename))
	     (gnus-output-to-rmail filename)
	   (rmail-output filename 1 t t)))))
    ;; Remember the directory name to save articles.
    (setq gnus-newsgroup-last-mail filename)))

(defun gnus-summary-save-in-file (&optional filename)
  "Append this article to file.
Optional argument FILENAME specifies file name.
Directory to save to is default to `gnus-article-save-directory' which
is initialized from the SAVEDIR environment variable."
  (interactive)
  (let ((default-name
	  (funcall gnus-file-save-name gnus-newsgroup-name
		   gnus-current-headers gnus-newsgroup-last-file)))
    (or filename
	(setq filename
	      (read-file-name
	       (concat "Save article in file: (default "
		       (file-name-nondirectory default-name) ") ")
	       (file-name-directory default-name)
	       default-name)))
    (gnus-make-directory (file-name-directory filename))
    (gnus-eval-in-buffer-window 
     gnus-article-buffer
     (save-excursion
       (save-restriction
	 (widen)
	 (gnus-output-to-file filename))))
    ;; Remember the directory name to save articles.
    (setq gnus-newsgroup-last-file filename)))

(defun gnus-summary-save-in-pipe (&optional command)
  "Pipe this article to subprocess."
  (interactive)
  (let ((command (read-string "Shell command on article: "
			      gnus-last-shell-command)))
    (if (string-equal command "")
	(setq command gnus-last-shell-command))
    (gnus-eval-in-buffer-window 
     gnus-article-buffer
     (save-restriction
       (widen)
       (shell-command-on-region (point-min) (point-max) command nil)))
    (setq gnus-last-shell-command command)))

;; Summary extract commands

(defun gnus-summary-insert-pseudos (pslist)
  (let ((buffer-read-only nil)
	(article (gnus-summary-article-number))
	b)
    (or (gnus-summary-goto-subject article)
	(error (format "No such article: %d" article)))
    (gnus-summary-position-cursor)
    (if (eq gnus-view-pseudos 'automatic)
	(while pslist
	  (and (assq 'execute (car pslist))
	       (gnus-execute-command (cdr (assq 'execute (car pslist)))))
	  (setq pslist (cdr pslist)))
      (save-excursion
	(while pslist
	  (gnus-summary-goto-subject (or (cdr (assq 'article (car pslist)))
					 (gnus-summary-article-number)))
	  (forward-line 1)
	  (setq b (point))
	  (insert "          " (file-name-nondirectory 
				(cdr (assq 'name (car pslist))))
		  ": " (or (cdr (assq 'execute (car pslist))) "") "\n")
	  (add-text-properties 
	   b (1+ b) (list 'gnus-subject (cdr (assq 'name (car pslist)))
			  'gnus-number gnus-reffed-article-number
			  'gnus-mark gnus-unread-mark
			  'gnus-pseudo (car pslist)
			  'gnus-thread 0))
	  (gnus-sethash (int-to-string gnus-reffed-article-number)
			(car pslist) gnus-newsgroup-headers-hashtb-by-number)
	  (setq gnus-reffed-article-number (1- gnus-reffed-article-number))
	  (setq pslist (cdr pslist)))))))

(defun gnus-request-pseudo-article (props)
  (cond ((assq 'execute props)
	 (gnus-execute-command (cdr (assq 'execute props))))
	((assq 'digest props)
	 )
	)
  (let ((gnus-current-article (gnus-summary-article-number)))
    (run-hooks 'gnus-mark-article-hook)))

(defun gnus-execute-command (command &optional automatic)
  (save-excursion
    (gnus-article-setup-buffer)
    (set-buffer gnus-article-buffer)
    (let ((command (if automatic command (read-string "Command: " command)))
	  (buffer-read-only nil))
      (erase-buffer)
      (insert "$ " command "\n\n")
      (if gnus-view-pseudo-asynchronously
	  (start-process "gnus-execute" nil "sh" "-c" command)
	(call-process "sh" nil t nil "-c" command)))))

(defun gnus-copy-file (file &optional to)
  "Copy FILE to TO."
  (interactive
   (list (read-file-name "Copy file: " default-directory)
	 (read-file-name "Copy file to: " default-directory)))
  (or to (setq to (read-file-name "Copy file to: " default-directory)))
  (and (file-directory-p to) 
       (setq to (concat (file-name-as-directory to)
			(file-name-nondirectory file))))
  (copy-file file to))

;; Summary score file commands

;; Much modification of the kill (ahem, score) code and lots of the
;; functions are written by Per Abrahamsen <amanda@iesd.auc.dk>.

(defun gnus-summary-header (header)
  ;; Return HEADER for current articles, or error.
  (let ((article (gnus-summary-article-number)))
    (if article
	(aref (gnus-get-header-by-number article)
	      (nth 1 (assoc header gnus-header-index)))
      (error "No article on current line"))))

(defun gnus-summary-score-entry (header match type score date &optional prompt)
  "Enter score file entry.
HEADER is the header being scored.
MATCH is the string we are looking for.
TYPE is a flag indicating if it is a regexp or substring.
SCORE is the score to add.
DATE is the expire date."
  (interactive (list (completing-read "Header: "
				      gnus-header-index
				      (lambda (x) (fboundp (nth 2 x)))
				      t)
		     (read-string "Match: ")
		     (y-or-n-p "Use regexp match? ")
		     (prefix-numeric-value current-prefix-arg)
		     (if (y-or-n-p "Expire kill? ")
			 (current-time-string)
		       nil)))
  (and prompt (setq match (read-string "Match: " match)))
  (let ((score (gnus-score-default score)))
    (gnus-summary-score-effect header match type score)
  
    (gnus-summary-score-effect header match type score)
    (gnus-score-set header
		    (cons (list match type score date) 
			  (gnus-score-get header) ))
    (gnus-score-set 'touched t)))

(defun gnus-summary-score-effect (header match type score)
  "Simulate the effect of a score file entry.
HEADER is the header being scored.
MATCH is the string we are looking for.
TYPE is a flag indicating if it is a regexp or substring.
SCORE is the score to add."
  (interactive (list (completing-read "Header: "
				      gnus-header-index
				      (lambda (x) (fboundp (nth 2 x)))
				      t)
		     (read-string "Match: ")
		     (y-or-n-p "Use regexp match? ")
		     (prefix-numeric-value current-prefix-arg)))
  (save-excursion
    (or (and (stringp match) (> (length match) 0))
      (error "No match"))
    (goto-char (point-min))
    (let ((regexp (if type
		      match
		    (concat "\\`.*" (regexp-quote match) ".*\\'"))))
      (while (not (eobp))
	(let ((content (gnus-summary-header header))
	      (case-fold-search t))
	  (and content
	       (if (string-match regexp content)
		   (gnus-summary-raise-score score))))
	(beginning-of-line 2)))))

(defun gnus-summary-score-crossposting (score date)
   ;; Enter score file entry for current crossposting.
   ;; SCORE is the score to add.
   ;; DATE is the expire date.
   (let ((xref (gnus-summary-header "xref"))
	 (start 0)
	 group)
     (or xref (error "This article is not crossposted"))
     (while (string-match " \\([^ \t]+\\):" xref start)
       (setq start (match-end 0))
       (if (not (string= 
		 (setq group 
		       (substring xref (match-beginning 1) (match-end 1)))
		 gnus-newsgroup-name))
	   (gnus-summary-score-entry
	    "xref" (concat " " group ":") nil score date t)))))

(defun gnus-summary-temporarily-lower-by-subject (level)
  "Temporarily lower score by LEVEL for current subject.
See `gnus-kill-expiry-days'."
  (interactive "P")
  (gnus-summary-score-entry
   "subject" (gnus-simplify-subject-re (gnus-summary-header "subject"))
   nil (- (gnus-score-default level) )
   (current-time-string) t))

(defun gnus-summary-temporarily-lower-by-author (level)
  "Temporarily lower score by LEVEL for current author.
See `gnus-kill-expiry-days'."
  (interactive "P")
  (gnus-summary-score-entry
   "from" (gnus-summary-header "from") nil (- (gnus-score-default level)) 
   (current-time-string) t))

(defun gnus-summary-temporarily-lower-by-xref (level)
  "Temporarily lower score by LEVEL for current xref.
See `gnus-kill-expiry-days'."
  (interactive "P")
  (gnus-summary-score-crossposting (- (gnus-score-default level)) (current-time-string)))

(defun gnus-summary-temporarily-lower-by-thread (level)
  "Temporarily lower score by LEVEL for current thread.
See `gnus-kill-expiry-days'."
  (interactive "P")
  (gnus-summary-score-entry
   "references" (gnus-summary-header "id")
   nil (- (gnus-score-default level)) (current-time-string)))

(defun gnus-summary-lower-by-subject (level)
  "Lower score by LEVEL for current subject."
  (interactive "P")
  (gnus-summary-score-entry
   "subject" (gnus-simplify-subject-re (gnus-summary-header "subject"))
   nil (- (gnus-score-default level)) 
   nil t))

(defun gnus-summary-lower-by-author (level)
  "Lower score by LEVEL for current author."
  (interactive "P")
  (gnus-summary-score-entry
   "from" (gnus-summary-header "from") nil (- (gnus-score-default level)) nil t))

(defun gnus-summary-lower-by-xref (level)
  "Lower score by LEVEL for current xref."
  (interactive "P")
  (gnus-summary-score-crossposting (- (gnus-score-default level)) nil))

(defun gnus-summary-lower-followups-to-author (level)
  "Lower score by LEVEL for all followups to the current author."
  (interactive "P")
  (gnus-kill-file-lower-followups-to-author
   (gnus-score-default level)
   (let ((article (gnus-summary-article-number)))
     (if article (gnus-get-header-by-number article)
       (error "No article on current line")))))

(defun gnus-summary-temporarily-raise-by-subject (level)
  "Temporarily raise score by LEVEL for current subject.
See `gnus-kill-expiry-days'."
  (interactive "P")
  (gnus-summary-score-entry
   "subject" (gnus-simplify-subject-re (gnus-summary-header "subject"))
   nil level (current-time-string) t))

(defun gnus-summary-temporarily-raise-by-author (level)
  "Temporarily raise score by LEVEL for current author.
See `gnus-kill-expiry-days'."
  (interactive "P")
  (gnus-summary-score-entry
   "from" (gnus-summary-header "from") nil level (current-time-string) t))

(defun gnus-summary-temporarily-raise-by-xref (level)
  "Temporarily raise score by LEVEL for current xref.
See `gnus-kill-expiry-days'."
  (interactive "P")
  (gnus-summary-score-crossposting level (current-time-string)))

(defun gnus-summary-temporarily-raise-by-thread (level)
  "Temporarily raise score by LEVEL for current thread.
See `gnus-kill-expiry-days'."
  (interactive "P")
  (gnus-summary-score-entry
   "references" (gnus-summary-header "id")
   nil level (current-time-string)))

(defun gnus-summary-raise-by-subject (level)
  "Raise score by LEVEL for current subject."
  (interactive "P")
  (gnus-summary-score-entry
   "subject" (gnus-simplify-subject-re (gnus-summary-header "subject"))
   nil level nil t))

(defun gnus-summary-raise-by-author (level)
  "Raise score by LEVEL for current author."
  (interactive "P")
  (gnus-summary-score-entry
   "from" (gnus-summary-header "from") nil level nil t))

(defun gnus-summary-raise-by-xref (level)
  "Raise score by LEVEL for current xref."
  (interactive "P")
  (gnus-summary-score-crossposting level nil))

(defun gnus-summary-edit-global-kill ()
  "Edit a global score file."
  (interactive)
  (setq gnus-current-kill-article (gnus-summary-article-number))
  (gnus-kill-file-edit-file nil)	;Nil stands for global score file.
  (message
   (substitute-command-keys
    "Editing a global score file (Type \\[gnus-kill-file-exit] to exit)")))

(defun gnus-summary-raise-followups-to-author (level)
  "Raise score by LEVEL for all followups to the current author."
  (interactive "P")
  (gnus-kill-file-raise-followups-to-author
   (gnus-score-default level)
   (let ((article (gnus-summary-article-number)))
     (if article (gnus-get-header-by-number article)
       (error "No article on current line")))))

(defun gnus-summary-edit-local-kill ()
  "Edit a local score file applied to the current newsgroup."
  (interactive)
  (setq gnus-current-kill-article (gnus-summary-article-number))
  (gnus-kill-file-edit-file gnus-newsgroup-name)
  (message
   (substitute-command-keys
    "Editing a local score file (Type \\[gnus-kill-file-exit] to exit)")))



;;;
;;; Gnus article mode
;;;

(if gnus-article-mode-map
    nil
  (setq gnus-article-mode-map (make-keymap))
  (suppress-keymap gnus-article-mode-map)
  (define-key gnus-article-mode-map " " 'gnus-article-next-page)
  (define-key gnus-article-mode-map "\177" 'gnus-article-prev-page)
  (define-key gnus-article-mode-map "\C-x^" 'gnus-article-refer-article)
  (define-key gnus-article-mode-map "h" 'gnus-article-show-summary)
  (define-key gnus-article-mode-map "s" 'gnus-article-show-summary)
  (define-key gnus-article-mode-map "\C-xm" 'gnus-article-mail)
  (define-key gnus-article-mode-map "\C-xM" 'gnus-article-mail-with-original)
  (define-key gnus-article-mode-map "?" 'gnus-article-describe-briefly)
  
  ;; Duplicate almost all summary keystrokes in the article mode map.
  (let ((commands 
	 (list "#" "\M-#" "\C-c\M-#" "\r" "n" "p"
	       "N" "P" "\M-\C-n" "\M-\C-p" "." "\M-s" "\M-r"
	       "<" ">" "l" "j" "^" "\M-^" "-" "u" "U" "d" "D"
	       "\M-u" "\M-U" "k" "\C-k" "\M-\C-k" "c" "x" "X" 
	       "\M-\C-x" "\M-\177" "b" "B" "$" "w" "\C-c\C-r"
	       "t" "\M-t" "a" "f" "F" "C" "S" "r" "R" "\C-c\C-f"
	       "m" "o" "\C-o" "|" "\M-m" "\M-\C-m" "\M-k" "m" "M"
	       "V" "\C-c\C-d" "q" "Q")))
    (while commands
      (define-key gnus-article-mode-map (car commands) 
	'gnus-article-summary-command)
      (setq commands (cdr commands)))))


(defun gnus-article-mode ()
  "Major mode for reading an article.
All normal editing commands are switched off.
The following commands are available:

\\<gnus-article-mode-map>
\\[gnus-article-next-page]\t Scroll the article one page forwards
\\[gnus-article-prev-page]\t Scroll the article one page backwards
\\[gnus-article-refer-article]\t Go to the article referred to by an article id near point
\\[gnus-article-show-summary]\t Display the summary buffer
\\[gnus-article-mail]\t Send a reply to the address near point
\\[gnus-article-mail-with-original]\t Send a reply to the address near point; include the original article
\\[gnus-article-describe-briefly]\t Describe the current mode briefly
\\[gnus-info-find-node]\t Go to the Gnus info node

"
  (interactive)
  (if gnus-visual (gnus-article-make-menu-bar))
  (kill-all-local-variables)
  (setq mode-line-modified "-- ")
  (make-local-variable 'mode-line-format)
  (setq mode-line-format (copy-sequence mode-line-format))
  (and (equal (nth 3 mode-line-format) "   ")
       (setcar (nthcdr 3 mode-line-format) ""))
  (setq mode-name "Article")
  (setq major-mode 'gnus-article-mode)
  (make-local-variable 'minor-mode-alist)
  (or (assq 'gnus-show-mime minor-mode-alist)
      (setq minor-mode-alist
	    (cons (list 'gnus-show-mime " MIME") minor-mode-alist)))
  (use-local-map gnus-article-mode-map)
  (make-local-variable 'page-delimiter)
  (setq page-delimiter gnus-page-delimiter)
  (buffer-disable-undo (current-buffer))
  (setq buffer-read-only t)		;Disable modification
  (run-hooks 'gnus-article-mode-hook))

(defun gnus-article-setup-buffer ()
  "Initialize article mode buffer."
  (or (get-buffer gnus-article-buffer)
      (save-excursion
	(set-buffer (get-buffer-create gnus-article-buffer))
	(gnus-add-current-to-buffer-list)
	(gnus-article-mode))))

(defun gnus-request-article-this-buffer (article &optional group)
  "Get an article and insert it into this buffer."
  (setq group (or group gnus-newsgroup-name))
  ;; Using `gnus-request-article' directly will insert the article into
  ;; `nntp-server-buffer' - so we'll save some time by not having to
  ;; copy it from the server buffer into the article buffer.

  ;; We only request an article by message-id when we do not have the
  ;; headers for it, so we'll have to get those.
  (and (stringp article) (gnus-read-header article))

  ;; If the article number is negative, that means that this article
  ;; doesn't belong in this newsgroup (possibly), so we find its
  ;; message-id and request it by id instead of number.
  (if (and (numberp article) (< article 0))
      (save-excursion
	(set-buffer gnus-summary-buffer)
	(let ((header (gnus-gethash (int-to-string article)
				    gnus-newsgroup-headers-hashtb-by-number)))
	  (if (vectorp header)
	      ;; It's a real article.
	      (setq article (header-id header))
	    ;; It is an extracted pseudo-article.
	    (setq article nil)
	    (gnus-request-pseudo-article header)))))
  ;; Get the article and into the article buffer.
  (if article
      (progn
       (erase-buffer)
       (and (gnus-request-article article group (current-buffer))
	    'article))
    'pseudo))

(defun gnus-read-header (id)
  "Read the headers of article ID and enter them into the Gnus system."
  (or gnus-newsgroup-headers-hashtb-by-number
      (gnus-make-headers-hashtable-by-number))
  (let (header)
    (if (not (setq header 
		   (car (if (let ((gnus-nov-is-evil t))
			      (gnus-retrieve-headers 
			       (list id) gnus-newsgroup-name))
			    (gnus-get-newsgroup-headers)))))
	nil
      (if (stringp id)
	  (header-set-number header gnus-reffed-article-number))
      (setq gnus-newsgroup-headers (cons header gnus-newsgroup-headers))
      (gnus-sethash (int-to-string (header-number header)) header
		    gnus-newsgroup-headers-hashtb-by-number)
      (if (stringp id)
	  (setq gnus-reffed-article-number (1- gnus-reffed-article-number)))
      (setq gnus-current-headers header)
      header)))

(defun gnus-article-prepare (article &optional all-headers header)
  "Prepare ARTICLE in article mode buffer.
ARTICLE should either be an article number or a Message-ID.
If ARTICLE is an id, HEADER should be the article headers.
If ALL-HEADERS is non-nil, no headers are hidden."
  (save-excursion
    ;; Make sure we start in a summary buffer.
    (or (eq major-mode 'gnus-summary-mode)
	(set-buffer gnus-summary-buffer))
    (setq gnus-summary-buffer (current-buffer))
    ;; Make sure the connection to the server is alive.
    (or (gnus-server-opened (gnus-find-method-for-group gnus-newsgroup-name))
	(progn
	  (gnus-check-news-server 
	   (gnus-find-method-for-group gnus-newsgroup-name))
	  (gnus-request-group gnus-newsgroup-name t)))
    (or gnus-newsgroup-headers-hashtb-by-number
	(gnus-make-headers-hashtable-by-number))
    (let* ((article (if header (header-number header) article))
	   (summary-buffer (current-buffer))
	   (internal-hook gnus-article-internal-prepare-hook)
	   (bookmark (cdr (assq article gnus-newsgroup-bookmarks)))
	   (group gnus-newsgroup-name)
	   result)
      (save-excursion
	(gnus-article-setup-buffer)
	(set-buffer gnus-article-buffer)
	(let ((buffer-read-only nil))
	  (if (not (setq result (gnus-request-article-this-buffer 
				 article group)))
	      ;; There is no such article.
	      (progn
		(save-excursion
		  (set-buffer gnus-summary-buffer)
		  (setq gnus-current-article 0)
		  (and (numberp article) 
		       (gnus-summary-mark-as-read article gnus-canceled-mark))
		  (message "No such article (may be canceled)")
		  (ding) )
		(setq gnus-article-current nil)
		nil)
	    (if (not (eq result 'article))
		(progn
		  (save-excursion
		    (set-buffer summary-buffer)
		    (setq gnus-last-article gnus-current-article
			  gnus-current-article 0
			  gnus-current-headers nil
			  gnus-article-current nil)
		    (gnus-configure-windows 'article)
		    (gnus-set-mode-line 'summary)
		    (gnus-set-global-variables))
		  (gnus-set-mode-line 'article))
	      ;; The result from the `request' was an actual article -
	      ;; or at least some text that is now displayed in the
	      ;; article buffer.
	      (if (and (numberp article)
		       (not (eq article gnus-current-article)))
		  ;; Seems like a new article has been selected.
		  ;; `gnus-current-article' must be an article number.
		  (save-excursion
		    (set-buffer summary-buffer)
		    (setq gnus-last-article gnus-current-article)
		    (setq gnus-current-article article)
		    (setq gnus-current-headers 
			  (gnus-get-header-by-number gnus-current-article))
		    (setq gnus-article-current 
			  (cons gnus-newsgroup-name gnus-current-article))
		    (gnus-set-mode-line 'summary)
		    (run-hooks 'gnus-mark-article-hook)
		    (and gnus-visual 
			 (run-hooks 'gnus-visual-mark-article-hook))
		    ;; Set the global newsgroup variables here.
		    ;; Suggested by Jim Sisolak
		    ;; <sisolak@trans4.neep.wisc.edu>.
		    (gnus-set-global-variables)))
	      ;; gnus-have-all-headers must be either T or NIL.
	      (setq gnus-have-all-headers
		    (not (not (or all-headers gnus-show-all-headers))))
	      ;; Hooks for getting information from the article.
	      ;; This hook must be called before being narrowed.
	      (run-hooks 'internal-hook)
	      (run-hooks 'gnus-article-prepare-hook)
	      ;; Decode MIME message.
	      (if (and gnus-show-mime
		       (gnus-fetch-field "Mime-Version"))
		  (funcall gnus-show-mime-method))
	      ;; Perform the article display hooks.
	      (let ((buffer-read-only nil))
		(run-hooks 'gnus-article-display-hook))
	      ;; Do page break.
	      (goto-char (point-min))
	      (and gnus-break-pages (gnus-narrow-to-page))
	      (gnus-set-mode-line 'article)
	      (gnus-configure-windows 'article)
	      (goto-char 1)
	      (set-window-start 
	       (get-buffer-window gnus-article-buffer) (point-min))
	      (if bookmark
		  (progn
		    (message "Moved to bookmark")
		    (search-forward "\n\n" nil t)
		    (forward-line bookmark)))
	      t)))))))

(defun gnus-article-show-all-headers ()
  "Show all article headers in article mode buffer."
  (save-excursion 
    (setq gnus-have-all-headers t)
    (gnus-article-setup-buffer)
    (set-buffer gnus-article-buffer)
    (let ((buffer-read-only nil))
      (remove-text-properties 1 (point-max) '(invisible t)))))

(defun gnus-article-hide-headers-if-wanted ()
  "Hide unwanted headers if `gnus-have-all-headers' is nil.
Provided for backwards compatability."
  (or gnus-have-all-headers
      (gnus-article-hide-headers)))

(defun gnus-article-hide-headers (&optional delete)
  "Hide unwanted headers and possibly sort them as well."
  (interactive "P")
  (save-excursion
    (set-buffer gnus-article-buffer)
    (save-restriction
      (let ((sorted gnus-sorted-header-list)
	    (buffer-read-only nil)
	    want want-list beg want-l)
	;; First we narrow to just the headers.
	(widen)
	(goto-char 1)
	;; Hide any "From " lines at the beginning of (mail) articles. 
	(while (looking-at rmail-unix-mail-delimiter)
	  (forward-line 1))
	(if (/= (point) 1) 
	    (add-text-properties 1 (point) '(invisible t)))
	;; Then treat the rest of the header lines.
	(narrow-to-region 
	 (point) 
	 (progn (search-forward "\n\n" nil t) (forward-line -1) (point)))
	;; Then we use the two regular expressions
	;; `gnus-ignored-headers' and `gnus-visible-headers' to
	;; select which header lines is to remain visible in the
	;; article buffer.
	(goto-char 1)
	(while (re-search-forward "^[^ \t]*:" nil t)
	  (beginning-of-line)
	  ;; We add the headers we want to keep to a list and delete
	  ;; them from the buffer.
	  (if (or (and (stringp gnus-visible-headers)
		       (looking-at gnus-visible-headers))
		  (and (not (stringp gnus-visible-headers))
		       (stringp gnus-ignored-headers)
		       (not (looking-at gnus-ignored-headers))))
	      (progn
		(setq beg (point))
		(forward-line 1)
		;; Be sure to get multi-line headers...
		(re-search-forward "^[^ \t]*:" nil t)
		(beginning-of-line)
		(setq want-list 
		      (cons (buffer-substring beg (point)) want-list))
		(delete-region beg (point))
		(goto-char beg))
	    (forward-line 1)))
	;; Next we perform the sorting by looking at
	;; `gnus-sorted-header-list'. 
	(goto-char 1)
	(while (and sorted want-list)
	  (setq want-l want-list)
	  (while (and want-l
		      (not (string-match (car sorted) (car want-l))))
	    (setq want-l (cdr want-l)))
	  (if want-l 
	      (progn
		(insert (car want-l))
		(setq want-list (delq (car want-l) want-list))))
	  (setq sorted (cdr sorted)))
	;; Any headers that were not matched by the sorted list we
	;; just tack on the end of the visible header list.
	(while want-list
	  (insert (car want-list))
	  (setq want-list (cdr want-list)))
	;; And finally we make the unwanted headers invisible.
	(if delete
	    (delete-region (point) (point-max))
	  ;; Suggested by Sudish Joseph <joseph@cis.ohio-state.edu>.
	  (add-text-properties (point) (point-max) '(invisible t)))))))

(defun gnus-article-hide-signature ()
  "Hides the signature in an article.
It does this by hiding everyting after \"^-- *$\", which is what all
signatures should be preceded by. Note that this may mean that parts
of an article may disappear if the article has such a line in the
middle of the text."
  (interactive)
  (save-excursion
    (set-buffer gnus-article-buffer)
    (let ((buffer-read-only nil))
      (goto-char (point-max))
      (if (re-search-backward "^-- *$" nil t)
	  (progn
	    (add-text-properties (point) (point-max) '(invisible t)))))))

(defun gnus-article-hide-citation ()
  "Hide all cited text.
This function uses the famous, extremely intelligent \"shoot in foot\"
algorithm - which is simply deleting all lines that start with
\">\". Your mileage may vary. If you come up with anything better,
please do mail it to me."
  (interactive)
  (save-excursion
    (set-buffer gnus-article-buffer)
    (let ((buffer-read-only nil))
      (goto-char 1)
      (search-forward "\n\n" nil t)
      (while (not (eobp))
	(if (looking-at ">")
	    (add-text-properties 
	     (point) (save-excursion (forward-line 1) (point))
	     '(invisible t)))
	(forward-line 1)))))

;; Written by Per Abrahamsen <amanda@iesd.auc.dk>.
(defun gnus-article-treat-overstrike ()
  ;; Prepare article for overstrike commands.
  (interactive)
  (save-excursion
    (set-buffer gnus-article-buffer)
    (let ((buffer-read-only nil))
      (while (search-forward "\b" nil t)
	(let ((next (following-char))
	      (previous (char-after (- (point) 2))))
	  (cond ((eq next previous)
		 (delete-region (- (point) 2) (point))
		 (put-text-property (point) (1+ (point))
				    'face 'bold))
		((eq next ?_)
		 (delete-region (1- (point)) (1+ (point)))
		 (put-text-property (1- (point)) (point)
				    'face 'underline))
		((eq previous ?_)
		 (delete-region (- (point) 2) (point))
		 (put-text-property (point) (1+ (point))
				    'face 'underline))))))))

(defun gnus-article-word-wrap ()
  "Format too long lines."
  (interactive)
  (save-excursion
    (set-buffer gnus-article-buffer)
    (let ((buffer-read-only nil))
      (goto-char 1)
      (search-forward "\n\n" nil t)
      (end-of-line 1)
      (let ((paragraph-start "^\\W"))
	(while (not (eobp))
	  (and (>= (current-column) (window-width))
	       (/= (preceding-char) ?:)
	       (fill-paragraph nil))
	  (end-of-line 2))))))

(defun gnus-article-remove-cr ()
  (interactive)
  (save-excursion
    (let ((buffer-read-only nil))
      (set-buffer gnus-article-buffer)
      (while (search-forward "\r" nil t)
	(replace-match "")))))

(defun gnus-article-de-quoted-unreadable ()
  (interactive)
  (save-excursion
    (save-restriction
      (set-buffer gnus-article-buffer)
      (let ((buffer-read-only nil))
	(widen)
	(goto-char (point-min))
	(while (re-search-forward "=[0-9A-F][0-9A-F]" nil t)
	  (replace-match 
	   (char-to-string 
	    (+
	     (* 16 (gnus-hex-char-to-integer 
		    (char-after (1+ (match-beginning 0)))))
	     (gnus-hex-char-to-integer
	      (char-after (1- (match-end 0))))))))))))

;; Taken from hexl.el.
(defun gnus-hex-char-to-integer (character)
  "Take a char and return its value as if it was a hex digit."
  (if (and (>= character ?0) (<= character ?9))
      (- character ?0)
    (let ((ch (logior character 32)))
      (if (and (>= ch ?a) (<= ch ?f))
	  (- ch (- ?a 10))
	(error (format "Invalid hex digit `%c'." ch))))))

;; Article savers.

(defun gnus-output-to-rmail (file-name)
  "Append the current article to an Rmail file named FILE-NAME."
  (require 'rmail)
  ;; Most of these codes are borrowed from rmailout.el.
  (setq file-name (expand-file-name file-name))
  (setq rmail-default-rmail-file file-name)
  (let ((artbuf (current-buffer))
	(tmpbuf (get-buffer-create " *Gnus-output*")))
    (save-excursion
      (or (get-file-buffer file-name)
	  (file-exists-p file-name)
	  (if (gnus-yes-or-no-p
	       (concat "\"" file-name "\" does not exist, create it? "))
	      (let ((file-buffer (create-file-buffer file-name)))
		(save-excursion
		  (set-buffer file-buffer)
		  (rmail-insert-rmail-file-header)
		  (let ((require-final-newline nil))
		    (write-region (point-min) (point-max) file-name t 1)))
		(kill-buffer file-buffer))
	    (error "Output file does not exist")))
      (set-buffer tmpbuf)
      (buffer-disable-undo (current-buffer))
      (erase-buffer)
      (insert-buffer-substring artbuf)
      (gnus-convert-article-to-rmail)
      ;; Decide whether to append to a file or to an Emacs buffer.
      (let ((outbuf (get-file-buffer file-name)))
	(if (not outbuf)
	    (append-to-file (point-min) (point-max) file-name)
	  ;; File has been visited, in buffer OUTBUF.
	  (set-buffer outbuf)
	  (let ((buffer-read-only nil)
		(msg (and (boundp 'rmail-current-message)
			  rmail-current-message)))
	    ;; If MSG is non-nil, buffer is in RMAIL mode.
	    (if msg
		(progn (widen)
		       (narrow-to-region (point-max) (point-max))))
	    (insert-buffer-substring tmpbuf)
	    (if msg
		(progn
		  (goto-char (point-min))
		  (widen)
		  (search-backward "\^_")
		  (narrow-to-region (point) (point-max))
		  (goto-char (1+ (point-min)))
		  (rmail-count-new-messages t)
		  (rmail-show-message msg))))))
      )
    (kill-buffer tmpbuf)
    ))

(defun gnus-output-to-file (file-name)
  "Append the current article to a file named FILE-NAME."
  (setq file-name (expand-file-name file-name))
  (let ((artbuf (current-buffer))
	(tmpbuf (get-buffer-create " *Gnus-output*")))
    (save-excursion
      (set-buffer tmpbuf)
      (buffer-disable-undo (current-buffer))
      (erase-buffer)
      (insert-buffer-substring artbuf)
      ;; Append newline at end of the buffer as separator, and then
      ;; save it to file.
      (goto-char (point-max))
      (insert "\n")
      (append-to-file (point-min) (point-max) file-name))
    (kill-buffer tmpbuf)
    ))

(defun gnus-convert-article-to-rmail ()
  "Convert article in current buffer to Rmail message format."
  (let ((buffer-read-only nil))
    ;; Convert article directly into Babyl format.
    ;; Suggested by Rob Austein <sra@lcs.mit.edu>
    (goto-char (point-min))
    (insert "\^L\n0, unseen,,\n*** EOOH ***\n")
    (while (search-forward "\n\^_" nil t) ;single char
      (replace-match "\n^_"))		;2 chars: "^" and "_"
    (goto-char (point-max))
    (insert "\^_")))

(defun gnus-narrow-to-page (&optional arg)
  "Make text outside current page invisible except for page delimiter.
A numeric arg specifies to move forward or backward by that many pages,
thus showing a page other than the one point was originally in."
  (interactive "P")
  (setq arg (if arg (prefix-numeric-value arg) 0))
  (save-excursion
    (forward-page -1)			;Beginning of current page.
    (widen)
    (if (> arg 0)
	(forward-page arg)
      (if (< arg 0)
	  (forward-page (1- arg))))
    ;; Find the end of the page.
    (forward-page)
    ;; If we stopped due to end of buffer, stay there.
    ;; If we stopped after a page delimiter, put end of restriction
    ;; at the beginning of that line.
    ;; These are commented out.
    ;;    (if (save-excursion (beginning-of-line)
    ;;			(looking-at page-delimiter))
    ;;	(beginning-of-line))
    (narrow-to-region (point)
		      (progn
			;; Find the top of the page.
			(forward-page -1)
			;; If we found beginning of buffer, stay there.
			;; If extra text follows page delimiter on same line,
			;; include it.
			;; Otherwise, show text starting with following line.
			(if (and (eolp) (not (bobp)))
			    (forward-line 1))
			(point)))
    ))

(defun gnus-gmt-to-local ()
  "Rewrite Date: field described in GMT to local in current buffer.
The variable gnus-local-timezone is used for local time zone.
Intended to be used with gnus-article-prepare-hook."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (narrow-to-region (point-min)
			(progn (search-forward "\n\n" nil 'move) (point)))
      (goto-char (point-min))
      (if (re-search-forward "^Date:[ \t]\\(.*\\)$" nil t)
	  (let ((buffer-read-only nil)
		(date (buffer-substring (match-beginning 1) (match-end 1))))
	    (delete-region (match-beginning 1) (match-end 1))
	    (insert
	     (timezone-make-date-arpa-standard date nil gnus-local-timezone))
	    ))
      )))


;; Article mode commands

(defun gnus-article-next-page (lines)
  "Show next page of current article.
If end of article, return non-nil. Otherwise return nil.
Argument LINES specifies lines to be scrolled up."
  (interactive "P")
  (move-to-window-line -1)
  ;; Fixed by enami@ptgd.sony.co.jp (enami tsugutomo)
  (if (save-excursion
	(end-of-line)
	(and (pos-visible-in-window-p)	;Not continuation line.
	     (eobp)))
      ;; Nothing in this page.
      (if (or (not gnus-break-pages)
	      (save-excursion
		(save-restriction
		  (widen) (forward-line 1) (eobp)))) ;Real end-of-buffer?
	  t				;Nothing more.
	(gnus-narrow-to-page 1)		;Go to next page.
	nil
	)
    ;; More in this page.
    (condition-case ()
	(scroll-up lines)
      (end-of-buffer
       ;; Long lines may cause an end-of-buffer error.
       (goto-char (point-max))))
    nil
    ))

(defun gnus-article-prev-page (lines)
  "Show previous page of current article.
Argument LINES specifies lines to be scrolled down."
  (interactive "P")
  (move-to-window-line 0)
  (if (and gnus-break-pages
	   (bobp)
	   (not (save-restriction (widen) (bobp)))) ;Real beginning-of-buffer?
      (progn
	(gnus-narrow-to-page -1) ;Go to previous page.
	(goto-char (point-max))
	(recenter -1))
    (scroll-down lines)))

(defun gnus-article-refer-article ()
  "Read article specified by message-id around point."
  (interactive)
  (search-forward ">" nil t)	;Move point to end of "<....>".
  (if (re-search-backward "\\(<[^<> \t\n]+>\\)" nil t)
      (let ((message-id
	     (buffer-substring (match-beginning 1) (match-end 1))))
	(set-buffer gnus-summary-buffer)
	(gnus-summary-refer-article message-id))
    (error "No references around point")))

(defun gnus-article-mail (yank)
  "Send a reply to the address near point.
If YANK is non-nil, include the original article."
  (interactive "P")
  (let ((address 
	 (buffer-substring
	  (save-excursion (re-search-backward "[ \t\n]" nil t) (1+ (point)))
	  (save-excursion (re-search-forward "[ \t\n]" nil t) (1- (point))))))
    (and address
	 (progn
	   (switch-to-buffer gnus-summary-buffer)
	   (funcall gnus-mail-reply-method yank address)))))

(defun gnus-article-mail-with-original ()
  "Send a reply to the address near point and include the original article."
  (interactive)
  (gnus-article-mail 'yank))

(defun gnus-article-show-summary ()
  "Reconfigure windows to show summary buffer."
  (interactive)
  (gnus-configure-windows 'article)
  (pop-to-buffer gnus-summary-buffer)
  (gnus-summary-goto-subject gnus-current-article))

(defun gnus-article-describe-briefly ()
  "Describe article mode commands briefly."
  (interactive)
  (message
   (substitute-command-keys "\\<gnus-article-mode-map>\\[gnus-article-next-page]:Next page  \\[gnus-article-prev-page]:Prev page  \\[gnus-article-show-summary]:Show summary  \\[gnus-info-find-node]:Run Info  \\[gnus-article-describe-briefly]:This help")))

(defun gnus-article-summary-command ()
  "Execute the last keystroke in the summary buffer."
  (interactive)
  (message "                                                                              ")
  (let ((obuf (current-buffer))
	(owin (current-window-configuration)))
    (switch-to-buffer gnus-summary-buffer 'norecord)
    (execute-kbd-macro (this-command-keys))
    (set-buffer obuf)
    (let ((npoint (point)))
      (set-window-configuration owin)
      (set-window-start (get-buffer-window (current-buffer)) (point)))))

;; caesar-region written by phr@prep.ai.mit.edu  Nov 86
;; Modified by tower@prep Nov 86
;; Modified by umerin@flab.flab.Fujitsu.JUNET for ROT47.

(defun gnus-caesar-region (&optional n)
  "Caesar rotation of region by N, default 13, for decrypting netnews.
ROT47 will be performed for Japanese text in any case."
  (interactive (if current-prefix-arg	; Was there a prefix arg?
		   (list (prefix-numeric-value current-prefix-arg))
		 (list nil)))
  (cond ((not (numberp n)) (setq n 13))
	(t (setq n (mod n 26))))	;canonicalize N
  (if (not (zerop n))		; no action needed for a rot of 0
      (progn
	(if (or (not (boundp 'caesar-translate-table))
		(not caesar-translate-table)
		(/= (aref caesar-translate-table ?a) (+ ?a n)))
	    (let ((i 0) (lower "abcdefghijklmnopqrstuvwxyz") upper)
	      (message "Building caesar-translate-table...")
	      (setq caesar-translate-table (make-vector 256 0))
	      (while (< i 256)
		(aset caesar-translate-table i i)
		(setq i (1+ i)))
	      (setq lower (concat lower lower) upper (upcase lower) i 0)
	      (while (< i 26)
		(aset caesar-translate-table (+ ?a i) (aref lower (+ i n)))
		(aset caesar-translate-table (+ ?A i) (aref upper (+ i n)))
		(setq i (1+ i)))
	      ;; ROT47 for Japanese text.
	      ;; Thanks to ichikawa@flab.fujitsu.junet.
	      (setq i 161)
	      (let ((t1 (logior ?O 128))
		    (t2 (logior ?! 128))
		    (t3 (logior ?~ 128)))
		(while (< i 256)
		  (aset caesar-translate-table i
			(let ((v (aref caesar-translate-table i)))
			  (if (<= v t1) (if (< v t2) v (+ v 47))
			    (if (<= v t3) (- v 47) v))))
		  (setq i (1+ i))))
	      (message "Building caesar-translate-table... done")))
	(let ((from (region-beginning))
	      (to (region-end))
	      (i 0) str len)
	  (setq str (buffer-substring from to))
	  (setq len (length str))
	  (while (< i len)
	    (aset str i (aref caesar-translate-table (aref str i)))
	    (setq i (1+ i)))
	  (goto-char from)
	  (delete-region from to)
	  (insert str)))))


;;;
;;; Gnus Score File Mode
;;;

(if gnus-kill-file-mode-map
    nil
  (setq gnus-kill-file-mode-map (copy-keymap emacs-lisp-mode-map))
  (define-key gnus-kill-file-mode-map "\C-c\C-x"
    'gnus-kill-file-set-expunge-below)
  (define-key gnus-kill-file-mode-map "\C-c@"
    'gnus-kill-file-set-mark-below)
  (define-key gnus-kill-file-mode-map "\C-c\C-k\C-s"
    'gnus-kill-file-temporarily-lower-by-subject)
  (define-key gnus-kill-file-mode-map "\C-c\C-k\C-a"
    'gnus-kill-file-temporarily-lower-by-author)
  (define-key gnus-kill-file-mode-map "\C-c\C-k\C-x"
    'gnus-kill-file-temporarily-lower-by-xref)
  (define-key gnus-kill-file-mode-map "\C-c\C-ks"
    'gnus-kill-file-lower-by-subject)
  (define-key gnus-kill-file-mode-map "\C-c\C-ka"
    'gnus-kill-file-lower-by-author)
  (define-key gnus-kill-file-mode-map "\C-c\C-kt"
    'gnus-kill-file-lower-by-thread)
  (define-key gnus-kill-file-mode-map "\C-c\C-kx"
    'gnus-kill-file-lower-by-xref)
  (define-key gnus-kill-file-mode-map "\C-c\C-kf"
    'gnus-kill-file-lower-followups-to-author)
  (define-key gnus-kill-file-mode-map "\C-c\C-i\C-s"
    'gnus-kill-file-temporarily-raise-by-subject)
  (define-key gnus-kill-file-mode-map "\C-c\C-i\C-a"
    'gnus-kill-file-temporarily-raise-by-author)
  (define-key gnus-kill-file-mode-map "\C-c\C-i\C-t"
    'gnus-kill-file-temporarily-raise-by-thread)
  (define-key gnus-kill-file-mode-map "\C-c\C-i\C-x"
    'gnus-kill-file-temporarily-raise-by-xref)
  (define-key gnus-kill-file-mode-map "\C-c\C-is"
    'gnus-kill-file-raise-by-subject)
  (define-key gnus-kill-file-mode-map "\C-c\C-ia"
    'gnus-kill-file-raise-by-author)
  (define-key gnus-kill-file-mode-map "\C-c\C-ix"
    'gnus-kill-file-raise-by-xref)
  (define-key gnus-kill-file-mode-map "\C-c\C-if"
    'gnus-kill-file-raise-followups-to-author)
  (define-key gnus-kill-file-mode-map "\C-c\C-a" 'gnus-kill-file-apply-buffer)
  (define-key gnus-kill-file-mode-map "\C-c\C-e" 'gnus-kill-file-apply-last-sexp)
  (define-key gnus-kill-file-mode-map "\C-c\C-c" 'gnus-kill-file-exit)
  (define-key gnus-kill-file-mode-map "\C-c\C-i" 'gnus-info-find-node))

(defun gnus-kill-file-mode ()
  "Major mode for editing score files.

In addition to Emacs-Lisp mode, the following commands are available:

\\[gnus-kill-file-set-expunge-below]	Automatically expunge articles below LEVEL.
\\[gnus-kill-file-set-mark-below]	Automatically mark articles below LEVEL.
\\[gnus-kill-file-temporarily-lower-by-author]	Insert temporary lower command for current author.
\\[gnus-kill-file-temporarily-lower-by-thread]	Insert temporary lower command for current thread.
\\[gnus-kill-file-temporarily-lower-by-xref]		Insert temporary lower command for current cross-posting.
\\[gnus-kill-file-lower-by-subject]	Insert permanent lower command for current subject.
\\[gnus-kill-file-lower-by-author]	Insert permanent lower command for current author.
\\[gnus-kill-file-lower-followups-to-author]	Insert permanent lower command for followups to the current author.
\\[gnus-kill-file-lower-by-xref]		Insert permanent lower command for current cross-posting.
\\[gnus-kill-file-temporarily-raise-by-subject]	Insert temporary raise command for current subject.
\\[gnus-kill-file-temporarily-raise-by-author]	Insert temporary raise command for current author.
\\[gnus-kill-file-temporarily-raise-by-thread]	Insert temporary raise command for current thread.
\\[gnus-kill-file-temporarily-raise-by-xref]		Insert temporary raise command for current cross-posting.
\\[gnus-kill-file-raise-by-subject]	Insert permanent raise command for current subject.
\\[gnus-kill-file-raise-by-author]	Insert permanent raise command for current author.
\\[gnus-kill-file-raise-followups-to-author]	Insert permanent raise command for followups to the current author.
\\[gnus-kill-file-raise-by-xref]		Insert permanent raise command for current cross-posting.
\\[gnus-kill-file-apply-buffer]	Apply current buffer to selected newsgroup.
\\[gnus-kill-file-apply-last-sexp]	Apply sexp before point to selected newsgroup.
\\[gnus-kill-file-exit]	Save file and exit editing score file.
\\[gnus-info-find-node]	Read Info about score files.

  A score file contains Lisp expressions to be applied to a selected
newsgroup.  The purpose is to mark articles as read on the basis of
some set of regexps.  A global score file is applied to every
newsgroup, and a local score file is applied to a specified newsgroup.
Since a global score file is applied to every newsgroup, for better
performance use a local one.

  A score file can contain any kind of Emacs Lisp expressions expected
to be evaluated in the summary buffer.  Writing Lisp programs for this
purpose is not so easy because the internal working of Gnus must be
well-known.  For this reason, Gnus provides a general function which
does this easily for non-Lisp programmers.

  The `gnus-kill' function executes commands available in summary mode
by their key sequences. `gnus-kill' should be called with FIELD,
REGEXP and optional COMMAND and ALL.  FIELD is a string representing
the header field or an empty string.  If FIELD is an empty string, the
entire article body is searched for.  REGEXP is a string which is
compared with FIELD value. COMMAND is a string representing a valid
key sequence in summary mode or Lisp expression. COMMAND defaults to
'(gnus-summary-mark-as-read nil \"X\").  Make sure that COMMAND is
executed in the summary buffer.  If the second optional argument ALL
is non-nil, the COMMAND is applied to articles which are already
marked as read or unread.  Articles which are marked are skipped over
by default.

  For example, if you want to mark articles of which subjects contain
the string `AI' as read, a possible score file may look like:

	(gnus-kill \"Subject\" \"AI\")

  If you want to mark articles with `D' instead of `X', you can use
the following expression:

	(gnus-kill \"Subject\" \"AI\" \"d\")

In this example it is assumed that the command
`gnus-summary-mark-as-read-forward' is assigned to `d' in summary mode.

  It is possible to remove unnecessary headers which are marked with
`X' in a score file as follows:

	(gnus-expunge \"X\")

  If the summary buffer is empty after applying score files, Gnus will
exit the selected newsgroup normally.  If headers which are marked
with `D' are deleted in a score file, it is impossible to read articles
which are marked as read in the previous Gnus sessions.  Marks other
than `D' should be used for articles which should really be deleted.

Entry to this mode calls emacs-lisp-mode-hook and
gnus-kill-file-mode-hook with no arguments, if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (use-local-map gnus-kill-file-mode-map)
  (set-syntax-table emacs-lisp-mode-syntax-table)
  (setq major-mode 'gnus-kill-file-mode)
  (setq mode-name "score-file")
  (lisp-mode-variables nil)
  (run-hooks 'emacs-lisp-mode-hook 'gnus-kill-file-mode-hook))

(defun gnus-kill-file-edit-file (newsgroup)
  "Begin editing a score file for NEWSGROUP.
If NEWSGROUP is nil, the global score file is selected."
  (interactive "sNewsgroup: ")
  (let ((file (gnus-newsgroup-kill-file newsgroup)))
    (gnus-make-directory (file-name-directory file))
    ;; Save current window configuration if this is first invocation.
    (or (and (get-file-buffer file)
	     (get-buffer-window (get-file-buffer file)))
	(setq gnus-winconf-kill-file (current-window-configuration)))
    ;; Hack windows.
    (let ((buffer (find-file-noselect file)))
      (cond ((get-buffer-window buffer)
	     (pop-to-buffer buffer))
	    ((eq major-mode 'gnus-group-mode)
	     (gnus-configure-windows '(1 0 0)) ;Take all windows.
	     (pop-to-buffer gnus-group-buffer)
	     ;; Fix by sachs@SLINKY.CS.NYU.EDU (Jay Sachs).
	     (let ((gnus-summary-buffer buffer))
	       (gnus-configure-windows '(1 1 0))) ;Split into two.
	     (pop-to-buffer buffer))
	    ((eq major-mode 'gnus-summary-mode)
	     (gnus-configure-windows 'article)
	     (pop-to-buffer gnus-article-buffer)
	     (bury-buffer gnus-article-buffer)
	     (switch-to-buffer buffer))
	    (t				;No good rules.
	     (find-file-other-window file))
	    ))
    (gnus-kill-file-mode)
    ))

;; Fix by Sudish Joseph <joseph@cis.ohio-state.edu>.
(defun gnus-kill-set-kill-buffer ()
  (let* ((file (gnus-newsgroup-kill-file gnus-newsgroup-name))
	 (buffer (find-file-noselect file)))
    (set-buffer buffer)
    (gnus-kill-file-mode)
    (bury-buffer buffer)))

(defun gnus-kill-save-kill-buffer ()
  (save-excursion
    (let ((file (gnus-newsgroup-kill-file gnus-newsgroup-name)))
      (if (get-file-buffer file)
	  (progn
	    (set-buffer (get-file-buffer file))
	    (and (buffer-modified-p) (save-buffer))
	    (kill-buffer (current-buffer)))))))

(defun gnus-article-fetch-field (field)
  (save-excursion
    (set-buffer gnus-article-buffer)
    (save-restriction
      (widen)
      (goto-char 1)
      (narrow-to-region 1 (save-excursion 
			    (search-forward "\n\n" nil t) (point)))
      (goto-char 1)
      (prog1
	  (mail-fetch-field field)
	(widen)))))

(defun gnus-kill-file-enter-kill (field regexp level date edit)
  ;; Enter score file entry.
  ;; FIELD: String containing the name of the header field to score.
  ;; REGEXP: The string to score.
  ;; LEVEL: How much to raise the score by.
  ;; DATE: A date string for expire score or nil for permanent kills.
  ;; EDIT: Allow the user to edit REGEXP iff non-nil.
  (save-excursion
    (gnus-kill-set-kill-buffer)
    (goto-char (point-min))
    (let ((regexp 
	   (if edit (read-string 
		     (format "Add %d to articles with %s matching: " 
			     level (downcase field))
		     regexp)
	     regexp))
 	  entry string kill beg)
      (setq entry (if date (cons regexp date) regexp)
 	    string (format "(gnus-raise %S (quote %S) %S)\n"
 			   field entry level))
      (while (and (setq beg (point))
 		  (condition-case nil
 		      (setq kill (read (current-buffer)))
 		    (error nil))
 		  (or (not (eq (nth 0 kill) 'gnus-raise))
 		      (not (string= (downcase (nth 1 kill)) (downcase field)))
 		      (not (eq (nth 3 kill) level))))
	(setq kill nil))
      (if (not kill)
  	  (progn
  	    (goto-char (point-min))
 	    (insert string))
  	(let ((list (nth 2 kill)))
 	  (if (and (listp list) (eq 'quote (car list)))
 	      (setq list (car (cdr list))))
  	  (setcar 
  	   (nthcdr 2 kill) 
  	   (if (and (listp list) (listp (cdr list)))
 	       (list 'quote (cons entry list))
 	     (list 'quote (list entry list)))))
  	(delete-region beg (point))
 	(insert (gnus-pp-gnus-kill kill)))
      (gnus-kill-file-apply-string string))
    ;; Added by Sudish Joseph <joseph@cis.ohio-state.edu>.
    (or edit 
 	(message "Added kill file entry %s: %s" (downcase field) regexp))))
    
(defun gnus-kill-file-set-variable (symbol value)
   ;; Set SYMBOL to VALUE in the score file.
   (save-excursion
     (gnus-kill-set-kill-buffer)
     (goto-char (point-min))
     (let ((string (format "(setq %S %S)\n" symbol value))
	   kill beg)
       (while (and (setq beg (point))
		   (condition-case nil
		       (setq kill (read (current-buffer)))
		     (error nil))
		   (or (not (eq (nth 0 kill) 'setq))
		       (not (eq (nth 1 kill) symbol))))
	 (setq kill nil))
       (if (not kill)
	   (progn
	     (goto-char (point-min))
	     (insert string))
	 (delete-region beg (point))
	 (insert string)))))
    
(defun gnus-kill-file-set-expunge-below (level)
   "Automatically expunge articles with score below LEVEL."
   (interactive "P")
   (setq level (if level
 		  (prefix-numeric-value level)
 		gnus-summary-default-score))
   (if (eq major-mode 'gnus-summary-mode)
       (progn
	 (gnus-score-set 'expunge level)
	 (gnus-score-set 'touched t))
     (gnus-kill-file-set-variable 'expunge-below level))
   (message "Set expunge below level to %d." level))

 (defun gnus-kill-file-set-mark-below (level)
   "Automatically mark articles with score below LEVEL as read."
   (interactive "P")
   (setq level (if level
		   (prefix-numeric-value level)
		 gnus-summary-default-score))
   (if (eq major-mode 'gnus-summary-mode)
       (progn
	 (gnus-score-set 'mark level)
	 (gnus-score-set 'touched t)
	 (gnus-summary-set-mark-below level))
     (gnus-kill-file-set-variable 'mark-below level))
   (message "Set mark below level to %d." level))
 
 (defun gnus-kill-file-temporarily-raise-by-subject (level &optional header)
   "Temporarily raise score by LEVEL for current subject.
 See `gnus-kill-expiry-days'."
   (interactive "p")
   (gnus-kill-file-raise-by-subject level header (current-time-string)))
  
 (defun gnus-kill-file-temporarily-raise-by-author (level &optional header)
   "Temporarily raise score by LEVEL for current author.
 See `gnus-kill-expiry-days'."
   (interactive "p")
   (gnus-kill-file-raise-by-author level header (current-time-string)))
  
 (defun gnus-kill-file-temporarily-raise-by-thread (level &optional header)
   "Temporarily raise score by LEVEL for current thread.
 See `gnus-kill-expiry-days'."
   (interactive "p")
   (gnus-kill-file-enter-kill 
    "References"
    (regexp-quote (header-id (or header gnus-current-headers)))
    level
    (current-time-string)
    nil))
  
 (defun gnus-kill-file-temporarily-raise-by-xref (level &optional header)
   "Insert temporary score commands for articles that have been crossposted.
 By default use the current crossposted groups.
 See `gnus-kill-expiry-days'."
   (interactive "p")
   (gnus-kill-file-raise-by-xref level header (current-time-string)))
  
 (defun gnus-kill-file-raise-by-subject (level &optional header date)
   "Raise score by LEVEL for current subject."
   (interactive "p")
   (gnus-kill-file-enter-kill
    "Subject"
    (regexp-quote 
     (gnus-simplify-subject 
      (header-subject (or header gnus-current-headers))))
    level
    date
    t))
  
 (defun gnus-kill-file-raise-by-author (level &optional header date)
   "Raise score by LEVEL for current author."
   (interactive "p")
   (gnus-kill-file-enter-kill
    "From"
    (regexp-quote (header-from (or header gnus-current-headers)))
    level
    date
    t))
 
 (defun gnus-kill-file-raise-by-xref (level &optional header date)
   "Raise score by LEVEL for articles that have been crossposted.
 By default use the current crossposted groups."
   (interactive "p")
   (let ((xref (header-xref (or header gnus-current-headers)))
	 (start 0)
	 group)
     (if xref
	 (while (string-match " \\([^ \t]+\\):" xref start)
	   (setq start (match-end 0))
	   (if (not (string= 
		     (setq group 
			   (substring xref (match-beginning 1) (match-end 1)))
		     gnus-newsgroup-name))
	       (gnus-kill-file-enter-kill 
		"Xref"
		(concat " " (regexp-quote group) ":")
		level
		date
		t))))))

(defun gnus-kill-file-raise-followups-to-author
  (level &optional header)
  "Raise score for all followups to the current author."
  (interactive)
  (let ((name (header-from (or header gnus-current-headers)))
	(string))
    (save-excursion
      (gnus-kill-set-kill-buffer)
      (goto-char (point-min))
      (setq name (read-string (concat "Add " level
				      " to followup articles to: ")
			      (regexp-quote name)))
      (setq string
 	    (format "(gnus-kill %S %S '(gnus-summary-temporarily-raise-by-thread %S))\n"
		    "From" name level))
      (insert string)
      (gnus-kill-file-apply-string string))
    (message "Added permanent score file entry for followups to %s." name)))

(defun gnus-kill-file-temporarily-lower-by-subject (level &optional header)
  "Temporarily lower score by LEVEL for current subject.
See `gnus-kill-expiry-days'."
  (interactive "p")
  (gnus-kill-file-lower-by-subject level header (current-time-string)))

(defun gnus-kill-file-temporarily-lower-by-author (level &optional header)
  "Temporarily lower score by LEVEL for current author.
See `gnus-kill-expiry-days'."
  (interactive "p")
  (gnus-kill-file-lower-by-author level header (current-time-string)))

(defun gnus-kill-file-temporarily-lower-by-thread (level &optional header)
  "Temporarily lower score by LEVEL for current thread.
See `gnus-kill-expiry-days'."
  (interactive "p")
  (gnus-kill-file-temporarily-raise-by-thread (- (gnus-score-default level)) header))

(defun gnus-kill-file-temporarily-lower-by-xref (level &optional header)
  "Insert temporary score commands for articles that have been crossposted.
By default use the current crossposted groups.
See `gnus-kill-expiry-days'."
  (interactive "p")
  (gnus-kill-file-lower-by-xref level header (current-time-string)))

(defun gnus-kill-file-lower-by-subject (level &optional header date)
    "Lower score by LEVEL for current subject."
  (interactive "p")
  (gnus-kill-file-raise-by-subject (- (gnus-score-default level)) header date))

(defun gnus-kill-file-lower-by-author (level &optional header date)
  "Lower score by LEVEL for current author."
  (interactive "p")
  (gnus-kill-file-raise-by-author (- (gnus-score-default level)) header date))

(defun gnus-kill-file-lower-by-xref (level &optional header date)
  "Lower score by LEVEL for articles that have been crossposted.
By default use the current crossposted groups."
  (gnus-kill-file-raise-by-xref (- (gnus-score-default level)) header date))

(defun gnus-kill-file-lower-followups-to-author
  (level &optional header)
  "Lower score for all followups to the current author."
  (interactive "p")
  (gnus-kill-file-raise-followups-to-author (- (gnus-score-default level)) header))

(defun gnus-kill-file-apply-buffer ()
  "Apply current buffer to current newsgroup."
  (interactive)
  (if (and gnus-current-kill-article
	   (get-buffer gnus-summary-buffer))
      ;; Assume newsgroup is selected.
      (gnus-kill-file-apply-string (buffer-string))
    (ding) (message "No newsgroup is selected.")))

(defun gnus-kill-file-apply-string (string)
  "Apply STRING to current newsgroup."
  (interactive)
  (let ((string (concat "(progn \n" string "\n)" )))
    (save-excursion
      (save-window-excursion
	(pop-to-buffer gnus-summary-buffer)
	(eval (car (read-from-string string)))))))

(defun gnus-kill-file-apply-last-sexp ()
  "Apply sexp before point in current buffer to current newsgroup."
  (interactive)
  (if (and gnus-current-kill-article
	   (get-buffer gnus-summary-buffer))
      ;; Assume newsgroup is selected.
      (let ((string
	     (buffer-substring
	      (save-excursion (forward-sexp -1) (point)) (point))))
	(save-excursion
	  (save-window-excursion
	    (pop-to-buffer gnus-summary-buffer)
	    (eval (car (read-from-string string))))))
    (ding) (message "No newsgroup is selected.")))

(defun gnus-kill-file-exit ()
  "Save a score file, then return to the previous buffer."
  (interactive)
  (save-buffer)
  (let ((killbuf (current-buffer)))
    ;; We don't want to return to article buffer.
    (and (get-buffer gnus-article-buffer)
	 (bury-buffer gnus-article-buffer))
    ;; Delete the KILL file windows.
    (delete-windows-on killbuf)
    ;; Restore last window configuration if available.
    (and gnus-winconf-kill-file
	 (set-window-configuration gnus-winconf-kill-file))
    (setq gnus-winconf-kill-file nil)
    ;; Kill the KILL file buffer.  Suggested by tale@pawl.rpi.edu.
    (kill-buffer killbuf)))

;; Basic ideas by emv@math.lsa.umich.edu (Edward Vielmetti)

(defalias 'gnus-batch-kill 'gnus-batch-score)
;;;###autoload
(defun gnus-batch-score ()
  "Run batched scoring.
Usage: emacs -batch -l gnus -f gnus-batch-kill <newsgroups> ...
Newsgroups is a list of strings in Bnews format.  If you want to score
the comp hierarchy, you'd say \"comp.all\". If you would not like to
score the alt hierarchy, you'd say \"!alt.all\"."
  (interactive)
  (let* ((yes-and-no
	  (gnus-parse-n-options
	   (apply (function concat)
		  (mapcar (lambda (g) (concat g " "))
			  command-line-args-left))))
	 (gnus-expert-user t)
	 (nnmail-spool-file nil)
	 (gnus-use-dribble-file nil)
	 (yes (car yes-and-no))
	 (no (cdr yes-and-no))
	 group subscribed newsrc entry
	 ;; Disable verbose message.
	 gnus-novice-user gnus-large-newsgroup)
    ;; Eat all arguments.
    (setq command-line-args-left nil)
    ;; Start Gnus.
    (gnus)
    ;; Apply kills to specified newsgroups in command line arguments.
    (setq newsrc (cdr gnus-newsrc-assoc))
    (while newsrc
      (setq group (car (car newsrc)))
      (setq entry (gnus-gethash group gnus-newsrc-hashtb))
      (if (and (<= (nth 1 (car newsrc)) 5)
	       (and (car entry)
		    (or (eq (car entry) t)
			(not (zerop (car entry)))))
	       (if yes (string-match yes group) t)
	       (or (null no) (not (string-match no group))))
	  (progn
	    (gnus-summary-read-group group nil t)
	    (and (eq (current-buffer) (get-buffer gnus-summary-buffer))
		 (gnus-summary-exit))))
      (setq newsrc (cdr newsrc)))
    ;; Exit Emacs.
    (switch-to-buffer gnus-group-buffer)
    (gnus-group-save-newsrc)))

;; For score files

(defun gnus-Newsgroup-kill-file (newsgroup)
  "Return the name of a score file for NEWSGROUP.
If NEWSGROUP is nil, return the global score file instead."
  (cond ((or (null newsgroup)
	     (string-equal newsgroup ""))
	 ;; The global score file is placed at top of the directory.
	 (expand-file-name gnus-kill-file-name
			   (or gnus-kill-files-directory "~/News")))
	(gnus-use-long-file-name
	 ;; Append ".KILL" to capitalized newsgroup name.
	 (expand-file-name (concat (gnus-capitalize-newsgroup newsgroup)
				   "." gnus-kill-file-name)
			   (or gnus-kill-files-directory "~/News")))
	(t
	 ;; Place "KILL" under the hierarchical directory.
	 (expand-file-name (concat (gnus-newsgroup-directory-form newsgroup)
				   "/" gnus-kill-file-name)
			   (or gnus-kill-files-directory "~/News")))
	))

(defun gnus-newsgroup-kill-file (newsgroup)
  "Return the name of a score file name for NEWSGROUP.
If NEWSGROUP is nil, return the global score file name instead."
  (cond ((or (null newsgroup)
	     (string-equal newsgroup ""))
	 ;; The global KILL file is placed at top of the directory.
	 (expand-file-name gnus-kill-file-name
			   (or gnus-kill-files-directory "~/News")))
	(gnus-use-long-file-name
	 ;; Append ".KILL" to newsgroup name.
	 (expand-file-name (concat newsgroup "." gnus-kill-file-name)
			   (or gnus-kill-files-directory "~/News")))
	(t
	 ;; Place "KILL" under the hierarchical directory.
	 (expand-file-name (concat (gnus-newsgroup-directory-form newsgroup)
				   "/" gnus-kill-file-name)
			   (or gnus-kill-files-directory "~/News")))
	))


(defalias 'gnus-expunge 'gnus-summary-remove-lines-marked-with)

(defun gnus-apply-kill-file ()
  "Apply a score file to the current newsgroup.
Returns the number of articles marked as read."
  (let* ((kill-files (list (gnus-newsgroup-kill-file nil)
			   (gnus-newsgroup-kill-file gnus-newsgroup-name)))
	 (unreads (length gnus-newsgroup-unreads))
	 (gnus-summary-inhibit-highlight t)
	 (mark-below (or gnus-summary-mark-below gnus-summary-default-score))
	 (expunge-below gnus-summary-expunge-below)
	 form beg)
    (setq gnus-newsgroup-kill-headers nil)
    (or gnus-newsgroup-headers-hashtb-by-number
	(gnus-make-headers-hashtable-by-number))
    ;; If there are any previously scored articles, we remove these
    ;; from the `gnus-newsgroup-headers' list that the score functions
    ;; will see. This is probably pretty wasteful when it comes to
    ;; conses, but is, I think, faster than having to assq in every
    ;; single score funtion.
    (let ((files kill-files))
      (while files
	(if (file-exists-p (car files))
	    (let ((headers gnus-newsgroup-headers))
	      (if gnus-kill-killed
		  (setq gnus-newsgroup-kill-headers
			(mapcar (lambda (header) (header-number header))
				headers))
		(while headers
		  (or (gnus-member-of-range 
		       (header-number (car headers)) 
		       gnus-newsgroup-killed)
		      (setq gnus-newsgroup-kill-headers 
			    (cons (header-number (car headers))
				  gnus-newsgroup-kill-headers)))
		  (setq headers (cdr headers))))
	      (setq files nil))
	  (setq files (cdr files)))))
    (if gnus-newsgroup-kill-headers
	(save-excursion
	  (while kill-files
	    (if (file-exists-p (car kill-files))
		(progn
		  (message "Processing kill file %s..." (car kill-files))
		  (find-file (car kill-files))
		  (goto-char (point-min))
		  (while (progn
			   (setq beg (point))
			   (setq form (condition-case nil 
					  (read (current-buffer)) 
					(error nil))))
		    (if (or (eq (car form) 'gnus-kill)
			    (eq (car form) 'gnus-raise)
			    (eq (car form) 'gnus-lower))
			(progn
			  (delete-region beg (point))
			  (insert (or (eval form) "")))
		      (eval form)))
		  (and (buffer-modified-p) (save-buffer))
		  (message "Processing kill file %s...done" (car kill-files))))
	    (setq kill-files (cdr kill-files)))))
    (if expunge-below (gnus-summary-expunge-below expunge-below))
    (let (gnus-summary-inhibit-highlight)
      (gnus-summary-set-mark-below mark-below))
    (if beg
	(let ((nunreads (- unreads (length gnus-newsgroup-unreads))))
	  (or (eq nunreads 0)
	      (message "Marked %d articles as read" nunreads))
	  nunreads)
      0)))

;; Kill changes and new format by suggested by JWZ and Sudish Joseph
;; <joseph@cis.ohio-state.edu>.  
(defun gnus-kill (field regexp &optional exe-command all)
  "If FIELD of an article matches REGEXP, execute COMMAND.
Optional 1st argument COMMAND is default to
	(gnus-summary-mark-as-read nil \"X\").
If optional 2nd argument ALL is non-nil, articles marked are also applied to.
If FIELD is an empty string (or nil), entire article body is searched for.
COMMAND must be a lisp expression or a string representing a key sequence."
  ;; We don't want to change current point nor window configuration.
  (save-excursion
    (save-window-excursion
      ;; Selected window must be summary buffer to execute keyboard
      ;; macros correctly. See command_loop_1.
      (switch-to-buffer gnus-summary-buffer 'norecord)
      (goto-char (point-min))		;From the beginning.
      (let ((kill-list regexp)
	    (date (current-time-string))
	    (command (or exe-command '(gnus-summary-mark-as-read 
				       nil gnus-kill-file-mark)))
	    kill kdate prev)
	(if (listp kill-list)
	    ;; It is a list.
	    (if (not (consp (cdr kill-list)))
		;; It's on the form (regexp . date).
		(if (zerop (gnus-execute field (car kill-list) 
					 command nil (not all)))
		    (if (> (gnus-days-between date (cdr kill-list))
			   gnus-kill-expiry-days)
			(setq regexp nil))
		  (setcdr kill-list date))
	      (while (setq kill (car kill-list))
		(if (consp kill)
		    ;; It's a temporary kill.
		    (progn
		      (setq kdate (cdr kill))
		      (if (zerop (gnus-execute 
				  field (car kill) command nil (not all)))
			  (if (> (gnus-days-between date kdate)
				 gnus-kill-expiry-days)
			      ;; Time limit has been exceeded, so we
			      ;; remove the match.
			      (if prev
				  (setcdr prev (cdr kill-list))
				(setq regexp (cdr regexp))))
			;; Successful kill. Set the date to today.
			(setcdr kill date)))
		  ;; It's a permanent kill.
		  (gnus-execute field kill command nil (not all)))
		(setq prev kill-list)
		(setq kill-list (cdr kill-list))))
	  (gnus-execute field kill-list command nil (not all)))
	)))
  (if regexp
      (gnus-pp-gnus-kill
       (nconc (list 'gnus-kill field 
		    (if (consp regexp) (list 'quote regexp) regexp))
	      (if (or exe-command all) (list (list 'quote exe-command)))
	      (if all (list t) nil)))))

(defun gnus-pp-gnus-kill (object)
  (if (or (not (consp (nth 2 object)))
	  (not (consp (cdr (nth 2 object))))
	  (and (eq 'quote (car (nth 2 object)))
	       (not (consp (cdr (car (cdr (nth 2 object))))))))
      (concat "\n" (prin1-to-string object))
    (save-excursion
      (set-buffer (get-buffer-create "*Gnus PP*"))
      (buffer-disable-undo (current-buffer))
      (erase-buffer)
      (insert (format "\n(%S %S\n  '(" (nth 0 object) (nth 1 object)))
      (let ((klist (car (cdr (nth 2 object))))
	    (first t))
	(while klist
	  (insert (if first (progn (setq first nil) "")  "\n    ")
		  (prin1-to-string (car klist)))
	  (setq klist (cdr klist))))
      (insert ")")
      (and (nth 3 object)
	   (insert "\n  " 
		   (if (and (consp (nth 3 object))
			    (not (eq 'quote (car (nth 3 object))))) 
		       "'" "")
		   (prin1-to-string (nth 3 object))))
      (and (nth 4 object)
	   (insert "\n  t"))
      (insert ")")
      (prog1
	  (buffer-substring (point-min) (point-max))
	(kill-buffer (current-buffer))))))

(defun gnus-execute-1 (function regexp form header)
  (save-excursion
    (let (did-kill)
      (if (null header)
	  nil				;Nothing to do.
	(if function
	    ;; Compare with header field.
	    (let (value)
	      (and header
		   (progn
		     (setq value (funcall function header))
		     ;; Number (Lines:) or symbol must be converted to string.
		     (or (stringp value)
			 (setq value (prin1-to-string value)))
		     (setq did-kill (string-match regexp value)))
		   (if (stringp form)	;Keyboard macro.
		       (execute-kbd-macro form)
		     (funcall form))))
	  ;; Search article body.
	  (let ((gnus-current-article nil) ;Save article pointer.
		(gnus-last-article nil)
		(gnus-break-pages nil)	;No need to break pages.
		(gnus-mark-article-hook nil)) ;Inhibit marking as read.
	    (message "Searching for article: %d..." (header-number header))
	    (gnus-article-setup-buffer)
	    (gnus-article-prepare (header-number header) t)
	    (if (save-excursion
		  (set-buffer gnus-article-buffer)
		  (goto-char (point-min))
		  (setq did-kill (re-search-forward regexp nil t)))
		(if (stringp form)	;Keyboard macro.
		    (execute-kbd-macro form)
		  (funcall form))))))
      did-kill)))

(defun gnus-execute (field regexp form &optional backward ignore-marked)
  "If FIELD of article header matches REGEXP, execute lisp FORM (or a string).
If FIELD is an empty string (or nil), entire article body is searched for.
If optional 1st argument BACKWARD is non-nil, do backward instead.
If optional 2nd argument IGNORE-MARKED is non-nil, articles which are
marked as read or ticked are ignored."
  (save-excursion
    (let ((killed-no 0)
	  function header article)
      (if (or (null field) (string-equal field ""))
	  (setq field nil)
	;; Get access function of header filed.
	(setq function (intern-soft (concat "gnus-header-" (downcase field))))
	(if (and function (fboundp function))
	    (setq function (symbol-function function))
	  (error "Unknown header field: \"%s\"" field))
	;; Make FORM funcallable.
	(if (and (listp form) (not (eq (car form) 'lambda)))
	    (setq form (list 'lambda nil form)))
	;; Starting from the current article.
	(while (or (and (not article)
			(setq article (gnus-summary-article-number))
			t)
		   (setq article 
			 (gnus-summary-search-subject 
			  backward (not ignore-marked))))
	  (and (memq article gnus-newsgroup-kill-headers)
	       (gnus-execute-1 function regexp form 
			       (gnus-get-header-by-number article))
	       (setq killed-no (1+ killed-no)))))
      killed-no)))


;;; Gnus Score File

;; All score code written by Per Abrahamsen <abraham@iesd.auc.dk>.

(defun gnus-score-default (level)
  (if level (prefix-numeric-value level) 
    gnus-score-interactive-default-score))

(defun gnus-score-set (symbol value &optional alist)
  ;; Set SYMBOL to VALUE in ALIST.
  (let* ((local-score (gnus-score-file-name gnus-newsgroup-name))
	 (alist 
	  (or alist 
	      (setq gnus-score-alist 
		    (cdr (assoc local-score gnus-score-cache)))
	      (progn
		(gnus-score-load local-score)
		gnus-score-alist)))
	 (entry (assoc symbol alist)))
    (cond (entry
	   (setcdr entry value))
	  ((null alist)
	   (error "Empty alist"))
	  (t
	   (setcdr alist (cons (cons symbol value) (cdr alist)))))))

(defun gnus-score-get (symbol &optional alist)
  ;; Get SYMBOL's definition in ALIST.
  (cdr (assoc symbol 
	      (or alist 
;		  (setq gnus-score-alist 
;			(cdr (assoc (gnus-score-file-name gnus-newsgroup-name)
;				    gnus-score-cache))
		  
		  (progn
		    (gnus-score-load 
		     (gnus-score-file-name gnus-newsgroup-name))
		    gnus-score-alist)))))

(defun gnus-score-edit-file (group)
  "Edit score file for GROUP."
  (interactive (list (read-string "Edit SCORE file for: "
				  (cons (or gnus-newsgroup-name "") 1))))
  (and (get-buffer gnus-summary-buffer) (gnus-score-save))
  (find-file (gnus-score-file-name group))
  (emacs-lisp-mode))

(defun gnus-score-load-file (file)
  ;; Load score file FILE. Updates `gnus-scores-lists'.
  (let ((cache (assoc file gnus-score-cache)))
    (if cache
	(progn
	  (setq gnus-score-alist (cdr cache))
	  (if (memq t (mapcar (lambda (e) (stringp (car e))) gnus-score-alist))
	      (setq gnus-scores-lists 
		    (cons gnus-score-alist gnus-scores-lists))))
      (setq gnus-score-alist nil)
      (load file t nil t)
      (if (memq t (mapcar (lambda (e) (stringp (car e))) gnus-score-alist))
	  (setq gnus-scores-lists (cons gnus-score-alist gnus-scores-lists)))
      (or gnus-score-alist
	  (setq gnus-score-alist (copy-alist '((touched . nil)))))
      (setq gnus-score-cache
	    (cons (cons file gnus-score-alist) gnus-score-cache))))
  (let ((mark (gnus-score-get 'mark))
	(expunge (gnus-score-get 'expunge))
	(files (gnus-score-get 'files))
	(eval (gnus-score-get 'eval)))
    (if files (mapcar (lambda (file) (gnus-score-load-file file)) files))
    (if eval (eval eval))
    (if mark (setq gnus-summary-mark-below mark))
    (if expunge (setq gnus-summary-expunge-below expunge))))

(defun gnus-score-load (file)
  ;; Load score FILE.
  ;; Updates free variables `gnus-score-alist' and `scores'.

  (let ((cache (assoc file gnus-score-cache)))
    (if cache
	(setq gnus-score-alist (cdr cache))
      (setq gnus-score-alist nil)
      (load file t nil t)
      (or gnus-score-alist
	  (setq gnus-score-alist (copy-alist '((touched . nil)))))
      (setq gnus-score-cache
	    (cons (cons file gnus-score-alist) gnus-score-cache)))))
  
(defun gnus-score-save ()
  ;; Save all SCORE information.
  (let (cache)
    (save-excursion
      (set-buffer gnus-summary-buffer)
      (setq cache gnus-score-cache
	    gnus-score-cache nil))
    (save-excursion
      (setq gnus-score-alist nil)
      (set-buffer (get-buffer-create "*Score*"))
      (buffer-disable-undo (current-buffer))
      (let (entry score file)
	(while cache
	  (setq entry (car cache)
		cache (cdr cache)
		file (car entry)
		score (cdr entry))
	  (if (null (gnus-score-get 'touched score))
	      ()
	    (gnus-score-set 'touched nil score)
	    (erase-buffer)
	    (let (emacs-lisp-mode-hook)
	      (pp (list 'setq 'gnus-score-alist (list 'quote score))
		  (current-buffer)))
	    (make-directory (file-name-directory file) t)
	      (write-region (point-min) (point-max) file nil 'silent))))
      (kill-buffer (current-buffer)))))
  
(defun gnus-score-headers ()
  ;; Score `gnus-newsgroup-headers'.
  (let ((score-files (and gnus-score-find-score-files-function
			  (fboundp gnus-score-find-score-files-function)
			  (funcall gnus-score-find-score-files-function
				   gnus-newsgroup-name)))
	scores)
    ;; Load the SCORE files.
    (while score-files
      (gnus-score-load-file (car score-files))
      (setq score-files (cdr score-files)))
    (setq scores gnus-scores-lists
	  gnus-scores-lists nil)
    (if (not (and gnus-summary-default-score
		  scores
		  (> (length gnus-newsgroup-headers)
		     (length gnus-newsgroup-scored))))
	()
      (let* ((entries gnus-header-index)
	     (now (current-time-string))
	     (expire (- (gnus-useful-date now) gnus-kill-expiry-days))
	     (headers gnus-newsgroup-headers)
	     entry header)
	(message "Scoring...")
	;; Create articles, an alist of the form `(HEADER . SCORE)'.
	(while headers
	  (setq header (car headers)
		headers (cdr headers))
	  ;; WARNING: The assq makes the function O(N*S) while it could
	  ;; be written as O(N+S), where N is (length gnus-newsgroup-headers)
	  ;; and S is (length gnus-newsgroup-scored).
	  (or (assq (header-number header) gnus-newsgroup-scored)
	      (setq gnus-scores-articles       ;Total of 2 * N cons-cells used.
		    (cons (cons header gnus-summary-default-score)
			  gnus-scores-articles))))
  
	(save-excursion
	  (set-buffer (get-buffer-create "*Headers*"))
	  (buffer-disable-undo (current-buffer))
	  ;; Run each header through the score process.
	  (while entries
	    (setq entry (car entries)
		  header (nth 0 entry)
		  entries (cdr entries))
	    (setq gnus-score-index (nth 1 (assoc header gnus-header-index)))
	    (if (< 0 (apply 'max (mapcar
				  (lambda (score)
				    (length (gnus-score-get header score)))
				  scores)))
		(funcall (nth 2 entry) scores header now expire)))
	  ;; Remove the buffer.
	  (kill-buffer (current-buffer)))

	(message "Scoring...")
	;; Add articles to `gnus-newsgroup-scored'.
	(while gnus-scores-articles
	  (or (= gnus-summary-default-score (cdr (car gnus-scores-articles)))
	      (setq gnus-newsgroup-scored
		    (cons (cons (header-number 
				 (car (car gnus-scores-articles)))
				(cdr (car gnus-scores-articles)))
			  gnus-newsgroup-scored)))
	  (setq gnus-scores-articles (cdr gnus-scores-articles)))

	(message "Scoring...done")))))

;(defun gnus-score-integer (scores header now expire)
;  )

;(defun gnus-score-date (scores header now expire)
;  )

(defun gnus-score-string (scores header now expire)
  ;; Score ARTICLES according to HEADER in SCORES.
  ;; Update matches entries to NOW and remove unmatched entried older
  ;; than EXPIRE.
  
  ;; Insert the unique article headers in the buffer.
  (let ((gnus-score-index (nth 1 (assoc header gnus-header-index)))
	;; gnus-score-index is used as a free variable.
	alike last this art entries alist articles)

    ;; Sorting the articles costs os O(N*log N) but will allow us to
    ;; only match with each unique header.  Thus the actual matching
    ;; will be O(M*U) where M is the number of strings to match with,
    ;; and U is the number of unique headers.  It is assumed (but
    ;; untested) this will be a net win because of the large constant
    ;; factor involved with string matching.
    (setq gnus-scores-articles (sort gnus-scores-articles 'gnus-score-string<)
	  articles gnus-scores-articles)

    (erase-buffer)
    (while articles
      (setq art (car articles)
	    this (aref (car art) gnus-score-index)
	    articles (cdr articles))
      (if (equal last this)
	  ;; O(N*H) cons-cells used here, where H is the number of
	  ;; headers.
	  (setq alike (cons art alike))
	(if last
	    (progn
	      ;; Insert the line, with a text property on the
	      ;; terminating newline refering to the articles with
	      ;; this line.
	      (insert last ?\n)
	      (put-text-property (1- (point)) (point) 'articles alike)))
	(setq alike (list art)
	      last this)))
    (and last				; Bwadr, duplicate code.
	 (progn
	   (insert last ?\n)			
	   (put-text-property (1- (point)) (point) 'articles alike)))
  
    ;; Find matches.
    (while scores
      (setq alist (car scores)
	    scores (cdr scores)
	    entries (assoc header alist))
      (while (cdr entries)		;First entry is the header index.
	(let* ((rest (cdr entries))		
	       (kill (car rest))
	       (match (nth 0 kill))
	       (type (nth 1 kill))
	       (score (nth 2 kill))
	       (date (nth 3 kill))
	       (found nil)
	       (case-fold-search t)
	       arts art)
	  (goto-char (point-min))
	  (while (if type
		     (re-search-forward match nil t)
		   (search-forward match nil t))
	    (end-of-line 1)
	    (setq found t
		  arts (get-text-property (point) 'articles))
	    ;; Found a match, update scores.
	    (while arts
	      (setq art (car arts)
		    arts (cdr arts))
	      (setcdr art (+ score (cdr art)))))
	  ;; Update expire date
	  (cond ((null date))		;Permanent entry.
		(found			;Match, update date.
		 (gnus-score-set 'touched t alist)
		 (setcar (nthcdr 3 kill) now))
		((< (gnus-useful-date date) expire) ;Old entry, remove.
		 (gnus-score-set 'touched t alist)
		 (setcdr entries (cdr rest))
		 (setq rest entries)))
	  (setq entries rest))))))

(defun gnus-score-string< (a1 a2)
  ;; Compare headers in articles A2 and A2.
  ;; The header index used is the free variable `gnus-score-index'.
  (string-lessp (aref (car a1) gnus-score-index)
		(aref (car a2) gnus-score-index)))

(defun gnus-useful-date (date)
  ;; Return the numeric day corresponding to the DATE string.
  (let ((d (mapcar (lambda (s) (and s (string-to-int s)) )
		   (timezone-parse-date date))))
    (timezone-absolute-from-gregorian (nth 1 d) (nth 2 d) (car d))))

(defun gnus-score-build-cons (article)
  ;; Build a `gnus-newsgroup-scored' type cons from ARTICLE.
  (cons (header-number (car article)) (cdr article)))

(defconst gnus-header-index
  ;; Name to index alist.
  '(("number" 0 gnus-score-integer)
    ("subject" 1 gnus-score-string)
    ("from" 2 gnus-score-string)
    ("date" 3 gnus-score-date)
    ("id" 4 gnus-score-string) 
    ("references" 5 gnus-score-string) 
    ("chars" 6 gnus-score-integer) 
    ("lines" 7 gnus-score-integer) 
    ("xref" 8 gnus-score-string)))

(defvar gnus-score-file-suffix "SCORE"
  "Suffix of the score files.")

(defun gnus-score-file-name (newsgroup)
  "Return the name of a score file for NEWSGROUP."
  (cond  ((or (null newsgroup)
	      (string-equal newsgroup ""))
	  ;; The global score file is placed at top of the directory.
	  (expand-file-name gnus-score-file-suffix
			    (or gnus-kill-files-directory "~/News")))
	 (gnus-use-long-file-name
	  ;; Append ".SCORE" to newsgroup name.
	  (expand-file-name (concat newsgroup "." gnus-score-file-suffix)
			    (or gnus-kill-files-directory "~/News")))
	 (t
	  ;; Place "KILL" under the hierarchical directory.
	  (expand-file-name (concat (gnus-newsgroup-directory-form newsgroup)
				    "/" gnus-score-file-suffix)
			    (or gnus-kill-files-directory "~/News")))))

(defun gnus-score-score-files (group)
  "Return a list of all possible score files."
  (or gnus-kill-files-directory (setq gnus-kill-files-directory "~/News/"))
  (if (not (file-exists-p gnus-kill-files-directory))
      (setq gnus-score-file-list nil)
    (if gnus-use-long-file-name
	(if (or (not gnus-score-file-list)
		(gnus-file-newer-than gnus-kill-files-directory
				      (car gnus-score-file-list)))
	    (setq gnus-score-file-list
		  (cons (nth 5 (file-attributes gnus-kill-files-directory))
			(directory-files
			 gnus-kill-files-directory t
			 (concat gnus-score-file-suffix "$")))))
      (let ((dir (expand-file-name
		  (concat gnus-kill-files-directory
			  (gnus-replace-chars-in-string group ?. ?/))))
	    (mdir (length (expand-file-name gnus-kill-files-directory)))
	    files)
	(if (file-exists-p (concat dir "/" gnus-score-file-suffix))
	    (setq files (list (concat dir "/" gnus-score-file-suffix))))
	(while (>= (1+ (length dir)) mdir)
	  (and (file-exists-p (concat dir "/all/" gnus-score-file-suffix))
	       (setq files (cons (concat dir "/all/" gnus-score-file-suffix)
				 files)))
	  (string-match "/[^/]*$" dir)
	  (setq dir (substring dir (match-beginning 0))))
	(setq gnus-score-file-list (cons nil files)))))
  (cdr gnus-score-file-list))
	
(defun gnus-score-find-single (group)
  "Return list containing the score file for GROUP."
  (list (gnus-score-file-name group)))

(defun gnus-score-find-hierarchical (group)
  "Return list of score files for GROUP.
This includes the score file for the group and all its parents."
  (let ((all (copy-sequence '(nil)))
	(start 0))
    (while (string-match "\\." group (1+ start))
      (setq start (match-beginning 0))
      (setq all (cons (substring group 0 start) all)))
    (setq all (cons group all))
    (mapcar 'gnus-score-file-name (nreverse all))))

(defun gnus-score-find-bnews (group)
  "Return a list of score files for GROUP.
The score files are those files in the ~/News directory which matches
GROUP using BNews sys file syntax."
  (let ((sfiles (gnus-score-score-files group))
	(klen (length (expand-file-name gnus-kill-files-directory)))
	ofiles not-match regexp)
    (save-excursion
      (set-buffer (get-buffer-create "*gnus score files*"))
      (buffer-disable-undo (current-buffer))
      (while sfiles
	(erase-buffer)
	(insert (car sfiles))
	(goto-char 1)
	(re-search-forward (concat "." gnus-score-file-suffix "$"))
	(replace-match "") 
	(goto-char 1)
	(delete-char klen)
	(while (search-forward "all" nil t)
	  (replace-match ".+"))
	(goto-char 1)
	(if (looking-at "not.")
	    (progn
	      (setq not-match t)
	      (setq regexp (buffer-substring 5 (point-max))))
	  (setq regexp (buffer-substring 1 (point-max)))
	  (setq not-match nil))
	(if (or (and not-match
		     (not (string-match regexp group)))
		(and (not not-match)
		     (string-match regexp group)))
	    (setq ofiles (cons (car sfiles) ofiles)))
	(setq sfiles (cdr sfiles)))
      (kill-buffer (current-buffer))
      ofiles)))



;;; Gnus Posting Functions
;;;

(defvar gnus-organization-file "/usr/lib/news/organization"
  "*Local news organization file.")

(defvar gnus-post-news-buffer "*post-news*")
(defvar gnus-winconf-post-news nil)

(autoload 'news-reply-mode "rnewspost")

;;; Post news commands of Gnus group mode and summary mode

(defun gnus-group-post-news ()
  "Post an article."
  (interactive)
  (gnus-set-global-variables)
  ;; Save window configuration.
  (setq gnus-winconf-post-news (current-window-configuration))
  ;; Fix by Sudish Joseph <joseph@cis.ohio-state.edu>.
  (or gnus-newsgroup-name (setq gnus-newsgroup-name (gnus-group-group-name)))
  (unwind-protect
      (gnus-post-news 'post nil)
    (or (and (eq (current-buffer) (get-buffer gnus-post-news-buffer))
	     (not (zerop (buffer-size))))
	;; Restore last window configuration.
	(and gnus-winconf-post-news
	     (set-window-configuration gnus-winconf-post-news))))
  ;; We don't want to return to summary buffer nor article buffer later.
  (setq gnus-winconf-post-news nil)
  (if (get-buffer gnus-summary-buffer)
      (bury-buffer gnus-summary-buffer))
  (if (get-buffer gnus-article-buffer)
      (bury-buffer gnus-article-buffer)))

(defun gnus-summary-post-news ()
  "Post an article."
  (interactive)
  (gnus-set-global-variables)
  ;; Save window configuration.
  (setq gnus-winconf-post-news (current-window-configuration))
  (unwind-protect
      (gnus-post-news 'post gnus-newsgroup-name)
    (or (and (eq (current-buffer) (get-buffer gnus-post-news-buffer))
	     (not (zerop (buffer-size))))
	;; Restore last window configuration.
	(and gnus-winconf-post-news
	     (set-window-configuration gnus-winconf-post-news))))
  ;; We don't want to return to article buffer later.
  (if (get-buffer gnus-article-buffer)
      (bury-buffer gnus-article-buffer)))

(defun gnus-summary-followup (yank)
  "Compose a followup to an article.
If prefix argument YANK is non-nil, original article is yanked automatically."
  (interactive "P")
  (gnus-set-global-variables)
  (gnus-summary-select-article t)
  (let ((headers gnus-current-headers)
	(gnus-newsgroup-name gnus-newsgroup-name))
    ;; Check Followup-To: poster.
    (set-buffer gnus-article-buffer)
    (if (and gnus-use-followup-to
	     (string-equal "poster" (gnus-fetch-field "followup-to"))
	     (or (not (eq gnus-use-followup-to t))
		 (not (gnus-y-or-n-p 
		       "Do you want to ignore `Followup-To: poster'? "))))
	;; Mail to the poster.  Gnus is now RFC1036 compliant.
	(gnus-summary-reply yank)
      ;; Save window configuration.
      (setq gnus-winconf-post-news (current-window-configuration))
      (unwind-protect
	  (gnus-post-news 'followup headers gnus-article-buffer yank)
	(or (and (eq (current-buffer) (get-buffer gnus-post-news-buffer))
		 (not (zerop (buffer-size))))
	    ;; Restore last window configuration.
	    (and gnus-winconf-post-news
		 (set-window-configuration gnus-winconf-post-news))))
      ;; We don't want to return to article buffer later.
      (bury-buffer gnus-article-buffer))))

(defun gnus-summary-followup-with-original ()
  "Compose a followup to an article and include the original article."
  (interactive)
  (gnus-summary-followup t))

;; Suggested by Daniel Quinlan <quinlan@best.com>.
(defun gnus-summary-followup-and-reply (yank)
  "Compose a followup and do an auto mail to author."
  (interactive "P")
  (let ((gnus-auto-mail-to-author t))
    (gnus-summary-followup yank)))

(defun gnus-summary-followup-and-reply-with-original ()
  "Compose a followup, include the original, and do an auto mail to author."
  (interactive)
  (gnus-summary-followup-and-reply t))

(defun gnus-summary-cancel-article ()
  "Cancel an article you posted."
  (interactive)
  (gnus-set-global-variables)
  (gnus-summary-select-article t)
  (gnus-eval-in-buffer-window gnus-article-buffer
			      (gnus-cancel-news)))

(defun gnus-summary-supersede-article ()
  "Compose an article that will supersede a previous article.
This is done simply by taking the old article and adding a Supersedes
header line with the old Message-ID."
  (interactive)
  (gnus-set-global-variables)
  (if (not
       (string-equal
	(downcase (mail-strip-quoted-names 
		   (header-from gnus-current-headers)))
	(downcase (mail-strip-quoted-names (gnus-inews-user-name)))))
      (error "This article is not yours."))
  (gnus-summary-select-article t)
  (save-excursion
    (set-buffer gnus-article-buffer)
    (let ((buffer-read-only nil))
      (goto-char (point-min))
      (search-forward "\n\n" nil t)
      (if (not (re-search-backward "^Message-ID: " nil t))
	  (error "No Message-ID in this article"))))
  (if (gnus-post-news 'post gnus-newsgroup-name)
      (progn
	(erase-buffer)
	(insert-buffer gnus-article-buffer)
	(goto-char (point-min))
	(search-forward "\n\n" nil t)
	(if (not (re-search-backward "^Message-ID: " nil t))
	    (error "No Message-ID in this article")
	  (replace-match "Supersedes: "))
	(search-forward "\n\n")
	(forward-line -1)
	(insert mail-header-separator))))


;;; Post a News using NNTP

;;;###autoload
(fset 'sendnews 'gnus-post-news)

;;;###autoload
(fset 'postnews 'gnus-post-news)

(defun gnus-post-news (method &optional header article-buffer yank)
  "Begin editing a new USENET news article to be posted.
Type \\[describe-mode] in the buffer to get a list of commands."
  (interactive)
  (if (or (not gnus-novice-user)
	  gnus-expert-user
	  (not (eq 'post 
		   (nth 1 (assoc 
			   (format "%s" (car (gnus-find-method-for-group 
					      gnus-newsgroup-name)))
			   gnus-valid-select-methods))))
	  (gnus-y-or-n-p "Are you sure you want to post to all of USENET? "))
      (let ((sumart (if (eq method 'followup)
			(save-excursion
			  (set-buffer gnus-summary-buffer)
			  (cons (current-buffer) gnus-current-article))))
	    post-buf)
	(if (and gnus-interactive-post
		 (not gnus-expert-user)
		 (eq method 'post)
		 (not header))
	    (setq header 
		  (completing-read "Group: " gnus-active-hashtb nil t)))
	(setq mail-reply-buffer article-buffer)
	(setq gnus-post-news-buffer 
	      (setq post-buf
		    (gnus-request-post-buffer 
		     method (if (stringp header) 
				(gnus-group-real-name header) header)
		     article-buffer)))
	(if (eq method 'post)
	    (progn
	      (delete-other-windows)
	      (switch-to-buffer post-buf))
	  (delete-other-windows)
	  (if (not yank)
	      (progn
		(switch-to-buffer article-buffer)
		(pop-to-buffer post-buf))
	    (switch-to-buffer post-buf)))
	(gnus-overload-functions)
	(make-local-variable 'gnus-article-reply)
	(make-local-variable 'gnus-article-check-size)
	(setq gnus-article-reply sumart)
	;; Handle author copy using BCC field.
	(if (and gnus-mail-self-blind
		 (not (mail-fetch-field "bcc")))
	    (progn
	      (mail-position-on-field "BCC")
	      (insert (if (stringp gnus-mail-self-blind)
			  gnus-mail-self-blind
			(user-login-name)))))
	;; Handle author copy using FCC field.
	(if gnus-author-copy
	    (progn
	      (mail-position-on-field "FCC")
	      (insert gnus-author-copy)))
	(goto-char (point-min))
	(if (and (eq method 'post) (not header))
	    (end-of-line)
	  (search-forward (concat "\n" mail-header-separator "\n"))
	  (if yank 
	      (save-excursion
		(run-hooks 'news-reply-header-hook)
		(mail-yank-original nil)))
	  (if gnus-post-prepare-function
	      (funcall gnus-post-prepare-function 
		       (if (stringp header) header gnus-newsgroup-name))))))
  (setq gnus-article-check-size (cons (buffer-size) (gnus-article-checksum)))
  (message "")
  t)

(defun gnus-inews-news (&optional use-group-method)
  "Send a news message.
If given a prefix, and the group is a foreign group, this function
will attempt to use the foreign server to post the article."
  (interactive "P")
  ;; Check whether the article is a good Net Citizen.
  (if (and gnus-article-check-size (not (gnus-inews-check-post)))
      ;; Aber nein!
      ()
    ;; Looks ok, so we do the nasty.
    (let* ((case-fold-search nil)
	   (server-running (gnus-server-opened gnus-select-method))
	   (reply gnus-article-reply))
      (save-excursion
	;; Connect to default NNTP server if necessary.
	;; Suggested by yuki@flab.fujitsu.junet.
	(gnus-start-news-server)	;Use default server.
	;; NNTP server must be opened before current buffer is modified.
	(widen)
	(goto-char (point-min))
	(run-hooks 'news-inews-hook)
	(save-restriction
	  (narrow-to-region
	   (point-min)
	   (progn
	     (goto-char (point-min))
	     (search-forward (concat "\n" mail-header-separator "\n"))
	     (point)))

	  ;; Correct newsgroups field: change sequence of spaces to comma and 
	  ;; eliminate spaces around commas.  Eliminate imbedded line breaks.
	  (goto-char (point-min))
	  (if (search-forward-regexp "^Newsgroups: +" nil t)
	      (save-restriction
		(narrow-to-region
		 (point)
		 (if (re-search-forward "^[^ \t]" nil 'end)
		     (match-beginning 0)
		   (point-max)))
		(goto-char (point-min))
		(replace-regexp "\n[ \t]+" " ") ;No line breaks (too confusing)
		(goto-char (point-min))
		(replace-regexp "[ \t\n]*,[ \t\n]*\\|[ \t]+" ",")))

	  ;; Added by Per Abrahamsen <abraham@iesd.auc.dk>.
	  ;; Help save the the world!
	  (or 
	   gnus-expert-user
	   (let ((newsgroups (mail-fetch-field "newsgroups"))
		 (followup-to (mail-fetch-field "followup-to"))
		 groups to)
	     (if (and (string-match "," newsgroups) (not followup-to))
		 (progn
		   (while (string-match "," newsgroups)
		     (setq groups
			   (cons (list (substring newsgroups
						  0 (match-beginning 0)))
				 groups))
		     (setq newsgroups (substring newsgroups (match-end 0))))
		   (setq groups (nreverse (cons (list newsgroups) groups)))

		   (setq to
			 (completing-read "Followups to: (default all groups) "
					  groups))
		   (if (> (length to) 0)
		       (progn
			 (goto-char (point-min))
			 (insert "Followup-To: " to "\n")))))))

	  ;; Cleanup Followup-To.
	  (goto-char (point-min))
	  (if (search-forward-regexp "^Followup-To: +" nil t)
	      (save-restriction
		(narrow-to-region
		 (point)
		 (if (re-search-forward "^[^ \t]" nil 'end)
		     (match-beginning 0)
		   (point-max)))
		(goto-char (point-min))
		(replace-regexp "\n[ \t]+" " ") ;No line breaks (too confusing)
		(goto-char (point-min))
		(replace-regexp "[ \t\n]*,[ \t\n]*\\|[ \t]+" ",")))

	  ;; Mail the message too if To:, Bcc:. or Cc: exists.
	  (if (or (mail-fetch-field "to" nil t)
		  (mail-fetch-field "bcc" nil t)
		  (mail-fetch-field "cc" nil t))
	      (if gnus-mail-send-method
		  (progn
		    (message "Sending via mail...")
		    (widen)
		    (funcall gnus-mail-send-method)
		    (message "Sending via mail... done")
		    (save-excursion
		      (save-restriction
			(goto-char 1)
			(narrow-to-region
			 1 (re-search-forward mail-header-separator))
			(goto-char 1)
			(delete-matching-lines "BCC:.*"))))
		(ding)
		(message "No mailer defined.  To: and/or Cc: fields ignored.")
		(sit-for 1))))

	;; Send to NNTP server. 
	(message "Posting to USENET...")
	(if (gnus-inews-article use-group-method)
	    (progn
	      (message "Posting to USENET... done")
	      (if (and reply
		       (get-buffer (car reply))
		       (buffer-name (car reply)))
		  (progn
		    (save-excursion
		      (set-buffer gnus-summary-buffer)
		      (gnus-summary-mark-article-as-replied 
		       (cdr reply))))))
	  ;; We cannot signal an error.
	  (ding) (message "Article rejected: %s" 
			  (gnus-status-message gnus-select-method)))
	(set-buffer-modified-p nil))
      ;; If NNTP server is opened by gnus-inews-news, close it by myself.
      (or server-running
	  (gnus-close-server (gnus-find-method-for-group gnus-newsgroup-name)))
      (and (fboundp 'bury-buffer) (bury-buffer))
      ;; Restore last window configuration.
      (and gnus-winconf-post-news
	   (set-window-configuration gnus-winconf-post-news))
      (setq gnus-winconf-post-news nil))))

(defun gnus-inews-check-post ()
  "Check whether the post looks ok."
  (and 
   ;; Check excessive size.
   (if (> (buffer-size) 60000)
       (gnus-y-or-n-p (format "The article is %d octets long. Really post? "
			      (buffer-size)))
     t)
   ;; Check for commands in Subject.
   (save-excursion
     (save-restriction
       (goto-char (point-min))
       (narrow-to-region (point) (search-forward mail-header-separator))
       (if (string-match "^cmsg " (mail-fetch-field "subject"))
	   (gnus-y-or-n-p
	    "The control code \"cmsg \" is in the subject. Really post? ")
	 t)))
   ;; Check for control characters.
   (save-excursion
     (if (re-search-forward "[\000-\007\013\015-\037\200-\237]" nil t)
	 (gnus-y-or-n-p 
	  "The article contains control characters. Really post? ")
       t))
   ;; Check for multiple identical headers.
   (let (found)
     (save-excursion
       (save-restriction
	 (goto-char (point-min))
	 (narrow-to-region (point) (search-forward mail-header-separator))
	 (goto-char (point-min))
	 (while (and (not found) (re-search-forward "^[^ \t:]+: " nil t))
	   (save-excursion
	     (or (re-search-forward 
		  (concat "^" (setq found
				    (buffer-substring (match-beginning 0) 
						      (match-end 0))))
		  nil t)
		 (setq found nil))))
	 (if found
	     (gnus-y-or-n-p 
	      (format "Multiple %s headers. Really post? " found))
	   t))))
   ;; Check for version and sendsys.
   (save-excursion
     (save-restriction
       (goto-char (point-min))
       (narrow-to-region (point) (search-forward mail-header-separator))
       (if (re-search-backward "^Sendsys:\\|^Version:" nil t)
	   (gnus-yes-or-no-p
	    (format "The article contains a %s command. Really post? "
		    (buffer-substring (match-beginning 0) (match-end 0))))
	 t)))
   (save-excursion
     (save-restriction
       (goto-char (point-min))
       (narrow-to-region (point) (search-forward mail-header-separator))
       (let* ((case-fold-search t)
	      (from (mail-fetch-field "from")))
	 (if (and from
		  (string-match "@" from)
		  (not (string-match "@[^\\.]*\\." from)))
	     (gnus-yes-or-no-p
	      (format "The domain looks strange: \"%s\". Really post? "
		      from))
	   t))))
   ;; Use the (size . checksum) variable to see whether the
   ;; article is empty or has only quoted text.
   (if (and (= (buffer-size) (car gnus-article-check-size))
	    (= (gnus-article-checksum) (cdr gnus-article-check-size)))
       (gnus-yes-or-no-p
	"It looks like there's no new text in your article. Really post? ")
     t)))

(defun gnus-article-checksum ()
  (let ((sum 0))
    (save-excursion
      (while (not (eobp))
	(setq sum (logxor sum (following-char)))
	(forward-char 1)))
    sum))

(defun gnus-cancel-news ()
  "Cancel an article you posted."
  (interactive)
  (if (gnus-yes-or-no-p "Do you really want to cancel this article? ")
      (let ((from nil)
	    (newsgroups nil)
	    (message-id nil)
	    (distribution nil))
	(save-excursion
	  ;; Get header info. from original article.
	  (save-restriction
	    (gnus-article-show-all-headers)
	    (goto-char (point-min))
	    (search-forward "\n\n" nil 'move)
	    (narrow-to-region (point-min) (point))
	    (setq from (mail-fetch-field "from"))
	    (setq newsgroups (mail-fetch-field "newsgroups"))
	    (setq message-id (mail-fetch-field "message-id"))
	    (setq distribution (mail-fetch-field "distribution")))
	  ;; Verify if the article is absolutely user's by comparing
	  ;; user id with value of its From: field.
	  (if (not
	       (string-equal
		(downcase (mail-strip-quoted-names from))
		(downcase (mail-strip-quoted-names (gnus-inews-user-name)))))
	      (progn
		(ding) (message "This article is not yours."))
	    ;; Make control article.
	    (set-buffer (get-buffer-create " *Gnus-canceling*"))
	    (buffer-disable-undo (current-buffer))
	    (erase-buffer)
	    (insert "Newsgroups: " newsgroups "\n"
		    "Subject: cancel " message-id "\n"
		    "Control: cancel " message-id "\n"
		    mail-header-separator "\n"
		    "This is a cancel message from " from ".\n")
	    ;; Send the control article to NNTP server.
	    (message "Canceling your article...")
	    (if (gnus-inews-article)
		(message "Canceling your article... done")
	      (ding) (message "Failed to cancel your article"))
	    ;; Kill the article buffer.
	    (kill-buffer (current-buffer)))))))


;;; Lowlevel inews interface

(defun gnus-inews-article (&optional use-group-method)
  "Post an article in current buffer using NNTP protocol."
  (let ((artbuf (current-buffer))
	(tmpbuf (get-buffer-create " *Gnus-posting*")))
    (widen)
    (goto-char (point-max))
    ;; require a newline at the end for inews to append .signature to
    (or (= (preceding-char) ?\n)
	(insert ?\n))
    ;; Prepare article headers.  All message body such as signature
    ;; must be inserted before Lines: field is prepared.
    (save-restriction
      (goto-char (point-min))
      (narrow-to-region 
       (point-min) 
       (save-excursion
	 (search-forward (concat "\n" mail-header-separator "\n")) 
	 (forward-line -1) 
	 (point)))
      (gnus-inews-insert-headers)
      (run-hooks gnus-inews-article-header-hook)
      (widen))
    (save-excursion
      (set-buffer tmpbuf)
      (buffer-disable-undo (current-buffer))
      (erase-buffer)
      (insert-buffer-substring artbuf)
      ;; Remove the header separator.
      (goto-char (point-min))
      (search-forward (concat "\n" mail-header-separator "\n"))
      (replace-match "\n\n")
      ;; This hook may insert a signature.
      (run-hooks 'gnus-prepare-article-hook)
      ;; Run final inews hooks.  This hook may do FCC.
      ;; The article must be saved before being posted because
      ;; `gnus-request-post' modifies the buffer.
      (run-hooks 'gnus-inews-article-hook)
      ;; Post an article to NNTP server.
      ;; Return NIL if post failed.
      (prog1
	  (gnus-request-post 
	   (if use-group-method
	       (gnus-find-method-for-group gnus-newsgroup-name)
	     gnus-select-method) use-group-method)
	(kill-buffer (current-buffer))))))

(defun gnus-inews-insert-headers ()
  "Prepare article headers.
Headers already prepared in the buffer are not modified.
Headers in `gnus-required-headers' will be generated."
  (let ((Date (gnus-inews-date))
	(Message-ID (gnus-inews-message-id))
	(Organization (gnus-inews-organization))
	(From (gnus-inews-user-name))
	(Path (gnus-inews-path))
	(Subject nil)
	(Newsgroups nil)
	(Distribution nil)
	(Lines (gnus-inews-lines))
	(X-Newsreader gnus-version)
	(headers gnus-required-headers)
	(case-fold-search t)
	header value)
    ;; First we remove any old Message-IDs. This might be slightly
    ;; fascist, but if the user really wants to generate Message-IDs
    ;; by herself, she should remove it from the `gnus-required-list'. 
    (goto-char (point-min))
    (and (memq 'Message-ID headers)
	 (re-search-forward "^Message-ID:" nil t)
	 (delete-region (progn (beginning-of-line) (point))
			(progn (forward-line 1) (point))))
    ;; Remove NNTP-posting-host.
    (goto-char (point-min))
    (and (re-search-forward "nntp-posting-host^:" nil t)
	 (delete-region (progn (beginning-of-line) (point))
			(progn (forward-line 1) (point))))
    ;; Insert new Sender if the From is strange. 
    (let ((from (mail-fetch-field "from")))
      (if (and from (not (string= (downcase from) (downcase From))))
	  (progn
	    (goto-char (point-min))    
	    (and (re-search-forward "^Sender:" nil t)
		 (delete-region (progn (beginning-of-line) (point))
				(progn (forward-line 1) (point))))
	    (insert "Sender: " From "\n"))))
    ;; If there are References, and no "Re: ", then the thread has
    ;; changed name. See Son-of-1036.
    (if (and (mail-fetch-field "references")
	     (get-buffer gnus-article-buffer))
	(let ((psubject (gnus-simplify-subject-re
			 (mail-fetch-field "subject")))
	      subject)
	  (save-excursion
	    (set-buffer (get-buffer gnus-article-buffer))
	    (save-restriction
	      (gnus-narrow-to-headers)
	      (if (setq subject (mail-fetch-field "subject"))
		  (progn
		    (and gnus-summary-gather-subject-limit
			 (> (length subject) gnus-summary-gather-subject-limit)
			 (setq subject
			       (substring subject 0
					  gnus-summary-gather-subject-limit)))
		    (setq subject (gnus-simplify-subject-re subject))))))
	  (or (and psubject subject (string= subject psubject))
	      (progn
		(string-match "@" Message-ID)
		(setq Message-ID
		      (concat (substring Message-ID 0 (match-beginning 0))
			      "_-_" 
			      (substring Message-ID (match-beginning 0))))))))
    ;; Go through all the required headers and see if they are in the
    ;; articles already. If they are not, or are empty, they are
    ;; inserted automatically - except for Subject, Newsgroups and
    ;; Distribution. 
    (while headers
      (goto-char (point-min))
      (setq header (car headers))
      (if (or (not (re-search-forward 
		    (concat "^" (downcase (symbol-name header)) ":") nil t))
	      (progn
		(if (= (following-char) ? ) (forward-char 1) (insert " "))
		(looking-at "[ \t]*$")))
	  (progn
 	    (setq value (or (and (boundp header) (symbol-value header))
			    (read-from-minibuffer
			     (format "Empty header for %s; enter value: " 
				     header))))
	    (if (bolp)
		(save-excursion
		  (goto-char (point-max))
		  (insert (symbol-name header) ": " value "\n"))
	      (replace-match value))))
      (setq headers (cdr headers)))))

(defun gnus-inews-insert-signature ()
  "Insert a signature file.
If `gnus-signature-function' is bound and returns a string, this
string is used instead of the variable `gnus-signature-file'.
In either case, if the string is a file name, this file is
inserted. If the string is not a file name, the string itself is
inserted. 
If you never want any signature inserted, set both those variables to
nil."
  (save-excursion
    (let ((signature 
	   (or (and gnus-signature-function
		    (fboundp gnus-signature-function)
		    (funcall gnus-signature-function gnus-newsgroup-name))
	       gnus-signature-file))
	  b)
      (if (and signature
	       (or (file-exists-p signature)
		   (string-match " " signature)
		   (not (string-match 
			 "^/[^/]+/" (expand-file-name signature)))))
	  (progn
	    (goto-char (point-max))
	    ;; Delete any previous signatures.
	    (if (and mail-signature (search-backward "\n-- \n" nil t))
		(delete-region (1+ (point)) (point-max)))
	    (insert "\n-- \n")
	    (and (< 4 (setq b (count-lines 
			       (point)
			       (progn
				 (if (file-exists-p signature)
				     (insert-file-contents signature)
				   (insert signature))
				 (goto-char (point-max))
				 (or (bolp) (insert "\n"))
				 (point)))))
		 (not gnus-expert-user)
		 (not
		  (gnus-y-or-n-p
		   (format
		    "Your .sig is %d lines; it should be max 4. Really post? "
		    b)))
		 (if (file-exists-p signature)
		     (error (format "Edit %s." signature))
		   (error "Trim your signature."))))))))

(defun gnus-inews-do-fcc ()
  "Process FCC: fields in current article buffer.
Unless the first character of the field is `|', the article is saved
to the specified file using the function specified by the variable
gnus-author-copy-saver.  The default function rmail-output saves in
Unix mailbox format.
If the first character is `|', the contents of the article is send to
a program specified by the rest of the value."
  (let ((fcc-list nil)
	(fcc-file nil)
	(case-fold-search t))		;Should ignore case.
    (save-excursion
      (save-restriction
	(goto-char (point-min))
	(search-forward "\n\n")
	(narrow-to-region (point-min) (point))
	(goto-char (point-min))
	(while (re-search-forward "^FCC:[ \t]*" nil t)
	  (setq fcc-list
		(cons (buffer-substring
		       (point)
		       (progn
			 (end-of-line)
			 (skip-chars-backward " \t")
			 (point)))
		      fcc-list))
	  (delete-region (match-beginning 0)
			 (progn (forward-line 1) (point))))
	;; Process FCC operations.
	(widen)
	(while fcc-list
	  (setq fcc-file (car fcc-list))
	  (setq fcc-list (cdr fcc-list))
	  (cond ((string-match "^[ \t]*|[ \t]*\\(.*\\)[ \t]*$" fcc-file)
		 (let ((program (substring fcc-file
					   (match-beginning 1) (match-end 1))))
		   ;; Suggested by yuki@flab.fujitsu.junet.
		   ;; Send article to named program.
		   (call-process-region (point-min) (point-max) shell-file-name
					nil nil nil "-c" program)
		   ))
		(t
		 ;; Suggested by hyoko@flab.fujitsu.junet.
		 ;; Save article in Unix mail format by default.
		 (if (and gnus-author-copy-saver
			  (not (eq gnus-author-copy-saver 'rmail-output)))
		     (funcall gnus-author-copy-saver fcc-file)
		   (if (and (file-readable-p fcc-file) (rmail-file-p fcc-file))
		       (gnus-output-to-rmail fcc-file)
		     (rmail-output fcc-file 1 t t)))
		 ))
	  )
	))
    ))

(defun gnus-inews-path ()
  "Return uucp path."
  (let ((login-name (gnus-inews-login-name)))
    (cond ((null gnus-use-generic-path)
	   (concat (nth 1 gnus-select-method) "!" login-name))
	  ((stringp gnus-use-generic-path)
	   ;; Support GENERICPATH.  Suggested by vixie@decwrl.dec.com.
	   (concat gnus-use-generic-path "!" login-name))
	  (t login-name))
    ))

(defun gnus-inews-user-name ()
  "Return user's network address as \"NAME@DOMAIN (FULL-NAME)\"."
  (let ((full-name (gnus-inews-full-name)))
    (concat (if (or gnus-user-login-name gnus-use-generic-from
		    gnus-local-domain (getenv "DOMAINNAME"))
		(concat (gnus-inews-login-name) "@"
			(gnus-inews-domain-name gnus-use-generic-from))
	      user-mail-address)
	    ;; User's full name.
	    (cond ((string-equal full-name "") "")
		  ((string-equal full-name "&")	;Unix hack.
		   (concat " (" (user-login-name) ")"))
		  (t
		   (concat " (" full-name ")"))))))

(defun gnus-inews-login-name ()
  "Return login name."
  (or gnus-user-login-name (getenv "LOGNAME") (user-login-name)))

(defun gnus-inews-full-name ()
  "Return full user name."
  (or gnus-user-full-name (getenv "NAME") (user-full-name)))

(defun gnus-inews-domain-name (&optional genericfrom)
  "Return user's domain name.
If optional argument GENERICFROM is a string, use it as the domain
name; if it is non-nil, strip off local host name from the domain name.
If the function `system-name' returns full internet name and the
domain is undefined, the domain name is got from it."
  (if (or genericfrom gnus-local-domain (getenv "DOMAINNAME"))
      (let ((domain 
	     (or (if (stringp genericfrom) genericfrom)
		 (getenv "DOMAINNAME")
		 gnus-local-domain
		 ;; Function `system-name' may return full internet name.
		 ;; Suggested by Mike DeCorte <mrd@sun.soe.clarkson.edu>.
		 (if (string-match "\\." (system-name))
		     (substring (system-name) (match-end 0)))
		 (read-string "Domain name (no host): ")))
	    (host (or (if (string-match "\\." (system-name))
			  (substring (system-name) 0 (match-beginning 0)))
		      (system-name))))
	(if (string-equal "." (substring domain 0 1))
	    (setq domain (substring domain 1)))
	;; Support GENERICFROM as same as standard Bnews system.
	;; Suggested by ohm@kaba.junet and vixie@decwrl.dec.com.
	(cond ((null genericfrom)
	       (concat host "." domain))
	      ;;((stringp genericfrom) genericfrom)
	      (t domain)))
    (substring user-mail-address (1+ (string-match "@" user-mail-address)))))

(defun gnus-inews-full-address ()
  (let ((domain (gnus-inews-domain-name))
	(system (system-name))
	(case-fold-search t))
    (if (string-match "\\." system) system
      (if (string-match (concat "^" (regexp-quote system)) domain) domain
	(concat system "." domain)))))

(defun gnus-inews-message-id ()
  "Generate unique Message-ID for user."
  ;; Message-ID should not contain a slash and should be terminated by
  ;; a number.  I don't know the reason why it is so.
  (concat "<" (gnus-inews-unique-id) "@" (gnus-inews-full-address) ">"))

(defun gnus-inews-unique-id ()
  "Generate unique ID from user name and current time."
  (concat (downcase (gnus-inews-login-name))
	  (mapconcat 
	   (lambda (num) (gnus-number-base-x num 3 31))
	   (current-time) "")))

(defun gnus-inews-date ()
  "Current time string."
  (timezone-make-date-arpa-standard (current-time-string)))

(defun gnus-inews-organization ()
  "Return user's organization.
The ORGANIZATION environment variable is used if defined.
If not, the variable `gnus-local-organization' is used instead.
If it is a function, the function will be called with the current
newsgroup name as the argument.
If this is a file name, the contents of this file will be used as the
organization."
  (let* ((organization 
	  (or (getenv "ORGANIZATION")
	      (if gnus-local-organization
		  (if (and (symbolp gnus-local-organization)
			   (fboundp gnus-local-organization))
		      (funcall gnus-local-organization gnus-newsgroup-name)
		    gnus-local-organization))
	      gnus-organization-file
	      "~/.organization")))
    (and (stringp organization)
	 (> (length organization) 0)
	 (or (file-exists-p organization)
	     (string-match " " organization)
	     (not (string-match  "^/[^/]+/" (expand-file-name organization))))
	 (save-excursion
	   (set-buffer (get-buffer-create " *Gnus organization*"))
	   (buffer-disable-undo (current-buffer))
	   (erase-buffer)
	   (if (file-exists-p organization)
	       (insert-file-contents organization)
	     (insert organization))
	   (goto-char (point-min))
	   (while (re-search-forward " *\n *" nil t)
	     (replace-match " "))
	   (buffer-substring (point-min) (point-max))))))

(defun gnus-inews-lines ()
  "Count the number of lines and return numeric string."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (search-forward "\n\n" nil 'move)
      (int-to-string (count-lines (point) (point-max))))))


;;;
;;; Gnus Mail Functions 
;;;

(autoload 'news-mail-reply "rnewspost")
(autoload 'news-mail-other-window "rnewspost")

;;; Mail reply commands of Gnus summary mode

(defun gnus-summary-reply (yank)
  "Reply mail to news author.
If prefix argument YANK is non-nil, original article is yanked automatically.
Customize the variable gnus-mail-reply-method to use another mailer."
  (interactive "P")
  ;; Bug fix by jbw@bigbird.bu.edu (Joe Wells)
  ;; Stripping headers should be specified with mail-yank-ignored-headers.
  (gnus-summary-select-article t)
  (setq gnus-winconf-post-news (current-window-configuration))
  (let ((gnus-newsgroup-name gnus-newsgroup-name))
    (bury-buffer gnus-article-buffer)
    (funcall gnus-mail-reply-method yank)))

(defun gnus-summary-reply-with-original ()
  "Reply mail to news author with original article.
Customize the variable gnus-mail-reply-method to use another mailer."
  (interactive)
  (gnus-summary-reply t))

(defun gnus-summary-mail-forward ()
  "Forward the current message to another user.
Customize the variable gnus-mail-forward-method to use another mailer."
  (interactive)
  (gnus-summary-select-article t)
  (setq gnus-winconf-post-news (current-window-configuration))
  (set-buffer gnus-article-buffer)
  (let ((gnus-newsgroup-name gnus-newsgroup-name))
    (funcall gnus-mail-forward-method)))

(defun gnus-summary-mail-other-window ()
  "Compose mail in other window.
Customize the variable `gnus-mail-other-window-method' to use another
mailer."
  (interactive)
  (setq gnus-winconf-post-news (current-window-configuration))
  (let ((gnus-newsgroup-name gnus-newsgroup-name))
    (funcall gnus-mail-other-window-method)))

(defun gnus-mail-reply-using-mail (&optional yank to-address)
  (save-excursion
    (set-buffer gnus-summary-buffer)
    (let ((info (nth 2 (gnus-gethash gnus-newsgroup-name gnus-newsrc-hashtb)))
	  (group (gnus-group-real-name gnus-newsgroup-name))
	  (cur (cons (current-buffer) (cdr gnus-article-current)))
	  from subject date to reply-to message-of
	  references message-id sender follow-to cc)
      (set-buffer (get-buffer-create "*mail*"))
      (mail-mode)
      (make-local-variable 'gnus-article-reply)
      (setq gnus-article-reply cur)
      (local-set-key "\C-c\C-c" 'gnus-mail-send-and-exit)
      (if (and (buffer-modified-p)
	       (> (buffer-size) 0)
	       (not (gnus-y-or-n-p 
		     "Unsent article being composed; erase it? ")))
	  ()
	(erase-buffer)
	(save-excursion
	  (set-buffer gnus-article-buffer)
	  (let ((buffer-read-only nil))
	    (goto-char (point-min))
	    (narrow-to-region (point-min)
			      (progn (search-forward "\n\n") (point)))
	    (add-text-properties (point-min) (point-max) '(invisible nil)))
	  (if (and (boundp 'gnus-reply-to-function)
		   gnus-reply-to-function)
	      (save-excursion
		(save-restriction
		  (gnus-narrow-to-headers)
		  (setq follow-to (funcall gnus-reply-to-function group)))))
	  (setq from (mail-fetch-field "from"))
	  (setq date (mail-fetch-field "date"))
	  (and from
	       (let ((stop-pos 
		      (string-match "  *at \\|  *@ \\| *(\\| *<" from)))
		 (setq message-of
		       (concat (if stop-pos (substring from 0 stop-pos) from)
			       "'s message of " date))))
	  (setq sender (mail-fetch-field "sender"))
	  (setq subject (or (mail-fetch-field "subject")
			    "Re: none"))
	  (or (string-match "^[Rr][Ee]:" subject)
	      (setq subject (concat "Re: " subject)))
	  (setq cc (mail-fetch-field "cc"))
	  (setq reply-to (mail-fetch-field "reply-to"))
	  (setq references (mail-fetch-field "references"))
	  (setq message-id (mail-fetch-field "message-id"))
	  (widen))
	(setq news-reply-yank-from from)
	(setq news-reply-yank-message-id message-id)
	(mail-setup (or to-address follow-to reply-to from sender "") 
		    subject message-of nil gnus-article-buffer nil)
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
	  (fill-region-as-paragraph begin (1+ (point))))
	(goto-char (point-min))
	(search-forward (concat "\n" mail-header-separator "\n"))
	(if yank
	    (let ((last (point)))
	      (run-hooks 'news-reply-header-hook)
	      (mail-yank-original nil)
	      (goto-char last))))
      (if (not yank)
	  (let ((mail (current-buffer)))
	    (switch-to-buffer gnus-article-buffer)
	    (delete-other-windows)
	    (switch-to-buffer-other-window mail))
	(delete-other-windows)
	(switch-to-buffer (current-buffer))))))

(defun gnus-mail-yank-original ()
  (interactive)
  (run-hooks 'news-reply-header-hook)
  (mail-yank-original nil))

(defun gnus-mail-send-and-exit ()
  (interactive)
  (let ((cbuf (current-buffer)))
    (mail-send-and-exit nil)
    (if (get-buffer gnus-group-buffer)
	(progn
	  (save-excursion
	    (set-buffer cbuf)
	    (let ((reply gnus-article-reply))
	      (if (and reply
		       (get-buffer (car reply))
		       (buffer-name (car reply)))
		  (progn
		    (set-buffer (car reply))
		    (and (cdr reply)
			 (gnus-summary-mark-article-as-replied 
			  (cdr reply)))))))
	  (and gnus-winconf-post-news
	       (set-window-configuration gnus-winconf-post-news))
	  (setq gnus-winconf-post-news nil)))))

(defun gnus-mail-forward-using-mail ()
  "Forward the current message to another user using mail."
  ;; This is almost a carbon copy of rmail-forward in rmail.el.
  (let ((forward-buffer (current-buffer))
	(subject
	 (concat "[" gnus-newsgroup-name "] "
		 (or (gnus-fetch-field "Subject") "")))
	beg)
    ;; If only one window, use it for the mail buffer.
    ;; Otherwise, use another window for the mail buffer
    ;; so that the Rmail buffer remains visible
    ;; and sending the mail will get back to it.
    (if (if (one-window-p t)
	    (mail nil nil subject)
	  (mail-other-window nil nil subject))
	(save-excursion
	  (setq beg (goto-char (point-max)))
	  (insert "------- Start of forwarded message -------\n")
	  (insert-buffer forward-buffer)
	  (goto-char (point-max))
	  (insert "------- End of forwarded message -------\n")
	  ;; Suggested by Sudish Joseph <joseph@cis.ohio-state.edu>. 
	  (goto-char beg)
	  (while (setq beg (next-single-property-change (point) 'invisible))
	    (goto-char beg)
	    (delete-region beg (or (next-single-property-change 
				    (point) 'invisible)
				   (point-max))))
	  ;; You have a chance to arrange the message.
	  (run-hooks 'gnus-mail-forward-hook)))))

(defun gnus-mail-other-window-using-mail ()
  "Compose mail other window using mail."
  (news-mail-other-window)
  (gnus-overload-functions))


;;;
;;; Dribble file
;;;

(defvar gnus-dribble-ignore nil)

(defun gnus-dribble-file-name ()
  (concat gnus-startup-file "-dribble"))

(defun gnus-dribble-open ()
  (save-excursion 
    (set-buffer 
     (setq gnus-dribble-buffer (find-file-noselect (gnus-dribble-file-name))))
    (buffer-disable-undo (current-buffer))
    (bury-buffer gnus-dribble-buffer)
    (auto-save-mode t)
    (goto-char (point-max))))

(defun gnus-dribble-enter (string)
  (if (and (not gnus-dribble-ignore)
	   gnus-dribble-buffer
	   (buffer-name gnus-dribble-buffer))
      (let ((obuf (current-buffer)))
	(set-buffer gnus-dribble-buffer)
	(insert string "\n")
	(set-window-point (get-buffer-window (current-buffer)) (point-max))
	(set-buffer obuf))))

(defun gnus-dribble-read-file ()
  (let ((dribble-file (gnus-dribble-file-name)))
    (save-excursion 
      (set-buffer (setq gnus-dribble-buffer 
			(get-buffer-create 
			 (file-name-nondirectory dribble-file))))
      (gnus-add-current-to-buffer-list)
      (erase-buffer)
      (set-visited-file-name dribble-file)
      (buffer-disable-undo (current-buffer))
      (bury-buffer (current-buffer))
      (set-buffer-modified-p nil)
      (let ((auto (make-auto-save-file-name))
	    (gnus-dribble-ignore t))
	(if (or (file-exists-p auto) (file-exists-p dribble-file))
	    (progn
	      (if (file-newer-than-file-p auto dribble-file)
		  (setq dribble-file auto))
	      (insert-file-contents dribble-file)
	      (if (not (zerop (buffer-size)))
		  (set-buffer-modified-p t))
	      (if (gnus-y-or-n-p 
		   "Auto-save file exists. Do you want to read it? ")
		  (progn
		    (message "Reading %s..." dribble-file) 
		    (eval-current-buffer)
		    (message "Reading %s...done" dribble-file)))))))))

(defun gnus-dribble-delete-file ()
  (if (file-exists-p (gnus-dribble-file-name))
      (delete-file (gnus-dribble-file-name)))
  (if gnus-dribble-buffer
      (save-excursion
	(set-buffer gnus-dribble-buffer)
	(let ((auto (make-auto-save-file-name)))
	  (if (file-exists-p auto)
	      (delete-file auto))
	  (erase-buffer)
	  (set-buffer-modified-p nil)))))

(defun gnus-dribble-save ()
  (if (and gnus-dribble-buffer
	   (buffer-name gnus-dribble-buffer))
      (save-excursion
	(set-buffer gnus-dribble-buffer)
	(save-buffer))))

(defun gnus-dribble-clear ()
  (save-excursion
    (if (and gnus-dribble-buffer
	     (buffer-name (get-buffer gnus-dribble-buffer)))
	(progn
	  (set-buffer gnus-dribble-buffer)
	  (erase-buffer)
	  (set-buffer-modified-p nil)
	  (setq buffer-saved-size (buffer-size))))))

;;;
;;; Server Communication
;;;

(defun gnus-start-news-server (&optional confirm)
  "Open a method for getting news.
If CONFIRM is non-nil, the user will be asked for an NNTP server."
  (let (how where)
    (if gnus-current-select-method
	;; Stream is already opened.
	nil
      ;; Open NNTP server.
      (if (null gnus-nntp-service) (setq gnus-nntp-server nil))
      (if confirm
	  (progn
	    ;; Read server name with completion.
	    (setq gnus-nntp-server
		  (completing-read "NNTP server: "
				   (mapcar (lambda (server) (list server))
					   (cons (list gnus-nntp-server)
						 gnus-secondary-servers))
				   nil nil gnus-nntp-server))))

      (if (and gnus-nntp-server 
	       (stringp gnus-nntp-server)
	       (not (string= gnus-nntp-server "")))
	  (setq gnus-select-method
		(cond ((or (string= gnus-nntp-server "")
			   (string= gnus-nntp-server "::"))
		       (list 'nnspool (system-name)))
		      ((string-match ":" gnus-nntp-server)
		       (list 'nnmh gnus-nntp-server))
		      (t
		       (list 'nntp gnus-nntp-server)))))

      (setq how (car gnus-select-method))
      (setq where (car (cdr gnus-select-method)))
      (cond ((eq how 'nnspool)
	     (require 'nnspool)
	     (message "Looking up local news spool..."))
	    ((eq how 'nnmh)
	     (require 'nnmh)
	     (message "Looking up mh spool..."))
	    (t
	     (require 'nntp)))
      (setq gnus-current-select-method gnus-select-method)
      (run-hooks 'gnus-open-server-hook)
      (or 
       ;; gnus-open-server-hook might have opened it
       (gnus-server-opened gnus-select-method)  
       (gnus-open-server gnus-select-method)
       (error "%s" (gnus-nntp-message 
		    (format "Cannot open NNTP server on %s" 
			    where))))
      gnus-select-method)))

(defun gnus-check-news-server (method)
  "If the news server is down, start it up again."
  (let ((method (if method method gnus-select-method)))
    (if (gnus-server-opened method)
	;; Stream is already opened.
	t
      ;; Open server.
      (message "Opening server %s on %s..." (car method) (nth 1 method))
      (run-hooks 'gnus-open-server-hook)
      (or (gnus-server-opened method)
	  (gnus-open-server method))
      (message ""))))

(defun gnus-nntp-message (&optional message)
  "Check the status of the NNTP server.
If the status of the server is clear and MESSAGE is non-nil, MESSAGE
is returned insted of the status string."
  (let ((status (gnus-status-message (gnus-find-method-for-group 
				      gnus-newsgroup-name)))
	(message (or message "")))
    (if (and (stringp status) (> (length status) 0))
	status message)))

(defun gnus-get-function (method function)
  (let ((func (intern (format "%s-%s" (car method) function))))
    (if (not (fboundp func)) 
	(progn
	  (require (car method))
	  (if (not (fboundp func)) 
	      (error "No such function: %s" func))))
    func))

;; Specifying port number suggested by Stephane Laveau <laveau@corse.inria.fr>.
(defun gnus-open-server (method)
  (apply (gnus-get-function method 'open-server) (cdr method)))

(defun gnus-close-server (method)
  (funcall (gnus-get-function method 'close-server) (nth 1 method)))

(defun gnus-request-list (method)
  (funcall (gnus-get-function method 'request-list) (nth 1 method)))

(defun gnus-request-list-newsgroups (method)
  (funcall (gnus-get-function method 'request-list-newsgroups) (nth 1 method)))

(defun gnus-request-newgroups (date method)
  (funcall (gnus-get-function method 'request-newgroups) 
	   date (nth 1 method)))

(defun gnus-server-opened (method)
  (funcall (gnus-get-function method 'server-opened) (nth 1 method)))

(defun gnus-status-message (method)
  (funcall (gnus-get-function method 'status-message) (nth 1 method)))

(defun gnus-request-group (group &optional dont-check)
  (let ((method (gnus-find-method-for-group group)))
    (funcall (gnus-get-function method 'request-group) 
	     (gnus-group-real-name group) (nth 1 method) dont-check)))

(defun gnus-close-group (group)
  (let ((method (gnus-find-method-for-group group)))
    (funcall (gnus-get-function method 'close-group) 
	     (gnus-group-real-name group) (nth 1 method))))

(defun gnus-retrieve-headers (articles group)
  (let ((method (gnus-find-method-for-group group)))
    (funcall (gnus-get-function method 'retrieve-headers) 
	     articles (gnus-group-real-name group) (nth 1 method))))

(defun gnus-request-article (article group buffer)
  (let ((method (gnus-find-method-for-group group)))
    (funcall (gnus-get-function method 'request-article) 
	     article (gnus-group-real-name group) (nth 1 method) buffer)))

(defun gnus-request-head (article group)
  (let ((method (gnus-find-method-for-group group)))
    (funcall (gnus-get-function method 'request-head) 
	     article (gnus-group-real-name group) (nth 1 method))))

;; Fix by Sudish Joseph <joseph@cis.ohio-state.edu>.
(defun gnus-request-post-buffer (post header artbuf)
   (let* ((group gnus-newsgroup-name)
	  (info (nth 2 (gnus-gethash group gnus-newsrc-hashtb)))
	  (method
	   (if (and gnus-post-method
		    ;; Fix by Sudish Joseph <joseph@cis.ohio-state.edu>.
 		    (memq 'post (assoc
 				 (format "%s" (car (gnus-find-method-for-group
						    gnus-newsgroup-name)))
					gnus-valid-select-methods)))
	       gnus-post-method
	     (gnus-find-method-for-group gnus-newsgroup-name))))
    (funcall (gnus-get-function method 'request-post-buffer) 
	     post header artbuf (gnus-group-real-name group) info)))

(defun gnus-request-post (method &optional force)
  (and (not force) gnus-post-method
       (memq 'post (assoc (format "%s" (car method))
 			  gnus-valid-select-methods))
       (setq method gnus-post-method))
  (funcall (gnus-get-function method 'request-post) 
	   (nth 1 method)))

(defun gnus-request-expire-articles (articles group &optional force)
  (let ((method (gnus-find-method-for-group group)))
    (funcall (gnus-get-function method 'request-expire-articles) 
	     articles (gnus-group-real-name group) (nth 1 method)
	     force)))

(defun gnus-request-move-article (article group server accept-function)
  (let ((method (gnus-find-method-for-group group)))
    (funcall (gnus-get-function method 'request-move-article) 
	     article (gnus-group-real-name group) 
	     (nth 1 method) accept-function)))

(defun gnus-request-accept-article (group)
  (let ((func (if (symbolp group) group
		(car (gnus-find-method-for-group group)))))
    (funcall (intern (format "%s-request-accept-article" func))
	     (if (stringp group) (gnus-group-real-name group)
	       group))))

(defun gnus-request-replace-article (article group buffer)
  (let ((func (car (gnus-find-method-for-group group))))
    (funcall (intern (format "%s-request-replace-article" func))
	     article (gnus-group-real-name group) buffer)))

(defun gnus-find-method-for-group (group)
  (let ((info (nth 2 (gnus-gethash group gnus-newsrc-hashtb))))
    (if (or (not info)
	    (not (nth 4 info)))
	gnus-select-method
      (nth 4 info))))

(defun gnus-check-backend-function (func group)
  (let ((method (if (stringp group) (car (gnus-find-method-for-group group))
		 group)))
    (fboundp (intern (format "%s-%s" method func)))))

(defun gnus-methods-using (method)
  (let ((valids gnus-valid-select-methods)
	outs)
    (while valids
      (if (memq method (car valids)) 
	  (setq outs (cons (car valids) outs)))
      (setq valids (cdr valids)))
    outs))

;;; 
;;; Active & Newsrc File Handling
;;;

;; Newsrc related functions.
;; Gnus internal format of gnus-newsrc-assoc:
;; (("alt.general" 3 (1 . 1))
;;  ("alt.misc"    3 ((1 . 10) (12 . 15)))
;;  ("alt.test"    7 (1 . 99) (45 57 93)) ...)
;; The first item is the group name; the second is the subscription
;; level; the third is either a range of a list of ranges of read
;; articles, the optional fourth element is a list of marked articles,
;; the optional fifth element is the select method.
;;
;; Gnus internal format of gnus-newsrc-hashtb:
;; (95 ("alt.general" 3 (1 . 1)) ("alt.misc" 3 ((1 . 10) (12 . 15))) ...)
;; This is the entry for "alt.misc". The first element is the number
;; of unread articles in "alt.misc". The cdr of this entry is the
;; element *before* "alt.misc" in gnus-newsrc-assoc, which makes is
;; trivial to remove or add new elements into gnus-newsrc-assoc
;; without scanning the entire list. So, to get the actual information
;; of "alt.misc", you'd say something like 
;; (nth 2 (gnus-gethash "alt.misc" gnus-newsrc-hashtb))
;;
;; Gnus internal format of gnus-active-hashtb:
;; ((1 . 1))
;;  (5 . 10))
;;  (67 . 99)) ...)
;; The only element in each entry in this hash table is a range of
;; (possibly) available articles. (Articles in this range may have
;; been expired or cancelled.)
;;
;; Gnus internal format of gnus-killed-list and gnus-zombie-list:
;; ("alt.misc" "alt.test" "alt.general" ...)

(defun gnus-setup-news (&optional rawfile level)
  "Setup news information.
If RAWFILE is non-nil, the .newsrc file will also be read.
If LEVEL is non-nil, the news will be set up at level LEVEL."
  (let ((init (not (and gnus-newsrc-assoc gnus-active-hashtb (not rawfile)))))
    ;; Clear some variables to re-initialize news information.
    (if init (setq gnus-newsrc-assoc nil gnus-active-hashtb nil))
    ;; Read the active file and create `gnus-active-hashtb'.
    ;; If `gnus-read-active-file' is nil, then we just create an empty
    ;; hash table. The partial filling out of the hash table will be
    ;; done in `gnus-get-unread-articles'.
    (if (and gnus-read-active-file (not level))
	(gnus-read-active-file)
      (setq gnus-active-hashtb (make-vector 4095 0)))

    ;; Read the newsrc file and create `gnus-newsrc-hashtb'.
    (if init (gnus-read-newsrc-file rawfile))
    ;; Find the number of unread articles in each non-dead group.
    (gnus-get-unread-articles (or level 6))
    ;; Find new newsgroups and treat them.
    (if (and init gnus-check-new-newsgroups gnus-read-active-file (not level))
	(gnus-find-new-newsgroups))
    (if (and init gnus-check-bogus-newsgroups 
	     gnus-read-active-file (not level))
	(gnus-check-bogus-newsgroups))))

(defun gnus-find-new-newsgroups ()
  "Search for new newsgroups and add them.
Each new newsgroup will be treated with `gnus-subscribe-newsgroup-method.'
The `-n' option line from .newsrc is respected."
  (interactive)
  (or (gnus-check-first-time-used)
      (if (or (consp gnus-check-new-newsgroups)
	      (eq gnus-check-new-newsgroups 'ask-server))
	  (gnus-ask-server-for-new-groups)
	(let ((groups 0)
	      group new-newsgroups)
	  (or gnus-have-read-active-file (gnus-read-active-file))
	  (setq gnus-newsrc-last-checked-date (current-time-string))
	  (if (not gnus-killed-hashtb) (gnus-make-hashtable-from-killed))
	  ;; Go though every newsgroup in `gnus-active-hashtb' and compare
	  ;; with `gnus-newsrc-hashtb' and `gnus-killed-hashtb'.
	  (mapatoms
	   (lambda (sym)
	     (setq group (symbol-name sym))
	     (if (or (gnus-gethash group gnus-killed-hashtb)
		     (gnus-gethash group gnus-newsrc-hashtb))
		 ()
	       (if (and gnus-newsrc-options-n-yes
			(string-match gnus-newsrc-options-n-yes group))
		   (progn
		     (setq groups (1+ groups))
		     (gnus-sethash group group gnus-killed-hashtb)
		     (funcall gnus-subscribe-options-newsgroup-method group))
		 (if (or (null gnus-newsrc-options-n-no)
			 (not (string-match gnus-newsrc-options-n-no group)))
		     ;; Add this group.
		     (progn
		       (setq groups (1+ groups))
		       (gnus-sethash group group gnus-killed-hashtb)
		       (if gnus-subscribe-hierarchical-interactive
			   (setq new-newsgroups (cons group new-newsgroups))
			 (funcall gnus-subscribe-newsgroup-method group)))))))
	   gnus-active-hashtb)
	  (if new-newsgroups 
	      (gnus-subscribe-hierarchical-interactive new-newsgroups))
	  ;; Suggested by Per Abrahamsen <amanda@iesd.auc.dk>.
	  (if (> groups 0)
	      (message "%d new newsgroup%s arrived." 
		       groups (if (> groups 1) "s have" " has")))))))

(defun gnus-ask-server-for-new-groups ()
  (let* ((date (timezone-parse-date (or gnus-newsrc-last-checked-date
					(current-time-string))))
	 (methods (cons gnus-select-method 
			(append
			 (and (consp gnus-check-new-newsgroups)
			      gnus-check-new-newsgroups)
			 gnus-secondary-select-methods)))
	 (time-string
	  (format "%s%02d%02d %s%s%s"
		  (substring (aref date 0) 2) (string-to-int (aref date 1)) 
		  (string-to-int (aref date 2)) (substring (aref date 3) 0 2)
		  (substring (aref date 3) 3 5) (substring (aref date 3) 6 8)))
	 (groups 0)
	 (new-date (current-time-string))
	 hashtb group new-newsgroups)
    ;; Go thorugh both primary and secondary select methods and
    ;; request new newsgroups.  
    (while methods
      (if (gnus-request-newgroups time-string (car methods))
	  (save-excursion
	    (or hashtb (setq hashtb (gnus-make-hashtable 
				     (count-lines (point-min) (point-max)))))
	    (set-buffer nntp-server-buffer)
	    ;; Enter all the new groups in a hashtable.
	    (gnus-active-to-gnus-format (car methods) hashtb)))
      (setq methods (cdr methods)))
    ;; Now all new groups from all select methods are in `hashtb'.
    (mapatoms
     (lambda (group-sym)
       (setq group (symbol-name group-sym))
       (if (gnus-gethash group gnus-active-hashtb)
	   ()
	 (gnus-sethash group (symbol-value group-sym) gnus-active-hashtb)
	 (if (and gnus-newsrc-options-n-yes
		  (string-match gnus-newsrc-options-n-yes group))
	     (progn
	       (setq groups (1+ groups))
	       (funcall gnus-subscribe-options-newsgroup-method group))
	   (if (or (null gnus-newsrc-options-n-no)
		   (not (string-match gnus-newsrc-options-n-no group)))
	       ;; Add this group.
	       (progn
		 (setq groups (1+ groups))
		 (if gnus-subscribe-hierarchical-interactive
		     (setq new-newsgroups (cons group new-newsgroups))
		   (funcall gnus-subscribe-newsgroup-method group)))))))
     hashtb)
    (if new-newsgroups 
	(gnus-subscribe-hierarchical-interactive new-newsgroups))
    (setq gnus-newsrc-last-checked-date new-date)
    ;; Suggested by Per Abrahamsen <amanda@iesd.auc.dk>.
    (if (> groups 0)
	(message "%d new newsgroup%s arrived." 
		 groups (if (> groups 1) "s have" " has")))))

(defun gnus-check-first-time-used ()
  (if (or (file-exists-p gnus-startup-file)
	  (file-exists-p (concat gnus-startup-file ".el"))
	  (file-exists-p (concat gnus-startup-file ".eld")))
      nil
    (message "First time user; subscribing you to default groups")
    (or gnus-have-read-active-file (gnus-read-active-file))
    (setq gnus-newsrc-last-checked-date (current-time-string))
    (let ((groups gnus-default-subscribed-newsgroups)
	  group)
      (if (eq groups t)
	  nil
	(setq groups (or groups gnus-backup-default-subscribed-newsgroups))
	(mapatoms
	 (lambda (sym)
	   (setq group (symbol-name sym))
	   (if (and gnus-newsrc-options-n-yes
		    (string-match gnus-newsrc-options-n-yes group))
	       (funcall gnus-subscribe-options-newsgroup-method group)
	     (and (or (null gnus-newsrc-options-n-no)
		      (not (string-match gnus-newsrc-options-n-no group)))
		  (setq gnus-killed-list (cons group gnus-killed-list)))))
	 gnus-active-hashtb)
	(while groups
	  (if (gnus-gethash (car groups) gnus-active-hashtb)
	      (gnus-group-change-level (car groups) 3 9))
	  (setq groups (cdr groups)))
	(gnus-group-make-doc-group)
	(message
	 (substitute-command-keys "\\<gnus-group-mode-map>\\[gnus-group-list-killed] to list killed groups"))))))

;; `gnus-group-change-level' is the fundamental function for changing
;; subscription levels of newsgroups. This might mean just changing
;; from level 1 to 2, which is pretty trivial, from 2 to 6 or back
;; again, which subscribes/unsubscribes a group, which is equally
;; trivial. Changing from 1-7 to 8-9 means that you kill a group, and
;; from 8-9 to 1-7 means that you remove the group from the list of
;; killed (or zombie) groups and add them to the (kinda) subscribed
;; groups. And last but not least, moving from 8 to 9 and 9 to 8,
;; which is trivial.
;; ENTRY can either be a string (newsgroup name) or a list (if
;; FROMKILLED is t, it's a list on the format (NUM INFO-LIST),
;; otherwise it's a list in the format of the `gnus-newsrc-hashtb'
;; entries. 
;; LEVEL is the new level of the group, OLDLEVEL is the old level and
;; PREVIOUS is the group (in hashtb entry format) to insert this group
;; after. 
(defun gnus-group-change-level (entry level &optional oldlevel
				      previous fromkilled)
  (let (group info active num)
    ;; Glean what info we can from the arguments
    (if (consp entry)
	(if fromkilled (setq group (nth 1 entry))
	  (setq group (car (nth 2 entry))))
      (setq group entry))
    (if (and (stringp entry)
	     oldlevel 
	     (< oldlevel 8))
	(setq entry (gnus-gethash entry gnus-newsrc-hashtb)))
    (if (and (not oldlevel)
	     (listp entry))
	(setq oldlevel (car (cdr (nth 2 entry)))))
    (if (stringp previous)
	(setq previous (gnus-gethash previous gnus-newsrc-hashtb)))

    (gnus-dribble-enter
     (format "(gnus-group-change-level %S %S %S %S %S)" 
	     group level oldlevel (car (nth 2 previous)) fromkilled))
    
    ;; Then we remove the newgroup from any old structures, if needed.
    ;; If the group was killed, we remove it from the killed or zombie
    ;; list. If not, and it is in fact going to be killed, we remove
    ;; it from the newsrc hash table and assoc.
    (cond ((>= oldlevel 8)
	   (if (= oldlevel 8)
	       (setq gnus-zombie-list (delete group gnus-zombie-list))
	     (setq gnus-killed-list (delete group gnus-killed-list))))
	  (t
	   (if (>= level 8)
	       (progn
		 (gnus-sethash (car (nth 2 entry))
			       nil gnus-newsrc-hashtb)
		 (if (nth 3 entry)
		     (setcdr (gnus-gethash (car (nth 3 entry))
					   gnus-newsrc-hashtb)
			     (cdr entry)))
		 (setcdr (cdr entry) (cdr (cdr (cdr entry))))))))

    ;; Finally we enter (if needed) the list where it is supposed to
    ;; go, and change the subscription level. If it is to be killed,
    ;; we enter it into the killed or zombie list.
    (cond ((>= level 8)
	   (if (= level 8)
	       (setq gnus-zombie-list (cons group gnus-zombie-list))
	     (setq gnus-killed-list (cons group gnus-killed-list))))
	  (t
	   ;; If the list is to be entered into the newsrc assoc, and
	   ;; it was killed, we have to create an entry in the newsrc
	   ;; hashtb format and fix the pointers in the newsrc assoc.
	   (if (>= oldlevel 8)
	       (progn
		 (if (listp entry)
		     (progn
		       (setq info (cdr entry))
		       (setq num (car entry)))
		   (setq active (gnus-gethash group gnus-active-hashtb))
		   (setq num (- (1+ (cdr active)) (car active)))
		   ;; Check whether the group is foreign. If so, the
		   ;; foreign select method has to be entered into the
		   ;; info. 
		   (let ((method (gnus-group-method-name group)))
		     (if (eq method gnus-select-method)
			 (setq info (list group level 
					  (cons 1 (1- (car active)))))
		       (setq info (list group level (cons 1 (1- (car active)))
					nil method)))))
		 (setq entry (cons info (if previous (cdr (cdr previous))
					  (cdr gnus-newsrc-assoc))))
		 (setcdr (if previous (cdr previous) gnus-newsrc-assoc)
			 entry)
		 (gnus-sethash group (cons num (if previous (cdr previous)
						 gnus-newsrc-assoc))
			       gnus-newsrc-hashtb)
		 (if (cdr entry)
		     (setcdr (gnus-gethash (car (car (cdr entry)))
					   gnus-newsrc-hashtb)
			     entry)))
	     ;; It was alive, and it is going to stay alive, so we
	     ;; just change the level and don't change any pointers or
	     ;; hash table entries.
	     (setcar (cdr (car (cdr (cdr entry)))) level))))))

(defun gnus-kill-newsgroup (newsgroup)
  "Obsolete function. Kills a newsgroup."
  (gnus-group-change-level (gnus-gethash newsgroup gnus-newsrc-hashtb) 9))

(defun gnus-check-bogus-newsgroups (&optional confirm)
  "Remove bogus newsgroups.
If CONFIRM is non-nil, the user has to confirm the deletion of every
newsgroup." 
  (let ((newsrc (cdr gnus-newsrc-assoc))
	bogus group)
    (message "Checking bogus newsgroups...")
    (or gnus-have-read-active-file (gnus-read-active-file))
    ;; Find all bogus newsgroup that are subscribed.
    (while newsrc
      (setq group (car (car newsrc)))
      (if (or (gnus-gethash group gnus-active-hashtb)
	      (nth 4 (car newsrc))
	      (and confirm
		   (not (gnus-y-or-n-p
			 (format "Remove bogus newsgroup: %s " group)))))
	  ;; Active newsgroup.
	  ()
	;; Found a bogus newsgroup.
	(setq bogus (cons group bogus)))
      (setq newsrc (cdr newsrc)))
    ;; Remove all bogus subscribed groups by first killing them, and
    ;; then removing them from the list of killed groups.
    (while bogus
      (gnus-group-change-level 
       (gnus-gethash (car bogus) gnus-newsrc-hashtb) 9)
      (setq gnus-killed-list (delete (car bogus) gnus-killed-list))
      (setq bogus (cdr bogus)))
    ;; Then we remove all bogus groups from the list of killed and
    ;; zombie groups. They are are removed without confirmation.
    (let ((dead-lists '(gnus-killed-list gnus-zombie-list))
	  killed)
      (while dead-lists
	(setq killed (symbol-value (car dead-lists)))
	(while killed
	  (setq group (car killed))
	  (or (gnus-gethash group gnus-active-hashtb)
	      ;; The group is bogus.
	      (set (car dead-lists)
		   (delete group (symbol-value (car dead-lists)))))
	  (setq killed (cdr killed)))
	(setq dead-lists (cdr dead-lists))))
    ;; While we're at it, we check the killed list for duplicates.
    ;; This has nothing to do with bogosity, but it's a convenient
    ;; place to put the check.
    (let ((killed gnus-killed-list))
      (while killed
	(message "%d" (length killed))
	(setcdr killed (delete (car killed) (cdr killed)))
	(setq killed (cdr killed))))
    (message "Checking bogus newsgroups... done")))

;; Go though `gnus-newsrc-assoc' and compare with `gnus-active-hashtb'
;; and compute how many unread articles there are in each group.
(defun gnus-get-unread-articles (&optional level)
  (let ((newsrc (cdr gnus-newsrc-assoc))
	(level (or level 6))
	info group active)
    (message "Checking new news...")
    (while newsrc
      (setq info (car newsrc))
      (setq group (car info))
      (setq active (gnus-gethash group gnus-active-hashtb))

      ;; Check newsgroups. If the user doesn't want to check them, or
      ;; they can't be checked (for instance, if the news server can't
      ;; be reached) we just set the number of unread articles in this
      ;; newsgroup to t. This means that Gnus thinks that there are
      ;; unread articles, but it has no idea how many.
      (if (nth 4 info)
	  (if (or (and gnus-activate-foreign-newsgroups 
		       (not (numberp gnus-activate-foreign-newsgroups)))
		  (and (numberp gnus-activate-foreign-newsgroups)
		       (<= (nth 1 info) gnus-activate-foreign-newsgroups)
		       (<= (nth 1 info) level)))
	      (or (eq (car (nth 4 info)) 'nnvirtual)
		  (setq active (gnus-activate-newsgroup (car info)))))
	(if (and (not gnus-read-active-file)
		 (<= (nth 1 info) level))
	    (progn
	      (setq active (gnus-activate-newsgroup (car info))))))
      
      (or active (progn (gnus-sethash group nil gnus-active-hashtb)
			(setcar (gnus-gethash group gnus-newsrc-hashtb) t)))
      (and active (gnus-get-unread-articles-in-group info active))
      (setq newsrc (cdr newsrc)))
    (message "Checking new news... done")))

;; Create a hash table out of the newsrc alist. The `car's of the
;; alist elements are used as keys.
(defun gnus-make-hashtable-from-newsrc-alist ()
  (let ((alist gnus-newsrc-assoc)
	 prev)
    (setq gnus-newsrc-hashtb (gnus-make-hashtable (length alist)))
    (setq alist 
	  (setq prev (setq gnus-newsrc-assoc 
			   (cons (list "dummy.group" 0 (cons 0 0)) alist))))
    (while alist
      (gnus-sethash (car (car alist)) (cons nil prev) gnus-newsrc-hashtb)
      (setq prev alist)
      (setq alist (cdr alist)))))

(defun gnus-make-hashtable-from-killed ()
  "Create a hash table from the killed and zombie lists."
  (let ((lists '(gnus-killed-list gnus-zombie-list))
	list)
    (setq gnus-killed-hashtb 
	  (gnus-make-hashtable 
	   (+ (length gnus-killed-list) (length gnus-zombie-list))))
    (while lists
      (setq list (symbol-value (car lists)))
      (setq lists (cdr lists))
      (while list
	(gnus-sethash (car list) (car list) gnus-killed-hashtb)
	(setq list (cdr list))))))

(defun gnus-get-unread-articles-in-group (info active)
  (let* ((range (nth 2 info))
	 (num 0)
	 (marked (nth 3 info))
	 srange lowest group)
    ;; Modify the list of read articles according to what articles 
    ;; are available; then tally the unread articles and add the
    ;; number to the group hash table entry.
    (cond ((zerop (cdr active))
	   (setq num 0))
	  ((not range)
	   (setq num (- (1+ (cdr active)) (car active))))
	  ((atom (car range))
	   ;; Fix a single (num . num) range according to the
	   ;; active hash table.
	   (if (< (cdr range) (car active)) (setcdr range (car active)))
	   ;; Compute number of unread articles.
	   (setq num (max 0 (- (cdr active) 
			       (- (1+ (cdr range)) (car range))))))
	  (t
	   ;; The read list is a list of ranges. Fix them according to
	   ;; the active hash table.
	   (setq srange range)
	   (setq lowest (1- (car active)))
	   (while (and (< (cdr (car srange)) lowest))
	     (if (and (cdr srange)
		      (<= (cdr (car srange)) (1+ lowest)))
		 (progn
		   (setcdr (car srange) (cdr (car (cdr srange))))
		   (setcdr srange (cdr (cdr srange))))
	       (setcdr (car srange) lowest)))
	   ;; Compute the number of unread articles.
	   (while range
	     (setq num (+ num (- (1+ (cdr (car range))) 
				 (car (car range)))))
	     (setq range (cdr range)))
	   (setq num (max 0 (- (cdr active) num)))))
    (and info
	 (setcar (gnus-gethash (car info) gnus-newsrc-hashtb) 
		 (max 0 (- num (length (cdr (assq 'tick marked)))
			   (length (cdr (assq 'dormant marked)))))))
    num))

(defun gnus-activate-newsgroup (group)
  (let ((method (gnus-find-method-for-group group))
	active)
    (or (gnus-server-opened method) (gnus-open-server method))
    (and (gnus-request-group group)
	 (save-excursion
	   (set-buffer nntp-server-buffer)
	   (goto-char 1)
	   (and (looking-at "[0-9]+ [0-9]+ \\([0-9]+\\) [0-9]+")
		(progn
		  (goto-char (match-beginning 1))
		  (gnus-sethash 
		   group (setq active (cons (read (current-buffer))
					    (read (current-buffer))))
		   gnus-active-hashtb)))))
    active))

(defun gnus-update-read-articles 
  (group unread unselected ticked &optional domarks replied expirable killed
	 dormant bookmark score)
  "Update the list of read and ticked articles in GROUP using the
UNREAD and TICKED lists.
Note: UNSELECTED has to be sorted over `<'.
Returns whether the updating was successful."
  (let* ((active (gnus-gethash group gnus-active-hashtb))
	 (entry (gnus-gethash group gnus-newsrc-hashtb))
	 (number (car entry))
	 (info (nth 2 entry))
	 (marked (nth 3 info))
	 (prev 1)
	 (unread (sort (copy-sequence unread) (function <)))
	 last read)
    (if (or (not info) (not active))
	;; There is no info on this group if it was, in fact,
	;; killed. Gnus stores no information on killed groups, so
	;; there's nothing to be done. 
	;; One could store the information somewhere temporarily,
	;; perhaps... Hmmm... 
	()
      ;; Remove any negative articles numbers.
      (while (and unread (< (car unread) 0))
	(setq unread (cdr unread)))
      (if (not (and (numberp number) (zerop number)))
	  (setq unread (nconc unselected unread)))
      ;; Set the number of unread articles in gnus-newsrc-hashtb.
      (setcar entry (max 0 (- (length unread) (length ticked) 
			      (length dormant))))
      ;; Compute the ranges of read articles by looking at the list of
      ;; unread articles.  
      (while unread
	(if (/= (car unread) prev)
	    (setq read (cons (cons prev (1- (car unread))) read)))
	(setq prev (1+ (car unread)))
	(setq unread (cdr unread)))
      (if (<= prev (cdr active))
	  (setq read (cons (cons prev (cdr active)) read)))
      ;; Enter this list into the group info.
      (setcar (cdr (cdr info)) 
	      (if (> (length read) 1) (nreverse read) (car read)))
      ;; Enter the list of ticked articles.
      (gnus-set-marked-articles 
       info ticked
       (if domarks replied (cdr (assq 'reply marked)))
       (if domarks expirable (cdr (assq 'expire marked)))
       (if domarks killed (cdr (assq 'killed marked)))
       (if domarks dormant (cdr (assq 'dormant marked)))
       (if domarks bookmark (cdr (assq 'bookmark marked)))
       (if domarks score (cdr (assq 'score marked))))
      t)))

(defun gnus-make-articles-unread (group articles)
  "Mark ARTICLES in GROUP as unread."
  (let ((info (nth 2 (gnus-gethash group gnus-newsrc-hashtb))))
    (setcar (nthcdr 2 info)
	    (gnus-remove-from-range (nth 2 info) articles))
    (gnus-group-update-group group t)))

(defun gnus-read-active-file ()
  "Get active file from NNTP server."
  (gnus-group-set-mode-line)
  (let ((methods (cons gnus-select-method gnus-secondary-select-methods)))
    (setq gnus-have-read-active-file nil)
    (while methods
      (let* ((where (nth 1 (car methods)))
	     (mesg (format "Reading active file%s via %s..."
			   (if (and where (not (zerop (length where))))
			       (concat " from " where) "")
			   (car (car methods)))))
	(message mesg)
	(if (gnus-request-list (car methods)) ; Get active 
	    (save-excursion
	      (set-buffer nntp-server-buffer)
	      (gnus-active-to-gnus-format 
	       (and gnus-have-read-active-file (car methods)))
	      (setq gnus-have-read-active-file t)
	      (message "%s...done" mesg))
	  (message "Cannot read active file from %s server." 
		   (car (car methods)))
	  (ding)))
      (setq methods (cdr methods)))))

;; rewritten by jwz based on ideas from Rick Sladkey <jrs@world.std.com>
;; Further rewrites by lmi.
(defun gnus-active-to-gnus-format (method &optional hashtb)
  "Convert active file format to internal format.
Lines matching `gnus-ignored-newsgroups' are ignored."
  (let ((cur (current-buffer))
	(hashtb (or hashtb 
		    (if method
			gnus-active-hashtb
		      (setq gnus-active-hashtb
			    (gnus-make-hashtable 
			     (count-lines (point-min) (point-max))))))))
    ;; Delete unnecessary lines.
    (goto-char (point-min))
    (delete-matching-lines gnus-ignored-newsgroups)
    (and method (not (eq method gnus-select-method))
	 (let ((prefix (gnus-group-prefixed-name "" method)))
	   (goto-char (point-min))
	   (while (and (not (eobp))
		       (null (insert prefix))
		       (zerop (forward-line 1))))))
    (goto-char (point-min))
    ;; Store active file in hashtable.
    (save-restriction
      (if (or (re-search-forward "\n.\r?$" nil t)
	      (goto-char (point-max)))
	  (progn
	    (beginning-of-line)
	    (narrow-to-region (point-min) (point))))
      (goto-char (point-min))
      (if (string-match "%[oO]" gnus-group-line-format)
	  ;; Suggested by Brian Edmonds <edmonds@cs.ubc.ca>.
	  ;; If we want information on moderated groups, we use this
	  ;; loop...   
	  (condition-case ()
	      (let ((mod-hashtb (make-vector 7 0))
		    group max mod)
		(while (not (eobp))
		  (setq group (let ((obarray hashtb))
				(read cur)))
		  (setq max (read cur))
		  (set group (cons (read cur) max))
		  ;; Enter moderated groups into a list.
		  (if (string= 
		       (symbol-name  (let ((obarray mod-hashtb)) (read cur)))
		       "m")
		      (setq gnus-moderated-list 
			    (cons (symbol-name group) gnus-moderated-list)))
		  (forward-line 1)))
	    (error 
	     (progn (ding) (message "Possible error in active file."))))
	;; And if we do not care about moderation, we use this loop,
	;; which is faster.
	(condition-case ()
	    (let (group max)
	      (while (not (eobp))
		;; group gets set to a symbol interned in the hash table
		;; (what a hack!!)
		(setq group (let ((obarray hashtb)) (read cur)))
		(setq max (read cur))
		(set group (cons (read cur) max))
		(forward-line 1)))
	  (error 
	   (progn (ding) (message "Possible error in active file."))))))))

(defun gnus-read-newsrc-file (&optional force)
  "Read startup file.
If FORCE is non-nil, the .newsrc file is read."
  (setq gnus-current-startup-file (gnus-make-newsrc-file gnus-startup-file))
  ;; Reset variables that might be defined in the .newsrc.eld file.
  (let ((variables gnus-variable-list))
    (while variables
      (set (car variables) nil)
      (setq variables (cdr variables))))
  (let* ((newsrc-file gnus-current-startup-file)
	 (quick-file (concat newsrc-file ".el")))
    (save-excursion
      ;; We always load the .newsrc.eld file. If always contains
      ;; much information that can not be gotten from the .newsrc
      ;; file (ticked articles, killed groups, foreign methods, etc.)
      (gnus-read-newsrc-el-file quick-file)
 
      (if (or force
	      (and (file-newer-than-file-p newsrc-file quick-file)
		   (file-newer-than-file-p newsrc-file 
					   (concat quick-file "d")))
	      (not gnus-newsrc-assoc))
	  ;; We read the .newsrc file. Note that if there if a
	  ;; .newsrc.eld file exists, it has already been read, and
	  ;; the `gnus-newsrc-hashtb' has been created. While reading
	  ;; the .newsrc file, Gnus will only use the information it
	  ;; can find there for changing the data already read -
	  ;; ie. reading the .newsrc file will not trash the data
	  ;; already read (except for read articles).
	  (save-excursion
	    (message "Reading %s..." newsrc-file)
	    (set-buffer (find-file-noselect newsrc-file))
	    (buffer-disable-undo (current-buffer))
	    (gnus-newsrc-to-gnus-format)
	    (kill-buffer (current-buffer))
	    (message "Reading %s... done" newsrc-file)))
      (and gnus-use-dribble-file (gnus-dribble-read-file)))))

(defun gnus-read-newsrc-el-file (file)
  (let ((ding-file (concat file "d")))
    ;; We always, always read the .eld file.
    (message "Reading %s..." ding-file)
    (condition-case nil
	(load ding-file t t t)
      (error nil))
    (gnus-make-hashtable-from-newsrc-alist)
    (if (not (file-newer-than-file-p file ding-file))
	()
      ;; Old format quick file
      (message "Reading %s..." file)
      ;; The .el file is newer than the .eld file, so we read that one
      ;; as well. 
      (gnus-read-old-newsrc-el-file file))))

;; Parse the old-style quick startup file
(defun gnus-read-old-newsrc-el-file (file)
  (let (newsrc killed marked group g m len info)
    (prog1
	(let (gnus-killed-assoc gnus-marked-assoc gnus-newsrc-assoc)
	  (prog1
	      (condition-case nil
		  (load file t t t)
		(error nil))
	    (setq newsrc gnus-newsrc-assoc
		  killed gnus-killed-assoc
		  marked gnus-marked-assoc)))
      (setq gnus-newsrc-assoc nil)
      (while newsrc
	(setq group (car newsrc))
	(let ((info (nth 2 (gnus-gethash (car group) gnus-newsrc-hashtb))))
	  (if info
	      (progn
		(setcar (nthcdr 2 info) (cdr (cdr group)))
		(setcar (cdr info) (if (nth 1 group) 3 6))
		(setq gnus-newsrc-assoc (cons info gnus-newsrc-assoc)))
	    (setq gnus-newsrc-assoc
		  (cons 
		   (setq info
			 (list (car group)
			       (if (nth 1 group) 3 6) (cdr (cdr group))))
		   gnus-newsrc-assoc)))
	  (if (setq m (assoc (car group) marked))
	    (setcdr (cdr (cdr info)) (cons (list (cons 'tick (cdr m))) nil))))
	(setq newsrc (cdr newsrc)))
      (setq newsrc killed)
      (while newsrc
	(setcar newsrc (car (car newsrc)))
	(setq newsrc (cdr newsrc)))
      (setq gnus-killed-list killed))
    (setq gnus-newsrc-assoc (nreverse gnus-newsrc-assoc))
    (gnus-make-hashtable-from-newsrc-alist)))
      
(defun gnus-make-newsrc-file (file)
  "Make server dependent file name by catenating FILE and server host name."
  (let* ((file (expand-file-name file nil))
	 (real-file (concat file "-" (nth 1 gnus-select-method))))
    (if (file-exists-p real-file)
	real-file file)
    ))

;; jwz: rewrote this function to be much more efficient, and not be subject
;; to regexp overflow errors when it encounters very long lines -- the old
;; behavior was to blow off the rest of the *file* when a line was encountered
;; that was too long to match!!  Now it uses only simple looking-at calls, and
;; doesn't create as many temporary strings.  It also now handles multiple
;; consecutive options lines (before it only handled the first.)
;; Tiny rewrite by lmi. 
(defun gnus-newsrc-to-gnus-format ()
  "Parse current buffer as .newsrc file."
  ;; We have to re-initialize these variables (except for
  ;; gnus-killed-list) because the quick startup file may contain bogus
  ;; values.
  (setq gnus-newsrc-options nil)
  (setq gnus-newsrc-options-n-yes nil)
  (setq gnus-newsrc-options-n-no nil)
  (setq gnus-newsrc-assoc nil)
  (gnus-parse-options-lines)
  (gnus-parse-newsrc-body))

(defun gnus-parse-options-lines ()
  ;; newsrc.5 seems to indicate that the options line can come anywhere
  ;; in the file, and that there can be any number of them:
  ;;
  ;;       An  options  line  starts  with  the  word  options (left-
  ;;       justified).  Then there are the list of  options  just  as
  ;;       they would be on the readnews command line.  For instance:
  ;;
  ;;       options -n all !net.sf-lovers !mod.human-nets -r
  ;;       options -c -r
  ;;
  ;;       A string of lines beginning with a space or tab after  the
  ;;       initial  options  line  will  be  considered  continuation
  ;;       lines.
  ;;
  ;; For now, we only accept it at the beginning of the file.

  (goto-char (point-min))
  (skip-chars-forward " \t\n")
  (setq gnus-newsrc-options nil)
  (while (looking-at "^options[ \t]*\\(.*\\)\n")
    ;; handle consecutive options lines
    (setq gnus-newsrc-options (concat gnus-newsrc-options
				      (if gnus-newsrc-options "\n\t")
				      (buffer-substring (match-beginning 1)
							(match-end 1))))
    (forward-line 1)
    (while (looking-at "[ \t]+\\(.*\\)\n")
      ;; handle subsequent continuation lines of this options line
      (setq gnus-newsrc-options (concat gnus-newsrc-options "\n\t"
					(buffer-substring (match-beginning 1)
							  (match-end 1))))
      (forward-line 1)))
  ;; Gather all "-n" options lines.
  (let ((start 0)
	(result nil))
    (if gnus-newsrc-options
	(while (and (string-match "^[ \t]*-n\\([^\n]*\\)$"
				  gnus-newsrc-options
				  start)
		    (setq start (match-end 0)))
	  (setq result (concat result
			       (and result " ")
			       (substring gnus-newsrc-options
					  (match-beginning 1)
					  (match-end 1))))))
    (let ((yes-and-no (and result (gnus-parse-n-options result))))
      (and (or gnus-options-subscribe (car yes-and-no))
	   (setq gnus-newsrc-options-n-yes 
		 (concat (or gnus-options-subscribe "") 
			 (or (car yes-and-no) ""))))
      (and (or gnus-options-not-subscribe (cdr yes-and-no))
	   (setq gnus-newsrc-options-n-no 
		 (concat (or gnus-options-not-subscribe "") 
			 (or (cdr yes-and-no) "")))))
    nil))

(defun gnus-parse-newsrc-body ()
  ;; Point has been positioned after the options lines.  We shouldn't
  ;; see any more in here.

  (let ((subscribe nil)
	(read-list nil)
	(line (1+ (count-lines (point-min) (point))))
	newsgroup
	p p2)
    (save-restriction
      (skip-chars-forward " \t")
      (while (not (eobp))
	(cond
	 ((= (following-char) ?\n)
	  ;; skip blank lines
	  nil)
	 (t
	  (setq p (point))
	  (skip-chars-forward "^:!\n")
	  (if (= (following-char) ?\n)
	      (error "line %d is unparsable in %s" line (buffer-name)))
	  (setq p2 (point))
	  (skip-chars-backward " \t")

	  ;; #### note: we could avoid consing a string here by binding obarray
	  ;; and reading the newsgroup directly into the gnus-newsrc-hashtb,
	  ;; then setq'ing newsgroup to symbol-name of that, like we do in
	  ;; gnus-active-to-gnus-format.
	  (setq newsgroup (buffer-substring p (point)))
	  (goto-char p2)

	  (setq subscribe (= (following-char) ?:))
	  (setq read-list nil)

	  (forward-char 1)		; after : or !
	  (skip-chars-forward " \t")
	  (while (not (= (following-char) ?\n))
	    (skip-chars-forward " \t")
	    (or
	     (and (cond
		   ((looking-at "\\([0-9]+\\)-\\([0-9]+\\)") ; a range
		    (setq read-list
			  (cons
			   (cons
			    (progn
			      ;; faster that buffer-substring/string-to-int
			      (narrow-to-region (point-min) (match-end 1))
			      (read (current-buffer)))
			    (progn
			      (narrow-to-region (point-min) (match-end 2))
			      (forward-char) ; skip over "-"
			      (prog1
				  (read (current-buffer))
				(widen))))
			   read-list))
		    t)
		   ((looking-at "[0-9]+")
		    ;; faster that buffer-substring/string-to-int
		    (narrow-to-region (point-min) (match-end 0))
		    (setq p (read (current-buffer)))
		    (widen)
		    (setq read-list (cons (cons p p) read-list))
		    t)
		   (t
		    ;; bogus chars in ranges
		    nil))
		  (progn
		    (goto-char (match-end 0))
		    (skip-chars-forward " \t")
		    (cond ((= (following-char) ?,)
			   (forward-char 1)
			   t)
			  ((= (following-char) ?\n)
			   t)
			  (t
			   ;; bogus char after range
			   nil))))
	     ;; if we get here, the parse failed
	     (progn
	       (end-of-line)		; give up on this line
	       (ding)
	       (message "Ignoring bogus line %d for %s in %s"
			line newsgroup (buffer-name))
	       (sleep-for 1))))
	  ;; We have already read .newsrc.eld, so we gently update the
	  ;; data in the hash table with the information we have just
	  ;; read. 
	  (let ((info (nth 2 (gnus-gethash newsgroup gnus-newsrc-hashtb))))
	    (if info
		(progn
		  (setcar (nthcdr 2 info) (nreverse read-list))
		  (setcar (cdr info) (if subscribe 3 (if read-list 6 7)))
		  (setq gnus-newsrc-assoc (cons info gnus-newsrc-assoc)))
	      (setq gnus-newsrc-assoc
		    (cons (list newsgroup (if subscribe 3 (if read-list 6 7))
				(nreverse read-list))
			  gnus-newsrc-assoc))))))
	(setq line (1+ line))
	(forward-line 1))))
  (setq gnus-newsrc-assoc (nreverse gnus-newsrc-assoc))
  (gnus-make-hashtable-from-newsrc-alist)
  nil)

(defun gnus-parse-n-options (options)
  "Parse -n NEWSGROUPS options and return a cons of YES and NO regexps."
  (let ((yes nil)
	(no nil)
	(yes-or-no nil)			;`!' or not.
	(newsgroup nil))
    ;; Parse each newsgroup description such as "comp.all".  Commas
    ;; and white spaces can be a newsgroup separator.
    (while
	(string-match "^[ \t\n,]*\\(!?\\)\\([^- \t\n,][^ \t\n,]*\\)" options)
      (setq yes-or-no
	    (substring options (match-beginning 1) (match-end 1)))
      (setq newsgroup
	    (regexp-quote
	     (substring options
			(match-beginning 2) (match-end 2))))
      (setq options (substring options (match-end 2)))
      ;; Rewrite "all" to ".+" not ".*".  ".+" requires at least one
      ;; character.
      (while (string-match "\\(^\\|\\\\[.]\\)all\\(\\\\[.]\\|$\\)" newsgroup)
	(setq newsgroup
	      (concat (substring newsgroup 0 (match-end 1))
		      ".+"
		      (substring newsgroup (match-beginning 2)))))
      ;; It is yes or no.
      (cond ((string-equal yes-or-no "!")
	     (setq no (cons newsgroup no)))
	    ((string-equal newsgroup ".+")) ;Ignore `all'.
	    (t
	     (setq yes (cons newsgroup yes))))
      )
    ;; Make a cons of regexps from parsing result.
    ;; We have to append \(\.\|$\) to prevent matching substring of
    ;; newsgroup.  For example, "jp.net" should not match with
    ;; "jp.network".
    ;; Fixes for large regexp problems are from yonezu@nak.math.keio.ac.jp.
    (cons (if yes
	      (concat "^\\("
		      (apply (function concat)
			     (mapcar
			      (lambda (newsgroup)
				(concat newsgroup "\\|"))
			      (cdr yes)))
		      (car yes) "\\)\\(\\.\\|$\\)"))
	  (if no
	      (concat "^\\("
		      (apply (function concat)
			     (mapcar
			      (lambda (newsgroup)
				(concat newsgroup "\\|"))
			      (cdr no)))
		      (car no) "\\)\\(\\.\\|$\\)")))
    ))

(defun gnus-save-newsrc-file ()
  "Save .newsrc file."
  ;; Note: We cannot save .newsrc file if all newsgroups are removed
  ;; from the variable gnus-newsrc-assoc.
  (and (or gnus-newsrc-assoc gnus-killed-list)
       gnus-current-startup-file
       (save-excursion
	 (if (zerop (save-excursion
		    (set-buffer gnus-dribble-buffer)
		    (buffer-size)))
	     (message "(No changes need to be saved)")
	   (if gnus-save-newsrc-file
	       (let ((make-backup-files t)
		     (version-control nil)
		     (require-final-newline t)) ;Don't ask even if requested.
		 (message "Saving %s..." gnus-current-startup-file)
		 ;; Make backup file of master newsrc.
		 ;; You can stop or change version control of backup file.
		 ;; Suggested by jason@violet.berkeley.edu.
		 (run-hooks 'gnus-save-newsrc-hook)
		 (gnus-gnus-to-newsrc-format)
		 (message "Saving %s... done" gnus-current-startup-file)))
	   ;; Quickly loadable .newsrc.
	   (set-buffer (get-buffer-create " *Gnus-newsrc*"))
	   (gnus-add-current-to-buffer-list)
	   (buffer-disable-undo (current-buffer))
	   (erase-buffer)
	   (message "Saving %s.eld..." gnus-current-startup-file)
	   (gnus-gnus-to-quick-newsrc-format)
	   (let ((make-backup-files nil)
		 (version-control nil)
		 (require-final-newline t)) ;Don't ask even if requested.
	     (write-region 1 (point-max) 
			   (concat gnus-current-startup-file ".eld") 
			   nil 'nomesg))
	   (kill-buffer (current-buffer))
	   (message "Saving %s.eld... done" gnus-current-startup-file)
	   (gnus-dribble-delete-file)))))

(defun gnus-gnus-to-quick-newsrc-format ()
  "Insert Gnus variables such as gnus-newsrc-assoc in lisp format."
  (insert ";; (ding) Gnus startup file.\n")
  (insert ";; Never delete this file - touch .newsrc instead to force Gnus\n")
  (insert ";; to read .newsrc.\n")
  (let ((variables gnus-variable-list)
	(gnus-newsrc-assoc (cdr gnus-newsrc-assoc))
	variable)
    ;; insert lisp expressions.
    (while variables
      (setq variable (car variables))
      (and (boundp variable)
	   (symbol-value variable)
	   (or gnus-save-killed-list (not (eq variable 'gnus-killed-list)))
	   (insert "(setq " (symbol-name variable) " '"
		   (prin1-to-string (symbol-value variable))
		   ")\n"))
      (setq variables (cdr variables)))))

(defun gnus-gnus-to-newsrc-format ()
  ;; Generate and save the .newsrc file.
  (let ((newsrc (cdr gnus-newsrc-assoc))
	info ranges range)
    (save-excursion
      (set-buffer (create-file-buffer gnus-startup-file))
      (buffer-disable-undo (current-buffer))
      (erase-buffer)
      ;; Write options.
      (if gnus-newsrc-options (insert "options " gnus-newsrc-options "\n"))
      ;; Write subscribed and unsubscribed.
      (while newsrc
	(setq info (car newsrc))
	(if (not (nth 4 info))		;Don't write foreign groups to .newsrc.
	    (progn
	      (insert (car info) (if (>= (nth 1 info) 6) "!" ":"))
	      (if (setq ranges (nth 2 info))
		  (progn
		    (insert " ")
		    (if (atom (car ranges))
			(if (= (car ranges) (cdr ranges))
			    (insert (int-to-string (car ranges)))
			  (insert (int-to-string (car ranges)) "-" 
				  (int-to-string (cdr ranges))))
		      (while ranges
			(setq range (car ranges)
			      ranges (cdr ranges))
			(if (= (car range) (cdr range))
			    (insert (int-to-string (car range)))
			  (insert (int-to-string (car range)) "-"
				  (int-to-string (cdr range))))
			(if ranges (insert ","))))))
	      (insert "\n")))
	(setq newsrc (cdr newsrc)))
      (write-region 1 (point-max) gnus-current-startup-file nil 'nomesg)
      (kill-buffer (current-buffer)))))

(defun gnus-read-descriptions-file ()
  (message "Reading descriptions file...")
  (if (not (gnus-request-list-newsgroups gnus-select-method))
      (progn
	(message "Couldn't read newsgroups descriptions")
	nil)
    (let (group)
      (setq gnus-description-hashtb 
	    (gnus-make-hashtable (length gnus-active-hashtb)))
      (save-excursion
	(save-restriction
	  (set-buffer nntp-server-buffer)
	  (goto-char (point-min))
	  (delete-non-matching-lines "^[a-zA-Z\\.0-9]+[ \t]")
	  (goto-char (point-min))
	  (if (or (search-forward "\n.\n" nil t)
		  (goto-char (point-max)))
	      (progn
		(beginning-of-line)
		(narrow-to-region (point-min) (point))))
	  (goto-char (point-min))
	  (while (not (eobp))
	    (setq group (let ((obarray gnus-description-hashtb))
			  (read (current-buffer))))
	    (skip-chars-forward " \t")
	    (set group (buffer-substring 
			(point) (save-excursion (end-of-line) (point))))
	    (forward-line 1))))
      (message "Reading descriptions file...done")
      t)))

(provide 'gnus)

;;; gnus.el ends here
