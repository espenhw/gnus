;;; gnus.el --- a newsreader for GNU Emacs
;; Copyright (C) 1987,88,89,90,93,94,95 Free Software Foundation, Inc.

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

;; Although Gnus looks suspiciously like GNUS, it isn't quite the same
;; beast.  Most internal structures have been changed.  If you have
;; written packages that depend on any of the hash tables,
;; `gnus-newsrc-alist', `gnus-killed-assoc', marked lists, the .newsrc
;; buffer, or internal knowledge of the `nntp-header-' macros, or
;; dependence on the buffers having a certain format, your code will
;; fail.

;;; Code:

(eval '(run-hooks 'gnus-load-hook))

(require 'mail-utils)
(require 'timezone)
(require 'nnheader)

(eval-when-compile (require 'cl))

;; Site dependent variables.  These variables should be defined in
;; paths.el.

(defvar gnus-default-nntp-server nil
  "Specify a default NNTP server.
This variable should be defined in paths.el, and should never be set
by the user.
If you want to change servers, you should use `gnus-select-method'.
See the documentation to that variable.")

(defconst gnus-backup-default-subscribed-newsgroups 
  '("news.announce.newusers" "news.groups.questions" "gnu.emacs.gnus")
  "Default default new newsgroups the first time Gnus is run.
Should be set in paths.el, and shouldn't be touched by the user.")

(defvar gnus-local-domain nil
  "Local domain name without a host name.
The DOMAINNAME environment variable is used instead if it is defined.
If the `system-name' function returns the full Internet name, there is
no need to set this variable.")

(defvar gnus-local-organization nil
  "String with a description of what organization (if any) the user belongs to.
The ORGANIZATION environment variable is used instead if it is defined.
If this variable contains a function, this function will be called
with the current newsgroup name as the argument.  The function should
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


;; Customization variables

;; Don't touch this variable.
(defvar gnus-nntp-service "nntp"
  "*NNTP service name (\"nntp\" or 119).
This is an obsolete variable, which is scarcely used.  If you use an
nntp server for your newsgroup and want to change the port number
used to 899, you would say something along these lines:

 (setq gnus-select-method '(nntp \"my.nntp.server\" (nntp-port-number 899)))")

(defvar gnus-nntpserver-file "/etc/nntpserver"
  "*A file with only the name of the nntp server in it.")

;; This function is used to check both the environment variable
;; NNTPSERVER and the /etc/nntpserver file to see whether one can find
;; an nntp server name default.
(defun gnus-getenv-nntpserver ()
  (or (getenv "NNTPSERVER")
      (and (file-readable-p gnus-nntpserver-file)
	   (save-excursion
	     (set-buffer (get-buffer-create " *gnus nntp*"))
	     (buffer-disable-undo (current-buffer))
	     (insert-file-contents gnus-nntpserver-file)
	     (let ((name (buffer-string)))
	       (prog1
		   (if (string-match "^[ \t\n]*$" name)
		       nil
		     name)
		 (kill-buffer (current-buffer))))))))
		 
(defvar gnus-select-method 
  (nconc
   (list 'nntp (or (condition-case ()
		       (gnus-getenv-nntpserver)
		     (error nil))
		   (if (and gnus-default-nntp-server
			    (not (string= gnus-default-nntp-server "")))
		       gnus-default-nntp-server)
		   (system-name)))
   (if (or (null gnus-nntp-service)
	   (equal gnus-nntp-service "nntp"))
       nil 
     (list gnus-nntp-service)))
  "*Default method for selecting a newsgroup.
This variable should be a list, where the first element is how the
news is to be fetched, the second is the address. 

For instance, if you want to get your news via NNTP from
\"flab.flab.edu\", you could say:

(setq gnus-select-method '(nntp \"flab.flab.edu\"))

If you want to use your local spool, say:

(setq gnus-select-method (list 'nnspool (system-name)))

If you use this variable, you must set `gnus-nntp-server' to nil.

There is a lot more to know about select methods and virtual servers -
see the manual for details.")

;; Added by Sudish Joseph <joseph@cis.ohio-state.edu>.
(defvar gnus-post-method nil
  "*Preferred method for posting USENET news.
If this variable is nil, Gnus will use the current method to decide
which method to use when posting.  If it is non-nil, it will override
the current method.  This method will not be used in mail groups and
the like, only in \"real\" newsgroups.

The value must be a valid method as discussed in the documentation of
`gnus-select-method'.")

(defvar gnus-refer-article-method nil
  "*Preferred method for fetching an article by Message-ID.
If you are reading news from the local spool (with nnspool), fetching
articles by Message-ID is painfully slow.  By setting this method to an
nntp method, you might get acceptable results.

The value of this variable must be a valid select method as discussed
in the documentation of `gnus-select-method'")

(defvar gnus-secondary-select-methods nil
  "*A list of secondary methods that will be used for reading news.
This is a list where each element is a complete select method (see
`gnus-select-method').  

If, for instance, you want to read your mail with the nnml backend,
you could set this variable:

(setq gnus-secondary-select-methods '((nnml \"\")))")

(defvar gnus-secondary-servers nil
  "*List of NNTP servers that the user can choose between interactively.
To make Gnus query you for a server, you have to give `gnus' a
non-numeric prefix - `C-u M-x gnus', in short.")

(defvar gnus-nntp-server nil
  "*The name of the host running the NNTP server.
This variable is semi-obsolete.  Use the `gnus-select-method'
variable instead.")

(defvar gnus-startup-file "~/.newsrc"
  "*Your `.newsrc' file.
`.newsrc-SERVER' will be used instead if that exists.")

(defvar gnus-init-file "~/.gnus"
  "*Your Gnus elisp startup file.
If a file with the .el or .elc suffixes exist, it will be read
instead.") 

(defvar gnus-group-faq-directory
  '("/ftp@mirrors.aol.com:/pub/rtfm/usenet/"
    "/ftp@ftp.uu.net:/usenet/news.answers/"
    "/ftp@ftp.seas.gwu.edu:/pub/rtfm/"
    "/ftp@rtfm.mit.edu:/pub/usenet/news.answers/"
    "/ftp@ftp.uni-paderborn.de:/pub/FAQ/"
    "/ftp@ftp.Germany.EU.net:/pub/newsarchive/news.answers/"
    "/ftp@ftp.sunet.se:/pub/usenet/"
    "/ftp@nctuccca.edu.tw:/USENET/FAQ/"
    "/ftp@hwarang.postech.ac.kr:/pub/usenet/news.answers/"
    "/ftp@ftp.hk.super.net:/mirror/faqs/")
  "*Directory where the group FAQs are stored.
This will most commonly be on a remote machine, and the file will be
fetched by ange-ftp.

This variable can also be a list of directories.  In that case, the
first element in the list will be used by default, and the others will
be used as backup sites.

Note that Gnus uses an aol machine as the default directory.  If this
feels fundamentally unclean, just think of it as a way to finally get
something of value back from them.

If the default site is too slow, try one of these:

   North America: ftp.uu.net                     /usenet/news.answers
		  mirrors.aol.com                /pub/rtfm/usenet
		  ftp.seas.gwu.edu               /pub/rtfm
                  rtfm.mit.edu                   /pub/usenet/news.answers
   Europe:        ftp.uni-paderborn.de           /pub/FAQ
		  ftp.Germany.EU.net             /pub/newsarchive/news.answers
		  ftp.sunet.se                   /pub/usenet
   Asia:          nctuccca.edu.tw                /USENET/FAQ
		  hwarang.postech.ac.kr          /pub/usenet/news.answers
		  ftp.hk.super.net               /mirror/faqs")

(defvar gnus-group-archive-directory
  "/ftp@ftp.hpc.uh.edu:/pub/emacs/ding-list/" 
  "*The address of the (ding) archives.")

(defvar gnus-group-recent-archive-directory
  "/ftp@ftp.hpc.uh.edu:/pub/emacs/ding-list-recent/"
  "*The address of the most recent (ding) articles.")

(defvar gnus-default-subscribed-newsgroups nil
  "*This variable lists what newsgroups should be subscribed the first time Gnus is used.
It should be a list of strings.
If it is `t', Gnus will not do anything special the first time it is
started; it'll just use the normal newsgroups subscription methods.")

(defvar gnus-use-cross-reference t
  "*Non-nil means that cross referenced articles will be marked as read.
If nil, ignore cross references.  If t, mark articles as read in
subscribed newsgroups.  If neither t nor nil, mark as read in all
newsgroups.") 

(defvar gnus-use-dribble-file t
  "*Non-nil means that Gnus will use a dribble file to store user updates.
If Emacs should crash without saving the .newsrc files, complete
information can be restored from the dribble file.")

(defvar gnus-dribble-directory nil
  "*The directory where dribble files will be saved.
If this variable is nil, the directory where the .newsrc files are
saved will be used.")

(defvar gnus-asynchronous nil
  "*If non-nil, Gnus will supply backends with data needed for async article fetching.")

(defvar gnus-large-newsgroup 200
  "*The number of articles which indicates a large newsgroup.
If the number of articles in a newsgroup is greater than this value,
confirmation is required for selecting the newsgroup.")

;; Suggested by Andrew Eskilsson <pi92ae@lelle.pt.hk-r.se>.
(defvar gnus-no-groups-message "No news is horrible news"
  "*Message displayed by Gnus when no groups are available.")

(defvar gnus-use-long-file-name (not (memq system-type '(usg-unix-v xenix)))
  "*Non-nil means that the default name of a file to save articles in is the group name.
If it's nil, the directory form of the group name is used instead.

If this variable is a list, and the list contains the element
`not-score', long file names will not be used for score files; if it
contains the element `not-save', long file names will not be used for
saving; and if it contains the element `not-kill', long file names
will not be used for kill files.")

(defvar gnus-article-save-directory (or (getenv "SAVEDIR") "~/News/")
  "*Name of the directory articles will be saved in (default \"~/News\").
Initialized from the SAVEDIR environment variable.")

(defvar gnus-kill-files-directory (or (getenv "SAVEDIR") "~/News/")
  "*Name of the directory where kill files will be stored (default \"~/News\").
Initialized from the SAVEDIR environment variable.")

(defvar gnus-default-article-saver 'gnus-summary-save-in-rmail
  "*A function to save articles in your favorite format.
The function must be interactively callable (in other words, it must
be an Emacs command).

Gnus provides the following functions:

* gnus-summary-save-in-rmail (Rmail format)
* gnus-summary-save-in-mail (Unix mail format)
* gnus-summary-save-in-folder (MH folder)
* gnus-summary-save-in-file (article format).
* gnus-summary-save-in-vm (use VM's folder format).")

(defvar gnus-prompt-before-saving 'always
  "*This variable says how much prompting is to be done when saving articles.
If it is nil, no prompting will be done, and the articles will be
saved to the default files.  If this variable is `always', each and
every article that is saved will be preceded by a prompt, even when
saving large batches of articles.  If this variable is neither nil not
`always', there the user will be prompted once for a file name for
each invocation of the saving commands.")

(defvar gnus-rmail-save-name (function gnus-plain-save-name)
  "*A function generating a file name to save articles in Rmail format.
The function is called with NEWSGROUP, HEADERS, and optional LAST-FILE.")

(defvar gnus-mail-save-name (function gnus-plain-save-name)
  "*A function generating a file name to save articles in Unix mail format.
The function is called with NEWSGROUP, HEADERS, and optional LAST-FILE.")

(defvar gnus-folder-save-name (function gnus-folder-save-name)
  "*A function generating a file name to save articles in MH folder.
The function is called with NEWSGROUP, HEADERS, and optional LAST-FOLDER.")

(defvar gnus-file-save-name (function gnus-numeric-save-name)
  "*A function generating a file name to save articles in article format.
The function is called with NEWSGROUP, HEADERS, and optional
LAST-FILE.")

(defvar gnus-split-methods nil
  "*Variable used to suggest where articles are to be saved.
The syntax of this variable is the same as `nnmail-split-methods'.  

For instance, if you would like to save articles related to Gnus in
the file \"gnus-stuff\", and articles related to VM in \"vm-stuff\",
you could set this variable to something like:

 '((\"^Subject:.*gnus\\|^Newsgroups:.*gnus\" \"gnus-stuff\")
   (\"^Subject:.*vm\\|^Xref:.*vm\" \"vm-stuff\"))")

(defvar gnus-save-score nil
  "*If non-nil, save group scoring info.")

(defvar gnus-use-adaptive-scoring nil
  "*If non-nil, use some adaptive scoring scheme.")

(defvar gnus-use-cache nil
  "*If non-nil, Gnus will cache (some) articles locally.")

(defvar gnus-keep-backlog nil
  "*If non-nil, Gnus will keep read articles for later re-retrieval.
If it is a number N, then Gnus will only keep the last N articles
read.  If it is neither nil nor a number, Gnus will keep all read
articles.  This is not a good idea.")

(defvar gnus-use-nocem nil
  "*If non-nil, Gnus will read NoCeM cancel messages.")

(defvar gnus-use-demon nil
  "If non-nil, Gnus might use some demons.")

(defvar gnus-use-scoring t
  "*If non-nil, enable scoring.")

(defvar gnus-fetch-old-headers nil
  "*Non-nil means that Gnus will try to build threads by grabbing old headers.
If an unread article in the group refers to an older, already read (or
just marked as read) article, the old article will not normally be
displayed in the Summary buffer.  If this variable is non-nil, Gnus
will attempt to grab the headers to the old articles, and thereby
build complete threads.  If it has the value `some', only enough
headers to connect otherwise loose threads will be displayed.
This variable can also be a number.  In that case, no more than that
number of old headers will be fetched. 

The server has to support NOV for any of this to work.")

;see gnus-cus.el
;(defvar gnus-visual t
;  "*If non-nil, will do various highlighting.
;If nil, no mouse highlights (or any other highlights) will be
;performed.  This might speed up Gnus some when generating large group
;and summary buffers.")

(defvar gnus-novice-user t
  "*Non-nil means that you are a usenet novice.
If non-nil, verbose messages may be displayed and confirmations may be
required.")

(defvar gnus-expert-user nil
  "*Non-nil means that you will never be asked for confirmation about anything.
And that means *anything*.")

(defvar gnus-verbose 7
  "*Integer that says how verbose Gnus should be.
The higher the number, the more messages Gnus will flash to say what
it's doing.  At zero, Gnus will be totally mute; at five, Gnus will
display most important messages; and at ten, Gnus will keep on
jabbering all the time.")

(defvar gnus-keep-same-level nil
  "*Non-nil means that the next newsgroup after the current will be on the same level.
When you type, for instance, `n' after reading the last article in the
current newsgroup, you will go to the next newsgroup.  If this variable
is nil, the next newsgroup will be the next from the group
buffer. 
If this variable is non-nil, Gnus will either put you in the
next newsgroup with the same level, or, if no such newsgroup is
available, the next newsgroup with the lowest possible level higher
than the current level.
If this variable is `best', Gnus will make the next newsgroup the one
with the best level.")

(defvar gnus-summary-make-false-root 'adopt
  "*nil means that Gnus won't gather loose threads.
If the root of a thread has expired or been read in a previous
session, the information necessary to build a complete thread has been
lost.  Instead of having many small sub-threads from this original thread
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
subject fields.  (Or rather, they will be printed with a string
given by the `gnus-summary-same-subject' variable.)")

(defvar gnus-summary-gather-exclude-subject "^ *$\\|^(none)$"
  "*A regexp to match subjects to be excluded from loose thread gathering.
As loose thread gathering is done on subjects only, that means that
there can be many false gatherings performed.  By rooting out certain
common subjects, gathering might become saner.")

(defvar gnus-summary-gather-subject-limit nil
  "*Maximum length of subject comparisons when gathering loose threads.
Use nil to compare full subjects.  Setting this variable to a low
number will help gather threads that have been corrupted by
newsreaders chopping off subject lines, but it might also mean that
unrelated articles that have subject that happen to begin with the
same few characters will be incorrectly gathered.

If this variable is `fuzzy', Gnus will use a fuzzy algorithm when
comparing subjects.")

;; Added by Per Abrahamsen <amanda@iesd.auc.dk>.
(defvar gnus-summary-same-subject ""
  "*String indicating that the current article has the same subject as the previous.
This variable will only be used if the value of
`gnus-summary-make-false-root' is `empty'.")

(defvar gnus-summary-goto-unread t
  "*If non-nil, marking commands will go to the next unread article.")

(defvar gnus-group-goto-unread t
  "*If non-nil, movement commands will go to the next unread and subscribed group.")

(defvar gnus-check-new-newsgroups t
  "*Non-nil means that Gnus will add new newsgroups at startup.
If this variable is `ask-server', Gnus will ask the server for new
groups since the last time it checked.  This means that the killed list
is no longer necessary, so you could set `gnus-save-killed-list' to
nil. 

A variant is to have this variable be a list of select methods.  Gnus
will then use the `ask-server' method on all these select methods to
query for new groups from all those servers.

Eg.
  (setq gnus-check-new-newsgroups 
        '((nntp \"some.server\") (nntp \"other.server\")))

If this variable is nil, then you have to tell Gnus explicitly to
check for new newsgroups with \\<gnus-group-mode-map>\\[gnus-find-new-newsgroups].")

(defvar gnus-check-bogus-newsgroups nil
  "*Non-nil means that Gnus will check and remove bogus newsgroup at startup.
If this variable is nil, then you have to tell Gnus explicitly to
check for bogus newsgroups with \\<gnus-group-mode-map>\\[gnus-group-check-bogus-groups].")

(defvar gnus-read-active-file t
  "*Non-nil means that Gnus will read the entire active file at startup.
If this variable is nil, Gnus will only know about the groups in your
`.newsrc' file.

If this variable is `some', Gnus will try to only read the relevant
parts of the active file from the server.  Not all servers support
this, and it might be quite slow with other servers, but this should
generally be faster than both the t and nil value.

If you set this variable to nil or `some', you probably still want to
be told about new newsgroups that arrive.  To do that, set
`gnus-check-new-newsgroups' to `ask-server'.  This may not work
properly with all servers.")

(defvar gnus-level-subscribed 5
  "*Groups with levels less than or equal to this variable are subscribed.")

(defvar gnus-level-unsubscribed 7
  "*Groups with levels less than or equal to this variable are unsubscribed.
Groups with levels less than `gnus-level-subscribed', which should be
less than this variable, are subscribed.")

(defvar gnus-level-zombie 8
  "*Groups with this level are zombie groups.")

(defvar gnus-level-killed 9
  "*Groups with this level are killed.")

(defvar gnus-level-default-subscribed 3
  "*New subscribed groups will be subscribed at this level.")

(defvar gnus-level-default-unsubscribed 6
  "*New unsubscribed groups will be unsubscribed at this level.")

(defvar gnus-activate-foreign-newsgroups 4
  "*If nil, Gnus will not check foreign newsgroups at startup.
If it is non-nil, it should be a number between one and nine.  Foreign
newsgroups that have a level lower or equal to this number will be
activated on startup.  For instance, if you want to active all
subscribed newsgroups, but not the rest, you'd set this variable to 
`gnus-level-subscribed'.

If you subscribe to lots of newsgroups from different servers, startup
might take a while.  By setting this variable to nil, you'll save time,
but you won't be told how many unread articles there are in the
groups.")

(defvar gnus-save-newsrc-file t
  "*Non-nil means that Gnus will save the `.newsrc' file.
Gnus always saves its own startup file, which is called
\".newsrc.eld\".  The file called \".newsrc\" is in a format that can
be readily understood by other newsreaders.  If you don't plan on
using other newsreaders, set this variable to nil to save some time on
exit.")

(defvar gnus-save-killed-list t
  "*If non-nil, save the list of killed groups to the startup file.
This will save both time (when starting and quitting) and space (both
memory and disk), but it will also mean that Gnus has no record of
which groups are new and which are old, so the automatic new
newsgroups subscription methods become meaningless.  You should always
set `gnus-check-new-newsgroups' to `ask-server' or nil if you set this
variable to nil.")

(defvar gnus-interactive-catchup t
  "*If non-nil, require your confirmation when catching up a group.")

(defvar gnus-interactive-post t
  "*If non-nil, group name will be asked for when posting.")

(defvar gnus-interactive-exit t
  "*If non-nil, require your confirmation when exiting Gnus.")

(defvar gnus-kill-killed t
  "*If non-nil, Gnus will apply kill files to already killed articles.
If it is nil, Gnus will never apply kill files to articles that have
already been through the scoring process, which might very well save lots
of time.")

(defvar gnus-extract-address-components 'gnus-extract-address-components
  "*Function for extracting address components from a From header.
Two pre-defined function exist: `gnus-extract-address-components',
which is the default, quite fast, and too simplistic solution, and
`mail-extract-address-components', which works much better, but is
slower.")

(defvar gnus-summary-default-score 0
  "*Default article score level.
If this variable is nil, scoring will be disabled.")

(defvar gnus-summary-zcore-fuzz 0
  "*Fuzziness factor for the zcore in the summary buffer.
Articles with scores closer than this to `gnus-summary-default-score'
will not be marked.")

(defvar gnus-simplify-subject-fuzzy-regexp nil
  "*Strings to be removed when doing fuzzy matches.
This can either be a egular expression or list of regular expressions
that will be removed from subject strings if fuzzy subject
simplification is selected.")

(defvar gnus-group-default-list-level gnus-level-subscribed
  "*Default listing level. 
Ignored if `gnus-group-use-permanent-levels' is non-nil.")

(defvar gnus-group-use-permanent-levels nil
  "*If non-nil, once you set a level, Gnus will use this level.")

(defvar gnus-show-mime nil
  "*If non-nil, do mime processing of articles.
The articles will simply be fed to the function given by
`gnus-show-mime-method'.")

(defvar gnus-strict-mime t
  "*If nil, decode MIME header even if there is not Mime-Version field.")
 
(defvar gnus-show-mime-method (function metamail-buffer)
  "*Function to process a MIME message.
The function is called from the article buffer.")

(defvar gnus-show-threads t
  "*If non-nil, display threads in summary mode.")

(defvar gnus-thread-hide-subtree nil
  "*If non-nil, hide all threads initially.
If threads are hidden, you have to run the command
`gnus-summary-show-thread' by hand or use `gnus-select-article-hook'
to expose hidden threads.")

(defvar gnus-thread-hide-killed t
  "*If non-nil, hide killed threads automatically.")

(defvar gnus-thread-ignore-subject nil
  "*If non-nil, ignore subjects and do all threading based on the Reference header.
If nil, which is the default, articles that have different subjects
from their parents will start separate threads.")

(defvar gnus-thread-operation-ignore-subject t
  "*If non-nil, subjects will be ignored when doing thread commands.
This affects commands like `gnus-summary-kill-thread' and
`gnus-summary-lower-thread'.  

If this variable is nil, articles in the same thread with different
subjects will not be included in the operation in question.  If this
variable is `fuzzy', only articles that have subjects that are fuzzily
equal will be included.")

(defvar gnus-thread-indent-level 4
  "*Number that says how much each sub-thread should be indented.")

(defvar gnus-ignored-newsgroups 
  (purecopy (mapconcat 'identity
                       '("^to\\."       ; not "real" groups
                         "^[0-9. \t]+ " ; all digits in name
                         "[][\"#'()]"   ; bogus characters
                         )
                       "\\|"))
  "*A regexp to match uninteresting newsgroups in the active file.
Any lines in the active file matching this regular expression are
removed from the newsgroup list before anything else is done to it,
thus making them effectively non-existent.")

(defvar gnus-ignored-headers
  "^Path:\\|^Posting-Version:\\|^Article-I.D.:\\|^Expires:\\|^Date-Received:\\|^References:\\|^Control:\\|^Xref:\\|^Lines:\\|^Posted:\\|^Relay-Version:\\|^Message-ID:\\|^Nf-ID:\\|^Nf-From:\\|^Approved:\\|^Sender:\\|^Received:\\|^Mail-from:"
  "*All headers that match this regexp will be hidden.
If `gnus-visible-headers' is non-nil, this variable will be ignored.")

(defvar gnus-visible-headers "^From:\\|^Newsgroups:\\|^Subject:\\|^Date:\\|^Followup-To:\\|^Reply-To:\\|^Organization:\\|^Summary:\\|^Keywords:\\|^To:\\|^Cc:\\|^Posted-To:\\|^Mail-Copy-To:"
  "*All headers that do not match this regexp will be hidden.
If this variable is non-nil, `gnus-ignored-headers' will be ignored.")

(defvar gnus-sorted-header-list
  '("^From:" "^Subject:" "^Summary:" "^Keywords:" "^Newsgroups:" "^To:" 
    "^Cc:" "^Date:" "^Organization:")
  "*This variable is a list of regular expressions.
If it is non-nil, headers that match the regular expressions will
be placed first in the article buffer in the sequence specified by
this list.")

(defvar gnus-show-all-headers nil
  "*If non-nil, don't hide any headers.")

(defvar gnus-save-all-headers t
  "*If non-nil, don't remove any headers before saving.")

(defvar gnus-saved-headers gnus-visible-headers
  "*Headers to keep if `gnus-save-all-headers' is nil.
If `gnus-save-all-headers' is non-nil, this variable will be ignored.
If that variable is nil, however, all headers that match this regexp
will be kept while the rest will be deleted before saving.")

(defvar gnus-inhibit-startup-message nil
  "*If non-nil, the startup message will not be displayed.")

(defvar gnus-signature-separator "^-- *$"
  "Regexp matching signature separator.")

(defvar gnus-auto-extend-newsgroup t
  "*If non-nil, extend newsgroup forward and backward when requested.")

(defvar gnus-auto-select-first t
  "*If nil, don't select the first unread article when entering a group.
If this variable is `best', select the highest-scored unread article
in the group.  If neither nil nor `best', select the first unread
article.

If you want to prevent automatic selection of the first unread article
in some newsgroups, set the variable to nil in
`gnus-select-group-hook'.") 

(defvar gnus-auto-select-next t
  "*If non-nil, offer to go to the next group from the end of the previous.
If the value is t and the next newsgroup is empty, Gnus will exit
summary mode and go back to group mode.  If the value is neither nil
nor t, Gnus will select the following unread newsgroup.  In
particular, if the value is the symbol `quietly', the next unread
newsgroup will be selected without any confirmation, and if it is
`almost-quietly', the next group will be selected without any
confirmation if you are located on the last article in the group.")

(defvar gnus-auto-select-same nil
  "*If non-nil, select the next article with the same subject.")

(defvar gnus-summary-check-current nil
  "*If non-nil, consider the current article when moving.
The \"unread\" movement commands will stay on the same line if the
current article is unread.")

(defvar gnus-auto-center-summary t
  "*If non-nil, always center the current summary buffer.")

(defvar gnus-break-pages t
  "*If non-nil, do page breaking on articles.
The page delimiter is specified by the `gnus-page-delimiter'
variable.")

(defvar gnus-page-delimiter "^\^L"
  "*Regexp describing what to use as article page delimiters.
The default value is \"^\^L\", which is a form linefeed at the
beginning of a line.")

(defvar gnus-use-full-window t
  "*If non-nil, use the entire Emacs screen.")

(defvar gnus-window-configuration nil
  "Obsolete variable.  See `gnus-buffer-configuration'.")

(defvar gnus-buffer-configuration
  '((group ([group 1.0 point] 
	    (if gnus-carpal [group-carpal 4])))
    (summary ([summary 1.0 point]
	      (if gnus-carpal [summary-carpal 4])))
    (article ([summary 0.25 point] 
	      (if gnus-carpal [summary-carpal 4]) 
	      [article 1.0]))
    (server ([server 1.0 point]
	     (if gnus-carpal [server-carpal 2])))
    (browse ([browse 1.0 point]
	     (if gnus-carpal [browse-carpal 2])))
    (group-mail ([mail 1.0 point]))
    (summary-mail ([mail 1.0 point]))
    (summary-reply ([article 0.5]
		    [mail 1.0 point]))
    (info ([nil 1.0 point]))
    (summary-faq ([summary 0.25]
		  [faq 1.0 point]))
    (edit-group ([group 0.5]
		 [edit-group 1.0 point]))
    (edit-server ([server 0.5]
		  [edit-server 1.0 point]))
    (edit-score ([summary 0.25]
		 [edit-score 1.0 point]))
    (post ([post 1.0 point]))
    (reply ([article 0.5]
	    [mail 1.0 point]))
    (mail-forward ([mail 1.0 point]))
    (post-forward ([post 1.0 point]))
    (reply-yank ([mail 1.0 point]))
    (mail-bounce ([article 0.5]
		  [mail 1.0 point]))
    (draft ([draft 1.0 point]))
    (pipe ([summary 0.25 point] 
	   (if gnus-carpal [summary-carpal 4]) 
	   ["*Shell Command Output*" 1.0]))
    (followup ([article 0.5]
	       [post 1.0 point]))
    (followup-yank ([post 1.0 point])))
  "Window configuration for all possible Gnus buffers.
This variable is a list of lists.  Each of these lists has a NAME and
a RULE.  The NAMEs are commonsense names like `group', which names a
rule used when displaying the group buffer; `summary', which names a
rule for what happens when you enter a group and do not display an
article buffer; and so on.  See the value of this variable for a
complete list of NAMEs.

Each RULE is a list of vectors.  The first element in this vector is
the name of the buffer to be displayed; the second element is the
percentage of the screen this buffer is to occupy (a number in the
0.0-0.99 range); the optional third element is `point', which should
be present to denote which buffer point is to go to after making this
buffer configuration.")

(defvar gnus-window-to-buffer
  '((group . gnus-group-buffer)
    (summary . gnus-summary-buffer)
    (article . gnus-article-buffer)
    (server . gnus-server-buffer)
    (browse . "*Gnus Browse Server*")
    (edit-group . gnus-group-edit-buffer)
    (edit-server . gnus-server-edit-buffer)
    (group-carpal . gnus-carpal-group-buffer)
    (summary-carpal . gnus-carpal-summary-buffer)
    (server-carpal . gnus-carpal-server-buffer)
    (browse-carpal . gnus-carpal-browse-buffer)
    (edit-score . gnus-score-edit-buffer)
    (mail . gnus-mail-buffer)
    (post . gnus-post-news-buffer)
    (faq . gnus-faq-buffer)
    (draft . gnus-draft-buffer))
  "Mapping from short symbols to buffer names or buffer variables.")

(defvar gnus-carpal nil
  "*If non-nil, display clickable icons.")

(defvar gnus-subscribe-newsgroup-method 'gnus-subscribe-zombies
  "*Function called with a group name when new group is detected.
A few pre-made functions are supplied: `gnus-subscribe-randomly'
inserts new groups at the beginning of the list of groups;
`gnus-subscribe-alphabetically' inserts new groups in strict
alphabetic order; `gnus-subscribe-hierarchically' inserts new groups
in hierarchical newsgroup order; `gnus-subscribe-interactively' asks
for your decision.")

;; Suggested by a bug report by Hallvard B Furuseth.
;; <h.b.furuseth@usit.uio.no>. 
(defvar gnus-subscribe-options-newsgroup-method
  (function gnus-subscribe-alphabetically)
  "*This function is called to subscribe newsgroups mentioned on \"options -n\" lines.
If, for instance, you want to subscribe to all newsgroups in the
\"no\" and \"alt\" hierarchies, you'd put the following in your
.newsrc file:

options -n no.all alt.all

Gnus will the subscribe all new newsgroups in these hierarchies with
the subscription method in this variable.")

(defvar gnus-subscribe-hierarchical-interactive nil
  "*If non-nil, Gnus will offer to subscribe hierarchically.
When a new hierarchy appears, Gnus will ask the user:

'alt.binaries': Do you want to subscribe to this hierarchy? ([d]ys):

If the user pressed `d', Gnus will descend the hierarchy, `y' will
subscribe to all newsgroups in the hierarchy and `s' will skip this
hierarchy in its entirety.")

(defvar gnus-group-sort-function 'gnus-group-sort-by-alphabet
  "*Function used for sorting the group buffer.
This function will be called with group info entries as the arguments
for the groups to be sorted.  Pre-made functions include
`gnus-group-sort-by-alphabet', `gnus-group-sort-by-unread',
`gnus-group-sort-by-level', `gnus-group-sort-by-score', and
`gnus-group-sort-by-rank'.  

This variable can also be a list of sorting functions.  In that case,
the most significant sort function should be the last function in the
list.")

;; Mark variables suggested by Thomas Michanek
;; <Thomas.Michanek@telelogic.se>. 
(defvar gnus-unread-mark ? 
  "*Mark used for unread articles.")
(defvar gnus-ticked-mark ?!
  "*Mark used for ticked articles.")
(defvar gnus-dormant-mark ??
  "*Mark used for dormant articles.")
(defvar gnus-del-mark ?r
  "*Mark used for del'd articles.")
(defvar gnus-read-mark ?R
  "*Mark used for read articles.")
(defvar gnus-expirable-mark ?E
  "*Mark used for expirable articles.")
(defvar gnus-killed-mark ?K
  "*Mark used for killed articles.")
(defvar gnus-souped-mark ?F
  "*Mark used for killed articles.")
(defvar gnus-kill-file-mark ?X
  "*Mark used for articles killed by kill files.")
(defvar gnus-low-score-mark ?Y
  "*Mark used for articles with a low score.")
(defvar gnus-catchup-mark ?C
  "*Mark used for articles that are caught up.")
(defvar gnus-replied-mark ?A
  "*Mark used for articles that have been replied to.")
(defvar gnus-process-mark ?# 
  "*Process mark.")
(defvar gnus-ancient-mark ?O
  "*Mark used for ancient articles.")
(defvar gnus-canceled-mark ?G
  "*Mark used for canceled articles.")
(defvar gnus-score-over-mark ?+
  "*Score mark used for articles with high scores.")
(defvar gnus-score-below-mark ?-
  "*Score mark used for articles with low scores.")
(defvar gnus-empty-thread-mark ? 
  "*There is no thread under the article.")
(defvar gnus-not-empty-thread-mark ?=
  "*There is a thread under the article.")

(defvar gnus-view-pseudo-asynchronously nil
  "*If non-nil, Gnus will view pseudo-articles asynchronously.")

(defvar gnus-view-pseudos nil
  "*If `automatic', pseudo-articles will be viewed automatically.
If `not-confirm', pseudos will be viewed automatically, and the user
will not be asked to confirm the command.")

(defvar gnus-view-pseudos-separately t
  "*If non-nil, one pseudo-article will be created for each file to be viewed.
If nil, all files that use the same viewing command will be given as a
list of parameters to that command.")

(defvar gnus-group-line-format "%M%S%p%5y: %(%g%)\n"
  "*Format of group lines.
It works along the same lines as a normal formatting string,
with some simple extensions.

%M    Only marked articles (character, \"*\" or \" \")
%S    Whether the group is subscribed (character, \"U\", \"K\", \"Z\" or \" \")
%L    Level of subscribedness (integer)
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
%p    Process mark (char)
%O    Moderated group (string, \"(m)\" or \"\")
%n    Select from where (string)
%z    A string that look like `<%s:%n>' if a foreign select method is used
%u    User defined specifier.  The next character in the format string should
      be a letter.  Gnus will call the function gnus-user-format-function-X,
      where X is the letter following %u.  The function will be passed the
      current header as argument.  The function should return a string, which
      will be inserted into the buffer just like information from any other
      group specifier.

Text between %( and %) will be highlighted with `gnus-mouse-face' when
the mouse point move inside the area.  There can only be one such area.

Note that this format specification is not always respected.  For
reasons of efficiency, when listing killed groups, this specification
is ignored altogether.  If the spec is changed considerably, your
output may end up looking strange when listing both alive and killed
groups.

If you use %o or %O, reading the active file will be slower and quite
a bit of extra memory will be used. %D will also worsen performance.
Also note that if you change the format specification to include any
of these specs, you must probably re-start Gnus to see them go into
effect.") 

(defvar gnus-summary-line-format "%U%R%z%I%(%[%4L: %-20,20n%]%) %s\n"
  "*The format specification of the lines in the summary buffer.

It works along the same lines as a normal formatting string,
with some simple extensions.

%N   Article number, left padded with spaces (string)
%S   Subject (string)
%s   Subject if it is at the root of a thread, and \"\" otherwise (string)
%n   Name of the poster (string)
%a   Extracted name of the poster (string)
%A   Extracted address of the poster (string)
%F   Contents of the From: header (string)
%x   Contents of the Xref: header (string)
%D   Date of the article (string)
%d   Date of the article (string) in DD-MMM format
%M   Message-id of the article (string)
%r   References of the article (string)
%c   Number of characters in the article (integer)
%L   Number of lines in the article (integer)
%I   Indentation based on thread level (a string of spaces)
%T   A string with two possible values: 80 spaces if the article
     is on thread level two or larger and 0 spaces on level one
%R   \"A\" if this article has been replied to, \" \" otherwise (character)
%U   Status of this article (character, \"R\", \"K\", \"-\" or \" \")
%[   Opening bracket (character, \"[\" or \"<\")
%]   Closing bracket (character, \"]\" or \">\")
%>   Spaces of length thread-level (string)
%<   Spaces of length (- 20 thread-level) (string)
%i   Article score (number)
%z   Article zcore (character)
%t   Number of articles under the current thread (number).
%e   Whether the thread is empty or not (character).
%u   User defined specifier.  The next character in the format string should
     be a letter.  Gnus will call the function gnus-user-format-function-X,
     where X is the letter following %u.  The function will be passed the
     current header as argument.  The function should return a string, which
     will be inserted into the summary just like information from any other
     summary specifier.

Text between %( and %) will be highlighted with `gnus-mouse-face'
when the mouse point is placed inside the area.  There can only be one
such area.

The %U (status), %R (replied) and %z (zcore) specs have to be handled
with care.  For reasons of efficiency, Gnus will compute what column
these characters will end up in, and \"hard-code\" that.  This means that
it is illegal to have these specs after a variable-length spec.  Well,
you might not be arrested, but your summary buffer will look strange,
which is bad enough.

The smart choice is to have these specs as for to the left as
possible. 

This restriction may disappear in later versions of Gnus.")

(defvar gnus-summary-dummy-line-format "*  :                          : %S\n"
  "*The format specification for the dummy roots in the summary buffer.
It works along the same lines as a normal formatting string,
with some simple extensions.

%S  The subject")

(defvar gnus-summary-mode-line-format "Gnus  %G/%A %Z"
  "*The format specification for the summary mode line.")

(defvar gnus-article-mode-line-format "Gnus  %G/%A %S"
  "*The format specification for the article mode line.")

(defvar gnus-group-mode-line-format "Gnus  List of groups   {%M:%S}  "
  "*The format specification for the group mode line.")

(defvar gnus-valid-select-methods
  '(("nntp" post address prompt-address)
    ("nnspool" post)
    ("nnvirtual" none virtual prompt-address) 
    ("nnmbox" mail respool) 
    ("nnml" mail respool)
    ("nnmh" mail respool) 
    ("nndir" none prompt-address address)
    ("nneething" none prompt-address)
    ("nndigest" none) 
    ("nndoc" none prompt-address) 
    ("nnbabyl" mail respool) 
    ("nnkiboze" post virtual) 
    ("nnsoup" post)
    ("nnfolder" mail respool))
  "An alist of valid select methods.
The first element of each list lists should be a string with the name
of the select method.  The other elements may be be the category of
this method (ie. `post', `mail', `none' or whatever) or other
properties that this method has (like being respoolable).
If you implement a new select method, all you should have to change is
this variable.  I think.")

(defvar gnus-updated-mode-lines '(group article summary)
  "*List of buffers that should update their mode lines.
The list may contain the symbols `group', `article' and `summary'.  If
the corresponding symbol is present, Gnus will keep that mode line
updated with information that may be pertinent. 
If this variable is nil, screen refresh may be quicker.")

;; Added by Keinonen Kari <kk85613@cs.tut.fi>.
(defvar gnus-mode-non-string-length 21
  "*Max length of mode-line non-string contents.
If this is nil, Gnus will take space as is needed, leaving the rest
of the modeline intact.")

;see gnus-cus.el
;(defvar gnus-mouse-face 'highlight
;  "*Face used for mouse highlighting in Gnus.
;No mouse highlights will be done if `gnus-visual' is nil.")

(defvar gnus-summary-mark-below nil
  "*Mark all articles with a score below this variable as read.
This variable is local to each summary buffer and usually set by the
score file.")  

(defvar gnus-thread-sort-functions '(gnus-thread-sort-by-number)
  "*List of functions used for sorting threads in the summary buffer.
By default, threads are sorted by article number.

Each function takes two threads and return non-nil if the first thread
should be sorted before the other.  If you use more than one function,
the primary sort function should be the last.

Ready-mady functions include `gnus-thread-sort-by-number',
`gnus-thread-sort-by-author', `gnus-thread-sort-by-subject',
`gnus-thread-sort-by-date', `gnus-thread-sort-by-score' and
`gnus-thread-sort-by-total-score' (see `gnus-thread-score-function').")

(defvar gnus-thread-score-function '+
  "*Function used for calculating the total score of a thread.

The function is called with the scores of the article and each
subthread and should then return the score of the thread.

Some functions you can use are `+', `max', or `min'.")

(defvar gnus-auto-subscribed-groups 
  "^nnml\\|^nnfolder\\|^nnmbox\\|^nnmh\\|^nnbabyl"
  "*All new groups that match this regexp will be subscribed automatically.
Note that this variable only deals with new groups.  It has no effect
whatsoever on old groups.")

(defvar gnus-options-subscribe nil
  "*All new groups matching this regexp will be subscribed unconditionally.
Note that this variable deals only with new newsgroups.  This variable
does not affect old newsgroups.")

(defvar gnus-options-not-subscribe nil
  "*All new groups matching this regexp will be ignored.
Note that this variable deals only with new newsgroups.  This variable
does not affect old (already subscribed) newsgroups.")

(defvar gnus-auto-expirable-newsgroups nil
  "*Groups in which to automatically mark read articles as expirable.
If non-nil, this should be a regexp that should match all groups in
which to perform auto-expiry.  This only makes sense for mail groups.")

(defvar gnus-total-expirable-newsgroups nil
  "*Groups in which to perform expiry of all read articles.
Use with extreme caution.  All groups that match this regexp will be
expiring - which means that all read articles will be deleted after
(say) one week.  (This only goes for mail groups and the like, of
course.)")

(defvar gnus-hidden-properties '(invisible t intangible t)
  "Property list to use for hiding text.")

(defvar gnus-modtime-botch nil
  "*Non-nil means .newsrc should be deleted prior to save.  Its use is
due to the bogus appearance that .newsrc was modified on disc.")

;; Hooks.

(defvar gnus-group-mode-hook nil
  "*A hook for Gnus group mode.")

(defvar gnus-summary-mode-hook nil
  "*A hook for Gnus summary mode.
This hook is run before any variables are set in the summary buffer.")

(defvar gnus-article-mode-hook nil
  "*A hook for Gnus article mode.")

(defun gnus-summary-prepare-exit-hook nil
  "*A hook called when preparing to exit from the summary buffer.
It calls `gnus-summary-expire-articles' by default.")
(add-hook 'gnus-summary-prepare-exit-hook 'gnus-summary-expire-articles)

(defun gnus-summary-exit-hook nil
  "*A hook called on exit from the summary buffer.")

(defvar gnus-open-server-hook nil
  "*A hook called just before opening connection to the news server.")

(defvar gnus-load-hook nil
  "*A hook run while Gnus is loaded.")

(defvar gnus-startup-hook nil
  "*A hook called at startup.
This hook is called after Gnus is connected to the NNTP server.")

(defvar gnus-get-new-news-hook nil
  "*A hook run just before Gnus checks for new news.")

(defvar gnus-group-prepare-function 'gnus-group-prepare-flat
  "*A function that is called to generate the group buffer.
The function is called with three arguments: The first is a number;
all group with a level less or equal to that number should be listed,
if the second is non-nil, empty groups should also be displayed.  If
the third is non-nil, it is a number.  No groups with a level lower
than this number should be displayed.

The only current function implemented are `gnus-group-prepare-flat'
\(which does the normal boring group display) and
`gnus-group-prepare-topics' (which does a folding display accoring to
topics).")

(defvar gnus-group-prepare-hook nil
  "*A hook called after the group buffer has been generated.
If you want to modify the group buffer, you can use this hook.")

(defvar gnus-summary-prepare-hook nil
  "*A hook called after the summary buffer has been generated.
If you want to modify the summary buffer, you can use this hook.")

(defvar gnus-summary-generate-hook nil
  "*A hook run just before generating the summary buffer.
This hook is commonly used to customize threading variables and the
like.")

(defvar gnus-article-prepare-hook nil
  "*A hook called after an article has been prepared in the article buffer.
If you want to run a special decoding program like nkf, use this hook.")

;(defvar gnus-article-display-hook nil
;  "*A hook called after the article is displayed in the article buffer.
;The hook is designed to change the contents of the article
;buffer.  Typical functions that this hook may contain are
;`gnus-article-hide-headers' (hide selected headers),
;`gnus-article-maybe-highlight' (perform fancy article highlighting), 
;`gnus-article-hide-signature' (hide signature) and
;`gnus-article-treat-overstrike' (turn \"^H_\" into bold characters).")
;(add-hook 'gnus-article-display-hook 'gnus-article-hide-headers-if-wanted)
;(add-hook 'gnus-article-display-hook 'gnus-article-treat-overstrike)
;(add-hook 'gnus-article-display-hook 'gnus-article-maybe-highlight)

(defvar gnus-article-x-face-command
  "{ echo '/* Width=48, Height=48 */'; uncompface; } | icontopbm | xv -quit -"
  "String or function to be executed to display an X-Face header.
If it is a string, the command will be executed in a sub-shell
asynchronously.  The compressed face will be piped to this command.") 

(defvar gnus-article-x-face-too-ugly nil
  "Regexp matching posters whose face shouldn't be shown automatically.")

(defvar gnus-select-group-hook nil
  "*A hook called when a newsgroup is selected.

If you'd like to simplify subjects like the
`gnus-summary-next-same-subject' command does, you can use the
following hook:

 (setq gnus-select-group-hook
      (list
	(lambda ()
	  (mapcar (lambda (header)
		     (mail-header-set-subject
		      header
		      (gnus-simplify-subject
		       (mail-header-subject header) 're-only)))
		  gnus-newsgroup-headers))))")

(defvar gnus-select-article-hook
  '(gnus-summary-show-thread)
  "*A hook called when an article is selected.
The default hook shows conversation thread subtrees of the selected
article automatically using `gnus-summary-show-thread'.")

(defvar gnus-apply-kill-hook '(gnus-apply-kill-file)
  "*A hook called to apply kill files to a group.
This hook is intended to apply a kill file to the selected newsgroup.
The function `gnus-apply-kill-file' is called by default.

Since a general kill file is too heavy to use only for a few
newsgroups, I recommend you to use a lighter hook function.  For
example, if you'd like to apply a kill file to articles which contains
a string `rmgroup' in subject in newsgroup `control', you can use the
following hook:

 (setq gnus-apply-kill-hook
      (list
	(lambda ()
	  (cond ((string-match \"control\" gnus-newsgroup-name)
		 (gnus-kill \"Subject\" \"rmgroup\")
		 (gnus-expunge \"X\"))))))")

(defvar gnus-visual-mark-article-hook 
  (list 'gnus-highlight-selected-summary)
  "*Hook run after selecting an article in the summary buffer.
It is meant to be used for highlighting the article in some way.  It
is not run if `gnus-visual' is nil.")

(defvar gnus-exit-group-hook nil
  "*A hook called when exiting (not quitting) summary mode.")

(defvar gnus-suspend-gnus-hook nil
  "*A hook called when suspending (not exiting) Gnus.")

(defvar gnus-exit-gnus-hook nil
  "*A hook called when exiting Gnus.")

(defvar gnus-save-newsrc-hook nil
  "*A hook called before saving any of the newsrc files.")

(defvar gnus-save-quick-newsrc-hook nil
  "*A hook called just before saving the quick newsrc file.
Can be used to turn version control on or off.")

(defvar gnus-save-standard-newsrc-hook nil
  "*A hook called just before saving the standard newsrc file.
Can be used to turn version control on or off.")

(defvar gnus-summary-update-hook 
  (list 'gnus-summary-highlight-line)
  "*A hook called when a summary line is changed.
The hook will not be called if `gnus-visual' is nil.

The default function `gnus-summary-highlight-line' will
highlight the line according to the `gnus-summary-highlight'
variable.")

(defvar gnus-mark-article-hook (list 'gnus-summary-mark-unread-as-read)
  "*A hook called when an article is selected for the first time.
The hook is intended to mark an article as read (or unread)
automatically when it is selected.")

;; Remove any hilit infestation.
(add-hook 'gnus-startup-hook
	  (lambda ()
	    (remove-hook 'gnus-summary-prepare-hook
			 'hilit-rehighlight-buffer-quietly)
	    (remove-hook 'gnus-summary-prepare-hook 'hilit-install-line-hooks)
	    (setq gnus-mark-article-hook '(gnus-summary-mark-unread-as-read))
	    (remove-hook 'gnus-article-prepare-hook
			 'hilit-rehighlight-buffer-quietly)))



;; Internal variables

;; Avoid highlighting in kill files.
(defvar gnus-summary-inhibit-highlight nil)
(defvar gnus-newsgroup-selected-overlay nil)

(defvar gnus-article-mode-map nil)
(defvar gnus-dribble-buffer nil)
(defvar gnus-headers-retrieved-by nil)
(defvar gnus-article-reply nil)
(defvar gnus-override-method nil)
(defvar gnus-article-check-size nil)

(defvar gnus-nocem-hashtb nil)

(defvar gnus-current-score-file nil)
(defvar gnus-scores-exclude-files nil)

(defvar gnus-opened-servers nil)

(defvar gnus-current-move-group nil)

(defvar gnus-newsgroup-dependencies nil)
(defvar gnus-newsgroup-async nil)
(defconst gnus-group-edit-buffer "*Gnus edit newsgroup*")

(defvar gnus-newsgroup-adaptive nil)

(defvar gnus-summary-display-table nil)

(defconst gnus-group-line-format-alist
  (` ((?M gnus-tmp-marked ?c)
      (?S gnus-tmp-subscribed ?c)
      (?L gnus-tmp-level ?d)
      (?N gnus-tmp-number ?s)
      (?I gnus-tmp-number-of-dormant ?d)
      (?T gnus-tmp-number-of-ticked ?d)
      (?R gnus-tmp-number-of-read ?s)
      (?t gnus-tmp-number-total ?d)
      (?y gnus-tmp-number-of-unread-unticked ?s)
      (?i gnus-tmp-number-of-ticked-and-dormant ?d)
      (?g gnus-tmp-group ?s)
      (?G gnus-tmp-qualified-group ?s)
      (?D gnus-tmp-newsgroup-description ?s)
      (?o gnus-tmp-moderated ?c)
      (?O gnus-tmp-moderated-string ?s)
      (?p gnus-tmp-process-marked ?c)
      (?s gnus-tmp-news-server ?s)
      (?n gnus-tmp-news-method ?s)
      (?z gnus-tmp-news-method-string ?s)
      (?u gnus-tmp-user-defined ?s))))

(defconst gnus-summary-line-format-alist 
  (` ((?N gnus-tmp-number ?d)
      (?S gnus-tmp-subject ?s)
      (?s gnus-tmp-subject-or-nil ?s)
      (?n gnus-tmp-name ?s)
      (?A (car (cdr (funcall gnus-extract-address-components gnus-tmp-from)))
	  ?s)
      (?a (or (car (funcall gnus-extract-address-components gnus-tmp-from)) 
	      gnus-tmp-from) ?s)
      (?F gnus-tmp-from ?s)
      (?x (, (macroexpand '(mail-header-xref gnus-tmp-header))) ?s)
      (?D (, (macroexpand '(mail-header-date gnus-tmp-header))) ?s)
      (?d (gnus-dd-mmm (mail-header-date gnus-tmp-header)) ?s)
      (?M (, (macroexpand '(mail-header-id gnus-tmp-header))) ?s)
      (?r (, (macroexpand '(mail-header-references gnus-tmp-header))) ?s)
      (?c (or (mail-header-chars gnus-tmp-header) 0) ?d)
      (?L gnus-tmp-lines ?d)
      (?I gnus-tmp-indentation ?s)
      (?T (if (= gnus-tmp-level 0) "" (make-string (frame-width) ? )) ?s)
      (?R gnus-tmp-replied ?c)
      (?\[ gnus-tmp-opening-bracket ?c)
      (?\] gnus-tmp-closing-bracket ?c)
      (?\> (make-string gnus-tmp-level ? ) ?s)
      (?\< (make-string (max 0 (- 20 gnus-tmp-level)) ? ) ?s)
      (?i gnus-tmp-score ?d)
      (?z gnus-tmp-score-char ?c)
      (?U gnus-tmp-unread ?c)
      (?t (gnus-summary-number-of-articles-in-thread 
	   (and (boundp 'thread) (car thread)) gnus-tmp-level)
	  ?d)
      (?e (gnus-summary-number-of-articles-in-thread 
	   (and (boundp 'thread) (car thread)) gnus-tmp-level t)
	  ?c)
      (?u gnus-tmp-user-defined ?s)))
  "An alist of format specifications that can appear in summary lines,
and what variables they correspond with, along with the type of the
variable (string, integer, character, etc).")

(defconst gnus-summary-dummy-line-format-alist
  (` ((?S gnus-tmp-subject ?s)
      (?N gnus-tmp-number ?d)
      (?u gnus-tmp-user-defined ?s))))

(defconst gnus-summary-mode-line-format-alist 
  (` ((?G gnus-tmp-group-name ?s)
      (?g (gnus-short-group-name gnus-tmp-group-name) ?s)
      (?p (gnus-group-real-name gnus-tmp-group-name) ?s)
      (?A gnus-tmp-article-number ?d)
      (?Z gnus-tmp-unread-and-unselected ?s)
      (?V gnus-version ?s)
      (?U gnus-tmp-unread ?d)
      (?S gnus-tmp-subject ?s)
      (?e gnus-tmp-unselected ?d)
      (?u gnus-tmp-user-defined ?s)
      (?d (length gnus-newsgroup-dormant) ?d)
      (?t (length gnus-newsgroup-marked) ?d)
      (?r (length gnus-newsgroup-reads) ?d)
      (?E gnus-newsgroup-expunged-tally ?d)
      (?s (gnus-current-score-file-nondirectory) ?s))))

(defconst gnus-group-mode-line-format-alist 
  (` ((?S gnus-tmp-news-server ?s)
      (?M gnus-tmp-news-method ?s)
      (?u gnus-tmp-user-defined ?s))))

(defvar gnus-have-read-active-file nil)

(defconst gnus-maintainer
  "gnus-bug@ifi.uio.no (The Gnus Bugfixing Girls + Boys)"
  "The mail address of the Gnus maintainers.")

(defconst gnus-version "September Gnus v0.14"
  "Version number for this version of Gnus.")

(defvar gnus-info-nodes
  '((gnus-group-mode		"(gnus)The Group Buffer")
    (gnus-summary-mode		"(gnus)The Summary Buffer")
    (gnus-article-mode		"(gnus)The Article Buffer"))
  "Assoc list of major modes and related Info nodes.")

(defvar gnus-group-buffer "*Group*")
(defvar gnus-summary-buffer "*Summary*")
(defvar gnus-article-buffer "*Article*")
(defvar gnus-server-buffer "*Server*")

(defvar gnus-work-buffer " *gnus work*")

(defvar gnus-original-article-buffer " *Original Article*")
(defvar gnus-original-article nil)

(defvar gnus-buffer-list nil
  "Gnus buffers that should be killed on exit.")

(defvar gnus-server-alist nil
  "List of available servers.")

(defvar gnus-slave nil
  "Whether this Gnus is a slave or not.")

(defvar gnus-variable-list
  '(gnus-newsrc-options gnus-newsrc-options-n
    gnus-newsrc-last-checked-date 
    gnus-newsrc-alist gnus-server-alist
    gnus-killed-list gnus-zombie-list)
  "Gnus variables saved in the quick startup file.")

(defvar gnus-newsrc-options nil
  "Options line in the .newsrc file.")

(defvar gnus-newsrc-options-n nil
  "List of regexps representing groups to be subscribed/ignored unconditionally.") 

(defvar gnus-newsrc-last-checked-date nil
  "Date Gnus last asked server for new newsgroups.")

(defvar gnus-newsrc-alist nil
  "Assoc list of read articles.
gnus-newsrc-hashtb should be kept so that both hold the same information.")

(defvar gnus-newsrc-hashtb nil
  "Hashtable of gnus-newsrc-alist.")

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

(defvar gnus-group-marked nil)

(defvar gnus-current-startup-file nil
  "Startup file for the current host.")

(defvar gnus-last-search-regexp nil
  "Default regexp for article search command.")

(defvar gnus-last-shell-command nil
  "Default shell command on article.")

(defvar gnus-current-select-method nil
  "The current method for selecting a newsgroup.")

(defvar gnus-group-list-mode nil)

(defvar gnus-article-internal-prepare-hook nil)

(defvar gnus-newsgroup-name nil)
(defvar gnus-newsgroup-begin nil)
(defvar gnus-newsgroup-end nil)
(defvar gnus-newsgroup-last-rmail nil)
(defvar gnus-newsgroup-last-mail nil)
(defvar gnus-newsgroup-last-folder nil)
(defvar gnus-newsgroup-last-file nil)
(defvar gnus-newsgroup-auto-expire nil)
(defvar gnus-newsgroup-active nil)

(defvar gnus-newsgroup-data nil)
(defvar gnus-newsgroup-data-reverse nil)
(defvar gnus-newsgroup-limit nil)
(defvar gnus-newsgroup-limits nil)

(defvar gnus-newsgroup-unreads nil
  "List of unread articles in the current newsgroup.")

(defvar gnus-newsgroup-unselected nil
  "List of unselected unread articles in the current newsgroup.")

(defvar gnus-newsgroup-reads nil
  "Alist of read articles and article marks in the current newsgroup.")

(defvar gnus-newsgroup-expunged-tally nil)

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

(defvar gnus-newsgroup-threads nil)

(defvar gnus-newsgroup-prepared nil)

(defvar gnus-newsgroup-ancient nil
  "List of `gnus-fetch-old-headers' articles in the current newsgroup.")

(defvar gnus-current-article nil)
(defvar gnus-article-current nil)
(defvar gnus-current-headers nil)
(defvar gnus-have-all-headers nil)
(defvar gnus-last-article nil)
(defvar gnus-newsgroup-history nil)
(defvar gnus-current-kill-article nil)

;; Save window configuration.
(defvar gnus-prev-winconf nil)


;; Format specs.  The chunks below are the machine-generated forms
;; that are to be evaled as the result of the default format strings.
;; We write them in here to get them byte-compiled.  That way the
;; default actions will be quite fast, while still retaining the full
;; flexibility of the user-defined format specs. 

;; First we have lots of dummy defvars to let the compiler know these
;; are realyl dynamic variables.

(defvar gnus-tmp-unread)
(defvar gnus-tmp-replied)
(defvar gnus-tmp-score-char)
(defvar gnus-tmp-indentation)
(defvar gnus-tmp-opening-bracket)
(defvar gnus-tmp-lines)
(defvar gnus-tmp-name)
(defvar gnus-tmp-closing-bracket)
(defvar gnus-tmp-subject-or-nil)
(defvar gnus-tmp-subject)
(defvar gnus-tmp-marked)
(defvar gnus-tmp-subscribed)
(defvar gnus-tmp-process-marked)
(defvar gnus-tmp-number-of-unread-unticked)
(defvar gnus-tmp-group-name)
(defvar gnus-tmp-group)
(defvar gnus-tmp-article-number)
(defvar gnus-tmp-unread-and-unselected)
(defvar gnus-tmp-news-method)
(defvar gnus-tmp-news-server)
(defvar gnus-tmp-article-number)
(defvar gnus-mouse-face)

(defun gnus-byte-code (func)
  (let ((fval (symbol-function func)))
    (if (byte-code-function-p fval)
	(list 'byte-code (aref fval 1) (aref fval 2) (aref fval 3))
      (list 'eval (cons 'progn (cdr (cdr fval)))))))

(defun gnus-summary-line-format-spec ()
  (insert gnus-tmp-unread gnus-tmp-replied 
	  gnus-tmp-score-char gnus-tmp-indentation)
  (let ((b (point)))
    (insert gnus-tmp-opening-bracket 
	    (format "%4d: %-20s" 
		    gnus-tmp-lines 
		    (if (> (length gnus-tmp-name) 20) 
			(substring gnus-tmp-name 0 20) 
		      gnus-tmp-name))
	    gnus-tmp-closing-bracket
	    " " gnus-tmp-subject-or-nil "\n")
    (put-text-property b (+ b 28) gnus-mouse-face-prop gnus-mouse-face)))
(defvar gnus-summary-line-format-spec 
  (gnus-byte-code 'gnus-summary-line-format-spec))

(defun gnus-summary-dummy-line-format-spec ()
  (insert "*  :                          : " gnus-tmp-subject "\n"))
(defvar gnus-summary-dummy-line-format-spec 
  (gnus-byte-code 'gnus-summary-dummy-line-format-spec))

(defun gnus-group-line-format-spec ()
  (insert gnus-tmp-marked gnus-tmp-subscribed 
	  gnus-tmp-process-marked
   (format "%5s: " gnus-tmp-number-of-unread-unticked))
  (let ((b (point)))
    (insert gnus-tmp-group "\n")
    (put-text-property b (1- (point)) gnus-mouse-face-prop gnus-mouse-face)))
(defvar gnus-group-line-format-spec 
  (gnus-byte-code 'gnus-group-line-format-spec))

(defun gnus-summary-mode-line-format-spec ()
(format "Gnus  %s/%d %s" gnus-tmp-group-name gnus-tmp-article-number gnus-tmp-unread-and-unselected))
(defvar gnus-summary-mode-line-format-spec
  (gnus-byte-code 'gnus-summary-mode-line-format-spec))

(defun gnus-group-mode-line-format-spec ()
(format "Gnus  List of groups   {%s:%s}  " gnus-tmp-news-method gnus-tmp-news-server))
(defvar gnus-group-mode-line-format-spec 
  (gnus-byte-code 'gnus-group-mode-line-format-spec))

(defun gnus-article-mode-line-format-spec ()
(format "Gnus  %s/%d %s" gnus-tmp-group-name gnus-tmp-article-number gnus-tmp-subject))
(defvar gnus-article-mode-line-format-spec
  (gnus-byte-code 'gnus-article-mode-line-format-spec))

(defvar gnus-old-specs 
  '((article-mode . "Gnus  %G/%A %S")
    (group-mode . "Gnus  List of groups   {%M:%S}  ")
    (summary-mode . "Gnus  %G/%A %Z")
    (group . "%M%S%p%5y: %(%g%)\n")
    (summary-dummy . "*  :                          : %S\n")
    (summary . "%U%R%z%I%(%[%4L: %-20,20n%]%) %s\n")))

;;; Phew.  All that gruft is over, fortunately.  Perhaps one should
;;; hand-optimize the functions above, though.

(defvar gnus-summary-mark-positions nil)
(defvar gnus-group-mark-positions nil)

(defvar gnus-summary-expunge-below nil)
(defvar gnus-reffed-article-number nil)

;;; Let the byte-compiler know that we know about this variable.
(defvar rmail-default-rmail-file)

(defvar gnus-cache-removeable-articles nil)

(defconst gnus-summary-local-variables 
  '(gnus-newsgroup-name 
    gnus-newsgroup-begin gnus-newsgroup-end 
    gnus-newsgroup-last-rmail gnus-newsgroup-last-mail 
    gnus-newsgroup-last-folder gnus-newsgroup-last-file 
    gnus-newsgroup-auto-expire gnus-newsgroup-unreads 
    gnus-newsgroup-unselected gnus-newsgroup-marked
    gnus-newsgroup-reads
    gnus-newsgroup-replied gnus-newsgroup-expirable
    gnus-newsgroup-processable gnus-newsgroup-killed
    gnus-newsgroup-bookmarks gnus-newsgroup-dormant
    gnus-newsgroup-headers gnus-newsgroup-threads
    gnus-newsgroup-prepared
    gnus-current-article gnus-current-headers gnus-have-all-headers
    gnus-last-article gnus-article-internal-prepare-hook
    gnus-newsgroup-dependencies gnus-newsgroup-selected-overlay
    gnus-newsgroup-scored gnus-newsgroup-kill-headers
    gnus-newsgroup-async
    gnus-score-alist gnus-current-score-file gnus-summary-expunge-below 
    gnus-summary-mark-below gnus-newsgroup-active gnus-scores-exclude-files
    gnus-newsgroup-history gnus-newsgroup-ancient
    (gnus-newsgroup-adaptive . gnus-use-adaptive-scoring)
    (gnus-newsgroup-expunged-tally . 0)
    gnus-cache-removeable-articles
    gnus-newsgroup-data gnus-newsgroup-data-reverse
    gnus-newsgroup-limit gnus-newsgroup-limits)
  "Variables that are buffer-local to the summary buffers.")

(defconst gnus-bug-message
  "Sending a bug report to the Gnus Towers.
========================================

The buffer below is a mail buffer.  When you press `C-c C-c', it will
be sent to the Gnus Bug Exterminators. 

At the bottom of the buffer you'll see lots of variable settings.
Please do not delete those.  They will tell the Bug People what your
environment is, so that it will be easier to locate the bugs.

If you have found a bug that makes Emacs go \"beep\", set
debug-on-error to t (`M-x set-variable RET debug-on-error RET t RET') 
and include the backtrace in your bug report.

Please describe the bug in annoying, painstaking detail.

Thank you for your help in stamping out bugs.
")

;;; End of variables.

;; Define some autoload functions Gnus might use.
(eval-and-compile

  ;; This little mapcar goes through the list below and marks the
  ;; symbols in question as autoloaded functions.
  (mapcar 
   (lambda (package)
     (let ((interactive (nth 1 (memq ':interactive package))))
       (mapcar 
	(lambda (function)
	  (let (keymap)
	    (when (consp function)
	      (setq keymap (car (memq 'keymap function)))
	      (setq function (car function)))
	    (autoload function (car package) nil interactive keymap)))
	(if (eq (cadr package) ':interactive)
	    (cdddr package)
	  (cdr package)))))
   '(("metamail" metamail-buffer)
     ("info" Info-goto-node)
     ("hexl" hexl-hex-string-to-integer)
     ("pp" pp pp-to-string pp-eval-expression)
     ("mail-extr" mail-extract-address-components)
     ("nnmail" nnmail-split-fancy nnmail-article-group)
     ("nnvirtual" nnvirtual-catchup-group)
     ("timezone" timezone-make-date-arpa-standard timezone-fix-time
      timezone-make-sortable-date timezone-make-time-string)
     ("sendmail" mail-position-on-field mail-setup)
     ("rmailout" rmail-output)
     ("rnewspost" news-mail-other-window news-reply-yank-original 
      news-caesar-buffer-body)
     ("rmail" rmail-insert-rmail-file-header rmail-count-new-messages
      rmail-show-message)
     ("gnus-soup" :interactive t
      gnus-group-brew-soup gnus-brew-soup gnus-soup-add-article 
      gnus-soup-send-replies gnus-soup-save-areas gnus-soup-pack-packet)
     ("nnsoup" nnsoup-pack-replies)
     ("gnus-mh" gnus-mh-mail-setup gnus-summary-save-article-folder 
      gnus-Folder-save-name gnus-folder-save-name)
     ("gnus-mh" :interactive t gnus-mh-save-in-folder)
     ("gnus-vis" gnus-group-make-menu-bar gnus-summary-make-menu-bar
      gnus-server-make-menu-bar gnus-article-make-menu-bar
      gnus-browse-make-menu-bar gnus-highlight-selected-summary
      gnus-summary-highlight-line gnus-carpal-setup-buffer
      gnus-article-add-button)
     ("gnus-vis" :interactive t
      gnus-article-push-button gnus-article-press-button 
      gnus-article-highlight gnus-article-highlight-some 
      gnus-article-hide gnus-article-hide-signature 
      gnus-article-highlight-headers gnus-article-highlight-signature 
      gnus-article-add-buttons gnus-article-add-buttons-to-head 
      gnus-article-next-button)
     ("gnus-demon" gnus-demon-add-nocem gnus-demon-add-scanmail
      gnus-demon-add-disconnection gnus-demon-add-handler
      gnus-demon-remove-handler)
     ("gnus-demon" :interactive t
      gnus-demon-init gnus-demon-cancel)
     ("gnus-nocem" gnus-nocem-scan-groups gnus-nocem-close)
     ("gnus-srvr" gnus-server-enter-setup-buffer)
     ("gnus-cite" :interactive t
      gnus-article-highlight-citation gnus-article-hide-citation-maybe 
      gnus-article-hide-citation)
     ("gnus-kill" gnus-kill gnus-apply-kill-file-internal 
      gnus-kill-file-edit-file gnus-kill-file-raise-followups-to-author 
      gnus-execute gnus-expunge)
     ("gnus-cache" gnus-cache-possibly-enter-article gnus-cache-save-buffers
      gnus-cache-possibly-remove-articles gnus-cache-request-article
      gnus-cache-retrieve-headers gnus-cache-possibly-alter-active
      gnus-cache-enter-remove-article)
     ("gnus-cache" :interactive t gnus-jog-cache)
     ("gnus-score" :interactive t
      gnus-summary-increase-score gnus-summary-lower-score
      gnus-score-flush-cache gnus-score-close 
      gnus-score-raise-same-subject-and-select 
      gnus-score-raise-same-subject gnus-score-default 
      gnus-score-raise-thread gnus-score-lower-same-subject-and-select 
      gnus-score-lower-same-subject gnus-score-lower-thread 
      gnus-possibly-score-headers)
     ("gnus-score" 
      (gnus-summary-score-map keymap) gnus-score-save gnus-score-headers
      gnus-current-score-file-nondirectory gnus-score-adaptive
      gnus-score-find-trace gnus-score-file-name)
     ("gnus-edit" :interactive t gnus-score-customize)
     ("gnus-topic" gnus-topic-fold gnus-group-prepare-topics)
     ("gnus-topic" :interactive t
      gnus-group-add-to-topic gnus-topic-toggle-topic)
     ("gnus-uu" (gnus-uu-extract-map keymap) (gnus-uu-mark-map keymap))
     ("gnus-uu" :interactive t
      gnus-uu-digest-mail-forward gnus-uu-digest-post-forward 
      gnus-uu-mark-series gnus-uu-mark-region 
      gnus-uu-mark-by-regexp gnus-uu-mark-all 
      gnus-uu-mark-sparse gnus-uu-mark-thread gnus-uu-decode-uu 
      gnus-uu-decode-uu-and-save gnus-uu-decode-unshar 
      gnus-uu-decode-unshar-and-save gnus-uu-decode-save 
      gnus-uu-decode-binhex gnus-uu-decode-uu-view 
      gnus-uu-decode-uu-and-save-view gnus-uu-decode-unshar-view 
      gnus-uu-decode-unshar-and-save-view gnus-uu-decode-save-view 
      gnus-uu-decode-binhex-view)
     ("gnus-msg" (gnus-summary-send-map keymap)
      gnus-mail-yank-original gnus-mail-send-and-exit
      gnus-sendmail-setup-mail gnus-article-mail 
      gnus-inews-message-id)
     ("gnus-msg" :interactive t
      gnus-group-post-news gnus-group-mail 
      gnus-summary-post-news gnus-summary-followup 
      gnus-summary-followup-with-original 
      gnus-summary-followup-and-reply 
      gnus-summary-followup-and-reply-with-original 
      gnus-summary-cancel-article gnus-summary-supersede-article 
      gnus-post-news gnus-inews-news gnus-cancel-news 
      gnus-summary-reply gnus-summary-reply-with-original 
      gnus-summary-mail-forward gnus-summary-mail-other-window gnus-bug)
     ("gnus-vm" gnus-vm-mail-setup)
     ("gnus-vm" :interactive t gnus-summary-save-in-vm
      gnus-summary-save-article-vm gnus-yank-article))))



;; Fix by Hallvard B Furuseth <h.b.furuseth@usit.uio.no>.
;; If you want the cursor to go somewhere else, set these two
;; functions in some startup hook to whatever you want.
(defalias 'gnus-summary-position-point 'gnus-goto-colon)
(defalias 'gnus-group-position-point 'gnus-goto-colon)

;;; Various macros and substs.

(defmacro gnus-eval-in-buffer-window (buffer &rest forms)
  "Pop to BUFFER, evaluate FORMS, and then return to the original window."
  (` (let ((GnusStartBufferWindow (selected-window)))
       (unwind-protect
	   (progn
	     (pop-to-buffer (, buffer))
	     (,@ forms))
	 (select-window GnusStartBufferWindow)))))

(defmacro gnus-gethash (string hashtable)
  "Get hash value of STRING in HASHTABLE."
  (` (symbol-value (intern-soft (, string) (, hashtable)))))

(defmacro gnus-sethash (string value hashtable)
  "Set hash value.  Arguments are STRING, VALUE, and HASHTABLE."
  (` (set (intern (, string) (, hashtable)) (, value))))

(defmacro gnus-intern-safe (string hashtable)
  "Set hash value.  Arguments are STRING, VALUE, and HASHTABLE."
  `(let ((symbol (intern ,string ,hashtable)))
     (or (boundp symbol)
	 (setq symbol nil))
     symbol))

(defmacro gnus-active (group)
  "Get active info on GROUP."
  `(gnus-gethash ,group gnus-active-hashtb))

(defmacro gnus-set-active (group active)
  "Set GROUP's active info."
  `(gnus-sethash ,group ,active gnus-active-hashtb))

;; modified by MORIOKA Tomohiko <morioka@jaist.ac.jp>
;;   function `substring' might cut on a middle of multi-octet
;;   character.
(defun gnus-truncate-string (str width)
  (substring str 0 width))

;; Added by Geoffrey T. Dairiki <dairiki@u.washington.edu>.  A safe way
;; to limit the length of a string.  This function is necessary since
;; `(substr "abc" 0 30)' pukes with "Args out of range".
(defsubst gnus-limit-string (str width)
  (if (> (length str) width)
      (substring str 0 width)
    str))

(defsubst gnus-simplify-subject-re (subject)
  "Remove \"Re:\" from subject lines."
  (if (string-match "^[Rr][Ee]: *" subject)
      (substring subject (match-end 0))
    subject))

(defsubst gnus-goto-char (point)
  (and point (goto-char point)))

(defmacro gnus-buffer-exists-p (buffer)
  (` (and (, buffer)
	  (funcall (if (stringp (, buffer)) 'get-buffer 'buffer-name)
		   (, buffer)))))

(defmacro gnus-kill-buffer (buffer)
  (` (if (gnus-buffer-exists-p (, buffer))
	 (kill-buffer (, buffer)))))

(defsubst gnus-point-at-bol ()
  "Return point at the beginning of the line."
  (let ((p (point)))
    (beginning-of-line)
    (prog1
	(point)
      (goto-char p))))

(defsubst gnus-point-at-eol ()
  "Return point at the end of the line."
  (let ((p (point)))
    (end-of-line)
    (prog1
	(point)
      (goto-char p))))

;; Delete the current line (and the next N lines.);
(defmacro gnus-delete-line (&optional n)
  `(delete-region (progn (beginning-of-line) (point))
		  (progn (forward-line ,(or n 1)) (point))))

;; Suggested by Brian Edmonds <edmonds@cs.ubc.ca>.
(defvar gnus-init-inhibit nil)
(defun gnus-read-init-file (&optional inhibit-next)
  (if gnus-init-inhibit
      (setq gnus-init-inhibit nil)
    (setq gnus-init-inhibit inhibit-next)
    (and gnus-init-file
	 (or (and (file-exists-p gnus-init-file) 
		  ;; Don't try to load a directory.
		  (not (file-directory-p gnus-init-file)))
	     (file-exists-p (concat gnus-init-file ".el"))
	     (file-exists-p (concat gnus-init-file ".elc")))
	 (load gnus-init-file nil t))))

;; Info access macros.

(defmacro gnus-info-group (info)
  `(nth 0 ,info))
(defmacro gnus-info-rank (info)
  `(nth 1 ,info))
(defmacro gnus-info-read (info)
  `(nth 2 ,info))
(defmacro gnus-info-marks (info)
  `(nth 3 ,info))
(defmacro gnus-info-method (info)
  `(nth 4 ,info))
(defmacro gnus-info-params (info)
  `(nth 5 ,info))

(defmacro gnus-info-level (info)
  `(let ((rank (gnus-info-rank ,info)))
     (if (consp rank)
	 (car rank)
       rank)))
(defmacro gnus-info-score (info)
  `(let ((rank (gnus-info-rank ,info)))
     (or (and (consp rank) (cdr rank)) 0)))

(defmacro gnus-info-set-group (info group)
  `(setcar ,info ,group))
(defmacro gnus-info-set-rank (info rank)
  `(setcar (cdr ,info) ,rank))
(defmacro gnus-info-set-read (info read)
  `(setcar (cddr ,info) ,read))
(defmacro gnus-info-set-marks (info marks)
  `(setcar (cdddr ,info) ,marks))
(defmacro gnus-info-set-method (info method)
  `(setcar (cddddr ,info) ,method))
(defmacro gnus-info-set-params (info params)
  `(setcar (cdddddr ,info) ,params))

(defmacro gnus-info-set-level (info level)
  `(let ((rank (cdr ,info)))
     (if (consp (car rank))
	 (setcar (car rank) ,level)
       (setcar rank ,level))))
(defmacro gnus-info-set-score (info score)
  `(let ((rank (cdr ,info)))
     (if (consp (car rank))
	 (setcdr (car rank) ,score)
       (setcar rank (cons (car rank) ,score)))))

(defmacro gnus-get-info (group)
  `(nth 2 (gnus-gethash ,group gnus-newsrc-hashtb)))

;;; Load the user startup file.
;; (eval '(gnus-read-init-file 'inhibit))

;;; Load the compatability functions. 

(require 'gnus-cus)
(require 'gnus-ems)


;;;
;;; Gnus Utility Functions
;;;

(defun gnus-extract-address-components (from)
  (let (name address)
    ;; First find the address - the thing with the @ in it.  This may
    ;; not be accurate in mail addresses, but does the trick most of
    ;; the time in news messages.
    (if (string-match "\\b[^@ \t<>]+[!@][^@ \t<>]+\\b" from)
	(setq address (substring from (match-beginning 0) (match-end 0))))
    ;; Then we check whether the "name <address>" format is used.
    (and address
 	 ;; Fix by MORIOKA Tomohiko <morioka@jaist.ac.jp>
 	 ;; Linear white space is not required.
 	 (string-match (concat "[ \t]*<" (regexp-quote address) ">") from)
 	 (and (setq name (substring from 0 (match-beginning 0)))
	      ;; Strip any quotes from the name.
	      (string-match "\".*\"" name)
	      (setq name (substring name 1 (1- (match-end 0))))))
    ;; If not, then "address (name)" is used.
    (or name
	(and (string-match "(.+)" from)
	     (setq name (substring from (1+ (match-beginning 0)) 
				   (1- (match-end 0)))))
	(and (string-match "()" from)
	     (setq name address))
	;; Fix by MORIOKA Tomohiko <morioka@jaist.ac.jp>.
	;; XOVER might not support folded From headers.
	(and (string-match "(.*" from)
	     (setq name (substring from (1+ (match-beginning 0)) 
				   (match-end 0)))))
    ;; Fix by Hallvard B Furuseth <h.b.furuseth@usit.uio.no>.
    (list (or name from) (or address from))))

(defun gnus-fetch-field (field)
  "Return the value of the header FIELD of current article."
  (save-excursion
    (save-restriction
      (let ((case-fold-search t))
	(gnus-narrow-to-headers)
	(mail-fetch-field field)))))

(defun gnus-goto-colon ()
  (beginning-of-line)
  (search-forward ":" (gnus-point-at-eol) t))

(defun gnus-narrow-to-headers ()
  "Narrow to the head of an article."
  (widen)
  (narrow-to-region
   (goto-char (point-min))
   (if (search-forward "\n\n" nil t)
       (1- (point))
     (point-max)))
  (goto-char (point-min)))

(defun gnus-update-format-specifications ()
  (gnus-make-thread-indent-array)

  (let ((formats '(summary summary-dummy group 
			   summary-mode group-mode article-mode))
	old-format new-format)
    (while formats
      (setq new-format (symbol-value
			(intern (format "gnus-%s-line-format" (car formats)))))
      (or (and (setq old-format (cdr (assq (car formats) gnus-old-specs)))
	       (equal old-format new-format))
	  (set (intern (format "gnus-%s-line-format-spec" (car formats)))
	       (if (not (stringp new-format)) new-format
		 (gnus-parse-format
		  new-format
		  (symbol-value 
		   (intern (format "gnus-%s-line-format-alist"
				   (if (eq (car formats) 'article-mode)
				       'summary-mode (car formats)))))
		  (not (string-match "mode$" (symbol-name (car formats))))))))
      (setq gnus-old-specs (cons (cons (car formats) new-format)
				 (delq (assq (car formats) gnus-old-specs)
				       gnus-old-specs)))
      (setq formats (cdr formats))))
      
  (gnus-update-group-mark-positions)
  (gnus-update-summary-mark-positions)

  (if (and (string-match "%D" gnus-group-line-format)
	   (not gnus-description-hashtb)
	   gnus-read-active-file)
      (gnus-read-all-descriptions-files)))

(defun gnus-update-summary-mark-positions ()
  (save-excursion
    (let ((gnus-replied-mark 129)
	  (gnus-score-below-mark 130)
	  (gnus-score-over-mark 130)
	  (thread nil)
	  pos)
      (gnus-set-work-buffer)
      (gnus-summary-insert-line 
       nil [0 "" "" "" "" "" 0 0 ""]  0 nil 128 t nil "" nil 1)
      (goto-char (point-min))
      (setq pos (list (cons 'unread (and (search-forward "\200" nil t)
					 (- (point) 2)))))
      (goto-char (point-min))
      (setq pos (cons (cons 'replied (and (search-forward "\201" nil t)
					  (- (point) 2))) pos))
      (goto-char (point-min))
      (setq pos (cons (cons 'score (and (search-forward "\202" nil t)
					(- (point) 2))) pos))
      (setq gnus-summary-mark-positions pos))))

(defun gnus-update-group-mark-positions ()
  (save-excursion
    (let ((gnus-process-mark 128)
	  (gnus-group-marked '("dummy.group")))
      (gnus-set-active "dummy.group" '(0 . 0))
      (gnus-set-work-buffer)
      (gnus-group-insert-group-line nil "dummy.group" 0 nil 0 nil)
      (goto-char (point-min))
      (setq gnus-group-mark-positions
	    (list (cons 'process (and (search-forward "\200" nil t)
				      (- (point) 2))))))))

(defun gnus-mouse-face-function (form)
  (` (put-text-property
      (point) (progn (insert (, form)) (point))
      ,gnus-mouse-face-prop gnus-mouse-face)))

(defun gnus-max-width-function (el max-width)
  (or (numberp max-width) (signal 'wrong-type-argument '(numberp max-width)))
  (` (let ((val (eval (, el))))
       (if (numberp val)
	   (setq val (int-to-string val)))
       (if (> (length val) (, max-width))
	   (substring val 0 (, max-width))
	 val))))

(defun gnus-parse-format (format spec-alist &optional insert)
  ;; This function parses the FORMAT string with the help of the
  ;; SPEC-ALIST and returns a list that can be eval'ed to return the
  ;; string.  If the FORMAT string contains the specifiers %( and %)
  ;; the text between them will have the mouse-face text property.
  (let (did-insert result)
    (setq result
	  (if (string-match "\\`\\(.*\\)%(\\(.*\\)%)\\(.*\n?\\)\\'" format)
	      ;; This is a complex that should have mouse-face set on
	      ;; parts of the format.
	      (if (and gnus-visual gnus-mouse-face)
		  (let ((pre (substring format (match-beginning 1)
					(match-end 1)))
			(button (substring format (match-beginning 2)
					   (match-end 2)))
			(post (substring format (match-beginning 3)
					 (match-end 3))))
		    (if (not insert)
			(list 'concat
			      (gnus-parse-simple-format pre spec-alist)
			      (gnus-mouse-face-function 
			       (gnus-parse-simple-format button spec-alist))
			      (gnus-parse-simple-format post spec-alist))
		      (setq did-insert t)
		      ;; So here we want mouse face and insert. 
		      (list 'progn
			    (list 'insert
				  (gnus-parse-simple-format pre spec-alist))
			    (gnus-mouse-face-function 
			     (gnus-parse-simple-format button spec-alist))
			    (list 'insert
				  (gnus-parse-simple-format
				   post spec-alist)))))
		;; We don't want faces.
		(gnus-parse-simple-format
		 (concat (substring format (match-beginning 1) (match-end 1))
			 (substring format (match-beginning 2) (match-end 2))
			 (substring format (match-beginning 3) (match-end 3)))
		 spec-alist))
	    ;; This is a simple format.
	    (gnus-parse-simple-format format spec-alist)))
    (if (and insert (not did-insert))
	(list 'insert result) 
      result)))

(defun gnus-parse-simple-format (format spec-alist)
  ;; This function parses the FORMAT string with the help of the
  ;; SPEC-ALIST and returns a list that can be eval'ed to return the
  ;; string.  The list will consist of the symbol `format', a format
  ;; specification string, and a list of forms depending on the
  ;; SPEC-ALIST.
  (let ((max-width 0)
	spec flist fstring newspec elem beg)
    (save-excursion
      (gnus-set-work-buffer)
      (insert format)
      (goto-char (point-min))
      (while (re-search-forward "%[-0-9]*\\(,[0-9]+\\)?\\([^0-9]\\)\\(.\\)?"
				nil t)
	(setq spec (string-to-char (match-string 2)))
	;; First check if there are any specs that look anything like
	;; "%12,12A", ie. with a "max width specification".  These have
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
	(and (eq (car elem) 'gnus-tmp-user-defined)
	     (setq elem
		   (list 
		    (list (intern (concat "gnus-user-format-function-"
					  (match-string 3)))
			  'gnus-tmp-header)
		    ?s))
	     (delete-region (match-beginning 3) (match-end 3)))
	(if (not (zerop max-width))
	    (let ((el (car elem)))
	      (cond ((= (car (cdr elem)) ?c) 
		     (setq el (list 'char-to-string el)))
		    ((= (car (cdr elem)) ?d)
		     (numberp el) (setq el (list 'int-to-string el))))
	      (setq flist (cons (gnus-max-width-function el max-width)
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

(defun gnus-set-work-buffer ()
  (if (get-buffer gnus-work-buffer)
      (progn
	(set-buffer gnus-work-buffer)
	(erase-buffer))
    (set-buffer (get-buffer-create gnus-work-buffer))
    (kill-all-local-variables)
    (buffer-disable-undo (current-buffer))
    (gnus-add-current-to-buffer-list)))

;; Article file names when saving.

(defun gnus-Numeric-save-name (newsgroup headers &optional last-file)
  "Generate file name from NEWSGROUP, HEADERS, and optional LAST-FILE.
If variable `gnus-use-long-file-name' is nil, it is ~/News/News.group/num.
Otherwise, it is like ~/News/news/group/num."
  (let ((default
	  (expand-file-name
	   (concat (if (gnus-use-long-file-name 'not-save)
		       (gnus-capitalize-newsgroup newsgroup)
		     (gnus-newsgroup-directory-form newsgroup))
		   "/" (int-to-string (mail-header-number headers)))
	   (or gnus-article-save-directory "~/News"))))
    (if (and last-file
	     (string-equal (file-name-directory default)
			   (file-name-directory last-file))
	     (string-match "^[0-9]+$" (file-name-nondirectory last-file)))
	default
      (or last-file default))))

(defun gnus-numeric-save-name (newsgroup headers &optional last-file)
  "Generate file name from NEWSGROUP, HEADERS, and optional LAST-FILE.
If variable `gnus-use-long-file-name' is non-nil, it is
~/News/news.group/num.  Otherwise, it is like ~/News/news/group/num."
  (let ((default
	  (expand-file-name
	   (concat (if (gnus-use-long-file-name 'not-save)
		       newsgroup
		     (gnus-newsgroup-directory-form newsgroup))
		   "/" (int-to-string (mail-header-number headers)))
	   (or gnus-article-save-directory "~/News"))))
    (if (and last-file
	     (string-equal (file-name-directory default)
			   (file-name-directory last-file))
	     (string-match "^[0-9]+$" (file-name-nondirectory last-file)))
	default
      (or last-file default))))

(defun gnus-Plain-save-name (newsgroup headers &optional last-file)
  "Generate file name from NEWSGROUP, HEADERS, and optional LAST-FILE.
If variable `gnus-use-long-file-name' is non-nil, it is
~/News/News.group.  Otherwise, it is like ~/News/news/group/news."
  (or last-file
      (expand-file-name
       (if (gnus-use-long-file-name 'not-save)
	   (gnus-capitalize-newsgroup newsgroup)
	 (concat (gnus-newsgroup-directory-form newsgroup) "/news"))
       (or gnus-article-save-directory "~/News"))))

(defun gnus-plain-save-name (newsgroup headers &optional last-file)
  "Generate file name from NEWSGROUP, HEADERS, and optional LAST-FILE.
If variable `gnus-use-long-file-name' is non-nil, it is
~/News/news.group.  Otherwise, it is like ~/News/news/group/news."
  (or last-file
      (expand-file-name
       (if (gnus-use-long-file-name 'not-save)
	   newsgroup
	 (concat (gnus-newsgroup-directory-form newsgroup) "/news"))
       (or gnus-article-save-directory "~/News"))))

;; For subscribing new newsgroup

(defun gnus-subscribe-hierarchical-interactive (groups)
  (let ((groups (sort groups 'string<))
	prefixes prefix start ans group starts)
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
  (let ((groups (cdr gnus-newsrc-alist))
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
			(setq before (match-string 1))
			(string< before newgroup)))))
	;; Remove tail of newsgroup name (eg. a.b.c -> a.b)
	(setq groupkey
	      (if (string-match "^\\(.*\\)\\.[^.]+$" groupkey)
		  (substring groupkey (match-beginning 1) (match-end 1)))))
      (gnus-subscribe-newsgroup newgroup before))))

(defun gnus-subscribe-interactively (newsgroup)
  "Subscribe new NEWSGROUP interactively.
It is inserted in hierarchical newsgroup order if subscribed.  If not,
it is killed."
  (if (gnus-y-or-n-p (format "Subscribe new newsgroup: %s " newsgroup))
      (gnus-subscribe-hierarchically newsgroup)
    (setq gnus-killed-list (cons newsgroup gnus-killed-list))))

(defun gnus-subscribe-zombies (newsgroup)
  "Make new NEWSGROUP a zombie group."
  (setq gnus-zombie-list (cons newsgroup gnus-zombie-list)))

(defun gnus-subscribe-newsgroup (newsgroup &optional next)
  "Subscribe new NEWSGROUP.
If NEXT is non-nil, it is inserted before NEXT.  Otherwise it is made
the first newsgroup."
  ;; We subscribe the group by changing its level to `subscribed'.
  (gnus-group-change-level 
   newsgroup gnus-level-default-subscribed
   gnus-level-killed (gnus-gethash (or next "dummy.group") gnus-newsrc-hashtb))
  (gnus-message 5 "Subscribe newsgroup: %s" newsgroup))

;; For directories

(defun gnus-newsgroup-directory-form (newsgroup)
  "Make hierarchical directory name from NEWSGROUP name."
  (let ((newsgroup (gnus-newsgroup-saveable-name newsgroup))
	(len (length newsgroup))
	idx)
    ;; If this is a foreign group, we don't want to translate the
    ;; entire name.  
    (if (setq idx (string-match ":" newsgroup))
	(aset newsgroup idx ?/)
      (setq idx 0))
    ;; Replace all occurrences of `.' with `/'.
    (while (< idx len)
      (if (= (aref newsgroup idx) ?.)
	  (aset newsgroup idx ?/))
      (setq idx (1+ idx)))
    newsgroup))

(defun gnus-newsgroup-saveable-name (group)
  ;; Replace any slashes in a group name (eg. an ange-ftp nndoc group)
  ;; with dots.
  (gnus-replace-chars-in-string group ?/ ?.))

(defun gnus-make-directory (dir)
  "Make DIRECTORY recursively."
  ;; Why don't we use `(make-directory dir 'parents)'? That's just one
  ;; of the many mysteries of the universe.
  (let* ((dir (expand-file-name dir default-directory))
	 dirs err)
    (if (string-match "/$" dir)
	(setq dir (substring dir 0 (match-beginning 0))))
    ;; First go down the path until we find a directory that exists.
    (while (not (file-exists-p dir))
      (setq dirs (cons dir dirs))
      (string-match "/[^/]+$" dir)
      (setq dir (substring dir 0 (match-beginning 0))))
    ;; Then create all the subdirs.
    (while (and dirs (not err))
      (condition-case ()
	  (make-directory (car dirs))
	(error (setq err t)))
      (setq dirs (cdr dirs)))
    ;; We return whether we were successful or not. 
    (not dirs)))

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
    subject))

;; Remove any leading "re:"s, any trailing paren phrases, and simplify
;; all whitespace.
;; Written by Stainless Steel Rat <ratinox@ccs.neu.edu>.
(defun gnus-simplify-buffer-fuzzy ()
  (goto-char (point-min))
  (while (or
	  (looking-at "^[ \t]*\\(re\\|fwd\\)[[{(^0-9]*[])}]?[:;][ \t]*")
	  (looking-at "^[[].*:[ \t].*[]]$"))
    (goto-char (point-min))
    (while (re-search-forward "^[ \t]*\\(re\\|fwd\\)[[{(^0-9]*[])}]?[:;][ \t]*"
			      nil t)
      (replace-match "" t t))
    (goto-char (point-min))
    (while (re-search-forward "^[[].*:[ \t].*[]]$" nil t)
      (goto-char (match-end 0))
      (delete-char -1)
      (delete-region 
       (progn (goto-char (match-beginning 0)))
       (re-search-forward ":"))))
  (goto-char (point-min))
  (while (re-search-forward "[ \t\n]*([^()]*)[ \t]*$" nil t)
    (replace-match "" t t))
  (goto-char (point-min))
  (while (re-search-forward "[ \t]+" nil t)
    (replace-match " " t t))
  (goto-char (point-min))
  (while (re-search-forward "[ \t]+$" nil t)
    (replace-match "" t t))
  (goto-char (point-min))
  (while (re-search-forward "^[ \t]+" nil t)
    (replace-match "" t t))
  (goto-char (point-min))
  (if gnus-simplify-subject-fuzzy-regexp
      (if (listp gnus-simplify-subject-fuzzy-regexp)
	  (let ((list gnus-simplify-subject-fuzzy-regexp))
	    (while list
	      (goto-char (point-min))
	      (while (re-search-forward (car list) nil t)
		(replace-match "" t t))
	      (setq list (cdr list))))
	(while (re-search-forward gnus-simplify-subject-fuzzy-regexp nil t)
	  (replace-match "" t t)))))

(defun gnus-simplify-subject-fuzzy (subject)
  "Siplify a subject string fuzzily."
  (let ((case-fold-search t))
    (save-excursion
      (gnus-set-work-buffer)
      (insert subject)
      (inline (gnus-simplify-buffer-fuzzy))
      (buffer-string))))

;; Add the current buffer to the list of buffers to be killed on exit. 
(defun gnus-add-current-to-buffer-list ()
  (or (memq (current-buffer) gnus-buffer-list)
      (setq gnus-buffer-list (cons (current-buffer) gnus-buffer-list))))

(defun gnus-string> (s1 s2)
  (not (or (string< s1 s2)
	   (string= s1 s2))))

;; Functions accessing headers.
;; Functions are more convenient than macros in some cases.

(defun gnus-header-number (header)
  (mail-header-number header))

(defun gnus-header-subject (header)
  (mail-header-subject header))

(defun gnus-header-from (header)
  (mail-header-from header))

(defun gnus-header-xref (header)
  (mail-header-xref header))

(defun gnus-header-lines (header)
  (mail-header-lines header))

(defun gnus-header-date (header)
  (mail-header-date header))

(defun gnus-header-id (header)
  (mail-header-id header))

(defun gnus-header-message-id (header)
  (mail-header-id header))

(defun gnus-header-chars (header)
  (mail-header-chars header))

(defun gnus-header-references (header)
  (mail-header-references header))

;;; General various misc type functions.

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
	gnus-newsrc-alist nil
	gnus-newsrc-hashtb nil
	gnus-killed-list nil
	gnus-zombie-list nil
	gnus-killed-hashtb nil
	gnus-active-hashtb nil
	gnus-moderated-list nil
	gnus-description-hashtb nil
	gnus-newsgroup-headers nil
	gnus-newsgroup-name nil
	gnus-server-alist nil
	gnus-opened-servers nil
	gnus-current-select-method nil)
  ;; Reset any score variables.
  (and gnus-use-scoring (gnus-score-close))
  ;; Kill the startup file.
  (and gnus-current-startup-file
       (get-file-buffer gnus-current-startup-file)
       (kill-buffer (get-file-buffer gnus-current-startup-file)))
  ;; Save any cache buffers.
  (and gnus-use-cache (gnus-cache-save-buffers))
  ;; Clear the dribble buffer.
  (gnus-dribble-clear)
  ;; Close down NoCeM.
  (and gnus-use-nocem (gnus-nocem-close))
  ;; Shut down the demons.
  (and gnus-use-demon (gnus-demon-cancel))
  ;; Kill global KILL file buffer.
  (if (get-file-buffer (gnus-newsgroup-kill-file nil))
      (kill-buffer (get-file-buffer (gnus-newsgroup-kill-file nil))))
  (gnus-kill-buffer nntp-server-buffer)
  ;; Kill Gnus buffers.
  (while gnus-buffer-list
    (gnus-kill-buffer (car gnus-buffer-list))
    (setq gnus-buffer-list (cdr gnus-buffer-list))))

(defun gnus-windows-old-to-new (setting)
  ;; First we take care of the really, really old Gnus 3 actions.
  (if (symbolp setting)
      (setq setting 
	    (cond ((memq setting '(SelectArticle))
		   'article)
		  ((memq setting '(SelectSubject ExpandSubject))
		   'summary)
		  ((memq setting '(SelectNewsgroup ExitNewsgroup))
		   'group)
		  (t setting))))
  (if (or (listp setting)
	  (not (and gnus-window-configuration
		    (memq setting '(group summary article)))))
      setting
    (let* ((setting (if (eq setting 'group) 
			(if (assq 'newsgroup gnus-window-configuration)
			    'newsgroup
			  'newsgroups) setting))
	   (elem (car (cdr (assq setting gnus-window-configuration))))
	   (total (apply '+ elem))
	   (types '(group summary article))
	   (pbuf (if (eq setting 'newsgroups) 'group 'summary))
	   (i 0)
	   perc
	   out)
      (while (< i 3)
	(or (not (numberp (nth i elem)))
	    (zerop (nth i elem))
	    (progn
	      (setq perc  (/ (* 1.0 (nth 0 elem)) total))
	      (setq out (cons (if (eq pbuf (nth i types))
				  (vector (nth i types) perc 'point)
				(vector (nth i types) perc))
			      out))))
	(setq i (1+ i)))
      (list (nreverse out)))))
	   
(defun gnus-add-configuration (conf)
  (setq gnus-buffer-configuration 
	(cons conf (delq (assq (car conf) gnus-buffer-configuration)
			 gnus-buffer-configuration))))

(defun gnus-configure-windows (setting &optional force)
  (setq setting (gnus-windows-old-to-new setting))
  (let ((r (if (symbolp setting)
	       (cdr (assq setting gnus-buffer-configuration))
	     setting))
	(in-buf (current-buffer))
	rule val w height hor ohor heights sub jump-buffer
	rel total to-buf all-visible)
    (or r (error "No such setting: %s" setting))

    (if (and (not force) (setq all-visible (gnus-all-windows-visible-p r)))
	;; All the windows mentioned are already visible, so we just
	;; put point in the assigned buffer, and do not touch the
	;; winconf. 
	(select-window (get-buffer-window all-visible t))
	 

      ;; Either remove all windows or just remove all Gnus windows.
      (if gnus-use-full-window
	  (delete-other-windows)
	(gnus-remove-some-windows)
	(switch-to-buffer nntp-server-buffer))

      (while r
	(setq hor (car r)
	      ohor nil)

	;; We have to do the (possible) horizontal splitting before the
	;; vertical. 
	(if (and (listp (car hor)) 
		 (eq (car (car hor)) 'horizontal))
	    (progn
	      (split-window 
	       nil
	       (if (integerp (nth 1 (car hor)))
		   (nth 1 (car hor))
		 (- (frame-width) (floor (* (frame-width) (nth 1 (car hor))))))
	       t)
	      (setq hor (cdr hor))))

	;; Go through the rules and eval the elements that are to be
	;; evaled.  
	(while hor
	  (if (setq val (if (vectorp (car hor)) (car hor) (eval (car hor))))
	      (progn
		;; Expand short buffer name.
		(setq w (aref val 0))
		(and (setq w (cdr (assq w gnus-window-to-buffer)))
		     (progn
		       (setq val (apply 'vector (mapcar 'identity val)))
		       (aset val 0 w)))
		(setq ohor (cons val ohor))))
	  (setq hor (cdr hor)))
	(setq rule (cons (nreverse ohor) rule))
	(setq r (cdr r)))
      (setq rule (nreverse rule))

      ;; We tally the window sizes.
      (setq total (window-height))
      (while rule
	(setq hor (car rule))
	(if (and (listp (car hor)) (eq (car (car hor)) 'horizontal))
	    (setq hor (cdr hor)))
	(setq sub 0)
	(while hor
	  (setq rel (aref (car hor) 1)
		heights (cons
			 (cond ((and (floatp rel) (= 1.0 rel))
				'x)
			       ((integerp rel)
				rel)
			       (t
				(max (floor (* total rel)) 4)))
			 heights)
		sub (+ sub (if (numberp (car heights)) (car heights) 0))
		hor (cdr hor)))
	(setq heights (nreverse heights)
	      hor (car rule))

	;; We then go through these heighs and create windows for them.
	(while heights
	  (setq height (car heights)
		heights (cdr heights))
	  (and (eq height 'x)
	       (setq height (- total sub)))
	  (and heights
	       (split-window nil height))
	  (setq to-buf (aref (car hor) 0))
	  (switch-to-buffer 
	   (cond ((not to-buf)
		  in-buf)
		 ((symbolp to-buf)
		  (symbol-value (aref (car hor) 0)))
		 (t
		  (aref (car hor) 0))))
	  (and (> (length (car hor)) 2)
	       (eq (aref (car hor) 2) 'point)
	       (setq jump-buffer (current-buffer)))
	  (other-window 1)
	  (setq hor (cdr hor)))
      
	(setq rule (cdr rule)))

      ;; Finally, we pop to the buffer that's supposed to have point. 
      (or jump-buffer (error "Missing `point' in spec for %s" setting))

      (select-window (get-buffer-window jump-buffer t))
      (set-buffer jump-buffer))))

(defun gnus-all-windows-visible-p (rule)
  (let (invisible hor jump-buffer val buffer)
    ;; Go through the rules and eval the elements that are to be
    ;; evaled.  
    (while (and rule (not invisible))
      (setq hor (car rule)
	    rule (cdr rule))
      (while (and hor (not invisible))
	(if (setq val (if (vectorp (car hor)) 
			  (car hor)
			(if (not (eq (car (car hor)) 'horizontal))
			    (eval (car hor)))))
	    (progn
	      ;; Expand short buffer name.
	      (setq buffer (or (cdr (assq (aref val 0) gnus-window-to-buffer))
			       (aref val 0)))
	      (setq buffer (if (symbolp buffer) (symbol-value buffer)
			     buffer))
	      (and (> (length val) 2) (eq 'point (aref val 2))
		   (setq jump-buffer buffer))
	      (setq invisible (not (and buffer (get-buffer-window buffer))))))
	(setq hor (cdr hor))))
    (and (not invisible) jump-buffer)))

(defun gnus-window-top-edge (&optional window)
  (nth 1 (window-edges window)))

(defun gnus-remove-some-windows ()
  (let ((buffers gnus-window-to-buffer)
	buf bufs lowest-buf lowest)
    (save-excursion
      ;; Remove windows on all known Gnus buffers.
      (while buffers
	(setq buf (cdr (car buffers)))
	(if (symbolp buf)
	    (setq buf (and (boundp buf) (symbol-value buf))))
	(and buf 
	     (get-buffer-window buf)
	     (progn
	       (setq bufs (cons buf bufs))
	       (pop-to-buffer buf)
	       (if (or (not lowest)
		       (< (gnus-window-top-edge) lowest))
		   (progn
		     (setq lowest (gnus-window-top-edge))
		     (setq lowest-buf buf)))))
	(setq buffers (cdr buffers)))
      ;; Remove windows on *all* summary buffers.
      (let (wins)
	(walk-windows
	 (lambda (win)
	   (let ((buf (window-buffer win)))
	     (if (string-match  "^\\*Summary" (buffer-name buf))
		 (progn
		   (setq bufs (cons buf bufs))
		   (pop-to-buffer buf)
		   (if (or (not lowest)
			   (< (gnus-window-top-edge) lowest))
		       (progn
			 (setq lowest-buf buf)
			 (setq lowest (gnus-window-top-edge))))))))))
      (and lowest-buf 
	   (progn
	     (pop-to-buffer lowest-buf)
	     (switch-to-buffer nntp-server-buffer)))
      (while bufs
	(and (not (eq (car bufs) lowest-buf))
	     (delete-windows-on (car bufs)))
	(setq bufs (cdr bufs))))))
			  
(defun gnus-version ()
  "Version numbers of this version of Gnus."
  (interactive)
  (let ((methods gnus-valid-select-methods)
	(mess gnus-version)
	meth)
    ;; Go through all the legal select methods and add their version
    ;; numbers to the total version string.  Only the backends that are
    ;; currently in use will have their message numbers taken into
    ;; consideration. 
    (while methods
      (setq meth (intern (concat (car (car methods)) "-version")))
      (and (boundp meth)
	   (stringp (symbol-value meth))
	   (setq mess (concat mess "; " (symbol-value meth))))
      (setq methods (cdr methods)))
    (gnus-message 2 mess)))

(defun gnus-info-find-node ()
  "Find Info documentation of Gnus."
  (interactive)
  ;; Enlarge info window if needed.
  (let ((mode major-mode))
    (gnus-configure-windows 'info)
    (Info-goto-node (car (cdr (assq mode gnus-info-nodes))))))

(defun gnus-replace-chars-in-string (string &rest pairs)
  "Replace characters in STRING from FROM to TO."
  (let ((string (substring string 0))	;Copy string.
	(len (length string))
	(idx 0)
	sym to)
    (or (zerop (% (length pairs) 2)) 
	(error "Odd number of translation pairs"))
    (setplist 'sym pairs)
    ;; Replace all occurrences of FROM with TO.
    (while (< idx len)
      (if (setq to (get 'sym (aref string idx)))
	  (aset string idx to))
      (setq idx (1+ idx)))
    string))

(defun gnus-days-between (date1 date2)
  ;; Return the number of days between date1 and date2.
  (- (gnus-day-number date1) (gnus-day-number date2)))

(defun gnus-day-number (date)
  (let ((dat (mapcar (lambda (s) (and s (string-to-int s)) )
		     (timezone-parse-date date))))
    (timezone-absolute-from-gregorian 
     (nth 1 dat) (nth 2 dat) (car dat))))

;; Returns a floating point number that says how many seconds have
;; lapsed between Jan 1 12:00:00 1970 and DATE.
(defun gnus-seconds-since-epoch (date)
  (let* ((tdate (mapcar (lambda (ti) (and ti (string-to-int ti)))
			(timezone-parse-date date)))
	 (ttime (mapcar (lambda (ti) (and ti (string-to-int ti)))
			(timezone-parse-time
			 (aref (timezone-parse-date date) 3))))
	 (edate (mapcar (lambda (ti) (and ti (string-to-int ti)))
			(timezone-parse-date "Jan 1 12:00:00 1970")))
	 (tday (- (timezone-absolute-from-gregorian 
		   (nth 1 tdate) (nth 2 tdate) (nth 0 tdate))
		  (timezone-absolute-from-gregorian 
		   (nth 1 edate) (nth 2 edate) (nth 0 edate)))))
    (+ (nth 2 ttime)
       (* (nth 1 ttime) 60)
       (* 1.0 (nth 0 ttime) 60 60)
       (* 1.0 tday 60 60 24))))

(defun gnus-file-newer-than (file date)
  (let ((fdate (nth 5 (file-attributes file))))
    (or (> (car fdate) (car date))
	(and (= (car fdate) (car date))
	     (> (nth 1 fdate) (nth 1 date))))))

(defun gnus-group-read-only-p (&optional group)
  "Check whether GROUP supports editing or not.
If GROUP is nil, `gnus-newsgroup-name' will be checked instead.  Note
that that variable is buffer-local to the summary buffers."
  (let ((group (or group gnus-newsgroup-name)))
    (not (gnus-check-backend-function 'request-replace-article group))))

(defun gnus-group-total-expirable-p (group)
  "Check whether GROUP is total-expirable or not."
  (let ((params (gnus-info-params (gnus-get-info group))))
    (or (memq 'total-expire params) 
	(cdr (assq 'total-expire params)) ; (total-expire . t)
	(and gnus-total-expirable-newsgroups ; Check var.
	     (string-match gnus-total-expirable-newsgroups group)))))

(defun gnus-group-auto-expirable-p (group)
  "Check whether GROUP is total-expirable or not."
  (let ((params (gnus-info-params (gnus-get-info group))))
    (or (memq 'auto-expire params) 
	(cdr (assq 'auto-expire params)) ; (auto-expire . t)
	(and gnus-auto-expirable-newsgroups ; Check var.
	     (string-match gnus-auto-expirable-newsgroups group)))))

(defun gnus-subject-equal (s1 s2)
  "Check whether two subjects are equal."
  (cond
   ((null gnus-summary-gather-subject-limit)
    (equal (gnus-simplify-subject-re s1)
	   (gnus-simplify-subject-re s2)))
   ((eq gnus-summary-gather-subject-limit 'fuzzy)
    (equal (gnus-simplify-subject-fuzzy s1)
	   (gnus-simplify-subject-fuzzy s2)))
   ((numberp gnus-summary-gather-subject-limit)
    (equal (gnus-limit-string s1 gnus-summary-gather-subject-limit)
	   (gnus-limit-string s2 gnus-summary-gather-subject-limit)))
   (t
    (equal s1 s2))))

;; Returns a list of writable groups.
(defun gnus-writable-groups ()
  (let ((alist gnus-newsrc-alist)
	groups)
    (while alist
      (or (gnus-group-read-only-p (car (car alist)))
	  (setq groups (cons (car (car alist)) groups)))
      (setq alist (cdr alist)))
    (nreverse groups)))

;; Two silly functions to ensure that all `y-or-n-p' questions clear
;; the echo area.
(defun gnus-y-or-n-p (prompt)
  (prog1
      (y-or-n-p prompt)
    (message "")))

(defun gnus-yes-or-no-p (prompt)
  (prog1
      (yes-or-no-p prompt)
    (message "")))

;; Check whether to use long file names.
(defun gnus-use-long-file-name (symbol)
  ;; The variable has to be set...
  (and gnus-use-long-file-name
       ;; If it isn't a list, then we return t.
       (or (not (listp gnus-use-long-file-name))
	   ;; If it is a list, and the list contains `symbol', we
	   ;; return nil.  
	   (not (memq symbol gnus-use-long-file-name)))))

;; I suspect there's a better way, but I haven't taken the time to do
;; it yet. -erik selberg@cs.washington.edu
(defun gnus-dd-mmm (messy-date)
  "Return a string like DD-MMM from a big messy string"
  (let ((datevec (timezone-parse-date messy-date)))
    (format "%2s-%s"
	    (or (aref datevec 2) "??")
	    (capitalize
	     (or (car 
		  (nth (1- (string-to-number (aref datevec 1)))
		       timezone-months-assoc))
		 "???")))))

;; Make a hash table (default and minimum size is 255).
;; Optional argument HASHSIZE specifies the table size.
(defun gnus-make-hashtable (&optional hashsize)
  (make-vector (if hashsize (max (gnus-create-hash-size hashsize) 255) 255) 0))

;; Make a number that is suitable for hashing; bigger than MIN and one
;; less than 2^x.
(defun gnus-create-hash-size (min)
  (let ((i 1))
    (while (< i min)
      (setq i (* 2 i)))
    (1- i)))

;; Show message if message has a lower level than `gnus-verbose'. 
;; Guide-line for numbers:
;; 1 - error messages, 3 - non-serious error messages, 5 - messages
;; for things that take a long time, 7 - not very important messages
;; on stuff, 9 - messages inside loops.
(defun gnus-message (level &rest args)
  (if (<= level gnus-verbose)
      (apply 'message args)
    ;; We have to do this format thingie here even if the result isn't
    ;; shown - the return value has to be the same as the return value
    ;; from `message'.
    (apply 'format args)))

;; Generate a unique new group name.
(defun gnus-generate-new-group-name (leaf)
  (let ((name leaf)
	(num 0))
    (while (gnus-gethash name gnus-newsrc-hashtb)
      (setq name (concat leaf "<" (int-to-string (setq num (1+ num))) ">")))
    name))

;; Find out whether the gnus-visual TYPE is wanted.
(defun gnus-visual-p (&optional type class)
  (and gnus-visual			; Has to be non-nil, at least.
       (if (not type)			; We don't care about type.
	   gnus-visual
	 (if (listp gnus-visual)	; It's a list, so we check it.
	     (or (memq type gnus-visual)
		 (memq class gnus-visual))
	   t))))

(defun gnus-parent-id (references)
  "Return the last Message-ID in REFERENCES."
  (and references
       (string-match "\\(<[^<>]+>\\) *$" references)
       (substring references (match-beginning 1) (match-end 1))))

(defun gnus-ephemeral-group-p (group)
  "Say whether GROUP is ephemeral or not."
  (assoc 'quit-config (gnus-find-method-for-group group)))

(defun gnus-group-quit-config (group)
  "Return the quit-config of GROUP."
  (cdr (assoc 'quit-config (gnus-find-method-for-group group))))

;;; List and range functions

(defun gnus-last-element (list)
  "Return last element of LIST."
  (while (cdr list)
    (setq list (cdr list)))
  (car list))

(defun gnus-copy-sequence (list)
  "Do a complete, total copy of a list."
  (if (and (consp list) (not (consp (cdr list))))
      (cons (car list) (cdr list))
    (mapcar (lambda (elem) (if (consp elem) 
			       (if (consp (cdr elem))
				   (gnus-copy-sequence elem)
				 (cons (car elem) (cdr elem)))
			     elem))
	    list)))

(defun gnus-set-difference (list1 list2)
  "Return a list of elements of LIST1 that do not appear in LIST2."
  (let ((list1 (copy-sequence list1)))
    (while list2
      (setq list1 (delq (car list2) list1))
      (setq list2 (cdr list2)))
    list1))

(defun gnus-sorted-complement (list1 list2)
  "Return a list of elements of LIST1 that do not appear in LIST2.
Both lists have to be sorted over <."
  (let (out)
    (if (or (null list1) (null list2))
	(or list1 list2)
      (while (and list1 list2)
	(cond ((= (car list1) (car list2))
	       (setq list1 (cdr list1)
		     list2 (cdr list2)))
	      ((< (car list1) (car list2))
	       (setq out (cons (car list1) out))
	       (setq list1 (cdr list1)))
	      (t
	       (setq out (cons (car list2) out))
	       (setq list2 (cdr list2)))))
      (nconc (nreverse out) (or list1 list2)))))

(defun gnus-intersection (list1 list2)      
  (let ((result nil))
    (while list2
      (if (memq (car list2) list1)
	  (setq result (cons (car list2) result)))
      (setq list2 (cdr list2)))
    result))

(defun gnus-sorted-intersection (list1 list2)
  ;; LIST1 and LIST2 have to be sorted over <.
  (let (out)
    (while (and list1 list2)
      (cond ((= (car list1) (car list2))
	     (setq out (cons (car list1) out)
		   list1 (cdr list1)
		   list2 (cdr list2)))
	    ((< (car list1) (car list2))
	     (setq list1 (cdr list1)))
	    (t
	     (setq list2 (cdr list2)))))
    (nreverse out)))

(defun gnus-set-sorted-intersection (list1 list2)
  ;; LIST1 and LIST2 have to be sorted over <.
  ;; This function modifies LIST1.
  (let* ((top (cons nil list1))
	 (prev top))
    (while (and list1 list2)
      (cond ((= (car list1) (car list2))
	     (setq prev list1
		   list1 (cdr list1)
		   list2 (cdr list2)))
	    ((< (car list1) (car list2))
	     (setcdr prev (cdr list1))
	     (setq list1 (cdr list1)))
	    (t
	     (setq list2 (cdr list2)))))
    (setcdr prev nil)
    (cdr top)))

(defun gnus-compress-sequence (numbers &optional always-list)
  "Convert list of numbers to a list of ranges or a single range.
If ALWAYS-LIST is non-nil, this function will always release a list of
ranges."
  (let* ((first (car numbers))
	 (last (car numbers))
	 result)
    (if (null numbers)
	nil
      (if (not (listp (cdr numbers)))
	  numbers
	(while numbers
	  (cond ((= last (car numbers)) nil) ;Omit duplicated number
		((= (1+ last) (car numbers)) ;Still in sequence
		 (setq last (car numbers)))
		(t			;End of one sequence
		 (setq result 
		       (cons (if (= first last) first
			       (cons first last)) result))
		 (setq first (car numbers))
		 (setq last  (car numbers))))
	  (setq numbers (cdr numbers)))
	(if (and (not always-list) (null result))
	    (if (= first last) (list first) (cons first last))
	  (nreverse (cons (if (= first last) first (cons first last))
			  result)))))))

(defalias 'gnus-uncompress-sequence 'gnus-uncompress-range)
(defun gnus-uncompress-range (ranges)
  "Expand a list of ranges into a list of numbers.
RANGES is either a single range on the form `(num . num)' or a list of
these ranges."
  (let (first last result)
    (cond 
     ((null ranges)
      nil)
     ((not (listp (cdr ranges)))
      (setq first (car ranges))
      (setq last (cdr ranges))
      (while (<= first last)
	(setq result (cons first result))
	(setq first (1+ first)))
      (nreverse result))
     (t
      (while ranges
	(if (atom (car ranges))
	    (if (numberp (car ranges))
		(setq result (cons (car ranges) result)))
	  (setq first (car (car ranges)))
	  (setq last  (cdr (car ranges)))
	  (while (<= first last)
	    (setq result (cons first result))
	    (setq first (1+ first))))
	(setq ranges (cdr ranges)))
      (nreverse result)))))

(defun gnus-add-to-range (ranges list)
  "Return a list of ranges that has all articles from both RANGES and LIST.
Note: LIST has to be sorted over `<'."
  (if (not ranges)
      (gnus-compress-sequence list t)
    (setq list (copy-sequence list))
    (or (listp (cdr ranges))
	(setq ranges (list ranges)))
    (let ((out ranges)
	  ilist lowest highest temp)
      (while (and ranges list)
	(setq ilist list)
	(setq lowest (or (and (atom (car ranges)) (car ranges))
			 (car (car ranges))))
	(while (and list (cdr list) (< (car (cdr list)) lowest))
	  (setq list (cdr list)))
	(if (< (car ilist) lowest)
	    (progn
	      (setq temp list)
	      (setq list (cdr list))
	      (setcdr temp nil)
	      (setq out (nconc (gnus-compress-sequence ilist t) out))))
	(setq highest (or (and (atom (car ranges)) (car ranges))
			  (cdr (car ranges))))
	(while (and list (<= (car list) highest))
	  (setq list (cdr list)))
	(setq ranges (cdr ranges)))
      (if list
	  (setq out (nconc (gnus-compress-sequence list t) out)))
      (setq out (sort out (lambda (r1 r2) 
			    (< (or (and (atom r1) r1) (car r1))
			       (or (and (atom r2) r2) (car r2))))))
      (setq ranges out)
      (while ranges
	(if (atom (car ranges))
	    (if (cdr ranges)
		(if (atom (car (cdr ranges)))
		    (if (= (1+ (car ranges)) (car (cdr ranges)))
			(progn
			  (setcar ranges (cons (car ranges) 
					       (car (cdr ranges))))
			  (setcdr ranges (cdr (cdr ranges)))))
		  (if (= (1+ (car ranges)) (car (car (cdr ranges))))
		      (progn
			(setcar (car (cdr ranges)) (car ranges))
			(setcar ranges (car (cdr ranges)))
			(setcdr ranges (cdr (cdr ranges)))))))
	  (if (cdr ranges)
	      (if (atom (car (cdr ranges)))
		  (if (= (1+ (cdr (car ranges))) (car (cdr ranges)))
		      (progn
			(setcdr (car ranges) (car (cdr ranges)))
			(setcdr ranges (cdr (cdr ranges)))))
		(if (= (1+ (cdr (car ranges))) (car (car (cdr ranges))))
		    (progn
		      (setcdr (car ranges) (cdr (car (cdr ranges))))
		      (setcdr ranges (cdr (cdr ranges))))))))
	(setq ranges (cdr ranges)))
      out)))

(defun gnus-remove-from-range (ranges list)
  "Return a list of ranges that has all articles from LIST removed from RANGES.
Note: LIST has to be sorted over `<'."
  ;; !!! This function shouldn't look like this, but I've got a headache.
  (gnus-compress-sequence 
   (gnus-sorted-complement
    (gnus-uncompress-range ranges) list)))

(defun gnus-member-of-range (number ranges)
  (if (not (listp (cdr ranges)))
      (and (>= number (car ranges)) 
	   (<= number (cdr ranges)))
    (let ((not-stop t))
      (while (and ranges 
		  (if (numberp (car ranges))
		      (>= number (car ranges))
		    (>= number (car (car ranges))))
		  not-stop)
	(if (if (numberp (car ranges))
		(= number (car ranges))
	      (and (>= number (car (car ranges)))
		   (<= number (cdr (car ranges)))))
	    (setq not-stop nil))
	(setq ranges (cdr ranges)))
      (not not-stop))))

(defun gnus-sublist-p (list sublist)
  "Test whether all elements in SUBLIST are members of LIST."
  (let ((sublistp t))
    (while sublist
      (unless (memq (pop sublist) list)
	(setq sublistp nil
	      sublist nil)))
    sublistp))


;;;
;;; Gnus group mode
;;;

(defvar gnus-group-mode-map nil)
(defvar gnus-group-group-map nil)
(defvar gnus-group-mark-map nil)
(defvar gnus-group-list-map nil)
(defvar gnus-group-sort-map nil)
(defvar gnus-group-soup-map nil)
(defvar gnus-group-sub-map nil)
(defvar gnus-group-sub-map nil)
(defvar gnus-group-help-map nil)
(defvar gnus-group-score-map nil)
(put 'gnus-group-mode 'mode-class 'special)

(if gnus-group-mode-map
    nil
  (setq gnus-group-mode-map (make-keymap))
  (suppress-keymap gnus-group-mode-map)
  (define-key gnus-group-mode-map " " 'gnus-group-read-group)
  (define-key gnus-group-mode-map "=" 'gnus-group-select-group)
  (define-key gnus-group-mode-map "\r" 'gnus-group-select-group)
  (define-key gnus-group-mode-map "\M-\r" 'gnus-group-quick-select-group)
  (define-key gnus-group-mode-map "j" 'gnus-group-jump-to-group)
  (define-key gnus-group-mode-map "n" 'gnus-group-next-unread-group)
  (define-key gnus-group-mode-map "p" 'gnus-group-prev-unread-group)
  (define-key gnus-group-mode-map "\177" 'gnus-group-prev-unread-group)
  (define-key gnus-group-mode-map "N" 'gnus-group-next-group)
  (define-key gnus-group-mode-map "P" 'gnus-group-prev-group)
  (define-key gnus-group-mode-map
    "\M-n" 'gnus-group-next-unread-group-same-level)
  (define-key gnus-group-mode-map 
    "\M-p" 'gnus-group-prev-unread-group-same-level)
  (define-key gnus-group-mode-map "," 'gnus-group-best-unread-group)
  (define-key gnus-group-mode-map "." 'gnus-group-first-unread-group)
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
  (define-key gnus-group-mode-map "\C-c\M-\C-a" 'gnus-group-description-apropos)
  (define-key gnus-group-mode-map "a" 'gnus-group-post-news)
  (define-key gnus-group-mode-map "\ek" 'gnus-group-edit-local-kill)
  (define-key gnus-group-mode-map "\eK" 'gnus-group-edit-global-kill)
  (define-key gnus-group-mode-map "\C-k" 'gnus-group-kill-group)
  (define-key gnus-group-mode-map "\C-y" 'gnus-group-yank-group)
  (define-key gnus-group-mode-map "\C-w" 'gnus-group-kill-region)
  (define-key gnus-group-mode-map "\C-x\C-t" 'gnus-group-transpose-groups)
  (define-key gnus-group-mode-map "\C-c\C-l" 'gnus-group-list-killed)
  (define-key gnus-group-mode-map "\C-c\C-x" 'gnus-group-expire-articles)
  (define-key gnus-group-mode-map "\C-c\M-\C-x" 'gnus-group-expire-all-groups)
  (define-key gnus-group-mode-map "V" 'gnus-version)
  (define-key gnus-group-mode-map "s" 'gnus-group-save-newsrc)
  (define-key gnus-group-mode-map "z" 'gnus-group-suspend)
  (define-key gnus-group-mode-map "Z" 'gnus-group-clear-dribble)
  (define-key gnus-group-mode-map "q" 'gnus-group-exit)
  (define-key gnus-group-mode-map "Q" 'gnus-group-quit)
  (define-key gnus-group-mode-map "?" 'gnus-group-describe-briefly)
  (define-key gnus-group-mode-map "\C-c\C-i" 'gnus-info-find-node)
  (define-key gnus-group-mode-map "\M-e" 'gnus-group-edit-group-method)
  (define-key gnus-group-mode-map "^" 'gnus-group-enter-server-mode)
  (define-key gnus-group-mode-map gnus-mouse-2 'gnus-mouse-pick-group)
  (define-key gnus-group-mode-map "<" 'beginning-of-buffer)
  (define-key gnus-group-mode-map ">" 'end-of-buffer)
  (define-key gnus-group-mode-map "\C-c\C-b" 'gnus-bug)
  (define-key gnus-group-mode-map "\C-c\C-s" 'gnus-group-sort-groups)

  (define-key gnus-group-mode-map "#" 'gnus-group-mark-group)
  (define-key gnus-group-mode-map "\M-#" 'gnus-group-unmark-group)
  (define-prefix-command 'gnus-group-mark-map)
  (define-key gnus-group-mode-map "M" 'gnus-group-mark-map)
  (define-key gnus-group-mark-map "m" 'gnus-group-mark-group)
  (define-key gnus-group-mark-map "u" 'gnus-group-unmark-group)
  (define-key gnus-group-mark-map "w" 'gnus-group-mark-region)

  (define-prefix-command 'gnus-group-group-map)
  (define-key gnus-group-mode-map "G" 'gnus-group-group-map)
  (define-key gnus-group-group-map "d" 'gnus-group-make-directory-group)
  (define-key gnus-group-group-map "h" 'gnus-group-make-help-group)
  (define-key gnus-group-group-map "a" 'gnus-group-make-archive-group)
  (define-key gnus-group-group-map "k" 'gnus-group-make-kiboze-group)
  (define-key gnus-group-group-map "m" 'gnus-group-make-group)
  (define-key gnus-group-group-map "E" 'gnus-group-edit-group)
  (define-key gnus-group-group-map "e" 'gnus-group-edit-group-method)
  (define-key gnus-group-group-map "p" 'gnus-group-edit-group-parameters)
  (define-key gnus-group-group-map "v" 'gnus-group-add-to-virtual)
  (define-key gnus-group-group-map "V" 'gnus-group-make-empty-virtual)
  (define-key gnus-group-group-map "D" 'gnus-group-enter-directory)
  (define-key gnus-group-group-map "f" 'gnus-group-make-doc-group)
  (define-key gnus-group-group-map "r" 'gnus-group-rename-group)
  (define-key gnus-group-group-map "\177" 'gnus-group-delete-group)
  (define-key gnus-group-group-map "t" 'gnus-group-add-to-topic)

  (define-prefix-command 'gnus-group-soup-map)
  (define-key gnus-group-group-map "s" 'gnus-group-soup-map)
  (define-key gnus-group-soup-map "b" 'gnus-group-brew-soup)
  (define-key gnus-group-soup-map "w" 'gnus-soup-save-areas)
  (define-key gnus-group-soup-map "s" 'gnus-soup-send-replies)
  (define-key gnus-group-soup-map "p" 'gnus-soup-pack-packet)
  (define-key gnus-group-soup-map "r" 'nnsoup-pack-replies)

  (define-prefix-command 'gnus-group-sort-map)
  (define-key gnus-group-group-map "S" 'gnus-group-sort-map)
  (define-key gnus-group-sort-map "s" 'gnus-group-sort-groups)
  (define-key gnus-group-sort-map "a" 'gnus-group-sort-groups-by-alphabet)
  (define-key gnus-group-sort-map "u" 'gnus-group-sort-groups-by-unread)
  (define-key gnus-group-sort-map "l" 'gnus-group-sort-groups-by-level)
  (define-key gnus-group-sort-map "v" 'gnus-group-sort-groups-by-score)
  (define-key gnus-group-sort-map "r" 'gnus-group-sort-groups-by-rank)
  (define-key gnus-group-sort-map "m" 'gnus-group-sort-groups-by-method)

  (define-prefix-command 'gnus-group-help-map)
  (define-key gnus-group-mode-map "H" 'gnus-group-help-map)
  (define-key gnus-group-help-map "f" 'gnus-group-fetch-faq)

  (define-prefix-command 'gnus-group-list-map)
  (define-key gnus-group-mode-map "A" 'gnus-group-list-map)
  (define-key gnus-group-list-map "k" 'gnus-group-list-killed)
  (define-key gnus-group-list-map "z" 'gnus-group-list-zombies)
  (define-key gnus-group-list-map "s" 'gnus-group-list-groups)
  (define-key gnus-group-list-map "u" 'gnus-group-list-all-groups)
  (define-key gnus-group-list-map "A" 'gnus-group-list-active)
  (define-key gnus-group-list-map "a" 'gnus-group-apropos)
  (define-key gnus-group-list-map "d" 'gnus-group-description-apropos)
  (define-key gnus-group-list-map "m" 'gnus-group-list-matching)
  (define-key gnus-group-list-map "M" 'gnus-group-list-all-matching)
  (define-key gnus-group-list-map "t" 'gnus-topic-toggle-topic)

  (define-prefix-command 'gnus-group-score-map)
  (define-key gnus-group-mode-map "W" 'gnus-group-score-map)
  (define-key gnus-group-score-map "f" 'gnus-score-flush-cache)

  (define-prefix-command 'gnus-group-sub-map)
  (define-key gnus-group-mode-map "S" 'gnus-group-sub-map)
  (define-key gnus-group-sub-map "l" 'gnus-group-set-current-level)
  (define-key gnus-group-sub-map "t" 'gnus-group-unsubscribe-current-group)
  (define-key gnus-group-sub-map "s" 'gnus-group-unsubscribe-group)
  (define-key gnus-group-sub-map "k" 'gnus-group-kill-group)
  (define-key gnus-group-sub-map "y" 'gnus-group-yank-group)
  (define-key gnus-group-sub-map "w" 'gnus-group-kill-region)
  (define-key gnus-group-sub-map "z" 'gnus-group-kill-all-zombies))

(defun gnus-group-mode ()
  "Major mode for reading news.

All normal editing commands are switched off.
\\<gnus-group-mode-map>
The group buffer lists (some of) the groups available.  For instance,
`\\[gnus-group-list-groups]' will list all subscribed groups with unread articles, while `\\[gnus-group-list-zombies]'
lists all zombie groups. 

Groups that are displayed can be entered with `\\[gnus-group-read-group]'.  To subscribe 
to a group not displayed, type `\\[gnus-group-unsubscribe-group]'. 

For more in-depth information on this mode, read the manual (`\\[gnus-info-find-node]'). 

The following commands are available:

\\{gnus-group-mode-map}"
  (interactive)
  (if (gnus-visual-p 'group-menu 'menu) (gnus-group-make-menu-bar))
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

;; Look at LEVEL and find out what the level is really supposed to be.
;; If LEVEL is non-nil, LEVEL will be returned, if not, what happens
;; will depend on whether `gnus-group-use-permanent-levels' is used.
(defun gnus-group-default-level (&optional level number-or-nil)
  (cond  
   (gnus-group-use-permanent-levels
    (setq gnus-group-default-list-level 
	  (or level gnus-group-default-list-level))
    (or gnus-group-default-list-level gnus-level-subscribed))
   (number-or-nil
    level)
   (t
    (or level gnus-group-default-list-level gnus-level-subscribed))))
  

(defvar gnus-tmp-prev-perm nil)

;;;###autoload
(defun gnus-no-server (&optional arg)
  "Read network news.
If ARG is a positive number, Gnus will use that as the
startup level.  If ARG is nil, Gnus will be started at level 2. 
If ARG is non-nil and not a positive number, Gnus will
prompt the user for the name of an NNTP server to use.
As opposed to `gnus', this command will not connect to the local server."
  (interactive "P")
  (let ((perm
	 (cons gnus-group-use-permanent-levels gnus-group-default-list-level)))
    (setq gnus-tmp-prev-perm nil)
    (setq gnus-group-use-permanent-levels t)
    (gnus (or arg (1- gnus-level-default-subscribed)) t)
    (setq gnus-tmp-prev-perm perm)))

;;;###autoload
(defun gnus-slave (&optional arg)
  "Read news as a slave."
  (interactive "P")
  (gnus arg nil 'slave))

;;;###autoload
(defun gnus (&optional arg dont-connect slave)
  "Read network news.
If ARG is non-nil and a positive number, Gnus will use that as the
startup level.  If ARG is non-nil and not a positive number, Gnus will
prompt the user for the name of an NNTP server to use."
  (interactive "P")
  (if (get-buffer gnus-group-buffer)
      (progn
	(switch-to-buffer gnus-group-buffer)
	(gnus-group-get-new-news))

    (gnus-clear-system)

    (nnheader-init-server-buffer)
    ;; We do this if `gnus-no-server' has been run.
    (if gnus-tmp-prev-perm 
	(setq gnus-group-use-permanent-levels (car gnus-tmp-prev-perm)
	      gnus-group-default-list-level (cdr gnus-tmp-prev-perm)
	      gnus-tmp-prev-perm nil))
    (gnus-read-init-file)

    (setq gnus-slave slave)

    (gnus-group-setup-buffer)
    (let ((buffer-read-only nil))
      (erase-buffer)
      (if (not gnus-inhibit-startup-message)
	  (progn
	    (gnus-group-startup-message)
	    (sit-for 0))))
    
    (let ((level (and arg (numberp arg) (> arg 0) arg))
	  did-connect)
      (unwind-protect
	  (progn
	    (or dont-connect 
		(setq did-connect
		      (gnus-start-news-server (and arg (not level))))))
	(if (and (not dont-connect) 
		 (not did-connect))
	    (gnus-group-quit)
	  (run-hooks 'gnus-startup-hook)
	  ;; NNTP server is successfully open. 

	  ;; Find the current startup file name.
	  (setq gnus-current-startup-file 
		(gnus-make-newsrc-file gnus-startup-file))

	  ;; Read the dribble file.
	  (and (or gnus-slave gnus-use-dribble-file) (gnus-dribble-read-file))

	  (gnus-summary-make-display-table)
	  (gnus-setup-news nil level)
	  (gnus-group-list-groups level)
	  (gnus-configure-windows 'group)
	  (gnus-group-set-mode-line))))))

(defun gnus-unload ()
  "Unload all Gnus features."
  (interactive)
  (or (boundp 'load-history)
      (error "Sorry, `gnus-unload' is not implemented in this Emacs version."))
  (let ((history load-history)
	feature)
    (while history
      (and (string-match "^gnus" (car (car history)))
	   (setq feature (cdr (assq 'provide (car history))))
	   (unload-feature feature 'force))
      (setq history (cdr history)))))

(defun gnus-compile ()
  "Byte-compile the Gnus startup file.
This will also compile the user-defined format specs."
  (interactive)
  (let ((file (concat (make-temp-name "/tmp/gnuss") ".el")))
    (save-excursion
      (gnus-message 7 "Compiling user file...")
      (nnheader-set-temp-buffer " *compile gnus*")
      (and (file-exists-p gnus-init-file)
	   (insert-file gnus-init-file))
      (goto-char (point-max))

      (let ((formats '(summary summary-dummy group 
			       summary-mode group-mode article-mode))
	    format fs)
	
	(while formats
	  (setq format (symbol-name (car formats))
		formats (cdr formats)
		fs (cons (symbol-value 
			  (intern (format "gnus-%s-line-format" format)))
			 fs))
	  (insert "(defun gnus-" format "-line-format-spec ()\n")
	  (insert 
	   (prin1-to-string
	    (symbol-value 
	     (intern (format "gnus-%s-line-format-spec" format)))))
	  (insert ")\n")
	  (insert "(setq gnus-" format 
		  "-line-format-spec (list 'gnus-byte-code 'gnus-"
		  format "-line-format-spec))\n"))

	(insert "(setq gnus-old-specs '" (prin1-to-string fs) ")\n")

	(write-region (point-min) (point-max) file nil 'silent)
	(byte-compile-file file)
	(rename-file
	 (concat file "c") 
	 (concat gnus-init-file 
		 (if (string-match "\\.el$" gnus-init-file) "c" ".elc"))
	 t)
	(when (file-exists-p file)
	  (delete-file file))
	(kill-buffer (current-buffer)))
      (gnus-message 7 "Compiling user file...done"))))

(defun gnus-indent-rigidly (start end arg)
  "Indent rigidly using only spaces and no tabs."
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (indent-rigidly start end arg)
      (goto-char (point-min))
      (while (search-forward "\t" nil t)
	(replace-match "        " t t)))))

(defun gnus-group-startup-message (&optional x y)
  "Insert startup message in current buffer."
  ;; Insert the message.
  (erase-buffer)
  (insert
   (format "              %s
          _    ___ _             _      
          _ ___ __ ___  __    _ ___     
          __   _     ___    __  ___     
              _           ___     _     
             _  _ __             _      
             ___   __            _      
                   __           _       
                    _      _   _        
                   _      _    _        
                      _  _    _         
                  __  ___               
                 _   _ _     _          
                _   _                   
              _    _                    
             _    _                     
            _                         
          __                             

" 
	   ""))
  ;; And then hack it.
  (gnus-indent-rigidly (point-min) (point-max) 
		       (/ (max (- (window-width) (or x 46)) 0) 2))
  (goto-char (point-min))
  (forward-line 1)
  (let* ((pheight (count-lines (point-min) (point-max)))
	 (wheight (window-height))
	 (rest (- wheight pheight)))
    (insert (make-string (max 0 (* 2 (/ rest 3))) ?\n)))
  ;; Fontify some.
  (goto-char (point-min))
  (and (search-forward "Praxis" nil t)
       (put-text-property (match-beginning 0) (match-end 0) 'face 'bold))
  (goto-char (point-min))
  (let* ((mode-string (gnus-group-set-mode-line)))
    (setq mode-line-buffer-identification 
	  (concat gnus-version (substring mode-string 4)))
    (set-buffer-modified-p t)))

(defun gnus-group-startup-message-old (&optional x y)
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

       A Praxis Release
      larsi@ifi.uio.no
" 
	   gnus-version))
  ;; And then hack it.
  ;; 18 is the longest line.
  (indent-rigidly (point-min) (point-max) 
		  (/ (max (- (window-width) (or x 28)) 0) 2))
  (goto-char (point-min))
  ;; +4 is fuzzy factor.
  (insert-char ?\n (/ (max (- (window-height) (or y 12)) 0) 2))

  ;; Fontify some.
  (goto-char (point-min))
  (search-forward "Praxis")
  (put-text-property (match-beginning 0) (match-end 0) 'face 'bold)
  (goto-char (point-min)))

(defun gnus-group-setup-buffer ()
  (or (get-buffer gnus-group-buffer)
      (progn
	(switch-to-buffer gnus-group-buffer)
	(gnus-add-current-to-buffer-list)
	(gnus-group-mode)
	(and gnus-carpal (gnus-carpal-setup-buffer 'group)))))

(defun gnus-group-list-groups (&optional level unread)
  "List newsgroups with level LEVEL or lower that have unread articles.
Default is all subscribed groups.
If argument UNREAD is non-nil, groups with no unread articles are also
listed." 
  (interactive (list (if current-prefix-arg
			 (prefix-numeric-value current-prefix-arg)
		       (or
			(gnus-group-default-level nil t)
			gnus-group-default-list-level
			gnus-level-subscribed))))
  (or level
      (setq level (car gnus-group-list-mode)
	    unread (cdr gnus-group-list-mode)))
  (setq level (gnus-group-default-level level))
  (gnus-group-setup-buffer)		;May call from out of group buffer
  (gnus-update-format-specifications)
  (let ((case-fold-search nil)
	(group (gnus-group-group-name)))
    (funcall gnus-group-prepare-function level unread nil)
    (if (zerop (buffer-size))
	(gnus-message 5 gnus-no-groups-message)
      (goto-char (point-min))
      (if (not group)
	  ;; Go to the first group with unread articles.
	  (gnus-group-search-forward nil nil nil t)
	;; Find the right group to put point on.  If the current group
	;; has disapeared in the new listing, try to find the next
	;; one.  If no next one can be found, just leave point at the
	;; first newsgroup in the buffer.
	(if (not (gnus-goto-char
		  (text-property-any
		   (point-min) (point-max) 
		   'gnus-group (gnus-intern-safe group gnus-active-hashtb))))
	    (let ((newsrc (nthcdr 3 (gnus-gethash group gnus-newsrc-hashtb))))
	      (while (and newsrc
			  (not (gnus-goto-char 
				(text-property-any 
				 (point-min) (point-max) 'gnus-group 
				 (gnus-intern-safe 
				  (car (car newsrc)) gnus-active-hashtb)))))
		(setq newsrc (cdr newsrc)))
	      (or newsrc (progn (goto-char (point-max))
				(forward-line -1))))))
      ;; Adjust cursor point.
      (gnus-group-position-point))))

(defun gnus-group-prepare-flat (level &optional all lowest regexp) 
  "List all newsgroups with unread articles of level LEVEL or lower.
If ALL is non-nil, list groups that have no unread articles.
If LOWEST is non-nil, list all newsgroups of level LOWEST or higher.
If REGEXP, only list groups matching REGEXP."
  (set-buffer gnus-group-buffer)
  (let ((buffer-read-only nil)
	(newsrc (cdr gnus-newsrc-alist))
	(lowest (or lowest 1))
	info clevel unread group)
    (erase-buffer)
    (if (< lowest gnus-level-zombie)
	;; List living groups.
	(while newsrc
	  (setq info (car newsrc)
		group (gnus-info-group info)
		newsrc (cdr newsrc)
		unread (car (gnus-gethash group gnus-newsrc-hashtb)))
	  (and unread			; This group might be bogus
	       (or (not regexp)
		   (string-match regexp group))
	       (<= (setq clevel (gnus-info-level info)) level) 
	       (>= clevel lowest)
	       (or all			; We list all groups?
		   (eq unread t)	; We list unactivated groups
		   (> unread 0)		; We list groups with unread articles
		   (cdr (assq 'tick (gnus-info-marks info)))) 
					; And groups with tickeds
	       (gnus-group-insert-group-line 
		nil group (gnus-info-level info) 
		(gnus-info-marks info) unread (gnus-info-method info)))))
      
    ;; List dead groups.
    (and (>= level gnus-level-zombie) (<= lowest gnus-level-zombie)
	 (gnus-group-prepare-flat-list-dead 
	  (setq gnus-zombie-list (sort gnus-zombie-list 'string<)) 
	  gnus-level-zombie ?Z
	  regexp))
    (and (>= level gnus-level-killed) (<= lowest gnus-level-killed)
	 (gnus-group-prepare-flat-list-dead 
	  (setq gnus-killed-list (sort gnus-killed-list 'string<)) 
	  gnus-level-killed ?K regexp))

    (gnus-group-set-mode-line)
    (setq gnus-group-list-mode (cons level all))
    (run-hooks 'gnus-group-prepare-hook)))

(defun gnus-group-prepare-flat-list-dead (groups level mark regexp)
  ;; List zombies and killed lists somehwat faster, which was
  ;; suggested by Jack Vinson <vinson@unagi.cis.upenn.edu>.  It does
  ;; this by ignoring the group format specification altogether.
  (let (group beg)
    (if regexp
	;; This loop is used when listing groups that match some
	;; regexp. 
	(while groups
	  (setq group (pop groups))
	  (when (string-match regexp group)
	    (add-text-properties 
	     (point) (prog1 (1+ (point))
		       (insert " " mark "     *: " group "\n"))
	     (list 'gnus-group (gnus-intern-safe group gnus-active-hashtb)
		   'gnus-unread t
		   'gnus-level level))))
      ;; This loop is used when listing all groups.
      (while groups
	(add-text-properties 
	 (point) (prog1 (1+ (point))
		   (insert " " mark "     *: " 
			   (setq group (pop groups)) "\n"))
	 (list 'gnus-group (gnus-intern-safe group gnus-active-hashtb)
	       'gnus-unread t
	       'gnus-level level))))))

(defmacro gnus-group-real-name (group)
  "Find the real name of a foreign newsgroup."
  `(let ((gname ,group))
     (if (string-match ":[^:]+$" gname)
	 (substring gname (1+ (match-beginning 0)))
       gname)))

(defsubst gnus-server-add-address (method)
  (let ((method-name (symbol-name (car method))))
    (if (and (memq 'address (assoc method-name gnus-valid-select-methods))
	     (not (assq (intern (concat method-name "-address")) method)))
	(append method (list (list (intern (concat method-name "-address"))
				   (nth 1 method))))
      method)))

(defsubst gnus-server-get-method (group method)
  ;; Input either a server name, and extended server name, or a
  ;; select method, and return a select method. 
  (cond ((stringp method)
	 (gnus-server-to-method method))
	((and (stringp (car method)) group)
	 (gnus-server-extend-method group method))
	(t
	 (gnus-server-add-address method))))

(defun gnus-server-to-method (server)
  "Map virtual server names to select methods."
  (or (and (equal server "native") gnus-select-method)
      (cdr (assoc server gnus-server-alist))))

(defun gnus-group-prefixed-name (group method)
  "Return the whole name from GROUP and METHOD."
  (and (stringp method) (setq method (gnus-server-to-method method)))
  (concat (format "%s" (car method))
	  (if (and 
	       (assoc (format "%s" (car method)) (gnus-methods-using 'address))
	       (not (string= (nth 1 method) "")))
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

(defsubst gnus-secondary-method-p (method)
  "Return whether METHOD is a secondary select method."
  (let ((methods gnus-secondary-select-methods)
	(gmethod (gnus-server-get-method nil method)))
    (while (and methods
		(not (equal (gnus-server-get-method nil (car methods)) 
			    gmethod)))
      (setq methods (cdr methods)))
    methods))

(defun gnus-group-foreign-p (group)
  "Say whether a group is foreign or not."
  (and (not (gnus-group-native-p group))
       (not (gnus-group-secondary-p group))))

(defun gnus-group-native-p (group)
  "Say whether the group is native or not."
  (not (string-match ":" group)))

(defun gnus-group-secondary-p (group)
  "Say whether the group is secondary or not."
  (gnus-secondary-method-p (gnus-find-method-for-group group)))

(defun gnus-group-topic-p ()
  "Return non-nil if the current line is a topic."
  (get-text-property (gnus-point-at-bol) 'gnus-topic))

(defun gnus-group-get-parameter (group &optional symbol)
  "Returns the group parameters for GROUP.
If SYMBOL, return the value of that symbol in the group parameters."
  (let ((params (gnus-info-params (gnus-get-info group))))
    (if symbol
	(gnus-group-parameter-value params symbol)
      params)))

(defun gnus-group-parameter-value (params symbol)
  "Return the value of SYMBOL in group PARAMS."
  (or (car (memq symbol params))	; It's either a simple symbol
      (cdr (assq symbol params))))	; or a cons.

(defun gnus-group-add-parameter (group param)
  "Add parameter PARAM to GROUP."
  (let ((info (gnus-get-info group)))
    (if (not info)
	() ; This is a dead group.  We just ignore it.
      ;; Cons the new param to the old one and update.
      (gnus-group-set-info (cons param (gnus-info-params info)) 
			   group 'params))))

(defun gnus-group-add-score (group &optional score)
  "Add SCORE to the GROUP score.  
If SCORE is nil, add 1 to the score of GROUP."
  (let ((info (gnus-get-info group)))
    (gnus-info-set-score info (+ (gnus-info-score info) (or score 1)))))

(defun gnus-summary-bubble-group ()
  "Increase the score of the current group.
This is a handy function to add to `gnus-summary-exit-hook' to
increase the score of each group you read."
  (gnus-group-add-score gnus-newsgroup-name))

(defun gnus-group-set-info (info &optional method-only-group part)
  (let* ((entry (gnus-gethash
		 (or method-only-group (gnus-info-group info))
		 gnus-newsrc-hashtb))
	 (part-info info)
	 (info (if method-only-group (nth 2 entry) info)))
    (if (not method-only-group)
	()
      (or entry
	  (error "Trying to change non-existent group %s" method-only-group))
      ;; We have recevied parts of the actual group info - either the
      ;; select method or the group parameters.  We first check
      ;; whether we have to extend the info, and if so, do that.
      (let ((len (length info))
	    (total (if (eq part 'method) 5 6)))
	(and (< len total)
	     (setcdr (nthcdr (1- len) info)
		     (make-list (- total len) nil)))
	;; Then we enter the new info.
	(setcar (nthcdr (1- total) info) part-info)))
    ;; We uncompress some lists of marked articles.
    (let (marked)
      (if (not (setq marked (gnus-info-marks info)))
	  ()
	(while marked
	  (or (eq 'score (car (car marked)))
	      (eq 'bookmark (car (car marked)))
	      (eq 'killed (car (car marked)))
	      (setcdr (car marked) 
		      (gnus-uncompress-range (cdr (car marked)))))
	  (setq marked (cdr marked)))))
    (if entry
	()
      ;; This is a new group, so we just create it.
      (save-excursion
	(set-buffer gnus-group-buffer)
	(if (gnus-info-method info)
	    ;; It's a foreign group...
	    (gnus-group-make-group 
	     (gnus-group-real-name (gnus-info-group info))
	     (prin1-to-string (car (gnus-info-method info)))
	     (nth 1 (gnus-info-method info)))
	  ;; It's a native group.
	  (gnus-group-make-group (gnus-info-group info)))
	(gnus-message 6 "Note: New group created")
	(setq entry 
	      (gnus-gethash (gnus-group-prefixed-name 
			     (gnus-group-real-name (gnus-info-group info))
			     (or (gnus-info-method info) gnus-select-method))
			    gnus-newsrc-hashtb))))
    ;; Whether it was a new group or not, we now have the entry, so we
    ;; can do the update.
    (if entry
	(progn
	  (setcar (nthcdr 2 entry) info)
	  (if (and (not (eq (car entry) t)) 
		   (gnus-active (gnus-info-group info)))
	      (let ((marked (gnus-info-marks info)))
		(setcar entry (length (gnus-list-of-unread-articles 
				       (car info)))))))
      (error "No such group: %s" (gnus-info-group info)))))

(defun gnus-group-set-method-info (group select-method)
  (gnus-group-set-info select-method group 'method))

(defun gnus-group-set-params-info (group params)
  (gnus-group-set-info params group 'params))

(defun gnus-group-update-group-line ()
  "Update the current line in the group buffer."
  (let* ((buffer-read-only nil)
	 (group (gnus-group-group-name))
	 (entry (and group (gnus-gethash group gnus-newsrc-hashtb))))
    (and entry 
	 (not (gnus-ephemeral-group-p group))
	 (gnus-dribble-enter 
	  (concat "(gnus-group-set-info '" 
		  (prin1-to-string (nth 2 entry)) ")")))
    (gnus-delete-line)
    (gnus-group-insert-group-line-info group)
    (forward-line -1)
    (gnus-group-position-point)))

(defun gnus-group-insert-group-line-info (group)
  "Insert GROUP on the current line."
  (let ((entry (gnus-gethash group gnus-newsrc-hashtb)) 
	active info)
    (if entry
	(progn
	  ;; (Un)subscribed group.
	  (setq info (nth 2 entry))
	  (gnus-group-insert-group-line 
	   nil group (gnus-info-level info) (gnus-info-marks info)
	   (car entry) (gnus-info-method info)))
      ;; This group is dead.
      (gnus-group-insert-group-line 
       nil group 
       (if (member group gnus-zombie-list) gnus-level-zombie gnus-level-killed)
       nil 
       (if (setq active (gnus-active group))
	   (- (1+ (cdr active)) (car active)) 0) 
       nil))))

(defun gnus-group-insert-group-line 
  (gformat gnus-tmp-group gnus-tmp-level gnus-tmp-marked gnus-tmp-number
	   gnus-tmp-method)
  (let* ((gformat (or gformat gnus-group-line-format-spec))
	 (gnus-tmp-active (gnus-active gnus-tmp-group))
	 (gnus-tmp-number-total 
	  (if gnus-tmp-active 
	      (1+ (- (cdr gnus-tmp-active) (car gnus-tmp-active)))
	    0))
	 (gnus-tmp-number-of-dormant 
	  (length (cdr (assq 'dormant gnus-tmp-marked))))
	 (gnus-tmp-number-of-ticked
	  (length (cdr (assq 'tick gnus-tmp-marked))))
	 (gnus-tmp-number-of-ticked-and-dormant
	  (+ gnus-tmp-number-of-ticked gnus-tmp-number-of-dormant))
	 (gnus-tmp-number-of-unread-unticked 
	  (if (numberp gnus-tmp-number) (int-to-string (max 0 gnus-tmp-number))
	    "*"))
	 (gnus-tmp-number-of-read
	  (if (numberp gnus-tmp-number)
	      (max 0 (- gnus-tmp-number-total gnus-tmp-number))
	    "*"))
	 (gnus-tmp-subscribed
	  (cond ((<= gnus-tmp-level gnus-level-subscribed) ? )
		((<= gnus-tmp-level gnus-level-unsubscribed) ?U)
		((= gnus-tmp-level gnus-level-zombie) ?Z)
		(t ?K)))
	 (gnus-tmp-qualified-group (gnus-group-real-name gnus-tmp-group))
	 (gnus-tmp-newsgroup-description 
	  (if gnus-description-hashtb
	      (or (gnus-gethash gnus-tmp-group gnus-description-hashtb) "")
	    ""))
	 (gnus-tmp-moderated
	  (if (member gnus-tmp-group gnus-moderated-list) ?m ? ))
	 (gnus-tmp-moderated-string 
	  (if (eq gnus-tmp-moderated ?m) "(m)" ""))
	 (gnus-tmp-method
	  (gnus-server-get-method gnus-tmp-group gnus-tmp-method))
	 (gnus-tmp-news-server (or (car (cdr gnus-tmp-method)) ""))
	 (gnus-tmp-news-method (or (car gnus-tmp-method) ""))
	 (gnus-tmp-news-method-string 
	  (if gnus-tmp-method
	      (format "(%s:%s)" (car gnus-tmp-method)
		      (car (cdr gnus-tmp-method))) ""))
	 (gnus-tmp-marked 
	  (if (and (numberp gnus-tmp-number) 
		   (zerop gnus-tmp-number)
		   (> gnus-tmp-number-of-ticked 0))
	      ?* ? ))
	 (gnus-tmp-number
	  (if (eq gnus-tmp-number t) "*" 
	    (+ gnus-tmp-number gnus-tmp-number-of-dormant 
	       gnus-tmp-number-of-ticked)))
	 (gnus-tmp-process-marked
	  (if (member gnus-tmp-group gnus-group-marked)
	      gnus-process-mark ? ))
	 (buffer-read-only nil)
	 header				; passed as parameter to user-funcs.
	 b)
    (beginning-of-line)
    (setq b (point))
    ;; Insert the text.
    (eval gformat)

    (add-text-properties 
     b (1+ b) (list 'gnus-group (gnus-intern-safe
				 gnus-tmp-group gnus-active-hashtb)
		    'gnus-unread (if (numberp gnus-tmp-number)
				     (string-to-int 
				      gnus-tmp-number-of-unread-unticked)
				   t)
		    'gnus-marked gnus-tmp-marked
		    'gnus-level gnus-tmp-level))))

(defun gnus-group-update-group (group &optional visible-only)
  "Update all lines where GROUP appear.
If VISIBLE-ONLY is non-nil, the group won't be displayed if it isn't
already." 
  (save-excursion
    (set-buffer gnus-group-buffer)
    ;; The buffer may be narrowed.
    (save-restriction
      (widen)
      (let ((ident (gnus-intern-safe group gnus-active-hashtb))
	    (loc (point-min))
	    found buffer-read-only visible)
	;; Enter the current status into the dribble buffer.
	(let ((entry (gnus-gethash group gnus-newsrc-hashtb)))
	  (if (and entry (not (gnus-ephemeral-group-p group)))
	      (gnus-dribble-enter 
	       (concat "(gnus-group-set-info '" (prin1-to-string (nth 2 entry))
		       ")"))))
	;; Find all group instances.  If topics are in use, each group
	;; may be listed in more than once.
	(while (setq loc (text-property-any 
			  loc (point-max) 'gnus-group ident))
	  (setq found t)
	  (goto-char loc)
	  (gnus-delete-line)
	  (gnus-group-insert-group-line-info group)
	  (setq loc (1+ loc)))
	(if (or found visible-only)
	    ()
	  ;; No such line in the buffer, find out where it's supposed to
	  ;; go, and insert it there (or at the end of the buffer).
	  ;; Fix by Per Abrahamsen <amanda@iesd.auc.dk>.
	  (let ((entry (cdr (cdr (gnus-gethash group gnus-newsrc-hashtb)))))
	    (while (and entry (car entry)
			(not
			 (gnus-goto-char
			  (text-property-any
			   (point-min) (point-max) 
			   'gnus-group (gnus-intern-safe 
					(car (car entry)) 
					gnus-active-hashtb)))))
	      (setq entry (cdr entry)))
	    (or entry (goto-char (point-max))))
	  ;; Finally insert the line.
	  (gnus-group-insert-group-line-info group))
	(gnus-group-set-mode-line)))))

(defun gnus-group-set-mode-line ()
  (when (memq 'group gnus-updated-mode-lines)
    (let* ((gformat (or gnus-group-mode-line-format-spec
			(setq gnus-group-mode-line-format-spec
			      (gnus-parse-format 
			       gnus-group-mode-line-format 
			       gnus-group-mode-line-format-alist))))
	   (gnus-tmp-news-server (car (cdr gnus-select-method)))
	   (gnus-tmp-news-method (car gnus-select-method))
	   (max-len 60)
	   ;; Get the resulting string.
	   (mode-string (eval gformat)))
      ;; If the line is too long, we chop it off.
      (when (> (length mode-string) max-len) 
	(setq mode-string (substring mode-string 0 (- max-len 4))))
      (prog1
	  (setq mode-line-buffer-identification mode-string)
	(set-buffer-modified-p t)))))

(defun gnus-group-group-name ()
  "Get the name of the newsgroup on the current line."
  (let ((group (get-text-property (gnus-point-at-bol) 'gnus-group)))
    (and group (symbol-name group))))

(defun gnus-group-group-level ()
  "Get the level of the newsgroup on the current line."
  (get-text-property (gnus-point-at-bol) 'gnus-level))

(defun gnus-group-group-unread ()
  "Get the number of unread articles of the newsgroup on the current line."
  (get-text-property (gnus-point-at-bol) 'gnus-unread))

(defun gnus-group-search-forward (&optional backward all level first-too)
  "Find the next newsgroup with unread articles.
If BACKWARD is non-nil, find the previous newsgroup instead.
If ALL is non-nil, just find any newsgroup.
If LEVEL is non-nil, find group with level LEVEL, or higher if no such
group exists.
If FIRST-TOO, the current line is also eligible as a target."
  (let ((way (if backward -1 1))
	(low gnus-level-killed)
	(beg (point))
	pos found lev)
    (if (and backward (progn (beginning-of-line)) (bobp))
	nil
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
			      (setq lev (get-text-property (point)
							   'gnus-level))
			      (<= lev gnus-level-subscribed)))
			 (or (not level)
			     (and (setq lev (get-text-property (point)
							       'gnus-level))
				  (or (= lev level)
				      (and (< lev low)
					   (< level lev)
					   (progn
					     (setq low lev)
					     (setq pos (point))
					     nil))))))))
	      (zerop (forward-line way)))))
    (if found 
	(progn (gnus-group-position-point) t)
      (goto-char (or pos beg))
      (and pos t))))

;;; Gnus group mode commands

;; Group marking.

(defun gnus-group-mark-group (n &optional unmark no-advance)
  "Mark the current group."
  (interactive "p")
  (let ((buffer-read-only nil)
	group)
    (while 
	(and (> n 0) 
	     (setq group (gnus-group-group-name))
	     (progn
	       (beginning-of-line)
	       (forward-char 
		(or (cdr (assq 'process gnus-group-mark-positions)) 2))
	       (delete-char 1)
	       (if unmark
		   (progn
		     (insert " ")
		     (setq gnus-group-marked (delete group gnus-group-marked)))
		 (insert "#")
		 (setq gnus-group-marked
		       (cons group (delete group gnus-group-marked))))
	       t)
	     (or no-advance (zerop (gnus-group-next-group 1))))
      (setq n (1- n)))
    (gnus-summary-position-point)
    n))

(defun gnus-group-unmark-group (n)
  "Remove the mark from the current group."
  (interactive "p")
  (gnus-group-mark-group n 'unmark))

(defun gnus-group-mark-region (unmark beg end)
  "Mark all groups between point and mark.
If UNMARK, remove the mark instead."
  (interactive "P\nr")
  (let ((num (count-lines beg end)))
    (save-excursion
      (goto-char beg)
      (- num (gnus-group-mark-group num unmark)))))

(defun gnus-group-remove-mark (group)
  (and (gnus-group-goto-group group)
       (save-excursion
	 (gnus-group-mark-group 1 'unmark t))))

;; Return a list of groups to work on.  Take into consideration N (the
;; prefix) and the list of marked groups.
(defun gnus-group-process-prefix (n)
  (cond (n
	 (setq n (prefix-numeric-value n))
	 ;; There is a prefix, so we return a list of the N next
	 ;; groups. 
	 (let ((way (if (< n 0) -1 1))
	       (n (abs n))
	       group groups)
	   (save-excursion
	     (while (and (> n 0)
			 (setq group (gnus-group-group-name)))
	       (setq groups (cons group groups))
	       (setq n (1- n))
	       (forward-line way)))
	   (nreverse groups)))
	(gnus-group-marked
	 ;; No prefix, but a list of marked articles.
	 (reverse gnus-group-marked))
	(t
	 ;; Neither marked articles or a prefix, so we return the
	 ;; current group.
	 (let ((group (gnus-group-group-name)))
	   (and group (list group))))))

;; Selecting groups.

(defun gnus-group-read-group (&optional all no-article group)
  "Read news in this newsgroup.
If the prefix argument ALL is non-nil, already read articles become
readable.  IF ALL is a number, fetch this number of articles.  If the
optional argument NO-ARTICLE is non-nil, no article will be
auto-selected upon group entry.  If GROUP is non-nil, fetch that
group."
  (interactive "P")
  (if (gnus-group-topic-p)
      (gnus-topic-fold) ; This is a topic line.
    ;; And this is a real group.
    (let ((group (or group (gnus-group-group-name)))
	  number active marked entry)
      (or group (error "No group on current line"))
      (setq marked (nth 3 (nth 2 (setq entry (gnus-gethash
					      group gnus-newsrc-hashtb)))))
      ;; This group might be a dead group.  In that case we have to get
      ;; the number of unread articles from `gnus-active-hashtb'.
      (setq number
	    (cond ((numberp all) all)
		  (entry (car entry))
		  ((setq active (gnus-active group))
		   (- (1+ (cdr active)) (car active)))))
      (gnus-summary-read-group 
       group (or all (and (numberp number) 
			  (zerop (+ number (length (cdr (assq 'tick marked)))
				    (length (cdr (assq 'dormant marked)))))))
       no-article))))

(defun gnus-group-select-group (&optional all)
  "Select this newsgroup.
No article is selected automatically.
If ALL is non-nil, already read articles become readable.
If ALL is a number, fetch this number of articles."
  (interactive "P")
  (gnus-group-read-group all t))

(defun gnus-group-quick-select-group (&optional all)
  "Select the current group \"quickly\". 
This means that no highlighting or scoring will be performed."
  (interactive "P")
  (let (gnus-visual
	gnus-score-find-score-files-function
	gnus-apply-kill-hook
	gnus-summary-expunge-below)
    (gnus-group-read-group all t)))

;;;###autoload
(defun gnus-fetch-group (group)
  "Start Gnus if necessary and enter GROUP.
Returns whether the fetching was successful or not."
  (interactive "sGroup name: ")
  (or (get-buffer gnus-group-buffer)
      (gnus))
  (gnus-group-select-group))

;; Enter a group that is not in the group buffer.  Non-nil is returned
;; if selection was successful.
(defun gnus-group-read-ephemeral-group 
  (group method &optional activate quit-config)
  (let ((group (if (gnus-group-foreign-p group) group
		 (gnus-group-prefixed-name group method))))
    (gnus-sethash 
     group
     (list t nil (list group gnus-level-default-subscribed nil nil 
		       (append method
			       (list
				(list 'quit-config 
				      (if quit-config quit-config
					(cons (current-buffer) 'summary)))))))
     gnus-newsrc-hashtb)
    (set-buffer gnus-group-buffer)
    (or (gnus-check-server method)
	(error "Unable to contact server: %s" (gnus-status-message method)))
    (if activate (or (gnus-request-group group)
		     (error "Couldn't request group")))
    (condition-case ()
	(gnus-group-read-group t t group)
      (error nil)
      (quit nil))
    (not (equal major-mode 'gnus-group-mode))))
  
(defun gnus-group-jump-to-group (group)
  "Jump to newsgroup GROUP."
  (interactive 
   (list (completing-read 
	  "Group: " gnus-active-hashtb nil 
	  (memq gnus-select-method gnus-have-read-active-file))))

  (if (equal group "")
      (error "Empty group name"))

  (let ((b (text-property-any 
	    (point-min) (point-max) 
	    'gnus-group (gnus-intern-safe group gnus-active-hashtb))))
    (if b
	;; Either go to the line in the group buffer...
	(goto-char b)
      ;; ... or insert the line.
      (or
       (gnus-active group)
       (gnus-activate-group group)
       (error "%s error: %s" group (gnus-status-message group)))

      (gnus-group-update-group group)
      (goto-char (text-property-any 
		  (point-min) (point-max)
		  'gnus-group (gnus-intern-safe group gnus-active-hashtb)))))
  ;; Adjust cursor point.
  (gnus-group-position-point))

(defun gnus-group-goto-group (group)
  "Goto to newsgroup GROUP."
  (let ((b (text-property-any (point-min) (point-max) 
			      'gnus-group (gnus-intern-safe
					   group gnus-active-hashtb))))
    (and b (goto-char b))))

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
		(gnus-group-search-forward 
		 backward (or (not gnus-group-goto-unread) all) level))
      (setq n (1- n)))
    (if (/= 0 n) (gnus-message 7 "No more%s newsgroups%s" (if all "" " unread")
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
  (gnus-group-position-point))

(defun gnus-group-prev-unread-group-same-level (n)
  "Go to next N'th unread newsgroup on the same level.
Returns the difference between N and the number of skips actually
done."
  (interactive "p")
  (gnus-group-next-unread-group (- n) t (gnus-group-group-level))
  (gnus-group-position-point))

(defun gnus-group-best-unread-group (&optional exclude-group)
  "Go to the group with the highest level.
If EXCLUDE-GROUP, do not go to that group."
  (interactive)
  (goto-char (point-min))
  (let ((best 100000)
	unread best-point)
    (while (setq unread (get-text-property (point) 'gnus-unread))
      (if (and (numberp unread) (> unread 0))
	  (progn
	    (if (and (< (get-text-property (point) 'gnus-level) best)
		     (or (not exclude-group)
			 (not (equal exclude-group (gnus-group-group-name)))))
		(progn 
		  (setq best (get-text-property (point) 'gnus-level))
		  (setq best-point (point))))))
      (forward-line 1))
    (if best-point (goto-char best-point))
    (gnus-summary-position-point)
    (and best-point (gnus-group-group-name))))

(defun gnus-group-first-unread-group ()
  "Go to the first group with unread articles."
  (interactive)
  (prog1
      (let ((opoint (point))
	    unread)
	(goto-char (point-min))
	(if (or (eq (setq unread (gnus-group-group-unread)) t) ; Not active.
		(not (zerop unread))	; Has unread articles.
		(zerop (gnus-group-next-unread-group 1))) ; Next unread group.
	    (point)			; Success.
	  (goto-char opoint)
	  nil))				; Not success.
    (gnus-group-position-point)))

(defun gnus-group-enter-server-mode ()
  "Jump to the server buffer."
  (interactive)
  (gnus-server-enter-setup-buffer))

(defun gnus-group-make-group (name &optional method address)
  "Add a new newsgroup.
The user will be prompted for a NAME, for a select METHOD, and an
ADDRESS."
  (interactive
   (cons 
    (read-string "Group name: ")
    (let ((method
	   (completing-read 
	    "Method: " (append gnus-valid-select-methods gnus-server-alist)
	    nil t)))
      (if (assoc method gnus-valid-select-methods)
	  (list method
		(if (memq 'prompt-address
			  (assoc method gnus-valid-select-methods))
		    (read-string "Address: ")
		  ""))
	(list method nil)))))
  
  (save-excursion
    (set-buffer gnus-group-buffer)
    (let* ((meth (and method (if address (list (intern method) address) 
			       method)))
	   (nname (if method (gnus-group-prefixed-name name meth) name))
	   info)
      (and (gnus-gethash nname gnus-newsrc-hashtb)
	   (error "Group %s already exists" nname))
      (gnus-group-change-level 
       (setq info (list t nname gnus-level-default-subscribed nil nil meth))
       gnus-level-default-subscribed gnus-level-killed 
       (and (gnus-group-group-name)
	    (gnus-gethash (gnus-group-group-name)
			  gnus-newsrc-hashtb))
       t)
      (gnus-set-active nname (cons 1 0))
      (or (gnus-ephemeral-group-p name)
	  (gnus-dribble-enter 
	   (concat "(gnus-group-set-info '" (prin1-to-string (cdr info)) ")")))
      (gnus-group-insert-group-line-info nname)

      (if (assoc method gnus-valid-select-methods)
	  (require (intern method)))
      (and (gnus-check-backend-function 'request-create-group nname)
	   (gnus-request-create-group nname))
      t)))

(defun gnus-group-delete-group (group &optional force)
  "Delete the current group.
If FORCE (the prefix) is non-nil, all the articles in the group will
be deleted.  This is \"deleted\" as in \"removed forever from the face
of the Earth\".  There is no undo."
  (interactive 
   (list (gnus-group-group-name)
	 current-prefix-arg))
  (or group (error "No group to rename"))
  (or (gnus-check-backend-function 'request-delete-group group)
      (error "This backend does not support group deletion"))
  (prog1
      (if (not (gnus-yes-or-no-p
		(format
		 "Do you really want to delete %s%s? " 
		 group (if force " and all its contents" ""))))
	  () ; Whew!
	(gnus-message 6 "Deleting group %s..." group)
	(if (not (gnus-request-delete-group group force))
	    (progn
	      (gnus-message 3 "Couldn't delete group %s" group)
	      (ding))
	  (gnus-message 6 "Deleting group %s...done" group)
	  (gnus-group-goto-group group)
	  (gnus-group-kill-group 1 t)
	  t))
    (gnus-group-position-point)))

(defun gnus-group-rename-group (group new-name)
  (interactive
   (list
    (gnus-group-group-name)
    (progn
      (or (gnus-check-backend-function 
	   'request-rename-group (gnus-group-group-name))
	  (error "This backend does not support renaming groups"))
      (read-string "New group name: "))))

  (or (gnus-check-backend-function 'request-rename-group group)
      (error "This backend does not support renaming groups"))

  (or group (error "No group to rename"))
  (and (string-match "^[ \t]*$" new-name) 
       (error "Not a valid group name"))

  ;; We find the proper prefixed name.
  (setq new-name
	(gnus-group-prefixed-name 
	 (gnus-group-real-name new-name)
	 (gnus-info-method (gnus-get-info group))))

  (gnus-message 6 "Renaming group %s to %s..." group new-name)
  (prog1
      (if (not (gnus-request-rename-group group new-name))
	  (progn
	    (gnus-message 3 "Couldn't rename group %s to %s" group new-name)
	    (ding))
	;; We rename the group internally by killing it...
	(gnus-group-goto-group group)
	(gnus-group-kill-group)
	;; ... changing its name ...
	(setcar (cdr (car gnus-list-of-killed-groups))
		new-name)
	;; ... and then yanking it.  Magic!
	(gnus-group-yank-group) 
	(gnus-message 6 "Renaming group %s to %s...done" group new-name)
	new-name)
    (gnus-group-position-point)))


(defun gnus-group-edit-group (group &optional part)
  "Edit the group on the current line."
  (interactive (list (gnus-group-group-name)))
  (let ((done-func '(lambda () 
		      "Exit editing mode and update the information."
		      (interactive)
		      (gnus-group-edit-group-done 'part 'group)))
	(part (or part 'info))
	(winconf (current-window-configuration))
	info)
    (or group (error "No group on current line"))
    (or (setq info (gnus-get-info group))
	(error "Killed group; can't be edited"))
    (set-buffer (get-buffer-create gnus-group-edit-buffer))
    (gnus-configure-windows 'edit-group)
    (gnus-add-current-to-buffer-list)
    (emacs-lisp-mode)
    ;; Suggested by Hallvard B Furuseth <h.b.furuseth@usit.uio.no>.
    (use-local-map (copy-keymap emacs-lisp-mode-map))
    (local-set-key "\C-c\C-c" done-func)
    (make-local-variable 'gnus-prev-winconf)
    (setq gnus-prev-winconf winconf)
    ;; We modify the func to let it know what part it is editing.
    (setcar (cdr (nth 4 done-func)) (list 'quote part))
    (setcar (cdr (cdr (nth 4 done-func))) group)
    (erase-buffer)
    (insert
     (cond 
      ((eq part 'method)
       ";; Type `C-c C-c' after editing the select method.\n\n")
      ((eq part 'params)
       ";; Type `C-c C-c' after editing the group parameters.\n\n")
      ((eq part 'info)
       ";; Type `C-c C-c' after editing the group info.\n\n")))
    (let ((cinfo (gnus-copy-sequence info))
	  marked)
      (if (not (setq marked (gnus-info-marks cinfo)))
	  ()
	(while marked
	  (or (eq 'score (car (car marked)))
	      (eq 'bookmark (car (car marked)))
	      (eq 'killed (car (car marked)))
	      (not (numberp (car (cdr (car marked)))))
	      (setcdr (car marked) 
		      (gnus-compress-sequence (sort (cdr (car marked)) '<) t)))
	  (setq marked (cdr marked))))
      (insert 
       (pp-to-string
	(cond ((eq part 'method)
	       (or (gnus-info-method info) "native"))
	      ((eq part 'params)
	       (gnus-info-params info))
	      (t
	       cinfo)))
       "\n"))))

(defun gnus-group-edit-group-method (group)
  "Edit the select method of GROUP."
  (interactive (list (gnus-group-group-name)))
  (gnus-group-edit-group group 'method))

(defun gnus-group-edit-group-parameters (group)
  "Edit the group parameters of GROUP."
  (interactive (list (gnus-group-group-name)))
  (gnus-group-edit-group group 'params))

(defun gnus-group-edit-group-done (part group)
  "Get info from buffer, update variables and jump to the group buffer."
  (set-buffer (get-buffer-create gnus-group-edit-buffer))
  (goto-char (point-min))
  (let ((form (read (current-buffer)))
	(winconf gnus-prev-winconf))
    (if (eq part 'info) 
	(gnus-group-set-info form)
      (gnus-group-set-info form group part))
    (kill-buffer (current-buffer))
    (and winconf (set-window-configuration winconf))
    (set-buffer gnus-group-buffer)
    (gnus-group-update-group (gnus-group-group-name))
    (gnus-group-position-point)))

(defun gnus-group-make-help-group ()
  "Create the Gnus documentation group."
  (interactive)
  (let ((path (cons (concat installation-directory "etc/") load-path))
 	(name (gnus-group-prefixed-name "gnus-help" '(nndoc "gnus-help")))
 	file)
    (and (gnus-gethash name gnus-newsrc-hashtb)
  	 (error "Documentation group already exists"))
    (while (and path
 		(not (file-exists-p 
 		      (setq file (concat (file-name-as-directory (car path))
 					 "gnus-tut.txt")))))
      (setq path (cdr path)))
    (if (not path)
  	(message "Couldn't find doc group")
      (gnus-group-make-group 
       (gnus-group-real-name name)
       (list 'nndoc name
 	     (list 'nndoc-address file)
  	     (list 'nndoc-article-type 'mbox)))))
  (gnus-group-position-point))

(defun gnus-group-make-doc-group (file type)
  "Create a group that uses a single file as the source."
  (interactive 
   (list (read-file-name "File name: ") 
	 (let ((err "")
	       found char)
	   (while (not found)
	     (message "%sFile type (mbox, babyl, digest) [mbd]: " err)
	     (setq found (cond ((= (setq char (read-char)) ?m) 'mbox)
			       ((= char ?b) 'babyl)
			       ((= char ?d) 'digest)
			       (t (setq err (format "%c unknown. " char))
				  nil))))
	   found)))
  (let* ((file (expand-file-name file))
	 (name (gnus-generate-new-group-name
		(gnus-group-prefixed-name
		 (file-name-nondirectory file) '(nndoc "")))))
    (gnus-group-make-group 
     (gnus-group-real-name name)
     (list 'nndoc name
	   (list 'nndoc-address file)
	   (list 'nndoc-article-type type)))
    (forward-line -1)
    (gnus-group-position-point)))

(defun gnus-group-make-archive-group (&optional all)
  "Create the (ding) Gnus archive group of the most recent articles.
Given a prefix, create a full group."
  (interactive "P")
  (let ((group (gnus-group-prefixed-name 
		(if all "ding.archives" "ding.recent") '(nndir ""))))
    (and (gnus-gethash group gnus-newsrc-hashtb)
	 (error "Archive group already exists"))
    (gnus-group-make-group
     (gnus-group-real-name group)
     (list 'nndir (if all "hpc" "edu")
	   (list 'nndir-directory  
		 (if all gnus-group-archive-directory 
		   gnus-group-recent-archive-directory)))))
  (forward-line -1)
  (gnus-group-position-point))

(defun gnus-group-make-directory-group (dir)
  "Create an nndir group.
The user will be prompted for a directory.  The contents of this
directory will be used as a newsgroup.  The directory should contain
mail messages or news articles in files that have numeric names."
  (interactive
   (list (read-file-name "Create group from directory: ")))
  (or (file-exists-p dir) (error "No such directory"))
  (or (file-directory-p dir) (error "Not a directory"))
  (let ((ext "")
	(i 0)
	group)
    (while (or (not group) (gnus-gethash group gnus-newsrc-hashtb))
      (setq group
	    (gnus-group-prefixed-name 
	     (concat (file-name-as-directory (directory-file-name dir))
		     ext)
	     '(nndir "")))
      (setq ext (format "<%d>" (setq i (1+ i)))))
    (gnus-group-make-group 
     (gnus-group-real-name group)
     (list 'nndir group (list 'nndir-directory dir))))
  (forward-line -1)
  (gnus-group-position-point))

(defun gnus-group-make-kiboze-group (group address scores)
  "Create an nnkiboze group.
The user will be prompted for a name, a regexp to match groups, and
score file entries for articles to include in the group."
  (interactive
   (list
    (read-string "nnkiboze group name: ")
    (read-string "Source groups (regexp): ")
    (let ((headers (mapcar (lambda (group) (list group))
			   '("subject" "from" "number" "date" "message-id"
			     "references" "chars" "lines" "xref"
			     "followup" "all" "body" "head")))
	  scores header regexp regexps)
      (while (not (equal "" (setq header (completing-read 
					  "Match on header: " headers nil t))))
	(setq regexps nil)
	(while (not (equal "" (setq regexp (read-string 
					    (format "Match on %s (string): "
						    header)))))
	  (setq regexps (cons (list regexp nil nil 'r) regexps)))
	(setq scores (cons (cons header regexps) scores)))
      scores)))
  (gnus-group-make-group group "nnkiboze" address)
  (save-excursion
    (gnus-set-work-buffer)
    (let (emacs-lisp-mode-hook)
      (pp scores (current-buffer)))
    (write-region (point-min) (point-max) 
		  (gnus-score-file-name (concat "nnkiboze:" group))))
  (forward-line -1)
  (gnus-group-position-point))

(defun gnus-group-add-to-virtual (n vgroup)
  "Add the current group to a virtual group."
  (interactive
   (list current-prefix-arg
	 (completing-read "Add to virtual group: " gnus-newsrc-hashtb nil t
			  "nnvirtual:")))
  (or (eq (car (gnus-find-method-for-group vgroup)) 'nnvirtual)
      (error "%s is not an nnvirtual group" vgroup))
  (let* ((groups (gnus-group-process-prefix n))
	 (method (gnus-info-method (gnus-get-info vgroup))))
    (setcar (cdr method)
	    (concat 
	     (nth 1 method) "\\|"
	     (mapconcat 
	      (lambda (s) 
		(gnus-group-remove-mark s)
		(concat "\\(^" (regexp-quote s) "$\\)"))
	      groups "\\|"))))
  (gnus-group-position-point))

(defun gnus-group-make-empty-virtual (group)
  "Create a new, fresh, empty virtual group."
  (interactive "sCreate new, empty virtual group: ")
  (let* ((method (list 'nnvirtual "^$"))
	 (pgroup (gnus-group-prefixed-name group method)))
    ;; Check whether it exists already.
    (and (gnus-gethash pgroup gnus-newsrc-hashtb)
	 (error "Group %s already exists." pgroup))
    ;; Subscribe the new group after the group on the current line.
    (gnus-subscribe-group pgroup (gnus-group-group-name) method)
    (gnus-group-update-group pgroup)
    (forward-line -1)
    (gnus-group-position-point)))

(defun gnus-group-enter-directory (dir)
  "Enter an ephemeral nneething group."
  (interactive "DDirectory to read: ")
  (let* ((method (list 'nneething dir))
	 (leaf (gnus-group-prefixed-name
		(file-name-nondirectory (directory-file-name dir))
		method))
	 (name (gnus-generate-new-group-name leaf)))
    (let ((nneething-read-only t))
      (or (gnus-group-read-ephemeral-group 
	   name method t
	   (cons (current-buffer) (if (eq major-mode 'gnus-summary-mode)
				      'summary 'group)))
	  (error "Couldn't enter %s" dir)))))

;; Group sorting commands
;; Suggested by Joe Hildebrand <hildjj@idaho.fuentez.com>.

(defun gnus-group-sort-groups (&optional func)
  "Sort the group buffer according to `gnus-group-sort-function'."
  (interactive)
  (let ((funcs 
	 (cond (func (list func))
	       ((listp gnus-group-sort-function) gnus-group-sort-function)
	       (t list gnus-group-sort-function))))
    ;; We peel off the dummy group off the alist.
    (and (equal (car (car gnus-newsrc-alist)) "dummy.group")
	 (setq gnus-newsrc-alist (cdr gnus-newsrc-alist)))
    ;; Do the sorting.
    (while funcs
      (setq gnus-newsrc-alist 
	    (sort gnus-newsrc-alist (car funcs)))
      (setq funcs (cdr funcs))))
  (gnus-make-hashtable-from-newsrc-alist)
  (gnus-group-list-groups))

(defun gnus-group-sort-groups-by-alphabet ()
  "Sort the group buffer alphabetically by group name."
  (interactive)
  (gnus-group-sort-groups 'gnus-group-sort-by-alphabet))

(defun gnus-group-sort-groups-by-unread ()
  "Sort the group buffer by unread articles."
  (interactive)
  (gnus-group-sort-groups 'gnus-group-sort-by-unread))

(defun gnus-group-sort-groups-by-level ()
  "Sort the group buffer by group level."
  (interactive)
  (gnus-group-sort-groups 'gnus-group-sort-by-level))

(defun gnus-group-sort-groups-by-score ()
  "Sort the group buffer by group score."
  (interactive)
  (gnus-group-sort-groups 'gnus-group-sort-by-score))

(defun gnus-group-sort-groups-by-rank ()
  "Sort the group buffer by group rank."
  (interactive)
  (gnus-group-sort-groups 'gnus-group-sort-by-rank))

(defun gnus-group-sort-groups-by-method ()
  "Sort the group buffer alphabetically by backend name."
  (interactive)
  (gnus-group-sort-groups 'gnus-group-sort-by-method))

(defun gnus-group-sort-by-alphabet (info1 info2)
  "Sort alphabetically."
  (string< (gnus-info-group info1) (gnus-info-group info2)))

(defun gnus-group-sort-by-unread (info1 info2)
  "Sort by number of unread articles."
  (let ((n1 (car (gnus-gethash (gnus-info-group info1) gnus-newsrc-hashtb)))
	(n2 (car (gnus-gethash (gnus-info-group info2) gnus-newsrc-hashtb))))
    (< (or (and (numberp n1) n1) 0)
       (or (and (numberp n2) n2) 0))))

(defun gnus-group-sort-by-level (info1 info2)
  "Sort by level."
  (< (gnus-info-level info1) (gnus-info-level info2)))

(defun gnus-group-sort-by-method (info1 info2)
  "Sort alphabetically by backend name."
  (string< (symbol-name (car (gnus-find-method-for-group
			      (gnus-info-group info1))))
	   (symbol-name (car (gnus-find-method-for-group 
			      (gnus-info-group info2))))))

(defun gnus-group-sort-by-score (info1 info2)
  "Sort by group score."
  (< (gnus-info-score info1) (gnus-info-score info2)))

(defun gnus-group-sort-by-rank (info1 info2)
  "Sort by level and score."
  (let ((level1 (gnus-info-level info1))
	(level2 (gnus-info-level info2)))
    (or (< level1 level2)
	(and (= level1 level2)
	     (< (gnus-info-score info1) (gnus-info-score info2))))))

;; Group catching up.

(defun gnus-group-catchup-current (&optional n all)
  "Mark all articles not marked as unread in current newsgroup as read.
If prefix argument N is numeric, the ARG next newsgroups will be
caught up.  If ALL is non-nil, marked articles will also be marked as
read.  Cross references (Xref: header) of articles are ignored.
The difference between N and actual number of newsgroups that were
caught up is returned."
  (interactive "P")
  (if (not (or (not gnus-interactive-catchup) ;Without confirmation?
	       gnus-expert-user
	       (gnus-y-or-n-p
		(if all
		    "Do you really want to mark all articles as read? "
		  "Mark all unread articles as read? "))))
      n
    (let ((groups (gnus-group-process-prefix n))
	  (ret 0))
      (while groups
	;; Virtual groups have to be given special treatment. 
	(let ((method (gnus-find-method-for-group (car groups))))
	  (if (eq 'nnvirtual (car method))
	      (nnvirtual-catchup-group
	       (gnus-group-real-name (car groups)) (nth 1 method) all)))
	(gnus-group-remove-mark (car groups))
	(if (prog1
		(gnus-group-goto-group (car groups))
	      (gnus-group-catchup (car groups) all))
	    (gnus-group-update-group-line)
	  (setq ret (1+ ret)))
	(setq groups (cdr groups)))
      (gnus-group-next-unread-group 1)
      ret)))

(defun gnus-group-catchup-current-all (&optional n)
  "Mark all articles in current newsgroup as read.
Cross references (Xref: header) of articles are ignored."
  (interactive "P")
  (gnus-group-catchup-current n 'all))

(defun gnus-group-catchup (group &optional all)
  "Mark all articles in GROUP as read.
If ALL is non-nil, all articles are marked as read.
The return value is the number of articles that were marked as read,
or nil if no action could be taken."
  (let* ((entry (gnus-gethash group gnus-newsrc-hashtb))
	 (num (car entry))
	 (marked (nth 3 (nth 2 entry))))
    (if (not (numberp (car entry)))
	(gnus-message 1 "Can't catch up; non-active group")
      ;; Do the updating only if the newsgroup isn't killed.
      (if (not entry)
	  ()
	(gnus-update-read-articles 
	 group nil nil (and (not all) (cdr (assq 'tick marked))))
	(and all 
	     (setq marked (nth 3 (nth 2 entry)))
	     (setcar (nthcdr 3 (nth 2 entry)) 
		     (delq (assq 'dormant marked) 
			   (nth 3 (nth 2 entry)))))))
    num))

(defun gnus-group-expire-articles (&optional n)
  "Expire all expirable articles in the current newsgroup."
  (interactive "P")
  (let ((groups (gnus-group-process-prefix n))
	group)
    (or groups (error "No groups to expire"))
    (while groups
      (setq group (car groups)
	    groups (cdr groups))
      (gnus-group-remove-mark group)
      (if (not (gnus-check-backend-function 'request-expire-articles group))
	  ()
	(let* ((info (gnus-get-info group))
	       (expirable (if (gnus-group-total-expirable-p group)
			      (cons nil (gnus-list-of-read-articles group))
			    (assq 'expire (gnus-info-marks info)))))
	  (and expirable 
	       (setcdr expirable
		       (gnus-request-expire-articles 
			(cdr expirable) group))))))))

(defun gnus-group-expire-all-groups ()
  "Expire all expirable articles in all newsgroups."
  (interactive)
  (save-excursion
    (gnus-message 5 "Expiring...")
    (let ((gnus-group-marked (mapcar (lambda (info) (gnus-info-group info))
				     (cdr gnus-newsrc-alist))))
      (gnus-group-expire-articles nil)))
  (gnus-group-position-point)
  (gnus-message 5 "Expiring...done"))

(defun gnus-group-set-current-level (n level)
  "Set the level of the next N groups to LEVEL."
  (interactive 
   (list
    current-prefix-arg
    (string-to-int
     (let ((s (read-string 
	       (format "Level (default %s): " (gnus-group-group-level)))))
       (if (string-match "^\\s-*$" s)
	   (int-to-string (gnus-group-group-level))
	 s)))))
  (or (and (>= level 1) (<= level gnus-level-killed))
      (error "Illegal level: %d" level))
  (let ((groups (gnus-group-process-prefix n))
	group)
    (while groups
      (setq group (car groups)
	    groups (cdr groups))
      (gnus-group-remove-mark group)
      (gnus-message 6 "Changed level of %s from %d to %d" 
		    group (gnus-group-group-level) level)
      (gnus-group-change-level group level
			       (gnus-group-group-level))
      (gnus-group-update-group-line)))
  (gnus-group-position-point))

(defun gnus-group-unsubscribe-current-group (&optional n)
  "Toggle subscription of the current group.
If given numerical prefix, toggle the N next groups."
  (interactive "P")
  (let ((groups (gnus-group-process-prefix n))
	group)
    (while groups
      (setq group (car groups)
	    groups (cdr groups))
      (gnus-group-remove-mark group)
      (gnus-group-unsubscribe-group
       group (if (<= (gnus-group-group-level) gnus-level-subscribed)
		 gnus-level-default-unsubscribed
	       gnus-level-default-subscribed))
      (gnus-group-update-group-line))
    (gnus-group-next-group 1)))

(defun gnus-group-unsubscribe-group (group &optional level)
  "Toggle subscribe from/to unsubscribe GROUP.
New newsgroup is added to .newsrc automatically."
  (interactive
   (list (completing-read
	  "Group: " gnus-active-hashtb nil 
	  (memq gnus-select-method gnus-have-read-active-file))))
  (let ((newsrc (gnus-gethash group gnus-newsrc-hashtb)))
    (cond
     ((string-match "^[ \t]$" group)
      (error "Empty group name"))
     (newsrc
      ;; Toggle subscription flag.
      (gnus-group-change-level 
       newsrc (if level level (if (<= (nth 1 (nth 2 newsrc)) 
				      gnus-level-subscribed) 
				  (1+ gnus-level-subscribed)
				gnus-level-default-subscribed)))
      (gnus-group-update-group group))
     ((and (stringp group)
	   (or (not (memq gnus-select-method gnus-have-read-active-file))
	       (gnus-active group)))
      ;; Add new newsgroup.
      (gnus-group-change-level 
       group 
       (if level level gnus-level-default-subscribed) 
       (or (and (member group gnus-zombie-list) 
		gnus-level-zombie) 
	   gnus-level-killed)
       (and (gnus-group-group-name)
	    (gnus-gethash (gnus-group-group-name) gnus-newsrc-hashtb)))
      (gnus-group-update-group group))
     (t (error "No such newsgroup: %s" group)))
    (gnus-group-position-point)))

(defun gnus-group-transpose-groups (n)
  "Move the current newsgroup up N places.
If given a negative prefix, move down instead.  The difference between
N and the number of steps taken is returned." 
  (interactive "p")
  (or (gnus-group-group-name)
      (error "No group on current line"))
  (gnus-group-kill-group 1)
  (prog1
      (forward-line (- n))
    (gnus-group-yank-group)
    (gnus-group-position-point)))

(defun gnus-group-kill-all-zombies ()
  "Kill all zombie newsgroups."
  (interactive)
  (setq gnus-killed-list (nconc gnus-zombie-list gnus-killed-list))
  (setq gnus-zombie-list nil)
  (gnus-group-list-groups))

(defun gnus-group-kill-region (begin end)
  "Kill newsgroups in current region (excluding current point).
The killed newsgroups can be yanked by using \\[gnus-group-yank-group]."
  (interactive "r")
  (let ((lines
	 ;; Count lines.
	 (save-excursion
	   (count-lines
	    (progn
	      (goto-char begin)
	      (beginning-of-line)
	      (point))
	    (progn
	      (goto-char end)
	      (beginning-of-line)
	      (point))))))
    (goto-char begin)
    (beginning-of-line)			;Important when LINES < 1
    (gnus-group-kill-group lines)))

(defun gnus-group-kill-group (&optional n discard)
  "The the next N groups.
The killed newsgroups can be yanked by using \\[gnus-group-yank-group].
However, only groups that were alive can be yanked; already killed 
groups or zombie groups can't be yanked.
The return value is the name of the (last) group that was killed."
  (interactive "P")
  (let ((buffer-read-only nil)
	(groups (gnus-group-process-prefix n))
	group entry level)
    (if (or t (< (length groups) 10))
	;; This is faster when there are few groups.
	(while groups
	  (setq group (car groups)
		groups (cdr groups))
	  (gnus-group-remove-mark group)
	  (setq level (gnus-group-group-level))
	  (gnus-delete-line)
	  (if (and (not discard)
		   (setq entry (gnus-gethash group gnus-newsrc-hashtb)))
	      (setq gnus-list-of-killed-groups 
		    (cons (cons (car entry) (nth 2 entry)) 
			  gnus-list-of-killed-groups)))
	  (gnus-group-change-level 
	   (if entry entry group) gnus-level-killed (if entry nil level)))
      ;; If there are lots and lots of groups to be killed, we use
      ;; this thing instead.
      ;; !!! Not written.
      )
      
    (gnus-group-position-point)
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
      ;; backward until something suitable is found.  If there are no
      ;; other newsgroups in this buffer, just make this newsgroup the
      ;; first newsgroup.
      (setq prev (gnus-group-group-name))
      (gnus-group-change-level 
       info (nth 2 info) gnus-level-killed 
       (and prev (gnus-gethash prev gnus-newsrc-hashtb))
       t)
      (gnus-group-insert-group-line-info (nth 1 info))
      (setq gnus-list-of-killed-groups 
	    (cdr gnus-list-of-killed-groups)))
    (forward-line -1)
    (gnus-group-position-point)
    group))
      
(defun gnus-group-list-all-groups (&optional arg)
  "List all newsgroups with level ARG or lower.
Default is gnus-level-unsubscribed, which lists all subscribed and most
unsubscribed groups."
  (interactive "P")
  (gnus-group-list-groups (or arg gnus-level-unsubscribed) t))

;; Redefine this to list ALL killed groups if prefix arg used.
;; Rewritten by engstrom@src.honeywell.com (Eric Engstrom).
(defun gnus-group-list-killed (&optional arg)
  "List all killed newsgroups in the group buffer.
If ARG is non-nil, list ALL killed groups known to Gnus.  This may
entail asking the server for the groups."
  (interactive "P")
  ;; Find all possible killed newsgroups if arg.
  (when arg
    ;; First make sure active file has been read.
    (or gnus-have-read-active-file (gnus-read-active-file))
    (or gnus-killed-hashtb (gnus-make-hashtable-from-killed))
    ;; Go through all newsgroups that are known to Gnus - enlarge kill list
    (mapatoms
     (lambda (sym)
       (let ((groups 0)
	     (group (symbol-name sym)))
	 (if (or (null group)
		 (gnus-gethash group gnus-killed-hashtb)
		 (gnus-gethash group gnus-newsrc-hashtb))
	     ()
	   (let ((do-sub (gnus-matches-options-n group)))
	     (if (or (eq do-sub 'subscribe) (eq do-sub 'ignore))
		 ()
	       (setq groups (1+ groups))
	       (setq gnus-killed-list 
		     (cons group gnus-killed-list))
	       (gnus-sethash group group gnus-killed-hashtb))))))
     gnus-active-hashtb))
  (if (not gnus-killed-list)
      (gnus-message 6 "No killed groups")
    (let (gnus-group-list-mode)
      (funcall gnus-group-prepare-function 
	       gnus-level-killed t gnus-level-killed))
    (goto-char (point-min)))
  (gnus-group-position-point))

(defun gnus-group-list-zombies ()
  "List all zombie newsgroups in the group buffer."
  (interactive)
  (if (not gnus-zombie-list)
      (gnus-message 6 "No zombie groups")
    (let (gnus-group-list-mode)
      (funcall gnus-group-prepare-function
	       gnus-level-zombie t gnus-level-zombie))
    (goto-char (point-min)))
  (gnus-group-position-point))

(defun gnus-group-list-active ()
  "List all groups that are available from the server(s)."
  (interactive)
  ;; First we make sure that we have really read the active file. 
  (or gnus-have-read-active-file
      (gnus-read-active-file))
  ;; Find all groups and sort them.
  (let ((groups 
	 (sort 
	  (let (list)
	    (mapatoms
	     (lambda (sym)
	       (and (symbol-value sym)
		    (setq list (cons (symbol-name sym) list))))
	     gnus-active-hashtb)
	    list)
	  'string<))
	(buffer-read-only nil))
    (erase-buffer)
    (while groups
      (gnus-group-insert-group-line-info (car groups))
      (setq groups (cdr groups)))
    (goto-char (point-min))))

(defun gnus-group-get-new-news (&optional arg)
  "Get newly arrived articles.
If ARG is a number, it specifies which levels you are interested in
re-scanning.  If ARG is non-nil and not a number, this will force
\"hard\" re-reading of the active files from all servers."
  (interactive "P")
  (run-hooks 'gnus-get-new-news-hook)
  ;; We might read in new NoCeM messages here.
  (and gnus-use-nocem (gnus-nocem-scan-groups))
  ;; If ARG is not a number, then we read the active file.
  (and arg
       (not (numberp arg))
       (progn
	 (let ((gnus-read-active-file t))
	   (gnus-read-active-file))
	 (setq arg nil)))

  (setq arg (gnus-group-default-level arg t))
  (if (and gnus-read-active-file (not arg))
      (progn
	(gnus-read-active-file)
	(gnus-get-unread-articles (or arg (1+ gnus-level-subscribed))))
    (let ((gnus-read-active-file (if arg nil gnus-read-active-file)))
      (gnus-get-unread-articles (or arg (1+ gnus-level-subscribed)))))
  (gnus-group-list-groups))

(defun gnus-group-get-new-news-this-group (&optional n)
  "Check for newly arrived news in the current group (and the N-1 next groups).
The difference between N and the number of newsgroup checked is returned.
If N is negative, this group and the N-1 previous groups will be checked."
  (interactive "P")
  (let* ((groups (gnus-group-process-prefix n))
	 (ret (if (numberp n) (- n (length groups)) 0))
	 group)
    (while groups
      (setq group (car groups)
	    groups (cdr groups))
      (gnus-group-remove-mark group)
      (or (gnus-get-new-news-in-group group)
	  (progn 
	    (ding) 
	    (message "%s error: %s" group (gnus-status-message group))
	    (sit-for 2))))
    (gnus-group-next-unread-group 1 t)
    (gnus-summary-position-point)
    ret))

(defun gnus-get-new-news-in-group (group)
  (when (and group (gnus-activate-group group 'scan))
    (gnus-get-unread-articles-in-group 
     (gnus-get-info group) (gnus-active group))
    (gnus-group-update-group-line)
    t))

(defun gnus-group-fetch-faq (group &optional faq-dir)
  "Fetch the FAQ for the current group."
  (interactive 
   (list
    (gnus-group-real-name (gnus-group-group-name))
    (cond (current-prefix-arg
	   (completing-read 
	    "Faq dir: " (and (listp gnus-group-faq-directory) 
			     gnus-group-faq-directory))))))
  (or faq-dir
      (setq faq-dir (if (listp gnus-group-faq-directory)
                        (car gnus-group-faq-directory)
                      gnus-group-faq-directory)))
  (or group (error "No group name given"))
  (let ((file (concat (file-name-as-directory faq-dir)
		      (gnus-group-real-name group))))
    (if (not (file-exists-p file))
	(error "No such file: %s" file)
      (find-file file))))
  
(defun gnus-group-describe-group (force &optional group)
  "Display a description of the current newsgroup."
  (interactive (list current-prefix-arg (gnus-group-group-name)))
  (and force (setq gnus-description-hashtb nil))
  (let ((method (gnus-find-method-for-group group))
	desc)
    (or group (error "No group name given"))
    (and (or (and gnus-description-hashtb
		  ;; We check whether this group's method has been
		  ;; queried for a description file.  
		  (gnus-gethash 
		   (gnus-group-prefixed-name "" method) 
		   gnus-description-hashtb))
	     (setq desc (gnus-group-get-description group))
	     (gnus-read-descriptions-file method))
	 (message
	  (or desc (gnus-gethash group gnus-description-hashtb)
	      "No description available")))))

;; Suggested by Per Abrahamsen <amanda@iesd.auc.dk>.
(defun gnus-group-describe-all-groups (&optional force)
  "Pop up a buffer with descriptions of all newsgroups."
  (interactive "P")
  (and force (setq gnus-description-hashtb nil))
  (if (not (or gnus-description-hashtb
	       (gnus-read-all-descriptions-files)))
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
		       'gnus-unread t 'gnus-marked nil
		       'gnus-level (1+ gnus-level-subscribed))))
     gnus-description-hashtb)
    (goto-char (point-min))
    (gnus-group-position-point)))

;; Suggested by by Daniel Quinlan <quinlan@best.com>.
(defun gnus-group-apropos (regexp &optional search-description)
  "List all newsgroups that have names that match a regexp."
  (interactive "sGnus apropos (regexp): ")
  (let ((prev "")
	(obuf (current-buffer))
	groups des)
    ;; Go through all newsgroups that are known to Gnus.
    (mapatoms 
     (lambda (group)
       (and (symbol-name group)
	    (string-match regexp (symbol-name group))
	    (setq groups (cons (symbol-name group) groups))))
     gnus-active-hashtb)
    ;; Go through all descriptions that are known to Gnus. 
    (if search-description
	(mapatoms 
	 (lambda (group)
	   (and (string-match regexp (symbol-value group))
		(gnus-active (symbol-name group))
		(setq groups (cons (symbol-name group) groups))))
	 gnus-description-hashtb))
    (if (not groups)
	(gnus-message 3 "No groups matched \"%s\"." regexp)
      ;; Print out all the groups.
      (save-excursion
	(pop-to-buffer "*Gnus Help*")
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
	(goto-char (point-min))))
    (pop-to-buffer obuf)))

(defun gnus-group-description-apropos (regexp)
  "List all newsgroups that have names or descriptions that match a regexp."
  (interactive "sGnus description apropos (regexp): ")
  (if (not (or gnus-description-hashtb
	       (gnus-read-all-descriptions-files)))
      (error "Couldn't request descriptions file"))
  (gnus-group-apropos regexp t))

;; Suggested by Per Abrahamsen <amanda@iesd.auc.dk>.
(defun gnus-group-list-matching (level regexp &optional all lowest) 
  "List all groups with unread articles that match REGEXP.
If the prefix LEVEL is non-nil, it should be a number that says which
level to cut off listing groups. 
If ALL, also list groups with no unread articles.
If LOWEST, don't list groups with level lower than LOWEST."
  (interactive "P\nsList newsgroups matching: ")
  (gnus-group-prepare-flat (or level gnus-level-subscribed)
			   all (or lowest 1) regexp)
  (goto-char (point-min))
  (gnus-group-position-point))

(defun gnus-group-list-all-matching (level regexp &optional lowest) 
  "List all groups that match REGEXP.
If the prefix LEVEL is non-nil, it should be a number that says which
level to cut off listing groups. 
If LOWEST, don't list groups with level lower than LOWEST."
  (interactive "P\nsList newsgroups matching: ")
  (gnus-group-list-matching (or level gnus-level-killed) regexp t lowest))

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
  (gnus-group-list-groups arg))

(defun gnus-group-read-init-file ()
  "Read the Gnus elisp init file."
  (interactive)
  (gnus-read-init-file))

(defun gnus-group-check-bogus-groups (&optional silent)
  "Check bogus newsgroups.
If given a prefix, don't ask for confirmation before removing a bogus
group."
  (interactive "P")
  (gnus-check-bogus-newsgroups (and (not silent) (not gnus-expert-user)))
  (gnus-group-list-groups))

(defun gnus-group-edit-global-kill (&optional article group)
  "Edit the global kill file.
If GROUP, edit that local kill file instead."
  (interactive "P")
  (setq gnus-current-kill-article article)
  (gnus-kill-file-edit-file group)
  (gnus-message 
   6
   (substitute-command-keys
    "Editing a global kill file (Type \\[gnus-kill-file-exit] to exit)")))

(defun gnus-group-edit-local-kill (article group)
  "Edit a local kill file."
  (interactive (list nil (gnus-group-group-name)))
  (gnus-group-edit-global-kill article group))

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
    ;; Do this on a separate list in case the user does a ^G before we finish
    (let ((gnus-buffer-list
	   (delq group-buf (delq gnus-dribble-buffer
				 (append gnus-buffer-list nil)))))
      (while gnus-buffer-list
	(gnus-kill-buffer (car gnus-buffer-list))
	(setq gnus-buffer-list (cdr gnus-buffer-list))))
    (if group-buf
	(progn
	  (setq gnus-buffer-list (list group-buf))
	  (bury-buffer group-buf)
	  (delete-windows-on group-buf t)))))

(defun gnus-group-clear-dribble ()
  "Clear all information from the dribble buffer."
  (interactive)
  (gnus-dribble-clear))

(defun gnus-group-exit ()
  "Quit reading news after updating .newsrc.eld and .newsrc.
The hook `gnus-exit-gnus-hook' is called before actually exiting."
  (interactive)
  (if (or noninteractive		;For gnus-batch-kill
	  (not (gnus-server-opened gnus-select-method)) ;NNTP connection closed
	  (not gnus-interactive-exit)	;Without confirmation
	  gnus-expert-user
	  (gnus-y-or-n-p "Are you sure you want to quit reading news? "))
      (progn
	(run-hooks 'gnus-exit-gnus-hook)
	;; Offer to save data from non-quitted summary buffers.
	(gnus-offer-save-summaries)
	;; Save the newsrc file(s).
	(gnus-save-newsrc-file)
	;; Kill-em-all.
	(gnus-close-backends)
	;; Reset everything.
	(gnus-clear-system))))

(defun gnus-close-backends ()
  ;; Send a close request to all backends that support such a request. 
  (let ((methods gnus-valid-select-methods)
	func)
    (while methods
      (if (fboundp (setq func (intern (concat (car (car methods))
					      "-request-close"))))
	  (funcall func))
      (setq methods (cdr methods)))))

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
	(if gnus-use-full-window
	    (delete-other-windows)
	  (gnus-remove-some-windows))
	(gnus-dribble-save)
	(gnus-close-backends)
	(gnus-clear-system))))

(defun gnus-offer-save-summaries ()
  (save-excursion
    (let ((buflist (buffer-list)) 
	  buffers bufname)
      (while buflist
	(and (setq bufname (buffer-name (car buflist)))
	     (string-match "Summary" bufname)
	     (save-excursion
	       (set-buffer bufname)
	       ;; We check that this is, indeed, a summary buffer.
	       (eq major-mode 'gnus-summary-mode))
	     (setq buffers (cons bufname buffers)))
	(setq buflist (cdr buflist)))
      (and buffers
	   (map-y-or-n-p 
	    "Update summary buffer %s? "
	    (lambda (buf)
	      (set-buffer buf)
	      (gnus-summary-exit))
	    buffers)))))

(defun gnus-group-describe-briefly ()
  "Give a one line description of the group mode commands."
  (interactive)
  (gnus-message 7 (substitute-command-keys "\\<gnus-group-mode-map>\\[gnus-group-read-group]:Select  \\[gnus-group-next-unread-group]:Forward  \\[gnus-group-prev-unread-group]:Backward  \\[gnus-group-exit]:Exit  \\[gnus-info-find-node]:Run Info  \\[gnus-group-describe-briefly]:This help")))

(defun gnus-group-browse-foreign-server (method)
  "Browse a foreign news server.
If called interactively, this function will ask for a select method
 (nntp, nnspool, etc.) and a server address (eg. nntp.some.where). 
If not, METHOD should be a list where the first element is the method
and the second element is the address."
  (interactive
   (list (let ((how (completing-read 
		     "Which backend: "
		     (append gnus-valid-select-methods gnus-server-alist)
		     nil t "nntp")))
	   ;; We either got a backend name or a virtual server name.
	   ;; If the first, we also need an address.
	   (if (assoc how gnus-valid-select-methods)
	       (list (intern how)
		     ;; Suggested by mapjph@bath.ac.uk.
		     (completing-read 
		      "Address: " 
		      (mapcar (lambda (server) (list server))
			      gnus-secondary-servers)))
	     ;; We got a server name, so we find the method.
	     (gnus-server-to-method how)))))
  (gnus-browse-foreign-server method))


;;;
;;; Browse Server Mode
;;;

(defvar gnus-browse-mode-hook nil)
(defvar gnus-browse-mode-map nil)
(put 'gnus-browse-mode 'mode-class 'special)

(if gnus-browse-mode-map
    nil
  (setq gnus-browse-mode-map (make-keymap))
  (suppress-keymap gnus-browse-mode-map)
  (define-key gnus-browse-mode-map " " 'gnus-browse-read-group)
  (define-key gnus-browse-mode-map "=" 'gnus-browse-select-group)
  (define-key gnus-browse-mode-map "n" 'gnus-browse-next-group)
  (define-key gnus-browse-mode-map "p" 'gnus-browse-prev-group)
  (define-key gnus-browse-mode-map "\177" 'gnus-browse-prev-group)
  (define-key gnus-browse-mode-map "N" 'gnus-browse-next-group)
  (define-key gnus-browse-mode-map "P" 'gnus-browse-prev-group)
  (define-key gnus-browse-mode-map "\M-n" 'gnus-browse-next-group)
  (define-key gnus-browse-mode-map "\M-p" 'gnus-browse-prev-group)
  (define-key gnus-browse-mode-map "\r" 'gnus-browse-select-group)
  (define-key gnus-browse-mode-map "u" 'gnus-browse-unsubscribe-current-group)
  (define-key gnus-browse-mode-map "l" 'gnus-browse-exit)
  (define-key gnus-browse-mode-map "L" 'gnus-browse-exit)
  (define-key gnus-browse-mode-map "q" 'gnus-browse-exit)
  (define-key gnus-browse-mode-map "Q" 'gnus-browse-exit)
  (define-key gnus-browse-mode-map "\C-c\C-c" 'gnus-browse-exit)
  (define-key gnus-browse-mode-map "?" 'gnus-browse-describe-briefly)
  (define-key gnus-browse-mode-map "\C-c\C-i" 'gnus-info-find-node)
  )

(defvar gnus-browse-current-method nil)
(defvar gnus-browse-return-buffer nil)

(defvar gnus-browse-buffer "*Gnus Browse Server*")

(defun gnus-browse-foreign-server (method &optional return-buffer)
  (setq gnus-browse-current-method method)
  (setq gnus-browse-return-buffer return-buffer)
  (let ((gnus-select-method method)
	groups group)
    (gnus-message 5 "Connecting to %s..." (nth 1 method))
    (or (gnus-check-server method)
	(error "Unable to contact server: %s" (gnus-status-message method)))
    (or (gnus-request-list method)
	(error "Couldn't request list: %s" (gnus-status-message method)))
    (get-buffer-create gnus-browse-buffer)
    (gnus-add-current-to-buffer-list)
    (and gnus-carpal (gnus-carpal-setup-buffer 'browse))
    (gnus-configure-windows 'browse)
    (buffer-disable-undo (current-buffer))
    (let ((buffer-read-only nil))
      (erase-buffer))
    (gnus-browse-mode)
    (setq mode-line-buffer-identification
	  (format
	   "Gnus  Browse Server {%s:%s}" (car method) (car (cdr method))))
    (save-excursion
      (set-buffer nntp-server-buffer)
      (let ((cur (current-buffer)))
	(goto-char (point-min))
	(or (string= gnus-ignored-newsgroups "")
	    (delete-matching-lines gnus-ignored-newsgroups))
	(while (re-search-forward 
		"\\(^[^ \t]+\\)[ \t]+[0-9]+[ \t]+[0-9]+" nil t)
	  (goto-char (match-end 1))
	  (setq groups (cons (cons (match-string 1)
				   (max 0 (- (1+ (read cur)) (read cur))))
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
    (goto-char (point-min))
    (gnus-group-position-point)))

(defun gnus-browse-mode ()
  "Major mode for browsing a foreign server.

All normal editing commands are switched off.

\\<gnus-browse-mode-map>
The only things you can do in this buffer is

1) `\\[gnus-browse-unsubscribe-current-group]' to subscribe to a group.
The group will be inserted into the group buffer upon exit from this
buffer.  

2) `\\[gnus-browse-read-group]' to read a group ephemerally.

3) `\\[gnus-browse-exit]' to return to the group buffer."
  (interactive)
  (kill-all-local-variables)
  (if (gnus-visual-p 'browse-menu 'menu) (gnus-browse-make-menu-bar))
  (setq mode-line-modified "-- ")
  (make-local-variable 'mode-line-format)
  (setq mode-line-format (copy-sequence mode-line-format))
  (and (equal (nth 3 mode-line-format) "   ")
       (setcar (nthcdr 3 mode-line-format) ""))
  (setq major-mode 'gnus-browse-mode)
  (setq mode-name "Browse Server")
  (setq mode-line-process nil)
  (use-local-map gnus-browse-mode-map)
  (buffer-disable-undo (current-buffer))
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (run-hooks 'gnus-browse-mode-hook))

(defun gnus-browse-read-group (&optional no-article)
  "Enter the group at the current line."
  (interactive)
  (let ((group (gnus-browse-group-name)))
    (or (gnus-group-read-ephemeral-group 
	 group gnus-browse-current-method nil
	 (cons (current-buffer) 'browse))
	(error "Couldn't enter %s" group))))

(defun gnus-browse-select-group ()
  "Select the current group."
  (interactive)
  (gnus-browse-read-group 'no))

(defun gnus-browse-next-group (n)
  "Go to the next group."
  (interactive "p")
  (prog1
      (forward-line n)
    (gnus-group-position-point)))

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
    (gnus-group-position-point)
    (if (/= 0 arg) (gnus-message 7 "No more newsgroups"))
    arg))

(defun gnus-browse-group-name ()
  (save-excursion
    (beginning-of-line)
    (when (re-search-forward ": \\(.*\\)$" (gnus-point-at-eol) t)
      (gnus-group-prefixed-name (match-string 1) gnus-browse-current-method))))
  
(defun gnus-browse-unsubscribe-group ()
  "Toggle subscription of the current group in the browse buffer."
  (let ((sub nil)
	(buffer-read-only nil)
	group)
    (save-excursion
      (beginning-of-line)
      ;; If this group it killed, then we want to subscribe it.
      (if (= (following-char) ?K) (setq sub t))
      (setq group (gnus-browse-group-name))
      (delete-char 1)
      (if sub
	  (progn
	    (gnus-group-change-level 
	     (list t group gnus-level-default-subscribed
		   nil nil gnus-browse-current-method) 
	     gnus-level-default-subscribed gnus-level-killed
	     (and (car (nth 1 gnus-newsrc-alist))
		  (gnus-gethash (car (nth 1 gnus-newsrc-alist))
				gnus-newsrc-hashtb))
	     t)
	    (insert ? ))
	(gnus-group-change-level 
	 group gnus-level-killed gnus-level-default-subscribed)
	(insert ?K)))
    t))

(defun gnus-browse-exit ()
  "Quit browsing and return to the group buffer."
  (interactive)
  (if (eq major-mode 'gnus-browse-mode)
      (kill-buffer (current-buffer)))
  (if gnus-browse-return-buffer
      (gnus-configure-windows 'server 'force)
    (gnus-configure-windows 'group 'force)
    (gnus-group-list-groups nil)))

(defun gnus-browse-describe-briefly ()
  "Give a one line description of the group mode commands."
  (interactive)
  (gnus-message 6
		(substitute-command-keys "\\<gnus-browse-mode-map>\\[gnus-group-next-group]:Forward  \\[gnus-group-prev-group]:Backward  \\[gnus-browse-exit]:Exit  \\[gnus-info-find-node]:Run Info  \\[gnus-browse-describe-briefly]:This help")))
      

;;;
;;; Gnus summary mode
;;;

(defvar gnus-summary-mode-map nil)
(defvar gnus-summary-mark-map nil)
(defvar gnus-summary-mscore-map nil)
(defvar gnus-summary-article-map nil)
(defvar gnus-summary-thread-map nil)
(defvar gnus-summary-goto-map nil)
(defvar gnus-summary-exit-map nil)
(defvar gnus-summary-interest-map nil)
(defvar gnus-summary-sort-map nil)
(defvar gnus-summary-backend-map nil)
(defvar gnus-summary-save-map nil)
(defvar gnus-summary-wash-map nil)
(defvar gnus-summary-wash-hide-map nil)
(defvar gnus-summary-wash-highlight-map nil)
(defvar gnus-summary-wash-time-map nil)
(defvar gnus-summary-help-map nil)
(defvar gnus-summary-limit-map nil)

(put 'gnus-summary-mode 'mode-class 'special)

(if gnus-summary-mode-map
    nil
  (setq gnus-summary-mode-map (make-keymap))
  (suppress-keymap gnus-summary-mode-map)

  ;; Non-orthogonal keys

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
  (define-key gnus-summary-mode-map 
    "\M-s" 'gnus-summary-search-article-forward)
  (define-key gnus-summary-mode-map 
    "\M-r" 'gnus-summary-search-article-backward)
  (define-key gnus-summary-mode-map "<" 'gnus-summary-beginning-of-article)
  (define-key gnus-summary-mode-map ">" 'gnus-summary-end-of-article)
  (define-key gnus-summary-mode-map "j" 'gnus-summary-goto-article)
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
  (define-key gnus-summary-mode-map 
    "k" 'gnus-summary-kill-same-subject-and-select)
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
  (define-key gnus-summary-mode-map "\C-w" 'gnus-summary-mark-region-as-read)
  (define-key gnus-summary-mode-map "\C-t" 'gnus-summary-toggle-truncation)
  (define-key gnus-summary-mode-map "?" 'gnus-summary-mark-as-dormant)
  (define-key gnus-summary-mode-map 
    "\C-c\M-\C-s" 'gnus-summary-limit-include-expunged)
  (define-key gnus-summary-mode-map 
    "\C-c\C-s\C-n" 'gnus-summary-sort-by-number)
  (define-key gnus-summary-mode-map 
    "\C-c\C-s\C-a" 'gnus-summary-sort-by-author)
  (define-key gnus-summary-mode-map 
    "\C-c\C-s\C-s" 'gnus-summary-sort-by-subject)
  (define-key gnus-summary-mode-map "\C-c\C-s\C-d" 'gnus-summary-sort-by-date)
  (define-key gnus-summary-mode-map "\C-c\C-s\C-i" 'gnus-summary-sort-by-score)
  (define-key gnus-summary-mode-map "=" 'gnus-summary-expand-window)
  (define-key gnus-summary-mode-map 
    "\C-x\C-s" 'gnus-summary-reselect-current-group)
  (define-key gnus-summary-mode-map "\M-g" 'gnus-summary-rescan-group)
  (define-key gnus-summary-mode-map "w" 'gnus-summary-stop-page-breaking)
  (define-key gnus-summary-mode-map "\C-c\C-r" 'gnus-summary-caesar-message)
  (define-key gnus-summary-mode-map "\M-t" 'gnus-summary-toggle-mime)
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
  (define-key gnus-summary-mode-map "Q" 'gnus-summary-exit-no-update)
  (define-key gnus-summary-mode-map "\C-c\C-i" 'gnus-info-find-node)
  (define-key gnus-summary-mode-map gnus-mouse-2 'gnus-mouse-pick-article)
  (define-key gnus-summary-mode-map "m" 'gnus-summary-mail-other-window)
  (define-key gnus-summary-mode-map "a" 'gnus-summary-post-news)
  (define-key gnus-summary-mode-map "x" 'gnus-summary-limit-to-unread)
  (define-key gnus-summary-mode-map "s" 'gnus-summary-isearch-article)
  (define-key gnus-summary-mode-map "t" 'gnus-summary-toggle-header)
  (define-key gnus-summary-mode-map "g" 'gnus-summary-show-article)
;  (define-key gnus-summary-mode-map "?" 'gnus-summary-describe-briefly)
  (define-key gnus-summary-mode-map "l" 'gnus-summary-goto-last-article)
  (define-key gnus-summary-mode-map "\C-c\C-v\C-v" 'gnus-uu-decode-uu-view)
  (define-key gnus-summary-mode-map "\C-d" 'gnus-summary-enter-digest-group)
  (define-key gnus-summary-mode-map "v" 'gnus-summary-verbose-headers)
  (define-key gnus-summary-mode-map "\C-c\C-b" 'gnus-bug)


  ;; Sort of orthogonal keymap
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
  (define-key gnus-summary-mark-map "S" 'gnus-summary-limit-include-expunged)
  (define-key gnus-summary-mark-map "C" 'gnus-summary-catchup)
  (define-key gnus-summary-mark-map "H" 'gnus-summary-catchup-to-here)
  (define-key gnus-summary-mark-map "\C-c" 'gnus-summary-catchup-all)
  (define-key gnus-summary-mark-map 
    "k" 'gnus-summary-kill-same-subject-and-select)
  (define-key gnus-summary-mark-map "K" 'gnus-summary-kill-same-subject)

  (define-prefix-command 'gnus-summary-mscore-map)
  (define-key gnus-summary-mark-map "V" 'gnus-summary-mscore-map)
  (define-key gnus-summary-mscore-map "c" 'gnus-summary-clear-above)
  (define-key gnus-summary-mscore-map "u" 'gnus-summary-tick-above)
  (define-key gnus-summary-mscore-map "m" 'gnus-summary-mark-above)
  (define-key gnus-summary-mscore-map "k" 'gnus-summary-kill-below)

  (define-key gnus-summary-mark-map "P" 'gnus-uu-mark-map)
  
  (define-key gnus-summary-mode-map "S" 'gnus-summary-send-map)

  (define-prefix-command 'gnus-summary-limit-map)
  (define-key gnus-summary-mode-map "/" 'gnus-summary-limit-map)
  (define-key gnus-summary-limit-map "/" 'gnus-summary-limit-to-subject)
  (define-key gnus-summary-limit-map "n" 'gnus-summary-limit-to-articles)
  (define-key gnus-summary-limit-map "w" 'gnus-summary-pop-limit)
  (define-key gnus-summary-limit-map "s" 'gnus-summary-limit-to-subject)
  (define-key gnus-summary-limit-map "u" 'gnus-summary-limit-to-unread)
  (define-key gnus-summary-limit-map "m" 'gnus-summary-limit-to-marks)
  (define-key gnus-summary-limit-map "v" 'gnus-summary-limit-to-score)
  (define-key gnus-summary-limit-map "D" 'gnus-summary-limit-include-dormant)
  (define-key gnus-summary-limit-map "d" 'gnus-summary-limit-exclude-dormant)
  (define-key gnus-summary-mark-map "E" 'gnus-summary-limit-include-expunged)
  (define-key gnus-summary-limit-map "c" 
    'gnus-summary-limit-exclude-childless-dormant)

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
  (define-key gnus-summary-goto-map "p" 'gnus-summary-pop-article)


  (define-prefix-command 'gnus-summary-thread-map)
  (define-key gnus-summary-mode-map "T" 'gnus-summary-thread-map)
  (define-key gnus-summary-thread-map "k" 'gnus-summary-kill-thread)
  (define-key gnus-summary-thread-map "l" 'gnus-summary-lower-thread)
  (define-key gnus-summary-thread-map "i" 'gnus-summary-raise-thread)
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
  (define-key gnus-summary-thread-map "\M-#" 'gnus-uu-unmark-thread)

  
  (define-prefix-command 'gnus-summary-exit-map)
  (define-key gnus-summary-mode-map "Z" 'gnus-summary-exit-map)
  (define-key gnus-summary-exit-map "c" 'gnus-summary-catchup-and-exit)
  (define-key gnus-summary-exit-map "C" 'gnus-summary-catchup-all-and-exit)
  (define-key gnus-summary-exit-map "E" 'gnus-summary-exit-no-update)
  (define-key gnus-summary-exit-map "Q" 'gnus-summary-exit)
  (define-key gnus-summary-exit-map "Z" 'gnus-summary-exit)
  (define-key gnus-summary-exit-map 
    "n" 'gnus-summary-catchup-and-goto-next-group)
  (define-key gnus-summary-exit-map "R" 'gnus-summary-reselect-current-group)
  (define-key gnus-summary-exit-map "G" 'gnus-summary-rescan-group)
  (define-key gnus-summary-exit-map "N" 'gnus-summary-next-group)
  (define-key gnus-summary-exit-map "P" 'gnus-summary-prev-group)


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
  (define-key gnus-summary-article-map "R" 'gnus-summary-refer-references)
  (define-key gnus-summary-article-map "g" 'gnus-summary-show-article)
  (define-key gnus-summary-article-map "s" 'gnus-summary-isearch-article)



  (define-prefix-command 'gnus-summary-wash-map)
  (define-key gnus-summary-mode-map "W" 'gnus-summary-wash-map)

  (define-prefix-command 'gnus-summary-wash-hide-map)
  (define-key gnus-summary-wash-map "W" 'gnus-summary-wash-hide-map)
  (define-key gnus-summary-wash-hide-map "a" 'gnus-article-hide)
  (define-key gnus-summary-wash-hide-map "h" 'gnus-article-hide-headers)
  (define-key gnus-summary-wash-hide-map "s" 'gnus-article-hide-signature)
  (define-key gnus-summary-wash-hide-map "c" 'gnus-article-hide-citation)
  (define-key gnus-summary-wash-hide-map "p" 'gnus-article-hide-pgp)
  (define-key gnus-summary-wash-hide-map 
    "\C-c" 'gnus-article-hide-citation-maybe)

  (define-prefix-command 'gnus-summary-wash-highlight-map)
  (define-key gnus-summary-wash-map "H" 'gnus-summary-wash-highlight-map)
  (define-key gnus-summary-wash-highlight-map "a" 'gnus-article-highlight)
  (define-key gnus-summary-wash-highlight-map 
    "h" 'gnus-article-highlight-headers)
  (define-key gnus-summary-wash-highlight-map
    "c" 'gnus-article-highlight-citation)
  (define-key gnus-summary-wash-highlight-map
    "s" 'gnus-article-highlight-signature)

  (define-prefix-command 'gnus-summary-wash-time-map)
  (define-key gnus-summary-wash-map "T" 'gnus-summary-wash-time-map)
  (define-key gnus-summary-wash-time-map "z" 'gnus-article-date-ut)
  (define-key gnus-summary-wash-time-map "u" 'gnus-article-date-ut)
  (define-key gnus-summary-wash-time-map "l" 'gnus-article-date-local)
  (define-key gnus-summary-wash-time-map "e" 'gnus-article-date-lapsed)
  (define-key gnus-summary-wash-time-map "o" 'gnus-article-date-original)

  (define-key gnus-summary-wash-map "b" 'gnus-article-add-buttons)
  (define-key gnus-summary-wash-map "B" 'gnus-article-add-buttons-to-head)
  (define-key gnus-summary-wash-map "o" 'gnus-article-treat-overstrike)
  (define-key gnus-summary-wash-map "w" 'gnus-article-word-wrap)
  (define-key gnus-summary-wash-map "c" 'gnus-article-remove-cr)
  (define-key gnus-summary-wash-map "q" 'gnus-article-de-quoted-unreadable)
  (define-key gnus-summary-wash-map "f" 'gnus-article-display-x-face)
  (define-key gnus-summary-wash-map "l" 'gnus-summary-stop-page-breaking)
  (define-key gnus-summary-wash-map "r" 'gnus-summary-caesar-message)
  (define-key gnus-summary-wash-map "t" 'gnus-summary-toggle-header)
  (define-key gnus-summary-wash-map "m" 'gnus-summary-toggle-mime)


  (define-prefix-command 'gnus-summary-help-map)
  (define-key gnus-summary-mode-map "H" 'gnus-summary-help-map)
  (define-key gnus-summary-help-map "v" 'gnus-version)
  (define-key gnus-summary-help-map "f" 'gnus-summary-fetch-faq)
  (define-key gnus-summary-help-map "d" 'gnus-summary-describe-group)
  (define-key gnus-summary-help-map "h" 'gnus-summary-describe-briefly)
  (define-key gnus-summary-help-map "i" 'gnus-info-find-node)


  (define-prefix-command 'gnus-summary-backend-map)
  (define-key gnus-summary-mode-map "B" 'gnus-summary-backend-map)
  (define-key gnus-summary-backend-map "e" 'gnus-summary-expire-articles)
  (define-key gnus-summary-backend-map "\M-\C-e" 
    'gnus-summary-expire-articles-now)
  (define-key gnus-summary-backend-map "\177" 'gnus-summary-delete-article)
  (define-key gnus-summary-backend-map "m" 'gnus-summary-move-article)
  (define-key gnus-summary-backend-map "r" 'gnus-summary-respool-article)
  (define-key gnus-summary-backend-map "w" 'gnus-summary-edit-article)
  (define-key gnus-summary-backend-map "c" 'gnus-summary-copy-article)
  (define-key gnus-summary-backend-map "q" 'gnus-summary-respool-query)
  (define-key gnus-summary-backend-map "i" 'gnus-summary-import-article)


  (define-prefix-command 'gnus-summary-save-map)
  (define-key gnus-summary-mode-map "O" 'gnus-summary-save-map)
  (define-key gnus-summary-save-map "o" 'gnus-summary-save-article)
  (define-key gnus-summary-save-map "m" 'gnus-summary-save-article-mail)
  (define-key gnus-summary-save-map "r" 'gnus-summary-save-article-rmail)
  (define-key gnus-summary-save-map "f" 'gnus-summary-save-article-file)
  (define-key gnus-summary-save-map "b" 'gnus-summary-save-article-body-file)
  (define-key gnus-summary-save-map "h" 'gnus-summary-save-article-folder)
  (define-key gnus-summary-save-map "v" 'gnus-summary-save-article-vm)
  (define-key gnus-summary-save-map "p" 'gnus-summary-pipe-output)
  (define-key gnus-summary-save-map "s" 'gnus-soup-add-article)

  (define-key gnus-summary-mode-map "X" 'gnus-uu-extract-map)

  (define-key gnus-summary-mode-map "\M-&" 'gnus-summary-universal-argument)
  (define-key gnus-summary-article-map "D" 'gnus-summary-enter-digest-group)

  (define-key gnus-summary-mode-map "V" 'gnus-summary-score-map)

  (define-key gnus-summary-mode-map "I" 'gnus-summary-increase-score)
  (define-key gnus-summary-mode-map "L" 'gnus-summary-lower-score)
  )




(defun gnus-summary-mode (&optional group)
  "Major mode for reading articles.

All normal editing commands are switched off.
\\<gnus-summary-mode-map>
Each line in this buffer represents one article.  To read an
article, you can, for instance, type `\\[gnus-summary-next-page]'.  To move forwards
and backwards while displaying articles, type `\\[gnus-summary-next-unread-article]' and `\\[gnus-summary-prev-unread-article]', 
respectively.

You can also post articles and send mail from this buffer.  To 
follow up an article, type `\\[gnus-summary-followup]'.  To mail a reply to the author 
of an article, type `\\[gnus-summary-reply]'.

There are approx. one gazillion commands you can execute in this 
buffer; read the info pages for more information (`\\[gnus-info-find-node]'). 

The following commands are available:

\\{gnus-summary-mode-map}"
  (interactive)
  (if (gnus-visual-p 'summary-menu 'menu) (gnus-summary-make-menu-bar))
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
  (gnus-make-thread-indent-array)
  (setq mode-line-modified "-- ")
  (make-local-variable 'mode-line-format)
  (setq mode-line-format (copy-sequence mode-line-format))
  (and (equal (nth 3 mode-line-format) "   ")
       (setcar (nthcdr 3 mode-line-format) ""))
  (setq major-mode 'gnus-summary-mode)
  (setq mode-name "Summary")
  (make-local-variable 'minor-mode-alist)
  (use-local-map gnus-summary-mode-map)
  (buffer-disable-undo (current-buffer))
  (setq buffer-read-only t)		;Disable modification
  (setq truncate-lines t)
  (setq selective-display t)
  (setq selective-display-ellipses t)	;Display `...'
  (setq buffer-display-table gnus-summary-display-table)
  (setq gnus-newsgroup-name group)
  (run-hooks 'gnus-summary-mode-hook))

(defun gnus-summary-make-display-table ()
  ;; Change the display table.  Odd characters have a tendency to mess
  ;; up nicely formatted displays - we make all possible glyphs
  ;; display only a single character.

  ;; We start from the standard display table, if any.
  (setq gnus-summary-display-table 
	(or (copy-sequence standard-display-table)
	    (make-display-table)))
  ;; Nix out all the control chars...
  (let ((i 32))
    (while (>= (setq i (1- i)) 0)
      (aset gnus-summary-display-table i [??])))
  ;; ... but not newline and cr, of course. (cr is necessary for the
  ;; selective display).  
  (aset gnus-summary-display-table ?\n nil)
  (aset gnus-summary-display-table ?\r nil)
  ;; We nix out any glyphs over 126 that are not set already.  
  (let ((i 256))
    (while (>= (setq i (1- i)) 127)
      ;; Only modify if the entry is nil.
      (or (aref gnus-summary-display-table i) 
	  (aset gnus-summary-display-table i [??])))))

(defun gnus-summary-clear-local-variables ()
  (let ((locals gnus-summary-local-variables))
    (while locals
      (if (consp (car locals))
	  (and (vectorp (car (car locals)))
	       (set (car (car locals)) nil))
	(and (vectorp (car locals))
	     (set (car locals) nil)))
      (setq locals (cdr locals)))))

;; Summary data functions.

(defmacro gnus-data-number (data)
  (` (car (, data))))

(defmacro gnus-data-mark (data)
  (` (nth 1 (, data))))

(defmacro gnus-data-set-mark (data mark)
  (` (setcar (nthcdr 1 (, data)) (, mark))))

(defmacro gnus-data-pos (data)
  (` (nth 2 (, data))))

(defmacro gnus-data-set-pos (data pos)
  (` (setcar (nthcdr 2 (, data)) (, pos))))

(defmacro gnus-data-header (data)
  (` (nth 3 (, data))))

(defmacro gnus-data-level (data)
  (` (nth 4 (, data))))

(defmacro gnus-data-unread-p (data)
  (` (= (nth 1 (, data)) gnus-unread-mark)))

(defmacro gnus-data-pseudo-p (data)
  (` (consp (nth 3 (, data)))))

(defmacro gnus-data-find (number)
  (` (assq (, number) gnus-newsgroup-data)))

(defmacro gnus-data-find-list (number &optional data)
  (` (memq (assq (, number) (, (or data 'gnus-newsgroup-data)))
	   (, (or data 'gnus-newsgroup-data)))))

(defmacro gnus-data-make (number mark pos header level)
  (` (list (, number) (, mark) (, pos) (, header) (, level))))

(defun gnus-data-enter (after-article number mark pos header level offset)
  (let ((data (gnus-data-find-list after-article)))
    (or data (error "No such article: %d" after-article))
    (setcdr data (cons (gnus-data-make number mark pos header level)
		       (cdr data)))
    (setq gnus-newsgroup-data-reverse nil)
    (gnus-data-update-list (cdr (cdr data)) offset)))

(defun gnus-data-enter-list (after-article list &optional offset)
  (when list
    (let ((data (and after-article (gnus-data-find-list after-article)))
	  (ilist list))
      (or data (not after-article) (error "No such article: %d" after-article))
      ;; Find the last element in the list to be spliced into the main
      ;; list.  
      (while (cdr list)
	(setq list (cdr list)))
      (if (not data)
	  (progn
	    (setcdr list gnus-newsgroup-data)
	    (setq gnus-newsgroup-data ilist)
	    (and offset (gnus-data-update-list (cdr list) offset)))
	(setcdr list (cdr data))
	(setcdr data ilist)
	(and offset (gnus-data-update-list (cdr data) offset)))
      (setq gnus-newsgroup-data-reverse nil))))

(defun gnus-data-remove (article &optional offset)
  (let ((data gnus-newsgroup-data))
    (if (= (gnus-data-number (car data)) article)
	(setq gnus-newsgroup-data (cdr gnus-newsgroup-data)
	      gnus-newsgroup-data-reverse nil)
      (while (cdr data)
	(and (= (gnus-data-number (car (cdr data))) article)
	     (progn
	       (setcdr data (cdr (cdr data)))
	       (and offset (gnus-data-update-list (cdr data) offset))
	       (setq data nil
		     gnus-newsgroup-data-reverse nil)))
	(setq data (cdr data))))))

(defmacro gnus-data-list (backward)
  (` (if (, backward)
	 (or gnus-newsgroup-data-reverse
	     (setq gnus-newsgroup-data-reverse
		   (reverse gnus-newsgroup-data)))
       gnus-newsgroup-data)))

(defun gnus-data-update-list (data offset)
  "Add OFFSET to the POS of all data entries in DATA."
  (while data
    (setcar (nthcdr 2 (car data)) (+ offset (nth 2 (car data))))
    (setq data (cdr data))))

(defun gnus-data-compute-positions ()
  "Compute the positions of all articles."
  (let ((data gnus-newsgroup-data)
	pos)
    (while data
      (when (setq pos (text-property-any 
		       (point-min) (point-max)
		       'gnus-number (gnus-data-number (car data))))
	(gnus-data-set-pos (car data) (+ pos 3)))
      (setq data (cdr data)))))

(defun gnus-summary-article-pseudo-p (article)
  "Say whether this article is a pseudo article or not."
  (not (vectorp (gnus-data-header (gnus-data-find article)))))

(defun gnus-article-parent-p (number)
  "Say whether this article is a parent or not."
  (let* ((data (gnus-data-find-list number)))
    (and (cdr data)			; There has to be an article after...
	 (< (gnus-data-level (car data)) ; And it has to have a higher level.
	    (gnus-data-level (nth 1 data))))))
    
(defmacro gnus-summary-skip-intangible ()
  "If the current article is intangible, then jump to a different article."
  '(let ((to (get-text-property (point) 'gnus-intangible)))
    (when to
      (gnus-summary-goto-subject to))))

(defmacro gnus-summary-article-intangible-p ()
  "Say whether this article is intangible or not."
  '(get-text-property (point) 'gnus-intangible))

;; Some summary mode macros.

(defmacro gnus-summary-article-number ()
  "The article number of the article on the current line.
If there isn's an article number here, then we return the current
article number."
  '(progn
     (gnus-summary-skip-intangible)
     (or (get-text-property (point) 'gnus-number) 
	 (progn
	   (forward-line -1)
	   gnus-newsgroup-end))))

(defmacro gnus-summary-article-header (&optional number)
  (` (gnus-data-header (gnus-data-find
			(, (or number '(gnus-summary-article-number)))))))

(defmacro gnus-summary-thread-level (&optional number)
  (` (gnus-data-level (gnus-data-find
		       (, (or number '(gnus-summary-article-number)))))))

(defmacro gnus-summary-article-mark (&optional number)
  (` (gnus-data-mark (gnus-data-find
		      (, (or number '(gnus-summary-article-number)))))))

(defmacro gnus-summary-article-pos (&optional number)
  (` (gnus-data-pos (gnus-data-find
		     (, (or number '(gnus-summary-article-number)))))))

(defmacro gnus-summary-article-subject (&optional number)
  "Return current subject string or nil if nothing."
  (` (let ((headers 
	    (, (if number
		   (` (gnus-data-header (assq (, number) gnus-newsgroup-data)))
		 '(gnus-data-header (assq (gnus-summary-article-number)
					  gnus-newsgroup-data))))))
       (and headers
	    (vectorp headers)
	    (mail-header-subject headers)))))

(defmacro gnus-summary-article-score (&optional number)
  "Return current article score."
  (` (or (cdr (assq (, (or number '(gnus-summary-article-number)))
		    gnus-newsgroup-scored))
	 gnus-summary-default-score 0)))

(defun gnus-summary-article-children (&optional number)
  (let* ((data (gnus-data-find-list (or number (gnus-summary-article-number))))
	 (level (gnus-data-level (car data)))
	 l children)
    (while (and (setq data (cdr data))
		(> (setq l (gnus-data-level (car data))) level))
      (and (= (1+ level) l)
	   (setq children (cons (gnus-data-number (car data))
				children))))
    (nreverse children)))

(defun gnus-summary-article-parent (&optional number)
  (let* ((data (gnus-data-find-list (or number (gnus-summary-article-number))
				    (gnus-data-list t)))
	 (level (gnus-data-level (car data)))
	 l)
    (if (zerop level)
	() ; This is a root.
      ;; We search until we find an article with a level less than
      ;; this one.  That function has to be the parent.
      (while (and (setq data (cdr data))
		  (not (< (gnus-data-level (car data)) level))))
      (and data (gnus-data-number (car data))))))


;; Various summary mode internalish functions.

(defun gnus-mouse-pick-article (e)
  (interactive "e")
  (mouse-set-point e)
  (gnus-summary-next-page nil t))

(defun gnus-summary-setup-buffer (group)
  "Initialize summary buffer."
  (let ((buffer (concat "*Summary " group "*")))
    (if (get-buffer buffer)
	(progn
	  (set-buffer buffer)
	  (not gnus-newsgroup-prepared))
      ;; Fix by Sudish Joseph <joseph@cis.ohio-state.edu>
      (setq gnus-summary-buffer (set-buffer (get-buffer-create buffer)))
      (gnus-add-current-to-buffer-list)
      (gnus-summary-mode group)
      (and gnus-carpal (gnus-carpal-setup-buffer 'summary))
      (setq gnus-newsgroup-name group)
      t)))

(defun gnus-set-global-variables ()
  ;; Set the global equivalents of the summary buffer-local variables
  ;; to the latest values they had.  These reflect the summary buffer
  ;; that was in action when the last article was fetched.
  (if (eq major-mode 'gnus-summary-mode) 
      (progn
	(setq gnus-summary-buffer (current-buffer))
	(let ((name gnus-newsgroup-name)
	      (marked gnus-newsgroup-marked)
	      (unread gnus-newsgroup-unreads)
	      (headers gnus-current-headers)
	      (data gnus-newsgroup-data)
	      (score-file gnus-current-score-file))
	  (save-excursion
	    (set-buffer gnus-group-buffer)
	    (setq gnus-newsgroup-name name)
	    (setq gnus-newsgroup-marked marked)
	    (setq gnus-newsgroup-unreads unread)
	    (setq gnus-current-headers headers)
	    (setq gnus-newsgroup-data data)
	    (setq gnus-current-score-file score-file))))))

(defun gnus-summary-last-article-p (&optional article)
  "Return whether ARTICLE is the last article in the buffer."
  (if (not (setq article (or article (gnus-summary-article-number))))
      t ; All non-existant numbers are the last article. :-)
    (cdr (gnus-data-find-list article))))
    
(defun gnus-summary-insert-dummy-line 
  (sformat gnus-tmp-subject gnus-tmp-number)
  "Insert a dummy root in the summary buffer."
  (or sformat (setq sformat gnus-summary-dummy-line-format-spec))
  (beginning-of-line)
  (add-text-properties
   (point) (progn (eval sformat) (point))
   (list 'gnus-number gnus-tmp-number 'gnus-intangible gnus-tmp-number)))

(defvar gnus-thread-indent-array nil)
(defvar gnus-thread-indent-array-level gnus-thread-indent-level)
(defun gnus-make-thread-indent-array ()
  (let ((n 200))
    (if (and gnus-thread-indent-array
	     (= gnus-thread-indent-level gnus-thread-indent-array-level))
	nil
      (setq gnus-thread-indent-array (make-vector 201 "")
	    gnus-thread-indent-array-level gnus-thread-indent-level)
      (while (>= n 0)
	(aset gnus-thread-indent-array n
	      (make-string (* n gnus-thread-indent-level) ? ))
	(setq n (1- n))))))

(defun gnus-summary-insert-line 
  (sformat gnus-tmp-header gnus-tmp-level gnus-tmp-current gnus-tmp-unread 
	   gnus-tmp-replied gnus-tmp-expirable gnus-tmp-subject-or-nil
	   &optional gnus-tmp-dummy gnus-tmp-score gnus-tmp-process)
  (or sformat (setq sformat gnus-summary-line-format-spec))
  (let* ((gnus-tmp-indentation (aref gnus-thread-indent-array gnus-tmp-level))
	 (gnus-tmp-lines (mail-header-lines gnus-tmp-header))
	 (gnus-tmp-score (or gnus-tmp-score gnus-summary-default-score 0))
	 (gnus-tmp-score-char
	  (if (or (null gnus-summary-default-score)
		  (<= (abs (- gnus-tmp-score gnus-summary-default-score))
		      gnus-summary-zcore-fuzz)) ? 
	    (if (< gnus-tmp-score gnus-summary-default-score)
		gnus-score-below-mark gnus-score-over-mark)))
	 (gnus-tmp-replied (cond (gnus-tmp-process gnus-process-mark)
				 (gnus-tmp-replied gnus-replied-mark)
				 (t gnus-unread-mark)))
	 (gnus-tmp-from (mail-header-from gnus-tmp-header))
	 (gnus-tmp-name 
	  (cond 
	   ((string-match "(.+)" gnus-tmp-from)
	    (substring gnus-tmp-from 
		       (1+ (match-beginning 0)) (1- (match-end 0))))
	   ((string-match "<[^>]+> *$" gnus-tmp-from)
	    (let ((beg (match-beginning 0)))
	      (or (and (string-match "^\"[^\"]*\"" gnus-tmp-from)
		       (substring gnus-tmp-from (1+ (match-beginning 0))
				  (1- (match-end 0))))
		  (substring gnus-tmp-from 0 beg))))
	   (t gnus-tmp-from)))
	 (gnus-tmp-subject (mail-header-subject gnus-tmp-header))
	 (gnus-tmp-number (mail-header-number gnus-tmp-header))
	 (gnus-tmp-opening-bracket (if gnus-tmp-dummy ?\< ?\[))
	 (gnus-tmp-closing-bracket (if gnus-tmp-dummy ?\> ?\]))
	 (buffer-read-only nil))
    (or (numberp gnus-tmp-lines) (setq gnus-tmp-lines 0))
    (put-text-property
     (point)
     (progn (eval sformat) (point))
     'gnus-number gnus-tmp-number)))

(defun gnus-summary-update-line (&optional dont-update)
  ;; Update summary line after change.
  (when (and gnus-summary-default-score
	     (not gnus-summary-inhibit-highlight))
    (let ((gnus-summary-inhibit-highlight t) ; Prevent recursion.
	  (article (gnus-summary-article-number)))
      (unless dont-update
	(if (and gnus-summary-mark-below
		 (< (gnus-summary-article-score)
		    gnus-summary-mark-below))
	    ;; This article has a low score, so we mark it as read.
	    (when (memq article gnus-newsgroup-unreads)
	      (gnus-summary-mark-article-as-read gnus-low-score-mark))
	  (when (eq (gnus-summary-article-mark) gnus-low-score-mark)
	    ;; This article was previously marked as read on account
	    ;; of a low score, but now it has risen, so we mark it as
	    ;; unread. 
	    (gnus-summary-mark-article-as-unread gnus-unread-mark))))
      ;; Do visual highlighting.
      (when (gnus-visual-p 'summary-highlight 'highlight)
	(run-hooks 'gnus-summary-update-hook)))))

(defun gnus-summary-update-lines (&optional beg end)
  "Mark article as read (or not) by taking into account scores."
  (when (and gnus-summary-default-score
	     (not gnus-summary-inhibit-highlight))
    (let ((beg (or beg (point-min)))
	  (end (or end (point-max)))
	  (gnus-summary-inhibit-highlight t)
	  (visual (gnus-visual-p 'summary-highlight 'highlight))
	  article)
      (save-excursion
	(set-buffer gnus-summary-buffer)
	(goto-char beg)
	(beginning-of-line)
	(while (and (not (eobp)) 
		    (< (point) end))
	  (if (and gnus-summary-mark-below
		   (< (or (cdr (assq 
				(setq article (gnus-summary-article-number))
				gnus-newsgroup-scored))
			  gnus-summary-default-score 0)
		      gnus-summary-mark-below))
	      ;; We want to possibly mark it as read...
	      (when (memq article gnus-newsgroup-unreads)
		(gnus-summary-mark-article-as-read gnus-low-score-mark))
	    ;; We want to possibly mark it as unread.
	    (when (eq (gnus-summary-article-mark article) gnus-low-score-mark)
	      (gnus-summary-mark-article-as-unread gnus-unread-mark)))
	  ;; Do the visual highlights at the same time.
	  (when visual
	    (run-hooks 'gnus-summary-update-hook))
	  (forward-line 1))))))

(defvar gnus-tmp-new-adopts)

(defun gnus-summary-number-of-articles-in-thread (thread &optional level char)
  ;; Sum up all elements (and sub-elements) in a list.
  (let* ((number
	  ;; Fix by Luc Van Eycken <Luc.VanEycken@esat.kuleuven.ac.be>.
	  (cond ((and (consp thread) (cdr thread))
		 (apply
		  '+ 1 (mapcar
			'gnus-summary-number-of-articles-in-thread 
			(cdr thread))))
		((null thread)
		 1)
		((and level (zerop level) gnus-tmp-new-adopts)
		 (apply '+ 1 (mapcar 
			      'gnus-summary-number-of-articles-in-thread 
			      gnus-tmp-new-adopts)))
		((memq (mail-header-number (car thread))
		       gnus-newsgroup-limit)
		 1) 
		(t 0))))
    (if char 
	(if (> number 1) gnus-not-empty-thread-mark
	  gnus-empty-thread-mark)
      number)))

(defun gnus-summary-set-local-parameters (group)
 "Go through the local params of GROUP and set all variable specs in that list."
  (let ((params (gnus-info-params (gnus-get-info group)))
	elem)
    (while params
      (setq elem (car params)
	    params (cdr params))
      (and (consp elem)			; Has to be a cons.
	   (consp (cdr elem))		; The cdr has to be a list.
	   (symbolp (car elem))		; Has to be a symbol in there.
	   (progn			; So we set it.
	     (make-local-variable (car elem))
	     (set (car elem) (eval (nth 1 elem))))))))

(defun gnus-summary-read-group 
  (group &optional show-all no-article kill-buffer no-display)
  "Start reading news in newsgroup GROUP.
If SHOW-ALL is non-nil, already read articles are also listed.
If NO-ARTICLE is non-nil, no article is selected initially.
If NO-DISPLAY, don't generate a summary buffer."
  (gnus-message 5 "Retrieving newsgroup: %s..." group)
  (let* ((new-group (gnus-summary-setup-buffer group))
	 (quit-config (gnus-group-quit-config group))
	 (did-select (and new-group (gnus-select-newsgroup group show-all))))
    (cond 
     ;; This summary buffer exists already, so we just select it. 
     ((not new-group)
      (gnus-set-global-variables)
      (gnus-kill-buffer kill-buffer)
      (gnus-configure-windows 'summary 'force)
      (gnus-set-mode-line 'summary)
      (gnus-summary-position-point)
      (message "")
      t)
     ;; We couldn't select this group.
     ((null did-select) 
      (when (and (eq major-mode 'gnus-summary-mode)
		 (not (equal (current-buffer) kill-buffer)))
	(kill-buffer (current-buffer))
	(if (not quit-config)
	    (progn
	      (set-buffer gnus-group-buffer)
	      (gnus-group-jump-to-group group)
	      (gnus-group-next-unread-group 1))
	  (if (not (buffer-name (car quit-config)))
	      (gnus-configure-windows 'group 'force)
	    (set-buffer (car quit-config))
	    (and (eq major-mode 'gnus-summary-mode)
		 (gnus-set-global-variables))
	    (gnus-configure-windows (cdr quit-config)))))
      (message "Can't select group")
      nil)
     ;; The user did a `C-g' while prompting for number of articles,
     ;; so we exit this group.
     ((eq did-select 'quit)
      (and (eq major-mode 'gnus-summary-mode)
	   (not (equal (current-buffer) kill-buffer))
	   (kill-buffer (current-buffer)))
      (gnus-kill-buffer kill-buffer)
      (if (not quit-config)
	  (progn
	    (set-buffer gnus-group-buffer)
	    (gnus-group-jump-to-group group)
	    (gnus-group-next-unread-group 1)
	    (gnus-configure-windows 'group 'force))
	(if (not (buffer-name (car quit-config)))
	    (gnus-configure-windows 'group 'force)
	  (set-buffer (car quit-config))
	  (and (eq major-mode 'gnus-summary-mode)
	       (gnus-set-global-variables))
	  (gnus-configure-windows (cdr quit-config))))
      ;; Finallt signal the quit.
      (signal 'quit nil))
     ;; The group was successfully selected.
     (t
      (gnus-set-global-variables)
      ;; Save the active value in effect when the group was entered.
      (setq gnus-newsgroup-active 
	    (gnus-copy-sequence
	     (gnus-active gnus-newsgroup-name)))
      ;; You can change the summary buffer in some way with this hook.
      (run-hooks 'gnus-select-group-hook)
      ;; Set any local variables in the group parameters.
      (gnus-summary-set-local-parameters gnus-newsgroup-name)
      ;; Do score processing.
      (when gnus-use-scoring
	(gnus-possibly-score-headers))
      (gnus-update-format-specifications)
      ;; Find the initial limit.
      (gnus-summary-initial-limit)
      ;; Generate the summary buffer.
      (unless no-display
	(gnus-summary-prepare))
      ;; If the summary buffer is empty, but there are some low-scored
      ;; articles or some excluded dormants, we include these in the
      ;; buffer. 
      (when (zerop (buffer-size))
	(cond (gnus-newsgroup-dormant
	       (gnus-summary-limit-include-dormant))
	      ((and gnus-newsgroup-scored show-all)
	       (gnus-summary-limit-include-expunged))))
      ;; Function `gnus-apply-kill-file' must be called in this hook.
      (run-hooks 'gnus-apply-kill-hook)
      (if (zerop (buffer-size))
	  (progn
	    ;; This newsgroup is empty.
	    (gnus-summary-catchup-and-exit nil t) ;Without confirmations.
	    (gnus-message 6 "No unread news")
	    (gnus-kill-buffer kill-buffer)
	    ;; Return nil from this function.
	    nil)
	;; Hide conversation thread subtrees.  We cannot do this in
	;; gnus-summary-prepare-hook since kill processing may not
	;; work with hidden articles.
	(and gnus-show-threads
	     gnus-thread-hide-subtree
	     (gnus-summary-hide-all-threads))
	;; Show first unread article if requested.
	(if (and (not no-article)
		 gnus-auto-select-first)
	    (progn
	      (if (eq gnus-auto-select-first 'best)
		  (gnus-summary-best-unread-article)
		(gnus-summary-first-unread-article)))
	  ;; Don't select any articles, just move point to the first
	  ;; article in the group.
	  (goto-char (point-min))
	  (gnus-summary-position-point)
	  (gnus-set-mode-line 'summary)
	  (gnus-configure-windows 'summary 'force))
	;; If we are in async mode, we send some info to the backend.
	(when gnus-newsgroup-async
	  (gnus-request-asynchronous gnus-newsgroup-name gnus-newsgroup-data))
	(gnus-kill-buffer kill-buffer)
	(when (get-buffer-window gnus-group-buffer)
	  ;; Gotta use windows, because recenter does wierd stuff if
	  ;; the current buffer ain't the displayed window.
 	  (let ((owin (selected-window))) 
 	    (select-window (get-buffer-window gnus-group-buffer))
  	    (when (gnus-group-goto-group group)
	      (recenter))
 	    (select-window owin))))
      ;; Mark this buffer as "prepared".
      (setq gnus-newsgroup-prepared t)
      t))))

(defun gnus-summary-prepare ()
  "Generate the summary buffer."
  (let ((buffer-read-only nil))
    (erase-buffer)
    (setq gnus-newsgroup-data nil
	  gnus-newsgroup-data-reverse nil)
    (run-hooks 'gnus-summary-generate-hook)
    ;; Generate the buffer, either with threads or without.
    (gnus-summary-prepare-threads 
     (if gnus-show-threads
	 (gnus-gather-threads (gnus-sort-threads (gnus-make-threads)))
       gnus-newsgroup-headers))
    ;; Do score marking and highlights.
    (gnus-summary-update-lines)
    (setq gnus-newsgroup-data (nreverse gnus-newsgroup-data))
    ;; Call hooks for modifying summary buffer.
    (goto-char (point-min))
    (run-hooks 'gnus-summary-prepare-hook)))

(defun gnus-gather-threads (threads)
  "Gather threads that have lost their roots."
  (if (not gnus-summary-make-false-root)
      threads 
    (let ((hashtb (gnus-make-hashtable 1023))
	  (prev threads)
	  (result threads)
	  subject hthread whole-subject)
      (while threads
	(setq whole-subject 
	      (setq subject (mail-header-subject (car (car threads)))))
	(if (and gnus-summary-gather-exclude-subject
		 (string-match gnus-summary-gather-exclude-subject
			       subject))
	    () ; We don't want to do anything with this.
	  (if gnus-summary-gather-subject-limit
	      (or (and (numberp gnus-summary-gather-subject-limit)
		       (> (length subject) gnus-summary-gather-subject-limit)
		       (setq subject
			     (substring subject 0 
					gnus-summary-gather-subject-limit)))
		  (and (eq 'fuzzy gnus-summary-gather-subject-limit)
		       (setq subject (gnus-simplify-subject-fuzzy subject))))
	    (setq subject (gnus-simplify-subject-re subject)))
	  (if (setq hthread 
		    (gnus-gethash subject hashtb))
	      (progn
		(or (stringp (car (car hthread)))
		    (setcar hthread (list whole-subject (car hthread))))
		(setcdr (car hthread) (nconc (cdr (car hthread)) 
					     (list (car threads))))
		(setcdr prev (cdr threads))
		(setq threads prev))
	    (gnus-sethash subject threads hashtb)))
	(setq prev threads)
	(setq threads (cdr threads)))
      result)))

(defun gnus-make-threads ()
  "Go through the dependency hashtb and find the roots.  Return all threads."
  ;; Then we find all the roots and return all the threads.
  (let (threads)
    (mapatoms
     (lambda (refs)
       (or (car (symbol-value refs))
	   (setq threads (append (cdr (symbol-value refs)) threads))))
     gnus-newsgroup-dependencies)
    threads))
  
(defun gnus-build-old-threads ()
  ;; Look at all the articles that refer back to old articles, and
  ;; fetch the headers for the articles that aren't there.  This will
  ;; build complete threads - if the roots haven't been expired by the
  ;; server, that is.
  (let (id heads)
    (mapatoms
     (lambda (refs)
       (if (not (car (symbol-value refs)))
	   (progn
	     (setq heads (cdr (symbol-value refs)))
	     (while heads
	       (if (not (memq (mail-header-number (car (car heads)))
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
  ;; ID.  Enter this line into the dependencies hash table, and return
  ;; the id of the parent article (if any).
  (let ((deps gnus-newsgroup-dependencies)
	found header)
    (prog1
	(save-excursion
	  (set-buffer nntp-server-buffer)
	  (goto-char (point-min))
	  (while (and (not found) (search-forward id nil t))
	    (beginning-of-line)
	    (setq found (looking-at 
			 (format "^[^\t]*\t[^\t]*\t[^\t]*\t[^\t]*\t%s"
				 (regexp-quote id))))
	    (or found (beginning-of-line 2)))
	  (when found
	    (let (ref)
	      (beginning-of-line)
	      (and
	       (setq header (gnus-nov-parse-line 
			     (read (current-buffer)) deps))
	       (gnus-parent-id (mail-header-references header))))))
      (when header
	(setq gnus-newsgroup-headers (cons header gnus-newsgroup-headers)
	      gnus-newsgroup-ancient (cons (mail-header-number header)
					   gnus-newsgroup-ancient))))))

(defun gnus-rebuild-thread (id)
  "Rebuild the thread containing ID."
  (let ((dep gnus-newsgroup-dependencies)
	(buffer-read-only nil)
	current headers refs thread art data)
    (if (not gnus-show-threads)
	(setq thread (list (car (gnus-gethash (downcase id) dep))))
      ;; Get the thread this article is part of.
      (setq thread (gnus-remove-thread id)))
    (setq current (save-excursion
		    (and (zerop (forward-line -1))
			 (gnus-summary-article-number))))
    ;; If this is a gathered thread, we have to go some re-gathering.
    (when (stringp (car thread))
      (let ((subject (car thread))
	    roots thr)
	(setq thread (cdr thread))
	(while thread
	  (unless (memq (setq thr (gnus-id-to-thread 
				      (gnus-root-id
				       (mail-header-id (car (car thread))))))
			roots)
	    (push thr roots))
	  (setq thread (cdr thread)))
	;; We now have all (unique) roots.
	(if (= (length roots) 1)
	    ;; All the loose roots are now one solid root.
	    (setq thread (car roots))
	  (setq thread (cons subject (gnus-sort-threads roots))))))
    (let ((beg (point)) 
	  threads)
      ;; We then insert this thread into the summary buffer.
      (let (gnus-newsgroup-data gnus-newsgroup-threads)
	(gnus-summary-prepare-threads (list thread))
	(setq data (nreverse gnus-newsgroup-data))
	(setq threads gnus-newsgroup-threads))
      ;; We splice the new data into the data structure.
      (gnus-data-enter-list current data)
      (gnus-data-compute-positions)
      (setq gnus-newsgroup-threads (nconc threads gnus-newsgroup-threads))
      ;; Do highlighting and stuff.
      (gnus-summary-update-lines beg (point)))))

(defun gnus-id-to-thread (id)
  "Return the (sub-)thread where ID appears."
  (gnus-gethash (downcase id) gnus-newsgroup-dependencies))

(defun gnus-root-id (id)
  "Return the id of the root of the thread where ID appears."
  (let (last-id prev)
    (while (and id (setq prev (car (gnus-gethash 
				    (downcase id)
				    gnus-newsgroup-dependencies))))
      (setq last-id id
	    id (gnus-parent-id (mail-header-references prev))))
    last-id))

(defun gnus-remove-thread (id)
  "Remove the thread that has ID in it."
  (let ((dep gnus-newsgroup-dependencies)
	headers thread prev last-id)
    ;; First go up in this thread until we find the root.
    (setq last-id (gnus-root-id id))
    (setq headers (list (car (gnus-id-to-thread last-id))
			(car (car (cdr (gnus-id-to-thread last-id))))))
    ;; We have now found the real root of this thread.  It might have
    ;; been gathered into some loose thread, so we have to search
    ;; through the threads to find the thread we wanted.
    (let ((threads gnus-newsgroup-threads)
	  sub)
      (while threads
	(setq sub (car threads))
	(if (stringp (car sub))
	    ;; This is a gathered threads, so we look at the roots
	    ;; below it to find whether this article in in this
	    ;; gathered root.
	    (progn
	      (setq sub (cdr sub))
	      (while sub
		(when (member (car (car sub)) headers)
		  (setq thread (car threads)
			threads nil
			sub nil))
		(setq sub (cdr sub))))
	  ;; It's an ordinary thread, so we check it.
	  (when (eq (car sub) (car headers))
	    (setq thread sub
		  threads nil)))
	(setq threads (cdr threads)))
      ;; If this article is in no thread, then it's a root. 
      (if thread 
	  (setq gnus-newsgroup-threads (delq thread gnus-newsgroup-threads))
	(setq thread (gnus-gethash (downcase last-id) dep)))
      (when thread
	(prog1 
	    thread ; We return this thread.
	  (if (stringp (car thread))
	      (progn
		;; If we use dummy roots, then we have to remove the
		;; dummy root as well.
		(when (eq gnus-summary-make-false-root 'dummy)
		  ;; Uhm.
		  )
		(setq thread (cdr thread))
		(while thread
		  (gnus-remove-thread-1 (car thread))
		  (setq thread (cdr thread))))
	    (gnus-remove-thread-1 thread)))))))

(defun gnus-remove-thread-1 (thread)
  "Remove the thread THREAD recursively."
  (let ((number (mail-header-number (car thread)))
	pos)
    (when (setq pos (text-property-any 
		     (point-min) (point-max) 'gnus-number number))
      (goto-char pos)
      (gnus-delete-line)
      (gnus-data-remove number))
    (setq thread (cdr thread))
    (while thread
      (gnus-remove-thread-1 (car thread))
      (setq thread (cdr thread)))))

(defun gnus-sort-threads (threads)
  "Sort THREADS as specified in `gnus-thread-sort-functions'."
  (let ((funs gnus-thread-sort-functions))
    (when funs
      (while funs
	(gnus-message 7 "Sorting with %S..." (car funs))
	(setq threads (sort threads (pop funs))))
      (gnus-message 7 "Sorting...done")))
  threads)

;; Written by Hallvard B Furuseth <h.b.furuseth@usit.uio.no>.
(defmacro gnus-thread-header (thread)
  ;; Return header of first article in THREAD.
  ;; Note that THREAD must never, evr be anything else than a variable -
  ;; using some other form will lead to serious barfage.
  (or (symbolp thread) (signal 'wrong-type-argument '(symbolp thread)))
  ;; (8% speedup to gnus-summary-prepare, just for fun :-)
  (list 'byte-code "\10\211:\203\17\0\211@;\203\16\0A@@\207" ; 
	(vector thread) 2))

(defun gnus-thread-sort-by-number (h1 h2)
  "Sort threads by root article number."
  (< (mail-header-number (gnus-thread-header h1))
     (mail-header-number (gnus-thread-header h2))))

(defun gnus-thread-sort-by-author (h1 h2)
  "Sort threads by root author."
  (string-lessp
   (let ((extract (funcall 
		   gnus-extract-address-components
		   (mail-header-from (gnus-thread-header h1)))))
     (or (car extract) (cdr extract)))
   (let ((extract (funcall
		   gnus-extract-address-components 
		   (mail-header-from (gnus-thread-header h2)))))
     (or (car extract) (cdr extract)))))

(defun gnus-thread-sort-by-subject (h1 h2)
  "Sort threads by root subject."
  (string-lessp
   (downcase (gnus-simplify-subject-re
	      (mail-header-subject (gnus-thread-header h1))))
   (downcase (gnus-simplify-subject-re 
	      (mail-header-subject (gnus-thread-header h2))))))

(defun gnus-thread-sort-by-date (h1 h2)
  "Sort threads by root article date."
  (string-lessp
   (gnus-sortable-date (mail-header-date (gnus-thread-header h1)))
   (gnus-sortable-date (mail-header-date (gnus-thread-header h2)))))

(defun gnus-thread-sort-by-score (h1 h2)
  "Sort threads by root article score.
Unscored articles will be counted as having a score of zero."
  (> (or (cdr (assq (mail-header-number (gnus-thread-header h1))
		    gnus-newsgroup-scored))
	 gnus-summary-default-score 0)
     (or (cdr (assq (mail-header-number (gnus-thread-header h2))
		    gnus-newsgroup-scored))
	 gnus-summary-default-score 0)))

(defun gnus-thread-sort-by-total-score (h1 h2)
  "Sort threads by the sum of all scores in the thread.
Unscored articles will be counted as having a score of zero."
  (> (gnus-thread-total-score h1) (gnus-thread-total-score h2)))

(defun gnus-thread-total-score (thread)
  ;;  This function find the total score of THREAD.
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
	 (or (cdr (assq (mail-header-number root) gnus-newsgroup-scored))
	     gnus-summary-default-score 0)
	 (mapcar 'gnus-thread-total-score
		 (cdr (gnus-gethash (downcase (mail-header-id root))
				    gnus-newsgroup-dependencies)))))

;; Added by Per Abrahamsen <amanda@iesd.auc.dk>.
(defvar gnus-tmp-prev-subject nil)
(defvar gnus-tmp-false-parent nil)
(defvar gnus-tmp-root-expunged nil)
(defvar gnus-tmp-dummy-line nil)

(defun gnus-summary-prepare-threads (threads)
  "Prepare summary buffer from THREADS and indentation LEVEL.  
THREADS is either a list of `(PARENT [(CHILD1 [(GRANDCHILD ...]...) ...])'  
or a straight list of headers."
  (message "Generating summary...")

  (setq gnus-newsgroup-threads threads)
  (beginning-of-line)

  (let ((level 0)
	thread header number subject stack state gnus-tmp-gathered mark
	new-roots gnus-tmp-new-adopts thread-end)

    (setq gnus-tmp-prev-subject nil)

    (if (vectorp (car threads))
	;; If this is a straight (sic) list of headers, then a
	;; threaded summary display isn't required, so we just create
	;; an unthreaded one.
	(gnus-summary-prepare-unthreaded threads)

      ;; Do the threaded display.

      (while (or threads stack gnus-tmp-new-adopts new-roots)

	(if (and (= level 0)
		 (progn (setq gnus-tmp-dummy-line nil) t)
		 (or (not stack)
		     (= (car (car stack)) 0))
		 (not gnus-tmp-false-parent)
		 (or gnus-tmp-new-adopts new-roots))
	    (progn
	      (if gnus-tmp-new-adopts
		  (setq level (if gnus-tmp-root-expunged 0 1)
			thread (list (car gnus-tmp-new-adopts))
			header (car (car thread))
			gnus-tmp-new-adopts (cdr gnus-tmp-new-adopts))
		(if new-roots
		    (setq thread (list (car new-roots))
			  header (car (car thread))
			  new-roots (cdr new-roots)))))

	  (if threads
	      ;; If there are some threads, we do them before the
	      ;; threads on the stack.
	      (setq thread threads
		    header (car (car thread)))
	    ;; There were no current threads, so we pop something off
	    ;; the stack. 
	    (setq state (car stack)
		  level (car state)
		  thread (cdr state)
		  stack (cdr stack)
		  header (car (car thread)))))

	(setq gnus-tmp-false-parent nil)
	(setq gnus-tmp-root-expunged nil)
	(setq thread-end nil)

	(if (stringp header)
	    (progn
	      ;; The header is a dummy root.
	      (cond 
	       ((eq gnus-summary-make-false-root 'adopt)
		;; We let the first article adopt the rest.
		(setq gnus-tmp-new-adopts (nconc gnus-tmp-new-adopts
						 (cdr (cdr (car thread)))))
		(setq gnus-tmp-gathered 
		      (nconc (mapcar
			      (lambda (h) (mail-header-number (car h)))
			      (cdr (cdr (car thread))))
			     gnus-tmp-gathered))
		(setq thread (cons (list (car (car thread))
					 (car (cdr (car thread))))
				   (cdr thread)))
		(setq level -1
		      gnus-tmp-false-parent t))
	       ((eq gnus-summary-make-false-root 'empty)
		;; We print adopted articles with empty subject fields.
		(setq gnus-tmp-gathered 
		      (nconc (mapcar
			      (lambda (h) (mail-header-number (car h)))
			      (cdr (cdr (car thread))))
			     gnus-tmp-gathered))
		(setq level -1))
	       ((eq gnus-summary-make-false-root 'dummy)
		;; We remember that we probably want to output a dummy
		;; root.   
		(setq gnus-tmp-dummy-line header)
		(setq gnus-tmp-prev-subject header))
	       (t
		;; We do not make a root for the gathered
		;; sub-threads at all.  
		(setq level -1))))
      
	  (setq number (mail-header-number header)
		subject (mail-header-subject header))

	  (cond 
	   ((and (null gnus-thread-ignore-subject)
		 (not (zerop level))
		 gnus-tmp-prev-subject
		 (not (gnus-subject-equal gnus-tmp-prev-subject subject)))
	    (setq new-roots (nconc new-roots (list (car thread)))
		  thread-end t
		  header nil))
	   ((not (memq number gnus-newsgroup-limit))
	    (setq gnus-tmp-gathered 
		  (nconc (mapcar
			  (lambda (h) (mail-header-number (car h)))
			  (cdr (car thread)))
			 gnus-tmp-gathered))
	    (setq gnus-tmp-new-adopts (if (cdr (car thread))
					  (append gnus-tmp-new-adopts 
						  (cdr (car thread)))
					gnus-tmp-new-adopts)
		  thread-end t
		  header nil)
	    (if (zerop level)
		(setq gnus-tmp-root-expunged t)))
	   ((and gnus-summary-mark-below
		 (< (or (cdr (assq number gnus-newsgroup-scored))
			gnus-summary-default-score 0)
		    gnus-summary-mark-below))
	    (setq gnus-newsgroup-unreads 
		  (delq number gnus-newsgroup-unreads)
		  gnus-newsgroup-reads
		  (cons (cons number gnus-low-score-mark)
			gnus-newsgroup-reads))))
	  
	  (and
	   header
	   (progn
	     ;; We may have an old dummy line to output before this
	     ;; article.  
	     (when gnus-tmp-dummy-line
	       (gnus-summary-insert-dummy-line 
		nil gnus-tmp-dummy-line (gnus-header-number header)))

	     ;; Compute the mark.
	     (setq 
	      mark
	      (cond 
	       ((memq number gnus-newsgroup-marked) gnus-ticked-mark)
	       ((memq number gnus-newsgroup-dormant) gnus-dormant-mark)
	       ((memq number gnus-newsgroup-unreads) gnus-unread-mark)
	       ((memq number gnus-newsgroup-expirable) gnus-expirable-mark)
	       (t (or (cdr (assq number gnus-newsgroup-reads))
		      gnus-ancient-mark))))

	     ;; Actually insert the line.
	     (inline
	       (gnus-summary-insert-line
		nil header level nil mark
		(memq number gnus-newsgroup-replied)
		(memq number gnus-newsgroup-expirable)
		(cond
		 ((and gnus-thread-ignore-subject
		       gnus-tmp-prev-subject
		       (not (gnus-subject-equal 
			     gnus-tmp-prev-subject subject)))
		  subject)
		 ((zerop level)
		  (if (and (eq gnus-summary-make-false-root 'empty)
			   (memq number gnus-tmp-gathered))
		      gnus-summary-same-subject
		    subject))
		 (t gnus-summary-same-subject))
		(and (eq gnus-summary-make-false-root 'adopt)
		     (= level 1)
		     (memq number gnus-tmp-gathered))
		(cdr (assq number gnus-newsgroup-scored))
		(memq number gnus-newsgroup-processable)))

	     (setq gnus-newsgroup-data 
		   (cons (gnus-data-make number mark (- (point) 4)
					 header level)
			 gnus-newsgroup-data))

	     (setq gnus-tmp-prev-subject subject))))

	(if (nth 1 thread) 
	    (setq stack (cons (cons (max 0 level) (nthcdr 1 thread)) stack)))
	(setq level (1+ level))
	(setq threads (if thread-end nil (cdr (car thread))))
	(or threads (setq level 0)))))
  (message "Generating summary...done"))

(defun gnus-summary-prepare-unthreaded (headers)
  "Generate an unthreaded summary buffer based on HEADERS."
  (let (header number mark)

    (while headers
      (setq header (car headers)
	    headers (cdr headers)
	    number (mail-header-number header))

      ;; We may have to root out some bad articles...
      (when (memq number gnus-newsgroup-limit)
	(setq mark
	      (cond 
	       ((memq number gnus-newsgroup-marked) gnus-ticked-mark)
	       ((memq number gnus-newsgroup-dormant) gnus-dormant-mark)
	       ((memq number gnus-newsgroup-unreads) gnus-unread-mark)
	       ((memq number gnus-newsgroup-expirable) gnus-expirable-mark)
	       (t (or (cdr (assq number gnus-newsgroup-reads))
		      gnus-ancient-mark))))
	(setq gnus-newsgroup-data 
	      (cons (gnus-data-make number mark (1+ (point)) header 0)
		    gnus-newsgroup-data))
	(gnus-summary-insert-line
	 nil header 0 nil mark (memq number gnus-newsgroup-replied)
	 (memq number gnus-newsgroup-expirable)
	 (mail-header-subject header) nil
	 (cdr (assq number gnus-newsgroup-scored))
	 (memq number gnus-newsgroup-processable))))))

(defun gnus-select-newsgroup (group &optional read-all)
  "Select newsgroup GROUP.
If READ-ALL is non-nil, all articles in the group are selected."
  (let* ((entry (gnus-gethash group gnus-newsrc-hashtb))
	 (info (nth 2 entry))
	 articles)

    (or (gnus-check-server
	 (setq gnus-current-select-method (gnus-find-method-for-group group)))
	(error "Couldn't open server"))
    
    (or (and entry (not (eq (car entry) t))) ; Either it's active...
	(gnus-activate-group group) ; Or we can activate it...
	(progn ; Or we bug out.
	  (kill-buffer (current-buffer))
	  (error "Couldn't request group %s: %s" 
		 group (gnus-status-message group))))

    (setq gnus-newsgroup-name group)
    (setq gnus-newsgroup-unselected nil)
    (setq gnus-newsgroup-unreads (gnus-list-of-unread-articles group))

    (and gnus-asynchronous
	 (gnus-check-backend-function 
	  'request-asynchronous gnus-newsgroup-name)
	 (setq gnus-newsgroup-async
	       (gnus-request-asynchronous gnus-newsgroup-name)))

    ;; Adjust and set lists of article marks.
    (when info
      (gnus-adjust-marked-articles info)
      (let ((marked (gnus-info-marks info)))
	(mapcar
	 (lambda (thing)
	   (let ((name (intern (format "gnus-newsgroup-%s" (car thing)))))
	     (set name (copy-sequence (cdr (assq (cdr thing) marked))))))
	 '((marked . tick) (replied . reply) 
	   (expirable . expire) (killed . killed)
	   (bookmarks . bookmark) (dormant . dormant)
	   (scored . score)))))

    (setq gnus-newsgroup-unreads 
	  (gnus-set-difference
	   (gnus-set-difference gnus-newsgroup-unreads gnus-newsgroup-marked)
	   gnus-newsgroup-dormant))

    (setq gnus-newsgroup-processable nil)
    
    (setq articles (gnus-articles-to-read group read-all))
    
    (cond 
     ((null articles) 
      (gnus-message 3 "Couldn't select newsgroup")
      'quit)
     ((eq articles 0) nil)
     (t
      ;; Init the dependencies hash table.
      (setq gnus-newsgroup-dependencies 
	    (gnus-make-hashtable (length articles)))
      ;; Retrieve the headers and read them in.
      (gnus-message 5 "Fetching headers...")
      (setq gnus-newsgroup-headers 
	    (if (eq 'nov 
		    (setq gnus-headers-retrieved-by
			  (gnus-retrieve-headers 
			   articles gnus-newsgroup-name
			   ;; We might want to fetch old headers, but
			   ;; not if there is only 1 article.
			   (and gnus-fetch-old-headers
				(or (and 
				     (not (eq gnus-fetch-old-headers 'some))
				     (not (numberp gnus-fetch-old-headers)))
				    (> (length articles) 1))))))
		(gnus-get-newsgroup-headers-xover articles)
	      (gnus-get-newsgroup-headers)))
      (gnus-message 5 "Fetching headers...done")      
      ;; Set the initial limit.
      (setq gnus-newsgroup-limit (copy-sequence articles))
      ;; Remove canceled articles from the list of unread articles.
      (setq gnus-newsgroup-unreads
	    (gnus-set-sorted-intersection 
	     gnus-newsgroup-unreads
	     (mapcar (lambda (headers) (mail-header-number headers))
		     gnus-newsgroup-headers)))
      ;; We might want to build some more threads first.
      (and gnus-fetch-old-headers
	   (eq gnus-headers-retrieved-by 'nov)
	   (gnus-build-old-threads))
      ;; Check whether auto-expire is to be done in this group.
      (setq gnus-newsgroup-auto-expire
	    (gnus-group-auto-expirable-p group))
      ;; First and last article in this newsgroup.
      (and gnus-newsgroup-headers
	   (setq gnus-newsgroup-begin 
		 (mail-header-number (car gnus-newsgroup-headers)))
	   (setq gnus-newsgroup-end
		 (mail-header-number
		  (gnus-last-element gnus-newsgroup-headers))))
      (setq gnus-reffed-article-number -1)
      ;; GROUP is successfully selected.
      (or gnus-newsgroup-headers t)))))

(defun gnus-articles-to-read (group read-all)
  ;; Find out what articles the user wants to read.
  (let* ((articles
	  ;; Select all articles if `read-all' is non-nil, or if there
	  ;; are no unread articles.
	  (if (or read-all
		  (and (zerop (length gnus-newsgroup-marked))
		       (zerop (length gnus-newsgroup-unreads))))
	      (gnus-uncompress-range (gnus-active group))
	    (sort (append gnus-newsgroup-dormant gnus-newsgroup-marked 
			  (copy-sequence gnus-newsgroup-unreads))
		  '<)))
	 (scored-list (gnus-killed-articles gnus-newsgroup-killed articles))
	 (scored (length scored-list))
	 (number (length articles))
	 (marked (+ (length gnus-newsgroup-marked)
		    (length gnus-newsgroup-dormant)))
	 (select
	  (cond 
	   ((numberp read-all)
	    read-all)
	   (t
	    (condition-case ()
		(cond 
		 ((and (or (<= scored marked) (= scored number))
		       (numberp gnus-large-newsgroup)
		       (> number gnus-large-newsgroup))
		  (let ((input
			 (read-string
			  (format
			   "How many articles from %s (default %d): "
			   gnus-newsgroup-name number))))
		    (if (string-match "^[ \t]*$" input) number input)))
		 ((and (> scored marked) (< scored number))
		  (let ((input
			 (read-string
			  (format "%s %s (%d scored, %d total): "
				  "How many articles from"
				  group scored number))))
		    (if (string-match "^[ \t]*$" input)
			number input)))
		 (t number))
	      (quit nil))))))
    (setq select (if (stringp select) (string-to-number select) select))
    (if (or (null select) (zerop select))
	select
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
	    (gnus-sorted-intersection
	     gnus-newsgroup-unreads
	     (gnus-sorted-complement gnus-newsgroup-unreads articles)))
      articles)))

(defun gnus-killed-articles (killed articles)
  (let (out)
    (while articles
      (if (inline (gnus-member-of-range (car articles) killed))
	  (setq out (cons (car articles) out)))
      (setq articles (cdr articles)))
    out))

(defun gnus-adjust-marked-articles (info &optional active)
  "Remove all marked articles that are no longer legal."
  (let* ((marked-lists (gnus-info-marks info))
	 (active (or active (gnus-active (gnus-info-group info))))
	 (min (car active))
	 m prev)
    ;; There are many types of marked articles.
    (while marked-lists
      (setq m (cdr (setq prev (car marked-lists))))
      (cond
       ((or (eq 'tick (car prev)) (eq 'dormant (car prev)))
	;; Make sure that all ticked articles are a subset of the
	;; active articles. 
	(while m
	  (if (< (car m) min)
	      (setcdr prev (cdr m))
	    (setq prev m))
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
	  (if (< (car (car m)) min)
	      (setcdr prev (cdr m))
	    (setq prev m))
	  (setq m (cdr m))))
       ((eq 'killed (car prev))
	;; Articles that have been through the kill process are
	;; to be a subset of active articles.
	(while (and m (< (or (and (numberp (car m)) (car m))
			     (cdr (car m)))
			 min))
	  (setcdr prev (cdr m))
	  (setq m (cdr m)))
	(if (and m (< (or (and (numberp (car m)) (car m))
			  (car (car m)))
		      min)) 
	    (setcar (if (numberp (car m)) m (car m)) min)))
       ((or (eq 'reply (car prev)) (eq 'expire (car prev)))
	;; The replied and expirable articles have to be articles
	;; that are active. 
	(while m
	  (if (< (car m) min)
	      (setcdr prev (cdr m))
	    (setq prev m))
	  (setq m (cdr m)))))
      (setq marked-lists (cdr marked-lists)))
    ;; Remove all lists that are empty.
    (setq marked-lists (gnus-info-marks info))
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
	  (gnus-info-set-marks info marked-lists)))
    ;; Finally, if there are no marked lists at all left, and if there
    ;; are no elements after the lists in the info list, we just chop
    ;; the info list off before the marked lists.
    (and (null marked-lists) 
	 (not (nthcdr 4 info))
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
	(progn
	  (setcar (nthcdr 3 info) newmarked)
	  (and (not newmarked)
	       (not (nthcdr 4 info))
	       (setcdr (nthcdr 2 info) nil)))
      (if newmarked
	  (setcdr (nthcdr 2 info) (list newmarked))))))

(defun gnus-add-marked-articles (group type articles &optional info force)
  ;; Add ARTICLES of TYPE to the info of GROUP.
  ;; If INFO is non-nil, use that info.  If FORCE is non-nil, don't
  ;; add, but replace marked articles of TYPE with ARTICLES.
  (let ((info (or info (gnus-get-info group)))
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
  ;; Is this mode line one we keep updated?
  (when (memq where gnus-updated-mode-lines)
    (let (mode-string)
      (save-excursion
	;; We evaluate this in the summary buffer since these
	;; variables are buffer-local to that buffer.
	(set-buffer gnus-summary-buffer)
	;; We bind all these variables that are used in the `eval' form
	;; below. 
	(let* ((mformat (if (eq where 'article) 
			    gnus-article-mode-line-format-spec
			  gnus-summary-mode-line-format-spec))
	       (gnus-tmp-group-name gnus-newsgroup-name)
	       (gnus-tmp-article-number (or gnus-current-article 0))
	       (gnus-tmp-unread gnus-newsgroup-unreads)
	       (gnus-tmp-unread-and-unticked (length gnus-newsgroup-unreads))
	       (gnus-tmp-unselected (length gnus-newsgroup-unselected))
	       (gnus-tmp-unread-and-unselected
		(cond ((and (zerop gnus-tmp-unread-and-unticked)
			    (zerop gnus-tmp-unselected)) "")
		      ((zerop gnus-tmp-unselected) 
		       (format "{%d more}" gnus-tmp-unread-and-unticked))
		      (t (format "{%d(+%d) more}"
				 gnus-tmp-unread-and-unticked
				 gnus-tmp-unselected))))
	       (gnus-tmp-subject
		(if gnus-current-headers
		    (mail-header-subject gnus-current-headers) ""))
	       max-len 
	       header);; passed as argument to any user-format-funcs
	  (setq mode-string (eval mformat))
	  (setq max-len (max 4 (if gnus-mode-non-string-length
				   (- (frame-width) 
				      gnus-mode-non-string-length)
				 (length mode-string))))
	  ;; We might have to chop a bit of the string off...
	  (when (> (length mode-string) max-len)
	    (setq mode-string 
		  (concat (gnus-truncate-string mode-string (- max-len 3))
			  "...")))
	  ;; Pad the mode string a bit.
	  (setq mode-string (format (format "%%-%ds" max-len) mode-string))))
      ;; Update the mode line.
      (setq mode-line-buffer-identification mode-string)
      (set-buffer-modified-p t))))

(defun gnus-create-xref-hashtb (from-newsgroup headers unreads ticked dormant)
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
      (if (and (setq xrefs (mail-header-xref header))
	       (not (memq (setq number (mail-header-number header)) unreads))
	       (not (memq number ticked))
	       (not (memq number dormant)))
	  (progn
	    (setq start 0)
	    (while (string-match "\\([^ ]+\\)[:/]\\([0-9]+\\)" xrefs start)
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

(defun gnus-mark-xrefs-as-read (from-newsgroup headers unreads expirable
					       ticked dormant)
  "Look through all the headers and mark the Xrefs as read."
  (let ((virtual (memq 'virtual 
		       (assoc (symbol-name (car (gnus-find-method-for-group 
						 from-newsgroup)))
			      gnus-valid-select-methods)))
	name entry info xref-hashtb idlist method
	nth4)
    (save-excursion
      (set-buffer gnus-group-buffer)
      (if (setq xref-hashtb 
		(gnus-create-xref-hashtb 
		 from-newsgroup headers unreads ticked dormant))
	  (mapatoms 
	   (lambda (group)
	     (if (string= from-newsgroup (setq name (symbol-name group)))
		 ()
	       (setq idlist (symbol-value group))
	       ;; Dead groups are not updated.
	       (if (and (prog1 
			    (setq entry (gnus-gethash name gnus-newsrc-hashtb)
				  info (nth 2 entry))
			  (if (stringp (setq nth4 (gnus-info-method info)))
			      (setq nth4 (gnus-server-to-method nth4))))
			;; Only do the xrefs if the group has the same
			;; select method as the group we have just read.
			(or (gnus-methods-equal-p 
			     nth4 (gnus-find-method-for-group from-newsgroup))
			    virtual
			    (equal nth4 
				   (setq method (gnus-find-method-for-group 
						 from-newsgroup)))
			    (and (equal (car nth4) (car method))
				 (equal (nth 1 nth4) (nth 1 method))))
			gnus-use-cross-reference
			(or (not (eq gnus-use-cross-reference t))
			    virtual
			    ;; Only do cross-references on subscribed
			    ;; groups, if that is what is wanted.  
			    (<= (gnus-info-level info) gnus-level-subscribed)))
		   (gnus-group-make-articles-read name idlist expirable))))
	   xref-hashtb)))))

(defun gnus-group-make-articles-read (group articles expirable)
  (let* ((num 0)
	 (entry (gnus-gethash group gnus-newsrc-hashtb))
	 (info (nth 2 entry))
	 (active (gnus-active group))
	 exps expirable range)
    ;; First peel off all illegal article numbers.
    (if active
	(let ((ids articles)
	      id first)
	  (setq exps nil)
	  (while ids
	    (setq id (car ids))
	    (if (and first (> id (cdr active)))
		(progn
		  ;; We'll end up in this situation in one particular
		  ;; obscure situation.  If you re-scan a group and get
		  ;; a new article that is cross-posted to a different
		  ;; group that has not been re-scanned, you might get
		  ;; crossposted article that has a higher number than
		  ;; Gnus believes possible.  So we re-activate this
		  ;; group as well.  This might mean doing the
		  ;; crossposting thingie will *increase* the number
		  ;; of articles in some groups.  Tsk, tsk.
		  (setq active (or (gnus-activate-group group) active))))
	    (if (or (> id (cdr active))
		    (< id (car active)))
		(setq articles (delq id articles)))
	    (and (memq id expirable)
		 (setq exps (cons id exps)))
	    (setq ids (cdr ids)))))
    ;; Update expirable articles.
    (gnus-add-marked-articles nil 'expirable exps info)
    ;; If the read list is nil, we init it.
    (and active
	 (null (gnus-info-read info))
	 (> (car active) 1)
	 (gnus-info-set-read info (cons 1 (1- (car active)))))
    ;; Then we add the read articles to the range.
    (gnus-info-set-read
     info
     (setq range
	   (gnus-add-to-range 
	    (gnus-info-read info) (setq articles (sort articles '<)))))
    ;; Then we have to re-compute how many unread
    ;; articles there are in this group.
    (if active
	(progn
	  (cond 
	   ((not range)
	    (setq num (- (1+ (cdr active)) (car active))))
	   ((not (listp (cdr range)))
	    (setq num (- (cdr active) (- (1+ (cdr range)) 
					 (car range)))))
	   (t
	    (while range
	      (if (numberp (car range))
		  (setq num (1+ num))
		(setq num (+ num (- (1+ (cdr (car range)))
				    (car (car range))))))
	      (setq range (cdr range)))
	    (setq num (- (cdr active) num))))
	  ;; Update the number of unread articles.
	  (setcar entry num)
	  ;; Update the group buffer.
	  (gnus-group-update-group group t)))))

(defun gnus-methods-equal-p (m1 m2)
  (let ((m1 (or m1 gnus-select-method))
	(m2 (or m2 gnus-select-method)))
    (or (equal m1 m2)
	(and (eq (car m1) (car m2))
	     (or (not (memq 'address (assoc (symbol-name (car m1))
					    gnus-valid-select-methods)))
		 (equal (nth 1 m1) (nth 1 m2)))))))

(defsubst gnus-header-value ()
  (buffer-substring (match-end 0) (gnus-point-at-eol)))

(defvar gnus-newsgroup-none-id 0)

(defun gnus-get-newsgroup-headers ()
  (setq gnus-article-internal-prepare-hook nil)
  (let ((cur nntp-server-buffer)
	(dependencies (save-excursion (set-buffer gnus-summary-buffer)
				      gnus-newsgroup-dependencies))
	headers id id-dep ref-dep end ref)
    (save-excursion
      (set-buffer nntp-server-buffer)
      (let ((case-fold-search t)
	    in-reply-to header number p lines)
	(goto-char (point-min))
	;; Search to the beginning of the next header.  Error messages
	;; do not begin with 2 or 3.
	(while (re-search-forward "^[23][0-9]+ " nil t)
	  (setq id nil
		ref nil)
	  ;; This implementation of this function, with nine
	  ;; search-forwards instead of the one re-search-forward and
	  ;; a case (which basically was the old function) is actually
	  ;; about twice as fast, even though it looks messier.  You
	  ;; can't have everything, I guess.  Speed and elegance
	  ;; doesn't always go hand in hand.
	  (setq 
	   header
	   (vector
	    ;; Number.
	    (prog1
		(read cur)
	      (end-of-line)
	      (setq p (point))
	      (narrow-to-region (point) 
				(or (and (search-forward "\n.\n" nil t)
					 (- (point) 2))
				    (point))))
	    ;; Subject.
	    (progn
	      (goto-char p)
	      (if (search-forward "\nsubject: " nil t)
		  (gnus-header-value) "(none)"))
	    ;; From.
	    (progn
	      (goto-char p)
	      (if (search-forward "\nfrom: " nil t)
		  (gnus-header-value) "(nobody)"))
	    ;; Date.
	    (progn
	      (goto-char p)
	      (if (search-forward "\ndate: " nil t)
		  (gnus-header-value) ""))
	    ;; Message-ID.
	    (progn
	      (goto-char p)
	      (if (search-forward "\nmessage-id: " nil t)
		  (setq id (gnus-header-value))
		;; If there was no message-id, we just fake one to make
		;; subsequent routines simpler.
		(setq id (concat "none+" 
				 (int-to-string 
				  (setq gnus-newsgroup-none-id 
					(1+ gnus-newsgroup-none-id)))))))
	    ;; References.
	    (progn
	      (goto-char p)
	      (if (search-forward "\nreferences: " nil t)
		  (prog1
		      (gnus-header-value)
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
		;; were no references and the in-reply-to header looks
		;; promising. 
		(if (and (search-forward "\nin-reply-to: " nil t)
			 (setq in-reply-to (gnus-header-value))
			 (string-match "<[^>]+>" in-reply-to))
		    (prog1
			(setq ref (substring in-reply-to (match-beginning 0)
					     (match-end 0)))
		      (setq ref (downcase ref))))
		(setq ref "")))
	    ;; Chars.
	    0
	    ;; Lines.
	    (progn
	      (goto-char p)
	      (if (search-forward "\nlines: " nil t)
		  (if (numberp (setq lines (read cur)))
		      lines 0)
		0))
	    ;; Xref.
	    (progn
	      (goto-char p)
	      (and (search-forward "\nxref: " nil t)
		   (gnus-header-value)))))
	  (if (and gnus-nocem-hashtb
		   (gnus-gethash id gnus-nocem-hashtb))
	      ;; Banned article.
	      (setq header nil)
	    ;; We do the threading while we read the headers.  The
	    ;; message-id and the last reference are both entered into
	    ;; the same hash table.  Some tippy-toeing around has to be
	    ;; done in case an article has arrived before the article
	    ;; which it refers to.
	    (if (boundp (setq id-dep (intern (downcase id) dependencies)))
		(if (car (symbol-value id-dep))
		    ;; An article with this Message-ID has already
		    ;; been seen, so we ignore this one, except we add
		    ;; any additional Xrefs (in case the two articles
		    ;; came from different servers).
		    (progn
		      (mail-header-set-xref 
		       (car (symbol-value id-dep))
		       (concat (or (mail-header-xref 
				    (car (symbol-value id-dep))) "")
			       (or (mail-header-xref header) "")))
		      (setq header nil))
		  (setcar (symbol-value id-dep) header))
	      (set id-dep (list header))))
	  (if header
	      (progn
		(if (boundp (setq ref-dep (intern ref dependencies)))
		    (setcdr (symbol-value ref-dep) 
			    (nconc (cdr (symbol-value ref-dep))
				   (list (symbol-value id-dep))))
		  (set ref-dep (list nil (symbol-value id-dep))))
		(setq headers (cons header headers))))
	  (goto-char (point-max))
	  (widen))
	(nreverse headers)))))

;; The following macros and functions were written by Felix Lee
;; <flee@cse.psu.edu>. 

(defmacro gnus-nov-read-integer ()
  '(prog1
       (if (= (following-char) ?\t)
	   0
	 (let ((num (condition-case nil (read buffer) (error nil))))
	   (if (numberp num) num 0)))
     (or (eobp) (forward-char 1))))

(defmacro gnus-nov-skip-field ()
  '(search-forward "\t" eol 'move))

(defmacro gnus-nov-field ()
  '(buffer-substring (point) (if (gnus-nov-skip-field) (1- (point)) eol)))

;; Goes through the xover lines and returns a list of vectors
(defun gnus-get-newsgroup-headers-xover (sequence)
  "Parse the news overview data in the server buffer, and return a
list of headers that match SEQUENCE (see `nntp-retrieve-headers')."
  ;; Get the Xref when the users reads the articles since most/some
  ;; NNTP servers do not include Xrefs when using XOVER.
  (setq gnus-article-internal-prepare-hook '(gnus-article-get-xrefs))
  (let ((cur nntp-server-buffer)
	(dependencies gnus-newsgroup-dependencies)
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

;; This function has to be called with point after the article number
;; on the beginning of the line.
(defun gnus-nov-parse-line (number dependencies)
  (let ((none 0)
	(eol (gnus-point-at-eol)) 
	(buffer (current-buffer))
	header ref id id-dep ref-dep)

    ;; overview: [num subject from date id refs chars lines misc]
    (narrow-to-region (point) eol)
    (or (eobp) (forward-char))

    (condition-case nil
	(setq header
	      (vector 
	       number			; number
	       (gnus-nov-field)      	; subject
	       (gnus-nov-field)      	; from
	       (gnus-nov-field)		; date
	       (setq id (or (gnus-nov-field)
			    (concat "none+"
				    (int-to-string 
				     (setq none (1+ none)))))) ; id
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
		 (gnus-nov-field))	; refs
	       (gnus-nov-read-integer)	; chars
	       (gnus-nov-read-integer)	; lines
	       (if (= (following-char) ?\n)
		   nil
		 (gnus-nov-field))	; misc
	       ))
      (error (progn 
	       (ding)
	       (message "Strange nov line.")
	       (setq header nil)
	       (goto-char eol))))

    (widen)

    ;; We build the thread tree.
    (and header
	 (if (and gnus-nocem-hashtb
		  (gnus-gethash id gnus-nocem-hashtb))
	     ;; Banned article.
	     (setq header nil)
	   (if (boundp (setq id-dep (intern (downcase id) dependencies)))
	       (if (car (symbol-value id-dep))
		   ;; An article with this Message-ID has already been seen,
		   ;; so we ignore this one, except we add any additional
		   ;; Xrefs (in case the two articles came from different
		   ;; servers.
		   (progn
		     (mail-header-set-xref 
		      (car (symbol-value id-dep))
		      (concat (or (mail-header-xref 
				   (car (symbol-value id-dep))) "")
			      (or (mail-header-xref header) "")))
		     (setq header nil))
		 (setcar (symbol-value id-dep) header))
	     (set id-dep (list header)))))
    (if header
	(progn
	  (if (boundp (setq ref-dep (intern (or ref "none") dependencies)))
	      (setcdr (symbol-value ref-dep) 
		      (nconc (cdr (symbol-value ref-dep))
			     (list (symbol-value id-dep))))
	    (set ref-dep (list nil (symbol-value id-dep))))))
    header))

(defun gnus-article-get-xrefs ()
  "Fill in the Xref value in `gnus-current-headers', if necessary.
This is meant to be called in `gnus-article-internal-prepare-hook'."
  (let ((headers (save-excursion (set-buffer gnus-summary-buffer)
				 gnus-current-headers)))
    (or (not gnus-use-cross-reference)
	(not headers)
	(and (mail-header-xref headers)
	     (not (string= (mail-header-xref headers) "")))
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
		  (mail-header-set-xref headers xref))))))))

(defun gnus-summary-insert-subject (id)
  "Find article ID and insert the summary line for that article."
  (let ((header (gnus-read-header id))
	number)
    (when header
      ;; Rebuild the thread that this article is part of and go to the
      ;; article we have fetched.
      (gnus-rebuild-thread (mail-header-id header))
      (gnus-summary-goto-subject (setq number (mail-header-number header)))
      (when (> number 0)
	;; We have to update the boundaries, possibly.
	(and (> number gnus-newsgroup-end)
	     (setq gnus-newsgroup-end number))
	(and (< number gnus-newsgroup-begin)
	     (setq gnus-newsgroup-begin number))
	(setq gnus-newsgroup-unselected
	      (delq number gnus-newsgroup-unselected)))
      ;; Report back a success.
      number)))

(defun gnus-summary-work-articles (n)
  "Return a list of articles to be worked upon.  The prefix argument,
the list of process marked articles, and the current article will be
taken into consideration."
  (let (articles article)
    (if (and n (numberp n))
	(let ((backward (< n 0))
	      (n (abs n)))
	  (save-excursion
	    (while 
		(and (> n 0)
		     (setq articles 
			   (cons (setq article (gnus-summary-article-number))
				 articles))
		     (if backward
			 (gnus-summary-find-prev nil article)
		       (gnus-summary-find-next nil article)))
	      (setq n (1- n))))
	  (sort articles (function <)))
      (or (reverse gnus-newsgroup-processable)
	  (list (gnus-summary-article-number))))))

(defun gnus-summary-search-group (&optional backward use-level)
  "Search for next unread newsgroup.
If optional argument BACKWARD is non-nil, search backward instead."
  (save-excursion
    (set-buffer gnus-group-buffer)
    (if (gnus-group-search-forward 
	 backward nil (if use-level (gnus-group-group-level) nil))
	(gnus-group-group-name))))

(defun gnus-summary-best-group (&optional exclude-group)
  "Find the name of the best unread group.
If EXCLUDE-GROUP, do not go to this group."
  (save-excursion
    (set-buffer gnus-group-buffer)
    (save-excursion
      (gnus-group-best-unread-group exclude-group))))

(defun gnus-summary-find-next (&optional unread article backward)
  (if backward (gnus-summary-find-prev)
    (let* ((article (or article (gnus-summary-article-number)))
	   (arts (gnus-data-find-list article))
	   result)
      (unless gnus-summary-check-current
	(setq arts (cdr arts)))
      (when (setq result
		(if unread
		    (progn
		      (while arts
			(when (gnus-data-unread-p (car arts))
			  (setq result (car arts)
				arts nil))
			(setq arts (cdr arts)))
		      result)
		  (car arts)))
	(goto-char (gnus-data-pos result))
	(gnus-data-number result)))))

(defun gnus-summary-find-prev (&optional unread article)
  (let* ((article (or article (gnus-summary-article-number)))
	 (arts (gnus-data-find-list article (gnus-data-list 'rev)))
	 result)
    (unless gnus-summary-check-current
      (setq arts (cdr arts)))
    (if (setq result
	      (if unread
		  (progn
		    (while arts
		      (and (gnus-data-unread-p (car arts))
			   (setq result (car arts)
				 arts nil))
		      (setq arts (cdr arts)))
		    result)
		(car arts)))
	(progn
	  (goto-char (gnus-data-pos result))
	  (gnus-data-number result)))))

(defun gnus-summary-find-subject (subject &optional unread backward article)
  (let* ((article (or article (gnus-summary-article-number)))
	 (articles (gnus-data-list backward))
	 (arts (gnus-data-find-list article articles))
	 result)
    (unless gnus-summary-check-current
      (setq arts (cdr arts)))
    (while arts
      (and (or (not unread)
	       (gnus-data-unread-p (car arts)))
	   (vectorp (gnus-data-header (car arts)))
	   (gnus-subject-equal 
	    subject (mail-header-subject (gnus-data-header (car arts))))
	   (setq result (car arts)
		 arts nil))
      (setq arts (cdr arts)))
    (and result
	 (goto-char (gnus-data-pos result))
	 (gnus-data-number result))))

(defun gnus-summary-search-forward (&optional unread subject backward)
  (cond (subject
	 (gnus-summary-find-subject subject unread backward))
	(backward
	 (gnus-summary-find-prev unread))
	(t
	 (gnus-summary-find-next unread))))

(defun gnus-summary-recenter ()
  "Center point in the summary window.
If `gnus-auto-center-summary' is nil, or the article buffer isn't
displayed, no centering will be performed." 
  ;; Suggested by earle@mahendo.JPL.NASA.GOV (Greg Earle).
  ;; Recenter only when requested.  Suggested by popovich@park.cs.columbia.edu.
  (let* ((top (cond ((< (window-height) 4) 0)
		    ((< (window-height) 7) 1)
		    (t 2)))
	 (height (1- (window-height)))
	 (bottom (save-excursion (goto-char (point-max))
				 (forward-line (- height))
				 (point)))
	 (window (get-buffer-window (current-buffer))))
    (and 
     ;; The user has to want it,
     gnus-auto-center-summary 
     ;; the article buffer must be displayed,
     (get-buffer-window gnus-article-buffer)
     ;; Set the window start to either `bottom', which is the biggest
     ;; possible valid number, or the second line from the top,
     ;; whichever is the least.
     (set-window-start
      window (min bottom (save-excursion (forward-line (- top)) (point)))))))

;; Function written by Stainless Steel Rat <ratinox@ccs.neu.edu>.
(defun gnus-short-group-name (group &optional levels)
  "Collapse GROUP name LEVELS."
  (let* ((name "") (foreign "") (depth -1) (skip 1)
	 (levels (or levels
		     (progn
		       (while (string-match "\\." group skip)
			 (setq skip (match-end 0)
			       depth (+ depth 1)))
		       depth))))
    (if (string-match ":" group)
	(setq foreign (substring group 0 (match-end 0))
	      group (substring group (match-end 0))))
    (while group
      (if (and (string-match "\\." group) (> levels 0))
	  (setq name (concat name (substring group 0 1))
		group (substring group (match-end 0))
		levels (- levels 1)
		name (concat name "."))
	(setq name (concat foreign name group)
	      group nil)))
    name))

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
  (let* ((read (gnus-info-read (gnus-get-info group)))
	 (active (gnus-active group))
	 (last (cdr active))
	 first nlast unread)
    ;; If none are read, then all are unread. 
    (if (not read)
	(setq first (car active))
      ;; If the range of read articles is a single range, then the
      ;; first unread article is the article after the last read
      ;; article.  Sounds logical, doesn't it?
      (if (not (listp (cdr read)))
	  (setq first (1+ (cdr read)))
	;; `read' is a list of ranges.
	(if (/= (setq nlast (or (and (numberp (car read)) (car read)) 
				(car (car read)))) 1)
	    (setq first 1))
	(while read
	  (if first 
	      (while (< first nlast)
		(setq unread (cons first unread))
		(setq first (1+ first))))
	  (setq first (1+ (if (atom (car read)) (car read) (cdr (car read)))))
	  (setq nlast (if (atom (car (cdr read))) 
			  (car (cdr read))
			(car (car (cdr read)))))
	  (setq read (cdr read)))))
    ;; And add the last unread articles.
    (while (<= first last)
      (setq unread (cons first unread))
      (setq first (1+ first)))
    ;; Return the list of unread articles.
    (nreverse unread)))

(defun gnus-list-of-read-articles (group)
  (let* ((info (gnus-get-info group))
	 (marked (gnus-info-marks info))
	 (active (gnus-active group)))
    (and info active
	 (gnus-set-difference
	  (gnus-sorted-complement 
	   (gnus-uncompress-range active) 
	   (gnus-list-of-unread-articles group))
	  (append 
	   (cdr (assq 'dormant marked))
	   (cdr (assq 'tick marked)))))))

;; Various summary commands

(defun gnus-summary-universal-argument ()
  "Perform any operation on all articles marked with the process mark."
  (interactive)
  (gnus-set-global-variables)
  (let ((articles (reverse gnus-newsgroup-processable))
	func)
    (or articles (error "No articles marked"))
    (or (setq func (key-binding (read-key-sequence "C-c C-u")))
	(error "Undefined key"))
    (while articles
      (gnus-summary-goto-subject (car articles))
      (command-execute func)
      (gnus-summary-remove-process-mark (car articles))
      (setq articles (cdr articles)))))

(defun gnus-summary-toggle-truncation (&optional arg)
  "Toggle truncation of summary lines.
With arg, turn line truncation on iff arg is positive."
  (interactive "P")
  (setq truncate-lines
	(if (null arg) (not truncate-lines)
	  (> (prefix-numeric-value arg) 0)))
  (redraw-display))

(defun gnus-summary-reselect-current-group (&optional all)
  "Once exit and then reselect the current newsgroup.
The prefix argument ALL means to select all articles."
  (interactive "P")
  (gnus-set-global-variables)
  (let ((current-subject (gnus-summary-article-number))
	(group gnus-newsgroup-name))
    (setq gnus-newsgroup-begin nil)
    (gnus-summary-exit t)
    ;; We have to adjust the point of group mode buffer because the
    ;; current point was moved to the next unread newsgroup by
    ;; exiting.
    (gnus-summary-jump-to-group group)
    (gnus-group-read-group all t)
    (gnus-summary-goto-subject current-subject)))

(defun gnus-summary-rescan-group (&optional all)
  "Exit the newsgroup, ask for new articles, and select the newsgroup."
  (interactive "P")
  (gnus-set-global-variables)
  ;; Fix by Ilja Weis <kult@uni-paderborn.de>.
  (let ((group gnus-newsgroup-name))
    (gnus-summary-exit)
    (gnus-summary-jump-to-group group)
    (save-excursion
      (set-buffer gnus-group-buffer)
      (gnus-group-get-new-news-this-group 1))
    (gnus-summary-jump-to-group group)
    (gnus-group-read-group all)))

(defun gnus-summary-update-info ()
  (let* ((group gnus-newsgroup-name))
    (when gnus-newsgroup-kill-headers
      (setq gnus-newsgroup-killed
	    (gnus-compress-sequence
	     (nconc
	      (gnus-set-sorted-intersection
	       (gnus-uncompress-range gnus-newsgroup-killed)
	       (setq gnus-newsgroup-unselected
		     (sort gnus-newsgroup-unselected '<)))
	      (setq gnus-newsgroup-unreads
		    (sort gnus-newsgroup-unreads '<))) t)))
    (unless (listp (cdr gnus-newsgroup-killed))
      (setq gnus-newsgroup-killed (list gnus-newsgroup-killed)))
    (let ((headers gnus-newsgroup-headers))
      (gnus-close-group group)
      (run-hooks 'gnus-exit-group-hook)
      (gnus-update-read-articles 
       group gnus-newsgroup-unreads gnus-newsgroup-unselected 
       gnus-newsgroup-marked
       t gnus-newsgroup-replied gnus-newsgroup-expirable
       gnus-newsgroup-killed gnus-newsgroup-dormant
       gnus-newsgroup-bookmarks 
       (and gnus-save-score gnus-newsgroup-scored))
      (when gnus-use-cross-reference
	(gnus-mark-xrefs-as-read 
	 group headers gnus-newsgroup-unreads gnus-newsgroup-expirable
	 gnus-newsgroup-marked gnus-newsgroup-dormant))
      ;; Do adaptive scoring, and possibly save score files.
      (when gnus-newsgroup-adaptive
	(gnus-score-adaptive))
      (when gnus-use-scoring 
	(gnus-score-save))
      ;; Do not switch windows but change the buffer to work.
      (set-buffer gnus-group-buffer)
      (or (gnus-ephemeral-group-p gnus-newsgroup-name)
	  (gnus-group-update-group group)))))
  
(defun gnus-summary-exit (&optional temporary)
  "Exit reading current newsgroup, and then return to group selection mode.
gnus-exit-group-hook is called with no arguments if that value is non-nil."
  (interactive)
  (gnus-set-global-variables)
  (gnus-kill-save-kill-buffer)
  (let* ((group gnus-newsgroup-name)
	 (quit-config (gnus-group-quit-config gnus-newsgroup-name))
	 (mode major-mode)
	 (buf (current-buffer)))
    (run-hooks 'gnus-summary-prepare-exit-hook)
    ;; Make all changes in this group permanent.
    (gnus-summary-update-info)		
    (set-buffer buf)
    (and gnus-use-cache (gnus-cache-possibly-remove-articles))
    ;; Make sure where I was, and go to next newsgroup.
    (set-buffer gnus-group-buffer)
    (or quit-config
	(progn
	  (gnus-group-jump-to-group group)
	  (gnus-group-next-unread-group 1)))
    (if temporary
	nil				;Nothing to do.
      ;; We set all buffer-local variables to nil.  It is unclear why
      ;; this is needed, but if we don't, buffer-local variables are
      ;; not garbage-collected, it seems.  This would the lead to en
      ;; ever-growing Emacs.
      (set-buffer buf)
      (gnus-summary-clear-local-variables)
      ;; We clear the global counterparts of the buffer-local
      ;; variables as well, just to be on the safe side.
      (gnus-configure-windows 'group 'force)
      (gnus-summary-clear-local-variables)
      ;; Return to group mode buffer. 
      (if (eq mode 'gnus-summary-mode)
	  (gnus-kill-buffer buf))
      (if (get-buffer gnus-article-buffer)
	  (bury-buffer gnus-article-buffer))
      (setq gnus-current-select-method gnus-select-method)
      (pop-to-buffer gnus-group-buffer)
      (if (not quit-config)
	  (progn
	    (gnus-group-jump-to-group group)
	    (gnus-group-next-unread-group 1))
	(if (not (buffer-name (car quit-config)))
	    (gnus-configure-windows 'group 'force)
	  (set-buffer (car quit-config))
	  (and (eq major-mode 'gnus-summary-mode)
	       (gnus-set-global-variables))
	  (gnus-configure-windows (cdr quit-config))))
      (run-hooks 'gnus-summary-exit-hook))))

(defalias 'gnus-summary-quit 'gnus-summary-exit-no-update)
(defun gnus-summary-exit-no-update (&optional no-questions)
  "Quit reading current newsgroup without updating read article info."
  (interactive)
  (gnus-set-global-variables)
  (let* ((group gnus-newsgroup-name)
	 (quit-config (gnus-group-quit-config group)))
    (if (or no-questions
	    gnus-expert-user
	    (gnus-y-or-n-p "Do you really wanna quit reading this group? "))
	(progn
	  (gnus-close-group group)
	  (gnus-summary-clear-local-variables)
	  (set-buffer gnus-group-buffer)
	  (gnus-summary-clear-local-variables)
	  ;; Return to group selection mode.
	  (gnus-configure-windows 'group 'force)
	  (if (get-buffer gnus-summary-buffer)
	      (kill-buffer gnus-summary-buffer))
	  (if (get-buffer gnus-article-buffer)
	      (bury-buffer gnus-article-buffer))
	  (if (equal (gnus-group-group-name) group)
	      (gnus-group-next-unread-group 1))
	  (if quit-config
	      (progn
		(if (not (buffer-name (car quit-config)))
		    (gnus-configure-windows 'group 'force)
		  (set-buffer (car quit-config))
		  (and (eq major-mode 'gnus-summary-mode)
		       (gnus-set-global-variables))
		  (gnus-configure-windows (cdr quit-config)))))))))

;; Suggested by Andrew Eskilsson <pi92ae@pt.hk-r.se>.
(defun gnus-summary-fetch-faq (&optional faq-dir)
  "Fetch the FAQ for the current group.
If FAQ-DIR (the prefix), prompt for a directory to search for the faq
in."
  (interactive 
   (list
    (if current-prefix-arg
	(completing-read 
	 "Faq dir: " (and (listp gnus-group-faq-directory)
			  gnus-group-faq-directory)))))
  (let (gnus-faq-buffer)
    (and (setq gnus-faq-buffer 
	       (gnus-group-fetch-faq gnus-newsgroup-name faq-dir))
	 (gnus-configure-windows 'summary-faq))))

;; Suggested by Per Abrahamsen <amanda@iesd.auc.dk>.
(defun gnus-summary-describe-group (&optional force)
  "Describe the current newsgroup."
  (interactive "P")
  (gnus-group-describe-group force gnus-newsgroup-name))

(defun gnus-summary-describe-briefly ()
  "Describe summary mode commands briefly."
  (interactive)
  (gnus-message 6
		(substitute-command-keys "\\<gnus-summary-mode-map>\\[gnus-summary-next-page]:Select  \\[gnus-summary-next-unread-article]:Forward  \\[gnus-summary-prev-unread-article]:Backward  \\[gnus-summary-exit]:Exit  \\[gnus-info-find-node]:Run Info  \\[gnus-summary-describe-briefly]:This help")))

;; Walking around group mode buffer from summary mode.

(defun gnus-summary-next-group (&optional no-article target-group backward)
  "Exit current newsgroup and then select next unread newsgroup.
If prefix argument NO-ARTICLE is non-nil, no article is selected
initially.  If NEXT-GROUP, go to this group.  If BACKWARD, go to
previous group instead."
  (interactive "P")
  (gnus-set-global-variables)
  (let ((current-group gnus-newsgroup-name)
	(current-buffer (current-buffer))
	entered)
    ;; First we semi-exit this group to update Xrefs and all variables.
    ;; We can't do a real exit, because the window conf must remain
    ;; the same in case the user is prompted for info, and we don't
    ;; want the window conf to change before that...
    (gnus-summary-exit t)
    (while (not entered)
      ;; Then we find what group we are supposed to enter.
      (set-buffer gnus-group-buffer)
      (gnus-group-jump-to-group current-group)
      (setq target-group 
	    (or target-group 	    
		(if (eq gnus-keep-same-level 'best) 
		    (gnus-summary-best-group gnus-newsgroup-name)
		  (gnus-summary-search-group backward gnus-keep-same-level))))
      (if (not target-group)
	  ;; There are no further groups, so we return to the group
	  ;; buffer.
	  (progn
	    (gnus-message 5 "Returning to the group buffer")
	    (setq entered t)
	    (set-buffer current-buffer)
	    (gnus-summary-exit))
	;; We try to enter the target group.
	(gnus-group-jump-to-group target-group)
	(let ((unreads (gnus-group-group-unread)))
	  (if (and (or (eq t unreads)
		       (and unreads (not (zerop unreads))))
		   (gnus-summary-read-group
		    target-group nil no-article current-buffer))
	      (setq entered t)
	    (setq current-group target-group
		  target-group nil)))))))

(defun gnus-summary-prev-group (&optional no-article)
  "Exit current newsgroup and then select previous unread newsgroup.
If prefix argument NO-ARTICLE is non-nil, no article is selected initially."
  (interactive "P")
  (gnus-summary-next-group no-article nil t))

;; Walking around summary lines.

(defun gnus-summary-first-subject (&optional unread)
  "Go to the first unread subject.
If UNREAD is non-nil, go to the first unread article.
Returns the article selected or nil if there are no unread articles."
  (interactive "P")
  (prog1
      (cond 
       ;; Empty summary.
       ((null gnus-newsgroup-data)
	(gnus-message 3 "No articles in the group")
	nil)
       ;; Pick the first article.
       ((not unread)
	(goto-char (gnus-data-pos (car gnus-newsgroup-data)))
	(gnus-data-number (car gnus-newsgroup-data)))
       ;; No unread articles.
       ((null gnus-newsgroup-unreads)
	(gnus-message 3 "No more unread articles")
	nil)
       ;; Find the first unread article.
       (t
	(let ((data gnus-newsgroup-data))
	  (while (and data
		      (not (gnus-data-unread-p (car data))))
	    (setq data (cdr data)))
	  (if data
	      (progn
		(goto-char (gnus-data-pos (car data)))
		(gnus-data-number (car data)))))))
    (gnus-summary-position-point)))

(defun gnus-summary-next-subject (n &optional unread dont-display)
  "Go to next N'th summary line.
If N is negative, go to the previous N'th subject line.
If UNREAD is non-nil, only unread articles are selected.
The difference between N and the actual number of steps taken is
returned."
  (interactive "p")
  (let ((backward (< n 0))
	(n (abs n)))
    (while (and (> n 0)
		(if backward
		    (gnus-summary-find-prev unread)
		  (gnus-summary-find-next unread)))
      (setq n (1- n)))
    (if (/= 0 n) (gnus-message 7 "No more%s articles"
			       (if unread " unread" "")))
    (or dont-display
	(progn
	  (gnus-summary-recenter)
	  (gnus-summary-position-point)))
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

(defun gnus-summary-goto-subject (article &optional force silent)
  "Go the subject line of ARTICLE.
If FORCE, also allow jumping to articles not currently shown."
  (let ((b (point))
	(data (gnus-data-find article)))
    ;; We read in the article if we have to.
    (and (not data) 
	 force
	 (gnus-summary-insert-subject article)
	 (setq data (gnus-data-find article)))
    (goto-char b)
    (if (and (not silent) (not data))
	(progn
	  (message "Can't find article %d" article)
	  nil)
      (goto-char (gnus-data-pos data))
      article)))

;; Walking around summary lines with displaying articles.

(defun gnus-summary-expand-window (&optional arg)
  "Make the summary buffer take up the entire Emacs frame.
Given a prefix, will force an `article' buffer configuration."
  (interactive "P")
  (gnus-set-global-variables)
  (if arg
      (gnus-configure-windows 'article 'force)
    (gnus-configure-windows 'summary 'force)))

(defun gnus-summary-display-article (article &optional all-header)
  "Display ARTICLE in article buffer."
  (gnus-set-global-variables)
  (if (null article)
      nil
    (prog1
	(gnus-article-prepare article all-header)
      (gnus-summary-show-thread)
      (run-hooks 'gnus-select-article-hook)
      (gnus-summary-recenter)
      (gnus-summary-goto-subject article)
      ;; Successfully display article.
      (gnus-summary-update-line)
      (gnus-article-set-window-start 
       (cdr (assq article gnus-newsgroup-bookmarks)))
      t)))

(defun gnus-summary-select-article (&optional all-headers force pseudo article)
  "Select the current article.
If ALL-HEADERS is non-nil, show all header fields.  If FORCE is
non-nil, the article will be re-fetched even if it already present in
the article buffer.  If PSEUDO is non-nil, pseudo-articles will also
be displayed."
  (let ((article (or article (gnus-summary-article-number)))
	(all-headers (not (not all-headers))) ;Must be T or NIL.
	did) 
    (and (not pseudo) 
	 (gnus-summary-article-pseudo-p article)
	 (error "This is a pseudo-article."))
    (prog1
	(save-excursion
	  (set-buffer gnus-summary-buffer)
	  (if (or (null gnus-current-article)
		  (null gnus-article-current)
		  (null (get-buffer gnus-article-buffer))
		  (not (eq article (cdr gnus-article-current)))
		  (not (equal (car gnus-article-current) gnus-newsgroup-name))
		  force)
	      ;; The requested article is different from the current article.
	      (progn
		(gnus-summary-display-article article all-headers)
		(setq did article))
	    (if (or all-headers gnus-show-all-headers) 
		(gnus-article-show-all-headers))
	    nil))
      (if did 
	  (gnus-article-set-window-start 
	   (cdr (assq article gnus-newsgroup-bookmarks)))))))

(defun gnus-summary-set-current-mark (&optional current-mark)
  "Obsolete function."
  nil)

(defun gnus-summary-next-article (&optional unread subject backward)
  "Select the next article.
If UNREAD, only unread articles are selected.
If SUBJECT, only articles with SUBJECT are selected.
If BACKWARD, the previous article is selected instead of the next."
  (interactive "P")
  (gnus-set-global-variables)
  (let (header)
    (cond
     ;; Is there such an article?
     ((and (gnus-summary-search-forward unread subject backward)
	   (or (gnus-summary-display-article (gnus-summary-article-number))
	       (eq (gnus-summary-article-mark) gnus-canceled-mark)))
      (gnus-summary-position-point))
     ;; If not, we try the first unread, if that is wanted.
     ((and subject
	   gnus-auto-select-same
	   (or (gnus-summary-first-unread-article)
	       (eq (gnus-summary-article-mark) gnus-canceled-mark)))
      (gnus-summary-position-point)
      (gnus-message 6 "Wrapped"))
     ;; Try to get next/previous article not displayed in this group.
     ((and gnus-auto-extend-newsgroup
	   (not unread) (not subject))
      (gnus-summary-goto-article 
       (if backward (1- gnus-newsgroup-begin) (1+ gnus-newsgroup-end))
       nil t))
     ;; Go to next/previous group.
     (t
      (or (gnus-ephemeral-group-p gnus-newsgroup-name)
	  (gnus-summary-jump-to-group gnus-newsgroup-name))
      (let ((cmd last-command-char)
	    (group 
	     (if (eq gnus-keep-same-level 'best) 
		 (gnus-summary-best-group gnus-newsgroup-name)
	       (gnus-summary-search-group backward gnus-keep-same-level))))
	;; For some reason, the group window gets selected.  We change
	;; it back.  
	(select-window (get-buffer-window (current-buffer)))
	;; Keep just the event type of CMD.
					;(and (listp cmd) (setq cmd (car cmd)))
	;; Select next unread newsgroup automagically.
	(cond 
	 ((not gnus-auto-select-next)
	  (gnus-message 7 "No more%s articles" (if unread " unread" "")))
	 ((or (eq gnus-auto-select-next 'quietly)
	      (and (eq gnus-auto-select-next 'almost-quietly)
		   (gnus-summary-last-article-p)))
	  ;; Select quietly.
	  (if (gnus-ephemeral-group-p gnus-newsgroup-name)
	      (gnus-summary-exit)
	    (gnus-message 7 "No more%s articles (%s)..."
			  (if unread " unread" "") 
			  (if group (concat "selecting " group)
			    "exiting"))
	    (gnus-summary-next-group nil group backward)))
	 (t
	  (let ((keystrokes '(?\C-n ?\C-p))
		key)
	    (while (or (null key) (memq key keystrokes))
	      (gnus-message 
	       7 "No more%s articles%s" (if unread " unread" "")
	       (if (and group 
			(not (gnus-ephemeral-group-p gnus-newsgroup-name)))
		   (format " (Type %s for %s [%s])"
			   (single-key-description cmd) group
			   (car (gnus-gethash group gnus-newsrc-hashtb)))
		 (format " (Type %s to exit %s)"
			 (single-key-description cmd)
			 gnus-newsgroup-name)))
	      ;; Confirm auto selection.
	      (let* ((event (read-char-exclusive)))
		(setq key (if (listp event) (car event) event))
		(if (memq key keystrokes)
		    (let ((obuf (current-buffer)))
		      (switch-to-buffer gnus-group-buffer)
		      (and group
			   (gnus-group-jump-to-group group))
		      (condition-case ()
			  (execute-kbd-macro (char-to-string key))
			(error (ding) nil))
		      (setq group (gnus-group-group-name))
		      (switch-to-buffer obuf)))))
	    (if (equal key cmd)
		(if (or (not group)
			(gnus-ephemeral-group-p gnus-newsgroup-name))
		    (gnus-summary-exit)
		  (gnus-summary-next-group nil group backward))
	      (execute-kbd-macro (char-to-string key)))))))))))

(defun gnus-summary-next-unread-article ()
  "Select unread article after current one."
  (interactive)
  (gnus-summary-next-article t (and gnus-auto-select-same
				    (gnus-summary-article-subject))))

(defun gnus-summary-prev-article (&optional unread subject)
  "Select the article after the current one.
If UNREAD is non-nil, only unread articles are selected."
  (interactive "P")
  (gnus-summary-next-article unread subject t))

(defun gnus-summary-prev-unread-article ()
  "Select unred article before current one."
  (interactive)
  (gnus-summary-prev-article t (and gnus-auto-select-same
				    (gnus-summary-article-subject))))

(defun gnus-summary-next-page (&optional lines circular)
  "Show next page of selected article.
If end of article, select next article.
Argument LINES specifies lines to be scrolled up.
If CIRCULAR is non-nil, go to the start of the article instead of 
instead of selecting the next article when reaching the end of the
current article." 
  (interactive "P")
  (setq gnus-summary-buffer (current-buffer))
  (gnus-set-global-variables)
  (let ((article (gnus-summary-article-number))
	(endp nil))
    (gnus-configure-windows 'article)
    (if (or (null gnus-current-article)
	    (null gnus-article-current)
	    (/= article (cdr gnus-article-current))
	    (not (equal (car gnus-article-current) gnus-newsgroup-name)))
	;; Selected subject is different from current article's.
	(gnus-summary-display-article article)
      (gnus-eval-in-buffer-window
       gnus-article-buffer
       (setq endp (gnus-article-next-page lines)))
      (if endp
 	  (cond (circular
 		 (gnus-summary-beginning-of-article))
 		(lines
 		 (gnus-message 3 "End of message"))
 		((null lines)
 		 (gnus-summary-next-unread-article)))))
    (gnus-summary-recenter)
    (gnus-summary-position-point)))

(defun gnus-summary-prev-page (&optional lines)
  "Show previous page of selected article.
Argument LINES specifies lines to be scrolled down."
  (interactive "P")
  (gnus-set-global-variables)
  (let ((article (gnus-summary-article-number)))
    (gnus-configure-windows 'article)
    (if (or (null gnus-current-article)
	    (null gnus-article-current)
	    (/= article (cdr gnus-article-current))
	    (not (equal (car gnus-article-current) gnus-newsgroup-name)))
	;; Selected subject is different from current article's.
	(gnus-summary-display-article article)
      (gnus-summary-recenter)
      (gnus-eval-in-buffer-window gnus-article-buffer
				  (gnus-article-prev-page lines))))
  (gnus-summary-position-point))

(defun gnus-summary-scroll-up (lines)
  "Scroll up (or down) one line current article.
Argument LINES specifies lines to be scrolled up (or down if negative)."
  (interactive "p")
  (gnus-set-global-variables)
  (gnus-configure-windows 'article)
  (or (gnus-summary-select-article nil nil 'pseudo)
      (gnus-eval-in-buffer-window 
       gnus-article-buffer
       (cond ((> lines 0)
	      (if (gnus-article-next-page lines)
		  (gnus-message 3 "End of message")))
	     ((< lines 0)
	      (gnus-article-prev-page (- lines))))))
  (gnus-summary-recenter)
  (gnus-summary-position-point))

(defun gnus-summary-next-same-subject ()
  "Select next article which has the same subject as current one."
  (interactive)
  (gnus-set-global-variables)
  (gnus-summary-next-article nil (gnus-summary-article-subject)))

(defun gnus-summary-prev-same-subject ()
  "Select previous article which has the same subject as current one."
  (interactive)
  (gnus-set-global-variables)
  (gnus-summary-prev-article nil (gnus-summary-article-subject)))

(defun gnus-summary-next-unread-same-subject ()
  "Select next unread article which has the same subject as current one."
  (interactive)
  (gnus-set-global-variables)
  (gnus-summary-next-article t (gnus-summary-article-subject)))

(defun gnus-summary-prev-unread-same-subject ()
  "Select previous unread article which has the same subject as current one."
  (interactive)
  (gnus-set-global-variables)
  (gnus-summary-prev-article t (gnus-summary-article-subject)))

(defun gnus-summary-first-unread-article ()
  "Select the first unread article. 
Return nil if there are no unread articles."
  (interactive)
  (gnus-set-global-variables)
  (prog1
      (if (gnus-summary-first-subject t)
	  (progn
	    (gnus-summary-show-thread)
	    (gnus-summary-first-subject t)
	    (gnus-summary-display-article (gnus-summary-article-number))))
    (gnus-summary-position-point)))

(defun gnus-summary-best-unread-article ()
  "Select the unread article with the highest score."
  (interactive)
  (gnus-set-global-variables)
  (let ((best -1000000)
	(data gnus-newsgroup-data)
	article score)
    (while data
      (and (gnus-data-unread-p (car data))
	   (> (setq score 
		    (gnus-summary-article-score (gnus-data-number (car data))))
	      best)
	   (setq best score
		 article (gnus-data-number (car data))))
      (setq data (cdr data)))
    (if article
	(gnus-summary-goto-article article)
      (error "No unread articles"))
    (gnus-summary-position-point)))

(defun gnus-summary-goto-article (article &optional all-headers force)
  "Fetch ARTICLE and display it if it exists.
If ALL-HEADERS is non-nil, no header lines are hidden."
  (interactive
   (list
    (string-to-int
     (completing-read 
      "Article number: "
      (mapcar (lambda (number) (list (int-to-string number)))
	      gnus-newsgroup-limit)))
    current-prefix-arg
    t))
  (prog1
      (if (gnus-summary-goto-subject article force)
	  (gnus-summary-display-article article all-headers)
	(message "Couldn't go to article %s" article) nil)
    (gnus-summary-position-point)))

(defun gnus-summary-goto-last-article ()
  "Go to the previously read article."
  (interactive)
  (prog1
      (and gnus-last-article
	   (gnus-summary-goto-article gnus-last-article))
    (gnus-summary-position-point)))

(defun gnus-summary-pop-article (number)
  "Pop one article off the history and go to the previous.
NUMBER articles will be popped off."
  (interactive "p")
  (let (to)
    (setq gnus-newsgroup-history
	  (cdr (setq to (nthcdr number gnus-newsgroup-history))))
    (if to
	(gnus-summary-goto-article (car to))
      (error "Article history empty")))
  (gnus-summary-position-point))

;; Summary commands and functions for limiting the summary buffer.

(defun gnus-summary-limit-to-articles (n)
  "Limit the summary buffer to the next N articles.
If not given a prefix, use the process marked articles instead."
  (interactive "P")
  (gnus-set-global-variables)
  (prog1
      (let ((articles (gnus-summary-work-articles n)))
	(gnus-summary-limit articles))
    (gnus-summary-position-point)))

(defun gnus-summary-pop-limit (&optional total)
  "Restore the previous limit.
If given a prefix, remove all limits."
  (interactive "P")
  (gnus-set-global-variables)
  (prog2
      (if total (setq gnus-newsgroup-limits 
		      (list (mapcar (lambda (h) (mail-header-number h))
				    gnus-newsgroup-headers))))
      (gnus-summary-limit nil 'pop)
    (gnus-summary-position-point)))

(defun gnus-summary-limit-to-subject (subject)
  "Limit the summary buffer to articles that have subjects that match a regexp."
  (interactive "sRegexp: ")
  (when (not (equal "" subject))
    (prog1
	(let ((articles (gnus-summary-find-matching "subject" subject 'all)))
	  (or articles (error "Found no matches for \"%s\"" subject))
	  (gnus-summary-limit articles))
      (gnus-summary-position-point))))

(defalias 'gnus-summary-delete-marked-as-read 'gnus-summary-limit-to-unread)
(make-obsolete 
 'gnus-summary-delete-marked-as-read 'gnus-summary-limit-to-unread)

(defun gnus-summary-limit-to-unread (&optional all)
  "Limit the summary buffer to articles that are not marked as read.
If ALL is non-nil, limit strictly to unread articles."
  (interactive "P")
  (if all
      (gnus-summary-limit-to-marks (char-to-string gnus-unread-mark))
    (gnus-summary-limit-to-marks
     ;; Concat all the marks that say that an article is read and have
     ;; those removed.  
     (list gnus-del-mark gnus-read-mark gnus-ancient-mark
	   gnus-killed-mark gnus-kill-file-mark
	   gnus-low-score-mark gnus-expirable-mark
	   gnus-canceled-mark gnus-catchup-mark)
     'reverse)))

(defalias 'gnus-summary-delete-marked-with 'gnus-summary-limit-to-marks)
(make-obsolete 'gnus-summary-delete-marked-with 'gnus-summary-limit-to-marks)

(defun gnus-summary-limit-to-marks (marks &optional reverse)
  "Limit the summary buffer to articles that are marked with MARKS (e.g. \"DK\").
If REVERSE, limit the summary buffer to articles that are not marked
with MARKS.  MARKS can either be a string of marks or a list of marks. 
Returns how many articles were removed."
  (interactive "sMarks: ")
  (gnus-set-global-variables)
  (prog1
      (let ((data gnus-newsgroup-data)
	    (marks (if (listp marks) marks
		     (append marks nil))) ; Transform to list.
	    articles)
	(while data
	  (and (if reverse (not (memq (gnus-data-mark (car data)) marks))
		 (memq (gnus-data-mark (car data)) marks))
	       (setq articles (cons (gnus-data-number (car data)) articles)))
	  (setq data (cdr data)))
	(gnus-summary-limit articles))
    (gnus-summary-position-point)))

(defun gnus-summary-limit-to-score (&optional score)
  "Limit to articles with score at or above SCORE."
  (interactive "P")
  (gnus-set-global-variables)
  (setq score (if score
		  (prefix-numeric-value score)
		(or gnus-summary-default-score 0)))
  (let ((data gnus-newsgroup-data)
	articles)
    (while data
      (when (>= (gnus-summary-article-score (gnus-data-number (car data)))
		score)
	(push (gnus-data-number (car data)) articles))
      (setq data (cdr data)))
    (prog1
	(gnus-summary-limit articles)
      (gnus-summary-position-point))))

(defun gnus-summary-limit-include-dormant ()
  "Display all the hidden articles that are marked as dormant."
  (interactive)
  (gnus-set-global-variables)
  (or gnus-newsgroup-dormant 
      (error "There are no dormant articles in this group"))
  (prog1
      (gnus-summary-limit (append gnus-newsgroup-dormant gnus-newsgroup-limit))
    (gnus-summary-position-point)))

(defun gnus-summary-limit-exclude-dormant ()
  "Hide all dormant articles."
  (interactive)
  (gnus-set-global-variables)
  (prog1
      (gnus-summary-limit-to-marks (list gnus-dormant-mark) 'reverse)
    (gnus-summary-position-point)))

(defun gnus-summary-limit-exclude-childless-dormant ()
  "Hide all dormant articles that have no children."
  (interactive)
  (gnus-set-global-variables)
  (let ((data gnus-newsgroup-data)
	articles)
    ;; Find all articles that are either not dormant or have
    ;; children. 
    (while data
      (and (or (not (= (gnus-data-mark (car data)) gnus-dormant-mark))
	       (gnus-article-parent-p (gnus-data-number (car data))))
	   (setq articles (cons (gnus-data-number (car data))
				articles)))
      (setq data (cdr data)))
    ;; Do the limiting.
    (prog1
	(gnus-summary-limit articles)
      (gnus-summary-position-point))))
 
(defun gnus-summary-limit (articles &optional pop)
  (if pop
      ;; We pop the previous limit off the stack and use that.
      (setq articles (car gnus-newsgroup-limits)
	    gnus-newsgroup-limits (cdr gnus-newsgroup-limits))
    ;; We use the new limit, so we push the old limit on the stack. 
    (setq gnus-newsgroup-limits 
	  (cons gnus-newsgroup-limit gnus-newsgroup-limits)))
  ;; Set the limit.
  (setq gnus-newsgroup-limit articles)
  (let ((total (length gnus-newsgroup-data))
	(data (gnus-data-find-list (gnus-summary-article-number)))
	found)
    ;; This will do all the work of generating the new summary buffer
    ;; according to the new limit.
    (gnus-summary-prepare)
    ;; Try to return to the article you were at, or on in the
    ;; neighborhood.  
    (if data
	;; We try to find some article after the current one.
	(while data
	  (and (gnus-summary-goto-subject (gnus-data-number (car data)))
	       (setq data nil
		     found t))
	  (setq data (cdr data))))
    (or found
	;; If there is no data, that means that we were after the last
	;; article.  The same goes when we can't find any articles
	;; after the current one.
	(progn
	  (goto-char (point-max))
	  (gnus-summary-find-prev)))
    ;; We return how many articles were removed from the summary
    ;; buffer as a result of the new limit.
    (- total (length gnus-newsgroup-data))))

(defun gnus-summary-initial-limit ()
  "Figure out what the initial limit is supposed to be on group entry.
This entails weeding out unwanted dormants, low-scored articles,
fetch-old-headers verbiage, and so on."
  ;; Most groups have nothing to remove.
  (if (and (null gnus-newsgroup-dormant)
	   (not (eq gnus-fetch-old-headers 'some))
	   (null gnus-summary-expunge-below))
      () ; Do nothing.
    (setq gnus-newsgroup-limits 
	  (cons gnus-newsgroup-limit gnus-newsgroup-limits))
    (setq gnus-newsgroup-limit nil)
    (mapatoms
     (lambda (node)
       (if (null (car (symbol-value node)))
	   (let ((nodes (cdr (symbol-value node))))
	     (while nodes
	       (gnus-summary-limit-children (car nodes))
	       (setq nodes (cdr nodes))))))
     gnus-newsgroup-dependencies)
    (when (not gnus-newsgroup-limit)
      (setq gnus-newsgroup-limit (pop gnus-newsgroup-limits)))
    gnus-newsgroup-limit))

(defun gnus-summary-limit-children (thread)
  "Return 1 if this subthread is visible and 0 if it is not."
  ;; First we get the number of visible children to this thread.  This
  ;; is done by recursing down the thread using this function, so this
  ;; will really go down to a leaf article first, before slowly
  ;; working its way up towards the root.
  (let ((children (apply '+ (mapcar (lambda (th)
				      (gnus-summary-limit-children th))
				    (cdr thread))))
	(number (mail-header-number (car thread)))
	score)
    (if (or 
	 ;; If this article is dormant and has absolutely no visible
	 ;; children, then this article isn't visible.
	 (and (memq number gnus-newsgroup-dormant)
	      (= children 0))
	 ;; If this is a "fetch-old-headered" and there is only one
	 ;; visible child (or less), then we don't want this article. 
	 (and (eq gnus-fetch-old-headers 'some)
	      (memq number gnus-newsgroup-ancient)
	      (<= children 1))
	 ;; If we use expunging, and this article is really
	 ;; low-scored, then we don't want this article.
	 (when (and gnus-summary-expunge-below
		    (< (setq score 
			     (or (cdr (assq number gnus-newsgroup-scored)) 
				 gnus-summary-default-score))
		       gnus-summary-expunge-below))
	   ;; We increase the expunge-tally here, but that has
	   ;; nothing to do with the limits, really.
	   (incf gnus-newsgroup-expunged-tally)
	   ;; We also mark as read here, if that's wanted.
	   (when (< score gnus-summary-mark-below)
	     (setq gnus-newsgroup-unreads (delq number gnus-newsgroup-unreads))
	     (push (cons number gnus-low-score-mark) gnus-newsgroup-reads))
	   t))
	;; Nope, invisible article.
	0
      ;; Ok, this article is to be visible, so we add it to the limit
      ;; and return 1.
      (setq gnus-newsgroup-limit (cons number gnus-newsgroup-limit))
      1)))

;; Summary article oriented commands

(defun gnus-summary-refer-parent-article (n)
  "Refer parent article N times.
The difference between N and the number of articles fetched is returned."
  (interactive "p")
  (gnus-set-global-variables)
  (while 
      (and 
       (> n 0)
       (let* ((header (gnus-summary-article-header))
	      (ref 
	       ;; If we try to find the parent of the currently
	       ;; displayed article, then we take a look at the actual
	       ;; References header, since this is slightly more
	       ;; reliable than the References field we got from the
	       ;; server. 
	       (if (and (eq (mail-header-number header) 
			    (cdr gnus-article-current))
			(equal gnus-newsgroup-name 
			       (car gnus-article-current)))
		   (save-excursion
		     (set-buffer gnus-original-article-buffer)
		     (gnus-narrow-to-headers)
		     (prog1
			 (mail-fetch-field "references")
		       (widen)))
		 ;; It's not the current article, so we take a bet on
		 ;; the value we got from the server. 
		 (mail-header-references header))))
	 (if ref
	     (or (gnus-summary-refer-article (gnus-parent-id ref))
		 (gnus-message 1 "Couldn't find parent"))
	   (gnus-message 1 "No references in article %d"
			 (gnus-summary-article-number))
	   nil)))
    (setq n (1- n)))
  (gnus-summary-position-point)
  n)

(defun gnus-summary-refer-references ()
  "Fetch all articles mentioned in the References header.
Return how many articles were fetched."
  (interactive)
  (gnus-set-global-variables)
  (let ((ref (mail-header-references (gnus-summary-article-header)))
	(current (gnus-summary-article-number))
	(n 0))
    ;; For each Message-ID in the References header...
    (while (string-match "<[^>]*>" ref)
      (incf n)
      ;; ... fetch that article.
      (gnus-summary-refer-article 
       (prog1 (match-string 0 ref)
	 (setq ref (substring ref (match-end 0))))))
    (gnus-summary-goto-subject current)
    (gnus-summary-position-point)
    n))
    
(defun gnus-summary-refer-article (message-id)
  "Fetch an article specified by MESSAGE-ID."
  (interactive "sMessage-ID: ")
  (when (and (stringp message-id)
	     (not (zerop (length message-id))))
    ;; Construct the correct Message-ID if necessary.
    ;; Suggested by tale@pawl.rpi.edu.
    (unless (string-match "^<" message-id)
      (setq message-id (concat "<" message-id)))
    (unless (string-match ">$" message-id)
      (setq message-id (concat message-id ">")))
    (let ((header (car (gnus-gethash (downcase message-id)
				     gnus-newsgroup-dependencies))))
      (if header
	  ;; The article is present in the buffer, to we just go to it.
	  (gnus-summary-goto-article (mail-header-number header) nil t)
	;; We fetch the article
	(let ((gnus-override-method gnus-refer-article-method)
	      number)
	  ;; Start the special refer-article method, if necessary.
	  (when gnus-refer-article-method
	    (gnus-check-server gnus-refer-article-method))
	  ;; Fetch the header, and display the article.
	  (when (setq number (gnus-summary-insert-subject message-id))
	    (gnus-summary-select-article nil nil nil number)))))))

(defun gnus-summary-enter-digest-group ()
  "Enter a digest group based on the current article."
  (interactive)
  (gnus-set-global-variables)
  (gnus-summary-select-article)
  ;; We do not want a narrowed article.
  (gnus-summary-stop-page-breaking)
  (let ((name (format "%s-%d" 
		      (gnus-group-prefixed-name 
		       gnus-newsgroup-name (list 'nndoc "")) 
		      gnus-current-article))
	(ogroup gnus-newsgroup-name)
	(buf (current-buffer)))
    (if (gnus-group-read-ephemeral-group 
	 name (list 'nndoc name
		    (list 'nndoc-address (get-buffer gnus-article-buffer))
		    '(nndoc-article-type digest))
	 t)
	(setcdr (nthcdr 4 (gnus-get-info name))
		(list (list (cons 'to-group ogroup))))
      (switch-to-buffer buf)
      (gnus-set-global-variables)
      (gnus-configure-windows 'summary)
      (gnus-message 3 "Article not a digest?"))))

(defun gnus-summary-isearch-article (&optional regexp-p)
  "Do incremental search forward on the current article.
If REGEXP-P (the prefix) is non-nil, do regexp isearch."
  (interactive "P")
  (gnus-set-global-variables)
  (gnus-summary-select-article)
  (gnus-eval-in-buffer-window 
   gnus-article-buffer
   (goto-char (point-min))
   (isearch-forward regexp-p)))

(defun gnus-summary-search-article-forward (regexp &optional backward)
  "Search for an article containing REGEXP forward.
If BACKWARD, search backward instead."
  (interactive
   (list (read-string
	  (format "Search article %s (regexp%s): "
		  (if current-prefix-arg "backward" "forward")
		  (if gnus-last-search-regexp
		      (concat ", default " gnus-last-search-regexp)
		    "")))
	 current-prefix-arg))
  (gnus-set-global-variables)
  (if (string-equal regexp "")
      (setq regexp (or gnus-last-search-regexp ""))
    (setq gnus-last-search-regexp regexp))
  (if (gnus-summary-search-article regexp backward)
      (gnus-article-set-window-start 
       (cdr (assq (gnus-summary-article-number) gnus-newsgroup-bookmarks)))
    (error "Search failed: \"%s\"" regexp)))

(defun gnus-summary-search-article-backward (regexp)
  "Search for an article containing REGEXP backward."
  (interactive
   (list (read-string
	  (format "Search article backward (regexp%s): "
		  (if gnus-last-search-regexp
		      (concat ", default " gnus-last-search-regexp)
		    "")))))
  (gnus-summary-search-article-forward regexp 'backward))

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
    ;; First of all, search current article.
    ;; We don't want to read article again from NNTP server nor reset
    ;; current point.
    (gnus-summary-select-article)
    (gnus-message 9 "Searching article: %d..." gnus-current-article)
    (setq last gnus-current-article)
    (gnus-eval-in-buffer-window
     gnus-article-buffer
     (save-restriction
       (widen)
       ;; Begin search from current point.
       (setq found (funcall re-search regexp nil t))))
    ;; Then search next articles.
    (while (and (not found)
		(gnus-summary-display-article 
		 (if backward (gnus-summary-find-prev)
		   (gnus-summary-find-next))))
      (gnus-message 9 "Searching article: %d..." gnus-current-article)
      (gnus-eval-in-buffer-window
       gnus-article-buffer
       (save-restriction
	 (widen)
	 (goto-char (if backward (point-max) (point-min)))
	 (setq found (funcall re-search regexp nil t)))))
    (message "")
    ;; Adjust article pointer.
    (or (eq last gnus-current-article)
	(setq gnus-last-article last))
    ;; Return T if found such article.
    found))

(defun gnus-summary-find-matching (header regexp &optional backward unread
					  not-case-fold)
  "Return a list of all articles that match REGEXP on HEADER.
The search stars on the current article and goes forwards unless
BACKWARD is non-nil.  If BACKWARD is `all', do all articles.
If UNREAD is non-nil, only unread articles will
be taken into consideration.  If NOT-CASE-FOLD, case won't be folded
in the comparisons."
  (let ((data (if (eq backward 'all) gnus-newsgroup-data
		(gnus-data-find-list 
		 (gnus-summary-article-number) (gnus-data-list backward))))
	(func (intern (concat "gnus-header-" header)))
	(case-fold-search (not not-case-fold))
	articles d)
    (or (fboundp func) (error "%s is not a valid header" header))
    (while data
      (setq d (car data))
      (and (or (not unread)		; We want all articles...
	       (gnus-data-unread-p d))	; Or just unreads.
	   (vectorp (gnus-data-header d)) ; It's not a pseudo.
	   (string-match regexp (funcall func (gnus-data-header d))) ; Match.
	   (setq articles (cons (gnus-data-number d) articles))) ; Success!
      (setq data (cdr data)))
    (nreverse articles)))
    
(defun gnus-summary-execute-command (header regexp command &optional backward)
  "Search forward for an article whose HEADER matches REGEXP and execute COMMAND.
If HEADER is an empty string (or nil), the match is done on the entire
article.  If BACKWARD (the prefix) is non-nil, search backward instead."
  (interactive
   (list (let ((completion-ignore-case t))
	   (completing-read 
	    "Header name: "
	    (mapcar (lambda (string) (list string))
		    '("Number" "Subject" "From" "Lines" "Date"
		      "Message-ID" "Xref" "References"))
	    nil 'require-match))
	 (read-string "Regexp: ")
	 (read-key-sequence "Command: ")
	 current-prefix-arg))
  (gnus-set-global-variables)
  ;; Hidden thread subtrees must be searched as well.
  (gnus-summary-show-all-threads)
  ;; We don't want to change current point nor window configuration.
  (save-excursion
    (save-window-excursion
      (gnus-message 6 "Executing %s..." (key-description command))
      ;; We'd like to execute COMMAND interactively so as to give arguments.
      (gnus-execute header regexp
		    (` (lambda ()
			 (call-interactively '(, (key-binding command)))))
		    backward)
      (gnus-message 6 "Executing %s...done" (key-description command)))))

(defun gnus-summary-beginning-of-article ()
  "Scroll the article back to the beginning."
  (interactive)
  (gnus-set-global-variables)
  (gnus-summary-select-article)
  (gnus-configure-windows 'article)
  (gnus-eval-in-buffer-window
   gnus-article-buffer
   (widen)
   (goto-char (point-min))
   (and gnus-break-pages (gnus-narrow-to-page))))

(defun gnus-summary-end-of-article ()
  "Scroll to the end of the article."
  (interactive)
  (gnus-set-global-variables)
  (gnus-summary-select-article)
  (gnus-configure-windows 'article)
  (gnus-eval-in-buffer-window 
   gnus-article-buffer
   (widen)
   (goto-char (point-max))
   (recenter -3)
   (and gnus-break-pages (gnus-narrow-to-page))))

(defun gnus-summary-show-article (&optional arg)
  "Force re-fetching of the current article.
If ARG (the prefix) is non-nil, show the raw article without any
article massaging functions being run."
  (interactive "P")
  (gnus-set-global-variables)
  (if (not arg)
      ;; Select the article the normal way.
      (gnus-summary-select-article nil 'force)
    ;; Bind the article treatment functions to nil.
    (let ((gnus-have-all-headers t)
	  gnus-article-display-hook
	  gnus-article-prepare-hook
	  gnus-visual)
      (gnus-summary-select-article nil 'force)))
  (gnus-configure-windows 'article)
  (gnus-summary-position-point))

(defun gnus-summary-verbose-headers (&optional arg)
  "Toggle permanent full header display.
If ARG is a positive number, turn header display on.
If ARG is a negative number, turn header display off."
  (interactive "P")
  (gnus-set-global-variables)
  (gnus-summary-toggle-header arg)
  (setq gnus-show-all-headers
	(cond ((or (not (numberp arg))
		   (zerop arg))
	       (not gnus-show-all-headers))
	      ((natnump arg)
	       t))))

(defun gnus-summary-toggle-header (&optional arg)
  "Show the headers if they are hidden, or hide them if they are shown.
If ARG is a positive number, show the entire header.
If ARG is a negative number, hide the unwanted header lines."
  (interactive "P")
  (gnus-set-global-variables)
  (save-excursion
    (set-buffer gnus-article-buffer)
    (let* ((buffer-read-only nil)
	   (inhibit-point-motion-hooks t) 
	   (hidden (text-property-any 
		    (goto-char (point-min)) (search-forward "\n\n")
		    'invisible t))
	   e)
      (goto-char (point-min))
      (when (search-forward "\n\n" nil t)
	(delete-region (point-min) (1- (point))))
      (goto-char (point-min))
      (save-excursion 
	(set-buffer gnus-original-article-buffer)
	(goto-char (point-min))
	(setq e (1- (or (search-forward "\n\n" nil t) (point-max)))))
      (insert-buffer-substring gnus-original-article-buffer 1 e)
      (let ((hook (delq 'gnus-article-hide-headers-if-wanted
			(delq 'gnus-article-hide-headers
			      (copy-sequence gnus-article-display-hook)))))
	(run-hooks 'hook))
      (if (or (not hidden) (and (numberp arg) (< arg 0)))
	  (gnus-article-hide-headers)))))

(defun gnus-summary-show-all-headers ()
  "Make all header lines visible."
  (interactive)
  (gnus-set-global-variables)
  (gnus-article-show-all-headers))

(defun gnus-summary-toggle-mime (&optional arg)
  "Toggle MIME processing.
If ARG is a positive number, turn MIME processing on."
  (interactive "P")
  (gnus-set-global-variables)
  (setq gnus-show-mime
	(if (null arg) (not gnus-show-mime)
	  (> (prefix-numeric-value arg) 0)))
  (gnus-summary-select-article t 'force))

(defun gnus-summary-caesar-message (&optional arg)
  "Caesar rotate the current article by 13.
The numerical prefix specifies how manu places to rotate each letter
forward."
  (interactive "P")
  (gnus-set-global-variables)
  (gnus-summary-select-article)
  (let ((mail-header-separator ""))
    (gnus-eval-in-buffer-window 
     gnus-article-buffer
     (save-restriction
       (widen)
       (let ((start (window-start)))
	 (news-caesar-buffer-body arg)
	 (set-window-start (get-buffer-window (current-buffer)) start))))))

(defun gnus-summary-stop-page-breaking ()
  "Stop page breaking in the current article."
  (interactive)
  (gnus-set-global-variables)
  (gnus-summary-select-article)
  (gnus-eval-in-buffer-window gnus-article-buffer (widen)))

;; Suggested by Brian Edmonds <bedmonds@prodigy.bc.ca>.

(defun gnus-summary-move-article (&optional n to-newsgroup select-method)
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
	(prefix (gnus-group-real-prefix gnus-newsgroup-name))
	art-group to-method sel-met)
    (if (and (not to-newsgroup) (not select-method))
	(setq to-newsgroup
	      (completing-read 
	       (format "Where do you want to move %s? %s"
		       (if (> (length articles) 1)
			   (format "these %d articles" (length articles))
			 "this article")
		       (if gnus-current-move-group
			   (format "(default %s) " gnus-current-move-group)
			 ""))
	       gnus-active-hashtb nil nil prefix)))
    (if to-newsgroup
        (progn
          (if (or (string= to-newsgroup "") (string= to-newsgroup prefix))
              (setq to-newsgroup (or gnus-current-move-group "")))
          (or (gnus-active to-newsgroup)
	      (gnus-activate-group to-newsgroup)
              (error "No such group: %s" to-newsgroup))
          (setq gnus-current-move-group to-newsgroup)))
    (setq to-method (if select-method (list select-method "")
		      (gnus-find-method-for-group to-newsgroup)))
    (or (gnus-check-backend-function 'request-accept-article (car to-method))
	(error "%s does not support article copying" (car to-method)))
    (or (gnus-check-server to-method)
	(error "Can't open server %s" (car to-method)))
    (gnus-message 6 "Moving to %s: %s..." 
		  (or select-method to-newsgroup) articles)
    (while articles
      (if (setq art-group
		(gnus-request-move-article 
		 (car articles)		; Article to move
		 gnus-newsgroup-name	; From newsgrouo
		 (nth 1 (gnus-find-method-for-group 
			 gnus-newsgroup-name)) ; Server
		 (list 'gnus-request-accept-article 
		       (if select-method
			   (list 'quote select-method)
			 to-newsgroup)
		       (not (cdr articles))) ; Accept form
		 (not (cdr articles))))	; Only save nov last time
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
		 (article (car articles)))
	    (gnus-summary-goto-subject article)
	    (gnus-summary-mark-article article gnus-canceled-mark)
	    ;; Update the group that has been moved to.
	    (if (not info)
		()			; This group does not exist yet.
	      (if (not (memq article gnus-newsgroup-unreads))
		  (gnus-info-set-read 
		   info (gnus-add-to-range (gnus-info-read info) 
					   (list (cdr art-group)))))

	      ;; Copy any marks over to the new group.
	      (let ((marks '((tick . gnus-newsgroup-marked)
			     (dormant . gnus-newsgroup-dormant)
			     (expire . gnus-newsgroup-expirable)
			     (bookmark . gnus-newsgroup-bookmarks)
			     (reply . gnus-newsgroup-replied)))
		    (to-article (cdr art-group)))

		;; See whether the article is to be put in the cache.
		(when gnus-use-cache
		  (gnus-cache-possibly-enter-article 
		   (gnus-info-group info) to-article
		   (let ((header (copy-sequence
				  (gnus-summary-article-header article))))
		     (mail-header-set-number header to-article)
		     header)
		   (memq article gnus-newsgroup-marked)
		   (memq article gnus-newsgroup-dormant)
		   (memq article gnus-newsgroup-unreads)))

		(while marks
		  (if (memq article (symbol-value (cdr (car marks))))
		      (gnus-add-marked-articles 
		       (gnus-info-group info) (car (car marks))
		       (list to-article) info))
		  (setq marks (cdr marks)))))
	    ;; Update marks.
	    (setq gnus-newsgroup-marked (delq article gnus-newsgroup-marked))
	    (setq gnus-newsgroup-unreads (delq article gnus-newsgroup-unreads))
	    (setq gnus-newsgroup-dormant
		  (delq article gnus-newsgroup-dormant))
	    (setq gnus-newsgroup-reads
		  (cons (cons article gnus-canceled-mark)
			gnus-newsgroup-reads)))
	(gnus-message 1 "Couldn't move article %s" (car articles)))
      (gnus-summary-remove-process-mark (car articles))
      (setq articles (cdr articles)))
    (gnus-set-mode-line 'summary)))

(defun gnus-summary-respool-article (&optional n respool-method)
  "Respool the current article.
The article will be squeezed through the mail spooling process again,
which means that it will be put in some mail newsgroup or other
depending on `nnmail-split-methods'.
If N is a positive number, respool the N next articles.
If N is a negative number, respool the N previous articles.
If N is nil and any articles have been marked with the process mark,
respool those articles instead.

Respooling can be done both from mail groups and \"real\" newsgroups.
In the former case, the articles in question will be moved from the
current group into whatever groups they are destined to.  In the
latter case, they will be copied into the relevant groups."
  (interactive "P")
  (gnus-set-global-variables)
  (let ((respool-methods (gnus-methods-using 'respool))
	(methname 
	 (symbol-name (car (gnus-find-method-for-group gnus-newsgroup-name)))))
    (or respool-method
	(setq respool-method
	      (completing-read
	       "What method do you want to use when respooling? "
	       respool-methods nil t methname)))
    (or (string= respool-method "")
	(if (assoc (symbol-name
		    (car (gnus-find-method-for-group gnus-newsgroup-name)))
		   respool-methods)
	    (gnus-summary-move-article n nil (intern respool-method))
	  (gnus-summary-copy-article n nil (intern respool-method))))))

;; Suggested by gregj@unidata.com (Gregory J. Grubbs).
(defun gnus-summary-copy-article (&optional n to-newsgroup select-method)
  "Move the current article to a different newsgroup.
If N is a positive number, move the N next articles.
If N is a negative number, move the N previous articles.
If N is nil and any articles have been marked with the process mark,
move those articles instead.
If TO-NEWSGROUP is string, do not prompt for a newsgroup to move to. 
If SELECT-METHOD is symbol, do not move to a specific newsgroup, but
re-spool using this method.
For this function to work, the newsgroup that you want to move to have
to support the `request-move' and `request-accept'
functions. (Ie. mail newsgroups at present.)"
  (interactive "P")
  (gnus-set-global-variables)
  (let ((articles (gnus-summary-work-articles n))
	(copy-buf (get-buffer-create "*copy work*"))
	(prefix (gnus-group-real-prefix gnus-newsgroup-name))
	art-group to-method)
    (buffer-disable-undo copy-buf)
    (if (and (not to-newsgroup) (not select-method))
	(setq to-newsgroup
	      (completing-read 
	       (format "Where do you want to copy %s? %s"
		       (if (> (length articles) 1)
			   (format "these %d articles" (length articles))
			 "this article")
		       (if gnus-current-move-group
			   (format "(default %s) " gnus-current-move-group)
			 ""))
	       gnus-active-hashtb nil nil prefix)))
    (if to-newsgroup
        (progn
          (if (or (string= to-newsgroup "") (string= to-newsgroup prefix))
              (setq to-newsgroup (or gnus-current-move-group "")))
          (or (gnus-active to-newsgroup)
	      (gnus-activate-group to-newsgroup)
              (error "No such group: %s" to-newsgroup))
          (setq gnus-current-move-group to-newsgroup)))
    (setq to-method (if select-method (list select-method "")
		      (gnus-find-method-for-group to-newsgroup)))
    (or (gnus-check-backend-function 'request-accept-article (car to-method))
	(error "%s does not support article copying" (car to-method)))
    (or (gnus-check-server to-method)
	(error "Can't open server %s" (car to-method)))
    (while articles
      (gnus-message 6 "Copying to %s: %s..." 
		    (or select-method to-newsgroup) articles)
      (if (setq art-group
		(save-excursion
		  (set-buffer copy-buf)
		  (gnus-request-article-this-buffer
		   (car articles) gnus-newsgroup-name)
		  (gnus-request-accept-article
		   (if select-method select-method to-newsgroup)
		   (not (cdr articles)))))
	  (let* ((entry 
		  (or
		   (gnus-gethash (car art-group) gnus-newsrc-hashtb)
		   (gnus-gethash 
		    (gnus-group-prefixed-name 
		     (car art-group) 
		     (if select-method (list select-method "")
		       (gnus-find-method-for-group to-newsgroup)))
		    gnus-newsrc-hashtb)))
		 (info (nth 2 entry))
		 (article (car articles)))
	    ;; We copy the info over to the new group.
	    (if (not info)
		()			; This group does not exist (yet).
	      (if (not (memq article gnus-newsgroup-unreads))
		  (gnus-info-set-read 
		   info (gnus-add-to-range (gnus-info-read info) 
					   (list (cdr art-group)))))

	      ;; Copy any marks over to the new group.
	      (let ((marks '((tick . gnus-newsgroup-marked)
			     (dormant . gnus-newsgroup-dormant)
			     (expire . gnus-newsgroup-expirable)
			     (bookmark . gnus-newsgroup-bookmarks)
			     (reply . gnus-newsgroup-replied)))
		    (to-article (cdr art-group)))

	      ;; See whether the article is to be put in the cache.
	      (when gnus-use-cache
		(gnus-cache-possibly-enter-article 
		 (gnus-info-group info) to-article 
		 (let ((header (copy-sequence
				(gnus-summary-article-header article))))
		   (mail-header-set-number header to-article)
		   header)
		 (memq article gnus-newsgroup-marked)
		 (memq article gnus-newsgroup-dormant)
		 (memq article gnus-newsgroup-unreads)))

	      (while marks
		(if (memq article (symbol-value (cdr (car marks))))
		    (gnus-add-marked-articles 
		     (gnus-info-group info) (car (car marks)) 
		     (list to-article) info))
		(setq marks (cdr marks))))))
	(gnus-message 1 "Couldn't copy article %s" (car articles)))
      (gnus-summary-remove-process-mark (car articles))
      (setq articles (cdr articles)))
    (kill-buffer copy-buf)))

(defun gnus-summary-import-article (file)
  "Import a random file into a mail newsgroup."
  (interactive "fImport file: ")
  (gnus-set-global-variables)
  (let ((group gnus-newsgroup-name)
	atts lines)
    (or (gnus-check-backend-function 'request-accept-article group)
	(error "%s does not support article importing" group))
    (or (file-readable-p file)
	(not (file-regular-p file))
	(error "Can't read %s" file))
    (save-excursion
      (set-buffer (get-buffer-create " *import file*"))
      (buffer-disable-undo (current-buffer))
      (erase-buffer)
      (insert-file-contents file)
      (goto-char (point-min))
      (if (nnheader-article-p)
	  ()
	(setq atts (file-attributes file)
	      lines (count-lines (point-min) (point-max)))
	(insert "From: " (read-string "From: ") "\n"
		"Subject: " (read-string "Subject: ") "\n"
		"Date: " (current-time-string (nth 5 atts)) "\n"
		"Message-ID: " (gnus-inews-message-id) "\n"
		"Lines: " (int-to-string lines) "\n"
		"Chars: " (int-to-string (nth 7 atts)) "\n\n"))
      (gnus-request-accept-article group t)
      (kill-buffer (current-buffer)))))

(defun gnus-summary-expire-articles ()
  "Expire all articles that are marked as expirable in the current group."
  (interactive)
  (gnus-set-global-variables)
  (when (gnus-check-backend-function 
	 'request-expire-articles gnus-newsgroup-name)
    ;; This backend supports expiry.
    (let* ((total (gnus-group-total-expirable-p gnus-newsgroup-name))
	   (expirable (if total
			  (gnus-list-of-read-articles gnus-newsgroup-name)
			(setq gnus-newsgroup-expirable
			      (sort gnus-newsgroup-expirable '<))))
	   es)
      (when expirable
	;; There are expirable articles in this group, so we run them
	;; through the expiry process.
	(gnus-message 6 "Expiring articles...")
	;; The list of articles that weren't expired is returned.
	(setq es (gnus-request-expire-articles expirable gnus-newsgroup-name))
	(or total (setq gnus-newsgroup-expirable es))
	;; We go through the old list of expirable, and mark all
	;; really expired articles as non-existant.
	(unless (eq es expirable)	;If nothing was expired, we don't mark.
	  (let ((gnus-use-cache nil))
	    (while expirable
	      (unless (memq (car expirable) es)
		(when (gnus-data-find (car expirable))
		  (gnus-summary-mark-article
		   (car expirable) gnus-canceled-mark)))
	      (setq expirable (cdr expirable)))))
	(gnus-message 6 "Expiring articles...done")))))

(defun gnus-summary-expire-articles-now ()
  "Expunge all expirable articles in the current group.
This means that *all* articles that are marked as expirable will be
deleted forever, right now."
  (interactive)
  (gnus-set-global-variables)
  (or gnus-expert-user
      (gnus-y-or-n-p
       "Are you really, really, really sure you want to expunge? ")
      (error "Phew!"))
  (let ((nnmail-expiry-wait -1)
	(nnmail-expiry-wait-function nil))
    (gnus-summary-expire-articles)))

;; Suggested by Jack Vinson <vinson@unagi.cis.upenn.edu>.
(defun gnus-summary-delete-article (&optional n)
  "Delete the N next (mail) articles.
This command actually deletes articles.  This is not a marking
command.  The article will disappear forever from your life, never to
return. 
If N is negative, delete backwards.
If N is nil and articles have been marked with the process mark,
delete these instead."
  (interactive "P")
  (gnus-set-global-variables)
  (or (gnus-check-backend-function 'request-expire-articles 
				   gnus-newsgroup-name)
      (error "The current newsgroup does not support article deletion."))
  ;; Compute the list of articles to delete.
  (let ((articles (gnus-summary-work-articles n))
	not-deleted)
    (if (and gnus-novice-user
	     (not (gnus-y-or-n-p 
		   (format "Do you really want to delete %s forever? "
			   (if (> (length articles) 1) "these articles"
			     "this article")))))
	()
      ;; Delete the articles.
      (setq not-deleted (gnus-request-expire-articles 
			 articles gnus-newsgroup-name 'force))
      (while articles
	(gnus-summary-remove-process-mark (car articles))	
	;; The backend might not have been able to delete the article
	;; after all.  
	(or (memq (car articles) not-deleted)
	    (gnus-summary-mark-article (car articles) gnus-canceled-mark))
	(setq articles (cdr articles))))
    (gnus-summary-position-point)
    (gnus-set-mode-line 'summary)
    not-deleted))

(defun gnus-summary-edit-article (&optional force)
  "Enter into a buffer and edit the current article.
This will have permanent effect only in mail groups.
If FORCE is non-nil, allow editing of articles even in read-only
groups."
  (interactive "P")
  (gnus-set-global-variables)
  (when (and (not force)
	     (gnus-group-read-only-p))
    (error "The current newsgroup does not support article editing."))
  (gnus-summary-select-article t)
  (gnus-configure-windows 'article)
  (select-window (get-buffer-window gnus-article-buffer))
  (gnus-message 6 "C-c C-c to end edits")
  (setq buffer-read-only nil)
  (text-mode)
  (use-local-map (copy-keymap (current-local-map)))
  (local-set-key "\C-c\C-c" 'gnus-summary-edit-article-done)
  (buffer-enable-undo)
  (widen)
  (goto-char (point-min))
  (search-forward "\n\n" nil t))

(defun gnus-summary-edit-article-done ()
  "Make edits to the current article permanent."
  (interactive)
  (if (gnus-group-read-only-p)
      (progn
	(gnus-summary-edit-article-postpone)
	(message "The current newsgroup does not support article editing.")
	(ding))
    (let ((buf (buffer-substring-no-properties (point-min) (point-max))))
      (erase-buffer)
      (insert buf)
      (if (not (gnus-request-replace-article 
		(cdr gnus-article-current) (car gnus-article-current) 
		(current-buffer)))
	  (error "Couldn't replace article.")
	(gnus-article-mode)
	(use-local-map gnus-article-mode-map)
	(setq buffer-read-only t)
	(buffer-disable-undo (current-buffer))
	(gnus-configure-windows 'summary))
      (and (gnus-visual-p 'summary-highlight 'highlight)
	   (run-hooks 'gnus-visual-mark-article-hook)))))

(defun gnus-summary-edit-article-postpone ()
  "Postpone changes to the current article."
  (interactive)
  (gnus-article-mode)
  (use-local-map gnus-article-mode-map)
  (setq buffer-read-only t)
  (buffer-disable-undo (current-buffer))
  (gnus-configure-windows 'summary)
  (and (gnus-visual-p 'summary-highlight 'highlight)
       (run-hooks 'gnus-visual-mark-article-hook)))

(defun gnus-summary-respool-query ()
  "Query where the respool algorithm would put this article."
  (interactive)
  (gnus-set-global-variables)
  (gnus-summary-select-article)
  (save-excursion
    (set-buffer gnus-article-buffer)
    (save-restriction
      (goto-char (point-min))
      (search-forward "\n\n")
      (narrow-to-region (point-min) (point))
      (pp-eval-expression
       (list 'quote (mapcar 'car (nnmail-article-group 'identity)))))))

;; Summary score commands.

;; Suggested by boubaker@cenatls.cena.dgac.fr.

(defun gnus-summary-raise-score (n)
  "Raise the score of the current article by N."
  (interactive "p")
  (gnus-set-global-variables)
  (gnus-summary-set-score (+ (gnus-summary-article-score) n)))

(defun gnus-summary-set-score (n)
  "Set the score of the current article to N."
  (interactive "p")
  (gnus-set-global-variables)
  (save-excursion
    (gnus-summary-show-thread)
    (let ((buffer-read-only nil))
      ;; Set score.
      (gnus-summary-update-mark
       (if (= n (or gnus-summary-default-score 0)) ? 
	 (if (< n (or gnus-summary-default-score 0)) 
	     gnus-score-below-mark gnus-score-over-mark)) 'score))
    (let* ((article (gnus-summary-article-number))
	   (score (assq article gnus-newsgroup-scored)))
      (if score (setcdr score n)
	(setq gnus-newsgroup-scored 
	      (cons (cons article n) gnus-newsgroup-scored))))
    (gnus-summary-update-line)))

(defun gnus-summary-current-score ()
  "Return the score of the current article."
  (interactive)
  (gnus-set-global-variables)
  (message "%s" (gnus-summary-article-score)))

;; Summary marking commands.

(defun gnus-summary-kill-same-subject-and-select (&optional unmark)
  "Mark articles which has the same subject as read, and then select the next.
If UNMARK is positive, remove any kind of mark.
If UNMARK is negative, tick articles."
  (interactive "P")
  (gnus-set-global-variables)
  (if unmark
      (setq unmark (prefix-numeric-value unmark)))
  (let ((count
	 (gnus-summary-mark-same-subject
	  (gnus-summary-article-subject) unmark)))
    ;; Select next unread article.  If auto-select-same mode, should
    ;; select the first unread article.
    (gnus-summary-next-article t (and gnus-auto-select-same
				      (gnus-summary-article-subject)))
    (gnus-message 7 "%d article%s marked as %s"
		  count (if (= count 1) " is" "s are")
		  (if unmark "unread" "read"))))

(defun gnus-summary-kill-same-subject (&optional unmark)
  "Mark articles which has the same subject as read. 
If UNMARK is positive, remove any kind of mark.
If UNMARK is negative, tick articles."
  (interactive "P")
  (gnus-set-global-variables)
  (if unmark
      (setq unmark (prefix-numeric-value unmark)))
  (let ((count
	 (gnus-summary-mark-same-subject
	  (gnus-summary-article-subject) unmark)))
    ;; If marked as read, go to next unread subject.
    (if (null unmark)
	;; Go to next unread subject.
	(gnus-summary-next-subject 1 t))
    (gnus-message 7 "%d articles are marked as %s"
		  count (if unmark "unread" "read"))))

(defun gnus-summary-mark-same-subject (subject &optional unmark)
  "Mark articles with same SUBJECT as read, and return marked number.
If optional argument UNMARK is positive, remove any kinds of marks.
If optional argument UNMARK is negative, mark articles as unread instead."
  (let ((count 1))
    (save-excursion
      (cond 
       ((null unmark)			; Mark as read.
	(while (and 
		(progn
		  (gnus-summary-mark-article-as-read gnus-killed-mark)
		  (gnus-summary-show-thread) t)
		(gnus-summary-find-subject subject))
	  (setq count (1+ count))))
       ((> unmark 0)			; Tick.
	(while (and
		(progn
		  (gnus-summary-mark-article-as-unread gnus-ticked-mark)
		  (gnus-summary-show-thread) t)
		(gnus-summary-find-subject subject))
	  (setq count (1+ count))))
       (t				; Mark as unread.
	(while (and
		(progn
		  (gnus-summary-mark-article-as-unread gnus-unread-mark)
		  (gnus-summary-show-thread) t)
		(gnus-summary-find-subject subject))
	  (setq count (1+ count)))))
      (gnus-set-mode-line 'summary)
      ;; Return the number of marked articles.
      count)))

(defun gnus-summary-mark-as-processable (n &optional unmark)
  "Set the process mark on the next N articles.
If N is negative, mark backward instead.  If UNMARK is non-nil, remove
the process mark instead.  The difference between N and the actual
number of articles marked is returned."
  (interactive "p")
  (gnus-set-global-variables)
  (let ((backward (< n 0))
	(n (abs n)))
    (while (and 
	    (> n 0)
	    (if unmark
		(gnus-summary-remove-process-mark
		 (gnus-summary-article-number))
	      (gnus-summary-set-process-mark (gnus-summary-article-number)))
	    (zerop (gnus-summary-next-subject (if backward -1 1) nil t)))
      (setq n (1- n)))
    (if (/= 0 n) (gnus-message 7 "No more articles"))
    (gnus-summary-recenter)
    (gnus-summary-position-point)
    n))

(defun gnus-summary-unmark-as-processable (n)
  "Remove the process mark from the next N articles.
If N is negative, mark backward instead.  The difference between N and
the actual number of articles marked is returned."
  (interactive "p")
  (gnus-set-global-variables)
  (gnus-summary-mark-as-processable n t))

(defun gnus-summary-unmark-all-processable ()
  "Remove the process mark from all articles."
  (interactive)
  (gnus-set-global-variables)
  (save-excursion
    (while gnus-newsgroup-processable
      (gnus-summary-remove-process-mark (car gnus-newsgroup-processable))))
  (gnus-summary-position-point))

(defun gnus-summary-mark-as-expirable (n)
  "Mark N articles forward as expirable.
If N is negative, mark backward instead.  The difference between N and
the actual number of articles marked is returned."
  (interactive "p")
  (gnus-set-global-variables)
  (gnus-summary-mark-forward n gnus-expirable-mark))

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
  (gnus-set-global-variables)
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
			 (goto-char (point-min))
			 (search-forward "\n\n" nil t)
			 (point)))
		  (point))))
	 gnus-newsgroup-bookmarks))
  (gnus-message 6 "A bookmark has been added to the current article."))

(defun gnus-summary-remove-bookmark (article)
  "Remove the bookmark from the current article."
  (interactive (list (gnus-summary-article-number)))
  (gnus-set-global-variables)
  ;; Remove old bookmark, if one exists.
  (let ((old (assq article gnus-newsgroup-bookmarks)))
    (if old 
	(progn
	  (setq gnus-newsgroup-bookmarks 
		(delq old gnus-newsgroup-bookmarks))
	  (gnus-message 6 "Removed bookmark."))
      (gnus-message 6 "No bookmark in current article."))))

;; Suggested by Daniel Quinlan <quinlan@best.com>.
(defun gnus-summary-mark-as-dormant (n)
  "Mark N articles forward as dormant.
If N is negative, mark backward instead.  The difference between N and
the actual number of articles marked is returned."
  (interactive "p")
  (gnus-set-global-variables)
  (gnus-summary-mark-forward n gnus-dormant-mark))

(defun gnus-summary-set-process-mark (article)
  "Set the process mark on ARTICLE and update the summary line."
  (setq gnus-newsgroup-processable (cons article gnus-newsgroup-processable))
  (let ((buffer-read-only nil))
    (if (gnus-summary-goto-subject article)
	(progn
	  (gnus-summary-show-thread)
	  (gnus-summary-update-mark gnus-process-mark 'replied)
	  t))))

(defun gnus-summary-remove-process-mark (article)
  "Remove the process mark from ARTICLE and update the summary line."
  (setq gnus-newsgroup-processable (delq article gnus-newsgroup-processable))
  (let ((buffer-read-only nil))
    (if (gnus-summary-goto-subject article)
	(progn
	  (gnus-summary-show-thread)
	  (gnus-summary-update-mark ?  'replied)
	  (if (memq article gnus-newsgroup-replied) 
	      (gnus-summary-update-mark gnus-replied-mark 'replied))
	  t))))

(defun gnus-summary-mark-forward (n &optional mark no-expire)
  "Mark N articles as read forwards.
If N is negative, mark backwards instead.
Mark with MARK.  If MARK is ? , ?! or ??, articles will be
marked as unread. 
The difference between N and the actual number of articles marked is
returned."
  (interactive "p")
  (gnus-set-global-variables)
  (let ((backward (< n 0))
	(gnus-summary-goto-unread
	 (and gnus-summary-goto-unread
	      (not (memq mark (list gnus-unread-mark
				    gnus-ticked-mark gnus-dormant-mark)))))
	(n (abs n))
	(mark (or mark gnus-del-mark)))
    (while (and (> n 0)
		(gnus-summary-mark-article nil mark no-expire)
		(zerop (gnus-summary-next-subject 
			(if backward -1 1) gnus-summary-goto-unread t)))
      (setq n (1- n)))
    (if (/= 0 n) (gnus-message 7 "No more %sarticles" (if mark "" "unread ")))
    (gnus-summary-recenter)
    (gnus-summary-position-point)
    (gnus-set-mode-line 'summary)
    n))

(defun gnus-summary-mark-article-as-read (mark)
  "Mark the current article quickly as read with MARK."
  (let ((article (gnus-summary-article-number)))
    (setq gnus-newsgroup-unreads (delq article gnus-newsgroup-unreads))
    (setq gnus-newsgroup-marked (delq article gnus-newsgroup-marked))
    (setq gnus-newsgroup-dormant (delq article gnus-newsgroup-dormant))
    (setq gnus-newsgroup-reads
	  (cons (cons article mark) gnus-newsgroup-reads))
    ;; Possibly remove from cache, if that is used. 
    (and gnus-use-cache (gnus-cache-enter-remove-article article))
    (and gnus-newsgroup-auto-expire 
	 (or (= mark gnus-killed-mark) (= mark gnus-del-mark)
	     (= mark gnus-catchup-mark) (= mark gnus-low-score-mark)
	     (= mark gnus-read-mark) (= mark gnus-souped-mark))
	 (progn
	   (setq mark gnus-expirable-mark)
	   (setq gnus-newsgroup-expirable 
		 (cons article gnus-newsgroup-expirable))))
    ;; Fix the mark.
    (gnus-summary-update-mark mark 'unread)
    t))

(defun gnus-summary-mark-article-as-unread (mark)
  "Mark the current article quickly as unread with MARK."
  (let ((article (gnus-summary-article-number)))
    (setq gnus-newsgroup-marked (delq article gnus-newsgroup-marked))
    (setq gnus-newsgroup-dormant (delq article gnus-newsgroup-dormant))
    (setq gnus-newsgroup-expirable (delq article gnus-newsgroup-expirable))
    (setq gnus-newsgroup-reads (delq article gnus-newsgroup-reads))
    (cond ((= mark gnus-ticked-mark)
	   (push article gnus-newsgroup-marked))
	  ((= mark gnus-dormant-mark)
	   (push article gnus-newsgroup-dormant))
	  (t     
	   (push article gnus-newsgroup-unreads)))
    (setq gnus-newsgroup-reads
	  (delq (assq article gnus-newsgroup-reads)
		gnus-newsgroup-reads))

    ;; See whether the article is to be put in the cache.
    (and gnus-use-cache
	 (vectorp (gnus-summary-article-header article))
	 (save-excursion
	   (gnus-cache-possibly-enter-article 
	    gnus-newsgroup-name article 
	    (gnus-summary-article-header article)
	    (= mark gnus-ticked-mark)
	    (= mark gnus-dormant-mark) (= mark gnus-unread-mark))))

    ;; Fix the mark.
    (gnus-summary-update-mark mark 'unread)
    t))

(defun gnus-summary-mark-article (&optional article mark no-expire)
  "Mark ARTICLE with MARK.  MARK can be any character.
Four MARK strings are reserved: `? ' (unread), `?!' (ticked), `??'
(dormant) and `?E' (expirable).
If MARK is nil, then the default character `?D' is used.
If ARTICLE is nil, then the article on the current line will be
marked." 
  ;; The mark might be a string.
  (and (stringp mark)
       (setq mark (aref mark 0)))
  ;; If no mark is given, then we check auto-expiring.
  (and (not no-expire)
       gnus-newsgroup-auto-expire 
       (or (not mark)
	   (and (numberp mark) 
		(or (= mark gnus-killed-mark) (= mark gnus-del-mark)
		    (= mark gnus-catchup-mark) (= mark gnus-low-score-mark)
		    (= mark gnus-read-mark) (= mark gnus-souped-mark))))
       (setq mark gnus-expirable-mark))
  (let* ((mark (or mark gnus-del-mark))
	 (article (or article (gnus-summary-article-number))))
    (or article (error "No article on current line"))
    (if (or (= mark gnus-unread-mark) 
	    (= mark gnus-ticked-mark) 
	    (= mark gnus-dormant-mark))
	(gnus-mark-article-as-unread article mark)
      (gnus-mark-article-as-read article mark))

    ;; See whether the article is to be put in the cache.
    (and gnus-use-cache
	 (not (= mark gnus-canceled-mark))
	 (vectorp (gnus-summary-article-header article))
	 (save-excursion
	   (gnus-cache-possibly-enter-article 
	    gnus-newsgroup-name article 
	    (gnus-summary-article-header article)
	    (= mark gnus-ticked-mark)
	    (= mark gnus-dormant-mark) (= mark gnus-unread-mark))))

    (if (gnus-summary-goto-subject article nil t)
	(let ((buffer-read-only nil))
	  (gnus-summary-show-thread)
	  ;; Fix the mark.
	  (gnus-summary-update-mark mark 'unread)
	  t))))

(defun gnus-summary-update-mark (mark type)
  (beginning-of-line)
  (let ((forward (cdr (assq type gnus-summary-mark-positions)))
	(buffer-read-only nil)
	plist)
    (when forward
      ;; Go to the right position on the line.
      (forward-char forward)
      ;; Replace the old mark with the new mark.
      (subst-char-in-region (point) (1+ (point)) (following-char) mark)
      ;; Optionally update the marks by some user rule.
      (when (eq type 'unread)
	(gnus-data-set-mark 
	 (gnus-data-find (gnus-summary-article-number)) mark)
	(gnus-summary-update-line (eq mark gnus-unread-mark))))))
  
(defun gnus-mark-article-as-read (article &optional mark)
  "Enter ARTICLE in the pertinent lists and remove it from others."
  ;; Make the article expirable.
  (let ((mark (or mark gnus-del-mark)))
    (if (= mark gnus-expirable-mark)
	(setq gnus-newsgroup-expirable (cons article gnus-newsgroup-expirable))
      (setq gnus-newsgroup-expirable (delq article gnus-newsgroup-expirable)))
    ;; Remove from unread and marked lists.
    (setq gnus-newsgroup-unreads (delq article gnus-newsgroup-unreads))
    (setq gnus-newsgroup-marked (delq article gnus-newsgroup-marked))
    (setq gnus-newsgroup-dormant (delq article gnus-newsgroup-dormant))
    (push (cons article mark) gnus-newsgroup-reads)
    ;; Possibly remove from cache, if that is used. 
    (when gnus-use-cache 
      (gnus-cache-enter-remove-article article))))

(defun gnus-mark-article-as-unread (article &optional mark)
  "Enter ARTICLE in the pertinent lists and remove it from others."
  (let ((mark (or mark gnus-ticked-mark)))
    (setq gnus-newsgroup-marked (delq article gnus-newsgroup-marked))
    (setq gnus-newsgroup-dormant (delq article gnus-newsgroup-dormant))
    (setq gnus-newsgroup-expirable (delq article gnus-newsgroup-expirable))
    (setq gnus-newsgroup-unreads (delq article gnus-newsgroup-unreads))
    (cond ((= mark gnus-ticked-mark)
	   (push article gnus-newsgroup-marked))
	  ((= mark gnus-dormant-mark)
	   (push article gnus-newsgroup-dormant))
	  (t     
	   (push article gnus-newsgroup-unreads)))
    (setq gnus-newsgroup-reads
	  (delq (assq article gnus-newsgroup-reads)
		gnus-newsgroup-reads))))

(defalias 'gnus-summary-mark-as-unread-forward 
  'gnus-summary-tick-article-forward)
(make-obsolete 'gnus-summary-mark-as-unread-forward 
	       'gnus-summary-tick-article-forward)
(defun gnus-summary-tick-article-forward (n)
  "Tick N articles forwards.
If N is negative, tick backwards instead.
The difference between N and the number of articles ticked is returned."
  (interactive "p")
  (gnus-summary-mark-forward n gnus-ticked-mark))

(defalias 'gnus-summary-mark-as-unread-backward 
  'gnus-summary-tick-article-backward)
(make-obsolete 'gnus-summary-mark-as-unread-backward 
	       'gnus-summary-tick-article-backward)
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
  (gnus-summary-mark-forward n gnus-del-mark t))

(defun gnus-summary-mark-as-read-backward (n)
  "Mark the N articles as read backwards.
The difference between N and the actual number of articles marked is
returned."
  (interactive "p")
  (gnus-summary-mark-forward (- n) gnus-del-mark t))

(defun gnus-summary-mark-as-read (&optional article mark)
  "Mark current article as read.
ARTICLE specifies the article to be marked as read.
MARK specifies a string to be inserted at the beginning of the line."
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

(defun gnus-summary-mark-unread-as-read ()
  "Intended to be used by `gnus-summary-mark-article-hook'."
  (when (memq gnus-current-article gnus-newsgroup-unreads)
    (gnus-summary-mark-article gnus-current-article gnus-read-mark)))

(defun gnus-summary-mark-region-as-read (point mark all)
  "Mark all unread articles between point and mark as read.
If given a prefix, mark all articles between point and mark as read,
even ticked and dormant ones."
  (interactive "r\nP")
  (save-excursion
    (let (article)
      (goto-char point)
      (beginning-of-line)
      (while (and 
	      (< (point) mark)
	      (progn
		(when (or all 
			  (memq (setq article (gnus-summary-article-number))
				gnus-newsgroup-unreads))
		  (gnus-summary-mark-article article gnus-del-mark))
		t)
	      (gnus-summary-find-next))))))

(defun gnus-summary-mark-below (score mark)
  "Mark articles with score less than SCORE with MARK."
  (interactive "P\ncMark: ")
  (gnus-set-global-variables)
  (setq score (if score
		  (prefix-numeric-value score)
		(or gnus-summary-default-score 0)))
  (save-excursion
    (set-buffer gnus-summary-buffer)
    (goto-char (point-min))
    (while (not (eobp))
      (and (< (gnus-summary-article-score) score)
	   (gnus-summary-mark-article nil mark))
      (gnus-summary-find-next))))

(defun gnus-summary-kill-below (&optional score)
  "Mark articles with score below SCORE as read."
  (interactive "P")
  (gnus-set-global-variables)
  (gnus-summary-mark-below score gnus-killed-mark))

(defun gnus-summary-clear-above (&optional score)
  "Clear all marks from articles with score above SCORE."
  (interactive "P")
  (gnus-set-global-variables)
  (gnus-summary-mark-above score gnus-unread-mark))

(defun gnus-summary-tick-above (&optional score)
  "Tick all articles with score above SCORE."
  (interactive "P")
  (gnus-set-global-variables)
  (gnus-summary-mark-above score gnus-ticked-mark))

(defun gnus-summary-mark-above (score mark)
  "Mark articles with score over SCORE with MARK."
  (interactive "P\ncMark: ")
  (gnus-set-global-variables)
  (setq score (if score
		  (prefix-numeric-value score)
		(or gnus-summary-default-score 0)))
  (save-excursion
    (set-buffer gnus-summary-buffer)
    (goto-char (point-min))
    (while (and (progn
		  (if (> (gnus-summary-article-score) score)
		      (gnus-summary-mark-article nil mark))
		  t)
		(gnus-summary-find-next)))))

;; Suggested by Daniel Quinlan <quinlan@best.com>.  
(defalias 'gnus-summary-show-all-expunged 'gnus-summary-limit-include-expunged)
(defun gnus-summary-limit-include-expunged ()
  "Display all the hidden articles that were expunged for low scores."
  (interactive)
  (gnus-set-global-variables)
  (let ((buffer-read-only nil))
    (let ((scored gnus-newsgroup-scored)
	  headers h)
      (while scored
	(or (gnus-summary-goto-subject (car (car scored)))
	    (and (setq h (gnus-summary-article-header (car (car scored))))
		 (< (cdr (car scored)) gnus-summary-expunge-below)
		 (setq headers (cons h headers))))
	(setq scored (cdr scored)))
      (or headers (error "No expunged articles hidden."))
      (goto-char (point-min))
      (save-excursion 
	(gnus-summary-update-lines 
	 (point)
	 (progn
	   (gnus-summary-prepare-unthreaded (nreverse headers))
	   (point)))))
    (goto-char (point-min))
    (gnus-summary-position-point)))

(defun gnus-summary-catchup (&optional all quietly to-here not-mark)
  "Mark all articles not marked as unread in this newsgroup as read.
If prefix argument ALL is non-nil, all articles are marked as read.
If QUIETLY is non-nil, no questions will be asked.
If TO-HERE is non-nil, it should be a point in the buffer.  All
articles before this point will be marked as read.
The number of articles marked as read is returned."
  (interactive "P")
  (gnus-set-global-variables)
  (prog1
      (if (or quietly
	      (not gnus-interactive-catchup) ;Without confirmation?
	      gnus-expert-user
	      (gnus-y-or-n-p
	       (if all
		   "Mark absolutely all articles as read? "
		 "Mark all unread articles as read? ")))
	  (if (and not-mark 
		   (not gnus-newsgroup-adaptive)
		   (not gnus-newsgroup-auto-expire))
	      (progn
		(when all
		  (setq gnus-newsgroup-marked nil
			gnus-newsgroup-dormant nil))
		(setq gnus-newsgroup-unreads nil))
	    ;; We actually mark all articles as canceled, which we
	    ;; have to do when using auto-expiry or adaptive scoring. 
	    (gnus-summary-show-all-threads)
	    (if (gnus-summary-first-subject (not all))
		(while (and 
			(if to-here (< (point) to-here) t)
			(gnus-summary-mark-article-as-read gnus-catchup-mark)
			(gnus-summary-find-next (not all)))))
	    (unless to-here
	      (setq gnus-newsgroup-unreads nil))
	    (gnus-set-mode-line 'summary)))
    (let ((method (gnus-find-method-for-group gnus-newsgroup-name)))
      (if (and (not to-here) (eq 'nnvirtual (car method)))
	  (nnvirtual-catchup-group
	   (gnus-group-real-name gnus-newsgroup-name) (nth 1 method) all)))
    (gnus-summary-position-point)))

(defun gnus-summary-catchup-to-here (&optional all)
  "Mark all unticked articles before the current one as read.
If ALL is non-nil, also mark ticked and dormant articles as read."
  (interactive "P")
  (gnus-set-global-variables)
  (save-excursion
    (let ((beg (point)))
      ;; We check that there are unread articles.
      (when (or all (gnus-summary-find-prev))
	(gnus-summary-catchup all t beg))))
  (gnus-summary-position-point))

(defun gnus-summary-catchup-all (&optional quietly)
  "Mark all articles in this newsgroup as read."
  (interactive "P")
  (gnus-set-global-variables)
  (gnus-summary-catchup t quietly))

(defun gnus-summary-catchup-and-exit (&optional all quietly)
  "Mark all articles not marked as unread in this newsgroup as read, then exit.
If prefix argument ALL is non-nil, all articles are marked as read."
  (interactive "P")
  (gnus-set-global-variables)
  (gnus-summary-catchup all quietly nil 'fast)
  ;; Select next newsgroup or exit.
  (if (eq gnus-auto-select-next 'quietly)
      (gnus-summary-next-group nil)
    (gnus-summary-exit)))

(defun gnus-summary-catchup-all-and-exit (&optional quietly)
  "Mark all articles in this newsgroup as read, and then exit."
  (interactive "P")
  (gnus-set-global-variables)
  (gnus-summary-catchup-and-exit t quietly))

;; Suggested by "Arne Eofsson" <arne@hodgkin.mbi.ucla.edu>.
(defun gnus-summary-catchup-and-goto-next-group (&optional all)
  "Mark all articles in this group as read and select the next group.
If given a prefix, mark all articles, unread as well as ticked, as
read." 
  (interactive "P")
  (gnus-set-global-variables)
  (gnus-summary-catchup all)
  (gnus-summary-next-group))

;; Thread-based commands.

(defun gnus-summary-articles-in-thread (&optional article)
  "Return a list of all articles in the current thread.
If ARTICLE is non-nil, return all articles in the thread that starts
with that article."
  (let* ((article (or article (gnus-summary-article-number)))
	 (data (gnus-data-find-list article))
	 (top-level (gnus-data-level (car data)))
	 (top-subject 
	  (cond ((null gnus-thread-operation-ignore-subject)
		 (gnus-simplify-subject-re
		  (mail-header-subject (gnus-data-header (car data)))))
		((eq gnus-thread-operation-ignore-subject 'fuzzy)
		 (gnus-simplify-subject-fuzzy
		  (mail-header-subject (gnus-data-header (car data)))))
		(t nil)))
	 articles)
    (if (not data)
	()				; This article doesn't exist.
      (while data
	(and (or (not top-subject)
		 (string= top-subject
			  (if (eq gnus-thread-operation-ignore-subject 'fuzzy)
			      (gnus-simplify-subject-fuzzy
			       (mail-header-subject 
				(gnus-data-header (car data))))
			    (gnus-simplify-subject-re
			     (mail-header-subject 
			      (gnus-data-header (car data)))))))
	     (setq articles (cons (gnus-data-number (car data)) articles)))
	(if (and (setq data (cdr data))
		 (> (gnus-data-level (car data)) top-level))
	    ()
	  (setq data nil)))
      ;; Return the list of articles.
      (nreverse articles))))

(defun gnus-summary-toggle-threads (&optional arg)
  "Toggle showing conversation threads.
If ARG is positive number, turn showing conversation threads on."
  (interactive "P")
  (gnus-set-global-variables)
  (let ((current (or (gnus-summary-article-number) gnus-newsgroup-end)))
    (setq gnus-show-threads
	  (if (null arg) (not gnus-show-threads)
	    (> (prefix-numeric-value arg) 0)))
    (gnus-summary-prepare)
    (gnus-summary-goto-subject current)
    (gnus-summary-position-point)))

(defun gnus-summary-show-all-threads ()
  "Show all threads."
  (interactive)
  (gnus-set-global-variables)
  (save-excursion
    (let ((buffer-read-only nil))
      (subst-char-in-region (point-min) (point-max) ?\^M ?\n t)))
  (gnus-summary-position-point))

(defun gnus-summary-show-thread ()
  "Show thread subtrees.
Returns nil if no thread was there to be shown."
  (interactive)
  (gnus-set-global-variables)
  (let ((buffer-read-only nil)
	(orig (point))
	;; first goto end then to beg, to have point at beg after let
	(end (progn (end-of-line) (point)))
	(beg (progn (beginning-of-line) (point))))
    (prog1
	;; Any hidden lines here?
	(search-forward "\r" end t)
      (subst-char-in-region beg end ?\^M ?\n t)
      (goto-char orig)
      (gnus-summary-position-point))))

(defun gnus-summary-hide-all-threads ()
  "Hide all thread subtrees."
  (interactive)
  (gnus-set-global-variables)
  (save-excursion
    (goto-char (point-min))
    (gnus-summary-hide-thread)
    (while (zerop (gnus-summary-next-thread 1))
      (gnus-summary-hide-thread)))
  (gnus-summary-position-point))

(defun gnus-summary-hide-thread ()
  "Hide thread subtrees.
Returns nil if no threads were there to be hidden."
  (interactive)
  (gnus-set-global-variables)
  (let ((buffer-read-only nil)
	(start (point))
	(article (gnus-summary-article-number))
	(end (point)))
    ;; Go forward until either the buffer ends or the subthread
    ;; ends. 
    (if (eobp)
	()
      (if (not (zerop (gnus-summary-next-thread 1)))
	  ()
	(gnus-summary-find-prev)
	(prog1
	    (save-excursion
	      (search-backward "\n" start t))
	  (subst-char-in-region start (point) ?\n ?\^M)
	  (gnus-summary-goto-subject article)
	  (gnus-summary-position-point))))))

(defun gnus-summary-go-to-next-thread (&optional previous)
  "Go to the same level (or less) next thread.
If PREVIOUS is non-nil, go to previous thread instead.
Return the article number moved to, or nil if moving was impossible."
  (let* ((level (gnus-summary-thread-level))
	 (article (gnus-summary-article-number))
	 (data (cdr (gnus-data-find-list article (gnus-data-list previous))))
	 oart)
    (while data
      (if (<= (gnus-data-level (car data)) level)
	  (setq oart (gnus-data-number (car data))
		data nil)
	(setq data (cdr data))))
    (and oart 
	 (gnus-summary-goto-subject oart))))

(defun gnus-summary-next-thread (n)
  "Go to the same level next N'th thread.
If N is negative, search backward instead.
Returns the difference between N and the number of skips actually
done."
  (interactive "p")
  (gnus-set-global-variables)
  (let ((backward (< n 0))
	(n (abs n)))
    (while (and (> n 0)
		(gnus-summary-go-to-next-thread backward))
      (setq n (1- n)))
    (gnus-summary-position-point)
    (if (/= 0 n) (gnus-message 7 "No more threads"))
    n))

(defun gnus-summary-prev-thread (n)
  "Go to the same level previous N'th thread.
Returns the difference between N and the number of skips actually
done."
  (interactive "p")
  (gnus-set-global-variables)
  (gnus-summary-next-thread (- n)))

(defun gnus-summary-go-down-thread ()
  "Go down one level in the current thread."
  (let ((children (gnus-summary-article-children)))
    (and children
	 (gnus-summary-goto-subject (car children)))))

(defun gnus-summary-go-up-thread ()
  "Go up one level in the current thread."
  (let ((parent (gnus-summary-article-parent)))
    (and parent
	 (gnus-summary-goto-subject parent))))

(defun gnus-summary-down-thread (n)
  "Go down thread N steps.
If N is negative, go up instead.
Returns the difference between N and how many steps down that were
taken."
  (interactive "p")
  (gnus-set-global-variables)
  (let ((up (< n 0))
	(n (abs n)))
    (while (and (> n 0)
		(if up (gnus-summary-go-up-thread)
		  (gnus-summary-go-down-thread)))
      (setq n (1- n)))
    (gnus-summary-position-point)
    (if (/= 0 n) (gnus-message 7 "Can't go further"))
    n))

(defun gnus-summary-up-thread (n)
  "Go up thread N steps.
If N is negative, go up instead.
Returns the difference between N and how many steps down that were
taken."
  (interactive "p")
  (gnus-set-global-variables)
  (gnus-summary-down-thread (- n)))

(defun gnus-summary-kill-thread (&optional unmark)
  "Mark articles under current thread as read.
If the prefix argument is positive, remove any kinds of marks.
If the prefix argument is negative, tick articles instead."
  (interactive "P")
  (gnus-set-global-variables)
  (if unmark
      (setq unmark (prefix-numeric-value unmark)))
  (let ((articles (gnus-summary-articles-in-thread)))
    (save-excursion
      ;; Expand the thread.
      (gnus-summary-show-thread)
      ;; Mark all the articles.
      (while articles
	(gnus-summary-goto-subject (car articles))
	(cond ((null unmark) 
	       (gnus-summary-mark-article-as-read gnus-killed-mark))
	      ((> unmark 0) 
	       (gnus-summary-mark-article-as-unread gnus-unread-mark))
	      (t 
	       (gnus-summary-mark-article-as-unread gnus-ticked-mark)))
	(setq articles (cdr articles))))
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

(defun gnus-summary-sort-by-number (&optional reverse)
  "Sort summary buffer by article number.
Argument REVERSE means reverse order."
  (interactive "P")
  (gnus-set-global-variables)
  (gnus-summary-sort 
   ;; `gnus-summary-article-number' is a macro, and `sort-subr' wants
   ;; a function, so we wrap it.
   (cons (lambda () (gnus-summary-article-number))
	 'gnus-thread-sort-by-number) reverse))

(defun gnus-summary-sort-by-author (&optional reverse)
  "Sort summary buffer by author name alphabetically.
If case-fold-search is non-nil, case of letters is ignored.
Argument REVERSE means reverse order."
  (interactive "P")
  (gnus-set-global-variables)
  (gnus-summary-sort
   (cons
    (lambda ()
      (let* ((header (gnus-summary-article-header))
	     (extract (funcall
		       gnus-extract-address-components
		       (mail-header-from header))))
	(concat (or (car extract) (cdr extract))
		"\r" (mail-header-subject header))))
    'gnus-thread-sort-by-author)
   reverse))

(defun gnus-summary-sort-by-subject (&optional reverse)
  "Sort summary buffer by subject alphabetically. `Re:'s are ignored.
If case-fold-search is non-nil, case of letters is ignored.
Argument REVERSE means reverse order."
  (interactive "P")
  (gnus-set-global-variables)
  (gnus-summary-sort
   (cons
    (lambda ()
      (let* ((header (gnus-summary-article-header))
	     (extract (funcall
		       gnus-extract-address-components
		       (mail-header-from header))))
	(concat 
	 (downcase (gnus-simplify-subject (gnus-summary-article-subject) t))
	 "\r" (or (car extract) (cdr extract)))))
    'gnus-thread-sort-by-subject)
   reverse))

(defun gnus-summary-sort-by-date (&optional reverse)
  "Sort summary buffer by date.
Argument REVERSE means reverse order."
  (interactive "P")
  (gnus-set-global-variables)
  (gnus-summary-sort
   (cons
    (lambda ()
      (gnus-sortable-date
       (mail-header-date 
	(gnus-summary-article-header))))
    'gnus-thread-sort-by-date)
   reverse))

(defun gnus-summary-sort-by-score (&optional reverse)
  "Sort summary buffer by score.
Argument REVERSE means reverse order."
  (interactive "P")
  (gnus-set-global-variables)
  (gnus-summary-sort 
   (cons (lambda () (gnus-summary-article-score))
	 'gnus-thread-sort-by-score)
   (not reverse)))

(defun gnus-summary-sort (predicate reverse)
  "Sort summary buffer by PREDICATE.  REVERSE means reverse order. 
PREDICATE is a cons of `(unthreaded-func . threaded-func)'."
  (let (buffer-read-only)
    (if (not gnus-show-threads)
	;; We do untreaded sorting...
	(progn
	  (goto-char (point-min))
	  (sort-subr reverse 'forward-line 'end-of-line (car predicate))
	  (gnus-data-compute-positions))
      ;; ... or we do threaded sorting.
      (let ((gnus-thread-sort-functions (list (cdr predicate)))
	    (gnus-summary-prepare-hook nil))
	;; We do that by simply regenerating the threads.
	(gnus-summary-prepare)
	;; Hide subthreads if needed.
	(when gnus-thread-hide-subtree
	  (gnus-summary-hide-all-threads))))
    ;; If in async mode, we send some info to the backend.
    (when gnus-newsgroup-async
      (gnus-request-asynchronous 
       gnus-newsgroup-name gnus-newsgroup-data))))
  
(defun gnus-sortable-date (date)
  "Make sortable string by string-lessp from DATE.
Timezone package is used."
  (let* ((date (timezone-fix-time date nil nil)) ;[Y M D H M S]
	 (year (aref date 0))
	 (month (aref date 1))
	 (day (aref date 2)))
    (timezone-make-sortable-date 
     year month day 
     (timezone-make-time-string
      (aref date 3) (aref date 4) (aref date 5)))))


;; Summary saving commands.

(defun gnus-summary-save-article (&optional n)
  "Save the current article using the default saver function.
If N is a positive number, save the N next articles.
If N is a negative number, save the N previous articles.
If N is nil and any articles have been marked with the process mark,
save those articles instead.
The variable `gnus-default-article-saver' specifies the saver function."
  (interactive "P")
  (gnus-set-global-variables)
  (let ((articles (gnus-summary-work-articles n))
	file)
    (while articles
      (let ((header (gnus-summary-article-header (car articles))))
	(if (vectorp header)
	    (progn
	      (save-window-excursion
		(gnus-summary-select-article t nil nil (car articles)))
	      (or gnus-save-all-headers
		  ;; Remove headers accoring to `gnus-saved-headers'.
		  (let ((gnus-visible-headers 
			 (or gnus-saved-headers gnus-visible-headers)))
		    (gnus-article-hide-headers t)))
	      ;; Remove any X-Gnus lines.
	      (save-excursion
		(save-restriction
		  (set-buffer gnus-article-buffer)
		  (let ((buffer-read-only nil))
		    (goto-char (point-min))
		    (narrow-to-region (point) (or (search-forward "\n\n" nil t)
						  (point-max)))
		    (while (re-search-forward "^X-Gnus" nil t)
		      (beginning-of-line)
		      (delete-region (point)
				     (progn (forward-line 1) (point))))
		    (widen))))
	      (save-window-excursion
		(if gnus-default-article-saver
		    (setq file (funcall
				gnus-default-article-saver
				(cond
				 ((not gnus-prompt-before-saving)
				  'default)
				 ((eq gnus-prompt-before-saving 'always)
				  nil)
				 (t file))))
		  (error "No default saver is defined."))))
	  (if (assq 'name header)
	      (gnus-copy-file (cdr (assq 'name header)))
	    (gnus-message 1 "Article %d is unsaveable" (car articles)))))
      (gnus-summary-remove-process-mark (car articles))
      (setq articles (cdr articles)))
    (gnus-summary-position-point)
    n))

(defun gnus-summary-pipe-output (&optional arg)
  "Pipe the current article to a subprocess.
If N is a positive number, pipe the N next articles.
If N is a negative number, pipe the N previous articles.
If N is nil and any articles have been marked with the process mark,
pipe those articles instead."
  (interactive "P")
  (gnus-set-global-variables)
  (let ((gnus-default-article-saver 'gnus-summary-save-in-pipe))
    (gnus-summary-save-article arg))
  (gnus-configure-windows 'pipe))

(defun gnus-summary-save-article-mail (&optional arg)
  "Append the current article to an mail file.
If N is a positive number, save the N next articles.
If N is a negative number, save the N previous articles.
If N is nil and any articles have been marked with the process mark,
save those articles instead."
  (interactive "P")
  (gnus-set-global-variables)
  (let ((gnus-default-article-saver 'gnus-summary-save-in-mail))
    (gnus-summary-save-article arg)))

(defun gnus-summary-save-article-rmail (&optional arg)
  "Append the current article to an rmail file.
If N is a positive number, save the N next articles.
If N is a negative number, save the N previous articles.
If N is nil and any articles have been marked with the process mark,
save those articles instead."
  (interactive "P")
  (gnus-set-global-variables)
  (let ((gnus-default-article-saver 'gnus-summary-save-in-rmail))
    (gnus-summary-save-article arg)))

(defun gnus-summary-save-article-file (&optional arg)
  "Append the current article to a file.
If N is a positive number, save the N next articles.
If N is a negative number, save the N previous articles.
If N is nil and any articles have been marked with the process mark,
save those articles instead."
  (interactive "P")
  (gnus-set-global-variables)
  (let ((gnus-default-article-saver 'gnus-summary-save-in-file))
    (gnus-summary-save-article arg)))

(defun gnus-summary-save-article-body-file (&optional arg)
  "Append the current article body to a file.
If N is a positive number, save the N next articles.
If N is a negative number, save the N previous articles.
If N is nil and any articles have been marked with the process mark,
save those articles instead."
  (interactive "P")
  (gnus-set-global-variables)
  (let ((gnus-default-article-saver 'gnus-summary-save-body-in-file))
    (gnus-summary-save-article arg)))

(defun gnus-read-save-file-name (prompt default-name)
  (let ((methods gnus-split-methods)
	split-name method)
    ;; Let the split methods have their say.
    (when gnus-split-methods
      (save-excursion
	(set-buffer gnus-original-article-buffer)
	(gnus-narrow-to-headers)
	(while methods
	  (goto-char (point-min))
	  (setq method (pop methods))
	  (when (cond ((stringp (car method))
		       (condition-case () 
			   (re-search-forward (car method) nil t)
			 (error nil)))
		      ((and (symbolp (car method))
			    (fboundp (car method)))
		       (funcall (car method)))
		      ((consp (car method))
		       (eval (car method))))
	    (setq split-name (cons (nth 1 methods) split-name))))
	(widen)))
    (cond
     ;; No split name was found
     ((null split-name)
      (read-file-name
       (concat prompt " (default " (file-name-nondirectory default-name) ") ")
       (file-name-directory default-name)
       default-name))
     ;; A single split name was found
     ((= 1 (length split-name))
      (read-file-name
       (concat prompt " (default " (car split-name) ") ")
       gnus-article-save-directory
       (concat gnus-article-save-directory (car split-name))))
     ;; A list of splits was found.
     (t
      (setq split-name (mapcar (lambda (el) (list el)) (nreverse split-name)))
      (let ((result (completing-read (concat prompt " ") split-name nil nil)))
	(concat gnus-article-save-directory
		(if (string= result "")
		    (car (car split-name))
		  result)))))))

(defun gnus-summary-save-in-rmail (&optional filename)
  "Append this article to Rmail file.
Optional argument FILENAME specifies file name.
Directory to save to is default to `gnus-article-save-directory' which
is initialized from the SAVEDIR environment variable."
  (interactive)
  (gnus-set-global-variables)
  (let ((default-name
	  (funcall gnus-rmail-save-name gnus-newsgroup-name
		   gnus-current-headers gnus-newsgroup-last-rmail)))
    (setq filename
	  (cond ((eq filename 'default)
		 default-name)
		(filename filename)
		(t (gnus-read-save-file-name 
		    "Save in rmail file:" default-name))))
    (gnus-make-directory (file-name-directory filename))
    (gnus-eval-in-buffer-window 
     gnus-original-article-buffer
     (save-excursion
       (save-restriction
	 (widen)
	 (gnus-output-to-rmail filename))))
    ;; Remember the directory name to save articles
    (setq gnus-newsgroup-last-rmail filename)))

(defun gnus-summary-save-in-mail (&optional filename)
  "Append this article to Unix mail file.
Optional argument FILENAME specifies file name.
Directory to save to is default to `gnus-article-save-directory' which
is initialized from the SAVEDIR environment variable."
  (interactive)
  (gnus-set-global-variables)
  (let ((default-name
	  (funcall gnus-mail-save-name gnus-newsgroup-name
		   gnus-current-headers gnus-newsgroup-last-mail)))
    (setq filename
	  (cond ((eq filename 'default)
		 default-name)
		(filename filename)
		(t (gnus-read-save-file-name 
		    "Save in Unix mail file:" default-name))))
    (setq filename
	  (expand-file-name filename
			    (and default-name
				 (file-name-directory default-name))))
    (gnus-make-directory (file-name-directory filename))
    (gnus-eval-in-buffer-window 
     gnus-original-article-buffer
     (save-excursion
       (save-restriction
	 (widen)
	 (if (and (file-readable-p filename) (mail-file-babyl-p filename))
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
  (gnus-set-global-variables)
  (let ((default-name
	  (funcall gnus-file-save-name gnus-newsgroup-name
		   gnus-current-headers gnus-newsgroup-last-file)))
    (setq filename
	  (cond ((eq filename 'default)
		 default-name)
		(filename filename)
		(t (gnus-read-save-file-name 
		    "Save in file:" default-name))))
    (gnus-make-directory (file-name-directory filename))
    (gnus-eval-in-buffer-window 
     gnus-article-buffer
     (save-excursion
       (save-restriction
	 (widen)
	 (gnus-output-to-file filename))))
    ;; Remember the directory name to save articles.
    (setq gnus-newsgroup-last-file filename)))

(defun gnus-summary-save-body-in-file (&optional filename)
  "Append this article body to a file.
Optional argument FILENAME specifies file name.
The directory to save in defaults to `gnus-article-save-directory' which
is initialized from the SAVEDIR environment variable."
  (interactive)
  (gnus-set-global-variables)
  (let ((default-name
	  (funcall gnus-file-save-name gnus-newsgroup-name
		   gnus-current-headers gnus-newsgroup-last-file)))
    (setq filename
	  (cond ((eq filename 'default)
		 default-name)
		(filename filename)
		(t (gnus-read-save-file-name 
		    "Save body in file:" default-name))))
    (gnus-make-directory (file-name-directory filename))
    (gnus-eval-in-buffer-window 
     gnus-article-buffer
     (save-excursion
       (save-restriction
	 (widen)
	 (goto-char (point-min))
	 (and (search-forward "\n\n" nil t)
	      (narrow-to-region (point) (point-max)))
	 (gnus-output-to-file filename))))
    ;; Remember the directory name to save articles.
    (setq gnus-newsgroup-last-file filename)))

(defun gnus-summary-save-in-pipe (&optional command)
  "Pipe this article to subprocess."
  (interactive)
  (gnus-set-global-variables)
  (setq command
	(cond ((eq command 'default)
	       gnus-last-shell-command)
	      (command command)
	      (t (read-string "Shell command on article: "
			      gnus-last-shell-command))))
  (if (string-equal command "")
      (setq command gnus-last-shell-command))
  (gnus-eval-in-buffer-window 
   gnus-article-buffer
   (save-restriction
     (widen)
     (shell-command-on-region (point-min) (point-max) command nil)))
  (setq gnus-last-shell-command command))

;; Summary extract commands

(defun gnus-summary-insert-pseudos (pslist &optional not-view)
  (let ((buffer-read-only nil)
	(article (gnus-summary-article-number))
 	after-article b e)
    (or (gnus-summary-goto-subject article)
	(error (format "No such article: %d" article)))
    (gnus-summary-position-point)
    ;; If all commands are to be bunched up on one line, we collect
    ;; them here.  
    (if gnus-view-pseudos-separately
	()
      (let ((ps (setq pslist (sort pslist 'gnus-pseudos<)))
	    files action)
	(while ps
	  (setq action (cdr (assq 'action (car ps))))
	  (setq files (list (cdr (assq 'name (car ps)))))
	  (while (and ps (cdr ps)
		      (string= (or action "1")
			       (or (cdr (assq 'action (car (cdr ps)))) "2")))
	    (setq files (cons (cdr (assq 'name (car (cdr ps)))) files))
	    (setcdr ps (cdr (cdr ps))))
	  (if (not files)
	      ()
	    (if (not (string-match "%s" action))
		(setq files (cons " " files)))
	    (setq files (cons " " files))
	    (and (assq 'execute (car ps))
		 (setcdr (assq 'execute (car ps))
			 (funcall (if (string-match "%s" action)
				      'format 'concat)
				  action 
				  (mapconcat (lambda (f) f) files " ")))))
	  (setq ps (cdr ps)))))
    (if (and gnus-view-pseudos (not not-view))
	(while pslist
	  (and (assq 'execute (car pslist))
	       (gnus-execute-command (cdr (assq 'execute (car pslist)))
				     (eq gnus-view-pseudos 'not-confirm)))
	  (setq pslist (cdr pslist)))
      (save-excursion
	(while pslist
	  (setq after-article (or (cdr (assq 'article (car pslist)))
				  (gnus-summary-article-number)))
	  (gnus-summary-goto-subject after-article)
	  (forward-line 1)
  	  (setq b (point))
	  (insert "          " (file-name-nondirectory
				(cdr (assq 'name (car pslist))))
		  ": " (or (cdr (assq 'execute (car pslist))) "") "\n")
	  (setq e (point))
	  (forward-line -1)		; back to `b'
	  (put-text-property b e 'gnus-number gnus-reffed-article-number)
	  (gnus-data-enter after-article
			   gnus-reffed-article-number
			   gnus-unread-mark 
			   b
			   (car pslist) 
			   0 
			   (- e b))
	  (setq gnus-newsgroup-unreads
		(cons gnus-reffed-article-number gnus-newsgroup-unreads))
	  (setq gnus-reffed-article-number (1- gnus-reffed-article-number))
	  (setq pslist (cdr pslist)))))))

(defun gnus-pseudos< (p1 p2)
  (let ((c1 (cdr (assq 'action p1)))
	(c2 (cdr (assq 'action p2))))
    (and c1 c2 (string< c1 c2))))

(defun gnus-request-pseudo-article (props)
  (cond ((assq 'execute props)
	 (gnus-execute-command (cdr (assq 'execute props)))))
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
  (gnus-set-global-variables)
  (or to (setq to (read-file-name "Copy file to: " default-directory)))
  (and (file-directory-p to) 
       (setq to (concat (file-name-as-directory to)
			(file-name-nondirectory file))))
  (copy-file file to))

;; Summary kill commands.

(defun gnus-summary-edit-global-kill (article)
  "Edit the \"global\" kill file."
  (interactive (list (gnus-summary-article-number)))
  (gnus-set-global-variables)
  (gnus-group-edit-global-kill article))

(defun gnus-summary-edit-local-kill ()
  "Edit a local kill file applied to the current newsgroup."
  (interactive)
  (gnus-set-global-variables)
  (setq gnus-current-headers (gnus-summary-article-header))
  (gnus-set-global-variables)
  (gnus-group-edit-local-kill 
   (gnus-summary-article-number) gnus-newsgroup-name))


;;;
;;; Gnus article mode
;;;

(put 'gnus-article-mode 'mode-class 'special)

(if gnus-article-mode-map
    nil
  (setq gnus-article-mode-map (make-keymap))
  (suppress-keymap gnus-article-mode-map)
  (define-key gnus-article-mode-map " " 'gnus-article-next-page)
  (define-key gnus-article-mode-map "\177" 'gnus-article-prev-page)
  (define-key gnus-article-mode-map "\C-c^" 'gnus-article-refer-article)
  (define-key gnus-article-mode-map "h" 'gnus-article-show-summary)
  (define-key gnus-article-mode-map "s" 'gnus-article-show-summary)
  (define-key gnus-article-mode-map "\C-c\C-m" 'gnus-article-mail)
  (define-key gnus-article-mode-map "?" 'gnus-article-describe-briefly)
  (define-key gnus-article-mode-map gnus-mouse-2 'gnus-article-push-button)
  (define-key gnus-article-mode-map "\r" 'gnus-article-press-button)
  (define-key gnus-article-mode-map "\t" 'gnus-article-next-button)
  (define-key gnus-article-mode-map "\C-c\C-b" 'gnus-bug)
  
  ;; Duplicate almost all summary keystrokes in the article mode map.
  (let ((commands 
	 (list 
	  "p" "N" "P" "\M-\C-n" "\M-\C-p"
	  "\M-n" "\M-p" "." "," "\M-s" "\M-r" "<" ">" "j"
	  "u" "!" "U" "d" "D" "E" "\M-u" "\M-U" "k" "\C-k" "\M-\C-k"
	  "\M-\C-l" "e" "#" "\M-#" "\M-\C-t" "\M-\C-s" "\M-\C-h"
	  "\M-\C-f" "\M-\C-b" "\M-\C-u" "\M-\C-d" "&" "\C-w"
	  "\C-t" "?" "\C-c\M-\C-s" "\C-c\C-s\C-n" "\C-c\C-s\C-a"
	  "\C-c\C-s\C-s" "\C-c\C-s\C-d" "\C-c\C-s\C-i" "\C-x\C-s"
	  "\M-g" "w" "\C-c\C-r" "\M-t" "C"
	  "o" "\C-o" "|" "\M-k" "\M-K" "V" "\C-c\C-d"
	  "\C-c\C-i" "x" "X" "t" "g" "?" "l"
	  "\C-c\C-v\C-v" "\C-d" "v" 
;;	  "Mt" "M!" "Md" "Mr"
;;	  "Mc" "M " "Me" "Mx" "M?" "Mb" "MB" "M#" "M\M-#" "M\M-r"
;;	  "M\M-\C-r" "MD" "M\M-D" "MS" "MC" "MH" "M\C-c" "Mk" "MK"
;;	  "Ms" "Mc" "Mu" "Mm" "Mk" "Gn" "Gp" "GN" "GP" "G\C-n" "G\C-p"
;;	  "G\M-n" "G\M-p" "Gf" "Gb" "Gg" "Gl" "Gp" "Tk" "Tl" "Ti" "TT"
;;	  "Ts" "TS" "Th" "TH" "Tn" "Tp" "Tu" "Td" "T#" "A " "An" "A\177" "Ap"
;;	  "A\r" "A<" "A>" "Ab" "Ae" "A^" "Ar" "Aw" "Ac" "Ag" "At" "Am"
;;	  "As" "Wh" "Ws" "Wc" "Wo" "Ww" "Wd" "Wq" "Wf" "Wt" "W\C-t"
;;	  "WT" "WA" "Wa" "WH" "WC" "WS" "Wb" "Hv" "Hf" "Hd" "Hh" "Hi"
;;	  "Be" "B\177" "Bm" "Br" "Bw" "Bc" "Bq" "Bi" "Oo" "Om" "Or"
;;	  "Of" "Oh" "Ov" "Op" "Vu" "V\C-s" "V\C-r" "Vr" "V&" "VT" "Ve"
;;	  "VD" "Vk" "VK" "Vsn" "Vsa" "Vss" "Vsd" "Vsi"
	  )))
    (while commands
      (define-key gnus-article-mode-map (car commands) 
	'gnus-article-summary-command)
      (setq commands (cdr commands))))

  (let ((commands (list "q" "Q"  "c" "r" "R" "\C-c\C-f" "m"  "a" "f" "F"
;;			"Zc" "ZC" "ZE" "ZQ" "ZZ" "Zn" "ZR" "ZG" "ZN" "ZP" 
			 "=" "n"  "^" "\M-^")))
    (while commands
      (define-key gnus-article-mode-map (car commands) 
	'gnus-article-summary-command-nosave)
      (setq commands (cdr commands)))))


(defun gnus-article-mode ()
  "Major mode for displaying an article.

All normal editing commands are switched off.

The following commands are available:

\\<gnus-article-mode-map>
\\[gnus-article-next-page]\t Scroll the article one page forwards
\\[gnus-article-prev-page]\t Scroll the article one page backwards
\\[gnus-article-refer-article]\t Go to the article referred to by an article id near point
\\[gnus-article-show-summary]\t Display the summary buffer
\\[gnus-article-mail]\t Send a reply to the address near point
\\[gnus-article-describe-briefly]\t Describe the current mode briefly
\\[gnus-info-find-node]\t Go to the Gnus info node"
  (interactive)
  (if (gnus-visual-p 'article-menu 'menu) (gnus-article-make-menu-bar))
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
  ;; Returns the article buffer.
  (if (get-buffer gnus-article-buffer)
      (save-excursion
	(set-buffer gnus-article-buffer)
	(buffer-disable-undo (current-buffer))
	(setq buffer-read-only t)
	(gnus-add-current-to-buffer-list)
	(or (eq major-mode 'gnus-article-mode)
	    (gnus-article-mode))
	(current-buffer))
    (save-excursion
      (set-buffer (get-buffer-create gnus-article-buffer))
      (gnus-add-current-to-buffer-list)
      (gnus-article-mode)
      (current-buffer))))

;; Set article window start at LINE, where LINE is the number of lines
;; from the head of the article.
(defun gnus-article-set-window-start (&optional line)
  (set-window-start 
   (get-buffer-window gnus-article-buffer)
   (save-excursion
     (set-buffer gnus-article-buffer)
     (goto-char (point-min))
     (if (not line)
	 (point-min)
       (gnus-message 6 "Moved to bookmark")
       (search-forward "\n\n" nil t)
       (forward-line line)
       (point)))))

(defun gnus-request-article-this-buffer (article group)
  "Get an article and insert it into this buffer."
  (prog1
      (save-excursion
	(if (get-buffer gnus-original-article-buffer)
	    (set-buffer (get-buffer gnus-original-article-buffer))
	  (set-buffer (get-buffer-create gnus-original-article-buffer))
	  (buffer-disable-undo (current-buffer))
	  (setq major-mode 'gnus-original-article-mode)
	  (setq buffer-read-only t)
	  (gnus-add-current-to-buffer-list))

	(setq group (or group gnus-newsgroup-name))

	;; Open server if it has closed.
	(gnus-check-server (gnus-find-method-for-group group))

	;; Using `gnus-request-article' directly will insert the article into
	;; `nntp-server-buffer' - so we'll save some time by not having to
	;; copy it from the server buffer into the article buffer.

	;; We only request an article by message-id when we do not have the
	;; headers for it, so we'll have to get those.
	(and (stringp article) 
	     (let ((gnus-override-method gnus-refer-article-method))
	       (gnus-read-header article)))

	;; If the article number is negative, that means that this article
	;; doesn't belong in this newsgroup (possibly), so we find its
	;; message-id and request it by id instead of number.
	(if (not (numberp article))
	    ()
	  (save-excursion
	    (set-buffer gnus-summary-buffer)
	    (let ((header (gnus-summary-article-header article)))
	      (if (< article 0)
		  (if (vectorp header)
		      ;; It's a real article.
		      (setq article (mail-header-id header))
		    ;; It is an extracted pseudo-article.
		    (setq article 'pseudo)
		    (gnus-request-pseudo-article header)))

	      (let ((method (gnus-find-method-for-group gnus-newsgroup-name)))
		(if (not (eq (car method) 'nneething))
		    ()
		  (let ((dir (concat (file-name-as-directory (nth 1 method))
				     (mail-header-subject header))))
		    (if (file-directory-p dir)
			(progn
			  (setq article 'nneething)
			  (gnus-group-enter-directory dir)))))))))

	(cond 
	 ;; We first check `gnus-original-article-buffer'.
	 ((and (equal (car gnus-original-article) group)
	       (eq (cdr gnus-original-article) article))
	  ;; We don't have to do anything, since it's already where we
	  ;; want it.  
	  'article)
	 ;; Check the backlog.
	 ((and gnus-keep-backlog
	       (gnus-backlog-request-article group article (current-buffer)))
	  'article)
	 ;; Check the cache.
	 ((and gnus-use-cache
	       (numberp article)
	       (gnus-cache-request-article article group))
	  'article)
	 ;; Get the article and put into the article buffer.
	 ((or (stringp article) (numberp article))
	  (let ((gnus-override-method 
		 (and (stringp article) gnus-refer-article-method))
		(buffer-read-only nil))
	    (erase-buffer)
	    ;; There may be some overlays that we have to kill...
	    (insert "i")
	    (let ((overlays (and (fboundp 'overlays-at)
				 (overlays-at (point-min)))))
	      (while overlays
		(delete-overlay (car overlays))
		(setq overlays (cdr overlays))))
	    (erase-buffer)	  
	    (if (gnus-request-article article group (current-buffer))
		(progn
		  (and gnus-keep-backlog 
		       (gnus-backlog-enter-article 
			group article (current-buffer)))
		  'article))))
	 ;; It was a pseudo.
	 (t article)))
    (setq gnus-original-article (cons group article))
    (let (buffer-read-only)
      (erase-buffer)
      ;; There may be some overlays that we have to kill...
      (insert "i")
      (let ((overlays (and (fboundp 'overlays-at)
			   (overlays-at (point-min)))))
	(while overlays
	  (delete-overlay (pop overlays))))
      (erase-buffer)
      (insert-buffer-substring gnus-original-article-buffer))))

(defun gnus-read-header (id)
  "Read the headers of article ID and enter them into the Gnus system."
  (let ((group gnus-newsgroup-name)
	(headers gnus-newsgroup-headers)
	header where)
    ;; First we check to see whether the header in question is already
    ;; fetched. 
    (if (stringp id)
	;; This is a Message-ID.
	(while headers
	  (if (string= id (mail-header-id (car headers)))
	      (setq header (car headers)
		    headers nil)
	    (setq headers (cdr headers))))
      ;; This is an article number.
      (while headers
	(if (= id (mail-header-number (car headers)))
	    (setq header (car headers)
		  headers nil)
	  (setq headers (cdr headers)))))
    (if header
	;; We have found the header.
	header
      ;; We have to really fetch the header to this article.
      (when (setq where
		  (if (gnus-check-backend-function 'request-head group)
		      (gnus-request-head id group)
		    (gnus-request-article id group)))
	(save-excursion
	  (set-buffer nntp-server-buffer)
	  (and (search-forward "\n\n" nil t)
	       (delete-region (1- (point)) (point-max)))
	  (goto-char (point-max))
	  (insert ".\n")
	  (goto-char (point-min))
	  (insert "211 "
		  (int-to-string
		   (cond
		    ((numberp id)
		     id)
		    ((cdr where)
		     (cdr where))
		    (t
		     gnus-reffed-article-number)))
		  " Article retrieved.\n"))
	(if (not (setq header (car (gnus-get-newsgroup-headers))))
	    () ; Malformed head.
	  (if (and (stringp id)
		   (not (string= (gnus-group-real-name group)
				 (car where))))
	      ;; If we fetched by Message-ID and the article came
	      ;; from a different group, we fudge some bogus article
	      ;; numbers for this article.
	      (mail-header-set-number header gnus-reffed-article-number))
	  (decf gnus-reffed-article-number)
	  (push header gnus-newsgroup-headers)
	  (setq gnus-current-headers header)
	  (push (mail-header-number header) gnus-newsgroup-limit)
	  header)))))

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
	  (gnus-check-server 
	   (gnus-find-method-for-group gnus-newsgroup-name))
	  (gnus-request-group gnus-newsgroup-name t)))
    (let* ((article (if header (mail-header-number header) article))
	   (summary-buffer (current-buffer))
	   (internal-hook gnus-article-internal-prepare-hook)
	   (group gnus-newsgroup-name)
	   result)
      (save-excursion
	(gnus-article-setup-buffer)
	(set-buffer gnus-article-buffer)
	(if (not (setq result (let ((buffer-read-only nil))
				(gnus-request-article-this-buffer 
				 article group))))
	    ;; There is no such article.
	    (save-excursion
	      (if (not (numberp article))
		  ()
		(setq gnus-article-current 
		      (cons gnus-newsgroup-name article))
		(set-buffer gnus-summary-buffer)
		(setq gnus-current-article article)
		(gnus-summary-mark-article article gnus-canceled-mark))
	      (gnus-message 1 "No such article (may be canceled)")
	      (ding)
	      nil)
	  (if (or (eq result 'pseudo) (eq result 'nneething))
	      (progn
		(save-excursion
		  (set-buffer summary-buffer)
		  (setq gnus-last-article gnus-current-article
			gnus-newsgroup-history (cons gnus-current-article
						     gnus-newsgroup-history)
			gnus-current-article 0
			gnus-current-headers nil
			gnus-article-current nil)
		  (if (eq result 'nneething)
		      (gnus-configure-windows 'summary)
		    (gnus-configure-windows 'article))
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
		  (setq gnus-last-article gnus-current-article
			gnus-newsgroup-history (cons gnus-current-article
						     gnus-newsgroup-history)
			gnus-current-article article
			gnus-current-headers 
			(gnus-summary-article-header gnus-current-article)
			gnus-article-current 
			(cons gnus-newsgroup-name gnus-current-article))
		  (gnus-summary-show-thread)
		  (run-hooks 'gnus-mark-article-hook)
		  (gnus-set-mode-line 'summary)
		  (and (gnus-visual-p 'article-highlight 'highlight)
		       (run-hooks 'gnus-visual-mark-article-hook))
		  ;; Set the global newsgroup variables here.
		  ;; Suggested by Jim Sisolak
		  ;; <sisolak@trans4.neep.wisc.edu>.
		  (gnus-set-global-variables)
		  (setq gnus-have-all-headers 
			(or all-headers gnus-show-all-headers))
		  (and gnus-use-cache 
		       (vectorp (gnus-summary-article-header article))
		       (gnus-cache-possibly-enter-article
			group article
			(gnus-summary-article-header article)
			(memq article gnus-newsgroup-marked)
			(memq article gnus-newsgroup-dormant)
			(memq article gnus-newsgroup-unreads)))))
	    ;; Hooks for getting information from the article.
	    ;; This hook must be called before being narrowed.
	    (let (buffer-read-only)
	      (run-hooks 'internal-hook)
	      (run-hooks 'gnus-article-prepare-hook)
	      ;; Decode MIME message.
	      (if (and gnus-show-mime
		       (or (not gnus-strict-mime)
			   (gnus-fetch-field "Mime-Version")))
		  (funcall gnus-show-mime-method))
	      ;; Perform the article display hooks.
	      (run-hooks 'gnus-article-display-hook))
	    ;; Do page break.
	    (goto-char (point-min))
	    (and gnus-break-pages (gnus-narrow-to-page))
	    (gnus-set-mode-line 'article)
	    (gnus-configure-windows 'article)
	    (goto-char (point-min))
	    t))))))

(defun gnus-article-show-all-headers ()
  "Show all article headers in article mode buffer."
  (save-excursion 
    (gnus-article-setup-buffer)
    (set-buffer gnus-article-buffer)
    (let ((buffer-read-only nil))
      (remove-text-properties (point-min) (point-max) 
			      gnus-hidden-properties))))

(defun gnus-article-hide-headers-if-wanted ()
  "Hide unwanted headers if `gnus-have-all-headers' is nil.
Provided for backwards compatability."
  (or (save-excursion (set-buffer gnus-summary-buffer) gnus-have-all-headers)
      (gnus-article-hide-headers)))

(defun gnus-article-hide-headers (&optional delete)
  "Hide unwanted headers and possibly sort them as well."
  (interactive "P")
  (save-excursion
    (set-buffer gnus-article-buffer)
    (save-restriction
      (let ((sorted gnus-sorted-header-list)
	    (buffer-read-only nil)
	    want-list beg want-l)
	;; First we narrow to just the headers.
	(widen)
	(goto-char (point-min))
	;; Hide any "From " lines at the beginning of (mail) articles. 
	(while (looking-at "From ")
	  (forward-line 1))
	(or (bobp) 
	    (add-text-properties (point-min) (point) gnus-hidden-properties))
	;; Then treat the rest of the header lines.
	(narrow-to-region 
	 (point) 
	 (progn (search-forward "\n\n" nil t) (forward-line -1) (point)))
	;; Then we use the two regular expressions
	;; `gnus-ignored-headers' and `gnus-visible-headers' to
	;; select which header lines is to remain visible in the
	;; article buffer.
	(goto-char (point-min))
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
	(goto-char (point-min))
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
	  (add-text-properties (point) (point-max) gnus-hidden-properties))))))

;; Written by Per Abrahamsen <amanda@iesd.auc.dk>.
(defun gnus-article-treat-overstrike ()
  "Translate overstrikes into bold text."
  (interactive)
  (save-excursion
    (set-buffer gnus-article-buffer)
    (let ((buffer-read-only nil))
      (while (search-forward "\b" nil t)
	(let ((next (following-char))
	      (previous (char-after (- (point) 2))))
	  (cond ((eq next previous)
		 (put-text-property (- (point) 2) (point)
				    'invisible t)
		 (put-text-property (point) (1+ (point))
				    'face 'bold))
		((eq next ?_)
		 (put-text-property (1- (point)) (1+ (point))
				    'invisible t)
		 (put-text-property (- (point) 2) (1- (point))
				    'face 'underline))
		((eq previous ?_)
		 (put-text-property (- (point) 2) (point)
				    'invisible t)
		 (put-text-property (point) (1+ (point))
				    'face 'underline))))))))

(defun gnus-article-word-wrap ()
  "Format too long lines."
  (interactive)
  (save-excursion
    (set-buffer gnus-article-buffer)
    (let ((buffer-read-only nil)
	  p)
      (widen)
      (goto-char (point-min))
      (search-forward "\n\n" nil t)
      (end-of-line 1)
      (let ((paragraph-start "^[>|#:<;* ]*[ \t]*$")
	    (adaptive-fill-regexp "[ \t]*\\([|#:<;>*]+ *\\)?")
	    (adaptive-fill-mode t))
	(while (not (eobp))
	  (and (>= (current-column) (min fill-column (window-width)))
	       (/= (preceding-char) ?:)
	       (fill-paragraph nil))
	  (end-of-line 2))))))

(defun gnus-article-remove-cr ()
  "Remove carriage returns from an article."
  (interactive)
  (save-excursion
    (set-buffer gnus-article-buffer)
    (let ((buffer-read-only nil))
      (goto-char (point-min))
      (while (search-forward "\r" nil t)
	(replace-match "" t t)))))

(defun gnus-article-display-x-face (&optional force)
  "Look for an X-Face header and display it if present."
  (interactive (list 'force))
  (save-excursion
    (set-buffer gnus-article-buffer)
    ;; Delete the old process, if any.
    (when (process-status "gnus-x-face")
      (delete-process "gnus-x-face"))
    (let ((inhibit-point-motion-hooks t)
	  (case-fold-search nil)
	  from)
      (save-restriction
	(gnus-narrow-to-headers)
	(setq from (mail-fetch-field "from"))
	(goto-char (point-min))
	(when (and gnus-article-x-face-command 
		   (or force
		       ;; Check whether this face is censored.
		       (not gnus-article-x-face-too-ugly)
		       (and gnus-article-x-face-too-ugly from
			    (not (string-match gnus-article-x-face-too-ugly 
					       from))))
		   ;; Has to be present.
		   (re-search-forward "^X-Face: " nil t))
	  ;; We now have the area of the buffer where the X-Face is stored.
	  (let ((beg (point))
		(end (1- (re-search-forward "^\\($\\|[^ \t]\\)" nil t))))
	    ;; We display the face.
	    (if (symbolp gnus-article-x-face-command)
		;; The command is a lisp function, so we call it.
		(if (fboundp gnus-article-x-face-command)
		    (funcall gnus-article-x-face-command beg end)
		  (error "%s is not a function" gnus-article-x-face-command))
	      ;; The command is a string, so we interpret the command
	      ;; as a, well, command, and fork it off.
	      (let ((process-connection-type nil))
		(process-kill-without-query
		 (start-process 
		  "gnus-x-face" nil "sh" "-c" gnus-article-x-face-command))
		(process-send-region "gnus-x-face" beg end)
		(process-send-eof "gnus-x-face")))))))))

(defun gnus-headers-decode-quoted-printable ()
  "Hack to remove QP encoding from headers."
  (let ((case-fold-search t)
	(inhibit-point-motion-hooks t)
	string)
    (goto-char (point-min))
    (while (re-search-forward "=\\?iso-8859-1\\?q\\?\\([^?\t\n]*\\)\\?=" nil t)
      (setq string (match-string 1))
      (narrow-to-region (match-beginning 0) (match-end 0))
      (delete-region (point-min) (point-max))
      (insert string)
      (gnus-mime-decode-quoted-printable (goto-char (point-min)) (point-max))
      (subst-char-in-region (point-min) (point-max) ?_ ? )
      (widen)
      (goto-char (point-min)))))
       
(defun gnus-article-de-quoted-unreadable (&optional force)
  "Do a naive translation of a quoted-printable-encoded article.
This is in no way, shape or form meant as a replacement for real MIME
processing, but is simply a stop-gap measure until MIME support is
written.
If FORCE, decode the article whether it is marked as quoted-printable
or not." 
  (interactive (list 'force))
  (save-excursion
    (set-buffer gnus-article-buffer)
    (let ((case-fold-search t)
	  (buffer-read-only nil)
	  (type (gnus-fetch-field "content-transfer-encoding")))
      (when (or force
		(and type (string-match "quoted-printable" type)))
	(goto-char (point-min))
	(search-forward "\n\n" nil 'move)
	(gnus-mime-decode-quoted-printable (point) (point-max))
	(gnus-headers-decode-quoted-printable)))))

(defun gnus-mime-decode-quoted-printable (from to)
  "Decode Quoted-Printable in the region between FROM and TO."
  (goto-char from)
  (while (search-forward "=" to t)
    (cond ((eq (following-char) ?\n)
	   (delete-char -1)
	   (delete-char 1))
	  ((looking-at "[0-9A-F][0-9A-F]")
	   (delete-char -1)
	   (insert (hexl-hex-string-to-integer
		    (buffer-substring (point) (+ 2 (point)))))
	   (delete-char 2))
	  ((looking-at "=")
	   (delete-char 1))
	  ((gnus-message 3 "Malformed MIME quoted-printable message")))))

(defun gnus-article-hide-pgp ()
  "Hide any PGP headers and signatures in the current article."
  (interactive)
  (save-excursion
    (set-buffer gnus-article-buffer)
    (let (buffer-read-only beg end)
      (widen)
      (goto-char (point-min))
      ;; Hide the "header".
      (and (search-forward "\n-----BEGIN PGP SIGNED MESSAGE-----\n" nil t)
	   (add-text-properties (match-beginning 0) (match-end 0)
				gnus-hidden-properties))
      (setq beg (point))
      ;; Hide the actual signature.
      (and (search-forward "\n-----BEGIN PGP SIGNATURE-----\n" nil t)
	   (setq end (match-beginning 0))
	   (add-text-properties 
	    (match-beginning 0)
	    (if (search-forward "\n-----END PGP SIGNATURE-----\n" nil t)
		(match-end 0)
	      ;; Perhaps we shouldn't hide to the end of the buffer
	      ;; if there is no end to the signature?
	      (point-max))
	    gnus-hidden-properties))
      (when (and beg end)
	(narrow-to-region beg end)
	(goto-char (point-min))
	(while (re-search-forward "^- " nil t)
	  (replace-match "" t t))
	(widen)))))

(defvar gnus-article-time-units
  `((year . ,(* 365.25 24 60 60))
    (week . ,(* 7 24 60 60))
    (day . ,(* 24 60 60))
    (hour . ,(* 60 60))
    (minute . 60)
    (second . 1))
  "Mapping from time units to seconds.")

(defun gnus-article-date-ut (&optional type)
  "Convert DATE date to universal time in the current article.
If TYPE is `local', convert to local time; if it is `lapsed', output
how much time has lapsed since DATE."
  (interactive (list 'ut))
  (let ((date (mail-header-date (or gnus-current-headers 
				    (gnus-summary-article-header) "")))
	(date-regexp "^Date: \\|^X-Sent: ")
	(inhibit-point-motion-hooks t))
    (when (and date (not (string= date "")))
      (save-excursion
	(save-restriction
	  (set-buffer gnus-article-buffer)
	  (gnus-narrow-to-headers)
	  (let ((buffer-read-only nil))
	    ;; Delete any old Date headers.
	    (if (zerop (nnheader-remove-header date-regexp t))
		(beginning-of-line)
	      (goto-char (point-max)))
	    (insert
	     (cond 
	      ;; Convert to the local timezone.  We have to slap a
	      ;; `condition-case' round the calls to the timezone
	      ;; functions since they aren't particularly resistant to
	      ;; buggy dates.
	      ((eq type 'local)
	       (concat "Date: " (condition-case ()
				    (timezone-make-date-arpa-standard date)
				  (error date))
		       "\n"))
	      ;; Convert to Universal Time.
	      ((eq type 'ut)
	       (concat "Date: "
		       (condition-case ()
			   (timezone-make-date-arpa-standard date nil "UT")
			 (error date))
		       "\n"))
	      ;; Get the original date from the article.
	      ((eq type 'original)
	       (concat "Date: " date "\n"))
	      ;; Do an X-Sent lapsed format.
	      ((eq type 'lapsed)
	       ;; If the date is seriously mangled, the timezone
	       ;; functions are liable to bug out, so we condition-case
	       ;; the entire thing.  
	       (let* ((real-sec (condition-case ()
				    (- (gnus-seconds-since-epoch 
					(timezone-make-date-arpa-standard
					 (current-time-string) 
					 (current-time-zone) "UT"))
				       (gnus-seconds-since-epoch 
					(timezone-make-date-arpa-standard 
					 date nil "UT")))
				  (error 0)))
		      (sec (abs real-sec))
		      num prev)
		 (if (zerop sec)
		     "X-Sent: Now\n"
		   (concat
		    "X-Sent: "
		    ;; This is a bit convoluted, but basically we go
		    ;; through the time units for years, weeks, etc,
		    ;; and divide things to see whether that results
		    ;; in positive answers.
		    (mapconcat 
		     (lambda (unit)
		       (if (zerop (setq num (ffloor (/ sec (cdr unit)))))
			   ;; The (remaining) seconds are too few to
			   ;; be divided into this time unit.
			   "" 
			 ;; It's big enough, so we output it.
			 (setq sec (- sec (* num (cdr unit))))
			 (prog1
			     (concat (if prev ", " "") (int-to-string 
							(floor num))
				     " " (symbol-name (car unit))
				     (if (> num 1) "s" ""))
			   (setq prev t))))
		     gnus-article-time-units "")
		    ;; If dates are odd, then it might appear like the
		    ;; article was sent in the future.
		    (if (> real-sec 0)
			" ago\n"
		      " in the future\n")))))
	      (t
	       (error "Unknown conversion type: %s" type))))))))))

(defun gnus-article-date-local ()
  "Convert the current article date to the local timezone."
  (interactive)
  (gnus-article-date-ut 'local))

(defun gnus-article-date-original ()
  "Convert the current article date to what it was originally.
This is only useful if you have used some other date conversion
function and want to see what the date was before converting."
  (interactive)
  (gnus-article-date-ut 'original))

(defun gnus-article-date-lapsed ()
  "Convert the current article date to time lapsed since it was sent."
  (interactive)
  (gnus-article-date-ut 'lapsed))

(defun gnus-article-maybe-highlight ()
  "Do some article highlighting if `gnus-visual' is non-nil."
  (if (gnus-visual-p 'article-highlight 'highlight)
      (gnus-article-highlight-some)))

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
			  (symbol-value 'rmail-current-message))))
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
		  (rmail-show-message msg)))))))
    (kill-buffer tmpbuf)))

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
    (kill-buffer tmpbuf)))

(defun gnus-convert-article-to-rmail ()
  "Convert article in current buffer to Rmail message format."
  (let ((buffer-read-only nil))
    ;; Convert article directly into Babyl format.
    ;; Suggested by Rob Austein <sra@lcs.mit.edu>
    (goto-char (point-min))
    (insert "\^L\n0, unseen,,\n*** EOOH ***\n")
    (while (search-forward "\n\^_" nil t) ;single char
      (replace-match "\n^_" t t))	;2 chars: "^" and "_"
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
			(point)))))

(defun gnus-gmt-to-local ()
  "Rewrite Date header described in GMT to local in current buffer.
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
		(date (buffer-substring-no-properties
		       (match-beginning 1) (match-end 1))))
	    (delete-region (match-beginning 1) (match-end 1))
	    (insert
	     (timezone-make-date-arpa-standard 
	      date nil (current-time-zone))))))))

;; Article mode commands

(defun gnus-article-next-page (&optional lines)
  "Show next page of current article.
If end of article, return non-nil.  Otherwise return nil.
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
	nil)
    ;; More in this page.
    (condition-case ()
	(scroll-up lines)
      (end-of-buffer
       ;; Long lines may cause an end-of-buffer error.
       (goto-char (point-max))))
    nil))

(defun gnus-article-prev-page (&optional lines)
  "Show previous page of current article.
Argument LINES specifies lines to be scrolled down."
  (interactive "P")
  (move-to-window-line 0)
  (if (and gnus-break-pages
	   (bobp)
	   (not (save-restriction (widen) (bobp)))) ;Real beginning-of-buffer?
      (progn
	(gnus-narrow-to-page -1)	;Go to previous page.
	(goto-char (point-max))
	(recenter -1))
    (scroll-down lines)))

(defun gnus-article-refer-article ()
  "Read article specified by message-id around point."
  (interactive)
  (search-forward ">" nil t)		;Move point to end of "<....>".
  (if (re-search-backward "\\(<[^<> \t\n]+>\\)" nil t)
      (let ((message-id (match-string 1)))
	(set-buffer gnus-summary-buffer)
	(gnus-summary-refer-article message-id))
    (error "No references around point")))

(defun gnus-article-show-summary ()
  "Reconfigure windows to show summary buffer."
  (interactive)
  (gnus-configure-windows 'article)
  (gnus-summary-goto-subject gnus-current-article))

(defun gnus-article-describe-briefly ()
  "Describe article mode commands briefly."
  (interactive)
  (gnus-message 6
		(substitute-command-keys "\\<gnus-article-mode-map>\\[gnus-article-next-page]:Next page  \\[gnus-article-prev-page]:Prev page  \\[gnus-article-show-summary]:Show summary  \\[gnus-info-find-node]:Run Info  \\[gnus-article-describe-briefly]:This help")))

(defun gnus-article-summary-command ()
  "Execute the last keystroke in the summary buffer."
  (interactive)
  (let ((obuf (current-buffer))
	(owin (current-window-configuration))
	func)
    (switch-to-buffer gnus-summary-buffer 'norecord)
    (setq func (lookup-key (current-local-map) (this-command-keys)))
    (call-interactively func)
    (set-buffer obuf)
    (set-window-configuration owin)
    (set-window-point (get-buffer-window (current-buffer)) (point))))

(defun gnus-article-summary-command-nosave ()
  "Execute the last keystroke in the summary buffer."
  (interactive)
  (let (func)
    (pop-to-buffer gnus-summary-buffer 'norecord)
    (setq func (lookup-key (current-local-map) (this-command-keys)))
    (call-interactively func)))


;; Basic ideas by emv@math.lsa.umich.edu (Edward Vielmetti)

;;;###autoload
(defalias 'gnus-batch-kill 'gnus-batch-score)
;;;###autoload
(defun gnus-batch-score ()
  "Run batched scoring.
Usage: emacs -batch -l gnus -f gnus-batch-score <newsgroups> ...
Newsgroups is a list of strings in Bnews format.  If you want to score
the comp hierarchy, you'd say \"comp.all\".  If you would not like to
score the alt hierarchy, you'd say \"!alt.all\"."
  (interactive)
  (let* ((yes-and-no
	  (gnus-newsrc-parse-options
	   (apply (function concat)
		  (mapcar (lambda (g) (concat g " "))
			  command-line-args-left))))
	 (gnus-expert-user t)
	 (nnmail-spool-file nil)
	 (gnus-use-dribble-file nil)
	 (yes (car yes-and-no))
	 (no (cdr yes-and-no))
	 group newsrc entry
	 ;; Disable verbose message.
	 gnus-novice-user gnus-large-newsgroup)
    ;; Eat all arguments.
    (setq command-line-args-left nil)
    ;; Start Gnus.
    (gnus)
    ;; Apply kills to specified newsgroups in command line arguments.
    (setq newsrc (cdr gnus-newsrc-alist))
    (while newsrc
      (setq group (car (car newsrc)))
      (setq entry (gnus-gethash group gnus-newsrc-hashtb))
      (if (and (<= (nth 1 (car newsrc)) gnus-level-subscribed)
	       (and (car entry)
		    (or (eq (car entry) t)
			(not (zerop (car entry)))))
	       (if yes (string-match yes group) t)
	       (or (null no) (not (string-match no group))))
	  (progn
	    (gnus-summary-read-group group nil t nil t)
	    (and (eq (current-buffer) (get-buffer gnus-summary-buffer))
		 (gnus-summary-exit))))
      (setq newsrc (cdr newsrc)))
    ;; Exit Emacs.
    (switch-to-buffer gnus-group-buffer)
    (gnus-group-save-newsrc)))

(defun gnus-apply-kill-file ()
  "Apply a kill file to the current newsgroup.
Returns the number of articles marked as read."
  (if (or (file-exists-p (gnus-newsgroup-kill-file nil))
	  (file-exists-p (gnus-newsgroup-kill-file gnus-newsgroup-name)))
      (gnus-apply-kill-file-internal)
    0))

(defun gnus-kill-save-kill-buffer ()
  (save-excursion
    (let ((file (gnus-newsgroup-kill-file gnus-newsgroup-name)))
      (if (get-file-buffer file)
	  (progn
	    (set-buffer (get-file-buffer file))
	    (and (buffer-modified-p) (save-buffer))
	    (kill-buffer (current-buffer)))))))

(defvar gnus-kill-file-name "KILL"
  "Suffix of the kill files.")

(defun gnus-newsgroup-kill-file (newsgroup)
  "Return the name of a kill file name for NEWSGROUP.
If NEWSGROUP is nil, return the global kill file name instead."
  (cond ((or (null newsgroup)
	     (string-equal newsgroup ""))
	 ;; The global KILL file is placed at top of the directory.
	 (expand-file-name gnus-kill-file-name
			   (or gnus-kill-files-directory "~/News")))
	((gnus-use-long-file-name 'not-kill)
	 ;; Append ".KILL" to newsgroup name.
	 (expand-file-name (concat (gnus-newsgroup-saveable-name newsgroup)
				   "." gnus-kill-file-name)
			   (or gnus-kill-files-directory "~/News")))
	(t
	 ;; Place "KILL" under the hierarchical directory.
	 (expand-file-name (concat (gnus-newsgroup-directory-form newsgroup)
				   "/" gnus-kill-file-name)
			   (or gnus-kill-files-directory "~/News")))))


;;;
;;; Dribble file
;;;

(defvar gnus-dribble-ignore nil)
(defvar gnus-dribble-eval-file nil)

(defun gnus-dribble-file-name ()
  (concat 
   (if gnus-dribble-directory
       (concat (file-name-as-directory gnus-dribble-directory)
	       (file-name-nondirectory gnus-current-startup-file))
     gnus-current-startup-file)
   "-dribble"))

(defun gnus-dribble-enter (string)
  (if (and (not gnus-dribble-ignore)
	   (or gnus-dribble-buffer
	       gnus-slave)
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
      (setq buffer-file-name dribble-file)
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
		   "Auto-save file exists.  Do you want to read it? ")
		  (setq gnus-dribble-eval-file t))))))))

(defun gnus-dribble-eval-file ()
  (if (not gnus-dribble-eval-file)
      ()
    (setq gnus-dribble-eval-file nil)
    (save-excursion
      (let ((gnus-dribble-ignore t))
	(set-buffer gnus-dribble-buffer)
	(eval-buffer (current-buffer))))))

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
    (if (gnus-buffer-exists-p gnus-dribble-buffer)
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
  (let (how)
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
		      ((string-match "^:" gnus-nntp-server)
		       (list 'nnmh gnus-nntp-server 
			     (list 'nnmh-directory 
				   (file-name-as-directory
				    (expand-file-name
				     (concat "~/" (substring
						   gnus-nntp-server 1)))))
			     (list 'nnmh-get-new-mail nil)))
		      (t
		       (list 'nntp gnus-nntp-server)))))

      (setq how (car gnus-select-method))
      (cond ((eq how 'nnspool)
	     (require 'nnspool)
	     (gnus-message 5 "Looking up local news spool..."))
	    ((eq how 'nnmh)
	     (require 'nnmh)
	     (gnus-message 5 "Looking up mh spool..."))
	    (t
	     (require 'nntp)))
      (setq gnus-current-select-method gnus-select-method)
      (run-hooks 'gnus-open-server-hook)
      (or 
       ;; gnus-open-server-hook might have opened it
       (gnus-server-opened gnus-select-method)  
       (gnus-open-server gnus-select-method)
       (gnus-y-or-n-p
	(format
	 "%s open error: '%s'.  Continue? "
	 (nth 1 gnus-select-method)
	 (gnus-status-message gnus-select-method)))
       (progn
	 (gnus-message 1 "Couldn't open server on %s" 
		       (nth 1 gnus-select-method))
	 (ding)
	 nil)))))

(defun gnus-check-server (&optional method)
  "If the news server is down, start it up again."
  (let ((method (if method method gnus-select-method)))
    (and (stringp method)
	 (setq method (gnus-server-to-method method)))
    (if (gnus-server-opened method)
	;; Stream is already opened.
	t
      ;; Open server.
      (gnus-message 5 "Opening server %s on %s..." (car method) (nth 1 method))
      (run-hooks 'gnus-open-server-hook)
      (prog1
	  (gnus-open-server method)
	(message "")))))

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
  (and (stringp method)
       (setq method (gnus-server-to-method method)))
  (let ((func (intern (format "%s-%s" (car method) function))))
    (if (not (fboundp func)) 
	(progn
	  (require (car method))
	  (if (not (fboundp func)) 
	      (error "No such function: %s" func))))
    func))

;;; Interface functions to the backends.

(defun gnus-open-server (method)
  (let ((elem (assoc method gnus-opened-servers)))
    ;; If this method was previously denied, we just return nil.
    (if (eq (cdr elem) 'denied)
	nil
      ;; Open the server.
      (let ((result
	     (funcall (gnus-get-function method 'open-server)
		      (nth 1 method) (nthcdr 2 method))))
	;; If this hasn't been opened before, we add it to the list.
	(unless elem 
	  (setq elem (list method nil)
		gnus-opened-servers (cons elem gnus-opened-servers)))
	;; Set the status of this server.
	(setcar (cdr elem) (if result 'ok 'denied))
	;; Return the result from the "open" call.
	result))))

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
  (let ((method (if (stringp method) (gnus-find-method-for-group method)
		  method)))
    (funcall (gnus-get-function method 'status-message) (nth 1 method))))

(defun gnus-request-group (group &optional dont-check)
  (let ((method (gnus-find-method-for-group group)))
    (funcall (gnus-get-function method 'request-group) 
	     (gnus-group-real-name group) (nth 1 method) dont-check)))

(defun gnus-request-asynchronous (group &optional articles)
  (let ((method (gnus-find-method-for-group group)))
    (funcall (gnus-get-function method 'request-asynchronous) 
	     (gnus-group-real-name group) (nth 1 method) articles)))

(defun gnus-list-active-group (group)
  (let ((method (gnus-find-method-for-group group))
	(func 'list-active-group))
    (and (gnus-check-backend-function func group)
	 (funcall (gnus-get-function method func) 
		  (gnus-group-real-name group) (nth 1 method)))))

(defun gnus-request-group-description (group)
  (let ((method (gnus-find-method-for-group group))
	(func 'request-group-description))
    (and (gnus-check-backend-function func group)
	 (funcall (gnus-get-function method func) 
		  (gnus-group-real-name group) (nth 1 method)))))

(defun gnus-close-group (group)
  (let ((method (gnus-find-method-for-group group)))
    (funcall (gnus-get-function method 'close-group) 
	     (gnus-group-real-name group) (nth 1 method))))

(defun gnus-retrieve-headers (articles group &optional fetch-old)
  (let ((method (gnus-find-method-for-group group)))
    (if (and gnus-use-cache (numberp (car articles)))
	(gnus-cache-retrieve-headers articles group)
      (funcall (gnus-get-function method 'retrieve-headers) 
	       articles (gnus-group-real-name group) (nth 1 method)
	       fetch-old))))

(defun gnus-retrieve-groups (groups method)
  (funcall (gnus-get-function method 'retrieve-groups) groups (nth 1 method)))

(defun gnus-request-article (article group &optional buffer)
  (let ((method (gnus-find-method-for-group group)))
    (funcall (gnus-get-function method 'request-article) 
	     article (gnus-group-real-name group) (nth 1 method) buffer)))

(defun gnus-request-head (article group)
  (let ((method (gnus-find-method-for-group group)))
    (funcall (gnus-get-function method 'request-head) 
	     article (gnus-group-real-name group) (nth 1 method))))

(defun gnus-request-body (article group)
  (let ((method (gnus-find-method-for-group group)))
    (funcall (gnus-get-function method 'request-body) 
	     article (gnus-group-real-name group) (nth 1 method))))

(defun gnus-request-post (method &optional force)
  (funcall (gnus-get-function method 'request-post) 
	   (nth 1 method)))

(defun gnus-request-scan (group method)
  (let ((method (if group (gnus-find-method-for-group group) method)))
    (funcall (gnus-get-function method 'request-scan) 
	     (and group (gnus-group-real-name group)) (nth 1 method))))

(defun gnus-request-update-info (info method)
  (funcall (gnus-get-function method 'request-update-info) 
	   (gnus-group-real-name (gnus-info-group info)) info (nth 1 method)))

(defun gnus-request-expire-articles (articles group &optional force)
  (let ((method (gnus-find-method-for-group group)))
    (funcall (gnus-get-function method 'request-expire-articles) 
	     articles (gnus-group-real-name group) (nth 1 method)
	     force)))

(defun gnus-request-move-article 
  (article group server accept-function &optional last)
  (let ((method (gnus-find-method-for-group group)))
    (funcall (gnus-get-function method 'request-move-article) 
	     article (gnus-group-real-name group) 
	     (nth 1 method) accept-function last)))

(defun gnus-request-accept-article (group &optional last)
  (let ((func (if (symbolp group) group
		(car (gnus-find-method-for-group group)))))
    (funcall (intern (format "%s-request-accept-article" func))
	     (if (stringp group) (gnus-group-real-name group) group)
	     last)))

(defun gnus-request-replace-article (article group buffer)
  (let ((func (car (gnus-find-method-for-group group))))
    (funcall (intern (format "%s-request-replace-article" func))
	     article (gnus-group-real-name group) buffer)))

(defun gnus-request-create-group (group)
  (let ((method (gnus-find-method-for-group group)))
    (funcall (gnus-get-function method 'request-create-group) 
	     (gnus-group-real-name group) (nth 1 method))))

(defun gnus-request-delete-group (group &optional force)
  (let ((method (gnus-find-method-for-group group)))
    (funcall (gnus-get-function method 'request-delete-group) 
	     (gnus-group-real-name group) force (nth 1 method))))

(defun gnus-request-rename-group (group new-name)
  (let ((method (gnus-find-method-for-group group)))
    (funcall (gnus-get-function method 'request-rename-group) 
	     (gnus-group-real-name group) 
	     (gnus-group-real-name new-name) (nth 1 method))))

(defun gnus-post-method (group force-group-method)
  "Return the posting method based on GROUP and FORCE."
  (let ((group-method (if (stringp group)
			  (gnus-find-method-for-group group)
			group)))
    (cond 
     ;; We want this group's method.
     (force-group-method group-method)
     ;; Override normal method.
     ((and gnus-post-method
	   (gnus-method-option-p group-method 'post))
      gnus-post-method)
     ;; Perhaps this is a mail group?
     ((gnus-member-of-valid 'post group)
      group-method)
     ;; Use the normal select method.
     (t gnus-select-method))))

(defun gnus-member-of-valid (symbol group)
  "Find out if GROUP has SYMBOL as part of its \"valid\" spec."
  (memq symbol (assoc
		(format "%s" (car (gnus-find-method-for-group group)))
		gnus-valid-select-methods)))

(defun gnus-method-option-p (method option)
  "Return non-nil if select METHOD has OPTION as a parameter."
  (memq 'post (assoc (format "%s" (car method))
		     gnus-valid-select-methods)))


(defmacro gnus-server-equal (ss1 ss2)
  "Say whether two servers are equal."
  `(let ((s1 ,ss1)
	 (s2 ,ss2))
     (or (equal s1 s2)
	 (and (= (length s1) (length s2))
	      (progn
		(while (and s1 (member (car s1) s2))
		  (setq s1 (cdr s1)))
		(null s1))))))

(defun gnus-server-extend-method (group method)
  ;; This function "extends" a virtual server.  If the server is
  ;; "hello", and the select method is ("hello" (my-var "something")) 
  ;; in the group "alt.alt", this will result in a new virtual server
  ;; called "helly+alt.alt".
  (let ((entry
	 (gnus-copy-sequence 
	  (if (equal (car method) "native") gnus-select-method
	    (cdr (assoc (car method) gnus-server-alist))))))
    (setcar (cdr entry) (concat (nth 1 entry) "+" group))
    (nconc entry (cdr method))))

(defun gnus-find-method-for-group (group &optional info)
  "Find the select method that GROUP uses."
  (or gnus-override-method
      (and (not group)
	   gnus-select-method)
      (let ((info (or info (gnus-get-info group)))
	    method)
	(if (or (not info)
		(not (setq method (gnus-info-method info))))
	    (setq method gnus-select-method)
	  (setq method
		(cond ((stringp method)
		       (gnus-server-to-method method))
		      ((stringp (car method))
		       (gnus-server-extend-method group method))
		      (t
		       method))))
	(gnus-server-add-address method))))

(defun gnus-check-backend-function (func group)
  "Check whether GROUP supports function FUNC."
  (let ((method (if (stringp group) (car (gnus-find-method-for-group group))
		  group)))
    (fboundp (intern (format "%s-%s" method func)))))

(defun gnus-methods-using (feature)
  "Find all methods that have FEATURE."
  (let ((valids gnus-valid-select-methods)
	outs)
    (while valids
      (if (memq feature (car valids)) 
	  (setq outs (cons (car valids) outs)))
      (setq valids (cdr valids)))
    outs))

;;; 
;;; Active & Newsrc File Handling
;;;

;; Newsrc related functions.
;; Gnus internal format of gnus-newsrc-alist:
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
;; element *before* "alt.misc" in gnus-newsrc-alist, which makes is
;; trivial to remove or add new elements into gnus-newsrc-alist
;; without scanning the entire list.  So, to get the actual information
;; of "alt.misc", you'd say something like 
;; (nth 2 (gnus-gethash "alt.misc" gnus-newsrc-hashtb))
;;
;; Gnus internal format of gnus-active-hashtb:
;; ((1 . 1))
;;  (5 . 10))
;;  (67 . 99)) ...)
;; The only element in each entry in this hash table is a range of
;; (possibly) available articles. (Articles in this range may have
;; been expired or canceled.)
;;
;; Gnus internal format of gnus-killed-list and gnus-zombie-list:
;; ("alt.misc" "alt.test" "alt.general" ...)

(defun gnus-setup-news (&optional rawfile level)
  "Setup news information.
If RAWFILE is non-nil, the .newsrc file will also be read.
If LEVEL is non-nil, the news will be set up at level LEVEL."
  (let ((init (not (and gnus-newsrc-alist gnus-active-hashtb (not rawfile)))))
    ;; Clear some variables to re-initialize news information.
    (if init (setq gnus-newsrc-alist nil 
		   gnus-active-hashtb nil))

    ;; Read the newsrc file and create `gnus-newsrc-hashtb'.
    (if init (gnus-read-newsrc-file rawfile))

    ;; If we don't read the complete active file, we fill in the
    ;; hashtb here. 
    (if (or (null gnus-read-active-file)
	    (eq gnus-read-active-file 'some))
	(gnus-update-active-hashtb-from-killed))

    ;; Read the active file and create `gnus-active-hashtb'.
    ;; If `gnus-read-active-file' is nil, then we just create an empty
    ;; hash table.  The partial filling out of the hash table will be
    ;; done in `gnus-get-unread-articles'.
    (and gnus-read-active-file 
	 (not level)
	 (gnus-read-active-file))

    (or gnus-active-hashtb
	(setq gnus-active-hashtb (make-vector 4095 0)))

    ;; Possibly eval the dribble file.
    (and init (or gnus-use-dribble-file gnus-slave) (gnus-dribble-eval-file))

    (gnus-update-format-specifications)

    ;; Find new newsgroups and treat them.
    (if (and init gnus-check-new-newsgroups gnus-read-active-file (not level)
	     (gnus-server-opened gnus-select-method))
	(gnus-find-new-newsgroups))

    ;; Find the number of unread articles in each non-dead group.
    (let ((gnus-read-active-file (and (not level) gnus-read-active-file)))
      (gnus-get-unread-articles (or level (1+ gnus-level-subscribed))))

    (if (and init gnus-check-bogus-newsgroups 
	     gnus-read-active-file (not level)
	     (gnus-server-opened gnus-select-method))
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
	  (gnus-message 5 "Looking for new newsgroups...")
	  (or gnus-have-read-active-file (gnus-read-active-file))
	  (setq gnus-newsrc-last-checked-date (current-time-string))
	  (if (not gnus-killed-hashtb) (gnus-make-hashtable-from-killed))
	  ;; Go though every newsgroup in `gnus-active-hashtb' and compare
	  ;; with `gnus-newsrc-hashtb' and `gnus-killed-hashtb'.
	  (mapatoms
	   (lambda (sym)
	     (if (or (null (setq group (symbol-name sym)))
		     (null (symbol-value sym))
		     (gnus-gethash group gnus-killed-hashtb)
		     (gnus-gethash group gnus-newsrc-hashtb))
		 ()
	       (let ((do-sub (gnus-matches-options-n group)))
		 (cond 
		  ((eq do-sub 'subscribe)
		   (setq groups (1+ groups))
		   (gnus-sethash group group gnus-killed-hashtb)
		   (funcall gnus-subscribe-options-newsgroup-method group))
		  ((eq do-sub 'ignore)
		   nil)
		  (t
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
	      (gnus-message 6 "%d new newsgroup%s arrived." 
			    groups (if (> groups 1) "s have" " has"))
	    (gnus-message 6 "No new newsgroups."))))))

(defun gnus-matches-options-n (group)
  ;; Returns `subscribe' if the group is to be uncoditionally
  ;; subscribed, `ignore' if it is to be ignored, and nil if there is
  ;; no match for the group.

  ;; First we check the two user variables.
  (cond
   ((and gnus-options-subscribe
	 (string-match gnus-options-subscribe group))
    'subscribe)
   ((and gnus-auto-subscribed-groups 
	 (string-match gnus-auto-subscribed-groups group))
    'subscribe)
   ((and gnus-options-not-subscribe
	 (string-match gnus-options-not-subscribe group))
    'ignore)
   ;; Then we go through the list that was retrieved from the .newsrc
   ;; file.  This list has elements on the form 
   ;; `(REGEXP . {ignore,subscribe})'.  The first match found (the list
   ;; is in the reverse order of the options line) is returned.
   (t
    (let ((regs gnus-newsrc-options-n))
      (while (and regs
		  (not (string-match (car (car regs)) group)))
	(setq regs (cdr regs)))
      (and regs (cdr (car regs)))))))

(defun gnus-ask-server-for-new-groups ()
  (let* ((date (or gnus-newsrc-last-checked-date (current-time-string)))
	 (methods (cons gnus-select-method 
			(append
			 (and (consp gnus-check-new-newsgroups)
			      gnus-check-new-newsgroups)
			 gnus-secondary-select-methods)))
	 (groups 0)
	 (new-date (current-time-string))
	 (hashtb (gnus-make-hashtable 100))
	 group new-newsgroups got-new method)
    ;; Go through both primary and secondary select methods and
    ;; request new newsgroups.  
    (while methods
      (setq method (gnus-server-get-method nil (car methods)))
      (and (gnus-check-server method)
	   (gnus-request-newgroups date method)
	   (save-excursion
	     (setq got-new t)
	     (set-buffer nntp-server-buffer)
	     ;; Enter all the new groups in a hashtable.
	     (gnus-active-to-gnus-format method hashtb 'ignore)))
      (setq methods (cdr methods)))
    (and got-new (setq gnus-newsrc-last-checked-date new-date))
    ;; Now all new groups from all select methods are in `hashtb'.
    (mapatoms
     (lambda (group-sym)
       (setq group (symbol-name group-sym))
       (if (or (null group)
	       (null (symbol-value group-sym))
	       (gnus-gethash group gnus-newsrc-hashtb)
	       (member group gnus-zombie-list)
	       (member group gnus-killed-list))
	   ;; The group is already known.
	   ()
	 (and (symbol-value group-sym)
	      (gnus-set-active group (symbol-value group-sym)))
	 (let ((do-sub (gnus-matches-options-n group)))
	   (cond ((eq do-sub 'subscribe)
		  (setq groups (1+ groups))
		  (gnus-sethash group group gnus-killed-hashtb)
		  (funcall 
		   gnus-subscribe-options-newsgroup-method group))
		 ((eq do-sub 'ignore)
		  nil)
		 (t
		  (setq groups (1+ groups))
		  (gnus-sethash group group gnus-killed-hashtb)
		  (if gnus-subscribe-hierarchical-interactive
		      (setq new-newsgroups (cons group new-newsgroups))
		    (funcall gnus-subscribe-newsgroup-method group)))))))
     hashtb)
    (if new-newsgroups 
	(gnus-subscribe-hierarchical-interactive new-newsgroups))
    ;; Suggested by Per Abrahamsen <amanda@iesd.auc.dk>.
    (if (> groups 0)
	(gnus-message 6 "%d new newsgroup%s arrived." 
		      groups (if (> groups 1) "s have" " has")))
    got-new))

(defun gnus-check-first-time-used ()
  (if (or (> (length gnus-newsrc-alist) 1)
	  (file-exists-p gnus-startup-file)
	  (file-exists-p (concat gnus-startup-file ".el"))
	  (file-exists-p (concat gnus-startup-file ".eld")))
      nil
    (gnus-message 6 "First time user; subscribing you to default groups")
    (or gnus-have-read-active-file (gnus-read-active-file))
    (setq gnus-newsrc-last-checked-date (current-time-string))
    (let ((groups gnus-default-subscribed-newsgroups)
	  group)
      (if (eq groups t)
	  nil
	(setq groups (or groups gnus-backup-default-subscribed-newsgroups))
	(mapatoms
	 (lambda (sym)
	   (if (null (setq group (symbol-name sym)))
	       ()
	     (let ((do-sub (gnus-matches-options-n group)))
	       (cond 
		((eq do-sub 'subscribe)
		 (gnus-sethash group group gnus-killed-hashtb)
		 (funcall gnus-subscribe-options-newsgroup-method group))
		((eq do-sub 'ignore)
		 nil)
		(t
		 (setq gnus-killed-list (cons group gnus-killed-list)))))))
	 gnus-active-hashtb)
	(while groups
	  (if (gnus-active (car groups))
	      (gnus-group-change-level 
	       (car groups) gnus-level-default-subscribed gnus-level-killed))
	  (setq groups (cdr groups)))
	(gnus-group-make-help-group)
	(and gnus-novice-user
	     (gnus-message 7 "`A k' to list killed groups"))))))

(defun gnus-subscribe-group (group previous &optional method)
  (gnus-group-change-level 
   (if method
       (list t group gnus-level-default-subscribed nil nil method)
     group) 
   gnus-level-default-subscribed gnus-level-killed previous t))

;; `gnus-group-change-level' is the fundamental function for changing
;; subscription levels of newsgroups.  This might mean just changing
;; from level 1 to 2, which is pretty trivial, from 2 to 6 or back
;; again, which subscribes/unsubscribes a group, which is equally
;; trivial.  Changing from 1-7 to 8-9 means that you kill a group, and
;; from 8-9 to 1-7 means that you remove the group from the list of
;; killed (or zombie) groups and add them to the (kinda) subscribed
;; groups.  And last but not least, moving from 8 to 9 and 9 to 8,
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
	     (< oldlevel gnus-level-zombie))
	(setq entry (gnus-gethash entry gnus-newsrc-hashtb)))
    (if (and (not oldlevel)
	     (consp entry))
	(setq oldlevel (car (cdr (nth 2 entry)))))
    (if (stringp previous)
	(setq previous (gnus-gethash previous gnus-newsrc-hashtb)))

    (if (and (>= oldlevel gnus-level-zombie)
	     (gnus-gethash group gnus-newsrc-hashtb))
	;; We are trying to subscribe a group that is already
	;; subscribed. 
	()				; Do nothing. 

      (or (gnus-ephemeral-group-p group)
	  (gnus-dribble-enter
	   (format "(gnus-group-change-level %S %S %S %S %S)" 
		   group level oldlevel (car (nth 2 previous)) fromkilled)))
    
      ;; Then we remove the newgroup from any old structures, if needed.
      ;; If the group was killed, we remove it from the killed or zombie
      ;; list.  If not, and it is in fact going to be killed, we remove
      ;; it from the newsrc hash table and assoc.
      (cond ((>= oldlevel gnus-level-zombie)
	     (if (= oldlevel gnus-level-zombie)
		 (setq gnus-zombie-list (delete group gnus-zombie-list))
	       (setq gnus-killed-list (delete group gnus-killed-list))))
	    (t
	     (if (and (>= level gnus-level-zombie)
		      entry)
		 (progn
		   (gnus-sethash (car (nth 2 entry)) nil gnus-newsrc-hashtb)
		   (if (nth 3 entry)
		       (setcdr (gnus-gethash (car (nth 3 entry))
					     gnus-newsrc-hashtb)
			       (cdr entry)))
		   (setcdr (cdr entry) (cdr (cdr (cdr entry))))))))

      ;; Finally we enter (if needed) the list where it is supposed to
      ;; go, and change the subscription level.  If it is to be killed,
      ;; we enter it into the killed or zombie list.
      (cond ((>= level gnus-level-zombie)
	     ;; Remove from the hash table.
	     (gnus-sethash group nil gnus-newsrc-hashtb)
	     ;; We do not enter foreign groups into the list of dead
	     ;; groups.  
	     (unless (gnus-group-foreign-p group)
	       (if (= level gnus-level-zombie)
		   (setq gnus-zombie-list (cons group gnus-zombie-list))
		 (setq gnus-killed-list (cons group gnus-killed-list)))))
	    (t
	     ;; If the list is to be entered into the newsrc assoc, and
	     ;; it was killed, we have to create an entry in the newsrc
	     ;; hashtb format and fix the pointers in the newsrc assoc.
	     (if (>= oldlevel gnus-level-zombie)
		 (progn
		   (if (listp entry)
		       (progn
			 (setq info (cdr entry))
			 (setq num (car entry)))
		     (setq active (gnus-active group))
		     (setq num 
			   (if active (- (1+ (cdr active)) (car active)) t))
		     ;; Check whether the group is foreign.  If so, the
		     ;; foreign select method has to be entered into the
		     ;; info. 
		     (let ((method (gnus-group-method-name group)))
		       (if (eq method gnus-select-method)
			   (setq info (list group level nil))
			 (setq info (list group level nil nil method)))))
		   (or previous 
		       (setq previous 
			     (let ((p gnus-newsrc-alist))
			       (while (cdr (cdr p))
				 (setq p (cdr p)))
			       p)))
		   (setq entry (cons info (cdr (cdr previous))))
		   (if (cdr previous)
		       (progn
			 (setcdr (cdr previous) entry)
			 (gnus-sethash group (cons num (cdr previous)) 
				       gnus-newsrc-hashtb))
		     (setcdr previous entry)
		     (gnus-sethash group (cons num previous)
				   gnus-newsrc-hashtb))
		   (if (cdr entry)
		       (setcdr (gnus-gethash (car (car (cdr entry)))
					     gnus-newsrc-hashtb)
			       entry)))
	       ;; It was alive, and it is going to stay alive, so we
	       ;; just change the level and don't change any pointers or
	       ;; hash table entries.
	       (setcar (cdr (car (cdr (cdr entry)))) level)))))))

(defun gnus-kill-newsgroup (newsgroup)
  "Obsolete function.  Kills a newsgroup."
  (gnus-group-change-level
   (gnus-gethash newsgroup gnus-newsrc-hashtb) gnus-level-killed))

(defun gnus-check-bogus-newsgroups (&optional confirm)
  "Remove bogus newsgroups.
If CONFIRM is non-nil, the user has to confirm the deletion of every
newsgroup." 
  (let ((newsrc (cdr gnus-newsrc-alist))
	bogus group entry)
    (gnus-message 5 "Checking bogus newsgroups...")
    (or gnus-have-read-active-file (gnus-read-active-file))
    ;; Find all bogus newsgroup that are subscribed.
    (while newsrc
      (setq group (car (car newsrc)))
      (if (or (gnus-active group) ; Active
	      (nth 4 (car newsrc))	; Foreign
	      (and confirm
		   (not (gnus-y-or-n-p
			 (format "Remove bogus newsgroup: %s " group)))))
	  ;; Don't remove.
	  ()
	;; Found a bogus newsgroup.
	(setq bogus (cons group bogus)))
      (setq newsrc (cdr newsrc)))
    ;; Remove all bogus subscribed groups by first killing them, and
    ;; then removing them from the list of killed groups.
    (while bogus
      (and (setq entry (gnus-gethash (car bogus) gnus-newsrc-hashtb))
	   (progn
	     (gnus-group-change-level entry gnus-level-killed)
	     (setq gnus-killed-list (delete (car bogus) gnus-killed-list))))
      (setq bogus (cdr bogus)))
    ;; Then we remove all bogus groups from the list of killed and
    ;; zombie groups.  They are are removed without confirmation.
    (let ((dead-lists '(gnus-killed-list gnus-zombie-list))
	  killed)
      (while dead-lists
	(setq killed (symbol-value (car dead-lists)))
	(while killed
	  (setq group (car killed))
	  (or (gnus-active group)
	      ;; The group is bogus.
	      (set (car dead-lists)
		   (delete group (symbol-value (car dead-lists)))))
	  (setq killed (cdr killed)))
	(setq dead-lists (cdr dead-lists))))
    (gnus-message 5 "Checking bogus newsgroups...done")))

(defun gnus-check-duplicate-killed-groups ()
  "Remove duplicates from the list of killed groups."
  (interactive)
  (let ((killed gnus-killed-list))
    (while killed
      (gnus-message 9 "%d" (length killed))
      (setcdr killed (delete (car killed) (cdr killed)))
      (setq killed (cdr killed)))))

;; Go though `gnus-newsrc-alist' and compare with `gnus-active-hashtb'
;; and compute how many unread articles there are in each group.
(defun gnus-get-unread-articles (&optional level) 
  (let* ((newsrc (cdr gnus-newsrc-alist))
	 (level (or level (1+ gnus-level-subscribed)))
	 (foreign-level
	  (min 
	   (cond ((and gnus-activate-foreign-newsgroups 
		       (not (numberp gnus-activate-foreign-newsgroups)))
		  (1+ gnus-level-subscribed))
		 ((numberp gnus-activate-foreign-newsgroups)
		  gnus-activate-foreign-newsgroups)
		 (t 0))
	   level))
	 (update
	  (fboundp (intern (format "%s-request-update-info"
				   (car gnus-select-method)))))
	 info group active virtuals method fmethod)
    (gnus-message 5 "Checking new news...")

    (while newsrc
      (setq info (car newsrc)
	    group (gnus-info-group info)
	    active (gnus-active group))

      ;; Check newsgroups.  If the user doesn't want to check them, or
      ;; they can't be checked (for instance, if the news server can't
      ;; be reached) we just set the number of unread articles in this
      ;; newsgroup to t.  This means that Gnus thinks that there are
      ;; unread articles, but it has no idea how many.
      (if (and (setq method (gnus-info-method info))
	       (not (gnus-server-equal
		     gnus-select-method
		     (prog1
			 (setq fmethod (gnus-server-get-method nil method))
		       ;; We do this here because it would be awkward
		       ;; to do it anywhere else.  Hell, it's pretty
		       ;; awkward here as well, but at least it's
		       ;; reasonable efficient. 
		       (and (fboundp (intern (format "%s-request-update-info"
						     (car fmethod))))
			    (<= (gnus-info-level info) foreign-level)
			    (gnus-request-update-info info method)))))
	       (not (gnus-secondary-method-p method)))
	  ;; These groups are foreign.  Check the level.
	  (if (<= (gnus-info-level info) foreign-level)
	      (setq active (gnus-activate-group (gnus-info-group info) 'scan)))

	;; These groups are native or secondary. 
	(if (<= (gnus-info-level info) level)
	    (progn
	      (if (and update (not method))
		  (progn
		    ;; Allow updating of native groups as well, even
		    ;; though that's pretty unlikely.
		    (gnus-request-update-info info gnus-select-method)
		    (setq active (gnus-activate-group 
				  (gnus-info-group info) 'scan)))
		(or gnus-read-active-file
		    (setq active (gnus-activate-group 
				  (gnus-info-group info) 'scan)))))))
      
      (if active
	  (gnus-get-unread-articles-in-group info active)
	;; The group couldn't be reached, so we nix out the number of
	;; unread articles and stuff.
	(gnus-set-active group nil)
	(setcar (gnus-gethash group gnus-newsrc-hashtb) t))

      (setq newsrc (cdr newsrc)))

    (gnus-message 5 "Checking new news...done")))

;; Create a hash table out of the newsrc alist.  The `car's of the
;; alist elements are used as keys.
(defun gnus-make-hashtable-from-newsrc-alist ()
  (let ((alist gnus-newsrc-alist)
	(ohashtb gnus-newsrc-hashtb)
	prev)
    (setq gnus-newsrc-hashtb (gnus-make-hashtable (length alist)))
    (setq alist 
	  (setq prev (setq gnus-newsrc-alist 
			   (if (equal (car (car gnus-newsrc-alist))
				      "dummy.group")
			       gnus-newsrc-alist
			     (cons (list "dummy.group" 0 nil) alist)))))
    (while alist
      (gnus-sethash (car (car alist)) 
		    (cons (and ohashtb (car (gnus-gethash 
					     (car (car alist)) ohashtb))) 
			  prev) gnus-newsrc-hashtb)
      (setq prev alist
	    alist (cdr alist)))))

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
  (let* ((range (gnus-info-read info))
	 (num 0)
	 (marked (gnus-info-marks info)))
    ;; If a cache is present, we may have to alter the active info.
    (and gnus-use-cache
	 (gnus-cache-possibly-alter-active (gnus-info-group info) active))
    ;; Modify the list of read articles according to what articles 
    ;; are available; then tally the unread articles and add the
    ;; number to the group hash table entry.
    (cond 
     ((zerop (cdr active))
      (setq num 0))
     ((not range)
      (setq num (- (1+ (cdr active)) (car active))))
     ((not (listp (cdr range)))
      ;; Fix a single (num . num) range according to the
      ;; active hash table.
      ;; Fix by Carsten Bormann <cabo@Informatik.Uni-Bremen.DE>.
      (and (< (cdr range) (car active)) (setcdr range (1- (car active))))
      (and (> (cdr range) (cdr active)) (setcdr range (cdr active)))
      ;; Compute number of unread articles.
      (setq num (max 0 (- (cdr active) (- (1+ (cdr range)) (car range))))))
     (t
      ;; The read list is a list of ranges.  Fix them according to
      ;; the active hash table.
      ;; First peel off any elements that are below the lower
      ;; active limit. 
      (while (and (cdr range) 
		  (>= (car active) 
		      (or (and (atom (car (cdr range))) (car (cdr range)))
			  (car (car (cdr range))))))
	(if (numberp (car range))
	    (setcar range 
		    (cons (car range) 
			  (or (and (numberp (car (cdr range)))
				   (car (cdr range))) 
			      (cdr (car (cdr range))))))
	  (setcdr (car range) 
		  (or (and (numberp (nth 1 range)) (nth 1 range))
		      (cdr (car (cdr range))))))
	(setcdr range (cdr (cdr range))))
      ;; Adjust the first element to be the same as the lower limit. 
      (if (and (not (atom (car range))) 
	       (< (cdr (car range)) (car active)))
	  (setcdr (car range) (1- (car active))))
      ;; Then we want to peel off any elements that are higher
      ;; than the upper active limit.  
      (let ((srange range))
	;; Go past all legal elements.
	(while (and (cdr srange) 
		    (<= (or (and (atom (car (cdr srange)))
				 (car (cdr srange)))
			    (car (car (cdr srange)))) (cdr active)))
	  (setq srange (cdr srange)))
	(if (cdr srange)
	    ;; Nuke all remaining illegal elements.
	    (setcdr srange nil))

	;; Adjust the final element.
	(if (and (not (atom (car srange)))
		 (> (cdr (car srange)) (cdr active)))
	    (setcdr (car srange) (cdr active))))
      ;; Compute the number of unread articles.
      (while range
	(setq num (+ num (- (1+ (or (and (atom (car range)) (car range))
				    (cdr (car range))))
			    (or (and (atom (car range)) (car range))
				(car (car range))))))
	(setq range (cdr range)))
      (setq num (max 0 (- (cdr active) num)))))
    ;; Set the number of unread articles.
    (setcar (gnus-gethash (gnus-info-group info) gnus-newsrc-hashtb) num)
    num))

(defun gnus-activate-group (group &optional scan)
  ;; Check whether a group has been activated or not.
  ;; If SCAN, request a scan of that group as well.
  (let ((method (gnus-find-method-for-group group))
	active)
    (and (gnus-check-server method)
	 ;; We escape all bugs and quit here to make it possible to
	 ;; continue if a group is so out-there that it reports bugs
	 ;; and stuff.
	 (progn
	   (and scan
		(gnus-check-backend-function 'request-scan (car method))
		(gnus-request-scan group method))
	   t)
	 (condition-case ()
	     (gnus-request-group group)
	   (error nil)
	   (quit nil))
	 (save-excursion
	   (set-buffer nntp-server-buffer)
	   (goto-char (point-min))
	   ;; Parse the result we got from `gnus-request-group'.
	   (and (looking-at "[0-9]+ [0-9]+ \\([0-9]+\\) [0-9]+")
		(progn
		  (goto-char (match-beginning 1))
		  (gnus-set-active 
		   group (setq active (cons (read (current-buffer))
					    (read (current-buffer)))))
		  ;; Return the new active info.
		  active))))))

(defun gnus-update-read-articles 
  (group unread unselected ticked &optional domarks replied expirable killed
	 dormant bookmark score)
  "Update the list of read and ticked articles in GROUP using the
UNREAD and TICKED lists.
Note: UNSELECTED has to be sorted over `<'.
Returns whether the updating was successful."
  (let* ((active (or gnus-newsgroup-active (gnus-active group)))
	 (entry (gnus-gethash group gnus-newsrc-hashtb))
	 (info (nth 2 entry))
	 (marked (gnus-info-marks info))
	 (prev 1)
	 (unread (sort (copy-sequence unread) (function <)))
	 read)
    (if (or (not info) (not active))
	;; There is no info on this group if it was, in fact,
	;; killed.  Gnus stores no information on killed groups, so
	;; there's nothing to be done. 
	;; One could store the information somewhere temporarily,
	;; perhaps...  Hmmm... 
	()
      ;; Remove any negative articles numbers.
      (while (and unread (< (car unread) 0))
	(setq unread (cdr unread)))
      ;; Remove any expired article numbers
      (while (and unread (< (car unread) (car active)))
	(setq unread (cdr unread)))
      (while (and ticked (< (car ticked) (car active)))
	(setq ticked (cdr ticked)))
      (while (and dormant (< (car dormant) (car active)))
	(setq dormant (cdr dormant)))
      (setq unread (sort (append unselected unread) '<))
      ;; Compute the ranges of read articles by looking at the list of
      ;; unread articles.  
      (while unread
	(if (/= (car unread) prev)
	    (setq read (cons (if (= prev (1- (car unread))) prev
			       (cons prev (1- (car unread)))) read)))
	(setq prev (1+ (car unread)))
	(setq unread (cdr unread)))
      (if (<= prev (cdr active))
	  (setq read (cons (cons prev (cdr active)) read)))
      ;; Enter this list into the group info.
      (gnus-info-set-read 
       info (if (> (length read) 1) (nreverse read) read))
      ;; Enter the list of ticked articles.
      (gnus-set-marked-articles 
       info ticked
       (if domarks replied (cdr (assq 'reply marked)))
       (if domarks expirable (cdr (assq 'expire marked)))
       (if domarks killed (cdr (assq 'killed marked)))
       (if domarks dormant (cdr (assq 'dormant marked)))
       (if domarks bookmark (cdr (assq 'bookmark marked)))
       (if domarks score (cdr (assq 'score marked))))
      ;; Set the number of unread articles in gnus-newsrc-hashtb.
      (gnus-get-unread-articles-in-group info (gnus-active group))
      t)))

(defun gnus-make-articles-unread (group articles)
  "Mark ARTICLES in GROUP as unread."
  (let* ((info (nth 2 (or (gnus-gethash group gnus-newsrc-hashtb)
			  (gnus-gethash (gnus-group-real-name group)
					gnus-newsrc-hashtb))))
	 (ranges (gnus-info-read info))
	 news article)
    (while articles
      (when (gnus-member-of-range 
	     (setq article (pop articles)) ranges)
	(setq news (cons article news))))
    (when news
      (gnus-info-set-read 
       info (gnus-remove-from-range (gnus-info-read info) (nreverse news)))
      (gnus-group-update-group group t))))

;; Enter all dead groups into the hashtb.
(defun gnus-update-active-hashtb-from-killed ()
  (let ((hashtb (setq gnus-active-hashtb (make-vector 4095 0)))
	(lists (list gnus-killed-list gnus-zombie-list))
	killed)
    (while lists
      (setq killed (car lists))
      (while killed
	(gnus-sethash (car killed) nil hashtb)
	(setq killed (cdr killed)))
      (setq lists (cdr lists)))))

;; Get the active file(s) from the backend(s).
(defun gnus-read-active-file ()
  (gnus-group-set-mode-line)
  (let ((methods (if (gnus-check-server gnus-select-method)
		     ;; The native server is available.
		     (cons gnus-select-method gnus-secondary-select-methods)
		   ;; The native server is down, so we just do the
		   ;; secondary ones.   
		   gnus-secondary-select-methods))
	list-type)
    (setq gnus-have-read-active-file nil)
    (save-excursion
      (set-buffer nntp-server-buffer)
      (while methods
	(let* ((method (gnus-server-get-method nil (car methods)))
	       (where (nth 1 method))
	       (mesg (format "Reading active file%s via %s..."
			     (if (and where (not (zerop (length where))))
				 (concat " from " where) "")
			     (car method))))
	  (gnus-message 5 mesg)
	  (if (not (gnus-check-server method))
	      ()
	    ;; Request that the backend scan its incoming messages.
	    (and (gnus-check-backend-function 'request-scan (car method))
		 (gnus-request-scan nil method))
	    (cond 
	     ((and (eq gnus-read-active-file 'some)
		   (gnus-check-backend-function 'retrieve-groups (car method)))
	      (let ((newsrc (cdr gnus-newsrc-alist))
		    (gmethod (gnus-server-get-method nil method))
		    groups)
		(while newsrc
		  (and (gnus-server-equal 
			(gnus-find-method-for-group 
			 (car (car newsrc)) (car newsrc))
			gmethod)
		       (setq groups (cons (gnus-group-real-name 
					   (car (car newsrc))) groups)))
		  (setq newsrc (cdr newsrc)))
		(gnus-check-server method)
		(setq list-type (gnus-retrieve-groups groups method))
		(cond 
		 ((not list-type)
		  (gnus-message 
		   1 "Cannot read partial active file from %s server." 
		   (car method))
		  (ding)
		  (sit-for 2))
		 ((eq list-type 'active)
		  (gnus-active-to-gnus-format method gnus-active-hashtb))
		 (t
		  (gnus-groups-to-gnus-format method gnus-active-hashtb)))))
	     (t
	      (if (not (gnus-request-list method))
		  (progn
		    (gnus-message 1 "Cannot read active file from %s server." 
				  (car method))
		    (ding))
		(gnus-active-to-gnus-format method)
		;; We mark this active file as read.
		(setq gnus-have-read-active-file
		      (cons method gnus-have-read-active-file))
		(gnus-message 5 "%sdone" mesg))))))
	(setq methods (cdr methods))))))

;; Read an active file and place the results in `gnus-active-hashtb'.
(defun gnus-active-to-gnus-format (method &optional hashtb ignore-errors)
  (let ((cur (current-buffer))
	(hashtb (or hashtb 
		    (if (and gnus-active-hashtb 
			     (not (equal method gnus-select-method)))
			gnus-active-hashtb
		      (setq gnus-active-hashtb
			    (if (equal method gnus-select-method)
				(gnus-make-hashtable 
				 (count-lines (point-min) (point-max)))
			      (gnus-make-hashtable 4096))))))
	(flag-hashtb (gnus-make-hashtable 60)))
    ;; Delete unnecessary lines.
    (goto-char (point-min))
    (while (search-forward "\nto." nil t)
      (delete-region (1+ (match-beginning 0)) 
		     (progn (forward-line 1) (point))))
    (or (string= gnus-ignored-newsgroups "")
	(progn
	  (goto-char (point-min))
	  (delete-matching-lines gnus-ignored-newsgroups)))
    ;; Make the group names readable as a lisp expression even if they
    ;; contain special characters.
    ;; Fix by Luc Van Eycken <Luc.VanEycken@esat.kuleuven.ac.be>.
    (goto-char (point-max))
    (while (re-search-backward "[][';?()#]" nil t)
      (insert ?\\))
    ;; If these are groups from a foreign select method, we insert the
    ;; group prefix in front of the group names. 
    (and method (not (gnus-server-equal
		      (gnus-server-get-method nil method)
		      (gnus-server-get-method nil gnus-select-method)))
	 (let ((prefix (gnus-group-prefixed-name "" method)))
	   (goto-char (point-min))
	   (while (and (not (eobp))
		       (progn (insert prefix)
			      (zerop (forward-line 1)))))))
    ;; Store the active file in a hash table.
    (goto-char (point-min))
    (if (string-match "%[oO]" gnus-group-line-format)
	;; Suggested by Brian Edmonds <edmonds@cs.ubc.ca>.
	;; If we want information on moderated groups, we use this
	;; loop...   
	(let* ((mod-hashtb (make-vector 7 0))
	       (m (intern "m" mod-hashtb))
	       group max min)
	  (while (not (eobp))
	    (condition-case nil
		(progn
		  (narrow-to-region (point) (gnus-point-at-eol))
		  (setq group (let ((obarray hashtb)) (read cur)))
		  (if (and (numberp (setq max (read cur)))
			   (numberp (setq min (read cur)))
			   (progn 
			     (skip-chars-forward " \t")
			     (not
			      (or (= (following-char) ?=)
				  (= (following-char) ?x)
				  (= (following-char) ?j)))))
		      (set group (cons min max))
		    (set group nil))
		  ;; Enter moderated groups into a list.
		  (if (eq (let ((obarray mod-hashtb)) (read cur)) m)
		      (setq gnus-moderated-list 
			    (cons (symbol-name group) gnus-moderated-list))))
	      (error 
	       (and group
		    (symbolp group)
		    (set group nil))))
	    (widen)
	    (forward-line 1)))
      ;; And if we do not care about moderation, we use this loop,
      ;; which is faster.
      (let (group max min)
	(while (not (eobp))
	  (condition-case ()
	      (progn
		(narrow-to-region (point) (gnus-point-at-eol))
		;; group gets set to a symbol interned in the hash table
		;; (what a hack!!) - jwz
		(setq group (let ((obarray hashtb)) (read cur)))
		(if (and (numberp (setq max (read cur)))
			 (numberp (setq min (read cur)))
			 (progn 
			   (skip-chars-forward " \t")
			   (not
			    (or (= (following-char) ?=)
				(= (following-char) ?x)
				(= (following-char) ?j)))))
		    (set group (cons min max))
		  (set group nil)))
	    (error 
	     (progn 
	       (and group
		    (symbolp group)
		    (set group nil))
	       (or ignore-errors
		   (gnus-message 3 "Warning - illegal active: %s"
				 (buffer-substring 
				  (gnus-point-at-bol) (gnus-point-at-eol)))))))
	  (widen)
	  (forward-line 1))))))

(defun gnus-groups-to-gnus-format (method &optional hashtb)
  ;; Parse a "groups" active file.
  (let ((cur (current-buffer))
	(hashtb (or hashtb 
		    (if (and method gnus-active-hashtb)
			gnus-active-hashtb
		      (setq gnus-active-hashtb
			    (gnus-make-hashtable 
			     (count-lines (point-min) (point-max)))))))
	(prefix (and method 
		     (not (gnus-server-equal
			   (gnus-server-get-method nil method)
			   (gnus-server-get-method nil gnus-select-method)))
		     (gnus-group-prefixed-name "" method))))

    (goto-char (point-min))
    ;; We split this into to separate loops, one with the prefix
    ;; and one without to speed the reading up somewhat.
    (if prefix
	(let (min max opoint group)
	  (while (not (eobp))
	    (condition-case ()
		(progn
		  (read cur) (read cur)
		  (setq min (read cur)
			max (read cur)
			opoint (point))
		  (skip-chars-forward " \t")
		  (insert prefix)
		  (goto-char opoint)
		  (set (let ((obarray hashtb)) (read cur)) 
		       (cons min max)))
	      (error (and group (symbolp group) (set group nil))))
	    (forward-line 1)))
      (let (min max group)
	(while (not (eobp))
	  (condition-case ()
	      (if (= (following-char) ?2)
		  (progn
		    (read cur) (read cur)
		    (setq min (read cur)
			  max (read cur))
		    (set (setq group (let ((obarray hashtb)) (read cur)))
			 (cons min max))))
	    (error (and group (symbolp group) (set group nil))))
	  (forward-line 1))))))

(defun gnus-read-newsrc-file (&optional force)
  "Read startup file.
If FORCE is non-nil, the .newsrc file is read."
  ;; Reset variables that might be defined in the .newsrc.eld file.
  (let ((variables gnus-variable-list))
    (while variables
      (set (car variables) nil)
      (setq variables (cdr variables))))
  (let* ((newsrc-file gnus-current-startup-file)
	 (quick-file (concat newsrc-file ".el")))
    (save-excursion
      ;; We always load the .newsrc.eld file.  If always contains
      ;; much information that can not be gotten from the .newsrc
      ;; file (ticked articles, killed groups, foreign methods, etc.)
      (gnus-read-newsrc-el-file quick-file)
 
      (if (or force
	      (and (file-newer-than-file-p newsrc-file quick-file)
		   (file-newer-than-file-p newsrc-file 
					   (concat quick-file "d")))
	      (not gnus-newsrc-alist))
	  ;; We read the .newsrc file.  Note that if there if a
	  ;; .newsrc.eld file exists, it has already been read, and
	  ;; the `gnus-newsrc-hashtb' has been created.  While reading
	  ;; the .newsrc file, Gnus will only use the information it
	  ;; can find there for changing the data already read -
	  ;; ie. reading the .newsrc file will not trash the data
	  ;; already read (except for read articles).
	  (save-excursion
	    (gnus-message 5 "Reading %s..." newsrc-file)
	    (set-buffer (find-file-noselect newsrc-file))
	    (buffer-disable-undo (current-buffer))
	    (gnus-newsrc-to-gnus-format)
	    (kill-buffer (current-buffer))
	    (gnus-message 5 "Reading %s...done" newsrc-file)))

      ;; Read any slave files.
      (or gnus-slave
	  (gnus-master-read-slave-newsrc)))))

(defun gnus-read-newsrc-el-file (file)
  (let ((ding-file (concat file "d")))
    ;; We always, always read the .eld file.
    (gnus-message 5 "Reading %s..." ding-file)
    (let (gnus-newsrc-assoc)
      (condition-case nil
	  (load ding-file t t t)
	(error
	 (gnus-message 1 "Error in %s" ding-file)
	 (ding)))
      (and gnus-newsrc-assoc (setq gnus-newsrc-alist gnus-newsrc-assoc)))
    (let ((inhibit-quit t))
      (gnus-uncompress-newsrc-alist))
    (gnus-make-hashtable-from-newsrc-alist)
    (if (not (file-newer-than-file-p file ding-file))
	()
      ;; Old format quick file
      (gnus-message 5 "Reading %s..." file)
      ;; The .el file is newer than the .eld file, so we read that one
      ;; as well. 
      (gnus-read-old-newsrc-el-file file))))

;; Parse the old-style quick startup file
(defun gnus-read-old-newsrc-el-file (file)
  (let (newsrc killed marked group m)
    (prog1
	(let ((gnus-killed-assoc nil)
	      gnus-marked-assoc gnus-newsrc-alist gnus-newsrc-assoc)
	  (prog1
	      (condition-case nil
		  (load file t t t)
		(error nil))
	    (setq newsrc gnus-newsrc-assoc
		  killed gnus-killed-assoc
		  marked gnus-marked-assoc)))
      (setq gnus-newsrc-alist nil)
      (while newsrc
	(setq group (car newsrc))
	(let ((info (gnus-get-info (car group))))
	  (if info
	      (progn
		(gnus-info-set-read info (cdr (cdr group)))
		(gnus-info-set-level
		 info (if (nth 1 group) gnus-level-default-subscribed 
			gnus-level-default-unsubscribed))
		(setq gnus-newsrc-alist (cons info gnus-newsrc-alist)))
	    (setq gnus-newsrc-alist
		  (cons 
		   (setq info
			 (list (car group)
			       (if (nth 1 group) gnus-level-default-subscribed
				 gnus-level-default-unsubscribed) 
			       (cdr (cdr group))))
		   gnus-newsrc-alist)))
	  (if (setq m (assoc (car group) marked))
	      (gnus-info-set-marks 
	       info (cons (list (cons 'tick (cdr m))) nil))))
	(setq newsrc (cdr newsrc)))
      (setq newsrc killed)
      (while newsrc
	(setcar newsrc (car (car newsrc)))
	(setq newsrc (cdr newsrc)))
      (setq gnus-killed-list killed))
    ;; The .el file version of this variable does not begin with
    ;; "options", while the .eld version does, so we just add it if it
    ;; isn't there.
    (and
     gnus-newsrc-options 
     (progn
       (and (not (string-match "^ *options" gnus-newsrc-options))
	    (setq gnus-newsrc-options (concat "options " gnus-newsrc-options)))
       (and (not (string-match "\n$" gnus-newsrc-options))
	    (setq gnus-newsrc-options (concat gnus-newsrc-options "\n")))
       ;; Finally, if we read some options lines, we parse them.
       (or (string= gnus-newsrc-options "")
	   (gnus-newsrc-parse-options gnus-newsrc-options))))

    (setq gnus-newsrc-alist (nreverse gnus-newsrc-alist))
    (gnus-make-hashtable-from-newsrc-alist)))
      
(defun gnus-make-newsrc-file (file)
  "Make server dependent file name by catenating FILE and server host name."
  (let* ((file (expand-file-name file nil))
	 (real-file (concat file "-" (nth 1 gnus-select-method))))
    (if (or (file-exists-p real-file)
	    (file-exists-p (concat real-file ".el"))
	    (file-exists-p (concat real-file ".eld")))
	real-file file)))

(defun gnus-uncompress-newsrc-alist ()
  ;; Uncompress all lists of marked articles in the newsrc assoc.
  (let ((newsrc gnus-newsrc-alist)
	marked)
    (while newsrc
      (if (not (setq marked (nth 3 (car newsrc))))
	  ()
	(while marked
	  (or (eq 'score (car (car marked)))
	      (eq 'bookmark (car (car marked)))
	      (eq 'killed (car (car marked)))
	      (setcdr (car marked) (gnus-uncompress-range (cdr (car marked)))))
	  (setq marked (cdr marked))))
      (setq newsrc (cdr newsrc)))))

(defun gnus-compress-newsrc-alist ()
  ;; Compress all lists of marked articles in the newsrc assoc.
  (let ((newsrc gnus-newsrc-alist)
	marked)
    (while newsrc
      (if (not (setq marked (nth 3 (car newsrc))))
	  ()
	(while marked
	  (or (eq 'score (car (car marked)))
	      (eq 'bookmark (car (car marked)))
	      (eq 'killed (car (car marked)))
	      (setcdr (car marked) 
		      (condition-case ()
			  (gnus-compress-sequence 
			   (sort (cdr (car marked)) '<) t)
			(error (cdr (car marked))))))
	  (setq marked (cdr marked))))
      (setq newsrc (cdr newsrc)))))

(defun gnus-newsrc-to-gnus-format ()
  (setq gnus-newsrc-options "")
  (setq gnus-newsrc-options-n nil)

  (or gnus-active-hashtb
      (setq gnus-active-hashtb (make-vector 4095 0)))
  (let ((buf (current-buffer))
	(already-read (> (length gnus-newsrc-alist) 1))
	group subscribed options-symbol newsrc Options-symbol
	symbol reads num1)
    (goto-char (point-min))
    ;; We intern the symbol `options' in the active hashtb so that we
    ;; can `eq' against it later.
    (set (setq options-symbol (intern "options" gnus-active-hashtb)) nil)
    (set (setq Options-symbol (intern "Options" gnus-active-hashtb)) nil)
  
    (while (not (eobp))
      ;; We first read the first word on the line by narrowing and
      ;; then reading into `gnus-active-hashtb'.  Most groups will
      ;; already exist in that hashtb, so this will save some string
      ;; space.
      (narrow-to-region
       (point)
       (progn (skip-chars-forward "^ \t!:\n") (point)))
      (goto-char (point-min))
      (setq symbol 
	    (and (/= (point-min) (point-max))
		 (let ((obarray gnus-active-hashtb)) (read buf))))
      (widen)
      ;; Now, the symbol we have read is either `options' or a group
      ;; name.  If it is an options line, we just add it to a string. 
      (cond 
       ((or (eq symbol options-symbol)
	    (eq symbol Options-symbol))
	(setq gnus-newsrc-options
	      ;; This concatting is quite inefficient, but since our
	      ;; thorough studies show that approx 99.37% of all
	      ;; .newsrc files only contain a single options line, we
	      ;; don't give a damn, frankly, my dear.
	      (concat gnus-newsrc-options
		      (buffer-substring 
		       (gnus-point-at-bol)
		       ;; Options may continue on the next line.
		       (or (and (re-search-forward "^[^ \t]" nil 'move)
				(progn (beginning-of-line) (point)))
			   (point)))))
	(forward-line -1))
       (symbol
	(or (boundp symbol) (set symbol nil))
	;; It was a group name.
	(setq subscribed (= (following-char) ?:)
	      group (symbol-name symbol)
	      reads nil)
	(if (eolp)
	    ;; If the line ends here, this is clearly a buggy line, so
	    ;; we put point a the beginning of line and let the cond
	    ;; below do the error handling.
	    (beginning-of-line)
	  ;; We skip to the beginning of the ranges.
	  (skip-chars-forward "!: \t"))
	;; We are now at the beginning of the list of read articles.
	;; We read them range by range.
	(while
	    (cond 
	     ((looking-at "[0-9]+")
	      ;; We narrow and read a number instead of buffer-substring/
	      ;; string-to-int because it's faster.  narrow/widen is
	      ;; faster than save-restriction/narrow, and save-restriction
	      ;; produces a garbage object.
	      (setq num1 (progn
			   (narrow-to-region (match-beginning 0) (match-end 0))
			   (read buf)))
	      (widen)
	      ;; If the next character is a dash, then this is a range.
	      (if (= (following-char) ?-)
		  (progn
		    ;; We read the upper bound of the range.
		    (forward-char 1)
		    (if (not (looking-at "[0-9]+"))
			;; This is a buggy line, by we pretend that
			;; it's kinda OK.  Perhaps the user should be
			;; dinged? 
			(setq reads (cons num1 reads))
		      (setq reads 
			    (cons 
			     (cons num1
				   (progn
				     (narrow-to-region (match-beginning 0) 
						       (match-end 0))
				     (read buf)))
			     reads))
		      (widen)))
		;; It was just a simple number, so we add it to the
		;; list of ranges.
		(setq reads (cons num1 reads)))
	      ;; If the next char in ?\n, then we have reached the end
	      ;; of the line and return nil.
	      (/= (following-char) ?\n))
	     ((= (following-char) ?\n)
	      ;; End of line, so we end.
	      nil)
	     (t
	      ;; Not numbers and not eol, so this might be a buggy
	      ;; line... 
	      (or (eobp)		
		  ;; If it was eob instead of ?\n, we allow it.
		  (progn
		    ;; The line was buggy.
		    (setq group nil)
		    (gnus-message 3 "Mangled line: %s" 
				  (buffer-substring (gnus-point-at-bol) 
						    (gnus-point-at-eol)))
		    (ding)
		    (sit-for 1)))
	      nil))
	  ;; Skip past ", ".  Spaces are illegal in these ranges, but
	  ;; we allow them, because it's a common mistake to put a
	  ;; space after the comma.
	  (skip-chars-forward ", "))

	;; We have already read .newsrc.eld, so we gently update the
	;; data in the hash table with the information we have just
	;; read. 
	(when group
	  (let ((info (gnus-get-info group))
		level)
	    (if info
		;; There is an entry for this file in the alist.
		(progn
		  (gnus-info-set-read info (nreverse reads))
		  ;; We update the level very gently.  In fact, we
		  ;; only change it if there's been a status change
		  ;; from subscribed to unsubscribed, or vice versa.
		  (setq level (gnus-info-level info))
		  (cond ((and (<= level gnus-level-subscribed)
			      (not subscribed))
			 (setq level (if reads
					 gnus-level-default-unsubscribed 
				       (1+ gnus-level-default-unsubscribed))))
			((and (> level gnus-level-subscribed) subscribed)
			 (setq level gnus-level-default-subscribed)))
		  (gnus-info-set-level info level))
	      ;; This is a new group.
	      (setq info (list group 
			       (if subscribed
				   gnus-level-default-subscribed 
				 (if reads
				     (1+ gnus-level-subscribed)
				   gnus-level-default-unsubscribed))
			       (nreverse reads))))
	    (setq newsrc (cons info newsrc))))))
      (forward-line 1))
    
    (setq newsrc (nreverse newsrc))

    (if (not already-read)
	()
      ;; We now have two newsrc lists - `newsrc', which is what we
      ;; have read from .newsrc, and `gnus-newsrc-alist', which is
      ;; what we've read from .newsrc.eld.  We have to merge these
      ;; lists.  We do this by "attaching" any (foreign) groups in the
      ;; gnus-newsrc-alist to the (native) group that precedes them. 
      (let ((rc (cdr gnus-newsrc-alist))
	    (prev gnus-newsrc-alist)
	    entry mentry)
	(while rc
	  (or (null (nth 4 (car rc)))	; It's a native group.
	      (assoc (car (car rc)) newsrc) ; It's already in the alist.
	      (if (setq entry (assoc (car (car prev)) newsrc))
		  (setcdr (setq mentry (memq entry newsrc))
			  (cons (car rc) (cdr mentry)))
		(setq newsrc (cons (car rc) newsrc))))
	  (setq prev rc
		rc (cdr rc)))))

    (setq gnus-newsrc-alist newsrc)
    ;; We make the newsrc hashtb.
    (gnus-make-hashtable-from-newsrc-alist)

    ;; Finally, if we read some options lines, we parse them.
    (or (string= gnus-newsrc-options "")
	(gnus-newsrc-parse-options gnus-newsrc-options))))

;; Parse options lines to find "options -n !all rec.all" and stuff.
;; The return value will be a list on the form
;; ((regexp1 . ignore)
;;  (regexp2 . subscribe)...)
;; When handling new newsgroups, groups that match a `ignore' regexp
;; will be ignored, and groups that match a `subscribe' regexp will be
;; subscribed.  A line like
;; options -n !all rec.all
;; will lead to a list that looks like
;; (("^rec\\..+" . subscribe) 
;;  ("^.+" . ignore))
;; So all "rec.*" groups will be subscribed, while all the other
;; groups will be ignored.  Note that "options -n !all rec.all" is very
;; different from "options -n rec.all !all". 
(defun gnus-newsrc-parse-options (options)
  (let (out eol)
    (save-excursion
      (gnus-set-work-buffer)
      (insert (regexp-quote options))
      ;; First we treat all continuation lines.
      (goto-char (point-min))
      (while (re-search-forward "\n[ \t]+" nil t)
	(replace-match " " t t))
      ;; Then we transform all "all"s into ".+"s.
      (goto-char (point-min))
      (while (re-search-forward "\\ball\\b" nil t)
	(replace-match ".+" t t))
      (goto-char (point-min))
      ;; We remove all other options than the "-n" ones.
      (while (re-search-forward "[ \t]-[^n][^-]*" nil t)
	(replace-match " ")
	(forward-char -1))
      (goto-char (point-min))

      ;; We are only interested in "options -n" lines - we
      ;; ignore the other option lines.
      (while (re-search-forward "[ \t]-n" nil t)
	(setq eol 
	      (or (save-excursion
		    (and (re-search-forward "[ \t]-n" (gnus-point-at-eol) t)
			 (- (point) 2)))
		  (gnus-point-at-eol)))
	;; Search for all "words"...
	(while (re-search-forward "[^ \t,\n]+" eol t)
	  (if (= (char-after (match-beginning 0)) ?!)
	      ;; If the word begins with a bang (!), this is a "not"
	      ;; spec.  We put this spec (minus the bang) and the
	      ;; symbol `ignore' into the list.
	      (setq out (cons (cons (concat 
				     "^" (buffer-substring 
					  (1+ (match-beginning 0))
					  (match-end 0)))
				    'ignore) out))
	    ;; There was no bang, so this is a "yes" spec.
	    (setq out (cons (cons (concat "^" (match-string 0))
				  'subscribe) out)))))
    
      (setq gnus-newsrc-options-n out))))

(defun gnus-save-newsrc-file ()
  "Save .newsrc file."
  ;; Note: We cannot save .newsrc file if all newsgroups are removed
  ;; from the variable gnus-newsrc-alist.
  (and (or gnus-newsrc-alist gnus-killed-list)
       gnus-current-startup-file
       (progn
	 (save-excursion
	   (if (and (or gnus-use-dribble-file gnus-slave)
		    (or (not gnus-dribble-buffer)
			(not (buffer-name gnus-dribble-buffer))
			(zerop (save-excursion
				 (set-buffer gnus-dribble-buffer)
				 (buffer-size)))))
	       (gnus-message 4 "(No changes need to be saved)")
	     (run-hooks 'gnus-save-newsrc-hook)
	     (if gnus-slave
		 (gnus-slave-save-newsrc)
	       (if gnus-save-newsrc-file
		   (progn
		     (gnus-message 5 "Saving %s..." gnus-current-startup-file)
		     ;; Make backup file of master newsrc.
		     (gnus-gnus-to-newsrc-format)
		     (gnus-message 5 "Saving %s...done"
				   gnus-current-startup-file)))
	       ;; Quickly loadable .newsrc.
	       (set-buffer (get-buffer-create " *Gnus-newsrc*"))
	       (make-local-variable 'version-control)
	       (setq version-control 'never)
	       (setq buffer-file-name 
		     (concat gnus-current-startup-file ".eld"))
	       (gnus-add-current-to-buffer-list)
	       (buffer-disable-undo (current-buffer))
	       (erase-buffer)
	       (gnus-message 5 "Saving %s.eld..." gnus-current-startup-file)
	       (gnus-gnus-to-quick-newsrc-format)
	       (run-hooks 'gnus-save-quick-newsrc-hook)
	       (save-buffer)
	       (kill-buffer (current-buffer))
	       (gnus-message 
		5 "Saving %s.eld...done" gnus-current-startup-file))
	     (gnus-dribble-delete-file))))))

(defun gnus-gnus-to-quick-newsrc-format ()
  "Insert Gnus variables such as gnus-newsrc-alist in lisp format."
  (insert ";; Gnus startup file.\n")
  (insert ";; Never delete this file - touch .newsrc instead to force Gnus\n")
  (insert ";; to read .newsrc.\n")
  (insert "(setq gnus-newsrc-file-version "
	  (prin1-to-string gnus-version) ")\n")
  (let ((variables gnus-variable-list)
	(inhibit-quit t)
	(gnus-newsrc-alist (cdr gnus-newsrc-alist))
	variable)
    ;; insert lisp expressions.
    (gnus-compress-newsrc-alist)
    (while variables
      (setq variable (car variables))
      (and (boundp variable)
	   (symbol-value variable)
	   (or gnus-save-killed-list (not (eq variable 'gnus-killed-list)))
	   (insert "(setq " (symbol-name variable) " '"
		   (prin1-to-string (symbol-value variable))
		   ")\n"))
      (setq variables (cdr variables)))
    (gnus-uncompress-newsrc-alist)))

(defun gnus-gnus-to-newsrc-format ()
  ;; Generate and save the .newsrc file.
  (let ((newsrc (cdr gnus-newsrc-alist))
	info ranges range)
    (save-excursion
      (set-buffer (create-file-buffer gnus-current-startup-file))
      (setq buffer-file-name gnus-current-startup-file)
      (buffer-disable-undo (current-buffer))
      (erase-buffer)
      ;; Write options.
      (if gnus-newsrc-options (insert gnus-newsrc-options))
      ;; Write subscribed and unsubscribed.
      (while newsrc
	(setq info (car newsrc))
	(if (not (gnus-info-method info))
	    ;; Don't write foreign groups to .newsrc.
	    (progn
	      (insert (gnus-info-group info)
		      (if (> (nth 1 info) gnus-level-subscribed)
			  "!" ":"))
	      (if (setq ranges (gnus-info-read info))
		  (progn
		    (insert " ")
		    (if (not (listp (cdr ranges)))
			(if (= (car ranges) (cdr ranges))
			    (insert (int-to-string (car ranges)))
			  (insert (int-to-string (car ranges)) "-" 
				  (int-to-string (cdr ranges))))
		      (while ranges
			(setq range (car ranges)
			      ranges (cdr ranges))
			(if (or (atom range) (= (car range) (cdr range)))
			    (insert (int-to-string 
				     (or (and (atom range) range) 
					 (car range))))
			  (insert (int-to-string (car range)) "-"
				  (int-to-string (cdr range))))
			(if ranges (insert ","))))))
	      (insert "\n")))
	(setq newsrc (cdr newsrc)))
      (make-local-variable 'version-control)
      (setq version-control 'never)
      ;; It has been reported that sometime the modtime on the .newsrc
      ;; file seems to be off.  We really do want to overwrite it, so
      ;; we clear the modtime here before saving.  It's a bit odd,
      ;; though... 
      ;; sometimes the modtime clear isn't sufficient.  most brute force:
      ;; delete the silly thing entirely first.  but this fails to provide
      ;; such niceties as .newsrc~ creation.
      (if gnus-modtime-botch
	  (delete-file gnus-startup-file)
	(clear-visited-file-modtime))
      (run-hooks 'gnus-save-standard-newsrc-hook)
      (save-buffer)
      (kill-buffer (current-buffer)))))


;;; Slave functions.

(defun gnus-slave-save-newsrc ()
  (save-excursion
    (set-buffer gnus-dribble-buffer)
    (let ((slave-name 
	   (make-temp-name (concat gnus-current-startup-file "-slave-"))))
      (write-region (point-min) (point-max) slave-name nil 'nomesg))))

(defun gnus-master-read-slave-newsrc ()
  (let ((slave-files 
	 (directory-files 
	  (file-name-directory gnus-current-startup-file)
	  t (concat 
	     "^" (regexp-quote
		  (concat
		   (file-name-nondirectory gnus-current-startup-file)
		   "-slave-")))
	  t))
	file)
    (if (not slave-files)
	()				; There are no slave files to read.
      (gnus-message 7 "Reading slave newsrcs...")
      (save-excursion
	(set-buffer (get-buffer-create " *gnus slave*"))
	(buffer-disable-undo (current-buffer))
	(setq slave-files 
	      (sort (mapcar (lambda (file) 
			      (list (nth 5 (file-attributes file)) file))
			    slave-files)
		    (lambda (f1 f2)
		      (or (< (car (car f1)) (car (car f2)))
			  (< (nth 1 (car f1)) (nth 1 (car f2)))))))
	(while slave-files
	  (erase-buffer)
	  (setq file (nth 1 (car slave-files)))
	  (insert-file-contents file)
	  (if (condition-case ()
		  (progn
		    (eval-buffer (current-buffer))
		    t)
		(error 
		 (message "Possible error in %s" file)
		 (ding)
		 (sit-for 2)
		 nil))
	      (or gnus-slave ; Slaves shouldn't delete these files.
		  (condition-case ()
		      (delete-file file)
		    (error nil))))
	  (setq slave-files (cdr slave-files))))
      (gnus-message 7 "Reading slave newsrcs...done"))))


;;; Group description.

(defun gnus-read-all-descriptions-files ()
  (let ((methods (cons gnus-select-method gnus-secondary-select-methods)))
    (while methods
      (gnus-read-descriptions-file (car methods))
      (setq methods (cdr methods)))
    t))

(defun gnus-read-descriptions-file (&optional method)
  (let ((method (or method gnus-select-method)))
    ;; We create the hashtable whether we manage to read the desc file
    ;; to avoid trying to re-read after a failed read.
    (or gnus-description-hashtb
	(setq gnus-description-hashtb 
	      (gnus-make-hashtable (length gnus-active-hashtb))))
    ;; Mark this method's desc file as read.
    (gnus-sethash (gnus-group-prefixed-name "" method) "Has read"
		  gnus-description-hashtb)

    (gnus-message 5 "Reading descriptions file via %s..." (car method))
    (cond 
     ((not (gnus-check-server method))
      (gnus-message 1 "Couldn't open server")
      nil)
     ((not (gnus-request-list-newsgroups method))
      (gnus-message 1 "Couldn't read newsgroups descriptions")
      nil)
     (t
      (let (group)
	(save-excursion
	  (save-restriction
	    (set-buffer nntp-server-buffer)
	    (goto-char (point-min))
	    (if (or (search-forward "\n.\n" nil t)
		    (goto-char (point-max)))
		(progn
		  (beginning-of-line)
		  (narrow-to-region (point-min) (point))))
	    (goto-char (point-min))
	    (while (not (eobp))
	      ;; If we get an error, we set group to 0, which is not a
	      ;; symbol... 
	      (setq group 
		    (condition-case ()
			(let ((obarray gnus-description-hashtb))
			  ;; Group is set to a symbol interned in this
			  ;; hash table.
			  (read nntp-server-buffer))
		      (error 0)))
	      (skip-chars-forward " \t")
	      ;; ...  which leads to this line being effectively ignored.
	      (and (symbolp group)
		   (set group (buffer-substring 
			       (point) (progn (end-of-line) (point)))))
	      (forward-line 1))))
	(gnus-message 5 "Reading descriptions file...done")
	t)))))

(defun gnus-group-get-description (group)
  "Get the description of a group by sending XGTITLE to the server."
  (when (gnus-request-group-description group)
    (save-excursion
      (set-buffer nntp-server-buffer)
      (goto-char (point-min))
      (when (looking-at "[^ \t]+[ \t]+\\(.*\\)")
	(match-string 1)))))

;;;
;;; Buffering of read articles.
;;;

(defvar gnus-backlog-buffer " *Gnus Backlog*")
(defvar gnus-backlog-articles nil)
(defvar gnus-backlog-hashtb nil)

(defun gnus-backlog-buffer ()
  (or (get-buffer gnus-backlog-buffer)
      (save-excursion
	(set-buffer (get-buffer-create gnus-backlog-buffer))
	(buffer-disable-undo (current-buffer))
	(setq buffer-read-only t)
	(gnus-add-current-to-buffer-list))))

(defun gnus-backlog-setup ()
  "Initialize backlog variables."
  (unless gnus-backlog-hashtb
    (setq gnus-backlog-hashtb (make-vector 1023 0))))

(defun gnus-backlog-enter-article (group number buffer)
  (gnus-backlog-setup)
  (let ((ident (intern (concat group ":" (int-to-string number))
		       gnus-backlog-hashtb))
	b)
    (if (memq ident gnus-backlog-articles)
	() ; It's already kept.
      ;; Remove the oldest article, if necessary.
      (and (numberp gnus-keep-backlog)
	   (>= (length gnus-backlog-articles) gnus-keep-backlog)
	   (gnus-backlog-remove-oldest-article))
      (setq gnus-backlog-articles (cons ident gnus-backlog-articles))
      ;; Insert the new article.
      (save-excursion
	(set-buffer (gnus-backlog-buffer))
	(let (buffer-read-only)
	  (goto-char (point-max))
	  (or (bolp) (insert "\n"))
	  (setq b (point))
	  (insert-buffer-substring buffer)
	  ;; Tag the beginning of the article with the ident.
	  (put-text-property b (1+ b) 'gnus-backlog ident))))))

(defun gnus-backlog-remove-oldest-article ()
  (save-excursion
    (set-buffer (gnus-backlog-buffer))
    (goto-char (point-min))
    (if (zerop (buffer-size))
	() ; The buffer is empty.
      (let ((ident (get-text-property (point) 'gnus-backlog))
	    buffer-read-only)
	;; Remove the ident from the list of articles.
	(and ident
	     (setq gnus-backlog-articles (delq ident gnus-backlog-articles)))
	;; Delete the article itself.
	(delete-region 
	 (point) (next-single-property-change
		  (1+ (point)) 'gnus-backlog nil (point-max)))))))

(defun gnus-backlog-request-article (group number buffer)
  (gnus-backlog-setup)
  (let ((ident (intern (concat group ":" (int-to-string number))
		       gnus-backlog-hashtb))
	beg end)
    (if (not (memq ident gnus-backlog-articles))
	() ; It wasn't in the backlog.
      (save-excursion
	(set-buffer (gnus-backlog-buffer))
	(if (not (setq beg (text-property-any 
			    (point-min) (point-max) 'gnus-backlog
			    ident)))
	    ;; It wasn't in the backlog after all.
	    (progn
	      (setq gnus-backlog-articles (delq ident gnus-backlog-articles))
	      nil)
	  ;; Find the end (i. e., the beginning of the next article).
	  (setq end
		(next-single-property-change 
		 (1+ beg) 'gnus-backlog (current-buffer) (point-max)))))
      (erase-buffer)
      (insert-buffer-substring gnus-backlog-buffer beg end))))

;; Allow redefinition of Gnus functions.

(gnus-ems-redefine)

(provide 'gnus)

;;; gnus.el ends here
