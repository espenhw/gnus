;;; gnus-load.el --- various Gnus variables
;; Copyright (C) 1996 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@ifi.uio.no>
;; Keywords: news

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

;;; Code:

(require 'gnus-util)
(require 'nnheader)

(defvar gnus-directory (or (getenv "SAVEDIR") "~/News/")
  "*Directory variable from which all other Gnus file variables are derived.")

(defvar gnus-default-directory nil
  "*Default directory for all Gnus buffers.")

;; Site dependent variables.  These variables should be defined in
;; paths.el.

(defvar gnus-default-nntp-server nil
  "Specify a default NNTP server.
This variable should be defined in paths.el, and should never be set
by the user.
If you want to change servers, you should use `gnus-select-method'.
See the documentation to that variable.")

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

(defvar gnus-message-archive-method 
  `(nnfolder
    "archive"
    (nnfolder-directory ,(nnheader-concat message-directory "archive"))
    (nnfolder-active-file 
     ,(nnheader-concat message-directory "archive/active"))
    (nnfolder-get-new-mail nil)
    (nnfolder-inhibit-expiry t))
  "*Method used for archiving messages you've sent.
This should be a mail method.

It's probably not a very effective to change this variable once you've
run Gnus once.  After doing that, you must edit this server from the
server buffer.")

(defvar gnus-message-archive-group nil
  "*Name of the group in which to save the messages you've written.
This can either be a string, a list of strings; or an alist
of regexps/functions/forms to be evaluated to return a string (or a list
of strings).  The functions are called with the name of the current
group (or nil) as a parameter.

If you want to save your mail in one group and the news articles you
write in another group, you could say something like:

 \(setq gnus-message-archive-group 
        '((if (message-news-p)
              \"misc-news\" 
            \"misc-mail\")))

Normally the group names returned by this variable should be
unprefixed -- which implictly means \"store on the archive server\".
However, you may wish to store the message on some other server.  In
that case, just return a fully prefixed name of the group --
\"nnml+private:mail.misc\", for instance.")

(defvar gnus-secondary-servers nil
  "*List of NNTP servers that the user can choose between interactively.
To make Gnus query you for a server, you have to give `gnus' a
non-numeric prefix - `C-u M-x gnus', in short.")

(defvar gnus-nntp-server nil
  "*The name of the host running the NNTP server.
This variable is semi-obsolete.	 Use the `gnus-select-method'
variable instead.")

(defvar gnus-secondary-select-methods nil
  "*A list of secondary methods that will be used for reading news.
This is a list where each element is a complete select method (see
`gnus-select-method').

If, for instance, you want to read your mail with the nnml backend,
you could set this variable:

(setq gnus-secondary-select-methods '((nnml \"\")))")

(defvar gnus-backup-default-subscribed-newsgroups
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

;; Customization variables

(defvar gnus-refer-article-method nil
  "*Preferred method for fetching an article by Message-ID.
If you are reading news from the local spool (with nnspool), fetching
articles by Message-ID is painfully slow.  By setting this method to an
nntp method, you might get acceptable results.

The value of this variable must be a valid select method as discussed
in the documentation of `gnus-select-method'.")

(defvar gnus-group-faq-directory
  '("/ftp@mirrors.aol.com:/pub/rtfm/usenet/"
    "/ftp@sunsite.auc.dk:/pub/usenet/"
    "/ftp@sunsite.doc.ic.ac.uk:/pub/usenet/news-faqs/"
    "/ftp@src.doc.ic.ac.uk:/usenet/news-FAQS/"
    "/ftp@ftp.seas.gwu.edu:/pub/rtfm/"
    "/ftp@rtfm.mit.edu:/pub/usenet/"
    "/ftp@ftp.uni-paderborn.de:/pub/FAQ/"
    "/ftp@ftp.sunet.se:/pub/usenet/"
    "/ftp@nctuccca.edu.tw:/USENET/FAQ/"
    "/ftp@hwarang.postech.ac.kr:/pub/usenet/"
    "/ftp@ftp.hk.super.net:/mirror/faqs/")
  "*Directory where the group FAQs are stored.
This will most commonly be on a remote machine, and the file will be
fetched by ange-ftp.

This variable can also be a list of directories.  In that case, the
first element in the list will be used by default.  The others can
be used when being prompted for a site.

Note that Gnus uses an aol machine as the default directory.  If this
feels fundamentally unclean, just think of it as a way to finally get
something of value back from them.

If the default site is too slow, try one of these:

   North America: mirrors.aol.com		 /pub/rtfm/usenet
		  ftp.seas.gwu.edu		 /pub/rtfm
		  rtfm.mit.edu			 /pub/usenet
   Europe:	  ftp.uni-paderborn.de		 /pub/FAQ
                  src.doc.ic.ac.uk               /usenet/news-FAQS
		  ftp.sunet.se			 /pub/usenet
	          sunsite.auc.dk                 /pub/usenet
   Asia:	  nctuccca.edu.tw		 /USENET/FAQ
		  hwarang.postech.ac.kr		 /pub/usenet
		  ftp.hk.super.net		 /mirror/faqs")

(defvar gnus-use-cross-reference t
  "*Non-nil means that cross referenced articles will be marked as read.
If nil, ignore cross references.  If t, mark articles as read in
subscribed newsgroups.	If neither t nor nil, mark as read in all
newsgroups.")

(defvar gnus-process-mark ?#
  "*Process mark.")

(defvar gnus-asynchronous nil
  "*If non-nil, Gnus will supply backends with data needed for async article fetching.")

(defvar gnus-large-newsgroup 200
  "*The number of articles which indicates a large newsgroup.
If the number of articles in a newsgroup is greater than this value,
confirmation is required for selecting the newsgroup.")

(defvar gnus-use-long-file-name (not (memq system-type '(usg-unix-v xenix)))
  "*Non-nil means that the default name of a file to save articles in is the group name.
If it's nil, the directory form of the group name is used instead.

If this variable is a list, and the list contains the element
`not-score', long file names will not be used for score files; if it
contains the element `not-save', long file names will not be used for
saving; and if it contains the element `not-kill', long file names
will not be used for kill files.

Note that the default for this variable varies according to what system
type you're using.  On `usg-unix-v' and `xenix' this variable defaults
to nil while on all other systems it defaults to t.")

(defvar gnus-kill-files-directory gnus-directory
  "*Name of the directory where kill files will be stored (default \"~/News\").")

(defvar gnus-save-score nil
  "*If non-nil, save group scoring info.")

(defvar gnus-use-undo t
  "*If non-nil, allow undoing in Gnus group mode buffers.")

(defvar gnus-use-adaptive-scoring nil
  "*If non-nil, use some adaptive scoring scheme.
If a list, then the values `word' and `line' are meaningful.  The
former will perform adaption on individual words in the subject
header while `line' will perform adaption on several headers.")

(defvar gnus-use-cache 'passive
  "*If nil, Gnus will ignore the article cache.
If `passive', it will allow entering (and reading) articles
explicitly entered into the cache.  If anything else, use the
cache to the full extent of the law.")

(defvar gnus-use-trees nil
  "*If non-nil, display a thread tree buffer.")

(defvar gnus-use-grouplens nil
  "*If non-nil, use GroupLens ratings.")

(defvar gnus-keep-backlog nil
  "*If non-nil, Gnus will keep read articles for later re-retrieval.
If it is a number N, then Gnus will only keep the last N articles
read.  If it is neither nil nor a number, Gnus will keep all read
articles.  This is not a good idea.")

(defvar gnus-use-nocem nil
  "*If non-nil, Gnus will read NoCeM cancel messages.")

(defvar gnus-suppress-duplicates nil
  "*If non-nil, Gnus will mark duplicate copies of the same article as read.")

(defvar gnus-use-demon nil
  "If non-nil, Gnus might use some demons.")

(defvar gnus-use-scoring t
  "*If non-nil, enable scoring.")

(defvar gnus-use-picons nil
  "*If non-nil, display picons.")

(defvar gnus-summary-prepare-exit-hook nil
  "*A hook called when preparing to exit from the summary buffer.
It calls `gnus-summary-expire-articles' by default.")
(add-hook 'gnus-summary-prepare-exit-hook 'gnus-summary-expire-articles)

(defvar gnus-novice-user t
  "*Non-nil means that you are a usenet novice.
If non-nil, verbose messages may be displayed and confirmations may be
required.")

(defvar gnus-expert-user nil
  "*Non-nil means that you will never be asked for confirmation about anything.
And that means *anything*.")

(defvar gnus-interactive-catchup t
  "*If non-nil, require your confirmation when catching up a group.")

(defvar gnus-interactive-exit t
  "*If non-nil, require your confirmation when exiting Gnus.")

(defvar gnus-extract-address-components 'gnus-extract-address-components
  "*Function for extracting address components from a From header.
Two pre-defined function exist: `gnus-extract-address-components',
which is the default, quite fast, and too simplistic solution, and
`mail-extract-address-components', which works much better, but is
slower.")

(defvar gnus-carpal nil
  "*If non-nil, display clickable icons.")

(defvar gnus-shell-command-separator ";"
  "String used to separate to shell commands.")

(defvar gnus-valid-select-methods
  '(("nntp" post address prompt-address)
    ("nnspool" post address)
    ("nnvirtual" post-mail virtual prompt-address)
    ("nnmbox" mail respool address)
    ("nnml" mail respool address)
    ("nnmh" mail respool address)
    ("nndir" post-mail prompt-address address)
    ("nneething" none address prompt-address)
    ("nndoc" none address prompt-address)
    ("nnbabyl" mail address respool)
    ("nnkiboze" post virtual)
    ("nnsoup" post-mail address)
    ("nndraft" post-mail)
    ("nnfolder" mail respool address)
    ("nngateway" none address prompt-address)
    ("nndejanews" none))
  "An alist of valid select methods.
The first element of each list lists should be a string with the name
of the select method.  The other elements may be the category of
this method (ie. `post', `mail', `none' or whatever) or other
properties that this method has (like being respoolable).
If you implement a new select method, all you should have to change is
this variable.	I think.")

(defvar gnus-updated-mode-lines '(group article summary tree)
  "*List of buffers that should update their mode lines.
The list may contain the symbols `group', `article' and `summary'.  If
the corresponding symbol is present, Gnus will keep that mode line
updated with information that may be pertinent.
If this variable is nil, screen refresh may be quicker.")

;; Added by Keinonen Kari <kk85613@cs.tut.fi>.
(defvar gnus-mode-non-string-length nil
  "*Max length of mode-line non-string contents.
If this is nil, Gnus will take space as is needed, leaving the rest
of the modeline intact.")

(defvar gnus-auto-expirable-newsgroups nil
  "*Groups in which to automatically mark read articles as expirable.
If non-nil, this should be a regexp that should match all groups in
which to perform auto-expiry.  This only makes sense for mail groups.")

(defvar gnus-total-expirable-newsgroups nil
  "*Groups in which to perform expiry of all read articles.
Use with extreme caution.  All groups that match this regexp will be
expiring - which means that all read articles will be deleted after
(say) one week.	 (This only goes for mail groups and the like, of
course.)")

(defvar gnus-group-uncollapsed-levels 1
  "Number of group name elements to leave alone when making a short group name.")

(defvar gnus-group-use-permanent-levels nil
  "*If non-nil, once you set a level, Gnus will use this level.")

;; Hooks.

(defvar gnus-load-hook nil
  "*A hook run while Gnus is loaded.")

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

(defvar gnus-group-change-level-function nil
  "Function run when a group level is changed.
It is called with three parameters -- GROUP, LEVEL and OLDLEVEL.")


;;; Internal variables

(defvar gnus-original-article-buffer " *Original Article*")
(defvar gnus-newsgroup-name nil)

(defvar gnus-current-select-method nil
  "The current method for selecting a newsgroup.")

(defvar gnus-tree-buffer "*Tree*"
  "Buffer where Gnus thread trees are displayed.")

;; Dummy variable.
(defvar gnus-use-generic-from nil)

;; Variable holding the user answers to all method prompts.
(defvar gnus-method-history nil)

;; Variable holding the user answers to all group prompts.
(defvar gnus-group-history nil)

(defvar gnus-server-alist nil
  "List of available servers.")

(defvar gnus-topic-indentation "") ;; Obsolete variable.

(defconst gnus-article-mark-lists
  '((marked . tick) (replied . reply)
    (expirable . expire) (killed . killed)
    (bookmarks . bookmark) (dormant . dormant)
    (scored . score) (saved . save)
    (cached . cache)))

(defvar gnus-headers-retrieved-by nil)
(defvar gnus-article-reply nil)
(defvar gnus-override-method nil)
(defvar gnus-article-check-size nil)
(defvar gnus-opened-servers nil)

(defvar gnus-current-kill-article nil)

(defvar gnus-have-read-active-file nil)

(defconst gnus-maintainer
  "gnus-bug@ifi.uio.no (The Gnus Bugfixing Girls + Boys)"
  "The mail address of the Gnus maintainers.")

(defvar gnus-info-nodes
  '((gnus-group-mode "(gnus)The Group Buffer")
    (gnus-summary-mode "(gnus)The Summary Buffer")
    (gnus-article-mode "(gnus)The Article Buffer")
    (mime/viewer-mode "(gnus)The Article Buffer")
    (gnus-server-mode "(gnus)The Server Buffer")
    (gnus-browse-mode "(gnus)Browse Foreign Server")
    (gnus-tree-mode "(gnus)Tree Display"))
  "Alist of major modes and related Info nodes.")

(defvar gnus-group-buffer "*Group*")
(defvar gnus-summary-buffer "*Summary*")
(defvar gnus-article-buffer "*Article*")
(defvar gnus-server-buffer "*Server*")

(defvar gnus-buffer-list nil
  "Gnus buffers that should be killed on exit.")

(defvar gnus-slave nil
  "Whether this Gnus is a slave or not.")

(defvar gnus-variable-list
  '(gnus-newsrc-options gnus-newsrc-options-n
    gnus-newsrc-last-checked-date
    gnus-newsrc-alist gnus-server-alist
    gnus-killed-list gnus-zombie-list
    gnus-topic-topology gnus-topic-alist
    gnus-format-specs)
  "Gnus variables saved in the quick startup file.")

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

;; Save window configuration.
(defvar gnus-prev-winconf nil)

(defvar gnus-reffed-article-number nil)

;;; Let the byte-compiler know that we know about this variable.
(defvar rmail-default-rmail-file)

(defvar gnus-dead-summary nil)

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
	(if (eq (nth 1 package) ':interactive)
	    (cdddr package)
	  (cdr package)))))
   '(("metamail" metamail-buffer)
     ("info" Info-goto-node)
     ("hexl" hexl-hex-string-to-integer)
     ("pp" pp pp-to-string pp-eval-expression)
     ("mail-extr" mail-extract-address-components)
     ("nnmail" nnmail-split-fancy nnmail-article-group)
     ("nnvirtual" nnvirtual-catchup-group nnvirtual-convert-headers)
     ("timezone" timezone-make-date-arpa-standard timezone-fix-time
      timezone-make-sortable-date timezone-make-time-string)
     ("rmailout" rmail-output)
     ("rmail" rmail-insert-rmail-file-header rmail-count-new-messages
      rmail-show-message)
     ("gnus-soup" :interactive t
      gnus-group-brew-soup gnus-brew-soup gnus-soup-add-article
      gnus-soup-send-replies gnus-soup-save-areas gnus-soup-pack-packet)
     ("nnsoup" nnsoup-pack-replies)
     ("score-mode" :interactive t gnus-score-mode)
     ("gnus-mh" gnus-mh-mail-setup gnus-summary-save-article-folder
      gnus-Folder-save-name gnus-folder-save-name)
     ("gnus-mh" :interactive t gnus-summary-save-in-folder)
     ("gnus-vis" gnus-group-make-menu-bar gnus-summary-make-menu-bar
      gnus-server-make-menu-bar gnus-article-make-menu-bar
      gnus-browse-make-menu-bar gnus-highlight-selected-summary
      gnus-summary-highlight-line gnus-carpal-setup-buffer
      gnus-group-highlight-line
      gnus-article-add-button gnus-insert-next-page-button
      gnus-insert-prev-page-button gnus-visual-turn-off-edit-menu)
     ("gnus-vis" :interactive t
      gnus-article-push-button gnus-article-press-button
      gnus-article-highlight gnus-article-highlight-some
      gnus-article-highlight-headers gnus-article-highlight-signature
      gnus-article-add-buttons gnus-article-add-buttons-to-head
      gnus-article-next-button gnus-article-prev-button)
     ("gnus-demon" gnus-demon-add-nocem gnus-demon-add-scanmail
      gnus-demon-add-disconnection gnus-demon-add-handler
      gnus-demon-remove-handler)
     ("gnus-demon" :interactive t
      gnus-demon-init gnus-demon-cancel)
     ("gnus-salt" gnus-highlight-selected-tree gnus-possibly-generate-tree
      gnus-tree-open gnus-tree-close)
     ("gnus-nocem" gnus-nocem-scan-groups gnus-nocem-close
      gnus-nocem-unwanted-article-p)
     ("gnus-srvr" gnus-enter-server-buffer gnus-server-set-info)
     ("gnus-srvr" gnus-browse-foreign-server)
     ("gnus-cite" :interactive t
      gnus-article-highlight-citation gnus-article-hide-citation-maybe
      gnus-article-hide-citation gnus-article-fill-cited-article
      gnus-article-hide-citation-in-followups)
     ("gnus-kill" gnus-kill gnus-apply-kill-file-internal
      gnus-kill-file-edit-file gnus-kill-file-raise-followups-to-author
      gnus-execute gnus-expunge)
     ("gnus-cache" gnus-cache-possibly-enter-article gnus-cache-save-buffers
      gnus-cache-possibly-remove-articles gnus-cache-request-article
      gnus-cache-retrieve-headers gnus-cache-possibly-alter-active
      gnus-cache-enter-remove-article gnus-cached-article-p
      gnus-cache-open gnus-cache-close gnus-cache-update-article)
     ("gnus-cache" :interactive t gnus-jog-cache gnus-cache-enter-article
      gnus-cache-remove-article)
     ("gnus-score" :interactive t
      gnus-summary-increase-score gnus-summary-lower-score
      gnus-score-flush-cache gnus-score-close
      gnus-score-raise-same-subject-and-select
      gnus-score-raise-same-subject gnus-score-default
      gnus-score-raise-thread gnus-score-lower-same-subject-and-select
      gnus-score-lower-same-subject gnus-score-lower-thread
      gnus-possibly-score-headers gnus-summary-raise-score 
      gnus-summary-set-score gnus-summary-current-score)
     ("gnus-score"
      (gnus-summary-score-map keymap) gnus-score-save gnus-score-headers
      gnus-current-score-file-nondirectory gnus-score-adaptive
      gnus-score-find-trace gnus-score-file-name)
     ("gnus-edit" :interactive t gnus-score-customize)
     ("gnus-topic" :interactive t gnus-topic-mode)
     ("gnus-topic" gnus-topic-remove-group)
     ("gnus-salt" :interactive t gnus-pick-mode gnus-binary-mode)
     ("gnus-uu" (gnus-uu-extract-map keymap) (gnus-uu-mark-map keymap))
     ("gnus-uu" :interactive t
      gnus-uu-digest-mail-forward gnus-uu-digest-post-forward
      gnus-uu-mark-series gnus-uu-mark-region gnus-uu-mark-buffer
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
      gnus-article-mail gnus-new-mail gnus-mail-reply
      gnus-copy-article-buffer gnus-extended-version)
     ("gnus-msg" :interactive t
      gnus-group-post-news gnus-group-mail gnus-summary-post-news
      gnus-summary-followup gnus-summary-followup-with-original
      gnus-summary-cancel-article gnus-summary-supersede-article
      gnus-post-news gnus-inews-news 
      gnus-summary-reply gnus-summary-reply-with-original
      gnus-summary-mail-forward gnus-summary-mail-other-window
      gnus-bug)
     ("gnus-picon" :interactive t gnus-article-display-picons
      gnus-group-display-picons gnus-picons-article-display-x-face
      gnus-picons-display-x-face)
     ("gnus-gl" bbb-login bbb-logout bbb-grouplens-group-p 
      gnus-grouplens-mode)
     ("smiley" :interactive t gnus-smiley-display)
     ("gnus" gnus-add-current-to-buffer-list gnus-add-shutdown)
     ("gnus-win" gnus-configure-windows)
     ("gnus-sum" gnus-summary-insert-line gnus-summary-read-group
      gnus-list-of-unread-articles gnus-list-of-read-articles
      gnus-offer-save-summaries gnus-make-thread-indent-array
      gnus-summary-exit)
     ("gnus-group" gnus-group-insert-group-line gnus-group-quit
      gnus-group-list-groups gnus-group-first-unread-group
      gnus-group-set-mode-line gnus-group-set-info gnus-group-save-newsrc
      gnus-group-setup-buffer gnus-group-get-new-news
      gnus-group-make-help-group gnus-group-update-group)
     ("gnus-bcklg" gnus-backlog-request-article gnus-backlog-enter-article
      gnus-backlog-remove-article) 
     ("gnus-art" gnus-article-read-summary-keys gnus-article-save
      gnus-article-prepare gnus-article-set-window-start
      gnus-article-show-all-headers gnus-article-next-page
      gnus-article-prev-page gnus-request-article-this-buffer
      gnus-article-mode gnus-article-setup-buffer gnus-narrow-to-page)
     ("gnus-art" :interactive t
      gnus-article-hide-headers gnus-article-hide-boring-headers
      gnus-article-treat-overstrike gnus-article-word-wrap
      gnus-article-remove-cr gnus-article-remove-trailing-blank-lines
      gnus-article-display-x-face gnus-article-de-quoted-unreadable
      gnus-article-mime-decode-quoted-printable gnus-article-hide-pgp
      gnus-article-hide-pem gnus-article-hide-signature
      gnus-article-strip-leading-blank-lines gnus-article-date-local
      gnus-article-date-original gnus-article-date-lapsed
      gnus-decode-rfc1522 gnus-article-show-all-headers
      gnus-article-edit-mode gnus-article-edit-article
      gnus-article-edit-done)
     ("gnus-int" gnus-request-type)
     ("gnus-start" gnus-newsrc-parse-options gnus-1 gnus-no-server-1
      gnus-dribble-enter)
     ("gnus-dup" gnus-dup-suppress-articles gnus-dup-enter-articles)
     ("gnus-range" gnus-copy-sequence)
     ("gnus-vm" gnus-vm-mail-setup)
     ("gnus-eform" gnus-edit-form)
     ("gnus-move" :interactive t
      gnus-group-move-group-to-server gnus-change-server)
     ("gnus-logic" gnus-score-advanced)
     ("gnus-undo" gnus-undo-mode gnus-undo-register 
      gnus-dup-unsuppress-article)
     ("gnus-async" gnus-async-request-fetched-article gnus-async-prefetch-next
      gnus-async-prefetch-article gnus-async-prefetch-remove-group)
     ("gnus-vm" :interactive t gnus-summary-save-in-vm
      gnus-summary-save-article-vm))))

;;; gnus-sum.el thingies


(defvar gnus-summary-line-format "%U\%R\%z\%I\%(%[%4L: %-20,20n%]%) %s\n"
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
%l   GroupLens score (string).
%P   The line number (number).
%u   User defined specifier.  The next character in the format string should
     be a letter.  Gnus will call the function gnus-user-format-function-X,
     where X is the letter following %u.  The function will be passed the
     current header as argument.  The function should return a string, which
     will be inserted into the summary just like information from any other
     summary specifier.

Text between %( and %) will be highlighted with `gnus-mouse-face'
when the mouse point is placed inside the area.	 There can only be one
such area.

The %U (status), %R (replied) and %z (zcore) specs have to be handled
with care.  For reasons of efficiency, Gnus will compute what column
these characters will end up in, and \"hard-code\" that.  This means that
it is illegal to have these specs after a variable-length spec.	 Well,
you might not be arrested, but your summary buffer will look strange,
which is bad enough.

The smart choice is to have these specs as for to the left as
possible.

This restriction may disappear in later versions of Gnus.")

;;;
;;; Skeleton keymaps
;;;

(defun gnus-suppress-keymap (keymap)
  (suppress-keymap keymap)
  (let ((keys `([delete] "\177" "\M-u"))) ;gnus-mouse-2 
    (while keys
      (define-key keymap (pop keys) 'undefined))))

(defvar gnus-article-mode-map (make-keymap))
(gnus-suppress-keymap gnus-article-mode-map)
(defvar gnus-summary-mode-map (make-keymap))
(gnus-suppress-keymap gnus-summary-mode-map)
(defvar gnus-group-mode-map (make-keymap))
(gnus-suppress-keymap gnus-group-mode-map)

;;; Function aliases later to be redefined for XEmacs usage.

(defalias 'gnus-make-overlay 'make-overlay)
(defalias 'gnus-overlay-put 'overlay-put)
(defalias 'gnus-move-overlay 'move-overlay)
(defalias 'gnus-overlay-end 'overlay-end)
(defalias 'gnus-extent-detached-p 'ignore)
(defalias 'gnus-extent-start-open 'ignore)
(defalias 'gnus-set-text-properties 'set-text-properties)
(defalias 'gnus-group-remove-excess-properties 'ignore)
(defalias 'gnus-topic-remove-excess-properties 'ignore)
(defalias 'gnus-appt-select-lowest-window 'appt-select-lowest-window)
(defalias 'gnus-mail-strip-quoted-names 'mail-strip-quoted-names)
(defalias 'gnus-make-local-hook 'make-local-hook)
(defalias 'gnus-add-hook 'add-hook)
(defalias 'gnus-character-to-event 'identity)
(defalias 'gnus-add-text-properties 'add-text-properties)
(defalias 'gnus-put-text-property 'put-text-property)
(defalias 'gnus-mode-line-buffer-identification 'identity)

(provide 'gnus-load)

;;; gnus-load.el ends here
