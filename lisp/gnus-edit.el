;;; gnus-edit.el --- Gnus SCORE file editing.
;; Copyright (C) 1995 Free Software Foundation, Inc.
;;
;; Author: Per Abrahamsen <abraham@iesd.auc.dk>
;; Keywords: news, help
;; Version: 0.1

;;; Commentary:
;;
;; Type `M-x gnus-score-customize RET' to invoke.

;;; Code:

(require 'custom)
(require 'gnus-score)

(autoload 'gnus-score-load "gnus-score")

(defconst gnus-score-custom-data
  '((tag . "Score")
    (doc . "Customization of Gnus SCORE files.

SCORE files allow you to assign a score to each article when you enter
a group, and automatically mark the articles as read or delete them
based on the score.  In the summary buffer you can use the score to
sort the articles by score (`C-c C-s C-s') or to jump to the unread
article with the highest score (`,').")
    (type . group)
    (data ""
	  ((header . nil)
	   (doc . "Name of SCORE file to customize.

Enter the name in the `File' field, then push the [Load] button to
load it.  When done editing, push the [Save] button to save the file.

Several score files may apply to each group, and several groups may
use the same score file.  This is controlled implicitly by the name of
the score file and the value of the global variable
`gnus-score-find-score-files-function', and explicitly by the the
`Files' and `Exclude Files' entries.") 
	   (compact . t)
	   (type . group)
	   (data ((tag . "Load")
		  (type . button)
		  (query . gnus-score-custom-load))
		 ((tag . "Save")
		  (type . button)
		  (query . gnus-score-custom-save))
		 ((name . file)
		  (tag . "File")
		  (directory . "~/News/")
		  (default-file . "SCORE")
		  (type . file))))
	  ((name . files)
	   (tag . "Files")
	   (doc . "\
List of score files to load when the the current score file is loaded.
You can use this to share score entries between multiple score files.

Push the `[INS]' button add a score file to the list, or `[DEL]' to
delete a score file from the list.")
	   (type . list)
	   (data ((type . repeat)
		  (header . nil)
		  (data (type . file)
			(directory . "~/News/")))))
	  ((name . exclude-files)
	   (tag . "Exclude Files")
	   (doc . "\
List of score files to exclude when the the current score file is loaded.
You can use this if you have a score file you want to share between a
number of newsgroups, except for the newsgroup this score file
matches.  [ Did anyone get that? ]

Push the `[INS]' button add a score file to the list, or `[DEL]' to
delete a score file from the list.")
	   (type . list)
	   (data ((type . repeat)
		  (header . nil)
		  (data (type . file)
			(directory . "~/News/")))))
	  ((name . mark)
	   (tag . "Mark")
	   (doc . "\
Articles below this score will be automatically marked as read.

This means that when you enter the summary buffer, the articles will
be shown but will already be marked as read.  You can then press `x'
to get rid of them entirely.

By default articles with a negative score will be marked as read.  To
change this, push the `Mark' button, and choose `Integer'.  You can
then enter a value in the `Mark' field.")
	   (type . gnus-score-custom-maybe-type))
	  ((name . expunge)
	   (tag . "Expunge")
	   (doc . "\
Articles below this score will not be shown in the summary buffer.")
	   (type . gnus-score-custom-maybe-type))
	  ((name . mark-and-expunge)
	   (tag . "Mark and Expunge")
	   (doc . "\
Articles below this score will be marked as read, but not shown.

Someone should explain me the difference between this and `expunge'
alone or combined with `mark'.")
	   (type . gnus-score-custom-maybe-type))
;	  ;; Sexp type isn't implemented yet.
;	  ((name . eval)
;	   (tag . "Eval")
;	   (doc . "Evaluate this expression when the entering sumamry buffer.")
;	   (type . sexp))
	  ;; Toggle type isn't implemented yet.
	  ((name . read-only)
	   (tag . "Read Only")
	   (doc . "Read-only score files will not be updated or saved.
Except from this buffer, of course!")
	   (type . toggle))
	  ((type . doc)
	   (header . nil)
	   (doc . "\
Each news header has an associated list of score entries.  
You can use the [INS] buttons to add new score entries anywhere in the
list, or the [DEL] buttons to delete specific score entries.

Each score entry should specify a string that should be matched with
the content actual header in order to determine whether the entry
applies to that header.  Enter that string in the `Match' field.

If the score entry matches, the articles score will be adjusted with
some amount.  Enter that amount in the in the `Score' field.  You
should specify a positive amount for score entries that matches
articles you find interesting, and a negative amount for score entries
matching articles you would rather avoid.  The final score for the
article will be the sum of the score of all score entries that match
the article. 

The score entry can be either permanent or expirable.  To make the
entry permanent, push the `Date' button and choose the `Permanent'
entry.  To make the entry expirable, choose instead the `Integer'
entry.  After choosing the you can enter the date the score entry was
last matched in the `Date' field.  The date will be automatically
updated each time the score entry matches an article.  When the date
become too old, the the score entry will be removed.

For your convenience, the date is specified as the number of days
elapsed since the (imaginary) Gregorian date Sunday, December 31, 1
BC.

Finally, you can choose what kind of match you want to perform by
pushing the `Type' button.  For most entries you can choose between
`Exact' which mean the header content must be exactly identical to the
match string, or `Substring' meaning the match string should be
somewhere in the header content, or even `Regexp' to use Emacs regular
expression matching.  The last choice is `Fuzzy' which is like `Exact'
except that whitespace derivations, a beginning `Re:' or a terminating
parenthetical remark are all ignored.  Each of the four types have a
variant which will ignore case in the comparison.  That variant is
indicated with a `(fold)' after its name."))
	  ((name . from)
	   (tag . "From")
	   (doc . "Scoring based on the authors email address.")
	   (type . gnus-score-custom-string-type))
	  ((name . subject)
	   (tag . "Subject")
	   (doc . "Scoring based on the articles subject.")
	   (type . gnus-score-custom-string-type))
	  ((name . followup)
	   (tag . "Followup")
	   (doc . "Scoring based on who the article is a followup to.

If you want to see all followups to your own articles, add an entry
with a positive score matching your email address here.  You can also
put an entry with a negative score matching someone who is so annoying
that you don't even want to see him quoted in followups.")
	   (type . gnus-score-custom-string-type))
	  ((name . xref)
	   (tag . "Xref")
	   (doc . "Scoring based on article crossposting.

If you want to score based on which newsgroups an article is posted
to, this is the header to use.  The syntax is a little different from
the `Newsgroups' header, but scoring in `Xref' is much faster.  As an
example, to match all crossposted articles match on `:.*:' using the
`Regexp' type.")
	   (type . gnus-score-custom-string-type))
	  ((name . references)
	   (tag . "References")
	   (doc . "Scoring based on article references.

The `References' header gives you an alternative way to score on
followups.  If you for example want to see follow all discussions
where people from `iesd.auc.dk' school participate, you can add a
substring match on `iesd.auc.dk>' on this header.")
	   (type . gnus-score-custom-string-type))
	  ((name . message-id)
	   (tag . "Message-ID")
	   (doc . "Scoring based on the articles message-id.

This isn't very useful, but Lars like completeness.  You can use it to
match all messaged generated by recent Gnus version with a `Substring'
match on `.fsf@'.")
	   (type . gnus-score-custom-string-type))
	  ((type . doc)
	   (header . nil)
	   (doc . "\
WARNING:  Scoring on the following three pseudo headers is very slow!
Scoring on any of the real headers use a technique that avoids
scanning the entire article, only the actual headers you score on are
scanned, and this scanning has been heavily optimized.  Using just a
single entry for one the three pseudo-headers `Head', `Body', and
`All' will require GNUS to retrieve and scan the entire article, which
can be very slow on large groups.  However, if you add one entry for
any of these headers, you can just as well add several.  Each
subsequent entry cost relatively little extra time."))
	  ((name . head)
	   (tag . "Head")
	   (doc . "Scoring based on the article header.

Instead of matching the content of a single header, the entire header
section of the article is matched.  You can use this to match on
arbitrary headers, foe example to single out TIN lusers, use a substring
match on `Newsreader: TIN'.  That should get 'em!")
	   (type . gnus-score-custom-string-type))
	  ((name . body)
	   (tag . "Body")
	   (doc . "Scoring based on the article body.

If you think any article that mentions `Kibo' is inherently
interesting, do a substring match on His name.  You Are Allowed.")
	   (type . gnus-score-custom-string-type))
	  ((name . all)
	   (tag . "All")
	   (doc . "Scoring based on the whole article.")
	   (type . gnus-score-custom-string-type))
	  ((name . date)
	   (tag . "Date")
	   (doc . "Scoring based on article date.

You can change the score of articles that have been posted before,
after, or at a specific date.  You should add the date in the `Match'
field, and then select `before', `after', or `at' by pushing the
`Type' button.  Imagine you want to lower the score of very old
articles, or want to raise the score of articles from the future (such
things happen!).  Then you can't use date scoring for that.  In fact,
I can't imagine anything you would want to use this for.   

For your convenience, the date is specified in Usenet date format.")
	   (type . gnus-score-custom-date-type))
	  ((type . doc)
	   (header . nil)
	   (doc . "\
The Lines and Chars headers use integer based scoring.  

This means that you should write an integer in the `Match' field, and
the push the `Type' field to if the `Chars' or `Lines' header should
be larger, equal, or smaller than the number you wrote in the match
field."))
	  ((name . chars)
	   (tag . "Characters")
	   (doc . "Scoring based on the number of characters in the article.")
	   (type . gnus-score-custom-integer-type))
	  ((name . lines)
	   (tag . "Lines")
	   (doc . "Scoring based on the number of lines in the article.")
	   (type . gnus-score-custom-integer-type))
	  ((name . orphan)
	   (tag . "Orphan")
	   (doc . "Score to add to articles with no parents.")
	   (type . gnus-score-custom-maybe-type)))))  
;; This is to complex for me to figure out right now.
;`adapt'
;     This entry controls the adaptive scoring.  If it is `t', the
;     default adaptive scoring rules will be used.  If it is `ignore', no
;     adaptive scoring will be performed on this group.  If it is a
;     list, this list will be used as the adaptive scoring rules.  If it
;     isn't present, or is something other than `t' or `ignore', the
;     default adaptive scoring rules will be used.  If you want to use
;     adaptive scoring on most groups, you'd set
;     `gnus-use-adaptive-scoring' to `t', and insert an `(adapt ignore)'
;     in the groups where you do not want adaptive scoring.  If you only
;     want adaptive scoring in a few groups, you'd set
;     `gnus-use-adaptive-scoring' to `nil', and insert `(adapt t)' in
;     the score files of the groups where you want it.
;; This isn't implemented in the old version of (ding) I use.
;`local'
;  List of local variables to bind in the summary buffer.

(defconst gnus-score-custom-type-properties
  '((gnus-score-custom-maybe-type
     (type . choice)
     (data ((type . integer)
	    (default . 0))
	   ((tag . "Default")
	    (type . const)
	    (default . nil))))
    (gnus-score-custom-string-type
     (type . list)
     (data ((type . repeat)
	    (header . nil)
	    (data . ((type . list)
		     (compact . t)
		     (data ((tag . "Match")
			    (width . 59)
			    (type . string))
			   "\n           "
			   ((tag . "Score")
			    (type . integer))
			   ((tag . "Date")
			    (type . choice)
			    (data ((type . integer)
				   (default . 0)
				   (width . 9))
				  ((tag . "Permanent")
				   (type . const)
				   (default . nil))))
			   ((tag . "Type")
			    (type . choice)
			    (data ((tag . "Exact")
				   (default . e)
				   (type . const))
				  ((tag . "Substring")
				   (default . s) 
				   (type . const))
				  ((tag . "Regexp")
				   (default . r)
				   (type . const))
				  ((tag . "Fuzzy")
				   (default . f)
				   (type . const))
				  ((tag . "Exact (fold)")
				   (default . E)
				   (type . const))
				  ((tag . "Substring (fold)")
				   (default . S) 
				   (type . const))
				  ((tag . "Regexp (fold)")
				   (default . R)
				   (type . const))
				  ((tag . "Fuzzy  (fold)")
				   (default . F)
				   (type . const))))))))))
    (gnus-score-custom-integer-type
     (type . list)
     (data ((type . repeat)
	    (header . nil)
	    (data . ((type . list)
		     (compact . t)
		     (data ((tag . "Match")
			    (type . integer))
			   ((tag . "Score")
			    (type . integer))
			   ((tag . "Date")
			    (type . choice)
			    (data ((type . integer)
				   (default . 0)
				   (width . 9))
				  ((tag . "Permanent")
				   (type . const)
				   (default . nil))))
			   ((tag . "Type")
			    (type . choice)
			    (data ((tag . "<")
				   (default . <)
				   (type . const))
				  ((tag . ">")
				   (default . >) 
				   (type . const))
				  ((tag . "=")
				   (default . =)
				   (type . const))
				  ((tag . ">=")
				   (default . >=)
				   (type . const))
				  ((tag . "<=")
				   (default . <=)
				   (type . const))))))))))
    (gnus-score-custom-date-type
     (type . list)
     (data ((type . repeat)
	    (header . nil)
	    (data . ((type . list)
		     (compact . t)
		     (data ((tag . "Match")
			    (width . 59)
			    (type . string))
			   "\n           "
			   ((tag . "Score")
			    (type . integer))
			   ((tag . "Date")
			    (type . choice)
			    (data ((type . integer)
				   (default . 0)
				   (width . 9))
				  ((tag . "Permanent")
				   (type . const)
				   (default . nil))))
			   ((tag . "Type")
			    (type . choice)
			    (data ((tag . "Before")
				   (default . before)
				   (type . const))
				  ((tag . "After")
				   (default . after) 
				   (type . const))
				  ((tag . "At")
				   (default . at)
				   (type . const))))))))))))

(defvar gnus-score-custom-file nil
  "Name of SCORE file being customized.")

(defun gnus-score-customize ()
  "Create a buffer for editing gnus SCORE files."
  (interactive)
  (let (gnus-score-alist)
    (custom-buffer-create "*Score Edit*" gnus-score-custom-data
			  gnus-score-custom-type-properties
			  'gnus-score-custom-set
			  'gnus-score-custom-get))
  (make-local-variable 'gnus-score-custom-file)
  (setq gnus-score-custom-file "SCORE")
  (make-local-variable 'gnus-score-alist)
  (setq gnus-score-alist nil)
  (custom-reset-all))

(defun gnus-score-custom-get (name)
  (if (eq name 'file)
      gnus-score-custom-file
    (let ((entry (assoc (symbol-name name) gnus-score-alist)))
      (if entry 
	  (mapcar 'gnus-score-custom-sanify (cdr entry))
	(setq entry (assoc name gnus-score-alist))
	(if (memq name '(files))
	    (cdr entry)
	  (car (cdr entry)))))))

(defun gnus-score-custom-set (name value)
  (cond ((eq name 'file)
	 (setq gnus-score-custom-file value))
	((assoc (symbol-name name) gnus-score-alist)
	 (if value
	     (setcdr (assoc (symbol-name name) gnus-score-alist) value)
	   (setq gnus-score-alist (delq (assoc (symbol-name name) 
					       gnus-score-alist) 
					gnus-score-alist))))
	((assoc (symbol-name name) gnus-header-index)
	 (if value
	     (setq gnus-score-alist 
		   (cons (cons (symbol-name name) value) gnus-score-alist))))
	((assoc name gnus-score-alist)
	 (cond ((null value)
		(setq gnus-score-alist (delq (assoc name gnus-score-alist)
					     gnus-score-alist)))
	       ((listp value)
		(setcdr (assoc name gnus-score-alist) value))
	       (t
		(setcdr (assoc name gnus-score-alist) (list value)))))
	((null value))
	((listp value)
	 (setq gnus-score-alist (cons (cons name value) gnus-score-alist)))
	(t
	 (setq gnus-score-alist 
	       (cons (cons name (list value)) gnus-score-alist)))))

(defun gnus-score-custom-sanify (entry)
  (list (nth 0 entry)
	(or (nth 1 entry) gnus-score-interactive-default-score)
	(nth 2 entry)
	(if (null (nth 3 entry)) 
	    's
	  (intern (substring (symbol-name (nth 3 entry)) 0 1)))))

(defvar gnus-score-cache nil)

(defun gnus-score-custom-load ()
  (interactive)
  (let ((file (custom-name-value 'file)))
    (if (eq file custom-nil)
	(error "You must specify a file name"))
    (setq file (expand-file-name file "~/News"))
    (gnus-score-load file)
    (setq gnus-score-custom-file file)
    (custom-reset-all)
    (message "Loaded")))

(defun gnus-score-custom-save ()
  (interactive)
  (custom-apply-all)
  (gnus-score-remove-from-cache gnus-score-custom-file)
  (let ((file gnus-score-custom-file)
	(score gnus-score-alist)
	emacs-lisp-mode-hook)
    (save-excursion
      (set-buffer (get-buffer-create "*Score*"))
      (buffer-disable-undo (current-buffer))
      (erase-buffer)
      (pp score (current-buffer))
      (gnus-make-directory (file-name-directory file))
      (write-region (point-min) (point-max) file nil 'silent)
      (kill-buffer (current-buffer))))
  (message "Saved"))

(provide 'gnus-edit)

;;; gnus-edit.el end here
