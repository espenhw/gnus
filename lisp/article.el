;;; article.el --- article treatment functions
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

(require 'nnheader)
(require 'gnus-util)
(require 'message)
(require 'custom)

(defgroup article nil
  "Article display."
  :group 'gnus)

(defcustom gnus-ignored-headers
  '("^Path:" "^Posting-Version:" "^Article-I.D.:" "^Expires:"
    "^Date-Received:" "^References:" "^Control:" "^Xref:" "^Lines:"
    "^Posted:" "^Relay-Version:" "^Message-ID:" "^Nf-ID:" "^Nf-From:"
    "^Approved:" "^Sender:" "^Received:" "^Mail-from:") 
  "All headers that match this regexp will be hidden.
This variable can also be a list of regexps of headers to be ignored.
If `gnus-visible-headers' is non-nil, this variable will be ignored."
  :type '(choice :custom-show nil
		 regexp
		 (repeat regexp))
  :group 'article)

(defcustom gnus-visible-headers 
  "^From:\\|^Newsgroups:\\|^Subject:\\|^Date:\\|^Followup-To:\\|^Reply-To:\\|^Organization:\\|^Summary:\\|^Keywords:\\|^To:\\|^Cc:\\|^Posted-To:\\|^Mail-Copies-To:\\|^Apparently-To:\\|^Gnus-Warning:\\|^Resent-"
  "All headers that do not match this regexp will be hidden.
This variable can also be a list of regexp of headers to remain visible.
If this variable is non-nil, `gnus-ignored-headers' will be ignored."
  :type '(repeat :value-to-internal (lambda (widget value)
				      (custom-split-regexp-maybe value))
		 :match (lambda (widget value)
			  (or (stringp value)
			      (widget-editable-list-match widget value)))
		 regexp)
  :group 'article)

(defcustom gnus-sorted-header-list
  '("^From:" "^Subject:" "^Summary:" "^Keywords:" "^Newsgroups:" "^To:"
    "^Cc:" "^Date:" "^Organization:")
  "This variable is a list of regular expressions.
If it is non-nil, headers that match the regular expressions will
be placed first in the article buffer in the sequence specified by
this list."
  :type '(repeat regexp)
  :group 'article)

(defcustom gnus-boring-article-headers '(empty followup-to reply-to)
  "Headers that are only to be displayed if they have interesting data.
Possible values in this list are `empty', `newsgroups', `followup-to',
`reply-to', and `date'."
  :type '(set (const :tag "Headers with no content." empty)
	      (const :tag "Newsgroups with only one group." newsgroups)
	      (const :tag "Followup-to identical to newsgroups." followup-to)
	      (const :tag "Reply-to identical to from." reply-to)
	      (const :tag "Date less than four days old." date))
  :group 'article)

(defcustom gnus-signature-separator '("^-- $" "^-- *$")
  "Regexp matching signature separator.
This can also be a list of regexps.  In that case, it will be checked
from head to tail looking for a separator.  Searches will be done from
the end of the buffer."
  :type '(repeat string)
  :group 'article)

(defcustom gnus-signature-limit nil
   "Provide a limit to what is considered a signature.
If it is a number, no signature may not be longer (in characters) than
that number.  If it is a floating point number, no signature may be
longer (in lines) than that number.  If it is a function, the function
will be called without any parameters, and if it returns nil, there is
no signature in the buffer.  If it is a string, it will be used as a
regexp.  If it matches, the text in question is not a signature."
  :type '(choice integer number function regexp)
  :group 'article)

(defcustom gnus-hidden-properties '(invisible t intangible t)
  "Property list to use for hiding text."
  :type 'sexp 
  :group 'article)

(defcustom gnus-article-x-face-command
  "{ echo '/* Width=48, Height=48 */'; uncompface; } | icontopbm | xv -quit -"
  "String or function to be executed to display an X-Face header.
If it is a string, the command will be executed in a sub-shell
asynchronously.	 The compressed face will be piped to this command."
  :type 'string				;Leave function case to Lisp.
  :group 'article)

(defcustom gnus-article-x-face-too-ugly nil
  "Regexp matching posters whose face shouldn't be shown automatically."
  :type 'regexp
  :group 'article)

(defcustom gnus-emphasis-alist
  (let ((format
	 "\\(\\s-\\|^\\)\\(%s\\(\\w+\\(\\s-+\\w+\\)*\\)%s\\)\\(\\s-\\|[?!.,;)]\\)")
	(types
	 '(("_" "_" underline)
	   ("/" "/" italic)
	   ("\\*" "\\*" bold)
	   ("_/" "/_" underline-italic)
	   ("_\\*" "\\*_" underline-bold)
	   ("\\*/" "/\\*" bold-italic)
	   ("_\\*/" "/\\*_" underline-bold-italic))))
    `(("\\(\\s-\\|^\\)\\(_\\(\\(\\w\\|_\\)+\\)_\\)\\(\\s-\\|[?!.,;]\\)"
       2 3 gnus-emphasis-underline)
      ,@(mapcar
	 (lambda (spec)
	   (list
	    (format format (car spec) (cadr spec))
	    2 3 (intern (format "gnus-emphasis-%s" (caddr spec)))))
	 types)))
  "Alist that says how to fontify certain phrases.
Each item looks like this:

  (\"_\\\\(\\\\w+\\\\)_\" 0 1 'underline)

The first element is a regular expression to be matched.  The second
is a number that says what regular expression grouping used to find
the entire emphasized word.  The third is a number that says what
regexp grouping should be displayed and highlighted.  The fourth
is the face used for highlighting."
  :type '(repeat (list :value ("" 0 0 default)
		       regexp
		       (integer :tag "Match group")
		       (integer :tag "Emphasize group")
		       face))
  :group 'article)

(defface gnus-emphasis-bold '((t (:bold t)))
  "Face used for displaying strong emphasized text (*word*)."
  :group 'article)

(defface gnus-emphasis-italic '((t (:italic t)))
  "Face used for displaying italic emphasized text (/word/)."
  :group 'article)

(defface gnus-emphasis-underline '((t (:underline t)))
  "Face used for displaying underlined emphasized text (_word_)."
  :group 'article)

(defface gnus-emphasis-bold-underline '((t (:bold t :underline t)))
  "Face used for displaying underlined bold emphasized text (_*word*_)."
  :group 'article)

(defface gnus-emphasis-underline-italic '((t (:italic t :underline t)))
  "Face used for displaying underlined italic emphasized text (_*word*_)."
  :group 'article)

(defface gnus-emphasis-bold-italic '((t (:bold t :italic t)))
  "Face used for displaying bold italic emphasized text (/*word*/)."
  :group 'article)

(defface gnus-emphasis-underline-bold-italic 
  '((t (:bold t :italic t :underline t)))
  "Face used for displaying underlined bold italic emphasized text (_/*word*/_)."
  :group 'article)

(eval-and-compile
  (autoload 'hexl-hex-string-to-integer "hexl")
  (autoload 'timezone-make-date-arpa-standard "timezone")
  (autoload 'mail-extract-address-components "mail-extr"))

;;; Internal variables.

(defvar gnus-inhibit-hiding nil)
(defvar gnus-newsgroup-name)

(defsubst article-hide-text (b e props)
  "Set text PROPS on the B to E region, extending `intangible' 1 past B."
  (add-text-properties b e props)
  (when (memq 'intangible props)
    (put-text-property 
     (max (1- b) (point-min))
     b 'intangible (cddr (memq 'intangible props)))))

(defsubst article-unhide-text (b e)
  "Remove hidden text properties from region between B and E."
  (remove-text-properties b e gnus-hidden-properties)
  (when (memq 'intangible gnus-hidden-properties)
    (put-text-property (max (1- b) (point-min))
		       b 'intangible nil)))

(defun article-hide-text-type (b e type)
  "Hide text of TYPE between B and E."
  (article-hide-text
   b e (cons 'article-type (cons type gnus-hidden-properties))))

(defun article-unhide-text-type (b e type)
  "Hide text of TYPE between B and E."
  (remove-text-properties
   b e (cons 'article-type (cons type gnus-hidden-properties)))
  (when (memq 'intangible gnus-hidden-properties)
    (put-text-property (max (1- b) (point-min))
		       b 'intangible nil)))

(defun article-hide-text-of-type (type)
  "Hide text of TYPE in the current buffer."
  (save-excursion
    (let ((b (point-min))
	  (e (point-max)))
      (while (setq b (text-property-any b e 'article-type type))
	(add-text-properties b (incf b) gnus-hidden-properties)))))

(defun article-delete-text-of-type (type)
  "Delete text of TYPE in the current buffer."
  (save-excursion
    (let ((b (point-min))
	  (e (point-max)))
      (while (setq b (text-property-any b e 'article-type type))
	(delete-region b (incf b))))))

(defun article-text-type-exists-p (type)
  "Say whether any text of type TYPE exists in the buffer."
  (text-property-any (point-min) (point-max) 'article-type type))

(defsubst article-header-rank ()
  "Give the rank of the string HEADER as given by `article-sorted-header-list'."
  (let ((list gnus-sorted-header-list)
	(i 0))
    (while list
      (when (looking-at (car list))
	(setq list nil))
      (setq list (cdr list))
      (incf i))
    i))

(defun article-hide-headers (&optional arg delete)
  "Toggle whether to hide unwanted headers and possibly sort them as well.
If given a negative prefix, always show; if given a positive prefix,
always hide."
  (interactive (article-hidden-arg))
  (if (article-check-hidden-text 'headers arg)
      ;; Show boring headers as well.
      (article-show-hidden-text 'boring-headers)
    ;; This function might be inhibited.
    (unless gnus-inhibit-hiding
      (save-excursion
	(save-restriction
	  (let ((buffer-read-only nil)
		(props (nconc (list 'article-type 'headers)
			      gnus-hidden-properties))
		(max (1+ (length gnus-sorted-header-list)))
		(ignored (when (not gnus-visible-headers)
			   (cond ((stringp gnus-ignored-headers)
				  gnus-ignored-headers)
				 ((listp gnus-ignored-headers)
				  (mapconcat 'identity gnus-ignored-headers
					     "\\|")))))
		(visible
		 (cond ((stringp gnus-visible-headers)
			gnus-visible-headers)
		       ((and gnus-visible-headers
			     (listp gnus-visible-headers))
			(mapconcat 'identity gnus-visible-headers "\\|"))))
		(inhibit-point-motion-hooks t)
		want-list beg)
	    ;; First we narrow to just the headers.
	    (widen)
	    (goto-char (point-min))
	    ;; Hide any "From " lines at the beginning of (mail) articles.
	    (while (looking-at "From ")
	      (forward-line 1))
	    (unless (bobp)
	      (if delete
		  (delete-region (point-min) (point))
		(article-hide-text (point-min) (point) props)))
	    ;; Then treat the rest of the header lines.
	    (narrow-to-region
	     (point)
	     (if (search-forward "\n\n" nil t) ; if there's a body
		 (progn (forward-line -1) (point))
	       (point-max)))
	    ;; Then we use the two regular expressions
	    ;; `gnus-ignored-headers' and `gnus-visible-headers' to
	    ;; select which header lines is to remain visible in the
	    ;; article buffer.
	    (goto-char (point-min))
	    (while (re-search-forward "^[^ \t]*:" nil t)
	      (beginning-of-line)
	      ;; Mark the rank of the header.
	      (put-text-property 
	       (point) (1+ (point)) 'message-rank
	       (if (or (and visible (looking-at visible))
		       (and ignored
			    (not (looking-at ignored))))
		   (article-header-rank) 
		 (+ 2 max)))
	      (forward-line 1))
	    (message-sort-headers-1)
	    (when (setq beg (text-property-any 
			     (point-min) (point-max) 'message-rank (+ 2 max)))
	      ;; We make the unwanted headers invisible.
	      (if delete
		  (delete-region beg (point-max))
		;; Suggested by Sudish Joseph <joseph@cis.ohio-state.edu>.
		(article-hide-text-type beg (point-max) 'headers))
	      ;; Work around XEmacs lossage.
	      (put-text-property (point-min) beg 'invisible nil))))))))

(defun article-hide-boring-headers (&optional arg)
  "Toggle hiding of headers that aren't very interesting.
If given a negative prefix, always show; if given a positive prefix,
always hide."
  (interactive (article-hidden-arg))
  (unless (article-check-hidden-text 'boring-headers arg)
    (save-excursion
      (save-restriction
	(let ((buffer-read-only nil)
	      (list gnus-boring-article-headers)
	      (inhibit-point-motion-hooks t)
	      elem)
	  (nnheader-narrow-to-headers)
	  (while list
	    (setq elem (pop list))
	    (goto-char (point-min))
	    (cond
	     ;; Hide empty headers.
	     ((eq elem 'empty)
	      (while (re-search-forward "^[^:]+:[ \t]*\n[^ \t]" nil t)
		(forward-line -1)
		(article-hide-text-type
		 (progn (beginning-of-line) (point))
		 (progn 
		   (end-of-line)
		   (if (re-search-forward "^[^ \t]" nil t)
		       (match-beginning 0)
		     (point-max)))
		 'boring-headers)))
	     ;; Hide boring Newsgroups header.
	     ((eq elem 'newsgroups)
	      (when (equal (gnus-fetch-field "newsgroups")
			   (gnus-group-real-name
			    (if (boundp 'gnus-newsgroup-name)
				gnus-newsgroup-name
			      "")))
		(article-hide-header "newsgroups")))
	     ((eq elem 'followup-to)
	      (when (equal (message-fetch-field "followup-to")
			   (message-fetch-field "newsgroups"))
		(article-hide-header "followup-to")))
	     ((eq elem 'reply-to)
	      (let ((from (message-fetch-field "from"))
		    (reply-to (message-fetch-field "reply-to")))
		(when (and
		       from reply-to
		       (equal 
			(nth 1 (mail-extract-address-components from))
			(nth 1 (mail-extract-address-components reply-to))))
		  (article-hide-header "reply-to"))))
	     ((eq elem 'date)
	      (let ((date (message-fetch-field "date")))
		(when (and date
			   (< (gnus-days-between (current-time-string) date)
			      4))
		  (article-hide-header "date")))))))))))

(defun article-hide-header (header)
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward (concat "^" header ":") nil t)
      (article-hide-text-type
       (progn (beginning-of-line) (point))
       (progn 
	 (end-of-line)
	 (if (re-search-forward "^[^ \t]" nil t)
	     (match-beginning 0)
	   (point-max)))
       'boring-headers))))

;; Written by Per Abrahamsen <amanda@iesd.auc.dk>.
(defun article-treat-overstrike ()
  "Translate overstrikes into bold text."
  (interactive)
  (save-excursion
    (let ((buffer-read-only nil))
      (while (search-forward "\b" nil t)
	(let ((next (following-char))
	      (previous (char-after (- (point) 2))))
	  ;; We do the boldification/underlining by hiding the
	  ;; overstrikes and putting the proper text property
	  ;; on the letters.
	  (cond 
	   ((eq next previous)
	    (article-hide-text-type (- (point) 2) (point) 'overstrike)
	    (put-text-property (point) (1+ (point)) 'face 'bold))
	   ((eq next ?_)
	    (article-hide-text-type (1- (point)) (1+ (point)) 'overstrike)
	    (put-text-property
	     (- (point) 2) (1- (point)) 'face 'underline))
	   ((eq previous ?_)
	    (article-hide-text-type (- (point) 2) (point) 'overstrike)
	    (put-text-property
	     (point) (1+ (point)) 'face 'underline))))))))

(defun article-fill ()
  "Format too long lines."
  (interactive)
  (save-excursion
    (let ((buffer-read-only nil))
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

(defun article-remove-cr ()
  "Remove carriage returns from an article."
  (interactive)
  (save-excursion
    (let ((buffer-read-only nil))
      (goto-char (point-min))
      (while (search-forward "\r" nil t)
	(replace-match "" t t)))))

(defun article-remove-trailing-blank-lines ()
  "Remove all trailing blank lines from the article."
  (interactive)
  (save-excursion
    (let ((buffer-read-only nil))
      (goto-char (point-max))
      (delete-region
       (point)
       (progn
	 (while (and (not (bobp))
		     (looking-at "^[ \t]*$"))
	   (forward-line -1))
	 (forward-line 1)
	 (point))))))

(defun article-display-x-face (&optional force)
  "Look for an X-Face header and display it if present."
  (interactive (list 'force))
  (save-excursion
    ;; Delete the old process, if any.
    (when (process-status "article-x-face")
      (delete-process "article-x-face"))
    (let ((inhibit-point-motion-hooks t)
	  (case-fold-search nil)
	  from)
      (save-restriction
	(nnheader-narrow-to-headers)
	(setq from (message-fetch-field "from"))
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
		(if (gnus-functionp gnus-article-x-face-command)
		    (funcall gnus-article-x-face-command beg end)
		  (error "%s is not a function" gnus-article-x-face-command))
	      ;; The command is a string, so we interpret the command
	      ;; as a, well, command, and fork it off.
	      (let ((process-connection-type nil))
		(process-kill-without-query
		 (start-process
		  "article-x-face" nil shell-file-name shell-command-switch
		  gnus-article-x-face-command))
		(process-send-region "article-x-face" beg end)
		(process-send-eof "article-x-face")))))))))

(defun article-decode-rfc1522 ()
  "Hack to remove QP encoding from headers."
  (let ((case-fold-search t)
	(inhibit-point-motion-hooks t)
	(buffer-read-only nil)
	string)
    (save-restriction
      (narrow-to-region
       (goto-char (point-min))
       (or (search-forward "\n\n" nil t) (point-max)))
      (goto-char (point-min))
      (while (re-search-forward 
	      "=\\?iso-8859-1\\?q\\?\\([^?\t\n]*\\)\\?=" nil t)
	(setq string (match-string 1))
	(save-restriction
	  (narrow-to-region (match-beginning 0) (match-end 0))
	  (delete-region (point-min) (point-max))
	  (insert string)
	  (article-mime-decode-quoted-printable 
	   (goto-char (point-min)) (point-max))
	  (subst-char-in-region (point-min) (point-max) ?_ ? )
	  (goto-char (point-max)))
	(when (looking-at "\\([ \t\n]+\\)=\\?")
	  (replace-match "" t t nil 1))
	(goto-char (point-min))))))

(defun article-de-quoted-unreadable (&optional force)
  "Do a naive translation of a quoted-printable-encoded article.
This is in no way, shape or form meant as a replacement for real MIME
processing, but is simply a stop-gap measure until MIME support is
written.
If FORCE, decode the article whether it is marked as quoted-printable
or not."
  (interactive (list 'force))
  (save-excursion
    (let ((case-fold-search t)
	  (buffer-read-only nil)
	  (type (gnus-fetch-field "content-transfer-encoding")))
      (article-decode-rfc1522)
      (when (or force
		(and type (string-match "quoted-printable" (downcase type))))
	(goto-char (point-min))
	(search-forward "\n\n" nil 'move)
	(article-mime-decode-quoted-printable (point) (point-max))))))

(defun article-mime-decode-quoted-printable-buffer ()
  "Decode Quoted-Printable in the current buffer."
  (article-mime-decode-quoted-printable (point-min) (point-max)))
  
(defun article-mime-decode-quoted-printable (from to)
  "Decode Quoted-Printable in the region between FROM and TO."
  (interactive "r")
  (goto-char from)
  (while (search-forward "=" to t)
    (cond ((eq (following-char) ?\n)
	   (delete-char -1)
	   (delete-char 1))
	  ((looking-at "[0-9A-F][0-9A-F]")
	   (subst-char-in-region
	    (1- (point)) (point) ?=
	    (hexl-hex-string-to-integer
	     (buffer-substring (point) (+ 2 (point)))))
	   (delete-char 2))
	  ((looking-at "=")
	   (delete-char 1))
	  ((gnus-message 3 "Malformed MIME quoted-printable message")))))

(defun article-hide-pgp (&optional arg)
  "Toggle hiding of any PGP headers and signatures in the current article.
If given a negative prefix, always show; if given a positive prefix,
always hide."
  (interactive (article-hidden-arg))
  (unless (article-check-hidden-text 'pgp arg)
    (save-excursion
      (let (buffer-read-only beg end)
	(widen)
	(goto-char (point-min))
	;; Hide the "header".
	(and (search-forward "\n-----BEGIN PGP SIGNED MESSAGE-----\n" nil t)
	     (article-hide-text-type (match-beginning 0) (match-end 0) 'pgp))
	(setq beg (point))
	;; Hide the actual signature.
	(and (search-forward "\n-----BEGIN PGP SIGNATURE-----\n" nil t)
	     (setq end (1+ (match-beginning 0)))
	     (article-hide-text-type
	      end
	      (if (search-forward "\n-----END PGP SIGNATURE-----\n" nil t)
		  (match-end 0)
		;; Perhaps we shouldn't hide to the end of the buffer
		;; if there is no end to the signature?
		(point-max))
	      'pgp))
	;; Hide "- " PGP quotation markers.
	(when (and beg end)
	  (narrow-to-region beg end)
	  (goto-char (point-min))
	  (while (re-search-forward "^- " nil t)
	    (article-hide-text-type (match-beginning 0) (match-end 0) 'pgp))
	  (widen))))))

(defun article-hide-pem (&optional arg)
  "Toggle hiding of any PEM headers and signatures in the current article.
If given a negative prefix, always show; if given a positive prefix,
always hide."
  (interactive (article-hidden-arg))
  (unless (article-check-hidden-text 'pem arg)
    (save-excursion
      (let (buffer-read-only end)
	(widen)
	(goto-char (point-min))
	;; hide the horrendously ugly "header".
	(and (search-forward "\n-----BEGIN PRIVACY-ENHANCED MESSAGE-----\n"
			     nil
			     t)
	     (setq end (1+ (match-beginning 0)))
	     (article-hide-text-type
	      end
	      (if (search-forward "\n\n" nil t)
		  (match-end 0)
		(point-max))
	      'pem))
	;; hide the trailer as well
	(and (search-forward "\n-----END PRIVACY-ENHANCED MESSAGE-----\n"
			     nil
			     t)
	     (article-hide-text-type
	      (match-beginning 0) (match-end 0) 'pem))))))

(defun article-hide-signature (&optional arg)
  "Hide the signature in the current article.
If given a negative prefix, always show; if given a positive prefix,
always hide."
  (interactive (article-hidden-arg))
  (unless (article-check-hidden-text 'signature arg)
    (save-excursion
      (save-restriction
	(let ((buffer-read-only nil))
	  (when (article-narrow-to-signature)
	    (article-hide-text-type (point-min) (point-max) 'signature)))))))

(defun article-strip-leading-blank-lines ()
  "Remove all blank lines from the beginning of the article."
  (interactive)
  (save-excursion
    (let (buffer-read-only)
      (goto-char (point-min))
      (when (search-forward "\n\n" nil t)
	(while (and (not (eobp))
		    (looking-at "[ \t]*$"))
	  (gnus-delete-line))))))

(defun article-strip-multiple-blank-lines ()
  "Replace consecutive blank lines with one empty line."
  (interactive)
  (save-excursion
    (let (buffer-read-only)
      ;; First make all blank lines empty.
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]+$" nil t)
	(replace-match "" nil t))
      ;; Then replace multiple empty lines with a single empty line.
      (goto-char (point-min))
      (while (re-search-forward "\n\n\n+" nil t)
	(replace-match "\n\n" t t)))))

(defun article-strip-blank-lines ()
  "Strip leading, trailing and multiple blank lines."
  (interactive)
  (article-strip-leading-blank-lines)
  (article-remove-trailing-blank-lines)
  (article-strip-multiple-blank-lines))

(defvar mime::preview/content-list)
(defvar mime::preview-content-info/point-min)
(defun article-narrow-to-signature ()
  "Narrow to the signature; return t if a signature is found, else nil."
  (widen)
  (when (and (boundp 'mime::preview/content-list)
	     mime::preview/content-list)
    ;; We have a MIMEish article, so we use the MIME data to narrow.
    (let ((pcinfo (car (last mime::preview/content-list))))
      (ignore-errors
	(narrow-to-region
	 (funcall (intern "mime::preview-content-info/point-min") pcinfo)
	 (point-max)))))
  
  (when (article-search-signature)
    (forward-line 1)
    ;; Check whether we have some limits to what we consider
    ;; to be a signature.
    (let ((limits (if (listp gnus-signature-limit) gnus-signature-limit
		    (list gnus-signature-limit)))
	  limit limited)
      (while (setq limit (pop limits))
	(if (or (and (integerp limit)
		     (< (- (point-max) (point)) limit))
		(and (floatp limit)
		     (< (count-lines (point) (point-max)) limit))
		(and (gnus-functionp limit)
		     (funcall limit))
		(and (stringp limit)
		     (not (re-search-forward limit nil t))))
	    ()				; This limit did not succeed.
	  (setq limited t
		limits nil)))
      (unless limited
	(narrow-to-region (point) (point-max))
	t))))

(defun article-search-signature ()
  "Search the current buffer for the signature separator.
Put point at the beginning of the signature separator."
  (let ((cur (point)))
    (goto-char (point-max))
    (if (if (stringp gnus-signature-separator)
	    (re-search-backward gnus-signature-separator nil t)
	  (let ((seps gnus-signature-separator))
	    (while (and seps
			(not (re-search-backward (car seps) nil t)))
	      (pop seps))
	    seps))
	t
      (goto-char cur)
      nil)))

(defun article-hidden-arg ()
  "Return the current prefix arg as a number, or 0 if no prefix."
  (list (if current-prefix-arg
	    (prefix-numeric-value current-prefix-arg)
	  0)))

(defun article-check-hidden-text (type arg)
  "Return nil if hiding is necessary.
Arg can be nil or a number.  Nil and positive means hide, negative
means show, 0 means toggle."
  (save-excursion
    (let ((hide (article-hidden-text-p type)))
      (cond
       ((or (null arg)
	    (> arg 0))
	nil)
       ((< arg 0)
	(article-show-hidden-text type))
       (t
	(if (eq hide 'hidden)
	    (article-show-hidden-text type)
	  nil))))))

(defun article-hidden-text-p (type)
  "Say whether the current buffer contains hidden text of type TYPE."
  (let ((pos (text-property-any (point-min) (point-max) 'article-type type)))
    (when pos
      (if (get-text-property pos 'invisible)
	  'hidden
	'shown))))

(defun article-show-hidden-text (type &optional hide)
  "Show all hidden text of type TYPE.
If HIDE, hide the text instead."
  (save-excursion
    (let ((buffer-read-only nil)
	  (inhibit-point-motion-hooks t)
	  (beg (point-min)))
      (while (gnus-goto-char (text-property-any
			      beg (point-max) 'article-type type))
	(setq beg (point))
	(forward-char)
	(if hide
	    (article-hide-text beg (point) gnus-hidden-properties)
	  (article-unhide-text beg (point)))
	(setq beg (point)))
      t)))

(defconst article-time-units
  `((year . ,(* 365.25 24 60 60))
    (week . ,(* 7 24 60 60))
    (day . ,(* 24 60 60))
    (hour . ,(* 60 60))
    (minute . 60)
    (second . 1))
  "Mapping from time units to seconds.")

(defun article-date-ut (&optional type highlight header)
  "Convert DATE date to universal time in the current article.
If TYPE is `local', convert to local time; if it is `lapsed', output
how much time has lapsed since DATE."
  (interactive (list 'ut t))
  (let* ((header (or header (message-fetch-field "date") ""))
	 (date (if (vectorp header) (mail-header-date header)
		 header))
	 (date-regexp "^Date: \\|^X-Sent: ")
	 (inhibit-point-motion-hooks t)
	 bface eface)
    (when (and date (not (string= date "")))
      (save-excursion
	(save-restriction
	  (nnheader-narrow-to-headers)
	  (let ((buffer-read-only nil))
	    ;; Delete any old Date headers.
	    (if (re-search-forward date-regexp nil t)
		(progn
		  (setq bface (get-text-property (gnus-point-at-bol) 'face)
			eface (get-text-property (1- (gnus-point-at-eol))
						 'face))
		  (message-remove-header date-regexp t)
		  (beginning-of-line))
	      (goto-char (point-max)))
	    (insert (article-make-date-line date type))
	    ;; Do highlighting.
	    (forward-line -1)
	    (when (looking-at "\\([^:]+\\): *\\(.*\\)$")
	      (put-text-property (match-beginning 1) (match-end 1)
				 'face bface)
	      (put-text-property (match-beginning 2) (match-end 2)
				 'face eface))))))))

(defun article-make-date-line (date type)
  "Return a DATE line of TYPE."
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
    ;; If the date is seriously mangled, the timezone functions are
    ;; liable to bug out, so we ignore all errors.
    (let* ((now (current-time))
	   (real-time
	    (ignore-errors
	      (gnus-time-minus
	       (gnus-encode-date
		(timezone-make-date-arpa-standard
		 (current-time-string now)
		 (current-time-zone now) "UT"))
	       (gnus-encode-date
		(timezone-make-date-arpa-standard
		 date nil "UT")))))
	   (real-sec (and real-time
			  (+ (* (float (car real-time)) 65536)
			     (cadr real-time))))
	   (sec (and real-time (abs real-sec)))
	   num prev)
      (cond
       ((null real-time)
	"X-Sent: Unknown\n")
       ((zerop sec)
	"X-Sent: Now\n")
       (t
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
	  article-time-units "")
	 ;; If dates are odd, then it might appear like the
	 ;; article was sent in the future.
	 (if (> real-sec 0)
	     " ago\n"
	   " in the future\n"))))))
   (t
    (error "Unknown conversion type: %s" type))))

(defun article-date-local (&optional highlight)
  "Convert the current article date to the local timezone."
  (interactive (list t))
  (article-date-ut 'local highlight))

(defun article-date-original (&optional highlight)
  "Convert the current article date to what it was originally.
This is only useful if you have used some other date conversion
function and want to see what the date was before converting."
  (interactive (list t))
  (article-date-ut 'original highlight))

(defun article-date-lapsed (&optional highlight)
  "Convert the current article date to time lapsed since it was sent."
  (interactive (list t))
  (article-date-ut 'lapsed highlight))

(defun article-show-all ()
  "Show all hidden text in the article buffer."
  (interactive)
  (save-excursion
    (let ((buffer-read-only nil))
      (article-unhide-text (point-min) (point-max)))))

(defun article-emphasize (&optional arg)
  "Emphasize text according to `gnus-emphasis-alist'."
  (interactive (article-hidden-arg))
  (unless (article-check-hidden-text 'emphasis arg)
    (save-excursion
      (let ((alist gnus-emphasis-alist)
	    (buffer-read-only nil)
	    (props (append '(article-type emphasis)
			   gnus-hidden-properties))
	    regexp elem beg invisible visible face)
	(goto-char (point-min))
	(search-forward "\n\n" nil t)
	(setq beg (point))
	(while (setq elem (pop alist))
	  (goto-char beg)
	  (setq regexp (car elem)
		invisible (nth 1 elem)
		visible (nth 2 elem)
		face (nth 3 elem))
	  (while (re-search-forward regexp nil t)
 	    (when (and (match-beginning visible) (match-beginning invisible))
 	      (article-hide-text
 	       (match-beginning invisible) (match-end invisible) props)
 	      (article-unhide-text-type
 	       (match-beginning visible) (match-end visible) 'emphasis)
 	      (put-text-property 
 	       (match-beginning visible) (match-end visible) 'face face)
 	      (goto-char (match-end invisible)))))))))

(provide 'article)

;;; article.el ends here
