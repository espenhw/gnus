;;; gnus-art.el --- article mode commands for Gnus
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

(require 'gnus-load)
(require 'gnus-sum)
(require 'article)
(require 'gnus-spec)
(require 'gnus-int)
(require 'browse-url)

(defcustom gnus-article-save-directory gnus-directory
  "*Name of the directory articles will be saved in (default \"~/News\")."
  :group 'article
  :type 'directory)

(defcustom gnus-save-all-headers t
  "*If non-nil, don't remove any headers before saving."
  :group 'article
  :type 'boolean)

(defcustom gnus-prompt-before-saving 'always
  "*This variable says how much prompting is to be done when saving articles.
If it is nil, no prompting will be done, and the articles will be
saved to the default files.  If this variable is `always', each and
every article that is saved will be preceded by a prompt, even when
saving large batches of articles.  If this variable is neither nil not
`always', there the user will be prompted once for a file name for
each invocation of the saving commands."
  :group 'article
  :type '(choice (item always)
		 (item :tag "never" nil)
		 (sexp :tag "once" :format "%t")))

(defcustom gnus-saved-headers gnus-visible-headers
  "Headers to keep if `gnus-save-all-headers' is nil.
If `gnus-save-all-headers' is non-nil, this variable will be ignored.
If that variable is nil, however, all headers that match this regexp
will be kept while the rest will be deleted before saving."
  :group 'article
  :type '(repeat string))

(defcustom gnus-default-article-saver 'gnus-summary-save-in-rmail
  "A function to save articles in your favourite format.
The function must be interactively callable (in other words, it must
be an Emacs command).

Gnus provides the following functions:

* gnus-summary-save-in-rmail (Rmail format)
* gnus-summary-save-in-mail (Unix mail format)
* gnus-summary-save-in-folder (MH folder)
* gnus-summary-save-in-file (article format).
* gnus-summary-save-in-vm (use VM's folder format)."
  :group 'article
  :type '(radio (function-item gnus-summary-save-in-rmail)
		(function-item gnus-summary-save-in-mail)
		(function-item gnus-summary-save-in-folder)
		(function-item gnus-summary-save-in-file)
		(function-item gnus-summary-save-in-vm)))

(defcustom gnus-rmail-save-name 'gnus-plain-save-name
  "A function generating a file name to save articles in Rmail format.
The function is called with NEWSGROUP, HEADERS, and optional LAST-FILE."
  :group 'article
  :type 'function)

(defcustom gnus-mail-save-name 'gnus-plain-save-name
  "A function generating a file name to save articles in Unix mail format.
The function is called with NEWSGROUP, HEADERS, and optional LAST-FILE."
  :group 'article
  :type 'function)

(defcustom gnus-folder-save-name 'gnus-folder-save-name
  "A function generating a file name to save articles in MH folder.
The function is called with NEWSGROUP, HEADERS, and optional LAST-FOLDER."
  :group 'article
  :type 'function)

(defcustom gnus-file-save-name 'gnus-numeric-save-name
  "A function generating a file name to save articles in article format.
The function is called with NEWSGROUP, HEADERS, and optional
LAST-FILE."
  :group 'article
  :type 'function)

(defcustom gnus-split-methods
  '((gnus-article-archive-name))
  "Variable used to suggest where articles are to be saved.
For instance, if you would like to save articles related to Gnus in
the file \"gnus-stuff\", and articles related to VM in \"vm-stuff\",
you could set this variable to something like:

 '((\"^Subject:.*gnus\\|^Newsgroups:.*gnus\" \"gnus-stuff\")
   (\"^Subject:.*vm\\|^Xref:.*vm\" \"vm-stuff\"))

This variable is an alist where the where the key is the match and the
value is a list of possible files to save in if the match is non-nil.

If the match is a string, it is used as a regexp match on the
article.  If the match is a symbol, that symbol will be funcalled
from the buffer of the article to be saved with the newsgroup as the
parameter.  If it is a list, it will be evaled in the same buffer.

If this form or function returns a string, this string will be used as
a possible file name; and if it returns a non-nil list, that list will
be used as possible file names."
  :group 'article
  :type '(repeat (choice (list function)
			 (cons regexp (repeat string))
			 sexp)))

(defcustom gnus-strict-mime t
  "*If nil, MIME-decode even if there is no Mime-Version header."
  :group 'article
  :type 'boolean)

(defcustom gnus-show-mime-method 'metamail-buffer
  "Function to process a MIME message.
The function is called from the article buffer."
  :group 'article
  :type 'function)

(defcustom gnus-decode-encoded-word-method (lambda ())
  "*Function to decode a MIME encoded-words.
The function is called from the article buffer."
  :group 'article
  :type 'function)

(defcustom gnus-page-delimiter "^\^L"
  "*Regexp describing what to use as article page delimiters.
The default value is \"^\^L\", which is a form linefeed at the
beginning of a line."
  :type 'regexp
  :group 'article)

(defcustom gnus-article-mode-line-format "Gnus: %%b %S"
  "*The format specification for the article mode line.
See `gnus-summary-mode-line-format' for a closer description."
  :type 'string
  :group 'article)

(defcustom gnus-article-mode-hook nil
  "*A hook for Gnus article mode."
  :type 'hook
  :group 'article)

(defcustom gnus-article-menu-hook nil
  "*Hook run after the creation of the article mode menu."
  :type 'hook
  :group 'article)

(defcustom gnus-article-prepare-hook nil
  "*A hook called after an article has been prepared in the article buffer.
If you want to run a special decoding program like nkf, use this hook."
  :type 'hook
  :group 'article)

(defcustom gnus-article-button-face 'bold
  "Face used for highlighting buttons in the article buffer.

An article button is a piece of text that you can activate by pressing
`RET' or `mouse-2' above it."
  :type 'face
  :group 'article)

(defcustom gnus-article-mouse-face 'highlight
  "Face used for mouse highlighting in the article buffer.

Article buttons will be displayed in this face when the cursor is
above them."
  :type 'face
  :group 'article)

(defcustom gnus-signature-face 'italic
  "Face used for highlighting a signature in the article buffer."
  :type 'face
  :group 'article)

(defface gnus-header-from-face 
  '((((class color)
      (background dark))
     (:foreground "light blue" :bold t :italic t))
    (((class color)
      (background light))
     (:foreground "MidnightBlue" :bold t :italic t))
    (t 
     (:bold t :italic t)))
  "Face used for displaying from headers."
  :group 'article)

(defface gnus-header-subject-face 
  '((((class color)
      (background dark))
     (:foreground "pink" :bold t :italic t))
    (((class color)
      (background light))
     (:foreground "firebrick" :bold t :italic t))
    (t 
     (:bold t :italic t)))
  "Face used for displaying subject headers."
  :group 'article)

(defface gnus-header-newsgroups-face 
  '((((class color)
      (background dark))
     (:foreground "yellow" :bold t :italic t))
    (((class color)
      (background light))
     (:foreground "indianred" :bold t :italic t))
    (t 
     (:bold t :italic t)))
  "Face used for displaying newsgroups headers."
  :group 'article)

(defface gnus-header-name-face 
  '((((class color)
      (background dark))
     (:foreground "cyan" :bold t))
    (((class color)
      (background light))
     (:foreground "DarkGreen" :bold t))
    (t 
     (:bold t)))
  "Face used for displaying header names."
  :group 'article)

(defface gnus-header-content-face
  '((((class color)
      (background dark))
     (:foreground "forest green" :italic t))
    (((class color)
      (background light))
     (:foreground "DarkGreen" :italic t))
    (t 
     (:italic t)))  "Face used for displaying header content."
  :group 'article)

(defcustom gnus-header-face-alist
  '(("From" nil gnus-header-from-face)
    ("Subject" nil gnus-header-subject-face)
    ("Newsgroups:.*," nil gnus-header-newsgroups-face)
    ("" gnus-header-name-face gnus-header-content-face))
  "Controls highlighting of article header.

An alist of the form (HEADER NAME CONTENT). 

HEADER is a regular expression which should match the name of an
header header and NAME and CONTENT are either face names or nil.

The name of each header field will be displayed using the face
specified by the first element in the list where HEADER match the
header name and NAME is non-nil.  Similarly, the content will be
displayed by the first non-nil matching CONTENT face."
  :group 'article
  :type '(repeat (list (regexp :tag "Header")
		       (choice :tag "Name"
			       (item :tag "skip" nil)
			       (face :value default))
		       (choice :tag "Content"
			       (item :tag "skip" nil)
			       (face :value default)))))

;;; Internal variables

(defvar gnus-article-mode-line-format-alist
    (nconc '((?w (gnus-article-wash-status) ?s))
	   gnus-summary-mode-line-format-alist))

(defvar gnus-number-of-articles-to-be-saved nil)

;;; Provide a mapping from `gnus-*' commands to Article commands.

(eval-and-compile
  (mapcar
   (lambda (func)
     (let (afunc gfunc)
       (if (consp func)
	   (setq afunc (car func)
		 gfunc (cdr func))
	 (setq afunc func
	       gfunc (intern (format "gnus-%s" func))))
       (fset gfunc 
	     `(lambda (&optional interactive &rest args)
		,(documentation afunc t)
		(interactive (list t))
		(save-excursion
		  (set-buffer gnus-article-buffer)
		  (if interactive
		      (call-interactively ',afunc)
		    (apply ',afunc args)))))))
   '(article-hide-headers
     article-hide-boring-headers
     article-treat-overstrike
     (article-fill . gnus-article-word-wrap)
     article-remove-cr
     article-display-x-face
     article-de-quoted-unreadable
     article-mime-decode-quoted-printable
     article-hide-pgp
     article-hide-pem
     article-hide-signature
     article-remove-trailing-blank-lines
     article-strip-leading-blank-lines
     article-strip-multiple-blank-lines
     article-strip-blank-lines
     article-date-local
     article-date-original
     article-date-lapsed
     article-emphasize
     (article-show-all . gnus-article-show-all-headers))))

(defalias 'gnus-decode-rfc1522 'article-decode-rfc1522)

;;; Saving functions.

(defun gnus-article-save (save-buffer file &optional num)
  "Save the currently selected article."
  (unless gnus-save-all-headers
    ;; Remove headers according to `gnus-saved-headers'.
    (let ((gnus-visible-headers
	   (or gnus-saved-headers gnus-visible-headers))
	  (gnus-article-buffer save-buffer))
      (gnus-article-hide-headers 1 t)))
  (save-window-excursion
    (if (not gnus-default-article-saver)
	(error "No default saver is defined.")
      ;; !!! Magic!  The saving functions all save
      ;; `gnus-original-article-buffer' (or so they think),
      ;; but we bind that variable to our save-buffer.
      (set-buffer gnus-article-buffer)
      (let* ((gnus-original-article-buffer save-buffer)
	     (filename
	      (cond
	       ((not gnus-prompt-before-saving)
		'default)
	       ((eq gnus-prompt-before-saving 'always)
		nil)
	       (t file)))
	     (gnus-number-of-articles-to-be-saved
	      (when (eq gnus-prompt-before-saving t) num))) ; Magic
	(set-buffer gnus-summary-buffer)
	(funcall gnus-default-article-saver filename)))))

(defun gnus-read-save-file-name (prompt default-name &optional filename)
  (cond
   ((eq filename 'default)
    default-name)
   (filename filename)
   (t
    (let* ((split-name (gnus-get-split-value gnus-split-methods))
	   (prompt
	    (format prompt (if (and gnus-number-of-articles-to-be-saved
				    (> gnus-number-of-articles-to-be-saved 1))
			       (format "these %d articles"
				       gnus-number-of-articles-to-be-saved)
			     "this article")))
	   (file
	    ;; Let the split methods have their say.
	    (cond
	     ;; No split name was found.
	     ((null split-name)
	      (read-file-name
	       (concat prompt " (default "
		       (file-name-nondirectory default-name) ") ")
	       (file-name-directory default-name)
	       default-name))
	     ;; A single split name was found
	     ((= 1 (length split-name))
	      (let* ((name (car split-name))
		     (dir (cond ((file-directory-p name)
				 (file-name-as-directory name))
				((file-exists-p name) name)
				(t gnus-article-save-directory))))
		(read-file-name
		 (concat prompt " (default " name ") ")
		 dir name)))
	     ;; A list of splits was found.
	     (t
	      (setq split-name (nreverse split-name))
	      (let (result)
		(let ((file-name-history (nconc split-name file-name-history)))
		  (setq result
			(read-file-name
			 (concat prompt " (`M-p' for defaults) ")
			 gnus-article-save-directory
			 (car split-name))))
		(car (push result file-name-history)))))))
      ;; Create the directory.
      (gnus-make-directory (file-name-directory file))
      ;; If we have read a directory, we append the default file name.
      (when (file-directory-p file)
	(setq file (concat (file-name-as-directory file)
			   (file-name-nondirectory default-name))))
      ;; Possibly translate some characters.
      (nnheader-translate-file-chars file)))))

(defun gnus-article-archive-name (group)
  "Return the first instance of an \"Archive-name\" in the current buffer."
  (let ((case-fold-search t))
    (when (re-search-forward "archive-name: *\\([^ \n\t]+\\)[ \t]*$" nil t)
      (nnheader-concat gnus-article-save-directory
		       (match-string 1)))))

(defun gnus-summary-save-in-rmail (&optional filename)
  "Append this article to Rmail file.
Optional argument FILENAME specifies file name.
Directory to save to is default to `gnus-article-save-directory'."
  (interactive)
  (gnus-set-global-variables)
  (let ((default-name
	  (funcall gnus-rmail-save-name gnus-newsgroup-name
		   gnus-current-headers gnus-newsgroup-last-rmail)))
    (setq filename (gnus-read-save-file-name
		    "Save %s in rmail file:" default-name filename))
    (gnus-make-directory (file-name-directory filename))
    (gnus-eval-in-buffer-window gnus-original-article-buffer
      (save-excursion
	(save-restriction
	  (widen)
	  (gnus-output-to-rmail filename))))
    ;; Remember the directory name to save articles
    (setq gnus-newsgroup-last-rmail filename)))

(defun gnus-summary-save-in-mail (&optional filename)
  "Append this article to Unix mail file.
Optional argument FILENAME specifies file name.
Directory to save to is default to `gnus-article-save-directory'."
  (interactive)
  (gnus-set-global-variables)
  (let ((default-name
	  (funcall gnus-mail-save-name gnus-newsgroup-name
		   gnus-current-headers gnus-newsgroup-last-mail)))
    (setq filename (gnus-read-save-file-name
		    "Save %s in Unix mail file:" default-name filename))
    (setq filename
	  (expand-file-name filename
			    (and default-name
				 (file-name-directory default-name))))
    (gnus-make-directory (file-name-directory filename))
    (gnus-eval-in-buffer-window gnus-original-article-buffer
      (save-excursion
	(save-restriction
	  (widen)
	  (if (and (file-readable-p filename) (mail-file-babyl-p filename))
	      (gnus-output-to-rmail filename)
	    (let ((mail-use-rfc822 t))
	      (rmail-output filename 1 t t))))))
    ;; Remember the directory name to save articles.
    (setq gnus-newsgroup-last-mail filename)))

(defun gnus-summary-save-in-file (&optional filename)
  "Append this article to file.
Optional argument FILENAME specifies file name.
Directory to save to is default to `gnus-article-save-directory'."
  (interactive)
  (gnus-set-global-variables)
  (let ((default-name
	  (funcall gnus-file-save-name gnus-newsgroup-name
		   gnus-current-headers gnus-newsgroup-last-file)))
    (setq filename (gnus-read-save-file-name
		    "Save %s in file:" default-name filename))
    (gnus-make-directory (file-name-directory filename))
    (gnus-eval-in-buffer-window gnus-original-article-buffer
      (save-excursion
	(save-restriction
	  (widen)
	  (gnus-output-to-file filename))))
    ;; Remember the directory name to save articles.
    (setq gnus-newsgroup-last-file filename)))

(defun gnus-summary-save-body-in-file (&optional filename)
  "Append this article body to a file.
Optional argument FILENAME specifies file name.
The directory to save in defaults to `gnus-article-save-directory'."
  (interactive)
  (gnus-set-global-variables)
  (let ((default-name
	  (funcall gnus-file-save-name gnus-newsgroup-name
		   gnus-current-headers gnus-newsgroup-last-file)))
    (setq filename (gnus-read-save-file-name
		    "Save %s body in file:" default-name filename))
    (gnus-make-directory (file-name-directory filename))
    (gnus-eval-in-buffer-window gnus-original-article-buffer
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
  (gnus-eval-in-buffer-window gnus-article-buffer
    (save-restriction
      (widen)
      (shell-command-on-region (point-min) (point-max) command nil)))
  (setq gnus-last-shell-command command))

;;; Article file names when saving.

(defun gnus-capitalize-newsgroup (newsgroup)
  "Capitalize NEWSGROUP name."
  (and (not (zerop (length newsgroup)))
       (concat (char-to-string (upcase (aref newsgroup 0)))
	       (substring newsgroup 1))))

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
	   gnus-article-save-directory)))
    (if (and last-file
	     (string-equal (file-name-directory default)
			   (file-name-directory last-file))
	     (string-match "^[0-9]+$" (file-name-nondirectory last-file)))
	default
      (or last-file default))))

(defun gnus-numeric-save-name (newsgroup headers &optional last-file)
  "Generate file name from NEWSGROUP, HEADERS, and optional LAST-FILE.
If variable `gnus-use-long-file-name' is non-nil, it is
~/News/news.group/num.	Otherwise, it is like ~/News/news/group/num."
  (let ((default
	  (expand-file-name
	   (concat (if (gnus-use-long-file-name 'not-save)
		       newsgroup
		     (gnus-newsgroup-directory-form newsgroup))
		   "/" (int-to-string (mail-header-number headers)))
	   gnus-article-save-directory)))
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
       gnus-article-save-directory)))

(defun gnus-plain-save-name (newsgroup headers &optional last-file)
  "Generate file name from NEWSGROUP, HEADERS, and optional LAST-FILE.
If variable `gnus-use-long-file-name' is non-nil, it is
~/News/news.group.  Otherwise, it is like ~/News/news/group/news."
  (or last-file
      (expand-file-name
       (if (gnus-use-long-file-name 'not-save)
	   newsgroup
	 (concat (gnus-newsgroup-directory-form newsgroup) "/news"))
       gnus-article-save-directory)))


;;;
;;; Gnus article mode
;;;

(put 'gnus-article-mode 'mode-class 'special)

(when t
  (gnus-define-keys gnus-article-mode-map
    " " gnus-article-goto-next-page
    "\177" gnus-article-goto-prev-page
    [delete] gnus-article-goto-prev-page
    "\C-c^" gnus-article-refer-article
    "h" gnus-article-show-summary
    "s" gnus-article-show-summary
    "\C-c\C-m" gnus-article-mail
    "?" gnus-article-describe-briefly
    gnus-mouse-2 gnus-article-push-button
    "\r" gnus-article-press-button
    "\t" gnus-article-next-button
    "\M-\t" gnus-article-prev-button
    "e" gnus-article-edit
    "<" beginning-of-buffer
    ">" end-of-buffer
    "\C-c\C-i" gnus-info-find-node
    "\C-c\C-b" gnus-bug

    "\C-d" gnus-article-read-summary-keys
    "\M-g" gnus-article-read-summary-keys)

  (substitute-key-definition
   'undefined 'gnus-article-read-summary-keys gnus-article-mode-map))

(defun gnus-article-make-menu-bar ()
  (gnus-turn-off-edit-menu 'article)
  (unless (boundp 'gnus-article-article-menu)
    (easy-menu-define
     gnus-article-article-menu gnus-article-mode-map ""
     '("Article"
       ["Scroll forwards" gnus-article-goto-next-page t]
       ["Scroll backwards" gnus-article-goto-prev-page t]
       ["Show summary" gnus-article-show-summary t]
       ["Fetch Message-ID at point" gnus-article-refer-article t]
       ["Mail to address at point" gnus-article-mail t]
       ))

    (easy-menu-define
     gnus-article-treatment-menu gnus-article-mode-map ""
     '("Treatment"
       ["Hide headers" gnus-article-hide-headers t]
       ["Hide signature" gnus-article-hide-signature t]
       ["Hide citation" gnus-article-hide-citation t]
       ["Treat overstrike" gnus-article-treat-overstrike t]
       ["Remove carriage return" gnus-article-remove-cr t]
       ["Remove quoted-unreadable" gnus-article-de-quoted-unreadable t]
       ))
    (run-hooks 'gnus-article-menu-hook)))

(defun gnus-article-mode ()
  "Major mode for displaying an article.

All normal editing commands are switched off.

The following commands are available in addition to all summary mode
commands:
\\<gnus-article-mode-map>
\\[gnus-article-next-page]\t Scroll the article one page forwards
\\[gnus-article-prev-page]\t Scroll the article one page backwards
\\[gnus-article-refer-article]\t Go to the article referred to by an article id near point
\\[gnus-article-show-summary]\t Display the summary buffer
\\[gnus-article-mail]\t Send a reply to the address near point
\\[gnus-article-describe-briefly]\t Describe the current mode briefly
\\[gnus-info-find-node]\t Go to the Gnus info node"
  (interactive)
  (when (and menu-bar-mode
	     (gnus-visual-p 'article-menu 'menu))
    (gnus-article-make-menu-bar))
  (kill-all-local-variables)
  (gnus-simplify-mode-line)
  (setq mode-name "Article")
  (setq major-mode 'gnus-article-mode)
  (make-local-variable 'minor-mode-alist)
  (unless (assq 'gnus-show-mime minor-mode-alist)
    (push (list 'gnus-show-mime " MIME") minor-mode-alist))
  (use-local-map gnus-article-mode-map)
  (gnus-update-format-specifications nil 'article-mode)
  (set (make-local-variable 'page-delimiter) gnus-page-delimiter)
  (gnus-set-default-directory)
  (buffer-disable-undo (current-buffer))
  (setq buffer-read-only t)
  (run-hooks 'gnus-article-mode-hook))

(defun gnus-article-setup-buffer ()
  "Initialize the article buffer."
  (let* ((name (if gnus-single-article-buffer "*Article*"
		 (concat "*Article " gnus-newsgroup-name "*")))
	 (original
	  (progn (string-match "\\*Article" name)
		 (concat " *Original Article"
			 (substring name (match-end 0))))))
    (setq gnus-article-buffer name)
    (setq gnus-original-article-buffer original)
    ;; This might be a variable local to the summary buffer.
    (unless gnus-single-article-buffer
      (save-excursion
	(set-buffer gnus-summary-buffer)
	(setq gnus-article-buffer name)
	(setq gnus-original-article-buffer original)
	(gnus-set-global-variables)))
    ;; Init original article buffer.
    (save-excursion
      (set-buffer (get-buffer-create gnus-original-article-buffer))
      (buffer-disable-undo (current-buffer))
      (setq major-mode 'gnus-original-article-mode)
      (gnus-add-current-to-buffer-list)
      (make-local-variable 'gnus-original-article))
    (if (get-buffer name)
	(save-excursion
	  (set-buffer name)
	  (buffer-disable-undo (current-buffer))
	  (setq buffer-read-only t)
	  (gnus-add-current-to-buffer-list)
	  (unless (eq major-mode 'gnus-article-mode)
	    (gnus-article-mode))
	  (current-buffer))
      (save-excursion
	(set-buffer (get-buffer-create name))
	(gnus-add-current-to-buffer-list)
	(gnus-article-mode)
	(make-local-variable 'gnus-summary-buffer)
	(current-buffer)))))

;; Set article window start at LINE, where LINE is the number of lines
;; from the head of the article.
(defun gnus-article-set-window-start (&optional line)
  (set-window-start
   (get-buffer-window gnus-article-buffer t)
   (save-excursion
     (set-buffer gnus-article-buffer)
     (goto-char (point-min))
     (if (not line)
	 (point-min)
       (gnus-message 6 "Moved to bookmark")
       (search-forward "\n\n" nil t)
       (forward-line line)
       (point)))))

(defun gnus-article-prepare (article &optional all-headers header)
  "Prepare ARTICLE in article mode buffer.
ARTICLE should either be an article number or a Message-ID.
If ARTICLE is an id, HEADER should be the article headers.
If ALL-HEADERS is non-nil, no headers are hidden."
  (save-excursion
    ;; Make sure we start in a summary buffer.
    (unless (eq major-mode 'gnus-summary-mode)
      (set-buffer gnus-summary-buffer))
    (setq gnus-summary-buffer (current-buffer))
    ;; Make sure the connection to the server is alive.
    (unless (gnus-server-opened
	     (gnus-find-method-for-group gnus-newsgroup-name))
      (gnus-check-server (gnus-find-method-for-group gnus-newsgroup-name))
      (gnus-request-group gnus-newsgroup-name t))
    (let* ((article (if header (mail-header-number header) article))
	   (summary-buffer (current-buffer))
	   (internal-hook gnus-article-internal-prepare-hook)
	   (group gnus-newsgroup-name)
	   result)
      (save-excursion
	(gnus-article-setup-buffer)
	(set-buffer gnus-article-buffer)
	;; Deactivate active regions.
	(when (and (boundp 'transient-mark-mode)
		   transient-mark-mode)
	  (setq mark-active nil))
	(if (not (setq result (let ((buffer-read-only nil))
				(gnus-request-article-this-buffer
				 article group))))
	    ;; There is no such article.
	    (save-excursion
	      (when (and (numberp article)
			 (not (memq article gnus-newsgroup-sparse)))
		(setq gnus-article-current
		      (cons gnus-newsgroup-name article))
		(set-buffer gnus-summary-buffer)
		(setq gnus-current-article article)
		(gnus-summary-mark-article article gnus-canceled-mark))
	      (unless (memq article gnus-newsgroup-sparse)
		(gnus-error
		 1 "No such article (may have expired or been canceled)")))
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
		  (unless (vectorp gnus-current-headers)
		    (setq gnus-current-headers nil))
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
	    (when (or (numberp article)
		      (stringp article))
	      ;; Hooks for getting information from the article.
	      ;; This hook must be called before being narrowed.
	      (let (buffer-read-only)
		(run-hooks 'internal-hook)
		(run-hooks 'gnus-article-prepare-hook)
		;; Decode MIME message.
		(if gnus-show-mime
		    (if (or (not gnus-strict-mime)
			    (gnus-fetch-field "Mime-Version"))
			(funcall gnus-show-mime-method)
		      (funcall gnus-decode-encoded-word-method)))
		;; Perform the article display hooks.
		(run-hooks 'gnus-article-display-hook))
	      ;; Do page break.
	      (goto-char (point-min))
	      (and gnus-break-pages (gnus-narrow-to-page)))
	    (gnus-set-mode-line 'article)
	    (gnus-configure-windows 'article)
	    (goto-char (point-min))
	    t))))))

(defun gnus-article-wash-status ()
  "Return a string which display status of article washing."
  (save-excursion
    (set-buffer gnus-article-buffer)
    (let ((cite (article-hidden-text-p 'cite))
	  (headers (article-hidden-text-p 'headers))
	  (boring (article-hidden-text-p 'boring-headers))
	  (pgp (article-hidden-text-p 'pgp))
	  (pem (article-hidden-text-p 'pem))
	  (signature (article-hidden-text-p 'signature))
	  (overstrike (article-hidden-text-p 'overstrike))
	  (emphasis (article-hidden-text-p 'emphasis))
	  (mime gnus-show-mime))
      (format "%c%c%c%c%c%c%c"
	      (if cite ?c ? )
	      (if (or headers boring) ?h ? )
	      (if (or pgp pem) ?p ? )
	      (if signature ?s ? )
	      (if overstrike ?o ? )
	      (if mime ?m ? )
	      (if emphasis ?e ? )))))

(defun gnus-article-hide-headers-if-wanted ()
  "Hide unwanted headers if `gnus-have-all-headers' is nil.
Provided for backwards compatibility."
  (or (save-excursion (set-buffer gnus-summary-buffer) gnus-have-all-headers)
      gnus-inhibit-hiding
      (gnus-article-hide-headers)))

;;; Article savers.

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
  (let ((artbuf (current-buffer)))
    (nnheader-temp-write nil
      (insert-buffer-substring artbuf)
      ;; Append newline at end of the buffer as separator, and then
      ;; save it to file.
      (goto-char (point-max))
      (insert "\n")
      (append-to-file (point-min) (point-max) file-name))))

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
  "Narrow the article buffer to a page.
If given a numerical ARG, move forward ARG pages."
  (interactive "P")
  (setq arg (if arg (prefix-numeric-value arg) 0))
  (save-excursion
    (set-buffer gnus-article-buffer)
    (goto-char (point-min))
    (widen)
    ;; Remove any old next/prev buttons.
    (when (gnus-visual-p 'page-marker)
      (let ((buffer-read-only nil))
	(gnus-remove-text-with-property 'gnus-prev)
	(gnus-remove-text-with-property 'gnus-next)))
    (when
	(cond ((< arg 0)
	       (re-search-backward page-delimiter nil 'move (1+ (abs arg))))
	      ((> arg 0)
	       (re-search-forward page-delimiter nil 'move arg)))
      (goto-char (match-end 0)))
    (narrow-to-region
     (point)
     (if (re-search-forward page-delimiter nil 'move)
	 (match-beginning 0)
       (point)))
    (when (and (gnus-visual-p 'page-marker)
	       (not (= (point-min) 1)))
      (save-excursion
	(goto-char (point-min))
	(gnus-insert-prev-page-button)))
    (when (and (gnus-visual-p 'page-marker)
	       (< (+ (point-max) 2) (buffer-size)))
      (save-excursion
	(goto-char (point-max))
	(gnus-insert-next-page-button)))))

;; Article mode commands

(defun gnus-article-goto-next-page ()
  "Show the next page of the article."
  (interactive)
  (when (gnus-article-next-page)
    (gnus-article-read-summary-keys nil (gnus-character-to-event ?n))))

(defun gnus-article-goto-prev-page ()
  "Show the next page of the article."
  (interactive)
  (if (bobp) (gnus-article-read-summary-keys nil (gnus-character-to-event ?n))
    (gnus-article-prev-page nil)))

(defun gnus-article-next-page (&optional lines)
  "Show the next page of the current article.
If end of article, return non-nil.  Otherwise return nil.
Argument LINES specifies lines to be scrolled up."
  (interactive "p")
  (move-to-window-line -1)
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
    (move-to-window-line 0)
    nil))

(defun gnus-article-prev-page (&optional lines)
  "Show previous page of current article.
Argument LINES specifies lines to be scrolled down."
  (interactive "p")
  (move-to-window-line 0)
  (if (and gnus-break-pages
	   (bobp)
	   (not (save-restriction (widen) (bobp)))) ;Real beginning-of-buffer?
      (progn
	(gnus-narrow-to-page -1)	;Go to previous page.
	(goto-char (point-max))
	(recenter -1))
    (prog1
	(condition-case ()
	    (scroll-down lines)
	  (error nil))
      (move-to-window-line 0))))

(defun gnus-article-refer-article ()
  "Read article specified by message-id around point."
  (interactive)
  (let ((point (point)))
    (search-forward ">" nil t)		;Move point to end of "<....>".
    (if (re-search-backward "\\(<[^<> \t\n]+>\\)" nil t)
	(let ((message-id (match-string 1)))
	  (goto-char point)
	  (set-buffer gnus-summary-buffer)
	  (gnus-summary-refer-article message-id))
      (goto-char (point))
      (error "No references around point"))))

(defun gnus-article-show-summary ()
  "Reconfigure windows to show summary buffer."
  (interactive)
  (gnus-configure-windows 'article)
  (gnus-summary-goto-subject gnus-current-article))

(defun gnus-article-describe-briefly ()
  "Describe article mode commands briefly."
  (interactive)
  (gnus-message 6
		(substitute-command-keys "\\<gnus-article-mode-map>\\[gnus-article-goto-next-page]:Next page	 \\[gnus-article-goto-prev-page]:Prev page  \\[gnus-article-show-summary]:Show summary  \\[gnus-info-find-node]:Run Info  \\[gnus-article-describe-briefly]:This help")))

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

(defun gnus-article-read-summary-keys (&optional arg key not-restore-window)
  "Read a summary buffer key sequence and execute it from the article buffer."
  (interactive "P")
  (let ((nosaves
	 '("q" "Q"  "c" "r" "R" "\C-c\C-f" "m"	"a" "f" "F"
	   "Zc" "ZC" "ZE" "ZQ" "ZZ" "Zn" "ZR" "ZG" "ZN" "ZP"
	   "=" "^" "\M-^" "|"))
	(nosave-but-article
	 '("A\r"))
	(nosave-in-article
	 '("\C-d"))
	keys)
    (save-excursion
      (set-buffer gnus-summary-buffer)
      (push (or key last-command-event) unread-command-events)
      (setq keys (read-key-sequence nil)))
    (message "")

    (if (or (member keys nosaves)
	    (member keys nosave-but-article)
	    (member keys nosave-in-article))
	(let (func)
	  (save-window-excursion
	    (pop-to-buffer gnus-summary-buffer 'norecord)
	    (setq func (lookup-key (current-local-map) keys)))
	  (if (not func)
	      (ding)
	    (unless (member keys nosave-in-article)
	      (set-buffer gnus-summary-buffer))
	    (call-interactively func))
	  (when (member keys nosave-but-article)
	    (pop-to-buffer gnus-article-buffer 'norecord)))
      ;; These commands should restore window configuration.
      (let ((obuf (current-buffer))
	    (owin (current-window-configuration))
	    (opoint (point))
	    func in-buffer)
	(if not-restore-window
	    (pop-to-buffer gnus-summary-buffer 'norecord)
	  (switch-to-buffer gnus-summary-buffer 'norecord))
	(setq in-buffer (current-buffer))
	(if (setq func (lookup-key (current-local-map) keys))
	    (call-interactively func)
	  (ding))
	(when (eq in-buffer (current-buffer))
	  (set-buffer obuf)
	  (unless not-restore-window
	    (set-window-configuration owin))
	  (set-window-point (get-buffer-window (current-buffer)) opoint))))))

(defun gnus-article-hide (&optional arg force)
  "Hide all the gruft in the current article.
This means that PGP stuff, signatures, cited text and (some)
headers will be hidden.
If given a prefix, show the hidden text instead."
  (interactive (list current-prefix-arg 'force))
  (gnus-article-hide-headers arg)
  (gnus-article-hide-pgp arg)
  (gnus-article-hide-citation-maybe arg force)
  (gnus-article-hide-signature arg))

(defun gnus-article-maybe-highlight ()
  "Do some article highlighting if `article-visual' is non-nil."
  (if (gnus-visual-p 'article-highlight 'highlight)
      (gnus-article-highlight-some)))

(defun gnus-request-article-this-buffer (article group)
  "Get an article and insert it into this buffer."
  (let (do-update-line)
    (prog1
	(save-excursion
	  (erase-buffer)
	  (gnus-kill-all-overlays)
	  (setq group (or group gnus-newsgroup-name))

	  ;; Open server if it has closed.
	  (gnus-check-server (gnus-find-method-for-group group))

	  ;; Using `gnus-request-article' directly will insert the article into
	  ;; `nntp-server-buffer' - so we'll save some time by not having to
	  ;; copy it from the server buffer into the article buffer.

	  ;; We only request an article by message-id when we do not have the
	  ;; headers for it, so we'll have to get those.
	  (when (stringp article)
	    (let ((gnus-override-method gnus-refer-article-method))
	      (gnus-read-header article)))

	  ;; If the article number is negative, that means that this article
	  ;; doesn't belong in this newsgroup (possibly), so we find its
	  ;; message-id and request it by id instead of number.
	  (when (and (numberp article)
		     gnus-summary-buffer
		     (get-buffer gnus-summary-buffer)
		     (buffer-name (get-buffer gnus-summary-buffer)))
	    (save-excursion
	      (set-buffer gnus-summary-buffer)
	      (let ((header (gnus-summary-article-header article)))
		(if (< article 0)
		    (cond 
		     ((memq article gnus-newsgroup-sparse)
		      ;; This is a sparse gap article.
		      (setq do-update-line article)
		      (setq article (mail-header-id header))
		      (let ((gnus-override-method gnus-refer-article-method))
			(gnus-read-header article))
		      (setq gnus-newsgroup-sparse
			    (delq article gnus-newsgroup-sparse)))
		     ((vectorp header)
		      ;; It's a real article.
		      (setq article (mail-header-id header)))
		     (t
		      ;; It is an extracted pseudo-article.
		      (setq article 'pseudo)
		      (gnus-request-pseudo-article header))))
		
		(let ((method (gnus-find-method-for-group 
			       gnus-newsgroup-name)))
		  (if (not (eq (car method) 'nneething))
		      ()
		    (let ((dir (concat (file-name-as-directory (nth 1 method))
				       (mail-header-subject header))))
		      (if (file-directory-p dir)
			  (progn
			    (setq article 'nneething)
			    (gnus-group-enter-directory dir)))))))))

	  (cond
	   ;; Refuse to select canceled articles.
	   ((and (numberp article)
		 gnus-summary-buffer
		 (get-buffer gnus-summary-buffer)
		 (buffer-name (get-buffer gnus-summary-buffer))
		 (eq (cdr (save-excursion
			    (set-buffer gnus-summary-buffer)
			    (assq article gnus-newsgroup-reads)))
		     gnus-canceled-mark))
	    nil)
	   ;; We first check `gnus-original-article-buffer'.
	   ((and (get-buffer gnus-original-article-buffer)
		 (numberp article)
		 (save-excursion
		   (set-buffer gnus-original-article-buffer)
		   (and (equal (car gnus-original-article) group)
			(eq (cdr gnus-original-article) article))))
	    (insert-buffer-substring gnus-original-article-buffer)
	    'article)
	   ;; Check the backlog.
	   ((and gnus-keep-backlog
		 (gnus-backlog-request-article group article (current-buffer)))
	    'article)
	   ;; Check asynchronous pre-fetch.
	   ((gnus-async-request-fetched-article group article (current-buffer))
	    (gnus-async-prefetch-next group article gnus-summary-buffer)
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
	      (gnus-kill-all-overlays)
	      (when (gnus-request-article article group (current-buffer))
		(when (numberp article)
		  (gnus-async-prefetch-next group article gnus-summary-buffer)
		  (when gnus-keep-backlog
		    (gnus-backlog-enter-article 
		     group article (current-buffer))))
		'article)))
	   ;; It was a pseudo.
	   (t article)))

      ;; Take the article from the original article buffer
      ;; and place it in the buffer it's supposed to be in.
      (when (and (get-buffer gnus-article-buffer)
		 ;;(numberp article)
		 (equal (buffer-name (current-buffer))
			(buffer-name (get-buffer gnus-article-buffer))))
	(save-excursion
	  (if (get-buffer gnus-original-article-buffer)
	      (set-buffer (get-buffer gnus-original-article-buffer))
	    (set-buffer (get-buffer-create gnus-original-article-buffer))
	    (buffer-disable-undo (current-buffer))
	    (setq major-mode 'gnus-original-article-mode)
	    (setq buffer-read-only t)
	    (gnus-add-current-to-buffer-list))
	  (let (buffer-read-only)
	    (erase-buffer)
	    (insert-buffer-substring gnus-article-buffer))
	  (setq gnus-original-article (cons group article))))
    
      ;; Update sparse articles.
      (when (and do-update-line
		 (or (numberp article)
		     (stringp article)))
	(let ((buf (current-buffer)))
	  (set-buffer gnus-summary-buffer)
	  (gnus-summary-update-article do-update-line)
	  (gnus-summary-goto-subject do-update-line nil t)
	  (set-window-point (get-buffer-window (current-buffer) t)
			    (point))
	  (set-buffer buf))))))

(defun gnus-article-date-ut (&optional type highlight)
  "Convert DATE date to universal time in the current article.
If TYPE is `local', convert to local time; if it is `lapsed', output
how much time has lapsed since DATE."
  (interactive (list 'ut t))
  (let ((headers (or gnus-current-headers (gnus-summary-article-header))))
    (save-excursion
      (set-buffer gnus-article-buffer)
      (article-date-ut type highlight headers))))

;;;
;;; Article editing
;;;

(defcustom gnus-article-edit-mode-hook nil
  "Hook run in article edit mode buffers."
  :group 'article
  :type 'hook)

(defvar gnus-article-edit-done-function nil)

(defvar gnus-article-edit-mode-map nil)

(unless gnus-article-edit-mode-map 
  (setq gnus-article-edit-mode-map (copy-keymap text-mode-map))

  (gnus-define-keys gnus-article-edit-mode-map
    "\C-c\C-c" gnus-article-edit-done
    "\C-c\C-k" gnus-article-edit-exit)

  (gnus-define-keys (gnus-article-edit-wash-map
		     "\C-c\C-w" gnus-article-edit-mode-map)
    "f" gnus-article-edit-full-stops))

(defun gnus-article-edit-mode ()
  "Major mode for editing articles.
This is an extended text-mode.

\\{gnus-article-edit-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'gnus-article-edit-mode)
  (setq mode-name "Article Edit")
  (use-local-map gnus-article-edit-mode-map)
  (make-local-variable 'gnus-article-edit-done-function)
  (make-local-variable 'gnus-prev-winconf)
  (setq buffer-read-only nil)
  (buffer-enable-undo)
  (widen)
  (run-hooks 'text-mode 'gnus-article-edit-mode-hook))

(defun gnus-article-edit (&optional force)
  "Edit the current article.
This will have permanent effect only in mail groups.
If FORCE is non-nil, allow editing of articles even in read-only
groups."
  (interactive "P")
  (when (and (not force)
	     (gnus-group-read-only-p))
    (error "The current newsgroup does not support article editing."))
  (gnus-article-edit-article
   `(lambda ()
      (gnus-summary-edit-article-done
       ,(or (mail-header-references gnus-current-headers) "")
       ,(gnus-group-read-only-p) ,gnus-summary-buffer))))

(defun gnus-article-edit-article (exit-func)
  "Start editing the contents of the current article buffer."
  (let ((winconf (current-window-configuration)))
    (set-buffer gnus-article-buffer)
    (gnus-article-edit-mode)
    (set-text-properties (point-min) (point-max) nil)
    (gnus-configure-windows 'edit-article)
    (setq gnus-article-edit-done-function exit-func)
    (setq gnus-prev-winconf winconf)
    (gnus-message 6 "C-c C-c to end edits")))

(defun gnus-article-edit-done ()
  "Update the article edits and exit."
  (interactive)
  (let ((func gnus-article-edit-done-function)
	(buf (current-buffer))
	(start (window-start)))
    (gnus-article-edit-exit)
    (save-excursion
      (set-buffer buf)
      (let ((buffer-read-only nil))
	(funcall func)))
    (set-buffer buf)
    (set-window-start (get-buffer-window buf) start)
    (set-window-point (get-buffer-window buf) (point))))

(defun gnus-article-edit-exit ()
  "Exit the article editing without updating."
  (interactive)
  ;; We remove all text props from the article buffer.
  (let ((buf (format "%s" (buffer-string)))
	(curbuf (current-buffer))
	(p (point))
	(window-start (window-start)))
    (erase-buffer)
    (insert buf)
    (let ((winconf gnus-prev-winconf))
      (gnus-article-mode)
      ;; The cache and backlog have to be flushed somewhat.
      (when gnus-use-cache
	(gnus-cache-update-article 	
	 (car gnus-article-current) (cdr gnus-article-current)))
      (when gnus-keep-backlog
	(gnus-backlog-remove-article 
	 (car gnus-article-current) (cdr gnus-article-current)))
      ;; Flush original article as well.
      (save-excursion
	(when (get-buffer gnus-original-article-buffer)
	  (set-buffer gnus-original-article-buffer)
	  (setq gnus-original-article nil)))
      (set-window-configuration winconf)
      ;; Tippy-toe some to make sure that point remains where it was.
      (let ((buf (current-buffer)))
	(set-buffer curbuf)
	(set-window-start (get-buffer-window (current-buffer)) window-start)
	(goto-char p)
	(set-buffer buf)))))
      
(defun gnus-article-edit-full-stops ()
  "Interactively repair spacing at end of sentences."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (search-forward-regexp "^$" nil t)
    (let ((case-fold-search nil))
      (query-replace-regexp "\\([.!?][])}]* \\)\\([[({A-Z]\\)" "\\1 \\2"))))

;;; 
;;; Article highlights
;;;

;; Written by Per Abrahamsen <abraham@iesd.auc.dk>.

;;; Internal Variables:

(defcustom gnus-button-url-regexp "\\b\\(s?https?\\|ftp\\|file\\|gopher\\|news\\|telnet\\|wais\\|mailto\\):\\(//[-a-zA-Z0-9_.]+:[0-9]*\\)?[-\\wa-zA-Z0-9_=!?#$@~`%&*+|\\/:;.,]*[-\\wa-zA-Z0-9_=#$@~`%&*+|\\/]"
  "Regular expression that matches URLs."
  :group 'article
  :type 'regexp)

(defcustom gnus-button-alist 
  `(("\\bin\\( +article\\)? +\\(<\\([^\n @<>]+@[^\n @<>]+\\)>\\)" 2 
     t gnus-button-message-id 3)
    ("\\(<?\\(url: ?\\)?news://\\([^>\n\t ]*\\)>?\\)" 1 t
     gnus-button-fetch-group 3)
    ("\\(<?\\(url: ?\\)?news:\\([^>\n\t ]*\\)>?\\)" 1 t
     gnus-button-message-id 3)
    ("\\(<URL: *\\)?mailto: *\\([^> \n\t]+\\)>?" 0 t gnus-button-reply 2)
    ;; This is how URLs _should_ be embedded in text...
    ("<URL: *\\([^\n\r>]*\\)>" 0 t gnus-button-url 1)
    ;; Next regexp stolen from highlight-headers.el.
    ;; Modified by Vladimir Alexiev.
    (,gnus-button-url-regexp 0 t gnus-button-url 0))
  "Alist of regexps matching buttons in article bodies.

Each entry has the form (REGEXP BUTTON FORM CALLBACK PAR...), where
REGEXP: is the string matching text around the button,
BUTTON: is the number of the regexp grouping actually matching the button,
FORM: is a lisp expression which must eval to true for the button to
be added, 
CALLBACK: is the function to call when the user push this button, and each
PAR: is a number of a regexp grouping whose text will be passed to CALLBACK.

CALLBACK can also be a variable, in that case the value of that
variable it the real callback function."
  :group 'article
  :type '(repeat (list regexp 
		       (integer :tag "Button")
		       (sexp :tag "Form")
		       (function :tag "Callback")
		       (repeat :tag "Par"
			       :inline t
			       (integer :tag "Regexp group")))))

(defcustom gnus-header-button-alist 
  `(("^\\(References\\|Message-I[Dd]\\):" "<[^>]+>"
     0 t gnus-button-message-id 0)
    ("^\\(From\\|Reply-To\\):" ": *\\(.+\\)$" 1 t gnus-button-reply 1)
    ("^\\(Cc\\|To\\):" "[^ \t\n<>,()\"]+@[^ \t\n<>,()\"]+" 
     0 t gnus-button-mailto 0)
    ("^X-[Uu][Rr][Ll]:" ,gnus-button-url-regexp 0 t gnus-button-url 0)
    ("^[^:]+:" ,gnus-button-url-regexp 0 t gnus-button-url 0)
    ("^[^:]+:" "\\(<\\(url: \\)?news:\\([^>\n ]*\\)>\\)" 1 t
     gnus-button-message-id 3))
  "Alist of headers and regexps to match buttons in article heads.

This alist is very similar to `gnus-button-alist', except that each
alist has an additional HEADER element first in each entry:

\(HEADER REGEXP BUTTON FORM CALLBACK PAR)

HEADER is a regexp to match a header.  For a fuller explanation, see
`gnus-button-alist'."
  :group 'article
  :type '(repeat (list (regexp :tag "Header")
		       regexp 
		       (integer :tag "Button")
		       (sexp :tag "Form")
		       (function :tag "Callback")
		       (repeat :tag "Par"
			       :inline t
			       (integer :tag "Regexp group")))))

(defvar gnus-button-regexp nil)
(defvar gnus-button-marker-list nil)
;; Regexp matching any of the regexps from `gnus-button-alist'.

(defvar gnus-button-last nil)
;; The value of `gnus-button-alist' when `gnus-button-regexp' was build.

;;; Commands:

(defun gnus-article-push-button (event)
  "Check text under the mouse pointer for a callback function.
If the text under the mouse pointer has a `gnus-callback' property,
call it with the value of the `gnus-data' text property."
  (interactive "e")
  (set-buffer (window-buffer (posn-window (event-start event))))
  (let* ((pos (posn-point (event-start event)))
         (data (get-text-property pos 'gnus-data))
	 (fun (get-text-property pos 'gnus-callback)))
    (if fun (funcall fun data))))

(defun gnus-article-press-button ()
  "Check text at point for a callback function.
If the text at point has a `gnus-callback' property,
call it with the value of the `gnus-data' text property."
  (interactive)
  (let* ((data (get-text-property (point) 'gnus-data))
	 (fun (get-text-property (point) 'gnus-callback)))
    (if fun (funcall fun data))))

(defun gnus-article-prev-button (n)
  "Move point to N buttons backward.
If N is negative, move forward instead."
  (interactive "p")
  (gnus-article-next-button (- n)))

(defun gnus-article-next-button (n)
  "Move point to N buttons forward.
If N is negative, move backward instead."
  (interactive "p")
  (let ((function (if (< n 0) 'previous-single-property-change
		    'next-single-property-change))
	(inhibit-point-motion-hooks t)
	(backward (< n 0))
	(limit (if (< n 0) (point-min) (point-max))))
    (setq n (abs n))
    (while (and (not (= limit (point)))
		(> n 0))
      ;; Skip past the current button.
      (when (get-text-property (point) 'gnus-callback)
	(goto-char (funcall function (point) 'gnus-callback nil limit)))
      ;; Go to the next (or previous) button.
      (gnus-goto-char (funcall function (point) 'gnus-callback nil limit))
      ;; Put point at the start of the button.
      (when (and backward (not (get-text-property (point) 'gnus-callback)))
	(goto-char (funcall function (point) 'gnus-callback nil limit)))
      ;; Skip past intangible buttons.
      (when (get-text-property (point) 'intangible)
	(incf n))
      (decf n))
    (unless (zerop n)
      (gnus-message 5 "No more buttons"))
    n))

(defun gnus-article-highlight (&optional force)
  "Highlight current article.
This function calls `gnus-article-highlight-headers',
`gnus-article-highlight-citation', 
`gnus-article-highlight-signature', and `gnus-article-add-buttons' to
do the highlighting.  See the documentation for those functions."
  (interactive (list 'force))
  (gnus-article-highlight-headers)
  (gnus-article-highlight-citation force)
  (gnus-article-highlight-signature)
  (gnus-article-add-buttons force)
  (gnus-article-add-buttons-to-head))

(defun gnus-article-highlight-some (&optional force)
  "Highlight current article.
This function calls `gnus-article-highlight-headers',
`gnus-article-highlight-signature', and `gnus-article-add-buttons' to
do the highlighting.  See the documentation for those functions."
  (interactive (list 'force))
  (gnus-article-highlight-headers)
  (gnus-article-highlight-signature)
  (gnus-article-add-buttons))

(defun gnus-article-highlight-headers ()
  "Highlight article headers as specified by `gnus-header-face-alist'."
  (interactive)
  (save-excursion
    (set-buffer gnus-article-buffer)
    (save-restriction
      (let ((alist gnus-header-face-alist)
	    (buffer-read-only nil)
	    (case-fold-search t)
	    (inhibit-point-motion-hooks t)
	    entry regexp header-face field-face from hpoints fpoints)
	(goto-char (point-min))
	(when (search-forward "\n\n" nil t)
	  (narrow-to-region (1- (point)) (point-min))
	  (while (setq entry (pop alist))
	    (goto-char (point-min))
	    (setq regexp (concat "^\\("
				 (if (string-equal "" (nth 0 entry))
				     "[^\t ]"
				   (nth 0 entry))
				 "\\)")
		  header-face (nth 1 entry)
		  field-face (nth 2 entry))
	    (while (and (re-search-forward regexp nil t)
			(not (eobp)))
	      (beginning-of-line)
	      (setq from (point))
	      (or (search-forward ":" nil t)
		  (forward-char 1))
	      (when (and header-face
			 (not (memq (point) hpoints)))
		(push (point) hpoints)
		(gnus-put-text-property from (point) 'face header-face))
	      (when (and field-face
			 (not (memq (setq from (point)) fpoints)))
		(push from fpoints)
		(if (re-search-forward "^[^ \t]" nil t)
		    (forward-char -2)
		  (goto-char (point-max)))
		(gnus-put-text-property from (point) 'face field-face)))))))))

(defun gnus-article-highlight-signature ()
  "Highlight the signature in an article.
It does this by highlighting everything after
`gnus-signature-separator' using `gnus-signature-face'." 
  (interactive)
  (save-excursion
    (set-buffer gnus-article-buffer)
    (let ((buffer-read-only nil)
	  (inhibit-point-motion-hooks t))
      (save-restriction
	(when (and gnus-signature-face
		   (article-narrow-to-signature))
	  (gnus-overlay-put (gnus-make-overlay (point-min) (point-max))
			    'face gnus-signature-face)
	  (widen)
	  (article-search-signature)
	  (let ((start (match-beginning 0))
		(end (set-marker (make-marker) (1+ (match-end 0)))))
	    (gnus-article-add-button start (1- end) 'gnus-signature-toggle
				     end)))))))

(defun gnus-article-add-buttons (&optional force)
  "Find external references in the article and make buttons of them.
\"External references\" are things like Message-IDs and URLs, as
specified by `gnus-button-alist'."
  (interactive (list 'force))
  (save-excursion
    (set-buffer gnus-article-buffer)
    ;; Remove all old markers.
    (while gnus-button-marker-list
      (set-marker (pop gnus-button-marker-list) nil))
    (let ((buffer-read-only nil)
	  (inhibit-point-motion-hooks t)
	  (case-fold-search t)
	  (alist gnus-button-alist)
	  beg entry regexp)
      (goto-char (point-min))
      ;; We skip the headers.
      (unless (search-forward "\n\n" nil t)
	(goto-char (point-max)))
      (setq beg (point))
      (while (setq entry (pop alist))
	(setq regexp (car entry))
	(goto-char beg)
	(while (re-search-forward regexp nil t)
	  (let* ((start (and entry (match-beginning (nth 1 entry))))
		 (end (and entry (match-end (nth 1 entry))))
		 (from (match-beginning 0)))
	    (when (or (eq t (nth 1 entry))
		      (eval (nth 1 entry)))
	      ;; That optional form returned non-nil, so we add the
	      ;; button. 
	      (gnus-article-add-button 
	       start end 'gnus-button-push 
	       (car (push (set-marker (make-marker) from)
			  gnus-button-marker-list))))))))))

;; Add buttons to the head of an article.
(defun gnus-article-add-buttons-to-head ()
  "Add buttons to the head of the article."
  (interactive)
  (save-excursion
    (set-buffer gnus-article-buffer)
    (let ((buffer-read-only nil)
	  (inhibit-point-motion-hooks t)
	  (case-fold-search t)
	  (alist gnus-header-button-alist)
	  entry beg end)
      (nnheader-narrow-to-headers)
      (while alist
	;; Each alist entry.
	(setq entry (car alist)
	      alist (cdr alist))
	(goto-char (point-min))
	(while (re-search-forward (car entry) nil t)
	  ;; Each header matching the entry.
	  (setq beg (match-beginning 0))
	  (setq end (or (and (re-search-forward "^[^ \t]" nil t)
			     (match-beginning 0))
			(point-max)))
	  (goto-char beg)
	  (while (re-search-forward (nth 1 entry) end t)
	    ;; Each match within a header.
	    (let* ((entry (cdr entry))
		   (start (match-beginning (nth 1 entry)))
		   (end (match-end (nth 1 entry)))
		   (form (nth 2 entry)))
	      (goto-char (match-end 0))
	      (and (eval form)
		   (gnus-article-add-button 
		    start end (nth 3 entry)
		    (buffer-substring (match-beginning (nth 4 entry))
				      (match-end (nth 4 entry)))))))
	  (goto-char end))))
    (widen)))

;;; External functions:

(defun gnus-article-add-button (from to fun &optional data)
  "Create a button between FROM and TO with callback FUN and data DATA."
  (and gnus-article-button-face
       (gnus-overlay-put (gnus-make-overlay from to)
			 'face gnus-article-button-face))
  (gnus-add-text-properties 
   from to
   (nconc (and gnus-article-mouse-face
	       (list gnus-mouse-face-prop gnus-article-mouse-face))
	  (list 'gnus-callback fun)
	  (and data (list 'gnus-data data)))))

;;; Internal functions:

(defun gnus-signature-toggle (end)
  (save-excursion
    (set-buffer gnus-article-buffer)
    (let ((buffer-read-only nil)
	  (inhibit-point-motion-hooks t))
      (if (get-text-property end 'invisible)
	  (article-unhide-text end (point-max))
	(article-hide-text end (point-max) gnus-hidden-properties)))))

(defun gnus-button-entry ()
  ;; Return the first entry in `gnus-button-alist' matching this place.
  (let ((alist gnus-button-alist)
	(entry nil))
    (while alist
      (setq entry (pop alist))
      (if (looking-at (car entry))
	  (setq alist nil)
	(setq entry nil)))
    entry))

(defun gnus-button-push (marker)
  ;; Push button starting at MARKER.
  (save-excursion
    (set-buffer gnus-article-buffer)
    (goto-char marker)
    (let* ((entry (gnus-button-entry))
	   (inhibit-point-motion-hooks t)
	   (fun (nth 3 entry))
	   (args (mapcar (lambda (group) 
			   (let ((string (buffer-substring
					  (match-beginning group)
					  (match-end group))))
			     (gnus-set-text-properties
			      0 (length string) nil string)
			     string))
			 (nthcdr 4 entry))))
      (cond
       ((fboundp fun)
	(apply fun args))
       ((and (boundp fun)
	     (fboundp (symbol-value fun)))
	(apply (symbol-value fun) args))
       (t
	(gnus-message 1 "You must define `%S' to use this button"
		      (cons fun args)))))))

(defun gnus-button-message-id (message-id)
  "Fetch MESSAGE-ID."
  (save-excursion
    (set-buffer gnus-summary-buffer)
    (gnus-summary-refer-article message-id)))

(defun gnus-button-fetch-group (address)
  "Fetch GROUP specified by ADDRESS."
  (if (not (string-match "^\\([^:/]+\\)\\(:\\([^/]+\\)\\)?/\\(.*\\)$" address))
      (error "Can't parse %s" address)
    (gnus-group-read-ephemeral-group
     (match-string 4 address)
     `(nntp ,(match-string 1 address) (nntp-address ,(match-string 1 address))
	    (nntp-port-number ,(if (match-end 3)
				   (match-string 3 address)
				 "nntp"))))))

(defun gnus-button-mailto (address)
  ;; Mail to ADDRESS.
  (set-buffer (gnus-copy-article-buffer))
  (message-reply address))

(defun gnus-button-reply (address)
  ;; Reply to ADDRESS.
  (message-reply address))

(defun gnus-button-url (address)
  "Browse ADDRESS."
  (funcall browse-url-browser-function address))

;;; Next/prev buttons in the article buffer.

(defvar gnus-next-page-line-format "%{%(Next page...%)%}\n")
(defvar gnus-prev-page-line-format "%{%(Previous page...%)%}\n")

(defvar gnus-prev-page-map nil)
(unless gnus-prev-page-map
  (setq gnus-prev-page-map (make-sparse-keymap))
  (define-key gnus-prev-page-map gnus-mouse-2 'gnus-button-prev-page)
  (define-key gnus-prev-page-map "\r" 'gnus-button-prev-page))

(defun gnus-insert-prev-page-button ()
  (let ((buffer-read-only nil))
    (gnus-eval-format 
     gnus-prev-page-line-format nil
     `(gnus-prev t local-map ,gnus-prev-page-map
		 gnus-callback gnus-article-button-prev-page))))

(defvar gnus-next-page-map nil)
(unless gnus-next-page-map
  (setq gnus-next-page-map (make-keymap))
  (suppress-keymap gnus-prev-page-map)
  (define-key gnus-next-page-map gnus-mouse-2 'gnus-button-next-page)
  (define-key gnus-next-page-map "\r" 'gnus-button-next-page))

(defun gnus-button-next-page ()
  "Go to the next page."
  (interactive)
  (let ((win (selected-window)))
    (select-window (get-buffer-window gnus-article-buffer t))
    (gnus-article-next-page)
    (select-window win)))

(defun gnus-button-prev-page ()
  "Go to the prev page."
  (interactive)
  (let ((win (selected-window)))
    (select-window (get-buffer-window gnus-article-buffer t))
    (gnus-article-prev-page)
    (select-window win)))

(defun gnus-insert-next-page-button ()
  (let ((buffer-read-only nil))
    (gnus-eval-format gnus-next-page-line-format nil
		      `(gnus-next t local-map ,gnus-next-page-map
				  gnus-callback 
				  gnus-article-button-next-page))))

(defun gnus-article-button-next-page (arg)
  "Go to the next page."
  (interactive "P")
  (let ((win (selected-window)))
    (select-window (get-buffer-window gnus-article-buffer t))
    (gnus-article-next-page)
    (select-window win)))

(defun gnus-article-button-prev-page (arg)
  "Go to the prev page."
  (interactive "P")
  (let ((win (selected-window)))
    (select-window (get-buffer-window gnus-article-buffer t))
    (gnus-article-prev-page)
    (select-window win)))

(provide 'gnus-art)

;;; gnus-art.el ends here
