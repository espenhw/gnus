;;; browse-url.el --- ask a WWW browser to load a URL

;; Copyright 1995 Free Software Foundation, Inc.

;; Author: Denis Howe <dbh@doc.ic.ac.uk>
;; Maintainer: Denis Howe <dbh@doc.ic.ac.uk>
;; Created: 03 Apr 1995
;; Version: 0.16 17 May 1995
;; Keywords: hypertext
;; X-Home page: http://wombat.doc.ic.ac.uk/

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 1, or (at your
;; option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Commentary:

;; The latest version of this package should be available from
;; <URL:http://wombat.doc.ic.ac.uk/emacs/browse-url.el>.

;; This package provides functions which read a URL (Uniform Resource
;; Locator) from the minibuffer, defaulting to the URL around point,
;; and ask a World-Wide Web browser to load it.  It can also load the
;; URL associated with the current buffer.  Different browsers use
;; different methods of remote control so there is one function for
;; each supported browser.  If the chosen browser is not running, it
;; is started.  Currently there is support for:

;; Function             Browser     Earliest version
;; browse-url-netscape  Netscape    1.1b1	   
;; browse-url-mosaic    XMosaic     <= 2.4
;; browse-url-w3        w3          0
;; browse-url-iximosaic IXI Mosaic  ?

;; Note that earlier versions of Netscape did not have remote control.
;; <URL:http://home.netscape.com/newsref/std/x-remote.html> and
;; <URL:http://home.netscape.com/info/APIs/>.

;; If using Mosaic, check the definition of browse-url-usr1-signal
;; below.
;; <URL:http://www.ncsa.uiuc.edu/SDG/Software/XMosaic/remote-control.html>

;; William M. Perry's excellent "w3" WWW browser for
;; Emacs <URL:ftp://cs.indiana.edu/pub/elisp/w3/>
;; has a function w3-follow-url-at-point, but that
;; doesn't let you edit the URL like browse-url.

;; I recommend Nelson Minar <nelson@santafe.edu>'s excellent
;; html-helper-mode.el for editing HTML and thank Nelson for
;; his many useful comments on this code.
;; <URL:http://www.santafe.edu/~nelson/hhm-beta/>

;; This package generalises function html-previewer-process in Marc
;; Andreessen <marca@ncsa.uiuc.edu>'s html-mode (LCD
;; modes/html-mode.el.Z) and provides better versions of the URL
;; functions in Michelangelo Grigni <mic@cs.ucsd.edu>'s ffap.el
;; (find-file-at-point) <URL:ftp://cs.ucsd.edu:/pub/mic/>.  The huge
;; hyperbole package also contains similar functions.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Help!

;; Can you write and test some code for the Macintrash and Windoze
;; Netscape remote control APIs?  (See the URL above).

;; Did earlier versions of Mosaic have remote control?

;; Do any other browsers have remote control?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Installation

;; In your ~/.emacs, put:
;;
;;	(setq browse-url-browser-function
;;	      (if (eq window-system 'x)
;;		  'browse-url-netscape ; or browse-url-mosaic
;;		'browse-url-w3))
;;	(autoload browse-url-browser-function "browse-url.el"
;;	  "Ask a WWW browser to show a URL." t)
;; Bind this to control-X w (normally undefined):
;;	(global-set-key "\C-xw" browse-url-browser-function)
;;	;; Note: no quote       ^

;; For viewing current buffer:
;;
;;	(autoload 'browse-url-of-file "browse-url.el"
;;	  "Ask a WWW browser to display the current file." t)
;;	(setq browse-url-save-file t)	; Always save

;; To get round the Netscape caching problem, you could try either of
;; the following (but not both).  EITHER write-file in
;; html-helper-mode makes Netscape reload document:
;;
;;	(autoload 'browse-url-netscape-reload "browse-url.el"
;;	  "Ask a WWW browser to redisplay the current file." t)
;;	(add-hook 'html-helper-mode-hook
;;		  (function (lambda ()
;;		     (add-hook 'local-write-file-hooks
;;			       (function (lambda ()
;;				  (let ((local-write-file-hooks))
;;				    (save-buffer))
;;				  (browse-url-netscape-reload)
;;				  t))			; => file written by hook
;;			       t))))			; append to l-w-f-hooks
;;
;; [Does this work for html-mode too?]

;; OR browse-url-of-file ask Netscape to load and then reload the
;; file:
;;
;;	(add-hook 'browse-url-of-file-hook 'browse-url-netscape-reload)

;; You may also want to customise browse-url-netscape-arguments, eg.
;;
;;	(setq browse-url-netscape-arguments '("-install"))
;;
;; or similarly for the other browsers. 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Change Log:

;; 0.00 03 Apr 1995 Denis Howe <dbh@doc.ic.ac.uk>
;;	Created.

;; 0.01 04 Apr 1995
;;	All names start with "browse-url-".  Added provide.

;; 0.02 05 Apr 1995
;;	Save file at start of browse-url-of-file.
;;	Use start-process instead of start-process-shell-command.

;; 0.03 06 Apr 1995
;;	Add browse-url-netscape-reload, browse-url-netscape-command.
;;	browse-url-of-file save file option.

;; 0.04 08 Apr 1995
;;	b-u-file-url separate function.  Change b-u-filename-alist
;;	default.

;; 0.05 09 Apr 1995
;;	Added b-u-of-file-hook.

;; 0.06 11 Apr 1995
;;	Improved .emacs suggestions and documentation.

;; 0.07 13 Apr 1995
;;	Added browse-url-interactive-arg optional prompt.

;; 0.08 18 Apr 1995
;;	Exclude final "." from browse-url-regexp.

;; 0.09 21 Apr 1995
;;	Added mouse-set-point to browse-url-interactive-arg.

;; 0.10 24 Apr 1995
;;	Added Mosaic signal sending variations.
;;	Thanks Brian K Servis <servis@ecn.purdue.edu>.
;;	Don't use xprop for Netscape.

;; 0.11 25 Apr 1995
;;	Fix reading of ~/.mosaicpid.  Thanks Dag.H.Wanvik@kvatro.no.

;; 0.12 27 Apr 1995
;;	Interactive prefix arg => URL *after* point.
;;	Thanks Michelangelo Grigni <mic@cs.ucsd.edu>.
;;	Added IXI Mosaic support.
;;	Thanks David Karr <dkarr@nmo.gtegsc.com>.

;; 0.13 28 Apr 1995
;;	Exclude final [,;] from browse-url-regexp.

;; 0.14 02 May 1995
;;	Provide browser argument variables.

;; 0.15 07 May 1995
;;	More Netscape options.  Thanks Peter Arius
;;	<arius@immd2.informatik.uni-erlangen.de>.

;; 0.16 17 May 1995
;;	Added browse-url-at-mouse.
;;	Thanks Wayne Mesard <wmesard@sgi.com>

;; 0.17 27 Jun 1995
;;	Renamed browse-url-at-point to browse-url-url-at-point.
;;	Added browse-url-at-point.
;;	Thanks Jonathan Cano <cano@patch.tandem.com>.

;; Netscape can cache Web pages so it may be necessary to tell it to
;; reload the current page if it has changed (eg. if you have edited
;; it).  There is currently no perfect automatic solution to this.

;; Netscape allows you to specify the id of the window you want to
;; control but which window DO you want to control and how do you
;; discover its id?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

(defvar browse-url-regexp
  "\\(https?://\\|ftp://\\|gopher://\\|telnet://\\|wais://\\|file:/\\|s?news:\\|mailto:\\)[^]\t\n \"'()<>[^`{}]*[^]\t\n \"'()<>[^`{}.,;]+"
  "A regular expression probably matching a URL.")

(defvar browse-url-browser-function
  'browse-url-netscape
  "*Function to display the current buffer in a WWW browser.
Used by function `browse-url-of-file'.")

(defvar browse-url-netscape-arguments nil
  "*A list of strings to pass to Netscape as arguments.")

(defvar browse-url-netscape-new-window-p nil
  "*If non-nil, Netscape always opens a new window.
Passing an interactive argument to \\[browse-url-netscape] reverses
the effect of this variable.  Requires Netscape version 1.1N or
later.")

(defvar browse-url-mosaic-arguments nil
  "*A list of strings to pass to Mosaic as arguments.")

(defvar browse-url-filename-alist
  '(("^/+" . "file:/"))
  "An alist of (REGEXP . STRING) pairs.
Any substring of a filename matching one of the REGEXPs is replaced by
the corresponding STRING.  All pairs are applied in the order given.
Used by function `browse-url-of-file'.")

(defvar browse-url-save-file nil
  "If non-nil, save the buffer before displaying its file.
Used by function `browse-url-of-file'.")

(defvar browse-url-of-file-hook nil
  "A hook to be run with run-hook after `browse-url-of-file' has asked
a browser to load a file.

Set this to `browse-url-netscape-reload' to force Netscape to load the
file rather than displaying a cached copy.")

(defvar browse-url-usr1-signal
  (if (and (boundp 'emacs-major-version)
	   (or (> emacs-major-version 19) (>= emacs-minor-version 29)))
      'sigusr1
    30)					; Check /usr/include/signal.h.
  "The argument to `signal-process' for sending SIGUSR1 to XMosaic.
Emacs 19.29 accepts 'sigusr1, earlier versions require an integer
which is 30 on SunOS and 16 on HP-UX and Solaris.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; URL input

;; thingatpt.el doesn't work for complex regexps.

(defun browse-url-url-at-point ()
  "Return the URL around or before point.
Then search backwards for the start of a URL.  If no URL found, return
the empty string."
  (if (or (looking-at browse-url-regexp)	; Already at start
	  (let ((eol (save-excursion (end-of-line) (point))))
	    ;; Search forwards for the next URL or end of line in case
	    ;; we're in the middle of one.
	    (and (re-search-forward browse-url-regexp eol 'lim)
		 (goto-char (match-beginning 0)))
	    ;; Now back to where we started or earlier.
	    (re-search-backward browse-url-regexp nil t)))
      (buffer-substring (match-beginning 0) (match-end 0))
    ""))				; No match

;; Todo: restrict to around or immediately before point.  Expand bare
;; hostname to URL.

(defun browse-url-interactive-arg (&optional prompt)
  "Read a URL from the minibuffer, optionally prompting with PROMPT.
Default to the URL at or before point.  If bound to a mouse button,
set point to the position clicked.  Return the result as a list for
use in `interactive'."
  (let ((event (elt (this-command-keys) 0)))
    (and (listp event) (mouse-set-point event)))
  (list (read-string (or prompt "URL: ") (browse-url-url-at-point))))

(defun browse-url-at-point ()
  "Pass the URL at or before point to a WWW browser."
  (interactive)
  (funcall browse-url-browser-function (browse-url-at-point)))

(defun browse-url-at-mouse (event)
  "Ask a browser to load a URL clicked with the mouse.
The URL is the one around or before the position of the mouse click
but point is not changed.  The URL is loaded using variable
`browse-url-browser-function'."
  (interactive "e")
  (save-excursion
    (let ((posn (event-start event)))
      (set-buffer (window-buffer (posn-window posn)))
      (goto-char (posn-point posn))
      (let ((url (browse-url-url-at-point)))
	(if (string-equal url "")
	    (error "No URL found"))
	(funcall browse-url-browser-function url)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Browse current buffer

(defun browse-url-of-file (&optional file)
  "Ask a WWW browser to display FILE.
Display the current buffer's file if FILE is nil or if called
interactively.  Turn the filename into a URL by performing
replacements given in variable `browse-url-filename-alist'.  Pass the
URL to a browser using variable `browse-url-browser-function' then run
`browse-url-of-file-hook'."
  (interactive)
  (setq file (or file
		 (buffer-file-name)
		 (and (boundp 'dired-directory) dired-directory)))
  (let ((buf (get-file-buffer file)))
    (if buf (save-excursion
	      (set-buffer buf)
	      (cond
	       ((not (buffer-modified-p)))
	       (browse-url-save-file (save-buffer))
	       (t (message "%s modified since last save" file))))))
  (funcall browse-url-browser-function
	   (browse-url-file-url file))
  (run-hooks 'browse-url-of-file-hook))

(defun browse-url-file-url (file)
  "Return the URL corresponding to FILE.
Uses variable `browse-url-filename-alist' to map filenames to URLs."
  (let ((maps browse-url-filename-alist))
    (while maps
      (let* ((map (car maps))
	     (from-re (car map))
	     (to-string (cdr map)))
	(setq maps (cdr maps))
	(if (string-match from-re file)
	    (setq file (concat (substring file 0 (match-beginning 0))
			       to-string
			       (substring file (match-end 0))))))))
  file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Browser-specific functions

(defun browse-url-netscape (url &optional new-window)
  "Ask the Netscape WWW browser to load URL.

Default to the URL around or before point.  The strings in variable
`browse-url-netscape-arguments' are also passed to Netscape.

If variable `browse-url-netscape-new-window-p' is non-nil, load the
document in a new Netscape window, otherwise use a random existing
one.  If optional argument NEW-WINDOW (prefix argument if interactive)
is non-nil, the effect of browse-url-netscape-new-window-p is
reversed."
  (interactive (append (browse-url-interactive-arg "Netscape URL: ")
		       (list (not (eq (null browse-url-netscape-new-window-p)
				      (null current-prefix-arg))))))
  (or (zerop
       (apply 'call-process "netscape" nil nil nil
	      (append browse-url-netscape-arguments
		      (if new-window '("-noraise"))
		      (list "-remote" 
			    (concat "openURL(" url 
				    (if new-window ",new-window")
				    ")")))))
      (progn				; Netscape not running - start it
	(message "Starting Netscape...")
	(apply 'start-process "netscape" nil "netscape"
	       (append browse-url-netscape-arguments (list url))))))

(defun browse-url-netscape-reload ()
  "Ask Netscape to reload its current document."
  (interactive)
  (browse-url-netscape-command "reload"))

(defun browse-url-netscape-command (command)
  "Send a remote control command to Netscape."
  (apply 'start-process "netscape" nil "netscape"
	 (append browse-url-netscape-arguments
		 (list "-remote" command))))

(defun browse-url-mosaic (url)
  "Ask the XMosaic WWW browser to load URL.
Default to the URL around or before point."
  (interactive (browse-url-interactive-arg "Mosaic URL: "))
  (let ((pidfile (expand-file-name "~/.mosaicpid"))
	pid pidbuf)
    (if (file-readable-p pidfile)
	(save-excursion
	  (find-file pidfile)
	  (goto-char (point-min))
	  (setq pid (read (current-buffer)))
	  (kill-buffer nil)))
    (if (and pid (zerop (signal-process pid 0))) ; Mosaic running
	(save-excursion
	  (find-file (format "/tmp/Mosaic.%d" pid))
	  (erase-buffer)
	  (insert "goto\n" url "\n")
	  (save-buffer)
	  (kill-buffer nil)
	  ;; Send signal SIGUSR to Mosaic
	  (message "Signalling Mosaic...")
	  (signal-process pid browse-url-usr1-signal)
	  ;; Or you could try:
	  ;; (call-process "kill" nil 0 nil "-USR1" (int-to-string pid))
	  )
      ;; Mosaic not running - start it
      (message "Starting Mosaic...")
      (apply 'start-process "xmosaic" nil "xmosaic"
	     (append browse-url-mosaic-arguments (list url))))))

(defun browse-url-iximosaic (url)
  "Ask the IXIMosaic WWW browser to load URL.
Default to the URL around or before point."
  (interactive (browse-url-interactive-arg "IXI Mosaic URL: "))
  (start-process "tellw3b" nil "tellw3b"
		 "-service WWW_BROWSER ixi_showurl " url))

(defun browse-url-w3 (url)
  "Ask the w3 WWW browser to load URL.
Default to the URL around or before point."
  (interactive (browse-url-interactive-arg "W3 URL: "))
  (w3-fetch url))

(provide 'browse-url)

;;; browse-url.el ends here
