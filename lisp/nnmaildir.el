;;; nnmaildir.el --- maildir backend for Gnus
;; Public domain.

;; Author: Paul Jarc <prj@po.cwru.edu>

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
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Maildir format is documented in the maildir(5) man page from qmail
;; (available at <URL:http://multivac.cwru.edu./prj/maildir.5>) and at
;; <URL:http://cr.yp.to/proto/maildir.html>.  nnmaildir also stores
;; extra information in the .nnmaildir/ directory within a maildir.
;;
;; Some goals of nnmaildir:
;; * Everything Just Works, and correctly.  E.g., stale NOV data is
;;   ignored; no need for -generate-nov-databases.
;; * Perfect reliability: [C-g] will never corrupt its data in memory,
;;   and SIGKILL will never corrupt its data in the filesystem.
;; * We use the filesystem as a database, so that, e.g., it's easy to
;;   manipulate marks from outside Gnus.
;; * All information about a group is stored in the maildir, for easy
;;   backup, copying, restoring, etc.
;;
;; Todo:
;; * Don't force article renumbering, so nnmaildir can be used with
;;   the cache and agent.  Alternatively, completely rewrite the Gnus
;;   backend interface, which would have other advantages as well.
;;
;; See also <URL:http://multivac.cwru.edu./nnmaildir/> until that
;; information is added to the Gnus manual.

;;; Code:

(eval-and-compile
  (require 'nnheader)
  (require 'gnus)
  (require 'gnus-util)
  (require 'gnus-range)
  (require 'gnus-start)
  (require 'gnus-int)
  (require 'message))
(eval-when-compile
  (require 'cl)
  (require 'nnmail))

(defconst nnmaildir-version "Gnus")

(defvar nnmaildir-article-file-name nil
  "*The filename of the most recently requested article.  This variable is set
by nnmaildir-request-article.")

;; The filename of the article being moved/copied:
(defvar nnmaildir--file nil)

;; Variables to generate filenames of messages being delivered:
(defvar   nnmaildir--delivery-time "")
(defconst nnmaildir--delivery-pid  (number-to-string (emacs-pid)))
(defvar   nnmaildir--delivery-ct   nil)

;; An obarry containing symbols whose names are server names and whose values
;; are servers:
(defvar nnmaildir--servers (make-vector 3 0))
;; The current server:
(defvar nnmaildir--cur-server nil)

;; A copy of nnmail-extra-headers
(defvar nnmaildir--extra nil)

;; A disk NOV structure (must be prin1-able, so no defstruct) looks like this:
["subject\tfrom\tdate"
 "references\tchars\lines"
 "To: you\tIn-Reply-To: <your.mess@ge>"
 (12345 67890)     ;; modtime of the corresponding article file
 (to in-reply-to)] ;; contemporary value of nnmail-extra-headers
(defconst nnmaildir--novlen 5)
(defmacro nnmaildir--nov-new (beg mid end mtime extra)
  `(vector ,beg ,mid ,end ,mtime ,extra))
(defmacro nnmaildir--nov-get-beg   (nov) `(aref ,nov 0))
(defmacro nnmaildir--nov-get-mid   (nov) `(aref ,nov 1))
(defmacro nnmaildir--nov-get-end   (nov) `(aref ,nov 2))
(defmacro nnmaildir--nov-get-mtime (nov) `(aref ,nov 3))
(defmacro nnmaildir--nov-get-extra (nov) `(aref ,nov 4))
(defmacro nnmaildir--nov-set-beg   (nov value) `(aset ,nov 0 ,value))
(defmacro nnmaildir--nov-set-mid   (nov value) `(aset ,nov 1 ,value))
(defmacro nnmaildir--nov-set-end   (nov value) `(aset ,nov 2 ,value))
(defmacro nnmaildir--nov-set-mtime (nov value) `(aset ,nov 3 ,value))
(defmacro nnmaildir--nov-set-extra (nov value) `(aset ,nov 4 ,value))

(defstruct nnmaildir--art
  (prefix nil :type string)  ;; "time.pid.host"
  (suffix nil :type string)  ;; ":2,flags"
  (num    nil :type natnum)  ;; article number
  (msgid  nil :type string)  ;; "<mess.age@id>"
  (nov    nil :type vector)) ;; cached nov structure, or nil

(defstruct nnmaildir--lists
  (nlist nil :type list)    ;; list of articles, ordered descending by number
  (flist nil :type vector)  ;; obarray mapping filename prefix->article
  (mlist nil :type vector)) ;; obarray mapping message-id->article

(defstruct nnmaildir--grp
  (name  nil :type string)     	     ;; "group.name"
  (new   nil :type list)       	     ;; new/ modtime
  (cur   nil :type list)       	     ;; cur/ modtime
  (lists nil :type nnmaildir--lists) ;; lists of articles in this group
  (cache nil :type vector)           ;; nov cache
  (index nil :type natnum)           ;; index of next cache entry to replace
  (mmth  nil :type vector))          ;; obarray mapping mark name->dir modtime

(defstruct nnmaildir--srv
  (address    nil :type string)         ;; server address string
  (method     nil :type list)           ;; (nnmaildir "address" ...)
  (prefix     nil :type string)         ;; "nnmaildir+address:"
  (dir        nil :type string)         ;; "/expanded/path/to/server/dir/"
  (ls         nil :type function)       ;; directory-files function
  (groups     nil :type vector)         ;; obarray mapping group names->groups
  (curgrp     nil :type nnmaildir--grp) ;; current group, or nil
  (error      nil :type string)         ;; last error message, or nil
  (mtime      nil :type list)           ;; modtime of dir
  (gnm        nil)                      ;; flag: split from mail-sources?
  (create-dir nil :type string))        ;; group creation directory

(defmacro nnmaildir--nlist-last-num (nlist)
  `(let ((nlist ,nlist))
     (if nlist (nnmaildir--art-num (car nlist)) 0)))
(defmacro nnmaildir--nlist-art (nlist num) ;;;; evals args multiple times
  `(and ,nlist
	(>= (nnmaildir--art-num (car ,nlist)) ,num)
	(nth (- (nnmaildir--art-num (car ,nlist)) ,num) ,nlist)))
(defmacro nnmaildir--flist-art (list file)
  `(symbol-value (intern-soft ,file ,list)))
(defmacro nnmaildir--mlist-art (list msgid)
  `(symbol-value (intern-soft ,msgid ,list)))

(defun nnmaildir--pgname (server gname)
  (let ((prefix (nnmaildir--srv-prefix server)))
    (if prefix (concat prefix gname)
      (setq gname (gnus-group-prefixed-name gname
					    (nnmaildir--srv-method server)))
      (setf (nnmaildir--srv-prefix server) (gnus-group-real-prefix gname))
      gname)))

(defun nnmaildir--param (pgname param)
  (setq param (gnus-group-find-parameter pgname param 'allow-list)
	param (if (vectorp param) (aref param 0) param))
  (eval param))

(defmacro nnmaildir--with-nntp-buffer (&rest body)
  `(save-excursion
     (set-buffer nntp-server-buffer)
     ,@body))
(defmacro nnmaildir--with-work-buffer (&rest body)
  `(save-excursion
     (set-buffer (get-buffer-create " *nnmaildir work*"))
     ,@body))
(defmacro nnmaildir--with-nov-buffer (&rest body)
  `(save-excursion
     (set-buffer (get-buffer-create " *nnmaildir nov*"))
     ,@body))
(defmacro nnmaildir--with-move-buffer (&rest body)
  `(save-excursion
     (set-buffer (get-buffer-create " *nnmaildir move*"))
     ,@body))

(defmacro nnmaildir--subdir (dir subdir)
  `(file-name-as-directory (concat ,dir ,subdir)))
(defmacro nnmaildir--srvgrp-dir (srv-dir gname)
  `(nnmaildir--subdir ,srv-dir ,gname))
(defmacro nnmaildir--tmp       (dir) `(nnmaildir--subdir ,dir "tmp"))
(defmacro nnmaildir--new       (dir) `(nnmaildir--subdir ,dir "new"))
(defmacro nnmaildir--cur       (dir) `(nnmaildir--subdir ,dir "cur"))
(defmacro nnmaildir--nndir     (dir) `(nnmaildir--subdir ,dir ".nnmaildir"))
(defmacro nnmaildir--nov-dir   (dir) `(nnmaildir--subdir ,dir "nov"))
(defmacro nnmaildir--marks-dir (dir) `(nnmaildir--subdir ,dir "marks"))

(defmacro nnmaildir--unlink (file-arg)
  `(let ((file ,file-arg))
     (if (file-attributes file) (delete-file file))))
(defun nnmaildir--mkdir (dir)
  (or (file-exists-p (file-name-as-directory dir))
      (make-directory-internal (directory-file-name dir))))

(defun nnmaildir--prepare (server group)
  (let (x groups)
    (catch 'return
      (if (null server)
	  (or (setq server nnmaildir--cur-server)
	      (throw 'return nil))
	(or (setq server (intern-soft server nnmaildir--servers))
	    (throw 'return nil))
	(setq server (symbol-value server)
	      nnmaildir--cur-server server))
      (or (setq groups (nnmaildir--srv-groups server))
          (throw 'return nil))
      (if (nnmaildir--srv-method server) nil
	(setq x (concat "nnmaildir:" (nnmaildir--srv-address server))
	      x (gnus-server-to-method x))
	(or x (throw 'return nil))
	(setf (nnmaildir--srv-method server) x))
      (if (null group)
	  (or (setq group (nnmaildir--srv-curgrp server))
	      (throw 'return nil))
	(or (setq group (intern-soft group groups))
            (throw 'return nil))
	(setq group (symbol-value group)))
      group)))

(defun nnmaildir--update-nov (server group article)
  (let ((nnheader-file-coding-system 'binary)
	(srv-dir (nnmaildir--srv-dir server))
	dir gname pgname msgdir prefix suffix file attr mtime novdir novfile
	nov msgid nov-beg nov-mid nov-end field pos extra val old-extra
	new-extra deactivate-mark)
    (catch 'return
      (setq suffix (nnmaildir--art-suffix article))
      (if (stringp suffix) nil
	(setf (nnmaildir--art-nov article) nil)
	(throw 'return nil))
      (setq gname (nnmaildir--grp-name group)
	    pgname (nnmaildir--pgname server gname)
	    dir (nnmaildir--srvgrp-dir srv-dir gname)
	    msgdir (if (nnmaildir--param pgname 'read-only)
		       (nnmaildir--new dir) (nnmaildir--cur dir))
	    prefix (nnmaildir--art-prefix article)
	    file (concat msgdir prefix suffix)
	    attr (file-attributes file))
      (if attr nil
	(setf (nnmaildir--art-suffix article) 'expire)
	(setf (nnmaildir--art-nov    article) nil)
	(throw 'return nil))
      (setq mtime (nth 5 attr)
	    attr (nth 7 attr)
	    nov (nnmaildir--art-nov article)
	    novdir (nnmaildir--nov-dir (nnmaildir--nndir dir))
	    novfile (concat novdir prefix))
      (or (equal nnmaildir--extra nnmail-extra-headers)
	  (setq nnmaildir--extra (copy-sequence nnmail-extra-headers)))
      (nnmaildir--with-nov-buffer
	(when (file-exists-p novfile) ;; If not, force reparsing the message.
	  (if nov nil ;; It's already in memory.
	    ;; Else read the data from the NOV file.
	    (erase-buffer)
	    (nnheader-insert-file-contents novfile)
	    (setq nov (read (current-buffer)))
	    (setf (nnmaildir--art-msgid article) (car nov))
	    (setq nov (cadr nov)))
	  ;; If the NOV's modtime matches the file's current modtime, and it
	  ;; has the right structure (i.e., it wasn't produced by a too-much
	  ;; older version of nnmaildir), then we may use this NOV data
	  ;; rather than parsing the message file, unless
	  ;; nnmail-extra-headers has been augmented since this data was last
	  ;; parsed.
	  (when (and (equal mtime (nnmaildir--nov-get-mtime nov))
		     (= (length nov) nnmaildir--novlen)
		     (stringp (nnmaildir--nov-get-beg	nov))
		     (stringp (nnmaildir--nov-get-mid	nov))
		     (stringp (nnmaildir--nov-get-end	nov))
		     (listp   (nnmaildir--nov-get-mtime nov))
		     (listp   (nnmaildir--nov-get-extra nov)))
	    ;; this NOV data is potentially up-to-date; now check extra headers
	    (setq old-extra (nnmaildir--nov-get-extra nov))
	    (when (equal nnmaildir--extra old-extra) ;; common case
	      (nnmaildir--nov-set-extra nov nnmaildir--extra) ;; save memory
	      (throw 'return nov))
	    ;; They're not equal, but maybe the new is a subset of the old...
	    (if (null nnmaildir--extra) (throw 'return nov))
	    (setq new-extra nnmaildir--extra)
	    (while new-extra
	      (if (memq (car new-extra) old-extra)
		  (progn
		    (setq new-extra (cdr new-extra))
		    (if new-extra nil (throw 'return nov)))
		(setq new-extra nil))))) ;;found one not in old-extra;quit loop
	;; Parse the NOV data out of the message.
	(erase-buffer)
	(nnheader-insert-file-contents file)
	(insert "\n")
	(goto-char (point-min))
	(save-restriction
	  (if (search-forward "\n\n" nil 'noerror)
	      (progn
		(setq nov-mid (count-lines (point) (point-max)))
		(narrow-to-region (point-min) (1- (point))))
	    (setq nov-mid 0))
	  (goto-char (point-min))
	  (delete-char 1)
	  (nnheader-fold-continuation-lines)
	  (setq nov (nnheader-parse-head 'naked)
		field (or (mail-header-lines nov) 0)))
	(if (or (zerop field) (nnmaildir--param pgname 'distrust-Lines:)) nil
	  (setq nov-mid field))
	(setq nov-mid (number-to-string nov-mid)
	      nov-mid (concat (number-to-string attr) "\t" nov-mid)
	      field (or (mail-header-references nov) "")
	      pos 0)
	(save-match-data
	  (while (string-match "\t" field pos)
	    (aset field (match-beginning 0) ? )
	    (setq pos (match-end 0)))
	  (setq nov-mid (concat field "\t" nov-mid)
		extra (mail-header-extra nov)
		nov-end "")
	  (while extra
	    (setq field (car extra) extra (cdr extra)
		  val (cdr field) field (symbol-name (car field))
		  pos 0)
	    (while (string-match "\t" field pos)
	      (aset field (match-beginning 0) ? )
	      (setq pos (match-end 0)))
	    (setq pos 0)
	    (while (string-match "\t" val pos)
	      (aset val (match-beginning 0) ? )
	      (setq pos (match-end 0)))
	    (setq nov-end (concat nov-end "\t" field ": " val)))
	  (setq nov-end (if (zerop (length nov-end)) "" (substring nov-end 1))
		field (or (mail-header-subject nov) "")
		pos 0)
	  (while (string-match "\t" field pos)
	    (aset field (match-beginning 0) ? )
	    (setq pos (match-end 0)))
	  (setq nov-beg field
		field (or (mail-header-from nov) "")
		pos 0)
	  (while (string-match "\t" field pos)
	    (aset field (match-beginning 0) ? )
	    (setq pos (match-end 0)))
	  (setq nov-beg (concat nov-beg "\t" field)
		field (or (mail-header-date nov) "")
		pos 0)
	  (while (string-match "\t" field pos)
	    (aset field (match-beginning 0) ? )
	    (setq pos (match-end 0)))
	  (setq nov-beg (concat nov-beg "\t" field)
		field (mail-header-id nov)
		pos 0)
	  (while (string-match "\t" field pos)
	    (aset field (match-beginning 0) ? )
	    (setq pos (match-end 0)))
	  (setq msgid field))
	(if (or (null msgid) (nnheader-fake-message-id-p msgid))
	    (setq msgid (concat "<" prefix "@nnmaildir>")))
	(setq nov (nnmaildir--nov-new nov-beg nov-mid nov-end mtime
				      nnmaildir--extra))
	(erase-buffer)
	(prin1 (list msgid nov) (current-buffer))
	(setq file (concat novfile ":"))
	(nnmaildir--unlink file)
	(write-region (point-min) (point-max) file nil 'no-message))
      (rename-file file novfile 'replace)
      (setf (nnmaildir--art-msgid article) msgid)
      nov)))

(defun nnmaildir--cache-nov (group article nov)
  (let ((cache (nnmaildir--grp-cache group))
	(index (nnmaildir--grp-index group))
	goner)
    (if (nnmaildir--art-nov article) nil
      (setq goner (aref cache index))
      (if goner (setf (nnmaildir--art-nov goner) nil))
      (aset cache index article)
      (setf (nnmaildir--grp-index group) (% (1+ index) (length cache))))
    (setf (nnmaildir--art-nov article) nov)))

(defun nnmaildir--grp-add-art (server group article)
  (let ((nov (nnmaildir--update-nov server group article))
	old-lists new-lists)
    (when nov
      (setq old-lists (nnmaildir--grp-lists group)
	    new-lists (copy-nnmaildir--lists old-lists))
      (setf (nnmaildir--lists-nlist new-lists)
	    (cons article (nnmaildir--lists-nlist new-lists)))
      (let ((inhibit-quit t))
        (setf (nnmaildir--grp-lists group) new-lists)
        (set (intern (nnmaildir--art-prefix article)
                     (nnmaildir--lists-flist new-lists))
             article)
        (set (intern (nnmaildir--art-msgid article)
                     (nnmaildir--lists-mlist new-lists))
             article))
      (nnmaildir--cache-nov group article nov)
      t)))

(defun nnmaildir--group-ls (server pgname)
  (or (nnmaildir--param pgname 'directory-files)
      (nnmaildir--srv-ls server)))

(defun nnmaildir--article-count (group)
  (let ((ct 0)
	(min 1))
    (setq group (nnmaildir--grp-lists group)
	  group (nnmaildir--lists-nlist group))
    (while group
      (if (stringp (nnmaildir--art-suffix (car group)))
	  (setq ct (1+ ct)
		min (nnmaildir--art-num (car group))))
      (setq group (cdr group)))
    (cons ct min)))

(defun nnmaildir-article-number-to-file-name
  (number group-name server-address-string)
  (let ((group (nnmaildir--prepare server-address-string group-name))
	list article suffix dir filename pgname)
    (catch 'return
      (if (null group)
	  ;; The given group or server does not exist.
	  (throw 'return nil))
      (setq list (nnmaildir--grp-lists group)
	    list (nnmaildir--lists-nlist list)
	    article (nnmaildir--nlist-art list number))
      (if (null article)
	  ;; The given article number does not exist in this group.
	  (throw 'return nil))
      (setq suffix (nnmaildir--art-suffix article))
      (if (not (stringp suffix))
	  ;; The article has expired.
	  (throw 'return nil))
      (setq dir (nnmaildir--srv-dir nnmaildir--cur-server)
	    dir (nnmaildir--srvgrp-dir dir group-name)
	    pgname (nnmaildir--pgname nnmaildir--cur-server group-name)
	    group (if (nnmaildir--param pgname 'read-only)
		      (nnmaildir--new dir) (nnmaildir--cur dir))
	    filename (concat group (nnmaildir--art-prefix article) suffix))
      (if (file-exists-p filename)
	  filename
	;; The article disappeared out from under us.
	(setf (nnmaildir--art-suffix article) 'expire)
	(setf (nnmaildir--art-nov    article) nil)
	nil))))

(defun nnmaildir-article-number-to-base-name
  (number group-name server-address-string)
  (let ((group (nnmaildir--prepare server-address-string group-name))
	list article suffix dir filename)
    (catch 'return
      (if (null group)
	  ;; The given group or server does not exist.
	  (throw 'return nil))
      (setq list (nnmaildir--grp-lists group)
	    list (nnmaildir--lists-nlist list)
	    article (nnmaildir--nlist-art list number))
      (if (null article)
	  ;; The given article number does not exist in this group.
	  (throw 'return nil))
      (setq suffix (nnmaildir--art-suffix article))
      (if (not (stringp suffix))
	  ;; The article has expired.
	  (throw 'return nil))
      (cons (nnmaildir--art-prefix article) suffix))))

(defun nnmaildir-base-name-to-article-number
  (base-name group-name server-address-string)
  (let ((group (nnmaildir--prepare server-address-string group-name))
	list article suffix dir filename)
    (catch 'return
      (if (null group)
	  ;; The given group or server does not exist.
	  (throw 'return nil))
      (setq list (nnmaildir--grp-lists group)
	    list (nnmaildir--lists-flist list)
	    article (nnmaildir--flist-art list base-name))
      (if (null article)
	  ;; The given article number does not exist in this group.
	  (throw 'return nil))
      (nnmaildir--art-num article))))

(defun nnmaildir-request-type (group &optional article)
  'mail)

(defun nnmaildir-status-message (&optional server)
  (nnmaildir--prepare server nil)
  (nnmaildir--srv-error nnmaildir--cur-server))

(defun nnmaildir-server-opened (&optional server)
  (and nnmaildir--cur-server
       (if server
	   (string-equal server (nnmaildir--srv-address nnmaildir--cur-server))
	 t)
       (nnmaildir--srv-groups nnmaildir--cur-server)
       t))

(defun nnmaildir-open-server (server &optional defs)
  (let ((x server)
	dir size)
    (catch 'return
      (setq server (intern-soft x nnmaildir--servers))
      (if server
	  (and (setq server (symbol-value server))
	       (nnmaildir--srv-groups server)
	       (setq nnmaildir--cur-server server)
	       (throw 'return t))
	(setq server (make-nnmaildir--srv :address x))
        (let ((inhibit-quit t))
          (set (intern x nnmaildir--servers) server)))
      (setq dir (assq 'directory defs))
      (if dir nil
	(setf (nnmaildir--srv-error server)
	      "You must set \"directory\" in the select method")
	(throw 'return nil))
      (setq dir (cadr dir)
	    dir (eval dir)
	    dir (expand-file-name dir)
	    dir (file-name-as-directory dir))
      (if (file-exists-p dir) nil
	(setf (nnmaildir--srv-error server) (concat "No such directory: " dir))
	(throw 'return nil))
      (setf (nnmaildir--srv-dir server) dir)
      (setq x (assq 'directory-files defs))
      (if (null x)
	  (setq x (symbol-function (if nnheader-directory-files-is-safe
				       'directory-files
				     'nnheader-directory-files-safe)))
	(setq x (cadr x))
	(if (functionp x) nil
	  (setf (nnmaildir--srv-error server)
		(concat "Not a function: " (prin1-to-string x)))
	  (throw 'return nil)))
      (setf (nnmaildir--srv-ls server) x)
      (setq x (funcall x dir nil "\\`[^.]" 'nosort)
	    x (length x)
	    size 1)
      (while (<= size x) (setq size (* 2 size)))
      (if (/= size 1) (setq size (1- size)))
      (and (setq x (assq 'get-new-mail defs))
	   (setq x (cdr x))
	   (car x)
	   (setf (nnmaildir--srv-gnm server) t)
	   (require 'nnmail))
      (setq x (assq 'create-directory defs))
      (when x
	(setq x (cadr x)
	      x (eval x))
	(setf (nnmaildir--srv-create-dir server) x))
      (setf (nnmaildir--srv-groups server) (make-vector size 0))
      (setq nnmaildir--cur-server server)
      t)))

(defun nnmaildir--parse-filename (file)
  (let ((prefix (car file))
	timestamp len)
    (if (string-match
	 "\\`\\([0-9]+\\)\\.\\([0-9]+\\)\\(_\\([0-9]+\\)\\)?\\(\\..*\\)\\'"
	 prefix)
	(progn
	  (setq timestamp (concat "0000" (match-string 1 prefix))
		len (- (length timestamp) 4))
	  (vector (string-to-number (substring timestamp 0 len))
		  (string-to-number (substring timestamp len))
		  (string-to-number (match-string 2 prefix))
		  (string-to-number (or (match-string 4 prefix) "-1"))
		  (match-string 5 prefix)
		  file))
      file)))

(defun nnmaildir--sort-files (a b)
  (catch 'return
    (if (consp a)
	(throw 'return (and (consp b) (string-lessp (car a) (car b)))))
    (if (consp b) (throw 'return t))
    (if (< (aref a 0) (aref b 0)) (throw 'return t))
    (if (> (aref a 0) (aref b 0)) (throw 'return nil))
    (if (< (aref a 1) (aref b 1)) (throw 'return t))
    (if (> (aref a 1) (aref b 1)) (throw 'return nil))
    (if (< (aref a 2) (aref b 2)) (throw 'return t))
    (if (> (aref a 2) (aref b 2)) (throw 'return nil))
    (if (< (aref a 3) (aref b 3)) (throw 'return t))
    (if (> (aref a 3) (aref b 3)) (throw 'return nil))
    (string-lessp (aref a 4) (aref b 4))))

(defun nnmaildir--scan (gname scan-msgs groups method srv-dir srv-ls)
  (catch 'return
    (let ((36h-ago (- (car (current-time)) 2))
	  absdir nndir tdir ndir cdir nattr cattr isnew pgname read-only ls
	  files file num dir flist group x)
      (setq absdir (nnmaildir--srvgrp-dir srv-dir gname)
	    nndir (nnmaildir--nndir absdir))
      (if (file-exists-p absdir) nil
	(setf (nnmaildir--srv-error nnmaildir--cur-server)
	      (concat "No such directory: " absdir))
	(throw 'return nil))
      (setq tdir (nnmaildir--tmp absdir)
	    ndir (nnmaildir--new absdir)
	    cdir (nnmaildir--cur absdir)
	    nattr (file-attributes ndir)
	    cattr (file-attributes cdir))
      (if (and (file-exists-p tdir) nattr cattr) nil
	(setf (nnmaildir--srv-error nnmaildir--cur-server)
	      (concat "Not a maildir: " absdir))
	(throw 'return nil))
      (setq group (nnmaildir--prepare nil gname)
	    pgname (nnmaildir--pgname nnmaildir--cur-server gname))
      (if group
	  (setq isnew nil)
	(setq isnew t
	      group (make-nnmaildir--grp :name gname :index 0
					 :lists (make-nnmaildir--lists)))
	(nnmaildir--mkdir nndir)
	(nnmaildir--mkdir (nnmaildir--nov-dir   nndir))
	(nnmaildir--mkdir (nnmaildir--marks-dir nndir))
	(write-region "" nil (concat nndir "markfile") nil 'no-message))
      (setq read-only (nnmaildir--param pgname 'read-only)
	    ls (or (nnmaildir--param pgname 'directory-files) srv-ls))
      (if read-only nil
	(setq x (nth 11 (file-attributes tdir)))
	(if (and (= x (nth 11 nattr)) (= x (nth 11 cattr))) nil
	  (setf (nnmaildir--srv-error nnmaildir--cur-server)
		(concat "Maildir spans filesystems: " absdir))
	  (throw 'return nil))
	(setq files (funcall ls tdir 'full "\\`[^.]" 'nosort))
	(while files
	  (setq file (car files) files (cdr files)
		x (file-attributes file))
	  (if (or (< 1 (cadr x)) (> 36h-ago (car (nth 4 x))))
	      (delete-file file))))
      (or scan-msgs
	  isnew
	  (throw 'return t))
      (setq nattr (nth 5 nattr))
      (if (equal nattr (nnmaildir--grp-new group))
	  (setq nattr nil))
      (if read-only (setq dir (and (or isnew nattr) ndir))
	(when (or isnew nattr)
	  (setq files (funcall ls ndir nil "\\`[^.]" 'nosort))
	  (while files
	    (setq file (car files) files (cdr files))
	    (rename-file (concat ndir file) (concat cdir file ":2,")))
	  (setf (nnmaildir--grp-new group) nattr))
	(setq cattr (nth 5 (file-attributes cdir)))
	(if (equal cattr (nnmaildir--grp-cur group))
	    (setq cattr nil))
	(setq dir (and (or isnew cattr) cdir)))
      (if dir nil (throw 'return t))
      (setq files (funcall ls dir nil "\\`[^.]" 'nosort))
      (when isnew
	(setq x (length files)
	      num 1)
	(while (<= num x) (setq num (* 2 num)))
	(if (/= num 1) (setq num (1- num)))
	(setq x (nnmaildir--grp-lists group))
	(setf (nnmaildir--lists-flist x) (make-vector num 0))
	(setf (nnmaildir--lists-mlist x) (make-vector num 0))
	(setf (nnmaildir--grp-mmth group) (make-vector 1 0))
	(setq num (nnmaildir--param pgname 'nov-cache-size))
	(if (numberp num) (if (< num 1) (setq num 1))
	  (setq x files
		num 16
		cdir (nnmaildir--marks-dir nndir)
		ndir (nnmaildir--subdir cdir "tick")
		cdir (nnmaildir--subdir cdir "read"))
	  (while x
	    (setq file (car x) x (cdr x))
	    (string-match "\\`\\([^:]*\\)\\(\\(:.*\\)?\\)\\'" file)
	    (setq file (match-string 1 file))
	    (if (or (not (file-exists-p (concat cdir file)))
		    (file-exists-p (concat ndir file)))
		(setq num (1+ num)))))
	(setf (nnmaildir--grp-cache group) (make-vector num nil))
        (let ((inhibit-quit t))
          (set (intern gname groups) group))
	(or scan-msgs (throw 'return t)))
      (setq flist (nnmaildir--grp-lists group)
	    num (nnmaildir--lists-nlist flist)
	    flist (nnmaildir--lists-flist flist)
	    num (nnmaildir--nlist-last-num num)
	    x files
	    files nil)
      (while x
	(setq file (car x) x (cdr x))
	(string-match "\\`\\([^:]*\\)\\(\\(:.*\\)?\\)\\'" file)
	(setq file (cons (match-string 1 file) (match-string 2 file)))
	(if (nnmaildir--flist-art flist (car file)) nil
	  (setq files (cons file files))))
      (setq files (mapcar 'nnmaildir--parse-filename files)
	    files (sort files 'nnmaildir--sort-files))
      (while files
	(setq file (car files) files (cdr files)
	      file (if (consp file) file (aref file 5))
	      x (make-nnmaildir--art :prefix (car file) :suffix(cdr file)
				     :num (1+ num)))
	(if (nnmaildir--grp-add-art nnmaildir--cur-server group x)
	    (setq num (1+ num))))
      (if read-only (setf (nnmaildir--grp-new group) nattr)
	(setf (nnmaildir--grp-cur group) cattr)))
    t))

(defun nnmaildir-request-scan (&optional scan-group server)
  (let ((coding-system-for-write nnheader-file-coding-system)
	(buffer-file-coding-system nil)
	(file-coding-system-alist nil)
	(nnmaildir-new-mail t)
	(nnmaildir-group-alist nil)
	(nnmaildir-active-file nil)
	x srv-ls srv-dir method groups group dirs grp-dir seen deactivate-mark)
    (nnmaildir--prepare server nil)
    (setq srv-ls (nnmaildir--srv-ls nnmaildir--cur-server)
	  srv-dir (nnmaildir--srv-dir nnmaildir--cur-server)
	  method (nnmaildir--srv-method nnmaildir--cur-server)
	  groups (nnmaildir--srv-groups nnmaildir--cur-server))
    (nnmaildir--with-work-buffer
      (save-match-data
	(if (stringp scan-group)
	    (if (nnmaildir--scan scan-group t groups method srv-dir srv-ls)
		(if (nnmaildir--srv-gnm nnmaildir--cur-server)
		    (nnmail-get-new-mail 'nnmaildir nil nil scan-group))
	      (unintern scan-group groups))
	  (setq x (nth 5 (file-attributes srv-dir)))
	  (if (equal x (nnmaildir--srv-mtime nnmaildir--cur-server))
	      (if scan-group nil
		(mapatoms (lambda (sym)
			    (nnmaildir--scan (symbol-name sym) t groups
					     method srv-dir srv-ls))
			  groups))
	    (setq dirs (funcall srv-ls srv-dir nil "\\`[^.]" 'nosort)
		  x (length dirs)
		  seen 1)
	    (while (<= seen x) (setq seen (* 2 seen)))
	    (if (/= seen 1) (setq seen (1- seen)))
	    (setq seen (make-vector seen 0)
		  scan-group (null scan-group))
	    (while dirs
	      (setq grp-dir (car dirs) dirs (cdr dirs))
	      (if (nnmaildir--scan grp-dir scan-group groups method srv-dir
				   srv-ls)
		  (intern grp-dir seen)))
	    (setq x nil)
	    (mapatoms (lambda (group)
			(setq group (symbol-name group))
			(if (intern-soft group seen) nil
			  (setq x (cons group x))))
		      groups)
	    (while x
	      (unintern (car x) groups)
	      (setq x (cdr x)))
	    (setf (nnmaildir--srv-mtime nnmaildir--cur-server)
		  (nth 5 (file-attributes srv-dir))))
	  (if (nnmaildir--srv-gnm nnmaildir--cur-server)
	      (nnmail-get-new-mail 'nnmaildir nil nil))))))
  t)

(defun nnmaildir-request-list (&optional server)
  (nnmaildir-request-scan 'find-new-groups server)
  (let (pgname ro ct-min deactivate-mark)
    (nnmaildir--prepare server nil)
    (nnmaildir--with-nntp-buffer
      (erase-buffer)
      (mapatoms (lambda (group)
		  (setq pgname (symbol-name group)
			pgname (nnmaildir--pgname nnmaildir--cur-server pgname)
			group (symbol-value group)
			ro (nnmaildir--param pgname 'read-only)
			ct-min (nnmaildir--article-count group))
		  (insert (nnmaildir--grp-name group) " ")
                  (princ (nnmaildir--nlist-last-num
                           (nnmaildir--lists-nlist
                             (nnmaildir--grp-lists group)))
                         nntp-server-buffer)
		  (insert " ")
		  (princ (cdr ct-min) nntp-server-buffer)
		  (insert " " (if ro "n" "y") "\n"))
		(nnmaildir--srv-groups nnmaildir--cur-server))))
  t)

(defun nnmaildir-request-newgroups (date &optional server)
  (nnmaildir-request-list server))

(defun nnmaildir-retrieve-groups (groups &optional server)
  (let (gname group ct-min deactivate-mark)
    (nnmaildir--prepare server nil)
    (nnmaildir--with-nntp-buffer
      (erase-buffer)
      (while groups
	(setq gname (car groups) groups (cdr groups))
	(setq group (nnmaildir--prepare nil gname))
	(if (null group) (insert "411 no such news group\n")
	  (setq ct-min (nnmaildir--article-count group))
	  (insert "211 ")
	  (princ (car ct-min) nntp-server-buffer)
	  (insert " ")
	  (princ (cdr ct-min) nntp-server-buffer)
	  (insert " ")
	  (princ (nnmaildir--nlist-last-num
		   (nnmaildir--lists-nlist
		     (nnmaildir--grp-lists group)))
		 nntp-server-buffer)
	  (insert " " gname "\n")))))
  'group)

(defun nnmaildir-request-update-info (gname info &optional server)
  (let ((group (nnmaildir--prepare server gname))
	pgname nlist flist last always-marks never-marks old-marks dotfile num
        dir markdirs marks mark ranges articles article read end new-marks ls
        old-mmth new-mmth mtime mark-sym deactivate-mark)
    (catch 'return
      (if group nil
	(setf (nnmaildir--srv-error nnmaildir--cur-server)
	      (concat "No such group: " gname))
	(throw 'return nil))
      (setq gname (nnmaildir--grp-name group)
	    pgname (nnmaildir--pgname nnmaildir--cur-server gname)
	    nlist (nnmaildir--grp-lists group)
	    flist (nnmaildir--lists-flist nlist)
	    nlist (nnmaildir--lists-nlist nlist))
      (if nlist nil
	(gnus-info-set-read info nil)
	(gnus-info-set-marks info nil 'extend)
	(throw 'return info))
      (setq old-marks (cons 'read (gnus-info-read info))
	    old-marks (cons old-marks (gnus-info-marks info))
	    last (nnmaildir--nlist-last-num nlist)
	    always-marks (nnmaildir--param pgname 'always-marks)
	    never-marks (nnmaildir--param pgname 'never-marks)
	    dir (nnmaildir--srv-dir nnmaildir--cur-server)
	    dir (nnmaildir--srvgrp-dir dir gname)
	    dir (nnmaildir--nndir dir)
	    dir (nnmaildir--marks-dir dir)
            ls (nnmaildir--group-ls nnmaildir--cur-server pgname)
	    markdirs (funcall ls dir nil "\\`[^.]" 'nosort)
	    num (length markdirs)
	    new-mmth 1)
      (while (<= new-mmth num) (setq new-mmth (* 2 new-mmth)))
      (if (/= new-mmth 1) (setq new-mmth (1- new-mmth)))
      (setq new-mmth (make-vector new-mmth 0)
	    old-mmth (nnmaildir--grp-mmth group))
      (while markdirs
	(setq mark (car markdirs) markdirs (cdr markdirs)
	      articles (nnmaildir--subdir dir mark)
	      mark-sym (intern mark)
	      ranges nil)
	(catch 'got-ranges
	  (if (memq mark-sym never-marks) (throw 'got-ranges nil))
	  (when (memq mark-sym always-marks)
	    (setq ranges (list (cons 1 last)))
	    (throw 'got-ranges nil))
	  (setq mtime (nth 5 (file-attributes articles)))
	  (set (intern mark new-mmth) mtime)
	  (when (equal mtime (symbol-value (intern-soft mark old-mmth)))
	    (setq ranges (assq mark-sym old-marks))
	    (if ranges (setq ranges (cdr ranges)))
	    (throw 'got-ranges nil))
	  (setq articles (funcall ls articles nil "\\`[^.]" 'nosort))
	  (while articles
	    (setq article (car articles) articles (cdr articles)
		  article (nnmaildir--flist-art flist article))
	    (if article
		(setq num (nnmaildir--art-num article)
		      ranges (gnus-add-to-range ranges (list num))))))
	(if (eq mark-sym 'read) (setq read ranges)
	  (if ranges (setq marks (cons (cons mark-sym ranges) marks)))))
      (gnus-info-set-read info read)
      (gnus-info-set-marks info marks 'extend)
      (setf (nnmaildir--grp-mmth group) new-mmth)
      info)))

(defun nnmaildir-request-group (gname &optional server fast)
  (let ((group (nnmaildir--prepare server gname))
	ct-min deactivate-mark)
    (nnmaildir--with-nntp-buffer
      (erase-buffer)
      (catch 'return
	(if group nil
	  (insert "411 no such news group\n")
	  (setf (nnmaildir--srv-error nnmaildir--cur-server)
		(concat "No such group: " gname))
	  (throw 'return nil))
	(setf (nnmaildir--srv-curgrp nnmaildir--cur-server) group)
	(if fast (throw 'return t))
	(setq ct-min (nnmaildir--article-count group))
	(insert "211 ")
	(princ (car ct-min) nntp-server-buffer)
	(insert " ")
	(princ (cdr ct-min) nntp-server-buffer)
	(insert " ")
	(princ (nnmaildir--nlist-last-num
		(nnmaildir--lists-nlist
		 (nnmaildir--grp-lists group)))
	       nntp-server-buffer)
	(insert " " gname "\n")
	t))))

(defun nnmaildir-request-create-group (gname &optional server args)
  (nnmaildir--prepare server nil)
  (catch 'return
    (let ((create-dir (nnmaildir--srv-create-dir nnmaildir--cur-server))
	  srv-dir dir groups)
      (when (zerop (length gname))
	(setf (nnmaildir--srv-error nnmaildir--cur-server)
	      "Invalid (empty) group name")
	(throw 'return nil))
      (when (eq (aref "." 0) (aref gname 0))
	(setf (nnmaildir--srv-error nnmaildir--cur-server)
	      "Group names may not start with \".\"")
	(throw 'return nil))
      (when (save-match-data (string-match "[\0/\t]" gname))
	(setf (nnmaildir--srv-error nnmaildir--cur-server)
	      (concat "Illegal characters (null, tab, or /) in group name: "
		      gname))
	(throw 'return nil))
      (setq groups (nnmaildir--srv-groups nnmaildir--cur-server))
      (when (intern-soft gname groups)
	(setf (nnmaildir--srv-error nnmaildir--cur-server)
	      (concat "Group already exists: " gname))
	(throw 'return nil))
      (setq srv-dir (nnmaildir--srv-dir nnmaildir--cur-server))
      (if (file-name-absolute-p create-dir)
	  (setq dir (expand-file-name create-dir))
	(setq dir srv-dir
	      dir (file-truename dir)
	      dir (concat dir create-dir)))
      (setq dir (nnmaildir--subdir (file-name-as-directory dir) gname))
      (nnmaildir--mkdir dir)
      (nnmaildir--mkdir (nnmaildir--tmp dir))
      (nnmaildir--mkdir (nnmaildir--new dir))
      (nnmaildir--mkdir (nnmaildir--cur dir))
      (setq create-dir (file-name-as-directory create-dir))
      (make-symbolic-link (concat create-dir gname) (concat srv-dir gname))
      (nnmaildir-request-scan 'find-new-groups))))

(defun nnmaildir-request-rename-group (gname new-name &optional server)
  (let ((group (nnmaildir--prepare server gname))
	(coding-system-for-write nnheader-file-coding-system)
	(buffer-file-coding-system nil)
	(file-coding-system-alist nil)
	srv-dir x groups)
    (catch 'return
      (if group nil
	(setf (nnmaildir--srv-error nnmaildir--cur-server)
	      (concat "No such group: " gname))
	(throw 'return nil))
      (when (zerop (length new-name))
	(setf (nnmaildir--srv-error nnmaildir--cur-server)
	      "Invalid (empty) group name")
	(throw 'return nil))
      (when (eq (aref "." 0) (aref new-name 0))
	(setf (nnmaildir--srv-error nnmaildir--cur-server)
	      "Group names may not start with \".\"")
	(throw 'return nil))
      (when (save-match-data (string-match "[\0/\t]" new-name))
	(setf (nnmaildir--srv-error nnmaildir--cur-server)
	      (concat "Illegal characters (null, tab, or /) in group name: "
		      new-name))
	(throw 'return nil))
      (if (string-equal gname new-name) (throw 'return t))
      (when (intern-soft new-name
			 (nnmaildir--srv-groups nnmaildir--cur-server))
	(setf (nnmaildir--srv-error nnmaildir--cur-server)
	      (concat "Group already exists: " new-name))
	(throw 'return nil))
      (setq srv-dir (nnmaildir--srv-dir nnmaildir--cur-server))
      (condition-case err
	  (rename-file (concat srv-dir gname)
		       (concat srv-dir new-name))
	(error
	 (setf (nnmaildir--srv-error nnmaildir--cur-server)
	       (concat "Error renaming link: " (prin1-to-string err)))
	 (throw 'return nil)))
      (setq x (nnmaildir--srv-groups nnmaildir--cur-server)
	    groups (make-vector (length x) 0))
      (mapatoms (lambda (sym)
		  (if (eq (symbol-value sym) group) nil
		    (set (intern (symbol-name sym) groups)
			 (symbol-value sym))))
		x)
      (setq group (copy-sequence group))
      (setf (nnmaildir--grp-name group) new-name)
      (set (intern new-name groups) group)
      (setf (nnmaildir--srv-groups nnmaildir--cur-server) groups)
      t)))

(defun nnmaildir-request-delete-group (gname force &optional server)
  (let ((group (nnmaildir--prepare server gname))
	pgname grp-dir dir dirs files ls deactivate-mark)
    (catch 'return
      (if group nil
	(setf (nnmaildir--srv-error nnmaildir--cur-server)
	      (concat "No such group: " gname))
	(throw 'return nil))
      (if (eq group (nnmaildir--srv-curgrp nnmaildir--cur-server))
	  (setf (nnmaildir--srv-curgrp nnmaildir--cur-server) nil))
      (setq gname (nnmaildir--grp-name group)
	    pgname (nnmaildir--pgname nnmaildir--cur-server gname))
      (unintern gname (nnmaildir--srv-groups nnmaildir--cur-server))
      (setq grp-dir (nnmaildir--srv-dir nnmaildir--cur-server)
	    grp-dir (nnmaildir--srvgrp-dir grp-dir gname))
      (if (not force) (setq grp-dir (directory-file-name grp-dir))
	(if (nnmaildir--param pgname 'read-only)
	    (progn (delete-directory  (nnmaildir--tmp grp-dir))
		   (nnmaildir--unlink (nnmaildir--new grp-dir))
		   (delete-directory  (nnmaildir--cur grp-dir)))
	  (nnmaildir--with-work-buffer
	    (erase-buffer)
            (setq ls (nnmaildir--group-ls nnmaildir--cur-server pgname)
		  files (funcall ls (nnmaildir--tmp grp-dir) 'full "\\`[^.]"
				 'nosort))
	    (while files
	      (delete-file (car files))
	      (setq files (cdr files)))
	    (delete-directory (nnmaildir--tmp grp-dir))
	    (setq files (funcall ls (nnmaildir--new grp-dir) 'full "\\`[^.]"
				 'nosort))
	    (while files
	      (delete-file (car files))
	      (setq files (cdr files)))
	    (delete-directory (nnmaildir--new grp-dir))
	    (setq files (funcall ls (nnmaildir--cur grp-dir) 'full "\\`[^.]"
				 'nosort))
	    (while files
	      (delete-file (car files))
	      (setq files (cdr files)))
	    (delete-directory (nnmaildir--cur grp-dir))))
	(setq dir (nnmaildir--nndir grp-dir)
	      dirs (cons (nnmaildir--nov-dir dir)
			 (funcall ls (nnmaildir--marks-dir dir) 'full "\\`[^.]"
				  'nosort)))
	(while dirs
	  (setq dir (car dirs) dirs (cdr dirs)
		files (funcall ls dir 'full "\\`[^.]" 'nosort))
	  (while files
	    (delete-file (car files))
	    (setq files (cdr files)))
	  (delete-directory dir))
	(setq dir (nnmaildir--nndir grp-dir))
	(nnmaildir--unlink (concat dir "markfile"))
	(nnmaildir--unlink (concat dir "markfile{new}"))
	(delete-directory (nnmaildir--marks-dir dir))
	(delete-directory dir)
	(setq grp-dir (directory-file-name grp-dir)
	      dir (car (file-attributes grp-dir)))
	(if (eq (aref "/" 0) (aref dir 0)) nil
	  (setq dir (concat (file-truename
			     (nnmaildir--srv-dir nnmaildir--cur-server))
			    dir)))
	(delete-directory dir))
      (nnmaildir--unlink grp-dir)
      t)))

(defun nnmaildir-retrieve-headers (articles &optional gname server fetch-old)
  (let ((group (nnmaildir--prepare server gname))
	srv-dir dir nlist mlist article num stop nov nlist2 deactivate-mark)
    (catch 'return
      (if group nil
	(setf (nnmaildir--srv-error nnmaildir--cur-server)
	      (if gname (concat "No such group: " gname) "No current group"))
	(throw 'return nil))
      (nnmaildir--with-nntp-buffer
	(erase-buffer)
	(setq nlist (nnmaildir--grp-lists group)
	      mlist (nnmaildir--lists-mlist nlist)
	      nlist (nnmaildir--lists-nlist nlist)
	      gname (nnmaildir--grp-name group)
	      srv-dir (nnmaildir--srv-dir nnmaildir--cur-server)
	      dir (nnmaildir--srvgrp-dir srv-dir gname))
	(cond
	 ((null nlist))
	 ((and fetch-old (not (numberp fetch-old)))
	  (while nlist
	    (setq article (car nlist) nlist (cdr nlist)
		  nov (nnmaildir--update-nov nnmaildir--cur-server group
					     article))
	    (when nov
	      (nnmaildir--cache-nov group article nov)
	      (setq num (nnmaildir--art-num article))
	      (princ num nntp-server-buffer)
	      (insert "\t" (nnmaildir--nov-get-beg nov) "\t"
		      (nnmaildir--art-msgid article) "\t"
		      (nnmaildir--nov-get-mid nov) "\tXref: nnmaildir " gname
		      ":")
	      (princ num nntp-server-buffer)
	      (insert "\t" (nnmaildir--nov-get-end nov) "\n")
	      (goto-char (point-min)))))
	 ((null articles))
	 ((stringp (car articles))
	  (while articles
	    (setq article (car articles) articles (cdr articles)
		  article (nnmaildir--mlist-art mlist article))
	    (when (and article
		       (setq nov (nnmaildir--update-nov nnmaildir--cur-server
							group article)))
	      (nnmaildir--cache-nov group article nov)
	      (setq num (nnmaildir--art-num article))
	      (princ num nntp-server-buffer)
	      (insert "\t" (nnmaildir--nov-get-beg nov) "\t"
		      (nnmaildir--art-msgid article) "\t"
		      (nnmaildir--nov-get-mid nov) "\tXref: nnmaildir " gname
		      ":")
	      (princ num nntp-server-buffer)
	      (insert "\t" (nnmaildir--nov-get-end nov) "\n"))))
	 (t
	  (if fetch-old
	      ;; Assume the article range is sorted ascending
	      (setq stop (car articles)
		    num  (car (last articles))
		    stop (if (numberp stop) stop (car stop))
		    num  (if (numberp num)  num  (cdr num))
		    stop (- stop fetch-old)
		    stop (if (< stop 1) 1 stop)
		    articles (list (cons stop num))))
	  (while articles
	    (setq stop (car articles) articles (cdr articles))
	    (while (eq stop (car articles))
	      (setq articles (cdr articles)))
	    (if (numberp stop) (setq num stop)
	      (setq num (cdr stop) stop (car stop)))
	    (setq nlist2 (nthcdr (- (nnmaildir--art-num (car nlist)) num)
				 nlist))
	    (while (and nlist2
			(setq article (car nlist2)
			      num (nnmaildir--art-num article))
			(>= num stop))
	      (setq nlist2 (cdr nlist2)
		    nov (nnmaildir--update-nov nnmaildir--cur-server group
					       article))
	      (when nov
		(nnmaildir--cache-nov group article nov)
		(princ num nntp-server-buffer)
		(insert "\t" (nnmaildir--nov-get-beg nov) "\t"
			(nnmaildir--art-msgid article) "\t"
			(nnmaildir--nov-get-mid nov) "\tXref: nnmaildir " gname
			":")
		(princ num nntp-server-buffer)
		(insert "\t" (nnmaildir--nov-get-end nov) "\n")
		(goto-char (point-min)))))))
	(sort-numeric-fields 1 (point-min) (point-max))
	'nov))))

(defun nnmaildir-request-article (num-msgid &optional gname server to-buffer)
  (let ((group (nnmaildir--prepare server gname))
	(case-fold-search t)
	list article suffix dir pgname deactivate-mark)
    (catch 'return
      (if group nil
	(setf (nnmaildir--srv-error nnmaildir--cur-server)
	      (if gname (concat "No such group: " gname) "No current group"))
	(throw 'return nil))
      (setq list (nnmaildir--grp-lists group))
      (if (numberp num-msgid)
	  (setq list (nnmaildir--lists-nlist list)
		article (nnmaildir--nlist-art list num-msgid))
	(setq list (nnmaildir--lists-mlist list)
	      article (nnmaildir--mlist-art list num-msgid))
	(if article (setq num-msgid (nnmaildir--art-num article))
	  (catch 'found
	    (mapatoms
              (lambda (grp)
                (setq group (symbol-value grp)
                      list (nnmaildir--grp-lists group)
                      list (nnmaildir--lists-mlist list)
                      article (nnmaildir--mlist-art list num-msgid))
                (when article
                  (setq num-msgid (nnmaildir--art-num article))
                  (throw 'found nil)))
              (nnmaildir--srv-groups nnmaildir--cur-server)))))
      (if article nil
	(setf (nnmaildir--srv-error nnmaildir--cur-server) "No such article")
	(throw 'return nil))
      (if (stringp (setq suffix (nnmaildir--art-suffix article))) nil
	(setf (nnmaildir--srv-error nnmaildir--cur-server)
	      "Article has expired")
	(throw 'return nil))
      (setq gname (nnmaildir--grp-name group)
	    pgname (nnmaildir--pgname nnmaildir--cur-server gname)
	    dir (nnmaildir--srv-dir nnmaildir--cur-server)
	    dir (nnmaildir--srvgrp-dir dir gname)
	    group (if (nnmaildir--param pgname 'read-only)
		      (nnmaildir--new dir) (nnmaildir--cur dir))
	    nnmaildir-article-file-name (concat group
						(nnmaildir--art-prefix
						 article)
						suffix))
      (if (file-exists-p nnmaildir-article-file-name) nil
	(setf (nnmaildir--art-suffix article) 'expire)
	(setf (nnmaildir--art-nov    article) nil)
	(setf (nnmaildir--srv-error nnmaildir--cur-server)
	      "Article has expired")
	(throw 'return nil))
      (save-excursion
	(set-buffer (or to-buffer nntp-server-buffer))
	(erase-buffer)
	(nnheader-insert-file-contents nnmaildir-article-file-name))
      (cons gname num-msgid))))

(defun nnmaildir-request-post (&optional server)
  (let (message-required-mail-headers)
    (funcall message-send-mail-function)))

(defun nnmaildir-request-replace-article (article gname buffer)
  (let ((group (nnmaildir--prepare nil gname))
	(coding-system-for-write nnheader-file-coding-system)
	(buffer-file-coding-system nil)
	(file-coding-system-alist nil)
	file dir suffix tmpfile deactivate-mark)
    (catch 'return
      (if group nil
	(setf (nnmaildir--srv-error nnmaildir--cur-server)
	      (concat "No such group: " gname))
	(throw 'return nil))
      (when (nnmaildir--param (nnmaildir--pgname nnmaildir--cur-server gname)
			      'read-only)
	(setf (nnmaildir--srv-error nnmaildir--cur-server)
	      (concat "Read-only group: " group))
	(throw 'return nil))
      (setq dir (nnmaildir--srv-dir nnmaildir--cur-server)
	    dir (nnmaildir--srvgrp-dir dir gname)
	    file (nnmaildir--grp-lists group)
	    file (nnmaildir--lists-nlist file)
	    file (nnmaildir--nlist-art file article))
      (if (and file (stringp (setq suffix (nnmaildir--art-suffix file))))
	  nil
	(setf (nnmaildir--srv-error nnmaildir--cur-server)
	      (format "No such article: %d" article))
	(throw 'return nil))
      (save-excursion
	(set-buffer buffer)
	(setq article file
	      file (nnmaildir--art-prefix article)
	      tmpfile (concat (nnmaildir--tmp dir) file))
	(when (file-exists-p tmpfile)
	  (setf (nnmaildir--srv-error nnmaildir--cur-server)
		(concat "File exists: " tmpfile))
	  (throw 'return nil))
	(write-region (point-min) (point-max) tmpfile nil 'no-message nil
		      'confirm-overwrite)) ;; error would be preferred :(
      (unix-sync) ;; no fsync :(
      (rename-file tmpfile (concat (nnmaildir--cur dir) file suffix) 'replace)
      t)))

(defun nnmaildir-request-move-article (article gname server accept-form
					       &optional last)
  (let ((group (nnmaildir--prepare server gname))
	pgname list suffix result nnmaildir--file deactivate-mark)
    (catch 'return
      (if group nil
	(setf (nnmaildir--srv-error nnmaildir--cur-server)
	      (concat "No such group: " gname))
	(throw 'return nil))
      (setq gname (nnmaildir--grp-name group)
	    pgname (nnmaildir--pgname nnmaildir--cur-server gname)
	    list (nnmaildir--grp-lists group)
	    list (nnmaildir--lists-nlist list)
	    article (nnmaildir--nlist-art list article))
      (if article nil
	(setf (nnmaildir--srv-error nnmaildir--cur-server) "No such article")
	(throw 'return nil))
      (if (stringp (setq suffix (nnmaildir--art-suffix article))) nil
	(setf (nnmaildir--srv-error nnmaildir--cur-server)
	      "Article has expired")
	(throw 'return nil))
      (setq nnmaildir--file (nnmaildir--srv-dir nnmaildir--cur-server)
	    nnmaildir--file (nnmaildir--srvgrp-dir nnmaildir--file gname)
	    nnmaildir--file (if (nnmaildir--param pgname 'read-only)
				(nnmaildir--new nnmaildir--file)
			      (nnmaildir--cur nnmaildir--file))
	    nnmaildir--file (concat nnmaildir--file
				    (nnmaildir--art-prefix article)
				    suffix))
      (if (file-exists-p nnmaildir--file) nil
	(setf (nnmaildir--art-suffix article) 'expire)
	(setf (nnmaildir--art-nov    article) nil)
	(setf (nnmaildir--srv-error nnmaildir--cur-server)
	      "Article has expired")
	(throw 'return nil))
      (nnmaildir--with-move-buffer
	(erase-buffer)
	(nnheader-insert-file-contents nnmaildir--file)
	(setq result (eval accept-form)))
      (if (or (null result) (nnmaildir--param pgname 'read-only)) nil
	(nnmaildir--unlink nnmaildir--file)
	(setf (nnmaildir--art-suffix article) 'expire)
	(setf (nnmaildir--art-nov    article) nil))
      result)))

(defun nnmaildir-request-accept-article (gname &optional server last)
  (let ((group (nnmaildir--prepare server gname))
	(coding-system-for-write nnheader-file-coding-system)
	(buffer-file-coding-system nil)
	(file-coding-system-alist nil)
	srv-dir dir file tmpfile curfile 24h num article)
    (catch 'return
      (if group nil
	(setf (nnmaildir--srv-error nnmaildir--cur-server)
	      (concat "No such group: " gname))
	(throw 'return nil))
      (setq gname (nnmaildir--grp-name group))
      (when (nnmaildir--param (nnmaildir--pgname nnmaildir--cur-server gname)
			      'read-only)
	(setf (nnmaildir--srv-error nnmaildir--cur-server)
	      (concat "Read-only group: " gname))
	(throw 'return nil))
      (setq srv-dir (nnmaildir--srv-dir nnmaildir--cur-server)
	    dir (nnmaildir--srvgrp-dir srv-dir gname)
	    file (format-time-string "%s" nil))
      (if (string-equal nnmaildir--delivery-time file) nil
	(setq nnmaildir--delivery-time file
	      nnmaildir--delivery-ct 0))
      (setq file (concat file "." nnmaildir--delivery-pid))
      (if (zerop nnmaildir--delivery-ct) nil
	(setq file (concat file "_"
			   (number-to-string nnmaildir--delivery-ct))))
      (setq file (concat file "." (system-name))
	    tmpfile (concat (nnmaildir--tmp dir) file)
	    curfile (concat (nnmaildir--cur dir) file ":2,"))
      (when (file-exists-p tmpfile)
	(setf (nnmaildir--srv-error nnmaildir--cur-server)
	      (concat "File exists: " tmpfile))
	(throw 'return nil))
      (when (file-exists-p curfile)
	(setf (nnmaildir--srv-error nnmaildir--cur-server)
	      (concat "File exists: " curfile))
	(throw 'return nil))
      (setq nnmaildir--delivery-ct (1+ nnmaildir--delivery-ct)
	    24h (run-with-timer 86400 nil
				(lambda ()
				  (nnmaildir--unlink tmpfile)
				  (setf (nnmaildir--srv-error
					  nnmaildir--cur-server)
					"24-hour timer expired")
				  (throw 'return nil))))
      (condition-case nil
	  (add-name-to-file nnmaildir--file tmpfile)
	(error
	 (write-region (point-min) (point-max) tmpfile nil 'no-message nil
		       'confirm-overwrite) ;; error would be preferred :(
	 (unix-sync))) ;; no fsync :(
      (cancel-timer 24h)
      (condition-case err
	  (add-name-to-file tmpfile curfile)
	(error
	 (setf (nnmaildir--srv-error nnmaildir--cur-server)
	       (concat "Error linking: " (prin1-to-string err)))
	 (nnmaildir--unlink tmpfile)
	 (throw 'return nil)))
      (nnmaildir--unlink tmpfile)
      (setq num (nnmaildir--grp-lists group)
	    num (nnmaildir--lists-nlist num)
	    num (1+ (nnmaildir--nlist-last-num num))
	    article (make-nnmaildir--art :prefix file :suffix ":2," :num num))
      (if (nnmaildir--grp-add-art nnmaildir--cur-server group article)
	  (cons gname num)))))

(defun nnmaildir-save-mail (group-art)
  (catch 'return
    (if group-art nil
      (throw 'return nil))
    (let ((ret group-art)
	  ga gname x groups nnmaildir--file deactivate-mark)
      (save-excursion
	(goto-char (point-min))
	(save-match-data
	  (while (looking-at "From ")
	    (replace-match "X-From-Line: ")
	    (forward-line 1))))
      (setq groups (nnmaildir--srv-groups nnmaildir--cur-server)
	    ga (car group-art) group-art (cdr group-art)
	    gname (car ga))
      (or (intern-soft gname groups)
	  (nnmaildir-request-create-group gname)
	  (throw 'return nil)) ;; not that nnmail bothers to check :(
      (if (nnmaildir-request-accept-article gname) nil
	(throw 'return nil))
      (setq x (nnmaildir--prepare nil gname)
	    nnmaildir--file (nnmaildir--srv-dir nnmaildir--cur-server)
	    nnmaildir--file (nnmaildir--subdir nnmaildir--file
                                               (nnmaildir--grp-name x))
	    x (nnmaildir--grp-lists x)
	    x (nnmaildir--lists-nlist x)
	    x (car x)
	    nnmaildir--file (concat nnmaildir--file
				    (nnmaildir--art-prefix x)
				    (nnmaildir--art-suffix x)))
      (while group-art
	(setq ga (car group-art) group-art (cdr group-art)
	      gname (car ga))
	(if (and (or (intern-soft gname groups)
		     (nnmaildir-request-create-group gname))
		 (nnmaildir-request-accept-article gname)) nil
	  (setq ret (delq ga ret)))) ;; We'll still try the other groups
      ret)))

(defun nnmaildir-active-number (group)
  (let ((x (nnmaildir--prepare nil group)))
    (catch 'return
      (if x nil
	(setf (nnmaildir--srv-error nnmaildir--cur-server)
	      (concat "No such group: " group))
	(throw 'return nil))
      (setq x (nnmaildir--grp-lists x)
	    x (nnmaildir--lists-nlist x))
      (if x
	  (setq x (car x)
		x (nnmaildir--art-num x)
		x (1+ x))
	1))))

(defun nnmaildir-request-expire-articles (ranges &optional gname server force)
  (let ((no-force (not force))
	(group (nnmaildir--prepare server gname))
	pgname time boundary time-iter bound-iter high low target dir nlist
	stop number article didnt suffix nnmaildir--file
	nnmaildir-article-file-name deactivate-mark)
    (catch 'return
      (if group nil
	(setf (nnmaildir--srv-error nnmaildir--cur-server)
	      (if gname (concat "No such group: " gname) "No current group"))
	(throw 'return (gnus-uncompress-range ranges)))
      (setq gname (nnmaildir--grp-name group)
	    pgname (nnmaildir--pgname nnmaildir--cur-server gname))
      (if (nnmaildir--param pgname 'read-only)
	  (throw 'return (gnus-uncompress-range ranges)))
      (setq time (or (nnmaildir--param pgname 'expire-age)
		     (* 86400 ;; seconds per day
			(or (and nnmail-expiry-wait-function
				 (funcall nnmail-expiry-wait-function gname))
			    nnmail-expiry-wait))))
      (if (or force (integerp time)) nil
	(throw 'return (gnus-uncompress-range ranges)))
      (setq boundary (current-time)
	    high (- (car boundary) (/ time 65536))
	    low (- (cadr boundary) (% time 65536)))
      (if (< low 0)
	  (setq low (+ low 65536)
		high (1- high)))
      (setcar (cdr boundary) low)
      (setcar boundary high)
      (setq dir (nnmaildir--srv-dir nnmaildir--cur-server)
	    dir (nnmaildir--srvgrp-dir dir gname)
	    dir (nnmaildir--cur dir)
	    nlist (nnmaildir--grp-lists group)
	    nlist (nnmaildir--lists-nlist nlist)
	    ranges (reverse ranges))
      (nnmaildir--with-move-buffer
	(while ranges
	  (setq number (car ranges) ranges (cdr ranges))
	  (while (eq number (car ranges))
	    (setq ranges (cdr ranges)))
	  (if (numberp number) (setq stop number)
	    (setq stop (car number) number (cdr number)))
	  (setq nlist (nthcdr (- (nnmaildir--art-num (car nlist)) number)
			      nlist))
	  (while (and nlist
		      (setq article (car nlist)
			    number (nnmaildir--art-num article))
		      (>= number stop))
	    (setq nlist (cdr nlist)
		  suffix (nnmaildir--art-suffix article))
	    (catch 'continue
	      (if (stringp suffix) nil
		(setf (nnmaildir--art-suffix article) 'expire)
		(setf (nnmaildir--art-nov    article) nil)
		(throw 'continue nil))
	      (setq nnmaildir--file (nnmaildir--art-prefix article)
		    nnmaildir--file (concat dir nnmaildir--file suffix)
		    time (file-attributes nnmaildir--file))
	      (if time nil
		(setf (nnmaildir--art-suffix article) 'expire)
		(setf (nnmaildir--art-nov    article) nil)
		(throw 'continue nil))
	      (setq time (nth 5 time)
		    time-iter time
		    bound-iter boundary)
	      (if (and no-force
		       (progn
			 (while (and bound-iter time-iter
				     (= (car bound-iter) (car time-iter)))
			   (setq bound-iter (cdr bound-iter)
				 time-iter (cdr time-iter)))
			 (and bound-iter time-iter
			      (car-less-than-car bound-iter time-iter))))
		  (setq didnt (cons number didnt))
		(save-excursion
		  (setq nnmaildir-article-file-name nnmaildir--file
			target (nnmaildir--param pgname 'expire-group)))
		(when (and (stringp target)
			   (not (string-equal target pgname))) ;; Move it.
		  (erase-buffer)
		  (nnheader-insert-file-contents nnmaildir--file)
		  (gnus-request-accept-article target nil nil 'no-encode))
		(if (equal target pgname)
		    (setq didnt (cons number didnt)) ;; Leave it here.
		  (nnmaildir--unlink nnmaildir--file)
		  (setf (nnmaildir--art-suffix article) 'expire)
		  (setf (nnmaildir--art-nov    article) nil))))))
	(erase-buffer))
      didnt)))

(defun nnmaildir-request-set-mark (gname actions &optional server)
  (let ((group (nnmaildir--prepare server gname))
	(coding-system-for-write nnheader-file-coding-system)
	(buffer-file-coding-system nil)
	(file-coding-system-alist nil)
	del-mark add-marks marksdir markfile action group-nlist nlist ranges
	begin end article all-marks todo-marks did-marks marks form mdir mfile
	pgname ls markfilenew deactivate-mark)
    (setq del-mark
	  (lambda ()
	    (setq mfile (nnmaildir--subdir marksdir (symbol-name (car marks)))
		  mfile (concat mfile (nnmaildir--art-prefix article)))
	    (nnmaildir--unlink mfile))
	  add-marks
	  (lambda ()
	    (while marks
	      (setq mdir (nnmaildir--subdir marksdir (symbol-name (car marks)))
		    mfile (concat mdir (nnmaildir--art-prefix article)))
	      (if (memq (car marks) did-marks) nil
		(nnmaildir--mkdir mdir)
		(setq did-marks (cons (car marks) did-marks)))
	      (if (file-exists-p mfile) nil
		(condition-case nil
		    (add-name-to-file markfile mfile)
		  (file-error
		   (if (file-exists-p mfile) nil
                     ;; too many links, maybe
		     (write-region "" nil markfilenew nil 'no-message)
		     (add-name-to-file markfilenew mfile 'ok-if-already-exists)
                     (rename-file markfilenew markfile 'replace)))))
	      (setq marks (cdr marks)))))
    (catch 'return
      (if group nil
	(setf (nnmaildir--srv-error nnmaildir--cur-server)
	      (concat "No such group: " gname))
	(while actions
	  (setq ranges (gnus-range-add ranges (caar actions))
		actions (cdr actions)))
	(throw 'return ranges))
      (setq group-nlist (nnmaildir--grp-lists group)
	    group-nlist (nnmaildir--lists-nlist group-nlist)
	    marksdir (nnmaildir--srv-dir nnmaildir--cur-server)
	    marksdir (nnmaildir--srvgrp-dir marksdir gname)
	    marksdir (nnmaildir--nndir marksdir)
	    markfile (concat marksdir "markfile")
	    markfilenew (concat markfile "{new}")
	    marksdir (nnmaildir--marks-dir marksdir)
	    gname (nnmaildir--grp-name group)
            pgname (nnmaildir--pgname nnmaildir--cur-server gname)
            ls (nnmaildir--group-ls nnmaildir--cur-server pgname)
	    all-marks (funcall ls marksdir nil "\\`[^.]" 'nosort)
	    marks all-marks)
      (while marks
	(setcar marks (intern (car marks)))
	(setq marks (cdr marks)))
      (while actions
	(setq action (car actions) actions (cdr actions)
	      nlist group-nlist
	      ranges (car action)
	      todo-marks (caddr action)
	      marks todo-marks)
	(while marks
	  (if (memq (car marks) all-marks) nil
	    (setq all-marks (cons (car marks) all-marks)))
	  (setq marks (cdr marks)))
	(setq form
	      (cond
	       ((eq 'del (cadr action))
		'(while marks
		   (funcall del-mark)
		   (setq marks (cdr marks))))
	       ((eq 'add (cadr action)) '(funcall add-marks))
	       (t
		'(progn
		   (funcall add-marks)
		   (setq marks all-marks)
		   (while marks
		     (if (memq (car marks) todo-marks) nil
		       (funcall del-mark))
		     (setq marks (cdr marks)))))))
	(if (numberp (cdr ranges)) (setq ranges (list ranges))
	  (setq ranges (reverse ranges)))
	(while ranges
	  (setq begin (car ranges) ranges (cdr ranges))
	  (while (eq begin (car ranges))
	    (setq ranges (cdr ranges)))
	  (if (numberp begin) (setq end begin)
	    (setq end (cdr begin) begin (car begin)))
	  (setq nlist (nthcdr (- (nnmaildir--art-num (car nlist)) end)
			      nlist))
	  (while (and nlist
		      (setq article (car nlist))
		      (>= (nnmaildir--art-num article) begin))
	    (setq nlist (cdr nlist))
	    (when (stringp (nnmaildir--art-suffix article))
	      (setq marks todo-marks)
	      (eval form)))))
      nil)))

(defun nnmaildir-close-group (group &optional server)
  t)

(defun nnmaildir-close-server (&optional server)
  (let (flist ls dirs dir files file x)
    (nnmaildir--prepare server nil)
    (setq server nnmaildir--cur-server)
    (when server
      (setq nnmaildir--cur-server nil)
      (save-match-data
	(mapatoms
          (lambda (group)
            (setq x (nnmaildir--pgname server (symbol-name group))
                  group (symbol-value group)
                  ls (nnmaildir--group-ls server x)
                  dir (nnmaildir--srv-dir server)
                  dir (nnmaildir--srvgrp-dir dir (nnmaildir--grp-name group))
                  x (nnmaildir--param x 'read-only)
                  x (if x (nnmaildir--new dir) (nnmaildir--cur dir))
                  files (funcall ls x nil "\\`[^.]" 'nosort)
                  x (length files)
                  flist 1)
            (while (<= flist x) (setq flist (* 2 flist)))
            (if (/= flist 1) (setq flist (1- flist)))
            (setq flist (make-vector flist 0))
            (while files
              (setq file (car files) files (cdr files))
              (string-match "\\`\\([^:]*\\)\\(:.*\\)?\\'" file)
              (intern (match-string 1 file) flist))
            (setq dir (nnmaildir--nndir dir)
                  dirs (cons (nnmaildir--nov-dir dir)
                             (funcall ls (nnmaildir--marks-dir dir) 'full
                                      "\\`[^.]" 'nosort)))
            (while dirs
              (setq dir (car dirs) dirs (cdr dirs)
                    files (funcall ls dir nil "\\`[^.]" 'nosort)
                    dir (file-name-as-directory dir))
              (while files
                (setq file (car files) files (cdr files))
                (if (intern-soft file flist) nil
                  (setq file (concat dir file))
                  (delete-file file)))))
          (nnmaildir--srv-groups server)))
      (unintern (nnmaildir--srv-address server) nnmaildir--servers)))
  t)

(defun nnmaildir-request-close ()
  (let (servers buffer)
    (mapatoms (lambda (server)
		(setq servers (cons (symbol-name server) servers)))
	      nnmaildir--servers)
    (while servers
      (nnmaildir-close-server (car servers))
      (setq servers (cdr servers)))
    (setq buffer (get-buffer " *nnmaildir work*"))
    (if buffer (kill-buffer buffer))
    (setq buffer (get-buffer " *nnmaildir nov*"))
    (if buffer (kill-buffer buffer))
    (setq buffer (get-buffer " *nnmaildir move*"))
    (if buffer (kill-buffer buffer)))
  t)

(defun nnmaildir--edit-prep ()
  (let ((extras '(mapcar mapatoms))
        name)
    (mapatoms
      (lambda (sym)
        (when (or (memq sym extras)
		  (and (fboundp sym)
		       (setq name (symbol-name sym))
		       (>= (length name) 10)
		       (or (string-equal "nnmaildir-" (substring name 0 10))
			   (and (>= (length name) 15)
				(string-equal "make-nnmaildir-"
					      (substring name 0 15))))))
          (put sym 'lisp-indent-function 0))))
    'done))

(provide 'nnmaildir)

;; Local Variables:
;; indent-tabs-mode: t
;; fill-column: 77
;; eval: (progn (require 'nnmaildir) (nnmaildir--edit-prep))
;; End:

;;; nnmaildir.el ends here
