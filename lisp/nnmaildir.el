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
;;   backend interface, which would have other advantages.
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

;; A server is a vector:
["server-name"
 select-method
 "/expanded/path/to/directory/containing/symlinks/to/maildirs/"
 directory-files-function
 group-name-transformation-function
 ;; An obarray containing symbols whose names are group names and whose values
 ;; are groups:
 group-hash
 ;; A group which has not necessarily been added to the group hash, or nil:
 tmp-group
 current-group ;; or nil
 "Last error message, or nil"
 directory-modtime
 get-new-mail-p ;; Should we split mail from mail-sources?
 "new/group/creation/directory"]

;; A group is a vector:
["group.name"
 "prefixed:group.name"
 ;; Modification times of the "new", and "cur" directories:
 new-modtime
 cur-modtime
 ;; A vector containing lists of articles:
 [;; A list of articles, with article numbers in descending order, ending with
  ;; article 1:
  article-list
  ;; An obarray containing symbols whose names are filename prefixes and whose
  ;; values are articles:
  file-hash
  ;; Same as above, but keyed on Message-ID:
  msgid-hash
  ;; An article which has not necessarily been added to the file and msgid
  ;; hashes, or nil:
  tmp-article]
 ;; A vector containing nil, or articles with NOV data:
 nov-cache
 ;; The index of the next nov-cache entry to be replaced:
 nov-cache-index
 ;; An obarray containing symbols whose names are mark names and whose values
 ;; are modtimes of mark directories:
 mark-modtime-hash]

;; An article is a vector:
["file.name.prefix"
 ":2,suffix" ;; or 'expire if expired
 number
 "msgid"
 ;; A NOV data vector, or nil:
 ["subject\tfrom\tdate"
  "references\tchars\lines"
  "extra"
  article-file-modtime
  ;; The value of nnmail-extra-headers when this NOV data was parsed:
  (to in-reply-to)]]

(defmacro nnmaildir--srv-new () '(make-vector 11 nil))
(defmacro nnmaildir--srv-get-name       (server) `(aref ,server  0))
(defmacro nnmaildir--srv-get-method     (server) `(aref ,server  1))
(defmacro nnmaildir--srv-get-dir        (server) `(aref ,server  2))
(defmacro nnmaildir--srv-get-ls         (server) `(aref ,server  3))
(defmacro nnmaildir--srv-get-groups     (server) `(aref ,server  4))
(defmacro nnmaildir--srv-get-curgrp     (server) `(aref ,server  6))
(defmacro nnmaildir--srv-get-error      (server) `(aref ,server  7))
(defmacro nnmaildir--srv-get-mtime      (server) `(aref ,server  8))
(defmacro nnmaildir--srv-get-gnm        (server) `(aref ,server  9))
(defmacro nnmaildir--srv-get-create-dir (server) `(aref ,server 10))
(defmacro nnmaildir--srv-set-name       (server val) `(aset ,server  0 ,val))
(defmacro nnmaildir--srv-set-method     (server val) `(aset ,server  1 ,val))
(defmacro nnmaildir--srv-set-dir        (server val) `(aset ,server  2 ,val))
(defmacro nnmaildir--srv-set-ls         (server val) `(aset ,server  3 ,val))
(defmacro nnmaildir--srv-set-groups     (server val) `(aset ,server  4 ,val))
(defmacro nnmaildir--srv-set-curgrp     (server val) `(aset ,server  6 ,val))
(defmacro nnmaildir--srv-set-error      (server val) `(aset ,server  7 ,val))
(defmacro nnmaildir--srv-set-mtime      (server val) `(aset ,server  8 ,val))
(defmacro nnmaildir--srv-set-gnm        (server val) `(aset ,server  9 ,val))
(defmacro nnmaildir--srv-set-create-dir (server val) `(aset ,server 10 ,val))

(defmacro nnmaildir--grp-new () '(make-vector 8 nil))
(defmacro nnmaildir--grp-get-name   (group) `(aref ,group 0))
(defmacro nnmaildir--grp-get-pname  (group) `(aref ,group 1))
(defmacro nnmaildir--grp-get-new    (group) `(aref ,group 2))
(defmacro nnmaildir--grp-get-cur    (group) `(aref ,group 3))
(defmacro nnmaildir--grp-get-lists  (group) `(aref ,group 4))
(defmacro nnmaildir--grp-get-cache  (group) `(aref ,group 5))
(defmacro nnmaildir--grp-get-index  (group) `(aref ,group 6))
(defmacro nnmaildir--grp-get-mmth   (group) `(aref ,group 7))
(defmacro nnmaildir--grp-set-name   (group val) `(aset ,group 0 ,val))
(defmacro nnmaildir--grp-set-pname  (group val) `(aset ,group 1 ,val))
(defmacro nnmaildir--grp-set-new    (group val) `(aset ,group 2 ,val))
(defmacro nnmaildir--grp-set-cur    (group val) `(aset ,group 3 ,val))
(defmacro nnmaildir--grp-set-lists  (group val) `(aset ,group 4 ,val))
(defmacro nnmaildir--grp-set-cache  (group val) `(aset ,group 5 ,val))
(defmacro nnmaildir--grp-set-index  (group val) `(aset ,group 6 ,val))
(defmacro nnmaildir--grp-set-mmth   (group val) `(aset ,group 7 ,val))

(defmacro nnmaildir--lists-new () '(make-vector 4 nil))
(defmacro nnmaildir--lists-get-nlist  (lists) `(aref ,lists 0))
(defmacro nnmaildir--lists-get-flist  (lists) `(aref ,lists 1))
(defmacro nnmaildir--lists-get-mlist  (lists) `(aref ,lists 2))
(defmacro nnmaildir--lists-get-tmpart (lists) `(aref ,lists 3))
(defmacro nnmaildir--lists-set-nlist  (lists val) `(aset ,lists 0 ,val))
(defmacro nnmaildir--lists-set-flist  (lists val) `(aset ,lists 1 ,val))
(defmacro nnmaildir--lists-set-mlist  (lists val) `(aset ,lists 2 ,val))
(defmacro nnmaildir--lists-set-tmpart (lists val) `(aset ,lists 3 ,val))

(defmacro nnmaildir--nlist-last-num (list)
  `(if ,list (nnmaildir--art-get-num (car ,list)) 0))
(defmacro nnmaildir--nlist-art (list num)
  `(and ,list
	(>= (nnmaildir--art-get-num (car ,list)) ,num)
	(nth (- (nnmaildir--art-get-num (car ,list)) ,num) ,list)))
(defmacro nnmaildir--flist-art (list file)
  `(symbol-value (intern-soft ,file ,list)))
(defmacro nnmaildir--mlist-art (list msgid)
  `(symbol-value (intern-soft ,msgid ,list)))

(defmacro nnmaildir--art-new () '(make-vector 5 nil))
(defmacro nnmaildir--art-get-prefix (article) `(aref ,article 0))
(defmacro nnmaildir--art-get-suffix (article) `(aref ,article 1))
(defmacro nnmaildir--art-get-num    (article) `(aref ,article 2))
(defmacro nnmaildir--art-get-msgid  (article) `(aref ,article 3))
(defmacro nnmaildir--art-get-nov    (article) `(aref ,article 4))
(defmacro nnmaildir--art-set-prefix (article val) `(aset ,article 0 ,val))
(defmacro nnmaildir--art-set-suffix (article val) `(aset ,article 1 ,val))
(defmacro nnmaildir--art-set-num    (article val) `(aset ,article 2 ,val))
(defmacro nnmaildir--art-set-msgid  (article val) `(aset ,article 3 ,val))
(defmacro nnmaildir--art-set-nov    (article val) `(aset ,article 4 ,val))

(defmacro nnmaildir--nov-new () '(make-vector 5 nil))
(defmacro nnmaildir--nov-get-beg   (nov) `(aref ,nov 0))
(defmacro nnmaildir--nov-get-mid   (nov) `(aref ,nov 1))
(defmacro nnmaildir--nov-get-end   (nov) `(aref ,nov 2))
(defmacro nnmaildir--nov-get-mtime (nov) `(aref ,nov 3))
(defmacro nnmaildir--nov-get-neh   (nov) `(aref ,nov 4))
(defmacro nnmaildir--nov-set-beg   (nov val) `(aset ,nov 0 ,val))
(defmacro nnmaildir--nov-set-mid   (nov val) `(aset ,nov 1 ,val))
(defmacro nnmaildir--nov-set-end   (nov val) `(aset ,nov 2 ,val))
(defmacro nnmaildir--nov-set-mtime (nov val) `(aset ,nov 3 ,val))
(defmacro nnmaildir--nov-set-neh   (nov val) `(aset ,nov 4 ,val))

(defmacro nnmaildir--subdir (dir subdir)
  `(file-name-as-directory (concat ,dir ,subdir)))
(defmacro nnmaildir--srv-grp-dir (srv-dir gname)
  `(nnmaildir--subdir ,srv-dir ,gname))
(defmacro nnmaildir--tmp (dir) `(nnmaildir--subdir ,dir "tmp"))
(defmacro nnmaildir--new (dir) `(nnmaildir--subdir ,dir "new"))
(defmacro nnmaildir--cur (dir) `(nnmaildir--subdir ,dir "cur"))
(defmacro nnmaildir--nndir (dir)
  `(nnmaildir--subdir ,dir ".nnmaildir"))
(defmacro nnmaildir--nov-dir (dir)
  `(nnmaildir--subdir ,dir "nov"))
(defmacro nnmaildir--marks-dir (dir)
  `(nnmaildir--subdir ,dir "marks"))

(defun nnmaildir--param (pgname param)
  (setq param
	(gnus-group-find-parameter pgname param 'allow-list)
	param (if (vectorp param) (aref param 0) param))
  (eval param))

(defmacro nnmaildir--unlink (file)
  `(if (file-attributes ,file) (delete-file ,file)))

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
      (setq groups (nnmaildir--srv-get-groups server))
      (if groups nil (throw 'return nil))
      (if (nnmaildir--srv-get-method server) nil
	(setq x (concat "nnmaildir:" (nnmaildir--srv-get-name server))
	      x (gnus-server-to-method x))
	(if x nil (throw 'return nil))
	(nnmaildir--srv-set-method server x))
      (if (null group)
	  (or (setq group (nnmaildir--srv-get-curgrp server))
	      (throw 'return nil))
	(setq group (intern-soft group groups))
	(if group nil (throw 'return nil))
	(setq group (symbol-value group)))
      group)))

(defun nnmaildir--update-nov (srv-dir group article)
  (let ((nnheader-file-coding-system 'binary)
	dir gname pgname msgdir prefix suffix file attr mtime novdir novfile
	nov msgid nov-beg nov-mid nov-end field pos extra val old-neh new-neh
	deactivate-mark)
    (catch 'return
      (setq suffix (nnmaildir--art-get-suffix article))
      (if (stringp suffix) nil
	(nnmaildir--art-set-nov article nil)
	(throw 'return nil))
      (setq gname (nnmaildir--grp-get-name group)
	    pgname (nnmaildir--grp-get-pname group)
	    dir (nnmaildir--srv-grp-dir srv-dir gname)
	    msgdir (if (nnmaildir--param pgname 'read-only)
		       (nnmaildir--new dir) (nnmaildir--cur dir))
	    prefix (nnmaildir--art-get-prefix article)
	    file (concat msgdir prefix suffix)
	    attr (file-attributes file))
      (if attr nil
	(nnmaildir--art-set-suffix article 'expire)
	(nnmaildir--art-set-nov article nil)
	(throw 'return nil))
      (setq mtime (nth 5 attr)
	    attr (nth 7 attr)
	    nov (nnmaildir--art-get-nov article)
	    novdir (nnmaildir--nov-dir (nnmaildir--nndir dir))
	    novfile (concat novdir prefix))
      (save-excursion
	(set-buffer (get-buffer-create " *nnmaildir nov*"))
	(when (file-exists-p novfile) ;; If not, force reparsing the message.
	  (if nov nil ;; It's already in memory.
	    ;; Else read the data from the NOV file.
	    (erase-buffer)
	    (nnheader-insert-file-contents novfile)
	    (setq nov (read (current-buffer)))
	    (nnmaildir--art-set-msgid article (car nov))
	    (setq nov (cadr nov)))
	  ;; If the NOV's modtime matches the file's current modtime,
	  ;; and it has the right length (i.e., it wasn't produced by
	  ;; a too-much older version of nnmaildir), then we may use
	  ;; this NOV data rather than parsing the message file,
	  ;; unless nnmail-extra-headers has been augmented since this
	  ;; data was last parsed.
	  (when (and (equal mtime (nnmaildir--nov-get-mtime nov))
		     (= (length nov) (length (nnmaildir--nov-new))))
	    ;; This NOV data is potentially up-to-date.
	    (setq old-neh (nnmaildir--nov-get-neh nov)
		  new-neh nnmail-extra-headers)
	    (if (equal new-neh old-neh) (throw 'return nov)) ;; Common case.
	    ;; They're not equal, but maybe the new is a subset of the old...
	    (if (null new-neh) (throw 'return nov))
	    (while new-neh
	      (if (memq (car new-neh) old-neh)
		  (progn
		    (setq new-neh (cdr new-neh))
		    (if new-neh nil (throw 'return nov)))
		(setq new-neh nil)))))
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
	(erase-buffer)
	(setq nov (nnmaildir--nov-new))
	(nnmaildir--nov-set-beg nov nov-beg)
	(nnmaildir--nov-set-mid nov nov-mid)
	(nnmaildir--nov-set-end nov nov-end)
	(nnmaildir--nov-set-mtime nov mtime)
	(nnmaildir--nov-set-neh nov (copy-sequence nnmail-extra-headers))
	(prin1 (list msgid nov) (current-buffer))
	(setq file (concat novfile ":"))
	(nnmaildir--unlink file)
	(write-region (point-min) (point-max) file nil 'no-message))
      (rename-file file novfile 'replace)
      (nnmaildir--art-set-msgid article msgid)
      nov)))

(defun nnmaildir--cache-nov (group article nov)
  (let ((cache (nnmaildir--grp-get-cache group))
	(index (nnmaildir--grp-get-index group))
	goner)
    (if (nnmaildir--art-get-nov article) nil
      (setq goner (aref cache index))
      (if goner (nnmaildir--art-set-nov goner nil))
      (aset cache index article)
      (nnmaildir--grp-set-index group (% (1+ index) (length cache))))
    (nnmaildir--art-set-nov article nov)))

(defun nnmaildir--grp-add-art (srv-dir group article)
  (let ((nov (nnmaildir--update-nov srv-dir group article))
	old-lists new-lists)
    (when nov
      (setq old-lists (nnmaildir--grp-get-lists group)
	    new-lists (nnmaildir--lists-new))
      (nnmaildir--lists-set-nlist
       new-lists (cons article (nnmaildir--lists-get-nlist old-lists)))
      (nnmaildir--lists-set-flist new-lists
				  (nnmaildir--lists-get-flist old-lists))
      (nnmaildir--lists-set-mlist new-lists
				  (nnmaildir--lists-get-mlist old-lists))
      (let ((inhibit-quit t))
        (nnmaildir--grp-set-lists group new-lists)
        (set (intern (nnmaildir--art-get-prefix article)
                     (nnmaildir--lists-get-flist new-lists))
             article)
        (set (intern (nnmaildir--art-get-msgid article)
                     (nnmaildir--lists-get-mlist new-lists))
             article))
      (nnmaildir--cache-nov group article nov)
      t)))

(defun nnmaildir--mkdir (dir)
  (or (file-exists-p (file-name-as-directory dir))
      (make-directory-internal (directory-file-name dir))))

(defun nnmaildir--article-count (group)
  (let ((ct 0)
	(min 1))
    (setq group (nnmaildir--grp-get-lists group)
	  group (nnmaildir--lists-get-nlist group))
    (while group
      (if (stringp (nnmaildir--art-get-suffix (car group)))
	  (setq ct (1+ ct)
		min (nnmaildir--art-get-num (car group))))
      (setq group (cdr group)))
    (cons ct min)))

(defun nnmaildir-article-number-to-file-name
  (number group-name server-address-string)
  (let ((group (nnmaildir--prepare server-address-string group-name))
	list article suffix dir filename)
    (catch 'return
      (if (null group)
	  ;; The given group or server does not exist.
	  (throw 'return nil))
      (setq list (nnmaildir--grp-get-lists group)
	    list (nnmaildir--lists-get-nlist list)
	    article (nnmaildir--nlist-art list number))
      (if (null article)
	  ;; The given article number does not exist in this group.
	  (throw 'return nil))
      (setq suffix (nnmaildir--art-get-suffix article))
      (if (not (stringp suffix))
	  ;; The article has expired.
	  (throw 'return nil))
      (setq dir (nnmaildir--srv-get-dir nnmaildir--cur-server)
	    dir (nnmaildir--srv-grp-dir dir group-name)
	    group (if (nnmaildir--param (nnmaildir--grp-get-pname group)
					'read-only)
		      (nnmaildir--new dir) (nnmaildir--cur dir))
	    filename (concat group (nnmaildir--art-get-prefix article) suffix))
      (if (file-exists-p filename)
	  filename
	;; The article disappeared out from under us.
	(nnmaildir--art-set-suffix article 'expire)
	(nnmaildir--art-set-nov article nil)
	nil))))

(defun nnmaildir-article-number-to-base-name
  (number group-name server-address-string)
  (let ((group (nnmaildir--prepare server-address-string group-name))
	list article suffix dir filename)
    (catch 'return
      (if (null group)
	  ;; The given group or server does not exist.
	  (throw 'return nil))
      (setq list (nnmaildir--grp-get-lists group)
	    list (nnmaildir--lists-get-nlist list)
	    article (nnmaildir--nlist-art list number))
      (if (null article)
	  ;; The given article number does not exist in this group.
	  (throw 'return nil))
      (setq suffix (nnmaildir--art-get-suffix article))
      (if (not (stringp suffix))
	  ;; The article has expired.
	  (throw 'return nil))
      (cons (nnmaildir--art-get-prefix article) suffix))))

(defun nnmaildir-base-name-to-article-number
  (base-name group-name server-address-string)
  (let ((group (nnmaildir--prepare server-address-string group-name))
	list article suffix dir filename)
    (catch 'return
      (if (null group)
	  ;; The given group or server does not exist.
	  (throw 'return nil))
      (setq list (nnmaildir--grp-get-lists group)
	    list (nnmaildir--lists-get-flist list)
	    article (nnmaildir--flist-art list base-name))
      (if (null article)
	  ;; The given article number does not exist in this group.
	  (throw 'return nil))
      (nnmaildir--art-get-num article))))

(defun nnmaildir-request-type (group &optional article)
  'mail)

(defun nnmaildir-status-message (&optional server)
  (nnmaildir--prepare server nil)
  (nnmaildir--srv-get-error nnmaildir--cur-server))

(defun nnmaildir-server-opened (&optional server)
  (and nnmaildir--cur-server
       (if server
	   (string-equal server
			 (nnmaildir--srv-get-name nnmaildir--cur-server))
	 t)
       (nnmaildir--srv-get-groups nnmaildir--cur-server)
       t))

(defun nnmaildir-open-server (server &optional defs)
  (let ((x server)
	dir size)
    (catch 'return
      (setq server (intern-soft x nnmaildir--servers))
      (if server
	  (and (setq server (symbol-value server))
	       (nnmaildir--srv-get-groups server)
	       (setq nnmaildir--cur-server server)
	       (throw 'return t))
	(setq server (nnmaildir--srv-new))
	(nnmaildir--srv-set-name server x)
        (let ((inhibit-quit t))
          (set (intern x nnmaildir--servers) server)))
      (setq dir (assq 'directory defs))
      (if dir nil
	(nnmaildir--srv-set-error
	 server "You must set \"directory\" in the select method")
	(throw 'return nil))
      (setq dir (cadr dir)
	    dir (eval dir)
	    dir (expand-file-name dir)
	    dir (file-name-as-directory dir))
      (if (file-exists-p dir) nil
	(nnmaildir--srv-set-error server (concat "No such directory: " dir))
	(throw 'return nil))
      (nnmaildir--srv-set-dir server dir)
      (setq x (assq 'directory-files defs))
      (if (null x)
	  (setq x (symbol-function (if nnheader-directory-files-is-safe
				       'directory-files
				     'nnheader-directory-files-safe)))
	(setq x (cadr x))
	(if (functionp x) nil
	  (nnmaildir--srv-set-error
	   server (concat "Not a function: " (prin1-to-string x)))
	  (throw 'return nil)))
      (nnmaildir--srv-set-ls server x)
      (setq x (funcall x dir nil "\\`[^.]" 'nosort)
	    x (length x)
	    size 1)
      (while (<= size x) (setq size (* 2 size)))
      (if (/= size 1) (setq size (1- size)))
      (and (setq x (assq 'get-new-mail defs))
	   (setq x (cdr x))
	   (car x)
	   (nnmaildir--srv-set-gnm server t)
	   (require 'nnmail))
      (setq x (assq 'create-directory defs))
      (when x
	(setq x (cadr x)
	      x (eval x))
	(nnmaildir--srv-set-create-dir server x))
      (nnmaildir--srv-set-groups server (make-vector size 0))
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
      (setq absdir (nnmaildir--srv-grp-dir srv-dir gname)
	    nndir (nnmaildir--nndir absdir))
      (if (file-attributes absdir) nil
	(nnmaildir--srv-set-error nnmaildir--cur-server
				  (concat "No such directory: " absdir))
	(throw 'return nil))
      (setq tdir (nnmaildir--tmp absdir)
	    ndir (nnmaildir--new absdir)
	    cdir (nnmaildir--cur absdir)
	    nattr (file-attributes ndir)
	    cattr (file-attributes cdir))
      (if (and (file-exists-p tdir) nattr cattr) nil
	(nnmaildir--srv-set-error nnmaildir--cur-server
				  (concat "Not a maildir: " absdir))
	(throw 'return nil))
      (setq group (nnmaildir--prepare nil gname))
      (if group
	  (setq isnew nil
		pgname (nnmaildir--grp-get-pname group))
	(setq isnew t
	      group (nnmaildir--grp-new)
	      pgname (gnus-group-prefixed-name gname method))
	(nnmaildir--grp-set-name group gname)
	(nnmaildir--grp-set-pname group pgname)
	(nnmaildir--grp-set-lists group (nnmaildir--lists-new))
	(nnmaildir--grp-set-index group 0)
	(nnmaildir--mkdir nndir)
	(nnmaildir--mkdir (nnmaildir--nov-dir   nndir))
	(nnmaildir--mkdir (nnmaildir--marks-dir nndir))
	(write-region "" nil (concat nndir "markfile") nil 'no-message))
      (setq read-only (nnmaildir--param pgname 'read-only)
	    ls (or (nnmaildir--param pgname 'directory-files) srv-ls))
      (if read-only nil
	(setq x (nth 11 (file-attributes tdir)))
	(if (and (= x (nth 11 nattr)) (= x (nth 11 cattr))) nil
	  (nnmaildir--srv-set-error nnmaildir--cur-server
				    (concat "Maildir spans filesystems: "
					    absdir))
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
      (if (equal nattr (nnmaildir--grp-get-new group))
	  (setq nattr nil))
      (if read-only (setq dir (and (or isnew nattr) ndir))
	(when (or isnew nattr)
	  (setq files (funcall ls ndir nil "\\`[^.]" 'nosort))
	  (while files
	    (setq file (car files) files (cdr files))
	    (rename-file (concat ndir file) (concat cdir file ":2,")))
	  (nnmaildir--grp-set-new group nattr))
	(setq cattr (file-attributes cdir)
	      cattr (nth 5 cattr))
	(if (equal cattr (nnmaildir--grp-get-cur group))
	    (setq cattr nil))
	(setq dir (and (or isnew cattr) cdir)))
      (if dir nil (throw 'return t))
      (setq files (funcall ls dir nil "\\`[^.]" 'nosort))
      (when isnew
	(setq x (length files)
	      num 1)
	(while (<= num x) (setq num (* 2 num)))
	(if (/= num 1) (setq num (1- num)))
	(setq x (nnmaildir--grp-get-lists group))
	(nnmaildir--lists-set-flist x (make-vector num 0))
	(nnmaildir--lists-set-mlist x (make-vector num 0))
	(nnmaildir--grp-set-mmth group (make-vector 1 0))
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
	(nnmaildir--grp-set-cache group (make-vector num nil))
        (let ((inhibit-quit t))
          (set (intern gname groups) group))
	(or scan-msgs (throw 'return t)))
      (setq flist (nnmaildir--grp-get-lists group)
	    num (nnmaildir--lists-get-nlist flist)
	    flist (nnmaildir--lists-get-flist flist)
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
	      x (nnmaildir--art-new))
	(nnmaildir--art-set-prefix x (car file))
	(nnmaildir--art-set-suffix x (cdr file))
	(nnmaildir--art-set-num x (1+ num))
	(if (nnmaildir--grp-add-art srv-dir group x)
	    (setq num (1+ num))))
      (if read-only (nnmaildir--grp-set-new group nattr)
	(nnmaildir--grp-set-cur group cattr)))
    t))

(defun nnmaildir-request-scan (&optional scan-group server)
  (let ((coding-system-for-write nnheader-file-coding-system)
	(buffer-file-coding-system nil)
	(file-coding-system-alist nil)
	(nnmaildir-get-new-mail t)
	(nnmaildir-group-alist nil)
	(nnmaildir-active-file nil)
	x srv-ls srv-dir method groups group dirs grp-dir seen deactivate-mark)
    (nnmaildir--prepare server nil)
    (setq srv-ls (nnmaildir--srv-get-ls nnmaildir--cur-server)
	  srv-dir (nnmaildir--srv-get-dir nnmaildir--cur-server)
	  method (nnmaildir--srv-get-method nnmaildir--cur-server)
	  groups (nnmaildir--srv-get-groups nnmaildir--cur-server))
    (save-excursion
      (set-buffer (get-buffer-create " *nnmaildir work*"))
      (save-match-data
	(if (stringp scan-group)
	    (if (nnmaildir--scan scan-group t groups method srv-dir srv-ls)
		(if (nnmaildir--srv-get-gnm nnmaildir--cur-server)
		    (nnmail-get-new-mail 'nnmaildir nil nil scan-group))
	      (unintern scan-group groups))
	  (setq x (nth 5 (file-attributes srv-dir)))
	  (if (equal x (nnmaildir--srv-get-mtime nnmaildir--cur-server))
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
	    (nnmaildir--srv-set-mtime nnmaildir--cur-server
				      (nth 5 (file-attributes srv-dir))))
	  (if (nnmaildir--srv-get-gnm nnmaildir--cur-server)
	      (nnmail-get-new-mail 'nnmaildir nil nil))))))
  t)

(defun nnmaildir-request-list (&optional server)
  (nnmaildir-request-scan 'find-new-groups server)
  (let (pgname ro ct-min deactivate-mark)
    (nnmaildir--prepare server nil)
    (save-excursion
      (set-buffer nntp-server-buffer)
      (erase-buffer)
      (mapatoms (lambda (group)
		  (setq group (symbol-value group)
			ro (nnmaildir--param (nnmaildir--grp-get-pname group)
					     'read-only)
			ct-min (nnmaildir--article-count group))
		  (insert (nnmaildir--grp-get-name group) " ")
                  (princ (nnmaildir--nlist-last-num
                           (nnmaildir--lists-get-nlist
                             (nnmaildir--grp-get-lists group)))
                         nntp-server-buffer)
		  (insert " ")
		  (princ (cdr ct-min) nntp-server-buffer)
		  (insert " " (if ro "n" "y") "\n"))
		(nnmaildir--srv-get-groups nnmaildir--cur-server))))
  t)

(defun nnmaildir-request-newgroups (date &optional server)
  (nnmaildir-request-list server))

(defun nnmaildir-retrieve-groups (groups &optional server)
  (let (gname group ct-min deactivate-mark)
    (nnmaildir--prepare server nil)
    (save-excursion
      (set-buffer nntp-server-buffer)
      (erase-buffer)
      (while groups
	(setq gname (car groups) groups (cdr groups))
	(nnmaildir-request-scan gname server)
	(setq group (nnmaildir--prepare nil gname))
	(if (null group) (insert "411 no such news group\n")
	  (setq ct-min (nnmaildir--article-count group))
	  (insert "211 ")
	  (princ (car ct-min) nntp-server-buffer)
	  (insert " ")
	  (princ (cdr ct-min) nntp-server-buffer)
	  (insert " ")
	  (princ (nnmaildir--nlist-last-num
		   (nnmaildir--lists-get-nlist
		     (nnmaildir--grp-get-lists group)))
		 nntp-server-buffer)
	  (insert " " gname "\n")))))
  'group)

(defun nnmaildir-request-update-info (gname info &optional server)
  (nnmaildir-request-scan gname server)
  (let ((group (nnmaildir--prepare server gname))
	srv-ls pgname nlist flist last always-marks never-marks old-marks
	dotfile num dir markdirs marks mark ranges articles article read end
	new-marks ls old-mmth new-mmth mtime mark-sym deactivate-mark)
    (catch 'return
      (if group nil
	(nnmaildir--srv-set-error nnmaildir--cur-server
				  (concat "No such group: " gname))
	(throw 'return nil))
      (setq srv-ls (nnmaildir--srv-get-ls nnmaildir--cur-server)
	    gname (nnmaildir--grp-get-name group)
	    pgname (nnmaildir--grp-get-pname group)
	    nlist (nnmaildir--grp-get-lists group)
	    flist (nnmaildir--lists-get-flist nlist)
	    nlist (nnmaildir--lists-get-nlist nlist))
      (if nlist nil
	(gnus-info-set-read info nil)
	(gnus-info-set-marks info nil 'extend)
	(throw 'return info))
      (setq old-marks (cons 'read (gnus-info-read info))
	    old-marks (cons old-marks (gnus-info-marks info))
	    last (nnmaildir--nlist-last-num nlist)
	    always-marks (nnmaildir--param pgname 'always-marks)
	    never-marks (nnmaildir--param pgname 'never-marks)
	    dir (nnmaildir--srv-get-dir nnmaildir--cur-server)
	    dir (nnmaildir--srv-grp-dir dir gname)
	    dir (nnmaildir--nndir dir)
	    dir (nnmaildir--marks-dir dir)
	    ls (nnmaildir--param pgname 'directory-files)
	    ls (or ls srv-ls)
	    markdirs (funcall ls dir nil "\\`[^.]" 'nosort)
	    num (length markdirs)
	    new-mmth 1)
      (while (<= new-mmth num) (setq new-mmth (* 2 new-mmth)))
      (if (/= new-mmth 1) (setq new-mmth (1- new-mmth)))
      (setq new-mmth (make-vector new-mmth 0)
	    old-mmth (nnmaildir--grp-get-mmth group))
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
	  (setq mtime (file-attributes articles)
		mtime (nth 5 mtime))
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
		(setq num (nnmaildir--art-get-num article)
		      ranges (gnus-add-to-range ranges (list num))))))
	(if (eq mark-sym 'read) (setq read ranges)
	  (if ranges (setq marks (cons (cons mark-sym ranges) marks)))))
      (gnus-info-set-read info read)
      (gnus-info-set-marks info marks 'extend)
      (nnmaildir--grp-set-mmth group new-mmth)
      info)))

(defun nnmaildir-request-group (gname &optional server fast)
  (nnmaildir-request-scan gname server)
  (let ((group (nnmaildir--prepare server gname))
	ct-min deactivate-mark)
    (save-excursion
      (set-buffer nntp-server-buffer)
      (erase-buffer)
      (catch 'return
	(if group nil
	  (insert "411 no such news group\n")
	  (nnmaildir--srv-set-error nnmaildir--cur-server
				    (concat "No such group: " gname))
	  (throw 'return nil))
	(nnmaildir--srv-set-curgrp nnmaildir--cur-server group)
	(if fast (throw 'return t))
	(setq ct-min (nnmaildir--article-count group))
	(insert "211 ")
	(princ (car ct-min) nntp-server-buffer)
	(insert " ")
	(princ (cdr ct-min) nntp-server-buffer)
	(insert " ")
	(princ (nnmaildir--nlist-last-num
		(nnmaildir--lists-get-nlist
		 (nnmaildir--grp-get-lists group)))
	       nntp-server-buffer)
	(insert " " gname "\n")
	t))))

(defun nnmaildir-request-create-group (gname &optional server args)
  (nnmaildir--prepare server nil)
  (catch 'return
    (let ((create-dir (nnmaildir--srv-get-create-dir nnmaildir--cur-server))
	  srv-dir dir groups)
      (when (zerop (length gname))
	(nnmaildir--srv-set-error nnmaildir--cur-server
				  "Invalid (empty) group name")
	(throw 'return nil))
      (when (eq (aref "." 0) (aref gname 0))
	(nnmaildir--srv-set-error nnmaildir--cur-server
				  "Group names may not start with \".\"")
	(throw 'return nil))
      (when (save-match-data (string-match "[\0/\t]" gname))
	(nnmaildir--srv-set-error nnmaildir--cur-server
				  (concat "Illegal characters (null, tab, or /) in group name: "
					  gname))
	(throw 'return nil))
      (setq groups (nnmaildir--srv-get-groups nnmaildir--cur-server))
      (when (intern-soft gname groups)
	(nnmaildir--srv-set-error nnmaildir--cur-server
				  (concat "Group already exists: " gname))
	(throw 'return nil))
      (setq srv-dir (nnmaildir--srv-get-dir nnmaildir--cur-server))
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
	(nnmaildir--srv-set-error nnmaildir--cur-server
				  (concat "No such group: " gname))
	(throw 'return nil))
      (when (zerop (length new-name))
	(nnmaildir--srv-set-error nnmaildir--cur-server
				  "Invalid (empty) group name")
	(throw 'return nil))
      (when (eq (aref "." 0) (aref new-name 0))
	(nnmaildir--srv-set-error nnmaildir--cur-server
				  "Group names may not start with \".\"")
	(throw 'return nil))
      (when (save-match-data (string-match "[\0/\t]" new-name))
	(nnmaildir--srv-set-error nnmaildir--cur-server
				  (concat "Illegal characters (null, tab, or /) in group name: "
					  new-name))
	(throw 'return nil))
      (if (string-equal gname new-name) (throw 'return t))
      (when (intern-soft new-name
			 (nnmaildir--srv-get-groups nnmaildir--cur-server))
	(nnmaildir--srv-set-error nnmaildir--cur-server
				  (concat "Group already exists: " new-name))
	(throw 'return nil))
      (setq srv-dir (nnmaildir--srv-get-dir nnmaildir--cur-server))
      (condition-case err
	  (rename-file (concat srv-dir gname)
		       (concat srv-dir new-name))
	(error
	 (nnmaildir--srv-set-error nnmaildir--cur-server
				   (concat "Error renaming link: "
					   (prin1-to-string err)))
	 (throw 'return nil)))
      (setq x (nnmaildir--srv-get-groups nnmaildir--cur-server)
	    groups (make-vector (length x) 0))
      (mapatoms (lambda (sym)
		  (if (eq (symbol-value sym) group) nil
		    (set (intern (symbol-name sym) groups)
			 (symbol-value sym))))
		x)
      (setq group (copy-sequence group))
      (nnmaildir--grp-set-name group new-name)
      (set (intern new-name groups) group)
      (nnmaildir--srv-set-groups nnmaildir--cur-server groups)
      t)))

(defun nnmaildir-request-delete-group (gname force &optional server)
  (let ((group (nnmaildir--prepare server gname))
	pgname grp-dir dir dirs files ls deactivate-mark)
    (catch 'return
      (if group nil
	(nnmaildir--srv-set-error nnmaildir--cur-server
				  (concat "No such group: " gname))
	(throw 'return nil))
      (if (eq group (nnmaildir--srv-get-curgrp nnmaildir--cur-server))
	  (nnmaildir--srv-set-curgrp nnmaildir--cur-server nil))
      (setq gname (nnmaildir--grp-get-name group)
	    pgname (nnmaildir--grp-get-pname group))
      (unintern gname (nnmaildir--srv-get-groups nnmaildir--cur-server))
      (setq grp-dir (nnmaildir--srv-get-dir nnmaildir--cur-server)
	    grp-dir (nnmaildir--srv-grp-dir grp-dir gname))
      (if (not force) (setq grp-dir (directory-file-name grp-dir))
	(if (nnmaildir--param pgname 'read-only)
	    (progn (delete-directory  (nnmaildir--tmp grp-dir))
		   (nnmaildir--unlink (nnmaildir--new grp-dir))
		   (delete-directory  (nnmaildir--cur grp-dir)))
	  (save-excursion
	    (set-buffer (get-buffer-create " *nnmaildir work*"))
	    (erase-buffer)
	    (setq ls (or (nnmaildir--param pgname 'directory-files)
			 (nnmaildir--srv-get-ls nnmaildir--cur-server))
		  files (funcall ls (nnmaildir--tmp grp-dir) 'full "\\`[^.]"
				 'nosort))
	    (while files
	      (delete-file (car files))
	      (setq files (cdr files)))
	    (delete-directory (concat grp-dir "tmp"))
	    (setq files (funcall ls (nnmaildir--new grp-dir) 'full "\\`[^.]"
				 'nosort))
	    (while files
	      (delete-file (car files))
	      (setq files (cdr files)))
	    (delete-directory (concat grp-dir "new"))
	    (setq files (funcall ls (nnmaildir--cur grp-dir) 'full "\\`[^.]"
				 'nosort))
	    (while files
	      (delete-file (car files))
	      (setq files (cdr files)))
	    (delete-directory (concat grp-dir "cur"))))
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
	(setq dir (nnmaildir--nndir grp-dir)
	      files (concat dir "markfile"))
	(nnmaildir--unlink files)
	(delete-directory (nnmaildir--marks-dir dir))
	(delete-directory dir)
	(setq grp-dir (directory-file-name grp-dir)
	      dir (car (file-attributes grp-dir)))
	(if (eq (aref "/" 0) (aref dir 0)) nil
	  (setq dir (concat (file-truename
			     (nnmaildir--srv-get-dir nnmaildir--cur-server))
			    dir)))
	(delete-directory dir))
      (nnmaildir--unlink grp-dir)
      t)))

(defun nnmaildir-retrieve-headers (articles &optional gname server fetch-old)
  (let ((group (nnmaildir--prepare server gname))
	srv-dir dir nlist mlist article num stop nov nlist2 deactivate-mark)
    (catch 'return
      (if group nil
	(nnmaildir--srv-set-error nnmaildir--cur-server
				  (if gname (concat "No such group: " gname)
				    "No current group"))
	(throw 'return nil))
      (save-excursion
	(set-buffer nntp-server-buffer)
	(erase-buffer)
	(setq nlist (nnmaildir--grp-get-lists group)
	      mlist (nnmaildir--lists-get-mlist nlist)
	      nlist (nnmaildir--lists-get-nlist nlist)
	      gname (nnmaildir--grp-get-name group)
	      srv-dir (nnmaildir--srv-get-dir nnmaildir--cur-server)
	      dir (nnmaildir--srv-grp-dir srv-dir gname))
	(cond
	 ((null nlist))
	 ((and fetch-old (not (numberp fetch-old)))
	  (while nlist
	    (setq article (car nlist) nlist (cdr nlist)
		  nov (nnmaildir--update-nov srv-dir group article))
	    (when nov
	      (nnmaildir--cache-nov group article nov)
	      (setq num (nnmaildir--art-get-num article))
	      (princ num nntp-server-buffer)
	      (insert "\t" (nnmaildir--nov-get-beg nov) "\t"
		      (nnmaildir--art-get-msgid article) "\t"
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
		       (setq nov (nnmaildir--update-nov srv-dir group
							article)))
	      (nnmaildir--cache-nov group article nov)
	      (setq num (nnmaildir--art-get-num article))
	      (princ num nntp-server-buffer)
	      (insert "\t" (nnmaildir--nov-get-beg nov) "\t"
		      (nnmaildir--art-get-msgid article) "\t"
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
	    (setq nlist2 (nthcdr (- (nnmaildir--art-get-num (car nlist)) num)
				 nlist))
	    (while (and nlist2
			(setq article (car nlist2)
			      num (nnmaildir--art-get-num article))
			(>= num stop))
	      (setq nlist2 (cdr nlist2)
		    nov (nnmaildir--update-nov srv-dir group article))
	      (when nov
		(nnmaildir--cache-nov group article nov)
		(princ num nntp-server-buffer)
		(insert "\t" (nnmaildir--nov-get-beg nov) "\t"
			(nnmaildir--art-get-msgid article) "\t"
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
	list article suffix dir deactivate-mark)
    (catch 'return
      (if group nil
	(nnmaildir--srv-set-error nnmaildir--cur-server
				  (if gname (concat "No such group: " gname)
				    "No current group"))
	(throw 'return nil))
      (setq list (nnmaildir--grp-get-lists group))
      (if (numberp num-msgid)
	  (setq list (nnmaildir--lists-get-nlist list)
		article (nnmaildir--nlist-art list num-msgid))
	(setq list (nnmaildir--lists-get-mlist list)
	      article (nnmaildir--mlist-art list num-msgid))
	(if article (setq num-msgid (nnmaildir--art-get-num article))
	  (catch 'found
	    (mapatoms
	     (lambda (grp)
	       (setq group (symbol-value grp)
		     list (nnmaildir--grp-get-lists group)
		     list (nnmaildir--lists-get-mlist list)
		     article (nnmaildir--mlist-art list num-msgid))
	       (when article
		 (setq num-msgid (nnmaildir--art-get-num article))
		 (throw 'found nil)))
	     (nnmaildir--srv-get-groups nnmaildir--cur-server)))))
      (if article nil
	(nnmaildir--srv-set-error nnmaildir--cur-server "No such article")
	(throw 'return nil))
      (if (stringp (setq suffix (nnmaildir--art-get-suffix article))) nil
	(nnmaildir--srv-set-error nnmaildir--cur-server "Article has expired")
	(throw 'return nil))
      (setq gname (nnmaildir--grp-get-name group)
	    dir (nnmaildir--srv-get-dir nnmaildir--cur-server)
	    dir (nnmaildir--srv-grp-dir dir gname)
	    group (if (nnmaildir--param (nnmaildir--grp-get-pname group)
					'read-only)
		      (nnmaildir--new dir) (nnmaildir--cur dir))
	    nnmaildir-article-file-name (concat group
						(nnmaildir--art-get-prefix
						 article)
						suffix))
      (if (file-exists-p nnmaildir-article-file-name) nil
	(nnmaildir--art-set-suffix article 'expire)
	(nnmaildir--art-set-nov article nil)
	(nnmaildir--srv-set-error nnmaildir--cur-server "Article has expired")
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
	(nnmaildir--srv-set-error nnmaildir--cur-server
				  (concat "No such group: " gname))
	(throw 'return nil))
      (when (nnmaildir--param (nnmaildir--grp-get-pname group) 'read-only)
	(nnmaildir--srv-set-error nnmaildir--cur-server
				  (concat "Read-only group: " group))
	(throw 'return nil))
      (setq dir (nnmaildir--srv-get-dir nnmaildir--cur-server)
	    dir (nnmaildir--srv-grp-dir dir gname)
	    file (nnmaildir--grp-get-lists group)
	    file (nnmaildir--lists-get-nlist file)
	    file (nnmaildir--nlist-art file article))
      (if (and file (stringp (setq suffix (nnmaildir--art-get-suffix file))))
	  nil
	(nnmaildir--srv-set-error nnmaildir--cur-server
				  (format "No such article: %d" article))
	(throw 'return nil))
      (save-excursion
	(set-buffer buffer)
	(setq article file
	      file (nnmaildir--art-get-prefix article)
	      tmpfile (concat (nnmaildir--tmp dir) file))
	(when (file-exists-p tmpfile)
	  (nnmaildir--srv-set-error nnmaildir--cur-server
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
	(nnmaildir--srv-set-error nnmaildir--cur-server
				  (concat "No such group: " gname))
	(throw 'return nil))
      (setq gname (nnmaildir--grp-get-name group)
	    pgname (nnmaildir--grp-get-pname group)
	    list (nnmaildir--grp-get-lists group)
	    list (nnmaildir--lists-get-nlist list)
	    article (nnmaildir--nlist-art list article))
      (if article nil
	(nnmaildir--srv-set-error nnmaildir--cur-server "No such article")
	(throw 'return nil))
      (if (stringp (setq suffix (nnmaildir--art-get-suffix article))) nil
	(nnmaildir--srv-set-error nnmaildir--cur-server "Article has expired")
	(throw 'return nil))
      (setq nnmaildir--file (nnmaildir--srv-get-dir nnmaildir--cur-server)
	    nnmaildir--file (nnmaildir--srv-grp-dir nnmaildir--file gname)
	    nnmaildir--file (if (nnmaildir--param pgname 'read-only)
				(nnmaildir--new nnmaildir--file)
			      (nnmaildir--cur nnmaildir--file))
	    nnmaildir--file (concat nnmaildir--file
				    (nnmaildir--art-get-prefix article)
				    suffix))
      (if (file-exists-p nnmaildir--file) nil
	(nnmaildir--art-set-suffix article 'expire)
	(nnmaildir--art-set-nov article nil)
	(nnmaildir--srv-set-error nnmaildir--cur-server "Article has expired")
	(throw 'return nil))
      (save-excursion
	(set-buffer (get-buffer-create " *nnmaildir move*"))
	(erase-buffer)
	(nnheader-insert-file-contents nnmaildir--file)
	(setq result (eval accept-form)))
      (if (or (null result) (nnmaildir--param pgname 'read-only)) nil
	(nnmaildir--unlink nnmaildir--file)
	(nnmaildir--art-set-suffix article 'expire)
	(nnmaildir--art-set-nov article nil))
      result)))

(defun nnmaildir-request-accept-article (gname &optional server last)
  (let ((group (nnmaildir--prepare server gname))
	(coding-system-for-write nnheader-file-coding-system)
	(buffer-file-coding-system nil)
	(file-coding-system-alist nil)
	srv-dir dir file tmpfile curfile 24h num article)
    (catch 'return
      (if group nil
	(nnmaildir--srv-set-error nnmaildir--cur-server
				  (concat "No such group: " gname))
	(throw 'return nil))
      (setq gname (nnmaildir--grp-get-name group))
      (when (nnmaildir--param (nnmaildir--grp-get-pname group) 'read-only)
	(nnmaildir--srv-set-error nnmaildir--cur-server
				  (concat "Read-only group: " gname))
	(throw 'return nil))
      (setq srv-dir (nnmaildir--srv-get-dir nnmaildir--cur-server)
	    dir (nnmaildir--srv-grp-dir srv-dir gname)
	    file (format-time-string "%s" nil))
      (if (string= nnmaildir--delivery-time file) nil
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
	(nnmaildir--srv-set-error nnmaildir--cur-server
				  (concat "File exists: " tmpfile))
	(throw 'return nil))
      (when (file-exists-p curfile)
	(nnmaildir--srv-set-error nnmaildir--cur-server
				  (concat "File exists: " curfile))
	(throw 'return nil))
      (setq nnmaildir--delivery-ct (1+ nnmaildir--delivery-ct)
	    24h (run-with-timer 86400 nil
				(lambda ()
				  (nnmaildir--unlink tmpfile)
				  (nnmaildir--srv-set-error
				   nnmaildir--cur-server
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
	 (nnmaildir--srv-set-error nnmaildir--cur-server
				   (concat "Error linking: "
					   (prin1-to-string err)))
	 (nnmaildir--unlink tmpfile)
	 (throw 'return nil)))
      (nnmaildir--unlink tmpfile)
      (setq article (nnmaildir--art-new)
	    num (nnmaildir--grp-get-lists group)
	    num (nnmaildir--lists-get-nlist num)
	    num (1+ (nnmaildir--nlist-last-num num)))
      (nnmaildir--art-set-prefix article file)
      (nnmaildir--art-set-suffix article ":2,")
      (nnmaildir--art-set-num article num)
      (if (nnmaildir--grp-add-art srv-dir group article) (cons gname num)))))

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
      (setq groups (nnmaildir--srv-get-groups nnmaildir--cur-server)
	    ga (car group-art) group-art (cdr group-art)
	    gname (car ga))
      (or (intern-soft gname groups)
	  (nnmaildir-request-create-group gname)
	  (throw 'return nil)) ;; not that nnmail bothers to check :(
      (if (nnmaildir-request-accept-article gname) nil
	(throw 'return nil))
      (setq x (nnmaildir--prepare nil gname)
	    nnmaildir--file (nnmaildir--srv-get-dir nnmaildir--cur-server)
	    nnmaildir--file (nnmaildir--subdir nnmaildir--file
                                               (nnmaildir--grp-get-name x))
	    x (nnmaildir--grp-get-lists x)
	    x (nnmaildir--lists-get-nlist x)
	    x (car x)
	    nnmaildir--file (concat nnmaildir--file
				    (nnmaildir--art-get-prefix x)
				    (nnmaildir--art-get-suffix x)))
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
	(nnmaildir--srv-set-error nnmaildir--cur-server
				  (concat "No such group: " group))
	(throw 'return nil))
      (setq x (nnmaildir--grp-get-lists x)
	    x (nnmaildir--lists-get-nlist x))
      (if x
	  (setq x (car x)
		x (nnmaildir--art-get-num x)
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
	(nnmaildir--srv-set-error nnmaildir--cur-server
				  (if gname (concat "No such group: " gname)
				    "No current group"))
	(throw 'return (gnus-uncompress-range ranges)))
      (setq gname (nnmaildir--grp-get-name group)
	    pgname (nnmaildir--grp-get-pname group))
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
      (setq dir (nnmaildir--srv-get-dir nnmaildir--cur-server)
	    dir (nnmaildir--srv-grp-dir dir gname)
	    dir (nnmaildir--cur dir)
	    nlist (nnmaildir--grp-get-lists group)
	    nlist (nnmaildir--lists-get-nlist nlist)
	    ranges (reverse ranges))
      (save-excursion
	(set-buffer (get-buffer-create " *nnmaildir move*"))
	(while ranges
	  (setq number (car ranges) ranges (cdr ranges))
	  (while (eq number (car ranges))
	    (setq ranges (cdr ranges)))
	  (if (numberp number) (setq stop number)
	    (setq stop (car number) number (cdr number)))
	  (setq nlist (nthcdr (- (nnmaildir--art-get-num (car nlist)) number)
			      nlist))
	  (while (and nlist
		      (setq article (car nlist)
			    number (nnmaildir--art-get-num article))
		      (>= number stop))
	    (setq nlist (cdr nlist)
		  suffix (nnmaildir--art-get-suffix article))
	    (catch 'continue
	      (if (stringp suffix) nil
		(nnmaildir--art-set-suffix article 'expire)
		(nnmaildir--art-set-nov article nil)
		(throw 'continue nil))
	      (setq nnmaildir--file (nnmaildir--art-get-prefix article)
		    nnmaildir--file (concat dir nnmaildir--file suffix)
		    time (file-attributes nnmaildir--file))
	      (if time nil
		(nnmaildir--art-set-suffix article 'expire)
		(nnmaildir--art-set-nov article nil)
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
		  (nnmaildir--art-set-suffix article 'expire)
		  (nnmaildir--art-set-nov article nil))))))
	(erase-buffer))
      didnt)))

(defun nnmaildir-request-set-mark (gname actions &optional server)
  (let ((group (nnmaildir--prepare server gname))
	(coding-system-for-write nnheader-file-coding-system)
	(buffer-file-coding-system nil)
	(file-coding-system-alist nil)
	del-mark add-marks marksdir markfile action group-nlist nlist ranges
	begin end article all-marks todo-marks did-marks marks form mdir mfile
	deactivate-mark)
    (setq del-mark
	  (lambda ()
	    (setq mfile (nnmaildir--subdir marksdir (symbol-name (car marks)))
		  mfile (concat mfile (nnmaildir--art-get-prefix article)))
	    (nnmaildir--unlink mfile))
	  add-marks
	  (lambda ()
	    (while marks
	      (setq mdir (nnmaildir--subdir marksdir (symbol-name (car marks)))
		    mfile (concat mdir (nnmaildir--art-get-prefix article)))
	      (if (memq (car marks) did-marks) nil
		(nnmaildir--mkdir mdir)
		(setq did-marks (cons (car marks) did-marks)))
	      (if (file-exists-p mfile) nil
		(condition-case nil
		    (add-name-to-file markfile mfile)
		  (file-error ;; too many links, probably
		   (if (file-exists-p mfile) nil
		     (nnmaildir--unlink markfile)
		     (write-region "" nil markfile nil 'no-message)
		     (add-name-to-file markfile mfile
				       'ok-if-already-exists)))))
	      (setq marks (cdr marks)))))
    (catch 'return
      (if group nil
	(nnmaildir--srv-set-error nnmaildir--cur-server
				  (concat "No such group: " gname))
	(while actions
	  (setq ranges (gnus-range-add ranges (caar actions))
		actions (cdr actions)))
	(throw 'return ranges))
      (setq group-nlist (nnmaildir--grp-get-lists group)
	    group-nlist (nnmaildir--lists-get-nlist group-nlist)
	    marksdir (nnmaildir--srv-get-dir nnmaildir--cur-server)
	    marksdir (nnmaildir--srv-grp-dir marksdir gname)
	    marksdir (nnmaildir--nndir marksdir)
	    markfile (concat marksdir "markfile")
	    marksdir (nnmaildir--marks-dir marksdir)
	    gname (nnmaildir--grp-get-name group)
	    all-marks (nnmaildir--grp-get-pname group)
	    all-marks (or (nnmaildir--param all-marks 'directory-files)
			  (nnmaildir--srv-get-ls nnmaildir--cur-server))
	    all-marks (funcall all-marks marksdir nil "\\`[^.]" 'nosort)
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
	  (setq nlist (nthcdr (- (nnmaildir--art-get-num (car nlist)) end)
			      nlist))
	  (while (and nlist
		      (setq article (car nlist))
		      (>= (nnmaildir--art-get-num article) begin))
	    (setq nlist (cdr nlist))
	    (when (stringp (nnmaildir--art-get-suffix article))
	      (setq marks todo-marks)
	      (eval form)))))
      nil)))

(defun nnmaildir-close-group (group &optional server)
  t)

(defun nnmaildir-close-server (&optional server)
  (let (srv-ls flist ls dirs dir files file x)
    (nnmaildir--prepare server nil)
    (setq server nnmaildir--cur-server)
    (when server
      (setq nnmaildir--cur-server nil
	    srv-ls (nnmaildir--srv-get-ls server))
      (save-match-data
	(mapatoms
	 (lambda (group)
	   (setq group (symbol-value group)
		 x (nnmaildir--grp-get-pname group)
		 ls (nnmaildir--param x 'directory-files)
		 ls (or ls srv-ls)
		 dir (nnmaildir--srv-get-dir server)
		 dir (nnmaildir--srv-grp-dir
		      dir (nnmaildir--grp-get-name group))
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
	 (nnmaildir--srv-get-groups server)))
      (unintern (nnmaildir--srv-get-name server) nnmaildir--servers)))
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
                       (>= (length (setq name (symbol-name sym))) 10)
                       (string-equal "nnmaildir-" (substring name 0 10))))
          (put sym 'lisp-indent-function 0))))
    'done))

(provide 'nnmaildir)

;; Local Variables:
;; eval: (progn (require 'nnmaildir) (nnmaildir--edit-prep))
;; End:

;;; nnmaildir.el ends here
