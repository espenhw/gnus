;;; nnvirtual.el --- virtual newsgroups access for Gnus
;; Copyright (C) 1994,95,96 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@ifi.uio.no>
;; 	Masanobu UMEDA <umerin@flab.flab.fujitsu.junet>
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

;; The other access methods (nntp, nnspool, etc) are general news
;; access methods. This module relies on Gnus and can not be used
;; separately.

;;; Code:

(require 'nntp)
(require 'nnheader)
(require 'gnus)

(defvar nnvirtual-always-rescan nil
  "*If non-nil, always scan groups for unread articles when entering a group.
If this variable is nil (which is the default) and you read articles
in a component group after the virtual group has been activated, the
read articles from the component group will show up when you enter the
virtual group.")



(defconst nnvirtual-version "nnvirtual 1.0"
  "Version number of this version of nnvirtual.")

(defvar nnvirtual-group-alist nil)
(defvar nnvirtual-current-group nil)
(defvar nnvirtual-component-groups nil)
(defvar nnvirtual-mapping nil)

(defvar nnvirtual-status-string "")

(eval-and-compile
  (autoload 'gnus-cache-articles-in-group "gnus-cache"))



;;; Interface functions.

(defun nnvirtual-retrieve-headers (articles &optional newsgroup server fetch-old)
  (when (nnvirtual-possibly-change-group newsgroup server t)
    (save-excursion
      (if (stringp (car articles))
	  'headers
	(let ((map nnvirtual-mapping)
	      (vbuf (nnheader-set-temp-buffer 
		     (get-buffer-create " *virtual headers*")))
	      (unfetched (mapcar (lambda (g) (list g))
				 nnvirtual-component-groups))
	      beg cgroup active article result prefix)
	  (while articles
	    (setq article (assq (pop articles) nnvirtual-mapping))
	    (setq cgroup (cadr article))
	    (gnus-request-group cgroup t)
	    (setq prefix (gnus-group-real-prefix cgroup))
	    (when (setq result (gnus-retrieve-headers 
				(list (caddr article)) cgroup))
	      (set-buffer nntp-server-buffer)
	      (if (zerop (buffer-size))
		  (nconc (assq cgroup unfetched) (list (caddr article)))
		;; If we got HEAD headers, we convert them into NOV
		;; headers.  This is slow, inefficient and, come to think
		;; of it, downright evil.  So sue me.  I couldn't be
		;; bothered to write a header parse routine that could
		;; parse a mixed HEAD/NOV buffer.
		(when (eq result 'headers)
		  (nnvirtual-convert-headers))
		(goto-char (point-min))
		(while (not (eobp))
		  (delete-region 
		   (point) (progn (read nntp-server-buffer) (point)))
		  (insert (int-to-string (car article)))
		  (beginning-of-line)
		  (looking-at 
		   "[^\t]*\t[^\t]*\t[^\t]*\t[^\t]*\t[^\t]*\t[^\t]*\t[^\t]*\t")
		  (goto-char (match-end 0))
		  (or (search-forward 
		       "\t" (save-excursion (end-of-line) (point)) t)
		      (end-of-line))
		  (while (= (char-after (1- (point))) ? )
		    (forward-char -1)
		    (delete-char 1))
		  (if (eolp)
		      (progn
			(end-of-line)
			(or (= (char-after (1- (point))) ?\t)
			    (insert ?\t))
			(insert (format "Xref: %s %s:%d\t" (system-name) 
					cgroup (caddr article))))
		    (if (not (string= "" prefix))
			(while (re-search-forward 
				"[^ ]+:[0-9]+"
				(save-excursion (end-of-line) (point)) t)
			  (save-excursion
			    (goto-char (match-beginning 0))
			    (insert prefix))))
		    (end-of-line)
		    (or (= (char-after (1- (point))) ?\t)
			(insert ?\t)))
		  (forward-line 1))
		(set-buffer vbuf)
		(goto-char (point-max))
		(insert-buffer-substring nntp-server-buffer))))
	  
	  ;; In case some of the articles have expired or been
	  ;; cancelled, we have to mark them as read in the
	  ;; component group.
	  (while unfetched
	    (when (cdar unfetched)
	      (gnus-group-make-articles-read 
	       (caar unfetched) (sort (cdar unfetched) '<)))
	    (setq unfetched (cdr unfetched)))

	  ;; The headers are ready for reading, so they are inserted into
	  ;; the nntp-server-buffer, which is where Gnus expects to find
	  ;; them.
	  (prog1
	      (save-excursion
		(set-buffer nntp-server-buffer)
		(erase-buffer)
		(insert-buffer-substring vbuf)
		'nov)
	    (kill-buffer vbuf)))))))

(defun nnvirtual-open-server (server &optional something)
  (nnheader-init-server-buffer))

(defun nnvirtual-close-server (&rest dum)
  t)

(defun nnvirtual-request-close ()
  (setq nnvirtual-current-group nil
	nnvirtual-component-groups nil
	nnvirtual-mapping nil
	nnvirtual-group-alist nil)
  t)

(defun nnvirtual-server-opened (&optional server)
  (and nntp-server-buffer
       (get-buffer nntp-server-buffer)))

(defun nnvirtual-status-message (&optional server)
  nnvirtual-status-string)

(defun nnvirtual-request-article (article &optional group server buffer)
  (when (and (nnvirtual-possibly-change-group group server t)
	     (numberp article))
    (let* ((amap (assq article nnvirtual-mapping))
	   (cgroup (cadr amap)))
      (cond
       ((not amap)
	(nnheader-report 'nnvirtual "No such article: %s" article))
       ((not (gnus-check-group cgroup))
	(nnheader-report
	 'nnvirtual "Can't open server where %s exists" cgroup))
       ((not (gnus-request-group cgroup t))
	(nnheader-report 'nnvirtual "Can't open component group %s" cgroup))
       (t
	(if buffer 
	    (save-excursion
	      (set-buffer buffer)
	      (gnus-request-article-this-buffer (caddr amap) cgroup))
	  (gnus-request-article (caddr amap) cgroup)))))))

(defun nnvirtual-request-group (group &optional server dont-check)
  (cond
   ((null (nnvirtual-possibly-change-group
	   group server 
	   (if nnvirtual-always-rescan nil dont-check)))
    (setq nnvirtual-current-group nil)
    (nnheader-report 'nnvirtual "No component groups in %s" group))
   (t
    (let ((len (length nnvirtual-mapping)))
      (nnheader-insert "211 %d 1 %d %s\n" len len group)))))

(defun nnvirtual-request-type (group &optional article)
  (when (nnvirtual-possibly-change-group group nil t)
    (if (not article)
	'unknown
      (let ((mart (assq article nnvirtual-mapping)))
	(when mart
	  (gnus-request-type (cadr mart) (car mart)))))))

(defun nnvirtual-request-update-mark (group article mark)
  (when (nnvirtual-possibly-change-group group nil t)
    (let* ((nart (assq article nnvirtual-mapping))
	   (cgroup (cadr nart))
	   ;; The component group might be a virtual group.
	   (nmark (gnus-request-update-mark cgroup (caddr nart) mark)))
      (when (and (= mark nmark)
		 (gnus-group-auto-expirable-p cgroup))
	(setq mark gnus-expirable-mark))))
  mark)
    
(defun nnvirtual-close-group (group &optional server)
  (when (nnvirtual-possibly-change-group group server t)
    ;; We copy the marks from this group to the component
    ;; groups here.
    (nnvirtual-update-marked)
    ;; Reset all relevant variables.
    (setq nnvirtual-current-group nil
	  nnvirtual-component-groups nil
	  nnvirtual-mapping nil)
    (setq nnvirtual-group-alist 
	  (delq (assoc group nnvirtual-group-alist) nnvirtual-group-alist)))
  t)
    
(defun nnvirtual-request-list (&optional server) 
  (nnheader-report 'nnvirtual "LIST is not implemented."))

(defun nnvirtual-request-newgroups (date &optional server)
  (nnheader-report 'nnvirtual "NEWGROUPS is not supported."))

(defun nnvirtual-request-list-newsgroups (&optional server)
  (nnheader-report 'nnvirtual "LIST NEWSGROUPS is not implemented."))

(defun nnvirtual-request-update-info (group info &optional server)
  (when (nnvirtual-possibly-change-group group server)
    (let ((map nnvirtual-mapping)
	  (marks (mapcar (lambda (m) (list (cdr m))) gnus-article-mark-lists))
	  reads marks mr m op)
      (while map
	(setq m (pop map))
	(unless (nth 3 m)
	  (push (car m) reads))
	(when (setq mr (nth 4 m))
	  (while mr
	    (setcdr (setq op (assq (pop mr) marks)) (cons (car m) (cdr op))))))
      (setq mr marks)
      (while mr
	(setcdr (car mr) (gnus-compress-sequence (sort (cdar mr) '<)))
	(setq mr (cdr mr)))
      (setcar (cddr info) (gnus-compress-sequence (nreverse reads)))
      
      ;; Enter these new marks into the info of the group.
      (if (nthcdr 3 info)
	  (setcar (nthcdr 3 info) marks)
	;; Add the marks lists to the end of the info.
	(when marks
	  (setcdr (nthcdr 2 info) (list marks))))
      t)))

(defun nnvirtual-catchup-group (group &optional server all)
  (nnvirtual-possibly-change-group group server t)
  (let ((gnus-group-marked nnvirtual-component-groups)
	(gnus-expert-user t))
    (save-excursion
      (set-buffer gnus-group-buffer)
      (gnus-group-catchup-current nil all))))

(defun nnvirtual-find-group-art (group article)
  "Return the real group and article for virtual GROUP and ARTICLE."
  (nnvirtual-possibly-change-group group nil t)
  (let ((mart (assq article nnvirtual-mapping)))
    (cons (cadr mart) (caddr mart))))


;;; Internal functions.

(defun nnvirtual-convert-headers ()
  "Convert HEAD headers into NOV headers."
  (save-excursion
    (set-buffer nntp-server-buffer)
    (let* ((dependencies (make-vector 100 0))
	   (headers (gnus-get-newsgroup-headers dependencies))
	   header)
      (erase-buffer)
      (while (setq header (pop headers))
	(insert (int-to-string (mail-header-number header)) "\t"
		(or (mail-header-subject header) "") "\t"
		(or (mail-header-from header) "") "\t"
		(or (mail-header-date header) "") "\t"
		(or (mail-header-id header) "") "\t"
		(or (mail-header-references header) "") "\t"
		(int-to-string (or (mail-header-chars header) 0)) "\t"
		(int-to-string (or (mail-header-lines header) 0)) "\t"
		(if (mail-header-xref header) 
		    (concat "Xref: " (mail-header-xref header) "\t")
		  "") "\n")))))

(defun nnvirtual-possibly-change-group (group regexp &optional dont-check)
  (let ((inf t))
    (unless (equal group nnvirtual-current-group)
      (and (setq inf (assoc group nnvirtual-group-alist))
	   regexp
	   (string= (nth 3 inf) regexp)
	   (progn
	     (setq nnvirtual-current-group (car inf))
	     (setq nnvirtual-component-groups (nth 1 inf))
	     (setq nnvirtual-mapping (nth 2 inf)))))
    (when (and regexp
	       (or (not inf)
		   (not dont-check)))
      (and inf (setq nnvirtual-group-alist 
		     (delq inf nnvirtual-group-alist)))
      (setq nnvirtual-mapping nil)
      (setq nnvirtual-current-group group)
      (let ((newsrc gnus-newsrc-alist)
	    (virt-group (gnus-group-prefixed-name 
			 nnvirtual-current-group '(nnvirtual ""))))
	(setq nnvirtual-component-groups nil)
	(while newsrc
	  (and (string-match regexp (car (car newsrc)))
	       (not (string= (car (car newsrc)) virt-group))
	       (setq nnvirtual-component-groups
		     (cons (car (car newsrc)) nnvirtual-component-groups)))
	  (setq newsrc (cdr newsrc))))
      (if nnvirtual-component-groups
	  (progn
	    (nnvirtual-create-mapping)
	    (setq nnvirtual-group-alist
		  (cons (list group nnvirtual-component-groups 
			      nnvirtual-mapping regexp)
			nnvirtual-group-alist)))
	(nnheader-report 'nnvirtual "No component groups: %s" group))))
  nnvirtual-component-groups)

(defun nnvirtual-update-marked ()
  "Copy marks from the virtual group to the component groups."
  (let ((mark-lists gnus-article-mark-lists)
	(uncompressed '(score bookmark))
	type list calist mart cgroups)
    (while mark-lists
      (setq type (cdar mark-lists))
      (when (setq list (symbol-value (intern (format "gnus-newsgroup-%s"
						     (car (pop mark-lists))))))
	(setq cgroups 
	      (mapcar (lambda (g) (list g)) nnvirtual-component-groups))
	(while list
	  (nconc (assoc (cadr (setq mart (assq (pop list) nnvirtual-mapping)))
			cgroups)
		 (list (caddr mart))))
	(while cgroups
	  (when (cdar cgroups)
	    (gnus-add-marked-articles 
	     (caar cgroups) type (cdar cgroups) nil t)
	    (gnus-group-update-group (caar cgroups) t))
	  (setq cgroups (cdr cgroups)))))))

(defun nnvirtual-marks (article marks)
  "Return a list of mark types for ARTICLE."
  (let (out)
    (while marks
      (when (memq article (cdar marks))
	(push (caar marks) out))
      (setq marks (cdr marks)))
    out))

(defun nnvirtual-create-mapping ()
  "Create an article mapping for the current group."
  (let* (div
	 (map (sort
	       (apply 
		'nconc
		(mapcar
		 (lambda (g)
		   (let* ((active (or (gnus-active g) (gnus-activate-group g)))
			  (unreads (gnus-list-of-unread-articles g))
			  (marks (gnus-uncompress-marks
				  (gnus-info-marks (gnus-get-info g)))))
		     (when gnus-use-cache
		       (push (cons 'cache (gnus-cache-articles-in-group g))
			     marks))
		     (when active
		       (setq div (/ (float (car active)) 
				    (if (zerop (cdr active))
					1 (cdr active))))
		       (mapcar (lambda (n) 
				 (list (* div (- n (car active)))
				       g n (and (memq n unreads) t)
				       (nnvirtual-marks n marks)))
			       (gnus-uncompress-range active)))))
		 nnvirtual-component-groups))
	       (lambda (m1 m2)
		 (< (car m1) (car m2)))))
	 (i 0))
    (setq nnvirtual-mapping map)
    (while map
      (setcar (pop map) (incf i)))))

(provide 'nnvirtual)

;;; nnvirtual.el ends here
