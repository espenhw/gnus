;;; gnus-topic.el --- a folding group mode for Gnus
;; Copyright (C) 1995 Free Software Foundation, Inc.

;; Author: Ilja Weis <kult@uni-paderborn.de>
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

;;; Code:

(require 'gnus)

(defvar gnus-group-topic-face 'underline
  "*Face used to highlight topic headers.")

(defvar gnus-group-topics '(("misc" "." nil))
  "*Alist of newsgroup topics.
This alist has entries of the form

   (TOPIC REGEXP SHOW)

where TOPIC is the name of the topic a group is put in if it matches
REGEXP.  A group can only be in one topic at a time.

If SHOW is nil, newsgroups will be inserted according to
`gnus-group-topic-topics-only', otherwise that variable is ignored and
the groups are always shown if SHOW is true or never if SHOW is a
number.")

(defvar gnus-group-topic-topics-only nil
  "*If non-nil, only the topics will be shown when typing `l' or `L'.")

;; Internal variables.

(defvar gnus-topics-not-listed nil)

;; Functions.

(defun gnus-group-topic-name ()
  (get-text-property (gnus-point-at-bol) 'gnus-topic))

(defun gnus-group-prepare-topics (level &optional all lowest regexp)
  "List all newsgroups with unread articles of level LEVEL or lower, and
use the `gnus-group-topics' to sort the groups.
If ALL is non-nil, list groups that have no unread articles.
If LOWEST is non-nil, list all newsgroups of level LOWEST or higher."
  (set-buffer gnus-group-buffer)
  (let ((buffer-read-only nil)
        (lowest (or lowest 1)))
    
    (erase-buffer)
    
    ;; List dead groups?
    (and (>= level 8) (<= lowest 8)
         (gnus-group-prepare-flat-list-dead 
          (setq gnus-zombie-list (sort gnus-zombie-list 'string<)) 8 ?Z
          regexp))
    
    (and (>= level 9) (<= lowest 9)
         (gnus-group-prepare-flat-list-dead 
          (setq gnus-killed-list (sort gnus-killed-list 'string<)) 9 ?K
          regexp))
    
    ;; Use topics
    (if (< lowest 8)
        (let ((topics gnus-group-topics)
              topic how)
          (erase-buffer)
          (while topics
            (setq topic (car (car topics))
                  how (nth 2 (car topics))
                  topics (cdr topics))

            (add-text-properties 
	     (point)
	     (progn
	       (insert topic "\n")
	       (point))
	     (list 'mouse-face gnus-mouse-face
		   'face gnus-group-topic-face
		   'gnus-topic topic))

            (if (and (or (and (not how) (not gnus-group-topic-topics-only))
			 (and how (not (numberp how))))
		     (not (member topic gnus-topics-not-listed)))
		(gnus-topic-insert-topic topic level all lowest t)
	      (setq gnus-topics-not-listed
		    (cons topic gnus-topics-not-listed)))))))

  (gnus-group-set-mode-line)
  (setq gnus-group-list-mode (cons level all))
  (run-hooks 'gnus-group-prepare-hook))

(defun gnus-topic-insert-topic (topic level &optional all lowest m)
  "Insert all groups matching TOPIC with unread articles of level LEVEL or lower.
If ALL is non-nil, list groups that have no unread articles.  If
LOWEST is non-nil, list all newsgroups of level LOWEST or higher.  If
M is non-nil, nothing will be inserted, but only
`gnus-group-listed-topics' will be changed."
  (let ((buffer-read-only nil)
        (regexp (car (cdr (assoc topic gnus-group-topics))))
        (newsrc (cdr gnus-newsrc-alist))
        info clevel unread group w)
    (setq lowest (or lowest 1))
    (while newsrc
      (setq info (car newsrc)
            group (car info)
            newsrc (cdr newsrc)
            unread (car (gnus-gethash group gnus-newsrc-hashtb)))
      (and unread
           (string-match regexp group)
           (<= (setq clevel (car (cdr info))) level)
           (>= clevel lowest)
           (or all
               (eq unread t)
               (> unread 0)
               (cdr (assq 'tick (nth 3 info))))
           (progn
	     (gnus-group-insert-group-line 
	      nil group (car (cdr info)) (nth 3 info) unread 
	      (nth 4 info))
	     (setq gnus-topics-not-listed
		   (delete topic gnus-topics-not-listed)))))))

(defun gnus-topic-remove-topic ()
  (let ((topic (gnus-group-topic-name))
	buffer-read-only)
    (setq gnus-topics-not-listed (cons topic gnus-topics-not-listed))
    (forward-line 1)
    (delete-region (point) 
		   (or (next-single-property-change (point) 'gnus-topic)
		       (point-max)))))
  
(defun gnus-topic-fold ()
  (let ((topic (gnus-group-topic-name))) 
    (save-excursion
      (if (not (member topic gnus-topics-not-listed))
	  (gnus-topic-remove-topic)
	(forward-line 1)
	(gnus-topic-insert-topic
	 topic (gnus-group-default-level) 
	 (cdr gnus-group-list-mode))))))

;;; gnus-topic.el ends here
