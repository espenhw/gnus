;;; gnus-topic.el --- a folding minor mode for Gnus group buffers
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
(eval-when-compile (require 'cl))

(defvar gnus-topic-mode nil
  "Minor mode for Gnus group buffers.")

(defvar gnus-topic-line-format "%i[ %(%[%n%]%) -- %a ]%v\n"
  "Format of topic lines.
It works along the same lines as a normal formatting string,
with some simple extensions.

%i  Indentation based on topic level.
%n  Topic name.
%v  Nothing if the topic is visible, \"...\" otherwise.
%g  Number of groups in the topic.
%a  Number of unread articles in the groups in the topic.
")

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

(defvar gnus-topic-unique t
  "*If non-nil, each group will only belong to one topic.")

(defvar gnus-topic-hide-subtopics t
  "*If non-nil, hide subtopics along with groups.")

;; Internal variables.

(defvar gnus-topic-killed-topics nil)

(defconst gnus-topic-line-format-alist
  `((?n name ?s)
    (?v visible ?s)
    (?i indentation ?s)
    (?g number-of-groups ?d)
    (?a (gnus-topic-articles-in-topic groups) ?d)
    (?l level ?d)))

(defvar gnus-topic-line-format-spec nil)

;; Functions.

(defun gnus-group-topic-name ()
  "The name of the topic on the current line."
  (let ((topic (get-text-property (gnus-point-at-bol) 'gnus-topic)))
    (and topic (symbol-name topic))))

(defun gnus-group-topic-level ()
  "The level of the topic on the current line."
  (get-text-property (gnus-point-at-bol) 'gnus-topic-level))

(defun gnus-group-prepare-topics (level &optional all lowest regexp list-topic)
  "List all newsgroups with unread articles of level LEVEL or lower, and
use the `gnus-group-topics' to sort the groups.
If ALL is non-nil, list groups that have no unread articles.
If LOWEST is non-nil, list all newsgroups of level LOWEST or higher."
  (set-buffer gnus-group-buffer)
  (let ((buffer-read-only nil)
        (lowest (or lowest 1))
	tlist info)
    
    (unless list-topic 
      (erase-buffer))
    
    ;; List dead groups?
    (when (and (>= level gnus-level-zombie) (<= lowest gnus-level-zombie))
      (gnus-group-prepare-flat-list-dead 
       (setq gnus-zombie-list (sort gnus-zombie-list 'string<)) 
       gnus-level-zombie ?Z
       regexp))
    
    (when (and (>= level gnus-level-killed) (<= lowest gnus-level-killed))
      (gnus-group-prepare-flat-list-dead 
       (setq gnus-killed-list (sort gnus-killed-list 'string<))
       gnus-level-killed ?K
       regexp))
    
    ;; Use topics.
    (when (< lowest gnus-level-zombie)
      (let ((topics (gnus-topic-find-groups nil level all))
	    topic how)
	;; The first time we set the topology to whatever we have
	;; gotten here, which can be rather random.
	(unless gnus-topic-topology
	  (setq gnus-topic-topology
		(list (list "Gnus" 'visible)
		      (mapcar (lambda (topic) (list (car topic) 'visible))
			      topics)))
	  (gnus-topic-enter-dribble))
	
	;; Check that all topics are in the topology.
	(gnus-topic-check-topology topics)

	(if list-topic
	    (let ((top (gnus-topic-find-topology list-topic)))
	      (gnus-topic-prepare-topic 
	       (cdr top) (car top) topics))
	  (gnus-topic-prepare-topic gnus-topic-topology 0 topics)))))

  (gnus-group-set-mode-line)
  (setq gnus-group-list-mode (cons level all))
  (run-hooks 'gnus-group-prepare-hook))

(defun gnus-topic-prepare-topic (topic level topic-alist)
  "Insert TOPIC into the group buffer."
  (let* ((type (pop topic))
	 (groups (nreverse (cdr (assoc (car type) topic-alist))))
	 (visiblep (eq (nth 1 type) 'visible))
	 info)
    ;; Insert the topic line.
    (gnus-topic-insert-topic-line 
     (car type) visiblep
     (not (eq (nth 2 type) 'hidden))
     level groups)
    (when visiblep
      ;; Insert all the groups that belong in this topic.
      (while groups
	(setq info (pop groups))
	(gnus-group-insert-group-line 
	 (gnus-info-group info)
	 (gnus-info-level info) (gnus-info-marks info) 
	 (car (gnus-gethash (gnus-info-group info)
			    gnus-newsrc-hashtb))
	 (gnus-info-method info))))
    ;; Insert any sub-topics.
    (when (or visiblep
	      (and (not gnus-topic-hide-subtopics)
		   (eq (nth 2 type) 'shown)))
      (while topic
	(gnus-topic-prepare-topic (pop topic) (1+ level) topic-alist)))))


(defun gnus-topic-find-groups (&optional topic level all)
  "Find all topics and all groups in all topics.
If TOPIC, just find the groups in that topic."
  (let ((newsrc (cdr gnus-newsrc-alist))
	(topics (if topic
		    (list (list topic))
		  (mapcar (lambda (e) (list (car e)))
			  gnus-group-topics)))
	(topic-alist (if topic (list (assoc topic gnus-group-topics))
		       gnus-group-topics))
        info clevel unread group w lowest gtopic params)
    (setq lowest (or lowest 1))
    (setq all (or all nil))
    (setq level (or level 7))
    ;; We go through the newsrc to look for matches.
    (while newsrc
      (setq info (car newsrc)
            group (gnus-info-group info)
	    params (gnus-info-params info)
            newsrc (cdr newsrc)
            unread (car (gnus-gethash group gnus-newsrc-hashtb)))
      (and 
       unread				; nil means that the group is dead.
       (<= (setq clevel (gnus-info-level info)) level) 
       (>= clevel lowest)		; Is inside the level we want.
       (or all
	   (eq unread t)
	   (> unread 0)
	   (cdr (assq 'tick (gnus-info-marks info))) ; Has right readedness.
	   ;; Check for permanent visibility.
	   (and gnus-permanently-visible-groups
		(string-match gnus-permanently-visible-groups group))
	   (memq 'visible params)
	   (cdr (assq 'visible params)))
       (progn
	 ;; So we find out what topic this group belongs to.  First we
	 ;; check the group parameters.
	 (setq gtopic (cdr (assq 'topic (gnus-info-params info))))
	 ;; On match, we add it.
	 (and (stringp gtopic) 
	      (or (not topic)
		  (string= gtopic topic))
	      (if (setq e (assoc gtopic topics))
		  (setcdr e (cons info (cdr e)))
		(setq topics (cons (list gtopic info) topics))))
	 ;; We look through the topic alist for further matches, if
	 ;; needed.  
	 (if (or (not gnus-topic-unique) (not (stringp gtopic)))
	     (let ((ts topic-alist))
	       (while ts
		 (if (string-match (nth 1 (car ts)) group)
		     (progn
		       (setcdr (setq e (assoc (car (car ts)) topics))
			       (cons info (cdr e)))
		       (and gnus-topic-unique (setq ts nil))))
		 (setq ts (cdr ts))))))))
    topics))

(defun gnus-topic-remove-topic (&optional insert total-remove hide)
  "Remove the current topic."
  (let ((topic (gnus-group-topic-name))
	(level (gnus-group-topic-level))
	(beg (progn (beginning-of-line) (point)))
	buffer-read-only)
    (when topic
      (while (and (zerop (forward-line 1))
		  (> (or (gnus-group-topic-level) (1+ level)) level)))
      (delete-region beg (point))
      (setcar (cdr (car (cdr (gnus-topic-find-topology topic))))
	      (if insert 'visible 'invisible))
      (when hide
	(setcdr (cdr (car (cdr (gnus-topic-find-topology topic))))
		(list hide)))
      (unless total-remove
	(gnus-topic-insert-topic topic)))))

(defun gnus-topic-insert-topic (topic)
  "Insert TOPIC."
  (gnus-group-prepare-topics 
   (car gnus-group-list-mode) (cdr gnus-group-list-mode)
   nil nil topic))
  
(defun gnus-topic-fold (&optional insert)
  "Remove/insert the current topic."
  (let ((topic (gnus-group-topic-name))) 
    (when topic
      (save-excursion
	(gnus-topic-remove-topic (or insert (not (gnus-topic-visible-p))))))))

(defun gnus-group-topic-p ()
  "Return non-nil if the current line is a topic."
  (get-text-property (gnus-point-at-bol) 'gnus-topic))

(defun gnus-topic-visible-p ()
  "Return non-nil if the current topic is visible."
  (get-text-property (gnus-point-at-bol) 'gnus-topic-visible))

(defun gnus-topic-insert-topic-line (name visiblep shownp level groups)
  (let* ((visible (if (and visiblep shownp) "" "..."))
	 (indentation (make-string (* 2 level) ? ))
	 (number-of-groups (length groups))
	 b)
    (beginning-of-line)
    ;; Insert the text.
    (add-text-properties 
     (point)
     (prog1 (1+ (point)) 
       (eval gnus-topic-line-format-spec))
     (list 'gnus-topic (intern name)
	   'gnus-topic-level level
	   'gnus-topic-visible visiblep))))

(defun gnus-topic-check-topology (topic-alist)  
  (let ((topics (gnus-topic-list))
	changed)
    (while topic-alist
      (unless (member (car (car topic-alist)) topics)
	(nconc gnus-topic-topology
	       (list (list (list (car (car topic-alist)) 'visible))))
	(setq changed t))
      (setq topic-alist (cdr topic-alist)))
    (when changed
      (gnus-topic-enter-dribble))))

(defvar gnus-tmp-topics nil)
(defun gnus-topic-list (&optional topology)
  (unless topology
    (setq topology gnus-topic-topology 
	  gnus-tmp-topics nil))
  (push (car (car topology)) gnus-tmp-topics)
  (mapcar 'gnus-topic-list (cdr topology))
  gnus-tmp-topics)

(defun gnus-topic-find-topology (topic &optional topology level remove)
  (unless topology
    (setq topology gnus-topic-topology)
    (setq level 0))
  (let ((top topology)
	result)
    (if (equal (car (car topology)) topic)
	(progn
	  (when remove
	    (delq topology remove))
	  (cons level topology))
      (setq topology (cdr topology))
      (while (and topology
		  (not (setq result (gnus-topic-find-topology
				     topic (car topology) (1+ level)
				     (and remove top)))))
	(setq topology (cdr topology)))
      result)))

(defun gnus-topic-enter-dribble ()
  (gnus-dribble-enter
   (format "(setq gnus-topic-topology '%S)" gnus-topic-topology)))

(defun gnus-topic-articles-in-topic (groups)
  (let ((total 0)
	number)
    (while groups
      (when (numberp (setq number (gnus-group-unread
				   (gnus-info-group (pop groups)))))
	(incf total number)))
    total))

(defun gnus-topic-parent-topic ()
  (save-excursion
    (let (topic)
      (while (not (setq topic (gnus-group-topic-name)))
	(forward-line -1))
      topic)))

(defun gnus-topic-goto-topic (topic)
  (goto-char (point-min))
  (while (and (not (equal topic (gnus-group-topic-name)))
	      (zerop (forward-line 1))))
  (gnus-group-topic-name))
  
(defun gnus-topic-update-topic ()
  (when (and (eq major-mode 'gnus-group-mode)
	     gnus-topic-mode)
    (let ((group (gnus-group-group-name)))
      (gnus-topic-goto-topic (gnus-topic-parent-topic))
      (gnus-topic-remove-topic t)
      (gnus-group-goto-group group)
      (gnus-group-position-point))))

;;; Topic mode, commands and keymap.

(defvar gnus-topic-mode-map nil)
(defvar gnus-group-topic-map nil)

(unless gnus-topic-mode-map
  (setq gnus-topic-mode-map (make-sparse-keymap))
  (define-key gnus-topic-mode-map "=" 'gnus-topic-select-group)
  (define-key gnus-topic-mode-map "\r" 'gnus-topic-select-group)
  (define-key gnus-topic-mode-map " " 'gnus-topic-read-group)
  (define-key gnus-topic-mode-map "\C-k" 'gnus-topic-kill-group)
  (define-key gnus-topic-mode-map "\C-y" 'gnus-topic-yank-group)

  (define-prefix-command 'gnus-group-topic-map)
  (define-key gnus-group-mode-map "T" 'gnus-group-topic-map)
  (define-key gnus-group-topic-map "c" 'gnus-topic-create-topic)
  (define-key gnus-group-topic-map "m" 'gnus-topic-move-to-topic)
  (define-key gnus-group-topic-map "h" 'gnus-topic-hide-topic)
  (define-key gnus-group-topic-map "s" 'gnus-topic-show-topic)
  )

;;;###autoload
(defun gnus-topic-mode (&optional arg redisplay)
  "Minor mode for Gnus group buffers."
  (interactive (list current-prefix-arg t))
  (when (eq major-mode 'gnus-group-mode)
    (make-local-variable 'gnus-topic-mode)
    (setq gnus-topic-mode 
	  (if (null arg) (not gnus-topic-mode)
	    (> (prefix-numeric-value arg) 0)))
    (when gnus-topic-mode
      (setq gnus-topic-line-format-spec 
	    (gnus-parse-format gnus-topic-line-format 
			       gnus-topic-line-format-alist t))
      (unless (assq 'gnus-topic-mode minor-mode-alist)
	(push '(gnus-topic-mode " Topic") minor-mode-alist))
      (unless (assq 'gnus-topic-mode minor-mode-map-alist)
	(push (cons 'gnus-topic-mode gnus-topic-mode-map)
	      minor-mode-map-alist)))
    (make-local-variable 'gnus-group-prepare-function)
    (setq gnus-group-prepare-function 
	  (if gnus-topic-mode
	      'gnus-group-prepare-topics
	    'gnus-group-prepare-flat))
    (add-hook 'gnus-summary-exit-hook 'gnus-topic-update-topic)
    (when redisplay
      (gnus-group-list-groups))))
    
(defun gnus-topic-select-group (&optional all)
  "Select this newsgroup.
No article is selected automatically.
If ALL is non-nil, already read articles become readable.
If ALL is a number, fetch this number of articles."
  (interactive "P")
  (if (gnus-group-topic-p)
      (let ((gnus-group-list-mode 
	     (if all (cons (if (numberp all) all 7) t) gnus-group-list-mode)))
	(gnus-topic-fold all))
    (gnus-group-select-group all)))

(defun gnus-topic-read-group (&optional all no-article group)
  "Read news in this newsgroup.
If the prefix argument ALL is non-nil, already read articles become
readable.  IF ALL is a number, fetch this number of articles.  If the
optional argument NO-ARTICLE is non-nil, no article will be
auto-selected upon group entry.  If GROUP is non-nil, fetch that
group."
  (interactive "P")
  (if (gnus-group-topic-p)
      (let ((gnus-group-list-mode 
	     (if all (cons (if (numberp all) all 7) t) gnus-group-list-mode)))
	(gnus-topic-fold all))
    (gnus-group-read-group all no-article group)))

(defun gnus-topic-create-topic (topic parent &optional previous)
  (interactive 
   (list
    (read-string "Create topic: ")
    (completing-read "Parent topic: " 
		     (mapcar (lambda (l) (list l)) (gnus-topic-list))
		     nil t)))
  ;; Check whether this topic already exists.
  (when (gnus-topic-find-topology topic)
    (error "Topic aleady exists"))
  (let ((top (cdr (gnus-topic-find-topology parent))))
    (unless top
      (error "No such topic: %s" parent))
    (when previous
      (while (and (cdr top)
		  (not (equal (car (car (car top))) previous)))
	(setq top (cdr top))))
    (setcdr top (cons (list (list topic 'visible)) (cdr top))))
  (gnus-topic-enter-dribble)
  (gnus-group-list-groups))

;; Written by "jeff (j.d.) sparkes" <jsparkes@bnr.ca>.
(defun gnus-topic-move-to-topic (n topic)
  "Move the current group to a topic."
  (interactive
   (list current-prefix-arg
	 (completing-read "Move to topic: " 
			  (mapcar (lambda (l) (list l)) (gnus-topic-list)))))
  (let ((groups (gnus-group-process-prefix n)))
    (mapcar (lambda (g) 
	      (gnus-group-remove-mark g)
	      (gnus-group-add-parameter g (cons 'topic topic)))
	    groups)
    (gnus-group-position-point))
  (gnus-topic-enter-dribble)
  (gnus-group-list-groups))

(defun gnus-topic-kill-group (&optional n discard)
  "Kill the next N groups."
  (interactive "P")
  (if (not (gnus-group-topic-p))
      (gnus-group-kill-group n discard)
    (let ((topic (gnus-group-topic-name)))
      (gnus-topic-remove-topic nil t)
      (push (gnus-topic-find-topology topic nil nil gnus-topic-topology)
	    gnus-topic-killed-topics))))
  
(defun gnus-topic-yank-group (&optional arg)
  "Yank the last ARG groups."
  (interactive "p")
  (if (null gnus-topic-killed-topics)
      (gnus-group-yank-group arg)
    (let ((parent (gnus-group-topic-name))
	  (item (nth 1 (pop gnus-topic-killed-topics))))
      (gnus-topic-create-topic
       (car item) (or parent (car (car gnus-topic-topology)))))))

(defun gnus-topic-hide-topic ()
  "Hide all subtopics under the current topic."
  (interactive)
  (when (gnus-group-topic-p)
    (gnus-topic-remove-topic nil nil 'hidden)))

(defun gnus-topic-show-topic ()
  "Show the hidden topic."
  (interactive)
  (when (gnus-group-topic-p)
    (gnus-topic-remove-topic t nil 'shown)))

;;; gnus-topic.el ends here
