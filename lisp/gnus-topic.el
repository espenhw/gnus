;;; gnus-topic.el --- a folding minor mode for Gnus group buffers
;; Copyright (C) 1995,96 Free Software Foundation, Inc.

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

(defvar gnus-topic-mode-hook nil
  "Hook run in topic mode buffers.")

(defvar gnus-topic-line-format "%i[ %(%{%n%}%) -- %A ]%v\n"
  "Format of topic lines.
It works along the same lines as a normal formatting string,
with some simple extensions.

%i  Indentation based on topic level.
%n  Topic name.
%v  Nothing if the topic is visible, \"...\" otherwise.
%g  Number of groups in the topic.
%a  Number of unread articles in the groups in the topic.
%A  Number of unread articles in the groups in the topic and its subtopics.
")

(defvar gnus-group-topic-topics-only nil
  "*If non-nil, only the topics will be shown when typing `l' or `L'.")

(defvar gnus-topic-unique t
  "*If non-nil, each group will only belong to one topic.")

;; Internal variables.

(defvar gnus-topic-killed-topics nil)
(defvar gnus-topic-inhibit-change-level nil)


(defconst gnus-topic-line-format-alist
  `((?n name ?s)
    (?v visible ?s)
    (?i indentation ?s)
    (?g number-of-groups ?d)
    (?a (gnus-topic-articles-in-topic entries) ?d)
    (?A total-number-of-articles ?d)
    (?l level ?d)))

(defvar gnus-topic-line-format-spec nil)
(defvar gnus-topic-active-topology nil)
(defvar gnus-topic-active-alist nil)

;; Functions.

(defun gnus-group-topic-name ()
  "The name of the topic on the current line."
  (get-text-property (gnus-point-at-bol) 'gnus-topic))

(defun gnus-group-topic-level ()
  "The level of the topic on the current line."
  (get-text-property (gnus-point-at-bol) 'gnus-topic-level))

(defun gnus-topic-init-alist ()
  "Initialize the topic structures."
  (setq gnus-topic-topology
	(cons (list "Gnus" 'visible)
	      (mapcar (lambda (topic)
			(list (list (car topic) 'visible)))
		      '(("misc")))))
  (setq gnus-topic-alist
	(list (cons "misc"
		    (mapcar (lambda (info) (gnus-info-group info))
			    (cdr gnus-newsrc-alist)))
	      (list "Gnus")))
  (gnus-topic-enter-dribble))

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
      (if list-topic
	  (let ((top (gnus-topic-find-topology list-topic)))
	    (gnus-topic-prepare-topic (cdr top) (car top) level all))
	(gnus-topic-prepare-topic gnus-topic-topology 0 level all))))

  (gnus-group-set-mode-line)
  (setq gnus-group-list-mode (cons level all))
  (run-hooks 'gnus-group-prepare-hook))

(defun gnus-topic-prepare-topic (topic level &optional list-level all silent)
  "Insert TOPIC into the group buffer.
If SILENT, don't insert anything.  Return the number of unread
articles in the topic and its subtopics."
  (let* ((type (pop topic))
	 (entries (gnus-topic-find-groups (car type) list-level all))
	 (visiblep (and (eq (nth 1 type) 'visible) (not silent)))
	 (gnus-group-indentation (make-string (* 2 level) ? ))
	 (beg (progn (beginning-of-line) (point)))
	 (topic (reverse topic))
	 (unread 0)
	 info entry end)
    ;; Insert any sub-topics.
    (while topic
      (incf unread
	    (gnus-topic-prepare-topic 
	     (pop topic) (1+ level) list-level all
	     (not visiblep))))
    (setq end (point))
    (goto-char beg)
    ;; Insert all the groups that belong in this topic.
    (while (setq info (nth 2 (setq entry (pop entries))))
      (when visiblep 
	(gnus-group-insert-group-line 
	 (gnus-info-group info)
	 (gnus-info-level info) (gnus-info-marks info) 
	 (car entry) (gnus-info-method info)))
      (when (numberp (car entry))
	(incf unread (car entry))))
    (goto-char beg)
    ;; Insert the topic line.
    (unless silent
      (gnus-extent-start-open (point))
      (gnus-topic-insert-topic-line 
       (car type) visiblep
       (not (eq (nth 2 type) 'hidden))
       level entries unread))
    (goto-char end)
    unread))

(defun gnus-topic-find-groups (topic &optional level all)
  "Return entries for all visible groups in TOPIC."
  (let ((groups (cdr (assoc topic gnus-topic-alist)))
        info clevel unread group w lowest gtopic params visible-groups entry)
    (setq lowest (or lowest 1))
    (setq level (or level 7))
    ;; We go through the newsrc to look for matches.
    (while groups
      (setq entry (gnus-gethash (pop groups) gnus-newsrc-hashtb)
	    info (nth 2 entry)
            group (gnus-info-group info)
	    params (gnus-info-params info)
            unread (car entry))
      (and 
       unread				; nil means that the group is dead.
       (<= (setq clevel (gnus-info-level info)) level) 
       (>= clevel lowest)		; Is inside the level we want.
       (or all
	   (and gnus-group-list-inactive-groups
		(eq unread t))
	   (> unread 0)
	   (and gnus-list-groups-with-ticked-articles
		(cdr (assq 'tick (gnus-info-marks info))))
					; Has right readedness.
	   ;; Check for permanent visibility.
	   (and gnus-permanently-visible-groups
		(string-match gnus-permanently-visible-groups group))
	   (memq 'visible params)
	   (cdr (assq 'visible params)))
       ;; Add this group to the list of visible groups.
       (push entry visible-groups)))
    (nreverse visible-groups)))

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
	(if (not (gnus-group-active-topic-p))
	    (gnus-topic-remove-topic
	     (or insert (not (gnus-topic-visible-p))))
	  (let ((gnus-topic-topology gnus-topic-active-topology)
		(gnus-topic-alist gnus-topic-active-alist))
	    (gnus-topic-remove-topic
	     (or insert (not (gnus-topic-visible-p))))))))))

(defun gnus-group-topic-p ()
  "Return non-nil if the current line is a topic."
  (get-text-property (gnus-point-at-bol) 'gnus-topic))

(defun gnus-topic-visible-p ()
  "Return non-nil if the current topic is visible."
  (get-text-property (gnus-point-at-bol) 'gnus-topic-visible))

(defun gnus-topic-insert-topic-line (name visiblep shownp level entries 
					  &optional unread)
  (let* ((visible (if visiblep "" "..."))
	 (indentation (make-string (* 2 level) ? ))
	 (total-number-of-articles unread)
	 (number-of-groups (length entries))
	 (active-topic (eq gnus-topic-alist gnus-topic-active-alist)))
    (beginning-of-line)
    ;; Insert the text.
    (add-text-properties 
     (point)
     (prog1 (1+ (point)) 
       (eval gnus-topic-line-format-spec)
       (gnus-topic-remove-excess-properties))
     (list 'gnus-topic name
	   'gnus-topic-level level
	   'gnus-active active-topic
	   'gnus-topic-visible visiblep))))

(defun gnus-topic-previous-topic (topic)
  "Return the previous topic on the same level as TOPIC."
  (let ((top (cdr (cdr (gnus-topic-find-topology
			(gnus-topic-parent-topic topic))))))
    (unless (equal topic (car (car (car top))))
      (while (and top (not (equal (car (car (car (cdr top)))) topic)))
	(setq top (cdr top)))
      (car (car (car top))))))

(defun gnus-topic-parent-topic (topic &optional topology)
  "Return the parent of TOPIC."
  (unless topology
    (setq topology gnus-topic-topology))
  (let ((parent (car (pop topology)))
	result found)
    (while (and topology
		(not (setq found (equal (car (car (car topology))) topic)))
		(not (setq result (gnus-topic-parent-topic topic 
							   (car topology)))))
      (setq topology (cdr topology)))
    (or result (and found parent))))

(defun gnus-topic-find-topology (topic &optional topology level remove)
  "Return the topology of TOPIC."
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

(defun gnus-topic-check-topology ()  
  ;; The first time we set the topology to whatever we have
  ;; gotten here, which can be rather random.
  (unless gnus-topic-alist
    (gnus-topic-init-alist))

  (let ((topics (gnus-topic-list))
	(alist gnus-topic-alist)
	changed)
    (while alist
      (unless (member (car (car alist)) topics)
	(nconc gnus-topic-topology
	       (list (list (list (car (car alist)) 'visible))))
	(setq changed t))
      (setq alist (cdr alist)))
    (when changed
      (gnus-topic-enter-dribble)))
  (let* ((tgroups (apply 'append (mapcar (lambda (entry) (cdr entry))
					 gnus-topic-alist)))
	 (entry (assoc (caar gnus-topic-topology) gnus-topic-alist))
	 (newsrc gnus-newsrc-alist)
	 group)
    (while newsrc
      (unless (member (setq group (gnus-info-group (pop newsrc))) tgroups)
	(setcdr entry (cons group (cdr entry)))))))

(defvar gnus-tmp-topics nil)
(defun gnus-topic-list (&optional topology)
  (unless topology
    (setq topology gnus-topic-topology 
	  gnus-tmp-topics nil))
  (push (car (car topology)) gnus-tmp-topics)
  (mapcar 'gnus-topic-list (cdr topology))
  gnus-tmp-topics)

(defun gnus-topic-enter-dribble ()
  (gnus-dribble-enter
   (format "(setq gnus-topic-topology '%S)" gnus-topic-topology)))

(defun gnus-topic-articles-in-topic (entries)
  (let ((total 0)
	number)
    (while entries
      (when (numberp (setq number (car (pop entries))))
	(incf total number)))
    total))

(defun gnus-group-parent-topic ()
  "Return the topic the current group belongs in."
  (let ((group (gnus-group-group-name)))
    (if group
	(gnus-group-topic group)
      (gnus-group-topic-name))))

(defun gnus-group-topic (group)
  "Return the topic GROUP is a member of."
  (let ((alist gnus-topic-alist)
	out)
    (while alist
      (when (member group (cdr (car alist)))
	(setq out (car (car alist))
	      alist nil))
      (setq alist (cdr alist)))
    out))

(defun gnus-topic-goto-topic (topic)
  (let ((orig (point)))
    (goto-char (point-min))
    (while (and (not (equal topic (gnus-group-topic-name)))
		(zerop (forward-line 1))))
    (or (gnus-group-topic-name)
	(progn
	  (goto-char orig)
	  nil))))
  
(defun gnus-topic-update-topic ()
  (when (and (eq major-mode 'gnus-group-mode)
	     gnus-topic-mode)
    (let ((group (gnus-group-group-name)))
      (gnus-topic-goto-topic (gnus-group-parent-topic))
      (gnus-topic-update-topic-line)
      (gnus-group-goto-group group)
      (gnus-group-position-point))))

(defun gnus-topic-update-topic-line (&optional topic level)
  (unless topic
    (setq topic gnus-topic-topology)
    (setq level 0))
  (let* ((type (pop topic))
	 (buffer-read-only nil)
	 (entries (gnus-topic-find-groups 
		   (car type) (car gnus-group-list-mode)
		   (cdr gnus-group-list-mode)))
	 (visiblep (eq (nth 1 type) 'visible))
	 (unread 0)
	 info entry end)
    ;; Tally any sub-topics.
    (while topic
      (incf unread (gnus-topic-update-topic-line (pop topic) (1+ level))))
    ;; Tally all the groups that belong in this topic.
    (while (setq info (nth 2 (setq entry (pop entries))))
      (when (numberp (car entry))
	(incf unread (car entry))))
    ;; Insert the topic line.
    (when (gnus-topic-goto-topic (car type))
      (gnus-topic-insert-topic-line 
       (car type) visiblep
       (not (eq (nth 2 type) 'hidden))
       level entries unread)
      (gnus-delete-line))
    unread))

(defun gnus-topic-grok-active (&optional force)
  "Parse all active groups and create topic structures for them."
  ;; First we make sure that we have really read the active file. 
  (when (or force
	    (not gnus-topic-active-alist))
    (when (or force
	      (not gnus-have-read-active-file))
      (let ((gnus-read-active-file t))
	(gnus-read-active-file)))
    (let (topology groups alist)
      ;; Get a list of all groups available.
      (mapatoms (lambda (g) (when (symbol-value g)
			      (push (symbol-name g) groups)))
		gnus-active-hashtb)
      (setq groups (sort groups 'string<))
      ;; Init the variables.
      (setq gnus-topic-active-topology '(("" visible)))
      (setq gnus-topic-active-alist nil)
      ;; Descend the top-level hierarchy.
      (gnus-topic-grok-active-1 gnus-topic-active-topology groups)
      ;; Set the top-level topic names to something nice.
      (setcar (car gnus-topic-active-topology) "Gnus active")
      (setcar (car gnus-topic-active-alist) "Gnus active"))))

(defun gnus-topic-grok-active-1 (topology groups)
  (let* ((name (caar topology))
	 (prefix (concat "^" (regexp-quote name)))
	 tgroups nprefix ntopology group)
    (while (and groups
		(string-match prefix (setq group (car groups))))
      (if (not (string-match "\\." group (match-end 0)))
	  ;; There are no further hierarchies here, so we just
	  ;; enter this group into the list belonging to this
	  ;; topic.
	  (push (pop groups) tgroups)
	;; New sub-hierarchy, so we add it to the topology.
	(nconc topology (list (setq ntopology 
				    (list (list (substring 
						 group 0 (match-end 0))
						'invisible)))))
	;; Descend the hierarchy.
	(setq groups (gnus-topic-grok-active-1 ntopology groups))))
    ;; We remove the trailing "." from the topic name.
    (setq name
	  (if (string-match "\\.$" name)
	      (substring name 0 (match-beginning 0))
	    name))
    ;; Add this topic and its groups to the topic alist.
    (push (cons name (nreverse tgroups)) gnus-topic-active-alist)
    (setcar (car topology) name)
    ;; We return the rest of the groups that didn't belong
    ;; to this topic.
    groups))

(defun gnus-group-active-topic-p ()
  "Return whether the current active comes from the active topics."
  (save-excursion
    (beginning-of-line)
    (get-text-property (point) 'gnus-active)))

;;; Topic mode, commands and keymap.

(defvar gnus-topic-mode-map nil)
(defvar gnus-group-topic-map nil)

(unless gnus-topic-mode-map
  (setq gnus-topic-mode-map (make-sparse-keymap))

  ;; Override certain group mode keys.
  (gnus-define-keys
   gnus-topic-mode-map
   "=" gnus-topic-select-group
   "\r" gnus-topic-select-group
   " " gnus-topic-read-group
   "\C-k" gnus-topic-kill-group
   "\C-y" gnus-topic-yank-group
   "\M-g" gnus-topic-get-new-news-this-topic
   "\C-i" gnus-topic-indent
   "AT" gnus-topic-list-active
   gnus-mouse-2 gnus-mouse-pick-topic)

  ;; Define a new submap.
  (gnus-define-keys
   (gnus-group-topic-map "T" gnus-group-mode-map)
   "#" gnus-topic-mark-topic
   "n" gnus-topic-create-topic
   "m" gnus-topic-move-group
   "c" gnus-topic-copy-group
   "h" gnus-topic-hide-topic
   "s" gnus-topic-show-topic
   "M" gnus-topic-move-matching
   "C" gnus-topic-copy-matching
   "r" gnus-topic-rename
   "\177" gnus-topic-delete))

(defun gnus-topic-make-menu-bar ()
  (unless (boundp 'gnus-topic-menu)
    (easy-menu-define
     gnus-topic-menu gnus-topic-mode-map ""
     '("Topics"
       ["Toggle topics" gnus-topic-mode t]
       ("Groups"
	["Copy" gnus-topic-copy-group t]
	["Move" gnus-topic-move-group t]
	["Copy matching" gnus-topic-copy-matching t]
	["Move matching" gnus-topic-move-matching t])
       ("Topics"
	["Show" gnus-topic-show-topic t]
	["Hide" gnus-topic-hide-topic t]
	["Delete" gnus-topic-delete t]
	["Rename" gnus-topic-rename t]
	["Create" gnus-topic-create-topic t]
	["Mark" gnus-topic-mark-topic t]
	["Indent" gnus-topic-indent t])
       ["List active" gnus-topic-list-active t]))))


(defun gnus-topic-mode (&optional arg redisplay)
  "Minor mode for topicsifying Gnus group buffers."
  (interactive (list current-prefix-arg t))
  (when (eq major-mode 'gnus-group-mode)
    (make-local-variable 'gnus-topic-mode)
    (setq gnus-topic-mode 
	  (if (null arg) (not gnus-topic-mode)
	    (> (prefix-numeric-value arg) 0)))
    ;; Infest Gnus with topics.
    (when gnus-topic-mode
      (when (and menu-bar-mode
		 (gnus-visual-p 'topic-menu 'menu))
	(gnus-topic-make-menu-bar))
      (setq gnus-topic-line-format-spec 
	    (gnus-parse-format gnus-topic-line-format 
			       gnus-topic-line-format-alist t))
      (unless (assq 'gnus-topic-mode minor-mode-alist)
	(push '(gnus-topic-mode " Topic") minor-mode-alist))
      (unless (assq 'gnus-topic-mode minor-mode-map-alist)
	(push (cons 'gnus-topic-mode gnus-topic-mode-map)
	      minor-mode-map-alist))
      (add-hook 'gnus-summary-exit-hook 'gnus-topic-update-topic)
      (make-local-variable 'gnus-group-prepare-function)
      (setq gnus-group-prepare-function 'gnus-group-prepare-topics)
      (make-local-variable 'gnus-group-goto-next-group-function)
      (setq gnus-group-goto-next-group-function 
	    'gnus-topic-goto-next-group)
      (setq gnus-group-change-level-function 'gnus-topic-change-level)
      (run-hooks 'gnus-topic-mode-hook)
      ;; We check the topology.
      (gnus-topic-check-topology))
    ;; Remove topic infestation.
    (unless gnus-topic-mode
      (remove-hook 'gnus-summary-exit-hook 'gnus-topic-update-topic)
      (remove-hook 'gnus-group-change-level-function 
		   'gnus-topic-change-level)
      (setq gnus-group-prepare-function 'gnus-group-prepare-flat))
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

(defun gnus-mouse-pick-topic (e)
  "Select the group or topic under the mouse pointer."
  (interactive "e")
  (mouse-set-point e)
  (gnus-topic-read-group nil))

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
    (completing-read "Parent topic: " gnus-topic-alist nil t)))
  ;; Check whether this topic already exists.
  (when (gnus-topic-find-topology topic)
    (error "Topic aleady exists"))
  (unless parent
    (setq parent (car (car gnus-topic-topology))))
  (let ((top (cdr (gnus-topic-find-topology parent))))
    (unless top
      (error "No such parent topic: %s" parent))
    (if previous
	(progn
	  (while (and (cdr top)
		      (not (equal (car (car (car (cdr top)))) previous)))
	    (setq top (cdr top)))
	  (setcdr top (cons (list (list topic 'visible)) (cdr top))))
      (nconc top (list (list (list topic 'visible)))))
    (unless (assoc topic gnus-topic-alist)
      (push (list topic) gnus-topic-alist)))
  (gnus-topic-enter-dribble)
  (gnus-group-list-groups))

(defun gnus-topic-move-group (n topic &optional copyp)
  "Move the current group to a topic."
  (interactive
   (list current-prefix-arg
	 (completing-read "Move to topic: " gnus-topic-alist nil t)))
  (let ((groups (gnus-group-process-prefix n))
	(topicl (assoc topic gnus-topic-alist))
	entry)
    (unless topicl
      (error "No such topic: %s" topic))
    (mapcar (lambda (g) 
	      (gnus-group-remove-mark g)
	      (when (and
		     (setq entry (assoc (gnus-group-topic g) gnus-topic-alist))
		     (not copyp))
		(setcdr entry (delete g (cdr entry))))
	      (nconc topicl (list g)))
	    groups)
    (gnus-group-position-point))
  (gnus-topic-enter-dribble)
  (gnus-group-list-groups))

(defun gnus-topic-copy-group (n topic)
  "Copy the current group to a topic."
  (interactive
   (list current-prefix-arg
	 (completing-read "Copy to topic: " gnus-topic-alist nil t)))
  (gnus-topic-move-group n topic t))

(defun gnus-topic-change-level (group level oldlevel)
  "Run when changing levels to enter/remove groups from topics."
  (when (and gnus-topic-mode 
	     (not gnus-topic-inhibit-change-level))
    ;; Remove the group from the topics.
    (when (and (< oldlevel gnus-level-zombie)
	       (>= level gnus-level-zombie))
      (let (alist)
	(when (setq alist (assoc (gnus-group-topic group) gnus-topic-alist))
	  (setcdr alist (delete group (cdr alist))))))
    ;; If the group is subscribed. then we enter it into the topics.
    (when (and (< level gnus-level-zombie)
	       (>= oldlevel gnus-level-zombie))
      (let ((entry (assoc (caar gnus-topic-topology) gnus-topic-alist)))
	(setcdr entry (cons group (cdr entry)))))))

(defun gnus-topic-goto-next-group (group props)
  "Go to group or the next group after group."
  (if (null group)
      (gnus-topic-goto-topic (cadr (memq 'gnus-topic props)))
    (if (gnus-group-goto-group group)
	t
      ;; The group is no longer visible.
      (let* ((list (assoc (gnus-group-topic group) gnus-topic-alist))
	     (after (cdr (member group (cdr list)))))
	;; First try to put point on a group after the current one.
	(while (and after
		    (not (gnus-group-goto-group (car after))))
	  (setq after (cdr after)))
	;; Then try to put point on a group before point.
	(unless after
	  (setq after (cdr (member group (reverse (cdr list)))))
	  (while (and after 
		      (not (gnus-group-goto-group (car after))))
	    (setq after (cdr after))))
	;; Finally, just put point on the topic.
	(unless after
	  (gnus-topic-goto-topic (car list))
	  (setq after nil))
	t))))

(defun gnus-topic-kill-group (&optional n discard)
  "Kill the next N groups."
  (interactive "P")
  (if (gnus-group-topic-p)
      (let ((topic (gnus-group-topic-name)))
	(gnus-topic-remove-topic nil t)
	(push (gnus-topic-find-topology topic nil nil gnus-topic-topology)
	      gnus-topic-killed-topics))
    (gnus-group-kill-group n discard)))
  
(defun gnus-topic-yank-group (&optional arg)
  "Yank the last topic."
  (interactive "p")
  (if gnus-topic-killed-topics
      (let ((previous (gnus-group-parent-topic))
	    (item (nth 1 (pop gnus-topic-killed-topics))))
	(gnus-topic-create-topic
	 (car item) (gnus-topic-parent-topic previous) previous))
    (let* ((prev (gnus-group-group-name))
	   (gnus-topic-inhibit-change-level t)
	   (gnus-group-indentation
	    (make-string 
	     (* 2 (or (save-excursion
			(gnus-topic-goto-topic (gnus-group-parent-topic))
			(gnus-group-topic-level)) 0)) ? ))
	   yanked group alist)
      ;; We first yank the groups the normal way...
      (setq yanked (gnus-group-yank-group arg))
      ;; Then we enter the yanked groups into the topics they belong
      ;; to. 
      (setq alist (assoc (save-excursion
			   (forward-line -1)
			   (gnus-group-parent-topic))
			 gnus-topic-alist))
      (when (stringp yanked)
	(setq yanked (list yanked)))
      (if (not prev)
	  (nconc alist yanked)
	(if (not (cdr alist))
	    (setcdr alist (nconc yanked (cdr alist)))
	  (while (cdr alist)
	    (when (equal (car (cdr alist)) prev)
	      (setcdr alist (nconc yanked (cdr alist)))
	      (setq alist nil))
	    (setq alist (cdr alist))))))))

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

(defun gnus-topic-mark-topic (topic)
  "Mark all groups in the topic with the process mark."
  (interactive (list (gnus-group-parent-topic)))
  (let ((groups (cdr (gnus-topic-find-groups topic))))
    (while groups
      (gnus-group-set-mark (gnus-info-group (nth 2 (pop groups)))))))

(defun gnus-topic-get-new-news-this-topic (&optional n)
  "Check for new news in the current topic."
  (interactive "P")
  (if (not (gnus-group-topic-p))
      (gnus-group-get-new-news-this-group n)
    (gnus-topic-mark-topic (gnus-group-topic-name))
    (gnus-group-get-new-news-this-group)))

(defun gnus-topic-move-matching (regexp topic &optional copyp)
  "Move all groups that match REGEXP to some topic."
  (interactive
   (let (topic)
     (nreverse
      (list
       (setq topic (completing-read "Move to topic: " gnus-topic-alist nil t))
       (read-string (format "Move to %s (regexp): " topic))))))
  (gnus-group-mark-regexp regexp)
  (gnus-topic-move-group nil topic copyp))

(defun gnus-topic-copy-matching (regexp topic &optional copyp)
  "Copy all groups that match REGEXP to some topic."
  (interactive
   (let (topic)
     (nreverse
      (list
       (setq topic (completing-read "Copy to topic: " gnus-topic-alist nil t))
       (read-string (format "Copy to %s (regexp): " topic))))))
  (gnus-topic-move-matching regexp topic t))

(defun gnus-topic-delete (topic)
  "Delete a topic."
  (interactive (list (gnus-group-topic-name)))
  (unless topic
    (error "No topic to be deleted"))
  (let ((entry (assoc topic gnus-topic-alist))
	(buffer-read-only nil))
    (when (cdr entry)
      (error "Topic not empty"))
    ;; Delete if visible.
    (when (gnus-topic-goto-topic topic)
      (gnus-delete-line))
    ;; Remove from alist.
    (setq gnus-topic-alist (delq entry gnus-topic-alist))
    ;; Remove from topology.
    (gnus-topic-find-topology topic nil nil 'delete)))

(defun gnus-topic-rename (old-name new-name)
  "Rename a topic."
  (interactive
   (let (topic)
     (list
      (setq topic (completing-read "Rename topic: " gnus-topic-alist nil t))
      (read-string (format "Rename %s to: " topic)))))
  (let ((top (gnus-topic-find-topology old-name))
	(entry (assoc old-name gnus-topic-alist)))
    (when top
      (setcar (car (cdr top)) new-name))
    (when entry 
      (setcar entry new-name))
    (gnus-group-list-groups)))

(defun gnus-topic-indent (&optional unindent)
  "Indent a topic -- make it a sub-topic of the previous topic.
If UNINDENT, remove an indentation."
  (interactive "P")
  (if unindent
      (gnus-topic-unindent)
    (let* ((topic (gnus-group-parent-topic))
	   (parent (gnus-topic-previous-topic topic)))
      (unless parent
	(error "Nothing to indent %s into" topic))
      (when topic
	(gnus-topic-goto-topic topic)
	(gnus-topic-kill-group)
	(gnus-topic-create-topic topic parent)
	(gnus-topic-goto-topic topic)))))

(defun gnus-topic-unindent ()
  "Unindent a topic."
  (interactive)
  (let* ((topic (gnus-group-parent-topic))
	 (parent (gnus-topic-parent-topic topic))
	 (grandparent (gnus-topic-parent-topic parent)))
    (unless grandparent
      (error "Nothing to indent %s into" topic))
    (when topic
      (gnus-topic-goto-topic topic)
      (gnus-topic-kill-group)
      (gnus-topic-create-topic topic grandparent)
      (gnus-topic-goto-topic topic))))

(defun gnus-topic-list-active (&optional force)
  "List all groups that Gnus knows about in a topicsified fashion.
If FORCE, always re-read the active file."
  (interactive "P")
  (gnus-topic-grok-active)
  (let ((gnus-topic-topology gnus-topic-active-topology)
	(gnus-topic-alist gnus-topic-active-alist))
    (gnus-group-list-groups 9 nil 1)))

(provide 'gnus-topic)

;;; gnus-topic.el ends here
