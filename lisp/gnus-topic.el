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

(defvar gnus-topic-line-format "%i[ %(%{%n%}%) -- %a ]%v\n"
  "Format of topic lines.
It works along the same lines as a normal formatting string,
with some simple extensions.

%i  Indentation based on topic level.
%n  Topic name.
%v  Nothing if the topic is visible, \"...\" otherwise.
%g  Number of groups in the topic.
%a  Number of unread articles in the groups in the topic.
")

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
    (?a number-of-articles ?d)
    (?l level ?d)))

(defvar gnus-topic-line-format-spec nil)

;; Functions.

(defun gnus-group-topic-name ()
  "The name of the topic on the current line."
  (get-text-property (gnus-point-at-bol) 'gnus-topic))

(defun gnus-group-topic-level ()
  "The level of the topic on the current line."
  (get-text-property (gnus-point-at-bol) 'gnus-topic-level))

(defun gnus-topic-init-alist ()
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
      (let (topics topic how)
	;; The first time we set the topology to whatever we have
	;; gotten here, which can be rather random.
	(unless gnus-topic-alist
	  (gnus-topic-init-alist))
	(gnus-topic-check-topology)

	(if list-topic
	    (let ((top (gnus-topic-find-topology list-topic)))
	      (gnus-topic-prepare-topic (cdr top) (car top) level all))
	  (gnus-topic-prepare-topic gnus-topic-topology 0 level all)))))

  (gnus-group-set-mode-line)
  (setq gnus-group-list-mode (cons level all))
  (run-hooks 'gnus-group-prepare-hook))

(defun gnus-topic-prepare-topic (topic level &optional list-level all)
  "Insert TOPIC into the group buffer."
  (let* ((type (pop topic))
	 (entries (gnus-topic-find-groups (car type) list-level all))
	 (visiblep (eq (nth 1 type) 'visible))
	 info entry)
    ;; Insert the topic line.
    (gnus-topic-insert-topic-line 
     (car type) visiblep
     (not (eq (nth 2 type) 'hidden))
     level entries)
    (when visiblep
      ;; Insert all the groups that belong in this topic.
      (while entries
	(setq entry (pop entries)
	      info (nth 2 entry))
	(gnus-group-insert-group-line 
	 (gnus-info-group info)
	 (gnus-info-level info) (gnus-info-marks info) 
	 (car entry) (gnus-info-method info))))
    ;; Insert any sub-topics.
    (when (or visiblep
	      (and (not gnus-topic-hide-subtopics)
		   (eq (nth 2 type) 'shown)))
      (while topic
	(gnus-topic-prepare-topic (pop topic) (1+ level) list-level all)))))

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
	   (eq unread t)
	   (> unread 0)
	   (cdr (assq 'tick (gnus-info-marks info))) ; Has right readedness.
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
	(gnus-topic-remove-topic (or insert (not (gnus-topic-visible-p))))))))

(defun gnus-group-topic-p ()
  "Return non-nil if the current line is a topic."
  (get-text-property (gnus-point-at-bol) 'gnus-topic))

(defun gnus-topic-visible-p ()
  "Return non-nil if the current topic is visible."
  (get-text-property (gnus-point-at-bol) 'gnus-topic-visible))

(defun gnus-topic-insert-topic-line (name visiblep shownp level entries)
  (let* ((visible (if (and visiblep shownp) "" "..."))
	 (indentation (make-string (* 2 level) ? ))
	 (number-of-articles (gnus-topic-articles-in-topic entries))
	 (number-of-groups (length entries)))
    (beginning-of-line)
    ;; Insert the text.
    (add-text-properties 
     (point)
     (prog1 (1+ (point)) 
       (eval gnus-topic-line-format-spec))
     (list 'gnus-topic name
	   'gnus-topic-level level
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
	 (entry (assoc "Gnus" gnus-topic-alist))
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
  (goto-char (point-min))
  (while (and (not (equal topic (gnus-group-topic-name)))
	      (zerop (forward-line 1))))
  (gnus-group-topic-name))
  
(defun gnus-topic-update-topic ()
  (when (and (eq major-mode 'gnus-group-mode)
	     gnus-topic-mode)
    (let ((group (gnus-group-group-name)))
      (gnus-topic-goto-topic (gnus-group-parent-topic))
      (gnus-topic-update-topic-line)
      (gnus-group-goto-group group)
      (gnus-group-position-point))))

(defun gnus-topic-update-topic-line ()
  (let* ((buffer-read-only nil)
	 (topic (gnus-group-topic-name))
	 (entry (gnus-topic-find-topology topic))
	 (level (car entry))
	 (type (nth 1 entry))
	 (entries (gnus-topic-find-groups (car type)))
	 (visiblep (eq (nth 1 type) 'visible)))
    ;; Insert the topic line.
    (if topic
	(progn
	  (gnus-delete-line)
	  (gnus-topic-insert-topic-line 
	   (car type) visiblep
	   (not (eq (nth 2 type) 'hidden)) level entries)))))

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
  (define-key gnus-topic-mode-map "\M-g" 'gnus-topic-get-new-news-this-topic)
  (define-key gnus-topic-mode-map "\C-i" 'gnus-topic-indent)

  (define-prefix-command 'gnus-group-topic-map)
  (define-key gnus-group-mode-map "T" 'gnus-group-topic-map)
  (define-key gnus-group-topic-map "#" 'gnus-topic-mark-topic)
  (define-key gnus-group-topic-map "n" 'gnus-topic-create-topic)
  (define-key gnus-group-topic-map "m" 'gnus-topic-move-group)
  (define-key gnus-group-topic-map "c" 'gnus-topic-copy-group)
  (define-key gnus-group-topic-map "h" 'gnus-topic-hide-topic)
  (define-key gnus-group-topic-map "s" 'gnus-topic-show-topic)
  (define-key gnus-group-topic-map "M" 'gnus-topic-move-matching)
  (define-key gnus-group-topic-map "C" 'gnus-topic-copy-matching)
  (define-key gnus-group-topic-map "r" 'gnus-topic-rename)
  (define-key gnus-group-topic-map "\177" 'gnus-topic-delete)

  (define-key gnus-topic-mode-map gnus-mouse-2 'gnus-mouse-pick-topic)
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
  (let ((top (cdr (gnus-topic-find-topology parent))))
    (unless top
      (error "No such topic: %s" parent))
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
  "Yank the last topic."
  (interactive "p")
  (if (null gnus-topic-killed-topics)
      (gnus-group-yank-group arg)
    (let ((previous (gnus-group-parent-topic))
	  (item (nth 1 (pop gnus-topic-killed-topics))))
      (gnus-topic-create-topic
       (car item) (gnus-topic-parent-topic previous) previous))))

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
     (list
      (setq topic (completing-read "Move to topic: " gnus-topic-alist nil t))
      (read-string (format "Move to %s (regexp): " topic)))))
  (gnus-group-mark-regexp regexp)
  (gnus-topic-move-group nil topic copyp))

(defun gnus-topic-copy-matching (regexp topic &optional copyp)
  "Copy all groups that match REGEXP to some topic."
  (interactive
   (let (topic)
     (list
      (setq topic (completing-read "Copy to topic: " gnus-topic-alist nil t))
      (read-string (format "Copy to %s (regexp): " topic)))))
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
   (list
    (completing-read "Rename topic: " gnus-topic-alist nil t)
    (read-string (format "Rename %s to: "))))
  (let ((top (gnus-topic-find-topology old-name))
	(entry (assoc old-name gnus-topic-alist)))
    (when top
      (setcar (car (cdr top)) new-name))
    (when entry 
      (setcar entry new-name))))

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
	(gnus-topic-create-topic topic parent)))))

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
      (gnus-topic-create-topic topic grandparent))))

(provide 'gnus-topic)

;;; gnus-topic.el ends here
