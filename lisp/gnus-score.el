;;; gnus-score --- scoring code for Gnus
;; Copyright (C) 1995 Free Software Foundation, Inc.

;; Author: Per Abrahamsen <amanda@iesd.auc.dk>
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

(defvar gnus-score-expiry-days 7
  "*Number of days before unused score file entries are expired.")

(defvar gnus-orphan-score nil
  "*All orphans get this score added. Set in the score file.")

(defvar gnus-default-adaptive-score-alist
  '((gnus-unread-mark)
    (gnus-ticked-mark (from 4))
    (gnus-dormant-mark (from 5))
    (gnus-del-mark (from -4) (subject -1))
    (gnus-read-mark (from 4) (subject 2))
    (gnus-expirable-mark (from -1) (subject -1))
    (gnus-killed-mark (from -1) (subject -3))
    (gnus-kill-file-mark)
    (gnus-catchup-mark (from -1) (subject -1)))
  "*Alist of marks and scores.")



;; Internal variables.

(defvar gnus-current-score-file nil)

(defvar gnus-adaptive-score-alist gnus-default-adaptive-score-alist)

(defvar gnus-score-alist nil
  "Alist containing score information.
The keys can be symbols or strings.  The following symbols are defined. 

touched: If this alist has been modified.
mark:    Automatically mark articles below this.
expunge: Automatically expunge articles below this.
files:   List of other SCORE files to load when loading this one.
eval:    Sexp to be evaluated when the score file is loaded.

String entries have the form (HEADER (MATCH TYPE SCORE DATE) ...) 
where HEADER is the header being scored, MATCH is the string we are
looking for, TYPE is a flag indicating whether it should use regexp or
substring matching, SCORE is the score to add and DATE is the date
of the last succesful match.")

(defvar gnus-score-cache nil)
(defvar gnus-scores-articles nil)
(defvar gnus-scores-exclude-files nil)
(defvar gnus-header-index nil)
(defvar gnus-score-index nil)

(defvar gnus-winconf-edit-score nil)

(autoload 'gnus-uu-ctl-map "gnus-uu" nil nil 'keymap)

;;; Summary mode score maps.

(defvar gnus-summary-score-map nil)
(defvar gnus-summary-increase-map nil)
(defvar gnus-summary-inc-subject-map nil)
(defvar gnus-summary-inc-author-map nil)
(defvar gnus-summary-inc-body-map nil)
(defvar gnus-summary-inc-id-map nil)
(defvar gnus-summary-inc-xref-map nil)
(defvar gnus-summary-inc-thread-map nil)
(defvar gnus-summary-inc-fol-map nil)
(defvar gnus-summary-lower-map nil)
(defvar gnus-summary-low-subject-map nil)
(defvar gnus-summary-low-author-map nil)
(defvar gnus-summary-low-body-map nil)
(defvar gnus-summary-low-id-map nil)
(defvar gnus-summary-low-xref-map nil)
(defvar gnus-summary-low-thread-map nil)
(defvar gnus-summary-low-fol-map nil)

  (define-prefix-command 'gnus-summary-score-map)
  (define-key gnus-summary-various-map "S" 'gnus-summary-score-map)
  (define-key gnus-summary-score-map "s" 'gnus-summary-set-score)
  (define-key gnus-summary-score-map "c" 'gnus-score-change-score-file)
  (define-key gnus-summary-score-map "m" 'gnus-score-set-mark-below)
  (define-key gnus-summary-score-map "x" 'gnus-score-set-expunge-below)
  (define-key gnus-summary-score-map "e" 'gnus-score-edit-alist)
  (define-key gnus-summary-score-map "f" 'gnus-score-edit-file)


  (define-prefix-command 'gnus-summary-increase-map)
  (define-key gnus-summary-mode-map "I" gnus-summary-increase-map)

  (define-key gnus-summary-increase-map "i" 'gnus-summary-raise-same-subject-and-select)
  (define-key gnus-summary-increase-map "I" 'gnus-summary-raise-same-subject)
  (define-key gnus-summary-increase-map "\C-i" 'gnus-summary-raise-score)

  (define-prefix-command 'gnus-summary-inc-subject-map)
  (define-key gnus-summary-increase-map "s" gnus-summary-inc-subject-map)
  (define-key gnus-summary-increase-map "S" 'gnus-summary-temporarily-raise-by-subject)
  (define-key gnus-summary-inc-subject-map "s" 'gnus-summary-temporarily-raise-by-subject)
  (define-key gnus-summary-inc-subject-map "S" 'gnus-summary-raise-by-subject)
  (define-key gnus-summary-inc-subject-map "t" 'gnus-summary-temporarily-raise-by-subject)
  (define-key gnus-summary-inc-subject-map "p" 'gnus-summary-raise-by-subject)

  (define-prefix-command 'gnus-summary-inc-author-map)
  (define-key gnus-summary-increase-map "a" 'gnus-summary-inc-author-map)
  (define-key gnus-summary-increase-map "A" 'gnus-summary-temporarily-raise-by-author)
  (define-key gnus-summary-inc-author-map "a" 'gnus-summary-temporarily-raise-by-author)
  (define-key gnus-summary-inc-author-map "A" 'gnus-summary-raise-by-author)
  (define-key gnus-summary-inc-author-map "t" 'gnus-summary-temporarily-raise-by-author)
  (define-key gnus-summary-inc-author-map "p" 'gnus-summary-raise-by-author)

  (define-prefix-command 'gnus-summary-inc-body-map)
  (define-key gnus-summary-increase-map "b" 'gnus-summary-inc-body-map)
  (define-key gnus-summary-increase-map "B" 'gnus-summary-temporarily-raise-by-body)
  (define-key gnus-summary-inc-body-map "b" 'gnus-summary-temporarily-raise-by-body)
  (define-key gnus-summary-inc-body-map "B" 'gnus-summary-raise-by-body)
  (define-key gnus-summary-inc-body-map "t" 'gnus-summary-temporarily-raise-by-body)
  (define-key gnus-summary-inc-body-map "p" 'gnus-summary-raise-by-body)

  (define-prefix-command 'gnus-summary-inc-id-map)
  (define-key gnus-summary-increase-map "i" 'gnus-summary-inc-id-map)
  (define-key gnus-summary-increase-map "I" 'gnus-summary-temporarily-raise-by-id)
  (define-key gnus-summary-inc-id-map "i" 'gnus-summary-temporarily-raise-by-id)
  (define-key gnus-summary-inc-id-map "I" 'gnus-summary-raise-by-id)
  (define-key gnus-summary-inc-id-map "t" 'gnus-summary-temporarily-raise-by-id)
  (define-key gnus-summary-inc-id-map "p" 'gnus-summary-raise-by-id)

  (define-prefix-command 'gnus-summary-inc-thread-map)
  (define-key gnus-summary-increase-map "t" 'gnus-summary-inc-thread-map)
  (define-key gnus-summary-increase-map "T" 'gnus-summary-temporarily-raise-by-thread)
  (define-key gnus-summary-inc-thread-map "t" 'gnus-summary-temporarily-raise-by-thread)
  (define-key gnus-summary-inc-thread-map "T" 'gnus-summary-raise-by-thread)
  (define-key gnus-summary-inc-thread-map "t" 'gnus-summary-temporarily-raise-by-thread)
  (define-key gnus-summary-inc-thread-map "p" 'gnus-summary-raise-by-thread)

  (define-prefix-command 'gnus-summary-inc-xref-map)
  (define-key gnus-summary-increase-map "x" 'gnus-summary-inc-xref-map)
  (define-key gnus-summary-increase-map "X" 'gnus-summary-temporarily-raise-by-xref)
  (define-key gnus-summary-inc-xref-map "x" 'gnus-summary-temporarily-raise-by-xref)
  (define-key gnus-summary-inc-xref-map "X" 'gnus-summary-raise-by-xref)
  (define-key gnus-summary-inc-xref-map "t" 'gnus-summary-temporarily-raise-by-xref)
  (define-key gnus-summary-inc-xref-map "p" 'gnus-summary-raise-by-xref)

  (define-prefix-command 'gnus-summary-inc-fol-map)
  (define-key gnus-summary-increase-map "f" 'gnus-summary-inc-fol-map)
  (define-key gnus-summary-increase-map "F" 'gnus-summary-raise-followups-to-author)
  (define-key gnus-summary-inc-fol-map "f" 'gnus-summary-raise-followups-to-author)
  (define-key gnus-summary-inc-fol-map "F" 'gnus-summary-raise-followups-to-author)
  (define-key gnus-summary-inc-fol-map "t" 'gnus-summary-raise-followups-to-author)
  (define-key gnus-summary-inc-fol-map "p" 'gnus-summary-raise-followups-to-author)


  (define-prefix-command 'gnus-summary-lower-map)
  (define-key gnus-summary-mode-map "L" 'gnus-summary-lower-map)

  (define-key gnus-summary-lower-map "l" 'gnus-summary-lower-same-subject-and-select)
  (define-key gnus-summary-lower-map "L" 'gnus-summary-lower-same-subject)
  (define-key gnus-summary-lower-map "\C-l" 'gnus-summary-lower-score)

  (define-prefix-command 'gnus-summary-low-subject-map)
  (define-key gnus-summary-lower-map "s" 'gnus-summary-low-subject-map)
  (define-key gnus-summary-lower-map "S" 'gnus-summary-temporarily-lower-by-subject)
  (define-key gnus-summary-low-subject-map "s" 'gnus-summary-temporarily-lower-by-subject)
  (define-key gnus-summary-low-subject-map "S" 'gnus-summary-lower-by-subject)
  (define-key gnus-summary-low-subject-map "t" 'gnus-summary-temporarily-lower-by-subject)
  (define-key gnus-summary-low-subject-map "p" 'gnus-summary-lower-by-subject)

  (define-prefix-command 'gnus-summary-low-body-map)
  (define-key gnus-summary-lower-map "b" 'gnus-summary-low-body-map)
  (define-key gnus-summary-lower-map "B" 'gnus-summary-temporarily-lower-by-body)
  (define-key gnus-summary-low-body-map "b" 'gnus-summary-temporarily-lower-by-body)
  (define-key gnus-summary-low-body-map "B" 'gnus-summary-lower-by-body)
  (define-key gnus-summary-low-body-map "t" 'gnus-summary-temporarily-lower-by-body)
  (define-key gnus-summary-low-body-map "p" 'gnus-summary-lower-by-body)

  (define-prefix-command 'gnus-summary-low-author-map)
  (define-key gnus-summary-lower-map "a" 'gnus-summary-low-author-map)
  (define-key gnus-summary-lower-map "A" 'gnus-summary-temporarily-lower-by-author)
  (define-key gnus-summary-low-author-map "a" 'gnus-summary-temporarily-lower-by-author)
  (define-key gnus-summary-low-author-map "A" 'gnus-summary-lower-by-author)
  (define-key gnus-summary-low-author-map "t" 'gnus-summary-temporarily-lower-by-author)
  (define-key gnus-summary-low-author-map "p" 'gnus-summary-lower-by-author)

  (define-prefix-command 'gnus-summary-low-id-map)
  (define-key gnus-summary-lower-map "i" 'gnus-summary-low-id-map)
  (define-key gnus-summary-lower-map "I" 'gnus-summary-temporarily-lower-by-id)
  (define-key gnus-summary-low-id-map "i" 'gnus-summary-temporarily-lower-by-id)
  (define-key gnus-summary-low-id-map "I" 'gnus-summary-lower-by-id)
  (define-key gnus-summary-low-id-map "t" 'gnus-summary-temporarily-lower-by-id)
  (define-key gnus-summary-low-id-map "p" 'gnus-summary-lower-by-id)

  (define-prefix-command 'gnus-summary-low-thread-map)
  (define-key gnus-summary-lower-map "t" 'gnus-summary-low-thread-map)
  (define-key gnus-summary-lower-map "T" 'gnus-summary-temporarily-lower-by-thread)
  (define-key gnus-summary-low-thread-map "t" 'gnus-summary-temporarily-lower-by-thread)
  (define-key gnus-summary-low-thread-map "T" 'gnus-summary-lower-by-thread)
  (define-key gnus-summary-low-thread-map "t" 'gnus-summary-temporarily-lower-by-thread)
  (define-key gnus-summary-low-thread-map "p" 'gnus-summary-lower-by-thread)

  (define-prefix-command 'gnus-summary-low-xref-map)
  (define-key gnus-summary-lower-map "x" 'gnus-summary-low-xref-map)
  (define-key gnus-summary-lower-map "X" 'gnus-summary-temporarily-lower-by-xref)
  (define-key gnus-summary-low-xref-map "x" 'gnus-summary-temporarily-lower-by-xref)
  (define-key gnus-summary-low-xref-map "X" 'gnus-summary-lower-by-xref)
  (define-key gnus-summary-low-xref-map "t" 'gnus-summary-temporarily-lower-by-xref)
  (define-key gnus-summary-low-xref-map "p" 'gnus-summary-lower-by-xref)

  (define-prefix-command 'gnus-summary-low-fol-map)
  (define-key gnus-summary-lower-map "f" 'gnus-summary-low-fol-map)
  (define-key gnus-summary-lower-map "F" 'gnus-summary-lower-followups-to-author)
  (define-key gnus-summary-low-fol-map "f" 'gnus-summary-lower-followups-to-author)
  (define-key gnus-summary-low-fol-map "F" 'gnus-summary-lower-followups-to-author)
  (define-key gnus-summary-low-fol-map "t" 'gnus-summary-lower-followups-to-author)
  (define-key gnus-summary-low-fol-map "p" 'gnus-summary-lower-followups-to-author)


;; Summary score file commands

;; Much modification of the kill (ahem, score) code and lots of the
;; functions are written by Per Abrahamsen <amanda@iesd.auc.dk>.

(defun gnus-summary-header (header)
  ;; Return HEADER for current articles, or error.
  (let ((article (gnus-summary-article-number)))
    (if article
	(aref (gnus-get-header-by-number article)
	      (nth 1 (assoc header gnus-header-index)))
      (error "No article on current line"))))

(defun gnus-summary-score-entry 
  (header match type score date &optional prompt silent)
  "Enter score file entry.
HEADER is the header being scored.
MATCH is the string we are looking for.
TYPE is a flag indicating if it is a regexp or substring.
SCORE is the score to add.
DATE is the expire date."
  (interactive
   (list (completing-read "Header: "
			  gnus-header-index
			  (lambda (x) (fboundp (nth 2 x)))
			  t)
	 (read-string "Match: ")
	 (y-or-n-p "Use regexp match? ")
	 (prefix-numeric-value current-prefix-arg)
	 (if (y-or-n-p "Expire kill? ")
	     (current-time-string)
	   nil)))
  (let ((score (gnus-score-default score))
	(header (downcase header)))
    (and prompt (setq match (read-string 
			     (format "Match %s on %s, %s: " 
				     (if date "temp" "permanent") 
				     header
				     (if (< score 0) "lower" "raise"))
			     match)))
    (and (>= (nth 1 (assoc header gnus-header-index)) 0)
	 (not silent)
	 (gnus-summary-score-effect header match type score))
    (and (= score gnus-score-interactive-default-score)
	 (setq score nil))
    (let ((new (cond (type
		  (list match score (and date (gnus-day-number date)) type))
		 (date
		  (list match score (gnus-day-number date)))
		 (score
		  (list match score))
		 (t
		  (list match))))
	  (old (gnus-score-get header))
	  elem)
      ;; We see whether we can collapse some score entries.
      ;; This isn't quite correct, because there may be more elements
      ;; later on with the same key that have matching elems... Hm.
      (if (and old
	       (setq elem (assoc match old))
	       (eq (nth 3 elem) (nth 3 new))
	       (or (and (numberp (nth 2 elem)) (numberp (nth 2 new)))
		   (and (not (nth 2 elem)) (not (nth 2 new)))))
	  ;; Yup, we just add this new score to the old elem.
	  (setcar (cdr elem) (+ (nth 1 elem) (nth 1 new)))
	;; Nope, we have to add a new elem.
	(gnus-score-set header (if old (cons new old) (list new)))))
    (gnus-score-set 'touched '(t))))

(defun gnus-summary-score-effect (header match type score)
  "Simulate the effect of a score file entry.
HEADER is the header being scored.
MATCH is the string we are looking for.
TYPE is a flag indicating if it is a regexp or substring.
SCORE is the score to add."
  (interactive (list (completing-read "Header: "
				      gnus-header-index
				      (lambda (x) (fboundp (nth 2 x)))
				      t)
		     (read-string "Match: ")
		     (y-or-n-p "Use regexp match? ")
		     (prefix-numeric-value current-prefix-arg)))
  (save-excursion
    (or (and (stringp match) (> (length match) 0))
      (error "No match"))
    (goto-char (point-min))
    (let ((regexp (if type
		      match
		    (concat "\\`.*" (regexp-quote match) ".*\\'"))))
      (while (not (eobp))
	(let ((content (gnus-summary-header header))
	      (case-fold-search t))
	  (and content
	       (if (string-match regexp content)
		   (gnus-summary-raise-score score))))
	(beginning-of-line 2)))))

(defun gnus-summary-score-crossposting (score date)
   ;; Enter score file entry for current crossposting.
   ;; SCORE is the score to add.
   ;; DATE is the expire date.
   (let ((xref (gnus-summary-header "xref"))
	 (start 0)
	 group)
     (or xref (error "This article is not crossposted"))
     (while (string-match " \\([^ \t]+\\):" xref start)
       (setq start (match-end 0))
       (if (not (string= 
		 (setq group 
		       (substring xref (match-beginning 1) (match-end 1)))
		 gnus-newsgroup-name))
	   (gnus-summary-score-entry
	    "xref" (concat " " group ":") nil score date t)))))

(defun gnus-summary-temporarily-lower-by-subject (level)
  "Temporarily lower score by LEVEL for current subject.
See `gnus-score-expiry-days'."
  (interactive "P")
  (gnus-summary-score-entry
   "subject" (gnus-simplify-subject-re (gnus-summary-header "subject"))
   nil (- (gnus-score-default level))
   (current-time-string) t))

(defun gnus-summary-temporarily-lower-by-author (level)
  "Temporarily lower score by LEVEL for current author.
See `gnus-score-expiry-days'."
  (interactive "P")
  (gnus-summary-score-entry
   "from" (gnus-summary-header "from") nil (- (gnus-score-default level)) 
   (current-time-string) t))

(defun gnus-summary-temporarily-lower-by-body (level)
  "Temporarily lower score by LEVEL for a match on the body of the article.
See `gnus-score-expiry-days'."
  (interactive "P")
  (gnus-summary-score-entry
   "body" "" nil (- (gnus-score-default level)) (current-time-string) t))

(defun gnus-summary-temporarily-lower-by-id (level)
  "Temporarily lower score by LEVEL for current message-id.
See `gnus-score-expiry-days'."
  (interactive "P")
  (gnus-summary-score-entry
   "message-id" (gnus-summary-header "message-id") 
   nil (- (gnus-score-default level)) 
   (current-time-string)))

(defun gnus-summary-temporarily-lower-by-xref (level)
  "Temporarily lower score by LEVEL for current xref.
See `gnus-score-expiry-days'."
  (interactive "P")
  (gnus-summary-score-crossposting 
   (- (gnus-score-default level)) (current-time-string)))

(defun gnus-summary-temporarily-lower-by-thread (level)
  "Temporarily lower score by LEVEL for current thread.
See `gnus-score-expiry-days'."
  (interactive "P")
  (gnus-summary-score-entry
   "references" (gnus-summary-header "message-id")
   nil (- (gnus-score-default level)) (current-time-string)))

(defun gnus-summary-lower-by-subject (level)
  "Lower score by LEVEL for current subject."
  (interactive "P")
  (gnus-summary-score-entry
   "subject" (gnus-simplify-subject-re (gnus-summary-header "subject"))
   nil (- (gnus-score-default level)) 
   nil t))

(defun gnus-summary-lower-by-author (level)
  "Lower score by LEVEL for current author."
  (interactive "P")
  (gnus-summary-score-entry
   "from" (gnus-summary-header "from") nil 
   (- (gnus-score-default level)) nil t))

(defun gnus-summary-lower-by-body (level)
  "Lower score by LEVEL for a match on the body of the article."
  (interactive "P")
  (gnus-summary-score-entry
   "body" "" nil (- (gnus-score-default level)) nil t))

(defun gnus-summary-lower-by-id (level)
  "Lower score by LEVEL for current message-id."
  (interactive "P")
  (gnus-summary-score-entry
   "message-id" (gnus-summary-header "message-id") nil 
   (- (gnus-score-default level)) nil))

(defun gnus-summary-lower-by-xref (level)
  "Lower score by LEVEL for current xref."
  (interactive "P")
  (gnus-summary-score-crossposting (- (gnus-score-default level)) nil))

(defun gnus-summary-lower-followups-to-author (level)
  "Lower score by LEVEL for all followups to the current author."
  (interactive "P")
  (gnus-summary-score-entry "followup" (gnus-summary-header "from")
			    nil level (current-time-string) t t))

(defun gnus-summary-temporarily-raise-by-subject (level)
  "Temporarily raise score by LEVEL for current subject.
See `gnus-score-expiry-days'."
  (interactive "P")
  (gnus-summary-score-entry
   "subject" (gnus-simplify-subject-re (gnus-summary-header "subject"))
   nil level (current-time-string) t))

(defun gnus-summary-temporarily-raise-by-author (level)
  "Temporarily raise score by LEVEL for current author.
See `gnus-score-expiry-days'."
  (interactive "P")
  (gnus-summary-score-entry
   "from" (gnus-summary-header "from") nil level (current-time-string) t))

(defun gnus-summary-temporarily-raise-by-body (level)
  "Temporarily raise score by LEVEL for a match on the body of the article.
See `gnus-score-expiry-days'."
  (interactive "P")
  (gnus-summary-score-entry "body" "" nil level (current-time-string) t))

(defun gnus-summary-temporarily-raise-by-id (level)
  "Temporarily raise score by LEVEL for current message-id.
See `gnus-score-expiry-days'."
  (interactive "P")
  (gnus-summary-score-entry
   "message-id" (gnus-summary-header "message-id") 
   nil level (current-time-string)))

(defun gnus-summary-temporarily-raise-by-xref (level)
  "Temporarily raise score by LEVEL for current xref.
See `gnus-score-expiry-days'."
  (interactive "P")
  (gnus-summary-score-crossposting level (current-time-string)))

(defun gnus-summary-temporarily-raise-by-thread (level)
  "Temporarily raise score by LEVEL for current thread.
See `gnus-score-expiry-days'."
  (interactive "P")
  (gnus-summary-score-entry
   "references" (gnus-summary-header "message-id")
   nil level (current-time-string)))

(defun gnus-summary-raise-by-subject (level)
  "Raise score by LEVEL for current subject."
  (interactive "P")
  (gnus-summary-score-entry
   "subject" (gnus-simplify-subject-re (gnus-summary-header "subject"))
   nil level nil t))

(defun gnus-summary-raise-by-author (level)
  "Raise score by LEVEL for current author."
  (interactive "P")
  (gnus-summary-score-entry
   "from" (gnus-summary-header "from") nil level nil t))

(defun gnus-summary-raise-by-body (level)
  "Raise score by LEVEL for a match on the body of the article."
  (interactive "P")
  (gnus-summary-score-entry "body" "" nil level nil t))

(defun gnus-summary-raise-by-id (level)
  "Raise score by LEVEL for current message-id."
  (interactive "P")
  (gnus-summary-score-entry
   "message-id" (gnus-summary-header "message-id") nil level nil))

(defun gnus-summary-raise-by-xref (level)
  "Raise score by LEVEL for current xref."
  (interactive "P")
  (gnus-summary-score-crossposting level nil))

(defun gnus-summary-raise-followups-to-author (level)
  "Raise score by LEVEL for all followups to the current author."
  (interactive "P")
  (gnus-summary-score-entry "followup" (gnus-summary-header "from")
			    nil level (current-time-string) t t))



;;;
;;; Gnus Score Files
;;;

;; All score code written by Per Abrahamsen <abraham@iesd.auc.dk>.

;; Added by Per Abrahamsen <amanda@iesd.auc.dk>.
(defun gnus-score-set-mark-below (score)
  "Automatically mark articles with score below SCORE as read."
  (interactive 
   (list (or (and current-prefix-arg (prefix-numeric-value current-prefix-arg))
	     (string-to-int (read-string "Mark below: ")))))
  (setq score (or score gnus-summary-default-score 0))
  (gnus-score-set 'mark (list score))
  (gnus-score-set 'touched '(t))
  (setq gnus-summary-mark-below score)
  (gnus-summary-update-lines))

(defun gnus-score-set-expunge-below (score)
  "Automatically expunge articles with score below SCORE."
  (interactive 
   (list (or (and current-prefix-arg (prefix-numeric-value current-prefix-arg))
	     (string-to-int (read-string "Expunge below: ")))))
  (setq score (or score gnus-summary-default-score 0))
  (gnus-score-set 'expunge (list score))
  (gnus-score-set 'touched '(t)))

(defun gnus-score-set (symbol value &optional alist)
  ;; Set SYMBOL to VALUE in ALIST.
  (let* ((alist 
	  (or alist 
	      gnus-score-alist
	      (progn
		(gnus-score-load (gnus-score-file-name gnus-newsgroup-name))
		gnus-score-alist)))
	 (entry (assoc symbol alist)))
    (cond ((gnus-score-get 'read-only alist)
	   ;; This is a read-only score file, so we do nothing.
	   )
	  (entry
	   (setcdr entry value))
	  ((null alist)
	   (error "Empty alist"))
	  (t
	   (setcdr alist
		   (cons (cons symbol value) (cdr alist)))))))

(defun gnus-score-get (symbol &optional alist)
  ;; Get SYMBOL's definition in ALIST.
  (cdr (assoc symbol 
	      (or alist 
		  gnus-score-alist
		  (progn
		    (gnus-score-load 
		     (gnus-score-file-name gnus-newsgroup-name))
		    gnus-score-alist)))))

(defun gnus-score-change-score-file (file)
  "Change current score alist."
  (interactive
   (list (completing-read "Score file: " gnus-score-cache)))
  (setq gnus-current-score-file file)
  (gnus-score-load-file file)
  (gnus-set-mode-line 'summary))

(defun gnus-score-edit-alist (file)
  "Edit the current score alist."
  (interactive (list gnus-current-score-file))
  (and (buffer-name gnus-summary-buffer) (gnus-score-save))
  (setq gnus-winconf-edit-score (current-window-configuration))
  (gnus-configure-windows 'article)
  (pop-to-buffer (find-file-noselect file))
  (message (substitute-command-keys 
	    "\\<gnus-score-mode-map>\\[gnus-score-edit-done] to save edits"))
  (gnus-score-mode))
  
(defun gnus-score-edit-file (file)
  "Edit a score file."
  (interactive 
   (list (read-file-name "Edit score file: " gnus-kill-files-directory)))
  (and (buffer-name gnus-summary-buffer) (gnus-score-save))
  (setq gnus-winconf-edit-score (current-window-configuration))
  (gnus-configure-windows 'article)
  (pop-to-buffer (find-file-noselect file))
  (message (substitute-command-keys 
	    "\\<gnus-score-mode-map>\\[gnus-score-edit-done] to save edits"))
  (gnus-score-mode))
  
(defun gnus-score-load-file (file)
  ;; Load score file FILE.  Returns a list a retrieved score-alists.
  (setq gnus-kill-files-directory (or gnus-kill-files-directory "~/News/"))
  (let* ((file (expand-file-name 
		(or (and (string-match
			  (concat "^" (expand-file-name
				       gnus-kill-files-directory)) 
			  (expand-file-name file))
			 file)
		    (concat gnus-kill-files-directory file))))
	 (cached (assoc file gnus-score-cache))
	 (global (member file gnus-internal-global-score-files))
	 lists alist)
    (if cached
	;; The score file was already loaded.
	(setq alist (cdr cached))
      ;; We load the score file.
      (setq gnus-score-alist nil)
      (setq alist (gnus-score-load-score-alist file))
      ;; We add '(touched) to the alist to signify that it hasn't been
      ;; touched (yet). 
      (or (assq 'touched alist) (setq alist (cons (list 'touched nil) alist)))
      ;; If it is a global score file, we make it read-only.
      (and global
	   (not (assq 'read-only alist))
	   (setq alist (cons (list 'read-only t) alist)))
      ;; Update cache.
      (setq gnus-score-cache
	    (cons (cons file alist) gnus-score-cache)))
    ;; If there are actual scores in the alist, we add it to the
    ;; return value of this function.
    (if (memq t (mapcar (lambda (e) (stringp (car e))) alist))
	(setq lists (list alist)))
    ;; Treat the other possible atoms in the score alist.
    (let ((mark (car (gnus-score-get 'mark alist)))
	  (expunge (car (gnus-score-get 'expunge alist)))
	  (mark-and-expunge 
	   (car (gnus-score-get 'mark-and-expunge alist)))
	  (read-only (gnus-score-get 'read-only alist))
	  (files (gnus-score-get 'files alist))
	  (exclude-files (gnus-score-get 'exclude-files alist))
          (orphan (gnus-score-get 'orphan alist))
	  (adapt (gnus-score-get 'adapt alist))
	  (eval (gnus-score-get 'eval alist)))
      ;; We do not respect eval and files atoms from global score
      ;; files. 
      (and files (not global)
	   (setq lists (apply 'append lists
			      (mapcar (lambda (file)
					(gnus-score-load-file file)) 
				      files))))
      (and eval (not global) (eval eval))
      (setq gnus-scores-exclude-files exclude-files)
      (if orphan (setq gnus-orphan-score (car orphan)))
      (setq gnus-adaptive-score-alist
	    (cond ((eq adapt t)
		   gnus-default-adaptive-score-alist)
		  ((eq adapt 'ignore)
		   nil)
		  ((consp adapt)
		   adapt)))
      (setq gnus-summary-mark-below 
	    (or mark mark-and-expunge gnus-summary-mark-below))
      (setq gnus-summary-expunge-below 
	    (or expunge mark-and-expunge gnus-summary-expunge-below)))
    (setq gnus-current-score-file file)
    (setq gnus-score-alist alist)
    lists))

(defun gnus-score-load (file)
  ;; Load score FILE.
  (let ((cache (assoc file gnus-score-cache)))
    (if cache
	(setq gnus-score-alist (cdr cache))
      (setq gnus-score-alist nil)
      (gnus-score-load-score-alist file)
      (or gnus-score-alist
	  (setq gnus-score-alist (copy-alist '((touched nil)))))
      (setq gnus-score-cache
	    (cons (cons file gnus-score-alist) gnus-score-cache)))))

(defun gnus-score-remove-from-cache (file)
  (setq gnus-score-cache 
	(delq (assoc file gnus-score-cache) gnus-score-cache)))

(defun gnus-score-load-score-alist (file)
  (let (alist)
    (if (file-readable-p file)
	(progn
	  (save-excursion
	    (gnus-set-work-buffer)
	    (insert-file-contents file)
	    (goto-char (point-min))
	    ;; Only do the loading if the score file isn't empty.
	    (if (save-excursion (re-search-forward "[()0-9a-zA-Z]" nil t))
		(setq alist
		      (condition-case ()
			  (read (current-buffer))
			(error 
			 (progn
			   (message "Problem with score file %s" file)
			   (ding) 
			   (sit-for 2)
			   nil))))))
	  (if (eq (car alist) 'setq)
	      (setq gnus-score-alist (gnus-score-transform-old-to-new alist))
	    (setq gnus-score-alist alist))
	  (setq gnus-score-alist
		(gnus-score-check-syntax gnus-score-alist file)))
      (setq gnus-score-alist nil))))

(defun gnus-score-check-syntax (alist file)
  (cond 
   ((null alist)
    nil)
   ((not (consp alist))
    (message "Score file is not a list: %s" file)
    (ding)
    nil)
   (t
    (let ((a alist)
	  err)
      (while (and a (not err))
	(cond ((not (listp (car a)))
	       (message "Illegal score element %s in %s" (car a) file)
	       (setq err t))
	      ((and (stringp (car (car a)))
		    (not (listp (nth 1 (car a)))))
	       (message "Illegal header match %s in %s" (nth 1 (car a)) file)
	       (setq err t))
	      (t
	       (setq a (cdr a)))))
      (if err
	  (progn
	    (ding)
	    nil)
	alist)))))    

(defun gnus-score-transform-old-to-new (alist)
  (let* ((alist (nth 2 alist))
	 out entry)
    (if (eq (car alist) 'quote)
	(setq alist (nth 1 alist)))
    (while alist
      (setq entry (car alist))
      (if (stringp (car entry))
	  (let ((scor (cdr entry)))
	    (setq out (cons entry out))
	    (while scor
	      (setcar scor
		      (list (car (car scor)) (nth 2 (car scor))
			    (and (nth 3 (car scor))
				 (gnus-day-number (nth 3 (car scor))))
			    (if (nth 1 (car scor)) 'r 's)))
	      (setq scor (cdr scor))))
	(setq out (cons (if (not (listp (cdr entry))) 
			    (list (car entry) (cdr entry))
			  entry)
			out)))
      (setq alist (cdr alist)))
    (cons (list 'touched t) (nreverse out))))
  
(defun gnus-score-save ()
  ;; Save all SCORE information.
  (let ((cache gnus-score-cache))
    (save-excursion
      (setq gnus-score-alist nil)
      (set-buffer (get-buffer-create "*Score*"))
      (buffer-disable-undo (current-buffer))
      (let (entry score file)
	(while cache
	  (setq entry (car cache)
		cache (cdr cache)
		file (car entry)
		score (cdr entry))
	  (if (or (not (equal (gnus-score-get 'touched score) '(t)))
		  (gnus-score-get 'read-only score)
		  (not (file-writable-p file)))
	      ()
	    (setq score (delq (assq 'touched score) score))
	    (erase-buffer)
	    (let (emacs-lisp-mode-hook)
	      (pp score (current-buffer)))
	    (gnus-make-directory (file-name-directory file))
	    (write-region (point-min) (point-max) file nil 'silent))))
      (kill-buffer (current-buffer)))))
  
(defun gnus-score-headers (score-files)
  ;; Score `gnus-newsgroup-headers'.
  (let (scores)
    ;; PLM: probably this is not the best place to clear orphan-score
    (setq gnus-orphan-score nil)
    ;; Load the SCORE files.
    (while score-files
      (if (stringp (car score-files))
	  ;; It is a string, which means that it's a score file name,
	  ;; so we load the score file and add the score alist to
	  ;; the list of alists.
	  (setq scores (nconc (gnus-score-load-file (car score-files)) scores))
	;; It is an alist, so we just add it to the list directly.
	(setq scores (nconc (car score-files) scores)))
      (setq score-files (cdr score-files)))
    ;; Prune the score files that are to be excluded, if any.
    (if (not gnus-scores-exclude-files)
	()
      (let ((s scores)
	    c)
	(while s
	  (and (setq c (rassq (car s) gnus-score-cache))
	       (member (car c) gnus-scores-exclude-files)
	       (setq scores (delq (car s) scores)))
	  (setq s (cdr s)))))
    (if (not (and gnus-summary-default-score
		  scores
		  (> (length gnus-newsgroup-headers)
		     (length gnus-newsgroup-scored))))
	()
      (let* ((entries gnus-header-index)
	     (now (gnus-day-number (current-time-string)))
	     (expire (- now gnus-score-expiry-days))
	     (headers gnus-newsgroup-headers)
	     entry header)
	(message "Scoring...")
	;; Create articles, an alist of the form `(HEADER . SCORE)'.
	(while headers
	  (setq header (car headers)
		headers (cdr headers))
	  ;; WARNING: The assq makes the function O(N*S) while it could
	  ;; be written as O(N+S), where N is (length gnus-newsgroup-headers)
	  ;; and S is (length gnus-newsgroup-scored).
	  (or (assq (header-number header) gnus-newsgroup-scored)
	      (setq gnus-scores-articles       ;Total of 2 * N cons-cells used.
		    (cons (cons header (or gnus-summary-default-score 0))
			  gnus-scores-articles))))

	(save-excursion
	  (set-buffer (get-buffer-create "*Headers*"))
	  (buffer-disable-undo (current-buffer))
          ;; score orphans
          (if gnus-orphan-score 
              (progn
                (setq gnus-score-index 
                      (nth 1 (assoc "references" gnus-header-index)))
                (gnus-score-orphans gnus-orphan-score)))
	  ;; Run each header through the score process.
	  (while entries
	    (setq entry (car entries)
		  header (nth 0 entry)
		  entries (cdr entries))
	    (setq gnus-score-index (nth 1 (assoc header gnus-header-index)))
	    (if (< 0 (apply 'max (mapcar
				  (lambda (score)
				    (length (gnus-score-get header score)))
				  scores)))
		(funcall (nth 2 entry) scores header now expire)))
	  ;; Remove the buffer.
	  (kill-buffer (current-buffer)))

	;; Add articles to `gnus-newsgroup-scored'.
	(while gnus-scores-articles
	  (or (= gnus-summary-default-score (cdr (car gnus-scores-articles)))
	      (setq gnus-newsgroup-scored
		    (cons (cons (header-number 
				 (car (car gnus-scores-articles)))
				(cdr (car gnus-scores-articles)))
			  gnus-newsgroup-scored)))
	  (setq gnus-scores-articles (cdr gnus-scores-articles)))

	(message "Scoring...done")))))


(defun gnus-get-new-thread-ids (articles)
  (let ((index (nth 1 (assoc "message-id" gnus-header-index)))
        (refind gnus-score-index)
        id-list art this tref)
    (while articles
      (setq art (car articles)
            this (aref (car art) index)
            tref (aref (car art) refind)
            articles (cdr articles))
      (if (string-equal tref "")        ;no references line
          (setq id-list (cons this id-list))))
    id-list))

;; Orphan functions written by plm@atcmp.nl (Peter Mutsaers).
(defun gnus-score-orphans (score)
  (let ((new-thread-ids (gnus-get-new-thread-ids gnus-scores-articles))
        (index (nth 1 (assoc "references" gnus-header-index)))
        alike articles art arts this last this-id)
    
    (setq gnus-scores-articles (sort gnus-scores-articles 'gnus-score-string<)
	  articles gnus-scores-articles)

    ;;more or less the same as in gnus-score-string
    (erase-buffer)
    (while articles
      (setq art (car articles)
            this (aref (car art) gnus-score-index)
            articles (cdr articles))
      ;;completely skip if this is empty (not a child, so not an orphan)
      (if (not (string= this ""))
          (if (equal last this)
              ;; O(N*H) cons-cells used here, where H is the number of
              ;; headers.
              (setq alike (cons art alike))
            (if last
                (progn
                  ;; Insert the line, with a text property on the
                  ;; terminating newline refering to the articles with
                  ;; this line.
                  (insert last ?\n)
                  (put-text-property (1- (point)) (point) 'articles alike)))
            (setq alike (list art)
                  last this))))
    (and last                           ; Bwadr, duplicate code.
         (progn
           (insert last ?\n)                    
           (put-text-property (1- (point)) (point) 'articles alike)))

    ;; PLM: now delete those lines that contain an entry from new-thread-ids
    (while new-thread-ids
      (setq this-id (car new-thread-ids)
            new-thread-ids (cdr new-thread-ids))
      (goto-char (point-min))
      (while (search-forward this-id nil t)
        ;; found a match. remove this line
	(beginning-of-line)
	(kill-line 1)))

    ;; now for each line: update its articles with score by moving to
    ;; every end-of-line in the buffer and read the articles property
    (goto-char (point-min))
    (while (eq 0 (progn
                   (end-of-line)
                   (setq arts (get-text-property (point) 'articles))
                   (while arts
                     (setq art (car arts)
                           arts (cdr arts))
                     (setcdr art (+ score (cdr art))))
                   (forward-line))))))
             

(defun gnus-score-integer (scores header now expire)
  (let ((gnus-score-index (nth 1 (assoc header gnus-header-index)))
	alike last this art entries alist articles)

    ;; Find matches.
    (while scores
      (setq alist (car scores)
	    scores (cdr scores)
	    entries (assoc header alist))
      (while (cdr entries)		;First entry is the header index.
	(let* ((rest (cdr entries))		
	       (kill (car rest))
	       (match (nth 0 kill))
	       (type (or (nth 3 kill) '>))
	       (score (or (nth 1 kill) gnus-score-interactive-default-score))
	       (date (nth 2 kill))
	       (found nil)
	       (match-func (if (or (eq type '>) (eq type '<) (eq type '<=)
				   (eq type '>=) (eq type '=))
			       type
			     (error "Illegal match type: %s" type)))
	       (articles gnus-scores-articles)
	       arts art)
	  ;; Instead of doing all the clever stuff that
	  ;; `gnus-score-string' does to minimize searches and stuff,
	  ;; I will assume that people generally will put so few
	  ;; matches on numbers that any cleverness will take more
	  ;; time than one would gain.
	  (while articles
	    (and (funcall match-func match 
			  (or (aref (car (car articles)) gnus-score-index) 0))
		 (progn
		   (setq found t)
		   (setcdr (car articles) (+ score (cdr (car articles))))))
	    (setq articles (cdr articles)))
	  ;; Update expire date
	  (cond ((null date))		;Permanent entry.
		(found			;Match, update date.
		 (gnus-score-set 'touched '(t) alist)
		 (setcar (nthcdr 2 kill) now))
		((< date expire) ;Old entry, remove.
		 (gnus-score-set 'touched '(t) alist)
		 (setcdr entries (cdr rest))
		 (setq rest entries)))
	  (setq entries rest))))))

(defun gnus-score-date (scores header now expire)
  (let ((gnus-score-index (nth 1 (assoc header gnus-header-index)))
	alike last this art entries alist articles)

    ;; Find matches.
    (while scores
      (setq alist (car scores)
	    scores (cdr scores)
	    entries (assoc header alist))
      (while (cdr entries)		;First entry is the header index.
	(let* ((rest (cdr entries))		
	       (kill (car rest))
	       (match (timezone-make-date-sortable (nth 0 kill)))
	       (type (or (nth 3 kill) 'before))
	       (score (or (nth 1 kill) gnus-score-interactive-default-score))
	       (date (nth 2 kill))
	       (found nil)
	       (match-func 
		(cond ((eq type 'after) 'string<)
		      ((eq type 'before) 'gnus-string>)
		      ((eq type 'at) 'string=)
		      (t (error "Illegal match type: %s" type))))
	       (articles gnus-scores-articles)
	       arts art l)
	  ;; Instead of doing all the clever stuff that
	  ;; `gnus-score-string' does to minimize searches and stuff,
	  ;; I will assume that people generally will put so few
	  ;; matches on numbers that any cleverness will take more
	  ;; time than one would gain.
	  (while articles
	    (and
	     (setq l (aref (car (car articles)) gnus-score-index))
	     (funcall match-func match (timezone-make-date-sortable l))
	     (progn
	       (setq found t)
	       (setcdr (car articles) (+ score (cdr (car articles))))))
	    (setq articles (cdr articles)))
	  ;; Update expire date
	  (cond ((null date))		;Permanent entry.
		(found			;Match, update date.
		 (gnus-score-set 'touched '(t) alist)
		 (setcar (nthcdr 2 kill) now))
		((< date expire) ;Old entry, remove.
		 (gnus-score-set 'touched '(t) alist)
		 (setcdr entries (cdr rest))
		 (setq rest entries)))
	  (setq entries rest))))))

(defun gnus-score-body (scores header now expire)
  (save-excursion
    (set-buffer nntp-server-buffer)
    (save-restriction
      (let* ((buffer-read-only nil)
	     (articles gnus-scores-articles)
	     (all-scores scores)
	     (request-func (cond ((string= "head" (downcase header))
				  'gnus-request-head)
				 ((string= "body" (downcase header))
				  'gnus-request-body)
				 (t 'gnus-request-article)))
	     alike last this art entries alist ofunc article)
	;; Not all backends support partial fetching.  In that case,
	;; we just fetch the entire article.
	(or (gnus-check-backend-function request-func gnus-newsgroup-name)
	    (progn
	      (setq ofunc request-func)
	      (setq request-func 'gnus-request-article)))
	(while articles
	  (setq article (header-number (car (car articles))))
	  (message "Scoring on article %s..." article)
	  (if (not (funcall request-func article gnus-newsgroup-name))
	      ()
	    (widen)
	    (goto-char (point-min))
	    ;; If just parts of the article is to be searched, but the
	    ;; backend didn't support partial fetching, we just narrow
	    ;; to the relevant parts.
	    (if ofunc
		(if (eq ofunc 'gnus-request-head)
		    (narrow-to-region
		     (point)
		     (or (search-forward "\n\n" nil t) (point-max)))
		  (narrow-to-region
		   (or (search-forward "\n\n" nil t) (point))
		   (point-max))))
	    (setq scores all-scores)
	    ;; Find matches.
	    (while scores
	      (setq alist (car scores)
		    scores (cdr scores)
		    entries (assoc header alist))
	      (while (cdr entries)	;First entry is the header index.
		(let* ((rest (cdr entries))		
		       (kill (car rest))
		       (match (nth 0 kill))
		       (type (or (nth 3 kill) 's))
		       (score (or (nth 1 kill) 
				  gnus-score-interactive-default-score))
		       (date (nth 2 kill))
		       (found nil)
		       (case-fold-search 
			(not (or (eq type 'R) (eq type 'S)
				 (eq type 'Regexp) (eq type 'String))))
		       (search-func 
			(cond ((or (eq type 'r) (eq type 'R)
				   (eq type 'regexp) (eq type 'Regexp))
			       're-search-forward)
			      ((or (eq type 's) (eq type 'S)
				   (eq type 'string) (eq type 'String))
			       'search-forward)
			      (t
			       (error "Illegal match type: %s" type))))
		       arts art)
		  (goto-char (point-min))
		  (if (funcall search-func match nil t)
		      ;; Found a match, update scores.
		      (progn
			(setcdr (car articles) (+ score (cdr (car articles))))
			(setq found t)))
		  ;; Update expire date
		  (cond ((null date))	;Permanent entry.
			(found		;Match, update date.
			 (gnus-score-set 'touched '(t) alist)
			 (setcar (nthcdr 2 kill) now))
			((< date expire) ;Old entry, remove.
			 (gnus-score-set 'touched '(t) alist)
			 (setcdr entries (cdr rest))
			 (setq rest entries)))
		  (setq entries rest)))))
	  (setq articles (cdr articles)))))))



(defun gnus-score-followup (scores header now expire)
  ;; Insert the unique article headers in the buffer.
  (let ((gnus-score-index (nth 1 (assoc header gnus-header-index)))
	;; gnus-score-index is used as a free variable.
	alike last this art entries alist articles)

    (setq gnus-scores-articles (sort gnus-scores-articles 'gnus-score-string<)
	  articles gnus-scores-articles)

    (erase-buffer)
    (while articles
      (setq art (car articles)
	    this (aref (car art) gnus-score-index)
	    articles (cdr articles))
      (if (equal last this)
	  (setq alike (cons art alike))
	(if last
	    (progn
	      (insert last ?\n)
	      (put-text-property (1- (point)) (point) 'articles alike)))
	(setq alike (list art)
	      last this)))
    (and last				; Bwadr, duplicate code.
	 (progn
	   (insert last ?\n)			
	   (put-text-property (1- (point)) (point) 'articles alike)))
  
    ;; Find matches.
    (while scores
      (setq alist (car scores)
	    scores (cdr scores)
	    entries (assoc header alist))
      (while (cdr entries)		;First entry is the header index.
	(let* ((rest (cdr entries))		
	       (kill (car rest))
	       (match (nth 0 kill))
	       (type (or (nth 3 kill) 's))
	       (score (or (nth 1 kill) gnus-score-interactive-default-score))
	       (date (nth 2 kill))
	       (found nil)
	       (mt (aref (symbol-name type) 0))
	       (case-fold-search 
		(not (or (= mt ?R) (= mt ?S) (= mt ?E) (= mt ?F))))
	       (dmt (downcase mt))
	       (search-func 
		(cond ((= dmt ?r) 're-search-forward)
		      ((or (= dmt ?e) (= dmt ?s) (= dmt ?f)) 'search-forward)
		      (t (error "Illegal match type: %s" type))))
	       arts art)
	  (goto-char (point-min))
	  (if (= dmt ?e)
	      (while (funcall search-func match nil t)
		(and (= (progn (beginning-of-line) (point))
			(match-beginning 0))
		     (= (progn (end-of-line) (point))
			(match-end 0))
		     (progn
		       (setq found (setq arts (get-text-property 
					       (point) 'articles)))
		       ;; Found a match, update scores.
		       (while arts
			 (setq art (car arts)
			       arts (cdr arts))
			 (gnus-score-add-followups (car art))))))
	    (while (funcall search-func match nil t)
	      (end-of-line)
	      (setq found (setq arts (get-text-property (point) 'articles)))
	      ;; Found a match, update scores.
	      (while arts
		(setq art (car arts)
		      arts (cdr arts))
		(gnus-score-add-followups (car art)))))
	  ;; Update expire date
	  (cond ((null date))		;Permanent entry.
		(found			;Match, update date.
		 (gnus-score-set 'touched '(t) alist)
		 (setcar (nthcdr 2 kill) now))
		((< date expire) ;Old entry, remove.
		 (gnus-score-set 'touched '(t) alist)
		 (setcdr entries (cdr rest))
		 (setq rest entries)))
	  (setq entries rest))))))

(defun gnus-score-add-followups (article)
  (save-excursion
    (set-buffer gnus-summary-buffer)
    (gnus-summary-score-entry 
     "references" (gnus-get-header-by-number article) 'e nil
     (current-time-string) nil)))


(defun gnus-score-string (scores header now expire)
  ;; Score ARTICLES according to HEADER in SCORES.
  ;; Update matches entries to NOW and remove unmatched entried older
  ;; than EXPIRE.
  
  ;; Insert the unique article headers in the buffer.
  (let ((gnus-score-index (nth 1 (assoc header gnus-header-index)))
	;; gnus-score-index is used as a free variable.
	alike last this art entries alist articles)

    ;; Sorting the articles costs os O(N*log N) but will allow us to
    ;; only match with each unique header.  Thus the actual matching
    ;; will be O(M*U) where M is the number of strings to match with,
    ;; and U is the number of unique headers.  It is assumed (but
    ;; untested) this will be a net win because of the large constant
    ;; factor involved with string matching.
    (setq gnus-scores-articles (sort gnus-scores-articles 'gnus-score-string<)
	  articles gnus-scores-articles)

    (erase-buffer)
    (while articles
      (setq art (car articles)
	    this (aref (car art) gnus-score-index)
	    articles (cdr articles))
      (if (equal last this)
	  ;; O(N*H) cons-cells used here, where H is the number of
	  ;; headers.
	  (setq alike (cons art alike))
	(if last
	    (progn
	      ;; Insert the line, with a text property on the
	      ;; terminating newline refering to the articles with
	      ;; this line.
	      (insert last ?\n)
	      (put-text-property (1- (point)) (point) 'articles alike)))
	(setq alike (list art)
	      last this)))
    (and last				; Bwadr, duplicate code.
	 (progn
	   (insert last ?\n)			
	   (put-text-property (1- (point)) (point) 'articles alike)))
  
    ;; Find matches.
    (while scores
      (setq alist (car scores)
	    scores (cdr scores)
	    entries (assoc header alist))
      (while (cdr entries)		;First entry is the header index.
	(let* ((rest (cdr entries))		
	       (kill (car rest))
	       (match (nth 0 kill))
	       (type (or (nth 3 kill) 's))
	       (score (or (nth 1 kill) gnus-score-interactive-default-score))
	       (date (nth 2 kill))
	       (found nil)
	       (mt (aref (symbol-name type) 0))
	       (case-fold-search 
		(not (or (= mt ?R) (= mt ?S) (= mt ?E) (= mt ?F))))
	       (dmt (downcase mt))
	       (search-func 
		(cond ((= dmt ?r) 're-search-forward)
		      ((or (= dmt ?e) (= dmt ?s) (= dmt ?f)) 'search-forward)
		      (t (error "Illegal match type: %s" type))))
	       arts art)
	  (goto-char (point-min))
	  (if (= dmt ?e)
	      (while (and (not (eobp)) (funcall search-func match nil t))
		(and (= (progn (beginning-of-line) (point))
			(match-beginning 0))
		     (= (progn (end-of-line) (point))
			(match-end 0))
		     (progn
		       (setq found (setq arts (get-text-property 
					       (point) 'articles)))
		       ;; Found a match, update scores.
		       (while arts
			 (setq art (car arts)
			       arts (cdr arts))
			 (setcdr art (+ score (cdr art))))
		       (forward-line 1))))
	    (and (string= match "") (setq match "\n"))
	    (while (funcall search-func match nil t)
	      (end-of-line)
	      (setq found (setq arts (get-text-property (point) 'articles)))
	      ;; Found a match, update scores.
	      (while arts
		(setq art (car arts)
		      arts (cdr arts))
		(setcdr art (+ score (cdr art))))))
	  ;; Update expire date
	  (cond ((null date))		;Permanent entry.
		(found			;Match, update date.
		 (gnus-score-set 'touched '(t) alist)
		 (setcar (nthcdr 2 kill) now))
		((< date expire) ;Old entry, remove.
		 (gnus-score-set 'touched '(t) alist)
		 (setcdr entries (cdr rest))
		 (setq rest entries)))
	  (setq entries rest))))))

(defun gnus-score-string< (a1 a2)
  ;; Compare headers in articles A2 and A2.
  ;; The header index used is the free variable `gnus-score-index'.
  (string-lessp (aref (car a1) gnus-score-index)
		(aref (car a2) gnus-score-index)))

(defun gnus-score-build-cons (article)
  ;; Build a `gnus-newsgroup-scored' type cons from ARTICLE.
  (cons (header-number (car article)) (cdr article)))

(defconst gnus-header-index
  ;; Name to index alist.
  '(("number" 0 gnus-score-integer)
    ("subject" 1 gnus-score-string)
    ("from" 2 gnus-score-string)
    ("date" 3 gnus-score-date)
    ("message-id" 4 gnus-score-string) 
    ("references" 5 gnus-score-string) 
    ("chars" 6 gnus-score-integer) 
    ("lines" 7 gnus-score-integer) 
    ("xref" 8 gnus-score-string)
    ("head" -1 gnus-score-body)
    ("body" -1 gnus-score-body)
    ("all" -1 gnus-score-body)
    ("followup" 2 gnus-score-followup)))

(defun gnus-current-score-file-nondirectory (&optional score-file)
  (let ((score-file (or score-file gnus-current-score-file)))
    (if score-file 
	(gnus-short-group-name (file-name-nondirectory score-file))
      "none")))

(defun gnus-score-adaptive ()
  (save-excursion
    (let* ((malist (gnus-copy-sequence gnus-adaptive-score-alist))
	   (alist malist)
	   (date (current-time-string)) 
	   elem headers)
      ;; First we transform the adaptive rule alist into something
      ;; that's faster to process.
      (while malist
	(setq elem (car malist))
	(if (symbolp (car elem))
	    (setcar elem (symbol-value (car elem))))
	(setq elem (cdr elem))
	(while elem
	  (setcdr (car elem) 
		  (cons (symbol-name (car (car elem))) (cdr (car elem))))
	  (setcar (car elem) 
		  (intern 
		   (concat "gnus-header-" 
			   (downcase (symbol-name (car (car elem)))))))
	  (setq elem (cdr elem)))
	(setq malist (cdr malist)))
      ;; The we score away.
      (goto-char (point-min))
      (while (not (eobp))
	(setq elem (cdr (assq (gnus-summary-article-mark) alist)))
	(if (not elem)
	    ()
	  (setq headers (gnus-get-header-by-number 
			 (gnus-summary-article-number)))
	  (while elem
	    (gnus-summary-score-entry 
	     (nth 1 (car elem)) (funcall (car (car elem)) headers)
	     'e (nth 2 (car elem)) date nil t)
	    (setq elem (cdr elem))))
	(forward-line 1)))))

(defun gnus-score-remove-lines-adaptive (marks)
  (save-excursion
    (let* ((malist (gnus-copy-sequence gnus-adaptive-score-alist))
	   (alist malist)
	   (date (current-time-string)) 
	   elem headers)
      ;; First we transform the adaptive rule alist into something
      ;; that's faster to process.
      (while malist
	(setq elem (car malist))
	(if (symbolp (car elem))
	    (setcar elem (symbol-value (car elem))))
	(setq elem (cdr elem))
	(while elem
	  (setcdr (car elem) 
		  (cons (symbol-name (car (car elem))) (cdr (car elem))))
	  (setcar (car elem) 
		  (intern 
		   (concat "gnus-header-" 
			   (downcase (symbol-name (car (car elem)))))))
	  (setq elem (cdr elem)))
	(setq malist (cdr malist)))
      ;; The we score away.
      (goto-char (point-min))
      (while (re-search-forward marks nil t)
	(beginning-of-line)
	(setq elem (cdr (assq (gnus-summary-article-mark) alist)))
	(if (not elem)
	    ()
	  (setq headers (gnus-get-header-by-number 
			 (gnus-summary-article-number)))
	  (while elem
	    (gnus-summary-score-entry 
	     (nth 1 (car elem)) (funcall (car (car elem)) headers)
	     'e (nth 2 (car elem)) date nil t)
	    (setq elem (cdr elem))))
	(delete-region (point) (progn (forward-line 1) (point)))))))

;;;
;;; Score mode.
;;;

(defvar gnus-score-mode-map nil)
(defvar gnus-score-mode-hook nil)

(if gnus-score-mode-map
    ()
  (setq gnus-score-mode-map (copy-keymap emacs-lisp-mode-map))
  (define-key gnus-score-mode-map "\C-c\C-c" 'gnus-score-edit-done)
  (define-key gnus-score-mode-map "\C-c\C-d" 'gnus-score-edit-insert-date))

(defun gnus-score-mode ()
  "Mode for editing score files.
This mode is an extended emacs-lisp mode.

\\{gnus-score-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map gnus-score-mode-map)
  (set-syntax-table emacs-lisp-mode-syntax-table)
  (setq major-mode 'gnus-score-mode)
  (setq mode-name "Score")
  (lisp-mode-variables nil)
  (run-hooks 'emacs-lisp-mode-hook 'gnus-score-mode-hook))

(defun gnus-score-edit-insert-date ()
  "Insert date in numerical format."
  (interactive)
  (insert (int-to-string (gnus-day-number (current-time-string)))))

(defun gnus-score-edit-done ()
  "Save the score file and return to the summary buffer."
  (interactive)
  (let ((bufnam (buffer-file-name (current-buffer))))
    (save-buffer)
    (kill-buffer (current-buffer))
    (and gnus-winconf-edit-score
	 (set-window-configuration gnus-winconf-edit-score))
    (gnus-score-remove-from-cache bufnam)
    (gnus-score-load-file bufnam)))

(provide 'gnus-score)

;;; gnus-score.el ends here