;;; gnus-diary.el --- Wrapper around the NNDiary Gnus backend

;; Copyright (C) 1999 Didier Verna.

;; PRCS: $Id: gnus-diary.el 1.8 Tue, 04 Sep 2001 11:32:13 +0200 didier $

;; Author:        Didier Verna <didier@xemacs.org>
;; Maintainer:    Didier Verna <didier@xemacs.org>
;; Created:       Tue Jul 20 10:42:55 1999 under XEmacs 21.2 (beta 18)
;; Last Revision: Wed Aug  8 14:38:14 2001
;; Keywords:      calendar mail news

;; This file is part of NNDiary.

;; NNDiary is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; NNDiary is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.


;;; Commentary:

;; Contents management by FCM version 0.1.

;; Description:
;; ===========

;; Gnus-Diary is a wrapper around the NNDiary Gnus backend.  It is here to
;; make your nndiary-user life easier in differnet ways.  So, you don't have
;; to use it if you don't want to.  But, really, you should.

;; Gnus-Diary offers the following improvements on top of the NNDiary backend:

;; - A nice summary line format:
;;   Displaying diary messages in standard summary line format (usually
;;   something like "<From Joe>: <Subject>") is pretty useless.  Most of the
;;   time, you're the one who wrote the message, and you mostly want to see
;;   the event's date.  Gnus-Diary offers you a nice summary line format which
;;   will do this.  By default, a summary line will appear like this:
;;
;;     <Event Date>: <Subject> <Remaining time>
;;
;;   for example, here's how Joe's birthday is displayed in my
;;   "nndiary:birhdays" summary buffer (the message is expirable, but will
;;   never be deleted, as it specifies a regular event):
;;
;;   E  Sat, Sep 22 01, 12:00: Joe's birthday (in 6 months, 1 week)

;;  - More article sorting functions:
;;    Gnus-Diary adds a new sorting function called
;;    `gnus-summary-sort-by-schedule'.  This function lets you organize your
;;    diary summary buffers from the closest event to the farthest one.

;;  - Automatic generation of diary group parameters:
;;    When you create a new diary group, or visit one, Gnus-Diary checks your
;;    group parameters, and if needed, sets the summary line format to the
;;    diary-specific value, adds the diary-specific sorting functions, and
;;    also adds the different `X-Diary-*' headers to the group's
;;    posting-style.  It is then easier to send a diary message, because if
;;    you use `C-u a' or `C-u m' on a diary group to prepare a message, these
;;    headers will be inserted automatically (but not filled with proper
;;    values yet).


;; Usage:
;; =====

;; 0/ Don't use any `gnus-user-format-function-[d|D]'.  Gnus-Diary provides
;;    both of these (sorry if you used them before).
;; 1/ Add '(require 'gnus-diary) to your gnusrc file.
;; 2/ Customize your gnus-diary options to suit your needs.



;; Bugs / Todo:
;; ===========

;; * Provide `gnus-group-diary-mail' and `gnus-group-post-diary-news' (or
;;   something like that), that would do just like `gnus-group-mail' and
;;   `gnus-group-post-news', but also prompt for diary header values with
;;   completion etc.
;; * Maybe not actually: we could just have a function that converts *any*
;;   message to a diary one, by prompting the schedule. You could then forward
;;   a message and make it a diary one etc.

;;; Code:

(require 'nndiary)

(defgroup gnus-diary nil
  "Utilities on top of the nndiary backend for Gnus.")

(defcustom gnus-diary-summary-line-format "%U%R%z%I %uD: %(%s%) (%ud)\n"
  "*Summary line format for nndiary groups."
  :type 'string
  :group 'gnus-diary
  :group 'gnus-summary-format)

(defcustom gnus-diary-time-format "%a, %b %e %y, %H:%M"
  "*Time format to display appointements in nndiary summary buffers.
Please refer to `format-time-string' for information on possible values."
  :type 'string
  :group 'gnus-diary)

(defcustom gnus-diary-delay-format-function 'gnus-diary-delay-format-english
  "*Function called to format a diary delay string.
It is passed two arguments.  The first one is non nil if the delay is in
the past.  The second one is of the form ((NUM . UNIT) ...) where NUM is
an integer and UNIT is one of 'year 'month 'week 'day 'hour or 'minute.
It should return strings like \"In 2 months, 3 weeks\", \"3 hours,
1 minute ago\" and so on.

There are currently two built-in format functions:
`gnus-diary-delay-format-english' (the default)
`gnus-diary-delay-format-french'"
  :type '(choice (const  :tag "english" gnus-diary-delay-format-english)
		 (const  :tag "french"  gnus-diary-delay-format-french)
		 (symbol :tag "other"))
  :group 'gnus-diary)

(defconst gnus-diary-version nndiary-version
  "Current Diary backend version.")


;; Summary line format ======================================================

(defun gnus-diary-delay-format-french (past delay)
  (if (null delay)
      "maintenant!"
    ;; Keep only a precision of two degrees
    (and (> (length delay) 1) (setf (nthcdr 2 delay) nil))
    (concat (if past "il y a " "dans ")
	    (let ((str "")
		  del)
	      (while (setq del (pop delay))
		(setq str (concat str
				  (int-to-string (car del)) " "
				  (cond ((eq (cdr del) 'year)
					 "an")
					((eq (cdr del) 'month)
					 "mois")
					((eq (cdr del) 'week)
					 "semaine")
					((eq (cdr del) 'day)
					 "jour")
					((eq (cdr del) 'hour)
					 "heure")
					((eq (cdr del) 'minute)
					 "minute"))
				  (unless (or (eq (cdr del) 'month)
					      (= (car del) 1))
				    "s")
				  (if delay ", "))))
	      str))))


(defun gnus-diary-delay-format-english (past delay)
  (if (null delay)
      "now!"
    ;; Keep only a precision of two degrees
    (and (> (length delay) 1) (setf (nthcdr 2 delay) nil))
    (concat (unless past "in ")
	    (let ((str "")
		  del)
	      (while (setq del (pop delay))
		(setq str (concat str
				  (int-to-string (car del)) " "
				  (symbol-name (cdr del))
				  (and (> (car del) 1) "s")
				  (if delay ", "))))
	      str)
	    (and past " ago"))))


(defun gnus-diary-header-schedule (headers)
  ;; Same as `nndiary-schedule', but given a set of headers HEADERS
  (mapcar
   (lambda (elt)
     (let ((head (cdr (assoc (intern (format "X-Diary-%s" (car elt)))
			     headers))))
       (when head
	 (nndiary-parse-schedule-value head (cadr elt) (caddr elt)))))
   nndiary-headers))

;; #### NOTE: Gnus sometimes gives me a HEADER not corresponding to any
;; message, with all fields set to nil here. I don't know what it is for, and
;; I just ignore it.
(defun gnus-user-format-function-d (header)
  ;; Returns an aproximative delay string for the next occurence of this
  ;; message. The delay is given only in the first non zero unit.
  ;; Code partly stolen from article-make-date-line
  (let* ((extras (mail-header-extra header))
	 (sched (gnus-diary-header-schedule extras))
	 (occur (nndiary-next-occurence sched (current-time)))
	 (now (current-time))
	 (real-time (subtract-time occur now)))
    (if (null real-time)
	"?????"
      (let* ((sec (+ (* (float (car real-time)) 65536) (cadr real-time)))
	     (past (< sec 0))
	     delay)
	(and past (setq sec (- sec)))
	(unless (zerop sec)
	  ;; This is a bit convoluted, but basically we go through the time
	  ;; units for years, weeks, etc, and divide things to see whether
	  ;; that results in positive answers.
	  (let ((units `((year . ,(* 365.25 24 3600))
			 (month . ,(* 31 24 3600))
			 (week . ,(* 7 24 3600))
			 (day . ,(* 24 3600))
			 (hour . 3600)
			 (minute . 60)))
		unit num)
	    (while (setq unit (pop units))
	      (unless (zerop (setq num (ffloor (/ sec (cdr unit)))))
		(setq delay (append delay `((,(floor num) . ,(car unit))))))
	      (setq sec (- sec (* num (cdr unit)))))))
	(funcall gnus-diary-delay-format-function past delay)))
    ))

;; #### NOTE: Gnus sometimes gives me a HEADER not corresponding to any
;; message, with all fields set to nil here. I don't know what it is for, and
;; I just ignore it.
(defun gnus-user-format-function-D (header)
  ;; Returns a formatted time string for the next occurence of this message.
  (let* ((extras (mail-header-extra header))
	 (sched (gnus-diary-header-schedule extras))
	 (occur (nndiary-next-occurence sched (current-time))))
    (format-time-string gnus-diary-time-format occur)))


;; Article sorting functions ================================================

(defun gnus-article-sort-by-schedule (h1 h2)
  (let* ((now (current-time))
	 (e1 (mail-header-extra h1))
	 (e2 (mail-header-extra h2))
	 (s1 (gnus-diary-header-schedule e1))
	 (s2 (gnus-diary-header-schedule e2))
	 (o1 (nndiary-next-occurence s1 now))
	 (o2 (nndiary-next-occurence s2 now)))
    (if (and (= (car o1) (car o2)) (= (cadr o1) (cadr o2)))
        (< (mail-header-number h1) (mail-header-number h2))
      (time-less-p o1 o2))))


(defun gnus-thread-sort-by-schedule (h1 h2)
  (gnus-article-sort-by-schedule (gnus-thread-header h1)
				 (gnus-thread-header h2)))

(defun gnus-summary-sort-by-schedule (&optional reverse)
  "Sort nndiary summary buffers by schedule of appointements.
Optional prefix (or REVERSE argument) means sort in reverse order."
  (interactive "P")
  (gnus-summary-sort 'schedule reverse))

(add-hook 'gnus-summary-menu-hook
	  (lambda ()
	    (easy-menu-add-item gnus-summary-misc-menu
				'("Sort")
				["Sort by schedule"
				 gnus-summary-sort-by-schedule
				 (eq (car (gnus-find-method-for-group
					   gnus-newsgroup-name))
				     'nndiary)]
				"Sort by number")))

;; Group parameters autosetting =============================================

(defun gnus-diary-update-group-parameters (group)
  ;; Ensure that nndiary groups have convenient group parameters:
  ;; - a posting style containing X-Diary headers
  ;; - a nice summary line format
  ;; - NNDiary specific sorting by schedule functions
  ;; In general, try not to mess with what the user might have modified.
  (let ((posting-style (gnus-group-get-parameter group 'posting-style t)))
    ;; Posting style:
    (mapcar (lambda (elt)
	      (let ((header (format "X-Diary-%s" (car elt))))
		(unless (assoc header posting-style)
		  (setq posting-style (append posting-style
					      `((,header "*")))))
		))
	    nndiary-headers)
    (gnus-group-set-parameter group 'posting-style posting-style)
    ;; Summary line format:
    (unless (gnus-group-get-parameter group 'gnus-summary-line-format t)
      (gnus-group-set-parameter group 'gnus-summary-line-format
				`(,gnus-diary-summary-line-format)))
    ;; Sorting by schedule:
    (unless (gnus-group-get-parameter group 'gnus-article-sort-functions)
      (gnus-group-set-parameter group 'gnus-article-sort-functions
				'((append gnus-article-sort-functions
					  (list
					   'gnus-article-sort-by-schedule)))))
    (unless (gnus-group-get-parameter group 'gnus-thread-sort-functions)
      (gnus-group-set-parameter group 'gnus-thread-sort-functions
				'((append gnus-thread-sort-functions
					  (list
					   'gnus-thread-sort-by-schedule)))))
    ))

;; Called when a group is subscribed. This is needed because groups created
;; because of mail splitting are *not* created with the backend function.
;; Thus, `nndiary-request-create-group-hooks' is inoperative.
(defun gnus-diary-maybe-update-group-parameters (group)
  (when (eq (car (gnus-find-method-for-group group)) 'nndiary)
    (gnus-diary-update-group-parameters group)))

(add-hook 'nndiary-request-create-group-hooks
	  'gnus-diary-update-group-parameters)
;; Now that we have `gnus-subscribe-newsgroup-hooks', this is not needed
;; anymore. Maybe I should remove this completely.
(add-hook 'nndiary-request-update-info-hooks
	  'gnus-diary-update-group-parameters)
(add-hook 'gnus-subscribe-newsgroup-hooks
	  'gnus-diary-maybe-update-group-parameters)

(defun gnus-diary-version ()
  "Current Diary backend version."
  (interactive)
  (message "NNDiary version %s" nndiary-version))

(provide 'gnus-diary)

;;; gnus-diary.el ends here
