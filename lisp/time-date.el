;;; time-date.el --- Date and time handling functions
;; Copyright (C) 1998 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;;	Masanobu Umeda <umerin@mse.kyutech.ac.jp>
;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(eval-and-compile
  (eval
   '(if (not (string-match "XEmacs" emacs-version))
	(require 'parse-time)

      (require 'timezone)
      (defun parse-time-string (date)
	"Convert DATE into time."
	(decode-time
	 (condition-case ()
	     (let* ((d1 (timezone-parse-date date))
		    (t1 (timezone-parse-time (aref d1 3))))
	       (apply 'encode-time
		      (mapcar (lambda (el)
				(and el (string-to-number el)))
			      (list
			       (aref t1 2) (aref t1 1) (aref t1 0)
			       (aref d1 2) (aref d1 1) (aref d1 0)
			       (number-to-string
				(* 60 (timezone-zone-to-minute (aref d1 4))))))))
	   ;; If we get an error, then we just return a 0 time.
	   (error (list 0 0))))))))

(defun date-to-time (date)
  "Convert DATE into time."
  (apply 'encode-time (parse-time-string date)))

(defun time-to-float (time)
  "Convert TIME to a floating point number."
  (+ (* (car time) 65536.0)
     (cadr time)))

(defun float-to-time (float)
  "Convert FLOAT (a floating point number) to an Emacs time structure."
  (list (floor float 65536)
	(floor (mod float 65536))))

(defun time-less-p (t1 t2)
  "Say whether time T1 is less than time T2."
  (or (< (car t1) (car t2))
      (and (= (car t1) (car t2))
	   (< (nth 1 t1) (nth 1 t2)))))

(defun days-to-time (days)
  "Convert DAYS into time."
  (let* ((seconds (* 1.0 days 60 60 24))
	 (rest (expt 2 16))
	 (ms (condition-case nil (floor (/ seconds rest))
	       (range-error (expt 2 16)))))
    (list ms (condition-case nil (round (- seconds (* ms rest)))
	       (range-error (expt 2 16))))))

(defun time-since (time)
  "Return the time since TIME, which is either an internal time or a date."
  (when (stringp time)
    ;; Convert date strings to internal time.
    (setq time (date-to-time time)))
  (let* ((current (current-time))
	 (rest (when (< (nth 1 current) (nth 1 time))
		 (expt 2 16))))
    (list (- (+ (car current) (if rest -1 0)) (car time))
	  (- (+ (or rest 0) (nth 1 current)) (nth 1 time)))))

(defun subtract-time (t1 t2)
  "Subtract two internal times."
  (let ((borrow (< (cadr t1) (cadr t2))))
    (list (- (car t1) (car t2) (if borrow 1 0))
	  (- (+ (if borrow 65536 0) (cadr t1)) (cadr t2)))))

(defun date-to-day (date)
  "Return the number of days between year 1 and DATE."
  (time-to-day (date-to-time date)))
  
(defun days-between (date1 date2)
  "Return the number of days between DATE1 and DATE2."
  (- (date-to-day date1) (date-to-day date2)))

(defun date-leap-year-p (year)
  "Return t if YEAR is a leap year."
  (or (and (zerop (% year 4))
	   (not (zerop (% year 100))))
      (zerop (% year 400))))

(defun time-to-day-in-year (time)
  "Return the day number within the year of the date month/day/year."
  (let* ((tim (decode-time time))
	 (month (nth 4 tim))
	 (day (nth 3 tim))
	 (year (nth 5 tim))
	 (day-of-year (+ day (* 31 (1- month)))))
    (when (> month 2)
      (setq day-of-year (- day-of-year (/ (+ 23 (* 4 month)) 10)))
      (when (date-leap-year-p year)
	(setq day-of-year (1+ day-of-year))))
    day-of-year))

(defun time-to-day (time)
  "The number of days between the Gregorian date 0001-12-31bce and TIME.
The Gregorian date Sunday, December 31, 1bce is imaginary."
  (let* ((tim (decode-time time))
	 (month (nth 4 tim))
	 (day (nth 3 tim))
	 (year (nth 5 tim)))
    (+ (time-to-day-in-year time)	; 	Days this year
       (* 365 (1- year))		;	+ Days in prior years
       (/ (1- year) 4)			;	+ Julian leap years
       (- (/ (1- year) 100))		;	- century years
       (/ (1- year) 400))))		;	+ Gregorian leap years

(provide 'time-date)

;;; time-date.el ends here
