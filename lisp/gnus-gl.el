;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GroupLens software and documentation is copyright (c) 1995 by Paul
;; Resnick (Massachusetts Institute of Technology); Brad Miller, John
;; Riedl, Jon Herlocker, and Joseph Konstan (University of Minnesota),
;; and David Maltz (Carnegie-Mellon University).
;;
;; Permission to use, copy, modify, and distribute this documentation
;; for non-commercial and commercial purposes without fee is hereby
;; granted provided that this copyright notice and permission notice
;; appears in all copies and that the names of the individuals and
;; institutions holding this copyright are not used in advertising or
;; publicity pertaining to this software without specific, written
;; prior permission.  The copyright holders make no representations
;; about the suitability of this software and documentation for any
;; purpose.  It is provided ``as is'' without express or implied
;; warranty.
;;
;; The copyright holders request that they be notified of
;; modifications of this code.  Please send electronic mail to
;; grouplens@cs.umn.edu for more information or to announce derived
;; works.  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Author: Brad Miller
;;
;; $Id: gnus-gl.el,v 1.1 1996/02/19 23:48:29 steve Exp $
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; User Documentation:
;; To use GroupLens you must load this file.
;; You must also register a pseudonym with the Better Bit Bureau.
;; http://www.cs.umn.edu/Research/GroupLens
;;
;;    ---------------- For your .emacs or .gnus file ----------------
;;
;; In addition, there are a few gnus-*-hooks that need to be set:
;; (add-hook 'gnus-startup-hook 'bbb-login)
;; (add-hook 'gnus-exit-gnus-hook 'bbb-logout)
;; (add-hook 'gnus-select-article-hook 'grouplens-do-time)
;; (setq gnus-score-find-score-files-function 'bbb-build-mid-scores-alist)
;; If you want to combine grouplens predictions with 'regular' gnus scores
;; check out the variables grouplens-score-offset grouplens-score-scale-factor
;;
;; If you want to see grouplens scores using our format you might want to
;; add a %uG to the gnus-summary-line-format.  For example, I use:
;; (setq gnus-summary-line-format "%U%R%uG%I%(%[%4L: %-20,20uB%]%) %s\n")
;; The above format also assumes that you are using gnus-bbdb  You can
;; just as easily ad %uG to whatever format string you use.  Or add
;; a %i to just see a simple numeric version of the predictions that
;; uses less space on the summary line.  If you use %uG you have several
;; choices for how things look.  See the doc string for the
;; grouplens-prediction-display variable.
;; (setq grouplens-prediction-display 'prediction-bar)
;;
;; If you use %uI on your group-line-format you will get (GroupLens Enhanced)
;; after the names of newsgroups supported by GroupLens.
;; (setq gnus-group-line-format "%M%S%p%5y: %(%g%) %uI\n")
;;
;; (setq gnus-summary-default-score 0)
;; (define-key gnus-summary-goto-map "n" 'grouplens-next-unread-article)
;; (define-key gnus-summary-mode-map "n" 'grouplens-next-unread-article)
;; (define-key gnus-summary-score-map "r" 'bbb-summary-rate-article)
;; (define-key gnus-summary-score-map "k" 'grouplens-score-thread)
;; (define-key gnus-summary-mode-map "," 'grouplens-best-unread-article)
;;
;; (add-hook 'gnus-exit-group-hook 'bbb-put-ratings)
;;
;; In addition there are some GroupLens user variables to set
;; (setq grouplens-pseudonym "foobar")
;; If you are using a bbb other than twain.cs.umn.edu you will need to
;; set the grouplens-bbb-host variable, and possibly the
;; grouplens-bbb-port variable. 
;;
;;(setq grouplens-newsgroups '("comp.lang.c++" "rec.humor" "rec.food.recipes"))
;; This sets up the groups for which you will get predictions and ratings.
;;

;; How do I Rate an article??
;;   Before you type n to go to the next article, hit a number from 1-5
;;   Type V r in the summary buffer and you will be prompted.
;;
;; What if, Gasp, I find a bug???
;; Please type M-x gnus-gl-submit-bug-report.  This will set up a
;; mail buffer with the  state of variables and buffers that will help
;; me debug the problem.  A short description up front would help too!
;; 
;; How do I display the prediction for an aritcle:
;;  If you set the gnus-summary-line-format as shown above, the score
;;  (prediction) will be shown automatically.
;;
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Programmer  Notes 
;; 10/9/95
;; gnus-scores-articles contains the articles
;; When scoring is done, the call tree looks something like:
;; gnus-possibly-score-headers
;;  ==> gnus-score-headers
;;      ==> gnus-score-load-file
;;          ==> get-all-mids  (from the eval form)
;;
;; it would be nice to have one that gets called after all the other
;; headers have been scored.
;; we may want a variable gnus-grouplens-scale-factor
;; and gnus-grouplens-offset  this would probably be either -3 or 0
;; to make the scores centered around zero or not.
;; Notes 10/12/95
;; According to Lars, Norse god of gnus, the simple way to insert a
;; call to an external function is to have a function added to the
;; variable gnus-score-find-files-function  This new function
;; gnus-grouplens-score-alist will return a core alist that
;; has (("message-id" ("<message-id-xxxx>" score) ("<message-id-xxxy>" score))
;; This seems like it would be pretty inefficient, though workable.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  TODO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; 3. Add some more ways to rate messages
;; 4. Better error handling for token timeouts.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bugs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 

(require 'gnus-score)
(eval-when-compile (require 'cl))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; User variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar grouplens-pseudonym ""
  "User's pseudonym.  This pseudonym is obtained during the registration process")

(defvar grouplens-bbb-host "twain.cs.umn.edu"
  "Host where the bbbd is running" )

(defvar grouplens-bbb-port 9000 
  "Port where the bbbd is listening" )

(defvar grouplens-newsgroups '("comp.lang.c++" "rec.humor" "rec.food.recipes"))

(defvar grouplens-prediction-display 'prediction-spot
  "valid values are: 
      prediction-spot -- an * corresponding to the prediction between 1 and 5, 
      confidence-interval -- a numeric confidence interval
      prediction-bar --  |#####     | the longer the bar, the better the article,
      confidence-bar --  |  -----   } the prediction is in the middle of the bar,
      confidence-spot -- )  *       | the spot gets bigger with more confidence,
      prediction-num  --   plain-old numeric value,
      confidence-plus-minus  -- prediction +/i confidence")

(defvar grouplens-score-offset 0
  "Offset the prediction by this value.  
   Setting this variable to -2 would have the following effect
   on grouplens scores:
   1   -->   -2
   2   -->   -1
   3   -->    0
   4   -->    1
   5   -->    2
   
   the reason a user might want to do this is to combine grouplens 
   predictions with scores calculated by other score methods"
)

(defvar grouplens-score-scale-factor 1
   "This variable allow sthe user to magify the effect of 
    grouplens scores. The scale factor is applied after
    the offset.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Program global variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar grouplens-bbb-token "0"
  "Current session token number")

(defvar grouplens-bbb-process nil
  "Process Id of current bbbd network stream process")

(defvar grouplens-rating-alist nil
  "Current set of  message-id rating pairs")

(defvar grouplens-current-hashtable (make-hash-table :size 100))
;; this seems like a pretty ugly way to get around the problem, but If 
;; I don't do this, then the compiler complains when I call gethash
;;
(eval-when-compile (setq grouplens-current-hashtable (make-hash-table :size 100)))

(defvar grouplens-current-group nil)

(defvar bbb-mid-list nil)

(defvar bbb-alist nil)

(defvar bbb-timeout-secs 10
  "Number of seconds to wait for some response from the BBB before
    we give up and assume that something has died..." )

(defvar starting-time nil)

(defvar elapsed-time 0)

(defvar previous-article nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Utility Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun connect-to-bbbd (host port)
  (setq process-buffer
	(get-buffer-create (format "trace of BBBD session to %s" host)))
  ;; clear the trace buffer of old output
  (save-excursion
    (set-buffer process-buffer)
    (erase-buffer))
  ;; open the connection to the server
  (setq grouplens-bbb-process nil)
  (catch 'done
    (condition-case error
	(setq grouplens-bbb-process 
	      (open-network-stream "BBBD" process-buffer host port))
      (error (gnus-message 3 "Error: Failed to connect to BBB")
	     nil))
    (and (null grouplens-bbb-process) (throw 'done nil))
    (set-process-filter grouplens-bbb-process 'bbb-process-filter)
    (save-excursion
      (set-buffer process-buffer)
      (make-local-variable 'bbb-read-point)
      (setq bbb-read-point (point-min))
      (and (null (setq greeting (bbb-read-response grouplens-bbb-process t)))
	   (throw 'done nil))
      ))
  grouplens-bbb-process )

(defun bbb-process-filter (process output)
  (save-excursion
    (set-buffer (process-buffer process))
    (goto-char (point-max))
    (insert output)))

(defun bbb-send-command (process command)
  (goto-char (point-max))
  (insert command "\r\n")
  (setq bbb-read-point (point))
  (setq bbb-response-point (point))
  (process-send-string process command)
  (process-send-string process "\r\n"))

;; This function eats the initial response of OK or ERROR from 
;; the BBB.
(defun bbb-read-response (process &optional return-response-string)
  (let ((case-fold-search nil)
	 match-end)
    (goto-char bbb-read-point)
    (while (and (not (search-forward "\r\n" nil t))
		(accept-process-output process bbb-timeout-secs))
      (goto-char bbb-read-point))
    (setq match-end (point))
    (goto-char bbb-read-point)
    (if (not (looking-at "OK"))
	(progn (setq bbb-read-point match-end) nil)
      (setq bbb-read-point match-end)
      (if return-response-string
	  (buffer-substring (point) match-end)
	t ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;       Login Functionons
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bbb-login ()
  "return the token number if login is successful, otherwise return nil"
  (interactive)
  (if (not (equal grouplens-pseudonym ""))
      (let ((bbb-process (connect-to-bbbd grouplens-bbb-host grouplens-bbb-port)))
	(if bbb-process
	    (save-excursion (set-buffer (process-buffer bbb-process))
			    (bbb-send-command bbb-process 
					      (concat "login " grouplens-pseudonym))
			    (if (setq login-response 
				      (bbb-read-response bbb-process t))
				(setq grouplens-bbb-token (extract-token-number))
			      (gnus-message 3 "Error: Grouplens login failed")
			      (setq grouplens-bbb-token "0")
			      nil))) )
    (gnus-message 3 "Error: you must set a pseudonym"))
    nil)

(defun extract-token-number ()
  (let ((token-pos (search-forward "token=" nil t) ))
    (if (looking-at "[0-9]+")
	(buffer-substring token-pos (match-end 0)))))

(defun bbb-logout ()
  "logout of bbb session"
  (let ((bbb-process (connect-to-bbbd grouplens-bbb-host grouplens-bbb-port)))
    (if bbb-process
	(save-excursion (set-buffer (process-buffer bbb-process))
			(bbb-send-command bbb-process 
					  (concat "logout " grouplens-bbb-token))
			(if (bbb-read-response bbb-process t)
			    t
			  nil) ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;       Get Predictions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; this function can be called as part of the function
;; to return the list of score files to use.
;; See the gnus variable gnus-score-find-score-files-function
;; *Note:*  If you want to use grouplens scores along with
;; calculated scores, you should see the offset and scale variables.
;; At this point, I don't recommend using both scores and grouplens 
;; predictions together.
(defun bbb-build-mid-scores-alist (groupname)
  (if (member groupname grouplens-newsgroups)
      (let* ((mid-list (get-all-mids))
	     (predict-list (bbb-get-predictions mid-list groupname)))
					; alist should be a list of lists 
					; ((("message-id" ("<message-id>"
					; score nil s)))
	(setq grouplens-current-group groupname)
	(setq previous-article nil)
	(list (list (list (append (list "message-id") predict-list)))))
    (progn (setq grouplens-current-group groupname)
	   nil))
  )

;; Ask the bbb for predictions, and build up the score alist.
(defun bbb-get-predictions (midlist groupname)
  (if (equal grouplens-bbb-token "0")
      (gnus-message 3 "Error: You are not logged in to a BBB")
    (gnus-message 5 "Fetching Predictions...")
    (let* ((predict-command (build-predict-command 
			     midlist groupname grouplens-bbb-token ))
	   (predict-list nil)
	   (bbb-process (connect-to-bbbd grouplens-bbb-host grouplens-bbb-port)))
      (if bbb-process
	  (save-excursion (set-buffer (process-buffer bbb-process))
			  (bbb-send-command bbb-process predict-command)
			  (if (setq response (bbb-read-response bbb-process nil))
			      (setq predict-list 
				    (bbb-get-prediction-response bbb-process)))))
      (setq bbb-alist predict-list))
    ))



(defun get-all-mids ()
  (let ((index (nth 1 (assoc "message-id" gnus-header-index)))
	(articles gnus-newsgroup-headers))
    (setq bbb-mid-list nil)
    (while articles
      (progn (setq art (car articles)
		   this (aref art index)
		   articles (cdr articles))
	     (setq bbb-mid-list (cons this bbb-mid-list))))
    bbb-mid-list)
  )

(defun build-predict-command (mlist grpname token)
  (let ((cmd (concat "getpredictions " token " " grpname "\r\n")))
    (while mlist
      (setq art (car mlist)
	    cmd (concat cmd art "\r\n")
	    mlist (cdr mlist)))
    (setq cmd (concat cmd ".\r\n"))
  cmd)
  )

(defun bbb-get-prediction-response (process)
  (let ((case-fold-search nil)
	match-end)
    (goto-char bbb-read-point)
    (while (and (not (search-forward ".\r\n" nil t))
		(accept-process-output process bbb-timeout-secs))
      (goto-char bbb-read-point))
    (setq match-end (point))
    (goto-char (+ bbb-response-point 4))  ;; we ought to be right before OK
    (build-response-alist)))

;; build-response-alist assumes that the cursor has been positioned at
;; the first line of the list of mid/rating pairs.  For now we will
;; use a prediction of 99 to signify no prediction.  Ultimately, we
;; should just ignore messages with no predictions.
(defun build-response-alist ()
  (let ((resp nil)
	(match-end (point)))
    (setq grouplens-current-hashtable (make-hash-table :size 100))
    (while
	(cond ((looking-at "\\(<.*>\\) :nopred=")
	       (setq resp (cons  (list (get-mid) 
				       gnus-summary-default-score 
				       nil 's) resp))
	       (forward-line 1)
	       t)
	      ((looking-at "\\(<.*>\\) :pred=\\([0-9]\.[0-9][0-9]\\) :conflow=\\([0-9]\.[0-9][0-9]\\) :confhigh=\\([0-9]\.[0-9][0-9]\\)")
	       (setq resp (cons  (list (get-mid) (get-pred) nil 's) resp))
	       (cl-puthash (get-mid)
			   (list (get-pred) (get-confl) (get-confh))
			   grouplens-current-hashtable)
	       (forward-line 1)
	       t)
	      ((looking-at "\\(<.*>\\) :pred=\\([0-9]\.[0-9][0-9]\\)")
	       (setq resp (cons  (list (get-mid) (get-pred) nil 's) resp))
	       (forward-line 1)
	       t)
	      (t nil) ))
    resp)
)

;; these two functions assume that there is an active match lying
;; around.  Where the first parenthesized expression is the
;; message-id, and the second is the prediction.  Since gnus assumes
;; that scores are integer values?? we round the prediction.
(defun get-mid ()
  (buffer-substring (match-beginning 1) (match-end 1)))

(defun get-pred ()
  (let ((tpred (round (string-to-int (buffer-substring  
				      (match-beginning 2) 
				      (match-end 2))))))
    (if (> tpred 0)
	(* grouplens-score-scale-factor (+ grouplens-score-offset  tpred))
      1))
)

(defun get-confl ()
  (string-to-number (buffer-substring (match-beginning 3) (match-end 3))))

(defun get-confh ()
  (string-to-number (buffer-substring (match-beginning 4) (match-end 4))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;      Prediction Display
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconst rating-range 4.0)
(defconst grplens-maxrating 5)
(defconst grplens-minrating 1)
(defconst grplens-predstringsize 12)

(defun gnus-user-format-function-G (header)
  (let* ((rate-string (make-string 12 ? ))
	 (mid
	  (aref header
		(nth 1 (assoc "message-id" gnus-header-index))))
	 (hashent (gethash mid grouplens-current-hashtable))
	 (iscore (if (string-match "September" gnus-version)
		    gnus-tmp-score
		  score))
	 (low (car (cdr hashent)))
	 (high (car (cdr (cdr hashent)))))
    (aset rate-string 0 ?|) (aset rate-string 11 ?|)
    (if (not (member grouplens-current-group grouplens-newsgroups))
	(progn 
	  (if (not (equal grouplens-prediction-display 'prediction-num))
	      (cond ((< iscore 0)
		     (setq iscore 1))
		    ((> iscore 5)
		     (setq iscore 5))))
	  (setq low 0) (setq high 0)))
    (if (and (grouplens-valid-score iscore) 
	     (not (null mid)))
	(cond 
	 ;; prediction-spot
	 ((equal grouplens-prediction-display 'prediction-spot)
	  (setq rate-string (fmt-prediction-spot rate-string iscore)))
	 ;; confidence-interval
	 ((equal grouplens-prediction-display 'confidence-interval)
	  (setq rate-string (fmt-confidence-interval iscore low high)))
	 ;; prediction-bar
	 ((equal grouplens-prediction-display 'prediction-bar)
	  (setq rate-string (fmt-prediction-bar rate-string iscore)))
	 ;; confidence-bar
	 ((equal grouplens-prediction-display 'confidence-bar)
	  (setq rate-string (format "|   %4.2f   |" iscore)))
	 ;; confidence-spot
	 ((equal grouplens-prediction-display 'confidence-spot)
	  (setq rate-string (format "|   %4.2f   |" iscore)))
	 ;; prediction-num
	 ((equal grouplens-prediction-display 'prediction-num)
	  (setq rate-string (fmt-prediction-num iscore)))
	 ;; confidence-plus-minus
	 ((equal grouplens-prediction-display 'confidence-plus-minus)
	       (setq rate-string (fmt-confidence-plus-minus iscore low high))
	       )
	 (t (gnus-message 3 "Invalid prediction display type"))
	 )
      (aset rate-string 5 ?N) (aset rate-string 6 ?A))
    rate-string))

(defun grouplens-valid-score (score)
  (if (equal grouplens-prediction-display 'prediction-num)
      t
    (and (>= score grplens-minrating)
	 (<= score grplens-maxrating))))

(defun requires-confidence (format-type)
  (or (equal format-type 'confidence-plus-minus)
      (equal format-type 'confidence-spot)
      (equal format-type 'confidence-interval)))

(defun have-confidence (clow chigh)
  (not (or (null clow)
	   (null chigh))))


(defun fmt-prediction-spot (rate-string score)
  (aset rate-string
	(round (* (/ (- score grplens-minrating) rating-range)
		  (+ (- grplens-predstringsize 4) 1.49)))
	?*)
  rate-string)

(defun fmt-confidence-interval (score low high)
  (if (have-confidence low high)
      (format "|%4.2f-%4.2f |" low high)
    (fmt-prediction-num score)))

(defun fmt-confidence-plus-minus (score low high)
  (if (have-confidence low high)
      (format "|%3.1f+/-%4.2f|" score (/ (- high low) 2.0))
    (fmt-prediction-num score)))

(defun fmt-prediction-bar (rate-string score)
  (let* ((i 1) 
	 (step (/ rating-range (- grplens-predstringsize 4)))
	 (half-step (/ step 2))
	 (loc (- grplens-minrating half-step)))
    (while (< i (- grplens-predstringsize 2))
      (if (> score loc)
	  (aset rate-string i ?#)
	(aset rate-string i ? ))
      (setq i (+ i 1))
      (setq loc (+ loc step)))
    )
  rate-string)

(defun fmt-prediction-num (score)
  (format "|   %4.2f   |" score)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;       Put Ratings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The message-id for the current article can be found in
;; (aref gnus-current-headers (nth 1 (assoc "message-id" gnus-header-index)))
;;

;; 

(defun bbb-put-ratings ()
  (if (and grouplens-rating-alist 
	   (member gnus-newsgroup-name grouplens-newsgroups))
      (let ((bbb-process (connect-to-bbbd grouplens-bbb-host 
					  grouplens-bbb-port))
	    (rate-command (build-rate-command grouplens-rating-alist)))
	(if bbb-process
	    (save-excursion (set-buffer (process-buffer bbb-process))
			    (gnus-message 5 "Sending Ratings...")
			    (bbb-send-command bbb-process rate-command)
			    (if (bbb-read-response bbb-process t)
				(setq grouplens-rating-alist nil)
			      nil)
			    (gnus-message 5 "Sending Ratings...Done"))
	  (gnus-message 3 "No BBB connection")))
    (setq grouplens-rating-alist nil))
)

(defun build-rate-command (rate-alist)
  (let ((cmd (concat "putratings " grouplens-bbb-token 
		     " " grouplens-current-group " \r\n")))
    (while rate-alist
      (setq this (car rate-alist)
	    cmd (concat cmd (car this) " :rating=" (cadr this) ".00"
			" :time=" (cddr this) "\r\n")
	    rate-alist (cdr rate-alist)))
    (concat cmd ".\r\n"))
)

;; Interactive rating functions.
(defun  bbb-summary-rate-article (rating &optional midin)
  (interactive "nRating: ")
  (let ((mid (or midin (get-current-id)))
	oldrating)
  (if (and rating (or (>  rating 0) 
		      (<  rating 6))
	   mid
	   (member gnus-newsgroup-name grouplens-newsgroups))
      (progn
	(if (not (setq oldrating (assoc mid grouplens-rating-alist)))
	    (setq grouplens-rating-alist (cons (cons mid (cons rating 0))
					       grouplens-rating-alist))
	  (setcdr oldrating (cons rating 0)))
	(gnus-summary-mark-article nil (int-to-string rating)))	
    (gnus-message 3 "Invalid rating"))))

(defun grouplens-next-unread-article (rating)
  "Select unread article after current one."
  (interactive "P")
  (if rating
      (bbb-summary-rate-article rating))
  (gnus-summary-next-article t (and gnus-auto-select-same
				    (gnus-summary-subject-string))))

(defun grouplens-best-unread-article (rating)
  "Select unread article after current one."
  (interactive "P")
  (if rating
      (bbb-summary-rate-article rating))
  (gnus-summary-best-unread-article))

(defun grouplens-score-thread-sept (score)
  "Raise the score of the articles in the current thread with SCORE."
  (interactive "nRating:")
  (let (e)
    (save-excursion
      (let ((articles (gnus-summary-articles-in-thread)))
	(while articles
	  (gnus-summary-goto-subject (car articles))
	  (gnus-set-global-variables)
	  (bbb-summary-rate-article score
			    (mail-header-id 
			     (gnus-summary-article-header (car articles))))
	  (setq articles (cdr articles))))
      (setq e (point)))
    (let ((gnus-summary-check-current t))
      (or (zerop (gnus-summary-next-subject 1 t))
	  (goto-char e))))
  (gnus-summary-recenter)
  (gnus-summary-position-point)
  (gnus-set-mode-line 'summary))

(defun grouplens-score-thread-v5 (score)
  "Raise the score of the articles in the current thread with SCORE."
  (interactive "nRating: ")
  (gnus-set-global-variables)
  (let ((scoring t)
	(level (gnus-summary-thread-level)))
    (save-excursion
      (while scoring
	(bbb-summary-rate-article score
				  (gnus-header-id
				   (gnus-get-header-by-number 
				    (gnus-summary-article-number))))
	;; ...and go forward until either the buffer ends or the subtree
	;; ends. 
	(if (not (and (zerop (forward-line 1))
		      (> (gnus-summary-thread-level) level)))
	    (setq scoring nil))))
    ;; Go to next unread subject.
    (gnus-summary-next-subject 1 t))
  (gnus-set-mode-line 'summary))

(if (string-match "September" gnus-version)
    (defalias 'grouplens-score-thread 'grouplens-score-thread-sept)
  (defalias 'grouplens-score-thread 'grouplens-score-thread-v5))

(defun get-current-id ()
  (if gnus-current-headers
      (aref gnus-current-headers (nth 1 (assoc "message-id" gnus-header-index)))
    (gnus-message 3 "You must select an article before you rate it")))



(defun gnus-user-format-function-I (header)
  (let ((gname (if (string-match "September" gnus-version)
		   gnus-tmp-group
		 group)))
    (if (member gname grouplens-newsgroups)
	"  (GroupLens Enhanced)"
      ""))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          TIME SPENT READING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun grouplens-start-timer ()
  (setq starting-time (current-time))
)

(defun grouplens-elapsed-time ()
  (let ((et (time-float (current-time))))
    (- et (time-float starting-time)))
)

(defun time-float (timeval)
  (+ (* (car timeval) 65536) 
	(cadr timeval)))

(defun grouplens-do-time ()
  (if (member gnus-newsgroup-name grouplens-newsgroups)
      (progn
	(cond ((not (null previous-article))
	       (setq  elapsed-time (grouplens-elapsed-time))
	       (if (not (setq oldrating (assoc previous-article
					       grouplens-rating-alist)))
		   (setq grouplens-rating-alist (cons (cons previous-article
							   (cons 0
								 elapsed-time))
						      grouplens-rating-alist))
		 (setcdr oldrating (cons (cadr oldrating) elapsed-time)))
	       ))
	(grouplens-start-timer)
	(setq previous-article (get-current-id)))
    ) ; end if
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          BUG REPORTING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst gnus-gl-version "$Id: gnus-gl.el,v 1.1 1996/02/19 23:48:29 steve Exp $")
(defconst gnus-gl-maintainer-address "grouplens-bug@cs.umn.edu")
(defun gnus-gl-submit-bug-report ()
  "Submit via mail a bug report on gnus-gl"
  (interactive)
  (require 'reporter)
  (reporter-submit-bug-report
   gnus-gl-maintainer-address
   (concat "gnus-gl.el " gnus-gl-version)
   (list 'grouplens-pseudonym
         'grouplens-bbb-host
         'grouplens-bbb-port
         'grouplens-newsgroups
	 'grouplens-bbb-token
	 'grouplens-bbb-process
	 'grouplens-current-group
         'grouplens-previous-article
	 'grouplens-mid-list
	 'bbb-alist)
   nil
   'gnus-gl-get-trace))

(defun gnus-gl-get-trace ()
(insert-buffer (concat "trace of BBBD session to " grouplens-bbb-host)))

;; end

(defvar gnus-grouplens-mode nil
  "Minor mode for providing a GroupLens interface in Gnus summary buffers.")

(defvar gnus-grouplens-mode-map nil)

(unless gnus-grouplens-mode-map
  (gnus-define-keys
   gnus-grouplens-mode-map
   "n" grouplens-next-unread-article
   "n" grouplens-next-unread-article
   "r" bbb-summary-rate-article
   "k" grouplens-score-thread
   "," grouplens-best-unread-article))

(defun gnus-grouplens-make-menu-bar ()
  (unless (boundp 'gnus-grouplens-menu)
    (easy-menu-define
     gnus-grouplens-menu gnus-grouplens-mode-map ""
     '("GroupLens"
       ["Login" bbb-login t]
       ["Rate" bbb-summary-rate-article t]
       ["Next article" grouplens-next-unread-article t]
       ["Best article" grouplens-best-unread-article t]
       ["Raise thread" grouplens-score-thread-sept t]
       ["Report bugs" gnus-gl-submit-bug-report t]))))

(defun gnus-grouplens-mode (&optional arg)
  "Minor mode for providing a GroupLens interface in Gnus summary buffers."
  (interactive "P")
  (when (eq major-mode 'gnus-summary-mode)
    (make-local-variable 'gnus-grouplens-mode)
    (setq gnus-grouplens-mode 
	  (if (null arg) (not gnus-grouplens-mode)
	    (> (prefix-numeric-value arg) 0)))
    (when gnus-grouplens-mode
      (add-hook 'gnus-startup-hook 'bbb-login)
      (add-hook 'gnus-exit-gnus-hook 'bbb-logout)
      (make-local-hook 'gnus-select-article-hook)
      (add-hook 'gnus-select-article-hook 'grouplens-do-time)
      (make-local-hook 'bbb-put-ratings)
      (add-hook 'gnus-exit-group-hook 'bbb-put-ratings)
      (setq gnus-score-find-score-files-function 'bbb-build-mid-scores-alist)

      ;; Set up the menu.
      (when (and menu-bar-mode
		 (gnus-visual-p 'grouplens-menu 'menu))
	(gnus-grouplens-make-menu-bar))
      (unless (assq 'gnus-grouplens-mode minor-mode-alist)
	(push '(gnus-grouplens-mode " GroupLens") minor-mode-alist))
      (unless (assq 'gnus-grouplens-mode minor-mode-map-alist)
	(push (cons 'gnus-grouplens-mode gnus-grouplens-mode-map)
	      minor-mode-map-alist))
      (run-hooks 'gnus-grouplens-mode-hook))))

(provide 'gnus-gl)
