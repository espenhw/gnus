;;; mm-uu.el -- Return uu stuffs as mm handles
;; Copyright (c) 1998 by Shenghuo Zhu <zsh@cs.rochester.edu>

;; Author: Shenghuo Zhu <zsh@cs.rochester.edu>
;; $Revision: 1.1 $
;; Keywords: news postscript uudecode binhex shar
  
;; This file is not part of GNU Emacs, but the same permissions
;; apply.
;;
;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;

;;; Code:

(eval-and-compile
  (autoload 'binhex-decode-region "binhex")
  (autoload 'binhex-decode-region-external "binhex")
  (autoload 'uudecode-decode-region "uudecode")
  (autoload 'uudecode-decode-region-external "uudecode"))

(defun mm-uu-copy-to-buffer (from to)
  "Copy the contents of the current buffer to a fresh buffer."
  (save-excursion
    (let ((obuf (current-buffer)))
      (set-buffer (generate-new-buffer " *mm-uu*"))
      (insert-buffer-substring obuf from to)
      (current-buffer))))

;;; postscript

(defconst mm-uu-postscript-begin-line "^%!PS-")
(defconst mm-uu-postscript-end-line "^%%EOF$")

(defconst mm-uu-uu-begin-line "^begin[ \t]+[0-7][0-7][0-7][ \t]+\\(.*\\)$")
(defconst mm-uu-uu-end-line "^end[ \t]*$")
(defvar mm-uu-decode-function 'uudecode-decode-region)

(defconst mm-uu-binhex-begin-line
  "^:...............................................................$")
(defconst mm-uu-binhex-end-line ":$")
(defvar mm-uu-binhex-decode-function 'binhex-decode-region)

(defconst mm-uu-shar-begin-line "^#! */bin/sh")
(defconst mm-uu-shar-end-line "^exit 0")

(defvar mm-uu-begin-line 
  (concat mm-uu-postscript-begin-line "\\|"
	  mm-uu-uu-begin-line "\\|"
	  mm-uu-binhex-begin-line "\\|"
	  mm-uu-shar-begin-line))

(defvar mm-uu-identifier-alist
  '((?% . postscript) (?b . uu) (?: . binhex) (?# . shar)))

;;;### autoload

(defun mm-uu-dissect ()
  "Dissect the current buffer and return a list of uu handles."
  (save-excursion
    (save-restriction
      (mail-narrow-to-head)
      (goto-char (point-max)))
    (let ((text-start (point)) start-char end-char 
	  type file-name end-line result)
      (while (re-search-forward mm-uu-begin-line nil t)
	(beginning-of-line)
	(setq start-char (point))
	(forward-line) ;; in case of failure
	(setq type (cdr (assq (aref (match-string 0) 0) 
			      mm-uu-identifier-alist)))
	(setq file-name 
	      (if (eq type 'uu)
		  (and (match-string 1)
		       (let ((nnheader-file-name-translation-alist
			      '((?/ . ?,) (? . ?_) (?* . ?_) (?$ . ?_))))
			 (nnheader-translate-file-chars (match-string 1))))))
	(setq end-line (symbol-value 
			(intern (concat "mm-uu-" (symbol-name type) 
				       "-end-line"))))
	(when (re-search-forward end-line nil t)
	  (setq end-char (point))
	  (when (or (not (eq type 'binhex))
		    (setq file-name 
			  (condition-case nil
			      (binhex-decode-region start-char end-char t)
			    (error nil))))
	    (if (> start-char text-start)
		(push
		 (list (mm-uu-copy-to-buffer text-start start-char) 
		       '("text/plain") nil nil nil nil) 
		 result))
	    (push 
	     (cond
	      ((eq type 'postscript)
	       (list (mm-uu-copy-to-buffer start-char end-char) 
		     '("application/postscript") nil nil nil nil))
	      ((eq type 'uu)
	       (list (mm-uu-copy-to-buffer start-char end-char) 
		     (list (or (and file-name
				    (string-match "\\.[^\\.]+$" file-name) 
				    (mailcap-extension-to-mime 
				     (match-string 0 file-name)))
			       "application/octet-stream"))
		     mm-uu-decode-function nil 
		     (if (and file-name (not (equal file-name "")))
			 (list "attachment" (cons 'filename file-name)))
		   file-name))
	      ((eq type 'binhex)
	       (list (mm-uu-copy-to-buffer start-char end-char) 
		     (list (or (and file-name
				    (string-match "\\.[^\\.]+$" file-name) 
				    (mailcap-extension-to-mime 
				     (match-string 0 file-name)))
			       "application/octet-stream"))
		     mm-uu-binhex-decode-function nil 
		     (if (and file-name (not (equal file-name "")))
			 (list "attachment" (cons 'filename file-name)))
		     file-name))
	      ((eq type 'shar)
	       (list (mm-uu-copy-to-buffer start-char end-char) 
		     '("application/x-shar") nil nil nil nil))) 
	     result)
	    (setq text-start end-char))))
      (if (and result
	       (> start-char text-start))
	  (push
	   (list (mm-uu-copy-to-buffer text-start (point-max)) 
		 '("text/plain") nil nil nil nil) 
	   result))
      (nreverse result))))

(provide 'mm-uu)

;;; mm-uu.el ends here
