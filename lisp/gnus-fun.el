;;; gnus-fun.el --- various frivoluos extension functions to Gnus
;; Copyright (C) 2002 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: news

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

(defcustom gnus-x-face-directory (expand-file-name "x-faces" gnus-directory)
  "*Directory where X-Face PBM files are stored."
  :group 'gnus-fun
  :type 'directory)

(defcustom gnus-convert-pbm-to-x-face-command "pbmtoxbm %s | compface"
  "Command for converting a PBM to an X-Face."
  :group 'gnus-fun
  :type 'string)

(defcustom gnus-convert-image-to-x-face-command "giftopnm %s | ppmnorm | pnmscale -width 48 -height 48 | ppmtopgm | pgmtopbm | pbmtoxbm | compface"
  "Command for converting a GIF to an X-Face."
  :group 'gnus-fun
  :type 'string)

(defun gnus-shell-command-to-string (command)
  "Like `shell-command-to-string' except not mingling ERROR."
  (with-output-to-string
    (call-process shell-file-name nil (list standard-output nil)
		  nil shell-command-switch command)))

(defun gnus-shell-command-on-region (start end command)
  "A simplified `shell-command-on-region'.
Output to the current buffer, replace text, and don't mingle error."
  (call-process-region start end shell-file-name t
		       (list (current-buffer) nil)
		       nil shell-command-switch command))

;;;###autoload
(defun gnus-random-x-face ()
  "Insert a random X-Face header from `gnus-x-face-directory'."
  (interactive)
  (when (file-exists-p gnus-x-face-directory)
    (let* ((files (directory-files gnus-x-face-directory t "\\.pbm$"))
	   (file (nth (random (length files)) files)))
      (when file
	(gnus-shell-command-to-string
	 (format gnus-convert-pbm-to-x-face-command
		 (shell-quote-argument file)))))))

;;;###autoload
(defun gnus-x-face-from-file (file)
  "Insert an X-Face header based on an image file."
  (interactive "fImage file name:" )
  (when (file-exists-p file)
    (gnus-shell-command-to-string
     (format gnus-convert-image-to-x-face-command
	     (shell-quote-argument file)))))

(defun gnus-convert-image-to-gray-x-face (file depth)
  (let* ((mapfile (mm-make-temp-file (expand-file-name "gnus." 
						       mm-tmp-directory)))
	 (levels (expt 2 depth))
	 (step (/ 255 (1- levels)))
	 color-alist bits bits-list mask pixel x-faces)
    (with-temp-file mapfile
      (insert "P3\n")
      (insert (format "%d 1\n" levels))
      (insert "255\n")
      (dotimes (i levels)
	(insert (format "%d %d %d\n"
			(* step i) (* step i) (* step i)))
	(push (cons (* step i) i) color-alist)))
    (when (file-exists-p file)
      (with-temp-buffer
	(insert (gnus-shell-command-to-string
		 (format "giftopnm %s | ppmnorm | pnmscale -width 48 -height 48 | ppmquant -fs -map %s | ppmtopgm | pnmnoraw"
			 (shell-quote-argument file)
			 mapfile)))
	(goto-char (point-min))
	(forward-line 3)
	(while (setq pixel (ignore-errors (read (current-buffer))))
	  (push (cdr (assq pixel color-alist)) bits-list))
	(setq bits-list (nreverse bits-list))
	(dotimes (bit-number depth)
	  (setq mask (expt 2 bit-number))
	  (with-temp-buffer
	    (insert "P1\n48 48\n")
	    (dolist (bits bits-list)
	      (insert (if (zerop (logand bits mask)) "0 " "1 ")))
	    (gnus-shell-command-on-region
	     (point-min) (point-max)
	     ;; the following is taken from xbmtoikon:
	     "pbmtoicon | sed '/^[ 	]*[*\\\\/]/d; s/[ 	]//g; s/,$//' | tr , '\\012' | sed 's/^0x//; s/^/0x/' | pr -l1 -t -w22 -3 -s, | sed 's/,*$/,/' | compface")
	    (push (buffer-string) x-faces))))
      (dotimes (i (length x-faces))
	(insert (if (zerop i) "X-Face:" (format "X-Face-%s:" i))
		(nth i x-faces))))
    (delete-file mapfile)))

;;;###autoload
(defun gnus-convert-gray-x-face-to-xpm (faces)
  (let* ((depth (length faces))
	 (scale (/ 255 (1- (expt 2 depth))))
	 (ok-p t)
	 (coding-system-for-read 'binary)
	 (coding-system-for-write 'binary)
	 default-enable-multibyte-characters
	 start bit-array bit-arrays pixel)
    (with-temp-buffer
      (dolist (face faces)
	(erase-buffer)
	(insert (uncompface face))
	(gnus-shell-command-on-region
	 (point-min) (point-max)
	 "pnmnoraw")
	(goto-char (point-min))
	(forward-line 2)
	(setq start (point))
	(insert "[")
	(while (not (eobp))
	  (forward-char 1)
	  (insert " "))
	(insert "]")
	(goto-char start)
	(setq bit-array (read (current-buffer)))
	(unless (= (length bit-array) (* 48 48))
	  (setq ok-p nil))
	(push bit-array bit-arrays))
      (when ok-p
	(erase-buffer)
	(insert "P2\n48 48\n255\n")
	(dotimes (i (* 48 48))
	  (setq pixel 0)
	  (dotimes (plane depth)
	    (setq pixel (+ (* pixel 2) (aref (nth plane bit-arrays) i))))
	  (insert (number-to-string (* scale pixel)) " "))
	(gnus-shell-command-on-region
	 (point-min) (point-max)
	 "ppmtoxpm")
	(buffer-string)))))

;;;###autoload
(defun gnus-convert-gray-x-face-region (beg end)
  "Convert the X-Faces in region to a PPM file."
  (interactive "r")
  (let ((input (buffer-substring beg end))
	faces)
    (with-temp-buffer
      (insert input)
      (goto-char (point-min))
      (while (not (eobp))
	(save-restriction
	  (mail-header-narrow-to-field)
	  (push (mail-header-field-value) faces)
	  (goto-char (point-max)))))
    (gnus-convert-gray-x-face-to-xpm faces)))

(defface gnus-x-face '((t (:foreground "black" :background "white")))
  "Face to show X-Face.
The colors from this face are used as the foreground and background
colors of the displayed X-Faces."
  :group 'gnus-article-headers)

(defun gnus-display-x-face-in-from (data)
  "Display the X-Face DATA in the From header."
  (let ((default-enable-multibyte-characters nil)
	pbm)
    (when (or (gnus-image-type-available-p 'xface)
	      (and (gnus-image-type-available-p 'pbm)
		   (setq pbm (uncompface data))))
      (save-excursion
	(save-restriction
	  (article-narrow-to-head)
	  (gnus-article-goto-header "from")
	  (when (bobp)
	    (insert "From: [no `from' set]\n")
	    (forward-char -17))
	  (gnus-add-image
	   'xface
	   (gnus-put-image
	    (if (gnus-image-type-available-p 'xface)
		(gnus-create-image
		 (concat "X-Face: " data)
		 'xface t :ascent 'center :face 'gnus-x-face)
	      (gnus-create-image
	       pbm 'pbm t :ascent 'center :face 'gnus-x-face))))
	  (gnus-add-wash-type 'xface))))))

(defun gnus-grab-cam-x-face ()
  "Grab a picture off the camera and make it into an X-Face."
  (interactive)
  (shell-command "xawtv-remote snap ppm")
  (let ((file nil))
    (while (null (setq file (directory-files "/tftpboot/sparky/tmp"
					     t "snap.*ppm")))
      (sleep-for 1))
    (setq file (car file))
    (with-temp-buffer
      (shell-command
       (format "pnmcut -left 110 -top 30 -width 144 -height 144 '%s' | ppmnorm 2>/dev/null | pnmscale -width 48 | ppmtopgm | pgmtopbm -threshold -value 0.92 | pbmtoxbm | compface"
	       file)
       (current-buffer))
      ;;(sleep-for 3)
      (delete-file file)
      (buffer-string))))

(defun gnus-grab-gray-x-face ()
  "Grab a picture off the camera and make it into an X-Face."
  (interactive)
  (shell-command "xawtv-remote snap ppm")
  (let ((file nil))
    (while (null (setq file (directory-files "/tftpboot/sparky/tmp"
					     t "snap.*ppm")))
      (sleep-for 1))
    (setq file (car file))
    (with-temp-buffer
      (shell-command
       (format "pnmcut -left 70 -top 100 -width 144 -height 144 '%s' | ppmquant 256 2>/dev/null | ppmtogif > '%s.gif'"
	       file file)
       (current-buffer))
      (delete-file file))
    (gnus-convert-image-to-gray-x-face (concat file ".gif") 3)
    (delete-file (concat file ".gif"))))

(provide 'gnus-fun)

;;; gnus-fun.el ends here
