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

(defcustom gnus-convert-pbm-to-x-face-command "pbmtoxbm '%s' | compface"
  "Command for converting a PBM to an X-Face."
  :group 'gnus-fun
  :type 'string)

(defcustom gnus-convert-image-to-x-face-command "giftopnm '%s' | ppmnorm 2>/dev/null | pnmscale -width 48 -height 48 | ppmtopgm | pgmtopbm | pbmtoxbm | compface"
  "Command for converting a PBM to an X-Face."
  :group 'gnus-fun
  :type 'string)

;;;###autoload
(defun gnus-random-x-face ()
  "Insert a random X-Face header from `gnus-x-face-directory'."
  (interactive)
  (when (file-exists-p gnus-x-face-directory)
    (let* ((files (directory-files gnus-x-face-directory t "\\.pbm$"))
	   (file (nth (random (length files)) files)))
      (when file
	(shell-command-to-string
	 (format gnus-convert-pbm-to-x-face-command file))))))

;;;###autoload
(defun gnus-x-face-from-file (file)
  "Insert an X-Face header based on an image file."
  (interactive "fImage file name:" )
  (when (file-exists-p file)
    (shell-command-to-string
     (format gnus-convert-image-to-x-face-command file))))
    

(provide 'gnus-fun)

;;; gnus-fun.el ends here
