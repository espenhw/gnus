;;; gnus-sound.el --- Sound effects for Gnus
;; Copyright (C) 1996 Free Software Foundation

;; Author: Steven L. Baur <steve@miranova.com>
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
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;; This file provides access to sound effects in Gnus.
;; Prerelease:  This file is partially stripped to support earcons.el
;; You can safely ignore most of it until Red Gnus. **Evil Laugh**
;;; Code:

(if (null (boundp 'running-xemacs))
    (defvar running-xemacs (string-match "XEmacs\\|Lucid" emacs-version)))

(require 'nnheader)
(eval-when-compile (require 'cl))

(defvar gnus-sound-inline-sound
  (and (fboundp 'device-sound-enabled-p)
       (device-sound-enabled-p))
  "When t, we will not spawn a subprocess to play sounds.")

(defvar gnus-sound-directory (nnheader-find-etc-directory "sounds")
  "The directory containing the Sound Files.")

(defvar gnus-sound-au-player "/usr/bin/showaudio"
  "Executable program for playing sun AU format sound files")
(defvar gnus-sound-wav-player "/usr/local/bin/play"
  "Executable program for playing WAV files")


;;; The following isn't implemented yet.  Wait for Red Gnus.
;(defvar gnus-sound-effects-enabled t
;  "When t, Gnus will use sound effects.")
;(defvar gnus-sound-enable-hooks nil
;  "Functions run when enabling sound effects.")
;(defvar gnus-sound-disable-hooks nil
;  "Functions run when disabling sound effects.")
;(defvar gnus-sound-theme-song nil
;  "Theme song for Gnus.")
;(defvar gnus-sound-enter-group nil
;  "Sound effect played when selecting a group.")
;(defvar gnus-sound-exit-group nil
;  "Sound effect played when exiting a group.")
;(defvar gnus-sound-score-group nil
;  "Sound effect played when scoring a group.")
;(defvar gnus-sound-busy-sound nil
;  "Sound effect played when going into a ... sequence.")


;;;###autoload
;(defun gnus-sound-enable-sound ()
;  "Enable Sound Effects for Gnus."
;  (interactive)
;  (setq gnus-sound-effects-enabled t)
;  (run-hooks gnus-sound-enable-hooks))

;;;###autoload
;(defun gnus-sound-disable-sound ()
;  "Disable Sound Effects for Gnus."
;  (interactive)
;  (setq gnus-sound-effects-enabled nil)
;  (run-hooks gnus-sound-disable-hooks))

;;;###autoload
(defun gnus-sound-play (file)
  "Play a sound through the speaker."
  (interactive)
  (let ((sound-file (if (file-exists-p file)
			file
		      (concat gnus-sound-directory file))))
    (when (file-exists-p sound-file)
      (if gnus-sound-inline-sound
	  (play-sound-file (concat gnus-sound-directory sound-file))
	(cond ((string-match "\\.wav$" sound-file)
	       (call-process gnus-sound-wav-player
			     (concat gnus-sound-directory sound-file)
			     0
			     nil))
	      ((string-match "\\.au$" sound-file)
	       (call-process gnus-sound-au-player
			     (concat gnus-sound-directory sound-file)
			     0
			     nil)))))))


;;; The following isn't implemented yet, wait for Red Gnus
;(defun gnus-sound-startrek-sounds ()
;  "Enable sounds from Star Trek the original series."
;  (interactive)
;  (setq gnus-sound-busy-sound "working.au")
;  (setq gnus-sound-enter-group "bulkhead_door.au")
;  (setq gnus-sound-exit-group "bulkhead_door.au")
;  (setq gnus-sound-score-group "ST_laser.au")
;  (setq gnus-sound-theme-song "startrek.au")
;  (add-hook 'gnus-select-group-hook 'gnus-sound-startrek-select-group)
;  (add-hook 'gnus-exit-group-hook 'gnus-sound-startrek-exit-group))
;;;***

(provide 'gnus-sound)

(run-hooks 'gnus-sound-load-hook)

;;; gnus-sound.el ends here
