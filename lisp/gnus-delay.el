;;; gnus-delay.el --- Delayed posting of articles -*- coding: latin-1; -*-

;; Copyright (C) 2001  Free Software Foundation, Inc.

;; Author: Kai Groﬂjohann <Kai.Grossjohann@CS.Uni-Dortmund.DE>
;; Keywords: mail, news, extensions

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Provide delayed posting of articles.

;;; Code:

(require 'nndraft)
(require 'gnus-draft)

(defvar gnus-delay-group "delayed"
  "Group name for storing delayed articles.")

(defvar gnus-delay-header "X-Gnus-Delayed"
  "Header name for storing info about delayed articles.")

(defvar gnus-delay-default-delay "3d"
  "*Default length of delay.")

(defun gnus-delay-article (delay)
  "Delay this article by some time.
DELAY is a string, giving the length of the time.  Possible values are
like 3d (meaning 3 days) or 2w (meaning two weeks)."
  (interactive
   (list (read-string (format "Length of delay (default `%s'): "
                              gnus-delay-default-delay)
                      nil nil gnus-delay-default-delay nil)))
  (let (num unit days deadline)
    (unless (string-match "\\([0-9]+\\)\\s-*\\([dw]\\)" delay)
      (error "Malformed delay `%s'" delay))
    (setq num (match-string 1 delay))
    (setq unit (match-string 2 delay))
    (if (string= unit "w")
        (setq delay (* 7 (string-to-number num)))
      (setq delay (string-to-number num)))
    (setq deadline (message-make-date
                    (seconds-to-time (+ (time-to-seconds (current-time))
                                        (* delay 24 60 60)))))
    (message-add-header (format "%s: %s" gnus-delay-header deadline)))
  (set-buffer-modified-p t)
  (nndraft-request-create-group gnus-delay-group)
  (message-disassociate-draft)
  (nndraft-request-associate-buffer gnus-delay-group)
  (save-buffer 0)
  (kill-buffer (current-buffer))
  (message-do-actions message-postpone-actions))

(defun gnus-delay-send-drafts ()
  "Send all the delayed messages that are due now."
  (interactive)
  (save-excursion
    (let* ((group (format "nndraft:%s" gnus-delay-group))
           (articles (nndraft-articles))
           article deadline)
      (gnus-activate-group group)
      (while (setq article (pop articles))
        (gnus-request-head article group)
        (set-buffer nntp-server-buffer)
        (unless (re-search-forward
                 (concat "^" (regexp-quote gnus-delay-header) ":\\s-+"))
          (error "Couldn't find delay for article %d" article))
        (setq deadline (nnheader-header-value))
        (setq deadline (apply 'encode-time (parse-time-string deadline)))
        (setq deadline (time-since deadline))
        (when (and (>= (nth 0 deadline) 0)
                   (>= (nth 1 deadline) 0))
          (message "Sending article %d" article)
          (gnus-draft-send article group)
          (message "Sending article %d...done" article))))))

;;;###autoload
(defun gnus-delay-initialize (&optional no-keymap no-check)
  "Initialize the gnus-delay package.
This sets up a key binding in `message-mode' to delay a message.
This tells Gnus to look for delayed messages after getting new news.

Key binding is skipped if optional arg NO-KEYMAP is non-nil.
Checking delayed messages is skipped if optional arg NO-CHECK is non-nil."
  (unless no-keymap
    (require 'message)
    (define-key message-mode-map (kbd "C-c C-j") 'gnus-delay-article))
  (unless no-check
    (add-hook 'gnus-get-new-news-hook 'gnus-delay-send-drafts)))

(provide 'gnus-delay)
;;; gnus-delay.el ends here
