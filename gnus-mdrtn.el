;;; gnus-mdrtn.el --- a group moderation package for Gnus
;; Copyright (C) 1996 Lars Magne Ingebrigtsen

;; Author: Lars Magne Ingebrigtsen <larsi@ifi.uio.no>
;; Keywords: news, moderation

;; This file is not part of GNU Emacs.

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

;; This package is designed for enabling moderators to do various
;; spiffy things while moderating groups.  Some of these things are
;; rather evil if done by non-moderators -- canceling other people's
;; articles, including Approved headers and so on.  So while this file
;; is GPL'd and there therefore is no distribution restriction --
;; please do not put this file at any public sites.  All people who
;; want a copy can get one, but let's try to keep this potentially
;; dangerous package out of the hands of Evil People, ok?  Pretty
;; please?

;; The moderation package is implemented as a minor mode for
;; summary buffers.  Put
;;
;; (add-hook 'gnus-summary-mode-hook 'gnus-moderate)
;;
;; in your .gnus.el file.

;; If you are the moderation of rec.zoofle, this is how it's supposed
;; to work: 
;;
;; 1) You split your incoming mail by matching on
;; "Newsgroups:.*rec.zoofle", which will put all the to-be-posted
;; articles in some mail group -- "nnml:rec.zoofle", for instance.
;;
;; 2) You enter that group once in a while and post articles
;; using the `e' (edit-and-post) or `s' (just send unedited)
;; commands.
;;
;; 3) If, while reading the rec.zoofle group, you happen upon
;; some articles that weren't approved by you, you can cancel
;; them with the `c' command.
;;
;; To use moderation mode in these two groups, say
;;
;; (setq gnus-moderated-groups "nnml:rec.zoofle\\|rec.zoofle")

;;; Code:

(require 'gnus-load)

(defvar gnus-moderated-groups nil
  "Regexp that match groups you moderate.")

(defvar gnus-moderation-ignored-headers "^\\(Received\\|To\\|Cc\\|X-From-Line\\|Return-Path\\|Xref\\):"
  "Headers to be removed before posting an approved article.")

(defvar gnus-moderation-mode nil
  "Minor mode for providing a moderation interface in Gnus summary buffers.")

(defvar gnus-moderation-mode-hook nil
  "Hook run in summary moderation mode buffers.")

;;; Internal variables.

(defvar gnus-moderation-mode-map nil)

(unless gnus-moderation-mode-map
  (setq gnus-moderation-mode-map (make-sparse-keymap))
  (gnus-define-keys gnus-moderation-mode-map
    "c" gnus-moderation-cancel-article
    "s" gnus-moderation-send-article
    "e" gnus-moderation-edit-article))

(defun gnus-moderation-make-menu-bar ()
  (unless (boundp 'gnus-moderation-menu)
    (easy-menu-define
     gnus-moderation-menu gnus-moderation-mode-map ""
     '("Moderation"
       ("Moderation"
	"Cancel" gnus-moderation-cancel-article
	"Send" gnus-moderation-send-article
	"Edit" gnus-moderation-edit-article)))))

(defun gnus-moderation-mode (&optional arg)
  "Minor mode for providing a moderation interface in Gnus summary buffers.

\\{gnus-moderation-mode-map}"
  (interactive "P")
  (when (eq major-mode 'gnus-summary-mode)
    (set (make-local-variable 'gnus-moderation-mode)
	 (if (null arg) (not gnus-moderation-mode)
	   (> (prefix-numeric-value arg) 0)))
    (when gnus-moderation-mode
      ;; Set up the menu.
      (when (and menu-bar-mode
		 (gnus-visual-p 'moderation-menu 'menu))
	(gnus-moderation-make-menu-bar))
      (unless (assq 'gnus-moderation-mode minor-mode-alist)
	(push '(gnus-moderation-mode " Moderation") minor-mode-alist))
      (unless (assq 'gnus-moderation-mode minor-mode-map-alist)
	(push (cons 'gnus-moderation-mode gnus-moderation-mode-map)
	      minor-mode-map-alist))
      (run-hooks 'gnus-moderation-mode-hook))))

(defun gnus-moderate ()
  "Turn on moderation mode in some buffers."
  (when (and gnus-moderated-groups
	     (string-match gnus-moderated-groups gnus-newsgroup-name))
    (gnus-moderation-mode 1)))

;;; Commands

(defun gnus-moderation-cancel-article (n)
  "Cancel the current article, even if it isn't yours."
  (interactive "P")
  (gnus-set-global-variables)
  (let ((articles (gnus-summary-work-articles n))
	(message-post-method
	 `(lambda (arg)
	    (gnus-post-method nil ,gnus-newsgroup-name)))
	article)
    (while (setq article (pop articles))
      (when (gnus-summary-select-article t nil nil article)
	(when (gnus-eval-in-buffer-window gnus-original-article-buffer
		(let ((user-mail-address
		       (nth 1 (mail-extract-address-components
			       (mail-fetch-field "from"))))
		      (message-cancel-message
		       (format
			"Moderator %s canceling a message in a group I moderate.\n"
			(message-make-from))))
		  (message-cancel-news)))
	  (gnus-summary-mark-as-read article gnus-canceled-mark)
	  (gnus-cache-remove-article 1))
	(gnus-article-hide-headers-if-wanted))
      (gnus-summary-remove-process-mark article))))

(defun gnus-moderation-edit-article ()
  "Edit an article before sending it."
  (interactive)
  (save-excursion
    (set-buffer gnus-summary-buffer)
    (gnus-set-global-variables)
    ;; Select article if needed.
    (gnus-summary-show-article t)
    (gnus-article-edit-article
     `(lambda ()
	(gnus-moderation-send-buffer)))))

(defun gnus-moderation-send-article ()
  "Post the current article after inserting an Approved header."
  (interactive)
  (gnus-summary-select-article)
  (gnus-eval-in-buffer-window gnus-original-article-buffer
    (gnus-moderation-send-buffer)))

(defun gnus-moderation-send-buffer ()
  "Send the current buffer as a message after inserting an Approved header."
  (let ((buf (current-buffer)))
    (nnheader-temp-write nil
      (insert-buffer-substring buf)
      (message-narrow-to-head)
      (message-remove-header gnus-moderation-ignored-headers t)
      (goto-char (point-max))
      (widen)
      (insert "Approved: " (message-make-from) "\n")
      (let ((method (if (message-functionp message-post-method)
			(funcall message-post-method)
		      message-post-method)))
	(require (car method))
	(funcall (intern (format "%s-open-server" (car method)))
		 (cadr method) (cddr method))
	(unless (funcall (intern (format "%s-request-post" (car method))))
	  (error "Couldn't post: %s" (nnheader-get-report 'nntp)))))))

(provide 'gnus-mdrtn)

;;; gnus-mdrtn ends here
