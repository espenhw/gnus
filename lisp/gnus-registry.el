;;; gnus-registry.el --- article registry for Gnus
;; Copyright (C) 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003
;;        Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
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

(eval-when-compile (require 'cl))

(require 'gnus)
(require 'gnus-int)
(require 'gnus-sum)
(require 'nnmail)

;; (defcustom gnus-summary-article-spool-hook nil
;;   "*A hook called after an article is spooled."
;;   :group 'gnus-summary
;;   :type 'hook)

(defun regtest (action id from &optional to method)
  (message "Registry: article %s %s from %s to %s"
	   id
	   (if method "respooling" "going")
	   (gnus-group-guess-full-name from)
	   (if to (gnus-group-guess-full-name to) "the Bit Bucket in the sky")))

(defun regtest-nnmail (id group)
  (message "Registry: article %s spooled to %s"
	   id
	   (gnus-group-prefixed-name group gnus-internal-registry-spool-current-method t)))

;;(add-hook 'gnus-summary-article-move-hook 'regtest) ; also does copy, respool, and crosspost
;;(add-hook 'gnus-summary-article-delete-hook 'regtest)
;;(add-hook 'gnus-summary-article-expire-hook 'regtest)
(add-hook 'nnmail-spool-hook 'regtest-nnmail)

;; TODO:

(provide 'gnus-registry)

;;; gnus-registry.el ends here
