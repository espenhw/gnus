;;; spam.el --- Identifying spam
;; Copyright (C) 2002 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: network

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

(require 'dns)
(require 'message)

;;; Blackholes

(defvar spam-blackhole-servers
  '("bl.spamcop.net" "relays.ordb.org" "dev.null.dk"
    "relays.visi.com" "rbl.maps.vix.com")
  "List of blackhole servers.")

(defun spam-check-blackholes ()
  "Check the Recevieved headers for blackholed relays."
  (let ((headers (message-fetch-field "received"))
	ips matches)
    (with-temp-buffer
      (insert headers)
      (goto-char (point-min))
      (while (re-search-forward
	      "\\[\\([0-9]+.[0-9]+.[0-9]+.[0-9]+\\)\\]" nil t)
	(push (mapconcat 'identity
			 (nreverse (split-string (match-string 1) "\\."))
			 ".")
	      ips)))
    (dolist (server spam-blackhole-servers)
      (dolist (ip ips)
	(when (query-dns (concat ip "." server))
	  (push (list ip server (query-dns (concat ip "." server) 'TXT))
		matches))))
    matches))

;;; Black- and white-lists

(defvar spam-directory "~/News/spam/"
  "When spam files are kept.")

(defvar spam-whitelist (expand-file-name "whitelist" spam-directory)
  "The location of the whitelist.
The file format is one regular expression per line.
The regular expression is matched against the address.")

(defvar spam-blacklist (expand-file-name "blacklist" spam-directory)
  "The location of the blacklist.
The file format is one regular expression per line.
The regular expression is matched against the address.")

(defvar spam-whitelist-cache nil)
(defvar spam-blacklist-cache nil)

(defun spam-enter-whitelist (address &optional blacklist)
  "Enter ADDRESS into the whitelist.
Optional arg BLACKLIST, if non-nil, means to enter in the blacklist instead."
  (interactive "sAddress: ")
  (let ((file (if blacklist spam-blacklist spam-whitelist)))
    (unless (file-exists-p (file-name-directory file))
      (make-directory (file-name-directory file) t))
    (save-excursion
      (set-buffer
       (find-file-noselect file))
      (goto-char (point-max))
      (unless (bobp)
	(insert "\n"))
      (insert address "\n")
      (save-buffer))))

(defun spam-enter-blacklist (address)
  "Enter ADDRESS into the blacklist."
  (interactive "sAddress: ")
  (spam-enter-whitelist address t))

(eval-and-compile
  (defalias 'spam-point-at-eol (if (fboundp 'point-at-eol)
				   'point-at-eol
				 'line-end-position)))

(defun spam-parse-whitelist (&optional blacklist)
  (let ((file (if blacklist spam-blacklist spam-whitelist))
	contents address)
    (when (file-exists-p file)
      (with-temp-buffer
	(insert-file-contents file)
	(while (not (eobp))
	  (setq address (buffer-substring (point) (spam-point-at-eol)))
	  (forward-line 1)
	  (unless (zerop (length address))
	    (setq address (regexp-quote address))
	    (while (string-match "\\\\\\*" address)
	      (setq address (replace-match ".*" t t address)))
	    (push address contents))))
      (nreverse contents))))

(defun spam-refresh-list-cache ()
  (setq spam-whitelist-cache (spam-parse-whitelist))
  (setq spam-blacklist-cache (spam-parse-whitelist t)))

(defun spam-address-whitelisted-p (address &optional blacklist)
  (let ((cache (if blacklist spam-blacklist-cache spam-whitelist-cache))
	found)
    (while (and (not found)
		cache)
      (when (string-match (pop cache) address)
	(setq found t)))
    found))

(provide 'spam)

;;; spam.el ends here
