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

(provide 'spam)

;;; spam.el ends here
