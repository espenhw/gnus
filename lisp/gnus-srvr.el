;;; gnus-srvr.el --- virtual server support for Gnus
;; Copyright (C) 1995 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@ifi.uio.no>
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
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;;; Code:

(require 'gnus)
(eval-when-compile (require 'cl))

(defvar gnus-server-mode-hook nil
  "Hook run in `gnus-server-mode' buffers.")

(defconst gnus-server-line-format "     {%(%h:%w%)} %s\n"
  "Format of server lines.
It works along the same lines as a normal formatting string,
with some simple extensions.")

(defvar gnus-server-mode-line-format "Gnus  List of servers"
  "The format specification for the server mode line.")

;;; Internal variables.

(defconst gnus-server-line-format-alist
  (` ((?h how ?s)
      (?n name ?s)
      (?w where ?s)
      (?s status ?s)))) 

(defconst gnus-server-mode-line-format-alist 
  (` ((?S news-server ?s)
      (?M news-method ?s)
      (?u user-defined ?s))))

(defvar gnus-server-line-format-spec nil)
(defvar gnus-server-mode-line-format-spec nil)
(defvar gnus-server-killed-servers nil)

(defvar gnus-server-mode-map nil)
(put 'gnus-server-mode 'mode-class 'special)

(if gnus-server-mode-map
    nil
  (setq gnus-server-mode-map (make-sparse-keymap))
  (suppress-keymap gnus-server-mode-map)
  (define-key gnus-server-mode-map " " 'gnus-server-read-server)
  (define-key gnus-server-mode-map "\r" 'gnus-server-read-server)
  (define-key gnus-server-mode-map gnus-mouse-2 'gnus-server-pick-server)
  (define-key gnus-server-mode-map "q" 'gnus-server-exit)
  (define-key gnus-server-mode-map "l" 'gnus-server-list-servers)
  (define-key gnus-server-mode-map "k" 'gnus-server-kill-server)
  (define-key gnus-server-mode-map "y" 'gnus-server-yank-server)
  (define-key gnus-server-mode-map "c" 'gnus-server-copy-server)
  (define-key gnus-server-mode-map "a" 'gnus-server-add-server)
  (define-key gnus-server-mode-map "e" 'gnus-server-edit-server)

  (define-key gnus-server-mode-map "O" 'gnus-server-open-server)
  (define-key gnus-server-mode-map "C" 'gnus-server-close-server)
  (define-key gnus-server-mode-map "D" 'gnus-server-deny-server)
  (define-key gnus-server-mode-map "R" 'gnus-server-remove-denials)
  )

(defun gnus-server-mode ()
  "Major mode for listing and editing servers.

All normal editing commands are switched off.
\\<gnus-server-mode-map>

For more in-depth information on this mode, read the manual (`\\[gnus-info-find-node]'). 

The following commands are available:

\\{gnus-server-mode-map}"
  (interactive)
  (if (gnus-visual-p 'server-menu 'menu) (gnus-server-make-menu-bar))
  (kill-all-local-variables)
  (setq mode-line-modified "-- ")
  (make-local-variable 'mode-line-format)
  (setq mode-line-format (copy-sequence mode-line-format))
  (and (equal (nth 3 mode-line-format) "   ")
       (setcar (nthcdr 3 mode-line-format) ""))
  (setq major-mode 'gnus-server-mode)
  (setq mode-name "Server")
					;  (gnus-group-set-mode-line)
  (setq mode-line-process nil)
  (use-local-map gnus-server-mode-map)
  (buffer-disable-undo (current-buffer))
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (run-hooks 'gnus-server-mode-hook))

(defun gnus-server-insert-server-line (sformat name method)
  (let* ((sformat (or sformat gnus-server-line-format-spec))
	 (how (car method))
	 (where (nth 1 method))
	 (elem (assoc method gnus-opened-servers))
	 (status (cond ((eq (nth 1 elem) 'denied)
			"(denied)")
		       ((gnus-server-opened method)
			"(open)")
		       (t
			"(closed)")))
	 b)
    (beginning-of-line)
    (setq b (point))
    ;; Insert the text.
    (insert (eval sformat))
    (add-text-properties b (1+ b) (list 'gnus-server (intern name)))))

(defun gnus-server-enter-server-buffer ()
  "Set up the server buffer."
  (gnus-server-setup-buffer)
  (gnus-configure-windows 'server)
  (gnus-server-prepare))

(defun gnus-server-setup-buffer ()
  (if (get-buffer gnus-server-buffer)
      ()
    (save-excursion
      (set-buffer (get-buffer-create gnus-server-buffer))
      (gnus-server-mode)
      (and gnus-carpal (gnus-carpal-setup-buffer 'server)))))

(defun gnus-server-prepare ()
  (setq gnus-server-mode-line-format-spec 
	(gnus-parse-format gnus-server-mode-line-format 
			   gnus-server-mode-line-format-alist))
  (setq gnus-server-line-format-spec 
	(gnus-parse-format gnus-server-line-format 
			   gnus-server-line-format-alist))
  (let ((alist gnus-server-alist)
	(buffer-read-only nil)
	(opened gnus-opened-servers)
	done)
    (erase-buffer)
    ;; First we do the real list of servers.
    (while alist
      (gnus-server-insert-server-line nil (car (car alist)) (cdr (car alist)))
      (and (assoc (cdr (car alist)) gnus-opened-servers)
	   (setq done (cons (cdr (car alist)) done)))
      (setq alist (cdr alist)))
    ;; Then we insert the list of servers that have been opened in
    ;; this session.
    (while opened 
      (or (member (car (car opened)) done)
	  (gnus-server-insert-server-line 
	   nil (format "%s:%s" (car (car (car opened))) 
		       (nth 1 (car (car opened))))
	   (car (car opened))))
      (setq opened (cdr opened))))
  (goto-char (point-min))
  (gnus-server-position-point))

(defun gnus-server-server-name ()
  (let ((server (get-text-property (gnus-point-at-bol) 'gnus-server)))
    (and server (symbol-name server))))

(defalias 'gnus-server-position-point 'gnus-goto-colon)

(defconst gnus-server-edit-buffer "*Gnus edit server*")

(defun gnus-server-update-server (server)
  (save-excursion
    (set-buffer gnus-server-buffer)
    (let ((buffer-read-only nil)
	  (info (cdr (assoc server gnus-server-alist))))
      (gnus-dribble-enter 
       (concat "(gnus-server-set-info \"" server "\" '"
	       (prin1-to-string info) ")"))
      ;; Buffer may be narrowed.
      (save-restriction
	(widen)
	(if (gnus-server-goto-server server)
	    (delete-region (progn (beginning-of-line) (point))
			   (progn (forward-line 1) (point))))
	(let ((entry (assoc server gnus-server-alist)))
	  (gnus-server-insert-server-line nil (car entry) (cdr entry))
	  (gnus-server-position-point))))))

(defun gnus-server-set-info (server info)
  ;; Enter a select method into the virtual server alist.
  (gnus-dribble-enter 
   (concat "(gnus-server-set-info \"" server "\" '"
	   (prin1-to-string info) ")"))
  (let* ((server (nth 1 info))
	 (entry (assoc server gnus-server-alist)))
    (if entry (setcdr entry info)
      (setq gnus-server-alist
	    (nconc gnus-server-alist (list (cons server info)))))))

;;; Interactive server functions.

(defun gnus-server-kill-server (server)
  "Kill the server on the current line."
  (interactive (list (gnus-server-server-name)))
  (or (gnus-server-goto-server server)
      (if server (error "No such server: %s" server)
	(error "No server on the current line")))
  (gnus-dribble-enter "")
  (let ((buffer-read-only nil))
    (delete-region (progn (beginning-of-line) (point))
		   (progn (forward-line 1) (point))))
  (setq gnus-server-killed-servers 
	(cons (assoc server gnus-server-alist) gnus-server-killed-servers))
  (setq gnus-server-alist (delq (car gnus-server-killed-servers)
				gnus-server-alist))
  (gnus-server-position-point))

(defun gnus-server-yank-server ()
  "Yank the previously killed server."
  (interactive)
  (or gnus-server-killed-servers
      (error "No killed servers to be yanked"))
  (let ((alist gnus-server-alist)
	(server (gnus-server-server-name))
	(killed (car gnus-server-killed-servers)))
    (if (not server) 
	(setq gnus-server-alist (nconc gnus-server-alist (list killed)))
      (if (string= server (car (car gnus-server-alist)))
	  (setq gnus-server-alist (cons killed gnus-server-alist))
	(while (and (cdr alist)
		    (not (string= server (car (car (cdr alist))))))
	  (setq alist (cdr alist)))
	(setcdr alist (cons killed (cdr alist)))
	(if alist
	    (setcdr alist (cons killed (cdr alist)))
 	  (setq gnus-server-alist (list killed)))))
    (gnus-server-update-server (car killed))
    (setq gnus-server-killed-servers (cdr gnus-server-killed-servers))
    (gnus-server-position-point)))

(defun gnus-server-exit ()
  "Return to the group buffer."
  (interactive)
  (kill-buffer (current-buffer))
  (switch-to-buffer gnus-group-buffer))

(defun gnus-server-list-servers ()
  "List all available servers."
  (interactive)
  (let ((cur (gnus-server-server-name)))
    (gnus-server-prepare)
    (if cur (gnus-server-goto-server cur)
      (goto-char (point-max))
      (forward-line -1))
    (gnus-server-position-point)))

(defun gnus-opened-servers-remove (method)
  "Remove METHOD from the list of opened servers."
  (setq gnus-opened-servers (delq (assoc method gnus-opened-servers)
				  gnus-opened-servers)))

(defun gnus-server-open-server (server)
  "Force an open of SERVER."
  (interactive (list (gnus-server-server-name)))
  (let ((method (gnus-server-to-method server)))
    (or method (error "No such server: %s" server))
    (gnus-opened-servers-remove method)
    (prog1
	(or (gnus-open-server method)
	    (progn (message "Couldn't open %s" server) nil))
      (gnus-server-update-server server)
      (gnus-server-position-point))))

(defun gnus-server-close-server (server)
  "Close SERVER."
  (interactive (list (gnus-server-server-name)))
  (let ((method (gnus-server-to-method server)))
    (or method (error "No such server: %s" server))
    (gnus-opened-servers-remove method)
    (prog1
	(gnus-close-server method)
      (gnus-server-update-server server)
      (gnus-server-position-point))))

(defun gnus-server-deny-server (server)
  "Make sure SERVER will never be attempted opened."
  (interactive (list (gnus-server-server-name)))
  (let ((method (gnus-server-to-method server)))
    (or method (error "No such server: %s" server))
    (gnus-opened-servers-remove method)
    (setq gnus-opened-servers
	  (cons (list method 'denied) gnus-opened-servers)))
  (gnus-server-update-server server)
  (gnus-server-position-point))

(defun gnus-server-remove-denials ()
  "Remove all marks as to whether Gnus could open servers or not."
  (interactive)
  (setq gnus-opened-servers nil)
  (gnus-server-list-servers))

(defun gnus-server-copy-server (from to)
  (interactive
   (list
    (or (gnus-server-server-name)
	(error "No server on the current line"))
    (read-string "Copy to: ")))
  (or from (error "No server on current line"))
  (or (and to (not (string= to ""))) (error "No name to copy to"))
  (and (assoc to gnus-server-alist) (error "%s already exists" to))
  (or (assoc from gnus-server-alist) 
      (error "%s: no such server" from))
  (let ((to-entry (gnus-copy-sequence (assoc from gnus-server-alist))))
    (setcar to-entry to)
    (setcar (nthcdr 2 to-entry) to)
    (setq gnus-server-killed-servers 
	  (cons to-entry gnus-server-killed-servers))
    (gnus-server-yank-server)))

(defun gnus-server-add-server (how where)
  (interactive 
   (list (intern (completing-read "Server method: "
				  gnus-valid-select-methods nil t))
	 (read-string "Server name: ")))
  (setq gnus-server-killed-servers 
	(cons (list where how where) gnus-server-killed-servers))
  (gnus-server-yank-server))

(defun gnus-server-goto-server (server)
  "Jump to a server line."
  (interactive
   (list (completing-read "Goto server: " gnus-server-alist nil t)))
  (let ((to (text-property-any (point-min) (point-max) 
			       'gnus-server (intern server))))
    (and to
	 (progn
	   (goto-char to) 
	   (gnus-server-position-point)))))

(defun gnus-server-edit-server (server)
  "Edit the server on the current line."
  (interactive (list (gnus-server-server-name)))
  (or server
      (error "No server on current line"))
  (let ((winconf (current-window-configuration)))
    (get-buffer-create gnus-server-edit-buffer)
    (gnus-configure-windows 'edit-server)
    (gnus-add-current-to-buffer-list)
    (emacs-lisp-mode)
    (make-local-variable 'gnus-prev-winconf)
    (setq gnus-prev-winconf winconf)
    (use-local-map (copy-keymap (current-local-map)))
    (let ((done-func '(lambda () 
			"Exit editing mode and update the information."
			(interactive)
			(gnus-server-edit-server-done 'group))))
      (setcar (cdr (nth 4 done-func)) server)
      (local-set-key "\C-c\C-c" done-func))
    (erase-buffer)
    (insert ";; Type `C-c C-c' after you have edited the server.\n\n")
    (insert (pp-to-string (cdr (assoc server gnus-server-alist))))))

(defun gnus-server-edit-server-done (server)
  (interactive)
  (set-buffer (get-buffer-create gnus-server-edit-buffer))
  (goto-char (point-min))
  (let ((form (read (current-buffer)))
	(winconf gnus-prev-winconf))
    (gnus-server-set-info server form)
    (kill-buffer (current-buffer))
    (and winconf (set-window-configuration winconf))
    (set-buffer gnus-server-buffer)
    (gnus-server-update-server (gnus-server-server-name))
    (gnus-server-list-servers)
    (gnus-server-position-point)))

(defun gnus-server-read-server (server)
  "Browse a server."
  (interactive (list (gnus-server-server-name)))
  (gnus-browse-foreign-server (gnus-server-to-method server) (current-buffer)))

(defun gnus-mouse-pick-server (e)
  (interactive "e")
  (mouse-set-point e)
  (gnus-server-read-server (gnus-server-server-name)))

;;; gnus-srvr.el ends here.
