;;; assistant.el --- guiding users through Emacs setup
;; Copyright (C) 2004 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: util

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

(eval-when-compile
  (require 'cl))

(defvar assistant-readers
  '(("variable" assistant-variable-reader)
    ("validate" assistant-sexp-reader)
    ("result" assistant-list-reader)
    ("next" assistant-list-reader)))

;;; Internal variables

(defvar assistant-data nil)
(defvar assistant-current-node nil)

(defun assistant-parse-buffer ()
  (let (results command value)
    (goto-char (point-min))
    (while (search-forward "@" nil t)
      (if (not (looking-at "[^ \t\n]+"))
	  (error "Dangling @")
	(setq command (downcase (match-string 0)))
	(goto-char (match-end 0)))
      (setq value
	    (if (looking-at "[ \t]*\n")
		(let (start)
		  (forward-line 1)
		  (setq start (point))
		  (unless (re-search-forward (concat "^@end " command) nil t)
		    (error "No @end %s found" command))
		  (beginning-of-line)
		  (prog1
		      (buffer-substring start (point))
		    (forward-line 1)))
	      (skip-chars-forward " \t")
	      (prog1
		  (buffer-substring (point) (line-end-position))
		(forward-line 1))))
      (push (list command (assistant-reader command value))
	    results))
    (assistant-segment (nreverse results))))

;; Segment the raw assistant data into a list of nodes.
(defun assistant-segment (list)
  (let ((ast nil)
	(node nil)
	(title (pop list)))
    (dolist (elem list)
      (when (and (equal (car elem) "node")
		 node)
	(push (nreverse node) ast)
	(setq node nil))
      (push elem node))
    (when node
      (push (nreverse node) ast))
    (cons title (nreverse ast))))

(defun assistant-reader (command value)
  (let ((formatter (cadr (assoc command assistant-readers))))
    (if (not formatter)
	value
      (funcall formatter value))))

(defun assistant-list-reader (value)
  (car (read-from-string (concat "(" value ")"))))

(defun assistant-variable-reader (value)
  (let ((section (car (read-from-string (concat "(" value ")")))))
    (append section (list (nth 2 section)))))

(defun assistant-sexp-reader (value)
  (if (zerop (length value))
      nil
    (car (read-from-string value))))

(defun assistant-buffer-name (title)
  (format "*Assistant %s*" title))

(defun assistant-get (ast command)
  (cadr (assoc command ast)))

(defun assistant-get-list (ast command)
  (let ((result nil))
    (dolist (elem ast)
      (when (equal (car elem) command)
	(push elem result)))
    (nreverse result)))

(defun assistant (file)
  "Assist setting up Emacs based on FILE."
  (interactive "fAssistant file name: ")
  (let ((ast
	 (with-temp-buffer
	   (insert-file-contents file)
	   (assistant-parse-buffer))))
    (pop-to-buffer (assistant-buffer-name (assistant-get ast "title")))
    (assistant-render ast)))

(defun assistant-render (ast)
  (let ((first-node (assistant-get (nth 1 ast) "node")))
    (set (make-local-variable 'assistant-data) ast)
    (set (make-local-variable 'assistant-current-node) first-node)
    (set (make-local-variable 'assistant-previous-node) nil)
    (assistant-render-node first-node)))

(defun assistant-find-node (node-name)
  (let ((ast (cdr assistant-data)))
    (while (and ast
		(not (string= node-name (assistant-get (car ast) "node"))))
      (pop ast))
    (car ast)))

(defun assistant-insert-previous-node (node)
  (insert (format "[ << Go back to %s ]  " node)))

(defun assistant-insert-next-node (node)
  (if node
      (insert (format "[ Proceed to %s >> ]" node))
    (insert "[ Finish ]")))

(defun assistant-render-node (node-name)
  (let ((node (assistant-find-node node-name)))
    (setq assistant-current-node node-name)
    (erase-buffer)
    (insert (cadar assistant-data) "\n\n")
    (insert node-name "\n\n")
    (insert (assistant-get node "text") "\n\n")
    (when assistant-previous-node
      (assistant-insert-previous-node assistant-previous-node))
    (assistant-insert-next-node (assistant-find-next-node))
    (insert "\n")))

(defun assistant-find-next-node ()
  (let* ((node (assistant-find-node node-name))
	 (nexts (assistant-get-list node "next"))
	 next)
    (while (and (setq elem (pop nexts))
		(not next))
      (when (assistant-eval (car elem) node)
	(setq next (cadr elem))))
    next))
      
(defun assistant-eval (form node)
  (let ((bindings nil))
    (dolist (variable (assistant-get-list node "variable"))
      (push (list (car variable) (nth 3 variable))
	    bingdings))
    (eval
     `(let ,bindings
	,@form))))

(provide 'assistant)
