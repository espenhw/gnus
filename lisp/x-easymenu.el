;;; easymenu.el - Easy menu support for Emacs 19 and XEmacs.
;; 
;; $Id: easymenu.el,v 5.9 1995/02/14 19:44:00 amanda Exp $
;;
;; LCD Archive Entry:
;; easymenu|Per Abrahamsen|abraham@iesd.auc.dk|
;; Easy menu support for XEmacs|
;; $Date: 1995/02/14 19:44:00 $|$Revision: 5.9 $|~/misc/easymenu.el.gz|

;; Copyright (C) 1992, 1993, 1994, 1995 Free Software Foundation, Inc.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;; Commentary:
;;
;; Easymenu allows you to define menus for both Emacs 19 and XEmacs.
;;
;; This file 
;; The advantages of using easymenu are:
;;
;; - Easier to use than either the Emacs 19 and XEmacs menu syntax.
;;
;; - Common interface for Emacs 18, Emacs 19, and XEmacs.  
;;   (The code does nothing when run under Emacs 18).
;;
;; The public functions are:
;; 
;; - Function: easy-menu-define SYMBOL MAPS DOC MENU
;;     SYMBOL is both the name of the variable that holds the menu and
;;            the name of a function that will present a the menu.
;;     MAPS is a list of keymaps where the menu should appear in the menubar.
;;     DOC is the documentation string for the variable.
;;     MENU is an XEmacs style menu description.  
;;
;;     See the documentation for easy-menu-define for details.
;;
;; - Function: easy-menu-change PATH NAME ITEMS
;;     Change an existing menu.
;;     The menu must already exist an be visible on the menu bar.
;;     PATH is a list of strings used for locating the menu on the menu bar. 
;;     NAME is the name of the menu.  
;;     ITEMS is a list of menu items, as defined in `easy-menu-define'.
;;
;; - Function: easy-menu-add MENU [ MAP ]
;;     Add MENU to the current menubar in MAP.
;;
;; - Function: easy-menu-remove MENU
;;     Remove MENU from the current menubar.
;;
;; Emacs 19 never uses `easy-menu-add' or `easy-menu-remove', menus
;; automatically appear and disappear when the keymaps specified by
;; the MAPS argument to `easy-menu-define' are activated.
;;
;; XEmacs will bind the map to button3 in each MAPS, but you must
;; explicitly call `easy-menu-add' and `easy-menu-remove' to add and
;; remove menus from the menu bar.

;;; Code:

;;;###autoload
(defmacro easy-menu-define (symbol maps doc menu)
  "Define a menu bar submenu in maps MAPS, according to MENU.
The arguments SYMBOL and DOC are ignored; they are present for
compatibility only.  SYMBOL is not evaluated.  In other Emacs versions
these arguments may be used as a variable to hold the menu data, and a
doc string for that variable.

The first element of MENU must be a string.  It is the menu bar item name.
The rest of the elements are menu items.

A menu item is usually a vector of three elements:  [NAME CALLBACK ENABLE]

NAME is a string--the menu item name.

CALLBACK is a command to run when the item is chosen,
or a list to evaluate when the item is chosen.

ENABLE is an expression; the item is enabled for selection
whenever this expression's value is non-nil.

Alternatively, a menu item may have the form: 

   [ NAME CALLBACK [ KEYWORD ARG ] ... ]

Where KEYWORD is one of the symbol defined below.

   :keys KEYS

KEYS is a string; a complex keyboard equivalent to this menu item.

   :active ENABLE

ENABLE is an expression; the item is enabled for selection
whenever this expression's value is non-nil.

   :suffix NAME

NAME is a string; the name of an argument to CALLBACK.

   :style STYLE
   
STYLE is a symbol describing the type of menu item.  The following are
defined:  

toggle: A checkbox.  
        Currently just prepend the name with the string \"Toggle \".
radio: A radio button. 
nil: An ordinary menu item.

   :selected SELECTED

SELECTED is an expression; the checkbox or radio button is selected
whenever this expression's value is non-nil.
Currently just disable radio buttons, no effect on checkboxes.

A menu item can be a string.  Then that string appears in the menu as
unselectable text.  A string consisting solely of hyphens is displayed
as a solid horizontal line.

A menu item can be a list.  It is treated as a submenu.
The first element should be the submenu name.  That's used as the
menu item in the top-level menu.  The cdr of the submenu list
is a list of menu items, as above."
  (` (progn
       (defvar (, symbol) nil (, doc))
       (easy-menu-do-define (quote (, symbol)) (, maps) (, doc) (, menu)))))

(defun easy-menu-do-define (symbol maps doc menu)
  (set symbol menu)
  (fset symbol (list 'lambda '(e)
		     doc
		     '(interactive "@e")
		     '(run-hooks 'activate-menubar-hook)
		     '(setq zmacs-region-stays 't)
		     (list 'popup-menu symbol)))
  (mapcar (function (lambda (map) (define-key map 'button3 symbol)))
	  (if (keymapp maps) (list maps) maps)))

(fset 'easy-menu-change (symbol-function 'add-menu))

(defun easy-menu-add (menu &optional map)
  "Add MENU to the current menu bar."
  (cond ((null current-menubar)
	 ;; Don't add it to a non-existing menubar.
	 nil)
	((assoc (car menu) current-menubar)
	 ;; Already present.
	 nil)
	((equal current-menubar '(nil))
	 ;; Set at left if only contains right marker.
	 (set-buffer-menubar (list menu nil)))
	(t
	 ;; Add at right.
	 (set-buffer-menubar (copy-sequence current-menubar))
	 (add-menu nil (car menu) (cdr menu)))))

(defun easy-menu-remove (menu)
  "Remove MENU from the current menu bar."
  (and current-menubar
       (assoc (car menu) current-menubar)
       (delete-menu-item (list (car menu)))))

(provide 'easymenu)

;;; easymenu.el ends here
;;; easymenu.el - Easy menu support for Emacs 19 and XEmacs.
;; 
;; $Id: easymenu.el,v 5.9 1995/02/14 19:44:00 amanda Exp $
;;
;; LCD Archive Entry:
;; easymenu|Per Abrahamsen|abraham@iesd.auc.dk|
;; Easy menu support for XEmacs|
;; $Date: 1995/02/14 19:44:00 $|$Revision: 5.9 $|~/misc/easymenu.el.gz|

;; Copyright (C) 1992, 1993, 1994, 1995 Free Software Foundation, Inc.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;; Commentary:
;;
;; Easymenu allows you to define menus for both Emacs 19 and XEmacs.
;;
;; This file 
;; The advantages of using easymenu are:
;;
;; - Easier to use than either the Emacs 19 and XEmacs menu syntax.
;;
;; - Common interface for Emacs 18, Emacs 19, and XEmacs.  
;;   (The code does nothing when run under Emacs 18).
;;
;; The public functions are:
;; 
;; - Function: easy-menu-define SYMBOL MAPS DOC MENU
;;     SYMBOL is both the name of the variable that holds the menu and
;;            the name of a function that will present a the menu.
;;     MAPS is a list of keymaps where the menu should appear in the menubar.
;;     DOC is the documentation string for the variable.
;;     MENU is an XEmacs style menu description.  
;;
;;     See the documentation for easy-menu-define for details.
;;
;; - Function: easy-menu-change PATH NAME ITEMS
;;     Change an existing menu.
;;     The menu must already exist an be visible on the menu bar.
;;     PATH is a list of strings used for locating the menu on the menu bar. 
;;     NAME is the name of the menu.  
;;     ITEMS is a list of menu items, as defined in `easy-menu-define'.
;;
;; - Function: easy-menu-add MENU [ MAP ]
;;     Add MENU to the current menubar in MAP.
;;
;; - Function: easy-menu-remove MENU
;;     Remove MENU from the current menubar.
;;
;; Emacs 19 never uses `easy-menu-add' or `easy-menu-remove', menus
;; automatically appear and disappear when the keymaps specified by
;; the MAPS argument to `easy-menu-define' are activated.
;;
;; XEmacs will bind the map to button3 in each MAPS, but you must
;; explicitly call `easy-menu-add' and `easy-menu-remove' to add and
;; remove menus from the menu bar.

;;; Code:

;;;###autoload
(defmacro easy-menu-define (symbol maps doc menu)
  "Define a menu bar submenu in maps MAPS, according to MENU.
The arguments SYMBOL and DOC are ignored; they are present for
compatibility only.  SYMBOL is not evaluated.  In other Emacs versions
these arguments may be used as a variable to hold the menu data, and a
doc string for that variable.

The first element of MENU must be a string.  It is the menu bar item name.
The rest of the elements are menu items.

A menu item is usually a vector of three elements:  [NAME CALLBACK ENABLE]

NAME is a string--the menu item name.

CALLBACK is a command to run when the item is chosen,
or a list to evaluate when the item is chosen.

ENABLE is an expression; the item is enabled for selection
whenever this expression's value is non-nil.

Alternatively, a menu item may have the form: 

   [ NAME CALLBACK [ KEYWORD ARG ] ... ]

Where KEYWORD is one of the symbol defined below.

   :keys KEYS

KEYS is a string; a complex keyboard equivalent to this menu item.

   :active ENABLE

ENABLE is an expression; the item is enabled for selection
whenever this expression's value is non-nil.

   :suffix NAME

NAME is a string; the name of an argument to CALLBACK.

   :style STYLE
   
STYLE is a symbol describing the type of menu item.  The following are
defined:  

toggle: A checkbox.  
        Currently just prepend the name with the string \"Toggle \".
radio: A radio button. 
nil: An ordinary menu item.

   :selected SELECTED

SELECTED is an expression; the checkbox or radio button is selected
whenever this expression's value is non-nil.
Currently just disable radio buttons, no effect on checkboxes.

A menu item can be a string.  Then that string appears in the menu as
unselectable text.  A string consisting solely of hyphens is displayed
as a solid horizontal line.

A menu item can be a list.  It is treated as a submenu.
The first element should be the submenu name.  That's used as the
menu item in the top-level menu.  The cdr of the submenu list
is a list of menu items, as above."
  (` (progn
       (defvar (, symbol) nil (, doc))
       (easy-menu-do-define (quote (, symbol)) (, maps) (, doc) (, menu)))))

(defun easy-menu-do-define (symbol maps doc menu)
  (set symbol menu)
  (fset symbol (list 'lambda '(e)
		     doc
		     '(interactive "@e")
		     '(run-hooks 'activate-menubar-hook)
		     '(setq zmacs-region-stays 't)
		     (list 'popup-menu symbol)))
  (mapcar (function (lambda (map) (define-key map 'button3 symbol)))
	  (if (keymapp maps) (list maps) maps)))

(fset 'easy-menu-change (symbol-function 'add-menu))

(defun easy-menu-add (menu &optional map)
  "Add MENU to the current menu bar."
  (cond ((null current-menubar)
	 ;; Don't add it to a non-existing menubar.
	 nil)
	((assoc (car menu) current-menubar)
	 ;; Already present.
	 nil)
	((equal current-menubar '(nil))
	 ;; Set at left if only contains right marker.
	 (set-buffer-menubar (list menu nil)))
	(t
	 ;; Add at right.
	 (set-buffer-menubar (copy-sequence current-menubar))
	 (add-menu nil (car menu) (cdr menu)))))

(defun easy-menu-remove (menu)
  "Remove MENU from the current menu bar."
  (and current-menubar
       (assoc (car menu) current-menubar)
       (delete-menu-item (list (car menu)))))

(provide 'easymenu)

;;; easymenu.el ends here
