;;; custom-opt.el --- An option group.
;;
;; Copyright (C) 1996 Free Software Foundation, Inc.
;;
;; Author: Per Abrahamsen <abraham@dina.kvl.dk>
;; Keywords: help, faces
;; Version: 1.20
;; X-URL: http://www.dina.kvl.dk/~abraham/custom/

;;; Code:

(require 'custom)

(defgroup options nil
  "This group contains often used customization options."
  :group 'emacs)

(defvar custom-options 
  '((line-number-mode boolean)
    (column-number-mode boolean)
    (debug-on-error boolean)
    (debug-on-quit boolean)
    (case-fold-search boolean)
    (case-replace boolean)
    (transient-mark-mode boolean))
  "Alist of customization options.
The first element of each entry should be a variable name, the second
a widget type.")

(let ((options custom-options)
      option name type)
  (while options
    (setq option (car options)
	  options (cdr options)
	  name (nth 0 option)
	  type (nth 1 option))
    (put name 'custom-type type)
    (custom-add-to-group 'options name 'custom-variable))
  (run-hooks 'custom-define-hook))

;;; The End.

(provide 'custom-opt)

;; custom-edit.el ends here
