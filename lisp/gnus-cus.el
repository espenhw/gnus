;;; gnus-cus.el --- User friendly customization of GNUS.
;; Copyright (C) 1995 Free Software Foundation, Inc.
;;
;; Author: Per Abrahamsen <abraham@iesd.auc.dk>
;; Keywords: help, news
;; Version: 0.0

;;; Code:

(require 'custom)

(custom-declare '()
  '((tag . "GNUS")
    (doc . "\
The coffe-brewing, all singing, all dancing, kitchen sink newsreader.")
    (type . group)
    (data ((tag . "Visual")
	   (doc . "\
GNUS can be made colorful and fun or grey and dull as you wish.")
	   (type . group)
	   (data ((tag . "Visual")
		  (doc . "Enable visual features.
If `visual' is disabled, there will be no menus and no faces.  All
the visual customization options below will be ignored.  GNUS will use
less space and be faster as a result.")
		  (default . t)
		  (name . gnus-visual)
		  (type . toggle))
		 ((tag . "Summary Selected Face")
		  (doc . "\
Face used for highlighting the current article in the summary buffer.")
		  (name . gnus-summary-selected-face)
		  (default . underline)
		  (type . face))
;;; gnus-summary-highlight
;;;   need cons and sexp
		 )))))

(provide 'gnus-cus)

;;; gnus-cus.el ends here
