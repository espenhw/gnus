;;; dgnushack.el --- a hack to set the load path for byte-compiling
;; Copyright (C) 1994,95 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@ifi.uio.no>
;; Version: 4.19
;; Keywords: news, path

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

;; Is this really the only way to set the load path? Seems awfully
;; kludgy to load this file just to do something as simple as
;; that... Anyways, it won't be in the production code, so who cares?

;;; Code:

(setq byte-compile-warnings '(free-vars unresolved callargs redefine))

(setq load-path (cons "." load-path))

(defun dgnushack-recompile ()
  (byte-recompile-directory "." 0))

;;; dgnushack.el ends here  

