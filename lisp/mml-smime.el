;;; mml-smime.el --- S/MIME support for MML
;; Copyright (c) 2000 Free Software Foundation, Inc.

;; Author: Simon Josefsson <simon@josefsson.org>
;; Keywords: Gnus, MIME, SMIME, MML

;; This file is a part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
;; option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; todo: move s/mime code from mml-sec.el here.

;;; Code:

(require 'smime)

(defun mml-smime-verify (handle ctl)
  (smime-verify-buffer)
  handle)

(provide 'mml-smime)

;;; mml-smime.el ends here
