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

;; This support creation of S/MIME parts in MML.

;; Usage:
;;    (mml-smime-setup)
;; 
;; Insert an attribute, postprocess=smime-sign (or smime-encrypt), into
;; the mml tag to be signed (or encrypted).
;;
;; It is based on rfc2015.el by Shenghuo Zhu.

;;; Code:

(require 'smime)

;;;###autoload
(defun mml-smime-sign (cont)
  ;; FIXME: You have to input the sender.
  (when (null smime-keys)
    (error "Please use M-x customize RET smime RET to configure SMIME"))
  (smime-sign-buffer)
  (goto-char (point-min))
  (when (looking-at "^MIME-Version: 1.0")
    (forward-line 1)
    (delete-region (point-min) (point)))
  (goto-char (point-max)))
  
;;;###autoload
(defun mml-smime-encrypt (cont)
  ;; FIXME: You have to input the receiptant.
  ;; FIXME: Should encrypt to myself so I can read it??
  (smime-encrypt-buffer)
  (goto-char (point-min))
  (when (looking-at "^MIME-Version: 1.0")
    (forward-line 1)
    (delete-region (point-min) (point)))
  (goto-char (point-max)))

;;;###autoload
(defun mml-smime-setup ()
  )

(provide 'mml-smime)

;;; mml-smime.el ends here
