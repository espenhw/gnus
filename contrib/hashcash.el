;;; hashcash.el --- Add hashcash payments to email

;; $Revision: 1.3 $
;; Copyright (C) 1997,2001 Paul E. Foley

;; Maintainer: Paul Foley <mycroft@actrix.gen.nz>
;; Keywords: mail, hashcash

;; Released under the GNU General Public License

;;; Commentary:

;; The hashcash binary is at http://www.cypherspace.org/hashcash/
;;
;; Call mail-add-payment to add a hashcash payment to a mail message
;; in the current buffer.
;;
;; To automatically add payments to all outgoing mail:
;;    (add-hook 'message-send-hook 'mail-add-payment)

;;; Code:

(defcustom hashcash-default-payment 0
  "*The default number of bits to pay to unknown users.
If this is zero, no payment header will be generated.
See `hashcash-payment-alist'."
  :type 'integer)

(defcustom hashcash-payment-alist nil
  "*An association list mapping email addresses to payment amounts.
Elements may consist of (ADDR AMOUNT) or (ADDR STRING AMOUNT), where
ADDR is the email address of the intended recipient and AMOUNT is
the value of hashcash payment to be made to that user.  STRING, if
present, is the string to be hashed; if not present ADDR will be used.")

(defcustom hashcash "hashcash"
  "*The path to the hashcash binary.")

(require 'mail-utils)

(defun hashcash-strip-quoted-names (addr)
  (setq addr (mail-strip-quoted-names addr))
  (if (and addr (string-match "^[^+@]+\\(\\+[^@]*\\)@" addr))
      (concat (subseq addr 0 (match-beginning 1)) (subseq addr (match-end 1)))
    addr))

(defun hashcash-payment-required (addr)
  "Return the hashcash payment value required for the given address."
  (let ((val (assoc addr hashcash-payment-alist)))
    (if val
	(if (cddr val)
	    (caddr val)
	  (cadr val))
      hashcash-default-payment)))

(defun hashcash-payment-to (addr)
  "Return the string with which hashcash payments should collide."
  (let ((val (assoc addr hashcash-payment-alist)))
    (if val
	(if (cddr val)
	    (cadr val)
	  (car val))
      addr)))

(defun hashcash-generate-payment (str val)
  "Generate a hashcash payment by finding a VAL-bit collison on STR."
  (if (> val 0)
      (save-excursion
	(set-buffer (get-buffer-create " *hashcash*"))
	(erase-buffer)
	(call-process hashcash nil t nil (concat "-b " (number-to-string val))
		      str)
	(goto-char (point-min))
	(buffer-substring (point-at-bol) (point-at-eol)))
    nil))

(defun hashcash-insert-payment (arg)
  "Insert an X-Hashcash header with a payment for ARG"
  (interactive "sPay to: ")
  (let ((pay (hashcash-generate-payment (hashcash-payment-to arg)
					(hashcash-payment-required arg))))
    (when pay
      (insert-before-markers "X-Hashcash: " pay "\n"))))

;;;###autoload
(defun mail-add-payment (&optional arg)
  "Add an X-Hashcash: header with a hashcash payment for each recipient address
Prefix arg sets default payment temporarily."
  (interactive "P")
  (let ((hashcash-default-payment (if arg (prefix-numeric-value arg)
				    hashcash-default-payment))
	(addrlist nil))
    (save-excursion
      (save-restriction
	(goto-char (point-min))
	(search-forward mail-header-separator)
	(beginning-of-line)
	(narrow-to-region (point-min) (point))
	(let ((to (hashcash-strip-quoted-names (mail-fetch-field "To" nil t)))
	      (cc (hashcash-strip-quoted-names (mail-fetch-field "Cc" nil t)))
	      (ng (hashcash-strip-quoted-names 
		   (mail-fetch-field "Newsgroups" nil t))))
	  (when to
	    (setq addrlist (split-string to ",[ \t\n]*")))
	  (when cc
	    (setq addrlist (nconc addrlist (split-string cc ",[ \t\n]*"))))
	  (when ng
	    (setq addrlist (nconc addrlist (split-string ng ",[ \t\n]*")))))
	(when addrlist
	  (mapc #'hashcash-insert-payment addrlist)))))
  t)

(provide 'hashcash)

;;; hashcash.el ends here
