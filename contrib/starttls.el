;;; starttls.el --- STARTTLS support via wrapper around GNU TLS

;; Copyright (C) 2003 Free Software Foundation, Inc.

;; Author: Simon Josefsson <simon@josefsson.org>
;; Keywords: comm, tls, gnutls, ssl

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

;; This package implements a simple wrapper around the GNU TLS command
;; line application "gnutls-cli" to make Emacs support STARTTLS.
;;
;; Usage is similar to `open-network-stream', i.e.:
;;
;; (setq tmp (open-starttls-stream "test" (current-buffer) "cyrus.andrew.cmu.edu" 143))
;; #<process test<9>>
;; (process-send-string tmp ". starttls\n")
;; nil
;; (negotiate-starttls tmp)
;; "*** Starting TLS handshake
;; - Certificate type: X.509
;;  - Certificate info:
;;  # Certificate is valid since: Thu Jun 26 19:00:00 CEST 2003
;;  # Certificate expires: Sat Jun 26 19:00:00 CEST 2004
;;  # Certificate fingerprint: 8d 59 d6 e1 c9 91 dc 5a bb 38 47 8c ec 85 1b 99
;;  # Certificate serial number: 3e fb 52 ce
;;  # Certificate version: #3
;;  # Certificate public key algorithm: RSA
;;  #   Modulus: 1024 bits
;;  # CN=cyrus.andrew.cmu.edu,OU=Computing Services,O=Carnegie Mellon University,L=Pittsburgh,ST=Pennsylvania,C=US
;;  # Certificate Issuer's info:
;;  # CN=CMU CA mail 1,OU=Computing Services,O=Carnegie Mellon University,L=Pittsburgh,ST=Pennsylvania,C=US
;;
;; - Peer's certificate is NOT trusted
;; - Version: TLS 1.0
;; - Key Exchange: RSA
;; - Cipher: ARCFOUR 128
;; - MAC: SHA
;; - Compression: NULL
;; "
;; (process-send-string tmp ". capability\n")
;; nil
;; (process-send-string tmp ". logout\n")
;; nil
;;
;; Resolving 'cyrus.andrew.cmu.edu'...
;; Connecting to '128.2.10.174:143'...
;;
;; - Simple Client Mode:
;;
;; * OK mail-fe4.andrew.cmu.edu Cyrus IMAP4 Murder v2.1.15-077 server ready
;; . OK Begin TLS negotiation now
;; * CAPABILITY IMAP4 IMAP4rev1 ACL QUOTA LITERAL+ MAILBOX-REFERRALS NAMESPACE UIDPLUS ID NO_ATOMIC_RENAME UNSELECT CHILDREN MULTIAPPEND SORT THREAD=ORDEREDSUBJECT THREAD=REFERENCES AUTH=PLAIN AUTH=KERBEROS_V4 AUTH=GSSAPI AUTH=ANONYMOUS ANNOTATEMORE
;; . OK Completed
;; * BYE LOGOUT received
;; . OK Completed
;; *** Received corrupted data(-9) - server has terminated the connection abnormally
;;
;; Process test<9> finished

;;; Code:

(eval-and-compile
  (autoload 'format-spec "format-spec")
  (autoload 'format-spec-make "format-spec"))

(defgroup starttls nil
  "Negotiated Transport Layer Security (STARTTLS) parameters."
  :group 'comm)

(defcustom starttls-programs '("gnutls-cli -s -p %p %h"
			       "gnutls-cli -s -p %p %h --protocols ssl3")
  "List of strings containing commands to open STARTTLS stream to a host.
Each entry in the list is tried until a connection is successful.
%s is replaced with server hostname, %p with port to connect to.
The program should read input on stdin and write output to
stdout.  Also see `starttls-connect' and `starttls-success' for
what the program should output after initial connection and
successful negotiation respectively."
  :type '(repeat string)
  :group 'starttls)

(defcustom starttls-process-connection-type t
  "*Value for `process-connection-type' to use when starting STARTTLS process.
Note that setting this to nil likely does not work, as
`process-send-eof' used in `negotiate-starttls' behave
differently depending on this setting, and it closes the
sub-process if this variable is set to nil."
  :type 'boolean
  :group 'starttls)

(defcustom starttls-connect "- Simple Client Mode:\n\n"
  "*Regular expression indicating successful connection.
The default is what GNUTLS's \"gnutls-cli\" outputs."
  ;; cli.c:main() print this string when it is starting to run in the
  ;; application read/write phase.  If the logic, or the string
  ;; itself, is modified, this have to be updated.
  :type 'regexp
  :group 'starttls)

(defcustom starttls-failure "*** Handshake has failed"
  "*Regular expression indicating failed TLS handshake.
The default is what GNUTLS's \"gnutls-cli\" outputs."
  ;; cli.c:do_handshake() print this string on failure.  If the logic,
  ;; or the string itself, is modified, this have to be updated.
  :type 'regexp
  :group 'starttls)

(defcustom starttls-success "- Compression: "
  "*Regular expression indicating completed TLS handshakes.
The default is what GNUTLS's \"gnutls-cli\" outputs."
  ;; cli.c:do_handshake() calls, on success, common.c:print_info(),
  ;; that unconditionally print this string last.  If that logic, or
  ;; the string itself, is modified, this have to be updated.
  :type 'regexp
  :group 'starttls)

(defun negotiate-starttls (process)
  "Negotiate TLS on process opened by `open-starttls-stream'.
This should typically only be done once.  It typically return a
multi-line informational message with information about the
handshake, or NIL on failure."
  (let (buffer response old-max done-ok done-bad)
    (if (null (setq buffer (process-buffer process)))
	;; XXX how to remove/extract the TLS negotiation junk?
	(process-send-eof process)
      (with-current-buffer buffer
	(save-excursion
	  (goto-char (point-max))
	  (setq old-max (point))
	  ;; `process-send-eof' closes sub-process unless we force
	  ;; `process-connection-type' to non-nil.  A cleaner solution
	  ;; would be to use:
	  ;; (process-send-string process (string-as-unibyte (format "%c" 4)))
	  ;; or something, but I could not get that to work.
	  (process-send-eof process)
	  (while (and process
		      (memq (process-status process) '(open run))
		      (save-excursion
			(set-buffer buffer) ;; XXX "blue moon" nntp.el bug
			(goto-char old-max)
			(progn
			  (setq
			   done-ok (re-search-forward starttls-success nil t)
			   done-bad (re-search-forward starttls-failure nil t))
			  (not (or done-ok done-bad)))))
	    (accept-process-output process 1 100)
	    (sit-for 0.1))
	  (setq info (buffer-substring-no-properties old-max (point-max)))
	  (delete-region old-max (point-max))
	  (if (or (and done-ok (not done-bad))
		  ;; prevent mitm that fake success msg after failure msg.
		  (and done-ok done-bad (< done-ok done-bad)))
	      info
	    (message "STARTTLS negotiation failed: %s" info)
	    nil))))))

(defun open-starttls-stream (name buffer host service)
  "Open a TLS connection for a service to a host.
Returns a subprocess-object to represent the connection.
Input and output work as for subprocesses; `delete-process' closes it.
Args are NAME BUFFER HOST SERVICE.
NAME is name for process.  It is modified if necessary to make it unique.
BUFFER is the buffer (or buffer-name) to associate with the process.
 Process output goes at end of that buffer, unless you specify
 an output stream or filter function to handle the output.
Third arg is name of the host to connect to, or its IP address.
Fourth arg SERVICE is name of the service desired, or an integer
specifying a port number to connect to."
  (let ((cmds starttls-programs) cmd done old-max)
    (message "Opening STARTTLS connection to `%s'..." host)
    (with-current-buffer buffer
      (setq old-max (point-max)))
    (while (and (not done) (setq cmd (pop cmds)))
      (message "Opening STARTTLS connection with `%s'..." cmd)
      (let* ((process-connection-type starttls-process-connection-type)
	     (process (start-process
		       name buffer shell-file-name shell-command-switch
		       (format-spec
			cmd
			(format-spec-make
			 ?h host
			 ?p (if (integerp service)
				(int-to-string service)
			      service)))))
	     response)
	(while (and process
		    (memq (process-status process) '(open run))
		    (save-excursion
		      (set-buffer buffer) ;; XXX "blue moon" nntp.el bug
		      (goto-char (point-min))
		      (not (setq done (re-search-forward
				       starttls-connect nil t)))))
	  (accept-process-output process 0 100)
	  (sit-for 0.1))
	(message "Opening STARTTLS connection with `%s'...%s" cmd
		 (if done "done" "failed"))
	(if done
	    (progn
	      (with-current-buffer buffer
		(delete-region old-max done))
	      (setq done process))
	  (delete-process process))))
    (message "Opening STARTTLS connection to `%s'...%s"
	     host (if done "done" "failed"))
    done))

;; Compatibility with starttls.el by Daiki Ueno <ueno@unixuser.org>:
(defalias 'starttls-open-stream 'open-starttls-stream)
(defalias 'starttls-negotiate 'negotiate-starttls)

(provide 'starttls)

;;; starttls.el ends here
