;;; gnus-picons.el:  Icon hacks for displaying pretty icons in Gnus.
;; Copyright (C) 1996 Wes Hardaker

;; Author:  Wes hardaker <hardaker@ece.ucdavis.edu>
;; Keywords:  gnus xpm annotation glyph faces

;;; Commentary:

;; Usage:
;;     - You must have XEmacs (19.12 or above I think) to use this.
;;     - Read the variable descriptions below.
;;
;;     - chose a setup:
;;
;;       1) display the icons in its own buffer:
;;
;;          (add-hook 'gnus-article-display-hook 'gnus-article-display-picons t)
;;          (add-hook 'gnus-summary-display-hook 'gnus-group-display-picons t)
;;          (setq gnus-picons-display-where 'picons)
;;
;;          Then add the picons buffer to your display configuration:
;;          The picons buffer needs to be at least 48 pixels high,
;;          which for me is 5 lines:
;;
;;          (gnus-add-configuration
;;           '(article (vertical 1.0 
;;                             (group 6)
;;                             (picons 5)
;;                             (summary .25 point)
;;                             (article 1.0))))
;;
;;          (gnus-add-configuration
;;           '(summary (vertical 1.0 (group 6)
;;                      (picons 5)
;;                      (summary 1.0 point))))
;;
;;       2) display the icons in the summary buffer
;;
;;          (add-hook 'gnus-article-display-hook 'gnus-article-display-picons t)
;;          (add-hook 'gnus-summary-display-hook 'gnus-group-display-picons t)
;;          (setq gnus-picons-display-where 'summary)
;;
;;       3) display the icons in the article buffer
;;
;;          (add-hook 'gnus-article-display-hook 'gnus-article-display-picons t)
;;          (add-hook 'gnus-article-display-hook 'gnus-group-display-picons t)
;;          (setq gnus-picons-display-where 'article)
;;
;;
;; Warnings:
;;     - I'm not even close to being a lisp expert.
;;     - The 't' (append) flag MUST be in the add-hook line
;;
;; TODO:
;;     - Remove the TODO section in the headers.
;;

;;; Code:

(require 'xpm)
(require 'annotations)
(eval-when-compile (require 'cl))

(defvar gnus-picons-buffer "*Icon Buffer*"
  "Buffer name to display the icons in if gnus-picons-display-where is 'picons.")

(defvar gnus-picons-display-where 'picons
  "Where to display the group and article icons.")

(defvar gnus-picons-database "/usr/local/faces"
  "Defines the location of the faces database.  
For information on obtaining this database of pretty pictures, please
see http://www.cs.indiana.edu/picons/ftp/index.html" )

(defvar gnus-picons-news-directory "news"
  "Sub-directory of the faces database containing the icons for newsgroups."
)

(defvar gnus-picons-user-directories '("local" "users" "usenix" "misc/MISC")
  "List of directories to search for user faces."
)

(defvar gnus-picons-domain-directories '("domains")
  "List of directories to search for domain faces.  
Some people may want to add \"unknown\" to this list."
)

(defvar gnus-group-annotations nil)
(defvar gnus-article-annotations nil)
(defvar gnus-x-face-annotations nil)

(defun gnus-picons-remove (plist)
  (let ((listitem (car plist)))
    (while (setq listitem (car plist))
      (if (annotationp listitem)
          (delete-annotation listitem))
      (setq plist (cdr plist))))
)

(defun gnus-picons-remove-all ()
  "Removes all picons from the Gnus display(s)."
  (interactive)
  (gnus-picons-remove gnus-article-annotations)
  (gnus-picons-remove gnus-group-annotations)
  (setq gnus-article-annotations nil
        gnus-group-annotations nil)
  (if (bufferp gnus-picons-buffer)
      (kill-buffer gnus-picons-buffer))
)

(defun gnus-get-buffer-name (variable)
  "Returns the buffer name associated with the contents of a variable."
  (cond ((symbolp variable)
         (let ((newvar (cdr (assq variable gnus-window-to-buffer))))
           (cond ((symbolp newvar)
                  (symbol-value newvar))
                 ((stringp newvar) newvar))))
        ((stringp variable)
         variable)))

(defvar gnus-picons-x-face-file-name 
  (format "/tmp/picon-xface.%s.xbm" (user-login-name))
  "The name of the file in which to store the converted X-face header.")

(defvar gnus-picons-convert-x-face (format "{ echo '/* Width=48, Height=48 */'; uncompface; } | icontopbm | pbmtoxbm > %s" gnus-picons-x-face-file-name)
  "Command to convert the x-face header into a xbm file."
)
       
(defun gnus-picons-article-display-x-face ()
  "Display the x-face header bitmap in the 'gnus-picons-display-where buffer."
  ;; delete any old ones.
  (gnus-picons-remove gnus-x-face-annotations)
  (setq gnus-x-face-annotations nil)
  ;; display the new one.
  (let ((gnus-article-x-face-command 'gnus-picons-display-x-face))
    (gnus-article-display-x-face)))

(defun gnus-picons-display-x-face (beg end)
  "Function to display the x-face header in the picons window.
To use:  (setq gnus-article-x-face-command 'gnus-picons-display-x-face)"
  (interactive)
  ;; convert the x-face header to a .xbm file
  (let ((process-connection-type nil)
      (process nil))
    (process-kill-without-query
     (setq process (start-process
      "gnus-x-face" nil "sh" "-c" gnus-picons-convert-x-face)))
    (process-send-region "gnus-x-face" beg end)
    (process-send-eof "gnus-x-face")
  ;; wait for it.
    (while (not (equal (process-status process) 'exit))
      (sleep-for .1)))
  ;; display it
  (save-excursion
    (set-buffer (gnus-get-buffer-name gnus-picons-display-where))
    (gnus-add-current-to-buffer-list)
    (beginning-of-buffer)
    (let ((iconpoint (point)))
      (if (not (looking-at "^$"))
        (if buffer-read-only
            (progn 
              (toggle-read-only)
              (open-line 1)
              (toggle-read-only)
              )
          (open-line 1)))
      (end-of-line)
      ;; append the annotation to gnus-article-annotations for deletion.
      (setq gnus-x-face-annotations 
          (append
           (gnus-picons-try-to-find-face
	    gnus-picons-x-face-file-name iconpoint)
           gnus-x-face-annotations)))
    ;; delete the tmp file
    (delete-file gnus-picons-x-face-file-name)))

(defun gnus-article-display-picons ()
"Display faces for an author and his/her domain in gnus-picons-display-where."
  (interactive)
  (if (and (featurep 'xpm) 
           (or (not (fboundp 'device-type)) (equal (device-type) 'x)))
      (save-excursion
        (let* ((iconpoint (point)) (from (mail-fetch-field "from"))
          (username 
           (progn
             (string-match "\\([-_a-zA-Z0-9]+\\)@" from)
             (match-string 1 from)))
           (hostpath
            (concat (gnus-picons-reverse-domain-path
                     (replace-in-string
                      (replace-in-string from ".*@\\([_a-zA-Z0-9-.]+\\).*" 
                                         "\\1") 
                      "\\." "/")) "/")))
          (switch-to-buffer (gnus-get-buffer-name gnus-picons-display-where))
          (gnus-add-current-to-buffer-list)
          (beginning-of-buffer)
          (setq iconpoint (point))
          (if (not (looking-at "^$"))
              (if buffer-read-only
                  (progn 
                    (toggle-read-only)
                    (open-line 1)
                    (toggle-read-only)
                    )
                (open-line 1)))

          (end-of-line)
          (gnus-picons-remove gnus-article-annotations)
          (setq gnus-article-annotations 'nil)
          (if (equal username from)
                (setq username (progn
                                 (string-match "<\\([_a-zA-Z0-9-.]+\\)>" from)
                                 (match-string 1 from))))
          (mapcar '(lambda (pathpart) 
                     (setq gnus-article-annotations
                           (append
                                   (gnus-picons-insert-face-if-exists 
                                    (concat 
                                     (file-name-as-directory 
                                      gnus-picons-database) pathpart)
                                    (concat hostpath username) 
                                    iconpoint)
                                    gnus-article-annotations))) 
                  gnus-picons-user-directories)
          (mapcar '(lambda (pathpart) 
                     (setq gnus-article-annotations 
                           (append
                                   (gnus-picons-insert-face-if-exists 
                                    (concat (file-name-as-directory 
                                             gnus-picons-database) pathpart)
                                    (concat hostpath "unknown") 
                                    iconpoint)
                                    gnus-article-annotations))) 
                           gnus-picons-domain-directories)
          (add-hook 'gnus-summary-exit-hook 'gnus-picons-remove-all)
          ))))

(defun gnus-group-display-picons ()
  "Display icons for the group in the gnus-picons-display-where buffer." 
  (interactive)
  (if (and (featurep 'xpm) 
           (or (not (fboundp 'device-type)) (equal (device-type) 'x)))
      (save-excursion
      (let
          ((iconpoint (point)))
        (switch-to-buffer (gnus-get-buffer-name gnus-picons-display-where))
        (gnus-add-current-to-buffer-list)
        (beginning-of-buffer)
        (cond 
         ((listp gnus-group-annotations)
          (mapcar 'delete-annotation gnus-group-annotations)
          (setq gnus-group-annotations nil))
         ((annotationp gnus-group-annotations)
          (delete-annotation gnus-group-annotations)
          (setq gnus-group-annotations nil))
         )
        (setq iconpoint (point))
        (if (not (looking-at "^$"))
            (open-line 1))
        (gnus-picons-remove gnus-group-annotations)
        (setq gnus-group-annotations nil)
        (setq gnus-group-annotations
              (gnus-picons-insert-face-if-exists 
               (concat (file-name-as-directory gnus-picons-database)  
                       gnus-picons-news-directory)
               (concat (replace-in-string gnus-newsgroup-name "\\." "/") 
                       "/unknown")
               iconpoint t))
        (add-hook 'gnus-summary-exit-hook 'gnus-picons-remove-all)))))


(defun gnus-picons-insert-face-if-exists (path filename ipoint &optional rev)
  "Inserts a face at point if I can find one"
  (let ((pathfile (concat path "/" filename "/face"))
        (newfilename 
         (replace-in-string filename 
                            "[_a-zA-Z0-9-]+/\\([_A-Za-z0-9-]+\\)$" "\\1"))
        (annotations nil))
    (if (and rev
         (not (equal filename newfilename)))
        (setq annotations (append
              (gnus-picons-insert-face-if-exists path newfilename ipoint rev)
               annotations)))
    (if (eq (length annotations) (length (setq annotations (append
          (gnus-picons-try-to-find-face (concat pathfile ".xpm") ipoint)
           annotations))))
        (setq annotations (append
                             (gnus-picons-try-to-find-face 
                              (concat pathfile ".xbm") ipoint)
                              annotations)))
    (if (and (not rev)
         (not (equal filename newfilename)))
        (setq annotations (append
              (gnus-picons-insert-face-if-exists path newfilename ipoint rev)
               annotations)))
    annotations
    )
  )
  
(defun gnus-picons-try-to-find-face (path ipoint)
  "If PATH exists, display it as a bitmap.  Returns t if succedded."
  (when (file-exists-p path)
    (let ((gl (make-glyph path)))
      (set-glyph-face gl 'default)
      (list (make-annotation gl ipoint 'text)))))

(defun gnus-picons-reverse-domain-path (str)
  "a/b/c/d -> d/c/b/a"
  (if (equal (replace-in-string str "^[^/]*$" "") "")
      str
    (concat (replace-in-string str "^.*/\\([_a-zA-Z0-9-]+\\)$" "\\1") "/"
            (gnus-picons-reverse-domain-path 
             (replace-in-string str "^\\(.*\\)/[_a-zA-Z0-9-]+$" "\\1")))))

(provide 'gnus-picon)

;;; gnus-picon.el ends here
