;; Is this really the only way to set the load path? Seems awfully
;; kludgy to load this file and run this function just to do something
;; as simple as that... Anyways, it won't be in the production code,
;; so who cares?

(defun dgnushack () 
  (setq load-path (cons "." load-path)))
  

