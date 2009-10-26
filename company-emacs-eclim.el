;; company-emacs-eclim.el -- a company-mode backend that replaces company-eclim
;;
;; To activate this backend, replace company-eclim with
;; company-emacs-eclim in the eclim-backends list, or call the
;; convenience function company-emacs-eclim-setup.

(require 'eclim)
(require 'eclim-java)
(require 'company)

(defvar cee--doc nil)
(make-variable-buffer-local 'cee--doc)

(defun company-emacs-eclim-setup ()
  "Convenience function that adds company-emacs-eclim to the list
  of available company backends."
  (flet ((replace-recur (elt rpl lst)
			(cond ((null lst) nil)
			      ((listp (first lst)) (cons (replace-recur elt rpl (first lst))
							 (replace-recur elt rpl (rest lst))))
			      (t (cons (if (equal (first lst) elt) rpl (first lst))
				       (replace-recur elt rpl (rest lst)))))))
    (let ((replaced (replace-recur 'company-eclim 'company-emacs-eclim company-backends)))
      (setq company-backends
	    (if (eq replaced company-backends)
		(cons 'company-emacs-eclim company-backends)
	      replaced)))))

(defun cee--correct-completions (candidates)
  "If we are lookup at a list of method call completions, check
  if we have already typed part of this call."
  (if (every (lambda (c) (string= "f" (eclim--completion-candidate-type c))) candidates)
      ;; When completing a method call that have alread been completed
      ;; up to the 'method(' point, eclim still reports the
      ;; completions as 'method(arg1, arg2, ...)', which is not what
      ;; company-mode expects. 
      (let ((common (try-completion "" (mapcar 'eclim--completion-candidate-doc candidates))))
	(save-excursion
	  (if (search-backward common (- (point) (length common)) t)
	      (mapcar (lambda (c)
			(list 
			 (eclim--completion-candidate-type c)
			 (eclim--completion-candidate-class c)
			 (substring (eclim--completion-candidate-doc c) (length common))))
		      candidates)
	    candidates)))
    candidates))

(defun cee--candidates (prefix)
  (interactive "d")
  (let ((project-file (eclim--project-current-file))
        (project-name (eclim--project-name)))
    (eclim--java-src-update)
    (setq cee--doc  (cee--correct-completions (eclim/java-complete))))
  (let ((completion-ignore-case nil))
    (all-completions prefix (mapcar 'eclim--completion-candidate-doc cee--doc))))

(defun cee--find-candidate (lookup)
  (find lookup cee--doc
   	:key #'eclim--completion-candidate-doc
   	:test #'string=))

(defun company-emacs-eclim (command &optional arg &rest ignored)
  "A `company-mode' back-end for eclim completion"
  (interactive)
  (case command
    ('prefix (and (derived-mode-p 'java-mode 'jde-mode)
                  buffer-file-name
                  eclim-executable
                  (eclim--project-name)
                  (not (company-in-string-or-comment))
                  (or (company-grab-symbol) 'stop)))
    ('candidates (cee--candidates arg))
    ('meta (eclim--completion-candidate-doc (cee--find-candidate arg)))
    ('no-cache (equal arg ""))))

(defun cee--delete-backward (delim)
  (let ((end (point))
	(start (search-backward delim (- company-point 1) t)))
    (when start
      (delete-region start end))))

(defun cee--generic-args (candidate)
  "If the doc string for this CANDIDATE is a generic arg list,
  return a list of the arguments, otherwise return nil."
  nil)

(defun cee--method-call (candidate)
  "If the doc string for this CANDIDATE is a method call argument
  list, return a list of lists representing the type and
  name of each argument."
  (let ((doc (eclim--completion-candidate-doc candidate)))
    (if (or (string-match "\\(.*\\)(\\(.*\\))" doc)
	    (string-match "\\(\\)\\(.*\\))" doc))
	(mapcar (lambda (e) (split-string e " ")) 
		(split-string (match-string 2 doc) ", " t)))))

(defun join-list (lst glue)
  "Utility function; returns a list based on LST with GLUE
inserted between each element."
  (cond ((null lst) nil)
	((null (rest lst)) lst)
	(t
	 (cons (first lst)
	       (cons glue (join-list (rest lst) glue))))))

;; TODO: handle Generic args (List<E, ..>)
;; TODO: handle override/implementation of methods
(defun cee--completion-finished (arg)
  "Post-completion hook after running company-mode completions."
  (let* ((candidate (cee--find-candidate arg))
	 (type (eclim--completion-candidate-type candidate)))
    (when candidate
      (if (string= "c" type)
	  (progn
	    ;; If this is a class, then remove the doc string and insert an import statement
	    (cee--delete-backward " - ")
	    (eclim--java-organize-imports
	     (eclim/java-import-order (eclim--project-name)) 
	     (list 
	      (concat (eclim--completion-candidate-package candidate) "." 
		      (eclim--completion-candidate-class candidate)))))
	;; Otherwise, check if this is a method call
	(if (string= "f" type)
	    (let ((call-args (cee--method-call candidate)))
	      (cee--delete-backward "(")
	      (yas/expand-snippet (point) (point)
				  (apply 'concat 
					 (append (list "(")
						 (join-list
						  (loop for arg in call-args
							for i from 1
							collect (concat "${" (int-to-string i) ":" (first arg) " "(second arg) "}"))
						  ", ")
						 (list ")")))))
	  ;; Otherwise, just delete the doc string
	  (cee--delete-backward " : "))))))

(add-hook 'company-completion-finished-hook
	  'cee--completion-finished)

(provide 'company-emacs-eclim)
