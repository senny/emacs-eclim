;; company-emacs-eclim.el -- a company-mode backend that replaces company-eclim
;;
;; To activate this backend, replace company-eclim with
;; company-emacs-eclim in the eclim-backends list, or call the
;; convenience function company-emacs-eclim-setup.

(require 'eclim)
(require 'eclim-java)
(require 'company)

(defvar company-emacs-eclim--doc nil)
(make-variable-buffer-local 'company-emacs-eclim--doc)

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

(defun company-emacs-eclim--candidates (prefix)
  (interactive "d")
  (let ((project-file (eclim--project-current-file))
        (project-name (eclim--project-name)))
    (eclim--java-src-update)
    (setq company-emacs-eclim--doc  (eclim/java-complete)))
  (let ((completion-ignore-case nil))
    (all-completions prefix (mapcar 'eclim--completion-candidate-doc company-emacs-eclim--doc))))

(defun company-emacs-eclim--find-candidate (lookup)
  (find lookup company-emacs-eclim--doc
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
    ('candidates (company-emacs-eclim--candidates arg))
    ('meta (eclim--completion-candidate-doc (company-emacs-eclim--find-candidate arg)))
    ('no-cache (equal arg ""))))

(defun company-emacs-eclim--delete-backward (delim)
  (let ((end (point))
	(start (search-backward delim company-point t)))
    (when start
      (delete-region start end))))

(defun company-emacs-eclim--generic-args (candidate)
  "If the doc string for this CANDIDATE is a generic arg list,
  return a list of the arguments, otherwise return nil."
  nil)

(defun company-emacs-eclim--method-call (candidate)
  "If the doc string for this CANDIDATE is a method call argument
  list, return a list of lists representing the type and
  name of each argument."
  (let ((doc (eclim--completion-candidate-doc candidate)))
    (if (string-match "\\(.*\\)(\\(.*\\))" doc)
	(mapcar (lambda (e) (split-string e " ")) 
		(split-string (match-string 2 doc) ", " t)))))

(defun join-list (lst glue)
  (cond ((null lst) nil)
	((null (rest lst)) lst)
	(t
	 (cons (first lst)
	       (cons glue (join-list (rest lst) glue))))))

;; TODO: hantera Generic args (List<E, ..>)
;; TODO: hantera funktionsanrop (fun(Type arg1, Type arg2, ..))
;; TODO: hantera override/implementation av metoder
(defun company-emacs-eclim--completion-finished (arg)
  "Post-completion hook after running company-mode completions."
  (let ((candidate (company-emacs-eclim--find-candidate arg)))
    (when candidate
      (if (string= "c" (eclim--completion-candidate-type candidate))
	  (progn
	    ;; If this is a class, then remove the doc string and insert an import statement
	    (company-emacs-eclim--delete-backward " - ")
	    (eclim--java-organize-imports
	     (eclim/java-import-order (eclim--project-name)) 
	     (list 
	      (concat (eclim--completion-candidate-package candidate) "." 
		      (eclim--completion-candidate-class candidate)))))
	;; Otherwise, check if this is a method call
	(let ((call-args (company-emacs-eclim--method-call candidate)))
	  (if call-args
	      (progn
		(company-emacs-eclim--delete-backward "(")
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
	    (company-emacs-eclim--delete-backward " : ")))))))

(add-hook 'company-completion-finished-hook
	  'company-emacs-eclim--completion-finished)

(provide 'company-emacs-eclim)
