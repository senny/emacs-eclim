;; company-emacs-eclim.el -- a company-mode backend that replaces company-eclim
;;
;;

(require 'eclim-java)
(require 'company)

(defvar company-emacs-eclim--doc nil)
(make-variable-buffer-local 'company-emacs-eclim--doc)

(defun company-emacs-eclim--candidates (prefix)
  (interactive "d")
  (let ((project-file (eclim--project-current-file))
        (project-name (eclim--project-name)))
    (eclim--java-src-update)
    (setq company-emacs-eclim--doc  (eclim/java-complete)))
  (let ((completion-ignore-case nil))
    (all-completions prefix (mapcar 'eclim--completion-candidate-class company-emacs-eclim--doc))))

(defun company-emacs-eclim--find-candidate (class)
  (nth company-selection company-emacs-eclim--doc))

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

(defun company-emacs-eclim--completion-finished (arg)
  "Post-completion hook after running company-mode completions."
  (let ((candidate (company-emacs-eclim--find-candidate arg)))
    (if (string= "c" (eclim--completion-candidate-type candidate))
	;; If this is a class, then insert an import statement
	(eclim--java-organize-imports
	 (eclim/java-import-order (eclim--project-name)) 
	 (list 
	  (concat (eclim--completion-candidate-package candidate) "." 
		  (eclim--completion-candidate-class candidate)))))))

(add-hook 'company-completion-finished-hook
	  'company-emacs-eclim--completion-finished)

(provide 'company-emacs-eclim)
