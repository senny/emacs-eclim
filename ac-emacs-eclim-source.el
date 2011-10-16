;; ac-emacs-eclim-source.el --- an interface to the Eclipse IDE.
;;
;; Copyright (C) 2009   Fredrik Appelberg
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Contributors
;;
;;; Conventions
;;
;; Conventions used in this file: Name internal variables and functions
;; "eclim--<descriptive-name>", and name eclim command invocations
;; "eclim/command-name", like eclim/project-list.
;;; Description
;; 
;; ac-emacs-eclim-source.el -- a emacs eclime source for auto-complete-mode
;;

(require 'auto-complete)

(defun ac-emacs-eclim-candidates ()
  (with-no-warnings
    (loop for c in (eclim/java-complete)
          collect (nth 2 c))))

(defun ac-emacs-eclim-available ()
  (eclim--accepted-p (buffer-file-name)))

(defvar ac-emacs-eclim-point)

(defun ac-emacs-eclim-init ()
  (setq ac-emacs-eclim-point ac-point)
  (message "Completion started at %s, ac-point is %s" (point) ac-point))

(defun ac-emacs-eclim-yasnippet-convert (s)
  "Convert a string to a yasnippet template"
  (if (string-match "\\(.*\\)(\\(.*\\))" s)
      (format "%s(%s)$0"
	      (match-string 1 s) 
	      (reduce (lambda (a b) (concat a ", " b))
		      (loop for arg in (split-string (match-string 2 s) " *, *")
			    for i from 1
			    collect (format "${%d:%s}" i arg))))
    s))

(defun ac-emacs-eclim-action ()
    (let* ((end (point))
	   (candidate 
	    (replace-regexp-in-string " *[:-].*" "" 
				      (buffer-substring-no-properties ac-emacs-eclim-point end)))
	   (template (ac-emacs-eclim-yasnippet-convert candidate)))
      (delete-region ac-emacs-eclim-point end)
      (message template)
      (if (and eclim-use-yasnippet template)
	  (yas/expand-snippet template)
	(insert candidate))))

(ac-define-source emacs-eclim
  '((candidates . ac-emacs-eclim-candidates)
    (available . ac-emacs-eclim-available)
    (init . ac-emacs-eclim-init)
    (action . ac-emacs-eclim-action)
    (requires . 0)
    (symbol . "f")))

(add-hook 'java-mode-hook (lambda () (add-to-list 'ac-sources 'ac-source-emacs-eclim)))

(provide 'ac-emacs-eclim-source)