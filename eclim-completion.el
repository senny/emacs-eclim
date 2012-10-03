;; company-emacs-eclim.el --- an interface to the Eclipse IDE.
;;
;; Copyright (C) 2012   Fredrik Appelberg
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
;; company-emacs-eclim.el -- completion functions used by the company-mode
;;    and auto-complete-mode backends.
;;

(defun eclim--completion-candidate-type (candidate)
  "Returns the type of a candidate."
  (assoc-default 'type candidate))

(defun eclim--completion-candidate-class (candidate)
  "Returns the class name of a candidate."
  (assoc-default 'info candidate))

(defun eclim--completion-candidate-doc (candidate)
  "Returns the documentation for a candidate."
  (assoc-default 'menu candidate))

(defun eclim--completion-candidate-package (candidate)
  "Returns the package name of a candidate."
  (let ((doc (eclim--completion-candidate-doc candidate)))
    (when (string-match "\\(.*\\)\s-\s\\(.*\\)" doc)
      (match-string 2 doc))))

(defun eclim--completion-candidates ()
  (with-no-warnings
    (mapcar (lambda (c) (assoc-default 'info c))
            (assoc-default 'completions (eclim/java-complete)))))

(defun eclim--completion-yasnippet-convert (completion)
  "Convert a completion string to a yasnippet template"
  (apply #' concat
            (loop for c across (replace-regexp-in-string ", " "," completion)
                  collect (case c
                              (40 "(${")
                              (60 "<${")
                              (44 "}, ${")
                              (41 "})")
                              (62 "}>")
                              (t (char-to-string c))))))

(defvar eclim--completion-start)

(defun eclim--completion-action ()
  (let* ((end (point))
         (completion (buffer-substring-no-properties eclim--completion-start end)))
    (if (string-match "\\([^-:]+\\) .*?\\(- *\\(.*\\)\\)?" completion)
        (let* ((insertion (match-string 1 completion))
               (rest (match-string 3 completion))
               (package (if (and rest (string-match "\\w+\\(\\.\\w+\\)*" rest)) rest nil))
               (template (eclim--completion-yasnippet-convert insertion)))
          (delete-region eclim--completion-start end)
          (if (and eclim-use-yasnippet template (featurep 'yasnippet))
              (yas/expand-snippet template)
            (insert insertion))
          (when package
            (eclim--java-organize-imports
             (eclim/execute-command "java_import_order" "-p")
             (list (concat package "." (substring insertion 0 (or (string-match "[<(]" insertion)
                                                                  (length insertion)))))))))))

(provide 'eclim-completion)