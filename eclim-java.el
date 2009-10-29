;; eclim-java.el --- an interface to the Eclipse IDE.
;;
;; Copyright (C) 2009  Yves Senn <yves senn * gmx ch>
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
;; - Tassilo Horn <tassilo@member.fsf.org>
;;
;;; Conventions
;;
;; Conventions used in this file: Name internal variables and functions
;; "eclim--<descriptive-name>", and name eclim command invocations
;; "eclim/command-name", like eclim/project-list.

;;* Eclim Java

(require 'json)
(require 'decompile)

(define-key eclim-mode-map (kbd "C-c C-e s") 'eclim-java-method-signature-at-point)
(define-key eclim-mode-map (kbd "C-c C-e d") 'eclim-javadoc-insert-at-point)
(define-key eclim-mode-map (kbd "C-c C-e f d") 'eclim-java-find-declaration)
(define-key eclim-mode-map (kbd "C-c C-e f r") 'eclim-java-find-references)
(define-key eclim-mode-map (kbd "C-c C-e i") 'eclim-java-import-missing)
(define-key eclim-mode-map (kbd "C-c C-e h") 'eclim-java-hierarchy)
;; TODO: find better binding for implement
(define-key eclim-mode-map (kbd "C-c C-e z") 'eclim-java-implement)

(defgroup eclim-java nil
  "Java: editing, browsing, refactoring"
  :group 'eclim)

(defcustom eclim-java-field-prefixes "\\(s_\\|m_\\)\\(.*\\)"
  "this variable contains a regular expression matching the java field
  prefixes. The prefixes get removed when using yasnippet to generate
  getter and setter methods. This variable allows you to have field
  names lik 'm_username' and get method names like 'setUsername' and 'getUsername'"
  :group 'eclim-java
  :type 'regexp)

(defvar eclim--java-search-types '("all"
                                   "annotation"
                                   "class"
                                   "classOrEnum"
                                   "classOrInterface"
                                   "constructor"
                                   "enum"
                                   "field"
                                   "interface"
                                   "method"
                                   "package"
                                   "type"))

(defvar eclim--java-search-scopes '("all"
                                    "project"
                                    "type"))

(defvar eclim--java-search-contexts '("all"
                                      "declarations"
                                      "implementors"
                                      "references"))

(defun eclim/java-complete ()
  (mapcar (lambda (line)
            (split-string line "|" nil))
          (eclim--call-process "java_complete"
                               "-p" (eclim--project-name)
                               "-f" (eclim--project-current-file)
                               "-e" "iso-8859-1"
                               "-l" "standard"
                               "-o" (number-to-string (eclim--byte-offset)))))


(defun eclim--java-current-class-name ()
  "Searches backward in the current buffer until a class declaration
has been found."
  (save-excursion
    (re-search-backward "class[ ]+\\([a-zA-Z]+\\) ")
    (match-string 1)))

(defun eclim--java-symbol-remove-prefix (name)
  (if (string-match eclim-java-field-prefixes name)
      (match-string 2 name)
    name))

(defun eclim--completion-candidate-type (candidate)
  "Returns the type of a candidate."
  (first candidate))

(defun eclim--completion-candidate-class (candidate)
  "Returns the class name of a candidate."
  (second candidate))

(defun eclim--completion-candidate-doc (candidate)
  "Returns the documentation for a candidate."
  (third candidate))

(defun eclim--completion-candidate-package (candidate)
  "Returns the package name of a candidate."
  (let ((doc (eclim--completion-candidate-doc candidate)))
    (when (string-match "\\(.*\\)\s-\s\\(.*\\)" doc)
      (match-string 2 doc))))

(defun eclim/java-classpath (project)
  (eclim--check-project project)
  (eclim--call-process "java_classpath" "-p" project))

(defun eclim/java-classpath-variables ()
  ;; TODO: fix trailing whitespaces
  (mapcar (lambda (line)
            (split-string line "-")) (eclim--call-process "java_classpath_variables")))

(defun eclim/java-classpath-variable-create (name path)
  (eclim--call-process "java_classpath_variable_create" "-n" name "-p" path))

(defun eclim/java-classpath-variable-delete (name)
  (eclim--call-process "java_classpath_variable_create" "-n" name))

(defun eclim/java-import (project pattern)
  (eclim--check-project project)
  (eclim--call-process "java_import"
                       "-n" project
                       "-p" pattern))

(defun eclim/java-import-order (project)
  (eclim--check-project project)
  (eclim--call-process "java_import_order"
                       "-p" project))

(defun eclim/java-import-missing (project)
  (eclim--check-project project)
  (eclim--call-process "java_import_missing"
                       "-p" project
                       "-f" (eclim--project-current-file)))

(defun eclim/java-import-unused (project)
  (eclim--check-project project)
  (eclim--call-process "java_imports_unused"
                       "-p" project
                       "-f" (eclim--project-current-file)))

(defun eclim/javadoc-comment (project file offset)
  (eclim--call-process "javadoc_comment" "-p" project "-f" file "-o" offset))

(defun eclim/java-hierarchy (project file offset encoding)
  (eclim--call-process "java_hierarchy"
                       "-p" project
                       "-f" file
                       "-o" (number-to-string offset)
                       "-e" encoding))

(defun eclim/java-search (&optional project file offset length pattern type context scope case-insensitive encoding)
  (mapcar (lambda (line)
            (split-string line "|" nil))
          (apply 'eclim--call-process (eclim--build-command "java_search"
                                                            "-n" project
                                                            "-f" file
                                                            "-o" offset
                                                            "-l" length
                                                            "-p" pattern
                                                            "-t" type
                                                            "-x" context
                                                            "-s" scope
                                                            "-i" case-insensitive))))

(defun eclim-java-hierarchy (project file offset encoding)
  (interactive (list (eclim--project-name)
                     (eclim--project-current-file)
                     (eclim--byte-offset)
                     (eclim--current-encoding)))
  (pop-to-buffer "*eclim: hierarchy*" t)
  (special-mode)
  (let ((buffer-read-only nil))
    (erase-buffer)
    (eclim--java-insert-hierarchy-node
     project
     (json-read-from-string
      (replace-regexp-in-string "'" "\""
                                (car (eclim/java-hierarchy project file offset encoding))))
     0)))

(defun eclim--java-insert-hierarchy-node (project node level)
  (let ((declaration (cdr (assoc 'name node)))
        (qualified-name (cdr (assoc 'qualified node))))
    (insert (format (concat "%-"(number-to-string (* level 2)) "s=> ") ""))
    (lexical-let ((file-path (first (first (eclim--java-find-declaration
                                            qualified-name)))))
      (insert-text-button declaration
                          'follow-link t
                          'help-echo qualified-name
                          'action (lambda (&rest ignore)
                                    (eclim--find-file file-path)))))
  (newline)
  (let ((children (cdr (assoc 'children node))))
    (loop for child across children do
          (eclim--java-insert-hierarchy-node project child (+ level 1)))))

(defun eclim--java-find-references (pattern)
  (eclim/java-search
   nil
   nil
   nil
   nil
   pattern
   "method"
   "references"
   "project"))

(defun eclim--java-find-declaration (pattern  &optional type)
  (eclim/java-search
   nil
   nil
   nil
   nil
   pattern
   type
   "declaration"
   "project"))

(defun eclim-java-find-declaration (pattern)
  (interactive (list (symbol-name (symbol-at-point))))
  (let ((search-result (eclim--java-find-declaration pattern)))
    ;; TODO: display multiple results in a grep like buffer
    (if (string= (caar search-result) "") (message "no declaration found")
      (if (= (length search-result) 1)
          (eclim--visit-declaration (car search-result))
        (message "TODO: currently not handling multiple results")))))

(defun eclim--java-convert-signature-to-pattern (signature)
  (replace-regexp-in-string "#" "."
                            (progn (string-match "^.*\\.\\(.*?\\)(.*$"
                                                 signature)
                                   (match-string 1 signature))))

(defun eclim-java-find-references (pattern)
  (interactive (list (eclim--java-convert-signature-to-pattern
                      (eclim--java-method-signature-at-point))))
  (eclim--find-display-results (eclim--project-dir)
                               pattern
                               (eclim--java-find-references pattern)))

(defun eclim-java-find (scope context type pattern)
  (interactive (list (eclim--completing-read "Scope: " eclim--java-search-scopes)
                     (eclim--completing-read "Context: " eclim--java-search-contexts)
                     (eclim--completing-read "Type: " eclim--java-search-types)
                     (read-string "Pattern: ")))
  (eclim--find-display-results (eclim--project-dir) pattern
                               (eclim/java-search
                                nil nil nil nil
                                pattern
                                type
                                context
                                scope)))

(defun eclim--java-method-signature-at-point ()
  ;; TODO: this does currently not work everywhere and needs some more love
  (let ((java-symbol (symbol-name (symbol-at-point))))
    (save-excursion
      (re-search-backward "[. ]" nil t)
      (let ((pattern (if (string= (char-to-string (char-after (point))) " ")
                         (concat (eclim--java-current-class-name) "." java-symbol)
                       java-symbol)))
        (forward-char 1)
        (third (car (eclim--java-find-declaration pattern "method")))))))

(defun eclim-java-method-signature-at-point ()
  (interactive)
  ;; TODO: make this work when the cursor is in the argument list
  (let* ((signature (eclim--java-method-signature-at-point))
         (message-log-max nil))
    (message signature)))

(defun eclim-javadoc-insert-at-point ()
  (interactive)
  (message (eclim/javadoc-comment (eclim--project-name) (eclim--project-current-file)
                                  (number-to-string (eclim--byte-offset)))))

(defun eclim--java-identifier-at-point ()
  "Returns a cons cell (BEG . START) where BEG is the start
buffer position of the token/identifier at point, and START is
the string from BEG to (point)."
  ;; TODO: make this work for dos buffers
  (let ((beg (+ 1 (or (save-excursion (re-search-backward "[.,-/+( ]" nil t)) 0))))
    (cons beg (buffer-substring-no-properties beg (point)))))

(defun eclim--java-package-components (package)
  "Returns the components of a Java package statement."
  (split-string package "\\."))

(defun eclim--java-wildcard-includes-p (wildcard package)
  "Returns true if PACKAGE is included in the WILDCARD import statement."
  (if (not (string-endswith-p wildcard ".*")) nil
    (equal (butlast (eclim--java-package-components wildcard))
           (butlast (eclim--java-package-components package)))))

(defun eclim--java-ignore-import-p (import)
  "Return true if this IMPORT should be ignored by the import
  functions."
  (string-match "^java\.lang\.[A-Z][^\.]*$" import))

(defun eclim--java-sort-imports (imports imports-order)
  "Sorts a list of imports according to a given sort order, removing duplicates."
  (flet ((sort-imports (imports-order imports result)
                       (cond ((null imports) result)
                             ((null imports-order)
                              (sort-imports nil nil (append result imports)))
                             (t
                              (flet ((matches-prefix (x) (string-startswith-p x (first imports-order))))
                                (sort-imports (rest imports-order)
                                              (remove-if #'matches-prefix imports)
                                              (append result (remove-if-not #'matches-prefix imports)))))))
         (remove-duplicates (imports result)
                            (if (null imports) result
                              (let ((f (first imports))
                                    (n (second imports)))
                                (if (null n) (cons f result)
                                  (if (or (eclim--java-wildcard-includes-p f n)
                                          (equal f n))
                                      (remove-duplicates (cons f (rest (rest imports))) result)
                                    (remove-duplicates (rest imports) (cons f result))))))))
    (reverse
     (remove-duplicates
      (sort-imports imports-order (sort imports #'string-lessp) '()) '()))))

(defun eclim--java-extract-imports ()
  "Extracts (by removing) import statements of a java
file. Returns a list of the extracted imports. Tries to leave the
cursor at a suitable point for re-inserting new import statements."
  (let ((imports '()))
    (goto-char 0)
    (while (search-forward-regexp "^\s*import \\(.*\\);" nil t)
      (push (match-string-no-properties 1) imports)
      (beginning-of-line)
      (kill-line))
    (delete-blank-lines)
    (if (null imports)
        (progn
          (end-of-line)
          (newline)
          (newline)))
    imports))

(defun eclim--java-organize-imports (imports-order &optional additional-imports unused-imports)
  "Organize the import statements in the current file according
  to IMPORTS-ORDER. If the optional parameter ADDITIONAL-IMPORTS
  is supplied, these import statements will be added to the
  rest. Imports listed in the optional parameter UNUSED-IMPORTS
  will be removed."
  (save-excursion
    (flet ((write-imports (imports last-import-first-part)
                          (when imports
                            (let ((first-part (first (eclim--java-package-components (first imports)))))
                              (if (not (equal last-import-first-part first-part))
                                  (newline))
                              (insert (format "import %s;\n" (first imports)))
                              (write-imports (rest imports) first-part)))))
      (let ((imports
             (remove-if #'eclim--java-ignore-import-p
                        (remove-if (lambda (x) (member x unused-imports))
                                   (append (eclim--java-extract-imports) additional-imports)))))
        (write-imports (eclim--java-sort-imports imports imports-order) nil)))))

(defun eclim-java-import ()
  "Reads the token at the point and calls eclim to resolve it to
a java type that can be imported."
  (interactive)
  (let* ((pattern (cdr (eclim--java-identifier-at-point)))
         (imports (eclim/java-import (eclim--project-name) pattern)))
    (eclim--java-organize-imports (eclim/java-import-order (eclim--project-name))
                                  (list (eclim--completing-read "Import: " imports)))))

(defun eclim-java-import-missing ()
  "Checks the current file for missing imports and prompts the
user if necessary."
  (interactive)
  (let ((imports-order (eclim/java-import-order (eclim--project-name))))
    (loop for unused across
          (json-read-from-string
           (replace-regexp-in-string "'" "\""
                                     (first (eclim/java-import-missing (eclim--project-name)))))
          do (let* ((candidates (append (cdr (assoc 'imports unused)) nil))
                    (len (length candidates)))
               (if (= len 0) nil
                 (eclim--java-organize-imports imports-order
                                               (if (= len 1) candidates
                                                 (list
                                                  (eclim--completing-read (concat "Missing type '" (cdr (assoc 'type unused)) "'")
                                                                         candidates)))))))))

(defun eclim-java-remove-unused-imports ()
  (interactive)
  (eclim--java-src-update)
  (let ((imports-order (eclim/java-import-order (eclim--project-name)))
        (unused (eclim/java-import-unused (eclim--project-name))))
    (eclim--java-organize-imports imports-order nil unused)))

(defun eclim/java-impl (project file &optional offset encoding type superType methods)
  (eclim--check-project project)
  (eclim--call-process "java_impl" "-p" project "-f" file))

(defun eclim-java-implement ()
  (interactive)
  ;; TODO: present the user with more fine grain control over the selection of methods
  (let* ((response (eclim/java-impl (eclim--project-name) (eclim--project-current-file)))
         (methods (remove-if-not (lambda (element) (string-match "(.*)" element))
                                 response)))
    (insert (eclim--completing-read "Signature: " methods) " {}")
    (backward-char)))


(defun eclim--java-complete-internal (completion-list)
  (let* ((window (get-buffer-window "*Completions*" 0))
         (c (eclim--java-identifier-at-point))
         (beg (car c))
         (word (cdr c))
         (compl (try-completion word
                                completion-list)))
    (if (and (eq last-command this-command)
             window (window-live-p window) (window-buffer window)
             (buffer-name (window-buffer window)))
        ;; If this command was repeated, and there's a fresh completion window
        ;; with a live buffer, and this command is repeated, scroll that
        ;; window.
        (with-current-buffer (window-buffer window)
          (if (pos-visible-in-window-p (point-max) window)
              (set-window-start window (point-min))
            (save-selected-window
              (select-window window)
              (scroll-up))))
      (cond
       ((null compl)
        (message "No completions."))
       ((stringp compl)
        (if (string= word compl)
            ;; Show completion buffer
            (let ((list (all-completions word completion-list)))
              (setq list (sort list 'string<))
              (with-output-to-temp-buffer "*Completions*"
                (display-completion-list list word)))
          ;; Complete
          (delete-region beg (point))
          (insert compl)
          ;; close completion buffer if there's one
          (let ((win (get-buffer-window "*Completions*" 0)))
            (if win (quit-window nil win)))))
       (t (message "That's the only possible completion."))))))

(defun eclim-java-complete ()
  (interactive)
  (when eclim-auto-save (save-buffer))
  (eclim--java-complete-internal (mapcar 'second (eclim/java-complete))))


(provide 'eclim-java)