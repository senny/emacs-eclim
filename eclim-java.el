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
(define-key eclim-mode-map (kbd "C-c C-e f d") 'eclim-java-find-declaration)
(define-key eclim-mode-map (kbd "C-c C-e f r") 'eclim-java-find-references)
(define-key eclim-mode-map (kbd "C-c C-e f t") 'eclim-java-find-type)
(define-key eclim-mode-map (kbd "C-c C-e f f") 'eclim-java-find-generic)
(define-key eclim-mode-map (kbd "C-c C-e r") 'eclim-java-refactor-rename-symbol-at-point)
(define-key eclim-mode-map (kbd "C-c C-e i") 'eclim-java-import-missing)
(define-key eclim-mode-map (kbd "C-c C-e u") 'eclim-java-remove-unused-imports)
(define-key eclim-mode-map (kbd "C-c C-e h") 'eclim-java-hierarchy)
(define-key eclim-mode-map (kbd "C-c C-e z") 'eclim-java-implement)
(define-key eclim-mode-map (kbd "C-c C-e d") 'eclim-java-doc-comment)

(defgroup eclim-java nil
  "Java: editing, browsing, refactoring"
  :group 'eclim)

(defcustom eclim-java-major-modes '(java-mode jde-mode)
  "This variable contains a list of major modes to edit java
files. There are certain operations, that eclim will only perform when
the current buffer is contained within this list"
  :group 'eclim-java
  :type 'list)

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
          (eclim/execute-command "java_complete" "-p" "-f" "-e" ("-l" "standard") "-o")))

(defun eclim/java-src-update (&optional save-others)
  "If `eclim-auto-save' is non-nil, save the current java
buffer. In addition, if `save-others' is non-nil, also save any
other unsaved buffer. Finally, tell eclim to update its java
sources."
  (when eclim-auto-save
    (when (buffer-modified-p) (save-buffer)) ;; auto-save current buffer, prompt on saving others
    (when save-others (save-some-buffers nil (lambda () (string-match "\\.java$" (buffer-file-name)))))))

(defadvice delete-file (around eclim--delete-file (filename) activate)
  "Advice the `delete-file' function to trigger a source update
in eclim when appropriate."
  (let ((buf (current-buffer))
	(pr nil)
	(fn nil))
    (switch-to-buffer (find-buffer-visiting filename))
    (ignore-errors
      (setq pr (eclim--project-name))
      (setq fn (eclim--project-current-file)))
    (switch-to-buffer buf)
    ad-do-it
    (when (and pr fn)
      (ignore-errors (apply 'eclim--call-process (list "java_src_update" "-p" pr "-f" fn))))))

(defun eclim--java-current-type-name (&optional type)
  "Searches backward in the current buffer until a type
declaration has been found. TYPE may be either 'class',
'interface', 'enum' or nil, meaning 'match all of the above'."
  (save-excursion
    (if (re-search-backward 
	 (concat (or type "\\(class\\|interface\\|enum\\)") "\\s-+\\([^<{\s-]+\\)") nil t)
	(match-string 2)
      "")))

(defun eclim--java-current-class-name ()
  "Searches backward in the current buffer until a class declaration
has been found."
  (eclim--java-current-type-name "class"))

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

(defun eclim-java-doc-comment ()
  "Inserts or updates a javadoc comment for the element at point."
  (interactive)
  (eclim/execute-command "javadoc_comment" "-p" "-f" "-o"))

(defun eclim-java-constructor ()
  (interactive)
  (eclim/execute-command "java_constructor" "-p" "-f" "-o"))

(defun eclim/java-hierarchy (project file offset encoding)
  (eclim--call-process "java_hierarchy"
		       "-p" project
		       "-f" file
		       "-o" (number-to-string offset)
		       "-e" encoding))

(defun eclim-java-refactor-rename-symbol-at-point ()
  "Rename the java symbol at point."
  (interactive)
  (let* ((i (eclim--java-identifier-at-point t))
	 (n (read-string (concat "Rename " (cdr i) " to: ") (cdr i))))
    (eclim/with-results files ("java_refactor_rename" "-p" "-e" "-f" ("-n" n) 
			       ("-o" (car i)) ("-l" (length (cdr i))))
			(when (not (string= "files:" (first files)))
			  (error (first files)))
			(when (not (file-exists-p (buffer-file-name)))
			  (kill-buffer)
			  (eclim-java-find-type n))
			(let ((current (current-buffer)))
			  (loop for file in files
				for buf = (get-file-buffer (file-name-nondirectory file))
				when buf do (progn (switch-to-buffer buf)
						   (revert-buffer t t t)))
			  (switch-to-buffer current))
			(message "Done"))))

(defun eclim-java-hierarchy (project file offset encoding)
  (interactive (list (eclim--project-name)
		     (eclim--project-current-file)
		     (eclim--byte-offset)
		     (eclim--current-encoding)))
  (let ((top-node (eclim--java-insert-file-path-for-hierarchy-nodes
		   (json-read-from-string
		    (replace-regexp-in-string
		     "'" "\"" (car (eclim/java-hierarchy project file offset encoding)))))))
  (pop-to-buffer "*eclim: hierarchy*" t)
  (special-mode)
  (let ((buffer-read-only nil))
    (erase-buffer)
    (eclim--java-insert-hierarchy-node
     project
     top-node
     0))))

(defun eclim--java-insert-file-path-for-hierarchy-nodes (node)
  ;Can't use *-find-type here because it will pop a buffer
  ;that isn't part of the project which then breaks future
  ;*-find-type calls and isn't what we want here anyway.
  (eclim/with-results hits ("java_search" ("-p" (cdr (assoc 'qualified node))) ("-t" "type") ("-x" "declarations") ("-s" "workspace"))
    (add-to-list 'node `(file-path . ,(first (split-string (first hits) "|"))))
    (let ((children (cdr (assoc 'children node))))
      (loop for child across children do
	    (eclim--java-insert-file-path-for-hierarchy-nodes child)))
    node))

(defun eclim--java-insert-hierarchy-node (project node level)
  (let ((declaration (cdr (assoc 'name node)))
	(qualified-name (cdr (assoc 'qualified node))))
    (insert (format (concat "%-"(number-to-string (* level 2)) "s=> ") ""))
    (lexical-let ((file-path (cdr (assoc 'file-path node))))
      (if file-path
	  (insert-text-button declaration
			  'follow-link t
			  'help-echo qualified-name
			  'action (lambda (&rest ignore)
				    (eclim--find-file file-path)))
	(insert declaration))))
  (newline)
  (let ((children (cdr (assoc 'children node))))
    (loop for child across children do
	  (eclim--java-insert-hierarchy-node project child (+ level 1)))))

(defun eclim--java-split-search-results (res)
  (mapcar (lambda (l) (split-string l "|" nil)) res))

(defun eclim-java-find-declaration ()
  "Find and display the declaration of the java identifier at point."
  (interactive)
  (let ((i (eclim--java-identifier-at-point t)))
    (eclim/with-results hits ("java_search" "-n" "-f" ("-o" (car i)) ("-l" (length (cdr i))) ("-x" "declaration"))
			(let ((r (eclim--java-split-search-results hits)))
			  (if (= (length r) 1)
			      (eclim--visit-declaration (car r))
			    (eclim--find-display-results (cdr i) r))))))

(defun eclim-java-find-references ()
  "Find and display references for the java identifier at point."
  (interactive)
  (let ((i (eclim--java-identifier-at-point t)))
    (eclim/with-results hits ("java_search" "-n" "-f" ("-o" (car i)) ("-l" (length (cdr i))) ("-x" "references"))
			(eclim--find-display-results (cdr i) (eclim--java-split-search-results hits)))))

(defun eclim-java-find-type (type-name)
  "Searches the project for a given class. The TYPE-NAME is the pattern, which will be used for the search."
  (interactive (list (read-string "Name: " (let ((case-fold-search nil)
						 (current-symbol (symbol-name (symbol-at-point))))
					     (if (string-match-p "^[A-Z]" current-symbol)
						 current-symbol
					       (eclim--java-current-type-name))))))
  (eclim-java-find-generic "workspace" "declarations" "type" type-name t))

(defun eclim-java-find-generic (scope context type pattern &optional open-single-file)
  (interactive (list (eclim--completing-read "Scope: " eclim--java-search-scopes)
		     (eclim--completing-read "Context: " eclim--java-search-contexts)
		     (eclim--completing-read "Type: " eclim--java-search-types)
		     (read-string "Pattern: ")))
  (eclim/with-results hits ("java_search" ("-p" pattern) ("-t" type) ("-x" context) ("-s" scope))
		      (eclim--find-display-results pattern (eclim--java-split-search-results hits) open-single-file)))

(defun eclim--java-identifier-at-point (&optional full)
  "Returns a cons cell (BEG . IDENTIFIER) where BEG is the start
buffer position of the token/identifier at point, and IDENTIFIER
is the string from BEG to (point). If argument FULL is non-nill,
IDENTIFIER will contain the whole identifier, not just the
start."
  (let ((boundary "\\([<>()\\[\\.\s\t\n!=,;]\\|]\\)"))
    ;; TODO: make this work for dos buffers
    (save-excursion
      (if (and full (re-search-forward boundary nil t))
	  (backward-char))
      (let ((end (point))
	    (start (progn 
		     (if (re-search-backward boundary nil t) (forward-char))
		     (point))))
	(cons (eclim--byte-offset)
	      (buffer-substring-no-properties start end))))))

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
	 (remove-duplicates (import result)
			    (loop for imp = import then (cdr imp)
				  for f = (first imp)
				  for n = (second imp)
				  while imp
				  when (not (or (eclim--java-wildcard-includes-p f n)
						(equal f n)))
				  collect f)))
    (remove-duplicates
     (sort-imports imports-order (sort imports #'string-lessp) '()) '())))

(defun eclim--java-extract-imports ()
  "Extracts (by removing) import statements of a java
file. Returns a list of the extracted imports. Tries to leave the
cursor at a suitable point for re-inserting new import statements."
  (goto-char 0)
  (let ((imports '()))
    (while (search-forward-regexp "^\s*import \\(.*\\);" nil t)
      (unless (save-match-data
		(string-match "^\s*import\s*static" (match-string 0)))
	(push (match-string-no-properties 1) imports)
	(delete-region (line-beginning-position) (line-end-position))))
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
    (flet ((write-imports (imports)
			  (loop for imp in imports
				for last-import-first-part = nil then first-part
				for first-part = (first (eclim--java-package-components imp))
				do (progn
				     (unless (equal last-import-first-part first-part)
				       (newline))
				     (insert (format "import %s;\n" imp))))))
      (let ((imports
	     (remove-if #'eclim--java-ignore-import-p
			(remove-if (lambda (x) (member x unused-imports))
				   (append (eclim--java-extract-imports) additional-imports)))))
	(write-imports (eclim--java-sort-imports imports imports-order))))))

(defun eclim-java-import ()
  "Reads the token at the point and calls eclim to resolve it to
a java type that can be imported."
  (interactive)
  (eclim/with-results imports ("java_import" "-n" ("-p" (cdr (eclim--java-identifier-at-point))))
		      (eclim--java-organize-imports (eclim/execute-command "java_import_order" "-p")
						    (list (eclim--completing-read "Import: " imports)))))

(defun eclim--fix-static-import (import-spec)
  (let ((imports (cdr (assoc 'imports import-spec)))
	(type (cdr (assoc 'type import-spec))))
    (message "Imports %s" imports)
    (if (not (= 1 (length imports)))
	import-spec
      (if (not (stringp type))
	  import-spec
	(progn
	  (message "Type: %s first element of imports: %s" type (elt imports 0))
	  (if (string-endswith-p (elt imports 0) type)
	      import-spec
	    (progn
	      (message "Appending")
	      (list
	       (cons 'imports (vector (concat (elt imports 0) "." type)))
	       (cons 'type type)))))))))

(defun eclim-java-import-missing ()
  "Checks the current file for missing imports and prompts the
user if necessary."
  (interactive)
  (eclim/with-results imports-order ("java_import_order" "-p")
		      (loop for unused across
			    (json-read-from-string
			     (replace-regexp-in-string "'" "\"" (first (eclim/execute-command "java_import_missing" "-p" "-f"))))
			    do (let* ((candidates (append (cdr (assoc 'imports (eclim--fix-static-import unused))) nil))
				      (len (length candidates)))
				 (if (= len 0) nil
				   (eclim--java-organize-imports imports-order
								 (if (= len 1) candidates
								   (list
								    (eclim--completing-read (concat "Missing type '" (cdr (assoc 'type unused)) "'")
											    candidates)))))))))

(defun eclim-java-remove-unused-imports ()
  "Remove usused import from the current java source file."
  (interactive)
  (eclim/with-results unused ("java_imports_unused" "-p" "-f")
		      (let ((imports-order (eclim/execute-command "java_import_order" "-p")))
			(eclim--java-organize-imports imports-order nil unused))))

(defun eclim-java-implement ()
  "Lets the user select from a list of methods to
implemnt/override, then inserts a skeleton for the chosen
method."
  (interactive)
  (eclim/with-results response ("java_impl" "-p" "-f" "-o")
		      (let* ((methods  
			      (remove-if (lambda (element) (string-match "//" element))
					 (remove-if-not (lambda (element) (string-match "(.*)" element))
							response)))
			     (start (point)))
			(insert 
			 "@Override\n"
			 (replace-regexp-in-string " abstract " " " 
						   (eclim--completing-read "Signature: " methods)) " {}")
			(backward-char)
			(indent-region start (point)))))

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

;; Request an eclipse source update when files are saved
(defun eclim--after-save-hook ()
  (when (member major-mode eclim-java-major-modes)
    (ignore-errors
      (if eclim-mode (apply 'eclim--call-process "java_src_update" (eclim--expand-args (list "-p" "-f"))))))
  t)

(add-hook 'after-save-hook 'eclim--after-save-hook)

(provide 'eclim-java)
