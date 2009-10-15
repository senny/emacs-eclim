
(defun eclim/java-complete ()
  (mapcar (lambda (line)
            (split-string line "|" nil))
          (eclim--call-process "java_complete"
                               "-p" (eclim--project-name)
                               "-f" (eclim--project-current-file)
                               "-e" "iso-8859-1"
                               "-l" "standard"
                               "-o" (number-to-string (eclim--byte-offset)))))

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

(defun eclim--java-identifier-at-point ()
  "Returns a string containing the word positioned under the
  cursor. Not exactly the perfect solution for picking out java
  tokens, but will have to do."
  (save-excursion
    (forward-char)
    (backward-word 1)
    (let ((p (point)))
      (forward-word 1)
      (buffer-substring-no-properties p (point)))))

(defun string-startswith-p (string prefix)
  ;; TODO: there is probably already a library function that does this
  (equal (substring-no-properties string 0 (string-width prefix)) prefix))

(defun string-endswith-p (string prefix)
  ;; TODO: there is probably already a library function that does this
  (let ((w (string-width string)))
    (equal (substring-no-properties string (- w (string-width prefix)) w) prefix)))

(defun eclim--java-package-components (package)
  "Returns the components of a Java package statement."
  (split-string package "\\."))

(defun eclim--java-wildcard-includes-p (wildcard package)
  "Returns true if PACKAGE is included in the WILDCARD import statement."
  (if (not (string-endswith-p wildcard ".*")) nil
    (equal (butlast (eclim--java-package-components wildcard))
	   (butlast (eclim--java-package-components package)))))

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
				    (n (first (rest imports))))
				(if (or (eclim--java-wildcard-includes-p f n)
					(equal f n))
				    (remove-duplicates (cons f (rest (rest imports))) result)
				  (remove-duplicates (rest imports) (cons f result)))))))
    (reverse 
     (remove-duplicates
      (sort-imports imports-order (sort imports #'string-lessp) '()) '()))))

(defun eclim--java-organize-imports (imports-order &optional additional-imports)
  "Organize the import statements in the current file according
  to IMPORTS-ORDER. If the optional parameter ADDITIONAL-IMPORTS
  is supplied, these import statements will be added to the
  rest."
  (save-excursion
    (flet ((write-imports (imports last-import-first-part)
			  (when imports
			    (let ((first-part (first (eclim--java-package-components (first imports)))))
			      (if (and last-import-first-part
				       (not (equal last-import-first-part first-part)))
				  (newline))
			      (insert (format "import %s;\n" (first imports)))
			      (write-imports (rest imports) first-part)))))
    (let ((imports additional-imports))
      (goto-char 0)
      (while (search-forward-regexp "^\s*import \\(.*\\);" nil t)
	(push (match-string-no-properties 1) imports)
	(beginning-of-line)
	(kill-line))
      (delete-blank-lines)
      (newline)
      (write-imports (eclim--java-sort-imports imports imports-order) nil)))))

(defun eclim-java-import ()
  "Reads the token at the point and calls eclim to resolve it to
a java type that can be imported."
  (interactive)
  (let* ((pattern (eclim--java-identifier-at-point))
	 (imports (eclim/java-import (eclim--project-name) pattern)))
    (eclim--java-organize-imports (eclim/java-import-order (eclim--project-name))
				  (list (eclim--choices-prompt "Import" imports)))))

(defun eclim-java-import-missing ()
  (interactive)
  (let ((imports (eclim/java-import-missing (eclim--project-name))))
    ;; TODO: display user selection for the missing imports
    ))

(defun eclim-java-remove-unused-imports ()
  (interactive)
  (let ((unused (eclim/java-import-unused (eclim--project-name))))
    ;; TODO: display user selection for the missing imports
    ))

(defun eclim/java-impl (project file &optional offset encoding type superType methods)
  (eclim--check-project project)
  (eclim--call-process "java_impl" "-p" project "-f" file))

(defun eclim-java-implement ()
  (interactive)
  ;; TODO: present the user with more fine grain control over the selection of methods
  (let* ((response (eclim/java-impl (eclim--project-name) (eclim--project-current-file)))
         (methods (remove-if-not (lambda (element) (string-match "(.*)" element))
                                 response)))
    (insert (ido-completing-read "Signature: " methods) " {}")
    (backward-char)))

(provide 'eclim-java)