(defun eclim/java-complete ()
  (mapcar (lambda (line)
            (split-string line "|"))
          (eclim--call-process "java_complete"
                               "-p" (eclim--project-name)
                               "-f" (eclim--project-current-file)
                               "-e" "iso-8859-1"
                               "-l" "standard"
                               "-o" (number-to-string (eclim--byte-offset)))))

(defun eclim/java_classpath (project)
  (eclim--check-project project)
  (eclim--call-process "java_classpath" "-p" project))

(defun eclim/java_classpath_variables ()
  ;; TODO: fix trailing whitespaces
  (mapcar (lambda (line)
            (split-string line "-")) (eclim--call-process "java_classpath_variables")))

(defun eclim/java_classpath_variable_create (name path)
  (eclim--call-process "java_classpath_variable_create" "-n" name "-p" path))

(defun eclim/java_classpath_variable_delete (name)
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

(defun eclim--java-organize-imports (imports-order)
  "Organize the import statements in the current file."
  ;; TODO: this is code I lifted directly from one of my other
  ;; projects. It doesn't care about the specified sort order and
  ;; should probably be replaced.
  (let ((p 0))
    (save-excursion
      (goto-char 0)
      (search-forward-regexp "^import")
      (beginning-of-line)
      (let ((p0 (point)))
	(loop do (setq p (search-forward-regexp "^import" (buffer-end 1) t)) while p)
	(end-of-line)
	(forward-line)
	(sort-regexp-fields nil "^import \\(static \\)?\\(.*\\)$" "\\2"
			    p0 (point))))))

(defun eclim--java-insert-import (import-statement imports-order)
  "Inserts an import statement at the corrent place in the current
file."
  ;; TODO: this is a pretty naive implementation that doesn't take
  ;; comments at the beginning of the file into account
  (save-excursion
    (goto-char 0)
    (forward-line)
    (forward-line)
    (insert "import " import-statement ";\n"))
  (eclim--java-organize-imports imports-order))

(defun eclim-java-import ()
  "Reads the token at the point and calls eclim to resolve it to
a java type that can be imported."
  (interactive)
  (let* ((pattern (eclim--java-identifier-at-point))
	 (imports (eclim/java-import (eclim--project-name) pattern)))
    (eclim--java-insert-import (eclim--choices-prompt "Import" imports)
			       (eclim/java-import-order (eclim--project-name)))))

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