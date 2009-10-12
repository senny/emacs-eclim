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

(provide 'eclim-java)