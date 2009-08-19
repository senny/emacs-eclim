(defun eclim/java-complete ()
  (mapcar (lambda (line)
            (split-string line "|"))
          (eclim--call-process "java_complete"
                               "-p" (eclim--project-name)
                               "-f" (file-relative-name buffer-file-name (eclim--project-dir))
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

(provide 'eclim-java)