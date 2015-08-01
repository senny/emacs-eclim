
(require 'eclim-java-run)

(ert-deftest command-for-java-configuration ()
  (let* ((conf '((name . "Test run")
                 (main-class . "com.acme.Project")
                 (program-args . "arg1 arg2")
                 (vm-args . "-Dvm-arg1=42")))
         (run-command (eclim-java-run--command conf
                                               (eclim-java-run--java-vm-args "/opt/lib.jar"))))
    (should (string-equal run-command
                          "java -classpath /opt/lib.jar -Dvm-arg1=42 com.acme.Project arg1 arg2"))))

(ert-deftest command-for-debug-configuration ()
  (let* ((conf '((name . "Debug test run")
                 (main-class . "com.acme.Project")
                 (program-args . "arg1 arg2")
                 (vm-args . "-Dvm-arg1=42")
                 (debug . t)))
         (run-command (eclim-java-run--command conf
                                               (eclim-java-run--java-vm-args "/opt/lib.jar"))))
    (should (string-equal run-command
                          "jdb -classpath/opt/lib.jar -Dvm-arg1=42 com.acme.Project arg1 arg2"))))

(ert-deftest choose-configuration ()
  (let* ((conf1 '((name . "Debug")))
         (conf2 '((name . "Run")))
         (choosen-conf (eclim-java-run--configuration "Run" (list conf1 conf2))))
    (should (equal choosen-conf conf2))))

(ert-deftest run-java-opens-buffer-in-correct-dir-with-correct-name ()
  (let* ((conf '((name . "Run")
                 (main-class . "com.acme.Project")
                 (program-args . "arg1")
                 (vm-args . "-Dvm-args1=42")))
         (buffer (eclim-java-run--run conf "/opt/lib.jar" "/tmp/")))
    (with-current-buffer buffer
      (should (string-equal default-directory "/tmp/"))
      (should (string-equal (buffer-name) "*Run*")))))
