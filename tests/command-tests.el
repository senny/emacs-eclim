(ert-deftest test-command-should-sync-for-normal-commands ()
  (should (eclim--command-should-sync-p "java_complete" '("-f" "~/src/test/Panda.java")))
  (should (not (eclim--command-should-sync-p "other_cmd" '("-x" "~/src/test/Panda.java")))))

(ert-deftest test-command-should-sync-for-special-commands ()
  (should (not (eclim--command-should-sync-p "project_by_resource" '("-f" "~/src/test/Panda.java"))))
  (should (not (eclim--command-should-sync-p "project_link_resource" '("-f" "~/src/test/Panda.java")))))

                 
