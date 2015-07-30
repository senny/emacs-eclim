;; ("child : String - LibraryA" "clone() : Object - Object" "equals(Object obj) : boolean - Object" "finalize() : void - Object" "getClass() : Class<?> - Object" "getter() : String - LibraryA" "getterWithParams(int x, int y, int z) : int - LibraryA" "hashCode() : int - Object" "new" "notify() : void - Object" "notifyAll() : void - Object" "STATIC_STRING_LIB_A : String - LibraryA" "staticMethod() : String - LibraryA" "toString() : String - Object" "wait(long timeout, int nanos) : void - Object" "wait(long timeout) : void - Object" "wait() : void - Object")


;; Result: ("java.util" "java.util.concurrent" "java.util.concurrent.atomic" "java.util.concurrent.locks" "java.util.function" "java.util.jar" "java.util.logging" "java.util.prefs" "java.util.regex" "java.util.spi" "java.util.stream" "java.util.zip")

(require 'company-emacs-eclim)

(ert-deftest compute-full-prefix-when-complete-import ()
  (with-temp-buffer
    (insert "import java.uti")
    (goto-char (point-max))
    (let ((before-prefix (company-emacs-eclim--before-prefix-in-buffer "uti")))
      (should (string-equal before-prefix "java.")))))

(ert-deftest when-completing-import-prefix-should-be-trimmed ()
  (let ((candidates (emacs-eclim--candidates-for-temp-buffer
                     '((content . "import java.uti")
                       (mocked-response . ("java.util" "java.util.stream"))
                       (prefix . "uti")))))
    (should (equal candidates '("util" "util.stream")))))

(ert-deftest when-completing-fields-candidates-shouldnt-change ()
  (let ((candidates (emacs-eclim--candidates-for-temp-buffer
                     '((content . "this.liba.sec")
                       (mocked-response . ("secondChild : String - LibraryA"))
                       (prefix . "sec")))))
    (should (equal candidates '("secondChild : String - LibraryA")))))

(ert-deftest when-completing-method-real-candidate-is-passed-as-test-property ()
  (let  ((candidates (emacs-eclim--candidates-for-temp-buffer
                      '((content . "this.liba.get")
                        (mocked-response . ("getterWithParams(int x, int y, int z) : int - LibraryA"))
                        (prefix . "get")))))
    (should (equal candidates '("getterWithParams")))
    (should (equal (get-text-property 0 'eclim-meta (first candidates)) "getterWithParams(int x, int y, int z) : int - LibraryA"))))

(defun emacs-eclim--candidates-for-temp-buffer (arg)
  (with-temp-buffer
    (insert (cdr (assoc 'content arg)))
    (goto-char (point-max))
    (flet ((eclim--completion-candidates () (cdr (assoc 'mocked-response arg))))
      (company-emacs-eclim--candidates (cdr (assoc 'prefix arg))))))
