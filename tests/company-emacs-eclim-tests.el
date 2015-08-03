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
