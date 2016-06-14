(ert-deftest completion-yasnippet-convert ()
  ;; Nested params should *not* be nested templates.
  (should (equal (eclim--completion-yasnippet-convert
                  "addAll(Collection<? super Object> c, T... elements)")
                 "addAll(${Collection<? super Object> c}, ${T... elements})"))
  ;; Corner case: no argument.
  (should (equal (eclim--completion-yasnippet-convert "toString()")
                 "toString()"))

  ;; Basic cases.
  (should (equal (eclim--completion-yasnippet-convert
                  "printf(Locale l, String format, Object... args)")
                 "printf(${Locale l}, ${String format}, ${Object... args})"))
  (should (equal (eclim--completion-yasnippet-convert "HashMap<K,V>")
                 "HashMap<${K}, ${V}>"))

  )

(ert-deftest completion-insert-empty-usable ()
  (let ((eclim-insertion-functions '(eclim-completion-insert-empty)))
    (cl-letf (((symbol-function 'eclim-java-import) #'ignore))
      (with-temp-buffer
        (insert "method(String arg1, List<String> arg2) - some.Class")
        (eclim--completion-action-java (line-beginning-position) (point))
        (should (equal (thing-at-point 'line) "method()"))
        (should (looking-at ")"))
        (erase-buffer)
        (insert "method2()")
        (should (equal (thing-at-point 'line) "method2()"))
        (should (eolp))
        ))))
