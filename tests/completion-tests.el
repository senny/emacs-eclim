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

