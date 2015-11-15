;; eclim-java.el --- an interface to the Eclipse IDE.
;;
;; Copyright (C) 2009  Yves Senn <yves senn * gmx ch>
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Contributors
;;
;; - Tassilo Horn <tassilo@member.fsf.org>
;;
;;; Conventions
;;
;; Conventions used in this file: Name internal variables and functions
;; "eclim--<descriptive-name>", and name eclim command invocations
;; "eclim/command-name", like eclim/project-list.

;;* Eclim Java

(require 'json)
(require 'dash)

(define-key eclim-mode-map (kbd "C-c C-e s") 'eclim-java-method-signature-at-point)
(define-key eclim-mode-map (kbd "C-c C-e f d") 'eclim-java-find-declaration)
(define-key eclim-mode-map (kbd "C-c C-e f r") 'eclim-java-find-references)
(define-key eclim-mode-map (kbd "C-c C-e f t") 'eclim-java-find-type)
(define-key eclim-mode-map (kbd "C-c C-e f f") 'eclim-java-find-generic)
(define-key eclim-mode-map (kbd "C-c C-e r") 'eclim-java-refactor-rename-symbol-at-point)
(define-key eclim-mode-map (kbd "C-c C-e i") 'eclim-java-import-organize)
(define-key eclim-mode-map (kbd "C-c C-e h") 'eclim-java-hierarchy)
(define-key eclim-mode-map (kbd "C-c C-e z") 'eclim-java-implement)
(define-key eclim-mode-map (kbd "C-c C-e d") 'eclim-java-doc-comment)
(define-key eclim-mode-map (kbd "C-c C-e f s") 'eclim-java-format)
(define-key eclim-mode-map (kbd "C-c C-e g") 'eclim-java-generate-getter-and-setter)
(define-key eclim-mode-map (kbd "C-c C-e t") 'eclim-run-junit)

(defvar eclim-java-show-documentation-map
  (let ((map (make-keymap)))
    (suppress-keymap map)
    (define-key map (kbd "<tab>") 'forward-button)
    (define-key map (kbd "S-<tab>") 'backward-button)
    (define-key map (kbd "q") 'eclim-quit-window)
    map))


(defgroup eclim-java nil
  "Java: editing, browsing, refactoring"
  :group 'eclim)

(defcustom eclim-java-major-modes '(java-mode jde-mode)
  "This variable contains a list of major modes to edit java
files. There are certain operations, that eclim will only perform when
the current buffer is contained within this list"
  :group 'eclim-java
  :type 'list)

;; Could this value be taken from Eclipse somehow?"
(defcustom eclim-java-documentation-root nil
  "Root directory of Java HTML documentation.

If Android is used then Eclipse may refer standard Java elements from the copy of
Java documentation under Android docs, so don't forget to set
`eclim-java-android-documentation-root' too in that case."
  :group 'eclim-java
  :type 'directory)

;; Could this value be taken from Eclipse somehow?"
(defcustom eclim-java-android-documentation-root nil
  "Root directory of Android HTML documentation."
  :group 'eclim-java
  :type 'directory)


(defvar eclim--java-search-types '("all"
                                   "annotation"
                                   "class"
                                   "classOrEnum"
                                   "classOrInterface"
                                   "constructor"
                                   "enum"
                                   "field"
                                   "interface"
                                   "method"
                                   "package"
                                   "type"))

(defvar eclim--java-search-scopes '("all"
                                    "project"
                                    "type"))

(defvar eclim--java-search-contexts '("all"
                                      "declarations"
                                      "implementors"
                                      "references"))

(defvar eclim--is-completing nil)
(defvar eclim-java-show-documentation-history nil)

(defun eclim/groovy-src-update (&optional save-others)
  "If `eclim-auto-save' is non-nil, save the current java
 buffer. In addition, if `save-others' is non-nil, also save any
 other unsaved buffer. Finally, tell eclim to update its java
 sources."
  (when eclim-auto-save
    (when (buffer-modified-p) (save-buffer)) ;; auto-save current buffer, prompt on saving others
    (when save-others (save-some-buffers nil (lambda () (string-match "\\.groovy$" (buffer-file-name)))))))

(defun eclim/java-src-update (&optional save-others)
  "If `eclim-auto-save' is non-nil, save the current java
buffer. In addition, if `save-others' is non-nil, also save any
other unsaved buffer. Finally, tell eclim to update its java
sources."
  (when eclim-auto-save
    (when (buffer-modified-p) (save-buffer)) ;; auto-save current buffer, prompt on saving others
    (when save-others (save-some-buffers nil (lambda () (string-match "\\.java$" (buffer-file-name)))))))

(defadvice delete-file (around eclim--delete-file activate)
  "Advice the `delete-file' function to trigger a source update
in eclim when appropriate."
  (let ((pr nil)
        (fn nil))
    (ignore-errors
      (and (setq pr (eclim-project-name filename))
           (setq fn (file-relative-name filename (eclim--project-dir pr)))))
    ad-do-it
    (when (and pr fn)
      (ignore-errors (apply 'eclim--call-process (list "java_src_update" "-p" pr "-f" fn))))))

(defun eclim--java-parser-read (str)
  (first
   (read-from-string
    (format "(%s)"
            (replace-regexp-in-string
             "[<>(),?]"
             (lambda (m) (assoc-default m '(("<" . "((") (">" . "))")
                                       ("(" . "((") (")" ."))")
                                       ("," . ")(")
                                       ("?" . "\\\\?"))))
             str)))))

(defun eclim--java-parse-method-signature (signature)
  (cl-flet ((parser3/parse-arg (arg)
                               (let ((arg-rev (reverse arg)))
                                 (cond ((null arg) nil)
                                       ((= (length arg) 1) (list (list :type (first arg))))
                                       ((listp (first arg-rev)) (list (cons :type arg)))
                                       (t (list (cons :name (first arg-rev)) (cons :type (reverse (rest arg-rev)))))))))
    (let ((ast (reverse (eclim--java-parser-read signature))))
      (list (cons :arglist (mapcar #'parser3/parse-arg (first ast)))
            (cons :name (second ast))
            (cons :return (reverse (rest (rest ast))))))))

(defun eclim--java-current-type-name (&optional type)
  "Searches backward in the current buffer until a type
declaration has been found. TYPE may be either 'class',
'interface', 'enum' or nil, meaning 'match all of the above'."
  (save-excursion
    (if (re-search-backward
         (concat (or type "\\(class\\|interface\\|enum\\)") "\\s-+\\([^<{\s-]+\\)") nil t)
        (match-string-no-properties 2)
      "")))

(defun eclim--java-current-class-name ()
  "Searches backward in the current buffer until a class declaration
has been found."
  (eclim--java-current-type-name "\\(class\\)"))

(defun eclim--java-generate-bean-properties (project file offset encoding type)
  "Generates a bean property for the symbol at point. TYPE specifies the property to generate."
  (eclim--call-process "java_bean_properties"
                       "-p" project
                       "-f" file
                       "-o" (number-to-string offset)
                       "-e" encoding
                       "-r" (cdr (eclim--java-identifier-at-point t))
                       "-t" type)
  (revert-buffer t t t))

(defun eclim--java-refactor (result)
  "Processes the resulst of a refactor command. RESULT is the
  results of invoking eclim/execute-command."
  (if (stringp result) (error "%s" result))
  (loop for (from to) in (mapcar (lambda (x) (list (assoc-default 'from x) (assoc-default 'to x))) result)
        do (when (and from to)
             (kill-buffer (find-buffer-visiting from))
             (find-file to)))
  (save-excursion
    (loop for file in (mapcar (lambda (x) (assoc-default 'file x)) result)
          do (when file
               (let ((buf (get-file-buffer (file-name-nondirectory file))))
                 (when buf
                   (switch-to-buffer buf)
                   (revert-buffer t t t))))))
  (message "Done"))

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

(defun eclim-java-doc-comment ()
  "Inserts or updates a javadoc comment for the element at point."
  (interactive)
  (eclim/execute-command "javadoc_comment" "-p" "-f" "-o"))

(defun eclim-run-java-doc ()
  "Run Javadoc on current or all projects."
  (interactive)
  (let ((proj-list (eclim/project-list)))
    (if (y-or-n-p "Run Javadoc for all projects?")
        (dotimes (i (length proj-list))
            (eclim--call-process-no-parse "javadoc" "-p" (rest (assq 'name (elt proj-list i)))))
      (eclim--call-process-no-parse "javadoc" "-p"))
    (message "Javadoc creation finished.")))

(defun eclim-java-format ()
  "Format the source code of the current java source file."
  (interactive)
  (eclim/execute-command "java_format" "-p" "-f" ("-h" 0) ("-t" (1- (point-max))) "-e"))

(defun eclim-java-generate-getter-and-setter (project file offset encoding)
  "Generates getter and setter methods for the symbol at point."
  (interactive (list (eclim-project-name)
                     (eclim--project-current-file)
                     (eclim--byte-offset)
                     (eclim--current-encoding)))
  (eclim--java-generate-bean-properties project file offset encoding "gettersetter"))

(defun eclim-java-generate-getter (project file offset encoding)
  "Generates a getter method for the symbol at point."
  (interactive (list (eclim-project-name)
                     (eclim--project-current-file)
                     (eclim--byte-offset)
                     (eclim--current-encoding)))
  (eclim--java-generate-bean-properties project file offset encoding "getter"))

(defun eclim-java-generate-setter (project file offset encoding)
  "Generates a setter method for the symbol at point."
  (interactive (list (eclim-project-name)
                     (eclim--project-current-file)
                     (eclim--byte-offset)
                     (eclim--current-encoding)))
  (eclim--java-generate-bean-properties project file offset encoding "setter"))

(defun eclim-java-constructor ()
  (interactive)
  (eclim/execute-command "java_constructor" "-p" "-f" "-o"))

(defun eclim/java-call-hierarchy (project file offset length encoding)
  (eclim--call-process "java_callhierarchy"
                       "-p" project
                       "-f" file
                       "-o" (number-to-string offset)
                       "-l" (number-to-string length)
                       "-e" encoding))

(defun eclim/java-hierarchy (project file offset encoding)
  (eclim--call-process "java_hierarchy"
                       "-p" project
                       "-f" file
                       "-o" (number-to-string offset)
                       "-e" encoding))

(defun eclim-java-refactor-rename-symbol-at-point ()
  "Rename the java symbol at point."
  (interactive)
  (let* ((i (eclim--java-identifier-at-point t))
         (n (read-string (concat "Rename " (cdr i) " to: ") (cdr i))))
    (eclim/with-results res ("java_refactor_rename" "-p" "-e" "-f" ("-n" n)
                             ("-o" (car i)) ("-l" (length (cdr i))))
      (eclim--java-refactor res))))

(defun eclim-java-refactor-move-class ()
  "Renames the java class. Searches backward in the current buffer
until a class declaration has been found."
  (interactive)
  (let* ((class-name (eclim--java-current-class-name))
         (package-name (eclim--java-current-package))
         (n (read-string (concat "Move " class-name " to: ") package-name)))
    (eclim/with-results res ("java_refactor_move" "-p" "-f" ("-n" n))
      (eclim--java-refactor res))))

(defun eclim-java-call-hierarchy (project file encoding)
  (interactive (list (eclim-project-name)
                     (eclim--project-current-file)
                     (eclim--current-encoding)))
  (let ((boundary "\\([<>()\\[\\.\s\t\n!=,;]\\|]\\)"))
    (save-excursion
      (if (re-search-backward boundary nil t)
          (forward-char))
      (let ((top-node (eclim/java-call-hierarchy project file (eclim--byte-offset)
                                                 (length (cdr (eclim--java-identifier-at-point t))) encoding)))
        (pop-to-buffer "*eclim: call hierarchy*" t)
        (special-mode)
        (let ((buffer-read-only nil))
          (erase-buffer)
          (eclim--java-insert-call-hierarchy-node
           project
           top-node
           0))))))
(defun eclim--java-insert-call-hierarchy-node (project node level)
  (let ((declaration (cdr (assoc 'name node))))
    (insert (format (concat "%-"(number-to-string (* level 2)) "s=> ") ""))
    (lexical-let ((position (cdr (assoc 'position node))))
      (if position
          (insert-text-button declaration
                              'follow-link t
                              'help-echo declaration
                              'action #'(lambda (&rest ignore)
                                          (eclim--visit-declaration position)))
        (insert declaration)))
    (newline)
    (loop for caller across (cdr (assoc 'callers node))
          do (eclim--java-insert-call-hierarchy-node project caller (1+ level)))))

(defun eclim-java-hierarchy (project file offset encoding)
  (interactive (list (eclim-project-name)
                     (eclim--project-current-file)
                     (eclim--byte-offset)
                     (eclim--current-encoding)))
  (let ((top-node (eclim/java-hierarchy project file offset encoding)))
    (pop-to-buffer "*eclim: hierarchy*" t)
    (special-mode)
    (let ((buffer-read-only nil))
      (erase-buffer)
      (eclim--java-insert-hierarchy-node
       project
       top-node
       0))))

(defun eclim--java-insert-file-path-for-hierarchy-node (node)
  (eclim/with-results hits ("java_search" ("-p" (cdr (assoc 'qualified node))) ("-t" "type") ("-x" "declarations") ("-s" "workspace"))
    (assoc-default 'filename (elt hits 0))))

(defun eclim--java-insert-hierarchy-node (project node level)
  (let ((declaration (cdr (assoc 'name node)))
        (qualified-name (cdr (assoc 'qualified node))))
    (insert (format (concat "%-"(number-to-string (* level 2)) "s=> ") ""))
    (lexical-let ((file-path (eclim--java-insert-file-path-for-hierarchy-node node)))
      (if file-path
          (insert-text-button declaration
                              'follow-link t
                              'help-echo qualified-name
                              'action (lambda (&rest ignore)
                                        (eclim--find-file file-path)))
        (insert declaration))))
  (newline)
  (let ((children (cdr (assoc 'children node))))
    (loop for child across children do
          (eclim--java-insert-hierarchy-node project child (+ level 1)))))

(defun eclim-java-find-declaration ()
  "Find and display the declaration of the java identifier at point."
  (interactive)
  (let ((i (eclim--java-identifier-at-point t)))
    (eclim/with-results hits ("java_search" "-n" "-f" ("-o" (car i)) ("-l" (length (cdr i))) ("-x" "declaration"))
      (eclim--find-display-results (cdr i) hits t))))

(defun eclim-c-find-declaration ()
  "Find and display the declaration of the c identifier at point."
  (interactive)
  (let ((i (eclim--java-identifier-at-point t)))
    (eclim/with-results hits ("c_search" "-n" "-f" ("-o" (car i)) ("-l" (length (cdr i))))
      (eclim--find-display-results (cdr i) hits t))))

(defun eclim-java-find-references ()
  "Find and display references for the java identifier at point."
  (interactive)
  (let ((i (eclim--java-identifier-at-point t)))
    (eclim/with-results hits ("java_search" "-n" "-f" ("-o" (car i)) ("-l" (length (cdr i))) ("-x" "references"))
      (eclim--find-display-results (cdr i) hits))))

(defun eclim-java-find-type (type-name &optional case-insensitive)
  "Searches the project for a given class. The TYPE-NAME is the
pattern, which will be used for the search. If invoked with the
universal argument the search will be made CASE-INSENSITIVE."
  (interactive (list (read-string "Name: " (let ((case-fold-search nil)
                                                 (current-symbol (symbol-name (symbol-at-point))))
                                             (if (string-match-p "^[A-Z]" current-symbol)
                                                 current-symbol
                                               (eclim--java-current-type-name))))
                     "P"))
  (eclim-java-find-generic "workspace" "declarations" "type" type-name case-insensitive t))

(defun eclim-java-find-generic (scope context type pattern &optional case-insensitive open-single-file)
  "Searches within SCOPE (all/project/type) for a
TYPE (all/annotation/class/classOrEnum/classOrInterface/constructor/enum/field/interface/method/package/type)
matching the given
CONTEXT (all/declarations/implementors/references) and
PATTERN. If invoked with the universal argument the search will
be made CASE-INSENSITIVE."
  (interactive (list (eclim--completing-read "Scope: " eclim--java-search-scopes)
                     (eclim--completing-read "Context: " eclim--java-search-contexts)
                     (eclim--completing-read "Type: " eclim--java-search-types)
                     (read-string "Pattern: ")
                     "P"))
  (eclim/with-results hits ("java_search" ("-p" pattern) ("-t" type) ("-x" context) ("-s" scope) (if case-insensitive '("-i" "")))
    (eclim--find-display-results pattern hits open-single-file)))

(defun eclim--java-identifier-at-point (&optional full position)
  "Returns a cons cell (BEG . IDENTIFIER) where BEG is the start
buffer byte offset of the token/identifier at point, and
IDENTIFIER is the string from BEG to (point). If argument FULL is
non-nill, IDENTIFIER will contain the whole identifier, not just
the start. If argument POSITION is non-nil, BEG will contain the
position of the identifier instead of the byte offset (which only
matters for buffers containing non-ASCII characters)."
  (let ((boundary "\\([<>()\\[\\.\s\t\n!=,;]\\|]\\)"))
    ;; TODO: make this work for dos buffers
    (save-excursion
      (if (and full (re-search-forward boundary nil t))
          (backward-char))
      (let ((end (point))
            (start (progn
                     (if (re-search-backward boundary nil t) (forward-char))
                     (point))))
        (cons (if position (point) (eclim--byte-offset))
              (buffer-substring-no-properties start end))))))

(defun eclim--java-package-components (package)
  "Returns the components of a Java package statement."
  (split-string package "\\."))

(defun eclim--java-current-package ()
  "Returns the package for the class in the current buffer."
  (save-excursion
    (goto-char 0)
    (if (re-search-forward "package \\(.*?\\);" (point-max) t)
        (match-string-no-properties 1))))

(defun eclim-soft-revert-imports (ignore-auto noconfirm)
  "Can be used as a REVERT-BUFFER-FUNCTION to only replace the
imports section of a java source file. This will preserve the
undo history."
  (interactive)
  (cl-flet ((cut-imports ()
                         (goto-char (point-min))
                         (if (re-search-forward "^import" nil t)
                             (progn
                               (beginning-of-line)
                               (let ((beg (point)))
                                 (goto-char (point-max))
                                 (re-search-backward "^import")
                                 (end-of-line)
                                 (let ((imports (buffer-substring-no-properties beg (point))))
                                   (delete-region beg (point))
                                   imports)))
                           (progn
                             (forward-line 1)
                             (delete-blank-lines)
                             (insert "\n\n\n")
                             (forward-line -2)))))
    (save-excursion
      (clear-visited-file-modtime)
      (cut-imports)
      (widen)
      (insert
       (let ((fname (buffer-file-name)))
         (with-temp-buffer
           (insert-file-contents fname)
           (cut-imports))))
      (not-modified)
      (set-visited-file-modtime))))

(defun eclim-java-import (type)
  "Adds an import statement for the given type, if one does not
exist already."
  (save-excursion
    (goto-char (point-min))
    (let ((revert-buffer-function 'eclim-soft-revert-imports))
      (when (not (re-search-forward (format "^import %s;" type) nil t))
        (eclim/execute-command "java_import" "-p" "-f" "-o" "-e" ("-t" type))
        (eclim--problems-update-maybe)))))

(defun eclim-java-import-organize (&optional types)
  "Checks the current file for missing imports, removes unused imports and
sorts import statements. "
  (interactive)
  (let ((revert-buffer-function 'eclim-soft-revert-imports))
    (eclim/with-results res ("java_import_organize" "-p" "-f" "-o" "-e"
                             (when types (list "-t" (reduce (lambda (a b) (concat a "," b)) types))))
      (eclim--problems-update-maybe)
      (when (vectorp res)
        (save-excursion
          (eclim-java-import-organize
           (mapcar (lambda (imports) (eclim--completing-read "Import: " (append imports '()))) res)))))))


(defun eclim--signature-has-keyword (sig java-keyword)
  "Returns true if a method signature SIG has the keyword JAVA-KEYWORD."
  ;; \_< is beginning of identifier E.g. don't match do_abstract".
  (string-match-p (format "\\_<%s\\_>" java-keyword) sig))


(defun eclim--colorize-signature (sig)
  "Minimal colorization for a method signature that we offer for completion,
so the essential bits stand out from the block of text that ido presents.
Keep this minimal: more highlighting could easily make things worse not better."
  (save-match-data
    (mapc #'(lambda(re-g-f)             ;; expecting single match per RE
              (when (string-match (elt re-g-f 0) sig)
                (setq sig (replace-match
                           (propertize (match-string (elt re-g-f 1) sig)
                                       'face (elt re-g-f 2))
                           nil nil sig (elt re-g-f 1)))))
          '(("\\_<\\(class\\|interface\\)\\s +\\([[:alnum:]_]+\\_>\\)"
             2 font-lock-type-face)
            ("\\_<\\([[:alnum:]_]+\\)(" 1 font-lock-function-name-face)
            ("all [[:digit:]]+ \\w+ methods" 0 font-lock-function-name-face))))
  sig)


(defun eclim-java-implement (&optional name)
  "Implement or override methods from parents of the class, prompting the
user to select with a completing read (even if one, as confirmation). If
NAME was specified programmatically, filters for that name (strict,
although only on method name not arguments) and if only one choice
implement it without prompting. The actual change is done by Eclipse
and will be close to point although not necessarily at it (e.g. if in a
sub block)."
  (interactive)
  (eclim/with-results list-response ("java_impl" "-p" "-f" "-o")
    (let* ((supertypes (assoc-default 'superTypes list-response))
           ;; "Choices" are lists of user-friendly method names. We want to
           ;; present interfaces/abstract first, otherwise Object can barge in.
           (choices nil) (choices-opt nil) (choices-last nil)
           ;; Maps a choice to a (supertype method1 method2...), needed
           ;; when we request eclim to implement that method.
           (choice-data (make-hash-table :test 'equal)))
      (loop
       for super-entry across supertypes do
       (let* ((package (assoc-default 'packageName super-entry))
              (super-sig (assoc-default 'signature super-entry))
              ;; Erase type arguments. This looks like "class List<String>".
              (friendly-super (replace-regexp-in-string "<[^<]*>" "" super-sig))
              (full-super (concat package "."
                                  (replace-regexp-in-string "^\\w+ " ""
                                                            friendly-super)))
              (is-interface (eclim--signature-has-keyword
                             super-sig "interface"))
              (methods (assoc-default 'methods super-entry))
              (required-methods nil))   ;; Eclim names here
         (loop
          for method across methods
          ;; Skip if specified name doesn't match.
          if (or (null name)
                 (string-match-p (format "\\_<%s(" (regexp-quote name)) method))
          do
          ;; This regexp stuff is how vim (and thus eclim) does it. Nothing
          ;; fancy. If it breaks, Google eclim/java/impl.vim for changes.
          (let ((name-for-eclim
                 ;; Remove keywords and return type. \_< begins identifier.
                 (replace-regexp-in-string "^\\s *[^(]*\\(\\_<[[:alnum:]_]+(\\)"
                                           "\\1"
                                           ;; Remove any and all type parameters.
                                           (replace-regexp-in-string "<[^<]*>" "" method)))
                ;; For the user, we have very different requirements. I like
                ;; knowing public and abstract, and the return type. I hate
                ;; packages -- I'm already implementing this class so I know.
                (friendly-name
                 ;; Packages are non-trivial to find (think Map.Entry) but
                 ;; if we stop at the first capitalized portion we're okay.
                 (replace-regexp-in-string "\\_<[[:lower:]][[:alnum:]_]+\\."
                                           "" method))
                (is-required (or is-interface (eclim--signature-has-keyword
                                               method "abstract"))))
            (let ((choice (format "%s [%s]" friendly-name friendly-super))
                  (data (list full-super name-for-eclim)))
              ;; This is probably overkill but what if our package erasing
              ;; resulted in duplicates? Use full name then. As in, really full.
              (when (gethash choice choice-data)
                (setq choice (format "%s [%s]" name-for-eclim full-super)))
              (cond (is-required (push choice choices))
                    ((member full-super '("java.lang.Object")) ; others like it?
                     (push choice choices-last))
                    (t (push choice choices-opt)))
              (puthash choice (list full-super name-for-eclim) choice-data)
              (when is-required (push name-for-eclim required-methods)))))
         ;; Since we don't allow multiple selection like Eclipse / vim, let's
         ;; provide for the cases that matter. Note that full non-abstract
         ;; overrides are typically a use case for *delegates*.
         (when (> (length required-methods) 1) ;; 1 method already there
           (let ((choice
                  (format "<all %d %s methods from %s>"
                          (length required-methods)
                          (cond (is-interface "missing") (name) (t "abstract"))
                          friendly-super))
                 (data (cons full-super (reverse required-methods))))
             (push choice choices) ;; I'll not worry about conflict here.
             (puthash choice data choice-data)))))
      ;; Keep inital order, except for our tweaks.
      (setq choices (append (nreverse choices) (reverse choices-opt)
                            (reverse choices-last)))
      (unless choices
        (if name (error "No such unimplemented method: %s" name) ;most likely
          (error "No candidates to implement"))) ;; Rare, given Object ancestor.

      ;; Ask user even if only one choice, for confirmation. Otherwise it's
      ;; possible to not even notice the change from a bad key combo. Unless
      ;; we were called programmatically a for specific method.
      (let ((choice
             (if (and name (eq 1 (length choices)))
                 (first choices)
               (funcall eclim-interactive-completion-function
                        "Implement: "
                        (mapcar #'eclim--colorize-signature choices)
                        nil t))))    ; require match
        (setq choice (substring-no-properties choice)) ; uncolorize
        (let* ((eclim-data (gethash choice choice-data))
               (super (car eclim-data)) (methods (cdr eclim-data))
               (methods-str (json-encode methods)))
          (eclim/with-results impl-result ("java_impl" "-p" "-f" "-o"
                                           ("-s" super) ("-m" methods-str))
            ;; eclim should give us a smaller list if it did something. But
            ;; it's probably not worth an error in case this changes.
            (revert-buffer t t t)))))))

(defun eclim-package-and-class ()
  (let ((package-name (eclim--java-current-package))
        (class-name   (eclim--java-current-class-name)))
    (if package-name (concat package-name "." class-name)
      class-name)))

(defun eclim-run-class ()
  "Run the current class."
  (interactive)
  (if (not (string= major-mode "java-mode"))
      (message "Sorry cannot run current buffer.")
    (compile (concat eclim-executable " -command java -p "  (eclim-project-name)
                     " -c " (eclim-package-and-class)))))

(defun eclim--java-junit-file (project file offset encoding)
  (concat eclim-executable
          " -command java_junit -p " project
          " -f " file
          " -o " (number-to-string offset)
          " -e " encoding))

(defun eclim--java-junit-project (project encoding)
  (concat eclim-executable
          " -command java_junit -p " project
          " -e " encoding))

(defun eclim--buffer-contains-substring (string)
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (search-forward string nil t))))

(defun eclim--java-make-popup-item (correction)
  (popup-make-item
   (cdr (assoc 'description correction))
   :value (cdr (assoc 'index correction))
   :document (cdr (assoc 'preview correction))))

(defun eclim-java-junit-buffer? ()
  (eclim--buffer-contains-substring "org.junit.Test"))

(defun eclim-java-testng-buffer? ()
  (eclim--buffer-contains-substring "org.testng.annotations.Test"))

(defun eclim-run-junit (project file offset encoding)
  "Run the current JUnit tests for current project or
current class or current method.

This method hooks onto the running Eclipse process and is thus
much faster than running mvn test -Dtest=TestClass#method."
  (interactive (list (eclim-project-name)
                     (eclim--project-current-file)
                     (eclim--byte-offset)
                     (eclim--current-encoding)))
  (if (not (string= major-mode "java-mode"))
      (message "Running JUnit tests only makes sense for Java buffers.")
    (compile (if (eclim-java-junit-buffer?)
                 (eclim--java-junit-file project file offset encoding)
               (eclim--java-junit-project project encoding)))))

(defun eclim-java-correct (line offset)
  (eclim/with-results correction-info ("java_correct" "-p" "-f" ("-l" line) ("-o" offset))
    (if (stringp correction-info)
        (message correction-info)
      (-if-let* ((corrections (cdr (assoc 'corrections correction-info)))
                 (cmenu (mapcar 'eclim--java-make-popup-item corrections))
                 (choice (popup-menu* cmenu)))
          (eclim/with-results correction-info
            ("java_correct"
             ("-p" (eclim-project-name))
             "-f"
             ("-l" line)
             ("-o" offset)
             ("-a" choice))
            ;; Problem updates can be distracting, but here the user was
            ;; actively trying to fix one.
            (eclim--problems-update-maybe))
        (message "No automatic corrections found. Sorry")))))

(defun eclim-java-show-documentation-for-current-element ()
  "Displays the doc comments for the element at the pointers position."
  (interactive)
  (let ((symbol (symbol-at-point)))
    (if symbol
        (let ((bounds (bounds-of-thing-at-point 'symbol))
              (window-config (current-window-configuration)))
          (eclim/with-results doc ("java_element_doc"
                                   ("-p" (eclim-project-name))
                                   "-f"
                                   ("-l" (- (cdr bounds) (car bounds)))
                                   ("-o" (save-excursion
                                           (goto-char (car bounds))
                                           (eclim--byte-offset))))

            (pop-to-buffer "*java doc*")
            (use-local-map eclim-java-show-documentation-map)

            (eclim--java-show-documentation-and-format doc)

            (message (substitute-command-keys
                      (concat
                       "\\[forward-button] - move to next link, "
                       "\\[backward-button] - move to previous link, "
                       "\\[eclim-quit-window] - quit")))))

      (message "No element found at point."))))


(defun eclim--java-show-documentation-and-format (doc &optional add-to-history)
  (make-local-variable 'eclim-java-show-documentation-history)
  (setq eclim-java-show-documentation-history
        (if add-to-history
            (push (buffer-substring (point-min) (point-max))
                  eclim-java-show-documentation-history)))

  (erase-buffer)
  (insert (cdr (assoc 'text doc)))

  (let ((links (cdr (assoc 'links doc)))
        link placeholder text href)
    (dotimes (i (length links))
      (setq link (aref links i))
      (setq text (cdr (assoc 'text link)))
      (setq href (cdr (assoc 'href link)))
      (setq placeholder (format "|%s[%s]|" text i))
      (goto-char (point-min))
      (while (search-forward placeholder nil t)
        (replace-match text)
        (make-text-button (match-beginning 0)
                          (+ (match-beginning 0) (length text))
                          'follow-link t
                          'action 'eclim-java-show-documentation-follow-link
                          'url href))))

  (when add-to-history
    (goto-char (point-max))
    (insert "\n\n")
    (insert-text-button "back" 'follow-link t 'action 'eclim--java-show-documentation-go-back))

  (goto-char (point-min)))


(defun eclim-java-show-documentation-follow-link (link)
  (interactive)
  (let ((url (button-get link 'url)))
    (if (string-match "^eclipse-javadoc" url)
        (eclim/with-results doc ("java_element_doc"
                                 ("-u" url))
          (eclim--java-show-documentation-and-format doc t))

      (if (string-match "^\.\." url)
          (let* ((doc-root-vars '(eclim-java-documentation-root
                                  eclim-java-android-documentation-root))
                 (path (replace-regexp-in-string "^[./]+" "" url))
                 (fullpath (cl-some (lambda (var)
                                      (let ((fullpath (concat (symbol-value var)
                                                              "/"
                                                              path)))
                                        (if (file-exists-p (replace-regexp-in-string
                                                            "#.+"
                                                            ""
                                                            fullpath))
                                         fullpath)))
                                    doc-root-vars)))
            (if fullpath
                (browse-url (concat "file://" fullpath))

              (message (concat "Can't find the root directory for this file: %s. "
                               "Are the applicable variables set properly? (%s)")
                       path
                       (mapconcat (lambda (var)
                                    (symbol-name var))
                                  doc-root-vars ", "))))

        (message "There is no handler for this kind of url yet. Implement it! : %s"
                 url)))))


(defun eclim--java-show-documentation-go-back (link)
  (erase-buffer)
  (insert (pop eclim-java-show-documentation-history))
  (goto-char (point-min)))

(provide 'eclim-java)
