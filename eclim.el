;; eclim.el --- an interface to the Eclipse IDE.
;;
;; Copyright (C) 2009  Tassilo Horn <tassilo@member.fsf.org>
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
;;  - Nikolaj Schumacher <bugs * nschum de>
;;  - Yves Senn <yves senn * gmx ch>
;;  - Fredrik Appelberg <fredrik * bitbakery se>
;;  - Alessandro Arzilli <alessandro.arzilli * gmail com>
;;
;;; Conventions
;;
;; Conventions used in this file: Name internal variables and functions
;; "eclim--<descriptive-name>", and name eclim command invocations
;; "eclim/command-name", like eclim/project-list.

;;* Eclim

(eval-when-compile (require 'cl))

;;** Basics

(defgroup eclim nil
  "Interface to the Eclipse IDE."
  :group 'tools)

(defcustom eclim-eclipse-dirs '("/Applications/eclipse" "/usr/lib/eclipse"
                                "/usr/local/lib/eclipse" "/usr/share/eclipse")
  "Path to the eclipse directory"
  :type '(sexp)
  :group 'eclim)

(defun eclim-executable-find ()
  (let (file)
    (dolist (eclipse-root eclim-eclipse-dirs)
      (and (file-exists-p
            (setq file (expand-file-name "plugins" eclipse-root)))
           (setq file (car (last (directory-files file t "^org.eclim_"))))
           (file-exists-p (setq file (expand-file-name "bin/eclim" file)))
           (return file)))))

(defcustom eclim-interactive-completion-function (if ido-mode 'ido-completing-read 'completing-read)
  "Defines a function which is used by eclim to complete a list of
choices interactively."
  :group 'eclim
  :type 'function)

(defcustom eclim-executable
  (or (executable-find "eclim") (eclim-executable-find))
  "Location of eclim executable."
  :group 'eclim
  :type 'file)

(defcustom eclim-auto-save t
  "Determines whether to save the buffer when retrieving completions.
eclim can only complete correctly when the buffer has been
saved."
  :group 'eclim
  :type '(choice (const :tag "Off" nil)
                 (const :tag "On" t)))

(defcustom eclim-use-yasnippet t
  "Determines whether the eclim snippets get turned on or off"
  :group 'eclim
  :type '(choice (const :tag "Off" nil)
                 (const :tag "On" t)))

(defcustom eclim-print-debug-messages nil
  "Determines whether debug messages should be printed."
  :group 'eclim
  :type '(choice (const :tag "Off" nil)
                 (const :tag "On" t)))

(defcustom eclim-limit-search-results t
  "Determines if code search results should be limited to files
  in the current workspace."
  :group 'eclim
  :type '(choice (const :tag "Off" nil)
                 (const :tag "On" t)))

(defvar eclim--snippet-directory
  (concat (file-name-directory load-file-name) "snippets"))

;; (defvar eclim--project-dir nil)
;; (make-variable-buffer-local 'eclim--project-dir)

(defvar eclim--project-name nil)
(make-variable-buffer-local 'eclim--project-name)

(defvar eclim--project-natures-cache nil)
(defvar eclim--projects-cache nil)

(defvar eclim--file-coding-system-mapping
  '(("undecided-dos" . "iso-8859-1")
    ("dos" . "iso-8859-1")
    ("undecided-unix" . "iso-8859-1")
    ("utf-8-dos" . "utf-8")
    ("utf-8-unix" . "utf-8")
    ("utf-8-emacs-unix" . "utf-8")))

(defvar eclim--compressed-urls-regexp "\\(^jar:file://\\)\\|\\(^zip://\\)")
(defvar eclim--compressed-file-path-replacement-regexp "\\\\")
(defvar eclim--compressed-file-path-removal-regexp "^/")

(defun string-startswith-p (string prefix)
  (eq t (compare-strings string nil (string-width prefix) prefix nil nil)))

(defun string-endswith-p (string prefix)
  (let* ((w (string-width string))
         (s (- w (string-width prefix))))
    (when (wholenump s) (eq t (compare-strings string (- w (string-width prefix)) w prefix nil nil)))))

;; TODO: this function is probably not really needed anymore
(defun eclim--build-command (command &rest args)
  (cons command
        (loop for a = args then (rest (rest a))
              for arg = (first a)
              for val = (second a)
              while arg when val append (list arg val))))

(defun eclim--make-command (args)
  "Creates a command string that can be executed from the
shell. The first element in ARGS is the name of the eclim
operation, and the rest are flags/values to be passed on to
eclimd."
  (let ((cmd (apply 'concat eclim-executable " -command "
                    (first args) " "
                    (loop for a = (rest args) then (rest (rest a))
                          for arg = (first a)
                          for val = (second a)
                          while arg when val collect (concat arg " " (if (numberp val) (number-to-string val) val) " ")))))
    (if eclim-print-debug-messages (message "Executing: %s" cmd))
    cmd))

(defun eclim--parse-result (result)
  "Handles the text result of an eclim operation by splitting it
into a list lines (removing empty elements). Also performs some
elementary error checking."
  (when (and result (or (string-match "connect:\s+\\(.*\\)" result)
                        (string-match "Missing argument for required option:\s*\\(.*\\)" result)))
    (error (match-string 1 (first result))))
  (json-read-from-string result))

(defun eclim--call-process (&rest args)
  "Calls eclim with the supplied arguments. Consider using
`eclim/execute-command' instead, as it has argument expansion,
error checking, and some other niceties.."
  (eclim--parse-result
   (shell-command-to-string (eclim--make-command args))))

(defvar eclim--async-buffers nil
  "Holds a list of available buffers for making async calls. We
  try to reuse them as much as possible.")

(defun eclim--get-async-buffer ()
  "Get an buffer from ECLIM--ASYNC-BUFFERS, or create a new one
if there are no unused ones."
  (if eclim--async-buffers (let ((buf (pop eclim--async-buffers)))
                            (if (buffer-name buf)
                                (save-excursion
                                  (set-buffer buf)
                                  (erase-buffer)
                                  buf)
                              (eclim--get-async-buffer)))
    (get-buffer-create (generate-new-buffer-name "*eclim-async*"))))

(defun eclim--call-process-async (callback &rest args)
  "Like `eclim--call-process', but the call is executed
asynchronously. CALLBACK is a function that accepts a list of
strings and will be called on completion."
    (lexical-let ((handler callback)
                  (buf (eclim--get-async-buffer)))
      (when eclim-print-debug-messages (message "Using async buffer %s" buf))
      (let ((proc (start-process-shell-command "eclim" buf (eclim--make-command args))))
        (let ((sentinel (lambda (process signal)
                          (save-excursion
                            (set-buffer (process-buffer process))
                            (funcall handler (eclim--parse-result (buffer-substring 1 (point-max))))
                            (push buf eclim--async-buffers)))))
          (set-process-sentinel proc sentinel)))))

(setq eclim--default-args
      '(("-n" . (eclim--project-name))
        ("-p" . (eclim--project-name))
        ("-e" . (eclim--current-encoding))
        ("-f" . (eclim--project-current-file))
        ("-o" . (eclim--byte-offset))
        ("-s" . "project")))

(defun eclim--args-contains (args flags)
  "Check if an (unexpanded) ARGS list contains any of the
specified FLAGS."
  (loop for f in flags
        return (find f args :test #'string= :key (lambda (a) (if (listp a) (car a) a)))))

(defun eclim--expand-args (args)
  "Takes a list of command-line arguments with which to call the
eclim server. Each element should be either a string or a
list. If it is a string, its default value is looked up in
`eclim--default-args' and used to construct a list. The argument
lists are then appended together."
  (mapcar (lambda (a) (if (numberp a) (number-to-string a) a))
          (loop for a in args
                append (if (listp a)
                           (list (car a) (eval (cadr a)))
                         (list a (eval (cdr (or (assoc a eclim--default-args)
                                                (error "sorry, no default value for: %s" a)))))))))

(defun eclim--execute-command-internal (executor cmd args)
  (lexical-let* ((expargs (eclim--expand-args args))
                 (sync (eclim--args-contains args '("-f" "-o")))
                 (check (eclim--args-contains args '("-p"))))
    (when sync (eclim/java-src-update))
    (when check
      (ignore-errors
        (eclim--check-project (if (listp check) (eval (second check)) (eclim--project-name)))))
    (let ((attrs-before (if sync (file-attributes (buffer-file-name)) nil)))
      (funcall executor (cons cmd expargs)
               (lambda ()
                 (when sync
                   (when (and (file-exists-p (buffer-file-name))
                              attrs-before
                              (not (= (second (sixth attrs-before))
                                      (second (sixth (file-attributes (buffer-file-name)))))))
                        (revert-buffer t t t))))))))

(defmacro eclim/execute-command (cmd &rest args)
  "Calls `eclim--expand-args' on ARGS, then calls eclim with the
results. Automatically saves the current buffer (and optionally
other java buffers as well), performs an eclim java_src_update
operation, and refreshes the current buffer if necessary. Raises
an error if the connection is refused. Automatically calls
`eclim--check-project' if neccessary."
  `(eclim--execute-command-internal
    (lambda (command-line cleaner)
      (let ((res (apply 'eclim--call-process command-line)))
        (funcall cleaner)
        res))
    ,cmd ',args))

(defmacro eclim/execute-command-async (callback cmd &rest args)
  "Calls `eclim--expand-args' on ARGS, then calls eclim with the
results. Automatically saves the current buffer (and optionally
other java buffers as well), performs an eclim java_src_update
operation, and refreshes the current buffer if necessary. Raises
an error if the connection is refused. Automatically calls
`eclim--check-project' if neccessary."
  `(eclim--execute-command-internal
    (lambda (command-line cleaner)
      (lexical-let ((cleaner cleaner))
        (apply 'eclim--call-process-async
         (lambda (res)
           (funcall cleaner)
           (funcall ,callback res))
         command-line)))
    ,cmd ',args))

(defun eclim--running-p ()
  "Returns t if eclim is currently capable of receiving commands,
nil otherwise."
  (ignore-errors
    (eclim/execute-command "ping")
    t))

(defmacro eclim/with-results (result params &rest body)
  "Convenience macro. PARAMS is a list where the first element is
CMD to execute and the rest an ARGS list. Calls eclim with CMD
and the expanded ARGS list and binds RESULT to the results. If
RESULT is non-nil, BODY is executed."
  (declare (indent defun))
  (let ((sync (eclim--args-contains (rest params) (list "-f" "-o"))))
    `(let* ((,result (eclim/execute-command ,@params))
            (eclim-auto-save (and eclim-auto-save (not ,sync))))
       (when ,result
         ,@body))))

(defmacro eclim/with-results-async (result params &rest body)
  "Convenience macro. PARAMS is a list where the first element is
CMD to execute and the rest an ARGS list. Calls eclim with CMD
and the expanded ARGS list and binds RESULT to the results. If
RESULT is non-nil, BODY is executed."
  (declare (indent defun))
  (let ((sync (eclim--args-contains (rest params) (list "-f" "-o"))))
    `(eclim/execute-command-async
        (lambda (,result)
          (let ((eclim-auto-save (and eclim-auto-save (not ,sync))))
            (when ,result ,@body)))
          ,@params)))

(defun eclim--completing-read (prompt choices)
  (funcall eclim-interactive-completion-function prompt choices))

(defun eclim--file-managed-p (&optional filename)
  "Return t if and only if this file is part of a project managed
by eclim. If the optional argument FILENAME is given, the return
value is computed for that file's instead."
  (ignore-errors
    (let ((file (or filename buffer-file-name)))
      (and file
           (file-exists-p file)
           (eclim--project-name file)))))

(defun eclim--project-dir (&optional filename)
  "Return this file's project root directory. If the optional
argument FILENAME is given, return that file's project root directory."
  (let ((root (locate-dominating-file (or filename buffer-file-name) ".project")))
    (when root
      (directory-file-name
       (file-name-directory
        (expand-file-name root))))))

(defun eclim--project-name (&optional filename)
  "Returns this file's project name. If the optional argument
FILENAME is given, return that file's  project name instead."
  (or eclim--project-name
      (setq eclim--project-name
            (let ((project-list (eclim/project-list))
                  (project-dir (eclim--project-dir (or filename buffer-file-name))))
              (when (and project-list project-dir)
                (car (or (cddr (assoc project-dir project-list)) ;; sensitive match
                         (cddr (assoc (downcase project-dir) ;; insensitive match
                                      (mapcar (lambda (project)
                                                (cons (downcase (first project))
                                                      (rest project)))
                                              project-list))))))))))

(defun eclim--find-file (path-to-file)
  (if (not (string-match-p "!" path-to-file))
      (unless (string= path-to-file (buffer-file-name))
        (find-file-other-window path-to-file))
    (let* ((parts (split-string path-to-file "!"))
           (archive-name (replace-regexp-in-string eclim--compressed-urls-regexp "" (first parts)))
           (file-name (second parts)))
      (find-file-other-window archive-name)
      (beginning-of-buffer)
      (re-search-forward (replace-regexp-in-string
                          eclim--compressed-file-path-removal-regexp ""
                          (regexp-quote (replace-regexp-in-string
                                         eclim--compressed-file-path-replacement-regexp
                                         "/" file-name))))
      (let ((old-buffer (current-buffer)))
        (archive-extract)
        (beginning-of-buffer)
        (kill-buffer old-buffer)))))

(defun eclim--find-display-results (pattern results &optional open-single-file)
  (let ((res (remove-if-not (lambda (r) (or (not eclim-limit-search-results) (eclim--accepted-p (car r))))
                            (remove-if (lambda (r) (zerop (length (remove-if (lambda (r) (zerop (length r))) r)))) results))))
    (if (and (= 1 (length res)) open-single-file) (eclim--visit-declaration (car res))
      (pop-to-buffer (get-buffer-create "*eclim: find"))
      (let ((buffer-read-only nil))
        (erase-buffer)
        (insert (concat "-*- mode: eclim-find; default-directory: " default-directory " -*-"))
        (newline 2)
        (insert (concat "eclim java_search -p " pattern))
        (newline)
        (dolist (result res)
          (insert (eclim--convert-find-result-to-string result default-directory))
          (newline))
        (grep-mode)))))

(defun eclim--convert-find-result-to-string (line &optional directory)
  (let ((converted-directory (replace-regexp-in-string "\\\\" "/" (car line))))
    (concat (if converted-directory
                (replace-regexp-in-string (concat (regexp-quote directory) "/?") "" converted-directory)
              converted-directory)
            ":" (replace-regexp-in-string " col " ":" (second line)) " "
            (third line))))

(defun eclim--visit-declaration (eclim-response)
  (let* ((file-name (car eclim-response))
         (line-and-column (cadr eclim-response))
         (position (split-string line-and-column " col ")))
    (eclim--find-file file-name)
    (goto-line (string-to-number (car position)))
    (move-to-column (- (string-to-number (cadr position)) 1))))

(defun eclim--string-strip (content)
  (replace-regexp-in-string "\s*$" "" content))

(defun eclim--project-current-file ()
  (file-relative-name buffer-file-name (eclim--project-dir)))

(defun eclim--byte-offset (&optional text)
  ;; TODO: restricted the ugly newline counting to dos buffers => remove it all the way later
  (let ((current-offset (1-(position-bytes (point)))))
    (when (not current-offset) (setq current-offset 0))
    (if (string-match "dos" (symbol-name buffer-file-coding-system))
        (+ current-offset (how-many "\n" (point-min) (point)))
      current-offset)))

(defun eclim--current-encoding ()
  (let* ((coding-system (symbol-name buffer-file-coding-system))
         (mapped-coding-system (cdr (assoc
                                     coding-system
                                     eclim--file-coding-system-mapping))))
    (if mapped-coding-system mapped-coding-system coding-system)))

(defun eclim/workspace-dir ()
  (eclim--call-process "workspace_dir"))

(defun eclim/jobs (&optional family)
  (eclim--call-process (eclim--build-command "jobs"
                                             "-f" family)))

(defun eclim-complete ()
  (interactive)
  ;; TODO build context sensitive completion mechanism
  (eclim-java-complete))

;;** The minor mode and its keymap

(defvar eclim-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-TAB") 'eclim-complete)
    map)
  "The keymap used in `eclim-mode'.")

(defvar eclim-mode-hook nil)

(define-minor-mode eclim-mode
  "An interface to the Eclipse IDE."
  nil
  " Eclim"
  eclim-mode-map
  (if eclim-mode
      (progn
        (when (and (featurep 'yasnippet) eclim-use-yasnippet)
          (yas/load-directory eclim--snippet-directory)))
    (kill-local-variable 'eclim--project-dir)
    (kill-local-variable 'eclim--project-name)))

(defcustom eclim-accepted-file-regexps
  '("")
  "List of regular expressions that are matched against filenames
to decide if eclim should be automatically started on a
particular file. By default all files part of a project managed
by eclim can be accepted (see `eclim--accepted-filename' for more
information). It is nevertheless possible to restrict eclim to
some files by changing this variable. For example, a value
of (\"\\\\.java\\\\'\" \"build\\\\.xml\\\\'\") can be used to restrict
the use of eclim to java and ant files."
  :group 'eclim
  :type '(repeat regexp))

(defun eclim--accepted-filename-p (filename)
  "Return t if and only one of the regular expressions in
`eclim-accepted-file-regexps' matches FILENAME."
  (if (member-if
       (lambda (regexp) (string-match regexp filename))
       eclim-accepted-file-regexps)
      t))


(defun eclim--accepted-p (filename)
  "Return t if and only if eclim should be automatically started on filename."
  (and
   filename
   (eclim--running-p)
   (eclim--file-managed-p filename)
   (eclim--accepted-filename-p filename)))

;; Request an eclipse source update when files are saved
(defun eclim--after-save-hook ()
  (when (eclim--accepted-p (buffer-file-name))
    (ignore-errors
      (apply 'eclim--call-process "java_src_update" (eclim--expand-args (list "-p" "-f")))))
  t)

(add-hook 'after-save-hook 'eclim--after-save-hook)


(define-globalized-minor-mode global-eclim-mode eclim-mode
  (lambda ()
    (if (and buffer-file-name
             (eclim--running-p)
             (eclim--project-dir buffer-file-name))
        (eclim-mode 1))))

(require 'eclim-project)
(require 'eclim-java)
(require 'eclim-ant)
(require 'eclim-maven)
(require 'eclim-problems)

(provide 'eclim)
