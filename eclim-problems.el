(require 'popup)

(defgroup eclim-problems nil
  "Problems: settings for displaying the problems buffer and highlighting errors in code."
  :group 'eclim)

(defcustom eclim-problems-refresh-delay 0.5
  "The delay (in seconds) to wait before we refresh the problem list buffer after a file is saved."
  :group 'eclim-problems
  :type 'number)

(defcustom eclim-problems-resize-file-column t
  "Resizes file column in emacs-eclim problems mode"
  :group 'eclim-problems
  :type '(choice (const :tag "Off" nil)
                 (const :tag "On" t)))

(defcustom eclim-problems-show-pos nil
  "Shows problem line/column in emacs-eclim problems mode"
  :group 'eclim-problems
  :type '(choice (const :tag "Off" nil)
                 (const :tag "On" t)))

(defcustom eclim-problems-show-file-extension nil
  "Shows file extensions in emacs-eclim problems mode"
  :group 'eclim-problems
  :type '(choice (const :tag "Off" nil)
                 (const :tag "On" t)))

(defcustom eclim-problems-hl-errors t
  "Highlights errors in the problem list buffer"
  :group 'eclim-problems
  :type '(choice (const :tag "Off" nil)
                 (const :tag "On" t)))

(defcustom eclim-problems-suppress-highlights nil
  "When set, error and warning highlights are disabled in source files,
although counts are printed and they remain navigable. This is
designed to be made buffer-local (by user, not eclim) most of the
time, but it also works globally."
  :group 'eclim-problems
  :type '(choice (const :tag "Allow" nil)
                  (const :tag "Suppress" t)
                  (sexp :tag "Suppress when"
                        :value (lambda() 'for-example buffer-read-only))))

(defface eclim-problems-highlight-error-face
  '((t (:underline "red")))
  "Face used for highlighting errors in code"
  :group 'eclim-problems)

(defface eclim-problems-highlight-warning-face
  '((t (:underline "orange")))
  "Face used for highlighting errors in code"
  :group 'eclim-problems)

(defvar eclim-autoupdate-problems t)

(defvar eclim-problems-mode-hook nil)

(defvar eclim--problems-filter-description "")
(defvar eclim--problems-project nil) ;; problems are relative to this project
(defvar eclim--problems-file nil) ;; problems are relative to this file (when eclim--problems-filefilter is non-nil)

(setq eclim-problems-mode-map
      (let ((map (make-keymap)))
        (suppress-keymap map t)
        (define-key map (kbd "a") 'eclim-problems-show-all)
        (define-key map (kbd "e") 'eclim-problems-show-errors)
        (define-key map (kbd "g") 'eclim-problems-buffer-refresh)
        (define-key map (kbd "q") 'eclim-quit-window)
        (define-key map (kbd "w") 'eclim-problems-show-warnings)
        (define-key map (kbd "f") 'eclim-problems-toggle-filefilter)
        (define-key map (kbd "c") 'eclim-problems-correct)
        (define-key map (kbd "RET") 'eclim-problems-open-current)
        map))

(define-key eclim-mode-map (kbd "C-c C-e b") 'eclim-problems)
(define-key eclim-mode-map (kbd "C-c C-e o") 'eclim-problems-open)

(defvar eclim--problems-list nil)
(defvar eclim--problems-refreshing nil) ;; Set to true while refreshing probs.

(defvar eclim--problems-filter nil) ;; nil -> all problems, w -> warnings, e -> errors
(defvar eclim--problems-filefilter nil) ;; should filter by file name

(defconst eclim--problems-buffer-name "*eclim: problems*")
(defconst eclim--problems-compilation-buffer-name "*compilation: eclim*")

(defun eclim--problems-mode ()
  (kill-all-local-variables)
  (buffer-disable-undo)
  (setq major-mode 'eclim-problems-mode
        mode-name "eclim/problems"
        mode-line-process ""
        truncate-lines t
        buffer-read-only t
        default-directory (eclim/workspace-dir))
  (setq-local line-move-visual nil)
  (setq mode-line-format
        (list "-"
              'mode-line-mule-info
              'mode-line-modified
              'mode-line-frame-identification
              'mode-line-buffer-identification

              "   "
              'mode-line-position

              "  "
              'eclim--problems-filter-description

              "  "
              'mode-line-modes
              '(which-func-mode ("" which-func-format "--"))

              'global-mode-string
              "-%-"))
  (hl-line-mode t)
  (use-local-map eclim-problems-mode-map)
  (run-mode-hooks 'eclim-problems-mode-hook))

(defun eclim--problem-goto-pos (p)
  (save-restriction
    (widen)
    (goto-char (point-min))
    (forward-line (1- (assoc-default 'line p)))
    (dotimes (i (1- (assoc-default 'column p)))
      (forward-char))))

(defun eclim--problems-apply-filter (f)
  (setq eclim--problems-filter f)
  (eclim-problems-buffer-refresh))

(defun eclim-problems-show-errors ()
  (interactive)
  (eclim--problems-apply-filter "e"))

(defun eclim-problems-toggle-filefilter ()
  (interactive)
  (setq eclim--problems-filefilter (not eclim--problems-filefilter))
  (eclim--problems-buffer-redisplay))

(defun eclim-problems-show-warnings ()
  (interactive)
  (eclim--problems-apply-filter "w"))

(defun eclim-problems-show-all ()
  (interactive)
  (eclim--problems-apply-filter nil))

(defun eclim--problems-insert-highlight (problem)
  (save-excursion
    (eclim--problem-goto-pos problem)
    (let* ((id (eclim--java-identifier-at-point t t))
           (start (car id))
           (end (+ (car id) (length (cdr id)))))
      (let ((highlight (make-overlay start end (current-buffer) t t)))
        (overlay-put highlight 'face
                     (if (eq t (assoc-default 'warning problem))
                         'eclim-problems-highlight-warning-face
                       'eclim-problems-highlight-error-face))
        (overlay-put highlight 'category 'eclim-problem)
        (overlay-put highlight 'kbd-help (assoc-default 'message problem))))))


(defun eclim-problems-clear-highlights ()
  "Clears all eclim problem highlights in the current buffer. This is temporary
until the next refresh."
  (interactive)
  (remove-overlays nil nil 'category 'eclim-problem))


(defun eclim-problems-highlight ()
  "Inserts the currently active problem highlights in the current buffer,
if `eclim-problems-suppress-highlights' allows it."
  (interactive)
  (when (eclim--accepted-p (buffer-file-name))
    (save-restriction
      (widen)
      (eclim-problems-clear-highlights)
      (unless (if (functionp eclim-problems-suppress-highlights)
                  (funcall eclim-problems-suppress-highlights)
                eclim-problems-suppress-highlights)
        (loop for problem across (cl-remove-if-not (lambda (p) (string= (assoc-default 'filename p) (buffer-file-name))) eclim--problems-list)
              do (eclim--problems-insert-highlight problem))))))

(defadvice find-file (after eclim-problems-highlight-on-find-file activate)
  (eclim-problems-highlight))
(defadvice find-file-other-window (after eclim-problems-highlight-on-find-file-other-window activate)
  (eclim-problems-highlight))
(defadvice other-window (after eclim-problems-highlight-on-other-window activate)
  (eclim-problems-highlight))
(defadvice switch-to-buffer (after eclim-problems-highlight-switch-to-buffer activate)
  (eclim-problems-highlight))

(defun eclim--problems-get-current-problem ()
  (let ((buf (get-buffer "*eclim: problems*")))
    (if (eq buf (current-buffer))
        ;; we are in the problems buffer
        (let ((problems (eclim--problems-filtered))
              (index (1- (line-number-at-pos))))
          (if (>= index (length problems))
              (error "No problem on this line.")
            (aref problems index)))
      ;; we need to figure out which problem corresponds to this pos
      (save-restriction
        (widen)
        (let ((line (line-number-at-pos))
              (col (current-column)))
          (or (cl-find-if (lambda (p) (and (string= (assoc-default 'filename p) (file-truename buffer-file-name))
                                        (= (assoc-default 'line p) line)))
                       eclim--problems-list)
              (error "No problem on this line")))))))

(defun eclim-problems-open-current (&optional same-window)
  (interactive)
  (let* ((p (eclim--problems-get-current-problem)))
    (funcall (if same-window
                 'find-file
               'find-file-other-window)
             (assoc-default 'filename p))
    (eclim--problem-goto-pos p)))

(defun eclim-problems-correct ()
  "Pops up a suggestion for the current correction. This can be
invoked in either the problems buffer or a source code buffer."
  (interactive)
  (let ((p (eclim--problems-get-current-problem)))
    (unless (string-match "\\.\\(groovy\\|java\\)$" (cdr (assoc 'filename p)))
      (error "Not a Java or Groovy file. Corrections are currently supported only for Java or Groovy"))
    (if (eq major-mode 'eclim-problems-mode)
        (let ((p-buffer (find-file-other-window (assoc-default 'filename p))))
          (with-selected-window (get-buffer-window p-buffer t)
            ;; Intentionally DON'T save excursion. Often times we need edits.
            (eclim--problem-goto-pos p)
            (eclim-java-correct (cdr (assoc 'line p)) (eclim--byte-offset))))
      ;; source code buffer
      (eclim-java-correct (cdr (assoc 'line p)) (eclim--byte-offset)))))

(defmacro eclim--with-problems-list (problems &rest body)
  (declare (indent defun))
  "Utility macro to refresh the problem list and do operations on
it asynchronously."
  (let ((res (cl-gensym)))
    `(when eclim--problems-project
       (setq eclim--problems-refreshing t)
       (eclim/with-results-async ,res ("problems" ("-p" eclim--problems-project) (when (string= "e" eclim--problems-filter) '("-e" "true")))
         (loop for problem across ,res
               do (let ((filecell (assq 'filename problem)))
                    (when filecell (setcdr filecell (file-truename (cdr filecell))))))
         (setq eclim--problems-list ,res)
         (let ((,problems ,res))
           (setq eclim--problems-refreshing nil)
           ,@body)))))

(defun eclim-problems-buffer-refresh ()
  "Refresh the problem list and draw it on screen."
  (interactive)
  (eclim--with-problems-list problems
    (eclim--problems-buffer-redisplay)
    (if (not (minibuffer-window-active-p (minibuffer-window)))
        (if (string= "e" eclim--problems-filter)
            (message "Eclim reports %d errors." (length problems))
          (message "Eclim reports %d errors, %d warnings."
                   (length (cl-remove-if-not (lambda (p) (not (eq t (assoc-default 'warning p)))) problems))
                   (length (cl-remove-if-not (lambda (p) (eq t (assoc-default 'warning p))) problems)))))))

(defun eclim--problems-cleanup-filename (filename)
  (let ((x (file-name-nondirectory filename)))
    (if eclim-problems-show-file-extension x (file-name-sans-extension x))))

(defun eclim--problems-filecol-size ()
  (if eclim-problems-resize-file-column
      (min 40
           (apply #'max 0
                  (mapcar (lambda (problem)
                            (length (eclim--problems-cleanup-filename (assoc-default 'filename problem))))
                          (eclim--problems-filtered))))
    40))

(defun eclim--problems-update-filter-description ()
  (if eclim--problems-filefilter
      (if eclim--problems-filter
          (setq eclim--problems-filter-description (concat "(file-" eclim--problems-filter ")"))
        (setq eclim--problems-filter-description "(file)"))
    (if eclim--problems-filter
        (setq eclim--problems-filter-description (concat eclim--problems-project "(" eclim--problems-filter ")"))
      (setq eclim--problems-filter-description eclim--problems-project))))

(defun eclim--problems-buffer-redisplay ()
  "Draw the problem list on screen."
  (let ((buf (get-buffer "*eclim: problems*")))
    (when buf
      (with-current-buffer
        (set-buffer buf)
        (eclim--problems-update-filter-description)
        (save-excursion
          (dolist (b (mapcar #'window-buffer (window-list)))
            (set-buffer b)
            (eclim-problems-highlight)))
        (let ((inhibit-read-only t)
              (line-number (line-number-at-pos))
              (filecol-size (eclim--problems-filecol-size)))
          (erase-buffer)
          (loop for problem across (eclim--problems-filtered)
                do (eclim--insert-problem problem filecol-size))
          (goto-char (point-min))
          (forward-line (1- line-number)))))))

(defun eclim--problems-filtered ()
  "Filter reported problems by eclim.

It filters out problems using the ECLIM--PROBLEMS-FILEFILTER
criteria. If IGNORE-TYPE-FILTER is nil (default), then problems
are also filtered according to ECLIM--PROBLEMS-FILTER, i.e.,
error type. Otherwise, error type is ignored. This is useful when
other mechanisms, like compilation's mode
COMPILATION-SKIP-THRESHOLD, implement this feature."
  (eclim--filter-problems eclim--problems-filter eclim--problems-filefilter eclim--problems-file eclim--problems-list))

(defun eclim--warning-filterp (x)
  (eq t (assoc-default 'warning x)))

(defun eclim--error-filterp (x)
  (not (eclim--warning-filterp x)))

(defun eclim--choose-type-filter (type-filter)
  (cond
   ((not type-filter) '(lambda (_) t))
   ((string= "e" type-filter) 'eclim--error-filterp)
   (t 'eclim--warning-filterp)))

(defun eclim--choose-file-filter (file-filter file)
  (if (not file-filter)
      '(lambda (_) t)
    '(lambda (x) (string= (assoc-default 'filename x) file))))

(defun eclim--filter-problems (type-filter file-filter file problems)
  (let ((type-filterp (eclim--choose-type-filter type-filter))
        (file-filterp (eclim--choose-file-filter file-filter file)))
    (cl-remove-if-not (lambda (x) (and (funcall type-filterp x) (funcall file-filterp x))) problems)))

(defun eclim--insert-problem (problem filecol-size)
  (let* ((filecol-format-string (concat "%-" (number-to-string filecol-size) "s"))
         (problem-new-line-pos (cl-position ?\n (assoc-default 'message problem)))
         (problem-message
          (if problem-new-line-pos
              (concat (substring (assoc-default 'message problem)
                                 0 problem-new-line-pos)
                      "...")
            (assoc-default 'message problem)))
         (filename (truncate-string-to-width
                    (eclim--problems-cleanup-filename (assoc-default 'filename problem))
                    40 0 nil t))
         (text (if eclim-problems-show-pos
                   (format (concat filecol-format-string
                                   " | line %-12s"
                                   " | %s")
                           filename
                           (assoc-default 'line problem)
                           problem-message)
                 ;; else
                 (format (concat filecol-format-string
                                 " | %s")
                         filename
                         problem-message))))
    (when (and eclim-problems-hl-errors (eq :json-false (assoc-default 'warning problem)))
      (put-text-property 0 (length text) 'face 'bold text))
    (insert text)
    (insert "\n")))

(defun eclim--get-problems-buffer ()
  "Return the eclim problems buffer, if it exists. Otherwise,
create and initialize a new buffer."
  (or (get-buffer "*eclim: problems*")
      (let ((buf (get-buffer-create "*eclim: problems*")))
        (save-excursion
          ;; (setq eclim--problems-project (eclim-project-name))
          (setq eclim--problems-file buffer-file-name)
          (set-buffer buf)
          (eclim--problems-mode)
          ;;(eclim-problems-buffer-refresh)
          (goto-char (point-min))))))

(defun eclim--problems-mode-init (&optional quiet)
  "Create and initialize the eclim problems buffer. If the
argument QUIET is non-nil, open the buffer in the background
without switching to it."
  (let ((buf (get-buffer-create "*eclim: problems*")))
    (save-excursion
      (setq eclim--problems-project (eclim-project-name))
      (setq eclim--problems-file buffer-file-name)
      (set-buffer buf)
      (eclim--problems-mode)
      (eclim-problems-buffer-refresh)
      (goto-char (point-min)))
    (if (not quiet)
        (switch-to-buffer buf))))

(defun eclim-problems ()
  "Show current compilation problems in a separate window."
  (interactive)
  (if (eclim-project-name)
      (eclim--problems-mode-init)
    (error "Could not figure out the current project. Is this an eclim managed buffer?")))

(defun eclim-problems-open ()
  "Opens a new (emacs) window inside the current frame showing the current project compilation problems"
  (interactive)
  (let ((w (selected-window)))
    (select-window (split-window nil (round (* (window-height w) 0.75)) nil))
    (eclim-problems)
    (select-window w)))

(add-hook 'find-file-hook
          (lambda () (when (and (eclim--accepted-p (buffer-file-name))
                                (not (get-buffer eclim--problems-buffer-name)))
                       (eclim--problems-mode-init t))))

(defun eclim-problems-refocus ()
  (interactive)
  (when (eclim--project-dir)
    (setq eclim--problems-project (eclim-project-name))
    (setq eclim--problems-file buffer-file-name)
    (with-current-buffer eclim--problems-buffer-name
      (eclim-problems-buffer-refresh))))

(defun eclim-problems-next (&optional same-window)
  (interactive)
  (let ((prob-buf (get-buffer eclim--problems-buffer-name)))
    (when prob-buf
      (set-buffer prob-buf)
      (if (boundp 'eclim--problems-list-at-first)
          (setq eclim--problems-list-at-first nil)
        (forward-line 1))
      (hl-line-move hl-line-overlay)
      (eclim-problems-open-current same-window))))

(defun eclim-problems-previous (&optional same-window)
  (interactive)
  (let ((prob-buf (get-buffer eclim--problems-buffer-name)))
    (when prob-buf
      (set-buffer prob-buf)
      (forward-line -1)
      (hl-line-move hl-line-overlay)
      (eclim-problems-open-current same-window))))

(defun eclim-problems-next-same-window ()
  (interactive)
  (eclim-problems-next t))

(defun eclim-problems-previous-same-window ()
  (interactive)
  (eclim-problems-previous t))

(defun eclim--problems-update-maybe ()
  "If autoupdate is enabled, this function triggers a delayed
refresh of the problems buffer."
  (when (and (not eclim--is-completing)
             (eclim--project-dir)
             eclim-autoupdate-problems)
    (setq eclim--problems-project (eclim-project-name))
    (setq eclim--problems-file buffer-file-name)
    (run-with-idle-timer eclim-problems-refresh-delay nil (lambda () (eclim-problems-buffer-refresh)))))

(defun eclim-problems-compilation-buffer ()
  "Creates a compilation buffer from eclim error messages. This
is convenient as it lets the user navigate between errors using
`next-error' (\\[next-error])."
  (interactive)
  (lexical-let ((filecol-size (eclim--problems-filecol-size))
                (project-directory (concat (eclim--project-dir) "/"))
                (compil-buffer (get-buffer-create eclim--problems-compilation-buffer-name))
                (project-name (eclim-project-name))) ; To store it in buffer.

    (with-current-buffer compil-buffer
      (setq default-directory project-directory)
      (setq mode-line-process
            (concat ": " (propertize "refreshing"
                                     'face 'compilation-mode-line-run))))
    ;; Remember that the part below is asynchronous. This can be tricky.
    (eclim--with-problems-list problems
      (let (saved-user-pos)
        (with-current-buffer compil-buffer
          (buffer-disable-undo)
          (setq buffer-read-only nil)
          (setq saved-user-pos (point))
          (erase-buffer)
          (let ((errors 0) (warnings 0))
            (loop for problem across (eclim--problems-filtered) do
                  (eclim--insert-problem-compilation
                   problem filecol-size project-directory)
                (if (eq t (assoc-default 'warning problem)) ; :json-false, WTH
                    (setq warnings (1+ warnings))
                  (setq errors (1+ errors))))
            (let ((msg (format
                        "Compilation results: %d errors, %d warnings [%s].\n"
                        errors warnings (current-time-string))))
              (insert "\n" msg)
              (goto-char (point-min))
            (insert msg "\n"))
            (compilation-mode)
            ;; The above killed local variables, so recover our lexical-lets
            (setq default-directory project-directory)
            (setq eclim--project-name project-name)
            ;; Remap the very dangerous "g" command :)  A make -k in some of
            ;; my projects would throw Eclipse off-balance by cleaning .classes.
            ;; May look funky, but it's safe.
            (local-set-key "g" 'eclim-problems-compilation-buffer)

            (setq mode-line-process
                  (concat ": "
                          (propertize (format "%d/%d" errors warnings)
                                      'face (when (> errors 0)
                                              'compilation-mode-line-fail))))))
        ;; Sometimes, buffer was already current. Note outside with-current-buf.
        (unless (eq compil-buffer (current-buffer))
          (display-buffer compil-buffer 'other-window))
        (with-selected-window (get-buffer-window compil-buffer t)
          (when (< saved-user-pos (point-max))
            (goto-char saved-user-pos)))))))


(defun eclim--insert-problem-compilation (problem filecol-size project-directory)
  (let ((filename (first (split-string (assoc-default 'filename problem) project-directory t)))
        (description (assoc-default 'message problem))
        (type (if (eq t (assoc-default 'warning problem)) "W" "E")))
    (let ((line (assoc-default 'line problem))
          (col (assoc-default 'column problem)))
      (insert (format "%s:%s:%s: %s: %s\n" filename line col (upcase type) description)))))

(defun eclim--count-current-errors ()
  (length
   (eclim--filter-problems "e" t (buffer-file-name (current-buffer)) eclim--problems-list)))

(defun eclim--count-current-warnings ()
  (length
   (eclim--filter-problems "w" t (buffer-file-name (current-buffer)) eclim--problems-list)))

(defun eclim-problems-next-same-file (&optional up)
  "Moves to the next problem in the current file, with wraparound. If UP
or prefix arg, moves to previous instead; see `eclim-problems-prev-same-file'."
  (interactive "P")
  ;; This seems pretty inefficient, but it's fast enough. Would be even
  ;; more inefficient if we didn't assume problems were sorted.
  (let ((problems-file
         (eclim--filter-problems nil t (buffer-file-name (current-buffer))
                                 eclim--problems-list))
        (pass-line (line-number-at-pos))
        (pass-col (+ (current-column) (if up 0 1)))
        (first-passed nil) (last-not-passed nil))
    (when (= 0 (length problems-file)) (error "No problems in this file"))
    (loop for p across problems-file until first-passed do
          (let ((line (assoc-default 'line p))
                (col (assoc-default 'column p)))
            (if (or (> line pass-line)
                      (and (= line pass-line) (> col pass-col)))
                (setq first-passed p)
              (setq last-not-passed p))))
    (eclim--problem-goto-pos
     (or
      (if up last-not-passed first-passed)
      (when up (message "Moved past first error, continuing to last")
            (elt problems-file (- (length problems-file) 1))) ; Ugh, vector
      (progn (message "Moved past last error, continuing to first")
             (elt problems-file 0))))))

(defun eclim-problems-prev-same-file ()
  "Moves to the previous problem in the same file, with wraparound."
  (interactive)
  (eclim-problems-next-same-file t))


(defun eclim-problems-modeline-string ()
  "Returns modeline string with additional info about
problems for current file"
  (concat (format ": %s/%s"
                  (eclim--count-current-errors)
                  (eclim--count-current-warnings))
          (when eclim--problems-refreshing "*")))

(provide 'eclim-problems)
