(defcustom eclim-problems-resize-file-column t
  "Resizes file column in emacs-eclim problems mode"
  :group 'eclim
  :type '(choice (const :tag "Off" nil)
		 (const :tag "On" t)))

(defcustom eclim-problems-show-pos nil
  "Shows problem line/column in emacs-eclim problems mode"
  :group 'eclim
  :type '(choice (const :tag "Off" nil)
		 (const :tag "On" t)))

(defcustom eclim-problems-show-file-extension nil
  "Shows file extensions in emacs-eclim problems mode"
  :group 'eclim
  :type '(choice (const :tag "Off" nil)
		 (const :tag "On" t)))

(defcustom eclim-problems-hl-errors t
  "Highlights errors"
  :group 'eclim
  :type '(choice (const :tag "Off" nil)
		 (const :tag "On" t)))

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
	(define-key map (kbd "q") 'quit-window)
	(define-key map (kbd "w") 'eclim-problems-show-warnings)
	(define-key map (kbd "f") 'eclim-problems-toggle-filefilter)
	(define-key map (kbd "RET") 'eclim-problems-open-current)
	map))

(define-key eclim-mode-map (kbd "C-c C-e b") 'eclim-problems)
(define-key eclim-mode-map (kbd "C-c C-e o") 'eclim-problems-open)

(defvar eclim--problems-list nil)

(defvar eclim--problems-filter nil) ;; nil -> all problems, w -> warnings, e -> errors
(defvar eclim--problems-filefilter nil) ;; should filter by file name

(defvar eclim--problems-faces
  '(("e" foreground-color . "red")
    ("w" foreground-color . "yellow")))

(defconst eclim--problems-buffer-name "*eclim: problems*")

(defun eclim--problems-mode ()
  (kill-all-local-variables)
  (buffer-disable-undo)
  (setq majod-mode 'eclim-problems-mode
	mode-name "eclim/problems"
	mode-line-process ""
	truncate-lines t
	line-move-visual nil
	buffer-read-only t
	default-directory (eclim/workspace-dir))
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

(defun eclim--problems ()
  "Calls eclipse to obtain all current problems. Returns a list of lists."
  (remove-if-not (lambda (l) (= (length l) 4)) ;; for now, ignore multiline errors
		 (mapcar (lambda (line) (split-string line "|" nil))
			 (eclim--call-process "problems" 
					      "-p" eclim--problems-project))))

(defun eclim--problem-file (p) (first p))
(defun eclim--problem-pos (p) (second p))
(defun eclim--problem-description (p) (third p))
(defun eclim--problem-type (p) (fourth p))

(defun eclim--problems-apply-filter (f)
  (setq eclim--problems-filter f)
  (eclim--problems-buffer-redisplay))

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

(defun eclim-problems-open-current ()
  (interactive)
  (let* ((p (nth (1- (line-number-at-pos)) (eclim--problems-filtered)))
	 (pos (split-string (eclim--problem-pos p) " col ")))
    (find-file-other-window (eclim--problem-file p))
    (goto-line (string-to-int (first pos)))
    (beginning-of-line)
    (dotimes (i (1- (string-to-int (second pos))))
      (forward-char))))

(defun eclim-problems-buffer-refresh ()
  "Refresh the problem list and draw it on screen."
  (interactive)
  (setq eclim--problems-list (eclim--problems))
  (eclim--problems-buffer-redisplay)
  ;; (when (not (current-message))
  (message "Eclim reports %d errors, %d warnings."
	   (length (remove-if-not (lambda (p) (string-equal "e" (eclim--problem-type p))) eclim--problems-list))
	     (length (remove-if-not (lambda (p) (string-equal "w" (eclim--problem-type p))) eclim--problems-list))))

(defun eclim--problems-cleanup-filename (filename)
  (let ((x (file-name-nondirectory (eclim--problem-file problem))))
    (if eclim-problems-show-file-extension x (file-name-sans-extension x))))

(defun eclim--problems-filecol-size ()
  (if eclim-problems-resize-file-column
      (min 40
	   (apply #'max 0
		  (mapcar (lambda (problem)
			    (length (eclim--problems-cleanup-filename (eclim--problem-file problem))))
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
  (eclim--problems-update-filter-description)
  (let ((inhibit-read-only t)
	(line-number (line-number-at-pos))
	(filecol-size (eclim--problems-filecol-size)))
    (erase-buffer)
    (dolist (problem (eclim--problems-filtered))
      (eclim--insert-problem problem filecol-size))
    (goto-line line-number)))

(defun eclim--problems-filtered ()
  (remove-if-not 
   (lambda (x) (and
		(or (not eclim--problems-filefilter)
		    (string= (eclim--problem-file x) eclim--problems-file))
		(or (not eclim--problems-filter) 
		    (string= (eclim--problem-type x) eclim--problems-filter))))
   eclim--problems-list))

(defun eclim--insert-problem (problem filecol-size)
  (let* ((filecol-format-string (concat "%-" (number-to-string filecol-size) "s"))
	 (filename (truncate-string-to-width (eclim--problems-cleanup-filename (eclim--problem-file problem))
					     40 0 nil t))
	 (text (if eclim-problems-show-pos
		   (format (concat filecol-format-string
				   " | %-12s"
				   " | %s")
			   filename
			   (eclim--problem-pos problem)
			   (eclim--problem-description problem))
		 ;; else
		 (format (concat filecol-format-string
				 " | %s")
			 filename
			 (eclim--problem-description problem)))))
    (when (and eclim-problems-hl-errors (string= (eclim--problem-type problem) "e"))
      (put-text-property 0 (length text) 'face 'bold text))
    (insert text)
    (insert "\n")))

(defun eclim--problems-mode-init ()
  (setq eclim--problems-project (eclim--project-name))
  (setq eclim--problems-file buffer-file-name)
  (switch-to-buffer (get-buffer-create "*eclim: problems*"))
  (eclim--problems-mode)
  (eclim-problems-buffer-refresh) 
  (beginning-of-buffer))

(defun eclim-problems ()
  "Show current compilation problems in a separate window."
  (interactive)
  (eclim--problems-mode-init))

(defun eclim-problems-open ()
  "Opens a new (emacs) window inside the current frame showing the current project compilation problems"
  (interactive)
  (let ((w (selected-window)))
    (select-window (split-window nil (round (* (window-height w) 0.75)) nil))
    (eclim-problems)
    (select-window w)))

(defun eclim-problems-refocus ()
  (interactive)

  (when (eclim--project-dir)
    (setq eclim--problems-project (eclim--project-name))
    (setq eclim--problems-file buffer-file-name)
    (with-current-buffer eclim--problems-buffer-name
      (eclim-problems-buffer-refresh))))

(defun eclim--problems-update-maybe ()
  "If autoupdate is enabled, this function triggers a delayed
refresh of the problems buffer."
  (when (and (eclim--project-dir)
	     eclim-autoupdate-problems)
    (setq eclim--problems-project (eclim--project-name))
    (setq eclim--problems-file buffer-file-name)
    (run-with-idle-timer 1 nil 
			 (lambda()
			   (let ((b (current-buffer))
				 (p (get-buffer eclim--problems-buffer-name)))
			     (if p
				 (progn
				   (set-buffer p)
				   (eclim-problems-buffer-refresh))
			       (eclim--problems-mode-init))
			     (set-buffer b))))))
 
(defun eclim-problems-compilation-buffer ()
  "Creates a compilation buffer from eclim error messages. This
is convenient as it lets the user navigate between errors using
`next-error' (\\[next-error])."
  (interactive)
  (let ((problems (eclim--problems))
	(filecol-size (eclim--problems-filecol-size))
	(project-directory (concat (eclim--project-dir buffer-file-name) "/"))
	(compil-buffer (get-buffer-create "*compilation*")))
    (with-current-buffer compil-buffer
      (message "Project directory is: %s" project-directory)
      (setq default-directory project-directory)
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert (concat "-*- mode: compilation; default-directory: "
		      project-directory
		      " -*-\n"))
      (dolist (problem (eclim--problems-filtered))
	(eclim--insert-problem-compilation problem filecol-size project-directory))
      (compilation-mode))
    (display-buffer compil-buffer 'other-window)))
    
(defun eclim--insert-problem-compilation (problem filecol-size project-directory)
  (let ((filename (first (split-string (eclim--problem-file problem) project-directory t)))
	 (position (split-string (eclim--problem-pos problem) " col " t))
	 (description (eclim--problem-description problem))
	 (type (eclim--problem-type problem)))
    (let ((line (first position))
	  (col (number-to-string (1+ (string-to-number (second position))))))
      (insert (format "%s:%s:%s: %s\n" filename line col description)))))

(add-hook 'after-save-hook #'eclim--problems-update-maybe)

(provide 'eclim-problems)
