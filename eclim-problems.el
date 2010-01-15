
(defvar eclim-autoupdate-problems t)

(defvar eclim-problems-mode-hook nil)

(defvar eclim--problems-project nil)

(defvar eclim-problems-mode-map
  (let ((map (make-keymap)))
    map))

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

(defun eclim--problems-buffer-refresh ()
  (let ((inhibit-read-only t)
	(line-number (line-number-at-pos)))
    (erase-buffer)
    (dolist (problem (eclim--problems))
      (eclim--insert-problem problem))
    (goto-line line-number)))

(defun eclim--insert-problem (problem)
  (insert (format "%-40s | %-12s | %s "
		  (truncate-string-to-width (file-name-nondirectory (eclim--problem-file problem)) 40 0 nil t)
		  (eclim--problem-pos problem)
		  (eclim--problem-description problem)))
  (insert "\n"))

(defun eclim--problems-mode-init ()
  (setq eclim--problems-project (eclim--project-name))
  (switch-to-buffer (get-buffer-create "*eclim: problems*"))
  (eclim--problems-mode)
  (eclim--problems-buffer-refresh)
  (beginning-of-buffer))

(defun eclim-problems ()
  "Show current compilation problems in a separate window."
  (interactive)
  (eclim--problems-mode-init))

(defun eclim--problems-update-maybe ())

(add-hook 'after-save-hook #'eclim--problems-update-maybe)

(provide 'eclim-problems')