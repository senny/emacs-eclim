
(defvar eclim-autoupdate-problems t)

(defvar eclim-problems-mode-hook nil)

(defvar eclim--problems-project nil)

(setq eclim-problems-mode-map
      (let ((map (make-keymap)))
	(suppress-keymap map t)
	(define-key map (kbd "a") 'eclim-problems-show-all)
	(define-key map (kbd "e") 'eclim-problems-show-errors)
	(define-key map (kbd "g") 'eclim-problems-buffer-refresh)
	(define-key map (kbd "q") 'quit-window)
	(define-key map (kbd "w") 'eclim-problems-show-warnings)
	(define-key map (kbd "RET") 'eclim-problems-open-current)
	map))

(define-key eclim-mode-map (kbd "C-c C-e b") 'eclim-problems)

(defvar eclim--problems-list nil)

(defvar eclim--problems-filter nil)

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
  (message "Eclim reports %d errors, %d warnings."
	   (length (remove-if-not (lambda (p) (string-equal "e" (eclim--problem-type p))) eclim--problems-list))
	   (length (remove-if-not (lambda (p) (string-equal "w" (eclim--problem-type p))) eclim--problems-list))))

(defun eclim--problems-buffer-redisplay ()
  "Draw the problem list on screen."
  (let ((inhibit-read-only t)
	(line-number (line-number-at-pos)))
    (erase-buffer)
    (dolist (problem (eclim--problems-filtered))
      (eclim--insert-problem problem))
    (goto-line line-number)))

(defun eclim--problems-filtered ()
  (remove-if-not 
   (lambda (x) (or (not eclim--problems-filter) 
		   (string= (eclim--problem-type x) eclim--problems-filter)))
   eclim--problems-list))

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
  (eclim-problems-buffer-refresh) 
  (beginning-of-buffer))

(defun eclim-problems ()
  "Show current compilation problems in a separate window."
  (interactive)
  (eclim--problems-mode-init))

(defun eclim--problems-update-maybe ()
  "If autoupdate is enabled, this function triggers a delayed
refresh of the problems buffer."
  (when (and eclim--project-dir
	     eclim-autoupdate-problems)
    (run-with-idle-timer 1 nil 
			 (lambda() 
			   (let ((b (current-buffer))
				 (p (get-buffer eclim--problems-buffer-name)))
			     (if p
 				 (progn
				   (switch-to-buffer p)
				   (eclim-problems-buffer-refresh))
			       (eclim--problems-mode-init))
			     (switch-to-buffer b))))))
 
(add-hook 'after-save-hook #'eclim--problems-update-maybe)

(provide 'eclim-problems)