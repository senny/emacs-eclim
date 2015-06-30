(add-to-list 'load-path "./tests")
(add-to-list 'load-path "./tests/elisp-lint")

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(require 'cl)

(defun pkg-installed-p (pkg)
  (package-installed-p (car pkg) (version-to-list (cadr pkg))))

(condition-case err
    (let* ((pkg-info
            (with-temp-buffer
              (insert-file-contents "emacs-eclim-pkg.el")
              (goto-char (point-min))
              (read (current-buffer))))
           (name (cadr pkg-info))
           (needed-packages (cadr (nth 4 pkg-info))))
      (assert (equal name "emacs-eclim"))
      (message "Loaded emacs-eclim-pkg.el")
      (message "Installing dependencies: %S" needed-packages)
      (if (every #'pkg-installed-p needed-packages)
          (message "All dependencies present.")
        (package-refresh-contents)
        (dolist (p needed-packages)
          (unless (pkg-installed-p p)
            (package-install (car p))
            (when (not (pkg-installed-p p))
              (error (message "Failed to install %s at %s." p)))
            ))))
  (error (message "Error loading dependencies: %s" err)))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            ;; Use spaces, not tabs.
            (setq indent-tabs-mode nil)))

(require 'elisp-lint)
