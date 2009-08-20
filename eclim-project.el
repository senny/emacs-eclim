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
;;  - Yves Senn <yves senn * gmx ch>
;;
;;; Conventions
;;
;; Conventions used in this file: Name internal variables and functions
;; "eclim--<descriptive-name>", and name eclim command invocations
;; "eclim/command-name", like eclim/project-list.

;;* Eclim Project

(defvar eclim-project-mode-hook nil)

(defvar eclim-project-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map t)
    (define-key map (kbd "m") 'eclim-project-mark-current)
    (define-key map (kbd "o") 'eclim-project-open)
    (define-key map (kbd "c") 'eclim-project-close)
    (define-key map (kbd "q") 'quit-window)
    map))

(defun eclim--check-nature (nature)
  (let ((natures (or eclim--project-natures-cache
                     (setq eclim--project-natures-cache))))
    (when (not (assoc-string nature natures))
      (error (concat "invalid project nature: " nature)))))

(defun eclim--check-project (project)
  (let ((projects (or eclim--projects-cache
                      (setq eclim--projects-cache (mapcar 'third (eclim/project-list))))))
    (when (not (assoc-string project projects))
      (error (concat "invalid project: " project)))))

(defun eclim--project-read ()
  (interactive)
  (if (eq major-mode
          'eclim-project-mode)
      (progn
        (or (eclim--project-get-marked)
            (eclim--project-current-line)))
    ()))

(defun eclim--project-mode-init ()
  (switch-to-buffer (get-buffer-create "*eclim: projects*"))
  (eclim--project-mode)
  (eclim--project-buffer-refresh))

(defun eclim--project-mode ()
  "Manage all your eclim projects one buffer"
  (kill-all-local-variables)
  (buffer-disable-undo)
  (setq major-mode 'eclim-project-mode
        mode-name "eclim/project"
        mode-line-process ""
        truncate-lines t
        line-move-visual nil
        buffer-read-only t
        default-directory (eclim/workspace-dir))
  (hl-line-mode t)
  (use-local-map eclim-project-mode-map)
  (run-mode-hooks 'eclim-project-mode-hook))

(defun eclim--project-buffer-refresh ()
  (let ((inhibit-read-only t)
        (line-number (line-number-at-pos)))
    (erase-buffer)
    (dolist (project (eclim/project-list))
      (eclim--insert-project project))
    (goto-line line-number)))

(defun eclim--insert-project (project)
  (let ((name (truncate-string-to-width (third project) 30 0 nil t))
        (status (second project))
        (path (first project)))
    (insert (format "  | %-6s | %-30s | %s" status name path))
    (insert "\n")))

(defun eclim--project-insert-mark-current (mark face)
  (let ((inhibit-read-only t))
    (save-excursion
      (beginning-of-line)
      (delete-char 1)
      (insert mark)
      (put-text-property (- (point) 1) (point) 'face face))))

(defun eclim--project-get-marked ()
  (interactive)
  (let ((marked-projects '()))
    (save-excursion
      (beginning-of-buffer)
      (while (re-search-forward "*" nil t)
        (push (eclim--project-current-line) marked-projects)))
    marked-projects))

(defun eclim--project-column-start (column)
  (save-excursion
    (+ (re-search-forward "|" nil t (- column 1)) 1)))

(defun eclim--project-column-end (column)
  (save-excursion
    (- (re-search-forward "|" nil t column) 1)))

(defun eclim--project-current-line ()
  (save-excursion
    (beginning-of-line)
    (eclim--string-strip (buffer-substring-no-properties
                          (eclim--project-column-start 3)
                          (eclim--project-column-end 3)))))

(defun eclim/project-list ()
  (mapcar (lambda (line) (nreverse (split-string line " *- *" nil)))
          (eclim--call-process "project_list")))

(defun eclim/project-import (folder)
  (eclim--call-process "project_import" "-f" folder))

(defun eclim/project-create (folder natures name &optional depends)
  ;; TODO: allow multiple natures
  (eclim--check-nature natures)
  (eclim--call-process "project_create" "-f" folder "-n" natures "-p" name))

(defun eclim/project-delete (project)
  (eclim--check-project project)
  (eclim--call-process "project_delete" "-p" project))

(defun eclim/project-open (project)
  (eclim--check-project project)
  (eclim--call-process "project_open" "-p" project))

(defun eclim/project-close (project)
  (message (concat "CLOSING: " project))
  (eclim--check-project project)
  (eclim--call-process "project_close" "-p" project))

(defun eclim/project-info (project)
  (eclim--check-project project)
  (eclim--call-process "project_info" "-p" project))

(defun eclim/project-settings (project)
  (eclim--check-project project)
  ;; TODO: make the output useable
  (eclim--call-process "project_settings" "-p" project))

(defun eclim/project-setting (project setting)
  (eclim--check-project project)
  ;; TODO: make the output useable
  (eclim--call-process "project_setting" "-p" project "-s" setting))

(defun eclim/project-nature-add (project nature)
  (eclim--check-project project)
  (eclim--check-nature nature)
  (eclim--call-process "project_nature_add" "-p" project "-n" nature))

(defun eclim/project-nature-remove (project nature)
  (eclim--check-project project)
  (eclim--check-nature nature)
  (eclim--call-process "project_nature_remove" "-p" project "-n" nature))

(defun eclim/project-natures (project)
  (eclim--check-project project)
  (eclim--call-process "project_natures" "-p" project))

(defun eclim/project-refresh (project)
  (eclim--check-project project)
  (eclim--call-process "project_refresh" "-p" project))

(defun eclim/project-refresh-file (project file)
  (eclim--check-project project)
  (eclim--call-process "project_refresh_file" "-p" project "-f" file))

(defun eclim/project-update (project)
  (eclim--check-project project)
  ;; TODO: add buildfile and settings options
  (eclim--call-process "project_update" "-p" project))

(defun eclim/project-nature-aliases ()
  (eclim--call-process "project_nature_aliases"))

(defun eclim-project-open (projects)
  (interactive (list (eclim--project-read)))
  (when (not (listp projects)) (set 'projects (list projects)))
  (dolist (project projects)
    (eclim/project-open project))
  (eclim--project-buffer-refresh))

(defun eclim-project-close (projects)
  (interactive (list (eclim--project-read)))
  (when (not (listp projects)) (set 'projects (list projects)))
  (dolist (project projects)
    (eclim/project-close project))
  (eclim--project-buffer-refresh))

(defun eclim-project-mark-current ()
  (interactive)
  (eclim--project-insert-mark-current "*" 'dired-mark)
  (forward-line 1))

(defun eclim-manage-projects ()
  (interactive)
  (eclim--project-mode-init))


(provide 'eclim-project)