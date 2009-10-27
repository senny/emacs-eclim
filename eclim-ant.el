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
;;
;;; Conventions
;;
;; Conventions used in this file: Name internal variables and functions
;; "eclim--<descriptive-name>", and name eclim command invocations
;; "eclim/command-name", like eclim/project-list.

;;* Eclim Ant

(define-key eclim-mode-map (kbd "C-c C-e a r") 'eclim-ant-run)
(define-key eclim-mode-map (kbd "C-c C-e a a") 'eclim-ant-run)
(define-key eclim-mode-map (kbd "C-c C-e a c") 'eclim-ant-clear-cache)
(define-key eclim-mode-map (kbd "C-c C-e a v") 'eclim-ant-validate)

(defvar eclim-ant-directory ""
  "The directory where the project buildfiles are located")

(defvar eclim--ant-target-cache nil)

(defun eclim--ant-buildfile-name ()
  (concat (file-name-as-directory eclim-ant-directory) "build.xml"))

(defun eclim--ant-buildfile-path ()
  (file-name-directory (concat (eclim--project-dir) "/" (eclim--ant-buildfile-name))))

(defun eclim/ant-target-list (project buildfile)
  (eclim--check-project project)
  (eclim--call-process "ant_targets" "-p" project "-f" buildfile))

(defun eclim-ant-clear-cache ()
  (interactive)
  (setq eclim--ant-target-cache nil))

(defun eclim--ant-targets (project buildfile)
  (when (null eclim--ant-target-cache)
    (setq eclim--ant-target-cache (make-hash-table :test 'equal)))
  (or (gethash buildfile eclim--ant-target-cache)
      (puthash buildfile (eclim/ant-target-list project buildfile) eclim--ant-target-cache)))

(defun eclim/ant-validate ()
  (mapcar (lambda (line)
            (split-string line "|"))
          (eclim--call-process "ant_validate" "-p" (eclim--project-name) "-f" (eclim--ant-buildfile-name))))

(defun eclim--ant-read-target (project buildfile)
  (ido-completing-read "Target: " (eclim--ant-targets project buildfile)))

(defun eclim-ant-validate ()
  (interactive)
  (message (eclim/ant-validate))
  ;; TODO: display the error messages to the user
  )

(defun eclim-ant-run (target)
  "run a specified ant target in the scope of the current project. If
the function is called interactively the users is presented with a
  list of all available ant targets."
  (interactive (list (eclim--ant-read-target (eclim--project-name)
                                             (eclim--ant-buildfile-name))))
  (let ((default-directory (eclim--ant-buildfile-path)))
    ;; TODO: use the right version of java to execute ant
    (compile (concat "ant " target))))

(provide 'eclim-ant)