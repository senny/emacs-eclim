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

(require 'compile)

;; Add regexp to make compilation-mode understand maven2 errors
(setq compilation-error-regexp-alist
      (append (list
               '("^\\(.*\\):\\[\\([0-9]*\\),\\([0-9]*\\)\\]" 1 2 3))
              compilation-error-regexp-alist))

(define-key eclim-mode-map (kbd "C-c C-e m p") 'eclim-maven-lifecycle-phase-run)
(define-key eclim-mode-map (kbd "C-c C-e m r") 'eclim-maven-run)

(defvar eclim-maven-directory ""
  "The directory where the project buildfiles are located")

(defvar eclim-maven-lifecycle-phases
  '("validate" "compile" "test" "package" "integration" "verify" "install" "deploy"))

(defun eclim--maven-lifecycle-phase-read ()
  (ido-completing-read "Phase: " eclim-maven-lifecycle-phases))

(defun eclim--maven-pom-path ()
  (concat (eclim--project-dir) "/pom.xml "))

(defun eclim--maven-execute (command)
  (let ((default-directory (eclim--project-dir)))
    (compile (concat "mvn -f " (eclim--maven-pom-path) " " command))))

(defun eclim-maven-run (goal)
  ""
  (interactive "MGoal: ")
  (eclim--maven-execute goal))

(defun eclim-maven-lifecycle-phase-run (phase)
  ""
  (interactive (list (eclim--maven-lifecycle-phase-read)))
  (eclim-maven-run phase))

(provide 'eclim-maven)