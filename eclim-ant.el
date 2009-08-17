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

(defun eclim--ant-buildfile-name ()
  "build.xml")

(defun eclim--ant-buildfile-path ()
  (concat (eclim--project-dir) "/" (eclim-ant-buildfile-name)))

(defun eclim/ant-target-list ()
  (eclim--call-process "ant_targets" "-p" (eclim--project-name) "-f" (eclim--ant-buildfile-name)))

(defun eclim/ant-validate ()
  (mapcar (lambda (line)
            (split-string line "|"))
          (eclim--call-process "ant_validate" "-p" (eclim--project-name) "-f" (eclim--ant-buildfile-name))))

(defun eclim-ant-validate ()
  (interactive)
  (message (eclim/ant-validate)))

(defun eclim-ant-run ()
  (interactive)
  (let* ((ant-targets (eclim/ant-target-list))
         (selected-target (ido-completing-read "Target: " ant-targets)))
    ;; TODO: run ant in shell
    ))

(provide 'eclim-ant)