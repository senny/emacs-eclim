;; eclim-scala.el --- an interface to the Eclipse IDE.
;;
;; Copyright (C) 2009  Yves Senn <yves senn * gmx ch>
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
;; - Shuai Lin <linshuai2012@gmail.com>
;;
;;; Conventions
;;
;; Conventions used in this file: Name internal variables and functions
;; "eclim--<descriptive-name>", and name eclim command invocations
;; "eclim/command-name", like eclim/project-list.

;;* Eclim Scala

(require 'eclim-java)

(defun eclim-scala-find-declaration ()
  "Find and display the declaration of the scala identifier at point."
  (interactive)
  (let ((i (eclim--java-identifier-at-point t)))
    (eclim/with-results hits
      (
       "scala_search"
       "-n"
       "-f"
       ("-o" (car i))
       ("-l" (length (cdr i)))
       ("-e" "utf-8")
       )
      (eclim--find-display-results (cdr i) hits t))))

(provide 'eclim-scala)
