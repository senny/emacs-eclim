;;; ac-emacs-eclim.el --- auto-complete source for eclim
;;
;; Copyright (C) 2009   Fredrik Appelberg
;;
;; Package-Requires: ((eclim "0.3") (auto-complete "1.5"))
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
;;; Conventions
;;
;; Conventions used in this file: Name internal variables and functions
;; "eclim--<descriptive-name>", and name eclim command invocations
;; "eclim/command-name", like eclim/project-list.
;;
;;; Commentary:
;;
;; An Emacs eclim source for auto-complete-mode.

;;; Code:

(require 'eclim)
(require 'eclim-java)
(require 'eclim-completion)
(require 'auto-complete)

(defface ac-emacs-eclim-candidate-face
  '((t (:background "gold1" :foreground "black")))
  "Face for emacs-eclim candidate."
  :group 'auto-complete)

(defface ac-emacs-eclim-selection-face
  '((t (:background "gold4" :foreground "white")))
  "Face for the emacs-eclim selected candidate."
  :group 'auto-complete)

(ac-define-source emacs-eclim
                  '((candidates . eclim--completion-candidates)
                    (action . ac-emacs-eclim-action)
                    (prefix . eclim-completion-start)
                    (document . eclim--completion-documentation)
                    (cache)
                    (selection-face . ac-emacs-eclim-selection-face)
                    (candidate-face . ac-emacs-eclim-candidate-face)
                    (symbol . "f")))

(defun ac-emacs-eclim-action ()
  (eclim--completion-action eclim--completion-start (point)))

(defun ac-emacs-eclim-java-setup ()
  (add-to-list 'ac-sources 'ac-source-emacs-eclim))

(defun ac-emacs-eclim-xml-setup ()
  (add-to-list 'ac-sources 'ac-source-emacs-eclim))

(defun ac-emacs-eclim-php-setup ()
  (add-to-list 'ac-sources 'ac-source-emacs-eclim))

(defun ac-emacs-eclim-ruby-setup ()
  (add-to-list 'ac-sources 'ac-source-emacs-eclim))

(defun ac-emacs-eclim-c-setup ()
  (add-to-list 'ac-sources 'ac-source-emacs-eclim))

(defun ac-emacs-eclim-scala-setup ()
  (add-to-list 'ac-sources 'ac-source-emacs-eclim))

;;;###autoload
(defun ac-emacs-eclim-config ()
  (add-hook 'java-mode-hook 'ac-emacs-eclim-java-setup)
  (add-hook 'groovy-mode-hook '(lambda ()
                                 (add-to-list 'ac-sources 'ac-source-emacs-eclim)))
  (add-hook 'xml-mode-hook 'ac-emacs-eclim-xml-setup)
  (add-hook 'nxml-mode-hook 'ac-emacs-eclim-xml-setup)
  (add-hook 'php-mode-hook 'ac-emacs-eclim-php-setup)
  (add-hook 'ruby-mode-hook 'ac-emacs-eclim-ruby-setup)
  (add-hook 'c-mode-hook 'ac-emacs-eclim-c-setup)
  (add-hook 'c++-mode-hook 'ac-emacs-eclim-c-setup)
  (add-hook 'scala-mode-hook 'ac-emacs-eclim-scala-setup))

(provide 'ac-emacs-eclim)
;;; ac-emacs-eclim.el ends here
