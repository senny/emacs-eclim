;; ac-emacs-eclim-source.el --- an interface to the Eclipse IDE.
;;
;; Copyright (C) 2009   Fredrik Appelberg
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
;;; Description
;;
;; ac-emacs-eclim-source.el -- a emacs eclime source for auto-complete-mode
;;

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

(defun ac-emacs-eclim-candidates ()
	(eclim--completion-candidates))
  ;; (with-no-warnings
  ;;   (mapcar (lambda (c) (assoc-default 'info c))
  ;;           (assoc-default 'completions (eclim/java-complete)))))

(defun ac-emacs-eclim-prefix ()
	)

(defun ac-emacs-eclim-available ()
	t)
  ;; (eclim--accepted-p (buffer-file-name)))

(defun ac-emacs-eclim-init ()
  (setq eclim--completion-start ac-point)
  (when eclim-print-debug-messages (message "Completion started at %s, ac-point is %s" (point) ac-point)))

(defun ac-emacs-eclim-prefix-nxml ()
	;; (if (re-search-backward "\\.\\(\\(?:[a-zA-Z0-9][_a-zA-Z0-9]*\\)?\\)\\=" nil t)
	(if (= (char-before) 32)
			(point)
		(if (re-search-backward "[< \"]\\(\\(?:[a-zA-Z0-9][:-_a-zA-Z0-9]*\\)?\\)\\=" nil t)
				(match-beginning 1))))

(ac-define-source emacs-eclim-nxml
  '((candidates . ac-emacs-eclim-candidates)
    (available . ac-emacs-eclim-available)
    (init . ac-emacs-eclim-init)
    (action . eclim--completion-action)
		(prefix . ac-emacs-eclim-prefix-nxml)
    (requires . 0)
    (cache)
    (selection-face . ac-emacs-eclim-selection-face)
    (candidate-face . ac-emacs-eclim-candidate-face)
    (symbol . "f")))

(ac-define-source emacs-eclim
  '((candidates . ac-emacs-eclim-candidates)
    (available . ac-emacs-eclim-available)
    (init . ac-emacs-eclim-init)
    (action . eclim--completion-action)
    (requires . 0)
    (cache)
    (selection-face . ac-emacs-eclim-selection-face)
    (candidate-face . ac-emacs-eclim-candidate-face)
    (symbol . "f")))

(ac-define-source emacs-eclim-c-dot
  '((candidates . ac-emacs-eclim-candidates)
    (available . ac-emacs-eclim-available)
    (init . ac-emacs-eclim-init)
    (action . eclim--completion-action)
    (prefix . c-dot)
    (requires . 0)
    (cache)
    (selection-face . ac-emacs-eclim-selection-face)
    (candidate-face . ac-emacs-eclim-candidate-face)
    (symbol . "f")))

(provide 'ac-emacs-eclim-source)
