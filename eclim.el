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
;;
;;; Conventions
;;
;; Conventions used in this file: Name internal variables and functions
;; "eclim--<descriptive-name>", and name eclim command invocations
;; "eclim/command-name", like eclim/project-list.

;;* Eclim

(eval-when-compile (require 'cl))

;;** Basics

(defgroup eclim nil
  "Interface to the Eclipse IDE."
  :group 'tools)

(defun eclim-executable-find ()
  (let (file)
    (dolist (eclipse-root '("/Applications/eclipse" "/usr/lib/eclipse"
                            "/usr/local/lib/eclipse"))
      (and (file-exists-p (setq file (expand-file-name "plugins" eclipse-root)))
           (setq file (car (last (directory-files file t "^org.eclim_"))))
           (file-exists-p (setq file (expand-file-name "bin/eclim" file)))
           (return file)))))

(defcustom eclim-executable
  (or (executable-find "eclim") (eclim-executable-find))
  "Location of eclim executable."
  :group 'eclim
  :type 'file)

(defcustom eclim-auto-save nil
  "Determines whether to save the buffer when retrieving completions.
eclim can only complete correctly when the buffer has been
saved."
  :group 'eclim
  :type '(choice (const :tag "Off" nil)
                 (const :tag "On" t)))

(defvar eclim--project-dir nil)
(make-variable-buffer-local 'eclim--project-dir)

(defvar eclim--project-name nil)
(make-variable-buffer-local 'eclim--project-name)

(defvar eclim--doc nil)
(make-variable-buffer-local 'eclim--doc)

(defun eclim--buffer-lines ()
  (goto-char (point-max))
  (let (lines)
    (while (= 0 (forward-line -1))
      (push (buffer-substring-no-properties (line-beginning-position)
                                            (line-end-position))
            lines))
    lines))

(defun eclim--call-process (&rest args)
  (let ((coding-system-for-read 'utf-8))
    (with-temp-buffer
      (if (= 0 (apply 'call-process eclim-executable nil t nil
                      "-command" args))
          (eclim--buffer-lines)
        ;; TODO: A more meaningful error message
        (message "Eclim command failed")
        nil))))

(defun eclim--project-dir ()
  "Return this file's project root directory."
  (or eclim--project-dir
      (setq eclim--project-dir
            (directory-file-name
             (expand-file-name
              (locate-dominating-file buffer-file-name ".project"))))))

(defun eclim--project-name ()
  (or eclim--project-name
      (setq eclim--project-name
            (car (cddr (assoc (eclim--project-dir)
                              (eclim/project-list)))))))

(defun eclim/project-list ()
  (mapcar (lambda (line) (nreverse (split-string line " *- *" nil)))
          (eclim--call-process "project_list")))

;;** The minor mode and its keymap

(defvar eclim-mode-map
  (let ((map (make-sparse-keymap)))
    )
  "The keymap used in `eclim-mode'.")

(define-minor-mode eclim-mode
  "An interface to the Eclipse IDE."
  nil
  "Eclim"
  eclim-mode-map
  (if eclim-mode
      (progn
        ;; Set project dir and name.
        (eclim--project-dir)
        (eclim--project-name))
    (kill-local-variable 'eclim--project-dir)
    (kill-local-variable 'eclim--project-name)))
