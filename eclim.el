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
      (and (file-exists-p
            (setq file (expand-file-name "plugins" eclipse-root)))
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

(defvar eclim--project-natures-cache nil)
(defvar eclim--projects-cache nil)

(defun eclim--buffer-lines ()
  (goto-char (point-max))
  (let (lines)
    (while (= 0 (forward-line -1))
      (push (replace-in-string (buffer-substring-no-properties (line-beginning-position)
                                                               (line-end-position)) "" "")
            lines))
    lines))

(defun eclim--error-buffer (text)
  (let ((errbuf (get-buffer-create "*Eclim errors*")))
    (set-buffer errbuf)
    (insert text)
    (setq buffer-read-only t)
    (display-buffer errbuf t)))

(defun eclim--call-process (&rest args)
  (message (apply 'concat eclim-executable " -command " (mapcar (lambda (arg)
                                                         (concat " " arg)) args)))
  (let ((coding-system-for-read 'utf-8))
    (with-temp-buffer
      (if (= 0 (apply 'call-process eclim-executable nil t nil
                      "-command" args))
          (eclim--buffer-lines)
        (eclim--error-buffer
         (buffer-substring-no-properties
          (point-min) (point-max)))
        nil))))

(defun eclim--project-dir ()
  "Return this file's project root directory."
  (or eclim--project-dir
      (setq eclim--project-dir
	    (directory-file-name
	     (file-name-directory
	      (expand-file-name
	       (locate-dominating-file buffer-file-name ".project")))))))

(defun eclim--project-name ()
  (or eclim--project-name
      (setq eclim--project-name
            (let* ((project-list (eclim/project-list))
                   (downcase-project-list (mapcar (lambda (project)
                                                    (list
                                                     (downcase (first project))
                                                     (second project)
                                                     (third project))) project-list))
                   (sensitive-match (car (cddr (assoc (eclim--project-dir) project-list))))
                   (insensitive-match (car (cddr (assoc (downcase (eclim--project-dir)) downcase-project-list)))))
              (or sensitive-match insensitive-match)))))

(defun eclim--string-strip (content)
  (replace-regexp-in-string "\s*$" "" content))

(defun eclim--project-current-file ()
  (file-relative-name buffer-file-name (eclim--project-dir)))

(defun eclim--temp-buffer ()
  (set-buffer (get-buffer-create "*eclim-temporary-buffer*"))
  (delete-region (point-min) (point-max)))

(defun eclim--byte-offset ()
  ;; TODO: replace ugly newline-counting with a serious solution
;;;   (+ (position-bytes (point))
;;;      (how-many "\n" (point-min) (point))))
  (position-bytes (point)))

(defun company-eclim--candidates (prefix)
  (interactive "d")
  (let ((project-file (eclim--project-current-file))
        (project-name (eclim--project-name)))
    (when eclim-auto-save
      (save-buffer)
      ;; FIXME: Sometimes this isn't finished when we complete.
      (company-eclim--call-process "java_src_update"
                                   "-p" (eclim--project-name)
                                   "-f" project-file))
    (setq company-eclim--doc
          (mapcar (lambda (line)
                    (cdr (split-string line "|" nil)))
                  (eclim--call-process
                   "java_complete" "-p" (eclim--project-name)
                   "-f" project-file
                   "-o" (number-to-string (eclim--byte-offset))
                   "-e" "utf-8"
                   "-l" "standard"))))
  (let ((completion-ignore-case nil))
    (all-completions prefix (mapcar 'car company-eclim--doc))))

(defun eclim/workspace-dir ()
  (car (eclim--call-process "workspace_dir")))

(defun eclim/jobs (&optional family)
  ;; TODO: implement the family option
  (eclim--call-process "jobs"))

(defun eclim/locate-file (pattern scope &optional project)
  ;; TODO: add optional project parameter
  (mapcar (lambda (line)
            (split-string line "|")) (eclim--call-process "locate_file" "-p" pattern "-s" scope)))

(defun eclim-complete ()
  (interactive)
  (insert (or
           (ido-completing-read "Completions: " (mapcar 'second (eclim/java-complete)))
           "")))

(defun company-eclim (command &optional arg &rest ignored)
  "A `company-mode' back-end for eclim completion"
  (interactive)
  (case command
    ('prefix (and (derived-mode-p 'java-mode 'jde-mode)
                  buffer-file-name
                  eclim-executable
                  (eclim--project-name)
                  (not (company-in-string-or-comment))
                  (or (company-grab-symbol) 'stop)))

    ('candidates (progn
                   (message (company-eclim--candidates arg))))
    ('meta (cadr (assoc arg company-eclim--doc)))
    ('no-cache (equal arg ""))
    ))

;;** The minor mode and its keymap

(defvar eclim-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-e SPC") 'eclim-complete)
    map)
  "The keymap used in `eclim-mode'.")

(define-minor-mode eclim-mode
  "An interface to the Eclipse IDE."
  nil
  "Eclim"
  eclim-mode-map
  (if eclim-mode
      (progn
        ;; Set project dir and name.
        ;; TODO: activate this mechanism somehow
        ;; (eclim--project-dir)
        ;; (eclim--project-name))
        )
    (kill-local-variable 'eclim--project-dir)
    (kill-local-variable 'eclim--project-name)))

(require 'eclim-project)
(require 'eclim-java)
(require 'eclim-ant)

(provide 'eclim)