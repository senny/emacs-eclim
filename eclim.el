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

(defcustom eclim-use-yasnippet t
  "Determines whether the eclim snippets get turned on or off"
  :group 'eclim
  :type '(choice (const :tag "Off" nil)
                 (const :tag "On" t)))

(defvar eclim--snippet-directory (concat (file-name-directory load-file-name) "snippets"))

(defvar eclim--project-dir nil)
(make-variable-buffer-local 'eclim--project-dir)

(defvar eclim--project-name nil)
(make-variable-buffer-local 'eclim--project-name)

(defvar eclim--project-natures-cache nil)
(defvar eclim--projects-cache nil)

(defvar eclim--file-coding-system-mapping
  '(("undecided-dos" . "iso-8859-1")
    ("dos" . "iso-8859-1")
    ("undecided-unix" . "iso-8859-1")))

(defun string-startswith-p (string prefix)
  ;; TODO: there is probably already a library function that does this
  (equal (substring-no-properties string 0 (string-width prefix)) prefix))

(defun string-endswith-p (string prefix)
  ;; TODO: there is probably already a library function that does this
  (let ((w (string-width string)))
    (equal (substring-no-properties string (- w (string-width prefix)) w) prefix)))

(defun eclim--buffer-lines ()
  (goto-char (point-max))
  (let (lines)
    (while (= 0 (forward-line -1))
      (push (replace-regexp-in-string "" ""
                                      (buffer-substring-no-properties (line-beginning-position)
                                                                      (line-end-position)))
            lines))
    lines))

(defun eclim--error-buffer (text)
  (let ((errbuf (get-buffer-create "*Eclim errors*")))
    (set-buffer errbuf)
    (insert text)
    (setq buffer-read-only t)
    (display-buffer errbuf t)))

(defun eclim--build-command (command &rest args)
  (let ((i 0)
        (arguments (list command)))
    (while (< i (length args))
      (when (nth (+ i 1) args)
        (add-to-list 'arguments (nth i args) t)
        (add-to-list 'arguments (nth (+ i 1) args) t))
      (setq i (+ i 2)))
    arguments))

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
  (when buffer-file-name
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
                (or sensitive-match insensitive-match))))))

(defun eclim-find-file (path-to-file)
  (if (not (string-match-p "!" path-to-file))
      (find-file-other-window path-to-file)
    (let* ((parts (split-string path-to-file "!"))
           (archive-name (replace-regexp-in-string "^jar:file://" "" (first parts)))
           (file-name (second parts)))
      (find-file-other-window archive-name)
      (beginning-of-buffer)
      (re-search-forward (regexp-quote (replace-regexp-in-string "\\\\" "/" file-name)))
      (let ((old-buffer (current-buffer)))
        (archive-extract)
        (beginning-of-buffer)
        (kill-buffer old-buffer)))))

(defun eclim--string-strip (content)
  (replace-regexp-in-string "\s*$" "" content))

(defun eclim--project-current-file ()
  (file-relative-name buffer-file-name (eclim--project-dir)))

(defun eclim--temp-buffer ()
  (set-buffer (get-buffer-create "*eclim-temporary-buffer*"))
  (delete-region (point-min) (point-max)))

(defun eclim--byte-offset ()
  ;; TODO: restricted the ugly newline counting to dos buffers => remove it all the way later
  (let ((current-offset (position-bytes (1- (point))))
        (current-file buffer-file-name)
        (current-line (line-number-at-pos (point))))
    (when (not current-offset) (setq current-offset 0))
    (if (string-match "dos" (symbol-name buffer-file-coding-system))
        (save-excursion
          (set-buffer (get-buffer-create "*eclim: temporary*"))
          (erase-buffer)
          (insert-file-contents-literally buffer-file-name)
          (goto-line current-line))
        (+ current-offset (how-many "\n" (point-min) (point)))
      current-offset)))

(defun eclim--current-encoding ()
  (let* ((coding-system (symbol-name buffer-file-coding-system))
         (mapped-coding-system (cdr (assoc
                                     coding-system
                                     eclim--file-coding-system-mapping))))
    (if mapped-coding-system mapped-coding-system coding-system)))

(defun eclim--java-src-update ()
  (when eclim-auto-save
    (save-buffer)
    ;; TODO: Sometimes this isn't finished when we complete.
    (eclim--call-process "java_src_update"
                         "-p" (eclim--project-name)
                         "-f" (eclim--project-current-file))))

(defun eclim/workspace-dir ()
  (car (eclim--call-process "workspace_dir")))

(defun eclim/jobs (&optional family)
  ;; TODO: implement the family option
  (eclim--call-process "jobs"))

(defun eclim/problems (project)
  (eclim--check-project project)
  (eclim--call-process "problems" "-p" project))

(defun eclim-problems ()
  (interactive)
  ;; TODO display the errors in a formatted list
  (message (eclim/problems (eclim--project-name))))

(defun eclim--choices-prompt (prompt choices)
  "Displays a prompt and lets the user choose between a list of choices."
  (or
   (ido-completing-read (concat prompt ": ") choices)
   ""))

(defun eclim-complete-1 (completion-list)
  (let* ((window (get-buffer-window "*Completions*" 0))
         (c (eclim--java-identifier-at-point))
         (beg (car c))
         (word (cdr c))
         (compl (try-completion word
                                completion-list)))
    (if (and (eq last-command this-command)
             window (window-live-p window) (window-buffer window)
             (buffer-name (window-buffer window)))
        ;; If this command was repeated, and there's a fresh completion window
        ;; with a live buffer, and this command is repeated, scroll that
        ;; window.
        (with-current-buffer (window-buffer window)
          (if (pos-visible-in-window-p (point-max) window)
              (set-window-start window (point-min))
            (save-selected-window
              (select-window window)
              (scroll-up))))
      (cond
       ((null compl)
        (message "No completions."))
       ((stringp compl)
        (if (string= word compl)
            ;; Show completion buffer
            (let ((list (all-completions word completion-list)))
              (setq list (sort list 'string<))
              (with-output-to-temp-buffer "*Completions*"
                (display-completion-list list word)))
          ;; Complete
          (delete-region beg (point))
          (insert compl)
          ;; close completion buffer if there's one
          (let ((win (get-buffer-window "*Completions*" 0)))
            (if win (quit-window nil win)))))
       (t (message "That's the only possible completion."))))))

(defun eclim-complete ()
  (interactive)
  (when eclim-auto-save (save-buffer))
  (eclim-complete-1 (mapcar 'second (eclim/java-complete))))

;;** The minor mode and its keymap

(defvar eclim-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-TAB") 'eclim-complete)
    map)
  "The keymap used in `eclim-mode'.")

(define-minor-mode eclim-mode
  "An interface to the Eclipse IDE."
  nil
  " Eclim"
  eclim-mode-map
  (if eclim-mode
      (progn
        (when (and (featurep 'yasnippet) eclim-use-yasnippet)
          (yas/load-directory eclim--snippet-directory))
        )
    (kill-local-variable 'eclim--project-dir)
    (kill-local-variable 'eclim--project-name)))

(define-globalized-minor-mode global-eclim-mode eclim-mode
  (lambda () (eclim-mode 1)))

(require 'eclim-project)
(require 'eclim-java)
(require 'eclim-ant)
(require 'eclim-maven)

(provide 'eclim)
