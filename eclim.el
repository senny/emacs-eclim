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

(defcustom eclim-interactive-completion-function 'ido-completing-read
  ""
  :group 'eclim
  :type 'function)

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

(defvar eclim--snippet-directory
  (concat (file-name-directory load-file-name) "snippets"))

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

(defvar eclim--compressed-urls-regexp "\\(^jar:file://\\)\\|\\(^zip://\\)")
(defvar eclim--compressed-file-path-replacement-regexp "\\\\")
(defvar eclim--compressed-file-path-removal-regexp "^/")

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

(defun eclim--completing-read (prompt choices)
  (funcall eclim-interactive-completion-function prompt choices))

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

(defun eclim--find-file (path-to-file)
  (if (not (string-match-p "!" path-to-file))
      (find-file-other-window path-to-file)
    (let* ((parts (split-string path-to-file "!"))
           (archive-name (replace-regexp-in-string eclim--compressed-urls-regexp "" (first parts)))
           (file-name (second parts)))
      (find-file-other-window archive-name)
      (beginning-of-buffer)
      (re-search-forward (replace-regexp-in-string
                          eclim--compressed-file-path-removal-regexp ""
                          (regexp-quote (replace-regexp-in-string
                                         eclim--compressed-file-path-replacement-regexp
                                         "/" file-name))))
      (let ((old-buffer (current-buffer)))
        (archive-extract)
        (beginning-of-buffer)
        (kill-buffer old-buffer)))))

(defun eclim--find-display-results (base-directory pattern results)
  (pop-to-buffer (get-buffer-create "*eclim: find"))
  (let ((buffer-read-only nil))
    (erase-buffer)
    (insert (concat "-*- mode: eclim-find; default-directory: " base-directory " -*-"))
    (newline 2)
    (insert (concat "eclim java_sarch -p " pattern))
    (newline)
    (dolist (result results)
      (insert (eclim--convert-find-result-to-string result base-directory))
      (newline))
    (setq default-directory base-directory)
    (grep-mode)))

(defun eclim--convert-find-result-to-string (line &optional directory)
  (let ((converted-directory (replace-regexp-in-string "\\\\" "/" (car line))))
    (concat (if converted-directory
                (replace-regexp-in-string (concat (regexp-quote directory) "/?") "" converted-directory)
              converted-directory)
            ":" (replace-regexp-in-string " col " ":" (second line)) " "
            (third line))))

(defun eclim--visit-declaration (eclim-response)
  (let* ((file-name (car eclim-response))
         (line-and-column (cadr eclim-response))
         (position (split-string line-and-column " col ")))
    (eclim--find-file file-name)
    (goto-line (string-to-number (car position)))
    (move-to-column (- (string-to-number (cadr position)) 1))))

(defun eclim--string-strip (content)
  (replace-regexp-in-string "\s*$" "" content))

(defun eclim--project-current-file ()
  (file-relative-name buffer-file-name (eclim--project-dir)))

(defun eclim--temp-buffer ()
  (set-buffer (get-buffer-create "*eclim-temporary-buffer*"))
  (delete-region (point-min) (point-max)))

(defun eclim--byte-offset ()
  ;; TODO: restricted the ugly newline counting to dos buffers => remove it all the way later
  (let ((current-offset (position-bytes (1- (point)))))
    (when (not current-offset) (setq current-offset 0))
    (if (string-match "dos" (symbol-name buffer-file-coding-system))
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

(defun eclim-complete ()
  (interactive)
  ;; TODO build context sensitive completion mechanism
  (eclim-java-complete))

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
