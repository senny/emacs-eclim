;; eclim-classpath.el --- an interface to the Eclipse IDE.
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
;; - Daniel Gopar <gopardaniel@yahoo.com>
;;
;;; Conventions
;;
;; Conventions used in this file: Name internal variables and functions
;; "eclim--<descriptive-name>", and name eclim command invocations
;; "eclim/command-name", like eclim/project-list.

;;* Eclim Classpath

;;; Code:

(defcustom eclim-classpath-buffer-name "*eclim: Classpath*"
  "Name for the eclim classpath buffer.  Don't need to change this."
  :group 'classpath
  :type 'string)

(defvar eclim-classpath-mode-hook nil)

;; To know if we're viewing src or var
(setq eclim-viewing-src nil)
;; Tmp var to know what proj we're currently in
(setq eclim-classpath--curr-project nil)

(setq eclim-classpath-mode-map
      (let ((map (make-keymap)))
        (suppress-keymap map t)
        (define-key map (kbd "n") 'next-line)
        (define-key map (kbd "p") 'previous-line)
        (define-key map (kbd "q") 'eclim-quit-window)
        (define-key map (kbd "RET") 'ffap)
        (define-key map (kbd "a") 'eclim-classpath--add-src-var-entry)
        (define-key map (kbd "d") 'eclim-classpath--delete-src-var-entry)
        map))

(defun eclim--classpath-mode ()
  "Set up everything for the major mode."
  (kill-all-local-variables)
  (buffer-disable-undo)
  (goto-char (point-min))
  (setq major-mode 'eclim-classpath-mode
        mode-name "eclim/classpath"
        mode-line-process ""
        truncate-lines t
        buffer-read-only t
        default-directory (eclim/workspace-dir))
  (setq-local line-move-visual nil)
  (setq mode-line-format
        (list "-"
              'mode-line-mule-info
              'mode-line-modified
              'mode-line-frame-identification
              'mode-line-buffer-identification

              "   "
              'mode-line-position

              "  "
              'eclim--problems-filter-description

              "  "
              'mode-line-modes
              '(which-func-mode ("" which-func-format "--"))

              'global-mode-string
              "-%-"))
  (hl-line-mode t)
  (use-local-map eclim-classpath-mode-map)
  (run-mode-hooks 'eclim-classpath-mode-hook))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Eclim functions                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun eclim/java-classpath (project)
  "Return string of *all* classpath entries separated by colon.
Argument PROJECT The current eclim project."
  (eclim--check-project project)
  (eclim--call-process "java_classpath" "-p" project))

;; TODO: Figure out why it doesn't work if we pass 'project'
;; works fine without any arguments
(defun eclim/java-src-dirs (project)
  "Return all src dirs from projecct that are listed in classpath.
Argument PROJECT The current eclim project."
  (eclim--check-project project)
  (eclim/execute-command "java_src_dirs" "-p"))

(defun eclim/java-classpath-variables ()
  "Return vector of associations of var name and path."
  (eclim/execute-command "java_classpath_variables"))

(defun eclim/java-classpath-variable-create (name path)
  "Create new VAR.
Argument NAME Name of the new variable to create.
Argument PATH The path of where the new variable is located ."
  (eclim--call-process "java_classpath_variable_create" "-n" name "-p" path))

(defun eclim/java-classpath-variable-delete (name)
  "Deletes VAR.
Argument NAME Variable to delete."
  (eclim--call-process "java_classpath_variable_delete" "-n" name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interactive commands                                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun eclim-classpath-show-src-dirs ()
  "Display all src directories from the current project."
  (interactive)
  (eclim--classpath-update-curr-proj)
  (setq eclim-viewing-src t)
  (let ((src-dirs (eclim/java-src-dirs (or (eclim-project-name) eclim-classpath--curr-project)))
        (classpath-buffer (get-buffer-create eclim-classpath-buffer-name)))
    (switch-to-buffer classpath-buffer)
    (read-only-mode -1)
    (erase-buffer)
    (insert src-dirs)
    (eclim--classpath-mode)))

;; TODO: Fix "ffap". Does not grab VAR path
(defun eclim-classpath-show-vars ()
  "Show all variables."
  (interactive)
  (eclim--classpath-update-curr-proj)
  (setq eclim-viewing-src nil)
  (let ((class-vars (eclim/java-classpath-variables))
        (classpath-buffer (get-buffer-create eclim-classpath-buffer-name))
        (var-list-responce ""))

    ;; Starting point on getting the name's and path's for all project variables
    (let ((var-list (mapcar 'identity class-vars))) ;; create vector into list
      (while var-list
        ;; Add the VAR name
        (setq var-list-responce (concat var-list-responce (cdr (assoc 'name (car var-list)))))
        ;; Show an arrow the the path of the VAR
        (setq var-list-responce (concat var-list-responce " => "))
        ;; Add actual variable PATH
        (setq var-list-responce (concat var-list-responce (cdr (assoc 'path (car var-list))) "\n"))
        ;; update the list please
        (setq var-list (cdr var-list))))

    (switch-to-buffer classpath-buffer)
    (read-only-mode -1)
    (erase-buffer)
    (insert var-list-responce)
    (eclim--classpath-mode)))

(defun eclim-classpath-variable-update (var-name var-path)
  "Update a VAR in eclipse.
Argument VAR-NAME Variable that is going to be updated.
Argument VAR-PATH New path that the variable will point to."
  (interactive (list (eclim--var-read)
                     (read-directory-name "New VAR Path: " (eclim--project-dir) nil t)))
  ;; Update var to point to new path
  (eclim/java-classpath-variable-create var-name var-path)
  ;; Refresh project for changes to take effect
  (eclim-project-update (eclim-project-name))
  (message "Updated variable %s to %s" var-name var-path))

(defun eclim-classpath-variable-delete (var-name)
  "Delete VAR in eclipse.
Argument VAR-NAME Variable to delete from elclipse workspace."
  (interactive (list (eclim--var-read)))
  (eclim/java-classpath-variable-delete var-name)
  (eclim-project-update (eclim-project-name))
  (message "Deleted variable %s" var-name))

(defun eclim-classpath-variable-create (var-name var-path)
  "Create new VAR in eclipse.
Argument VAR-NAME Name of variable to create.
Argument VAR-PATH The path that the new variable will point to."
  (interactive (list (upcase (read-string "New VAR Name: "))
                     (expand-file-name (read-directory-name "New VAR Path: "
                                                            (eclim--project-dir) nil t))))
  (eclim/java-classpath-variable-create var-name var-path)
  (eclim-project-update eclim-classpath--curr-project)
  (message "Variable %s Created" var-name))

;; TODO: Find if there's an easier way to grab variable path
(defun eclim-classpath-variable-new-entry ()
  "Create a VAR entry in the project.  Update the .classpath file."
  (interactive)
  (let* ((var (eclim--var-read))
         (list (mapcar (lambda (p) (if (string= (assoc-default 'name p) var)
                                  (assoc-default 'path p))) (eclim/java-classpath-variables)))
         (path (car (remq nil list)))
         (temp-path "")
         (var-entry "<classpathentry exported=\"true\" kind=\"var\" path=\"%s/%s\"/>"))
    (setq temp-path (expand-file-name (read-file-name (concat var ":") path nil t)))
    (setq temp-path (substring temp-path (length path)))
    (eclim--classpath-update-classpath-file (format var-entry var temp-path)
                                            (concat (eclim--project-dir) "/.classpath"))
    (message "Variable Entry Added to .classpath file")))

(defun eclim-classpath--add-src-var-entry ()
  "Add a src or variable to project."
  (interactive)
  (let ((cur-proj-dir (eclim--project-dir eclim-classpath--curr-project)))
    (if eclim-viewing-src
        (eclim--classpath-add-src-entry cur-proj-dir)
      (eclim--classpath-add-var-entry cur-proj-dir))))

(defun eclim-classpath--delete-src-var-entry ()
  "Delete a src or variable to project."
  (interactive)
  (if eclim-viewing-src
      (eclim--classpath-delete-src-entry)
    (eclim--classpath-delete-var-entry)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other funcs                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun eclim--classpath-add-src-entry (proj-dir)
  "Add a src directory to the .classpath file.  Helper function.
Argument PROJ-DIR The root directory of the current eclim project."
  (let* ((file (expand-file-name (read-directory-name "Src Path: " proj-dir nil t))) ;; src abs path
         (file-path (substring file (+ 1 (length proj-dir)) -1)) ;; only get relative path
         (src-entry (format "<classpathentry kind=\"src\" path=\"%s\"/>" file-path))) ;; str to add to file
    (eclim--classpath-update-classpath-file src-entry (concat proj-dir "/.classpath"))
    (eclim-project-update eclim-classpath--curr-project)
    ;; (eclim-classpath-show-src-dirs) ;; re-display buffer.
    (read-only-mode -1)
    (goto-char (point-min))

    ;; check if it has "/" at end
    (if (string= "/" (substring file -1))
        (insert (concat (substring file 0 -1) "\n"))
      (insert (concat file "\n")))

    (read-only-mode 1)
    (message "Source Entry Added")))

(defun eclim--classpath-add-var-entry (proj-dir)
  "Add a var entry to the .classpath file.  Helper function.
Argument PROJ-DIR The root directory of the current eclim project."
  (let* ((var (eclim--var-read))
         (list (mapcar (lambda (p) (if (string= (assoc-default 'name p) var)
                                  (assoc-default 'path p))) (eclim/java-classpath-variables)))
         (path (car (remq nil list)))
         (temp-path "")
         (var-entry "<classpathentry exported=\"true\" kind=\"var\" path=\"%s/%s\"/>"))
    (setq temp-path (expand-file-name (read-file-name (concat var ":") path nil t)))
    (setq temp-path (substring temp-path (+ 1 (length path))))
    (eclim--classpath-update-classpath-file (format var-entry var temp-path)
                                            (concat proj-dir "/.classpath"))
    ;; Add the new VAR entry to buffer
    (read-only-mode -1)
    (goto-char (point-min))
    (insert (concat var " => " path "\n"))
    (read-only-mode 1)

    (eclim-project-update eclim-classpath--curr-project)
    (message "Variable Entry Added")))

(defun eclim--classpath-delete-src-entry ()
  "Remove src directory from project that is in current buffer line"
  (read-only-mode -1)
  (kill-whole-line)
  (read-only-mode 1)
  (let* ((proj-dir (eclim--project-dir eclim-classpath--curr-project))
         (classpath-file (concat proj-dir "/.classpath"))
         ;; Get relative path to projects root
         (path (substring (substring (current-kill 0) 0 -1) (+ 1 (length proj-dir))))
         (line-to-remove (format "<classpathentry kind=\"src\" path=\"%s\"/>" path)))
    (with-temp-buffer
      (insert-file-contents classpath-file)
      (search-forward line-to-remove)
      (kill-whole-line)
      (write-file classpath-file)))
  (eclim-project-update eclim-classpath--curr-project)
  (message "Source Entry Deleted"))

(defun eclim--classpath-delete-var-entry ()
  "Remove VAR from eclipse that is in current buffer line"
  (read-only-mode -1)
  (kill-whole-line)
  (let ((var-to-delete (car (split-string (current-kill 0)))))
    (eclim/java-classpath-variable-delete var-to-delete)
    (read-only-mode 1)
    (eclim-project-update eclim-classpath--curr-project)
    (message "%s Deleted" var-to-delete)))

(defun eclim--var-read ()
  "Prompts user for a pre-defined variable."
  (eclim--completing-read "Var Name: "
                          (mapcar (lambda (p) (assoc-default 'name p)) (eclim/java-classpath-variables))))

(defun eclim--classpath-update-classpath-file (string file)
  "Find .classpath file in project and add a new entry to it.
Argument STRING Line of text that will be inserted into .classpath file.
Argument FILE The path of the .classpath file"
  (if (not (file-exists-p file))
      (error "File does not exist")
    (progn
      (with-temp-buffer
        (insert-file-contents file)
        (search-forward "<classpath>")
        (newline-and-indent)
        (insert string)
        (write-file file)))))

(defun eclim--classpath-update-curr-proj ()
  "Check if eclim mode is enabled then update help variable."
  (if (bound-and-true-p eclim-mode)
      (setq eclim-classpath--curr-project (eclim-project-name))))

(provide 'eclim-classpath)

;;; eclim-classpath.el ends here
