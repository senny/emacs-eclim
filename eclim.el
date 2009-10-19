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
		     ;; (message (concat "PROJECT: " (eclim--project-dir)))
		     ;; (message (concat "LIST: " (concat project-list)))
		     (sensitive-match (car (cddr (assoc (eclim--project-dir) project-list))))
		     (insensitive-match (car (cddr (assoc (downcase (eclim--project-dir)) downcase-project-list)))))
		(or sensitive-match insensitive-match))))))

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
    (if (string-match "dos" (symbol-name buffer-file-coding-system))
	(+ current-offset (how-many "\n" (point-min) (point)))
      current-offset)))

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

;; company-eclim replacement code

(defun company-eclim--candidates (prefix)
  (interactive "d")
  (let ((project-file (eclim--project-current-file))
        (project-name (eclim--project-name)))
    (eclim--java-src-update)
    (setq company-eclim--doc  (eclim/java-complete)))
  (let ((completion-ignore-case nil))
    (all-completions prefix (mapcar 'eclim--complection-candidate-class company-eclim--doc))))

(defun company-eclim--find-candidate (class)
  (find class company-eclim--doc 
	:key #'eclim--complection-candidate-class
	:test #'string=))

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
    ('candidates (company-eclim--candidates arg))
    ('meta (eclim--completion-candidate-doc (company-eclim--find-candidate arg)))
    ('no-cache (equal arg ""))))

(defun company-eclim--completion-finished (arg)
  "Post-completion hook after running company-mode completions."
  (let ((candidate (company-eclim--find-candidate arg)))
    (if (string= "c" (eclim--completion-candidate-type candidate))
	;; If this is a class, then insert an import statement
	(eclim--java-organize-imports
	 (eclim/java-import-order (eclim--project-name)) 
	 (list 
	  (concat (eclim--completion-candidate-package candidate) "." 
		  (eclim--completion-candidate-class candidate)))))))

(add-hook 'company-completion-finished-hook
	  'company-eclim--completion-finished)

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
        ;; Set project dir and name.
        ;; TODO: activate this mechanism somehow
        ;; (eclim--project-dir)
        ;; (eclim--project-name))
        (when (and (featurep 'yasnippet) eclim-use-yasnippet)
          (yas/load-directory eclim--snippet-directory))
        )
    (kill-local-variable 'eclim--project-dir)
    (kill-local-variable 'eclim--project-name)))

(require 'eclim-project)
(require 'eclim-java)
(require 'eclim-ant)

(provide 'eclim)