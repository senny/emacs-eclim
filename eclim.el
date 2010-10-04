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
;;  - Fredrik Appelberg <fredrik * bitbakery se>
;;  - Alessandro Arzilli <alessandro.arzilli * gmail com>
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

(defcustom eclim-interactive-completion-function (if ido-mode 'ido-completing-read 'completing-read)
  "Defines a function which is used by eclim to complete a list of
choices interactively."
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
    ("undecided-unix" . "iso-8859-1")
    ("utf-8-dos" . "utf-8")
    ("utf-8-unix" . "utf-8")))

(defvar eclim--compressed-urls-regexp "\\(^jar:file://\\)\\|\\(^zip://\\)")
(defvar eclim--compressed-file-path-replacement-regexp "\\\\")
(defvar eclim--compressed-file-path-removal-regexp "^/")

(defvar eclim--supress-errors nil)

(defun string-startswith-p (string prefix)
  ;; TODO: there is probably already a library function that does this
  (equal (substring-no-properties string 0 (string-width prefix)) prefix))

(defun string-endswith-p (string prefix)
  ;; TODO: there is probably already a library function that does this
  (let ((w (string-width string)))
    (equal (substring-no-properties string (- w (string-width prefix)) w) prefix)))

(defun eclim--build-command (command &rest args)
  (cons command
	(loop for a = args then (rest (rest a))
	      for arg = (first a)
	      for val = (second a)
	      while arg when val append (list arg val))))

(defun eclim--call-process (&rest args)
  "Calls eclim with the supplied arguments. Consider using
`eclim/execute-command' instead, as it has argument expansion,
error checking, and some other niceties.."
  (let ((cmd (apply 'concat eclim-executable " -command " 
		    (mapcar (lambda (arg) (concat " " arg))
			    (mapcar (lambda (arg) (if (numberp arg) (number-to-string arg) arg))
				    args)))))
    (message cmd)
    (remove-if (lambda (s) (= 0 (length s)))
	    (split-string
	     (shell-command-to-string cmd)
	     "\n"))))

(setq eclim--default-args
      '(("-n" . (eclim--project-name))
	("-p" . (eclim--project-name))
	("-e" . (eclim--current-encoding))
	("-f" . (eclim--project-current-file))
	("-o" . (eclim--byte-offset))
	("-s" . "project")))

(defun eclim--args-contains (args flags)
  "Check if an (unexpanded) ARGS list contains any of the
specified FLAGS."
  (loop for f in flags
	when (find f args :test #'string= :key (lambda (a) (if (listp a) (car a) a)))
	return t))

(defun eclim--expand-args (args)
  "Takes a list of command-line arguments with which to call the
eclim server. Each element should be either a string or a
list. If it is a string, its default value is looked up in
`eclim--default-args' and used to construct a list. The argument
lists are then appended together."
  (mapcar (lambda (a) (if (numberp a) (number-to-string a) a))
	  (loop for a in args
		append (if (listp a) 
			   (list (car a) (eval (cadr a))) 
			 (list a (eval (cdr (or (assoc a eclim--default-args)
						(error "sorry, no default value for: %s" a)))))))))
    
(defmacro eclim/execute-command (cmd &rest args)
  "Calls `eclim--expand-args' on ARGS, then calls eclim with the
results. Automatically saves the current buffer (and optionally
other java buffers as well), performs an eclim java_src_update
operation, and refreshes the current buffer if necessary. Raises
an error if the connection is refused. Automatically calls
`eclim--check-project' if neccessary."
  (let ((res (gensym))
	(expargs (gensym))
	(sync (eclim--args-contains args '("-f" "-o")))
	(check (eclim--args-contains args '("-p"))))
    `(let* ((,expargs (eclim--expand-args (quote ,args))))
       ,(when sync '(eclim/java-src-update))
       ,(when check '(eclim--check-project (eclim--project-name)))
       (let ((,res (apply 'eclim--call-process ,cmd ,expargs)))
	 (when (and ,res (or (string-match "connect:\s+\\(.*\\)" (first ,res))
			     (string-match "Missing argument for required option:\s*\\(.*\\)" (first ,res))))
	   (error (match-string 1 (first ,res))))
	 ,(when sync `(when (file-exists-p (buffer-file-name))
			(revert-buffer t t t)))
	 ,res))))

(defmacro eclim/with-results (result params &rest body)
  "Convenience macro. PARAMS is a list where the first element is
CMD to execute and the rest an ARGS list. Calls eclim with CMD
and the expanded ARGS list and binds RESULT to the results. If
RESULT is non-nil, BODY is executed."
  (let ((sync (eclim--args-contains (rest params) (list "-f" "-o"))))
    `(let* ((,result (eclim/execute-command ,@params))
	    (eclim-auto-save (and eclim-auto-save (not ,sync))))
       (when ,result
	 ,@body))))

(defun eclim--completing-read (prompt choices)
  (funcall eclim-interactive-completion-function prompt choices))

(defun eclim--project-dir ()
  "return this file's project root directory."
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

(defun eclim--find-display-results (pattern results &optional open-single-file)
  (let ((res (remove-if (lambda (r) (zerop (length (remove-if (lambda (r) (zerop (length r))) r)))) results)))
    (if (and (= 1 (length res)) open-single-file) (eclim--visit-declaration (car res))
      (pop-to-buffer (get-buffer-create "*eclim: find"))
      (let ((buffer-read-only nil))
	(erase-buffer)
	(insert (concat "-*- mode: eclim-find; default-directory: " default-directory " -*-"))
	(newline 2)
	(insert (concat "eclim java_search -p " pattern))
	(newline)
	(dolist (result res)
	  (insert (eclim--convert-find-result-to-string result default-directory))
	  (newline))
	(grep-mode)))))

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

(defun eclim--byte-offset (&optional text)
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

(defun eclim/workspace-dir ()
  (car (eclim--call-process "workspace_dir")))

(defun eclim/jobs (&optional family)
  (eclim--call-process (eclim--build-command "jobs"
                                             "-f" family)))

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
(require 'eclim-problems)

(provide 'eclim)
