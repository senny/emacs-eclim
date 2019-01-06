;; eclim-java-run.el --- an interface to the Eclipse IDE. -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2015 Łukasz Klich
;;
;; Author: Lukasz Klich <klich.lukasz@gmail.com>
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
;; "eclim--<descriptive-name>", and name external program invocations
;; "eclim/command-name", like eclim/project-list.
;;; Description
;;
;; eclim-java-run.el -- java run configurations for eclim
;;

(require' eclim-project)
(require 'eclim-java)
(require 's)
(require 'dash)

(define-key eclim-mode-map (kbd "C-c C-e u r") 'eclim-java-run-run)

(defun eclim-java-run-sourcepath (project)
  (let ((projects (-snoc (eclim-project-dependencies project) project)))
    (s-join ":" (-mapcat 'eclim-java-run--project-sourcepath projects))))

(defun eclim-java-run--project-dir (project)
  (file-name-as-directory (cdr (assoc 'path (eclim/project-info project)))))

(defun eclim-java-run--project-sourcepath (project)
  (eclim-java-run--read-sourcepath
   (concat (eclim-java-run--project-dir project)
           ".classpath")))

(defun eclim-java-run--read-sourcepath (classpath-file)
  (let* ((root (car (xml-parse-file classpath-file)))
         (classpathentries (xml-get-children root 'classpathentry))
         (srcs (-filter 'eclim-java-run--src?? classpathentries))
         (paths-relative (-map 'eclim-java-run--get-path srcs))
         (paths-absolute (--map (concat (file-name-directory classpath-file) it) paths-relative)))
    paths-absolute))

(defun eclim-java-run--src?? (classpathentry)
  (let* ((attrs (xml-node-attributes classpathentry))
         (kind (cdr (assq 'kind attrs))))
    (string-equal kind "src")))

(defun eclim-java-run--get-path (classpathentry)
  (let* ((attrs (xml-node-attributes classpathentry))
         (path (cdr (assq 'path attrs))))
    path))

(defun eclim-java-run--get-string-from-file (file-path)
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(defun eclim-java-run--load-configurations (project)
  (let* ((configurations-path (concat (eclim-java-run--project-dir project) ".eclim"))
         (configurations (read (eclim-java-run--get-string-from-file configurations-path))))
    configurations))

(defun eclim-java-run--get-value (key alist)
  (cdr (assoc key alist)))

(defun eclim-java-run--jdb? (config)
  (and (eclim-java-run--get-value 'debug config)
       (not (eclim-java-run--get-value 'debug-port config))))

(defun eclim-java-run--java-vm-args (classpath)
  (lambda (config)
    (concat "-classpath" (when (not (eclim-java-run--jdb? config)) " ")
            classpath " "
            (eclim-java-run--get-value 'vm-args config))))

(defun eclim-java-run--debug-vm-args (classpath sourcepath)
  (lambda (config)
    (concat "-sourcepath"
            sourcepath " "
            (funcall (eclim-java-run--java-vm-args classpath) config))))

(defun eclim-java-run--command (config vm-args-fn)
  (s-join " " (-flatten
               (list
                (if (eclim-java-run--jdb? config) "jdb" "java")
                (funcall vm-args-fn config)
                (eclim-java-run--get-value 'main-class config)
                (eclim-java-run--get-value 'program-args config)))))

(defun eclim-java-run--run-jdb (config classpath sourcepath project-dir)
  (let* ((command (eclim-java-run--command config
                                           (eclim-java-run--debug-vm-args classpath sourcepath))))
    (with-temp-buffer
      (setq default-directory project-dir)
      (eclim-debug/jdb command))))

(defun eclim-java-run--run-java (config classpath project-dir)
  (let* ((name (eclim-java-run--get-value 'name config))
         (command (eclim-java-run--command config (eclim-java-run--java-vm-args classpath)))
         (new-buffer-name (concat "*" name "*")))
    (when (buffer-live-p (get-buffer new-buffer-name)) (kill-buffer new-buffer-name))
    (with-temp-buffer
      (setq default-directory project-dir)
      (switch-to-buffer (process-buffer
                         (start-process-shell-command name new-buffer-name command))))))

(defun eclim-java-run--configuration (name confs)
  (car
   (--filter (string-equal (cdr (assoc 'name it)) name) confs)))

(defun eclim-java-run--ask-which-configuration ()
  (completing-read "Which configuration do you want to run?"
                   (--map (cdr (assoc 'name it))
                          (eclim-java-run--load-configurations (eclim-project-name)))
                   nil t))

(defun eclim-java-run-run (configuration-name)
  (interactive (list (eclim-java-run--ask-which-configuration)))
  (let* ((configurations (eclim-java-run--load-configurations (eclim-project-name)))
         (configuration (eclim-java-run--configuration configuration-name configurations))
         (project-dir (eclim-java-run--project-dir (eclim-project-name)))
         (classpath (eclim/java-classpath (eclim-project-name)))
         (debug? (eclim-java-run--jdb? configuration)))
    (if debug?
        (eclim-java-run--run-jdb configuration
                                 classpath
                                 (eclim-java-run-sourcepath (eclim-project-name))
                                 project-dir)
      (eclim-java-run--run-java configuration
                                classpath
                                project-dir))))

(defun eclim-run-configuartion (configuration-name)
      "Runs the configuration given in CONFIGURATION-NAME in the compilation buffer."
      (interactive (list (eclim-java-run--ask-which-configuration)))
      (let* ((configurations (eclim-java-run--load-configurations (eclim-project-name)))
             (configuration (eclim-java-run--configuration configuration-name configurations))
             (project-dir (eclim-java-run--project-dir (eclim-project-name)))
             (classpath (eclim/java-classpath (eclim-project-name)))
             (default-directory project-dir)
             (command (eclim-java-run--command configuration (eclim-java-run--java-vm-args classpath))))
        (compile command)))

(provide 'eclim-java-run)
;;; eclim-java-run.el ends here
