;; eclim-debug.el --- an interface to the Eclipse IDE. -*- lexical-binding: t; -*-
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
;; eclim-debug.el -- emacs-eclim integration with gud and jdb to
;; support debugging
;;

(require 'eclim-project)
(require 'eclim-java)
(require 'eclim-maven)
(require 'eclim-ant)
(require 'eclim-java-run)
(require 'gud)
(require 'dash)
(require 's)

(define-key eclim-mode-map (kbd "C-c C-e p t") 'eclim-debug-test)
(define-key eclim-mode-map (kbd "C-c C-e p a") 'eclim-debug-attach)

(defun eclim--debug-jdb-run-command (project main-class args)
  (let ((config `((name . ,(concat "*Debug - " main-class "*"))
                  (debug . t)
                  (main-class . ,main-class)
                  (program-args . ,args)
                  (vm-args . ,(concat "-sourcepath" (eclim-java-run-sourcepath project)))))
        (classpath (eclim/java-classpath project)))
    (eclim-java-run--command config (eclim-java-run--java-vm-args classpath))))

(defun eclim--debug-jdb-attach-command (project port)
  (let ((sourcepath (eclim-java-run-sourcepath project)))
    (format "jdb -attach %s -sourcepath%s "
            port
            sourcepath)))

(defun eclim--debug-attach-when-ready (txt project port)
  (when (s-contains? (concat "at address: " (number-to-string port)) txt)
    (remove-hook 'comint-output-filter-functions
                 'eclim--debug-attach-when-ready
                 t)
    (eclim-debug-attach port project)))

(defun eclim--debug-maven-run ()
  (concat "mvn -f " (eclim--maven-pom-path)
          "clean test -Dmaven.surefire.debug -Dtest=" (file-name-base)))

(defun eclim--debug-project-maven? ()
  (eclim--debug-file-exists-in-project-root? "pom.xml"))

(defun eclim--debug-ant-run (target)
  (let ((default-directory (eclim--ant-buildfile-path)))
    "ANT_OPTS=\"$ANT_OPTS -Xdebug -Xrunjdwp:transport=dt_socket,server=y,suspend=y,address=5005\" ant test"))

(defun eclim--debug-project-ant? ()
  (eclim--debug-file-exists-in-project-root? "build.xml"))

(defun eclim--debug-file-exists-in-project-root? (filename)
  (let* ((project-dir (eclim-java-run--project-dir (eclim-project-name)))
         (file (concat project-dir filename)))
    (file-exists-p file)))

(defun eclim--debug-run-process-and-attach (command port)
  (let ((project (eclim-project-name)))
    (with-current-buffer (compile command t)
      (setq-local comint-prompt-read-only t)
      (make-local-variable 'comint-output-filter-functions)
      (add-hook 'comint-output-filter-functions
                (lambda (txt) (eclim--debug-attach-when-ready txt project port))))))

(defun eclim-debug/jdb (command)
  (let ((buffer (current-buffer)))
    (toggle-maximize-buffer)
    (switch-to-buffer-other-window buffer t)
    (jdb command)
    (switch-to-buffer-other-window buffer t)))

(defun eclim-debug-junit ()
  (interactive)
  (let ((project (eclim-project-name))
        (classes (eclim-package-and-class)))
    (eclim-debug/jdb
     (eclim--debug-jdb-run-command project "org.junit.runner.JUnitCore" classes))))

(defun eclim-debug-maven-test ()
  (interactive)
  (eclim--debug-run-process-and-attach (eclim--debug-maven-run) 5005))

(defun eclim-debug-ant-test ()
  (interactive)
  (eclim--debug-run-process-and-attach (eclim--debug-ant-run) 5005))

(defun eclim-debug-attach (port project)
  (interactive (list (read-number "Port: " 5005) (eclim-project-name)))
  (eclim-debug/jdb (eclim--debug-jdb-attach-command project port)))

(defun eclim-debug-test ()
  (interactive)
  (cond ((eclim-java-junit-buffer?) (eclim-debug-junit))
        ((eclim--debug-project-maven?) (eclim-debug-maven-test))
        ((eclim--debug-projecta-ant?) (eclim-debug-ant-test))
        (t (message "I can't debug this. I wasn't program smart enough. Please help me"))))

(provide 'eclim-debug)
