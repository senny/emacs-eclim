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
(require 'gud)
(require 'dash)
(require 's)

(define-key eclim-mode-map (kbd "C-c C-e p t") 'eclim-debug-test)
(define-key eclim-mode-map (kbd "C-c C-e p a") 'eclim-debug-attach)

(defun eclim--debug-jdb-attach-command (project port)
  (let ((sourcepath (eclim--debug-sourcepath project)))
    (format "jdb -attach %s -sourcepath%s "
            port
            sourcepath)))

(defun eclim--debug-jdb-run-command (project args)
  (let ((sourcepath (eclim--debug-sourcepath project))
        (classpath (eclim/java-classpath project)))
    (concat (format "jdb -sourcepath%s -classpath%s "
                    sourcepath
                    classpath)
            (s-join " " args))))

(defun eclim--debug-sourcepath (project)
  (let ((projects (-snoc (eclim-project-dependencies project) project)))
    (s-join ":" (-mapcat 'eclim--debug-project-sourcepath projects))))

(defun eclim--debug-project-dir (project)
  (file-name-as-directory (cdr (assoc 'path (eclim/project-info project)))))

(defun eclim--debug-project-sourcepath (project)
  (eclim--debug-read-sourcepath
   (concat (eclim--debug-project-dir project)
           ".classpath")))

(defun eclim--debug-read-sourcepath (classpath-file)
  (let* ((root (car (xml-parse-file classpath-file)))
         (classpathentries (xml-get-children root 'classpathentry))
         (srcs (-filter 'eclim--debug-kind-src? classpathentries))
         (paths-relative (-map 'eclim--debug-get-path srcs))
         (paths-absolute (--map (concat (file-name-directory classpath-file) it) paths-relative)))
    paths-absolute))

(defun eclim--debug-kind-src? (classpathentry)
  (let* ((attrs (xml-node-attributes classpathentry))
         (kind (cdr (assq 'kind attrs))))
    (string-equal kind "src")))

(defun eclim--debug-get-path (classpathentry)
  (let* ((attrs (xml-node-attributes classpathentry))
         (path (cdr (assq 'path attrs))))
    path))

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
  (let* ((project-dir (eclim--debug-project-dir eclim-project-name))
         (file (concat project-dir filename)))
    (file-exists-p file)))

(defun eclim--debug-run-process-and-attach (command port)
  (let ((project eclim-project-name))
    (with-current-buffer (compile command t)
      (setq-local comint-prompt-read-only t)
      (make-local-variable 'comint-output-filter-functions)
      (add-hook 'comint-output-filter-functions
                (lambda (txt) (eclim--debug-attach-when-ready txt project port))))))

(defun eclim-debug-junit ()
  (interactive)
  (let ((project eclim-project-name)
        (classes (list "org.junit.runner.JUnitCore"
                       (eclim-package-and-class))))
    (jdb (eclim--debug-jdb-run-command project classes))))

(defun eclim-debug-maven-test ()
  (interactive)
  (eclim--debug-run-process-and-attach (eclim--debug-maven-run) 5005))

(defun eclim-debug-ant-test ()
  (interactive)
  (eclim--debug-run-process-and-attach (eclim--debug-ant-run) 5005))

(defun eclim-debug-attach (port project)
  (interactive (list (read-number "Port: " 5005) eclim-project-name))
  (jdb (eclim--debug-jdb-attach-command project port)))

(defun eclim-debug-test ()
  (interactive)
  (cond ((eclim-java-junit-buffer?) (eclim-debug-junit))
        ((eclim--debug-project-maven?) (eclim-debug-maven-test))
        ((eclim--debug-projecta-ant?) (eclim-debug-ant-test))
        (t (message "I can't debug this. I wasn't program smart enough. Please help me"))))

(provide 'eclim-debug)
