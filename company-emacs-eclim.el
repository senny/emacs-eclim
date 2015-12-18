;;; company-emacs-eclim.el --- an interface to the Eclipse IDE.
;;
;; Copyright (C) 2009-2012   Fredrik Appelberg
;; Copyright (C) 2013-2014   Dmitry Gutov
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
;;; Commentary:
;;
;; company-emacs-eclim.el -- company-mode backend that replaces company-eclim
;;
;; To activate this backend, replace company-eclim and/or company-nxml
;; with company-emacs-eclim in the eclim-backends list, or call the
;; convenience function company-emacs-eclim-setup.
;;
;; Minimum company-mode version required: 0.7.

;;; Code:

;;* Eclim Company

(require 'eclim)
(require 'eclim-completion)
(require 'eclim-java)
(require 'company)
(require 'cl-lib)

(defcustom company-emacs-eclim-ignore-case t
  "If t, case is ignored in completion matches."
  :group 'eclim-company
  :type '(choice (const :tag "Yes" t)
                 (const :tag "No" nil)))

;;;###autoload
(defun company-emacs-eclim-setup ()
  "Convenience function that adds company-emacs-eclim to the list
  of available company backends."
  (setq company-backends
        (cons 'company-emacs-eclim
              (cl-remove-if (lambda (b) (cl-find b '(company-nxml company-eclim)))
                            company-backends))))

(defun company-emacs-eclim--before-prefix-in-buffer (prefix)
  "Search for the text before prefix that may be included as part of completions"
  (ignore-errors
    (save-excursion
      (let ((end (progn
                   (backward-char (length prefix))
                   (point)))
            (start (progn
                     (while (save-excursion
                              (backward-char)
                              (eq ?. (char-after)))
                       (backward-char)
                       (beginning-of-thing 'symbol))
                     (point))))
        (buffer-substring-no-properties start end)))))

(defun company-emacs-eclim--candidates (prefix)
  (let ((before-prefix-in-buffer (company-emacs-eclim--before-prefix-in-buffer prefix)))
    (cl-labels
        ((annotate (str)
                   (if (string-match "(" str)
                       (propertize
                        (substring str 0 (match-beginning 0)) 'eclim-meta str)
                     str))
         (without-redundant-prefix (str)
                                   (if (and before-prefix-in-buffer
                                            (> (length before-prefix-in-buffer) 0)
                                            (string-prefix-p before-prefix-in-buffer str))
                                       (substring str (length before-prefix-in-buffer))
                                     str)))
      (mapcar
       (lambda (candidate)
         (annotate (without-redundant-prefix candidate)))
       ;; Company says backend is responsible for filtering prefix case.
       (if company-emacs-eclim-ignore-case
           (eclim--completion-candidates)
         (cl-remove-if-not #'(lambda(str) (string-prefix-p prefix str))
                           (eclim--completion-candidates)))))))

(defun company-emacs-eclim--annotation (candidate)
  (let ((str (get-text-property 0 'eclim-meta candidate)))
    (when (and str (string-match "(" str))
      (substring str (match-beginning 0)))))

;;;###autoload
(defun company-emacs-eclim (command &optional arg &rest ignored)
  "`company-mode' back-end for Eclim completion"
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-emacs-eclim))
    (prefix (let ((start (and eclim-mode
                              (eclim--accepted-p (buffer-file-name))
                              (eclim-completion-start))))
              (when start (buffer-substring-no-properties start (point)))))
    (candidates (company-emacs-eclim--candidates arg))
    (annotation (company-emacs-eclim--annotation arg))
    (meta (eclim--completion-documentation
           (concat arg (company-emacs-eclim--annotation arg))))
    (no-cache (equal arg ""))
    (ignore-case company-emacs-eclim-ignore-case)
    (sorted t)
    (post-completion (let ((ann (company-emacs-eclim--annotation arg)))
                       (when ann
                         (insert ann))
                       (company-emacs-eclim-action arg ann)))))

(defun company-emacs-eclim-action (completion annotation)
  (let* ((end (point))
         (len (+ (length completion) (length annotation)))
         (beg (- end len)))
    (eclim--completion-action beg end)))

(provide 'company-emacs-eclim)
;;; company-emacs-eclim.el ends here
