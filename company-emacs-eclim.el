;; company-emacs-eclim.el --- an interface to the Eclipse IDE.
;;
;; Copyright (C) 2009-2012   Fredrik Appelberg
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
;; "eclim--<descriptive-name>", and name eclim command invocations
;; "eclim/command-name", like eclim/project-list.
;;; Description
;;
;; company-emacs-eclim.el -- a company-mode backend that replaces company-eclim
;;
;; To activate this backend, replace company-eclim with
;; company-emacs-eclim in the eclim-backends list, or call the
;; convenience function company-emacs-eclim-setup.

;;* Eclim Company

(require 'eclim)
(require 'eclim-completion)
(require 'eclim-java)
(require 'company)

(defvar cee--candidates nil)
(make-variable-buffer-local 'cee--candidates)

(defun company-emacs-eclim-setup ()
  "Convenience function that adds company-emacs-eclim to the list
  of available company backends."
  (flet ((replace-recur (elt rpl lst)
												(cond ((null lst) nil)
															((listp (first lst)) (cons (replace-recur elt rpl (first lst))
																												 (replace-recur elt rpl (rest lst))))
															(t (cons (if (equal (first lst) elt) rpl (first lst))
																			 (replace-recur elt rpl (rest lst)))))))
    (let ((replaced (replace-recur 'company-eclim 'company-emacs-eclim company-backends)))
      (setq company-backends
						(if (eq replaced company-backends)
								(cons 'company-emacs-eclim company-backends)
							replaced)))))

(defun cee--candidates (prefix)
	(setq eclim--completion-start (- (point) (length prefix)))
	(setq cee--candidates (eclim--completion-candidates)))

(defun cee--lookup-candidate (lookup)
  "Looks up the candidate record that matches the string inserted
by company-mode in the list of eclim-matches."
  (find lookup cee--candidates
				:key #'eclim--completion-candidate-doc
				:test #'string=))

(defun company-emacs-eclim (command &optional arg &rest ignored)
  "A `company-mode' back-end for eclim completion"
  (interactive)
  (case command
    ('prefix (and (derived-mode-p 'java-mode 'jde-mode)
                  buffer-file-name
                  eclim-executable
                  (eclim--project-name)
                  (not (company-in-string-or-comment))
                  (or (company-grab-symbol) 'stop)))
    ('candidates (cee--candidates arg))
    ('meta (eclim--completion-candidate-doc (cee--lookup-candidate arg)))
    ('no-cache (equal arg ""))))

(defun cee--completion-finished (arg)
	(eclim--completion-action))

(add-hook 'company-completion-finished-hook
					'cee--completion-finished)

(provide 'company-emacs-eclim)
