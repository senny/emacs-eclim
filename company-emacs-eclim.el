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
 
(defun company-emacs-eclim (command &optional arg &rest ignored)
  "A `company-mode' back-end for eclim completion"
  (interactive)
  (case command
    ('prefix (buffer-substring-no-properties (eclim-completion-start) (point)))
    ('candidates (eclim--completion-candidates))
    ('meta (eclim--completion-documentation arg))
    ('no-cache (equal arg ""))))

(add-hook 'company-completion-finished-hook
          (lambda (arg) (eclim--completion-action)))

(provide 'company-emacs-eclim)
