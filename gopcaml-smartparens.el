;;; gopcaml-smartparens.el --- Support for smartparens in Gopcaml mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Kiran Gopinathan

;; Author: Kiran Gopinathan <kirang@comp.nus.edu.sg>
;; Keywords: tools, languages

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(require 'smartparens)

(defun gopcaml-state-sexp-filter (cmd)
  "Determines whether a CMD can be carried out in current Gopcaml mode state
or whether a smart-parens based operation is more suitable."
  (cond
   ;; if we're looking at a opening or closing parens - smart parens
   ;; is better
   ((or
     (save-match-data
       (sp--looking-back (sp--get-closing-regexp
			  (sp--get-pair-list-context 'navigate))))
     (save-match-data
       (sp--looking-at (sp--get-opening-regexp
			(sp--get-pair-list-context 'navigate))))
     ) nil)
   ((and gopcaml-state (gopcaml-state-available-filter))
    cmd
    )
   )
  )

(define-key gopcaml-mode-map (kbd "C-M-f") '(menu-item "" gopcaml-forward-sexp
						       :filter gopcaml-state-sexp-filter))

(define-key gopcaml-mode-map (kbd "C-M-b") '(menu-item "" gopcaml-backward-sexp
						       :filter gopcaml-state-sexp-filter))
(define-key gopcaml-mode-map (kbd "C-M-t") '(menu-item "" gopcaml-zipper-transpose
						       :filter gopcaml-state-filter))
(define-key gopcaml-mode-map (kbd "C-M-S-f") '(menu-item "" gopcaml-forward-sexp-selection
							 :filter gopcaml-state-sexp-filter))
(define-key gopcaml-mode-map (kbd "C-M-S-b") '(menu-item "" gopcaml-backward-sexp-selection
							 :filter gopcaml-state-sexp-filter))



(provide 'gopcaml-smartparens)
;;; gopcaml-smartparens.el ends here
