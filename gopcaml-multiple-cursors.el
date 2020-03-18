;;; gopcaml-multiple-cursors.el --- Support for multi-cursors in Gopcaml mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Kiran Gopinathan

;; Author: Kiran Gopinathan <kirang@comp.nus.edu.sg>
;; Keywords: languages, tools

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

(require 'multiple-cursors)

(defun gopcaml-extract-expression (start end)
  "Attempt to extract the expression in the current region START END."
  (interactive "^r")
  (when (and gopcaml-state (gopcaml-state-available-filter) (< (mc/num-cursors) 2))
    (let ((text (buffer-substring-no-properties start end))
	  bounds matches marks master)
      (setq bounds (car (gopcaml-find-extract-scope text start end)))
      (when (and bounds)
	;; retrieve list of matches in scope
	(setq matches (match-seq text (car bounds) (cdr bounds)))
	;; find which matches are valid
	(setq matches (gopcaml-find-valid-matches (car bounds) matches (car bounds) (cdr bounds)))
	(when matches
	  (push-mark)
	  ;; now we have a list of valid matches and a point to insert them
	  ;; convert each match to a list of markers
	  (dolist (match matches)
	    (push (cons
		   (set-marker (make-marker) (+ (car match) 1))
		   (set-marker (make-marker) (cdr match))
		   ) marks)
	    )
	  ;; now ready to edit
	  (goto-char (car bounds))
	  (setq text (string-join (list text " in\n" (make-string (current-column) 32)) "") )
	  (insert (format "let  = %s" text))

	  ;; move to insert point
	  (goto-char (+ (car bounds) 4))
	  (setq master (point))
	  (push-mark master)
	  ;; remove fake cursors if present
	  (mc/remove-fake-cursors)
	  (mc/save-excursion
	   ;; delete all the ocurrances
	   (dolist (mark marks)
	     (let ((start (car mark)) (end (cdr mark)))
	       ;; move to start of ocurrance
	       (push-mark (-(marker-position start) 1))
	       (exchange-point-and-mark)
	       ;; delete ocurrance
	       (delete-region
		(- (marker-position start) 1) (marker-position end))
	       ;; create cursor at the point
	       (mc/create-fake-cursor-at-point)
	       ;; clean up the markers
	       (set-marker start nil)
	       (set-marker end nil)
	       )
	     ))
	  ;; finally enable multiple-cursors-mode
	  (mc/maybe-multiple-cursors-mode)
	  )
	)
      ))
  )
(defun gopcaml-zipper-extract-expression ()
  "Extracts the current item and exits zipper mode."
  (interactive)
  (gopcaml-zipper-use-current-and-quit #'gopcaml-extract-expression))


(define-key gopcaml-mode-map (kbd "C-c C-e") #'gopcaml-extract-expression)
(define-key merlin-mode-map (kbd "C-c C-e") #'gopcaml-extract-expression)
(define-key gopcaml-zipper-mode-map (kbd "C-c C-e") #'gopcaml-zipper-extract-expression)


(provide 'gopcaml-multiple-cursors)
;;; gopcaml-multiple-cursors.el ends here

