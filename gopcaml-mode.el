;;; gopcaml-mode.el --- Structural Ocaml Editing     -*- lexical-binding: t; -*-

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

;; load in core Gopcaml library

(cond
 ((and (boundp 'gopcaml-dev-mode) gopcaml-dev-mode)
  ;; if in dev mode,
  (message "loading gopcaml-mode [dev]")
  (require 'gopcaml
	   ;; load dynamic module from the build directory
	   (expand-file-name "./_build/default/gopcaml.so"
			     (string-trim
			      (shell-command-to-string (format "dirname %s"
							       (or (and (boundp 'file) file)
								   (buffer-file-name (current-buffer))
								   )))))))
 (t
  (message "loading gopcaml-mode [normal]")
  (let ((opam-lib (ignore-errors (car (process-lines "opam" "config" "var" "lib")))))
    (if (and opam-lib (file-directory-p opam-lib))
	(require 'gopcaml (expand-file-name "./gopcaml-mode/gopcaml.so" opam-lib))
      (error "Could not find opam - please make sure it is installed")
      ))))

;; load in emacs dependencies
(require 'subr-x)
;; load in merlin/ocp deps
(require 'ocp-indent)
(require 'merlin)

;;; Code:

(defgroup gopcaml-faces nil
  "Faces for gopcaml mode."
  :group 'gopcaml-mode
  :group 'faces)

(defface gopcaml-highlight-face
  '((t :inherit caml-types-expr-face))
  "Face for highlighting expr."
  :group 'gopcaml-faces)

(defface gopcaml-zipper-face
  '((t (:background "dark slate gray")))
  "Face for highlighting zipper."
  :group 'gopcaml-faces)

(defvar-local gopcaml-temporary-highlight-overlays nil
  "Maintains an the overlay used for single-element highlights.")

(defcustom gopcaml-rebuild-delay 1
  "Number of idling seconds before rebuilding gopcaml-state."
  :type 'integer
  :group 'gopcaml)

(defvar-local gopcaml-zipper-overlay nil
  "Overlay used to highlight the zipper region.")

(defvar-local gopcaml-update-timer nil
  "Timer object used to periodically update gopcaml state.")

(defvar-local  gopcaml-expand-timer nil
  "Timer object used to periodically expand the element under point")

(defvar-local gopcaml-zipper-mode-quit nil
  "Function that can be called to quit the zipper mode.")

(defun gopcaml-remove-stored-overlays (&optional group)
  "Remove stored overlays - optionally only those of gopcaml-kind GROUP."
  (setq gopcaml-temporary-highlight-overlays
	(remove-if (lambda (it) (null it))
		   (mapcar (lambda (it)
			     (if (or (not group)
				     (equal (overlay-get it 'gopcaml-kind)
					    group))
				 (delete-overlay it)
			       nil))
			   gopcaml-temporary-highlight-overlays))))

(defun gopcaml-store-overlays (overlay &optional group)
  "Store an OVERLAY - optionally only those of gopcaml-kind GROUP."
  (overlay-put overlay 'gopcaml-kind group)
  (push overlay gopcaml-temporary-highlight-overlays))

(defun gopcaml-temporarily-highlight-region (bounds &optional group face)
  "Temporarily highlight region enclosed by BOUNDS using FACE.
removes all existing overlays of type GROUP if present."
  (unless face
    (setq face 'gopcaml-highlight-face))
  ;; when group is present, remove all existing overlays of the same group
  (gopcaml-remove-stored-overlays group)
  (lexical-let ((overlay (make-overlay (car bounds) (cdr bounds))))
    (overlay-put overlay 'face face)
    (gopcaml-store-overlays overlay group)
    ;; wait for user input
    (unwind-protect
	(sit-for 60) (gopcaml-remove-stored-overlays group))))

(defun gopcaml-temporarily-highlight-multiple-region (mbounds &optional group face)
  "Temporarily highlight regions listed in MBOUNDS using FACE.
removes all existing overlays of type GROUP if present."
  (unless face
    (setq face 'gopcaml-highlight-face))
  ;; when group is present, remove all existing overlays of the same group
  (gopcaml-remove-stored-overlays group)
  (dolist (bounds mbounds)
    (lexical-let ((overlay (make-overlay (car bounds) (cdr bounds))))
      (overlay-put overlay 'face face)
      (gopcaml-store-overlays overlay group)))
  ;; wait for user input
  (unwind-protect
      (sit-for 60) (gopcaml-remove-stored-overlays group))
  )

(defun gopcaml-highlight-current-structure-item ()
  "Highlight the structure-item enclosing the current point."
  (interactive)
  (let ((area (gopcaml-get-enclosing-bounds (point)))
	start end)
    (when area
      (setq start (caar area))
      (setq end (cadar area))
      (gopcaml-temporarily-highlight-region (cons start end)))))

(defun gopcaml-highlight-dirty-region ()
  "Highlight the dirty region."
  (interactive)
  (let ((area (car (gopcaml-get-dirty-region)))
	start end)
    (when area
      (setq start (car area))
      (setq end (cdr area))
      (gopcaml-temporarily-highlight-region (cons start end)))))

(defun move-gopcaml-zipper (zipper-fn &optional direction initial)
  "Move the zipper using ZIPPER-FN in direction DIRECTION."
  (let ((area (car (gopcaml-retrieve-zipper-bounds)))
	start end (point (point)) (buf-name (buffer-name)))
    (if area
	(progn
	  (setq start (car area))
	  (setq end (cadr area))
	  (cond
	   ((and
	     (not initial)
	     (equal point start)
	     (equal direction 'forward)
	     )
	    (goto-char end)
	    t)
	   ((and
	     (not initial)
	     (equal point end)
	     (equal direction 'backward)
	     )
	    (goto-char start)
	    t)
	   ((and
	     initial
	     (< point start)
	     (equal direction 'forward)
	     )
	    (goto-char start)
	    t)
	   ((and
	     initial
	     (> point end)
	     (equal direction 'backward)
	     )
	    (goto-char end)
	    t)
	   (t
	    (setq area (car (funcall zipper-fn)))
	    (if area
		(progn
		  (setq start (car area))
		  (setq end (cadr area))
		  (move-overlay gopcaml-zipper-overlay start end)
		  ;; (let ((wind-start (window-start)) (wind-end (window-end)))
		  ;;   (if (and wind-start wind-end (< wind-start start wind-end))
		  ;; 	(set-window-point (get-buffer-window buf-name) start)
		  ;;     (goto-char start))
		  ;;   )
		  (if (equal direction 'forward) (if (< end (window-end))
						     (goto-char end)
						   (goto-char start))
		    (if (> start (window-start))
			(goto-char start)
		      (goto-char end)
		      ))
		  t
		  )
	      nil))))
      nil)))

(defun gopcaml-zipper-mode-and-move (operation &optional
					       zipper-constructor selection-mode direction skip-zipper-mode)
  "Start gopcaml-zipper mode using ZIPPER-CONSTRUCTOR and perform OPERATION.
SELECTION-MODE indicates whether this was called in the context
of an existing selection, allowing for some slightly more
ergonomic movement when dealing with selections.  DIRECTION
indicates the direction of the movement - allows skipping
whitespace in direction before constructing zipper.
SKIP-ZIPPER-MODE if set will prevent the activation zipper mode."
  (interactive)
  (if  (not zipper-constructor)
      (setq zipper-constructor #'gopcaml-build-zipper))
  (let ((starting-pos (point))
	(selection-active (or (region-active-p) selection-mode)))
    (if (and selection-active (not (region-active-p)) (not skip-zipper-mode))
	;; if selection active and not region active
	;; i.e we're holding shift, but haven't selecting anything
	(progn
	  (push-mark (point))
	  (activate-mark))
      )

    ;;  if not already in zipper mode
    (if (not gopcaml-zipper)
	;; build zipper aronud point
	(progn
	  (cond
	   ((equal direction 'forward)
	    (skip-chars-forward " \n\t"))
	   ((equal direction 'backward)
	    (skip-chars-backward " \n\t")
	    ))
	  (message "calling constructor with %s" direction)
	  (let ((area
		 (car (funcall zipper-constructor
			       (point)
			       (line-number-at-pos)
			       (list direction))))
		start end overlay)
	    ;; if successfull then perform operation
	    (if area
		(progn
		  (setq start (car area))
		  (setq end (cadr area))
		  (cond
		   ;; if skip-zipper mode
		   (skip-zipper-mode
		    ;; just perform the operation and return the range
		    (setq area (car (funcall operation t)))
		    (gopcaml-delete-zipper)
		    (goto-char starting-pos)
		    area)
		   (t
		    (setq overlay (make-overlay start end))
		    (if (not selection-active)
			;; (overlay-put overlay 'face 'gopcaml-selection-face)
			(overlay-put overlay 'face 'gopcaml-zipper-face)
		      )
		    (overlay-put overlay 'gopcaml-kind 'zipper)
		    (setq gopcaml-zipper-overlay overlay)
		    (setq gopcaml-zipper-mode-pred t)
		    (setq gopcaml-zipper-mode-quit
			  (set-transient-map
			   (if (not selection-active)
			       gopcaml-zipper-mode-map
			     gopcaml-selection-zipper-mode-map)
			   t #'gopcaml-on-exit-zipper-mode))
		    ;; (funcall operation t)
		    (cond
		     ((equal direction 'forward)
		      (goto-char end)
		      )
		     ((equal direction 'backward)
		      (goto-char start)
		      )))))
	      (gopcaml-delete-zipper)
	      (when skip-zipper-mode
		(goto-char starting-pos))
	      nil
	      )))
      ;; otherwise just perfom operation
      (cond
       (skip-zipper-mode
	;; just perform the operation and return the range
	(let ((area (car (funcall operation t))))
	  (if gopcaml-zipper-mode-quit
	      (funcall gopcaml-zipper-mode-quit)
	    (gopcaml-delete-zipper))
	  area
	  ))
       (t (funcall operation nil)))
      )))

(defun gopcaml-zipper-mark-mode ()
  "Start gopcaml-zipper mark mode."
  (interactive)
  (let ((starting-pos (point))
	(selection-active (and transient-mark-mode mark-active)))
    ;;  if not already in zipper mode
    (if selection-active (setq starting-pos (min (mark) (region-end) (region-beginning) starting-pos)))
    (if (not gopcaml-zipper)
	;; build zipper aronud point
	(progn
	  (skip-chars-forward " \n\t")

	  (let ((area
		 (car (gopcaml-build-zipper
		       (point)
		       (line-number-at-pos)
		       nil)))
		start end overlay)
	    ;; if successfull then perform operation
	    (if area
		(progn
		  (setq start (car area))
		  (setq end (cadr area))
		  (setq overlay (make-overlay start end))
		  (overlay-put overlay 'gopcaml-kind 'zipper)
		  (setq gopcaml-zipper-overlay overlay)
		  (setq gopcaml-zipper-mode-quit
			(set-transient-map
			 gopcaml-mark-zipper-mode-map
			 t #'gopcaml-on-exit-zipper-mode))
		  ;; (funcall operation t)
		  (if selection-active
		      (progn
			(save-excursion
			  (goto-char (max end (region-end)))
			  (push-mark (point))
			  (activate-mark))
			(goto-char (min start starting-pos)))
		    (progn
		      (save-excursion
			(goto-char end)
			(set-mark (point))
			(activate-mark))
		      (goto-char (min start starting-pos))))
		  )
	      (gopcaml-delete-zipper)
	      )))
      ;; otherwise just perfom operation
      (let ((area (car (gopcaml-move-zipper-up))))
	(if area
	    (progn
	      (setq start (car area))
	      (setq end (cadr area))
	      (move-overlay gopcaml-zipper-overlay start end)
	      ;; (funcall operation t)
	      (save-excursion
		(goto-char (max end (region-end)))
		(set-mark (point))
		(activate-mark))
	      (goto-char (min start (region-beginning)))
	      )
	  )
	))))

(defun gopcaml-get-current-item-bounds ()
  "Return the bounds of the current item."
  (gopcaml-zipper-mode-and-move
   (lambda (_) (gopcaml-move-zipper-up)) nil nil 'forward t)
  )

(defun gopcaml-indent-current-sexp ()
  "Indent the current sexp."
  (interactive)
  (let ((area (gopcaml-get-current-item-bounds)))
    (when area
      (ocp-indent-region (car area) (cadr area))))
  )

(defun gopcaml-beginning-defun ()
  "Move backwards to the beginning of the defun."
  (interactive)
  (let ((area (car (gopcaml-find-defun-start (point) (line-number-at-pos)))))
    (if area
	(progn (goto-char  area))
      nil)))

(defun gopcaml-end-defun ()
  "Move forwards to the end of the defun."
  (interactive)
  (let ((area (car (gopcaml-find-defun-end (point) (line-number-at-pos)))))
    (if area
	(progn (goto-char  area))
      nil)))

(defun gopcaml-goto-nearest-letdef ()
  "Move backwards to the nearest letdef."
  (interactive)
  (let ((area (car (gopcaml-find-nearest-letdef (point) (line-number-at-pos)))))
    (if area
	(progn
	  (push-mark)
	  (goto-char  area))
      nil)))

(defun gopcaml-goto-nearest-pattern ()
  "Move backwards to the nearest letdef."
  (interactive)
  (let ((area (car (gopcaml-find-nearest-pattern (point) (line-number-at-pos)))))
    (if area
	(progn
	  (push-mark)
	  (goto-char area)
	  )
      nil)))

(defun gopcaml-move-to-hole ()
  "Move to (??) from the current point."
  (interactive)
  (let ((end (car (gopcaml-find-defun-end (point) (line-number-at-pos)))) (curr (point)))
    (when (and end (search-forward "(??)" end t))
      (if (equal (point) (+ curr 4)) (gopcaml-move-to-hole) (backward-char 4))
      )
    ))

(defun gopcaml-move-backward-to-hole ()
  "Move to (??) backward from the current point."
  (interactive)
  (let ((start (car (gopcaml-find-defun-start (point) (line-number-at-pos)))) (curr (point)))
    (when (and start (search-backward "(??)" start t))
      (if (equal (point) curr) (gopcaml-move-to-hole))
      )
    ))


(defun gopcaml-forward-list-full (selection-mode)
  "Move the zipper forwards (broadly from current point) in SELECTION-MODE."
  (interactive)
  (gopcaml-zipper-mode-and-move (lambda (initial)
				  (move-gopcaml-zipper
				   #'gopcaml-move-zipper-right
				   'forward
				   initial)
				  )
				#'gopcaml-broadly-build-zipper
				selection-mode
				'forward))

(defun gopcaml-forward-list ()
  "Move the zipper forwards (broadly from current point)."
  (interactive) (gopcaml-forward-list-full nil))

(defun gopcaml-forward-list-selection ()
  "Move the zipper forwards (broadly from current point)."
  (interactive) (gopcaml-forward-list-full t))

(defun gopcaml-backward-list-full (selection-mode)
  "Move the zipper backwards (broadly from current point) in SELECTION-MODE."
  (interactive)
  (gopcaml-zipper-mode-and-move (lambda (initial)
				  (move-gopcaml-zipper
				   #'gopcaml-move-zipper-left
				   'backward
				   initial))
				#'gopcaml-broadly-build-zipper
				selection-mode
				'backward))

(defun gopcaml-backward-list ()
  "Move the zipper backwards (broadly from current point)."
  (interactive) (gopcaml-backward-list-full nil))

(defun gopcaml-backward-list-selection ()
  "Move the zipper backwards (broadly from current point)."
  (interactive) (gopcaml-backward-list-full t))

(defun gopcaml-backward-up-list-full (selection-mode)
  "Move the zipper up (from expression at point) in SELECTION-MODE."
  (interactive)
  (gopcaml-zipper-mode-and-move (lambda (initial) (move-gopcaml-zipper
						   #'gopcaml-move-zipper-up
						   nil
						   initial))
				nil
				selection-mode
				'backward))
(defun gopcaml-backward-up-list ()
  "Move the zipper up (from expression at point)."
  (interactive) (gopcaml-backward-up-list-full nil))
(defun gopcaml-backward-up-list-selection ()
  "Move the zipper up (from expression at point)."
  (interactive) (gopcaml-backward-up-list-full t))


(defun gopcaml-down-list-full (selection-mode)
  "Move the zipper down (from expression at point) in SELECTION-MODE."
  (interactive)
  (gopcaml-zipper-mode-and-move (lambda (initial) (move-gopcaml-zipper
						   #'gopcaml-move-zipper-down
						   nil
						   initial))
				nil
				selection-mode
				'forward))
(defun gopcaml-down-list ()
  "Move the zipper dow (from expression at point)."
  (interactive) (gopcaml-down-list-full nil))

(defun gopcaml-down-list-selection ()
  "Move the zipper down (from expression at point)."
  (interactive) (gopcaml-down-list-full t))

(defun gopcaml-forward-sexp-full (selection-mode &optional arg)
  "Move the zipper forward ARG times (from expression at point) in SELECTION-MODE."
  (interactive "^p")
  (if (equal arg (- 1))
      (gopcaml-zipper-mode-and-move (lambda (initial) (move-gopcaml-zipper
						       #'gopcaml-move-zipper-left
						       'backward
						       initial))
				    nil
				    selection-mode
				    'backward)
    (gopcaml-zipper-mode-and-move (lambda (initial) (move-gopcaml-zipper
						     #'gopcaml-move-zipper-right
						     'forward
						     initial))
				  nil
				  selection-mode
				  'forward)))

(defun gopcaml-forward-sexp (&optional arg)
  "Move the zipper down (from expression at point)."
  (interactive "p") (gopcaml-forward-sexp-full nil arg))

(defun gopcaml-forward-sexp-selection (&optional arg)
  "Move the zipper down (from expression at point)."
  (interactive "p") (gopcaml-forward-sexp-full t arg))

(defun gopcaml-backward-sexp-full (selection-mode &optional arg)
  "Move the zipper backwards (from expression at point) in SELECTION-MODE."
  (interactive "p")
  (gopcaml-zipper-mode-and-move (lambda (initial) (move-gopcaml-zipper
						   #'gopcaml-move-zipper-left
						   'backward
						   initial))
				nil
				selection-mode
				'backward))

(defun gopcaml-backward-sexp (&optional arg)
  "Move the zipper down (from expression at point)."
  (interactive "p")
  (gopcaml-backward-sexp-full nil arg))

(defun gopcaml-backward-sexp-selection (&optional arg)
  "Move the zipper down (from expression at point)."
  (interactive "p") (gopcaml-backward-sexp-full t arg))

(defun gopcaml-ensure-space-between-backward ()
  "Ensures space between points."
  (interactive)
  (let ((start (point)) end
	(start-line (line-number-at-pos))
	end-line
	(indent (current-column))
	)
    (skip-chars-backward " \n\t")
    (setq end (point))
    (setq end-line (line-number-at-pos))
    (delete-region start end)
    (insert "\n")
    (insert "\n")
    (insert (make-string indent 32))
    (list (- (point) end) (- start end)
	  (- (line-number-at-pos) end-line)
	  (- start-line end-line))
    ))

(defun gopcaml-ensure-space-between-forward ()
  "Ensures space between points."
  (interactive)
  (let ((start (point)) end
	(start-line (line-number-at-pos))
	end-line
	indent
	)
    (skip-chars-forward " \n\t")
    (setq indent (current-column))
    (setq end (point))
    (setq end-line (line-number-at-pos))
    (delete-region start end)
    (insert "\n")
    (insert "\n")
    (insert (make-string indent 32))
    (list (- start (point)) (- start end)
	  (- start-line (line-number-at-pos)) (- start-line end-line))
    ))

(defun gopcaml-zipper-ensure-space ()
  "Ensures-spacing between current element."
  (message "%s" (car (gopcaml-zipper-is-top-level)))
  (if (and (car (gopcaml-zipper-is-top-level)) (car (gopcaml-zipper-is-top-level-parent)))
      (let
	  ((area (car (gopcaml-retrieve-zipper-bounds)))
	   start end
	   pre-change
	   post-change)
	(when area
	  (setq start (car area))
	  (setq end (cadr area))
	  (goto-char end)
	  (setq post-change (gopcaml-ensure-space-between-forward))
	  (setq post-change
		(list
		 (-  (car post-change)  (cadr post-change))
		 (-  (caddr post-change) (cadddr post-change) ))
		)
	  (goto-char start)
	  (setq pre-change (gopcaml-ensure-space-between-backward))
	  (setq pre-change
		(list
		 (- (cadr pre-change) (car pre-change))
		 (- (cadddr pre-change) (caddr pre-change))))
	  (setq area (car (gopcaml-zipper-space-update
			   (car pre-change) (cadr pre-change)
			   (car post-change) (cadr post-change))))
	  (when area
	    (move-overlay gopcaml-zipper-overlay (car area) (cadr area))
	    t)))
    nil))

(defun gopcaml-zipper-type ()
  "Type region enclosed by zipper."
  (interactive)
  (when gopcaml-zipper-overlay
    (let ((start (overlay-start gopcaml-zipper-overlay))
	  (end (overlay-end gopcaml-zipper-overlay)))
      (lexical-let*
      	  ((substring  (buffer-substring-no-properties start end))
      	   (on-success (lambda (type) (merlin--type-display nil type nil)))
      	   (on-error   (lambda (err)
      			 (let ((msg (assoc 'message err))
      			       (typ (assoc type err)))
      			   (cond ((and typ (equal (cdr typ) "parser"))
      				  (message "Error: the content of the region failed to parse."))
      				 (msg (message "Error: %s" (cdr msg)))
      				 (t
      				  (message "Unexpected error")))))))
      	(merlin--type-expression substring on-success on-error))
      )))

(defun gopcaml-zipper-kill-region ()
  "Kill the current item using the zipper."
  (interactive)
  (let ((area (car (gopcaml-begin-zipper-delete))) start end (buf-name (buffer-name)))
    (when area
      (setq start (car area))
      (setq end (cadr area))
      (kill-region start end)
      (setq area (car (gopcaml-retrieve-zipper-bounds)))
      (move-overlay gopcaml-zipper-overlay (car area) (cadr area))
      (goto-char (car area)))))

(defun gopcaml-zipper-move-vertical (move-fn)
  "Insert a let-def using the zipper MOVE-FN."
  (interactive)
  (let (area insert-pos start end text)
    (setq area (car (funcall move-fn)))
    (when area
      (setq insert-pos (car area))
      (setq start (cadr area))
      (setq end (caddr area))
      (setq text (buffer-substring-no-properties start end))
      (delete-region start end)
      (goto-char insert-pos)
      (insert "\n\n")
      (insert text)
      (insert "\n")
      (setq area (car (gopcaml-retrieve-zipper-bounds)))
      (move-overlay gopcaml-zipper-overlay (car area) (cadr area))
      (goto-char (car area))
      )
    ))

(defun gopcaml-zipper-move-up ()
  "Move the zipper element up."
  (interactive)
  (gopcaml-zipper-move-vertical #'gopcaml-zipper-move-elem-up))

(defun gopcaml-zipper-move-down ()
  "Move the zipper down."
  (interactive)
  (gopcaml-zipper-move-vertical #'gopcaml-zipper-move-elem-down))

(defun gopcaml-zipper-insert-letdef ()
  "Attempt to insert a let-def using the zipper."
  (interactive)
  (let (area (column (current-column)) text start-pos)
    (setq area (car (gopcaml-zipper-insert-let-def-start column)))
    (setq text (car area))
    (setq start-pos (cdr area))
    (goto-char start-pos)
    (insert "\n\n")
    (insert (make-string column 32))
    (insert text)
    (insert "\n")
    (setq area (car (gopcaml-retrieve-zipper-bounds)))
    (move-overlay gopcaml-zipper-overlay (car area) (cadr area))
    (goto-char (car area))
    ))

(defun gopcaml-copy-region ()
  "Copy region encompassed by the zipper."
  (when gopcaml-zipper-overlay
    (let ((start (overlay-start gopcaml-zipper-overlay))
	  (end (overlay-end gopcaml-zipper-overlay))
	  text)
      (setq text (buffer-substring-no-properties start end))
      (copy-region-as-kill start end)
      (message "copied \"%s\" to kill-ring" (truncate-string-to-width
					     text 40 nil nil t)))))

(defun gopcaml-mode-insert-type-hole ()
  "Insert a type hole at the cursor."
  (interactive)
  (insert "(??)"))

(defun gopcaml-zipper-swap (transform-fn)
  "Swap current text using output from zipper function TRANSFORM-FN."
  (let ((area (car (funcall transform-fn)))
	region1-start
	region1-end
	region2-start
	region2-end)
    (when area
      (setq region1-start (car area))
      (setq region1-end (car (cdr area)))
      (setq region2-start (car (cdr (cdr area))))
      (setq region2-end (car (cdr (cdr (cdr area)))))
      (let ((region1-str (buffer-substring region1-start region1-end))
	    (region2-str (buffer-substring region2-start region2-end)))
	(when (< region2-start region1-start)
	  (cl-psetq region1-start region2-start
		    region1-end region2-end
		    region1-str region2-str
		    region2-start region1-start
		    region2-end region1-end
		    region2-str region1-str))
	(progn
	  (delete-region region2-start region2-end)
	  (goto-char region2-start)
	  (insert region1-str))
	(progn
	  (delete-region region1-start region1-end)
	  (goto-char region1-start)
	  (insert region2-str)))
      (setq area (car (gopcaml-retrieve-zipper-bounds)))
      (move-overlay gopcaml-zipper-overlay (car area) (cadr area))
      (goto-char (car area))
      )))

(defun gopcaml-zipper-transpose ()
  "Transpose text using zipper."
  (interactive)
  (let ((area
	 (if (not gopcaml-zipper)
	     ;; build zipper aronud point
	     (let
		 ((area
		   (car (gopcaml-build-zipper (point) (line-number-at-pos))))
		  start end overlay)
	       ;; if successfull then perform operation
	       (if area
		   (progn
		     (setq start (car area))
		     (setq end (cadr area))
		     (setq overlay (make-overlay start end))
		     ;; (overlay-put overlay 'face 'gopcaml-selection-face)
		     (overlay-put overlay 'face 'gopcaml-zipper-face)
		     (overlay-put overlay 'gopcaml-kind 'zipper)
		     (setq gopcaml-zipper-overlay overlay)
		     (setq gopcaml-zipper-mode-quit
			   (set-transient-map
			    gopcaml-zipper-mode-map
			    t #'gopcaml-on-exit-zipper-mode))
		     area)
		 (gopcaml-delete-zipper)
		 nil))
	   ;; otherwise just perfom operation
	   (car (gopcaml-retrieve-zipper-bounds))
	   ))
	start end curr)
    (if area
	(progn
	  (setq curr (point))
	  (setq start (car area))
	  (setq end (cadr area))
	  (cond
	   ((and start end (equal start curr))
	    (gopcaml-zipper-swap #'gopcaml-begin-zipper-swap-backwards)
	    (gopcaml-move-zipper-right)
	    (setq area (car (gopcaml-retrieve-zipper-bounds))))
	   
	   ((and start end (equal end curr))
	    (gopcaml-zipper-swap #'gopcaml-begin-zipper-swap-forwards)
	    (setq area (car (gopcaml-retrieve-zipper-bounds))))
	   (t
	    (setq area nil)
	    nil))
	  (when area
	    (move-overlay gopcaml-zipper-overlay (car area) (cadr area))
	    (goto-char (cadr area))
	    t))
      nil)))

(defun gopcaml-zipper-move-forwards ()
  "Move current element forwards at the same level."
  (interactive)
  (gopcaml-zipper-swap #'gopcaml-begin-zipper-swap-forwards))

(defun gopcaml-zipper-move-backwards ()
  "Move current element backwards at the same level."
  (interactive)
  (gopcaml-zipper-swap #'gopcaml-begin-zipper-swap-backwards))

(defun gopcaml-state-filter (cmd)
  "Determines whether a CMD can be carried out in current Gopcaml mode state."
  (when (and gopcaml-state(gopcaml-state-available-filter))
    cmd))


(defun gopcaml-zipper-kill ()
  "Deletes the current item and exits zipper mode."
  (when gopcaml-zipper-mode-quit



    (funcall zipper-mode-quit)
    ))

(defvar gopcaml-zipper-mode-map
  (let ((gopcaml-map (make-sparse-keymap)))
    (define-key gopcaml-map (kbd "C-M-f")
      '(menu-item "" gopcaml-forward-sexp ))
    (define-key gopcaml-map (kbd "C-M-b")
      '(menu-item "" gopcaml-backward-sexp))
    (define-key gopcaml-map (kbd "C-M-q")
      #'gopcaml-indent-current-sexp)
    (define-key gopcaml-map (kbd "C-M-u")
      '(menu-item "" gopcaml-backward-up-list))
    (define-key gopcaml-map (kbd "C-M-S-u")
      '(menu-item ""  gopcaml-zipper-move-up))
    (define-key gopcaml-map (kbd "C-M-d")
      '(menu-item "" gopcaml-down-list ))
    (define-key gopcaml-map (kbd "C-M-S-d")
      '(menu-item ""  gopcaml-zipper-move-down))
    (define-key gopcaml-map (kbd "C-M-n")
      '(menu-item "" gopcaml-forward-list))
    (define-key gopcaml-map (kbd "C-M-p")
      '(menu-item "" gopcaml-backward-list))
    ;; (define-key gopcaml-map (kbd "C-M-w")
    ;;   #'gopcaml-zipper-kill)
    ;; (define-key gopcaml-map (kbd "C-M-w")
    ;;   '(menu-item "" (lambda () (interactive) (move-gopcaml-zipper #'gopcaml-zipper-kill-region))
    ;; 		  ))
    (define-key gopcaml-map (kbd "M-w")
      '(menu-item "" (lambda () (interactive) (gopcaml-copy-region))
		  ))
    (define-key gopcaml-map (kbd "C-M-S-n")
      '(menu-item "" (lambda () (interactive) (gopcaml-zipper-move-forwards))
		  ))
    (define-key gopcaml-map (kbd "C-M-S-f")
      '(menu-item "" (lambda () (interactive) (gopcaml-zipper-move-forwards))
		  ))
    (define-key gopcaml-map (kbd "C-M-S-p")
      '(menu-item "" (lambda () (interactive) (gopcaml-zipper-move-backwards))
		  ))
    (define-key gopcaml-map (kbd "C-M-S-b")
      '(menu-item "" (lambda () (interactive) (gopcaml-zipper-move-backwards))
		  ))
    (define-key gopcaml-map (kbd "C-M-t")
      '(menu-item "" (lambda () (interactive) (gopcaml-zipper-transpose))
		  ))
    (define-key gopcaml-map (kbd "M-SPC")
      '(menu-item "" (lambda () (interactive) (gopcaml-zipper-ensure-space))))
    (define-key gopcaml-map (kbd "C-M-SPC")
      '(menu-item "" (lambda () (interactive) (gopcaml-zipper-ensure-space))))

    gopcaml-map)
  "Map used when in zipper mode.  ari ari!")

(defvar gopcaml-selection-zipper-mode-map
  (let ((gopcaml-map (make-sparse-keymap)))
    (define-key gopcaml-map (kbd "C-M-S-f")
      '(menu-item "" gopcaml-forward-sexp-selection))
    (define-key gopcaml-map (kbd "C-M-S-b")
      '(menu-item "" gopcaml-backward-sexp-selection))

    (define-key gopcaml-map (kbd "C-M-S-u")
      '(menu-item "" gopcaml-backward-up-list-selection))
    (define-key gopcaml-map (kbd "C-M-S-d")
      '(menu-item "" gopcaml-down-list-selection))

    (define-key gopcaml-map (kbd "C-M-S-n")
      '(menu-item "" gopcaml-forward-list-selection))
    (define-key gopcaml-map (kbd "C-M-S-p")
      '(menu-item "" gopcaml-backward-list-selection))
    gopcaml-map)
  "Map used when in zipper mode for selections.  ari ari!")

(defvar gopcaml-mark-zipper-mode-map
  (let ((gopcaml-map (make-sparse-keymap)))
    (define-key gopcaml-map
      (kbd "C-M-@")
      #'gopcaml-zipper-mark-mode)
    (define-key gopcaml-map
      (kbd "C-M-SPC")
      #'gopcaml-zipper-mark-mode)
    gopcaml-map)
  "Map used when in zipper mode for marking expressions.  ari ari!")

(defun gopcaml-on-exit-zipper-mode ()
  "Exit gopcaml-zipper-mode."
  (gopcaml-delete-zipper)
  (delete-overlay gopcaml-zipper-overlay)
  (setq gopcaml-zipper-overlay nil)
  (setq gopcaml-zipper-mode-quit nil))

;; graciously taken from https://emacs.stackexchange.com/questions/12532/buffer-local-idle-timer
(defun run-with-local-idle-timer (secs repeat function &rest args)
  "`run-with-idle-timer' but always run in the `current-buffer'.
Cancels itself, if this buffer was killed.
SECS is the periodicity of the timer.
REPEAT dictates whether the timer should be called repeatedly.
FUNCTION is the function to call on timer.
ARGS are parameters to pass to the function."
  (let* (;; Chicken and egg problem.
         (fns (make-symbol "local-idle-timer"))
         (timer (apply 'run-with-idle-timer secs repeat fns args))
         (fn `(lambda (&rest args)
                (if (not (buffer-live-p ,(current-buffer)))
                    (cancel-timer ,timer)
                  (with-current-buffer ,(current-buffer)
                    (apply (function ,function) args))))))
    (fset fns fn)
    timer))

(defun gopcaml-before-change-remove-type-hole (beginning end)
  "Before inserting text, attempt to remove type holes.
BEGINNING is the start of the edited text region.
END is the end of the edited text region."
  (let ( (point (point)) element)
    (when (and (equal beginning end) (< (+ point 4) (point-max)))
      (setq element (buffer-substring-no-properties  point (+ point 4)))
      (if (equal "(??)" element)
	  (delete-region point (+ point 4))
	))))


(defun gopcaml-type-hole-needed ()
  "Check whether the current expression expansion needs a type hole."
  (save-excursion
    (skip-chars-forward " \t\n")
    (looking-at-p "\\(|\\|end\\|module\\|val\\|type\\)")
    )
  )

(defun gopcaml-list-free-variables (start end)
  "List free variables in region from START to END."
  (interactive "^r")
  (let ((text (buffer-substring-no-properties start end))
	bounds)
    (setq bounds (car (gopcaml-find-extract-scope text start end)))
    (message "got %s" bounds)
    (when (and bounds)
      (message text)
      (gopcaml-temporarily-highlight-region bounds)
      )
    ))
(defun gopcaml-list-pattern-scopes ()
  "Highlight all patterns in the current item."
  (interactive)
  (let ((mbounds (gopcaml-find-patterns-in-scope (point))))
    (when mbounds
      (gopcaml-temporarily-highlight-multiple-region mbounds))))

(defun match-seq (pattern beg end)
  "Return a list of all ocurrances of PATTERN in region from BEG to END."
  (let (matches)
    (save-excursion
      (goto-char beg)
      (save-match-data
	(while (search-forward pattern end t)
	  (push (cons (match-beginning 0) (match-end 0)) matches)
	  (goto-char (match-end 0)))
	matches))))

(defun highlight-occurrances-in-region (beg end)
  "Highlight all ocurrances of a string in the region from BEG to END."
  (interactive "^r")
  (gopcaml-temporarily-highlight-multiple-region (match-seq (read-string "Search string:") beg end))
  )

;;; setup keybindings

(define-key gopcaml-mode-map (kbd "TAB") #'gopcaml-move-to-hole)
(define-key gopcaml-mode-map (kbd "<backtab>") #'gopcaml-move-backward-to-hole)
(define-key gopcaml-mode-map (kbd "M-RET") #'gopcaml-mode-insert-type-hole)
(define-key gopcaml-mode-map (kbd "C-M-u") '(menu-item "" gopcaml-backward-up-list
						       :filter gopcaml-state-filter))
(define-key gopcaml-mode-map (kbd "C-M-q") '(menu-item "" gopcaml-indent-current-sexp
						       :filter gopcaml-state-filter))
(define-key gopcaml-mode-map (kbd "C-M-d") '(menu-item "" gopcaml-down-list
						       :filter gopcaml-state-filter))
(define-key gopcaml-mode-map (kbd "C-M-n") '(menu-item "" gopcaml-forward-list
						       :filter gopcaml-state-filter))
(define-key gopcaml-mode-map (kbd "C-M-p") '(menu-item "" gopcaml-backward-list
						       :filter gopcaml-state-filter))
(define-key gopcaml-mode-map (kbd "C-M-t") '(menu-item "" gopcaml-zipper-transpose
						       :filter gopcaml-state-filter))
(define-key gopcaml-mode-map (kbd "C-M-S-u") '(menu-item "" gopcaml-backward-up-list-selection
							 :filter gopcaml-state-filter))
(define-key gopcaml-mode-map (kbd "C-M-S-d") '(menu-item "" gopcaml-down-list-selection
							 :filter gopcaml-state-filter))
(define-key gopcaml-mode-map (kbd "C-M-S-n") '(menu-item "" gopcaml-forward-list-selection
							 :filter gopcaml-state-filter))
(define-key gopcaml-mode-map (kbd "C-M-S-p") '(menu-item "" gopcaml-backward-list-selection
							 :filter gopcaml-state-filter))

(define-key merlin-mode-map (kbd "C-c C-o") '(menu-item "" gopcaml-goto-nearest-letdef
							:filter gopcaml-state-filter))

(define-key merlin-mode-map (kbd "C-c C-p") '(menu-item "" gopcaml-goto-nearest-pattern
							:filter gopcaml-state-filter))

(define-key gopcaml-mode-map (kbd "C-M-@") '(menu-item "" gopcaml-zipper-mark-mode
						       :filter gopcaml-state-filter))
(define-key gopcaml-mode-map (kbd "C-M-SPC") '(menu-item "" gopcaml-zipper-mark-mode
							 :filter gopcaml-state-filter))

(defun gopcaml-setup-bindings ()
  "Setup bindings for gopcaml-mode."
  (message "setting up gopcaml-bindings")
  (setq-local end-of-defun-function #'gopcaml-end-defun)
  (setq-local beginning-of-defun-function #'gopcaml-beginning-defun)
  
  ;; backup bindings (if smartparens not installed)
  (unless (lookup-key gopcaml-mode-map (kbd "C-M-f"))
    (define-key gopcaml-mode-map (kbd "C-M-f") '(menu-item "" gopcaml-forward-sexp
							   :filter gopcaml-state-filter)))

  (unless (lookup-key gopcaml-mode-map (kbd "C-M-b"))
    (define-key gopcaml-mode-map (kbd "C-M-b") '(menu-item "" gopcaml-backward-sexp
							   :filter gopcaml-state-filter)))
  (unless (lookup-key gopcaml-mode-map (kbd "C-M-S-f"))
    (define-key gopcaml-mode-map (kbd "C-M-S-f") '(menu-item "" gopcaml-forward-sexp-selection
							     :filter gopcaml-state-filter)))

  (unless (lookup-key gopcaml-mode-map (kbd "C-M-S-b"))
    (define-key gopcaml-mode-map (kbd "C-M-S-b") '(menu-item "" gopcaml-backward-sexp-selection
							     :filter gopcaml-state-filter)))
  
  (setq-local forward-sexp-function nil)
  (add-hook 'after-change-functions #'gopcaml-update-dirty-region)
  (add-hook 'before-change-functions #'gopcaml-before-change-remove-type-hole)
  (setq gopcaml-update-timer
	(run-with-local-idle-timer gopcaml-rebuild-delay t
				   (lambda ()
				     (when gopcaml-state (gopcaml-ensure-updated-state)))))
  (if (and (boundp 'gopcaml-dev-mode) gopcaml-dev-mode
	   (boundp 'gopcaml-dev-mode-filemap) gopcaml-dev-mode-filemap
	   (boundp 'gopcaml-dev-mode-file) gopcaml-dev-mode-file)
      (progn
	(puthash gopcaml-dev-mode-file t gopcaml-dev-mode-filemap))))

(defun gopcaml-quit ()
  "Quit gopcaml-mode and tear down its timers and bindings."
  (interactive)
  (setq after-change-functions
	(remove #'gopcaml-update-dirty-region after-change-functions))
  (setq before-change-functions
	(remove #'gopcaml-before-change-remove-type-hole before-change-functions))
  (if gopcaml-update-timer
      (progn "cancelling gopcaml-update-timer" (cancel-timer gopcaml-update-timer)))
  (setq gopcaml-update-timer nil)
  (setq gopcaml-state nil)
  (setq gopcaml-zipper nil)
  (fundamental-mode))

(defun gopcaml-setup-hook ()
  "Initialize gopcaml-mode."
  (gopcaml-setup-bindings))

(add-hook 'gopcaml-mode-hook #'gopcaml-setup-hook)


(provide 'gopcaml-mode)

;; optional dependencies
(eval-after-load 'smartparens '(require 'gopcaml-smartparens))
(eval-after-load 'multiple-cursors '(require 'gopcaml-multiple-cursors))
;;; gopcaml-mode.el ends here
