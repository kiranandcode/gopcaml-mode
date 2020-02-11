(defun fake-module-reload (module)
  (let ((tmpfile (make-temp-file
                  (file-name-nondirectory module) nil module-file-suffix)))
    (copy-file module tmpfile t)
    (module-load tmpfile)))


(add-to-list 'load-path (expand-file-name "./_build/default/"))
(require 'gopcaml)
(fake-module-reload "./_build/default/gopcaml.so")


(defgroup gopcaml-faces nil
  "Faces for gopcaml mode."
  :group 'gopcaml-mode
  :group 'faces)

(defface gopcaml-highlight-face
  '((t :inherit caml-types-expr-face))
  "Face for highlighting expr."
  :group 'gopcaml-faces)

(defvar-local gopcaml-highlight-overlays nil
  "Maintains an assoc list of overlays used for highlights.")

(defun gopcaml-remove-stored-overlays (&optional group)
  "Remove stored overlays - optionally only those of gopcaml-kind GROUP."
  (setq gopcaml-highlight-overlays
	(remove-if (lambda (it) (null it))
	     (mapcar (lambda (it)
		       (if (or (not it) (equal (overlay-get it 'gopcaml-kind) group))
			   (delete-overlay it)
			 nil)) gopcaml-highlight-overlays)))
  )

(defun gopcaml-store-overlays (overlay &optional group)
  "Remove stored overlays - optionally only those of gopcaml-kind GROUP."
  (overlay-put overlay 'gopcaml-kind group)
  (push overlay gopcaml-highlight-overlays))

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
	(sit-for 60)
      (gopcaml-remove-stored-overlays group))))


(defun gopcaml-highlight-current-structure-item ()
  "Highlight the structure-item enclosing the current point."
  (interactive)
  (let ((area (gopcaml-get-enclosing-structure-bounds (point)))
	start end)
    (setq start (caar area))
    (setq end (cadar area))
    (gopcaml-temporarily-highlight-region (cons start end))
    ))


;; for temporary overlays
;; (sit-for)



(provide 'gopcaml-mode)
