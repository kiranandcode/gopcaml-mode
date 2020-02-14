;; (defun fake-module-reload (module)
;;   (let ((tmpfile (make-temp-file
;;                   (file-name-nondirectory module) nil module-file-suffix)))
;;     (copy-file module tmpfile t)
;;     (module-load tmpfile)))

(add-to-list 'load-path (expand-file-name "./_build/default/"))
(require 'gopcaml)
;; (fake-module-reload "./_build/default/gopcaml.so")

(defgroup gopcaml-faces nil
  "Faces for gopcaml mode."
  :group 'gopcaml-mode
  :group 'faces)

(defface gopcaml-highlight-face
  '((t :inherit caml-types-expr-face))
  "Face for highlighting expr."
  :group 'gopcaml-faces)

(defface gopcaml-zipper-face
  '((t :inherit caml-types-expr-face))
  "Face for highlighting zipper."
  :group 'gopcaml-faces)

(defvar-local gopcaml-temporary-highlight-overlays nil
  "Maintains an the overlay used for single-element highlights.")

(defcustom gopcaml-rebuild-delay 1
  "Number of idling seconds before rebuilding gopcaml-state."
  :type 'integer
  :group 'gopcaml)

(defun gopcaml-remove-stored-overlays (&optional group)
  "Remove stored overlays - optionally only those of gopcaml-kind GROUP."
  (setq gopcaml-temporary-highlight-overlays
	(remove-if (lambda (it) (null it))
		   (mapcar (lambda (it)
			     (if (or (not group) (equal (overlay-get it 'gopcaml-kind) group))
				 (delete-overlay it)
			       nil)) gopcaml-temporary-highlight-overlays)))
  )

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

(defvar-local gopcaml-zipper-overlay nil
  "Overlay used to highlight the zipper region.")

(defun move-gopcaml-zipper (zipper-fn)
  "Move the zipper using ZIPPER-FN."
  (let ((area (car (funcall zipper-fn))) start end)
    (when area
      (setq start (car area))
      (setq end (cadr area))
      (move-overlay gopcaml-zipper-overlay start end)
      (goto-char start)
      )))

(defvar gopcaml-zipper-mode-map
  (let ((gopcaml-map (make-sparse-keymap)))
    (define-key gopcaml-map (kbd "e") '(lambda ()
					 (interactive)
					 (move-gopcaml-zipper
					  #'gopcaml-move-zipper-down)))
    (define-key gopcaml-map (kbd "a") '(lambda ()
					 (interactive)
					 (move-gopcaml-zipper
					  #'gopcaml-move-zipper-up)))
    (define-key gopcaml-map (kbd "p") '(lambda ()
					 (interactive)
					 (move-gopcaml-zipper
					  #'gopcaml-move-zipper-left)))
    (define-key gopcaml-map (kbd "n") '(lambda ()
					 (interactive)
					 (move-gopcaml-zipper
					  #'gopcaml-move-zipper-right)))
    gopcaml-map
    )
  "Map used when in zipper mode.  ari ari!")

(defun gopcaml-on-exit-zipper-mode ()
  "Exit gopcaml-zipper-mode."
  (message "arrivederci!!")
  (gopcaml-delete-zipper)
  (delete-overlay gopcaml-zipper-overlay)
  (setq gopcaml-zipper-overlay nil))

(defun gopcaml-enter-zipper-mode ()
  "Start gopcaml-zipper mode."
  (interactive)
  (message "Entering zipper mode!")
  (message "Ari ari ari")
  (let ((area (car (gopcaml-build-zipper (point)))) start end overlay)
    (when area
      (setq start (car area))
      (setq end (cadr area))
      (setq overlay (make-overlay start end))
      (overlay-put overlay 'face 'gopcaml-zipper-face)
      (overlay-put overlay 'gopcaml-kind 'zipper)
      (setq gopcaml-zipper-overlay overlay)
      (set-transient-map
       gopcaml-zipper-mode-map
       t #'gopcaml-on-exit-zipper-mode)
      )))

;; graciously taken from https://emacs.stackexchange.com/questions/12532/buffer-local-idle-timer
(defun run-with-local-idle-timer (secs repeat function &rest args)
  "Like `run-with-idle-timer', but always runs in the `current-buffer'.

Cancels itself, if this buffer was killed."
  (let* (;; Chicken and egg problem.
         (fns (make-symbol "local-idle-timer"))
         (timer (apply 'run-with-idle-timer secs repeat fns args))
         (fn `(lambda (&rest args)
                (if (not (buffer-live-p ,(current-buffer)))
                    (cancel-timer ,timer)
                  (with-current-buffer ,(current-buffer)
                    (apply (function ,function) args))))))
    (fset fns fn)
    fn))

(defun gopcaml-setup-bindings ()
  "Setup bindings for gopcaml-mode."
  (message "setting up gopcaml-bindings")
  (bind-key (kbd "C-M-l") #'gopcaml-highlight-current-structure-item gopcaml-mode-map)
  (bind-key (kbd "C-M-k") #'gopcaml-highlight-dirty-region gopcaml-mode-map)
  (bind-key (kbd "C-M-z") #'gopcaml-enter-zipper-mode gopcaml-mode-map)
  (setq after-change-functions
	(cons #'gopcaml-update-dirty-region after-change-functions))
  (run-with-local-idle-timer gopcaml-rebuild-delay t #'gopcaml-ensure-updated-state)
  )


(add-hook 'gopcaml-mode-hook #'gopcaml-setup-bindings)

(local-set-key (kbd "C-M-l") #'gopcaml-highlight-current-structure-item)
(local-set-key (kbd "C-M-k") #'gopcaml-highlight-dirty-region)

(find-file "/home/kirang/Documents/code/ocaml/gopcaml-mode/gopcaml_state.ml")
(gopcaml-mode)
(gopcaml-setup-bindings)



(provide 'gopcaml-mode)
