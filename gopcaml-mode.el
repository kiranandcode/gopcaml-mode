;;; gopcaml-mode -- ocaml editing mode
;;; Commentary:
;;; An extension of tuareg mode to provide a better editing experience.
(require 'subr-x)
(require 'gopcaml (expand-file-name
		   "./_build/default/gopcaml.so"
		   (string-trim
		    (shell-command-to-string (format "dirname %s" file)))))

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
		  (let ((wind-start (window-start)) (wind-end (window-end)))
		    (if (and wind-start wind-end (< wind-start start wind-end))
			(set-window-point (get-buffer-window buf-name) start)
		      (goto-char start))
		    )
		  t
		  )
	      nil))))
      nil)))



(defun gopcaml-zipper-mode-and-move (operation &optional zipper-constructor selection-mode)
  "Start gopcaml-zipper mode using ZIPPER-CONSTRUCTOR and perform OPERATION."
  (interactive)
  (if (not zipper-constructor) (setq zipper-constructor #'gopcaml-build-zipper))

  (let ((selection-active (or (region-active-p) selection-mode)))
    (if (and selection-active (not (region-active-p)))
	(progn
	  (push-mark (point))
	  (activate-mark))
      )

    ;;  if not already in zipper mode
    (if (not gopcaml-zipper)
	;; build zipper aronud point
      (let ((area (car (funcall zipper-constructor (point) (line-number-at-pos)))) start end overlay)
	;; if successfull then perform operation
	(if area
	    (progn
	      (setq start (car area))
	      (setq end (cadr area))
	      (setq overlay (make-overlay start end))
	      (if (not selection-active)
		  ;; (overlay-put overlay 'face 'gopcaml-selection-face)
		  (overlay-put overlay 'face 'gopcaml-zipper-face)
		  )
	      (overlay-put overlay 'gopcaml-kind 'zipper)
	      (setq gopcaml-zipper-overlay overlay)
	      (set-transient-map
	       (if (not selection-active)
		   gopcaml-zipper-mode-map
		 gopcaml-selection-zipper-mode-map)
	       t #'gopcaml-on-exit-zipper-mode)
	      (funcall operation t))
	  (gopcaml-delete-zipper)
	  nil
	  ))
    ;; otherwise just perfom operation
     (funcall operation nil)
  )))

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
  (gopcaml-zipper-mode-and-move (lambda (initial) (move-gopcaml-zipper
					    #'gopcaml-move-zipper-right
					    'forward
					    initial))
				#'gopcaml-broadly-build-zipper
				selection-mode))

(defun gopcaml-forward-list ()
  "Move the zipper forwards (broadly from current point)."
  (interactive) (gopcaml-forward-list-full nil))

(defun gopcaml-forward-list-selection ()
  "Move the zipper forwards (broadly from current point)."
  (interactive) (gopcaml-forward-list-full t))

(defun gopcaml-backward-list-full (selection-mode)
  "Move the zipper backwards (broadly from current point) in SELECTION-MODE."
  (interactive)
  (gopcaml-zipper-mode-and-move (lambda (initial) (move-gopcaml-zipper
					    #'gopcaml-move-zipper-left
					    'backward
					    initial))
				#'gopcaml-broadly-build-zipper
				selection-mode))

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
				selection-mode))
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
				selection-mode))
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
						initial)))
    (gopcaml-zipper-mode-and-move (lambda (initial) (move-gopcaml-zipper
					      #'gopcaml-move-zipper-right
					      'forward
					      initial))
				  nil
				  selection-mode)))

(defun gopcaml-forward-sexp (&optional arg)
  "Move the zipper dow (from expression at point)."
  (interactive) (gopcaml-forward-sexp-full nil arg))

(defun gopcaml-forward-sexp-selection (&optional arg)
  "Move the zipper down (from expression at point)."
  (interactive) (gopcaml-forward-sexp-full t arg))

(defun gopcaml-backward-sexp-full (selection-mode &optional arg)
  (interactive)
  "Move the zipper backwards (from expression at point) in SELECTION-MODE."
  (gopcaml-zipper-mode-and-move (lambda (initial) (move-gopcaml-zipper
						   #'gopcaml-move-zipper-left
						   'backward
						   initial))
				nil
				selection-mode))

(defun gopcaml-backward-sexp (&optional arg)
  "Move the zipper dow (from expression at point)."
  (interactive) (gopcaml-backward-sexp-full nil arg))

(defun gopcaml-backward-sexp-selection (&optional arg)
  "Move the zipper down (from expression at point)."
  (interactive) (gopcaml-backward-sexp-full t arg))

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
	t))))

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
		     (set-transient-map
		      gopcaml-zipper-mode-map
		      t #'gopcaml-on-exit-zipper-mode)
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

(defvar gopcaml-zipper-mode-map
  (let ((gopcaml-map (make-sparse-keymap)))
    (define-key gopcaml-map (kbd "C-M-f")
      '(menu-item "" gopcaml-forward-sexp ))
    (define-key gopcaml-map (kbd "C-M-b")
      '(menu-item "" gopcaml-backward-sexp))

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
    (define-key gopcaml-map (kbd "C-M-k")
      '(menu-item "" (lambda () (interactive) (move-gopcaml-zipper #'gopcaml-zipper-kill-region))
		  ))
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

    ;; (define-key gopcaml-map (kbd "T") '(lambda ()
    ;; 					 (interactive)
    ;; 					 (gopcaml-zipper-type)))
    ;; (define-key gopcaml-map (kbd "i") #'gopcaml-zipper-insert-letdef)
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

(defun gopcaml-on-exit-zipper-mode ()
  "Exit gopcaml-zipper-mode."
  (gopcaml-delete-zipper)
  (delete-overlay gopcaml-zipper-overlay)
  (setq gopcaml-zipper-overlay nil))

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
	)
      )
    ))


(defun gopcaml-setup-bindings ()
  "Setup bindings for gopcaml-mode."
  (message "setting up gopcaml-bindings")
  (setq-local end-of-defun-function #'gopcaml-end-defun)
  (setq-local beginning-of-defun-function #'gopcaml-beginning-defun)
  (define-key gopcaml-mode-map (kbd "TAB") #'gopcaml-move-to-hole)
  (define-key gopcaml-mode-map (kbd "<backtab>") #'gopcaml-move-backward-to-hole)
  (define-key gopcaml-mode-map (kbd "C-M-u") '(menu-item "" gopcaml-backward-up-list
						    :filter gopcaml-state-filter))
  (define-key gopcaml-mode-map (kbd "C-M-d") '(menu-item "" gopcaml-down-list
						    :filter gopcaml-state-filter))
  (define-key gopcaml-mode-map (kbd "C-M-n") '(menu-item "" gopcaml-forward-list
						    :filter gopcaml-state-filter))
  (define-key gopcaml-mode-map (kbd "C-M-p") '(menu-item "" gopcaml-backward-list
						    :filter gopcaml-state-filter))
  (define-key gopcaml-mode-map (kbd "C-M-f") '(menu-item "" gopcaml-forward-sexp
						    :filter gopcaml-state-filter))
  (define-key gopcaml-mode-map (kbd "C-M-b") '(menu-item "" gopcaml-backward-sexp
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
  (define-key gopcaml-mode-map (kbd "C-M-S-f") '(menu-item "" gopcaml-forward-sexp-selection
						    :filter gopcaml-state-filter))
  (define-key gopcaml-mode-map (kbd "C-M-S-b") '(menu-item "" gopcaml-backward-sexp-selection
						    :filter gopcaml-state-filter))
  (setq-local forward-sexp-function nil)
  (add-hook 'after-change-functions #'gopcaml-update-dirty-region)
  (add-hook 'before-change-functions #'gopcaml-before-change-remove-type-hole)
  (setq gopcaml-update-timer
	(run-with-local-idle-timer gopcaml-rebuild-delay t
				   (lambda ()
				     (when gopcaml-state (gopcaml-ensure-updated-state)))))
  )

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
