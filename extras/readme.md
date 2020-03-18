# Extra features

This document lists a number of optional features that can be applied
to further improve your editing experience.

## Emacs Compilation integration
You can configure emacs to automatically use dune build as the compile command using the following snippet:
```
(add-hook 'tuareg-mode-hook
		 #'(lambda ()
		     (set (make-local-variable 'compile-command)
			  (concat "dune build"))
		     (set (make-local-variable 'compilation-read-command)
			    nil)
		     ))
```
This just reduces the friction between editing and compilation (just press `C-c C-c` and the project is rebuilt).

## YASnippets integration
If you use YASnippets, Gopcaml mode provides a few functions that can
be used to improve the snippet expansion process for ocaml.

To use them, simply move over the snippets directory to
  `~/.emacs.d/snippets/tuareg-mode`

For my own emacs config, I have also configured yas-snippets to
auto-expand snippets after pressing space. This significantly improves
the editing experience and makes it easier to maintain a valid AST, 
as the snippet expansions automatically handle the boilerplate:
```elisp
  (defun yas-no-expand-in-comment/string ()
    (setq yas-buffer-local-condition
	  '(if (nth 8 (syntax-ppss)) ;; non-nil if in a string or comment
	       '(require-snippet-condition . force-in-comment)
	     t)))
  ;; don't expand in comments
  (add-hook 'prog-mode-hook 'yas-no-expand-in-comment/string)
  ;; expand on pressing space
  (define-key yas-minor-mode-map (kbd "SPC") yas-maybe-expand)
  ;; only expand if we typed the last letter
  (setq yas-expand-only-for-last-commands '(self-insert-command))
```
## Smartparens integration
By default gopcaml-mode integrates with smartparens automatically -
this is achieved by disabling the movement commands when the cursor is
on a parens understood by smartparens mode. With this integration,
movement around parens is significantly more natural.

I personally also have some additional functions for smart-parens
that ensure parenthesis are deleted in pairs (it works as a nice
parallel to the wrapping functionality that smartparens usually provides):
```elisp
  (defun sp-paired-delete-char-forward (&optional arg)
    "Delete char forward, deleting parens pair if deleting parens."
    (interactive "p")
    (setq arg (or arg 1))
    (if (<= arg 0) (sp-paired-delete-char-backward (- arg))
      (let ((any-changes nil))
	(do ((i 0 (+ i 1))) ((>= i arg))
	  (let ((inhibit-message t)
		)
	    (unless (or (when (save-match-data
				(sp--looking-at
				 (sp--get-opening-regexp
				  (sp--get-pair-list-context 'navigate))))
			  (let (ok curr)
			    (forward-char 1)
			    (setq curr (point))
			    (setq ok (sp-get-enclosing-sexp 1))
			    (progn
			      (cond
			       ((and ok
				     (< (sp-get ok :beg) curr )
				     (<= curr (sp-get ok :beg-in)))
				(sp-splice-sexp)
				(backward-char 1)
				(setq any-changes t)
				t
				)
			       (t
				(backward-char 1)
				nil)
			       ))
			    ))
			(when 
			    (save-match-data
			      (sp--looking-at
			       (sp--get-closing-regexp
				(sp--get-pair-list-context 'navigate))))
			  (let (ok curr)
			    (setq curr (point))
			    (setq ok (sp-get-enclosing-sexp 1))
			    (progn
			      (cond
			       ((and ok (or (and
					     (<= (sp-get ok :beg) curr )
					     (< curr (sp-get ok :beg-in)))
					    (and
					     (<= (sp-get ok :end-in) curr )
					     (< curr (sp-get ok :end)))))
				(sp-splice-sexp)
				t
				)
			       (t
				nil)
			       ))
			    ))
			)
	      (delete-forward-char 1)
	      ))
	  )
	(if any-changes (forward-char 1)))
      )
    )

  (defun sp-paired-delete-char-backward (&optional arg)
    "Delete char backward, deleting parens pair if deleting parens."
    (interactive "p")
    (setq arg (or arg 1))
    (if (< arg 0) (sp-paired-delete-char-forward (- arg)))
    (do ((i 0 (+ i 1))) ((>= i arg))
      (let ((inhibit-message t))
	(unless (or (when 
			(sp--looking-back
			 (sp--get-closing-regexp
			  (sp--get-pair-list-context 'navigate)))
		      (let (ok curr)
			(backward-char 1)
			(setq curr (point))
			(setq ok (sp-get-enclosing-sexp 1))
			(progn
			  (cond
			   ((and ok (or (and
					 (<= (sp-get ok :beg) curr )
					 (< curr (sp-get ok :beg-in)))
					(and
					 (<= (sp-get ok :end-in) curr )
					 (< curr (sp-get ok :end)))))
			    (sp-splice-sexp)
			    t
			    )
			   (t
			    (forward-char 1)
			    nil)
			   ))
			))
		    (when 
			(save-match-data
			  (sp--looking-back
			   (sp--get-opening-regexp
			    (sp--get-pair-list-context 'navigate))))
		      (let (ok (curr (point)))
			(setq ok (sp-get-enclosing-sexp 1))
			(progn
			  (cond
			   ((and ok
				 (< (sp-get ok :beg) curr )
				 (<= curr (sp-get ok :beg-in)))
			    (sp-splice-sexp)
			    t
			    )
			   (t
			    nil)
			   ))
			))
		    )
	  (backward-delete-char-untabify 1)
	  ))
      ))

  (defun sp-paired-delete-char (&optional arg)
    "Deletes char, and parens if present."
    (interactive "p^")
    (setq arg (or arg 1))
    (if (< arg 0)
	(sp-paired-backward-delete-char-untabify (- arg)))
    (sp-paired-delete-char-forward arg))
  
  (define-key prog-mode-map [remap backward-delete-char-untabify]
    #'sp-paired-delete-char-backward)
  ;; (define-key prog-mode-map [remap kill-line]
  ;;   #'kill-line)
  (define-key prog-mode-map
    [remap delete-char] #'sp-paired-delete-char)
  (define-key prog-mode-map
    [remap delete-forward-char] #'sp-paired-delete-char-forward)
```
