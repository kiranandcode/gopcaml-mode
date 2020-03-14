# Gopcaml Ocaml Emacs Major Mode

The ultimate ocaml editing mode.

## Features
- AST-based code navigation - `C-M-n, C-M-p, C-M-u, C-M-d, C-M-f, C-M-b`

![ast-code-navigation](https://gitlab.com/gopiandcode/gopcaml-mode/-/raw/master/images/gopcaml_move_expression_example.gif?inline=false)

- AST-based code transformation -`C-M-N, C-M-P, C-M-F, C-M-B`

![ast-code-transform](https://gitlab.com/gopiandcode/gopcaml-mode/-/raw/master/images/gopcaml_move_function_example.gif?inline=false)

- Fixed move-to-defun, move-to-end-defun -`C-M-a, C-M-e`

![move-to-defun](https://gitlab.com/gopiandcode/gopcaml-mode/-/raw/master/images/gopcaml_move_to_defun_example.gif?inline=false)

- Jump to type hole - `TAB`

![jump-to-type-hole](https://gitlab.com/gopiandcode/gopcaml-mode/-/raw/master/images/gopcaml_move_to_type_hole.gif?inline=false)

- Automatic let binding expansion (i.e adds in automatically if defining let inside a let)

![automatic-let-bindings](https://gitlab.com/gopiandcode/gopcaml-mode/-/raw/master/images/gopcaml_auto_let_binding_example.gif?inline=false)

- Mark exp - `C-M-SPC`

![mark-sexp](https://gitlab.com/gopiandcode/gopcaml-mode/-/raw/master/images/gopcaml_mark_sexp.gif?inline=false)

- Move to nearest parameter - `C-c C-p`

![move-to-param](https://gitlab.com/gopiandcode/gopcaml-mode/-/raw/master/images/gopcaml_move_to_parameter.gif?inline=false)

- Move to nearest let def - `C-c C-o`

![move-to-let-def](https://gitlab.com/gopiandcode/gopcaml-mode/-/raw/master/images/gopcaml_move_to_nearest_letdef.gif?inline=false)

- Extract expression into letdef - `C-c C-e`

![extract-expression](https://gitlab.com/gopiandcode/gopcaml-mode/-/raw/master/images/gopcaml_extraction_expressions.gif?inline=false)


## Installation
Gopcaml mode is implemented using a mixture of ocaml and elisp.

- Install the project via opam:
```sh
opam install gopcaml-mode
```
- load the project in your init.el
```elisp
(let ((opam-share (ignore-errors (car (process-lines "opam" "config" "var" "share")))))
   (when (and opam-share (file-directory-p opam-share))
    ;; Register Gopcaml mode
     (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
     (autoload 'gopcaml-mode "gopcaml" nil t nil)
     ;; Automatically start it in OCaml buffers
     (add-hook 'tuareg-mode-hook 'gopcaml-mode t)
     (add-hook 'caml-mode-hook 'gopcaml-mode t)
     ))
```

Enjoy your ultimate editing experience.
