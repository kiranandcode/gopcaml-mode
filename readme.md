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


## Installation
Gopcaml mode is implemented using a mixture of ocaml and elisp.

First, install the ocaml dependencies:
- core
- ppx_deriving 
- ecaml 
- ocaml-compiler-libs
- ocaml-migrate-parsetree
- extlib

Clone the repo to some local directory and run `dune build` within the repo directory.

Add the following to your init.el

`
(add-to-list 'load-path "<PATH-TO-GOPCAML-REPO>")

(require 'gopcaml-mode)
`


Enjoy your ultimate editing experience.
