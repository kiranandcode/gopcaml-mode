(lang dune 2.1)
(package
 (name gopcaml-mode)
 (synopsis "Ultimate Ocaml editing plugin, providing advanced structural editing, movement and analysis in Emacs")
  (description "Gopcaml-mode is a plugin for emacs for editing OCaml code. It aims to extend existing Emacs editing experience to closer match the features in modern IDEs.")
 (depends
 (core (>= 0.13.0))
 (ppx_deriving (>= 4.4))
 (ecaml (>= 0.13.0))
 (ocaml-compiler-libs (>= 0.12.1))
 )
 )

(name gopcaml-mode)
(version 20d0323-dirty)
(generate_opam_files true)
(license GPL-3.0-only)
(source (uri git+https://gitlab.com/gopiandcode/gopcaml-mode.git))
(bug_reports https://gitlab.com/gopiandcode/gopcaml-mode/issues)
(homepage https://gitlab.com/gopiandcode/gopcaml-mode)
(authors "Kiran Gopinathan")
(maintainers "kirang@comp.nus.edu.sg")

