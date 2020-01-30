open! Core_kernel
open! Ecaml

let version = 0.1

let () =
  defun
    ("gopcaml-version" |> Symbol.intern)
    [%here]
    ~docstring:{|
    Returns gopcaml version number.
    |}
    (Returns Value.Type.int)
    (let open Defun.Let_syntax in
     
     return 1
     );
  defun
    ("gopcaml-test" |> Symbol.intern)
    [%here]
    ~docstring:{|
    Takes one argument NAME and says "Hello, NAME"
    |}
    (Returns Value.Type.unit)
    (let open Defun.Let_syntax in
     let%map_open name = required "name" string in
     message ("Hello," ^ name));
  provide ("gopcaml" |> Symbol.intern);;
    
