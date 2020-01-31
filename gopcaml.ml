open Core
open Ecaml

let version = 0.1
let gopcaml_version = "0.0"


let setup_local_variables () =
    let state_var = Buffer_local.defvar
      ("gopcaml-state" |> Symbol.intern)
      [%here]
      ~docstring:{|
    Holds gopcaml-mode state.
    |}
      ~type_: (Value.Type.option Gopcaml_state.State.ty)
      ~default_value:None () in
    state_var

let define_functions local_variables =
  let state_var = local_variables in
  defun
    ("gopcaml-version" |> Symbol.intern)
    [%here]
    ~docstring:{|
    Returns gopcaml version number.
    |}
    (Returns Value.Type.string_cached)
    (let open Defun.Let_syntax in
     return (Printf.sprintf "%s" gopcaml_version)
    );
  defun
  ("gopcaml-setup-state" |> Symbol.intern)
  [%here]
    ~docstring:{|
    Initializes state for gopcaml.
    |}
    (Returns Value.Type.unit)
    (let open Defun.Let_syntax in
     ignore (Gopcaml_state.setup_and_retrieve_gopcaml_state state_var);
     return ()
    )

let () =
  let local_variables = setup_local_variables () in
  define_functions local_variables;;

