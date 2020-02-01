open Core
open Ecaml

let version = 0.1
let gopcaml_version = "0.0"

module Variables = struct


  let state_var = Buffer_local.defvar
      ("gopcaml-state" |> Symbol.intern)
      [%here]
      ~docstring:{|
    Holds gopcaml-mode state.
    |}
      ~type_: (Gopcaml_state.State.ty)
      ~default_value:(Gopcaml_state.State.default)
      ()
end
module Customizable = struct

  let gopcaml_group =
    Customization.Group.defgroup "gopcaml"
      [%here]
      ~docstring:{|
      Gopcaml mode customization
    |}
      ~parents:[]

  let interface_extensions_var = Customization.defcustom
      ~show_form:true
      ("gopcaml-interface-extensions" |> Symbol.intern)
      [%here]
      ~group:gopcaml_group
      ~docstring:{|
      List of extensions to be automatically assumed to be interface files.
    |}
      ~type_: (Value.Type.list Value.Type.string)
      ~customization_type:(Customization.Type.Repeat Customization.Type.String)
      ~standard_value:["ml"]
      ()

  let implementation_extensions_var = Customization.defcustom
      ~show_form:true
      ("gopcaml-implementation-extensions" |> Symbol.intern)
      [%here]
      ~group:gopcaml_group
      ~docstring:{|
      List of extensions to be automatically assumed to be implementation files.
    |}
      ~type_: (Value.Type.list Value.Type.string)
      ~customization_type:(Customization.Type.Repeat Customization.Type.String)
      ~standard_value:["mli"]
      ()
end

let () =
  defun
    ("gopcaml-version" |> Symbol.intern)
    [%here]
    ~docstring:{|
    Returns gopcaml version number.
    |}
    (Returns Value.Type.string_cached)
    (let open Defun.Let_syntax in
     return (Printf.sprintf "%s" gopcaml_version)
    )

let gopcaml_mode =
  Major_mode.define_derived_mode
    ("gopcaml-mode" |> Symbol.intern)
    [%here]
    ~docstring:"OCaml major mode for structural syntax-aware \
                editing. Can be thought of as tuareg on steriods!"
    ~mode_line:"GopCaml"
    ~parent:Major_mode.Tuareg.major_mode
    ~initialize:((Returns Value.Type.unit),
                 fun () ->
                   ignore (Gopcaml_state.setup_gopcaml_state
                             ~state_var:Variables.state_var
                             ~interface_extension_var:Customizable.interface_extensions_var
                             ~implementation_extension_var:Customizable.implementation_extensions_var
                          )
                )
    ()

(* Finally, provide the gopcaml symbol  *)
let () =
  provide ("gopcaml" |> Symbol.intern)
