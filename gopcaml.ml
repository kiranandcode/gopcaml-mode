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
      ~type_: (Value.Type.option Gopcaml_state.State.ty)
      ~default_value:(None)
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
      ~standard_value:["mli"]
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
      ~standard_value:["ml"]
      ()
end

let define_functions () =
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
    ("gopcaml-set-file-type" |> Symbol.intern)
    [%here]
    ~docstring:{|
                       Configure gopcaml to treat current buffer as FILE-TYPE
                       |}
    (Returns Value.Type.unit)
    (let open Defun.Let_syntax in
     let%map_open file_type = required "file-type" Gopcaml_state.State.Filetype.ty in
     Gopcaml_state.set_gopcaml_file_type
       ~state_var:Variables.state_var
       file_type
    );
  defun
    ("gopcaml-get-file-type" |> Symbol.intern)
    [%here]
    ~docstring:{|
                       Retrieve gopcaml's stored file type for the current buffer.
                       |}
    (Returns Value.Type.string)
    (let open Defun.Let_syntax in
     let%map_open getter = return (Gopcaml_state.get_gopcaml_file_type
                                     ~state_var:Variables.state_var) in
     (getter ()));
  defun
    ("gopcaml-get-enclosing-structure-bounds" |> Symbol.intern)
    [%here]
    ~docstring:{| Retrieve a pair of points enclosing the structure item at the current point |}
    (Returns (Value.Type.option (Value.Type.list Position.type_)))
    (let open Defun.Let_syntax in
     let%map_open point = required "point" (Position.type_) in
     Gopcaml_state.retrieve_enclosing_structure_bounds
       ~state_var:Variables.state_var
       point
     |> Option.map ~f:(fun (a,b) -> [a;b])
     )  


let gopcaml_mode =
  Major_mode.define_derived_mode
    ("gopcaml-mode" |> Symbol.intern)
    [%here]
    ~docstring:"OCaml major mode for structural syntax-aware \
                editing. OCaml editing on steriods!"
    ~mode_line:"GopCaml"
    ~parent:Major_mode.Tuareg.major_mode
    ~initialize:((Returns Value.Type.unit),
                 fun () ->
                   message "Building initial state";
                   let _ =  (Gopcaml_state.setup_gopcaml_state
                               ~state_var:Variables.state_var
                               ~interface_extension_var:Customizable.interface_extensions_var
                               ~implementation_extension_var:Customizable.implementation_extensions_var
                            ) in
                   define_functions ()
                )
    ()

(* Finally, provide the gopcaml symbol  *)
let () =
  provide ("gopcaml" |> Symbol.intern)   


