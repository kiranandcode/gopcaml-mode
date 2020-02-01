open Core
open Ecaml

module State = struct

  (** records whether the current file is an interface or implementation  *)
  type file_type = Interface | Implementation

  (** holds the parse tree for the current file  *)
  type parse_tree =
    | Impl of Parsetree.structure
    | Intf of Parsetree.signature
  
  (** type of state of plugin  *)
  type t = {
    (** file type of the current buffer *)
    file_type: file_type;
    (** parse tree of the current buffer *)
    parse_tree: parse_tree option;
    
  } 

  (** elisp type for state of system  *)
  let ty : t Value.Type.t =
    Caml_embed.create_type
      (Type_equal.Id.create
      ~name:"gopcaml-state"
      Sexplib0.Sexp_conv.sexp_of_opaque)

  let default = {
    file_type = Interface;
    parse_tree = None;
  }

end


let build_implementation_tree value =
  let lexbuf = Lexing.from_string ~with_positions:true value in
  try Either.First (State.Impl (Parse.implementation lexbuf)) with
    Syntaxerr.Error e -> Either.Second e

let build_interface_tree value =
  let lexbuf = Lexing.from_string ~with_positions:true value in
  try Either.First (State.Intf (Parse.interface lexbuf)) with
    Syntaxerr.Error e -> Either.Second e

(** determines the file-type of the current file based on its extension *)
let retrieve_current_file_type ~implementation_extensions ~interface_extensions =
  Current_buffer.file_name ()
  |> Option.bind ~f:(fun file_name ->
      String.split ~on:'.' file_name
      |> List.last
      |> Option.bind ~f:(fun ext ->
          if List.mem ~equal:String.(=) implementation_extensions ext
          then Some State.Implementation
          else if List.mem ~equal:String.(=) interface_extensions ext
          then Some State.Interface
          else None
        )
    )

(** attempts to parse the current buffer according to the inferred file type  *)
let parse_current_buffer file_type =
  (* retrieve the text for the entire buffer *)
  let buffer_text = Current_buffer.contents ()
                    |> Text.to_utf8_bytes  in
  message "Building parse tree - may take a while if the file is large...";
  let start_time = Time.now () in
  let parse_tree = match file_type with
    | State.Implementation -> build_implementation_tree buffer_text
    | State.Interface -> build_interface_tree buffer_text
  in
  match parse_tree with
  | Either.Second e -> 
    message ("Could not build parse tree: " ^ ExtLib.dump e);
    None
  | Either.First tree ->
    let end_time = Time.now () in
    message (Printf.sprintf
               "Successfully built parse tree (%f ms)"
               ((Time.diff end_time start_time) |> Time.Span.to_ms)
            );
    Some tree

(** sets up the gopcaml-mode state - intended to be called by the startup hook of gopcaml mode*)
let setup_gopcaml_state
    ~state_var ~interface_extension_var ~implementation_extension_var =
  let current_buffer = Current_buffer.get () in
  (* we've set these values in their definition, so it doesn't make sense for them to be non-present *)
  let interface_extensions =
    Customization.value interface_extension_var in
  let implementation_extensions =
    Customization.value implementation_extension_var in
  let file_type =
    let inferred = retrieve_current_file_type
        ~implementation_extensions ~interface_extensions in
    match inferred with
    | Some vl -> vl
    | None ->
      message "Could not infer the ocaml type (interface or \
               implementation) of the current file - will attempt
               to proceed by defaulting to implementation.";
      State.Implementation
  in
  let parse_tree = parse_current_buffer file_type in
  if Option.is_none parse_tree then
    message "Could not build parse tree - please ensure that the \
             buffer is syntactically correct and call \
             gopcaml-initialize to enable the full POWER of syntactic \
             editing.";
  let state = State.{
      file_type = file_type;
      parse_tree = parse_tree;
    } in
  Buffer_local.set state_var state current_buffer
  


