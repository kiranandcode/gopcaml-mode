open Core
open Ecaml

module State = struct

  module Filetype = struct
    (** records whether the current file is an interface or implementation  *)
    type s = Interface | Implementation [@@deriving sexp]

    module Enum : Ecaml.Value.Type.Enum with type t = s = struct
      type t = s
      let all = [Interface; Implementation]
      let sexp_of_t = sexp_of_s
    end

    let ty =
      let to_ecaml file_type =
        match file_type with
        | Interface -> Value.intern "interface"
        | Implementation -> Value.intern "implementation" in
      Value.Type.enum
        (Sexp.Atom "filetype")
        (module Enum)
        to_ecaml

    let to_string  = function
    | Interface -> "interface"
    | Implementation -> "implementation"

    type t = s
  end

  (** holds the parse tree for the current file  *)
  type parse_tree =
    | Impl of Parsetree.structure_item list
    | Intf of Parsetree.signature_item list
  
  (** type of state of plugin - pre-validation *)
  type t = {
    (** file type of the current buffer *)
    file_type: Filetype.t;
    (** parse tree of the current buffer *)
    parse_tree: parse_tree option;
    
  } 

  module Validated = struct
    (** type of valid state of plugin  *)
    type s = {
      (** file type of the current buffer *)
      file_type: Filetype.t;
      (** parse tree of the current buffer *)
      parse_tree: parse_tree;
    } 

    let of_state (state: t) =
      state.parse_tree
      |> Option.map ~f:(fun parse_tree ->
          {file_type=state.file_type; parse_tree})

    type t = s

  end
  

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

let highlight_code_bounds ?current_buffer ~state_var =
  let current_buffer = match current_buffer with Some v -> v | None -> Current_buffer.get () in
  let state = Buffer_local.get_exn state_var current_buffer in
  let (iter,get_result) = Ast_transformer.bounds_iterator () in
  let ((min_line,min_column), (max_line,max_column)) = match state.parse_tree with
  | State.Impl si_list ->
    iter.structure iter si_list;
    get_result ()
  | State.Intf si_list ->
    iter.signature iter si_list;
    get_result ()
  in

  
()

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
          message (Printf.sprintf "extension of %s is %s" file_name ext);
          message (Printf.sprintf "impls: %s, intfs: %s"
                     (String.concat ~sep:", " implementation_extensions)
                     (String.concat ~sep:", " interface_extensions)
                  );

          if List.mem ~equal:String.(=) implementation_extensions ext
          then begin
            message "filetype is implementation";
            Some State.Filetype.Implementation
          end
          else if List.mem ~equal:String.(=) interface_extensions ext
          then begin
            message "filetype is interface";
            Some State.Filetype.Interface
          end
          else None
        )
    )

(** attempts to parse the current buffer according to the inferred file type  *)
let parse_current_buffer file_type =
  (* retrieve the text for the entire buffer *)
  let buffer_text = Current_buffer.contents ()
                    |> Text.to_utf8_bytes  in
  let _ = let open State.Filetype in
    match file_type with
    | Interface -> message "filetype is interface."
    | Implementation -> message "filetype is implementation." in
  message "Building parse tree - may take a while if the file is large...";
  let start_time = Time.now () in
  let parse_tree =
    let open State.Filetype in
    match file_type with
    | Implementation -> build_implementation_tree buffer_text
    | Interface -> build_interface_tree buffer_text
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
      State.Filetype.Implementation
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
  Buffer_local.set state_var (Some state) current_buffer

(** retrieve the gopcaml state *)
let get_gopcaml_file_type ?current_buffer ~state_var () =
  let current_buffer = match current_buffer with Some v -> v | None -> Current_buffer.get () in
  let state = Buffer_local.get_exn state_var current_buffer in
  let file_type_name = State.Filetype.to_string state.State.file_type in
  file_type_name

(** update the file type of the variable   *)
let set_gopcaml_file_type ?current_buffer ~state_var file_type =
  let current_buffer = match current_buffer with Some v -> v | None -> Current_buffer.get () in
  let state = Buffer_local.get_exn state_var current_buffer in
  let state = State.{state with file_type = file_type } in
  Buffer_local.set state_var (Some state) current_buffer
  
(** retrieves the gopcaml state value, attempting to construct the
   parse tree if it has not already been made *)
let retrieve_gopcaml_state ?current_buffer ~state_var =
  let open State in 
  let current_buffer = match current_buffer with Some v -> v | None -> Current_buffer.get () in
  let state = Buffer_local.get_exn state_var current_buffer in
  if Option.is_none state.parse_tree then begin
    message "Buffer AST not built - rebuilding...";
    let state = {state with parse_tree = parse_current_buffer state.file_type} in
    Buffer_local.set state_var (Some state) current_buffer;
    State.Validated.of_state state
  end else
    (State.Validated.of_state state)

