open Core
open Ecaml

module State_int = struct

  (** type of state of plugin  *)
  type t = {
    (** parse tree of the current buffer *)
    parse_tree: Parsetree.structure;
  } 

end

module State = struct

  include State_int

  let ty : t Value.Type.t =
    Caml_embed.create_type
      (Type_equal.Id.create
      ~name:"gopcaml-state"
      Sexplib0.Sexp_conv.sexp_of_opaque)

end




let build_parse_tree value =
  let lexbuf = Lexing.from_string ~with_positions:true value in
  try Either.First (Parse.implementation lexbuf) with  Syntaxerr.Error e -> Either.Second e

let print_parse_tree = ExtLib.dump


let parse_current_buffer () =
  (* retrieve the text for the entire buffer *)
  let buffer_text = Current_buffer.contents ()
                    |> Text.to_utf8_bytes  in
  message "Building parse tree - may take a while if the file is large...";
  let start_time = Time.now () in
  let parse_tree = build_parse_tree buffer_text
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

(** initializes gopcaml state if it has not been initialized already  *)
let initialize_gopcaml_state ?current_buffer state_var =
  let state =
    let open Option.Let_syntax in
    let%bind parse_tree = parse_current_buffer () in
    return State.{parse_tree} in
  let current_buffer = match current_buffer with
      Some v -> v | None -> Current_buffer.get ()
  in 
  Buffer_local.set state_var state current_buffer;
  state

(** setup's gopcaml state and retrieves gopcaml mode state variable *)
let setup_and_retrieve_gopcaml_state state_var =
  let current_buffer = Current_buffer.get () in 
  match Buffer_local.get state_var current_buffer with
  | Some state -> Some state
  | None ->
    initialize_gopcaml_state ~current_buffer state_var
