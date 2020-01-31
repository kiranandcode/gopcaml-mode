open Core
open Ecaml



type structure = Parsetree.structure_item_desc
module Ast = struct

  (* Hack to allow storing parse trees in emacs *)
  let ty :  Parsetree.structure Value.Type.t = Value.Type.map
      Value.Type.string
      ~name:(Sexp.Atom "parse-tree")
      ~of_:(fun x -> Obj.magic x) (* lol *)
      ~to_:(fun x -> Obj.magic x)

end



let build_parse_tree value =
  let lexbuf = Lexing.from_string ~with_positions:true value in
  try Some (Parse.implementation lexbuf) with  _ -> None

let print_parse_tree = Pprintast.string_of_structure



(* let to_special_string value = Lexing.from_string ~with_positions:true value
 *                               |> Parse.implementation
 *                               |> migrate.Migrate_parsetree.Versions.copy_structure
 *                               |> ExtLib.dump *)


let version = 0.1

let print_region ~parse_tree_var start_pos end_pos : unit =
  let print_region_internal () = 
    let text = Current_buffer.contents
      ~start:start_pos
      ~end_:end_pos
      () in
    let current_buffer = Current_buffer.get () in
    let parse_tree = match Buffer_local.get parse_tree_var current_buffer with
    | None ->
      message "Parse tree not found, rebuilding.";
      let text = Text.to_utf8_bytes text in
      let parse_tree = build_parse_tree text in
      Buffer_local.set parse_tree_var parse_tree current_buffer;
      parse_tree 
    | pt -> pt in
    match parse_tree with
    | Some value -> 
      print_parse_tree value |>  message
    | None ->
      message "Could not generate parse tree - syntax error."
  in
  ignore (print_region_internal ())
      


let () =
  let parse_tree_var = Buffer_local.defvar
    ("gopcaml-parse-tree" |> Symbol.intern)
    [%here]
    ~docstring:{|
    Holds the parse tree.
    |}
    ~type_: (Value.Type.option Ast.ty)
    ~default_value:None () in
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
    ("gopcaml-print-region" |> Symbol.intern)
    [%here]
    ~docstring:{|
    Prints the text between START and END.
    |}
    (Returns Value.Type.unit)
    (let open Defun.Let_syntax in
     let%map_open startm = required "start" Value.Type.value
     and endm  = Defun.required "end" Value.Type.value in
     let startm = Position.of_value_exn startm in
     let endm = Position.of_value_exn endm in
     print_region ~parse_tree_var startm endm
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
    
