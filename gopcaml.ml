open Core
open Ecaml


let to_special_string value = Lexing.from_string ~with_positions:true value
                              |> Parse.implementation
                              |> ExtLib.dump
            

let version = 0.1


let print_region start_pos end_pos : unit =
  let print_region_internal () = 
    let text = Current_buffer.contents
      ~start:start_pos
      ~end_:end_pos
      () in
    Text.to_utf8_bytes text
    |> to_special_string
    |>  message
  in
  ignore (print_region_internal ())
      

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
     print_region startm endm
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
    
