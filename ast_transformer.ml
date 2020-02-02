open Core

let bounds_iterator () =
  let min_position = ref (0,0) in
  let max_position = ref (0,0) in
  let retrieve_bounds () = !min_position,!max_position in
  let update_bounds pstr_loc =
    let open Lexing in
    let open Location in
    let () =
      let (ml, mc) = !min_position in
      min_position := (min pstr_loc.loc_start.pos_lnum ml, min pstr_loc.loc_start.pos_cnum mc)
    in
    let () =
      let (ml, mc) = !max_position in
      max_position := (max pstr_loc.loc_end.pos_lnum ml, max pstr_loc.loc_end.pos_cnum mc)
    in
    ()
  in
  Ast_iterator.{
    default_iterator
    with
      location = fun _ -> update_bounds
  }, retrieve_bounds
