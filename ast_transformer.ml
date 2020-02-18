open Core

(* returns total bounds of the current item *)
let bounds_iterator () =
  let min_position = ref None in
  let max_position = ref None in
  let retrieve_bounds () = Option.value_exn !min_position, Option.value_exn !max_position in
  let update_bounds pstr_loc =
    let open Lexing in
    let open Location in
    let () =
      let start_cnum = min pstr_loc.loc_end.pos_cnum pstr_loc.loc_start.pos_cnum in
      let min_cnum = match !min_position with
          None -> start_cnum
        | Some v -> v in
        if  start_cnum = -1 then ()
        else min_position := Some (min min_cnum start_cnum)
    in
    let () =
      let end_cnum = max pstr_loc.loc_end.pos_cnum pstr_loc.loc_start.pos_cnum in
      let max_cnum = match !max_position with
          None -> end_cnum
        | Some v -> v in
        if  end_cnum = -1 then ()
        else max_position := Some (max max_cnum end_cnum) 
    in
    ()
  in
  Ast_iterator.{
    default_iterator
    with
      location = fun _ -> update_bounds
  }, retrieve_bounds

let complete_bounds_iterator () =
  let min_col_position = ref (-1) in
  let max_col_position = ref (-1) in
  let min_line_position = ref (-1) in
  let max_line_position = ref (-1) in
  let cmp f a b = match (a,b) with
    | -1, -1 -> -1
    | -1,  b -> b
    | a,  -1 -> a
    | a,  b  -> f a b in
  let min = cmp min in
  let max = cmp max in
  let retrieve_bounds () =
    (!min_line_position, !min_col_position),
    (!max_line_position, !max_col_position)
  in
  let update_bounds pstr_loc =
    let open Lexing in
    let open Location in
    let () =
      min_col_position := min !min_col_position pstr_loc.loc_start.pos_cnum;
      min_line_position := min !min_line_position pstr_loc.loc_start.pos_lnum;
      max_col_position := max !max_col_position pstr_loc.loc_end.pos_cnum;
      max_line_position := max !max_line_position pstr_loc.loc_end.pos_lnum;
    in
    ()
  in
  Ast_iterator.{
    default_iterator
    with
      location = fun _ -> update_bounds
  }, retrieve_bounds

(* returns the nearest enclosing bounds to a point *)
let enclosing_bounds_iterator point ()  =
  let bounds = ref None in
  let retrieve_bounds () = !bounds in
  let within_bounds st ed point = st <= point && point <= ed  in
  let encloses (st,ed) (st',ed') =
    within_bounds st ed st' && within_bounds st ed ed' in
  let smaller (st,ed) (st',ed') =
    ed - st <= st' - ed' in
  (* given two regions enclosing point, returns the smaller one*)
  let choose (st,ed) (st',ed') =
    if encloses (st,ed) (st',ed') || smaller (st',ed') (st,ed)
    then (st',ed')
    else (st,ed) in
  let update_bounds ({
      loc_start={ pos_cnum=st;_ };
      loc_end={pos_cnum=ed;_}; _
    }: Location.t) =
    if within_bounds st ed point then
      match !bounds with
      | None -> bounds := Some (st,ed)
      | Some (st',ed') -> bounds := Some (choose (st,ed) (st',ed'))
    else () in
  Ast_iterator.{
    default_iterator
    with
      location = fun _ -> update_bounds
  },retrieve_bounds
