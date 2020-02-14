open Core

(* returns total bounds of the current item *)
let bounds_iterator () =
  let min_position = ref (-1,-1) in
  let max_position = ref (0,0) in
  let retrieve_bounds () = !min_position,!max_position in
  let update_bounds pstr_loc =
    let open Lexing in
    let open Location in
    let () =
      let (ml, mc) = match !min_position with
          (-1,-1) -> pstr_loc.loc_start.pos_lnum, pstr_loc.loc_start.pos_cnum
        | (-1,v) -> pstr_loc.loc_start.pos_lnum, v
        | (v,-1) -> v, pstr_loc.loc_start.pos_cnum
        | v -> v in
      let min_position' =
        match pstr_loc.loc_start.pos_lnum = -1, pstr_loc.loc_start.pos_cnum = -1 with
        | false,false -> (min pstr_loc.loc_start.pos_lnum ml, min pstr_loc.loc_start.pos_cnum mc)
        | false,true -> (min pstr_loc.loc_start.pos_lnum ml, mc)
        | true,false -> (ml, min pstr_loc.loc_start.pos_cnum mc)
        | _ -> !min_position in
      min_position := min_position'
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
