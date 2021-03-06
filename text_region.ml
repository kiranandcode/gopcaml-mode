open Core
open Generic_types

module Diff = struct
  type t = int * int

  let of_pair ~line ~col = (line,col)

  let negate (line,col) = (-line,-col)

  let combine (l1,c1) (l2,c2) = (l1 + l2,c1 + c2)

  let to_string (l1,c1) = Printf.sprintf "(%d,%d)" l1 c1

  (* increments the diff by 1 newline + indentation *)
  let add_newline_with_indent ~indent (line,col)  =
    (line + 1, col + 1 + indent)

  let update_lexing_position (pos: Lexing.position) (line,col) : Lexing.position =
    let cnum = match pos.pos_cnum with -1 -> -1 | _ -> max (pos.pos_cnum + col) (-1) in
    let lnum = match pos.pos_lnum with -1 -> -1 | _ -> max (pos.pos_lnum + line) (-1) in
    {pos with pos_cnum = cnum; pos_lnum = lnum}

end

module Position = struct
  type t = {line: int; col: int}

  let of_lexing (pos: Lexing.position) : t =
    let Lexing.{pos_lnum; pos_cnum; _} = pos in
    {line=pos_lnum; col = pos_cnum}

  let (+) {line=l1;col=c1} (line,col) =
    let c1 = match c1 with -1 -> -1 | _ -> max (c1 + col) (-1) in
    let l1 = match l1 with -1 -> -1 | _ -> max (l1 + line) (-1) in
    {line=l1; col = c1}

  let cmp f a b = match (a,b) with
    -1,-1 -> -1
    | a,-1 -> a
    | -1,b -> b
    | a,b -> f a b 
  let min = cmp min
  let max = cmp max

  let min {line=l1;col=c1} {line=l2;col=c2} =
    {line=min l1 l2; col=min c1 c2}

  let max {line=l1;col=c1} {line=l2;col=c2} =
    {line=max l1 l2; col=max c1 c2}

end

type pos = Position.t

type t = (pos[@opaque]) * (pos[@opaque])

let to_bounds Position.({col=cs;_},{col=ce; _}) = (cs,ce)

let to_string Position.({col=cs;line=ls},{col=ce; line=le}) =
  Printf.sprintf "{col: %d - %d; line: %d - %d}" cs ce ls le

let pp = to_string

let shift_region (r_start, r_end) shift =
  let open Position in
  r_start + shift, r_end + shift

let extend_region (r_start, r_end) shift =
  let open Position in
  r_start, r_end + shift

let of_location (loc :Location.t) : t =
  let st = Position.of_lexing loc.loc_start in
  let ed = Position.of_lexing loc.loc_end in
  (st,ed)

let union (st1,ed1) (st2,ed2) =
  let open Position in
  let (st1,ed1) = min st1 ed1, max st1 ed1 in
  let (st2,ed2) = min st2 ed2, max st2 ed2 in
  (Position.min st1 st2),(Position.max ed1 ed2)

let ast_bounds_iterator () =
  let bounds = ref None in
  let retrieve_bounds () = Option.value_exn !bounds in
  let update_bounds pstr_loc =
    let new_bounds = of_location pstr_loc in
    let new_bounds = match !bounds with
      | None -> new_bounds
      | Some old_bounds -> union old_bounds new_bounds in
    bounds := Some new_bounds
  in
  Ast_iterator.{
    default_iterator
    with
      location = fun _ -> update_bounds
  }, retrieve_bounds

let ast_bounds_mapper ~diff =
  {Ast_mapper.default_mapper with
   location = (fun _ ({ loc_start; loc_end; _ } as loc) ->
       {loc with
        loc_start= Diff.update_lexing_position loc_start diff;
        loc_end= Diff.update_lexing_position loc_end diff; }
     ) }

let before_point (({  col=c1; _ },_):t) point =
  match c1 with
  | -1 -> false
  | a  ->
    point < a

let contains_point (({  col=c1; _ },{  col=c2; _ }):t) point =
  match c1,c2 with
  | -1,-1 | -1, _ | _, -1 -> false
  | a, b  ->
    a <= point && point <= b

let contains_ne_point (({  col=c1; _ },{  col=c2; _ }):t) point =
  match c1,c2 with
  | -1,-1 | -1, _ | _, -1 -> false
  | a, b  -> a < point && point < b


let equals_point ?forward (({  col=c1; _ },{  col=c2; _ }):t) point =
  match c1,c2 with
  | -1,-1 | -1, _ | _, -1 -> false
  | a, b  ->
    match forward with
    | Some true -> a = point
    | Some false -> b = point
    | _ -> a = point || point = b


let distance ?forward (({ col=c1; _ },{ col=c2; _ }):t) point =
  match c1,c2 with
  | -1,-1 | -1, _ | _, -1 -> None
  | start, ed  ->
    match forward with
    | Some true -> Some (abs (start - point))
    | Some false -> Some (abs (ed - point))
    | _ -> Some (min (abs (start - point)) (abs (ed - point)))

let distance_line ?forward (({ col=c1; line=l1 },{ col=c2; line=l2 }):t) ~point ~line =
  let diff c1 c2 point = match c1,c2 with
    | -1,-1 | -1, _ | _, -1 -> None
    | start, ed  ->
      match forward with
      | None -> Some (min (abs (start - point)) (abs (ed - point)))
      | Some true -> Some (abs (start - point))
      | Some false -> Some (abs (ed - point))
  in
  let diff_line c1 c2 point = match c1,c2 with
    | -1,-1 | -1, _ | 0,0 | 0,_ | _,0 | _, -1 -> None
    | start, ed  ->
      match forward with
      | None -> Some (min (abs (start - point)) (abs (ed - point)))
      | Some true -> Some (abs (start - point))
      | Some false -> Some (abs (ed - point))
  in
  let col_diff = diff c1 c2 point in
  let line_diff = diff_line l1 l2 line in
  (col_diff, line_diff)

let line_start (({ line=l1; _ },_):t) = l1

(* let line_end ((_,{ line=l1; _ }):t) = l1 *)

let column_start (({ col=c1; _ },_):t) = c1

let column_end ((_,{ col=c1; _ }):t) = c1

let to_diff (({ line=l1; col=c1; },{ line=l2; col=c2; }): t) =
  let (>>=) x f = Option.bind ~f x in
  let unwrap vl = match  vl with -1 -> None | v -> Some v in
  (unwrap l1) >>= fun l1 -> 
  (unwrap l2) >>= fun l2 -> 
  (unwrap c1) >>= fun c1 -> 
  (unwrap c2) >>= fun c2 -> 
  Some (l1 - l2, c1 - c2)

let swap_diff
    (({ line=a_l1; col=a_c1; },{ line=a_l2; col=a_c2; }): t)
    (({ line=b_l1; col=b_c1; },{ line=b_l2; col=b_c2; }): t) =
  let (>>=) x f = Option.bind ~f x in
  let unwrap vl = match  vl with -1 -> None | v -> Some v in
  (unwrap a_l1) >>= fun a_l1 -> 
  (unwrap a_l2) >>= fun a_l2 -> 
  (unwrap a_c1) >>= fun a_c1 -> 
  (unwrap a_c2) >>= fun a_c2 -> 
  (unwrap b_l1) >>= fun b_l1 -> 
  (unwrap b_l2) >>= fun b_l2 -> 
  (unwrap b_c1) >>= fun b_c1 -> 
  (unwrap b_c2) >>= fun b_c2 -> 
  let forward_shift = (a_l2 - b_l2, a_c2 - b_c2) in
  let backwards_shift = (b_l1 - a_l1, b_c1 - a_c1) in
  Some (forward_shift,backwards_shift)

let diff_between
    ((_, { line=a_l1; col=a_c1; }): t)
    (({ line=b_l1; col=b_c1; }, _): t) =
  let (>>=) x f = Option.bind ~f x in
  let unwrap vl = match  vl with -1 -> None | v -> Some v in
  (unwrap a_l1) >>= fun a_l1 -> 
  (unwrap a_c1) >>= fun a_c1 -> 
  (unwrap b_l1) >>= fun b_l1 -> 
  (unwrap b_c1) >>= fun b_c1 -> 
  let backwards_shift = (a_l1 - b_l1, a_c1 - b_c1) in
  Some (backwards_shift)


let to_shift_from_start ((_,{ line=a_l2; col=a_c2; }): t) =
  (a_l2,a_c2)

