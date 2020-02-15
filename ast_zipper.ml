open Core

type t =
  | Signature_item of Parsetree.signature_item
  | Structure_item of Parsetree.structure_item
  | Sequence of t list * t * t list

(* Huet's zipper for asts *)
type zipper =
  | Top
  | Node of {below: t list; parent: zipper; above: t list; }

type location =
  | MkLocation of t * zipper

let rec t_to_bounds = function
  | Signature_item si ->
    let (iter,get) = Ast_transformer.bounds_iterator () in
    iter.signature_item iter si;
    get ()
  | Structure_item si ->
    let (iter,get) = Ast_transformer.bounds_iterator () in
    iter.structure_item iter si;
    get ()
  (* | Signature_item { psig_loc = { loc_start; loc_end;_ }; _ } 
   * | Structure_item { pstr_loc = { loc_start; loc_end;_ }; _ } ->
   *   (loc_start.pos_cnum,loc_end.pos_cnum) *)
  (* if its a sequence, take the union *)
  | Sequence (left,elem,right) ->
    List.map ~f:t_to_bounds (left @ right)
    |> List.fold ~f:(fun (x1,y1) (x2,y2) -> (min x1 x2, max y1 y2)) ~init:(t_to_bounds elem)

(** converts a zipper to the bounds of the current item *)
let to_bounds (MkLocation (current,_)) = 
  t_to_bounds current

(** updates the bounds of the zipper by a fixed offset *)
let update_bounds ~(diff:int) state =
  let mapper = {Ast_mapper.default_mapper with location = (fun _ { loc_start; loc_end; loc_ghost } ->
      Location.{
        loc_start={loc_start with pos_cnum = (if loc_start.pos_cnum = -1
                                              then -1
                                              else loc_start.pos_cnum + diff)};
        loc_end={loc_end with pos_cnum = (if loc_end.pos_cnum = -1
                                          then -1
                                          else loc_end.pos_cnum + diff)};
        loc_ghost=loc_ghost}
    ) } in
  let rec update state = 
    match state with
    | Signature_item si -> Signature_item (mapper.signature_item mapper si)
    | Structure_item si -> Structure_item (mapper.structure_item mapper si)
    | Sequence (l,c,r) ->
      let update_ls = List.map ~f:update in
      Sequence (update_ls l, update c, update_ls r) in
  update state

let make_zipper_intf left intf right =
  let left = List.map ~f:(fun x -> Signature_item x) left in
  let right = List.map ~f:(fun x -> Signature_item x) right in
  let intf = Signature_item intf in
  MkLocation (Sequence (List.rev left, intf, right), Top)

let make_zipper_impl left impl right =
  let left = List.map ~f:(fun x -> Structure_item x) left in
  let right = List.map ~f:(fun x -> Structure_item x) right in
  let impl = Structure_item impl in
  MkLocation (Sequence (List.rev left, impl, right), Top)

let go_up (MkLocation (current,parent)) =
  match parent with
  | Top -> None
  | Node { below; parent; above } ->
    let current = Sequence (below,current,above) in
    Some (MkLocation (current,parent))

let go_down (MkLocation (current,parent)) =
  match current with
  | Sequence (left,focused,right) ->
    Some (MkLocation (focused, Node {below=left;parent;above=right;}))
  | _ -> None


let go_left (MkLocation (current,parent)) =
  match parent with
  | Node { below=l::left; parent; above } ->
    Some (MkLocation (l, Node {below=left; parent; above=current::above}))
  | _ -> None

let go_right (MkLocation (current,parent)) =
  match parent with
  | Node { below; parent; above=r::right } ->
    Some (MkLocation (r, Node {below=current::below; parent; above=right}))
  | _ -> None


(** swaps two elements at the same level, returning the new location  *)
let calculate_swap_bounds (MkLocation (current,parent)) =
  match parent with
  | Node { below=l::left; parent; above=r::right; } ->
    let current_bounds =  t_to_bounds current in
    let prev_bounds = t_to_bounds l in
    let prev_diff = snd current_bounds - snd prev_bounds in
    let current_diff = fst prev_bounds - fst current_bounds in
    Some (
      current_bounds,
      prev_bounds,
      (MkLocation (
        r,
        (Node {
            below=(update_bounds ~diff:prev_diff l)::(update_bounds ~diff:current_diff current)::left;
            parent;
            above=right
          }))))
  | _ -> None


(** swaps two elements forward at the same level, returning the new location  *)
let calculate_swap_forward_bounds (MkLocation (current,parent)) =
  match parent with
  | Node { below=left; parent; above=r::right; } ->
    let current_bounds =  t_to_bounds current in
    let prev_bounds = t_to_bounds r in
    let prev_diff = fst current_bounds - fst prev_bounds in
    let current_diff = snd prev_bounds - snd current_bounds in
    Some (
      current_bounds,
      prev_bounds,
      MkLocation (
        (update_bounds ~diff:current_diff current),
        (Node {
            below=(update_bounds ~diff:prev_diff r)::left;
            parent;
            above=right;
          })))
  | _ -> None


(** swaps two elements forward at the same level, returning the new location  *)
let calculate_swap_backwards_bounds (MkLocation (current,parent)) =
  match parent with
  | Node { below=l::left; parent; above=right; } ->
    let current_bounds =  t_to_bounds current in
    let prev_bounds = t_to_bounds l in
    let prev_diff = snd current_bounds - snd prev_bounds in
    let current_diff = fst prev_bounds - fst current_bounds in
    Some (
      current_bounds,
      prev_bounds,
      MkLocation (
        (update_bounds ~diff:current_diff current),
        (Node {
            below=left;
            parent;
            above=(update_bounds ~diff:prev_diff l)::right;
          })))
  | _ -> None

