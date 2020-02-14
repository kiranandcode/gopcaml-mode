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

(* converts a zipper to the bounds of the current item *)
let to_bounds (MkLocation (current,_)) = 
  let rec to_bounds = function
    | Signature_item { psig_loc = { loc_start; loc_end;_ }; _ } 
    | Structure_item { pstr_loc = { loc_start; loc_end;_ }; _ } ->
      (loc_start.pos_cnum,loc_end.pos_cnum)
    (* if its a sequence, take the union *)
    | Sequence (left,elem,right) ->
      List.map ~f:to_bounds (left @ right)
      |> List.fold ~f:(fun (x1,y1) (x2,y2) -> (min x1 x2, max y1 y2)) ~init:(to_bounds elem) in
  to_bounds current

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


