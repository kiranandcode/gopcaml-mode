open Core

module TextRegion = struct
  type pos = int * int
  type t = pos * pos
end

type t =
  | Signature_item of Parsetree.signature_item
  | Structure_item of Parsetree.structure_item
  | Sequence of (int * int) option * t list * t * t list
  | EmptySequence of (int * int)

(* Huet's zipper for asts *)
type zipper =
  | Top
  | Node of {bounds: (int * int) option; below: t list; parent: zipper; above: t list; }

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
  (* if its a sequence, take the union *)
  | Sequence (None, left,elem,right) ->
    List.map ~f:t_to_bounds (left @ right)
    |> List.fold ~f:(fun (x1,y1) (x2,y2) -> (min x1 x2, max y1 y2)) ~init:(t_to_bounds elem)
  | Sequence (Some (s,r), left,elem,right) ->
    List.map ~f:t_to_bounds (left @ elem :: right)
    |> List.fold ~f:(fun (x1,y1) (x2,y2) -> (min x1 x2, max y1 y2)) ~init:(s,r)
  | EmptySequence b -> b


let t_list_to_bounds ls =
  match ls with
  | h :: t ->
    List.map ~f:t_to_bounds t
    |> List.fold ~f:(fun (x1,y1) (x2,y2) -> (min x1 x2, max y1 y2)) ~init:(t_to_bounds h)
    |> fun x -> Some x
  | _ -> None

(** converts a zipper to the bounds of the current item *)
let to_bounds (MkLocation (current,_)) = 
  t_to_bounds current

(** updates the bounds of the zipper by a fixed offset *)
let update_bounds ~(diff:int) state =
  let mapper = {Ast_mapper.default_mapper with location = (fun _ { loc_start; loc_end; loc_ghost } ->
      {
        loc_start={loc_start with pos_cnum = (if loc_start.pos_cnum = -1
                                              then -1
                                              else loc_start.pos_cnum + diff)};
        loc_end={loc_end with pos_cnum = (if loc_end.pos_cnum = -1
                                          then -1
                                          else loc_end.pos_cnum + diff)};
        loc_ghost=loc_ghost}
    ) } in
  (* update the bounds of a zipper by a fixed offset *)
  let rec update state =
    let update_region r_s r_r = match r_s,r_r with
        | -1,-1 -> -1,-1
        | -1,r -> -1, r + diff
        | s,-1 -> s + diff,-1
        | s,r -> s + diff, r + diff in
    match state with
    | Signature_item si -> Signature_item (mapper.signature_item mapper si)
    | Structure_item si -> Structure_item (mapper.structure_item mapper si)
    | Sequence (None, l,c,r) ->
      let update_ls = List.map ~f:update in
      Sequence (None, update_ls l, update c, update_ls r)
    | Sequence (Some (r_s,r_r), l,c,r) ->
      let update_ls = List.map ~f:update in
      let r_m,r_r = update_region r_s r_r in
      Sequence (Some (r_m,r_r), update_ls l, update c, update_ls r)
    | EmptySequence (r_s,r_r) ->
      let r_m,r_r = update_region r_s r_r in
      EmptySequence (r_m,r_r)
  in

  update state


let rec unwrap_module_type ?range
    ({ pmty_desc;
       pmty_loc={ loc_start; loc_end; _ (* loc_ghost *) };
       _ (* pmty_loc; pmty_attributes *) }: Parsetree.module_type) : _ option =
  let meta_pos = match range with None -> Some (loc_start.pos_cnum, loc_end.pos_cnum) | v -> v in
  begin match pmty_desc with
    (* | Parsetree.Pmty_ident _ -> (??) *)
    | Parsetree.Pmty_signature (h :: t)  ->
      Some (Sequence (meta_pos, [], Signature_item h, List.map ~f:(fun x -> Signature_item x) t))
    | Parsetree.Pmty_functor (_, o_mt, mt) ->
      begin match Option.bind ~f:unwrap_module_type o_mt with
        | Some v -> unwrap_module_type mt
                    |> Option.map ~f:(fun m -> Sequence (meta_pos,[],v,[m]))
        | None -> unwrap_module_type mt
                  |> Option.map ~f:(fun m -> Sequence (meta_pos, [], m,[]))
      end
    (* | Parsetree.Pmty_with (_, _) -> (??)
     * | Parsetree.Pmty_typeof _ -> (??)
     * | Parsetree.Pmty_extension _ -> (??)
     * | Parsetree.Pmty_alias _ -> (??) *)
    | _ -> None
  end 

let rec unwrap_module_expr ?range
    ({ pmod_desc;
       pmod_loc={ loc_start; loc_end; _ (* loc_ghost *) };
       _ (* pmod_loc; pmod_attributes *) }: Parsetree.module_expr)  =
  let meta_pos = match range with None -> Some (loc_start.pos_cnum, loc_end.pos_cnum) | v -> v in
  match pmod_desc with
  (* | Parsetree.Pmod_ident _ -> (??) *) (* X *)
  | Parsetree.Pmod_structure (m :: mt) ->
    Some (Sequence (meta_pos,[], Structure_item m, List.map ~f:(fun x -> Structure_item x) mt))
  (* struct ... end *)
  | Parsetree.Pmod_functor (_, o_mt, me) ->
    let o_mt = Option.bind ~f:unwrap_module_type o_mt in
    let o_me = unwrap_module_expr me in
    let expr = [o_mt; o_me] |> List.map ~f:(Option.to_list) |> ListLabels.flatten in
    begin match expr with
      | [h;r] -> Some (Sequence (meta_pos, [], h, [r]))
      | [h] -> Some (Sequence (meta_pos, [], h, []))
      | _ -> None
    end
  | Parsetree.Pmod_apply (mexp1, mexp2) ->
    let expr = [mexp1;mexp2]
               |> List.map ~f:unwrap_module_expr
               |> List.map ~f:Option.to_list
               |> ListLabels.flatten in
    begin match expr with
      | [h;r] -> Some (Sequence (meta_pos, [], h, [r]))
      | [h] -> Some (Sequence (meta_pos, [], h, []))
      | _ -> None
    end
  | Parsetree.Pmod_constraint (mexp1, mtyp1) ->
    let (let+) x f = Option.bind ~f x in
    let+ mexp1 = unwrap_module_expr mexp1 in
    let+ mtyp1 = unwrap_module_type mtyp1 in
    Some (Sequence (meta_pos, [], mexp1, [mtyp1]))
  (* | Parsetree.Pmod_unpack _ -> (??) *)
  (* | Parsetree.Pmod_extension _ -> (??) *)
  | _ -> None


let t_descend t =
  let range = t_to_bounds t in
  match t with
  | Signature_item ({ psig_desc; _ } as si) ->
    begin match psig_desc with
      (* | Parsetree.Psig_value _ -> (??) *) (* val x: T *)
      (* | Parsetree.Psig_type (_, _) -> (??) *)  (* type t1 = ... and tn = ... *)
      (* | Parsetree.Psig_typesubst _ -> (??) *) (* type t1 = ... and tn = ... *)
      (* | Parsetree.Psig_typext _ -> (??) *) (* type t1 += ... *)
      (* | Parsetree.Psig_exception _ -> (??) *) (* type exn *)
      | Parsetree.Psig_module {
          (* pmd_name; *)
          pmd_type=pmd_type;
          (* pmd_attributes;
           * pmd_loc *) _ } ->
        unwrap_module_type ~range pmd_type |> Option.value ~default:(Signature_item si)
      | _ -> Signature_item si
      (* | Parsetree.Psig_modsubst _ -> (??) *)
      (* | Parsetree.Psig_recmodule _ -> (??) *)
      (* | Parsetree.Psig_modtype _ -> (??) *)
      (* | Parsetree.Psig_open _ -> (??) *)
      (* | Parsetree.Psig_include _ -> (??) *)
      (* | Parsetree.Psig_class _ -> (??) *)
      (* | Parsetree.Psig_class_type _ -> (??) *)
      (* | Parsetree.Psig_attribute _ -> (??) *)
      (* | Parsetree.Psig_extension (_, _) -> (??)) *)
    end
  | Structure_item ({ pstr_desc; _ } as si) -> begin match pstr_desc with
      (* | Parsetree.Pstr_eval (_, _) -> (??) *) (* E *)
      (* | Parsetree.Pstr_value (_, _) -> (??) *)  (* let P1 = E1 and ... and Pn = EN *)
      (* | Parsetree.Pstr_primitive _ -> (??) *)
      (* | Parsetree.Pstr_type (_, _) -> (??) *)
      (* | Parsetree.Pstr_typext _ -> (??) *)
      (* | Parsetree.Pstr_exception _ -> (??) *)
      | Parsetree.Pstr_module { (* pmb_name; *) pmb_expr; _ (* pmb_attributes; pmb_loc *) } ->
        unwrap_module_expr ~range pmb_expr |> Option.value ~default:(Structure_item si)
      (* | Parsetree.Pstr_recmodule _ -> (??) *)
      (* | Parsetree.Pstr_modtype _ -> (??) *)
      (* | Parsetree.Pstr_open _ -> (??) *)
      (* | Parsetree.Pstr_class _ -> (??) *)
      (* | Parsetree.Pstr_class_type _ -> (??) *)
      (* | Parsetree.Pstr_include _ -> (??) *)
      (* | Parsetree.Pstr_attribute _ -> (??) *)
      (* | Parsetree.Pstr_extension (_, _) -> (??) *)
      | _ -> Structure_item si
    end
  | v -> v


let make_zipper_intf left intf right =
  let left = List.map ~f:(fun x -> Signature_item x) left in
  let right = List.map ~f:(fun x -> Signature_item x) right in
  let intf = Signature_item intf in
  MkLocation (Sequence (None, List.rev left, intf, right), Top)

let make_zipper_impl left impl right =
  let left = List.map ~f:(fun x -> Structure_item x) left in
  let right = List.map ~f:(fun x -> Structure_item x) right in
  let impl = Structure_item impl in
  MkLocation (Sequence (None, List.rev left, impl, right), Top)

let contains_point (a,b) point =
  a <= point && point <= b

let at_start current point =
  match current with
  | Structure_item { pstr_loc = { loc_start; _ }; _ }
  | Signature_item { psig_loc = { loc_start; _ }; _ } ->
    loc_start.pos_cnum = point
  | _ -> false

(** moves the location to the nearest expression enclosing or around it   *)
let rec move_zipper_to_point point = function
  | MkLocation (Sequence (bounds, l,c,r), parent) ->
    let distance (start,ed) = min (abs (start - point)) (abs (ed - point)) in
    (* finds the closest strictly enclosing item *)
    let  find_closest_enclosing ls =
      let rec loop ls acc =
      match ls with
      | h :: t ->
        if contains_point (t_to_bounds h) point
        then Some (acc, h, t)
        else loop t (h :: acc)
      | [] -> None in
      loop ls [] in
    begin match find_closest_enclosing (List.rev l @ c :: r)  with
      (* we found an enclosing expression - go into it *)
      | Some (l,c,r) ->
        move_zipper_to_point point (MkLocation (c, Node {below=l;parent; above=r; bounds;}))
      (* none of the subelements contain the point - find the closest one *)
      | None ->
        let sub_items = (List.rev l @ c :: r)
                        |> List.map ~f:(fun elem -> distance (t_to_bounds elem), elem) in
        let min_item = List.min_elt
            ~compare:(fun (d,_) (d',_) -> Int.compare d d') sub_items in
        match min_item with
        | None -> (* this can never happen - list always has at least 1 element *) assert false
        | Some (min, _) ->
          begin match List.split_while sub_items ~f:(fun (d,_) -> d <> min) with
            | (l,(_, c) :: r) ->
              let l = List.map ~f:snd l |> List.rev in
              let r = List.map ~f:snd r in
              move_zipper_to_point point (MkLocation (c, Node {below=l; parent; above=r; bounds}))
          | _ -> assert false
          end
    end
  | (MkLocation (current,parent) as v) ->
    if  contains_point (t_to_bounds current) point
    then match t_descend current with
      | (Sequence _ as s) ->
        let (MkLocation (current', _) as zipper) =
          move_zipper_to_point point (MkLocation (s,parent)) in
        let selected_distance =
          let (start,ed) = t_to_bounds current' in
          min (abs (start - point)) (abs (ed - point)) in
        let enclosing_distance =
          let (start,ed) = t_to_bounds current in
          min (abs (start - point)) (abs (ed - point)) in
        if enclosing_distance < selected_distance
        then v
        else zipper
      | v -> (MkLocation (v, parent))
    else v



let go_up (MkLocation (current,parent)) =
  match parent with
  | Top -> None
  | Node { below; parent; above; bounds } ->
    let current = Sequence (bounds, below,current,above) in
    Some (MkLocation (current,parent))

let go_down (MkLocation (current,parent)) =
  match t_descend current with
  | Sequence (bounds, left,focused,right) ->
    Some (MkLocation (focused, Node {below=left;parent;above=right; bounds;}))
  | _ -> None

let go_left (MkLocation (current,parent)) =
  match parent with
  | Node { bounds; below=l::left; parent; above } ->
    Some (MkLocation (l, Node {below=left; parent; above=current::above; bounds}))
  | _ -> None

let go_right (MkLocation (current,parent)) =
  match parent with
  | Node { below; parent; above=r::right; bounds } ->
    Some (MkLocation (r, Node {below=current::below; parent; above=right; bounds}))
  | _ -> None

(** deletes the current element of the zipper  *)
let calculate_zipper_delete_bounds (MkLocation (current,_) as loc) =
  let current_bounds =  t_to_bounds current in
  let diff = fst current_bounds - snd current_bounds in
  let update_bounds = update_bounds ~diff in
  let update_meta_bound bounds = 
    match bounds with None -> None | Some (st,ed) -> Some (st, ed + diff)
  in
  (* update parent *)
  let rec update_parent parent = match parent with
    | Top -> Top
    | Node {below;parent;above; bounds} ->
      let above = List.map ~f:update_bounds above in
      let bounds = update_meta_bound bounds in
      let parent = update_parent parent in 
      Node {below; parent; above; bounds} in
  (* returns a zipper with the first element removed *)
  let rec remove_current  (MkLocation (current,parent)) = 
    match parent with
    | Top -> None
    | Node {below; parent=up; above=r::right; bounds} ->
      let r = update_bounds r in
      let right = List.map ~f:update_bounds right in
      let up = update_parent up in
      let bounds = update_meta_bound bounds in
      Some (MkLocation(r, Node{below;parent=up;above=right; bounds}))
    | Node {below=l::left; parent=up; above=right; bounds} ->
      let right = List.map ~f:update_bounds right in
      let up = update_parent up in
      let bounds = update_meta_bound bounds in
      Some (MkLocation(l, Node{below=left;parent=up;above=right; bounds}))
    | Node {below=[]; parent=up; above=[]; bounds=(Some (st, ed)) } ->
      let up = update_parent up in
       Some (MkLocation (EmptySequence (st,ed + diff), up)) 
    | Node {below=[]; parent=up; above=[]; _} ->
      remove_current (MkLocation (current, up)) in
  remove_current loc |> Option.map ~f:(fun v -> v,current_bounds) 

(** swaps two elements at the same level, returning the new location  *)
let calculate_swap_bounds (MkLocation (current,parent)) =
  match parent with
  | Node { below=l::left; parent; above=r::right; bounds } ->
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
              above=right;
              bounds
            }))))
  | _ -> None

(** swaps two elements forward at the same level, returning the new location  *)
let calculate_swap_forward_bounds (MkLocation (current,parent)) =
  match parent with
  | Node { below=left; parent; above=r::right; bounds } ->
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
            above=right; bounds
          })))
  | _ -> None

(** swaps two elements forward at the same level, returning the new location  *)
let calculate_swap_backwards_bounds (MkLocation (current,parent)) =
  match parent with
  | Node { below=l::left; parent; above=right; bounds } ->
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
            bounds
          })))
  | _ -> None


(** finds the item bounds for the nearest structure/signature (essentially defun) item  *)
let find_nearest_definition_item_bounds point zipper : _ option =
  let zipper = move_zipper_to_point point zipper in
  let rec loop zipper =
    let MkLocation (current,_) = zipper in
    match current with
    | Signature_item { psig_loc = { loc_start; _ }; _ } 
    | Structure_item {  pstr_loc = { loc_start; _ };_ } -> 
      if point = loc_start.pos_cnum
      then (go_left zipper) |> Option.bind ~f:loop
      else Some loc_start.pos_cnum
    | _ ->  (go_up zipper) |> Option.bind ~f:loop
  in
  loop zipper
