open Core

module TextRegion : sig
  module Diff : sig
    type t
    val negate : t -> t
    val add_newline_with_indent: indent:int -> t -> t 
    val update_lexing_position : Lexing.position -> t -> Lexing.position 
  end

  type t

  val of_location: Location.t -> t

  val to_bounds : t -> (int * int)

  val shift_region : t -> Diff.t -> t

  val extend_region : t -> Diff.t -> t

  val union : t -> t -> t

  val contains_point : t -> int -> bool

  val ast_bounds_iterator : unit -> Ast_iterator.iterator * (unit -> t)

  val ast_bounds_mapper : diff:Diff.t -> Ast_mapper.mapper

  val distance : t -> int -> int option

  val distance_line : t -> point:int -> line:int -> (int * int) option

  val line_start : t -> int

  val column_start : t -> int

  val column_end : t -> int

  val to_diff : t -> Diff.t option

  val swap_diff : t -> t -> (Diff.t * Diff.t) option

  val to_shift_from_start: t -> Diff.t

end = struct

  module Diff = struct
    type t = int * int

    let negate (line,col) = (-line,-col)

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

  type t = pos * pos

  let to_bounds Position.({col=cs;_},{col=ce; _}) = (cs,ce)

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
    {Ast_mapper.default_mapper with location = (fun _ ({ loc_start; loc_end; _ } as loc) ->
         {loc with
          loc_start= Diff.update_lexing_position loc_start diff;
          loc_end= Diff.update_lexing_position loc_end diff; }
       ) }


  let contains_point (({  col=c1; _ },{  col=c2; _ }):t) point =
    match c1,c2 with
    | -1,-1 | -1, _ | _, -1 -> false
    | a, b  -> a <= point && point <= b

  let distance (({ col=c1; _ },{ col=c2; _ }):t) point =
    match c1,c2 with
    | -1,-1 | -1, _ | _, -1 -> None
    | start, ed  -> Some (min (abs (start - point)) (abs (ed - point)))

  let distance_line (({ col=c1; line=l1 },{ col=c2; line=l2 }):t) ~point ~line =
    let (let+) x f = Option.bind ~f x in
    let diff c1 c2 point = match c1,c2 with
    | -1,-1 | -1, _ | _, -1 -> None
    | start, ed  -> Some (min (abs (start - point)) (abs (ed - point)))  in
    let+ col_diff = diff c1 c2 point in
    let+ line_diff = diff l1 l2 line in
    Some (col_diff, line_diff)

  let line_start (({ line=l1; _ },_):t) = l1

  let column_start (({ col=c1; _ },_):t) = c1

  let column_end ((_,{ col=c1; _ }):t) = c1

  let to_diff (({ line=l1; col=c1; },{ line=l2; col=c2; }): t) =
    let (let+) x f = Option.bind ~f x in
    let unwrap vl = match  vl with -1 -> None | v -> Some v in
    let+ l1 = unwrap l1 in
    let+ l2 = unwrap l2 in
    let+ c1 = unwrap c1 in
    let+ c2 = unwrap c2 in
    Some (l1 - l2, c1 - c2)

  let swap_diff
      (({ line=a_l1; col=a_c1; },{ line=a_l2; col=a_c2; }): t)
      (({ line=b_l1; col=b_c1; },{ line=b_l2; col=b_c2; }): t) =
    let (let+) x f = Option.bind ~f x in
    let unwrap vl = match  vl with -1 -> None | v -> Some v in
    let+ a_l1 = unwrap a_l1 in
    let+ a_l2 = unwrap a_l2 in
    let+ a_c1 = unwrap a_c1 in
    let+ a_c2 = unwrap a_c2 in
    let+ b_l1 = unwrap b_l1 in
    let+ b_l2 = unwrap b_l2 in
    let+ b_c1 = unwrap b_c1 in
    let+ b_c2 = unwrap b_c2 in
    let forward_shift = (a_l2 - b_l2, a_c2 - b_c2) in
    let backwards_shift = (b_l1 - a_l1, b_c1 - a_c1) in
    Some (forward_shift,backwards_shift)

  let to_shift_from_start ((_,{ line=a_l2; col=a_c2; }): t) =
    (a_l2,a_c2)

end

type unwrapped_type =
  | ModuleExpr
  | ModuleTyp

type t =
  | Signature_item of Parsetree.signature_item
  | Structure_item of Parsetree.structure_item
  | Sequence of (TextRegion.t * unwrapped_type) option * t list * t * t list
  | EmptySequence of TextRegion.t * unwrapped_type

(* Huet's zipper for asts *)
type zipper =
  | Top
  | Node of {
      bounds: (TextRegion.t * unwrapped_type) option;
      below: t list;
      parent: zipper;
      above: t list;
    }

type location =
  | MkLocation of t * zipper

let rec t_to_bounds = function
  | Signature_item si ->
    let (iter,get) = TextRegion.ast_bounds_iterator () in
    iter.signature_item iter si;
    get ()
  | Structure_item si ->
    let (iter,get) = TextRegion.ast_bounds_iterator () in
    iter.structure_item iter si;
    get ()
  (* if its a sequence, take the union *)
  | Sequence (None, left,elem,right) ->
    List.map ~f:t_to_bounds (left @ right)
    |> List.fold ~f:(fun  a b  -> TextRegion.union a b ) ~init:(t_to_bounds elem)
  | Sequence (Some region, left,elem,right) ->
    List.map ~f:t_to_bounds (left @ elem :: right)
    |> List.fold ~f:TextRegion.union ~init:(fst region)
  | EmptySequence (b,_) -> b


let t_list_to_bounds ls =
  match ls with
  | h :: t ->
    List.map ~f:t_to_bounds t
    |> List.fold ~f:TextRegion.union ~init:(t_to_bounds h)
    |> fun x -> Some x
  | _ -> None

(** converts a zipper to the bounds of the current item *)
let to_bounds (MkLocation (current,_)) = 
  TextRegion.to_bounds (t_to_bounds current)

(** updates the bounds of the zipper by a fixed offset *)
let update_bounds ~diff state =
  let mapper = TextRegion.ast_bounds_mapper ~diff in
  (* update the bounds of a zipper by a fixed offset *)
  let rec update state =
    match state with
    | Signature_item si -> Signature_item (mapper.signature_item mapper si)
    | Structure_item si -> Structure_item (mapper.structure_item mapper si)
    | Sequence (None, l,c,r) ->
      let update_ls = List.map ~f:update in
      Sequence (None, update_ls l, update c, update_ls r)
    | Sequence (Some (region, ty), l,c,r) ->
      let update_ls = List.map ~f:update in
      let region = TextRegion.shift_region region diff in
      Sequence (Some (region,ty), update_ls l, update c, update_ls r)
    | EmptySequence (region,ty) ->
      let region = TextRegion.shift_region region diff in
      EmptySequence (region,ty)
  in

  update state


let rec unwrap_module_type ?range
    ({ pmty_desc;
       pmty_loc=location;
       _ (* pmty_loc; pmty_attributes *) }: Parsetree.module_type) : _ option =
  let meta_pos = match range with
      None -> Some (TextRegion.of_location location, ModuleTyp)
    | Some v -> Some (v, ModuleTyp) in
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
       pmod_loc=location;
       _ (* pmod_loc; pmod_attributes *) }: Parsetree.module_expr)  =
  let meta_pos = match range with None -> Some (TextRegion.of_location location,ModuleExpr)
                                | Some v -> Some (v,ModuleExpr) in
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


let at_start current point =
  match current with
  | Structure_item { pstr_loc = { loc_start; _ }; _ }
  | Signature_item { psig_loc = { loc_start; _ }; _ } ->
    loc_start.pos_cnum = point
  | _ -> false

(** moves the location to the nearest expression enclosing or around it   *)
let rec move_zipper_to_point point line forward =
  let distance region =
    let line_pos =
      if not forward then
        if TextRegion.line_start region > line then 1 else 0
      else 0 in
    match TextRegion.distance_line region ~point ~line with
    | None -> line_pos,Int.max_value, Int.max_value
    | Some (col,line) -> (line_pos, line,col)  in
  function
  | MkLocation (Sequence (bounds, l,c,r), parent) ->
    (* finds the closest strictly enclosing item *)
    let  find_closest_enclosing ls =
      let rec loop ls acc =
        match ls with
        | h :: t ->
          if TextRegion.contains_point (t_to_bounds h) point
          then Some (acc, h, t)
          else loop t (h :: acc)
        | [] -> None in
      loop ls [] in
    begin match find_closest_enclosing (List.rev l @ c :: r)  with
      (* we found an enclosing expression - go into it *)
      | Some (l,c,r) ->
        move_zipper_to_point point line forward
          (MkLocation (c, Node {below=l;parent; above=r; bounds;}))
      (* none of the subelements contain the point - find the closest one *)
      | None ->
        let sub_items = (List.rev l @ c :: r)
                        |> List.map ~f:(fun elem -> distance (t_to_bounds elem), elem) in
        let min_item = List.min_elt
            ~compare:(fun (d,_) (d',_) ->
                Tuple3.compare ~cmp1:Int.compare ~cmp2:Int.compare ~cmp3:Int.compare d d'
              ) sub_items in
        match min_item with
        | None -> (* this can never happen - list always has at least 1 element *) assert false
        | Some (min, _) ->
          begin match List.split_while sub_items ~f:(fun (d,_) ->
              not @@ Tuple3.equal ~eq1:Int.equal ~eq2:Int.equal ~eq3:Int.equal d min
            ) with
          | ([],(_, c) :: r) ->
            (* let start_col = TextRegion.column_start (t_to_bounds c) in *)
            let sel_line = TextRegion.line_start (t_to_bounds c) in
            let r = List.map ~f:snd r in
            if sel_line > line && (not forward)
            then (MkLocation (Sequence (bounds, [],c,r), parent))
            else
              move_zipper_to_point point line forward
                (MkLocation (c, Node {below=l; parent; above=r; bounds}))
            | (l,(_, c) :: r) ->
              let l = List.map ~f:snd l |> List.rev in
              let r = List.map ~f:snd r in
              move_zipper_to_point point line forward
                (MkLocation (c, Node {below=l; parent; above=r; bounds}))
            | _ -> assert false
          end
    end
  | (MkLocation (current,parent) as v) ->
    if  TextRegion.contains_point (t_to_bounds current) point
    then match t_descend current with
      | (Sequence _ as s) ->
        let (MkLocation (current', _) as zipper) =
          move_zipper_to_point point line forward (MkLocation (s,parent)) in
        let selected_distance =
          distance (t_to_bounds current') in
        let enclosing_distance =
          distance (t_to_bounds current) in
        if (snd3 enclosing_distance < snd3 selected_distance) ||
           ((trd3 enclosing_distance = trd3 selected_distance) &&
            (trd3 enclosing_distance < trd3 selected_distance)) ||
           (fst3 selected_distance > line)
        then v
        else zipper
      | v -> (MkLocation (v, parent))
    else v

(** moves the location to the nearest structure item enclosing or around it   *)
let rec move_zipper_broadly_to_point point line forward =
  let distance region =
    let line_pos =
      if not forward then
        if TextRegion.line_start region > line then 1 else 0
      else 0 in
    match TextRegion.distance_line region ~point ~line with
    | None -> line_pos,Int.max_value, Int.max_value
    | Some (col,line) -> (line_pos, line,col)  in
  function
  | MkLocation (Sequence (bounds, l,c,r), parent) ->
    (* finds the closest strictly enclosing item *)
    let  find_closest_enclosing ls =
      let rec loop ls acc =
        match ls with
        | h :: t ->
          if TextRegion.contains_point (t_to_bounds h) point
          then Some (acc, h, t)
          else loop t (h :: acc)
        | [] -> None in
      loop ls [] in
    begin match find_closest_enclosing (List.rev l @ c :: r)  with
      (* we found an enclosing expression - go into it *)
      | Some (l,c,r) ->
        move_zipper_broadly_to_point point line forward
          (MkLocation (c, Node {below=l;parent; above=r; bounds;}))
      (* none of the subelements contain the point - find the closest one *)
      | None ->
        let sub_items = (List.rev l @ c :: r)
                        |> List.map ~f:(fun elem -> distance (t_to_bounds elem), elem) in
        let min_item = List.min_elt
            ~compare:(fun (d,_) (d',_) ->
                Tuple3.compare ~cmp1:Int.compare ~cmp2:Int.compare ~cmp3:Int.compare d d'
              ) sub_items in
        match min_item with
        | None -> (* this can never happen - list always has at least 1 element *) assert false
        | Some (min, _) ->
          begin match List.split_while sub_items ~f:(fun (d,_) ->
              not @@ Tuple3.equal ~eq1:Int.equal ~eq2:Int.equal ~eq3:Int.equal d min
            ) with
          | ([],(_, c) :: r) ->
            (* let start_col = TextRegion.column_start (t_to_bounds c) in *)
            let sel_line = TextRegion.line_start (t_to_bounds c) in
            let r = List.map ~f:snd r in
            if sel_line > line && (not forward)
            then (MkLocation (Sequence (bounds, [],c,r), parent))
            else
              move_zipper_broadly_to_point point line forward
                (MkLocation (c, Node {below=l; parent; above=r; bounds}))
            | (l,(_, c) :: r) ->
              let l = List.map ~f:snd l |> List.rev in
              let r = List.map ~f:snd r in
              move_zipper_broadly_to_point point line forward
                (MkLocation (c, Node {below=l; parent; above=r; bounds}))
            | _ -> assert false
          end
    end
  | (MkLocation (current,parent) as v) ->
    if  TextRegion.contains_point (t_to_bounds current) point
    then match t_descend current with
      | (Sequence _ as s) ->
        let (MkLocation (current', _) as zipper) =
          move_zipper_broadly_to_point point line forward (MkLocation (s,parent)) in
        let selected_distance =
          distance (t_to_bounds current') in
        let enclosing_distance =
          distance (t_to_bounds current) in
        if (snd3 enclosing_distance < snd3 selected_distance) ||
           ((trd3 enclosing_distance = trd3 selected_distance) &&
            (trd3 enclosing_distance < trd3 selected_distance)) ||
           (fst3 selected_distance > line)
        then v
        else zipper
      | v -> (MkLocation (v, parent))
    else v

module Synthesis = struct

  (* Returns a structure representing "let _ = (??)" *)
  let empty_let_structure =
    let pure_def = 
      let open Ast_helper in
      let loc_start = Lexing.{
          pos_fname= "file";
          pos_lnum= 0;
          pos_bol= 0;
          pos_cnum= 0;
        } in
      [Str.value
         ~loc:(Location.{
             loc_start;
             loc_end=loc_start;
             loc_ghost=false;
           })
         Asttypes.Nonrecursive
         [
           Vb.mk (Pat.any ())
             (Exp.ident Location.{txt=(Longident.Lident "??");
                                  loc=(!default_loc)} )
         ]
      ] in
    let str = Pprintast.string_of_structure pure_def in
    let sized_def =
      let buf = Lexing.from_string ~with_positions:true str in
      Parse.implementation buf  in
    (List.hd_exn sized_def,str)

  let insert_let_def indent (MkLocation (current,parent))  =
    let (let+) x f = Option.bind ~f x in
    match parent with
    | Top -> None
    | Node {below; parent; above; bounds} ->
      (* range of the current item *)
      let current_range = t_to_bounds current in
      let editing_pos = snd (TextRegion.to_bounds current_range) in
      (* position of the start of the empty structure *)
      let shift_from_start =
        TextRegion.to_shift_from_start current_range
        |> TextRegion.Diff.add_newline_with_indent ~indent:0
        |> TextRegion.Diff.add_newline_with_indent ~indent in

      let empty_let_structure,text =
        (* update the structure to be positioned at the right location *)
        let (st,text) = empty_let_structure in
        let mapper =  TextRegion.ast_bounds_mapper ~diff:shift_from_start  in
        Structure_item (mapper.structure_item mapper st), text in
      (* calculate the diff after inserting the item *)
      let+ diff =
        empty_let_structure
        |> t_to_bounds
        |> TextRegion.to_diff
        (* we're inserting rather than deleting *)
        |> Option.map ~f:TextRegion.Diff.negate 
        (* newline after end of current element *)
        |> Option.map ~f:(TextRegion.Diff.add_newline_with_indent ~indent:0) 
        (* 1 more newline and then offset *)
        |> Option.map ~f:(TextRegion.Diff.add_newline_with_indent ~indent) 
        (* newline after end of inserted element *)
        |> Option.map ~f:(TextRegion.Diff.add_newline_with_indent ~indent:0) in
      let update_bounds = update_bounds ~diff in
      let update_meta_bound bounds = 
        match bounds with
          None -> None
        | Some (bounds,ty) -> Some (TextRegion.extend_region bounds diff,ty)
      in
      (* update parent *)
      let rec update_parent parent = match parent with
        | Top -> Top
        | Node {below;parent;above; bounds} ->
          let above = List.map ~f:update_bounds above in
          let bounds = update_meta_bound bounds in
          let parent = update_parent parent in 
          Node {below; parent; above; bounds} in
      let parent = update_parent parent in
      let above = List.map ~f:update_bounds above in
      let bounds = update_meta_bound bounds in
      let parent = Node {below=current::below; parent; above; bounds} in
      Some (MkLocation (empty_let_structure,parent), (text,editing_pos))


end



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

let go_left (MkLocation (current,parent) as loc) =
  match parent with
  | Node { bounds; below=l::left; parent; above } ->
    Some (MkLocation (l, Node {below=left; parent; above=current::above; bounds}))
  | _ -> go_up loc

let go_right (MkLocation (current,parent) as loc) =
  match parent with
  | Node { below; parent; above=r::right; bounds } ->
    Some (MkLocation (r, Node {below=current::below; parent; above=right; bounds}))
  | _ -> go_up loc

(** deletes the current element of the zipper  *)
let calculate_zipper_delete_bounds (MkLocation (current,_) as loc) =
  let (let+) x f = Option.bind ~f x in
  let current_bounds =  t_to_bounds current in
  let+ diff = TextRegion.to_diff current_bounds (* fst current_bounds - snd current_bounds *) in
  let update_bounds = update_bounds ~diff in
  let update_meta_bound bounds = 
    match bounds with None -> None | Some (bounds,ty) -> Some (TextRegion.extend_region bounds diff,ty)
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
    | Node {below=[]; parent=up; above=[]; bounds=(Some (bounds, ty)) } ->
      let up = update_parent up in
      Some (MkLocation (EmptySequence (TextRegion.extend_region bounds diff,ty), up)) 
    | Node {below=[]; parent=up; above=[]; _} ->
      remove_current (MkLocation (current, up)) in
  remove_current loc |> Option.map ~f:(fun v -> v,current_bounds) 

(** swaps two elements at the same level, returning the new location  *)
let calculate_swap_bounds (MkLocation (current,parent)) =
  match parent with
  | Node { below=l::left; parent; above=r::right; bounds } ->
    let (let+) x f = Option.bind ~f x in
    let current_bounds =  t_to_bounds current in
    let prev_bounds = t_to_bounds l in
    let+ (prev_diff,current_diff) = TextRegion.swap_diff current_bounds prev_bounds in
    (* let prev_diff = snd current_bounds - snd prev_bounds in
     * let current_diff = fst prev_bounds - fst current_bounds in *)
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
    let (let+) x f = Option.bind ~f x in
    let current_bounds =  t_to_bounds current in
    let prev_bounds = t_to_bounds r in
    let+ (current_diff,prev_diff) = TextRegion.swap_diff prev_bounds current_bounds in
    (* let prev_diff = fst current_bounds - fst prev_bounds in
     * let current_diff = snd prev_bounds - snd current_bounds in *)
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
    let (let+) x f = Option.bind ~f x in
    let current_bounds =  t_to_bounds current in
    let prev_bounds = t_to_bounds l in
    let+ (prev_diff,current_diff) = TextRegion.swap_diff current_bounds prev_bounds in
    (* let prev_diff = snd current_bounds - snd prev_bounds in
     * let current_diff = fst prev_bounds - fst current_bounds in *)
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
let find_nearest_definition_item_bounds point line forward zipper : _ option =
  let zipper = move_zipper_to_point point line forward zipper in
  let rec loop zipper =
    let get_result pos_start pos_end =
      if (not forward && point = pos_start)
      then (go_left zipper) |> Option.bind ~f:loop
      else if (forward && point = pos_end)
      then (go_right zipper) |> Option.bind ~f:loop
      else if forward then  Some pos_end else Some pos_start in
    let MkLocation (current,_) = zipper in
    match current with
    | Signature_item { psig_loc = { loc_start; loc_end; _ }; _ } 
    | Structure_item {  pstr_loc = { loc_start; loc_end; _ };_ } ->
      get_result loc_start.pos_cnum loc_end.pos_cnum
    | Sequence (Some (bound,_), _,_,_) ->
      let start_column = TextRegion.column_start bound in
      let end_column = TextRegion.column_end bound in
      get_result start_column end_column
    | Sequence (None, _,_,_) ->
      let bound = (t_to_bounds current) in
      let start_column = (TextRegion.column_start bound) in
      let end_column = TextRegion.column_end bound in
      get_result start_column end_column
    | _ ->  (go_up zipper) |> Option.bind ~f:loop
  in
  loop zipper


