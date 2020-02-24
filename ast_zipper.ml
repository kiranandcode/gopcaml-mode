open Core

module TextRegion : sig
  module Diff : sig
    type t
    val of_pair : line:int -> col:int -> t
    val combine : t -> t -> t
    val to_string : t -> string
    val negate : t -> t
    val add_newline_with_indent: indent:int -> t -> t 
    val update_lexing_position : Lexing.position -> t -> Lexing.position 
  end

  type t

  val of_location: Location.t -> t

  val to_bounds : t -> (int * int)

  val to_string : t -> string

  val pp : t -> string

  val shift_region : t -> Diff.t -> t

  val extend_region : t -> Diff.t -> t

  val union : t -> t -> t

  val contains_point : t -> int -> bool

  val equals_point : t -> int -> bool

  val ast_bounds_iterator : unit -> Ast_iterator.iterator * (unit -> t)

  val ast_bounds_mapper : diff:Diff.t -> Ast_mapper.mapper

  val distance : t -> int -> int option

  val distance_line : t -> point:int -> line:int -> (int option * int option)

  val line_start : t -> int

  val column_start : t -> int

  val column_end : t -> int

  val to_diff : t -> Diff.t option

  val swap_diff : t -> t -> (Diff.t * Diff.t) option

  val diff_between : t -> t -> Diff.t option

  val to_shift_from_start: t -> Diff.t

end = struct

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


  let contains_point (({  col=c1; _ },{  col=c2; _ }):t) point =
    match c1,c2 with
    | -1,-1 | -1, _ | _, -1 -> false
    | a, b  -> a <= point && point <= b

  let equals_point (({  col=c1; _ },{  col=c2; _ }):t) point =
    match c1,c2 with
    | -1,-1 | -1, _ | _, -1 -> false
    | a, b  -> a = point || point = b


  let distance (({ col=c1; _ },{ col=c2; _ }):t) point =
    match c1,c2 with
    | -1,-1 | -1, _ | _, -1 -> None
    | start, ed  -> Some (min (abs (start - point)) (abs (ed - point)))

  let distance_line (({ col=c1; line=l1 },{ col=c2; line=l2 }):t) ~point ~line =
    let diff c1 c2 point = match c1,c2 with
      | -1,-1 | -1, _ | _, -1 -> None
      | start, ed  -> Some (min (abs (start - point)) (abs (ed - point)))  in
    let diff_line c1 c2 point = match c1,c2 with
      | -1,-1 | -1, _ | 0,0 | 0,_ | _,0 | _, -1 -> None
      | start, ed  -> Some (min (abs (start - point)) (abs (ed - point)))  in
    let col_diff = diff c1 c2 point in
    let line_diff = diff_line l1 l2 line in
    (col_diff, line_diff)

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

  let diff_between
      ((_, { line=a_l1; col=a_c1; }): t)
      (({ line=b_l1; col=b_c1; }, _): t) =
    let (let+) x f = Option.bind ~f x in
    let unwrap vl = match  vl with -1 -> None | v -> Some v in
    let+ a_l1 = unwrap a_l1 in
    let+ a_c1 = unwrap a_c1 in
    let+ b_l1 = unwrap b_l1 in
    let+ b_c1 = unwrap b_c1 in
    let backwards_shift = (a_l1 - b_l1, a_c1 - b_c1) in
    Some (backwards_shift)


  let to_shift_from_start ((_,{ line=a_l2; col=a_c2; }): t) =
    (a_l2,a_c2)

end


type unwrapped_type =
  | ModuleExpr
  | ModuleTyp
  | LetBinding
  | ValueBinding
  | TypeDeclaration
  | ModuleList
  | ModuleSubst
  | ModuleOpen
  | ValueType
  | TypeDecl
  | Eval
  | Function
  | Apply
  | Match
  | Try
  | Tuple
  | Constructor
  | Variant
  | Record
  | RecordField
  | Field
  | Array
  | IfThenElse
  | Seq
  | While
  | For
  | Constraint
  | Coerce
  | Send
  | New
  | AssignField
  | Override
  | LetModule
[@@deriving show]

type t =
  | Signature_item of Parsetree.signature_item[@opaque]
  | Structure_item of Parsetree.structure_item[@opaque]
  | Value_binding of Parsetree.value_binding[@opaque]
  | Type_declaration of Parsetree.type_declaration[@opaque]
  | Attribute of Parsetree.attribute[@opaque]
  | CoreType of Parsetree.core_type[@opaque]
  | Pattern of Parsetree.pattern[@opaque]
  | Expression of Parsetree.expression[@opaque]
  | Text of TextRegion.t
  | Sequence of ((TextRegion.t * unwrapped_type) option * t list * t * t list)[@opaque]
  | EmptySequence of TextRegion.t * unwrapped_type[@opaque]

let rec to_string = function
  | Signature_item _ -> "Signature_item"
  | Structure_item _ -> "Structure_item"
  | Value_binding _ -> "Value_binding"
  | Type_declaration _ -> "Type_declaration"
  | Attribute  _ -> "Attribute of"
  | CoreType  _ -> "CoreType of"
  | Pattern  _ -> "Pattern of"
  | Text _ -> "Text"
  | Expression _ -> "Expression of"
  | Sequence  (bound, left, current, right) ->
    let bound = match bound with
      | None -> ""
      | Some (_,s) -> show_unwrapped_type s
    in
    let items = List.map ~f:to_string (left @ current :: right)
                |> String.concat ~sep:", " in 
    "Sequence of " ^ bound ^ " (" ^ items ^  ")"
  | EmptySequence (_, ty) -> "EmptySequence of " ^ (show_unwrapped_type ty)


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
  | Text s -> s
  | Signature_item si ->
    let (iter,get) = TextRegion.ast_bounds_iterator () in
    iter.signature_item iter si;
    get ()
  | Structure_item si ->
    let (iter,get) = TextRegion.ast_bounds_iterator () in
    iter.structure_item iter si;
    get ()
  | CoreType ct ->
    let (iter,get) = TextRegion.ast_bounds_iterator () in
    iter.typ iter ct;
    get ()
  | Attribute attr ->
    let (iter,get) = TextRegion.ast_bounds_iterator () in
    iter.attribute iter attr;
    get ()
  | Type_declaration tdcl ->
    let (iter,get) = TextRegion.ast_bounds_iterator () in
    iter.type_declaration iter tdcl;
    get ()
  | Value_binding vb ->
    let (iter,get) = TextRegion.ast_bounds_iterator () in
    iter.value_binding iter vb;
    get ()
  | Pattern pat ->
    let (iter,get) = TextRegion.ast_bounds_iterator () in
    iter.pat iter pat;
    get ()
  | Expression expr ->
    let (iter,get) = TextRegion.ast_bounds_iterator () in
    iter.expr iter expr;
    get ()
  (* if its a sequence, take the union *)
  | Sequence (None, left,elem,right) ->
    List.map ~f:t_to_bounds (left @ right)
    |> List.fold ~f:(fun  a b  -> TextRegion.union a b ) ~init:(t_to_bounds elem)
  | Sequence (Some region, left,elem,right) ->
    List.map ~f:t_to_bounds (left @ elem :: right)
    |> List.fold ~f:TextRegion.union ~init:(fst region)
  | EmptySequence (b,_) -> b

let t_shift_by_offset ~diff t =
  let rec map t =
    let mapper = TextRegion.ast_bounds_mapper ~diff in
    match t with
    | Signature_item si -> Signature_item (mapper.signature_item mapper si)
    | Structure_item si  -> Structure_item (mapper.structure_item mapper si)
    | Attribute si  -> Attribute (mapper.attribute mapper si)
    | CoreType si  -> CoreType (mapper.typ mapper si)
    | Value_binding vb -> Value_binding (mapper.value_binding mapper vb)
    | Type_declaration tdcl -> Type_declaration (mapper.type_declaration mapper tdcl)
    | Pattern pat -> Pattern (mapper.pat mapper pat)
    | Expression expr -> Expression (mapper.expr mapper expr)
    | Text s -> Text (TextRegion.shift_region s diff)
    | Sequence (bounds, left,elem,right) ->
      let left = List.map ~f:map left in
      let right = List.map ~f:map right in
      let elem = map elem in
      let bounds = match bounds with
        | None -> None
        | Some (region,ty) ->
          let region = TextRegion.shift_region region diff in
          Some (region,ty)
      in
      Sequence (bounds, left, elem, right)
    | EmptySequence (region,ty)  ->
      let region = TextRegion.shift_region region diff in
      EmptySequence (region,ty) in
  map t

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
    | Value_binding vb -> Value_binding (mapper.value_binding mapper vb)
    | CoreType si  -> CoreType (mapper.typ mapper si)
    | Attribute si  -> Attribute (mapper.attribute mapper si)
    | Type_declaration tdcl -> Type_declaration (mapper.type_declaration mapper tdcl)
    | Pattern pat -> Pattern (mapper.pat mapper pat)
    | Expression expr -> Expression (mapper.expr mapper expr)
    | Text s -> Text (TextRegion.shift_region s diff)
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

let unwrap_loc ({ loc; _ }: 'a Asttypes.loc) =
  Text (TextRegion.of_location loc)
let rec unwrap_type_declaration ({
    (* ptype_name; *)
    ptype_params;
    ptype_cstrs;
    (* ptype_kind; *)
    (* ptype_private; *)
    ptype_manifest;
    ptype_attributes; _
  } as decl : Parsetree.type_declaration) : _ option =

  let params = List.map ptype_params ~f:(fun (cty, _) -> CoreType cty) in 
  let strs = List.map ptype_cstrs ~f:(fun (c1, c2, loc) ->
      let range = TextRegion.of_location loc in
      let bounds = Some (range, TypeDecl) in
      Sequence (bounds, [], CoreType c1, [CoreType c2])
    ) in
  let o_man = Option.map ~f:(fun v -> CoreType v) ptype_manifest in
  let attributes = List.map ~f:(fun v -> Attribute v) ptype_attributes in
  let items = params @ strs @ (Option.to_list o_man ) @ attributes in
  let bounds =
    let range =
      let iter,get = TextRegion.ast_bounds_iterator () in
      iter.type_declaration iter decl;
      get ()
    in
    Some (range, TypeDecl)
  in 
  match items with
  | h::t -> Some (Sequence (bounds, [], h, t))
  | [] -> None
and unwrap_module_type ?range
    ({ pmty_desc;
       pmty_loc=location;
       pmty_attributes;
       _ (* pmty_loc; pmty_attributes *) }: Parsetree.module_type) : _ option =
  let meta_pos = match range with
      None -> Some (TextRegion.of_location location, ModuleTyp)
    | Some v -> Some (v, ModuleTyp) in
  begin match pmty_desc with
    (* | Parsetree.Pmty_ident _ -> (??) *)
    | Parsetree.Pmty_signature (h :: t)  ->
      let left = List.map ~f:(fun x -> Attribute x) pmty_attributes in 
      Some (Sequence (meta_pos, left, Signature_item h, List.map ~f:(fun x -> Signature_item x) t))
    | Parsetree.Pmty_functor (_, o_mt, mt) ->
      let left = List.map ~f:(fun x -> Attribute x) pmty_attributes in 
      begin match Option.bind ~f:unwrap_module_type o_mt with
        | Some v -> unwrap_module_type mt
                    |> Option.map ~f:(fun m -> Sequence (meta_pos,left,v,[m]))
        | None -> unwrap_module_type mt
                  |> Option.map ~f:(fun m -> Sequence (meta_pos, left, m,[]))
      end
    (* | Parsetree.Pmty_with (_, _) -> (??)
     * | Parsetree.Pmty_typeof _ -> (??)
     * | Parsetree.Pmty_extension _ -> (??)
     * | Parsetree.Pmty_alias _ -> (??) *)
    | _ -> None
  end
and unwrap_module_expr ?range
    ({ pmod_desc;
       pmod_loc=location;
       pmod_attributes;
       _ (* pmod_loc; pmod_attributes *) }: Parsetree.module_expr)  =
  let meta_pos = match range with None -> Some (TextRegion.of_location location,ModuleExpr)
                                | Some v -> Some (v,ModuleExpr) in
  match pmod_desc with
  (* | Parsetree.Pmod_ident _ -> (??) *) (* X *)
  | Parsetree.Pmod_structure (m :: mt) ->
    let left = List.map ~f:(fun x -> Attribute x) pmod_attributes in 
    Some (Sequence (meta_pos,left, Structure_item m, List.map ~f:(fun x -> Structure_item x) mt))
  (* struct ... end *)
  | Parsetree.Pmod_functor (_, o_mt, me) ->
    let o_mt = Option.bind ~f:unwrap_module_type o_mt in
    let o_me = unwrap_module_expr me in
    let expr = [o_mt; o_me] |> List.map ~f:(Option.to_list) |> ListLabels.flatten in
    let left = List.map ~f:(fun x -> Attribute x) pmod_attributes in 
    begin match expr with
      | [h;r] -> Some (Sequence (meta_pos, left, h, [r]))
      | [h] -> Some (Sequence (meta_pos, left, h, []))
      | _ -> None
    end
  | Parsetree.Pmod_apply (mexp1, mexp2) ->
    let left = List.map ~f:(fun x -> Attribute x) pmod_attributes in 
    let expr = [mexp1;mexp2]
               |> List.map ~f:unwrap_module_expr
               |> List.map ~f:Option.to_list
               |> ListLabels.flatten in
    begin match expr with
      | [h;r] -> Some (Sequence (meta_pos, left, h, [r]))
      | [h] -> Some (Sequence (meta_pos, left, h, []))
      | _ -> None
    end
  | Parsetree.Pmod_constraint (mexp1, mtyp1) ->
    let left = List.map ~f:(fun x -> Attribute x) pmod_attributes in 
    let (let+) x f = Option.bind ~f x in
    let+ mexp1 = unwrap_module_expr mexp1 in
    let+ mtyp1 = unwrap_module_type mtyp1 in
    Some (Sequence (meta_pos, left, mexp1, [mtyp1]))
  (* | Parsetree.Pmod_unpack _ -> (??) *)
  (* | Parsetree.Pmod_extension _ -> (??) *)
  | _ -> None
and unwrap_case ?range ({ pc_lhs; pc_guard; pc_rhs }: Parsetree.case) =
  let items =
    let patt = [Pattern pc_lhs] in
    let guard = Option.map ~f:(fun v -> Expression v) pc_guard |> Option.to_list in
    let expr = [Expression pc_rhs] in
    patt @ guard @ expr in
  begin
    match items with
    | h :: [] -> Some h
    | h :: t ->
      let range =
        match range with
        | Some r -> r
        | None ->
          let sequence = Sequence (None, [], h, t) in
          t_to_bounds sequence
      in
      let bounds = Some (range, Function) in 
      Some (Sequence (bounds, [], h, t))
    | [] -> None
  end
and unwrap_expr ?range ({ pexp_desc; pexp_attributes; _ } as expr: Parsetree.expression) =
  let range = match range with
    | Some r -> r
    | None  -> t_to_bounds (Expression expr) in
  let attrs = List.map ~f:(fun a -> Attribute a) pexp_attributes in
  match pexp_desc with
  (* | Parsetree.Pexp_ident _ -> (??) *)
  (* | Parsetree.Pexp_constant _ -> (??) *)
  | Parsetree.Pexp_let (_, vbs, expr) ->
    let vbs = List.map ~f:(fun vb -> Value_binding vb) vbs in
    let expr = Expression expr in
    let bounds = Some (range, LetBinding) in
    begin
      match vbs,attrs with
      | [],[] -> expr
      | v :: vbs, attrs -> Sequence (bounds, [], v, vbs @ expr :: attrs)
      | [], attrs -> Sequence (bounds, [], expr, attrs)
    end 
  | Parsetree.Pexp_function (case :: cases) ->
    let case = unwrap_case ~range case |> Option.to_list in
    let remain = List.filter_map ~f:unwrap_case cases in
    let items = case @ remain in
    let bounds = Some (range, Function) in
    begin
      match items with
      | [] -> Expression expr
      | h :: [] -> h
      | h :: t -> Sequence (bounds, [], h, t)
    end
  | Parsetree.Pexp_fun (_, o_deflt, pat, expr) ->
    let items =
      let dflt = Option.map ~f:(fun e -> Expression e) o_deflt
                 |> Option.to_list in
      let pat = [Pattern pat] in
      let expr = [Expression expr] in
      dflt @ pat @ expr in 
    let bounds = Some (range, Function) in
    begin
      match items with
      | [] -> Expression expr
      | h :: [] -> h
      | h :: t -> Sequence (bounds, [], h , t)
    end
  | Parsetree.Pexp_apply (expr, elems) ->
    let expr = Expression expr in
    let elems = List.map ~f:(fun (_,e) -> Expression e) elems in
    let bounds  = Some (range, Apply) in
    begin
      match elems with
      | [] -> expr
      | h :: t -> Sequence (bounds, [], expr, h :: t)
    end
  | Parsetree.Pexp_match (expr, cases) ->
    let expr = Expression expr in
    let cases = List.filter_map ~f:unwrap_case cases in
    begin
      match cases with
      | [] -> expr
      | h :: t -> Sequence (Some (range, Match), [], expr, h::t)
    end
  | Parsetree.Pexp_try (expr, cases) ->
    let expr = Expression expr in
    let cases = List.filter_map ~f:unwrap_case cases in
    begin
      match cases with
      | [] -> expr
      | h :: t -> Sequence (Some (range, Try), [], expr, h::t)
    end
  | Parsetree.Pexp_tuple (e :: exprs) ->
    let expr = Expression e in
    let exprs = List.map ~f:(fun e -> Expression e) exprs in
    let bounds = Some (range, Tuple) in
    begin
      match exprs with
      | [] -> expr
      | h :: t -> Sequence (bounds, [], expr, h :: t)
    end
  | Parsetree.Pexp_construct (_, Some expr) ->
    let expr = Expression expr in
    let bounds = Some (range, Constructor) in 
    Sequence (bounds, [], expr, [])
  | Parsetree.Pexp_variant (_, Some expr) ->
    let expr = Expression expr in
    let bounds = Some (range, Variant) in 
    Sequence (bounds, [], expr, [])
  | Parsetree.Pexp_record (exp_list, exp) ->
    let exps = List.map ~f:(fun (vl, exp) ->
        let vl = unwrap_loc vl in
        let exp = Expression exp in 
        let range = 
          let sequence = Sequence (None, [], vl, [exp]) in 
          t_to_bounds sequence in
        let bounds = Some (range, RecordField) in
        Sequence (bounds, [], vl, [exp])
      ) exp_list in
    let exp = Option.map ~f:(fun v -> Expression v) exp |> Option.to_list in
    let items = exps @ exp in
    let bounds = Some (range, Record) in     
    begin
      match items with
      | h :: [] -> h
      | h :: t -> Sequence (bounds, [], h, t)
      | [] -> Expression expr
    end
  | Parsetree.Pexp_field (expr, loc) ->
    let expr = Expression expr in
    let loc = unwrap_loc loc in
    let bounds = Some (range, Field) in
    Sequence (bounds, [], expr, [loc])
  | Parsetree.Pexp_setfield (e1, loc, e2) ->
    let e1 = Expression e1 in
    let loc = unwrap_loc loc in
    let e2 = Expression e2 in
    let bounds = Some (range, Field) in
    Sequence (bounds, [], e1, [loc; e2])
  | Parsetree.Pexp_array (h :: t) ->
    let expr = Expression h in
    let t = List.map ~f:(fun v -> Expression v) t in
    let bounds = Some (range, Array) in
    Sequence (bounds, [], expr, t)
  | Parsetree.Pexp_ifthenelse (e1, e2, oe3) ->
    let e1 = Expression e1 in
    let e2 = Expression e2 in
    let e3 = Option.map oe3 ~f:(fun e -> Expression e) |> Option.to_list in
    let bounds = Some (range, IfThenElse) in
    Sequence (bounds, [], e1, e2::e3)
  | Parsetree.Pexp_sequence (e1, e2) ->
    let e1 = Expression e1 in
    let e2 = Expression e2 in
    let bounds = Some (range, Seq) in
    Sequence (bounds, [], e1, [e2])
  | Parsetree.Pexp_while (e1, e2) ->
    let e1 = Expression e1 in
    let e2 = Expression e2 in
    let bounds = Some (range, While) in
    Sequence (bounds, [], e1, [e2])
  | Parsetree.Pexp_for (pat, e1, e2, _, e3) ->
    let pat = Pattern pat in
    let e1 = Expression e1 in
    let e2 = Expression e2 in
    let e3 = Expression e3 in
    let bounds = Some (range, For) in
    Sequence (bounds, [], pat, [e1;e2;e3])
  | Parsetree.Pexp_constraint (exp, coretype) ->
    let expr = Expression exp in
    let coretype = CoreType coretype in
    let bounds = Some (range, Constraint) in
    Sequence (bounds, [], expr, [coretype])
  | Parsetree.Pexp_coerce (e1, c1, c2) ->
    let e1 = Expression e1 in
    let c1 = Option.map ~f:(fun v -> CoreType v) c1 |> Option.to_list in
    let c2 =  [CoreType c2] in
    let bounds = Some (range, Coerce) in
    Sequence (bounds, [], e1, c1 @ c2)
  | Parsetree.Pexp_send (e1, loc) ->
    let e1 = Expression e1 in
    let loc = unwrap_loc loc in
    let bounds = Some (range, Send) in
    Sequence (bounds, [], e1, [loc])
  | Parsetree.Pexp_new location ->
    let location = unwrap_loc location in
    let bounds = Some (range, New) in
    Sequence (bounds, [], location, [])
  | Parsetree.Pexp_setinstvar (loc, e1) ->
    let loc = unwrap_loc loc in
    let e1 = Expression e1 in
    let bounds = Some (range, AssignField) in
    Sequence (bounds, [], loc, [e1])
  | Parsetree.Pexp_override ls ->
    let items = List.map ~f:(fun (loc,expr) ->
        let loc = unwrap_loc loc in
        let expr  = Expression expr in
        let range =
          let sequence = Sequence (None, [], loc, [expr]) in
          t_to_bounds sequence in
        let bounds = Some (range, Field) in
        Sequence (bounds, [], loc, [expr])
      ) ls in
    let bounds = Some (range, Override) in
    begin
      match items with
      | h :: t -> Sequence (bounds, [], h, t)
      | [] -> Expression expr
    end
  | Parsetree.Pexp_letmodule (loc, mexpr, expr) ->
    let loc = unwrap_loc loc in 
    let mexpr =  unwrap_module_expr mexpr |> Option.to_list in
    let expr = [Expression expr] in
    let bounds = Some (range, LetModule) in 
    Sequence (bounds, [], loc, mexpr @ expr)
  | _ -> Expression expr
(* | Parsetree.Pexp_letexception (_, _) -> (??) *)
(* | Parsetree.Pexp_assert _ -> (??) *)
(* | Parsetree.Pexp_lazy _ -> (??) *)
(* | Parsetree.Pexp_poly (_, _) -> (??) *)
(* | Parsetree.Pexp_object _ -> (??) *)
(* | Parsetree.Pexp_newtype (_, _) -> (??) *)
(* | Parsetree.Pexp_pack _ -> (??) *)
(* | Parsetree.Pexp_open (_, _) -> (??) *)
(* | Parsetree.Pexp_letop _ -> (??) *)
(* | Parsetree.Pexp_extension _ -> (??) *)
(* | Parsetree.Pexp_unreachable -> (??) *)

and t_descend ?range t =
  let range = Option.value range ~default:(t_to_bounds t) in
  match t with
  | Expression expr -> unwrap_expr ~range expr
  | Value_binding { pvb_pat; pvb_expr; _ } ->
    let left = [] in
    let right = [Expression pvb_expr] in
    let current = Pattern pvb_pat in
    let bounds = Some (range, ValueBinding) in 
    Sequence (bounds, left, current, right)
  | Signature_item ({ psig_desc; _ } as si) ->
    begin match psig_desc with
      | Parsetree.Psig_value { pval_type; pval_attributes; _ } ->
        let current = CoreType pval_type in
        let right = List.map ~f:(fun a -> Attribute a) pval_attributes in
        let bounds = Some (range, ValueType) in
        begin
          match right with
          | h :: t -> Sequence (bounds, [], current, h :: t)
          | [] -> current
        end (* val x: T *)
      | Parsetree.Psig_typesubst fields
      (* type t1 = ... and tn = ... *)
      | Parsetree.Psig_type (_, fields) ->
        begin
          match List.filter_map ~f:unwrap_type_declaration fields with
          | h:: t -> Sequence (Some (range, TypeDecl), [], h, t)
          | [] -> Signature_item si
        end
      (* type t1 = ... and tn = ... *)
      | Parsetree.Psig_module {
          (* pmd_name; *)
          pmd_type=pmd_type;
          pmd_attributes=l::ls;
          (* pmd_attributes;
           * pmd_loc *) _ } ->
        unwrap_module_type  pmd_type
        |> Option.map ~f:(fun t ->
            let elems = List.map ~f:(fun a -> Attribute a) (l :: ls) in
            let bounds = Some (range,ModuleTyp) in
            Sequence (bounds, elems, t, [])
          )
        |> Option.value ~default:(Signature_item si)
      | Parsetree.Psig_module {
          (* pmd_name; *)
          pmd_type=pmd_type;
          pmd_attributes=[];
          (* pmd_attributes;
           * pmd_loc *) _ } ->
        unwrap_module_type ~range  pmd_type
        |> Option.value ~default:(Signature_item si)

      | Parsetree.Psig_modsubst { pms_attributes=l::ls; _ } ->
        let bounds = Some (range, ModuleSubst) in
        let current = Attribute l in
        let right = List.map ~f:(fun v -> Attribute v) ls in
        Sequence (bounds, [], current, right)
      | Parsetree.Psig_recmodule (l :: ls) ->
        let unwrap_decl ({ (* pmd_name; *) pmd_type; pmd_attributes; _ (* pmd_loc *) }:
                           Parsetree.module_declaration) =
          unwrap_module_type  pmd_type
          |> Option.map ~f:(fun t ->
              let elems = List.map ~f:(fun a -> Attribute a) pmd_attributes in
              let range = t_to_bounds t in 
              let bounds = Some (range,ModuleTyp) in
              Sequence (bounds, elems, t, [])
            )
        in
        let right = List.filter_map ~f:unwrap_decl (l :: ls) in
        let bounds = Some (range, ModuleList) in
        begin
          match right with
          | current::right -> Sequence (bounds, [], current, right)
          | [] -> Signature_item si
        end
      | Parsetree.Psig_include { pincl_mod=ty; (* pincl_loc; *) pincl_attributes=attrs; _ } 
      | Parsetree.Psig_modtype {  pmtd_type=Some ty; pmtd_attributes=attrs; _ (* pmtd_loc *) } ->
        let current = unwrap_module_type ty in
        let attributes = List.map ~f:(fun a -> Attribute a) attrs in
        let bounds = Some (range, ModuleList) in
        begin
          match current,attributes with
          | Some current, left -> Sequence (bounds, left, current,[])
          | None, h :: t -> Sequence (bounds, [], h, t)
          | _ -> Signature_item si
        end
      | Parsetree.Psig_open
          { (* popen_expr; popen_override; popen_loc; *) popen_attributes=l::ls; _ } ->
        let current = Attribute l in
        let right = List.map ~f:(fun v -> Attribute v) ls in
        let bounds = Some (range, ModuleOpen) in
        Sequence (bounds, [], current, right)
      | Parsetree.Psig_attribute attr -> Attribute attr
      (* | Parsetree.Psig_typext _ -> (??) *) (* type t1 += ... *)
      (* | Parsetree.Psig_exception _ -> (??) type exn *)
      (* | Parsetree.Psig_class _ -> (??) *)
      (* | Parsetree.Psig_class_type _ -> (??) *)
      | _ -> Signature_item si
    end
  | Structure_item ({ pstr_desc; _ } as si) -> begin match pstr_desc with
      | Parsetree.Pstr_eval (expr, attr) ->
        let current = Expression expr in
        let right = List.map ~f:(fun v -> Attribute v) attr in
        let bounds = Some (range, Eval) in
        Sequence (bounds, [], current, right)
      (* E *)
      | Parsetree.Pstr_value (_, v :: []) ->
        t_descend ~range (Value_binding v)
      | Parsetree.Pstr_value (_, v :: vb) ->
        let right = List.map ~f:(fun vb -> Value_binding vb) vb in
        let left = [] in
        let current = Value_binding v in
        let bounds = Some (range, LetBinding) in
        Sequence (bounds,left,current,right)
      (* let P1 = E1 and ... and Pn = EN *)
      | Parsetree.Pstr_primitive {
          pval_type; pval_attributes; _
        } ->
        let current = CoreType pval_type in
        let right = List.map ~f:(fun v -> Attribute v) pval_attributes in
        let bounds = Some (range, ValueBinding) in
        Sequence (bounds, [], current, right)
      | Parsetree.Pstr_type (_, v :: []) ->
        Ecaml.message "descending on type";
        t_descend ~range (Type_declaration v)
      | Parsetree.Pstr_type (_, t :: ts) ->
        Ecaml.message "descending on multi type";
        let right = List.map ~f:(fun vb -> Type_declaration vb) ts in
        let left = [] in
        let current = Type_declaration t in
        let bounds = Some (range, TypeDeclaration) in
        Sequence (bounds,left,current,right)
      | Parsetree.Pstr_recmodule ({  pmb_expr; pmb_attributes; _  } :: []) 
      | Parsetree.Pstr_module { (* pmb_name; *) pmb_expr; pmb_attributes;
                                              _ (* pmb_attributes; pmb_loc *) } ->
        let expr_range = if List.length pmb_attributes > 0 then None else Some range in 
        unwrap_module_expr ?range:expr_range pmb_expr
        |> Option.map ~f:(fun t ->
            let attrs = List.map ~f:(fun a -> Attribute a) pmb_attributes in
            let bounds = Some (range, ModuleExpr) in 
            if List.length attrs > 0 then Sequence (bounds, attrs, t, []) else t
          )
        |> Option.value ~default:(Structure_item si)
      | Parsetree.Pstr_recmodule ({ pmb_expr; _ } :: vs) ->
        let (let+) x f = Option.bind ~f x in
        begin
          let+ current = unwrap_module_expr ~range pmb_expr in
          let+ right = List.fold ~init:(Some []) vs
              ~f:(fun acc { pmb_expr; _} ->
                  let+ ls = acc in
                  let+ l = unwrap_module_expr pmb_expr in
                  Some (l :: ls)
                ) in
          let right = List.rev right in
          let left = [] in
          let bounds = Some (range, ModuleList) in
          Some (Sequence (bounds, left, current, right))
        end |> Option.value ~default:(Structure_item si)
      | Parsetree.Pstr_modtype { pmtd_type; pmtd_attributes; _ } ->
        let ty = Option.bind ~f:unwrap_module_type pmtd_type
                 |> Option.to_list in
        let attr = List.map ~f:(fun a -> Attribute a) pmtd_attributes in
        let items = ty @ attr in
        let bounds = Some (range, ModuleTyp) in
        begin
          match items with
          | h :: t -> Sequence (bounds, [], h, t)
          | [] -> Structure_item si
        end
      | Parsetree.Pstr_include { pincl_mod=expr; pincl_attributes=attr; _ }
      | Parsetree.Pstr_open { popen_expr=expr; popen_attributes=attr; _ } ->
        let expr = unwrap_module_expr expr |> Option.to_list in
        let attr = List.map ~f:(fun a -> Attribute a) attr in
        let items = expr @ attr in
        let bounds = Some (range, ModuleOpen) in
        begin
          match items with
          | h :: t -> Sequence (bounds, [], h, t)
          | [] -> Structure_item si
        end
      | Parsetree.Pstr_attribute attr -> Attribute attr
      (* | Parsetree.Pstr_typext _ -> (??) *)
      (* | Parsetree.Pstr_exception _ -> (??) *)
      (* | Parsetree.Pstr_class _ -> (??) *)
      (* | Parsetree.Pstr_class_type _ -> (??) *)
      (* | Parsetree.Pstr_extension (_, _) -> (??) *)
      | _ -> Structure_item si
    end
  | v ->
    Ecaml.message (Printf.sprintf "%s %s" "t_descend returning " (to_string v));
    v
and go_down (MkLocation (current,parent)) =
  match t_descend current with
  | Sequence (bounds, left,focused,right) ->
    Some (MkLocation (focused, Node {below=left;parent;above=right; bounds;}))
  | _ -> None

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
    let region_line = TextRegion.line_start region in 
    let region_column = TextRegion.column_start region in 
    let line_pos =
      if not forward then begin
        if (not (region_line = 0) && region_line <= line) ||
           (region_line = 0 &&  region_column <= point)
        then 0
        else 1
      end
      else 0 in
    match TextRegion.distance_line region ~point ~line with
    | None,None -> line_pos,Int.max_value, Int.max_value
    | None, Some line -> (line_pos, Int.max_value, line)  
    | Some col, None -> (line_pos, col, Int.max_value)  
    | Some col, Some line -> (line_pos, col, line)  in
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
    let current_bounds =  (t_to_bounds current) in 
    if TextRegion.equals_point current_bounds point
    then v
    else if TextRegion.contains_point current_bounds point
    then match t_descend current with
      | (Sequence _ as s) ->
        let (MkLocation (current', _) as zipper) =
          move_zipper_to_point point line forward (MkLocation (s,parent)) in
        let selected_distance =
          distance (t_to_bounds current') in
        let enclosing_distance =
          distance (t_to_bounds current) in
        if (trd3 enclosing_distance < trd3 selected_distance) ||
           ((trd3 enclosing_distance = trd3 selected_distance) &&
            (snd3 enclosing_distance < snd3 selected_distance))
        then v
        else zipper
      | v -> (MkLocation (v, parent))
    else v

(** determines whether the item is a toplevel thing that can be freely
    moved around - (internal let in etc. are not supported within the
    zipper.) *)
let is_top_level  = function
  | Structure_item _ 
  | Signature_item _
  | Sequence (Some (_, ModuleTyp), _, _ , _) 
  | Sequence (Some (_, ModuleExpr), _, _ , _) ->
    true
  | _ -> false


(** moves the location to the nearest structure item enclosing or around it   *)
let rec move_zipper_broadly_to_point point line forward =
  let distance region =
    let region_line = TextRegion.line_start region in 
    let region_column = TextRegion.column_start region in 
    let line_pos =
      if not forward then begin
        if (not (region_line = 0) && region_line <= line) ||
           (region_line = 0 &&  region_column <= point)
        then 0
        else 1
      end
      else 0 in
    match TextRegion.distance_line region ~point ~line with
    | None,None -> line_pos,Int.max_value, Int.max_value
    | None, Some line -> (line_pos, Int.max_value, line)  
    | Some col, None -> (line_pos, col, Int.max_value)  
    | Some col, Some line -> (line_pos, col, line)  in
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
    then
      begin
        let descend = t_descend current in
        if not (is_top_level descend) then v else
          match descend with
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

      end
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

let insert_element (MkLocation (current,parent)) (element: t)  =
  let (let+) x f = Option.bind ~f x in
  match parent with
  | Top -> None
  | Node {below; parent; above; bounds} ->
    (* range of the current item *)
    let current_range = t_to_bounds current in
    let insert_range = t_to_bounds element in
    let editing_pos = snd (TextRegion.to_bounds current_range) in
    (* position of the start of the empty structure *)
    let+ shift_backwards =
      TextRegion.diff_between current_range insert_range 
      |> Option.map ~f:(TextRegion.Diff.add_newline_with_indent ~indent:0)
      |> Option.map ~f:(TextRegion.Diff.add_newline_with_indent ~indent:0) in

    let element =
      (* update the structure to be positioned at the right location *)
      t_shift_by_offset ~diff:shift_backwards element in
    (* calculate the diff after inserting the item *)
    let+ diff =
      insert_range
      |> TextRegion.to_diff
      (* we're inserting rather than deleting *)
      |> Option.map ~f:TextRegion.Diff.negate 
      (* newline after end of current element *)
      |> Option.map ~f:(TextRegion.Diff.add_newline_with_indent ~indent:0) 
      (* 1 more newline and then offset *)
      |> Option.map ~f:(TextRegion.Diff.add_newline_with_indent ~indent:0) 
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
    Some (MkLocation (element,parent), editing_pos)


let describe_current_item  (MkLocation (current,_)) =
  Ecaml.message (to_string current)

let go_up (MkLocation (current,parent)) =
  match parent with
  | Top -> None
  | Node { below; parent; above; bounds } ->
    let current = Sequence (bounds, below,current,above) in
    Some (MkLocation (current,parent))


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


let update_zipper_space_bounds (MkLocation (current,parent))
    (pre_column,pre_line) (post_column,post_line) =
  if not (is_top_level current) then None else 
    let pre_diff = TextRegion.Diff.of_pair ~line:pre_line ~col:pre_column
                   |> TextRegion.Diff.negate in
    let post_diff = TextRegion.Diff.of_pair ~line:post_line ~col:post_column
                    |> TextRegion.Diff.negate in
    let current = t_shift_by_offset ~diff:pre_diff current in
    let diff = TextRegion.Diff.combine pre_diff post_diff in
    let update_bounds = update_bounds ~diff in
    let update_meta_bound bounds = 
      match bounds with None -> None
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
    match parent with
    | Top -> None
    | Node {below; parent=up; above=right; bounds} ->
      let right = List.map ~f:update_bounds right in
      let parent = update_parent up in
      let bounds = update_meta_bound bounds in
      Some (MkLocation(current, Node{below;parent;above=right; bounds}))

let move_up (MkLocation (current,parent) as loc)  =
  let (let+) x f = Option.bind ~f x in
  if not (is_top_level current) then None else 
    match parent with
    | Top -> None
    | Node _ ->
      let+ (loc,bounds) = calculate_zipper_delete_bounds loc in
      let+ loc = go_up loc in
      let+ loc = go_left loc in
      let+ (loc,insert_pos) = insert_element loc current in
      Some (loc, insert_pos, TextRegion.to_bounds bounds)

let move_down (MkLocation (current,_) as loc)  =
  if not (is_top_level current) then None else 
    let (let+) x f = Option.bind ~f x in
    let+ (loc,bounds) = calculate_zipper_delete_bounds loc in
    let+ (MkLocation (curr,_) as loc) = go_down loc in
    if not (is_top_level curr) then None else 
      let+ (loc,insert_pos) = insert_element loc current in
      Some (loc, insert_pos, TextRegion.to_bounds bounds)

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
  Ecaml.message "calling find_nearest definition to zipper";
  let zipper = move_zipper_broadly_to_point point line forward zipper in
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
      Ecaml.message (Printf.sprintf "%s %d %d %d %b"
                       "sig_item region found "
                       loc_start.pos_cnum
                       loc_end.pos_cnum
                       point
                       forward
                    );
      get_result loc_start.pos_cnum loc_end.pos_cnum
    | Sequence (Some (bound,_), _,_,_) ->
      let start_column = TextRegion.column_start bound in
      let end_column = TextRegion.column_end bound in
      Ecaml.message (Printf.sprintf "%s %d %d %d %b"
                       "bounded sequence found "
                       start_column
                       end_column
                       point
                       forward
                    );
      get_result start_column end_column
    | Sequence (None, _,_,_) ->
      let bound = (t_to_bounds current) in
      let start_column = (TextRegion.column_start bound) in
      let end_column = TextRegion.column_end bound in
      Ecaml.message (Printf.sprintf "%s %d %d %d %b"
                       "un bounded sequence found "
                       start_column
                       end_column
                       point
                       forward
                    );
      get_result start_column end_column
    | _ ->  (go_up zipper) |> Option.bind ~f:loop
  in
  loop zipper

