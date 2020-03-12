open Core

let rec unwrap_longident (li: Longident.t) = match li with
  | Longident.Lident l -> [l]
  | Longident.Lapply (l1, l2) ->
    unwrap_longident l1 @ unwrap_longident l2
  | Longident.Ldot (li, s) -> unwrap_longident li @ [s]

(** given a list of items, removes all elements  *)
let subtract base removal =
  List.filter base
    ~f:(fun v -> not (List.mem ~equal:String.equal removal v))

let dedup = List.dedup_and_sort ~compare:String.compare

let rec find_variables_exp ({
    pexp_desc; _
  }: Parsetree.expression) : string list   =
  match pexp_desc with
  | Parsetree.Pexp_ident { txt; _ } ->
    unwrap_longident txt
  | Parsetree.Pexp_constant _ -> []
  | Parsetree.Pexp_let (_, vbs, exp) ->
    let (bound_variables, used_variables) =
      List.fold_right ~init:([],[]) ~f:(fun vb (bound,used) ->
          let (new_bound, new_used) = find_variables_vb vb in
          (* remove any previously bound variables from new_used: *)
          let new_used = subtract new_used bound in
          ((bound @ new_bound), (used @ new_used) )
        ) vbs in
    let bound_variables = dedup bound_variables in
    let used_variables_in_bindings = List.dedup_and_sort ~compare:String.compare used_variables in
    let used_variables_in_expr = find_variables_exp exp in 
    let used_variables_in_expr = subtract used_variables_in_expr bound_variables in
    let all_variables =
      used_variables_in_bindings
      @
      used_variables_in_expr
    in
    List.dedup_and_sort ~compare:String.compare all_variables
  | Parsetree.Pexp_function cases ->
    List.concat_map ~f:find_variables_case cases
  | Parsetree.Pexp_fun (lab, eval, ppat, exp) ->
    let bound_variables = match lab with
      | Asttypes.Nolabel -> []
      | Asttypes.Optional string
      | Asttypes.Labelled string -> [string] in
    let bound_variables = bound_variables @ (find_variables_pat ppat) in
    let variables_used_in_default = Option.map ~f:find_variables_exp eval |> Option.value ~default:[] in
    let variables_used_in_exp = find_variables_exp exp in
    let variables_used_in_exp = subtract variables_used_in_exp bound_variables in
    let all_bindings = variables_used_in_default @ variables_used_in_exp in
    dedup all_bindings
  | Parsetree.Pexp_apply (e1, e2) ->
    let expression_variables = find_variables_exp e1 in
    let apply_exprs = List.map ~f:snd e2 in
    let apply_variables = List.concat_map ~f:find_variables_exp apply_exprs in 
    let all_bindings = expression_variables @ apply_variables in
    dedup all_bindings
  | Parsetree.Pexp_try (match_exp, cases)
  | Parsetree.Pexp_match (match_exp, cases) ->
    let variables_in_match_exp = find_variables_exp match_exp in
    let variables_in_cases = List.concat_map ~f:find_variables_case cases in
    let all_bindings = variables_in_match_exp @ variables_in_cases in 
    dedup all_bindings
  | Parsetree.Pexp_array exps
  | Parsetree.Pexp_tuple exps ->
    let all_bindings = List.concat_map ~f:find_variables_exp exps in
    dedup all_bindings
  | Parsetree.Pexp_variant (_, args)
  | Parsetree.Pexp_construct (_, args) ->
    Option.map ~f:find_variables_exp args |> Option.value ~default:[]
  | Parsetree.Pexp_record (fields, base) ->
    let fields = List.map ~f:snd fields in
    let variables_in_fields = List.concat_map ~f:find_variables_exp fields in
    let variables_in_base = Option.map ~f:find_variables_exp base |> Option.value ~default:[] in
    let all_bindings = variables_in_fields @ variables_in_base in 
    dedup all_bindings
  | Parsetree.Pexp_field (ex, _) -> find_variables_exp ex
  | Parsetree.Pexp_setfield (e1, _, e2) ->
    let e1_variables = find_variables_exp e1 in
    let e2_variables = find_variables_exp e2 in
    let all_bindings = e1_variables @ e2_variables in
    dedup all_bindings
  | Parsetree.Pexp_ifthenelse (e1, e2, e3) ->
    let e1_vars = find_variables_exp e1 in 
    let e2_vars = find_variables_exp e2 in 
    let e3_vars = Option.map ~f:find_variables_exp e3 |> Option.value ~default:[] in
   dedup (e1_vars @ e2_vars @ e3_vars)
  | Parsetree.Pexp_while (e1, e2)
  | Parsetree.Pexp_sequence (e1, e2) ->
    let e1_vars = find_variables_exp e1 in 
    let e2_vars = find_variables_exp e2 in
    dedup (e1_vars @ e2_vars)
  | Parsetree.Pexp_for (pat, e1, e2, _, e3) ->
    let e1_vars = find_variables_exp e1 in 
    let e2_vars = find_variables_exp e2 in
    let e3_vars = find_variables_exp e3 in
    let for_index_pattern = find_variables_pat pat in 
    let e3_vars = subtract e3_vars for_index_pattern in
    let vars = e1_vars @ e2_vars @ e3_vars in
    dedup vars
  | Parsetree.Pexp_assert e1
  | Parsetree.Pexp_newtype (_, e1)
  | Parsetree.Pexp_poly (e1, _)
  | Parsetree.Pexp_lazy e1
  | Parsetree.Pexp_setinstvar (_, e1)
  | Parsetree.Pexp_send (e1, _)
  | Parsetree.Pexp_coerce (e1, _, _)
  | Parsetree.Pexp_constraint (e1, _) ->
    find_variables_exp e1
  | Parsetree.Pexp_new _ -> []
  | Parsetree.Pexp_override fields ->
    let fields = List.map ~f:snd fields in
    let vars = List.concat_map ~f:find_variables_exp fields in
    dedup vars
  | Parsetree.Pexp_letop { let_; ands; body } ->
    let (bound_variables, used_variables) =
      List.fold ~init:([],[]) ~f:(fun (bound,used) bop ->
          let (b, u) = find_variables_bop bop in
          let u = subtract u bound in
          let bound = bound @ b in 
          let used = used @ u in
          (bound,used)
        ) (let_ :: ands)
    in 
    let expr_variables = find_variables_exp body in
    let expr_variables = subtract expr_variables bound_variables in
    let all_variables = used_variables @ expr_variables in
    dedup all_variables
  | Parsetree.Pexp_open (_, _) -> (??)
  | Parsetree.Pexp_letmodule (_, _, _) -> (??)
  | Parsetree.Pexp_letexception (_, _) -> (??)
  | Parsetree.Pexp_object _ -> (??)
  | Parsetree.Pexp_pack _ -> (??)
  | Parsetree.Pexp_extension ext -> find_variables_ext ext
  | Parsetree.Pexp_unreachable -> []
and find_variables_od ({ popen_expr; _ }: Parsetree.open_declaration) =
  find_variables_mexp popen_expr
and  find_variables_mexp ({ pmod_desc; _ }: Parsetree.module_expr) =
  match pmod_desc with
  | Parsetree.Pmod_ident _ -> (??)
  | Parsetree.Pmod_structure _ -> (??)
  | Parsetree.Pmod_functor (_, _, _) -> (??)
  | Parsetree.Pmod_apply (_, _) -> (??)
  | Parsetree.Pmod_constraint (_, _) -> (??)
  | Parsetree.Pmod_unpack _ -> (??)
  | Parsetree.Pmod_extension _ -> (??)
and find_variables_ext ((_, pylod): Parsetree.extension) =
  match pylod with
  | Parsetree.PStr _ -> assert false
  | Parsetree.PSig _ -> assert false
  | Parsetree.PTyp _ -> assert false (* TODO: add support for more diverse patterns *)
  | Parsetree.PPat (pat, exp) ->
    let bound_variables = find_variables_pat pat in
    let variables_in_exp = Option.map ~f:find_variables_exp exp |> Option.value ~default:[] in
    let all_bindings = subtract variables_in_exp bound_variables in
    dedup all_bindings
and find_variables_vb ({ pvb_pat; pvb_expr; _ }: Parsetree.value_binding) =
  find_variables_pat pvb_pat, find_variables_exp pvb_expr
and find_variables_bop ({ pbop_pat; pbop_exp; _ }: Parsetree.binding_op) =
  find_variables_pat pbop_pat, find_variables_exp pbop_exp
and find_variables_pat ({ ppat_desc; _ }: Parsetree.pattern) : string list =
  match ppat_desc with
  | Parsetree.Ppat_any -> []
  | Parsetree.Ppat_var { txt; _ } -> [txt]
  | Parsetree.Ppat_alias (pat, { txt; _ }) ->
    find_variables_pat pat @ [txt]
  | Parsetree.Ppat_constant _ -> []
  | Parsetree.Ppat_interval (_, _) -> []
  | Parsetree.Ppat_tuple ls ->
    List.concat_map ls ~f:(find_variables_pat)
  | Parsetree.Ppat_variant (_, popt)
  | Parsetree.Ppat_construct (_, popt) ->
    (Option.map ~f:(find_variables_pat) popt |> Option.value ~default:[])
  | Parsetree.Ppat_array fields ->
    List.concat_map ~f:(find_variables_pat) fields
  | Parsetree.Ppat_record (fields, _) ->
    List.map fields ~f:(fun (_,p) -> p) |> List.concat_map ~f:(find_variables_pat)
  | Parsetree.Ppat_or (p1, p2) ->
    find_variables_pat p1 @ find_variables_pat p2
  | Parsetree.Ppat_open (_, p1)
  | Parsetree.Ppat_exception p1
  | Parsetree.Ppat_lazy p1
  | Parsetree.Ppat_constraint (p1, _) ->
    find_variables_pat p1
  | Parsetree.Ppat_type _ -> []
  | Parsetree.Ppat_unpack { txt; _ } ->
    [txt]
  | Parsetree.Ppat_extension _ -> []
and find_variables_case ({ pc_lhs; pc_guard; pc_rhs }: Parsetree.case) =
  let bound_variables = find_variables_pat pc_lhs in
  let used_variables_in_guard =
    (pc_guard |> Option.map ~f:find_variables_exp |> Option.value ~default:[])  in
  let used_variables_in_expr = find_variables_exp pc_rhs in
  let used_variables_in_expr = subtract used_variables_in_expr bound_variables in
  let all_used = used_variables_in_guard @ used_variables_in_expr in
  dedup all_used


