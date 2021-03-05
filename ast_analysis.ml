open Core

let rec unwrap_longident (li: Longident.t) = match li with
  | Longident.Lident l -> [l]
  | Longident.Lapply (l1, l2) ->
    unwrap_longident l1 @ unwrap_longident l2
  | Longident.Ldot (li, s) -> unwrap_longident li @ [s]

(** given a list of items, removes all elements  *)
let subtract base removal =
  List.filter ~f:(fun v -> not (List.mem ~equal:String.equal removal v))
    base

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
      List.fold_right ~f:(fun (vb: Parsetree.value_binding) ((bound: _ list),used) ->
          let (new_bound, new_used) = find_variables_vb vb in
          (* remove any previously bound variables from new_used: *)
          let new_used = subtract new_used bound in
          ((bound @ new_bound), (used @ new_used) )
        ) vbs ~init:([],[]) in
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
  | Parsetree.Pexp_open (od, expr) ->
    let od_vari = find_variables_od od in
    let expr_vari = find_variables_exp expr in
    let all_variables = od_vari @ expr_vari in 
    dedup all_variables
  | Parsetree.Pexp_letmodule (_, mexpr, expr) ->
    let mexpr_vari = find_variables_mexp mexpr in
    let expr_vari = find_variables_exp expr in 
    let all_variables = mexpr_vari @ expr_vari in 
    dedup all_variables
  | Parsetree.Pexp_letexception (_, expr) -> find_variables_exp expr
  | Parsetree.Pexp_object cs -> find_variables_cs cs
  | Parsetree.Pexp_pack mexpr -> find_variables_mexp mexpr
  | Parsetree.Pexp_extension ext -> find_variables_ext ext
  | Parsetree.Pexp_unreachable -> []
and find_variables_od ({ popen_expr; _ }: Parsetree.open_declaration) =
  find_variables_mexp popen_expr
and  find_variables_mexp ({ pmod_desc; _ }: Parsetree.module_expr) =
  match pmod_desc with
  | Parsetree.Pmod_ident _ -> []
  | Parsetree.Pmod_structure str -> List.concat_map ~f:find_variables_si str
  | Parsetree.Pmod_functor (omt, mexp) ->
     let variables =
       match omt with
       | Parsetree.Unit -> []
       | Parsetree.Named (_, mt) -> find_variables_mt mt in
    let expr = find_variables_mexp mexp in
    let all_variables = variables @ expr in
    dedup all_variables
  | Parsetree.Pmod_apply (mexp, mexp2) ->
    find_variables_mexp mexp @  find_variables_mexp mexp2
  | Parsetree.Pmod_constraint (mexp, mt) ->
    find_variables_mexp mexp @ find_variables_mt mt
  | Parsetree.Pmod_unpack expr -> find_variables_exp expr
  | Parsetree.Pmod_extension ext -> find_variables_ext ext
and find_variables_si ({ pstr_desc; _ }: Parsetree.structure_item) =
  match pstr_desc with
  | Parsetree.Pstr_eval (expr, _) -> find_variables_exp expr
  | Parsetree.Pstr_value (_, vbs) ->
    let (bound, unbound) = List.map  ~f:find_variables_vb vbs
                           |> List.unzip in
    let bound = List.concat bound in 
    let unbound = List.concat unbound in 
    let bindings = subtract unbound bound in
    dedup bindings
  | Parsetree.Pstr_primitive _ -> []
  | Parsetree.Pstr_type (_, _) -> []
  | Parsetree.Pstr_typext _ -> []
  | Parsetree.Pstr_exception _ -> []
  | Parsetree.Pstr_module mb -> find_variables_mb mb
  | Parsetree.Pstr_recmodule mbs ->
    List.concat_map ~f:find_variables_mb mbs
  | Parsetree.Pstr_modtype mt -> find_variables_mtdcl mt
  | Parsetree.Pstr_open od -> find_variables_od od
  | Parsetree.Pstr_class cls ->
    List.concat_map ~f:find_variables_cls cls
  | Parsetree.Pstr_class_type ct -> List.concat_map ~f:find_variables_ctdcl ct
  | Parsetree.Pstr_include id -> find_variables_id id
  | Parsetree.Pstr_attribute _ -> []
  | Parsetree.Pstr_extension (ext, _) -> find_variables_ext ext
and find_variables_id ({ pincl_mod; _ }: Parsetree.include_declaration) =
  find_variables_mexp pincl_mod
and find_variables_mb ({ pmb_expr; _ }: Parsetree.module_binding) =
  find_variables_mexp pmb_expr
and find_variables_cls ({ pci_expr={ pcl_desc; _ }; _ }: Parsetree.class_declaration) =
  match pcl_desc with
  | Parsetree.Pcl_constr (_, _) -> []
  | Parsetree.Pcl_structure cs -> find_variables_cs cs
  | Parsetree.Pcl_fun (lab, default_expr, patter, csexp) ->
    let bound_variables = match lab with
      | Asttypes.Nolabel -> []
      | Asttypes.Optional string
      | Asttypes.Labelled string -> [string] in
    let variables_in_deflt = Option.map ~f:find_variables_exp default_expr
                             |> Option.value ~default:[] in
    let bound_variables = bound_variables @ (find_variables_pat patter) in
    let variables_in_csexp = variables_in_deflt @ find_variables_csexp csexp  in
    let variables = subtract variables_in_csexp bound_variables in 
    dedup variables
  | Parsetree.Pcl_apply (csexp, exprs) ->
    let variables_in_csexp = find_variables_csexp csexp in
    let exprs = List.map ~f:snd exprs |> List.concat_map ~f:find_variables_exp in
    let variables = variables_in_csexp @ exprs in 
    dedup variables
  | Parsetree.Pcl_let (_, vbs, body) ->
    let (bound_variables, used_variables) =
      List.fold ~init:([],[]) ~f:(fun (bound,used) bop ->
          let (b, u) = find_variables_vb bop in
          let u = subtract u bound in
          let bound = bound @ b in 
          let used = used @ u in
          (bound,used)
        ) vbs
    in 
    let expr_variables = find_variables_csexp body in
    let expr_variables = subtract expr_variables bound_variables in
    let all_variables = used_variables @ expr_variables in
    dedup all_variables
  | Parsetree.Pcl_constraint (csexp, ct) ->
    find_variables_csexp csexp @ find_variables_ct ct
  | Parsetree.Pcl_extension ext -> find_variables_ext ext
  | Parsetree.Pcl_open ( _, csexp) ->
    let variables =  find_variables_csexp csexp in
    dedup variables
and find_variables_ctdcl ({    pci_expr; _ }: Parsetree.class_type_declaration) =
  find_variables_ct pci_expr
and find_variables_cs { pcstr_self; pcstr_fields } =
    let (bound_variables, used_variables) =
      List.fold ~init:([],[]) ~f:(fun (bound,used) bop ->
          let (u, b) = find_variables_cf bop in
          let u = subtract u bound in
          let bound = bound @ b in 
          let used = used @ u in
          (bound,used)
        ) pcstr_fields
    in 
    let bound_variables = (find_variables_pat pcstr_self) @ bound_variables  in
    let bound_variables = dedup bound_variables in 
    let expr_variables = subtract used_variables bound_variables in
    dedup expr_variables
and find_variables_cf ({ pcf_desc; _ }: Parsetree.class_field) =
  match pcf_desc with
  | Parsetree.Pcf_inherit (_, csexp, name) -> find_variables_csexp csexp,
                                              (name
                                               |> Option.map
                                                 ~f:(fun ({ txt; _ }: string Asttypes.loc) -> txt)
                                               |> Option.to_list)
  | Parsetree.Pcf_val ({ txt; _ }, _, cfk) ->
    find_variables_cfk cfk, [txt]
  | Parsetree.Pcf_method ({ txt; _ }, _, cfk) ->
    find_variables_cfk cfk, [txt]
  | Parsetree.Pcf_constraint _ -> [],[]
  | Parsetree.Pcf_initializer exp -> find_variables_exp exp,[]
  | Parsetree.Pcf_attribute _ -> [], []
  | Parsetree.Pcf_extension ext -> find_variables_ext ext,[]
and find_variables_ct ({ pcty_desc; _ }: Parsetree.class_type) =
  match pcty_desc with
  | Parsetree.Pcty_constr (_, _) -> []
  | Parsetree.Pcty_signature csi -> find_variables_csi csi
  | Parsetree.Pcty_arrow (lab, _, ct) ->
    let bound_variables = match lab with
      | Asttypes.Nolabel -> []
      | Asttypes.Optional string
      | Asttypes.Labelled string -> [string] in
    let ct_variables = find_variables_ct ct in
    let ct_variables = subtract ct_variables bound_variables in 
    dedup ct_variables
  | Parsetree.Pcty_extension ext -> find_variables_ext ext
  | Parsetree.Pcty_open (_, ct) ->
    let all_variables = find_variables_ct ct in
    dedup all_variables
and find_variables_csi ({ pcsig_fields; _ }: Parsetree.class_signature) =
  List.concat_map ~f:find_variables_ctf pcsig_fields
and find_variables_ctf ({ pctf_desc; _ }: Parsetree.class_type_field) =
  match pctf_desc with
  | Parsetree.Pctf_inherit ct -> find_variables_ct ct
  | Parsetree.Pctf_val _ -> []
  | Parsetree.Pctf_method _ -> []
  | Parsetree.Pctf_constraint _ -> []
  | Parsetree.Pctf_attribute _ -> []
  | Parsetree.Pctf_extension ext -> find_variables_ext ext
and find_variables_cfk (cfk: Parsetree.class_field_kind) =
  match cfk with
  | Parsetree.Cfk_virtual _ -> []
  | Parsetree.Cfk_concrete (_, exp) -> find_variables_exp exp
and find_variables_csexp ({ pcl_desc; _ }: Parsetree.class_expr) =
  match pcl_desc with
  | Parsetree.Pcl_constr (_, _) -> []
  | Parsetree.Pcl_structure cs -> find_variables_cs cs
  | Parsetree.Pcl_fun (lab, default_expr, patter, csexp) ->
    let bound_variables = match lab with
      | Asttypes.Nolabel -> []
      | Asttypes.Optional string
      | Asttypes.Labelled string -> [string] in
    let variables_in_deflt = Option.map ~f:find_variables_exp default_expr
                             |> Option.value ~default:[] in
    let bound_variables = bound_variables @ (find_variables_pat patter) in
    let variables_in_csexp = variables_in_deflt @ find_variables_csexp csexp  in
    let variables = subtract variables_in_csexp bound_variables in 
    dedup variables
  | Parsetree.Pcl_apply (csexp, exprs) ->
    let cs_expr_vari = find_variables_csexp csexp in
    let exprs = List.map ~f:snd exprs |> List.concat_map ~f:find_variables_exp in
    let all_variables = cs_expr_vari @ exprs in 
    dedup all_variables
  | Parsetree.Pcl_let (_, vbs, body) ->
    let (bound_variables, used_variables) =
      List.fold ~init:([],[]) ~f:(fun (bound,used) bop ->
          let (b, u) = find_variables_vb bop in
          let u = subtract u bound in
          let bound = bound @ b in 
          let used = used @ u in
          (bound,used)
        ) vbs
    in 
    let expr_variables = find_variables_csexp body in
    let expr_variables = subtract expr_variables bound_variables in
    let all_variables = used_variables @ expr_variables in
    dedup all_variables
  | Parsetree.Pcl_constraint (_, _) -> []
  | Parsetree.Pcl_extension ext -> find_variables_ext ext
  | Parsetree.Pcl_open (_, cexp) ->
    dedup (find_variables_csexp cexp)
and find_variables_mtdcl ({ pmtd_type; _ }: Parsetree.module_type_declaration) =
  Option.map pmtd_type ~f:find_variables_mt |> Option.value ~default:[]
and find_variables_mt ({ pmty_desc; _ }: Parsetree.module_type) =
  match pmty_desc with
  | Parsetree.Pmty_ident _ -> []
  | Parsetree.Pmty_signature _ -> []
  | Parsetree.Pmty_functor (omt, mt) ->
     let param_mt = match omt with
       | Parsetree.Unit -> []
       | Parsetree.Named (_, omt) -> find_variables_mt omt in
    let expr_mt = find_variables_mt mt in
    param_mt @ expr_mt
  | Parsetree.Pmty_with (mt, _) -> find_variables_mt mt
  | Parsetree.Pmty_typeof mexp -> find_variables_mexp mexp
  | Parsetree.Pmty_extension ext -> find_variables_ext ext
  | Parsetree.Pmty_alias _ -> []
and find_variables_ext ((_, pylod): Parsetree.extension) =
  match pylod with
  | Parsetree.PStr str -> List.concat_map ~f:find_variables_si str
  | Parsetree.PSig _ -> []
  | Parsetree.PTyp _ -> []
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
  | Parsetree.Ppat_alias (pat, { txt; _ }) -> (* (Some v) as x *)
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
     txt|> Option.to_list
  | Parsetree.Ppat_extension _ -> []
and find_variables_case ({ pc_lhs; pc_guard; pc_rhs }: Parsetree.case) =
  let bound_variables = find_variables_pat pc_lhs in
  let used_variables_in_guard =
    (pc_guard |> Option.map ~f:find_variables_exp |> Option.value ~default:[])  in
  let used_variables_in_expr = find_variables_exp pc_rhs in
  let used_variables_in_expr = subtract used_variables_in_expr bound_variables in
  let all_used = used_variables_in_guard @ used_variables_in_expr in
  dedup all_used


let rec find_pattern_scopes_exp ({
    pexp_desc; pexp_loc={ loc_start={pos_cnum=s; _}; _ }; _
  }: Parsetree.expression) : (int * int ) list   =
  match pexp_desc with
  | Parsetree.Pexp_ident _ -> []
  | Parsetree.Pexp_constant _ -> []
  | Parsetree.Pexp_let (_, vbs, exp) ->
    let bound_variables  =
      List.fold_right ~init:[] ~f:(fun vb acc ->
          let scopes = find_pattern_scopes_vb vb in
          (* remove any previously bound variables from new_used: *)
          acc @ scopes
        ) vbs in
    let used_variables_in_expr = find_pattern_scopes_exp exp in 
    let all_variables =
      bound_variables @ 
      used_variables_in_expr
    in
    all_variables
  | Parsetree.Pexp_function cases ->
    List.concat_map ~f:find_pattern_scopes_case cases
  | Parsetree.Pexp_fun (lab, eval, ppat, exp) ->
    let bound_variables = match lab with
      | Asttypes.Nolabel -> []
      | Asttypes.Optional string
      | Asttypes.Labelled string -> [(s, s + String.length string)]  in
    let bound_variables = bound_variables @ (find_pattern_scopes_pat ppat) in
    let variables_used_in_default = Option.map ~f:find_pattern_scopes_exp eval |> Option.value ~default:[] in
    let variables_used_in_exp = find_pattern_scopes_exp exp in
    let all_bindings = bound_variables @ variables_used_in_default @ variables_used_in_exp in
    all_bindings
  | Parsetree.Pexp_apply (e1, e2) ->
    let expression_variables = find_pattern_scopes_exp e1 in
    let (pats, apply_exprs) = List.fold ~init:([],[]) ~f:(fun (pats,exps) (labl,expr) ->
        let es =
          let loc = expr.pexp_loc in 
          loc.loc_start.pos_cnum
        in
        let labl = match labl with
          | Asttypes.Nolabel -> []
          | Asttypes.Labelled string 
          | Asttypes.Optional string ->
            [(es-1 - String.length string, es-1)]
        in
        let expr_pats = find_pattern_scopes_exp expr in 
        pats @ labl @ expr_pats, expr :: exps
      ) e2 in
    let apply_variables = List.concat_map ~f:find_pattern_scopes_exp apply_exprs in 
    let all_bindings = pats @ expression_variables @ apply_variables in
    all_bindings
  | Parsetree.Pexp_try (match_exp, cases)
  | Parsetree.Pexp_match (match_exp, cases) ->
    let variables_in_match_exp = find_pattern_scopes_exp match_exp in
    let variables_in_cases = List.concat_map ~f:find_pattern_scopes_case cases in
    let all_bindings = variables_in_match_exp @ variables_in_cases in 
    all_bindings
  | Parsetree.Pexp_array exps
  | Parsetree.Pexp_tuple exps ->
    let all_bindings = List.concat_map ~f:find_pattern_scopes_exp exps in
    all_bindings
  | Parsetree.Pexp_variant (_, args)
  | Parsetree.Pexp_construct (_, args) ->
    Option.map ~f:find_pattern_scopes_exp args |> Option.value ~default:[]
  | Parsetree.Pexp_record (fields, base) ->
    let fields = List.map ~f:snd fields in
    let variables_in_fields = List.concat_map ~f:find_pattern_scopes_exp fields in
    let variables_in_base = Option.map ~f:find_pattern_scopes_exp base |> Option.value ~default:[] in
    let all_bindings = variables_in_fields @ variables_in_base in 
    all_bindings
  | Parsetree.Pexp_field (ex, _) -> find_pattern_scopes_exp ex
  | Parsetree.Pexp_setfield (e1, _, e2) ->
    let e1_variables = find_pattern_scopes_exp e1 in
    let e2_variables = find_pattern_scopes_exp e2 in
    let all_bindings = e1_variables @ e2_variables in
    all_bindings
  | Parsetree.Pexp_ifthenelse (e1, e2, e3) ->
    let e1_vars = find_pattern_scopes_exp e1 in 
    let e2_vars = find_pattern_scopes_exp e2 in 
    let e3_vars = Option.map ~f:find_pattern_scopes_exp e3 |> Option.value ~default:[] in
   (e1_vars @ e2_vars @ e3_vars)
  | Parsetree.Pexp_while (e1, e2)
  | Parsetree.Pexp_sequence (e1, e2) ->
    let e1_vars = find_pattern_scopes_exp e1 in 
    let e2_vars = find_pattern_scopes_exp e2 in
    (e1_vars @ e2_vars)
  | Parsetree.Pexp_for (pat, e1, e2, _, e3) ->
    let e1_vars = find_pattern_scopes_exp e1 in 
    let e2_vars = find_pattern_scopes_exp e2 in
    let e3_vars = find_pattern_scopes_exp e3 in
    let for_index_pattern = find_pattern_scopes_pat pat in 
    let vars = for_index_pattern @ e1_vars @ e2_vars @ e3_vars  in
    vars
  | Parsetree.Pexp_assert e1
  | Parsetree.Pexp_newtype (_, e1)
  | Parsetree.Pexp_poly (e1, _)
  | Parsetree.Pexp_lazy e1
  | Parsetree.Pexp_setinstvar (_, e1)
  | Parsetree.Pexp_send (e1, _)
  | Parsetree.Pexp_coerce (e1, _, _)
  | Parsetree.Pexp_constraint (e1, _) ->
    find_pattern_scopes_exp e1
  | Parsetree.Pexp_new _ -> []
  | Parsetree.Pexp_override fields ->
    let fields = List.map ~f:snd fields in
    let vars = List.concat_map ~f:find_pattern_scopes_exp fields in
    vars
  | Parsetree.Pexp_letop { let_; ands; body } ->
    let (bound_variables, used_variables) =
      List.fold ~init:([],[]) ~f:(fun (bound,used) bop ->
          let (b, u) = find_pattern_scopes_bop bop in
          let bound = bound @ b in 
          let used = used @ u in
          (bound,used)
        ) (let_ :: ands)
    in 
    let expr_variables = find_pattern_scopes_exp body in
    let all_variables = bound_variables @ used_variables @ expr_variables in
    all_variables
  | Parsetree.Pexp_open (od, expr) ->
    let od_vari = find_pattern_scopes_od od in
    let expr_vari = find_pattern_scopes_exp expr in
    let all_variables = od_vari @ expr_vari in 
    all_variables
  | Parsetree.Pexp_letmodule (_, mexpr, expr) ->
    let mexpr_vari = find_pattern_scopes_mexp mexpr in
    let expr_vari = find_pattern_scopes_exp expr in 
    let all_variables = mexpr_vari @ expr_vari in 
    all_variables
  | Parsetree.Pexp_letexception (_, expr) -> find_pattern_scopes_exp expr
  | Parsetree.Pexp_object cs -> find_pattern_scopes_cs cs
  | Parsetree.Pexp_pack mexpr -> find_pattern_scopes_mexp mexpr
  | Parsetree.Pexp_extension ext -> find_pattern_scopes_ext ext
  | Parsetree.Pexp_unreachable -> []
and find_pattern_scopes_pat ({ ppat_loc; _ }: Parsetree.pattern)  =
  [ppat_loc.loc_start.pos_cnum, ppat_loc.loc_end.pos_cnum]
and find_pattern_scopes_od ({ popen_expr; _ }: Parsetree.open_declaration) =
  find_pattern_scopes_mexp popen_expr
and find_pattern_scopes_vb ({ pvb_pat; pvb_expr; _ }: Parsetree.value_binding) =
  find_pattern_scopes_pat pvb_pat @ find_pattern_scopes_exp pvb_expr
and  find_pattern_scopes_mexp ({ pmod_desc; _ }: Parsetree.module_expr) =
  match pmod_desc with
  | Parsetree.Pmod_ident _ -> []
  | Parsetree.Pmod_structure str -> List.concat_map ~f:find_pattern_scopes_si str
  | Parsetree.Pmod_functor (omt, mexp) ->
     let variables = match omt with
       | Parsetree.Unit -> []
       | Parsetree.Named (_, omt) -> find_pattern_scopes_mt omt in
    let expr = find_pattern_scopes_mexp mexp in
    let all_variables = variables @ expr in
    all_variables
  | Parsetree.Pmod_apply (mexp, mexp2) ->
    find_pattern_scopes_mexp mexp @  find_pattern_scopes_mexp mexp2
  | Parsetree.Pmod_constraint (mexp, mt) ->
    find_pattern_scopes_mexp mexp @ find_pattern_scopes_mt mt
  | Parsetree.Pmod_unpack expr -> find_pattern_scopes_exp expr
  | Parsetree.Pmod_extension ext -> find_pattern_scopes_ext ext
and find_pattern_scopes_si ({ pstr_desc; _ }: Parsetree.structure_item) =
  match pstr_desc with
  | Parsetree.Pstr_eval (expr, _) -> find_pattern_scopes_exp expr
  | Parsetree.Pstr_value (_, vbs) ->
    let bound = List.map  ~f:find_pattern_scopes_vb vbs in
    let bound = List.concat bound in
    bound
  | Parsetree.Pstr_primitive _ -> []
  | Parsetree.Pstr_type (_, _) -> []
  | Parsetree.Pstr_typext _ -> []
  | Parsetree.Pstr_exception _ -> []
  | Parsetree.Pstr_module mb -> find_pattern_scopes_mb mb
  | Parsetree.Pstr_recmodule mbs ->
    List.concat_map ~f:find_pattern_scopes_mb mbs
  | Parsetree.Pstr_modtype mt -> find_pattern_scopes_mtdcl mt
  | Parsetree.Pstr_open od -> find_pattern_scopes_od od
  | Parsetree.Pstr_class cls ->
    List.concat_map ~f:find_pattern_scopes_cls cls
  | Parsetree.Pstr_class_type ct -> List.concat_map ~f:find_pattern_scopes_ctdcl ct
  | Parsetree.Pstr_include id -> find_pattern_scopes_id id
  | Parsetree.Pstr_attribute _ -> []
  | Parsetree.Pstr_extension (ext, _) -> find_pattern_scopes_ext ext
and find_pattern_scopes_id ({ pincl_mod; _ }: Parsetree.include_declaration) =
  find_pattern_scopes_mexp pincl_mod
and find_pattern_scopes_mb ({ pmb_expr; _ }: Parsetree.module_binding) =
  find_pattern_scopes_mexp pmb_expr
and find_pattern_scopes_cls ({ pci_expr={ pcl_desc; pcl_loc={loc_start={pos_cnum=s; _}; _ };
                                          _}; _ }: Parsetree.class_declaration) =
  match pcl_desc with
  | Parsetree.Pcl_constr (_, _) -> []
  | Parsetree.Pcl_structure cs -> find_pattern_scopes_cs cs
  | Parsetree.Pcl_fun (lab, eval, ppat, exp) ->
    let bound_variables = match lab with
      | Asttypes.Nolabel -> []
      | Asttypes.Optional string
      | Asttypes.Labelled string -> [(s, s + String.length string)]  in
    let bound_variables = bound_variables @ (find_pattern_scopes_pat ppat) in
    let variables_used_in_default = Option.map ~f:find_pattern_scopes_exp eval |> Option.value ~default:[] in
    let variables_used_in_exp = find_pattern_scopes_csexp exp in
    let all_bindings = bound_variables @ variables_used_in_default @ variables_used_in_exp in
    all_bindings
  | Parsetree.Pcl_apply (csexp, exprs) ->
    let variables_in_csexp = find_pattern_scopes_csexp csexp in
    let exprs = List.map ~f:snd exprs |> List.concat_map ~f:find_pattern_scopes_exp in
    let variables = variables_in_csexp @ exprs in 
    variables
  | Parsetree.Pcl_let (_, vbs, body) ->
    let bound_variables  =
      List.fold_right ~init:[] ~f:(fun vb acc ->
          let scopes = find_pattern_scopes_vb vb in
          (* remove any previously bound variables from new_used: *)
          acc @ scopes
        ) vbs in
    let used_variables_in_expr = find_pattern_scopes_csexp body in 
    let all_variables =
      bound_variables @ 
      used_variables_in_expr
    in
    all_variables
  | Parsetree.Pcl_constraint (csexp, ct) ->
    find_pattern_scopes_csexp csexp @ find_pattern_scopes_ct ct
  | Parsetree.Pcl_extension ext -> find_pattern_scopes_ext ext
  | Parsetree.Pcl_open ( _, csexp) ->
    let variables =  find_pattern_scopes_csexp csexp in
    variables
and find_pattern_scopes_ctdcl ({    pci_expr; _ }: Parsetree.class_type_declaration) =
  find_pattern_scopes_ct pci_expr
and find_pattern_scopes_cs { pcstr_self; pcstr_fields } =
  let bound_variables =
      List.fold ~init:[] ~f:(fun acc bop ->
          let u = find_pattern_scopes_cf bop in
          acc @ u
        ) pcstr_fields
    in 
    let bound_variables = (find_pattern_scopes_pat pcstr_self) @ bound_variables  in
    let expr_variables =  bound_variables in
    expr_variables
and find_pattern_scopes_cf ({ pcf_desc; _ }: Parsetree.class_field) : (int * int) list =
  match pcf_desc with
  | Parsetree.Pcf_inherit (_, csexp, name) -> find_pattern_scopes_csexp csexp @ 
                                              (name
                                               |> Option.map
                                                 ~f:(fun ({ loc={
                                                     loc_start={pos_cnum=s;_};
                                                     loc_end={pos_cnum=e;_}; _
                                                   }; _ }: string Asttypes.loc) -> (s,e))
                                               |> Option.to_list)
  | Parsetree.Pcf_method  ({ loc={loc_start={pos_cnum=s;_}; loc_end={pos_cnum=e;_};_};_}, _, cfk)
  | Parsetree.Pcf_val ({ loc={loc_start={pos_cnum=s;_}; loc_end={pos_cnum=e;_};_};_}, _, cfk) ->
    (s,e) :: find_pattern_scopes_cfk cfk
  | Parsetree.Pcf_constraint _ -> []
  | Parsetree.Pcf_initializer exp -> find_pattern_scopes_exp exp
  | Parsetree.Pcf_attribute _ -> []
  | Parsetree.Pcf_extension ext -> find_pattern_scopes_ext ext
and find_pattern_scopes_ct ({ pcty_desc; pcty_loc={loc_start={pos_cnum=s;_};_};_ }
                            : Parsetree.class_type) =
  match pcty_desc with
  | Parsetree.Pcty_constr (_, _) -> []
  | Parsetree.Pcty_signature csi -> find_pattern_scopes_csi csi
  | Parsetree.Pcty_arrow (lab, _, ct) ->
    let bound_variables = match lab with
      | Asttypes.Nolabel -> []
      | Asttypes.Optional string
      | Asttypes.Labelled string -> [(s, s + String.length string)]  in
    let bound_variables = bound_variables @ (find_pattern_scopes_ct ct) in
    let all_bindings = bound_variables in
    all_bindings
  | Parsetree.Pcty_extension ext -> find_pattern_scopes_ext ext
  | Parsetree.Pcty_open (_, ct) ->
    let all_variables = find_pattern_scopes_ct ct in
    all_variables
and find_pattern_scopes_csi ({ pcsig_fields; _ }: Parsetree.class_signature) =
  List.concat_map ~f:find_pattern_scopes_ctf pcsig_fields
and find_pattern_scopes_ctf ({ pctf_desc; _ }: Parsetree.class_type_field) =
  match pctf_desc with
  | Parsetree.Pctf_inherit ct -> find_pattern_scopes_ct ct
  | Parsetree.Pctf_val _ -> []
  | Parsetree.Pctf_method _ -> []
  | Parsetree.Pctf_constraint _ -> []
  | Parsetree.Pctf_attribute _ -> []
  | Parsetree.Pctf_extension ext -> find_pattern_scopes_ext ext
and find_pattern_scopes_cfk (cfk: Parsetree.class_field_kind) =
  match cfk with
  | Parsetree.Cfk_virtual _ -> []
  | Parsetree.Cfk_concrete (_, exp) -> find_pattern_scopes_exp exp
and find_pattern_scopes_csexp ({ pcl_desc; pcl_loc={loc_start={pos_cnum=s;_};_}; _ }: Parsetree.class_expr) =
  match pcl_desc with
  | Parsetree.Pcl_constr (_, _) -> []
  | Parsetree.Pcl_structure cs -> find_pattern_scopes_cs cs
  | Parsetree.Pcl_fun (lab, eval, ppat, exp) ->
    let bound_variables = match lab with
      | Asttypes.Nolabel -> []
      | Asttypes.Optional string
      | Asttypes.Labelled string -> [(s, s + String.length string)]  in
    let bound_variables = bound_variables @ (find_pattern_scopes_pat ppat) in
    let variables_used_in_default = Option.map ~f:find_pattern_scopes_exp eval |> Option.value ~default:[] in
    let variables_used_in_exp = find_pattern_scopes_csexp exp in
    let all_bindings = bound_variables @ variables_used_in_default @ variables_used_in_exp in
    all_bindings
  | Parsetree.Pcl_apply (csexp, exprs) ->
    let variables_in_csexp = find_pattern_scopes_csexp csexp in
    let exprs = List.map ~f:snd exprs |> List.concat_map ~f:find_pattern_scopes_exp in
    let variables = variables_in_csexp @ exprs in 
    variables
  | Parsetree.Pcl_let (_, vbs, body) ->
    let bound_variables  =
      List.fold_right ~init:[] ~f:(fun vb acc ->
          let scopes = find_pattern_scopes_vb vb in
          (* remove any previously bound variables from new_used: *)
          acc @ scopes
        ) vbs in
    let used_variables_in_expr = find_pattern_scopes_csexp body in 
    let all_variables =
      bound_variables @ 
      used_variables_in_expr
    in
    all_variables
  | Parsetree.Pcl_constraint (csexp, ct) ->
    find_pattern_scopes_csexp csexp @ find_pattern_scopes_ct ct
  | Parsetree.Pcl_extension ext -> find_pattern_scopes_ext ext
  | Parsetree.Pcl_open ( _, csexp) ->
    let variables =  find_pattern_scopes_csexp csexp in
    variables
and find_pattern_scopes_mtdcl ({ pmtd_type; _ }: Parsetree.module_type_declaration) =
  Option.map pmtd_type ~f:find_pattern_scopes_mt |> Option.value ~default:[]
and find_pattern_scopes_mt ({ pmty_desc; _ }: Parsetree.module_type) =
  match pmty_desc with
  | Parsetree.Pmty_ident _ -> []
  | Parsetree.Pmty_signature _ -> []
  | Parsetree.Pmty_functor (omt, mt) ->
    let param_mt = match omt with
      | Parsetree.Unit -> []
      | Parsetree.Named (_, omt) ->
         find_pattern_scopes_mt omt  in
    let expr_mt = find_pattern_scopes_mt mt in
    param_mt @ expr_mt
  | Parsetree.Pmty_with (mt, _) -> find_pattern_scopes_mt mt
  | Parsetree.Pmty_typeof mexp -> find_pattern_scopes_mexp mexp
  | Parsetree.Pmty_extension ext -> find_pattern_scopes_ext ext
  | Parsetree.Pmty_alias _ -> []
and find_pattern_scopes_ext ((_, pylod): Parsetree.extension) =
  match pylod with
  | Parsetree.PStr st -> List.concat_map ~f:find_pattern_scopes_si st
  | Parsetree.PSig _ -> []
  | Parsetree.PTyp _ -> []
  | Parsetree.PPat (pat, exp) ->
    let bound_variables = find_pattern_scopes_pat pat in
    let variables_in_exp = Option.map ~f:find_pattern_scopes_exp exp |> Option.value ~default:[] in
    let all_bindings = bound_variables @ variables_in_exp in
    all_bindings
and find_pattern_scopes_bop ({ pbop_pat; pbop_exp; _ }: Parsetree.binding_op) =
  find_pattern_scopes_pat pbop_pat, find_pattern_scopes_exp pbop_exp
and find_pattern_scopes_case ({ pc_lhs; pc_guard; pc_rhs }: Parsetree.case) =
  let bound_variables = find_pattern_scopes_pat pc_lhs in
  let used_variables_in_guard =
    (pc_guard |> Option.map ~f:find_pattern_scopes_exp |> Option.value ~default:[])  in
  let used_variables_in_expr = find_pattern_scopes_exp pc_rhs in
  let used_variables_in_expr =  bound_variables @ used_variables_in_expr in
  let all_used = used_variables_in_guard @ used_variables_in_expr in
  all_used

(* scope - variable, start and end *)
type scope = string * int * int

let rec calculate_str_bounds (str: Parsetree.structure_item list) =
  let calc_str_b ({ pstr_loc; _ }: Parsetree.structure_item) =
    pstr_loc.loc_start.pos_cnum, pstr_loc.loc_end.pos_cnum
  in
  let union (s1,e1) (s2,e2) =
    let sn = if s1 < s2 then s1 else s2 in 
    let en = if e1 < e2 then e2 else e1 in
    sn,en
  in
  match str with
  | [] -> None
  | h :: t ->
    let tailb = calculate_str_bounds t in
    let currentb = calc_str_b h in
    Option.map ~f:(fun tailb -> union tailb currentb) tailb

let rec calculate_cstr_bounds (str: Parsetree.class_field list) =
  let calc_str_b ({ pcf_loc=pstr_loc; _ }: Parsetree.class_field) =
    pstr_loc.loc_start.pos_cnum, pstr_loc.loc_end.pos_cnum
  in
  let union (s1,e1) (s2,e2) =
    let sn = if s1 < s2 then s1 else s2 in 
    let en = if e1 < e2 then e2 else e1 in
    sn,en
  in
  match str with
  | [] -> None
  | h :: t ->
    let tailb = calculate_cstr_bounds t in
    let currentb = calc_str_b h in
    Option.map ~f:(fun tailb -> union tailb currentb) tailb

let rec find_fun_scope ({ pexp_desc; pexp_loc; _ }: Parsetree.expression) =
  match pexp_desc with
  | Parsetree.Pexp_fun (_, _, _, body) -> find_fun_scope body
  | _ -> pexp_loc.loc_start.pos_cnum, pexp_loc.loc_end.pos_cnum
  

(* returns a list of scopes -  *)
let rec find_scopes_exp ({
    pexp_desc;  _
  }: Parsetree.expression) : scope list =
  match pexp_desc with
  | Parsetree.Pexp_ident _ -> []
  | Parsetree.Pexp_constant _ -> []
  | Parsetree.Pexp_let (_, vbs, body) ->
    let (bound_variables, sub_scopes) =
      List.fold ~init:([],[]) ~f:(fun (bound,used) bop ->
          let (variables, scopes) = find_scopes_vb bop in
          let bound = bound @ variables in 
          let used = used @ scopes in
          (bound,used)
        ) vbs
    in
    let let_scopes =
      let let_start,let_end =
        let e3 = body.pexp_loc in
        e3.loc_start.pos_cnum, e3.loc_end.pos_cnum
      in
      List.map ~f:(fun v -> (v, let_start, let_end)) bound_variables
    in 
    let body_scopes = find_scopes_exp body in
    let all_scopes = let_scopes @ sub_scopes @ body_scopes in
    all_scopes
  | Parsetree.Pexp_function cases ->
    List.concat_map ~f:find_scopes_case cases
  | Parsetree.Pexp_fun (lab, eval, ppat, exp) ->
    let bound_variables = match lab with
      | Asttypes.Nolabel -> []
      | Asttypes.Optional string
      | Asttypes.Labelled string -> [string] in
    let bound_variables = bound_variables @ (find_variables_pat ppat) in
    let scopes_used_in_default = Option.map ~f:find_scopes_exp eval |> Option.value ~default:[] in
    let scopes_made_by_fun =
      let fun_start,fun_end =
         find_fun_scope exp
      in 
      List.map ~f:(fun v -> (v,fun_start,fun_end)) bound_variables
    in
    let scopes_used_in_fun = find_scopes_exp exp in 
    let all_bindings = scopes_used_in_default @ scopes_made_by_fun @ scopes_used_in_fun in
    all_bindings
  | Parsetree.Pexp_apply (e1, e2) ->
    let expression_variables = find_scopes_exp e1 in
    let apply_exprs = List.map ~f:snd e2 in
    let apply_variables = List.concat_map ~f:find_scopes_exp apply_exprs in 
    let all_bindings = expression_variables @ apply_variables in
    all_bindings
  | Parsetree.Pexp_try (match_exp, cases)
  | Parsetree.Pexp_match (match_exp, cases) ->
    let variables_in_match_exp = find_scopes_exp match_exp in
    let variables_in_cases = List.concat_map ~f:find_scopes_case cases in
    let all_bindings = variables_in_match_exp @ variables_in_cases in 
    all_bindings
  | Parsetree.Pexp_array exps
  | Parsetree.Pexp_tuple exps ->
    let all_bindings = List.concat_map ~f:find_scopes_exp exps in
    all_bindings
  | Parsetree.Pexp_variant (_, args)
  | Parsetree.Pexp_construct (_, args) ->
    Option.map ~f:find_scopes_exp args |> Option.value ~default:[]
  | Parsetree.Pexp_record (fields, base) ->
    let fields = List.map ~f:snd fields in
    let variables_in_fields = List.concat_map ~f:find_scopes_exp fields in
    let variables_in_base = Option.map ~f:find_scopes_exp base |> Option.value ~default:[] in
    let all_bindings = variables_in_fields @ variables_in_base in 
    all_bindings
  | Parsetree.Pexp_field (ex, _) -> find_scopes_exp ex
  | Parsetree.Pexp_setfield (e1, _, e2) ->
    let e1_variables = find_scopes_exp e1 in
    let e2_variables = find_scopes_exp e2 in
    let all_bindings = e1_variables @ e2_variables in
    all_bindings
  | Parsetree.Pexp_ifthenelse (e1, e2, e3) ->
    let e1_vars = find_scopes_exp e1 in 
    let e2_vars = find_scopes_exp e2 in 
    let e3_vars = Option.map ~f:find_scopes_exp e3 |> Option.value ~default:[] in
   (e1_vars @ e2_vars @ e3_vars)
  | Parsetree.Pexp_while (e1, e2)
  | Parsetree.Pexp_sequence (e1, e2) ->
    let e1_vars = find_scopes_exp e1 in 
    let e2_vars = find_scopes_exp e2 in
    (e1_vars @ e2_vars)
  | Parsetree.Pexp_for (pat, e1, e2, _, e3) ->
    let e1_scopes = find_scopes_exp e1 in 
    let e2_scopes = find_scopes_exp e2 in
    let e3_scopes = find_scopes_exp e3 in
    let for_index_vars = find_variables_pat pat in
    let for_index_scopes =
      let for_start,for_end =
        let e3 = e3.pexp_loc in
        e3.loc_start.pos_cnum, e3.loc_end.pos_cnum
      in
      List.map ~f:(fun var -> (var, for_start, for_end)) for_index_vars
    in
    let body_scopes = find_scopes_exp e3 in 
    let all_scopes = for_index_scopes @ e1_scopes @ e2_scopes @ e3_scopes @ body_scopes in
    all_scopes
  | Parsetree.Pexp_assert e1
  | Parsetree.Pexp_newtype (_, e1)
  | Parsetree.Pexp_poly (e1, _)
  | Parsetree.Pexp_lazy e1
  | Parsetree.Pexp_setinstvar (_, e1)
  | Parsetree.Pexp_send (e1, _)
  | Parsetree.Pexp_coerce (e1, _, _)
  | Parsetree.Pexp_constraint (e1, _) ->
    find_scopes_exp e1
  | Parsetree.Pexp_new _ -> []
  | Parsetree.Pexp_override fields ->
    let fields = List.map ~f:snd fields in
    let vars = List.concat_map ~f:find_scopes_exp fields in
    vars
  | Parsetree.Pexp_letop { let_; ands; body } ->
    let (bound_variables, sub_scopes) =
      List.fold ~init:([],[]) ~f:(fun (bound,used) bop ->
          let (variables, scopes) = find_scopes_bop bop in
          let bound = bound @ variables in 
          let used = used @ scopes in
          (bound,used)
        ) (let_ :: ands)
    in
    let let_scopes =
      let let_start,let_end =
        let e3 = body.pexp_loc in
        e3.loc_start.pos_cnum, e3.loc_end.pos_cnum
      in
      List.map ~f:(fun v -> (v, let_start, let_end)) bound_variables
    in 
    let body_scopes = find_scopes_exp body in
    let all_scopes = let_scopes @ sub_scopes @ body_scopes in
    all_scopes
  | Parsetree.Pexp_open (od, expr) ->
    let od_vari = find_scopes_od od in
    let expr_vari = find_scopes_exp expr in
    let all_variables = od_vari @ expr_vari in 
    all_variables
  | Parsetree.Pexp_letmodule (_, mexpr, expr) ->
    let mexpr_vari = find_scopes_mexp mexpr in
    let expr_vari = find_scopes_exp expr in 
    let all_variables = mexpr_vari @ expr_vari in 
    all_variables
  | Parsetree.Pexp_letexception (_, expr) -> find_scopes_exp expr
  | Parsetree.Pexp_object cs -> find_scopes_cs cs
  | Parsetree.Pexp_pack mexpr -> find_scopes_mexp mexpr
  | Parsetree.Pexp_extension ext -> find_scopes_ext ext
  | Parsetree.Pexp_unreachable -> []
and find_scopes_od ({ popen_expr; _ }: Parsetree.open_declaration) =
  find_scopes_mexp popen_expr
and  find_scopes_mexp ({ pmod_desc; _ }: Parsetree.module_expr) =
  match pmod_desc with
  | Parsetree.Pmod_ident _ -> []
  | Parsetree.Pmod_structure str ->
    let rec loop ls acc = match ls with
      | [] -> List.rev acc
      | h :: t ->
        let (bound_variables, sub_scopes) = find_scopes_si h in
        let new_scopes =
          match calculate_str_bounds t with
          | None -> []
          | Some (start_scope,end_scopes) ->
            List.map ~f:(fun v -> (v, start_scope, end_scopes)) bound_variables
        in
        let all_scopes = new_scopes @ sub_scopes in
        let all_scopes = List.rev all_scopes in 
        loop t (all_scopes @ acc)
    in
    loop str []
  | Parsetree.Pmod_functor (omt, mexp) ->
    let variables = match omt with
      | Parsetree.Unit -> []
      | Parsetree.Named (_, omt) -> find_scopes_mt omt in
    let expr = find_scopes_mexp mexp in
    let all_variables = variables @ expr in
    all_variables
  | Parsetree.Pmod_apply (mexp, mexp2) ->
    find_scopes_mexp mexp @  find_scopes_mexp mexp2
  | Parsetree.Pmod_constraint (mexp, mt) ->
    find_scopes_mexp mexp @ find_scopes_mt mt
  | Parsetree.Pmod_unpack expr -> find_scopes_exp expr
  | Parsetree.Pmod_extension ext -> find_scopes_ext ext
and find_scopes_si ({ pstr_desc; _ }: Parsetree.structure_item) =
  match pstr_desc with
  | Parsetree.Pstr_eval (expr, _) -> [], find_scopes_exp expr
  | Parsetree.Pstr_value (_, vbs) ->
    let (bound_variables, sub_scopes) = List.map  ~f:find_scopes_vb vbs
                                        |> List.unzip in
    let bound_variables = List.concat bound_variables in 
    let sub_scopes = List.concat sub_scopes in 
    bound_variables, sub_scopes
  | Parsetree.Pstr_primitive { pval_name={ txt=t; _ }; _ } -> [t],[]
  | Parsetree.Pstr_type (_, _) -> [], []
  | Parsetree.Pstr_typext _ -> [], []
  | Parsetree.Pstr_exception _ -> [], []
  | Parsetree.Pstr_module mb -> [], find_scopes_mb mb
  | Parsetree.Pstr_recmodule mbs ->
    [], List.concat_map ~f:find_scopes_mb mbs
  | Parsetree.Pstr_modtype mt -> [], find_scopes_mtdcl mt
  | Parsetree.Pstr_open od -> [], find_scopes_od od
  | Parsetree.Pstr_class cls ->
    [], List.concat_map ~f:find_scopes_cls cls
  | Parsetree.Pstr_class_type ct -> [], List.concat_map ~f:find_scopes_ctdcl ct
  | Parsetree.Pstr_include id -> [], find_scopes_id id
  | Parsetree.Pstr_attribute _ -> [], []
  | Parsetree.Pstr_extension (ext, _) -> [], find_scopes_ext ext
and find_scopes_id ({ pincl_mod; _ }: Parsetree.include_declaration) =
  find_scopes_mexp pincl_mod
and find_scopes_mb ({ pmb_expr; _ }: Parsetree.module_binding) =
  find_scopes_mexp pmb_expr
and find_scopes_cls ({ pci_expr={ pcl_desc; _ }; _ }: Parsetree.class_declaration) =
  match pcl_desc with
  | Parsetree.Pcl_constr (_, _) -> []
  | Parsetree.Pcl_structure cs -> find_scopes_cs cs
  | Parsetree.Pcl_fun  (lab, eval, ppat, exp) ->
    let bound_variables = match lab with
      | Asttypes.Nolabel -> []
      | Asttypes.Optional string
      | Asttypes.Labelled string -> [string] in
    let bound_variables = bound_variables @ (find_variables_pat ppat) in
    let scopes_used_in_default = Option.map ~f:find_scopes_exp eval |> Option.value ~default:[] in
    let scopes_made_by_fun =
      let e3 = exp.pcl_loc in
      let fun_start,fun_end =
        e3.loc_start.pos_cnum, e3.loc_end.pos_cnum
      in 
      List.map ~f:(fun v -> (v,fun_start,fun_end)) bound_variables
    in
    let scopes_used_in_fun = find_scopes_csexp exp in 
    let all_bindings = scopes_used_in_default @ scopes_made_by_fun @ scopes_used_in_fun in
    all_bindings
  | Parsetree.Pcl_apply (csexp, exprs) ->
    let variables_in_csexp = find_scopes_csexp csexp in
    let exprs = List.map ~f:snd exprs |> List.concat_map ~f:find_scopes_exp in
    let variables = variables_in_csexp @ exprs in 
    variables
  | Parsetree.Pcl_let (_, vbs, body) ->
    let (bound_variables, sub_scopes) =
      List.fold ~init:([],[]) ~f:(fun (bound,used) bop ->
          let (variables, scopes) = find_scopes_vb bop in
          let bound = bound @ variables in 
          let used = used @ scopes in
          (bound,used)
        ) vbs
    in
    let let_scopes =
      let let_start,let_end =
        let e3 = body.pcl_loc in
        e3.loc_start.pos_cnum, e3.loc_end.pos_cnum
      in
      List.map ~f:(fun v -> (v, let_start, let_end)) bound_variables
    in 
    let body_scopes = find_scopes_csexp body in
    let all_scopes = let_scopes @ sub_scopes @ body_scopes in
    all_scopes
  | Parsetree.Pcl_constraint (csexp, ct) ->
    find_scopes_csexp csexp @ find_scopes_ct ct
  | Parsetree.Pcl_extension ext -> find_scopes_ext ext
  | Parsetree.Pcl_open ( _, csexp) ->
    let variables =  find_scopes_csexp csexp in
    variables
and find_scopes_ctdcl ({    pci_expr; _ }: Parsetree.class_type_declaration) =
  find_scopes_ct pci_expr
and find_scopes_cs ({ pcstr_self; pcstr_fields }: Parsetree.class_structure) =
    let rec loop ls acc = match ls with
      | [] -> List.rev acc
      | h :: t ->
        let (bound_variables, sub_scopes) = find_scopes_cf h in
        let new_scopes =
          match calculate_cstr_bounds t with
          | None -> []
          | Some (start_scope,end_scopes) ->
            List.map ~f:(fun v -> (v, start_scope, end_scopes)) bound_variables
        in
        let all_scopes = new_scopes @ sub_scopes in
        let all_scopes = List.rev all_scopes in 
        loop t (all_scopes @ acc)
    in
    let field_scopes = loop pcstr_fields [] in 
    let bound_variables = (find_variables_pat pcstr_self)  in
    let bound_variables = bound_variables in
    let new_scopes =
      match  calculate_cstr_bounds pcstr_fields with
      | None -> []
      | Some (start_b, end_b) ->
        List.map ~f:(fun v -> (v, start_b, end_b)) bound_variables
    in
    let all_scopes = new_scopes @ field_scopes in
    all_scopes
and find_scopes_cf ({ pcf_desc; _ }: Parsetree.class_field) =
  match pcf_desc with
  | Parsetree.Pcf_inherit (_, csexp, name) -> (name
                                               |> Option.map
                                                 ~f:(fun ({ txt; _ }: string Asttypes.loc) -> txt)
                                               |> Option.to_list), find_scopes_csexp csexp
  | Parsetree.Pcf_val ({ txt; _ }, _, cfk) ->
    [txt], find_scopes_cfk cfk
  | Parsetree.Pcf_method ({ txt; _ }, _, cfk) ->
    [txt], find_scopes_cfk cfk
  | Parsetree.Pcf_constraint _ -> [],[]
  | Parsetree.Pcf_initializer exp -> [], find_scopes_exp exp
  | Parsetree.Pcf_attribute _ -> [], []
  | Parsetree.Pcf_extension ext -> [], find_scopes_ext ext
and find_scopes_ct ({ pcty_desc; _ }: Parsetree.class_type) =
  match pcty_desc with
  | Parsetree.Pcty_constr (_, _) -> []
  | Parsetree.Pcty_signature csi -> find_scopes_csi csi
  | Parsetree.Pcty_arrow (lab, _, ct) ->
    let bound_variables = match lab with
      | Asttypes.Nolabel -> []
      | Asttypes.Optional string
      | Asttypes.Labelled string -> [string] in
    let ct_scopes = find_scopes_ct ct in
    let new_scopes =
      let start_b,end_b =
        let e3 = ct.pcty_loc in
        e3.loc_start.pos_cnum, e3.loc_end.pos_cnum
      in
      List.map ~f:(fun v -> (v, start_b, end_b)) bound_variables
    in
    let all_scopes = ct_scopes @ new_scopes in
    all_scopes
  | Parsetree.Pcty_extension ext -> find_scopes_ext ext
  | Parsetree.Pcty_open (_, ct) ->
    let all_variables = find_scopes_ct ct in
    all_variables
and find_scopes_csi ({ pcsig_fields; _ }: Parsetree.class_signature) =
  List.concat_map ~f:find_scopes_ctf pcsig_fields
and find_scopes_ctf ({ pctf_desc; _ }: Parsetree.class_type_field) =
  match pctf_desc with
  | Parsetree.Pctf_inherit ct -> find_scopes_ct ct
  | Parsetree.Pctf_val _ -> []
  | Parsetree.Pctf_method _ -> []
  | Parsetree.Pctf_constraint _ -> []
  | Parsetree.Pctf_attribute _ -> []
  | Parsetree.Pctf_extension ext -> find_scopes_ext ext
and find_scopes_cfk (cfk: Parsetree.class_field_kind) =
  match cfk with
  | Parsetree.Cfk_virtual _ -> []
  | Parsetree.Cfk_concrete (_, exp) -> find_scopes_exp exp
and find_scopes_csexp ({ pcl_desc; _ }: Parsetree.class_expr) =
  match pcl_desc with
  | Parsetree.Pcl_constr (_, _) -> []
  | Parsetree.Pcl_structure cs -> find_scopes_cs cs
  | Parsetree.Pcl_fun  (lab, eval, ppat, exp) ->
    let bound_variables = match lab with
      | Asttypes.Nolabel -> []
      | Asttypes.Optional string
      | Asttypes.Labelled string -> [string] in
    let bound_variables = bound_variables @ (find_variables_pat ppat) in
    let scopes_used_in_default = Option.map ~f:find_scopes_exp eval |> Option.value ~default:[] in
    let scopes_made_by_fun =
      let e3 = exp.pcl_loc in
      let fun_start,fun_end =
        e3.loc_start.pos_cnum, e3.loc_end.pos_cnum
      in 
      List.map ~f:(fun v -> (v,fun_start,fun_end)) bound_variables
    in
    let scopes_used_in_fun = find_scopes_csexp exp in 
    let all_bindings = scopes_used_in_default @ scopes_made_by_fun @ scopes_used_in_fun in
    all_bindings
  | Parsetree.Pcl_apply (csexp, exprs) ->
    let cs_expr_vari = find_scopes_csexp csexp in
    let exprs = List.map ~f:snd exprs |> List.concat_map ~f:find_scopes_exp in
    let all_variables = cs_expr_vari @ exprs in 
    all_variables
  | Parsetree.Pcl_let (_, vbs, body) ->
    let (bound_variables, sub_scopes) =
      List.fold ~init:([],[]) ~f:(fun (bound,used) bop ->
          let (variables, scopes) = find_scopes_vb bop in
          let bound = bound @ variables in 
          let used = used @ scopes in
          (bound,used)
        ) vbs
    in
    let let_scopes =
      let let_start,let_end =
        let e3 = body.pcl_loc in
        e3.loc_start.pos_cnum, e3.loc_end.pos_cnum
      in
      List.map ~f:(fun v -> (v, let_start, let_end)) bound_variables
    in 
    let body_scopes = find_scopes_csexp body in
    let all_scopes = let_scopes @ sub_scopes @ body_scopes in
    all_scopes
  | Parsetree.Pcl_constraint (_, _) -> []
  | Parsetree.Pcl_extension ext -> find_scopes_ext ext
  | Parsetree.Pcl_open (_, cexp) ->
    (find_scopes_csexp cexp)
and find_scopes_mtdcl ({ pmtd_type; _ }: Parsetree.module_type_declaration) =
  Option.map pmtd_type ~f:find_scopes_mt |> Option.value ~default:[]
and find_scopes_mt ({ pmty_desc; _ }: Parsetree.module_type) =
  match pmty_desc with
  | Parsetree.Pmty_ident _ -> []
  | Parsetree.Pmty_signature _ -> []
  | Parsetree.Pmty_functor (omt, mt) ->
    let param_mt = match omt with
      | Parsetree.Unit -> []
      | Parsetree.Named (_, omt) -> find_scopes_mt omt in
    let expr_mt = find_scopes_mt mt in
    param_mt @ expr_mt
  | Parsetree.Pmty_with (mt, _) -> find_scopes_mt mt
  | Parsetree.Pmty_typeof mexp -> find_scopes_mexp mexp
  | Parsetree.Pmty_extension ext -> find_scopes_ext ext
  | Parsetree.Pmty_alias _ -> []
and find_scopes_ext ((_, pylod): Parsetree.extension) =
  match pylod with
  | Parsetree.PStr str ->
    let rec loop ls acc = match ls with
      | [] -> List.rev acc
      | h :: t ->
        let (bound_variables, sub_scopes) = find_scopes_si h in
        let new_scopes =
          match calculate_str_bounds t with
          | None -> []
          | Some (start_scope,end_scopes) ->
            List.map ~f:(fun v -> (v, start_scope, end_scopes)) bound_variables
        in
        let all_scopes = new_scopes @ sub_scopes in
        let all_scopes = List.rev all_scopes in 
        loop t (all_scopes @ acc)
    in
    loop str []
  | Parsetree.PSig _ -> []
  | Parsetree.PTyp _ -> []
  | Parsetree.PPat (pat, exp) ->
    let bound_variables = find_variables_pat pat in
    let scopes_in_exp = Option.map ~f:find_scopes_exp exp |> Option.value ~default:[] in
    let new_scopes =
      match exp with
      | None -> []
      | Some exp ->
        let e3 = exp.pexp_loc in
        let (pos_start,pos_end) = e3.loc_start.pos_cnum, e3.loc_end.pos_cnum in
        List.map ~f:(fun v -> (v, pos_start, pos_end)) bound_variables
    in
    let all_scopes = scopes_in_exp @ new_scopes in
    all_scopes
and find_scopes_vb ({ pvb_pat; pvb_expr; _ }: Parsetree.value_binding) =
  find_variables_pat pvb_pat, find_scopes_exp pvb_expr
and find_scopes_bop ({ pbop_pat; pbop_exp; _ }: Parsetree.binding_op) =
  find_variables_pat pbop_pat, find_scopes_exp pbop_exp
and find_scopes_case ({ pc_lhs; pc_guard; pc_rhs }: Parsetree.case) =
  let bound_variables = find_variables_pat pc_lhs in
  let scopes_in_guard =
    (pc_guard |> Option.map ~f:find_scopes_exp |> Option.value ~default:[])  in
  let case_scopes =
    let case_start, case_end =
      let e3 = pc_rhs.pexp_loc in
      e3.loc_start.pos_cnum, e3.loc_end.pos_cnum
    in 
    List.map ~f:(fun v -> (v, case_start, case_end)) bound_variables
  in
  let body_scopes = find_scopes_exp pc_rhs in 
  scopes_in_guard @ case_scopes @ body_scopes


let find_lub_scope (variables: string list) (scopes: scope list) (startp,endp) =
  let union (s1,e1) (s2,e2) =
    let sz1 = e1 - s1 in
    let sz2 = e2 - s2 in
    if sz1 < sz2 then (s1,e1) else (s2,e2)
  in
  let contains_point (s1,e1)  =
    s1 <= startp && endp <= e1
  in
  let rec loop ls acc =
    match ls with
    | [] -> acc
    | h :: t ->
      let (variable, s,e) = h in
      let acc = 
      if contains_point (s,e) && List.mem ~equal:String.equal variables variable then
        let bound = (s,e) in
        match acc with
        | None -> Some bound
        | Some old_bound -> Some (union bound old_bound)
      else acc
      in
      loop t acc
  in
  loop scopes None

let find_smallest_enclosing_scope  (scopes: scope list) (startp,endp) =
  let smallest (s1,e1) (s2,e2) =
    let sz1 = e1 - s1 in
    let sz2 = e2 - s2 in
    if sz1 < sz2 then (s1,e1) else (s2,e2)
  in
  let contains_point (s1,e1)  =
    s1 <= startp && endp <= e1
  in
  let rec loop ls acc =
    match ls with
    | [] -> acc
    | h :: t ->
      let (_, s,e) = h in
      let acc = 
      if contains_point (s,e) then
        let bound = (s,e) in
        match acc with
        | None -> Some bound
        | Some old_bound -> Some (smallest bound old_bound)
      else acc
      in
      loop t acc
  in
  loop scopes None

(** return list of scopes that should be excluded  *)
let find_excluded_scopes (scopes: scope list) (startp,endp)  =
  let contains_point (_,s1,e1)  =
    startp < s1 && e1 < endp
  in
  List.filter ~f:contains_point scopes
  |> List.map ~f:(fun (_,s1,e1) -> (s1,e1))
  

  
let find_valid_matches (matches: _ list) (patterns: _ list) =
  let contained_by  (s2,e2) (s1,e1)  =
    (s1 <= s2 && e2 <= e1) in
  let pattern_contains bound = List.find patterns ~f:(contained_by bound) |> Option.is_empty in
  List.filter ~f:pattern_contains matches





