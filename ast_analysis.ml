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
  | Parsetree.Pmod_functor (_, omt, mexp) ->
    let variables = Option.map ~f:find_variables_mt omt |> Option.value ~default:[] in 
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
          let (b, u) = find_variables_cf bop in
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
  | Parsetree.Pmty_functor (_, omt, mt) ->
    let param_mt = Option.map ~f:find_variables_mt omt |> Option.value ~default:[] in
    let expr_mt = find_variables_mt mt in
    param_mt @ expr_mt
  | Parsetree.Pmty_with (mt, _) -> find_variables_mt mt
  | Parsetree.Pmty_typeof mexp -> find_variables_mexp mexp
  | Parsetree.Pmty_extension ext -> find_variables_ext ext
  | Parsetree.Pmty_alias _ -> []
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


