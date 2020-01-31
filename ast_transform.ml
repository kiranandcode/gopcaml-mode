open Core

[@@@ocaml.warning "-33"]

let transform_string x = x
let transform_bool b = b
let transform_list f ls = List.map ~f ls
let transform_char a = a
let transform_option f = Option.map ~f

let transform_lexing_position (value: Lexing.position) : Serializable_ast.Lexing.position =
  match value with
  | {pos_fname; pos_lnum; pos_bol; pos_cnum} ->
    Serializable_ast.{pos_fname; pos_lnum; pos_bol; pos_cnum}

let transform_location_t (value: Location.t) : Serializable_ast.Location.t =
  match value with
  | {loc_start; loc_end; loc_ghost } ->
    Serializable_ast.{loc_start = transform_lexing_position loc_start;
                      loc_end = transform_lexing_position loc_end;
                     loc_ghost}

let transform_location_loc f (value : 'a Location.loc) : 'b Serializable_ast.Location.loc =
  match value with
  | { txt; loc } ->
    Serializable_ast.{txt = f txt; loc = transform_location_t loc}


let transform_constant value = let open Parsetree in match value with
  | Pconst_integer (value_0,value_1) -> Serializable_ast.Pconst_integer (transform_string value_0,
                                                                         transform_option (transform_char) value_1)
| Pconst_char value -> Serializable_ast.Pconst_char (transform_char value)
| Pconst_string (value_0,value_1) -> Serializable_ast.Pconst_string (transform_string value_0,transform_option (transform_string) value_1)
| Pconst_float (value_0,value_1) -> Serializable_ast.Pconst_float (transform_string value_0,transform_option (transform_char) value_1)

let transform_rec_flag value = let open Asttypes in match value with
| Nonrecursive -> Serializable_ast.Nonrecursive
| Recursive -> Serializable_ast.Recursive

let transform_closed_flag value = let open Asttypes in match value with
| Closed -> Serializable_ast.Closed
| Open -> Serializable_ast.Open

let transform_arg_label value = let open Asttypes in match value with
| Nolabel -> Serializable_ast.Nolabel
| Labelled value -> Serializable_ast.Labelled (transform_string value)
| Optional value -> Serializable_ast.Optional (transform_string value)

let transform_direction_flag value = let open Asttypes in match value with
| Upto -> Serializable_ast.Upto
| Downto -> Serializable_ast.Downto

let transform_variance value = let open Asttypes in match value with
| Covariant -> Serializable_ast.Covariant
| Contravariant -> Serializable_ast.Contravariant
| Invariant -> Serializable_ast.Invariant

let transform_private_flag value = let open Asttypes in match value with
| Private -> Serializable_ast.Private
| Public -> Serializable_ast.Public

let transform_mutable_flag value = let open Asttypes in match value with
| Immutable -> Serializable_ast.Immutable
| Mutable -> Serializable_ast.Mutable

let transform_override_flag value = let open Asttypes in match value with
| Override -> Serializable_ast.Override
| Fresh -> Serializable_ast.Fresh

let transform_virtual_flag value = let open Asttypes in match value with
| Virtual -> Serializable_ast.Virtual
| Concrete -> Serializable_ast.Concrete




let rec transform_longident_t (value: Longident.t) : Serializable_ast.Longident.t =
  match value with
  | Lident s -> Serializable_ast.Longident.Lident s
  | Ldot (a,b) -> Serializable_ast.Longident.Ldot (transform_longident_t a,b)
  | Lapply (a,b) -> Serializable_ast.Longident.Lapply (transform_longident_t a,transform_longident_t b)

let rec transform_value_binding value = let open Parsetree in match value with
| {
   pvb_pat;
   pvb_expr;
   pvb_attributes;
   pvb_loc;
} -> Serializable_ast.{
   pvb_pat = transform_pattern pvb_pat;
   pvb_expr = transform_expression pvb_expr;
   pvb_attributes = transform_attributes pvb_attributes;
   pvb_loc = transform_location_t pvb_loc;
}
and transform_attribute value = let open Parsetree in match value with
| {
   attr_name;
   attr_payload;
   attr_loc;
} -> Serializable_ast.{
   attr_name = transform_location_loc (transform_string) attr_name;
   attr_payload = transform_payload attr_payload;
   attr_loc = transform_location_t attr_loc;
}
and transform_row_field_desc value = let open Parsetree in match value with
| Rtag (value_0,value_1,value_2) -> Serializable_ast.Rtag (transform_location_loc (transform_string) value_0,transform_bool value_1,transform_list (transform_core_type) value_2)
| Rinherit value -> Serializable_ast.Rinherit (transform_core_type value)
and transform_row_field value = let open Parsetree in match value with
| {
   prf_desc;
   prf_loc;
   prf_attributes;
} -> Serializable_ast.{
   prf_desc = transform_row_field_desc prf_desc;
   prf_loc = transform_location_t prf_loc;
   prf_attributes = transform_attributes prf_attributes;
}
and transform_object_field_desc value = let open Parsetree in match value with
| Otag (value_0,value_1) -> Serializable_ast.Otag (transform_location_loc (transform_string) value_0,transform_core_type value_1)
| Oinherit value -> Serializable_ast.Oinherit (transform_core_type value)
and transform_object_field value = let open Parsetree in match value with
| {
   pof_desc;
   pof_loc;
   pof_attributes;
} -> Serializable_ast.{
   pof_desc = transform_object_field_desc pof_desc;
   pof_loc = transform_location_t pof_loc;
   pof_attributes = transform_attributes pof_attributes;
}
and transform_package_type value = let open Parsetree in
  match value with
  | (value_0, value_12) ->
    transform_location_loc (transform_longident_t) value_0,
      List.map ~f:(fun (value_1,value_2) ->
       (transform_location_loc (transform_longident_t) value_1, transform_core_type value_2)
      ) value_12
and transform_extension value = let open Parsetree in match value with
| (value_0,value_1) -> (transform_location_loc (transform_string) value_0,transform_payload value_1)
and transform_core_type_desc value = let open Parsetree in match value with
| Ptyp_any -> Serializable_ast.Ptyp_any
| Ptyp_var value -> Serializable_ast.Ptyp_var (transform_string value)
| Ptyp_arrow (value_0,value_1,value_2) -> Serializable_ast.Ptyp_arrow (transform_arg_label value_0,transform_core_type value_1,transform_core_type value_2)
| Ptyp_tuple value -> Serializable_ast.Ptyp_tuple (transform_list transform_core_type  value)
| Ptyp_constr (value_0,value_1) -> Serializable_ast.Ptyp_constr (transform_location_loc
                                                                   (transform_longident_t) value_0,transform_list (transform_core_type) value_1)
| Ptyp_object (value_0,value_1) -> Serializable_ast.Ptyp_object (transform_list (transform_object_field) value_0,transform_closed_flag value_1)
| Ptyp_class (value_0,value_1) -> Serializable_ast.Ptyp_class (transform_location_loc (transform_longident_t) value_0,transform_list (transform_core_type) value_1)
| Ptyp_alias (value_0,value_1) -> Serializable_ast.Ptyp_alias (transform_core_type value_0,transform_string value_1)
| Ptyp_variant (value_0,value_1,value_2) -> Serializable_ast.Ptyp_variant (transform_list (transform_row_field) value_0,transform_closed_flag value_1,transform_option (transform_list (transform_string)) value_2)
| Ptyp_poly (value_0,value_1) -> Serializable_ast.Ptyp_poly (transform_list (transform_location_loc (transform_string)) value_0,transform_core_type value_1)
| Ptyp_package value -> Serializable_ast.Ptyp_package (transform_package_type value)
| Ptyp_extension value -> Serializable_ast.Ptyp_extension (transform_extension value)
and transform_core_type value = let open Parsetree in match value with
| {
   ptyp_desc;
   ptyp_loc;
   ptyp_loc_stack;
   ptyp_attributes;
} -> Serializable_ast.{
   ptyp_desc = transform_core_type_desc ptyp_desc;
   ptyp_loc = transform_location_t ptyp_loc;
   ptyp_loc_stack = transform_list (transform_location_t) ptyp_loc_stack;
   ptyp_attributes = transform_attributes ptyp_attributes;
}

and transform_structure_item value = let open Parsetree in match value with
| {
   pstr_desc;
   pstr_loc;
} -> Serializable_ast.{
   pstr_desc = transform_structure_item_desc pstr_desc;
   pstr_loc = transform_location_t pstr_loc;
}
and transform_structure value = let open Parsetree in match value with
| (value_0) -> (transform_list (transform_structure_item) value_0)
and transform_signature_item value = let open Parsetree in match value with
| {
   psig_desc;
   psig_loc;
} -> Serializable_ast.{
   psig_desc = transform_signature_item_desc psig_desc;
   psig_loc = transform_location_t psig_loc;
}
and transform_signature value = let open Parsetree in match value with
| (value_0) -> (transform_list (transform_signature_item) value_0)
and transform_payload value = let open Parsetree in match value with
| PStr value -> Serializable_ast.PStr (transform_structure value)
| PSig value -> Serializable_ast.PSig (transform_signature value)
| PTyp value -> Serializable_ast.PTyp (transform_core_type value)
| PPat (value_0,value_1) -> Serializable_ast.PPat (transform_pattern value_0,transform_option (transform_expression) value_1)
and transform_attributes value = let open Parsetree in match value with
| (value_0) -> (transform_list (transform_attribute) value_0)
and transform_pattern_desc value = let open Parsetree in match value with
| Ppat_any -> Serializable_ast.Ppat_any
| Ppat_var value -> Serializable_ast.Ppat_var (transform_location_loc transform_string value)
| Ppat_alias (value_0,value_1) -> Serializable_ast.Ppat_alias (transform_pattern value_0,transform_location_loc (transform_string) value_1)
| Ppat_constant value -> Serializable_ast.Ppat_constant (transform_constant value)
| Ppat_interval (value_0,value_1) -> Serializable_ast.Ppat_interval (transform_constant value_0,transform_constant value_1)
| Ppat_tuple value -> Serializable_ast.Ppat_tuple (transform_list transform_pattern value)
| Ppat_construct (value_0,value_1) -> Serializable_ast.Ppat_construct (transform_location_loc (transform_longident_t) value_0,transform_option (transform_pattern) value_1)
| Ppat_variant (value_0,value_1) -> Serializable_ast.Ppat_variant (transform_string value_0,transform_option (transform_pattern) value_1)
| Ppat_record (value_01,value_2) ->
  Serializable_ast.Ppat_record
    (transform_list (fun (value0, value1) ->
         (transform_location_loc transform_longident_t value0, transform_pattern value1))
        value_01, transform_closed_flag value_2)
| Ppat_array value -> Serializable_ast.Ppat_array (transform_list transform_pattern value)
| Ppat_or (value_0,value_1) -> Serializable_ast.Ppat_or (transform_pattern value_0,transform_pattern value_1)
| Ppat_constraint (value_0,value_1) -> Serializable_ast.Ppat_constraint (transform_pattern value_0,transform_core_type value_1)
| Ppat_type value -> Serializable_ast.Ppat_type (transform_location_loc transform_longident_t value)
| Ppat_lazy value -> Serializable_ast.Ppat_lazy (transform_pattern value)
| Ppat_unpack value -> Serializable_ast.Ppat_unpack (transform_location_loc transform_string value)
| Ppat_exception value -> Serializable_ast.Ppat_exception (transform_pattern value)
| Ppat_extension value -> Serializable_ast.Ppat_extension (transform_extension value)
| Ppat_open (value_0,value_1) -> Serializable_ast.Ppat_open (transform_location_loc (transform_longident_t) value_0,transform_pattern value_1)
and transform_pattern value = let open Parsetree in match value with
| {
   ppat_desc;
   ppat_loc;
   ppat_loc_stack;
   ppat_attributes;
} -> Serializable_ast.{
   ppat_desc = transform_pattern_desc ppat_desc;
   ppat_loc = transform_location_t ppat_loc;
   ppat_loc_stack = transform_list (transform_location_t) ppat_loc_stack;
   ppat_attributes = transform_attributes ppat_attributes;
}
and transform_case value = let open Parsetree in match value with
| {
   pc_lhs;
   pc_guard;
   pc_rhs;
} -> Serializable_ast.{
   pc_lhs = transform_pattern pc_lhs;
   pc_guard = transform_option (transform_expression) pc_guard;
   pc_rhs = transform_expression pc_rhs;
}
and transform_expression_desc value = let open Parsetree in match value with
| Pexp_ident value -> Serializable_ast.Pexp_ident (transform_location_loc transform_longident_t value)
| Pexp_constant value -> Serializable_ast.Pexp_constant (transform_constant value)
| Pexp_let (value_0,value_1,value_2) -> Serializable_ast.Pexp_let (transform_rec_flag value_0,transform_list (transform_value_binding) value_1,transform_expression value_2)
| Pexp_function value -> Serializable_ast.Pexp_function (transform_list transform_case value)
| Pexp_fun (value_0,value_1,value_2,value_3) -> Serializable_ast.Pexp_fun (transform_arg_label value_0,transform_option (transform_expression) value_1,transform_pattern value_2,transform_expression value_3)
| Pexp_apply (value_0,value_12) -> Serializable_ast.Pexp_apply (
    transform_expression value_0,
    transform_list
      (fun (value_1, value_2) ->
      transform_arg_label value_1, transform_expression value_2) value_12
    )
| Pexp_match (value_0,value_1) -> Serializable_ast.Pexp_match (transform_expression value_0,transform_list (transform_case) value_1)
| Pexp_try (value_0,value_1) -> Serializable_ast.Pexp_try (transform_expression value_0,transform_list (transform_case) value_1)
| Pexp_tuple value -> Serializable_ast.Pexp_tuple (transform_list transform_expression value)
| Pexp_construct (value_0,value_1) -> Serializable_ast.Pexp_construct (transform_location_loc (transform_longident_t) value_0,transform_option (transform_expression) value_1)
| Pexp_variant (value_0,value_1) -> Serializable_ast.Pexp_variant (transform_string value_0,transform_option (transform_expression) value_1)
| Pexp_record (value_01,value_2) -> Serializable_ast.Pexp_record (
    transform_list (fun (value_0,value_1) -> 
    transform_location_loc transform_longident_t value_0, transform_expression value_1) value_01,
    transform_option (transform_expression) value_2)
| Pexp_field (value_0,value_1) -> Serializable_ast.Pexp_field (transform_expression value_0,transform_location_loc (transform_longident_t) value_1)
| Pexp_setfield (value_0,value_1,value_2) -> Serializable_ast.Pexp_setfield (transform_expression value_0,transform_location_loc (transform_longident_t) value_1,transform_expression value_2)
| Pexp_array value -> Serializable_ast.Pexp_array (transform_list transform_expression value)
| Pexp_ifthenelse (value_0,value_1,value_2) -> Serializable_ast.Pexp_ifthenelse (transform_expression value_0,transform_expression value_1,transform_option (transform_expression) value_2)
| Pexp_sequence (value_0,value_1) -> Serializable_ast.Pexp_sequence (transform_expression value_0,transform_expression value_1)
| Pexp_while (value_0,value_1) -> Serializable_ast.Pexp_while (transform_expression value_0,transform_expression value_1)
| Pexp_for (value_0,value_1,value_2,value_3,value_4) -> Serializable_ast.Pexp_for (transform_pattern value_0,transform_expression value_1,transform_expression value_2,transform_direction_flag value_3,transform_expression value_4)
| Pexp_constraint (value_0,value_1) -> Serializable_ast.Pexp_constraint (transform_expression value_0,transform_core_type value_1)
| Pexp_coerce (value_0,value_1,value_2) -> Serializable_ast.Pexp_coerce (transform_expression value_0,transform_option (transform_core_type) value_1,transform_core_type value_2)
| Pexp_send (value_0,value_1) -> Serializable_ast.Pexp_send (transform_expression value_0,transform_location_loc (transform_string) value_1)
| Pexp_new value -> Serializable_ast.Pexp_new (transform_location_loc transform_longident_t value)
| Pexp_setinstvar (value_0,value_1) -> Serializable_ast.Pexp_setinstvar (transform_location_loc (transform_string) value_0,transform_expression value_1)
| Pexp_override (value_01) -> Serializable_ast.Pexp_override (
    transform_list (fun (value_0,value_1) ->
        transform_location_loc transform_string value_0, transform_expression value_1
      ) value_01)
| Pexp_letmodule (value_0,value_1,value_2) -> Serializable_ast.Pexp_letmodule (transform_location_loc (transform_string) value_0,transform_module_expr value_1,transform_expression value_2)
| Pexp_letexception (value_0,value_1) -> Serializable_ast.Pexp_letexception (transform_extension_constructor value_0,transform_expression value_1)
| Pexp_assert value -> Serializable_ast.Pexp_assert (transform_expression value)
| Pexp_lazy value -> Serializable_ast.Pexp_lazy (transform_expression value)
| Pexp_poly (value_0,value_1) -> Serializable_ast.Pexp_poly (transform_expression value_0,transform_option (transform_core_type) value_1)
| Pexp_object value -> Serializable_ast.Pexp_object (transform_class_structure value)
| Pexp_newtype (value_0,value_1) -> Serializable_ast.Pexp_newtype (transform_location_loc (transform_string) value_0,transform_expression value_1)
| Pexp_pack value -> Serializable_ast.Pexp_pack (transform_module_expr value)
| Pexp_open (value_0,value_1) -> Serializable_ast.Pexp_open (transform_open_declaration value_0,transform_expression value_1)
| Pexp_letop value -> Serializable_ast.Pexp_letop (transform_letop value)
| Pexp_extension value -> Serializable_ast.Pexp_extension (transform_extension value)
| Pexp_unreachable -> Serializable_ast.Pexp_unreachable
and transform_open_declaration value = let open Parsetree in match value with
| (value_0) -> (transform_open_infos (transform_module_expr) value_0)
and transform_expression value = let open Parsetree in match value with
| {
   pexp_desc;
   pexp_loc;
   pexp_loc_stack;
   pexp_attributes;
} -> Serializable_ast.{
   pexp_desc = transform_expression_desc pexp_desc;
   pexp_loc = transform_location_t pexp_loc;
   pexp_loc_stack = transform_list (transform_location_t) pexp_loc_stack;
   pexp_attributes = transform_attributes pexp_attributes;
}
and transform_structure_item_desc value = let open Parsetree in match value with
| Pstr_eval (value_0,value_1) -> Serializable_ast.Pstr_eval (transform_expression value_0,transform_attributes value_1)
| Pstr_value (value_0,value_1) -> Serializable_ast.Pstr_value (transform_rec_flag value_0,transform_list (transform_value_binding) value_1)
| Pstr_primitive value -> Serializable_ast.Pstr_primitive (transform_value_description value)
| Pstr_type (value_0,value_1) -> Serializable_ast.Pstr_type (transform_rec_flag value_0,transform_list (transform_type_declaration) value_1)
| Pstr_typext value -> Serializable_ast.Pstr_typext (transform_type_extension value)
| Pstr_exception value -> Serializable_ast.Pstr_exception (transform_type_exception value)
| Pstr_module value -> Serializable_ast.Pstr_module (transform_module_binding value)
| Pstr_recmodule value -> Serializable_ast.Pstr_recmodule (transform_list transform_module_binding value)
| Pstr_modtype value -> Serializable_ast.Pstr_modtype (transform_module_type_declaration value)
| Pstr_open value -> Serializable_ast.Pstr_open (transform_open_declaration value)
| Pstr_class value -> Serializable_ast.Pstr_class (transform_list transform_class_declaration  value)
| Pstr_class_type value -> Serializable_ast.Pstr_class_type (transform_list transform_class_type_declaration  value)
| Pstr_include value -> Serializable_ast.Pstr_include (transform_include_declaration value)
| Pstr_attribute value -> Serializable_ast.Pstr_attribute (transform_attribute value)
| Pstr_extension (value_0,value_1) -> Serializable_ast.Pstr_extension (transform_extension value_0,transform_attributes value_1)
and transform_signature_item_desc value = let open Parsetree in match value with
| Psig_value value -> Serializable_ast.Psig_value (transform_value_description value)
| Psig_type (value_0,value_1) -> Serializable_ast.Psig_type (transform_rec_flag value_0,transform_list (transform_type_declaration) value_1)
| Psig_typesubst value -> Serializable_ast.Psig_typesubst (transform_list transform_type_declaration value)
| Psig_typext value -> Serializable_ast.Psig_typext (transform_type_extension value)
| Psig_exception value -> Serializable_ast.Psig_exception (transform_type_exception value)
| Psig_module value -> Serializable_ast.Psig_module (transform_module_declaration value)
| Psig_modsubst value -> Serializable_ast.Psig_modsubst (transform_module_substitution value)
| Psig_recmodule value -> Serializable_ast.Psig_recmodule (transform_list transform_module_declaration value)
| Psig_modtype value -> Serializable_ast.Psig_modtype (transform_module_type_declaration value)
| Psig_open value -> Serializable_ast.Psig_open (transform_open_description value)
| Psig_include value -> Serializable_ast.Psig_include (transform_include_description value)
| Psig_class value -> Serializable_ast.Psig_class (transform_list transform_class_description value)
| Psig_class_type value -> Serializable_ast.Psig_class_type (transform_list transform_class_type_declaration value)
| Psig_attribute value -> Serializable_ast.Psig_attribute (transform_attribute value)
| Psig_extension (value_0,value_1) -> Serializable_ast.Psig_extension (transform_extension value_0,transform_attributes value_1)
and transform_module_expr value = let open Parsetree in match value with
| {
   pmod_desc;
   pmod_loc;
   pmod_attributes;
} -> Serializable_ast.{
   pmod_desc = transform_module_expr_desc pmod_desc;
   pmod_loc = transform_location_t pmod_loc;
   pmod_attributes = transform_attributes pmod_attributes;
}
and transform_module_expr_desc value = let open Parsetree in match value with
| Pmod_ident value -> Serializable_ast.Pmod_ident (transform_location_loc transform_longident_t value)
| Pmod_structure value -> Serializable_ast.Pmod_structure (transform_structure value)
| Pmod_functor (value_0,value_1,value_2) -> Serializable_ast.Pmod_functor (transform_location_loc (transform_string) value_0,transform_option (transform_module_type) value_1,transform_module_expr value_2)
| Pmod_apply (value_0,value_1) -> Serializable_ast.Pmod_apply (transform_module_expr value_0,transform_module_expr value_1)
| Pmod_constraint (value_0,value_1) -> Serializable_ast.Pmod_constraint (transform_module_expr value_0,transform_module_type value_1)
| Pmod_unpack value -> Serializable_ast.Pmod_unpack (transform_expression value)
| Pmod_extension value -> Serializable_ast.Pmod_extension (transform_extension value)
and transform_module_type value = let open Parsetree in match value with
| {
   pmty_desc;
   pmty_loc;
   pmty_attributes;
} -> Serializable_ast.{
   pmty_desc = transform_module_type_desc pmty_desc;
   pmty_loc = transform_location_t pmty_loc;
   pmty_attributes = transform_attributes pmty_attributes;
}
and transform_module_type_desc value = let open Parsetree in match value with
| Pmty_ident value -> Serializable_ast.Pmty_ident (transform_location_loc transform_longident_t value)
| Pmty_signature value -> Serializable_ast.Pmty_signature (transform_signature value)
| Pmty_functor (value_0,value_1,value_2) -> Serializable_ast.Pmty_functor (transform_location_loc (transform_string) value_0,transform_option (transform_module_type) value_1,transform_module_type value_2)
| Pmty_with (value_0,value_1) -> Serializable_ast.Pmty_with (transform_module_type value_0,transform_list (transform_with_constraint) value_1)
| Pmty_typeof value -> Serializable_ast.Pmty_typeof (transform_module_expr value)
| Pmty_extension value -> Serializable_ast.Pmty_extension (transform_extension value)
| Pmty_alias value -> Serializable_ast.Pmty_alias (transform_location_loc transform_longident_t value)
and transform_with_constraint value = let open Parsetree in match value with
| Pwith_type (value_0,value_1) -> Serializable_ast.Pwith_type (transform_location_loc (transform_longident_t) value_0,transform_type_declaration value_1)
| Pwith_module (value_0,value_1) -> Serializable_ast.Pwith_module (transform_location_loc (transform_longident_t) value_0,transform_location_loc (transform_longident_t) value_1)
| Pwith_typesubst (value_0,value_1) -> Serializable_ast.Pwith_typesubst (transform_location_loc (transform_longident_t) value_0,transform_type_declaration value_1)
| Pwith_modsubst (value_0,value_1) -> Serializable_ast.Pwith_modsubst (transform_location_loc (transform_longident_t) value_0,transform_location_loc (transform_longident_t) value_1)
and transform_type_declaration value = let open Parsetree in match value with
| {
  ptype_name;
  ptype_params;
  ptype_cstrs ;
   ptype_kind;
   ptype_private;
   ptype_manifest;
   ptype_attributes;
   ptype_loc;
} -> Serializable_ast.{
   ptype_name = transform_location_loc (transform_string) ptype_name;
   ptype_params = List.map ~f:(fun (value0,value1) ->
       transform_core_type value0, transform_variance value1
     ) ptype_params;
  ptype_cstrs =  List.map ~f:(fun (value0,value1,value2) ->
       transform_core_type value0, transform_core_type value1, transform_location_t value2
     ) ptype_cstrs;
   ptype_kind = transform_type_kind ptype_kind;
   ptype_private = transform_private_flag ptype_private;
   ptype_manifest = transform_option (transform_core_type) ptype_manifest;
   ptype_attributes = transform_attributes ptype_attributes;
   ptype_loc = transform_location_t ptype_loc;
}
and transform_type_kind value = let open Parsetree in match value with
| Ptype_abstract -> Serializable_ast.Ptype_abstract
| Ptype_variant value -> Serializable_ast.Ptype_variant (transform_list transform_constructor_declaration value)
| Ptype_record value -> Serializable_ast.Ptype_record (transform_list transform_label_declaration value)
| Ptype_open -> Serializable_ast.Ptype_open
and transform_constructor_declaration value = let open Parsetree in match value with
| {
   pcd_name;
   pcd_args;
   pcd_res;
   pcd_loc;
   pcd_attributes;
} -> Serializable_ast.{
   pcd_name = transform_location_loc (transform_string) pcd_name;
   pcd_args = transform_constructor_arguments pcd_args;
   pcd_res = transform_option (transform_core_type) pcd_res;
   pcd_loc = transform_location_t pcd_loc;
   pcd_attributes = transform_attributes pcd_attributes;
}
and transform_constructor_arguments value = let open Parsetree in match value with
| Pcstr_tuple value -> Serializable_ast.Pcstr_tuple (transform_list transform_core_type value)
| Pcstr_record value -> Serializable_ast.Pcstr_record (transform_list transform_label_declaration value)
and transform_label_declaration value = let open Parsetree in match value with
| {
   pld_name;
   pld_mutable;
   pld_type;
   pld_loc;
   pld_attributes;
} -> Serializable_ast.{
   pld_name = transform_location_loc (transform_string) pld_name;
   pld_mutable = transform_mutable_flag pld_mutable;
   pld_type = transform_core_type pld_type;
   pld_loc = transform_location_t pld_loc;
   pld_attributes = transform_attributes pld_attributes;
}
and transform_extension_constructor value = let open Parsetree in match value with
| {
   pext_name;
   pext_kind;
   pext_loc;
   pext_attributes;
} -> Serializable_ast.{
   pext_name = transform_location_loc (transform_string) pext_name;
   pext_kind = transform_extension_constructor_kind pext_kind;
   pext_loc = transform_location_t pext_loc;
   pext_attributes = transform_attributes pext_attributes;
}
and transform_extension_constructor_kind value = let open Parsetree in match value with
| Pext_decl (value_0,value_1) -> Serializable_ast.Pext_decl (transform_constructor_arguments value_0,transform_option (transform_core_type) value_1)
| Pext_rebind value -> Serializable_ast.Pext_rebind (transform_location_loc transform_longident_t value)
and transform_class_structure value = let open Parsetree in match value with
| {
   pcstr_self;
   pcstr_fields;
} -> Serializable_ast.{
   pcstr_self = transform_pattern pcstr_self;
   pcstr_fields = transform_list (transform_class_field) pcstr_fields;
}
and transform_class_field value = let open Parsetree in match value with
| {
   pcf_desc;
   pcf_loc;
   pcf_attributes;
} -> Serializable_ast.{
   pcf_desc = transform_class_field_desc pcf_desc;
   pcf_loc = transform_location_t pcf_loc;
   pcf_attributes = transform_attributes pcf_attributes;
}
and transform_class_field_desc value = let open Parsetree in match value with
| Pcf_inherit (value_0,value_1,value_2) -> Serializable_ast.Pcf_inherit (transform_override_flag value_0,transform_class_expr value_1,transform_option (transform_location_loc (transform_string)) value_2)
| Pcf_val (value_0,value_1,value_2) -> Serializable_ast.Pcf_val (transform_location_loc (transform_string) value_0,transform_mutable_flag value_1,transform_class_field_kind value_2)
| Pcf_method (value_0,value_1,value_2) -> Serializable_ast.Pcf_method (transform_location_loc (transform_string) value_0,transform_private_flag value_1,transform_class_field_kind value_2)
| Pcf_constraint (value_0,value_1) -> Serializable_ast.Pcf_constraint (transform_core_type value_0,transform_core_type value_1)
| Pcf_initializer value -> Serializable_ast.Pcf_initializer (transform_expression value)
| Pcf_attribute value -> Serializable_ast.Pcf_attribute (transform_attribute value)
| Pcf_extension value -> Serializable_ast.Pcf_extension (transform_extension value)
and transform_class_expr value = let open Parsetree in match value with
| {
   pcl_desc;
   pcl_loc;
   pcl_attributes;
} -> Serializable_ast.{
   pcl_desc = transform_class_expr_desc pcl_desc;
   pcl_loc = transform_location_t pcl_loc;
   pcl_attributes = transform_attributes pcl_attributes;
}
and transform_class_expr_desc value = let open Parsetree in match value with
| Pcl_constr (value_0,value_1) -> Serializable_ast.Pcl_constr (transform_location_loc (transform_longident_t) value_0,transform_list (transform_core_type) value_1)
| Pcl_structure value -> Serializable_ast.Pcl_structure (transform_class_structure value)
| Pcl_fun (value_0,value_1,value_2,value_3) -> Serializable_ast.Pcl_fun (transform_arg_label value_0,transform_option (transform_expression) value_1,transform_pattern value_2,transform_class_expr value_3)
| Pcl_apply (value_0,value_12) -> Serializable_ast.Pcl_apply
                                    (
                                      transform_class_expr value_0,
                                      transform_list
                                        (fun (value_1,value_2) ->
                                           transform_arg_label value_1,transform_expression value_2
                                        ) value_12
                                      )
| Pcl_let (value_0,value_1,value_2) -> Serializable_ast.Pcl_let (transform_rec_flag value_0,transform_list (transform_value_binding) value_1,transform_class_expr value_2)
| Pcl_constraint (value_0,value_1) -> Serializable_ast.Pcl_constraint (transform_class_expr value_0,transform_class_type value_1)
| Pcl_extension value -> Serializable_ast.Pcl_extension (transform_extension value)
| Pcl_open (value_0,value_1) -> Serializable_ast.Pcl_open (transform_open_description value_0,transform_class_expr value_1)
and transform_class_field_kind value = let open Parsetree in match value with
| Cfk_virtual value -> Serializable_ast.Cfk_virtual (transform_core_type value)
| Cfk_concrete (value_0,value_1) -> Serializable_ast.Cfk_concrete (transform_override_flag value_0,transform_expression value_1)
and transform_class_type value = let open Parsetree in match value with
| {
   pcty_desc;
   pcty_loc;
   pcty_attributes;
} -> Serializable_ast.{
   pcty_desc = transform_class_type_desc pcty_desc;
   pcty_loc = transform_location_t pcty_loc;
   pcty_attributes = transform_attributes pcty_attributes;
}
and transform_class_type_desc value = let open Parsetree in match value with
| Pcty_constr (value_0,value_1) -> Serializable_ast.Pcty_constr (transform_location_loc (transform_longident_t) value_0,transform_list (transform_core_type) value_1)
| Pcty_signature value -> Serializable_ast.Pcty_signature (transform_class_signature value)
| Pcty_arrow (value_0,value_1,value_2) -> Serializable_ast.Pcty_arrow (transform_arg_label value_0,transform_core_type value_1,transform_class_type value_2)
| Pcty_extension value -> Serializable_ast.Pcty_extension (transform_extension value)
| Pcty_open (value_0,value_1) -> Serializable_ast.Pcty_open (transform_open_description value_0,transform_class_type value_1)
and transform_open_description value = let open Parsetree in match value with
| (value_0) -> (transform_open_infos (transform_location_loc transform_longident_t) value_0)
and transform_open_infos : 'a 'b. ('a -> 'b) -> 'a Parsetree.open_infos -> 'b Serializable_ast.open_infos = (fun (type a b)(f: a -> b) (value: a Parsetree.open_infos) -> let open Parsetree in match value with
| {
   popen_expr;
   popen_override;
   popen_loc;
   popen_attributes;
} -> Serializable_ast.{
   popen_expr = f popen_expr;
   popen_override = transform_override_flag popen_override;
   popen_loc = transform_location_t popen_loc;
   popen_attributes = transform_attributes popen_attributes;
})
and transform_class_signature value = let open Parsetree in match value with
| {
   pcsig_self;
   pcsig_fields;
} -> Serializable_ast.{
   pcsig_self = transform_core_type pcsig_self;
   pcsig_fields = transform_list (transform_class_type_field) pcsig_fields;
}
and transform_class_type_field value = let open Parsetree in match value with
| {
   pctf_desc;
   pctf_loc;
   pctf_attributes;
} -> Serializable_ast.{
   pctf_desc = transform_class_type_field_desc pctf_desc;
   pctf_loc = transform_location_t pctf_loc;
   pctf_attributes = transform_attributes pctf_attributes;
}
and transform_class_type_field_desc value = let open Parsetree in match value with
| Pctf_inherit value -> Serializable_ast.Pctf_inherit (transform_class_type value)
| Pctf_val (value_0,value_1,value_2,value_3) ->
  Serializable_ast.Pctf_val (
    transform_location_loc transform_string value_0,
                            transform_mutable_flag value_1,
                            transform_virtual_flag value_2,
                            transform_core_type value_3)
| Pctf_method (value_0,value_1,value_2,value_3) -> Serializable_ast.Pctf_method
                                                     (transform_location_loc (transform_string) value_0,
                                                      transform_private_flag value_1,
                                                      transform_virtual_flag value_2,
                                                      transform_core_type value_3)
| Pctf_constraint (value_0,value_1) -> Serializable_ast.Pctf_constraint (transform_core_type value_0,transform_core_type value_1)
| Pctf_attribute value -> Serializable_ast.Pctf_attribute (transform_attribute value)
| Pctf_extension value -> Serializable_ast.Pctf_extension (transform_extension value)
and transform_letop value = let open Parsetree in match value with
| {
   let_;
   ands;
   body;
} -> Serializable_ast.{
   let_ = transform_binding_op let_;
   ands = transform_list (transform_binding_op) ands;
   body = transform_expression body;
}
and transform_binding_op value = let open Parsetree in match value with
| {
   pbop_op;
   pbop_pat;
   pbop_exp;
   pbop_loc;
} -> Serializable_ast.{
   pbop_op = transform_location_loc (transform_string) pbop_op;
   pbop_pat = transform_pattern pbop_pat;
   pbop_exp = transform_expression pbop_exp;
   pbop_loc = transform_location_t pbop_loc;
}
and transform_value_description value = let open Parsetree in match value with
| {
   pval_name;
   pval_type;
   pval_prim;
   pval_attributes;
   pval_loc;
} -> Serializable_ast.{
   pval_name = transform_location_loc (transform_string) pval_name;
   pval_type = transform_core_type pval_type;
   pval_prim = transform_list (transform_string) pval_prim;
   pval_attributes = transform_attributes pval_attributes;
   pval_loc = transform_location_t pval_loc;
}
and transform_type_extension value = let open Parsetree in match value with
| {
   ptyext_path;
   ptyext_params;
   ptyext_constructors;
   ptyext_private;
   ptyext_loc;
   ptyext_attributes;
} -> Serializable_ast.{
   ptyext_path = transform_location_loc (transform_longident_t) ptyext_path;
   ptyext_params = transform_list
       (fun (value0,value1) -> (transform_core_type value0, transform_variance value1))
       ptyext_params;
   ptyext_constructors = transform_list (transform_extension_constructor) ptyext_constructors;
   ptyext_private = transform_private_flag ptyext_private;
   ptyext_loc = transform_location_t ptyext_loc;
   ptyext_attributes = transform_attributes ptyext_attributes;
}
and transform_type_exception value = let open Parsetree in match value with
| {
   ptyexn_constructor;
   ptyexn_loc;
   ptyexn_attributes;
} -> Serializable_ast.{
   ptyexn_constructor = transform_extension_constructor ptyexn_constructor;
   ptyexn_loc = transform_location_t ptyexn_loc;
   ptyexn_attributes = transform_attributes ptyexn_attributes;
}
and transform_module_binding value = let open Parsetree in match value with
| {
   pmb_name;
   pmb_expr;
   pmb_attributes;
   pmb_loc;
} -> Serializable_ast.{
   pmb_name = transform_location_loc (transform_string) pmb_name;
   pmb_expr = transform_module_expr pmb_expr;
   pmb_attributes = transform_attributes pmb_attributes;
   pmb_loc = transform_location_t pmb_loc;
}
and transform_module_type_declaration value = let open Parsetree in match value with
| {
   pmtd_name;
   pmtd_type;
   pmtd_attributes;
   pmtd_loc;
} -> Serializable_ast.{
   pmtd_name = transform_location_loc (transform_string) pmtd_name;
   pmtd_type = transform_option (transform_module_type) pmtd_type;
   pmtd_attributes = transform_attributes pmtd_attributes;
   pmtd_loc = transform_location_t pmtd_loc;
}
and transform_class_declaration value = let open Parsetree in match value with
| (value_0) -> (transform_class_infos (transform_class_expr) value_0)
and transform_class_infos : 'a 'b. ('a -> 'b) -> 'a Parsetree.class_infos -> 'b Serializable_ast.class_infos =
  (fun (type a b) (f: a -> b) (value: a Parsetree.class_infos) : b Serializable_ast.class_infos -> let open Parsetree in match value with
| {
   pci_virt;
   pci_params;
   pci_name;
   pci_expr;
   pci_loc;
   pci_attributes;
} -> Serializable_ast.{
   pci_virt = transform_virtual_flag pci_virt;
   pci_params = transform_list
     (fun (a,b) ->  transform_core_type a, transform_variance b)  pci_params;
   pci_name = transform_location_loc (transform_string) pci_name;
   pci_expr = f pci_expr;
   pci_loc = transform_location_t pci_loc;
   pci_attributes = transform_attributes pci_attributes;
})
and transform_class_type_declaration (value: Parsetree.class_type_declaration) : Serializable_ast.class_type_declaration = let open Parsetree in match value with
| (value_0) -> (transform_class_infos (transform_class_type) value_0)
and transform_include_declaration value = let open Parsetree in match value with
| (value_0) -> (transform_include_infos (transform_module_expr) value_0)
and transform_include_infos : 'a 'b . ('a -> 'b) -> 'a Parsetree.include_infos -> 'b Serializable_ast.include_infos = (fun (type a b) (f: a -> b) value -> let open Parsetree in match value with
| {
   pincl_mod;
   pincl_loc;
   pincl_attributes;
 } -> Serializable_ast.{
   pincl_mod = f pincl_mod;
   pincl_loc = transform_location_t pincl_loc;
   pincl_attributes = transform_attributes pincl_attributes;
})
and transform_module_declaration value = let open Parsetree in match value with
| {
   pmd_name;
   pmd_type;
   pmd_attributes;
   pmd_loc;
} -> Serializable_ast.{
   pmd_name = transform_location_loc (transform_string) pmd_name;
   pmd_type = transform_module_type pmd_type;
   pmd_attributes = transform_attributes pmd_attributes;
   pmd_loc = transform_location_t pmd_loc;
}
and transform_module_substitution value = let open Parsetree in match value with
| {
   pms_name;
   pms_manifest;
   pms_attributes;
   pms_loc;
} -> Serializable_ast.{
   pms_name = transform_location_loc (transform_string) pms_name;
   pms_manifest = transform_location_loc (transform_longident_t) pms_manifest;
   pms_attributes = transform_attributes pms_attributes;
   pms_loc = transform_location_t pms_loc;
}
and transform_include_description value = let open Parsetree in match value with
| (value_0) -> (transform_include_infos (transform_module_type) value_0)
and transform_class_description value = let open Parsetree in match value with
| (value_0) -> (transform_class_infos (transform_class_type) value_0)

