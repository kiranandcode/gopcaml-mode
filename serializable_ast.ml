open Core

module Lexing = struct
  type position = {
    pos_fname : String.t;
    pos_lnum : int;
    pos_bol : int;
    pos_cnum : int;
  } [@@deriving sexp]
end

module Location = struct
  type t = {
    loc_start : Lexing.position;
    loc_end : Lexing.position;
    loc_ghost : bool;
  } [@@deriving sexp]

  type 'a loc = { txt : 'a; loc : t; } [@@deriving sexp]
end

module Longident = struct
  type t = Lident of string | Ldot of t * string | Lapply of t * t [@@deriving sexp]
end

type constant =
      Pconst_integer of string * char option
    | Pconst_char of char
    | Pconst_string of string * string option
    | Pconst_float of string * char option [@@deriving sexp]

type rec_flag = Nonrecursive | Recursive [@@deriving sexp]
type closed_flag = Closed | Open [@@deriving sexp]
type arg_label = Nolabel | Labelled of string | Optional of string [@@deriving sexp]
type direction_flag = Upto | Downto  [@@deriving sexp]
type variance = Covariant | Contravariant | Invariant [@@deriving sexp]
type private_flag = Private | Public [@@deriving sexp]
type mutable_flag = Immutable | Mutable [@@deriving sexp]
type override_flag = Override | Fresh [@@deriving sexp]
type virtual_flag = Virtual | Concrete [@@deriving sexp]

type value_binding = {
  pvb_pat : pattern;
  pvb_expr : expression;
  pvb_attributes : attributes;
  pvb_loc : Location.t;
} and attribute = {
  attr_name : string Location.loc;
  attr_payload : payload;
  attr_loc : Location.t;
} and row_field_desc =
      Rtag of string Location.loc * bool * core_type list
    | Rinherit of core_type
and row_field = {
  prf_desc : row_field_desc;
  prf_loc : Location.t;
  prf_attributes : attributes;
} 
and object_field_desc =
    Otag of string Location.loc * core_type
    | Oinherit of core_type
and object_field = {
  pof_desc : object_field_desc;
  pof_loc : Location.t;
  pof_attributes : attributes;
} and package_type =
    Longident.t Location.loc *
    (Longident.t Location.loc * core_type) list
and extension = string Location.loc * payload
and core_type_desc =
    Ptyp_any
  | Ptyp_var of string
  | Ptyp_arrow of arg_label * core_type * core_type
  | Ptyp_tuple of core_type list
  | Ptyp_constr of Longident.t Location.loc * core_type list
  | Ptyp_object of object_field list * closed_flag
  | Ptyp_class of Longident.t Location.loc * core_type list
  | Ptyp_alias of core_type * string
  | Ptyp_variant of row_field list * closed_flag *
      string list option
  | Ptyp_poly of string Location.loc list * core_type
  | Ptyp_package of package_type
  | Ptyp_extension of extension
and core_type = {
  ptyp_desc : core_type_desc;
  ptyp_loc : Location.t;
  ptyp_loc_stack : Location.t list;
  ptyp_attributes : attributes;
} and structure_item = {
  pstr_desc : structure_item_desc;
  pstr_loc : Location.t;
}
and structure = structure_item list
and signature_item = {
  psig_desc : signature_item_desc;
  psig_loc : Location.t;
}
and signature = signature_item list
and payload =
    PStr of structure
  | PSig of signature
  | PTyp of core_type
  | PPat of pattern * expression option
and attributes = attribute list
and pattern_desc =
    Ppat_any
  | Ppat_var of string Location.loc
  | Ppat_alias of pattern * string Location.loc
  | Ppat_constant of constant
  | Ppat_interval of constant * constant
  | Ppat_tuple of pattern list
  | Ppat_construct of Longident.t Location.loc * pattern option
  | Ppat_variant of string * pattern option
  | Ppat_record of (Longident.t Location.loc * pattern) list *
      closed_flag
  | Ppat_array of pattern list
  | Ppat_or of pattern * pattern
  | Ppat_constraint of pattern * core_type
  | Ppat_type of Longident.t Location.loc
  | Ppat_lazy of pattern
  | Ppat_unpack of string Location.loc
  | Ppat_exception of pattern
  | Ppat_extension of extension
  | Ppat_open of Longident.t Location.loc * pattern
and pattern = {
  ppat_desc : pattern_desc;
  ppat_loc : Location.t;
  ppat_loc_stack : Location.t list;
  ppat_attributes : attributes;
} and case = {
  pc_lhs : pattern;
  pc_guard : expression option;
  pc_rhs : expression;
} and expression_desc =
    Pexp_ident of Longident.t Location.loc
  | Pexp_constant of constant
  | Pexp_let of rec_flag * value_binding list *
                expression
  | Pexp_function of case list
  | Pexp_fun of arg_label * expression option *
                pattern * expression
  | Pexp_apply of expression *
                  (arg_label * expression) list
  | Pexp_match of expression * case list
  | Pexp_try of expression * case list
  | Pexp_tuple of expression list
  | Pexp_construct of Longident.t Location.loc * expression option
  | Pexp_variant of string * expression option
  | Pexp_record of (Longident.t Location.loc * expression) list *
                   expression option
  | Pexp_field of expression * Longident.t Location.loc
  | Pexp_setfield of expression * Longident.t Location.loc *
                     expression
  | Pexp_array of expression list
  | Pexp_ifthenelse of expression * expression *
                       expression option
  | Pexp_sequence of expression * expression
  | Pexp_while of expression * expression
  | Pexp_for of pattern * expression *
                expression * direction_flag * expression
  | Pexp_constraint of expression * core_type
  | Pexp_coerce of expression * core_type option *
                   core_type
  | Pexp_send of expression * string Location.loc
  | Pexp_new of Longident.t Location.loc
  | Pexp_setinstvar of string Location.loc * expression
  | Pexp_override of
      (string Location.loc * expression) list
  | Pexp_letmodule of string Location.loc * module_expr *
                      expression
  | Pexp_letexception of extension_constructor *
                         expression
  | Pexp_assert of expression
  | Pexp_lazy of expression
  | Pexp_poly of expression * core_type option
  | Pexp_object of class_structure
  | Pexp_newtype of string Location.loc * expression
  | Pexp_pack of module_expr
  | Pexp_open of open_declaration * expression
  | Pexp_letop of letop
  | Pexp_extension of extension
  | Pexp_unreachable 
and open_declaration = module_expr open_infos
and expression = {
  pexp_desc : expression_desc;
  pexp_loc : Location.t;
  pexp_loc_stack : Location.t list;
  pexp_attributes : attributes;
} 
and structure_item_desc =
    Pstr_eval of expression * attributes
  | Pstr_value of rec_flag * value_binding list
  | Pstr_primitive of value_description
  | Pstr_type of rec_flag * type_declaration list
  | Pstr_typext of type_extension
  | Pstr_exception of type_exception
  | Pstr_module of module_binding
  | Pstr_recmodule of module_binding list
  | Pstr_modtype of module_type_declaration
  | Pstr_open of open_declaration
  | Pstr_class of class_declaration list
  | Pstr_class_type of class_type_declaration list
  | Pstr_include of include_declaration
  | Pstr_attribute of attribute
  | Pstr_extension of extension * attributes 
and signature_item_desc =
    Psig_value of value_description
  | Psig_type of rec_flag * type_declaration list
  | Psig_typesubst of type_declaration list
  | Psig_typext of type_extension
  | Psig_exception of type_exception
  | Psig_module of module_declaration
  | Psig_modsubst of module_substitution
  | Psig_recmodule of module_declaration list
  | Psig_modtype of module_type_declaration
  | Psig_open of open_description
  | Psig_include of include_description
  | Psig_class of class_description list
  | Psig_class_type of class_type_declaration list
  | Psig_attribute of attribute
  | Psig_extension of extension * attributes
and module_expr = {
  pmod_desc : module_expr_desc;
  pmod_loc : Location.t;
  pmod_attributes : attributes;
}
and module_expr_desc =
    Pmod_ident of Longident.t Location.loc
  | Pmod_structure of structure
  | Pmod_functor of string Location.loc * module_type option *
      module_expr
  | Pmod_apply of module_expr * module_expr
  | Pmod_constraint of module_expr * module_type
  | Pmod_unpack of expression
  | Pmod_extension of extension
and module_type = {
  pmty_desc : module_type_desc;
  pmty_loc : Location.t;
  pmty_attributes : attributes;
}
and module_type_desc =
    Pmty_ident of Longident.t Location.loc
  | Pmty_signature of signature
  | Pmty_functor of string Location.loc * module_type option *
      module_type
  | Pmty_with of module_type * with_constraint list
  | Pmty_typeof of module_expr
  | Pmty_extension of extension
  | Pmty_alias of Longident.t Location.loc
and with_constraint =
    Pwith_type of Longident.t Location.loc * type_declaration
  | Pwith_module of Longident.t Location.loc * Longident.t Location.loc
  | Pwith_typesubst of Longident.t Location.loc *
      type_declaration
  | Pwith_modsubst of Longident.t Location.loc * Longident.t Location.loc
and type_declaration = {
  ptype_name : string Location.loc;
  ptype_params : (core_type * variance) list;
  ptype_cstrs :
    (core_type * core_type * Location.t) list;
  ptype_kind : type_kind;
  ptype_private : private_flag;
  ptype_manifest : core_type option;
  ptype_attributes : attributes;
  ptype_loc : Location.t;
}
and type_kind =
    Ptype_abstract
  | Ptype_variant of constructor_declaration list
  | Ptype_record of label_declaration list
  | Ptype_open
and constructor_declaration = {
  pcd_name : string Location.loc;
  pcd_args : constructor_arguments;
  pcd_res : core_type option;
  pcd_loc : Location.t;
  pcd_attributes : attributes;
}
and constructor_arguments =
    Pcstr_tuple of core_type list
  | Pcstr_record of label_declaration list
and label_declaration = {
  pld_name : string Location.loc;
  pld_mutable : mutable_flag;
  pld_type : core_type;
  pld_loc : Location.t;
  pld_attributes : attributes;
}
and extension_constructor = {
  pext_name : string Location.loc;
  pext_kind : extension_constructor_kind;
  pext_loc : Location.t;
  pext_attributes : attributes;
}
and extension_constructor_kind =
    Pext_decl of constructor_arguments * core_type option
  | Pext_rebind of Longident.t Location.loc
and class_structure = {
  pcstr_self : pattern;
  pcstr_fields : class_field list;
}
and class_field = {
  pcf_desc : class_field_desc;
  pcf_loc : Location.t;
  pcf_attributes : attributes;
}
and class_field_desc =
    Pcf_inherit of override_flag * class_expr *
      string Location.loc option
  | Pcf_val of
      (string Location.loc * mutable_flag *
       class_field_kind)
  | Pcf_method of
      (string Location.loc * private_flag *
       class_field_kind)
  | Pcf_constraint of (core_type * core_type)
  | Pcf_initializer of expression
  | Pcf_attribute of attribute
  | Pcf_extension of extension
and class_expr = {
  pcl_desc : class_expr_desc;
  pcl_loc : Location.t;
  pcl_attributes : attributes;
}
and class_expr_desc =
    Pcl_constr of Longident.t Location.loc * core_type list
  | Pcl_structure of class_structure
  | Pcl_fun of arg_label * expression option *
      pattern * class_expr
  | Pcl_apply of class_expr *
      (arg_label * expression) list
  | Pcl_let of rec_flag * value_binding list *
      class_expr
  | Pcl_constraint of class_expr * class_type
  | Pcl_extension of extension
  | Pcl_open of open_description * class_expr
and class_field_kind =
    Cfk_virtual of core_type
  | Cfk_concrete of override_flag * expression
and class_type = {
  pcty_desc : class_type_desc;
  pcty_loc : Location.t;
  pcty_attributes : attributes;
}
and class_type_desc =
    Pcty_constr of Longident.t Location.loc * core_type list
  | Pcty_signature of class_signature
  | Pcty_arrow of arg_label * core_type *
      class_type
  | Pcty_extension of extension
  | Pcty_open of open_description * class_type
and open_description = Longident.t Location.loc open_infos
and 'a open_infos = {
  popen_expr : 'a;
  popen_override : override_flag;
  popen_loc : Location.t;
  popen_attributes : attributes;
}
and class_signature = {
  pcsig_self : core_type;
  pcsig_fields : class_type_field list;
}
and class_type_field = {
  pctf_desc : class_type_field_desc;
  pctf_loc : Location.t;
  pctf_attributes : attributes;
}
and class_type_field_desc =
    Pctf_inherit of class_type
  | Pctf_val of
      (string Location.loc * mutable_flag *
       virtual_flag * core_type)
  | Pctf_method of
      (string Location.loc * private_flag *
       virtual_flag * core_type)
  | Pctf_constraint of (core_type * core_type)
  | Pctf_attribute of attribute
  | Pctf_extension of extension
and letop = {
  let_ : binding_op;
  ands : binding_op list;
  body : expression;
}
and binding_op = {
  pbop_op : string Location.loc;
  pbop_pat : pattern;
  pbop_exp : expression;
  pbop_loc : Location.t;
}
and value_description = {
  pval_name : string Location.loc;
  pval_type : core_type;
  pval_prim : string list;
  pval_attributes : attributes;
  pval_loc : Location.t;
}
and type_extension = {
  ptyext_path : Longident.t Location.loc;
  ptyext_params : (core_type * variance) list;
  ptyext_constructors : extension_constructor list;
  ptyext_private : private_flag;
  ptyext_loc : Location.t;
  ptyext_attributes : attributes;
}
and type_exception = {
  ptyexn_constructor : extension_constructor;
  ptyexn_loc : Location.t;
  ptyexn_attributes : attributes;
}
and module_binding = {
  pmb_name : string Location.loc;
  pmb_expr : module_expr;
  pmb_attributes : attributes;
  pmb_loc : Location.t;
}
and module_type_declaration = {
  pmtd_name : string Location.loc;
  pmtd_type : module_type option;
  pmtd_attributes : attributes;
  pmtd_loc : Location.t;
}
and class_declaration = class_expr class_infos
and 'a class_infos = {
  pci_virt : virtual_flag;
  pci_params : (core_type * variance) list;
  pci_name : string Location.loc;
  pci_expr : 'a;
  pci_loc : Location.t;
  pci_attributes : attributes;
}
and class_type_declaration = class_type class_infos
and include_declaration = module_expr include_infos
and 'a include_infos = {
  pincl_mod : 'a;
  pincl_loc : Location.t;
  pincl_attributes : attributes;
}
and module_declaration = {
  pmd_name : string Location.loc;
  pmd_type : module_type;
  pmd_attributes : attributes;
  pmd_loc : Location.t;
}
and module_substitution = {
  pms_name : string Location.loc;
  pms_manifest : Longident.t Location.loc;
  pms_attributes : attributes;
  pms_loc : Location.t;
}
and include_description = module_type include_infos
and class_description = class_type class_infos
[@@ deriving sexp]

