open Core


(* (\* zipper for the ocaml ast - allows recursive and backwards and widthwards traversal *\)
 * type t =
 *   (\* zips a parse tree structure with the structure item, and its position  *\)
 *   | Attr of  Parsetree.attribute
 *   | Case of  Parsetree.case
 *   | Class_field of  Parsetree.class_field
 *   | Class_type_field of Parsetree.class_type_field
 *   | Signature_item of  Parsetree.signature_item
 *   | Structure_item of  Parsetree.structure_item
 * 
 *   | Type_kind of  Parsetree.type_kind
 *   | BindingOp of  Parsetree.binding_op
 *   | ClassDeclaration of Parsetree.class_declaration
 *   | ClassDescription of Parsetree.class_description
 *   | ClassExpr of  Parsetree.class_expr
 *   | Class_type of  Parsetree.class_type
 *   | Class_type_declaration of Parsetree.class_type_declaration
 *   | Constructor_declaration of Parsetree.constructor_declaration
 *   | Expr of  Parsetree.expression
 *   | Extension of  Parsetree.extension
 *   | Extension_constructor of Parsetree.extension_constructor
 *   | Include_declaration of Parsetree.include_declaration
 *   | Include_description of Parsetree.include_description
 *   | Label_declaration of Parsetree.label_declaration
 *   | Module_binding of  Parsetree.module_binding
 *   | Module_declaration of Parsetree.module_declaration
 *   | Module_substitution of Parsetree.module_substitution
 *   | Module_expr of  Parsetree.module_expr
 *   | Module_type of  Parsetree.module_type
 *   | Module_type_declaration of Parsetree.module_type_declaration
 *   | Open_declaration of Parsetree.open_declaration
 *   | Open_description of Parsetree.open_description
 *   | Pat of  Parsetree.pattern
 *   | Payload of  Parsetree.payload
 *   | CoreTyp of  Parsetree.core_type
 *   | Row_field of  Parsetree.row_field
 *   | Object_field of  Parsetree.object_field
 *   | Type_declaration of Parsetree.type_declaration
 *   | Type_extension of  Parsetree.type_extension
 *   | Type_exception of  Parsetree.type_exception
 *   | Value_binding of  Parsetree.value_binding
 *   | Value_description of Parsetree.value_description
 *   | With_constraint of Parsetree.with_constraint
 *   (\* sequence *\)
 * and m = 
 *   | Signature of  Parsetree.signature
 *   | Structure of  Parsetree.structure
 *   | Attrs of  Parsetree.attribute list
 *   | Cases of  Parsetree.case list
 *   | Class_signature of Parsetree.class_signature
 *   | Class_structure of Parsetree.class_structure
 *   | Type_declarations of  Parsetree.type_declaration list
 *   | Module_declarations of  Parsetree.module_declaration list
 *   | Class_descriptions of Parsetree.class_description list
 *   | Class_type_declarations of Parsetree.class_type_declaration list
 *   | Value_bindings of Parsetree.value_binding list
 *   | Module_bindings of Parsetree.module_binding list
 *   | Class_declarations of Parsetree.class_declaration list
 * 
 * 
 * type 'a zipelement =
 *   | Single : t -> t zipelement
 *   | Multi : m -> m zipelement
 * 
 * type ('a,'b) zipint =
 *   | Root : (t,t) zipint
 *   (\* given an element of type t, and
 *      an elemnt of type 'a with parent 'b,
 *      return an element of type t with parent 'a
 *   *\)
 *   | SingleParent : 'a zipelement * (t, 'b) zipint -> ('a,t) zipint
 *   (\* given an element of type t, and
 *      an elemnt of type m with parent 'b,
 *      return an element of type t with parent m
 *   *\)
 *   | MultipleParent : 'a zipelement * int * (m,'b) zipint -> ('a,m) zipint
 * 
 * type zipper =
 *   | MkZipper : ('a,'b) zipint -> zipper
 * 
 * (\* tests whether the current item is at the root *\)
 * let at_root zip = let (MkZipper zipper) = zip in
 *   match zipper with
 *   | SingleParent (_,Root) -> true
 *   | _ -> false
 * 
 * (\* moves the zipper upwards *\)
 * let go_up (zip: zipper) : zipper option =
 *   let (MkZipper zipper) = zip in
 *   match zipper with
 *   | Root | SingleParent (_, Root) -> None
 *   | SingleParent (_, parent) -> Some (MkZipper parent)
 *   | MultipleParent (_, _, parent) -> Some (MkZipper parent)
 * 
 * (\* moves the zipper downwards *\)
 * let go_down (zip: zipper) : zipper option =
 *   let (MkZipper zipper) = zip in
 *   match zipper with
 *   | Root -> None
 *   | SingleParent (element, _) ->
 *     begin match element with
 *       | Single value -> (match value with
 *           | Attr {  attr_payload; _ } ->  (match attr_payload with
 *               | Parsetree.PStr str -> Some (MkZipper (SingleParent (Multi (Structure str), zipper)))
 *               | Parsetree.PSig sign -> Some (MkZipper (SingleParent (Multi (Signature sign), zipper)))
 *               | Parsetree.PTyp { ptyp_desc; _  } ->
 *                 (match ptyp_desc with
 *                  | Parsetree.Ptyp_alias (ty, _) ->
 *                    Some (MkZipper (SingleParent (Single (CoreTyp ty), zipper)))
 *                  | Parsetree.Ptyp_poly (_, ty) ->
 *                    Some (MkZipper (SingleParent (Single (CoreTyp ty), zipper)))
 *                  | Parsetree.Ptyp_extension ext -> 
 *                    Some (MkZipper (SingleParent (Single (Extension ext), zipper)))
 *                  | _ -> None
 *                 )
 *               | Parsetree.PPat ({ ppat_desc; _ }, _) ->
 *                 (match ppat_desc with
 *                  | Parsetree.Ppat_alias (ty, _) ->
 *                    Some (MkZipper (SingleParent (Single (Pat ty), zipper)))
 *                  | Parsetree.Ppat_extension ext -> 
 *                    Some (MkZipper (SingleParent (Single (Extension ext), zipper)))
 *                  | _ -> None
 *                 )
 *             )
 *           | Case { pc_rhs; _ } -> 
 *             Some (MkZipper (SingleParent (Single (Expr pc_rhs), zipper)))
 *           | Class_field { pcf_desc; _ } ->
 *             (match pcf_desc with
 *              | Parsetree.Pcf_val (_, _, cfk)
 *              | Parsetree.Pcf_method (_, _, cfk) 
 *                ->
 *                (match cfk with
 *                 | Parsetree.Cfk_virtual coretype ->
 *                   Some (MkZipper (SingleParent (Single (CoreTyp coretype), zipper)))
 *                 | Parsetree.Cfk_concrete (_, expr) ->
 *                   Some (MkZipper (SingleParent (Single (Expr expr), zipper)))
 *                )
 * 
 *              | Parsetree.Pcf_initializer expr -> 
 *                   Some (MkZipper (SingleParent (Single (Expr expr), zipper)))
 *              | Parsetree.Pcf_attribute attr -> 
 *                   Some (MkZipper (SingleParent (Single (Attr attr), zipper)))
 *              | Parsetree.Pcf_extension ext -> 
 *                Some (MkZipper (SingleParent (Single (Extension ext), zipper)))
 *              | _ -> None)
 *           | Class_type_field { pctf_desc; _ } ->
 *             (match pctf_desc with
 *              | Parsetree.Pctf_inherit ct ->
 *                Some (MkZipper (SingleParent (Single (Class_type ct), zipper)))
 *              | Parsetree.Pctf_val (_, _, _, ct) 
 *              | Parsetree.Pctf_method (_, _, _, ct) -> 
 *                   Some (MkZipper (SingleParent (Single (CoreTyp ct), zipper)))
 *              | Parsetree.Pctf_attribute attr -> 
 *                   Some (MkZipper (SingleParent (Single (Attr attr), zipper)))
 *              | Parsetree.Pctf_extension ext -> 
 *                Some (MkZipper (SingleParent (Single (Extension ext), zipper)))
 *              | _ -> None)
 *           | Signature_item { psig_desc; _ } -> (match psig_desc with
 *               | Parsetree.Psig_value {  pval_type = ct; _ } -> 
 *                   Some (MkZipper (SingleParent (Single (CoreTyp ct), zipper)))
 * 
 *               (\* | Parsetree.Psig_type (_, _) -> (??) *\)
 *               | Parsetree.Psig_typesubst subs ->
 *                 Some (MkZipper (SingleParent (Multi (Type_declarations subs),
 *                                               zipper)))
 *               (\* | Parsetree.Psig_typext _ -> (??) *\)
 *               | Parsetree.Psig_exception tex -> 
 *                   Some (MkZipper (SingleParent (Single (Type_exception tex), zipper)))
 *               | Parsetree.Psig_module {pmd_type; _} -> 
 *                 Some (MkZipper (SingleParent
 *                                   (Single (Module_type pmd_type), zipper)))
 *               | Parsetree.Psig_modsubst _ -> None
 *               | Parsetree.Psig_recmodule ls -> 
 *                 Some (MkZipper (SingleParent
 *                                   (Multi (Module_declarations ls), zipper)))
 *               | Parsetree.Psig_modtype {pmtd_type; _} ->
 *                 Option.map pmtd_type ~f: begin fun pmd_type ->
 *                   (MkZipper (SingleParent
 *                                   (Single (Module_type pmd_type), zipper)))
 *                 end
 *               | Parsetree.Psig_open _ -> None
 *               | Parsetree.Psig_include { pincl_mod; _ } ->
 *                   Some (MkZipper (SingleParent
 *                                   (Single (Module_type pincl_mod), zipper)))
 *               | Parsetree.Psig_class clsd -> 
 *                   Some (MkZipper (SingleParent
 *                                   (Multi (Class_descriptions clsd), zipper)))
 * 
 *               | Parsetree.Psig_class_type ctdl -> 
 *                   Some (MkZipper (SingleParent
 *                                   (Multi (Class_type_declarations ctdl), zipper)))
 *               | Parsetree.Psig_attribute attr ->
 *                 Some (MkZipper (SingleParent
 *                                   (Single (Attr attr), zipper)))                
 *               | Parsetree.Psig_extension (_, attrs) -> 
 *                 Some (MkZipper (SingleParent
 *                                   (Multi (Attrs attrs), zipper)))
 *               | _ -> None)
 *           | Structure_item { pstr_desc; _ } ->
 *             (match pstr_desc with
 *              | Parsetree.Pstr_eval (expr, _) -> 
 *                   Some (MkZipper (SingleParent (Single (Expr expr), zipper)))
 *              | Parsetree.Pstr_value (_, vblist) -> 
 *                Some (MkZipper (SingleParent (Multi (Value_bindings vblist), zipper)))
 * 
 *              | Parsetree.Pstr_type (_, tdls) -> 
 *                Some (MkZipper (SingleParent (Multi (Type_declarations tdls), zipper)))
 * 
 *              | Parsetree.Pstr_typext _ -> (??)
 *              | Parsetree.Pstr_exception exp -> 
 *                Some (MkZipper (SingleParent (Single (Type_exception exp), zipper)))
 *              | Parsetree.Pstr_module mbd -> 
 *                Some (MkZipper (SingleParent (Single (Module_binding mbd), zipper)))
 *              | Parsetree.Pstr_recmodule mbds -> 
 *                Some (MkZipper (SingleParent (Multi (Module_bindings mbds), zipper)))
 *              | Parsetree.Pstr_modtype mdty -> 
 *                Some (MkZipper (SingleParent
 *                                  (Single (Module_type_declaration mdty), zipper)))
 *              | Parsetree.Pstr_open odl ->
 *                Some (MkZipper (SingleParent
 *                                  (Single (Open_declaration odl), zipper)))
 *              | Parsetree.Pstr_class cdls -> 
 *                Some (MkZipper (SingleParent
 *                                  (Multi (Class_declarations cdls), zipper)))
 *              | Parsetree.Pstr_class_type ctdls -> 
 *                Some (MkZipper (SingleParent
 *                                  (Multi (Class_type_declarations ctdls), zipper)))
 *              | Parsetree.Pstr_include incl -> 
 *                Some (MkZipper (SingleParent
 *                                  (Single (Include_declaration incl), zipper)))
 *              | Parsetree.Pstr_attribute attr -> 
 *                Some (MkZipper (SingleParent
 *                                  (Single (Attr attr), zipper)))
 *              | Parsetree.Pstr_extension (ext,_) -> 
 *                Some (MkZipper (SingleParent
 *                                  (Single (Extension ext), zipper)))
 *              | Parsetree.Pstr_primitive vd -> 
 *                Some (MkZipper (SingleParent
 *                                  (Single (Value_description vd), zipper)))
 *             )
 *           | Type_kind _ -> (??)
 *           | BindingOp _ -> (??)
 *           | ClassDeclaration _ -> (??)
 *           | ClassDescription _ -> (??)
 *           | ClassExpr _ -> (??)
 *           | Class_type _ -> (??)
 *           | Class_type_declaration _ -> (??)
 *           | Constructor_declaration _ -> (??)
 *           | Expr _ -> (??)
 *           | Extension _ -> (??)
 *           | Extension_constructor _ -> (??)
 *           | Include_declaration _ -> (??)
 *           | Include_description _ -> (??)
 *           | Label_declaration _ -> (??)
 *           | Module_binding _ -> (??)
 *           | Module_declaration _ -> (??)
 *           | Module_substitution _ -> (??)
 *           | Module_expr _ -> (??)
 *           | Module_type _ -> (??)
 *           | Module_type_declaration _ -> (??)
 *           | Open_declaration _ -> (??)
 *           | Open_description _ -> (??)
 *           | Pat _ -> (??)
 *           | Payload _ -> (??)
 *           | CoreTyp _ -> (??)
 *           | Row_field _ -> (??)
 *           | Object_field _ -> (??)
 *           | Type_declaration _ -> (??)
 *           | Type_extension _ -> (??)
 *           | Type_exception _ -> (??)
 *           | Value_binding _ -> (??)
 *           | Value_description _ -> (??)
 *           | With_constraint _ -> (??))
 *      | Multi _ -> assert false
 *     end
 *   | MultipleParent (_, _, _) -> assert false *)




