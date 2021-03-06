module Parsetree = Migrate_parsetree.Ast_412.Parsetree
module Asttypes = Migrate_parsetree.Ast_412.Asttypes
(* Pretty much all of this file has been copied from the OCaml repository *)
(* Corresponding licenses for each file included at the start of each module *)
module Docstrings = struct
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                               Leo White                                *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Location

(* Docstrings *)

(* A docstring is "attached" if it has been inserted in the AST. This
   is used for generating unexpected docstring warnings. *)
type ds_attached =
  | Unattached   (* Not yet attached anything.*)
  | Info         (* Attached to a field or constructor. *)
  | Docs         (* Attached to an item or as floating text. *)

(* A docstring is "associated" with an item if there are no blank lines between
   them. This is used for generating docstring ambiguity warnings. *)
type ds_associated =
  | Zero             (* Not associated with an item *)
  | One              (* Associated with one item *)
  | Many             (* Associated with multiple items (ambiguity) *)

type docstring =
  { ds_body: string;
    ds_loc: Location.t;
    mutable ds_attached: ds_attached;
    mutable ds_associated: ds_associated; }

(* List of docstrings *)

let docstrings : docstring list ref = ref []

(* Warn for unused and ambiguous docstrings *)

let warn_bad_docstrings () = ()


(* Docstring constructors and destructors *)

let docstring body loc =
  let ds =
    { ds_body = body;
      ds_loc = loc;
      ds_attached = Unattached;
      ds_associated = Zero; }
  in
  ds

let register ds =
  docstrings := ds :: !docstrings

let docstring_body ds = ds.ds_body

let docstring_loc ds = ds.ds_loc

(* Docstrings attached to items *)

type docs =
  { docs_pre: docstring option;
    docs_post: docstring option; }

let empty_docs = { docs_pre = None; docs_post = None }

let doc_loc = {txt = "ocaml.doc"; loc = Location.none}

let docs_attr ds =
  let open Parsetree in
  let body = ds.ds_body in
  let loc = ds.ds_loc in
  let exp =
    { pexp_desc = Pexp_constant (Pconst_string(body, loc, None));
      pexp_loc = loc;
      pexp_loc_stack = [];
      pexp_attributes = []; }
  in
  let item =
    { pstr_desc = Pstr_eval (exp, []); pstr_loc = loc }
  in
  { attr_name = doc_loc;
    attr_payload = PStr [item];
    attr_loc = loc }

let add_docs_attrs docs attrs =
  let attrs =
    match docs.docs_pre with
    | None | Some { ds_body=""; _ } -> attrs
    | Some ds -> docs_attr ds :: attrs
  in
  let attrs =
    match docs.docs_post with
    | None | Some { ds_body=""; _ } -> attrs
    | Some ds -> attrs @ [docs_attr ds]
  in
  attrs

(* Docstrings attached to constructors or fields *)

type info = docstring option

let empty_info = None

let info_attr = docs_attr

let add_info_attrs info attrs =
  match info with
  | None | Some {ds_body=""; _} -> attrs
  | Some ds -> attrs @ [info_attr ds]

(* Docstrings not attached to a specific item *)

type text = docstring list

let empty_text = []
let empty_text_lazy = lazy []

let text_loc = {txt = "ocaml.text"; loc = Location.none}

let text_attr ds =
  let open Parsetree in
  let body = ds.ds_body in
  let loc = ds.ds_loc in
  let exp =
    { pexp_desc = Pexp_constant (Pconst_string(body, loc, None));
      pexp_loc = loc;
      pexp_loc_stack = [];
      pexp_attributes = []; }
  in
  let item =
    { pstr_desc = Pstr_eval (exp, []); pstr_loc = loc }
  in
  { attr_name = text_loc;
    attr_payload = PStr [item];
    attr_loc = loc }

let add_text_attrs dsl attrs =
  let fdsl = List.filter (function {ds_body=""} -> false| _ ->true) dsl in
  (List.map text_attr fdsl) @ attrs

(* Find the first non-info docstring in a list, attach it and return it *)
let get_docstring ~info dsl =
  let rec loop = function
    | [] -> None
    | {ds_attached = Info; _} :: rest -> loop rest
    | ds :: _ ->
        ds.ds_attached <- if info then Info else Docs;
        Some ds
  in
  loop dsl

(* Find all the non-info docstrings in a list, attach them and return them *)
let get_docstrings dsl =
  let rec loop acc = function
    | [] -> List.rev acc
    | {ds_attached = Info; _} :: rest -> loop acc rest
    | ds :: rest ->
        ds.ds_attached <- Docs;
        loop (ds :: acc) rest
  in
    loop [] dsl

(* "Associate" all the docstrings in a list *)
let associate_docstrings dsl =
  List.iter
    (fun ds ->
       match ds.ds_associated with
       | Zero -> ds.ds_associated <- One
       | (One | Many) -> ds.ds_associated <- Many)
    dsl

(* Map from positions to pre docstrings *)

let pre_table : (Lexing.position, docstring list) Hashtbl.t =
  Hashtbl.create 50

let set_pre_docstrings pos dsl =
  if dsl <> [] then Hashtbl.add pre_table pos dsl

let get_pre_docs pos =
  try
    let dsl = Hashtbl.find pre_table pos in
      associate_docstrings dsl;
      get_docstring ~info:false dsl
  with Not_found -> None

let mark_pre_docs pos =
  try
    let dsl = Hashtbl.find pre_table pos in
      associate_docstrings dsl
  with Not_found -> ()

(* Map from positions to post docstrings *)

let post_table : (Lexing.position, docstring list) Hashtbl.t =
  Hashtbl.create 50

let set_post_docstrings pos dsl =
  if dsl <> [] then Hashtbl.add post_table pos dsl

let get_post_docs pos =
  try
    let dsl = Hashtbl.find post_table pos in
      associate_docstrings dsl;
      get_docstring ~info:false dsl
  with Not_found -> None

let mark_post_docs pos =
  try
    let dsl = Hashtbl.find post_table pos in
      associate_docstrings dsl
  with Not_found -> ()

let get_info pos =
  try
    let dsl = Hashtbl.find post_table pos in
      get_docstring ~info:true dsl
  with Not_found -> None

(* Map from positions to floating docstrings *)

let floating_table : (Lexing.position, docstring list) Hashtbl.t =
  Hashtbl.create 50

let set_floating_docstrings pos dsl =
  if dsl <> [] then Hashtbl.add floating_table pos dsl

let get_text pos =
  try
    let dsl = Hashtbl.find floating_table pos in
      get_docstrings dsl
  with Not_found -> []

let get_post_text pos =
  try
    let dsl = Hashtbl.find post_table pos in
      get_docstrings dsl
  with Not_found -> []

(* Maps from positions to extra docstrings *)

let pre_extra_table : (Lexing.position, docstring list) Hashtbl.t =
  Hashtbl.create 50

let set_pre_extra_docstrings pos dsl =
  if dsl <> [] then Hashtbl.add pre_extra_table pos dsl

let get_pre_extra_text pos =
  try
    let dsl = Hashtbl.find pre_extra_table pos in
      get_docstrings dsl
  with Not_found -> []

let post_extra_table : (Lexing.position, docstring list) Hashtbl.t =
  Hashtbl.create 50

let set_post_extra_docstrings pos dsl =
  if dsl <> [] then Hashtbl.add post_extra_table pos dsl

let get_post_extra_text pos =
  try
    let dsl = Hashtbl.find post_extra_table pos in
      get_docstrings dsl
  with Not_found -> []

(* Docstrings from parser actions *)
module WithParsing = struct
let symbol_docs () =
  { docs_pre = get_pre_docs (Parsing.symbol_start_pos ());
    docs_post = get_post_docs (Parsing.symbol_end_pos ()); }

let symbol_docs_lazy () =
  let p1 = Parsing.symbol_start_pos () in
  let p2 = Parsing.symbol_end_pos () in
    lazy { docs_pre = get_pre_docs p1;
           docs_post = get_post_docs p2; }

let rhs_docs pos1 pos2 =
  { docs_pre = get_pre_docs (Parsing.rhs_start_pos pos1);
    docs_post = get_post_docs (Parsing.rhs_end_pos pos2); }

let rhs_docs_lazy pos1 pos2 =
  let p1 = Parsing.rhs_start_pos pos1 in
  let p2 = Parsing.rhs_end_pos pos2 in
    lazy { docs_pre = get_pre_docs p1;
           docs_post = get_post_docs p2; }

let mark_symbol_docs () =
  mark_pre_docs (Parsing.symbol_start_pos ());
  mark_post_docs (Parsing.symbol_end_pos ())

let mark_rhs_docs pos1 pos2 =
  mark_pre_docs (Parsing.rhs_start_pos pos1);
  mark_post_docs (Parsing.rhs_end_pos pos2)

let symbol_info () =
  get_info (Parsing.symbol_end_pos ())

let rhs_info pos =
  get_info (Parsing.rhs_end_pos pos)

let symbol_text () =
  get_text (Parsing.symbol_start_pos ())

let symbol_text_lazy () =
  let pos = Parsing.symbol_start_pos () in
    lazy (get_text pos)

let rhs_text pos =
  get_text (Parsing.rhs_start_pos pos)

let rhs_post_text pos =
  get_post_text (Parsing.rhs_end_pos pos)

let rhs_text_lazy pos =
  let pos = Parsing.rhs_start_pos pos in
    lazy (get_text pos)

let symbol_pre_extra_text () =
  get_pre_extra_text (Parsing.symbol_start_pos ())

let symbol_post_extra_text () =
  get_post_extra_text (Parsing.symbol_end_pos ())

let rhs_pre_extra_text pos =
  get_pre_extra_text (Parsing.rhs_start_pos pos)

let rhs_post_extra_text pos =
  get_post_extra_text (Parsing.rhs_end_pos pos)
end

include WithParsing

module WithMenhir = struct
let symbol_docs (startpos, endpos) =
  { docs_pre = get_pre_docs startpos;
    docs_post = get_post_docs endpos; }

let symbol_docs_lazy (p1, p2) =
  lazy { docs_pre = get_pre_docs p1;
         docs_post = get_post_docs p2; }

let rhs_docs pos1 pos2 =
  { docs_pre = get_pre_docs pos1;
    docs_post = get_post_docs pos2; }

let rhs_docs_lazy p1 p2 =
    lazy { docs_pre = get_pre_docs p1;
           docs_post = get_post_docs p2; }

let mark_symbol_docs (startpos, endpos) =
  mark_pre_docs startpos;
  mark_post_docs endpos;
  ()

let mark_rhs_docs pos1 pos2 =
  mark_pre_docs pos1;
  mark_post_docs pos2;
  ()

let symbol_info endpos =
  get_info endpos

let rhs_info endpos =
  get_info endpos

let symbol_text startpos =
  get_text startpos

let symbol_text_lazy startpos =
  lazy (get_text startpos)

let rhs_text pos =
  get_text pos

let rhs_post_text pos =
  get_post_text pos

let rhs_text_lazy pos =
  lazy (get_text pos)

let symbol_pre_extra_text startpos =
  get_pre_extra_text startpos

let symbol_post_extra_text endpos =
  get_post_extra_text endpos

let rhs_pre_extra_text pos =
  get_pre_extra_text pos

let rhs_post_extra_text pos =
  get_post_extra_text pos
end

(* (Re)Initialise all comment state *)

let init () =
  docstrings := [];
  Hashtbl.reset pre_table;
  Hashtbl.reset post_table;
  Hashtbl.reset floating_table;
  Hashtbl.reset pre_extra_table;
  Hashtbl.reset post_extra_table
end

module Ast_helper = struct
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                         Alain Frisch, LexiFi                           *)
(*                                                                        *)
(*   Copyright 2012 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)
(** Helpers to produce Parsetree fragments *)

open Asttypes
open Parsetree
open Docstrings

type 'a with_loc = 'a Location.loc
type loc = Location.t

type lid = Longident.t with_loc
type str = string with_loc
type str_opt = string option with_loc
type attrs = attribute list

let default_loc = ref Location.none

let with_default_loc l f =
  Misc.protect_refs [Misc.R (default_loc, l)] f

module Const = struct
  let integer ?suffix i = Pconst_integer (i, suffix)
  let int32 ?(suffix='l') i = integer ~suffix (Int32.to_string i)
  let int64 ?(suffix='L') i = integer ~suffix (Int64.to_string i)
  let nativeint ?(suffix='n') i = integer ~suffix (Nativeint.to_string i)
  let float ?suffix f = Pconst_float (f, suffix)
  let char c = Pconst_char c
  let string ?quotation_delimiter ?(loc= !default_loc) s =
    Pconst_string (s, loc, quotation_delimiter)
end

module Attr = struct
  let mk ?(loc= !default_loc) name payload =
    { attr_name = name;
      attr_payload = payload;
      attr_loc = loc }
end

module Typ = struct
  let mk ?(loc = !default_loc) ?(attrs = []) d =
    {ptyp_desc = d;
     ptyp_loc = loc;
     ptyp_loc_stack = [];
     ptyp_attributes = attrs}

  let attr d a = {d with ptyp_attributes = d.ptyp_attributes @ [a]}

  let any ?loc ?attrs () = mk ?loc ?attrs Ptyp_any
  let var ?loc ?attrs a = mk ?loc ?attrs (Ptyp_var a)
  let arrow ?loc ?attrs a b c = mk ?loc ?attrs (Ptyp_arrow (a, b, c))
  let tuple ?loc ?attrs a = mk ?loc ?attrs (Ptyp_tuple a)
  let constr ?loc ?attrs a b = mk ?loc ?attrs (Ptyp_constr (a, b))
  let object_ ?loc ?attrs a b = mk ?loc ?attrs (Ptyp_object (a, b))
  let class_ ?loc ?attrs a b = mk ?loc ?attrs (Ptyp_class (a, b))
  let alias ?loc ?attrs a b = mk ?loc ?attrs (Ptyp_alias (a, b))
  let variant ?loc ?attrs a b c = mk ?loc ?attrs (Ptyp_variant (a, b, c))
  let poly ?loc ?attrs a b = mk ?loc ?attrs (Ptyp_poly (a, b))
  let package ?loc ?attrs a b = mk ?loc ?attrs (Ptyp_package (a, b))
  let extension ?loc ?attrs a = mk ?loc ?attrs (Ptyp_extension a)

  let force_poly t =
    match t.ptyp_desc with
    | Ptyp_poly _ -> t
    | _ -> poly ~loc:t.ptyp_loc [] t (* -> ghost? *)

  let varify_constructors var_names t =
    let check_variable vl loc v =
      if List.mem v vl then
        raise Syntaxerr.(Error(Variable_in_scope(loc,v))) in
    let var_names = List.map (fun v -> v.txt) var_names in
    let rec loop t =
      let desc =
        match t.ptyp_desc with
        | Ptyp_any -> Ptyp_any
        | Ptyp_var x ->
            check_variable var_names t.ptyp_loc x;
            Ptyp_var x
        | Ptyp_arrow (label,core_type,core_type') ->
            Ptyp_arrow(label, loop core_type, loop core_type')
        | Ptyp_tuple lst -> Ptyp_tuple (List.map loop lst)
        | Ptyp_constr( { txt = Longident.Lident s }, [])
          when List.mem s var_names ->
            Ptyp_var s
        | Ptyp_constr(longident, lst) ->
            Ptyp_constr(longident, List.map loop lst)
        | Ptyp_object (lst, o) ->
            Ptyp_object (List.map loop_object_field lst, o)
        | Ptyp_class (longident, lst) ->
            Ptyp_class (longident, List.map loop lst)
        | Ptyp_alias(core_type, string) ->
            check_variable var_names t.ptyp_loc string;
            Ptyp_alias(loop core_type, string)
        | Ptyp_variant(row_field_list, flag, lbl_lst_option) ->
            Ptyp_variant(List.map loop_row_field row_field_list,
                         flag, lbl_lst_option)
        | Ptyp_poly(string_lst, core_type) ->
          List.iter (fun v ->
            check_variable var_names t.ptyp_loc v.txt) string_lst;
            Ptyp_poly(string_lst, loop core_type)
        | Ptyp_package(longident,lst) ->
            Ptyp_package(longident,List.map (fun (n,typ) -> (n,loop typ) ) lst)
        | Ptyp_extension (s, arg) ->
            Ptyp_extension (s, arg)
      in
      {t with ptyp_desc = desc}
    and loop_row_field field =
      let prf_desc = match field.prf_desc with
        | Rtag(label,flag,lst) ->
            Rtag(label,flag,List.map loop lst)
        | Rinherit t ->
            Rinherit (loop t)
      in
      { field with prf_desc; }
    and loop_object_field field =
      let pof_desc = match field.pof_desc with
        | Otag(label, t) ->
            Otag(label, loop t)
        | Oinherit t ->
            Oinherit (loop t)
      in
      { field with pof_desc; }
    in
    loop t

end

module Pat = struct
  let mk ?(loc = !default_loc) ?(attrs = []) d =
    {ppat_desc = d;
     ppat_loc = loc;
     ppat_loc_stack = [];
     ppat_attributes = attrs}
  let attr d a = {d with ppat_attributes = d.ppat_attributes @ [a]}

  let any ?loc ?attrs () = mk ?loc ?attrs Ppat_any
  let var ?loc ?attrs a = mk ?loc ?attrs (Ppat_var a)
  let alias ?loc ?attrs a b = mk ?loc ?attrs (Ppat_alias (a, b))
  let constant ?loc ?attrs a = mk ?loc ?attrs (Ppat_constant a)
  let interval ?loc ?attrs a b = mk ?loc ?attrs (Ppat_interval (a, b))
  let tuple ?loc ?attrs a = mk ?loc ?attrs (Ppat_tuple a)
  let construct ?loc ?attrs a b = mk ?loc ?attrs (Ppat_construct (a, b))
  let variant ?loc ?attrs a b = mk ?loc ?attrs (Ppat_variant (a, b))
  let record ?loc ?attrs a b = mk ?loc ?attrs (Ppat_record (a, b))
  let array ?loc ?attrs a = mk ?loc ?attrs (Ppat_array a)
  let or_ ?loc ?attrs a b = mk ?loc ?attrs (Ppat_or (a, b))
  let constraint_ ?loc ?attrs a b = mk ?loc ?attrs (Ppat_constraint (a, b))
  let type_ ?loc ?attrs a = mk ?loc ?attrs (Ppat_type a)
  let lazy_ ?loc ?attrs a = mk ?loc ?attrs (Ppat_lazy a)
  let unpack ?loc ?attrs a = mk ?loc ?attrs (Ppat_unpack a)
  let open_ ?loc ?attrs a b = mk ?loc ?attrs (Ppat_open (a, b))
  let exception_ ?loc ?attrs a = mk ?loc ?attrs (Ppat_exception a)
  let extension ?loc ?attrs a = mk ?loc ?attrs (Ppat_extension a)
end

module Exp = struct
  let mk ?(loc = !default_loc) ?(attrs = []) d =
    {pexp_desc = d;
     pexp_loc = loc;
     pexp_loc_stack = [];
     pexp_attributes = attrs}
  let attr d a = {d with pexp_attributes = d.pexp_attributes @ [a]}

  let ident ?loc ?attrs a = mk ?loc ?attrs (Pexp_ident a)
  let constant ?loc ?attrs a = mk ?loc ?attrs (Pexp_constant a)
  let let_ ?loc ?attrs a b c = mk ?loc ?attrs (Pexp_let (a, b, c))
  let fun_ ?loc ?attrs a b c d = mk ?loc ?attrs (Pexp_fun (a, b, c, d))
  let function_ ?loc ?attrs a = mk ?loc ?attrs (Pexp_function a)
  let apply ?loc ?attrs a b = mk ?loc ?attrs (Pexp_apply (a, b))
  let match_ ?loc ?attrs a b = mk ?loc ?attrs (Pexp_match (a, b))
  let try_ ?loc ?attrs a b = mk ?loc ?attrs (Pexp_try (a, b))
  let tuple ?loc ?attrs a = mk ?loc ?attrs (Pexp_tuple a)
  let construct ?loc ?attrs a b = mk ?loc ?attrs (Pexp_construct (a, b))
  let variant ?loc ?attrs a b = mk ?loc ?attrs (Pexp_variant (a, b))
  let record ?loc ?attrs a b = mk ?loc ?attrs (Pexp_record (a, b))
  let field ?loc ?attrs a b = mk ?loc ?attrs (Pexp_field (a, b))
  let setfield ?loc ?attrs a b c = mk ?loc ?attrs (Pexp_setfield (a, b, c))
  let array ?loc ?attrs a = mk ?loc ?attrs (Pexp_array a)
  let ifthenelse ?loc ?attrs a b c = mk ?loc ?attrs (Pexp_ifthenelse (a, b, c))
  let sequence ?loc ?attrs a b = mk ?loc ?attrs (Pexp_sequence (a, b))
  let while_ ?loc ?attrs a b = mk ?loc ?attrs (Pexp_while (a, b))
  let for_ ?loc ?attrs a b c d e = mk ?loc ?attrs (Pexp_for (a, b, c, d, e))
  let constraint_ ?loc ?attrs a b = mk ?loc ?attrs (Pexp_constraint (a, b))
  let coerce ?loc ?attrs a b c = mk ?loc ?attrs (Pexp_coerce (a, b, c))
  let send ?loc ?attrs a b = mk ?loc ?attrs (Pexp_send (a, b))
  let new_ ?loc ?attrs a = mk ?loc ?attrs (Pexp_new a)
  let setinstvar ?loc ?attrs a b = mk ?loc ?attrs (Pexp_setinstvar (a, b))
  let override ?loc ?attrs a = mk ?loc ?attrs (Pexp_override a)
  let letmodule ?loc ?attrs a b c= mk ?loc ?attrs (Pexp_letmodule (a, b, c))
  let letexception ?loc ?attrs a b = mk ?loc ?attrs (Pexp_letexception (a, b))
  let assert_ ?loc ?attrs a = mk ?loc ?attrs (Pexp_assert a)
  let lazy_ ?loc ?attrs a = mk ?loc ?attrs (Pexp_lazy a)
  let poly ?loc ?attrs a b = mk ?loc ?attrs (Pexp_poly (a, b))
  let object_ ?loc ?attrs a = mk ?loc ?attrs (Pexp_object a)
  let newtype ?loc ?attrs a b = mk ?loc ?attrs (Pexp_newtype (a, b))
  let pack ?loc ?attrs a = mk ?loc ?attrs (Pexp_pack a)
  let open_ ?loc ?attrs a b = mk ?loc ?attrs (Pexp_open (a, b))
  let letop ?loc ?attrs let_ ands body =
    mk ?loc ?attrs (Pexp_letop {let_; ands; body})
  let extension ?loc ?attrs a = mk ?loc ?attrs (Pexp_extension a)
  let unreachable ?loc ?attrs () = mk ?loc ?attrs Pexp_unreachable

  let case lhs ?guard rhs =
    {
     pc_lhs = lhs;
     pc_guard = guard;
     pc_rhs = rhs;
    }

  let binding_op op pat exp loc =
    {
      pbop_op = op;
      pbop_pat = pat;
      pbop_exp = exp;
      pbop_loc = loc;
    }
end

module Mty = struct
  let mk ?(loc = !default_loc) ?(attrs = []) d =
    {pmty_desc = d; pmty_loc = loc; pmty_attributes = attrs}
  let attr d a = {d with pmty_attributes = d.pmty_attributes @ [a]}

  let ident ?loc ?attrs a = mk ?loc ?attrs (Pmty_ident a)
  let alias ?loc ?attrs a = mk ?loc ?attrs (Pmty_alias a)
  let signature ?loc ?attrs a = mk ?loc ?attrs (Pmty_signature a)
  let functor_ ?loc ?attrs a b = mk ?loc ?attrs (Pmty_functor (a, b))
  let with_ ?loc ?attrs a b = mk ?loc ?attrs (Pmty_with (a, b))
  let typeof_ ?loc ?attrs a = mk ?loc ?attrs (Pmty_typeof a)
  let extension ?loc ?attrs a = mk ?loc ?attrs (Pmty_extension a)
end

module Mod = struct
let mk ?(loc = !default_loc) ?(attrs = []) d =
  {pmod_desc = d; pmod_loc = loc; pmod_attributes = attrs}
  let attr d a = {d with pmod_attributes = d.pmod_attributes @ [a]}

  let ident ?loc ?attrs x = mk ?loc ?attrs (Pmod_ident x)
  let structure ?loc ?attrs x = mk ?loc ?attrs (Pmod_structure x)
  let functor_ ?loc ?attrs arg body =
    mk ?loc ?attrs (Pmod_functor (arg, body))
  let apply ?loc ?attrs m1 m2 = mk ?loc ?attrs (Pmod_apply (m1, m2))
  let constraint_ ?loc ?attrs m mty = mk ?loc ?attrs (Pmod_constraint (m, mty))
  let unpack ?loc ?attrs e = mk ?loc ?attrs (Pmod_unpack e)
  let extension ?loc ?attrs a = mk ?loc ?attrs (Pmod_extension a)
end

module Sig = struct
  let mk ?(loc = !default_loc) d = {psig_desc = d; psig_loc = loc}

  let value ?loc a = mk ?loc (Psig_value a)
  let type_ ?loc rec_flag a = mk ?loc (Psig_type (rec_flag, a))
  let type_subst ?loc a = mk ?loc (Psig_typesubst a)
  let type_extension ?loc a = mk ?loc (Psig_typext a)
  let exception_ ?loc a = mk ?loc (Psig_exception a)
  let module_ ?loc a = mk ?loc (Psig_module a)
  let mod_subst ?loc a = mk ?loc (Psig_modsubst a)
  let rec_module ?loc a = mk ?loc (Psig_recmodule a)
  let modtype ?loc a = mk ?loc (Psig_modtype a)
  let open_ ?loc a = mk ?loc (Psig_open a)
  let include_ ?loc a = mk ?loc (Psig_include a)
  let class_ ?loc a = mk ?loc (Psig_class a)
  let class_type ?loc a = mk ?loc (Psig_class_type a)
  let extension ?loc ?(attrs = []) a = mk ?loc (Psig_extension (a, attrs))
  let attribute ?loc a = mk ?loc (Psig_attribute a)
  let text txt =
    let f_txt = List.filter (fun ds -> docstring_body ds <> "") txt in
    List.map
      (fun ds -> attribute ~loc:(docstring_loc ds) (text_attr ds))
      f_txt
end

module Str = struct
  let mk ?(loc = !default_loc) d = {pstr_desc = d; pstr_loc = loc}

  let eval ?loc ?(attrs = []) a = mk ?loc (Pstr_eval (a, attrs))
  let value ?loc a b = mk ?loc (Pstr_value (a, b))
  let primitive ?loc a = mk ?loc (Pstr_primitive a)
  let type_ ?loc rec_flag a = mk ?loc (Pstr_type (rec_flag, a))
  let type_extension ?loc a = mk ?loc (Pstr_typext a)
  let exception_ ?loc a = mk ?loc (Pstr_exception a)
  let module_ ?loc a = mk ?loc (Pstr_module a)
  let rec_module ?loc a = mk ?loc (Pstr_recmodule a)
  let modtype ?loc a = mk ?loc (Pstr_modtype a)
  let open_ ?loc a = mk ?loc (Pstr_open a)
  let class_ ?loc a = mk ?loc (Pstr_class a)
  let class_type ?loc a = mk ?loc (Pstr_class_type a)
  let include_ ?loc a = mk ?loc (Pstr_include a)
  let extension ?loc ?(attrs = []) a = mk ?loc (Pstr_extension (a, attrs))
  let attribute ?loc a = mk ?loc (Pstr_attribute a)
  let text txt =
    let f_txt = List.filter (fun ds -> docstring_body ds <> "") txt in
    List.map
      (fun ds -> attribute ~loc:(docstring_loc ds) (text_attr ds))
      f_txt
end

module Cl = struct
  let mk ?(loc = !default_loc) ?(attrs = []) d =
    {
     pcl_desc = d;
     pcl_loc = loc;
     pcl_attributes = attrs;
    }
  let attr d a = {d with pcl_attributes = d.pcl_attributes @ [a]}

  let constr ?loc ?attrs a b = mk ?loc ?attrs (Pcl_constr (a, b))
  let structure ?loc ?attrs a = mk ?loc ?attrs (Pcl_structure a)
  let fun_ ?loc ?attrs a b c d = mk ?loc ?attrs (Pcl_fun (a, b, c, d))
  let apply ?loc ?attrs a b = mk ?loc ?attrs (Pcl_apply (a, b))
  let let_ ?loc ?attrs a b c = mk ?loc ?attrs (Pcl_let (a, b, c))
  let constraint_ ?loc ?attrs a b = mk ?loc ?attrs (Pcl_constraint (a, b))
  let extension ?loc ?attrs a = mk ?loc ?attrs (Pcl_extension a)
  let open_ ?loc ?attrs a b = mk ?loc ?attrs (Pcl_open (a, b))
end

module Cty = struct
  let mk ?(loc = !default_loc) ?(attrs = []) d =
    {
     pcty_desc = d;
     pcty_loc = loc;
     pcty_attributes = attrs;
    }
  let attr d a = {d with pcty_attributes = d.pcty_attributes @ [a]}

  let constr ?loc ?attrs a b = mk ?loc ?attrs (Pcty_constr (a, b))
  let signature ?loc ?attrs a = mk ?loc ?attrs (Pcty_signature a)
  let arrow ?loc ?attrs a b c = mk ?loc ?attrs (Pcty_arrow (a, b, c))
  let extension ?loc ?attrs a = mk ?loc ?attrs (Pcty_extension a)
  let open_ ?loc ?attrs a b = mk ?loc ?attrs (Pcty_open (a, b))
end

module Ctf = struct
  let mk ?(loc = !default_loc) ?(attrs = [])
           ?(docs = empty_docs) d =
    {
     pctf_desc = d;
     pctf_loc = loc;
     pctf_attributes = add_docs_attrs docs attrs;
    }

  let inherit_ ?loc ?attrs a = mk ?loc ?attrs (Pctf_inherit a)
  let val_ ?loc ?attrs a b c d = mk ?loc ?attrs (Pctf_val (a, b, c, d))
  let method_ ?loc ?attrs a b c d = mk ?loc ?attrs (Pctf_method (a, b, c, d))
  let constraint_ ?loc ?attrs a b = mk ?loc ?attrs (Pctf_constraint (a, b))
  let extension ?loc ?attrs a = mk ?loc ?attrs (Pctf_extension a)
  let attribute ?loc a = mk ?loc (Pctf_attribute a)
  let text txt =
   let f_txt = List.filter (fun ds -> docstring_body ds <> "") txt in
     List.map
      (fun ds -> attribute ~loc:(docstring_loc ds) (text_attr ds))
      f_txt

  let attr d a = {d with pctf_attributes = d.pctf_attributes @ [a]}

end

module Cf = struct
  let mk ?(loc = !default_loc) ?(attrs = [])
        ?(docs = empty_docs) d =
    {
     pcf_desc = d;
     pcf_loc = loc;
     pcf_attributes = add_docs_attrs docs attrs;
    }

  let inherit_ ?loc ?attrs a b c = mk ?loc ?attrs (Pcf_inherit (a, b, c))
  let val_ ?loc ?attrs a b c = mk ?loc ?attrs (Pcf_val (a, b, c))
  let method_ ?loc ?attrs a b c = mk ?loc ?attrs (Pcf_method (a, b, c))
  let constraint_ ?loc ?attrs a b = mk ?loc ?attrs (Pcf_constraint (a, b))
  let initializer_ ?loc ?attrs a = mk ?loc ?attrs (Pcf_initializer a)
  let extension ?loc ?attrs a = mk ?loc ?attrs (Pcf_extension a)
  let attribute ?loc a = mk ?loc (Pcf_attribute a)
  let text txt =
    let f_txt = List.filter (fun ds -> docstring_body ds <> "") txt in
    List.map
      (fun ds -> attribute ~loc:(docstring_loc ds) (text_attr ds))
      f_txt

  let virtual_ ct = Cfk_virtual ct
  let concrete o e = Cfk_concrete (o, e)

  let attr d a = {d with pcf_attributes = d.pcf_attributes @ [a]}

end

module Val = struct
  let mk ?(loc = !default_loc) ?(attrs = []) ?(docs = empty_docs)
        ?(prim = []) name typ =
    {
     pval_name = name;
     pval_type = typ;
     pval_attributes = add_docs_attrs docs attrs;
     pval_loc = loc;
     pval_prim = prim;
    }
end

module Md = struct
  let mk ?(loc = !default_loc) ?(attrs = [])
        ?(docs = empty_docs) ?(text = []) name typ =
    {
     pmd_name = name;
     pmd_type = typ;
     pmd_attributes =
       add_text_attrs text (add_docs_attrs docs attrs);
     pmd_loc = loc;
    }
end

module Ms = struct
  let mk ?(loc = !default_loc) ?(attrs = [])
        ?(docs = empty_docs) ?(text = []) name syn =
    {
     pms_name = name;
     pms_manifest = syn;
     pms_attributes =
       add_text_attrs text (add_docs_attrs docs attrs);
     pms_loc = loc;
    }
end

module Mtd = struct
  let mk ?(loc = !default_loc) ?(attrs = [])
        ?(docs = empty_docs) ?(text = []) ?typ name =
    {
     pmtd_name = name;
     pmtd_type = typ;
     pmtd_attributes =
       add_text_attrs text (add_docs_attrs docs attrs);
     pmtd_loc = loc;
    }
end

module Mb = struct
  let mk ?(loc = !default_loc) ?(attrs = [])
        ?(docs = empty_docs) ?(text = []) name expr =
    {
     pmb_name = name;
     pmb_expr = expr;
     pmb_attributes =
       add_text_attrs text (add_docs_attrs docs attrs);
     pmb_loc = loc;
    }
end

module Opn = struct
  let mk ?(loc = !default_loc) ?(attrs = []) ?(docs = empty_docs)
        ?(override = Fresh) expr =
    {
     popen_expr = expr;
     popen_override = override;
     popen_loc = loc;
     popen_attributes = add_docs_attrs docs attrs;
    }
end

module Incl = struct
  let mk ?(loc = !default_loc) ?(attrs = []) ?(docs = empty_docs) mexpr =
    {
     pincl_mod = mexpr;
     pincl_loc = loc;
     pincl_attributes = add_docs_attrs docs attrs;
    }

end

module Vb = struct
  let mk ?(loc = !default_loc) ?(attrs = []) ?(docs = empty_docs)
        ?(text = []) pat expr =
    {
     pvb_pat = pat;
     pvb_expr = expr;
     pvb_attributes =
       add_text_attrs text (add_docs_attrs docs attrs);
     pvb_loc = loc;
    }
end

module Ci = struct
  let mk ?(loc = !default_loc) ?(attrs = [])
        ?(docs = empty_docs) ?(text = [])
        ?(virt = Concrete) ?(params = []) name expr =
    {
     pci_virt = virt;
     pci_params = params;
     pci_name = name;
     pci_expr = expr;
     pci_attributes =
       add_text_attrs text (add_docs_attrs docs attrs);
     pci_loc = loc;
    }
end

module Type = struct
  let mk ?(loc = !default_loc) ?(attrs = [])
        ?(docs = empty_docs) ?(text = [])
      ?(params = [])
      ?(cstrs = [])
      ?(kind = Ptype_abstract)
      ?(priv = Public)
      ?manifest
      name =
    {
     ptype_name = name;
     ptype_params = params;
     ptype_cstrs = cstrs;
     ptype_kind = kind;
     ptype_private = priv;
     ptype_manifest = manifest;
     ptype_attributes =
       add_text_attrs text (add_docs_attrs docs attrs);
     ptype_loc = loc;
    }

  let constructor ?(loc = !default_loc) ?(attrs = []) ?(info = empty_info)
        ?(args = Pcstr_tuple []) ?res name =
    {
     pcd_name = name;
     pcd_args = args;
     pcd_res = res;
     pcd_loc = loc;
     pcd_attributes = add_info_attrs info attrs;
    }

  let field ?(loc = !default_loc) ?(attrs = []) ?(info = empty_info)
        ?(mut = Immutable) name typ =
    {
     pld_name = name;
     pld_mutable = mut;
     pld_type = typ;
     pld_loc = loc;
     pld_attributes = add_info_attrs info attrs;
    }

end

(** Type extensions *)
module Te = struct
  let mk ?(loc = !default_loc) ?(attrs = []) ?(docs = empty_docs)
        ?(params = []) ?(priv = Public) path constructors =
    {
     ptyext_path = path;
     ptyext_params = params;
     ptyext_constructors = constructors;
     ptyext_private = priv;
     ptyext_loc = loc;
     ptyext_attributes = add_docs_attrs docs attrs;
    }

  let mk_exception ?(loc = !default_loc) ?(attrs = []) ?(docs = empty_docs)
      constructor =
    {
     ptyexn_constructor = constructor;
     ptyexn_loc = loc;
     ptyexn_attributes = add_docs_attrs docs attrs;
    }

  let constructor ?(loc = !default_loc) ?(attrs = [])
        ?(docs = empty_docs) ?(info = empty_info) name kind =
    {
     pext_name = name;
     pext_kind = kind;
     pext_loc = loc;
     pext_attributes = add_docs_attrs docs (add_info_attrs info attrs);
    }

  let decl ?(loc = !default_loc) ?(attrs = []) ?(docs = empty_docs)
             ?(info = empty_info) ?(args = Pcstr_tuple []) ?res name =
    {
     pext_name = name;
     pext_kind = Pext_decl(args, res);
     pext_loc = loc;
     pext_attributes = add_docs_attrs docs (add_info_attrs info attrs);
    }

  let rebind ?(loc = !default_loc) ?(attrs = [])
        ?(docs = empty_docs) ?(info = empty_info) name lid =
    {
     pext_name = name;
     pext_kind = Pext_rebind lid;
     pext_loc = loc;
     pext_attributes = add_docs_attrs docs (add_info_attrs info attrs);
    }

end

module Csig = struct
  let mk self fields =
    {
     pcsig_self = self;
     pcsig_fields = fields;
    }
end

module Cstr = struct
  let mk self fields =
    {
     pcstr_self = self;
     pcstr_fields = fields;
    }
end

(** Row fields *)
module Rf = struct
  let mk ?(loc = !default_loc) ?(attrs = []) desc = {
    prf_desc = desc;
    prf_loc = loc;
    prf_attributes = attrs;
  }
  let tag ?loc ?attrs label const tys =
    mk ?loc ?attrs (Rtag (label, const, tys))
  let inherit_?loc ty =
    mk ?loc (Rinherit ty)
end

(** Object fields *)
module Of = struct
  let mk ?(loc = !default_loc) ?(attrs=[]) desc = {
    pof_desc = desc;
    pof_loc = loc;
    pof_attributes = attrs;
  }
  let tag ?loc ?attrs label ty =
    mk ?loc ?attrs (Otag (label, ty))
  let inherit_ ?loc ty =
    mk ?loc (Oinherit ty)
end
end

module Ast_mapper = struct
open Parsetree
open Ast_helper
open Location

module String = Misc.Stdlib.String

type mapper = {
  attribute: mapper -> attribute -> attribute;
  attributes: mapper -> attribute list -> attribute list;
  binding_op: mapper -> binding_op -> binding_op;
  case: mapper -> case -> case;
  cases: mapper -> case list -> case list;
  class_declaration: mapper -> class_declaration -> class_declaration;
  class_description: mapper -> class_description -> class_description;
  class_expr: mapper -> class_expr -> class_expr;
  class_field: mapper -> class_field -> class_field;
  class_signature: mapper -> class_signature -> class_signature;
  class_structure: mapper -> class_structure -> class_structure;
  class_type: mapper -> class_type -> class_type;
  class_type_declaration: mapper -> class_type_declaration
                          -> class_type_declaration;
  class_type_field: mapper -> class_type_field -> class_type_field;
  constant: mapper -> constant -> constant;
  constructor_declaration: mapper -> constructor_declaration
                           -> constructor_declaration;
  expr: mapper -> expression -> expression;
  extension: mapper -> extension -> extension;
  extension_constructor: mapper -> extension_constructor
                         -> extension_constructor;
  include_declaration: mapper -> include_declaration -> include_declaration;
  include_description: mapper -> include_description -> include_description;
  label_declaration: mapper -> label_declaration -> label_declaration;
  location: mapper -> Location.t -> Location.t;
  module_binding: mapper -> module_binding -> module_binding;
  module_declaration: mapper -> module_declaration -> module_declaration;
  module_substitution: mapper -> module_substitution -> module_substitution;
  module_expr: mapper -> module_expr -> module_expr;
  module_type: mapper -> module_type -> module_type;
  module_type_declaration: mapper -> module_type_declaration
                           -> module_type_declaration;
  open_declaration: mapper -> open_declaration -> open_declaration;
  open_description: mapper -> open_description -> open_description;
  pat: mapper -> pattern -> pattern;
  payload: mapper -> payload -> payload;
  signature: mapper -> signature -> signature;
  signature_item: mapper -> signature_item -> signature_item;
  structure: mapper -> structure -> structure;
  structure_item: mapper -> structure_item -> structure_item;
  typ: mapper -> core_type -> core_type;
  type_declaration: mapper -> type_declaration -> type_declaration;
  type_extension: mapper -> type_extension -> type_extension;
  type_exception: mapper -> type_exception -> type_exception;
  type_kind: mapper -> type_kind -> type_kind;
  value_binding: mapper -> value_binding -> value_binding;
  value_description: mapper -> value_description -> value_description;
  with_constraint: mapper -> with_constraint -> with_constraint;
}

let map_fst f (x, y) = (f x, y)
let map_snd f (x, y) = (x, f y)
let map_tuple f1 f2 (x, y) = (f1 x, f2 y)
let map_tuple3 f1 f2 f3 (x, y, z) = (f1 x, f2 y, f3 z)
let map_opt f = function None -> None | Some x -> Some (f x)

let map_loc sub {loc; txt} = {loc = sub.location sub loc; txt}

module C = struct
  (* Constants *)

  let map sub c = match c with
    | Pconst_integer _
    | Pconst_char _
    | Pconst_float _
      -> c
    | Pconst_string (s, loc, quotation_delimiter) ->
        let loc = sub.location sub loc in
        Const.string ~loc ?quotation_delimiter s
end

module T = struct
  (* Type expressions for the core language *)

  let row_field sub {
      prf_desc;
      prf_loc;
      prf_attributes;
    } =
    let loc = sub.location sub prf_loc in
    let attrs = sub.attributes sub prf_attributes in
    let desc = match prf_desc with
      | Rtag (l, b, tl) -> Rtag (map_loc sub l, b, List.map (sub.typ sub) tl)
      | Rinherit t -> Rinherit (sub.typ sub t)
    in
    Rf.mk ~loc ~attrs desc

  let object_field sub {
      pof_desc;
      pof_loc;
      pof_attributes;
    } =
    let loc = sub.location sub pof_loc in
    let attrs = sub.attributes sub pof_attributes in
    let desc = match pof_desc with
      | Otag (l, t) -> Otag (map_loc sub l, sub.typ sub t)
      | Oinherit t -> Oinherit (sub.typ sub t)
    in
    Of.mk ~loc ~attrs desc

  let map sub {ptyp_desc = desc; ptyp_loc = loc; ptyp_attributes = attrs} =
    let open Typ in
    let loc = sub.location sub loc in
    let attrs = sub.attributes sub attrs in
    match desc with
    | Ptyp_any -> any ~loc ~attrs ()
    | Ptyp_var s -> var ~loc ~attrs s
    | Ptyp_arrow (lab, t1, t2) ->
        arrow ~loc ~attrs lab (sub.typ sub t1) (sub.typ sub t2)
    | Ptyp_tuple tyl -> tuple ~loc ~attrs (List.map (sub.typ sub) tyl)
    | Ptyp_constr (lid, tl) ->
        constr ~loc ~attrs (map_loc sub lid) (List.map (sub.typ sub) tl)
    | Ptyp_object (l, o) ->
        object_ ~loc ~attrs (List.map (object_field sub) l) o
    | Ptyp_class (lid, tl) ->
        class_ ~loc ~attrs (map_loc sub lid) (List.map (sub.typ sub) tl)
    | Ptyp_alias (t, s) -> alias ~loc ~attrs (sub.typ sub t) s
    | Ptyp_variant (rl, b, ll) ->
        variant ~loc ~attrs (List.map (row_field sub) rl) b ll
    | Ptyp_poly (sl, t) -> poly ~loc ~attrs
                             (List.map (map_loc sub) sl) (sub.typ sub t)
    | Ptyp_package (lid, l) ->
        package ~loc ~attrs (map_loc sub lid)
          (List.map (map_tuple (map_loc sub) (sub.typ sub)) l)
    | Ptyp_extension x -> extension ~loc ~attrs (sub.extension sub x)

  let map_type_declaration sub
      {ptype_name; ptype_params; ptype_cstrs;
       ptype_kind;
       ptype_private;
       ptype_manifest;
       ptype_attributes;
       ptype_loc} =
    let loc = sub.location sub ptype_loc in
    let attrs = sub.attributes sub ptype_attributes in
    Type.mk ~loc ~attrs (map_loc sub ptype_name)
      ~params:(List.map (map_fst (sub.typ sub)) ptype_params)
      ~priv:ptype_private
      ~cstrs:(List.map
                (map_tuple3 (sub.typ sub) (sub.typ sub) (sub.location sub))
                ptype_cstrs)
      ~kind:(sub.type_kind sub ptype_kind)
      ?manifest:(map_opt (sub.typ sub) ptype_manifest)

  let map_type_kind sub = function
    | Ptype_abstract -> Ptype_abstract
    | Ptype_variant l ->
        Ptype_variant (List.map (sub.constructor_declaration sub) l)
    | Ptype_record l -> Ptype_record (List.map (sub.label_declaration sub) l)
    | Ptype_open -> Ptype_open

  let map_constructor_arguments sub = function
    | Pcstr_tuple l -> Pcstr_tuple (List.map (sub.typ sub) l)
    | Pcstr_record l ->
        Pcstr_record (List.map (sub.label_declaration sub) l)

  let map_type_extension sub
      {ptyext_path; ptyext_params;
       ptyext_constructors;
       ptyext_private;
       ptyext_loc;
       ptyext_attributes} =
    let loc = sub.location sub ptyext_loc in
    let attrs = sub.attributes sub ptyext_attributes in
    Te.mk ~loc ~attrs
      (map_loc sub ptyext_path)
      (List.map (sub.extension_constructor sub) ptyext_constructors)
      ~params:(List.map (map_fst (sub.typ sub)) ptyext_params)
      ~priv:ptyext_private

  let map_type_exception sub
      {ptyexn_constructor; ptyexn_loc; ptyexn_attributes} =
    let loc = sub.location sub ptyexn_loc in
    let attrs = sub.attributes sub ptyexn_attributes in
    Te.mk_exception ~loc ~attrs
      (sub.extension_constructor sub ptyexn_constructor)

  let map_extension_constructor_kind sub = function
      Pext_decl(ctl, cto) ->
        Pext_decl(map_constructor_arguments sub ctl, map_opt (sub.typ sub) cto)
    | Pext_rebind li ->
        Pext_rebind (map_loc sub li)

  let map_extension_constructor sub
      {pext_name;
       pext_kind;
       pext_loc;
       pext_attributes} =
    let loc = sub.location sub pext_loc in
    let attrs = sub.attributes sub pext_attributes in
    Te.constructor ~loc ~attrs
      (map_loc sub pext_name)
      (map_extension_constructor_kind sub pext_kind)

end

module CT = struct
  (* Type expressions for the class language *)

  let map sub {pcty_loc = loc; pcty_desc = desc; pcty_attributes = attrs} =
    let open Cty in
    let loc = sub.location sub loc in
    let attrs = sub.attributes sub attrs in
    match desc with
    | Pcty_constr (lid, tys) ->
        constr ~loc ~attrs (map_loc sub lid) (List.map (sub.typ sub) tys)
    | Pcty_signature x -> signature ~loc ~attrs (sub.class_signature sub x)
    | Pcty_arrow (lab, t, ct) ->
        arrow ~loc ~attrs lab (sub.typ sub t) (sub.class_type sub ct)
    | Pcty_extension x -> extension ~loc ~attrs (sub.extension sub x)
    | Pcty_open (o, ct) ->
        open_ ~loc ~attrs (sub.open_description sub o) (sub.class_type sub ct)

  let map_field sub {pctf_desc = desc; pctf_loc = loc; pctf_attributes = attrs}
    =
    let open Ctf in
    let loc = sub.location sub loc in
    let attrs = sub.attributes sub attrs in
    match desc with
    | Pctf_inherit ct -> inherit_ ~loc ~attrs (sub.class_type sub ct)
    | Pctf_val (s, m, v, t) ->
        val_ ~loc ~attrs (map_loc sub s) m v (sub.typ sub t)
    | Pctf_method (s, p, v, t) ->
        method_ ~loc ~attrs (map_loc sub s) p v (sub.typ sub t)
    | Pctf_constraint (t1, t2) ->
        constraint_ ~loc ~attrs (sub.typ sub t1) (sub.typ sub t2)
    | Pctf_attribute x -> attribute ~loc (sub.attribute sub x)
    | Pctf_extension x -> extension ~loc ~attrs (sub.extension sub x)

  let map_signature sub {pcsig_self; pcsig_fields} =
    Csig.mk
      (sub.typ sub pcsig_self)
      (List.map (sub.class_type_field sub) pcsig_fields)
end

let map_functor_param sub = function
  | Unit -> Unit
  | Named (s, mt) -> Named (map_loc sub s, sub.module_type sub mt)

module MT = struct
  (* Type expressions for the module language *)

  let map sub {pmty_desc = desc; pmty_loc = loc; pmty_attributes = attrs} =
    let open Mty in
    let loc = sub.location sub loc in
    let attrs = sub.attributes sub attrs in
    match desc with
    | Pmty_ident s -> ident ~loc ~attrs (map_loc sub s)
    | Pmty_alias s -> alias ~loc ~attrs (map_loc sub s)
    | Pmty_signature sg -> signature ~loc ~attrs (sub.signature sub sg)
    | Pmty_functor (param, mt) ->
        functor_ ~loc ~attrs
          (map_functor_param sub param)
          (sub.module_type sub mt)
    | Pmty_with (mt, l) ->
        with_ ~loc ~attrs (sub.module_type sub mt)
          (List.map (sub.with_constraint sub) l)
    | Pmty_typeof me -> typeof_ ~loc ~attrs (sub.module_expr sub me)
    | Pmty_extension x -> extension ~loc ~attrs (sub.extension sub x)

  let map_with_constraint sub = function
    | Pwith_type (lid, d) ->
        Pwith_type (map_loc sub lid, sub.type_declaration sub d)
    | Pwith_module (lid, lid2) ->
        Pwith_module (map_loc sub lid, map_loc sub lid2)
    | Pwith_typesubst (lid, d) ->
        Pwith_typesubst (map_loc sub lid, sub.type_declaration sub d)
    | Pwith_modsubst (s, lid) ->
        Pwith_modsubst (map_loc sub s, map_loc sub lid)

  let map_signature_item sub {psig_desc = desc; psig_loc = loc} =
    let open Sig in
    let loc = sub.location sub loc in
    match desc with
    | Psig_value vd -> value ~loc (sub.value_description sub vd)
    | Psig_type (rf, l) ->
        type_ ~loc rf (List.map (sub.type_declaration sub) l)
    | Psig_typesubst l ->
        type_subst ~loc (List.map (sub.type_declaration sub) l)
    | Psig_typext te -> type_extension ~loc (sub.type_extension sub te)
    | Psig_exception ed -> exception_ ~loc (sub.type_exception sub ed)
    | Psig_module x -> module_ ~loc (sub.module_declaration sub x)
    | Psig_modsubst x -> mod_subst ~loc (sub.module_substitution sub x)
    | Psig_recmodule l ->
        rec_module ~loc (List.map (sub.module_declaration sub) l)
    | Psig_modtype x -> modtype ~loc (sub.module_type_declaration sub x)
    | Psig_open x -> open_ ~loc (sub.open_description sub x)
    | Psig_include x -> include_ ~loc (sub.include_description sub x)
    | Psig_class l -> class_ ~loc (List.map (sub.class_description sub) l)
    | Psig_class_type l ->
        class_type ~loc (List.map (sub.class_type_declaration sub) l)
    | Psig_extension (x, attrs) ->
        let attrs = sub.attributes sub attrs in
        extension ~loc ~attrs (sub.extension sub x)
    | Psig_attribute x -> attribute ~loc (sub.attribute sub x)
end


module M = struct
  (* Value expressions for the module language *)

  let map sub {pmod_loc = loc; pmod_desc = desc; pmod_attributes = attrs} =
    let open Mod in
    let loc = sub.location sub loc in
    let attrs = sub.attributes sub attrs in
    match desc with
    | Pmod_ident x -> ident ~loc ~attrs (map_loc sub x)
    | Pmod_structure str -> structure ~loc ~attrs (sub.structure sub str)
    | Pmod_functor (param, body) ->
        functor_ ~loc ~attrs
          (map_functor_param sub param)
          (sub.module_expr sub body)
    | Pmod_apply (m1, m2) ->
        apply ~loc ~attrs (sub.module_expr sub m1) (sub.module_expr sub m2)
    | Pmod_constraint (m, mty) ->
        constraint_ ~loc ~attrs (sub.module_expr sub m)
                    (sub.module_type sub mty)
    | Pmod_unpack e -> unpack ~loc ~attrs (sub.expr sub e)
    | Pmod_extension x -> extension ~loc ~attrs (sub.extension sub x)

  let map_structure_item sub {pstr_loc = loc; pstr_desc = desc} =
    let open Str in
    let loc = sub.location sub loc in
    match desc with
    | Pstr_eval (x, attrs) ->
        let attrs = sub.attributes sub attrs in
        eval ~loc ~attrs (sub.expr sub x)
    | Pstr_value (r, vbs) -> value ~loc r (List.map (sub.value_binding sub) vbs)
    | Pstr_primitive vd -> primitive ~loc (sub.value_description sub vd)
    | Pstr_type (rf, l) -> type_ ~loc rf (List.map (sub.type_declaration sub) l)
    | Pstr_typext te -> type_extension ~loc (sub.type_extension sub te)
    | Pstr_exception ed -> exception_ ~loc (sub.type_exception sub ed)
    | Pstr_module x -> module_ ~loc (sub.module_binding sub x)
    | Pstr_recmodule l -> rec_module ~loc (List.map (sub.module_binding sub) l)
    | Pstr_modtype x -> modtype ~loc (sub.module_type_declaration sub x)
    | Pstr_open x -> open_ ~loc (sub.open_declaration sub x)
    | Pstr_class l -> class_ ~loc (List.map (sub.class_declaration sub) l)
    | Pstr_class_type l ->
        class_type ~loc (List.map (sub.class_type_declaration sub) l)
    | Pstr_include x -> include_ ~loc (sub.include_declaration sub x)
    | Pstr_extension (x, attrs) ->
        let attrs = sub.attributes sub attrs in
        extension ~loc ~attrs (sub.extension sub x)
    | Pstr_attribute x -> attribute ~loc (sub.attribute sub x)
end

module E = struct
  (* Value expressions for the core language *)

  let map sub {pexp_loc = loc; pexp_desc = desc; pexp_attributes = attrs} =
    let open Exp in
    let loc = sub.location sub loc in
    let attrs = sub.attributes sub attrs in
    match desc with
    | Pexp_ident x -> ident ~loc ~attrs (map_loc sub x)
    | Pexp_constant x -> constant ~loc ~attrs (sub.constant sub x)
    | Pexp_let (r, vbs, e) ->
        let_ ~loc ~attrs r (List.map (sub.value_binding sub) vbs)
          (sub.expr sub e)
    | Pexp_fun (lab, def, p, e) ->
        fun_ ~loc ~attrs lab (map_opt (sub.expr sub) def) (sub.pat sub p)
          (sub.expr sub e)
    | Pexp_function pel -> function_ ~loc ~attrs (sub.cases sub pel)
    | Pexp_apply (e, l) ->
        apply ~loc ~attrs (sub.expr sub e) (List.map (map_snd (sub.expr sub)) l)
    | Pexp_match (e, pel) ->
        match_ ~loc ~attrs (sub.expr sub e) (sub.cases sub pel)
    | Pexp_try (e, pel) -> try_ ~loc ~attrs (sub.expr sub e) (sub.cases sub pel)
    | Pexp_tuple el -> tuple ~loc ~attrs (List.map (sub.expr sub) el)
    | Pexp_construct (lid, arg) ->
        construct ~loc ~attrs (map_loc sub lid) (map_opt (sub.expr sub) arg)
    | Pexp_variant (lab, eo) ->
        variant ~loc ~attrs lab (map_opt (sub.expr sub) eo)
    | Pexp_record (l, eo) ->
        record ~loc ~attrs (List.map (map_tuple (map_loc sub) (sub.expr sub)) l)
          (map_opt (sub.expr sub) eo)
    | Pexp_field (e, lid) ->
        field ~loc ~attrs (sub.expr sub e) (map_loc sub lid)
    | Pexp_setfield (e1, lid, e2) ->
        setfield ~loc ~attrs (sub.expr sub e1) (map_loc sub lid)
          (sub.expr sub e2)
    | Pexp_array el -> array ~loc ~attrs (List.map (sub.expr sub) el)
    | Pexp_ifthenelse (e1, e2, e3) ->
        ifthenelse ~loc ~attrs (sub.expr sub e1) (sub.expr sub e2)
          (map_opt (sub.expr sub) e3)
    | Pexp_sequence (e1, e2) ->
        sequence ~loc ~attrs (sub.expr sub e1) (sub.expr sub e2)
    | Pexp_while (e1, e2) ->
        while_ ~loc ~attrs (sub.expr sub e1) (sub.expr sub e2)
    | Pexp_for (p, e1, e2, d, e3) ->
        for_ ~loc ~attrs (sub.pat sub p) (sub.expr sub e1) (sub.expr sub e2) d
          (sub.expr sub e3)
    | Pexp_coerce (e, t1, t2) ->
        coerce ~loc ~attrs (sub.expr sub e) (map_opt (sub.typ sub) t1)
          (sub.typ sub t2)
    | Pexp_constraint (e, t) ->
        constraint_ ~loc ~attrs (sub.expr sub e) (sub.typ sub t)
    | Pexp_send (e, s) ->
        send ~loc ~attrs (sub.expr sub e) (map_loc sub s)
    | Pexp_new lid -> new_ ~loc ~attrs (map_loc sub lid)
    | Pexp_setinstvar (s, e) ->
        setinstvar ~loc ~attrs (map_loc sub s) (sub.expr sub e)
    | Pexp_override sel ->
        override ~loc ~attrs
          (List.map (map_tuple (map_loc sub) (sub.expr sub)) sel)
    | Pexp_letmodule (s, me, e) ->
        letmodule ~loc ~attrs (map_loc sub s) (sub.module_expr sub me)
          (sub.expr sub e)
    | Pexp_letexception (cd, e) ->
        letexception ~loc ~attrs
          (sub.extension_constructor sub cd)
          (sub.expr sub e)
    | Pexp_assert e -> assert_ ~loc ~attrs (sub.expr sub e)
    | Pexp_lazy e -> lazy_ ~loc ~attrs (sub.expr sub e)
    | Pexp_poly (e, t) ->
        poly ~loc ~attrs (sub.expr sub e) (map_opt (sub.typ sub) t)
    | Pexp_object cls -> object_ ~loc ~attrs (sub.class_structure sub cls)
    | Pexp_newtype (s, e) ->
        newtype ~loc ~attrs (map_loc sub s) (sub.expr sub e)
    | Pexp_pack me -> pack ~loc ~attrs (sub.module_expr sub me)
    | Pexp_open (o, e) ->
        open_ ~loc ~attrs (sub.open_declaration sub o) (sub.expr sub e)
    | Pexp_letop {let_; ands; body} ->
        letop ~loc ~attrs (sub.binding_op sub let_)
          (List.map (sub.binding_op sub) ands) (sub.expr sub body)
    | Pexp_extension x -> extension ~loc ~attrs (sub.extension sub x)
    | Pexp_unreachable -> unreachable ~loc ~attrs ()

  let map_binding_op sub {pbop_op; pbop_pat; pbop_exp; pbop_loc} =
    let open Exp in
    let op = map_loc sub pbop_op in
    let pat = sub.pat sub pbop_pat in
    let exp = sub.expr sub pbop_exp in
    let loc = sub.location sub pbop_loc in
    binding_op op pat exp loc

end

module P = struct
  (* Patterns *)

  let map sub {ppat_desc = desc; ppat_loc = loc; ppat_attributes = attrs} =
    let open Pat in
    let loc = sub.location sub loc in
    let attrs = sub.attributes sub attrs in
    match desc with
    | Ppat_any -> any ~loc ~attrs ()
    | Ppat_var s -> var ~loc ~attrs (map_loc sub s)
    | Ppat_alias (p, s) -> alias ~loc ~attrs (sub.pat sub p) (map_loc sub s)
    | Ppat_constant c -> constant ~loc ~attrs (sub.constant sub c)
    | Ppat_interval (c1, c2) -> interval ~loc ~attrs c1 c2
    | Ppat_tuple pl -> tuple ~loc ~attrs (List.map (sub.pat sub) pl)
    | Ppat_construct (l, p) ->
        construct ~loc ~attrs (map_loc sub l) (map_opt (sub.pat sub) p)
    | Ppat_variant (l, p) -> variant ~loc ~attrs l (map_opt (sub.pat sub) p)
    | Ppat_record (lpl, cf) ->
        record ~loc ~attrs
               (List.map (map_tuple (map_loc sub) (sub.pat sub)) lpl) cf
    | Ppat_array pl -> array ~loc ~attrs (List.map (sub.pat sub) pl)
    | Ppat_or (p1, p2) -> or_ ~loc ~attrs (sub.pat sub p1) (sub.pat sub p2)
    | Ppat_constraint (p, t) ->
        constraint_ ~loc ~attrs (sub.pat sub p) (sub.typ sub t)
    | Ppat_type s -> type_ ~loc ~attrs (map_loc sub s)
    | Ppat_lazy p -> lazy_ ~loc ~attrs (sub.pat sub p)
    | Ppat_unpack s -> unpack ~loc ~attrs (map_loc sub s)
    | Ppat_open (lid,p) -> open_ ~loc ~attrs (map_loc sub lid) (sub.pat sub p)
    | Ppat_exception p -> exception_ ~loc ~attrs (sub.pat sub p)
    | Ppat_extension x -> extension ~loc ~attrs (sub.extension sub x)
end

module CE = struct
  (* Value expressions for the class language *)

  let map sub {pcl_loc = loc; pcl_desc = desc; pcl_attributes = attrs} =
    let open Cl in
    let loc = sub.location sub loc in
    let attrs = sub.attributes sub attrs in
    match desc with
    | Pcl_constr (lid, tys) ->
        constr ~loc ~attrs (map_loc sub lid) (List.map (sub.typ sub) tys)
    | Pcl_structure s ->
        structure ~loc ~attrs (sub.class_structure sub s)
    | Pcl_fun (lab, e, p, ce) ->
        fun_ ~loc ~attrs lab
          (map_opt (sub.expr sub) e)
          (sub.pat sub p)
          (sub.class_expr sub ce)
    | Pcl_apply (ce, l) ->
        apply ~loc ~attrs (sub.class_expr sub ce)
          (List.map (map_snd (sub.expr sub)) l)
    | Pcl_let (r, vbs, ce) ->
        let_ ~loc ~attrs r (List.map (sub.value_binding sub) vbs)
          (sub.class_expr sub ce)
    | Pcl_constraint (ce, ct) ->
        constraint_ ~loc ~attrs (sub.class_expr sub ce) (sub.class_type sub ct)
    | Pcl_extension x -> extension ~loc ~attrs (sub.extension sub x)
    | Pcl_open (o, ce) ->
        open_ ~loc ~attrs (sub.open_description sub o) (sub.class_expr sub ce)

  let map_kind sub = function
    | Cfk_concrete (o, e) -> Cfk_concrete (o, sub.expr sub e)
    | Cfk_virtual t -> Cfk_virtual (sub.typ sub t)

  let map_field sub {pcf_desc = desc; pcf_loc = loc; pcf_attributes = attrs} =
    let open Cf in
    let loc = sub.location sub loc in
    let attrs = sub.attributes sub attrs in
    match desc with
    | Pcf_inherit (o, ce, s) ->
        inherit_ ~loc ~attrs o (sub.class_expr sub ce)
          (map_opt (map_loc sub) s)
    | Pcf_val (s, m, k) -> val_ ~loc ~attrs (map_loc sub s) m (map_kind sub k)
    | Pcf_method (s, p, k) ->
        method_ ~loc ~attrs (map_loc sub s) p (map_kind sub k)
    | Pcf_constraint (t1, t2) ->
        constraint_ ~loc ~attrs (sub.typ sub t1) (sub.typ sub t2)
    | Pcf_initializer e -> initializer_ ~loc ~attrs (sub.expr sub e)
    | Pcf_attribute x -> attribute ~loc (sub.attribute sub x)
    | Pcf_extension x -> extension ~loc ~attrs (sub.extension sub x)

  let map_structure sub {pcstr_self; pcstr_fields} =
    {
      pcstr_self = sub.pat sub pcstr_self;
      pcstr_fields = List.map (sub.class_field sub) pcstr_fields;
    }

  let class_infos sub f {pci_virt; pci_params = pl; pci_name; pci_expr;
                         pci_loc; pci_attributes} =
    let loc = sub.location sub pci_loc in
    let attrs = sub.attributes sub pci_attributes in
    Ci.mk ~loc ~attrs
     ~virt:pci_virt
     ~params:(List.map (map_fst (sub.typ sub)) pl)
      (map_loc sub pci_name)
      (f pci_expr)
end

(* Now, a generic AST mapper, to be extended to cover all kinds and
   cases of the OCaml grammar.  The default behavior of the mapper is
   the identity. *)

let default_mapper =
  {
    constant = C.map;
    structure = (fun this l -> List.map (this.structure_item this) l);
    structure_item = M.map_structure_item;
    module_expr = M.map;
    signature = (fun this l -> List.map (this.signature_item this) l);
    signature_item = MT.map_signature_item;
    module_type = MT.map;
    with_constraint = MT.map_with_constraint;
    class_declaration =
      (fun this -> CE.class_infos this (this.class_expr this));
    class_expr = CE.map;
    class_field = CE.map_field;
    class_structure = CE.map_structure;
    class_type = CT.map;
    class_type_field = CT.map_field;
    class_signature = CT.map_signature;
    class_type_declaration =
      (fun this -> CE.class_infos this (this.class_type this));
    class_description =
      (fun this -> CE.class_infos this (this.class_type this));
    type_declaration = T.map_type_declaration;
    type_kind = T.map_type_kind;
    typ = T.map;
    type_extension = T.map_type_extension;
    type_exception = T.map_type_exception;
    extension_constructor = T.map_extension_constructor;
    value_description =
      (fun this {pval_name; pval_type; pval_prim; pval_loc;
                 pval_attributes} ->
        Val.mk
          (map_loc this pval_name)
          (this.typ this pval_type)
          ~attrs:(this.attributes this pval_attributes)
          ~loc:(this.location this pval_loc)
          ~prim:pval_prim
      );

    pat = P.map;
    expr = E.map;
    binding_op = E.map_binding_op;

    module_declaration =
      (fun this {pmd_name; pmd_type; pmd_attributes; pmd_loc} ->
         Md.mk
           (map_loc this pmd_name)
           (this.module_type this pmd_type)
           ~attrs:(this.attributes this pmd_attributes)
           ~loc:(this.location this pmd_loc)
      );

    module_substitution =
      (fun this {pms_name; pms_manifest; pms_attributes; pms_loc} ->
         Ms.mk
           (map_loc this pms_name)
           (map_loc this pms_manifest)
           ~attrs:(this.attributes this pms_attributes)
           ~loc:(this.location this pms_loc)
      );

    module_type_declaration =
      (fun this {pmtd_name; pmtd_type; pmtd_attributes; pmtd_loc} ->
         Mtd.mk
           (map_loc this pmtd_name)
           ?typ:(map_opt (this.module_type this) pmtd_type)
           ~attrs:(this.attributes this pmtd_attributes)
           ~loc:(this.location this pmtd_loc)
      );

    module_binding =
      (fun this {pmb_name; pmb_expr; pmb_attributes; pmb_loc} ->
         Mb.mk (map_loc this pmb_name) (this.module_expr this pmb_expr)
           ~attrs:(this.attributes this pmb_attributes)
           ~loc:(this.location this pmb_loc)
      );


    open_declaration =
      (fun this {popen_expr; popen_override; popen_attributes; popen_loc} ->
         Opn.mk (this.module_expr this popen_expr)
           ~override:popen_override
           ~loc:(this.location this popen_loc)
           ~attrs:(this.attributes this popen_attributes)
      );

    open_description =
      (fun this {popen_expr; popen_override; popen_attributes; popen_loc} ->
         Opn.mk (map_loc this popen_expr)
           ~override:popen_override
           ~loc:(this.location this popen_loc)
           ~attrs:(this.attributes this popen_attributes)
      );

    include_description =
      (fun this {pincl_mod; pincl_attributes; pincl_loc} ->
         Incl.mk (this.module_type this pincl_mod)
           ~loc:(this.location this pincl_loc)
           ~attrs:(this.attributes this pincl_attributes)
      );

    include_declaration =
      (fun this {pincl_mod; pincl_attributes; pincl_loc} ->
         Incl.mk (this.module_expr this pincl_mod)
           ~loc:(this.location this pincl_loc)
           ~attrs:(this.attributes this pincl_attributes)
      );


    value_binding =
      (fun this {pvb_pat; pvb_expr; pvb_attributes; pvb_loc} ->
         Vb.mk
           (this.pat this pvb_pat)
           (this.expr this pvb_expr)
           ~loc:(this.location this pvb_loc)
           ~attrs:(this.attributes this pvb_attributes)
      );


    constructor_declaration =
      (fun this {pcd_name; pcd_args; pcd_res; pcd_loc; pcd_attributes} ->
        Type.constructor
          (map_loc this pcd_name)
          ~args:(T.map_constructor_arguments this pcd_args)
          ?res:(map_opt (this.typ this) pcd_res)
          ~loc:(this.location this pcd_loc)
          ~attrs:(this.attributes this pcd_attributes)
      );

    label_declaration =
      (fun this {pld_name; pld_type; pld_loc; pld_mutable; pld_attributes} ->
         Type.field
           (map_loc this pld_name)
           (this.typ this pld_type)
           ~mut:pld_mutable
           ~loc:(this.location this pld_loc)
           ~attrs:(this.attributes this pld_attributes)
      );

    cases = (fun this l -> List.map (this.case this) l);
    case =
      (fun this {pc_lhs; pc_guard; pc_rhs} ->
         {
           pc_lhs = this.pat this pc_lhs;
           pc_guard = map_opt (this.expr this) pc_guard;
           pc_rhs = this.expr this pc_rhs;
         }
      );



    location = (fun _this l -> l);

    extension = (fun this (s, e) -> (map_loc this s, this.payload this e));
    attribute = (fun this a ->
      {
        attr_name = map_loc this a.attr_name;
        attr_payload = this.payload this a.attr_payload;
        attr_loc = this.location this a.attr_loc
      }
    );
    attributes = (fun this l -> List.map (this.attribute this) l);
    payload =
      (fun this -> function
         | PStr x -> PStr (this.structure this x)
         | PSig x -> PSig (this.signature this x)
         | PTyp x -> PTyp (this.typ this x)
         | PPat (x, g) -> PPat (this.pat this x, map_opt (this.expr this) g)
      );
  }

let extension_of_error {kind; main; sub} =
  if kind <> Location.Report_error then
    raise (Invalid_argument "extension_of_error: expected kind Report_error");
  let str_of_pp pp_msg = Format.asprintf "%t" pp_msg in
  let extension_of_sub sub =
    { loc = sub.loc; txt = "ocaml.error" },
    PStr ([Str.eval (Exp.constant
                       (Pconst_string (str_of_pp sub.txt, sub.loc, None)))])
  in
  { loc = main.loc; txt = "ocaml.error" },
  PStr (Str.eval (Exp.constant
                    (Pconst_string (str_of_pp main.txt, main.loc, None))) ::
        List.map (fun msg -> Str.extension (extension_of_sub msg)) sub)

let attribute_of_warning loc s =
  Attr.mk
    {loc; txt = "ocaml.ppwarning" }
    (PStr ([Str.eval ~loc (Exp.constant (Pconst_string (s, loc, None)))]))

let cookies : Parsetree.expression String.Map.t ref = ref String.Map.empty

let get_cookie k =
  try Some (String.Map.find k !cookies)
  with Not_found -> None

let set_cookie k v =
  cookies := String.Map.add k v !cookies

let tool_name_ref = ref "_none_"

let tool_name () = !tool_name_ref


module PpxContext = struct
  open Longident
  open Asttypes
  open Ast_helper

  let lid name = { txt = Lident name; loc = Location.none }

  let make_string s = Exp.constant (Const.string s)

  let make_bool x =
    if x
    then Exp.construct (lid "true") None
    else Exp.construct (lid "false") None

  let rec make_list f lst =
    match lst with
    | x :: rest ->
      Exp.construct (lid "::") (Some (Exp.tuple [f x; make_list f rest]))
    | [] ->
      Exp.construct (lid "[]") None

  let make_pair f1 f2 (x1, x2) =
    Exp.tuple [f1 x1; f2 x2]

  let make_option f opt =
    match opt with
    | Some x -> Exp.construct (lid "Some") (Some (f x))
    | None   -> Exp.construct (lid "None") None

  let get_cookies () =
    lid "cookies",
    make_list (make_pair make_string (fun x -> x))
      (String.Map.bindings !cookies)

  let mk fields =
    {
      attr_name = { txt = "ocaml.ppx.context"; loc = Location.none };
      attr_payload = Parsetree.PStr [Str.eval (Exp.record fields None)];
      attr_loc = Location.none
    }

  let make ~tool_name () =
    let fields =
      [
        lid "tool_name",    make_string tool_name;
        lid "include_dirs", make_list make_string !Clflags.include_dirs;
        lid "load_path",    make_list make_string (Load_path.get_paths ());
        lid "open_modules", make_list make_string !Clflags.open_modules;
        lid "for_package",  make_option make_string !Clflags.for_package;
        lid "debug",        make_bool !Clflags.debug;
        lid "use_threads",  make_bool !Clflags.use_threads;
        lid "use_vmthreads", make_bool false;
        lid "recursive_types", make_bool !Clflags.recursive_types;
        lid "principal", make_bool !Clflags.principal;
        lid "transparent_modules", make_bool !Clflags.transparent_modules;
        lid "unboxed_types", make_bool !Clflags.unboxed_types;
        lid "unsafe_string", make_bool !Clflags.unsafe_string;
        get_cookies ()
      ]
    in
    mk fields

  let get_fields = function
    | PStr [{pstr_desc = Pstr_eval
                 ({ pexp_desc = Pexp_record (fields, None) }, [])}] ->
        fields
    | _ ->
        raise_errorf "Internal error: invalid [@@@ocaml.ppx.context] syntax"

  let restore fields =
    let field name payload =
      let rec get_string = function
        | { pexp_desc = Pexp_constant (Pconst_string (str, _, None)) } -> str
        | _ -> raise_errorf "Internal error: invalid [@@@ocaml.ppx.context \
                             { %s }] string syntax" name
      and get_bool pexp =
        match pexp with
        | {pexp_desc = Pexp_construct ({txt = Longident.Lident "true"},
                                       None)} ->
            true
        | {pexp_desc = Pexp_construct ({txt = Longident.Lident "false"},
                                       None)} ->
            false
        | _ -> raise_errorf "Internal error: invalid [@@@ocaml.ppx.context \
                             { %s }] bool syntax" name
      and get_list elem = function
        | {pexp_desc =
             Pexp_construct ({txt = Longident.Lident "::"},
                             Some {pexp_desc = Pexp_tuple [exp; rest]}) } ->
            elem exp :: get_list elem rest
        | {pexp_desc =
             Pexp_construct ({txt = Longident.Lident "[]"}, None)} ->
            []
        | _ -> raise_errorf "Internal error: invalid [@@@ocaml.ppx.context \
                             { %s }] list syntax" name
      and get_pair f1 f2 = function
        | {pexp_desc = Pexp_tuple [e1; e2]} ->
            (f1 e1, f2 e2)
        | _ -> raise_errorf "Internal error: invalid [@@@ocaml.ppx.context \
                             { %s }] pair syntax" name
      and get_option elem = function
        | { pexp_desc =
              Pexp_construct ({ txt = Longident.Lident "Some" }, Some exp) } ->
            Some (elem exp)
        | { pexp_desc =
              Pexp_construct ({ txt = Longident.Lident "None" }, None) } ->
            None
        | _ -> raise_errorf "Internal error: invalid [@@@ocaml.ppx.context \
                             { %s }] option syntax" name
      in
      match name with
      | "tool_name" ->
          tool_name_ref := get_string payload
      | "include_dirs" ->
          Clflags.include_dirs := get_list get_string payload
      | "load_path" ->
          Load_path.init (get_list get_string payload)
      | "open_modules" ->
          Clflags.open_modules := get_list get_string payload
      | "for_package" ->
          Clflags.for_package := get_option get_string payload
      | "debug" ->
          Clflags.debug := get_bool payload
      | "use_threads" ->
          Clflags.use_threads := get_bool payload
      | "use_vmthreads" ->
          if get_bool payload then
            raise_errorf "Internal error: vmthreads not supported after 4.09.0"
      | "recursive_types" ->
          Clflags.recursive_types := get_bool payload
      | "principal" ->
          Clflags.principal := get_bool payload
      | "transparent_modules" ->
          Clflags.transparent_modules := get_bool payload
      | "unboxed_types" ->
          Clflags.unboxed_types := get_bool payload
      | "unsafe_string" ->
          Clflags.unsafe_string := get_bool payload
      | "cookies" ->
          let l: (string * Parsetree.expression) list = get_list (get_pair get_string (fun x -> x)) payload in
          cookies :=
            List.fold_left
              (fun s (k, v) -> String.Map.add k v s) String.Map.empty
              l
      | _ ->
          ()
    in
    List.iter (function ({txt=Lident name}, x) -> field name x | _ -> ()) fields

  let update_cookies fields =
    let fields =
      List.filter
        (function ({txt=Lident "cookies"}, _) -> false | _ -> true)
        fields
    in
    fields @ [get_cookies ()]
end

let ppx_context = PpxContext.make

let extension_of_exn exn =
  match error_of_exn exn with
  | Some (`Ok error) -> extension_of_error error
  | Some `Already_displayed ->
      { loc = Location.none; txt = "ocaml.error" }, PStr []
  | None -> raise exn


let apply_lazy ~source ~target mapper =
  let implem ast =
    let fields, ast =
      match ast with
      | {pstr_desc = Pstr_attribute ({attr_name = {txt = "ocaml.ppx.context"};
                                      attr_payload = x})} :: l ->
          PpxContext.get_fields x, l
      | _ -> [], ast
    in
    PpxContext.restore fields;
    let ast =
      try
        let mapper = mapper () in
        mapper.structure mapper ast
      with exn ->
        [{pstr_desc = Pstr_extension (extension_of_exn exn, []);
          pstr_loc  = Location.none}]
    in
    let fields = PpxContext.update_cookies fields in
    Str.attribute (PpxContext.mk fields) :: ast
  in
  let iface ast =
    let fields, ast =
      match ast with
      | {psig_desc = Psig_attribute ({attr_name = {txt = "ocaml.ppx.context"};
                                      attr_payload = x;
                                      attr_loc = _})} :: l ->
          PpxContext.get_fields x, l
      | _ -> [], ast
    in
    PpxContext.restore fields;
    let ast =
      try
        let mapper = mapper () in
        mapper.signature mapper ast
      with exn ->
        [{psig_desc = Psig_extension (extension_of_exn exn, []);
          psig_loc  = Location.none}]
    in
    let fields = PpxContext.update_cookies fields in
    Sig.attribute (PpxContext.mk fields) :: ast
  in

  let ic = open_in_bin source in
  let magic =
    really_input_string ic (String.length Config.ast_impl_magic_number)
  in

  let rewrite transform =
    Location.input_name := input_value ic;
    let ast = input_value ic in
    close_in ic;
    let ast = transform ast in
    let oc = open_out_bin target in
    output_string oc magic;
    output_value oc !Location.input_name;
    output_value oc ast;
    close_out oc
  and fail () =
    close_in ic;
    failwith "Ast_mapper: OCaml version mismatch or malformed input";
  in

  if magic = Config.ast_impl_magic_number then
    rewrite (implem : structure -> structure)
  else if magic = Config.ast_intf_magic_number then
    rewrite (iface : signature -> signature)
  else fail ()

let drop_ppx_context_str ~restore = function
  | {pstr_desc = Pstr_attribute
                   {attr_name = {Location.txt = "ocaml.ppx.context"};
                    attr_payload = a;
                    attr_loc = _}}
    :: items ->
      if restore then
        PpxContext.restore (PpxContext.get_fields a);
      items
  | items -> items

let drop_ppx_context_sig ~restore = function
  | {psig_desc = Psig_attribute
                   {attr_name = {Location.txt = "ocaml.ppx.context"};
                    attr_payload = a;
                    attr_loc = _}}
    :: items ->
      if restore then
        PpxContext.restore (PpxContext.get_fields a);
      items
  | items -> items

let add_ppx_context_str ~tool_name ast =
  Ast_helper.Str.attribute (ppx_context ~tool_name ()) :: ast

let add_ppx_context_sig ~tool_name ast =
  Ast_helper.Sig.attribute (ppx_context ~tool_name ()) :: ast


let apply ~source ~target mapper =
  apply_lazy ~source ~target (fun () -> mapper)

let run_main mapper =
  try
    let a = Sys.argv in
    let n = Array.length a in
    if n > 2 then
      let mapper () =
        try mapper (Array.to_list (Array.sub a 1 (n - 3)))
        with exn ->
          (* PR#6463 *)
          let f _ _ = raise exn in
          {default_mapper with structure = f; signature = f}
      in
      apply_lazy ~source:a.(n - 2) ~target:a.(n - 1) mapper
    else begin
      Printf.eprintf "Usage: %s [extra_args] <infile> <outfile>\n%!"
                     Sys.executable_name;
      exit 2
    end
  with exn ->
    prerr_endline (Printexc.to_string exn);
    exit 2

let register_function : (string -> (string list -> mapper) -> unit) ref =
     ref (fun _name f -> run_main f)
let register name f = !register_function name f
end

module Ast_iterator = struct
open Parsetree
open Location

type iterator = {
  attribute: iterator -> attribute -> unit;
  attributes: iterator -> attribute list -> unit;
  binding_op: iterator -> binding_op -> unit;
  case: iterator -> case -> unit;
  cases: iterator -> case list -> unit;
  class_declaration: iterator -> class_declaration -> unit;
  class_description: iterator -> class_description -> unit;
  class_expr: iterator -> class_expr -> unit;
  class_field: iterator -> class_field -> unit;
  class_signature: iterator -> class_signature -> unit;
  class_structure: iterator -> class_structure -> unit;
  class_type: iterator -> class_type -> unit;
  class_type_declaration: iterator -> class_type_declaration -> unit;
  class_type_field: iterator -> class_type_field -> unit;
  constructor_declaration: iterator -> constructor_declaration -> unit;
  expr: iterator -> expression -> unit;
  extension: iterator -> extension -> unit;
  extension_constructor: iterator -> extension_constructor -> unit;
  include_declaration: iterator -> include_declaration -> unit;
  include_description: iterator -> include_description -> unit;
  label_declaration: iterator -> label_declaration -> unit;
  location: iterator -> Location.t -> unit;
  module_binding: iterator -> module_binding -> unit;
  module_declaration: iterator -> module_declaration -> unit;
  module_substitution: iterator -> module_substitution -> unit;
  module_expr: iterator -> module_expr -> unit;
  module_type: iterator -> module_type -> unit;
  module_type_declaration: iterator -> module_type_declaration -> unit;
  open_declaration: iterator -> open_declaration -> unit;
  open_description: iterator -> open_description -> unit;
  pat: iterator -> pattern -> unit;
  payload: iterator -> payload -> unit;
  signature: iterator -> signature -> unit;
  signature_item: iterator -> signature_item -> unit;
  structure: iterator -> structure -> unit;
  structure_item: iterator -> structure_item -> unit;
  typ: iterator -> core_type -> unit;
  row_field: iterator -> row_field -> unit;
  object_field: iterator -> object_field -> unit;
  type_declaration: iterator -> type_declaration -> unit;
  type_extension: iterator -> type_extension -> unit;
  type_exception: iterator -> type_exception -> unit;
  type_kind: iterator -> type_kind -> unit;
  value_binding: iterator -> value_binding -> unit;
  value_description: iterator -> value_description -> unit;
  with_constraint: iterator -> with_constraint -> unit;
}
(** A [iterator] record implements one "method" per syntactic category,
    using an open recursion style: each method takes as its first
    argument the iterator to be applied to children in the syntax
    tree. *)

let iter_fst f (x, _) = f x
let iter_snd f (_, y) = f y
let iter_tuple f1 f2 (x, y) = f1 x; f2 y
let iter_tuple3 f1 f2 f3 (x, y, z) = f1 x; f2 y; f3 z
let iter_opt f = function None -> () | Some x -> f x

let iter_loc sub {loc; txt = _} = sub.location sub loc

module T = struct
  (* Type expressions for the core language *)

  let row_field sub {
      prf_desc;
      prf_loc;
      prf_attributes;
    } =
    sub.location sub prf_loc;
    sub.attributes sub prf_attributes;
    match prf_desc with
    | Rtag (_, _, tl) -> List.iter (sub.typ sub) tl
    | Rinherit t -> sub.typ sub t

  let object_field sub {
      pof_desc;
      pof_loc;
      pof_attributes;
    } =
    sub.location sub pof_loc;
    sub.attributes sub pof_attributes;
    match pof_desc with
    | Otag (_, t) -> sub.typ sub t
    | Oinherit t -> sub.typ sub t

  let iter sub {ptyp_desc = desc; ptyp_loc = loc; ptyp_attributes = attrs; _} =
    sub.location sub loc;
    sub.attributes sub attrs;
    match desc with
    | Ptyp_any
    | Ptyp_var _ -> ()
    | Ptyp_arrow (_lab, t1, t2) ->
        sub.typ sub t1; sub.typ sub t2
    | Ptyp_tuple tyl -> List.iter (sub.typ sub) tyl
    | Ptyp_constr (lid, tl) ->
        iter_loc sub lid; List.iter (sub.typ sub) tl
    | Ptyp_object (ol, _o) ->
        List.iter (object_field sub) ol
    | Ptyp_class (lid, tl) ->
        iter_loc sub lid; List.iter (sub.typ sub) tl
    | Ptyp_alias (t, _) -> sub.typ sub t
    | Ptyp_variant (rl, _b, _ll) ->
        List.iter (row_field sub) rl
    | Ptyp_poly (_, t) -> sub.typ sub t
    | Ptyp_package (lid, l) ->
        iter_loc sub lid;
        List.iter (iter_tuple (iter_loc sub) (sub.typ sub)) l
    | Ptyp_extension x -> sub.extension sub x

  let iter_type_declaration sub
      {ptype_name; ptype_params; ptype_cstrs;
       ptype_kind;
       ptype_private = _;
       ptype_manifest;
       ptype_attributes;
       ptype_loc} =
    iter_loc sub ptype_name;
    List.iter (iter_fst (sub.typ sub)) ptype_params;
    List.iter
      (iter_tuple3 (sub.typ sub) (sub.typ sub) (sub.location sub))
      ptype_cstrs;
    sub.type_kind sub ptype_kind;
    iter_opt (sub.typ sub) ptype_manifest;
    sub.location sub ptype_loc;
    sub.attributes sub ptype_attributes

  let iter_type_kind sub = function
    | Ptype_abstract -> ()
    | Ptype_variant l ->
        List.iter (sub.constructor_declaration sub) l
    | Ptype_record l -> List.iter (sub.label_declaration sub) l
    | Ptype_open -> ()

  let iter_constructor_arguments sub = function
    | Pcstr_tuple l -> List.iter (sub.typ sub) l
    | Pcstr_record l ->
        List.iter (sub.label_declaration sub) l

  let iter_type_extension sub
      {ptyext_path; ptyext_params;
       ptyext_constructors;
       ptyext_private = _;
       ptyext_loc;
       ptyext_attributes} =
    iter_loc sub ptyext_path;
    List.iter (sub.extension_constructor sub) ptyext_constructors;
    List.iter (iter_fst (sub.typ sub)) ptyext_params;
    sub.location sub ptyext_loc;
    sub.attributes sub ptyext_attributes

  let iter_type_exception sub
      {ptyexn_constructor; ptyexn_loc; ptyexn_attributes} =
    sub.extension_constructor sub ptyexn_constructor;
    sub.location sub ptyexn_loc;
    sub.attributes sub ptyexn_attributes

  let iter_extension_constructor_kind sub = function
      Pext_decl(ctl, cto) ->
        iter_constructor_arguments sub ctl; iter_opt (sub.typ sub) cto
    | Pext_rebind li ->
        iter_loc sub li

  let iter_extension_constructor sub
      {pext_name;
       pext_kind;
       pext_loc;
       pext_attributes} =
    iter_loc sub pext_name;
    iter_extension_constructor_kind sub pext_kind;
    sub.location sub pext_loc;
    sub.attributes sub pext_attributes

end

module CT = struct
  (* Type expressions for the class language *)

  let iter sub {pcty_loc = loc; pcty_desc = desc; pcty_attributes = attrs} =
    sub.location sub loc;
    sub.attributes sub attrs;
    match desc with
    | Pcty_constr (lid, tys) ->
        iter_loc sub lid; List.iter (sub.typ sub) tys
    | Pcty_signature x -> sub.class_signature sub x
    | Pcty_arrow (_lab, t, ct) ->
        sub.typ sub t; sub.class_type sub ct
    | Pcty_extension x -> sub.extension sub x
    | Pcty_open (o, e) ->
        sub.open_description sub o; sub.class_type sub e

  let iter_field sub {pctf_desc = desc; pctf_loc = loc; pctf_attributes = attrs}
    =
    sub.location sub loc;
    sub.attributes sub attrs;
    match desc with
    | Pctf_inherit ct -> sub.class_type sub ct
    | Pctf_val (_s, _m, _v, t) -> sub.typ sub t
    | Pctf_method (_s, _p, _v, t) -> sub.typ sub t
    | Pctf_constraint (t1, t2) ->
        sub.typ sub t1; sub.typ sub t2
    | Pctf_attribute x -> sub.attribute sub x
    | Pctf_extension x -> sub.extension sub x

  let iter_signature sub {pcsig_self; pcsig_fields} =
    sub.typ sub pcsig_self;
    List.iter (sub.class_type_field sub) pcsig_fields
end

let iter_functor_param sub = function
  | Unit -> ()
  | Named (name, mty) ->
    iter_loc sub name;
    sub.module_type sub mty

module MT = struct
  (* Type expressions for the module language *)

  let iter sub {pmty_desc = desc; pmty_loc = loc; pmty_attributes = attrs} =
    sub.location sub loc;
    sub.attributes sub attrs;
    match desc with
    | Pmty_ident s -> iter_loc sub s
    | Pmty_alias s -> iter_loc sub s
    | Pmty_signature sg -> sub.signature sub sg
    | Pmty_functor (param, mt2) ->
        iter_functor_param sub param;
        sub.module_type sub mt2
    | Pmty_with (mt, l) ->
        sub.module_type sub mt;
        List.iter (sub.with_constraint sub) l
    | Pmty_typeof me -> sub.module_expr sub me
    | Pmty_extension x -> sub.extension sub x

  let iter_with_constraint sub = function
    | Pwith_type (lid, d) ->
        iter_loc sub lid; sub.type_declaration sub d
    | Pwith_module (lid, lid2) ->
        iter_loc sub lid; iter_loc sub lid2
    | Pwith_typesubst (lid, d) ->
        iter_loc sub lid; sub.type_declaration sub d
    | Pwith_modsubst (s, lid) ->
        iter_loc sub s; iter_loc sub lid

  let iter_signature_item sub {psig_desc = desc; psig_loc = loc} =
    sub.location sub loc;
    match desc with
    | Psig_value vd -> sub.value_description sub vd
    | Psig_type (_, l)
    | Psig_typesubst l ->
      List.iter (sub.type_declaration sub) l
    | Psig_typext te -> sub.type_extension sub te
    | Psig_exception ed -> sub.type_exception sub ed
    | Psig_module x -> sub.module_declaration sub x
    | Psig_modsubst x -> sub.module_substitution sub x
    | Psig_recmodule l ->
        List.iter (sub.module_declaration sub) l
    | Psig_modtype x -> sub.module_type_declaration sub x
    | Psig_open x -> sub.open_description sub x
    | Psig_include x -> sub.include_description sub x
    | Psig_class l -> List.iter (sub.class_description sub) l
    | Psig_class_type l ->
        List.iter (sub.class_type_declaration sub) l
    | Psig_extension (x, attrs) ->
        sub.attributes sub attrs;
        sub.extension sub x
    | Psig_attribute x -> sub.attribute sub x
end


module M = struct
  (* Value expressions for the module language *)

  let iter sub {pmod_loc = loc; pmod_desc = desc; pmod_attributes = attrs} =
    sub.location sub loc;
    sub.attributes sub attrs;
    match desc with
    | Pmod_ident x -> iter_loc sub x
    | Pmod_structure str -> sub.structure sub str
    | Pmod_functor (param, body) ->
        iter_functor_param sub param;
        sub.module_expr sub body
    | Pmod_apply (m1, m2) ->
        sub.module_expr sub m1; sub.module_expr sub m2
    | Pmod_constraint (m, mty) ->
        sub.module_expr sub m; sub.module_type sub mty
    | Pmod_unpack e -> sub.expr sub e
    | Pmod_extension x -> sub.extension sub x

  let iter_structure_item sub {pstr_loc = loc; pstr_desc = desc} =
    sub.location sub loc;
    match desc with
    | Pstr_eval (x, attrs) ->
        sub.attributes sub attrs; sub.expr sub x
    | Pstr_value (_r, vbs) -> List.iter (sub.value_binding sub) vbs
    | Pstr_primitive vd -> sub.value_description sub vd
    | Pstr_type (_rf, l) -> List.iter (sub.type_declaration sub) l
    | Pstr_typext te -> sub.type_extension sub te
    | Pstr_exception ed -> sub.type_exception sub ed
    | Pstr_module x -> sub.module_binding sub x
    | Pstr_recmodule l -> List.iter (sub.module_binding sub) l
    | Pstr_modtype x -> sub.module_type_declaration sub x
    | Pstr_open x -> sub.open_declaration sub x
    | Pstr_class l -> List.iter (sub.class_declaration sub) l
    | Pstr_class_type l ->
        List.iter (sub.class_type_declaration sub) l
    | Pstr_include x -> sub.include_declaration sub x
    | Pstr_extension (x, attrs) ->
        sub.attributes sub attrs; sub.extension sub x
    | Pstr_attribute x -> sub.attribute sub x
end

module E = struct
  (* Value expressions for the core language *)

  let iter sub {pexp_loc = loc; pexp_desc = desc; pexp_attributes = attrs; _} =
    sub.location sub loc;
    sub.attributes sub attrs;
    match desc with
    | Pexp_ident x -> iter_loc sub x
    | Pexp_constant _ -> ()
    | Pexp_let (_r, vbs, e) ->
        List.iter (sub.value_binding sub) vbs;
        sub.expr sub e
    | Pexp_fun (_lab, def, p, e) ->
        iter_opt (sub.expr sub) def;
        sub.pat sub p;
        sub.expr sub e
    | Pexp_function pel -> sub.cases sub pel
    | Pexp_apply (e, l) ->
        sub.expr sub e; List.iter (iter_snd (sub.expr sub)) l
    | Pexp_match (e, pel) ->
        sub.expr sub e; sub.cases sub pel
    | Pexp_try (e, pel) -> sub.expr sub e; sub.cases sub pel
    | Pexp_tuple el -> List.iter (sub.expr sub) el
    | Pexp_construct (lid, arg) ->
        iter_loc sub lid; iter_opt (sub.expr sub) arg
    | Pexp_variant (_lab, eo) ->
        iter_opt (sub.expr sub) eo
    | Pexp_record (l, eo) ->
        List.iter (iter_tuple (iter_loc sub) (sub.expr sub)) l;
        iter_opt (sub.expr sub) eo
    | Pexp_field (e, lid) ->
        sub.expr sub e; iter_loc sub lid
    | Pexp_setfield (e1, lid, e2) ->
        sub.expr sub e1; iter_loc sub lid;
        sub.expr sub e2
    | Pexp_array el -> List.iter (sub.expr sub) el
    | Pexp_ifthenelse (e1, e2, e3) ->
        sub.expr sub e1; sub.expr sub e2;
        iter_opt (sub.expr sub) e3
    | Pexp_sequence (e1, e2) ->
        sub.expr sub e1; sub.expr sub e2
    | Pexp_while (e1, e2) ->
        sub.expr sub e1; sub.expr sub e2
    | Pexp_for (p, e1, e2, _d, e3) ->
        sub.pat sub p; sub.expr sub e1; sub.expr sub e2;
        sub.expr sub e3
    | Pexp_coerce (e, t1, t2) ->
        sub.expr sub e; iter_opt (sub.typ sub) t1;
        sub.typ sub t2
    | Pexp_constraint (e, t) ->
        sub.expr sub e; sub.typ sub t
    | Pexp_send (e, _s) -> sub.expr sub e
    | Pexp_new lid -> iter_loc sub lid
    | Pexp_setinstvar (s, e) ->
        iter_loc sub s; sub.expr sub e
    | Pexp_override sel ->
        List.iter (iter_tuple (iter_loc sub) (sub.expr sub)) sel
    | Pexp_letmodule (s, me, e) ->
        iter_loc sub s; sub.module_expr sub me;
        sub.expr sub e
    | Pexp_letexception (cd, e) ->
        sub.extension_constructor sub cd;
        sub.expr sub e
    | Pexp_assert e -> sub.expr sub e
    | Pexp_lazy e -> sub.expr sub e
    | Pexp_poly (e, t) ->
        sub.expr sub e; iter_opt (sub.typ sub) t
    | Pexp_object cls -> sub.class_structure sub cls
    | Pexp_newtype (_s, e) -> sub.expr sub e
    | Pexp_pack me -> sub.module_expr sub me
    | Pexp_open (o, e) ->
        sub.open_declaration sub o; sub.expr sub e
    | Pexp_letop {let_; ands; body} ->
        sub.binding_op sub let_;
        List.iter (sub.binding_op sub) ands;
        sub.expr sub body
    | Pexp_extension x -> sub.extension sub x
    | Pexp_unreachable -> ()

  let iter_binding_op sub {pbop_op; pbop_pat; pbop_exp; pbop_loc} =
    iter_loc sub pbop_op;
    sub.pat sub pbop_pat;
    sub.expr sub pbop_exp;
    sub.location sub pbop_loc

end

module P = struct
  (* Patterns *)

  let iter sub {ppat_desc = desc; ppat_loc = loc; ppat_attributes = attrs; _} =
    sub.location sub loc;
    sub.attributes sub attrs;
    match desc with
    | Ppat_any -> ()
    | Ppat_var s -> iter_loc sub s
    | Ppat_alias (p, s) -> sub.pat sub p; iter_loc sub s
    | Ppat_constant _ -> ()
    | Ppat_interval _ -> ()
    | Ppat_tuple pl -> List.iter (sub.pat sub) pl
    | Ppat_construct (l, p) ->
        iter_loc sub l; iter_opt (sub.pat sub) p
    | Ppat_variant (_l, p) -> iter_opt (sub.pat sub) p
    | Ppat_record (lpl, _cf) ->
        List.iter (iter_tuple (iter_loc sub) (sub.pat sub)) lpl
    | Ppat_array pl -> List.iter (sub.pat sub) pl
    | Ppat_or (p1, p2) -> sub.pat sub p1; sub.pat sub p2
    | Ppat_constraint (p, t) ->
        sub.pat sub p; sub.typ sub t
    | Ppat_type s -> iter_loc sub s
    | Ppat_lazy p -> sub.pat sub p
    | Ppat_unpack s -> iter_loc sub s
    | Ppat_exception p -> sub.pat sub p
    | Ppat_extension x -> sub.extension sub x
    | Ppat_open (lid, p) ->
        iter_loc sub lid; sub.pat sub p

end

module CE = struct
  (* Value expressions for the class language *)

  let iter sub {pcl_loc = loc; pcl_desc = desc; pcl_attributes = attrs} =
    sub.location sub loc;
    sub.attributes sub attrs;
    match desc with
    | Pcl_constr (lid, tys) ->
        iter_loc sub lid; List.iter (sub.typ sub) tys
    | Pcl_structure s ->
        sub.class_structure sub s
    | Pcl_fun (_lab, e, p, ce) ->
        iter_opt (sub.expr sub) e;
        sub.pat sub p;
        sub.class_expr sub ce
    | Pcl_apply (ce, l) ->
        sub.class_expr sub ce;
        List.iter (iter_snd (sub.expr sub)) l
    | Pcl_let (_r, vbs, ce) ->
        List.iter (sub.value_binding sub) vbs;
        sub.class_expr sub ce
    | Pcl_constraint (ce, ct) ->
        sub.class_expr sub ce; sub.class_type sub ct
    | Pcl_extension x -> sub.extension sub x
    | Pcl_open (o, e) ->
        sub.open_description sub o; sub.class_expr sub e

  let iter_kind sub = function
    | Cfk_concrete (_o, e) -> sub.expr sub e
    | Cfk_virtual t -> sub.typ sub t

  let iter_field sub {pcf_desc = desc; pcf_loc = loc; pcf_attributes = attrs} =
    sub.location sub loc;
    sub.attributes sub attrs;
    match desc with
    | Pcf_inherit (_o, ce, _s) -> sub.class_expr sub ce
    | Pcf_val (s, _m, k) -> iter_loc sub s; iter_kind sub k
    | Pcf_method (s, _p, k) ->
        iter_loc sub s; iter_kind sub k
    | Pcf_constraint (t1, t2) ->
        sub.typ sub t1; sub.typ sub t2
    | Pcf_initializer e -> sub.expr sub e
    | Pcf_attribute x -> sub.attribute sub x
    | Pcf_extension x -> sub.extension sub x

  let iter_structure sub {pcstr_self; pcstr_fields} =
    sub.pat sub pcstr_self;
    List.iter (sub.class_field sub) pcstr_fields

  let class_infos sub f {pci_virt = _; pci_params = pl; pci_name; pci_expr;
                         pci_loc; pci_attributes} =
    List.iter (iter_fst (sub.typ sub)) pl;
    iter_loc sub pci_name;
    f pci_expr;
    sub.location sub pci_loc;
    sub.attributes sub pci_attributes
end

(* Now, a generic AST mapper, to be extended to cover all kinds and
   cases of the OCaml grammar.  The default behavior of the mapper is
   the identity. *)

let default_iterator =
  {
    structure = (fun this l -> List.iter (this.structure_item this) l);
    structure_item = M.iter_structure_item;
    module_expr = M.iter;
    signature = (fun this l -> List.iter (this.signature_item this) l);
    signature_item = MT.iter_signature_item;
    module_type = MT.iter;
    with_constraint = MT.iter_with_constraint;
    class_declaration =
      (fun this -> CE.class_infos this (this.class_expr this));
    class_expr = CE.iter;
    class_field = CE.iter_field;
    class_structure = CE.iter_structure;
    class_type = CT.iter;
    class_type_field = CT.iter_field;
    class_signature = CT.iter_signature;
    class_type_declaration =
      (fun this -> CE.class_infos this (this.class_type this));
    class_description =
      (fun this -> CE.class_infos this (this.class_type this));
    type_declaration = T.iter_type_declaration;
    type_kind = T.iter_type_kind;
    typ = T.iter;
    row_field = T.row_field;
    object_field = T.object_field;
    type_extension = T.iter_type_extension;
    type_exception = T.iter_type_exception;
    extension_constructor = T.iter_extension_constructor;
    value_description =
      (fun this {pval_name; pval_type; pval_prim = _; pval_loc;
                 pval_attributes} ->
        iter_loc this pval_name;
        this.typ this pval_type;
        this.location this pval_loc;
        this.attributes this pval_attributes;
      );

    pat = P.iter;
    expr = E.iter;
    binding_op = E.iter_binding_op;

    module_declaration =
      (fun this {pmd_name; pmd_type; pmd_attributes; pmd_loc} ->
         iter_loc this pmd_name;
         this.module_type this pmd_type;
         this.location this pmd_loc;
         this.attributes this pmd_attributes;
      );

    module_substitution =
      (fun this {pms_name; pms_manifest; pms_attributes; pms_loc} ->
         iter_loc this pms_name;
         iter_loc this pms_manifest;
         this.location this pms_loc;
         this.attributes this pms_attributes;
      );

    module_type_declaration =
      (fun this {pmtd_name; pmtd_type; pmtd_attributes; pmtd_loc} ->
         iter_loc this pmtd_name;
         iter_opt (this.module_type this) pmtd_type;
         this.location this pmtd_loc;
         this.attributes this pmtd_attributes;
      );

    module_binding =
      (fun this {pmb_name; pmb_expr; pmb_attributes; pmb_loc} ->
         iter_loc this pmb_name; this.module_expr this pmb_expr;
         this.location this pmb_loc;
         this.attributes this pmb_attributes;
      );

    open_declaration =
      (fun this {popen_expr; popen_override = _; popen_attributes; popen_loc} ->
         this.module_expr this popen_expr;
         this.location this popen_loc;
         this.attributes this popen_attributes
      );

    open_description =
      (fun this {popen_expr; popen_override = _; popen_attributes; popen_loc} ->
         iter_loc this popen_expr;
         this.location this popen_loc;
         this.attributes this popen_attributes
      );


    include_description =
      (fun this {pincl_mod; pincl_attributes; pincl_loc} ->
         this.module_type this pincl_mod;
         this.location this pincl_loc;
         this.attributes this pincl_attributes
      );

    include_declaration =
      (fun this {pincl_mod; pincl_attributes; pincl_loc} ->
         this.module_expr this pincl_mod;
         this.location this pincl_loc;
         this.attributes this pincl_attributes
      );


    value_binding =
      (fun this {pvb_pat; pvb_expr; pvb_attributes; pvb_loc} ->
         this.pat this pvb_pat;
         this.expr this pvb_expr;
         this.location this pvb_loc;
         this.attributes this pvb_attributes
      );


    constructor_declaration =
      (fun this {pcd_name; pcd_args; pcd_res; pcd_loc; pcd_attributes} ->
         iter_loc this pcd_name;
         T.iter_constructor_arguments this pcd_args;
         iter_opt (this.typ this) pcd_res;
         this.location this pcd_loc;
         this.attributes this pcd_attributes
      );

    label_declaration =
      (fun this {pld_name; pld_type; pld_loc; pld_mutable = _; pld_attributes}->
         iter_loc this pld_name;
         this.typ this pld_type;
         this.location this pld_loc;
         this.attributes this pld_attributes
      );

    cases = (fun this l -> List.iter (this.case this) l);
    case =
      (fun this {pc_lhs; pc_guard; pc_rhs} ->
         this.pat this pc_lhs;
         iter_opt (this.expr this) pc_guard;
         this.expr this pc_rhs
      );

    location = (fun _this _l -> ());

    extension = (fun this (s, e) -> iter_loc this s; this.payload this e);
    attribute = (fun this a ->
      iter_loc this a.attr_name;
      this.payload this a.attr_payload;
      this.location this a.attr_loc
    );
    attributes = (fun this l -> List.iter (this.attribute this) l);
    payload =
      (fun this -> function
         | PStr x -> this.structure this x
         | PSig x -> this.signature this x
         | PTyp x -> this.typ this x
         | PPat (x, g) -> this.pat this x; iter_opt (this.expr this) g
      );
  }
end
