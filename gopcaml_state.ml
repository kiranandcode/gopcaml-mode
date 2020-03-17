open Core
open Ecaml


module State = struct

  module Filetype = struct

    (** records whether the current file is an interface or implementation *)
    type s = Interface | Implementation [@@deriving sexp]

    module Enum : Ecaml.Value.Type.Enum with type t = s = struct
      type t = s
      let all = [Interface; Implementation]
      let sexp_of_t = sexp_of_s
    end

    let ty =
      let to_ecaml file_type =
        match file_type with
        | Interface -> Value.intern "interface"
        | Implementation -> Value.intern "implementation" in
      Value.Type.enum
        (Sexp.Atom "filetype")
        (module Enum)
        to_ecaml

    let to_string  = function
      | Interface -> "interface"
      | Implementation -> "implementation"

    type t = s
  end

  module Direction = struct

    (** records whether moving forward or backwards *)
    type s = Forward | Backward [@@deriving sexp]


    module Enum : Ecaml.Value.Type.Enum with type t = s = struct
      type t = s
      let all = [Forward; Backward]
      let sexp_of_t = sexp_of_s
    end

    let ty =
      let to_ecaml direction =
        match direction with
        | Forward -> Value.intern "forward"
        | Backward -> Value.intern "backward" in
      Value.Type.enum
        (Sexp.Atom "direction")
        (module Enum)
        to_ecaml

    let to_string  = function
      | Forward -> "forward"
      | Backward -> "backward"

    type t = s
  end


  module Zipper = struct

    type t = Ast_zipper.location 

    (** elisp type for state of system  *)
    let ty : t Value.Type.t =
      Caml_embed.create_type
        (Type_equal.Id.create
           ~name:"gopcaml-zipper-location"
           Sexplib0.Sexp_conv.sexp_of_opaque)
  end

  (** region of the buffer  *)
  type region = {
    start_mark: Marker.t;
    end_mark: Marker.t ;
    (** denotes the start and end of the region  *)
    logical_start: Line_and_column.t;
    logical_end: Line_and_column.t 
  }

  (** holds the parse tree for the current file  *)
  type 'a ast_tree =
    (** the variant simply defines the type of ast.
        the value is a list of the top-level items, where each item is
        reported as: region * ast in that region

        when a change occurs, we:
        - find region containing change,
        - reparse region, update element 
        - if that fails (could be that toplevel structure changed),
          then parse from region start to end of file,
          update rest of list
    *)
    | Impl : (region * Parsetree.structure_item) list  -> Parsetree.structure_item ast_tree
    | Intf : (region * Parsetree.signature_item) list -> Parsetree.signature_item ast_tree

  type parse_tree =
    | MkParseTree : 'a ast_tree -> parse_tree

  type 'a ast_item =
    | ImplIt : (region * Parsetree.structure_item) -> Parsetree.structure_item ast_item
    | IntfIt : (region * Parsetree.signature_item)  -> Parsetree.signature_item ast_item

  type parse_item =
    | MkParseItem : 'a ast_item -> parse_item

  module TreeBuilder = struct
    let unwrap_current_buffer current_buffer =
      match current_buffer with Some v -> v | None -> Current_buffer.get () 

    (** builds the abstract tree for the current buffer buffer  *)
    let build_abstract_tree f g h ?current_buffer value =
      let current_buffer = unwrap_current_buffer current_buffer in
      let lexbuf = Lexing.from_string ~with_positions:true value in
      let items =
        f lexbuf
        |> List.map ~f:(fun item ->
            let (iterator,get_result) = Ast_transformer.bounds_iterator () in
            g iterator item;
            let (min_column, max_column) = get_result () in
            let start_marker,end_marker = Marker.create (), Marker.create () in
            let get_position column = 
              Position.of_int_exn column
            in
            (* Point.goto_line_and_column Line_and_column.{line;column};
             * Point.get () in *)

            Marker.set start_marker current_buffer (get_position min_column);
            Marker.set end_marker current_buffer (get_position max_column);
            {start_mark=start_marker;
             end_mark=end_marker;
             logical_start = Line_and_column.{line=0;column=min_column}; 
             logical_end = Line_and_column.{line=0;column=max_column};
            },item)
      in
      if not @@ String.is_empty value then
        try Either.First (h items) with
          Syntaxerr.Error e -> Either.Second e
      else Either.First (h [])



    let build_implementation_tree =
      build_abstract_tree
        Parse.implementation
        (fun iterator item -> iterator.structure_item iterator item)
        (fun x -> Impl x)

    let build_interface_tree =
      build_abstract_tree
        Parse.interface
        (fun iterator item -> iterator.signature_item iterator item)
        (fun x -> Intf x)

    (** determines the file-type of the current file based on its extension *)
    let retrieve_current_file_type ~implementation_extensions ~interface_extensions =
      Current_buffer.file_name ()
      |> Option.bind ~f:(fun file_name ->
          String.split ~on:'.' file_name
          |> List.last
          |> Option.bind ~f:(fun ext ->
              if List.mem ~equal:String.(=) implementation_extensions ext
              then begin
                message "filetype is implementation";
                Some Filetype.Implementation
              end
              else if List.mem ~equal:String.(=) interface_extensions ext
              then begin
                message "filetype is interface";
                Some Filetype.Interface
              end
              else None
            )
        )


    (** attempts to parse the current buffer according to the inferred file type  *)
    let parse_current_buffer ?start ?end_ file_type =
      (* retrieve the text for the entire buffer *)
      let buffer_text =
        Current_buffer.contents ?start ?end_ () |> Text.to_utf8_bytes  in
      let perform_parse () = 
        message "Building parse tree - may take a while if the file is large...";
        let start_time = Time.now () in
        let parse_tree =
          let map ~f = Either.map ~second:(fun x -> x) ~first:(fun x -> f x) in
          let open Filetype in
          match file_type with
          | Implementation -> map ~f:(fun x -> MkParseTree x) @@
            build_implementation_tree buffer_text
          | Interface -> map ~f:(fun x -> MkParseTree x) @@
            build_interface_tree buffer_text
        in
        match parse_tree with
        | Either.Second _e ->
          message ("Could not build parse tree (syntax error)");
          None
        | Either.First tree ->
          let end_time = Time.now () in
          message (Printf.sprintf
                     "Successfully built parse tree (%f ms)"
                     ((Time.diff end_time start_time) |> Time.Span.to_ms)
                  );
          Some tree
      in
      if not @@ String.is_empty buffer_text then
        try perform_parse ()
        with
          Parser.Error -> message (Printf.sprintf "parsing got error parse.error"); None
        | Syntaxerr.Error _ -> None
      else match file_type with
        | Interface -> Some (MkParseTree (Intf []))
        | Implementation -> Some (MkParseTree (Impl []))

    let calculate_region mi ma structure_list _ (* dirty_region *) =
      (* first split the list of structure-items by whether they are invalid or not  *)
      let is_invalid ms2 me2 =
        let region_contains s1 e1 s2 e2  =
          let open Position in
          (((s1 <= s2) && (s2 <= e1)) ||
           ((s1 <= e2) && (e2 <= e1)) ||
           ((s2 <= s1) && (s1 <= e2)) ||
           ((s2 <= e1) && (e1 <= e2))
          ) in
        match Marker.position ms2, Marker.position me2 with
        | Some s2, Some e2 ->
          region_contains mi ma s2 e2
        | _ -> true in
      let (pre, invalid) =
        List.split_while ~f:(fun ({ start_mark; end_mark; _ }, _) ->
            not @@ is_invalid start_mark end_mark
          ) structure_list in
      let invalid = List.rev invalid in
      let (post, inb) =
        List.split_while ~f:(fun ({ start_mark; end_mark; _ }, _) ->
            not @@ is_invalid start_mark end_mark
          ) invalid in
      let post = List.rev post in
      (pre,inb,post) 

    let calculate_start_end f mi ma pre_edit_region invalid_region post_edit_region =
      let start_region =
        match List.last pre_edit_region with
        | Some (_, st) ->
          let (iterator,get_bounds) =  Ast_transformer.bounds_iterator () in
          f iterator st;
          let (_,c) = get_bounds () in
          Position.of_int_exn c
        | None ->
          match invalid_region with
          | (_,st) :: _ ->
            let (iterator,get_bounds) =  Ast_transformer.bounds_iterator () in
            f iterator st;
            let (_,c) = get_bounds () in
            Position.of_int_exn c
          | [] -> mi
      in
      let end_region =
        match post_edit_region with
        | (_, st) :: _ ->
          let (iterator,get_bounds) =  Ast_transformer.bounds_iterator () in
          f iterator st;
          let (_,c) = get_bounds () in
          Position.of_int_exn c
        | [] ->
          match List.last invalid_region with
          | Some (_,st) ->
            let (iterator,get_bounds) =  Ast_transformer.bounds_iterator () in
            f iterator st;
            let (_,c) = get_bounds () in
            Position.of_int_exn c
          | None -> ma in
      (start_region,end_region)

    let abstract_rebuild_region f start_region end_region pre_edit_region post_edit_region  =
      (* first, attempt to parse the exact modified region *)
      match parse_current_buffer
              ~start:start_region ~end_:end_region Filetype.Interface
      with
      | Some v -> let reparsed_range = f v in pre_edit_region @ reparsed_range @ post_edit_region
      | None ->
        (* otherwise, try to reparse from the start to the end *)
        match parse_current_buffer
                ~start:start_region Filetype.Interface
        with
        | Some v -> let reparsed_range = f v in pre_edit_region @ reparsed_range
        | None ->
          (* otherwise, try to reparse from the start to the end *)
          match parse_current_buffer Filetype.Interface
          with
          | Some v -> let reparsed_range = f v in reparsed_range
          | None -> pre_edit_region @ post_edit_region

    let rebuild_intf_parse_tree min max structure_list dirty_region =
      let mi,ma = Position.of_int_exn min, Position.of_int_exn max in
      let (pre_edit_region,invalid_region,post_edit_region) =
        calculate_region mi ma structure_list dirty_region in
      let (start_region,end_region) =
        calculate_start_end
          (fun iterator st -> iterator.signature_item iterator st)
          mi ma pre_edit_region invalid_region post_edit_region in
      abstract_rebuild_region
        (fun (MkParseTree tree) -> 
           match tree with
           | Impl _ -> assert false
           | Intf reparsed_range -> reparsed_range)
        start_region end_region pre_edit_region post_edit_region

    let rebuild_impl_parse_tree min max structure_list dirty_region =
      let mi,ma = Position.of_int_exn min, Position.of_int_exn max in
      let (pre_edit_region,invalid_region,post_edit_region) =
        calculate_region mi ma structure_list dirty_region in
      let (start_region,end_region) =
        calculate_start_end
          (fun iterator st -> iterator.structure_item iterator st)
          mi ma pre_edit_region invalid_region post_edit_region in
      abstract_rebuild_region
        (fun (MkParseTree tree) -> 
           match tree with
           | Impl reparsed_range -> reparsed_range
           | Intf _ -> assert false)
        start_region end_region pre_edit_region post_edit_region

  end


  module DirtyRegion = struct
    (** Tracks the ast - either clean, or dirty (and whether it has
        been changed since last compile attempt) *)
    type t =
      | Clean of parse_tree
      | Dirty of (parse_tree option * bool) 

    let get_dirty_region = function
      | Clean _ -> None
      | Dirty _ -> Some (0,-1)

    let is_dirty = function
      | Clean _ -> false
      | _ -> true

    (** creates a clean dirty region from a parse tree *)
    let create tree = Clean tree

    (** updates the parse tree to denote the range of the dirty region *)
    let update (s:t) (_s,_e,_l: (int * int * int)) : t =
      (* todo: track detailed changes *)
      match (s : t) with
      | Clean tree -> Dirty (Some tree, true)
      | Dirty (tree,_) -> Dirty (tree, true)


    (** builds an updated parse_tree (updating any dirty regions) *)
    let to_tree (dr:t) (_file_type: Filetype.t) : parse_tree option =
      match dr with
      | Clean tree -> Some tree
      | Dirty _ ->
        TreeBuilder.parse_current_buffer _file_type

    (** returns the parse tree - even if it may be dirty *)
    let to_tree_immediate (dr:t) (_file_type: Filetype.t) : parse_tree option =
      match dr with
      | Clean tree -> Some tree
      | Dirty (tree, _) ->  tree

  end

  (** type of state of plugin - pre-validation *)
  type t = {
    (** file type of the current buffer *)
    file_type: Filetype.t;
    (** parse tree of the current buffer + any dirty regions *)
    parse_tree: DirtyRegion.t;
  }

  module Validated = struct
    (** type of valid state of plugin  *)
    type s = {
      (** file type of the current buffer *)
      file_type: Filetype.t;
      (** parse tree of the current buffer *)
      parse_tree: parse_tree;
    } 

    (** builds a validated instance of gopcaml-state  -
        returning a new copy of the state if it has changed*)
    let of_state (state: t)  =
      let (let+) x f = Option.bind ~f x in
      let should_store = ref false in
      let+ parse_tree = DirtyRegion.to_tree state.parse_tree state.file_type in
      if DirtyRegion.is_dirty state.parse_tree then should_store := true;
      if !should_store then
        Some ({file_type=state.file_type; parse_tree},
              Some ({file_type=state.file_type; parse_tree = (DirtyRegion.create parse_tree)}:t))
      else
        Some ({file_type=state.file_type; parse_tree}, None)

    (** attempts to retrieve the state immediately - even if it is old or outdated  *)
    let of_state_immediate ({ file_type; parse_tree }:t) =
      (match parse_tree with
       | DirtyRegion.Clean tree -> Some tree
       | DirtyRegion.Dirty (tree,_) ->  tree
      )
      |> Option.map ~f:(fun tree -> ({file_type; parse_tree = tree}))

    (** attempts to retrieve the state immediately - even if it is old or outdated  *)
    let try_ensure ({ file_type; parse_tree } as state :t) =
      (match parse_tree with
       | DirtyRegion.Clean _ -> None, true
       | DirtyRegion.Dirty (_,false) -> None, false
       | DirtyRegion.Dirty (tree,true)  -> 
         let parse_tree = DirtyRegion.to_tree state.parse_tree state.file_type in
         begin
           match parse_tree with
           | Some tree ->
             Some ({file_type=file_type; parse_tree = (DirtyRegion.create tree)} :t), true
           | None  ->
             Some ({file_type=file_type; parse_tree = DirtyRegion.Dirty (tree,false)}:t), false
         end
      )

    type t = s

  end


  (** elisp type for state of system  *)
  let ty : t Value.Type.t =
    Caml_embed.create_type
      (Type_equal.Id.create
         ~name:"gopcaml-state"
         Sexplib0.Sexp_conv.sexp_of_opaque)

  let default = {
    file_type = Interface;
    parse_tree = DirtyRegion.Dirty (None,false);
  }

end

(** sets up the gopcaml-mode state - intended to be called by the startup hook of gopcaml mode*)
let setup_gopcaml_state
    ~state_var ~interface_extension_var ~implementation_extension_var =
  let current_buffer = Current_buffer.get () in
  (* we've set these values in their definition, so it doesn't make sense for them to be non-present *)
  let interface_extensions =
    Customization.value interface_extension_var in
  let implementation_extensions =
    Customization.value implementation_extension_var in
  let file_type =
    let inferred = State.TreeBuilder.retrieve_current_file_type
        ~implementation_extensions ~interface_extensions in
    match inferred with
    | Some vl -> vl
    | None ->
      message "Could not infer the ocaml type (interface or \
               implementation) of the current file - will attempt
               to proceed by defaulting to implementation.";
      State.Filetype.Implementation
  in
  let parse_tree = State.TreeBuilder.parse_current_buffer file_type in
  if Option.is_none parse_tree then
    message "Could not build parse tree - please ensure that the \
             buffer is syntactically correct and call \
             gopcaml-initialize to enable the full POWER of syntactic \
             editing.";
  let state = State.{
      file_type = file_type;
      parse_tree = match parse_tree with
          None -> DirtyRegion.Dirty (None, false)
        | Some tree -> DirtyRegion.create tree;
    } in
  Buffer_local.set state_var (Some state) current_buffer

(** retrieve the gopcaml state *)
let get_gopcaml_file_type ?current_buffer ~state_var () =
  let current_buffer = match current_buffer with Some v -> v | None -> Current_buffer.get () in
  let state = Buffer_local.get_exn state_var current_buffer in
  let file_type_name = State.Filetype.to_string state.State.file_type in
  file_type_name

(** update the file type of the variable   *)
let set_gopcaml_file_type ?current_buffer ~state_var file_type =
  let current_buffer = match current_buffer with Some v -> v | None -> Current_buffer.get () in
  let state = Buffer_local.get_exn state_var current_buffer in
  let state = State.{state with parse_tree=Dirty (None, true); file_type = file_type } in
  Buffer_local.set state_var (Some state) current_buffer
[@@warning "-23"]

(** from a list of expressions, finds the enclosing one  *)
let find_enclosing_expression list point =
  let open State in
  let (left,remain) = List.split_while list ~f:(fun (region,_) ->
      let (let+) v f = Option.bind ~f v in
      let contains = 
        let+ start_position = Marker.position region.start_mark in
        let+ end_position = Marker.position region.end_mark in           
        Some (not @@ Position.between ~low:start_position ~high:end_position point) in
      Option.value ~default:true contains) in 
  let remove_region = List.map ~f:(fun (_,b) -> b) in
  match remain with
  | (_,current) :: right -> Some (remove_region left,current, remove_region right)
  | [] -> None

(** from a list of expressions, returns the nearest expression  *)
let find_nearest_expression list point =
  let open State in
  match find_enclosing_expression list point with
  | None ->
    (* no enclosing expression *)
    let (let+) v f = Option.bind ~f v in
    let distance ((region,_) as value) =
      let distance = 
        let+ start_position = Marker.position region.start_mark in
        let+ end_position = Marker.position region.end_mark in
        Some (min (abs (Position.to_int start_position - Position.to_int point))
                (abs (Position.to_int end_position - Position.to_int point))) in
      distance, value in
    let regions = List.map list ~f:distance in
    let+ (min, _) = List.min_elt ~compare:(fun (d,_) (d',_) ->
        let d = match d with Some v -> v | None -> Int.max_value in
        let d' = match d' with Some v -> v | None -> Int.max_value in
        Int.compare d d') regions in
    let eq = Option.equal (Int.equal) in
    let remove_meta (_,(_,v)) = v in
    begin match List.split_while regions ~f:(fun (d,_)  -> not @@ eq d min)  with
      | (left, current :: right) ->
        Some (
          List.map ~f:remove_meta left,
          remove_meta current,
          List.map ~f:remove_meta right)
      | _ -> None
    end
  | v -> v

let list_split_last ls = 
  let rec loop ls acc =
    match ls with
    | h :: [] -> Some (h,List.rev acc)
    | h :: t -> loop t (h :: acc)
    | [] -> None in
  loop ls []

let build_zipper (state: State.Validated.t) point =
  let find_nearest_prev_expression f list =
    let (let+) v f = Option.bind ~f v in
    let+ (left,current,right) = find_nearest_expression list point in
    if (f current) = (Position.to_int point)
    then begin
      match list_split_last left with
      | Some (last,left) -> Some (left, last, current::right)
      | None -> Some (left,current,right)
    end
    else Some (left,current,right)
  in
  let sif ({  psig_loc = { loc_start; _ }; _ }:Parsetree.signature_item) = loc_start.pos_cnum in
  let stf ({  pstr_loc = { loc_start; _ }; _ }:Parsetree.structure_item) = loc_start.pos_cnum in    
  begin match state.parse_tree with 
    | (State.MkParseTree (State.Impl si_list)) ->
      find_nearest_prev_expression stf si_list
      |> Option.map ~f:(fun (left,current,right) ->
          Ast_zipper.make_zipper_impl left current right )
    | (State.MkParseTree (State.Intf si_list)) ->
      find_nearest_prev_expression sif si_list
      |> Option.map ~f:(fun (left,current,right) ->
          Ast_zipper.make_zipper_intf left current right
        )
  end

let find_enclosing_structure (state: State.Validated.t) point : State.parse_item option =
  let open State in
  let open Validated in
  let find_enclosing_expression list = 
    List.find list ~f:(fun (region,_) ->
        let (let+) v f = Option.bind ~f v in
        let contains = 
          let+ start_position = Marker.position region.start_mark in
          let+ end_position = Marker.position region.end_mark in           
          Some (Position.between ~low:start_position ~high:end_position point) in
        Option.value ~default:false contains) in
  match state.parse_tree with 
  | (State.MkParseTree (State.Impl si_list)) ->
    find_enclosing_expression si_list  |> Option.map ~f:(fun x -> State.MkParseItem (State.ImplIt x))
  | (State.MkParseTree (State.Intf si_list)) ->
    find_enclosing_expression si_list |> Option.map ~f:(fun x -> State.MkParseItem (State.IntfIt x))

(* determines whether we are inside a letdef *)
let inside_let_def state point =
  let contains ({ loc_start; loc_end; _ }: Location.t) =
    (loc_start.pos_cnum <= point) && (point <= loc_end.pos_cnum)
  in
  let rec is_let_def_struct ({pstr_desc;_}: Parsetree.structure_item) = (match pstr_desc with
      | Parsetree.Pstr_eval (expr, _) -> is_let_def_expr expr
      | Parsetree.Pstr_value (_, vbs) -> 
        List.fold ~init:false ~f:(fun acc value -> acc || is_in_value_binding value) vbs
      | Parsetree.Pstr_module mb -> is_let_def_mod_expr mb.pmb_expr 
      | Parsetree.Pstr_recmodule mods ->
        List.fold ~init:false ~f:(fun acc value -> acc || is_let_def_mod_expr value.pmb_expr) mods
      | Parsetree.Pstr_class_type cty_decl -> 
        List.fold ~init:false ~f:(fun acc { pci_expr; _ } -> acc || is_let_def_class_type pci_expr)
          cty_decl
      | Parsetree.Pstr_class c_decls -> 
        List.fold ~init:false ~f:(fun acc { pci_expr; _ } -> acc || is_let_def_class_expr  pci_expr)
          c_decls
      | _  -> false)

  and is_let_def_sig ({ psig_desc; psig_loc }: Parsetree.signature_item) =
    if contains psig_loc then (match psig_desc with
        | Parsetree.Psig_module { pmd_type; _ } -> is_let_def_mod_type pmd_type
        | Parsetree.Psig_recmodule decls -> 
          List.fold ~init:false ~f:(fun acc { pmd_type; _ } -> acc || is_let_def_mod_type  pmd_type)
            decls
        | Parsetree.Psig_modtype {  pmtd_type; _  } ->
          Option.map ~f:is_let_def_mod_type pmtd_type |> Option.value ~default:false
        | Parsetree.Psig_include { pincl_mod; _ } -> is_let_def_mod_type pincl_mod
        | Parsetree.Psig_class c_decls -> 
          List.fold ~init:false ~f:(fun acc { pci_expr; _ } -> acc || is_let_def_class_type pci_expr)
            c_decls
        | Parsetree.Psig_class_type c_decls -> 
          List.fold ~init:false ~f:(fun acc { pci_expr; _ } -> acc || is_let_def_class_type  pci_expr)
            c_decls
        | _  -> false
      ) else false
  and is_in_value_binding ({ pvb_expr; pvb_loc; _ }: Parsetree.value_binding) =
    if contains pvb_loc then contains pvb_expr.pexp_loc else false
  and is_let_def_case ({ pc_guard; pc_rhs;_ }: Parsetree.case) =
    (Option.map  ~f:is_let_def_expr pc_guard |> Option.value ~default:false) || (is_let_def_expr pc_rhs)
  and is_let_def_mod_type ({ pmty_desc; pmty_loc; _ }: Parsetree.module_type) =
    if contains pmty_loc then (match pmty_desc with
        | Parsetree.Pmty_functor (_, omt, mt) ->
          (Option.map ~f:is_let_def_mod_type omt |> Option.value ~default:false) ||
          is_let_def_mod_type mt
        | Parsetree.Pmty_with (mt, _) -> is_let_def_mod_type mt
        | Parsetree.Pmty_typeof mexpr -> is_let_def_mod_expr mexpr
        | _  -> false) else false
  and is_let_def_mod_expr ({ pmod_desc; pmod_loc; _ }: Parsetree.module_expr) =
    if contains pmod_loc then
      (match pmod_desc with
       | Parsetree.Pmod_structure st -> 
         List.fold ~init:false ~f:(fun acc value -> acc || is_let_def_struct value) st
       | Parsetree.Pmod_functor (_, mt, mexpr) ->
         (Option.map ~f:is_let_def_mod_type mt |> Option.value ~default:false) ||
         is_let_def_mod_expr mexpr
       | Parsetree.Pmod_constraint (mexpr, mt) -> 
         is_let_def_mod_expr mexpr || is_let_def_mod_type mt
       | Parsetree.Pmod_apply (mexp1, mexp2) ->
         is_let_def_mod_expr mexp1 || is_let_def_mod_expr mexp2
       | Parsetree.Pmod_unpack expr -> is_let_def_expr expr
       | _  -> false)
    else false
  and is_let_def_class_field_type_kind ({ pctf_desc; pctf_loc; _ }: Parsetree.class_type_field) =
    if contains pctf_loc then (match pctf_desc with
        | Parsetree.Pctf_inherit ct -> is_let_def_class_type ct
        | _  -> false) else false
  and is_let_def_class_signature ({  pcsig_fields;_ }: Parsetree.class_signature) =
    List.fold ~init:false pcsig_fields
      ~f:(fun acc value -> acc || is_let_def_class_field_type_kind value)
  and is_let_def_class_type ({ pcty_desc; pcty_loc; _ }: Parsetree.class_type) =
    if contains pcty_loc then
      (match pcty_desc with
       | Parsetree.Pcty_signature cs ->  is_let_def_class_signature cs
       | Parsetree.Pcty_arrow (_, _, cty) -> is_let_def_class_type cty
       | Parsetree.Pcty_open (_, cty) ->
         is_let_def_class_type cty
       | _  -> false
      )
    else false
  and is_let_def_class_expr ({ pcl_desc; pcl_loc; _ }: Parsetree.class_expr) =
    if contains pcl_loc then (match pcl_desc with
        | Parsetree.Pcl_structure cs -> is_let_def_class_structure cs
        | Parsetree.Pcl_fun (_, oexpr, _, clsexpr) ->
          (Option.map ~f:is_let_def_expr oexpr |> Option.value ~default:false) ||
          is_let_def_class_expr clsexpr
        | Parsetree.Pcl_apply (clsexpr, fields) -> 
          is_let_def_class_expr clsexpr ||
          List.fold ~init:false ~f:(fun acc (_,value) -> acc || is_let_def_expr value) fields
        | Parsetree.Pcl_let (_, vbs, cexp) ->
          List.fold ~init:false ~f:(fun acc value -> acc || is_in_value_binding value) vbs
          || is_let_def_class_expr cexp
        | Parsetree.Pcl_constraint (cexp, ctyp) ->
          is_let_def_class_expr cexp || is_let_def_class_type ctyp
        | Parsetree.Pcl_open (_, cexp) -> is_let_def_class_expr cexp
        | _  -> false) else false
  and is_let_def_class_field_kind cfk = match cfk with
    | Parsetree.Cfk_virtual _ -> false
    | Parsetree.Cfk_concrete (_, exp) -> is_let_def_expr exp
  and is_let_def_class_field ({ pcf_desc; pcf_loc; _ }: Parsetree.class_field) =
    if contains pcf_loc then (match pcf_desc with
        | Parsetree.Pcf_inherit (_, cexp, _) -> is_let_def_class_expr cexp
        | Parsetree.Pcf_val (_, _, cfk) -> is_let_def_class_field_kind cfk
        | Parsetree.Pcf_method (_, _, cfk) -> is_let_def_class_field_kind cfk
        | Parsetree.Pcf_initializer exp -> is_let_def_expr exp
        | _ -> false
      ) else false
  and is_let_def_class_structure ({  pcstr_fields; _ }: Parsetree.class_structure) =
    List.fold ~init:false pcstr_fields ~f:(fun acc value -> acc || is_let_def_class_field value)
  and is_let_def_expr ({ pexp_desc; pexp_loc; _ }:Parsetree.expression) =
    if contains pexp_loc then
      (match pexp_desc with
       (* check if in any of the value bindings *)
       | Parsetree.Pexp_let (_, vbs, expr) ->
         List.fold ~init:false ~f:(fun acc value -> acc || is_in_value_binding value) vbs
         || is_let_def_expr expr
       | Parsetree.Pexp_function cases -> 
         List.fold ~init:false ~f:(fun acc value -> acc || is_let_def_case value) cases
       | Parsetree.Pexp_apply (expr, args) ->
         is_let_def_expr expr
         || List.fold ~init:false ~f:(fun acc (_, value) -> acc || is_let_def_expr value) args
       | Parsetree.Pexp_try (expr, cases)
       | Parsetree.Pexp_match (expr, cases) ->
         is_let_def_expr expr || 
         List.fold ~init:false ~f:(fun acc value -> acc || is_let_def_case value) cases
       | Parsetree.Pexp_fun (_, oe1, _, e2) ->
         (Option.map ~f:is_let_def_expr oe1 |> Option.value ~default:false) || is_let_def_expr e2
       | Parsetree.Pexp_open (_, expr)
       | Parsetree.Pexp_newtype (_, expr)
       | Parsetree.Pexp_lazy expr 
       | Parsetree.Pexp_poly (expr, _)
       | Parsetree.Pexp_assert expr 
       | Parsetree.Pexp_setinstvar (_, expr)
       | Parsetree.Pexp_send (expr, _)
       | Parsetree.Pexp_field (expr, _) 
       | Parsetree.Pexp_coerce (expr, _, _)
       | Parsetree.Pexp_constraint (expr, _)
       | Parsetree.Pexp_construct (_, Some expr) 
       | Parsetree.Pexp_variant (_, Some expr) -> is_let_def_expr expr
       | Parsetree.Pexp_record (fields, oe1) ->
         List.fold ~init:false ~f:(fun acc (_, value) -> acc || is_let_def_expr value) fields ||
         (Option.map ~f:is_let_def_expr oe1 |> Option.value ~default:false)
       | Parsetree.Pexp_tuple arr
       | Parsetree.Pexp_array arr ->
         List.fold ~init:false ~f:(fun acc value -> acc || (is_let_def_expr value)) arr
       | Parsetree.Pexp_ifthenelse (e1, e2, oe3) -> 
         is_let_def_expr e1 || is_let_def_expr e2 ||
         (Option.map ~f:is_let_def_expr oe3 |> Option.value  ~default:false)
       | Parsetree.Pexp_setfield (e1, _, e2)
       | Parsetree.Pexp_while (e1, e2) 
       | Parsetree.Pexp_sequence (e1, e2) -> is_let_def_expr e1 || is_let_def_expr e2
       | Parsetree.Pexp_for (_, e1, e2, _, e3) -> 
         List.fold ~init:false ~f:(fun acc value -> acc || (is_let_def_expr value)) [e1;e2;e3]
       | Parsetree.Pexp_override overrides -> 
         List.fold ~init:false ~f:(fun acc (_, value) -> acc || (is_let_def_expr value)) overrides
       | Parsetree.Pexp_letmodule (_, mod_expr, expr) ->
         is_let_def_mod_expr mod_expr || is_let_def_expr expr
       | Parsetree.Pexp_letexception (_, expr) -> is_let_def_expr expr
       | Parsetree.Pexp_object cs -> is_let_def_class_structure cs
       | Parsetree.Pexp_pack mexp -> is_let_def_mod_expr mexp
       | Parsetree.Pexp_letop _ -> true
       | _  -> false
      )
    else false
  in
  find_enclosing_structure state (Position.of_int_exn point)
  |> Option.map ~f:(fun (State.MkParseItem it) ->
      match it with
      | State.ImplIt (_, st) -> is_let_def_struct st
      | State.IntfIt (_, si) -> is_let_def_sig si
    )

let apply_iterator (item: State.parse_item) iter f  =
  let open State in
  let (MkParseItem elem) = item in
  begin match elem with
    | ImplIt (_,it) -> iter.Ast_iterator.structure_item iter it
    | IntfIt (_, it) -> iter.Ast_iterator.signature_item iter it
  end;
  f ()

(** returns a tuple of points enclosing the current expression *)
let find_enclosing_bounds (state: State.Validated.t) ~point =
  find_enclosing_structure state point
  |> Option.bind ~f:begin fun expr ->
    let (iter,getter) = Ast_transformer.enclosing_bounds_iterator (Position.to_int point) () in
    apply_iterator expr iter getter
    |> Option.map ~f:(fun (a,b) -> (Position.of_int_exn (a + 1), Position.of_int_exn (b + 1)))
  end

(** returns a tuple of points enclosing the current structure *)
let find_enclosing_structure_bounds (state: State.Validated.t) ~point =
  find_enclosing_structure state point
  |> Option.bind ~f:begin fun expr -> let (State.MkParseItem expr) = expr in
    let region = match expr with
      | ImplIt (r,_) -> r
      | IntfIt (r,_) -> r in
    match Marker.position region.start_mark,Marker.position region.end_mark with
    | Some s, Some e -> Some (Position.add s 1, Position.add e 1)
    | _ -> None
  end

(** updates the dirty region of the parse tree *)
let update_dirty_region ?current_buffer ~state_var (s,e,l) =
  let (let+) x f = ignore @@ Option.map ~f x in
  let open State in
  let current_buffer = match current_buffer with Some v -> v | None -> Current_buffer.get () in
  let+ state = Buffer_local.get state_var current_buffer in
  let parse_tree = DirtyRegion.update state.parse_tree (s,e,l) in
  let state = {state with parse_tree = parse_tree} in
  Buffer_local.set state_var (Some state) current_buffer


(** retrieves the dirty region if it exists *)
let get_dirty_region ?current_buffer ~state_var ()  =
  let open State in
  let current_buffer = match current_buffer with Some v -> v | None -> Current_buffer.get () in
  let state = Buffer_local.get_exn state_var current_buffer in
  DirtyRegion.get_dirty_region state.parse_tree


(** retrieves the gopcaml state value, attempting to construct the
    parse tree if it has not already been made *)
let retrieve_gopcaml_state ?current_buffer ~state_var () =
  let current_buffer = match current_buffer with Some v -> v | None -> Current_buffer.get () in
  let state = Buffer_local.get_exn state_var current_buffer in
  let (let+) x f = Option.bind ~f x in
  let+ (v_state,state) = State.Validated.of_state state in
  if Option.is_some state then Buffer_local.set state_var state current_buffer;
  Some v_state

(** retrieves the gopcaml state value, attempting to construct the
    parse tree if it has not already been made *)
let check_gopcaml_state_available ?current_buffer ~state_var () =
  let current_buffer = match current_buffer with Some v -> v | None -> Current_buffer.get () in
  let state = Buffer_local.get_exn state_var current_buffer in
  let (state,ensured) = State.Validated.try_ensure state in
  if Option.is_some state then Buffer_local.set state_var state current_buffer;
  ensured

(** retrieves the gopcaml state value, without attempting to construct the
    parse tree if it has not already been made *)
let retrieve_gopcaml_state_immediate ?current_buffer ~state_var () =
  let current_buffer = match current_buffer with Some v -> v | None -> Current_buffer.get () in
  let state = Buffer_local.get_exn state_var current_buffer in
  State.Validated.of_state_immediate state

(** retrieve the points enclosing structure at the current position *)
let retrieve_enclosing_structure_bounds ?current_buffer ~state_var point =
  retrieve_gopcaml_state ?current_buffer ~state_var ()
  |> Option.bind ~f:(find_enclosing_structure_bounds ~point)

(** retrieve the points enclosing expression at the current position *)
let retrieve_enclosing_bounds ?current_buffer ~state_var point =
  retrieve_gopcaml_state ?current_buffer ~state_var ()
  |> Option.bind ~f:(find_enclosing_bounds ~point)


let print_zipper =
  Option.map ~f:(fun zipper ->
      (* Ecaml.message (Ast_zipper.describe_current_item zipper); *)
      zipper)

(** retrieve a zipper expression at the current position *)
let build_zipper_enclosing_point ?direction ?current_buffer ~state_var ~zipper_var point line  =
  let direction = match direction with
    | None -> false
    | Some v -> v in 
  let current_buffer = match current_buffer with Some v -> v | None -> Current_buffer.get () in
  retrieve_gopcaml_state ~current_buffer ~state_var ()
  |> Option.bind ~f:(fun state ->
      let zipper = build_zipper state point
                   |> Option.map ~f:(Ast_zipper.move_zipper_to_point ((Position.to_int point) - 1)
                                       line
                                       direction) in
      Buffer_local.set zipper_var zipper current_buffer;
      zipper)
  |> print_zipper
  |> Option.map ~f:Ast_zipper.to_bounds
  |> Option.map ~f:(fun (st,ed) ->
      Position.of_int_exn (st + 1), Position.of_int_exn (ed + 1)
    )

(** retrieve a zipper enclosing structure at the current position *)
let build_zipper_broadly_enclosing_point ?current_buffer ~state_var ~zipper_var point line  =
  let current_buffer = match current_buffer with Some v -> v | None -> Current_buffer.get () in
  retrieve_gopcaml_state ~current_buffer ~state_var ()
  |> Option.bind ~f:(fun state ->
      let zipper = build_zipper state point
                   |> Option.map ~f:(Ast_zipper.move_zipper_broadly_to_point
                                       ((Position.to_int point) - 1)
                                       line false) in
      Buffer_local.set zipper_var zipper current_buffer;
      zipper)
  |> print_zipper
  |> Option.map ~f:Ast_zipper.to_bounds
  |> Option.map ~f:(fun (st,ed) ->
      Position.of_int_exn (st + 1), Position.of_int_exn (ed + 1)
    )

(** returns the point corresponding to the start of the nearest defun (or respective thing in ocaml) *)
let find_nearest_defun ?current_buffer ~state_var point line =
  let current_buffer = match current_buffer with Some v -> v | None -> Current_buffer.get () in
  retrieve_gopcaml_state ~current_buffer ~state_var ()
  |> Option.bind ~f:(fun state -> build_zipper state (Position.sub point 1) )
  |> Option.bind ~f:(fun zipper -> Ast_zipper.find_nearest_definition_item_bounds
                        (Position.to_int point - 1)
                        (line + 1)
                        false
                        zipper)
  |> Option.map ~f:(fun x -> x + 1)

(** returns the point corresponding to the start of the nearest defun (or respective thing in ocaml) *)
let find_nearest_defun_end ?current_buffer ~state_var point line =
  let current_buffer = match current_buffer with Some v -> v | None -> Current_buffer.get () in
  retrieve_gopcaml_state ~current_buffer ~state_var ()
  |> Option.bind ~f:(fun state -> build_zipper state (Position.sub point 1))
  |> Option.bind ~f:(fun zipper -> Ast_zipper.find_nearest_definition_item_bounds
                        (Position.to_int point - 1)
                        line
                        true
                        zipper)
  |> Option.map ~f:(fun x -> x + 1)


(** returns the point corresponding to the start of the nearest letdef (or respective thing in ocaml) *)
let find_nearest_letdef ?current_buffer ~state_var point line =
  let current_buffer = match current_buffer with Some v -> v | None -> Current_buffer.get () in
  retrieve_gopcaml_state ~current_buffer ~state_var ()
  |> Option.bind ~f:(fun state -> build_zipper state (Position.sub point 1)
                    )
  |> Option.map ~f:( Ast_zipper.move_zipper_to_point (Position.to_int point) line false )
  |> Option.bind ~f:(fun zipper -> Ast_zipper.find_nearest_letdef
                        (Position.to_int point - 1)
                        zipper)
  |> Option.map ~f:(fun x -> x + 1)

(** returns the point corresponding to the start of the nearest pattern (or respective thing in ocaml) *)
let find_nearest_pattern ?current_buffer ~state_var point line =
  let current_buffer = match current_buffer with Some v -> v | None -> Current_buffer.get () in
  retrieve_gopcaml_state ~current_buffer ~state_var ()
  |> Option.bind ~f:(fun state -> build_zipper state (Position.sub point 1))
  |> Option.map ~f:( Ast_zipper.move_zipper_to_point (Position.to_int point) line false )
  |> Option.bind ~f:(fun zipper -> Ast_zipper.find_nearest_pattern
                        (Position.to_int point - 1)
                        zipper)
  |> Option.map ~f:(fun x -> x + 1)


(** returns whether the point is inside a let def (and thus when
    expanding let we should include an in) *)
let inside_defun ?current_buffer ~state_var point =
  let current_buffer = match current_buffer with Some v -> v | None -> Current_buffer.get () in
  retrieve_gopcaml_state_immediate ~current_buffer ~state_var ()
  |> Option.bind ~f:(fun state -> inside_let_def state point)


(** retrieve zipper *)
let retrieve_zipper ?current_buffer ~zipper_var =
  let current_buffer = match current_buffer with Some v -> v | None -> Current_buffer.get () in
  Buffer_local.get zipper_var current_buffer

(** delete zipper *)
let delete_zipper ?current_buffer ~zipper_var () =
  let current_buffer = match current_buffer with Some v -> v | None -> Current_buffer.get () in
  Buffer_local.set zipper_var None current_buffer

(** delete state *)
let delete_state ?current_buffer ~state_var () =
  let current_buffer = match current_buffer with Some v -> v | None -> Current_buffer.get () in
  Buffer_local.set state_var None current_buffer



let abstract_zipper_to_bounds zipper = zipper
                                       |> Option.map ~f:Ast_zipper.to_bounds
                                       |> Option.map ~f:(fun (st,ed) ->
                                           Position.of_int_exn (st + 1), Position.of_int_exn (ed + 1)
                                         )

(** retrieve bounds for current zipper *)
let retrieve_zipper_bounds ?current_buffer ~zipper_var () =
  retrieve_zipper ?current_buffer ~zipper_var
  |>  abstract_zipper_to_bounds

(** checks whether the current zipper item is a top-level item *)
let check_zipper_toplevel ?current_buffer ~zipper_var () =
  retrieve_zipper ?current_buffer ~zipper_var
  |> Option.map ~f:(Ast_zipper.zipper_is_top_level)

let check_zipper_toplevel_parent ?current_buffer ~zipper_var () =
  retrieve_zipper ?current_buffer ~zipper_var
  |> Option.map ~f:(Ast_zipper.zipper_is_top_level_parent)

(** attempts to move the current zipper left *)
let move_zipper_left ?current_buffer ~zipper_var () =
  let current_buffer = match current_buffer with Some v -> v | None -> Current_buffer.get () in
  retrieve_zipper ~current_buffer ~zipper_var
  |> print_zipper
  |> Option.bind ~f:Ast_zipper.go_left
  |> Option.map ~f:(fun zipper ->
      Buffer_local.set zipper_var (Some zipper) current_buffer;
      zipper
    )
  |> print_zipper
  |>  abstract_zipper_to_bounds

(** attempts to move the current zipper left *)
let ensure_zipper_space ?current_buffer ~zipper_var (pre_column,pre_line) (post_column,post_line) () =
  let current_buffer = match current_buffer with Some v -> v | None -> Current_buffer.get () in
  retrieve_zipper ~current_buffer ~zipper_var
  |> Option.bind ~f:(fun zipper -> Ast_zipper.update_zipper_space_bounds zipper
                        (pre_column,pre_line) (post_column,post_line))
  |> Option.map ~f:(fun zipper ->
      Buffer_local.set zipper_var (Some zipper) current_buffer;
      zipper
    )
  |> print_zipper
  |>  abstract_zipper_to_bounds

(** attempts to move the current zipper right *)
let move_zipper_right ?current_buffer ~zipper_var () =
  let current_buffer = match current_buffer with Some v -> v | None -> Current_buffer.get () in
  retrieve_zipper ~current_buffer ~zipper_var
  |> Option.bind ~f:Ast_zipper.go_right
  |> Option.map ~f:(fun zipper ->
      Buffer_local.set zipper_var (Some zipper) current_buffer;
      zipper
    )
  |> print_zipper
  |>  abstract_zipper_to_bounds

(** attempts to move the current zipper down *)
let move_zipper_down ?current_buffer ~zipper_var () =
  let current_buffer = match current_buffer with Some v -> v | None -> Current_buffer.get () in
  retrieve_zipper ~current_buffer ~zipper_var
  |> Option.bind ~f:Ast_zipper.go_down
  |> Option.map ~f:(fun zipper ->
      Buffer_local.set zipper_var (Some zipper) current_buffer;
      zipper
    )
  |> print_zipper
  |>  abstract_zipper_to_bounds

(** attempts to move the current zipper up *)
let move_zipper_up ?current_buffer ~zipper_var () =
  let current_buffer = match current_buffer with Some v -> v | None -> Current_buffer.get () in
  retrieve_zipper ~current_buffer ~zipper_var
  |> Option.bind ~f:Ast_zipper.go_up
  |> Option.map ~f:(fun zipper ->
      Buffer_local.set zipper_var (Some zipper) current_buffer;
      zipper
    )
  |> print_zipper
  |>  abstract_zipper_to_bounds  

(** attempts "update" the buffer using the zipper, returning the two regions to be swapped *)
let abstract_zipper_update f ?current_buffer ~zipper_var () =
  let current_buffer = match current_buffer with Some v -> v | None -> Current_buffer.get () in
  retrieve_zipper ~current_buffer ~zipper_var
  |> Option.bind ~f
  |> Option.map ~f:(fun (r1,r2,zipper) ->
      let r1 = Ast_zipper.TextRegion.to_bounds r1 in
      let r2 = Ast_zipper.TextRegion.to_bounds r2 in
      (r1,r2,zipper)
    )
  |> Option.map ~f:(fun ((l1,l2),(r1,r2),zipper) ->
      Buffer_local.set zipper_var (Some zipper) current_buffer;
      (Position.of_int_exn (l1 + 1),Position.of_int_exn (l2 + 1)),
      (Position.of_int_exn (r1 + 1),Position.of_int_exn (r2 + 1))
    )

(** attempts to swap the zipper *)
let zipper_swap = abstract_zipper_update Ast_zipper.calculate_swap_bounds

(** attempts to move the current expression forwards *)
let zipper_swap_forwards = abstract_zipper_update Ast_zipper.calculate_swap_forward_bounds

(** attempts to move the current expression backwards *)
let zipper_swap_backwards = abstract_zipper_update Ast_zipper.calculate_swap_backwards_bounds  

(** deletes the current item using the zipper, returning the region to be swapped *)
let zipper_delete_current ?current_buffer ~zipper_var () =
  let current_buffer = match current_buffer with Some v -> v | None -> Current_buffer.get () in
  retrieve_zipper ~current_buffer ~zipper_var
  |> Option.bind ~f:Ast_zipper.calculate_zipper_delete_bounds
  |> Option.map ~f:(fun (zipper, r1) ->
      let r1 = Ast_zipper.TextRegion.to_bounds r1 in
      (zipper,r1)
    )
  |> Option.map ~f:(fun (zipper,(l1,l2)) ->
      Buffer_local.set zipper_var (Some zipper) current_buffer;
      (Position.of_int_exn (l1 + 1),Position.of_int_exn (l2 + 1))
    )

let zipper_move_up ?current_buffer ~zipper_var ()  =
  let current_buffer = match current_buffer with Some v -> v | None -> Current_buffer.get () in
  retrieve_zipper ~current_buffer ~zipper_var
  |> Option.bind ~f:(Ast_zipper.move_up)
  |> Option.map ~f:(fun (zipper,pos,(ds,de)) ->
      Buffer_local.set zipper_var (Some zipper) current_buffer;
      (Position.of_int_exn (pos + 1),
       (Position.of_int_exn (ds + 1),
        Position.of_int_exn (de + 1)
       ))
    )

let zipper_move_down ?current_buffer ~zipper_var ()  =
  let current_buffer = match current_buffer with Some v -> v | None -> Current_buffer.get () in
  retrieve_zipper ~current_buffer ~zipper_var
  |> Option.bind ~f:(Ast_zipper.move_down)
  |> Option.map ~f:(fun (zipper,pos,(ds,de)) ->
      Buffer_local.set zipper_var (Some zipper) current_buffer;
      (Position.of_int_exn (pos + 1),
       (Position.of_int_exn (ds + 1),
        Position.of_int_exn (de + 1)
       ))
    )

(** finds all variables in the given expression *)
let find_variables_region text =
  try
    let exp =
      let lexbuf = Lexing.from_string ~with_positions:true text in
      Parse.expression lexbuf
    in 
    Ast_analysis.find_variables_exp exp
  with
    Parser.Error -> message (Printf.sprintf "parsing got error parse.error"); []
  | Syntaxerr.Error _ -> []


(** find the closest scope containing all variables used by the current expression  *)
let find_extract_start_scope ?current_buffer ~state_var st_p ed_p text () =
  let variables = find_variables_region text in 
  retrieve_gopcaml_state_immediate ?current_buffer ~state_var ()
  |> Option.bind ~f:(fun v -> find_enclosing_structure v st_p)
  |> Option.bind ~f:(fun (State.MkParseItem it: State.parse_item) ->
      match it with
      | State.ImplIt (_,si) ->
        let scopes = snd (Ast_analysis.find_scopes_si si) in
        let st_p,ed_p = Position.to_int st_p - 1, Position.to_int ed_p - 1 in 
        let selected_scope =
          Ast_analysis.find_lub_scope variables scopes (st_p,ed_p)
        in
        let selected_scope =
          match selected_scope with
          | None ->
            Ast_analysis.find_smallest_enclosing_scope scopes (st_p, ed_p)
          | v -> v
        in
        selected_scope
      | State.IntfIt (_, _) -> None
    )
  |> Option.map ~f:(fun (s,e) -> (Position.of_int_exn (s + 1), Position.of_int_exn (e + 1)))

(** find patterns in enclosing item  *)
let find_patterns_in_current_internal ?current_buffer ~state_var point =
  retrieve_gopcaml_state_immediate ?current_buffer ~state_var ()
  |> Option.bind ~f:(fun v -> find_enclosing_structure v point)
  |> Option.map ~f:(fun (State.MkParseItem it: State.parse_item) ->
      match it with
      | State.ImplIt (_,si) ->
        let scopes = (Ast_analysis.find_pattern_scopes_si si) in
        scopes
      | State.IntfIt (_, _) -> []
    )

(** find patterns in enclosing item  *)
let find_excluded_scopes_in_current_internal ?current_buffer ~state_var point (startp,endp) =
  retrieve_gopcaml_state_immediate ?current_buffer ~state_var ()
  |> Option.bind ~f:(fun v -> find_enclosing_structure v point)
  |> Option.map ~f:(fun (State.MkParseItem it: State.parse_item) ->
      match it with
      | State.ImplIt (_,si) ->
        let scopes = snd (Ast_analysis.find_scopes_si si) in
        let pat_scopes = (Ast_analysis.find_pattern_scopes_si si) in
        Ast_analysis.find_excluded_scopes scopes (startp,endp) @ pat_scopes
      | State.IntfIt (_, _) -> []
    )

(** find patterns in enclosing item  *)
let find_patterns_in_current ?current_buffer ~state_var point () =
  find_patterns_in_current_internal ?current_buffer ~state_var point
  |> Option.map ~f:(List.map ~f:(fun (st_p,ed_p) ->
            let st_p,ed_p = Position.of_int_exn (st_p + 1), Position.of_int_exn (ed_p + 1) in 
            st_p,ed_p
          ))
  |> Option.value ~default:[]


(** given a list of matching regions for the current scope, returns those that correspond
    to valid matches  *)
let find_extraction_matches ?current_buffer ~state_var point matches (start_p,end_p) () =
  let (start_p,end_p) = Position.to_int start_p - 1, Position.to_int end_p - 1 in 
  let matches = List.map ~f:(fun (a,b) -> Position.to_int a - 1, Position.to_int b - 1) matches in 
  find_excluded_scopes_in_current_internal ?current_buffer ~state_var point (start_p,end_p)
  |> Option.map ~f:(Ast_analysis.find_valid_matches matches)
  |> Option.map ~f:(List.map ~f:(fun (st_p,ed_p) ->
            let st_p,ed_p = Position.of_int_exn (st_p + 1), Position.of_int_exn (ed_p + 1) in 
            st_p,ed_p
    ))
  |> Option.value ~default:[]


