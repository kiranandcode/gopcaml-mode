open Core
open Ecaml

module State = struct

  module Filetype = struct
    (** records whether the current file is an interface or implementation  *)
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
            let ((min_line,min_column), (max_line,max_column)) = get_result () in
            let start_marker,end_marker = Marker.create (), Marker.create () in
            let get_position line column = 
              message (Printf.sprintf "getting position %d %d" line column);
              Position.of_int_exn column
            in
            (* Point.goto_line_and_column Line_and_column.{line;column};
             * Point.get () in *)

            Marker.set start_marker current_buffer (get_position min_line min_column);
            Marker.set end_marker current_buffer (get_position max_line max_column);
            {start_mark=start_marker;
             end_mark=end_marker;
             logical_start = Line_and_column.{line=min_line;column=min_column}; 
             logical_end = Line_and_column.{line=min_line;column=min_column};
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
              message (Printf.sprintf "extension of %s is %s" file_name ext);
              message (Printf.sprintf "impls: %s, intfs: %s"
                         (String.concat ~sep:", " implementation_extensions)
                         (String.concat ~sep:", " interface_extensions)
                      );

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
      message (Printf.sprintf "Parsing called on \"%s\"" buffer_text);
      let perform_parse () = 
        let _ = let open Filetype in
          match file_type with
          | Interface -> message "filetype is interface."
          | Implementation -> message "filetype is implementation." in
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
        | Either.Second e -> 
          message ("Could not build parse tree: " ^ ExtLib.dump e);
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
      with Parser.Error -> 
        message (Printf.sprintf "parsing got error parse.error");
        None
         | Syntaxerr.Error err ->
           message (Printf.sprintf "parsing got error %s" (ExtLib.dump err));
           None
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
          let ((_,_),(_,c)) = get_bounds () in
          Position.of_int_exn c
        | None ->
          match invalid_region with
          | (_,st) :: _ ->
            let (iterator,get_bounds) =  Ast_transformer.bounds_iterator () in
            f iterator st;
            let ((_,_),(_,c)) = get_bounds () in
            Position.of_int_exn c
          | [] -> mi
      in
      let end_region =
        match post_edit_region with
        | (_, st) :: _ ->
          let (iterator,get_bounds) =  Ast_transformer.bounds_iterator () in
          f iterator st;
          let ((_,_),(_,c)) = get_bounds () in
          Position.of_int_exn c
        | [] ->
          match List.last invalid_region with
          | Some (_,st) ->
            let (iterator,get_bounds) =  Ast_transformer.bounds_iterator () in
            f iterator st;
            let ((_,_),(_,c)) = get_bounds () in
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
    (** tracks dirty extent of the current buffer as a range
        (in relation to the current buffer)

        we don't track it in relation to the ast due to the
        markers delimiting each structure item
        use automatically updating to buffer changes
    *)
    type t =
      | Clean of parse_tree
      | Dirty of { tree: parse_tree; min: int; max: int; }

    let get_dirty_region = function
      | Clean _ -> None
      | Dirty {min;max;_} -> Some (min,max)

    let is_dirty = function
      | Clean _ -> false
      | _ -> true

    (** creates a clean dirty region from a parse tree *)
    let create tree = Clean tree

    (** updates the parse tree to denote the range of the dirty region *)
    let update (dr:t) (s,e,l: (int * int * int)) : t =
      let find_bounding_regions (tree:parse_tree) min max =
        let (MkParseTree items) = tree in
        match items with
        | Intf items ->
          let pre,inv,post = TreeBuilder.calculate_region min max items () in
          let (min,max) = TreeBuilder.calculate_start_end
              (fun iterator st -> iterator.signature_item iterator st)
              min max pre inv post in
          (Position.to_int min, Position.to_int max)
        | Impl items ->
          let pre,inv,post = TreeBuilder.calculate_region min max items () in
          let (min,max) = TreeBuilder.calculate_start_end
              (fun iterator st -> iterator.structure_item iterator st)
              min max pre inv post in
          (Position.to_int min, Position.to_int max)
      in
      match (dr : t) with
      | Clean tree ->
        let (min,max) = Position.of_int_exn s, Position.of_int_exn (e + l) in
        let (min,max) = find_bounding_regions tree min max in
        Dirty {tree; min;max}
      | Dirty {tree;min;max;} ->
        let min,max = Int.min min s, Int.max (max + l) (e + l) in
        let (min,max) = Position.of_int_exn min, Position.of_int_exn max in
        let (min,max) = find_bounding_regions tree min max in
        Dirty {tree; min;max}


    (** builds an updated parse_tree (updating any dirty regions) *)
    let to_tree (dr:t) (_file_type: Filetype.t) : parse_tree =
      match dr with
      | Clean tree -> tree
      | Dirty {tree;min;max;} ->
        let (MkParseTree items) = tree  in
        match items with
        | Impl items ->
          let items = TreeBuilder.rebuild_impl_parse_tree min max items () in
          MkParseTree (Impl items)
        | Intf items ->
          let items = TreeBuilder.rebuild_intf_parse_tree min max items () in
          MkParseTree (Intf items)
  end

  (** type of state of plugin - pre-validation *)
  type t = {
    (** file type of the current buffer *)
    file_type: Filetype.t;
    (** parse tree of the current buffer + any dirty regions *)
    parse_tree: DirtyRegion.t option;
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
      let+ parse_tree = match state.parse_tree with
        | (Some _ as t) -> t
        | None -> 
          message "Buffer AST not built - rebuilding...";
          should_store := true;
          TreeBuilder.parse_current_buffer state.file_type
          |> Option.map ~f:DirtyRegion.create
      in
      if DirtyRegion.is_dirty parse_tree then should_store := true;
      let parse_tree = DirtyRegion.to_tree parse_tree state.file_type in
      if !should_store then
        Some ({file_type=state.file_type; parse_tree},
              Some ({file_type=state.file_type; parse_tree = Some (DirtyRegion.create parse_tree)}:t))
      else
        Some ({file_type=state.file_type; parse_tree}, None)

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
    parse_tree = None;
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
      parse_tree = Option.map ~f:DirtyRegion.create parse_tree;
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
  let state = State.{state with file_type = file_type } in
  Buffer_local.set state_var (Some state) current_buffer

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

(** returns a tuple of points enclosing the current structure *)
let find_enclosing_structure_bounds (state: State.Validated.t) ~point =
  find_enclosing_structure state point
  |> Option.bind ~f:begin fun expr -> let (State.MkParseItem expr) = expr in
    let region = match expr with
      | ImplIt (r,_) -> r
      | IntfIt (r,_) -> r in
    match Marker.position region.start_mark,Marker.position region.end_mark with
    | Some s, Some e -> Some (Position.add s 0, Position.add e 0)
    | _ -> None
  end




(** updates the dirty region of the parse tree *)
let update_dirty_region ?current_buffer ~state_var (s,e,l) =
  message (Printf.sprintf "updating dirty region with s:%d-%d l:%d" s e l);
  let open State in
  let current_buffer = match current_buffer with Some v -> v | None -> Current_buffer.get () in
  let state = Buffer_local.get_exn state_var current_buffer in
  match state.parse_tree with
  | None ->
    message "no parse tree not updating dirty region";
    ()                  (* no parse tree, no point tracking dirty regions *)
  | Some dr ->
    message "dirty region present, removing";
    let parse_tree = DirtyRegion.update dr (s,e,l) in
    let state = {state with parse_tree = Some parse_tree} in
    Buffer_local.set state_var (Some state) current_buffer

(** retrieves the dirty region if it exists *)
let get_dirty_region ?current_buffer ~state_var ()  =
  let open State in
  let current_buffer = match current_buffer with Some v -> v | None -> Current_buffer.get () in
  let state = Buffer_local.get_exn state_var current_buffer in
  match state.parse_tree with
  | None -> None
  | Some dr -> DirtyRegion.get_dirty_region dr

(** retrieves the gopcaml state value, attempting to construct the
    parse tree if it has not already been made *)
let retrieve_gopcaml_state ?current_buffer ~state_var =
  let current_buffer = match current_buffer with Some v -> v | None -> Current_buffer.get () in
  let state = Buffer_local.get_exn state_var current_buffer in
  let (let+) x f = Option.bind ~f x in
  let+ (v_state,state) = State.Validated.of_state state in
  if Option.is_some state then Buffer_local.set state_var state current_buffer;
  Some v_state



(** retrieve the points enclosing structure at the current position *)
let retrieve_enclosing_structure_bounds ?current_buffer ~state_var point =
  retrieve_gopcaml_state ?current_buffer ~state_var
  |> Option.bind ~f:(find_enclosing_structure_bounds ~point)

