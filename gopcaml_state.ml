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
              message (Printf.sprintf "getting position %d" column);
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
    (** tracks dirty extent of the current buffer as a range
        (in relation to the current buffer)

        we don't track it in relation to the ast due to the
        markers delimiting each structure item
        use automatically updating to buffer changes
    *)
    type t =
      | Clean of parse_tree
      | Dirty

    let get_dirty_region = function
      | Clean _ -> None
      | Dirty -> Some (0,-1)

    let is_dirty = function
      | Clean _ -> false
      | _ -> true

    (** creates a clean dirty region from a parse tree *)
    let create tree = Clean tree

    (** updates the parse tree to denote the range of the dirty region *)
    let update (_:t) (_s,_e,_l: (int * int * int)) : t =
      (* todo: track detailed changes *)
      Dirty


    (** builds an updated parse_tree (updating any dirty regions) *)
    let to_tree (dr:t) (_file_type: Filetype.t) : parse_tree option =
      match dr with
      | Clean tree -> Some tree
      | Dirty ->
        TreeBuilder.parse_current_buffer _file_type

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
    parse_tree = DirtyRegion.Dirty;
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
      parse_tree = match parse_tree with None -> DirtyRegion.Dirty | Some tree -> DirtyRegion.create tree;
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
  let state = State.{state with parse_tree=Dirty; file_type = file_type } in
  Buffer_local.set state_var (Some state) current_buffer
[@@warning "-23"]

let build_zipper (state: State.Validated.t) point =
  let open State in
  let open Validated in
  let find_enclosing_expression list = 
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
  in
  match state.parse_tree with 
  | (State.MkParseTree (State.Impl si_list)) ->
    find_enclosing_expression si_list
    |> Option.map ~f:(fun (left,current,right) ->
        Ast_zipper.make_zipper_impl left current right )
  | (State.MkParseTree (State.Intf si_list)) ->
    find_enclosing_expression si_list
    |> Option.map ~f:(fun (left,current,right) ->
        Ast_zipper.make_zipper_intf left current right)


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
    (* message (Printf.sprintf "enclosing structure: %s"(ExtLib.dump expr)); *)
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
  (* message (Printf.sprintf "updating dirty region with s:%d-%d l:%d" s e l); *)
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


(** retrieve the points enclosing structure at the current position *)
let retrieve_enclosing_structure_bounds ?current_buffer ~state_var point =
  retrieve_gopcaml_state ?current_buffer ~state_var ()
  |> Option.bind ~f:(find_enclosing_structure_bounds ~point)

(** retrieve the points enclosing expression at the current position *)
let retrieve_enclosing_bounds ?current_buffer ~state_var point =
  retrieve_gopcaml_state ?current_buffer ~state_var ()
  |> Option.bind ~f:(find_enclosing_bounds ~point)


(** retrieve a zipper enclosing structure at the current position *)
let build_zipper_enclosing_point ?current_buffer ~state_var ~zipper_var point =
  let current_buffer = match current_buffer with Some v -> v | None -> Current_buffer.get () in
  retrieve_gopcaml_state ~current_buffer ~state_var ()
    |> Option.bind ~f:(fun state ->
        let zipper = build_zipper state point in
        Buffer_local.set zipper_var zipper current_buffer;
        zipper)
  |> Option.map ~f:Ast_zipper.to_bounds
  |> Option.map ~f:(fun (st,ed) ->
      Position.of_int_exn (st + 1), Position.of_int_exn (ed + 1)
    )


(** retrieve zipper *)
let retrieve_zipper ?current_buffer ~zipper_var =
  let current_buffer = match current_buffer with Some v -> v | None -> Current_buffer.get () in
  Buffer_local.get zipper_var current_buffer

(** delete zipper *)
let delete_zipper ?current_buffer ~zipper_var () =
  let current_buffer = match current_buffer with Some v -> v | None -> Current_buffer.get () in
  Buffer_local.set zipper_var None current_buffer

let abstract_zipper_to_bounds zipper = zipper
  |> Option.map ~f:Ast_zipper.to_bounds
  |> Option.map ~f:(fun (st,ed) ->
      Position.of_int_exn (st + 1), Position.of_int_exn (ed + 1)
    )

(** retrieve bounds for current zipper *)
let retrieve_zipper_bounds ?current_buffer ~zipper_var () =
  retrieve_zipper ?current_buffer ~zipper_var
  |>  abstract_zipper_to_bounds

(** attempts to move the current zipper left *)
let move_zipper_left ?current_buffer ~zipper_var () =
  let current_buffer = match current_buffer with Some v -> v | None -> Current_buffer.get () in
  retrieve_zipper ~current_buffer ~zipper_var
  |> Option.bind ~f:Ast_zipper.go_left
  |> Option.map ~f:(fun zipper ->
      Buffer_local.set zipper_var (Some zipper) current_buffer;
      zipper
    )
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
  |>  abstract_zipper_to_bounds  

(** attempts to swap the zipper *)
let zipper_swap ?current_buffer ~zipper_var () =
  let current_buffer = match current_buffer with Some v -> v | None -> Current_buffer.get () in
  retrieve_zipper ~current_buffer ~zipper_var
  |> Option.bind ~f:Ast_zipper.calculate_swap_bounds
  |> Option.map ~f:(fun ((l1,l2),(r1,r2),zipper) ->
      Buffer_local.set zipper_var (Some zipper) current_buffer;
      (Position.of_int_exn (l1 + 1),Position.of_int_exn (l2 + 1)),
      (Position.of_int_exn (r1 + 1),Position.of_int_exn (r2 + 1))
    )

  
