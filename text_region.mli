
module Diff : sig
  type t
  val of_pair : line:int -> col:int -> t
  val combine : t -> t -> t
  val to_string : t -> string
  val add_newline_with_indent: indent:int -> t -> t
  val negate : t -> t 
  val update_lexing_position : Lexing.position -> t -> Lexing.position 
end

type t

val of_location: Location.t -> t

val to_bounds : t -> (int * int)

val to_string : t -> string

val pp : t -> string

val shift_region : t -> Diff.t -> t

val extend_region : t -> Diff.t -> t

val union : t -> t -> t

val before_point : t -> int -> bool

val contains_point : t -> int -> bool

val contains_ne_point : t -> int -> bool

val equals_point : ?forward:bool -> t -> int -> bool

val ast_bounds_iterator : unit -> Ast_iterator.iterator * (unit -> t)

val ast_bounds_mapper : diff:Diff.t -> Ast_mapper.mapper

val distance : ?forward:bool -> t -> int -> int option

val distance_line : ?forward:bool -> t -> point:int -> line:int -> (int option * int option)

val line_start : t -> int

val column_start : t -> int

val column_end : t -> int

val to_diff : t -> Diff.t option

val swap_diff : t -> t -> (Diff.t * Diff.t) option

val diff_between : t -> t -> Diff.t option

val to_shift_from_start: t -> Diff.t

