open Core
open Ecaml

module Level = struct
  type t = [`debug | `verbose | `info | `none ] [@@deriving ord, enum, sexp, eq]

  module Enum : Ecaml.Value.Type.Enum with type t = t = struct
    type nonrec t = t
    let all = [`debug ; `verbose ; `info ; `none ]
    let sexp_of_t = sexp_of_t
  end

  let to_ecaml file_type =
    match file_type with
    | `debug -> Value.intern "debug"
    | `verbose -> Value.intern "verbose"
    | `info -> Value.intern "info"
    | `none -> Value.intern "none"

  let ty =
      Value.Type.enum
        (Sexp.Atom "message-level")
        (module Enum)
        to_ecaml

  let custom_ty = Ecaml.Customization.Type.enum Enum.all to_ecaml

end

let log_level_var = ref @@ fun () -> assert false

let setup_logging var = log_level_var := fun () -> var

let log_level () = (Customization.value @@ !log_level_var ())

let message ?at:(level=`info) msg =
  (* TODO: This is somewhat inefficient, as it means that each message
     call requires interaction between OCaml and Emacs - is there a
     way to do this smarter? *)
  let log_level = log_level () in
  if Level.equal log_level `none || (Level.compare level log_level < 0)
  then ()
  else Ecaml.message msg

