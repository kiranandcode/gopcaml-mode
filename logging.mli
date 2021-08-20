open Ecaml
open Core

module Level : sig
  (** Type encoding the logging levels supported by the plugin *)
  type t = [ `debug |  `verbose | `info  | `none  ]

  val ty : t Ecaml.Value.Type.t

  val custom_ty: Ecaml.Customization.Type.t

end

(** Setup the variable used to determine logging level  *)
val setup_logging: Level.t Ecaml.Customization.t -> unit

(** [message ~log_level ?at msg] logs a message to the user if they have requested
   receiving messages at that log level.

    The logging level to log [at] defaults to `info.  *)
val message : ?at:Level.t -> string -> unit
