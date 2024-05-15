type t = {
  name : string;
  party : string;
}
(** [t] represents a candidate in an election. *)

val create : string * string -> t

val name : t -> string

val party : t -> string
