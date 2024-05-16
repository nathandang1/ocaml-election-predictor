type t = {
  name : string;
  party : string;
}
(** [t] represents a candidate in an election. *)

val create : string * string -> t
(**[create nme prty] creates a candidate of type [t] who's name = [nme] and party [prty]  *)

val name : t -> string
(**returns the [name] of a candidate *)
val party : t -> string
(**returns the [party] of a candidate *)

