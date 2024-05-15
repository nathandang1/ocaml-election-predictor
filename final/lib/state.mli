type t = {
  name : string;  (** [name] is the name of the state *)
  abbr : string;  (** [abbr] is an abbreviation of the state's name. *)
  votes : int;  (** [votes] is the number of electoral votes the state has *)
  pop : int;  (** [pop] is the population of the state *)
  pref_can : string;
  pref_percent : float;
}
(** [t] contains the basic metadata of a state *)

type d = (string * float) list

val outcome : t -> d -> string -> float -> d
