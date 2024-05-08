type t = {
  name : string;  (** [name] is the name of the state *)
  abbr : string;  (** [abbr] is an abbreviation of the state's name. *)
  votes : int;  (** [votes] is the number of electoral votes the state has *)
  pop : int;  (** [pop] is the population of the state *)
}
(** [t] contains the basic metadata of a state *)
