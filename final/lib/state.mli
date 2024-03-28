(** An S is a state-level electoral subdivision of an electoral map. *)
module S : sig
  type c
  (** [c] is a tuple of candidates for the election. *)

  type t = {
    name : string;  (** [name] is the name of the state *)
    votes : int;  (** [votes] is the number of electoral votes the state has *)
    pop : int;  (** [pop] is the population of the state *)
    pref_can : string;
        (** [pref_can] is the name of the candidate that the state prefers
            currently *)
    pref_percent : float;
        (** [pref_percent] is the net advantage [pref_can] has over the
            competition *)
  }
  (** [t] contains the basic instances of a State *)
  (* = {name: string; votes: int; pop: int ; pref_can : c ; percent_win :
     float} *)

  type d = (string * float) list
  (** [d] is the state-specific data. *)

  val outcome : t -> d -> float -> d
  (** [outcome s d prior] is each candidate's probability of winning state [s]
      given some data [d] and user-specified prior belief [prior].*)

  val from_csv : bool
  (** [from_csv] indicates whether a state can be extracted from a .csv file,
      and is [false].*)

  val of_data : string list list -> t
  (** [of_data data] is the state created using information extracted from the
      nested string list [data]. Requires that [data] contains a single list,
      whose elements include the name of the state, its votes, its population,
      its preferred candidate, and its preference percentage, in that order. *)
end
