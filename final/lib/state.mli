(** An S is a state-level electoral subdivision of an electoral map. *)
module S : sig
  type c
  (** [c] is a tuple of candidates for the election. *)

  type t
  (** [t] contains the basic instances of a State *)
  (* = {name: string; votes: int; pop: int ; pref_can : c ; percent_win :
     float} *)

  type d
  (** [d] is the state-specific data. *)

  val outcome : t -> d -> d
  (** [outcome d] is each candidate's probability of winning given some data
      [d]. *)

  val name : t -> string
  (** [name_of state] is the name of the state [state]. *)

  val population : t -> int
  (** [population_of state] is the population of the state [state]. *)

  val from_csv : bool
  (** [from_csv] indicates whether a state can be extracted from a .csv file,
      and is [false].*)

  val of_data : string list list -> t
  (** [of_data data] is the state created using information extracted from the
      nested string list [data]. Requires that [data] contains a single list,
      whose elements include the name of the state, its votes, its population,
      its preferred candidate, and its preference percentage, in that order. *)
end
