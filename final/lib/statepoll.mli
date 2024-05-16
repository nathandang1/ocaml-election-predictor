(** module that will represent the individual polling data of a state *)

type state = {
  name : string;
  abbreviation : string;
  mutable preferred_candidate : string;
  mutable preferred_margin : float;
  mutable num_votes : int;
  mutable population : int;
}
(** [state] is an abstract type that will represent a state. *)

exception ImproperList of string
(** [ImproperList] will be thrown if the string list inserted into
    [create_state] is improperly formatted. refer to specification for more
    detail.*)

val create_state : string list -> state
(** [create_state lst] creates a state based off of information provided by
    string list [lst] [lst] should be formatted so that there are only six
    elements. name,abbr,votes,pop,pref_can,pref_percent The first element is the
    name of the state, the second the state's abbreviation, the third the number
    of votes the state has, the fourth the population of the state, the fifth
    the preferred candidate in the state, and the sixth the margin of preference
    said candidate has over the opposition. Failure to format in this way will
    yield an ImproperList error. Additionally, the number of votes and the
    population must be positive integers greater than zero. The margin of
    preference should be greater than 0.0 and less than 100.0. Failure to abide
    by this specification will yield an ImproperList error.*)

(** Getter Methods *)

val get_name : state -> string
(** [get_name state] returns the name of state [state]*)

val get_preferred_candidate_name : state -> string
(** [get_preferred_candidate_name state] returns the name of the preferred
    candidate of state [state]*)

val get_abbreviation : state -> string
(** [get_abbreviation state] returns the abbreviation state [state]*)

val get_preferred_margin : state -> float
(** [get_preferred_margin state] returns the margin that the preferred candidate
    of state [state] is ahead by*)

val get_num_votes : state -> int
(** [get_num_votes state] returns the number of votes that state [state] has in
    the general election/electoral college*)

val get_population : state -> int
(** [get_population state] returns the population of state [state]*)

(** Setter Methods *)

val set_preferred_candidate : state -> string -> unit
(** [set_preferred_candidate state candidate] sets the preferred candidate of
    [state] to [candidate]*)

val set_preferred_margin : state -> float -> unit
(** [set_preferred_margin state marg] sets the margin of preference for state
    [state]'s to [marg]*)

val set_num_votes : state -> int -> unit
(** [set_num_votes state votes] changes the number of votes for state [state] to
    [votes]*)

val set_population : state -> int -> unit
(** [set_population state pop] changes the population of state [state] to [pop]*)

(** Other *)

val export_state_to_csv : state -> Csv.t
(** [export_state_to_csv state] exports the data of state [state] to a local csv*)

val equals : state -> state -> bool
(** [equals state1 state2] returns whether state1 and state2 are equal based off
    the name of the state (case sensitive)*)
