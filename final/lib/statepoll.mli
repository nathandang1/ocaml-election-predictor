(** module that will represent the individual polling data of a state *)

type state
(** [state] is an abstract type that will represent a state. *)

val create_state : string list -> state 
(** [create_state lst] creates a state based off of information provided by string list [lst]*)

val get_name : state -> string
(** [get_name state] returns the name of state [state]*)

val get_preferred_candidate : state -> string
(** [get_preferred_candidate state] returns the preferred candidate of state [state]*)

val get_preferred_margin : state -> float
(** [get_preferred_margin state] returns the margin that the preferred 
    candidate of state [state] is ahead by*)

val get_num_votes : state -> int
(** [get_num_votes state] returns the number of votes that state [state] has in 
    the general election/electoral college*)

val get_population : state -> int 
(** [get_population state] returns the population of state [state]*)

val export_state_to_csv : state -> Csv.t
(** [export_state_to_csv state] exports the data of state [state] to a local csv*)

(** TODO: Add signatures for the setter functions *)