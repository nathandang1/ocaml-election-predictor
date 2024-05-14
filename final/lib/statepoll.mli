(** module that will represent the individual polling data of a state *)

type state
(** [state] is an abstract type that will represent a state. *)

exception ImproperList of string
(** [ImproperList] will be thrown if the string list inserted into
    [create_state] is improperly formatted. refer to specification for more
    detail.*)

val create_state : string list -> state
(** [create_state lst] creates a state based off of information provided by
    string list [lst] [lst] should be formatted so that there are only five
    elements. The first element is the name of the state, the second, the
    preferred candidate, the third the preferred margin, the fourth the number
    of votes, and the fifth the population. Any lists that are formatted
    otherwise will be rejected, whether it be the type of the data (when
    ()_of_string is called) or the length of the list*)

(** Getter Methods *)

val get_name : state -> string
(** [get_name state] returns the name of state [state]*)

val get_preferred_candidate_name : state -> string
(** [get_preferred_candidate_name state] returns the name of the preferred candidate of state
    [state]*)

val get_preferred_candidate_party : state -> string
(** [get_preferred_candidate_name state] returns the party of the preferred candidate of state
    [state]*)

val get_preferred_margin : state -> float
(** [get_preferred_margin state] returns the margin that the preferred candidate
    of state [state] is ahead by*)

val get_num_votes : state -> int
(** [get_num_votes state] returns the number of votes that state [state] has in
    the general election/electoral college*)

val get_population : state -> int
(** [get_population state] returns the population of state [state]*)

(** Setter Methods *)

val set_preferred_candidate : state -> Candidate.t -> unit  
(** [set_preferred_candidate state candidate] sets the preferred candidate of [state] to [candidate]*)

val set_preferred_margin : state -> float -> unit
(** [set_preferred_margin state marg] sets the margin of preference for state [state]'s to [marg]*)

val set_num_votes : state -> int -> unit
(** [set_num_votes state votes] changes the number of votes for state [state] to [votes]*)

val set_population : state -> int -> unit 
(** [set_population state pop] changes the population of state [state] to [pop]*)

val export_state_to_csv : state -> Csv.t
(** [export_state_to_csv state] exports the data of state [state] to a local csv*)

val save_data_locally : state -> unit 
(** [save_data_locally state] saves the data csv of [state] to the local computer*)
