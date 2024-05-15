type country
(** [country] is an abstract type that will represent a country. *)

type state = Statepoll.state
(** [state] is an abstract type that will represent a state. *)

val create_country : (state list) -> bool -> string -> country 
(** [create_country a b c] creates a country with states [a], electoral college enabling [b] and name [c]*)

val get_name : country -> string
(** [get_name a] returns the name of country [a]*)

val get_states : country -> state list 
(** [get_states a] returns the states of country [a]*)

val get_population : country -> int
(** [get_population a] returns the population of country [a]*)

val get_electoral_votes : country -> int option
(** [get_electoral_votes a] returns an option type containing the number of electoral votes 
    in country [a], and returns None if electoral college is not enabled*)

val electoral_college_enabled : country -> bool
(** [electoral_college_enabled a] returns whether country [a] observes an electoral college election system*)

val export_data : country -> Csv.t 
(** [export_data a] produces a CSV for country [a]*)

val save_data_locally : country -> unit 
(** [save_data_locally a] saves the CSV associated with country [a] to the local machine*)

val remove_state : country -> state -> unit 
(** [remove_state a b] removes state [b] from country [a], given that [b] is a state in [a]*)

val add_state : country -> state -> unit
(** [add_state a b] adds state [b] to country [a], unless state [b] is already contained in [a]*)

val contains_state : country -> state -> bool 
(** [contains_state a b] returns whether state [b] is a state in country [a]*)

val equals : country -> country -> bool
(** [equals a b] returns whether country [a] and country [b] are equal based on states*)