type state = Statepoll.state
(** [state] is an abstract type that will represent a state. *)

type country = {
    electoral_college : bool; 
    mutable states : state list; 
    name : string ; 
}
(** [country] is an abstract type that will represent a country. *)

exception ImproperCSV of string
(** [ImproperCSV] will be thrown if the csv inserted into
    [create_country_from_CSV] is improperly formatted. refer to specification for more
    detail.*)

val create_country_from_CSV : Csv.t -> string -> bool -> country
(** [create_country_from_CSV csv name electoral] creates a new country from csv [csv] with name
    [name] and electoral college toggle [electoral]
    The CSV must have six columns and be formatted in how a state is formatted, otherwise ImproperCSV is thrown
    Additionally, an empty CSV file with only headers will also yield an ImproperCSV error*)

val create_country : (state list) -> bool -> string -> country 
(** [create_country a b c] creates a country with states [a], electoral college enabling [b] and name [c]*)

val get_name : country -> string
(** [get_name a] returns the name of country [a]*)

val get_states : country -> state list 
(** [get_states a] returns the states of country [a]*)

val get_state_names : country -> string list
(** [get_state_names a] returns a list of all states in country [a]*)

val get_population : country -> int
(** [get_population a] returns the population of country [a]*)

val get_electoral_votes : country -> int option
(** [get_electoral_votes a] returns an option type containing the number of electoral votes 
    in country [a], and returns None if electoral college is not enabled*)

val electoral_college_enabled : country -> bool
(** [electoral_college_enabled a] returns whether country [a] observes an electoral college election system*)

val export_data : country -> Csv.t 
(** [export_data a] produces a CSV for country [a]*)

val save_data_locally : country -> string -> unit 
(** [save_data_locally a b] saves the CSV associated with country [a] to the local machine
    at filename string [b]*)

val remove_state : country -> state -> unit 
(** [remove_state a b] removes state [b] from country [a], given that [b] is a state in [a]*)

val add_state : country -> state -> unit
(** [add_state a b] adds state [b] to country [a], unless state [b] is already contained in [a]*)

val contains_state : country -> state -> bool 
(** [contains_state a b] returns whether state [b] is a state in country [a]*)

val equals : country -> country -> bool
(** [equals a b] returns whether country [a] and country [b] are equal based on states*)