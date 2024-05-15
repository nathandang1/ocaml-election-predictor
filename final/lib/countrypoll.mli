type country

type state = Statepoll.state

val create_country : (state list) -> bool -> string -> country 

val get_name : country -> string

val get_states : country -> state list 

val get_population : country -> int

val electoral_college_enabled : country -> bool

val export_data : country -> Csv.t 

val save_data_locally : country -> unit 

val remove_state : country -> string -> unit 

val add_state : country -> state -> unit

val contains_state : country -> state -> bool 

val equals : country -> country -> bool