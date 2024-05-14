type country

type state

val create_country : state list -> bool -> string -> country 

val get_name : country -> string

val get_states : country -> state list 

val electoral_college_enabled : country -> bool

val export_data : country -> Csv.t 

val save_data_locally : country -> unit 