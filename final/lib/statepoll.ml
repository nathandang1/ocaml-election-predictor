type state =  {
  name : string; 
  mutable preferred_candidate : string; 
  mutable preferred_margin : float; 
  mutable num_votes : int; 
  mutable population : int; 
  }

exception ImproperList of string 
let create_state lst = try match lst with 
| nam :: can :: marg :: votes :: pop :: [] ->
  {
    name = nam; 
    preferred_candidate = can; 
    preferred_margin = float_of_string marg; 
    num_votes = int_of_string votes; 
    population = int_of_string pop 
  }
  (** TODO: Make this message better*)
| _ -> raise (ImproperList "Please reference specification")
with _ -> raise (ImproperList "Please ensure that your list is a string list
that is structured in [string; string; string_of_float; 
string_of_int; string_of_int] form ")

let get_name state = raise(Failure "Not implemented")
let get_preferred_candidate state = raise(Failure "Not implemented")
let get_preferred_margin state = raise(Failure "Not implemented")
let get_num_votes state = raise(Failure "Not implemented")
let get_population state = raise(Failure "Not implemented")
let export_state_to_csv state = raise(Failure "Not implemented")