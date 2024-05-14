type state =  {
  name : string; 
  mutable preferred_candidate : Candidate.t; 
  mutable preferred_margin : float; 
  mutable num_votes : int; 
  mutable population : int; 
  }

exception ImproperList of string 
let create_state lst = try match lst with 
| nam :: can_name :: can_party :: marg :: votes :: pop :: [] ->
  {
    name = nam; 
    preferred_candidate = {name = can_name; party = can_party}; 
    preferred_margin = float_of_string marg; 
    num_votes = int_of_string votes; 
    population = int_of_string pop 
  }
  (** TODO: Make this message better*)
| _ -> raise (ImproperList "Please reference specification")
with _ -> raise (ImproperList "Please ensure that your list is a string list
that is structured in [string; string; string_of_float; 
string_of_int; string_of_int] form ")

let get_name st = st.name 
let get_preferred_candidate_name st = st.preferred_candidate.name
let get_preferred_candidate_party st = st.preferred_candidate.party 
let get_preferred_margin st = st.preferred_margin 
let get_num_votes st = st.num_votes
let get_population st = st.population

let change_preferred_candidate st can = st.preferred_candidate <- can 
let change_preferred_margin st marg = st.preferred_margin <- marg 
let change_num_votes st vot = st.num_votes <- vot
let change_population st pop = st.population <- pop 

let export_state_to_csv st = 
  let attributes = ["Name of State"; "Preferred Candidate Name"; 
  "Preferred Candidate Party"; 
  "Preferred Candidate Margin of Preference"; 
  "Number of Votes"; "Population Size"] 
in 
  let state_data = [get_name st; 
  get_preferred_candidate_name st; 
  get_preferred_candidate_party st; 
  (string_of_float (get_preferred_margin st)); 
  (string_of_int (get_num_votes st)); 
  (string_of_int (get_population st))]
in 
  let data = [attributes; state_data] 
in 
  Csv.transpose data 