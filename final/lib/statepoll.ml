type state =  {
  name : string; 
  mutable preferred_candidate : Candidate.t; 
  mutable preferred_margin : float; 
  mutable num_votes : int; 
  mutable population : int; 
  }

exception ImproperList of string 
let create_state lst = try 
match lst with 
| nam :: can_name :: can_party :: marg :: votes :: pop :: [] ->
  {
    name = nam; 
    preferred_candidate = {name = can_name; party = can_party}; 
    preferred_margin = float_of_string marg; 
    num_votes = int_of_string votes; 
    population = int_of_string pop 
  }
| _ -> raise (ImproperList "")
with _ -> raise (ImproperList "")

let equals (state1 : state) state2 = state1 = state2 
let get_name st = st.name 
let get_preferred_candidate_name st = st.preferred_candidate.name
let get_preferred_candidate_party st = st.preferred_candidate.party 
let get_preferred_margin st = st.preferred_margin 
let get_num_votes st = st.num_votes
let get_population st = st.population

let set_preferred_candidate st can = st.preferred_candidate <- can 
let set_preferred_margin st marg = st.preferred_margin <- marg 
let set_num_votes st vot = st.num_votes <- vot
let set_population st pop = st.population <- pop 

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

let save_data_locally st = 
  let csv = export_state_to_csv st in 
  let file_name = st.name ^ "_data.csv" in 
  let () = print_endline ("Data saved at: " ^ file_name) in 
  Csv.save file_name csv 