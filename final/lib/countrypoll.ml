type state = Statepoll.state 

type country = {
  electoral_college : bool; 
  mutable states : state list; 
  name : string ; 
}
(** helpers *)
let sort_helper_comparator (a : state) (b : state) = String.compare a.name b.name
let sort_states states = List.sort (sort_helper_comparator) states
let rec equals_helper_2 states1 states2 = match states1 with 
| [] -> true
| h :: t -> 
  let state2_of_interest = List.hd states2 in 
  if (Statepoll.equals state2_of_interest h) then equals_helper_2 t (List.tl states2)
  else 
    false 

let equals_helper states1 states2 = if (List.length states1) = (List.length states2) 
  then 
  equals_helper_2 (sort_states states1) (sort_states states2)
else 
  false 

let rec count_pop (lst : state list) acc = match lst with 
  | [] -> acc
  | h :: t -> count_pop t (acc + h.population)

let rec count_electoral_votes (lst : state list) acc = match lst with 
  | [] -> acc
  | h :: t -> count_electoral_votes t (acc + h.num_votes)

let rec contains_state_helper states st = match states with 
  | [] -> false
  | h :: t -> if (Statepoll.equals st h) then true else contains_state_helper t st
let rec remove_state_helper states st acc = match states with
  | [] -> acc
  | h :: t -> 
    if Statepoll.equals st h 
      then remove_state_helper t st acc 
  else
    remove_state_helper t st (h :: acc)

let rec data_helper lst acc = match lst with 
  | [] -> acc
  | h :: t -> 
    let csv_sub = (Statepoll.export_state_to_csv h) in 
    let csv_arr = Csv.to_array csv_sub in 
    let of_interest = csv_arr.(1) in 
    let list_conv = Array.to_list of_interest in 
    data_helper t (list_conv :: acc)

let rec get_names_helper lst acc = match lst with 
| [] -> acc
| h :: t -> get_names_helper t ((Statepoll.get_name h) :: acc)


let rec create_country_csv_helper lst acc = match lst with 
| [] -> acc
| h :: t -> create_country_csv_helper t ((Statepoll.create_state h) :: acc)
  
(** implementations *)
exception ImproperCSV of string 
let equals cnt1 cnt2 = equals_helper cnt1.states cnt2.states 
let create_country lst boo nam = 
  {electoral_college = boo; states = lst; name = nam}

let create_country_from_CSV (csv : Csv.t) name bool = 
  if (Csv.columns csv) <> 6 
    then raise 
  (ImproperCSV "")
else if (Csv.lines csv) = 1
  then raise
  (ImproperCSV "")
else 
  try 
  let states = create_country_csv_helper (List.tl csv) [] in 
  create_country states bool name
with 
| Statepoll.ImproperList _ -> 
  raise (ImproperCSV (""))

let get_population cnt = count_pop cnt.states 0 

let get_electoral_votes cnt = 
  if cnt.electoral_college then 
    let votes = count_electoral_votes cnt.states 0 
in 
    Some (votes)
else
  None 


let get_name cnt = cnt.name 

let get_states cnt = cnt.states

let get_state_names cnt = 
  let states = get_states cnt 
in 
  let state_names = get_names_helper states []
in 
  List.sort (String.compare) state_names
let contains_state cnt st = contains_state_helper cnt.states st 

let add_state cnt st = 
  if contains_state cnt st 
    then () 
else 
    cnt.states <- (st :: cnt.states)

let remove_state cnt st = 
  if ((contains_state cnt st)) then 
    let state_list_new = remove_state_helper cnt.states st []
in 
  cnt.states <- state_list_new
else 
  ()

let electoral_college_enabled cnt = cnt.electoral_college

let attributes = [
  "name"; 
  "abbr"; 
  "votes"; 
  "pop"; 
  "pref_can"; 
  "pref_percent"
  ] 

let export_data cnt = 
  let state_data = data_helper cnt.states [] in 
  let csv_data = attributes :: state_data in 
  Csv.transpose (Csv.transpose csv_data)
  
let save_data_locally cnt filename = 
  let csv = export_data cnt in 
  let file_name = filename ^ ".csv" in 
  Csv.save file_name csv 