type state = Statepoll.state 

type country = {
  electoral_college : bool; 
  mutable states : state list; 
  name : string ; 
  mutable population : int
}

let rec equals_helper_2 states1 states2 acc = match states1 with 
| [] -> false
| h :: t -> 
  if Statepoll.equals h (List.nth states2 acc) then 
    equals_helper_2 t (List.tl states2) (acc + 1) 
else 
  true 

let equals_helper states1 states2 = 
  if 
    List.length states1 <> List.length states2 
  then false
else 
  equals_helper_2 states1 states2 0

let equals cnt1 cnt2 = equals_helper cnt1.states cnt2.states 

let rec count_pop (lst : state list) acc = match lst with 
| [] -> acc
| h :: t -> count_pop t (acc + h.population)

let create_country lst boo nam = 
  let pop = count_pop lst 0 in 
{electoral_college = boo; states = lst; name = nam; population = pop}

let get_name cnt = cnt.name 

let get_states cnt = cnt.states

let get_population cnt = cnt.population

let rec contains_state_helper states st = match states with 
| [] -> false
| h :: t -> if Statepoll.equals st h then true else contains_state_helper t st

let contains_state cnt st = contains_state_helper cnt.states st 

let add_state cnt st = 
  if contains_state cnt st 
    then () 
else 
  let () = cnt.population <- (cnt.population + st.population) 
in 
  cnt.states <- (st :: cnt.states)

let rec remove_state_helper states st acc = match states with
| [] -> acc
| h :: t -> 
  if Statepoll.equals st h 
    then remove_state_helper t st acc 
else
  remove_state_helper t st (h :: acc)

let remove_state cnt st = 
  if ((contains_state cnt st) <> true) 
    then ()
else 
  let new_list = remove_state_helper cnt.states st []
in 
  cnt.states <- (st :: new_list)

let electoral_college_enabled cnt = cnt.electoral_college

let attributes = ["Name of State"; "Preferred Candidate Name"; 
  "Preferred Candidate Party"; 
  "Preferred Candidate Margin of Preference"; 
  "Number of Votes"; "Population Size"] 
let rec data_helper lst acc = match lst with 
| [] -> acc
| h :: t -> 
  let csv_sub = Csv.transpose (Statepoll.export_state_to_csv h) in 
  let csv_arr = Csv.to_array csv_sub in 
  let of_interest = csv_arr.(1) in 
  let list_conv = Array.to_list of_interest in 
  data_helper t (list_conv :: acc)

let export_data cnt = 
  let state_data = data_helper cnt.states [] in 
  let csv_data = attributes :: state_data in 
  Csv.transpose csv_data 
  
let save_data_locally cnt = 
  let csv = export_data cnt in 
  let file_name = cnt.name ^ "_data.csv" in 
  let () = print_endline ("Data saved at: " ^ file_name) in 
  Csv.save file_name csv 