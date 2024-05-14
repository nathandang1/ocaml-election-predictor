type state = Statepoll.state 

type country = {
  electoral_college : bool; 
  states : state list; 
  name : string 
}

let create_country lst boo nam = {electoral_college = boo; states = lst; name = nam}

let get_name cnt = cnt.name 

let get_states cnt = cnt.states

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