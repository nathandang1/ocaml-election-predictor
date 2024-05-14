open Csv

let filter_rows rows =
  List.filter
    (fun row ->
      let party = List.nth row 1 in
      let year = int_of_string (List.nth row 2) in
      let candidatevotes = float_of_string (List.nth row 3) in
      let totalvotes = float_of_string (List.nth row 4) in
      (party = "DEMOCRAT" || party = "REPUBLICAN")
      && year >= 1990 && totalvotes <> 0.)
    rows

let add_percent_votes rows =
  List.map
    (fun row ->
      let candidatevotes = float_of_string (List.nth row 3) in
      let totalvotes = float_of_string (List.nth row 4) in
      let percent_votes = candidatevotes /. totalvotes in
      row @ [ string_of_float percent_votes ])
    rows

let group_by_state rows =
  let table = Hashtbl.create 50 in
  List.iter
    (fun row ->
      let state = List.nth row 0 in
      let data = Hashtbl.find_opt table state in
      match data with
      | Some data -> Hashtbl.replace table state (row :: data)
      | None -> Hashtbl.add table state [ row ])
    rows;
  table

let write_to_csv state data =
  let oc = open_out (state ^ ".csv") in
  Csv.to_channel oc data;
  close_out oc

let () =
  let data = Csv.load "1976-2020-president.csv" in
  let filtered = filter_rows data in
  let with_percent_votes = add_percent_votes filtered in
  let grouped = group_by_state with_percent_votes in
  Hashtbl.iter write_to_csv grouped
