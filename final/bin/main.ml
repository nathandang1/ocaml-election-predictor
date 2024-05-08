open Final

let file_data () =
  let cand_path = "data/metadata/candidates.csv" in
  let state_path = "data/metadata/states.csv" in
  let poll_path = "data/polling" in
  Extractor.data (cand_path, state_path, poll_path)

let main () =
  let data = file_data () in
  let state_odds = Model.run_all data in
  let outcomes = Simulator.simulate_all state_odds in
  let electors = Simulator.votes outcomes in
  let winner : Candidate.t = fst (List.hd electors) in
  let votes = snd (List.hd electors) in
  print_endline
    (winner.name ^ " has won the election with " ^ string_of_int votes
   ^ " votes.")

let _ = main ()
