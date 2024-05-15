type screen =
  | Quit
  | Title
  | Menu
  | StatePoll
  | Simulator
  | Results of Candidate.t list * State.t list * (State.t * Csv.t) list

let title () =
  ANSITerminal.erase Screen;
  ANSITerminal.print_string [ ANSITerminal.Bold ] "OCAML ELECTION SIMULATOR \n";
  print_endline "";
  ANSITerminal.print_string [] "<Press ENTER to Start> \n";
  print_endline "";
  ignore (read_line ());
  Menu

let menu () =
  ANSITerminal.erase Screen;
  ANSITerminal.print_string [ ANSITerminal.Bold ] "MAIN MENU \n";
  print_endline "";
  ANSITerminal.print_string [] "[1] Configure Data \n";
  print_endline "";
  ANSITerminal.print_string [] "[2] Run Simulator \n";
  print_endline "";
  match read_line () with
  | "1" -> StatePoll
  | "2" -> Simulator
  | _ -> Menu

let state_poll () = failwith ""

let uniform_model () =
  ANSITerminal.erase Screen;
  ANSITerminal.print_string [ ANSITerminal.Bold ] "UNIFORM MODEL \n";
  print_endline "";
  ANSITerminal.print_string []
    "The uniform model assumes that all candidates are equally likely to win \
     the election. It conducts a simulation based on that assumption. \n";
  print_endline "";
  ANSITerminal.print_string [] "<Press ENTER to Run> \n";
  print_endline "";
  ignore (read_line ());
  let cand_path = "data/metadata/candidates.csv" in
  let state_path = "data/metadata/states.csv" in
  let poll_path = "data/polling" in
  match Extractor.data (cand_path, state_path, poll_path) with
  | cand_path, state_path, poll_path ->
      Results (cand_path, state_path, poll_path)

let simulator () =
  ANSITerminal.erase Screen;
  ANSITerminal.print_string [ ANSITerminal.Bold ] "MODELS \n";
  print_endline "";
  ANSITerminal.print_string [] "[1] Uniform Random \n";
  print_endline "";
  ANSITerminal.print_string [] "[2] Naive Bayes \n";
  print_endline "";
  match read_line () with
  | "1" -> uniform_model ()
  | "2" -> failwith ""
  | _ -> Simulator

let results (candidates, state, polling) =
  let state_odds = Model.run_all (candidates, state, polling) in
  let outcomes = Simulator.simulate_all state_odds in
  let electors = Simulator.votes outcomes in
  let winner : Candidate.t = fst (List.hd electors) in
  let votes = snd (List.hd electors) in
  ANSITerminal.erase Screen;
  print_endline
    (winner.name ^ " has won the election with " ^ string_of_int votes
   ^ " votes.");
  Quit

let rec transition = function
  | Quit -> ()
  | Title -> transition (title ())
  | Menu -> transition (menu ())
  | StatePoll -> state_poll ()
  | Simulator -> transition (simulator ())
  | Results (candidates, state, polling) ->
      transition (results (candidates, state, polling))
