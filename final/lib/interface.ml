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

let add_poll () = failwith ""
let remove_poll () = failwith ""
let add_state () = failwith ""
let remove_state () = failwith ""

let state_poll () =
  ANSITerminal.erase Screen;
  ANSITerminal.print_string [ ANSITerminal.Bold ] "POLLING DATA \n";
  print_endline "";
  ANSITerminal.print_string [] "[1] Add Poll \n";
  print_endline "";
  ANSITerminal.print_string [] "[2] Remove Poll \n";
  print_endline "";
  ANSITerminal.print_string [] "[3] Add State \n";
  print_endline "";
  ANSITerminal.print_string [] "[4] Remove State \n";
  print_endline "";
  Menu

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

let rec print_outcomes = function
  | ((state : State.t), (winner : Candidate.t)) :: tl ->
      ANSITerminal.print_string []
        (state.name ^ " -> " ^ winner.name ^ " [" ^ string_of_int state.votes
       ^ "] \n");
      print_outcomes tl
  | [] -> ()

let rec print_electors = function
  | ((candidate : Candidate.t), num_of_electors) :: tl ->
      ANSITerminal.print_string []
        (candidate.name ^ " : " ^ string_of_int num_of_electors ^ " electors \n");
      print_electors tl
  | [] -> ()

let results (candidates, state, polling) =
  let state_odds = Model.run_all (candidates, state, polling) in
  let outcomes = Simulator.simulate_all state_odds in
  let electors = Simulator.votes outcomes in
  let winner : Candidate.t = fst (List.hd electors) in
  let votes = snd (List.hd electors) in
  ANSITerminal.erase Screen;
  ANSITerminal.print_string [ ANSITerminal.Bold ] "RESULTS \n";
  print_endline "";
  print_outcomes outcomes;
  print_endline "";
  print_electors electors;
  print_endline "";
  print_endline
    (winner.name ^ " has won the election with " ^ string_of_int votes
   ^ " votes.");
  print_endline "";
  Quit

let rec transition = function
  | Quit -> ()
  | Title -> transition (title ())
  | Menu -> transition (menu ())
  | StatePoll -> transition (state_poll ())
  | Simulator -> transition (simulator ())
  | Results (candidates, state, polling) ->
      transition (results (candidates, state, polling))
