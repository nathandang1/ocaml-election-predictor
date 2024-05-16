type screen =
  | Quit
  | Title
  | Menu
  | StatePoll
  | Simulator
  | Results of Candidate.t list * State.t list * Model.model * bool

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

let rec read_int_rec query =
  ANSITerminal.print_string [] query;
  match int_of_string_opt (read_line ()) with
  | Some n -> n
  | None -> read_int_rec query

let rec read_int_rec_range (lb, ub) query =
  ANSITerminal.print_string [] query;
  match int_of_string_opt (read_line ()) with
  | Some n ->
      if n >= lb && ub >= n then n else read_int_rec_range (lb, ub) query
  | None -> read_int_rec_range (lb, ub) query

let rec read_float_rec query =
  ANSITerminal.print_string [] query;
  match float_of_string_opt (read_line ()) with
  | Some n -> n
  | None -> read_float_rec query

let rec print_states = function
  | (state : State.t) :: tl ->
      ANSITerminal.print_string [] ("[" ^ state.abbr ^ "] " ^ state.name ^ "\n");
      print_states tl
  | [] -> ()

let country states =
  let statepoll_states =
    List.map
      (fun (state : State.t) ->
        Statepoll.create_state
          [
            state.name;
            state.abbr;
            string_of_int state.votes;
            string_of_int state.pop;
            state.pref_can;
            string_of_float state.pref_percent;
          ])
      states
  in
  Countrypoll.create_country statepoll_states true "USA"

let poll_cols =
  [ "year"; "state"; "percent_votes_rep"; "percent_votes_dem"; "winner" ]

let add_poll (_, states) =
  ANSITerminal.erase Screen;
  ANSITerminal.print_string [ ANSITerminal.Bold ] "ADDING POLL: \n";
  print_states states;
  print_endline "";
  ANSITerminal.print_string []
    "Please specify the state for which you would like to add a poll: \n";
  let () = ANSITerminal.print_string [] "ABBR: " in
  let abbr = read_line () in
  let (selected_state : State.t) =
    List.hd (List.filter (fun (s : State.t) -> s.abbr = abbr) states)
  in
  print_endline "";
  ANSITerminal.print_string [] "Please specify the data for this poll: \n";
  let year = read_int_rec "YEAR: " |> string_of_int in
  let () = ANSITerminal.print_string [] "STATE: " in
  let state = read_line () in
  let rep_percentage =
    read_float_rec "REPUBLICAN PERCENTAGE: " |> string_of_float
  in
  let dem_percentage =
    read_float_rec "DEMOCRAT PERCENTAGE: " |> string_of_float
  in
  let () = ANSITerminal.print_string [] "WINNER [rep, dem]: " in
  let winner = read_line () in
  let local =
    Local.shadow
      ("data/local/data-extraction/state-data/"
      ^ String.lowercase_ascii selected_state.name
      ^ ".csv")
      ("data/data-extraction/state-data/"
      ^ String.lowercase_ascii selected_state.name
      ^ ".csv")
  in
  let path = Local.path local in
  let true_local =
    "data/local/data-extraction/state-data/"
    ^ String.lowercase_ascii selected_state.name
    ^ ".csv"
  in
  try
    let data = Csv.load path in
    let new_data =
      List.hd data
      :: [ year; state; rep_percentage; dem_percentage; winner ]
      :: List.tl data
    in
    Csv.save true_local new_data
  with _ ->
    let new_data =
      [ poll_cols; [ year; state; rep_percentage; dem_percentage; winner ] ]
    in
    Csv.save true_local new_data

let remove_poll (_, states) =
  ANSITerminal.erase Screen;
  ANSITerminal.print_string [ ANSITerminal.Bold ] "ADDING POLL: \n";
  print_states states;
  print_endline "";
  ANSITerminal.print_string []
    "Please specify the state for which you would like to remove a poll: \n";
  let () = ANSITerminal.print_string [] "ABBR: " in
  let abbr = read_line () in
  let (selected_state : State.t) =
    List.hd (List.filter (fun (s : State.t) -> s.abbr = abbr) states)
  in
  print_endline "";
  ANSITerminal.print_string [ ANSITerminal.Bold ]
    (String.uppercase_ascii selected_state.name ^ ": \n");
  let local =
    Local.shadow
      ("data/local/data-extraction/state-data/"
      ^ String.lowercase_ascii selected_state.name
      ^ ".csv")
      ("data/data-extraction/state-data/"
      ^ String.lowercase_ascii selected_state.name
      ^ ".csv")
  in
  let path = Local.path local in
  let data = Csv.load path in
  let _ = List.map (fun x -> print_endline (List.hd x)) (List.tl data) in
  print_endline "";
  ANSITerminal.print_string []
    "Please specify the poll you would like to remove: \n";
  let () = ANSITerminal.print_string [] "REMOVED POLL: " in
  let removed_poll = read_line () in
  let new_data = List.filter (fun row -> List.hd row <> removed_poll) data in
  let true_local =
    "data/local/data-extraction/state-data/"
    ^ String.lowercase_ascii selected_state.name
    ^ ".csv"
  in
  Csv.save true_local new_data

let add_state (_, states) =
  ANSITerminal.erase Screen;
  ANSITerminal.print_string [ ANSITerminal.Bold ] "ADDING STATES: \n";
  print_states states;
  print_endline "";
  ANSITerminal.print_string []
    "Please specify the state you would like to add: \n";
  let () = ANSITerminal.print_string [] "NAME: " in
  let name = read_line () in
  let () = ANSITerminal.print_string [] "ABBR: " in
  let abbr = read_line () in
  let votes = read_int_rec "VOTES: " in
  let pop = read_int_rec "POPULATION: " in
  let () = ANSITerminal.print_string [] "PREFERRED CANDIDATE: " in
  let pref_can = read_line () in
  let pref_percent = read_float_rec "PREFERRED PERCENT: " in
  let new_state : State.t =
    { name; abbr; votes; pop; pref_can; pref_percent }
  in
  let all_states =
    List.sort_uniq
      (fun (s0 : State.t) (s1 : State.t) -> Stdlib.compare s0.name s1.name)
      (new_state :: states)
  in
  let nation = country all_states in
  Countrypoll.save_data_locally nation "data/local/metadata/states"

let remove_state (_, states) =
  ANSITerminal.erase Screen;
  ANSITerminal.print_string [ ANSITerminal.Bold ] "REMOVING STATES: \n";
  print_states states;
  print_endline "";
  ANSITerminal.print_string []
    "Please specify the state you would like to remove: \n";
  ANSITerminal.print_string [] "ABBR: ";
  let state_abbr = read_line () in
  let other_states =
    List.filter (fun (s : State.t) -> s.abbr <> state_abbr) states
  in
  let nation = country other_states in
  Countrypoll.save_data_locally nation "data/local/metadata/states"

let print_statepoll (s : Statepoll.state) =
  print_endline ("Name: " ^ Statepoll.get_name s);
  print_endline ("Abbreviation: " ^ Statepoll.get_abbreviation s);
  print_endline ("* Votes: " ^ string_of_int (Statepoll.get_num_votes s));
  print_endline ("* Population: " ^ string_of_int (Statepoll.get_population s));
  print_endline
    ("* Preferred Candidate: " ^ Statepoll.get_preferred_candidate_name s);
  print_endline
    ("* Preferred Margin: " ^ string_of_float (Statepoll.get_preferred_margin s))

let modify_state (_, states) =
  ANSITerminal.erase Screen;
  ANSITerminal.print_string [ ANSITerminal.Bold ] "MODIFYING STATES: \n";
  print_states states;
  print_endline "";
  ANSITerminal.print_string []
    "Please specify the state you would like to modify: \n";
  ANSITerminal.print_string [] "ABBR: ";
  let state_abbr = read_line () in
  let (selected_state : State.t) =
    List.hd (List.filter (fun (s : State.t) -> s.abbr = state_abbr) states)
  in
  let other_states =
    List.filter (fun (s : State.t) -> s.abbr <> state_abbr) states
  in
  let modifying_state =
    Statepoll.create_state
      [
        selected_state.name;
        selected_state.abbr;
        string_of_int selected_state.votes;
        string_of_int selected_state.pop;
        selected_state.pref_can;
        string_of_float selected_state.pref_percent;
      ]
  in
  print_endline "";
  ANSITerminal.print_string [ ANSITerminal.Bold ]
    (String.uppercase_ascii selected_state.name ^ ": \n");
  print_statepoll modifying_state;
  print_endline "";
  ANSITerminal.print_string []
    "Please specify the field you would like to modify: \n";
  let () =
    ANSITerminal.print_string []
      "FIELD NAME [vote, pop, pref_can, pref_margin]: "
  in
  let field_name = read_line () in
  let rec choose_val () =
    match field_name with
    | "vote" ->
        Statepoll.set_num_votes modifying_state (read_int_rec "NEW VALUE: ")
    | "pop" ->
        Statepoll.set_num_votes modifying_state (read_int_rec "NEW VALUE: ")
    | "pref_can" ->
        ANSITerminal.print_string [] "NEW VALUE: ";
        Statepoll.set_preferred_candidate modifying_state (read_line ())
    | "pref_margin" ->
        Statepoll.set_preferred_margin modifying_state
          (read_float_rec "NEW VALUE: ")
    | _ -> choose_val ()
  in
  choose_val ();
  print_endline "";
  ANSITerminal.print_string [ ANSITerminal.Bold ] "UPDATED STATE: \n";
  print_statepoll modifying_state;
  print_endline "";
  ignore (read_line ());
  let nation = country other_states in
  let () = Countrypoll.add_state nation modifying_state in
  Countrypoll.save_data_locally nation "data/local/metadata/states"

let cand_local =
  Local.shadow "data/local/metadata/candidates.csv"
    "data/metadata/candidates.csv"

let state_local =
  Local.shadow "data/local/metadata/states.csv" "data/metadata/states.csv"

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
  ANSITerminal.print_string [] "[5] Modify State \n";
  print_endline "";
  let cand_path = Local.path cand_local in
  let state_path = Local.path state_local in
  let current_data = Extractor.data (cand_path, state_path) in
  let () =
    match read_line () with
    | "1" -> add_poll current_data
    | "2" -> remove_poll current_data
    | "3" -> add_state current_data
    | "4" -> remove_state current_data
    | "5" -> modify_state current_data
    | _ -> ()
  in
  Menu

let uniform_model () =
  ANSITerminal.erase Screen;
  ANSITerminal.print_string [ ANSITerminal.Bold ] "UNIFORM MODEL \n";
  print_endline "";
  ANSITerminal.print_string []
    "The uniform model assumes that all candidates are equally likely to win \
     the election. It conducts a simulation based on that assumption. \n";
  print_endline "";
  ANSITerminal.print_string [] "< Press ENTER to Run > \n";
  print_endline "";
  ignore (read_line ());
  let cand_path = Local.path cand_local in
  let state_path = Local.path state_local in
  match Extractor.data (cand_path, state_path) with
  | cands, states -> Results (cands, states, Uniform, true)

let rec randomization_query () =
  ANSITerminal.print_string [] "< Enable Randomization [Y/n]? > ";
  match read_line () with
  | "Y" -> true
  | "n" -> false
  | _ -> randomization_query ()

let naive_bayes () =
  ANSITerminal.erase Screen;
  ANSITerminal.print_string [ ANSITerminal.Bold ] "BAYES MODEL \n";
  print_endline "";
  ANSITerminal.print_string []
    "The Naive Bayes model uses Bayesian inference to determine the \
     likelihoods of each candidate winning the election. It conducts a \
     simulation based on that assumption. \n";
  print_endline "";
  let randomized = randomization_query () in
  let () = print_endline "" in
  let randomness =
    read_int_rec_range (0, 50) "< Randomness Score? [0 - 50]? > "
  in
  let cand_path = Local.path cand_local in
  let state_path = Local.path state_local in
  match Extractor.data (cand_path, state_path) with
  | cands, states -> Results (cands, states, Bayes randomness, randomized)

let logistic_model () =
  ANSITerminal.erase Screen;
  ANSITerminal.print_string [ ANSITerminal.Bold ] "LOGISTIC MODEL \n";
  print_endline "";
  ANSITerminal.print_string []
    "Logistic regression is a classic machine learning model used for \
     classification. In this simulation the LR model gives strong weightage to \
     the preferences expressed by states through past election data. \n";
  print_endline "";
  let randomized = randomization_query () in
  let cand_path = Local.path cand_local in
  let state_path = Local.path state_local in
  match Extractor.data (cand_path, state_path) with
  | cands, states -> Results (cands, states, Logistic, randomized)

let simulator () =
  ANSITerminal.erase Screen;
  ANSITerminal.print_string [ ANSITerminal.Bold ] "MODELS \n";
  print_endline "";
  ANSITerminal.print_string [] "[1] Uniform Random \n";
  print_endline "";
  ANSITerminal.print_string [] "[2] Naive Bayes \n";
  print_endline "";
  ANSITerminal.print_string [] "[3] Logistic Regression \n";
  print_endline "";
  match read_line () with
  | "1" -> uniform_model ()
  | "2" -> naive_bayes ()
  | "3" -> logistic_model ()
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

let results (candidates, state) model randomized =
  let state_odds = Model.run_all (candidates, state) model in
  let outcomes = Simulator.simulate_all state_odds randomized in
  let electors = Simulator.votes outcomes in
  let tied = snd (List.hd electors) = snd (List.hd (List.tl electors)) in
  let winner : Candidate.t = fst (List.hd electors) in
  let votes = snd (List.hd electors) in
  ANSITerminal.erase Screen;
  ANSITerminal.print_string [ ANSITerminal.Bold ] "RESULTS \n";
  print_endline "";
  print_outcomes outcomes;
  print_endline "";
  print_electors electors;
  print_endline "";
  if tied then
    print_endline
      ("The election is tied. Both candidates have " ^ string_of_int votes
     ^ " electors.")
  else
    print_endline
      (winner.name ^ " has won the election with " ^ string_of_int votes
     ^ " electors.");
  print_endline "";
  Quit

let rec transition = function
  | Quit -> ()
  | Title -> transition (title ())
  | Menu -> transition (menu ())
  | StatePoll -> transition (state_poll ())
  | Simulator -> transition (simulator ())
  | Results (candidates, state, model, randomized) ->
      transition (results (candidates, state) model randomized)
