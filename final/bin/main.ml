open Final

(* FILE READING CODE *)
module CSV = struct
  type t = Csv.t

  let from_csv = true
  let of_data (data : string list list) : Csv.t = data
end

module CSVFile = File.Make (CSV)

(** [read_csv path] reads the csv found at [path]. *)
let read_csv path = Csv.transpose (CSVFile.extract (CSVFile.of_name path))
(* FILE READING CODE *)

module ExtractableStringList = struct
  type t = string list

  let from_csv = false
  let of_data data = List.hd data
end

module StringListFile = File.Make (ExtractableStringList)
module CandidateFile = File.Make (Candidate.C)
module StateFile = File.Make (State.S)

let candidate_files : string list =
  StringListFile.(extract (of_name "data/candidates_list.txt"))

let state_files : string list =
  StringListFile.(extract (of_name "data/states_list.txt"))

module GroupExtract (F : File.F) = struct
  let rec from_lst = function
    | [] -> []
    | h :: t -> F.(extract (of_name h)) :: from_lst t
end

module ExtractCandidates = GroupExtract (CandidateFile)
module ExtractStates = GroupExtract (StateFile)

let candidates : Candidate.C.t list = ExtractCandidates.from_lst candidate_files
let states : State.S.t list = ExtractStates.from_lst state_files
let () = print_endline (string_of_int (List.length states))


let rec prompt_and_print (states_list : State.S.t list) acc = match states with 
| [] -> acc
| h :: t -> let () = print_endline ("what is the probability that Biden wins in " ^ h.name ^ "? (0.0 - 1.0)") in 
let the_input = read_line () in 
match (float_of_string_opt the_input) with 
| None -> let () = print_endline ("please enter a valid probability") in prompt_and_print states_list acc
| Some x -> 
  if x < 0.0 || x > 1.0 then 
  let () = print_endline ("please enter a valid probability") in prompt_and_print states_list acc 
else
  let tup = (h, x) in prompt_and_print t (tup :: acc) 

let rec prompt_and_print (states_list : State.S.t list) acc =
  match states_list with
  | [] -> acc
  | h :: t -> (
      let () =
        print_endline
          ("what is the probability that Biden wins in " ^ h.name
         ^ "? (0.0 - 1.0)")
      in
      let the_input = read_line () in
      match float_of_string_opt the_input with
      | None ->
          let () = print_endline "please enter a valid probability" in
          prompt_and_print states_list acc
      | Some x ->
          if x < 0.0 || x > 1.0 then
            let () = print_endline "please enter a valid probability" in
            prompt_and_print states_list acc
          else
            let tup = (h, x) in
            prompt_and_print t (tup :: acc))

let biden_probabilities = prompt_and_print states []

let rec candidate_two_probabilities probs acc =
  match probs with
  | [] -> acc
  | (a, b) :: t -> candidate_two_probabilities t ((a, 1. -. b) :: acc)

let trump_probabilities = candidate_two_probabilities biden_probabilities []

(* Convert from a Candidate list to a string list *)
let rec extract_cands (candidates : Candidate.C.t list) =
  match candidates with
  | [] -> []
  | h :: t -> (h.name, 0) :: extract_cands t

let candidates_electors = extract_cands candidates

let rec electors_to_string cands =
  match cands with
  | [] -> ""
  | h :: t ->
      "[" ^ fst h ^ ": "
      ^ string_of_int (snd h)
      ^ " electors] " ^ electors_to_string t

let get_winner electors =
  let winner = List.hd electors in
  print_endline
    ("Winner: " ^ fst winner ^ ", " ^ string_of_int (snd winner) ^ " electors.")

(* Get association list data *)
let csv_parse_relevant file =
  let lst = read_csv file in
  let relevant = List.tl lst in
  List.fold_left
    (fun acc cand ->
      (List.hd cand, float_of_string (List.hd (List.tl cand))) :: acc)
    [] relevant

let get_right_data state_name =
  let map_states =
    [
      ("New Jersey", "nj_poll.csv");
      ("North Carolina", "nc_poll.csv");
      ("Pennsylvania", "pa_poll.csv");
    ]
  in
  let file_name = "data/polling/" ^ List.assoc state_name map_states in
  csv_parse_relevant file_name

(** [calc_state_results state_prior c] calculates a Naive Bayes probability of
    which candidate is more likely to win in the state, given some tuple
    [state_prior] containing the state and the prior value. *)
let calc_state_results (state_prior : State.S.t * float) cand =
  let state = fst state_prior in
  let prior = snd state_prior in
  let data = get_right_data state.name in

  let new_data = State.S.outcome state data cand prior in
  let other_cand = List.hd (List.remove_assoc cand new_data) in
  let new_val_cand = List.assoc cand new_data in
  let new_val_other = snd other_cand in
  (* Should have a separate case for = (WIP) *)
  if new_val_cand >= new_val_other then cand else fst other_cand

let rec print_states (states_with_priors : (State.S.t * float) list) cand
    electors =
  match states_with_priors with
  | [] ->
      print_endline ("Final: " ^ electors_to_string electors);
      print_endline "";
      get_winner electors
  | h :: t ->
      let pred_winner = calc_state_results h cand in
      let state = fst h in
      let win_line = pred_winner ^ " wins " ^ state.name ^ ". \t" in
      (* Compute the winner in the state *)
      let new_vote = List.assoc pred_winner electors + state.votes in
      let new_counts =
        (pred_winner, new_vote) :: List.remove_assoc pred_winner electors
      in
      let sorted_counts =
        List.sort
          (fun cand1 cand2 -> -1 * Stdlib.compare (snd cand1) (snd cand2))
          new_counts
      in
      let () = print_endline (win_line ^ electors_to_string sorted_counts) in
      print_states t cand sorted_counts

let rec prompt_priors (states_list : State.S.t list) acc cand =
  match states_list with
  | [] -> acc
  | h :: t -> (
      let () =
        print_endline
          ("What is the probability that " ^ cand ^ " wins in " ^ h.name
         ^ "? (Enter a probability from 0.0 - 1.0)")
      in
      let the_input = read_line () in
      match float_of_string_opt the_input with
      | None ->
          let () = print_endline "Please enter a valid probability." in
          prompt_priors states_list acc cand
      | Some x ->
          if x < 0.0 || x > 1.0 then
            let () = print_endline "Please enter a valid probability." in
            prompt_priors states_list acc cand
          else
            let tup = (h, x) in
            prompt_priors t (tup :: acc) cand)

let first_cand = (List.hd candidates).name
let cand_probabilities = prompt_priors states [] first_cand

(* let rec candidate_two_probabilities probs acc = match probs with | [] -> acc
   | (a, b) :: t -> candidate_two_probabilities t ((a, 1. -. b) :: acc)

   let biden_probabilities = candidate_two_probabilities trump_probabilities
   [] *)
let cand_probabilities = List.rev cand_probabilities
let _ = print_states cand_probabilities first_cand candidates_electors
