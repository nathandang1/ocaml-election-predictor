open Final

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

let rec print_states (states : State.S.t list) electors =
  match states with
  | [] ->
      print_endline ("Final: " ^ electors_to_string electors);
      print_endline "";
      get_winner electors
  | h :: t ->
      (* TODO: change computation of the "winner" here. *)
      let line = h.pref_can ^ " wins " ^ h.name ^ ". \t" in
      let new_vote = List.assoc h.pref_can electors + h.votes in
      let new_counts =
        (h.pref_can, new_vote) :: List.remove_assoc h.pref_can electors
      in
      let sorted_counts =
        List.sort
          (fun cand1 cand2 -> -1 * Stdlib.compare (snd cand1) (snd cand2))
          new_counts
      in
      let () = print_endline (line ^ electors_to_string sorted_counts) in
      print_states t sorted_counts

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

let cand_probabilities = prompt_priors states [] (List.hd candidates).name

(* let rec candidate_two_probabilities probs acc = match probs with | [] -> acc
   | (a, b) :: t -> candidate_two_probabilities t ((a, 1. -. b) :: acc)

   let biden_probabilities = candidate_two_probabilities trump_probabilities
   [] *)
let cand_probabilities = List.rev cand_probabilities

module CSV = struct
  type t = Csv.t

  let from_csv = true
  let of_data (data : string list list) : Csv.t = data
end

module CSVFile = File.Make (CSV)

(** [read_csv path] reads the csv found at [path]. *)
let read_csv path = CSVFile.extract path

(** [calc_state_results state_prior c electors] calculates which candidate is
    more likely to win in the state, given some tuple [state_prior] containing
    the state and the prior value. *)
let rec calc_state_results (state_prior : State.S.t * float) cand electors =
  let data = [ ("Donald Trump", 0.5); ("Joe Biden", 0.5) ] in
  let state = fst state_prior in
  let prior = snd state_prior in
  let new_val_cand = List.assoc cand data *. prior in
  let new_val_other = List.assoc cand data *. (1. -. prior) in

  (* Should have a separate case for = (WIP) *)
  if new_val_cand >= new_val_other then
    let new_vote = List.assoc cand electors + state.votes in
    let new_counts = (cand, new_vote) :: List.remove_assoc cand electors in
    List.sort
      (fun cand1 cand2 -> -1 * Stdlib.compare (snd cand1) (snd cand2))
      new_counts
  else
    let other_cand = fst (List.hd (List.remove_assoc cand electors)) in
    let new_vote = List.assoc other_cand electors + state.votes in
    let new_counts =
      (other_cand, new_vote) :: List.remove_assoc other_cand electors
    in
    List.sort
      (fun cand1 cand2 -> -1 * Stdlib.compare (snd cand1) (snd cand2))
      new_counts

let _ = print_states states candidates_electors
