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

<<<<<<< HEAD
let rec prompt_and_print states acc = match states with 
| [] -> acc
| h :: t -> let () = print_endline ("what is the probability that Biden wins in " ^ h ^ "? (0.0 - 1.0)") in 
let the_input = read_line () in 
match (float_of_string_opt the_input) with 
| None -> let () = print_endline ("please enter a valid probability") in prompt_and_print states acc
| Some x -> 
  if x < 0.0 || x > 1.0 then 
  let () = print_endline ("please enter a valid probability") in prompt_and_print states acc 
else
  let tup = (h, x) in prompt_and_print t (tup :: acc) 

let rec candidate_two_probabilities probs acc = match probs with 
| [] -> acc
| (a, b) :: t -> candidate_two_probabilities t ((a, 1. -. b) :: acc)

=======
>>>>>>> 6f018ab14933321c60c805870b5868d6bfa07364
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

let _ = print_states states candidates_electors
