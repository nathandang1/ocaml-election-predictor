(* Modules for Loading State and Candidate Data *)
exception CsvFormatError

module LabelExtract = struct
  let extract (of_row : (string * int) list -> string list -> 't) (csv : Csv.t)
      =
    let rows = csv in
    let cols = Csv.transpose csv in
    let row_len = List.length rows in
    let col_len = List.length cols in
    if (row_len > 0 && col_len > 0) = false then raise CsvFormatError
    else
      let labels = List.mapi (fun i lbl -> (lbl, i)) (List.hd rows) in
      let data = List.tl rows in

      let rec to_lst = function
        | h :: t -> of_row labels h :: to_lst t
        | [] -> []
      in
      to_lst data
end

module StateList = struct
  type t = State.t list

  let to_state labels row : State.t =
    {
      name = List.nth row (List.assoc "name" labels);
      abbr = List.nth row (List.assoc "abbr" labels);
      votes = int_of_string (List.nth row (List.assoc "votes" labels));
      pop = int_of_string (List.nth row (List.assoc "pop" labels));
      pref_can = "" (* placeholder *);
      pref_percent = 0. (* placeholder *);
    }

  let extract = LabelExtract.extract to_state
end

module CandidateList = struct
  type t = Candidate.t list

  let to_candidate labels row : Candidate.t =
    {
      name = List.nth row (List.assoc "name" labels);
      party = List.nth row (List.assoc "party" labels);
    }

  let extract = LabelExtract.extract to_candidate
end

module StatesFile = File.Make (StateList)
module CandidatesFile = File.Make (CandidateList)

(* Module for Loading Polling Data *)
module Polling = struct
  type t = Csv.t

  let extract (csv : Csv.t) =
    csv |> Csv.transpose
    |> List.mapi (fun i0 lst ->
           List.mapi (fun i1 str -> if i0 + i1 = 0 then "polls" else str) lst)
end

module PollingFile = File.Make (Polling)

let get_poll file = file |> PollingFile.of_path |> PollingFile.data

let data (cand_path, state_path, poll_path) =
  let candidates = cand_path |> CandidatesFile.of_path |> CandidatesFile.data in
  let states = state_path |> StatesFile.of_path |> StatesFile.data in
  let polling =
    List.map
      (fun (state : State.t) ->
        (state, get_poll (poll_path ^ "/" ^ state.abbr ^ ".csv")))
      states
  in
  ( (candidates : Candidate.t list),
    (states : State.t list),
    (polling : (State.t * Csv.t) list) )
