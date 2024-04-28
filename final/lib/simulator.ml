exception InvalidData

(* METADATA *)

module CandidateList = struct
  type t = Candidate.t list

  let of_csv (csv : Csv.t) =
    let labels = List.hd csv |> List.mapi (fun pos lbl -> (lbl, pos)) in
    let get_pos lbl lst =
      try List.nth lst (List.assoc lbl labels)
      with Not_found -> raise InvalidData
    in
    let rec extract : string list list -> t = function
      | [] -> []
      | h :: t ->
          { name = get_pos "name" h; party = get_pos "party" h } :: extract t
    in
    extract (List.tl csv)
end

module StateList = struct
  type t = State.t list

  let of_csv (csv : Csv.t) =
    let labels = List.hd csv |> List.mapi (fun pos lbl -> (lbl, pos)) in
    let get_pos lbl lst =
      try List.nth lst (List.assoc lbl labels)
      with Not_found -> raise InvalidData
    in
    let rec extract : string list list -> t = function
      | [] -> []
      | h :: t -> (
          try
            {
              name = get_pos "name" h;
              votes = int_of_string (get_pos "votes" h);
              pop = int_of_string (get_pos "pop" h);
              pref_can = get_pos "pref_can" h;
              pref_percent = float_of_string (get_pos "pref_percent" h);
              abbr = get_pos "abbr" h;
            }
            :: extract t
          with _ -> raise InvalidData)
    in
    extract (List.tl csv)
end

module CandidatesFile = Data.Make (CandidateList)
module StatesFile = Data.Make (StateList)

let candidates : Candidate.t list =
  CandidatesFile.(of_path "data/metadata/candidates.csv" |> extract)

let states : State.t list =
  StatesFile.(of_path "data/metadata/states.csv" |> extract)

(* POLLING DATA *)

module Polling = struct
  type p = (string * float) list
  type t = p list

  let of_csv (csv : Csv.t) =
    let csv_t = Csv.transpose csv in
    let labels = List.hd csv_t |> List.mapi (fun pos lbl -> (pos, lbl)) in
    let rec extract : string list list -> t = function
      | [] -> []
      | h :: t -> (
          try
            List.mapi
              (fun pos num -> (List.assoc pos labels, float_of_string num))
              h
            :: extract t
          with _ -> raise InvalidData)
    in
    extract (List.tl csv_t)
end

module PollingFile = Data.Make (Polling)

(* STATE & POLLING DATA*)

module StringMap = Map.Make (struct
  type t = string

  let compare = Stdlib.compare
end)

let get_polling (state : State.t) =
  PollingFile.(of_path ("data/polling/" ^ state.abbr ^ "_poll.csv") |> extract)

let states_and_polls =
  List.fold_left
    (fun map (state : State.t) ->
      StringMap.add state.name (state, get_polling state) map)
    StringMap.empty states
