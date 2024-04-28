exception InvalidData

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
