val data :
  string * string * string ->
  Candidate.t list * State.t list * (State.t * Csv.t) list
(** [data (cand_path, state_path, poll_path)] is a tuple containing the relevant
    data for an election simulation model, where [cand_path] is the path to
    candidate metadata, [state_path] is the path to state metadata, and
    [poll_path] is the path to the folder containing polling data. *)
