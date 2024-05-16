val data : string * string -> Candidate.t list * State.t list
(** [data (cand_path, state_path)] is a tuple containing the relevant data for
    an election simulation model, where [cand_path] is the path to candidate
    metadata, and [state_path] is the path to state metadata. *)
