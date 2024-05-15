val simulate : State.t * (Candidate.t * float) list -> State.t * Candidate.t
(** [simulate (state, probabilities)] is the simulated winner of [state] given
    the likelihoods of winning for each candidate specified in [probabilities]. *)

val simulate_all :
  (State.t * (Candidate.t * float) list) list -> (State.t * Candidate.t) list
(** [simulate states] is a list with the simulated winners for all states in
    [states] given the specified likelihoods of winning for each candidate. *)

val votes : (State.t * Candidate.t) list -> (Candidate.t * int) list
(** [votes outcome] is a list with the total number of electors for each
    candidate given [outcome]. The list is sorted from the candidate with the
    most electors to the candidate with the least electors. *)
