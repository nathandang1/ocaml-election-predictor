val simulate :
  State.t * (Candidate.t * float) list -> bool -> State.t * Candidate.t
(** [simulate (state, probabilities) is_random] is the simulated winner of
    [state] given the likelihoods of winning for each candidate specified in
    [probabilities]. If [is_random] is true, then the simulator randomizes with
    weighted outputs. *)

val simulate_all :
  (State.t * (Candidate.t * float) list) list ->
  bool ->
  (State.t * Candidate.t) list
(** [simulate_all states is_random] is a list with the simulated winners for all
    states in [states] given the specified likelihoods of winning for each
    candidate. If [is_random] is true, then the simulator randomizes with
    weighted outputs.*)

val votes : (State.t * Candidate.t) list -> (Candidate.t * int) list
(** [votes outcome] is a list with the total number of electors for each
    candidate given [outcome]. The list is sorted from the candidate with the
    most electors to the candidate with the least electors. *)
