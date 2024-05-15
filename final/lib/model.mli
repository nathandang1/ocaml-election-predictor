val run :
  Candidate.t list * State.t * (State.t * Csv.t) list ->
  State.t * (Candidate.t * float) list
(** [run (candidates, state, polling)] is the probabilities that each candidate
    in [candidates] wins in [state] given [polling]. *)

val run_all :
  Candidate.t list * State.t list * (State.t * Csv.t) list ->
  (State.t * (Candidate.t * float) list) list
(** [run (candidates, states, polling)] is the probabilities that each candidate
    in [candidates] wins in each state in [states] given [polling]. *)
