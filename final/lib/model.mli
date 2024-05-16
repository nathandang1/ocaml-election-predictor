type model =
  | Uniform
  | Bayes
  | Logistic

val run :
  Candidate.t list * State.t -> model -> State.t * (Candidate.t * float) list
(** [run (candidates, state, polling) model] is the probabilities that each
    candidate in [candidates] wins in [state] given [polling], with model
    [model]. *)

val run_all :
  Candidate.t list * State.t list ->
  model ->
  (State.t * (Candidate.t * float) list) list
(** [run (candidates, states, polling) model] is the probabilities that each
    candidate in [candidates] wins in each state in [states] given [polling],
    with model [model]. *)
