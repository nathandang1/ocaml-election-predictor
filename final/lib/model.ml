type model =
  | Uniform
  | Bayes

let uniform_model candidates =
  List.map
    (fun cand -> (cand, 1. /. float_of_int (List.length candidates)))
    candidates

let bayes_model (state : State.t) =
  match
    Models.naive_bayes_randomized
      (List.tl
         (Csv.load ("data/data-extraction/state-data/" ^ state.name ^ ".csv")))
  with
  | dem, rep ->
      [
        (Candidate.create ("Joe Biden", "Democrat"), dem);
        (Candidate.create ("Donald Trump", "Republican"), rep);
      ]

let run ((candidates : Candidate.t list), (state : State.t)) model =
  let probabilities =
    match model with
    | Uniform -> uniform_model candidates
    | Bayes -> bayes_model (state : State.t)
  in
  (state, probabilities)

let run_all (candidates, states) model =
  List.map (fun state -> run (candidates, state) model) states
