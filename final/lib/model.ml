type model =
  | Uniform
  | Bayes
  | Logistic

let uniform_model candidates =
  List.map
    (fun cand -> (cand, 1. /. float_of_int (List.length candidates)))
    candidates

let bayes_model (state : State.t) =
  match
    Models.naive_bayes_randomized
      (List.tl
         (Csv.load
            ("data/data-extraction/state-data/"
            ^ String.lowercase_ascii state.name
            ^ ".csv")))
  with
  | dem, rep ->
      [
        (Candidate.create ("Joe Biden", "Democrat"), dem);
        (Candidate.create ("Donald Trump", "Republican"), rep);
      ]

let logistic_model (state : State.t) =
  let data =
    List.tl
      (Csv.load
         ("data/data-extraction/state-data/"
         ^ String.lowercase_ascii state.name
         ^ ".csv"))
  in
  match Models.logistic_regression data with
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
    | Logistic -> logistic_model (state : State.t)
  in
  (state, probabilities)

let run_all (candidates, states) model =
  List.map (fun state -> run (candidates, state) model) states
