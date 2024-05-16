type model =
  | Uniform
  | Bayes of int
  | Logistic

let uniform_model candidates =
  List.map
    (fun cand -> (cand, 1. /. float_of_int (List.length candidates)))
    candidates

let bayes_model (state : State.t) randomness =
  let local =
    Local.shadow
      ("data/local/data-extraction/state-data/"
      ^ String.lowercase_ascii state.name
      ^ ".csv")
      ("data/data-extraction/state-data/"
      ^ String.lowercase_ascii state.name
      ^ ".csv")
  in
  let path = Local.path local in
  try
    let data = List.tl (Csv.load path) in
    match Models.naive_bayes_randomized data randomness with
    | dem, rep ->
        [
          (Candidate.create ("Joe Biden", "Democrat"), dem);
          (Candidate.create ("Donald Trump", "Republican"), rep);
        ]
  with _ ->
    [
      (Candidate.create ("Joe Biden", "Democrat"), 0.5);
      (Candidate.create ("Donald Trump", "Republican"), 0.5);
    ]

let logistic_model (state : State.t) =
  let local =
    Local.shadow
      ("data/local/data-extraction/state-data/"
      ^ String.lowercase_ascii state.name
      ^ ".csv")
      ("data/data-extraction/state-data/"
      ^ String.lowercase_ascii state.name
      ^ ".csv")
  in
  let path = Local.path local in
  try
    let data = List.tl (Csv.load path) in
    match Models.logistic_regression data with
    | dem, rep ->
        [
          (Candidate.create ("Joe Biden", "Democrat"), dem);
          (Candidate.create ("Donald Trump", "Republican"), rep);
        ]
  with _ ->
    [
      (Candidate.create ("Joe Biden", "Democrat"), 0.5);
      (Candidate.create ("Donald Trump", "Republican"), 0.5);
    ]

let run ((candidates : Candidate.t list), (state : State.t)) model =
  let probabilities =
    match model with
    | Uniform -> uniform_model candidates
    | Bayes randomness -> bayes_model (state : State.t) randomness
    | Logistic -> logistic_model (state : State.t)
  in
  (state, probabilities)

let run_all (candidates, states) model =
  List.map (fun state -> run (candidates, state) model) states
