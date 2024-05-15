let run ((candidates : Candidate.t list), (state : State.t), polling) =
  let () = ignore polling in
  (* INSERT MODEL HERE *)
  let probabilities =
    (* TEMP IMPLEMENTATION: declares each candidate is equally likely to win. *)
    List.map
      (fun cand -> (cand, 1. /. float_of_int (List.length candidates)))
      candidates
  in
  (state, probabilities)

let run_all (candidates, states, polling) =
  List.map (fun state -> run (candidates, state, polling)) states
