let () = Random.self_init ()

let random_simulate
    ((state : State.t), (probabilities : (Candidate.t * float) list)) =
  let rec partition = function
    | [] -> []
    | (c, p) :: [] -> [ (c, 0., p) ]
    | (c, p) :: t ->
        let rem = partition t in
        let _, _, prob = List.hd rem in
        (c, prob, prob +. p) :: rem
  in
  let range = partition probabilities in
  let win_num =
    Random.float (List.fold_left (fun acc (_, n) -> acc +. n) 0. probabilities)
  in
  let rec in_range num = function
    | (c, lb, ub) :: t -> if num >= lb && num <= ub then c else in_range num t
    | [] -> failwith "out of bounds"
  in
  let winner = in_range win_num range in
  (state, winner)

let certain_simulate
    ((state : State.t), (probabilities : (Candidate.t * float) list)) =
  let sorted_lst =
    List.sort_uniq (fun (_, p0) (_, p1) -> -Stdlib.compare p0 p1) probabilities
  in
  (state, fst (List.hd sorted_lst))

let simulate ((state : State.t), (probabilities : (Candidate.t * float) list)) =
  function
  | true -> random_simulate (state, probabilities)
  | false -> certain_simulate (state, probabilities)

let rec simulate_all lst is_random =
  match lst with
  | h :: t -> simulate h is_random :: simulate_all t is_random
  | [] -> []

let votes outcome =
  let candidates =
    outcome
    |> List.map (fun (_, cand) -> cand)
    |> List.sort_uniq (fun (c0 : Candidate.t) (c1 : Candidate.t) ->
           Stdlib.compare c0.name c1.name)
  in
  let rec sum_votes = function
    | (h : Candidate.t) :: t ->
        let votes =
          outcome
          |> List.filter (fun (_, (c : Candidate.t)) -> c.name = h.name)
          |> List.fold_left (fun acc ((s : State.t), _) -> acc + s.votes) 0
        in
        (h, votes) :: sum_votes t
    | [] -> []
  in
  List.sort
    (fun (_, n0) (_, n1) -> -Stdlib.compare n0 n1)
    (sum_votes candidates)
