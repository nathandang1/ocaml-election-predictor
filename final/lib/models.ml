(* This file contains the Naive Bayes and Logistic Regression models.*)
let geometric_mean lst =
  let product = List.fold_left ( *. ) 1. lst in
  if List.length lst > 0 then exp (log product /. float_of_int (List.length lst))
  else 1.

let get_reg_total iterations =
  let percent_total = ref 0. in
  for _ = 1 to iterations do
    let result = Regularizer.do_computations () in
    let trump_sum = fst (snd result) in
    let biden_sum = snd (snd result) in
    percent_total := !percent_total +. Regularizer.diff trump_sum biden_sum
  done;
  !percent_total

let naive_bayes_randomized state_data randomness =
  let dem_wins =
    List.filter
      (fun lst -> List.nth lst (List.length lst - 1) = "dem")
      state_data
  in
  let rep_wins =
    List.filter
      (fun lst -> List.nth lst (List.length lst - 1) = "rep")
      state_data
  in

  let dem_rep_percentages =
    List.map (fun lst -> float_of_string (List.nth lst 2)) dem_wins
  in
  let dem_dem_percentages =
    List.map (fun lst -> float_of_string (List.nth lst 3)) dem_wins
  in
  let rep_rep_percentages =
    List.map (fun lst -> float_of_string (List.nth lst 2)) rep_wins
  in
  let rep_dem_percentages =
    List.map (fun lst -> float_of_string (List.nth lst 3)) rep_wins
  in

  let dem_frac =
    float_of_int (List.length dem_wins) /. float_of_int (List.length state_data)
  in
  let rep_frac =
    float_of_int (List.length rep_wins) /. float_of_int (List.length state_data)
  in

  let likelihood_dem =
    (dem_frac *. geometric_mean dem_dem_percentages)
    +. (rep_frac *. geometric_mean rep_dem_percentages)
  in
  let likelihood_rep =
    (dem_frac *. geometric_mean dem_rep_percentages)
    +. (rep_frac *. geometric_mean rep_rep_percentages)
  in
  (* Intuition: if net percent change (get_reg_total 10) < 0, reg_factor < 1. If
     net percent change > 0, reg_factor > 1. Only have to change one
     computation. *)
  let net_change = get_reg_total randomness in
  (* let () = print_float net_change in let () = print_endline "" in *)
  let total_likelihood = likelihood_dem +. likelihood_rep in
  let multiply_factor = (100. +. net_change) /. 100. in
  ( likelihood_dem /. total_likelihood,
    multiply_factor *. likelihood_rep /. total_likelihood )

(* Logistic Regression *)
(* Acknowledgement: The following code for the logistic regression algorithm was
   written with the guidance of Github Copilot.*)
(* Citation: 'Give ideas for how to implement logistic regression natively in
   OCaml.', GitHub Copilot, OpenAI/Microsoft, 15 May 2024,
   https://github.com/features/copilot. *)

let sigmoid z = 1. /. (1. +. exp (-.z))

let hypothesis theta x =
  sigmoid (List.fold_left2 (fun acc t x -> acc +. (t *. x)) 0.0 theta x)

let gradient theta xs ys =
  let m = float_of_int (List.length ys) in
  let n = List.length theta in
  let grad = List.init n (fun _ -> 0.0) in
  let grad =
    List.fold_left2
      (fun grad x y ->
        let h = hypothesis theta x in
        List.mapi (fun j g -> g +. ((h -. y) *. List.nth x j)) grad)
      grad xs ys
  in
  List.map (fun g -> g /. m) grad

let rec gradient_descent theta xs ys alpha num_iters =
  let alpha = abs_float alpha in
  if num_iters = 0 then theta
  else
    let grad = gradient theta xs ys in
    let theta = List.mapi (fun j t -> t -. (alpha *. List.nth grad j)) theta in
    gradient_descent theta xs ys alpha (num_iters - 1)

let logistic_regression_helper data alpha iters =
  let () = Random.self_init () in
  let features =
    List.map (fun lst -> [ List.nth lst 2; List.nth lst 3 ]) data
  in
  let features_float =
    List.map (fun lst -> List.map float_of_string lst) features
  in
  let features_float_random =
    List.map
      (fun lst -> List.map (fun x -> x +. (Random.float 1. -. 0.5)) lst)
      features_float
  in
  let labels =
    List.map
      (fun lst -> if List.hd (List.rev lst) = "rep" then -1. else 1.)
      data
  in
  let theta_0 = [ 0.0; 0.0 ] in
  let xs = features_float_random in
  let ys = labels in
  let theta = gradient_descent theta_0 xs ys alpha iters in
  let recent = List.hd (List.rev features) in
  let recent_float = List.map float_of_string recent in
  let prediction = hypothesis theta recent_float in
  prediction

let logistic_regression data =
  let alpha = 0.01 in
  let iters = 1000 in
  let pred = logistic_regression_helper data alpha iters in
  (pred, 1. -. pred)
