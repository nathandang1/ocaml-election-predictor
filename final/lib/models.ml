(** [geometric_mean lst] computes the geometric mean of [lst] *)
let geometric_mean lst =
  let product = List.fold_left ( *. ) 1. lst in
  if List.length lst > 0 then exp (log product /. float_of_int (List.length lst))
  else 1.

(** [naive_bayes_randomized data] takes in
    [[year; state; republican votes; democrat votes; winner] ...] *)
let naive_bayes_randomized state_data =
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

  let likelihood_dem =
    geometric_mean dem_dem_percentages *. geometric_mean rep_dem_percentages
  in
  let likelihood_rep =
    geometric_mean dem_rep_percentages *. geometric_mean rep_rep_percentages
  in

  (likelihood_dem, likelihood_rep)

(* logistic regression *)
(* Sigmoid function *)
let sigmoid z = 1. /. (1. +. exp (-.z))

(* Hypothesis function *)
let hypothesis theta x =
  sigmoid (List.fold_left2 (fun acc t x -> acc +. (t *. x)) 0.0 theta x)

let interpret_hypo res =
  (* -1 denotes republican winner, 1 denotes democrat winner*)
  if res < 0.5 then -1 else 1

(* Gradient of the cost function *)
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

(* Gradient descent algorithm *)
let rec gradient_descent theta xs ys alpha num_iters =
  if num_iters = 0 then theta
  else
    let grad = gradient theta xs ys in
    let theta = List.mapi (fun j t -> t -. (alpha *. List.nth grad j)) theta in
    gradient_descent theta xs ys alpha (num_iters - 1)

let data = List.tl (Csv.load "bin/florida.csv")

let logistic_regression data alpha iters =
  let features =
    List.map (fun lst -> [ List.nth lst 2; List.nth lst 3 ]) data
  in
  let features_float =
    List.map (fun lst -> List.map float_of_string lst) features
  in
  let labels =
    List.map
      (fun lst -> if List.hd (List.rev lst) = "rep" then -1. else 1.)
      data
  in
  let theta_0 = [ 0.0; 0.0 ] in
  let xs = features_float in
  let ys = labels in
  let theta = gradient_descent theta_0 xs ys alpha iters in
  let recent = List.hd (List.rev features) in
  let recent_float = List.map float_of_string recent in
  let prediction = hypothesis theta recent_float in
  prediction
