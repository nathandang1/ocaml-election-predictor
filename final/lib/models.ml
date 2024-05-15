open Owl

(* Function to load your data - replace with your actual data loading methods *)
let load_data () =
  (* Simulate some data *)
  let x = Mat.uniform 100 10 in
  (* let x = Mat.create 100 1 1. in *)
  (* 100 samples, 10 features each *)
  (* Generate binary labels, 1 or 0 based on a 0.5 threshold *)
  (* let y = Mat.map (fun z -> if z < 0.5 then 0. else 1.) (Mat.uniform 100 1)
     in (x, y) *)
  let zeros = Mat.zeros 50 1 in
  let ones = Mat.create 50 1 1. in
  (* y = [0..(x50)...1..(x50)..] *)
  let y = Mat.concatenate ~axis:0 [| zeros; ones |] in
  (x, y)

(* Sigmoid function *)
let sigmoid x = Mat.map (fun z -> 1. /. (1. +. exp (-.z))) x

(* Logistic regression function using gradient descent *)
let logistic_regression x y =
  let alpha = 0.01 in
  (* Learning rate *)
  let iterations = 1000 in
  (* Number of iterations *)
  let n_samples = float_of_int (Mat.row_num x) in
  let n_features = Mat.col_num x in

  (* Initialize weights and bias *)
  let w = Mat.zeros n_features 1 in
  (* Notice the dimensions [n_features, 1] *)
  let b = ref 0. in

  (* Gradient descent loop *)
  for _ = 1 to iterations do
    let z = Mat.((x *@ w) +$ !b) in
    let predictions = sigmoid z in

    (* Gradient of the loss w.r.t weights *)
    let dw =
      Mat.(div (dot (transpose x) (sub predictions y)) (create 1 1 n_samples))
    in
    let db = Mat.(sum' (sub predictions y) /. n_samples) in

    (* Update rules *)
    Mat.(w -= (dw *$ alpha));
    b := !b -. (alpha *. db)
  done;

  (w, !b)

(* Main function *)
let run () =
  let x, y = load_data () in
  let weights, bias = logistic_regression x y in
  Printf.printf "Learned weights:\n";
  Mat.print weights;
  Printf.printf "Learned bias: %f\n" bias

(** [geometric_mean lst] computes the geometric mean of [lst]. *)
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
