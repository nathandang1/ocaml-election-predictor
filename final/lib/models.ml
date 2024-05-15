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

(* let weak_learner x y = *)

(* Main function *)
let run () =
  let x, y = load_data () in
  let weights, bias = logistic_regression x y in
  Printf.printf "Learned weights:\n";
  Mat.print weights;
  Printf.printf "Learned bias: %f\n" bias

(* let outcome state (data : d) cand prior : d = let pref_can_new = List.assoc
   state.pref_can data +. state.pref_percent in (* incorporate "preferred
   candidate" info - will change this into other features *) let new_data =
   (state.pref_can, pref_can_new) :: List.remove_assoc state.pref_can data in
   let cand_curr = List.assoc cand new_data *. prior in let other_cand = fst
   (List.hd (List.remove_assoc cand new_data)) in let other_cand_curr =
   List.assoc other_cand new_data *. (1. -. prior) in [ (cand, cand_curr);
   (other_cand, other_cand_curr) ] *)

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

  let dem_rep_product =
    List.fold_left
      (fun acc lst -> acc *. float_of_string (List.nth lst 2))
      1. dem_wins
  in
  let dem_dem_product =
    List.fold_left
      (fun acc lst -> acc *. float_of_string (List.nth lst 3))
      1. dem_wins
  in
  let rep_rep_product =
    List.fold_left
      (fun acc lst -> acc *. float_of_string (List.nth lst 2))
      1. rep_wins
  in
  let rep_dem_product =
    List.fold_left
      (fun acc lst -> acc *. float_of_string (List.nth lst 3))
      1. rep_wins
  in
  [ dem_rep_product; dem_dem_product; rep_rep_product; rep_dem_product ]
