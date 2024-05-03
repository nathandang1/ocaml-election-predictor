(* Import Owl Library *)
open Owl

(* Function to load your data - replace with your actual data loading methods *)
let load_data () =
  (* Simulate some data *)
  let x = Mat.uniform 100 10 in
  (* 100 samples, 10 features each *)
  (* Generate binary labels, 1 or 0 based on a 0.5 threshold *)
  (* let y = Mat.map (fun z -> if z < 0.5 then 0. else 1.) (Mat.uniform 100 1)
     in (x, y) *)
  let y = Mat.zeros 100 1 in

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
