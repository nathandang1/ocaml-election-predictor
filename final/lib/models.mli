val geometric_mean : float list -> float
(** [geometric_mean lst] computes the geometric mean of [lst] *)

val get_reg_total : int -> float

val naive_bayes_randomized : string list list -> float * float
(** [naive_bayes_randomized data] takes in
    [[year; state; republican votes; democrat votes; winner] ...] *)

(* Sigmoid function *)
val sigmoid : float -> float

(* Hypothesis function *)
val hypothesis : float list -> float list -> float
val interpret_hypo : float -> int

(* Gradient of the cost function *)
val gradient : float list -> float list list -> float list -> float list

(* Gradient descent algorithm *)
val gradient_descent :
  float list -> float list list -> float list -> float -> int -> float list

val logistic_regression_helper : string list list -> float -> int -> float
val logistic_regression : string list list -> float -> float -> float * float
