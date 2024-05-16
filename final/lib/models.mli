(* Acknowledgement: The documentation for some of these functions were written
   by GitHub CoPilot.*)
(* Citation: 'Develop Succinct OCaml style documentation for these functions.',
   GitHub Copilot, OpenAI/Microsoft, 15 May 2024,
   https://github.com/features/copilot. *)

val geometric_mean : float list -> float
(** [geometric_mean lst] computes the geometric mean of [lst]. *)

val get_reg_total : int -> float
(** [get_reg_total iters] uses the Regularizer module to perform randomization
    [iters] number of times. Returns a summed percentage change across these
    iterations. *)

val naive_bayes_randomized : string list list -> int -> float * float
(** [naive_bayes_randomized data randomness] takes in [data] with the following
    format:
    [[year; state; republican votes; democrat votes; winner]; ..(more rows)..]
    and outputs [(dem, rep)] corresponding to the randomized probabilities that
    a Democrat/Republican candidate wins the state. The factor of randomization
    is determined by [randomness]. Requires: all values in the [state] column to
    be uniform. *)

val sigmoid : float -> float
(** [sigmoid z] applies the sigmoid function to argument [z]. *)

val hypothesis : float list -> float list -> float
(** [hypothesis theta x] computes the hypothesis value for logistic regression.*)

val gradient : float list -> float list list -> float list -> float list
(** [gradient theta xs ys] computes the gradient of the cost function with
    respect to the parameters theta, given the input features xs and the
    corresponding output values ys. *)

val gradient_descent :
  float list -> float list list -> float list -> float -> int -> float list
(** [gradient_descent theta xs ys alpha num_iters] is a recursive function that
    performs gradient descent optimization to find the optimal values of theta
    for a given set of input features [xs] and corresponding output values [ys]. *)

val logistic_regression_helper : string list list -> float -> int -> float
(** [logistic_regression_helper data alpha iters] is a helper function that
    performs logistic regression on the given data. It takes in a list of data
    points [data], a learning rate [alpha], and the number of iterations
    [iters]. The function extracts the features from the data points, converts
    them to float values, and assigns labels based on the last element of each
    data point. If the last element is "rep", the label is -1.0, otherwise it is
    1.0. The function returns the extracted features and labels as a tuple. *)

val logistic_regression : string list list -> float * float
(** [logistic_regression data r_prior d_prior] is a function that performs
    logistic regression on the given data. It calculates the probability of a
    binary outcome using the logistic function and returns the predicted
    probabilities for two classes based on the prior probabilities [r_prior] and
    [d_prior].*)
