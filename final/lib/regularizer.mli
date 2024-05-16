val open_metadata : string -> Csv.t
(**[open_metadata path] returns the Csv.t format of the csv file whose location
   is at [path]. *)

val ones : int -> float list
(** [ones n] creates a list of [n] ones. *)

val issues_weights : 'a list -> float list
(** [issues_weights issues] applies a random weight between 0.0 and 1.0 to each
    "issue" in [issues]. *)

val element_product : float list -> float list -> float list
(** [element_product lst1 lst2] returns the element-wise product of [lst1] and
    [lst2]. Requires: [lst1] and [lst2] have the same length. *)

val sum_float_list : float list -> float
(** [sum_float_list lst] computes the sum of [lst]. *)

val softmax : float list -> float list
(** [softmax lst] applies the softmax function - (e^z_i)/sum_over_i(e^z_i) - to
    [lst] element-wise. *)

val concatenate_lists : 'a list -> 'b list -> ('a * 'b) list
(** [concatenate_lists lst1 lst2] returns [lst1] and [lst2] concatenated
    element-wise into a list of tuples with each tuple taken pair-wise from
    [lst1] and [lst2]. Requires: [lst1] and [lst2] have the same length. *)

val do_computations : unit -> (string * float) list * (float * float)
(** [do_computations ()] assigns weights for each issue in 'metadata' and
    computes how these weights affect each candidate. Returns:
    [(issue_weights, (trump_weight, biden_weight))] *)

val diff : float -> float -> float
(** [diff val1 val2] computes the unsigned percent difference between [val1] and
    [val2]. Requires: [val1 +. val2 != 0] *)
