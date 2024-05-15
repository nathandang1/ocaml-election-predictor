val open_metadata : string -> Csv.t
(**[open_metadata path] returns the Csv.t format of the csv file who's location
   is at [path]. *)

val meta_labels : string list
(**[meta_labels] is a list of strings of the labels from the csv file. *)

val ones : int -> float list
(** [ones n] creates a float list of [n] ones. *)

val issues_weights : 'a list -> float list
(** [issues_weights issues] applies a random weight between 0.0 and 1.0 to each
    "issue" in [issues]. *)

val element_product : float list -> float list -> float list
(** [element_product lst1 lst2] computes the element-wise product of [lst1] and
    [lst2]. *)
val sum_float_list : float list -> float
(** [sum_float_list lst] computes the sum of [lst]. *)

val softmax : float list -> float list
(** [softmax lst] applies the softmax function - (e^z_i)/sum_over_i(e^z_i) - to
    [lst] element-wise. *)

    val concatenate_lists : 'a list -> 'b list -> ('a * 'b) list
    (** [concatenate_lists lst1 lst2] concatenates the value of [lst1] and [lst2] into a list of tuples with each element being a concatenation of elements from [lst1] and [lst2] *)

