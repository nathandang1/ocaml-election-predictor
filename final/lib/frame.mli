type t
(** [t] represents a dataframe. Features adapted from:
    pandas.pydata.org/docs/reference/frame.html *)

val labels : t -> string list
(** [labels df] are the labels for the columns of [df]. *)

val indices : t -> string list
(** [index df] are the indices for the rows of [df]. *)

val data : t -> string list list
(** [data df] is a nested-list representation (with each list representing a
    row) for the data contained in [df]. *)

val get_col : t -> string -> string list
(** [get_col df lbl] is the data for the column of [df] with label [lbl]. *)

val get_row : t -> string -> string list
(** [get_row df idx] is the data for the row of [df] with index [idx]. *)

val of_csv : Csv.t -> t
(** [of_csv csv] is the dataframe for [csv], with an initial index of ("0", "1",
    "2", ...). Requires that the first row of [csv] contains its column headers.
    Raises [InvalidCSV] if the CSV cannot be converted into a dataframe. *)

val to_csv : t -> Csv.t
(** [to_csv df] is the CSV corresponding to [df]. *)
