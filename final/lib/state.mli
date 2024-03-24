type t = {name: string; votes: int; pop: int}
(** [t] contains the basic instances of a State *)

type d 
(** [d] is the state-specific data. *)

val outcome : d -> float list
(** [outcome d] is each candidate's probability of winning given some data [d]. *)