type c
(** [c] is a variant of possible candidates  *)
type t = {name: string; votes: int; pop: int ; pref_can : c ; percent_win : float}
(** [t] contains the basic instances of a State *)


type d 
(** [d] is the state-specific data. *)

val outcome : d -> float list
(** [outcome d] is each candidate's probability of winning given some data [d]. *)