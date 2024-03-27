(** An S is a state-level electoral subdivision of an electoral map. *)
module S : sig
  type c
  (** [c] is a variant of possible candidates *)

  type t
  (** [t] contains the basic instances of a State *)
  (* = {name: string; votes: int; pop: int ; pref_can : c ; percent_win :
     float} *)

  type d
  (** [d] is the state-specific data. *)

  val outcome : t -> d -> (c * float) list
  (** [outcome d] is each candidate's probability of winning given some data
      [d]. *)
end
