(** A C is a candidate for an election. *)
module C : sig
  type t = {
    name : string;
    party : string;
  }
  (** [t] represents a candidate in an election. *)

  val create : string * string -> t
  (** [create (nm, pty)] creates a candidate with name [nm] and party [pty]. *)

  val name : t -> string
  (** [name_of c] is the name of candidate [c]. *)

  val party : t -> string
  (** [party_of c] is the political party of candidate [c]. *)

  val from_csv : bool
  (** [from_csv] indicates whether a candidate can be extracted from a .csv
      file, and is [false].*)

  val of_data : string list list -> t
  (** [of_data data] is the candidate created using information extracted from
      the nested string list [data]. Requires that [data] contains a single
      list, whose first element is the name of the candidate, and whose second
      element is the name of the candidate's political party. *)
end
