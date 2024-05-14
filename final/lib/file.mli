(** An ExtractableType is a type of value that can be constructed using the data
    extracted from either a .txt or a .csv file. *)
module type ExtractableType = sig
  type t
  (** [t] represents the type that can be constructed using data from a file. *)

  val from_csv : bool
  (** [from_csv] indicates whether [t] can be constructed from a .csv file. If
      [from_csv] is false, then [t] should be constructable from a .txt file. *)

  val of_data : string list list -> t
  (** [of_data data] is the result when a nested list of strings is converted
      into a value of type [t]. As a precondition, [of_data] can assume that
      [data] contains a single list containing the rows of a .txt file if
      [from_csv] is false, or that [data] contains the columns of a .csv file if
      [from_csv] is true. *)
end

(** An F is a .txt or .csv file from which a certain data type can be extracted. *)
module type F = sig
  type t
  (** [f] represents a .txt or .csv file. *)

  type d
  (** [d] represents the data type that can be extracted from a file. *)

  val of_name : string -> t
  (** [of_name name] is the file with the name [name]. *)

  val extract : t -> d
  (** [extract file] is the value of type [d] extracted from the file [file]. *)
end

(** [Make(Extr)] is the files from which values of type [Extr] can be extracted. *)
module Make (Extr : ExtractableType) : F with type d = Extr.t
