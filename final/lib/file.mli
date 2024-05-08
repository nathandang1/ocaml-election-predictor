(** A file is a CSV representing collection of some data. *)
module type F = sig
  type d
  (** [d] represents the type of data stored in the file. *)

  type t
  (** [t] represents a file storing data of type [d]. *)

  val of_path : string -> t
  (** [of_path path] is the file at location [path]. *)

  val data : t -> d
  (** [data file] is the data stored in [file]. *)
end

(** An extractable is a data type that can be extracted from a CSV file. *)
module type ExtractableType = sig
  type t
  (** [t] represents the type of data that can be extracted. *)

  val extract : Csv.t -> t
  (** [extract csv] is the data extracted from [csv]. *)
end

(** Functor building an implementation of a file given an extractable type. *)
module Make (Extr : ExtractableType) : F with type d = Extr.t
