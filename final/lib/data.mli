(** An [ExtractableType] is a value whose data can be extracted from a .csv
    file. *)
module type ExtractableType = sig
  type t
  (** [t] represenets a type that can be constructed using data from a csv. *)

  val of_csv : Csv.t -> t
  (** [of_csv csv] is the value extracted from the CSV file [csv]. *)
end

(** A [File] is a .csv file containing data for an [ExtractableType]. *)
module type File = sig
  type t
  (** [t] represents a .csv file. *)

  type d
  (** [d] represents a type that can be extracted from a .csv file. *)

  val of_path : string -> t
  (** [of_path path] is the file with the file path [path]. *)

  val extract : t -> d
  (** [extract file] is the value of type [d] extracted from [file]. *)
end

(** [Make(Ext)] are the files from which values of type [Ext] can be extracted. *)
module Make (Ext : ExtractableType) : File with type d = Ext.t
