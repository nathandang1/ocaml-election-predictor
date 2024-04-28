module type ExtractableType = sig
  type t

  val of_csv : Csv.t -> t
end

module type File = sig
  type t
  type d

  val of_path : string -> t
  val extract : t -> d
end

module Make (Ext : ExtractableType) : File with type d = Ext.t = struct
  type t = string
  (** AF: The string [f] represents the file with file path [f]. RI: [f] must be
      the path to a file that actually exists. *)

  type d = Ext.t

  let of_path path = path
  let extract file = file |> Csv.load |> Ext.of_csv
end
