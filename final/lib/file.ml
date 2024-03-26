open Batteries

module type ExtractableType = sig
  type t

  val from_csv : bool
  val of_data : string list list -> t
end

module type F = sig
  type t
  type d

  val of_name : string -> t
  val extract : t -> d
end

module Make (Extr : ExtractableType) : F with type d = Extr.t = struct
  type t = string
  (** AF: The string [f] represents the file with the name [f]. RI: [f] must be
      the name of a file that actually exists. *)

  type d = Extr.t

  let of_name (name : string) : t = name
  let txt_data file = [ file |> BatFile.lines_of |> BatList.of_enum ]
  let csv_data file : string list list = file |> Csv.load |> Csv.transpose

  let extract file =
    file |> (if Extr.from_csv then csv_data else txt_data) |> Extr.of_data
end
