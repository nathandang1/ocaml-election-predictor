module type F = sig
  type d
  type t

  val of_path : string -> t
  val data : t -> d
end

module type ExtractableType = sig
  type t

  val extract : Csv.t -> t
end

module Make (Extr : ExtractableType) : F with type d = Extr.t = struct
  type t = string
  type d = Extr.t

  let of_path (path : string) : t = path
  let data file = file |> Csv.load |> Csv.square |> Extr.extract
end
