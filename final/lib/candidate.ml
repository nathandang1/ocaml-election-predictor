module C = struct
  type t = {
    name : string;
    party : string;
  }

  let create (nm, pty) = { name = nm; party = pty }
  let name_of c = c.name
  let party_of c = c.party
  let from_csv = false

  let of_data data =
    match List.hd data with
    | nm :: pty :: _ -> { name = nm; party = pty }
    | _ -> failwith "invalid data"
end
