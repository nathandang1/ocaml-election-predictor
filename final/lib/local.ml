type t = string * string

let shadow local global = (local, global)

let path (local, global) =
  try
    ignore (Csv.load local);
    local
  with _ -> global
