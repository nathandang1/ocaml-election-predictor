open Candidate

module S = struct
  type c = (string * C.t) list

  type t = {
    name : string;
    votes : int;
    pop : int;
    pref_can : string;
    pref_percent : float;
  }

  type d = (string * float) list

  let outcome state (data : d) =
    let pref_can_curr = List.assoc state.pref_can data in
    let pref_can_new = pref_can_curr +. state.pref_percent in
    (state.pref_can, pref_can_new) :: List.remove_assoc state.pref_can data

  let name state = state.name
  let population state = state.pop
  let from_csv = false

  let of_data data =
    match List.hd data with
    | nm :: vts :: p :: c :: c_per :: _ ->
        {
          name = nm;
          votes = int_of_string vts;
          pop = int_of_string p;
          pref_can = (if c = "Donald Trump" then "Trump" else "Biden");
          pref_percent = float_of_string c_per;
        }
    | _ -> failwith "invalid data"
end
