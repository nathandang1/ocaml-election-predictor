module S = struct
  type c =
    | Biden
    | Trump

  type t = {
    name : string;
    votes : int;
    pop : int;
    pref_can : c;
    pref_percent : float;
  }

  type d = (c * float) list

  let outcome state (data : d) =
    let biden_val = List.assoc Biden data in
    let trump_val = List.assoc Trump data in
    let biden_boost =
      if state.pref_can = Biden then state.pref_percent /. 5.
      else -1. *. state.pref_percent /. 20.
    in
    let trump_boost =
      if state.pref_can = Trump then state.pref_percent /. 5.
      else -1. *. state.pref_percent /. 20.
    in
    [ (Biden, biden_val +. biden_boost); (Trump, trump_val +. trump_boost) ]

  let from_csv = false

  let of_data data =
    match List.hd data with
    | nm :: vts :: p :: c :: c_per :: _ ->
        {
          name = nm;
          votes = int_of_string vts;
          pop = int_of_string p;
          pref_can = (if c = "Donald Trump" then Trump else Biden);
          pref_percent = float_of_string c_per;
        }
    | _ -> failwith "invalid data"
end
