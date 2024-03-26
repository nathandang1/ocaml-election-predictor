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

type d = c * float list

let outcome state data =
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
