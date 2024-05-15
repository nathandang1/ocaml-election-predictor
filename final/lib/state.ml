type t = {
  name : string;
  abbr : string;
  votes : int;
  pop : int;
  pref_can : string;
  pref_percent : float;
}

type d = (string * float) list

let outcome state (data : d) cand prior : d =
  let pref_can_new = List.assoc state.pref_can data +. state.pref_percent in
  (* incorporate "preferred candidate" info - will change this into other
     features *)
  let new_data =
    (state.pref_can, pref_can_new) :: List.remove_assoc state.pref_can data
  in
  let cand_curr = List.assoc cand new_data *. prior in
  let other_cand = fst (List.hd (List.remove_assoc cand new_data)) in
  let other_cand_curr = List.assoc other_cand new_data *. (1. -. prior) in
  [ (cand, cand_curr); (other_cand, other_cand_curr) ]
