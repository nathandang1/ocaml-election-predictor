type state = {
  name : string;
  abbreviation : string;
  mutable preferred_candidate : string;
  mutable preferred_margin : float;
  mutable num_votes : int;
  mutable population : int;
}

exception ImproperList of string

let create_state lst =
  try
    match lst with
    | [ nam; abbr; votes; pop; pref_can; pref_percent ] ->
        if
          int_of_string (String.trim votes) <= 0
          || int_of_string (String.trim pop) <= 0
          || float_of_string (String.trim pref_percent) <= 0.0
          || float_of_string (String.trim pref_percent) >= 100.0
        then raise (ImproperList "Data Value Specfication Violated")
        else
          {
            name = String.trim nam;
            abbreviation = String.trim abbr;
            num_votes = int_of_string (String.trim votes);
            population = int_of_string (String.trim pop);
            preferred_candidate = String.trim pref_can;
            preferred_margin = float_of_string (String.trim pref_percent);
          }
    | _ -> raise (ImproperList "List Length Specification Violated")
  with Failure _ ->
    raise (ImproperList "Data Formatting Specification Violated")

let equals (state1 : state) state2 = state1.name = state2.name
let get_name st = st.name
let get_preferred_candidate_name st = st.preferred_candidate
let get_abbreviation st = st.abbreviation
let get_preferred_margin st = st.preferred_margin
let get_num_votes st = st.num_votes
let get_population st = st.population
let set_preferred_candidate st can = st.preferred_candidate <- can
let set_preferred_margin st marg = st.preferred_margin <- marg
let set_num_votes st vot = st.num_votes <- vot
let set_population st pop = st.population <- pop

let export_state_to_csv st =
  let attributes =
    [ "name"; "abbr"; "votes"; "pop"; "pref_can"; "pref_percent" ]
  in
  let state_data =
    [
      get_name st;
      get_abbreviation st;
      string_of_int (get_num_votes st);
      string_of_int (get_population st);
      get_preferred_candidate_name st;
      string_of_float (get_preferred_margin st);
    ]
  in
  let data = [ attributes; state_data ] in
  Csv.transpose (Csv.transpose data)
