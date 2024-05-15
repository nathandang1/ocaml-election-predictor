open Final
open OUnit2 
(* these does not reflect real data *)
let candidate1 =
  { Candidate.name = "Donald Trump"; Candidate.party = "Republican" }
let candidate2 =
  { Candidate.name = "Joe Biden"; Candidate.party = "Democratic" }

let state1_attributes = ["New York"; "Donald Trump"; "Republican"; 
  "3.2"; "28"; "196800000"]
let state1 = Statepoll.create_state state1_attributes
let state2_attributes = ["Florida"; "Joe Biden"; "Democratic"; 
"1.6"; "14"; "10000"]
let state2 = Statepoll.create_state state2_attributes
let state3_attributes = ["California"; "Donald Trump"; "Republican"; 
"10.0"; "50"; "102938480101"]
let state3 = Statepoll.create_state state3_attributes
let state4_attributes = ["Pennsylvania"; "Joe Biden"; "Democratic"; 
"4.6"; "10"; "10000000"]
let state4 = Statepoll.create_state state4_attributes
let state_list_1 = [state1; state2]
let state_list_2 = [state3; state4]
let country1 = {
  Countrypoll.electoral_college = true; 
  Countrypoll.name = "NY and FL"; 
  Countrypoll.states = state_list_1
}
let test_create = [
  "only test" >:: (fun _ -> assert_equal (Countrypoll.create_country state_list_1 true "NY and FL") country1)
]
(* after this passes, we can use the creation method for the rest of the countries! *)
let country2 = Countrypoll.create_country state_list_2 false "CA and PA"
let country1_dupe = Countrypoll.create_country state_list_1 true "NY and FL" 
let country1_dupe2 = Countrypoll.create_country state_list_1 true "NY and FL" 
let country1_rev = Countrypoll.create_country (List.rev state_list_1) true "NY and FL"
let test_equals = [
  "reflexive test" >:: (fun _ -> assert_equal (Countrypoll.equals country1 country1) true); 

  "symmetric test" >:: (fun _ -> assert_equal (Countrypoll.equals country1 country1_dupe) 
  (Countrypoll.equals country1 country2)); 

  "transitivity test" >:: (fun _ -> assert_equal 
  ((Countrypoll.equals country1 country1_dupe) 
  && (Countrypoll.equals country1_dupe country1_dupe2) 
  && (Countrypoll.equals country1 country1_dupe2)) true); 

  "false test" >:: (fun _ -> assert_equal (Countrypoll.equals country1 country2) false); 

  "corner case: the same list of states ordered differently are still equal" >::
  (fun _ -> assert_equal (Countrypoll.equals country1 country1_rev) true)
]
let test_contains = [
  "true test" >:: (fun _ -> assert_equal (Countrypoll.contains_state country1 state1) true); 
  "false test" >:: (fun _ -> assert_equal (Countrypoll.contains_state country1 state3) false)
]
let getter_tests = [
  "get_name" >:: (fun _ -> assert_equal (Countrypoll.get_name country1) "NY and FL"); 
  "get_states" >:: (fun _ -> assert_equal (Countrypoll.get_states country1) state_list_1); 
  "electoral_college_enabled (true)" >:: (fun _ -> 
    assert_equal (Countrypoll.electoral_college_enabled country1) true); 
  "electoral_college_enabled (false)" >:: (fun _ ->
    assert_equal (Countrypoll.electoral_college_enabled country2) false); 
  "get_population" >:: (fun _ -> 
    assert_equal (Countrypoll.get_population country1) 196810000); 
  "get_electoral_votes (Some case)" >:: (fun _ -> 
    assert_equal (Countrypoll.get_electoral_votes country1) (Some 42)); 
  "get_electoral_votes (None case)" >:: (fun _ -> 
    assert_equal (Countrypoll.get_electoral_votes country2) None); 
  "get_state_names" >:: (fun _ -> 
    assert_equal (Countrypoll.get_state_names country1) ["Florida"; "New York"])
  ]
let mutability_tests = [
  "changing the state populations affect the total population" >:: (fun _ ->
    let original_population = Countrypoll.get_population country1 
  in 
    let () = 
    List.iter (fun (x : Statepoll.state)-> x.population <- x.population + 1) 
    (Countrypoll.get_states country1)
  in 
    let new_population = Countrypoll.get_population country1
  in
    assert_equal (original_population = new_population) false); 

  "removing a state affects the total population" >:: (fun _ -> 
    let original_population = Countrypoll.get_population country1 
  in 
    let () = Countrypoll.remove_state country1 state1 
  in 
    let new_population = Countrypoll.get_population country1
in 
    assert_equal (new_population = original_population) false); 

  "removing a state means the list returned by the get_state function changes" >:: (fun _ -> 
    let original_states = Countrypoll.get_states country1 
  in 
    let () = Countrypoll.remove_state country1 state2 
in 
  let new_states = Countrypoll.get_states country1
in 
    assert_equal ((List.length original_states) = (List.length new_states)) false); 

  "adding a state affects the total population" >:: (fun _ -> 
    let original_popuation = Countrypoll.get_population country1
  in 
    let () = Countrypoll.add_state country1 state3 
  in 
    let new_population = Countrypoll.get_population country1
  in 
    assert_equal (new_population = original_popuation) false); 

  "adding a state means the list returned by the get_state function changes" >:: (fun _ ->
    let original_list = Countrypoll.get_states country1 
  in 
    let () = Countrypoll.add_state country1 state1
  in 
    let new_list = Countrypoll.get_states country1 
  in 
    assert_equal ((List.length new_list) = (List.length original_list)) false); 

  "trying to remove a state that isn't in the country does nothing" >:: (fun _ ->
    let original_states_length = List.length (Countrypoll.get_states country2) 
  in 
    let () = Countrypoll.remove_state country2 state1 
  in 
    let new_states_length = List.length (Countrypoll.get_states country2)
  in 
    assert_equal new_states_length original_states_length
    ); 
  "adding a state that is already in the country does nothing" >:: (fun _ -> 
    let original_states_length = List.length (Countrypoll.get_states country2) 
  in 
    let () = Countrypoll.add_state country2 state3
  in 
    let new_states_length = List.length (Countrypoll.get_states country2)
  in 
    assert_equal new_states_length original_states_length
    )
]
let countrypoll_tests = "full test suite" >::: List.flatten [
  test_equals; 
  test_contains; 
  getter_tests; 
  mutability_tests;
]
let run_countrypoll_tests = run_test_tt_main countrypoll_tests