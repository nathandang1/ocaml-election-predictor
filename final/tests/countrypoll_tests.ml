open Final
open OUnit2 
(** these does not reflect real data *)
let candidate1 =
  { Candidate.name = "Donald Trump"; Candidate.party = "Republican" }
let candidate2 =
  { Candidate.name = "Joe Biden"; Candidate.party = "Democratic" }
let state1 = {
    Statepoll.name = "New York";
    Statepoll.preferred_candidate = candidate1;
    Statepoll.preferred_margin = 3.2;
    Statepoll.num_votes = 28;
    Statepoll.population = 196800000;
}
let state2 = {
    Statepoll.name = "Florida";
    Statepoll.preferred_candidate = candidate2;
    Statepoll.preferred_margin = 1.6;
    Statepoll.num_votes = 14;
    Statepoll.population = 10000;
}
let state3 = {
    Statepoll.name = "California";
    Statepoll.preferred_candidate = candidate1;
    Statepoll.preferred_margin = 10.0;
    Statepoll.num_votes = 50;
    Statepoll.population = 102938480101;
}
let state4 = {
    Statepoll.name = "Pennsylvania";
    Statepoll.preferred_candidate = candidate2;
    Statepoll.preferred_margin = 4.6;
    Statepoll.num_votes = 10;
    Statepoll.population = 10000000;
}
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
(** after this passes, we can use the creation method for the rest of the countries! *)
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
    assert_equal (Countrypoll.get_electoral_votes country2) None)
]
let mutability_tests = [
  "changing the state populations affect the total population" >:: (fun _ ->
    let () = 
    List.iter (fun (x : Statepoll.state)-> x.population <- x.population + 1) 
    (Countrypoll.get_states country1)
  in 
    assert_equal (Countrypoll.get_population country1) 196810002); 
  "removing a state affects the total population" >:: (fun _ -> 
    let () = 
    Countrypoll.remove_state country1 state1 in 
    assert_equal (Countrypoll.get_population country1) 10001); 
  "removing a state means the list returned by the get_state function changes" >:: (fun _ -> 
    assert_equal (Countrypoll.get_states country1) [state2]); 
  "adding a state affects the total population" >:: (fun _ -> 
    let () = Countrypoll.add_state country1 state3 in 
    assert_equal (Countrypoll.get_population country1) 102938490101); 
  "adding a state means the list returned by the get_state function changes" >:: (fun _ ->
    assert_equal (Countrypoll.get_states country1) [state2; state3])
]

let corner_case_tests = [
    
]
let countrypoll_tests = "full test suite" >::: List.flatten [
  test_equals; 
  test_contains; 
  getter_tests; 
  mutability_tests;
  ]
let run_countrypoll_tests = run_test_tt_main countrypoll_tests