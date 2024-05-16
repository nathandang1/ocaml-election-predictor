open Final
open OUnit2
let state = {
    Statepoll.name = "New York";
    Statepoll.abbreviation = "ny"; 
    Statepoll.preferred_candidate = "Biden";
    Statepoll.preferred_margin = 3.2;
    Statepoll.num_votes = 28;
    Statepoll.population = 196800000;
}
(* Getter Method Tests*)
let state_attributes = ["New York"; "ny"; "28"; "Biden"; "3.2"]
let create_test = [
  "valid creation" >:: (fun _ -> assert_equal (Statepoll.create_state state_attributes) state);
  "error is thrown (length of list is faulty)" >:: (fun _ -> 
    assert_raises 
  (Statepoll.ImproperList "")
  (fun () -> Statepoll.create_state ("hi" :: ["New York"; "Biden"; "Democratic"; "3.2"; "28"; "196800000"])));
  "error is thrown (one of the attributes is wrong)" >:: (fun _ -> 
    assert_raises 
    (Statepoll.ImproperList "")
    (fun () -> Statepoll.create_state (["New York"; "ny"; "Hi"; "Biden"; "whats good Rowan"])); 
  )
]
let state_copy = state
let state_copy_2 = state_copy 
let state2 = {
  Statepoll.name = "California";
  Statepoll.abbreviation = "ca"; 
  Statepoll.preferred_candidate = "Trump";
  Statepoll.preferred_margin = 3.2;
  Statepoll.num_votes = 28;
  Statepoll.population = 196800000;
}
let test_equals = [ 
  "reflexive test" >:: (fun _ -> assert_equal (Statepoll.equals state state) true); 
  "symmetric test" >:: (fun _ -> assert_equal (Statepoll.equals state state_copy) 
  (Statepoll.equals state_copy state)); 
  "transitive test" >:: (fun _ -> assert_equal ((Statepoll.equals state state_copy) 
  && (Statepoll.equals state_copy state_copy_2))
  (Statepoll.equals state state_copy_2)); 
  "false test" >:: (fun _ -> assert_equal (Statepoll.equals state state2) false)
]
let getter_method_tests = [
  "get_name" >:: (fun _ -> assert_equal (Statepoll.get_name state) "New York"); 
  "get_preferred_candidate_name" >:: (fun _ -> assert_equal (Statepoll.get_preferred_candidate_name state) "NY"); 
  "get_abbreviation" >:: (fun _ -> assert_equal (Statepoll.get_abbreviation state) "Democratic"); 
  "get_preferred-margin" >:: (fun _ -> assert_equal (Statepoll.get_preferred_margin state) 3.2); 
  "get_num_votes" >:: (fun _ -> assert_equal (Statepoll.get_num_votes state) 28); 
  "get_population" >:: (fun _ -> assert_equal (Statepoll.get_population state) 196800000)
]
let setter_method_tests = [
  "set_preferred_candidate" >:: (fun _ -> 
    let () = Statepoll.set_preferred_candidate state "Trump" 
  in
    assert_equal (Statepoll.get_preferred_candidate_name state) "Trump"); 
  "set_preferred_margin" >:: (fun _ -> 
    let original_margin = Statepoll.get_preferred_margin state
  in 
    let () = Statepoll.set_preferred_margin state (original_margin +. 1.0 )
  in 
    assert_equal (Statepoll.get_preferred_margin state = original_margin ) false); 
  "set_num_votes" >:: (fun _ -> 
    let original_votes = Statepoll.get_num_votes state
  in 
    let () = Statepoll.set_num_votes state (original_votes + 1) 
  in 
    assert_equal ((Statepoll.get_num_votes state) = original_votes) false); 
  "set_population" >:: (fun _ ->
    let original_population = Statepoll.get_population state
  in 
  let () = Statepoll.set_population state (original_population + 1) 
  in
  assert_equal ((Statepoll.get_population state) = original_population) false)
]
let statepoll_tests = "full test suite" >::: List.flatten [
  create_test; 
  test_equals; 
  getter_method_tests; 
  setter_method_tests
  ]
let run_statepoll_tests = run_test_tt_main statepoll_tests