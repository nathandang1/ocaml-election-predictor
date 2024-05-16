open Final
open OUnit2

(* these does not reflect real data *)
let candidate1 =
  { Candidate.name = "Donald Trump"; Candidate.party = "Republican" }

let candidate2 =
  { Candidate.name = "Joe Biden"; Candidate.party = "Democratic" }

let state1_attributes = [ "New York"; "ny"; "28"; "196800000"; "Trump"; "3.2" ]
let state1 = Statepoll.create_state state1_attributes
let state2_attributes = [ "Florida"; "fl"; "14"; "10000"; "Biden"; "1.6" ]
let state2 = Statepoll.create_state state2_attributes

let state3_attributes =
  [ "California"; "ca"; "50"; "102938480101"; "Trump"; "10.0" ]

let state3 = Statepoll.create_state state3_attributes

let state4_attributes =
  [ "Pennsylvania"; "pa"; "10"; "10000000"; "Bden"; "4.6" ]

let state4 = Statepoll.create_state state4_attributes
let state_list_1 = [ state1; state2 ]
let state_list_2 = [ state3; state4 ]
let state_list_3 = [ state1; state2; state3 ]

let country1 =
  {
    Countrypoll.electoral_college = true;
    Countrypoll.name = "NY and FL";
    Countrypoll.states = state_list_1;
  }

let test_create =
  [
    ( "only test" >:: fun _ ->
      assert_equal
        (Countrypoll.create_country state_list_1 true "NY and FL")
        country1 );
  ]

(* after this passes, we can use the creation method for the rest of the
   countries! *)
let country2 = Countrypoll.create_country state_list_2 false "CA and PA"
let country1_dupe = Countrypoll.create_country state_list_1 true "NY and FL"
let country1_dupe2 = Countrypoll.create_country state_list_1 true "NY and FL"
let country3 = Countrypoll.create_country state_list_3 true "Bing Bong"

let country1_rev =
  Countrypoll.create_country (List.rev state_list_1) true "NY and FL"

let test_equals =
  [
    ( "reflexive test" >:: fun _ ->
      assert_equal (Countrypoll.equals country1 country1) true );
    ( "symmetric test" >:: fun _ ->
      assert_equal
        (Countrypoll.equals country1 country1_dupe)
        (Countrypoll.equals country1_dupe country1) );
    ( "transitivity test" >:: fun _ ->
      assert_equal
        (Countrypoll.equals country1 country1_dupe
        && Countrypoll.equals country1_dupe country1_dupe2
        && Countrypoll.equals country1 country1_dupe2)
        true );
    ( "false test" >:: fun _ ->
      assert_equal (Countrypoll.equals country1 country2) false );
    ( "corner case: two countries with different number of states are not equal"
    >:: fun _ -> assert_equal (Countrypoll.equals country1 country3) false );
    ( "corner case: the same list of states ordered differently are still equal"
    >:: fun _ -> assert_equal (Countrypoll.equals country1 country1_rev) true );
  ]

let test_contains =
  [
    ( "true test" >:: fun _ ->
      assert_equal (Countrypoll.contains_state country1 state1) true );
    ( "false test" >:: fun _ ->
      assert_equal (Countrypoll.contains_state country1 state3) false );
  ]

let getter_tests =
  [
    ( "get_name" >:: fun _ ->
      assert_equal (Countrypoll.get_name country1) "NY and FL" );
    ( "get_states" >:: fun _ ->
      assert_equal (Countrypoll.get_states country1) state_list_1 );
    ( "electoral_college_enabled (true)" >:: fun _ ->
      assert_equal (Countrypoll.electoral_college_enabled country1) true );
    ( "electoral_college_enabled (false)" >:: fun _ ->
      assert_equal (Countrypoll.electoral_college_enabled country2) false );
    ( "get_population" >:: fun _ ->
      assert_equal (Countrypoll.get_population country1) 196810000 );
    ( "get_electoral_votes (Some case)" >:: fun _ ->
      assert_equal (Countrypoll.get_electoral_votes country1) (Some 42) );
    ( "get_electoral_votes (None case)" >:: fun _ ->
      assert_equal (Countrypoll.get_electoral_votes country2) None );
    ( "get_state_names" >:: fun _ ->
      assert_equal
        (Countrypoll.get_state_names country1)
        [ "Florida"; "New York" ] );
  ]

let mutability_tests =
  [
    ( "changing the state populations affect the total population" >:: fun _ ->
      let original_population = Countrypoll.get_population country1 in
      let () =
        List.iter
          (fun (x : Statepoll.state) -> x.population <- x.population + 1)
          (Countrypoll.get_states country1)
      in
      let new_population = Countrypoll.get_population country1 in
      assert_equal (original_population = new_population) false );
    ( "removing a state affects the total population" >:: fun _ ->
      let original_population = Countrypoll.get_population country1 in
      let () = Countrypoll.remove_state country1 state1 in
      let new_population = Countrypoll.get_population country1 in
      assert_equal (new_population = original_population) false );
    ( "removing a state means the list returned by the get_state function \
       changes"
    >:: fun _ ->
      let original_states = Countrypoll.get_states country1 in
      let () = Countrypoll.remove_state country1 state2 in
      let new_states = Countrypoll.get_states country1 in
      assert_equal (List.length original_states = List.length new_states) false
    );
    ( "adding a state affects the total population" >:: fun _ ->
      let original_popuation = Countrypoll.get_population country1 in
      let () = Countrypoll.add_state country1 state3 in
      let new_population = Countrypoll.get_population country1 in
      assert_equal (new_population = original_popuation) false );
    ( "adding a state means the list returned by the get_state function changes, \n\
      \    given that the state is not in the country already"
    >:: fun _ ->
      let original_list = Countrypoll.get_states country1 in
      let () = Countrypoll.add_state country1 state3 in
      let new_list = Countrypoll.get_states country1 in
      assert_equal (List.length new_list = List.length original_list) false );
    ( "trying to remove a state that isn't in the country does nothing"
    >:: fun _ ->
      let original_states_length =
        List.length (Countrypoll.get_states country2)
      in
      let () = Countrypoll.remove_state country2 state1 in
      let new_states_length = List.length (Countrypoll.get_states country2) in
      assert_equal new_states_length original_states_length );
    ( "adding a state that is already in the country does nothing" >:: fun _ ->
      let original_states_length =
        List.length (Countrypoll.get_states country2)
      in
      let () = Countrypoll.add_state country2 state3 in
      let new_states_length = List.length (Countrypoll.get_states country2) in
      assert_equal new_states_length original_states_length );
  ]

let good_csv = Csv.load "test.csv"
let bad_csv_1 = Csv.load "test_bad.csv"
let bad_csv_2 = Csv.load "test_bad_2.csv"
let empty_csv = Csv.load "empty_country.csv"

let test_create_from_csv =
  [
    ( "exception thrown when column count is not 6" >:: fun _ ->
      assert_raises (Countrypoll.ImproperCSV "") (fun () ->
          Countrypoll.create_country_from_CSV bad_csv_1 "bad country" true) );
    ( "exception thrown when row is faulty" >:: fun _ ->
      assert_raises (Countrypoll.ImproperCSV "") (fun () ->
          Countrypoll.create_country_from_CSV bad_csv_2 "also bad" false) );
    ( "when a good csv is passed, the country created is valid \n\
      \  (this test checks attributes"
    >:: fun _ ->
      let country = Countrypoll.create_country_from_CSV good_csv "good" true in
      assert_equal (List.length (Countrypoll.get_states country)) 4 );
    ( "an empty CSV throws an exception" >:: fun _ ->
      assert_raises (Countrypoll.ImproperCSV "") (fun () ->
          Countrypoll.create_country_from_CSV empty_csv "empty" true) );
  ]

let alabama =
  Statepoll.create_state
    [ "Alabama"; "al"; "9"; "5108468"; "Trump"; "0.18875664116805574" ]

let alaska =
  Statepoll.create_state
    [ "Alaska"; "ak"; "3"; "733406"; "Trump"; "0.051270956337204764" ]

let arizona =
  Statepoll.create_state
    [ "Arizona"; "az"; "11"; "7341344"; "Biden"; "0.1731993941811405" ]

let arkansas =
  Statepoll.create_state
    [ "Arkansas"; "ar"; "6"; "3067732"; "Trump"; "0.19474136752138244" ]

let state_list_test = [ alabama; alaska; arizona; arkansas ]
let test_country = Countrypoll.create_country state_list_test true "temu US"

let test_country_from_csv =
  Countrypoll.create_country_from_CSV good_csv "good" true

let test_create_from_csv2 =
  [
    ( "equals is true (the state lists are accurate)" >:: fun _ ->
      assert_equal (Countrypoll.equals test_country test_country_from_csv) true
    );
  ]

let () = Countrypoll.save_data_locally test_country_from_csv "fuck my life"

let export_to_csv_tests =
  [
    ( "calling export_data on test_country_from_csv csv it originated from"
    >:: fun _ ->
      let () =
        print_endline
          (string_of_int
             (Csv.compare
                (Countrypoll.export_data test_country_from_csv)
                good_csv))
      in
      assert_equal
        (Csv.compare (Countrypoll.export_data test_country_from_csv) good_csv)
        0 );
  ]

(*GPT*)
let does_not_throw_exception test name () =
  try Countrypoll.save_data_locally test name
  with _ -> assert_failure "Unexpected Exception"

(*END GPT*)
let export_to_machine_tests =
  [
    ( "trying to export does not throw an exception" >:: fun _ ->
      does_not_throw_exception test_country "hello" () );
  ]

let countrypoll_tests =
  "full test suite"
  >::: List.flatten
         [
           test_equals;
           test_contains;
           getter_tests;
           mutability_tests;
           test_create_from_csv;
           test_create_from_csv2;
           export_to_csv_tests;
           export_to_machine_tests;
         ]

let run_countrypoll_tests () = run_test_tt_main countrypoll_tests
