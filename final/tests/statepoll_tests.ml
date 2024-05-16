open Final
open OUnit2

let state =
  {
    Statepoll.name = "New York";
    Statepoll.abbreviation = "ny";
    Statepoll.preferred_candidate = "Biden";
    Statepoll.preferred_margin = 3.2;
    Statepoll.num_votes = 28;
    Statepoll.population = 196800000;
  }

(* Getter Method Tests*)
let state_attributes = [ "New York"; "ny"; "28"; "196800000"; "Biden"; "3.2" ]

let state_whitespace =
  Statepoll.create_state
    [
      "   New York    ";
      "  ny   ";
      "  28  ";
      "  196800000  ";
      " Biden ";
      " 0.1   ";
    ]

let create_test =
  [
    ( "valid creation" >:: fun _ ->
      assert_equal (Statepoll.create_state state_attributes) state );
    ( "error is thrown (length of list is faulty)" >:: fun _ ->
      assert_raises
        (Statepoll.ImproperList "List Length Specification Violated") (fun () ->
          Statepoll.create_state
            ("hi"
            :: [ "New York"; "Biden"; "Democratic"; "3.2"; "28"; "196800000" ]))
    );
    ( "error is thrown (non numeric number of votes)" >:: fun _ ->
      assert_raises
        (Statepoll.ImproperList "Data Formatting Specification Violated")
        (fun () ->
          Statepoll.create_state
            [ "New York"; "ny"; "hello"; "196800000"; "Biden"; "3.2" ]) );
    ( "error is thrown (non numeric number of votes)" >:: fun _ ->
      assert_raises
        (Statepoll.ImproperList "Data Formatting Specification Violated")
        (fun () ->
          Statepoll.create_state
            [ "New York"; "ny"; "28"; "heyo"; "Biden"; "3.2" ]) );
    ( "error is thrown (non numeric number of votes)" >:: fun _ ->
      assert_raises
        (Statepoll.ImproperList "Data Formatting Specification Violated")
        (fun () ->
          Statepoll.create_state
            [ "New York"; "ny"; "28"; "100"; "Biden"; "blurb" ]) );
    ( "error is throw (population is negative)" >:: fun _ ->
      assert_raises (Statepoll.ImproperList "Data Value Specfication Violated")
        (fun () ->
          Statepoll.create_state
            [ "New York"; "ny"; "28"; "-196800000"; "Biden"; "3.2" ]) );
    ( "error is throw (votes are negative)" >:: fun _ ->
      assert_raises (Statepoll.ImproperList "Data Value Specfication Violated")
        (fun () ->
          Statepoll.create_state
            [ "New York"; "ny"; "-28"; "196800000"; "Biden"; "3.2" ]) );
    ( "error is throw (margin of preference is negative)" >:: fun _ ->
      assert_raises (Statepoll.ImproperList "Data Value Specfication Violated")
        (fun () ->
          Statepoll.create_state
            [ "New York"; "ny"; "28"; "196800000"; "Biden"; "-3.2" ]) );
    ( "error is throw (population is zero)" >:: fun _ ->
      assert_raises (Statepoll.ImproperList "Data Value Specfication Violated")
        (fun () ->
          Statepoll.create_state [ "New York"; "ny"; "28"; "0"; "Biden"; "3.2" ])
    );
    ( "error is throw (votes are zero)" >:: fun _ ->
      assert_raises (Statepoll.ImproperList "Data Value Specfication Violated")
        (fun () ->
          Statepoll.create_state
            [ "New York"; "ny"; "0"; "196800000"; "Biden"; "3.2" ]) );
    ( "error is throw (margin of preference is zero)" >:: fun _ ->
      assert_raises (Statepoll.ImproperList "Data Value Specfication Violated")
        (fun () ->
          Statepoll.create_state
            [ "New York"; "ny"; "28"; "196800000"; "Biden"; "0.0" ]) );
    ( "error is throw (margin of preference is 100)" >:: fun _ ->
      assert_raises (Statepoll.ImproperList "Data Value Specfication Violated")
        (fun () ->
          Statepoll.create_state
            [ "New York"; "ny"; "28"; "196800000"; "Biden"; "100.0" ]) );
    ( "error is throw (margin of preference is greater than 100)" >:: fun _ ->
      assert_raises (Statepoll.ImproperList "Data Value Specfication Violated")
        (fun () ->
          Statepoll.create_state
            [ "New York"; "ny"; "28"; "196800000"; "Biden"; "1000.0" ]) );
    ( "whitespace name is trimmed" >:: fun _ ->
      assert_equal (Statepoll.get_name state_whitespace) "New York" );
    ( "whitespace abbreviation is trimmed" >:: fun _ ->
      assert_equal (Statepoll.get_abbreviation state_whitespace) "ny" );
    ( "whitespace votes are trimmed" >:: fun _ ->
      assert_equal (Statepoll.get_num_votes state_whitespace) 28 );
    ( "whitespace population is trimmed" >:: fun _ ->
      assert_equal (Statepoll.get_population state_whitespace) 196800000 );
    ( "whitespace candidate name is trimmed" >:: fun _ ->
      assert_equal
        (Statepoll.get_preferred_candidate_name state_whitespace)
        "Biden" );
    ( "whitespace margin of victory is trimmed" >:: fun _ ->
      assert_equal (Statepoll.get_preferred_margin state_whitespace) 0.1 );
  ]

let state_copy = state

let state_attributes_caps =
  [ "NEW YORK"; "ny"; "28"; "196800000"; "Biden"; "3.2" ]

let state_all_caps = Statepoll.create_state state_attributes_caps
let state_copy_2 = state_copy

let state2_attributes =
  [ "California"; "ca"; "28"; "196800000"; "Trump"; "3.2" ]

let state2 = Statepoll.create_state state2_attributes

let test_equals =
  [
    ( "reflexive test" >:: fun _ ->
      assert_equal (Statepoll.equals state state) true );
    ( "symmetric test" >:: fun _ ->
      assert_equal
        (Statepoll.equals state state_copy)
        (Statepoll.equals state_copy state) );
    ( "transitive test" >:: fun _ ->
      assert_equal
        (Statepoll.equals state state_copy
        && Statepoll.equals state_copy state_copy_2)
        (Statepoll.equals state state_copy_2) );
    ( "false test" >:: fun _ ->
      assert_equal (Statepoll.equals state state2) false );
    ( "same letters, different cases are treated different" >:: fun _ ->
      assert_equal (Statepoll.equals state state_all_caps) false );
  ]

let getter_method_tests =
  [
    ("get_name" >:: fun _ -> assert_equal (Statepoll.get_name state) "New York");
    ( "get_preferred_candidate_name" >:: fun _ ->
      assert_equal (Statepoll.get_preferred_candidate_name state) "Biden" );
    ( "get_abbreviation" >:: fun _ ->
      assert_equal (Statepoll.get_abbreviation state) "ny" );
    ( "get_preferred-margin" >:: fun _ ->
      assert_equal (Statepoll.get_preferred_margin state) 3.2 );
    ( "get_num_votes" >:: fun _ ->
      assert_equal (Statepoll.get_num_votes state) 28 );
    ( "get_population" >:: fun _ ->
      assert_equal (Statepoll.get_population state) 196800000 );
  ]

let setter_method_tests =
  [
    ( "set_preferred_candidate" >:: fun _ ->
      let () = Statepoll.set_preferred_candidate state "Trump" in
      assert_equal (Statepoll.get_preferred_candidate_name state) "Trump" );
    ( "set_preferred_margin" >:: fun _ ->
      let original_margin = Statepoll.get_preferred_margin state in
      let () = Statepoll.set_preferred_margin state (original_margin +. 1.0) in
      assert_equal
        (Statepoll.get_preferred_margin state = original_margin)
        false );
    ( "set_num_votes" >:: fun _ ->
      let original_votes = Statepoll.get_num_votes state in
      let () = Statepoll.set_num_votes state (original_votes + 1) in
      assert_equal (Statepoll.get_num_votes state = original_votes) false );
    ( "set_population" >:: fun _ ->
      let original_population = Statepoll.get_population state in
      let () = Statepoll.set_population state (original_population + 1) in
      assert_equal (Statepoll.get_population state = original_population) false
    );
  ]

let answer_key = Csv.load "fake_state.csv"

let fake_state =
  Statepoll.create_state [ "California"; "ca"; "10"; "10"; "Biden"; "2.3" ]

let answer_key_2 = Csv.load "fake_state_2.csv"

let even_faker_state =
  Statepoll.create_state
    [ "California"; "ca"; "1000000000"; "100000000000000000"; "Trump"; "99.9" ]

let csv_method_tests =
  [
    ( "test for Statepoll.export_state_to_csv" >:: fun _ ->
      let csv_cali_fake = Statepoll.export_state_to_csv fake_state in
      assert_equal (Csv.compare csv_cali_fake answer_key) 0 );
    ( "test for Statepoll.export_state_to_csv, bigger numbers to ensure \
       scalability"
    >:: fun _ ->
      let csv_cali_faker = Statepoll.export_state_to_csv even_faker_state in
      assert_equal (Csv.compare csv_cali_faker answer_key_2) 0 );
  ]

let statepoll_tests =
  "full test suite"
  >::: List.flatten
         [
           create_test;
           test_equals;
           getter_method_tests;
           setter_method_tests;
           csv_method_tests;
         ]

let run_statepoll_tests () = run_test_tt_main statepoll_tests
