open Final
open OUnit2

let candidate1 =
  { Candidate.name = "Biden"; Candidate.party = "Democratic" }

let candidate2 =
  { Candidate.name = "Trump"; Candidate.party = "Republican" }

let state = {
    Statepoll.name = "New York";
    Statepoll.preferred_candidate = candidate1;
    Statepoll.preferred_margin = 3.2;
    Statepoll.num_votes = 28;
    Statepoll.population = 196800000;
}

let statepoll_tests =
  (*Test suite*)
  "statepoll_tests"
  >::: [
         ( "Test the get_name function from statepoll." >:: fun _ -> 
          assert_equal (Statepoll.get_name state) "New York" );
         ( "Test the get_preferred_candidate_name function from statepoll."
         >:: fun _ ->
           assert_equal (Statepoll.get_preferred_candidate_name state) "Biden"
         );
         ( "Test the get_preferred_candidate_party function from statepoll."
         >:: fun _ ->
           assert_equal
             (Statepoll.get_preferred_candidate_party state)
             "Democratic" );
         ( "Test the get_preferred_margin function from statepoll." >:: fun _ ->
           assert_equal (Statepoll.get_preferred_margin state) 3.2 );
         ( "Test the get_num_votes function from statepoll." >:: fun _ ->
           assert_equal (Statepoll.get_num_votes state) 28 );
         ( "Test the get_population function from statepoll." >:: fun _ ->
           assert_equal (Statepoll.get_population state) 196800000 );
         ( "Test the set_preferred function from statepoll." >:: fun _ ->
           let () = (Statepoll.set_preferred_candidate state) candidate2 in
           assert_equal (Statepoll.get_preferred_candidate_name state) "Trump"
         );
         ( "Test the set_preferred function from statepoll." >:: fun _ ->
              let () = (Statepoll.set_preferred_candidate state) candidate2 in
              assert_equal (Statepoll.get_preferred_candidate_party state) "Republican"
            );
            ( "Test the set_num_votes function from statepoll." >:: fun _ ->
              let () = (Statepoll.set_num_votes state) 40 in
              assert_equal (Statepoll.get_num_votes state) 40
            );
            ( "Test the set_population function from statepoll." >:: fun _ ->
              let () = (Statepoll.set_population state) 4023245 in
              assert_equal (Statepoll.get_population state) 4023245
            );
       ]

let run_statepoll_tests = run_test_tt_main statepoll_tests