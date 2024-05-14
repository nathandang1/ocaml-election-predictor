open Final
open OUnit2

let candidate_tests =
  (*Test suite*)
  "candidate_tests"
  >::: [
         ( "Test the simple create function from candidate." >:: fun _ ->
           let candidate1 =
             { Candidate.name = "Donald Trump"; Candidate.party = "Republican" }
           in
           assert_equal candidate1
             (Candidate.create ("Donald Trump", "Republican")) );
         ( "Test the simple create function from candidate." >:: fun _ ->
           let candidate2 =
             { Candidate.name = "Joe Biden"; Candidate.party = "Democratic" }
           in
           assert_equal candidate2
             (Candidate.create ("Joe Biden", "Democratic")) );
         ( "Test the get name feature function from candidate." >:: fun _ ->
           let candidate2 =
             { Candidate.name = "Joe Biden"; Candidate.party = "Democratic" }
           in
           assert_equal (Candidate.name candidate2) "Joe Biden" );
         ( "Test the get party feature function from candidate." >:: fun _ ->
           let candidate2 =
             { Candidate.name = "Joe Biden"; Candidate.party = "Democratic" }
           in
           assert_equal (Candidate.party candidate2) "Democratic" );
         ( "Test the of_data function from candidate." >:: fun _ ->
           let candidate2 =
             { Candidate.name = "Joe Biden"; Candidate.party = "Democratic" }
           in
           let candidate_list : string list list =
             [ [ "Joe Biden"; "Democratic" ]; [] ]
           in
           assert_equal (Candidate.of_data candidate_list) candidate2 );
         ( "Test the of_data function from candidate." >:: fun _ ->
           let candidate2 =
             { Candidate.name = "Trump"; Candidate.party = "Republican" }
           in
           let candidate_list : string list list =
             [ [ "Trump"; "Republican" ]; [ "Biden"; "Democratic" ] ]
           in
           assert_equal (Candidate.of_data candidate_list) candidate2 );
       ]

let _ = run_test_tt_main candidate_tests

let state_tests =
  (*Test suite*)
  "state_tests"
  >::: [
         ( "Test the of_data function from state." >:: fun _ ->
           let state1 =
             {
               State.name = "New York";
               State.votes = 28;
               State.pop = 19680000;
               State.pref_can = "Biden";
               State.pref_percent = 53.4;
               State.abbr = "NY";
             }
           in
           let state_list : string list list =
             [ [ "New York"; "28"; "19680000"; "Biden"; "53.4"; "NY" ]; [] ]
           in
           assert_equal (State.of_data state_list) state1 );
         ( "Test the of_data function from state." >:: fun _ ->
           let state2 =
             {
               State.name = "Florida";
               State.votes = 30;
               State.pop = 22240000;
               State.pref_can = "Trump";
               State.pref_percent = 55.1;
               State.abbr = "FL";
             }
           in
           let state_list : string list list =
             [ [ "Florida"; "30"; "22240000"; "Trump"; "55.1"; "FL" ]; [] ]
           in
           assert_equal (State.of_data state_list) state2 );
       ]

let _ = run_test_tt_main state_tests

let statepoll_tests =
  (*Test suite*)
  "statepoll_tests"
  >::: [
         ( "Test the get_name function from statepoll." >:: fun _ ->
           let candidate1 =
             { Candidate.name = "Biden"; Candidate.party = "Democratic" }
           in
           let state1 =
             {
               Statepoll.name = "New York";
               Statepoll.preferred_candidate = candidate1;
               Statepoll.preferred_margin = 3.2;
               Statepoll.num_votes = 28;
               Statepoll.population = 196800000;
             }
           in
           assert_equal (Statepoll.get_name state1) "New York" );
         ( "Test the get_preferred_candidate_name function from statepoll."
         >:: fun _ ->
           let candidate1 =
             { Candidate.name = "Biden"; Candidate.party = "Democratic" }
           in
           let state1 =
             {
               Statepoll.name = "New York";
               Statepoll.preferred_candidate = candidate1;
               Statepoll.preferred_margin = 3.2;
               Statepoll.num_votes = 28;
               Statepoll.population = 196800000;
             }
           in
           assert_equal (Statepoll.get_preferred_candidate_name state1) "Biden"
         );
         ( "Test the get_preferred_candidate_party function from statepoll."
         >:: fun _ ->
           let candidate1 =
             { Candidate.name = "Biden"; Candidate.party = "Democratic" }
           in
           let state1 =
             {
               Statepoll.name = "New York";
               Statepoll.preferred_candidate = candidate1;
               Statepoll.preferred_margin = 3.2;
               Statepoll.num_votes = 28;
               Statepoll.population = 196800000;
             }
           in
           assert_equal
             (Statepoll.get_preferred_candidate_party state1)
             "Democratic" );
         ( "Test the get_preferred_margin function from statepoll." >:: fun _ ->
           let candidate1 =
             { Candidate.name = "Biden"; Candidate.party = "Democratic" }
           in
           let state1 =
             {
               Statepoll.name = "New York";
               Statepoll.preferred_candidate = candidate1;
               Statepoll.preferred_margin = 3.2;
               Statepoll.num_votes = 28;
               Statepoll.population = 196800000;
             }
           in
           assert_equal (Statepoll.get_preferred_margin state1) 3.2 );
         ( "Test the get_num_votes function from statepoll." >:: fun _ ->
           let candidate1 =
             { Candidate.name = "Biden"; Candidate.party = "Democratic" }
           in
           let state1 =
             {
               Statepoll.name = "New York";
               Statepoll.preferred_candidate = candidate1;
               Statepoll.preferred_margin = 3.2;
               Statepoll.num_votes = 28;
               Statepoll.population = 196800000;
             }
           in
           assert_equal (Statepoll.get_num_votes state1) 28 );
         ( "Test the get_population function from statepoll." >:: fun _ ->
           let candidate1 =
             { Candidate.name = "Biden"; Candidate.party = "Democratic" }
           in
           let state1 =
             {
               Statepoll.name = "New York";
               Statepoll.preferred_candidate = candidate1;
               Statepoll.preferred_margin = 3.2;
               Statepoll.num_votes = 28;
               Statepoll.population = 196800000;
             }
           in
           assert_equal (Statepoll.get_population state1) 196800000 );
         ( "Test the set_preferred function from statepoll." >:: fun _ ->
           let candidate1 =
             { Candidate.name = "Biden"; Candidate.party = "Democratic" }
           in
           let candidate2 =
             { Candidate.name = "Trump"; Candidate.party = "Republican" }
           in
           let state1 =
             {
               Statepoll.name = "New York";
               Statepoll.preferred_candidate = candidate1;
               Statepoll.preferred_margin = 3.2;
               Statepoll.num_votes = 28;
               Statepoll.population = 196800000;
             }
           in
           let () = (Statepoll.set_preferred_candidate state1) candidate2 in
           assert_equal (Statepoll.get_preferred_candidate_name state1) "Trump"
         );
         ( "Test the set_preferred function from statepoll." >:: fun _ ->
              let candidate1 =
                { Candidate.name = "Biden"; Candidate.party = "Democratic" }
              in
              let candidate2 =
                { Candidate.name = "Trump"; Candidate.party = "Republican" }
              in
              let state1 =
                {
                  Statepoll.name = "New York";
                  Statepoll.preferred_candidate = candidate1;
                  Statepoll.preferred_margin = 3.2;
                  Statepoll.num_votes = 28;
                  Statepoll.population = 196800000;
                }
              in
              let () = (Statepoll.set_preferred_candidate state1) candidate2 in
              assert_equal (Statepoll.get_preferred_candidate_party state1) "Republican"
            );
            ( "Test the set_num_votes function from statepoll." >:: fun _ ->
              let candidate1 =
                { Candidate.name = "Biden"; Candidate.party = "Democratic" }

              in
              let state1 =
                {
                  Statepoll.name = "New York";
                  Statepoll.preferred_candidate = candidate1;
                  Statepoll.preferred_margin = 3.2;
                  Statepoll.num_votes = 28;
                  Statepoll.population = 196800000;
                }
              in
              let () = (Statepoll.set_num_votes state1) 40 in
              assert_equal (Statepoll.get_num_votes state1) 40
            );
            ( "Test the set_population function from statepoll." >:: fun _ ->
              let candidate1 =
                { Candidate.name = "Biden"; Candidate.party = "Democratic" }

              in
              let state1 =
                {
                  Statepoll.name = "New York";
                  Statepoll.preferred_candidate = candidate1;
                  Statepoll.preferred_margin = 3.2;
                  Statepoll.num_votes = 28;
                  Statepoll.population = 196800000;
                }
              in
              let () = (Statepoll.set_population state1) 4023245 in
              assert_equal (Statepoll.get_population state1) 4023245
            );
       ]

let _ = run_test_tt_main statepoll_tests
