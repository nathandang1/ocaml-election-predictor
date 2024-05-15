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

let run_candidate_tests = run_test_tt_main candidate_tests