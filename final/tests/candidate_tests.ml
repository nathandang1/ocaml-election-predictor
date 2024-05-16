open Final
open OUnit2

let candidate1 =
  { Candidate.name = "Donald Trump"; Candidate.party = "Republican" }

let test_candidate =
  "test suite for candidate.ml"
  >::: [
         ( "create test" >:: fun _ ->
           assert_equal
             (Candidate.create ("Donald Trump", "Republican"))
             candidate1 );
         ( "name test" >:: fun _ ->
           assert_equal (Candidate.name candidate1) "Donald Trump" );
         ( "party test" >:: fun _ ->
           assert_equal (Candidate.party candidate1) "Republican" );
       ]

let run_candidate_test () = run_test_tt_main test_candidate
