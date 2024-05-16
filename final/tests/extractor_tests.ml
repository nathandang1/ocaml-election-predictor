open Final
open OUnit2

let state_lst =
  [
    {
      State.name = "Alabama";
      State.abbr = "al";
      State.votes = 9;
      State.pop = 5108468;
      State.pref_can = "Trump";
      State.pref_percent = 0.18875664116805574;
    };
    {
      State.name = "Alaska";
      State.abbr = "ak";
      State.votes = 3;
      State.pop = 733406;
      State.pref_can = "Trump";
      State.pref_percent = 0.051270956337204764;
    };
    {
      State.name = "Arizona";
      State.abbr = "az";
      State.votes = 11;
      State.pop = 7341344;
      State.pref_can = "Biden";
      State.pref_percent = 0.1731993941811405;
    };
  ]

let candidate_lst : Candidate.t list =
  [
    { Candidate.name = "Donald Trump"; Candidate.party = "Republican" };
    { Candidate.name = "Joe Biden"; Candidate.party = "Democrat" };
  ]

let test_extractor =
  "test suite for extractor.ml"
  >::: [
         ( "data test" >:: fun _ ->
           assert_equal
             (Extractor.data ("candidates.csv", "states.csv"))
             (candidate_lst, state_lst) );
       ]

let run_extractor_test () = run_test_tt_main test_extractor
