open Final
open OUnit2
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

let run_state_tests = run_test_tt_main state_tests