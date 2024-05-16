open Final
open OUnit2

let lst1 = [ 1.1; 2.2; 3.3 ]

let test_models =
  "test suite for models.ml"
  >::: [
         ( "create test" >:: fun _ ->
           assert_equal (Float.round (Models.geometric_mean lst1)) 2. );
       ]

let run_models_test () = run_test_tt_main test_models
