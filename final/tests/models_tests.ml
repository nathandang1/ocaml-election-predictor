open Final
open OUnit2

let lst1 = [ 1.1; 2.2; 3.3 ]

(* As the sigmoid function is critical to logistic regression, it is tested
   extensively even though coverage is achieved. However, given challenges with
   roundoff, assert_equal is preferred to evaluate large/small number
   performance.*)
let test_sigmoid =
  [
    ( "Testing sigmoid function with input 0, expecting 0.5" >:: fun _ ->
      assert_equal (Models.sigmoid 0.) 0.5 );
    ( "Testing sigmoid function with input 1, expecting value greater than 0.5"
    >:: fun _ -> assert_equal (Models.sigmoid 1.) (1. /. (1. +. exp (-1.))) );
    ( "Testing sigmoid function with input -1, expecting value less than 0.5"
    >:: fun _ -> assert_equal (Models.sigmoid (-1.)) (1. /. (1. +. exp 1.)) );
    ( "Testing sigmoid function with input 2, expecting value greater than 0.5"
    >:: fun _ -> assert_equal (Models.sigmoid 2.) (1. /. (1. +. exp (-2.))) );
    ( "Testing sigmoid function with input -2, expecting value less than 0.5"
    >:: fun _ -> assert_equal (Models.sigmoid (-2.)) (1. /. (1. +. exp 2.)) );
    ( "Testing sigmoid function with input 4, expecting value greater than 0.5"
    >:: fun _ -> assert_equal (Models.sigmoid 4.) (1. /. (1. +. exp (-4.))) );
    ( "Testing sigmoid function with input 0, expecting exactly 0.5" >:: fun _ ->
      assert_bool "" (Models.sigmoid 0. = 0.5) );
    ( "Testing sigmoid function with input 1, expecting value greater than 0.5"
    >:: fun _ -> assert_bool "" (Models.sigmoid 1. > 0.5) );
    ( "Testing sigmoid function with input -1, expecting value less than 0.5"
    >:: fun _ -> assert_bool "" (Models.sigmoid (-1.) < 0.5) );
    ( "Testing sigmoid function with large positive input 100, expecting value \
       close to 1"
    >:: fun _ -> assert_bool "" (Models.sigmoid 100. > 0.99) );
    ( "Testing sigmoid function with large negative input -100, expecting \
       value close to 0"
    >:: fun _ -> assert_bool "" (Models.sigmoid (-100.) < 0.01) );
    ( "Testing sigmoid function with large positive input 1000, expecting \
       value close to 1"
    >:: fun _ -> assert_bool "" (Models.sigmoid 10000. > 0.99) );
    ( "Testing sigmoid function with large negative input -1000, expecting \
       value close to 0"
    >:: fun _ -> assert_bool "" (Models.sigmoid (-10000.) < 0.01) );
    ( "Testing sigmoid function with very small positive input 0.0001, \
       expecting value slightly greater than 0.5"
    >:: fun _ -> assert_bool "" (Models.sigmoid 0.0001 > 0.5) );
    ( "Testing sigmoid function with very small negative input -0.0001, \
       expecting value slightly less than 0.5"
    >:: fun _ -> assert_bool "" (Models.sigmoid (-0.0001) < 0.5) );
  ]

let test_models = "test suite for models.ml" >::: List.flatten [ test_sigmoid ]

(* let test_models = "test suite for models.ml" >::: [ ( "create test" >:: fun _
   -> assert_equal (Float.round (Models.geometric_mean lst1)) 2. ); ] [] *)
let run_models_test () = run_test_tt_main test_models
