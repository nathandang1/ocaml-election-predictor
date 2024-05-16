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

let test_hypothesis =
  [
    ( "Testing hypothesis function with theta [1.; 2.; 3.] and x [1.; 2.; 3.], \
       expecting sigmoid(14)"
    >:: fun _ ->
      assert_equal
        (Models.hypothesis [ 1.; 2.; 3. ] [ 1.; 2.; 3. ])
        (Models.sigmoid 14.) );
    ( "Testing hypothesis function with theta [0.; 0.; 0.] and x [1.; 2.; 3.], \
       expecting sigmoid(0)"
    >:: fun _ ->
      assert_equal
        (Models.hypothesis [ 0.; 0.; 0. ] [ 1.; 2.; 3. ])
        (Models.sigmoid 0.) );
    ( "Testing hypothesis function with theta [1.; 2.; 3.] and x [0.; 0.; 0.], \
       expecting sigmoid(0)"
    >:: fun _ ->
      assert_equal
        (Models.hypothesis [ 1.; 2.; 3. ] [ 0.; 0.; 0. ])
        (Models.sigmoid 0.) );
    ( "Testing hypothesis function with theta [1.; -2.; 3.] and x [-1.; 2.; \
       -3.], expecting sigmoid(-14)"
    >:: fun _ ->
      assert_equal
        (Models.hypothesis [ 1.; -2.; 3. ] [ -1.; 2.; -3. ])
        (Models.sigmoid (-14.)) );
    ( "Testing hypothesis function with large positive theta and x, expecting \
       value close to 1"
    >:: fun _ ->
      assert_bool ""
        (Models.hypothesis [ 1000.; 1000.; 1000. ] [ 1.; 1.; 1. ] > 0.99) );
    ( "Testing hypothesis function with large negative theta and x, expecting \
       value close to 0"
    >:: fun _ ->
      assert_bool ""
        (Models.hypothesis [ -1000.; -1000.; -1000. ] [ 1.; 1.; 1. ] < 0.01) );
    ( "Testing hypothesis function with very small positive theta and x, \
       expecting value slightly greater than 0.5"
    >:: fun _ ->
      assert_bool ""
        (Models.hypothesis [ 0.0001; 0.0001; 0.0001 ] [ 1.; 1.; 1. ] > 0.5) );
    ( "Testing hypothesis function with very small negative theta and x, \
       expecting value slightly less than 0.5"
    >:: fun _ ->
      assert_bool ""
        (Models.hypothesis [ -0.0001; -0.0001; -0.0001 ] [ 1.; 1.; 1. ] < 0.5)
    );
  ]

let test_gradient =
  [
    ( "Testing gradient function with varying list sizes" >:: fun _ ->
      for i = 1 to 10 do
        let theta = List.init i (fun _ -> 1.0) in
        let xs = List.init i (fun _ -> List.init i (fun _ -> 1.0)) in
        let ys = List.init i (fun _ -> float_of_int i) in
        assert_bool ""
          (Models.gradient theta xs ys <> List.init i (fun _ -> 0.0))
      done );
    ( "Testing gradient function with varying list sizes and checking returned \
       list length"
    >:: fun _ ->
      for i = 1 to 10 do
        let theta = List.init i (fun _ -> 1.0) in
        let xs = List.init i (fun _ -> List.init i (fun _ -> 1.0)) in
        let ys = List.init i (fun _ -> float_of_int i) in
        assert_equal i (List.length (Models.gradient theta xs ys))
      done );
    ( "Testing gradient function with empty lists, expecting empty list"
    >:: fun _ -> assert_equal [] (Models.gradient [] [] []) );
    ( "Testing gradient function with theta [0.; 0.; 0.], xs [[1.; 2.; 3.]], \
       ys [1.], expecting non-zero values"
    >:: fun _ ->
      assert_bool ""
        (Models.gradient [ 0.; 0.; 0. ] [ [ 1.; 2.; 3. ] ] [ 1. ]
        <> [ 0.; 0.; 0. ]) );
    ( "Testing gradient function with varying theta and xs, ys [1.], expecting \
       non-zero values for different list sizes"
    >:: fun _ ->
      for i = 1 to 100 do
        let theta = List.init i (fun _ -> 0.0) in
        let xs = List.init i (fun _ -> List.init i (fun _ -> 1.0)) in
        let ys = List.init i (fun _ -> 1.0) in
        assert_bool ""
          (Models.gradient theta xs ys <> List.init i (fun _ -> 0.0))
      done );
  ]

let test_models =
  "test suite for models.ml"
  >::: List.flatten [ test_sigmoid; test_hypothesis; test_gradient ]

(* let test_models = "test suite for models.ml" >::: [ ( "create test" >:: fun _
   -> assert_equal (Float.round (Models.geometric_mean lst1)) 2. ); ] [] *)
let run_models_test () = run_test_tt_main test_models
