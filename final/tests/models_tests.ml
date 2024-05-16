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

let test_gradient_descent =
  [
    ( "Testing gradient_descent function with num_iters = 0, expecting theta \
       unchanged"
    >:: fun _ ->
      assert_equal [ 1.; 2.; 3. ]
        (Models.gradient_descent [ 1.; 2.; 3. ] [ [ 1.; 2.; 3. ] ] [ 6. ] 0.1 0)
    );
    ( "Testing gradient_descent function with alpha = 0, expecting theta \
       unchanged"
    >:: fun _ ->
      assert_equal [ 1.; 2.; 3. ]
        (Models.gradient_descent [ 1.; 2.; 3. ]
           [ [ 1.; 2.; 3. ] ]
           [ 6. ] 0.0 100) );
    ( "Testing gradient_descent function with num_iters = 1" >:: fun _ ->
      assert_bool ""
        (Models.gradient_descent [ 1.; 2.; 3. ] [ [ 1.; 2.; 3. ] ] [ 6. ] 0.1 1
        <> [ 1.; 2.; 3. ]) );
    ( "Testing gradient_descent function with num_iters = 5" >:: fun _ ->
      assert_bool ""
        (Models.gradient_descent [ 1.; 2.; 3. ] [ [ 1.; 2.; 3. ] ] [ 6. ] 0.1 5
        <> [ 1.; 2.; 3. ]) );
    ( "Testing gradient_descent function with num_iters = 10" >:: fun _ ->
      assert_bool ""
        (Models.gradient_descent [ 1.; 2.; 3. ] [ [ 1.; 2.; 3. ] ] [ 6. ] 0.1 10
        <> [ 1.; 2.; 3. ]) );
    ( "Testing gradient_descent function with list size = 1" >:: fun _ ->
      assert_equal 1
        (List.length (Models.gradient_descent [ 1. ] [ [ 1. ] ] [ 1. ] 0.1 100))
    );
    ( "Testing gradient_descent function with list size = 5" >:: fun _ ->
      assert_equal 5
        (List.length
           (Models.gradient_descent [ 1.; 1.; 1.; 1.; 1. ]
              [ [ 1.; 1.; 1.; 1.; 1. ] ]
              [ 5. ] 0.1 100)) );
    ( "Testing gradient_descent function with list size = 10" >:: fun _ ->
      assert_equal 10
        (List.length
           (Models.gradient_descent
              [ 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1. ]
              [ [ 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1. ] ]
              [ 10. ] 0.1 100)) );
    ( "Testing gradient_descent function with empty theta, xs, and ys"
    >:: fun _ -> assert_equal [] (Models.gradient_descent [] [] [] 0.1 100) );
    ( "Testing gradient_descent function with large alpha" >:: fun _ ->
      let theta =
        Models.gradient_descent [ 1.; 2.; 3. ]
          [ [ 1.; 2.; 3. ] ]
          [ 6. ] 100.0 100
      in
      assert_bool "" (List.for_all (( < ) 0.) theta) );
    ( "Testing gradient_descent function with large num_iters" >:: fun _ ->
      let theta =
        Models.gradient_descent [ 1.; 2.; 3. ]
          [ [ 1.; 2.; 3. ] ]
          [ 6. ] 0.1 10000
      in
      assert_bool "" (List.for_all (( < ) 0.) theta) );
  ]

let florida =
  [
    [ "1992"; "FLORIDA"; "0.4090249693604387"; "0.3899012156452978"; "rep" ];
    [ "1996"; "FLORIDA"; "0.4232454874966859"; "0.4802557119781926"; "dem" ];
    [ "2000"; "FLORIDA"; "0.4884682657204043"; "0.4883782120403614"; "rep" ];
    [ "2004"; "FLORIDA"; "0.5209751623233695"; "0.4709111002771423"; "rep" ];
    [ "2008"; "FLORIDA"; "0.4821531916597622"; "0.5103330527066491"; "dem" ];
    [ "2012"; "FLORIDA"; "0.4913097776197552"; "0.5000786506869869"; "dem" ];
    [ "2016"; "FLORIDA"; "0.4902194141659073"; "0.4782331580580505"; "rep" ];
    [ "2020"; "FLORIDA"; "0.5121981962250404"; "0.4786145072544223"; "rep" ];
  ]

let rec repeat n f acc = if n = 0 then acc else repeat (n - 1) f (f () :: acc)

let average r =
  let sum = List.fold_left ( +. ) 0. r in
  sum /. float_of_int (List.length r)

let test_logistic_regression_helper =
  [
    ( "Testing average over several tries, given randomness of logistic \
       regression model"
    >:: fun _ ->
      let florida =
        [
          [
            "1992"; "FLORIDA"; "0.4090249693604387"; "0.3899012156452978"; "rep";
          ];
          [
            "1996"; "FLORIDA"; "0.4232454874966859"; "0.4802557119781926"; "dem";
          ];
          [
            "2000"; "FLORIDA"; "0.4884682657204043"; "0.4883782120403614"; "rep";
          ];
          [
            "2004"; "FLORIDA"; "0.5209751623233695"; "0.4709111002771423"; "rep";
          ];
          [
            "2008"; "FLORIDA"; "0.4821531916597622"; "0.5103330527066491"; "dem";
          ];
          [
            "2012"; "FLORIDA"; "0.4913097776197552"; "0.5000786506869869"; "dem";
          ];
          [
            "2016"; "FLORIDA"; "0.4902194141659073"; "0.4782331580580505"; "rep";
          ];
          [
            "2020"; "FLORIDA"; "0.5121981962250404"; "0.4786145072544223"; "rep";
          ];
        ]
      in
      let results =
        repeat 1000
          (fun () -> Models.logistic_regression_helper florida 0.01 1000)
          []
      in
      let avg = average results in
      assert_bool "" (avg < 0.3) );
  ]

let test_geometric_mean =
  [
    ( "Testing geometric_mean with a list of 1s" >:: fun _ ->
      assert_equal 1. (Models.geometric_mean (List.init 5 (fun _ -> 1.))) );
    ( "Testing geometric_mean with a list of [2.; 4.; 8.]" >:: fun _ ->
      assert_equal 4. (Models.geometric_mean [ 2.; 4.; 8. ]) );
    ( "Testing geometric_mean with a list containing 0" >:: fun _ ->
      assert_equal 0. (Models.geometric_mean [ 1.; 2.; 0.; 3. ]) );
    ( "Testing geometric_mean with an empty list" >:: fun _ ->
      assert_equal 1. (Models.geometric_mean []) );
    ( "Testing geometric_mean with a list of [4.0000000001; 3.9999999999]"
    >:: fun _ ->
      assert_equal 4. (Models.geometric_mean [ 4.0000000001; 3.9999999999 ]) );
  ]

let test_models =
  "test suite for models.ml"
  >::: List.flatten
         [
           test_sigmoid;
           test_hypothesis;
           test_gradient;
           test_gradient_descent;
           test_logistic_regression_helper;
           test_geometric_mean;
         ]

let run_models_test () = run_test_tt_main test_models
