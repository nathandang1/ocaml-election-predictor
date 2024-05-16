open OUnit2
open Final
open Models

let test_sigmoid _ =
  assert_equal (sigmoid 0.) 0.5;
  assert_bool "sigmoid of a large positive number should be close to 1"
    (sigmoid 1000. > 0.99);
  assert_bool "sigmoid of a large negative number should be close to 0"
    (sigmoid (-1000.) < 0.01);
  assert_equal (sigmoid (log 2.)) 0.5;
  assert_equal (sigmoid (log 3.)) (2. /. 3.)

let test_hypothesis _ =
  assert_equal (hypothesis [ 1.; 2. ] [ 3.; 4. ]) (sigmoid 11.);
  assert_equal (hypothesis [ 0.; 0. ] [ 3.; 4. ]) 0.5;
  assert_equal (hypothesis [ 1.; 1. ] [ 1.; 1. ]) (sigmoid 2.);
  assert_equal (hypothesis [ 2.; 3. ] [ 4.; 5. ]) (sigmoid 23.);
  assert_equal (hypothesis [ 1.; 2.; 3. ] [ 4.; 5.; 6. ]) (sigmoid 32.)

let test_gradient _ =
  let theta = [ 1.; 2. ] in
  let xs = [ [ 3.; 4. ]; [ 5.; 6. ]; [ 7.; 8. ] ] in
  let ys = [ 1.; 0.; 1. ] in
  let grad = gradient theta xs ys in
  assert_equal (List.length grad) 2;
  assert_bool "first element of grad should be a float"
    (match List.hd grad with
    | f when f = f -> true
    | _ -> false);
  assert_bool "second element of grad should be a float"
    (match List.nth grad 1 with
    | f when f = f -> true
    | _ -> false)

let test_gradient_descent _ =
  let theta = [ 1.; 2. ] in
  let xs = [ [ 3.; 4. ]; [ 5.; 6. ]; [ 7.; 8. ] ] in
  let ys = [ 1.; 0.; 1. ] in
  let alpha = 0.01 in
  let num_iters = 1000 in
  let theta = gradient_descent theta xs ys alpha num_iters in
  assert_equal (List.length theta) 2;
  assert_bool "first element of theta should be a float"
    (match List.hd theta with
    | f when f = f -> true
    | _ -> false);
  assert_bool "second element of theta should be a float"
    (match List.nth theta 1 with
    | f when f = f -> true
    | _ -> false)

let () =
  run_test_tt_main
    ("all tests"
    >::: [
           "test_sigmoid" >:: test_sigmoid;
           "test_hypothesis" >:: test_hypothesis;
           "test_gradient" >:: test_gradient;
           "test_gradient_descent" >:: test_gradient_descent;
         ])
