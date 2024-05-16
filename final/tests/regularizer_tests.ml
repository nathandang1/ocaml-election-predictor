open Final
open OUnit2

let lst1 = [ 1.5; 3.2; 4.3 ]
let lst2 = [ 0.5; 2.2; 5.4 ]
let lst3 = [ "climate"; "abortion"; "voting"; "race" ]

let all_elements_between_zero_and_one lst =
  List.for_all (fun x -> x >= 0. && x <= 1.) lst

let test_opens =
  try
    let _ = Regularizer.open_metadata "metadata.csv" in
    ()
  with _ -> assert_failure "Unexpected Exception."

let test_open =
  [ ("ensures function does not throw an exception" >:: fun _ -> test_opens) ]

let test_ones =
  [
    ("ones creation" >:: fun _ -> assert_equal (Regularizer.ones 2) [ 1.; 1. ]);
  ]

let test_issues_weights =
  [
    ( "creates without issues" >:: fun _ ->
      assert_equal
        (all_elements_between_zero_and_one (Regularizer.issues_weights lst3))
        true );
  ]

let list_negatives = [ -1.0; -2.0; -3.0 ]
let list_mixed = [ -1.0; 2.0; -3.0 ]

let float_lst_equal lst1 lst2 =
  List.for_all
    (fun x -> x < 0.1 && x > -0.1)
    (List.map2 (fun a b -> a -. b) lst1 lst2)

let test_element_product =
  [
    ( "element_product test (positives x positives)" >:: fun _ ->
      assert_equal
        (float_lst_equal
           (Regularizer.element_product lst1 lst2)
           [ 0.75; 7.04; 23.22 ])
        true );
    ( "element_product test (negatives x negatives)" >:: fun _ ->
      assert_equal
        (float_lst_equal
           (Regularizer.element_product list_negatives list_negatives)
           [ 1.0; 4.0; 9.0 ])
        true );
    ( "element_product test (negatives x positives)" >:: fun _ ->
      assert_equal
        (float_lst_equal
           (Regularizer.element_product lst1 list_mixed)
           [ -1.5; 6.4; -12.9 ])
        true );
  ]

let test_sum_float_list =
  [
    ( "sum_float_list test (all positive)" >:: fun _ ->
      assert_equal (Regularizer.sum_float_list lst1) 9.0 );
    ( "sum_float_list test (all negative)" >:: fun _ ->
      assert_equal (Regularizer.sum_float_list list_negatives) (-6.0) );
    ( "sum_float_list test (mixed values)" >:: fun _ ->
      assert_equal (Regularizer.sum_float_list list_mixed) (-2.0) );
  ]

let test_concatenate =
  [
    ( "concatenate_lists test (positive x positive)" >:: fun _ ->
      assert_equal
        (Regularizer.concatenate_lists lst1 lst2)
        [ (1.5, 0.5); (3.2, 2.2); (4.3, 5.4) ] );
    ( "concatenate_lists test (positive x negative)" >:: fun _ ->
      assert_equal
        (Regularizer.concatenate_lists lst1 list_negatives)
        [ (1.5, -1.0); (3.2, -2.0); (4.3, -3.0) ] );
    ( "concatenate_lists test (negative x negative)" >:: fun _ ->
      assert_equal
        (Regularizer.concatenate_lists list_negatives list_negatives)
        [ (-1.0, -1.0); (-2.0, -2.0); (-3.0, -3.0) ] );
  ]

let test_regularizer =
  "test suite for regularizer.ml"
  >::: List.flatten
         [
           test_open;
           test_ones;
           test_issues_weights;
           test_element_product;
           test_sum_float_list;
           test_concatenate;
         ]

let run_regularizer_test () = run_test_tt_main test_regularizer
