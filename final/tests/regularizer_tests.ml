open Final
open OUnit2

let lst1 = [ 1.5; 3.2; 4.3 ]
let lst2 = [ 0.5; 2.2; 5.4 ]
let lst3 = [ "climate"; "abortion"; "voting"; "race" ]

let all_elements_between_zero_and_one lst =
  List.for_all (fun x -> x >= 0. && x <= 1.) lst

let test_regularizer =
  "test suite for regularizer.ml"
  >::: [
         ( "ones test" >:: fun _ ->
           assert_equal (Regularizer.ones 3) [ 1.; 1.; 1. ] );
         ( "issue_weights test" >:: fun _ ->
           assert_equal
             (all_elements_between_zero_and_one
                (Regularizer.issues_weights lst3))
             true );
         ( "element_product test" >:: fun _ ->
           assert_equal
             (Regularizer.element_product lst1 lst2)
             [ 0.75; 7.04; 23.22 ] );
         ( "sum_float_list test" >:: fun _ ->
           assert_equal (Regularizer.sum_float_list lst1) 9.0 );
         ( "concatenate_lists test" >:: fun _ ->
           assert_equal
             (Regularizer.concatenate_lists lst1 lst2)
             [ (1.5, 0.5); (3.2, 2.2); (4.3, 5.4) ] );
       ]

let run_regularizer_test = run_test_tt_main test_regularizer
