open Statepoll_tests
open Candidate_tests
open Countrypoll_tests
open Models_tests
open Extractor_tests
open Regularizer_tests

let () = run_statepoll_tests ()
let () = run_candidate_test ()
let () = run_countrypoll_tests ()
let () = run_models_test ()
let () = run_extractor_test ()
let () = run_regularizer_test ()
