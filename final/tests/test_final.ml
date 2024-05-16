open Statepoll_tests
open Candidate_tests
open Countrypoll_tests
open Models_tests
open Model_test_broken

let () = run_statepoll_tests
let () = run_candidate_test
let () = run_countrypoll_tests
let () = run_models_test
let () = run_models_tests ()
