open Final
open OUnit2

(* We have elected to test the sigmoid function carefully, not just within the
   context of coverage. The sigmoid function is integral to the logistic
   regression decision boundary. If it is not working as expected, then
   inference using Logisitc Regression fails. We wished to confirm that our
   implementation aligned with the properties of the sigmoid function in ML,
   where the [z] argment is usually written as [w'x], where ' denotes a
   transpose. We wished to see that, despite a different method of
   implementation, the behavior was ultimately the same. These test cases give
   us confidence that the sigmoid function indeed works the way we expect for
   standard and large negative and positive inputs, in addition to achieving
   coverage of the relatively simple function.*)
let test_sigmoid =
  [
    ( "Testing sigmoid function with input 0, expecting exactly 0.5" >:: fun _ ->
      assert_bool "" (Models.sigmoid 0. = 0.5) );
    ( "Testing sigmoid function with input 1, expecting value greater than 0.5"
    >:: fun _ -> assert_bool "" (Models.sigmoid 1. > 0.5) );
    ( "Testing sigmoid function with input -1, expecting value less than 0.5"
    >:: fun _ -> assert_bool "" (Models.sigmoid (-1.) < 0.5) );
    ( "Testing sigmoid function with large positive input 1000, expecting \
       value close to 1"
    >:: fun _ -> assert_bool "" (Models.sigmoid 10000. > 0.99) );
    ( "Testing sigmoid function with large negative input -1000, expecting \
       value close to 0"
    >:: fun _ -> assert_bool "" (Models.sigmoid (-10000.) < 0.01) );
  ]

(* Acknowledgement: The following code for testing the hypothesis function was
   enhanced with the guidance of Github Copilot. Copilot helped add
   approximately 3 extra tests cases.*)
(* Citation: 'Given this function for the hypothesis function in a logistic
   regression implementation, and this current test suite, enhance the test
   suite by adding tests that cover unique input types.', GitHub Copilot,
   OpenAI/Microsoft, 16 May 2024, https://github.com/features/copilot. *)
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

(* Acknowledgement: The following code for testing the gradient function was
   enhanced with the guidance of Github Copilot. Copilot helped add
   approximately 4 extra tests cases.*)
(* Citation: 'Given this function for computing the gradient in a logistic
   regression implementation, and this current test suite, enhance the test
   suite by adding tests that cover unique input types.', GitHub Copilot,
   OpenAI/Microsoft, 16 May 2024, https://github.com/features/copilot. *)
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
    ( "Testing gradient function evaluation to an exact value based on \
       hand-computed values for the gradient, starting from standard theta = \
       [0.,0.]"
    >:: fun _ ->
      let theta = [ 0.0; 0.0 ] in
      let xs = [ [ 2.0; 3.0 ]; [ 3.0; 4.0 ]; [ 4.0; 5.0 ] ] in
      let ys = [ 2.0; 3.0; 4.0 ] in
      let result = Models.gradient theta xs ys in
      assert_bool "First entry not in range [-9, -8]"
        (List.nth result 0 > -9. && List.nth result 0 < -8.);
      assert_bool "Second entry not in range [-11, -10]"
        (List.nth result 1 > -11. && List.nth result 1 < -10.) );
    ( "Testing gradient function evaluation to an exact value based on \
       hand-computed \n\
      \      values for the gradient, starting from arbitrary theta"
    >:: fun _ ->
      let theta = [ 0.5; 0.5 ] in
      let xs = [ [ 1.0; 2.0 ]; [ 2.0; 3.0 ]; [ 3.0; 4.0 ] ] in
      let ys = [ 1.0; 2.0; 3.0 ] in
      let result = Models.gradient theta xs ys in
      assert_bool "First entry not in range [-3, -2]"
        (List.nth result 0 > -3. && List.nth result 0 < -2.);
      assert_bool "Second entry not in range [-4, -3]"
        (List.nth result 1 > -4. && List.nth result 1 < -3.) );
  ]

(* Acknowledgement: The following code for testing the hypothesis function was
   enhanced with the guidance of Github Copilot. Copilot helped add
   approximately 3 extra tests cases.*)
(* Citation: 'Given this function for gradient descent in a logistic regression
   implementation, and this current test suite, enhance the test suite by adding
   tests that cover unique input types.', GitHub Copilot, OpenAI/Microsoft, 15
   May 2024, https://github.com/features/copilot. *)
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
    ( "Testing gradient_descent function exact evaluation for simple case, \
       comparing output to by-hand computation."
    >:: fun _ ->
      let theta = [ 0.0; 0.0 ] in
      let xs = [ [ 1.0; 2.0 ] ] in
      let ys = [ 1.0 ] in
      let alpha = 1. in
      let num_iters = 1 in
      let result = Models.gradient_descent theta xs ys alpha num_iters in
      let expected = [ 0.5; 1.0 ] in
      assert_equal (List.nth expected 0) (List.nth result 0);
      assert_equal (List.nth expected 1) (List.nth result 1) );
    ( "Testing gradient_descent function exact evaluation for a simple case \
       with multiple x data inputs,\n\
      \     a more complicated alpha, and 2 iterations. The calculation is \
       done by hand using the gradient descent algorithm."
    >:: fun _ ->
      let theta = [ 0.0; 0.0 ] in
      let xs = [ [ 2.0; 3.0 ]; [ 3.0; 4.0 ] ] in
      let ys = [ 2.0; 3.0 ] in
      let alpha = 0.5 in
      let num_iters = 2 in
      let result = Models.gradient_descent theta xs ys alpha num_iters in
      assert_bool "First entry not in range"
        (List.nth result 0 >= 4. && List.nth result 0 <= 5.);
      assert_bool "Second entry not in range"
        (List.nth result 1 >= 6. && List.nth result 1 <= 7.) );
  ]

(** [repeat n f accum] constructs a list with the specified property [f] for [n]
    iterations in order to enable averaging over random algorithms.*)
let rec repeat n f accum =
  if n = 0 then accum else repeat (n - 1) f (f () :: accum)

let average r =
  let sum = List.fold_left ( +. ) 0. r in
  sum /. float_of_int (List.length r)

let test_logistic_regression_helper =
  [
    ( "Right leaning state: Testing average over several tries for a right \
       leaning state, given randomness of logistic regression model. The \
       prediction for a right leaning state is < 0.5, and left leaning state \
       is greater than 0.5. We know this should be the case for Florida, which \
       is a strong right state. "
    >:: fun _ ->
      (* pass in florida election data*)
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
      assert_bool "" (avg < 0.5) );
    ( "Left leaning state: Testing average over several tries for a left \
       leaning state, given randomness of logistic regression model. The \
       prediction \n\
      \       for a right leaning state is < 0.5, and left leaning state is \
       greater than 0.5. We know this should be the case for California, which \
       is a strong left state."
    >:: fun _ ->
      (* pass in california election data.*)
      let california =
        [
          [ "1992"; "CALI"; "0.3261466937592129"; "0.4600658784028094"; "dem" ];
          [ "1996"; "CALI"; "0.3820942008004616"; "0.5109886561852729"; "dem" ];
          [ "2000"; "CALI"; "0.4165149680525545"; "0.5344973682775446"; "dem" ];
          [ "2004"; "CALI"; "0.4435769597724177"; "0.5430555753467436"; "dem" ];
          [ "2008"; "CALI"; "0.3695485883246447"; "0.6101263834713425"; "dem" ];
          [ "2012"; "CALI"; "0.371203785207048"; "0.6023895914168964"; "dem" ];
          [ "2016"; "CALI"; "0.3161710653843943"; "0.6172639960455788"; "dem" ];
          [ "2020"; "CALI"; "0.3432072362528492"; "0.6348394689387351"; "dem" ];
        ]
      in
      let res =
        repeat 1000
          (fun () -> Models.logistic_regression_helper california 0.01 1000)
          []
      in
      let avg = average res in
      assert_bool "" (avg > 0.5) );
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

let test_get_reg_total =
  [
    ( "Testing get_reg_total with 0 iterations, where we expect that there is \
       going to be \n\
      \    no change in the total."
    >:: fun _ -> assert_equal (Models.get_reg_total 0) 0.0 );
  ]

let test_naive_bayes =
  [
    ( "Testing Naive Bayes for Sample Input data for a fake state in which \
       democratic candidate consistently dominates the republican candidate \
       (as evidence by the entire label column being dem), and where the \
       probabilities are not 1 or 0."
    >:: fun _ ->
      let data =
        [
          [ "1992"; "A"; "0.3261466937592129"; "0.4600658784028094"; "dem" ];
          [ "1996"; "A"; "0.3820942008004616"; "0.5109886561852729"; "dem" ];
          [ "2000"; "A"; "0.4165149680525545"; "0.5344973682775446"; "dem" ];
          [ "2004"; "A"; "0.4435769597724177"; "0.5430555753467436"; "dem" ];
          [ "2008"; "A"; "0.3695485883246447"; "0.6101263834713425"; "dem" ];
          [ "2012"; "A"; "0.371203785207048"; "0.6023895914168964"; "dem" ];
          [ "2016"; "A"; "0.3161710653843943"; "0.6172639960455788"; "dem" ];
          [ "2020"; "A"; "0.3432072362528492"; "0.6348394689387351"; "dem" ];
        ]
      in
      let results = Models.naive_bayes_randomized data 0 in
      assert_bool "" (fst results > snd results) );
    ( "Testing Naive Bayes for Sample Input data for a fake state in which \
       republican candidate consistently dominates the democrat candidate (as \
       evidence by the entire label column being rep), and where the \
       probabilities are not 1 or 0."
    >:: fun _ ->
      let data =
        [
          [ "1992"; "B"; "0.3945749808515083"; "0.3028711132430195"; "rep" ];
          [ "1996"; "B"; "0.5080125817399221"; "0.33267113649532326"; "rep" ];
          [ "2000"; "B"; "0.5862095531587057"; "0.27666339823504693"; "rep" ];
          [ "2004"; "B"; "0.6106532991253942"; "0.35516861912104364"; "rep" ];
          [ "2008"; "B"; "0.5942451953880631"; "0.37889373599389325"; "rep" ];
          [ "2012"; "B"; "0.5480157739729447"; "0.40812659112464433"; "rep" ];
          [ "2016"; "B"; "0.5128151207753728"; "0.36550871290111986"; "rep" ];
          [ "2020"; "B"; "0.5283314327038078"; "0.42771952271020497"; "rep" ];
        ]
      in
      let results = Models.naive_bayes_randomized data 0 in
      assert_bool "" (snd results > fst results) );
    ( "Naive Bayes for Sample Input data for a fake state in which republican \
       candidate consistently dominates the democrat candidate (as evidence by \
       the entire label column being rep), and where the probabilities ARE 1 \
       and 0."
    >:: fun _ ->
      let data =
        [
          [ "1992"; "C"; "1.0"; "0.0"; "rep" ];
          [ "1996"; "C"; "1.0"; "0.0"; "rep" ];
          [ "2000"; "C"; "1.0"; "0.0"; "rep" ];
          [ "2004"; "C"; "1.0"; "0.0"; "rep" ];
          [ "2008"; "C"; "1.0"; "0.0"; "rep" ];
          [ "2012"; "C"; "1.0"; "0.0"; "rep" ];
        ]
      in
      let results = Models.naive_bayes_randomized data 0 in
      assert_bool "" (snd results > fst results) );
    ( "Naive Bayes for Sample Input data for a fake state in which democrat \
       candidate consistently dominates the republican candidate (as evidence \
       by the entire label column being rep), and where the probabilities ARE \
       0 and 1."
    >:: fun _ ->
      let data =
        [
          [ "1992"; "D"; "0.0"; "1.0"; "dem" ];
          [ "1996"; "D"; "0.0"; "1.0"; "dem" ];
          [ "2004"; "D"; "0.0"; "1.0"; "dem" ];
          [ "2008"; "D"; "0.0"; "1.0"; "dem" ];
          [ "2012"; "D"; "0.0"; "1.0"; "dem" ];
          [ "2016"; "D"; "0.0"; "1.0"; "dem" ];
          [ "2020"; "D"; "0.0"; "1.0"; "dem" ];
        ]
      in
      let results = Models.naive_bayes_randomized data 0 in
      assert_bool "" (fst results > snd results) );
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
           test_get_reg_total;
           test_naive_bayes;
         ]

let run_models_test () = run_test_tt_main test_models
