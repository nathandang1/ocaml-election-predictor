let () = Random.self_init ()
let open_metadata path = Csv.load path
let meta () = open_metadata "data/metadata/metadata.csv"
let meta_labels = List.hd (meta ())
let issues = List.tl (meta ())

(** [ones n] creates a float list of [n] ones. *)
let ones n = List.init n (fun _ -> 1.)

(** [issues_weights issues] applies a random weight between 0.0 and 1.0 to each
    "issue" in [issues]. *)
let issues_weights issues =
  List.map (fun x -> x *. Random.float 1.0) (ones (List.length issues))

(** [element_product lst1 lst2] computes the element-wise product of [lst1] and
    [lst2]. *)
let rec element_product lst1 lst2 =
  match lst1 with
  | [] -> []
  | h1 :: t1 -> (
      match lst2 with
      | [] -> []
      | h2 :: t2 -> (h1 *. h2) :: element_product t1 t2)

(** [sum_float_list lst] computes the sum of [lst]. *)
let sum_float_list lst = List.fold_left (fun acc a -> a +. acc) 0. lst

(** [softmax lst] applies the softmax function - (e^z_i)/sum_over_i(e^z_i) - to
    [lst] element-wise. *)
let softmax lst =
  let exp_lst = List.map exp lst in
  let exp_sum = sum_float_list exp_lst in
  List.map (fun e -> e /. exp_sum) exp_lst

(** [concatenate_lists lst1 lst2] *)
let rec concatenate_lists lst1 lst2 =
  match lst1 with
  | [] -> []
  | h1 :: t1 -> (
      match lst2 with
      | [] -> []
      | h2 :: t2 -> (h1, h2) :: concatenate_lists t1 t2)

(* print functions *)
let print_string_list_list lst =
  List.iter (fun sublist -> List.iter (fun s -> print_endline s) sublist) lst

let print_string_list lst = List.iter (fun s -> print_string (s ^ " ")) lst
let print_float_list lst = List.iter (fun x -> Printf.printf "%.*f\n" 4 x) lst

(* Perform computations *)
let do_computations () =
  let () = Random.self_init () in

  let transp = Csv.transpose issues in
  let issues_titles = List.hd transp in

  let trump_values =
    List.map (fun s -> float_of_string s) (List.hd (List.tl transp))
  in
  let biden_values =
    List.map (fun s -> float_of_string s) (List.hd (List.tl (List.tl transp)))
  in
  (* Applying softmax to the randomly generated weights standardizes the weight
     sum to 1 and simulates emphasizing the more "important" issues. *)
  let new_issues_weights = softmax (issues_weights issues) in
  let trump_weights = element_product trump_values new_issues_weights in
  let biden_weights = element_product biden_values new_issues_weights in
  let trump_sum = sum_float_list trump_weights in
  let biden_sum = sum_float_list biden_weights in
  let issues_titles_weights =
    concatenate_lists issues_titles new_issues_weights
  in

  (issues_titles_weights, (trump_sum, biden_sum))

(* Print results *)
let print_issues_and_totals () =
  let result = do_computations () in

  let issues_titles_weights = fst result in
  let trump_sum = fst (snd result) in
  let biden_sum = snd (snd result) in

  let () =
    List.iter
      (fun (title, value) ->
        print_string (title ^ " ");
        print_float value;
        print_endline "")
      issues_titles_weights
  in
  let () =
    print_string "Trump weight: ";
    print_float trump_sum;
    print_endline "";
    print_string "Biden weight: ";
    print_float biden_sum;
    print_endline "\n"
  in
  ()

let print_totals_only () =
  let result = do_computations () in

  let trump_sum = fst (snd result) in
  let biden_sum = snd (snd result) in

  let () =
    print_string "Trump weight: ";
    print_float trump_sum;
    print_endline "";
    print_string "Biden weight: ";
    print_float biden_sum;
    print_endline "\n"
  in
  ()

(* More computations *)
<<<<<<< Updated upstream
let diff val1 val2 = 200. *. (val1 -. val2) /. (val1 +. val2)
(* let () = print_float diff *)
=======
let diff = 200. *. abs_float (trump_sum -. biden_sum) /. (trump_sum +. biden_sum)
(* let () = print_float diff *)
>>>>>>> Stashed changes
