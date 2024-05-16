let () = Random.self_init ()
let open_metadata path = Csv.load path

(** [ones n] creates a list of [n] ones. *)
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

(* Perform computations *)
let do_computations () =
  let () = Random.self_init () in

  let meta = open_metadata "data/metadata/metadata.csv" in
  (* let meta_labels = List.hd meta in *)
  let issues = List.tl meta in

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

(** [diff val1 val2] computes the unsigned percent difference between [val1] and
    [val2]. Requires: [val1 +. val2 != 0] *)
let diff val1 val2 = 200. *. (val1 -. val2) /. (val1 +. val2)
