open Final

(* let _ = Interface.transition Title *)

(* let trump_total = ref 0. let biden_total = ref 0. let percent_total = ref 0.

   let () = for i = 0 to 10 do let result = Regularizer.do_computations () in
   let trump_sum = fst (snd result) in let biden_sum = snd (snd result) in
   trump_total := !trump_total +. trump_sum; biden_total := !biden_total +.
   biden_sum; percent_total := !percent_total +. Regularizer.diff trump_sum
   biden_sum; (* let () = print_float (Regularizer.diff trump_sum biden_sum) in
   let () = print_endline "" in *) () done

   (* let () = print_float !trump_total let () = print_endline "" let () =
   print_float !biden_total *) (* let () = print_float !percent_total let () =
   print_endline "\n" *)

   let output = Models.naive_bayes_randomized (List.tl (Csv.load
   "data/data-extraction/state-data/florida.csv"))

   let () = Printf.printf "%.*f\n" 4 (fst output) let () = Printf.printf
   "%.*f\n" 4 (snd output) *)

let _ = Interface.transition Title
