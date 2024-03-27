let states = [ ("Cornell", 7); ("Harvard", 5); ("Yale", 3) ] (* *)

let run () =
  print_endline "***** OCAML ELECTION PREDICTOR *****";
  print_endline "";
  print_endline
    "[Cornell | 7] -> [Joe Biden: 22 votes] [Donald Trump: 12 votes]";
  print_endline "[Harvard | 5] -> [Joe Biden: 22 votes] [Donald Trump: 8 votes]";
  print_endline "[Yale | 3] -> [Joe Biden: 22 votes] [Donald Trump: 28 votes]";
  print_endline "";
  print_endline
    "[OVERALL | 15] -> [Joe Biden: 66 votes, 12 electors] [Donald Trump: 48 \
     votes, 3 elector]"

let _ = run ()
