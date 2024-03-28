open Final

module ExtractableStringList = struct
  type t = string list

  let from_csv = false
  let of_data data = List.hd data
end

module StringListFile = File.Make (ExtractableStringList)
module CandidateFile = File.Make (Candidate.C)
module StateFile = File.Make (State.S)

let candidate_files : string list =
  StringListFile.(extract (of_name "data/candidates_list.txt"))

let state_files : string list =
  StringListFile.(extract (of_name "data/states_list.txt"))

module GroupExtract (F : File.F) = struct
  let rec from_lst = function
    | [] -> []
    | h :: t -> F.(extract (of_name h)) :: from_lst t
end

module ExtractCandidates = GroupExtract (CandidateFile)
module ExtractStates = GroupExtract (StateFile)

let candidates : Candidate.C.t list = ExtractCandidates.from_lst candidate_files
let states : State.S.t list = ExtractStates.from_lst state_files

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
     votes, 3 electors]"

let _ = run ()
