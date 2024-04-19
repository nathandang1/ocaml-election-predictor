module StringKey = struct
  type t = string

  let compare a b = Stdlib.compare a b
end

module StringMap = Map.Make (StringKey)

type t = {
  indices : int StringMap.t;
  labels : int StringMap.t;
  data : Csv.t;
}
(** AF: The lists nested in [df.data] correspond to the rows of elements in the
    dataframe [df]; where if [idx] is bound to [n] in [df.indices] then [idx] is
    the index of the [n]th row, and if [lbl] is bound to [m] in [df.labels] then
    [lbl] is the label of the [m]th column. RI: All lists in [df] should be of
    the same length; and the number of bindings in [df.indices] should be equal
    to the length of [df.data] and the number of bindings in [df.labels] should
    be equal to the length of any list nested in [df.data]. *)

let labels df = List.map (fun (x, _) -> x) (StringMap.bindings df.labels)
let indices df = List.map (fun (x, _) -> x) (StringMap.bindings df.indices)
let data df = df.data

let get_col df lbl =
  List.nth (Csv.transpose df.data) (StringMap.find lbl df.labels)

let get_row df idx = List.nth df.data (StringMap.find idx df.indices)

exception InvalidCSV

let of_csv (csv : Csv.t) =
  match Csv.square csv with
  | [] -> raise InvalidCSV
  | h :: t ->
      let num_of_cols =
        if List.length csv > 0 then csv |> List.hd |> List.length else 0
      in
      let labels =
        let enum = ref (-1) in
        List.fold_left
          (fun map lbl ->
            incr enum;
            StringMap.add lbl !enum map)
          StringMap.empty h
      in
      if num_of_cols <> (labels |> StringMap.bindings |> List.length) then
        raise InvalidCSV
      else
        let num_of_rows = List.length csv in
        let indices =
          let rec nats n = if n = 0 then [ 0 ] else n :: nats (n - 1) in
          List.fold_left
            (fun map idx -> StringMap.add (string_of_int idx) idx map)
            StringMap.empty
            (nats (num_of_rows - 1))
        in
        let data = t in
        { labels; indices; data }

let to_csv df : Csv.t = labels df :: df.data
