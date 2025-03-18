
include Map.Make (String)

let from_list list = list |> List.to_seq |> of_seq

