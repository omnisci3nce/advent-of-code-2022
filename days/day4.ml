let get_range s = 
  let range = String.split_on_char '-' s in
  (List.nth range 0 |> int_of_string), (List.nth range 1 |> int_of_string)

let parse_pair line = match (String.split_on_char ',' line) with
  | [a; b] -> (get_range a, get_range b)
  | _ -> failwith "wrong format"

let _fully_contains (p1, p2) : bool =
  match p1, p2 with
  | (p1_lower, p1_upper), (p2_lower, p2_upper)
      when p2_lower >= p1_lower && p2_upper <= p1_upper -> true
  | (p1_lower, p1_upper), (p2_lower, p2_upper)
      when p1_lower >= p2_lower && p1_upper <= p2_upper -> true 
  | _ -> false

(*

[        ] p1
      [       ] p2

   p1 [       ]
  [      ] p2
*)

let partial_contains (p1, p2) : bool =
  match (p1, p2) with
  | (_, p1_upper), (p2_lower, p2_upper)
    when p1_upper >= p2_lower && p2_upper >= p1_upper-> true
  | (p1_lower, p1_upper), (_, p2_upper)
    when p2_upper >= p1_lower && p1_upper >= p2_upper-> true
  | _ -> false

let () =
  let contents = In_channel.with_open_bin "days/input_day4.txt" In_channel.input_all in
  let lines = String.split_on_char '\n' contents in
  let pairs = List.map parse_pair lines in
  let contains = List.filter partial_contains pairs in
  List.iter (fun (p1, p2) -> Printf.printf "%d-%d,%d-%d\n" (fst p1) (snd p1) (fst p2) (snd p2)) contains;
  Printf.printf "pairs that partially contain the other: %d\n" (List.length contains)