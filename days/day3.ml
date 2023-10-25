open Containers

(* integer priority of each letter from a..zA..Z *)
let get_priority c =
  let offset_from base c = CCChar.to_int c - CCChar.to_int base in
  match c with
    | 'a' .. 'z' -> 1 + offset_from 'a' c
    | 'A' .. 'Z' -> 1 + offset_from 'A' c
    | _ -> failwith "cooked"

module CharSet = Set.Make(Char)

(* kinda scuffed *)
let parse_rucksack line =
  let compartment_size = (String.length line) / 2 in
  let char_list = String.to_list line in
  let rucksack1 = List.take compartment_size char_list |> String.of_list in
  let rucksack2 = List.last compartment_size char_list |> String.of_list in
  (rucksack1, rucksack2)

(* string -> CharSet.t *)
let charset s = String.to_seq s |> CharSet.of_seq

(* string list -> char *)
let find_badge n_rucksacks = 
  let charsets = List.map charset n_rucksacks in
  let intersection = List.fold_left CharSet.inter CharSet.empty charsets in
  CharSet.choose intersection

(* only slightly different to the above. could actually just combine them but fk it *)
let find_duplicate (r1, r2) = (charset r1, charset r2)
                              |> (fun (chars1, chars2) -> CharSet.inter chars1 chars2)
                              |> CharSet.choose

let sum = List.fold_left (+) 0

let () =
  let lines =
    In_channel.with_open_bin "days/input_day3_sample.txt" In_channel.input_all
    |> String.split_on_char '\n' in
  let rucksacks = List.map parse_rucksack lines in
  
  let duplicates: char list = List.map find_duplicate rucksacks in
  List.iter (fun d -> Printf.printf "Char duplicate item %c\n" d) duplicates;

  List.iter (fun r -> Printf.printf "%s %s\n" (fst r) (snd r)) rucksacks;

  let items_sum = rucksacks
    |> List.map find_duplicate
    |> List.map get_priority
    |> sum in
  Printf.printf "(part 1) sum: %d\n" items_sum;

  (* Part 2 *)
  let groups = List.chunks 3 lines in
  let priority_sum = groups
    |> List.map find_badge
    |> List.map get_priority
    |> sum in
  Printf.printf "(part 2) sum: %d\n" priority_sum