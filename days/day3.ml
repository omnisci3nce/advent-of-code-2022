let item_types = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

(** association list of item type -> priority *)
let priorities =
  let index = ref 0 in
  String.fold_left (fun l c ->
    index := !index + 1;
    ((String.make 1 c), !index) :: l
  ) [] item_types


let parse_rucksack line =
  let compartment_size = (String.length line) / 2 in
  let c1 = ref ""
  and c2 = ref "" in
  String.iteri (fun i c ->
    if i < compartment_size then
      c1 := !c1 ^ ( String.make 1 c)
    else 
      c2 := !c2 ^ (String.make 1 c)
    ) line;
  (!c1, !c2)

let find_shared_item rucksack =
  let c1 = Hashtbl.create 8
  and c2 = Hashtbl.create 8 in
  let argh tbl s =
    String.iter (fun c -> Hashtbl.add tbl (String.make 1 c) 1) s
  in
  argh c1 (fst rucksack);
  argh c2 (snd rucksack);
  let keys = Hashtbl.to_seq_keys c1 in
  match Seq.find (fun k -> Option.is_some (Hashtbl.find_opt c2 k)) keys with
  | Some s -> s
  | _ -> failwith ""

let find_badge (r1, r2, r3) =
  let r1_items = Hashtbl.create 8
  and r2_items = Hashtbl.create 8
  and r3_items = Hashtbl.create 8 in
  let argh tbl s =
    String.iter (fun c -> Hashtbl.add tbl (String.make 1 c) 1) s
  in
  argh r1_items r1;
  argh r2_items r2;
  argh r3_items r3;
  let keys = Hashtbl.to_seq_keys r1_items in
  match Seq.find (fun k ->
    Option.is_some (Hashtbl.find_opt r2_items k)
    &&
    Option.is_some (Hashtbl.find_opt r3_items k)
  ) keys with
  | Some badge -> badge
  | _ -> failwith ""

let flip f x y = f y x
let sum = List.fold_left (+) 0
let () =
  let contents = In_channel.with_open_bin "days/input_day3.txt" In_channel.input_all in
  let lines = String.split_on_char '\n' contents in
  let rucksacks = List.map parse_rucksack lines in
  List.iter (fun r -> Printf.printf "%s %s\n" (fst r) (snd r)) rucksacks;

  let items_sum = rucksacks
    |> List.map find_shared_item
    |> List.map ((flip List.assoc) priorities)
    |> sum in
  Printf.printf "(part 1) sum: %d\n" items_sum;

  (* Part 2 *)
  let groups = CCList.chunks 3 lines in
  (* each line of the group is one rucksack *)
  let priority_sum = groups
    |> List.map (fun g -> find_badge ((List.nth g 0),(List.nth g 1),(List.nth g 2)))
    |> List.map ((flip List.assoc) priorities)
    |> sum in
  Printf.printf "(part 2) sum: %d\n" priority_sum