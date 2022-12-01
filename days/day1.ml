
let elf_calories s : int list list =
  let rec inner (elves: int list list) (current: int list) (lines: string list) = match lines with
    | ("" :: remaining)      -> (* new elf *) inner ([current] @ elves) [] remaining
    | (calorie :: remaining) -> (* add cal to elf*) inner elves ((int_of_string calorie) :: current) remaining
    | _ -> elves                (* no more lines *)
  in
  let lines = String.split_on_char '\n' s in
  inner [] [] lines

let sum = List.fold_left (+) 0

let () =
  let contents = In_channel.with_open_bin "days/input.txt" In_channel.input_all in
  let elves = elf_calories contents in
  List.iteri (fun i elf ->
    Printf.printf "Elf %d : %d entries for %d total calories\n" i (List.length elf) (sum elf)) elves;

  let max_elf_calories = elves
    |> List.map sum
    |> List.fold_left max 0 in
  Printf.printf "\n(Part 1) Max elf calories is %d\n" max_elf_calories;

  let top_three = elves
    |> List.map sum
    |> List.sort compare
    |> List.rev
    |> CCList.take 3
    |> sum in
    Printf.printf "\n(Part 2) Total of top three elves is %d\n" top_three;
()