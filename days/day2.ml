type move = Rock | Paper | Scissors
type outcome = Win | Draw | Lose

let parse_enemy_move = function
  | "A" -> Rock
  | "B" -> Paper
  | "C" -> Scissors
  | _ -> failwith "Invalid move"

let parse_my_outcome = function
  | "X" -> Lose
  | "Y" -> Draw
  | "Z" -> Win
  | _ -> failwith "Invalid outcome"

let determine_outcome round = match round with
  | (Rock, Scissors)  -> Lose
  | (Paper, Scissors) -> Win
  | (Rock, Paper)     -> Win
  | (Scissors, Paper) -> Lose
  | (Paper, Rock)     -> Lose
  | (Scissors, Rock)  -> Win
  | _ -> Draw

let move_for_target_outcome = function
  | (Rock, Win) -> Paper
  | (Rock, Lose) -> Scissors
  | (Paper, Win) -> Scissors
  | (Paper, Lose) -> Rock
  | (Scissors, Win) -> Rock
  | (Scissors, Lose) -> Paper
  | (move, Draw) -> move

let points = function
  | Win -> 6
  | Lose -> 0
  | Draw -> 3

let get_score round =
  let my_move = move_for_target_outcome round in
  let outcome = determine_outcome (fst round, my_move) in
  let score = points outcome in
  match my_move with
  | Rock     -> 1 + score
  | Paper    -> 2 + score
  | Scissors -> 3 + score

let parse_round line =
  let r = String.split_on_char ' ' line in
  match List.length r with
  | 2 -> (parse_enemy_move (List.hd r), parse_my_outcome (List.nth r 1))
  | _ -> failwith "Invalid line"

let sum = List.fold_left (+) 0
let () =
  let contents = In_channel.with_open_bin "days/input_day2.txt" In_channel.input_all in
  let lines = String.split_on_char '\n' contents in
  let score = lines |> List.map parse_round |> List.map get_score |> sum in
  Printf.printf "(Part 2) final score %d\n" score