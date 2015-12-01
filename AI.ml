open Gamestate
open Gamelogic
open Card

type gamestage = Initial | Flop | Turn | River

let game_stage g = match g.flop with
  | h1::h2::h3::h4::h5::[] -> River
  | h1::h2::h3::h4::[]     -> Turn
  | h1::h2::h3::[]         -> Flop
  | _                      -> Initial

let call_amount g =
  let i,ai = List.hd g.players in
  g.bet - ai.amount_in

let point_standard g = match game_stage g with
  | Initial -> 20.
  | Flop    -> 40.
  | Turn    -> 60.
  | River   -> 80.

let same_suit_bonus  = function
  | h1::h2::[] -> if same_suit h1 h2 then 10. else 0.
  | _          -> 0.

let card_bonus c = match value_to_string (val_of_card c) with
  | "2"  -> 0.
  | "3"  -> 1.
  | "4"  -> 2.
  | "5"  -> 3.
  | "6"  -> 4.
  | "7"  -> 5.
  | "8"  -> 6.
  | "9"  -> 7.
  | "10" -> 8.
  | "J"  -> 9.
  | "Q"  -> 10.
  | "K"  -> 11.
  | "A"  -> 12.
  | _    -> failwith "?"

let rec cards_bonus = function
  | [] -> 0.
  | h::t -> (card_bonus h) +. (cards_bonus t)

let best_hand_bonus clist = match determine_best_hand clist with
  | HighCard clist -> cards_bonus clist
  | Pair clist    -> 10. +. cards_bonus clist
  | TwoPair clist  -> 20. +. cards_bonus clist
  | Triple clist  -> 30. +. cards_bonus clist
  | Straight clist -> 40. +. cards_bonus clist
  | Flush clist    -> 50. +. cards_bonus clist
  | FullHouse clist -> 60. +. cards_bonus clist
  | Quads clist    -> 70. +. cards_bonus clist
  | StraightFlush clist -> 80. +. cards_bonus clist
  | RoyalFlush clist -> 90. +. cards_bonus clist

let pot_odds g = 
  let bet_needed = float_of_int (call_amount g) in
  (float_of_int g.pot) /. bet_needed

let hand_points_initial g =
  let _, ai = List.hd g.players in
  (cards_bonus ai.cards) +. (same_suit_bonus ai.cards)

let hand_points_midgame g =
  let _, ai = List.hd g.players in
  (best_hand_bonus (ai.cards @ g.flop))
  
let hand_points g = match game_stage g with
  | Initial -> hand_points_initial g
  | _       -> hand_points_midgame g

let rand_multiplier () = Random.float 2.

let floor_bet_to_all_in bet g =
  let i,ai = List.hd g.players in
  if bet > ai.stake then ai.stake else bet

let turn g =
  let modified_points = (rand_multiplier ()) *. (hand_points g) in
  let points_needed = (point_standard g) /. (pot_odds g) in
  let diff_in_points = int_of_float (modified_points -. points_needed) in
  if diff_in_points <= 0 && (g.last_move = Check || g.last_move = Deal) then
    (print_endline "AI checks" ; check g)
  else if diff_in_points <= 0 then
    (print_endline "AI folds" ; fold g)
  else if diff_in_points > 10 then (*not sure when he can raise*)
    let amount = floor_bet_to_all_in (diff_in_points - (call_amount g)) g in
    (Printf.printf "AI raise %d\n" amount ; do_raise g amount)
  else
    (print_endline "AI calls" ; call g)


